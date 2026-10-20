(in-package :alloy-desktop)

(defstruct (%modal-result (:constructor %make-modal-result (value accepted-p)))
  value
  accepted-p)

(defvar *modal-stack* nil
  "Stack of active modal desktop windows; first is innermost.")

(defvar *modal-dismiss-on-input* nil
  "Innermost modal window whose host policy dismisses it on input.")

(defun current-modal-window ()
  (first *modal-stack*))

(defun modal-dismiss-on-input-window ()
  *modal-dismiss-on-input*)

(defun call-with-modal-state (window dismiss-on-input-p function)
  "Run FUNCTION with WINDOW as the innermost modal window.
Nested calls restore the previous modal and dismiss-on-input state on unwind."
  (push window *modal-stack*)
  (unwind-protect
      (let ((*modal-dismiss-on-input* (and dismiss-on-input-p window)))
        (funcall function))
    (setf *modal-stack* (remove window *modal-stack*))))

(defun clear-modal-state ()
  (setf *modal-stack* nil
        *modal-dismiss-on-input* nil)
  nil)

(defun %set-other-windows-enabled (modal-window enabled)
  "Enable or disable every registered live desktop window except MODAL-WINDOW."
  (loop for window in (registered-windows)
        unless (eq window modal-window)
        do #+win32
           (let ((hwnd (%window-hwnd window)))
             (when (and hwnd (not (cffi:null-pointer-p hwnd)))
               (%enable-window hwnd (if enabled 1 0))))
           #-win32
           (when (%glfw:pointer window)
             (setf (%glfw:attribute :mouse-passthrough window) (not enabled))))
  nil)

(defun exit-modal (value)
  "Terminate the innermost modal operation with accepted VALUE."
  (unless (current-modal-window)
    (error "EXIT-MODAL called outside a modal desktop operation."))
  (throw '%modal-exit (%make-modal-result value t)))

(defun abort-modal ()
  "Terminate the innermost modal operation as not accepted."
  (unless (current-modal-window)
    (error "ABORT-MODAL called outside a modal desktop operation."))
  (throw '%modal-exit (%make-modal-result nil nil)))

(defun %call-modal-iteration (function iteration-guard abort-restart-report)
  "Run one modal tick through ITERATION-GUARD, or under a plain abort restart when there is none."
  (%call-with-ui-task-drain-scope
   (lambda ()
     (if iteration-guard
         (funcall iteration-guard function)
         (with-simple-restart (abort "~A" abort-restart-report)
           (funcall function))))))

(defun run-modal-dialog (dialog
                         &key
                           owner
                           focus
                           focus-function
                           timeout
                           dismiss-on-input-p
                           prepare-dialog
                           before-iteration
                           reap-before-poll
                           after-poll
                           windows-function
                           frame-function
                           iteration-guard
                           (abort-restart-report "Cancel the current desktop operation."))
  "Run DIALOG as a synchronous desktop modal operation.

This is the complete neutral modal engine: UI-thread marshaling, modal state,
show/native focus, optional pane focus, sibling input blocking, timeout/native
close, nested event progression, accepted/aborted result, and synchronous
teardown. Host-specific policy is limited to narrow hooks at the same points
used by the pre-migration loop.

OWNER, when given, becomes DIALOG's owner through (SETF WINDOW-OWNER) on the
UI thread before PREPARE-DIALOG runs.
PREPARE-DIALOG runs on the UI thread before modal state is entered.
FOCUS-FUNCTION is the host policy seam for focusing the requested pane. When
omitted, the neutral FOCUS-PANE operation focuses it in DIALOG.
REAP-BEFORE-POLL runs before POLL-DESKTOP-EVENTS; AFTER-POLL runs immediately
after it. WINDOWS-FUNCTION supplies the live native windows whose should-close
flag must be reaped. FRAME-FUNCTION receives DT for the final step of a tick.
ITERATION-GUARD, such as CALL-GUARDED-ITERATION, receives each tick as a thunk;
without it an error in a tick propagates out of the modal operation."
  (call-on-ui-thread
   (lambda ()
     (let ((outcome
             (catch '%modal-exit
               (unwind-protect
                   (progn
                     (when owner
                       (setf (window-owner dialog) owner))
                     (when prepare-dialog
                       (funcall prepare-dialog))
                     (call-with-modal-state
                      dialog dismiss-on-input-p
                      (lambda ()
                        (show-window dialog)
                        (%glfw:focus dialog)
                        (when focus
                          (if focus-function
                              (funcall focus-function focus)
                              (focus-pane dialog focus)))
                        (%set-other-windows-enabled dialog nil)
                        (let ((start (get-internal-real-time))
                              (deadline (and timeout
                                             (+ (get-internal-real-time)
                                                (round (* timeout
                                                          internal-time-units-per-second))))))
                          (loop until (or (%glfw:should-close-p dialog)
                                          (and deadline
                                               (>= (get-internal-real-time) deadline))
                                          (not (eq dialog (current-modal-window))))
                                do (%call-modal-iteration
                                    (lambda ()
                                      (when before-iteration
                                        (funcall before-iteration))
                                      (when reap-before-poll
                                        (funcall reap-before-poll))
                                      (poll-desktop-events :timeout (float 1/60 0d0))
                                      (when after-poll
                                        (funcall after-poll))
                                      (when windows-function
                                        (let ((dead
                                                (loop for window in (funcall windows-function)
                                                      when (and (not (eq window dialog))
                                                                (%glfw:pointer window)
                                                                (%glfw:should-close-p window))
                                                        collect window)))
                                          (dolist (window dead)
                                            (alloy:deallocate window))))
                                      (when frame-function
                                        (let ((dt (float
                                                   (/ (- (get-internal-real-time) start)
                                                      internal-time-units-per-second)
                                                   0f0)))
                                          (setf start (get-internal-real-time))
                                          (funcall frame-function dt))))
                                    iteration-guard
                                    abort-restart-report)))))
                     (%make-modal-result nil nil))
                 (%set-other-windows-enabled dialog t)
                 ;; The existing engine disposes the modal synchronously before
                 ;; returning. Preserve that order rather than leaving a native
                 ;; should-close window for a later outer-loop iteration.
                 (when (%glfw:pointer dialog)
                   (alloy:deallocate dialog))))))
       (if (%modal-result-p outcome)
           (values (%modal-result-value outcome)
                   (%modal-result-accepted-p outcome))
           (values nil nil))))))

;;; Modal input gating belonged to the OM bridge only because the modal engine
;;; lived there. It is desktop behavior and shares the same result authority.

(defmethod alloy:handle :around ((event alloy:input-event)
                                  (window windowing:window))
  (cond ((let ((modal-window (current-modal-window)))
           (and modal-window (not (eq window modal-window))))
         nil)
        ((and (eq window (modal-dismiss-on-input-window))
              (or (typep event 'alloy:pointer-down)
                  (typep event 'alloy:key-down)))
         (abort-modal))
        (t (call-next-method))))

(defmethod alloy:handle :around ((event alloy:text-event)
                                  (window windowing:window))
  (cond ((let ((modal-window (current-modal-window)))
           (and modal-window (not (eq window modal-window))))
         nil)
        ((eq window (modal-dismiss-on-input-window))
         (abort-modal))
        (t (call-next-method))))
