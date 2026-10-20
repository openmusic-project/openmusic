(in-package :alloy-desktop)

(defun poll-desktop-events (&key timeout)
  "Poll desktop events through the active GLFW backend.

TIMEOUT follows GLFW:POLL-EVENTS semantics: NIL polls without waiting, T waits
until an event arrives, and a real value waits for at most that many seconds."
  (%glfw:poll-events :timeout timeout))

;;; Error boundary of the event loop.

(defun call-with-abort-restart (thunk)
  "Run THUNK under an ABORT restart that cancels THUNK only, so the caller keeps serving its queue."
  (with-simple-restart (abort "Cancel the current desktop operation.")
    (funcall thunk)))

(defun capture-backtrace (&optional (count 64))
  "Return the stack as it is now, so call it from a handler-bind while the signalling frames are live."
  (with-output-to-string (stream)
    ;; a frame's print-object can signal anything; say so IN the backtrace rather
    ;; than answer an empty string that hides why the diagnosis is missing
    (handler-case (sb-debug:print-backtrace :stream stream :count count)
      (error (c) (format stream "~&<backtrace unavailable: ~A>~%" c)))))

(defun condition-signature (condition)
  "Return the string that identifies CONDITION for backtrace deduplication."
  (let ((text (princ-to-string condition)))
    (format nil "~A|~A" (type-of condition) (subseq text 0 (min 90 (length text))))))

(defun call-guarded (thunk new-signature-p report)
  "Run THUNK under an abort restart and pass an error to REPORT with the backtrace captured at signal time when NEW-SIGNATURE-P accepts it."
  (let ((backtrace nil))
    (handler-case
        (handler-bind ((error (lambda (condition)
                                (unless backtrace
                                  (when (funcall new-signature-p condition)
                                    (setf backtrace (capture-backtrace)))))))
          (call-with-abort-restart thunk))
      (error (condition) (funcall report condition backtrace)))))

(defun %iteration-signature-new-p (condition)
  (not (equal (condition-signature condition) *iteration-error-signature*)))

(defun %report-iteration-error (condition &optional backtrace)
  (format *error-output* "~&[desktop-loop] ~A~%" (princ-to-string condition))
  (let ((signature (condition-signature condition)))
    (unless (equal signature *iteration-error-signature*)
      (setf *iteration-error-signature* signature)
      (write-string (or backtrace (capture-backtrace)) *error-output*)))
  (finish-output *error-output*))

(defun call-guarded-iteration (thunk)
  "Run one event-loop iteration THUNK so an error aborts that iteration only and is reported with one backtrace per distinct error."
  (call-guarded thunk #'%iteration-signature-new-p #'%report-iteration-error))

;;; The desktop event loop.

(defun %window-render-enabled-p (window)
  (not (window-render-suspended-p window)))

(defun frame-once (windows-fn dt &key (window-enabled-p #'%window-render-enabled-p)
                                      (ui-task-runner #'funcall))
  "One frame of the windows WINDOWS-FN returns, DT seconds after the previous one:
FRAME-DESKTOP-WINDOWS with a failure in one window reported and confined to that
window. WINDOW-ENABLED-P selects the windows framed, by default those whose
rendering the host does not suspend (WINDOW-RENDER-SUSPENDED-P); UI-TASK-RUNNER
runs each queued UI task."
  #+darwin (drain-retired-nsmenus)
  (frame-desktop-windows windows-fn dt
                         :window-enabled-p window-enabled-p
                         :task-runner #'call-guarded-on-window
                         :ui-task-runner ui-task-runner))

(defun %no-window-left-p ()
  "True when every registered window is dead or asked to close, or none is registered."
  (or (zerop (window-registry-count))
      (every (lambda (window)
               (or (not (window-alive-p window))
                   (%glfw:should-close-p window)))
             (registered-windows))))

(defmacro %guarded-loop-iteration (&body body)
  `(call-guarded-iteration
    (lambda ()
      ;; this tick enters the native toolkit, whose own layout code divides by
      ;; zero and expects IEEE infinity back
      (mask-float-traps)
      ,@body)))

(defun run-event-loop (screen &key (until #'%no-window-left-p)
                                   (window-enabled-p #'%window-render-enabled-p)
                                   (ui-task-runner #'funcall))
  "Pump SCREEN's events and frame its live windows about 60 times a second until
UNTIL, a function of no arguments, answers true; by default once every registered
window is dead or asked to close. Each pass polls the events, settles a popup
chain that lost the focus, destroys the windows whose close was requested, then
runs FRAME-ONCE over the live windows with the seconds since the previous pass
(WINDOW-ENABLED-P and UI-TASK-RUNNER are passed on). An error aborts that pass
only and is reported."
  (let ((start (get-internal-real-time)))
    ;; the reap runs BEFORE the until clause, every pass: that clause scans the
    ;; registry outside the guard, and a loop that ends on its very first test
    ;; still owes the caller a registry with no dead entries left in it
    (loop do (%guarded-loop-iteration (reap-dead-windows))
          until (funcall until)
          do (%guarded-loop-iteration
               (poll-desktop-events :timeout (float 1/60 0d0))
               (popup-resolve-focus-loss)
               (reap-closed-windows screen)
               (let ((dt (float (/ (- (get-internal-real-time) start)
                                   internal-time-units-per-second)
                                0f0)))
                 (setf start (get-internal-real-time))
                 (frame-once #'live-windows dt
                             :window-enabled-p window-enabled-p
                             :ui-task-runner ui-task-runner))))))
