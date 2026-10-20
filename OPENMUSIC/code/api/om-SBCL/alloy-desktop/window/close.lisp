(in-package :alloy-desktop)

;;; Single close authority: every path that ends a window -- explicit close, the
;;; native close button, the frame reaper, session teardown -- runs the closing
;;; hook and the retirement chain exactly once, while the window is registered.

(defgeneric window-close-allowed-p (window)
  (:documentation "Whether WINDOW may close now; a host vetoes here.")
  (:method ((window t)) t))

(defun %forget-window (window)
  "Drop WINDOW from the registry, focus and z-order, running WINDOW-CLOSED once."
  (let ((registered-p (window-registered-p window)))
    (forget-window-focus window)
    (unregister-window window)
    (%forget-window-z-order window)
    (retire-share-window window)
    (when registered-p
      (window-closed window))
    window))

(defun %fire-window-closing-once (window &key (call-hook t))
  "Run WINDOW-CLOSING and the retirement chain exactly once per window."
  (let ((state (registered-window-state window)))
    (when (and state (not (window-closing-fired-p state)))
      (setf (window-closing-fired-p state) t)
      (when call-hook
        ;; the funeral must complete even when a host closing hook signals
        (handler-case (window-closing window)
          (serious-condition (c)
            (format *error-output* "[desktop-close] ~A: ~A~%"
                    (type-of window) (princ-to-string c)))))
      (run-window-retirement window :continue-on-error-p nil)
      t)))

(defgeneric %close-owned-windows (owner)
  (:documentation "Retire transient state, then destroy the owner's children."))

(defmethod %close-owned-windows ((owner window))
  "Destroy OWNER's owned windows now, each one running its own closing hook."
  (dolist (child (owned-windows owner))
    (when (typep child 'window)
      (if (window-alive-p child)
          (alloy:deallocate child)
          (close-window child :force t)))))

(defmethod close-window ((window window) &key force now)
  "Close WINDOW unless vetoed: owned windows, closing hook (skipped when FORCE), retirement, unregistration, then the native close (immediate when NOW)."
  (when (or force (window-close-allowed-p window))
    (%close-owned-windows window)
    (%fire-window-closing-once window :call-hook (not force))
    (%forget-window window)
    (when (window-alive-p window)
      (if now
          (alloy:deallocate window)
          (windowing:close window)))
    t))

;; the native destroy is the primary deallocate method, so the funeral runs
;; before it, while the window and its context are still alive
(defmethod alloy:deallocate :before ((window window))
  (%close-owned-windows window)
  (%fire-window-closing-once window))

(defmethod alloy:deallocate :after ((window window))
  (%forget-window window))

;; GLFW has already raised should-close-p when this event arrives, and every
;; reaper collects on that flag -- including a modal loop the veto itself may
;; run. Disarm first; re-arm only once the close is allowed.
(defmethod alloy:handle :around ((event windowing:close) (window window))
  (when (window-alive-p window)
    (setf (%glfw:should-close-p window) nil))
  (when (window-close-allowed-p window)
    (when (window-alive-p window)
      (setf (%glfw:should-close-p window) t))
    (call-next-method)))

(defun reap-dead-windows ()
  "Drop every registered window whose native handle already died, reporting each one."
  (dolist (window (remove-if #'window-alive-p (registered-windows)))
    (format *error-output* "[desktop-liveness] ~A: registered with a dead native handle; dropped~%"
            (%window-label window))
    (finish-output *error-output*)
    (%forget-window window)))

(defun reap-closed-windows (screen)
  "Destroy every native window of SCREEN whose close was requested; call it outside any GLFW callback, before the frame."
  (let ((dead (loop for w being the hash-values of (windowing::windows screen)
                    when (and (window-alive-p w) (%glfw:should-close-p w))
                      collect w)))
    (dolist (w dead) (alloy:deallocate w))))

(defun close-all-windows ()
  "Destroy every registered window, reporting a failing teardown and falling back to the native destroy, then clear the registry."
  (dolist (window (registered-windows))
    (handler-case
        ;; report from a handler-bind: a handler-case clause runs AFTER the
        ;; unwind and its backtrace would show this loop, not the origin
        (handler-bind ((serious-condition
                         (lambda (c)
                           (format *error-output* "[desktop-teardown] ~A: ~A~%"
                                   (type-of window) (princ-to-string c))
                           (sb-debug:print-backtrace :stream *error-output* :count 15)
                           (finish-output *error-output*))))
          (when (window-alive-p window)
            (alloy:deallocate window)))
      (serious-condition ()
        ;; the primary destroy never ran if a :before method failed; without this
        ;; the native window outlives the screen it was created on
        (handler-case (when (window-alive-p window)
                        (%glfw:destroy window))
          (serious-condition (c)
            (format *error-output* "[desktop-teardown] ~A destroy: ~A~%"
                    (type-of window) (princ-to-string c))
            (finish-output *error-output*))))))
  (clear-window-registry)
  (%clear-window-z-order))
