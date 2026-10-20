(in-package :alloy-desktop)

(defun frame-desktop-windows (windows-fn dt
                              &key
                                (window-enabled-p (constantly t))
                                (task-runner (lambda (window thunk)
                                               (declare (ignore window))
                                               (funcall thunk)))
                                (ui-task-runner #'funcall))
  "One frame for the windows WINDOWS-FN returns: animation update for every
enabled window, then a full render only for enabled windows whose retained tree
is dirty. Animation runs before the dirty check so caret, hover, timers, or other
animations can request the render that follows. Child invalidation is expected
to propagate to its window so stale descendant flags do not keep an idle window
rendering. WINDOWS-FN is called separately for the update and render passes.
WINDOW-ENABLED-P and TASK-RUNNER let a host bridge apply window policy without
moving it into ALLOY-DESKTOP. UI-TASK-RUNNER is the corresponding per-task
policy seam. The UI queue is drained immediately after each enabled window's
animation update, preserving the historical desktop tick order; after the first
window drains the current snapshot, later windows normally observe an empty
queue."
  (dolist (window (funcall windows-fn))
    (let ((window window))
      (when (funcall window-enabled-p window)
        (funcall task-runner
                 window
                 (lambda ()
                   (org.shirakumo.alloy.animation:update window dt)
                   ;; Keep the pre-migration :AFTER semantics: if UPDATE exits
                   ;; non-locally, this drain is skipped for this window.
                   (%drain-ui-tasks :task-runner ui-task-runner))))))
  (dolist (window (funcall windows-fn))
    (let ((window window))
      (when (funcall window-enabled-p window)
        (funcall task-runner
                 window
                 (lambda ()
                   (when (alloy:render-needed-p window)
                     (alloy:render window window)))))))
  nil)
