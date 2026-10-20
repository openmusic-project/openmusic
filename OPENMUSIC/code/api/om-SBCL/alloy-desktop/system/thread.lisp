(in-package :alloy-desktop)

(defparameter *process-initial-bindings* '(*package*)
  "Specials that a thread started for a desktop application receives a binding of its own for, as LispWorks gives every process through mp:*process-initial-bindings* (lw-mp-87.htm:44). Pass it as the :INITIAL-BINDINGS of MAKE-DESKTOP-THREAD, DESKTOP-THREAD-ENTRY and RUN-APPLICATION.")

(defun desktop-thread-entry (function &key initial-bindings (armed-traps '()))
  "Return a thunk running FUNCTION with private bindings of the INITIAL-BINDINGS specials, valued as they are at this call, and with exactly ARMED-TRAPS enabled."
  (let ((values (mapcar #'symbol-value initial-bindings)))
    (lambda ()
      (progv initial-bindings values
        (set-float-traps armed-traps)
        (funcall function)))))

(defun make-desktop-thread (function &key name initial-bindings (armed-traps '()))
  "Start a thread on FUNCTION through DESKTOP-THREAD-ENTRY and return the thread."
  (bordeaux-threads:make-thread
   (desktop-thread-entry function :initial-bindings initial-bindings
                                  :armed-traps armed-traps)
   :name name))

(defun kill-thread (thread)
  "Terminate THREAD when it is alive, aborting through SB-THREAD:ABORT-THREAD when it is the calling thread, which BORDEAUX-THREADS:DESTROY-THREAD refuses."
  (when (and thread (bordeaux-threads:thread-alive-p thread))
    (if (eq thread (bordeaux-threads:current-thread))
        (sb-thread:abort-thread)
        (bordeaux-threads:destroy-thread thread))))

(defun find-thread (name)
  "Return the first live thread whose name is the string designator NAME."
  (find-if (lambda (thread)
             (equal (bordeaux-threads:thread-name thread) (string name)))
           (bordeaux-threads:all-threads)))

(defun thread-state (thread)
  "Return :ALIVE when THREAD is a running thread and :DEAD otherwise."
  (if (and thread (bordeaux-threads:thread-alive-p thread))
      :alive
      :dead))
