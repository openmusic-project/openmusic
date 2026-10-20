(in-package :alloy-desktop)

;;; Generic desktop UI executor.
;;;
;;; This layer owns only execution mechanics: which thread owns the desktop UI,
;;; the FIFO task queue, asynchronous posting, queue draining, and synchronous
;;; marshaling.  Host policy (restarts, dynamic bindings, logging, etc.) belongs
;;; in the bridge and is supplied through TASK-RUNNER when the queue is drained.

(defvar *ui-thread* nil
  "Thread that currently owns desktop UI operations, or NIL when no desktop session is active.")

(defvar *ui-tasks* nil)
(defvar *ui-tasks-lock* (bordeaux-threads:make-lock "alloy-desktop-ui-tasks"))

(defvar *draining-ui-tasks* nil
  "True while the current thread is draining the UI queue.")

(defun ui-thread ()
  *ui-thread*)

(defun (setf ui-thread) (thread)
  (setf *ui-thread* thread))

(defun adopt-ui-thread (&optional (thread (bordeaux-threads:current-thread)))
  "Make THREAD the owner of desktop UI operations and return it."
  (setf *ui-thread* thread))

(defun release-ui-thread (&optional thread)
  "Release the current UI thread.

When THREAD is non-NIL, clear the owner only when it is the current owner.  This
makes unwind cleanup safe if ownership has already moved elsewhere. Pending
tasks are discarded so they cannot run in a later desktop session. Synchronous
waiters observe the released owner and signal instead of executing stale work."
  (bordeaux-threads:with-lock-held (*ui-tasks-lock*)
    (when (or (null thread) (eq thread *ui-thread*))
      (setf *ui-thread* nil
            *ui-tasks* nil)))
  nil)

(defun on-ui-thread-p ()
  (and *ui-thread*
       (eq (bordeaux-threads:current-thread) *ui-thread*)))

(defun post-to-ui-thread (thunk)
  "Append THUNK to the desktop UI queue.  THUNK runs on a later desktop scheduler tick."
  (check-type thunk function)
  (bordeaux-threads:with-lock-held (*ui-tasks-lock*)
    (setf *ui-tasks* (nconc *ui-tasks* (list thunk))))
  thunk)

(defun %unqueue-ui-task (task)
  "Remove TASK from the pending UI queue; true when it had not yet been taken."
  (bordeaux-threads:with-lock-held (*ui-tasks-lock*)
    (when (member task *ui-tasks*)
      (setf *ui-tasks* (remove task *ui-tasks*))
      t)))

(defun %drain-ui-tasks (&key (task-runner #'funcall))
  "Drain the current FIFO queue exactly once.

TASK-RUNNER receives each queued thunk.  It is the host-policy seam: a bridge can
wrap task execution in application-specific restarts or error handling without
putting that policy in ALLOY-DESKTOP.  Tasks posted while a drain is in progress
remain queued for the next drain.  Recursive drains on the same thread are no-ops."
  (check-type task-runner function)
  (unless *draining-ui-tasks*
    (let ((*draining-ui-tasks* t)
          (queued nil))
      (bordeaux-threads:with-lock-held (*ui-tasks-lock*)
        (setf queued *ui-tasks*
              *ui-tasks* nil))
      (dolist (thunk queued)
        (funcall task-runner thunk))))
  nil)

(defun %call-with-ui-task-drain-scope (function)
  "Run FUNCTION as a fresh UI-task drain scope.

Ordinary recursive queue drains remain suppressed while a queued task is
running. A nested native event-loop iteration, however, is a new desktop tick
and must be able to serve tasks posted after the outer drain took its snapshot.
This internal operation preserves that historical distinction entirely inside
ALLOY-DESKTOP; host bridges do not control drain boundaries."
  (check-type function function)
  (let ((*draining-ui-tasks* nil))
    (funcall function)))

(defun call-on-ui-thread (thunk)
  "Run THUNK on the UI thread and return all of its values.

If no live UI owner exists, or the caller already owns the UI, run inline.
Otherwise enqueue a synchronous task and wait for it.  If the UI owner dies
before taking the task, remove it from the queue and run it inline.  If the UI
owner dies after taking it but before completion, signal an error."
  (check-type thunk function)
  (let ((ui *ui-thread*))
    (if (or (null ui)
            (eq (bordeaux-threads:current-thread) ui)
            (not (bordeaux-threads:thread-alive-p ui)))
        (funcall thunk)
        (let* ((lock (bordeaux-threads:make-lock "alloy-desktop-call-ui"))
               (cv (bordeaux-threads:make-condition-variable))
               (done nil)
               (result nil)
               (condition nil)
               (task (lambda ()
                       (unwind-protect
                            (handler-case
                                (setf result (multiple-value-list (funcall thunk)))
                              (serious-condition (c)
                                (setf condition c)))
                         (bordeaux-threads:with-lock-held (lock)
                           (setf done t)
                           (bordeaux-threads:condition-notify cv))))))
          (bordeaux-threads:with-lock-held (*ui-tasks-lock*)
            (unless (eq ui *ui-thread*)
              (error "ALLOY-DESKTOP UI session ended before a synchronous call"))
            (setf *ui-tasks* (nconc *ui-tasks* (list task))))
          (bordeaux-threads:with-lock-held (lock)
            (loop until done
                  do (bordeaux-threads:condition-wait cv lock :timeout 0.1)
                  until (let ((live *ui-thread*))
                          (and (not done)
                               (or (not (eq live ui))
                                   (not (bordeaux-threads:thread-alive-p live)))))))
          (cond (done
                 (if condition
                     (error condition)
                     (values-list result)))
                ((%unqueue-ui-task task)
                 (funcall thunk))
                (t
                 ;; taken from the queue but never finished: the UI thread died mid-task
                 (error "ALLOY-DESKTOP UI thread terminated during a synchronous call")))))))

