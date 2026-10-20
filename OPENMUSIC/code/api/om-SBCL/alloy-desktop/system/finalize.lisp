(in-package :alloy-desktop)

(defun finalize-on-gc (owner thunk)
  "Call THUNK (no arguments) once OWNER is garbage-collected; THUNK must close over the resource to free and never over OWNER, which it would otherwise keep alive."
  (sb-ext:finalize owner thunk :dont-save t)
  owner)

(defun cancel-finalize (owner)
  "Cancel OWNER's GC finalizers, if any."
  (sb-ext:cancel-finalization owner)
  owner)

(defstruct (managed (:constructor %make-managed (box destructor)))
  box destructor)

(defun manage-resource (resource destructor)
  "Wrap RESOURCE in a MANAGED handle whose DESTRUCTOR (one argument) runs on RESOURCE when the handle is collected or when FREE-MANAGED is called."
  (let ((box (list resource)))
    (finalize-on-gc box (lambda () (funcall destructor resource)))
    (%make-managed box destructor)))

(defun managed-value (m)
  "Return the live resource of M, or NIL once it has been freed."
  (car (managed-box m)))

(defun free-managed (m)
  "Free the resource of M now and cancel its GC finalizer, doing nothing when it is already freed."
  (let ((box (managed-box m)))
    (when (car box)
      (funcall (managed-destructor m) (car box))
      (setf (car box) nil)
      (cancel-finalize box))))
