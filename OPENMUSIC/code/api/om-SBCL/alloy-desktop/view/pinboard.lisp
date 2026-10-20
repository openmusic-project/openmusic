(in-package :alloy-desktop)

;;; Flat realization of pinboard objects: a nested item is entered straight into its
;;; host's layout (and the window focus), as CAPI draws pinboard objects on their owner.

(defun enter-flat (host child &optional focus)
  "Enter CHILD into HOST's layout and into FOCUS when not yet attached; true only when it entered now."
  (when (and (typep host 'alloy:layout)
             (typep child 'alloy:layout-element))
    (unless (layout-attached-p child)
      (alloy:enter child host)
      (when (and focus (typep child 'alloy:focus-element) (not (focus-attached-p child)))
        (alloy:enter child focus))
      t)))

(defun leave-flat (host child)
  "Take CHILD out of its focus parent and, when HOST is a layout holding it, out of HOST."
  (let ((fparent (focus-attached-p child)))
    (when fparent (alloy:leave child fparent)))
  (when (and (typep host 'alloy:layout) (layout-attached-p child))
    (alloy:leave child host)))
