(in-package :om-api)

(export '(om-api-init) :om-api)

(defvar *api-init-func-list* nil)
(setf *api-init-func-list* nil)

(defun om-api-add-init-func (func-name)
  (unless (member func-name *api-init-func-list* :test 'equal)
    (push func-name *api-init-func-list*)))

(defun om-api-init ()
  (mapc 'funcall (reverse oa::*api-init-func-list*)))
