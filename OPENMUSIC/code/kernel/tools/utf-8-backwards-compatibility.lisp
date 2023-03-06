(in-package :om)

;;;
;;; try to handle latin-1 files gracefully in default utf-8 environment
;;; 


;;
;; handle external-format-errors from 'with-open-file
;;

(defmacro with-safe-open-file (args &body body)
  (let ((default-format :utf-8)
        (backup-format :latin-1))
    `(handler-case (with-open-file ,(append args (list :external-format default-format))
		     ,@body)
       (sys::external-format-error ()
         (progn (print (format nil "using default utf-8 as external format threw error: could not load file as ~a. Now trying with ~a"
                               ,default-format ,backup-format))
		(with-open-file ,(append args (list :external-format backup-format))
                  ,@body))))))


