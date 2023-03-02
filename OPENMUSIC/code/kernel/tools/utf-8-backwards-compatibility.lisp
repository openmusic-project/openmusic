;;;
;;; ensure old  latin-1 files can be accessed (load, read) in utf-8 version of OM
;;;
;;; this macro originally copied from Jean's code in om#
;;;


(in-package :om)

(defmacro with-safe-open-file (args &body body)
  (let ((default-format :utf-8)
        (backup-format :latin-1))

    `(handler-bind ((error
                      #'(lambda (err)
                          (format nil "#'with-safe-open-file: an error of type ~a occurred: ~%\"~a\"" (type-of err) err)
                          (abort err))))
       (let ((rep
               (catch 'format-failed
                 (handler-bind ((sys::external-format-error
                                  #'(lambda (err) (declare (ignore err))
                                      (throw 'format-failed ,default-format)
                                      )))

		   ;; main body
                   (with-open-file ,(append args (list :external-format default-format))
                     ,@body)
                   ))))

         (if (equal rep ,default-format)
             (progn (print (format nil "using default utf-8 as external format threw error: could not load file as ~a. Now trying with ~a"
                                   ,default-format ,backup-format))
		    (with-open-file ,(append args (list :external-format backup-format))
                      ,@body))
             rep)
         ))
    ))
