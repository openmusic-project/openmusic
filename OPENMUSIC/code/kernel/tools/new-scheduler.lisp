
(in-package :om)

(defun clock-time () (get-internal-real-time))

(defmacro start (&body body)
  `(eval ,.body))
