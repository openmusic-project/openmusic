(in-package :om-api)

;;; om-show-documentation: defined in kernel/doc/documentation.lisp; only exported here.
;;; om-page-setup / om-print-window: defined in print.lisp; only exported here.

(export '(quoted-form-p
          lambda-expression-p
          om-report-condition
          om-read-list-from-string
          om-text-to-lines
          memq
          while
          om-copy-command
          om-paste-command
          om-cut-command
          om-random-value
          om-show-documentation
          om-page-setup
          om-print-window
          om-closure-function)
        :om-api)

(defun memq (item list)
  (member item list :test #'eq))

(defmacro %car (x)
  `(car (the cons ,x)))

(defmacro %cdr (x)
  `(cdr (the cons ,x)))

(defmacro %cadr (x)
  `(cadr (the cons ,x)))

(defun quoted-form-p (form)
  (and (consp form)
       (eq (%car form) 'quote)
       (consp (%cdr form))
       (null (%cdr (%cdr form)))))

(defun lambda-expression-p (form)
  (and (consp form)
       (eq (%car form) 'lambda)
       (consp (%cdr form))
       (listp (%cadr form))
       (null (cdr (last form)))))

(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
        (go ,testlab)
        ,toplab
        (progn ,@body)
        ,testlab
        (when ,test (go ,toplab)))))

(defmethod om-report-condition ((c condition))
  (format nil "~A" c))

(defun om-read-list-from-string (string &optional (pos 0))
  "Read successive Lisp forms from STRING starting at POS; return them as a list."
  (let ((eof (list :eof))
        (acc nil))
    (loop
      (multiple-value-bind (val newpos)
          (handler-case (read-from-string string nil eof :start pos)
            (end-of-file () (values eof pos))
            (reader-error () (values eof pos)))
        (when (eq val eof) (return (nreverse acc)))
        (push val acc)
        (when (<= newpos pos) (return (nreverse acc)))
        (setf pos newpos)))))

(defun om-text-to-lines (text)
  (let ((p2 (position #\Newline text))
        (rep nil))
    (loop while p2 do
      (push (subseq text 0 p2) rep)
      (setf text (subseq text (1+ p2)))
      (setf p2 (position #\Newline text)))
    (push text rep)
    (nreverse rep)))

(defmethod om-copy-command ((self t))
  (print "Nothing to copy to clipboard"))

(defmethod om-paste-command ((self t))
  (print "Nothing to paste from clipboard"))

(defmethod om-cut-command ((self t))
  (print "Nothing to cut and copy to clipboard"))

(defun om-random-value (num)
  (cond ((zerop num) 0)
        ((minusp num) (- (random (- num))))
        (t (random num))))

(defun om-closure-function (fun)
  (declare (ignore fun))
  t)

(in-package :cl-user)

(defun set-nthcdr (index list new-value)
  "If INDEX is 0, just return NEW-VALUE."
  (if (not (zerop index))
      (rplacd (nthcdr (1- index) list) new-value))
  new-value)

(handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
  (sb-ext:without-package-locks
    (defsetf nthcdr set-nthcdr)))
