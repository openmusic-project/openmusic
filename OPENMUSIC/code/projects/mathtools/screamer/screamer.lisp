;;; -*- Mode: LISP; Package: (SCREAMER :USE CL :COLON-MODE :EXTERNAL); Base: 10; Syntax: Ansi-common-lisp -*-

;;; LaHaShem HaAretz U'Mloah

;;; Screamer
;;; A portable efficient implementation of nondeterministic Common Lisp
;;; Version 3.20
;;;
;;; Written by:
;;;
;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;
;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;; Copyright 1993 University of Toronto. All rights reserved.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;; this software and associated documentation files (the "Software"), to deal in
;;; the Software without restriction, including without limitation the rights to
;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright and authorship notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Important notice: In this version of Screamer, if Screamer is already
;;; loaded and you wish to recompile the entire file, the recompilation will
;;; proceed much faster if you first do:
;;; (CLRHASH SCREAMER::*FUNCTION-RECORD-TABLE*)

(in-package :screamer)

(declaim (declaration magic))

(defmacro define-screamer-package (defined-package-name &body options)
  `(defpackage ,defined-package-name
     ,@options
     (:shadowing-import-from :screamer :defun :multiple-value-bind :y-or-n-p)
     (:use :cl :screamer)))

(define-screamer-package :screamer-user)

(defmacro defstruct-compile-time (options &body items)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defstruct ,options ,@items)))

(defmacro defvar-compile-time (name &optional initial-value documentation)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defvar ,name ,initial-value ,documentation)))

(defmacro defun-compile-time (function-name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (cl:defun ,function-name ,lambda-list ,@body)
     (eval-when (:compile-toplevel) (compile ',function-name))))

;;; Needed because Allegro has some bogosity whereby (MACRO-FUNCTION <m> <e>)
;;; returns NIL during compile time when <m> is a macro being defined for the
;;; first time in the file being compiled.
(defmacro defmacro-compile-time (function-name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (defmacro ,function-name ,lambda-list ,@body)))

(defparameter *screamer-version* "3.20"
  "The version of Screamer which is loaded. This is currently still 3.20,
while we're considering how to express versions for this copy of Screamer in
the future.")

(defvar-compile-time *dynamic-extent?*
    ;; SBCL cannot stack-allocate LET-bound lambdas that screamer
    ;; currently uses, so setting dynamic-extent to T will only
    ;; generate compiler notes about it inability to do so.
    #-sbcl t
    #+sbcl nil
    "Set to T to enable the dynamic extent optimization, NIL to
disable it. Default is platform dependent.")

(defvar *iscream?* nil
  "T if Screamer is running under ILisp/GNUEmacs with iscream.el loaded.")

(defvar *nondeterministic?* nil "This must be globally NIL.")

(defvar-compile-time *screamer?* nil
  "This must be NIL except when defining internal Screamer functions.")

(defvar-compile-time *nondeterministic-context?* nil
  "This must be globally NIL.")

(defvar-compile-time *local?* nil "This must be globally NIL.")

(defvar-compile-time *block-tags* '() "This must be globally NIL.")

(defvar-compile-time *tagbody-tags* '() "This must be globally NIL.")

(defvar *trail* (make-array 4096 :adjustable t :fill-pointer 0) "The trail.")

(defvar-compile-time *function-record-table* (make-hash-table :test #'equal)
  "The function record table.")

(defvar-compile-time *ordered-lambda-list-keywords*
    '(&optional &rest &key &allow-other-keys &aux)
  "The allowed lambda list keywords in order.")

(defmacro-compile-time choice-point-internal (form)
  ;; note: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
  ;;       trail?
  `(catch 'fail
     (let ((*nondeterministic?* t))
       (unwind-protect ,form
         (block nil
           (tagbody
            loop
              (if (= (fill-pointer *trail*) trail-pointer) (return))
              (funcall (vector-pop *trail*))
              ;; note: This is to allow the trail closures to be garbage
              ;;       collected.
              (setf (aref *trail* (fill-pointer *trail*)) nil)
              (go loop)))))))

(defmacro-compile-time choice-point-external (&rest forms)
  ;; note: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
  ;;       trail?
  `(let ((trail-pointer (fill-pointer *trail*))) ,@forms))

(defmacro-compile-time choice-point (form)
  `(choice-point-external (choice-point-internal ,form)))

(defstruct-compile-time function-record
  function-name
  (lambda-list nil)
  (body nil)
  (callees nil)
  (deterministic? t)
  (old-deterministic? nil)
  (screamer? *screamer?*))

(defstruct-compile-time (nondeterministic-function
                         (:print-function print-nondeterministic-function)
                         (:predicate nondeterministic-function?-internal))
  function)

(defun-compile-time screamer-error (header &rest args)
  (apply
   #'error
   (concatenate
    'string
    header
    "~2%There are eight types of nondeterministic contexts:

  1. the body of a function defined with SCREAMER::DEFUN
  2. the body of a FOR-EFFECTS macro invocation
  3. the body of an ALL-VALUES macro invocation
  4. the first argument of a ONE-VALUE macro invocation
  5. the body of a PRINT-VALUES macro invocation
  6. the second argument of an ITH-VALUE macro invocation
  7. the body of a POSSIBLY? macro invocation
  8. the body of a NECESSARILY? macro invocation.

Note that the default forms of &OPTIONAL and &KEY arguments and the
initialization forms of &AUX variables are always deterministic
contexts even though they may appear inside a SCREAMER::DEFUN.") args))

(defun-compile-time get-function-record (function-name)
  (or (gethash function-name *function-record-table*)
      (setf (gethash function-name *function-record-table*)
            (make-function-record :function-name function-name))))

(defun-compile-time peal-off-documentation-string-and-declarations
    (body &optional documentation-string?)
  ;; note: This will need to be done as well for LOCALLY and MACROLET when we
  ;;       eventually implement them.
  ;; needs work: This requires that the documentation string preceed all
  ;;             declarations which needs to be fixed.
  (let (documentation-string declarations)
    (when (and documentation-string?
               (not (null body))
               (not (null (rest body)))
               (stringp (first body)))
      (setf documentation-string (first body))
      (setf body (rest body)))
    (loop (unless (and (not (null body))
                       (consp (first body))
                       (eq (first (first body)) 'declare))
            (return))
      (push (first body) declarations)
      (pop body))
    (values body (reverse declarations) documentation-string)))

(defun-compile-time self-evaluating? (thing)
  (and (not (consp thing))
       (or (not (symbolp thing))
           (null thing)
           (eq thing t)
           (eq (symbol-package thing) (symbol-package :x)))))

(defun-compile-time quotify (thing)
  (if (self-evaluating? thing) thing `',thing))

(defun-compile-time lambda-expression? (form)
  (and (consp form)
       (eq (first form) 'lambda)
       (or (and (null (rest (last form)))
                (>= (length form) 2)
                (listp (second form)))
           (error "Invalid syntax for LAMBDA expression: ~S" form))))

(defun-compile-time valid-function-name? (function-name)
  (or (and (symbolp function-name) (not (null function-name)))
      (and (consp function-name)
           (eq (first function-name) 'setf)
           (null (rest (last function-name)))
           (= (length function-name) 2)
           (symbolp (second function-name))
           (not (null (second function-name))))))

(defun-compile-time check-function-name (function-name)
  (unless (valid-function-name? function-name)
    (error "Invalid function name: ~S" function-name)))

(defun-compile-time every-other (list)
  (cond ((null list) list)
        ((null (rest list)) list)
        (t (cons (first list) (every-other (rest (rest list)))))))

(defun-compile-time check-lambda-list-internal (lambda-list &optional mode)
  (cond
    ((null lambda-list))
    ((member (first lambda-list) *ordered-lambda-list-keywords* :test #'eq)
     (check-lambda-list-internal (rest lambda-list) (first lambda-list)))
    (t (let ((parameter (first lambda-list)))
         (ecase mode
           ((nil)
            (unless (symbolp parameter)
              (error "Invalid parameter: ~S" parameter)))
           (&optional
            (unless (or (symbolp parameter)
                        (and (consp parameter)
                             (null (rest (last parameter)))
                             (or (= (length parameter) 1)
                                 (= (length parameter) 2)
                                 (and (= (length parameter) 3)
                                      (symbolp (third parameter))))
                             (symbolp (first parameter))))
              (error "Invalid &OPTIONAL parameter: ~S" parameter)))
           (&rest
            (unless (symbolp parameter)
              (error "Invalid &REST parameter: ~S" parameter)))
           (&key
            (unless (or (symbolp parameter)
                        (and (consp parameter)
                             (null (rest (last parameter)))
                             (or (= (length parameter) 1)
                                 (= (length parameter) 2)
                                 (and (= (length parameter) 3)
                                      (symbolp (third parameter))))
                             (or (symbolp (first parameter))
                                 (and (consp (first parameter))
                                      (null (rest (last (first parameter))))
                                      (= (length (first parameter)) 2)
                                      (symbolp (first (first parameter)))
                                      (symbolp (second (first parameter)))))))
              (error "Invalid &KEY parameter: ~S" parameter)))
           (&aux
            (unless (or (symbolp parameter)
                        (and (consp parameter)
                             (null (rest (last parameter)))
                             (or (= (length parameter) 1)
                                 (= (length parameter) 2))
                             (symbolp (first parameter))))
              (error "Invalid &AUX parameter: ~S" parameter)))))
       (check-lambda-list-internal (rest lambda-list) mode))))

(defun-compile-time check-lambda-list (lambda-list)
  (unless (null (rest (last lambda-list)))
    (error "Improper lambda-list: ~S" lambda-list))
  (let ((rest (member '&rest lambda-list :test #'eq)))
    (if rest
        (let ((rest (rest rest)))
          (unless (not (member '&rest rest :test #'eq))
            (error "&REST cannot appear more than once: ~S" lambda-list))
          (unless (and (not (null rest))
                       (not (member (first rest) lambda-list-keywords :test #'eq))
                       (or (null (rest rest))
                           (member (first (rest rest)) lambda-list-keywords
                                   :test #'eq)))
            (error "&REST must be followed by exactly one variable: ~S"
                   lambda-list)))))
  (let ((allow-other-keys (member '&allow-other-keys lambda-list :test #'eq)))
    (if allow-other-keys
        (unless (or (null (rest allow-other-keys))
                    (member (first (rest allow-other-keys)) lambda-list-keywords
                            :test #'eq))
          (error "&ALLOW-OTHER-KEYS must not be followed by a parameter: ~S"
                 lambda-list))))
  (let ((keywords
         (remove-if-not #'(lambda (argument)
                            (member argument lambda-list-keywords :test #'eq))
                        lambda-list)))
    (unless (every #'(lambda (keyword)
                       (member keyword *ordered-lambda-list-keywords* :test #'eq))
                   keywords)
      (error "Invalid lambda list keyword: ~S" lambda-list))
    (unless (every #'(lambda (x y)
                       (member y (member x *ordered-lambda-list-keywords*
                                         :test #'eq)
                               :test #'eq))
                   keywords
                   (rest keywords))
      (error "Invalid order for lambda list keywords: ~S" lambda-list)))
  (check-lambda-list-internal lambda-list))

(defun-compile-time walk-lambda-list-reducing
    (map-function reduce-function screamer? partial? nested? lambda-list
                  environment &optional mode)
  (cond
    ((null lambda-list) (funcall reduce-function))
    ((member (first lambda-list) *ordered-lambda-list-keywords* :test #'eq)
     (walk-lambda-list-reducing map-function
                                reduce-function
                                screamer?
                                partial?
                                nested?
                                (rest lambda-list)
                                environment
                                (first lambda-list)))
    (t (ecase mode
         ((nil &rest &allow-other-keys &aux)
          (walk-lambda-list-reducing map-function
                                     reduce-function
                                     screamer?
                                     partial?
                                     nested?
                                     (rest lambda-list)
                                     environment
                                     mode))
         ((&optional &key)
          (if (and (consp (first lambda-list))
                   (consp (rest (first lambda-list))))
              (funcall
               reduce-function
               (walk map-function reduce-function screamer? partial? nested?
                     (second (first lambda-list)) environment)
               (walk-lambda-list-reducing map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          (rest lambda-list)
                                          environment
                                          mode))
              (walk-lambda-list-reducing map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         (rest lambda-list)
                                         environment
                                         mode)))))))

(defun-compile-time walk-lambda-list
    (map-function reduce-function screamer? partial? nested? lambda-list
                  environment)
  (check-lambda-list lambda-list)
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function lambda-list 'lambda-list)
       (walk-lambda-list-reducing map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  lambda-list
                                  environment))
      (funcall map-function lambda-list 'lambda-list)))

(defun-compile-time walk-block
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper BLOCK: ~S" form))
  (unless (>= (length form) 2)
    (error "BLOCK must have at least one argument, a NAME: ~S" form))
  (unless (symbolp (second form)) (error "NAME must be a symbol: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'block)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest (rest form)))))
      (funcall map-function form 'block)))

(defun-compile-time walk-catch
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper PROGN: ~S" form))
  (unless (>= (length form) 2)
    (error "CATCH must have at least one argument, a TAG: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'catch)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'catch)))

(defun-compile-time walk-eval-when
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper EVAL-WHEN: ~S" form))
  (unless (>= (length form) 2)
    (error "EVAL-WHEN must have at least one argument: ~S" form))
  (unless (listp (second form))
    (error "First argument of EVAL-WHEN must be a list: ~S" form))
  (unless (null (rest (last (second form))))
    (error "Improper list of SITUATIONS: ~S" form))
  (unless (every #'(lambda (situation)
                     (member situation '(:compile-toplevel
                                         :load-toplevel
                                         :execute
                                         compile
                                         load
                                         evel)
                             :test #'eq))
                 (second form))
    (error "Invalid SITUATION: ~S" form))
  (if (member :execute (second form) :test #'eq)
      (walk-progn map-function
                  reduce-function
                  screamer?
                  partial?
                  nested?
                  `(progn ,@(rest (rest form)))
                  environment)
      (funcall map-function nil 'quote)))

(defun-compile-time walk-flet/labels
    (map-function reduce-function screamer? partial? nested? form environment
                  form-type)
  (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
  (unless (>= (length form) 2)
    (error "~S must have BINDINGS: ~S" form-type form))
  (unless (and (listp (second form))
               (null (rest (last (second form))))
               (every #'(lambda (binding)
                          (and (consp binding)
                               (null (rest (last binding)))
                               (>= (length binding) 2)
                               (valid-function-name? (first binding))
                               (listp (second binding))))
                      (second form)))
    (error "Invalid BINDINGS for ~S: ~S" form-type form))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form form-type)
       (if nested?
           (funcall
            reduce-function
            (reduce
             reduce-function
             (mapcar
              #'(lambda (binding)
                  (funcall reduce-function
                           (walk-lambda-list map-function
                                             reduce-function
                                             screamer?
                                             partial?
                                             nested?
                                             (second binding)
                                             environment)
                           (mapcar
                            #'(lambda (subform)
                                (walk map-function
                                      reduce-function
                                      screamer?
                                      partial?
                                      nested?
                                      subform
                                      environment))
                            (peal-off-documentation-string-and-declarations
                             (rest (rest binding)) t))))
              (second form)))
            (reduce reduce-function
                    (mapcar #'(lambda (subform)
                                (walk map-function
                                      reduce-function
                                      screamer?
                                      partial?
                                      nested?
                                      subform
                                      environment))
                            (rest (rest form)))))
           (reduce reduce-function
                   (mapcar #'(lambda (subform)
                               (walk map-function
                                     reduce-function
                                     screamer?
                                     partial?
                                     nested?
                                     subform
                                     environment))
                           (rest (rest form))))))
      (funcall map-function form form-type)))

(defun-compile-time walk-function
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper FUNCTION: ~S" form))
  (unless (= (length form) 2)
    (error "FUNCTION must have one argument: ~S" form))
  (cond ((lambda-expression? (second form))
         (if (and reduce-function nested?)
             (funcall
              reduce-function
              (funcall map-function form 'function-lambda)
              (funcall
               reduce-function
               (walk-lambda-list map-function
                                 reduce-function
                                 screamer?
                                 partial?
                                 nested?
                                 (second (second form))
                                 environment)
               (reduce
                reduce-function
                (mapcar #'(lambda (subform)
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  subform
                                  environment))
                        (peal-off-documentation-string-and-declarations
                         (rest (rest (second form))) t)))))
             (funcall map-function form 'function-lambda)))
        ((valid-function-name? (second form))
         (cond
           ((symbolp (second form))
            (if (or (special-operator-p (second form))
                    (macro-function (second form) environment))
                (error "You can't reference the FUNCTION of a special form or~%~
                      macro: ~S"
                       form)
                (funcall map-function form 'function-symbol)))
           (t (funcall map-function form 'function-setf))))
        (t (error "Invalid argument to FUNCTION: ~S" form))))

(defun-compile-time walk-go (map-function form)
  (unless (null (rest (last form))) (error "Improper GO: ~S" form))
  (unless (= (length form) 2) (error "GO must have one argument: ~S" form))
  (unless (or (symbolp (second form)) (integerp (second form)))
    (error "TAG of GO must be a symbol or integer: ~S" form))
  (funcall map-function form 'go))

(defun-compile-time walk-if
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper IF: ~S" form))
  (unless (or (= (length form) 3) (= (length form) 4))
    (error "IF must have two or three arguments: ~S" form))
  (if reduce-function
      (if (= (length form) 4)
          (funcall reduce-function
                   (funcall map-function form 'if)
                   (funcall reduce-function
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  (second form)
                                  environment)
                            (funcall reduce-function
                                     (walk map-function
                                           reduce-function
                                           screamer?
                                           partial?
                                           nested?
                                           (third form)
                                           environment)
                                     (walk map-function
                                           reduce-function
                                           screamer?
                                           partial?
                                           nested?
                                           (fourth form)
                                           environment))))
          (funcall reduce-function
                   (funcall map-function form 'if)
                   (funcall reduce-function
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  (second form)
                                  environment)
                            (walk map-function
                                  reduce-function
                                  screamer?
                                  partial?
                                  nested?
                                  (third form)
                                  environment))))
      (funcall map-function form 'if)))

(defun-compile-time walk-let/let*
    (map-function reduce-function screamer? partial? nested? form environment
                  form-type)
  (unless (null (rest (last form))) (error "Improper ~S: ~S" form-type form))
  (unless (>= (length form) 2)
    (error "~S must have BINDINGS: ~S" form-type form))
  (unless (and (listp (second form))
               (null (rest (last (second form))))
               (every #'(lambda (binding)
                          (or (symbolp binding)
                              (and (consp binding)
                                   (null (rest (last binding)))
                                   (or (= (length binding) 1)
                                       (= (length binding) 2))
                                   (symbolp (first binding)))))
                      (second form)))
    (error "Invalid BINDINGS for ~S: ~S" form-type form))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form form-type)
       (funcall reduce-function
                (reduce reduce-function
                        (mapcar #'(lambda (binding)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          (second binding)
                                          environment))
                                (remove-if-not
                                 #'(lambda (binding)
                                     (and (consp binding)
                                          (= (length binding) 2)))
                                 (second form))))
                (reduce reduce-function
                        (mapcar #'(lambda (subform)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          subform
                                          environment))
                                (peal-off-documentation-string-and-declarations
                                 (rest (rest form)))))))
      (funcall map-function form form-type)))

(defun-compile-time walk-multiple-value-call
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper MULTIPLE-VALUE-CALL: ~S" form))
  (unless (>= (length form) 2)
    (error "MULTIPLE-VALUE-CALL must have at least one argument, a FUNCTION: ~S"
           form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'multiple-value-call)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'multiple-value-call)))

(defun-compile-time walk-multiple-value-prog1
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper MULTIPLE-VALUE-PROG1: ~S" form))
  (unless (>= (length form) 2)
    (error "MULTIPLE-VALUE-PROG1 must have at least one argument, a FORM: ~S"
           form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'multiple-value-prog1)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'multiple-value-prog1)))

(defun-compile-time walk-progn
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper PROGN: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'progn)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'progn)))

(defun-compile-time walk-progv
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper PROGV: ~S" form))
  (unless (>= (length form) 3)
    (error "PROGV must have at least two arguments: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'progv)
               (funcall reduce-function
                        (funcall reduce-function
                                 (walk map-function
                                       reduce-function
                                       screamer?
                                       partial?
                                       nested?
                                       (second form)
                                       environment)
                                 (walk map-function
                                       reduce-function
                                       screamer?
                                       partial?
                                       nested?
                                       (third form)
                                       environment))
                        (reduce reduce-function
                                (mapcar #'(lambda (subform)
                                            (walk map-function
                                                  reduce-function
                                                  screamer?
                                                  partial?
                                                  nested?
                                                  subform
                                                  environment))
                                        (rest (rest (rest form)))))))
      (funcall map-function form 'progv)))

(defun-compile-time walk-quote (map-function form)
  (unless (null (rest (last form))) (error "Improper QUOTE: ~S" form))
  (unless (= (length form) 2)
    (error "QUOTE must have one argument: ~S" form))
  (funcall map-function (second form) 'quote))

(defun-compile-time walk-return-from
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper RETURN-FROM: ~S" form))
  (unless (or (= (length form) 2) (= (length form) 3))
    (error "RETURN-FROM must have one or two arguments,~%~
          a NAME and an optional RESULT: ~S" form))
  (unless (symbolp (second form)) (error "NAME must be a symbol: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'return-from)
               (walk map-function
                     reduce-function
                     screamer?
                     partial?
                     nested?
                     (if (= (length form) 3) (third form) nil)
                     environment))
      (funcall map-function form 'return-from)))

(defun-compile-time walk-setq
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper SETQ: ~S" form))
  (unless (every #'symbolp (every-other (rest form)))
    (error "Invalid destination for SETQ: ~S" form))
  (unless (evenp (length (rest form)))
    (error "Odd number of arguments to SETQ: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'setq)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (every-other (rest (rest form))))))
      (funcall map-function form 'setq)))

(defun-compile-time walk-tagbody
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper TAGBODY: ~S" form))
  (unless (every #'(lambda (subform)
                     (or (symbolp subform) (integerp subform) (listp subform)))
                 (rest form))
    (error "A subforms of a TAGBODY must be symbols, integers or lists: ~S"
           form))
  (let ((tags (remove-if #'consp (rest form))))
    (unless (= (length tags) (length (remove-duplicates tags)))
      (error "TAGBODY has duplicate TAGs: ~S" form)))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'tagbody)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (remove-if-not #'consp (rest form)))))
      (funcall map-function form 'tagbody)))

(defun-compile-time walk-the
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper THE: ~S" form))
  (unless (= (length form) 3) (error "THE must have two arguments: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (walk map-function
                     reduce-function
                     screamer?
                     partial?
                     nested?
                     (third form)
                     environment)
               (funcall map-function form 'the))
      (funcall map-function form 'the)))

(defun-compile-time walk-throw
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper THROW: ~S" form))
  (unless (= (length form) 3)
    (error "THROW must have two arguments, a TAG and a RESULT: ~S" form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'throw)
               (funcall reduce-function
                        (walk map-function
                              reduce-function
                              screamer?
                              partial?
                              nested?
                              (second form)
                              environment)
                        (walk map-function
                              reduce-function
                              screamer?
                              partial?
                              nested?
                              (third form)
                              environment)))
      (funcall map-function form 'throw)))

(defun-compile-time walk-unwind-protect
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper UNWIND-PROTECT: ~S" form))
  (unless (>= (length form) 2)
    (error "UNWIND-PROTECT must have at least one argument, a PROTECTED-FORM: ~S"
           form))
  (if reduce-function
      (funcall
       reduce-function
       (funcall map-function form 'unwind-protect)
       (funcall reduce-function
                (walk map-function
                      reduce-function
                      screamer?
                      partial?
                      nested?
                      (second form)
                      environment)
                (reduce reduce-function
                        (mapcar #'(lambda (subform)
                                    (walk map-function
                                          reduce-function
                                          screamer?
                                          partial?
                                          nested?
                                          subform
                                          environment))
                                (rest (rest form))))))
      (funcall map-function form 'unwind-protect)))

(defun-compile-time walk-for-effects
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper FOR-EFFECTS: ~S" form))
  ;; note: We used to think that we should never walk the body of FOR-EFFECTS
  ;;       as we thought that the walker would get confused on the code
  ;;       generated by FOR-EFFECTS and that FOR-EFFECTS called
  ;;       CPS-CONVERT-PROGN on its body and that CPS-CONVERT-PROGN did the
  ;;       walk for us. But that was wrong since FORM-CALLEES also walks and
  ;;       thus would miss functions called in the body of a FOR-EFFECTS. So now
  ;;       we walk the body of a FOR-EFFECTS without macro-expanding it, but
  ;;       only when NESTED? is true which is essentially only for FORM-CALLEES
  ;;       since DETERMINISTIC? must not walk the body of FOR-EFFECTS or else
  ;;       it will mistakingly report that that a FOR-EFFECTS form is
  ;;       nondeterministic when its body is nondeterministic.
  (if (and reduce-function nested?)
      (funcall reduce-function
               (funcall map-function form 'for-effects)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'for-effects)))

(defun-compile-time walk-setf
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form))) (error "Improper SETF: ~S" form))
  (unless (evenp (length (rest form)))
    (error "Odd number of arguments to SETF: ~S" form))
  (if *local?*
      (if reduce-function
          (funcall reduce-function
                   (funcall map-function form 'local-setf)
                   (reduce reduce-function
                           (mapcar #'(lambda (subform)
                                       (walk map-function
                                             reduce-function
                                             screamer?
                                             partial?
                                             nested?
                                             subform
                                             environment))
                                   (every-other (rest (rest form))))))
          (funcall map-function form 'local-setf))
      (walk map-function
            reduce-function
            screamer?
            partial?
            nested?
            (let ((*macroexpand-hook* #'funcall))
              (macroexpand-1 form environment))
            environment)))

(defun-compile-time walk-multiple-value-call-nondeterministic
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper MULTIPLE-VALUE-CALL-NONDETERMINISTIC: ~S" form))
  (unless (>= (length form) 2)
    (error "MULTIPLE-VALUE-CALL-NONDETERMINISTIC must have at least one ~
          argument, a FUNCTION: ~S"
           form))
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'multiple-value-call-nondeterministic)
               (reduce reduce-function
                       (mapcar #'(lambda (subform)
                                   (walk map-function
                                         reduce-function
                                         screamer?
                                         partial?
                                         nested?
                                         subform
                                         environment))
                               (rest form))))
      (funcall map-function form 'multiple-value-call-nondeterministic)))

(defun-compile-time walk-full (map-function form)
  (unless (null (rest (last form))) (error "Improper FULL: ~S" form))
  (unless (= (length form) 2)
    (error "FULL must have exactly one argument, a FORM: ~S" form))
  (funcall map-function form 'full))

(defun-compile-time walk-macro-call
    (map-function reduce-function screamer? partial? nested? form environment)
  (if reduce-function
      (funcall reduce-function
               (funcall map-function form 'macro-call)
               (walk map-function
                     reduce-function
                     screamer?
                     partial?
                     nested?
                     (let ((*macroexpand-hook* #'funcall))
                       (macroexpand-1 form environment))
                     environment))
      (walk map-function
            reduce-function
            screamer?
            partial?
            nested?
            (let ((*macroexpand-hook* #'funcall))
              (macroexpand-1 form environment))
            environment)))

(defun-compile-time walk-function-call
    (map-function reduce-function screamer? partial? nested? form environment)
  (unless (null (rest (last form)))
    (error "Improper function call form: ~S" form))
  (cond
    ((lambda-expression? (first form))
     (if reduce-function
         (funcall
          reduce-function
          (funcall map-function form 'lambda-call)
          (funcall
           reduce-function
           (reduce reduce-function
                   (mapcar #'(lambda (subform)
                               (walk map-function
                                     reduce-function
                                     screamer?
                                     partial?
                                     nested?
                                     subform
                                     environment))
                           (rest form)))
           (funcall
            reduce-function
            (walk-lambda-list map-function
                              reduce-function
                              screamer?
                              partial?
                              nested?
                              (second (first form))
                              environment)
            (reduce reduce-function
                    (mapcar #'(lambda (subform)
                                (walk map-function
                                      reduce-function
                                      screamer?
                                      partial?
                                      nested?
                                      subform
                                      environment))
                            (peal-off-documentation-string-and-declarations
                             (rest (rest (first form))) t))))))
         (funcall map-function form 'lambda-call)))
    ((valid-function-name? (first form))
     (if (symbolp (first form))
         (if reduce-function
             (funcall reduce-function
                      (funcall map-function form 'symbol-call)
                      (reduce reduce-function
                              (mapcar #'(lambda (subform)
                                          (walk map-function
                                                reduce-function
                                                screamer?
                                                partial?
                                                nested?
                                                subform
                                                environment))
                                      (rest form))))
             (funcall map-function form 'symbol-call))
         (if reduce-function
             (funcall reduce-function
                      (funcall map-function form 'setf-call)
                      (reduce reduce-function
                              (mapcar #'(lambda (subform)
                                          (walk map-function
                                                reduce-function
                                                screamer?
                                                partial?
                                                nested?
                                                subform
                                                environment))
                                      (rest form))))
             (funcall map-function form 'setf-call))))
    (t (error "CAR of form ~S is not a valid function" form))))

;;; Possible FORM-TYPEs
;;;  Other:
;;;   LAMBDA-LIST VARIABLE
;;;  Special forms:
;;;   BLOCK CATCH EVAL-WHEN FLET FUNCTION-LAMBDA FUNCTION-SYMBOL FUNCTION-SETF
;;;   GO IF LABELS LET LET* MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1 PROGN
;;;   PROGV QUOTE RETURN-FROM SETQ TAGBODY THE THROW UNWIND-PROTECT
;;;  Symbolics special forms:
;;;   SYS:VARIABLE-LOCATION COMPILER:INVISIBLE-REFERENCES
;;;  Screamer special forms:
;;;   FOR-EFFECTS LOCAL-SETF
;;;  Partial special forms:
;;;   FULL
;;;  Other:
;;;   MACRO-CALL LAMBDA-CALL SYMBOL-CALL SETF-CALL

(defun-compile-time walk
    (map-function reduce-function screamer? partial? nested? form environment)
  ;; needs work: Cannot walk MACROLET or special forms not in both CLtL1 and
  ;;             CLtL2.
  (cond
    ((self-evaluating? form) (funcall map-function form 'quote))
    ((symbolp form) (funcall map-function form 'variable))
    ((eq (first form) 'block)
     (walk-block
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'catch)
     (walk-catch
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'eval-when)
     (walk-eval-when
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'flet)
     (walk-flet/labels
      map-function reduce-function screamer? partial? nested? form environment
      'flet))
    ((eq (first form) 'function)
     (walk-function
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'go) (walk-go map-function form))
    ((eq (first form) 'if)
     (walk-if map-function reduce-function screamer? partial? nested? form
              environment))
    ((eq (first form) 'labels)
     (walk-flet/labels
      map-function reduce-function screamer? partial? nested? form environment
      'labels))
    ((eq (first form) 'let)
     (walk-let/let*
      map-function reduce-function screamer? partial? nested? form environment
      'let))
    ((eq (first form) 'let*)
     (walk-let/let*
      map-function reduce-function screamer? partial? nested? form environment
      'let*))
    ;; needs work: This is a temporary kludge to support MCL.
    ((and (eq (first form) 'locally) (null (fourth form)))
     (walk map-function reduce-function screamer? partial? nested? (third form)
           environment))
    ((eq (first form) 'multiple-value-call)
     (walk-multiple-value-call
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'multiple-value-prog1)
     (walk-multiple-value-prog1
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'progn)
     (walk-progn
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'progv)
     (walk-progv
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'quote) (walk-quote map-function form))
    ((eq (first form) 'return-from)
     (walk-return-from
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'setq)
     (walk-setq
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'tagbody)
     (walk-tagbody
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'the)
     (walk-the
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'throw)
     (walk-throw
      map-function reduce-function screamer? partial? nested? form environment))
    ((eq (first form) 'unwind-protect)
     (walk-unwind-protect
      map-function reduce-function screamer? partial? nested? form environment))
    ((and screamer? (eq (first form) 'for-effects))
     (walk-for-effects
      map-function reduce-function screamer? partial? nested? form environment))
    ((and screamer? (eq (first form) 'setf))
     (walk-setf
      map-function reduce-function screamer? partial? nested? form environment))
    ((and screamer? (eq (first form) 'local))
     (let ((*local?* t))
       (walk-progn
        map-function reduce-function screamer? partial? nested? form
        environment)))
    ((and screamer? (eq (first form) 'global))
     (let ((*local?* nil))
       (walk-progn
        map-function reduce-function screamer? partial? nested? form
        environment)))
    ((and screamer? (eq (first form) 'multiple-value-call-nondeterministic))
     (walk-multiple-value-call-nondeterministic
      map-function reduce-function screamer? partial? nested? form environment))
    ((and partial? (eq (first form) 'full)) (walk-full map-function form))
    ((and (symbolp (first form))
          (macro-function (first form) environment))
     (walk-macro-call
      map-function reduce-function screamer? partial? nested? form environment))
    ((special-operator-p (first form))
     (error "Cannot (currently) handle the special form ~S" (first form)))
    (t (walk-function-call
        map-function reduce-function screamer? partial? nested? form
        environment))))

(defun-compile-time process-subforms (function form form-type environment)
  (case form-type
    (lambda-list (error "This shouldn't happen"))
    ((variable go) form)
    ((eval-when)
     (cons (first form)
           (cons (second form)
                 (mapcar #'(lambda (subform)
                             (funcall function subform environment))
                         (rest (rest form))))))
    ((flet labels)
     `(,(first form)
        ,(mapcar
          #'(lambda (binding)
              (cl:multiple-value-bind (body declarations documentation-string)
                  (peal-off-documentation-string-and-declarations
                   (rest (rest binding)) t)
                `(,(first binding)
                   ;; needs work: To process subforms of lambda list.
                   ,(second binding)
                   ,@(if documentation-string (list documentation-string))
                   ,@declarations
                   ,@(mapcar
                      #'(lambda (subform) (funcall function subform environment))
                      body))))
          (second form))
        ,@(mapcar
           #'(lambda (subform) (funcall function subform environment))
           (rest (rest form)))))
    ((let let*)
     (cl:multiple-value-bind (body declarations)
         (peal-off-documentation-string-and-declarations (rest (rest form)))
       `(,(first form)
          ,(mapcar
            #'(lambda (binding)
                (if (and (consp binding) (= (length binding) 2))
                    `(,(first binding)
                       ,(funcall function (second binding) environment))
                    binding))
            (second form))
          ,@declarations
          ,@(mapcar
             #'(lambda (subform) (funcall function subform environment)) body))))
    (progn
      `(progn ,@(mapcar
                 #'(lambda (subform) (funcall function subform environment))
                 (rest form))))
    (quote (quotify form))
    (the `(the ,(second form) ,(funcall function (third form) environment)))
    (macro-call (error "This shouldn't happen"))
    (lambda-call
     (cl:multiple-value-bind (body declarations documentation-string)
         (peal-off-documentation-string-and-declarations
          (rest (rest (first form))) t)
       ;; needs work: To process subforms of lambda list.
       `((lambda ,(second (first form))
           ,@(if documentation-string (list documentation-string))
           ,@declarations
           ,@(mapcar #'(lambda (subform) (funcall function subform environment))
                     body))
         ,@(mapcar
            #'(lambda (subform) (funcall function subform environment))
            (rest form)))))
    (otherwise
     (cons (first form)
           (mapcar #'(lambda (subform) (funcall function subform environment))
                   (rest form))))))

(defun-compile-time deterministic? (form environment)
  (walk
   #'(lambda (form form-type)
       (case form-type
         ((symbol-call setf-call)
          (function-record-deterministic? (get-function-record (first form))))
         (multiple-value-call-nondeterministic nil)
         ;; note: not really sure about CATCH, THROW and UNWIND-PROTECT
         (otherwise t)))
   ;; note: potentially inefficient because must walk entire form even
   ;;       after it is known to be nondeterministic
   #'(lambda (&optional (x nil x?) y) (if x? (and x y) t))
   t
   nil
   nil
   form
   environment))

(defun-compile-time deterministic-lambda-list? (lambda-list environment)
  (walk-lambda-list
   #'(lambda (form form-type)
       (case form-type
         ((symbol-call setf-call)
          (function-record-deterministic? (get-function-record (first form))))
         (multiple-value-call-nondeterministic nil)
         ;; note: not really sure about CATCH, THROW and UNWIND-PROTECT
         (otherwise t)))
   ;; note: potentially inefficient because must walk entire form even
   ;;       after it is known to be nondeterministic
   #'(lambda (&optional (x nil x?) y) (if x? (and x y) t))
   t
   nil
   nil
   lambda-list
   environment))

(defun-compile-time needs-substitution? (form environment)
  (walk
   #'(lambda (form form-type)
       (case form-type
         (function-lambda
          (not (and (every #'(lambda (form) (deterministic? form environment))
                           (peal-off-documentation-string-and-declarations
                            (rest (rest (second form))) t))
                    (deterministic-lambda-list?
                     (second (second form)) environment))))
         ((function-symbol function-setf)
          (not (function-record-deterministic?
                (get-function-record (second form)))))
         (return-from (let ((tag (assoc (second form) *block-tags* :test #'eq)))
                        (and tag (second tag))))
         (go (let ((tag (assoc (second form) *tagbody-tags*)))
               (and tag (second tag))))
         (setq *local?*)
         (local-setf t)
         (otherwise nil)))
   ;; note: potentially inefficient because must walk entire form even
   ;;       after it is known to need substitution
   #'(lambda (&optional (x nil x?) y) (if x? (or x y) '()))
   t
   nil
   t
   form
   environment))

(defun-compile-time contains-local-setf/setq? (form environment)
  (walk #'(lambda (form form-type)
            (declare (ignore form))
            (or (and *local?* (eq form-type 'setq))
                (eq form-type 'local-setf)))
        ;; note: potentially inefficient because must walk entire form even
        ;;       after it is known to contain a LOCAL SETF/SETQ special form
        #'(lambda (&optional (x nil x?) y) (if x? (or x y) '()))
        t
        nil
        nil
        form
        environment))

(defun-compile-time form-callees (form environment)
  (walk #'(lambda (form form-type)
            (case form-type
              ((function-symbol function-setf) (list (second form)))
              ((symbol-call setf-call) (list (first form)))
              (otherwise '())))
        #'(lambda (&optional (x nil x?) y)
            (if x? (union x y :test #'equal) '()))
        t
        nil
        t
        form
        environment))

(defun-compile-time callees (function-name)
  (function-record-callees (get-function-record function-name)))

(defun-compile-time indirect-callees-internal (function-names callees)
  (if (null function-names)
      callees
      (let ((function-name (first function-names)))
        (if (member function-name callees :test #'equal)
            (indirect-callees-internal (rest function-names) callees)
            (indirect-callees-internal
             (rest function-names)
             (indirect-callees-internal
              (callees function-name) (cons function-name callees)))))))

(defun-compile-time indirect-callees (function-name)
  (indirect-callees-internal (callees function-name) '()))

(defun-compile-time callers (function-name)
  (let ((callers '())
        (function-names '()))
    (maphash #'(lambda (function-name function-record)
                 (declare (ignore function-record))
                 (push function-name function-names))
             *function-record-table*)
    (dolist (caller function-names)
      (if (member function-name (callees caller) :test #'equal)
          (pushnew caller callers :test #'equal)))
    callers))

(defun-compile-time indirect-callers-internal (function-names callers)
  (if (null function-names)
      callers
      (let ((function-name (first function-names)))
        (if (member function-name callers :test #'equal)
            (indirect-callers-internal (rest function-names) callers)
            (indirect-callers-internal
             (rest function-names)
             (indirect-callers-internal
              (callers function-name) (cons function-name callers)))))))

(defun-compile-time indirect-callers (function-name)
  (indirect-callers-internal (callers function-name) '()))

(defun-compile-time expand-local-setf (pairs environment)
  (if (null pairs)
      '(progn)
      (let ((d (gensym "DUMMY-"))
            (dummy-argument (gensym "DUMMY-")))
        (cl:multiple-value-bind (vars vals stores store-form access-form)
            (get-setf-expansion (first pairs) environment)
          `(let* (,@(mapcar #'list vars vals)
                  (,dummy-argument ,(second pairs))
                    (,d ,access-form))
             (trail #'(lambda () ,(subst d (first stores) store-form)))
             ,@(if (null (rest (rest pairs)))
                   (list (subst dummy-argument (first stores) store-form))
                   (list (subst dummy-argument (first stores) store-form)
                         (expand-local-setf (rest (rest pairs)) environment))))))))

(defun-compile-time expand-local-setq (pairs environment)
  (if (null pairs)
      '(progn)
      (let ((d (gensym "DUMMY-")))
        `(let ((,d ,(first pairs)))
           (trail #'(lambda () (setq ,(first pairs) ,d)))
           ,@(if (null (rest (rest pairs)))
                 (list `(setq
                         ,(first pairs)
                         ,(perform-substitutions (second pairs) environment)))
                 (list `(setq
                         ,(first pairs)
                         ,(perform-substitutions (second pairs) environment))
                       (expand-local-setq (rest (rest pairs)) environment)))))))

(defun-compile-time perform-substitutions (form environment)
  (if (needs-substitution? form environment)
      (walk
       #'(lambda (form form-type)
           (case form-type
             (lambda-list (error "This shouldn't happen"))
             (variable (error "This shouldn't happen"))
             (block (let ((*block-tags*
                           (cons (list (second form) nil) *block-tags*)))
                      (process-subforms
                       #'perform-substitutions form form-type environment)))
             (function-lambda
              (unless (deterministic-lambda-list?
                       (second (second form)) environment)
                (screamer-error
                 "Cannot (currently) handle a LAMDBA expression with~%~
              nondeterministic initializations forms for~%~
              &OPTIONAL and &AUX parameters: ~S"
                 form))
              (cl:multiple-value-bind (body declarations documentation-string)
                  (peal-off-documentation-string-and-declarations
                   (rest (rest (second form))) t)
                (if (every #'(lambda (form) (deterministic? form environment))
                           body)
                    ;; needs work: To process subforms of lambda list.
                    `#'(lambda ,(second (second form))
                         ,@(if documentation-string (list documentation-string))
                         ,@declarations
                         ,@(mapcar
                            #'(lambda (subform)
                                (perform-substitutions subform environment))
                            body))
                    (let ((continuation (gensym "CONTINUATION-")))
                      ;; note: This conses every time #'(LAMBDA (...) ...) is
                      ;;       accessed when it is nondeterministic. A small
                      ;;       price to pay for a lot of error checking.
                      `(make-nondeterministic-function
                        :function
                        ;; needs work: To process subforms of lambda list.
                        #'(lambda (,continuation ,@(second (second form)))
                            ,@(if documentation-string (list documentation-string))
                            ,@declarations
                            ,continuation ;ignore
                            ,(cps-convert-progn body
                                                continuation
                                                '()
                                                t
                                                environment)))))))
             ((function-symbol function-setf)
              (if (function-record-deterministic?
                   (get-function-record (second form)))
                  form
                  ;; note: This conses every time #'FOO  or #'(SETF FOO) is
                  ;;       accessed when FOO or (SETF FOO) is nondeterministic.
                  ;;       A small price to pay for a lot of error checking.
                  `(make-nondeterministic-function
                    :function #',(cps-convert-function-name (second form)))))
             (go (let ((tag (assoc (second form) *tagbody-tags*)))
                   ;; note: Can't issue an error here if tag not found since it
                   ;;       might be outside the scope of a FOR-EFFECTS.
                   (if (and tag (second tag)) `(,(second tag)) form)))
             (quote (error "This shouldn't happen"))
             (return-from
              (let ((tag (assoc (second form) *block-tags* :test #'eq))
                    (value (perform-substitutions
                            (if (= (length form) 3) (third form) nil)
                            environment)))
                ;; note: Can't issue an error here if tag not found since it
                ;;       might be outside the scope of a FOR-EFFECTS.
                (if (and tag (second tag))
                    (possibly-beta-reduce-funcall
                     (second tag) '() value (fourth tag))
                    `(return-from ,(second form) ,value))))
             (setq (if *local?*
                       (expand-local-setq (rest form) environment)
                       (process-subforms
                        #'perform-substitutions form form-type environment)))
             (tagbody (let ((*tagbody-tags*
                             (append (mapcar #'(lambda (tag) (list tag nil))
                                             (remove-if #'consp (rest form)))
                                     *tagbody-tags*)))
                        (process-subforms
                         #'perform-substitutions form form-type environment)))
             (for-effects (perform-substitutions
                           (let ((*macroexpand-hook* #'funcall))
                             (macroexpand-1 form environment))
                           environment))
             (local-setf (perform-substitutions
                          (expand-local-setf (rest form) environment)
                          environment))
             (macro-call (error "This shouldn't happen"))
             (otherwise (process-subforms
                         #'perform-substitutions form form-type environment))))
       nil
       t
       nil
       nil
       form
       environment)
      form))

(defun-compile-time is-magic-declaration? (form)
  (and (consp form)
       (eq (first form) 'declare)
       (consp (rest form))
       (consp (second form))
       (eq (first (second form)) 'magic)))

(defun-compile-time is-magic-continuation? (continuation)
  ;; Checks that CONTINUATION is of the form:
  ;;   #'(lambda (...) (declare (magic) ...) ...)
  (and (consp continuation)
       (eq (first continuation) 'function)
       (null (rest (last continuation)))
       (= (length continuation) 2)
       (lambda-expression? (second continuation))
       (>= (length (second continuation)) 3)
       (is-magic-declaration? (third (second continuation)))))

(defun-compile-time magic-continuation-argument (continuation)
  (if (or (eq (first (second (second continuation))) '&optional)
          (eq (first (second (second continuation))) '&rest))
      (second (second (second continuation)))
      (first (second (second continuation)))))

(defun-compile-time possibly-beta-reduce-funcall
    (continuation types form value?)
  (unless (or (and (symbolp continuation) (not (symbol-package continuation)))
              (and (consp continuation)
                   (eq (first continuation) 'function)
                   (null (rest (last continuation)))
                   (= (length continuation) 2)
                   (symbolp (second continuation)))
              (is-magic-continuation? continuation))
    (error "Please report this bug; This shouldn't happen (A)"))
  (cond
    ((symbolp continuation)
     (if value?
         (if (null types)
             (if (consp form)
                 `(multiple-value-call ,continuation ,form)
                 ;; note: This optimization is technically unsound if FORM
                 ;;       is a symbol macro that returns multiple values.
                 `(funcall ,continuation ,form))
             ;; note: This optimization assumes that there are no VALUES
             ;;       types.
             `(funcall ,continuation (the (and ,@types) ,form)))
         `(progn ,form (funcall ,continuation))))
    ((symbolp (second continuation))
     (if value?
         (if (null types)
             (if (consp form)
                 `(multiple-value-call ,continuation ,form)
                 ;; note: This optimization is technically unsound if FORM
                 ;;       is a symbol macro that returns multiple values.
                 `(,(second continuation) ,form))
             ;; note: This optimization assumes that there are no VALUES
             ;;       types.
             `(,(second continuation) (the (and ,@types) ,form)))
         `(progn ,form (,(second continuation)))))
    (t (if value?
           (progn
             (if (null (second (second continuation)))
                 (error "Please report this bug; This shouldn't happen (B)"))
             (cond
               ((eq (first (second (second continuation))) '&rest)
                (if (null types)
                    `(let ((,(magic-continuation-argument continuation)
                            (multiple-value-list ,form)))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))
                    `(let ((,(magic-continuation-argument continuation)
                            (list (the (and ,@types) ,form))))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))))
               ((or (and (consp form)
                         (not
                          (and (eq (first form) 'function)
                               (null (rest (last form)))
                               (= (length form) 2)
                               (symbolp (second form)))))
                    (and (symbolp form) (symbol-package form))
                    (symbol-package (magic-continuation-argument continuation)))
                (if (null types)
                    `(let ((,(magic-continuation-argument continuation) ,form))
                       ,@(if (and *dynamic-extent?* (is-magic-continuation? form))
                             `((declare
                                (dynamic-extent
                                 ,(magic-continuation-argument continuation)))))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))
                    `(let ((,(magic-continuation-argument continuation)
                            (the (and ,@types) ,form)))
                       (declare
                        (type (and ,@types)
                              ,(magic-continuation-argument continuation)))
                       ;; Peal off LAMBDA, arguments, and DECLARE.
                       ,@(rest (rest (rest (second continuation)))))))
               ;; note: This case may be unsoundly taken in the following cases:
               ;;       a. (MAGIC-CONTINUATION-ARGUMENT CONTINUATION) is a
               ;;          non-Screamer GENSYM. This can only happen if a
               ;;          a BINDING-VARIABLE is a GENSYM in CPS-CONVERT-LET*.
               ;;       b. FORM is a non-Screamer GENSYM
               (t (if (null types)
                      (subst form
                             (magic-continuation-argument continuation)
                             ;; Peal off LAMBDA, arguments, and DECLARE.
                             `(progn ,@(rest (rest (rest (second continuation)))))
                             :test #'eq)
                      (subst `(the (and ,@types) ,form)
                             (magic-continuation-argument continuation)
                             ;; Peal off LAMBDA, arguments, and DECLARE.
                             `(progn ,@(rest (rest (rest (second continuation)))))
                             :test #'eq)))))
           (progn
             (unless (null (second (second continuation)))
               (error "Please report this bug; This shouldn't happen (C)"))
             ;; Peal off LAMBDA, arguments, and DECLARE.
             `(progn ,form ,@(rest (rest (rest (second continuation))))))))))

(defun-compile-time void-continuation (continuation)
  (unless (or (and (symbolp continuation) (not (symbol-package continuation)))
              (and (consp continuation)
                   (eq (first continuation) 'function)
                   (null (rest (last continuation)))
                   (= (length continuation) 2)
                   (symbolp (second continuation)))
              (is-magic-continuation? continuation))
    (error "Please report this bug; This shouldn't happen (D)"))
  (let ((dummy-argument (gensym "DUMMY-")))
    ;; note: We could get rid of this bogosity by having two versions of each
    ;;       nondeterministic function, one which returned a value and one which
    ;;       didn't.
    `#'(lambda (&rest ,dummy-argument)
         (declare (magic)
                  (ignore ,dummy-argument))
         ,@(cond ((symbolp continuation) `((funcall ,continuation)))
                 ((symbolp (second continuation)) `((,(second continuation))))
                 ;; Peal off LAMBDA, arguments, and DECLARE.
                 (t (rest (rest (rest (second continuation)))))))))

(defun-compile-time cps-convert-function-name (function-name)
  (if (symbolp function-name)
      (intern (format nil "~A-NONDETERMINISTIC" (string function-name))
              (symbol-package function-name))
      `(setf ,(intern (format nil "~A-NONDETERMINISTIC"
                              (string (second function-name)))
                      (symbol-package (second function-name))))))

(defun-compile-time cps-convert-block
    (name body continuation types value? environment)
  (let* ((c (gensym "CONTINUATION-"))
         (*block-tags* (cons (list name c types value?) *block-tags*)))
    (possibly-beta-reduce-funcall
     `#'(lambda (,c)
          (declare (magic))
          ,(cps-convert-progn body c types value? environment))
     '()
     continuation
     t)))

(defun-compile-time cps-convert-if (antecedent
                                    consequent
                                    alternate
                                    continuation
                                    types
                                    value?
                                    environment)
  (let ((c (gensym "CONTINUATION-"))
        (dummy-argument (gensym "DUMMY-"))
        (other-arguments (gensym "OTHER-")))
    (possibly-beta-reduce-funcall
     `#'(lambda (,c)
          (declare (magic))
          ,(cps-convert
            antecedent
            `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
                 (declare (magic)
                          (ignore ,other-arguments))
                 (if ,dummy-argument
                     ,(cps-convert consequent c types value? environment)
                     ,(cps-convert alternate c types value? environment)))
            '()
            t
            environment))
     '()
     continuation
     t)))

(defun-compile-time cps-convert-let (bindings
                                     body
                                     declarations
                                     continuation
                                     types
                                     value?
                                     environment
                                     &optional
                                     new-bindings)
  (if (null bindings)
      `(let ,new-bindings
         ,@declarations
         ,(cps-convert-progn body continuation types value? environment))
      (let* ((binding (first bindings))
             (binding-variable
              (if (symbolp binding) binding (first binding)))
             (binding-form
              (if (and (consp binding) (= (length binding) 2))
                  (second binding)
                  nil))
             (dummy-argument (gensym "DUMMY-"))
             (other-arguments (gensym "OTHER-")))
        (cps-convert
         binding-form
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-convert-let (rest bindings)
                                body
                                declarations
                                continuation
                                types
                                value?
                                environment
                                (cons (list binding-variable dummy-argument)
                                      new-bindings)))
         '()
         t
         environment))))

(defun-compile-time cps-convert-let* (bindings
                                      body
                                      declarations
                                      continuation
                                      types
                                      value?
                                      environment)
  (if (null bindings)
      (if (null declarations)
          (cps-convert-progn body continuation types value? environment)
          `(let ()
             ,@declarations
             ,(cps-convert-progn body continuation types value? environment)))
      (let* ((binding (first bindings))
             (binding-variable
              (if (symbolp binding) binding (first binding)))
             (binding-form
              (if (and (consp binding) (= (length binding) 2))
                  (second binding)
                  nil))
             (other-arguments (gensym "OTHER-")))
        (cps-convert
         binding-form
         `#'(lambda (&optional ,binding-variable &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-convert-let* (rest bindings)
                                 body
                                 declarations
                                 continuation
                                 types
                                 value?
                                 environment))
         '()
         t
         environment))))

(defun-compile-time cps-convert-multiple-value-call-internal
    (nondeterministic? function forms continuation types value? environment
                       &optional arguments)
  (if (null forms)
      (if nondeterministic?
          ;; needs work: TYPES is never actually used in this branch.
          `(apply-nondeterministic-nondeterministic
            ,(if value? continuation (void-continuation continuation))
            ,function
            (append ,@(reverse arguments)))
          (possibly-beta-reduce-funcall
           continuation
           types
           `(apply ,function (append ,@(reverse arguments)))
           value?))
      (let ((dummy-argument (gensym "DUMMY-")))
        (cps-convert
         (first forms)
         `#'(lambda (&rest ,dummy-argument)
              (declare (magic))
              ,(cps-convert-multiple-value-call-internal
                nondeterministic? function (rest forms) continuation types value?
                environment (cons dummy-argument arguments)))
         nil
         t
         environment))))

(defun-compile-time cps-convert-multiple-value-call
    (nondeterministic? function forms continuation types value? environment)
  (let ((dummy-argument (gensym "DUMMY-"))
        (other-arguments (gensym "OTHER-")))
    (cps-convert
     function
     `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
          (declare (magic)
                   (ignore ,other-arguments))
          ,(cps-convert-multiple-value-call-internal
            nondeterministic? dummy-argument forms continuation types value?
            environment))
     nil
     t
     environment)))

(defun-compile-time cps-convert-multiple-value-prog1
    (form forms continuation types value? environment)
  (if value?
      (let ((dummy-argument (gensym "DUMMY-")))
        (cps-convert
         form
         `#'(lambda (&rest ,dummy-argument)
              (declare (magic))
              ,(cps-convert-progn
                forms
                `#'(lambda ()
                     (declare (magic))
                     (possibly-beta-reduce-funcall
                      continuation types `(values-list ,dummy-argument) t))
                nil
                nil
                environment))
         types
         t
         environment))
      (cps-convert-progn (cons form forms) continuation types nil environment)))

(defun-compile-time cps-convert-progn
    (body continuation types value? environment)
  (cond
    ((null body) (possibly-beta-reduce-funcall continuation types nil value?))
    ((null (rest body))
     (cps-convert (first body) continuation types value? environment))
    (t (cps-convert
        (first body)
        `#'(lambda ()
             (declare (magic))
             ,(cps-convert-progn
               (rest body) continuation types value? environment))
        '()
        nil
        environment))))

(defun-compile-time cps-convert-return-from (name result environment)
  (let ((tag (assoc name *block-tags* :test #'eq)))
    (if (and tag (second tag))
        (cps-convert result (second tag) (third tag) (fourth tag) environment)
        ;; note: Can't issue an error here if tag not found since it might be
        ;;       outside the scope of a FOR-EFFECTS. Thus we must compile a
        ;;       RETURN-FROM nondeterministic code to deterministic code.
        ;;       Likewise, can't issue an error here if tag is found but
        ;;       (SECOND TAG) is NIL since this arrises when you have a
        ;;       RETURN-FROM inside a FOR-EFFECTS to a tag outside the
        ;;       FOR-EFFECTS.
        (let ((dummy-argument (gensym "DUMMY-")))
          (cps-convert
           result
           `#'(lambda (&rest ,dummy-argument)
                (declare (magic))
                (return-from ,name (values-list ,dummy-argument)))
           '()
           t
           environment)))))

(defun-compile-time cps-convert-setq
    (arguments continuation types value? environment)
  (if (null arguments)
      (possibly-beta-reduce-funcall continuation types nil value?)
      (let ((dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cps-convert
         (second arguments)
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments)
                       ,@(if (and (null (rest (rest arguments)))
                                  (not (null types)))
                             `((type (and ,@types) ,dummy-argument))))
              ,(if (null (rest (rest arguments)))
                   (possibly-beta-reduce-funcall
                    continuation
                    types
                    `(setq ,(first arguments) ,dummy-argument)
                    value?)
                   `(progn (setq ,(first arguments) ,dummy-argument)
                           ,(cps-convert-setq
                             (rest (rest arguments))
                             continuation
                             types
                             value?
                             environment))))
         (if (null (rest (rest arguments))) types '())
         t
         environment))))

(defun-compile-time cps-convert-tagbody
    (body continuation types value? environment)
  (let ((segments (list (list 'header)))
        (*tagbody-tags* *tagbody-tags*)) ;cool!
    (dolist (form body)
      (if (consp form)
          (push form (rest (first segments)))
          (let ((c (gensym "CONTINUATION-")))
            (push (list form c) *tagbody-tags*)
            (push (list c) segments))))
    (push nil (rest (first segments)))
    (let ((segments (reverse segments))
          (dummy-argument (gensym "DUMMY-"))
          (other-arguments (gensym "OTHER-")))
      ;; needs work: The closures created by LABELS functions aren't declared to
      ;;             have DYNAMIC-EXTENT since I don't know how to do this in
      ;;             Common Lisp.
      `(labels ,(mapcar
                 #'(lambda (segment)
                     (let ((next (rest (member segment segments :test #'eq))))
                       `(,(first segment)
                          (&optional ,dummy-argument &rest ,other-arguments)
                          (declare (ignore ,dummy-argument ,other-arguments))
                          ,(cps-convert-progn
                            (reverse (rest segment))
                            (if next `#',(first (first next)) continuation)
                            (if next '() types)
                            (or next value?)
                            environment))))
                 (rest segments))
         ,(let ((next (rest segments)))
               (cps-convert-progn
                (reverse (rest (first segments)))
                (if next `#',(first (first next)) continuation)
                (if next '() types)
                (or next value?)
                environment))))))

(defun-compile-time cps-convert-local-setf/setq
    (arguments continuation types value? environment)
  (if (null arguments)
      (possibly-beta-reduce-funcall continuation types nil value?)
      (let ((d (gensym "DUMMY-"))
            (dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cl:multiple-value-bind (vars vals stores store-form access-form)
            (get-setf-expansion (first arguments) environment)
          (cps-convert
           (second arguments)
           `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
                (declare (magic)
                         (ignore ,other-arguments)
                         ,@(if (and (null (rest (rest arguments)))
                                    (not (null types)))
                               `((type (and ,@types) ,dummy-argument))))
                (let* (,@(mapcar #'list vars vals) (,d ,access-form))
                  (unwind-protect
                       ,(if (null (rest (rest arguments)))
                            (possibly-beta-reduce-funcall
                             continuation
                             types
                             (subst dummy-argument (first stores) store-form)
                             value?)
                            `(progn ,(subst
                                      dummy-argument
                                      (first stores)
                                      store-form)
                                    ,(cps-convert-local-setf/setq
                                      (rest (rest arguments))
                                      continuation
                                      types
                                      value?
                                      environment)))
                    ,(subst d (first stores) store-form))))
           (if (null (rest (rest arguments))) types '())
           t
           environment)))))

(defun-compile-time cps-convert-call (function-name
                                      arguments
                                      continuation
                                      types
                                      value?
                                      environment
                                      &optional
                                      dummy-arguments)
  ;; needs work: TYPES is never actually used here.
  (if (null arguments)
      (let ((c (gensym "CONTINUATION-")))
        (possibly-beta-reduce-funcall
         `#'(lambda (,c)
              (declare (magic))
              (,(cps-convert-function-name function-name)
                ,c
                ,@(reverse dummy-arguments)))
         '()
         (if value? continuation (void-continuation continuation))
         t))
      (let ((dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cps-convert
         (first arguments)
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-convert-call
                function-name
                (rest arguments)
                continuation
                types
                value?
                environment
                (cons dummy-argument dummy-arguments)))
         '()
         t
         environment))))

(defun-compile-time cps-non-convert-call (function-name
                                          arguments
                                          continuation
                                          types
                                          value?
                                          environment
                                          &optional
                                          dummy-arguments)
  (if (null arguments)
      (possibly-beta-reduce-funcall
       continuation
       types
       (if (not (null types))
           `(the (and ,@types) (,function-name ,@(reverse dummy-arguments)))
           `(,function-name ,@(reverse dummy-arguments)))
       value?)
      (let ((dummy-argument (gensym "DUMMY-"))
            (other-arguments (gensym "OTHER-")))
        (cps-convert
         (first arguments)
         `#'(lambda (&optional ,dummy-argument &rest ,other-arguments)
              (declare (magic)
                       (ignore ,other-arguments))
              ,(cps-non-convert-call
                function-name
                (rest arguments)
                continuation
                types
                value?
                environment
                (cons dummy-argument dummy-arguments)))
         '()
         t
         environment))))

(defun-compile-time cps-convert (form continuation types value? environment)
  (walk #'(lambda (form form-type)
            (if (and (not (eq form-type 'quote))
                     (deterministic? form environment)
                     (not (contains-local-setf/setq? form environment)))
                (possibly-beta-reduce-funcall
                 continuation
                 types
                 (perform-substitutions form environment)
                 value?)
                (case form-type
                  (lambda-list (error "This shouldn't happen"))
                  (variable (possibly-beta-reduce-funcall
                             continuation types form value?))
                  (block (cps-convert-block (second form)
                                            (rest (rest form))
                                            continuation
                                            types
                                            value?
                                            environment))
                  ((function-lambda function-symbol function-setf)
                   (possibly-beta-reduce-funcall
                    continuation
                    types
                    (perform-substitutions form environment)
                    value?))
                  (go (error "This shouldn't happen"))
                  (if (cps-convert-if (second form)
                                      (third form)
                                      (if (null (rest (rest (rest form))))
                                          nil
                                          (fourth form))
                                      continuation
                                      types
                                      value?
                                      environment))
                  (let (cl:multiple-value-bind (body declarations)
                           (peal-off-documentation-string-and-declarations
                            (rest (rest form)))
                         (cps-convert-let
                          (second form)
                          body
                          declarations
                          continuation
                          types
                          value?
                          environment)))
                  (let* (cl:multiple-value-bind (body declarations)
                            (peal-off-documentation-string-and-declarations
                             (rest (rest form)))
                          (cps-convert-let*
                           (second form)
                           body
                           declarations
                           continuation
                           types
                           value?
                           environment)))
                  (multiple-value-call
                      (cps-convert-multiple-value-call
                       nil
                       (second form)
                       (rest (rest form))
                       continuation
                       types
                       value?
                       environment))
                  (multiple-value-prog1
                      (cps-convert-multiple-value-prog1
                       (second form)
                       (rest (rest form))
                       continuation
                       types
                       value?
                       environment))
                  (progn (cps-convert-progn
                          (rest form) continuation types value? environment))
                  (quote (possibly-beta-reduce-funcall
                          continuation types (quotify form) value?))
                  (return-from (cps-convert-return-from
                                (second form)
                                (if (= (length form) 2) nil (third form))
                                environment))
                  (setq (if *local?*
                            (cps-convert-local-setf/setq
                             (rest form) continuation types value? environment)
                            (cps-convert-setq
                             (rest form) continuation types value? environment)))
                  (tagbody (cps-convert-tagbody
                            (rest form) continuation types value? environment))
                  (the (cps-convert (third form)
                                    continuation
                                    (cons (second form) types)
                                    value?
                                    environment))
                  (for-effects (possibly-beta-reduce-funcall
                                continuation types form value?))
                  (local-setf
                   (cps-convert-local-setf/setq
                    (rest form) continuation types value? environment))
                  (multiple-value-call-nondeterministic
                   (cps-convert-multiple-value-call
                    t
                    (second form)
                    (rest (rest form))
                    continuation
                    types
                    value?
                    environment))
                  (macro-call (error "This shouldn't happen"))
                  (lambda-call
                   (unless (deterministic-lambda-list?
                            (second (first form)) environment)
                     (screamer-error
                      "Cannot (currently) handle a LAMDBA expression with~%~
                   nondeterministic initializations forms for~%~
                   &OPTIONAL and &AUX parameters: ~S"
                      form))
                   (unless (every
                            #'(lambda (argument)
                                (and (symbolp argument)
                                     (not (member argument lambda-list-keywords
                                                  :test #'eq))))
                            (second (first form)))
                     (error "Cannot (currently) handle a nondeterministic~%~
                         form whose CAR is a LAMBDA expression with~%~
                         lambda list keywords or arguments that are not~%~
                         symbols: ~S"
                            form))
                   (unless (= (length (second (first form)))
                              (length (rest form)))
                     (error "The form ~S has a CAR which is a LAMBDA~%~
                         expression which takes a different number of~%~
                         arguments than it is called with"
                            form))
                   (cl:multiple-value-bind (body declarations)
                       (peal-off-documentation-string-and-declarations
                        (rest (rest (first form))) t)
                     ;; note: The documentation string is lost for lambda calls
                     ;;       that are CPS Converted.
                     (cps-convert-let
                      (mapcar #'list (second (first form)) (rest form))
                      body
                      declarations
                      continuation
                      types
                      value?
                      environment)))
                  ((symbol-call setf-call)
                   (if (function-record-deterministic?
                        (get-function-record (first form)))
                       (cps-non-convert-call (first form)
                                             (rest form)
                                             continuation
                                             types
                                             value?
                                             environment)
                       (cps-convert-call (first form)
                                         (rest form)
                                         continuation
                                         types
                                         value?
                                         environment)))
                  (otherwise
                   (screamer-error
                    "Cannot (currently) handle the special form ~S inside a~%~
                  nondeterministic context."
                    (first form))))))
        nil
        t
        nil
        nil
        form
        environment))

(defun-compile-time declare-deterministic (function-name)
  (setf (function-record-deterministic? (get-function-record function-name)) t))

(defun-compile-time declare-nondeterministic (function-name)
  (setf (function-record-deterministic? (get-function-record function-name))
        nil))

(defun-compile-time compute-callees (body environment)
  ;; note: What bogosity in Common Lisp! UNION should allow zero arguments and
  ;;       return NIL as the identity element for use by REDUCE.
  (reduce
   #'union
   (mapcar #'(lambda (form) (form-callees form environment))
           (peal-off-documentation-string-and-declarations body t))
   :initial-value '()))

(defun-compile-time cache-definition (function-name lambda-list body callees)
  (let ((function-record (get-function-record function-name)))
    (setf (function-record-lambda-list function-record) lambda-list)
    (setf (function-record-body function-record) body)
    (setf (function-record-callees function-record) callees)))

(defun-compile-time determine-whether-deterministic (function-name environment)
  ;; note: This is using the current rather than the saved ENVIRONMENT.
  (let* ((function-record (get-function-record function-name)))
    (setf (function-record-deterministic? function-record)
          (and (every #'(lambda (form) (deterministic? form environment))
                      (peal-off-documentation-string-and-declarations
                       (function-record-body function-record) t))
               (deterministic-lambda-list?
                (function-record-lambda-list function-record) environment)))))

(defun-compile-time determine-whether-callers-are-deterministic
    (function-name function-names environment)
  ;; note: This is using the current rather than the saved ENVIRONMENT.
  (dolist (caller (callers function-name))
    (unless (member caller function-names :test #'equal)
      (determine-whether-deterministic caller environment)
      (determine-whether-callers-are-deterministic
       caller (cons caller function-names) environment))))

(defun-compile-time function-definition (function-name environment)
  ;; note: This is using the current rather than the saved ENVIRONMENT.
  (let* ((function-record (get-function-record function-name))
         (lambda-list (function-record-lambda-list function-record))
         (body (function-record-body function-record)))
    (cl:multiple-value-bind (body declarations documentation-string)
        (peal-off-documentation-string-and-declarations body t)
      (if (function-record-deterministic? function-record)
          (let ((*block-tags* (list (list function-name nil))))
            ;; needs work: To process subforms of lambda list.
            (list `(cl:defun ,function-name ,lambda-list
                     ,@(if documentation-string (list documentation-string))
                     ,@declarations
                     ,@(mapcar #'(lambda (form)
                                   (perform-substitutions form environment))
                               body))
                  `(declare-deterministic ',function-name)))
          (let* ((continuation (gensym "CONTINUATION-"))
                 ;; note: Could provide better TYPES and VALUE? here.
                 (*block-tags* (list (list function-name continuation '() t))))
            (list `(cl:defun ,function-name ,lambda-list
                     ,@(if documentation-string (list documentation-string))
                     ,@declarations
                     (declare
                      (ignore
                       ,@(reduce
                          #'append
                          (mapcar
                           #'(lambda (argument)
                               (if (consp argument)
                                   (if (and (consp (rest argument))
                                            (consp (rest (rest argument))))
                                       (list (first argument) (third argument))
                                       (list (first argument)))
                                   (list argument)))
                           (set-difference
                            lambda-list
                            lambda-list-keywords
                            :test #'eq)))))
                     (screamer-error
                      "Function ~S is a nondeterministic function. As such, it~%~
                  must be called only from a nondeterministic context."
                      ',function-name))
                  `(cl:defun ,(cps-convert-function-name function-name)
                       (,continuation ,@lambda-list)
                     ,@(if documentation-string (list documentation-string))
                     ,@declarations
                     ,continuation      ;ignore
                     ,(cps-convert-progn body continuation '() t environment))
                  `(declare-nondeterministic ',function-name)))))))

(defun-compile-time modified-function-definitions (function-name environment)
  ;; note: This is using the current rather than the saved ENVIRONMENT.
  (let ((function-record (get-function-record function-name))
        (callers (indirect-callers function-name))
        (function-records '()))
    (setf (function-record-old-deterministic? function-record)
          (function-record-deterministic? function-record))
    (setf (function-record-deterministic? function-record) t)
    (push function-record function-records)
    (dolist (caller callers)
      (let ((function-record (get-function-record caller)))
        (unless (member function-record function-records :test #'eq)
          (setf (function-record-old-deterministic? function-record)
                (function-record-deterministic? function-record))
          (setf (function-record-deterministic? function-record) t)
          (push function-record function-records))))
    (dolist (caller callers)
      (dolist (callee (callees caller))
        (let ((function-record (get-function-record callee)))
          (unless (member function-record function-records :test #'eq)
            (setf (function-record-old-deterministic? function-record)
                  (function-record-deterministic? function-record))
            (push function-record function-records)))))
    (determine-whether-deterministic function-name environment)
    (determine-whether-callers-are-deterministic function-name nil environment)
    (let ((definitions (function-definition function-name environment)))
      (unless (eq (not (function-record-deterministic? function-record))
                  (not (function-record-old-deterministic? function-record)))
        (dolist (caller callers)
          (if (and (not (equal caller function-name))
                   (some #'(lambda (callee)
                             (let ((function-record (get-function-record callee)))
                               (not (eq (not (function-record-deterministic?
                                              function-record))
                                        (not (function-record-old-deterministic?
                                              function-record))))))
                         (callees caller)))
              (setf definitions
                    (append (function-definition caller environment)
                            definitions)))))
      ;; note: This is so that macroexpand without compile doesn't get out of
      ;;       sync.
      (dolist (function-record function-records)
        (setf (function-record-deterministic? function-record)
              (function-record-old-deterministic? function-record)))
      definitions)))

;;; The protocol

(defmacro-compile-time defun
    (function-name lambda-list &body body &environment environment)
  (let ((*nondeterministic-context?* t))
    (check-function-name function-name)
    (let* ((callees (compute-callees body environment))
           (function-record (get-function-record function-name))
           (function-record-lambda-list
            (function-record-lambda-list function-record))
           (function-record-body (function-record-body function-record))
           (function-record-callees (function-record-callees function-record))
           (function-record-deterministic?
            (function-record-deterministic? function-record))
           (function-record-old-deterministic?
            (function-record-old-deterministic? function-record))
           (function-record-screamer?
            (function-record-screamer? function-record)))
      (cache-definition function-name lambda-list body callees)
      (let ((modified-function-definitions
             ;; note: This is using the current rather than the saved ENVIRONMENT.
             (modified-function-definitions function-name environment)))
        ;; note: This is so that macroexpand without compile doesn't get out of
        ;;       sync.
        (setf (function-record-lambda-list function-record)
              function-record-lambda-list)
        (setf (function-record-body function-record) function-record-body)
        (setf (function-record-callees function-record)
              function-record-callees)
        (setf (function-record-deterministic? function-record)
              function-record-deterministic?)
        (setf (function-record-old-deterministic? function-record)
              function-record-old-deterministic?)
        (setf (function-record-screamer? function-record)
              function-record-screamer?)
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (cache-definition ',function-name ',lambda-list ',body ',callees)
           ,@modified-function-definitions
           ',function-name)))))

(defmacro-compile-time either (&body expressions)
  "Nondeterministically evaluates and returns the value of one of its
EXPRESSIONS.

EITHER takes any number of arguments. With no arguments, \(EITHER) is
equivalent to \(FAIL) and is thus deterministic. With one argument,
\(EITHER EXPRESSION) is equivalent to expression itself and is thus deterministic
only when EXPRESSION is deterministic. With two or more argument it is
nondeterministic and can only appear in a nondeterministic context.

It sets up a choice point and evaluates the first EXPRESSION returning its
result. Whenever backtracking proceeds to this choice point, the next
EXPRESSION is evaluated and its result returned. When no more EXPRESSIONS
remain, the current choice point is removed and backtracking continues to the
next most recent choice point.

As an optimization, the choice point created for this expression is removed
before the evaluation of the last EXPRESSION so that a failure during the
evaluation of the last expression will backtrack directly to the parent choice
point of the EITHER expression.

EITHER is a special form, not a function. It is an error for the expression
#'EITHER to appear in a program."
  ;; FIXME: ref to operators providing nondeterministic contexts
  (cond ((not expressions)
         '(fail))
        ((not (rest expressions))
         (first expressions))
        (t
         `(if (a-boolean)
              ,(first expressions)
              (either ,@(rest expressions))))))

(defmacro-compile-time local (&body expressions &environment environment)
  "Evaluates EXPRESSIONS in the same fashion as PROGN except that all
SETF and SETQ expressions lexically nested in its body result in local
side effects which are undone upon backtracking.

This affects only side effects introduced explicitly via SETF and
SETQ. Side effects introduced by either user defined functions or builtin
Common Lisp functions such as RPLACA are always global.

Behaviour of side effects introduced by macro-expansions such as INCF
depends on the exact macro-expansion. If (INCF (FOO)) expands using
eg. SET-FOO, LOCAL is unable to undo the side-effect.

LOCAL does not currently distinguish between initially uninitialized
and intialized places, such as unbound variables or hash-table keys
with no prior values. As a result, an attempt to assign an unbound
variable inside LOCAL will signal an error due to the system's attempt
to first read the variable. Similarly, undoing a (SETF GETHASH) when
the key did not previously exist in the table will insert a NIL into
the table instead of doing a REMHASH.

LOCAL and GLOBAL expressions may be nested inside one another. The
nearest surrounding declaration determines whether or not a given SETF
or SETQ results in a local or global side effect.

Side effects default to be global when there is no surrounding LOCAL or GLOBAL
expression. Local side effects can appear both in deterministic as well as
nondeterministic contexts though different techniques are used to implement
the trailing of prior values for restoration upon backtracking. In
nondeterministic contexts, LOCAL as well as SETF are treated as special forms
rather than macros. This should be completely transparent to the user."
  (let ((*local?* t))
    `(progn ,@(mapcar
               #'(lambda (form) (perform-substitutions form environment))
               expressions))))

(defmacro-compile-time global (&body expressions &environment environment)
  "Evaluates EXPRESSIONS in the same fashion as PROGN except that all SETF and
SETQ expressions lexically nested in its body result in global side effects
which are not undone upon backtracking.

Note that this affects only side effects introduced explicitly via SETF and
SETQ. Side effects introduced by Common Lisp builtin functions such as RPLACA
are always global anyway.

Furthermore, it affects only occurrences of SETF and SETQ which appear
textually nested in the body of the GLOBAL expression -- not those appearing
in functions called from the body.

LOCAL and GLOBAL expressions may be nested inside one another. The nearest
surrounding declaration determines whether or not a given SETF or SETQ results
in a local or global side effect.

Side effects default to be global when there is no surrounding LOCAL or GLOBAL
expression. Global side effects can appear both in deterministic as well as
nondeterministic contexts. In nondeterministic contexts, GLOBAL as well as
SETF are treated as special forms rather than macros. This should be
completely transparent to the user."
  (let ((*local?* nil))
    `(progn ,@(mapcar
               #'(lambda (form) (perform-substitutions form environment))
               expressions))))

(defmacro-compile-time for-effects (&body forms &environment environment)
  "Evaluates FORMS as an implicit PROGN in a nondeterministic context and
returns NIL.

The body is repeatedly backtracked to its first choice-point until the body
fails.

Local side effects performed by FORMS are undone when FOR-EFFECTS returns.

A FOR-EFFECTS expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the FOR-EFFECTS expression appears in,
FORMS are always in a nondeterministic context.

A FOR-EFFECTS expression is is always deterministic."
  `(choice-point
    ,(let ((*nondeterministic-context?* t))
          (cps-convert-progn forms '#'fail nil nil environment))))

(defmacro-compile-time one-value (expression &optional (default-expression '(fail)))
  "Returns the first value of a nondeterministic expression. EXPRESSION is
evaluated, deterministically returning only its first nondeterministic value,
if any.

No further execution of EXPRESSION is attempted after it successfully returns
one value.

If EXPRESSION does not return any nondeterministic values \(i.e. it fails)
then DEFAULT-EXPRESSION is evaluated and its value returned instead.
DEFAULT-EXPRESSION defaults to \(FAIL) if not present.

Local side effects performed by EXPRESSION are undone when ONE-VALUE returns,
but local side effects performed by DEFAULT-EXPRESSION are not undone when
ONE-VALUE returns.

A ONE-VALUE expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the ONE-VALUE expression appears in,
EXPRESSION is always in a nondeterministic context, while DEFAULT-EXPRESSION
is in whatever context the ONE-VALUE expression appears.

A ONE-VALUE expression is nondeterministic if DEFAULT-EXPRESSION is present
and is nondeterministic, otherwise it is deterministic.

If DEFAULT-EXPRESSION is present and nondeterministic, and if EXPRESSION
fails, then it is possible to backtrack into the DEFAULT-EXPRESSION and for
the ONE-VALUE expression to nondeterministically return multiple times.
ONE-VALUE is analogous to the cut primitive \(!) in Prolog."
  `(block one-value
     (for-effects (return-from one-value ,expression))
     ,default-expression))

(defmacro-compile-time possibly? (&body forms)
  "Evaluates FORMS as an implicit PROGN in nondeterministic context,
returning true if the body ever yields true.

The body is repeatedly backtracked as long as it yields NIL. Returns
the first true value yielded by the body, or NIL if body fails before
yielding true.

Local side effects performed by the body are undone when POSSIBLY? returns.

A POSSIBLY? expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the POSSIBLY? expression appears in,
its body is always in a nondeterministic context.

A POSSIBLY? expression is always deterministic."
  `(one-value (let ((value (progn ,@forms))) (unless value (fail)) value) nil))

(defmacro-compile-time necessarily? (&body forms)
  "Evaluates FORMS as an implicit PROGN in nondeterministic context,
returning true if the body never yields false.

The body is repeatedly backtracked as long as it yields true. Returns the last
true value yielded by the body if it fails before yielding NIL, otherwise
returns NIL.

Local side effects performed by the body are undone when NECESSARILY? returns.

A NECESSARILY? expression can appear in both deterministic and
nondeterministic contexts. Irrespective of what context the NECESSARILY?
expression appears in, its body is always in a nondeterministic context.

A NECESSARILY? expression is always deterministic."
  `(let ((result t))
     (one-value
      (let ((value (progn ,@forms)))
        (when value (setf result value) (fail))
        value)
      result)))

(defmacro-compile-time all-values (&body expressions)
  "Evaluates EXPRESSIONS as an implicit PROGN and returns a list of all of the
nondeterministic values returned by the last EXPRESSION.

These values are produced by repeatedly evaluating the body and backtracking
to produce the next value, until the body fails and yields no further values.

Accordingly, local side effects performed by the body while producing each
value are undone before attempting to produce subsequent values, and all local
side effects performed by the body are undone upon exit from ALL-VALUES.

Returns the list containing NIL if there are no EXPRESSIONS. An ALL-VALUES
expression can appear in both deterministic and nondeterministic contexts.
Irrespective of what context the ALL-VALUES expression appears in, the
EXPRESSIONS are always in a nondeterministic context. An ALL-VALUES expression
itself is always deterministic. ALL-VALUES is analogous to the bagof primitive
in Prolog."
  (let ((values (gensym "VALUES"))
        (last-value-cons (gensym "LAST-VALUE-CONS")))
    `(let ((,values '())
           (,last-value-cons nil))
       (for-effects
         (let ((value (progn ,@expressions)))
           (global (if (null ,values)
                       (setf ,last-value-cons (list value)
                             ,values ,last-value-cons)
                       (setf (rest ,last-value-cons) (list value)
                             ,last-value-cons (rest ,last-value-cons))))))
       ,values)))

(defmacro-compile-time ith-value (i expression &optional (default-expression '(fail)))
  "Returns the Ith value of a nondeterministic expression. EXPRESSION is
evaluated, deterministically returning only its Ith nondeterministic value, if
any. I must be an integer. The first nondeterministic value returned by
EXPRESSION is numbered zero, the second one, etc. The Ith value is produced by
repeatedly evaluating EXPRESSION, backtracking through and discarding the
first I values and deterministically returning the next value produced.

No further execution of EXPRESSION is attempted after it successfully returns
the desired value.

If EXPRESSION fails before returning both the I values to be discarded, as
well as the desired Ith value, then DEFAULT-EXPRESSION is evaluated and its
value returned instead. DEFAULT-EXPRESSION defaults to \(FAIL) if not present.

Local side effects performed by EXPRESSION are undone when ITH-VALUE returns,
but local side effects performed by DEFAULT-EXPRESSION and by I are not undone
when ITH-VALUE returns.

An ITH-VALUE expression can appear in both deterministic and nondeterministic
contexts. Irrespective of what context the ITH-VALUE expression appears in,
EXPRESSION is always in a nondeterministic context, while DEFAULT-EXPRESSION
and I are in whatever context the ITH-VALUE expression appears.

An ITH-VALUE expression is nondeterministic if DEFAULT-EXPRESSION is present
and is nondeterministic, or if I is nondeterministic. Otherwise it is
deterministic.

If DEFAULT-EXPRESSION is present and nondeterministic, and if EXPRESSION
fails, then it is possible to backtrack into the DEFAULT-EXPRESSION and for
the ITH-VALUE expression to nondeterministically return multiple times.

If I is nondeterministic then the ITH-VALUE expression operates
nondeterministically on each value of I. In this case, backtracking for each
value of EXPRESSION and DEFAULT-EXPRESSION is nested in, and restarted for,
each backtrack of I."
  (let ((counter (gensym "I")))
    `(block ith-value
       (let ((,counter (value-of ,i)))
         (for-effects (let ((value ,expression))
                        (if (zerop ,counter)
                            (return-from ith-value value)
                            (decf ,counter))))
         ,default-expression))))

(defun trail (function)
  ;; note: Is it really better to use VECTOR-PUSH-EXTEND than CONS for the
  ;;       trail?
  (if *nondeterministic?* (vector-push-extend function *trail* 1024)))

(defun y-or-n-p
    (&optional (format-string nil format-string?) &rest format-args)
  (cond
    (*iscream?*
     (let ((query (if format-string?
                      (format nil "~A (Y or N): "
                              (apply #'format nil format-string format-args))
                      "(Y or N): ")))
       (emacs-eval '(y-or-n-p-begin))
       (unwind-protect
            (tagbody
             loop
               (format *query-io* "~%~A" query)
               (let ((char (read-char *query-io*)))
                 (when (or (char= char #\y) (char= char #\Y))
                   (format *query-io* "Y")
                   (return-from y-or-n-p t))
                 (when (or (char= char #\n) (char= char #\N))
                   (format *query-io* "N")
                   (return-from y-or-n-p nil)))
               (format *query-io* "Please type a single character, Y or N")
               (go loop))
         (emacs-eval '(y-or-n-p-end)))))
    (format-string? (apply #'cl:y-or-n-p format-string format-args))
    (t (cl:y-or-n-p))))

(defmacro-compile-time print-values (&body expressions)
  "Evaluates EXPRESSIONS as an implicit PROGN and prints
each of the nondeterministic values returned by the last EXPRESSION in
succession using PRINT.

After each value is printed, the user is queried as to whether or not further
values are desired. These values are produced by repeatedly evaluating the
body and backtracking to produce the next value, until either the user
indicates that no further values are desired or until the body fails and
yields no further values.

Accordingly, local side effects performed by the body while producing each
value are undone after printing each value, before attempting to produce
subsequent values, and all local side effects performed by the body are undone
upon exit from PRINT-VALUES, either because there are no further values or
because the user declines to produce further values.

A PRINT-VALUES expression can appear in both deterministic and
nondeterministic contexts. Irrespective of what context the PRINT-VALUES
expression appears in, the EXPRESSIONS are always in a nondeterministic
context. A PRINT-VALUES expression itself is always deterministic and always
returns NIL.

PRINT-VALUES is analogous to the standard top-level user interface in Prolog."
  ;; FIXME: Documentation lies: does not always return NIL.
  `(catch 'succeed
     (for-effects
       (let ((value (progn ,@expressions)))
         (print value)
         (unless (y-or-n-p "Do you want another solution?")
           (throw 'succeed value))))))

;;; note: Should have way of having a stream of values.

(eval-when (:compile-toplevel :load-toplevel :execute) (setf *screamer?* t))

(defun print-nondeterministic-function
    (nondeterministic-function stream print-level)
  (declare (ignore print-level))
  (format stream "#<~A ~S>"
          'nondeterministic
          (nondeterministic-function-function nondeterministic-function)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'a-boolean))

(cl:defun a-boolean ()
  "Equivalent to \(EITHER T NIL)."
  (screamer-error
   "A-BOOLEAN is a nondeterministic function. As such, it must be called only~%~
   from a nondeterministic context."))

(cl:defun a-boolean-nondeterministic (continuation)
  (choice-point (funcall continuation t))
  (funcall continuation nil))

(defun fail ()
  "Backtracks to the most recent choise point. Equivalent to
\(EITHER). Note that FAIL is deterministic function and thus it is
permissible to reference #'FAIL, and write \(FUNCALL #'FAIL) or
\(APPLY #'FAIL). In nondeterministic contexts, the expression \(FAIL)
is optimized to generate inline backtracking code."
  ;; FIXME: Since we export FAIL, throwing to it is probably a bad idea.
  ;; ...better throw to %FAIL.
  (throw 'fail nil))

(defmacro-compile-time when-failing ((&body failing-forms) &body forms)
  (let ((old-fail (gensym "FAIL-")))
    `(let ((,old-fail #'fail))
       (unwind-protect
            (progn (setf (symbol-function 'fail)
                         #'(lambda () ,@failing-forms (funcall ,old-fail)))
                   ,@forms)
         (setf (symbol-function 'fail) ,old-fail)))))

(defmacro-compile-time count-failures (&body forms)
  (let ((values (gensym "VALUES-")))
    `(let ((failure-count 0))
       (when-failing ((incf failure-count))
         (let ((,values (multiple-value-list (progn ,@forms))))
           (format t "Failures         = ~10<~;~d~>" failure-count)
           (values-list ,values))))))

(defun nondeterministic-function? (x)
  "Returns T if X is a nondeterministic function and NIL otherwise.

#'FOO returns a nondeterministic function object iff it is used in nondeterminisitc
context and FOO is either a nondeterministic LAMBDA form, or the name of a
nondeterministic function defined using SCREAMER::DEFUN.

Currently, if FOO is a nondeterministic function defined using
SCREAMER::DEFUN, #'FOO and \(SYMBOL-FUNCTION 'FOO) in deterministic context
will return an ordinary deterministic Common Lisp function, which will signal
an error at runtime."
  (nondeterministic-function?-internal (value-of x)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'funcall-nondeterministic))

(cl:defun funcall-nondeterministic (function &rest arguments)
  "Analogous to CL:FUNCALL, except FUNCTION can be either a nondeterministic
function, or an ordinary determinisitic function.

You must use FUNCALL-NONDETERMINISTIC to funcall a nondeterministic function.
An error is signalled if you attempt to funcall a nondeterministic
function object with CL:FUNCALL.

You can use FUNCALL-NONDETERMINISTIC to funcall either a deterministic or
nondeterministic function, though even if all of the ARGUMENTS are
deterministic and FUNCTION is a deterministic function object, the call
expression will still be nondeterministic \(with presumably a single value),
since it is impossible to determine at compile time that a given call to
FUNCALL-NONDETERMINISTIC will be passed only deterministic function objects
for function."
  (declare (ignore function arguments))
  (screamer-error
   "FUNCALL-NONDETERMINISTIC is a nondeterministic function. As such, it~%~
   must be called only from a nondeterministic context."))

(cl:defun funcall-nondeterministic-nondeterministic
    (continuation function &rest arguments)
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
        (apply (nondeterministic-function-function function)
               continuation
               arguments)
        (funcall continuation (apply function arguments)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'apply-nondeterministic))

(cl:defun apply-nondeterministic (function &rest arguments)
  "Analogous to the CL:APPLY, except FUNCTION can be either a nondeterministic
function, or an ordinary deterministic function.

You must use APPLY-NONDETERMINISTIC to apply a nondeterministic function. An
error is signalled if a nondeterministic function object is used with
CL:APPLY.

You can use APPLY-NONDETERMINISTIC to apply either a deterministic or
nondeterministic function, though even if all of the ARGUMENTS are
deterministic and FUNCTION is a deterministic function object, the call
expression will still be nondeterministic \(with presumably a single value),
since it is impossible to determine at compile time that a given call to
APPLY-NONDETERMINISTIC will be passed only deterministic function objects for
function."
  (declare (ignore function arguments))
  (screamer-error
   "APPLY-NONDETERMINISTIC is a nondeterministic function. As such, it must~%~
   be called only from a nondeterministic context."))

(cl:defun apply-nondeterministic-nondeterministic
    (continuation function argument &rest arguments)
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
        ;; note: I don't know how to avoid the consing here.
        (apply (nondeterministic-function-function function)
               continuation
               (apply #'list* (cons argument arguments)))
        (funcall continuation (apply function argument arguments)))))

(cl:defun multiple-value-call-nondeterministic (function-form &rest values-forms)
  "Analogous to the CL:MULTIPLE-VALUE-CALL, except FUNCTION-FORM can evaluate
to either a nondeterministic function, or an ordinary deterministic function.

You must use MULTIPLE-VALUE-CALL-NONDETERMINISTIC to multiple-value-call a
nondeterministic function. An error is signalled if a nondeterministic function
object is used with CL:MULTIPLE-VALUE-CALL.

You can use MULTIPLE-VALUE-CALL-NONDETERMINISTIC to call either a
deterministic or nondeterministic function, though even if all of the
VALUES-FORMS are deterministic and FUNCTION-FORM evaluates to a deterministic
function object, the call expression will still be nondeterministic \(with
presumably a single value), since it is impossible to determine at compile
time that a given call to MULTIPLE-VALUE-CALL-NONDETERMINISTIC will be passed
only deterministic function objects for function.

While MULTIPLE-VALUE-CALL-NONDETERMINISTIC appears to be a function, it
is really a special-operator implemented by the code-walkers processing
nondeterministic source contexts."
  (declare (ignore function-form values-forms))
  (screamer-error
   "MULTIPLE-VALUE-CALL-NONDETERMINISTIC is a nondeterministic special form. As such,~%~
    it must be called only from a nondeterministic context."))

(defmacro-compile-time multiple-value-bind
    (variables form &body body &environment environment)
  (if (every #'(lambda (form) (deterministic? form environment))
             (peal-off-documentation-string-and-declarations body))
      `(cl:multiple-value-bind ,variables ,form ,@body)
      (let ((other-arguments (gensym "OTHER-")))
        `(multiple-value-call-nondeterministic
          #'(lambda (&optional ,@variables &rest ,other-arguments)
              (declare (ignore ,other-arguments))
              ,@body)
          ,form))))

(defun unwind-trail ()
  (tagbody
   loop
     (if (zerop (fill-pointer *trail*)) (return-from unwind-trail))
     (funcall (vector-pop *trail*))
     ;; note: This is to allow the trail closures to be garbage collected.
     (setf (aref *trail* (fill-pointer *trail*)) nil)
     (go loop)))

(defun purge (function-name)
  "Removes any information about FUNCTION-NAME from Screamer's
who-calls database."
  (remhash (value-of function-name) *function-record-table*)
  t)

(defun unwedge-screamer ()
  "Removes any information about all user defined functions from
Screamer's who-calls database."
  (maphash #'(lambda (function-name function-record)
               (unless (function-record-screamer? function-record)
                 (remhash function-name *function-record-table*)))
           *function-record-table*)
  t)

;;; note: These optimized versions of AN-INTEGER, AN-INTEGER-ABOVE,
;;;       AN-INTEGER-BELOW, AN-INTEGER-BETWEEN and A-MEMBER-OF have different
;;;       failure behavior as far as WHEN-FAILING is concerned than the
;;;       original purely Screamer versions. This is likely to affect only
;;;       failure counts generated by COUNT-FAILURES. A small price to pay for
;;;       tail recursion optimization.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer))

(cl:defun an-integer ()
  "Generator yielding integers in sequence 0, 1, -1, 2, -2, ..."
  (screamer-error
   "AN-INTEGER is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun an-integer-nondeterministic (continuation)
  (choice-point-external
   (choice-point-internal (funcall continuation 0))
   (let ((i 1))
     (loop (choice-point-internal (funcall continuation i))
       (choice-point-internal (funcall continuation (- i)))
       (incf i)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer-above))

(cl:defun an-integer-above (low)
  "Generator yielding integers starting from LOW and continuing sequentially
in increasing direction."
  (declare (ignore low))
  (screamer-error
   "AN-INTEGER-ABOVE is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-above-nondeterministic (continuation low)
  (let ((low (ceiling (value-of low))))
    (choice-point-external
     (let ((i low))
       (loop (choice-point-internal (funcall continuation i))
         (incf i))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer-below))

(cl:defun an-integer-below (high)
  "Generator yielding integers starting from HIGH and continuing sequentially
in decreasing direction."
  (declare (ignore high))
  (screamer-error
   "AN-INTEGER-BELOW is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-below-nondeterministic (continuation high)
  (let ((high (floor (value-of high))))
    (choice-point-external
     (let ((i high))
       (loop (choice-point-internal (funcall continuation i))
         (decf i))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'an-integer-between))

(cl:defun an-integer-between (low high)
  "Nondeterministically returns an integer in the closed interval [LOW, HIGH].
The results are returned in ascending order. Both LOW and HIGH must be
integers. Fails if the interval does not contain any integers."
  (declare (ignore low high))
  (screamer-error
   "AN-INTEGER-BETWEEN is a nondeterministic function. As such, it must be~%~
   called only from a nondeterministic context."))

(cl:defun an-integer-between-nondeterministic (continuation low high)
  (let ((low (ceiling (value-of low)))
        (high (floor (value-of high))))
    (unless (> low high)
      (choice-point-external
       (do ((i low (1+ i))) ((= i high))
         (choice-point-internal (funcall continuation i))))
      (funcall continuation high))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'a-member-of))

(cl:defun a-member-of (sequence)
  "Nondeterministically returns an element of SEQUENCE. The elements are
returned in the order that they appear in SEQUENCE. The SEQUENCE must be
either a list or a vector."
  (declare (ignore sequence))
  (screamer-error
   "A-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))

(cl:defun a-member-of-nondeterministic (continuation sequence)
  (let ((sequence (value-of sequence)))
    (cond
      ((listp sequence)
       (unless (null sequence)
         (choice-point-external
          (loop (if (null (rest sequence)) (return))
            (choice-point-internal (funcall continuation (first sequence)))
            (setf sequence (value-of (rest sequence)))))
         (funcall continuation (first sequence))))
      ((vectorp sequence)
       (let ((n (1- (length sequence))))
         (unless (zerop n)
           (choice-point-external
            (dotimes (i n)
              (choice-point-internal (funcall continuation (aref sequence i)))))
           (funcall continuation (aref sequence n)))))
      (t (error "SEQUENCE must be a sequence")))))

;;; note: The following two functions work only when Screamer is running under
;;;       ILisp/GNUEmacs with iscream.el loaded.

(defun emacs-eval (expression)
  (unless *iscream?*
    (error "Cannot do EMACS-EVAL unless Screamer is running under~%~
          ILisp/GNUEmacs with iscream.el loaded."))
  (format *terminal-io* "~A~A~A"
          (format nil "~A" (code-char 27))
          (string-downcase (format nil "~A" expression))
          (format nil "~A" (code-char 29))))

(defmacro-compile-time local-output (&body forms)
  `(progn
     (unless *iscream?*
       (error "Cannot do LOCAL-OUTPUT unless Screamer is running under~%~
            ILisp/GNUEmacs with iscream.el loaded."))
     (trail #'(lambda () (emacs-eval '(pop-end-marker))))
     (emacs-eval '(push-end-marker))
     ,@forms))

;;; Constraints

(defvar *name* 0 "The counter for anonymous names.")

(defvar *minimum-shrink-ratio* 1e-2
  "Ignore propagations which reduce the range of a variable by less than this
ratio.")

(defvar *maximum-discretization-range* 20
  "Discretize integer variables whose range is not greater than this number.
Discretize all integer variables if NIL. Must be an integer or NIL.")

(defvar *strategy* :gfc
  "Strategy to use for FUNCALLV and APPLYV. Either :GFC for Generalized
Forward Checking, or :AC for Arc Consistency. Default is :GFC.")

;;; note: Enable this to use CLOS instead of DEFSTRUCT for variables.
#+(or)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :screamer-clos *features* :test #'eq))

#-screamer-clos
(defstruct-compile-time (variable (:print-function print-variable)
                                  (:predicate variable?)
                                  (:constructor make-variable-internal))
  name
  (noticers nil)
  (enumerated-domain t)
  (enumerated-antidomain nil)
  value
  (possibly-integer? t)
  (possibly-noninteger-real? t)
  (possibly-nonreal-number? t)
  (possibly-boolean? t)
  (possibly-nonboolean-nonnumber? t)
  (lower-bound nil)
  (upper-bound nil))

#+screamer-clos
(defclass variable ()
  ((name :accessor variable-name :initarg :name)
   (noticers :accessor variable-noticers :initform nil)
   (enumerated-domain :accessor variable-enumerated-domain :initform t)
   (enumerated-antidomain :accessor variable-enumerated-antidomain
                          :initform nil)
   (value :accessor variable-value)
   (possibly-integer? :accessor variable-possibly-integer? :initform t)
   (possibly-noninteger-real? :accessor variable-possibly-noninteger-real?
                              :initform t)
   (possibly-nonreal-number? :accessor variable-possibly-nonreal-number?
                             :initform t)
   (possibly-boolean? :accessor variable-possibly-boolean? :initform t)
   (possibly-nonboolean-nonnumber?
    :accessor variable-possibly-nonboolean-nonnumber?
    :initform t)
   (lower-bound :accessor variable-lower-bound :initform nil)
   (upper-bound :accessor variable-upper-bound :initform nil)))

#+screamer-clos
(defmethod print-object ((variable variable) stream)
  (print-variable variable stream nil))

#+screamer-clos
(defun-compile-time variable? (thing) (typep thing 'variable))

(defun booleanp (x) (typep x 'boolean))

(defun infinity-min (x y) (and x y (min x y)))

(defun infinity-max (x y) (and x y (max x y)))

(defun infinity-+ (x y) (and x y (+ x y)))

(defun infinity-- (x y) (and x y (- x y)))

(defun infinity-* (x y) (and x y (* x y)))

(defun contains-variables? (x)
  (typecase x
    (cons (or (contains-variables? (car x)) (contains-variables? (cdr x))))
    (variable t)
    (otherwise nil)))

(defun eliminate-variables (x)
  (if (contains-variables? x)
      (if (consp x)
          (cons (eliminate-variables (car x)) (eliminate-variables (cdr x)))
          (eliminate-variables (variable-value x)))
      x))

(defun print-variable (x stream print-level)
  (declare (ignore print-level))
  (let ((x (value-of x)))
    (cond
      ((variable? x)
       (if (and (not (eq (variable-enumerated-domain x) t))
                (not (null (variable-enumerated-antidomain x))))
           (error "This shouldn't happen"))
       (format stream "[~S" (variable-name x))
       (format stream "~A"
               (cond ((variable-boolean? x) " Boolean")
                     ((variable-integer? x) " integer")
                     ((variable-real? x)
                      (if (variable-noninteger? x) " noninteger-real" " real"))
                     ((variable-number? x)
                      (cond ((variable-nonreal? x) " nonreal-number")
                            ((variable-noninteger? x) " noninteger-number")
                            (t " number")))
                     ((variable-nonnumber? x) " nonnumber")
                     ((variable-nonreal? x) " nonreal")
                     ((variable-noninteger? x) " noninteger")
                     (t "")))
       (if (variable-real? x)
           (if (variable-lower-bound x)
               (if (variable-upper-bound x)
                   (format stream " ~D:~D"
                           (variable-lower-bound x) (variable-upper-bound x))
                   (format stream " ~D:" (variable-lower-bound x)))
               (if (variable-upper-bound x)
                   (format stream " :~D" (variable-upper-bound x)))))
       (if (and (not (eq (variable-enumerated-domain x) t))
                (not (variable-boolean? x)))
           (format stream " enumerated-domain:~S"
                   (variable-enumerated-domain x)))
       (if (not (null (variable-enumerated-antidomain x)))
           (format stream " enumerated-antidomain:~S"
                   (variable-enumerated-antidomain x)))
       (format stream "]"))
      (t (format stream "~S" x)))))

(defun make-variable (&optional (name nil name?))
  "Creates and returns a new variable. Variables are assigned a name
which is only used to identify the variable when it is printed. If the
parameter NAME is given then it is assigned as the name of the
variable. Otherwise, a unique name is assigned. The parameter NAME can
be any Lisp object."
  (let ((variable
         #-screamer-clos
          (make-variable-internal :name (if name? name (incf *name*)))
          #+screamer-clos
          (make-instance 'variable :name (if name? name (incf *name*)))))
    (setf (variable-value variable) variable)
    variable))

(defun variable-integer? (x)
  (and (not (variable-possibly-boolean? x))
       (not (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (variable-possibly-integer? x)))

(defun variable-noninteger? (x)
  (and (or (variable-possibly-boolean? x)
           (variable-possibly-nonboolean-nonnumber? x)
           (variable-possibly-nonreal-number? x)
           (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(defun variable-real? (x)
  (and (not (variable-possibly-boolean? x))
       (not (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (or (variable-possibly-noninteger-real? x)
           (variable-possibly-integer? x))))

(defun variable-nonreal? (x)
  (and (or (variable-possibly-boolean? x)
           (variable-possibly-nonboolean-nonnumber? x)
           (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(defun variable-number? (x)
  (and (not (variable-possibly-boolean? x))
       (not (variable-possibly-nonboolean-nonnumber? x))
       (or (variable-possibly-nonreal-number? x)
           (variable-possibly-noninteger-real? x)
           (variable-possibly-integer? x))))

(defun variable-nonnumber? (x)
  (and (or (variable-possibly-boolean? x)
           (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(defun variable-boolean? (x)
  (and (variable-possibly-boolean? x)
       (not (variable-possibly-nonboolean-nonnumber? x))
       (not (variable-possibly-nonreal-number? x))
       (not (variable-possibly-noninteger-real? x))
       (not (variable-possibly-integer? x))))

(defun variable-nonboolean? (x)
  (and (not (variable-possibly-boolean? x))
       (or (variable-possibly-nonboolean-nonnumber? x)
           (variable-possibly-nonreal-number? x)
           (variable-possibly-noninteger-real? x)
           (variable-possibly-integer? x))))

(defun variable-true? (x) (eq (variable-value x) t))

(defun variable-false? (x) (null (variable-value x)))

(defun value-of (x)
  "Returns X if X is not a variable. If X is a variable then VALUE-OF
dereferences X and returns the dereferenced value. If X is bound then
the value returned will not be a variable. If X is unbound then the
value returned will be a variable which may be X itself or another
variable which is shared with X."
  (tagbody
   loop
     (if (or (not (variable? x))
             #+screamer-clos (not (slot-boundp x 'value))
             (eq (variable-value x) x))
         (return-from value-of x))
     (setf x (variable-value x))
     (go loop)))

(defun variablize (x)
  (if (variable? x)
      (tagbody
       loop
         (if (or (not (variable? (variable-value x)))
                 (eq (variable-value x) x))
             (return-from variablize x))
         (setf x (variable-value x))
         (go loop))
      (let ((y (make-variable))) (restrict-value! y x) y)))

(defun bound? (x)
  "Returns T if X is not a variable or if X is a bound
variable. Otherwise returns NIL. BOUND? is analogous to the
extra-logical predicates VAR and NONVAR typically available in
Prolog."
  (not (variable? (value-of x))))

(defun ground? (x)
  "The primitive GROUND? is an extension of the primitive BOUND? which
can recursively determine whether an entire aggregate object is
bound. Returns T if X is bound and either the value of X is atomic or
all of the slots in the value of X are also bound. Otherwise returns
nil."
  (let ((x (value-of x)))
    (and (not (variable? x))
         (or (not (consp x)) (and (ground? (car x)) (ground? (cdr x)))))))

(defun apply-substitution (x)
  (let ((x (value-of x)))
    (if (consp x)
        (cons (apply-substitution (car x)) (apply-substitution (cdr x)))
        x)))

(defun occurs-in? (x value)
  ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; note: Will loop if VALUE is circular.
  (cond
    ((eq x value) t)
    ((and (variable? value) (not (eq value (variable-value value))))
     (occurs-in? x (variable-value value)))
    ((consp value) (or (occurs-in? x (car value)) (occurs-in? x (cdr value))))
    (t nil)))

(defun attach-noticer!-internal (noticer x)
  ;; note: Will loop if X is circular.
  (typecase x
    (cons (attach-noticer!-internal noticer (car x))
          (attach-noticer!-internal noticer (cdr x)))
    (variable (if (eq x (variable-value x))
                  ;; note: I can't remember why this check for duplication is
                  ;;       here.
                  (unless (member noticer (variable-noticers x) :test #'eq)
                    ;; note: This can't be a PUSH because of the Lucid screw.
                    (local (setf (variable-noticers x)
                                 (cons noticer (variable-noticers x)))))
                  (attach-noticer!-internal noticer (variable-value x))))))

(defun attach-noticer! (noticer x)
  (attach-noticer!-internal noticer x)
  (funcall noticer))

(defun run-noticers (x)
  (dolist (noticer (variable-noticers x)) (funcall noticer)))

;;; Restrictions

(defun restrict-integer! (x)
  ;; note: X must be a variable.
  (unless (variable-possibly-integer? x) (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when (variable-possibly-boolean? x)
          (local (setf (variable-possibly-boolean? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when (and (variable-lower-bound x)
                   (not (integerp (variable-lower-bound x))))
          (if (and (variable-upper-bound x)
                   (< (variable-upper-bound x)
                      (ceiling (variable-lower-bound x))))
              (fail))
          (local (setf (variable-lower-bound x)
                       (ceiling (variable-lower-bound x))))
          (setf run? t))
        (when (and (variable-upper-bound x)
                   (not (integerp (variable-upper-bound x))))
          (if (and (variable-lower-bound x)
                   (> (variable-lower-bound x)
                      (floor (variable-upper-bound x))))
              (fail))
          (local (setf (variable-upper-bound x) (floor (variable-upper-bound x))))
          (setf run? t))
        (when run?
          (cond ((eq (variable-enumerated-domain x) t)
                 (if (and (variable-lower-bound x)
                          (variable-upper-bound x)
                          (or (null *maximum-discretization-range*)
                              (<= (- (variable-upper-bound x)
                                     (variable-lower-bound x))
                                  *maximum-discretization-range*)))
                     (set-enumerated-domain!
                      x (all-values (an-integer-between
                                     (variable-lower-bound x)
                                     (variable-upper-bound x))))))
                ((not (every #'integerp (variable-enumerated-domain x)))
                 ;; note: Could do less consing if had LOCAL DELETE-IF.
                 ;;       This would also allow checking list only once.
                 (set-enumerated-domain!
                  x (remove-if-not #'integerp (variable-enumerated-domain x)))))
          (run-noticers x)))))

(defun restrict-noninteger! (x)
  ;; note: X must be a variable.
  (unless (or (variable-possibly-noninteger-real? x)
              (variable-possibly-nonreal-number? x)
              (variable-possibly-boolean? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (variable-possibly-integer? x))
    (local (setf (variable-possibly-integer? x) nil))
    (if (and (not (eq (variable-enumerated-domain x) t))
             (some #'integerp (variable-enumerated-domain x)))
        ;; note: Could do less consing if had LOCAL DELETE-IF.
        ;;       This would also allow checking list only once.
        (set-enumerated-domain!
         x (remove-if #'integerp (variable-enumerated-domain x))))
    (run-noticers x)))

(defun restrict-real! (x)
  ;; note: X must be a variable.
  (unless (or (variable-possibly-integer? x)
              (variable-possibly-noninteger-real? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when (variable-possibly-boolean? x)
          (local (setf (variable-possibly-boolean? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (not (every #'realp (variable-enumerated-domain x))))
              ;; note: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if-not #'realp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-nonreal! (x)
  ;; note: X must be a variable.
  (unless (or (variable-possibly-nonreal-number? x)
              (variable-possibly-boolean? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-integer? x)
          (local (setf (variable-possibly-integer? x) nil))
          (setf run? t))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (some #'realp (variable-enumerated-domain x)))
              ;; note: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if #'realp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-number! (x)
  ;; note: X must be a variable.
  (unless (or (variable-possibly-integer? x)
              (variable-possibly-noninteger-real? x)
              (variable-possibly-nonreal-number? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-boolean? x)
          (local (setf (variable-possibly-boolean? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (not (every #'numberp (variable-enumerated-domain x))))
              ;; note: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if-not #'numberp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-nonnumber! (x)
  ;; note: X must be a variable.
  (unless (or (variable-possibly-boolean? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-integer? x)
          (local (setf (variable-possibly-integer? x) nil))
          (setf run? t))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when run?
          (if (and (not (eq (variable-enumerated-domain x) t))
                   (some #'numberp (variable-enumerated-domain x)))
              ;; note: Could do less consing if had LOCAL DELETE-IF.
              ;;       This would also allow checking list only once.
              (set-enumerated-domain!
               x (remove-if #'numberp (variable-enumerated-domain x))))
          (run-noticers x)))))

(defun restrict-boolean! (x)
  ;; note: X must be a variable.
  (unless (variable-possibly-boolean? x) (fail))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (variable-possibly-integer? x)
          (local (setf (variable-possibly-integer? x) nil))
          (setf run? t))
        (when (variable-possibly-noninteger-real? x)
          (local (setf (variable-possibly-noninteger-real? x) nil))
          (setf run? t))
        (when (variable-possibly-nonreal-number? x)
          (local (setf (variable-possibly-nonreal-number? x) nil))
          (setf run? t))
        (when (variable-possibly-nonboolean-nonnumber? x)
          (local (setf (variable-possibly-nonboolean-nonnumber? x) nil))
          (setf run? t))
        (when run?
          (cond
            ((eq (variable-enumerated-domain x) t)
             (local
               (cond
                 ((member t (variable-enumerated-antidomain x) :test #'eq)
                  (cond ((member nil (variable-enumerated-antidomain x) :test #'eq)
                         (fail))
                        (t (setf (variable-enumerated-domain x) '(nil))
                           (setf (variable-enumerated-antidomain x) '())
                           (setf (variable-value x) nil))))
                 ((member nil (variable-enumerated-antidomain x) :test #'eq)
                  (setf (variable-enumerated-domain x) '(t))
                  (setf (variable-enumerated-antidomain x) '())
                  (setf (variable-value x) t))
                 (t (setf (variable-enumerated-domain x) '(t nil))
                    (unless (null (variable-enumerated-antidomain x))
                      (setf (variable-enumerated-antidomain x) '()))))))
            ((not (every #'booleanp (variable-enumerated-domain x)))
             ;; note: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if-not #'booleanp (variable-enumerated-domain x)))))
          (run-noticers x)))))

(defun restrict-nonboolean! (x)
  ;; note: X must be a variable.
  (unless (or (variable-possibly-integer? x)
              (variable-possibly-noninteger-real? x)
              (variable-possibly-nonreal-number? x)
              (variable-possibly-nonboolean-nonnumber? x))
    (fail))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (variable-possibly-boolean? x))
    (local (setf (variable-possibly-boolean? x) nil))
    (cond ((eq (variable-enumerated-domain x) t)
           (local (setf (variable-enumerated-antidomain x)
                        (adjoin t
                                (adjoin nil (variable-enumerated-antidomain x)
                                        :test #'eq)
                                :test #'eq))))
          ((some #'booleanp (variable-enumerated-domain x))
           ;; note: Could do less consing if had LOCAL DELETE-IF.
           ;;       This would also allow checking list only once.
           (set-enumerated-domain!
            x (remove-if #'booleanp (variable-enumerated-domain x)))))
    (run-noticers x)))

(defun restrict-lower-bound! (x lower-bound)
  ;; note: X must be a variable.
  ;; note: LOWER-BOUND must be a real constant.
  (if (variable-integer? x) (setf lower-bound (ceiling lower-bound)))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (or (not (variable-lower-bound x))
                 (> lower-bound (variable-lower-bound x))))
    (if (and (variable-upper-bound x) (< (variable-upper-bound x) lower-bound))
        (fail))
    (when (or (not (variable-lower-bound x))
              (not (variable-upper-bound x))
              (>= (/ (- lower-bound (variable-lower-bound x))
                     (- (variable-upper-bound x) (variable-lower-bound x)))
                  *minimum-shrink-ratio*))
      (local (setf (variable-lower-bound x) lower-bound))
      (cond ((eq (variable-enumerated-domain x) t)
             (if (and lower-bound
                      (variable-upper-bound x)
                      (variable-integer? x)
                      (or (null *maximum-discretization-range*)
                          (<= (- (variable-upper-bound x) lower-bound)
                              *maximum-discretization-range*)))
                 (set-enumerated-domain!
                  x (all-values (an-integer-between lower-bound
                                                    (variable-upper-bound x))))))
            ((some #'(lambda (element) (< element lower-bound))
                   (variable-enumerated-domain x))
             ;; note: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if #'(lambda (element) (< element lower-bound))
                           (variable-enumerated-domain x)))))
      (run-noticers x))))

(defun restrict-upper-bound! (x upper-bound)
  ;; note: X must be a variable.
  ;; note: UPPER-BOUND must be a real constant.
  (when (variable-integer? x)
    (setf upper-bound (floor upper-bound)))
  (when (and (or (eq (variable-value x) x) (not (variable? (variable-value x))))
             (or (not (variable-upper-bound x))
                 (< upper-bound (variable-upper-bound x))))
    (when (and (variable-lower-bound x) (> (variable-lower-bound x) upper-bound))
      (fail))
    (when (or (not (variable-lower-bound x))
              (not (variable-upper-bound x))
              (>= (/ (- (variable-upper-bound x) upper-bound)
                     (- (variable-upper-bound x) (variable-lower-bound x)))
                  *minimum-shrink-ratio*))
      (local (setf (variable-upper-bound x) upper-bound))
      (cond ((eq (variable-enumerated-domain x) t)
             (when (and (variable-lower-bound x)
                        upper-bound
                        (variable-integer? x)
                        (or (null *maximum-discretization-range*)
                            (<= (- upper-bound (variable-lower-bound x))
                                *maximum-discretization-range*)))
               (set-enumerated-domain!
                x (all-values (an-integer-between (variable-lower-bound x)
                                                  upper-bound)))))
            ((some #'(lambda (element) (> element upper-bound))
                   (variable-enumerated-domain x))
             ;; note: Could do less consing if had LOCAL DELETE-IF.
             ;;       This would also allow checking list only once.
             (set-enumerated-domain!
              x (remove-if #'(lambda (element) (> element upper-bound))
                           (variable-enumerated-domain x)))))
      (run-noticers x))))

(defun restrict-bounds! (x lower-bound upper-bound)
  ;; note: X must be a variable.
  ;; note: LOWER-BOUND and UPPER-BOUND must be real constants.
  (when (variable-integer? x)
    (if lower-bound (setf lower-bound (ceiling lower-bound)))
    (if upper-bound (setf upper-bound (floor upper-bound))))
  (if (or (eq (variable-value x) x) (not (variable? (variable-value x))))
      (let ((run? nil))
        (when (and lower-bound
                   (or (not (variable-lower-bound x))
                       (> lower-bound (variable-lower-bound x))))
          (if (and (variable-upper-bound x)
                   (< (variable-upper-bound x) lower-bound))
              (fail))
          (when (or (not (variable-lower-bound x))
                    (not (variable-upper-bound x))
                    (>= (/ (- lower-bound (variable-lower-bound x))
                           (- (variable-upper-bound x) (variable-lower-bound x)))
                        *minimum-shrink-ratio*))
            (local (setf (variable-lower-bound x) lower-bound))
            (setf run? t)))
        (when (and upper-bound
                   (or (not (variable-upper-bound x))
                       (< upper-bound (variable-upper-bound x))))
          (if (and (variable-lower-bound x)
                   (> (variable-lower-bound x) upper-bound))
              (fail))
          (when (or (not (variable-lower-bound x))
                    (not (variable-upper-bound x))
                    (>= (/ (- (variable-upper-bound x) upper-bound)
                           (- (variable-upper-bound x) (variable-lower-bound x)))
                        *minimum-shrink-ratio*))
            (local (setf (variable-upper-bound x) upper-bound))
            (setf run? t)))
        (when run?
          (cond ((eq (variable-enumerated-domain x) t)
                 (if (and (variable-lower-bound x)
                          (variable-upper-bound x)
                          (variable-integer? x)
                          (or (null *maximum-discretization-range*)
                              (<= (- (variable-upper-bound x)
                                     (variable-lower-bound x))
                                  *maximum-discretization-range*)))
                     (set-enumerated-domain!
                      x (all-values (an-integer-between
                                     (variable-lower-bound x)
                                     (variable-upper-bound x))))))
                ((or (and lower-bound
                          (some #'(lambda (element) (< element lower-bound))
                                (variable-enumerated-domain x)))
                     (and upper-bound
                          (some #'(lambda (element) (> element upper-bound))
                                (variable-enumerated-domain x))))
                 ;; note: Could do less consing if had LOCAL DELETE-IF.
                 ;;       This would also allow checking list only once.
                 (set-enumerated-domain!
                  x (remove-if #'(lambda (element)
                                   (or (and lower-bound (< element lower-bound))
                                       (and upper-bound (> element upper-bound))))
                               (variable-enumerated-domain x)))))
          (run-noticers x)))))

(defun share! (x y)
  ;; note: X and Y must be variables such that (EQ X (VALUE-OF X)) and
  ;;       (EQ Y (VALUE-OF Y)).
  (let ((run? nil)
        (y-lower-bound? nil)
        (y-upper-bound? nil)
        (x-lower-bound (variable-lower-bound x))
        (x-upper-bound (variable-upper-bound x))
        (y-lower-bound (variable-lower-bound y))
        (y-upper-bound (variable-upper-bound y)))
    (cond ((and (variable-integer? y) (not (variable-integer? x)))
           (if x-lower-bound (setf x-lower-bound (ceiling x-lower-bound)))
           (if x-upper-bound (setf x-upper-bound (floor x-upper-bound))))
          ((and (not (variable-integer? y)) (variable-integer? x))
           (when (and y-lower-bound (not (integerp y-lower-bound)))
             (setf y-lower-bound (ceiling y-lower-bound))
             (setf y-lower-bound? t))
           (when (and y-upper-bound (not (integerp y-upper-bound)))
             (setf y-upper-bound (floor y-upper-bound))
             (setf y-upper-bound? t))))
    (when (and (not (variable-possibly-integer? x))
               (variable-possibly-integer? y))
      (local (setf (variable-possibly-integer? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-noninteger-real? x))
               (variable-possibly-noninteger-real? y))
      (local (setf (variable-possibly-noninteger-real? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-nonreal-number? x))
               (variable-possibly-nonreal-number? y))
      (local (setf (variable-possibly-nonreal-number? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-boolean? x))
               (variable-possibly-boolean? y))
      (local (setf (variable-possibly-boolean? y) nil))
      (setf run? t))
    (when (and (not (variable-possibly-nonboolean-nonnumber? x))
               (variable-possibly-nonboolean-nonnumber? y))
      (local (setf (variable-possibly-nonboolean-nonnumber? y) nil))
      (setf run? t))
    (unless (or (variable-possibly-integer? y)
                (variable-possibly-noninteger-real? y)
                (variable-possibly-nonreal-number? y)
                (variable-possibly-boolean? y)
                (variable-possibly-nonboolean-nonnumber? y))
      (fail))
    (cond ((and x-lower-bound
                (or (not y-lower-bound) (> x-lower-bound y-lower-bound)))
           (local (setf (variable-lower-bound y) x-lower-bound))
           (setf run? t))
          (y-lower-bound?
           (local (setf (variable-lower-bound y) y-lower-bound))
           (setf run? t)))
    (cond ((and x-upper-bound
                (or (not y-upper-bound) (< x-upper-bound y-upper-bound)))
           (local (setf (variable-upper-bound y) x-upper-bound))
           (setf run? t))
          (y-upper-bound?
           (local (setf (variable-upper-bound y) y-upper-bound))
           (setf run? t)))
    (unless (or (null (variable-lower-bound y))
                (null (variable-upper-bound y))
                (< (variable-lower-bound y) (variable-upper-bound y)))
      (fail))
    (if run?
        (let ((lower-bound (variable-lower-bound y))
              (upper-bound (variable-upper-bound y)))
          (if (eq (variable-enumerated-domain y) t)
              (if (and lower-bound
                       upper-bound
                       (variable-integer? y)
                       (or (null *maximum-discretization-range*)
                           (<= (- upper-bound lower-bound)
                               *maximum-discretization-range*)))
                  (set-enumerated-domain!
                   y (all-values (an-integer-between lower-bound upper-bound))))
              (if lower-bound
                  (if upper-bound
                      (if (some #'(lambda (element)
                                    (or (< element lower-bound)
                                        (> element upper-bound)))
                                (variable-enumerated-domain y))
                          ;; note: Could do less consing if had LOCAL DELETE-IF.
                          ;;       This would also allow checking list only once.
                          (set-enumerated-domain!
                           y (remove-if #'(lambda (element)
                                            (or (< element lower-bound)
                                                (> element upper-bound)))
                                        (variable-enumerated-domain y))))
                      (if (some #'(lambda (element) (< element lower-bound))
                                (variable-enumerated-domain y))
                          ;; note: Could do less consing if had LOCAL DELETE-IF.
                          ;;       This would also allow checking list only once.
                          (set-enumerated-domain!
                           y (remove-if #'(lambda (element)
                                            (< element lower-bound))
                                        (variable-enumerated-domain y)))))
                  (if upper-bound
                      (if (some #'(lambda (element) (> element upper-bound))
                                (variable-enumerated-domain y))
                          ;; note: Could do less consing if had LOCAL DELETE-IF.
                          ;;       This would also allow checking list only once.
                          (set-enumerated-domain!
                           y (remove-if #'(lambda (element)
                                            (> element upper-bound))
                                        (variable-enumerated-domain y)))))))))
    (local (let* ((enumerated-domain
                   (cond
                     ((eq (variable-enumerated-domain x) t)
                      (if (eq (variable-enumerated-domain y) t)
                          t
                          (set-difference (variable-enumerated-domain y)
                                          (variable-enumerated-antidomain x)
                                          :test #'equal)))
                     ((eq (variable-enumerated-domain y) t)
                      (set-difference (variable-enumerated-domain x)
                                      (variable-enumerated-antidomain y)
                                      :test #'equal))
                     (t (intersection (variable-enumerated-domain x)
                                      (variable-enumerated-domain y)
                                      :test #'equal))))
                  (enumerated-antidomain
                   (if (eq enumerated-domain t)
                       (union (variable-enumerated-antidomain x)
                              (variable-enumerated-antidomain y)
                              :test #'equal)
                       '())))
             (if (null enumerated-domain) (fail))
             (if (and (not (eq enumerated-domain t))
                      (or (eq (variable-enumerated-domain y) t)
                          (< (length enumerated-domain)
                             (length (variable-enumerated-domain y)))))
                 (setf (variable-enumerated-domain y) enumerated-domain))
             (if (if (eq enumerated-domain t)
                     (> (length enumerated-antidomain)
                        (length (variable-enumerated-antidomain y)))
                     (not (null (variable-enumerated-antidomain y))))
                 (setf (variable-enumerated-antidomain y) enumerated-antidomain)))
           (setf (variable-noticers y)
                 (append (variable-noticers y) (variable-noticers x)))
           (setf (variable-noticers x) '())
           (setf (variable-value x) y))
    (run-noticers y)))

(defun restrict-value! (x value)
  ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; note: VALUE must not be a variable.
  (if (occurs-in? x value) (fail))
  (typecase value
    (integer (unless (variable-possibly-integer? x) (fail)))
    (real (unless (variable-possibly-noninteger-real? x) (fail)))
    (number (unless (variable-possibly-nonreal-number? x) (fail)))
    (boolean (unless (variable-possibly-boolean? x) (fail)))
    (otherwise (unless (variable-possibly-nonboolean-nonnumber? x) (fail))))
  ;; needs work: This is sound only if VALUE does not contain any variables.
  (if (eq (variable-enumerated-domain x) t)
      (if (member value (variable-enumerated-antidomain x) :test #'equal)
          (fail))
      (unless (member value (variable-enumerated-domain x) :test #'equal)
        (fail)))
  (if (and (realp value)
           (or (and (variable-lower-bound x)
                    (< value (variable-lower-bound x)))
               (and (variable-upper-bound x)
                    (> value (variable-upper-bound x)))))
      (fail))
  (local (setf (variable-value x) value)
         (typecase value
           (integer (if (variable-possibly-noninteger-real? x)
                        (setf (variable-possibly-noninteger-real? x) nil))
                    (if (variable-possibly-nonreal-number? x)
                        (setf (variable-possibly-nonreal-number? x) nil))
                    (if (variable-possibly-boolean? x)
                        (setf (variable-possibly-boolean? x) nil))
                    (if (variable-possibly-nonboolean-nonnumber? x)
                        (setf (variable-possibly-nonboolean-nonnumber? x) nil))
                    (if (or (null (variable-lower-bound x))
                            (not (integerp (variable-lower-bound x)))
                            (> value (variable-lower-bound x)))
                        (setf (variable-lower-bound x) value))
                    (if (or (null (variable-upper-bound x))
                            (not (integerp (variable-upper-bound x)))
                            (< value (variable-upper-bound x)))
                        (setf (variable-upper-bound x) value)))
           (real (if (variable-possibly-integer? x)
                     (setf (variable-possibly-integer? x) nil))
                 (if (variable-possibly-nonreal-number? x)
                     (setf (variable-possibly-nonreal-number? x) nil))
                 (if (variable-possibly-boolean? x)
                     (setf (variable-possibly-boolean? x) nil))
                 (if (variable-possibly-nonboolean-nonnumber? x)
                     (setf (variable-possibly-nonboolean-nonnumber? x) nil))
                 (if (or (null (variable-lower-bound x))
                         (> value (variable-lower-bound x)))
                     (setf (variable-lower-bound x) value))
                 (if (or (null (variable-upper-bound x))
                         (< value (variable-upper-bound x)))
                     (setf (variable-upper-bound x) value)))
           (number (if (variable-possibly-integer? x)
                       (setf (variable-possibly-integer? x) nil))
                   (if (variable-possibly-noninteger-real? x)
                       (setf (variable-possibly-noninteger-real? x) nil))
                   (if (variable-possibly-boolean? x)
                       (setf (variable-possibly-boolean? x) nil))
                   (if (variable-possibly-nonboolean-nonnumber? x)
                       (setf (variable-possibly-nonboolean-nonnumber? x) nil)))
           (boolean (if (variable-possibly-integer? x)
                        (setf (variable-possibly-integer? x) nil))
                    (if (variable-possibly-noninteger-real? x)
                        (setf (variable-possibly-noninteger-real? x) nil))
                    (if (variable-possibly-nonreal-number? x)
                        (setf (variable-possibly-nonreal-number? x) nil))
                    (if (variable-possibly-nonboolean-nonnumber? x)
                        (setf (variable-possibly-nonboolean-nonnumber? x) nil)))
           (otherwise (if (variable-possibly-integer? x)
                          (setf (variable-possibly-integer? x) nil))
                      (if (variable-possibly-noninteger-real? x)
                          (setf (variable-possibly-noninteger-real? x) nil))
                      (if (variable-possibly-nonreal-number? x)
                          (setf (variable-possibly-nonreal-number? x) nil))
                      (if (variable-possibly-boolean? x)
                          (setf (variable-possibly-boolean? x) nil))))
         (cond ((eq (variable-enumerated-domain x) t)
                ;; needs work: This is sound only if VALUE does not contain any
                ;;             variables.
                (setf (variable-enumerated-domain x) (list value))
                (setf (variable-enumerated-antidomain x) '()))
               ((not (null (rest (variable-enumerated-domain x))))
                ;; needs work: This is sound only if VALUE does not contain any
                ;;             variables.
                (setf (variable-enumerated-domain x) (list value)))))
  (run-noticers x))

(defun restrict-true! (x)
  ;; note: X must be a Boolean variable.
  (if (eq (variable-value x) nil) (fail))
  (when (eq (variable-value x) x)
    (local (setf (variable-value x) t)
           (setf (variable-enumerated-domain x) '(t)))
    (run-noticers x)))

(defun restrict-false! (x)
  ;; note: X must be a Boolean variable.
  (if (eq (variable-value x) t) (fail))
  (when (eq (variable-value x) x)
    (local (setf (variable-value x) nil)
           (setf (variable-enumerated-domain x) '(nil)))
    (run-noticers x)))

(defun set-enumerated-domain! (x enumerated-domain)
  ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; note: All callers must insure that the new ENUMERATED-DOMAIN is a subset
  ;;       of the old one.
  (if (null enumerated-domain) (fail))
  (local
    (cond
      ((eq (variable-enumerated-domain x) t)
       (setf (variable-enumerated-domain x) enumerated-domain)
       (unless (null (variable-enumerated-antidomain x))
         (setf (variable-enumerated-antidomain x) '()))
       (if (and (variable-possibly-boolean? x)
                (not (some #'booleanp enumerated-domain)))
           (setf (variable-possibly-boolean? x) nil))
       (if (and (variable-possibly-nonboolean-nonnumber? x)
                (not (some #'(lambda (x)
                               (and (not (booleanp x)) (not (numberp x))))
                           enumerated-domain)))
           (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (if (and (variable-possibly-nonreal-number? x)
                (not (some #'(lambda (x) (and (not (realp x)) (numberp x)))
                           enumerated-domain)))
           (setf (variable-possibly-nonreal-number? x) nil))
       (if (and (variable-possibly-noninteger-real? x)
                (not (some #'(lambda (x) (and (not (integerp x)) (realp x)))
                           enumerated-domain)))
           (setf (variable-possibly-noninteger-real? x) nil))
       (if (and (variable-possibly-integer? x)
                (not (some #'integerp enumerated-domain)))
           (setf (variable-possibly-integer? x) nil))
       (if (variable-real? x)
           (let ((lower-bound (reduce #'min enumerated-domain))
                 (upper-bound (reduce #'max enumerated-domain)))
             (if (or (null (variable-lower-bound x))
                     (> lower-bound (variable-lower-bound x)))
                 (setf (variable-lower-bound x) lower-bound))
             (if (or (null (variable-upper-bound x))
                     (< upper-bound (variable-upper-bound x)))
                 (setf (variable-upper-bound x) upper-bound))))
       (if (null (rest enumerated-domain))
           (setf (variable-value x) (first enumerated-domain)))
       t)
      ((< (length enumerated-domain) (length (variable-enumerated-domain x)))
       (setf (variable-enumerated-domain x) enumerated-domain)
       (if (and (variable-possibly-boolean? x)
                (not (some #'booleanp enumerated-domain)))
           (setf (variable-possibly-boolean? x) nil))
       (if (and (variable-possibly-nonboolean-nonnumber? x)
                (not (some #'(lambda (x)
                               (and (not (booleanp x)) (not (numberp x))))
                           enumerated-domain)))
           (setf (variable-possibly-nonboolean-nonnumber? x) nil))
       (if (and (variable-possibly-nonreal-number? x)
                (not (some #'(lambda (x) (and (not (realp x)) (numberp x)))
                           enumerated-domain)))
           (setf (variable-possibly-nonreal-number? x) nil))
       (if (and (variable-possibly-noninteger-real? x)
                (not (some #'(lambda (x) (and (not (integerp x)) (realp x)))
                           enumerated-domain)))
           (setf (variable-possibly-noninteger-real? x) nil))
       (if (and (variable-possibly-integer? x)
                (not (some #'integerp enumerated-domain)))
           (setf (variable-possibly-integer? x) nil))
       (if (variable-real? x)
           (let ((lower-bound (reduce #'min enumerated-domain))
                 (upper-bound (reduce #'max enumerated-domain)))
             (if (or (null (variable-lower-bound x))
                     (> lower-bound (variable-lower-bound x)))
                 (setf (variable-lower-bound x) lower-bound))
             (if (or (null (variable-upper-bound x))
                     (< upper-bound (variable-upper-bound x)))
                 (setf (variable-upper-bound x) upper-bound))))
       (if (null (rest enumerated-domain))
           (setf (variable-value x) (first enumerated-domain)))
       t)
      (t nil))))

(defun restrict-enumerated-domain! (x enumerated-domain)
  ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; note: ENUMERATED-DOMAIN must not be a variable.
  (unless (typep enumerated-domain 'sequence) (fail))
  (when (every #'ground? enumerated-domain)
    (setf enumerated-domain
          (remove-duplicates (map 'list #'eliminate-variables enumerated-domain)
                             :test #'equal))
    (unless (variable-possibly-boolean? x)
      (setf enumerated-domain (remove-if #'booleanp enumerated-domain)))
    (unless (variable-possibly-nonboolean-nonnumber? x)
      (setf enumerated-domain
            (remove-if #'(lambda (x) (and (not (booleanp x)) (not (numberp x))))
                       enumerated-domain)))
    (unless (variable-possibly-nonreal-number? x)
      (setf enumerated-domain
            (remove-if #'(lambda (x) (and (not (realp x)) (numberp x)))
                       enumerated-domain)))
    (unless (variable-possibly-noninteger-real? x)
      (setf enumerated-domain
            (remove-if #'(lambda (x) (and (not (integerp x)) (realp x)))
                       enumerated-domain)))
    (unless (variable-possibly-integer? x)
      (setf enumerated-domain (remove-if #'integerp enumerated-domain)))
    (if (variable-upper-bound x)
        (let ((upper-bound (variable-upper-bound x)))
          (setf enumerated-domain
                (remove-if #'(lambda (element) (> element upper-bound))
                           enumerated-domain))))
    (if (variable-lower-bound x)
        (let ((lower-bound (variable-lower-bound x)))
          (setf enumerated-domain
                (remove-if #'(lambda (element) (< element lower-bound))
                           enumerated-domain))))
    (setf enumerated-domain
          (if (eq (variable-enumerated-domain x) t)
              (set-difference enumerated-domain
                              (variable-enumerated-antidomain x)
                              :test #'equal)
              (intersection (variable-enumerated-domain x) enumerated-domain
                            :test #'equal)))
    (if (set-enumerated-domain! x enumerated-domain) (run-noticers x))))

(defun restrict-enumerated-antidomain! (x enumerated-antidomain)
  ;; note: X must be a variable such that (EQ X (VALUE-OF X)).
  ;; note: ENUMERATED-ANTIDOMAIN must not be a variable.
  (unless (typep enumerated-antidomain 'sequence) (fail))
  (when (every #'ground? enumerated-antidomain)
    (setf enumerated-antidomain
          (remove-duplicates
           (map 'list #'eliminate-variables enumerated-antidomain)
           :test #'equal))
    (cond
      ((eq (variable-enumerated-domain x) t)
       (setf enumerated-antidomain
             (union (variable-enumerated-antidomain x) enumerated-antidomain
                    :test #'equal))
       (when (> (length enumerated-antidomain)
                (length (variable-enumerated-antidomain x)))
         (local (setf (variable-enumerated-antidomain x) enumerated-antidomain))
         (run-noticers x)))
      ((set-enumerated-domain!
        x (set-difference (variable-enumerated-domain x) enumerated-antidomain
                          :test #'equal))
       (run-noticers x)))))

;;; Rules

(defun +-rule-up (z x y)
  (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  ;; note: We can't assert that Z in not an integer when either X or Y are not
  ;;       integers since they may be Gaussian integers. But we can if either
  ;;       X or Y is real. If the Screamer type system could distinguish
  ;;       Gaussian integers from other complex numbers we could whenever X or
  ;;       Y was not a Gaussian integer.
  (if (and (or (variable-noninteger? x) (variable-noninteger? y))
           (or (variable-real? x) (variable-real? y)))
      (restrict-noninteger! z))
  (if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
  ;; note: Ditto.
  (if (and (or (variable-nonreal? x) (variable-nonreal? y))
           (or (variable-real? x) (variable-real? y)))
      (restrict-nonreal! z))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      (restrict-bounds!
       z
       (infinity-+ (variable-lower-bound x) (variable-lower-bound y))
       (infinity-+ (variable-upper-bound x) (variable-upper-bound y))))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (+ x y)))
        (fail))))

(defun +-rule-down (z x y)
  ;; note: We can't assert that X and Y are integers when Z is an integer since
  ;;       Z may be an integer when X and Y are Gaussian integers. But we can
  ;;       make such an assertion if either X or Y is real. If the Screamer
  ;;       type system could distinguish Gaussian integers from other complex
  ;;       numbers we could make such an assertion whenever either X or Y was
  ;;       not a Gaussian integer.
  (if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)))
      (restrict-integer! x))
  ;; note: Ditto.
  (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
      (restrict-real! x))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      (restrict-bounds!
       x
       (infinity-- (variable-lower-bound z) (variable-upper-bound y))
       (infinity-- (variable-upper-bound z) (variable-lower-bound y))))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (+ x y)))
        (fail))))

(defun /-rule (z x y)
  (when (and (variable-lower-bound x) (plusp (variable-lower-bound x)))
    (cond ((and (variable-upper-bound x) (not (zerop (variable-upper-bound x))))
           (if (variable-lower-bound z)
               (cond
                 ((minusp (variable-lower-bound z))
                  (restrict-lower-bound!
                   y (/ (variable-lower-bound z) (variable-lower-bound x))))
                 (t (restrict-lower-bound! y 0)
                    (restrict-lower-bound!
                     y (/ (variable-lower-bound z) (variable-upper-bound x))))))
           (if (variable-upper-bound z)
               (cond
                 ((plusp (variable-upper-bound z))
                  (restrict-upper-bound!
                   y (/ (variable-upper-bound z) (variable-lower-bound x))))
                 (t (restrict-upper-bound! y 0)
                    (restrict-upper-bound!
                     y (/ (variable-upper-bound z) (variable-upper-bound x)))))))
          (t (if (variable-lower-bound z)
                 (cond
                   ((minusp (variable-lower-bound z))
                    (restrict-lower-bound!
                     y (/ (variable-lower-bound z) (variable-lower-bound x))))
                   (t (restrict-lower-bound! y 0))))
             (if (variable-upper-bound z)
                 (cond
                   ((plusp (variable-upper-bound z))
                    (restrict-upper-bound!
                     y (/ (variable-upper-bound z) (variable-lower-bound x))))
                   (t (restrict-upper-bound! y 0)))))))
  (when (and (variable-upper-bound x) (minusp (variable-upper-bound x)))
    (cond ((and (variable-lower-bound x) (not (zerop (variable-lower-bound x))))
           (if (variable-upper-bound z)
               (cond
                 ((plusp (variable-upper-bound z))
                  (restrict-lower-bound!
                   y (/ (variable-upper-bound z) (variable-upper-bound x))))
                 (t (restrict-lower-bound! y 0)
                    (restrict-lower-bound!
                     y (/ (variable-upper-bound z) (variable-lower-bound x))))))
           (if (variable-lower-bound z)
               (cond
                 ((minusp (variable-lower-bound z))
                  (restrict-upper-bound!
                   y (/ (variable-lower-bound z) (variable-upper-bound x))))
                 (t (restrict-upper-bound! y 0)
                    (restrict-upper-bound!
                     y (/ (variable-lower-bound z) (variable-lower-bound x)))))))
          (t (if (variable-upper-bound z)
                 (cond
                   ((plusp (variable-upper-bound z))
                    (restrict-lower-bound!
                     y (/ (variable-upper-bound z) (variable-upper-bound x))))
                   (t (restrict-lower-bound! y 0))))
             (if (variable-lower-bound z)
                 (cond
                   ((minusp (variable-lower-bound z))
                    (restrict-upper-bound!
                     y (/ (variable-lower-bound z) (variable-upper-bound x))))
                   (t (restrict-upper-bound! y 0))))))))

(defun *-rule-up (z x y)
  (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  ;; note: We can't assert that Z in not an integer when either X or Y are not
  ;;       integers since they may be Gaussian integers. But we can if either
  ;;       X or Y is real. If the Screamer type system could distinguish
  ;;       Gaussian integers from other complex numbers we could whenever X or
  ;;       Y was not a Gaussian integer.
  (if (and (or (variable-noninteger? x) (variable-noninteger? y))
           (or (variable-real? x) (variable-real? y)))
      (restrict-noninteger! z))
  (if (and (variable-real? x) (variable-real? y)) (restrict-real! z))
  ;; note: Ditto.
  (if (and (or (variable-nonreal? x) (variable-nonreal? y))
           (or (variable-real? x) (variable-real? y)))
      (restrict-nonreal! z))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      ;; note: Can sometimes do better than the following even when ranges are
      ;;       not finite.
      (restrict-bounds!
       z
       (infinity-min
        (infinity-* (variable-lower-bound x) (variable-lower-bound y))
        (infinity-min
         (infinity-* (variable-lower-bound x) (variable-upper-bound y))
         (infinity-min
          (infinity-* (variable-upper-bound x) (variable-lower-bound y))
          (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))
       (infinity-max
        (infinity-* (variable-lower-bound x) (variable-lower-bound y))
        (infinity-max
         (infinity-* (variable-lower-bound x) (variable-upper-bound y))
         (infinity-max
          (infinity-* (variable-upper-bound x) (variable-lower-bound y))
          (infinity-* (variable-upper-bound x) (variable-upper-bound y)))))))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (* x y)))
        (fail))))

(defun *-rule-down (z x y)
  ;; note: We can't assert that X and Y are integers when Z is an integer since
  ;;       Z may be an integer when X and Y are Gaussian integers. But we can
  ;;       make such an assertion if either X or Y is real. If the Screamer
  ;;       type system could distinguish Gaussian integers from other complex
  ;;       numbers we could make such an assertion whenever either X or Y was
  ;;       not a Gaussian integer.
  (if (and (variable-integer? z) (or (variable-real? x) (variable-real? y)))
      (restrict-integer! x))
  ;; note: Ditto.
  (if (and (variable-real? z) (or (variable-real? x) (variable-real? y)))
      (restrict-real! x))
  (if (and (variable-real? x) (variable-real? y) (variable-real? z))
      (/-rule z y x))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? x))
             (not (variable? y))
             (not (variable? z))
             (/= z (* x y)))
        (fail))))

(defun min-rule-up (z x y)
  (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  (restrict-bounds!
   z
   (infinity-min (variable-lower-bound x) (variable-lower-bound y))
   (if (variable-upper-bound x)
       (if (variable-upper-bound y)
           (min (variable-upper-bound x) (variable-upper-bound y))
           (variable-upper-bound x))
       (variable-upper-bound y)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (min x y)))
        (fail))))

(defun min-rule-down (z x y)
  ;; note: The analog of the following for upper bounds, namely restricting
  ;;       the upper bound of either X or Y to (VARIABLE-UPPER-BOUND Z) is
  ;;       nondeterministic.
  (if (variable-lower-bound z)
      (restrict-lower-bound! x (variable-lower-bound z)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (min x y)))
        (fail))))

(defun max-rule-up (z x y)
  (if (and (variable-integer? x) (variable-integer? y)) (restrict-integer! z))
  (restrict-bounds!
   z
   (if (variable-lower-bound x)
       (if (variable-lower-bound y)
           (max (variable-lower-bound x) (variable-lower-bound y))
           (variable-lower-bound x))
       (variable-lower-bound y))
   (infinity-max (variable-upper-bound x) (variable-upper-bound y)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (max x y)))
        (fail))))

(defun max-rule-down (z x y)
  ;; note: The analog of the following for lower bounds, namely restricting
  ;;       the lower bound of either X or Y to (VARIABLE-LOWER-BOUND Z) is
  ;;       nondeterministic.
  (if (variable-upper-bound z)
      (restrict-upper-bound! x (variable-upper-bound z)))
  (let ((x (value-of x))
        (y (value-of y))
        (z (value-of z)))
    (if (and (not (variable? z))
             (not (variable? x))
             (not (variable? y))
             (/= z (max x y)))
        (fail))))

(defun =-rule (x y)
  (cond
    ;; note: I forget why +-RULE *-RULE MIN-RULE and MAX-RULE must perform the
    ;;       check in the second COND clause irrespective of whether the first
    ;;       clause is executed.
    ((and (variable-real? x) (variable-real? y))
     (restrict-bounds! x (variable-lower-bound y) (variable-upper-bound y))
     (restrict-bounds! y (variable-lower-bound x) (variable-upper-bound x)))
    ((and (not (variable? x)) (not (variable? y)) (/= x y)) (fail))))

(defun <=-rule (x y)
  (if (variable-lower-bound x)
      (restrict-lower-bound! y (variable-lower-bound x)))
  (if (variable-upper-bound y)
      (restrict-upper-bound! x (variable-upper-bound y))))

(defun <-rule (x y)
  (if (variable-lower-bound x)
      (restrict-lower-bound! y (if (variable-integer? y)
                                   (1+ (floor (variable-lower-bound x)))
                                   (variable-lower-bound x))))
  (if (variable-upper-bound y)
      (restrict-upper-bound! x (if (variable-integer? x)
                                   (1- (ceiling (variable-upper-bound y)))
                                   (variable-upper-bound y))))
  (let ((x (value-of x))
        (y (value-of y)))
    (if (and (not (variable? x)) (not (variable? y)) (>= x y)) (fail))))

(defun /=-rule (x y)
  ;; note: Got rid of the nondeterministic version of /=-RULE.
  (let ((x (value-of x))
        (y (value-of y)))
    (if (and (not (variable? x)) (not (variable? y)) (= x y)) (fail))))

;;; Lifted Arithmetic Functions (Two argument optimized)

(defun +v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first two optimizations below violate Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? x) (zerop (value-of x))) (value-of y))
        ((and (bound? y) (zerop (value-of y))) (value-of x))
        ((and (bound? x) (bound? y)) (+ (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (+-rule-up z x y) (+-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (+-rule-up z x y) (+-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (+-rule-down z x y) (+-rule-down z y x)) z)
             z))))

(defun -v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first optimization below violates Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? y) (zerop (value-of y))) (value-of x))
        ((and (bound? x) (bound? y)) (- (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (+-rule-down x y z) (+-rule-down x z y)) x)
             (attach-noticer!
              #'(lambda () (+-rule-up x y z) (+-rule-down x z y)) y)
             (attach-noticer!
              #'(lambda () (+-rule-up x y z) (+-rule-down x y z)) z)
             z))))

(defun *v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first four optimizations below violate Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? x) (zerop (value-of x))) 0)
        ((and (bound? y) (zerop (value-of y))) 0)
        ((and (bound? x) (= (value-of x) 1)) (value-of y))
        ((and (bound? y) (= (value-of y) 1)) (value-of x))
        ((and (bound? x) (bound? y)) (* (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (*-rule-up z x y) (*-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (*-rule-up z x y) (*-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (*-rule-down z x y) (*-rule-down z y x)) z)
             z))))

(defun /v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  ;; needs work: The first three optimizations below violate Common Lisp type
  ;;             propagation conventions.
  (cond ((and (bound? x) (zerop (value-of x))) 0)
        ((and (bound? y) (zerop (value-of y))) (fail))
        ((and (bound? y) (= (value-of y) 1)) (value-of x))
        ((and (bound? x) (bound? y)) (/ (value-of x) (value-of y)))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-numberv)))
             (attach-noticer!
              #'(lambda () (*-rule-down x y z) (*-rule-down x z y)) x)
             (attach-noticer!
              #'(lambda () (*-rule-up x y z) (*-rule-down x z y)) y)
             (attach-noticer!
              #'(lambda () (*-rule-up x y z) (*-rule-down x y z)) z)
             z))))

(defun minv2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<=v2-internal x y) (value-of x))
        ((known?-<=v2-internal y x) (value-of y))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-realv)))
             (attach-noticer!
              #'(lambda () (min-rule-up z x y) (min-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (min-rule-up z x y) (min-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (min-rule-down z x y) (min-rule-down z y x)) z)
             z))))

(defun maxv2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<=v2-internal y x) (value-of x))
        ((known?-<=v2-internal x y) (value-of y))
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-realv)))
             (attach-noticer!
              #'(lambda () (max-rule-up z x y) (max-rule-down z y x)) x)
             (attach-noticer!
              #'(lambda () (max-rule-up z x y) (max-rule-down z x y)) y)
             (attach-noticer!
              #'(lambda () (max-rule-down z x y) (max-rule-down z y x)) z)
             z))))

;;; Lifted Type Functions (KNOWN? optimized)

(defun known?-integerpv (x)
  (let ((x (value-of x)))
    (typecase x
      (integer t)
      (variable (variable-integer? x))
      (otherwise nil))))

(defun known?-notv-integerpv (x)
  (let ((x (value-of x)))
    (typecase x
      (integer nil)
      (variable (variable-noninteger? x))
      (otherwise t))))

(defun known?-realpv (x)
  (let ((x (value-of x)))
    (typecase x
      (real t)
      (variable (variable-real? x))
      (otherwise nil))))

(defun known?-notv-realpv (x)
  (let ((x (value-of x)))
    (typecase x
      (real nil)
      (variable (variable-nonreal? x))
      (otherwise t))))

(defun known?-numberpv (x)
  (let ((x (value-of x)))
    (typecase x
      (number t)
      (variable (variable-number? x))
      (otherwise nil))))

(defun known?-notv-numberpv (x)
  (let ((x (value-of x)))
    (typecase x
      (number nil)
      (variable (variable-nonnumber? x))
      (otherwise t))))

(defun known?-booleanpv (x)
  (let ((x (value-of x)))
    (typecase x
      (boolean t)
      (variable (variable-boolean? x))
      (otherwise nil))))

(defun known?-notv-booleanpv (x)
  (let ((x (value-of x)))
    (typecase x
      (boolean nil)
      (variable (variable-nonboolean? x))
      (otherwise t))))

;;; Lifted Arithmetic Comparison Functions (Two argument KNOWN? optimized)

(defun known?-<=v2-variable (x y)
  (and (variable-upper-bound x)
       (variable-lower-bound y)
       (<= (variable-upper-bound x) (variable-lower-bound y))))

(defun known?-<v2-variable (x y)
  (and (variable-upper-bound x)
       (variable-lower-bound y)
       (< (variable-upper-bound x) (variable-lower-bound y))))

(defun known?-=v2-variable (x y)
  (or (and (variable-real? x)
           (variable-real? y)
           (known?-<=v2-variable x y)
           (known?-<=v2-variable y x))
      (and (not (eq x (variable-value x)))
           (not (eq y (variable-value y)))
           (= (variable-value x) (variable-value y)))))

(defun known?-/=v2-variable (x y)
  (or (and (variable-real? x)
           (variable-real? y)
           (or (known?-<v2-variable x y) (known?-<v2-variable y x)))
      (and (not (eq x (variable-value x)))
           (not (eq y (variable-value y)))
           (/= (variable-value x) (variable-value y)))))

(defun known?-=v2-internal (x y)
  (known?-=v2-variable (variablize x) (variablize y)))

(defun known?-<=v2-internal (x y)
  (known?-<=v2-variable (variablize x) (variablize y)))

(defun known?-<v2-internal (x y)
  (known?-<v2-variable (variablize x) (variablize y)))

(defun known?-/=v2-internal (x y)
  (known?-/=v2-variable (variablize x) (variablize y)))

(defun known?-=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (known?-=v2-internal x y))

(defun known?-<=v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (known?-<=v2-internal x y))

(defun known?-<v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (known?-<v2-internal x y))

(defun known?-/=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (known?-/=v2-internal x y))

;;; Lifted Type Functions (ASSERT! optimized)

(defun assert!-integerpv (x)
  (let ((x (value-of x)))
    (typecase x
      (integer)
      (variable (restrict-integer! x))
      (otherwise (fail)))))

(defun assert!-notv-integerpv (x)
  (let ((x (value-of x)))
    (typecase x
      (integer (fail))
      (variable (restrict-noninteger! x))
      (otherwise))))

(defun assert!-realpv (x)
  (let ((x (value-of x)))
    (typecase x
      (real)
      (variable (restrict-real! x))
      (otherwise (fail)))))

(defun assert!-notv-realpv (x)
  (let ((x (value-of x)))
    (typecase x
      (real (fail))
      (variable (restrict-nonreal! x))
      (otherwise))))

(defun assert!-numberpv (x)
  (let ((x (value-of x)))
    (typecase x
      (number)
      (variable (restrict-number! x))
      (otherwise (fail)))))

(defun assert!-notv-numberpv (x)
  (let ((x (value-of x)))
    (typecase x
      (number (fail))
      (variable (restrict-nonnumber! x))
      (otherwise))))

(defun assert!-booleanpv (x)
  (let ((x (value-of x)))
    (typecase x
      (boolean)
      (variable (restrict-boolean! x))
      (otherwise (fail)))))

(defun assert!-notv-booleanpv (x)
  (let ((x (value-of x)))
    (typecase x
      (boolean (fail))
      (variable (restrict-nonboolean! x))
      (otherwise))))

;;; Lifted Arithmetic Comparison Functions (Two argument ASSERT! optimized)

(defun assert!-=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    (attach-noticer! #'(lambda () (=-rule x y)) x)
    (attach-noticer! #'(lambda () (=-rule x y)) y)))

(defun assert!-<=v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    (attach-noticer! #'(lambda () (<=-rule x y)) x)
    (attach-noticer! #'(lambda () (<=-rule x y)) y)))

(defun assert!-<v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    (attach-noticer! #'(lambda () (<-rule x y)) x)
    (attach-noticer! #'(lambda () (<-rule x y)) y)))

(defun assert!-/=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (let ((x (variablize x))
        (y (variablize y)))
    ;; note: Got rid of the nondeterministic version that called the
    ;;       nondeterministic version of /=-RULE.
    (attach-noticer! #'(lambda () (/=-rule x y)) x)
    (attach-noticer! #'(lambda () (/=-rule x y)) y)))

;;; Lifted Type Functions

(defun integerpv (x)
  "Returns T if X is known to be integer valued, and NIL if X is known be
non-integer value.

If it is not known whether or not X is integer valued when INTEGERPV is called
then INTEGERPV creates and returns a new boolean variable V.

The values of X and V are mutually constrained via noticers so that V is equal
to T if and only if X is known to be integer valued, and V is equal to NIL if
and only if X is known to be non-integer valued.

If X later becomes known to be integer valued, a noticer attached to X
restricts V to equal T. Likewise, if X later becomes known to be non-integer
valued, a noticer attached to X restricts V to equal NIL.

Furthermore, if V ever becomes known to equal T then a noticer attached to V
restricts X to be integer valued. Likewise, if V ever becomes known to equal
NIL then a noticer attached to V restricts X to be non-integer valued."
  (cond ((known?-integerpv x) t)
        ((known?-notv-integerpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-integer? x) (restrict-true! z))
                        ((variable-noninteger? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-integer! x))
                        ((variable-false? z) (restrict-noninteger! x))))
              z)
             z))))

(defun realpv (x)
  "Returns T if X is known to be real, NIL if X is known to be non-real,
and otherwise returns a new boolean variable V.

The values of X and V are mutually constrained via noticers so that V is equal
to T if and only if X is known to be real and V is equal to NIL if and only if
X is known to be non-real.

* If X later becomes known to be real, a noticer attached to X restricts V to
  equal T. Likewise, if X later becomes known to be non-real, a noticer
  attached to X restricts V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X
  to be real. Likewise, if V ever becomes known to equal NIL then a noticer
  attached to V restricts X to be non-real."
  (cond ((known?-realpv x) t)
        ((known?-notv-realpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-real? x) (restrict-true! z))
                        ((variable-nonreal? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-real! x))
                        ((variable-false? z) (restrict-nonreal! x))))
              z)
             z))))

(defun numberpv (x)
  "Returns T if X is known to be numeric, NIL if X is known to be
non-numeric, and otherwise returns a new boolean variable V.

The values of X and V are mutually constrained via noticers so that V is equal
to T if and only if X is known to be numeric and V is equal to NIL if and only
if X is known to be non-numeric.

* If X later becomes known to be numeric, a noticer attached to X restricts V
  to equal T. Likewise, if X later becomes known to be non-numeric, a noticer
  attached to X restricts V to equal NIL.

* If V ever becomes known
  to equal T then a noticer attached to V restricts X to be numeric. Likewise,
  if V ever becomes known to equal NIL then a noticer attached to V restricts X
  to be non-numeric."
  (cond ((known?-numberpv x) t)
        ((known?-notv-numberpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-number? x) (restrict-true! z))
                        ((variable-nonnumber? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-number! x))
                        ((variable-false? z) (restrict-nonnumber! x))))
              z)
             z))))

(defun booleanpv (x)
  "The expression \(BOOLEANPV X) is an abbreviation for \(MEMBERV X '\(T NIL))."
  (cond ((known?-booleanpv x) t)
        ((known?-notv-booleanpv x) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-boolean? x) (restrict-true! z))
                        ((variable-nonboolean? x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (restrict-boolean! x))
                        ((variable-false? z) (restrict-nonboolean! x))))
              z)
             z))))

;;; Lifted MEMBERV

(defun known?-memberv-list-internal (x y)
  (and (consp y)
       (or (known?-equalv x (first y))
           (known?-memberv-list-internal x (rest y)))))

(defun known?-memberv-list (x y)
  (typecase y
    (cons (or (known?-equalv x (first y)) (known?-memberv-list x (rest y))))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every
               #'(lambda (element) (known?-memberv-list-internal x element))
               (variable-enumerated-domain y)))
         (known?-memberv-list x (variable-value y))))
    (otherwise nil)))

(defun known?-memberv-internal (x y)
  (typecase y
    (list (known?-memberv-list x y))
    (vector (some #'(lambda (element) (known?-equalv x element)) y))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every
               #'(lambda (element)
                   (typecase element
                     (list (known?-memberv-list-internal x element))
                     (vector (some #'(lambda (e) (known?-equalv x e)) element))
                     (otherwise nil)))
               (variable-enumerated-domain y)))
         (known?-memberv-internal x (variable-value y))))
    (otherwise (fail))))

(defun known?-memberv (x y)
  (cond ((and (variable? x) (not (eq (variable-value x) x)))
         (known?-memberv (variable-value x) y))
        ((and (variable? x) (not (eq (variable-enumerated-domain x) t)))
         ;; note: This first alternative is an optimization in case membership
         ;;       can be determined simply through sharing relationships.
         (or (known?-memberv-internal x y)
             (every #'(lambda (element) (known?-memberv-internal element y))
                    (variable-enumerated-domain x))))
        (t (known?-memberv-internal x y))))

(defun known?-notv-memberv-list-internal (x y)
  (or (not (consp y))
      (and (known?-notv-equalv x (first y))
           (known?-notv-memberv-list-internal x (rest y)))))

(defun known?-notv-memberv-list (x y)
  (typecase y
    (cons (and (known?-notv-equalv x (first y))
               (known?-notv-memberv-list x (rest y))))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every #'(lambda (element)
                         (known?-notv-memberv-list-internal x element))
                     (variable-enumerated-domain y)))
         (known?-notv-memberv-list x (variable-value y))))
    (otherwise t)))

(defun known?-notv-memberv-internal (x y)
  (typecase y
    (list (known?-notv-memberv-list x y))
    (vector (every #'(lambda (element) (known?-notv-equalv x element)) y))
    (variable
     (if (eq (variable-value y) y)
         (and (not (eq (variable-enumerated-domain y) t))
              (every
               #'(lambda (element)
                   (typecase element
                     (list (known?-notv-memberv-list-internal x element))
                     (vector
                      (every #'(lambda (e) (known?-notv-equalv x e)) element))
                     (otherwise nil)))
               (variable-enumerated-domain y)))
         (known?-notv-memberv-internal x (variable-value y))))
    (otherwise (fail))))

(defun known?-notv-memberv (x y)
  (cond
    ((and (variable? x) (not (eq (variable-value x) x)))
     (known?-notv-memberv (variable-value x) y))
    ((and (variable? x) (not (eq (variable-enumerated-domain x) t)))
     ;; note: This first alternative is an optimization in case membership
     ;;       can be determined simply through sharing relationships.
     (or (known?-notv-memberv-internal x y)
         (every #'(lambda (element) (known?-notv-memberv-internal element y))
                (variable-enumerated-domain x))))
    (t (known?-notv-memberv-internal x y))))

(defun assert!-memberv-internal (x y)
  (let ((x (value-of x)))
    (if (known?-notv-memberv x y) (fail))
    (if (variable? x)
        (let ((y (value-of y)))
          (unless (variable? y) (restrict-enumerated-domain! x y))))))

(defun assert!-memberv (x y)
  (let ((y (value-of y)))
    (if (vectorp y)
        (dotimes (i (length y))
          (attach-noticer! #'(lambda () (assert!-memberv-internal x y))
                           (aref y i)))
        (attach-noticer! #'(lambda () (assert!-memberv-internal x y)) y))))

(defun assert!-notv-memberv-internal (x y)
  (let ((x (value-of x)))
    (if (known?-memberv x y) (fail))
    (if (variable? x)
        (let ((y (value-of y)))
          (unless (variable? y) (restrict-enumerated-antidomain! x y))))))

(defun assert!-notv-memberv (x y)
  (let ((y (value-of y)))
    (if (vectorp y)
        (dotimes (i (length y))
          (attach-noticer! #'(lambda () (assert!-notv-memberv-internal x y))
                           (aref y i)))
        (attach-noticer! #'(lambda () (assert!-notv-memberv-internal x y)) y))))

(defun memberv (x sequence)
  "Returns T if X is known to be a member of SEQUENCE \(using the Common Lisp
function EQL as a test function), NIL if X is known not to be a member of
SEQUENCE, and otherwise returns a new boolean variable V.

When a new variable is created, the values of X and V are mutually constrained
via noticers so that V is equal to T if and only if X is known to be a member
of SEQUENCE and V is equal to NIL if and only if X is known not to be a member
of SEQUENCE.

* If X later becomes known to be a member of SEQUENCE, a noticer attached to X
  restricts v to equal T. Likewise, if X later becomes known not to be a
  member of SEQUENCE, a noticer attached to X restricts V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X
  to be a member of SEQUENCE. Likewise, if V ever becomes known to equal NIL
  then a noticer attached to V restricts X not to be a member of SEQUENCE.

The current implementation imposes two constraints on the parameter SEQUENCE.
First, SEQUENCE must be bound when MEMBERV is called. Second, SEQUENCE must
not contain any unbound variables when MEMBERV is called.

The value of parameter SEQUENCE must be a sequence, i.e. either a list or a
vector."
  (cond ((known?-memberv x sequence) t)
        ((known?-notv-memberv x sequence) nil)
        (t (let ((x (variablize x))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-memberv x sequence) (restrict-true! z))
                        ((known?-notv-memberv x sequence) (restrict-false! z))))
              x)
             (if (vectorp sequence)
                 (dolist (element sequence)
                   (attach-noticer!
                    #'(lambda ()
                        (cond ((known?-memberv x sequence) (restrict-true! z))
                              ((known?-notv-memberv x sequence) (restrict-false! z))))
                    element))
                 (attach-noticer!
                  #'(lambda ()
                      (cond ((known?-memberv x sequence) (restrict-true! z))
                            ((known?-notv-memberv x sequence) (restrict-false! z))))
                  sequence))
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-memberv x sequence))
                        ((variable-false? z) (assert!-notv-memberv x sequence))))
              z)
             z))))

;;; Lifted Arithmetic Comparison Functions (Two argument optimized)

(defun =v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (cond ((known?-=v2-internal x y) t)
        ((known?-/=v2-internal x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-=v2-variable x y) (restrict-true! z))
                        ((known?-/=v2-variable x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-=v2-variable x y) (restrict-true! z))
                        ((known?-/=v2-variable x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-=v2 x y))
                        ((variable-false? z) (assert!-/=v2 x y))))
              z)
             z))))

(defun <=v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<=v2-internal x y) t)
        ((known?-<v2-internal y x) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<=v2-variable x y) (restrict-true! z))
                        ((known?-<v2-variable y x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<=v2-variable x y) (restrict-true! z))
                        ((known?-<v2-variable y x) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-<=v2 x y))
                        ((variable-false? z) (assert!-<v2 y x))))
              z)
             z))))

(defun <v2 (x y)
  (assert!-realpv x)
  (assert!-realpv y)
  (cond ((known?-<v2-internal x y) t)
        ((known?-<=v2-internal y x) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<v2-variable x y) (restrict-true! z))
                        ((known?-<=v2-variable y x) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-<v2-variable x y) (restrict-true! z))
                        ((known?-<=v2-variable y x) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-<v2 x y))
                        ((variable-false? z) (assert!-<=v2 y x))))
              z)
             z))))

(defun /=v2 (x y)
  (assert!-numberpv x)
  (assert!-numberpv y)
  (cond ((known?-/=v2-internal x y) t)
        ((known?-=v2-internal x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-/=v2-variable x y) (restrict-true! z))
                        ((known?-=v2-variable x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-/=v2-variable x y) (restrict-true! z))
                        ((known?-=v2-variable x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-/=v2 x y))
                        ((variable-false? z) (assert!-=v2 x y))))
              z)
             z))))

;;; Lifted NOTV, ANDV and ORV

(defun notv (x)
  (assert!-booleanpv x)
  (let ((x (value-of x)))
    (cond ((eq x t) nil)
          ((eq x nil) t)
          (t (let ((z (a-booleanv)))
               (attach-noticer!
                #'(lambda ()
                    (cond ((variable-true? x) (restrict-false! z))
                          ((variable-false? x) (restrict-true! z))))
                x)
               (attach-noticer!
                #'(lambda ()
                    (cond ((variable-true? z) (restrict-false! x))
                          ((variable-false? z) (restrict-true! x))))
                z)
               z)))))

(defun andv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (if (member nil xs :test #'eq)
        nil
        (let* ((xs (remove t xs :test #'eq))
               (count (length xs)))
          (cond
            ((zerop count) t)
            ((= count 1) (first xs))
            (t (let ((z (a-booleanv)))
                 (attach-noticer!
                  #'(lambda ()
                      (cond ((variable-true? z) (dolist (x xs) (restrict-true! x)))
                            ((and (= count 1) (variable-false? z))
                             (dolist (x xs)
                               (unless (variable-true? x) (restrict-false! x))))))
                  z)
                 (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!-internal
                      #'(lambda ()
                          (cond ((variable-false? x) (restrict-false! z))
                                ((variable-true? x)
                                 (local (decf count))
                                 (cond ((zerop count) (restrict-true! z))
                                       ((and (= count 1) (variable-false? z))
                                        (dolist (x xs)
                                          (unless (variable-true? x)
                                            (restrict-false! x))))))))
                      x)))
                 z)))))))

(defun andv (&rest xs) (andv-internal xs))

(defun assert!-notv-andv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (unless (member nil xs :test #'eq)
      (let* ((xs (remove t xs :test #'eq))
             (count (length xs)))
        (cond ((zerop count) (fail))
              ((= count 1) (restrict-false! (first xs)))
              (t (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!-internal
                      #'(lambda ()
                          (cond ((variable-false? x))
                                ((variable-true? x)
                                 (local (decf count))
                                 (cond ((zerop count) (fail))
                                       ((= count 1)
                                        (dolist (x xs)
                                          (unless (variable-true? x)
                                            (restrict-false! x))))))))
                      x)))))))))

(defun assert!-notv-andv (&rest xs) (assert!-notv-andv-internal xs))

(defun orv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (if (member t xs :test #'eq)
        t
        (let* ((xs (remove nil xs :test #'eq))
               (count (length xs)))
          (cond
            ((zerop count) nil)
            ((= count 1) (first xs))
            (t (let ((z (a-booleanv)))
                 (attach-noticer!
                  #'(lambda ()
                      (cond ((variable-false? z)
                             (dolist (x xs) (restrict-false! x)))
                            ((and (= count 1) (variable-true? z))
                             (dolist (x xs)
                               (unless (variable-false? x) (restrict-true! x))))))
                  z)
                 (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!-internal
                      #'(lambda ()
                          (cond ((variable-true? x) (restrict-true! z))
                                ((variable-false? x)
                                 (local (decf count))
                                 (cond ((zerop count) (restrict-false! z))
                                       ((and (= count 1) (variable-true? z))
                                        (dolist (x xs)
                                          (unless (variable-false? x)
                                            (restrict-true! x))))))))
                      x)))
                 z)))))))

(defun orv (&rest xs) (orv-internal xs))

(defun assert!-orv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (unless (member t xs :test #'eq)
      (let* ((xs (remove nil xs :test #'eq))
             (count (length xs)))
        (cond ((zerop count) (fail))
              ((= count 1) (restrict-true! (first xs)))
              (t (dolist (x xs)
                   (let ((x x))
                     (attach-noticer!-internal
                      #'(lambda ()
                          (cond ((variable-true? x))
                                ((variable-false? x)
                                 (local (decf count))
                                 (cond ((zerop count) (fail))
                                       ((= count 1)
                                        (dolist (x xs)
                                          (unless (variable-false? x)
                                            (restrict-true! x))))))))
                      x)))))))))

(defun assert!-orv (&rest xs) (assert!-orv-internal xs))

(defun assert!-clause (xs ps)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs)))
    (unless (some #'eq xs ps)
      (let (new-xs new-ps)
        (do ((xrest xs (rest xrest))
             (prest ps (rest prest)))
            ((or (null xrest) (null prest)))
          (let ((x (first xrest))
                (p (first prest)))
            (unless (eq x (not p))
              (push x new-xs)
              (push p new-ps))))
        (let ((count (length new-xs)))
          (cond ((zerop count) (fail))
                ((= count 1)
                 (if (first new-ps)
                     (restrict-true! (first new-xs))
                     (restrict-false! (first new-xs))))
                (t (do ((xrest new-xs (rest xrest))
                        (prest new-ps (rest prest)))
                       ((null xrest))
                     (let ((x (first xrest)))
                       (attach-noticer!-internal
                        (if (first prest)
                            #'(lambda ()
                                (cond ((variable-true? x))
                                      ((variable-false? x)
                                       (local (decf count))
                                       (cond ((zerop count) (fail))
                                             ((= count 1)
                                              (do ((xrest new-xs (rest xrest))
                                                   (prest new-ps (rest prest)))
                                                  ((null xrest))
                                                (let ((x (first xrest)))
                                                  (unless (bound? x)
                                                    (if (first prest)
                                                        (restrict-true! x)
                                                        (restrict-false! x))))))))))
                            #'(lambda ()
                                (cond ((variable-false? x))
                                      ((variable-true? x)
                                       (local (decf count))
                                       (cond
                                         ((zerop count) (fail))
                                         ((= count 1)
                                          (do ((xrest new-xs (rest xrest))
                                               (prest new-ps (rest prest)))
                                              ((null xrest))
                                            (let ((x (first xrest)))
                                              (unless (bound? x)
                                                (if (first prest)
                                                    (restrict-true! x)
                                                    (restrict-false! x)))))))))))
                        x))))))))))

(defun count-trues-internal (xs) (count-if #'identity xs))

(defun count-trues (&rest xs) (count-trues-internal xs))

(defun count-truesv-internal (xs)
  (dolist (x xs) (assert!-booleanpv x))
  (let ((xs (mapcar #'value-of xs))
        (lower 0)
        (upper (length xs)))
    (dolist (x xs)
      (cond ((eq x t) (incf lower))
            ((eq x nil) (decf upper))))
    (if (= lower upper)
        lower
        (let ((z (an-integer-betweenv lower upper))
              (xs (remove-if #'bound? xs)))
          (attach-noticer!
           #'(lambda ()
               (if (= upper (variable-lower-bound z))
                   (dolist (x xs)
                     (unless (variable-false? x) (restrict-true! x))))
               (if (= lower (variable-upper-bound z))
                   (dolist (x xs)
                     (unless (variable-true? x) (restrict-false! x)))))
           z)
          (dolist (x xs)
            (let ((x x))
              (attach-noticer!
               #'(lambda ()
                   (cond ((variable-false? x)
                          (local (decf upper))
                          (restrict-upper-bound! z upper)
                          (if (= upper (variable-lower-bound z))
                              (dolist (x xs)
                                (unless (variable-false? x) (restrict-true! x)))))
                         ((variable-true? x)
                          (local (incf lower))
                          (restrict-lower-bound! z lower)
                          (if (= lower (variable-upper-bound z))
                              (dolist (x xs)
                                (unless (variable-true? x) (restrict-false! x)))))))
               x)))
          z))))

(defun count-truesv (&rest xs) (count-truesv-internal xs))

;;; Lifted FUNCALLV and APPLYV

(defun finite-domain? (variable)
  (let ((variable (value-of variable)))
    (or (not (variable? variable))
        (not (eq (variable-enumerated-domain variable) t))
        (and (variable-integer? variable)
             (variable-lower-bound variable)
             (variable-upper-bound variable)))))

;;; note: SOLUTION, LINEAR-FORCE and STATIC-ORDERING were moved here to be
;;;       before KNOWN?-CONSTRAINT to avoid forward references to
;;;       nondeterministic functions.

(defun solution (x force-function)
  (funcall-nondeterministic
   (value-of force-function) (variables-in (value-of x)))
  (apply-substitution x))

(defun linear-force (variable)
  (let ((variable (value-of variable)))
    (if (variable? variable)
        (restrict-value!
         variable
         (cond ((not (eq (variable-enumerated-domain variable) t))
                (a-member-of (variable-enumerated-domain variable)))
               ((variable-integer? variable)
                (if (variable-lower-bound variable)
                    (if (variable-upper-bound variable)
                        (an-integer-between
                         (variable-lower-bound variable)
                         (variable-upper-bound variable))
                        (an-integer-above (variable-lower-bound variable)))
                    (if (variable-upper-bound variable)
                        (an-integer-below (variable-upper-bound variable))
                        (an-integer))))
               (t (error "It is only possible to linear force a variable that~%~
                        has a countable domain"))))))
  (value-of variable))

(defun static-ordering-internal (variables force-function)
  (if variables
      (let ((variable (value-of (first variables))))
        (cond ((variable? variable)
               (funcall-nondeterministic force-function variable)
               (static-ordering-internal variables force-function))
              (t (static-ordering-internal (rest variables) force-function))))))

(defun static-ordering (force-function)
  ;; note: This closure will heap cons.
  (let ((force-function (value-of force-function)))
    #'(lambda (variables) (static-ordering-internal variables force-function))))

(defun known?-constraint (f polarity? x)
  (let ((f (value-of f)))
    (if (variable? f)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
    (and (every #'finite-domain? x)
         (block exit
           (for-effects
             (if (if polarity?
                     (not (apply f (solution x (static-ordering #'linear-force))))
                     (apply f (solution x (static-ordering #'linear-force))))
                 (return-from exit nil)))
           t))))

(defun propagate-gfc (predicate polarity? variables unassigned-variable)
  ;; note: UNASSIGNED-VARIABLE must be a variable which is not bound and
  ;;       all of the VARIABLES except the UNASSIGNED-VARIABLE must be bound.
  (let ((unassigned-variable (value-of unassigned-variable)))
    ;; There is no way to propagate a value to a variable that doesn't have an
    ;; enumerated domain.
    (if (and (not (eq (variable-enumerated-domain unassigned-variable) t))
             (not (null (rest (variable-enumerated-domain
                               unassigned-variable)))))
        ;; note: Could do less consing if had LOCAL DELETE-IF-NOT.
        ;; note: Consing.
        (let* ((variable-values (mapcar #'value-of variables))
               (new-enumerated-domain
                (if polarity?
                    (remove-if-not
                     #'(lambda (value)
                         (apply predicate
                                ;; note: Consing.
                                (mapcar #'(lambda (variable variable-value)
                                            (if (eq variable unassigned-variable)
                                                value
                                                variable-value))
                                        variables
                                        variable-values)))
                     (variable-enumerated-domain unassigned-variable))
                    (remove-if
                     #'(lambda (value)
                         (apply predicate
                                ;; note: Consing.
                                (mapcar #'(lambda (variable variable-value)
                                            (if (eq variable unassigned-variable)
                                                value
                                                variable-value))
                                        variables
                                        variable-values)))
                     (variable-enumerated-domain unassigned-variable)))))
          (if (set-enumerated-domain! unassigned-variable new-enumerated-domain)
              (run-noticers unassigned-variable))))))

(defun a-tuple (variables variable value)
  (if (null variables)
      nil
      (cons (cond ((eq (first variables) variable) value)
                  ((variable? (first variables))
                   (a-member-of (variable-enumerated-domain (first variables))))
                  (t (first variables)))
            (a-tuple (rest variables) variable value))))

(defun propagate-ac (predicate polarity? variables)
  (unless (some #'(lambda (variable)
                    (and (variable? variable)
                         (eq (variable-enumerated-domain variable) t)))
                variables)
    (dolist (variable variables)
      ;; note: Could do less consing if had LOCAL DELETE-IF-NOT.
      (if (variable? variable)
          (let ((new-enumerated-domain
                 (if polarity?
                     (remove-if-not
                      #'(lambda (value)
                          (possibly?
                            ;; note: Consing.
                            (apply predicate (a-tuple variables variable value))))
                      (variable-enumerated-domain variable))
                     (remove-if
                      #'(lambda (value)
                          (possibly?
                            ;; note: Consing.
                            (apply predicate (a-tuple variables variable value))))
                      (variable-enumerated-domain variable)))))
            (if (set-enumerated-domain! variable new-enumerated-domain)
                (run-noticers variable)))))))

(defun assert!-constraint-gfc (predicate polarity? variables)
  (let ((predicate (value-of predicate))
        (multiple-unassigned-variables? nil)
        (unassigned-variable nil))
    (if (variable? predicate)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
    (unless (functionp predicate)
      (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
    (dolist (variable variables)
      (unless (bound? variable)
        (if unassigned-variable (setf multiple-unassigned-variables? t))
        (setf unassigned-variable variable)))
    (cond
      (multiple-unassigned-variables?
       ;; The case where two or more variables are unbound
       (let ((variables (copy-list variables)))
         (dolist (variable variables)
           (unless (bound? variable)
             (let ((variable variable))
               (attach-noticer!
                #'(lambda ()
                    (global
                      (block exit
                        (let ((unassigned-variable nil))
                          (dolist (variable variables)
                            (unless (bound? variable)
                              (if unassigned-variable (return-from exit))
                              (setf unassigned-variable variable)))
                          (if unassigned-variable
                              (propagate-gfc
                               predicate polarity? variables unassigned-variable)
                              (unless (if polarity?
                                          (apply predicate (mapcar #'value-of variables))
                                          (not (apply predicate
                                                      (mapcar #'value-of variables))))
                                (fail)))))))
                variable))))))
      (unassigned-variable
       ;; The case where all but one of the variables are bound
       (propagate-gfc predicate polarity? variables unassigned-variable))
      ;; The case where all variables are bound
      ;; note: Consing.
      (t (unless (if polarity?
                     (apply predicate (mapcar #'value-of variables))
                     (not (apply predicate (mapcar #'value-of variables))))
           (fail))))))

(defun assert!-constraint-ac (predicate polarity? variables)
  (let ((predicate (value-of predicate)))
    (if (variable? predicate)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV or APPLYV to be an unbound variable"))
    (unless (functionp predicate)
      (error "The first argument to FUNCALLV or APPLYV must be a deterministic~%~
           function"))
    (dolist (variable variables)
      (attach-noticer!
       #'(lambda () (propagate-ac predicate polarity? variables))
       variable))
    (propagate-ac predicate polarity? variables)))

(defun assert!-constraint (predicate polarity? variables)
  (ecase *strategy*
    (:gfc (assert!-constraint-gfc predicate polarity? variables))
    (:ac (assert!-constraint-ac predicate polarity? variables))))

(defun known?-funcallv (f &rest x) (known?-constraint f t x))

(defun known?-notv-funcallv (f &rest x) (known?-constraint f nil x))

(defun assert!-funcallv (f &rest x) (assert!-constraint f t x))

(defun assert!-notv-funcallv (f &rest x) (assert!-constraint f nil x))

(defun funcallv (f &rest x)
  (let ((f (value-of f)))
    (if (variable? f)
        (error "The current implementation does not allow the first argument~%~
              of FUNCALLV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to FUNCALLV must be a deterministic function"))
    (if (every #'bound? x)
        (apply f (mapcar #'value-of x))
        (let ((z (make-variable)))
          (assert!-constraint
           #'(lambda (&rest x) (equal (first x) (apply f (rest x)))) t (cons z x))
          (dolist (argument x)
            (attach-noticer!
             #'(lambda ()
                 (if (every #'bound? x)
                     (assert!-equalv z (apply f (mapcar #'value-of x)))))
             argument))
          z))))

(defun arguments-for-applyv (x xs)
  (unless (bound? (first (last (cons x xs))))
    (error "The current implementation does not allow the last argument to~%~
          APPLYV to be an unbound variable"))
  (apply #'list* (mapcar #'value-of (cons x xs))))

(defun known?-applyv (f x &rest xs)
  (known?-constraint f t (arguments-for-applyv x xs)))

(defun known?-notv-applyv (f x &rest xs)
  (known?-constraint f nil (arguments-for-applyv x xs)))

(defun assert!-applyv (f x &rest xs)
  (assert!-constraint f t (arguments-for-applyv x xs)))

(defun assert!-notv-applyv (f x &rest xs)
  (assert!-constraint f nil (arguments-for-applyv x xs)))

(defun applyv (f x &rest xs)
  (let ((f (value-of f)))
    (if (variable? f)
        (error "The current implementation does not allow the first argument~%~
              of APPLYV to be an unbound variable"))
    (unless (functionp f)
      (error "The first argument to APPLYV must be a deterministic function"))
    (let ((arguments (arguments-for-applyv x xs)))
      (if (every #'bound? arguments)
          (apply f (mapcar #'value-of arguments))
          (let ((z (make-variable)))
            (assert!-constraint
             #'(lambda (&rest x) (equal (first x) (apply f (rest x))))
             t
             (cons z arguments))
            (dolist (argument arguments)
              (attach-noticer!
               #'(lambda ()
                   (if (every #'bound? arguments)
                       (assert!-equalv z (apply f (mapcar #'value-of arguments)))))
               argument))
            z)))))

;;; Lifted EQUALV

(defun known?-equalv (x y)
  (or (eql x y)
      (cond ((variable? x)
             (and (not (eq (variable-value x) x))
                  (known?-equalv (variable-value x) y)))
            ((variable? y)
             (and (not (eq (variable-value y) y))
                  (known?-equalv x (variable-value y))))
            (t (and (consp x)
                    (consp y)
                    (known?-equalv (car x) (car y))
                    (known?-equalv (cdr x) (cdr y)))))))

(defun assert!-equalv (x y)
  (unless (eql x y)
    (cond ((variable? x)
           (cond ((not (eq (variable-value x) x))
                  (assert!-equalv (variable-value x) y))
                 ((variable? y)
                  (if (eq (variable-value y) y)
                      (share! x y)
                      (assert!-equalv x (variable-value y))))
                 (t (restrict-value! x y))))
          ((variable? y)
           (if (eq (variable-value y) y)
               (restrict-value! y x)
               (assert!-equalv x (variable-value y))))
          ((and (consp x) (consp y))
           (assert!-equalv (car x) (car y))
           (assert!-equalv (cdr x) (cdr y)))
          (t (fail)))))

(defun known?-notv-equalv (x y) (one-value (progn (assert!-equalv x y) nil) t))

(defun assert!-notv-equalv (x y)
  ;; note: Can be made more efficient so that if you later find out that
  ;;       X and Y are KNOWN?-NUMBERPV you can then ASSERT!-/=V2.
  (if (known?-equalv x y) (fail))
  (unless (known?-notv-equalv x y)
    (let ((x (variablize x))
          (y (variablize y)))
      (attach-noticer! #'(lambda () (if (known?-equalv x y) (fail))) x)
      (attach-noticer! #'(lambda () (if (known?-equalv x y) (fail))) y))))

(defun equalv (x y)
  "Returns T if the aggregate object X is known to equal the aggregate object
Y, NIL if the aggregate object X is known not to equal the aggregate object Y,
and a new boolean variable V if it is not known whether or not X equals Y when
EQUALV is called.

The values of X, Y and V are mutually constraints via noticers so that V
equals T if and only if X is known to equal Y and V equals NIL if and only if
X is known not to equal Y.

Noticers are attached to V as well as to all variables nested in both in X and
Y. When the noticers attached to variables nested in X and Y detect that X is
known to equal Y they restrict V to equal T. Likewise, when the noticers
attached to variables nested in X and Y detect that X is known not to equal Y
they restrict V to equal NIL.

Furthermore, if V later becomes known to equal T then X and Y are unified.
Likewise, if V later becomes known to equal NIL then X and Y are restricted to
not be equal. This is accomplished by attaching noticers to the variables
nested in X and Y which detect when X becomes equal to Y and fail.

The expression \(KNOWN? (EQUALV X Y)) is analogous to the extra-logical predicate
== typically available in Prolog.

The expression \(KNOWN? (NOTV (EQUALV X Y))) is analogous to the extra-logical
predicate \\= typically available in Prolog.

The expression \(ASSERT! (EQUALV X Y)) is analogous to Prolog unification.

The expression \(ASSERT! (NOTV (EQUALV X Y))) is analogous to the
disunification operator available in Prolog-II."
  ;; note: Can be made more efficient and return an AND tree of individual
  ;;       constraints needed to make EQUALV true. This can be done also for
  ;;       the KNOWN? and ASSERT! versions.
  (cond ((known?-equalv x y) t)
        ((known?-notv-equalv x y) nil)
        (t (let ((x (variablize x))
                 (y (variablize y))
                 (z (a-booleanv)))
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-equalv x y) (restrict-true! z))
                        ((known?-notv-equalv x y) (restrict-false! z))))
              x)
             (attach-noticer!
              #'(lambda ()
                  (cond ((known?-equalv x y) (restrict-true! z))
                        ((known?-notv-equalv x y) (restrict-false! z))))
              y)
             (attach-noticer!
              #'(lambda ()
                  (cond ((variable-true? z) (assert!-equalv x y))
                        ((variable-false? z) (assert!-notv-equalv x y))))
              z)
             z))))

;;; Lifted Arithmetic Functions

(defun +v-internal (xs)
  (if (null xs) 0 (+v2 (first xs) (+v-internal (rest xs)))))

(defun +v (&rest xs) (+v-internal xs))

(defun -v-internal (x xs)
  (if (null xs) x (-v-internal (-v2 x (first xs)) (rest xs))))

(defun -v (x &rest xs) (if (null xs) (-v2 0 x) (-v-internal x xs)))

(defun *v-internal (xs)
  (if (null xs) 1 (*v2 (first xs) (*v-internal (rest xs)))))

(defun *v (&rest xs) (*v-internal xs))

(defun /v-internal (x xs)
  (if (null xs) x (/v-internal (/v2 x (first xs)) (rest xs))))

(defun /v (x &rest xs) (if (null xs) (/v2 1 x) (/v-internal x xs)))

(defun minv-internal (x xs)
  (if (null xs) x (minv-internal (minv2 x (first xs)) (rest xs))))

(defun minv (x &rest xs) (if (null xs) x (minv-internal x xs)))

(defun maxv-internal (x xs)
  (if (null xs) x (maxv-internal (maxv2 x (first xs)) (rest xs))))

(defun maxv (x &rest xs) (if (null xs) x (maxv-internal x xs)))

;;; Lifted Arithmetic Comparison Functions (KNOWN? optimized)

(defun known?-=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-=v2 x (first xs))
           (known?-=v-internal (first xs) (rest xs)))))

(defun known?-=v (x &rest xs) (known?-=v-internal x xs))

(defun known?-<v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<v2 x (first xs))
           (known?-<v-internal (first xs) (rest xs)))))

(defun known?-<v (x &rest xs) (known?-<v-internal x xs))

(defun known?-<=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<=v2 x (first xs))
           (known?-<=v-internal (first xs) (rest xs)))))

(defun known?-<=v (x &rest xs) (known?-<=v-internal x xs))

(defun known?->v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<v2 (first xs) x)
           (known?->v-internal (first xs) (rest xs)))))

(defun known?->v (x &rest xs) (known?->v-internal x xs))

(defun known?->=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-<=v2 (first xs) x)
           (known?->=v-internal (first xs) (rest xs)))))

(defun known?->=v (x &rest xs) (known?->=v-internal x xs))

(defun known?-/=v-internal (x xs)
  (if (null xs)
      t
      (and (known?-/=v2 x (first xs))
           (known?-/=v-internal x (rest xs))
           (known?-/=v-internal (first xs) (rest xs)))))

(defun known?-/=v (x &rest xs) (known?-/=v-internal x xs))

;;; Lifted Arithmetic Comparison Functions (ASSERT! optimized)

(defun assert!-=v-internal (x xs)
  (unless (null xs)
    (assert!-=v2 x (first xs))
    (assert!-=v-internal (first xs) (rest xs))))

(defun assert!-=v (x &rest xs) (assert!-=v-internal x xs))

(defun assert!-<v-internal (x xs)
  (unless (null xs)
    (assert!-<v2 x (first xs))
    (assert!-<v-internal (first xs) (rest xs))))

(defun assert!-<v (x &rest xs) (assert!-<v-internal x xs))

(defun assert!-<=v-internal (x xs)
  (unless (null xs)
    (assert!-<=v2 x (first xs))
    (assert!-<=v-internal (first xs) (rest xs))))

(defun assert!-<=v (x &rest xs) (assert!-<=v-internal x xs))

(defun assert!->v-internal (x xs)
  (unless (null xs)
    (assert!-<v2 (first xs) x)
    (assert!->v-internal (first xs) (rest xs))))

(defun assert!->v (x &rest xs) (assert!->v-internal x xs))

(defun assert!->=v-internal (x xs)
  (unless (null xs)
    (assert!-<=v2 (first xs) x)
    (assert!->=v-internal (first xs) (rest xs))))

(defun assert!->=v (x &rest xs) (assert!->=v-internal x xs))

(defun assert!-/=v-internal (x xs)
  (unless (null xs)
    (assert!-/=v2 x (first xs))
    (assert!-/=v-internal x (rest xs))
    (assert!-/=v-internal (first xs) (rest xs))))

(defun assert!-/=v (x &rest xs) (assert!-/=v-internal x xs))

;;; Lifted Arithmetic Comparisons Functions

(defun =v-internal (x xs)
  (if (null xs)
      t
      (andv (=v2 x (first xs)) (=v-internal (first xs) (rest xs)))))

(defun =v (x &rest xs)
  "Returns a boolean value which is constrained to be T if all of the
arguments are numerically equal, and constrained to be NIL if two or more of
the arguments numerically differ.

This function takes one or more arguments. All of the arguments are restricted
to be numeric.

Returns T when called with one argument. A call such as \(=V X1 X2 ... Xn)
with more than two arguments behaves like a conjunction of two argument calls:

  \(ANDV (=V X1 X2) ... (=V Xi Xi+1) ... (=V Xn-1 Xn))

When called with two arguments, returns T if X1 is known to be equal to X2 at
the time of call, NIL if X1 is known not to be equal to X2 at the time of
call, and a new boolean variable V if is not known if the two values are
equal.

Two numeric values are known to be equal only when they are both bound and
equal according to the Common Lisp function =.

Two numeric values are known not to be equal when their domains are disjoint.
Furthermore, two real values are known not to be equal when their ranges are
disjoint, i.e. the upper bound of one is greater than the lower bound of the
other.

When a new variable is created, the values of X1, X2, and V are mutually
constrained via noticers so that V is equal to T if and only if X1 is known to
be equal to X2, and V is equal to NIL if and only if X1 is known not to be
equal to X2.

* If it later becomes known that X1 is equal to X2 noticers attached to X1 and
  X2 restrict V to equal T. Likewise if it later becomes known that X1 is not
  equal to X2 noticers attached to X1 and X2 restrict V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X1
  to be equal to X2. Likewise if V ever becomes known to equal NIL then a
  noticer attached to V restricts X1 not to be equal to X2.

* If X1 is known to be real then the noticer attached to X2 continually
  restrict the upper bound of X1 to be no higher than the upper bound of X2
  and the lower bound of X1 to be no lower than the lower bound of X2.
  Likewise for bounds of X1 if X2 is known to be real.

Restricting two values x1 and x2 to be equal is performed by attaching
noticers to x1 and x2. These noticers continually restrict the domains of x1
and x2 to be equivalent sets (using the Common Lisp function = as a test
function) as their domains are restricted.

Restricting two values X1 and X2 to not be equal is also performed by
attaching noticers to X1 and X2. These noticers however do not restrict the
domains or ranges of X1 or X2. They simply monitor their continually
restrictions and fail when any assertion causes X1 to be known to be equal to
X2."
  (=v-internal x xs))

(defun <v-internal (x xs)
  (if (null xs)
      t
      (andv (<v2 x (first xs)) (<v-internal (first xs) (rest xs)))))

(defun <v (x &rest xs)
  "Returns a boolean value which is constrained to be T if each argument Xi is
less than the following argument Xi+1 and constrained to be NIL if some
argument Xi is greater than or equal to the following argument Xi+1.

This function takes one or more arguments. All of the arguments are restricted
to be real.

Returns T when called with one argument. A call such as \(<V X1 X2 ... Xn)
with more than two arguments behaves like a conjunction of two argument calls:

  \(ANDV \(<V X1 X2) ... \(<V Xi Xi+1 ) ... \(<V XNn-1 Xn))

When called with two arguments, returns T if X1 is known to be less than X2 at
the time of call, NIL if X1 is known to be greater than or equal to X2 at the
time of call, and otherwise a new boolean variable V.

A real value X1 is known to be less than a real value X2 if X1 has an upper
bound, X2 has a lower bound and the upper bound of X1 is less than the lower
bound of X2.

A real value X1 is known to be greater than or equal to a real value X2 if X1
has a lower bound, X2 has an upper bound and the lower bound of X1 is greater
than or equal to the upper bound of X2.

When a new variable is created, the values of X1, X2 and v are mutually
constrained via noticers so that V is equal to T if and only if X1 is known to
be less than X2 and V is equal to NIL if and only if X1 is known to be greater
than or equal to X2.

* If it later becomes known that X1 is less than X2, noticers attached to X1
  and X2 restrict V to equal T. Likewise, if it later becomes known that X1 is
  greater than or equal to X2, noticers attached to X1 and X2 restrict V to
  equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X1
  to be less than X2. Likewise, if V ever becomes known to equal NIL then a
  noticer attached to V restricts X1 to be greater than or equal to X2.

Restricting a real value X1 to be less than a real value X2 is performed by
attaching noticers to X1 and X2. The noticer attached to X1 continually
restricts the lower bound of X2 to be no lower than the upper bound of X1 if
X1 has an upper bound. The noticer attached to X2 continually restricts the
upper bound of X1 to be no higher than the lower bound of X2 if X2 has a lower
bound. Since these restrictions only guarantee that X1 be less than or equal
to X2, the constraint that X1 be strictly less than X2 is enforced by having
the noticers fail when both X1 and X2 become known to be equal.

Restricting a real value X1 to be greater than or equal to a real value X2 is
performed by an analogous set of noticers without this last equality check."
  (<v-internal x xs))

(defun <=v-internal (x xs)
  (if (null xs)
      t
      (andv (<=v2 x (first xs)) (<=v-internal (first xs) (rest xs)))))

(defun <=v (x &rest xs) (<=v-internal x xs))

(defun >v-internal (x xs)
  (if (null xs)
      t
      (andv (<v2 (first xs) x) (>v-internal (first xs) (rest xs)))))

(defun >v (x &rest xs) (>v-internal x xs))

(defun >=v-internal (x xs)
  (if (null xs)
      t
      (andv (<=v2 (first xs) x) (>=v-internal (first xs) (rest xs)))))

(defun >=v (x &rest xs) (>=v-internal x xs))

(defun /=v-internal (x xs)
  (if (null xs)
      t
      (andv (/=v2 x (first xs))
            (/=v-internal x (rest xs))
            (/=v-internal (first xs) (rest xs)))))

(defun /=v (x &rest xs)
  "Returns a boolean value which is constrained to be T if no two arguments
are numerically equal, and constrained to be NIL if any two or more arguments
are numerically equal.

This function takes one or more arguments. All of the arguments are restricted
to be numeric.

Returns T when called with one argument. A call such as \(/=V X1 X2 ... Xn)
with more than two arguments behaves like a conjunction of two argument calls:

  \(ANDV \(/=V X1 X2) ... \(/=V X1 Xn)
        \(/=V X2 X3) ... \(/=V X2 Xn)
        ...
        \(/=V Xi Xi+1 ... \(/=V Xi Xn)
        ...
        \(/=V Xn-1 xn))

When called with two arguments, returns T if X1 is known not to be equal to X2
at the time of call, NIL if X1 is known to be equal to X2 at the time of
call, and otherwise a new boolean variable V.

Two numeric values are known not to be equal when their domains are disjoint.

Two real values are known not to be equal when their ranges are disjoint, i.e.
the upper bound of one is greater than the lower bound of the other.

Two numeric values are known to be equal only when they are both bound and
equal according to the Common Lisp function =.

When a new variable is created, the values of X1, X2 and V are mutually
constrained via noticers so that V is equal to T if and only if X1 is known
not to be equal to X2 and V is equal to NIL if and only if X1 is known to be
equal to X2.

* If it later becomes known that X1 is not equal to X2, noticers attached to
  X1 and X2 restrict V to equal T. Likewise, if it later becomes known that X1
  is equal to X2, noticers attached to X1 and X2 restrict V to equal NIL.

* If V ever becomes known to equal T then a noticer attached to V restricts X1
  to not be equal to X2. Likewise, if V ever becomes known to equal NIL then a
  noticer attached to V restricts X1 to be equal to X2.

Restricting two values X1 and X2 to be equal is performed by attaching
noticers to X1 and X2. These noticers continually restrict the domains of X1
and X2 to be equivalent sets \(using the Common Lisp function = as a test
function) as their domains are restricted. Furthermore, if X1 is known to be
real then the noticer attached to X2 continually restrict the upper bound of
X1 to be no higher than the upper bound of X2 and the lower bound of X1 to be
no lower than the lower bound of X2. The noticer of X2 performs a symmetric
restriction on the bounds of X1 if it is known to be real.

Restricting two values X1 and X2 to not be equal is also performed by
attaching noticers to X1 and X2. These noticers however, do not restrict the
domains or ranges of X1 or X2. They simply monitor their continually
restrictions and fail when any assertion causes X1 to be known to be equal to
X2."
  (/=v-internal x xs))

;;; The Optimizer Macros for ASSERT!, KNOWN? and DECIDE

(defun known?-true (x) (assert!-booleanpv x) (eq (value-of x) t))

(defun known?-false (x) (assert!-booleanpv x) (null (value-of x)))

(defun-compile-time transform-known? (form polarity?)
  (if (and (consp form) (null (rest (last form))))
      (cond
        ((and (eq (first form) 'notv)
              (= (length form) 2))
         (transform-known? (second form) (not polarity?)))
        ((eq (first form) 'andv)
         (cons (if polarity? 'and 'or)
               (mapcar #'(lambda (form) (transform-known? form polarity?))
                       (rest form))))
        ((eq (first form) 'orv)
         (cons (if polarity? 'or 'and)
               (mapcar #'(lambda (form) (transform-known? form polarity?))
                       (rest form))))
        ((member (first form)
                 '(integerpv realpv numberpv memberv booleanpv
                   =v <v <=v >v >=v /=v funcallv applyv equalv)
                 :test #'eq)
         (cons (cdr (assoc (first form)
                           (if polarity?
                               '((integerpv . known?-integerpv)
                                 (realpv . known?-realpv)
                                 (numberpv . known?-numberpv)
                                 (memberv . known?-memberv)
                                 (booleanpv . known?-booleanpv)
                                 (=v . known?-=v)
                                 (<v . known?-<v)
                                 (<=v . known?-<=v)
                                 (>v . known?->v)
                                 (>=v . known?->=v)
                                 (/=v . known?-/=v)
                                 (funcallv . known?-funcallv)
                                 (applyv . known?-applyv)
                                 (equalv . known?-equalv))
                               '((integerpv . known?-notv-integerpv)
                                 (realpv . known?-notv-realpv)
                                 (numberpv . known?-notv-numberpv)
                                 (memberv . known?-notv-memberv)
                                 (booleanpv . known?-notv-booleanpv)
                                 (=v . known?-/=v)
                                 (<v . known?->=v)
                                 (<=v . known?->v)
                                 (>v . known?-<=v)
                                 (>=v . known?-<v)
                                 (/=v . known?-=v)
                                 (funcallv . known?-notv-funcallv)
                                 (applyv . known?-notv-applyv)
                                 (equalv . known?-notv-equalv)))
                           :test #'eq))
               (rest form)))
        (polarity? `(known?-true ,form))
        (t `(known?-false ,form)))
      (if polarity? `(known?-true ,form) `(known?-false ,form))))

(defmacro-compile-time known? (x)
  "Restricts X to be a boolean. If X is equal to T after being restricted to be boolean,
returns T. If X is equal to NIL or if the value of X is unknown returns NIL.
The argument X can be either a variable or a non-variable.

The initial restriction to boolean may cause other assertions to be made due
to noticers attached to X. A call to KNOWN? fails if X is known not to be
boolean prior to the assertion or if any of the assertions performed by the
noticers result in failure.

Restricting X to be boolean attaches a noticer on X so that any subsequent
assertion which restricts X to be non-boolean will fail.

Except for the fact that one cannot write #'KNOWN?, KNOWN? behaves like a
function, even though it is implemented as a macro.

The reason it is implemented as a macro is to allow a number of compile time
optimizations. Expressions like \(KNOWN? \(NOTV X)), \(KNOWN? \(NUMBERPV X))
and \(KNOWN? \(NOTV \(NUMBERPV X))) are transformed into calls to functions
internal to Screamer which eliminate the need to create the boolean
variable\(s) normally returned by functions like NOTV and NUMBERV. Calls to
the functions NUMBERPV, REALPV, INTEGERPV, MEMBERV, BOOLEANPV, =V, <V, <=V, V,
>=v, /=v, NOTV, FUNCALLV, APPLYV and EQUALV which appear directly nested in a
call to KNOWN?, or directly nested in a call to NOTV which is in turn directly
nested in a call to KNOWN?, are similarly transformed."
  ;; FIXME: better done with a function & compiler-macro
  (transform-known? x t))

(defun assert!-true (x) (assert!-equalv x t))

(defun assert!-false (x) (assert!-equalv x nil))

(defun-compile-time transform-assert! (form polarity?)
  (if (and (consp form) (null (rest (last form))))
      (cond
        ((and (eq (first form) 'notv)
              (= (length form) 2))
         (transform-assert! (second form) (not polarity?)))
        ((eq (first form) 'andv)
         (if polarity?
             `(progn ,@(mapcar
                        #'(lambda (form) (transform-assert! form polarity?))
                        (rest form)))
             (cond ((null (rest form)) `(fail))
                   ((null (rest (rest form))) `(assert!-false ,(second form)))
                   (t `(assert!-notv-andv ,@(rest form))))))
        ((eq (first form) 'orv)
         (if polarity?
             (cond ((null (rest form)) `(fail))
                   ((null (rest (rest form))) `(assert!-true ,(second form)))
                   (t `(assert!-orv ,@(rest form))))
             `(progn ,@(mapcar
                        #'(lambda (form) (transform-assert! form polarity?))
                        (rest form)))))
        ((member (first form)
                 '(integerpv realpv numberpv memberv booleanpv
                   =v <v <=v >v >=v /=v funcallv applyv equalv)
                 :test #'eq)
         (cons (cdr (assoc (first form)
                           (if polarity?
                               '((integerpv . assert!-integerpv)
                                 (realpv . assert!-realpv)
                                 (numberpv . assert!-numberpv)
                                 (memberv . assert!-memberv)
                                 (booleanpv . assert!-booleanpv)
                                 (=v . assert!-=v)
                                 (<v . assert!-<v)
                                 (<=v . assert!-<=v)
                                 (>v . assert!->v)
                                 (>=v . assert!->=v)
                                 (/=v . assert!-/=v)
                                 (funcallv . assert!-funcallv)
                                 (applyv . assert!-applyv)
                                 (equalv . assert!-equalv))
                               '((integerpv . assert!-notv-integerpv)
                                 (realpv . assert!-notv-realpv)
                                 (numberpv . assert!-notv-numberpv)
                                 (memberv . assert!-notv-memberv)
                                 (booleanpv . assert!-notv-booleanpv)
                                 (=v . assert!-/=v)
                                 (<v . assert!->=v)
                                 (<=v . assert!->v)
                                 (>v . assert!-<=v)
                                 (>=v . assert!-<v)
                                 (/=v . assert!-=v)
                                 (funcallv . assert!-notv-funcallv)
                                 (applyv . assert!-notv-applyv)
                                 (equalv . assert!-notv-equalv)))
                           :test #'eq))
               (rest form)))
        (polarity? `(assert!-true ,form))
        (t `(assert!-false ,form)))
      (if polarity? `(assert!-true ,form) `(assert!-false ,form))))

(defmacro-compile-time assert! (x)
  "Restricts X to T. No meaningful result is returned. The argument X can be
either a variable or a non-variable.

This assertion may cause other assertions to be made due to noticers attached
to X.

A call to ASSERT! fails if X is known not to equal T prior to the assertion or
if any of the assertions performed by the noticers result in failure.

Except for the fact that one cannot write #'ASSERT!, ASSERT! behaves like a
function, even though it is implemented as a macro.

The reason it is implemented as a macro is to allow a number of compile time
optimizations. Expressions like \(ASSERT! \(NOTV X)), \(ASSERT! \(NUMBERPV X))
and \(ASSERT! \(NOTV \(NUMBERV X))) are transformed into calls to functions
internal to Screamer which eliminate the need to create the boolean
variable\(s) normally returned by functions like NOTV and NUMBERPV. Calls to
the functions NUMBERPV, REALPV, INTEGERPV, MEMBERV, BOOLEANPV, =V, <V, <=V,
>V, >=V, /=V, NOTV, FUNCALLV, APPLYV and EQUALV which appear directly nested
in a call to ASSERT!, or directly nested in a call to NOTV which is in turn
directly nested in a call to ASSERT!, are similarly transformed."
  ;; FIXME: Should probably be a function + a compiler macro.
  (transform-assert! x t))

(defun-compile-time transform-decide (form polarity?)
  (if (and (consp form) (null (rest (last form))))
      (cond
        ((and (eq (first form) 'notv)
              (= (length form) 2))
         (transform-decide (second form) (not polarity?)))
        ((eq (first form) 'andv)
         (let ((result (mapcar #'(lambda (form)
                                   (multiple-value-list
                                    (transform-decide form polarity?)))
                               (rest form))))
           (values (reduce #'append (mapcar #'first result))
                   (cons (if polarity? 'progn 'either)
                         (mapcar #'second result))
                   (cons (if polarity? 'either 'progn)
                         (mapcar #'third result)))))
        ((eq (first form) 'orv)
         (let ((result (mapcar #'(lambda (form)
                                   (multiple-value-list
                                    (transform-decide form polarity?)))
                               (rest form))))
           (values (reduce #'append (mapcar #'first result))
                   (cons (if polarity? 'either 'progn)
                         (mapcar #'second result))
                   (cons (if polarity? 'progn 'either)
                         (mapcar #'third result)))))
        ((member (first form)
                 '(integerpv realpv numberpv memberv booleanpv
                   =v <v <=v >v >=v /=v funcallv applyv equalv)
                 :test #'eq)
         (let ((arguments (mapcar #'(lambda (argument)
                                      (declare (ignore argument))
                                      (gensym "ARGUMENT-"))
                                  (rest form))))
           (values (mapcar #'list arguments (rest form))
                   (cons (cdr (assoc (first form)
                                     (if polarity?
                                         '((integerpv . assert!-integerpv)
                                           (realpv . assert!-realpv)
                                           (numberpv . assert!-numberpv)
                                           (memberv . assert!-memberv)
                                           (booleanpv . assert!-booleanpv)
                                           (=v . assert!-=v)
                                           (<v . assert!-<v)
                                           (<=v . assert!-<=v)
                                           (>v . assert!->v)
                                           (>=v . assert!->=v)
                                           (/=v . assert!-/=v)
                                           (funcallv . assert!-funcallv)
                                           (applyv . assert!-applyv)
                                           (equalv . assert!-equalv))
                                         '((integerpv . assert!-notv-integerpv)
                                           (realpv . assert!-notv-realpv)
                                           (numberpv . assert!-notv-numberpv)
                                           (memberv . assert!-notv-memberv)
                                           (booleanpv . assert!-notv-booleanpv)
                                           (=v . assert!-/=v)
                                           (<v . assert!->=v)
                                           (<=v . assert!->v)
                                           (>v . assert!-<=v)
                                           (>=v . assert!-<v)
                                           (/=v . assert!-=v)
                                           (funcallv . assert!-notv-funcallv)
                                           (applyv . assert!-notv-applyv)
                                           (equalv . assert!-notv-equalv)))
                                     :test #'eq))
                         arguments)
                   (cons (cdr (assoc (first form)
                                     (if polarity?
                                         '((integerpv . assert!-notv-integerpv)
                                           (realpv . assert!-notv-realpv)
                                           (numberpv . assert!-notv-numberpv)
                                           (memberv . assert!-notv-memberv)
                                           (booleanpv . assert!-notv-booleanpv)
                                           (=v . assert!-/=v)
                                           (<v . assert!->=v)
                                           (<=v . assert!->v)
                                           (>v . assert!-<=v)
                                           (>=v . assert!-<v)
                                           (/=v . assert!-=v)
                                           (funcallv . assert!-notv-funcallv)
                                           (applyv . assert!-notv-applyv)
                                           (equalv . assert!-notv-equalv))
                                         '((integerpv . assert!-integerpv)
                                           (realpv . assert!-realpv)
                                           (numberpv . assert!-numberpv)
                                           (memberv . assert!-memberv)
                                           (booleanpv . assert!-booleanpv)
                                           (=v . assert!-=v)
                                           (<v . assert!-<v)
                                           (<=v . assert!-<=v)
                                           (>v . assert!->v)
                                           (>=v . assert!->=v)
                                           (/=v . assert!-/=v)
                                           (funcallv . assert!-funcallv)
                                           (applyv . assert!-applyv)
                                           (equalv . assert!-equalv)))
                                     :test #'eq))
                         arguments))))
        (t (let ((argument (gensym "ARGUMENT-")))
             (values (list (list argument form))
                     (if polarity?
                         `(assert!-true ,argument)
                         `(assert!-false ,argument))
                     (if polarity?
                         `(assert!-false ,argument)
                         `(assert!-true ,argument))))))
      (let ((argument (gensym "ARGUMENT-")))
        (values
         (list (list argument form))
         (if polarity? `(assert!-true ,argument) `(assert!-false ,argument))
         (if polarity? `(assert!-false ,argument) `(assert!-true ,argument))))))

(defmacro-compile-time decide (x)
  "Restricts X to a be boolean. After X is restricted a nondeterministic
choice is made. For one branch, X is restricted to equal T and \(DECIDE X)
returns T as a result. For the other branch, X is restricted to equal NIL and
\(DECIDE X) returns NIL as a result. The argument X can be either a variable
or a non-variable.

The initial restriction to boolean may cause other assertions to be made due
to noticers attached to X. A call to DECIDE immediately fails if X is known
not to be boolean prior to the assertion or if any of the assertions performed
by the noticers result in failure.

Restricting X to be boolean attaches a noticer on X so that any subsequent
assertion which restricts X to be non-boolean will fail.

Except for implementation optimizations \(DECIDE X) is equivalent to:

  \(EITHER \(PROGN \(ASSERT! X) T) \(PROGN \(ASSERT! \(NOTV X)) NIL))

Except for the fact that one cannot write #'DECIDE, DECIDE behaves like a
function, even though it is implemented as a macro.

The reason it is implemented as a macro is to allow a number of compile time
optimizations. Expressions like \(DECIDE \(NOTV X)), \(DECIDE \(NUMBERPV X))
and \(DECIDE \(NOTV \(NUMBERPV X))) are transformed into calls to functions
internal to Screamer which eliminate the need to create the boolean
variable\(s) normally returned by functions like notv and numberv. Calls to
the functions NUMBERPV, REALPV, INTEGERPV, MEMBERPV, BOOLEANPV, =V, <V, <=V,
>V, >=V, /=V, NOTV, FUNCALLV, APPLYV and EQUALV which appear directly nested
in a call to decide, or directly nested in a call to NOTV which is in turn
directly nested in a call to decide, are similarly transformed."
  ;; FIXME: Sounds like this should be a function + compiler-macro.
  (cl:multiple-value-bind (arguments true false)
      (transform-decide x t)
    `(let ,arguments
       (either (progn ,true t) (progn ,false nil)))))

;;; Lifted Generators
;;; note: The following functions could be handled more efficiently as special
;;;       cases.

(defun a-booleanv (&optional (name nil name?))
  "Returns a boolean variable."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (booleanpv v))
    v))

(defun an-integerv (&optional (name nil name?))
  "Returns an integer variable."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (integerpv v))
    v))

(defun an-integer-abovev (low &optional (name nil name?))
  "Returns an integer variable whose value is constrained to be greater than
or equal to LOW."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (integerpv v) (>=v v low)))
    v))

(defun an-integer-belowv (high &optional (name nil name?))
  "Returns an integer variable whose value is constrained to be less than or
equal to HIGH."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (integerpv v) (<=v v high)))
    v))

(defun an-integer-betweenv (low high &optional (name nil name?))
  "Returns an integer variable whose value is constrained to be greater than
or equal to LOW and less than or equal to HIGH. If the resulting integer
variable is bound, its value is returned instead. Fails if it is known that
there is no integer between LOW and HIGH at the time of call.

The expression \(AN-INTEGER-BETWEENV LOW HIGH) is an abbreviation for:

 \(LET ((V (MAKE-VARIABLE)))
    \(ASSERT! (INTEGERPV V))
    \(ASSERT! (>=V V LOW))
    \(ASSERT! (<=V V HIGH))
    \(VALUE-OF v))
"
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (integerpv v) (>=v v low) (<=v v high)))
    (value-of v)))

(defun a-realv (&optional (name nil name?))
  "Returns a real variable."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (realpv v))
    v))

(defun a-real-abovev (low &optional (name nil name?))
  "Returns a real variable whose value is constrained to be greater than or equal to LOW."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (realpv v) (>=v v low)))
    v))

(defun a-real-belowv (high &optional (name nil name?))
  "Returns a real variable whose value is constrained to be less than or equal to HIGH."
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (realpv v) (<=v v high)))
    v))

(defun a-real-betweenv (low high &optional (name nil name?))
  "Returns a real variable whose value is constrained to be greater than or
equal to low and less than or equal to high. If the resulting real variable is
bound, its value is returned instead. Fails if it is known that low is greater
than high at the time of call.

The expression \(A-REAL-BETWEENV LOW HIGH) is an abbreviation for:

 \(LET ((V (MAKE-VARIABLE)))
    \(ASSERT! (REALPV V))
    \(ASSERT! (>=V V LOW))
    \(ASSERT! (<=V V HIGH))
    \(VALUE-OF V))
"
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (andv (realpv v) (>=v v low) (<=v v high)))
    v))

(defun a-numberv (&optional (name nil name?))
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (numberpv v))
    v))

(defun a-member-ofv (values &optional (name nil name?))
  (let ((v (if name? (make-variable name) (make-variable))))
    (assert! (memberv v values))
    (value-of v)))

;;; Search Control

(defun variables-in (x)
  (typecase x
    (cons (append (variables-in (car x)) (variables-in (cdr x))))
    (variable (list x))
    (otherwise nil)))

;;; note: SOLUTION and LINEAR-FORCE used to be here but was moved to be before
;;;       KNOWN?-CONSTRAINT to avoid forward references to nondeterministic
;;;       functions.

(defun divide-and-conquer-force (variable)
  (let ((variable (value-of variable)))
    (if (variable? variable)
        (cond
          ((not (eq (variable-enumerated-domain variable) t))
           (let ((n (floor (length (variable-enumerated-domain variable)) 2)))
             (set-enumerated-domain!
              variable
              (either (subseq (variable-enumerated-domain variable) 0 n)
                      (subseq (variable-enumerated-domain variable) n)))
             (run-noticers variable)))
          ((and (variable-real? variable)
                (variable-lower-bound variable)
                (variable-upper-bound variable))
           (if (variable-integer? variable)
               (let ((midpoint (floor (+ (variable-lower-bound variable)
                                         (variable-upper-bound variable))
                                      2)))
                 (either (let ((old-bound (variable-upper-bound variable)))
                           (restrict-upper-bound! variable midpoint)
                           (if (= old-bound (variable-upper-bound variable))
                               (fail)))
                         (let ((old-bound (variable-lower-bound variable)))
                           (restrict-lower-bound! variable (1+ midpoint))
                           (if (= old-bound (variable-lower-bound variable))
                               (fail)))))
               (let ((midpoint (/ (+ (variable-lower-bound variable)
                                     (variable-upper-bound variable))
                                  2)))
                 (either (let ((old-bound (variable-upper-bound variable)))
                           (restrict-upper-bound! variable midpoint)
                           (if (= old-bound (variable-upper-bound variable))
                               (fail)))
                         (let ((old-bound (variable-lower-bound variable)))
                           (restrict-lower-bound! variable midpoint)
                           (if (= old-bound (variable-lower-bound variable))
                               (fail)))))))
          (t (error "It is only possible to divide and conquer force a~%~
                  variable that has a countable domain or a finite range")))))
  (value-of variable))

;;; note: STATIC-ORDERING used to be here but was moved to be before
;;;       KNOWN?-CONSTRAINT to avoid a forward reference to a nondeterministic
;;;       function.

(defun domain-size (x)
  (let ((x (value-of x)))
    (typecase x
      (cons (infinity-* (domain-size (car x)) (domain-size (cdr x))))
      (variable
       (cond ((not (eq (variable-enumerated-domain x) t))
              (length (variable-enumerated-domain x)))
             ((and (variable-lower-bound x)
                   (variable-upper-bound x)
                   (variable-integer? x))
              (1+ (- (variable-upper-bound x) (variable-lower-bound x))))
             (t nil)))
      (otherwise 1))))

(defun range-size (x)
  (let ((x (value-of x)))
    (typecase x
      (integer 0)
      (real 0.0)
      (variable (and (variable-real? x)
                     (variable-lower-bound x)
                     (variable-upper-bound x)
                     (- (variable-upper-bound x) (variable-lower-bound x))))
      (otherwise nil))))

(defun corrupted? (variable)
  (let* ((lower-bound (variable-lower-bound variable))
         (upper-bound (variable-upper-bound variable)))
    (and lower-bound
         upper-bound
         (/= lower-bound upper-bound)
         (let ((midpoint (/ (+ lower-bound upper-bound) 2)))
           (or (= midpoint lower-bound) (= midpoint upper-bound))))))

(defun find-best (cost order list)
  (let ((best nil)
        (best-cost nil))
    (dolist (x list)
      (let ((x (value-of x)))
        (if (and (variable? x) (not (corrupted? x)))
            (let ((cost (funcall cost x)))
              (when (and (not (null cost))
                         (or (null best-cost) (funcall order cost best-cost)))
                (setf best x)
                (setf best-cost cost))))))
    best))

(defun reorder-internal
    (variables cost-function terminate? order force-function)
  (let ((variable (find-best cost-function order variables)))
    (when (and variable
               (not (funcall terminate? (funcall cost-function variable))))
      (funcall-nondeterministic force-function (value-of variable))
      (reorder-internal
       variables cost-function terminate? order force-function))))

(defun reorder (cost-function terminate? order force-function)
  ;; note: This closure will heap cons.
  (let ((cost-function (value-of cost-function))
        (terminate? (value-of terminate?))
        (order (value-of order))
        (force-function (value-of force-function)))
    #'(lambda (variables)
        (reorder-internal
         variables cost-function terminate? order force-function))))

(defmacro-compile-time best-value
    (form1 objective-form &optional (form2 nil form2?))
  (let ((bound (gensym "BOUND-"))
        (best (gensym "BEST-"))
        (objective (gensym "OBJECTIVE-")))
    `(let ((,bound nil)
           (,best nil)
           (,objective (variablize ,objective-form)))
       (attach-noticer!
        #'(lambda ()
            (if (and ,bound (<= (variable-upper-bound ,objective) ,bound)) (fail)))
        ,objective)
       (for-effects
         (let ((value ,form1))
           (global (setf ,bound (variable-upper-bound ,objective))
                   (setf ,best value))))
       (if ,bound (list ,best ,bound) ,(if form2? form2 '(fail))))))

(defun template-internal (template variables)
  (cond
    ((and (symbolp template) (char= #\? (aref (string template) 0)))
     (let ((binding (assoc template variables :test #'eq)))
       (if binding
           (values (cdr binding) variables)
           (let ((variable (make-variable template)))
             (values variable (cons (cons template variable) variables))))))
    ((consp template)
     (cl:multiple-value-bind (car-template car-variables)
         (template-internal (car template) variables)
       (cl:multiple-value-bind (cdr-template cdr-variables)
           (template-internal (cdr template) car-variables)
         (values (cons car-template cdr-template) cdr-variables))))
    (t (values template variables))))

(defun template (template)
  (template-internal (value-of template) '()))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *screamer?* nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :screamer *features* :test #'eq))

;;; Tam V'Nishlam Shevah L'El Borei Olam
