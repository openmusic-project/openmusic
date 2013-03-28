;;;;=========================================================
;;;;
;;;;  PATCH-WORK functionals
;;;;  Camilo Rueda.
;;;;  © 1993 IRCAM 
;;;;
;;;;=========================================================
;;;;=========================================================
;;;;
;;;;  PATCH-WORK: Functional object construction box
;;;;  [Camilo] 1993
;;;;
;;;; Isssues not yet settled: 
;;;;     EVAL-ONCE module inside RECURSIVE patches,
;;;;     Modules call compiler explicitly, so don't work in no-LISP images
;;;;=========================================================
 
(in-package :pw)

(defclass C-funarg-input (C-abstract-in) ())

(defmethod compile-me ((self C-funarg-input) obj)
  (let* ((in-index (patch-value (first (pw-controls self)) obj))
         (var
          (read-from-string (format () "~A~A" "var" in-index))))
    (unless (assoc in-index (abstract-in-list obj))
      (push (cons in-index var) (abstract-in-list obj)))
    `',var))

(defmethod patch-value ((self C-funarg-input) obj)
  (declare (IGNORE obj)) nil)

(defunp parameter ((order fix>0 (:type-list '(no-connection)))) nil
        "parameter of a functional. The order must be unique among all parameters of the function"
  (declare (ignore order)))

(defclass C-funarg-patch (C-PW-functional) 
  ((code :initform nil :initarg :code :accessor code)
   (fun-name :initform nil :accessor fun-name)
   (abstract-in-list :initform nil :accessor abstract-in-list)
   (clock :initform 0 :accessor clock)    ;;;so that ev-once works (in the future!)
   (compiled-code :initform nil :accessor compiled-code)))

(defmethod form-new-code ((self C-funarg-patch) code)
  (let ((new-let-code (copy-list (first (last code)))))
    (setf (nth 2 new-let-code) `(list (list '*global-calling-object* ,self)))
    (append (butlast code) (list new-let-code))))
          
(defmethod get-right-compilation ((self C-funarg-patch) name code)
  (and (first (last code))
       (if name 
         (setf (compiled-code self) (fdefinition (compile (eval (eval (form-new-code self code))))))
         (setf (compiled-code self) (compile ()  (eval (form-new-code self code)))) )))

(defmethod patch-value ((self C-funarg-patch) obj)
  (setf (abstract-in-list self) nil)
  (setf (fun-name self) (and (second (pw-controls self))
                             (patch-value (second (input-objects self)) obj)))
  (let ((full-fun-name (and (fun-name self)
                            (read-from-string (format () "USER-SUPPLIED-IN-OUTS::~A" (fun-name self))))))
    (if (eq (first (pw-controls self)) (first (input-objects self)))
       (or (compiled-code self) #'identity)
      (let* ((code (compile-me (first (input-objects self)) self)) ;;(compile-me (first (input-objects self)) self))
             (varlist (mapcar #'cdr (sort (abstract-in-list self) '< :key #'car))))
        (if full-fun-name
          (setf (code self) `(list  'defun ',full-fun-name ',varlist
                                    (list 'let (list (list '*global-calling-object* pw::*global-clock*))
                                          '(incf (clock *global-calling-object*)) ,code)))
          (setf (code self) 
                ;;`(list 'function 
                `(list 'lambda ',varlist
                       (list 'let (list (list '*global-calling-object* pw::*global-clock*))
                             '(incf (clock *global-calling-object*)) ,code))))
        (get-right-compilation self full-fun-name (code self))) )
      ))

(defmethod funarg-me? ((self C-funarg-patch)) t)

;;;============================================================
;;; No funarg'ing method by default

(defmethod funarg-me? ((self C-patch)) nil)

;;;=============================================================

(defmethod compile-me ((self C-funarg-patch) obj)
  (declare (ignore obj))
  (code self))

(defmethod decompile ((self C-funarg-patch))
   (append (call-next-method) `(nil ',(list (code self) (fun-name self)))))

(defmethod complete-box ((self C-funarg-patch) args)
  (setf (code self) (first args))
  (setf (fun-name self) (second args))
  (get-right-compilation self (fun-name self) (code self)))

(defunp funarg ((patch list (:value '() :type-list ()))
                &optional (name list (:value '() :type-list (no-connection)))) nil
        " constructs a functional object equivalent to the input patch"
  (declare (ignore patch name)))

(defclass C-eval-fun (C-pw-functional) ())

(defmethod patch-value ((self C-eval-fun) obj) (declare (ignore obj)) nil)

(defmethod compile-me ((self C-eval-fun) obj)
  (let ((name (patch-value (first (pw-controls self)) obj)))
    (if name
      (let ((abs (mapcar #'(lambda (ctrl input) 
                             (if (eq ctrl input)
                               `'',(patch-value ctrl obj)
                               (compile-me input obj) ))
                         (rest (pw-controls self)) (rest (input-objects self)))))
        `(list ',(intern (string name) "USER-SUPPLIED-IN-OUTS") ,@abs))
      (error "A name must be supplied to eval-fun module"))))

(defunp eval-fun ((name list (:value '() :type-list (no-connection)))
                  &rest (arg list (:value '() :type-list ()))) nil
        "defines a recursive call of a named function defined by the funarg module"
  (declare (ignore name arg)))

(defunp Curry ((fun list (:value '() :type-list nil))
               &rest (arg fix (:value most-negative-fixnum :type-list ()))) nil
        "Returns the curried version of <fun> with already evaluated arguments taken from
<args>"
  (let (name fun-vars call-vars)
    (when fun
      (dotimes (index (length arg))
        (if (and (numberp (nth index arg)) (= (nth index arg) most-negative-fixnum))
          (progn
            (setq name (read-from-string (format () "~A~A" "var" index)))
            (push name call-vars)
            (push name fun-vars))
          (push `',(nth index arg) call-vars)))
      (eval `(function (lambda ,(nreverse fun-vars)
                         (apply ',fun
                                (list ,@ (nreverse call-vars)))))))))

(defclass C-delay-evalexp (C-PW-resize-x)
  ((abstract-in-list :initform nil :accessor abstract-in-list)))

(defmethod patch-value ((self C-delay-evalexp) obj)
  (setf (abstract-in-list self) nil)
  (if (eq (first (pw-controls self)) (first (input-objects self)))
    (eval `(function (lambda () ',(patch-value (first (pw-controls self)) obj))))
    (eval `(function 
            (lambda ()  ,(eval (compile-me (first (input-objects self)) self)))))))

(defmethod compile-me ((self C-delay-evalexp) obj)
  (let ((code
         (if (eq (first (pw-controls self)) (first (input-objects self)))
           `'',(patch-value (first (pw-controls self)) obj)
           (compile-me (first (input-objects self)) obj) )))
    `(list 'eval (list 'function (list 'lambda () ,code)))))

(defunp del-eval ((exp list (:type-list ()))) nil 
"returns a form delaying the evaluation of <exp>"
(declare (ignore exp)))

(defun get-the-user-fun (fun)
  (if (symbolp fun)
    (or (fdefinition fun)
        (fdefinition (read-from-string (format nil "USER-SUPPLIED-IN-OUTS::~A" fun)))
        (error "can't find definition of function: ~A" fun))))

(defunp compose-fun ((fun1 list (:value 'identity :type-list ()))
                     (fun2 list (:value 'identity :type-list ()))
                     &rest (fun list (:value 'identity :type-list ()))) nil
        "returns a function which is the composition of the given functions"
  (let ((args (list (gensym)))
        (all-funs (mapcar #'(lambda (fun) (if (symbolp fun) (get-the-user-fun fun) fun))
                           (pw::x-append fun1 fun2 fun))))
    (eval
     `(function (lambda ,args
                  ,(reduce #'(lambda (x y) `(funcall ,y ,x)) all-funs :initial-value (first args)))))))

(defmethod funarg-me ((self C-abstract) obj)
  (setf (abstract-in-list self) nil)
  (let ((code (funarg-me (out-obj self) obj))
        (varlist (mapcar #'cdr (sort (abstract-in-list self) '< :key #'car)))
        (abs (mapcar #'(lambda (ctrl input) 
                         (if (eq ctrl input)
                           `'',(patch-value ctrl obj)
                           (compile-me input obj) ))
                     (pw-controls self) (input-objects self))))
    `(list (list 'lambda ',varlist ,code) ,@abs)))

;;;;=================================================
;;;; This should replace the method given in file abst-fun.lisp (Kernel)

(defmethod compile-me ((self C-abstract) obj)
  (setf (abstract-in-list self) nil)
  (if (funarg-me? obj)
    (funarg-me self obj)
    (let* ((code (compile-me (out-obj self) obj))
           (varlist (mapcar #'cdr (sort (abstract-in-list self) '< :key #'car)))
           (pw-fun (read-from-string (format nil "~A" (gensym "abs"))))
           (patches (controls (patch-win self)))
           (in-boxes (find-abstract-in-boxes (patch-win self) patches))
           (in-docs-temp)
           (in-put-docs 
            (when in-boxes 
              (setq in-docs-temp (get-absin-boxes-types-n patches in-boxes))
              (cond
               ((member nil in-docs-temp)
                (CCL:message-dialog 
                 "WARNING! absin box connected to irreducible types. ALL-type used")
                (mapcar #'(lambda (type-spec) (declare (ignore type-spec)) '(nilNum))
                        in-docs-temp))
               (t in-docs-temp))))
           new-patch-box)
      (setq new-patch-box (make-std-patch-box 'C-compiled-patch
                                              (string pw-fun)
                                              in-put-docs (patch-win self) in-boxes))
      (setq pw-fun (read-from-string (string (pw-function self))))
      (copy-PW-symbolic-type-data (pw-function new-patch-box) pw-fun)
      (set-function-arg-names pw-fun varlist)
      (setf (pw-function new-patch-box) pw-fun)
      (setf (code new-patch-box) `(list 'function (list 'lambda ',varlist ,code)))
      (save new-patch-box)
      )))

(defmethod funarg-me ((self C-patch) obj) (compile-me self obj))

;;;=========================================================

;;(pw-addmenu-fun *dyn-menu* 'funarg 'C-funarg-patch)
;;(pw-addmenu-fun *dyn-menu* 'parameter 'C-funarg-input)

(defvar *all-functionals-menu* (pw::new-menu "Functionals"))
;(defvar *funarg-menu* (pw::new-menu "Function"))


;;(add-menu-items *special-menu* (new-leafmenu "-" ()))

(pw-addmenu-fun *all-functionals-menu* 'funarg 'C-funarg-patch)
(pw-addmenu-fun *all-functionals-menu* 'parameter 'C-funarg-input)
(pw-addmenu-fun *all-functionals-menu* 'eval-fun 'C-eval-fun)
(pw-addmenu *all-functionals-menu* '(Curry))
;;;(pw-addmenu-fun *special-menu* 'del-eval 'C-delay-evalexp)
(pw-addmenu *all-functionals-menu* '(compose-fun))

;;(add-menu-items *all-functionals-menu* *funarg-menu*)
;;(add-menu-items (pw::the-user-menu) *all-functionals-menu*)

#|(eval-when (load eval compile)
(unless (member *special-menu* (menu-items (the-user-menu)))
  (add-menu-items (the-user-menu) *special-menu*)))|#



