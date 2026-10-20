(in-package :om-api)

(require :sb-introspect)

;;; Pure re-exports of sb-mop introspection accessors and sb-introspect helpers.
;;; Wrapped in eval-when so import runs at compile-time, before reader interns
;;; the unqualified symbols in the export list below (CLHS 3.2.3.1).

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FUNCTION-LAMBDA-LIST needs an OM-owned symbol: redefining the imported
  ;; SB-INTROSPECT symbol would modify a locked SBCL implementation package.
  (shadow 'function-lambda-list :om-api)
  (import '(sb-mop:validate-superclass
            sb-mop:class-direct-default-initargs
            sb-mop:class-prototype
            sb-mop:funcallable-standard-class
            sb-mop:class-finalized-p
            sb-mop:finalize-inheritance
            sb-mop:slot-definition-initargs
            sb-mop:slot-definition-initform
            sb-mop:slot-definition-initfunction
            sb-mop:slot-definition-readers
            sb-mop:slot-definition-writers
            sb-mop:slot-definition-type
            sb-mop:slot-definition-allocation
            sb-mop:slot-definition-location
            sb-mop:slot-definition-name
            sb-mop:method-qualifiers
            sb-mop:method-function
            sb-mop:method-generic-function
            sb-mop:generic-function-lambda-list
            sb-mop:generic-function-method-class
            sb-mop:generic-function-methods
            sb-mop:generic-function-name
            sb-mop:specializer-direct-generic-functions
            sb-mop:specializer-direct-methods
            sb-mop:intern-eql-specializer)
          :om-api)
  (import '(sb-introspect:function-type
            sb-introspect:who-calls
            sb-introspect:who-specializes-directly
            sb-introspect:find-definition-source
            sb-introspect:valid-function-name-p)
          :om-api)
  ;; CCL is a nickname of SYSTEM.  MCL-heritage libraries call this spelling
  ;; directly, so both compatibility packages must share the OM-owned symbol.
  (shadowing-import 'om-api::function-lambda-list :system))

(export '(;; upstream LW exports
          class-instance-slots
          class-class-slots
          class-direct-instance-slots
          class-direct-class-slots
          slot-name
          get-class-default-initargs
          get-class-precedence-list
          function-spec-p
          function-name
          method-name
          arglist
          om-remove-method-definition
          eql-specializer?
          eql-specializer-object
          function-documentation
          class-documentation
          ;; SBCL port additions
          class-direct-superclasses
          class-direct-subclasses
          class-direct-slots
          class-precedence-list
          class-default-initargs
          class-slots
          class-name
          ;; pure re-exports (sb-mop)
          validate-superclass
          class-direct-default-initargs
          class-prototype
          funcallable-standard-class
          class-finalized-p
          finalize-inheritance
          slot-definition-initargs
          slot-definition-initform
          slot-definition-initfunction
          slot-definition-readers
          slot-definition-writers
          slot-definition-type
          slot-definition-allocation
          slot-definition-location
          slot-definition-name
          method-qualifiers
          method-specializers
          method-function
          method-generic-function
          generic-function-lambda-list
          generic-function-method-class
          generic-function-methods
          generic-function-name
          specializer-direct-generic-functions
          specializer-direct-methods
          intern-eql-specializer
          ;; pure re-exports (sb-introspect)
          function-lambda-list
          function-type
          who-calls
          who-specializes-directly
          find-definition-source
          valid-function-name-p)
        :om-api)

(defun function-lambda-list (function &optional (error-p t))
  "Return FUNCTION's lambda list using the LispWorks compatibility contract.

In addition to function objects and names, accept the literal LAMBDA
expressions used by legacy OpenMusic libraries.  Return :NONE on failure when
ERROR-P is false."
  (cond ((and (consp function)
              (eq 'lambda (first function))
              (consp (rest function)))
         (second function))
        (error-p
         (sb-introspect:function-lambda-list function))
        (t
         (handler-case (sb-introspect:function-lambda-list function)
           (error () :none)))))

;;; Slot helpers

(defun method-name (method)
  (sb-mop:generic-function-name (sb-mop:method-generic-function method)))

(defun class-slot? (slot)
  (eql :class (sb-mop:slot-definition-allocation slot)))

(defun slot-name (slot)
  (sb-mop:slot-definition-name slot))

;;; Class introspection (filter slots by allocation)

(defmethod class-instance-slots ((class t))
  (unless (sb-mop:class-finalized-p class)
    (sb-mop:finalize-inheritance class))
  (loop for item in (sb-mop:class-slots class)
        unless (class-slot? item) collect item))

(defmethod class-class-slots ((class t))
  (unless (sb-mop:class-finalized-p class)
    (sb-mop:finalize-inheritance class))
  (loop for item in (sb-mop:class-slots class)
        when (class-slot? item) collect item))

(defmethod class-direct-instance-slots ((class t))
  (loop for item in (sb-mop:class-direct-slots class)
        unless (class-slot? item) collect item))

(defmethod class-direct-class-slots ((class t))
  (loop for item in (sb-mop:class-direct-slots class)
        when (class-slot? item) collect item))

;;; Class hierarchy

(defun class-direct-superclasses (class)
  (sb-mop:class-direct-superclasses
   (if (symbolp class) (find-class class) class)))

(defun class-direct-subclasses (class)
  (sb-mop:class-direct-subclasses
   (if (symbolp class) (find-class class) class)))

(defun class-direct-slots (class)
  (sb-mop:class-direct-slots
   (if (symbolp class) (find-class class) class)))

(defun class-precedence-list (class)
  (let ((c (if (symbolp class) (find-class class) class)))
    (unless (sb-mop:class-finalized-p c)
      (sb-mop:finalize-inheritance c))
    (sb-mop:class-precedence-list c)))

(defun class-default-initargs (class)
  (let ((c (if (symbolp class) (find-class class) class)))
    (unless (sb-mop:class-finalized-p c)
      (sb-mop:finalize-inheritance c))
    (sb-mop:class-default-initargs c)))

(defun class-slots (class)
  (let ((c (if (symbolp class) (find-class class) class)))
    (unless (sb-mop:class-finalized-p c)
      (sb-mop:finalize-inheritance c))
    (sb-mop:class-slots c)))

;;; Method ops

;; Deliberate no-op as in vanilla (kernel save.lisp calls it under "do nothing").
(defun om-remove-method-definition (method)
  (declare (ignore method))
  t)
;;(defun om-remove-method-definition (method)
;;  (remove-method (fdefinition (method-name method)) method))

(defun get-class-default-initargs (class)
  (class-default-initargs class))

(defun get-class-precedence-list (class)
  (class-precedence-list class))

;; Unconditional t as in vanilla (keeps the kernel's illegal-arg branch dead).
(defun function-spec-p (spec)
  (declare (ignore spec))
  t)

;; LW shape: eql specializers surface as (eql obj) conses (kernel does (cadr sp)).
(defun method-specializers (method)
  (mapcar (lambda (s)
            (if (typep s 'sb-mop:eql-specializer)
                (list 'eql (sb-mop:eql-specializer-object s))
                s))
          (sb-mop:method-specializers method)))

(defun eql-specializer? (method-specializer)
  (consp method-specializer))

(defun eql-specializer-object (specializer)
  (if (consp specializer)
      (cadr specializer)
      (sb-mop:eql-specializer-object specializer)))

(defun extract-lambda-list-names-and-keys (lambda-list)
  "Return parameter names and keys like LispWorks CLOS::EXTRACT-LAMBDA-LIST-NAMES-AND-KEYS."
  (let ((section :required)
        result)
    (dolist (item lambda-list (nreverse result))
      (cond ((eq item '&aux)
             (return (nreverse result)))
            ((member item '(&whole &environment &body) :test #'eq)
             (error "Illegal lambda-list: ~S" lambda-list))
            ((eq item '&optional)
             (setf section :optional)
             (push item result))
            ((eq item '&rest)
             (setf section :rest)
             (push item result))
            ((eq item '&key)
             (setf section :key)
             (push item result))
            ((eq item '&allow-other-keys)
             (push item result))
            (t
             (push (case section
                     (:optional (if (consp item) (car item) item))
                     (:key (let ((name (if (consp item) (car item) item)))
                             (if (consp name)
                                 (car name)
                                 (intern (symbol-name name) :keyword))))
                     (otherwise item))
                   result))))))

(defun arglist (fun)
  (when (symbolp fun) (setf fun (fdefinition fun)))
  (cond ((typep fun 'generic-function)
         ;; gf lambda-list is always present, even with zero methods
         (extract-lambda-list-names-and-keys
          (sb-mop:generic-function-lambda-list fun)))
        ;; PCL method functions take (args next-methods); the user lambda-list
        ;; sits on the fast function after two internal params
        ((typep fun 'sb-pcl::%method-function)
         (extract-lambda-list-names-and-keys
          (cddr (sb-introspect:function-lambda-list
                 (sb-pcl::%method-function-fast-function fun)))))
        (t (extract-lambda-list-names-and-keys
            (sb-introspect:function-lambda-list fun)))))

(defmethod function-name (fun)
  (when (symbolp fun) (setf fun (fdefinition fun)))
  ;; TRACE wraps functions; the name lives on the innermost function
  (let ((fun (sb-ext:unencapsulated-function fun)))
    (if (typep fun 'generic-function)
        (sb-mop:generic-function-name fun)
        (or (sb-kernel:%fun-name fun)
            (nth-value 2 (function-lambda-expression fun))))))

(defun function-documentation (function)
  (documentation function 'function))

(defun class-documentation (class)
  (let ((realclass (if (symbolp class) (find-class class nil) class)))
    (if realclass
        (documentation realclass t)
        (concatenate 'string "Class " (string class) " not found"))))
