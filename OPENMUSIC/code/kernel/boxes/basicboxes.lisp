;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;This file contains the definition of the basic boxes in OM
;(i.e. sequence, omand, omor, omif, etc)
;For each box we subclass the OMboxCall class in order to redefine :
;      the evaluation      (special-value method) or
;      the persistance     (omng-save method) or
;      the code generation (gen-code method), etc.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;===============================================================================
;CONTROL
;===============================================================================

;--------------SEQUENCE--------------------

(defclass box-seq-call (OMBoxcall)  
   ((numouts :initform 1 :accessor numouts))
   (:documentation "This is the class for the method exception box SEQUENCE.#enddoc#
#seealso# (OMBoxcall) #seealso#
#numouts# The number of outputs in Sequence boxes can change dynamicly, this slots contains this number. #numouts#"))

(defmethod OpenEditorframe ((self box-seq-call)) 
   (not (dialog-message "Compiled Function SEQUENCE.")))

(defmethod get-boxcallclass-fun ((self (eql 'sequence))) 'box-seq-call)

(om-with-redefinitions
 (defmethod* sequence  ((patch t) &rest oppatch) :numouts 1 
   :initvals '(nil) :indoc '("something" "more things")
   :icon 161
   :doc "Evaluates sequentially a series of values, functions or subpatches.

Accepts many optional inputs as needed. 

The different outputs correspond to each of the inputs. To get the result of the nth item connected to the sequence box after the sequential evaluation, evaluate or connect the nth output (all the inputs will be evaluated anyway)."

   (let* ((thelist (append (list patch) oppatch))
          (numout (car (last thelist)))
          (thelist (butlast thelist)))
     (nth numout thelist))))

(defmethod call-gen-code ((self box-seq-call) numout)
   `(sequence ,.(decode self) , numout))

(defmethod gen-code-call ((self box-seq-call))
   `(values ,.(decode self)))

(defmethod special-value ((self  box-seq-call) &optional (args nil))
  (values-list args))

(defmethod do-delete-one-input-extra ((self box-seq-call))
   "When you delete an optional input from the sequence boxes you must delete one output also."
   (setf (numouts self) (- (numouts self) 1))
   t)

(defmethod do-add-one-input-extra ((self box-seq-call))
   "When you add an optional input to the sequence boxes you must add one output too."
   (setf (numouts self) (+ (numouts self) 1))
   t)


(defmethod omNG-save ((self box-seq-call) &optional (values? nil))
  (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
         (value (when values? (omNG-save (value self) values?))))
    `(om-load-seqbox  ,(name self) ',(reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self)) ,value ,(allow-lock self) ,(numouts self))))

;Used to restore a sequence box.
(defmethod om-load-seqbox (name reference inputs position size value lock numouts)
   (let ((newbox (omNG-make-new-boxcall (mk-object-refer 'genfun  reference) (om-correct-point position) name)))
     (setf (frame-size newbox) (om-correct-point size))
     (setf (numouts newbox) numouts)
     (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
     (setf (value newbox) value)
     (setf (allow-lock newbox) lock)
     newbox))


;--------------CALL-NEXT-METHOD-------------------------

(defclass box-next-call (OMBoxcall) ()
   (:documentation "This is the class for the method exception box CALLNEXT-METHOD.
OM Callnext-method function is the equivalent of the CLOS fun call-next-method.#enddoc#
#seealso# (OMBoxcall) #seealso#"))

(defmethod OpenEditorframe ((self box-next-call)) 
   (not (dialog-message "Compiled Function CALLNEXT-METHOD.")))

(defmethod get-boxcallclass-fun ((self (eql 'callnext-method))) 'box-next-call)

(defmethod* callnext-method  () :numouts 1 :initvals nil  
   :doc "Equivalent to CLOS call-next-method.

CallNext-Method is designed to be used in an OM method definition. 
It will call the method with the same name belonging to the next direct super-class of the method argument(s). 
The arguments are automatically passed and are identical to the current method, so there is no input to this box." 
   :icon 147
   nil)

(defmethod gen-code ((self box-next-call) numout)
  (declare (ignore numout))
  `(call-next-method))

(defmethod special-value ((self  box-next-call) &optional (args nil))
  (declare (ignore args))
  (dialog-message "Do not evaluate this box")
  (om-abort))

;--------------REPEAT-N-------------------------

(defclass box-repeat-n-call (OMBoxcall)  
   ()
   (:documentation "This is the class for the method exception box REPEAT-N.#enddoc#
#seealso# (OMBoxcall) #seealso#"))

(defmethod get-boxcallclass-fun ((self (eql 'repeat-n))) 'box-repeat-n-call)

(defmethod OpenEditorframe ((self box-repeat-n-call)) 
  (not (dialog-message "Compiled Function REPEAT-N.")))

(defmethod* repeat-n  ((self t) (n integer)) :numouts 1 :initvals '(nil 0) :indoc '("something" "times")
  :doc "Repeats <n> times the evaluation of <self> and collects the <n> results into a list.

Ex. (repeat-n (+ 1 1) 4) ==> (2 2 2 2)" :icon 181
  (let (rep)
    (loop for i from 1 to n do
          (push self rep))
    (reverse rep)))

(defvar *start-repeat-generation* nil)
(defvar *repeat-ev-once-list* nil)

(defmethod call-gen-code ((self box-repeat-n-call) numout)
   (declare (ignore numout))
   (let ((times (gen-code (second (inputs self)) 0))
         (*start-repeat-generation* t) code1 rep)
     (setf *repeat-ev-once-list* nil)
     (setf code1 (gen-code (first (inputs self)) 0))
     (setf rep `(progn
                  ,.*repeat-ev-once-list*
                  (loop for i from 1 to ,times 
                        collect ,code1)))
     (setf *repeat-ev-once-list* nil)
     rep))

(defmethod gen-code-call ((self box-repeat-n-call))
   (call-gen-code self 0))
  
(defmethod special-value ((self  box-repeat-n-call) &optional (args nil))
  (declare (ignore numout))
  (let* ((times (second args))
         (rep (if (zerop times) nil (list (first args)))))
    (loop for i from 1 to (- times 1) do
          (push (omNG-box-value (first (inputs self))) rep))
    (reverse rep)))
 
;--------------OMIF-------------------------

(defclass OMboxif (OMBoxcall) ())

(defmethod get-boxcallclass-fun ((self (eql 'omif))) 'OMBoxif)

(defmethod OpenEditorframe ((self OMBoxif)) 
   (not (dialog-message "Compiled Function OMIF.")))

(defmethod* omif ((test t) (action t) &optional else) 
   :numouts 1 
   :initvals '(nil nil nil) 
   :indoc '("IF" "THEN" "ELSE")
   :doc "IF <test> THEN <action> ELSE <else>.

If the evaluation of <test> is not NIL, evaluates <action>. 
Otherwise evaluates <else> (when supplied) or returns NIL.

Ex. (omif (= 4 4) 'A 'B)  ==>  'A
Ex. (omif (= 4 5) 'A 'B)  ==>  'B
Ex. (omif (= 4 5) 'A)  ==>  NIL

" 
   :icon 180
   (if test action else))


(defmethod call-gen-code ((self OMBoxif) numout)
   (declare (ignore numout))
   `(if ,(gen-code (first (inputs self)) 0) 
        ,(gen-code (second (inputs self)) 0) 
      ,(gen-code (third (inputs self)) 0)))

(defmethod gen-code-call ((self OMBoxif))
   (call-gen-code self 0))
 
(defmethod omNG-box-value ((self OMBoxif) &optional (numout 0))
   (declare (ignore numout))
   (cond
    ((equal (allow-lock self) "l") (special-lambda-value self 'omif))
    ((and (equal (allow-lock self) "x") (value self)) (nth 0 (value self)))
    ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth 0 (value self)))
    (t (let ((test (omNG-box-value (first (inputs self))))
             rep)
         (if test
           (setf rep (list (omNG-box-value (second (inputs self)))))
           (setf rep (list (when (third (inputs self)) (omNG-box-value (third (inputs self)))))))
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) rep))
         (when (equal (allow-lock self) "x")
           (setf (value self) rep))
         (nth 0 rep)))))

;--------------CONDITIONAL-------------------------

(defclass OMBoxcond (OMBoxcall) ()
   (:documentation "This is the class for the method exception box CONDITIONAL.#enddoc#
#seealso# (OMBoxcall) #seealso#"))

(defmethod get-boxcallclass-fun ((self (eql 'conditional))) 'OMBoxcond)

(defmethod OpenEditorframe ((self OMBoxcond)) 
   (not (dialog-message "Compiled Function CONDITIONAL.")))

(defmethod* conditional ((test t) &rest addtest) 
   :numouts 1 
   :initvals '(nil t) 
   :indoc '("test" "add-test")
   :doc "Evaluates all the inputs from left to right while no one returns a non-NIL value.

Returns the first non-NIL value found.

This function is equaivalent to a logical OR." 
   :icon 180
   (declare (ignore addtest))
   t)

(defmethod call-gen-code ((self OMBoxcond) numout)
   (declare (ignore numout))
   `(or ,.(decode self)))

(defmethod gen-code-call ((self OMBoxcond))
   (call-gen-code self 0))
 
(defmethod omNG-box-value ((self OMBoxcond) &optional (numout 0))
   (declare (ignore numout))
   (cond
    ((equal (allow-lock self) "l") (special-lambda-value self 'conditional))
    ((and (equal (allow-lock self) "x") (value self)) (nth 0 (value self)))
    ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth 0 (value self)))
    (t (let (rep)
         (loop for item in (inputs self)
               while (not rep) do
               (setf rep (omNG-box-value item)))
         (when rep (setf rep (list rep)))
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) rep))
         (when (equal (allow-lock self) "x")
           (setf (value self) rep))
         (nth 0 rep)))))

;--------------Initialize-Instance---------------------

(defclass OMBoxCallNextInit (OMBoxcall) 
   ((whichclass :initform nil :initarg :whichclass :accessor whichclass))
   (:documentation "This is the class for the method exception box INIT-INSTANCE.#enddoc#
#seealso# (OMBoxcall) #seealso#
#whichclass# Say the class of the instance which is being initialized. #whichclass#"))

(omg-defclass nextinitboxframe (nonbuttonboxframe boxframe) ())

(defmethod get-boxcallclass-fun ((self (eql 'init-instance))) 'OMBoxCallNextInit)
(defmethod get-frame-class ((self OMBoxCallNextInit)) 'nextinitboxframe)

(defmethod OpenEditorframe ((self OMBoxCallNextInit)) 
   (not (dialog-message "Compiled Function INIT-INSTANCE.")))

(defmethod* init-instance ((self t) &rest args) 
   (declare (ignore args))
   :initvals '(nil nil) 
   :indoc '("self" "slot")
   :doc "Used in the initialise-instance method for a class." 
   :icon 147
   nil)

;construct
(defun omNG-make-boxcallnext (class posi name)
   "This function makes a init-instance box called <name> for the class <class> at <posi>."
   (let* ((inputs (get-inputs-from-class (class-name class)))
          rep)
     (setf rep (eval `(make-instance 'OMBoxCallNextInit 
                        :name ,name
                        :reference 'init-instance 
                        :icon 147
                        :whichclass ',(class-name class)
                        :inputs (list ,.inputs))))
     (setf (frame-position  rep) (borne-position posi))
     (setf (frame-size rep) (om-make-point (+ 5 (* (length inputs) 55)) 45))
     (setf (allow-lock  rep) "&")
     rep))


(defmethod gen-code ((self OMBoxCallNextInit) numout)
   (declare (ignore numout)) 'self)

(defmethod redef-boxcallnext ((self OMBoxCallNextInit))
   "Called when you redefine a class and it had an init method."
   (let* ((new-inputs (get-inputs-from-class (whichclass self))))
     (mapc #'(lambda (oldin newin) 
               (setf (connected? newin) (connected? oldin))) 
           (inputs self) new-inputs)
     (setf (inputs self) new-inputs)))

 
(defmethod omNG-box-value ((self OMBoxCallNextInit) &optional (numout 0))
   (declare (ignore numout))
   (not (dialog-message "You can not evaluate the function init-instance")))

(defmethod omNG-save ((self OMBoxCallNextInit) &optional (values? nil))
   (declare (ignore values?))
   `(om-load-boxcallnext ',(whichclass self) ,(om-save-point (frame-position self)) ,(frame-name self)))

(defun om-load-boxcallnext (class posi name)
   (omNG-make-boxcallnext (find-class class nil) posi name))


;--------------OMOR-------------------------

(defclass ORboxCall (OMBoxcall) ()
   (:documentation "This is the class for the method exception box OMOR.#enddoc#
#seealso# (OMBoxcall) #seealso#"))

(defmethod get-boxcallclass-fun ((self (eql 'omor))) 'ORboxCall)

(defmethod OpenEditorframe ((self ORboxCall)) 
   (not (dialog-message "Compiled Function OMOR.")))

(defmethod* OMor  ((self t) &rest rest) :numouts 1 :initvals '(nil nil) :indoc '("something" "other things")
   :doc "Logical OR:
Yields the value T (true) if one at least among the values connected to it evaluates to T. 

Accepts as many optional inputs as needed.

OMOR can be used to compose conditions as input to an OMIF" 
   :icon 219
   (eval `(or ,self ,.rest)))

(defmethod omNG-box-value ((self ORboxCall) &optional (numout 0))
   (cond
    ((equal (allow-lock self) "l") (special-lambda-value self 'omor))
    ((and (equal (allow-lock self) "x") (value self)) (nth numout (value self)))
    ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth numout (value self)))
    (t (let ((rep (omNG-box-value (first (inputs self)))))
         (loop while (not rep)
               for item in (cdr (inputs self)) do
               (setf rep (omNG-box-value item))) 
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) (list rep)))
         (when (equal (allow-lock self) "x")
           (setf (value self) (list rep)))
         rep))))

(defmethod call-gen-code ((self ORboxCall) numout)
   (declare (ignore numout))
   `(or ,.(decode self)))

(defmethod gen-code-call ((self ORboxCall))
   (call-gen-code self 0))
  

(defmethod special-value ((self  ORboxCall) &optional (args nil))
   (declare (ignore numout))
   (eval `(or ,.args)))

;--------------OMAND-------------------------

(defclass ANDboxCall (OMBoxcall) ()
   (:documentation "This is the class for the method exception box OMand.
We must use an specil subclass of OMBoxcall because and do not eval all inputs.#enddoc#
#seealso# (omand OMBoxcall  get-boxcallclass-fun) #seealso#"))

(defmethod get-boxcallclass-fun ((self (eql 'omand))) 'ANDboxCall)

(defmethod OpenEditorframe ((self ANDboxCall)) 
  (not (dialog-message "Compiled Function OMAND.")))

(defmethod* omand  ((self t) &rest rest) :numouts 1 :initvals '(nil nil) :indoc '("something" "other things")
   :doc "Logical AND :
Yields the value T (true) if all the values connected to it evaluates to T. 

Accepts as many optional inputs as needed.

OMAND can be used to compose conditions as input to an OMIF"
   :icon 218
   (eval `(and ,self ,.rest)))

(defmethod omNG-box-value ((self ANDboxCall) &optional (numout 0))
   (cond
    ((equal (allow-lock self) "l") (special-lambda-value self 'omand))
    ((and (equal (allow-lock self) "x") (value self)) (nth numout (value self)))
    ((and (equal (allow-lock self) "&") (ev-once-p self)) (nth numout (value self)))
    (t (let ((rep (omNG-box-value (first (inputs self)))))
         (loop while rep
               for item in (cdr (inputs self)) do
               (setf rep (omNG-box-value item))) 
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) (list rep)))
         (when (equal (allow-lock self) "x")
           (setf (value self) (list rep)))
         rep))))


(defmethod call-gen-code ((self ANDboxCall) numout)
   (declare (ignore numout))
   `(and ,.(decode self)))

(defmethod gen-code-call ((self ANDboxCall))
   (call-gen-code self 0))
  


;;=================
;; SPLIT LIST
(defmethod! list-elements ((list list))  
  :initvals '(nil) 
  :indoc '("a list")
  :doc 
"Returns the elements of the list on different ouputs (up to 5O elements).

Use > and < to add/remove outputs.

It is advised to use this box in mode 'eval once' in order to avoid useless computations.
"
  :icon 235
  :numouts 2
  (values-list (first-n list 50)))


(defclass OMBoxSplit (OMBoxcall)  
   ((numouts :initform 2 :accessor numouts)))

(defmethod get-boxcallclass-fun ((self (eql 'list-elements))) 'OMBoxSplit)

(defmethod do-add-one-input ((self OMBoxSplit))
  (if (< (numouts self) 50)
      (setf (numouts self) (+ (numouts self) 1))
    (om-beep))
    t)

(defmethod do-delete-one-input ((self OMBoxSplit))
   (setf (numouts self) (- (numouts self) 1))
   t)

(defmethod omNG-copy ((self OMBoxSplit))
  `(let* ((copy ,(call-next-method)))
     (setf (numouts copy) ,(numouts self))
     copy))

(defmethod omNG-save ((self OMBoxSplit) &optional (values? nil))
  `(let ((box ,(call-next-method)))
     (setf (numouts box) ,(numouts self))
     box))



;------------comparison-----------------
(defmethod* om< ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is smaller than <num2>.
Returns T if the test is verified and NIL if not.
" :icon 255 
  (< num1 num2))

(defmethod* om> ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is greater than <num2>
Returns T if the test is verified and NIL if not." :icon 256 
  (> num1 num2))

(defmethod* om<= ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is smaller or equal to <num2>
Returns T if the test is verified and NIL if not." :icon 257 
  (<= num1 num2))

(defmethod* om>= ((num1 number) (num2 number))  
  :initvals '(0 1) :indoc '("a number" "a number")
  :doc "Tests if <num1> is greater or equal to <num2>
Returns T if the test is verified and NIL if not." :icon 258 
  (>= num1 num2))


(defmethod* om= (a b &optional (test-predicate '=)) 
  :initvals '(0 0 =) :indoc '("a number" "a number" "an equality-test function")
  :menuins '((2 (("=" '=) ("equal" 'equal) ("eq" 'eq) ("eql" 'eql) ("string-equal" 'string-equal))))
  :doc "Tests if <a> is equal to <b>.
Tests by default with '=' (numbers) or with another test function as specified in <test-predicate>.

Returns T if the test is verified and NIL if not." :icon 259 
  (apply test-predicate (list a b)))

(defmethod* om/= ((num1 number) (num2 number))  
  :initvals '(0 0) :indoc '("a number" "a number")
  :doc "Tests if <num1> is NOT equal to <num2>
Returns T if the test is verified and NIL if not." :icon 260 
  (/= num1 num2))


;===============================================================================
;DATA
;===============================================================================

(defun is-om-slot? (class slot-symb)
   (let ((slotlist (get-all-slots-of-class class)))
     (member (string slot-symb) slotlist :test 'string-equal :key 'name)))
     
(defmethod* get-slot ((object t) (slot symbol))
   :initvals '(nil nil) 
   :indoc '("object" "slot") 
   :icon 237
   :doc
   "Returns the value of an object's slot. 

<object> must be an object instance (e.g. the first output of a factory box, or the output of an instance or global variable box). 
<slot> is a slot name corresponding to one of the corresponding classe's slots. 

Warning : It is advised not to use GET-SLOT with predefined OM object, which have particular internal slots value management.
Use rather the get/set slots mechanism provided by the SLOTS boxes (shift+drag an object or factory box)."
   (if (omclass-p (class-of (class-of object)))
     (if (is-om-slot? (type-of object) slot)
       (slot-value object (internp (string slot) (symbol-package (type-of object))))
       (om-beep-msg (string+ (string slot) " is not an OM slot of the class " (string (type-of object)) ".")))
     (om-beep-msg (string+ (string (type-of object)) " is not an OM class."))))

(defmethod* get-slot ((object list) (slot symbol))
   (loop for item in object 
         collect  (get-slot item slot)))

(defmethod* set-slot ((object t) (slot symbol) (value t))
   :initvals '(nil nil nil) 
   :indoc '("object" "slot" "value") 
   :icon 335
   :doc
   "Modifies the value of an object's slot. 

<object> must be an object instance (e.g. the first output of a factory box, or the output of an instance or global variable box). 
<slot> is a slot name corresponding to one of the corresponding classe's slots. 
<value> is the new value to set in the <slot> field of <object>

Returns the modified object <object>.

Warning : It is advised not to use GET-SLOT with predefined OM object, which have particular internal slots value management.
Use rather the get/set slots mechanism provided by the SLOTS boxes (shift+drag an object or factory box).
"
   (if (omclass-p (class-of (class-of object)))
     (if (is-om-slot? (type-of object) slot)
        (setf (slot-value  object (internp (string slot) (symbol-package (type-of object)))) value)
       (om-beep-msg (string+ (string slot) " is not an OM slot of the class " (string (type-of object)) ".")))
     (om-beep-msg (string+ (string (type-of object)) " is not an OM class."))))
  
 
;===============================================================
;Instances of this class can be used as global variable

(defclass* store ()
   ((value :initform nil :initarg :value :accessor value :documentation "the value of the variable"))
   (:icon 215)
   (:documentation "A general storage class used to simulate variables.

A store is generally associated to a global variable in OM.

The slot <value> can contain any data, including instances of other classes. 
It can be accessed (get/set) using the methods get-slot / set-slot or using the SLOTS box (shift + drag a STORE box). "))

  
;===============================================================
;copy

(defmethod omNG-copy ((self t))
   "Return an expresion that produce a copy of 'self' when it is valued."
   (cond
    ((om-color-p self) (om-copy-color self))
    ((om-point-p self) (om-copy-point self))
    ((omclass-p  (class-of self))
     (let* ((theclass (class-name (class-of self)))
            (slots (mapcan #'(lambda (slot)
                               (list (string2initarg (string (first slot))) 
                                     (omNG-copy (slot-value self (first slot))))) 
                           (get-initargs-of-class theclass))))
       `(make-instance ',theclass ,.slots)))
    ((equal (class-name (class-of (class-of self))) 'standard-class)
     (let* ((theclass (class-name (class-of self)))
            (slots (mapcan #'(lambda (slot)
                               (list `(setf (slot-value copy ',(first slot)) 
                                            ,(omNG-copy (slot-value self (first slot)))))) 
                           (get-init-slots-of-class theclass))))
       `(let ((copy (make-instance ',theclass)))
          ,.slots
          copy)))
    (t self)))


(defmethod omNG-copy ((self function)) self)

(defmethod omNG-copy ((self hash-table)) (omng-save self))

(defmethod omNG-copy ((self null))  nil)

(defmethod omNG-copy ((self symbol)) `',self)

;(defmethod omNG-copy ((self list)) `(list ,@(mapcar #'omNG-copy self)))

(defmethod omNG-copy ((self list))
  (if (> (length self) 2047)
    `(append ,.(mapcar #'omNG-copy (group-list self 2047 'circular)))
    `(list ,@(mapcar #'omNG-copy self))
    ))

(defmethod omNG-copy ((self cons)) 
  (if (quoted-form-p self)
    `',(omng-copy (second self))
    (if (null (cdr (last self)))
       (call-next-method)
       `(cons ,(omng-save (first (last self))) ,(omng-save (cdr (last self)))))))



(defmethod* clone ((self t)) 
   :icon 205 
   :indoc '("an object")
   :doc "Makes a copy of an object.

Returns the copy of the object."
   (eval (omng-copy self)))

(defmethod* clone ((self simple-container))
  (copy-container self))
 


;;;===================================

(defmethod! om-shell ((str string))
  :icon 642
  :indoc '("a system command line")
  :initvals '("")
  :doc "Sends the command line <str> (a string) to the system."
  (om-cmd-line str t))




;--------------------------FOR maquettes

(defmethod* TemporalBoxes ((self OMMaquette))
   :icon 326
   :indoc '("a maquette")
   "Returns de temporal boxes contained in the maquette <self>." 
   (loop for item in (boxes self)
         when (boxtempobj-p item)
         collect item))

(defmethod* addBox2Maquette ((self temporalbox) (maquette ommaquette))
   :icon 328
   :initvals '(nil nil)
   :indoc '("temporal object" "a maquette")
   :doc "Adds <self> (a temporal box or a list of temporal boxes) in <maquette>.

This function does not remove the previous TemporalBoxes contained in <maquette>.
In order to remove the previous TemporalBoxes, consider using the functions RemoveTemporalBox or RemoveAllTemporalBoxes.
"
   (let ((newbox (clone self)))
     (if (not (editorframe maquette))
         (omng-add-element maquette newbox)
       (omg-add-element (editorframe maquette) (make-frame-from-callobj newbox)))
     self))

(defmethod* addBox2Maquette ((self list) (maquette ommaquette))
   (loop for item in self 
         collect (addBox2Maquette item maquette)))

(defmethod* removeTemporalBox ((self temporalbox))
   :icon 327
   :initvals '(nil)
   :indoc '("temporal object")
   :doc "Removes <self> (a temporal box or a list of temporal boxes) from its container <maquette> (if it has one)."
   (let ((maquette (mycontainer self)))
     (when maquette
       (if (not (editorframe maquette))
         (omng-remove-element maquette self)
         (omg-remove-element (editorframe maquette) (car (frames self)))))
     self))

(defmethod* removeTemporalBox ((self list))
   (loop for item in self 
         collect (removeTemporalBox item)))

(defmethod* get-maquette ((self temporalbox))
   :icon 333
   :indoc '("a temporal box")
   "Returns de maquette containing the temporal box <self>." 
   (mycontainer self))

(defmethod* removeAllTemporalBoxes ((self OMMaquette))
   :icon 327
   :initvals '(nil)
   :indoc '("a maquette")
   :doc "Removes all TemporalBoxes in <self>."
   (removeTemporalBox (TemporalBoxes self)))

;;;;=====================================
(defmethod! set-eval-func ((self OMMaquette) (func OMMaquette))
       :icon 333
        :indoc '("a maquette" "a function to evaluate the maquette")
        :doc "Sets a specific function as an evauation process for the maquette <self>.

<func> can be either a specifically designed function or a patch. This patch or function must be able to produce a global result starting from the TemporalBoxes contained in the maquette.
This function is useful in particular when designing sound synthesis processes in the Maquette : <func> can be the process that synthesizes a sound starting from the data computed by the temporal boxes and their temporal layout.

Returns the Maquette <self>.

Note : the EVAL-FUNC can also be attached in a Maquette by dragging the patch or function icon to the bottom-left corner of the maquette editor.

"
        nil)

(defmethod! set-eval-func ((self OMMaquette) (func t)) nil)

(defmethod! set-eval-func ((self OMMaquette) (func null)) 
  (setf (eval-func self) nil))

(defmethod! set-eval-func ((self OMMaquette) (func OMPatch))
  (setf (eval-func self) (omng-make-new-boxcall func (om-make-point 0 0) "maqfunc"))
  (unless (loaded? func) (load-patch func))
  (when (editorframe self)
    (om-invalidate-view (editor (editorframe self)))) 
  self)

(defmethod! set-eval-func ((self OMMaquette) (func OMPatchAbs))
  (setf (eval-func self) (omng-make-new-boxcall (eval (omng-copy func)) (om-make-point 0 0) "absfunc"))
  (unless (loaded? func) (load-patch func))
  (when (editorframe self)
    (om-invalidate-view (editor (editorframe self)))) 
  self)

(defmethod! set-eval-func ((self OMMaquette) (func OMGenericFunction))
  (setf (eval-func self) (omng-make-new-boxcall func (om-make-point 0 0) "maqfunc"))
  (when (editorframe self)
    (om-invalidate-view (editor (editorframe self)))) 
  self)

(defmethod! set-eval-func ((self OMMaquette) (func OMLispFun))
  (setf (eval-func self) (omng-make-new-boxcall func (om-make-point 0 0) "maqfunc"))
  (when (editorframe self)
    (om-invalidate-view (editor (editorframe self)))) 
  self)
  
(defmethod! set-eval-func ((self OMMaquette) (func symbol))
  (when (fboundp func)
    (set-eval-func self (omNG-make-new-lispfun func))))


;;;========================================


(defmethod! get-tempobjs-value ((self list) &optional (test nil))
  (let ((rep nil))
    (loop for tempbox in self do
          (when (boxtempobj-p tempbox)
            (when (or (not test) (funcall test (get-mus-ob tempbox)))
              (let ((obj (get-mus-ob tempbox)))
                (cond ((allowed-in-maq-p obj)
                       (setf (offset obj) (slot-value tempbox 'offset)))
                      ((consp obj)
                       (loop for ob in obj do 
                             (when (allowed-in-maq-p ob)
                               (setf (offset ob) (+ (offset ob) (slot-value tempbox 'offset))))))
                      )
                (push obj rep)))))
    (reverse rep)))

(defmethod! get-tempobjs ((self list) &optional (test nil))
  (let ((rep nil))
    (loop for tempbox in self do
          (when (boxtempobj-p tempbox)
            (if (or (not test) (funcall test tempbox))
              (push tempbox rep))))
    (reverse rep)))


(defmethod! test-value-class ((self Temporalbox) class)
  (equal class (class-name (class-of (get-mus-ob self)))))

(defmethod! test-obj-class ((self t) class)
  (equal class (class-name (class-of self))))


;--------------------------------------------------------------------

(defmethod make-one-instance ((self Temporalbox) &rest slots-vals)
   (setf slots-vals (apply 'pretraitement (cons self slots-vals)))
   (apply 'make-a-tempobj slots-vals))

#|
(defmethod make-a-tempobj ((offset integer) (extend integer) (color t) 
                             value (posy integer) (strech number) (sizey integer) store refer)
   (unless refer (setf refer (make-instance 'OMTemporalPatch :name "tempobj")))
   (let* ((newobj (omNG-make-tempobj refer (om-make-point 0 0) "tempobj")))
     (setf (offset newobj) offset)
     (setf (extend newobj) extend)
     (setf (sizey newobj) sizey)
     (setf (posy newobj) posy)
     (setf (free-store newobj) store)
     (when value (setf (value newobj) value))
     (setf (strech-fact newobj) strech)
     (setf (colorframe newobj) color)
     newobj))
|#

(defmethod make-a-tempobj ((offset integer) (extend integer) (color t) 
                             value (posy integer) (strech number) (sizey integer) store refer &optional mute lock)
   (unless refer (setf refer (make-instance 'OMPatchAbs :name "tempobj")))
   (let* ((newobj (omNG-make-tempobj refer (om-make-point 0 0) "tempobj")))
     (setf (offset newobj) offset)
     (setf (extend newobj) extend)
     (setf (sizey newobj) sizey)
     (setf (posy newobj) posy)
     (setf (free-store newobj) store)
     (when value (setf (value newobj) value))
     (setf (strech-fact newobj) strech)
     (setf (colorframe newobj) color)
     (setf (mute newobj) mute)
     (setf (lock newobj) lock)
     newobj))

;these functions are kept by compatibility with old patches but they are not put in the package

(defmethod* starttime ((self temporalbox) &optional (val nil))
  :icon 239
  :numouts 2
  :initvals '(nil nil)
  :indoc '("the temporal object" "if you use this arg you make a set")
  :doc "get or set the start time of the temporal object"
  (if (null val) (values self (offset self))
      (setf (offset self) val))
  (values self (offset self)))

(defmethod* objdur ((self temporalbox) &optional (val nil))
  :icon 239
  :numouts 2
  :initvals '(nil nil)
  :indoc '("the temporal object" "if you use this arg you make a set")
  :doc "get or set the duration of the temporal object"
  (cond
   ((null val) (values self (extend self)))
   ((and (integerp val) (> val 0))
    (setf (extend self) val)
    (values self (extend self)))
   (t
    (om-beep-msg "Bad value in the function objdur"))))

(defmethod* mymaquette ((self temporalbox))
  :icon 239
  :initvals '(nil)
  :indoc '("the temporal object" )
  :doc "get the maquette which contains self"
  (mycontainer self))

(defmethod* put-in-maq ((self temporalbox) (maquette ommaquette))
  :icon 239
  :initvals '(nil nil)
  :indoc '("the temporal object" "the maquette")
  :doc "put in the maquette"
  (if (not (editorframe maquette))
    (omng-add-element maquette self)
    (omg-add-element (editorframe maquette) (make-frame-from-callobj self)))
  self)

(defmethod* objfree-store ((self temporalbox) &optional control)
  :icon 239
  :numouts 2
  :initvals '(nil nil)
  :indoc '("the temporal object" )
  :doc "get or set the freq temporal object"
  (cond
   ((null control) (values self (free-store self)))
   (t (setf (free-store self) control)
      (values self (free-store self)))))

(defmethod* eval-tempobj ((self temporalbox))
  :icon 239
  :initvals '(nil)
  :indoc '("the temporal object" )
  :doc "get or set the freq temporal object"
  (cond
   ((car (frames self)) (eval+redraw (car (frames self))))
   (t (omng-box-value self))))




