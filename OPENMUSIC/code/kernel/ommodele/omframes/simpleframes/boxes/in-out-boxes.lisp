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
;Objects + Frames for I/O in patches.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;--------------------------------------------------
;Typed inputs for methods
;--------------------------------------------------

(defclass OMTypedIn (OMBoxcall)
   ((keys :initform nil :accessor keys)
    (enable :initform t :accessor enable)
    (docu :initform "no documentation" :accessor docu)
    (defval :initform nil :accessor defval)
    (indice :initform t :initarg :indice :accessor indice))
   (:documentation "Input boxes in a method are instance of this class. #enddoc#
#seealso# (ommethod TypedInFrame) #seealso#
#keys# The keyword of the input. #keys#
#enable# A flag for the input edition. #enable#
#docu# Store the documentation of the input. #docu#
#defval# Store the default value of the input. #defval#
#indice# Used in order to sort all inputs in a patch. #indice#"))


(defmethod Typed-in-p ((self t)) nil)
(defmethod Typed-in-p ((self OMTypedIn)) t)


(defmethod allow-lock-button ((self OMTypedIn)) nil)
(defmethod get-frame-class ((self OMTypedIn)) 'TypedInFrame)
(defmethod get-frame-name ((self OMTypedIn)) (name self))
(defmethod get-default-input-val ((self OMTypedIn)) (defval self))
(defmethod get-documentation ((self OMTypedIn)) (string+ "This is the  " (format () "~D" (indice self)) " input of type "
                                                     (string (reference self)) ". " (docu self)))
(defmethod get-object-insp-name ((self OMTypedIn)) "Class Specializer")


(defmethod basic-type-p  ((self OMTypedIn))
   (get-basic-type (reference self)))
 
(defmethod numouts ((self OMTypedIn)) 
   (if (basic-type-p self) 1
       (length (get-outs-name (defval self)))))


(defmethod make-outputs-of-frame ((self OMTypedIn) module)
   (if (basic-type-p self)
       (make-basic-output self module)
       (make-outputs-from-names self (defval self) module)))
        
(defmethod make-basic-output ((self t) module) 
   (let ((thenewout (om-make-view (get-out-class self)
                      :position (om-make-point (- (round (w module) 2) 4) 
                                                 (- (h module) 9))
                      :size (om-make-point 8 8)
                      :help-spec "Basic Type input"
                      :index 0)))
     (push thenewout (outframes module))
     (om-add-subviews module thenewout)))


(defun make-new-typed-input (name type indice posi)
   (let* ((gentype (find-if #'(lambda (x) (equal (class-name x) type)) 
                            (list+ *Basic-Lisp-Types* *OM-class-list*)))
          (icon (icon gentype))
          (theinput (make-instance 'OMTypedIn
                      :name name
                      :icon icon
                      :reference type
                      :indice indice)))
     (setf (defval theinput) (get-super-default-value type))
     (setf (frame-position theinput) posi)
     theinput))

(defun get-typed-boxes (lis)
   (let* (rep)
     (mapc #'(lambda (box)
               (if (Typed-in-p box) 
                 (push box rep))) lis)
     (reverse rep)))

(defmethod gen-code ((self OMTypedIn) numout)
   (if (= numout 0)
     (read-from-string (name self))
     `(,(internp (nth numout (get-outs-name (defval self))) (symbol-package (reference self))) ,(read-from-string (name self)))))

(defmethod omNG-box-value ((self OMTypedIn) &optional (numout 0))
   (if (basic-type-p self) (eval (defval self))
       (rep-editor (defval self) numout)))

(defmethod do-add-one-input ((self OMTypedIn))  nil)
(defmethod do-add-all-inputs ((self OMTypedIn)) nil)
(defmethod do-delete-one-input ((self OMTypedIn)) nil)

(defmethod do-add-one-keyword ((self OMTypedIn) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OMTypedIn)) nil)

(defmethod OpenEditorframe ((self OMTypedIn))
   (or (editorframe self)
       (if (equal (win-mod (editor (om-view-container (car (frames self))))) :abs)
         (set-input-typed-dialog self)
         (om-beep-msg "The input edition must be made at function definition."))))


(defmethod reset-type ((self OMTypedIn))
   (unless (equal (reference self) t)
     (if (car (frames self))
       (omg-change-type (car (frames self)) 't)
       (setf (reference self) t))))
     


;USED FOR INITIALIZE-INSTANCE

(defclass OMinitTypedIn (OMTypedIn) 
   ((self? :initform nil :initarg :self? :accessor self?)
    (classname :initform nil :initarg :classname :accessor classname))
   (:documentation "This is class the class of inputs in a initialize instance method. #enddoc#
#seealso# (ommethod TypedInFrame omclass) #seealso#
#self?# T if the input is the first input of the method. #self?#
#classname# The name of the class specializing the init method. #classname#"))

(defmethod numouts ((self OMinitTypedIn))
   (if (self? self) 1 (call-next-method)))

(defun make-new-typed-init (name type indice posi self? classname)
   (let* ((gentype (find-if #'(lambda (x) (equal (class-name x) type)) 
                            (list+ *Basic-Lisp-Types* *OM-class-list*)))
          (icon (icon gentype))
          (theinput (make-instance 'OMinitTypedIn
                      :name name
                      :icon icon
                      :self? self?
                      :classname classname
                      :reference type
                      :indice indice)))
     (setf (defval theinput) (get-super-default-value type))
     (setf (frame-position theinput) posi)
     theinput))

(defmethod gen-code ((self OMinitTypedIn) numout)
   (cond
    (*pretraitement-passing*
     (if (= numout 0)
       `(nth ,(- (indice self) 1) args)
       `(,(internp (nth numout (get-outs-name (defval self))) (symbol-package (reference self))) 
         (nth ,(- (indice self) 1) args))))
    ((self? self) (read-from-string "self"))
    (t (if (= numout 0)
         `(get-init-from-args args ,(string2initarg  (name self)) self)
         `(,(internp (nth numout (get-outs-name (defval self))) (symbol-package (reference self))) 
           (get-init-from-args args ,(string2initarg  (name self)) self))))))
           
(defun get-init-from-args (args name class)
   (let (rep)
     (loop for item in args
           for i = 0 then (+ i 1)
           while (not rep) do
           (unless (oddp i)
             (if (string-equal (string name) (string item))
               (setf rep i))))
    (if rep
       (nth (+ rep 1) args)
       (get-defval-for-slot class (string name) ))))

  
;----FRAME

(omg-defclass TypedInFrame (boxframe) ())

(defmethod initialize-instance ((self TypedInFrame) &rest initargs)
   (call-next-method)
   (om-set-font self *om-default-font1*))


(defmethod omG-change-type ((self TypedInFrame) thetype)
   (if (keys (object self))
     (om-beep-msg (string+ "You can not type a " (string (keys (object self))) " input."))
     (let* ((obj (object self))
            (container (om-view-container self))
            (newin (make-new-typed-input (name obj) thetype (indice obj) (frame-position obj)))
            (frame (make-frame-from-callobj newin)))
       (when (active-mode self) 
         (omG-unselect self)) 
       (setf (indice (object self)) nil)
       (omg-remove-element container self)
       (real-make-delete-before container (list self))
       (omG-add-element container frame))))


(defmethod omg-remove-element ((self methodpanel) (box TypedInFrame))
   (if (or (null (indice (object box))) (equal (win-mod (editor self)) :abs))
     (progn
       (call-next-method)
       (let* ((boxes (get-subframes self)) 
              (inputs (find-class-boxes boxes 'TypedInFrame)))
         (when (indice (object box))
           (loop for item in inputs do
                 (when (> (indice (object item)) (indice (object box)))
                   (setf (indice (object item)) (- (indice (object item)) 1))
                   (om-invalidate-view item t))))))
     (om-beep-msg "You can not modify the inputs of a function already defined")))


(defmethod draw-typed-special ((self typedinframe))
  (om-with-focused-view self
    (om-with-font *om-default-font1*
                  ;(om-draw-rect-outline 0 9 (w self) (- (h self) 28))
                  (om-draw-string (- (round (w self) 2) 4) 8 
                                  (format () "~D" (indice (object self))))))
  )

(defmethod om-draw-contents ((self TypedInFrame))
  (call-next-method)
  (draw-typed-special self))


(defmethod OMGMoveObject ((self TypedInFrame) new-position)
   (if (om-shift-key-p)
     (let* ((target (om-view-container self)) newobj)
       (when (and target (find-class (reference (object self)) nil) (omclass-p  (find-class (reference (object self)) nil)))
         (setf newobj (omNG-make-new-boxcall-slots 
                       (find-class (reference (object self)))  
                       (borne-position new-position) (mk-unique-name target "slot")))
         (omG-add-element target (make-frame-from-callobj newobj))))
     (call-next-method)))


(defmethod move-frame-delta ((self TypedInFrame) dir)
   (let ((pixnum (if (om-shift-key-p) 10 1)) new-position)
     (setf new-position
           (case dir
             (0 (om-subtract-points (om-view-position self) (om-make-point 0 pixnum)))
             (1 (om-add-points (om-view-position self) (om-make-point 0 pixnum)))
             (2 (om-add-points (om-view-position self) (om-make-point pixnum 0)))
             (3 (om-subtract-points (om-view-position self) (om-make-point pixnum 0)))))
       (mapc #'(lambda (conection)
                 (draw-connection conection nil)) (connections self))
       (om-set-view-position self new-position)
       (setf (frame-position (object self)) new-position)))
  

(defun check-method-arg-name (name panel)
  (cond ((not (string-equal (string (read-from-string name)) name))
         (om-message-dialog "Wrong name for a method input: Avoid SPACE or other special characters!!")
         nil)
        ((find name (find-class-boxes (get-subframes panel) 'TypedInFrame) :key 'name :test 'string-equal)
         (om-message-dialog "Wrong name: Choose different names for the different inputs!!")
         nil)
        (t t)))

(defmethod omG-rename ((self TypedInFrame) new-name)
  (if (check-method-arg-name new-name (om-view-container self))
    (setf (name (object self)) new-name))
  (call-next-method self (name (object self))))

;-------------------------------------------------------

(defclass selfTempIn (OMTypedIn)
   ((in-symbol :initform nil :initarg :in-symbol :accessor in-symbol))
   (:documentation "The self input of a initialize instance method has a specific class. #enddoc#
#seealso# (selfInFrame) #seealso#
#in-symbol# A sylmbol used for code generation. #in-symbol#"))

(defmethod OpenEditorframe ((self selfTempIn)) (not (dialog-message "You can't edit this input.")))

(defmethod get-frame-class ((self selfTempIn)) 'selfInFrame)

(defun make-self-temp-input (name posi)
  (let ((rep (make-instance 'selfTempIn
                :name name
                :icon 238
                :reference 'TemporalBox
                :indice 0)))
     (setf (frame-position rep) posi)
     (setf (defval rep) (make-instance 'temporalbox))
     rep))


(pushr 'tempin *spec-new-boxes-types*)

(defmethod patch-has-temp-in-p ((self ompatch)) 
  (or (find-class-boxes (boxes self) 'selfTempIn)
      (find-class-boxes (boxes self) 'maq-TempIn)))


(defmethod get-new-box-from-type ((type (eql 'tempin)) position container)
  (let* ((patch (object container))
         (self-boxes (patch-has-temp-in-p patch)))
    (if self-boxes (om-beep-msg "This patch has a temporal input")
        (let* ((newbox (make-self-temp-input "self" position))
              (boxes (get-subframes container))
              (inputs (find-class-boxes boxes 'InFrame)))
          (loop for item in inputs do
                (setf (indice (object item)) (+ (indice (object item)) 1))
                (om-invalidate-view item t))
          (setf (defval newbox) (make-instance 'temporalbox))
          newbox))))


(defmethod omNG-add-element ((self OMPatch) (elem selfTempIn))
   "When you add a new input to the patch 'self' you must update all ompatchboxes attached to 'self'."
  (push elem (boxes self))
  (setf (mycontainer elem) self)
  (setf *input-to-erase* -1)
  (loop for item in (attached-objs self) do
        (update-from-reference item))
  (setf *input-to-erase* nil)
 )

;;; REMOVE
(defmethod omng-remove-element ((self OMPatch) (box selfTempIn))
   "When you remove an input from the patch 'self' you must update all ompatchboxes attached to 'self'."
   (call-next-method)
   (setf *input-to-erase* (indice box))
   (loop for item in (attached-objs self) do
         (update-from-reference item))
   (setf *input-to-erase* nil))

(defmethod boxinputs-sizefactor ((self selfTempIn)) 15)

(defmethod gen-code ((self selfTempIn) numout)
   (if (= numout 0)
     (in-symbol self)
     `(,(internp (nth numout (get-outs-name (defval self))) (symbol-package (reference self))) ,(in-symbol self))))



;;; FRAME

(omg-defclass selfInFrame (TypedInFrame) ())

(defmethod omg-remove-element ((self patchPanel) (box selfInFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   
   (let* ((boxes (get-subframes self)) 
          (inputs (find-class-boxes boxes 'InFrame)))
     (when (indice (object box))
       (loop for item in inputs do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t)))))
   (call-next-method))


;;; si on a un patch rouge attache a une temporal box,
;;alors on peut mettre la temporalbox comme defval du selfTempinIn

(defmethod new-defval ((tempi selftempin) (patch ompatch)) nil)

(defmethod new-defval ((tempin selftempin) (patch ompatchabs)) 
  (when (subtypep (type-of (car (attached-objs patch))) 'temporalbox)
    (setf (defval tempin) (car (attached-objs patch)))))

;;; on fait ca au moment ou on ajoute le selfinframe
;;; i.e. quand on ouvre le patch ou quand on la crŽe manuellement ("tempin")
(defmethod add-subview-extra ((self selfInFrame))
  (call-next-method)
  (let ((patch (object (om-view-container self))))
    (new-defval (object self) patch)))



;--------------------------------------------------
;Patch inputs 
;--------------------------------------------------
(defclass OMIn (OMBoxcall)
   ((docu :initform "" :accessor docu)
    (defval :initform nil :accessor defval)
    (in-symbol :initform nil :accessor in-symbol)
    (indice :initform t :initarg :indice :accessor indice))
   (:documentation "Input boxes in a patch are instance of this class. #enddoc#
#seealso# (omboxcall ompatch inFrame) #seealso#
#docu# Store the documentation of the input. #docu#
#defval# Store the default value of the input. #defval#
#in-symbol# A symbol used for code generation. #in-symbol#
#indice# Used in order to sort all inputs in a patch. #indice#"))

(defmethod OpenEditorframe ((self OMIn)) (or (editorframe self) (set-input-dialog self)))
(defmethod numouts ((self OMIn)) 1)
(defmethod get-frame-class ((self OMIn)) 'inFrame)
(defmethod get-frame-name ((self OMIn)) (or (frame-name self) (name self)))
(defmethod allow-lock-button ((self OMIn)) nil)
(defmethod get-documentation ((self OMIn)) "Input ~D~%" (indice self) (docu self))
(defmethod get-object-insp-name ((self OMIn)) "Normal input")
(defmethod get-icon-box-class ((self OMIn)) 'inout-icon-box)

(defun make-new-patch-input (name indice posi &optional (icon 154) (class 'OMin))
   (let* ((theinput (make-instance class
                      :name name
                      :icon icon
                      :reference nil
                      :indice indice)))
     (setf (frame-position theinput) posi)
     theinput))

(defmethod omNG-add-element ((self OMPatch) (elem OMIn))
   "When you add a new input to the patch 'self' you must update all ompatchboxes attached to 'self'."
  (push elem (boxes self))
  (setf (mycontainer elem) self)
  (unless *loaading-stack* ;;; do not update if patch is being loaded: inputs are already OK
    (loop for item in (attached-objs self) do
          (update-from-reference item))))

(defvar *input-to-erase* nil)

(defmethod omng-remove-element ((self OMPatch) (box OMin))
   "When you remove an input from the patch 'self' you must update all ompatchboxes attached to 'self'."
   (call-next-method)
   (setf *input-to-erase* (indice box))
   (loop for item in (attached-objs self) do
         (update-from-reference item))
   (setf *input-to-erase* nil))

(defmethod gen-code ((self OMin) numout)
   (declare (ignore numout))
   (if *compiling-macro-boite* (defval self) (in-symbol self)))

(defmethod omNG-box-value ((self OMIn) &optional (numout nil))
   (declare (ignore numout))
   (if numout 
       (nth numout (current-box-value self))
     (current-box-value self)))

;;; OMout does not store values (?)
(defmethod current-box-value ((self OMin) &optional (numout nil))
  (list (eval (defval self))))

(defmethod get-default-input-val ((self OMIn)) nil) 
(defmethod do-add-one-input ((self OMIN))  nil)
(defmethod do-add-all-inputs ((self OMIN)) nil)
(defmethod do-delete-one-input ((self Omin)) nil)

(defmethod do-add-one-keyword ((self OMIN) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self Omin)) nil)

;-------FRAME
(omg-defclass inFrame (boxframe) ()
   (:documentation "Simple frame for OMIN boxes. #enddoc#
#seealso# (OMIN) #seealso#")
   (:default-initargs :view-font (list *signs-font* 18)))



(defmethod omg-remove-element ((self patchPanel) (box InFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (inputs (find-class-boxes boxes 'InFrame)))
     (when (indice (object box))
       (loop for item in inputs do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))))

(defmethod omG-rename ((self inFrame) new-name)
  (setf (name (object self)) new-name)
  (call-next-method))

(pushr 'in *spec-new-boxes-types*)
(defmethod get-new-box-from-type ((type (eql 'in)) position container)
  (if (add-input-enabled container 'in)
  (let* ((boxes (get-subframes container))
         (i (length (list+ (find-class-boxes boxes 'selfInFrame) (find-class-boxes boxes 'InFrame)))))
    (make-new-patch-input (mk-unique-name container "input") i position))
  (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))

(defmethod show-big-doc ((self inframe)) nil)

;--------------------------------------------------
;Patch outputs 
;--------------------------------------------------


(defclass OMout (OMBoxcall)
   ((indice :initform t :initarg :indice :accessor indice))
   (:documentation "Output boxes in a patch are instance of this class. #enddoc#
#seealso# (omboxcall ompatch outFrame) #seealso#
#indice# Used in order to sort all outputs in a patch. #indice#"))

(defmethod OpenEditorframe ((self OMout))  (not (dialog-message "Outputs have no editor")))
(defmethod allow-lock ((self OMout)))
(defmethod allow-lock-button ((self OMout)) nil)
(defmethod get-frame-name ((self OMOut)) (or (frame-name self) (name self)))
(defmethod get-frame-class ((self OMout)) 'outFrame)
(defmethod numouts ((self OMout)) 0)
(defmethod show-info-window ((self OMout) &optional (i 0)) (om-beep-msg "No info for out boxes"))
(defmethod get-icon-box-class ((self OMOut)) 'inout-icon-box)

(defun make-new-output (name indice posi &optional (icon 155) (class 'OMout))
   (let* ((theout (make-instance class
                    :name name
                    :icon icon
                    :reference nil
                    :indice indice)))
     (setf (frame-position theout) posi)
     (setf (inputs theout) (list (make-instance 'input-funbox
                                   :name "out"
                                   :value nil
                                   :box-ref theout
                                   :doc-string nil)))
     theout))

(defmethod gen-code ((self OMout) numout)
   (declare (ignore numout))
   (let ((theinput (connected? (car (inputs self)))))
     (if theinput
       (gen-code (first theinput) (second theinput)) 'nil)))

(defmethod omNG-box-value ((self OMout) &optional (numout 0))
   (declare (ignore numout))
   (setf (value self) (mapcar 'omng-box-value (inputs self))))

;;; OMout does not store values (?)
(defmethod current-box-value ((self OMout) &optional (numout nil))
  (if numout (nth numout (value self))
    (value self)))


(defmethod omNG-add-element ((self OMPatch) (elem OMOut))
   "When you add a new output to the patch 'self' you must update all ompatchboxes attached to 'self'."
   (setf (mycontainer elem) self)
   (push elem (boxes self))
   (loop for item in (attached-objs self) do
         (update-from-reference item)))

(defvar *output-to-delete* nil)
(defmethod omng-remove-element ((self OMPatch) (box OMOut))
   "When you remove an output from the patch 'self' you must update all ompatchboxes attached to 'self'."
   (call-next-method)
   (setf *output-to-delete* (indice box))
   (loop for item in (attached-objs self) do
         (update-from-reference item))
   (setf *output-to-delete* nil))

(defmethod do-add-one-input ((self OmOut))  nil)
(defmethod do-delete-one-input ((self OmOut)) nil)
(defmethod do-add-all-inputs ((self OmOut)) nil)

(defmethod do-add-one-keyword ((self OmOut) &optional (input-key nil))  nil)
(defmethod do-delete-one-keyword ((self OmOut)) nil)

;-------------FRAME
(omg-defclass outFrame (boxframe) ()
   (:documentation "Simple frame for OMOut boxes. #enddoc#
#seealso# (OMIN) #seealso#")
   (:default-initargs :view-font (list *signs-font* 18)))

;;;le numero sur la fleche : transféré dans inout-icon-box
;;;(defmethod om-draw-contents ((self outFrame))
;;;   (call-next-method)
;;;   (with-font-focused-view self
;;;     (draw-string (- (round (w self) 2) 2) 22 (format () "~D" (indice (object self))))))

(defmethod omg-remove-element ((self methodpanel) (box outFrame))
   "Remove only for the generic function definition."
   (if (equal (win-mod (editor self)) :abs)
     (call-next-method)
     (om-beep-msg "You can not modify the outputs of a function already defined")))


(defmethod omg-remove-element ((self patchPanel) (box outFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (inputs (find-class-boxes boxes 'outFrame)))
     (when (indice (object box))
       (loop for item in inputs do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))))


(pushr 'out *spec-new-boxes-types*)
(defmethod get-new-box-from-type ((type (eql 'out)) position container)
  (if (add-output-enabled container 'out)
    (let* ((boxes (get-subframes container)) 
           (i (length (list+ (find-class-boxes boxes 'tempOutFrame) (find-class-boxes boxes 'outFrame)))))
      (make-new-output (mk-unique-name container "output") i position))
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))
    ))

(defmethod show-big-doc ((self outframe)) nil)

;==================================
; special icon-view avec numero dessus
;==================================

(omg-defclass inout-icon-box (icon-box) ())

(defmethod om-draw-contents ((self inout-icon-box))
  (call-next-method)
  (let ((container (om-view-container self)))
    (unless (tempoutframe-p container)
      (draw-in-inout-box self (object container))
      )))

(defmethod draw-in-inout-box ((container t) object)
  (om-with-focused-view container
    (om-with-font *om-default-font1b*
                  (om-draw-string (- (round (w container) 2) 7) 12 (format () "~2D" (indice object))))
    ))
  
;------------------------------------------
;SPECIAL I/O for maquette patches
;------------------------------------------

(defclass OMtempOut (OMout) ()
   (:documentation "This is the class for special output boxes in a OMTemporalPatch.
(This output determine the value of the temporal box.) #enddoc#
#seealso# (omboxcall OMTemporalPatch tempOutFrame) #seealso#
#indice# Used in order to sort all outputs in a patch. #indice#"))

(pushr 'tempout *spec-new-boxes-types*)

(defmethod patch-has-temp-out-p ((self ompatch)) (find-class-boxes (boxes self) 'OMtempOut))


(defmethod get-new-box-from-type ((type (eql 'tempout)) position container)
  (declare (ignore position))
  (let* ((patch (object container))
         (self-boxes (find-class-boxes (boxes patch) 'OMtempOut)))
    (if self-boxes (om-beep-msg "This patch has a temporal output")
        (let* ((newbox (make-new-temp-output "tempout" position))
              (boxes (get-subframes container))
              (inputs (find-class-boxes boxes 'outFrame)))
          (loop for item in inputs do
                (setf (indice (object item)) (+ (indice (object item)) 1))
                (om-invalidate-view item t))
          newbox))))

(defmethod omNG-add-element ((self OMPatch) (elem OMTempOut))
   "When you add a new output to the patch 'self' you must update all ompatchboxes attached to 'self'."
   (setf (mycontainer elem) self)
   (push elem (boxes self))
   (setf *output-to-delete* -1)
   (loop for item in (attached-objs self) do
         (update-from-reference item))
   (setf *output-to-delete* nil))

(defmethod get-frame-class ((self OMtempOut)) 'tempOutFrame)

(defun make-new-temp-output (name posi)
  (let* ((icon 163)
         (theout (make-instance 'OMtempOut
                   :name name
                   :icon icon
                   :reference nil
                   :indice 0)))
    (setf (frame-position theout) posi)
    (setf (inputs theout) (list (make-instance 'input-funbox
                                  :name "out"
                                  :value nil
                                  :doc-string "out")))
    theout))

;-----FRAME
(omg-defclass tempOutFrame (boxframe) ())

(defmethod tempoutframe-p ((setf t)) nil)
(defmethod tempoutframe-p ((setf tempoutframe)) t)


(defmethod omg-remove-element ((self patchPanel) (box tempoutFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method)
   (let* ((boxes (get-subframes self)) 
          (inputs (find-class-boxes boxes 'outFrame)))
     (when (indice (object box))
       (loop for item in inputs do
             (when (> (indice (object item)) (indice (object box)))
               (setf (indice (object item)) (- (indice (object item)) 1))
               (om-invalidate-view item t))))))


;======================INPUTS DIALOGS================================

(omg-defclass inputEditor (editorView) ())

(defmethod handle-key-event ((self inputEditor) char) nil)

(defmethod close-editor-after ((self inputEditor))
  (setf (editorFrame (object self)) nil))

(defmethod close-editor-before ((self inputEditor))
  (call-next-method))


(defmethod editor-set-help-action ((self inputEditor) txt)
  (let ((boxframe (car (frames (object self))))) 
         (setf (docu (object self)) txt)
         (om-view-set-help boxframe txt)
         (om-view-set-help (iconview boxframe) txt)))

(defmethod editor-set-defval-action ((self inputEditor) val inst-p)
   (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                                               (om-beep-msg "error : bad default value form")
                                               ;(om-set-dialog-item-text (defval-item self) (format () "~S" (eval (defval (object self)))))
                                               (om-abort)
                                               )))
     (let* ((*package* (find-package :om))
            (input (object self))
            newval)
       (if inst-p
         (setf newval val)
         (setf newval `',(read-from-string val)))
       (setf (defval input) newval))))

(defmethod editor-copy ((self inputEditor))
   nil)

(defmethod editor-paste ((self inputEditor))
   nil)


;---------------------------------------------------------------
;A special class for defval,
;---------------------------------------------------------------
(defclass defval-item () 
  ((editbox :accessor editbox :initarg :editbox :initform nil)
   (dropbox :accessor dropbox :initarg :dropbox :initform nil)
   (object :initform nil :initarg :object :accessor object)
   (instance-p :initform nil :initarg :instance-p :accessor instance-p)))

(defmethod (setf instance-p) (instance-p (self defval-item))
   (setf (slot-value self 'instance-p) instance-p)
   (if instance-p
     (progn
       (om-set-fg-color (editbox self) *om-light-gray-color*)
       (om-invalidate-view (editbox self)))
     (progn
       (om-set-fg-color (editbox self) *om-black-color*)
       (om-invalidate-view (editbox self))
       (setf (droppedval (dropbox self)) nil)
       (om-invalidate-view (dropbox self)))
     ))

;-----------

(omg-defclass defval-editbox (om-text-edit-view) 
   ((item :initform nil :initarg :item :accessor item)))

(defmethod om-view-key-handler ((self defval-editbox) key)
   (setf (instance-p (item self)) nil)
   (call-next-method))



;-----------

(defclass defval-drop (drop-area) 
  ((droppedval :accessor droppedval :initarg :droppedval :initform nil)
   (icn :accessor icn :initarg :icn :initform nil)
   (item :initform nil :initarg :item :accessor item)))

(defmethod om-draw-contents ((self defval-drop))
  (call-next-method)
  (if (droppedval self)
    (om-draw-picture self (cadr (icn self)) :pos (om-make-point 40 5) :size (om-make-point 20 20))
    (om-with-focused-view self
      (om-with-fg-color self *om-light-gray-color*
        (om-draw-string 5 15 "drop instance")
        (om-draw-string 5 25 "here")))))

(defmethod drop-area-action ((target defval-drop) (dragged t)) nil)

(defmethod drop-area-action ((target defval-drop) (dragged instboxframe)) 
  (let* ((inform (instance (reference (object dragged))))
         (defval (eval (omng-copy inform))))
    (when (listp defval) (setf defval `',defval))
    (setf (droppedval target) defval)
    (setf (icn target) (get&corrige-icon (iconid (iconview dragged))))
    (setf (instance-p (item target)) t)
    (om-invalidate-view target)
    t))

;------------------Dialogs--------------------

(defmethod allow-rename ((self OMIn)) t)
(defmethod allow-rename ((self OMTypedIn)) t)
(defmethod allow-rename ((self OMOut)) t)


(defmethod get-editor-class ((self OMIn)) 'InputEditor)

(defun set-input-dialog (theinput)
  (let* ((dialog (make-editor-window 'inputEditor theinput (or (frame-name theinput) (name theinput)) nil
                                     :winpos :centered
                                     :winsize (om-make-point 220 158)
                                     :resize nil))
         (doc-scroller (om-make-dialog-item 'om-text-edit-view
                                            (om-make-point 107 50) (om-make-point 100 75)
                                            (docu theinput)
                                            :save-buffer-p t
                                            :scrollbars :v
                                            :allow-returns t
                                            :font *om-default-font2*
                                            :wrap-p t ))
         (defvalitem (make-instance 'defval-item :object theinput))
         (defval-scroller (om-make-dialog-item 'defval-editbox
                                               (om-make-point 3 50) (om-make-point 100 35) 
                                               (format () "~S" (eval (defval theinput))) 
                                               :item defvalitem
                                               :scrollbars :v
                                               :font *om-default-font2*
                                               ;:allow-returns t
                                               :help-spec "double click to edit initform <command> to see"
                                               ))
         (dropzone (om-make-view 'defval-drop 
                                 :position (om-make-point 3 94) 
                                 :size (om-make-point 100 30)
                                 :item defvalitem
                                 :bg-color *om-white-color*
                                 :object theinput
                                 ))
          
         (theindex (indice theinput)))
    (setf (dropbox defvalitem) dropzone)
    (setf (editbox defvalitem) defval-scroller)
    
    (let ((v (eval (defval theinput))))
      (when (omclass-p (class-of (class-of v)))
        (setf (instance-p defvalitem) t)
        (setf (droppedval dropzone) v)
        (setf (icn dropzone) (get&corrige-icon (icon (class-of v))))
        )
      )

    (om-set-bg-color (editor dialog) *azulito*)
    (om-set-bg-color dialog *azulito*)
    
    (om-add-subviews (editor dialog) doc-scroller defval-scroller dropzone
                     (om-make-dialog-item 'om-static-text (om-make-point 3 30) (om-make-point 80 18) 
                                          "Default value" 
                                          :bg-color *azulito*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text (om-make-point 107 30) (om-make-point 100 18) 
                                          "Documentation" 
                                          
                                          :bg-color *azulito*
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-static-text (om-make-point 5 2) (om-make-point 200 22)
                                          (format () "~D: ~D" theindex (or (frame-name theinput) (name theinput))) 
                                          
                                          :font *om-default-font3b* 
                                          :bg-color *azulito*)
                     (om-make-dialog-item 'om-button (om-make-point 10 126) (om-make-point 80 18) "Apply"
                                          :di-action (om-dialog-item-act item 
                                                       (editor-set-help-action (editor dialog) (om-dialog-item-text doc-scroller))
                                                       (editor-set-defval-action (editor dialog)
                                                                                 (if (instance-p defvalitem) (droppedval dropzone)
                                                                                     (om-dialog-item-text defval-scroller))
                                                                                 (instance-p defvalitem))
                                                       ))
                     )
    dialog))



(defmethod get-editor-class ((self OMTypedIn)) 'InputEditor)

(defun set-input-typed-dialog (thein)
  (let* ((dialog (make-editor-window 'inputEditor thein (or (frame-name thein) (name thein)) nil
                                    :winpos :centered
                                    :winsize (om-make-point 160 308)
                                    :resize nil))
        (doc-scroller (om-make-dialog-item 'om-text-edit-view (om-make-point 7 194) (om-make-point 140 80)
                                           (docu thein) 
                                           :save-buffer-p t
                                           :scrollbars :v
                                           :font *om-default-font2*
                                           :wrap-p t ))
        (defvalitem (make-instance 'defval-item :object thein))
        (defval-scroller (om-make-dialog-item 'defval-editbox
                                              (om-make-point 7 102) (om-make-point 140 30) 
                                              (format () "~S" (eval (defval thein)))
                                              :item defvalitem
                                              :font *om-default-font2*
                                              :allow-returns t
                                              :scrollbars :v
                                              :help-spec "double click to edit initform <command> to see"
                                              ))
        (dropzone (om-make-view 'defval-drop 
                                 :position (om-make-point 7 137) 
                                 :size (om-make-point 124 34)
                                 :item defvalitem
                                 :bg-color *om-white-color*
                                 :object thein))
          (thekey (keys thein)))

    (setf (dropbox defvalitem) dropzone)
    (setf (editbox defvalitem) defval-scroller)
    
    (let ((v (eval (defval thein))))
      (when (omclass-p (class-of (class-of v)))
        (setf (instance-p defvalitem) t)
        (setf (droppedval dropzone) v)
        (setf (icn dropzone) (get&corrige-icon (icon (class-of v))))
        )
      )

    (om-set-bg-color (editor dialog) *azulito*)
    (om-set-bg-color  dialog *azulito*)
    (om-add-subviews (editor dialog) doc-scroller defval-scroller dropzone
                                        (om-make-dialog-item 'om-static-text (om-make-point 7 5) (om-make-point 160 24) 
                                                                                  (format () "~D: ~D" (indice thein) (or (frame-name thein) (name thein)))  
                                                                                  :bg-color *azulito*
                                                                                  :font *om-default-font3b*)
                                        
                                        (om-make-dialog-item 'om-static-text (om-make-point 7 24) (om-make-point 60 16) "Keys"  
                                                                                  :font *om-default-font2b* :bg-color *azulito*)
                                        (om-make-dialog-item 'om-radio-button (om-make-point 7 40) (om-make-point 80 18)
                                                                                  "standard "
                                                                                  :di-action (om-dialog-item-act item
                                                                                   (setf (keys (object (om-view-container item))) nil))
                                                                                  :checked-p (null thekey)
                                                                                  :enable t
                                                                                  :bg-color *azulito*)
                                        (om-make-dialog-item  'om-radio-button (om-make-point 7 60) (om-make-point 80 18) "&optional"
                                                                                  :di-action (om-dialog-item-act item
                                                                                   (reset-type (object (om-view-container item)))
                                                                                   (setf (keys (object (om-view-container item))) '&optional))
                                                                                  :checked-p (equal thekey '&optional)
                                                                                  :enable t
                                                                                  :bg-color *azulito*)
                                        (om-make-dialog-item 'om-radio-button (om-make-point 90 40) (om-make-point 80 18) "&rest       "
                                                                                  :di-action (om-dialog-item-act item
                                                                                   (reset-type (object (om-view-container item)))
                                                                                   (setf (keys (object (om-view-container item))) '&rest))
                                                                                  :checked-p (equal thekey '&rest)
                                                                                  :enable t
                                                                                  :bg-color *azulito*)
                                        (om-make-dialog-item 'om-radio-button (om-make-point 90 60) (om-make-point 80 18) "&key        "
                                                                                  :di-action (om-dialog-item-act item
                                                                                   (reset-type (object (om-view-container item)))
                                                                                   (setf (keys (object (om-view-container item))) '&key))
                                                                                  :checked-p (equal thekey '&key)
                                                                                  :enable t
                                                                                  :bg-color *azulito*)
                                        
                                        (om-make-dialog-item 'om-static-text (om-make-point 7 84) (om-make-point 120 18) "Default Value" 
                                                                                  :font *om-default-font2b* :bg-color *azulito*)
                                        
                                        (om-make-dialog-item 'om-button 
                                                             (om-make-point 40 278) (om-make-point 80 18) 
                                                             "Apply" 
                                                             :di-action (om-dialog-item-act item
                                                                          (editor-set-help-action (editor dialog) (om-dialog-item-text doc-scroller))
                                                                          (editor-set-defval-action (editor dialog)
                                                                                                    (if (instance-p defvalitem) (droppedval dropzone)
                                                                                                        (om-dialog-item-text defval-scroller))
                                                                                                    (instance-p defvalitem))))
    
                                        (om-make-dialog-item 'om-static-text  (om-make-point 7 176)(om-make-point 120 18) "Documentation"
                                                                                  :font *om-default-font2b* :bg-color *azulito*)
                                        
                                        )
    dialog))
