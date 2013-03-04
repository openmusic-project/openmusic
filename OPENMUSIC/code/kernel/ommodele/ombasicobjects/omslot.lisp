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
;This file defines the class OMSLOT.
;We can not use the meta-programmation technique, for slot extension because there is not a slot class in MCL.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om) 

#|
(defclass omslot (ombasicobject)
   ((classname  :initarg :classname :accessor classname)
    (thetype :initarg :thetype :accessor thetype)
    (theinitform :initarg :theinitform :accessor theinitform)
    (alloc  :initarg :alloc :accessor alloc)
    (slot-package  :initarg :slot-package :accessor slot-package)
    (io-p  :initarg :io-p :accessor io-p)
    (doc  :initarg :doc :accessor doc))
   (:documentation "Instance of this class allow define graphicly slot of omClasses. #enddoc#
#seealso# (omclass OMBasicObject) #seealso#
#classname# A name with the class which contains the slot. #classname#
#thetype# OM slots are typed, the default type if T, however, there are not type checking in OM. #thetype#
#theinitform# Initform value for the slot. #theinitform#
#alloc# 'class or 'instance #alloc#
#io-p# Public or private slot flag. #io-p#")
   (:metaclass omstandardclass))
|#

;--------------------------------------------------
;Method redefinition of OMBasicObject
;--------------------------------------------------
(defmethod get-documentation ((Self Omslot))
   "In MCL slot do not have documentation, we generate a simple string."
    (doc self)  )

(defmethod set-doc ((Self Omslot) Newdoc)
   (setf (doc self) newdoc)
   (set-slot-doc (find-class (classname self)) (name self) newdoc)
   (resave-class (find-class (classname self))))

(defmethod get-class-icon ((Self Omslot)) 'slot-icon-frame)

(defmethod get-object-insp-name ((Self Omslot)) "Slot")

(defmethod omng-copy ((Self Omslot)) "You can not copy a slot" (om-abort))

(defmethod omng-rename ((Self Omslot) Name)
   "Slots have special rename method, because as we define reader/writer slots methods, 
all names are not allowed i.e. 'car'."
   (ignore-errors
    (let* ((Theclass (find-class (classname self)))
           (Theslots (get-elements theclass))
           (Pos (position  self theslots :test #'(lambda (slot slot1) (string-equal (name slot) (name slot1))))))
      (setf (name (nth pos theslots)) name)
      (redef-class theclass (make-super-class-list theclass) (make-slot-list theslots))
      (nth pos theslots))))

(defmethod openeditorframe ((Self Omslot)) 
   "Slot do not have editor."
   (not (dialog-message "Slots have no editor window.")))

(defmethod read-thetype ((self omslot))
  (or (thetype self) t))

;--------------------------------------------------
;Other methods
;--------------------------------------------------

(defmethod get-define-slot ((Self Omslot))
   "Cons a lisp expression to define a slot in a class."
   (if (io-p self)
     (list (internp (name self) (slot-package self)) ':initform  (theinitform self)  
           ':initarg (string2initarg (name self)) ':type (read-thetype self) ':allocation (alloc self)
           ':documentation (doc self)
           ':accessor (internp (name self) (slot-package self)))
     (list (internp (name self) (slot-package self)) ':initform (theinitform self) ':type (read-thetype self) 
           ':allocation (alloc self) 
           ':documentation (doc self)
           ':accessor (internp (name self) (slot-package self)))))

(defmethod omg-change-type-slot ((Self Omslot) Type)
   "Set the slot's type to 'type' and redefine the class containing the slot 'self'."
   (let* ((Theclass (find-class (classname self)))
          (Theslots (get-elements theclass)) 
          (Pos (position  self theslots :test #'(lambda (slot slot1) (string-equal (name slot) (name slot1)))))
          Newclass)
     (setf (thetype (nth pos theslots)) type)
     (setf (theinitform (nth pos theslots)) (omng-copy (get-super-default-value  type)))
     (setf newclass (redef-class theclass (make-super-class-list theclass) (make-slot-list theslots)))
     (nth pos theslots)))


(defmethod omg-change-initarg ((Self Omslot) Val)
   "If val is T the slot is an initarg of the class else it is graphicly privated."
   (let* ((Theclass (find-class (classname self)))
          (Theslots (get-elements theclass))
          (Pos (position  self theslots :test #'(lambda (slot slot1) (string-equal (name slot) (name slot1)))))
          Newclass)
     (setf (io-p (nth pos theslots)) val)
     (setf newclass (redef-class theclass (make-super-class-list theclass) (make-slot-list theslots)))
     newclass))

(defmethod omg-change-allocation ((Self Omslot) Val)
   "Set the slot's allocation to val an redefine the class containing the slot 'self'."
   (unless (equal (alloc self) val)
     (if (not (protected-p self))
       (let* ((Theclass (find-class (classname self)))
              (Theslots (get-elements theclass))
              (Pos (position  self theslots :test #'(lambda (slot slot1) (string-equal (name slot) (name slot1))))))
         (setf (alloc (nth pos theslots)) val)
         (if (equal val :class)
           (setf (io-p (nth pos theslots)) nil))
         (redef-class theclass (make-super-class-list theclass) (make-slot-list theslots))
         (nth pos theslots))
       (om-beep-msg "This class is protected"))))

(defmethod omg-change-initform ((Self Omslot) Val)
   "Set the slot's default value to 'val' and redefine the class containing the slot 'self'."
   (let* ((Theclass (find-class (classname self)))
          (Theslots (get-elements theclass))
          (Pos (position  self theslots :test #'(lambda (slot slot1) (string-equal (name slot) (name slot1)))))
          (Alloc (alloc (nth pos theslots)))
          Newclass)
     (setf (theinitform (nth pos theslots)) (omng-copy val))
     (setf newclass (redef-class theclass (make-super-class-list theclass) (make-slot-list theslots)))
     (when (equal alloc ':class)
       (setf (slot-value (make-instance theclass) (internp (name self) (slot-package self))) val))
     newclass))

;A slot is (un) protected if its class is (un) protected
(defmethod protected-p ((Self Omslot)) 
   (protected-p (find-class (classname self))))

;-----------Slot builder
  
(defun omng-make-new-slot (Classname Name Type Alloc Initform Io? &optional (Package :om))
   "Make an OMslot instance."
   (declare (special *OM-class-list*))
   (let ((*Package* (find-package package))
         (Thetype (exist-class-p (string type)))
         Icon)
     (unless thetype
       (setf thetype (exist-class-p "t") ))
     (setf icon (icon (nth thetype (concatenate 'list *OM-class-list* *Basic-Lisp-Types*))))
     (make-instance *def-metaclass-slot*
       :classname classname
       :name name
       :icon icon
       :thetype type
       :io-p io?
       :doc  (if (find-class classname nil) (find-slot-doc (find-class classname nil) name) "no doc")
       :slot-package package
       :theinitform (if (symbolp initform) `',initform initform)
       :alloc alloc)))


;------
;Tools
;------
;--Make a list of OMSlots from the instance slots and 
;--the class slots list (listc listi)                                                     
(defun make-slots (Classname Listc Listi)
   (concatenate 'list 
                (mapcar #'(lambda (Slot)
                            (omNG-make-new-slot classname (string-downcase (string (slot-definition-name slot)))
                                                (slot-definition-type slot) ':class (slot-definition-initform slot) 
                                                (not (null (slot-definition-initargs slot))) (package-name (symbol-package (slot-definition-name slot))))) listc)
                (mapcar #'(lambda (Slot)
                            (omNG-make-new-slot classname (string-downcase (string (slot-definition-name slot)))
                                                (slot-definition-type slot) ':instance (slot-definition-initform slot)
                                                (not (null (slot-definition-initargs slot))) (package-name (symbol-package (slot-definition-name slot))))) listi)))


;--Only OMclasses in the CPL
(defun get-om-superclasses (Class)
  (let ((Thelist (get-class-precedence-list class)))
    (remove-if #'(lambda (Class) 
                   (not (omclass-p  (class-of class)))) thelist)))

;--Get all OMSlots of the class named class
(defun get-all-slots-of-class (Class)
   (let* ((Class-List (omcpl (find-class class)))
          Theslottc Theslotti )
     (mapcar #'(lambda (Superclass)
                 (setf theslottc (remove-duplicates (concatenate 'list theslottc (class-direct-class-slots superclass))
                                                    :test #'(lambda (x y) (equal (slot-definition-name x) (slot-definition-name y)))))
                 (setf theslotti (remove-duplicates (concatenate 'list theslotti (class-direct-instance-slots superclass))
                                                    :test #'(lambda (x y) (equal (slot-definition-name x) (slot-definition-name y)))))
               )
             (reverse class-list))
    (make-slots class theslottc theslotti)))

;--Get only the direct OMSlots of the class named class
(defun get-direct-slots-of-class (Class)
   (let* ((Myclass (find-class class))
          (Theslottc (class-direct-class-slots myclass))
          (Theslotti (class-direct-instance-slots myclass)))
     (make-slots class theslottc theslotti)))

;--Get a list of slot's name and type for the class named class
(defun get-init-slots-of-class (Class)
   (mapcar #'(lambda (Slot)
               (list (internp (name slot) (symbol-package class)) (read-thetype slot))) (get-all-slots-of-class class)))

;--Get a list of the direct OMslot of the class class
(defun get-direct-initargs-of-class (Class)
   (let (Rep)
     (mapc #'(lambda (Slot)
               (when (io-p slot)
                 (push slot rep))) (get-direct-slots-of-class class))
     (reverse rep)))

;--Get a list of the direct and inherited OMslot of the class class
(defun get-all-initargs-of-class (Class)
   (let (Rep)
     (mapc #'(lambda (Slot)
               (when (io-p slot)
                 (push slot rep))) (get-all-slots-of-class class))
     (reverse rep)))

;--All initargs without class allocation slots
(defun get-all-instance-initargs (Class)
   (let (Rep)
     (mapc #'(lambda (Slot)
               (when (io-p slot)
                 (push slot rep))) (get-all-slots-instances class))
     (reverse rep)))

;--Get all instance allocation OMSlots of the class named class
(defun get-all-slots-instances (Class)
   (let* ((Class-List (omcpl (find-class class)))
          Theslotti )
     (mapcar #'(lambda (Superclass)
                (setf theslotti (remove-duplicates (concatenate 'list theslotti (class-direct-instance-slots superclass))
                                                    :test #'(lambda (x y) (equal (slot-definition-name x) (slot-definition-name y)))))) 
             (reverse class-list))
    (make-slots class nil theslotti)))




;---Used to construct list '(slotname slottype) for all initargs
(defun get-initargs-of-class (Class)
   (mapcar #'(lambda (Slot)
               (list (internp (name slot) (symbol-package class)) (read-thetype slot))) (get-all-instance-initargs class)))

;include doc
(defun get-initargs-docs (Class)
   (mapcar #'(lambda (Slot)
               (doc slot)) (get-all-initargs-of-class class)))

;---Cons a list with '(:initarg initval) for all slots. 

(defun make-slot-init-list (Class Args)
   (let ((I 0) Rep)
     (loop for slot in (get-all-slots-instances class) do
           (when (io-p slot) 
             (push (string2initarg  (name slot)) rep)
             (push `',(nth i args) rep)
             (incf i)))
     (reverse rep)))

;---Code lisp generation for class definition.
(defun make-slot-list (Slots)
   (mapcar #'(lambda (Slot) (get-define-slot slot)) slots))

;---Return a list with the initform of the slot 'name' in the class 'class'
(defun get-defval-for-slot (Class Name)
   (let (Rep)
     (loop for item in (get-all-initargs-of-class (type-of class))
           while (not rep) do
           (when (string-equal (name item) name)
             (setf rep (eval (theinitform item)))))
     rep))


(defun slot-names (Class)
   (loop for item in (get-all-initargs-of-class class)
           collect  (name item)))