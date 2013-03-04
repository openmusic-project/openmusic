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
;This file implements the metaobject OMBoxClass.
;They are boxes in a hierarchical tree.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

                

;*******************************************************************************************
;*******************************************************************************************
;Boxes in a hierchical tree of classes.
;*******************************************************************************************
;*******************************************************************************************


;-----------------------------------------------------------------
;Class Boxes
;-----------------------------------------------------------------

(defclass OMBoxClass (OMBox) ()
   (:documentation "This is the class for the boxes in a hierarchical tree class editor.#enddoc#
#seealso# (OMClass classboxframe) #seealso#")
   (:metaclass omstandardclass))

(defmethod OMBoxClass? ((self OMBoxClass)) t)
(defmethod OMBoxClass? ((self t)) nil)

;----------------------------------------------
;Methods redefinition
;----------------------------------------------
(defmethod get-object-insp-name ((self OMBoxClass)) "class")
(defmethod allow-alias       ((self OMBoxClass)) 
   "For the momment Class boxes are the only objects that alow alias." 
   t)
;Non lock button for class boxes.
(defmethod allow-lock        ((self OMBoxClass))nil)

(defmethod get-frame-class   ((self OMBoxClass)) 
   "Frame class for class boxes is 'classboxframe." 
   'classboxframe)

(defmethod get-out-class     ((self OMBoxClass)) 
   "Output's frames for class boxes are instances of 'outflecheclass." 
   'outflecheclass)

(defmethod get-input-class-frame   ((self OMBoxClass)) 
   "Input's frames for class boxes are instances of 'input-classframe." 
   'input-classframe)

(defmethod get-documentation ((self OMBoxClass))  
   "Documentation of a box class is the same as the class reference."
   (documentation (find-class (reference self)) t))

(defmethod get-reference     ((self OMBoxClass))  
   "The reference of a box class is an OMClass."
   (reference self))

(defmethod OpenEditorframe ((self OMBoxClass))
   "The editor of a box class is the editor of its class reference."
   (OpenObjectEditor (find-class (get-reference self))) nil)

;----------------------------------------------
;Other methods
;----------------------------------------------
(defmethod correct-connection ((self OMBoxClass) list pack)
   "When you add a class to a package and the package does not contain all superclass list of the new class, 
this method create alias and put these in the package." 
  (let ((superclasses (make-super-class-list (find-class (get-reference self)))))
    (mapc #'(lambda (name)
              (let ((classsource (find-super-class-box name list)))
                (when (not classsource)
                  (let* ((newbox (omNG-make-new-boxclass name (om-make-point 22 22)))
                         (alias (omNG-make-new-boxalias newbox (om-make-point 22 22) (string-downcase (string name))
                                                        (protected-p (find-class (get-reference self))))))
                    (push alias (aliasclasses pack))
                    (setf classsource  alias)))
                (connect-class self classsource))) superclasses)))

(defmethod connect-class ((target OMBoxClass) (source OMBoxClass))
   "The class 'target' inherites from the class 'source'."
   (let ((new-input (make-instance 'input-funbox
                      :name ""
                      :value nil
                      :doc-string "")))
     (setf (connected? new-input) (list source 0 nil))
     (setf (inputs target)
           (list+ (inputs target) (list new-input)))))


(defmethod connect-define-class ((self OMBoxClass))
   "Once the new superclass of 'self' is putted in the inputs slots, 
this method redefine the class with the new superclass list."
   (handler-bind ((error #'(lambda (c) 
                                               
                                               (error-in-connection self)
                                               
                             (om-message-dialog (string+ "Error in class definition" 
                                                                                    (om-report-condition c))
                                             :size (om-make-point 300 200))
                             (om-abort))))
     (let ((theclass (find-class (get-reference self)))
           slots super-list)
       (mapc #'(lambda (input)
                 (push (get-reference (first (connected? input))) super-list)) (inputs self))
       (setf super-list (reverse super-list))
       (setf slots (make-slot-list (get-elements theclass)))
       (redef-class theclass super-list slots))))

(defmethod unconnect-class  ((self OMBoxClass) index)
   "The class 'self' does not inherite more from the 'index' element of the superclass list."
   (let ((theclass (find-class (get-reference self)))
         slots super-list)
     (setf (inputs self) (remove (nth index (inputs self)) (inputs self) :test 'equal))
     (mapc #'(lambda (input)
               (push (get-reference (first (connected? input))) super-list)) (inputs self))
     (setf super-list (reverse super-list))
     (setf slots (make-slot-list (get-elements theclass)))
     (redef-class theclass super-list slots)))


(defmethod error-in-connection ((self OMBoxClass))
   "If there is an error in a class box connection (inheritance problem), this method is called
in order to redefine the class reference of 'self'."
   (setf (inputs self) (butlast (inputs self)))
   (let ((theclass (find-class (get-reference self)))
         slots super-list)
     (mapc #'(lambda (input)
               (push (get-reference (first (connected? input))) super-list)) (inputs self))
     (setf super-list (reverse super-list))
     (setf slots (make-slot-list (get-elements theclass)))
     (redef-class theclass super-list slots)))

(defmethod dead-reference ((self OMBoxClass))
   "When you delete a class, this method redefine all sub-classes and remove the box class from the hierarchical tree."
   (when (frames self)
     (om-close-window (om-view-window  (car (frames self)))))
   (let ((packa (get-real-container (find-class (reference self)))))
     (when packa
       (let* ((boxes (classeswin packa))
             (subclasses (get-subclasses-list self boxes)))
         (loop for item in subclasses do
               (setf (inputs (first item)) (remove (nth (second item) (inputs (first item))) 
                                               (inputs (first item)) :test 'equal)))
     (setf (classeswin packa) (remove self (classeswin packa) :test 'equal))))))

(defmethod omNG-box-value ((self OMBoxClass) &optional (num-out 0))
   "Class boxes are not valuables."
   (declare (ignore num-out))  nil)

(defmethod get-subclasses-list ((self OMBoxClass) frames)
   "Return a list with the class boxes connected to 'self'."
  (let (rep)
    (loop for item in frames do
          (loop for input in (inputs item) do
                (when (and (connected? input) (equal (first (connected? input)) self))
                  (push (list item (second (connected? input))) rep))))
    rep))

(defmethod update-from-reference ((self OMBoxClass) &optional (udt? t))
   (declare (ignore udt?)) nil)

;----------------------------------------------
;Tools
;----------------------------------------------
;----Builder
(defun omNG-make-new-boxclass (class posi)
   "Cons a new box class having as reference the class 'class' and place it at 'posi'."
   (let* ((theclass (find-class class))
          (rep (make-instance 'OMBoxClass 
                 :name (string class)
                 :reference class 
                 :icon (icon theclass))))
     (setf (frame-position rep) (borne-position posi))
     (push rep (attached-objs theclass))
     rep))

(defun find-super-class-box (name list)
  (find-if #'(lambda (box) (equal (get-reference box) name)) list))



;------------ALIAS-----------------------

(defclass OMBoxAlias (OMBoxcall)  ()
   (:documentation "Alias from boxes are instance of this class.#enddoc#
#seealso# (OMbox) #seealso#"))

(defmethod numouts ((self OMBoxAlias)) 
  ;(numouts (reference self))
  1)

(defmethod get-frame-class ((self OMBoxAlias)) 'aliasboxframe)

(defmethod get-out-class ((self OMBoxAlias)) 
  ;(get-out-class (reference self))
  'outflecheclass)

(defmethod get-frame-name ((self OMBoxAlias)) (string-downcase (string (name (reference self)))))
(defmethod get-documentation ((self OMBoxAlias)) (string+ "This is an alias of the box " (name (reference self))))

(defmethod get-object-insp-name ((self OMBoxAlias)) 
  (string+ "alias to "  (get-object-insp-name (reference self))))

(defmethod OpenEditorframe ((self OMBoxAlias)) 
  (OpenObjectEditor (reference self)) nil)

(defmethod omNG-box-value ((self OMBoxAlias) &optional (numout 0))
  (omNG-box-value (reference self) numout))

(defmethod get-reference ((self OMBoxAlias))
   "The reference of an alias box class is the reference of the original box."
   (if (omclass-p (reference self)) (reference self)
     (get-reference (reference self))))

(defmethod unconnect-class  ((self OMBoxAlias) index)
  (unconnect-class (reference self) index))


(defmethod do-add-one-input ((self OMBoxAlias)) nil)
(defmethod do-add-all-inputs ((self OMBoxAlias)) nil)
(defmethod do-delete-one-input ((self OMBoxAlias))  nil)

(defmethod do-add-one-keyword ((self OMBoxAlias) &optional (input-key nil)) nil)
(defmethod do-delete-one-keyword ((self OMBoxAlias))  nil)

(defmethod get-subclasses-list ((self OMBoxAlias) frames)
  (let (rep)
    (loop for item in frames do
          (loop for input in (inputs item) do
                (when (and (connected? input) (equal (first (connected? input)) self)) 
                  (push (list item (second (connected? input))) rep))))
    rep))

(defmethod update-from-reference ((self OMBoxAlias) &optional (udt? t))
   (declare (ignore udt?)) nil)

(defmethod dead-reference ((self OMBoxAlias))
   "Called when the original of the  box alias 'self' is deleted."
  (when (frames self)
    (om-close-window (om-view-window (car (frames self)))))
  (let* ((packa (get-real-container self))
         (boxes (classeswin packa))
         (subclasses (get-subclasses-list self boxes)))
    (loop for item in subclasses do
          (setf (inputs (first item)) (remove (nth (second item) (inputs (first item))) 
                                              (inputs (first item)) :test 'equal)))
    (omng-remove-element packa self)))


(defmethod connect-class ((target OMBoxClass) (source OMBoxAlias))
   "The class 'target' inherites from the original class of the alias 'source'."
   (let ((new-input (make-instance 'input-funbox
                      :name ""
                      :value nil
                      :doc-string "")))
     (setf (connected? new-input) (list source 0 nil))
     (setf (inputs target)
           (list+ (inputs target) (list new-input)))))






   












