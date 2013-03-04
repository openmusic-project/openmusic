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
;This file implements the meta-object OMBox.
;There are two main subclasses of OMBox : OMboxCall and OMBoxClass.
;The first ones are boxes in a Path (Maquette), the second ones are class-references in a class hierarchical tree.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)

;-------------------------------------------------
;The  OM Call Objects is the more general class
;subclass this classe to create a new connectobject
;--------------------------------------------------
#|
(defclass OMBox (OMBasicObject) 
   ((inputs :initform nil :initarg :inputs :accessor inputs)
    (reference :initform nil :initarg :reference :accessor reference)
    (frame-position :initform nil  :accessor frame-position)
    (frame-size :initform nil  :accessor frame-size)
    (frame-name :initform nil  :accessor frame-name))
   (:documentation "The OM meta-object OMBox is is the more general class for connectable objects.
There are two main type the connectable objects OMboxCall and OMBoxClass.
The first ones are boxes in a Path (Maquette), the second ones are class-references in a class hierarchical tree.#enddoc#
#seealso# (OMboxCall OMBoxClass) #seealso#
#inputs# A list of input instances objects see input-funbox class.#inputs#
#reference# The reference specifies the box's type, for exemple if the reference is a class the box is a factory of instances,
if the reference is a generic function the box is a call of generic function. Differents sub-classes of OMBox are defined
in relation with the reference.#reference#
#frame-position# Store the position of the graphic frame. #frame-position#
#frame-size# Store the size of the graphic simple frame. #frame-size#
#frame-name# Store the name of the graphic simple frame, it is not necessary the same as the box.#frame-name#")
   (:metaclass omstandardclass))
|#

;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------

;-------------------Protocole------------------------------
(defgeneric numouts (box)  
   (:documentation "Return the box outputs number."))

(defgeneric get-input-class-frame (box)  
   (:documentation "Specify the class of input's frame of the box."))

(defgeneric get-out-class (box)  
   (:documentation "Specify the class of output's frame of the box."))

(defgeneric get-frame-class (box)  
   (:documentation "Specify the class of the box's frame."))

(defgeneric get-frame-name (box)  
   (:documentation "get the name that will be showed in the box's frame."))

(defgeneric make-frame-from-callobj (box)  
   (:documentation "Cons the frame that visualize the object 'box'."))

(defgeneric remove-extra (patch box)  
   (:documentation "Called when you remove the box 'box' from the patch 'patch'."))

(defmethod omng-MoveObject ((self OMBox) newpos)
   (declare (ignore newpos))  t)

;------------------
(defmethod numouts ((self OMBox)) "The default value is 1" 1)
(defmethod get-input-class-frame ((self OMBox)) "The default value is 'input-funboxframe" 'input-funboxframe)
(defmethod get-out-class ((self OMBox)) "The default value is 'outfleche" 'outfleche)
(defmethod get-icon-box-class ((self OMBox)) 'icon-box)
(defmethod get-frame-class ((self OMBox)) "The default value is 'boxframe" 'boxframe)

(defmethod get-frame-name ((self OMBox))
   "NIL if the frame does not show a name (i.e. 'om+ function or editors)"
   (declare (special *function-without-name*))
   (unless (member (reference self) *function-without-name*)
     (let ((thename (string (reference self))))
       (string-downcase thename))))

(defmethod remove-extra ((self OMPatch) (box OMBox)) "Sub-class this method." nil)

;;; donne le rapport de la taille de la boite en fonction du nombre d'inputs/outputs
(defmethod boxinputs-sizefactor ((self OMBox)) 10)


(defmethod spec-obj-icon-size ((self t)) nil)

(defmethod def-icon-size ((self ombox)) 
  (if (member (reference self) *function-without-name*)
      (let ((icn (second (get&corrige-icon (icon self)))))
        (list (om-pict-width icn) (om-pict-height icn)))
    (or (spec-obj-icon-size (reference self))
        ;'(24 24)
        nil
        )))

(defmethod spec-input-frame ((self OMBox) index) nil)

(defmethod get-box-documentation ((self OMBox))
  (get-documentation self))

(defmethod make-frame-from-callobj ((self OMBox))
  "This method is used by multiple sub-classes grace of polymorphic function as
'get-frame-class, 'get-input-class-frame, etc. Differentes boxes are comments or editor redifine this method."
  (let* ((icon (icon self))
         (iconsize (icon-sizes icon (def-icon-size self)))
         (name (if (frame-name self) (frame-name self) (get-frame-name self)))
         (boxnamefont *ombox-font*)
         (numouts (numouts self))
         (index 0)
         (size-name (get-name-size name boxnamefont))
         (h-name (if name (+ 3 (om-string-h boxnamefont))0))
         input-frames module boxframex)
    
    (setf (inputs self) (update-inputs (reference self) (inputs self)))
    
    (setf boxframex  (if (frame-size self)
                         (om-point-h (frame-size self))
                       (apply #'max (list (first iconsize) 
                                          (* (boxinputs-sizefactor self) numouts) 
                                          (* (boxinputs-sizefactor self) (length (inputs self))) 
                                          size-name))))
        
    (setf input-frames (mapcar #'(lambda (input)   
                                   (let ((docstr (doc-string input)))
                                     (setf index (+ index 1))
                                     (om-make-view (or (spec-input-frame self (- index 1)) (get-input-class-frame self))
                                                   :object input
                                                   :help-spec (string+ "<" (string-downcase (name input))
                                                                       ">" (if (and docstr (not (string-equal docstr "")))
                                                                               (string+ " " (doc-string input)) ""))
                                                   :size (om-make-point 8 8)
                                                   :position (om-make-point (- (* index (round boxframex (+ (length (inputs self)) 1))) 4) 
                                                                            1)
                                                   )
                                     ))
                               (inputs self)))
    (setq module
          (om-make-view (get-frame-class self) 
                        :position (frame-position self)
                        ;;;:help-spec (get-documentation self)
                        :size  (om-make-point boxframex (+ (second iconsize) (+ 19 h-name)))
                        :object self
                        :subviews input-frames
                        ))
    (setf (inputframes module) input-frames)
    ;;;(loop for input-f in input-frames do (om-add-subviews module input-f))
    
    (make-outputs-of-frame self module)
    (setf (outframes module) (reverse (outframes module)))
    (setf (name module) name)
    (setf (frames self) (list module))
    (om-add-subviews module (setf (iconView module)
                                  (om-make-view (get-icon-box-class self)
                                                :iconID icon
                                                :help-spec (get-box-documentation self)
                                                :size (om-make-point (first iconsize) (second iconsize))
                                                :position (om-make-point (round (- (w module) (first iconsize)) 2) 10))))
    (when name
      (om-add-subviews module (setf (nameView module)
                                    (om-make-dialog-item 'box-dialog-name     
                                                         (om-make-point (+  (x (iconView module)) (round (- (first iconsize) size-name) 2))
                                                                        (- (h module) (+ 10 h-name)))                                 
                                                         (om-make-point size-name h-name)
                                                         name
                                                         :value name
                                                         :font boxnamefont
                                                         :help-spec (get-documentation self)
                                                         
                                                         ))))
    (when (allow-lock self)
      (add-lock-button module (allow-lock self)))
    (add-box-resize module)
    module))

;--------------------------------------------------
;Other methods
;--------------------------------------------------

(defmethod is-connected? ((self OMBox) (source OMBox))
   "T if one input of 'self' is connected to one output of 'source'."
   (let ((list (inputs source)) rep)
     (loop while list do
           (let ((connec (connected? (pop list))))
             (when (and connec (equal (first connec) self))
               (setf rep t)
               (setf list nil))))
     rep))


(defmethod unconnected ((self OMBox) (source OMBox))
   "T if  inputs of 'self' are not connected."
   (let ((list (inputs source)))
     (loop for input in list do
           (let ((connec (connected? input)))
             (when (and connec (equal (first connec) self))
               (setf (connected? input) nil))))))

(defmethod get-output-text ((self t) i) "option-click to evalue or drag for connections")



(defmethod make-outputs-of-frame ((self OMBox) module)
   "Cons a list of views which are the outputs of the box."
   (let ((numouts (numouts self)))
     (loop for i from 0 to (- numouts 1) do
           (let ((thenewout (om-make-view (get-out-class self)
                                              :position (om-make-point (- (* (+ i 1) (round (w module) (+ numouts 1))) 4) 
                                                         (- (h module) 9))
                                              :size (om-make-point 8 8)
                              :help-spec (get-output-text self i)
                              :index i)))
             (push thenewout (outframes module))
             (om-add-subviews module thenewout)))))


(defmethod omNG-make-alias ((self OMBox))
   "Generic function to make boxes alias the method 'allow-alias' filters some type of boxes 
(i.e. functions non allow alias)."
   (if (allow-alias self)
     (omNG-make-new-boxalias self (om-add-points (om-make-point 20 20) (frame-position self))
                             (string+ (name self) "-alias")) 
     (not (dialog-message (string+ 
                           (get-object-insp-name self)
                           " objects don't accept alias")))))

(defmethod omng-make-new-boxalias ((self OMBox) posi name &optional (protect nil))
   (let* ((rep (make-instance 'OMBoxAlias 
                 :name name
                 :reference self 
                 :icon (icon self)
                 :protected-p protect)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (frames self))
     (push rep (attached-objs (find-class (get-reference rep))))
     rep))

(defmethod omng-make-new-boxalias ((self OMClass) posi name &optional (protect nil))
   (let* ((rep (make-instance 'OMBoxAlias 
                 :name name
                 :reference self 
                 :icon (icon self)
                 :protected-p protect)))
     (setf (frame-position rep) (borne-position posi))
     (push rep (attached-objs self))
     rep))



