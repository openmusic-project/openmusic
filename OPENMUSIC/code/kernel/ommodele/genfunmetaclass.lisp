;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors:     Gerardo M. Sarria M., Jose Fernando Diago
;              Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;This file defines the classes for the OMFuncallableObject meta-object.
;Last Modifications :
;26/03/2003 first date.
;DocFile


(in-package :om)

;------------------------------------------------------------------------------
; OM Funcallable Object
;------------------------------------------------------------------------------
(defclass OMfuncallableObject (standard-generic-function) 
  ((name :initform nil :initarg :name :accessor name))
  (:metaclass funcallable-standard-class))

(defmethod omNG-rename ((self OMfuncallableObject) name)
  "This method changes the name of the object self with the new name NAME"
  (setf (name self) name))

;------------------------------------------------------------------------------
; OM Funcallable Basic Objects
;------------------------------------------------------------------------------
(defclass OMfuncallableBasicObject (OMfuncallableObject)
  ((icon :initform nil :initarg :icon :accessor icon)
   (args :initform nil :initarg :args :accessor args)
   (frames :initform nil :accessor frames)
   (EditorFrame :initform nil :accessor EditorFrame)
   (attached-objs :initform nil :accessor attached-objs)
   (protected-p :initform nil :initarg :protected-p :accessor protected-p)
   (infowin :initform nil :accessor infowin))
  (:metaclass funcallable-standard-class))

(defmethod allow-alias ((self OMfuncallableBasicObject))
  "T if SELF allows an alias the default is nil"
  nil)

(defmethod get-documentation ((self OMfuncallableBasicObject))
  "Return a string with the doc of the object"
  "An OMBasicObject")

(defmethod get-elements ((self OMfuncallableBasicObject))
  "The default value is NIL" 
  nil)

(defmethod get-class-icon ((self OMfuncallableBasicObject)) 
  "The default class for simple frames is 'icon-finder'."
  'icon-finder)

(defmethod get-object-insp-name ((self OMfuncallableBasicObject))
  "To redefine."
  "OM Basic Obj")

(defmethod omG-change-icon ((self OMfuncallableBasicObject)  new-icon)
  "Changes the icon ID of SELF with the new ID NEW-ICON, and update the current frames"
  (unless new-icon (setf new-icon (choise-icon)))
  (when new-icon
    (setf (icon self) new-icon)
    (mapcar #'(lambda (frame)
		(omg-change-icon frame new-icon)) (frames self))
    (mapcar #'(lambda (obj)
		(omg-change-icon obj new-icon)) (attached-objs self))))

(defmethod omG-rename ((self OMfuncallableBasicObject) new-name)
  "This method changes graphicly the name of SELF with the new name NEWNAME, and updates the current frames"
  (when (EditorFrame self)
    (omG-rename (EditorFrame self) new-name))
  (omNG-rename self new-name))

(defmethod omNG-add-element ((self OMfuncallableBasicObject) elem)
  "Method to specialize" 
  (declare (ignore elem))
  self)

(defmethod omNG-change-container ((self OMfuncallableBasicObject) oldcont newcont)
  "Method to specialize" 
  self)

(defmethod omNG-copy ((self OMfuncallableBasicObject))
  "This method is called by patches, folders, etc."
  `(make-instance ',(class-name (class-of self))
		  :name ,(name self)
		  :icon ,(copy-icon (icon self))))

(defmethod omNG-delete ((self OMfuncallableBasicObject)) t)

(defmethod omNG-remove-element ((self OMfuncallableBasicObject) elem)
  "Method to specialize" 
  self)

(defmethod omNG-protect-object ((self OMfuncallableBasicObject))
  "A general method for all OMBasicObjects"
  (setf (protected-p self) t)
  self)

(defmethod OpenObjectEditor ((self OMfuncallableBasicObject))
  "If there is a EditorFrame open for SELF select the window of EditorFrame, else create a new Editor frame, and select its window."
   (setf (EditorFrame self) (OpenEditorframe self))
  (when (EditorFrame self)
    (om-select-window (window (Editorframe self))))
  )

(defmethod get-name-icon ((self OMfuncallableBasicObject))
  (name self))


(defmethod kernel-p ((self OMfuncallableBasicObject))
  (protected-p self))

(defmethod name-exist-p ((self OMfuncallableBasicObject) name)
  (find-if #'(lambda (x) (string-equal name (name x))) (get-elements self)))

(defmethod metaObject-p ((self OMfuncallableBasicObject)) t)

(defmethod get-real-container ((self OMfuncallableBasicObject))
   (when (mypathname self)
     (let ((path (get-relative-path self)) object)
       (when path
         (setf object *current-workSpace*)
         (setf path (cdr path))
         (loop for item in path do
               (let ((elements (get-elements object)))
                 (setf object (find-if #'(lambda (x) (string-equal (name x) item)) elements))
                 (unless object
                   (setf path nil))))
         object))))
