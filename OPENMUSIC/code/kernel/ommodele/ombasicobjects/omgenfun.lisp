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
;This file defines the OMGenericfunction meta-object.
;Last Modifications :
;18/10/97 first date.
;DocFile


;;; !!! Attention fonction get-elements a arranger et tester

(in-package :om)

;---------------------------
;the Generic Functions in OM
;---------------------------
#|
(defclass OMGenericFunction (standard-generic-function OMBasicObject) 
   ((numouts :initform nil :accessor numouts)
    (inputs-default :initform nil :accessor inputs-default)
    (lib-fun-p :initform nil :accessor lib-fun-p)
    (inputs-doc :initform nil :accessor inputs-doc)
    (inputs-menus :initform nil :accessor inputs-menus))
   (:default-initargs :protected-p t)
   (:documentation "The generic function meta-object in OM. #enddoc#
#seealso# (ommethod) #seealso#
#numouts# Multiple values are allowed in OM, but it must be specified in the definition of the function. #numouts#
#inputs-default# This slot containt a list of default values for each arg in the function's lambda list. #inputs-default#
#lib-fun-p# Non nil, if the function was defined in a Library. #lib-fun-p#
#inputs-doc# This slot containt a list of string doc for each arg in the function's lambda list. #inputs-doc#
#inputs-menus# Some arg choose values from a pop-up-menu, this slot contains this information #inputs-menus#")
   (:metaclass omstandardclass))
|#

;--------------------------------------------------
;Method redefinition of OMBasicObject
;--------------------------------------------------
(defmethod get-documentation ((self OMGenericFunction))
   "Doc of generic function is managed by MCL."
   (documentation (function-name self) 'function))

(defmethod get-elements ((self OMGenericFunction))
   "Elements of a generic function are methods."
  (generic-function-methods self)
  )

(defmethod update-methods ((self omgenericfunction))
  (mapcar #'(lambda (obj)
              (unless (protected-p obj)
                (omng-save obj (mypathname obj))))
          (get-elements self)))

(defmethod get-class-icon ((self OMGenericFunction)) 'genfun-icon-frame)

(defmethod get-object-insp-name ((self OMGenericFunction)) "Generic Function")

(defmethod OpenEditorframe ((self OMGenericFunction))
   "Show a window with icon of methods"
   (or (editorframe self)
       (panel (make-show-methods-win self))))


(defmethod get-editor-class ((self OMGenericFunction)) 'GenericFunEditor)

;;; ajouté 60 au winsize...
(defmethod make-show-methods-win ((self OMGenericFunction))
   (let* ((themethods (get-elements  self))
          (thewindow (make-editor-window 'GenericFunEditor self (string+ "OM Generic Function - " (name self)) nil
                                         :winsize (om-make-point 300 (+ 60 (* (+ 1 (floor (length themethods) 2)) 70)))
                                         :winpos (om-mouse-position nil)
                                         ;:wintype '(:toolbox)
                                         ))
          (container (panel thewindow))
          (i -1))
     (setf (name container) (name self))
     (setf (object container) self)
     (mapc #'(lambda (elem)
               (om-add-subviews container (make-icon-from-method elem (om-make-point 10 (+ 10 (* (incf i) 50))))))
           (get-elements self))
     thewindow))


(defmethod omNG-change-container ((self OMGenericFunction) oldcont newcont)
   "Change the container of a generic function from the package 'oldcont' to the new package newcont.
change of container for generic functions can be made only in the user package."
   (setf oldcont (get-real-container self))
   (setf (functions oldcont) (remove self (functions oldcont) :test 'equal))
   (omNG-add-element newcont self)
   (mapc #'(lambda (el) 
             (when (mypathname el)
               (let ((newpath (make-pathname :directory (pathname-directory (corrige-pack-path newcont)) :name (method-name-ID el)
                                             :type (obj-file-extension el))))
                 (om-without-interrupts 
                   (rename-file (mypathname el) newpath))
                 (setf (mypathname el) newpath)))) (get-elements self))
   ;(upDateUsermenu)
   t)

(defmethod omng-remove-element ((self OMGenericFunction) elem)
  "Remove the method 'elem' from 'self"
  (make-remove-method elem ))

(defmethod set-doc ((self OMGenericFunction) newdoc)
   "Doc of generic function is managed by MCL."
   (unless (string-equal newdoc (get-documentation self))
     (setf (documentation self 'function) newdoc)
     (update-methods self)))

(defmethod omG-change-icon ((self OMGenericFunction) new-icon)
  (call-next-method)
  (update-methods self))

(defmethod genfun-om-attributes ((self OMGenericFunction) &optional attributes)
   "Get or set OM attributes of the generic function 'self'."
   (if attributes 
     (loop for item in attributes do
           (if (or (equal (first item) :documentation)
                   (equal (first item) :doc))
             (set-doc self (second item))
             (setf (slot-value self (interne (first item))) (second item))))
     (loop for item in *genfun-att-list*
           collect (list (string2initarg (string item)) (funcall item self)))))



;------------Other methods-----------------------------
(defmethod make-remove-function ((self OMGenericFunction))
   "Make unbound the generic function 'self' and delete all method files."
   (loop for item in (get-elements self) do
         (when (mypathname item)
           (om-delete-file (mypathname item))))         
   (fmakunbound (function-name self)))

;(defun aze () t)
;(defmethod! xxqq () t)
;(fboundp 'xxqq)
;(fmakunbound 'xxqq)

(defmethod OMGenfun-p ((self OMGenericFunction)) t)
(defmethod OMGenfun-p ((self t)) nil)

;(defmethod print-object ((self OMGenericFunction) x)
;  (if (stringp (name self))
;    (format x (string+ "Function " (name self)))
;    (call-next-method)))

;------------------Tools
;A list with only the parameter's names of the generic function.
(defun get-only-par-names (funname)
   (let ((lambda-lis (arglist funname)))
     (loop for item in lambda-lis
           when (not (or (equal item '&optional)
                         (equal item '&rest)
                         (equal item '&aux)
                         (equal item '&key)))
           collect (string item))))



