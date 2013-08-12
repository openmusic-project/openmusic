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
;OMBasicTYpe class is used to simulate Building Classes in lisp, i.e. integer, list, etc.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;--------------------------------
;A class for the Basic Lisp types
;--------------------------------
#|
(defclass OMBasictype (OMBasicObject)
   ((defval :initform nil :initarg :defval :accessor defval))
   (:documentation "This class implements building class in Common Lisp
because we can not inherite for building class we use delegation.
There are not all basic classes from lisp, you can add eassy new basic types if
they have a building class correspondant, for this see the function initbasic-lisp-types.#enddoc#
#seealso# (OMBasicObject) #seealso#
#defval# The default value for the Basic Type.#defval#"))
|#

;Return the name of 'SELF' as a string.
(defmethod class-name ((self OMBasictype))
   (interne (name self)))

(defmethod get-object-insp-name ((self OMBasictype)) "Building type")
(defmethod get-documentation ((self OMBasictype))
   (string+ "This is the Building OM class " (name self)))

(defmethod get-class-icon ((self OMBasictype)) 'type-icon-frame)

(defun omNG-make-new-basictype (name icon defval &optional (protected t))
   "This function constructs an OMBasicType instance (exemple :integer, string, etc.)."
   (make-instance 'OMBasictype :name name :icon icon :defval defval :protected-p protected))

;---------------------------
;Initialize the Lisp's Types
;---------------------------

 

(defun init-basic-lisp-types ()
   (setf *Basic-Lisp-Types* 
         (list (make-instance 'OMBasictype :name "t"        :icon 130 :defval t :protected-p t)
               (make-instance 'OMBasictype :name "integer"  :icon 128 :defval 0 :protected-p t)
               (make-instance 'OMBasictype :name "float"    :icon 131 :defval 0.0 :protected-p t)
               (make-instance 'OMBasictype :name "number"   :icon 132 :defval 0 :protected-p t)
               (make-instance 'OMBasictype :name "rational" :icon 133 :defval 1/2 :protected-p t)
               (make-instance 'OMBasictype :name "list"     :icon 129 :defval nil :protected-p t)
               (make-instance 'OMBasictype :name "string"   :icon 134 :defval "om" :protected-p t)
               (make-instance 'OMBasictype :name "null"     :icon 177 :defval nil :protected-p t))))

(init-basic-lisp-types)

(defmethod get-super-default-value ((type t))
  (let* ((type (if (equal type 'cons) 'list type))
         (basic-p (find-if #'(lambda (x) (equal (class-name x) type)) *Basic-Lisp-Types*)))
    (if basic-p
        (defval basic-p)
      (make-instance type :from-file t))))

(defun get-basic-type (type)
   "Search for 'type' in the basic type list (*Basic-Lisp-Types*)"
   (find-if #'(lambda (x) (equal (class-name x) type)) *Basic-Lisp-Types*))

;-----------------------------------------------
;Minimalistic checking...
;-----------------------------------------------

(defmethod check-type-p  ((self t) val) 
  (when (equal (class-name (class-of val)) self) t))

(defmethod check-type-p  ((self (eql 'integer)) val)
  (integerp val))

(defmethod check-type-p  ((self (eql 'float)) val)
  (floatp val))

(defmethod check-type-p  ((self (eql 't)) val)
  (declare (ignore val)) t)

(defmethod check-type-p  ((self (eql 'number)) val)
  (numberp val))

(defmethod check-type-p  ((self (eql 'rational)) val)
  (rationalp val))

(defmethod check-type-p  ((self (eql 'list)) val)
  (listp val))

(defmethod check-type-p  ((self (eql 'string)) val)
   (stringp val))

(defmethod check-type-p  ((self (eql 'null)) val)
   (null val))



