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
;This file defines the class OMLispFun  used to handle lisp functions in OM.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

#|
(defclass OMLispFun (OMBasicObject) 
   ((funname :initform nil :initarg :funname :accessor funname))
   (:documentation "This class implements the lisp functions.#enddoc#
#seealso# (OMBoxlispCall) #seealso#
#funname# The symbol name of the function.#funname#"))
|#

(defun omNG-make-new-lispfun (name)
  "this function allox construct an OMLispFun instance from a symbol function name"
  (make-instance 'OMLispFun :name (string name) :funname name :icon 144))

(defmethod get-object-insp-name ((self OMLispFun)) "Lisp Function")

(defmethod get-elements ((self OMLispFun))
   "Lisp function don't have elements" nil)

(defmethod get-class-icon ((self OMLispFun)) 'lispfun-icon-frame)

(defmethod OpenEditorframe ((self OMLispFun))
   "You can not visualize lisp funtions"
   (not (dialog-message (format nil "This is a compiled Lisp Function. It can not be open in graphical editor."))))

(defmethod get-documentation ((self OMLispFun))
   (documentation (funname self) 'function))


