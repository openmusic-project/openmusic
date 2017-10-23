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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================


;;; UTILITY SUPERCLASSES FOR OBJECTS IN OM PROGRAMS

(in-package :om)

(defclass named-object ()
  ((name :initform nil :initarg :name :accessor name)))

(defmethod set-name ((self named-object) newname)
  (setf (name self) newname))

(defmethod get-name ((self named-object))
  (name self))

(defmethod set-name ((self t) newname) nil)
(defmethod get-name ((self t)) nil)



(defclass object-in-box ()
  ((associated-box :initform nil :accessor associated-box)))
  
(defmethod (setf value) :after ((value object-in-box) (self OMBoxEditCall)) 
  (setf (associated-box value) self))

(defmethod notify-change ((self object-in-box))
  (when (associated-box self)
    (update-if-editor (associated-box self))))
