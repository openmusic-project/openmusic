;=========================================================================
; OM API
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;Author: Paulo Henrique Raposo

(in-package :oa)

;;; Specializers on kernel classes (OMPersistantObject, OMPatchAbs, ...)
;;; live in projects/zoom/persistence.lisp.

(defgeneric get-win-zoom (self)
  (:documentation "Zoom factor associated with SELF for persistence; default 1.0."))

(defmethod get-win-zoom ((self t)) *om-zoom-default*)

(defgeneric set-win-zoom (self zoom)
  (:documentation "Store ZOOM factor on SELF for persistence; no-op by default."))

(defmethod set-win-zoom ((self t) zoom) (declare (ignore zoom)) nil)

(export '(get-win-zoom set-win-zoom) :oa)
