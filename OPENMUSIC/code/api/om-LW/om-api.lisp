;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================


;;===========================================================================
;DocFile
; OM API INITIALIZATION 
;DocFile
;;===========================================================================


(in-package :om-api)

(export '(om-api-init) :om-api)



;;; API INIT 
(defvar *api-init-func-list* nil)
(setf *api-init-func-list* nil)

(defun om-api-add-init-func (func-name)
   (unless (member func-name *api-init-func-list* :test 'equal)
        (push func-name *api-init-func-list*)))

(defun om-api-init ()
   (mapc 'funcall (reverse oa::*api-init-func-list*)))



;;; API external files :
;;(defvar *extern-files* nil)
;;(setf *extern-files* '())
;;(eval-when (eval compile load)
;;  (mapc #'compile&load *extern-files*))