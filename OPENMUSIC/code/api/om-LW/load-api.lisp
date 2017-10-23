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

;;===========================================================================
;DocFile
;This file define the *api-files* global variable, which contains a list
;with the OM-API file's pathnames.
;DocFile
;;===========================================================================

(in-package :cl-user)

(load (make-pathname :directory (append (pathname-directory (truename *load-pathname*)) '("lw-lisp-tools")) :name "load-lw-lisp-tools" :type "lisp"))

(defpackage "OM-API"
  (:nicknames "OA")
  (:use "COMMON-LISP" "CL-USER" "OM-LISP" "CAPI" "LISPWORKS" "GP"))

(in-package :oa)

(defvar *api-directory* nil)
(setf *api-directory* (pathname-directory (truename *load-pathname*)))
 
(defvar *api-files* nil)
(setf *api-files* '(
                    "om-api"
                    "mop"
                    "system"
                    "tools"
                    "graphics"
                    "graphic-object"
                    "window"
                    "view"
                    "item-view"
                    "draw"
                    "actions"
                    "transient-drawing"
                    "dialog-items"
                    "icons-picts"
                    "menu"
                    "om-text-editor"
                    "user-interface"
                    "draganddrop"
                    "tooltips"
                    "print"
                    "libraries"                 
                    "om-special"  
                    
                    ))

(mapc #'(lambda (filename) (compile&load (make-pathname :directory *api-directory* :name filename))) *api-files*)



(pushnew :om-api *features*)

