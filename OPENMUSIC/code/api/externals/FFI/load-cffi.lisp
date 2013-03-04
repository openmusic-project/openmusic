;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;Macintosh version (Digitool Macintosh Common Lisp - MCL)
;
;Copyright (C) 2004 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Jean Bresson and Augusto Agon
;;===========================================================================



(in-package :oa)

;;;========================
;;; CFFI : FOPREIGN FUNCTIONS INTERFACE
;;;========================

(defvar *cffi-files* nil)

(setf *cffi-files* '(
                    "utils"
                    "features"
                    "cffi-lispworks"
                    "package"
                    "libraries"
                    "early-types"
                    "types"
                    "enum"
                    "strings"
                    "functions"
                    "foreign-vars"
                    ))

(defun load-cffi ()
  (mapc #'(lambda (filename) 
            (compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "CFFI")) :name filename))) 
        *cffi-files*))

(load-cffi)

;; redefinitions 
(defun om-write-ptr (ptr pos type value) 
  (cffi::%mem-set value ptr type pos))

(defun om-read-ptr (ptr pos type) 
  (cffi::%mem-ref ptr type pos))
 
;(setf aaa (om-make-pointer 5))
;(om-write-ptr aaa 1 :float 8.0)
;(om-read-ptr aaa 0 :float)

