;;===========================================================================
;LW Score Editors 
;Interface tools for score editing and inspection 
;;===========================================================================

;===========================================================================
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; See file LICENSE for further informations on licensing terms.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; Author: Karim Haddad
;;===========================================================================

;;===========================================================================
;DocFile
;This file loads the LW Score Editors
;;===========================================================================

(defpackage "OM-EDIT"
  (:use "COMMON-LISP" "CL-USER" "CAPI" "LISPWORKS" "OM-LISP"))

(in-package :om-edit)
 
(defvar *lw-score-edit-directory* nil)
(setf *lw-score-edit-directory* (pathname-directory (truename *load-pathname*)))


(mapc #'(lambda (filename) (om-lisp::compile-if-needed-and-load 
                            (make-pathname :directory *lw-score-edit-directory* 
                                           :name filename))) 
      '("treeeditor" "tempoeditor" "infoeditor" "treetempoeditor" "commenteditor"))






