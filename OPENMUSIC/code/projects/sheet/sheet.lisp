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
;===========================================================================

; SHEET package by C. Agon & J. Bresson

(in-package :om)


;--------------------------------------------------
;Loading & compiling files 
;--------------------------------------------------


(defvar  *sheet-files* nil)

(setf *sheet-files* 
  '(
        "sheetobjects;sheet"
        ;"sheetobjects;sheet-persistant"
        "sheetobjects;sheet-patch"
        
        "sheeteditor;sheet-window"
        "sheeteditor;sheet-patch-editor"
        "sheeteditor;sheet-spaces"
        "sheeteditor;sheet-special-cases"
        ;"sheeteditor;sheetpages"

        "sheetobjects;sheet-analysis"
        
))
 


(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *sheet-files*))


(defvar *sheetpackage* (omNG-protect-object (omNG-make-new-package "Sheet")))
(defvar *sheettoolspackage* (omNG-protect-object (omNG-make-new-package "Sheet Tools")))
(addPackage2Pack *sheetpackage* *scorepackage*)
(addPackage2Pack *sheettoolspackage* *sheetpackage*)

(AddClass2Pack '(OMSheet) *sheetpackage*)
(AddClass2Pack '(sheet-track sheet-track-obj sheet-access) *sheettoolspackage*)

(push :omsheet *features*)

;; re-generate the score section
(add-ref-section (gen-ref-entries *scorepackage*))