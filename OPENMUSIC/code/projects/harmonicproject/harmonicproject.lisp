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

;;; Harmonic project by J. Bresson, C. Truchet

(in-package :om)


;--------------------------------------------------
;Loading & compiling files 
;--------------------------------------------------


(defvar  *H-files* nil)

(setf *H-files* 
  '(
    "code;classes"
    "code;accords"
    "code;tools"

    "code;fonctions;marches-harmonie"
    "code;fonctions;arpeges"

    "code;interface;tools-data"
    "code;interface;interface-tonalite"
    "code;interface;score"

    ;"code;old-carlos;tonality"
    ;"code;old-carlos;arpege"
 
))



(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *H-files*))