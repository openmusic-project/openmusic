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

;;; MATHTOOLS by C. Agon, M. Andreatta et al.

(in-package :om)


;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------
(defvar *Zn-files* nil)

(setf *Zn-files* '(
                   "circle;cercle"
                   "circle;cercle-analysis"

                   "sieves;cribles"


                   "screamer;package"
                   "screamer;screamer"
                   
                   "groups;tools"
                   "groups;zn;scream-groups"
                   "groups;zn;orbites"
                   "groups;zn;tl-zn"

                   "groups;dn;pcs"
                   "groups;dn;pcs-update"
                   
                   "groups;dn;ominterface"

                   "sequences;lewin1"
                   "sequences;suites"

                   ;"Polynomials;XXXX"
                   
                   "canons;vuza;canons"
                   "canons;noll;augcanons"
                   "canons;amiot;cylocanons"
                                               
                   "package"
                   ))

(push :screamer *features*)


;--------------------------------------------------
;Loading files 
;--------------------------------------------------


(eval-when (eval compile load)
  (mapc #'(lambda (filename) 
            (compile&load (namestring (make-local-path *load-pathname* filename)))) 
        *Zn-files*))


