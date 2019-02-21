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

(in-package :om)

(defvar *basic-package* (omNG-protect-object (omNG-make-new-package "Basic Tools" :doc "Objects and tools for data representation and processing.")))

; LISTS
(defvar *list-package* (omNG-protect-object (omNG-make-new-package "List Processing")))
(AddGenFun2Pack  '(last-elem last-n first-n x-append flat create-list expand-lst 
                             mat-trans group-list remove-dup subs-posn interlock list-modulo
                             list-explode list-filter table-filter band-filter range-filter posn-match) *list-package*)
(AddPackage2Pack  *list-package* *basic-package*)

;;; ARITHMETICS
(defvar *arith-package* (omNG-protect-object (omNG-make-new-package "Arithmetic")))
(AddGenFun2Pack  '(om+ om- om* om/ om// om^ om-e om-abs om-min om-max
                   list-min list-max om-mean om-log om-round om-scale om-scale^ om-scale/sum reduce-tree
                   interpolation factorize om-clip om-random perturbation
                   om-cos om-sin
                   ) *arith-package*)
(AddPackage2Pack *arith-package* *Basic-package*)

;;; COMBINATORIAL
(defvar *comb-package* (omNG-protect-object (omNG-make-new-package "Combinatorial")))
(AddGenFun2Pack '(sort-list rotate nth-random permut-random posn-order permutations) *comb-package*)
(AddPackage2Pack *comb-package* *basic-package*)

;;; SERIES
(defvar *numser-package* (omNG-protect-object (omNG-make-new-package "Series")))
(AddGenFun2Pack '(arithm-ser geometric-ser fibo-ser inharm-ser prime-ser prime? x->dx dx->x) *numser-package* )
(AddPackage2Pack *numser-package* *basic-package*)

;;; SETS
(defvar *set-package* (omNG-protect-object (omNG-make-new-package "Sets")))
(AddGenFun2Pack  '(x-union x-intersect x-Xor x-diff included?) *set-package*)
(AddPackage2Pack *set-package* *basic-package*)


;;; FUNCTIONS
(defvar *function-package* (omNG-protect-object (omNG-make-new-package "Curves & Functions")))

(AddGenFun2Pack '(point-pairs om-sample           
                              x-transfer y-transfer
                              om-spline
                              linear-fun bpf-interpol
                              reduce-points reduce-n-points
                              bpf-offset bpf-crossfade bpf-extract bpf-scale bpf-concat
                              om-rotate om-translate om-mirror
                              ) *function-package*)
; removed : board
(AddClass2Pack '(bpf bpf-lib bpc bpc-lib) *function-package*     
               :position (list (om-make-point 75 115) (om-make-point 75 225) (om-make-point 150 115) (om-make-point 150 225) (om-make-point 150 25)))
(AddPackage2Pack *function-package* *basic-package*)


;;; DATA STRUCTURES
(defvar *basic-data-package* (omNG-protect-object (omNG-make-new-package "Array")))
(AddGenFun2Pack '(new-comp get-comp comp-list comp-field add-comp remove-comp) *basic-data-package*)
(AddClass2Pack '(class-array) *basic-data-package* :position (list (om-make-point 175 50)))
(AddPackage2Pack *basic-data-package* *basic-package*)

;;; FILE BOX / TEXTFILE
(defvar *file-package* (omNG-protect-object (omNG-make-new-package "Text")))
(addpackage2pack *file-package* *basic-package*)
(AddClass2Pack '(TextFile) *file-package*)
(addgenfun2pack '(eval-textfile save-data) *file-package*)



;;; PICT
(defvar *graphics-package*  (omNG-protect-object (omNG-make-new-package "Picture")))

(AddClass2Pack '(picture) *graphics-package*)
(AddGenFun2Pack '(save-picture) *graphics-package*)

(addPackage2Pack *graphics-package* *basic-package*)


;;; IN OM ROOT
(AddPackage2Pack *basic-package* *om-package-tree*)

(add-ref-section (gen-ref-entries *basic-package*))


