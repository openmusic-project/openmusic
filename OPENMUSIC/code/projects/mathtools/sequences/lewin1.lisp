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


(defmethod* ifunc ((set1 list) (set2 list) (mod integer)) 
  :initvals '((0 3 7) (0 4 7) 12) :indoc '("first set" "second set" "modulo")
  :icon 424
  :doc "It computes Lewin intervallic function of two sets, i.e. the multiplicity of any given interval i from i=0 to i=11 between couples of elements of the two sets. For example the IFUNC between the C minor set (0 3 7) and the C major set (0 4 7) is equal to (2 1 0 0 2 1 0 1 0 2 0 0) since the two sets do have 2 notes in common etc. If the two sets are the same, IFUNC gives a complete version of the interval vector by Allen Forte."
  (loop for i from 0 to (- mod 1) collect
        (let ((repi 0))
          (loop for x in set1 do
              (loop for y in set2 do
                    (when (= (mod- y x mod) i)
                      (setf repi (+ repi 1)))))
          repi)))
          
                    


(defmethod* inj ((set1 list) (set2 list) fun) 
  :initvals '((0 4 7) (2 5 7 11) #'(lambda (x) (mod+ x 7 12)))
  :indoc '("first set" "second set" "function")
  :icon 424
  :doc "The injection number of two sets set1 and set2 with respect to a given function fun gives the number of elements x of set1 which are transformed into elements of set2 via the function fun. "
  (let ((rep 0))
    (loop for x in set1 do
          (when (member (funcall fun x) set2)
            (setf rep (+ rep 1))))
    rep))


(defmethod* inj-transp ((set1 list) (set2 list) k mod) 
  :initvals '((0 4 7) (2 5 7 11) 7 12)
  :indoc '("first set" "second set" "k" "mod")
  :icon 424
  :doc "The injection number of two sets set1 and set2 with respect to a given transposition Tk by k semitones gives the number of elements x of set1 which are transformed into elements of set2 via Tk."
  (let ((rep 0))
    (loop for x in set1 do
          (when (member (mod+ k  x mod) set2)
            (setf rep (+ rep 1))))
    rep))


(defmethod* J ((set list))
  :initvals '((9 11 2 8 10 9))
  :indoc '("set or a list of sets")
  :icon 424
  :doc "na na na"
  
  (if (listp (car set))
    (loop for el in set collect (j el))
    
    (let* ((ss (sort. set))
           (nd (remove-duplicates ss))
           (int (x->dx (append nd (om+ 12 (first-n nd 2)))))
           (i3 (position 3 int)))
      (nth 0 int)
      set))
  )