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

;;; -----------------------------------------------------------------------------
;;; pcs type fn-name
;;; -----------------------------------------------------------------------------

(defmethod! pc-set ((type symbol) (fn-name t))
   :icon 240
   :menuins '( (0 (("integer" :integer) ("vector" :vector) ("pitch" :pitch))))
   :initvals '(:integer '6-Z10)
   :doc "|Pitch-class, one of the 12 pitch-classes designated by integers 0 11. 
Pitch-class 0 refers to all
notated pitches C, B-sharp, D-double-flat. Pitch-class 1 refers to all notated pitches C-sharp,
D-flat, B-double-sharp , and so on.

type - :integer :vector :pitch

The name of pcs consisting of two numerals separated by a hyphen. The numeral to the left of the
hyphen is the cardinal-number of the set; the numeral to the right of the hyphen is the ordinal
number of the set, its position on the list of prime-forms.


(pcs :integer '6-Z10)
=> (0 1 3 4 5 7)

(pcs :vector '9-7)
=> (6 7 7 6 7 3)

(pcs :pitch '6-Z10)
=> (c c# d# e f g)
 
The final parameter is an fname-list and expects the composer to make explicit reference to the
Forte Notation name for a particular pitch-class set. Thus, the name 3-1 denotes a pitch class
set of 0,1,2 and the interval vector, 210000, an ordered array of numerals representing the
interval content of the pitch class set.

(pcs :integer '(3-1 3-8))
=> ((0 1 2) (0 2 6))

(pcs :vector '(3-1 3-8))
=> ((2 1 0 0 0 0) (0 1 0 1 0 1))

(pcs :pitch '(3-1 3-8))
=> ((c c# d) (c d f#))
"
   (pcs type fn-name))

;;; -----------------------------------------------------------------------------
;;; pcs-cardinal number
;;; -----------------------------------------------------------------------------

(defmethod! Dn-orbites ((self integer))
   :icon 240
   :initvals '(3)
   :doc "This function displays all the sets of the chosen cardinal-number (the number of element in a set).

(dn-orbites 3)
=> (3-1 3-2 3-3 3-4 3-5 3-6 3-7 3-8 3-9 3-10 3-11 3-12)

(dn-orbites 5)
=> (5-1 5-2 5-3 5-4 5-5 5-6 5-7 5-8 5-9 5-10 5-11 5-z12 5-13 5-14 5-15
    5-16 5-z17 5-z18 5-19 5-20 5-21 5-22 5-23 5-24 5-25 5-26 5-27 5-28
    5-29 5-30 5-31 5-32 5-33 5-34 5-35 5-z36 5-z37 5-z38)"
   (pcs-cardinal self))


;;; -----------------------------------------------------------------------------
;;; pcs-complement type low high fn-name
;;; -----------------------------------------------------------------------------

(defmethod! comp ((type symbol) (from integer) (to integer) (list list))
   :icon 240
   :menuins '( (0 (("integer" :integer) ("pitch" :pitch))))
   :initvals '(:integer 4 5  '(0 1 2 7))
   :doc "This function is able to produce an output of 'complementary' intervals to that of the pcs generation. 
If for example, A is a pitch-class-set containing 4 elements (pc integers),
then the complement of A is the set of 8 elements not contained in A.

type - :integer :pitch

(pcs :integer '4-6)
=> (0 1 2 7)

(pcs-complement :integer 4 5 (pcs :integer '4-6))
=> (3 4 7 8 9 10 11)

(pcs-complement :pitch 0 5 (pcs :integer '5-6))
=> (d# e g g# a a# b)

(pcs-complement :integer 0 6 (pcs :integer '(6-7 5-7 4-5 2-1)))
=> ((3 4 5 9 10 11) (3 4 5 8 9 10 11)
    (3 4 5 7 8 9 10 11) (2 3 4 5 6 7 8 9 10 11))"
   (pcs-complement type from to list))


;;; -----------------------------------------------------------------------------
;;; pcs-invert type fn-set
;;; -----------------------------------------------------------------------------

(defmethod! inv ((type symbol) (set list))
   :icon 240
   :menuins '( (0 (("integer" :integer) ("pitch" :pitch))))
   :initvals '(:integer '(0 1 2 7))
   :doc "The function pcs-invert enables an intervallic statement in symbols or integers to be inverted.

type - :integer :pitch

(pcs-invert :integer (pcs :integer '6-Z10))
=> (0 11 9 8 7 5)

(pcs-invert :pitch (pcs :integer '6-Z10))
=> (c b a g# g f)

(pcs-invert :integer (pcs :integer '9-7))
=> (0 11 10 9 8 7 5 4 2)

(pcs-invert :integer '((6 7 9 11 0 2 3) (1 3 6 9 0 11 4 7)))
=> ((6 5 3 1 0 10 9) (11 9 6 3 0 1 8 5))

(pcs-invert :pitch (pcs :integer '(5-1 5-8)))
=> ((c b a# a g#) (c a# a g# f#))

(pcs-invert :integer (pcs :integer '(3-1 3-8)))
=> ((0 11 10) (0 10 6))"
   (pcs-invert type set))


;;; -----------------------------------------------------------------------------
;;; pcs-normal-order type integer-list
;;; -----------------------------------------------------------------------------

(defmethod! n-ord ((type symbol) (set list))
   :icon 240
   :menuins '( (0 (("integer" :integer) ("pitch" :pitch))))
   :initvals '(:integer '(3 9 6 5 0))
   :doc "Normal order.
type - :integer  :pitch

(pcs-normal-order :integer '(3 8 9 6 1 2 0 10))
=> (6 8 9 10 0 1 2 3)

(pcs-normal-order :integer '(3 9 6 5 0))
=> (3 5 6 9 0)

(pcs-normal-order :integer '(0 3 4 5 10))
=> (10 0 3 4 5)

(pcs-normal-order :pitch '((3 9 6 5 0) (0 3 4 5 10)))
=> ((d# f f# a c) (a# c d# e f))"
   (pcs-normal-order type set))


;;; -----------------------------------------------------------------------------
;;; pcs-prime-form type integer-set
;;; -----------------------------------------------------------------------------
(defmethod! p-form ((type symbol) (set list))
   :icon 240
   :menuins '( (0 (("fn" :fn) ("integer" :integer) ("pitch" :pitch))))
   :initvals '(:integer '(3 9 6 5 0))
   :doc "Set in prime form.
type - :fn :integer :pitch

(pcs-prime-form :integer '(11 0 5 6))
=> (0 1 6 7)

(pcs-prime-form :fn '(11 0 5 6))
=> 4-9

(pcs-prime-form :fn '(10 1 3 4))
=> 4-13

(pcs-prime-form :fn '((10 11 1 3 4) (10 0 1 3 4)))
=> (5-z12 5-10)"
   (pcs-prime-form type set))


;;; -----------------------------------------------------------------------------
;;; pcs-sub-power type set-low set-high set
;;; -----------------------------------------------------------------------------
(defmethod! sub-power ((type symbol) (low integer) (high integer) (set list))
   :icon 240
   :menuins '( (0 (("fn" :fn) ("integer" :integer) ("pitch" :pitch))))
   :initvals '(:integer 3 3 '(11 0 5 6))
   :doc "type - :fn :integer :pitch

(pcs-sub-power :integer 3 3 '(11 0 5 6))
=> ((11 0 5) (11 0 6) (11 5 6) (0 5 6))

(pcs-sub-power :fn 2 3 '(11 0 5 6))
=> (2-5 2-6 2-1 3-5)

(pcs-sub-power :integer 3 3 '(10 0 1 3 4))
=> ((10 0 1) (10 0 3) (10 0 4) (10 1 3) (10 1 4)
    (10 3 4) (0 1 3) (0 1 4) (0 3 4) (1 3 4))

(pcs-sub-power :fn 3 4 '(10 0 1 3 4))
=> (3-8 3-7 3-10 3-5 3-3 3-2 4-10 4-12 4-z15 4-13 4-3)

(pcs-sub-power :integer 4 4 '((10 11 1 3 4) (10 0 1 3 4)))
=> (((10 11 1 3) (10 11 1 4) (10 11 3 4) (10 1 3 4) (11 1 3 4))
    ((10 0 1 3) (10 0 1 4) (10 0 3 4) (10 1 3 4) (0 1 3 4)))"
   (pcs-sub-power type low high set))


;;; -----------------------------------------------------------------------------
;;; pcs-sub-prime-form type set-low set-high set
;;; -----------------------------------------------------------------------------
(defmethod! sub-p-form ((type symbol) (low integer) (high integer) (set list))
   :icon 240
   :menuins '( (0 (("fn" :fn) ("integer" :integer) ("vector" :vector) ("pitch" :pitch))))
   :initvals '(:integer 3 3 '(11 0 5 6))
   :doc "type - :fn :integer :vector :pitch

(pcs-sub-prime-form :integer 3 3 '(11 0 5 6))
=> ((0 1 6) (0 5 6))

(pcs-sub-prime-form :fn 3 3 '(11 0 5 6))
=> 3-5

(pcs-sub-prime-form :fn 3 3 '(10 1 3 4))
=> (3-7 3-10 3-5 3-2)

(pcs-sub-prime-form :vector 3 3 '(10 1 3 4))
=> ((0 1 1 0 1 0) (0 0 2 0 0 1) (1 0 0 0 1 1) (1 1 1 0 0 0))

(pcs-sub-prime-form :fn 4 4 '((10 11 1 3 4) (10 0 1 3 4)))
=> ((4-8 4-13 4-11) (4-10 4-12 4-z15 4-13 4-3))"
   (pcs-sub-prime-form type low high set))


;;; -----------------------------------------------------------------------------
;;; pcs-sub-relation type low high sets
;;; -----------------------------------------------------------------------------

(defmethod! sub-rel ((type symbol) (low integer) (high integer) (set list))
   :icon 240
   :menuins '( (0 (("fn" :fn) ("integer" :integer) ("vector" :vector) ("pitch" :pitch))))
   :initvals '(:integer 3 3 '((10 11 1 3 4) (10 0 1 3 4)))
   :doc "type - :fn :integer :vector :pitch
(pcs-sub-relation :fn 3 4 '((10 11 1 3 4) (10 0 1 3 4)))
=> (3-7 3-10 3-5 3-2 4-13)

(pcs-sub-relation :integer 3 4 '((10 11 1 3 4) (10 0 1 3 4)))
=> ((0 2 5) (0 3 6) (0 1 6) (0 1 3) (0 1 3 6))

(pcs-sub-relation :fn 3 3 '(5-10 7-3))
=> (3-5 3-2 3-8 3-10 3-7 3-3)"
   (pcs-sub-relation type low high set))


;;; -----------------------------------------------------------------------------
;;; pcs-subcomplex type low high fn-name
;;; -----------------------------------------------------------------------------

(defmethod! sub-complex ((type symbol) (low integer) (high integer) (fn-name symbol))
   :icon 240
   :menuins '( (0 (("fn" :fn) ("integer" :integer) ("pitch" :pitch))))
   :initvals '(:fn 3 3 '4-6)
   :doc "type - :fn :integer :pitch

(pcs-subcomplex :fn 3 3 '4-6)
=> (3-1 3-5 3-9)

(pcs-subcomplex :integer 3 3 '4-6)
=> ((0 1 2) (0 1 6) (0 2 7))

(pcs-subcomplex :pitch 3 3 '4-6)
=> ((c c# d) (c c# f#) (c d g))

(pcs-subcomplex :fn 3 4 '4-25)
=> (3-8)

(pcs-subcomplex :fn 3 6 '4-25)
=> (3-8 5-15 5-28 5-33 6-7 6-21 6-22 6-30 6-34 6-35)

(pcs-subcomplex :vector 3 3 '6-z3)
=> ((2 1 0 0 0 0) (1 1 1 0 0 0)
    (1 0 1 1 0 0) (1 0 0 1 1 0)
    (1 0 0 0 1 1) (0 2 0 1 0 0)
    (0 1 1 0 1 0) (0 1 0 1 0 1)
    (0 0 2 0 0 1))"
   (pcs-subcomplex type low high fn-name))


;;; -----------------------------------------------------------------------------
;;; pcs-transpose type transp-value set
;;; -----------------------------------------------------------------------------

(defmethod! transp ((type symbol) (value integer) (set list))
   :icon 420
   :menuins '( (0 ( ("integer" :integer) ("pitch" :pitch))))
   :initvals '(:integer 3 '(0 1 3 4 5 7))
   :doc "The function pcs-transpose enables an intervallic statement in symbols or integers to be
transposed in accordance with the protocol attached to the transposition of pitch class sets.

type - :integer :pitch

(pcs-transpose :integer 3 (pcs :integer '6-Z10))
=> (3 4 6 7 8 10)

(pcs-transpose :pitch 3 (pcs :integer '6-Z10))
=> (d# e f# g g# a#)
 
Multiple lists may be processed by writing the transp-value for each list in its own list, thus: 

(pcs-transpose :integer '(3 11) (pcs :integer '(5-1 5-8)))
=> ((3 4 5 6 7) (11 1 2 3 5))

(pcs-transpose :integer 4 '(0 1 2 4 5 7 8))
=> (4 5 6 8 9 11 0)

(pcs-transpose :integer '(0 6 11) (pcs :integer '(5-1 5-8 6-Z10)))
=> ((0 1 2 3 4) (6 8 9 10 0) (11 0 2 3 4 6))"
   (pcs-transpose type value set))



;;; -----------------------------------------------------------------------------
;;; End
;;; -----------------------------------------------------------------------------


;===================

(defun summation (n k)
  (let ((pgcd (pgcd n k)))
    (loop for j from 1 to pgcd
          when (integerp (/ pgcd j)) sum (* (euler j) (binomial (/ n j) (/ k j))))))

(defmethod! Dn-card ((n integer) (k integer))
  :initvals '(12 6) :indoc '("Zn" "elments")
  :doc "Nombre d'ensembles de classes de hauteurs ayant k elements a une transposition et une inversion pres. 
Par exemple il y a 38 gammes de sept notes a l'interieur du total chromatique a une transposition et/ou une inversion pres." 
  :icon 240
  (cond
   ((oddp n)
    (* (/ 1 (* 2 n)) (+ (summation n k) 
                        (* n (binomial (/ (- n 1) 2) (floor (/ k 2)))))))
   ((and (evenp n) (evenp k))
    (* (/ 1 (* 2 n)) (+ (summation n k) 
                        (* n (binomial (/ n 2) (/ k 2))))))
   ((and (evenp n) (oddp k))
    (* (/ 1 (* 2 n)) (+ (summation n k) 
                        (* n (binomial (- (/ n 2) 1) (floor (/ k 2)))))))))
