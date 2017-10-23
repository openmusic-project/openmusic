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


;==========================================
;Some tools for algebraic operations
;==========================================

;--------------------------------------------
;True if n is a period of R

(defun is-period? (n R)
  "True if n is a period of R.
For example 3 is a period of '(4 5 7  4 5 7  4 5 7)."
  (when (integerp (/ (length R) n))
    (let ((period (first-n R n))
          (rep t))
      (loop while (and R rep) do
            (if (not (equal period (first-n R n)))
              (setf rep nil))
            (setf R (nthcdr n R)))
      rep)))

;--------------------------------------------
;get the generareur of R

(defmethod* get-min-period ((R list))
  :initvals '(nil) 
  :indoc '("the list")
  :icon 420
  :doc  "It finds the minimum sublist that generates a given list R by repetition.
For example for R = '(  4 5 8 4 5 8 4 5 8 4 5 8) the answer is '( 4 5 8 )."
  (let (rep)
    (loop for i from 1 to (length R)
          while (not rep) do
          (if (is-period? i R)
            (setf rep (first-n R i))))
    rep))



;--------------------------------------------
;t if m and n are relativily primes.

(defmethod relativelyPrimes ((m rational) (n rational))
   "Two numbers are relativily primes if the pgcd of the factors of m and n is 1."
   (let ((primesM (prime-facts m))
         (primesN (prime-facts n)))
     (null (intersection primesN primesM :key 'first))))



;=========================================
; Plus modulo n
(defmethod* mod+ ((self integer) (num integer) (n integer)) 
  :initvals '(0 0) :indoc '("first input" "second input")
  :icon 420
  :doc "Sum of two of numbers or trees modulo n."
  (second (multiple-value-list (floor (+ self num) n))))


(defmethod* mod+ ((self number) (num list) n)  
  (mapcar #'(lambda (input)
              (mod+ self input n)) num))

(defmethod* mod+ ((self list) (num number) n)   
  (mapcar #'(lambda (input)
              (mod+  input num n)) self))

(defmethod* mod+ ((self list) (num list) n)
  (mapcar #'(lambda (input1 input2)
              (mod+ input1 input2 n)) self num))

;=========================================
; Minus modulo n
(defmethod* mod- ((self integer) (num integer) (n integer)) 
  :initvals '(0 0) :indoc '("first input" "second input")
  :icon 420
  :doc "Sum of two of numbers or trees modulo n."
  (second (multiple-value-list (floor (- self num) n))))


(defmethod* mod- ((self number) (num list) n)  
  (mapcar #'(lambda (input)
              (mod- self input n)) num))

(defmethod* mod- ((self list) (num number) n)   
  (mapcar #'(lambda (input)
              (mod-  input num n)) self))

(defmethod* mod- ((self list) (num list) n)
  (mapcar #'(lambda (input1 input2)
              (mod- input1 input2 n)) self num))


;=========================================
; Times modulo n
(defmethod* mod* ((self integer) (num integer) (n integer)) 
  :initvals '((0 1 2 3) 7 12) :indoc '("first input" "second input" "n")
  :icon 420
  :doc "Multiplication of two numbers or trees modulo n."
  (second (multiple-value-list (floor (* self num) n))))


(defmethod* mod* ((self number) (num list) n)  
  (mapcar #'(lambda (input)
              (mod* self input n)) num))

(defmethod* mod* ((self list) (num number) n)   
  (mapcar #'(lambda (input)
              (mod*  input num n)) self))

(defmethod* mod* ((self list) (num list) n)
  (mapcar #'(lambda (input1 input2)
              (mod* input1 input2 n)) self num))

;=========================================

(defun divise-p (m n)
  (integerp (/ n m)))


