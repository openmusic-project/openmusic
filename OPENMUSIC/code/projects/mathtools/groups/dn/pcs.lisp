
;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
;;;
;;; Copyright © 1996 Janusz Podrazik
;;; Copyright © 2001 MRaC Publishing Limited
;;; All rights reserved.
;;;
;;; File: pitch-class-set.lisp 
;;;
;;; Author: Janusz Podrazik <podrazik@mracpublishing.com>
;;;
;;; Created: 96-04-15
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted, so long as the following
;;; conditions are met:
;;;      - no fees or compensation are charged for use, copies, or
;;;        access to this software
;;;      - this copyright notice is included intact.
;;; This software is made available AS IS, and no warranty is made about 
;;; the software or its performance. 
;;;
;;;
;;; Purpose: Allen Forte's unique notation system commonly known as
;;;          Forte Notation has come to be recognised as one of the most
;;;          successful attempts at providing a means of describing and
;;;          analysing musical pitch structures that defy interpretation as
;;;          tonal or 12-note serial systems. Thus, Forte's system is equally
;;;          at home describing Ives and Stravinsky as it is Schoenberg and
;;;          Webern.
;;;
;;;
;;; References: Forte, A., "The Structure of Atonal Music" Yale University (1973) 
;;;
;;;
;;; Modification History:
;;; date      modification
;;; ============================================================
;;; 1996-04-15  Created
;;; 1997-02-20  Relese as a module for Symbolic Composer.
;;; 1998-03-15  First relese of the pitch-class-set code.
;;; 2001-02-01  correction to pcs 7-20, fix function requirement-test
;;; 2001-09-05  fix to pcs-normal-order, pcs-prime-form-to-fn,
;;;             pcs-i and pcs-v.
;;; 2001-12-04  requirement-test fix

;(in-package :cl-user)
(in-package :om)

;;; -----------------------------------------------------------------------------
;;; gen-integer
;;; -----------------------------------------------------------------------------

(defun GEN-INTEGER (l h)
  "(gen-integer 3 8)"
  (loop for i from l to h
        collect i))

;;; -----------------------------------------------------------------------------
;;; position-select
;;; -----------------------------------------------------------------------------

(defun POSITION-SELECT (position sequence)
  "(position-select 3 '(a b c d e f g))"
  (prog (out element)
    (setq position
          (cond ((atom position) (list position))
                (t position)))
    loop
    (cond ((null position) (return (nreverse out))))
    (setq element (nth (car position) sequence))
    (setq position (cdr position))
    (setq out (cons element out))
    (go loop)))

;;; -----------------------------------------------------------------------------
;;; element-position
;;; -----------------------------------------------------------------------------

(defun ELEMENT-POSITION (n pattern)
  "(element-position 'c '(a b c a b c a))"
  (let ((out) (element))
    (dotimes (i (count n pattern :test #'equal))
      (setq element (position n pattern :test #'equal))
      (setq pattern (substitute '+ n pattern :test #'equal :count 1))
      (push element out))
    (nreverse out)))

;;; -----------------------------------------------------------------------------
;;; position-remove
;;; -----------------------------------------------------------------------------

(defun POSITION-REMOVE (position sequence)
  "(position-remove 3 '(a b c d e f))"
  (prog (part1 part2 position1 x)
    (setq position 
          (cond
           ((atom position) (list position))
           (t (sort-ascending position))))
    (setq position1 (car position))
    (setq x 0)
    (dotimes (i (length position))
      (setq part1 (subseq sequence 0 position1))
      (setq part2
            (cond ((< (length sequence) (+ position1 1)) nil)
                  (t (subseq sequence (+ position1 1) (length sequence)))))
      (setq sequence (append part1 part2))
      (setq position (cdr position))
      (setq position1
            (- (cond
                ((eq nil (car position)) 0)
                (t (car position))) (incf x))))
    (return sequence)))

;;; -----------------------------------------------------------------------------
;;; sort-ascending
;;; -----------------------------------------------------------------------------

(defun SORT-ASCENDING (set)
  "(sort-ascending '(4 2 5 1 3))"
  (prog (out element)
    loop
    (cond ((null set) (return (nreverse out))))
    (setq element
          (loop for i in set
                minimize i))
    (setq set (remove element set :count 1))
    (setq out (cons element out))
    (go loop)))

;;; -----------------------------------------------------------------------------
;;; element-substitute
;;; -----------------------------------------------------------------------------

(defun ELEMENT-SUBSTITUTE (new old list)
  "(element-substitute '(1 2 3) '(a b c) '(a b c c b a))"
  (sublis (mapcar 'cons old new) list))

;;; -----------------------------------------------------------------------------
;;; integer-to-pitch
;;; -----------------------------------------------------------------------------

(defun INTEGER-TO-PITCH (integer-list)
  "(integer-to-pitch '(0 6 1 7))"
  (element-substitute
   '(c c# d d# e f f# g g# a a# b)
   (gen-integer 0 11)
   integer-list))

;;; -----------------------------------------------------------------------------
;;; pitch-to-integer
;;; -----------------------------------------------------------------------------

(defun PITCH-TO-INTEGER (pitch-list)
  "(pitch-to-integer '(c f# c# g))"
  (element-substitute
   (gen-integer 0 11)
   '(c c# d d# e f f# g g# a a# b)
   pitch-list))

;;; -----------------------------------------------------------------------------
;;; pcs-flatten
;;; -----------------------------------------------------------------------------

(defun pcs-flatten (list)
  "(pcs-flatten '((1 2 3) (1 2 3)))"
  (prog (out)
    loop
    (cond ((null list) (return (nreverse out))))
    (cond ((or (listp (car list)))
           (setq out (append (nreverse (pcs-flatten (car list))) out)))
          (t (setq out (cons (car list) out))))
    (setq list (cdr list))
    (go loop)))

;;; -----------------------------------------------------------------------------
;;; append-sublist
;;; -----------------------------------------------------------------------------

(defun APPEND-SUBLIST (list)
  (loop for x in list append x))

;;; -----------------------------------------------------------------------------
;;; compress
;;; -----------------------------------------------------------------------------

(defun COMPRESS (l)
  "(compress '(4 - 19))"
  (intern (to-string l)))

(defun TO-STRING (l)
  (prog (out)
    (setq out "")
    loop
    (cond ((null l) (return out)))
    (setq out (str-cat out (conv-to-string (car l))))
    (setq l (cdr l))
    (go loop)))

(defun CONV-TO-STRING (s)
  (cond ((numberp s)  (make-real-string (prin1-to-string s)))
        (t (symbol-name s))))

(defun MAKE-REAL-STRING (s)
  (coerce (coerce s 'list) 'string))

(defun STR-CAT (&rest l)
  (cond ((null l) "")
        (t (string+ (car l) (apply 'str-cat (cdr l))))))

;;; -----------------------------------------------------------------------------
;;; power-set
;;; -----------------------------------------------------------------------------

(defVar *POWER-SET-SPECIAL* nil)

(defun GENERATE-POWER-SET1 (set)
  (cond ((null set) nil)
	((null (cdr set))
	 (unless (member set *power-set-special* :test #'equal)
	   (push set *power-set-special*)))
	(t (do ((this (cdr set) (cdr this))
		(that (car set) (car this))
		(rest nil)
		(current nil))
	       ((null that))
	     (setq current (append (reverse rest) this))
	     (unless (member current *power-set-special* :test #'equal)
	       (push current *power-set-special*)
	       (generate-power-set1 current))
	     (push that rest)))))

(defun POWER-SET (set)
  "(power-set '(1 2 3 4))"
  (setq *power-set-special* nil)
  (push set *power-set-special*)
  (generate-power-set1 set)
  *power-set-special*)

;;; -----------------------------------------------------------------------------
;;; pcs-cardinal
;;; -----------------------------------------------------------------------------

(defun PCS-BUILDER (cardinal set)
  "(pcs-builder 2 '(1 2 3 4 5 6))"
  (prog (out elem)
    loop
    (cond ((null set) (return (nreverse out))))
    (setq elem (compress (list cardinal '- (car set))))
    (setq out (cons elem out))
    (setq set (cdr set))
    (go loop)))

(defun PCS-CARDINAL (integer)
  "(pcs-cardinal 3)"
  (cond
   ((eq 2 integer) (pcs-builder 2 '(1 2 3 4 5 6)))
   ((eq 3 integer) (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11 12)))
   ((eq 4 integer) (pcs-builder 4 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 z15 16 17 18 19 20 21 22 23 24 25 26 27 28 z29)))
   ((eq 5 integer) (pcs-builder 5 '(1 2 3 4 5 6 7 8 9 10 11 z12 13 14 15 16 z17 z18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 z36 z37 z38)))
   ((eq 6 integer) (pcs-builder 6 '(1 2 z3 z4 5 z6 7 8 9 z10 z11 z12 z13 14 15 16 z17 18 z19 20 21 22 z23 z24 z25 z26 27 z28 z29 30 31 32 33 34 35 z36 z37 z38 z39 z40 z41 z42 z43 z44 z45 z46 z47 z48 z49 z50)))
   ((eq 7 integer) (pcs-builder 7 '(1 2 3 4 5 6 7 8 9 10 11 z12 13 14 15 16 z17 z18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 z36 z37 z38)))
   ((eq 8 integer) (pcs-builder 8 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 z15 16 17 18 19 20 21 22 23 24 25 26 27 28 z29)))
   ((eq 9 integer) (pcs-builder 9 '(1 2 3 4 5 6 7 8 9 10 11 12)))
   ((eq 10 integer) (pcs-builder 10 '(1 2 3 4 5 6)))))

;;; -----------------------------------------------------------------------------
;;; pcs-normal-order
;;; -----------------------------------------------------------------------------

(defun ASCENDING-FROM-ZERO (list)
  "(ascending-from-zero '(1 4 6 8 11))"
    (prog (elem result len number)
      (setq list (sort-ascending list))
      (setq len (length list))
      (setq number (car list))
      (dotimes (i len)
        (setq elem (- (car list) number))
        (setq result (cons elem result))
        (setq list (cdr list)))
      (return (nreverse result))))

(defun PERMUTE-CIRCULAR (set)
  "(permute-circular '(1 4 11 6 8))"
  (prog (count last out)
    (setq count (cdr set))
    (setq out (cons set out))
    loop
    (cond ((null count) (return (nreverse out))))
    (setq last (car set))
    (setq set (pcs-flatten (list (rest set) last)))
    (setq out (cons set out))
    (setq count (cdr count))
    (go loop)))

(defun PERMUTE-CIRCULAR-AND-ADD12 (set)
  "(permute-circular-and-add12 '(1 4 11 6 8))"
  (prog (count add out)
    (setq count (cdr set))
    (setq set (ascending-from-zero set))
    (setq out (cons set out))
    loop
    (cond ((null count) (return (nreverse out))))
    (setq add (+ (car set) 12))
    (setq set (pcs-flatten (list (rest set) add)))
    (setq out (cons set out))
    (setq count (cdr count))
    (go loop)))

(defun SUBTRACT-FIRST-FROM-NEXT-TO-LAST (set)
  "(subtract-first-from-next-to-last '(1 4 11 6 8))"
  (prog (first item out)
    (setq first (car set))
    (setq set (cdr set))
    loop
    (cond ((null set) (return (nreverse out))))
    (setq item (- (car set) first))
    (setq out (cons item out))
    (setq set (cdr set))
    (go loop)))

(defun SUBTRACT-FIRST-FROM-NEXT-TO-LAST~ (set)
  (mapcar #'(lambda (x)
              (subtract-first-from-next-to-last x)) set))

(defun REQUIREMENT-TEST (set)
  "(requirement-test '(1 4 11 6 8))"
  (prog (rotate set-perm all-diff least-last least-select
                position diff-next least-next least out)
    (cond ((eq 2 (length set)) (setq position (setq out (list (ascending-from-zero set)))))
          (t (setq rotate (permute-circular (ascending-from-zero set)))
             (setq set-perm (permute-circular-and-add12 set))
             (setq all-diff (subtract-first-from-next-to-last~ set-perm))
             (setq least-last (pcs-flatten (mapcar 'last all-diff)))
             (setq least-select (car (sort-ascending least-last)))
             (setq position (element-position least-select least-last))
             (setq out (position-select position rotate))
             (setq least-next (mapcar 'butlast all-diff))))
    loop
    (cond ((not (null (eq 1 (length position)))) (return (car out))))
    (setq diff-next (position-select position least-next))
    (setq least (mapcar 'first diff-next))
    (setq least-select (car (sort-ascending least)))
    (setq least-next (mapcar 'rest diff-next))
    (setq position (cond ((eq nil (car least-next)) '(0))
                         (t (element-position least-select least))))
    (setq out (position-select position out))
    (go loop)))

(defun PCS-NORMAL-ORDER-F1 (set)
  (requirement-test set))

(defun PCS-NORMAL-ORDER-F2 (set)
  (mapcar #'(lambda (x)
              (requirement-test x)) set))

(defun PCS-NORMAL-ORDER (type set)
  "(pcs-normal-order :integer '(1 4 11 6 8))"
  (prog (result out)
    (setq result
          (cond
           ((atom (car set)) (pcs-normal-order-f1 set))
           (t (pcs-normal-order-f2 set))))
    (setq out
          (cond
           ((eq type :pitch) (integer-to-pitch result))
           ((eq type :integer) result)))
    (return out)))

;;; -----------------------------------------------------------------------------
;;; pcs-complement
;;; -----------------------------------------------------------------------------

(defun PCS-COMPLEMENT (type from to list)
  (prog (result out out1 out2 out3)
    (setq list
          (cond
           ((atom (car list))
            (cond
             ((numberp (car list)) list)
             (t (pitch-to-integer list))))
           (t (cond
               ((numberp (car (car list))) list)
               (t (pitch-to-integer list))))))
    (dotimes (i (cond
                 ((listp (car list)) (length list))
                 (t 1)))
      (setq out (cond
                 ((atom (car list))
                  (cond
                   ((< (- from 1) (length  list))
                    (if (> (+ to 1) (length  list))
                      (pcs-complement-f1 list)))))
                 (t (cond
                     ((< (- from 1) (length (car list)))
                      (if (> (+ to 1) (length (car list)))
                        (pcs-complement-f1 (car list))))))))
      (setq out1 (cond
                  ((eq nil out)
                   (cond ((atom (car list)) list)
                         (t (car list))))
                  (t out)))
      (setq out2 (cons out1 out2))
      (setq list (cdr list)))
    (setq out3
          (cond
           ((listp (car list)) (nreverse out2))
           (t out1)))
    (setq result
          (cond
           ((equal type :integer) out3)
           ((equal type :pitch) (integer-to-pitch out3))))
    (return result)))

(defun PCS-COMPLEMENT-F1 (list)
  (prog (out elem)
    (setq out (gen-integer 0 11))
    (dotimes (i (length list))
      (setq elem (car list))
      (setq out (remove elem out))
      (setq list (cdr list)))
    (return out)))

;;; -----------------------------------------------------------------------------
;;; pcs-invert
;;; -----------------------------------------------------------------------------

(defun PCS-INVERT (type set)
  (setq set
        (cond
         ((atom (car set))
          (cond
           ((numberp (car set)) set)
           (t (pitch-to-integer set))))
         (t (cond
             ((numberp (car (car set))) set)
             (t (pitch-to-integer set))))))
  (cond
   ((atom (car set))
    (cond
     ((equal type :integer)
      (pcs-invert-f1 set))
     ((equal type :pitch)
      (integer-to-pitch (pcs-invert-f1 set)))))
   (t (cond
       ((equal type :integer)
        (mapcar 'pcs-invert-f1 set))
       ((equal type :pitch)
        (integer-to-pitch (mapcar 'pcs-invert-f1 set)))))))

(defun PCS-INVERT-F1 (set)
  (prog (out elem)
    (dotimes (i (length set))
      (setq elem
            (cond
             ((= 0 (car set)) 0)
             ((= 1 (car set)) 11)
             ((= 2 (car set)) 10)
             ((= 3 (car set)) 9)
             ((= 4 (car set)) 8)
             ((= 5 (car set)) 7)
             ((= 6 (car set)) 6)
             ((= 7 (car set)) 5)
             ((= 8 (car set)) 4)
             ((= 9 (car set)) 3)
             ((= 10 (car set)) 2)
             ((= 11 (car set)) 1)))
      (setq out (cons elem out))
      (setq set (cdr set)))
    (return (nreverse out))))

;;; -----------------------------------------------------------------------------
;;; pcs-prime-form
;;; -----------------------------------------------------------------------------

(defun PCS-PRIME-FORM-TO-FN (x)
  (cadr (assoc x '(((0 1) 2-1)
                   ((0 2) 2-2)
                   ((0 3) 2-3)
                   ((0 4) 2-4)
                   ((0 5) 2-5)
                   ((0 6) 2-6)
                   ((0 1 2) 3-1)
                   ((0 1 3) 3-2)
                   ((0 1 4) 3-3)
                   ((0 1 5) 3-4)
                   ((0 1 6) 3-5)
                   ((0 2 4) 3-6)
                   ((0 2 5) 3-7)
                   ((0 2 6) 3-8)
                   ((0 2 7) 3-9)
                   ((0 3 6) 3-10)
                   ((0 3 7) 3-11)
                   ((0 4 8) 3-12)  
                   ((0 1 2 3) 4-1)
                   ((0 1 2 4) 4-2)
                   ((0 1 3 4) 4-3)
                   ((0 1 2 5) 4-4)
                   ((0 1 2 6) 4-5)
                   ((0 1 2 7) 4-6) 
                   ((0 1 4 5) 4-7)
                   ((0 1 5 6) 4-8)
                   ((0 1 6 7) 4-9)
                   ((0 2 3 5) 4-10) 
                   ((0 1 3 5) 4-11)
                   ((0 2 3 6) 4-12)
                   ((0 1 3 6) 4-13)
                   ((0 2 3 7) 4-14)
                   ((0 1 4 6) 4-z15)
                   ((0 1 5 7) 4-16)
                   ((0 3 4 7) 4-17)
                   ((0 1 4 7) 4-18)
                   ((0 1 4 8) 4-19)
                   ((0 1 5 8) 4-20)
                   ((0 2 4 6) 4-21)
                   ((0 2 4 7) 4-22)
                   ((0 2 5 7) 4-23)
                   ((0 2 4 8) 4-24)
                   ((0 2 6 8) 4-25)
                   ((0 3 5 8) 4-26)
                   ((0 2 5 8) 4-27)
                   ((0 3 6 9) 4-28)
                   ((0 1 3 7) 4-z29)  
                   ((0 1 2 3 4) 5-1)
                   ((0 1 2 3 5) 5-2)
                   ((0 1 2 4 5) 5-3)
                   ((0 1 2 3 6) 5-4)
                   ((0 1 2 3 7) 5-5)
                   ((0 1 2 5 6) 5-6)
                   ((0 1 2 6 7) 5-7)
                   ((0 2 3 4 6) 5-8)
                   ((0 1 2 4 6) 5-9)
                   ((0 1 3 4 6) 5-10)
                   ((0 2 3 4 7) 5-11)
                   ((0 1 3 5 6) 5-z12)
                   ((0 1 2 4 8) 5-13)
                   ((0 1 2 5 7) 5-14)
                   ((0 1 2 6 8) 5-15)
                   ((0 1 3 4 7) 5-16) 
                   ((0 1 3 4 8) 5-z17)
                   ((0 1 4 5 7) 5-z18)
                   ((0 1 3 6 7) 5-19)
                   ((0 1 3 7 8) 5-20)
                   ((0 1 4 5 8) 5-21)
                   ((0 1 4 7 8) 5-22)
                   ((0 2 3 5 7) 5-23)
                   ((0 1 3 5 7) 5-24)
                   ((0 2 3 5 8) 5-25)
                   ((0 2 4 5 8) 5-26)
                   ((0 1 3 5 8) 5-27)
                   ((0 2 3 6 8) 5-28)
                   ((0 1 3 6 8) 5-29)
                   ((0 1 4 6 8) 5-30)
                   ((0 1 3 6 9) 5-31)
                   ((0 1 4 6 9) 5-32)
                   ((0 2 4 6 8) 5-33)
                   ((0 2 4 6 9) 5-34)
                   ((0 2 4 7 9) 5-35)
                   ((0 1 2 4 7) 5-z36)
                   ((0 3 4 5 8) 5-z37)
                   ((0 1 2 5 8) 5-z38)
                   ((0 1 2 3 4 5) 6-1)
                   ((0 1 2 3 4 6) 6-2)
                   ((0 1 2 3 5 6) 6-z3)
                   ((0 1 2 4 5 6) 6-z4)
                   ((0 1 2 3 6 7) 6-5)
                   ((0 1 2 5 6 7) 6-z6)
                   ((0 1 2 6 7 8) 6-7)
                   ((0 2 3 4 5 7) 6-8)
                   ((0 1 2 3 5 7) 6-9)
                   ((0 1 3 4 5 7) 6-z10)
                   ((0 1 2 4 5 7) 6-z11)
                   ((0 1 2 4 6 7) 6-z12)
                   ((0 1 3 4 6 7) 6-z13)
                   ((0 1 3 4 5 8) 6-14)
                   ((0 1 2 4 5 8) 6-15)
                   ((0 1 4 5 6 8) 6-16)
                   ((0 1 2 4 7 8) 6-z17)
                   ((0 1 2 5 7 8) 6-18)
                   ((0 1 3 4 7 8) 6-z19)
                   ((0 1 4 5 8 9) 6-20)
                   ((0 2 3 4 6 8) 6-21)
                   ((0 1 2 4 6 8) 6-22)
                   ((0 2 3 5 6 8) 6-z23)
                   ((0 1 3 4 6 8) 6-z24)
                   ((0 1 3 5 6 8) 6-z25)
                   ((0 1 3 5 7 8) 6-z26)
                   ((0 1 3 4 6 9) 6-27)
                   ((0 1 3 5 6 9) 6-z28)
                   ((0 1 3 6 8 9) 6-z29)
                   ((0 1 3 6 7 9) 6-30)
                   ((0 1 3 5 8 9) 6-31)
                   ((0 2 4 5 7 9) 6-32)
                   ((0 2 3 5 7 9) 6-33)
                   ((0 1 3 5 7 9) 6-34)
                   ((0 2 4 6 8 10) 6-35)
                   ((0 1 2 3 4 7) 6-z36)
                   ((0 1 2 3 4 8) 6-z37)
                   ((0 1 2 3 7 8) 6-z38)
                   ((0 2 3 4 5 8) 6-z39)
                   ((0 1 2 3 5 8) 6-z40)
                   ((0 1 2 3 6 8) 6-z41)
                   ((0 1 2 3 6 9) 6-z42)
                   ((0 1 2 5 6 8) 6-z43)
                   ((0 1 2 5 6 9) 6-z44)
                   ((0 2 3 4 6 9) 6-z45)
                   ((0 1 2 4 6 9) 6-z46)
                   ((0 1 2 4 7 9) 6-z47)
                   ((0 1 2 5 7 9) 6-z48)
                   ((0 1 3 4 7 9) 6-z49)
                   ((0 1 4 6 7 9) 6-z50)   
                   ((0 1 2 3 4 5 6) 7-1)
                   ((0 1 2 3 4 5 7) 7-2)
                   ((0 1 2 3 4 5 8) 7-3)
                   ((0 1 2 3 4 6 7) 7-4)
                   ((0 1 2 3 5 6 7) 7-5)
                   ((0 1 2 3 4 7 8) 7-6)
                   ((0 1 2 3 6 7 8) 7-7)
                   ((0 2 3 4 5 6 8) 7-8)
                   ((0 1 2 3 4 6 8) 7-9)
                   ((0 1 2 3 4 6 9) 7-10)
                   ((0 1 3 4 5 6 8) 7-11)
                   ((0 1 2 3 4 7 9) 7-z12)
                   ((0 1 2 4 5 6 8) 7-13)
                   ((0 1 2 3 5 7 8) 7-14)
                   ((0 1 2 4 6 7 8) 7-15)
                   ((0 1 2 3 5 6 9) 7-16)
                   ((0 1 2 4 5 6 9) 7-z17)
                   ((0 1 2 3 5 8 9) 7-z18)
                   ((0 1 2 3 6 7 9) 7-19) 
                   ((0 1 2 4 7 8 9) 7-20)
                   ((0 1 2 4 5 8 9) 7-21)
                   ((0 1 2 5 6 8 9) 7-22)
                   ((0 2 3 4 5 7 9) 7-23)
                   ((0 1 2 3 5 7 9) 7-24)
                   ((0 2 3 4 6 7 9) 7-25)
                   ((0 1 3 4 5 7 9) 7-26)
                   ((0 1 2 4 5 7 9) 7-27)
                   ((0 1 3 5 6 7 9) 7-28)
                   ((0 1 2 4 6 7 9) 7-29)
                   ((0 1 2 4 6 8 9) 7-30)
                   ((0 1 3 4 6 7 9) 7-31)
                   ((0 1 3 4 6 8 9) 7-32)
                   ((0 1 2 4 6 8 10) 7-33)
                   ((0 1 3 4 6 8 10) 7-34)
                   ((0 1 3 5 6 8 10) 7-35)
                   ((0 1 2 3 5 6 8) 7-z36)
                   ((0 1 3 4 5 7 8) 7-z37)
                   ((0 1 2 4 5 7 8) 7-z38)
                   ((0 1 2 3 4 5 6 7) 8-1)
                   ((0 1 2 3 4 5 6 8) 8-2)
                   ((0 1 2 3 4 5 6 9) 8-3)
                   ((0 1 2 3 4 5 7 8) 8-4)
                   ((0 1 2 3 4 6 7 8) 8-5)
                   ((0 1 2 3 5 6 7 8) 8-6)
                   ((0 1 2 3 4 5 8 9) 8-7)
                   ((0 1 2 3 4 7 8 9) 8-8)
                   ((0 1 2 3 6 7 8 9) 8-9)
                   ((0 2 3 4 5 6 7 9) 8-10)
                   ((0 1 2 3 4 5 7 9) 8-11)
                   ((0 1 3 4 5 6 7 9) 8-12)
                   ((0 1 2 3 4 6 7 9) 8-13)
                   ((0 1 2 4 5 6 7 9) 8-14)
                   ((0 1 2 3 4 6 8 9) 8-z15)
                   ((0 1 2 3 5 7 8 9) 8-16)
                   ((0 1 3 4 5 6 8 9) 8-17)
                   ((0 1 2 3 5 6 8 9) 8-18)
                   ((0 1 2 4 5 6 8 9) 8-19)
                   ((0 1 2 4 5 7 8 9) 8-20)
                   ((0 1 2 3 4 6 8 10) 8-21)
                   ((0 1 2 3 5 6 8 10) 8-22)
                   ((0 1 2 3 5 7 8 10) 8-23)
                   ((0 1 2 4 5 6 8 10) 8-24)
                   ((0 1 2 4 6 7 8 10) 8-25)
                   ((0 1 2 4 5 7 9 10) 8-26)
                   ((0 1 2 4 5 7 8 10) 8-27)
                   ((0 1 3 4 6 7 9 10) 8-28)
                   ((0 1 2 3 5 6 7 9) 8-z29)
                   ((0 1 2 3 4 5 6 7 8) 9-1)
                   ((0 1 2 3 4 5 6 7 9) 9-2)
                   ((0 1 2 3 4 5 6 8 9) 9-3)
                   ((0 1 2 3 4 5 7 8 9) 9-4)
                   ((0 1 2 3 4 6 7 8 9) 9-5)
                   ((0 1 2 3 4 5 6 8 10) 9-6)
                   ((0 1 2 3 4 5 7 8 10) 9-7)
                   ((0 1 2 3 4 6 7 8 10) 9-8)
                   ((0 1 2 3 5 6 7 8 10) 9-9)
                   ((0 1 2 3 4 6 7 9 10) 9-10)
                   ((0 1 2 3 5 6 7 9 10) 9-11)
                   ((0 1 2 4 5 6 8 9 10) 9-12)
                   ((0 1 2 3 4 5 6 7 8 9) 10-1)
                   ((0 1 2 3 4 5 6 7 8 10) 10-2)
                   ((0 1 2 3 4 5 6 7 9 10) 10-3)
                   ((0 1 2 3 4 5 6 8 9 10) 10-4)
                   ((0 1 2 3 4 5 7 8 9 10) 10-5)
                   ((0 1 2 3 4 6 7 8 9 10) 10-6)) :test 'equal)))

(defun PCS-PRIME-FORM1 (set)
  (setq set (pcs-normal-order :integer set))
  (pcs-transpose :integer (- 12 (car set)) set))

(defun PCS-PRIME-FORM2 (set)
  (prog (out normal-order)
    (setq set (pcs-normal-order :integer set))
    (setq normal-order
          (pcs-normal-order :integer
                            (sort-ascending
                             (pcs-invert :integer
                                         (pcs-transpose :integer (- 12 (car set)) set)))))
    (setq out
          (pcs-transpose :integer
                         (- 12 (car normal-order)) normal-order))
    (return out)))

(defun PCS-PRIME-FORM-CHECK (set)
  (cond
   ((not (null (pcs-prime-form-to-fn (pcs-prime-form2 set)))) (pcs-prime-form2 set))
   (t (pcs-prime-form1 set))))


(defun PCS-PRIME-FORM3 (type set)
  (cond
   ((equal type :integer)
    (pcs-prime-form1 set))
   ((equal type :pitch)
    (integer-to-pitch (pcs-prime-form-check set)))
   ((equal type :fn) (pcs-prime-form-to-fn (pcs-prime-form-check set)))
   ((equal type :vector)
    (pcs :vector (pcs-prime-form-to-fn (pcs-prime-form-check set))))))


(defun PCS-PRIME-FORM (type set)
  (setq set
        (cond
         ((atom (car set))
          (cond
           ((numberp (car set)) set)
           (t (pitch-to-integer set))))
         (t (cond
             ((numberp (car (car set))) set)
             (t (pitch-to-integer set))))))
  (cond
   ((atom (car set)) (pcs-prime-form3 type set))
   (t (mapcar #'(lambda (x)
                  (pcs-prime-form3 type x)) set))))

;;; -----------------------------------------------------------------------------
;;; pcs-sub-power
;;; -----------------------------------------------------------------------------

(defun PCS-SUB-POWER-F1 (n-element set)
  (prog (subsets out elem result)
    (setq subsets (power-set set))
    (dotimes (i (length subsets))
      (setq elem
            (cond
             ((equal n-element (length (car subsets))) (car subsets))))
      (setq out (cons elem out))
      (setq subsets (cdr subsets)))
    (setq result (remove nil (nreverse out)))
    (return result)))

(defun PCS-SUB-POWER-F2 (sn-l sn-h set)
  (prog (out result elem n-element)
    (setq n-element
          (gen-integer sn-l sn-h))
    (dotimes (i (length n-element))
      (setq elem (pcs-sub-power-f1 (car n-element) set))
      (setq out (cons elem out))
      (setq n-element (cdr n-element)))
    (setq result (append-sublist (nreverse out)))
    (return result)))

(defun PCS-SUB-POWER-F3 (type sn-l sn-h set)
  (prog (out set-out result)
    (setq set
          (cond
           ((atom set) (pcs :integer set))
           ((numberp (car set)) set)
           (t (pitch-to-integer set))))
    (setq out
          (cond
           ((equal type :integer) (pcs-sub-power-f2 sn-l sn-h set))
           ((equal type :pitch)
            (integer-to-pitch (pcs-sub-power-f2 sn-l sn-h set)))
           ((equal type :fn)
            (delete-duplicates
             (cond
              ((atom
                (setq set-out
                      (pcs-prime-form :fn
                                      (pcs-normal-order :integer
                                                        (pcs-sub-power-f2 sn-l sn-h set)))))
               (list set-out))
              (t set-out)) :test #'equal))))
    (setq result
          (cond
           ((equal 1 (length out)) (car out))
           (t out)))
    (return result)))

(defun PCS-SUB-POWER (type low high set)
  (cond
   ((atom (car set)) (pcs-sub-power-f3 type low high set))
   (t (mapcar
       #'(lambda (x) 
           (pcs-sub-power-f3 type low high x))
       set))))

;;; -----------------------------------------------------------------------------
;;; pcs-sub-prime-form
;;; -----------------------------------------------------------------------------

(defun PCS-SUB-PRIME-FORM-F1 (type sn-l sn-h set)
  (prog (out set-out result)
    (setq out
          (delete-duplicates
           (cond
            ((atom 
              (setq set-out
                    (pcs-prime-form type (pcs-sub-power-f2 sn-l sn-h set))))
             (list set-out))
            (t set-out)) :test #'equal))
    (setq result
          (cond
           ((equal 1 (length out)) (car out))
           (t out)))
    (return result)))

(defun PCS-SUB-PRIME-FORM (type low high set)
  (cond
   ((atom (car set)) (pcs-sub-prime-form-f1 type low high set))
   (t (mapcar
       #'(lambda (x) 
           (pcs-sub-prime-form-f1 type low high x))
       set))))

;;; -----------------------------------------------------------------------------
;;; pcs-sub-relation
;;; -----------------------------------------------------------------------------

(defun REMOVE-UNIQUE-F1 (elem set)
  (prog (e-out out)
    (dotimes (i (length set))
      (setq e-out
            (cond
             ((equal elem (car set)) elem)))
      (setq out (cons e-out out))
      (setq set (cdr set)))
    (return (remove 'nil (nreverse out)))))

(defun REMOVE-UNIQUE (set)
  (prog (x elem out set1 set-incf)
    (setq x 0)
    (setq set-incf set)
    (dotimes (i (length set))
      (setq set1 (position-remove x set-incf))
      (setq elem 
            (remove-unique-f1 (car set) set1))
      (setq out (cons elem out))
      (setq set (cdr set))
      (setq x (incf x)))
    (return (delete-duplicates (pcs-flatten (nreverse out)) :test #'equal))))

(defun PCS-SUB-RELATION (type low high sets)
  "(pcs-sub-relation :fn 3 4 '((10 11 1 3 4) (10 0 1 3 4)))"
  (prog (out result)
    (setq out
          (remove-unique
           (cond
            ((atom (car sets))
             (pcs-flatten
              (pcs-flatten
               (pcs-sub-prime-form :fn low high (pcs :integer sets)))))
            (t (pcs-flatten
                (pcs-flatten
                 (list (pcs-sub-prime-form :fn low high sets))))))))
    (setq result
          (cond
           ((equal type :fn)
            (cond
             ((equal 1 (length out)) (car out))
             (t out)))
           (t (pcs type out))))
    (return result)))

;;; -----------------------------------------------------------------------------
;;; pcs-transpose
;;; -----------------------------------------------------------------------------

(defun PCS-TRANSPOSE (type transp-value set)
  (setq set
        (cond
         ((atom (car set))
          (cond
           ((numberp (car set)) set)
           (t (pitch-to-integer set))))
         (t (cond
             ((numberp (car (car set))) set)
             (t (pitch-to-integer set))))))
  (cond
   ((atom (car set))
    (cond
     ((equal type :integer)
      (pcs-transpose-f1 transp-value set))
     ((equal type :pitch)
      (integer-to-pitch
       (pcs-transpose-f1 transp-value set)))))
   (t (cond
       ((equal type :integer)
        (mapcar #'(lambda (x y) (pcs-transpose-f1 x y))
                transp-value set))
       ((equal type :pitch)
        (integer-to-pitch
         (mapcar #'(lambda (x y) (pcs-transpose-f1 x y))
                 transp-value set)))))))

(defun PCS-TRANSPOSE-F1 (transp-value set)
  (prog (integ transp out out1)
    loop
    (cond ((null set) (return (remove 'nil (nreverse out)))))
    (setq integ (car set))
    (setq transp (+ transp-value integ))
    (setq out1
          (cond
           ((> 11 transp) transp)
           ((= 11 transp) transp)
           ((= 12 transp) 0)
           ((= 13 transp) 1)
           ((= 14 transp) 2)
           ((= 15 transp) 3)
           ((= 16 transp) 4)
           ((= 17 transp) 5)
           ((= 18 transp) 6)
           ((= 19 transp) 7)
           ((= 20 transp) 8)
           ((= 21 transp) 9)
           ((= 22 transp) 10)
           ((= 23 transp) 11)))
    (setq out (cons out1 out))
    (setq set (cdr set))
    (go loop)))

;;; -----------------------------------------------------------------------------
;;; pcs
;;; -----------------------------------------------------------------------------

(defun PCS (type list)
  (prog (result)
    (setq result
          (cond
           ((atom list)
            (cond
             ((equal type :integer) (pcs-i list))
             ((equal type :pitch) (integer-to-pitch (pcs-i list)))
             ((equal type :vector) (pcs-v list))))
           (t (cond
               ((equal type :integer)
                (mapcar #'(lambda (x) (pcs-i x)) list))
               ((equal type :pitch)
                (integer-to-pitch 
                 (mapcar #'(lambda (x) (pcs-i x)) list)))
               ((equal type :vector)
                (mapcar #'(lambda (x) (pcs-v x)) list))))))
    (return result)))

(defun PCS-I (x)
  (cadr (assoc x '((2-1 (0 1))
                   (2-2 (0 2))
                   (2-3 (0 3))
                   (2-4 (0 4))
                   (2-5 (0 5))
                   (2-6 (0 6))
                   (3-1 (0 1 2))
                   (3-2 (0 1 3))
                   (3-3 (0 1 4))
                   (3-4 (0 1 5))
                   (3-5 (0 1 6))
                   (3-6 (0 2 4))
                   (3-7 (0 2 5))
                   (3-8 (0 2 6))
                   (3-9 (0 2 7))
                   (3-10 (0 3 6))
                   (3-11 (0 3 7))
                   (3-12 (0 4 8))
                   (4-1 (0 1 2 3))
                   (4-2 (0 1 2 4))
                   (4-3 (0 1 3 4))
                   (4-4 (0 1 2 5))
                   (4-5 (0 1 2 6))
                   (4-6 (0 1 2 7)) 
                   (4-7 (0 1 4 5))
                   (4-8 (0 1 5 6))
                   (4-9 (0 1 6 7))
                   (4-10 (0 2 3 5)) 
                   (4-11 (0 1 3 5))
                   (4-12 (0 2 3 6))
                   (4-13 (0 1 3 6))
                   (4-14 (0 2 3 7))
                   (4-z15 (0 1 4 6))
                   (4-16 (0 1 5 7))
                   (4-17 (0 3 4 7))
                   (4-18 (0 1 4 7))
                   (4-19 (0 1 4 8))
                   (4-20 (0 1 5 8))
                   (4-21 (0 2 4 6))
                   (4-22 (0 2 4 7))
                   (4-23 (0 2 5 7))
                   (4-24 (0 2 4 8))
                   (4-25 (0 2 6 8))
                   (4-26 (0 3 5 8))
                   (4-27 (0 2 5 8))
                   (4-28 (0 3 6 9))
                   (4-z29 (0 1 3 7))
                   (5-1 (0 1 2 3 4))
                   (5-2 (0 1 2 3 5))
                   (5-3 (0 1 2 4 5))
                   (5-4 (0 1 2 3 6))
                   (5-5 (0 1 2 3 7))
                   (5-6 (0 1 2 5 6))
                   (5-7 (0 1 2 6 7))
                   (5-8 (0 2 3 4 6))
                   (5-9 (0 1 2 4 6))
                   (5-10 (0 1 3 4 6))
                   (5-11 (0 2 3 4 7))
                   (5-z12 (0 1 3 5 6))
                   (5-13 (0 1 2 4 8))
                   (5-14 (0 1 2 5 7))
                   (5-15 (0 1 2 6 8))
                   (5-16 (0 1 3 4 7)) 
                   (5-z17 (0 1 3 4 8))
                   (5-z18 (0 1 4 5 7))
                   (5-19 (0 1 3 6 7))
                   (5-20 (0 1 3 7 8))
                   (5-21 (0 1 4 5 8))
                   (5-22 (0 1 4 7 8))
                   (5-23 (0 2 3 5 7))
                   (5-24 (0 1 3 5 7))
                   (5-25 (0 2 3 5 8))
                   (5-26 (0 2 4 5 8))
                   (5-27 (0 1 3 5 8))
                   (5-28 (0 2 3 6 8))
                   (5-29 (0 1 3 6 8))
                   (5-30 (0 1 4 6 8))
                   (5-31 (0 1 3 6 9))
                   (5-32 (0 1 4 6 9))
                   (5-33 (0 2 4 6 8))
                   (5-34 (0 2 4 6 9))
                   (5-35 (0 2 4 7 9))
                   (5-z36 (0 1 2 4 7))
                   (5-z37 (0 3 4 5 8))
                   (5-z38 (0 1 2 5 8))
                   (6-1 (0 1 2 3 4 5))
                   (6-2 (0 1 2 3 4 6))
                   (6-z3 (0 1 2 3 5 6))
                   (6-z4 (0 1 2 4 5 6))
                   (6-5 (0 1 2 3 6 7))
                   (6-z6 (0 1 2 5 6 7))
                   (6-7 (0 1 2 6 7 8))
                   (6-8 (0 2 3 4 5 7))
                   (6-9 (0 1 2 3 5 7))
                   (6-z10 (0 1 3 4 5 7))
                   (6-z11 (0 1 2 4 5 7))
                   (6-z12 (0 1 2 4 6 7))
                   (6-z13 (0 1 3 4 6 7))
                   (6-14 (0 1 3 4 5 8))
                   (6-15 (0 1 2 4 5 8))
                   (6-16 (0 1 4 5 6 8))
                   (6-z17 (0 1 2 4 7 8))
                   (6-18 (0 1 2 5 7 8))
                   (6-z19 (0 1 3 4 7 8))
                   (6-20 (0 1 4 5 8 9))
                   (6-21 (0 2 3 4 6 8))
                   (6-22 (0 1 2 4 6 8))
                   (6-z23 (0 2 3 5 6 8))
                   (6-z24 (0 1 3 4 6 8))
                   (6-z25 (0 1 3 5 6 8))
                   (6-z26 (0 1 3 5 7 8))
                   (6-27 (0 1 3 4 6 9))
                   (6-z28 (0 1 3 5 6 9))
                   (6-z29 (0 1 3 6 8 9))
                   (6-30 (0 1 3 6 7 9))
                   (6-31 (0 1 3 5 8 9))
                   (6-32 (0 2 4 5 7 9))
                   (6-33 (0 2 3 5 7 9))
                   (6-34 (0 1 3 5 7 9))
                   (6-35 (0 2 4 6 8 10))
                   (6-z36 (0 1 2 3 4 7))
                   (6-z37 (0 1 2 3 4 8))
                   (6-z38 (0 1 2 3 7 8))
                   (6-z39 (0 2 3 4 5 8))
                   (6-z40 (0 1 2 3 5 8))
                   (6-z41 (0 1 2 3 6 8))
                   (6-z42 (0 1 2 3 6 9))
                   (6-z43 (0 1 2 5 6 8))
                   (6-z44 (0 1 2 5 6 9))
                   (6-z45 (0 2 3 4 6 9))
                   (6-z46 (0 1 2 4 6 9))
                   (6-z47 (0 1 2 4 7 9))
                   (6-z48 (0 1 2 5 7 9))
                   (6-z49 (0 1 3 4 7 9))
                   (6-z50 (0 1 4 6 7 9))
                   (7-1 (0 1 2 3 4 5 6))
                   (7-2 (0 1 2 3 4 5 7))
                   (7-3 (0 1 2 3 4 5 8))
                   (7-4 (0 1 2 3 4 6 7))
                   (7-5 (0 1 2 3 5 6 7))
                   (7-6 (0 1 2 3 4 7 8))
                   (7-7 (0 1 2 3 6 7 8))
                   (7-8 (0 2 3 4 5 6 8))
                   (7-9 (0 1 2 3 4 6 8))
                   (7-10 (0 1 2 3 4 6 9))
                   (7-11 (0 1 3 4 5 6 8))
                   (7-z12 (0 1 2 3 4 7 9))
                   (7-13 (0 1 2 4 5 6 8))
                   (7-14 (0 1 2 3 5 7 8))
                   (7-15 (0 1 2 4 6 7 8))
                   (7-16 (0 1 2 3 5 6 9))
                   (7-z17 (0 1 2 4 5 6 9))
                   (7-z18 (0 1 2 3 5 8 9))
                   (7-19 (0 1 2 3 6 7 9)) 
                   (7-20 (0 1 2 4 7 8 9))
                   (7-21 (0 1 2 4 5 8 9))
                   (7-22 (0 1 2 5 6 8 9))
                   (7-23 (0 2 3 4 5 7 9))
                   (7-24 (0 1 2 3 5 7 9))
                   (7-25 (0 2 3 4 6 7 9))
                   (7-26 (0 1 3 4 5 7 9))
                   (7-27 (0 1 2 4 5 7 9))
                   (7-28 (0 1 3 5 6 7 9))
                   (7-29 (0 1 2 4 6 7 9))
                   (7-30 (0 1 2 4 6 8 9))
                   (7-31 (0 1 3 4 6 7 9))
                   (7-32 (0 1 3 4 6 8 9))
                   (7-33 (0 1 2 4 6 8 10))
                   (7-34 (0 1 3 4 6 8 10))
                   (7-35 (0 1 3 5 6 8 10))
                   (7-z36 (0 1 2 3 5 6 8))
                   (7-z37 (0 1 3 4 5 7 8))
                   (7-z38 (0 1 2 4 5 7 8))
                   (8-1 (0 1 2 3 4 5 6 7))
                   (8-2 (0 1 2 3 4 5 6 8))
                   (8-3 (0 1 2 3 4 5 6 9))
                   (8-4 (0 1 2 3 4 5 7 8))
                   (8-5 (0 1 2 3 4 6 7 8))
                   (8-6 (0 1 2 3 5 6 7 8))
                   (8-7 (0 1 2 3 4 5 8 9))
                   (8-8 (0 1 2 3 4 7 8 9))
                   (8-9 (0 1 2 3 6 7 8 9))
                   (8-10 (0 2 3 4 5 6 7 9))
                   (8-11 (0 1 2 3 4 5 7 9))
                   (8-12 (0 1 3 4 5 6 7 9))
                   (8-13 (0 1 2 3 4 6 7 9))
                   (8-14 (0 1 2 4 5 6 7 9))
                   (8-z15 (0 1 2 3 4 6 8 9))
                   (8-16 (0 1 2 3 5 7 8 9))
                   (8-17 (0 1 3 4 5 6 8 9))
                   (8-18 (0 1 2 3 5 6 8 9))
                   (8-19 (0 1 2 4 5 6 8 9))
                   (8-20 (0 1 2 4 5 7 8 9))
                   (8-21 (0 1 2 3 4 6 8 10))
                   (8-22 (0 1 2 3 5 6 8 10))
                   (8-23 (0 1 2 3 5 7 8 10))
                   (8-24 (0 1 2 4 5 6 8 10))
                   (8-25 (0 1 2 4 6 7 8 10))
                   (8-26 (0 1 2 4 5 7 9 10))
                   (8-27 (0 1 2 4 5 7 8 10))
                   (8-28 (0 1 3 4 6 7 9 10))
                   (8-z29 (0 1 2 3 5 6 7 9))
                   (9-1 (0 1 2 3 4 5 6 7 8))
                   (9-2 (0 1 2 3 4 5 6 7 9))
                   (9-3 (0 1 2 3 4 5 6 8 9))
                   (9-4 (0 1 2 3 4 5 7 8 9))
                   (9-5 (0 1 2 3 4 6 7 8 9))
                   (9-6 (0 1 2 3 4 5 6 8 10))
                   (9-7 (0 1 2 3 4 5 7 8 10))
                   (9-8 (0 1 2 3 4 6 7 8 10))
                   (9-9 (0 1 2 3 5 6 7 8 10))
                   (9-10 (0 1 2 3 4 6 7 9 10))
                   (9-11 (0 1 2 3 5 6 7 9 10))
                   (9-12 (0 1 2 4 5 6 8 9 10))
                   (10-1 (0 1 2 3 4 5 6 7 8 9))
                   (10-2 (0 1 2 3 4 5 6 7 8 10))
                   (10-3 (0 1 2 3 4 5 6 7 9 10))
                   (10-4 (0 1 2 3 4 5 6 8 9 10))
                   (10-5 (0 1 2 3 4 5 7 8 9 10))
                   (10-6 (0 1 2 3 4 6 7 8 9 10))) :test 'equal)))

(defun PCS-V (x)
  (cadr (assoc x '((2-1 (1 0 0 0 0 0))
                   (2-2 (0 1 0 0 0 0))
                   (2-3 (0 0 1 0 0 0))
                   (2-4 (0 0 0 1 0 0))
                   (2-5 (0 0 0 0 1 0))
                   (2-6 (0 0 0 0 0 1)) 
                   (3-1 (2 1 0 0 0 0))
                   (3-2 (1 1 1 0 0 0))
                   (3-3 (1 0 1 1 0 0))
                   (3-4 (1 0 0 1 1 0))
                   (3-5 (1 0 0 0 1 1))
                   (3-6 (0 2 0 1 0 0))
                   (3-7 (0 1 1 0 1 0))
                   (3-8 (0 1 0 1 0 1))
                   (3-9 (0 1 0 0 2 0))
                   (3-10 (0 0 2 0 0 1))
                   (3-11 (0 0 1 1 1 0))
                   (3-12 (0 0 0 3 0 0))
                   (4-1 (3 2 1 0 0 0))
                   (4-2 (2 2 1 1 0 0))
                   (4-3 (2 1 2 1 0 0))
                   (4-4 (2 1 1 1 1 0))
                   (4-5 (2 1 0 1 1 1))
                   (4-6 (2 1 0 0 2 1))
                   (4-7 (2 0 1 2 1 0))
                   (4-8 (2 0 0 1 2 1))
                   (4-9 (2 0 0 0 2 2))
                   (4-10 (1 2 2 0 1 0))
                   (4-11 (1 2 1 1 1 0))
                   (4-12 (1 1 2 1 0 1))
                   (4-13 (1 1 2 0 1 1))
                   (4-14 (1 1 1 1 2 0))
                   (4-z15 (1 1 1 1 1 1))
                   (4-16 (1 1 0 1 2 1))
                   (4-17 (1 0 2 2 1 0))
                   (4-18 (1 0 2 1 1 1))
                   (4-19 (1 0 1 3 1 0))
                   (4-20 (1 0 1 2 2 0))
                   (4-21 (0 3 0 2 0 1))
                   (4-22 (0 2 1 1 2 0))
                   (4-23 (0 2 1 0 3 0))
                   (4-24 (0 2 0 3 0 1))
                   (4-25 (0 2 0 2 0 2))
                   (4-26 (0 1 2 1 2 0))
                   (4-27 (0 1 2 1 1 1))
                   (4-28 (0 0 4 0 0 2))
                   (4-z29 (1 1 1 1 1 1))
                   (5-1 (4 3 2 1 0 0))
                   (5-2 (3 3 2 1 1 0))
                   (5-3 (3 2 2 2 1 0))
                   (5-4 (3 2 2 1 1 1))
                   (5-5 (3 2 1 1 2 1))
                   (5-6 (3 1 1 2 2 1))
                   (5-7 (3 1 0 1 3 2))
                   (5-8 (2 3 2 2 0 1))
                   (5-9 (2 3 1 2 1 1))
                   (5-10 (2 2 3 1 1 1))
                   (5-11 (2 2 2 2 2 0))
                   (5-z12 (2 2 2 1 2 1))
                   (5-13 (2 2 1 3 1 1))
                   (5-14 (2 2 1 1 3 1))
                   (5-15 (2 2 0 2 2 2))
                   (5-16 (2 1 3 2 1 1))
                   (5-z17 (2 1 2 3 2 0))
                   (5-z18 (2 1 2 2 2 1))
                   (5-19 (2 1 2 1 2 2))
                   (5-20 (2 1 1 2 3 1))
                   (5-21 (2 0 2 4 2 0))
                   (5-22 (2 0 2 3 2 1))
                   (5-23 (1 3 2 1 3 0))
                   (5-24 (1 3 1 2 2 1))
                   (5-25 (1 2 3 1 2 1))
                   (5-26 (1 2 2 3 1 1))
                   (5-27 (1 2 2 2 3 0))
                   (5-28 (1 2 2 2 1 2))
                   (5-29 (1 2 2 1 3 1))
                   (5-30 (1 2 1 3 2 1))
                   (5-31 (1 1 4 1 1 2))
                   (5-32 (1 1 3 2 2 1))
                   (5-33 (0 4 0 4 0 2))
                   (5-34 (0 3 2 2 2 1))
                   (5-35 (0 3 2 1 4 0))
                   (5-z36 (2 2 2 1 2 1))
                   (5-z37 (2 1 2 3 2 0))
                   (5-z38 (2 1 2 2 2 1))
                   (6-1 (5 4 3 2 1 0)) 
                   (6-2 (4 4 3 2 1 1))
                   (6-z3 (4 3 3 2 2 1))
                   (6-z4 (4 3 2 3 2 1)) 
                   (6-5 (4 2 2 2 3 2)) 
                   (6-z6 (4 2 1 2 4 2))
                   (6-7 (4 2 0 2 4 3))
                   (6-8 (3 4 3 2 3 0))
                   (6-9 (3 4 2 2 3 1))
                   (6-z10 (3 3 3 3 2 1)) 
                   (6-z11 (3 3 3 2 3 1))
                   (6-z12 (3 3 2 2 3 2))
                   (6-z13 (3 2 4 2 2 2))
                   (6-14 (3 2 3 4 3 0))
                   (6-15 (3 2 3 4 2 1))
                   (6-16 (3 2 2 4 3 1))
                   (6-z17 (3 2 2 3 3 2))
                   (6-18 (3 2 2 2 4 2))
                   (6-z19 (3 1 3 4 3 1))
                   (6-20 (3 0 3 6 3 0))
                   (6-21 (2 4 2 4 1 2))
                   (6-22 (2 4 1 4 2 2))
                   (6-z23 (2 3 4 2 2 2))
                   (6-z24 (2 3 3 3 3 1))
                   (6-z25 (2 3 3 2 4 2))
                   (6-z26 (2 3 2 3 4 1))
                   (6-27 (2 2 5 2 2 2))
                   (6-z28 (2 2 4 3 2 2))
                   (6-z29 (2 2 4 2 3 2))
                   (6-30 (2 2 4 2 2 3))
                   (6-31 (2 2 3 4 3 1))
                   (6-32 (1 4 3 2 5 0))
                   (6-33 (1 4 3 2 4 1))
                   (6-34 (1 4 2 4 2 2))
                   (6-35 (0 6 0 6 0 3))
                   (6-z36 (4 3 3 2 2 1))
                   (6-z37 (4 3 2 3 2 1))
                   (6-z38 (4 2 1 2 4 2))
                   (6-z39 (3 3 3 3 2 1))
                   (6-z40 (3 3 3 2 3 1))
                   (6-z41 (3 3 2 2 3 2))
                   (6-z42 (3 2 4 2 2 2))
                   (6-z43 (3 2 2 3 3 2))
                   (6-z44 (3 1 3 4 3 1))
                   (6-z45 (2 3 4 2 2 2))
                   (6-z46 (2 3 3 3 3 1))
                   (6-z47 (2 3 3 2 4 1))
                   (6-z48 (2 3 2 3 4 1))
                   (6-z49 (2 2 4 3 2 2))
                   (6-z50 (2 2 4 2 3 2))
                   (7-1 (6 5 4 3 2 1))
                   (7-2 (5 5 4 3 3 1))
                   (7-3 (5 4 4 4 3 1))
                   (7-4 (5 4 4 3 3 2))
                   (7-5 (5 4 3 3 4 2))
                   (7-6 (5 3 3 4 4 2))
                   (7-7 (5 3 2 3 5 3))
                   (7-8 (4 5 4 4 2 2))
                   (7-9 (4 5 3 4 3 2))
                   (7-10 (4 4 5 3 3 2))
                   (7-11 (4 4 4 4 4 1))
                   (7-z12 (4 4 4 3 4 2))
                   (7-13 (4 4 3 5 3 2))
                   (7-14 (4 4 3 3 5 2))
                   (7-15 (4 4 2 4 4 3))
                   (7-16 (4 3 5 4 3 2))
                   (7-z17 (4 3 4 5 4 1))
                   (7-z18 (4 3 4 4 4 2))
                   (7-19 (4 3 4 3 4 3))
                   (7-20 (4 3 3 4 5 2))
                   (7-21 (4 2 4 6 4 1))
                   (7-22 (4 2 4 5 4 2))
                   (7-23 (3 5 4 3 5 1))
                   (7-24 (3 5 3 4 4 2))
                   (7-25 (3 4 5 3 4 2))
                   (7-26 (3 4 4 5 3 2))
                   (7-27 (3 4 4 4 5 1))
                   (7-28 (3 4 4 4 3 3))
                   (7-29 (3 4 4 3 5 2))
                   (7-30 (3 4 3 5 4 2))
                   (7-31 (3 3 6 3 3 3))
                   (7-32 (3 3 5 4 4 2))
                   (7-33 (2 6 2 6 2 3))
                   (7-34 (2 5 4 4 4 2))
                   (7-35 (2 5 4 3 6 1))
                   (7-z36 (4 4 4 3 4 2))
                   (7-z37 (4 3 4 5 4 1))
                   (7-z38 (4 3 4 4 4 2))
                   (8-1 (7 6 5 4 4 2))
                   (8-2 (6 6 5 5 4 2))
                   (8-3 (6 5 6 5 4 2))
                   (8-4 (6 5 5 5 5 2))
                   (8-5 (6 5 4 5 5 3))
                   (8-6 (6 5 4 4 6 3))
                   (8-7 (6 4 5 6 5 2))
                   (8-8 (6 4 4 5 6 3))
                   (8-9 (6 4 4 4 6 4))
                   (8-10 (5 6 6 4 5 2))
                   (8-11 (5 6 5 5 5 2))
                   (8-12 (5 5 6 5 4 3))
                   (8-13 (5 5 6 4 5 3))
                   (8-14 (5 5 5 5 6 2))
                   (8-z15 (5 5 5 5 5 3))
                   (8-16 (5 5 4 5 6 3))
                   (8-17 (5 4 6 6 5 2))
                   (8-18 (5 4 6 5 5 3))
                   (8-19 (5 4 5 7 5 2))
                   (8-20 (5 4 5 6 6 2))
                   (8-21 (4 7 4 6 4 3))
                   (8-22 (4 6 5 5 6 2))
                   (8-23 (4 6 5 4 7 2))
                   (8-24 (4 6 4 7 4 3))
                   (8-25 (4 6 4 6 4 4))
                   (8-26 (4 5 6 5 6 2))
                   (8-27 (4 5 6 5 5 3))
                   (8-28 (4 4 8 4 4 4))
                   (8-z29 (5 5 5 5 5 3))
                   (9-1 (8 7 6 6 6 3))
                   (9-2 (7 7 7 6 6 3))
                   (9-3 (7 6 7 7 6 3))
                   (9-4 (7 6 6 7 7 3))
                   (9-5 (7 6 6 6 7 4))
                   (9-6 (6 8 6 7 6 3))
                   (9-7 (6 7 7 6 7 3))
                   (9-8 (6 7 6 7 6 4))
                   (9-9 (6 7 6 6 8 3))
                   (9-10 (6 6 8 6 6 4))
                   (9-11 (6 6 7 7 7 3))
                   (9-12 (6 6 6 9 6 3))
                   (10-1 (9 8 8 8 8 4))
                   (10-2 (8 9 8 8 8 4))
                   (10-3 (8 8 9 8 8 4))
                   (10-4 (8 8 8 9 8 4))
                   (10-5 (8 8 8 8 9 4))
                   (10-6 (8 8 8 8 8 5))) :test 'equal)))

;;; -----------------------------------------------------------------------------
;;; pcs-subcomplex
;;; -----------------------------------------------------------------------------

(defun PCS-SUBCOMPLEX (type low high fn-name)
  (cond
   ((eq fn-name '4-1) (pcs-subcomplex4-1 type low high))
   ((eq fn-name '4-2) (pcs-subcomplex4-2 type low high))
   ((eq fn-name '4-3) (pcs-subcomplex4-3 type low high))
   ((eq fn-name '4-4) (pcs-subcomplex4-4 type low high))
   ((eq fn-name '4-5) (pcs-subcomplex4-5 type low high))
   ((eq fn-name '4-6) (pcs-subcomplex4-6 type low high))
   ((eq fn-name '4-7) (pcs-subcomplex4-7 type low high))
   ((eq fn-name '4-8) (pcs-subcomplex4-8 type low high))
   ((eq fn-name '4-9) (pcs-subcomplex4-9 type low high))
   ((eq fn-name '4-10) (pcs-subcomplex4-10 type low high))
   ((eq fn-name '4-11) (pcs-subcomplex4-11 type low high))
   ((eq fn-name '4-12) (pcs-subcomplex4-12 type low high))
   ((eq fn-name '4-13) (pcs-subcomplex4-13 type low high))
   ((eq fn-name '4-14) (pcs-subcomplex4-14 type low high))
   ((eq fn-name '4-z15) (pcs-subcomplex4-z15 type low high))
   ((eq fn-name '4-16) (pcs-subcomplex4-16 type low high))
   ((eq fn-name '4-17) (pcs-subcomplex4-17 type low high))
   ((eq fn-name '4-18) (pcs-subcomplex4-18 type low high))
   ((eq fn-name '4-19) (pcs-subcomplex4-19 type low high))
   ((eq fn-name '4-20) (pcs-subcomplex4-20 type low high))
   ((eq fn-name '4-21) (pcs-subcomplex4-21 type low high))
   ((eq fn-name '4-22) (pcs-subcomplex4-22 type low high))
   ((eq fn-name '4-23) (pcs-subcomplex4-23 type low high))
   ((eq fn-name '4-24) (pcs-subcomplex4-24 type low high))
   ((eq fn-name '4-25) (pcs-subcomplex4-25 type low high))
   ((eq fn-name '4-26) (pcs-subcomplex4-26 type low high))
   ((eq fn-name '4-27) (pcs-subcomplex4-27 type low high))
   ((eq fn-name '4-28) (pcs-subcomplex4-28 type low high))
   ((eq fn-name '4-z29) (pcs-subcomplex4-z29 type low high))
   ((eq fn-name '5-1) (pcs-subcomplex5-1 type low high))
   ((eq fn-name '5-2) (pcs-subcomplex5-2 type low high))
   ((eq fn-name '5-3) (pcs-subcomplex5-3 type low high))
   ((eq fn-name '5-4) (pcs-subcomplex5-4 type low high))
   ((eq fn-name '5-5) (pcs-subcomplex5-5 type low high))
   ((eq fn-name '5-6) (pcs-subcomplex5-6 type low high))
   ((eq fn-name '5-7) (pcs-subcomplex5-7 type low high))
   ((eq fn-name '5-8) (pcs-subcomplex5-8 type low high))
   ((eq fn-name '5-9) (pcs-subcomplex5-9 type low high))
   ((eq fn-name '5-10) (pcs-subcomplex5-10 type low high))
   ((eq fn-name '5-11) (pcs-subcomplex5-11 type low high))
   ((eq fn-name '5-z12) (pcs-subcomplex5-z12 type low high))
   ((eq fn-name '5-13) (pcs-subcomplex5-13 type low high))
   ((eq fn-name '5-14) (pcs-subcomplex5-14 type low high))
   ((eq fn-name '5-15) (pcs-subcomplex5-15 type low high))
   ((eq fn-name '5-16) (pcs-subcomplex5-16 type low high))
   ((eq fn-name '5-z17) (pcs-subcomplex5-z17 type low high))
   ((eq fn-name '5-z18) (pcs-subcomplex5-z18 type low high))
   ((eq fn-name '5-19) (pcs-subcomplex5-19 type low high))
   ((eq fn-name '5-20) (pcs-subcomplex5-20 type low high))
   ((eq fn-name '5-21) (pcs-subcomplex5-21 type low high))
   ((eq fn-name '5-22) (pcs-subcomplex5-22 type low high))
   ((eq fn-name '5-23) (pcs-subcomplex5-23 type low high))
   ((eq fn-name '5-24) (pcs-subcomplex5-24 type low high))
   ((eq fn-name '5-25) (pcs-subcomplex5-25 type low high))
   ((eq fn-name '5-26) (pcs-subcomplex5-26 type low high))
   ((eq fn-name '5-27) (pcs-subcomplex5-27 type low high))
   ((eq fn-name '5-28) (pcs-subcomplex5-28 type low high))
   ((eq fn-name '5-29) (pcs-subcomplex5-29 type low high))
   ((eq fn-name '5-30) (pcs-subcomplex5-30 type low high))
   ((eq fn-name '5-31) (pcs-subcomplex5-31 type low high))
   ((eq fn-name '5-32) (pcs-subcomplex5-32 type low high))
   ((eq fn-name '5-33) (pcs-subcomplex5-33 type low high))
   ((eq fn-name '5-34) (pcs-subcomplex5-34 type low high))
   ((eq fn-name '5-35) (pcs-subcomplex5-35 type low high))
   ((eq fn-name '5-z36) (pcs-subcomplex5-z36 type low high))
   ((eq fn-name '5-z37) (pcs-subcomplex5-z37 type low high))
   ((eq fn-name '5-z38) (pcs-subcomplex5-z38 type low high))
   ((eq fn-name '6-1) (pcs-subcomplex6-1 type low high))
   ((eq fn-name '6-2) (pcs-subcomplex6-2 type low high))
   ((eq fn-name '6-z3) (pcs-subcomplex6-z3 type low high))
   ((eq fn-name '6-z4) (pcs-subcomplex6-z4 type low high))
   ((eq fn-name '6-5) (pcs-subcomplex6-5 type low high))
   ((eq fn-name '6-z6) (pcs-subcomplex6-z6 type low high))
   ((eq fn-name '6-7) (pcs-subcomplex6-7 type low high))
   ((eq fn-name '6-8) (pcs-subcomplex6-8 type low high))
   ((eq fn-name '6-9) (pcs-subcomplex6-9 type low high))
   ((eq fn-name '6-z10) (pcs-subcomplex6-z10 type low high))
   ((eq fn-name '6-z11) (pcs-subcomplex6-z11 type low high))
   ((eq fn-name '6-z12) (pcs-subcomplex6-z12 type low high))
   ((eq fn-name '6-z13) (pcs-subcomplex6-z13 type low high))
   ((eq fn-name '6-14) (pcs-subcomplex6-14 type low high))
   ((eq fn-name '6-15) (pcs-subcomplex6-15 type low high))
   ((eq fn-name '6-16) (pcs-subcomplex6-16 type low high))
   ((eq fn-name '6-z17) (pcs-subcomplex6-z17 type low high))
   ((eq fn-name '6-18) (pcs-subcomplex6-18 type low high))
   ((eq fn-name '6-z19) (pcs-subcomplex6-z19 type low high))
   ((eq fn-name '6-20) (pcs-subcomplex6-20 type low high))
   ((eq fn-name '6-21) (pcs-subcomplex6-21 type low high))
   ((eq fn-name '6-22) (pcs-subcomplex6-22 type low high))
   ((eq fn-name '6-z23) (pcs-subcomplex6-z23 type low high))
   ((eq fn-name '6-z24) (pcs-subcomplex6-z24 type low high))
   ((eq fn-name '6-z25) (pcs-subcomplex6-z25 type low high))
   ((eq fn-name '6-z26) (pcs-subcomplex6-z26 type low high))
   ((eq fn-name '6-27) (pcs-subcomplex6-27 type low high))
   ((eq fn-name '6-z28) (pcs-subcomplex6-z28 type low high))
   ((eq fn-name '6-z29) (pcs-subcomplex6-z29 type low high))
   ((eq fn-name '6-30) (pcs-subcomplex6-30 type low high))
   ((eq fn-name '6-31) (pcs-subcomplex6-31 type low high))
   ((eq fn-name '6-32) (pcs-subcomplex6-32 type low high))
   ((eq fn-name '6-33) (pcs-subcomplex6-33 type low high))
   ((eq fn-name '6-34) (pcs-subcomplex6-34 type low high))
   ((eq fn-name '6-35) (pcs-subcomplex6-35 type low high))
   ((eq fn-name '6-z36) (pcs-subcomplex6-z36 type low high))
   ((eq fn-name '6-z37) (pcs-subcomplex6-z37 type low high))
   ((eq fn-name '6-z38) (pcs-subcomplex6-z38 type low high))
   ((eq fn-name '6-z39) (pcs-subcomplex6-z39 type low high))
   ((eq fn-name '6-z40) (pcs-subcomplex6-z40 type low high))
   ((eq fn-name '6-z41) (pcs-subcomplex6-z41 type low high))
   ((eq fn-name '6-z42) (pcs-subcomplex6-z42 type low high))
   ((eq fn-name '6-z43) (pcs-subcomplex6-z43 type low high))
   ((eq fn-name '6-z44) (pcs-subcomplex6-z44 type low high))
   ((eq fn-name '6-z45) (pcs-subcomplex6-z45 type low high))
   ((eq fn-name '6-z46) (pcs-subcomplex6-z46 type low high))
   ((eq fn-name '6-z47) (pcs-subcomplex6-z47 type low high))
   ((eq fn-name '6-z48) (pcs-subcomplex6-z48 type low high))
   ((eq fn-name '6-z49) (pcs-subcomplex6-z49 type low high))
   ((eq fn-name '6-z50) (pcs-subcomplex6-z50 type low high))))

(defun PCS-SUBCOMPLEX4-1 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2)))
       ((eq ph 4) (pcs-builder 3 '(1 2)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2)) (pcs-builder 5 '(1 2 4 5)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2)) (pcs-builder 5 '(1 2 4 5)) (pcs-builder 6 '(1 2 z3 z36 5 8 9)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(1 2 4 5)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(1 2 4 5)) (pcs-builder 6 '(1 2 z3 z36 5 8 9)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(1 2 4 5)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(1 2 4 5)) (pcs-builder 6 '(1 2 z3 z36 5 8 9)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2 z3 z36 5 8 9)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(1 2))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2)) (pcs-builder 5 '(1 2 4 5))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2)) (pcs-builder 5 '(1 2 4 5)) (pcs-builder 6 '(1 2 z3 z36 5 8 9))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(1 2 4 5))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(1 2 4 5)) (pcs-builder 6 '(1 2 z3 z36 5 8 9))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(1 2 4 5))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(1 2 4 5)) (pcs-builder 6 '(1 2 z3 z36 5 8 9))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2 z3 z36 5 8 9))))))))))

(defun PCS-SUBCOMPLEX4-2 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 6)))
       ((eq ph 4) (pcs-builder 3 '(1 2 3 6)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 5 '(1 2 3 8 9 11 13 z36)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 5 '(1 2 3 8 9 11 13 z36)) (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(1 2 3 8 9 11 13 z36)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(1 2 3 8 9 11 13 z36)) (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(1 2 3 8 9 11 13 z36)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(1 2 3 8 9 11 13 z36)) (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 6))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(1 2 3 6))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 5 '(1 2 3 8 9 11 13 z36))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 5 '(1 2 3 8 9 11 13 z36)) (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(1 2 3 8 9 11 13 z36))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(1 2 3 8 9 11 13 z36)) (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(1 2 3 8 9 11 13 z36))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(1 2 3 8 9 11 13 z36)) (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(z3 z36 z4 z37 8 9 z10 z39 z11 z40 14 15 16 21 22))))))))))

(defun PCS-SUBCOMPLEX4-3 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3)))
       ((eq ph 4) (pcs-builder 3 '(2 3)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3)) (pcs-builder 5 '(1 3 10 16 z17)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3)) (pcs-builder 5 '(1 3 10 16 z17)) (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(1 3 10 16 z17)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(1 3 10 16 z17)) (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(1 3 10 16 z17)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(1 3 10 16 z17)) (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(2 3))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3)) (pcs-builder 5 '(1 3 10 16 z17))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3)) (pcs-builder 5 '(1 3 10 16 z17)) (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(1 3 10 16 z17))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(1 3 10 16 z17)) (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(1 3 10 16 z17))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(1 3 10 16 z17)) (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2 z3 z36 z4 z37 14 15 27))))))))))

(defun PCS-SUBCOMPLEX4-4 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 3 4 7)))
       ((eq ph 4) (pcs-builder 3 '(1 3 4 7)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 3 4 7)) (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 3 4 7)) (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)) (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)) (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)) (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 3 4 7))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(1 3 4 7))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 7)) (pcs-builder 5 '(2 3 4 6 11 14 z37 z38))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 7)) (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)) (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(2 3 4 6 11 14 z37 z38))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)) (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(2 3 4 6 11 14 z37 z38))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(2 3 4 6 11 14 z37 z38)) (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2 z3 z36 5 8 9 z10 z39 z11 z40 14 15 16 18))))))))))

(defun PCS-SUBCOMPLEX4-5 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 4 5 8)))
       ((eq ph 4) (pcs-builder 3 '(1 4 5 8)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8)) (pcs-builder 5 '(4 5 6 7 9 13 15 z38)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8)) (pcs-builder 5 '(4 5 6 7 9 13 15 z38)) (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(4 5 6 7 9 13 15 z38)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(4 5 6 7 9 13 15 z38)) (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(4 5 6 7 9 13 15 z38)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(4 5 6 7 9 13 15 z38)) (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 4 5 8))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(1 4 5 8))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8)) (pcs-builder 5 '(4 5 6 7 9 13 15 z38))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8)) (pcs-builder 5 '(4 5 6 7 9 13 15 z38)) (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(4 5 6 7 9 13 15 z38))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(4 5 6 7 9 13 15 z38)) (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(4 5 6 7 9 13 15 z38))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(4 5 6 7 9 13 15 z38)) (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 z3 z36 z4 z37 5 z6 z38 7 9 z12 z41 15 16 z17 z43 18 21 22))))))))))

(defun PCS-SUBCOMPLEX4-6 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 5 9)))
       ((eq ph 4) (pcs-builder 3 '(1 5 9)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 5 9)) (pcs-builder 5 '(5 7 14 z36)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 5 9)) (pcs-builder 5 '(5 7 14 z36)) (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(5 7 14 z36)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(5 7 14 z36)) (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(5 7 14 z36)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(5 7 14 z36)) (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 5 9))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(1 5 9))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 5 9)) (pcs-builder 5 '(5 7 14 z36))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 5 9)) (pcs-builder 5 '(5 7 14 z36)) (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(5 7 14 z36))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(5 7 14 z36)) (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(5 7 14 z36))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(5 7 14 z36)) (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 z6 z38 7 9 z11 z40 z12 z41 18))))))))))

(defun PCS-SUBCOMPLEX4-7 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4)))
       ((eq ph 4) (pcs-builder 3 '(3 4)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4)) (pcs-builder 5 '(3 6 z18 21)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4)) (pcs-builder 5 '(3 6 z18 21)) (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(3 6 z18 21)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(3 6 z18 21)) (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(3 6 z18 21)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(3 6 z18 21)) (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(3 4))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4)) (pcs-builder 5 '(3 6 z18 21))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4)) (pcs-builder 5 '(3 6 z18 21)) (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(3 6 z18 21))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(3 6 z18 21)) (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(3 6 z18 21))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(3 6 z18 21)) (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 5 14 15 16 z19 z44 20 31))))))))))

(defun PCS-SUBCOMPLEX4-8 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(4 5)))
       ((eq ph 4) (pcs-builder 3 '(4 5)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(4 5)) (pcs-builder 5 '(6 7 20 22)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(4 5)) (pcs-builder 5 '(6 7 20 22)) (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(6 7 20 22)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(6 7 20 22)) (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(6 7 20 22)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(6 7 20 22)) (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(4 5))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(4 5))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5)) (pcs-builder 5 '(6 7 20 22))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5)) (pcs-builder 5 '(6 7 20 22)) (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(6 7 20 22))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(6 7 20 22)) (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(6 7 20 22))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(6 7 20 22)) (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 z6 z38 7 16 z17 z43 18 z19 z44))))))))))

(defun PCS-SUBCOMPLEX4-9 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(5)))
       ((eq ph 4) (pcs-builder 3 '(5)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(5)) (pcs-builder 5 '(7 19)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(5)) (pcs-builder 5 '(7 19)) (pcs-builder 6 '(5 z6 z38 7 18 30)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(7 19)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(7 19)) (pcs-builder 6 '(5 z6 z38 7 18 30)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(7 19)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(7 19)) (pcs-builder 6 '(5 z6 z38 7 18 30)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 z6 z38 7 18 30)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(5))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(5))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(5)) (pcs-builder 5 '(7 19))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(5)) (pcs-builder 5 '(7 19)) (pcs-builder 6 '(5 z6 z38 7 18 30))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(7 19))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(7 19)) (pcs-builder 6 '(5 z6 z38 7 18 30))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(7 19))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(7 19)) (pcs-builder 6 '(5 z6 z38 7 18 30))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 z6 z38 7 18 30))))))))))

(defun PCS-SUBCOMPLEX4-10 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 7)))
       ((eq ph 4) (pcs-builder 3 '(2 7)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 7)) (pcs-builder 5 '(2 10 23 25)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 7)) (pcs-builder 5 '(2 10 23 25)) (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(2 10 23 25)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(2 10 23 25)) (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(2 10 23 25)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(2 10 23 25)) (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 7))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(2 7))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 7)) (pcs-builder 5 '(2 10 23 25))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 7)) (pcs-builder 5 '(2 10 23 25)) (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(2 10 23 25))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(2 10 23 25)) (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(2 10 23 25))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(2 10 23 25)) (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2 8 9 z11 z40 27 32 33))))))))))

(defun PCS-SUBCOMPLEX4-11 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 6 7)))
       ((eq ph 4) (pcs-builder 3 '(2 4 6 7)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7)) (pcs-builder 5 '(2 3 9 23 24 26 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7)) (pcs-builder 5 '(2 3 9 23 24 26 27)) (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(2 3 9 23 24 26 27)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(2 3 9 23 24 26 27)) (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(2 3 9 23 24 26 27)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(2 3 9 23 24 26 27)) (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 6 7))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(2 4 6 7))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7)) (pcs-builder 5 '(2 3 9 23 24 26 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7)) (pcs-builder 5 '(2 3 9 23 24 26 27)) (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(2 3 9 23 24 26 27))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(2 3 9 23 24 26 27)) (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(2 3 9 23 24 26 27))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(2 3 9 23 24 26 27)) (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2 8 9 z10 z39 z11 z40 14 15 21 22 z24 z46 31 32 33 34))))))))))

(defun PCS-SUBCOMPLEX4-12 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 8 10)))
       ((eq ph 4) (pcs-builder 3 '(2 3 8 10)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 8 10)) (pcs-builder 5 '(4 8 10 16 z18 26 28 31)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 8 10)) (pcs-builder 5 '(4 8 10 16 z18 26 28 31)) (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(4 8 10 16 z18 26 28 31)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(4 8 10 16 z18 26 28 31)) (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(4 8 10 16 z18 26 28 31)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(4 8 10 16 z18 26 28 31)) (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 8 10))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(2 3 8 10))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 8 10)) (pcs-builder 5 '(4 8 10 16 z18 26 28 31))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 8 10)) (pcs-builder 5 '(4 8 10 16 z18 26 28 31)) (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(4 8 10 16 z18 26 28 31))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(4 8 10 16 z18 26 28 31)) (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(4 8 10 16 z18 26 28 31))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(4 8 10 16 z18 26 28 31)) (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 z3 z36 5 z10 z39 z13 z42 15 21 z23 z45 27 z28 z49 30 31 34))))))))))

(defun PCS-SUBCOMPLEX4-13 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 5 7 10)))
       ((eq ph 4) (pcs-builder 3 '(2 5 7 10)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 5 7 10)) (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 5 7 10)) (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)) (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)) (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)) (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 5 7 10))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(2 5 7 10))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 5 7 10)) (pcs-builder 5 '(4 10 z12 19 25 29 31 z36))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 5 7 10)) (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)) (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(4 10 z12 19 25 29 31 z36))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)) (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(4 10 z12 19 25 29 31 z36))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(4 10 z12 19 25 29 31 z36)) (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 z3 z36 5 z11 z40 z12 z41 z13 z42 18 z23 z45 z25 z47 27 z29 z50 30 33))))))))))

(defun PCS-SUBCOMPLEX4-14 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 9 11)))
       ((eq ph 4) (pcs-builder 3 '(2 4 9 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 9 11)) (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 9 11)) (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)) (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)) (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)) (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 9 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(2 4 9 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 9 11)) (pcs-builder 5 '(5 11 z17 z18 20 23 27 29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 9 11)) (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)) (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(5 11 z17 z18 20 23 27 29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)) (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(5 11 z17 z18 20 23 27 29))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(5 11 z17 z18 20 23 27 29)) (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 8 9 z11 z40 14 16 18 z24 z46 z25 z47 31 32 33))))))))))

(defun PCS-SUBCOMPLEX4-Z15 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 5 7 8)))
       ((eq ph 4) (pcs-builder 3 '(3 5 7 8)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 5 7 8)) (pcs-builder 5 '(6 9 10 14 19 28 30 32)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 5 7 8)) (pcs-builder 5 '(6 9 10 14 19 28 30 32)) (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(6 9 10 14 19 28 30 32)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(6 9 10 14 19 28 30 32)) (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(6 9 10 14 19 28 30 32)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(6 9 10 14 19 28 30 32)) (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 5 7 8))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(3 5 7 8))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 5 7 8)) (pcs-builder 5 '(6 9 10 14 19 28 30 32))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 5 7 8)) (pcs-builder 5 '(6 9 10 14 19 28 30 32)) (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(6 9 10 14 19 28 30 32))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(6 9 10 14 19 28 30 32)) (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(6 9 10 14 19 28 30 32))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(6 9 10 14 19 28 30 32)) (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 5 9 z12 z41 16 z17 z43 18 21 22 z24 z46 27 30 31 34))))))))))

(defun PCS-SUBCOMPLEX4-16 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(4 5 8 9)))
       ((eq ph 4) (pcs-builder 3 '(4 5 8 9)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(4 5 8 9)) (pcs-builder 5 '(7 14 15 z18 20 24 29 30)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(4 5 8 9)) (pcs-builder 5 '(7 14 15 z18 20 24 29 30)) (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(7 14 15 z18 20 24 29 30)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(7 14 15 z18 20 24 29 30)) (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(7 14 15 z18 20 24 29 30)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(7 14 15 z18 20 24 29 30)) (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(4 5 8 9))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(4 5 8 9))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 8 9)) (pcs-builder 5 '(7 14 15 z18 20 24 29 30))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 8 9)) (pcs-builder 5 '(7 14 15 z18 20 24 29 30)) (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(7 14 15 z18 20 24 29 30))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(7 14 15 z18 20 24 29 30)) (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(7 14 15 z18 20 24 29 30))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(7 14 15 z18 20 24 29 30)) (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 z6 z38 7 9 z12 z41 16 z17 z43 18 22 z25 z47 z26 z48 31 33 34))))))))))

(defun PCS-SUBCOMPLEX4-17 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 11)))
       ((eq ph 4) (pcs-builder 3 '(3 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 11)) (pcs-builder 5 '(11 16 21 32)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 11)) (pcs-builder 5 '(11 16 21 32)) (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(11 16 21 32)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(11 16 21 32)) (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(11 16 21 32)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(11 16 21 32)) (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(3 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 11)) (pcs-builder 5 '(11 16 21 32))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 11)) (pcs-builder 5 '(11 16 21 32)) (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(11 16 21 32))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(11 16 21 32)) (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(11 16 21 32))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(11 16 21 32)) (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(8 14 15 16 z19 z44 20 27 31))))))))))

(defun PCS-SUBCOMPLEX4-18 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 5 10 11)))
       ((eq ph 4) (pcs-builder 3 '(3 5 10 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 5 10 11)) (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 5 10 11)) (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)) (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)) (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)) (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 5 10 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(3 5 10 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 5 10 11)) (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 5 10 11)) (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)) (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)) (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(16 z18 19 22 31 32 z36 z38)) (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 z11 z40 z13 z42 15 z17 z43 18 z19 z44 27 z28 z49 z29 z50 30 31))))))))))

(defun PCS-SUBCOMPLEX4-19 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4 11 12)))
       ((eq ph 4) (pcs-builder 3 '(3 4 11 12)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 5 '(13 z17 21 22 26 30 z37)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 5 '(13 z17 21 22 26 30 z37)) (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(13 z17 21 22 26 30 z37)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(13 z17 21 22 26 30 z37)) (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(13 z17 21 22 26 30 z37)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(13 z17 21 22 26 30 z37)) (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4 11 12))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(3 4 11 12))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 5 '(13 z17 21 22 26 30 z37))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 5 '(13 z17 21 22 26 30 z37)) (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(13 z17 21 22 26 30 z37))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(13 z17 21 22 26 30 z37)) (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(13 z17 21 22 26 30 z37))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(13 z17 21 22 26 30 z37)) (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14 15 16 z19 z44 20 21 22 31 34))))))))))

(defun PCS-SUBCOMPLEX4-20 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(4 11)))
       ((eq ph 4) (pcs-builder 3 '(4 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(4 11)) (pcs-builder 5 '(20 21 27 z38)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(4 11)) (pcs-builder 5 '(20 21 27 z38)) (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(20 21 27 z38)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(20 21 27 z38)) (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(20 21 27 z38)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(20 21 27 z38)) (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(4 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(4 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 11)) (pcs-builder 5 '(20 21 27 z38))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 11)) (pcs-builder 5 '(20 21 27 z38)) (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(20 21 27 z38))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(20 21 27 z38)) (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(20 21 27 z38))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(20 21 27 z38)) (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14 15 16 18 z19 z44 20 31 32))))))))))

(defun PCS-SUBCOMPLEX4-21 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(6 8)))
       ((eq ph 4) (pcs-builder 3 '(6 8)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(6 8)) (pcs-builder 5 '(8 9 24 33 34)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(6 8)) (pcs-builder 5 '(8 9 24 33 34)) (pcs-builder 6 '(2 9 21 22 33 34 35)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(8 9 24 33 34)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(8 9 24 33 34)) (pcs-builder 6 '(2 9 21 22 33 34 35)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(8 9 24 33 34)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(8 9 24 33 34)) (pcs-builder 6 '(2 9 21 22 33 34 35)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 9 21 22 33 34 35)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(6 8))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(6 8))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8)) (pcs-builder 5 '(8 9 24 33 34))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8)) (pcs-builder 5 '(8 9 24 33 34)) (pcs-builder 6 '(2 9 21 22 33 34 35))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(8 9 24 33 34))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(8 9 24 33 34)) (pcs-builder 6 '(2 9 21 22 33 34 35))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(8 9 24 33 34))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(8 9 24 33 34)) (pcs-builder 6 '(2 9 21 22 33 34 35))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 9 21 22 33 34 35))))))))))

(defun PCS-SUBCOMPLEX4-22 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(6 7 9 11)))
       ((eq ph 4) (pcs-builder 3 '(6 7 9 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 5 '(11 23 24 27 30 34 35 z36)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 5 '(11 23 24 27 30 34 35 z36)) (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(11 23 24 27 30 34 35 z36)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(11 23 24 27 30 34 35 z36)) (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(11 23 24 27 30 34 35 z36)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(11 23 24 27 30 34 35 z36)) (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(6 7 9 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(6 7 9 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 5 '(11 23 24 27 30 34 35 z36))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 5 '(11 23 24 27 30 34 35 z36)) (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(11 23 24 27 30 34 35 z36))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(11 23 24 27 30 34 35 z36)) (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(11 23 24 27 30 34 35 z36))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(11 23 24 27 30 34 35 z36)) (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(8 9 z11 z40 14 16 22 z24 z46 z25 z47 z26 z48 31 32 33 34))))))))))

(defun PCS-SUBCOMPLEX4-23 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(7 9)))
       ((eq ph 4) (pcs-builder 3 '(7 9)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(7 9)) (pcs-builder 5 '(14 23 29 35)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(7 9)) (pcs-builder 5 '(14 23 29 35)) (pcs-builder 6 '(8 9 18 z25 z47 32 33)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(14 23 29 35)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(14 23 29 35)) (pcs-builder 6 '(8 9 18 z25 z47 32 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(14 23 29 35)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(14 23 29 35)) (pcs-builder 6 '(8 9 18 z25 z47 32 33)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(8 9 18 z25 z47 32 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(7 9))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(7 9))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(7 9)) (pcs-builder 5 '(14 23 29 35))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(7 9)) (pcs-builder 5 '(14 23 29 35)) (pcs-builder 6 '(8 9 18 z25 z47 32 33))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(14 23 29 35))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(14 23 29 35)) (pcs-builder 6 '(8 9 18 z25 z47 32 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(14 23 29 35))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(14 23 29 35)) (pcs-builder 6 '(8 9 18 z25 z47 32 33))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(8 9 18 z25 z47 32 33))))))))))

(defun PCS-SUBCOMPLEX4-24 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(6 8 12)))
       ((eq ph 4) (pcs-builder 3 '(6 8 12)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 5 '(13 26 30 33)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 5 '(13 26 30 33)) (pcs-builder 6 '(15 16 21 22 31 34 35)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(13 26 30 33)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(13 26 30 33)) (pcs-builder 6 '(15 16 21 22 31 34 35)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(13 26 30 33)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(13 26 30 33)) (pcs-builder 6 '(15 16 21 22 31 34 35)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 16 21 22 31 34 35)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(6 8 12))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(6 8 12))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 5 '(13 26 30 33))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 5 '(13 26 30 33)) (pcs-builder 6 '(15 16 21 22 31 34 35))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(13 26 30 33))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(13 26 30 33)) (pcs-builder 6 '(15 16 21 22 31 34 35))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(13 26 30 33))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(13 26 30 33)) (pcs-builder 6 '(15 16 21 22 31 34 35))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 16 21 22 31 34 35))))))))))

(defun PCS-SUBCOMPLEX4-25 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(8)))
       ((eq ph 4) (pcs-builder 3 '(8)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(8)) (pcs-builder 5 '(15 28 33)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(8)) (pcs-builder 5 '(15 28 33)) (pcs-builder 6 '(7 21 22 30 34 35)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(15 28 33)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(15 28 33)) (pcs-builder 6 '(7 21 22 30 34 35)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(15 28 33)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(15 28 33)) (pcs-builder 6 '(7 21 22 30 34 35)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(7 21 22 30 34 35)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(8))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(8))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(8)) (pcs-builder 5 '(15 28 33))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(8)) (pcs-builder 5 '(15 28 33)) (pcs-builder 6 '(7 21 22 30 34 35))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(15 28 33))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(15 28 33)) (pcs-builder 6 '(7 21 22 30 34 35))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(15 28 33))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(15 28 33)) (pcs-builder 6 '(7 21 22 30 34 35))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(7 21 22 30 34 35))))))))))

(defun PCS-SUBCOMPLEX4-26 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(7 11)))
       ((eq ph 4) (pcs-builder 3 '(7 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(7 11)) (pcs-builder 5 '(25 27 32 35 z37)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(7 11)) (pcs-builder 5 '(25 27 32 35 z37)) (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(25 27 32 35 z37)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(25 27 32 35 z37)) (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(25 27 32 35 z37)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(25 27 32 35 z37)) (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(7 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(7 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(7 11)) (pcs-builder 5 '(25 27 32 35 z37))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(7 11)) (pcs-builder 5 '(25 27 32 35 z37)) (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(25 27 32 35 z37))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(25 27 32 35 z37)) (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(25 27 32 35 z37))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(25 27 32 35 z37)) (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14 z25 z47 z26 z48 27 31 32 33))))))))))

(defun PCS-SUBCOMPLEX4-27 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(7 8 10 11)))
       ((eq ph 4) (pcs-builder 3 '(7 8 10 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(7 8 10 11)) (pcs-builder 5 '(25 26 28 29 31 32 34 z38)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(7 8 10 11)) (pcs-builder 5 '(25 26 28 29 31 32 34 z38)) (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(25 26 28 29 31 32 34 z38)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(25 26 28 29 31 32 34 z38)) (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(25 26 28 29 31 32 34 z38)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(25 26 28 29 31 32 34 z38)) (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(7 8 10 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(7 8 10 11)) (pcs-builder 5 '(25 26 28 29 31 32 34 z38))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(7 8 10 11)) (pcs-builder 5 '(25 26 28 29 31 32 34 z38)) (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(25 26 28 29 31 32 34 z38))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(25 26 28 29 31 32 34 z38)) (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(25 26 28 29 31 32 34 z38))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(25 26 28 29 31 32 34 z38)) (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 18 21 z23 z45 z24 z46 z25 z47 27 z28 z49 z29 z50 30 31 33 34))))))))))

(defun PCS-SUBCOMPLEX4-28 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(10)))
       ((eq ph 4) (pcs-builder 3 '(10)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(10)) (pcs-builder 5 '(31)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(10)) (pcs-builder 5 '(31)) (pcs-builder 6 '(27 30)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(31)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(31)) (pcs-builder 6 '(27 30)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(31)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(31)) (pcs-builder 6 '(27 30)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(27 30)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(10))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(10))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(10)) (pcs-builder 5 '(31))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(10)) (pcs-builder 5 '(31)) (pcs-builder 6 '(27 30))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(31))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(31)) (pcs-builder 6 '(27 30))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(31))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(31)) (pcs-builder 6 '(27 30))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(27 30))))))))))

(defun PCS-SUBCOMPLEX4-Z29 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 5 8 11)))
       ((eq ph 4) (pcs-builder 3 '(2 5 8 11)))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 5 8 11)) (pcs-builder 5 '(5 13 16 19 20 24 25 28)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 5 8 11)) (pcs-builder 5 '(5 13 16 19 20 24 25 28)) (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 5 ph) (pcs-builder 5 '(5 13 16 19 20 24 25 28)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 5 '(5 13 16 19 20 24 25 28)) (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(5 13 16 19 20 24 25 28)))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 5 '(5 13 16 19 20 24 25 28)) (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34)))))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 5 8 11))))
         ((eq ph 4) (pcs type (pcs-builder 3 '(2 5 8 11))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 5 8 11)) (pcs-builder 5 '(5 13 16 19 20 24 25 28))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 5 8 11)) (pcs-builder 5 '(5 13 16 19 20 24 25 28)) (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 5 ph) (pcs type (pcs-builder 5 '(5 13 16 19 20 24 25 28))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 5 '(5 13 16 19 20 24 25 28)) (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(5 13 16 19 20 24 25 28))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 5 '(5 13 16 19 20 24 25 28)) (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34))))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 9 z10 z39 z12 z41 15 16 z17 z43 18 21 22 27 30 33 34))))))))))

;    --------------- 

(defun PCS-SUBCOMPLEX5-1 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 6)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 4 '(1 2 3)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 4 '(1 2 3)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 4 '(1 2 3)) (pcs-builder 6 '(1 2)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 3)))
       ((eq 5 ph) (pcs-builder 4 '(1 2 3)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3)) (pcs-builder 6 '(1 2)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 6))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 4 '(1 2 3))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 4 '(1 2 3))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6)) (pcs-builder 4 '(1 2 3)) (pcs-builder 6 '(1 2))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 3))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(1 2 3))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3)) (pcs-builder 6 '(1 2))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2))))))))))

(defun PCS-SUBCOMPLEX5-2 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 6 7)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 4 10 11)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 4 10 11)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 4 10 11)) (pcs-builder 6 '(1 2 8 9)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 4 10 11)))
       ((eq 5 ph) (pcs-builder 4 '(1 2 4 10 11)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 4 10 11)) (pcs-builder 6 '(1 2 8 9)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2 8 9)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 2 8 9)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 6 7))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 4 10 11))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 4 10 11))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 4 10 11)) (pcs-builder 6 '(1 2 8 9))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 4 10 11))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(1 2 4 10 11))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 4 10 11)) (pcs-builder 6 '(1 2 8 9))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2 8 9))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 2 8 9))))))))))

(defun PCS-SUBCOMPLEX5-3 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 6 7)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(2 3 4 7 11)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(2 3 4 7 11)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(2 3 4 7 11)) (pcs-builder 6 '(1 14 15)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 3 4 7 11)))
       ((eq 5 ph) (pcs-builder 4 '(2 3 4 7 11)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 4 7 11)) (pcs-builder 6 '(1 14 15)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 14 15)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(1 14 15)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 6 7))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(2 3 4 7 11))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(2 3 4 7 11))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(2 3 4 7 11)) (pcs-builder 6 '(1 14 15))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 3 4 7 11))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(2 3 4 7 11))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 4 7 11)) (pcs-builder 6 '(1 14 15))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 14 15))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(1 14 15))))))))))


(defun PCS-SUBCOMPLEX5-4 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 7 8 10)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 10)) (pcs-builder 4 '(1 4 5 12 13)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 10)) (pcs-builder 4 '(1 4 5 12 13)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 10)) (pcs-builder 4 '(1 4 5 12 13)) (pcs-builder 6 '(2 z3 z36 5)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 4 5 12 13)))
       ((eq 5 ph) (pcs-builder 4 '(1 4 5 12 13)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 4 5 12 13)) (pcs-builder 6 '(2 z3 z36 5)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 z3 z36 5)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 z3 z36 5)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 7 8 10))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 10)) (pcs-builder 4 '(1 4 5 12 13))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 10)) (pcs-builder 4 '(1 4 5 12 13))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 10)) (pcs-builder 4 '(1 4 5 12 13)) (pcs-builder 6 '(2 z3 z36 5))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 4 5 12 13))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(1 4 5 12 13))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 4 5 12 13)) (pcs-builder 6 '(2 z3 z36 5))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 z3 z36 5))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 z3 z36 5))))))))))

(defun PCS-SUBCOMPLEX5-5 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 4 5 8 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 4 5 8 9 11)) (pcs-builder 4 '(1 5 6 14 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 4 5 8 9 11)) (pcs-builder 4 '(1 5 6 14 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 4 5 8 9 11)) (pcs-builder 4 '(1 5 6 14 z29)) (pcs-builder 6 '(5 9)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 5 6 14 z29)))
       ((eq 5 ph) (pcs-builder 4 '(1 5 6 14 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 5 6 14 z29)) (pcs-builder 6 '(5 9)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 9)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 9)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 4 5 8 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 4 5 8 9 11)) (pcs-builder 4 '(1 5 6 14 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 4 5 8 9 11)) (pcs-builder 4 '(1 5 6 14 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 4 5 8 9 11)) (pcs-builder 4 '(1 5 6 14 z29)) (pcs-builder 6 '(5 9))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 5 6 14 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(1 5 6 14 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 5 6 14 z29)) (pcs-builder 6 '(5 9))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 9))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 9))))))))))

(defun PCS-SUBCOMPLEX5-6 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 3 4 5 7 8)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8)) (pcs-builder 4 '(4 5 7 8 z15)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8)) (pcs-builder 4 '(4 5 7 8 z15)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8)) (pcs-builder 4 '(4 5 7 8 z15)) (pcs-builder 6 '(5 16)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(4 5 7 8 z15)))
       ((eq 5 ph) (pcs-builder 4 '(4 5 7 8 z15)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(4 5 7 8 z15)) (pcs-builder 6 '(5 16)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 16)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 16)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 3 4 5 7 8))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8)) (pcs-builder 4 '(4 5 7 8 z15))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8)) (pcs-builder 4 '(4 5 7 8 z15))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8)) (pcs-builder 4 '(4 5 7 8 z15)) (pcs-builder 6 '(5 16))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(4 5 7 8 z15))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(4 5 7 8 z15))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(4 5 7 8 z15)) (pcs-builder 6 '(5 16))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 16))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 16))))))))))

(defun PCS-SUBCOMPLEX5-7 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 4 5 8 9)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 6 '(z6 z38 7 18)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 6 8 9 16)))
       ((eq 5 ph) (pcs-builder 4 '(5 6 8 9 16)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 6 '(z6 z38 7 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(z6 z38 7 18)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(z6 z38 7 18)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 4 5 8 9))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 6 '(z6 z38 7 18))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 6 8 9 16))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(5 6 8 9 16))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 6 '(z6 z38 7 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(z6 z38 7 18))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(z6 z38 7 18))))))))))

(defun PCS-SUBCOMPLEX5-8 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 6 8 10)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6 8 10)) (pcs-builder 4 '(2 12 21)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6 8 10)) (pcs-builder 4 '(2 12 21)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 6 8 10)) (pcs-builder 4 '(2 12 21)) (pcs-builder 6 '(2 21)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 12 21)))
       ((eq 5 ph) (pcs-builder 4 '(2 12 21)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 12 21)) (pcs-builder 6 '(2 21)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 21)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 21)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 6 8 10))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6 8 10)) (pcs-builder 4 '(2 12 21))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6 8 10)) (pcs-builder 4 '(2 12 21))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 6 8 10)) (pcs-builder 4 '(2 12 21)) (pcs-builder 6 '(2 21))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 12 21))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(2 12 21))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 12 21)) (pcs-builder 6 '(2 21))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 21))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 21))))))))))

(defun PCS-SUBCOMPLEX5-9 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8)) (pcs-builder 4 '(2 5 11 z15 21)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8)) (pcs-builder 4 '(2 5 11 z15 21)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8)) (pcs-builder 4 '(2 5 11 z15 21)) (pcs-builder 6 '(2 9 21 22)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 5 11 z15 21)))
       ((eq 5 ph) (pcs-builder 4 '(2 5 11 z15 21)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 5 11 z15 21)) (pcs-builder 6 '(2 9 21 22)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 9 21 22)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 9 21 22)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8)) (pcs-builder 4 '(2 5 11 z15 21))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8)) (pcs-builder 4 '(2 5 11 z15 21))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8)) (pcs-builder 4 '(2 5 11 z15 21)) (pcs-builder 6 '(2 9 21 22))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 5 11 z15 21))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(2 5 11 z15 21))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 5 11 z15 21)) (pcs-builder 6 '(2 9 21 22))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 9 21 22))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 9 21 22))))))))))

(defun PCS-SUBCOMPLEX5-10 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10)) (pcs-builder 4 '(3 10 12 13 z15)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10)) (pcs-builder 4 '(3 10 12 13 z15)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10)) (pcs-builder 4 '(3 10 12 13 z15)) (pcs-builder 6 '(2 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(3 10 12 13 z15)))
       ((eq 5 ph) (pcs-builder 4 '(3 10 12 13 z15)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(3 10 12 13 z15)) (pcs-builder 6 '(2 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 27)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(2 27)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10)) (pcs-builder 4 '(3 10 12 13 z15))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10)) (pcs-builder 4 '(3 10 12 13 z15))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10)) (pcs-builder 4 '(3 10 12 13 z15)) (pcs-builder 6 '(2 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(3 10 12 13 z15))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(3 10 12 13 z15))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(3 10 12 13 z15)) (pcs-builder 6 '(2 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 27))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(2 27))))))))))

(defun PCS-SUBCOMPLEX5-11 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 6 7 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(2 4 14 17 22)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(2 4 14 17 22)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(2 4 14 17 22)) (pcs-builder 6 '(8 14 16)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 4 14 17 22)))
       ((eq 5 ph) (pcs-builder 4 '(2 4 14 17 22)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 14 17 22)) (pcs-builder 6 '(8 14 16)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(8 14 16)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(8 14 16)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 6 7 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(2 4 14 17 22))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(2 4 14 17 22))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(2 4 14 17 22)) (pcs-builder 6 '(8 14 16))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 4 14 17 22))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(2 4 14 17 22))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 14 17 22)) (pcs-builder 6 '(8 14 16))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(8 14 16))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(8 14 16))))))))))

(defun PCS-SUBCOMPLEX5-Z12 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 5 6 7 10)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 10)) (pcs-builder 4 '(13)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 10)) (pcs-builder 4 '(13)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 10)) (pcs-builder 4 '(13)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(13)))
       ((eq 5 ph) (pcs-builder 4 '(13)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(13)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 5 6 7 10))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 10)) (pcs-builder 4 '(13))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 10)) (pcs-builder 4 '(13))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 10)) (pcs-builder 4 '(13))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(13))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(13))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX5-13 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 8 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8 11 12)) (pcs-builder 4 '(2 5 19 24 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8 11 12)) (pcs-builder 4 '(2 5 19 24 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8 11 12)) (pcs-builder 4 '(2 5 19 24 z29)) (pcs-builder 6 '(15 16 21 22)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 5 19 24 z29)))
       ((eq 5 ph) (pcs-builder 4 '(2 5 19 24 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 5 19 24 z29)) (pcs-builder 6 '(15 16 21 22)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 16 21 22)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 16 21 22)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 8 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8 11 12)) (pcs-builder 4 '(2 5 19 24 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8 11 12)) (pcs-builder 4 '(2 5 19 24 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8 11 12)) (pcs-builder 4 '(2 5 19 24 z29)) (pcs-builder 6 '(15 16 21 22))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 5 19 24 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(2 5 19 24 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 5 19 24 z29)) (pcs-builder 6 '(15 16 21 22))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 16 21 22))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 16 21 22))))))))))

(defun PCS-SUBCOMPLEX5-14 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 3 4 5 7 8 9)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 9)) (pcs-builder 4 '(4 6 z15 16 23)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 9)) (pcs-builder 4 '(4 6 z15 16 23)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 9)) (pcs-builder 4 '(4 6 z15 16 23)) (pcs-builder 6 '(9 18)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(4 6 z15 16 23)))
       ((eq 5 ph) (pcs-builder 4 '(4 6 z15 16 23)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(4 6 z15 16 23)) (pcs-builder 6 '(9 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(9 18)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(9 18)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 3 4 5 7 8 9))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 9)) (pcs-builder 4 '(4 6 z15 16 23))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 9)) (pcs-builder 4 '(4 6 z15 16 23))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 9)) (pcs-builder 4 '(4 6 z15 16 23)) (pcs-builder 6 '(9 18))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(4 6 z15 16 23))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(4 6 z15 16 23))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(4 6 z15 16 23)) (pcs-builder 6 '(9 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(9 18))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(9 18))))))))))

(defun PCS-SUBCOMPLEX5-15 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 4 5 8 9)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 16 25)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 16 25)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 16 25)) (pcs-builder 6 '(7 22)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 16 25)))
       ((eq 5 ph) (pcs-builder 4 '(5 16 25)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 16 25)) (pcs-builder 6 '(7 22)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(7 22)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(7 22)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 4 5 8 9))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 16 25))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 16 25))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 16 25)) (pcs-builder 6 '(7 22))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 16 25))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(5 16 25))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 16 25)) (pcs-builder 6 '(7 22))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(7 22))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(7 22))))))))))

(defun PCS-SUBCOMPLEX5-16 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 8 10 11)) (pcs-builder 4 '(3 12 17 18 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 8 10 11)) (pcs-builder 4 '(3 12 17 18 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 8 10 11)) (pcs-builder 4 '(3 12 17 18 z29)) (pcs-builder 6 '(15 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(3 12 17 18 z29)))
       ((eq 5 ph) (pcs-builder 4 '(3 12 17 18 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(3 12 17 18 z29)) (pcs-builder 6 '(15 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 27)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 27)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 8 10 11)) (pcs-builder 4 '(3 12 17 18 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 8 10 11)) (pcs-builder 4 '(3 12 17 18 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 8 10 11)) (pcs-builder 4 '(3 12 17 18 z29)) (pcs-builder 6 '(15 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(3 12 17 18 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(3 12 17 18 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(3 12 17 18 z29)) (pcs-builder 6 '(15 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 27))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 27))))))))))

(defun PCS-SUBCOMPLEX5-Z17 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 4 9 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 4 9 11 12)) (pcs-builder 4 '(3 14 19)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 4 9 11 12)) (pcs-builder 4 '(3 14 19)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 4 9 11 12)) (pcs-builder 4 '(3 14 19)) (pcs-builder 6 '(14)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(3 14 19)))
       ((eq 5 ph) (pcs-builder 4 '(3 14 19)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(3 14 19)) (pcs-builder 6 '(14)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 4 9 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 9 11 12)) (pcs-builder 4 '(3 14 19))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 9 11 12)) (pcs-builder 4 '(3 14 19))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 9 11 12)) (pcs-builder 4 '(3 14 19)) (pcs-builder 6 '(14))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(3 14 19))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(3 14 19))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(3 14 19)) (pcs-builder 6 '(14))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14))))))))))

(defun PCS-SUBCOMPLEX5-Z18 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 4 58 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 4 58 9 10 11)) (pcs-builder 4 '(7 12 14 16 18)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 4 58 9 10 11)) (pcs-builder 4 '(7 12 14 16 18)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 4 58 9 10 11)) (pcs-builder 4 '(7 12 14 16 18)) (pcs-builder 6 '(5 31)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(7 12 14 16 18)))
       ((eq 5 ph) (pcs-builder 4 '(7 12 14 16 18)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(7 12 14 16 18)) (pcs-builder 6 '(5 31)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 31)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 31)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 4 58 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 58 9 10 11)) (pcs-builder 4 '(7 12 14 16 18))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 58 9 10 11)) (pcs-builder 4 '(7 12 14 16 18))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 58 9 10 11)) (pcs-builder 4 '(7 12 14 16 18)) (pcs-builder 6 '(5 31))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(7 12 14 16 18))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(7 12 14 16 18))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 12 14 16 18)) (pcs-builder 6 '(5 31))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 31))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 31))))))))))

(defun PCS-SUBCOMPLEX5-19 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 13 z15 18 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 13 z15 18 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 13 z15 18 z29)) (pcs-builder 6 '(5 18 30)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(9 13 z15 18 z29)))
       ((eq 5 ph) (pcs-builder 4 '(9 13 z15 18 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(9 13 z15 18 z29)) (pcs-builder 6 '(5 18 30)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 18 30)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(5 18 30)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 13 z15 18 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 13 z15 18 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 13 z15 18 z29)) (pcs-builder 6 '(5 18 30))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(9 13 z15 18 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(9 13 z15 18 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(9 13 z15 18 z29)) (pcs-builder 6 '(5 18 30))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 18 30))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(5 18 30))))))))))

(defun PCS-SUBCOMPLEX5-20 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 5 8 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 5 8 9 11)) (pcs-builder 4 '(8 14 16 20 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 5 8 9 11)) (pcs-builder 4 '(8 14 16 20 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 5 8 9 11)) (pcs-builder 4 '(8 14 16 20 z29)) (pcs-builder 6 '(16 18)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(8 14 16 20 z29)))
       ((eq 5 ph) (pcs-builder 4 '(8 14 16 20 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(8 14 16 20 z29)) (pcs-builder 6 '(16 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(16 18)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(16 18)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 5 8 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 8 9 11)) (pcs-builder 4 '(8 14 16 20 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 8 9 11)) (pcs-builder 4 '(8 14 16 20 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 8 9 11)) (pcs-builder 4 '(8 14 16 20 z29)) (pcs-builder 6 '(16 18))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(8 14 16 20 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(8 14 16 20 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(8 14 16 20 z29)) (pcs-builder 6 '(16 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(16 18))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(16 18))))))))))

(defun PCS-SUBCOMPLEX5-21 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)) (pcs-builder 6 '(14 15 16 z19 z44 20 31)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(7 17 19 20)))
       ((eq 5 ph) (pcs-builder 4 '(7 17 19 20)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(7 17 19 20)) (pcs-builder 6 '(14 15 16 z19 z44 20 31)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14 15 16 z19 z44 20 31)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14 15 16 z19 z44 20 31)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)) (pcs-builder 6 '(14 15 16 z19 z44 20 31))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(7 17 19 20))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(7 17 19 20))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 17 19 20)) (pcs-builder 6 '(14 15 16 z19 z44 20 31))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14 15 16 z19 z44 20 31))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14 15 16 z19 z44 20 31))))))))))

(defun PCS-SUBCOMPLEX5-22 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4 5 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(3 4 5 10 11 12)) (pcs-builder 4 '(8 18 19)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4 5 10 11 12)) (pcs-builder 4 '(8 18 19)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4 5 10 11 12)) (pcs-builder 4 '(8 18 19)) (pcs-builder 6 '(z19 z44)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(8 18 19)))
       ((eq 5 ph) (pcs-builder 4 '(8 18 19)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(8 18 19)) (pcs-builder 6 '(z19 z44)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(z19 z44)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(z19 z44)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4 5 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 10 11 12)) (pcs-builder 4 '(8 18 19))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 10 11 12)) (pcs-builder 4 '(8 18 19))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 10 11 12)) (pcs-builder 4 '(8 18 19)) (pcs-builder 6 '(z19 z44))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(8 18 19))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(8 18 19))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(8 18 19)) (pcs-builder 6 '(z19 z44))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(z19 z44))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(z19 z44))))))))))

(defun PCS-SUBCOMPLEX5-23 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 6 7 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 22 23)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 22 23)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 22 23)) (pcs-builder 6 '(8 9 32 33)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(10 11 14 22 23)))
       ((eq 5 ph) (pcs-builder 4 '(10 11 14 22 23)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(10 11 14 22 23)) (pcs-builder 6 '(8 9 32 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(8 9 32 33)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(8 9 32 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 6 7 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 22 23))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 22 23))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 22 23)) (pcs-builder 6 '(8 9 32 33))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(10 11 14 22 23))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(10 11 14 22 23))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(10 11 14 22 23)) (pcs-builder 6 '(8 9 32 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(8 9 32 33))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(8 9 32 33))))))))))

(defun PCS-SUBCOMPLEX5-24 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 5 6 7 8 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 11)) (pcs-builder 4 '(11 16 21 22 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 11)) (pcs-builder 4 '(11 16 21 22 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 11)) (pcs-builder 4 '(11 16 21 22 z29)) (pcs-builder 6 '(9 22 33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(11 16 21 22 z29)))
       ((eq 5 ph) (pcs-builder 4 '(11 16 21 22 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(11 16 21 22 z29)) (pcs-builder 6 '(9 22 33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(9 22 33 34)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(9 22 33 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 5 6 7 8 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 11)) (pcs-builder 4 '(11 16 21 22 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 11)) (pcs-builder 4 '(11 16 21 22 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 11)) (pcs-builder 4 '(11 16 21 22 z29)) (pcs-builder 6 '(9 22 33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(11 16 21 22 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(11 16 21 22 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 16 21 22 z29)) (pcs-builder 6 '(9 22 33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(9 22 33 34))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(9 22 33 34))))))))))

(defun PCS-SUBCOMPLEX5-25 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 5 7 8 10 11)) (pcs-builder 4 '(10 13 26 27 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 5 7 8 10 11)) (pcs-builder 4 '(10 13 26 27 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 5 7 8 10 11)) (pcs-builder 4 '(10 13 26 27 z29)) (pcs-builder 6 '(27 33)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(10 13 26 27 z29)))
       ((eq 5 ph) (pcs-builder 4 '(10 13 26 27 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(10 13 26 27 z29)) (pcs-builder 6 '(27 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(27 33)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(27 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 5 7 8 10 11)) (pcs-builder 4 '(10 13 26 27 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 5 7 8 10 11)) (pcs-builder 4 '(10 13 26 27 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 5 7 8 10 11)) (pcs-builder 4 '(10 13 26 27 z29)) (pcs-builder 6 '(27 33))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(10 13 26 27 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(10 13 26 27 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(10 13 26 27 z29)) (pcs-builder 6 '(27 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(27 33))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(27 33))))))))))

(defun PCS-SUBCOMPLEX5-26 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 4 6 7 8 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 4 6 7 8 10 11 12)) (pcs-builder 4 '(11 12 19 24 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 4 6 7 8 10 11 12)) (pcs-builder 4 '(11 12 19 24 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 4 6 7 8 10 11 12)) (pcs-builder 4 '(11 12 19 24 27)) (pcs-builder 6 '(15 21 31 34)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(11 12 19 24 27)))
       ((eq 5 ph) (pcs-builder 4 '(11 12 19 24 27)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(11 12 19 24 27)) (pcs-builder 6 '(15 21 31 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 21 31 34)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 21 31 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 4 6 7 8 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 6 7 8 10 11 12)) (pcs-builder 4 '(11 12 19 24 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 6 7 8 10 11 12)) (pcs-builder 4 '(11 12 19 24 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 6 7 8 10 11 12)) (pcs-builder 4 '(11 12 19 24 27)) (pcs-builder 6 '(15 21 31 34))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(11 12 19 24 27))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(11 12 19 24 27))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 12 19 24 27)) (pcs-builder 6 '(15 21 31 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 21 31 34))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 21 31 34))))))))))

(defun PCS-SUBCOMPLEX5-27 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 6 7 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(11 14 20 22 26)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(11 14 20 22 26)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(11 14 20 22 26)) (pcs-builder 6 '(14 31 32)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(11 14 20 22 26)))
       ((eq 5 ph) (pcs-builder 4 '(11 14 20 22 26)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(11 14 20 22 26)) (pcs-builder 6 '(14 31 32)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14 31 32)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14 31 32)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 6 7 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(11 14 20 22 26))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(11 14 20 22 26))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(11 14 20 22 26)) (pcs-builder 6 '(14 31 32))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(11 14 20 22 26))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(11 14 20 22 26))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 14 20 22 26)) (pcs-builder 6 '(14 31 32))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14 31 32))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14 31 32))))))))))

(defun PCS-SUBCOMPLEX5-28 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 z15 25 27 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 z15 25 27 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 z15 25 27 z29)) (pcs-builder 6 '(21 30 34)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 z15 25 27 z29)))
       ((eq 5 ph) (pcs-builder 4 '(12 z15 25 27 z29)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 z15 25 27 z29)) (pcs-builder 6 '(21 30 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(21 30 34)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(21 30 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 z15 25 27 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 z15 25 27 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 z15 25 27 z29)) (pcs-builder 6 '(21 30 34))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 z15 25 27 z29))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(12 z15 25 27 z29))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 z15 25 27 z29)) (pcs-builder 6 '(21 30 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(21 30 34))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(21 30 34))))))))))

(defun PCS-SUBCOMPLEX5-29 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 5 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 5 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 23 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 5 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 23 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 5 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 23 27)) (pcs-builder 6 '(18 z25 z47 33)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(13 14 16 23 27)))
       ((eq 5 ph) (pcs-builder 4 '(13 14 16 23 27)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(13 14 16 23 27)) (pcs-builder 6 '(18 z25 z47 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(18 z25 z47 33)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(18 z25 z47 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 5 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 23 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 23 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 23 27)) (pcs-builder 6 '(18 z25 z47 33))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(13 14 16 23 27))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(13 14 16 23 27))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 14 16 23 27)) (pcs-builder 6 '(18 z25 z47 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(18 z25 z47 33))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(18 z25 z47 33))))))))))

(defun PCS-SUBCOMPLEX5-30 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4 5 6 7 8 9 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(z15 16 19 22 24)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(z15 16 19 22 24)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(z15 16 19 22 24)) (pcs-builder 6 '(16 22 31 34)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(z15 16 19 22 24)))
       ((eq 5 ph) (pcs-builder 4 '(z15 16 19 22 24)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(z15 16 19 22 24)) (pcs-builder 6 '(16 22 31 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(16 22 31 34)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(16 22 31 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4 5 6 7 8 9 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(z15 16 19 22 24))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(z15 16 19 22 24))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(z15 16 19 22 24)) (pcs-builder 6 '(16 22 31 34))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(z15 16 19 22 24))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(z15 16 19 22 24))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(z15 16 19 22 24)) (pcs-builder 6 '(16 22 31 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(16 22 31 34))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(16 22 31 34))))))))))

(defun PCS-SUBCOMPLEX5-31 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18 27 28)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18 27 28)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18 27 28)) (pcs-builder 6 '(27 30)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 13 18 27 28)))
       ((eq 5 ph) (pcs-builder 4 '(12 13 18 27 28)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 18 27 28)) (pcs-builder 6 '(27 30)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(27 30)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(27 30)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18 27 28))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18 27 28))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18 27 28)) (pcs-builder 6 '(27 30))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 13 18 27 28))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(12 13 18 27 28))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 18 27 28)) (pcs-builder 6 '(27 30))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(27 30))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(27 30))))))))))

(defun PCS-SUBCOMPLEX5-32 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(3 5 7 8 10 11)) (pcs-builder 4 '(z15 17 18 26 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 5 7 8 10 11)) (pcs-builder 4 '(z15 17 18 26 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 5 7 8 10 11)) (pcs-builder 4 '(z15 17 18 26 27)) (pcs-builder 6 '(27 31)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(z15 17 18 26 27)))
       ((eq 5 ph) (pcs-builder 4 '(z15 17 18 26 27)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(z15 17 18 26 27)) (pcs-builder 6 '(27 31)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(27 31)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(27 31)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 5 7 8 10 11)) (pcs-builder 4 '(z15 17 18 26 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 5 7 8 10 11)) (pcs-builder 4 '(z15 17 18 26 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 5 7 8 10 11)) (pcs-builder 4 '(z15 17 18 26 27)) (pcs-builder 6 '(27 31))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(z15 17 18 26 27))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(z15 17 18 26 27))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(z15 17 18 26 27)) (pcs-builder 6 '(27 31))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(27 31))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(27 31))))))))))

(defun PCS-SUBCOMPLEX5-33 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(6 8 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)) (pcs-builder 6 '(21 22 34 35)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(21 24 25)))
       ((eq 5 ph) (pcs-builder 4 '(21 24 25)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(21 24 25)) (pcs-builder 6 '(21 22 34 35)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(21 22 34 35)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(21 22 34 35)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(6 8 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)) (pcs-builder 6 '(21 22 34 35))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(21 24 25))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(21 24 25))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(21 24 25)) (pcs-builder 6 '(21 22 34 35))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(21 22 34 35))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(21 22 34 35))))))))))

(defun PCS-SUBCOMPLEX5-34 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(6 7 8 9 10 11)) (pcs-builder 4 '(21 22 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(6 7 8 9 10 11)) (pcs-builder 4 '(21 22 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(6 7 8 9 10 11)) (pcs-builder 4 '(21 22 27)) (pcs-builder 6 '(33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(21 22 27)))
       ((eq 5 ph) (pcs-builder 4 '(21 22 27)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(21 22 27)) (pcs-builder 6 '(33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(33 34)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(33 34)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 8 9 10 11)) (pcs-builder 4 '(21 22 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 8 9 10 11)) (pcs-builder 4 '(21 22 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 8 9 10 11)) (pcs-builder 4 '(21 22 27)) (pcs-builder 6 '(33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(21 22 27))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(21 22 27))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(21 22 27)) (pcs-builder 6 '(33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(33 34))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(33 34))))))))))

(defun PCS-SUBCOMPLEX5-35 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(6 7 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 4 '(22 23 26)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 4 '(22 23 26)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 4 '(22 23 26)) (pcs-builder 6 '(32 33)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(22 23 26)))
       ((eq 5 ph) (pcs-builder 4 '(22 23 26)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(22 23 26)) (pcs-builder 6 '(32 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(32 33)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(32 33)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(6 7 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 4 '(22 23 26))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 4 '(22 23 26))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 7 9 11)) (pcs-builder 4 '(22 23 26)) (pcs-builder 6 '(32 33))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(22 23 26))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(22 23 26))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(22 23 26)) (pcs-builder 6 '(32 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(32 33))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(32 33))))))))))

(defun PCS-SUBCOMPLEX5-Z36 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 5 6 7 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 5 6 7 9 10 11)) (pcs-builder 4 '(2 6 13 18 22)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 5 6 7 9 10 11)) (pcs-builder 4 '(2 6 13 18 22)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 5 6 7 9 10 11)) (pcs-builder 4 '(2 6 13 18 22)) (pcs-builder 6 '(z11 z40)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 6 13 18 22)))
       ((eq 5 ph) (pcs-builder 4 '(2 6 13 18 22)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 6 13 18 22)) (pcs-builder 6 '(z11 z40)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(z11 z40)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(z11 z40)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 5 6 7 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 5 6 7 9 10 11)) (pcs-builder 4 '(2 6 13 18 22))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 5 6 7 9 10 11)) (pcs-builder 4 '(2 6 13 18 22))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 5 6 7 9 10 11)) (pcs-builder 4 '(2 6 13 18 22)) (pcs-builder 6 '(z11 z40))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 6 13 18 22))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(2 6 13 18 22))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 6 13 18 22)) (pcs-builder 6 '(z11 z40))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(z11 z40))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(z11 z40))))))))))

(defun PCS-SUBCOMPLEX5-Z37 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 3 4 7 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 3 4 7 11 12)) (pcs-builder 4 '(4 19 26)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 3 4 7 11 12)) (pcs-builder 4 '(4 19 26)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 3 4 7 11 12)) (pcs-builder 4 '(4 19 26)) (pcs-builder 6 '(14)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(4 19 26)))
       ((eq 5 ph) (pcs-builder 4 '(4 19 26)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(4 19 26)) (pcs-builder 6 '(14)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(14)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 3 4 7 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 7 11 12)) (pcs-builder 4 '(4 19 26))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 7 11 12)) (pcs-builder 4 '(4 19 26))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 7 11 12)) (pcs-builder 4 '(4 19 26)) (pcs-builder 6 '(14))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(4 19 26))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(4 19 26))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(4 19 26)) (pcs-builder 6 '(14))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(14))))))))))

(defun PCS-SUBCOMPLEX5-Z38 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 3 4 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 10 11)) (pcs-builder 4 '(4 5 18 20 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 10 11)) (pcs-builder 4 '(4 5 18 20 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 10 11)) (pcs-builder 4 '(4 5 18 20 27)) (pcs-builder 6 '(15 18)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(4 5 18 20 27)))
       ((eq 5 ph) (pcs-builder 4 '(4 5 18 20 27)))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(4 5 18 20 27)) (pcs-builder 6 '(15 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 18)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) (pcs-builder 6 '(15 18)))))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 3 4 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 10 11)) (pcs-builder 4 '(4 5 18 20 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 10 11)) (pcs-builder 4 '(4 5 18 20 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 3 4 5 7 8 10 11)) (pcs-builder 4 '(4 5 18 20 27)) (pcs-builder 6 '(15 18))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(4 5 18 20 27))))
         ((eq 5 ph) (pcs type (pcs-builder 4 '(4 5 18 20 27))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(4 5 18 20 27)) (pcs-builder 6 '(15 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 18))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) (pcs type (pcs-builder 6 '(15 18))))))))))

;    --------------- 

(defun PCS-SUBCOMPLEX6-1 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 6 7)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 3 4 7 10 11)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 3 4 7 10 11)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(1 2 3)))
       ((eq ph 6) (pcs-builder 5 '(1 2 3)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 6 7))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 3 4 7 10 11))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7)) (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 3 4 7 10 11))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 7 10 11)) (pcs-builder 5 '(1 2 3))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(1 2 3))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(1 2 3))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-2 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(1 2 4 8 9 10)))
       ((eq ph 6) (pcs-builder 5 '(1 2 4 8 9 10)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 10))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 10 11 12 13 z15 21)) (pcs-builder 5 '(1 2 4 8 9 10))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(1 2 4 8 9 10))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(1 2 4 8 9 10))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z3 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 3 4 5 12 13)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(4)))
       ((eq ph 6) (pcs-builder 5 '(4)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 10))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 3 4 5 12 13))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(4))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(4))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z36 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 3 4 5 12 13)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(4)))
       ((eq ph 6) (pcs-builder 5 '(4)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 10))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10)) (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 3 4 5 12 13))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 3 4 5 12 13)) (pcs-builder 5 '(4))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(4))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(4))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z4 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 8)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 3 5)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 5)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 5)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 8))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 3 5))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 5))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 5))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z37 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 8)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 3 5)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 5)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 5)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 8))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 8)) (pcs-builder 4 '(2 3 5))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 3 5))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 5))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 5))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-5 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(4 5 6 7 z18 19)))
       ((eq ph 6) (pcs-builder 5 '(4 5 6 7 z18 19)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 4 5 6 7 8 9 12 13 14 z15 16 18 z29)) (pcs-builder 5 '(4 5 6 7 z18 19))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(4 5 6 7 z18 19))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(4 5 6 7 z18 19))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z6 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 4 5 8 9)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 6 8 9 16)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(7)))
       ((eq ph 6) (pcs-builder 5 '(7)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 4 5 8 9))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 6 8 9 16))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(7))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(7))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z38 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 4 5 8 9)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 6 8 9 16)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(7)))
       ((eq ph 6) (pcs-builder 5 '(7)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 4 5 8 9))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 6 8 9 16))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16)) (pcs-builder 5 '(7))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(7))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(7))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-7 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 4 5 8 9)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16 25)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 6 8 9 16 25)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(7 15)))
       ((eq ph 6) (pcs-builder 5 '(7 15)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 4 5 8 9))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16 25))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 4 5 8 9)) (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 6 8 9 16 25))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 8 9 16 25)) (pcs-builder 5 '(7 15))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(7 15))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(7 15))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-8 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 6 7 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(1 2 4 10 11 14 17 22 23)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(1 2 4 10 11 14 17 22 23)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(1 2 4 10 11 14 17 22 23)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 4 10 11 14 17 22 23)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 4 10 11 14 17 22 23)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 4 10 11 14 17 22 23)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 6 7 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(1 2 4 10 11 14 17 22 23))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(1 2 4 10 11 14 17 22 23))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11)) (pcs-builder 4 '(1 2 4 10 11 14 17 22 23))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 4 10 11 14 17 22 23))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 4 10 11 14 17 22 23))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 4 10 11 14 17 22 23))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-9 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11)) (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11)) (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11)) (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(2 5 9 14 23 24)))
       ((eq ph 6) (pcs-builder 5 '(2 5 9 14 23 24)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11)) (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11)) (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11)) (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(1 2 4 5 6 10 11 14 z15 16 21 22 23 z29)) (pcs-builder 5 '(2 5 9 14 23 24))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(2 5 9 14 23 24))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(2 5 9 14 23 24))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z10 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 4 11 12 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 4 11 12 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z39 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 4 11 12 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11)) (pcs-builder 4 '(2 4 11 12 z29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 4 11 12 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 11 12 z29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z11 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(z36)))
       ((eq ph 6) (pcs-builder 5 '(z36)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 4 6 10 11 13 14 18 22))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(z36))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(z36))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z40 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(z36)))
       ((eq ph 6) (pcs-builder 5 '(z36)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 4 6 10 11 13 14 18 22))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 6 10 11 13 14 18 22)) (pcs-builder 5 '(z36))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(z36))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(z36))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z12 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 6 13 z15 16 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 6 13 z15 16 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z41 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 6 13 z15 16 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 6 13 z15 16 z29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 6 13 z15 16 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 6 13 z15 16 z29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z13 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 13 18)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 18)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 13 18))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 18))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z42 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 13 18)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 18)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 18)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 18))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 13 18))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 18))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 18))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))


(defun PCS-SUBCOMPLEX6-14 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 6 7 9 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11 12)) (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11 12)) (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11 12)) (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(3 11 z17 21 27 z37)))
       ((eq ph 6) (pcs-builder 5 '(3 11 z17 21 27 z37)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 6 7 9 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11 12)) (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11 12)) (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 6 7 9 11 12)) (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 4 7 11 14 17 19 20 22 26)) (pcs-builder 5 '(3 11 z17 21 27 z37))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(3 11 z17 21 27 z37))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(3 11 z17 21 27 z37))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-15 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(3 13 16 21 26 z38)))
       ((eq ph 6) (pcs-builder 5 '(3 13 16 21 26 z38)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 3 4 5 7 11 12 17 18 19 20 24 27 z29)) (pcs-builder 5 '(3 13 16 21 26 z38))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(3 13 16 21 26 z38))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(3 13 16 21 26 z38))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-16 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(6 11 13 20 21 30)))
       ((eq ph 6) (pcs-builder 5 '(6 11 13 20 21 30)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 4 5 7 8 14 z15 16 17 19 20 22 24 z29)) (pcs-builder 5 '(6 11 13 20 21 30))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(6 11 13 20 21 30))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(6 11 13 20 21 30))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z17 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 8 z15 16 18 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 8 z15 16 18 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z43 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(5 8 z15 16 18 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(5 8 z15 16 18 z29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(5 8 z15 16 18 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(5 8 z15 16 18 z29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-18 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(7 14 19 20 29 z38)))
       ((eq ph 6) (pcs-builder 5 '(7 14 19 20 29 z38)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 7 8 9 10 11)) (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(4 5 6 8 9 13 14 z15 16 18 20 23 27 z29)) (pcs-builder 5 '(7 14 19 20 29 z38))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(7 14 19 20 29 z38))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(7 14 19 20 29 z38))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z19 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4 5 8 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(7 8 17 18 19 20)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(21 22)))
       ((eq ph 6) (pcs-builder 5 '(21 22)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4 5 8 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(7 8 17 18 19 20))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(21 22))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(21 22))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z44 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4 5 8 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(7 8 17 18 19 20)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(21 22)))
       ((eq ph 6) (pcs-builder 5 '(21 22)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4 5 8 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 5 8 10 11 12)) (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(7 8 17 18 19 20))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 8 17 18 19 20)) (pcs-builder 5 '(21 22))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(21 22))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(21 22))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-20 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(3 4 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(7 17 19 20)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(21)))
       ((eq ph 6) (pcs-builder 5 '(21)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(3 4 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(3 4 11 12)) (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(7 17 19 20))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 17 19 20)) (pcs-builder 5 '(21))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(21))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(21))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-21 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(8 9 13 26 28 33)))
       ((eq ph 6) (pcs-builder 5 '(8 9 13 26 28 33)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 10 11 12)) (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 5 11 12 z15 19 21 24 25 27 z29)) (pcs-builder 5 '(8 9 13 26 28 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(8 9 13 26 28 33))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(8 9 13 26 28 33))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-22 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(9 13 15 24 30 33)))
       ((eq ph 6) (pcs-builder 5 '(9 13 15 24 30 33)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(1 2 3 4 5 6 7 8 9 11 12)) (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(2 5 11 z15 16 19 21 22 24 25 z29)) (pcs-builder 5 '(9 13 15 24 30 33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(9 13 15 24 30 33))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(9 13 15 24 30 33))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z23 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 13 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 13 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z45 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 13 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 13 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 13 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 13 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 13 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z24 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(11 14 z15 22 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(11 14 z15 22 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z46 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(11 14 z15 22 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(11 14 z15 22 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(11 14 z15 22 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 14 z15 22 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z25 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(13 14 16 22 23 26 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(29)))
       ((eq ph 6) (pcs-builder 5 '(29)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 5 6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(13 14 16 22 23 26 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(29))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(29))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z47 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(13 14 16 22 23 26 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(29)))
       ((eq ph 6) (pcs-builder 5 '(29)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 5 6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(13 14 16 22 23 26 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 14 16 22 23 26 27)) (pcs-builder 5 '(29))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(29))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(29))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z26 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(4 5 6 7 8 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(16 22 26)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(16 22 26)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(16 22 26)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(4 5 6 7 8 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(16 22 26))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(16 22 26))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(16 22 26))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z48 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(4 5 6 7 8 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(16 22 26)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(16 22 26)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(16 22 26)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(4 5 6 7 8 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(4 5 6 7 8 9 11)) (pcs-builder 4 '(16 22 26))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(16 22 26))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(16 22 26))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(16 22 26))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-27 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(10 16 25 31 32)))
       ((eq ph 6) (pcs-builder 5 '(10 16 25 31 32)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(3 10 12 13 z15 17 18 26 27 28 z29)) (pcs-builder 5 '(10 16 25 31 32))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(10 16 25 31 32))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(10 16 25 31 32))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z28 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 18 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(12 18 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 18 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 18 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 18 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 18 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z49 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(12 18 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(12 18 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(12 18 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(12 18 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(12 18 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 18 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(12 18 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z29 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(13 18 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(13 18 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(13 18 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(13 18 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 18 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 18 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-Z50 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(13 18 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(13 18 27)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(13 18 27)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) nil)
       ((eq ph 6) nil)))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(13 18 27))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(13 18 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 18 27))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(13 18 27))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) nil)
         ((eq ph 6) nil)))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-30 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 5 7 8 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(19 28 31)))
       ((eq ph 6) (pcs-builder 5 '(19 28 31)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 5 7 8 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 5 7 8 10 11)) (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(9 12 13 z15 18 25 27 28 z29)) (pcs-builder 5 '(19 28 31))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(19 28 31))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(19 28 31))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-31 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(z18 21 26 27 30 32)))
       ((eq ph 6) (pcs-builder 5 '(z18 21 26 27 30 32)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(7 11 12 14 z15 16 17 18 19 20 22 24 26 27)) (pcs-builder 5 '(z18 21 26 27 30 32))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(z18 21 26 27 30 32))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(z18 21 26 27 30 32))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-32 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 6 7 9 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 20 22 23 26)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(10 11 14 20 22 23 26)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(23 27 35)))
       ((eq ph 6) (pcs-builder 5 '(23 27 35)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 6 7 9 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 20 22 23 26))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 6 7 9 11)) (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(10 11 14 20 22 23 26))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(10 11 14 20 22 23 26)) (pcs-builder 5 '(23 27 35))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(23 27 35))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(23 27 35))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-33 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(23 24 25 29 34 35)))
       ((eq ph 6) (pcs-builder 5 '(23 24 25 29 34 35)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 4 5 6 7 8 9 10 11))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 4 5 6 7 8 9 10 11)) (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(10 11 13 14 16 21 22 23 26 27 z29)) (pcs-builder 5 '(23 24 25 29 34 35))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(23 24 25 29 34 35))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(23 24 25 29 34 35))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-34 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(24 26 28 30 33 34)))
       ((eq ph 6) (pcs-builder 5 '(24 26 28 30 33 34)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(2 3 4 5 6 7 8 9 10 11 12)) (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(11 12 z15 16 19 21 22 24 25 27 z29)) (pcs-builder 5 '(24 26 28 30 33 34))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(24 26 28 30 33 34))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(24 26 28 30 33 34))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))

(defun PCS-SUBCOMPLEX6-35 (type pl ph)
  (cond
   ((equal type :fn)
    (cond
     ((eq pl 3)
      (cond
       ((eq ph 3) (pcs-builder 3 '(6 8 12)))
       ((eq ph 4) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)))))
       ((eq ph 5) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33)))))
       ((eq ph 6) (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33)))))))
     ((eq pl 4)
      (cond
       ((eq 4 ph) (pcs-builder 4 '(21 24 25)))
       ((eq 5 ph) (pcs-flatten (list (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33)))))
       ((eq 6 ph) (pcs-flatten (list (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33)))))))
     ((eq pl 5)
      (cond
       ((eq ph 5) (pcs-builder 5 '(33)))
       ((eq ph 6) (pcs-builder 5 '(33)))))
     ((eq pl 6)
      (cond
       ((eq ph 6) nil)))))
   (t (cond
       ((eq pl 3)
        (cond
         ((eq ph 3) (pcs type (pcs-builder 3 '(6 8 12))))
         ((eq ph 4) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25))))))
         ((eq ph 5) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33))))))
         ((eq ph 6) (pcs type (pcs-flatten (list (pcs-builder 3 '(6 8 12)) (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33))))))))
       ((eq pl 4)
        (cond
         ((eq 4 ph) (pcs type (pcs-builder 4 '(21 24 25))))
         ((eq 5 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33))))))
         ((eq 6 ph) (pcs type (pcs-flatten (list (pcs-builder 4 '(21 24 25)) (pcs-builder 5 '(33))))))))
       ((eq pl 5)
        (cond
         ((eq ph 5) (pcs type (pcs-builder 5 '(33))))
         ((eq ph 6) (pcs type (pcs-builder 5 '(33))))))
       ((eq pl 6)
        (cond
         ((eq ph 6) nil)))))))


;;; -----------------------------------------------------------------------------
;;; End
;;; -----------------------------------------------------------------------------
