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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================


(in-package :om)
;===================================================
;popup + dialog used to change staff size port, etc.
;================================================
(defvar *all-satff-om*   '(f g c1 c3 c4 g_8 g^8 f2 gf gg ff gff ggf ggff empty))

(defvar *chord-satff-om* '(("F" f) ("G" g) ("G_8" g_8) ("G^8" g^8) ("C1" c1) ("C3" c3) ("C4" c4) ("GF" gf) ("GG" gg) ("FF" ff) ("GFF" gff) ("GGF" ggf) ("GGFF" ggff) ("EMPTY" empty)))
(defvar *voice-satff-om* '(("F" f) ("G" g) ("C1" c1) ("C3" c3) ("C4" c4) ("F2" f2) ("G2" g2) ("EMPTY" empty)))
(defvar *mus-font-size* '(("8" 8) ("12" 12) ("16" 16) ("20" 20) ("24" 24) ("28" 28)
                            ("36" 36) ("48" 48) ("60" 60) ("72" 72)))


(defclass* scale ()
    ((alteration-list :initform (list  nil (diese) nil (diese) nil nil (diese) nil (diese) nil (diese) nil) 
                      :initarg :alteration-list :accessor alteration-list)
     (lines-list :initform (list  0 0 1 1 2 3 3 4 4 5 5 6) 
                 :initarg :lines-list :accessor lines-list)
     (approx-factor :initform 100 :initarg :approx-factor :accessor approx-factor))
   (:icon 262))

(defmethod get-slot-in-out-names ((self scale))
   (values '("self" "alteration-list" "lines-list" "approx-factor")
           '(nil (nil #\# nil #\# nil nil #\# nil #\# nil #\# nil) (0 0 1 1 2 3 3 4 4 5 5 6) 100)
           '("scale object" "accidentals" "line position" "division of 200 (1 ton)")
           '(nil nil nil
                 (( 3 (("1" 200) ("1/2" 100) ("1/3" 200/3) ("1/4" 50) ("1/5" 40) ("1/6" 100/3) 
                       ("1/7" 200/7) ("1/8" 25) ("1/10" 20)  ("1/12" 100/6) ("1/14" 100/7) ("1/16" 25/2)
                       ("72-EDO_#" 200/12)))))))

(defmethod Class-has-editor-p  ((self scale)) nil)

(defmethod give-alteration  ((self scale) midic)
    (let* ((vlength (length (lines-list self)))
           (int-cents (mod midic 1200))
           (index (round (/ int-cents (approx-factor self) )))
           (up-octave (truncate (/ index (length (lines-list self))))))
      (setq index (mod index (length (lines-list self))))
      (if (and (= (approx-factor self) 25) (= index (1- vlength))) (incf up-octave))
      (list (nth index  (lines-list self)) (nth  index (alteration-list self)) up-octave)))

;-------1
(defvar *1-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list (list  nil  nil nil  (diese)  (diese)  (diese))
      :lines-list (list  0 1 2 3 4 5 )
      :approx-factor 200))

(defvar *1#-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list (list  (diese)  (diese) nil  nil  nil  nil)
      :lines-list (list  0 1 3 4 5 6 )
      :approx-factor 200))

;-------2
(defvar *2-tone-chromatic-scale*
   (make-instance 'scale
     :alteration-list
     (list  nil (diese) nil (diese) nil nil (diese) nil (diese) nil (diese) nil)
     :lines-list
     (list  0 0 1 1 2 3 3 4 4 5 5 6)
     :approx-factor 100))

(defvar *c-major-scale*
   (make-instance 'scale
     :alteration-list
     (list  nil (diese)  nil (bemol) nil   nil  (diese) nil (bemol) nil (bemol)  nil)
     :lines-list
     (list   0 0 1 2 2 3 3 4 5 5 6 6)
     :approx-factor 100))


;-------3
(defvar *3-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list (list  nil (t-4/12) (t-8/12)
                              nil (t-4/12) (t-8/12)
                              nil (t-4/12) (t-8/12)
                              #\# (d-1/3) (d-2/3)
                              #\# (d-1/3) (d-2/3)
                              #\# (d-1/3) (d-2/3))
      :lines-list (list  0 0 0 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)
      :approx-factor 200/3))

(defvar *3#-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list (list  #\# (d-1/3) (d-2/3)
                              #\# (d-1/3) (d-2/3)
                              nil (t-4/12) (t-8/12)
                              nil (t-4/12) (t-8/12)
                              nil (t-4/12) (t-8/12)
                              nil (t-4/12) (t-8/12))
      :lines-list (list  0 0 0 1 1 1 3 3 3  4 4 4 5 5 5 6 6 6 )
      :approx-factor 200/3))

;-------4
(defvar *4-tone-chromatic-scale*
   (make-instance 'scale
     :alteration-list
     (list  nil (t-1/4) (diese) (t-3/4) nil (t-1/4) (diese) (t-3/4) nil (t-1/4) nil (t-1/4) (diese)
            (t-3/4) nil (t-1/4) (diese) (t-3/4) nil (t-1/4) (diese) (t-3/4) nil (t-1/4))
     :lines-list
     (list  0 0 0 0 1 1 1 1 2 2 3 3 3 3 4 4 4 4 5 5 5 5 6 6)
     :approx-factor 50))

;-------5
(defvar *5-tone-chromatic-scale*
   (make-instance 'scale
     :alteration-list
     (list  nil (t-2/10) (t-4/10) (t-6/10) (t-8/10)
            nil (t-2/10) (t-4/10) (t-6/10) (t-8/10)
            nil (t-2/10) (t-4/10) (t-6/10) (t-8/10)
            #\# (d-1/5) (d-2/5) (d-3/5) (d-4/5)
            #\# (d-1/5) (d-2/5) (d-3/5) (d-4/5)
            #\# (d-1/5) (d-2/5) (d-3/5) (d-4/5))
     :lines-list
     (list  0 0 0 0 0
            1 1 1 1 1
            2 2 2 2 2
            3 3 3 3 3
            4 4 4 4 4
            5 5 5 5 5)
     :approx-factor 40))

(defvar *5#-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list (list  #\# (d-1/5) (d-2/5) (d-3/5) (d-4/5)
                              #\# (d-1/5) (d-2/5) (d-3/5) (d-4/5)
                              nil (t-2/10) (t-4/10) (t-6/10) (t-8/10)
                              nil (t-2/10) (t-4/10) (t-6/10) (t-8/10)
                              nil (t-2/10) (t-4/10) (t-6/10) (t-8/10)
                               nil (t-2/10) (t-4/10) (t-6/10) (t-8/10))
      :lines-list (list  0 0 0 0 0
                         1 1 1 1 1
                         3 3 3 3 3
                         4 4 4 4 4
                         5 5 5 5 5
                         6 6 6 6 6 )
      :approx-factor 40))

;-------6
(defvar *6-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list (list  nil (t-2/12) (t-4/12) (t-1/2) (t-8/12) (t-10/12)
                              nil (t-2/12) (t-4/12) (t-1/2) (t-8/12) (t-10/12)
                              nil (t-2/12) (t-4/12) nil (t-2/12) (t-4/12)
                              #\# (t-8/12) (t-10/12) nil (t-2/12) (t-4/12)
                              #\# (t-8/12) (t-10/12) nil (t-2/12) (t-4/12)
                              #\# (t-8/12) (t-10/12) nil (t-2/12) (t-4/12))
      :lines-list (list  0 0 0 0 0 0
                         1 1 1 1 1 1
                         2 2 2 3 3 3
                         3 3 3 4 4 4
                         4 4 4 5 5 5
                         5 5 5 6 6 6)
      :approx-factor 200/6))



;-------7
(defvar *7-tone-chromatic-scale*
   (make-instance 'scale
     :alteration-list
     (list  nil (t-2/14) (t-4/14) (t-6/14) (t-8/14) (t-10/14) (t-12/14)
            nil (t-2/14) (t-4/14) (t-6/14) (t-8/14) (t-10/14) (t-12/14)
            nil (t-2/14) (t-4/14) (t-6/14) (t-8/14) (t-10/14) (t-12/14)
            #\# (d-1/7) (d-2/7) (d-3/7) (d-4/7) (d-5/7) (d-6/7)
            #\# (d-1/7) (d-2/7) (d-3/7) (d-4/7) (d-5/7) (d-6/7)
            #\# (d-1/7) (d-2/7) (d-3/7) (d-4/7) (d-5/7) (d-6/7))
     :lines-list
     (list  0 0 0 0 0 0 0
            1 1 1 1 1 1 1
            2 2 2 2 2 2 2
            3 3 3 3 3 3 3
            4 4 4 4 4 4 4
            5 5 5 5 5 5 5)
     :approx-factor 200/7))

(defvar *7#-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list (list  #\# (d-1/7) (d-2/7) (d-3/7) (d-4/7) (d-5/7) (d-6/7)
                              #\# (d-1/7) (d-2/7) (d-3/7) (d-4/7) (d-5/7) (d-6/7)
                              nil (t-2/14) (t-4/14) (t-6/14) (t-8/14) (t-10/14) (t-12/14)
                              nil (t-2/14) (t-4/14) (t-6/14) (t-8/14) (t-10/14) (t-12/14)
                              nil (t-2/14) (t-4/14) (t-6/14) (t-8/14) (t-10/14) (t-12/14))
      :lines-list (list  0 0 0 0 0 0 0
                         1 1 1 1 1 1 1
                         3 3 3 3 3 3 3
                         4 4 4 4 4 4 4
                         5 5 5 5 5 5 5
                         6 6 6 6 6 6 6 )
      :approx-factor 200/7))

;-------8
(defvar *8-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list
      (list  nil (t-1/8) (t-1/4) (t-3/8) (t-1/2) (t-5/8) (t-3/4) (t-7/8)
             nil (t-1/8) (t-1/4) (t-3/8) (t-1/2) (t-5/8) (t-3/4) (t-7/8)
             nil (t-1/8) (t-1/4) (t-3/8)
             nil (t-1/8) (t-1/4) (t-3/8) (t-1/2) (t-5/8) (t-3/4) (t-7/8)
             nil (t-1/8) (t-1/4) (t-3/8) (t-1/2) (t-5/8) (t-3/4) (t-7/8)
             nil (t-1/8) (t-1/4) (t-3/8) (t-1/2) (t-5/8) (t-3/4) (t-7/8)
             nil (t-1/8) (t-1/4) (t-3/8) )
      :lines-list
      (list  0 0 0 0 0 0 0 0
             1 1 1 1 1 1 1 1
             2 2 2 2 3 3 3 3
             3 3 3 3 4 4 4 4
             4 4 4 4 5 5 5 5
             5 5 5 5 6 6 6 6)
      :approx-factor 25))

;-------10
(defvar *10-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list
      (list  nil (t-1/10) (t-2/10) (t-3/10) (t-4/10) (t-5/10) (t-6/10) (t-7/10) (t-8/10) (t-9/10)
             nil (t-1/10) (t-2/10) (t-3/10) (t-4/10) (t-5/10) (t-6/10) (t-7/10) (t-8/10) (t-9/10)
             nil (t-1/10) (t-2/10) (t-3/10) (t-4/10) nil (t-1/10) (t-2/10) (t-3/10) (t-4/10)
             #\# (t-6/10) (t-7/10) (t-8/10) (t-9/10)  nil (t-1/10) (t-2/10) (t-3/10) (t-4/10)
             #\# (t-6/10) (t-7/10) (t-8/10) (t-9/10) nil (t-1/10) (t-2/10) (t-3/10) (t-4/10)
             #\# (t-6/10) (t-7/10) (t-8/10) (t-9/10) nil (t-1/10) (t-2/10) (t-3/10) (t-4/10))
      :lines-list (list  0 0 0 0 0 0 0 0 0 0
                         1 1 1 1 1 1 1 1 1 1
                         2 2 2 2 2 3 3 3 3 3
                         3 3 3 3 3 4 4 4 4 4
                         4 4 4 4 4 5 5 5 5 5
                         5 5 5 5 5 6 6 6 6 6)
      :approx-factor 20))

;-------12
(defvar *12-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list
      (list  nil (t-1/12) (t-2/12) (t-3/12) (t-4/12) (t-5/12) (t-6/12) (t-7/12) (t-8/12) (t-9/12) (t-10/12) (t-11/12)
             nil (t-1/12) (t-2/12) (t-3/12) (t-4/12) (t-5/12) (t-6/12) (t-7/12) (t-8/12) (t-9/12) (t-10/12) (t-11/12)
             nil (t-1/12) (t-2/12) (t-3/12) (t-4/12) (t-5/12) nil (t-1/12)  (t-2/12) (t-3/12) (t-4/12) (t-5/12)
             #\# (t-7/12) (t-8/12) (t-9/12) (t-10/12) (t-11/12) nil (t-1/12)  (t-2/12) (t-3/12) (t-4/12) (t-5/12)
             #\# (t-7/12) (t-8/12) (t-9/12) (t-10/12) (t-11/12) nil (t-1/12)  (t-2/12) (t-3/12) (t-4/12) (t-5/12)
             #\# (t-7/12) (t-8/12) (t-9/12) (t-10/12) (t-11/12) nil (t-1/12)  (t-2/12) (t-3/12) (t-4/12) (t-5/12))
      :lines-list (list  0 0 0 0 0 0 0 0 0 0 0 0
                         1 1 1 1 1 1 1 1 1 1 1 1
                         2 2 2 2 2 2 3 3 3 3 3 3
                         3 3 3 3 3 3 4 4 4 4 4 4
                         4 4 4 4 4 4 5 5 5 5 5 5
                         5 5 5 5 5 5 6 6 6 6 6 6)
      :approx-factor 200/12))

;-------14
(defvar *14-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list
      (list  nil (t-1/14) (t-2/14) (t-3/14) (t-4/14) (t-5/14) (t-6/14) (t-7/14) (t-8/14) (t-9/14) (t-10/14) (t-11/14) (t-12/14) (t-13/14)
             nil (t-1/14) (t-2/14) (t-3/14) (t-4/14) (t-5/14) (t-6/14) (t-7/14) (t-8/14) (t-9/14) (t-10/14) (t-11/14) (t-12/14) (t-13/14)
             nil (t-1/14) (t-2/14) (t-3/14) (t-4/14) (t-5/14) (t-6/14) nil (t-1/14) (t-2/14) (t-3/14) (t-4/14) (t-5/14) (t-6/14)
             #\# (t-8/14) (t-9/14) (t-10/14) (t-11/14) (t-12/14) (t-13/14) nil (t-1/14) (t-2/14) (t-3/14) (t-4/14) (t-5/14) (t-6/14)
             #\# (t-8/14) (t-9/14) (t-10/14) (t-11/14) (t-12/14) (t-13/14) nil (t-1/14) (t-2/14) (t-3/14) (t-4/14) (t-5/14) (t-6/14)
             #\# (t-8/14) (t-9/14) (t-10/14) (t-11/14) (t-12/14) (t-13/14) nil (t-1/14) (t-2/14) (t-3/14) (t-4/14) (t-5/14) (t-6/14))
      :lines-list
      (list  0 0 0 0 0 0 0 0 0 0 0 0 0 0
             1 1 1 1 1 1 1 1 1 1 1 1 1 1
             2 2 2 2 2 2 2 3 3 3 3 3 3 3
             3 3 3 3 3 3 3 4 4 4 4 4 4 4
             4 4 4 4 4 4 4 5 5 5 5 5 5 5
             5 5 5 5 5 5 5 6 6 6 6 6 6 6)
      :approx-factor 200/14))

;-------16
(defvar *16-tone-chromatic-scale*
    (make-instance 'scale
      :alteration-list
      (list  nil (t-1/16) (t-1/8) (t-3/16) (t-1/4) (t-5/16) (t-3/8) (t-7/16) (t-1/2) (t-9/16) (t-5/8) (t-11/16) (t-3/4) (t-13/16) (t-7/8) (t-15/16)
             nil (t-1/16) (t-1/8) (t-3/16) (t-1/4) (t-5/16) (t-3/8) (t-7/16) (t-1/2) (t-9/16) (t-5/8) (t-11/16) (t-3/4) (t-13/16) (t-7/8) (t-15/16)
             nil (t-1/16) (t-1/8) (t-3/16) (t-1/4) (t-5/16) (t-3/8) (t-7/16)
             nil (t-1/16) (t-1/8) (t-3/16) (t-1/4) (t-5/16) (t-3/8) (t-7/16) (t-1/2) (t-9/16) (t-5/8) (t-11/16) (t-3/4) (t-13/16) (t-7/8) (t-15/16)
             nil (t-1/16) (t-1/8) (t-3/16) (t-1/4) (t-5/16) (t-3/8) (t-7/16) (t-1/2) (t-9/16) (t-5/8) (t-11/16) (t-3/4) (t-13/16) (t-7/8) (t-15/16)
             nil (t-1/16) (t-1/8) (t-3/16) (t-1/4) (t-5/16) (t-3/8) (t-7/16) (t-1/2) (t-9/16) (t-5/8) (t-11/16) (t-3/4) (t-13/16) (t-7/8) (t-15/16)
             nil (t-1/16) (t-1/8) (t-3/16) (t-1/4) (t-5/16) (t-3/8) (t-7/16) )
      :lines-list
      (list  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
             1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
             2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3
             3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4
             4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5
             5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6)
      :approx-factor 200/16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;-------EDO alternate (standard) notation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;=====5 EDO=====

(defvar *5-EDO_#*
  (make-instance 'scale
                 :alteration-list
                 (list nil (t-S2) (t-F1) (t-S1) (t-S6))
                 :lines-list (list 0 1 3 4 5)
                 :approx-factor 1200/5))


(defvar *5-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S2) (t-F1) (t-S1) (t-F10))
:lines-list (list 0 1 3 4 6)
:approx-factor 1200/5))

;=====6 EDO=====

(defvar *6-EDO_#*
    (make-instance 'scale
      :alteration-list (list  (diese)  (diese) nil  nil  nil  nil)
      :lines-list (list  0 1 3 4 5 6 )
      :approx-factor 200))

(defvar *6-EDO_b*
(make-instance 'scale
:alteration-list
(list nil nil nil (t-F8) (t-F8) (t-F8))
:lines-list (list 0 1 2 4 5 6)
:approx-factor 1200/6))

;=====7 EDO=====

(defvar *7-EDO*
    (make-instance 'scale
      :alteration-list
  (list nil nil nil nil nil nil nil)
      :lines-list (list 0 1 2 3 4 5 6)
      :approx-factor 1200/7))

;=====8 EDO=====

(defvar *8-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S12) (t-S8) (t-S4) (t-S8) (t-S4) nil (t-S12))
:lines-list (list 0 0 1 2 3 4 5 5)
:approx-factor 1200/8))

(defvar *8-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F4) (t-F8) (t-F4) (t-F8) (t-F12) nil (t-F4))
:lines-list (list 0 1 2 3 4 5 5 6)
:approx-factor 1200/8))

;=====9 EDO=====


(defvar *9-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S9) (t-S7) nil (t-S1) (t-F1) (t-S8) (t-S1) (t-F1))
:lines-list (list 0 0 1 2 3 4 4 5 6)
:approx-factor 1200/9))


(defvar *9-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F7) (t-F9) nil (t-S1) (t-F1) (t-F8) (t-S1) (t-F1))
:lines-list (list 0 1 2 2 3 4 5 5 6)
:approx-factor 1200/9))

;=====10 EDO=====

(defvar *10-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S9) (t-S2) (t-F2) (t-F1) (t-S8) (t-S1) (t-S10) (t-S6) (t-F1))
:lines-list (list 0 0 1 2 3 3 4 4 5 6)
:approx-factor 1200/10))


(defvar *10-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F7) (t-S2) (t-F2) (t-F1) (t-F8) (t-S1) (t-F6) (t-F10) (t-F1))
:lines-list (list 0 1 1 2 3 4 4 5 6 6)
:approx-factor 1200/10))


;=====12 EDO=====

(defvar *12-EDO_#*
   (make-instance 'scale
     :alteration-list
     (list  nil (diese) nil (diese) nil nil (diese) nil (diese) nil (diese) nil)
     :lines-list
     (list  0 0 1 1 2 3 3 4 4 5 5 6)
     :approx-factor 100))

(defvar *12-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F8) nil (t-F8) nil nil (t-F8) nil (t-F8) nil (t-F8) nil)
:lines-list (list 0 1 1 2 2 3 4 4 5 5 6 6)
:approx-factor 1200/12))

;=====14 EDO=====


(defvar *14-EDO_^*
(make-instance 'scale
:alteration-list
(list nil (t-Sa) nil (t-Sa) nil (t-Sa) nil (t-Sa) nil (t-Sa) nil (t-Sa) nil (t-Sa))
:lines-list (list 0 0 1 1 2 2 3 3 4 4 5 5 6 6)
:approx-factor 1200/14))


(defvar *14-EDO_v*
(make-instance 'scale
:alteration-list
(list nil (t-Fa) nil (t-Fa) nil (t-Fa) nil (t-Fa) nil (t-Fa) nil (t-Fa) nil (t-Fa))
:lines-list (list 0 1 1 2 2 3 3 4 4 5 5 6 6 7)
:approx-factor 1200/14))

;=====15 EDO=====

(defvar *15-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S7) (t-F2) (t-S2) (t-S9) nil (t-F1) (t-S6) (t-S10) (t-S1) (t-S8) (t-F1) (t-S6) (t-S10) (t-S1))
:lines-list (list 0 0 1 1 1 2 3 3 3 4 4 5 5 5 6)
:approx-factor 1200/15))


(defvar *15-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F9) (t-F2) (t-S2) (t-F7) nil (t-F1) (t-F10) (t-F6) (t-S1) (t-F8) (t-F1) (t-F10) (t-F6) (t-S1))
:lines-list (list 0 1 1 1 2 2 3 4 4 4 5 5 6 6 6)
:approx-factor 1200/15))

;=====16 EDO=====

(defvar *16-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S7) (t-S4) (t-S1) (t-S8) (t-F1) (t-S4) (t-S1) (t-S8) (t-F1) (t-S4) (t-S9) nil (t-S7) (t-S12) (t-S1))
:lines-list (list 0 0 0 1 1 2 2 3 3 4 4 4 5 5 5 6)
:approx-factor 1200/16))


(defvar *16-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F9) (t-F4) (t-S1) (t-F8) (t-F1) (t-F4) (t-S1) (t-F8) (t-F1) (t-F12) (t-F7) nil (t-F9) (t-F4) (t-S1))
:lines-list (list 0 1 1 1 2 2 3 3 4 4 5 5 5 6 6 6)
:approx-factor 1200/16))


;=====17 EDO=====

(defvar *17-EDO*
(make-instance 'scale
:alteration-list
(list nil (t-F8) (t-S8) nil (t-F8) (t-S8) nil nil (t-F8) (t-S8) nil (t-F8) (t-S8) nil (t-F8) (t-S8) nil)
:lines-list (list 0 1 0 1 2 1 2 3 4 3 4 5 4 5 6 5 6)
:approx-factor 1200/17))


(defvar *17-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S4) (t-S8) nil (t-S4) (t-S8) nil nil (t-S4) (t-S8) nil (t-S4) (t-S8) nil (t-S4) (t-S8) nil)
:lines-list (list 0 0 0 1 1 1 2 3 3 3 4 4 4 5 5 5 6)
:approx-factor 1200/17))


(defvar *17-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F8) (t-F4) nil (t-F8) (t-F4) nil nil (t-F8) (t-F4) nil (t-F8) (t-F4) nil (t-F8) (t-F4) nil)
:lines-list (list 0 1 1 1 2 2 2 3 4 4 4 5 5 5 6 6 6)
:approx-factor 1200/17))


;=====18 EDO=====

(defvar *18-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S7) (t-S9) nil (t-S7) (t-S9) nil (t-F1) (t-S1) (t-S8) (t-F1) (t-S1) (t-S8) (t-F1) (t-S1) (t-S8) (t-F1) (t-S1))
:lines-list (list 0 0 0 1 1 1 2 3 3 3 4 4 4 5 5 5 6 6)
:approx-factor 1200/18))


(defvar *18-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F9) (t-F7) nil (t-F9) (t-F7) nil (t-F1) (t-S1) (t-F8) (t-F1) (t-S1) (t-F8) (t-F1) (t-S1) (t-F8) (t-F1) (t-S1))
:lines-list (list 0 1 1 1 2 2 2 3 3 4 4 4 5 5 5 6 6 6)
:approx-factor 1200/18))

;=====19 EDO=====


(defvar *19-EDO*
(make-instance 'scale
:alteration-list
(list nil (t-S8) (t-F8) nil (t-S8) (t-F8) nil (t-S8) nil (t-S8) (t-F8) nil (t-S8) (t-F8) nil (t-S8) (t-F8) nil (t-S8))
:lines-list (list 0 0 1 1 1 2 2 2 3 3 4 4 4 5 5 5 6 6 6)
:approx-factor 1200/19))

;=====22 EDO=====

(defvar *22-EDO*
(make-instance 'scale
:alteration-list
(list nil (t-F8) (t-S7) (t-S8) nil (t-F8) (t-S7) (t-S8) nil nil (t-F8) (t-S7) (t-S8) nil (t-F8) (t-S7) (t-S8) nil (t-F8) (t-S7) (t-S8) nil)
:lines-list (list 0 1 0 0 1 2 1 1 2 3 4 3 3 4 5 4 4 5 6 5 5 6)
:approx-factor 1200/22))


(defvar *22-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S7) (t-S8) nil (t-S1) (t-S7) (t-S8) nil nil (t-S1) (t-S7) (t-S8) nil (t-S1) (t-S7) (t-S8) nil (t-S1) (t-S7) (t-S8) nil)
:lines-list (list 0 0 0 0 1 1 1 1 2 3 3 3 3 4 4 4 4 5 5 5 5 6)
:approx-factor 1200/22))


(defvar *22-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F8) (t-F7) (t-F1) nil (t-F8) (t-F7) (t-F1) nil nil (t-F8) (t-F7) (t-F1) nil (t-F8) (t-F7) (t-F1) nil (t-F8) (t-F7) (t-F1) nil)
:lines-list (list 0 1 1 1 1 2 2 2 2 3 4 4 4 4 5 5 5 5 6 6 6 6)
:approx-factor 1200/22))

;=====24 EDO=====

(defvar *24-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F12) (t-F8) (t-F4) nil (t-F12) (t-F8) (t-F4) nil (t-F4) nil (t-F12) (t-F8) (t-F4) nil (t-F12) (t-F8) (t-F4) nil (t-F12) (t-F8) (t-F4) nil (t-F4))
:lines-list (list 0 1 1 1 1 2 2 2 2 3 3 4 4 4 4 5 5 5 5 6 6 6 6 7)
:approx-factor 1200/24))

;=====30 EDO=====

(defvar *30-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S2) (t-S7) (t-S9) (t-F2) nil (t-S2) (t-S7) (t-S9) (t-F2) nil (t-S2) (t-F1) (t-S1) (t-S6) (t-S8) (t-S10) (t-F1) (t-S1) (t-S6) (t-S8) (t-S10) (t-F1) (t-S1) (t-S6) (t-S8) (t-S10) (t-F1) (t-S1) (t-F2))
:lines-list (list 0 0 0 0 1 1 1 1 1 2 2 2 3 3 3 3 3 4 4 4 4 4 5 5 5 5 5 6 6 7)
:approx-factor 1200/30))


(defvar *30-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S2) (t-F9) (t-F7) (t-F2) nil (t-S2) (t-F9) (t-F7) (t-F2) nil (t-S2) (t-F1) (t-S1) (t-F10) (t-F8) (t-F6) (t-F1) (t-S1) (t-F10) (t-F8) (t-F6) (t-F1) (t-S1) (t-F10) (t-F8) (t-F6) (t-F1) (t-S1) (t-F2))
:lines-list (list 0 0 1 1 1 1 1 2 2 2 2 2 3 3 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6 7)
:approx-factor 1200/30))

;=====31 EDO=====

(defvar *31-EDO*
(make-instance 'scale
:alteration-list
(list nil (t-S4) (t-S8) (t-F8) (t-F4) nil (t-S4) (t-S8) (t-F8) (t-F4) nil (t-S4) (t-S8) nil (t-S4) (t-S8) (t-F8) (t-F4) nil (t-S4) (t-S8) (t-F8) (t-F4) nil (t-S4) (t-S8) (t-F8) (t-F4) nil (t-S4) (t-S8))
:lines-list (list 0 0 0 1 1 1 1 1 2 2 2 2 2 3 3 3 4 4 4 4 4 5 5 5 5 5 6 6 6 6 6)
:approx-factor 1200/31))

;=====36 EDO=====

(defvar *36-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S7) (t-S8) (t-S9) (t-F1) nil (t-S1) (t-S7) (t-S8) (t-S9) (t-F1) nil (t-S1) (t-F1) nil (t-S1) (t-S7) (t-S8) (t-S9) (t-F1) nil (t-S1) (t-S7) (t-S8) (t-S9) (t-F1) nil (t-S1) (t-S7) (t-S8) (t-S9) (t-F1) nil (t-S1) (t-F1))
:lines-list (list 0 0 0 0 0 1 1 1 1 1 1 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 5 5 5 5 5 5 6 6 6 7)
:approx-factor 1200/36))


(defvar *36-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-F9) (t-F8) (t-F7) (t-F1) nil (t-S1) (t-F9) (t-F8) (t-F7) (t-F1) nil (t-S1) (t-F1) nil (t-S1) (t-F9) (t-F8) (t-F7) (t-F1) nil (t-S1) (t-F9) (t-F8) (t-F7) (t-F1) nil (t-S1) (t-F9) (t-F8) (t-F7) (t-F1) nil (t-S1) (t-F1))
:lines-list (list 0 0 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 4 4 4 4 4 4 5 5 5 5 5 5 6 6 6 6 6 6 7)
:approx-factor 1200/36))

;=====42 EDO=====

(defvar *42-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) nil nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) nil)
:lines-list (list 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 6)
:approx-factor 1200/42))


(defvar *42-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil nil (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil)
:lines-list (list 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6)
:approx-factor 1200/42))


;=====48 EDO=====

(defvar *48-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S4) (t-S7) (t-S8) (t-S9) (t-S12) (t-F1) nil (t-S1) (t-S4) (t-S7) (t-S8) (t-S9) (t-S12) (t-F1) nil (t-S1) (t-S4) (t-F1) nil (t-S1) (t-S4) (t-S7) (t-S8) (t-S9) (t-S12) (t-F1) nil (t-S1) (t-S4) (t-S7) (t-S8) (t-S9) (t-S12) (t-F1) nil (t-S1) (t-S4) (t-S7) (t-S8) (t-S9) (t-S12) (t-F1) nil (t-S1) (t-S4) (t-F1))
:lines-list (list 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 2 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 6 6 6 6 7)
:approx-factor 1200/48))


(defvar *48-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-F12) (t-F9) (t-F8) (t-F7) (t-F4) (t-F1) nil (t-S1) (t-F12) (t-F9) (t-F8) (t-F7) (t-F4) (t-F1) nil (t-S1) (t-F4) (t-F1) nil (t-S1) (t-F12) (t-F9) (t-F8) (t-F7) (t-F4) (t-F1) nil (t-S1) (t-F12) (t-F9) (t-F8) (t-F7) (t-F4) (t-F1) nil (t-S1) (t-F12) (t-F9) (t-F8) (t-F7) (t-F4) (t-F1) nil (t-S1) (t-F4) (t-F1))
:lines-list (list 0 0 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 7 7)
:approx-factor 1200/48))

;=====60 EDO=====

(defvar *60-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F2) (t-F1))
:lines-list (list 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 7 7)
:approx-factor 1200/60))


(defvar *60-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F2) (t-F1))
:lines-list (list 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 7 7)
:approx-factor 1200/60))

;=====72 EDO=====


(defvar *72-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-S4) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S12) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S4) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S12) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S4) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S4) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S12) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S4) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S12) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S4) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S12) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S4) (t-F2) (t-F1))
:lines-list (list 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 7 7)
:approx-factor 1200/72))


(defvar *72-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-F12) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F4) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F12) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F4) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F4) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F12) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F4) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F12) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F4) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F12) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F4) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-F4) (t-F2) (t-F1))
:lines-list (list 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7)
:approx-factor 1200/72))

;=====84 EDO=====

(defvar *84-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F3) (t-F2) (t-F1))
:lines-list (list 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 7 7 7)
:approx-factor 1200/84))


(defvar *84-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-S3) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F3) (t-F2) (t-F1))
:lines-list (list 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7)
:approx-factor 1200/84))

;=====96 EDO=====

(defvar *96-EDO_#*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-S3) (t-S4) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-S12) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S4) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-S12) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S4) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S4) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-S12) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S4) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-S12) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S4) (t-S5) (t-S6) (t-S7) (t-S8) (t-S9) (t-S10) (t-S11) (t-S12) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-S4) (t-F3) (t-F2) (t-F1))
:lines-list (list 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 7 7 7)
:approx-factor 1200/96))


(defvar *96-EDO_b*
(make-instance 'scale
:alteration-list
(list nil (t-S1) (t-S2) (t-S3) (t-F12) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F4) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F12) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F4) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F4) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F12) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F4) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F12) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F4) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F12) (t-F11) (t-F10) (t-F9) (t-F8) (t-F7) (t-F6) (t-F5) (t-F4) (t-F3) (t-F2) (t-F1) nil (t-S1) (t-S2) (t-S3) (t-F4) (t-F3) (t-F2) (t-F1))
:lines-list (list 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7)
:approx-factor 1200/96))



;----------------

(defvar *current-1/2-scale*  *2-tone-chromatic-scale*)
(defvar *current-1/4-scale* *4-tone-chromatic-scale*)
(defvar *current-1/8-scale* *8-tone-chromatic-scale*)

(setf *current-1/2-scale*  *2-tone-chromatic-scale*)
(setf *current-1/4-scale* *4-tone-chromatic-scale*)
(setf *current-1/8-scale* *8-tone-chromatic-scale*)

#|
(setf *current-1/4-scale*
   (make-instance 'scale
     :alteration-list
     (list  nil (t-1/4) (diese) (inv-bemol) nil (t-1/4) (diese) (inv-bemol) nil (t-1/4) nil (t-1/4) (diese)
            (inv-bemol) nil (t-1/4) (diese) (inv-bemol) nil (t-1/4) (diese) (inv-bemol) nil (t-1/4))
     :lines-list
     (list  0 0 0 1 1 1 1 2 2 2 3 3 3 4 4 4 4 5 5 5 5 6 6 6)
     :approx-factor 50))
|#


;for EDO scales

(defparameter *scales-list* (list
                             (list 50.0 *5-EDO_#* "5 EDO_#")
                             (list 50.1 *5-EDO_b* "5 EDO_b")
                             (list 1 *1-tone-chromatic-scale* "6 EDO_#")
                             (list 60.1 *6-EDO_b* "6 EDO_b")
                             (list 1.0 *1#-tone-chromatic-scale* "6 EDO_om(t)")
                             (list 70 *7-EDO* "7 EDO")
                             (list 80.0 *8-EDO_#* "8 EDO_#")
                             (list 80.1 *8-EDO_b* "8 EDO_b")
                             (list 90.0 *9-EDO_#* "9 EDO_#")
                             (list 90.1 *9-EDO_b* "9 EDO_b")
                             (list 100.0 *10-EDO_#* "10 EDO_#")
                             (list 100.1 *10-EDO_b* "10 EDO_b")
                             (list 2 *current-1/2-scale* "12 EDO_#")
                             (list 120.1 *12-EDO_b* "12 EDO_b")
                             (list 140.0 *14-EDO_^* "14 EDO_^")
                             (list 140.1 *14-EDO_v* "14 EDO_v")
                             (list 150.0 *15-EDO_#* "15 EDO_#")
                             (list 150.1 *15-EDO_b* "15 EDO_b")
                             (list 160.0 *16-EDO_#* "16 EDO_#")
                             (list 160.1 *16-EDO_b* "16 EDO_b")
                             (list 170 *17-EDO* "17 EDO")
                             (list 170.0 *17-EDO_#* "17 EDO_#")
                             (list 170.1 *17-EDO_b* "17 EDO_b")
                             (list 180.0 *18-EDO_#* "18 EDO_#")
                             (list 180.1 *18-EDO_b* "18 EDO_b")
                             (list 3 *3-tone-chromatic-scale* "18 EDO_om")
                             (list 3.0 *3#-tone-chromatic-scale* "18 EDO_om(t)")
                             (list 190 *19-EDO* "19 EDO")
                             (list 220 *22-EDO* "22 EDO")
                             (list 220.0 *22-EDO_#* "22 EDO_#")
                             (list 220.1 *22-EDO_b* "22 EDO_b")
                             (list 4 *current-1/4-scale* "24 EDO_#")
                             (list 240.1 *24-EDO_b* "24 EDO_b")
                             (list 300.0 *30-EDO_#* "30 EDO_#")
                             (list 300.1 *30-EDO_b* "30 EDO_b")
                             (list 5 *5-tone-chromatic-scale* "30 EDO_om")
                             (list 5.0 *5#-tone-chromatic-scale* "30 EDO_om(t)")
                             (list 310 *31-EDO* "31 EDO")
                             (list 360.0 *36-EDO_#* "36 EDO_#")
                             (list 360.1 *36-EDO_b* "36 EDO_b")
                             (list 6 *6-tone-chromatic-scale* "36 EDO_om")
                             (list 420.0 *42-EDO_#* "42 EDO_#")
                             (list 420.1 *42-EDO_b* "42 EDO_b")
                             (list 7 *7-tone-chromatic-scale* "42 EDO_om")
                             (list 7.0 *7#-tone-chromatic-scale* "42 EDO_om(t)")
                             (list 480.0 *48-EDO_#* "48 EDO_#")
                             (list 480.1 *48-EDO_b* "48 EDO_b")
                             (list 8 *current-1/8-scale* "48 EDO_om")
                             (list 600.0 *60-EDO_#* "60 EDO_#")
                             (list 600.1 *60-EDO_b* "60 EDO_b")
                             (list 10 *10-tone-chromatic-scale* "60 EDO_om")
                             (list 720.0 *72-EDO_#* "72 EDO_#")
                             (list 720.1 *72-EDO_b* "72 EDO_b")
                             (list 12 *12-tone-chromatic-scale* "72 EDO_om")
                             (list 840.0 *84-EDO_#* "84 EDO_#")
                             (list 840.1 *84-EDO_b* "84 EDO_b")
                             (list 14 *14-tone-chromatic-scale* "84 EDO_om")
                             (list 960.0 *96-EDO_#* "96 EDO_#")
                             (list 960.1 *96-EDO_b* "96 EDO_b")
                             (list 16 *16-tone-chromatic-scale* "96 EDO_om")
                             ))

(defun get-current-scale (tone)
    (or (cadr (find tone *scales-list* :test 'equal :key 'car))
        *current-1/2-scale*))


(defun default-scale-p (scale)
    (or (equal *current-1/2-scale* scale)
        (equal *current-1/4-scale* scale)
        (equal *current-1/8-scale* scale)))

(defun approx2tone (app)
  (case app (25 8) (50 4) (100 2) (otherwise (* 2 (/ 100 app)))))


(defmethod approx-scale ((self scale) midic &optional (ref-midic 0))
  (approx-m midic (approx2tone (approx-factor self)) ref-midic))

(defun get-new-scale (tone)
    (omng-copy (get-current-scale tone)))

(defmethod get-super-default-value ((self (eql 'scale)))
    (omng-copy (get-current-scale 2)))

(defmethod  omng-copy ((self scale))
    (make-instance 'scale
      :alteration-list (copy-list (alteration-list self))
      :lines-list   (copy-list (lines-list self))
      :approx-factor   (approx-factor self)))


(defmethod scale-midi2pixel ((scale scale) midi top linespace)
  (let* ((app (approx-factor scale))
         (topmc (* top 100))
         (tone (approx2tone app))
         (midiapp (approx-scale scale midi))
         (numtones (* 6 tone)))
    (* (+ (* 3.5 (floor (- topmc midiapp) 1200)) 
                 (lines-from-do (/ (mod (- (* top 100) midiapp) 1200) app) scale numtones) -0.5 ) linespace)))





;;;===============================================

(defclass scaleeditor (chordeditor) 
              ((ref-chord :initarg :ref-chord :accessor ref-chord 
                      :initform (make-instance 'chord))))

(defmethod playosc? ((self scaleeditor)) nil)
(defmethod slotsedit? ((self scaleeditor)) nil)
(defmethod special-tone ((self scaleeditor)) (approx2tone (approx-factor (object self))))
(defmethod get-inspector ((self scaleeditor)) nil)

(defmethod Class-has-editor-p  ((self scale)) t)
(defmethod get-editor-class ((self scale)) 'scaleeditor)
(defmethod get-help-list ((self scaleeditor)) nil)

(defclass scalepanel (chordpanel) ())
(defclass omscale-controls-view (omcontrols-view) ())


(defmethod get-score-class-panel ((self scaleeditor)) 'scalePanel)

(defmethod get-score-class-ctrls ((self scaleeditor)) 'omscale-controls-view)

(defmethod get-win-ed-size  ((self scale)) (om-make-point 700 200))
(defmethod get-editor-field-size ((self scaleeditor)) (om-make-point 10000 10))

(defmethod default-edition-params ((self scale))
  (pairlis '(approx fontsize staff cmnpref deltapict outport inport 
             zoom notechancolor? grillestep mode winsize winpos score-mode obj-mode cursor-mode show-stems scale) 
           (list *global-midi-approx* *music-fontsize* *default-satff* (make-instance 'edition-values) (om-make-point 0 0) 
                 nil nil
                 1 nil 1000 0 (om-make-point 720 230) (om-make-point 100 100) 0 1 :normal t nil)))

(defmethod objectfromeditor ((self scalepanel)) 
   (ref-chord (editor self)))

(defmethod get-approx-scale ((self scalepanel))  
  (object (editor self)))

(defmethod editor-default-edit-params ((self scaleeditor))
  (default-edition-params (ref-chord self)))


(defmethod init-boxes-in-score ((self scalepanel)) nil)

(defmethod om-drag-start ((self scalepanel)) nil)

; (get-edit-param (om-make-view 'scaleeditor) 'fontsize)

(defmethod initialize-instance :after ((self scaleeditor) &rest l)
  (declare (ignore l))
  (setf (obj-mode (panel self)) 'note)
  (change-editor-tone (panel self) (approx2tone (approx-factor (object self))))
  (change-editor-mode (panel self) 3)
  (update-panel (panel self))
  )

(defmethod change-editor-tone ((self scalepanel) tone)
  (let ((ref (get-new-scale tone)))
    (unless (= (approx-factor (object (om-view-container self))) (approx-factor ref))
      (setf (approx-factor (object (om-view-container self))) (approx-factor ref)
            (alteration-list (object (om-view-container self))) (alteration-list ref)
            (lines-list (object (om-view-container self))) (lines-list ref)))
    (update-chord (om-view-container self))
    (call-next-method)))


(defmethod update-editor-after-eval ((self scaleeditor) val) 
  (om-with-compiler-warnings nil
    (setf (object self) val)
    (change-editor-tone (panel self) (approx2tone (approx-factor (object self))))
    ;(update-panel (panel self))
    ))

(defmethod update-chord ((self scaleeditor))
  (setf (ref-chord self) (make-instance 'chord :lmidic (arithm-ser 6000 (- 7200 (approx-factor (object self)))  
                                                                   (approx-factor (object self))))))

(defmethod report-modifications ((self scaleeditor)) ;(om-inspect self)
  (let ((refscale (object self))) ;(get-current-scale (get-edit-param self 'approx))))
    (setf (approx-factor (object self)) (/ 200 (get-edit-param self 'approx)))
    (setf (alteration-list (object self)) (copy-list (alteration-list refscale)))
    (setf (lines-list (object self)) (copy-list (lines-list refscale)))
    (print (lines-list (object self)))
    (loop for note in (inside (ref-chord self))
          for i = 0 then (+ i 1) do
            (when (tonalite note)
              (setf (nth i (lines-list (object self))) (+ (nth i (lines-list refscale)) 
                                                          (line-diff (nth i (alteration-list refscale))
                                                                     (alt-2-str (tonalt (tonalite note))))))
              (setf (nth i (alteration-list (object self))) (string (alt-2-str (tonalt (tonalite note))))))
            )
    (when (and (ref self) (subtypep (type-of (ref self)) 'scoreeditor))
      (update-panel (panel (ref self))))
    (when (not (subtypep (type-of (ref self)) 'scoreeditor))
      (setf *default-editor-scale* (list (alteration-list (object self)) (lines-list (object self)) (approx-factor (object self))))
      (save-preferences))
    ))


(defun line-diff (ref-alt new-alt)
  (cond ((string-equal (string ref-alt) (string new-alt)) 0)
        ((equal ref-alt nil) 
         (cond
          ((string-equal (string new-alt) (string (diese))) -1)
          ((string-equal (string new-alt) (string (db-diese))) -1)
          ((string-equal (string new-alt) (string (bemol))) 1)
          ((string-equal (string new-alt) (string (db-bemol))) 1)
          (t 0)))
        ((string-equal ref-alt (string (diese)))
         (cond
          ((string-equal (string new-alt) (string (db-diese))) -1)
          ((string-equal (string new-alt) (string (bemol))) 1)
          ((string-equal (string new-alt) (string (db-bemol))) 2)
          (t 0)))
        ((string-equal ref-alt (string (bemol)))
         (cond
          ((string-equal (string new-alt) (string (diese))) -1)
          ((string-equal (string new-alt) (string (db-diese))) -2)
          ((string-equal (string new-alt) (string (db-bemol))) 1)
          (t 0)))
        (t 0)))
        
#|        
(defmethod handle-key-event ((self scalepanel) key) 
  (case key
    (#\s (progn 
           (print (list "Saved!"))
           (save-preferences)
           (om-close-window *pref-window*)
           (om-window-close-event  (om-view-container self))
           ))
           ))
|#
(defmethod handle-key-event ((self scalepanel) key) nil)

(defmethod om-get-menu-context ((self scaleeditor)) nil)

(defmethod om-get-menu-context ((self scalepanel)) 
  (let ((selection (get-click-in-obj self (graphic-obj self) 
                                     (grap-class-from-type (obj-mode self)) 
                                     (om-mouse-position self))))
    (when selection 
      (if (= (staff-tone self) 2)
          (om-get-menu-context selection)
        (om-beep-msg "Microtonal scales can not be modified!")))))

(defmethod om-score-click-handler ((self scalepanel) where double-click-p)
  (let* ((mode-obj (grap-class-from-type  (obj-mode self)))
         (graph-obj (get-click-in-obj self (graphic-obj self) mode-obj where)))
    (when graph-obj 
      (om-popup-menu-context self))
     (report-modifications (om-view-container self))
     (update-panel self)
     ))



;;;===================================================
;;; NEW scales

(defclass atone-scale (scale)
  ((midic-base :initform 0 :initarg :midic-base :accessor midic-base)))


(defmethod approx-scale ((self atone-scale) midic &optional (ref-midic 0))
  (a-tone-approx-m midic (approx-factor self) (midic-base self)))

(defmethod* a-tone-approx-m  ((midic t) approx-factor base)
           (let* ((distance (-  midic base))
                  (steps (floor distance approx-factor))
                  (interval (- midic (+ base (* steps approx-factor)))))
             (round (if (< interval (/ approx-factor 2))
                 (+ base (* steps approx-factor))
               (+ base (* (+ 1 steps) approx-factor))))))

(defmethod* a-tone-approx-m  ((midic list) approx-factor base)
           (loop for item in midic collect (a-tone-approx-m item approx-factor base)))


(defmethod give-alteration  ((self atone-scale) midic)
  (let* ((length (length (lines-list self)))
         (distance (-  midic (midic-base self)))
         (steps (round distance (approx-factor self))) index)
    (setf steps (mod steps (length (lines-list self))))
    (setf index (if (minusp steps) (- length steps) steps))
    (list (nth index  (lines-list self)) (nth  index (alteration-list self)) 0)))

(defmethod scale-midi2pixel ((scale atone-scale) midi top linespace)
 (let* ((topmc (* top 100))
        (midiapp (approx-scale scale midi))
        (octaves (floor (+  (- topmc midiapp) (- (midic-base scale) 6000 (+ (approx-factor scale) -1))) 1200))  ;;; why -1 ?
        (line (car (give-alteration scale midiapp))))
   (* (- (* 3.5 (+ 2 octaves)) (+ 4 (/ line 2 ))) linespace)))



;;;==================
;;; 31 SCALE (HF)

;(give-alteration *31-equal-scale* 6010)

(defparameter *31-equal-scale*
    (make-instance 'atone-scale
      :alteration-list
      (list  (inv-bemol) nil (t-1/4) (t-1/2) 
             (bemol) (inv-bemol) nil (t-1/4) (t-1/2)
             (bemol) (inv-bemol) nil (t-1/4) 
             (inv-bemol) nil (t-1/4) (t-1/2)
             (bemol) (inv-bemol) nil (t-1/4) (t-1/2)
             (bemol) (inv-bemol) nil (t-1/4) (t-1/2)
             (bemol) (inv-bemol) nil (t-1/4))
      :lines-list
      (list  0 0 0 0
             1 1 1 1 1
             2 2 2 2 
             3 3 3 3 
             4 4 4 4 4
             5 5 5 5 5
             6 6 6 6)
      :approx-factor (/ 1200 31)
      :midic-base 5971))

;(pushr (list 38.7 *31-equal-scale* "H.F. 1/31") *scales-list*)

(defparameter *19-equal-scale*
  (make-instance 'atone-scale
                 :alteration-list
                 (list  (bemol) nil (diese)
                        (bemol) nil (diese)
                        (bemol) nil
                        (bemol) nil (diese)
                        (bemol) nil (diese)
                        (bemol) nil (diese)
                        (bemol) nil)
      :lines-list
      (list  0 0 0 
             1 1 1 
             2 2  
             3 3 3  
             4 4 4 
             5 5 5 
             6 6)
      :approx-factor (/ 1200 19)
      :midic-base 5952))


;(pushr (list 63 *19-equal-scale* "19 ET") *scales-list*)

    

;(defparameter *5-equal-scale*
;  (make-instance 'atone-scale
;                 :alteration-list (list (bemol) nil (t-1/4) (diese) (t-3/4))
;                 :lines-list '(0 0 0 0 0)   
;                :approx-factor (/ 1200 5)
;                :midic-base 6000))

;(pushr (list 240 *5-equal-scale* "5 scale") *scales-list*)           


