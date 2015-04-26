;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou,Paris, France.
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)
;===================================================
;popup + dialog used to change staff size port, etc.
;================================================
(defvar *all-satff-om*   '(f g g2 f2 gf gg ff gff ggf ggff empty))

(defvar *chord-satff-om* '(("F" f) ("G" g) ("GF" gf) ("GG" gg) ("FF" ff) ("GFF" gff) ("GGF" ggf) ("GGFF" ggff) ("EMPTY" empty)))
(defvar *voice-satff-om* '(("F" f) ("G" g) ("F2" f2) ("G2" g2) ("EMPTY" empty)))
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
                       ("1/7" 200/7) ("1/8" 25) ("1/10" 20)  ("1/12" 100/6) ("1/14" 100/7) ("1/16" 25/2)))))))

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



(defvar *current-1/2-scale*  *2-tone-chromatic-scale*)
(defvar *current-1/4-scale* *4-tone-chromatic-scale*)
(defvar *current-1/8-scale* *8-tone-chromatic-scale*)

(setf *current-1/2-scale*  *2-tone-chromatic-scale*)
(setf *current-1/4-scale* *4-tone-chromatic-scale*)
(setf *current-1/8-scale* *8-tone-chromatic-scale*)


(defparameter *scales-list* (list 
                             (list 1   *1-tone-chromatic-scale*  "1")
                             (list 1.0 *1#-tone-chromatic-scale* "1#")
                             (list 2   *current-1/2-scale* "1/2")
                             (list 3   *3-tone-chromatic-scale* "1/3")
                             (list 3.0 *3#-tone-chromatic-scale* "1/3#")
                             (list 4   *current-1/4-scale* "1/4")
                             (list 5   *5-tone-chromatic-scale* "1/5")
                             (list 5.0 *5#-tone-chromatic-scale* "1/5#")
                             (list 6   *6-tone-chromatic-scale*  "1/6")
                             (list 7   *7-tone-chromatic-scale*  "1/7")
                             (list 7.0 *7#-tone-chromatic-scale* "1/7#")
                             (list 8   *current-1/8-scale* "1/8")
                             (list 10   *10-tone-chromatic-scale* "1/10")
                             (list 12   *12-tone-chromatic-scale*  "1/12")
                             (list 14   *14-tone-chromatic-scale*  "1/14")
                             (list 16   *16-tone-chromatic-scale* "1/16")
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
  ;(update-panel (panel self))
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
    (update-panel (panel self))))

(defmethod update-chord ((self scaleeditor))
  (setf (ref-chord self) (make-instance 'chord :lmidic (arithm-ser 6000 (- 7200 (approx-factor (object self)))  
                                                                   (approx-factor (object self))))))

(defmethod report-modifications ((self scaleeditor))
  (let ((refscale (get-current-scale (get-edit-param self 'approx))))
    (setf (approx-factor (object self)) (/ 200 (get-edit-param self 'approx)))
    (setf (alteration-list (object self)) (copy-list (alteration-list refscale)))
    (setf (lines-list (object self)) (copy-list (lines-list (get-current-scale refscale))))
  (loop for note in (inside (ref-chord self))
        for i = 0 then (+ i 1) do
        (when (tonalite note)
          (setf (nth i (lines-list (object self))) (+ (nth i (lines-list refscale)) 
                                                           (line-diff (nth i (alteration-list refscale))
                                                                 (alt-2-str (tonalt (tonalite note))))))
          (setf (nth i (alteration-list (object self))) (string (alt-2-str (tonalt (tonalite note))))))
        )
  (when (and (ref self) (subtypep (type-of (ref self)) 'scoreeditor))
    (update-panel (panel (ref self))))))


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
     (report-modifications (om-view-container self))))



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
