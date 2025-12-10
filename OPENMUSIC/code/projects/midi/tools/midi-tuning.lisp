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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; MIDI package

(in-package :om)

;;; split note on channels in case of microtonal setup (4 or 8)
;;; tone = 0, 1/8 = 1, 1/4 = 2, 3/8 = 3
;;; default bend channel 1 = 0, channel 2 = 25 mc, channel 3 = 50 mc, channel 4 = 75mc
;(defun micro-channel (midic &optional approx)
;  (let ((channel-mc-unit (/ 200 (or approx 8))))
;    (round (mod midic 100) channel-mc-unit)))

;; t / nil / list of approx where it must be applied
(defparameter *micro-channel-mode-on* '(4 8))
(defparameter *micro-channel-approx* 8)

(defun micro-channel-on (approx)
  (and 
   approx
   (if (consp *micro-channel-mode-on*) 
       (find approx *micro-channel-mode-on* :test '=)
     *micro-channel-mode-on*)))




;;;mettre le let modulo au debut !
(defun micro-channel (midic &optional approx)
  (let ((modulo (mod midic 100)))
    (cond
;5 EDO     
     ((or (= 50.1 approx) (= 50.0 approx))
      (cond ((= modulo 40) 1)
            ((= modulo 80) 2)
            ((= modulo 20) 3)
            ((= modulo 60) 4)
            (t 0)))

;7 EDO
     ((or (= 70.1 approx) (= 70.0 approx))
      (cond ((= modulo 71) 1)
            ((= modulo 43) 2)
            ((= modulo 14) 3)
            ((= modulo 86) 4)
            ((= modulo 57) 5)
            ((= modulo 29) 6)
            (t 0)))

;8 EDO   
     ((or (= 80.1 approx) (= 80.0 approx))
      (if (= 50 modulo) 1 0))

;9 EDO 
     ((or (= 90.1 approx) (= 90.0 approx))
      (cond ((= modulo 33) 1)
              ((= modulo 67) 2)
              (t 0)))

;10 EDO      
     ((or (= 100.1 approx) (= 100.0 approx))
      (cond ((= modulo 20) 1)
              ((= modulo 40) 2)
              ((= modulo 60) 3)
              ((= modulo 80) 4)
              (t 0)))

;14 EDO    
     ((or (= 140.1 approx) (= 140.0 approx))
      (let ((modulo (mod midic 100)))
        (cond ((= modulo 86) 1)
              ((= modulo 71) 2)
              ((= modulo 57) 3)
              ((= modulo 43) 4)
              ((= modulo 29) 5)
              ((= modulo 14) 6)
              (t 0))))

;15 EDO
     ((or (= 150.1 approx) (= 150.0 approx))
      (cond ((= modulo 80) 1)
              ((= modulo 60) 2)
              ((= modulo 40) 3)
              ((= modulo 20) 4)
              (t 0)))
;16 EDO
     ((or (= 160.1 approx) (= 160.0 approx))
      (cond ((= modulo 75) 1)
              ((= modulo 50) 2)
              ((= modulo 25) 3)
              (t 0)))

;17 EDO       
     ((or (= 170 approx) (= 170.0 approx) (= 170.1 approx))
      (if (or (= (mod midic 100) 71)
                (= (mod midic 100) 82)
                (= (mod midic 100) 94)
                (= (mod midic 100) 76)
                (= (mod midic 100) 88)) 
            1 0)); a voir si on fait comme en 1/4 ou comme Steven...

;18 EDO
     ((or (= 3 approx) (= 3.0 approx) (= 180.1 approx) (= 180.0 approx))
      (cond ((= modulo 67) 1)
              ((= modulo 33) 2)
              (t 0)))

;19 EDO 
     ((= 190 approx)
      (if (or (= modulo 63)
              (= modulo 89)
              (= modulo 79)
              (= modulo 68)
              (= modulo 95)
              (= modulo 84)
              (= modulo 74))
          1 0))

;22 EDO
     ((or (= 220 approx) (= 220.1 approx) (= 200.0 approx))
      (if (or (= modulo 55)
                (= modulo 64)
                (= modulo 73)
                (= modulo 82)
                (= modulo 91))
            1 0))

;24 EDO
     ((or (= 4 approx) (= 240.1 approx))
      (if (= 50 modulo) 2 0))

;30 EDO
     ((or (= 5 approx) (= 5.0 approx)(= 300.1 approx) (= 300.0 approx))
      (cond ((= modulo 40) 1)
              ((= modulo 80) 2)
              ((= modulo 20) 3)
              ((= modulo 60) 4)
              (t 0)))

;31 EDO
     ((= 310 approx) 
      (cond ((or(= modulo 39)
                  (= modulo 55)
                  (= modulo 71)
                  (= modulo 48)
                  (= modulo 65)
                  (= modulo 42)
                  (= modulo 58)
                  (= modulo 74)
                  (= modulo 52)
                  (= modulo 68)
                  (= modulo 45)
                  (= modulo 61))
               1)
              ((or (= modulo 77)
                   (= modulo 94)
                   (= modulo 87)
                   (= modulo 81)
                   (= modulo 97)
                   (= modulo 90)
                   (= modulo 84))
               2)
              (t 0)))

;36 EDO
     ((or (= 6 approx) (= 360.1 approx) (= 360.0 approx))
      (cond ((= modulo 33) 1)
              ((= modulo 67) 2)
              (t 0)))
;41 EDO
     ((or (= 410.0 approx) (= 410.1 approx) (= 410.2 approx) (= 410.3 approx) (= 410.4 approx))
      (cond ((or(= modulo 29)
                (= modulo 46)
                (= modulo 34)
                (= modulo 51)
                (= modulo 39)
                (= modulo 56)
                (= modulo 44)
                (= modulo 32)
                (= modulo 49)
                (= modulo 37)
                (= modulo 54)
                (= modulo 41))
             1)
            ((or (= modulo 59)
                 (= modulo 76)
                 (= modulo 63)
                 (= modulo 80)
                 (= modulo 68)
                 (= modulo 85)
                 (= modulo 73)
                 (= modulo 61)
                 (= modulo 78)
                 (= modulo 66)
                 (= modulo 83)
                 (= modulo 71))
             2)
            ((or (= modulo 88)
                 (= modulo 93)
                 (= modulo 98)
                 (= modulo 90)
                 (= modulo 95))
             3)
            (t 0)))

;42 EDO
     ((or (= 7 approx) (= 7.0 approx) (= 420.1 approx) (= 420.0 approx))
      (cond ((= modulo 29) 1)
              ((= modulo 57) 2)
              ((= modulo 86) 3)
              ((= modulo 14) 4)
              ((= modulo 43) 5)
              ((= modulo 71) 6)
              (t 0)))

;48 EDO
     ((or (= 8 approx) (= 480.0 approx) (= 480.1 approx)) 
     ; (let ((modulo (mod (approx-m midic 8) 100))); midic is already approxed?
        (cond ((= 25 modulo) 1) 
              ((= 50 modulo) 2) 
              ((= 75 modulo) 3) 
              (t 0)));)

;60 EDO
     ((or (= 10 approx) (= 600.0 approx) (= 600.1 approx))
      ;(let ((modulo (mod (approx-m midic 10) 100)))
        (cond ((= 20 modulo) 1) 
              ((= 40 modulo) 2) 
              ((= 60 modulo) 3)
              ((= 80 modulo) 4)(t 0)));)

;72 EDO
     ((or (= 720.1 approx) (= 720.0 approx))
      (cond ((= modulo 17) 1)
              ((= modulo 33) 2)
              ((= modulo 50) 3)
              ((= modulo 67) 4)
              ((= modulo 83) 5)
              (t 0)))

;84 EDO
     ((or (= 840.1 approx) (= 840.0 approx))
      (cond ((= modulo 14) 1)
              ((= modulo 29) 2)
              ((= modulo 43) 3)
              ((= modulo 57) 4)
              ((= modulo 71) 5)
              ((= modulo 86) 6)
              (t 0)))


;96 EDO
     ((or (= 16 approx) (=  960.0 approx) (= 960.1 approx)) 
      (cond ((= 12 modulo) 1) 
              ((= 25 modulo) 2) 
              ((= 38 modulo) 3) 
              ((= 50 modulo) 4) 
              ((= 62 modulo) 5) 
              ((= 75 modulo) 6) 
              ((= 88 modulo) 7) 
              (t 0)))

     (t (let ((mod (if (= approx 4) 2 (/ 200 (or *micro-channel-approx* approx)))))
          (round (approx-m (mod midic 100) approx) mod)
          )))))
