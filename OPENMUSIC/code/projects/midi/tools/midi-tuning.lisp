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


(defun micro-channel (midic &optional approx)
  (cond

;5 EDO     
   ((or (= 50.1 approx) (= 50.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 40) 1)
            ((= modulo 80) 2)
            ((= modulo 20) 3)
            ((= modulo 60) 4)
            (t 0))))

;7 EDO
   ((or (= 70.1 approx) (= 70.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 71) 1)
            ((= modulo 43) 2)
            ((= modulo 14) 3)
            ((= modulo 86) 4)
            ((= modulo 57) 5)
            ((= modulo 29) 6)
            (t 0))))

;8 EDO   
   ((or (= 80.1 approx) (= 80.0 approx))
    (let ((modulo (mod midic 100)))
      (if (= 50 modulo) 1 0)))

;9 EDO 
   ((or (= 90.1 approx) (= 90.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 33) 1)
            ((= modulo 67) 2)
            (t 0))))

;10 EDO      
   ((or (= 100.1 approx) (= 100.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 20) 1)
            ((= modulo 40) 2)
            ((= modulo 60) 3)
            ((= modulo 80) 4)
            (t 0))))

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
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 80) 1)
            ((= modulo 60) 2)
            ((= modulo 40) 3)
            ((= modulo 20) 4)
            (t 0))))
;16 EDO
   ((or (= 160.1 approx) (= 160.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 75) 1)
            ((= modulo 50) 2)
            ((= modulo 25) 3)
            (t 0))))

;17 EDO       
   ((or (= 170 approx) (= 170.0 approx) (= 170.1 approx))
    (cond 
     ((= (mod midic 100) 71) 1)
     ((= (mod midic 100) 41) 2)
     ((= (mod midic 100) 12) 3)
     ((= (mod midic 100) 82) 4)
     ((= (mod midic 100) 53) 5)
     ((= (mod midic 100) 24) 6)
     ((= (mod midic 100) 94) 7)
     ((= (mod midic 100) 65) 8)
     ((= (mod midic 100) 35) 10)
     ((= (mod midic 100) 6) 11)
     ((= (mod midic 100) 76) 12)
     ((= (mod midic 100) 47) 13)
     ((= (mod midic 100) 18) 14)
     ((= (mod midic 100) 88) 15)
     ((= (mod midic 100) 59) 16)
     ((= (mod midic 100) 29) 17)
     (t 0)))

;18 EDO
   ((or (= 3 approx) (= 3.0 approx) (= 180.1 approx) (= 180.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 67) 1)
            ((= modulo 33) 2)
            (t 0))))

;19 EDO 
   ((= 190 approx)
    (cond
     ((= (mod midic 100) 63) 1)
     ((= (mod midic 100) 26) 2)
     ((= (mod midic 100) 89) 3)
     ((= (mod midic 100) 53) 4)
     ((= (mod midic 100) 16) 5)
     ((= (mod midic 100) 79) 6)
     ((= (mod midic 100) 42) 7)
     ((= (mod midic 100) 5) 8)
     ((= (mod midic 100) 68) 10)
     ((= (mod midic 100) 32) 11)
     ((= (mod midic 100) 95) 12)
     ((= (mod midic 100) 58) 13)
     ((= (mod midic 100) 21) 14)
     ((= (mod midic 100) 84) 15)
     ((= (mod midic 100) 47) 16)
     ((= (mod midic 100) 11) 17)
     ((= (mod midic 100) 74) 18)
     ((= (mod midic 100) 37) 19)
     (t 0)))

;22 EDO
   ((or (= 220 approx) (= 220.1 approx) (= 200.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 55) 1)
            ((= modulo 9) 2)
            ((= modulo 64) 3)
            ((= modulo 18) 4)
            ((= modulo 73) 5)
            ((= modulo 27) 6)
            ((= modulo 82) 7)
            ((= modulo 36) 8)
            ((= modulo 91) 10)
            ((= modulo 45) 11)
            (t 0))))

;24 EDO
   ((or (= 4 approx) (= 240.1 approx))
    (let ((modulo (mod midic 100)))
      (if (= 50 modulo) 2 0)))

;30 EDO
   ((or (= 5 approx) (= 5.0 approx)(= 300.1 approx) (= 300.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 40) 1)
            ((= modulo 80) 2)
            ((= modulo 20) 3)
            ((= modulo 60) 4)
            (t 0))))

;31 EDO
   ((= 310 approx) 
    (cond 
     ((= (mod midic 100) 39) 1)
     ((= (mod midic 100) 77) 2)
     ((= (mod midic 100) 16) 3)
     ((= (mod midic 100) 55) 4)
     ((= (mod midic 100) 94) 5)
     ((= (mod midic 100) 32) 6)
     ((= (mod midic 100) 71) 7)
     ((= (mod midic 100) 10) 8)
     ((= (mod midic 100) 48) 10)
     ((= (mod midic 100) 87) 11)
     ((= (mod midic 100) 26) 12)
     ((= (mod midic 100) 65) 13)
     ((= (mod midic 100) 3) 14)
     ((= (mod midic 100) 42) 15)
     ((= (mod midic 100) 81) 16)
     ((= (mod midic 100) 19) 17)
     ((= (mod midic 100) 58) 18)
     ((= (mod midic 100) 97) 19)
     ((= (mod midic 100) 35) 20)
     ((= (mod midic 100) 74) 21)
     ((= (mod midic 100) 13) 22)
     ((= (mod midic 100) 52) 23)
     ((= (mod midic 100) 90) 24)
     ((= (mod midic 100) 29) 25)
     ((= (mod midic 100) 68) 26)
     ((= (mod midic 100) 6) 27)
     ((= (mod midic 100) 45) 28)
     ((= (mod midic 100) 84) 29)
     ((= (mod midic 100) 23) 30)
     ((= (mod midic 100) 61) 31)
     (t 0)))

;36 EDO
   ((or (= 6 approx) (= 360.1 approx) (= 360.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 33) 1)
            ((= modulo 67) 2)
            (t 0))))

;42 EDO
   ((or (= 7 approx) (= 7.0 approx) (= 420.1 approx) (= 420.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 29) 1)
            ((= modulo 57) 2)
            ((= modulo 86) 3)
            ((= modulo 14) 4)
            ((= modulo 43) 5)
            ((= modulo 71) 6)
            (t 0))))

;48 EDO
   ((or (= 8 approx) (= 480.0 approx) (= 480.1 approx)) 
    (let ((modulo (mod (approx-m midic 8) 100)))
      (cond ((= 25 modulo) 1) 
            ((= 50 modulo) 2) 
            ((= 75 modulo) 3) 
            (t 0))))

;60 EDO
   ((or (= 10 approx) (= 600.0 approx) (= 600.1 approx))
    (let ((modulo (mod (approx-m midic 10) 100)))
      (cond ((= 20 modulo) 1) 
            ((= 40 modulo) 2) 
            ((= 60 modulo) 3)
            ((= 80 modulo) 4)(t 0))))

;72 EDO
   ((or (= 720.1 approx) (= 720.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 17) 1)
            ((= modulo 33) 2)
            ((= modulo 50) 3)
            ((= modulo 67) 4)
            ((= modulo 83) 5)
            (t 0))))

;84 EDO
   ((or (= 840.1 approx) (= 840.0 approx))
    (let ((modulo (mod midic 100)))
      (cond ((= modulo 14) 1)
            ((= modulo 29) 2)
            ((= modulo 43) 3)
            ((= modulo 57) 4)
            ((= modulo 71) 5)
            ((= modulo 86) 6)
            (t 0))))


;96 EDO
   ((or (= 16 approx) (=  960.0 approx) (= 960.1 approx)) 
    (let ((modulo (mod midic 100)))
      (cond ((= 12 modulo) 1) 
            ((= 25 modulo) 2) 
            ((= 38 modulo) 3) 
            ((= 50 modulo) 4) 
            ((= 62 modulo) 5) 
            ((= 75 modulo) 6) 
            ((= 88 modulo) 7) 
            (t 0))))

   (t (let ((mod (if (= approx 4) 2 (/ 200 (or *micro-channel-approx* approx)))))
        (round (approx-m (mod midic 100) approx) mod)
        ))))
