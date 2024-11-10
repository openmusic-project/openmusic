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

;;; FLUID package
; Authors: Karim Haddad, Steven Socha
;==============================
; Fluid tuning parser
; 
;==============================

(in-package :om)

;;;CHANGE-TUNING

(defmethod change-tuning ((self number) (value number))
  (let ((port self))
    (cond 

;5 EDO
     ((or (= 50.0 value) (= 50.1 value))
      (fluid-pitchwheel '(0 1638 3277 819 2458) '(1 2 3 4 5) port))

;6 EDO
     ((or (= 1 value) (= 60.1 value))
      (fluid-pitchwheel '(0) '(1) port))

;6 EDO(t)
     ((= 1.0 value)
      (fluid-pitchwheel '(4096) '(1) port))

;7 EDO   
     ((= 70 value)
      (fluid-pitchwheel '(0 2926 1755 585 3511 2341 1170) 
                        '(1 2 3 4 5 6 7) port))
;8 EDO
     ((or (= 80.1 value) (= 80.0 value))
      (fluid-pitchwheel '(0 2048) 
                        '(1 2) port))

;9 EDO
     ((or (= 90.1 value) (= 90.0 value))
      (fluid-pitchwheel '(0 1365 2731) 
                        '(1 2 3) port))
;10 EDO
     ((or (= 100.0 value) (= 100.1 value))
      (fluid-pitchwheel '(0 819 1638 2458 3277) 
                        '(1 2 3 4 5) port))
;12 EDO
     ((or (= 2 value) (= 120.1 value))
      (fluid-pitchwheel (repeat-n 0 16) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) port))

;14 EDO
     ((or (= 140.0 value) (= 140.1 value))
      (fluid-pitchwheel '(0 3511 2926 2341 1755 1170 585) 
                        '(1 2 3 4 5 6 7) port))
;15 EDO
     ((or (= 150.0 value) (= 150.1 value))
      (fluid-pitchwheel '(0 3277 2458 1638 819) 
                        '(1 2 3 4 5) port))
;16 EDO
     ((or (= 160.0 value) (= 160.1 value))
      (fluid-pitchwheel '(0 3072 2048 1024) 
                        '(1 2 3 4) port))

;17 EDO 
     ((or (= 170 value) (= 170.0 value) (= 170.1 value))
      (fluid-pitchwheel
       '(0 2891 1687 482 3373 2168 964 3855 2650 1446 241 3132 1928 723 3614 2409 1205)
       '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18) port))

;18 EDO    
     ((or (= 3 value) (= 180.0 value) (= 180.1 value))
      (fluid-pitchwheel '(0 2731 1365) '(1 2 3) port))

;18 EDO(t)
     ((= 3.0 value)
      (fluid-pitchwheel '(4096 6827 5461) '(1 2 3) port))

;19 EDO
     ((= 190 value)
      (fluid-pitchwheel
       '(0 2587 1078 3665 2156 647 3234 1725 216 2803 1293 3880 2371 862 3449 1940 431 3018 1509)
       '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18 19 20) port))

;22 EDO
     ((or (= 220 value) (= 220.0 value) (= 220.1 value))
      (fluid-pitchwheel '(0 2234 372 2607 745 2979 1117 3351 1489 3724 1862) 
                        '(1 2 3 4 5 6 7 8 9 11 12) port))
;24 EDO
     ((or (= 4 value) (= 240.1 value)) 
      (fluid-pitchwheel '(0 2048) '(1 2) port))

;30 EDO
     ((or (= 5 value) (= 300.0 value) (= 300.1 value)) 
      (fluid-pitchwheel '(0 1638 3277 819 2458) '(1 2 3 4 5) port))

;30 EDO(t)
     ((= 5.0 value) 
      (fluid-pitchwheel '(4096 5734 7373 4915 6554) '(1 2 3 4 5) port))

;31 EDO     
     ((= 310 value)
      (fluid-pitchwheel
       '(0 1586 3171 661 2246 3832 1321 2907 396 1982 3567 1057 2643 132 1718 3303 793 2378 3964 1453 3039 529 2114 3700 1189 2775 264 1850 3435 925 2510)
       '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32) port))

     ((= 310 value)
      (fluid-pitchwheel
       '(0 1586 3171 661 2246 3832 1321 2907 396 1982 3567 1057 2643 132 1718 3303 793 2378 3964 1453 3039 529 2114 3700 1189 2775 264 1850 3435 925 2510)
       '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32) port))

;36 EDO
     ((or (= 6 value) (= 360.0 value) (= 360.1 value))
      (fluid-pitchwheel '(0 1365 2731) 
                        '(1 2 3) port))

;42 EDO
     ((or (= 7 value) (= 420.0 value) (= 420.1 value))
      (fluid-pitchwheel '(0 1170 2341 3511 585 1755 2926)
                        '(1 2 3 4 5 6 7) port))

;42 EDO(t)
     ((= 7.0 value)
      (fluid-pitchwheel '(4096 5266 6436 7607 4681 5852 7022)
                        '(1 2 3 4 5 6 7) port))

;48 EDO
     ((or (= 8 value) (= 480.0 value) (= 480.1 value))
      (fluid-pitchwheel '(0 1024 2048 3072) 
                        '(1 2 3 4) port))

;60 EDO
     ((or (= 10 value) (= 600.0 value) (= 600.1 value))
      (fluid-pitchwheel '(0 819 1638 2458 3277) 
                        '(1 2 3 4 5) port))
;72 EDO  
     ((or (= 12 value) (= 720.0 value) (= 720.1 value))
      (fluid-pitchwheel '(0 683 1365 2048 2731 3413) 
                        '(1 2 3 4 5 6) port))

;84 EDO
     ((or (= 14 value) (= 840.0 value) (= 840.1 value))
      (fluid-pitchwheel '(0 585 1170 1755 2341 2926 3511) '(1 2 3 4 5 6 7) port))

;96 EDO
     ((or (= 16 value) (= 960.0 value) (= 960.1 value))
      (fluid-pitchwheel '(0 512 1024 1536 2048 2560 3072 3584) '(1 2 3 4 5 6 7 8) port))
          
     (t 
      (fluid-pitchwheel (repeat-n 0 16) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) port)))))


(defmethod change-tuning ((self number) (value string))
  (let ((port self)
        (val (car (find value *scales-list* :test 'equal :key 'third))))
    (change-tuning port val)))

(defmethod change-tuning ((self fluidPanel) value)
  (let ((port (midiport (channelctr self))))
    (change-tuning port value)))
    
(defmethod change-tuning ((self fluid-ctrl) value)
  (let ((port (midiport self)))
    (change-tuning port value)))
