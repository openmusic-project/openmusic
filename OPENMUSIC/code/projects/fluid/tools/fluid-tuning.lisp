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
; Author: Karim Haddad
;==============================
; Fluid tuning parser
; 
;==============================

(in-package :om)

;;;CHANGE-TUNING

(defmethod change-tuning ((self number) (value number))
  (let ((port self))
    (cond 
     ((or (= 2 value) (= 120.1 value))
      (fluid-pitchwheel '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) port))
     ((or (= 3 value) (= 180.0 value) (= 180.1 value))
      (fluid-pitchwheel '(0 2730 5461 0 2730 5461 0 2730 5461 0 2730 5461) '(1 2 3 4 5 6 7 8 9 11 12 13) port))
     ((or (= 4 value) (= 240.1 value)) 
      (fluid-pitchwheel '(0 2048 0 2048 0 2048 0 2048) '(1 3 2 4 5 7 6 8) port))
     ((or (= 5 value) (= 300.0 value) (= 300.1 value)) 
      (fluid-pitchwheel '(0 1638 3276 819 2457 0 1638 3276 819 2457) '(1 2 3 4 5 6 7 8 9 11) port))
     ((or (= 6 value) (= 360.0 value) (= 350.1 value))
      (fluid-pitchwheel '(0 1365 2730) '(1 2 3) port))
     ((or (= 7 value) (= 420.0 value) (= 420.1 value))
      (fluid-pitchwheel '(0 1170 2340 3510 585 1755 2925)
                        '(1 2 3 4 5 6 7) port))
     ((or (= 8 value) (= 480.0 value) (= 480.1 value))
      (fluid-pitchwheel '(0 1024 2048 3072 0 1024 2040 3072  0 1024 2040 3072) 
                        '(1 2 3 4 5 6 7 8 11 12 13 14) port))
     ((or (= 80.1 value) (= 80.0 value))
      (fluid-pitchwheel '(0 2048) 
                        '(1 2) port))
     ((or (= 90.1 value) (= 90.0 value))
      (fluid-pitchwheel '(0 1352 2744) 
                        '(1 2 3) port))
     ((or (= 10 value) 
          (= 100.0 value) (= 100.1 value) ;a voir
          (= 600.0 value) (= 600.1 value)
          )
      (fluid-pitchwheel '(0 819 1638 2458 3277) 
                        '(1 2 3 4 5) port))
     ((or (= 12 value) (= 720.0 value) (= 720.1 value))
      (fluid-pitchwheel '(0 682 1365 2048 2730 3413) '(1 2 3 4 5 6) port))
     ((or (= 14 value) (= 840.0 value) (= 840.1 value))
      (fluid-pitchwheel '(0 585 1170 1755 2340 2925 3510) '(1 2 3 4 5 6 7) port))
     ((or (= 140.0 value) (= 140.1 value))
      (fluid-pitchwheel '(0 3523 2908 2335 1761 1188 573) 
                        '(1 2 3 4 5 6 7) port))
     ((or (= 150.0 value) (= 150.1 value))
      (fluid-pitchwheel '(0 3277 2458 1638 819) 
                        '(1 2 3 4 5) port))
     ((or (= 160.0 value) (= 160.1 value))
      (fluid-pitchwheel '(0 3072 2048 1024) 
                        '(1 2 3 4) port))
     ((or (= 170 value) (= 170.1 value))
      (fluid-pitchwheel
       '(0 2907 1679 491 3358 2170 982 3849 2662 1433 245 3112 1924 737 3604 2416 1187)
       '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18) port))
     ((or (= 180.0 value) (= 180.1 value))
      (fluid-pitchwheel '(0 2744 1352) 
                        '(1 2 3) port))
     ((= 190 value)
      (fluid-pitchwheel
       '(0 2580 1064 3644 2170 655 3235 1720 204 2784 1310 3890 2375 860 3440 1924 450 3030 1515)
       '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18 19 20) port))
     ((or (= 16 value) (= 960.0 value) (= 960.1 value))
      (fluid-pitchwheel '(0 512 1024 1536 2048 2560 3072 3584) '(1 2 3 4 5 6 7 8) port))
     
     ((or (= 220 value) (= 220.0 value) (= 220.1 value))
      (fluid-pitchwheel '(0 2253 369 2621 737 2990 1106 3359 1475 3727 1843) 
                        '(1 2 3 4 5 6 7 8 9 11 12) port))

     ((or (= 300.0 value) (= 300.1 value))
      (fluid-pitchwheel '(0 1638 3277 819 2458) 
                        '(1 2 3 4 5) port))
     ((= 310 value)
      (fluid-pitchwheel
       '(0 1597 3153 655 2252 3849 1310 2907 409 1965 3563 1064 2662 122 1720 3317 778 2375 3972 1433 3030 532 2129 3685 1187 2784 245 1842 3440 941 2498 0)
       '(1 2 3 4 5 6 7 8 9 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32) port))
     ((or (= 360.0 value) (= 360.1 value))
      (fluid-pitchwheel '(0 1352 2744) 
                        '(1 2 3) port))
     ((or (= 420.0 value) (= 420.1 value))
      (fluid-pitchwheel '(0 1188 2335 3523 573 1761 2908) 
                        '(1 2 3 4 5 6 7) port))
     ((or (= 720.0 value) (= 720.1 value))
      (fluid-pitchwheel '(0 696 1352 2048 2744 3499) 
                        '(1 2 3 4 5 6) port))
     ((or (= 840.0 value) (= 840.1 value))
      (fluid-pitchwheel '(0 573 1146 1719 2292 2865 3438 4011) ; Check the values!!!
                        '(1 2 3 4 5 6 7) port))
          
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
