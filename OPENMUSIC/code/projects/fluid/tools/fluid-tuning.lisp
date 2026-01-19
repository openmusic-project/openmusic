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
;==============================

(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;OCTAVE TUNINGS;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;for EDOs 17, 19, 22, 31 and 41

;;;INIT

(defun init-edos-octave-tunings (port)
  (progn

;===17 EDO===

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 0 "EDO_17a" 
     (let ((data (loop for i in '(0.0 41.0 12.0 53.0 24.0 65.0 35.0 6.0 47.0 18.0 59.0 29.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 0 "EDO_17b" 
     (let ((data (loop for i in '(71.0 0.0 82.0 0.0 94.0 0.0 0.0 76.0 0.0 88.0 0.0 0.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)
;===19 EDO===

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 1 "EDO_19a" 
     (let ((data (loop for i in '(0.0 26.0 53.0 16.0 42.0 5.0 32.0 58.0 21.0 47.0 11.0 37.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)


    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 1 "EDO_19b" 
     (let ((data (loop for i in '(63.0 89.0 0.0 79.0 0.0 68.0 95.0 0.0 84.0 0.0 74.0 0.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)    
;===22 EDO===

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 2 "EDO_22a" 
     (let ((data (loop for i in '(0.0 9.0 18.0 27.0 36.0 45.0 0.0 9.0 18.0 27.0 36.0 45.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)


    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 2 "EDO_22b" 
     (let ((data (loop for i in '(55.0 64.0 73.0 82.0 91.0 0.0 55.0 64.0 73.0 82.0 91.0 0.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)
    
;===31 EDO===

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 3 "EDO_31a" 
     (let ((data (loop for i in '(0.0 16.0 32.0 10.0 26.0 3.0 19.0 35.0 13.0 29.0 6.0 23.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)


    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 3 "EDO_31b" 
     (let ((data (loop for i in '(39.0 55.0 71.0 48.0 65.0 42.0 58.0 74.0 52.0 68.0 45.0 61.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     2 3 "EDO_31c" 
     (let ((data (loop for i in '(77.0 94.0 0.0 87.0 0.0 81.0 97.0 0.0 90.0 0.0 84.0 0.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double 
                           :count (length data)
                           :initial-contents data)) 1)

;===41 EDO===


    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 4 "41-EDO_a"
     (let ((data (loop for i in '(0.0 17.0 5.0 22.0 10.0 27.0 15.0 2.0 20.0 7.0 24.0 12.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double
                           :count (length data)
                           :initial-contents data)) 1)

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 4 "41-EDO_b"
     (let ((data (loop for i in '(29.0 46.0 34.0 51.0 39.0 56.0 44.0 32.0 49.0 37.0 54.0 41.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double
                           :count (length data)
                           :initial-contents data)) 1)

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     2 4 "41-EDO_c"
     (let ((data (loop for i in '(59.0 76.0 63.0 80.0 68.0 85.0 73.0 61.0 78.0 66.0 83.0 71.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double
                           :count (length data)
                           :initial-contents data)) 1)

    (cl-fluid::fluid_synth_activate_octave_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     3 4 "41-EDO_d"
     (let ((data (loop for i in '(88.0 0.0 93.0 0.0 98.0 0.0 0.0 90.0 0.0 95.0 0.0 0.0)
                       collect (coerce i 'double-float))))
       (cffi:foreign-alloc :double
                           :count (length data)
                           :initial-contents data)) 1)

;===53 EDO===

(cl-fluid::fluid_synth_activate_octave_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
0 5 "53-EDO_a"
(let ((data (loop for i in '(0.0 13.0 4.0 17.0 8.0 21.0 11.0 2.0 15.0 6.0 19.0 9.0)
collect (coerce i 'double-float))))
(cffi:foreign-alloc :double
:count (length data)
:initial-contents data)) 1)

(cl-fluid::fluid_synth_activate_octave_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
1 5 "53-EDO_b"
(let ((data (loop for i in '(23.0 36.0 26.0 40.0 30.0 43.0 34.0 25.0 38.0 28.0 42.0 32.0)
collect (coerce i 'double-float))))
(cffi:foreign-alloc :double
:count (length data)
:initial-contents data)) 1)

(cl-fluid::fluid_synth_activate_octave_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
2 5 "53-EDO_c"
(let ((data (loop for i in '(45.0 58.0 49.0 62.0 53.0 66.0 57.0 47.0 60.0 51.0 64.0 55.0)
collect (coerce i 'double-float))))
(cffi:foreign-alloc :double
:count (length data)
:initial-contents data)) 1)

(cl-fluid::fluid_synth_activate_octave_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
3 5 "53-EDO_d"
(let ((data (loop for i in '(68.0 81.0 72.0 85.0 75.0 89.0 79.0 70.0 83.0 74.0 87.0 77.0)
collect (coerce i 'double-float))))
(cffi:foreign-alloc :double
:count (length data)
:initial-contents data)) 1)

(cl-fluid::fluid_synth_activate_octave_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
4 5 "53-EDO_e"
(let ((data (loop for i in '(91.0 0.0 94.0 0.0 98.0 0.0 0.0 92.0 0.0 96.0 0.0 0.0)
collect (coerce i 'double-float))))
(cffi:foreign-alloc :double
:count (length data)
:initial-contents data)) 1)

))

;a INITIALISER quand on charge les synths et selon number of synths:
; a mettre dans fluid-preferences juste apres (load-sf-to-all)
(defun init-oct-tun-to-all-synth ()
  (when cl-fluid::*fl-synths*
    (loop for i from 0 to (1- (length cl-fluid::*fl-synths*))
          do (init-edos-octave-tunings i))))

;;;;;;;;;;;;

(defmacro fluidloaded? (&body body)
  `(if (not cl-fluid::*fl-synths*)
      (om-message-dialog "No fluidsynth loaded!")
    ,@body))

(defmethod activate-17edo ((port number))
  (fluidloaded? 
  (progn 
        (cl-fluid::fluid_synth_activate_tuning
         (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
         0 ;chan
         0 ;bank
         0 ;prog 
         1)

        (cl-fluid::fluid_synth_activate_tuning
         (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
         1 ;chan
         1 ;bank
         0 ;prog 
         1)
        )))

(defmethod activate-19edo ((port number))
  (fluidloaded? 
  (progn
    (cl-fluid::fluid_synth_activate_tuning
       (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
       0 ;chan
       0 ;bank
       1 ;prog 
       1)

      (cl-fluid::fluid_synth_activate_tuning
       (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
       1 ;chan
       1 ;bank
       1 ;prog 
       1))))

(defmethod activate-22edo ((port number))  
  (fluidloaded? 
  (progn
    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 ;chan
     0 ;bank
     2 ;prog 
     1)

    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 ;chan
     1 ;bank
     2 ;prog 
     1))))

(defmethod activate-31edo ((port number))  
  (fluidloaded? 
  (progn
    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 ;chan
     0 ;bank
     3 ;prog 
     1)

    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 ;chan
     1 ;bank
     3 ;prog 
     1)

    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     2 ;chan
     2 ;bank
     3 ;prog 
     1) 
    )))

(defmethod activate-41edo ((port number))  
  (fluidloaded? 
  (progn
    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     0 ;chan
     0 ;bank
     4 ;prog 
     1)

    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     1 ;chan
     1 ;bank
     4 ;prog 
     1)

    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     2 ;chan
     2 ;bank
     4 ;prog 
     1)

    (cl-fluid::fluid_synth_activate_tuning
     (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
     3 ;chan
     3 ;bank
     4 ;prog 
     1)

    )))

(defmethod activate-53edo ((port number))  
  (fluidloaded? 
  (progn
(cl-fluid::fluid_synth_activate_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
0 ;chan
0 ;bank
5 ;prog
1)

(cl-fluid::fluid_synth_activate_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
1 ;chan
1 ;bank
5 ;prog
1)

(cl-fluid::fluid_synth_activate_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
2 ;chan
2 ;bank
5 ;prog
1)

(cl-fluid::fluid_synth_activate_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
3 ;chan
3 ;bank
5 ;prog
1)

(cl-fluid::fluid_synth_activate_tuning
(cl-fluid::synthptr (nth port cl-fluid::*fl-synths*))
4 ;chan
4 ;bank
5 ;prog
1)

)))


;;All other EDOs deactivate otcave tuning:
(defmethod oct-tun-off ((port number) (chan list))
  (when cl-fluid::*fl-synths*
  (loop for i in chan
        do (cl-fluid::fluid_synth_deactivate_tuning
            (cl-fluid::synthptr (nth port cl-fluid::*fl-synths*)); 0 -> port
            i ;chan
            1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;CHANGE-TUNING;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defmethod change-tuning ((self number) (value number))
  (let ((port self))
    (fluid-pitchwheel (repeat-n 0 16) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16) port)
    (oct-tun-off port '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    (cond 
;5 EDO
     ((or (= 50.0 value) (= 50.1 value))
      (fluid-pitchwheel '(0 1638 3277 819 2458) '(1 2 3 4 5) port))

;6 EDO
     ((or (equal 1 value) (= 60.1 value))
      (fluid-pitchwheel '(0) '(1) port))

;6 EDO(t)
     ((equal 1.0 value)
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
      (activate-17edo port))

;18 EDO    
     ((or (equal 3 value) (= 180.0 value) (= 180.1 value))
      (fluid-pitchwheel '(0 2731 1365) '(1 2 3) port))

;18 EDO(t)
     ((equal 3.0 value)
      (fluid-pitchwheel '(4096 6827 5461) '(1 2 3) port))

;19 EDO
     ((= 190 value) (activate-19edo port))

;22 EDO
     ((or (= 220 value) (= 220.0 value) (= 220.1 value))
      (activate-22edo port))
;24 EDO
     ((or (= 4 value) (= 240.1 value)) 
      (fluid-pitchwheel '(0 2048 0 2048 0 2048 0 2048 0 2048 0 2048 0 2048) 
                        '(1 3 2 4 5 7 6 8 9 11 12 14 13 15) port))

;30 EDO
     ((or (equal 5 value) (= 300.0 value) (= 300.1 value)) 
      (fluid-pitchwheel '(0 1638 3277 819 2458) '(1 2 3 4 5) port))

;30 EDO(t)
     ((equal 5.0 value) 
      (fluid-pitchwheel '(4096 5734 7373 4915 6554) '(1 2 3 4 5) port))

;31 EDO     
     ((= 310 value) (activate-31edo port))
     
;36 EDO
     ((or (= 6 value) (= 360.0 value) (= 360.1 value))
      (fluid-pitchwheel '(0 1365 2731) 
                        '(1 2 3) port))

;41 EDO     
     ((or (= 410.0 value) (= 410.1 value) (= 410.2 value) (= 410.3 value) (= 410.4 value))
      (activate-41edo port))


;42 EDO
     ((or (equal 7 value) (= 420.0 value) (= 420.1 value))
      (fluid-pitchwheel '(0 1170 2341 3511 585 1755 2926)
                        '(1 2 3 4 5 6 7) port))

;42 EDO(t)
     ((equal 7.0 value)
      (fluid-pitchwheel '(4096 5266 6436 7607 4681 5852 7022)
                        '(1 2 3 4 5 6 7) port))

;48 EDO
     ((or (= 8 value) (= 480.0 value) (= 480.1 value))
      (fluid-pitchwheel '(0 1024 2048 3072 0 1024 2048 3072) 
                        '(1 2 3 4 5 6 7 8) port))

;53 EDO
     ((or (= 530.0 value) (= 530.1 value) (= 530.2 value))
      (activate-53edo port))

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
