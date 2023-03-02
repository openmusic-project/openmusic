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
; FLUID TOOLS
;
;==============================

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;SOUDFONTS MANAGMENT

;;load sf2

(defmethod* fluid-load-sf2 ((nth-synth number) &optional (path nil)) 
  :icon 912
  :indoc '("nth" "path")
  :initvals '(0 nil)
  :doc "Loads a soundFont file to the given <nth-synth> intance."
  (let ((synth (nth nth-synth cl-fluidsynth::*fl-synths*))
         (pathname (or path (om-choose-file-dialog))))
    (if (= 1 (cl-fluid::fluid_is_soundfont (namestring pathname)))
        (progn
          (setf (cl-fluid::sf2path synth) (namestring pathname))
          (push (cl-fluid::sf2path synth) (cl-fluid::sf2stack synth))
          (cl-fluid::fluid_synth_sfload 
           (cl-fluid::getsptr  synth)
           (namestring pathname)
           1))
      (om-message-dialog (format nil "WARNING: ~A  is not a SoundFont!" (namestring pathname)))
      )))


(defmethod remove-all-sf2 ((synth cl-fluid::fl-synth))
  "removes all loaded soundfonts in <synth>"
  (let ((paths (cl-fluid::sf2stack synth)))
    (loop for i in paths
            do (cl-fluid::fluid_synth_remove_sfont
                 (cl-fluid::getsptr synth)
                 (cl-fluid::fluid_synth_get_sfont_by_name
                  (cl-fluid::getsptr synth)
                  i)))
    (setf (cl-fluid::sf2stack synth) nil)))

#|
(defmethod print-sf2-pgms (&optional (path nil))
  (let ((pathname (or path (om-choose-file-dialog))))
    (om-terminal (format nil "echo \"inst 1\" | fluidsynth -q ~A" (namestring pathname)))))
|#

(defmethod! fluid-make-presets ((port number))
"Returns the presets of a sfont loaded in a fsynth with port number <port> for console.
The format is a list of list where the car of each is the string name of the preset followed by the number program."
(let ((sf2
       (cl-fluid::fluid_synth_get_sfont 
        (cl-fluid::getsptr  (nth port cl-fluid::*fl-synths*))
        0)))
  (if (cffi:null-pointer-p sf2)
      (om-beep-msg "no soundfont loaded in this synth")
    (remove nil
    (loop for i from 0 to 127
        collect (let ((pt (cl-fluid::fluid_sfont_get_preset sf2 0 i)))
                  (unless (cffi:null-pointer-p pt)
                    (list (cl-fluid::fluid_preset_get_name pt) i))))))))

;(fluid-make-presets 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;PGMOUT

(defmethod* fluid-pgmout ((progm integer) (chans integer) &optional (port 0)) 
  :icon 912
  :indoc '("program number" "MIDI channel(s)" "port")
  :initvals '(2 1 0)
  :doc "Sends a program change event with program number <progm> to channel(s) <chans>.

<progm> and <chans> can be single numbers or lists."
  (cl-fluidsynth::fluid_synth_program_change 
   (cl-fluid::getsptr (nth port cl-fluidsynth::*fl-synths*)) (1- chans) progm ))

(defmethod* fluid-pgmout ((progm number) (chans list) &optional (port 0))
  (loop for item in chans do
        (fluid-pgmout progm item port)))

(defmethod* fluid-pgmout ((progm list) (chans list) &optional (port 0))
  (if (or (null port) (integerp port))
      (loop for item in chans 
            for item1 in progm do
              (fluid-pgmout item1 item port))
    (loop for item in chans 
          for item1 in progm 
          for item2 in port
          do (fluid-pgmout item1 item item2))))

;;;use this instead:


(defmethod* fluid-pgm-change ((progm integer) (chans integer) &key (port 0) (bank 0)) 
  :icon 912
  :indoc '("program number" "MIDI channel(s)" "port" "bank")
  :initvals '(2 1 0 0)
  :doc "Sends a program change event with program number <progm> to channel(s) <chans>.
<progm> and <chans> can be single numbers or lists.
<port> port 0 = 1st instance of fsynth."
  (let ((synth (cl-fluid::getsptr (nth port cl-fluidsynth::*fl-synths*))))
    (cl-fluidsynth::fluid_synth_program_change 
     synth (1- chans) progm)
    (cl-fluid::fluid_synth_bank_select
     synth (1- chans) bank)
    (cl-fluid::fluid_synth_program_reset synth)
    ))

(defmethod* fluid-pgm-change ((progm integer) (chans list) &key (port 0) (bank 0)) 
  (loop for i in chans
        do (fluid-pgm-change progm i :port port :bank bank)))

(defmethod* fluid-pgm-change ((progm list) (chans list) &key (port 0) (bank 0)) 
  (loop for pg in progm
        for ch in chans
        do (fluid-pgm-change pg ch :port port :bank bank)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;PITCHWHEEL

(defmethod* fluid-pitchwheel ((vals integer) (chans integer) &optional port) 
  :icon 912
  :indoc '("pitchwheel value" "MIDI channel(s)" "port")
  :initvals '(2 1 nil)
  :doc "Sends a pitchwheel message
   1/16th tonw = 512
   1/8th tone = 1024
   1/4th tone = 2048"
  (let ((port (if port port 0)))
  (cl-fluidsynth::fluid_synth_pitch_bend
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (1- chans) (+ 8192 vals))))

(defmethod* fluid-pitchwheel ((vals integer) (chans list) &optional port) 
  (loop for item in chans do
          (fluid-pitchwheel vals item port)))

(defmethod* fluid-pitchwheel ((vals list) (chans list) &optional port) 
  (loop for item in chans 
        for item1 in vals
        do
          (fluid-pitchwheel item1 item port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;GAIN

(defmethod! fluid-gain ((vals number) &optional port)
  :icon 912
  :indoc '("vals" "nth-synth")
  :initvals '(0.8 0)
  :doc "Sends gain (0 - 1.0) settings to fluidsynth."
  (if (atom port)
      (cl-fluid::fluid_synth_set_gain
       (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
       vals)
    (loop for i in port
          do (cl-fluid::fluid_synth_set_gain
              (cl-fluid::getsptr  (nth i cl-fluidsynth::*fl-synths*))
              vals)
             )))

(defmethod! fluid-gain ((vals list) &optional port)
  (loop for i in vals
        for p in port
        do (fluid-gain i p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;VOLUME

(defmethod! fluid-volume ((vals integer)
                          (chans integer) &optional (port 0))
  :icon 912
  :indoc '("vals" "chans" "port")
  :initvals '(100 1 0)
  :doc "Sends volume control change settings to channel <chans>."
  (cl-fluid::fluid_synth_cc
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (1- chans) 7 vals))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;PAN

(defmethod! fluid-pan ((vals integer)
                       (chans integer) &optional (port 0))
  :icon 912
  :indoc '("vals" "chans" "port")
  :initvals '(64 1 0)
  :doc "Sends volume control change settings to channel <chans>."
  (cl-fluid::fluid_synth_cc
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (1- chans) 10 vals))

(defmethod! fluid-pan ((vals integer)
                       (chans list) &optional (port 0))
  (loop for i in chans
        do (fluid-pan vals i port)))

(defmethod! fluid-pan ((vals list)
                       (chans list) &optional (port 0))
  (loop for i in chans
        for val in vals
        do (fluid-pan val i port)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;REVERB


(defmethod! fluid-reverb-on ((switch number) &optional (port 0))
  :menuins '((0 (("off" 0) ("on" 1))))
  :initvals '(0 0)
  :outdoc '("on/off reverb" "port")
  :icon 912
  :doc "Turns on/off fluidsynth's reverb"
    (cl-fluidsynth::fluid_synth_set_reverb_on 
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     switch)) 

(defmethod! fluid-reverb ((roomsize number)
                          (damping number)
                          (width number)
                          (level number)
                          &optional (port 0)
                          )
  :icon 912
  :indoc '("roomsize" "damping" "width" "level" "port")
  :initvals '(2.0 0.0 0.5 2.9 0)
  :doc "Sends reverb settings to fluidsynth.:"
  ;(fluid-reverb-on 1 port)
  (cl-fluidsynth::fluid_synth_set_reverb 
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (coerce roomsize 'double-float)
   (coerce damping 'double-float)
   (coerce width 'double-float)
   (coerce level 'double-float)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;CHORUS

(defmethod! fluid-chorus-on ((switch number) &optional (port 0))
  :menuins '((0 (("off" 0) ("on" 1))))
  :initvals '(0 0)
  :outdoc '("on/off chorus" "port")
  :icon 912
  :doc "Turns on/off fluidsynth's reverb"
    (cl-fluidsynth::fluid_synth_set_chorus_on 
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     switch))

(defmethod! fluid-chorus ((nr number)
                          (level number)
                          (speed number)
                          (depth number)
                          (type number)
                          &optional (port 0)
                          )
  :icon 912
  :indoc '("nr" "level" "speed" "depth" "type" "port")
  :initvals '(3 2.0 0.3 8.0 0 0)
  :doc "Sends chorus settings to fluidsynth.:"
  ;(fluid-chorus-on 1 port)
  (cl-fluid::fluid_synth_set_chorus
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   nr
   (coerce level 'double-float)
   (coerce speed 'double-float)
   (coerce depth 'double-float)
   type))
