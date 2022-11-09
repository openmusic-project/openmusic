;;; ===========================================================================
;;; FluidSynth Player class for OM.
;;; 
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or
;;; (at your option) any later version.
;;;   
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;   
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program; if not, write to the Free Software 
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;; 
;;; Authors: Anders Vinjar, Karim Haddad

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;from scoreobjects.lisp


(add-player-for-object 'score-element '(:midi-player :osc-scoreplayer :microplayer :fluidsynth))
(add-player-for-object 'simple-score-element '(:midi-player :osc-scoreplayer :microplayer :fluidsynth))
(add-player-for-object 'midifile :fluidsynth)


(enable-player :fluidsynth)

(defmethod player-name ((player (eql :fluidsynth))) "OM Fluidsynth player")   ;;; A short name
(defmethod player-desc ((player (eql :fluidsynth))) "FluidSynth system")   ;;; a description
(defmethod player-special-action ((player (eql :fluidsynth))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :fluidsynth))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :fluidsynth))) :midi)   ;;; communication protocol (:midi / :udp)


(defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) object at interval params)
  ;(print (format nil "~s" params))
  
  (let ((approx (if (find :approx params)
                    (nth (1+ (position :approx params)) params)
                  (if (caller player) (get-edit-param (caller player) 'approx))))
       ; (port (if (find :port params)
       ;             (nth (1+ (position :port params)) params)
       ;         (if (caller player) (get-edit-param (caller player) 'outport))))
        ;(port (get-edit-param (caller player) 'outport)) ; avoir -> "error: objects of type null have no edition params!"
        ;;Pas besoin de port ici...
        )
    ;(print (list "params" port player (caller player) ))
    ;(print port)
   ; (if (equal port :default) (setf port *def-midi-out*))
    (mapcar #'(lambda (evt) 
              ;  (call-next-method engine player evt (+ (or (car interval) 0) (om-midi::midi-evt-date evt)) interval params)
                (schedule-task player 
                               #'(lambda () 
                                   ;(print evt)
                                   (player-play-object engine evt :interval interval))
                               (+ (or (car interval) 0) (om-midi::midi-evt-date evt))
                               nil)
                )
            
            
            (append (and (and *midi-microplay* approx (find approx '(4 8) :test '=))
			 ;; (let ((chan-offset (lchan object)))
			 ;;   (microplay-events approx at (get-obj-dur object) port chan-offset))
			 (microplay-events at (get-obj-dur object) port)
			 )
                    (remove nil (flat (PrepareToPlay :midi object at :interval interval :approx approx :port nil)))
                    )
            )
    (sort-events player)
    ))

(defmethod player-start ((engine (eql :fluidsynth)) &optional play-list)
  (midi-start))

(defparameter *key-ons* (make-hash-table :test #'equal))

(defun fluid-all-notes-off ()
  (loop for syn from 0 to (1- (length cl-fluidsynth::*fl-synths*))
          do
          (let ((synth (nth syn cl-fluidsynth::*fl-synths*)))
          (loop for i from 0 to 15
                do (cl-fluidsynth::fluid_synth_all_notes_off 
                    (cl-fluidsynth::getsptr synth)
                    i)))))


(defmethod player-stop ((engine (eql :fluidsynth)) &optional play-list)
  (fluid-all-notes-off)
 ; (loop for i from 0 to 15
 ;       do (cl-fluidsynth::fluid_synth_all_notes_off cl-fluidsynth::*fluidsynth* i))
  (midi-stop)
  (if *midi-microplay* (microplay-reset nil engine)))

(defmethod player-pause ((engine (eql :fluidsynth)) &optional play-list)
  (fluid-all-notes-off))

;; (defmethod player-loop ((self (eql :fluidsynth)) player &optional play-list) (call-next-method))


(defmethod player-play-object ((engine (eql :fluidsynth)) (object om-midi::midi-evt) &key interval params)
  (declare (ignore interval params))
  (let* ((chan (om-midi::midi-evt-chan object))
	 (key-index (1- chan))
	 (port (om-midi::midi-evt-port object))
         (midip (car (om-midi::midi-evt-fields object)))
         (vel (second(om-midi::midi-evt-fields object))))
   ; (print (list "port" object port))
    ;(print (list "info" chan key-index port (om-midi::midi-evt-fields object)))
   ; (when (and *midi-port-modulo-channel* (>= chan 16))
   ;   (incf port (+ (floor key-index 16)))
   ;   (setf (om-midi::midi-evt-port object) port
;	    (om-midi::midi-evt-chan object) (mod chan 16)))
    (cond ((or (equal (om-midi::midi-evt-type object) :keyOff)
	       (and (equal (om-midi::midi-evt-type object) :keyOn) (= 0 (cadr (om-midi::midi-evt-fields object)))))
           (cl-fluidsynth::fluid_synth_noteoff (cl-fluid::getsptr (nth port cl-fluidsynth::*fl-synths*)) (1- chan) midip)
	   (setf (gethash key-index *key-ons*)
		 (delete (list port (car (om-midi::midi-evt-fields object)) chan)
			 (gethash key-index *key-ons*)
			 :test 'equal)))
	  ((equal (om-midi::midi-evt-type object) :keyOn)
            ;(print (list "info" chan ))
           (cl-fluidsynth::fluid_synth_noteon (cl-fluid::getsptr (nth port cl-fluidsynth::*fl-synths*)) (1- chan) midip vel)
	   (pushnew (list port (car (om-midi::midi-evt-fields object)) chan)
		    (gethash key-index *key-ons*) :test 'equal)))
    ;(midi-send-evt object) ;for midi player
    ))
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;choose sf2 font

(defmethod* fluid-choose-sf ((path t) &optional port) 

(fluid_synth_sfload 
(cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
soundfont 1))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;PGMOUT

(defmethod* fluid-pgmout ((progm integer) (chans integer) &optional port) 
  :icon 912
  :indoc '("program number" "MIDI channel(s)" "port")
  :initvals '(2 1 nil)
  :doc "Sends a program change event with program number <progm> to channel(s) <chans>.

<progm> and <chans> can be single numbers or lists."
  (cl-fluidsynth::fluid_synth_program_change 
   (cl-fluid::getsptr (nth port cl-fluidsynth::*fl-synths*)) (1- chans) progm ))

(defmethod* fluid-pgmout ((progm number) (chans list) &optional port)
  (loop for item in chans do
        (fluid-pgmout progm item port)))

(defmethod* fluid-pgmout ((progm list) (chans list) &optional port)
  (if (or (null port) (integerp port))
      (loop for item in chans 
            for item1 in progm do
              (fluid-pgmout item1 item port))
    (loop for item in chans 
          for item1 in progm 
          for item2 in port
          do (fluid-pgmout item1 item item2))))

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
  :doc "Sends gain (0 - 1.0) settings to fluidsynth.:"
  (cl-fluid::fluid_synth_set_gain
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   vals))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;VOLUME

(defmethod! fluid-volume ((vals integer)
                          (chans integer) &optional port)
  :icon 912
  :indoc '("vals" "chans" "port")
  :initvals '(100 1 0)
  :doc "Sends volume control change settings to fluidsynth.:"
  (cl-fluid::fluid_synth_cc
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (1- chans) 7 vals))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;REVERB


(defmethod! fluid-reverb-on ((switch number) &optional port)
  :menuins '((0 (("off" 0) ("on" 1))))
  :initvals '(0)
  :outdoc '("on/off reverb")
  :icon 912
  :doc "Turns on/off fluidsynth's reverb"
    (cl-fluidsynth::fluid_synth_set_reverb_on 
     (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
     switch)) 

(defmethod! fluid-reverb ((roomsize number)
                          (damping number)
                          (width number)
                          (level number)
                          &optional port
                          )
  :icon 912
  :indoc '("roomsize" "damping" "width" "level" "port")
  :initvals '(2.0 0.0 0.5 2.9 0)
  :doc "Sends reverb settings to fluidsynth.:"
  (fluid-reverb-on 1 port)
  (cl-fluidsynth::fluid_synth_set_reverb 
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   (coerce roomsize 'double-float)
   (coerce damping 'double-float)
   (coerce width 'double-float)
   (coerce level 'double-float)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;CHORUS

(defmethod! fluid-chorus-on ((switch number) &optional port)
  :menuins '((0 (("off" 0) ("on" 1))))
  :initvals '(0)
  :outdoc '("on/off reverb")
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
                          &optional port
                          )
  :icon 912
  :indoc '("roomsize" "damping" "width" "level" "port")
  :initvals '(3 2.0 0.3 8.0 0 0)
  :doc "Sends reverb settings to fluidsynth.:"
  (fluid-chorus 1 port)
  (cl-fluid::fluid_synth_set_chorus
   (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
   nr
   (coerce level 'double-float)
   (coerce speed 'double-float)
   (coerce depth 'double-float)
   type))