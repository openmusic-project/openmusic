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

;;; Authors: Anders Vinjar, Karim Haddad

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-player-for-object 'score-element '(:midi-player :osc-scoreplayer :microplayer :fluidsynth))
(add-player-for-object 'simple-score-element '(:midi-player :osc-scoreplayer :microplayer :fluidsynth))
(add-player-for-object 'arp-chord '(:midi-player :osc-scoreplayer :microplayer :fluidsynth))
(add-player-for-object 'midifile '(:midi-plater :fluidsynth))


(enable-player :fluidsynth)

(defmethod player-name ((player (eql :fluidsynth))) "OM Fluidsynth player")   ;;; A short name
(defmethod player-desc ((player (eql :fluidsynth))) "FluidSynth system")   ;;; a description
(defmethod player-special-action ((player (eql :fluidsynth))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :fluidsynth))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :fluidsynth))) :midi)   ;;; communication protocol (:midi / :udp)


(defun get-gen-port (self)
  (let ((ports (remove-duplicates  (flat (get-port self)))))
    (if (= 1 (length ports)) (car ports))))


(defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) object at interval params)
  (let ((approx (if (find :approx params)
                    (nth (1+ (position :approx params)) params)
                  (if (caller player) (get-edit-param (caller player) 'approx))))
        ;(port (if (find :port params)
        ;            (nth (1+ (position :port params)) params)
        ;        (if (caller player) (get-edit-param (caller player) 'outport))))
        ;(port (get-edit-param (caller player) 'outport)) ; avoir -> "error: objects of type null have no edition params!"
        (port (get-gen-port object))
        )
    ;(print (list "params"  player (caller player) (object (caller player))  approx params port))
    (when (and port *fluid-auto-microtune*)
      (change-tuning port approx))
    ; (if (equal port :default) (setf port *def-midi-out*))
    (mapcar #'(lambda (evt) 
              ;  (call-next-method engine player evt (+ (or (car interval) 0) (om-midi::midi-evt-date evt)) interval params)
                (schedule-task player 
                               #'(lambda () 
                                   ;(print evt)
                                   (player-play-object engine evt :interval interval))
                               (+ (or (car interval) 0) (om-midi::midi-evt-date evt))
                               nil))
            (append (and (and *midi-microplay* approx (find approx '(4 8) :test '=))
			 ;; (let ((chan-offset (lchan object)))
			 ;;   (microplay-events approx at (get-obj-dur object) port chan-offset))
			 (microplay-events at (get-obj-dur object) port)
			 )
                    (remove nil (flat (PrepareToPlay :midi object at :interval interval :approx approx :port nil)))
                    ))
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
  (let ((chan (om-midi::midi-evt-chan object)))
    (when chan
      (let* ((key-index (1- chan))
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
               (cl-fluidsynth::fluid_synth_noteon (cl-fluid::getsptr (nth port cl-fluidsynth::*fl-synths*)) (1- chan) midip vel)
               (pushnew (list port (car (om-midi::midi-evt-fields object)) chan)
                        (gethash key-index *key-ons*) :test 'equal))
              ((equal (om-midi::midi-evt-type object) :progchange)
               (fluid-pgm-change (car (om-midi::midi-evt-fields object)) 
                                 (om-midi::midi-evt-chan object)
                                 :port port))
              ((equal (om-midi::midi-evt-type object) :ctrlchange)
               (cl-fluid::fluid_synth_cc
                (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
                (1- (om-midi::midi-evt-chan object)) (car (om-midi::midi-evt-fields object))
                (second (om-midi::midi-evt-fields object))
                ))
              ((equal (om-midi::midi-evt-type object) :pitchbend)
               (cl-fluid::fluid_synth_pitch_bend
                (cl-fluid::getsptr  (nth port cl-fluidsynth::*fl-synths*))
                (1- (om-midi::midi-evt-chan object)) 
                (car (om-midi::midi-evt-fields object))
                ))
              ((equal (om-midi::midi-evt-type object) :lyric)
               (unless (or (= 10 (car (om-midi::midi-evt-fields object)))
                           (= 13 (car (om-midi::midi-evt-fields object))))
                 (print (ascii->string (om-midi::midi-evt-fields object))))
               ))))))

 

