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
;;; Author: Anders Vinjar

(in-package :om)

;;; fluidsynth event player

(defmethod player-name ((player (eql :fluidsynth))) "Fluidsynth event player")   
(defmethod player-desc ((player (eql :fluidsynth))) "(default)")   
(defmethod player-special-action ((player (eql :fluidsynth))) nil)  
(defmethod player-params ((player (eql :fluidsynth))) nil)   
(defmethod player-type ((player (eql :fluidsynth))) :midi)   

(progn
  (pushnew :fluidsynth *all-players*)
  (enable-player :fluidsynth))

;; setup fluidsynth as optional player for relevant classes

(add-player-for-object simple-score-element :fluidsynth)
(add-player-for-object score-element :fluidsynth)


;; container is already defined, so fine to specialize here:

(defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) (object container) at interval)
  (let ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object))))))
    (when (or (not interval) note-in-interval?)
      (mapc #'(lambda (sub)
		(prepare-to-play engine player sub (+ at (offset->ms sub)) interval))
	    (inside object)))))

(defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) object at interval)
  (call-next-method))

(defvar *fluidplayer-synth* cl-fluidsynth::*fluidsynth*)

(defun play-note (note)
  (let ((chan (1- (chan note)))		;chans = 1-16
	(key (floor (midic note) 100))
	(dur (/ (dur note) 1000))
	(vel (vel note)))
    (mp:process-run-function "play fluid note" ()
			     #'(lambda ()
				 (cl-fluidsynth::fluid_synth_noteon *fluidplayer-synth* chan key vel)
				 (when (plusp dur) (sleep dur))
				 (cl-fluidsynth::fluid_synth_noteoff *fluidplayer-synth* chan key)))))

(defmethod player-play-object ((engine (eql :fluidsynth)) (object note) &key interval)
  (play-note object))

;; various specializing to play midifile and midievents with fluidsynth:

(defmethod inside ((self midifile))
  (oa::midi-seq-events (fileseq self)))

;;; FIXME; get proper offset
(defmethod offset->ms ((self oa::midimsg2evt) &optional grandparent)
  (oa::event-date self))

(defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) (object midifile) at interval)
  (let ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object))))))
    (when (or (not interval) note-in-interval?)
      (let ((events (inside object)))
	(mapc #'(lambda (sub)
		  (prepare-to-play engine player sub (+ at (offset->ms sub)) interval))
	      events)))))


;; (defmethod offset->ms ((self miditrack) &optional grandparent)
;;   0)

;; (defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) (object midifile) at interval)
;;   (let ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object))))))
;;     (when (or (not interval) note-in-interval?)
;;       (mapc #'(lambda (track)
;; 		(prepare-to-play engine player track (+ at (offset->ms track)) interval))
;; 	    (slot-value object 'tracks)))))

;; (defmethod inside ((self miditrack))
;;   (slot-value self 'midinotes))

;; (defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) (object miditrack) at interval)
;;   (let ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object))))))
;;     (when (or (not interval) note-in-interval?)
;;       (let ((events (inside object)))
;; 	(mapc #'(lambda (sub)
;; 		  (prepare-to-play engine player sub (+ at (offset->ms sub)) interval))
;; 	      events)))))

(defun fluid-play-note (key date dur vel chan)
  (declare (ignore date))
  (mp:process-run-function "play fluid event" ()
			   #'(lambda ()
			       (cl-fluidsynth::fluid_synth_noteon *fluidplayer-synth* chan key vel)
			       (when (plusp dur) (sleep dur))
			       (cl-fluidsynth::fluid_synth_noteoff *fluidplayer-synth* chan key))))

;; play event using OMs own scheduler
(defun play-cl-midinote (event)
  (let ((chan (oa::event-chan event))	
	(key (oa::event-pitch event))
	(dur (/ (oa::event-dur event) 1000.0))
	(vel (oa::event-velocity event)))
    (fluid-play-note key nil dur vel chan)))

(defun play-cl-midiprogramchange (event)
  (let ((chan (oa::event-chan event))	
	(pgm (car (oa::event-fields event))))
    (mp:process-run-function "play fluid program-change" ()
			     #'(lambda ()
				 (cl-fluidsynth::fluid_synth_program_change *fluidplayer-synth* chan pgm)))))

(defun play-cl-midicontrolchange (event)
  (let ((chan (oa::event-chan event))	
	(ctrl (car (oa::event-fields event)))
	(val (cadr (oa::event-fields event))))
    (mp:process-run-function "play fluid cc" ()
			     #'(lambda ()
				 (cl-fluidsynth::fluid_synth_cc *fluidplayer-synth* chan ctrl val)))))

(defmethod player-play-object ((engine (eql :fluidsynth)) (object oa::midimsg2evt) &key interval)
  (when (or (not interval) (point-in-interval (offset->ms object) interval))
    (cond ((= (oa::event-type object) (oa::om-midi-get-num-from-type "Note")) (play-cl-midinote object))
	  ((= (oa::event-type object) (oa::om-midi-get-num-from-type "ProgChange")) (play-cl-midiprogramchange object))
	  ((= (oa::event-type object) (oa::om-midi-get-num-from-type "CtrlChange")) (play-cl-midicontrolchange object))
	  (t nil))))

;; list-representation of events: (pitch date dur vel channel)

(defmethod player-play-object ((engine (eql :fluidsynth)) (object list) &key interval)
  ;; expects list: '(pitch date dur vel channel)
  (apply #'fluid-play-note object))

(defmethod offset->ms ((self list) &optional grandparent)
  )


;; 'edition-params is looked up by new-player, and seems to be
;; defaulted to :midishare somewhere:

(defmethod get-edit-param ((box ommidifilebox) (param (eql 'player))) 
  :fluidsynth)

