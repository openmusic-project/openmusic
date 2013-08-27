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

;; setup fluidsynth as option for relevant classes

(let* ((curlist (players-for-object (make-instance 'simple-score-element)))
       (newlist (pushnew :fluidsynth curlist)))
  (defmethod players-for-object ((self simple-score-element)) newlist))

(let* ((curlist (players-for-object (make-instance 'score-element)))
       (newlist (pushnew :fluidsynth curlist)))
  (defmethod players-for-object ((self score-element)) newlist))

;;

(defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) object at interval)
  (if (container-p object)
      (let* ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object)))))
	     (interval-at (if interval (- at (car interval)) at)))
	(when (or (not interval) note-in-interval?)
	  (mapc #'(lambda (sub)
		    (prepare-to-play engine player sub (+ at (offset->ms sub)) interval))
		(inside object))))
      (call-next-method)))

(defvar *fluidplayer-synth* cl-fluidsynth::*fluidsynth*)

(defun play-note (note)
  (let ((chan (chan note))
	(key (floor (midic note) 100))
	(dur (/ (dur note) 1000))
	(vel (vel note))
	(synth *fluidplayer-synth*))
    (mp:process-run-function "play fluid note" ()
			     #'(lambda ()
				 (cl-fluidsynth::fluid_synth_noteon *fluidplayer-synth* chan key vel)
				 (sleep dur)
				 (cl-fluidsynth::fluid_synth_noteoff *fluidplayer-synth* chan key)))))

(defmethod player-play-object ((engine (eql :fluidsynth)) (object note) &key interval)
  (play-note object))




