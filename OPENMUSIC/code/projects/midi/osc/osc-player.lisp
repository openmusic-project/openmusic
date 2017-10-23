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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================


(in-package :om)


(enable-player :osc-player)

(defmethod player-name ((player (eql :osc-player))) "Basic OSC player")   ;;; A short name
(defmethod player-desc ((player (eql :osc-player))) "Sends OSC events via UDP")   ;;; a description
(defmethod player-special-action ((player (eql :osc-player))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :osc-player))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :osc-player))) :udp)   ;;; communication protocol


(defmethod player-play-object ((engine (eql :osc-player)) (object oscevent) &key interval params)
  ;(print (format nil "~A : play ~A - ~A" engine object (list (port object) (host object) (bundle object))))
  (om-send-osc-bundle (port object) (host object) (bundle object))
  )



(enable-player :osc-scoreplayer)

(defparameter *osc-score-output* 3000)

(defmethod player-name ((player (eql :osc-scoreplayer))) "OSC player for score objects")   ;;; A short name
(defmethod player-desc ((player (eql :osc-scoreplayer))) "Sends OSC events via UDP")   ;;; a description
(defmethod player-special-action ((player (eql :osc-scoreplayer))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :osc-scoreplayer))) '("localhost" 3000))   ;;; the default values for the player params
(defmethod player-type ((player (eql :osc-scoreplayer))) :udp)   ;;; communication protocol

(defmethod prepare-to-play ((engine (eql :osc-scoreplayer)) (player omplayer) object at interval params)
  ;(print (format nil "~s" params))
  (mapcar #'(lambda (evt) 
              (schedule-task player 
                             #'(lambda () 
                                 (player-play-object :osc-player evt :interval interval))
                             (+ (or (car interval) 0) (offset evt))
                             nil))
          (remove nil (flat (PrepareToPlay :osc object at :interval interval))))
  (sort-events player)
  )

(defmethod PrepareToPlay ((player (eql :osc)) (self note) at &key  approx port interval voice)
  (when (not (memq (tie self) '(continue end)))
    (let ((pitch (midic self))
          (vel (vel self))
          (dur (real-dur self)))		   
      (let ((newinterval (and interval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))))
        (when (or (null interval) newinterval)
          (note-osc-events pitch vel 
		       ;; dur
		       (if interval 
			   (- (second newinterval) (first newinterval) 1) 
			   dur)
		       ;; DATE
		       (if interval
			   (- (first newinterval) (first interval)) ; (- (first newinterval)  (+ *MidiShare-start-time* (first newinterval)))
			   at)
		       (chan self)))))))

(defun note-osc-events (pitch vel dur date chan)
  (let ((evt (make-instance 'oscevent 
                            :host "localhost" :port *osc-score-output*
                            :bundle (list "/om/note" pitch vel dur chan))))
    (setf (offset evt) date)
    (list evt)))

 