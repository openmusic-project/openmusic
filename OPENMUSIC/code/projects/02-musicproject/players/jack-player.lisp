;;===========================================================================
;;JACK Player class for OM.
;;
;;This program is free software; you can redistribute it and/or modify
;;it under the terms of the GNU Lesser General Public License as published by
;;the Free Software Foundation; either version 2.1 of the License, or
;;(at your option) any later version.
;;  
;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU Lesser General Public License for more details.
;;  
;;You should have received a copy of the GNU Lesser General Public License
;;along with this program; if not, write to the Free Software 
;;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;Author: Anders Vinjar

(in-package :om)

;;; JACK MIDI PLAYER                  
(defmethod player-name ((player (eql :jack))) "Jack midi player")   ;;; A short name
(defmethod player-desc ((player (eql :jack))) "(default)")   ;;; a description
(defmethod player-special-action ((player (eql :jack))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :jack))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :jack))) :midi)   ;;; communication protocol (:midi / :udp)


;; redefined from various places here
(defmethod players-for-object ((self score-element)) '(:jack))
(defmethod players-for-object ((self simple-score-element)) '(:jack))


;; in select-players.lisp
(enable-player :jack)
;;*enabled-players*
;;(enabled-players-for-object t)

(mapcar #'(lambda (pl) (disable-player pl))
	'(:microplayer :libaudiostream :multiplayer
	  ;;:midishare ; keep ms-player while developing jack-player
	  ))
(print *enabled-players*)

(defmethod prepare-to-play ((engine (eql :jack)) (player omplayer) object at interval)
  (call-next-method))

(defmethod player-play-object ((engine (eql :jack)) object &key interval)
  (call-next-method))

(defmethod player-play-object ((engine (eql :jack)) (object chord-seq) &key interval)
  (mapcar #'(lambda (obj chord-onset) (player-play-object :jack obj :interval (+ (or interval 0) chord-onset)))
	  (inside object)
	  (lonset object)))

(defmethod player-play-object ((engine (eql :jack)) (object chord) &key interval)
  (mapcar #'(lambda (obj note-offset)
	      (player-play-object :jack obj :interval (+ (or interval 0) note-offset)))
	  (inside object)
	  (loffset object)))

(defun jack-player-play-note (object interval)
  (let ((start (/ interval 1000.0))
	(dur (/ (get-obj-dur object) 1000.0)) 
	(noteno (/ (midic object) 100))
	(vel (vel object))
	(chan (chan object)))
    (cl-jack::jack-midi-play-event start dur noteno vel chan)))

(defmethod player-play-object ((engine (eql :jack)) (object note) &key interval)
  (jack-player-play-note object interval))

;;; START (PLAY WHAT IS SCHEDULED)
(defmethod player-start ((engine (eql :jack)) &optional play-list)
  (setf cl-jack::*playing* t)
  ;;(when *jackplayer* (jack-midi-start-player *jackplayer*))
  (call-next-method))

;;; PAUSE (all)
(defmethod player-pause ((engine (eql :jack)) &optional play-list)
  ;;(when *jackplayer* (jack-midi-pause-player *jackplayer*))
  (call-next-method))

;;; CONTINUE (all)
(defmethod player-continue ((engine (eql :jack)) &optional play-list)
  ;;(when *jackplayer* (jack-midi-cont-player *jackplayer*))
  (call-next-method))

;;; STOP (all)
(defmethod player-stop ((engine (eql :jack)) &optional play-list)
  (cl-jack::jack-midi-stop-all)
  (call-next-method))
