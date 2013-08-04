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
(defmethod player-name ((player (eql :jackmidi))) "Jack midi player")   ;;; A short name
(defmethod player-desc ((player (eql :jackmidi))) "(default)")   ;;; a description
(defmethod player-special-action ((player (eql :jackmidi))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :jackmidi))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :jackmidi))) :midi)   ;;; communication protocol (:midi / :udp)


;; redefined from various places here
(defmethod players-for-object ((self score-element)) '(:jackmidi))
(defmethod players-for-object ((self simple-score-element)) '(:jackmidi))


;; in select-players.lisp
(enable-player :jackmidi)
;;*enabled-players*
;;(enabled-players-for-object t)

(mapcar #'(lambda (pl) (disable-player pl))
	'(:microplayer :libaudiostream :multiplayer
	  ;;:midishare ; keep ms-player while developing jack-player
	  ))

(defparameter *jack-midi-seqs* (make-hash-table))


#|
(print *enabled-players*)

;; 1) setup ref. queue for this object (chord-seq, voice, editor...)


(setf a (make-hash-table))
(setf obj (make-instance 'chord-seq))
(setf (name obj) (gensym "chord-seq-"))
(name obj)
(setf (gethash (name obj) a) obj)
(describe (gethash (name obj) a))
(maphash #'(lambda (key val) (print (list key val)))
	 *jack-midi-seqs*)

|#

(defun jack-init-queue-for-this-object (object at interval)
  (let ((seq (gethash object *jack-midi-seqs*)))
    (when (hash-table-p seq) (clrhash seq))
    (setf (gethash object *jack-midi-seqs*) (cl-jack::make-om-seq))))

(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) object at interval)
  ;;(print (list 'prepare-to-play engine player object at interval (clock-time) (ref-clock-time player)))
  (describe player)
  (print '(im being called))
  (jack-init-queue-for-this-object object at interval)
  (call-next-method))

(defmethod player-play-object ((engine (eql :jackmidi)) object &key interval)
  (call-next-method))

(defmethod player-play-object ((engine (eql :jackmidi)) (object chord-seq) &key interval)
  (mapcar #'(lambda (obj chord-onset) (player-play-object :jackmidi obj :interval (+ (or interval 0) chord-onset)))
	  (inside object)
	  (lonset object)))

(defmethod player-play-object ((engine (eql :jackmidi)) (object chord) &key interval)
  (mapcar #'(lambda (obj note-offset)
	      (player-play-object :jackmidi obj :interval (+ (or interval 0) note-offset)))
	  (inside object)
	  (loffset object)))

(defun jack-player-play-note (object interval)
  (let ((start (/ interval 1000.0))
	(dur (/ (get-obj-dur object) 1000.0)) 
	(noteno (/ (midic object) 100))
	(vel (vel object))
	(chan (chan object)))
    (cl-jack::jack-midi-play-event start dur noteno vel chan)))

(defmethod player-play-object ((engine (eql :jackmidi)) (object note) &key interval)
  (jack-player-play-note object interval))

;;; START (PLAY WHAT IS SCHEDULED)
(defmethod player-start ((engine (eql :jackmidi)) &optional play-list)
  (setf cl-jack::*playing* t)
  ;;(when *jackplayer* (jack-midi-start-player *jackplayer*))
  (call-next-method))

;;; PAUSE (all)
(defmethod player-pause ((engine (eql :jackmidi)) &optional play-list)
  (print 'pause play-list)
  (call-next-method))

;;; CONTINUE (all)
(defmethod player-continue ((engine (eql :jackmidi)) &optional play-list)
  (print 'continue play-list)
  ;;(when *jackplayer* (jack-midi-cont-player *jackplayer*))
  (call-next-method))

;;; STOP (all)
(defmethod player-stop ((engine (eql :jackmidi)) &optional play-list)
  (cl-jack::jack-midi-stop-all)
  (call-next-method))
