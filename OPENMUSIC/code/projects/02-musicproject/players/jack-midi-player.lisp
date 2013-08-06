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
(defmethod player-name ((player (eql :jackmidi))) "Jack midi player")   
(defmethod player-desc ((player (eql :jackmidi))) "(default)")   
(defmethod player-special-action ((player (eql :jackmidi))) nil)  
(defmethod player-params ((player (eql :jackmidi))) nil)   
(defmethod player-type ((player (eql :jackmidi))) :midi)   


;; redefined from various places here
(defmethod players-for-object ((self score-element)) '(:jackmidi))
(defmethod players-for-object ((self simple-score-element)) '(:jackmidi))


;; in select-players.lisp
(enable-player :jackmidi)
;;*enabled-players*
;;(enabled-players-for-object t)

(mapc #'(lambda (pl) (disable-player pl))
      '(:microplayer :libaudiostream :multiplayer
	;;:midishare ; keep ms-player while developing jack-player
	))

(enabled-players-for-object (make-instance 'voice))

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
  (print (list 'prepare-to-play object))
  (jack-init-queue-for-this-object object at interval)
  ;;(print (list 'prepare-to-play object))
  (call-next-method))

(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) (object simple-container) at interval)
  (jack-init-queue-for-this-object object at interval)
  ;;(print (list 'prepare-to-play object))
  (call-next-method)
  )

(defmethod player-play-object ((engine (eql :jackmidi)) object &key interval)
  (call-next-method))

(defmethod player-play-object ((engine (eql :jackmidi)) (object simple-container) &key interval)
  (mapc #'(lambda (obj)
	    (player-play-object :jackmidi obj
				:interval (or interval 0)))
	(inside object)))



(defmethod player-play-object ((engine (eql :jackmidi)) (object poly) &key interval)
  ;;(break)
  ;;(print (list 'player-play-object object (inside object)))
  (mapc #'(lambda (sub)
	      (player-play-object engine sub :interval interval))
	  (inside object)))

(defmethod player-play-object ((engine (eql :jackmidi)) (object voice) &key interval)
  ;;(print (list 'player-play-object object (inside object)))
  (mapc #'(lambda (sub)
	      ;;(print (list 'offset->ms (offset->ms sub) sub))
  	      (player-play-object engine sub :interval (+ (or interval 0) (offset object)
							  (offset->ms sub)
							  )))
  	  (inside object))
  ;;(call-next-method)
  )

(defmethod player-play-object ((engine (eql :jackmidi)) (object measure) &key interval)
  (mapc #'(lambda (sub)
  	      (player-play-object engine sub :interval (+ (or interval 0) (offset object)
							  (offset->ms sub)
							  )))
  	  (inside object))
  ;;(call-next-method)
  )

(defmethod player-play-object ((engine (eql :jackmidi)) (object group) &key interval)
  (print (list 'player-play-object object (inside object)))
  (mapc #'(lambda (sub)
  	      (player-play-object engine sub :interval (+ (or interval 0) (offset object) (offset->ms sub))))
  	  (inside object))
  ;;(call-next-method)
  )

(defmethod player-play-object ((engine (eql :jackmidi)) (object chord-seq) &key interval)
  (mapc #'(lambda (chord chord-onset)
	      (player-play-object engine chord :interval (+ (or interval 0) chord-onset)))
	  (inside object)
	  (lonset object)))

(defmethod player-play-object ((engine (eql :jackmidi)) (object chord) &key interval)
  (mapc #'(lambda (obj note-offset)
	      (player-play-object engine obj :interval (+ (or interval 0) note-offset)))
	  (inside object)
	  (loffset object)))

(defun jack-player-play-note (object interval)
  (let ((start (/ interval 1000.0))
	(dur (/ (get-obj-dur object) 1000.0)) 
	(noteno (/ (midic object) 100))
	(vel (vel object))
	(chan (chan object)))
    (cl-jack::jack-midi-play-event start dur noteno vel chan)))

(defmethod player-play-object ((engine (eql :jackmidi)) (object rest) &key interval)
  nil)

(defmethod player-play-object ((engine (eql :jackmidi)) (object continuation-chord) &key interval)
  nil)

(defmethod player-play-object ((engine (eql :jackmidi)) (object note) &key interval)
  (jack-player-play-note object interval))

;;; START (PLAY WHAT IS SCHEDULED)
(defmethod player-start ((engine (eql :jackmidi)) &optional play-list)
  (setf cl-jack::*playing* t)
  ;;(when *jackplayer* (jack-midi-start-player *jackplayer*))
  (call-next-method))

;;; PAUSE (all)
(defmethod player-pause ((engine (eql :jackmidi)) &optional play-list)
  (print (list 'pause play-list))
  (call-next-method))

;;; CONTINUE (all)
(defmethod player-continue ((engine (eql :jackmidi)) &optional play-list)
  (print 'continue play-list)
  'update-queue-time-from-framenow
  ;;(when *jackplayer* (jack-midi-cont-player *jackplayer*))
  (call-next-method))

;;; STOP (all)
(defmethod player-stop ((engine (eql :jackmidi)) &optional play-list)
  (cl-jack::jack-midi-stop-all)
  (call-next-method))

;;(player-stop :jackmidi)
