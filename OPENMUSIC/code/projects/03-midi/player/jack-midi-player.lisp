;;; ===========================================================================
;;; JACK Player class for OM.
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

;;; JACK MIDI PLAYER                  
(defmethod player-name ((player (eql :jackmidi))) "Jack midi player")   
(defmethod player-desc ((player (eql :jackmidi))) "(default)")   
(defmethod player-special-action ((player (eql :jackmidi))) nil)  
(defmethod player-params ((player (eql :jackmidi))) nil)   
(defmethod player-type ((player (eql :jackmidi))) :midi)

(progn
  (pushnew :jackmidi *all-players*)
  (pushnew :jackmidi *enabled-players*)
  (enable-player :jackmidi))

#+linux (mapc #'(lambda (pl) (disable-player pl))
	      '(:microplayer :libaudiostream :multiplayer :midishare))



;; setup jackmidi as option for relevant classes

(let* ((curlist (players-for-object (make-instance 'simple-score-element)))
       (newlist (pushnew :jackmidi curlist)))
  (defmethod players-for-object ((self simple-score-element)) newlist))

(let* ((curlist (players-for-object (make-instance 'score-element)))
       (newlist (pushnew :jackmidi curlist)))
  (defmethod players-for-object ((self score-element)) newlist))

(add-player-for-object score-element :jackmidi)
(add-player-for-object simple-score-element :jackmidi)

;; hook into global pool of seqs for running cl-jack-client

(defparameter *jack-midi-seqs* cl-jack::*jack-seqs*) 

(defun jack-possibly-init-queue-for-this-player (queue)
  (or (gethash queue *jack-midi-seqs*)
      (setf (gethash queue *jack-midi-seqs*) (cl-jack::make-jack-seq))))

(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) object at interval)
  (let (;;(thisqueue (first (get-my-play-list engine (play-list player))))
	(newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at)))
    (jack-possibly-init-queue-for-this-player object) 
    (print (list 'object-at-start object))
    (jack-send-to-jack object at interval object)

    ;;(call-next-method)
    ))

(defun jack-send-to-jack (object at interval queue)
  ;;(print (list object at interval queue))
  (cond ((container-p object)
	 (mapc #'(lambda (sub)
		   (jack-send-to-jack sub (+ at (offset->ms sub)) interval queue))
	       (inside object)))
	((rest-p object) nil)
	((note-p object)
	 ;; send off events to jacks scheduler
	 (unless (equal (tie object) 'end)
	   (let* ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object)))))
		  (interval-at (if interval (- at (car interval)) at)))
	     (when (or (not interval) note-in-interval?)
	       (jack-player-play-note queue object interval-at)))))
	(t (error "fixme: :jackmidi, dont know how to play ~A" object)))
  )



(defun jack-player-play-note (queue object offset)
  (let ((seq (gethash queue *jack-midi-seqs*))
	(start (/ offset 1000.0))
	(dur (/ (real-dur object) 1000.0)) 
	(noteno (/ (midic object) 100))
	(vel (vel object))
	(chan (chan object)))
    (cl-jack::jack-play-event seq start dur noteno vel chan) 
    )) 

(defun jack-kill-queue (obj)
  ;;(print (list obj (gethash obj *jack-midi-seqs*)))
  (when (gethash obj *jack-midi-seqs*)
    (cl-jack::jack-all-notes-off-and-kill-seq (gethash obj *jack-midi-seqs*))))

(defmethod player-start ((engine (eql :jackmidi)) &optional play-list)
  ;;(print (list 'hiho play-list))
  t
  )

(defmethod player-stop ((engine (eql :jackmidi)) &optional play-list)
  ;;(print (list 'object-at-end (first (first play-list))))
  (mapc #'(lambda (item) (jack-kill-queue (if (listp item) (first item) item)))
	play-list))

(defmethod player-pause ((engine (eql :jackmidi)) &optional play-list)
  (print (format nil "~A : pause" engine))
  (call-next-method))

(defmethod player-continue ((engine (eql :jackmidi)) &optional play-list)
  (print (format nil "~A : continue" engine))
  (call-next-method))

;; (defmethod player-set-loop ((engine (eql :jackmidi)) &optional start end)
;;   (print (format nil " player-set-loop: ~A ~A" start end))
;;   t)

(defmethod player-loop ((engine (eql :jackmidi)) &optional play-list)
  ;;(print (format nil "~A : loop, plls: ~A" engine play-list))
  (loop for item in play-list
       do (jack-send-to-jack (first item) 0 (second item) (first item)))
  t)

(setf *midiplayer* t)
