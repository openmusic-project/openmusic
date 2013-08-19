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

(progn
  (pushnew :jackmidi *all-players*)
  (enable-player :jackmidi))

#+linux (mapc #'(lambda (pl) (disable-player pl))
	      '(:microplayer :libaudiostream :multiplayer :midishare))

;; redefined from various places here
(defmethod players-for-object ((self score-element)) '(:jackmidi))
(defmethod players-for-object ((self simple-score-element)) '(:jackmidi))

;; (players-for-object (make-instance 'sound))
;; (enabled-players-for-object (make-instance 'note))

;; hook into global pool of seqs for running cl-jack-client

(defparameter *jack-midi-seqs* cl-jack::*jack-seqs*) 

(defun jack-possibly-init-queue-for-this-player (queue)
  (or (gethash queue *jack-midi-seqs*)
      (setf (gethash queue *jack-midi-seqs*) (cl-jack::make-jack-seq))))

(defun jack-kill-queue (obj)
  (when (gethash obj *jack-midi-seqs*)
    (cl-jack::jack-all-notes-off-and-kill-seq (gethash obj *jack-midi-seqs*))))

(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) object at interval)
  (let ((thisqueue (first (get-my-play-list engine (play-list player))))
	(newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at)))
    ;;(print (list interval newinterval))
    (jack-possibly-init-queue-for-this-player thisqueue)
    (cond ((container-p object)
	   (mapc #'(lambda (sub)
		     (prepare-to-play engine player sub (+ at (offset sub)) interval))
		 (inside object)))
	  ((rest-p object) nil)
	  (t (let* ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object)))))
		    (interval-at (if interval (- at (car interval)) at)))
	       (when (or (not interval ) note-in-interval?)
		 (jack-player-play-note thisqueue object interval-at)))))
    t))


#|


(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) (object group) at interval)
  ;; (print (mapcar #'(lambda (sub)
  ;; 		     (+ at (offset->ms sub)))
  ;; 		 (inside object)))
  ;;(break)
  (print (list (get-obj-dur object) (tree object)
	       (tree2onsets (tree object))
	       (mapcar #'(lambda (dur) (round (* 1000 dur (/ (qtempo object) 60))))
		       (tree2onsets (tree object)))))
  (call-next-method))

(group)
(tree2onsets '((4 4) ((1 (1 1 1 1 1 1 1 1)) (1 (1 1 1 1 1)) (1 (1 3 3)) (1 (4.0 -3)))))
(tree2ratio '(? ((1 1 1 1))))

|#

(defun jack-player-play-note (queue object offset)
  (let ((seq (gethash queue *jack-midi-seqs*))
	(start (/ offset 1000.0))
	(dur (/ (get-obj-dur object) 1000.0)) 
	(noteno (/ (midic object) 100))
	(vel (vel object))
	(chan (chan object)))
    (cl-jack::jack-play-event seq start dur noteno vel chan) 
    )) 

(defmethod player-stop ((self (eql :jackmidi)) &optional play-list)
  (jack-kill-queue (first play-list))
  (call-next-method))

;; (defmethod player-stop :after ((self (eql :jackmidi)) &optional play-list)
;;    (jack-kill-queue (first play-list)))


#|

(container-p (make-instance 'measure))

(mapcar #'(lambda (x) (class-name x)) (class-direct-superclasses (class-of (make-instance 'note))) )
(mapcar #'(lambda (x) (class-name x)) (class-direct-superclasses (class-of (make-instance 'sequence*))) )

(mapcar #'(lambda (cl )
	    (list cl
		  (mapcar #'(lambda (x) (subtypep cl x))
			  '(sequence* container simple-container))))
	'(note chord chord-seq measure group voice poly multi-seq))

(prepare-to-play :jackmidi (make-instance 'omplayer) (make-instance 'chord-seq) 0 nil)
(prepare-to-play :jackmidi (make-instance 'omplayer) (make-instance 'chord)  0 nil)

|#

#|


(defmethod player-play-object ((engine (eql :jackmidi)) object &key interval)
  (call-next-method))

(defmethod player-play-object ((engine (eql :jackmidi)) (object simple-container) &key interval)
  (mapc #'(lambda (obj)
	    (player-play-object :jackmidi obj :interval interval))
	(inside object)))


(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) (object chord-seq) at interval)
  (print (list 'prepare-to-play (inside object) at))
  (jack-init-queue-for-this-object object at interval)
  (print (list 'prepare-to-play object))
  (call-next-method)
  )

(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) (object multi-seq) at interval)
  (print (list 'prepare-to-play (inside object) at))
  (jack-init-queue-for-this-object object at interval)
  (print (list 'prepare-to-play object))
  (call-next-method)
  )

(defmethod player-play-object ((engine (eql :jackmidi)) (object poly) &key interval)
  ;;(break)
  ;;(print (list 'player-play-object object (inside object)))
  (mapc #'(lambda (sub)
	      (player-play-object engine sub :interval interval))
	  (inside object)))

(defmethod player-play-object ((engine (eql :jackmidi)) (object voice) &key interval)
  ;;(print (list 'player-play-object object (inside object) 'interval interval))
  ;;(print (list 'interval interval))
  (mapc #'(lambda (sub)
	      ;;(print (list 'offset->ms (offset->ms sub) sub '(offset object) (offset object)))
  	      (player-play-object engine sub :interval (+ (offset object) (offset->ms sub))))
  	  (inside object)))

(defmethod player-play-object ((engine (eql :jackmidi)) (object measure) &key interval)
  (mapc #'(lambda (sub)
  	      (player-play-object engine sub :interval (+ (offset object) 
							  (offset->ms sub)
							  )))
  	  (inside object))
  ;;(call-next-method)
  )

(defmethod player-play-object ((engine (eql :jackmidi)) (object group) &key interval)
  ;;(print (list 'player-play-object object (inside object)))
  (mapc #'(lambda (sub)
  	      (player-play-object engine sub :interval (+ (or interval 0) (offset object) (offset->ms sub))))
  	  (inside object))
  ;;(call-next-method)
  )


(defmethod player-play-object ((engine (eql :jackmidi)) (object chord-seq) &key interval)
  (mapc #'(lambda (chord chord-onset)
	    (if interval
		(progn
		  (print (list 'interval chord-onset (interval-intersec interval 
							    (list chord-onset
								  (+ chord-onset (get-obj-dur chord))))))
		  (when (interval-intersec interval 
					   (list chord-onset (+ chord-onset (get-obj-dur chord))))
		    (player-play-object engine chord :interval interval)))
		(player-play-object engine chord :interval chord-onset)))
	(inside object)
	(lonset object)))



(defmethod player-play-object ((engine (eql :jackmidi)) (object chord) &key interval)
  (let ((at (offset object)))
    (mapc #'(lambda (obj note-offset)
	      (print (list 'chord at note-offset interval))
	      (player-play-object engine obj :interval (+ at note-offset)))
	  (inside object)
	  (loffset object))))



(defmethod player-play-object ((engine (eql :jackmidi)) (object note) &key interval)
  (jack-player-play-note object interval))

(defmethod player-play-object ((engine (eql :jackmidi)) (object rest) &key interval)
  nil)

(defmethod player-play-object ((engine (eql :jackmidi)) (object continuation-chord) &key interval)
  nil)

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
  (print (list 'continue play-list))
  'update-queue-time-from-framenow
  ;;(when *jackplayer* (jack-midi-cont-player *jackplayer*))
  (call-next-method))

;;; STOP (all)
(defmethod player-stop ((engine (eql :jackmidi)) &optional play-list)
  (cl-jack::jack-midi-stop-all)
  (call-next-method))

;;(player-stop :jackmidi)
|#
