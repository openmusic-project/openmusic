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

(defun init-jack-midi-player ()
  ;; (pushnew :jackmidi *all-players*)
  ;; (pushnew :jackmidi *enabled-players*)
  (enable-player :jackmidi)
  (add-player-for-object 'score-element :jackmidi)
  (add-player-for-object 'simple-score-element :jackmidi)
  (add-player-for-object 'eventmidi-seq :jackmidi)
  (add-player-for-object 'midifile :jackmidi)
  )

(defmethod get-edit-param ((box ommidifilebox) (param (eql 'player))) :fluidsynth)


(om-add-init-func 'init-jack-midi-player)

#+linux (mapc #'(lambda (pl) (disable-player pl))
	      '(:microplayer :libaudiostream :multiplayer :midishare))



;; setup jackmidi as option for relevant classes

;; (let* ((curlist (players-for-object (make-instance 'simple-score-element)))
;;        (newlist (pushnew :jackmidi curlist)))
;;   (defmethod players-for-object ((self simple-score-element)) newlist))

;; (let* ((curlist (players-for-object (make-instance 'score-element)))
;;        (newlist (pushnew :jackmidi curlist)))
;;   (defmethod players-for-object ((self score-element)) newlist))

;; hook into global pool of seqs for running cl-jack-client



(defparameter *jack-midi-seqs* cl-jack::*jack-seqs*)
(defparameter *jack-use-om-scheduler* nil)

(defun jack-possibly-init-queue-for-this-player (queue)
  (or (gethash queue *jack-midi-seqs*)
      (and (setf (gethash queue *jack-midi-seqs*) (cl-jack::make-jack-seq))
	   ;;(print (format nil "setup new seq for: ~A" queue))
	   queue)))

(defun jack-player-play-note (queue note offset &optional approx)
  ;;(print (list 'jack-player-play-note queue note offset))
  (let* ((seq (gethash queue *jack-midi-seqs*))
	 (start (/ offset 1000.0))
	 (dur (/ (real-dur note) 1000.0))
	 (tempered-midic (approx-m (midic note) (or approx 2)))
	 (noteno (floor tempered-midic 100))
	 (vel (vel note))
	 (chan (1- (if (and approx *midi-microplay* (member approx '(4 8)))
		       (+ (chan note) (micro-channel tempered-midic approx))
		       (chan note)))))
    ;; (print (list start chan (chan note) (midic note) tempered-midic noteno approx))
    (cl-jack::jack-play-note seq start dur noteno vel chan)))

(defun jack-player-play-midievent (queue event offset)
  ;;(print (list 'jack-player-play-event queue event offset))
  (let ((seq (gethash queue *jack-midi-seqs*)))
    (cl-jack::jack-play-event seq offset event)))


(defmacro jack-first-n (elms n siz) `(butlast ,elms (- ,siz ,n)))
(defmacro jack-after-n (elms n) `(nthcdr ,n ,elms))

;; this works for very long 'sequences', ie: chord-seqs or lists.  Need a better way to do hierarchic structures,
;; ie. voice, poly, maquette?

(defun jack-send-first-and-rest (elms split nelems at interval queue &optional approx)
  ;; send off first elements, to get on with playing:
  (mapc #'(lambda (sub)
	    (jack-send-to-jack sub (+ at (offset->ms sub)) interval queue approx))
	(jack-first-n elms split nelems))
  ;; send rest when time permits
  (mp::process-run-function "sending rest of elems" nil
			    #'(lambda ()
				(mapc #'(lambda (sub)
					  (jack-send-to-jack sub (+ at (offset->ms sub)) interval queue approx))
				      (jack-after-n elms split)))))

(defun jack-send-container (object at interval queue approx)
  (let* ((elms (inside object))
	 (nelems (length elms))
	 (split 200))
    (if (> nelems split)
	(jack-send-first-and-rest elms split nelems at interval queue approx)
	(mapc #'(lambda (sub)
		  (jack-send-to-jack sub (+ at (offset->ms sub)) interval queue approx))
	      (inside object)))))

(defun jack-send-to-jack (object at interval queue &optional approx)
  (cond ((container-p object) (jack-send-container object at interval queue approx))
	((rest-p object) nil)
	((note-p object)
	 ;; send off events to jacks scheduler
	 (unless (member (tie object) '(end continue))
	   (let* ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object)))))
		  (interval-at (if interval (- at (car interval)) at)))
	     (when (or (not interval) note-in-interval?)
	       (jack-player-play-note queue object interval-at approx)))))
	(t (error "fixme: :jackmidi, dont know how to play ~A" object))))


(defun jack-send-events-to-jack (object at interval queue)
  (cond ((container-p object) (jack-send-container object at interval queue))
	((typep object 'midifile) nil)
	((rest-p object) nil)
	 ;; send off events to jacks scheduler
	((note-p object)
	 (unless (member (tie object) '(end continue))
	   (let* ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object)))))
		  (interval-at (if interval (- at (car interval)) at)))
	     (when (or (not interval) note-in-interval?)
	       (jack-player-play-note queue object interval-at)))))
	((midievent-p object)
	 (let* ((event-in-interval? (point-in-interval at interval))
		(interval-at (if interval (- at (car interval)) at)))
	   (when (or (not interval) note-in-interval?)
	     (jack-player-play-event queue object interval-at))))
	(t (error "fixme: :jackmidi, dont know how to play ~A" object))))

(defmethod player-schedule :around ((player omplayer) obj (engine (eql :jackmidi)) &key (at 0) interval)
  (declare (ignore interval))
  (when (equal (state player) :play)	;extend stop-time if obj scheduled after start
    (setf (stop-time player) (+ at (cadr (play-interval player))))
    (player-start engine (list obj)))
  (call-next-method))


(defun jack-setup-microplay-chans ()
;;; make or send ... ?
  (pitchwheel '(0 1024 2048 3072) '(1 2 3 4)))

(defmethod prepare-to-play ((engine (eql :jackmidi)) (player omplayer) (object simple-container) at interval params)
  (let ((approx (if (caller player)
		    (get-edit-param (caller player) 'approx)
		    *global-midi-approx*)))
    (if (not *jack-use-om-scheduler*)
	(progn
	  (jack-possibly-init-queue-for-this-player object)
	  (when (and (/= approx 2) *midi-microplay*)  (jack-setup-microplay-chans))
	  (jack-send-to-jack object at interval object approx))
	(call-next-method))))

(defmethod player-start ((engine (eql :jackmidi)) &optional play-list)
  (mapc #'(lambda (obj)
	    (jack-possibly-init-queue-for-this-player obj))
	play-list))

(defmethod player-loop ((engine (eql :jackmidi)) player &optional play-list)
  (mapc #'(lambda (item)
	    (let ((interval (play-interval player)))
              (if *jack-use-om-scheduler*
                  (prepare-to-play engine player item 0 interval)
		  (jack-send-to-jack item 0 interval item)
		  )))
	play-list))

(defmethod player-play-object ((engine (eql :jackmidi)) (object container) &key interval)
  (if (not *jack-use-om-scheduler*)
      t
      (mapc #'(lambda (sub)
		(player-play-object engine sub :interval interval))
	    (inside object))))

(defmethod player-play-object ((engine (eql :jackmidi)) (object note) &key interval)
  (if (not *jack-use-om-scheduler*)
      t
      (jack-player-play-note-now-in-global-seq object interval)))

(defun jack-player-play-note-now-in-global-seq (object at)
  (let ((seq cl-jack::*jack-seq*)
	(start at)
	(dur (/ (real-dur object) 1000.0))
	(noteno (round (midic object) 100))
	(vel (vel object))
	(chan (1- (chan object))))
    (cl-jack::jack-play-note seq start dur noteno vel chan)))

(defun jack-player-send-evt-now-in-global-seq (event)
  (let ((seq cl-jack::*jack-seq*)
	(start 0))
    (cl-jack::jack-play-event seq start event)))

(defun jack-kill-queue (obj)
  (when (gethash obj *jack-midi-seqs*)
    (cl-jack::jack-all-notes-off-and-kill-seq (gethash obj *jack-midi-seqs*))))


(defmethod player-stop ((engine (eql :jackmidi)) &optional play-list)
  (if (not *jack-use-om-scheduler*)
      (mapc #'(lambda (item)
		(jack-kill-queue (if (listp item) (first item) item)))
	    play-list)
      (call-next-method)))

(defmethod player-pause ((engine (eql :jackmidi)) &optional play-list)
  (print (list 'player-pause play-list))
  (if (not *jack-use-om-scheduler*)
      t
      ;;(setf cl-jack::*playing* nil)
      (call-next-method)))

(defmethod player-continue ((engine (eql :jackmidi)) &optional play-list)
  (if (not *jack-use-om-scheduler*)
      t
      ;;(setf cl-jack::*playing* t)
      (call-next-method)))

#|

(setf *jack-use-om-scheduler* nil)
(setf *jack-use-om-scheduler* t)
;; this is used if *jack-use-om-scheduler* is t



(cl-jack::jack-play-note cl-jack::*jack-seq* 0 1 60 100 1)

(clrhash cl-jack::*jack-seq*)

|#

(setf *midiplayer* t)


;; general function om-midi-send-evt expects instances of midimsg2evt:

#|
(defun om-midi-send-evt (event &optional (player *midiplayer*))
  (declare (ignore player))
  (let ((time (cl-jack::jack-frame-now))
	(seq cl-jack::*jack-seq*))
    (case (om-midi::midi-evt-type event)
      (5 (cl-jack::seqhash-midi-program-change seq time (oa::event-pgm event) (1+ (oa::event-chan event))))
      (7 (cl-jack::seqhash-midi-pitch-wheel-msg seq time (oa::event-bend event) (1+ (oa::event-chan event))))
      (4 (cl-jack::seqhash-midi-control-change seq time (oa::event-ctrl event) (oa::event-val event) (1+ (oa::event-chan event))))
      (t (print (om-midi::midi-evt-type event))))
    (print event)))
|#

;; dont know if this first one is needed, leave until midi-system stabilises 
(defmethod om-midi::send-midi-event-function ((midisystem (eql :cl-midi))) 'om::jack-midi-send-evt)
(defmethod om-midi::send-midi-event-function ((midisystem (eql :cl-jack))) 'om::jack-midi-send-evt)

(defun jack-midi-send-evt (event &optional player)
  (declare (ignore player))
  (let ((time (cl-jack::jack-frame-now))
	(seq cl-jack::*jack-seq*))
    (case (om-midi::midi-evt-type event)
      (:ProgChange (cl-jack::seqhash-midi-program-change seq time (car (om-midi::midi-evt-fields event)) (1- (om-midi::midi-evt-chan event))))
      (:PitchBend (cl-jack::seqhash-midi-pitch-wheel-msg seq time
      							(round (om-midi::midi-evt-fields event))
      							(1- (om-midi::midi-evt-chan event))))
      (:PitchWheel (cl-jack::seqhash-midi-pitch-wheel-msg seq time (car (om-midi::midi-evt-fields event)) (1- (om-midi::midi-evt-chan event))))
      (:CtrlChange (cl-jack::seqhash-midi-control-change seq time
							(cadr (om-midi::midi-evt-fields event))
							(car (om-midi::midi-evt-fields event))
							(1- (om-midi::midi-evt-chan event))))
      
      (t (print (list 'event-type (om-midi::midi-evt-type event) (om-midi::midi-evt-fields event)))))
    ;;(print event)
    ))

(setf (symbol-function 'om-midi::cl-midi-send-evt) (symbol-function 'jack-midi-send-evt))

;; (position "ProgChange" *midi-event-types* :test #'subseq :key #'car)

