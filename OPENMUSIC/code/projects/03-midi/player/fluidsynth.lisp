(in-package :om)

;; various specializing to play midifile and midievents with fluidsynth:

(defmethod inside ((self midifile))
  (oa::midi-seq-events (fileseq self)))

;;; FIXME; get proper offset
(defmethod offset->ms ((self oa::cl-midievent) &optional grandparent)
  (oa::event-date self))

(defmethod prepare-to-play ((engine (eql :fluidsynth)) (player omplayer) (object midifile) at interval)
  (let ((note-in-interval? (interval-intersec interval (list at (+ at (real-dur object))))))
    (when (or (not interval) note-in-interval?)
      (mapc #'(lambda (sub)
		(prepare-to-play engine player sub (+ at (offset->ms sub)) interval))
	    (inside object)))))

;; play event using OMs own scheduler
(defun play-cl-midievent (event)
  (when (= (oa::event-type event) (oa::om-midi-get-num-from-type "Note"))
    (let ((chan (oa::event-chan event))	
	  (key (oa::event-pitch event))
	  (dur (/ (oa::event-dur event) 1000.0))
	  (vel (oa::event-velocity event)))
      (mp:process-run-function "play fluid event" ()
			       #'(lambda ()
				   (cl-fluidsynth::fluid_synth_noteon *fluidplayer-synth* chan key vel)
				   (sleep dur)
				   (cl-fluidsynth::fluid_synth_noteoff *fluidplayer-synth* chan key))))))

(defmethod player-play-object ((engine (eql :fluidsynth)) (object oa::cl-midievent) &key interval)
  (when (or (not interval) (point-in-interval (offset->ms object) interval))
    (play-cl-midievent object)))

;; 'edition-params is looked up by new-player, and seems to be
;; defaulted to :midishare somewhere:

(defmethod get-edit-param ((box ommidifilebox) (param (eql 'player))) 
  :fluidsynth)


;; (defmethod get-player-engine ((self editorview)) (get-edit-param self 'player))
;; (defmethod get-player-engine ((self play-editor-mixin)) t)

;; (defmethod set-edit-param ((self EditorView) param val) 
;;    (if (and (editor-edit-params self) (assoc param (editor-edit-params self)))
;;      (rplacd (assoc param (editor-edit-params self)) val)
;;      (when (edition-params (ref self))
;;        (push (cons param val) (edition-params (ref self))))))


(setf *midiplayer* t)
