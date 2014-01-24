(in-package :om)

; (defparameter *refnum* (ms::midiopen "Common Lisp"))
; (ms::MidiClose *refnum*)

(enable-player :midishare-rt)

;;; NEW MIDI PLAYER (NOT YET AVAILABLE)                 
(defmethod player-name ((player (eql :midishare-rt))) "MidiShare RT")   ;;; A short name
(defmethod player-desc ((player (eql :midishare-rt))) "Uses the MidiShare interface and the OM scheduler")   ;;; a description
(defmethod player-special-action ((player (eql :midishare-rt))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :midishare-rt))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :midishare-rt))) :midi)   ;;; communication protocol (:midi / :udp)


(defmethod prepare-to-play ((engine (eql :midishare-rt)) (player omplayer) object at interval params)
  (let ((approx (if (caller player) (get-edit-param (caller player) 'approx))))
    (mapcar #'(lambda (evt) 
                ;(print (list "play note at" (om-midi::midi-evt-date evt)))
                (call-next-method engine player evt (+ (or (car interval) 0) (om-midi::midi-evt-date evt)) interval params))
            ;(remove-if #'(lambda (evt) (or (null evt) (and interval (or (< (om-midi::midi-evt-date evt) (car interval))
            ;                                                           (> (om-midi::midi-evt-date evt) (cadr interval))))))
            (remove nil 
                    (flat (PrepareToPlay :midi object at :interval interval))
                    )
            )
    ))


(defmethod player-stop ((engine (eql :midishare-rt)) &optional play-list)
  (om-midi::midishare-stop))

(defmethod player-loop ((self (eql :midishare-rt)) player &optional play-list)
  (declare (ignore player))
  (loop for obj in play-list do
        (prepare-to-play self player obj 0 (play-interval player))))

;;; PLAY (NOW) 
;;; NOT CALLED WITH MS PLAYER 
(defmethod player-play-object ((engine (eql :midishare-rt)) (object om-midi::midi-evt) &key interval params)
  ;(print (format nil "~A : play ~A - ~A" engine object interval))
  (om-midi::midishare-send-evt object)
  )





