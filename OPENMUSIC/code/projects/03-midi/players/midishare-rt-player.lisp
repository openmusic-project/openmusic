(in-package :om)

; (defparameter *refnum* (ms::midiopen "Common Lisp"))
; (ms::MidiClose *refnum*)

(enable-player :midishare-rt)

;;; NEW MIDI PLAYER (NOT YET AVAILABLE)                 
(defmethod player-name ((player (eql :midishare-rt))) "MidiShare RT")   ;;; A short name
(defmethod player-desc ((player (eql :midishare-rt))) "experimental real-time MIDI player")   ;;; a description
(defmethod player-special-action ((player (eql :midishare-rt))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :midishare-rt))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :midishare-rt))) :midi)   ;;; communication protocol (:midi / :udp)


(defmethod prepare-to-play ((engine (eql :midishare-rt)) (player omplayer) object at interval)
  (let ((approx (if (caller player) (get-edit-param (caller player) 'approx))))
    (mapcar #'(lambda (evt) (call-next-method engine player evt (midi-evt-date evt) interval))
            (remove-if #'(lambda (evt) (or (null evt) (and interval (or (< (midi-evt-date evt) (car interval))
                                                                        (> (midi-evt-date evt) (cadr interval))))))
                       (flat (PrepareToPlay :midi object at :interval interval))))))

;;; PLAY (NOW) 
;;; NOT CALLED WITH MS PLAYER 
(defmethod player-play-object ((engine (eql :midishare-rt)) object &key interval)
  ;(print (format nil "~A : play ~A - ~A" engine object interval))
  (om-midi::midishare-send-evt object))





