(in-package :om)

; (defparameter *refnum* (ms::midiopen "Common Lisp"))
; (ms::MidiClose *refnum*)

(enable-player :midi-player)

;;; NEW MIDI PLAYER (NOT YET AVAILABLE)                 
(defmethod player-name ((player (eql :midi-player))) "OM MIDI player")   ;;; A short name
(defmethod player-desc ((player (eql :midi-player))) "Uses the default MIDI system to send MIDI events in real time")   ;;; a description
(defmethod player-special-action ((player (eql :midi-player))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :midi-player))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :midi-player))) :midi)   ;;; communication protocol (:midi / :udp)


    
(defmethod prepare-to-play ((engine (eql :midi-player)) (player omplayer) object at interval params)
  (print "MIDI")
  ;(print (format nil "~s" params))
  (let ((approx (if (find :approx params)
                    (nth (1+ (position :approx params)) params)
                  (if (caller player) (get-edit-param (caller player) 'approx))))
        (port (if (find :port params)
                    (nth (1+ (position :port params)) params)
                (if (caller player) (get-edit-param (caller player) 'outport)))))
    ;(print params)
    (if (equal port :default) (setf port *def-midi-out*))
    (mapcar #'(lambda (evt) 
              ;  (call-next-method engine player evt (+ (or (car interval) 0) (om-midi::midi-evt-date evt)) interval params)
                (schedule-task player 
                               #'(lambda () 
                                   ;(print evt)
                                   (player-play-object engine evt :interval interval))
                               (+ (or (car interval) 0) (om-midi::midi-evt-date evt))
                               nil)
                )

                ;(remove-if #'(lambda (evt) (or (null evt) (and interval (or (< (om-midi::midi-evt-date evt) (car interval))
                ;                                                           (> (om-midi::midi-evt-date evt) (cadr interval))))))
            (remove nil (flat (PrepareToPlay :midi object at :interval interval :approx approx :port port)))
            )
    (sort-events player)
    ))

(defmethod player-start ((engine (eql :midi-player)) &optional play-list)
  (midi-start))

(defmethod player-stop ((engine (eql :midi-player)) &optional play-list)
  (loop for ch in *key-ons*
        for c = 1 then (+ c 1) do
        (loop for note in ch do
              (midi-send-evt 
               (om-midi:make-midi-evt :type :keyOff
                                      :chan c :date 0 :ref 0 :port (car note)
                                      :fields (list (cadr note) 0))
               ))
        )
  (midi-stop)
  (setf *key-ons* (make-list 16)))

(defmethod player-loop ((self (eql :midi-player)) player &optional play-list)
  (declare (ignore player))
  (loop for obj in play-list do
        (prepare-to-play  self player obj 0 (play-interval player) nil)))

(defparameter *key-ons* (make-list 16))

;;; PLAY (NOW) 
(defmethod player-play-object ((engine (eql :midi-player)) (object om-midi::midi-evt) &key interval params)
  ;(print (format nil "~A : play ~A - ~A" engine object interval))
  ;(print object)
  (cond 
   ((or (equal (om-midi::midi-evt-type object) :keyOff)
        (and (equal (om-midi::midi-evt-type object) :keyOn) (= 0 (cadr (om-midi::midi-evt-fields object)))))
    (setf (nth (1- (om-midi::midi-evt-chan object)) *key-ons*) 
          (delete (list (om-midi::midi-evt-port object) (car (om-midi::midi-evt-fields object)))
                  (nth (1- (om-midi::midi-evt-chan object)) *key-ons*)
                  :test 'equal)))
   ((equal (om-midi::midi-evt-type object) :keyOn)
    (pushnew (list (om-midi::midi-evt-port object) (car (om-midi::midi-evt-fields object))) (nth (1- (om-midi::midi-evt-chan object)) *key-ons*) :test 'equal))
   
   )
  ;(print *key-ons*)
  (midi-send-evt object)
  )








