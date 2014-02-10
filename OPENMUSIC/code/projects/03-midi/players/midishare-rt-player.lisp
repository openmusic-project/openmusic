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
  (let ((approx (if (caller player) (get-edit-param (caller player) 'approx)))
        (port (if (caller player) (get-edit-param (caller player) 'outport))))
    (if (equal port :default) (setf port *def-midi-out*))
    (mapcar #'(lambda (evt) 
              ;  (call-next-method engine player evt (+ (or (car interval) 0) (om-midi::midi-evt-date evt)) interval params)
                (schedule-task player 
                               #'(lambda () 
                                   (player-play-object engine evt :interval interval :params params))
                               (+ (or (car interval) 0) (om-midi::midi-evt-date evt))
                               nil)
                )

                ;(remove-if #'(lambda (evt) (or (null evt) (and interval (or (< (om-midi::midi-evt-date evt) (car interval))
                ;                                                           (> (om-midi::midi-evt-date evt) (cadr interval))))))
            (remove nil (flat (PrepareToPlay :midi object at :interval interval :approx approx :port port)))
            )
    (sort-events player)
    ))


(defmethod player-stop ((engine (eql :midishare-rt)) &optional play-list)
  (om-midi::midishare-stop)
  (loop for ch in *key-ons* 
        for c = 1 then (+ c 1) do
        (loop for note in ch do
              (om-midi::midishare-send-evt 
               (om-midi:make-midi-evt :type 'om-midi::keyOff
                                      :chan c :date 0 :ref 0 :port (car note)
                                      :fields (list (cadr note) 0))
               ))
        )
  (setf *key-ons* (make-list 16)))

(defmethod player-loop ((self (eql :midishare-rt)) player &optional play-list)
  (declare (ignore player))
  (loop for obj in play-list do
        (prepare-to-play  self player obj 0 (play-interval player) nil)))

(defparameter *key-ons* (make-list 16))

;;; PLAY (NOW) 
(defmethod player-play-object ((engine (eql :midishare-rt)) (object om-midi::midi-evt) &key interval params)
  ;(print (format nil "~A : play ~A - ~A" engine object interval))
  ;(print object)
  (cond 
   ((or (equal (om-midi::midi-evt-type object) 'om-midi::keyOff)
        (and (equal (om-midi::midi-evt-type object) 'om-midi::keyOn) (= 0 (cadr (om-midi::midi-evt-fields object)))))
    (setf (nth (1- (om-midi::midi-evt-chan object)) *key-ons*) 
          (delete (list (om-midi::midi-evt-port object) (car (om-midi::midi-evt-fields object)))
                  (nth (1- (om-midi::midi-evt-chan object)) *key-ons*)
                  :test 'equal)))
   ((equal (om-midi::midi-evt-type object) 'om-midi::keyOn)
    (pushnew (list (om-midi::midi-evt-port object) (car (om-midi::midi-evt-fields object))) (nth (1- (om-midi::midi-evt-chan object)) *key-ons*) :test 'equal))
   
   )
  ;(print *key-ons*)
  (om-midi::midishare-send-evt object)
  )





#|
(setq *ms-player* (ms::openplayer "MSPLAYER"))
(midishare::MidiConnect *ms-player* 0 -1)

(om-midi::make-midi-evt :type 'Start :port (or port *def-midi-out*) :date 0)

(let ((ev1 (midishare::MidiNewEv ms::typeStart)))
  (ms::MidiSendIm *ms-player* ev1))

(let ((ev1 (midishare::MidiNewEv ms::typeStop)))
  (ms::MidiSendIm *ms-player* ev1))

;;; CHANGE PROGRAM ON CANAL 0
(let ((ev1 (midishare::MidiNewEv ms::typeProgChange)))
  (midishare::chan ev1 0)
  (midishare::pgm ev1 5)
  (ms::MidiSendIm *ms-player* ev1))

;;; SEND NOTE ON (never terminates)
(let ((noteon (midishare::MidiNewEv ms::typeKeyOn)))
  (midishare::chan noteon 0)
  (midishare::pitch noteon 60)
  (midishare::vel noteon 100)
  (ms::MidiSendIm *ms-player* noteon)
  )

(ms::startplayer *ms-player*)
(ms::stopplayer *ms-player*)
(midishare::PausePlayer *ms-player*)

;;; DEVRAIT ARRETER LA NOTE.. ?
(let ((ev2 (midishare::MidiNewEv ms::typeCtrlChange)))
    (midishare::chan ev2 0)
    (midishare::ctrl ev2 120)
    (midishare::val ev2 0)
    (ms::MidiSendIm *ms-player* ev2))


(let ((evt (midishare::MidiNewEv ms::typeKeyOff)))
    (midishare::chan evt 0)
    (midishare::pitch evt 60)
    (midishare::vel evt 100)
  (ms::MidiSendIm *ms-player* evt)
  )

(let ((evt (midishare::MidiNewEv ms::typeCtrlChange)))
    (midishare::chan evt 0)
    (midishare::ctrl evt 125)
  (ms::MidiSendIm 1 evt)
  )


(midishare::StopPlayer *ms-player*)

|#





