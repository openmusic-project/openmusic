(in-package :om)

;===================
;==== MIDI PLAY ====
;===================

;;; redefinition : préparer la séquence MIDI
(defmethod InitPlayingSeq ((player (eql 'midishare)) dur &key (port nil))
  (when *midiplayer*
    (let ((midiport (or port *Outmidiport*))
          startevent tempoevent)
      (setf *playing-midi-seq* (om-midi-new-seq))
      (setf *MidiShare-start-time* 1)   
      (setf startevent (om-midi-new-evt (om-midi-get-num-from-type "Start") :port port :date 0))
      (when startevent (om-midi-seq-add-evt *playing-midi-seq* startevent))
      (setf tempoevent  (om-midi-new-evt (om-midi-get-num-from-type "Tempo") :port port :ref 0 :date 0 :tempo 1000000))
      (when tempoevent (om-midi-seq-add-evt *playing-midi-seq* tempoevent))
      )))
    
(defmethod FinalizePlayingSeq ((player (eql 'midishare)) dur &key (port nil))
  (when *midiplayer*
    (let ((midiport (or port *Outmidiport*))
          finalevent)
    (setf finalevent (om-midi-new-evt (om-midi-get-num-from-type "Stop") :port port :date (+ dur 1)))
    (when finalevent (om-midi-seq-add-evt *playing-midi-seq* finalevent))
    
    (handler-bind ((error #'(lambda (e) 
                              (om-beep-msg "Error setting Midi player...")
                              ;(midiplay-reset)
                              ;(oa::om-midi-extend) ;;; restarts with more memory...
                              (abort e))))
      (om-midi-set-player *midiplayer* *playing-midi-seq* 1000)
      )
    )))
  





(defun send-pitchwheel-event (chan port val)
  (let ((event (om-midi-new-evt (om-midi-get-num-from-type "PitchWheel") :date 0 :chan chan :port port :bend val)))
    (when event 
      (om-midi-send-evt event *midiplayer*)
      )))

(defmethod player-stop :after ((engine (eql :midishare)) &optional play-list)
  (when (and *midiplayer* *ms-microplay*)
     (send-pitchwheel-event 0 *outmidiport* 0) 
     (send-pitchwheel-event 1 *outmidiport* 0) 
     (send-pitchwheel-event 2 *outmidiport* 0) 
     (send-pitchwheel-event 3 *outmidiport* 0)))























