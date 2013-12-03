(in-package :om)


(defvar *midiplayer* nil      "refnum of the player")
(defvar *midirecorder* nil    "refnum of the recorder")
(defvar *midifilter* nil      "allow filter midievents in the recorder")



(defvar *midi-share?* nil "Is MidiShare loaded?")

;;;Open MidiShare and connections
;;;
;;;=== MidiOpen : redefinition 
;;;== modif : connection du recorder
(defun midi-open ()
  "Check if MidiShare is present, if this is the case open the player and
the recorder, this function is called by a def-load-pointers"
  (setf *midiplayer* nil)
  (if (setf *midi-share?* (midishare-startup))
      (om-without-interrupts  
        (open-ms-players)
        (enable-player :midishare)
        )
    (om-message-dialog (format nil (om-str :lib-error) "MIDI")))
  t)

; (midi-close)
; (midi-open)

;;; Close MidiShare and off the scheduler
(defun midi-close ()
   "If MidiShare is present, close the player and the recorder before quit the application"
   (when *midi-share?*
     ;(close-ms-players) ;;; remettre ?
     (midishare-exit)
     (disable-player :midishare)
     (setf *midi-share?* nil))
   )

(defun open-ms-players ()
  (setq *midiplayer* (midishare-open-player "OMPlayer"))
  (setf *midirecorder* (midishare-open-player "OMRecorder"))
  )

(defun close-ms-players ()
  (when *midiplayer* (midishare-close-player *midiplayer*))
  (when *midirecorder* (midishare-close-player *midirecorder*))
  )

(defun midiplay-reset ()
  (ignore-errors (close-ms-players))
  (open-ms-players)
  (print "MIDI player reset."))

(defun make-port-menu (list posi) (declare (ignore list posi)))

;;;===============================
;#-linux (om-add-init-func 'midi-open)  
;(om-add-exit-cleanup-func 'midi-close t)
;;;===============================



;;;(ms::midishare)                                           	; <== EVALUATE THIS EXPRESSION.
;;;(defparameter *refnum* (ms::midiopen "Common Lisp"))     	; <== EVALUATE THIS EXPRESSION.
;;;
;;;
;;;(ms::MidiConnect *refnum* 0 -1)
;;;
(defun ms-send-note (pitch)
  (let ((event (ms::MidiNewEv ms::typeNote)))	; ask for a new note event
    (ms::chan event 0)			; set the midi channel to 0 (means channel 1)
    (ms::port event 0)			; set the destination port to 0
    (ms::field event 0 pitch)		; set the pitch field
    (ms::field event 1 100)		; set the velocity field
    (ms::field event 2 1000)		; set the duration field to 1 second
    (ms::MidiSendIm *refnum* event))	; send the note immediatly
  )						; <== EVALUATE THIS DEFINITION
;;;
;;;
;;;
;;;(send-note 60)

;;; redefinition : préparer la séquence MIDI
(defmethod InitPlayingSeq ((player (eql 'midishare)) dur &key (port nil))
  (when *midiplayer*
    (let ((midiport (or port *def-midi-out*))
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
    (let ((midiport (or port *def-midi-out*))
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
  (when (and *midiplayer* *midi-microplay*)
     (send-pitchwheel-event 0 *def-midi-out* 0) 
     (send-pitchwheel-event 1 *def-midi-out* 0) 
     (send-pitchwheel-event 2 *def-midi-out* 0) 
     (send-pitchwheel-event 3 *def-midi-out* 0)))



;;; DEFAULT MIDI PLAYER                  
(defmethod player-name ((player (eql :midishare))) "MidiShare player")   ;;; A short name
(defmethod player-desc ((player (eql :midishare))) "(default)")   ;;; a description
(defmethod player-special-action ((player (eql :midishare))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :midishare))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :midishare))) :midi)   ;;; communication protocol (:midi / :udp)



;;; NEW MIDI PLAYER (NOT YET AVAILABLE)                 
(defmethod player-name ((player (eql :midishare-rt))) "MidiShare RT")   ;;; A short name
(defmethod player-desc ((player (eql :midishare-rt))) "experimental real-time MIDI player")   ;;; a description
(defmethod player-special-action ((player (eql :midishare-rt))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :midishare-rt))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :midishare-rt))) :midi)   ;;; communication protocol (:midi / :udp)


;;;==============================
;;; Stop/play/pause for MidiShare player
;;;==============================

(defvar *ms-list-to-play* nil)
(defvar *ms-loop* nil)

;;; IN MIDISHARE DEFAULT PLAYER, YOU MUST STOP BEFORE TO PLAY SOMETHING NEW
;(defmethod prepare-to-play ((engine (eql :midishare)) (player omplayer) object at interval)
;  (player-stop :midishare)
;  (InitPlayingSeq 'midishare (get-obj-dur object))
;  (PrepareToPlay 'midishare object (+ at (real-duration object 0)) :interval interval)
;  (FinalizePlayingSeq 'midishare (get-obj-dur object))
;  )


(defmethod prepare-to-play ((engine (eql :midishare)) (player omplayer) object at interval)
  (let ((approx (if (caller player) (get-edit-param (caller player) 'approx))))
    (push (list object at interval approx) *ms-list-to-play*)))


;;; PLAY (NOW) 
;;; NOT CALLED WITH MS PLAYER 
(defmethod player-play-object ((engine (eql :midishare)) object &key interval)
  (print (format nil "~A : play ~A - ~A" engine object interval)))

;;; START (PLAY WHAT IS SCHEDULED)
;(defmethod player-start ((engine (eql :midishare)) &optional play-list)
;  (when *midiplayer* (om-midi-start-player *midiplayer*)))

(defmethod player-start ((engine (eql :midishare)) &optional play-list)
  ;(print *ms-list-to-play*)
  (om-midi-stop-player *midiplayer*)
  (om-midi-set-player *midiplayer* (om-midi-new-seq) 1000)
  (let ((object (mapcar 'car *ms-list-to-play*)))
    (InitPlayingSeq 'midishare (get-obj-dur object))
    (loop for item in *ms-list-to-play* do
          (PrepareToPlay 'midishare (nth 0 item) 
                         (+ (nth 1 item) (real-duration (nth 0 item) 0)) 
                         :interval (nth 2 item) :approx (nth 3 item)))
    (FinalizePlayingSeq 'midishare (get-obj-dur object))
    (when *ms-loop* (om-midi-set-loop-player *midiplayer* 0 *ms-loop*))
    (when *midiplayer* (om-midi-start-player *midiplayer*))))

;;; PAUSE (all)
(defmethod player-pause ((engine (eql :midishare)) &optional play-list)
  (when *midiplayer* (om-midi-pause-player *midiplayer*)))

;;; CONTINUE (all)
(defmethod player-continue ((engine (eql :midishare)) &optional play-list)
  (when *midiplayer* (om-midi-cont-player *midiplayer*)))

;;; STOP (all)
(defmethod player-stop ((engine (eql :midishare)) &optional play-list)
  (when *midiplayer* (om-midi-stop-player *midiplayer*)
    (om-midi-set-player *midiplayer* (om-midi-new-seq) 1000))
  (setf *ms-list-to-play* nil *ms-loop* nil))

;;; SET LOOP (before play)
;;; desyncronizes with the general scheduler...
(defmethod player-set-loop ((engine (eql :midishare)) &optional start end)
  ;(om-midi-set-loop-player *midiplayer* 0 (- end start))
  (setf *ms-loop* (- end start))
  )

(defmethod player-record ((engine (eql :midishare)))
  (midi-start-record))

(defmethod player-record-stop ((engine (eql :midishare)))
  (midi-stop-record))



;================================
;RECORD
;================================

(defun midi-start-record ()
  (if *recording-midi-p*
    (om-beep-msg "Recording is on")
    (when *midirecorder*
      (om-print "Recording...")
      ; pour entendre ce qu'on enregistre
      (om-midi-connect 0 0)

      (setf *recording-midi-p* t)
      (om-midi-set-player *midirecorder* (om-midi-new-seq) 1000)
      (om-midi-record-player *midirecorder* 1)
      (om-midi-start-player *midirecorder*) t)))

(defun midi-stop-record ()
   (when *recording-midi-p*
     (let (recording-seq rep)
       (om-print "Recording Off...")
       ; pour entendre ce qu'on enregistre
       (om-midi-disconnect 0 0)
       (om-midi-stop-player *midirecorder*)
       (setf recording-seq (om-midi-player-get-seq *midirecorder*))
       (when recording-seq
         (let ((newseq (delete-tempo-info recording-seq 1000)))
           (setf rep (midievents2midilist newseq))
           (om-midi-free-seq newseq))
         (setf *recording-midi-p* nil)
         (loop for note in rep 
               when (not (minusp (third note))) collect note)))))


