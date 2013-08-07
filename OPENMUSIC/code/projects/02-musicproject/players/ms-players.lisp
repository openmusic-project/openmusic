(in-package :om)


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

(defvar *list-to-play* nil)
(defvar *loop* nil)

;;; IN MIDISHARE DEFAULT PLAYER, YOU MUST STOP BEFORE TO PLAY SOMETHING NEW
(defmethod prepare-to-play ((engine (eql :midishare)) (player omplayer) object at interval)
  (push (list object at interval) *list-to-play*)
  )

(defmethod prepare-to-play ((engine (eql :midishare)) (player omplayer) object at interval)
  (player-stop :midishare)
  (InitPlayingSeq 'midishare (get-obj-dur object))
  (PrepareToPlay 'midishare object (+ at (real-duration object 0)) :interval interval)
  (FinalizePlayingSeq 'midishare (get-obj-dur object))
  )

;;; PLAY (NOW) 
;;; NOT CALLED WITH MS PLAYER 
(defmethod player-play-object ((engine (eql :midishare)) object &key interval)
  (print (format nil "~A : play ~A - ~A" engine object interval)))

;;; START (PLAY WHAT IS SCHEDULED)
(defmethod player-start ((engine (eql :midishare)) &optional play-list)
  (when *midiplayer* (om-midi-start-player *midiplayer*)))

(defmethod player-start ((engine (eql :midishare)) &optional play-list)
  ; (om-midi-stop-player *midiplayer*)
  ;(om-midi-set-player *midiplayer* (om-midi-new-seq) 1000)
  (let ((object (mapcar 'car *list-to-play*)))
    (InitPlayingSeq 'midishare (get-obj-dur object))
    (loop for item in *list-to-play* do
          (PrepareToPlay 'midishare (car item) (+ (cadr item) (real-duration (car item) 0)) :interval (caddr item)))
    (FinalizePlayingSeq 'midishare (get-obj-dur object))
    (when *loop* (om-midi-set-loop-player *midiplayer* 0 *loop*))
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
  (setf *list-to-play* nil *loop* nil))

;;; SET LOOP (before play)
;;; desyncronizes with the general scheduler...
(defmethod player-set-loop ((engine (eql :midishare)) &optional start end)
  ;(om-midi-set-loop-player *midiplayer* 0 (- end start))
  (setf *loop* (- end start))
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
           (setf rep (mievents2midilist newseq))
           (om-midi-free-seq newseq))
         (setf *recording-midi-p* nil)
         (loop for note in rep 
               when (not (minusp (third note))) collect note)))))


