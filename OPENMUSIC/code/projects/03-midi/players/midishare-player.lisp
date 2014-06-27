(in-package :om)


(defvar *midiplayer* nil      "refnum of the player")
(defvar *midirecorder* nil    "refnum of the recorder")
(defvar *midifilter* nil      "allow filter midievents in the recorder")

; (defparameter *refnum* (ms::midiopen "Common Lisp"))
; (ms::MidiClose *refnum*)

(defun init-midishare-players ()
  "Check if MidiShare is present, if this is the case open the player and
the recorder, this function is called by a def-load-pointers"
  (if (or om-midi::*midishare-loaded?* 
          (and (print "MS player try to load MidiShare...") (om-midi::midishare-startup)))
      (om-without-interrupts  
        (setq *midiplayer* (om-midi::midishare-open-player "OMPlayer"))
        ;(setf *midirecorder* (om-midi::midishare-open-player "OMRecorder"))
        (enable-player :midishare)
        t)
    (progn 
      ;(om-message-dialog (format nil (om-str :lib-error) "MIDI"))
      (print "MidiShare player could not start. Could not load MidiShare")
      nil)
    ))

(defun close-midishare-players ()
   "If MidiShare is present, close the player and the recorder before quit the application"
   ;(when om-midi::*midishare-loaded?*
     (when *midiplayer* (om-midi::midishare-close-player *midiplayer*))  ;;; remettre ?
     (when *midirecorder* (om-midi::midishare-close-player *midirecorder*)) ;;; remettre ?
     ;(om-midi::midishare-exit) ;;; do nothing     
     ;(setf *midi-share?* nil)
   ;  )
   (disable-player :midishare)
   )

;;; redefinition of the midishare restart fun
(defun om-midi::midishare-restart-players ()
  (ignore-errors 
    (when *midiplayer* (om-midi::midishare-close-player *midiplayer*))  
    (when *midirecorder* (om-midi::midishare-close-player *midirecorder*)))
  (init-midishare-players)
  (print "MidiShare players restarted."))

;;;===============================
(om-add-init-func 'init-midishare-players)  
(om-add-exit-cleanup-func 'close-midishare-players t)
;;;===============================

;;; DEFAULT MIDI PLAYER                  
(defmethod player-name ((player (eql :midishare))) "MidiShare player")   ;;; A short name
(defmethod player-desc ((player (eql :midishare))) "Uses MidiShare as MIDI I/O and player")   ;;; a description
(defmethod player-special-action ((player (eql :midishare))) nil)  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :midishare))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :midishare))) :midi)   ;;; communication protocol (:midi / :udp)


;;;=============================
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

(defmethod prepare-to-play ((engine (eql :midishare)) (player omplayer) object at interval params)
  (let ((approx (if (find :approx params)
                    (nth (1+ (position :approx params)) params)
                  (if (caller player) (get-edit-param (caller player) 'approx))))
        (port (if (find :port params)
                  (nth (1+ (position :port params)) params)
                (if (caller player) (get-edit-param (caller player) 'outport)))))
    (if (equal port :default) (setf port *def-midi-out*))
    (push (list object at interval approx port) *ms-list-to-play*)))

;;; PLAY (NOW) 
;;; NOT CALLED WITH MS PLAYER 
(defmethod player-play-object ((engine (eql :midishare)) object &key interval params)
  (print (format nil "~A : play ~A - ~A" engine object interval)))

;;; START (PLAY WHAT IS SCHEDULED)
;(defmethod player-start ((engine (eql :midishare)) &optional play-list)
;  (when *midiplayer* (om-midi-start-player *midiplayer*)))

(defmethod player-start ((engine (eql :midishare)) &optional play-list)
  ;(print *ms-list-to-play*)
  (om-midi::midishare-stop-player *midiplayer*)
  ;(print "222")
  (om-midi::midishare-set-player *midiplayer* 
                                (append (midi-seq-start-events)
                                        (loop for item in *ms-list-to-play*
                                              append
                                              (remove nil 
                                                      (flat (PrepareToPlay :midi (car item) (nth 1 item) ;(+ (nth 1 item) (real-duration (car item) 0)) 
                                                                           :interval (nth 2 item) :approx (nth 3 item)
                                                                           :port (nth 4 item)))))
                                         (midi-seq-end-events (get-obj-dur (mapcar 'car *ms-list-to-play*))))
                                 1000)
  ;(print "333")
  (when *ms-loop* (om-midi::midishare-set-loop-player *midiplayer* 0 *ms-loop*))
  (when *midiplayer* (om-midi::midishare-start-player *midiplayer*))
  )


;;; PAUSE (all)
(defmethod player-pause ((engine (eql :midishare)) &optional play-list)
  (when *midiplayer* (om-midi::midishare-pause-player *midiplayer*)))

;;; CONTINUE (all)
(defmethod player-continue ((engine (eql :midishare)) &optional play-list)
  (when *midiplayer* (om-midi::midishare-cont-player *midiplayer*)))

;;; STOP (all)
(defmethod player-stop ((engine (eql :midishare)) &optional play-list)
  (when *midiplayer* 
    (om-midi::midishare-stop-player *midiplayer*)
    (when *midi-microplay*
      (let ((ports (remove-duplicates (mapcar 'fifth *ms-list-to-play*))))
        (loop for p in ports do (microplay-reset p engine)))))
  (setf *ms-list-to-play* nil *ms-loop* nil))



;;; SET LOOP (before play)
;;; desyncronizes with the general scheduler...
(defmethod player-set-loop ((engine (eql :midishare)) &optional start end)
  ;(om-midi-set-loop-player *midiplayer* 0 (- end start))
  (setf *ms-loop* (- end start))
  )



;================================
;RECORD = TOD0 !!!
;================================

(defmethod player-record ((engine (eql :midishare)))
  (midishare-start-record))

(defmethod player-record-stop ((engine (eql :midishare)))
  (midishare-stop-record))

(defvar *ms-player-record* nil)

(defun midishare-start-record ()
  (if *ms-player-record*
    (om-beep-msg "MIDI recording is on..")
    (progn 
      (unless *midirecorder*
        (setf *midirecorder* (om-midi::midishare-open-player "OMRecorder")))
      (when *midirecorder*
        (om-print "MidiShare recording...")
        ; pour entendre ce qu'on enregistre
        (om-midi::midishare-connect 0 0)
        (setf *ms-player-record* t)
        (om-midi::midishare-set-player *midirecorder* nil 1000)
        (om-midi::midishare-record-player *midirecorder* 1)
        (om-midi::midishare-start-player *midirecorder*)
        t))))

(defun midishare-stop-record ()
   (when *ms-player-record*
     (om-print "Recording Off.")
     (om-midi::midishare-disconnect 0 0)
     (setf *ms-player-record* nil)
     (om-midi::midishare-stop-player *midirecorder*)
     (let ((recording-seq (om-midi::midishare-player-get-seq *midirecorder*)))
       (when recording-seq
         (let ((newseq (midievents2midilist (delete-tempo-info recording-seq 1000))))
           (remove-if 'minusp newseq :key 'third))))))



