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
  


(defmethod* PrepareToPlay ((player t) (self MidiFile) at &key approx port interval voice)
   (declare (ignore approx))
   (when *midiplayer*
     (let (converted-seq event)
       ;(setf converted-seq (delete-tempo-info (fileseq self) (clicks self)))
       ;(setf converted-seq (fileseq self))
       (setf event (om-midi-seq-first-evt (fileseq self)))
       ;(setf event (midishare::firstEv (fileseq self)))
       (setf port (verify-port port))
       (if interval
         (let ((newinterval (interval-intersec interval (list at (+ at (get-obj-dur self))))))
           (when newinterval
             (loop while event do   
                   (when (and (point-in-interval (+ (om-midi-evt-get event :date) at)  newinterval)
                              (not (= (om-midi-evt-get event :type) (om-midi-get-num-from-type "EndTrack"))))
                     (let ((newevent (om-midi-copy-evt event)))
                           (when (or (and newevent (not (ms::nullptrp newevent)))
                                     (om-beep-msg "Error at copying MIDI event"))
                             (om-midi-evt-set newevent :port port)
                             (om-midi-evt-set newevent :date (- (+ (om-midi-evt-get event :date) at) (first interval)))
                             (if (not (= (om-midi-evt-get newevent :type) (om-midi-get-num-from-type "Tempo"))) 
                                 (om-midi-seq-add-evt *playing-midi-seq* newevent)))))
                     (setf event (om-midi-next-evt event)))))
         (loop while event do
               (unless (= (om-midi-evt-get event :type) (om-midi-get-num-from-type "EndTrack"))
                 (let ((newevent (om-midi-copy-evt event)))
                   (when (or (and newevent (not (ms::nullptrp newevent)))
                             (om-beep-msg "Error at copying MIDI event"))
                     (om-midi-evt-set newevent :port port)
                     (om-midi-evt-set newevent :date (+ (om-midi-evt-get event :date) at))
                     (if (not (= (om-midi-evt-get newevent :type) (om-midi-get-num-from-type "Tempo"))) 
                         (om-midi-seq-add-evt *playing-midi-seq* newevent)))))
               (setf event (om-midi-next-evt event)))
       ;(midishare::MidiFreeSeq converted-seq)
         ))))


(defmethod* PrepareToPlay ((player t) (self EventMidi-seq) at &key approx port interval voice)
   (declare (ignore approx))
   (when *midiplayer*
     (setf port (verify-port port))
     (loop for date in (Ldate self)
           for type in (Ltype self)
           for param in (Lfields self)
           for ref in (Lref self)
           for midiport in (Lport self)
           for chan in (Lchan self) do
           (when (or (null interval) (point-in-interval (+ at date) interval))
             (let ((param1 (first (list! param)))
                   (param2 (second (list! param)))
                   ;(really-at (if interval (- (+ at date) (first interval)) (+ at date))) 
                   (really-at (if interval (- (+ at (strechDate date (Qtempo self))) (first interval)) 
                                  (+ at (strechDate date (Qtempo self))))) 
                   event)
               (setf event (om-midi-new-evt type))
               (if (or (not event) (ms::nullptrp event))
                   (om-beep-msg (format nil "MidiShare can not create a new event of type ~D" type))
                 (progn
                   (om-midi-evt-set event :chan (- chan 1))
                   (om-midi-evt-set event :date really-at)
                   (om-midi-evt-set event :port (verify-port midiport))
                   (om-midi-evt-set event :ref ref)
                   (cond
                    ((= (om-midi-evt-get event :type) (om-midi-get-num-from-type "PitchBend"))
                     (if (= 1 (length param)) 
                       (om-midi-evt-set event :bend param1)
                       (progn (om-midi-evt-set event :field (list 0 param1)) (om-midi-evt-set event :field (list 1 param2)))
                       ))
                    ((= type (om-midi-get-num-from-type "ProgChange"))
                     (om-midi-evt-set event :pgm param1))
                    ((= type (om-midi-get-num-from-type "ChanPress"))
                     (om-midi-evt-set event :param param1))
                    ((= type (om-midi-get-num-from-type "KeyPress"))
                     (om-midi-evt-set event :kpress param2)
                     (om-midi-evt-set event :pitch param1))
                    ((= type (om-midi-get-num-from-type "CtrlChange"))
                     (om-midi-evt-set event :ctrlchange (list param1 param2)))
                    ((= type (om-midi-get-num-from-type "SysEx"))
                     (om-midi-evt-set event :bytes param1))
                    ((= type (om-midi-get-num-from-type "Tempo"))
                     (om-midi-evt-set event :tempo (bpm2mstempo param1)))
                    (t (if (and (istextual type) (stringp param1)) 
                           (om-midi-evt-set event :text param1)
                         (om-midi-evt-set event :vals param)))
                    )
                   (unless (= (om-midi-evt-get event :type) (om-midi-get-num-from-type "Tempo"))
                     (om-midi-seq-add-evt *playing-midi-seq* event))
                   )))))))



(defmethod* PrepareToPlay ((player t) (self midicontrol) at &key approx port interval voice)
   (declare (ignore approx))
   (when *midiplayer*
     (let ((evtseq (objfromobjs self (make-instance 'eventmidi-seq)))) 
       (setf (Qtempo evtseq) (Qtempo self))
       (PrepareToPlay player evtseq at 
                                :approx approx 
                                :port port
                                :interval interval
                                :voice voice)
)))


(defmethod* PrepareToPlay ((player t) (self MidiEvent) at &key approx port interval voice)
   (declare (ignore approx))
   (when (and *midiplayer* (or (null interval) (point-in-interval at interval)))
     (setf port (verify-port port))   
     (let ((param1 (first (ev-fields self)))
           (param2 (second (ev-fields self)))
           (really-at (if interval (- at (first interval)) at)) 
           (ref (ev-ref self)) 
           (type (ev-type self)) 
           event)
       (setf event (om-midi-new-evt type))
       (if (or (not event) (ms::nullptrp event))
           (om-beep-msg (format nil "MidiShare can not create a new event of type ~D" type))
         (progn
           (om-midi-evt-set event :chan (- (ev-chan self) 1))			
           (om-midi-evt-set event :port (verify-port (ev-port self)))
           (om-midi-evt-set event :ref ref)
           (om-midi-evt-set event :date (+ really-at (ev-date self)))
           (cond
            ((= (om-midi-evt-get event :type) (om-midi-get-num-from-type "PitchBend"))
                     (if (= 1 (length (ev-fields self))) 
                       (om-midi-evt-set event :bend param1)
                       (progn (om-midi-evt-set event :field (list 0 param1)) (om-midi-evt-set event :field (list 1 param2)))
                       ))
            ((= type (om-midi-get-num-from-type "ProgChange"))
             (om-midi-evt-set event :pgm param1))
            ((= type (om-midi-get-num-from-type "ChanPress"))
             (om-midi-evt-set event :param param1))
            ((= type (om-midi-get-num-from-type "KeyPress"))
             (om-midi-evt-set event :kpress param2)
             (om-midi-evt-set event :pitch param1))
            ((= type (om-midi-get-num-from-type "CtrlChange"))
             (om-midi-evt-set event :ctrlchange (list param1 param2)))
            ((= type (om-midi-get-num-from-type "Tempo"))
             (om-midi-evt-set event :tempo (bpm2mstempo param1)))
            ((= type (om-midi-get-num-from-type "SysEx"))
                     (om-midi-evt-set event :bytes param1))
            (t (if (and (istextual type) (stringp (car (ev-fields self))))
                   (om-midi-evt-set event :text (car (ev-fields self)))
                 (om-midi-evt-set event :vals (ev-fields self))))
            )
           (om-midi-seq-add-evt *playing-midi-seq* event))))))



;=== Measure send a TimeSign Midi event as a bar marker (and time signature information) 
(defmethod* PrepareToPlay ((player t) (self measure) at &key approx port interval voice)
   ;(setf port (verify-port port))
   (let ((MeasureEvent (om-midi-new-evt (om-midi-get-num-from-type "TimeSign")))
         (num (first (first (tree self))))
         (den (round (log (second (first (tree self))) 2)))
         (div 8 )   ;;; (max-div self))
         )
   (when MeasureEvent	    
             (om-midi-evt-set MeasureEvent :port port)
             (om-midi-evt-set MeasureEvent :chan 0)
             (om-midi-evt-set MeasureEvent :field (list 0 num))
             (om-midi-evt-set MeasureEvent :field (list 1 den))
             (om-midi-evt-set MeasureEvent :field (list 2 24))
             (om-midi-evt-set MeasureEvent :field (list 3 div))	       
             (om-midi-evt-set MeasureEvent :date at)
             (om-midi-evt-set MeasureEvent :ref (if voice voice 0))
             (om-midi-seq-add-evt *playing-midi-seq* MeasureEvent)))
   (loop for sub in (inside self) do
         (let ((objstart (+ at (offset->ms sub))))
           (if interval
             (let ((newinterval (interval-intersec interval 
                                                   (list objstart (+ objstart (get-obj-dur sub))))))
               (when newinterval
                 (PrepareToPlay player sub objstart 
                                :approx approx 
                                :port port
                                :interval interval
                                :voice voice)))
             (PrepareToPlay player sub objstart 
                            :approx approx 
                            :port port
                            :voice voice)))))


;=== Multi-seq plays all subcomponets (chord-seqs) each one in a different track. 
(defmethod* PrepareToPlay ((player t) (self multi-seq) at &key approx port interval voice)
  ;(setf port (verify-port port))
  (loop for sub in (inside self) 
        for v = 1 then (+ v 1) do
        (let ((objstart (+ at (offset->ms sub))))
          (if interval
            (let ((newinterval (interval-intersec interval 
                                                  (list objstart (+ objstart (get-obj-dur sub))))))
              (when newinterval
                (PrepareToPlay player sub objstart 
                               :approx approx 
                               :port port
                               :interval interval
                               :voice v)))
            (PrepareToPlay player sub objstart 
                           :approx approx 
                           :port port
                           :voice v)))))

;;; SEPARER LES VOIES OU PAS ???
;=== Poly plays all subcomponets (voices) each one in a different track. 
(defmethod* PrepareToPlay ((player t) (self poly) at &key approx port interval voice)
  ;(setf port (verify-port port))
  (loop for sub in (inside self) 
        for v = 1 then (+ v 1) do
        (let ((objstart (+ at (offset->ms sub))))
          (if interval
            (let ((newinterval (interval-intersec interval 
                                                  (list objstart (+ objstart (get-obj-dur sub))))))
              (when newinterval
                (PrepareToPlay player sub objstart 
                               :approx approx 
                               :port port
                               :interval interval
                               :voice v)))
            (PrepareToPlay player sub objstart 
                           :approx approx 
                           :port port
                           :voice v)))))

;=== Note is a leaf of the "container tree"

(defun micro-channel (midic &optional approx)
  (+ 1 (/ (mod midic 100) (/ 200 (or approx 8)))))


(defmethod* PrepareToPlay ((player t) (self note) at &key  approx port interval voice)
  (when (and *midiplayer* (not (memq (tie self) '(continue end))))
    (setf port (or port (port self)))
    (setf approx (or approx 2))
    (let ((chan (+ (1- (chan self)) (1- (micro-channel (approx-m  (midic self) approx) 8))))
          (pitch (truncate (approx-scale (get-current-scale approx) (midic self)) 100))
          (vel (vel self))
          (dur (- (real-dur self) 2))
          (date (+ *MidiShare-start-time* at))
          (voice (or voice 0)))
      (if interval
        (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
          (when newinterval
            (playnote port chan pitch vel (- (second newinterval) (first newinterval) 1) 
                      (- (+  *MidiShare-start-time* (first newinterval)) 
                         (first interval))
                      voice)))
        (playnote port chan pitch vel dur date voice)))))



;=== Send a Note event
(defun playnote (port chan pitch vel dur date track)
  (setf port (or port *outmidiport*))
  (if (< dur 65000)
      (let ((event (om-midi-new-evt (om-midi-get-num-from-type "Note") :port port :chan chan :date date :ref track :vals (list pitch vel dur))))
        (when event 
          (om-midi-seq-add-evt *playing-midi-seq* event)))
    (playlongnote port chan pitch vel dur date track)
    ))

(defun playlongnote (port chan pitch vel dur date track)
   (let ((event (om-midi-new-evt (om-midi-get-num-from-type "KeyOn") :port port :chan chan :date date :ref track))
         (event1 (om-midi-new-evt (om-midi-get-num-from-type "KeyOff") :port port :chan chan :date (+ dur date) :ref track)))
     (when event	       
       (om-midi-evt-set event :field (list 0 pitch))	       
       (om-midi-evt-set event :field (list 1 vel))	       
       (om-midi-seq-add-evt *playing-midi-seq* event))
     (when event1
       (om-midi-evt-set event1 :field (list 0 pitch))	       
       (om-midi-evt-set event1 :field (list 1 0))	       
       (om-midi-seq-add-evt *playing-midi-seq* event1)))
   )


;=== Play a chord in "arp" mode
(defmethod* PrepareToPlay ((player t) (chord arp-chord) at &key  approx port interval voice)
   (when *midiplayer*
     ;(setf port (verify-port port))
     (loop for note in (notes chord)
           for offset from 0 by 500
           do (PrepareToPlay player note (+  offset  at) 
                             :approx approx
                             :port port :interval interval :voice voice))))




;;;==============================
;;; Stop/play/pause for MidiShare player
;;;==============================
(defmethod Play-player ((self (eql 'midishare)))
   (when *midiplayer* (om-midi-start-player *midiplayer*)))

(defmethod Continue-Player ((self (eql 'midishare)))
   (when *midiplayer* (om-midi-cont-player *midiplayer*)))

(defmethod Pause-Player ((self (eql 'midishare)))
   (when *midiplayer* (om-midi-pause-player *midiplayer*)))

(defmethod Stop-Player ((self (eql 'midishare)) &optional view)
   (declare (ignore view))
   (when *midiplayer* (om-midi-stop-player *midiplayer*)
     (om-midi-set-player *midiplayer* (om-midi-new-seq) 1000)
     ))

(defun setPlayLoop (start end)
  (om-midi-set-loop-player *midiplayer* start end))



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

(defun midi-stop-record (port)
   (when *recording-midi-p*
     (let (recording-seq rep)
       (om-print "Recording Off...")
       ; pour entendre ce qu'on enregistre
       (om-midi-disconnect 0 0)
       (om-midi-stop-player *midirecorder*)
       (setf recording-seq (om-midi-player-get-seq *midirecorder*))
       (when recording-seq
         (let ((newseq (delete-tempo-info recording-seq 1000)))
           (setf rep (mievents2midilist newseq port))
           (om-midi-free-seq newseq))
         (setf *recording-midi-p* nil)
         (loop for note in rep 
               when (not (minusp (third note))) collect note)))))



;;;==============================
;;; MICROTONALITE
;;;==============================

(defvar *microplay* nil)
(setf *microplay* nil)


(defun make-pitchwheel-event (date chan port val)
  (let ((event (om-midi-new-evt (om-midi-get-num-from-type "PitchWheel") :date date :chan chan :port port :bend val)))
    (when event (om-midi-seq-add-evt *playing-midi-seq* event)
      )))


(defun send-pitchwheel-event (chan port val)
  (let ((event (om-midi-new-evt (om-midi-get-num-from-type "PitchWheel") :date 0 :chan chan :port port :bend val)))
    (when event 
      (om-midi-send-evt event *midiplayer*)
      )))



(defmethod* PrepareToPlay ((player t) (self chord-seq) at &key approx port interval voice)
  (when *microplay*
    (cond 
     ((or (= approx 8) (= approx 4))
      (make-pitchwheel-event at 0 port 0) 
      (make-pitchwheel-event at 1 port 1024) 
      (make-pitchwheel-event at 2 port 2048) 
      (make-pitchwheel-event at 3 port 3072) 
      (make-pitchwheel-event (+ at (get-obj-dur self)) 0 port 0) 
      (make-pitchwheel-event (+ at (get-obj-dur self)) 1 port 0) 
      (make-pitchwheel-event (+ at (get-obj-dur self)) 2 port 0) 
      (make-pitchwheel-event (+ at (get-obj-dur self)) 3 port 0)) 
     (t nil))
    )
  (call-next-method))



(defmethod Stop-Player :after ((self (eql 'midishare)) &optional view)
   (declare (ignore view))
   (when (and nil *midiplayer* *microplay*) 
     (send-pitchwheel-event 0 *outmidiport* 0) 
     (send-pitchwheel-event 1 *outmidiport* 0) 
     (send-pitchwheel-event 2 *outmidiport* 0) 
     (send-pitchwheel-event 3 *outmidiport* 0)))














