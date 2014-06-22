(in-package :om)


(defun midi-seq-start-events (&optional port)
  (list (om-midi::make-midi-evt :type :Start :port (or port *def-midi-out*) :date 0)
        (om-midi::make-midi-evt :type :Tempo :port (or port *def-midi-out*) :date 0 :ref 0 :date 0 :fields (list *midi-tempo*))
        ))

(defmethod midi-seq-end-events (at &optional (port nil))
  (list (om-midi::make-midi-evt :type :Stop :port (or port *def-midi-out*) :date (+ at 1))))


;;; :MIDI is a 'meta' player identifier for preparetoplay, that will build a sequence of OM-MIDI-EVT
;;===================
;; MIDI CLASSES
;;===================

(defmethod PrepareToPlay ((player (eql :midi)) (self MidiFile) at &key approx port interval voice)
  (declare (ignore approx voice))
   ;(setf converted-seq (delete-tempo-info (fileseq self) (clicks self)))
   ;(setf converted-seq (fileseq self))
  (setf port (or port *def-midi-out*))
  (let ((newinterval (and interval (interval-intersec interval (list at (+ at (get-obj-dur self)))))))
    (loop for event in (fileseq self) 
          when (and (or (not interval) newinterval)
                    (not (equal (om-midi::midi-evt-type event) :Tempo))
                    (or (null interval) (point-in-interval (+ (om-midi::midi-evt-date event) at) newinterval))
                    (not (equal (om-midi::midi-evt-type event) :EndTrack)))
          collect   
          (let ((newevent (om-midi::copy-midi-evt event)))
            (setf (om-midi::midi-evt-port newevent) port)
            (setf (om-midi::midi-evt-date newevent) (if newinterval 
                                               (- (+ (om-midi::midi-evt-date event) at) (first interval))
                                             (+ (om-midi::midi-evt-date event) at)))
            newevent))
  ))


(defmethod PrepareToPlay ((player (eql :midi)) (self EventMidi-seq) at &key approx port interval voice)
  (declare (ignore approx voice))
  (setf port (or port *def-midi-out*))
  (let ((newinterval (and interval (interval-intersec interval (list at (+ at (get-obj-dur self)))))))
    (loop for event in (evtlist self) 
          when (and (not (equal (om-midi::midi-evt-type event) :Tempo))
                    (or (null interval) (point-in-interval (+ (om-midi::midi-evt-date event) at) newinterval))
                    (not (equal (om-midi::midi-evt-type event) :EndTrack)))
          collect   
        (let ((newevent (om-midi::copy-midi-evt event))
               ;(really-at (if interval (- (+ at date) (first interval)) (+ at date))) 
              (really-at (if interval 
                             (- (+ at (strechDate (om-midi::midi-evt-date event) (Qtempo self))) (first interval)) 
                           (+ at (strechDate (om-midi::midi-evt-date event) (Qtempo self))))))
          (setf (om-midi::midi-evt-port newevent) port)
          (setf (om-midi::midi-evt-date newevent) really-at)
          newevent))
    ))


(defmethod PrepareToPlay ((player (eql :midi)) (self midicontrol) at &key approx port interval voice)
  (let ((evtseq (objfromobjs self (make-instance 'eventmidi-seq)))) 
    (setf (Qtempo evtseq) (Qtempo self))
    (PrepareToPlay player evtseq at
                   :approx approx 
                   :port port
                   :interval interval
                   :voice voice)
    ))


(defmethod PrepareToPlay ((player (eql :midi)) (self MidiEvent) at &key approx port interval voice)
   (declare (ignore approx))
   (when (or (null interval) (point-in-interval at interval))
     (list (om-midi::make-midi-evt
      :date (+ (ev-date self) (if interval (- at (first interval)) at))
      :type (ev-type self)
      :chan (ev-chan self)
      :ref (or voice (ev-ref self))
      :port (or port (ev-port self) *def-midi-out*)
      :fields (ev-fields self)
      ))))


;;======================
;; SCORE CLASSES
;;======================

;=== Measure send a TimeSign Midi event as a bar marker (and time signature information) 
(defmethod PrepareToPlay ((player (eql :midi)) (self measure) at &key approx port interval voice)
  ;(setf port (verify-port port))
  (let* ((num (first (first (tree self))))
         (den (round (log (second (first (tree self))) 2)))
         (div 8 )   ;;; (max-div self))
         (MeasureEvent (om-midi::make-midi-evt :type :TimeSign
                                      :ref (if voice voice 0) 
                                      :chan 0 ;;; WHANT SI THE CHANNEL OF A TIME SIGN EVENT ??
                                      :date at
                                      :fields (list num den 24 div)
                                      :port port)))

    (cons MeasureEvent
          (remove nil
                  (loop for sub in (inside self) collect
                        (let ((objstart (+ at (offset->ms sub))))
                          (let ((in-interval (or (null interval)
                                                 (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub)))))))
                            (when in-interval
                              (PrepareToPlay player sub objstart 
                                             :approx approx 
                                             :port port
                                             :interval interval
                                             :voice voice)))
                  ))))))


;=== Multi-seq plays all subcomponets (chord-seqs) each one in a different track. 
(defmethod PrepareToPlay ((player (eql :midi)) (self multi-seq) at &key approx port interval voice)
  ;(setf port (verify-port port))
  (loop for sub in (inside self) 
        for v = 1 then (+ v 1) collect
        (let* ((objstart (+ at (offset->ms sub)))
               (in-interval (or (null interval)
                               (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub)))))))
          (when in-interval
            (PrepareToPlay player sub objstart 
                               :approx approx 
                               :port port
                               :interval interval
                               :voice v)
            ))))


;;; SEPARER LES VOIES OU PAS ???
;=== Poly plays all subcomponets (voices) each one in a different track. 
(defmethod PrepareToPlay ((player (eql :midi)) (self poly) at &key approx port interval voice)
  ;(setf port (verify-port port))
  (loop for sub in (inside self) 
        for v = 1 then (+ v 1) collect
        (let* ((objstart (+ at (offset->ms sub)))
               (in-interval (or (null interval)
                               (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub)))))))
          (when in-interval
            (PrepareToPlay player sub objstart 
                               :approx approx 
                               :port port
                               :interval interval
                               :voice v)
            ))))


;=== Play a chord in "arp" mode
(defmethod PrepareToPlay ((player (eql :midi)) (chord arp-chord) at &key  approx port interval voice)
     ;(setf port (verify-port port))
    (loop for note in (notes chord)
          for offset from 0 by 400
          collect (PrepareToPlay player note (+ offset at) 
                                 :approx approx
                                 :port port :interval interval :voice voice)))


;=== Note is a leaf of the "container tree"

;;; split note on channels in case of microtonal setup
;;; tone = 0, 1/8 = 1, 1/4 = 2, 1/8 = 3
(defun micro-channel (midic &optional approx)
  (let ((channel-mc-unit (/ 200 (or approx 8))))
    ;;; by default channel 1 = 25 mc, channel 2 = 50 mc, channel 3 = 75mc
    (round (mod midic 100) channel-mc-unit)))

(round (mod 6452 100) (/ 200 8))

(defun note-events (port chan pitch vel dur date track)
   (list (om-midi::make-midi-evt :type :Note
                        :date date 
                        :port (or port *def-midi-out*) 
                        :chan chan 
                        :ref track
                        :fields (list pitch vel dur))
         ))

(defun note-events (port chan pitch vel dur date track)
   (list (om-midi::make-midi-evt :type :KeyOn
                        :date date 
                        :port (or port *def-midi-out*) 
                        :chan chan 
                        :ref track
                        :fields (list pitch vel))
         (om-midi::make-midi-evt :type :KeyOff
                        :date (+ dur date) 
                        :port (or port *def-midi-out*) 
                        :chan chan 
                        :ref track
                        :fields (list pitch 0))
         ))
         
(defmethod PrepareToPlay ((player (eql :midi)) (self note) at &key  approx port interval voice)
  (when (not (memq (tie self) '(continue end)))
    (setf port (or port (port self)))
    (setf approx (or approx 2))
    (let ((chan (+ (chan self) (micro-channel (approx-m (midic self) approx) 8)))
          (pitch (truncate (approx-scale (get-current-scale approx) (midic self)) 100))
          (vel (vel self))
          (dur (- (real-dur self) 2))  ;;; why ?
          (date at) ;;; (+ *MidiShare-start-time* at))
          (voice (or voice 0)))
      (let ((newinterval (and interval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))))
        (when (or (null interval) newinterval)
          (note-events port chan pitch vel 
                    ;;; dur
                    (if interval 
                        (- (second newinterval) (first newinterval) 1) 
                      dur)
                    ;;; DATE
                    (if interval
                        (- (first newinterval)  (first interval))  ;;; (- (first newinterval)  (+ *MidiShare-start-time* (first newinterval)))
                      date)
                    voice)))
      )))


;=== Send a Note event
#|
(defun playnote (port chan pitch vel dur date track)
  (setf port (or port *def-midi-out*))
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
|#



;;;==============================
;;; MICROTONALITE
;;;==============================

;;; use msb / LSB ?
(defun make-pitchwheel-event (date chan port val)
  (om-midi::make-midi-evt :type :PitchBend
                        :date date
                        :port port 
                        :chan chan 
                        :ref 0
                        :fields val))

;;; variable set by the MIDI preferences
(defvar *midi-microplay* nil)


; (om+ 8192 '(0 1024 2048 3072))

(defun microplay-reset (port player)
  (let ((send-fun (or (om-midi::send-midi-event-function player)
                      'midi-send-evt))
        (p (or port *def-midi-out*)))    
  (funcall send-fun (make-pitchwheel-event 0 1 p 0))
  (funcall send-fun (make-pitchwheel-event 0 2 p 0))
  (funcall send-fun (make-pitchwheel-event 0 3 p 0))
  (funcall send-fun (make-pitchwheel-event 0 4 p 0))
  ))



(defun microplay-events (at dur port)
  ;;; make or send ... ?
  (list (make-pitchwheel-event at 1 port 0) 
        (make-pitchwheel-event at 2 port 1024) 
        (make-pitchwheel-event at 3 port 2048) 
        (make-pitchwheel-event at 4 port 3072) 
        (make-pitchwheel-event (+ at dur) 1 port 0) 
        (make-pitchwheel-event (+ at dur) 2 port 0) 
        (make-pitchwheel-event (+ at dur) 3 port 0) 
        (make-pitchwheel-event (+ at dur) 4 port 0)))

(defmethod PrepareToPlay ((player (eql :midi)) (self chord-seq) at &key approx port interval voice)
  (if (and *midi-microplay* approx (find approx '(4 8) :test '=))
    (append 
     (microplay-events at (get-obj-dur self) port)
     (call-next-method))
    (call-next-method)))

(defmethod PrepareToPlay ((player (eql :midi)) (self voice) at &key approx port interval voice)
  (if (and *midi-microplay* approx (find approx '(4 8) :test '=))
    (append 
     (microplay-events at (get-obj-dur self) port)
     (call-next-method))
    (call-next-method)))





