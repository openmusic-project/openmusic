;;; MIDI SEQUENCES

(in-package :om)

;=====================
;=== EVENTMIDI-SEQ ===
;=====================
(defclass* EventMidi-seq (sequence* midi-score-element) 
   ((Ltype :initform nil :accessor Ltype :initarg :Ltype :type t :documentation "list of event types")
    (Ldate :initform nil :accessor Ldate :initarg :Ldate :type t :documentation "list of dates (ms)")
    (Lref :initform nil :accessor Lref :initarg :Lref :type t  :documentation "list of track numbers")
    (Lport :initform nil :accessor Lport :initarg :Lport :type t :documentation "list of output port numbers")
    (Lchan :initform nil :accessor Lchan :initarg :Lchan :type t :documentation "list of MIDI channels (1-16)")
    (Lfields :initform nil :accessor Lfields :initarg :Lfields :type t :documentation "list of event data")
    (name :initform (string "Midi Events sequence") :accessor name :type string)
    (evtlist :initform nil :accessor evtlist)
    )
   (:icon 901)
   (:documentation "
A sequence of MIDI events.

EventMIDI-seq represents a list of any types of MIDI events.
It is equivalent to a MIDI file contents and can be saved as such without any data loss.

The structure is similar to that of a CHORD-SEQ: each parameters are specified by a separate list.
"
    ))


(add-player-for-object 'EventMidi-seq :midi-player)

(defmethod default-edition-params ((self EventMidi-seq))
  (pairlis '(player) (list :midi-player)))


(defmethod eventmidi-seq-p ((self EventMidi-seq))  t)
(defmethod eventmidi-seq-p ((self t)) nil)

(defmethod empty-midiseq-p ((self EventMidi-seq))
   (null (evtlist self)))

(defmethod set-evt-list ((self EventMidi-seq))
  (let ((defdelay (if (>= (length (list! (slot-value self 'Ldate))) 2)
                      (- (car (last (slot-value  self 'Ldate))) 
                         (car (last (slot-value  self 'Ldate) 2)))
                    1000))
        (dates (list! (slot-value self 'Ldate)))
        (types (list! (slot-value self 'Ltype)))
        (fields (list! (slot-value self 'Lfields)))
        (chans (list! (slot-value self 'LChan)))
        (refs (list! (slot-value self 'Lref)))
        (ports (list! (slot-value self 'Lport))))
    (setf (evtlist self)
          (loop while (or dates types fields chans ports refs)
                for date = (or (pop dates) (+ date defdelay))
                for field = (or (pop fields) field)
                for type = (or (pop types) type) ;; (if (numberp (setf type (or (pop types) type))) (num2evType type) type)
                for chan = (or (pop chans) chan)
                for ref = (or (pop refs) ref)
                for port = (or (pop ports) port)
                collect (om-midi::make-midi-evt
                         :date date
                         :type type
                         :chan chan
                         :ref ref
                         :port port
                         :fields field
                         )))
    ))

(defmethod initialize-instance ((self EventMidi-seq) &rest initargs &key lparam)
  ;(declare (ignore initargs)) 
  (call-next-method)
  (set-evt-list self)
  self)

(defmethod Ldate ((self EventMidi-seq)) (mapcar 'om-midi::midi-evt-date (evtlist self)))
(defmethod Lfields ((self EventMidi-seq)) (mapcar 'om-midi::midi-evt-fields (evtlist self)))
(defmethod Lchan ((self EventMidi-seq)) (mapcar 'om-midi::midi-evt-chan (evtlist self)))
(defmethod Lport ((self EventMidi-seq)) (mapcar 'om-midi::midi-evt-port (evtlist self)))
(defmethod Lref ((self EventMidi-seq)) (mapcar 'om-midi::midi-evt-ref (evtlist self)))
(defmethod Ltype ((self EventMidi-seq)) (mapcar 'om-midi::midi-evt-type (evtlist self)))


(defmethod allowed-in-maq-p ((self EventMidi-seq))  t)

(defmethod get-obj-dur ((self EventMidi-seq))
   (loop for item in (Ldate self)
         maximize item))

; next method
;(defmethod real-duration ((self EventMidi-seq) time)
;  (values (get-obj-dur self) (+ time (get-obj-dur self))))

(defmethod strech ((self EventMidi-seq) (num integer) (denom integer) &optional parent))
  
(defmethod allow-strech-p ((self simple-container) (factor number))  factor)

(defmethod draw-mini-view  ((self t) (value EventMidi-seq)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod draw-obj-in-rect ((self EventMidi-seq) x x1 y y1 edparams view)
    (let ((mid (round (/ (h view) 2)))
           (long (w view)) 
           l f dur xpt)
      (om-with-focused-view view
        (om-draw-string 10 15 (if (stringp (name self)) (name self) "MidiEvents sequence"))
        (om-draw-rect 0 0 (w view) (h view)  )
        (om-draw-line 0 mid (w view) mid)
        (unless (empty-midiseq-p self)   
           (setf l (nth (- (length (LDate self)) 1) (Ldate self)))
           (setf f (nth 0 (lDate self)))
           (setf dur (- l f))
           (if (= dur 0)
               (om-draw-line (round (/ (w view) 2)) (+ mid 5) (round (/ (w view) 2)) (- mid 5))
             (loop for date in (Ldate self) do 
                       (setf xpt (round (* long (/ date dur))))
                       (om-draw-line xpt (+ mid 2) xpt (- mid 2))
                       )
             )
           ))))


(defmethod execption-save-p ((self eventmidi-seq)) 'eventmidi-seq)
(defmethod save-exepcion ((self eventmidi-seq))
  `(when (find-class ',(type-of self) nil)
     (let ((rep (make-instance ',(type-of self)
                               :Ldate ',(Ldate self)
                               :Ltype ',(Ltype self)
                               :Lchan ',(Lchan self)
                               :Lref ',(Lref self)
                               :Lport ',(Lport self)
                               :Lfields ',(Lfields self))))
       (setf (name rep) ,(name self))
       rep
       )))


;;;========================
;;; Functions for midiseq
;;;========================

(defmethod! temporal-sort ((self eventmidi-seq))
  :indoc '("an EventMIDI-seq object")
  :initvals '(nil)
  :doc "Sorts the events in <self> in temporal order and returns a new EventMIDI-seq."
  :icon 915
  (let ((sorted-seq (make-instance 'eventmidi-seq)))
    (setf (name sorted-seq) (name self))
    (setf (evtlist sorted-seq) (sort (mapcar #'om-midi::copy-midi-evt (evtlist self)) 'om-midi::midi-evt-<))
    sorted-seq
    ))


(defmethod! separate-channels ((self eventmidi-seq))
  :indoc '("an EventMIDI-seq object")
  :initvals '(nil)
  :doc "Separates MIDI channels in <self> on diferents tacks."
  :icon 915
  (loop for evt in (evtlist self) do
        (setf (om-midi::midi-evt-ref evt) (om-midi::midi-evt-chan evt)))
  self)


;=== Creates a list of MidiEvents 

(defmethod! get-midievents ((self EventMidi-seq) &optional test)
  :icon 902
  (remove nil
          (loop for e in (evtlist self) collect
                (let ((event (make-instance 'MidiEvent
                                            :ev-date (om-midi::midi-evt-date e)
                                            :ev-type (om-midi::midi-evt-type e)
                                            :ev-chan (om-midi::midi-evt-chan e)
                                            :ev-ref (om-midi::midi-evt-ref e)
                                            :ev-port (om-midi::midi-evt-port e)
                                            :ev-fields (om-midi::midi-evt-fields e)
                                            )))
                  (when (or (not test) (funcall test event))
                    event)))))


(defmethod! create-midiseq ((self t) &optional newname)
  :initvals '(nil nil)
  :indoc '("and object" "a sequence name (string)")
  :doc "Creates a new EventMIDI-seq object from <self> with possibility to set a specific name (<newname>) to the new sequence."
  :icon 914 
  (let ((new-emseq (objFromObjs self (make-instance 'EventMidi-seq))))
    (if (stringp newname) (setf (name new-emseq) newname))
    new-emseq
))



;=== Converts a list of MidiEvents in EventMidi-seq
;=== NOw evry object that can be coverted as MidiEvent list with get-midievents method
;=== can be converted to EventMidi-seq
(defmethod* objFromObjs ((self list) (type eventmidi-seq))
  (let ((reponse (make-instance (type-of type)))
        (eventList (get-midievents self))
        (dateList nil)
        (typeList nil)
        (chanList nil)
        (portList nil)
        (refList nil)
        (fieldsList nil))
    (loop for listItem in eventList do
                (push (ev-date listItem) dateList)
                (push (ev-type listItem) typeList)
                (push (ev-chan listItem) chanList)
                (push (ev-port listItem) portList)
                (push (ev-ref listItem) refList)
                (push (ev-fields listItem) fieldsList))
    (setf (Ldate reponse) (reverse dateList))
    (setf (Ltype reponse) (reverse typeList))
    (setf (Lchan reponse) (reverse chanList))
    (setf (Lport reponse) (reverse portList))
    (setf (Lref reponse) (reverse refList))
    (setf (Lfields reponse) (reverse fieldsList))
    (set-evt-list reponse)
    (temporal-sort reponse)
    ))

(defmethod* objFromObjs ((self MidiEvent) (type eventmidi-seq))
  (let ((reponse (make-instance (type-of type))))
    (setf (Ldate reponse) (list (ev-date self)))
    (setf (Ltype reponse) (list (ev-type self)))
    (setf (Lchan reponse) (list (ev-chan self)))
    (setf (Lport reponse) (list (ev-port self)))
    (setf (Lref reponse) (list (ev-ref self)))
    (setf (Lfields reponse) (list (ev-fields self)))
    (set-evt-list reponse)
    reponse))







;=== Ctreates tracks with a list of notes (pitch date dur vel chan track port)
;=== (grouping notes with same track value)
(defun midiList2trackList (midilist)
  (let ((tracks-list nil) (tracks nil) (rep nil) trackNum pos)
  (loop for note in midilist do
        (if (plusp (third note))
          (progn
            (setf trackNum (sixth note))
            (if (member trackNum tracks)
              (progn
                (setf pos (position trackNum tracks))
                ;(push (list (first note) (second note) (third note) (fourth note) (fifth note)) (nth pos tracks-list))
                (push note (nth pos tracks-list))
                )
              (progn
                ;(push (list (list (first note) (second note) (third note) (fourth note) (fifth note))) tracks-list) 
                (push (list note) tracks-list) 
                (push trackNum tracks)
                )))))
  (loop for trk in tracks-list do
        (push (reverse trk) rep))
  rep))


(defmethod evm-seq2midilist ((self eventmidi-seq))
  (midievents2midilist (evtlist self)))
 
;=== Returns a list of tracks from th EventMidi-seq object
;=== A track is a list of notes (pitch date dur vel chan) 
(defmethod! get-midi-notes ((self eventmidi-seq))
  :initvals '(nil) 
  :indoc '("a MIDI fiule or sequence") 
  :icon 909
  (let ((trackList (midilist2trackList (evm-seq2midiList self))) tmpList rep)
    (loop for track in trackList do
         (setf tmpList (mat-trans track))
         (push (mat-trans (list (first tmpList) (second tmpList) (third tmpList) (fourth tmpList) (fifth tmpList))) rep))
    (reverse rep)))






