;;; MIDI SEQUENCES

;=====================
;=== EVENTMIDI-SEQ ===
;=====================
(defclass* EventMidi-seq (sequence* Midi-score-element) 
   ((Ltype :initform (list  'KeyOn) :accessor Ltype :initarg :Ltype :type t :documentation "list of event types")
    (Ldate :initform (list 0) :accessor Ldate :initarg :Ldate :type t :documentation "list of dates (ms)")
    (Lref :initform (list 0) :accessor Lref :initarg :Lref :type t  :documentation "list of track numbers")
    (Lport :initform (list 0) :accessor Lport :initarg :Lport :type t :documentation "list of output port numbers")
    (Lchan :initform (list 1) :accessor Lchan :initarg :Lchan :type t :documentation "list of MIDI channels (1-16)")
    (Lfields :initform (list nil) :accessor Lfields :initarg :Lfields :type t :documentation "list of event data")
    (name :initform (string "Midi Events sequence") :accessor name :type string))
   (:icon 901)
   (:documentation "
A sequence of MIDI events.

EventMIDI-seq represents a lost of any types of MIDI events.
It is equivalent to a MIDI file contents and can be saved as such without any data loss.

The structure is similar to that of a CHORD-SEQ: each parameters are specified by a separate list.
"
    ))

(defmethod eventmidi-seq-p ((self EventMidi-seq))  t)
(defmethod eventmidi-seq-p ((self t)) nil)

(defmethod empty-midiseq-p ((self EventMidi-seq))
   (not (Ldate self))) 

(defvar *load-version* *om-version*)

(defmethod initialize-instance ((self EventMidi-seq) &rest initargs &key lparam)
  (declare (ignore initargs)) 
  (call-next-method)
  (if (and (< *load-version* 4.8) (not (member :ml-maquette *features*)))
    (progn
      (change-class self 'eventmidi-seq-old)
      (setf (lparam self) lparam))
    (let* ((defdelay (if (>= (length (slot-value  self 'Ldate)) 2)
                       (- (car (last (slot-value  self 'Ldate))) 
                          (car (last (slot-value  self 'Ldate) 2)))
                       1000))
           (dates (list! (slot-value  self 'Ldate)))
           (types (list! (slot-value  self 'Ltype)))
           (fields (list! (slot-value  self 'Lfields)))
           (chans (list! (slot-value  self 'LChan)))
           (refs (list! (slot-value  self 'Lref)))
           (ports (list! (slot-value  self 'Lport)))
           (updateself 
            (loop while (or dates types fields chans ports refs)
                  
                  for date = (or (pop dates) (+ date defdelay))
                  for field = (or (pop fields) field)
                  for type = (if (numberp (setf type (or (pop types) type))) type (om-midi-symb2mtype type))
                  for chan = (or (pop chans) chan)
                  for ref = (or (pop refs) ref)
                  for port = (or (pop ports) port)
                  
                  collect date into da
                  collect type into ty
                  collect field into fi
                  collect chan into ch
                  collect port into po
                  collect ref into re
                  finally (return (list da ty fi ch po re)))          
            ))
      (setf (Ldate self) (first updateself))
      (setf (Ltype self) (second updateself))
      (setf (Lfields self) (third updateself))
      (setf (Lchan self) (fourth updateself))
      (setf (Lport self) (nth 4 updateself))
      (setf (Lref self) (nth 5 updateself))
      self)))
 
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
     (make-instance ',(type-of self)
       :Ldate ',(Ldate self)
       :Ltype ',(Ltype self)
       :Lchan ',(Lchan self)
       :Lref ',(Lref self)
       :Lport ',(Lport self)
       :Lfields ',(Lfields self)
       )))


;;;========================
;;; Functions for midiseq
;;;========================

(defmethod! temporal-sort ((self eventmidi-seq))
  :indoc '("an EventMIDI-seq object")
  :initvals '(nil)
  :doc "Sorts the events in <self> in temporal order and returns a new EventMIDI-seq."
  :icon 915
  (let ((seqlist (list (Ltype self) (Ldate self) (Lref self) (Lport self) (Lchan self) (Lfields self)))
        (sorted-seq (make-instance 'eventmidi-seq))
        tr-list)
    (setf tr-list (mat-trans seqList))
    (setf tr-list (sort tr-list '< :key 'second))
    (setf tr-list (mat-trans tr-List))
    (setf (Ltype sorted-seq) (first tr-list))
    (setf (Ldate sorted-seq) (second tr-list))
    (setf (Lref sorted-seq) (third tr-list))
    (setf (Lport sorted-seq) (fourth tr-list))
    (setf (Lchan sorted-seq) (fifth tr-list))
    (setf (Lfields sorted-seq) (sixth tr-list))
    (setf (name sorted-seq) (name self))
    sorted-seq
))

(defmethod! separate-channels ((self eventmidi-seq))
  :indoc '("an EventMIDI-seq object")
  :initvals '(nil)
  :doc "Separates MIDI channels in <self> on diferents tacks (modifies the 'lref' slot)."
  :icon 915
  (loop for ch in (Lchan self)
        for i = 0 then (+ i 1) do
        (setf (nth i (Lref self)) ch))
  self)


;=== Creates a list of MidiEvents 
(defmethod! get-midievents ((self EventMidi-seq) &optional test)
  :icon 902
  (let ((evtList nil) evt)
  (loop for i from 0 to (- (length (Ldate self)) 1) do
        (setf event (make-instance 'MidiEvent
          :ev-date (nth i (Ldate self))
          :ev-type (nth i (Ltype self))
          :ev-chan (nth i (Lchan self))
          :ev-ref (nth i (Lref self))
          :ev-port (nth i (Lport self))
          :ev-fields (nth i (Lfields self))
        ))
        (if (or (not test) (funcall test event))
          (push event evtList)
        ))
  (reverse evtList)))
 


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
    (temporal-sort reponse)))

(defmethod* objFromObjs ((self MidiEvent) (type eventmidi-seq))
  (let ((reponse (make-instance (type-of type))))
    (setf (Ldate reponse) (list (ev-date self)))
    (setf (Ltype reponse) (list (ev-type self)))
    (setf (Lchan reponse) (list (ev-chan self)))
    (setf (Lport reponse) (list (ev-port self)))
    (setf (Lref reponse) (list (ev-ref self)))
    (setf (Lfields reponse) (list (ev-fields self)))
    reponse))

;=== Returns a complete midi notes (pitch date dur vel chan track port) list
(defmethod evm-seq2midilist ((self eventmidi-seq))
  (let ((midiList nil))
    (loop for date in (Ldate self)
          for type in (Ltype self)
          for param in (Lfields self)
          for ref in (Lref self)
          for port in (Lport self)
          for chan in (Lchan self) do
          (case type
            (0  (push (list (first param) date (third param) (second param) chan ref port) midiList))
            (1 (if (= (second param) 0)
                 (close-notes-on midiList (first param) chan date ref)
                 (push (list (first param) date (* -1 date) (second param) chan ref port) midiList)))
            (2 (close-notes-on midiList (first param) chan date ref))))
    (reverse midiList)))

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

(defmethod update-miniview ((self t) (value eventmidi-seq)) 
  (om-invalidate-view self t))