;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)


; KEPT ONLY FOR COMPATIBILITY

(defclass* EventMidi (simple-score-element)
   ((ev-type :initform "typeVolume" :accessor ev-type :initarg :ev-type :type string)
    (ev-chan :initform 0 :accessor ev-chan :initarg :ev-chan :type integer)
    (ev-field :initform nil :accessor ev-field))
   (:icon 148))
  

(defmethod allowed-in-maq-p ((self EventMidi))  t)
(defmethod get-obj-dur ((self EventMidi)) 0)

(defmethod real-dur ((self EventMidi)) (values 0 0))
(defmethod extent ((self EventMidi)) 0)
(defmethod exeption-class-p ((self (eql 'Eventmidi))) t)

;;; ...


;====================
;==== MIDI EVENT ====
;====================
(defclass* MidiEvent (simple-score-element)
   ((ev-type :initform 'KeyOn :accessor ev-type :initarg :ev-type :type t :documentation "the type of event")
    (ev-date :initform 0 :accessor ev-date :initarg :ev-date :documentation "the date of event (ms)")
    (ev-ref :initform 0 :accessor ev-ref :initarg :ev-ref :type integer :documentation "a track number")
    (ev-port :initform 0 :accessor ev-port :initarg :ev-port :type integer :documentation "output port")
    (ev-chan :initform 1 :accessor ev-chan :initarg :ev-chan :type integer :documentation "MIDI channel (1-16)")
    (ev-fields :initform nil :accessor ev-fields :initarg :ev-fields :documentation "event data (list or value)"))
   (:icon 913)
   (:documentation"
A MIDI event object.

MIDIEvent is the primitive of all MIDI objects. It can contain any type of MIDI event.
'Playing' the MIDI event (fot intance in a maquette or using the patch editor keyboard shortcuts) just sends the EVENT data.

Lists of MIDIEvents can be extracted form other OM objects using GET-MIDIEVENTS.

"))

(defmethod midievent-p ((self MidiEvent))  t)
(defmethod midievent-p ((self t)) nil)
  
(defmethod allowed-in-maq-p ((self MidiEvent))  t)
(defmethod get-obj-dur ((self MidiEvent)) 0)

(defmethod real-dur ((self MidiEvent)) (values 0 0))
(defmethod extent ((self MidiEvent)) 0)

(defmethod get-slot-in-out-names ((self MidiEvent))
  (values '("self" "ev-type" "ev-date" "ev-ref" "ev-port" "ev-chan" "ev-fields")
          '(nil 'KeyOn 0 0 0 1 nil)
          '("object" "midi event type" "date" "track" "port" "channel" "event content")
          (list nil (list (list 1 *ms-events-symb*)) nil nil nil nil nil)))


(defmethod initialize-instance :after ((self midievent) &rest l &key (mode 0))
  (if (not (numberp (ev-type self))) (setf (ev-type self) (om-midi-symb2mtype (ev-type self))))
  (setf (ev-fields self) (list! (ev-fields self)))
)

(defmethod update-miniview ((self t) (value MidiEvent)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self MidiEvent) x x1 y y1 edparams view)
   (om-with-focused-view view
     (om-draw-rect 0 0 (w view) (h view)  )
     (om-draw-string 10 20 "MIDI Event")
     (om-draw-string 10 30 (eventtype2str (ev-type self)))
))

(defmethod draw-mini-view  ((self t) (value MidiEvent)) 
   (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))

;=== Printing MIDI Event
(defmethod print-object ((self MidiEvent) x) 
  (format x "[MIDIEVENT ~D ~D / track ~D / port ~D / chan ~D / VALUE=~D]" (ev-date self) (eventtype2str (ev-type self)) (ev-ref self) (ev-port self) (ev-chan self) (ev-fields self)))


;======================================
;==== Test functions for MIDI events ==
;======================================
(defmethod! test-Date ((self MidiEvent) tmin tmax)
  :initvals '(nil nil nil)
  :indoc '("a MidiEvent" "min date" "max date")
  :doc "Tests if <self> falls between <tmin> and <tmax>."
  :icon 907 
  (and (or (not tmin)(>= (ev-date self) tmin))
       (or (not tmax)(< (ev-date self) tmax))))
  

(defmethod! test-Channel ((self MidiEvent) channel)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "MIDI channel number (1-16) or channel list")
  :doc "Tests if <self> is in channel <channel>."
  :icon 907 
  (or (not channel) (member (ev-chan self) (list! channel))))

(defmethod! test-Port ((self MidiEvent) port)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "output port number (or list)")
  :doc "Tests is <self> ouputs to <port>."
  :icon 907 
  (or (not port) (member (ev-port self) (list! port))))

(defmethod! test-Ref ((self MidiEvent) ref)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a track number or list")
  :doc "Tests <self> is on track <ref>."
  :icon 907 
  (or (not ref) (member (ev-ref self) (list! ref))))

(defmethod! test-Track ((self MidiEvent) track)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a track number or list")
  :doc "Tests <self> is on track <ref>."
  :icon 907 
  (test-ref self track))

(defmethod! test-Type ((self MidiEvent) type)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a MIDI event type") 
  :menuins (list (list 1 *ms-events-symb*))
  :doc "Tests if <self> is of type <type>.

(see function MS-EVENT for a list of valid MIDI event types)
"
  :icon 907 
  (or (not type)
      (if (symbolp type)
        (= (ev-type self) (om-midi-symb2mtype type))
        (member (ev-type self) (list! type)))))


(defmethod! midievent-filter ((self MidiEvent) type ref port channel)
  :initvals '(nil nil nil nil nil)
  :indoc '("a MIDIEvent" "event type(s)" "track number(s)" "output port(s)" "MIDI channel(s)")
  :doc "Tests the attributes of <self>.

Returns T if <self> matches <type> (see function MS-EVENT for a list of valid MIDI event types), <ref>, <port> and <channel>.

If a test value is NIL, the test is not performed on this attribute.

"
  :icon 907 
  (and (or (not type) (member (ev-type self) (mapcar 'om-midi-symb2mtype (list! type))))
                            (or (not ref) (member (ev-ref self) (list! ref)))
                            (or (not port) (member (ev-port self) (list! port)))
                            (or (not channel) (member (ev-chan self) (list! channel)))))

(defmethod! get-midievents ((self MidiEvent) &optional test)
  :icon 902 
  (if (or (not test) (funcall test self)) (list self) nil))


;=== converts to string the slot "fields" of a textual MidiEvent

;;; replaced copy-instance with copy-container
(defmethod! me-textinfo ((self MidiEvent))
    :indoc '("a MIDIEvent or list of MIDIEvents")
    :icon 908
    :doc "
Returns the MIDIEvent or list after converting to string the data (ev-field) of all textual events (e.g. types 'textual', 'copyright', 'lyrics', 'instrname', etc.)
"
  (let ((newEvt (copy-container self)))
    (if (istextual (ev-type self))
      (setf (ev-fields newEvt) (list2string (ev-fields self))))
  newEvt))

;=== converts to string the slot "fields" of textual events from a MidiEvent list
(defmethod! me-textinfo ((self list))
  (let ((rep nil))
    (loop for event in self do
          (if (midievent-p event)
            (progn
              ;(me-textinfo event)
              (push (me-textinfo event) rep)
              ))) 
    (reverse rep)))


;;; compatibility : play a eventmidi

(defmethod* PrepareToPlay ((player t) (self EventMidi) at &key approx port interval voice)
   (declare (ignore approx))
   (when (and *midiplayer* (or (null interval) (point-in-interval at interval)))
     (setf port (verify-port port))
     (let ((param1 (first (ev-field self)))
           (param2 (second (ev-field self)))
           (really-at (if interval (- at (first interval)) at)) 
           (midichan (- (ev-chan self) 1))
           (miditype (if (string-equal (ev-type self) "typeVolume")
                         (om-midi-get-num-from-type "CtrlChange") (om-midi-get-num-from-type (subseq (ev-type self) 4))))
           event)
       (cond
        ((string-equal (ev-type self) "typePitchWheel")
         (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                           :bend param1)))
        ((string-equal (ev-type self) "typePitchBend")
         (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                           :bend (round (- (* (/ param1 127) 16382 ) 8192)))))
        ((string-equal (ev-type self) "typeProgChange")
         (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                           :pgm param1)))
        ((string-equal (ev-type self) "typeChanPress")
         (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                           :param param1)))
        ((string-equal (ev-type self) "typeKeyPress")
         (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                           :kpress param2 :pitch param1)))
        ((string-equal (ev-type self) "typeCtrlChange")
         (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                           :ctrlchange (list param1 param2))))
        ((string-equal (ev-type self) "typeVolume")
         (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                           :ctrlchange (list 7 param1))))
        ((string-equal (ev-type self) "typeSysEx")
         (setf event (om-midi-make-new-evt miditype :date really-at :port port :chan midichan 
                                           :bytes param1)))
        (t (setf event (om-midi-make-new-evt miditype :date really-at :port port :chan midichan 
                                             :vals (ev-field self))))
        )
       (when event
         (om-midi-seq-add-evt *playing-midi-seq* event)))))

;;;==================================
;;; generic function get-midievents :
;;; converts anything to a list of midiEvent
;;;==================================

(defmethod! get-midievents ((self t) &optional test)
  :icon 902
  :indoc '("an OM object" "a test function")
  :doc "
Converts any OM object (<self>) to a list of MIDIEvents.

The optional argument <test> is a function or lambda patch testing MIDIEvents one by one.
If <test> returns T, then the MIDIEvent is collected.
"
  nil)

(defmethod! get-midievents ((self list) &optional test)
  :icon 902
  (let ((evtList nil) event)
    (loop for listitem in self do
          (if (midievent-p listitem)
              (progn
              (setf event (make-instance 'MidiEvent
                                   :ev-date (ev-date listitem)
                                   :ev-type (ev-type listitem)
                                   :ev-chan (ev-chan listitem)     
                                   :ev-ref (ev-ref listitem)
                                   :ev-port (ev-port listitem)
                                   :ev-fields (ev-fields listitem)
                                   ))
              (if (or (not test) (funcall test event))
                (push event evtList)
                ))
            (let ((tmpList (get-midievents listItem)))
              (if tmpList (push tmpList evtList)))
            ))
    (flat (reverse evtList))))


;=========================================
;Not yet finish
;=========================================
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




;===================
;==== TEMPO-MAP ====
;===================
;=== A Tempo-Map is an object containing a tempo changes and time signatures (also used as bar markers)
;=== of an object (MidiFile, EventMidi-seq, Voice, ...)
;=== It can be extracted from an object this method get-midievents is defined for this object
(defclass* Tempo-map (sequence*) 
   ((tempo-Evts :initform nil :accessor tempo-Evts :initarg :tempo-Evts :type t :documentation "tempo changes")
    (timeSign-Evts :initform nil :accessor timeSign-Evts :initarg :timeSign-Evts :type t :documentation "measure changes"))
    (:icon 911)
    (:documentation "
A TEMPO-MAP represents the tempo events and measure changes (also used as bar markers) in a MIDI sequence.

<tempo-evts> is a list of ((t1 tempo1) (t2 tempo2) ...)
<timesign-evts> is a list of ((t1 (measure-info1)) (t2 (measure-info2)) ...)

"
     ))



(defmethod update-miniview ((self t) (value Tempo-map)) 
  (om-invalidate-view self t))

;=== Creates a list of MidiEvents 
(defmethod! get-midievents ((self Tempo-Map) &optional test)
  :icon 902
  (let ((evtList nil) evt fields)
  (loop for tempoitem in (tempo-Evts self) do
        (setf event (make-instance 'MidiEvent
          :ev-date (first tempoitem)
          :ev-type (om-midi-get-num-from-type "Tempo")
          :ev-ref 0
          :ev-fields (second tempoItem)))
        (if (or (not test) (funcall test event))
          (push event evtList)))
  (loop for timesignitem in (timeSign-Evts self) do
        (setf event (make-instance 'MidiEvent
          :ev-date (first timesignitem)
          :ev-type (om-midi-get-num-from-type "TimeSign")
          :ev-ref 0
          :ev-fields (second timesignItem)))
        (if (or (not test) (funcall test event))
          (push event evtList)))
  (reverse evtList)))


;=== Extract tempo-map from a simple-container
;=== get-midievent method must be defined for this container
(defmethod! get-TempoMap ((self simple-container))
  :initvals '(nil) 
  :indoc '("a musical object or MIDI sequence") 
  :icon 905
  :doc "Extracts and generates a TEMPO-MAP object from <self>."
  (let ((tempoEvents nil)
        (tempoMap (make-instance 'tempo-Map))
        (tempoList nil) (timeSignList nil))
    (setf tempoEvents (get-midievents self #'(lambda (x) (or (test-type x 'tempo) (test-type x 'timeSign)))))
    (loop for event in tempoEvents do
          (cond
           ((= (ev-type event) (om-midi-get-num-from-type "Tempo"))
            (push (list (ev-date event) (first (ev-fields event))) tempoList))
           ((= (ev-type event) (om-midi-get-num-from-type "TimeSign"))
            (push (list (ev-date event) (ev-fields event)) timeSignList))
           (t nil)))
    
    (setf (tempo-Evts tempoMap) (reverse tempoList))
    (setf (timeSign-Evts tempoMap) (reverse timesignList))
    tempoMap))

(defmethod* objFromObjs ((self simple-container) (type Tempo-Map))
  (get-tempoMap self))


;=== If a tempo-map can be extracted from an object,
;=== we are able to find time of begining of each measure
;=== (can be useful to test midievents with measure number instead of time)
(defmethod! mesure-time ((self simple-container) num)
  :indoc '("a score object" "measure number")
  :initvals '(nil 0)
  :doc "
Returns time (ms) of measure <num> in <self>
"
  :icon 919
  (let ((mesureList (timeSign-Evts (get-tempomap self)))
        mesureList2 
        (last (- 1)))
    (loop for mesure in mesureList do
          (if (not (= last (first mesure))) (push mesure mesureList2))
          (setf last (first mesure))
          )
    (first (nth num (reverse mesureList2)))
))

(defmethod draw-mini-view  ((self t) (value Tempo-Map)) 
   (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))

(defmethod draw-obj-in-rect ((self Tempo-Map) x x1 y y1 edparams view)
   (om-with-focused-view view
     (loop for tpair in (tempo-evts self)
           for i = 1 then (+ i 1) do
           (om-draw-string 10 (* 12 i) (format nil "~D: ~D" (car tpair) (cadr tpair)))
           )))

;=====================
;OLD MIDI SEQUENCE compatibility
;=====================
(defclass* EventMidi-seq-old (sequence*) 
   ((Ldate :initform (list 0 1000) :accessor Ldate :initarg :Ldate :type t)
    (Ltype :initform (list  "typeVolume") :accessor Ltype :initarg :Ltype :type t)
    (Lchan :initform (list 1) :accessor Lchan :initarg :Lchan :type t)
    (Lparam :initform (list 127) :accessor Lparam :initarg :Lparam :type t))
   (:icon 148))

(defmethod initialize-instance ((self EventMidi-seq-old) &rest initargs)
  (declare (ignore initargs)) 
  (call-next-method)
  (let* ((defdelay (if (>= (length (slot-value  self 'Ldate)) 2)
                     (- (car (last (slot-value  self 'Ldate))) 
                        (car (last (slot-value  self 'Ldate) 2)))
                     1000))
         (dates (list! (slot-value  self 'Ldate)))
         (types (list! (slot-value  self 'Ltype)))
         (params (list! (slot-value  self 'Lparam)))
         (chans (list! (slot-value  self 'LChan)))
         (updateself (loop while (or dates  types  params chans)
                           for date = (or (pop dates) (+ date defdelay))
                           for type = (or (pop types) type)
                           for param = (or (pop params) param)
                           for chan = (or (pop chans) chan)
                           collect date into da
                           collect type into ty
                           collect param into pa
                           collect chan into ch
                           finally (return (list da ty pa ch)))))
    (setf (Ldate self) (first updateself))
    (setf (Ltype self) (second updateself))
    (setf (Lparam self) (third updateself))
    (setf (Lchan self) (fourth updateself))
    self))
  
(defmethod allowed-in-maq-p ((self EventMidi-seq-old))  t)

(defmethod get-obj-dur ((self EventMidi-seq-old))
   (loop for item in (Ldate self)
         maximize item))

; next method
;(defmethod real-duration ((self EventMidi-seq-old) time)
;  (values (get-obj-dur self) (+ time (get-obj-dur self))))

(defmethod strech ((self EventMidi-seq-old) (num integer) (denom integer) &optional parent ))

(defmethod* PrepareToPlay ((player t) (self EventMidi-seq-old) at &key  approx port interval voice)
   (declare (ignore approx))
   (when *midiplayer*
     (setf port (verify-port port))
     (loop for date in (Ldate self)
           for type in (Ltype self)
           for param in (Lparam self)
           for chan in (Lchan self) do
           (when (or (null interval) (point-in-interval  (+ at date) interval))
             (let ((param1 (first (list! param)))
                   (param2 (second (list! param)))
                   (really-at (if interval (- (+ at date) (first interval)) (+ at date)))
                   (midichan (- chan 1))
                   (miditype (if (string-equal type "typeVolume")
                                 (om-midi-get-num-from-type "CtrlChange") 
                               (om-midi-get-num-from-type (subseq (string type) 4))))
                   event)
                 (cond
                  ((string-equal type "typePitchWheel")
                   (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                                :bend param1)))
                  ((string-equal type "typePitchBend")
                   (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                                :bend (round (- (* (/ param1 127) 16382 ) 8192)))))
                  ((string-equal type "typeProgChange")
                   (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                                :pgm param1)))
                  ((string-equal type "typeChanPress")
                   (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                                :param param1)))
                  ((string-equal type "typeKeyPress")
                   (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                                :kpress param2 :pitch param1)))
                  ((string-equal type "typeCtrlChange")
                   (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                                :ctrlchange (list param1 param2))))
                  ((string-equal type "typeVolume")
                   (setf event (om-midi-new-evt miditype :date really-at :port port :chan midichan 
                                                :ctrlchange (list 7 param1))))
                  ((string-equal type "typeSysEx")
                   (setf event (om-midi-make-new-evt miditype :date really-at :port port :chan midichan 
                                                     :bytes param1)))
                  (t (setf event (om-midi-make-new-evt miditype :date really-at :port port :chan midichan 
                                                       :vals param)))
                  )
                 (when event 
                   (om-midi-seq-add-evt *playing-midi-seq* event)
                   ))))))

(defmethod update-miniview ((self t) (value EventMidi-seq-old)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self  EventMidi-seq-old) x x1 y y1 edparams  view)
   (om-with-focused-view view
     (om-draw-rect 0 0 (w view) (h view)  )))