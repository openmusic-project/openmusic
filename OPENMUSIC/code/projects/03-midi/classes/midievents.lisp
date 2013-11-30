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


;====================
;==== MIDI EVENT ====
;====================
;;; THE INTERNAL ONE (SIMPLE AND FAST)

(defclass Midi-Score-Element (simple-score-element) ())

;;; THE OBJECT, INHERITING SCORE-ELEMENT
(defclass! MidiEvent (simple-score-element)
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
          (list nil (list (list 1 *midi-event-types*)) nil nil nil nil nil)))


(defmethod initialize-instance :after ((self midievent) &rest l &key (mode 0))
  (if (numberp (ev-type self)) 
      (setf (ev-type self) (num2evType (ev-type self))))
  (setf (ev-fields self) (list! (ev-fields self)))
)

(defmethod update-miniview ((self t) (value MidiEvent)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self MidiEvent) x x1 y y1 edparams view)
   (om-with-focused-view view
     (om-draw-rect 0 0 (w view) (h view)  )
     (om-draw-string 10 20 "MIDI Event")
     (om-draw-string 10 30 (string (ev-type self)))
))

(defmethod draw-mini-view  ((self t) (value MidiEvent)) 
   (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))

;=== Printing MIDI Event
;(defmethod print-object ((self MidiEvent) x) (call-next-method))

(defmethod evt-to-string ((self MidiEvent))
  (format nil "MIDIEVENT:: @~D ~A chan ~D track ~D port ~D: ~D" 
           (ev-date self) (ev-type self) (ev-chan self) (ev-ref self) (ev-port self) (ev-fields self)))


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
  :menuins (list (list 1 *midi-event-types*))
  :doc "Tests if <self> is of type <type>.

(see function MS-EVENT for a list of valid MIDI event types)
"
  :icon 907 
  (or (not type)
      (find (ev-type self) (list! type))))


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





