;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;;; MIDI package
;;; OLD MIDI CLASSES
;;; KEPT ONLY FOR COMPATIBILITY
;;; CONSIDER REMOVING IT...

(in-package :om)

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

(defmethod* PrepareToPlay ((player t) (self EventMidi) at &key approx port interval voice)
   (declare (ignore approx))
   (when (and *midiplayer* (or (null interval) (point-in-interval at interval)))
     (setf port (or port *def-midi-out*))
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

;;; ...

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
     (setf port (or port *def-midi-out*))
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
