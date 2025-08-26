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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; MIDI package

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
                    (not (equal (om-midi::midi-evt-type event) :timesign))
                    (not (equal (om-midi::midi-evt-type event) :sysex))this will send this to player and no notes playing if commented?
                    (not (equal (om-midi::midi-evt-type event) :pitchbend))
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
  (let ((newinterval (and interval (interval-intersec 
                                    interval 
                                    (list at (+ at (strechDate (get-obj-dur self) (Qtempo self))))))))
    (loop for event in (evtlist self) 
          when (and (not (equal (om-midi::midi-evt-type event) :Tempo))
                    (or (null interval) (and newinterval (point-in-interval (+ (om-midi::midi-evt-date event) at) newinterval)))
                    (not (equal (om-midi::midi-evt-type event) :EndTrack)))
          collect   
        (let ((newevent (om-midi::copy-midi-evt event))
              (really-at (if interval 
                             (- (+ at (strechDate (om-midi::midi-evt-date event) (Qtempo self))) (first interval)) 
                           (+ at (strechDate (om-midi::midi-evt-date event) (Qtempo self))))))
          (unless (om-midi::midi-evt-port newevent) (setf (om-midi::midi-evt-port newevent) port))
          (setf (om-midi::midi-evt-date newevent) really-at)
          newevent))
    ))


(defmethod PrepareToPlay ((player (eql :midi)) (self midicontrol) at &key approx port interval voice)
  (let ((evtseq (objfromobjs self (make-instance 'eventmidi-seq)))) 
    (setf (Qtempo evtseq) (Qtempo self))
    (PrepareToPlay player evtseq at
                   :approx approx 
                   :port (or port (port self))
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
                                      :ref 0 ; (if voice voice 0) ;; send to 0 to read in external sequencers/DAWs (?)
                                    ;  :chan 1 ;;; WHANT IS THE CHANNEL OF A TIME SIGN EVENT ?
                                      :date at
                                      :fields (list num den 24 div)
                                      :port (or port *def-midi-out*))))

    ;; (cerror "forsett" "stopp")
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


;=== Note is a leaf of the "container tree"

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
  (when (and (not (memq (tie self) '(continue end)))
             (>= (midic self) 0)) ;; otherwise MIDI generates an error
    (setf port (or port (port self)))
    (setf approx (or approx 2))
    (let ((channel-shift (micro-channel (approx-m (midic self) (round approx)) approx)))
      (let ((chan (+ (chan self) channel-shift))
            (pitch (truncate (approx-scale (get-current-scale approx) (midic self)) 100))
            (vel (vel self))
            (dur (- (real-dur self) 2))			    ; why ?
            (date at)					
            (voice (or voice 0)))
        
        ;(print (format nil "pitch(mc) ~D, chan ~D, approx=~D, appx.pitch=~D ===> shif ~D to channel ~D" 
        ;               (midic self) (chan self) approx pitch
        ;               channel-shift chan))
        
        (let ((newinterval (and interval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))))
          (when (or (null interval) newinterval)
            (note-events port chan pitch vel 
                         ;; dur
                         (if interval 
                             (- (second newinterval) (first newinterval) 1) 
			   dur)
                         ;; DATE
                         (if interval
                             (- (first newinterval)  (first interval)) 
			   date)
                         voice))
          )))))



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

(defmethod PrepareToPlay ((player (eql :midi)) (self chord-seq) at &key approx port interval voice)
  ;(print (list player approx))
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

|#











