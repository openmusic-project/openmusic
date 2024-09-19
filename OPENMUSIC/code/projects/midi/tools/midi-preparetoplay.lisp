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

;;; split note on channels in case of microtonal setup (4 or 8)
;;; tone = 0, 1/8 = 1, 1/4 = 2, 3/8 = 3
;;; default bend channel 1 = 0, channel 2 = 25 mc, channel 3 = 50 mc, channel 4 = 75mc
;(defun micro-channel (midic &optional approx)
;  (let ((channel-mc-unit (/ 200 (or approx 8))))
;    (round (mod midic 100) channel-mc-unit)))

;; t / nil / list of approx where it must be applied
(defparameter *micro-channel-mode-on* '(4 8))
(defparameter *micro-channel-approx* 8)

(defun micro-channel-on (approx)
  (and 
   approx
   (if (consp *micro-channel-mode-on*) 
       (find approx *micro-channel-mode-on* :test '=)
     *micro-channel-mode-on*)))

#|
(defun micro-channel (midic &optional approx)
  (if (micro-channel-on approx)
      (let ((mod (/ 200 (or *micro-channel-approx* approx))))
        (round (approx-m (mod midic 100) approx) mod))
    0))
|#

;fix 290323
;when channel-shift = always and channel-shift-approx is nil (depending on approx)
;1/4 tones are no2 corrected.

#|
(defun micro-channel (midic &optional approx)
  (if (micro-channel-on approx)
      (if (= approx 4)
          (let ((modulo (mod (approx-m midic 4) 100)))
            (if (= 0 modulo) 0 2))
      (let ((mod (if (= approx 4) 2 (/ 200 (or *micro-channel-approx* approx)))))
        (round (approx-m (mod midic 100) approx) mod)))
    0))
|#

;; to finish (fullying up the other unused chans)


(defun micro-channel (midic &optional approx)
      (cond
       ((or (= approx 4) (= approx 224.1))
        (let ((modulo (mod (approx-m midic 4) 100)))
          (if (= 0 modulo) 0 2)))
       ((or (= 3 approx) (= 180.0 approx) (= 180.1 approx))
        (let ((modulo (mod (approx-m midic 3) 100)))
          (cond ((= 33 modulo) 1) ((= 133 modulo) 2) (t 0))))
       ((or (= 5 approx) (= 300.0 approx) (= 300.1 approx))
        (let ((modulo (mod (approx-m midic 5) 100)))
          (cond ((= 40 modulo) 1) ((= 80 modulo) 2) ((= 20 modulo) 3) ((= 60 modulo) 4)(t 0))
          ))
       ((or (= 6 approx) (= 360.0 approx) (= 350.1 approx))
        (let ((modulo (mod (approx-m midic 6) 100)))
          (cond ((= 33 modulo) 1) ((= 67 modulo) 2) (t 0))
          ))
       ((or (= 7 approx) (= 420.0 approx) (= 420.1 approx))
        (let ((modulo (mod (approx-m midic 7) 100)))
          (cond ((= 29 modulo) 1) ((= 57 modulo) 2) ((= 86 modulo) 3) 
                ((= 14 modulo) 4) ((= 43 modulo) 5) ((= 71 modulo) 6)(t 0))
          ))
       ((or (= approx 8) (= approx 480.0) (= approx 480.1)) 
        (let ((modulo (mod (approx-m midic 8) 100)))
          (cond ((= 25 modulo) 1) 
                ((= 50 modulo) 2) 
                ((= 75 modulo) 3) 
                (t 0))
          ))
       ((or (= 10 approx) 
          (= 100.0 approx) (= 100.1 approx) ;a voir
          (= 600.0 approx) (= 600.1 approx)
          )
        (let ((modulo (mod (approx-m midic 10) 100)))
          (cond ((= 20 modulo) 1) ((= 40 modulo) 2) ((= 60 modulo) 3)((= 80 modulo) 4)(t 0))
          ))
       ((or (= 12 approx) (= 720.0 approx) (= 720.1 approx))
        (let ((modulo (mod (approx-m midic 12) 100)))
          (cond ((= 17 modulo) 1) ((= 33 modulo) 2) 
                ((= 50 modulo) 3) ((= 67 modulo) 4)
                ((= 83 modulo) 5) (t 0))
          ))
       ((or (= 14 approx) (= 840.0 approx) (= 840.1 approx))
        (let ((modulo (mod (approx-m midic 14) 100)))
          (cond ((= 14 modulo) 1) ((= 29 modulo) 2) ((= 43 modulo) 3)((= 57 modulo) 4)
                ((= 71 modulo) 5)((= 86 modulo) 6) (t 0))
          ))
       ((or (= approx 16) (= approx 960.0) (= approx 960.1)) 
        (let ((modulo (mod (approx-m midic 16) 100)))
          (cond ((= 12 modulo) 1) 
                ((= 25 modulo) 2) 
                ((= 38 modulo) 3) 
                ((= 50 modulo) 4) 
                ((= 62 modulo) 5) 
                ((= 75 modulo) 6) 
                ((= 88 modulo) 7) 
                (t 0))
          ))
       ((or (= 80.1 approx) (= 80.0 approx))
       (let ((modulo (mod midic 100)))
         (if (= 50 modulo) 1 0)))
       ((or (= 90.1 approx) (= 90.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 33) 1)
                ((= modulo 67) 2)
                (t 0))))
       #|
       ((or (= 100.1 approx) (= 100.0 approx)
            (= 600.1 approx) (= 600.0 approx)
            );same as 10!
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 20) 1)
                ((= modulo 40) 2)
                ((= modulo 60) 3)
                ((= modulo 80) 4)
                (t 0))))
       |#
       ((or (= 140.1 approx) (= 140.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 86) 1)
                ((= modulo 71) 2)
                ((= modulo 57) 3)
                ((= modulo 43) 4)
                ((= modulo 29) 5)
                ((= modulo 14) 6)
                (t 0))))
       ((or (= 150.1 approx) (= 150.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 80) 1)
                ((= modulo 60) 2)
                ((= modulo 40) 3)
                ((= modulo 20) 4)
                (t 0))))
       ((or (= 160.1 approx) (= 160.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 75) 1)
                ((= modulo 50) 2)
                ((= modulo 25) 3)
                (t 0))))
       ((or (= 180.1 approx) (= 180.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 67) 1)
                ((= modulo 33) 2)
                (t 0))))
       #|
       ((or (= 220 approx) (= 220.1 approx) (= 200.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 55) 1)
                ((= modulo 9) 2)
                ((= modulo 64) 3)
                ((= modulo 18) 4)
                ((= modulo 73) 5)
                ((= modulo 27) 6)
                ((= modulo 82) 7)
                ((= modulo 36) 8)
                ((= modulo 91) 10)
                ((= modulo 45) 11)
                (t 0))))
       |#
       ((or (= 300.1 approx) (= 300.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 40) 1)
                ((= modulo 80) 2)
                ((= modulo 20) 3)
                ((= modulo 60) 4)
                (t 0))))
       ((or (= 360.1 approx) (= 360.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 33) 1)
                ((= modulo 67) 2)
                (t 0))))
       ((or (= 420.1 approx) (= 420.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 29) 1)
                ((= modulo 57) 2)
                ((= modulo 86) 3)
                ((= modulo 14) 4)
                ((= modulo 43) 5)
                ((= modulo 71) 6)
                (t 0))))
       ((or (= 720.1 approx) (= 720.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 17) 1)
                ((= modulo 33) 2)
                ((= modulo 50) 3)
                ((= modulo 67) 4)
                ((= modulo 83) 5)
                (t 0))))
       ((or (= 840.1 approx) (= 840.0 approx))
        (let ((modulo (mod midic 100)))
          (cond ((= modulo 14) 1)
                ((= modulo 29) 2)
                ((= modulo 43) 3)
                ((= modulo 57) 4)
                ((= modulo 71) 5)
                ((= modulo 86) 6)
                (t 0))))
       
       ((or (= approx 170) (= approx 170.1))
       (cond 
        ((or 
         (= (mod midic 100) 71)
         (= (mod midic 100) 82) 
         (= (mod midic 100) 94) 
         (= (mod midic 100) 76) 
         (= (mod midic 100) 88)) 
         1)
        ((= (mod midic 100) 41) 2)
        ((= (mod midic 100) 12) 3)
        ((= (mod midic 100) 53) 4)
        ((= (mod midic 100) 24) 5)
        ((= (mod midic 100) 65) 6)
        ((= (mod midic 100) 35) 7)
        ((= (mod midic 100) 6) 8)
        ((= (mod midic 100) 47) 10)
        ((= (mod midic 100) 18) 11)
        ((= (mod midic 100) 59) 12)
        ((= (mod midic 100) 29) 13)
        (t 0)
            ))
       ((= approx 190)
        (cond
        ((or 
          (= (mod midic 100) 63)
          (= (mod midic 100) 89) 
          (= (mod midic 100) 79) 
          (= (mod midic 100) 68) 
          (= (mod midic 100) 95)
          (= (mod midic 100) 84)
          (= (mod midic 100) 74))
         1)
         ((= (mod midic 100) 26) 2)
         ((= (mod midic 100) 53) 3)
         ((= (mod midic 100) 16) 4)
         ((= (mod midic 100) 42) 5)
         ((= (mod midic 100) 5) 6)
         ((= (mod midic 100) 32) 7)
         ((= (mod midic 100) 58) 8)
         ((= (mod midic 100) 21) 10)
         ((= (mod midic 100) 47) 11)
         ((= (mod midic 100) 11) 12)
         ((= (mod midic 100) 37) 13)
        (t 0)))
       ((or (= approx 220) (= approx 220.1))
       (cond 
        ((or 
         (= (mod midic 100) 55)
         (= (mod midic 100) 64) 
         (= (mod midic 100) 73) 
         (= (mod midic 100) 82) 
         (= (mod midic 100) 91)) 
         1)
        ((= (mod midic 100) 9) 2)
        ((= (mod midic 100) 18) 3)
        ((= (mod midic 100) 27) 4)
        ((= (mod midic 100) 36) 5)
        ((= (mod midic 100) 45) 6)
        ((= (mod midic 100) 35) 7)
        (t 0)
            ))
       ((= approx 310) 
       (cond 
        ((or 
         (= (mod midic 100) 39)
         (= (mod midic 100) 55) 
         (= (mod midic 100) 71) 
         (= (mod midic 100) 48) 
         (= (mod midic 100) 65)
         (= (mod midic 100) 42)
         (= (mod midic 100) 58)
         (= (mod midic 100) 74)
         (= (mod midic 100) 52)
         (= (mod midic 100) 68)
         (= (mod midic 100) 45)
         (= (mod midic 100) 61)) 
         1)
        ((or 
         (= (mod midic 100) 77)
         (= (mod midic 100) 94) 
         (= (mod midic 100) 87) 
         (= (mod midic 100) 81)
         (= (mod midic 100) 97) 
         (= (mod midic 100) 90) 
         (= (mod midic 100) 84))
         2)
        ((= (mod midic 100) 16) 3)
        ((= (mod midic 100) 32) 4)
        ((= (mod midic 100) 10) 5)
        ((= (mod midic 100) 26) 6)
        ((= (mod midic 100) 3) 7)
        ((= (mod midic 100) 19) 8)
        ((= (mod midic 100) 35) 10)
        ((= (mod midic 100) 13) 11)
        ((= (mod midic 100) 29) 12)
        ((= (mod midic 100) 6) 13)
        ((= (mod midic 100) 23) 14)
        (t 0)
        ))
       (t (let ((mod (if (= approx 4) 2 (/ 200 (or *micro-channel-approx* approx)))))
        (round (approx-m (mod midic 100) approx) mod)
        ))))



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











