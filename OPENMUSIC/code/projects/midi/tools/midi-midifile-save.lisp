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

;===========================================
;Save VOICE and POLY in MidiFile
;===========================================
;=======================================================
;== Save voice/poly in midifile                      ==
;== Considering tempo and time signature information ==
;=======================================================

;;;;preparetoSave
;;(modifies version of preparetoplay

(defmethod PrepareToSave ((player t) (self container) at &key approx port interval voice) 
  (remove nil
          (loop for sub in (inside self) collect
                (let ((objstart (+ at (offset->ms sub))))
                  (let ((in-interval (or (null interval)
                                         (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub)))))))
                    (when in-interval 
                      (PrepareToSave player sub objstart 
                                     :approx approx 
                                     :port port
                                     :interval interval
                                     :voice voice)))))))


(defmethod PrepareToSave ((player (eql :midi)) (self poly) at &key approx port interval voice)
  ;(setf port (verify-port port))
  (loop for sub in (inside self) 
        for v = 1 then (+ v 1) collect
        (let* ((objstart (+ at (offset->ms sub)))
               (in-interval (or (null interval)
                               (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub)))))))
          (when in-interval
            (PrepareToSave player sub objstart 
                               :approx approx 
                               :port port
                               :interval interval
                               :voice v)
            ))))

(defmethod PrepareToSave ((player (eql :midi)) (self measure) at &key approx port interval voice)
  ;(setf port (verify-port port))
             (remove nil
                  (loop for sub in (inside self) collect
                        (let ((objstart (+ at (offset->ms sub))))
                          (let ((in-interval (or (null interval)
                                                 (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub)))))))
                            (when in-interval
                              (PrepareToSave player sub objstart 
                                             :approx approx 
                                             :port port
                                             :interval interval
                                             :voice voice)))))))

(defmethod PrepareToSave ((player (eql :midi)) (self note) at &key  approx port interval voice)
  (when (and (not (memq (tie self) '(continue end)))
             (>= (midic self) 0)) ;; otherwise MIDI generates an error
    (setf port (or port (port self)))
    (setf approx (or approx 2))
    (let ((channel-shift (micro-channel (approx-m (midic self) (round approx)) approx)))
      (let ((chan (+ (chan self) channel-shift))
            (pitch (truncate (approx-scale (get-current-scale approx) (midic self)) 100))
            (vel (vel self))
            (dur (real-dur self))
            (date at)					
            (voice (or voice 0)))
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

(defmethod PrepareToSave ((player (eql :midi)) (self rest) at &key  approx port interval voice)
  nil)



;;;;;;;;;;;

(defmethod voice->notempovoice ((self voice))
  (let ((clone (clone self)))
    (setf (tempo clone) 60)
    clone))

(defmethod voice->notempovoice ((self poly))
  (let* ((clone (clone self))
         (voices (inside clone)))
    (mapcar #'voice->notempovoice voices)
    (make-instance 'poly 
                   :voices (mapcar #'voice->notempovoice voices))))

;;;formating

(defun construct-ts (meas onsets)
  (loop for i in meas
        for on in onsets
          collect (om-midi::make-midi-evt :type :TimeSign :date on :ref 0  :fields (list (car i) (round (log (second i) 2)) 24 8) :port 0)))

(defun tempofmeas (tempo)
  "Returns only measure index and BPM"
  (loop for i in (cadr-tempo tempo)
          collect (list (caar i) (second (cadr i)))))

(defun construct-tempo (lst)
  (loop for i in lst
          collect (om-midi::make-midi-evt :type :Tempo :date (car i) :ref 0 :fields (list (bpm2mstempo (second i))))))

;;;;

(defun set-tempo-map (object newobj) 
  (let* ((onsets (loop for i in (inside newobj)
                       collect (offset->ms i)))
         (tempo (tempo object))
         (measures (get-time-sig object))
         (meas (construct-ts measures onsets))
         (tempos (loop for i in (tempofmeas tempo)
                       collect (list (nth (car i) onsets) (second i))))
         (tempi (construct-tempo tempos)))    
    (sort (x-append tempi meas) 'om-midi::midi-evt-<)
    ))
    

; Remplaces save-midile function (in midi-connections.lisp)

(defmethod* om-save-midifile ((name pathname) (object voice) &optional (approx 2) (format nil) retune-channels)
  (let* ((newobj (voice->notempovoice object))
         (seq (sort (remove nil (flat (PrepareToSave :midi newobj 0 :approx approx :voice 1)))
                    'om-midi::midi-evt-<))
         (sys (check-def-midi-file-system 'om-midi::save-midi-file-function)))
    (if sys 
        (when seq
          (setf seq (x-append (set-tempo-map object newobj) seq))
          (when retune-channels
	    (setf seq (nconc (setup-retune-messages approx) seq)))
	  (funcall (om-midi::save-midi-file-function sys) seq name (or format *def-midi-format*) 1000)))
	(om-abort)))


;assuming no polymetrics, no polytempi

(defmethod* om-save-midifile ((name pathname) (object poly) &optional (approx 2) (format nil) retune-channels)
  (let* ((newobj (voice->notempovoice object))
         (seq (sort (remove nil (flat (PrepareToSave :midi newobj 0 :approx approx :voice 1)))
                    'om-midi::midi-evt-<))
         (sys (check-def-midi-file-system 'om-midi::save-midi-file-function)))
    (if sys 
        (when seq
          (setf seq (x-append (set-tempo-map (car (inside object)) (car (inside newobj))) seq))
          (when retune-channels
	    (setf seq (nconc (setup-retune-messages approx) seq)))
	  (funcall (om-midi::save-midi-file-function sys) seq name (or format *def-midi-format*) 1000)))
	(om-abort)))


;directs to save-midifile for other objects

(defmethod* om-save-midifile ((name pathname) (object t) &optional (approx 2) (format nil) retune-channels)
  (save-midifile name object approx nil (or format *def-midi-format*) retune-channels))