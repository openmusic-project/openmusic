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

;===================================================
; MIDIFILE I/O
; AND CONNECTION WITH THE LOW-LEVEL MIDI SYSTEM
; ALL EVENST HERE ARE MIDI-EVT STRUCTS
;===================================================


(in-package :om)

(defun check-def-midi-system (function &optional (try-substitute t))
  (when (and *default-midi-system* (not (find *default-midi-system* om-midi::*midi-systems*)))
    (print (string+ "Warning: System " (string *default-midi-system*) " is not registered as a MID system.")))
  
  (if (or (null *default-midi-system*) 
          (null (funcall function *default-midi-system*)))
      ;;; TRY TO FIND ANOTHER ONE
      (let ((substitute (find-if function om-midi::*midi-systems*)))
        ;;; just for warning messages
        (cond ((null *default-midi-system*) 
               (print "No default MIDI system defined"))
              ((null (funcall function *default-midi-system*))
               (print (string+ "MIDI system " (string *default-midi-system*) " has no " (string function)))))
        (if (and try-substitute substitute)
            (and (print (string+ "MIDI system " (string substitute) " will be used instead"))
                 substitute)
          (om-beep)))
    *default-midi-system*)
  )


(defun check-def-midi-file-system (function)
  (when (and *default-midi-file-system* (not (find *default-midi-file-system* om-midi::*midi-file-systems*)))
    (print (string+ "Warning: System " (string *default-midi-file-system*) " is not registered as a MID system.")))
  
  (if (or (null *default-midi-file-system*) 
          (null (funcall function *default-midi-file-system*)))
      ;;; TRY TO FIND ANOTHER ONE
      (let ((substitute (find-if function om-midi::*midi-file-systems*)))
        ;;; just for warning messages
        (cond ((null *default-midi-file-system*) 
               (print "No default MIDI system defined"))
              ((null (funcall function *default-midi-file-system*))
               (print (string+ "MIDI system " (string *default-midi-file-system*) " has no " (string function)))))
        (if substitute 
            (and (print (string+ "MIDI system " (string substitute) " will be used instead"))
                 substitute)
          (om-beep)))
    *default-midi-file-system*)
  )

    
;;; LOADS THE MIDIFILE USING THE DEFAULT MIDI SYSTEM
;;; Expected return values are 
;;; (evtseq nbtracks clicks format)
;;; evtseq is a sequence of midi-evt
(defun midi-load-file (pathname)
  (let ((sys (check-def-midi-file-system 'om-midi::load-midi-file-function)))
    (if sys 
      (funcall (om-midi::load-midi-file-function sys) pathname)
      (om-abort)
      )))


(defun send-1/8-midi-evt ()
  (loop
       for chan in '(1 2 3 4 5 6 7 8 11 12 13 14)
       for pb in '(8192 9215 10239 11263 8192 9215 10239 11263)
       collect (om-midi::make-midi-evt
                :type :PitchBend
                :date 0
                :chan chan
                :fields (list pb))))

(defun send-1/4-midi-evt ()
  (loop
       for chan in '(1 3 2 4 5 7 6 8 9 11 12 14 13 15)
       for pb in '(8192 10239 8192 10239 8192 10239 8192 10239 8192 10239 8192 10239 8192 10239)
       collect (om-midi::make-midi-evt
                :type :PitchBend
                :date 0
                :chan chan
                :fields (list pb))))

(defun setup-retune-messages (approx)
  (cond 
   ((= approx 16)
    (loop
       for chan from 1 to 8
       for pb from 8192 by 512
       collect (om-midi::make-midi-evt
                :type :PitchBend
                :date 0
                :chan chan
                :fields (list pb))))
   ((= approx 8)
    (send-1/8-midi-evt))
   ((= approx 4)
    (send-1/4-midi-evt))
   (t nil)))


;= Saves sequence with tempo 60
;= modif  --->  clicks = 1000 so that 1 click = 1ms at tempo 60
 
(defun save-midifile (name object approx tempo &optional (format nil) retune-channels)
  (let ((seq (sort (remove nil (flat (PrepareToPlay :midi object 0 :approx approx :voice 1)))
                   'om-midi::midi-evt-<))
        (sys (check-def-midi-file-system 'om-midi::save-midi-file-function)))
    (if sys 
        (when seq
          (if tempo
              (setf seq (insert-tempo-info seq tempo))
	      (push (om-midi::make-midi-evt :type :Tempo :date 0 :fields (list *midi-tempo*)) seq))
	  
	  (when retune-channels
	    (setf seq (nconc (setup-retune-messages approx) seq)))
	  (funcall (om-midi::save-midi-file-function sys) seq name (or format *def-midi-format*) 1000))
	(om-abort))))


;;; JUST SEND A MIDI EVENT (for OM MIDI Player)

(defun midi-send-evt (evt)
  (let ((sys (check-def-midi-system 'om-midi::send-midi-event-function)))
    (when sys
      (let ((rep (funcall (om-midi::send-midi-event-function sys) evt)))
        (unless rep
          ;(print "[MIDI send failed]")
          ;(funcall (om-midi::midi-connect-function sys)
          ;         (get-pref (find-pref-module :midi) :midi-setup))
          ;(funcall (om-midi::send-midi-event-function sys) evt)
        )
        rep))))


;;; SEND MIDI RAW DATA (for OM MIDI Player)

(defun midi-send-bytes (bytes port)
  (let ((sys (check-def-midi-system 'om-midi::send-midi-data-function)))
    (when sys
      (let ((rep (funcall (om-midi::send-midi-data-function sys) bytes port)))
        (unless rep
          ;(print "[MIDI send failed]")
          ;(funcall (om-midi::midi-connect-function sys)
          ;         (get-pref (find-pref-module :midi) :midi-setup))
          ;(funcall (om-midi::send-midi-event-function sys) evt)
          )
        rep))))

(defun midi-stop ()
  (let ((sys (check-def-midi-system 'om-midi::midi-stop-function)))
    (when sys (funcall (om-midi::midi-stop-function sys)))))

(defun midi-start ()
  (let ((sys (check-def-midi-system 'om-midi::midi-start-function)))
    (when sys (funcall (om-midi::midi-start-function sys)))))


;;; START/STOP MIDI IN

(defun midi-in-start (port fun bsize redirect)
  (let ((sys (check-def-midi-system 'om-midi::midi-in-start-function nil)))
    (when sys (funcall (om-midi::midi-in-start-function sys) port fun bsize redirect))))


(defun midi-in-stop (process)
  (let ((sys (check-def-midi-system 'om-midi::midi-in-stop-function nil)))
    (when sys (funcall (om-midi::midi-in-stop-function sys) process))))


