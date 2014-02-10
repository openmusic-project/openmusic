;===================================================
; MIDIFILE I/O
; AND CONNECTION WITH THE LOW-LEVEL MIDI SYSTEM
; ALL EVENST HERE ARE MIDI-EVT STRUCTS
;===================================================


(in-package :om)

(defun ckeck-def-midi-system (function)
  (when (and *default-midi-system* (not (find *default-midi-system* om-midi::*midi-systems*)))
    (print (string+ "Warning: System " (string *default-midi-system*) "is not registered as a MID system.")))
  
  (if (or (null *default-midi-system*) 
          (null (funcall function *default-midi-system*)))
      ;;; TRY TO FIND ANOTHER ONE
      (let ((substitute (find-if function om-midi::*midi-systems*)))
        ;;; just for warning messages
        (cond ((null *default-midi-system*) 
               (print "No default MIDI system defined"))
              ((null (funcall function *default-midi-system*))
               (print (string+ "MIDI system " (string *default-midi-system*) " has no " (string function)))))
        (if substitute 
            (and (print (string+ "MIDI system " (string substitute) " will be used instead"))
                 substitute)
          (om-beep)))
    *default-midi-system*)
  )
    
;;; LOADS THE MIDIFILE USING THE DEFAULT MIDI SYSTEM
;;; Expected return values are 
;;; (evtseq nbtracks clicks format)
;;; evtseq is a sequence of midi-evt
(defun midi-load-file (pathname)
  (let ((sys (ckeck-def-midi-system 'om-midi::load-midi-file-function)))
    (if sys 
      (funcall (om-midi::load-midi-file-function sys) pathname)
      (om-abort)
      )))

;= Saves sequence with tempo 60
;= modif  --->  clicks = 1000 so that 1 click = 1ms at tempo 60
 
(defun save-midifile (name object approx tempo &optional (format nil))
  (let ((seq (sort (flat (PrepareToPlay :midi object 0 :approx approx :voice 1))
                   'om-midi::midi-evt-<))
        (sys (ckeck-def-midi-system 'om-midi::save-midi-file-function)))
    (if sys 
        (when seq
          (if (print tempo) 
              (setf seq (insert-tempo-info seq tempo))
            (push (om-midi::make-midi-evt :type 'om-midi::Tempo :date 0 :fields (list *midi-tempo*)) seq))
          (funcall (om-midi::save-midi-file-function sys) seq name (or format *def-midi-format*) 1000))
      (om-abort)
      )))


;;; JUST SEND A MIDI EVENT
;;; !!! REQUIRES A PLAYER

(defun midi-send-evt (evt)
  (let ((sys (ckeck-def-midi-system 'om-midi::send-midi-event-function)))
    (when sys (funcall (om-midi::send-midi-event-function sys) evt))))





