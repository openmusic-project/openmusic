;===================================================
; MIDIFILE I/O
; AND CONNECTION WITH THE LOW-LEVEL MIDI SYSTEM
; ALL EVENST HERE ARE MIDI-EVT STRUCTS
;===================================================


(in-package :om)

(defun check-def-midi-system (function)
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
        (if substitute 
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

;= Saves sequence with tempo 60
;= modif  --->  clicks = 1000 so that 1 click = 1ms at tempo 60
 
(defun save-midifile (name object approx tempo &optional (format nil))
  (let ((seq (sort (flat (PrepareToPlay :midi object 0 :approx approx :voice 1))
                   'om-midi::midi-evt-<))
        (sys (check-def-midi-file-system 'om-midi::save-midi-file-function)))
    (if sys 
        (when seq
          (if (print tempo) 
              (setf seq (insert-tempo-info seq tempo))
            (push (om-midi::make-midi-evt :type :Tempo :date 0 :fields (list *midi-tempo*)) seq))
          (funcall (om-midi::save-midi-file-function sys) seq name (or format *def-midi-format*) 1000))
      (om-abort)
      )))


;;; JUST SEND A MIDI EVENT (for OM MIDI Player)

(defun midi-send-evt (evt)
  (let ((sys (check-def-midi-system 'om-midi::send-midi-event-function)))
    (when sys
      (let ((rep (funcall (om-midi::send-midi-event-function sys) evt)))
        (unless rep
          (print "Try to reconnect MIDI devices...")
          (funcall (om-midi::midi-connect-function sys)
                   (get-pref (find-pref-module :midi) :midi-setup))
          (funcall (om-midi::send-midi-event-function sys) evt))
        ))))

(defun midi-stop ()
  (let ((sys (check-def-midi-system 'om-midi::midi-stop-function)))
    (when sys (funcall (om-midi::midi-stop-function sys)))))

(defun midi-start ()
  (let ((sys (check-def-midi-system 'om-midi::midi-start-function)))
    (when sys (funcall (om-midi::midi-start-function sys)))))


;;; START/STOP MIDI IN

(defun midi-in-start (port fun bsize)
  (let ((sys (check-def-midi-system 'om-midi::midi-in-start-function)))
    (when sys (funcall (om-midi::midi-in-start-function sys) port fun bsize))))


(defun midi-in-stop (process)
  (let ((sys (check-def-midi-system 'om-midi::midi-in-stop-function)))
    (when sys (funcall (om-midi::midi-in-stop-function sys) process))))
