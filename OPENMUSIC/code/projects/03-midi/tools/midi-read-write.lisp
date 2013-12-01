;===================================================
; MIDIFILE I/O
;===================================================


(in-package :om)

(defparameter *default-midi-system* nil)

(setf *default-midi-system* :cl-midi)
;(setf *default-midi-system* :midishare)

;;; LOADS THE MIDIFILE USING THE DEFAULT MIDI SYSTEM
;;; Expected return values are 
;;; (evtseq nbtracks clicks format)
;;; evtseq is a sequence of midi-evt
(defun midi-load-file (pathname)
  (let ((sys (or *default-midi-system*
                 (find-if 'om-midi::load-midi-file-function om-midi::*midi-systems*))))
    (when sys 
      (funcall (om-midi::load-midi-file-function sys) pathname))
    ))



(defvar *def-midi-format* 1)


;= Saves sequence with tempo 60
;= modif  --->  clicks = 1000 so that 1 click = 1ms at tempo 60

 
(defun save-midifile (name object approx tempo &optional (format nil))
  (let ((seq (sort (flat (PrepareToPlay :midi object 0 :approx approx :voice 1))
                   'midi-evt-<))
        (sys (or *default-midi-system*
                 (find-if 'om-midi::load-midi-file-function om-midi::*midi-systems*))))
    (unless sys
      (om-beep-msg "No MIDI system defined")
      (om-abort))
    (unless (om-midi::save-midi-file-function sys)
      (om-beep-msg (string+ "MIDI system " (sting sys) "has no MIDI-save function"))
      (om-abort))

    (when seq
      (if tempo 
          (setf seq (insert-tempo-info seq tempo))
        (push (make-midi-evt :type 'Tempo :date 0 :fields (list *midi-tempo*)) seq))
      (funcall (om-midi::save-midi-file-function sys) seq name (or format *def-midi-format*) 1000))
    ))


(om-midi::save-midi-file-function *default-midi-system*)





;===========================================
;Save objects in MidiFile (const tempo = 60)
;===========================================

(defmethod* save-as-midi ((object t) &optional filename &key  (approx 2) (format nil)) 
    :initvals '(nil) 
  :icon 900
  :doc "Saves <object> as a MIDI file.

- <filename> defines the target pathname. If not specified, will be asked through a file choose dialog.
- <approx> specifies the tone division (2, 4 or 8).
- <format> alows to choose the MIDIFile format (0 or 1)

For POLY objects: If all voice have same tempo, this tempo is saved in MidiFile. Else All voices are saved at tempo 60."
  (let ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog  :directory (def-save-directory) 
                                                                                 :prompt (om-str :save-as) 
                                                                                 :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi")))))
    (when name 
      (unless (stringp (pathname-type name))
          (setf name (make-pathname :device (pathname-device name)
                                    :directory (pathname-directory name)
                                    :name (pathname-name name)
                                    :type "midi")))
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
      (when (save-midifile name object approx (or format *def-midi-format*))
        (namestring name)
        ))))


(defmethod* save-as-midi ((object voice) &optional filename &key (approx 2) (format nil)) 
            (let ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog :directory (def-save-directory) 
                                                                                          :prompt (om-str :save-as) 
                                                                                          :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi")))))
              (when name 
                (unless (stringp (pathname-type name))
                  (setf name (make-pathname :device (pathname-device name)
                                            :directory (pathname-directory name)
                                            :name (pathname-name name)
                                            :type "midi")))
                (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
                (when (save-midifile name object approx (tempo-a-la-noire (car (tempo object))) (or format *def-midi-format*))
                  (namestring name)))))

;=======================================================
;== Save voice/poly in midifile                       ==
;== Considering tempo and time signature information ==
;=======================================================

;=== Tests if all voices of a ply object have the same tempo
;=== returns the tempo in case true and nil if not
(defmethod poly-same-tempo ((self poly))
  (let* ((alltempo (loop for voiceItem in (inside self) collect (tempo voiceItem)))
         (currtempo (car (first alltempo))))
    
    (loop for item in alltempo 
          while currtempo do
          
          (if (or (cadr item) ;; tempo changes in a voice
                  (not (= (/ (cadr (car item)) (car (car item)))
                          (/ (cadr currtempo) (car currtempo)))))
                  (setf currtempo nil) (setf currtempo (car item))))
    (list currtempo nil)))


(defmethod* save-as-midi ((object poly) &optional filename &key (approx 2) (format nil))
  (let* ((name (or (and filename (pathname filename)) (om-choose-new-file-dialog
                                                          :directory (def-save-directory) 
                                                          :prompt (om-str :save-as) 
                                                          :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi"))))
           (tempo (poly-same-tempo object)))
      (when name 
        (unless (stringp (pathname-type name))
          (setf name (make-pathname :device (pathname-device name)
                                    :directory (pathname-directory name)
                                    :name (pathname-name name)
                                    :type "midi")))
        (setf *last-saved-dir* (make-pathname :directory (pathname-directory name)))
        (when 
            (save-midifile name object approx 
                           (if tempo (tempo-a-la-noire (car tempo)) nil)
                           (or format *def-midi-format*)))  
        (namestring name))))










