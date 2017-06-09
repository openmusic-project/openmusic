(in-package :om)

;===========================================
;Save objects in MidiFile (const tempo = 60)
;===========================================
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

(defmethod object-midi-tempo ((self t)) nil)

(defmethod object-midi-tempo ((self voice)) 
  (tempo-a-la-noire (car (tempo self))))

(defmethod object-midi-tempo ((self poly)) 
  (let ((tempo (poly-same-tempo self)))
    (if tempo (tempo-a-la-noire (car tempo)) nil)))

(defmethod midi-export ((self t) &key path name format approx retune-channels)
  (let ((pathname (or path (om-choose-new-file-dialog  :directory (def-save-directory) 
                                                       :name name
                                                       :prompt (om-str :save-as) 
                                                       :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi")))))
    (when pathname 
      (unless (stringp (pathname-type pathname))
	(setf pathname (make-pathname :device (pathname-device pathname)
				      :directory (pathname-directory pathname)
				      :name (pathname-name pathname)
				      :type "midi")))
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let ((tempo (object-midi-tempo self)))
        (when (save-midifile pathname self approx tempo (or format *def-midi-format*) retune-channels)
          (namestring pathname))))))



;;; OM BOX 

(defmethod* save-as-midi ((object t) &optional filename &key (approx 2) (format nil) retune-channels) 
  :initvals '(nil)
  :icon 900
  :doc "Saves <object> as a MIDI file.

- <filename> defines the target pathname. If not specified, will be asked through a file choose dialog.
- <approx> specifies the tone division (2, 4 or 8).
- <format> allows to choose the MIDIFile format (0 or 1)
- <retune-channels> (t or nil) send pitchbend message per channel to fit setting for approx

For POLY objects: If all voice have same tempo, this tempo is saved in MidiFile. Otherwise all voices are saved at tempo 60."
  (midi-export object
	       :path filename
	       :name (if filename (pathname-name filename) "midi-out")
	       :approx approx
	       :format format
	       :retune-channels retune-channels))







