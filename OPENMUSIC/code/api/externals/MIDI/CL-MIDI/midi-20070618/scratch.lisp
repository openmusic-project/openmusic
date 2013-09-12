(in-package :midi)

(setf f (read-midi-file (pathname "/home/andersvi/test.midi")))
 
(midifile-division f)
(midifile-format f)

(class-name (class-of (nth 12 (car (midifile-tracks f)))))

(class-of (nth 12 (car (midifile-tracks f))))

(typep (nth 12 (car (midifile-tracks f))) 'midi::note-on-message)

(clos::class-direct-subclasses )

(mapcar #'class-name (clos::class-precedence-list (find-class 'midi::pitch-bend-message)))
(mapcar #'class-name (clos::class-direct-subclasses  (find-class 'midi::mode-message)))

(POLY-MODE-ON-MESSAGE MONO-MODE-ON-MESSAGE OMNI-MODE-ON-MESSAGE OMNI-MODE-OFF-MESSAGE ALL-NOTES-OFF-MESSAGE LOCAL-CONTROL-MESSAGE RESET-ALL-CONTROLLERS-MESSAGE)

(PITCH-BEND-MESSAGE CHANNEL-PRESSURE-MESSAGE PROGRAM-CHANGE-MESSAGE CONTROL-CHANGE-MESSAGE POLYPHONIC-KEY-PRESSURE-MESSAGE NOTE-ON-MESSAGE NOTE-OFF-MESSAGE)

(lispworks-tools:class-browser)

(slot-value (nth 12 (car (midifile-tracks f))) 'channel)
(first (car (midifile-tracks f)))

(write-midi-file f "/tmp/ut.midi")

(make-instance note-on-message )

(print 'yo)

(ln)

(multiple-value-bind var-list values-form body...)


(defvar )

(defun lag-notelista ()
  (loop with note
     with kanal
     with tid = 0
     with rytme = 20
     with dyn
     repeat 100
     do (setf note (+ 40 (random 60))
	      kanal (random 16)
	      statuson (+ #x90 kanal)
	      statusoff (- statuson #x10)
	      dyn (+ 30 (random 80)))
     collect (make-instance 'note-on-message :key note :velocity dyn :time tid :status statuson)
     collect (make-instance 'note-off-message :key note :velocity 0 :time (incf tid rytme) :status statusoff)))

(progn
  (setf g (make-instance 'midifile :format 0 :division 480 :tracks (list (lag-notelista))))
  (write-midi-file g "/tmp/ut.midi")
  (sys:run-shell-command "timidity /tmp/ut.midi" :wait nil))



(write-track notelista)


(defun load-midi-file (name) 
  (let ((themidiFile (make-instance 'MidiFile))
	rep track-list err)
    (multiple-value-bind (err recording-seq nbtracks clicks format timedef)
        (om-midi-load-file (namestring name) (om-midi-new-seq))
      (unless (and err (zerop err)) (om-beep-msg (string+ "Error loading a MIDI file " (namestring name))) (om-abort))
      (om-print (string+ "Loading MIDI file: " (namestring name) " ..."))
      (when recording-seq
        (setf  (fileseq themidiFile) (convert-tempo-info recording-seq clicks))
        (setf track-list (make-list nbtracks :initial-element nil))
        (setf rep (mievents2midilist (fileseq themidiFile)))
	(loop for note in rep do
	     (if (plusp (third note))
		 (push (list (first note) (second note) (third note) (fourth note) (fifth note)) (nth (sixth note) track-list))))
        (setf (MidiFileName themidiFile) name)
        (setf (extent themidiFile) (loop for track in track-list 
				      if track maximize (+ (third (car track)) (second (car track)))))
        (setf (Qvalue themidiFile)  1000)
        (setf (tracks themidiFile) (loop for track in track-list 
				      if track collect  (make-instance 'MidiTrack
								       :midinotes  (reverse track))))
        themidifile))))
