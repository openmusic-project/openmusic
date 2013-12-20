
(in-package :om-api)

(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "CL-MIDI" "midi-20070618")) :name "midi")) 
(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "CL-MIDI")) :name "clmidi-api"))

(pushnew :cl-midi *features*)
(pushnew :cl-midi om-midi::*midi-systems*)

(defmethod om-midi::load-midi-file-function ((midisystem (eql :cl-midi))) 'om-midi::cl-midi-load-file)
(defmethod om-midi::save-midi-file-function ((midisystem (eql :cl-midi))) 'om-midi::cl-midi-save-file)

(defmethod om-midi::send-midi-event-function ((midisystem (eql :cl-midi))) 'om-midi::cl-midi-send-evt)
