
(in-package :om-api)

(compile&load (make-pathname :directory (append oa::*externals-directory* (list "MIDI" "PortMidi")) :name "portmidi"))
(compile&load (make-pathname :directory (append oa::*externals-directory* (list "MIDI" "PortMidi")) :name "portmidi-api"))
(compile&load (make-pathname :directory (append oa::*externals-directory* (list "MIDI" "PortMidi")) :name "portmidi-setup"))

(pushnew :portmidi *features*)

(defmethod om-midi::send-midi-event-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-send-evt)
(defmethod om-midi::midi-stop-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-stop)
(defmethod om-midi::midi-start-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-start)

(defmethod om-midi::midi-setup-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-setup)
(defmethod om-midi::midi-connect-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-connect-ports)
(defmethod om-midi::midi-restart-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-restart)

(defmethod om-midi::midi-in-start-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-in-start)
(defmethod om-midi::midi-in-stop-function ((midisystem (eql :portmidi))) 'om-midi::portmidi-in-stop)

(om-add-init-func 'om-midi::om-start-portmidi)






