


(in-package :om-api)

(compile&load (make-pathname :directory (append oa::*externals-directory* (list "MIDI" "PortMidi")) :name "portmidi"))
(compile&load (make-pathname :directory (append oa::*externals-directory* (list "MIDI" "PortMidi")) :name "portmidi-api"))

(pushnew :portmidi *features*)

(om-add-init-func 'om-start-portmidi)






