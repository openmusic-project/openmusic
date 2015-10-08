(in-package :oa)

;;;==============================
;;; MAIN LOAD EXTERNAL CALL
;;;==============================

(defparameter *externals-directory* (pathname-directory *load-pathname*))

(require :asdf)
;(handler-case (require :asdf)				    ;use what lw provides if it's there
;  (error () (load (make-pathname :directory (append *externals-directory* '("ASDF")) :name "asdf"))))

(load (make-pathname :directory (append *externals-directory* '("FFI")) :name "load-cffi"))
(load (make-pathname :directory (append *externals-directory* '("ieee-floats")) :name "ieee-floats"))

(defun load-external-libs (&optional libs)
  
  (loop for lib in libs do
        (unless (member lib (list :midi :midishare :portmidi :audio :opengl :sdif :osc :xml :jack :fluidsynth))
          (print (format nil "Library ~s can not be loaded" lib))))
  
  ;;; MINIMAL MIDI API IS ALWAYS LOADED
  (load (make-pathname :directory (append *externals-directory* '("MIDI")) :name "midi-api"))
  (when (member :midi libs)
    (load (make-pathname :directory (append *externals-directory* '("MIDI" "CL-MIDI")) :name "load-clmidi")))
  (when (member :midishare libs)
    (load (make-pathname :directory (append *externals-directory* '("MIDI" "MidiShare")) :name "load-midishare")))
  (when (member :portmidi libs)
    (load (make-pathname :directory (append *externals-directory* '("MIDI" "PortMidi")) :name "load-portmidi")))

  (when (member :audio libs)
    (load (make-pathname :directory (append *externals-directory* '("Audio")) :name "load-audio")))
  
  (when (member :opengl libs)
    (load (make-pathname :directory (append *externals-directory* '("OpenGL")) :name "load-opengl")))
  
  (when (member :sdif libs)
    (load (make-pathname :directory (append *externals-directory* '("SDIF")) :name "load-sdif")))
  
  (when (find :udp libs)
    (load (make-pathname :directory (append *externals-directory* '("lispworks-udp")) :name "lispworks-udp.asd"))
    (asdf:operate 'asdf:load-op 'lispworks-udp))

  (when (and (find :udp libs) (find :osc libs))
    (load (make-pathname :directory (append *externals-directory* '("OSC")) :name "om-osc.lisp")))
  
  (when (member :xml libs)
    (load (make-pathname :directory (append *externals-directory* '("XML")) :name "load-xml")))
  
  (when (member :json libs)
    (load (make-pathname :directory (append *externals-directory* '("Yason")) :name "package"))
    (load (make-pathname :directory (append *externals-directory* '("Yason")) :name "parse")))
  
  (when (find :jack libs)
    (load (make-pathname :directory (append *externals-directory* '("JACK")) :name "cl-jack-load")))
  
  (when (find :fluidsynth libs)
    (load (make-pathname :directory (append *externals-directory* '("FluidSynth")) :name "load-fluidsynth")))

  (when (member :svg libs)
    (load (make-pathname :directory (append *externals-directory* '("cl-svg")) :name "cl-svg.asd"))
    (asdf:load-system :cl-svg))
  
  t)
