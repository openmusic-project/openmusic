(in-package :oa)

;;;==============================
;;; AUDIO prototypes
;;;==============================

(export '(
          om-sound
          
          om-make-sound
          om-sound-file-name
          om-sound-n-samples
          om-sound-sample-rate
          om-sound-sample-size
          om-sound-n-channels
          om-sound-data-pos
          om-sound-format     
          om-sound-get-pict
          om-read-sound-data
          
          *om-player-sample-rate*
          om-start-audio
          om-open-audio-player
          om-close-audio-player
          om-add-sound-to-player
          om-start-audio-player
          om-pause-audio-player
          om-continue-audio-player
          om-stop-audio-player
          om-reset-audio-player
          om-set-audio-track-volume
          om-set-audio-track-pan
          om-play-one-sound
          ) :om-api)


(defun audio-prototypes () 
  (unless (find :om-audio-api *features*)
    (defclass om-sound ()())
    (defun om-make-sound (class name))
    (defmethod om-sound-file-name (sound))
    (defmethod om-sound-sample-rate (sound))
    (defmethod om-sound-sample-size (sound))
    (defmethod om-sound-n-samples (sound))
    (defmethod om-sound-n-channels (sound))
    (defmethod om-sound-data-pos (sound))
    (defmethod om-sound-format (sound))
    (defmethod om-read-sound-data (sound position &optional datatype))
    (defmethod om-sound-get-pict (sound))
  
    (defvar *om-player-sample-rate* nil)
    (defun om-start-audio ())
    (defun om-open-audio-player ())
    (defun om-close-audio-player (player))
    (defun om-add-sound-to-player (player sound at &optional start end (tracknum 1) (vol nil) (pan nil)))
    (defun om-reset-audio-player (player))
    (defun om-start-audio-player (player))
    (defun om-pause-audio-player (player))
    (defun om-continue-audio-player (player))
    (defun om-stop-audio-player (player))
    (defun om-set-audio-track-volume (player tracknum vol))
    (defun om-set-audio-track-pan (player tracknum pan))
    (defun om-play-one-sound (snd player))
    )
  )

(cl:defpackage "Audio"
    (:nicknames "AU")
    (:use common-lisp))

;;;==============================
;;; OSC prototypes
;;;==============================
#|
(export '(om-start-osc-server
          om-stop-osc-server
          om-send-osc-bundle
          om-decode-msg-or-bundle) :om-api)

(defun osc-prototypes ()
  (defun om-send-osc-bundle (port host bundle))
  (defun om-decode-msg-or-bundle (msg))
  (defun om-start-osc-server (port host fun))
  (defun om-stop-osc-server (server))
)
|#

;;;==============================
;;; SDIF prototypes
;;;==============================

(defun sdif-prototypes ()
  (defpackage "SDIF-PACKAGE"
    (:nicknames "SDIF")
    (:use common-lisp)))
  

;;;==============================
;;; MAIN LOAD EXTERNAL CALL
;;;==============================

(defparameter *externals-directory* (pathname-directory *load-pathname*))

(load (make-pathname :directory (append *externals-directory* '("ASDF")) :name "asdf"))
(load (make-pathname :directory (append *externals-directory* '("FFI")) :name "load-cffi"))
(load (make-pathname :directory (append *externals-directory* '("ieee-floats")) :name "ieee-floats"))

(defun load-external-libs (&optional libs)
  
  (loop for lib in libs do
        (unless (member lib (list :midi :midishare :audio :opengl :sdif :osc :xml :jack :fluidsynth))
          (print (format nil "Library ~s can not be loaded" lib))))
  
  ;;; MINIMAL MIDI API IS ALWAYS LOADED
  (load (make-pathname :directory (append *externals-directory* '("MIDI")) :name "midi-api"))
  
  ;;; CL-MIDI
  (if (member :midi libs)
      (load (make-pathname :directory (append *externals-directory* '("MIDI" "CL-MIDI")) :name "load-clmidi")))
  
  ;;; MIDISHARE
  (when (member :midishare libs)
      (load (make-pathname :directory (append *externals-directory* '("MIDI" "MidiShare")) :name "load-midishare")))

  (if (member :audio libs)
      (load (make-pathname :directory (append *externals-directory* '("Audio")) :name "load-audio"))
    (audio-prototypes))
  (if (member :opengl libs)
      (load (make-pathname :directory (append *externals-directory* '("OpenGL")) :name "load-opengl"))
    )
  (if (member :sdif libs)
      (load (make-pathname :directory (append *externals-directory* '("SDIF")) :name "load-sdif"))
    )
  (when (find :udp libs)
      (load (make-pathname :directory (append *externals-directory* '("lispworks-udp")) :name "lispworks-udp.asd"))
      (asdf:operate 'asdf:load-op 'lispworks-udp)
      )
  (if (and (find :udp libs) (find :osc libs))
      (load (make-pathname :directory (append *externals-directory* '("OSC")) :name "om-osc.lisp"))
    ;(osc-prototypes)
    )
  (if (member :xml libs)
        (load (make-pathname :directory (append *externals-directory* '("XML")) :name "load-xml")))
  (if (member :json libs)
      (progn
        (load (make-pathname :directory (append *externals-directory* '("Yason")) :name "package"))
        (load (make-pathname :directory (append *externals-directory* '("Yason")) :name "parse"))))
  (when (find :jack libs)
    (load (make-pathname :directory (append *externals-directory* '("JACK")) :name "cl-jack-load")))
  (when (find :fluidsynth libs)
    (load (make-pathname :directory (append *externals-directory* '("FluidSynth")) :name "load-fluidsynth")))
  t)
