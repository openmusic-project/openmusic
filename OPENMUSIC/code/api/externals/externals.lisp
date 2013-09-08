(in-package :oa)

;;;========================
;;; MIDI prototypes
;;;========================

(export '(
          *ms-setup-app*
          *om-midi-settings-app-path*
          *om-midi-settings-app-default-path*
          om-launch-ms-setup
          om-midi-startup
          om-midi-exit
          om-midi-get-time
          om-midi-open-player
          om-midi-close-player
          om-midi-get-num-from-type
          om-midi-symb2mtype
          om-midi-new-evt
          om-midi-copy-evt
          om-midi-send-evt
          om-midi-new-seq
          om-midi-seq-add-evt
          om-midi-seq-concat-evt
          om-midi-seq-first-evt
          om-midi-next-evt
          om-midi-get-evt-text
          om-midi-evt-get
          om-midi-evt-set
          om-midi-copy-seq
          om-midi-free-seq
          om-midi-save-seq-in-file
          om-midi-load-file
          
          om-midi-set-player 
          om-midi-start-player
          om-midi-pause-player 
          om-midi-cont-player 
          om-midi-stop-player 
          om-midi-record-player 
          om-midi-player-get-seq 
          om-midi-set-loop-player 
          om-midi-connect 
          om-midi-disconnect 
          ) :om-api)

(defun midi-prototypes ()
  (defvar *om-midi-settings-app-path* nil "the path of the midishare setup program")
  (defun om-launch-ms-setup (&key before after) (declare (ignore before after)) nil)
  (defun om-midi-startup () nil)
  (defun om-midi-exit () nil)
  (defmacro om-midi-get-time () nil)
  (defun om-midi-open-player (name) (declare (ignore name)) nil)
  (defun om-midi-close-player (player) (declare (ignore player)) nil)
  (defun om-midi-get-num-from-type (typestr) (declare (ignore typestr)) nil)
  (defun om-midi-symb2mtype (sym) (declare (ignore sym)) nil)
  (defun om-midi-new-evt (type &key port ref chan date vals pgm pitch kpress dur ctrlchange bend param tempo bytes) 
    (declare (ignore type port chan date vals pgm pitch kpress dur ctrlchange bend param tempo bytes)) nil)
  (defun om-midi-copy-event (event) (declare (ignore event)) nil)
  (defun om-midi-send-evt (event player) (declare (ignore event player)) nil)
  (defun om-midi-new-seq () nil)
  (defun om-midi-seq-add-evt (seq evt) (declare (ignore seq evt)) nil)
  (defun om-midi-seq-concat-evt (seq evt &optional (end t)) (declare (ignore seq evt end)) nil)
  (defun om-midi-free-seq (seq) (declare (ignore seq)) nil)
  (defun om-midi-copy-seq (seq &optional filtertest) (declare (ignore seq filtertest)) nil)
  (defun om-midi-seq-first-evt (seq) (declare (ignore seq)) nil)
  (defun om-midi-next-evt (evt) (declare (ignore evt)) nil)
  (defun om-midi-get-evt-text (evt) (declare (ignore evt)) nil)
  (defun om-midi-evt-get (msevent slot) (declare (ignore msevent slot)) nil)
  (defun om-midi-evt-set (msevent &key date dur port ref chan pgm param kpress bend tempo ctrlchange vals bytes field) 
    (declare (ignore msevent date dur port chan pgm param kpress ctrlchange vals bytes field)) nil)
  (defun om-midi-save-seq-in-file (seq filename &key (fileformat 1) (timedef 0) (clicks 1000) (tracks 1)) 
    (declare (ignore seq filename fileformat timedef clicks tracks)) nil)
  (defun om-midi-load-file (pathname sequence) (declare (ignore pathname sequence)) nil)
  
  (defun om-midi-set-player (player seq &optional (ticks 1000)))
  (defun om-midi-start-player (player) )
  (defun om-midi-pause-player (player) )
  (defun om-midi-cont-player (player) )
  (defun om-midi-stop-player (player) )
  (defun om-midi-record-player (player track) )
  (defun om-midi-player-get-seq (player) )
  (defun om-midi-set-loop-player (player start end))
  (defun om-midi-connect (src dest))
  (defun om-midi-disconnect (src dest))
)

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

;(export '(
;          *om-udp-max-buf-size*
;          om-make-send-connection
;          om-make-receive-connection
;          om-send-udp-packet
;          om-receive-udp-packet
;          om-close-udp-connection
;          ) :om-api)
;(defun udp-prototypes ()
;  (defvar *om-udp-max-buf-size* nil)
;  (defun om-make-send-connection (host port))
;  (defun om-make-receive-connection (port))
;  (defun om-send-udp-packet (connection data size))
;  (defun om-receive-udp-packet (connection buffer size))
;  (defun om-close-udp-connection (connection))
;)

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

;;;==============================
;;; SDIF prototypes
;;;==============================
;;; TEMP AUDIO API FOR OMSOUNDS

(defun sdif-prototypes ()
  (defpackage "SDIF-PACKAGE"
    (:nicknames "SDIF")
    (:use common-lisp)))
  

;;;==============================
;;; MAIN LOAD EXTERNAL CALL
;;;==============================

(defparameter *externals-directory* (pathname-directory *load-pathname*))

(defun load-external-libs (&optional libs)
  
  (load (make-pathname :directory (append *externals-directory* '("FFI")) :name "load-cffi"))
  (load (make-pathname :directory (append *externals-directory* '("ASDF")) :name "asdf"))

  (loop for lib in libs do
        (unless (member lib (list :midi :audio :opengl :sdif :osc :xml))
          (print (format nil "Library ~s can not be loaded" lib))))
  
  (if (member :midi libs)
      (load (make-pathname :directory (append *externals-directory* '("MIDI")) :name "load-midi"))
    (midi-prototypes))
  (if (member :audio libs)
      (load (make-pathname :directory (append *externals-directory* '("Audio")) :name "load-audio"))
    (audio-prototypes))
  (if (member :opengl libs)
      (load (make-pathname :directory (append *externals-directory* '("OpenGL")) :name "load-opengl"))
    )
  (if (member :sdif libs)
      (load (make-pathname :directory (append *externals-directory* '("SDIF")) :name "load-sdif"))
    )
  (when (member :udp libs)
      (load (make-pathname :directory (append *externals-directory* '("lispworks-udp")) :name "lispworks-udp.asd"))
      (asdf:operate 'asdf:load-op 'lispworks-udp)
      )
  (if (member :osc libs)
      (load (make-pathname :directory (append *externals-directory* '("OSC")) :name "load-osc.lisp"))
    (osc-prototypes))
  (if (member :xml libs)
        (load (make-pathname :directory (append *externals-directory* '("XML")) :name "load-xml")))
  (if (member :json libs)
      (progn
        (load (make-pathname :directory (append *externals-directory* '("Yason")) :name "package"))
        (load (make-pathname :directory (append *externals-directory* '("Yason")) :name "parse")))
    )
  t)