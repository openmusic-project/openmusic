(defpackage :cl-jack
  (:use :common-lisp :cffi)
  (:export "JACK-OPEN-SOUND" "PRINT-JACK-SF" "CL-JACK-PLAY-SOUND" "CL-JACK-CLOSE-SOUND" "JACKPLAY-TOGGLE-READ" "CL-JACK-SEEK"))
(in-package :cl-jack)


(defun compile?-and-load (file)
  (cl-user::compile-file-if-needed file)
  (load file))

(defparameter cl-jack-files '("cl-jack"
			      ;;"cl-jack-midi"  ;; leave while checking :portmidi
			      "cl-jack-audio"
			      "cl-jackplay-interleaved"))

(dolist (file cl-jack-files)
  (compile?-and-load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

;; various callbacks for audio and event(midi)-handling are defined in
;; the files above.  The callback-function can aswell be changed on
;; the fly.

(defun cl-jack-init-everything ()
  (print (format nil "Initializing cl-jack..."))
  (cl-jack-init-jack)
  #+cl-jack-midi (cl-jack-init-midi)
  (cl-jack-init-audio)
  (jack-set-process-callback *CLJackClient* (callback cl-jack-process-callback) 0)
  (jack-activate *CLJackClient*)
  (cl-jack-connect-audio-client-to-system-output)
  #+:cl-jack-midi (pushnew :cl-jack om-midi::*midi-systems*)
  (print (format nil "Init cl-jack: done")))

;;(cl-jack-init-everything)
(oa::om-add-init-func 'cl-jack-init-everything)

(pushnew :cl-jack *features*)
(provide :cl-jack)

