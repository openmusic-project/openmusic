(defpackage :cl-jack
  (:use :common-lisp :cffi)
  (:export "JACK-OPEN-SOUND" "PRINT-JACK-SF" "CL-JACK-PLAY-SOUND" "CL-JACK-CLOSE-SOUND" "JACKPLAY-TOGGLE-READ" "CL-JACK-SEEK"))
;(in-package :cl-jack)

(pushnew :cl-jack *features*)
(provide :cl-jack)

