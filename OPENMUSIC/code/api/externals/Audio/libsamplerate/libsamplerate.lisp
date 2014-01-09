(in-package :cl-user)

(defpackage "LibSampleRate"
  (:nicknames "LSR")
  (:use common-lisp cffi))

(in-package :lsr)

(push :libsamplerate *features*)

;;; MUST BE INSTALLED !
;;; or linked statically (in LibAudioStream / MacOS)

(defvar *libsamplerate-pathname* 
  #+win32
  "/WINDOWS/system32/libsamplerate.dll"
  #-win32
  "/Library/Frameworks/LibAudioStream.framework/LibAudioStream")

(defvar *libsamplerate* nil)

#+win32
(defun libsamplerate-framework ()
  (or *libsamplerate*
      (setq *libsamplerate*
            (if (probe-file *libsamplerate-pathname*)
                (progn 
                  (print (concatenate 'string "Loading libsamplerate library: " (namestring *libsamplerate-pathname*))) 
                  (fli:register-module "libsamplerate" 
                                       :real-name (namestring *libsamplerate-pathname*)
                                       :connection-style :immediate)
                  t)))))

#+linux
(progn
  (defparameter *libsamplerate* nil)

  (defun init-libsamplerate ()
    (define-foreign-library libsamplerate
      (t (:default "libsamplerate")))
    (setf *libsamplerate*
	  (handler-case (progn (use-foreign-library libsamplerate) t)
	    (error () (progn (print (format nil "could not load foreign-library libsamplerate"))
			     nil)))))
  (oa:om-add-init-func 'init-libsamplerate))

(defparameter SRC_SINC_BEST_QUALITY 0)
(defparameter SRC_SINC_MEDIUM_QUALITY 1)
(defparameter SRC_SINC_FASTEST 2)
(defparameter SRC_ZERO_ORDER_HOLD 3)
(defparameter SRC_LINEAR 4)

(defcstruct SRC_DATA 
  (data_in :pointer)
  (data_out :pointer)
  (input_frames :long)
  (output_frames :long)
  (input_frames_used :long)
  (output_frames_gen :long)
  (end_of_input :int)
  (src_ratio :double))

(defcstruct SRC_CB_DATA 
  (frames :long)
  (data_in :pointer))

(defcfun (src-simple "src_simple") :int 
         (src-data :pointer) 
         (converter-type :int) 
         (channels :int))

(defcfun (src-strerror "src_strerror") :string 
         (error :int))