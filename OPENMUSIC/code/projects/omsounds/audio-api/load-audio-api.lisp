;;===========================================================================
; OM AUDIO API 
;;===========================================================================
(in-package :cl-user)

(compile&load (decode-local-path "libsndfile/libsndfile"))
(compile&load (decode-local-path "libsndfile/libsndfile-api"))
(compile&load (decode-local-path "file-access"))
(compile&load (decode-local-path "libsamplerate/libsamplerate"))
(compile&load (decode-local-path "libsamplerate/libsamplerate-api"))
(compile&load (decode-local-path "omjuceaudiolib/omjuceaudiolib"))


(push :audio *features*)






;;; TODO CLEANUP !!!
#-macosx
(defun init-libsndfile ()
  (cffi:define-foreign-library libsndfile
    (:darwin "libsndfile.dylib")
    #+win32(:unix (:or "cygsndfile-1.dll" "libsndfile.so.1" "libsndfile.so"))
    (t (:default #+win32 "libsndfile-1" #-win32 "libsndfile")))
  (handler-case 
      (cffi:use-foreign-library libsndfile)
    (error () (progn 
                (print (format nil "could not load foreign-library libsndfile"))
                nil))))

#+macosx
(defun init-libsndfile ()
  (let ((libpath (namestring (om::om-lib-pathname "libsndfile.dylib"))))
    (if (probe-file libpath)
        (progn
          (print (format nil "Loading libsndfile library: ~A" libpath))
          (handler-case 
              (fli:register-module "libsndfile" 
                                   :real-name libpath
                                   :connection-style :immediate)
            (error () (format nil "could not load foreign-library ~A" libpath))))
      (print (format nil "could not find foreign-library ~A" libpath)))))

(oa:om-add-init-func 'init-libsndfile)

#-macosx
(defun init-libsamplerate ()
  (cffi:define-foreign-library libsamplerate
    (:darwin "libsamplerate.dylib")
    (:unix (:or "libsamplerate.dll" "libsamplerate.so"))
    (t (:default "libsamplerate")))
  (handler-case 
      (cffi:use-foreign-library libsamplerate)
    (error () (progn 
                (print (format nil "could not load foreign-library libsamplerate"))
                nil)))
  )

#+macosx
(defun init-libsamplerate ()
  (let ((libpath (namestring (om::om-lib-pathname "libsamplerate.dylib"))))
    (if (probe-file libpath)
        (progn
          (print (format nil "Loading libsamplerate library: ~A" libpath))
          (handler-case 
              (fli:register-module "libsamplerate" 
                                   :real-name libpath
                                   :connection-style :immediate)
            (error () (format nil "could not load foreign-library ~A" libpath))))
      (print (format nil "could not find foreign-library ~A" libpath)))))

(oa:om-add-init-func 'init-libsamplerate)



(defparameter *juceaudiolib-pathname*
  #+win32
  "/WINDOWS/system32/OMJuceAudioLib.dll"
  #+(or darwin macos macosx)  
  "OM6/OPENMUSIC/resources/lib/mac/OMJuceAudioLib.dylib"
  #+(and :linux :x86-64) "OMJuceAudioLib.so"
  #+(and :linux :x86) "/usr/lib/OMJuceAudioLib.so"
  )

(defun load-juceaudiolib ()
  (let ((libpath (namestring (om::om-lib-pathname *juceaudiolib-pathname*))))
    #-linux (if (probe-file libpath)
		(progn 
		  (print (concatenate 'string "Loading Juce Audio library: " libpath))
		  (fli:register-module "JuceAudio" 
				       :real-name libpath
				       :connection-style :immediate)
		  t)
		(print (concatenate 'string "Juce Audio library not found: " libpath)))
    #+linux (progn 
	      (print (concatenate 'string "Loading Juce Audio library: " libpath))
	      (fli:register-module "JuceAudio" 
				   :real-name libpath
				   :connection-style :immediate)
	      libpath)))

(om::om-add-init-func 'load-juceaudiolib)



