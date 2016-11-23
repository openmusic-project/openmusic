(in-package :cl-user)

(defpackage :juce)

(push :omjuceaudiolib *features*)

(in-package :juce)

(defparameter *juceaudiolib-pathname*
  #+win32
  "/WINDOWS/system32/OMJuceAudioLib.dll"
  #+(or darwin macos macosx)  
  "OM6/OPENMUSIC/resources/lib/mac/OMJuceAudioLib.dylib"
  #+(or linux (and clisp unix (not macos)))
  "/usr/lib/OMJuceAudioLib.so")

(defun load-juceaudiolib ()
  (let ((libpath (namestring (om::om-lib-pathname *juceaudiolib-pathname*))))
    (if (probe-file libpath)
        (progn 
          (print (concatenate 'string "Loading Juce Audio library: " libpath))
          (fli:register-module "JuceAudio" 
                               :real-name libpath
                               :connection-style :immediate)
          t)
      (print (concatenate 'string "Juce Audio library not found: " libpath)))))

(om::om-add-init-func 'load-juceaudiolib)


;;;==============================================
;;  PLAYER
;;;==============================================
(cffi:defcfun ("OpenAudioPlayer" OpenAudioPlayer) :pointer (inchannels :int) (outchannels :int) (samplerate :int))

;(openaudioplayer 2 2 44100)

(cffi:defcfun ("CloseAudioPlayer" CloseAudioPlayer) :void (player :pointer))

;;;==============================================
;;  BUFFER
;;;==============================================

(cffi:defcfun ("MakeDataReader" MakeDataReader) :pointer (buffer :pointer) (channels :int) (size :int) (sr :int))
(cffi:defcfun ("MakeFileReader" MakeFileReader) :pointer (file :string))

(cffi:defcfun ("FreeReader" FreeReader) :void (reader :pointer))

(cffi:defcfun ("StartReader" StartReader) :void (player :pointer) (reader :pointer))

(cffi:defcfun ("PauseReader" PauseReader) :void (player :pointer) (reader :pointer))

(cffi:defcfun ("StopReader" StopReader) :void (player :pointer) (reader :pointer))

(cffi:defcfun ("SetPosReader" SetPosReader) :void (reader :pointer) (pos :long))

(cffi:defcfun ("GetPosReader" GetPosReader) :long (reader :pointer))

(cffi:defcfun ("LoopReader" LoopReader) :void (reader :pointer) (looper :boolean))

