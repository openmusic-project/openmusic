(in-package :cl-user)

(defpackage :juce)

;(fli:register-module 
; "OMJuceAudioLib" 
; :real-name "/Users/bouche/Documents/GIT/om7/OPENMUSIC/resources/lib/mac/OMJuceAudioLib.dylib"
; :connection-style :immediate)

(push :omjuceaudiolib *features*)

(in-package :juce)

;;;==============================================
;;  PLAYER
;;;==============================================
(cffi:defcfun ("OpenAudioPlayer" OpenAudioPlayer) :pointer (inchannels :int) (outchannels :int) (samplerate :int))

;(openaudioplayer 2 2 44100)

(cffi:defcfun ("CloseAudioPlayer" CloseAudioPlayer) :void (player :pointer))

(cffi:defcfun ("ChangeSampleRate" ChangeSampleRate) :void (player :pointer) (SR :int))

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
