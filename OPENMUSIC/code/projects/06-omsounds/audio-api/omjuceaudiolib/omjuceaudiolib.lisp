(in-package :cl-user)

(defpackage :juce)

;(fli:register-module 
; "OMJuceAudioLib" 
; :real-name "/Users/bouche/Documents/GIT/om7/OPENMUSIC/resources/lib/mac/OMJuceAudioLib.dylib"
; :connection-style :immediate)

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

(cffi:defcfun ("MakeBufferPointer" MakeBufferPointer) :pointer (buffer :pointer) (channels :int) (size :int) (sr :int))

(cffi:defcfun ("FreeBufferPointer" FreeBufferPointer) :void (buffer :pointer))

(cffi:defcfun ("PlayBuffer" PlayBuffer) :void (player :pointer) (buffer :pointer))

(cffi:defcfun ("PauseBuffer" PauseBuffer) :void (player :pointer) (buffer :pointer))

(cffi:defcfun ("StopBuffer" StopBuffer) :void (player :pointer) (buffer :pointer))

(cffi:defcfun ("SetPosBuffer" SetPosBuffer) :void (buffer :pointer) (pos :long))

(cffi:defcfun ("GetPosBuffer" GetPosBuffer) :long (buffer :pointer))

(cffi:defcfun ("LoopBuffer" LoopBuffer) :void (buffer :pointer) (looper :boolean))

;;;==============================================
;;  FILE
;;;==============================================

(cffi:defcfun ("MakeFilePointer" MakeFilePointer) :pointer (file :string))

(cffi:defcfun ("FreeFilePointer" FreeFilePointer) :void (file :pointer))

(cffi:defcfun ("PlayFile" PlayFile) :void (player :pointer) (file :pointer))

(cffi:defcfun ("PauseFile" PauseFile) :void (player :pointer) (file :pointer))

(cffi:defcfun ("StopFile" StopFile) :void (player :pointer) (file :pointer))

(cffi:defcfun ("SetPosFile" SetPosFile) :void (file :pointer) (pos :long))

(cffi:defcfun ("GetPosFile" GetPosFile) :long (file :pointer))

;(cffi:defcfun ("LoopFile" LoopFile) :void (file :pointer) (looper :boolean))
