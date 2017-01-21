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
(cffi:defcfun ("openAudioManager" openAudioManager) :pointer)
(cffi:defcfun ("closeAudioManager" closeAudioManager) :void (player :pointer))

(cffi:defcfun ("getDevicesTypeCount" getDevicesTypeCount) :int (player :pointer))
(cffi:defcfun ("getDeviceTypeName" getDeviceTypeName) :string (player :pointer) (type :int))
(cffi:defcfun ("setDeviceType" setDeviceType) :void (player :pointer) (type :string))
(cffi:defcfun ("getCurrentDeviceType" getCurrentDeviceType) :string (player :pointer))

(cffi:defcfun ("getInputDevicesCountForType" getInputDevicesCountForType) :int (player :pointer) (type :int))
(cffi:defcfun ("getOutputDevicesCountForType" getOutputDevicesCountForType) :int (player :pointer) (type :int))
(cffi:defcfun ("getNthInputDeviceName" getNthInputDeviceName) :string (player :pointer) (type :int) (n :int))
(cffi:defcfun ("getNthOutputDeviceName" getNthOutputDeviceName) :string (player :pointer) (type :int) (n :int))
(cffi:defcfun ("getInputDevicesCount" getInputDevicesCount) :int (player :pointer))
(cffi:defcfun ("getOutputDevicesCount" getOutputDevicesCount) :int (player :pointer))

(cffi:defcfun ("getInputChannelsCount" GetInputChannelsCount) :int (player :pointer))
(cffi:defcfun ("getOutputChannelsCount" GetOutputChannelsCount) :int (player :pointer))

(cffi:defcfun ("getAvailableSampleRatesCount" getAvailableSampleRatesCount) :int (player :pointer))
(cffi:defcfun ("getNthAvailableSampleRate" getNthAvailableSampleRate) :int (player :pointer) (n :int))
(cffi:defcfun ("getCurrentSampleRate" getCurrentSampleRate) :int (player :pointer))
(cffi:defcfun ("setSampleRate" setSampleRate) :int (player :pointer) (sr :int))

(cffi:defcfun ("getAvailableBufferSizesCount" getAvailableBufferSizesCount) :int (player :pointer))
(cffi:defcfun ("getNthAvailableBufferSize" getNthAvailableBufferSize) :int (player :pointer) (n :int))
(cffi:defcfun ("getCurrentBufferSize" getCurrentBufferSize) :int (player :pointer))
(cffi:defcfun ("getDefaultBufferSize" getDefaultBufferSize) :int (player :pointer))
(cffi:defcfun ("setBufferSize" setBufferSize) :int (player :pointer) (size :int))

(cffi:defcfun ("setAudioDevice" setAudioDevice) :void 
  (player :pointer) (output :int) (input :int)  (in-channels :int) (out-channels :int) (sr :int) (buffsize :int))

(cffi:defcfun ("setOutputChannelsMapping" setOutputChannelsMapping) :int (player :pointer) (n :int) (map :pointer))

;;; SCAN UTILITIES (INDEPENDENT ON THE CURRENT SETUP)
(defun get-audio-drivers (audiomanager)
  (let ((n-types (juce::getDevicesTypeCount audiomanager)))
    (loop for type from 0 to (1- n-types) collect
          (juce::getDeviceTypeName audiomanager type))))

(defun get-all-audio-output-devices (audiomanager)
  (let ((n-types (juce::getDevicesTypeCount audiomanager)))
    (loop for type from 0 to (1- n-types) append
          (let ((type-name (juce::getDeviceTypeName audiomanager type)))
            (loop for n from 0 to (1- (juce::getOutputDevicesCountForType audiomanager type)) 
                    collect (juce::getNthOutputDeviceName audiomanager type n)
                    )))))

(defun get-all-audio-input-devices (audiomanager)
  (let ((n-types (juce::getDevicesTypeCount audiomanager)))
    (loop for type from 0 to (1- n-types) append
          (let ((type-name (juce::getDeviceTypeName audiomanager type)))
            (loop for n from 0 to (1- (juce::getInputDevicesCountForType audiomanager type)) 
                    collect (juce::getNthInputDeviceName audiomanager type n)
                    )))))

(defun audio-driver-output-devices (audiomanager driver)
  (let ((type-num (position driver (get-audio-drivers audiomanager) :test 'string-equal)))
    (if type-num
        (loop for n from 0 to (1- (juce::getOutputDevicesCountForType audiomanager type-num)) 
              collect (juce::getNthOutputDeviceName audiomanager type-num n))
      (error "Audio driver ~S not found." driver))))

(defun audio-driver-input-devices (audiomanager driver)
  (let ((type-num (position driver (get-audio-drivers audiomanager) :test 'string-equal)))
    (if type-num
        (loop for n from 0 to (1- (juce::getInputDevicesCountForType audiomanager type-num)) 
              collect (juce::getNthInputDeviceName audiomanager type-num n))
      (error "Audio driver ~S not found." driver))))

(defun setoutputchannels (player activechannelslist)
  (let* ((l (length activechannelslist))
         (map (cffi:foreign-alloc :int :count l :initial-contents (mapcar '1- activechannelslist))))
    (unwind-protect 
        (setoutputchannelsmapping player l map)
      (cffi-sys:foreign-free map))))
      
(defun getinputchannelslist (player)
  (or (loop for i from 1 to (juce::GetInputChannelsCount player) collect i) '(0)))

(defun getoutputchannelslist (player)
  (or (loop for i from 1 to (juce::GetOutputChannelsCount player) collect i) '(0)))

(defun getsamplerates  (player)
  (loop for i from 0 to (1- (juce::getavailablesampleratescount player))
        collect (juce::getnthavailablesamplerate player i)))

(defun getbuffersizes  (player)
  (loop for i from 0 to (1- (juce::getavailablebuffersizescount player))
        collect (juce::getnthavailablebuffersize player i)))

;;; probleme abvec les caractères accentués !!
(defun setdevices (player input-device-name inch output-device-name outch sample-rate buffer-size)
  (let* ((driver (getCurrentDeviceType player))
         (in-n (or (position input-device-name 
                             (audio-driver-output-devices player driver) 
                             :test 'string-equal) 0))
         (out-n (or (position output-device-name 
                              (audio-driver-input-devices player driver) 
                              :test 'string-equal) 0)))
    (juce::setaudiodevice player in-n out-n inch outch sample-rate buffer-size)))

;(convert-string input-device-name)
;(cffi::lisp-string-to-foreign input-device-name str (1+ (length input-device-name)))
;(setf str (getNthOutputDeviceName om::*juce-player* 0 3))

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
(cffi:defcfun ("GetGainReader" GetGainReader) :float (reader :pointer))
(cffi:defcfun ("SetGainReader" SetGainReader) :void (reader :pointer) (gain :float))


