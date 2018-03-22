;============================================================================
; o7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

(in-package :cl-user)

(defpackage :juce)

(push :omaudiolib *features*)

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
(cffi:defcfun ("getCurrentDeviceName" getCurrentDeviceName) :string (player :pointer))
(cffi:defcfun ("setInputDevice" setInputDevice) :int (player :pointer) (n :int))
(cffi:defcfun ("setOutputDevice" setOutputDevice) :int (player :pointer) (n :int))

(cffi:defcfun ("initializeAudioChannels" initializeAudioChannels) :void (player :pointer) (in-channels :int) (out-channels :int))
(cffi:defcfun ("getInputChannelsCount" GetInputChannelsCount) :int (player :pointer))
(cffi:defcfun ("getOutputChannelsCount" GetOutputChannelsCount) :int (player :pointer))
(cffi:defcfun ("setOutputChannelsMapping" setOutputChannelsMapping) :int (player :pointer) (n :int) (map :pointer))

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

(cffi:defcfun ("makeAudioSourceFromBuffer" makeAudioSourceFromBuffer) :pointer (buffer :pointer) (channels :int) (size :int) (sr :int))
(cffi:defcfun ("makeAudioSourceFromFile" makeAudioSourceFromFile) :pointer (file :string))
(cffi:defcfun ("freeAudioSource" freeAudioSource) :void (source :pointer))
(cffi:defcfun ("startAudioSource" startAudioSource) :void (player :pointer) (source :pointer))
(cffi:defcfun ("pauseAudioSource" pauseAudioSource) :void (player :pointer) (source :pointer))
(cffi:defcfun ("stopAudioSource" stopAudioSource) :void (player :pointer) (source :pointer))
(cffi:defcfun ("setAudioSourcePos" setAudioSourcePos) :void (source :pointer) (pos :long))
(cffi:defcfun ("getAudioSourcePos" getAudioSourcePos) :long (source :pointer))
(cffi:defcfun ("getAudioSourceGain" getAudioSourceGain) :float (source :pointer))
(cffi:defcfun ("setAudioSourceGain" setAudioSourceGain) :void (source :pointer) (gain :float))

;;;==============================================
;;  FILE I/O
;;;==============================================

(cffi:defcfun ("makeAudioFileReader" makeAudioFileReader) :pointer (file :string))
(cffi:defcfun ("freeAudioFileReader" freeAudioFileReader) :void (handler :pointer))

(cffi:defcfun ("getAudioFileNumChannels" getAudioFileNumChannels) :int (handler :pointer))
(cffi:defcfun ("getAudioFileNumSamples" getAudioFileNumSamples) :long (handler :pointer))
(cffi:defcfun ("getAudioFileSampleRate" getAudioFileSampleRate) :double (handler :pointer))
(cffi:defcfun ("getAudioFileSampleSize" getAudioFileSampleSize) :int (handler :pointer))
(cffi:defcfun ("getAudioFileFloatFormat" getAudioFileFloatFormat) :boolean (handler :pointer))
(cffi:defcfun ("getAudioFileFormat" getAudioFileFormat) :string (handler :pointer))

(cffi:defcfun ("getAudioFileSamples" getAudioFileSamples) :boolean (handler :pointer) (buffer :pointer) (startp :long-long) (nsamples :int))

(cffi:defcfun ("makeAudioFileWriter" makeAudioFileWriter) :pointer (file :string) (format :int))
(cffi:defcfun ("freeAudioFileWriter" freeAudioFileWriter) :void (handler :pointer))
(cffi:defcfun ("writeSamplesToAudioFile" writeSamplesToAudioFile) 
    :boolean 
  (handler :pointer) (buffer :pointer)
  (nch :int) (size :long-long) (sr :double) (resolution :int))



(defun juce-get-sound-info (path)
  (let ((fileptr (juce::makeAudioFileReader path)))
   (unless (fli:null-pointer-p fileptr)
     (unwind-protect 
         (let ((channels (juce::getAudioFileNumChannels fileptr))
               (size (juce::getAudioFileNumSamples fileptr))
               (sr (round (juce::getAudioFileSampleRate fileptr)))
               (ss (juce::getAudioFileSampleSize fileptr))
               (floats (juce::getAudioFileFloatFormat fileptr))
               (format (juce::getAudioFileFormat fileptr)))
           
          (values format channels sr ss size floats)
          )
       (juce::freeAudioFileReader fileptr)))))
 
(defun juce-get-sound-buffer (path &optional (datatype :float))
  (let ((fileptr (juce::makeAudioFileReader path)))
    (unless (fli:null-pointer-p fileptr)
      (unwind-protect 
          (let ((channels (juce::getAudioFileNumChannels fileptr))
                (size (juce::getAudioFileNumSamples fileptr))
                (sr (round (juce::getAudioFileSampleRate fileptr)))
                (ss (juce::getAudioFileSampleSize fileptr))
                (floats (juce::getAudioFileFloatFormat fileptr))
                (format (juce::getAudioFileFormat fileptr)))
            (let ((buffer (fli:allocate-foreign-object 
                           :type :pointer :nelems channels 
                           :initial-contents (loop for c from 0 to (1- channels) collect 
                                                   (fli::allocate-foreign-object 
                                                    :type datatype 
                                                    :nelems size)))))
              (juce::getAudioFileSamples fileptr buffer 0 size)
              (values buffer format channels sr ss size floats)
              ))
        (juce::freeAudioFileReader fileptr)))))


;;; format = :aiff, wav, :ogg, :flac ...
(defun juce-save-sound-in-file (buffer filename size nch sr resolution format &optional (datatype :float))
  (let* ((format_code (case format
                   (:wav 0)
                   (:aiff 1)
                   (otherwise 0)))
                   
         (fileptr (juce::makeAudioFileWriter filename format_code)))

    (unwind-protect 
        (juce::writeSamplesToAudioFile fileptr buffer nch size (coerce sr 'double-float) resolution)
      
      (juce::freeAudioFileWriter fileptr))))




