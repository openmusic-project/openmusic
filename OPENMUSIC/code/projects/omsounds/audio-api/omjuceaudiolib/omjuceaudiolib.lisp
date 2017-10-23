;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Connection with OMAudioLib 
; author: J. Bresson
;=========================================================================

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

(cffi:defcfun ("initializeAudioChannels" initializeAudioChannels) :void (player :pointer) (in-channels :int) (out-channels :int))
(cffi:defcfun ("getInputChannelsCount" GetInputChannelsCount) :int (player :pointer))
(cffi:defcfun ("getOutputChannelsCount" GetOutputChannelsCount) :int (player :pointer))
(cffi:defcfun ("setOutputChannelsMapping" setOutputChannelsMapping) :int (player :pointer) (n :int) (map :pointer))

(cffi:defcfun ("getDevicesTypeCount" getDevicesTypeCount) :int (player :pointer))
(cffi:defcfun ("getDeviceTypeName" getDeviceTypeName) :string (player :pointer) (type :int))
(cffi:defcfun ("setDeviceType" setDeviceType) :void (player :pointer) (type :string))
(cffi:defcfun ("getCurrentDeviceType" getCurrentDeviceType) :string (player :pointer))

(cffi:defcfun ("getInputDevicesCount" getInputDevicesCount) :int (player :pointer))
(cffi:defcfun ("getOutputDevicesCount" getOutputDevicesCount) :int (player :pointer))
(cffi:defcfun ("getInputDevicesCountForType" getInputDevicesCountForType) :int (player :pointer) (type :int))
(cffi:defcfun ("getOutputDevicesCountForType" getOutputDevicesCountForType) :int (player :pointer) (type :int))
(cffi:defcfun ("getNthInputDeviceName" getNthInputDeviceName) :string (player :pointer) (type :int) (n :int))
(cffi:defcfun ("getNthOutputDeviceName" getNthOutputDeviceName) :string (player :pointer) (type :int) (n :int))
(cffi:defcfun ("setInputDevice" setInputDevice) :int (player :pointer) (n :int))
(cffi:defcfun ("setOutputDevice" setOutputDevice) :int (player :pointer) (n :int))
(cffi:defcfun ("getCurrentDeviceName" getCurrentDeviceName) :string (player :pointer))

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
                             :test 'string-equal) -1))
         (out-n (or (position output-device-name 
                              (audio-driver-input-devices player driver) 
                              :test 'string-equal) -1)))
    (juce::setaudiodevice player in-n out-n inch outch sample-rate buffer-size)))

;(get-all-audio-output-devices om::*juce-player*)
;(convert-string input-device-name)
;(cffi::lisp-string-to-foreign input-device-name str (1+ (length input-device-name)))
;(setf str (getNthOutputDeviceName om::*juce-player* 0 3))
; (juce::GetOutputChannelsCount om::*juce-player*)
;;;==============================================
;;  BUFFER
;;;==============================================

(cffi:defcfun ("MakeDataReader" MakeDataReader) :pointer (buffer :pointer) (channels :int) (size :int) (sr :int))
(cffi:defcfun ("MakeFileReader" MakeFileReader) :pointer (file :string))
(cffi:defcfun ("FreeReader" FreeReader) :void (reader :pointer))
(cffi:defcfun ("StartReader" StartReader) :void (player :pointer) (reader :pointer))
(cffi:defcfun ("PauseReader" PauseReader) :void (player :pointer) (reader :pointer))
(cffi:defcfun ("StopReader" StopReader) :void (player :pointer) (reader :pointer))
(cffi:defcfun ("GetPosReader" GetPosReader) :long (reader :pointer))
(cffi:defcfun ("SetPosReader" SetPosReader) :void (reader :pointer) (pos :long))
(cffi:defcfun ("GetGainReader" GetGainReader) :float (reader :pointer))
(cffi:defcfun ("SetGainReader" SetGainReader) :void (reader :pointer) (gain :float))


