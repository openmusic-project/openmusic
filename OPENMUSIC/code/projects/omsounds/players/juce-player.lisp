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
;===========================================================================


;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING JUCE
;;; Author: D. Bouche
;;;===========================================

(in-package :om)

;Constants to use to create players.
(defconstant *audio-buffsize* 512)

(defvar *juce-player* nil)
(setf *audio-driver* 
  #+macosx "CoreAudio" 
  #+linux "ALSA"
  #+mswindows "DirectSound"
  )

(defmethod player-name ((self (eql :om-audio))) "OM inbuilt audio player")
(defmethod player-desc ((self (eql :om-audio))) "")
(enable-player :om-audio)
(add-player-for-object 'sound :om-audio)


;;; not used anymore...
(defun juce-player-setup-with-check (player)
  (unless *audio-driver* 
    (setf *audio-driver* (car (juce::get-audio-drivers player)))
    (om-print (format nil "No driver set for audio. Selecting default: '~A'." *audio-driver*)))
  (if *audio-driver*
      (let ((out-devices (juce::audio-driver-output-devices player *audio-driver*)))
        (if out-devices
            (let ()
               ;;; try with the recorded device
               (om-print (format nil "AUDIO PLAYER SETUP: ~A x ~A, ~AHz" *audio-out-device* *audio-out-n-channels* *audio-sr*))
               
               (let* ((device-names (juce::audio-driver-output-devices player *audio-driver*))
                      (out-device-index (position *audio-out-device* device-names)))
                 
                 (if out-device-index
                     
                     (juce::setoutputdevice player out-device-index)
                   
                   (progn 
                     (om-beep-msg (format nil "Selected audio device: '~A' not available.~%Restoring default: '~A'" 
                                          *audio-out-device* (juce::getCurrentDeviceName player)))
                     (setf *audio-out-device* (juce::getCurrentDeviceName player)))))

               ;;; Channels
               (setf *audio-out-chan-options* (juce::getoutputchannelslist player))

               (unless (find *audio-out-n-channels* *audio-out-chan-options*)
                 (setf *audio-out-n-channels* (or (car (last *audio-out-chan-options*)) 2))
                 (om-print (format nil "Corrected num channels: ~A" *audio-out-n-channels*)))

               (juce::initializeaudiochannels player 0 *audio-out-n-channels*)
               
               ;;; Sample rate
               (setf *audio-sr-options* (juce::getsamplerates player))
               
               (unless (find *audio-sr* *audio-sr-options*)
                 (setf *audio-sr* (or (car *audio-sr-options*) 44100))
                 (om-print (format nil "Corrected sample rate: ~A" *audio-sr*)))
               
               (juce::setsamplerate player *audio-sr*)
               
               ;;; Buffer size 
               (when (find *audio-buffsize* (juce::getbuffersizes player))
                 (juce::setbuffersize player *audio-buffsize*))
               )
          
          (om-beep-msg (format nil "ERROR OPENING AUDIO: No audio device found with driver '~A'." *audio-driver*)))
        )
    (om-beep-msg "ERROR OPENING AUDIO: Could not find any audio driver.")
    ))

; (juce::get-audio-drivers *juce-player*)
; (juce::getCurrentDeviceType *juce-player*)
; (juce::setDeviceType *juce-player* "DirectSound")
;"Speaker/HP (Realtek High Definition Audio)"

(defun juce-player-setup (player)

  (om-print "Audio Setup...")

  (let ((drivers (juce::get-audio-drivers player)))
    (unless (find *audio-driver* drivers :test 'string-equal)
      (om-print (format nil "  Warning: Device type '~A' not available.~%Will use '~A' as default..." 
                     *audio-driver* (car drivers)))
      (setf *audio-driver* (car drivers))))
  
  (unless (string-equal (juce::getCurrentDeviceType player) *audio-driver*)
    (om-print (format nil "  Current driver: ~A" (juce::getCurrentDeviceType player)))
    (juce::setDeviceType player *audio-driver*)
    (om-print (format nil "  Setting audio driver: ~A" *audio-driver*)))
  (om-print (format nil "  Audio driver: ~A" (juce::getCurrentDeviceType player)))
  
  (let ((out-devices (juce::audio-driver-output-devices player (juce::getCurrentDeviceType player))))
    (if (and *audio-out-device* (not (string-equal *audio-out-device* "")))
      (progn 
        (om-print (format nil "  Selected device: ~A" *audio-out-device*)) 
        (if (find *audio-out-device* out-devices :test 'string-equal)
            (juce::setoutputdevice player (position *audio-out-device* out-devices :test 'string-equal))
          (om-print (format nil "  => not found in available devices: ~A" out-devices)))
        )
      (progn 
        (om-print (format nil "  Selecting default device: ~A" (car out-devices)))
        (setf *audio-out-device* (car out-devices))
        (juce::setoutputdevice player 0))))
  
  ;#-linux (setf *audio-out-device* (juce::getCurrentDeviceName player))
  ;(om-print (format nil "Audio initialized with: '~A'" *audio-out-device*))  
  
  (setf *audio-out-chan-options* (juce::getoutputchannelslist player))
  (setf *audio-sr-options* (juce::getsamplerates player))
  
  (juce::setsamplerate player *audio-sr*)
  (setf *audio-sr* (juce::getcurrentsamplerate player)) 

  (unless (find *audio-out-n-channels* *audio-out-chan-options*)
    (setf *audio-out-n-channels* (or (car (last *audio-out-chan-options*)) 2))
    (om-print (format nil "  Restoring default output channels (~A)" *audio-out-n-channels*)))
  
  (om-print (format nil "  Initializing audio channels (~Ax~A)" 0 *audio-out-n-channels*))
  (juce::initializeaudiochannels *juce-player* 0 *audio-out-n-channels*)
 
  t)
 

;; called from preferences
(defmethod player-get-drivers ((player (eql :om-audio)))
  (when *juce-player* (juce::get-audio-drivers *juce-player*)))

(defmethod player-get-devices ((player (eql :om-audio)))
  (when *juce-player* (juce::audio-driver-output-devices *juce-player* *audio-driver*)))
 
;;; called from preferences
(defmethod player-apply-setup ((player (eql :om-audio)))
  (when *juce-player* (juce-player-setup *juce-player*)))

;; called from init-om-session
(defmethod player-open ((self (eql :om-audio)))
  (setq *juce-player* (juce::openAudioManager))
  (when *juce-player* (juce::initializeaudiochannels *juce-player* 0 2)))

(defmethod player-close ((self (eql :om-audio)))
  (when *juce-player* (juce::closeAudioManager *juce-player*))
  (setf *juce-player* nil))


(defun get-internal-interval (interval sound at)
  (om- (interval-intersec interval (list at (+ at (real-dur sound)))) at))

#|
(defmethod prepare-to-play ((engine (eql :om-audio)) (player omplayer) object at interval params)
  (when (loaded object)
    (let* ((newinterval (get-internal-interval interval object at)))
      
      (unless (player-data object)
        (setf (player-data object) (juce::makeAudioSourceFromFile (namestring (om-sound-file-name object)))))
      
      (if (player-data object)   ;; 
            
      (when (or (null interval) newinterval) ;; the object has to be played
        (call-next-method engine player object at newinterval params)
        ;; => will schedule a player-play-object at <at>
        )
        (om-print "SOUND NOT LOADED" "juce-player =>")))))
|#

(defmethod prepare-to-play ((engine (eql :om-audio)) (player omplayer) object at interval params)
  (when (loaded object)
    (let* ((off (if (equal (type-of (caller player)) 'soundeditor) 0 at))
           (newinterval  (get-internal-interval interval object off)))
      (unless (player-data object)
        (setf (player-data object) (juce::makeAudioSourceFromFile (namestring (om-sound-file-name object)))))
      (if (player-data object)   ;; 
          (when (or (null interval) newinterval) ;; the object has to be played
        (call-next-method engine player object off newinterval 
                          params)
        ;; => will schedule a player-play-object at <at>
        )
        (om-print "SOUND NOT LOADED" "juce-player =>")))))


;;; do nothing 
(defmethod player-start ((engine (eql :om-audio)) &optional play-list)
  (call-next-method))

;;; PAUSE
(defmethod player-pause ((engine (eql :om-audio)) &optional play-list)
  (loop for object in play-list do
        (when (player-data object)
          (juce::pauseAudioSource *juce-player* (player-data object)))))

;;; CONTINUE
(defmethod player-continue ((engine (eql :om-audio)) &optional play-list)
  (loop for object in play-list do
        (when (player-data object)
          (juce::startAudioSource *juce-player* (player-data object)))))

;;; STOP
(defmethod player-stop ((engine (eql :om-audio)) &optional play-list)
  (loop for object in play-list do
        (when (player-data object)
          (juce::stopAudioSource *juce-player* (player-data object)))))


;;; PLAY (NOW)
;;; we just suppose the stop will be called somewhere else..
(defmethod player-play-object ((engine (eql :om-audio)) (object sound) &key interval params)
  (declare (ignore params))
  (when (player-data object)
    (when interval 
      (juce::setAudioSourcePos (player-data object) (round (* (sample-rate object) 0.001 (car interval)))))
    (juce::startAudioSource *juce-player* (player-data object)))
  )

(defmethod player-loop ((self (eql :om-audio)) player &optional play-list)
  (loop for obj in play-list do
        (schedule-task player 
                       #'(lambda () 
                           (player-play-object 
                            self obj 
                            :interval (get-internal-interval (play-interval player) obj (offset->ms obj))))
                       (car (play-interval player)))
        ))


(defmethod (setf vol) (vol (object sound))
  (let ((vv (if (integerp vol) (* vol 0.01) vol)))
  (setf (slot-value object 'vol) vv)
  (when (player-data object)
    (juce::setAudioSourceGain (player-data object) (* vv *audio-master-gain*)))
  ;(print (juce::getAudioSourceGain (player-data object)))
  vv))

(defmethod make-player-specific-controls ((self (eql :om-audio)) control-view)
 (let ((snd (object (editor control-view))))
   (list 
    ;(om-make-dialog-item 'om-static-text 
    ;                     (om-make-point 405 8)
    ;                     (om-make-point 40 20) "Gain" ;level
    ;                     :font *om-default-font1*)
    (om-make-view 'graphic-numbox :position (om-make-point 360 8) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :draw-fun #'(lambda (item) 
                                      (om-with-font (om-make-font "Verdana" 8)
                                                    (om-draw-string 1 15 (format nil "~A" (* (value item) 0.01)))))
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (setf (vol snd) (* (value item) 0.01)))
                        :font *om-default-font2*
                        :value (round (* (vol snd) 100))
                        :min-val 0
                        :max-val 100))))

