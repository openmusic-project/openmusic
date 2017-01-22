;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING JUCE
;;;===========================================

(in-package :om)

;Constants to use to create players.
(defconstant *audio-buffsize* 512)

(defvar *juce-player* nil)
(defvar *audio-driver* "CoreAudio")  ;;; change this on other platforms

(defmethod player-name ((self (eql :om-audio))) "Default audio player")
(defmethod player-desc ((self (eql :om-audio))) "(based on Juce)")
(enable-player :om-audio)
(add-player-for-object 'sound :om-audio)

(defun apply-audio-setup (player)
     (print (format nil "AUDIO PLAYER SETUP: ~A x ~A, ~AHz" *audio-out-device* *audio-out-n-channels* *audio-sr*))
     (juce::setdevices  player 
                        "" 0 
                        *audio-out-device* *audio-out-n-channels*
                        *audio-sr* *audio-buffsize*))
   
(defun juce-player-setup-with-check ()
  (let ((in-devices (juce::audio-driver-input-devices *juce-player* *audio-driver*))
        (out-devices (juce::audio-driver-output-devices *juce-player* *audio-driver*)))
    (unless (and *audio-out-device* (find *audio-out-device* out-devices :test 'string-equal))
      (when *audio-out-device* 
          (om-beep-msg (format nil "Selected audio device: \"~A\" not available. Restoring default." *audio-out-device*)))
      (setf *audio-out-device* (car out-devices)))
    
    (apply-audio-setup *juce-player*)

    (setf *audio-out-chan-options* (juce::getoutputchannelslist *juce-player*))
    (setf *audio-sr-options* (juce::getsamplerates *juce-player*))
    (let ((nch-ok (find *audio-out-n-channels* *audio-out-chan-options*))
          (sr-ok (find *audio-sr* *audio-sr-options*)))
      (unless nch-ok
        (setf *audio-out-n-channels* (car (last *audio-out-chan-options*))))
      (unless sr-ok
        (setf *audio-sr* (car *audio-sr-options*)))
      (unless (and nch-ok sr-ok)
        (if nch-ok
            (juce::setsamplerate *juce-player* *audio-sr*)
          (apply-audio-setup *juce-player*)
          ))
      )
    t))


;; called from preferences
(defmethod player-get-devices ((player (eql :om-audio)))
  (when *juce-player* (juce::audio-driver-output-devices *juce-player* *audio-driver*)))

(defmethod player-apply-setup ((player (eql :om-audio)))
  (when *juce-player* (juce-player-setup-with-check)))

(defmethod player-open ((self (eql :om-audio)))
  (setq *juce-player* (juce::openAudioManager))
  (when *juce-player* (juce-player-setup-with-check)))

(defmethod player-close ((self (eql :om-audio)))
  (when *juce-player* (juce::closeAudioManager *juce-player*))
  (setf *juce-player* nil))


(defun get-internal-interval (interval sound at)
  (om- (interval-intersec interval (list at (+ at (real-dur sound)))) at))

(defmethod prepare-to-play ((engine (eql :om-audio)) (player omplayer) object at interval params)
  (when (loaded object)
    (let* ((newinterval (get-internal-interval interval object at)))
      (if (player-data object)   ;; (juce::makefilereader (namestring (om-sound-file-name object)))
            
      (when (or (null interval) newinterval) ;; the object has to be played
        (call-next-method engine player object at newinterval params)
        ;; => will schedule a player-play-object at <at>
        )
        (om-print "SOUND NOT LOADED" "juce-player =>")))))

;;; do nothing 
(defmethod player-start ((engine (eql :om-audio)) &optional play-list)
  (call-next-method))

;;; PAUSE
(defmethod player-pause ((engine (eql :om-audio)) &optional play-list)
  (loop for object in play-list do
        (juce::pausereader *juce-player* (player-data object))))

;;; CONTINUE
(defmethod player-continue ((engine (eql :om-audio)) &optional play-list)
  (loop for object in play-list do
        (juce::startreader *juce-player* (player-data object))))

;;; STOP
(defmethod player-stop ((engine (eql :om-audio)) &optional play-list)
  (loop for object in play-list do
        (juce::stopreader *juce-player* (player-data object))))


;;; PLAY (NOW)
;;; we just suppose the stop will be called somewhere else..
(defmethod player-play-object ((engine (eql :om-audio)) (object sound) &key interval params)
  (declare (ignore params))
  (when interval 
    (juce::setposreader (player-data object) (round (* (sample-rate object) 0.001 (car interval)))))
  (juce::startreader *juce-player* (player-data object)))

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
    (juce::setgainreader (player-data object) (* vv *audio-master-gain*)))
  ;(print (juce::getgainreader (player-data object)))
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

