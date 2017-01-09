;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING JUCE
;;;===========================================

(in-package :om)

;Constants to use to create players.
(defconstant *audio-in-chan* 0)
(defconstant *audio-out-chan* 2)
(defvar *audio-sr* 44100)
(defconstant *audio-buffsize* 512)
(defconstant *audio-streambuffsize* 65536)

(defvar *juce-player* nil)

(defmethod player-name ((self (eql :om-audio))) "Default audio player")
(defmethod player-desc ((self (eql :om-audio))) "(based on Juce)")
(enable-player :om-audio)
(add-player-for-object 'sound :om-audio)

(defun player-setup (player)
  (let ((in-devices (juce::getinputdevicenames player))
        (out-devices (juce::getoutputdevicenames player)))
    (print (format nil "AUDIO SETUP: ~A x ~A / ~A x ~A, ~AHz" 
                   (car out-devices) *audio-out-chan* (car in-devices) *audio-in-chan* *audio-sr*))
    (juce::setdevices 
     player 
     (car in-devices) *audio-in-chan* 
     (car out-devices) *audio-out-chan*
     *audio-sr*)  
    ))

(defmethod player-open ((self (eql :om-audio)))
  (setq *juce-player* (juce::OpenAudioPlayer))
  (when *juce-player* (player-setup *juce-player*)))

(defmethod player-close ((self (eql :om-audio)))
  (when *juce-player* (juce::closeaudioplayer *juce-player*))
  (setf *juce-player* nil))

; (set-audio-sample-rate 44100)
;; called from preferences
(defun set-audio-sample-rate (sr)
  (setq *audio-sr* sr)
  (when *juce-player* (player-setup *juce-player*)))

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
