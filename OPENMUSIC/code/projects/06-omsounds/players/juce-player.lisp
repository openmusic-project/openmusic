;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING JUCE
;;;===========================================

(in-package :om)

;Constants to use to create players.
(defconstant *audio-in-chan* 2)
(defconstant *audio-out-chan* 2)
(defvar *audio-sr* 44100)
(defconstant *audio-buffsize* 512)
(defconstant *audio-streambuffsize* 65536)

(defvar *juce-player* nil)

(defmethod player-name ((self (eql :om-audio))) "Default audio player")
(defmethod player-desc ((self (eql :om-audio))) "(based on Juce)")
(enable-player :om-audio)
(add-player-for-object 'sound :om-audio)

(defmethod player-open ((self (eql :om-audio)))
  (setf *juce-player* (juce::OpenAudioPlayer *audio-in-chan* *audio-out-chan* *audio-sr*)))

(defmethod player-close ((self (eql :om-audio)))
  (when *juce-player* (juce::closeaudioplayer *juce-player*))
  (setf *juce-player* nil))


;; called from preferences
(defun set-audio-sample-rate (sr)
  (setq *audio-sr* sr)
  (player-close :om-audio)
  (player-open :om-audio))


(defmethod prepare-to-play ((engine (eql :om-audio)) (player omplayer) object at interval params)
  (when (loaded object)
  (let* ((newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at))
         (from (car newinterval))
         (to (cadr newinterval))
         newptr)
    (setf (player-data object)
          (juce::makefilereader (namestring (om-sound-file-name object))))
    (when (or (null interval) newinterval)
      (call-next-method engine player object at newinterval params)))))



(defmethod player-start ((engine (eql :om-audio)) &optional play-list)
  (call-next-method))

;;; PAUSE
(defmethod player-pause ((engine (eql :om-audio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-pause-object engine (nth i play-list)))
    ))

;;; CONTINUE
(defmethod player-continue ((engine (eql :om-audio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-continue-object engine (nth i play-list)))
    ))

;;; STOP
(defmethod player-stop ((engine (eql :om-audio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-stop-object engine (nth i play-list)))
    ))



;;; PLAY (NOW)
(defmethod player-play-object ((engine (eql :om-audio)) (object sound) &key interval params)
  ;(print "play")
  (when interval 
    (juce::setposreader (player-data object) (round (* (sample-rate object) 0.001 (car interval)))))
  (juce::startreader *juce-player* (player-data object)))

(defmethod player-loop ((self (eql :om-audio)) player &optional play-list)
  (declare (ignore player))
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (let ((thesound (nth i play-list)))
              ;(las-stop thesound (tracknum thesound))
              ;(las-loop-play thesound (tracknum thesound))
              (juce::stopreader *juce-player* (player-data thesound))
              ;(juce::setposreader (player-data thesound) (round (* (sample-rate thesound) 0.001 TIMEINMS))  ----  REMPLACER TIMEINMS
              (juce::startreader *juce-player* (player-data thesound))))))

;;; NOT IN OM PLAYER API

;;; PAUSE ONLY ONE OBJECT
(defmethod player-pause-object ((engine (eql :om-audio)) (object sound) &key interval)
  ;(las-pause object (tracknum object))
  (juce::startreader *juce-player* (player-data object))
  )

;;; RESTART ONLY ONE OBJECT
(defmethod player-continue-object ((engine (eql :om-audio)) (object sound) &key interval)
  ;(las-play object (car interval) (cadr interval) (tracknum object))
  (juce::startreader *juce-player* (player-data object))
  )

;;; STOP ONLY ONE OBJECT
(defmethod player-stop-object ((engine (eql :om-audio)) (object sound) &key interval)
  ;(las-stop object (tracknum object))
  (juce::stopreader *juce-player* (player-data object))
  ) 



