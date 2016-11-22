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
  (juce::closeaudioplayer *juce-player*)
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
          (make-instance 'las-player-sound :filename (om-sound-file-name object)))
    (if (and (or (null interval) newinterval) (las-sound-sndlasptr-current (player-data object)))
        (progn
          (setf newptr (if (> (om-sound-n-channels object) 1) 
                           (las-sound-sndlasptr-current (player-data object)) 
                         (las-make-stereo-sound (las-sound-sndlasptr-current (player-data object)))))
          (if (or from to)
              (let ((begin (if from (round (* from (/ las-srate 1000.0)))))
                    (end (if to (round (* to (/ las-srate 1000.0)))))
                    (max (las-sound-n-samples-current (player-data object))))
                (if (and begin (or (< begin 0) (not begin)))
                    (setf begin 0))
                (if (and end (or (> end max) (not end)))
                    (setf end max))
                (las-sound-set-sndlasptr-to-play (player-data object) (las-slice-sample-cut newptr begin end)))
            (las-sound-set-sndlasptr-to-play (player-data object) newptr))
          (las-sound-update-las-infos (player-data object))
          (call-next-method engine player object at newinterval params))))))


(defmethod player-start ((engine (eql :om-audio)) &optional play-list)
  (call-next-method))

;;; PAUSE
(defmethod player-pause ((engine (eql :om-audio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-pause-object engine (nth i play-list)))
    (las-pause-all-players)))

;;; CONTINUE
(defmethod player-continue ((engine (eql :om-audio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-continue-object engine (nth i play-list)))
    (las-cont-all-players)))

;;; STOP
(defmethod player-stop ((engine (eql :om-audio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-stop-object engine (nth i play-list)))
    (las-stop-all-players)))

;;; PLAY (NOW)
(defmethod player-play-object ((engine (eql :om-audio)) (object sound) &key interval params)
  (las-play object (car interval) (cadr interval) (tracknum object)))

(defmethod player-loop ((self (eql :om-audio)) player &optional play-list)
  (declare (ignore player))
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (let ((thesound (nth i play-list)))
              (las-stop thesound (tracknum thesound))
              (las-loop-play thesound (tracknum thesound))))))

;;; NOT IN OM PLAYER API

;;; PAUSE ONLY ONE OBJECT
(defmethod player-pause-object ((engine (eql :om-audio)) (object sound) &key interval)
  (las-pause object (tracknum object)))

;;; RESTART ONLY ONE OBJECT
(defmethod player-continue-object ((engine (eql :om-audio)) (object sound) &key interval)
  (las-play object (car interval) (cadr interval) (tracknum object)))

;;; STOP ONLY ONE OBJECT
(defmethod player-stop-object ((engine (eql :om-audio)) (object sound) &key interval)
  (las-stop object (tracknum object)))



