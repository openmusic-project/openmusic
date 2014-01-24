;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING THE LAS ARCHITECTURE
;;;===========================================

(in-package :om)



(defmethod player-name ((self (eql :libaudiostream))) "LibAudioStream")
(defmethod player-desc ((self (eql :libaudiostream))) "internal OM Player")

(add-player-for-object 'sound :libaudiostream)

(defun las-open ()
 (if (las-load-library)
     (progn 
       (las-init-full-system)
       (enable-player :libaudiostream))
   (om-message-dialog (format nil (om-str :lib-error) "LibAudioStream"))))

(defun las-close ()
  (las-close-full-system)
  (disable-player :libaudiostream))

(om-add-init-func 'las-open)  
(om-add-exit-cleanup-func 'las-close t)



(defmethod prepare-to-play ((engine (eql :libaudiostream)) (player omplayer) object at interval params)
  (let* ((newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at))
         (from (car newinterval))
         (to (cadr newinterval))
         newptr)
    (if (om-sound-sndlasptr-current object)
        (progn
          (setf newptr (if (> (om-sound-n-channels object) 1) (om-sound-sndlasptr-current object) (las-make-stereo-sound (om-sound-sndlasptr-current object))))
          (if (or from to)
              (let ((begin (if from (round (* from (/ las-srate 1000.0)))))
                    (end (if to (round (* to (/ las-srate 1000.0)))))
                    (max (om-sound-n-samples-current object)))
                (if (and begin (or (< begin 0) (not begin)))
                    (setf begin 0))
                (if (and end (or (> end max) (not end)))
                    (setf end max))
                (om-sound-set-sndlasptr-to-play object (las-slice-sample-cut newptr begin end)))
            (om-sound-set-sndlasptr-to-play object newptr))
          (om-sound-update-las-infos object)
          (call-next-method engine player object at newinterval params)))))


(defmethod player-start ((engine (eql :libaudiostream)) &optional play-list)
  (call-next-method))

;;; PAUSE
(defmethod player-pause ((engine (eql :libaudiostream)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-pause-object engine (nth i play-list)))
    (las-pause-all-players)))

;;; CONTINUE
(defmethod player-continue ((engine (eql :libaudiostream)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-continue-object engine (nth i play-list)))
    (las-cont-all-players)))

;;; STOP
(defmethod player-stop ((engine (eql :libaudiostream)) &optional play-list)
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (player-stop-object engine (nth i play-list)))
    (las-stop-all-players)))


;;; PLAY (NOW)
(defmethod player-play-object ((engine (eql :libaudiostream)) (object sound) &key interval params)
  (las-play object (car interval) (cadr interval) (tracknum object)))

(defmethod player-loop ((self (eql :libaudiostream)) player &optional play-list)
  (declare (ignore player))
  (if play-list
      (loop for i from 0 to (1- (length play-list)) do
            (let ((thesound (nth i play-list)))
              (las-stop thesound (tracknum thesound))
              (las-loop-play thesound (tracknum thesound))))))

;;; NOT IN OM PLAYER API

;;; PAUSE ONLY ONE OBJECT
(defmethod player-pause-object ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-pause object (tracknum object)))

;;; RESTART ONLY ONE OBJECT
(defmethod player-continue-object ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-play object (car interval) (cadr interval) (tracknum object)))

;;; STOP ONLY ONE OBJECT
(defmethod player-stop-object ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-stop object (tracknum object)))

;(defclass las-player (omplayer) 
;  ((sound-to-play :initform nil :initarg :sound-to-play :accessor sound-to-play))
;  ())

;;; TODO
;;; called when a box or editor attached to player is removed/closed
(defmethod player-cleanup ((player (eql :libaudiostream)) snd)
  (let* ((status-list (if (= (tracknum snd) 0)
                          oa::*audio-player-hidden-tracks-info*
                        oa::*audio-player-visible-tracks-info*))
         (chan (if (eq player oa::*audio-player-hidden*)
                   (oa::tracknum-sys snd)
                 (tracknum snd)))
         (loadedsnd (car (gethash chan status-list)))
         (status (cadr (gethash chan status-list))))
    (if (eq snd loadedsnd)
        (let () 
           (if (= (tracknum snd) 0)
               (las-stop snd)
             (las-stop snd (tracknum snd)))
          (setf (car (gethash chan status-list)) nil)))))


;;; creates the player-specific controls on the sound editor control panel
(defmethod make-player-specific-controls ((self (eql :libaudiostream)) control-view)
  (let* ((snd (object (editor control-view)))
         (track (tracknum snd)))
    (list 
     (om-make-dialog-item 'om-static-text (om-make-point 420 8)
                          (om-make-point 40 20) "Track"
                          :font *om-default-font1*)
     (om-make-dialog-item 'numBox
                          (om-make-point 480 8)
                          (om-make-point 60 18) (if (> track 0) (format () " ~D" track) "no track")
                          :min-val 0 :max-val 32
                          :font *om-default-font1*
                          :bg-color *om-white-color*
                          :fg-color (if (> track 0) *om-black-color* *om-gray-color*)
                          :value track
                          :afterfun #'(lambda (item)
                                        (if (/= (tracknum snd) (value item))
                                            (progn
                                              (general-stop (player (editor control-view)))
                                              (setf (tracknum snd) (value item))
                                              (om-set-fg-color item (if (> (value item) 0) *om-black-color* *om-gray-color*))
                                              (om-set-dialog-item-text item (if (> (value item) 0) (format () " ~D" (value item)) "no track"))
                                              (if (> (value item) 0) (las-switch-sound-las-player snd 1) (las-switch-sound-las-player snd 0))
                                              (report-modifications (editor control-view)))))))))


;;;===========================================

(defmethod player-change-channel-vol ((player (eql :libaudiostream)) channel value)
  (las-change-channel-vol-visible channel value))

(defmethod player-change-channel-pan ((player (eql :libaudiostream)) channel value) 
  (las-change-channel-pan-visible channel value))