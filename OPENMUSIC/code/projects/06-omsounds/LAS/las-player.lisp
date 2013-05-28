;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING THE LAS ARCHITECTURE
;;;===========================================

(in-package :om)


;;; METHODES A REDEFINIR QUAND ON UTILISE OMPLAYER

;;; par défaut (call-next-method) schedule player-play-object au moment voulu...
(defmethod prepare-to-play ((engine (eql :libaudio)) (player omplayer) object at interval)
  (call-next-method))

;;; si prepare-to-play est personnalisé, il faudra aussi changer player-start...
(defmethod player-start ((engine (eql :libaudio)) &optional play-list)
  (call-next-method))

;;; PAUSE
(defmethod player-pause ((engine (eql :libaudio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (- (length play-list) 1) do
            (player-pause-object engine (nth i play-list)))
    (las-pause-all-players)))

;;; CONTINUE
(defmethod player-continue ((engine (eql :libaudio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (- (length play-list) 1) do
            (player-continue-object engine (nth i play-list)))
    (las-cont-all-players)))

;;; STOP
(defmethod player-stop ((engine (eql :libaudio)) &optional play-list)
  (if play-list
      (loop for i from 0 to (- (length play-list) 1) do
            (player-stop-object engine (nth i play-list)))
    (las-stop-all-players)))

;;; PLAY (NOW)
(defmethod player-play-object ((engine (eql :libaudio)) (object sound) &key interval)
  (las-play object (car interval) (cadr interval) (tracknum object)))

;;; NOT IN OM PLAYER API

;;; PAUSE ONLY ONE OBJECT
(defmethod player-pause-object ((engine (eql :libaudio)) (object sound) &key interval)
  (las-pause object (tracknum object)))

;;; RESTART ONLY ONE OBJECT
(defmethod player-continue-object ((engine (eql :libaudio)) (object sound) &key interval)
  (las-play object (car interval) (cadr interval) (tracknum object)))

;;; STOP ONLY ONE OBJECT
(defmethod player-stop-object ((engine (eql :libaudio)) (object sound) &key interval)
  (las-stop object (tracknum object)))

;(defclass las-player (omplayer) 
  ;((sound-to-play :initform nil :initarg :sound-to-play :accessor sound-to-play))
;  ())

;;; TODO
;;; called when a box or editor attached to player is removed/closed
(defmethod player-cleanup ((player (eql :libaudio)))
  (let* ((snd (sound-to-play player))
         (status-list (if (eq player oa::*audio-player-hidden*)
                          oa::*audio-player-hidden-tracks-info*
                        oa::*audio-player-visible-tracks-info*))
         (chan (if (eq player oa::*audio-player-hidden*)
                   (oa::tracknum-sys (sound-to-play player))
                 (tracknum (sound-to-play player))))
         (loadedsnd (car (gethash chan status-list)))
         (status (cadr (gethash chan status-list))))
    (if (eq snd loadedsnd)
        (let () 
          (oa::om-smart-stop snd)
          (setf (car (gethash chan status-list)) nil)))
    ))


;;; creates the player-specific control on the sound editor control panel
(defmethod make-player-specific-controls ((self (eql :libaudio)) control-view)
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
                                              (report-modifications (editor control-view))))))
     ;(om-make-dialog-item 'om-check-box (om-make-point 590 4)
     ;                     (om-make-point 170 20) "Use Original Sound"
     ;                     :font *om-default-font1*
     ;                     :checked-p (if (or (= -1 (oa::current-is-original snd)) 
     ;                                        (= 0 (oa::current-is-original snd))) nil t)
     ;                     :di-action (om-dialog-item-act item (let ()
     ;                                                           (oa::om-use-original-sound sndpanel))))
     )))