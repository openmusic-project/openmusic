;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING THE LAS ARCHITECTURE
;;;===========================================

(in-package :om)

(defclass las-player (omplayer) 
  ;((sound-to-play :initform nil :initarg :sound-to-play :accessor sound-to-play))
  ())

;;; retrieve the class from the 'stored' attribute of the sound editor
(defmethod class-from-player-type ((type (eql :libaudio))) 'las-player)



;;; called when a box or editor attached to player is played
(defmethod player-play ((self las-player) (object sound) &key interval)
  ;(setf (sound-to-play self) object)
  (call-next-method)
  (las-play object (car interval) (cadr interval) (tracknum object)))

;;; called when a box or editor attached to player is stoped
(defmethod player-pause ((player las-player) &optional (object sound))
  (call-next-method)
  (las-pause object (tracknum object)))

;;; called when a box or editor attached to player is stoped
(defmethod player-stop ((player las-player) &optional (object sound))
  (call-next-method)
  (las-stop object (tracknum object)))

;;; called when a box or editor attached to player is removed/closed
(defmethod player-cleanup ((player las-player))
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
(defmethod make-player-specific-controls ((self las-player) control-view)
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
                                        (setf (tracknum snd) (value item))
                                        (om-set-dialog-item-text item (if (> (value item) 0) (format () " ~D" (value item)) "no track"))
                                        (om-set-fg-color item (if (> (value item) 0) *om-black-color* *om-gray-color*))
                                        (player-stop self snd)
                                        ; (if (eq (oa::assoc-player snd) *audio-player-visible*) (oa::om-smart-stop snd))
                                        (report-modifications (editor control-view))))
     ;(om-make-dialog-item 'om-check-box (om-make-point 590 4)
     ;                     (om-make-point 170 20) "Use Original Sound"
     ;                     :font *om-default-font1*
     ;                     :checked-p (if (or (= -1 (oa::current-is-original snd)) 
     ;                                        (= 0 (oa::current-is-original snd))) nil t)
     ;                     :di-action (om-dialog-item-act item (let ()
     ;                                                           (oa::om-use-original-sound sndpanel))))
     )))