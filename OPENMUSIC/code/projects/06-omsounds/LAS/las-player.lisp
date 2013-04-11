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


(defmethod player-play ((self las-player) (object t) &key interval)
  (om-beep-msg "LAS player plays only SOUND objects"))


;;; called when a box or editor attached to player is played
(defmethod player-play ((self las-player) (object sound) &key interval)
  ;(setf (sound-to-play self) object)
  (call-next-method)
  (oa::om-smart-play object (car interval) (cadr interval) (tracknum object)))

;;; called when a box or editor attached to player is stoped
(defmethod player-stop ((player las-player) &optional object)
  (call-next-method)
  (oa::om-smart-stop object))

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
  (let ((snd (object (editor control-view))))
    (list 
     (om-make-dialog-item 'om-static-text (om-make-point 420 4)
                          (om-make-point 80 20) "Track"
                          :font *om-default-font1*)
     (om-make-dialog-item 'numBox
                          (om-make-point 530 8)
                          (om-make-point 28 18) (format () " ~D" (tracknum (object (editor control-view))))
                          :min-val 0 :max-val 32
                          :font *om-default-font1*
                          :bg-color *om-white-color*
                          :value (tracknum (object (editor control-view)))
                          :afterfun #'(lambda (item)
                                        (let ()
                                          (if (eq (oa::assoc-player snd) *audio-player-visible*)
                                              (oa::om-smart-stop snd))
                                          (print "test")
                                          ;(setf (tracknum (object (om-view-container self))) (- (value item) 1))
                                          (report-modifications (editor control-view)))))
     (om-make-dialog-item 'om-check-box (om-make-point 590 4)
                          (om-make-point 170 20) "Use Original Sound"
                          :font *om-default-font1*
                          :checked-p (if (or (= -1 (oa::current-is-original snd)) 
                                             (= 0 (oa::current-is-original snd))) nil t)
                          :di-action (om-dialog-item-act item (let ()
                                                                (oa::om-use-original-sound sndpanel))))
     )))