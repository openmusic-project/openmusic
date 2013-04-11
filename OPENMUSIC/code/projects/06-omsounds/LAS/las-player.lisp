;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING THE LAS ARCHITECTURE
;;;===========================================

(in-package :om)

(defclass las-player (omplayer) 
  ((sound-to-play :initform nil :initarg :sound-to-play :accessor sound-to-play)))

;;; retrieve the class from the 'stored' attribute of the sound editor
(defmethod class-from-player-type ((type (eql :libaudio))) 'las-player)

;;; called when a box or editor attached to player is played
(defmethod player-start ((self las-player) obj &key interval)
  (setf (sound-to-play self) obj)
  (call-next-method)
  (oa::om-smart-play obj interval))

;;; called when a box or editor attached to player is stoped
(defmethod player-stop ((player las-player))
  (call-next-method)
  (when (sound-to-play player)
    (oa::om-smart-stop (sound-to-play player))))

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
     (om-make-dialog-item 'om-check-box (om-make-point 420 4)
                          (om-make-point 130 20) "Send to track:"
                          :font *om-default-font1*
                          :checked-p (if (oa::assoc-player snd) (if (eq (oa::assoc-player snd) oa::*audio-player-hidden*) nil t) nil)
                          :di-action (om-dialog-item-act item (let ()
                                                                (editor-stop (editor self)) 
                                                                (oa::om-send-to-track sndpanel))))
     (om-make-dialog-item 'numBox
                          (om-make-point 530 8)
                          (om-make-point 28 18) (format () " ~D" (tracknum (object (om-view-container self))))
                          :min-val 1 :max-val 32
                          :font *om-default-font1*
                          :bg-color *om-white-color*
                          :value (tracknum (object (editor self)))
                          :afterfun #'(lambda (item)
                                        (let ()
                                          (if (eq (oa::assoc-player snd) *audio-player-visible*)
                                              (oa::om-smart-stop snd sndpanel))
                                          (setf (tracknum (object (om-view-container self))) (- (value item) 1)) 
                                          (report-modifications (om-view-container self)))))
     (om-make-dialog-item 'om-check-box (om-make-point 590 4)
                          (om-make-point 170 20) "Use Original Sound"
                          :font *om-default-font1*
                          :checked-p (if (or (= -1 (oa::current-is-original snd)) 
                                             (= 0 (oa::current-is-original snd))) nil t)
                          :di-action (om-dialog-item-act item (let ()
                                                                (oa::om-use-original-sound sndpanel))))
     )))