(in-package :om)

(defclass las-player (omplayer) 
  (sound-to-play :initform nil :initarg :sound-to-play :accessor sound-to-play))

(defmethod class-from-player-type ((type (eql :libaudio))) 'las-player)

;;; called when a box or editor attached to player is played
(defmethod player-start ((self las-player) obj &key interval)
  (setf (sound-to-play self) obj)
  (call-next-method)
  (oa::om-smart-play obj))

(defmethod player-stop ((player omplayer))
  (oa::om-smart-stop player))

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
