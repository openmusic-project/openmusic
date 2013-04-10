(in-package :om)


(defclass las-player (omplayer) ())

(defmethod class-from-player-type ((type (eql :libaudiostream)) 'las-player))

;;; called when a box or editor attached to player is played
(defmethod player-start ((self las-player) obj &key interval)
  (om-smart-play-stop obj))

;;; called when a box or editor attached to player is removed/closed
(defmethod box-stop-player (box (player las-player))  
  (let* (;(player (oa::assoc-player self))
         (status-list (if (eq player oa::*audio-player-hidden*)
                          oa::*audio-player-hidden-tracks-info*
                        oa::*audio-player-visible-tracks-info*))
         (chan (if (eq player oa::*audio-player-hidden*)
                   (oa::tracknum-sys snd)
                 (tracknum snd)))
         (loadedsnd (car (gethash chan status-list)))
         (status (cadr (gethash chan status-list))))
    (if (eq snd loadedsnd)
        (let () 
          (oa::om-smart-stop snd)
          (setf (car (gethash chan status-list)) nil)))
    ))
