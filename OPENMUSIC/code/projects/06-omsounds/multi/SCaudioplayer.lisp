(in-package :om)


#+linux(add-assoc-player *general-player* 'SCAudioPlayer)

;; application-control defined in 02-musicproject/players/SCplayer.lisp

;;(launch-SCplayer-app)

;================
; PROTOCOL
;================

(defvar *sc-file-player-file-to-play* nil)

(defmethod InitPlayingSeq ((player (eql 'SCAudioPlayer)) dur &key (port nil)) t)
    
(defmethod FinalizePlayingSeq ((player (eql 'SCAudioPlayer)) dur &key (port nil)) t)

(defmethod* PrepareToPlay ((player (Eql 'SCAudioPlayer)) (self sound) at &key  approx port (interval '(0)) voice)
  (declare (ignore approx))
  (setf *sc-file-player-file-to-play* (namestring (om-sound-file-name self)))
  (om-send-osc-message *SCplayer-out-port* *SCplayer-host*  (list "/scfileplayer/open" (namestring (om-sound-file-name self)) (/ (car interval) 1000.0))))

(defmethod Play-player ((self (eql 'SCAudioPlayer)))
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *SCplayer-out-port* *SCplayer-host*  (list "/scfileplayer/play" 1))))

(defmethod Stop-Player ((self (eql 'SCAudioPlayer)) &optional view)
   (declare (ignore view))
   (when *sc-file-player-file-to-play*
     (om-send-osc-message *SCplayer-out-port* *SCplayer-host*  (list "/scfileplayer/play" 0))))

(defmethod Pause-Player ((self (eql 'SCAudioPlayer)))
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *SCplayer-out-port* *SCplayer-host*  (list "/scfileplayer/pause" 1))))

(defmethod Continue-Player ((self (eql 'SCAudioPlayer)))
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *SCplayer-out-port* *SCplayer-host*  (list "/scfileplayer/pause" 0))))

(defmethod Reset-Player ((self (eql 'SCAudioPlayer)) &optional view)
  (declare (ignore view))
  (om-send-osc-message *SCplayer-out-port* *SCplayer-host*  (list "/scfileplayer/reset"))
  (setf *sc-file-player-file-to-play* nil)
  t)

;; add SuperCollider general player to list:

(setf *audio-players* (x-append :scaudioplayer *audio-players*))
(defmethod audio-player-name ((self (eql :mplayer))) "SCAudioPlayer")
(defmethod audio-player-desc ((self (eql :mplayer))) "external SCAudioPlayer")
(defmethod player-special-action ((self (eql :mplayer))) (launch-SCplayer-app))

