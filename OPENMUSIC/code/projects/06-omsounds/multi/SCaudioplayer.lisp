(in-package :om)

;; application-control and communication defined in
;; 02-musicproject/players/SCplayer.lisp

;;(launch-SCplayer-app)

;================
; PROTOCOL
;================

(defvar *sc-file-player-file-to-play* nil)

(defmethod InitPlayingSeq ((player (eql 'SCAudioPlayer)) dur &key (port nil)) t)
    
(defmethod FinalizePlayingSeq ((player (eql 'SCAudioPlayer)) dur &key (port nil)) t)

(defmethod* PrepareToPlay ((player (Eql 'SCAudioPlayer)) (self sound) at &key  approx port (interval '(0 -1)) voice)
  (declare (ignore approx))
  (print (list 'PrepareToPlay (gensym "ptp")))			;; individual players?
  (setf *sc-file-player-file-to-play* (namestring (om-sound-file-name self)))
  (om-send-osc-message *SCplayer-lang-port*
		       *SCplayer-host*
		       (list "/scfileplayer/open" *sc-file-player-file-to-play* interval)))



(defmethod Play-player ((self (eql 'SCAudioPlayer)))
  (print (list 'play-player self *sc-file-player-file-to-play*))
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *SCplayer-lang-port* *SCplayer-host*  (list "/scfileplayer/play" 1))))

(defmethod Stop-Player ((self (eql 'SCAudioPlayer)) &optional view)
  (declare (ignore view))
  (print (list 'stop-player))
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *SCplayer-lang-port* *SCplayer-host*  (list "/scfileplayer/play" 0))))

(defmethod Pause-Player ((self (eql 'SCAudioPlayer)))
  (print (list 'pause-player))
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *SCplayer-lang-port* *SCplayer-host*  (list "/scfileplayer/pause" 1))))

(defmethod Continue-Player ((self (eql 'SCAudioPlayer)))
  (print (list 'continue-player))
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *SCplayer-lang-port* *SCplayer-host*  (list "/scfileplayer/pause" 0))))

(defmethod Reset-Player ((self (eql 'SCAudioPlayer)) &optional view)
  (declare (ignore view))
  (print (list 'reset-player))
  (om-send-osc-message *SCplayer-lang-port* *SCplayer-host*  (list "/scfileplayer/reset"))
  (setf *sc-file-player-file-to-play* nil)
  t)

;; add SuperCollider general player to list:

(setf *audio-players* (x-append :scaudioplayer *audio-players*))
(defmethod audio-player-name ((self (eql :scaudioplayer))) "SCAudioPlayer")
(defmethod audio-player-desc ((self (eql :scaudioplayer))) "external SuperCollider Player for OM")
(defmethod player-special-action ((self (eql :scaudioplayer))) (launch-SCplayer-app))
