(in-package :om)

;; application-control and communication defined in
;; 02-musicproject/players/SCplayer.lisp

;;(launch-SCplayer-app)

(defmethod player-name ((self (eql :scaudioplayer))) "scaudioplayer")
(defmethod player-desc ((self (eql :scaudioplayer))) "external supercollider player for om")
(defmethod player-special-action ((self (eql :scaudioplayer))) (launch-scplayer-app))
(defmethod player-type ((player (eql :scaudioplayer))) :UDP)
;================
; PROTOCOL
;================

(defvar *sc-file-player-file-to-play* nil)

(defmethod prepare-to-play ((engine (eql 'scaudioplayer)) (player omplayer) object at interval)
  (om-send-osc-message *scplayer-lang-port*
		       *scplayer-host*
		       (list "/scfileplayer/open"
			     (setf *sc-file-player-file-to-play* (namestring (om-sound-file-name object)))))
  (when interval
    (let ((newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at)))
      (om-send-osc-message *scplayer-lang-port* *scplayer-host*  (list "/scfileplayer/start" (/ (car newinterval) 1000.0)))))
  (call-next-method))


(defmethod player-start ((engine (eql :scaudioplayer)) &optional play-list)
  (call-next-method))

(defmethod player-play-object ((engine (eql :scaudioplayer)) (object sound) &key interval)
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *scplayer-lang-port* *scplayer-host*  (list "/scfileplayer/play" 1))))

(defmethod player-stop ((engine (eql :scaudioplayer)) &optional play-list)
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *scplayer-lang-port* *scplayer-host*  (list "/scfileplayer/play" 0))
    (setf *sc-file-player-file-to-play* nil)))

(defmethod player-pause ((engine (eql :scaudioplayer)) &optional play-list)
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *scplayer-lang-port* *scplayer-host*  (list "/scfileplayer/pause" 1))))

(defmethod player-continue ((engine (eql :scaudioplayer)) &optional play-list)
  (when *sc-file-player-file-to-play*
    (om-send-osc-message *scplayer-lang-port* *scplayer-host*  (list "/scfileplayer/pause" 0))))


