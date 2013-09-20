;;===========================================================================
;;SuperCollider player engine (audio) for OM.
;; 
;;This program is free software; you can redistribute it and/or modify
;;it under the terms of the GNU Lesser General Public License as published by
;;the Free Software Foundation; either version 2.1 of the License, or
;;(at your option) any later version.
;;  
;;This program is distributed in the hope that it will be useful,
;;but WITHOUT ANY WARRANTY; without even the implied warranty of
;;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;GNU Lesser General Public License for more details.
;;  
;;You should have received a copy of the GNU Lesser General Public License
;;along with this program; if not, write to the Free Software 
;;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;Author: Anders Vinjar


(in-package :om)

;; application-control and communication defined in
;; 02-musicproject/players/SCplayer.lisp

;;(launch-SCplayer-app)

;; (let* ((curlist (players-for-object (make-instance 'sound)))
;;        (newlist (pushnew :scaudioplayer curlist)))
;;   (defmethod players-for-object ((self sound)) newlist))

(defmethod player-name ((self (eql :scaudioplayer))) "scaudioplayer")
(defmethod player-desc ((self (eql :scaudioplayer))) "external supercollider player")
(defmethod player-special-action ((self (eql :scaudioplayer))) (launch-scplayer-app))
(defmethod player-type ((player (eql :scaudioplayer))) :UDP)

;; application-control and communication defined in
;; 02-musicproject/players/SCplayer.lisp

;;(launch-scplayer-app)

(defun init-sc-player ()
  (let* ((curlist (players-for-object (make-instance 'sound)))
	 (newlist (pushnew :scaudioplayer curlist)))
    (defmethod players-for-object ((self sound)) newlist))
  (pushnew :scaudioplayer *enabled-players*))

;;(init-sc-player)
(om-add-init-func 'init-sc-player)

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

(defmethod player-set-loop ((engine (eql :mplayer)) &optional start end)
  (print (format nil "~A : set loop" engine)))

(defmethod player-loop ((engine (eql :mplayer)) &optional play-list)
  (print (format nil "~A : loop" engine)))





