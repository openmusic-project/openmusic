;; ===========================================================================
;; mplayer player engine for OM (audio).
;;  
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2.1 of
;; the License, or (at your option) any later version.
;;   
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;   
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.
;; 
;; Author: Anders Vinjar

(in-package :om)

;==================
; PLAYER
;==================

(defmethod player-name ((engine (eql :mplayer))) "mplayer")
(defmethod player-desc ((engine (eql :mplayer)))
  (format nil "external mplayer: ~A" (namestring *mplayer-path*)))
;;(defmethod player-special-action ((engine (eql :mplayer))) (mplayer-launch-mplayer-app))
(defmethod player-type ((engine (eql :mplayer))) :external)

;==================
; external app
;==================

;; TODO: set up case for non-jack-enabled mplayer and jack running.

(defvar *mplayer-args*)
(defvar *mplayer-path*)
(defvar *mplayer-alsa-device* "default")

(defun init-mplayer-app ()
  
  ;; enable mplayer-engine for sound class:
  
  (pushnew :mplayer *enabled-players*)
  (add-player-for-object 'sound :mplayer))

;;(init-mplayer-app)
(om-add-init-func 'init-mplayer-app)

;==================
; control external mplayers
;==================

(setq *mplayer-args* (format nil "-ao jack,pulse,alsa:device=~A -idle -slave -quiet" *mplayer-alsa-device*))
(setq *mplayer-path* (or (probe-file "/usr/local/bin/mplayer") "mplayer"))

(defvar *mplayers* (make-hash-table))
(defstruct mplayer-proc pid iostream error-stream paused loop-points)

;; send cmd to running mplayer-proc.  If its not running already
;; launch a new one and return struct for bookkeeping

(defun mplayer-send-cmd (obj cmd)
  (flet ((mplayer-launch-one-mplayer ()
	   (let* ((path *mplayer-path*)
		  (exec (format nil "~A ~A" path *mplayer-args*)))
	     (multiple-value-bind (iostr error-stream pid)
		 (system::run-shell-command exec
					    :wait nil
					    :input :stream
					    :output :stream
					    :error-output :stream)
	       (make-mplayer-proc :iostream iostr :pid pid :error-stream error-stream)))))
    (let ((thisplayer (or (gethash obj *mplayers*)
			  (setf (gethash obj *mplayers*)
				(mplayer-launch-one-mplayer)))))
      (format (mplayer-proc-iostream thisplayer) "~A~%" cmd)
      thisplayer)))


(defun mplayer-kill-and-cleanup-one-mplayer (obj)
  (let ((thisplayer (gethash obj *mplayers*))) 
    (let ((pid (mplayer-proc-pid thisplayer))
	  (iostr (mplayer-proc-iostream thisplayer))
	  (error-stream (mplayer-proc-error-stream thisplayer)))
      (when pid
	(system::run-shell-command (format nil "kill -9 ~A" pid)))
      (when iostr (close iostr))
      (when error-stream (close error-stream))
      (remhash obj *mplayers*))))

;================
; PROTOCOL
;================

;; (mplayer-send-cmd 'one-player "loadfile /home/andersvi/lyd/andersvi/Floratone-1m.wav")
;; (mplayer-send-cmd 'one-player "stop")
;; (mplayer-proc-pid (gethash 'one-player *mplayers*))
;; (remhash 'one-player *mplayers*)

;; (au::wave-file-p "/home/andersvi/lyd/andersvi/Floratone-1m.wav")
;; (au::sound-get-info-wave "/home/andersvi/lyd/andersvi/Floratone-1m.wav")
;; (om-api::om-sound-get-info "/home/andersvi/lyd/andersvi/Floratone-1m.wav")

(defmethod prepare-to-play ((engine (eql :mplayer)) (player omplayer) object at interval params)
  (mplayer-send-cmd object (format nil "loadfile ~A" (namestring (om-sound-file-name object))))
  (mplayer-send-cmd object (format nil "seek ~A 2" (if interval
						       (/ (car interval) 1000.0)
						       0)))
  t)

(defmethod player-start ((engine (eql :mplayer)) &optional play-list)
  ;;(print (list 'player-start play-list))
  t ;;(call-next-method)
  )

(defmethod player-play-object ((engine (eql :mplayer)) object &key interval)
  (mplayer-send-cmd object (format nil "seek ~A 2" (if interval
						       (/ (car interval) 1000.0)
						       0))))

(defmethod player-stop-object ((engine (eql :mplayer)) object &key interval)
  (declare (ignore interval))
  (mplayer-send-cmd object "vol 0.0 1")
  (mplayer-send-cmd object "stop")
  (mplayer-send-cmd object "quit")
  (mplayer-kill-and-cleanup-one-mplayer object))

(defmethod player-stop ((engine (eql :mplayer)) &optional playlist)
  (when playlist
    (dolist (snd playlist) (player-stop-object engine snd))))

(defmethod player-continue-object ((engine (eql :mplayer)) object &key interval)
  (declare (ignore interval))
  (when (mplayer-proc-paused (gethash object *mplayers*))
    (mplayer-send-cmd object "pause")	;mplayers pause-cmd is a toggle
    (setf (mplayer-proc-paused (gethash object *mplayers*)) nil)))

(defmethod player-continue ((engine (eql :mplayer)) &optional playlist)
  (when playlist
    (dolist (snd playlist) (player-continue-object engine snd)))
  nil)

(defmethod player-pause-object ((engine (eql :mplayer)) object &key interval)
  (unless (mplayer-proc-paused (gethash object *mplayers*))
    (mplayer-send-cmd object "pause")
    (setf (mplayer-proc-paused (gethash object *mplayers*)) t)))

(defmethod player-pause ((engine (eql :mplayer)) &optional playlist)
  (when playlist
    (dolist (snd playlist) (player-pause-object engine snd)))
  nil)

;; (defvar *mplayer-loop-points* '())

;; (defmethod player-set-loop-object ((engine (eql :mplayer)) &optional start end)
;;   (setf (mplayer-proc-loop-points (gethash object *mplayers*)) (list start end)))

(defmethod player-set-loop ((engine (eql :mplayer)) &optional start end)
  (print (list 'player-set-loop engine))
  ;;(print (list 'player-set-loop start end))
  ;;(setf (mplayer-proc-loop-points (gethash object *mplayers*)) (list start end))
  nil)

;; (defmethod player-loop-object ((engine (eql :mplayer)) &optional play-list)
;;   (print (list 'player-loop-object play-list))
;;   (dolist (object play-list)
;;     (mplayer-send-cmd object (format nil "seek ~A 2" (car (mplayer-proc-loop-points (gethash object *mplayers*))))))
;;   nil)

(defmethod player-loop ((engine (eql :mplayer)) player &optional play-list)
  (print (list 'player-loop play-list))
  (dolist (snd play-list)
    (mplayer-send-cmd (if (listp snd) (first snd) snd) (format nil "seek ~A 2" (car *mplayer-loop-points*))))
  nil) 

;; (defmethod player-loop ((engine (eql :mplayer)) &optional play-list)
;;   (print (list 'player-loop play-list))
;;   (when playlist
;;     (dolist (snd play-list) (player-loop-object engine snd)))
;;   nil)


