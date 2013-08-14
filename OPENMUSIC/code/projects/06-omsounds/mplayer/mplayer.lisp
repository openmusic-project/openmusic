(in-package :om)

;==================
; PLAYER
;==================

(defmethod player-name ((engine (eql :mplayer))) "mplayer")
(defmethod player-desc ((engine (eql :mplayer))) "external mplayer")
;;(defmethod player-special-action ((engine (eql :mplayer))) (mplayer-launch-mplayer-app))
(defmethod player-type ((engine (eql :mplayer))) :external)


;; enable mplayer-engine for class sound:

(let* ((curlist (players-for-object (make-instance 'sound)))
       (newlist (pushnew :mplayer curlist)))
  (defmethod players-for-object ((self sound)) newlist))

(pushnew :mplayer *enabled-players*)

;==================
; APP
;==================

;; TODO: set up case for non-jack-enabled mplayer and jack running.
(defun init-mplayer-app ()
  (setf *mplayer-args* "-ao jack,alsa -idle -slave -quiet")
  (setf *mplayer-path* (or (probe-file "/usr/local/bin/mplayer")
			   (probe-file "/usr/bin/mplayer"))))

;;(init-mplayer-app)
(om-add-init-func 'init-mplayer-app)

(defvar *mplayers* (make-hash-table))
(defstruct mplayer-proc pid iostream error-stream paused)

;; launch one mplayer and return struct for bookkeeping


(defun mplayer-send-cmd (obj cmd)
  (let ((thisplayer (gethash obj *mplayers*)))
    (unless thisplayer
      (setf thisplayer
	    (setf (gethash obj *mplayers*)
		  (mplayer-launch-one-mplayer))))
    ;; (format (mplayer-proc-iostream (gethash *mplayers* obj)) "pausing_keep_force ")
    (format (mplayer-proc-iostream thisplayer) "~A~%" cmd)))

(defun mplayer-send-cmd (obj cmd)
  (let ((thisplayer (or (gethash obj *mplayers*)
			(setf (gethash obj *mplayers*)
			      (mplayer-launch-one-mplayer)))))
    ;; (format (mplayer-proc-iostream (gethash *mplayers* obj)) "pausing_keep_force ")
    (format (mplayer-proc-iostream thisplayer) "~A~%" cmd)))

(defun mplayer-launch-one-mplayer ()
  (let* ((path *mplayer-path*)
	 (exec (format nil "~A ~A" path *mplayer-args*)))
    (multiple-value-bind (iostr error-stream pid)
	(system::run-shell-command exec
				   :wait nil
				   :input :stream
				   :output :stream
				   :error-output :stream)
      (make-mplayer-proc :iostream iostr :pid pid :error-stream error-stream))))

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

(defmethod prepare-to-play ((engine (eql :mplayer)) (player omplayer) object at interval)
  (mplayer-send-cmd object (format nil "loadfile ~A" (namestring (om-sound-file-name object))))
  (mplayer-send-cmd object (format nil "seek ~A 2" (if interval
						(/ (car interval) 1000.0)
						0)))
  t)

(defmethod player-start ((engine (eql :mplayer)) &optional play-list)
  (print (list 'player-start play-list))
  t ;;(call-next-method)
  )

(defmethod player-play-object ((engine (eql :mplayer)) object &key interval)
  (print (list 'player-play-object object interval))
  (mplayer-send-cmd object (format nil "seek ~A 2" (if interval
						       (/ (car interval) 1000.0)
						       0))))

(defmethod player-stop-object ((engine (eql :mplayer)) object &key interval)
  (print (list 'player-stop-object object interval))
  ;;(mplayer-send-cmd object "stop")
  (mplayer-send-cmd object "vol 0.0 1")
  (mplayer-send-cmd object "stop; quit")
  (mplayer-kill-and-cleanup-one-mplayer object))

(defmethod player-stop ((engine (eql :mplayer)) &optional playlist)
  (print (list 'player-stop playlist))
  (when playlist
    (dolist (snd playlist) (player-stop-object engine snd))))

(defmethod player-pause-object ((engine (eql :mplayer)) object &key interval)
  (print (list 'player-pause-object object interval))
  (unless (mplayer-proc-paused object)
    (mplayer-send-cmd object "pause")
    (setf (mplayer-proc-paused object) t)))

(defmethod player-pause ((engine (eql :mplayer)) &optional playlist)
  (when playlist
    (dolist (snd playlist) (player-pause-object engine snd)))
  nil)

(defmethod player-continue-object ((engine (eql :mplayer)) object &key interval)
  (print (list 'player-continue-object object interval))
  (when (mplayer-proc-paused object)
    (mplayer-send-cmd object "pause")	;mplayers pause-cmd is a toggle
    (setf (mplayer-proc-paused object) nil)))

(defmethod player-continue ((engine (eql :mplayer)) &optional playlist)
  (when playlist
    (dolist (snd playlist) (player-continue-object engine snd)))
  nil)


(defvar *mplayer-loop-points* '())

(defmethod player-set-loop ((engine (eql :mplayer)) &optional start end)
  ;;(break)
  (print (list 'player-set-loop start end))
  (setf *mplayer-loop-points* (mapcar #'(lambda (ms) (/ ms 1000.0)) (list start end)))
  (print (format nil "~A : set loop ~A" engine *mplayer-loop-points*)))

(defmethod player-loop ((engine (eql :mplayer)) &optional play-list)
  (print (list 'player-loop play-list))
  (dolist (snd play-list)
    (mplayer-send-cmd snd (format nil "seek ~A 2" (car *mplayer-loop-points*)))))
