(in-package :om)

;==================
; APP
;==================

(defmethod player-name ((engine (eql :mplayer))) "mplayer")
(defmethod player-desc ((engine (eql :mplayer))) "external mplayer")
(defmethod player-special-action ((engine (eql :mplayer))) (mplayer-launch-mplayer-app))


;; enable mplayer-engine for class sound:

(let* ((curlist (players-for-object (make-instance 'sound)))
       (newlist (pushnew :mplayer curlist)))
  (defmethod players-for-object ((self sound)) newlist))

(pushnew :mplayer *enabled-players*)

(defvar *mplayer-iostr* nil)
(defvar *mplayer-pid* nil)
(defvar *mplayer-path* nil)
(defvar *mplayer-args* nil)
(defvar *mplayer-error-stream* nil)

(defun init-mplayer-app ()
  (setf *mplayer-args* "-ao jack,alsa -idle -slave -quiet")
  (setf *mplayer-path* "/usr/local/bin/mplayer"))

(init-mplayer-app)

;;(setf *mplayer-pid* nil)
;;(om-add-init-func 'init-mplayer-app)

(defun mplayer-send-cmd (cmd)
  (progn (unless *mplayer-pid*
	   (mplayer-launch-mplayer-app))
	 ;; (format *mplayer-iostr* "pausing_keep_force ")
	 (format *mplayer-iostr* "~A~%" cmd)))

(defun mplayer-launch-mplayer-app ()
  (unless *mplayer-pid*
    (let* ((path *mplayer-path*)
	   (exec (format nil "~A ~A" path *mplayer-args*)))
      ;; (pprint exec)
      (multiple-value-bind (iostr error-stream pid)
      	  (system::run-shell-command exec
      				     :wait nil
      				     :input :stream
      				     :output :stream
      				     :error-output :stream)
	(setf *mplayer-pid* pid)
      	(setf *mplayer-iostr* iostr)
      	(setf *mplayer-error-stream* error-stream)
	(format *mplayer-iostr* "pause~%")
      	;;*mplayer-pid*
      	*mplayer-iostr*)
      )))


(defun mplayer-kill-and-cleanup-one-mplayer-pid (&optional (pid *mplayer-pid*))
  (progn 
    (when pid
      (system::run-shell-command (format nil "kill -9 ~A" pid)))
    (setf *mplayer-pid* nil)
    (when *mplayer-iostr*
      (close *mplayer-iostr*)
      (setf *mplayer-iostr* nil))
    (when *mplayer-error-stream*
      (close *mplayer-error-stream*)
      (setf *mplayer-error-stream* nil))))


;; ;;(mplayer-launch-mplayer-app)

;; (mplayer-kill-and-cleanup-one-mplayer-pid)

;; (mplayer-send-cmd "/tmp/01-floratone.ogg")
;; (mplayer-send-cmd "loadfile /tmp/01-floratone.ogg")
;; (mplayer-send-cmd "pause")

;; (progn
;;   (mplayer-send-cmd "loadfile /tmp/01-floratone.ogg")
;;   (mplayer-send-cmd "pause"))

;; (mplayer-send-cmd "mute")

;; (mplayer-send-cmd "loadfile /tmp/01-floratone.ogg")
;; (mplayer-send-cmd "pause")
;; (mplayer-send-cmd "play")
;; (mplayer-send-cmd "stop")
;; (mplayer-send-cmd "mute")
;; (mplayer-send-cmd "loop -1")		;0=forever, -1=noloop
;; (mplayer-send-cmd "loop 0")		;0=forever, -1=noloop
;; (mplayer-send-cmd "volume -5")
;; (mplayer-send-cmd "volume 100 1")
;; (mplayer-send-cmd "volume 10 1")	; 0=relative, 1=absolute (val: 0--100)
;; (mplayer-send-cmd (format nil "seek ~F" -2.2))
;; (mplayer-send-cmd (format nil "seek 90 2")) ;2 = seconds, 1 = percent, 0 = relative
;; (mplayer-send-cmd (format nil "seek 0 2"))
;; (mplayer-send-cmd (format nil "speed_set ~A" 10.0))
;; (mplayer-send-cmd (format nil "speed_set ~A" 1.0))
;; (mplayer-send-cmd (format nil "speed_set ~A" 0.4))
;; (mplayer-send-cmd "quit")

;; (mplayer-kill-and-cleanup-one-mplayer-pid *mplayer-pid*)

;================
; PROTOCOL
;================

(defvar *mplayer-file-to-play* nil)

;; (au::wave-file-p "/home/andersvi/lyd/andersvi/Floratone-1m.wav")
;; (au::sound-get-info-wave "/home/andersvi/lyd/andersvi/Floratone-1m.wav")
;; (om-api::om-sound-get-info "/home/andersvi/lyd/andersvi/Floratone-1m.wav")

(defmethod prepare-to-play ((engine (eql :mplayer)) (player omplayer) object at interval)
  (setf *mplayer-file-to-play* (namestring (om-sound-file-name object)))
  (mplayer-send-cmd (format nil "loadfile ~A" *mplayer-file-to-play*))
  (mplayer-send-cmd "pause")		;mplayer starts playing after loading...
  (mplayer-send-cmd (format nil "seek ~A 2" (if at at 0)))
  (when interval
    (let ((newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at)))
      (mplayer-send-cmd (format nil "seek ~A 2" (/ (car newinterval) 1000.0)))))
  (call-next-method))

(defmethod player-start ((engine (eql :mplayer)) &optional play-list) (call-next-method))

(defmethod player-play-object ((engine (eql :mplayer)) (object sound) &key interval)
  (when *mplayer-file-to-play*
    (mplayer-send-cmd "seek 0 2")))

(defmethod player-stop ((engine (eql :mplayer)) &optional playlist)
  (declare (ignore view))
  (when *mplayer-file-to-play*
    (mplayer-send-cmd "stop")
    ;;(mplayer-send-cmd "seek 0 2")
    ))

;; use *mplayer-toggle-pause* here. AV:

(defparameter *mplayer-paused* nil)

(defmethod player-pause ((engine (eql :mplayer)) &optional playlist)
  (when (and *mplayer-file-to-play* (not *mplayer-paused*))
    (mplayer-send-cmd "pause")
    (setf *mplayer-paused* t)))

(defmethod player-continue ((engine (eql :mplayer)) &optional playlist)
  (when (and *mplayer-file-to-play* *mplayer-paused*)
    (mplayer-send-cmd "pause")
    (setf *mplayer-paused* nil)))

;; these dont work yet. AV.

(defmethod player-set-loop ((engine (eql :mplayer)) &optional start end)
  (print (format nil "~A : set loop" engine))
  t)

(defmethod player-loop ((engine (eql :mplayer)) &optional play-list)
  (mplayer-send-cmd "loop 0"))


