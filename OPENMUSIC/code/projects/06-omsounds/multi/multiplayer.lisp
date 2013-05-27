(in-package :om)


#+(and cocoa om-osc-api) 
; (add-assoc-player *general-player* 'multiplayer)

(defvar *multiplayer-in-port* nil)
(setf *multiplayer-in-port* 7072)

(defvar *multiplayer-out-port* nil)
(setf *multiplayer-out-port* 7071)

(defvar *multiplayer-host* nil)
(setf *multiplayer-host* "127.0.0.1")
 
;==================
; APP
;==================

(defvar *multiplayer-app* nil)
(defvar *multiplayer-path* nil)

(defun init-multiplayer-app ()
  (setf *multiplayer-path* 
        (probe-file (om-default-application-path '() "MultiPlayer"))))


(om-add-init-func 'init-multiplayer-app)

(defun launch-multiplayer-app ()
  (unless (and *multiplayer-app* (om-find-process *multiplayer-app*))
    (let ((path *multiplayer-path*))
      (if (or (null path) (not (probe-file path)))
        (om-message-dialog "MultiPlayer application not found. Please set the application path in OM preferences.") 
      (setf *multiplayer-app* (om-run-application path))
      ))))

;================
; PROTOCOL
;================

(defvar *multiplayer-file-to-play* nil)

;(defmethod InitPlayingSeq ((player (eql 'multiplayer)) dur &key (port nil)) t)
;(defmethod FinalizePlayingSeq ((player (eql 'multiplayer)) dur &key (port nil)) t)

(defmethod prepare-to-play ((engine (eql :multiplayer)) (player omplayer) object at interval)
   
   (when *multiplayer-file-to-play* (player-stop :multiplayer (list *multiplayer-file-to-play*)))
   
   (print (list "multiplayer prepare" (om-sound-file-name object)))

   (setf *multiplayer-file-to-play* (namestring (om-sound-file-name object)))

   ;(let* ((message nil)
   ;       (reply nil)
   ;       (timeout 500)
   ;       (listen-answer-process (om-start-osc-server *multiplayer-in-port* "localhost"  
   ;                                                   #'(lambda (msg host) 
   ;                                                       (let* ((rmsg (om-decode-msg-or-bundle msg))
   ;                                                              (msgcontents (if (listp (cadr rmsg)) (cadr rmsg) rmsg)))
   ;                                                        ; (print (format nil "RECEIVED FROM MultiPlayer ~A" msgcontents))
   ;                                                        ; (when (and (not reply)
   ;                                                        ;            (stringp (car msgcontents))
   ;                                                        ;            (string-equal "/Multiplayer/loaded" (car msgcontents)))
   ;                                                       ;    (setf reply t)
    ;                                                       ;   (setf message msgcontents)
   ;                                                       ;    )
   ;                                                         )
   ;                                                       ))))
   (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/open" (namestring (om-sound-file-name object))))
   (sleep 0.1)
   ;(loop for count = 0 then (+ count 1)
   ;      while (not reply) do
   ;      (sleep 0.01)
   ;      (setf count (+ count 1))
   ;      (when (> count timeout)
   ;        (print "file loader did not answer...")
   ;        (setf reply t)))
   ;(om-stop-osc-server listen-answer-process)
   ;)
   (when interval
     (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/start" (/ (car interval) 1000.0))))
   
   ;;; schedule the playback
   (call-next-method)
   )



(defmethod player-start ((engine (eql :multiplayer)) &optional play-list) (call-next-method))

(defmethod player-play-object ((engine (eql :multiplayer)) (object sound) &key interval)
  (print "multiplayer play")
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/play" 1))))

(defmethod player-stop ((engine (eql :multiplayer)) &optional play-list)
  (print "multiplayer stop")
  (when *multiplayer-file-to-play*
     (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/play" 0))
     (setf *multiplayer-file-to-play* nil)))

(defmethod player-pause ((engine (eql :multiplayer)) &optional play-list)
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/pause" 1))))

(defmethod player-continue ((engine (eql :multiplayer)) &optional play-list)
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/pause" 0))))

;(defmethod Reset-Player ((self (eql 'multiplayer)) &optional view)
;   (declare (ignore view))
;   (setf *multiplayer-file-to-play* nil)
;   t)





