(in-package :om)


#+(and cocoa om-osc-api) 
(add-assoc-player *general-player* 'multiplayer)

(defvar *multiplayer-in-port* nil)
(setf *multiplayer-in-port* 7474)

(defvar *multiplayer-out-port* nil)
(setf *multiplayer-out-port* 7374)

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

(defmethod InitPlayingSeq ((player (eql 'multiplayer)) dur &key (port nil)) t)
    
(defmethod FinalizePlayingSeq ((player (eql 'multiplayer)) dur &key (port nil)) t)


(defmethod* PrepareToPlay ((player (Eql 'multiplayer)) (self sound) at &key  approx port interval voice)
   (declare (ignore approx))
   
   (setf *multiplayer-file-to-play* (namestring (om-sound-file-name self)))

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
   (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/open" (namestring (om-sound-file-name self))))
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
   )

(defmethod Play-player ((self (eql 'multiplayer)))
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/play" 1))))

(defmethod Stop-Player ((self (eql 'multiplayer)) &optional view)
   (declare (ignore view))
   (when *multiplayer-file-to-play*
     (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/play" 0))))

(defmethod Pause-Player ((self (eql 'multiplayer)))
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/pause" 1))))

(defmethod Continue-Player ((self (eql 'multiplayer)))
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/pause" 0))))

(defmethod Reset-Player ((self (eql 'multiplayer)) &optional view)
   (declare (ignore view))
   (setf *multiplayer-file-to-play* nil)
   t)





