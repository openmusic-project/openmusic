(in-package :om)


(defmethod get-score-player ((self scorepanel)) 
  (if (equal (get-edit-param (editor self) 'player) :microplayer) 'microplayer 'midishare))


#+(and cocoa om-osc-api) 
(add-assoc-player *general-player* 'microplayer)


;=====================================================
;MICROPLAYER PROCESS

(defvar *microosc-packets* nil)
(defvar *index-packets* 0)

(defvar *microplayer-in-port* nil)
(setf *microplayer-in-port* 3010)

(defvar *microplayer-out-port* nil)
(setf *microplayer-out-port* 3000)

(defvar *microplayer-host* nil)
(setf *microplayer-host* "127.0.0.1")

;; (defvar *max-buf-size* nil)
;; definie dans l'api
(setf *om-udp-max-buf-size* 500)


(defvar *micro-listen-process* nil)

(defun open-microplayer ()
  (unless *micro-listen-process*
    (setf *micro-listen-process* (om-start-osc-server *microplayer-in-port* *microplayer-host*  #'send-more-notes))))

(defun close-microplayer ()
  (when *micro-listen-process*
    (om-stop-osc-server *micro-listen-process*)
    (setf *micro-listen-process* nil)))

; (om-add-init-func 'open-microplayer)
; (om-add-exit-cleanup-func 'close-microplayer)


;=====================================================
;MICROPLAYER APP

(defvar *micro-player-app* nil)
(defvar *micro-player-path* nil)

(defun init-microplayer-app ()
  (setf *micro-player-path* 
        (or (probe-file (om-default-application-path '("MicroPlayer") "bm-microton"))
            (probe-file (om-external-app nil "MicroPlayer")))))

(om-add-init-func 'init-microplayer-app)


(defun launch-microplayer-app ()
  (unless (and *micro-player-app* (om-find-process *micro-player-app*))
    (let ((path *micro-player-path*))
      (if (or (null path) (not (probe-file path)))
        (om-message-dialog "MicroPlayer application not found. Please set the application path in OM preferences.") 
      (setf *micro-player-app* (om-run-application path))
      )))
  (open-microplayer))

; (launch-microplayer-app)


;=====================================================
;MICROPLAYER PROTOCOL

(defun micro-reset ()
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/reset")))
  (setf *microosc-packets* nil))

;================
(defun micro-start ()
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/start"))))

;================
(defun sort-micro-events ()
  (setf *microosc-packets* (sort *microosc-packets* '< :key 'second)))

;================


(defun send-200 ()
  (loop for i from 1 to 200
        while (< *index-packets* (length *microosc-packets*)) do
        (let ((event (copy-list (nth  *index-packets* *microosc-packets*))))
          (unless (zerop *index-packets*)
            (setf (nth 1 event) (- (nth 1 event) (second (nth  (- *index-packets* 1) *microosc-packets*)))))
          (setf *index-packets* (+ *index-packets* 1))
          (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  (list event))))
  )

;================


(defun send-more-notes (msg)
  (let ((message (om-decode-msg-or-bundle msg)))
  (when (string-equal (string (car message)) "/play.mt/more")
    (send-200))
  nil))
 
;================

(defmethod InitPlayingSeq ((player (eql 'microplayer)) dur &key (port nil))
  (setf *microosc-packets* nil)
  (setf *MidiShare-start-time* 1))
    
(defmethod FinalizePlayingSeq ((player (eql 'microplayer)) dur &key (port nil))
  (sort-micro-events)
  t)


(defmethod* PrepareToPlay ((player (Eql 'microplayer)) (self t) at &key  approx port interval voice)
   (declare (ignore approx))
   (call-next-method))

(defmethod* PrepareToPlay ((player (Eql 'microplayer)) (self measure) at &key approx port interval voice)
   (setf port (verify-port port))
   (loop for sub in (inside self) do
         (let ((objstart (+ at (offset->ms sub))))
           (if interval
             (let ((newinterval (interval-intersec interval 
                                                   (list objstart (+ objstart (get-obj-dur sub))))))
               (when newinterval
                 (PrepareToPlay player sub objstart 
                                :approx approx 
                                :port port
                                :interval interval
                                :voice voice)))
             (PrepareToPlay player sub objstart 
                            :approx approx 
                            :port port
                            :voice voice)))))


(defmethod* PrepareToPlay ((player (Eql 'microplayer)) (self note) at &key  approx port interval voice)
   (when (not (memq (tie self) '(continue end)))
     (let ((chan (chan self))
           (pitch (/ (approx-scale (get-current-scale approx) (midic self)) 100.0))
           (vel (vel self))
           (dur (- (real-dur self) 2))
           (date (+  *MidiShare-start-time* at)))
       (if interval
         (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
           (when newinterval
             (playoscnote chan pitch vel (- (second newinterval) (first newinterval) 1) 
                       (- (+  *MidiShare-start-time* (first newinterval)) 
                          (first interval)))))
         (playoscnote chan pitch vel dur date)))))


(defun playoscnote (chan pitch vel dur date)
   (push (list "/play.µt/fifos" date  pitch vel  dur chan) *microosc-packets*))


(defmethod Play-player ((self (eql 'microplayer)))
   (setf *index-packets* 0) 
   (send-200) 
   (micro-start)) 

(defmethod Continue-Player ((self (eql 'microplayer)))
    (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/continue"))))

(defmethod Pause-Player ((self (eql 'microplayer)))
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/pause"))))

(defmethod Stop-Player ((self (eql 'microplayer)) &optional view)
   (declare (ignore view))
   (micro-reset))

(defmethod Reset-Player ((self (eql 'microplayer)) &optional view)
   (declare (ignore view))
   (micro-reset))





