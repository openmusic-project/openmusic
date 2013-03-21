(in-package :om)


;; FIXME: just redefining method from microplayer.lisp here
(defmethod get-score-player ((self scorepanel)) 
  (if (equal (get-edit-param (editor self) 'player) :SCplayer)
      'scplayer
      'midishare))


#+linux
(add-assoc-player *general-player* 'scplayer)

(assoc-players *general-player*)


;;=====================================================
;;SCPLAYER PROCESS

(defvar *SCosc-packets* nil)
(defvar *index-packets* 0)

(defvar *SCplayer-in-port* nil)
(setf *SCplayer-in-port* 57120)

(defvar *SCplayer-out-port* nil)
;; TODO: get sclangs port after startup
(setf *SCplayer-out-port* 57120)
(setf *SCplayer-out-port* 57121)

(defvar *SCplayer-host* nil)
(setf *SCplayer-host* "127.0.0.1")


(setf *om-udp-max-buf-size* 500)

(defvar *SC-listen-process* nil)

(defun open-SCplayer ()
  (unless *SC-listen-process*
    (setf *SC-listen-process* (om-start-osc-server *SCplayer-in-port* *SCplayer-host*  #'SC-send-more-notes))))

(defun close-SCplayer ()
  (when *SC-listen-process*
    (om-stop-osc-server *SC-listen-process*)
    (setf *SC-listen-process* nil)))

;; (om-add-init-func 'open-SCplayer)
;; (om-add-exit-cleanup-func 'close-SCplayer)


;;=====================================================
;;SCPLAYER APP

(defvar *SC-player-app* nil)
(defvar *SC-player-path* nil)
(defvar *SC-player-pid* nil)

(defun init-SCplayer-app ()
  (setf *SC-player-path* 
        #-linux (or (probe-file (om-default-application-path '("SCPlayer") "bm-SCton"))
		    (probe-file (om-external-app nil "SCPlayer")))
	#+linux "scplayer"))

(om-add-init-func 'init-SCplayer-app)

(setf *SC-player-path* "sclang")
;; (om-cmd-line "ps aux | grep sclang")
;; (om-cmd-line "pkill sclang")
;; (setf *SC-player-pid* nil)

;; (mp:find-process-from-name "sclang")

(defun launch-SCplayer-app ()
  (unless *SC-player-pid*
    (multiple-value-bind (io err pid)
	(system:run-shell-command "sclang" :wait nil :input :stream :output :stream :error-output :stream)
      (setf *SC-player-pid* pid)
      (print (format nil "started sclang: pid: ~A" pid)))))

(setf *SC-player-pid* nil)

;; (launch-SCplayer-app)

;; (om-send-osc-bundle *SCplayer-out-port* *SCplayer-host*  '(("/play.sc_om/start" 1209 1210 12 13)))

;;=====================================================
;;SCPLAYER PROTOCOL

(defun SC-reset ()
  (om-send-osc-bundle *SCplayer-out-port* *SCplayer-host*  '(("/play.sc_om/reset")))
  (setf *SCosc-packets* nil))

;;================
(defun SC-start ()
  (om-send-osc-bundle *SCplayer-out-port* *SCplayer-host*  '(("/play.sc_om/start"))))

;;================
(defun sort-SC-events ()
  (setf *SCosc-packets* (sort *SCosc-packets* '< :key 'second)))

;;================

(defun SC-send-200 ()
  (loop for i from 1 to 200
     while (< *index-packets* (length *SCosc-packets*))
     do
       (let ((event (copy-list (nth  *index-packets* *SCosc-packets*))))
	 (unless (zerop *index-packets*)
	   (setf (nth 1 event) (- (nth 1 event) (second (nth  (- *index-packets* 1) *SCosc-packets*)))))
	 (setf *index-packets* (+ *index-packets* 1))
	 (om-send-osc-bundle *SCplayer-out-port* *SCplayer-host*  (list event)))))

;;================


(defun SC-send-more-notes (msg)
  (let ((message (om-decode-msg-or-bundle msg)))
    (when (string-equal (string (car message)) "/play.sc_om/more")
      (SC-send-200))
    nil))

;;================

(defmethod InitPlayingSeq ((player (eql 'SCplayer)) dur &key (port nil))
  (setf *SCosc-packets* nil)
  (setf *MidiShare-start-time* 1))

(defmethod FinalizePlayingSeq ((player (eql 'SCplayer)) dur &key (port nil))
  (sort-SC-events)
  t)

(defmethod* PrepareToPlay ((player (Eql 'SCplayer)) (self t) at &key  approx port interval voice)
  (declare (ignore approx))
  (call-next-method))

(defmethod* PrepareToPlay ((player (Eql 'SCplayer)) (self measure) at &key approx port interval voice)
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

(defmethod* PrepareToPlay ((player (Eql 'SCplayer)) (self note) at &key  approx port interval voice)
  (when (not (memq (tie self) '(continue end)))
    (let ((chan (chan self))
	  (pitch (/ (approx-scale (get-current-scale approx) (midic self)) 100.0))
	  (vel (vel self))
	  (dur (- (real-dur self) 2))
	  (date (+  *MidiShare-start-time* at))
	  )
      (if interval
	  (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
	    (when newinterval
	      (playoscnote chan pitch vel
			   (- (second newinterval) (first newinterval) 1) 
			   (- (+  *MidiShare-start-time* (first newinterval)) 
			      (first interval)))))
	  (playoscnote chan pitch vel dur date)))))

(defun playoscnote (chan pitch vel dur date)
  (push (list "/play.sc_om/fifos" date pitch vel dur chan) *SCosc-packets*))

(defmethod Play-player ((self (eql 'SCplayer)))
  (setf *index-packets* 0) 
  (SC-send-200) 
  (SC-start)) 

(defmethod Continue-Player ((self (eql 'SCplayer)))
  (om-send-osc-bundle *SCplayer-out-port* *SCplayer-host*  '(("/play.sc_om/continue"))))

(defmethod Pause-Player ((self (eql 'SCplayer)))
  (om-send-osc-bundle *SCplayer-out-port* *SCplayer-host*  '(("/play.sc_om/pause"))))

(defmethod Stop-Player ((self (eql 'SCplayer)) &optional view)
  (declare (ignore view))
  (SC-reset))

(defmethod Reset-Player ((self (eql 'SCplayer)) &optional view)
  (declare (ignore view))
  (SC-reset))
