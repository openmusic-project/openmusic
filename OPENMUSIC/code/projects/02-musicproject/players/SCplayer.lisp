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

(defvar *SCplayer-out-port* nil)
;; TODO: get sclangs port after startup
(setf *SCplayer-out-port* 57130)
;; (setf *SCplayer-out-port* 57121)

(defvar *SCplayer-host* nil)
(setf *SCplayer-host* "127.0.0.1")

(setf *om-udp-max-buf-size* 500)

;;=====================================================
;;SCPLAYER APP

(defvar *SC-cmd-line* nil)
(defvar *SC-player-path* nil)

(defvar *SC-player-pid* nil)
(defvar *SC-player-io* nil)
(defvar *SC-setup-file* nil)

(setf *SC-setup-file* (namestring (make-pathname :directory (pathname-directory *load-pathname*) :name "scratch_MIDI.sc")))

(defun init-SCplayer-app ()
  (progn
    (setf *SC-player-path* 
	  #-linux (capi::prompt-for-file "Path to sclang executable:" :pathname (om-default-application-path "" "sclang"))
	  #+linux "sclang")
    (setf *SC-cmd-line*
	  (format nil "~A -u ~A ~A"
		  *SC-player-path* *SCplayer-out-port* *SC-setup-file*))))

(om-add-init-func 'init-SCplayer-app)

(setf *SC-player-pid* nil)
(setf *SC-player-io* nil)

(defun launch-SCplayer-app ()
  (unless *SC-player-pid*
    (multiple-value-bind (io err pid)
	(system:run-shell-command *SC-cmd-line*
				  :wait nil
				  :input :stream
				  :output :stream
				  :error-output :stream)
      (setf *SC-player-pid* pid)
      (setf *SC-player-io* io)
      (print (format nil "started sclang: pid: ~A" pid)))))

;; (launch-SCplayer-app)
(om-add-init-func 'launch-SCplayer-app)

(defun stop-and-cleanup-SCplayer-app ()
  (progn
    (when *SC-player-pid* (sys:run-shell-command (format nil "kill -9 ~A"  *SC-player-pid*)))
    (sys:run-shell-command (format nil "pkill scsynth"))
    (setf *SC-player-pid* nil)
    (setf *SC-player-io* nil)))

;; (stop-and-cleanup-scplayer-app)
(om-add-exit-cleanup-func 'stop-and-cleanup-scplayer-app)



(defun SCplayer-send-cmd (cmd)
  (progn (unless *SC-player-pid* (launch-SCplayer-app))
	 (format *sc-player-io* "~A~%" cmd)))

(defun get-from-shell (stream &optional outputstr)
  (do ((ch (read-char-no-hang stream)
	   (read-char-no-hang stream)))
      ((null ch))
    (write-char ch outputstr)))

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
	  (date at)
	  )
      (if interval
	  (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
	    (when newinterval
	      (playoscnote chan pitch vel
			   (- (second newinterval) (first newinterval) 1) 
			   (- (first newinterval) (first interval)))))
	  (playoscnote chan pitch vel dur date)))))

(defun playoscnote (chan pitch vel dur date)
  ;; (print (list chan pitch vel dur date))
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
