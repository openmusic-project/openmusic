;;===========================================================================
;;SuperCollider player engine (events/midi) for OM.
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


(defmethod player-name ((self (eql :sceventplayer))) "sceventplayer")
(defmethod player-desc ((self (eql :sceventplayer))) "external SuperCollider player for OM")
(defmethod player-special-action ((self (eql :sceventplayer))) (launch-scplayer-app))
(defmethod player-type ((player (eql :sceventplayer))) :UDP)


;;=====================================================
;;scplayer process

(defvar *scosc-packets* '())
(defvar *index-packets* 0)

(defvar *scplayer-host* "127.0.0.1")
(defvar *scplayer-lang-port* "not set yet")

;;; NetAddr.langPort is reported by sclang after bootup to ensure no
;;; clashes:

(defvar *sclang-tmp-file* "/tmp/omsc.lang.port.tmp") 

;; sit in a loop until sclang tells us its langPort:

(defparameter *scplayer-read-port-proc* nil)

;; wait in a loop to file with port-info is ready
(defun sc-read-new-sclang-port (file)
  (loop
     (mp:process-wait (format nil "waiting for file ~a" file) 'probe-file file)
     (format *standard-output* "~&reading new sclang port from ~a: " file)
     (let ((temp-file (format nil "~a.temp" file))) 
       (rename-file file temp-file) ;;atomic read, just in case
       (with-open-file (istream temp-file)
	 (loop (let ((line (read-line istream nil)))
		 (unless line
		   (return))
		 (write-line line *standard-output*)
		 (setf *scplayer-lang-port* (parse-integer line :junk-allowed t))
		 (format *standard-output* "~a ~%" *scplayer-lang-port*))))
       (delete-file temp-file)			 ;delete file
       (mp:process-kill *scplayer-read-port-proc*) ;kill proc when done
       )))

(defun sc-read-port-from-file ()
  (delete-file *sclang-tmp-file*)
  (setf *scplayer-read-port-proc*
	(mp:process-run-function "reading sclang-port from file" ()
				 #'sc-read-new-sclang-port
				 *sclang-tmp-file*)))

;;=====================================================
;;scplayer app


(defvar *sc-player-pid* nil)
(defvar *sc-player-io* nil)
(defvar *sc-cmd-line* nil)
(defvar *sc-setup-file* nil)

(defun shell-command-returning-output (cmd &optional (return-last t) echo)
  (multiple-value-bind (out)
      (sys:run-shell-command cmd :wait nil :output :stream)
    (let (val lastval)
      (with-open-stream (out out)
	(loop 
	   (if (setf val (read-line out nil nil))
	       (when return-last (setf lastval val))
	       (return lastval))
	   (when echo (print val)))))))


(defvar *sc-player-path* (shell-command-returning-output "which sclang" t t))

(defun init-scplayer-app ()
  (let ((sc-setup-file (or *sc-setup-file* (omroot "resources;SC;SC3-OM-Player.sc")))
	(sc-player-path (or *sc-player-path*
			    (capi::prompt-for-file "path to sclang executable:" :pathname (om-default-application-path "" "sclang")))))
    (progn
      (setf *sc-cmd-line* (format nil "~a ~a" sc-player-path sc-setup-file)))))


(defun launch-scplayer-app ()
  (unless *sc-player-pid*
    (when (and (streamp *sc-player-io*) (open-stream-p *sc-player-io*)) (close *sc-player-io*))
    (multiple-value-bind (io err pid)
	(system:run-shell-command *sc-cmd-line*
				  :wait nil
				  :input :stream
				  :output :stream
				  :error-output nil)
      (declare (ignore err))
      (setf *sc-player-pid* pid)
      (setf *sc-player-io* io)
      (print (format nil "started sclang: pid: ~a" pid)))))

;; (launch-scplayer-app)



(defun sc-init-app ()
  (init-scplayer-app)
  (launch-scplayer-app)
  (sc-read-port-from-file)
  (enable-sceventplayer)
  (add-player-for-object simple-score-element :sceventplayer))

(om-add-init-func 'sc-init-app)

;;(setf *om-udp-max-buf-size* 500)

(defun stop-and-cleanup-scplayer-app ()
  (progn
    (when *sc-player-pid* (sys:run-shell-command (format nil "kill -9 ~A"  *sc-player-pid*) :wait t))
    (when (open-stream-p *sc-player-io*)
      (close *sc-player-io*)
      (setf *sc-player-io* nil))
    (setf *sc-player-pid* nil)
    (sys:run-shell-command (format nil "pkill -9 scsynth") :wait t) ;just rub everything for now...
    (sys:run-shell-command (format nil "pkill -9 sclang") :wait t)
    ))

;; (stop-and-cleanup-scplayer-app)
(om-add-exit-cleanup-func 'stop-and-cleanup-scplayer-app)

(defun scplayer-send-cmd (cmd)
  (progn (unless *sc-player-pid* (launch-scplayer-app))
	 (format *sc-player-io* "~A~%" cmd)))

(defun get-from-shell (stream &optional outputstr)
  (do ((ch (read-char-no-hang stream)
	   (read-char-no-hang stream)))
      ((null ch))
    (write-char ch outputstr)))

;; (scplayer-send-cmd "NetAddr.langPort")
;; (get-from-shell *sc-player-io*)

;;=====================================================
;;scPLAYER PROTOCOL

(defun sc-reset ()
  (om-send-osc-bundle *scplayer-lang-port* *scplayer-host*  '(("/play.sc_om/reset")))
  (setf *scosc-packets* nil))

;;================
(defun sc-start ()
  (om-send-osc-bundle *scplayer-lang-port* *scplayer-host*  '(("/play.sc_om/start"))))

;;================
(defun sort-sc-events ()
  (setf *scosc-packets* (sort *scosc-packets* '< :key 'second)))

;;================

(defun sc-send-200 ()
  (loop for i from 1 to 200
     while (< *index-packets* (length *scosc-packets*))
     do
       (let ((event (copy-list (nth  *index-packets* *scosc-packets*))))
	 (unless (zerop *index-packets*)
	   (setf (nth 1 event) (- (nth 1 event) (second (nth  (- *index-packets* 1) *scosc-packets*)))))
	 (setf *index-packets* (+ *index-packets* 1))
	 (om-send-osc-bundle *scplayer-lang-port* *scplayer-host*  (list event)))))

;;================

(defun sc-send-more-notes (msg)
  (let ((message (om-decode-msg-or-bundle msg)))
    (when (string-equal (string (car message)) "/play.sc_om/more")
      (sc-send-200))
    nil))

;;================

(defmethod InitPlayingSeq ((player (eql :sceventplayer)) dur &key (port nil))
  (setf *scosc-packets* nil)
  (setf *MidiShare-start-time* 1))

(defmethod FinalizePlayingSeq ((player (eql :sceventplayer)) dur &key (port nil))
  (sort-sc-events)
  t)

(defmethod* PrepareToPlay ((player (Eql 'scplayer)) (self t) at &key  approx port interval voice)
  (declare (ignore approx))
  (call-next-method))

(defmethod* PrepareToPlay ((player (Eql 'scplayer)) (self measure) at &key approx port interval voice)
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

(defmethod* PrepareToPlay ((player (Eql 'scplayer)) (self note) at &key  approx port interval voice)
  (when (not (memq (tie self) '(continue end)))
    (let ((chan (chan self))
	  (pitch (/ (approx-scale (get-current-scale approx) (midic self)) 100.0))
	  (vel (vel self))
	  (dur (real-dur self))
	  (date at)
	  )
      (if interval
	  (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
	    (when newinterval
	      (sc-playoscnote chan pitch vel
			   (- (second newinterval) (first newinterval) 1) 
			   (- (first newinterval) (first interval)))))
	  (progn
	    ;;(print (list 'sc-playoscnote chan pitch vel dur date))
	    (sc-playoscnote chan pitch vel dur date))))))

(defun sc-playoscnote (chan pitch vel dur date)
  ;;(print (list "/play.sc_om/fifos" date pitch vel dur chan))
  (push (list "/play.sc_om/fifos" date pitch vel dur chan) *scosc-packets*))

(defmethod prepare-to-play ((engine (eql :sceventplayer)) (player omplayer) object at interval)
  (setf *index-packets* 0) 
  (sc-send-200) 
  (sc-start)) 

(defvar *sceventplayers* (make-hash-table))
(defstruct sceventplayer-track (paused))

(defmethod player-pause-object ((engine (eql :sceventplayer)) object &key interval)
  (unless (sceventplayer-track-paused (gethash object *sceventplayers*))
    (om-send-osc-bundle *scplayer-lang-port* *scplayer-host*  '(("/play.sc_om/pause")))
    (setf (sceventplayer-track-paused (gethash object *sceventplayers*)) t)))

(defmethod player-pause ((engine (eql :sceventplayer)) &optional playlist)
  (when playlist
    (dolist (obj playlist) (player-pause-object engine obj)))
  nil)

(defmethod player-continue-object ((engine (eql :sceventplayer)) object &key interval)
  (when (sceventplayer-track-paused (gethash object *sceventplayers*))
    (om-send-osc-bundle *scplayer-lang-port* *scplayer-host*  '(("/play.sc_om/continue"))) 
    (setf (sceventplayer-track-paused (gethash object *sceventplayers*)) nil)))

(defmethod player-continue ((engine (eql :sceventplayer)) &optional playlist)
  (when playlist
    (dolist (snd playlist) (player-continue-object engine snd)))
  nil)

(defmethod player-stop-object ((engine (eql :sceventplayer)) object &key interval)
  (sc-reset))

(defmethod player-stop ((engine (eql :sceventplayer)) &optional playlist)
  (when playlist
    (dolist (obj playlist) (player-stop-object engine snd))))

