;;===========================================================================
;;JACK API for Common Lisp/CFFI
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


(in-package :cl-jack)

(defvar *jack-midi-output-port* nil)
(defvar *jack-midi-input-port* nil)

;;; global pool of seqs for this client, for separate control [start/stop/pause...]:
(defun make-jack-seqs () (make-hash-table)) 
(defparameter *jack-seqs* (make-jack-seqs))

;;; event-seq is a hash-table, keys are frameno at jacks' start-of-period (ie: jack-last-frame-time)
(defun make-jack-seq () (make-hash-table))

;;; provide one default seq for simple debugging etc:
 ;default sequencer
(defvar *jack-seq*
  (setf (gethash '*jack-seq* *jack-seqs*) (make-jack-seq)))

;;; TIME HANDLING - SEQUENCING

;; (let ((start (jack-last-frame-time *CLJackClient*)))
;;   (print 'yo)
;;   (- start (jack-last-frame-time *CLJackClient*)))

(defun jack-period-now (&optional sek)
  (+ (jack-last-frame-time *CLJackClient*)
     (jack-get-buffer-size *CLJackClient*)
     (round (if sek (* sek (jack-get-sample-rate *CLJackClient*)) 0))))

;;; too late to schedule things inside current period, this looks up
;;; current frame with exactly one period latency:

(defun jack-frame-now (&optional sek)
  (round (+ (jack-frame-time *CLJackClient*)
	    (jack-get-buffer-size *CLJackClient*) 
	    (if sek (* sek (jack-get-sample-rate *CLJackClient*)) 0))))

(defun ms->frame (ms)
  (round (* ms (jack-get-sample-rate cl-jack::*CLJackClient*)) 1000))

(defun sec->frame (sec)
  (round (* sec (jack-get-sample-rate cl-jack::*CLJackClient*))))


(defun frame->period-offset (frame)
  "returns 2 frame nos: start of period & offset within period"
  (let ((bufsiz (jack-get-buffer-size *CLJackClient*)))
    (multiple-value-bind (n rem)
	(floor frame bufsiz)
      (values (* n bufsiz) rem))))

;;; MIDI EVENTS

(defconstant noteofftag #x80)
(defconstant noteontag #x90)
(defconstant programchangetag #xC0)

(defun make-midi-note-on-tag (&optional (channel 1))
  (dpb channel (byte 4 0) noteontag))

(defun make-midi-note-off-tag (&optional (channel 1))
  (dpb channel (byte 4 0) noteofftag))

(defun make-midi-programchange-tag (&optional (channel 1))
  (dpb channel (byte 4 0) programchangetag))

;; TODO: expand with support for channels, messages, CC, sysex...

(defun jack-add-event-this-period (seq period event)
  (setf (gethash period seq)
	(sort (nconc (gethash period seq) (list event))
	      #'(lambda (a b) (< (car a) (car b))))))

(defun jack-add-event-this-frame (seq frame event)
  (push event (gethash frame seq)))

;; old version keying on period-boundaries.  This works, and is
;; presumably more efficient, but warnings from the jack-devels say
;; there is no guarantee what any future period-boundary might be.
;;
;;
;; (defun seqhash-midi-note-on (seq frame noteno velocity &optional (channel 1))
;;   "times (start, dur) in sec.; noteno, vel, chan is standard midi"
;;   (multiple-value-bind (period offset)
;;       (frame->period-offset frame)
;;     (let ((noteon (list offset (make-midi-note-on-tag channel) noteno velocity channel)))
;;       (jack-add-event-this-period seq period noteon))))

;; (defun seqhash-midi-note-off (seq frame noteno velocity &optional (channel 1))
;;   (multiple-value-bind (period offset)
;;       (frame->period-offset frame)
;;     (let ((noteoff (list offset (make-midi-note-off-tag channel) noteno velocity channel)))
;;       (jack-add-event-this-period seq period noteoff))))

;; version just hasing on frame-number

(defun seqhash-midi-note-on (seq frame noteno velocity &optional (channel 1))
  "times (start, dur) in sec.; noteno, vel, chan is standard midi"
  (let ((noteon (list frame (make-midi-note-on-tag channel) noteno velocity channel)))
    (jack-add-event-this-frame seq frame noteon)))

(defun seqhash-midi-note-off (seq frame noteno velocity &optional (channel 1))
  (let ((noteoff (list frame (make-midi-note-off-tag channel) noteno velocity channel)))
    (jack-add-event-this-frame seq frame noteoff)))

;; interface to higher-level funcs:

(defun jack-play-event (seq start dur noteno &optional (vel 80) (chan 1))
  (let* ((startframe (jack-frame-now start))
	 (endframe (+ startframe (sec->frame dur))))
    (seqhash-midi-note-on seq startframe noteno vel chan)
    (seqhash-midi-note-off seq endframe noteno 0 chan)))

(defun jack-all-notes-off (seq)
  (let ((sounding-notes '()))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (mapc #'(lambda (ev) (push (cddr ev) sounding-notes)) val))
	     seq)
    (clrhash seq)
    (mapc #'(lambda (ev)
	      (apply #'seqhash-midi-note-off seq (jack-frame-now) ev))
	  sounding-notes)))

(defun jack-all-notes-off-and-kill-seq (seq)
  (jack-all-notes-off seq)
  (sleep (float (/ 2
		   (jack-get-buffer-size *CLJACKCLIENT*)
		   (jack-get-sample-rate *CLJACKCLIENT*))))
  (remhash seq *jack-seqs*))

(defun jack-seq-hush-this-seq (seq)
  (jack-all-notes-off seq))

(defun jack-seq-hush-all-seqs ()
  (maphash #'(lambda (key seq)
	       (declare (ignore key))
	       (jack-all-notes-off-and-kill-seq seq))
	   *jack-seqs*))


(defparameter *playing* t)		;nil=shut up
;; (setf *playing* nil)

;; old version looking up period-boundary keys:
;;
;; (defun lookup-queue-at-frame (jack-period seq)
;;   (let ((queue-time (or (gethash 'queuetime seq) 0)))
;;     (gethash (+ jack-period queue-time) seq)))
;;
;;
;; (defun play-from-seq (port-buf seq)
;;   (when *playing*
;;     (let ((this-period (jack-last-frame-time *CLJackClient*)))
;;       (let ((notes-this-period (lookup-queue-at-frame this-period seq)))
;; 	(when notes-this-period
;; 	  (remhash this-period seq)
;; 	  (loop for note in notes-this-period
;; 	     for offset = (first note)
;; 	     do 
;; 	     (print (list 'found-note offset note))
;; 	     (let ((buffer (jack-midi-event-reserve port-buf offset 3))) ;offset inside period
;; 	       (unless (null-pointer-p buffer)
;; 		 (setf (mem-aref buffer :int8 0) (second note) ;tag
;; 		       (mem-aref buffer :int8 1) (third note)  ;noteno
;; 		       (mem-aref buffer :int8 2) (fourth note))))))))))
					;vel

(defun play-from-seq (port-buf seq)
  (when *playing*
    (let ((this-period (jack-last-frame-time *CLJackClient*)))
      (loop for offset from 0 below (jack-get-buffer-size *CLJACKCLIENT*)
	 for key from this-period
	 for notes = (gethash key seq)
	 when notes
	 do 
	   (remhash key seq)
	   (dolist (note notes)
	     (let ((buffer (jack-midi-event-reserve port-buf offset 3))) ;offset inside period
	       (unless (null-pointer-p buffer)
		 (setf (mem-aref buffer :int8 0) (second note) ;tag
		       (mem-aref buffer :int8 1) (third note)  ;noteno
		       (mem-aref buffer :int8 2) (fourth note)))))))))

;; callback function handles seq-events, plugged into jacks
;; process-callback

(defun cl-jack-handle-event-seqs (nframes)
  (let ((port-buf (jack-port-get-buffer *jack-midi-output-port* nframes)))
    (jack-midi-clear-buffer port-buf)
    ;;(play-from-seq port-buf *jack-seq*)
    (maphash #'(lambda (key seq)
		 (declare (ignore key))
		 (play-from-seq port-buf seq))
	     *jack-seqs*)))



(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg :pointer))
  (declare (ignore arg))
  (cl-jack-handle-event-seqs nframes)
  0)


;; get up and running

(unless *CLJackClient*
  (setf *CLJackClient* (jack-client-open "CLJack" JackNullOption 0)))

;; (loop for i from 0
;;      and port = (mem-aref (jack-get-ports *CLJackClient* "" "midi" 0) :string i)
;;      while port
;;      collect port)

(unless *jack-midi-output-port*
  (setf *jack-midi-output-port*
	(let ((port (jack-port-register *CLJackClient*
					"midiout"
					*jack-default-midi-type*
					(foreign-enum-value 'jackportflags :jackportisoutput)
					0)))
	  (if (zerop (pointer-address port)) ;0 if not allocated
	      (error "*jack-midi-output-port* for Jack not allocated")
	      port))))

(jack-set-process-callback *CLJackClient* (callback cl-jack-process-callback) 0)
(jack-activate *CLJackClient*)

;;(Jack-deactivate *CLJackClient*)

#|


(setf midiports (jack-get-ports *CLJackClient*  "" "midi" 0))

(loop for i from 0
     and port = (mem-aref midiports :string i)
     while port
     collect port)
("CLJack:midiout" "CLJack:midiout" "fluidsynth:midi")


(defcfun "jack_connect" :int
  (client :pointer)
  (source-port :string)
  (destination-port :string))

(defcfun "jack_port_name" :string
  (port (:pointer jack_port_t)))

(jack-connect *CLJackClient*
	      (jack-port-name *jack-midi-output-port*)
	      "fluidsynth:midi")

(jack-disconnect *CLJackClient*
	      (jack-port-name *jack-midi-output-port*)
	      "fluidsynth:midi")

(defcfun "jack_port_by_name" :pointer
  (client :pointer)
  (port-name :string))

(jack-client-close *CLJackClient*)

(jack-frame-now)

(clrhash *jack-seq*)

(seqhash-midi-note-on *jack-seq* (jack-frame-now) (setf *thisnote* 60) 127 1)
(seqhash-midi-note-off *jack-seq* (jack-frame-now) *thisnote* 127 1)

(with-hash-table-iterator (get-note *jack-seq*)
  (loop (multiple-value-bind (more? time notes) (get-note)
	  (unless more? (return nil))
	  (if (> time 10000)
	      (return nil)
	      (if (zerop (mod time 2))
		  (remhash time *jack-seq*))))))

(defun play-some-notes (&optional (tempo 0.1) (dur 8))
  ;;(clrhash *jack-seq*)
  (loop with offset = 0
     for sek from 0 below dur by tempo
     do
       (let* ((start (jack-frame-now sek))
	      (end (+ start 10000))
	      (channel (random 16)))
	 (progn
	   (seqhash-midi-note-on *jack-seq* start (setf *thisnote* (+ 20 (random 100))) 80 channel)
	   (seqhash-midi-note-off  *jack-seq* end *thisnote* 0 channel)))))

(defun play-some-notes (&optional (tempo 0.1) (dur 8))
  (loop with note = 0
     for sek from 0  by tempo
     do
       (let* ((start (jack-frame-now sek))
	      (end (jack-frame-now (+ sek tempo))))
	 (when  (>= sek dur)
	   (loop-finish)
	   (seqhash-midi-note-off *jack-seq* end (+ 40 note) 50 0))
	 (progn
	   (seqhash-midi-note-on *jack-seq* start (+ 40 (setf note (mod (incf note) 60))) 80 0)
	   (seqhash-midi-note-off *jack-seq* end (+ 40 note) 50 0)
	   ))))

(let ((rytme 1/32))
  (play-some-notes rytme (+ 6 rytme)))

(jack-all-notes-off *jack-seq*)

(jack-midi-stop-all)

(seqhash-midi-note-on *jack-seq* (jack-frame-now) 60 127 0)
(seqhash-midi-note-off *jack-seq* (jack-frame-now) 60 127 0)

(dotimes (i 80)
  (let* ((note (mod (+ 40 i (random 80)) 120))
	 (rytme (/ i 21))
	 (dur (* rytme 1)))
    (seqhash-midi-note-on *jack-seq* (jack-frame-now rytme) note  100 0)
    (seqhash-midi-note-off *jack-seq* (jack-frame-now (+ rytme dur)) note 0 0)
    ))



(loop repeat 3 do (play-some-notes (+ 1/30 (random 0.1)) 12))

(hash-table-count *jack-seq*)
(hash-table-size *jack-seq*)
(clrhash *jack-seq*)

;; example of (non-callback-based) rt-scheduling

(setf *jack-midi-playing* nil)
(setf *jack-midi-playing* t)
(setf *play-queue* t)
(setf *play-queue* nil)

(let ((note 40))
  (loop
     (when (not *jack-midi-playing*)
       (return))
     (let ((rytme (/ 1 64)))
       (when *play-queue*
	 (let ((note (+ 40 (mod (+ (incf note) (* 12 (random 3))) 60)))
	       (rytme (/ 1 64))
	       (dur (* rytme 8)))
	   (seqhash-midi-note-on *jack-seq* (jack-frame-now rytme) note 90 0)
	   (seqhash-midi-note-off *jack-seq* (jack-frame-now (+ rytme dur)) note 0 0)))
       (sleep rytme))
     ))

(cl-user::set-up-profiler :packages '(cl-jack))
(cl-user::profile (loop repeat 300 do (play-some-notes (+ 1/30 (random 0.1)) 12)))
(env:start-environment)

|#
