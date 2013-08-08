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

(defparameter *OM-midi-output-port* nil)
(defparameter *OM-midi-input-port* nil)

;;; event-pool is a hash-table, keys are frame at jacks start-of-period

(defun make-om-seq () (make-hash-table))
(defparameter *om-seq* (make-om-seq))

;;; TIME HANDLING - SEQUENCING

(defun framenow (&optional (sek 0))
  (round (+ (jack-last-frame-time *OMJackClient*)
	    (jack-get-buffer-size *OMJackClient*)
	    (* sek (jack-get-sample-rate *OMJackClient*)))))

(defun ms->frame (ms)
  (round (* ms (jack-get-sample-rate cl-jack::*OMJackClient*)) 1000))

(defun sec->frame (sec)
  (round (* sec (jack-get-sample-rate cl-jack::*OMJackClient*))))

;; (list (- (framenow 1) (framenow 0))
;;       (- (framenow 1.0) (framenow 0.0)))


(defun frame->period-offset (time)
  "returns 2 frame nos: start of period & offset within period"
  (multiple-value-bind (n rem)
      (floor time (jack-get-buffer-size *OMJackClient*))
    (values (* n (jack-get-buffer-size *OMJackClient*)) rem)))

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

;; TODO: expand with support for channels, messages more of midi to
;; come...

(defun seqhash-note-on (seq time noteno velocity &optional (channel 1))
  (multiple-value-bind (period offset)
      (frame->period-offset time)
    (let ((noteon (list offset (make-midi-note-on-tag channel) noteno velocity channel)))
      (setf (gethash period seq)
	    (sort (nconc (gethash period seq) (list noteon))
		  #'(lambda (a b) (< (car a) (car b))))))))

(defun seqhash-note-off (seq time noteno velocity &optional (channel 1))
  (multiple-value-bind (period offset)
      (frame->period-offset time)
    (let ((noteoff (list offset (make-midi-note-off-tag channel) noteno velocity channel)))
      (setf (gethash period seq)
	    (sort (nconc (gethash period seq) (list noteoff))
		  #'(lambda (a b) (< (car a) (car b))))))))

;; used by player-methods to play objects w. jack-midi.
(defun jack-midi-play-event (start dur noteno &optional (vel 80) (chan 1))
  "times (start, dur) in sec.; noteno, vel, chan is standard midi"
  (let* ((startframe (framenow start))
	 (endframe (+ startframe (sec->frame dur))))
    (seqhash-note-on  *om-seq* startframe noteno vel chan)
    (seqhash-note-off *om-seq* endframe noteno 0 chan)))

(defun all-notes-off ()
  (mp:process-run-function "hush" nil
   #'(lambda () (let ((sounding-notes '()))
		  (maphash #'(lambda (key val)
			       (declare (ignore key))
			       (mapc #'(lambda (ev) (push (cddr ev) sounding-notes)) val))
			   *om-seq*)
		  (clrhash *om-seq*)
		  (mapc #'(lambda (ev)
			      (apply #'seqhash-note-off *om-seq* (framenow) ev))
			  sounding-notes)
		  t))))

(defun jack-midi-stop-all ()
  (all-notes-off))

(defparameter *playing* t)		;nil=shut up
;; (setf *playing* nil)

(defun lookup-queue-at-frame (jack-period seq)
  (let ((queue-time (or (gethash 'queuetime seq) 0)))
    (gethash (+ jack-period queue-time) seq)))

(defun play-from-seq (port-buf seq)
  (when *playing*
    (let ((this-period (jack-last-frame-time *OMJackClient*)))
      (let ((notes-this-period (lookup-queue-at-frame this-period seq)))
	(when notes-this-period
	  (remhash this-period seq)
	  (loop for note in notes-this-period
	     for offset = (first note)
	     do 
	     (let ((buffer (jack-midi-event-reserve port-buf offset 3))) ;offset inside period
	       (unless (null-pointer-p buffer)
		 (setf (mem-aref buffer :int8 0) (second note) ;tag
		       (mem-aref buffer :int8 1) (third note)  ;noteno
		       (mem-aref buffer :int8 2) (fourth note)))))))))) ;vel

;; callback function handles midi-events, plugged into jacks
;; process-callback
(defun cl-jack-handle-midi-seq (nframes)
  (let ((port-buf (jack-port-get-buffer *OM-midi-output-port* nframes)))
    (jack-midi-clear-buffer port-buf)
    (play-from-seq port-buf *om-seq*)))

(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg :pointer))
  (declare (ignore arg))
  (cl-jack-handle-midi-seq nframes)
  0)


;; get up and running

(unless *OMJackClient*
  (setf *OMJackClient* (jack-client-open "OpenMusic" JackNullOption 0)))

;; (loop for i from 0
;;      and port = (mem-aref (jack-get-ports *OMJackClient* "" "midi" 0) :string i)
;;      while port
;;      collect port)

(unless *OM-midi-output-port*
  (setf *OM-midi-output-port*
	(let ((port (jack-port-register *OMJackClient*
					"midiout"
					*jack-default-midi-type*
					(foreign-enum-value 'jackportflags :jackportisoutput)
					0)))
	  (if (zerop (pointer-address port)) ;0 if not allocated
	      (error "*OM-midi-output-port* for Jack not allocated")
	      port))))

(jack-set-process-callback *OMJackClient* (callback cl-jack-process-callback) 0)
(jack-activate *OMJackClient*)

;;(jack-deactivate *OMJackClient*)

#|


(setf midiports (jack-get-ports *OMJackClient*  "" "midi" 0))

(loop for i from 0
     and port = (mem-aref midiports :string i)
     while port
     collect port)
("OpenMusic:midiout" "OpenMusic:midiout" "fluidsynth:midi")


(defcfun "jack_connect" :int
  (client :pointer)
  (source-port :string)
  (destination-port :string))

(defcfun "jack_port_name" :string
  (port (:pointer jack_port_t)))

(jack-connect *OMJackClient*
	      (jack-port-name *OM-midi-output-port*)
	      "fluidsynth:midi")

(jack-disconnect *OMJackClient*
	      (jack-port-name *OM-midi-output-port*)
	      "fluidsynth:midi")

(defcfun "jack_port_by_name" :pointer
  (client :pointer)
  (port-name :string))

(jack-client-close *OMJackClient*)

(seqhash-note-on *om-seq* (framenow) (setf *thisnote* (+ 40 (random 40))) 127 1)
(seqhash-note-off *om-seq* (framenow) *thisnote* 127 1)

(with-hash-table-iterator (get-note *om-seq*)
  (loop (multiple-value-bind (more? time notes) (get-note)
	  (unless more? (return nil))
	  (if (> time 10000)
	      (return nil)
	      (if (zerop (mod time 2))
		  (remhash time *om-seq*))))))

(defun play-some-notes (&optional (tempo 0.1) (dur 8))
  ;;(clrhash *om-seq*)
  (loop with offset = 0
     for sek from 0 below dur by tempo
     do
       (let* ((start (framenow sek))
	      (end (+ start 10000))
	      (channel (random 16)))
	 (progn
	   (seqhash-note-on *om-seq* start (setf *thisnote* (+ 20 (random 100))) 80 channel)
	   (seqhash-note-off *om-seq* end *thisnote* 50 channel)))))

(defun play-some-notes (&optional (tempo 0.1) (dur 8))
  (loop with note = 0
     for sek from 0  by tempo
     do
       (let* ((start (framenow sek))
	      (end (framenow (+ sek tempo))))
	 (when  (>= sek dur)
	   (loop-finish)
	   (seqhash-note-off *om-seq* end (+ 40 note) 50 0))
	 (progn
	   (seqhash-note-on *om-seq* start (+ 40 (setf note (mod (incf note) 60))) 80 0)
	   (seqhash-note-off *om-seq* end (+ 40 note) 50 0)
	   ))))

(let ((rytme 1/32))
  (play-some-notes rytme (+ 6 rytme)))

(all-notes-off)
(progn
  (clrhash *om-seq*)
  (all-notes-off 0))

(jack-midi-stop-all)

(seqhash-note-on *om-seq* (framenow) 40 127 0)
(seqhash-note-off *om-seq* (framenow) 40 127 0)

(dotimes (i 80)
  (let* ((note (mod (+ 40 i (random 80)) 120))
	 (rytme (/ i 21))
	 (dur (* rytme 1)))
    (seqhash-note-on *om-seq* (framenow rytme) note  100 0)
    (seqhash-note-off *om-seq* (framenow (+ rytme dur)) note 0 0)
    ))



(loop repeat 3 do (play-some-notes (+ 1/30 (random 0.1)) 12))

(hash-table-count *om-seq*)
(hash-table-size *om-seq*)
(clrhash *om-seq*)



(setf *jack-midi-playing* nil)
(setf *jack-midi-playing* t)

(let ((note 40))
  (loop
     (when (not *jack-midi-playing*)
       (return))
     (let* ((note (+ 20 (mod (+ (incf note) (random 20)) 100)))
	    (rytme (/ 3 64))
	    (dur (* rytme 4)))
       (seqhash-note-on *om-seq* (framenow rytme) note 90 0)
       (seqhash-note-off *om-seq* (framenow (+ rytme dur)) note 0 0)
       (sleep rytme))
     ))



(cl-user::set-up-profiler :packages '(cl-jack))
(cl-user::profile (loop repeat 300 do (play-some-notes (+ 1/30 (random 0.1)) 12)))
(env:start-environment)

|#
