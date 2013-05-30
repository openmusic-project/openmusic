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

(require :cffi "../FFI/load-cffi.lisp")
(load "cl-jack")

(in-package :cl-jack)

(defparameter *OM-midi-output-port* nil)
(defparameter *OM-midi-input-port* nil)

(unless *OMJackClient*
  (setq *OMJackClient* (jack-client-open "OpenMusic" JackNullOption 0)))

(setf *OM-midi-output-port*
      (let ((port (jack-port-register
		   *OMJackClient*
		   "midiout"
		   *jack-default-midi-type*
		   (foreign-enum-value 'jackportflags :jackportisoutput)
		   0)))
	(if (zerop (pointer-address port)) ;0 if not allocated
	    (error "*OM-midi-output-port* for Jack not allocated")
	    port)))

;;; event-pool is a hash-table, keys are frame at jacks start-of-period

(defun make-om-seq () (make-hash-table))
(defparameter *om-seq* (make-om-seq))

;;; TIME HANDLING - SEQUENCING

(defparameter *jack-buffer-size* (jack-get-buffer-size *OMJackClient*))

(defun framenow (&optional (sek 0))
  (round (+ (jack-last-frame-time *OMJackClient*)
			    *jack-buffer-size*
			    (* sek (jack-get-sample-rate *OMJackClient*)))))

;; (list (- (framenow 1) (framenow 0))
;;       (- (framenow 1.0) (framenow 0.0)))


(defun frame->period-offset (time)
  "returns 2 frame nos: start of period & offset within period"
  (multiple-value-bind (n rem)
      (floor time *jack-buffer-size*)
    (values (* n *jack-buffer-size*) rem)))

;;; MIDI EVENTS

(defconstant noteofftag #x80)
(defconstant noteontag #x90)
(defconstant programchangetag #xC0)

(defun make-midi-note-on-tag (&optional (channel 0))
  (dpb channel (byte 4 0) noteontag))

(defun make-midi-note-off-tag (&optional (channel 0))
  (dpb channel (byte 4 0) noteofftag))

(defun make-midi-programchange-tag (&optional (channel 0))
  (dpb channel (byte 4 0) programchangetag))

;; TODO: expand with support for channels, messages more of midi to
;; come...

(defun seqhash-note-on (seq time noteno velocity &optional (channel 0))
  (multiple-value-bind (period offset)
      (frame->period-offset time)
    (let ((noteon (list offset (make-midi-note-on-tag channel) noteno velocity channel)))
      (setf (gethash period seq)
	    (sort (nconc (gethash period seq) (list noteon)) ;TODO - prio-q
		  #'(lambda (a b) (< (car a) (car b))))))))

(defun seqhash-note-off (seq time noteno velocity &optional (channel 0))
  (multiple-value-bind (period offset)
      (frame->period-offset time)
    (let ((noteoff (list offset (make-midi-note-off-tag channel) noteno velocity channel)))
      (setf (gethash period seq)
	    (sort (nconc (gethash period seq) (list noteoff))
		  #'(lambda (a b) (< (car a) (car b))))))))

(defun all-notes-off (&optional (channel 0))
  (loop with frame = (framenow 0.1)	;give sort a chance...
     for i from 0 to 127
     do
       (seqhash-note-off *om-seq*
			 frame	
			 i 0 channel)))



(defparameter *playing* t)		;nil=shut up
;; (setf *playing* nil)

(defun play-from-seq (port-buf seq)
  (when *playing*
    (let ((this-period (jack-last-frame-time *OMJackClient*)))
      (let ((notes-this-period (gethash this-period seq)))
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

(defun cl-jack-handle-midi-seq (nframes)
  (let ((port-buf (jack-port-get-buffer *OM-midi-output-port* nframes)))
    (jack-midi-clear-buffer port-buf)
    (play-from-seq port-buf *om-seq*)))

(defcallback process :int ((nframes jack_nframes_t) (arg :pointer))
  (declare (ignore arg))
  (cl-jack-handle-midi-seq nframes)
  0)

(jack-set-process-callback *OMJackClient* (callback process) 0)
(jack-activate *OMJackClient*)

(unless *OMJackClient*
  (jack-activate *OMJackClient*))	;get up and running

;;(jack-deactivate *OMJackClient*)

#|



(setq nframes (jack-get-buffer-size *OMJackClient*))
(jack-get-sample-rate *OMJackClient*)
(jack-client-close *OMJackClient*)
(jack-port-get-buffer *OM-midi-output-port* nframes)
(setf port-buf (jack-port-get-buffer *OM-midi-output-port* nframes))
(jack-midi-clear-buffer port-buf)
(setf buffer (jack-midi-event-reserve port-buf 0 3))

(pointer-address *OM-midi-output-port*)
(pointer-address port-buf)
(pointer-address buffer)

(setf midiports (jack-get-ports *OMJackClient* "" "midi" 0))

(loop for i from 0
     and port = (mem-aref midiports :string i)
     while port
     collect port)

(jack-client-close *OMJackClient*)


(seqhash-note-on *om-seq* (framenow) (setf *thisnote* (+ 40 (random 40))) 127 1)
(seqhash-note-off *om-seq* (framenow) *thisnote* 127 1)

(progn
  (loop for i from 20 to 127 by 1
     do
       (seqhash-note-on *om-seq* (framenow (random 0.1)) i 80 1))
  (sleep 0.4)
  (all-notes-off 1))

(all-notes-off 1)
(maphash #'(lambda (key val) (print (list key val))) *om-seq*)

(with-hash-table-iterator (get-note *om-seq*)
  (loop (multiple-value-bind (more? time notes) (get-note)
	  (unless more? (return nil))
	  (if (> time 10000)
	      (return nil)
	      (if (zerop (mod time 2))
		  (remhash time *om-seq*))))))

(maphash #'(lambda (key val) (remhash key *om-seq*)) *om-seq*)
(clrhash *om-seq*)

(seqhash-note-on *om-seq* (framenow 0.0) 70 127 0)

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
  (loop with offset = 0
     with note = 0
     for sek from 0  by tempo
     do
       (let* ((start (framenow sek))
	      (end (framenow 3)))
	 (when  (>= sek dur)
	   (loop-finish)
	   (seqhash-note-off *om-seq* end (+ 40 note) 50 0))
	 (progn
	   (seqhash-note-on *om-seq* start (+ 40 (setf note (mod (incf note) 60))) 60 0)
	   (seqhash-note-off *om-seq* end (+ 40 note) 50 0)
	   ))))

(seqhash-note-on *om-seq* (framenow) 40 127 0)
(seqhash-note-off *om-seq* (framenow) 40 127 0)

(dotimes (i 80)
  (let* ((note (mod (+ 40 i (random 80)) 120))
	 (rytme (/ i 21))
	 (dur (* rytme 1)))
    (seqhash-note-on *om-seq* (framenow rytme) note  100 0)
    (seqhash-note-off *om-seq* (framenow (+ rytme dur)) note 0 0)
    ))

(let ((rytme 1/8))
  (play-some-notes rytme (+ 3 rytme)))

(loop repeat 3 do (play-some-notes (+ 1/30 (random 0.1)) 12))

(hash-table-count *om-seq*)
(hash-table-size *om-seq*)
(clrhash *om-seq*)



(cl-user::set-up-profiler :packages '(cl-jack))
(cl-user::profile (loop repeat 300 do (play-some-notes (+ 1/30 (random 0.1)) 12)))
(env:start-environment)

|#
