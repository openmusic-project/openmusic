(in-package :cl-jack)

;;===========================================================================
;;JACK Player for Common Lisp/CFFI
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
;; 

;;(require :cl-jack "cl-jack-load")

(load "cl-jack-load.lisp")

;; open some soundfile

(defparameter *read-sound-channels* 1)
(defparameter *jack-sndfile-handle* nil)

(setf somesounds
      '("/home/andersvi/lyd/andersvi/Floratone-1m-mono.wav"
	"/home/andersvi/lyd/andersvi/Floratone-1m.wav" "/home/andersvi/Musikk/Bruckner/Anton_Bruckner_Symphonie_Nr.7_E-Dur.ogg"
      ;'("/Users/bresson/FILES/Audio/Bassclarinet1.aif" "/Users/bresson/FILES/Audio/africa.aiff")
      ))

(defun jack-open-sound (path)
  "sets the current sound read in, closes any previously open at the same handle for now"
  (if (probe-file path)
      (progn
	(if (boundp '*jack-sndfile-handle*) (sf::sf_close *jack-sndfile-handle*))
	(let ((sfinfo (foreign-alloc '(:struct sf::SF_INFO))))
	  (setf *jack-sndfile-handle* (sf::sf_open (namestring path) sf::SFM_READ sfinfo)
		*read-sound-channels* (cffi:foreign-slot-value sfinfo '(:struct sf::SF_INFO) 'sf::channels)))
	*jack-sndfile-handle*)
      (format *error-output* "~&jack-open-sound - no such file: ~s~%" path)))

(defun jack-open-sound (path)
  "sets the current sound read in, closes any previously open at the same handle for now"
  (if (probe-file path)
      (progn
	(if (boundp '*jack-sndfile-handle*) (sf::sf_close *jack-sndfile-handle*))
	(with-foreign-object (sfinfo '(:struct sf::SF_INFO))
	  (setf *jack-sndfile-handle* (sf::sf_open (namestring path) sf::SFM_READ sfinfo)
		*read-sound-channels* (cffi:foreign-slot-value sfinfo '(:struct sf::SF_INFO) 'sf::channels)))
	*jack-sndfile-handle*)
      (format *error-output* "~&jack-open-sound - no such file: ~s~%" path)))

;; using one jack ringbuffer pr. port/chan/track for buffered I/O:

(defconstant *sample-size* (foreign-type-size 'jack_default_audio_sample_t))


;; DISK_WORK

(defparameter *can-process* nil)
(defparameter *jack-get-me-some* t)
(defparameter *producer* nil)
(defparameter *pause-reading* nil)
(defparameter *stop-reading* nil)

(defun jackplay-toggle-pause (&optional val)
  (if val
      (setf *pause-reading* val)
      (setf *pause-reading* (not *pause-reading*)))
  (if *pause-reading*
      :pausing
      :not-pausing))


(defparameter *jack-debug* nil)
(defparameter *jack-stats* nil)

(defparameter *jack-ringbuffers*
  (make-array (length *OM-jack-audio-output-ports*)
	      :initial-contents (loop repeat 2 collect (jack-ringbuffer-create (* *sample-size* (ash 1 15))))))

(defun disk-to-ringbuffer-proc (sf-handle &optional (disk-io-frames 4096) (chans 2))
  (let ((disk-io-bytes (* disk-io-frames *sample-size*)))
    (with-foreign-object (frames-buffer 'jack_default_audio_sample_t (* chans disk-io-bytes))
      (loop
	 (unless *stop-reading*
	   (do ()
	       ((< (jack-ringbuffer-write-space (aref *jack-ringbuffers* 0)) disk-io-bytes))

	     (let ((read-frames-cnt (sf::sf-readf-float sf-handle frames-buffer disk-io-frames)))

	       (when (< read-frames-cnt disk-io-frames) (sf::sf_seek sf-handle 0.d0 0))

	       (dotimes (ch chans)
		 (let ((rb (aref *jack-ringbuffers* ch)))
		   (do ((i ch (+ i chans)))
		       ((>= i (* read-frames-cnt chans)) t)
		     (jack-ringbuffer-write rb
					    (mem-aptr frames-buffer 'jack_default_audio_sample_t i)
					    *sample-size*))))))
	   (setf *jack-get-me-some* nil))

	 ;; wait for process-callback to poke me
	 (mp:process-wait-local (format nil "cl-jack diskin ~:[reading~;pausing~]" *pause-reading*)
				#'(lambda () *jack-get-me-some*))))))


(setf *jack-debug* t)
(setf *jack-debug* nil)

(setf *jack-stats* t)

(defvar *rbresetlock* (mp::make-lock :name "rb-reset-lock"))

(jack-ringbuffer-reset (aref *jack-ringbuffers* 1))

(mp:with-lock (*rbresetlock*)
    (loop for rb across *jack-ringbuffers*
       do (jack-ringbuffer-reset rb))
    (sleep 0.2))


(mp:with-lock (*rbresetlock*)
  (if (boundp '*jack-sndfile-handle*) (sf::sf_close *jack-sndfile-handle*))
  ;; (jack-open-sound (first somesounds))       
  ;; (jack-open-sound (second somesounds))  
  (jack-open-sound (third somesounds))
  (and *producer*
       (mp:process-alive-p *producer*)
       (mp:process-kill *producer*))
  (loop for rb across *jack-ringbuffers*
     do (jack-ringbuffer-reset rb))
  
  (setf *producer* (mp:process-run-function "cl-jack-producer-thread" '()
					    'disk-to-ringbuffer-proc
					    *jack-sndfile-handle*
					    4096
					    *read-sound-channels*))
  ;; delay release of lock for new reader to get set up 
  (sleep 0.1)
  )

;; (jackplay-toggle-pause)

;; (setf *stop-reading* t)
;; (setf *stop-reading* nil)

;; process-callback, "non-interleaving", strait copy from ringbuffers - 1 pr. port :

(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg (:pointer :void)))
  (declare (ignore arg))
  ;;(funcall #'cl-jack-handle-midi-seq nframes) ;plug to handle midi-seq
  (let ((to-read (* nframes *sample-size*)))
    (with-foreign-object (inbuf 'jack_default_audio_sample_t nframes)
      (loop for outport in *OM-jack-audio-output-ports*
	 for rb across *jack-ringbuffers*
	 for outbuf = (jack-port-get-buffer outport nframes)
	 do
	   (unless (mp:with-lock (*rbresetlock* nil 0)
		     (let ((read-count (jack-ringbuffer-read rb inbuf to-read)))
	       
		       (when (and (= read-count to-read)
				  (mp:process-alive-p *producer*))
			 (foreign-funcall "memcpy" :pointer outbuf :pointer inbuf size_t to-read)))
		     t)
	     ;; output zero if i can't grab lock
	     (foreign-funcall "memset" :pointer outbuf jack_default_audio_sample_t 0.0 size_t to-read)))))
  (setf *jack-get-me-some* t)
  (when (mp:process-alive-p *producer*)
    (mp:process-poke *producer*))
  ;;return 0 or get kicked out from jack
  0)

(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg :pointer))
  (declare (ignore arg))
  (cl-jack-handle-midi-seq nframes)
  (loop for inport in *OM-jack-audio-input-ports*
     for outport in *OM-jack-audio-output-ports*
     do
       (foreign-funcall "memset"
			:pointer (jack-port-get-buffer outport nframes)
			jack_default_audio_sample_t 0.0
			size_t (* nframes (foreign-type-size 'size_t))))
  0)


;;(jack-activate *OMJackClient*)
;; (jack-open-sound (second somesounds))


#|
;; plug new callback into jack-client. note: current callback can be
;; re-evaluated while everything is running to provide changed process-callback

(jack-deactivate *OMJackClient*)
(jack-set-process-callback *OMJackClient* (callback cl-jack-process-callback) 0)
(jack-activate *OMJackClient*)


;; various cleanups

(jack-deactivate *OMJackClient*)
(sf::sf_close *jack-sndfile-handle*)
(jack-client-close *OMJackClient*)

*jack-sndfile-handle*

(sys:run-shell-command "jack_lsp")

|#
