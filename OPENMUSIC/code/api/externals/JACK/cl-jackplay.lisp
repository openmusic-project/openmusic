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

;; (require :cl-jack "cl-jack-load")
;; (require :libsndfile (make-pathname :directory (append om-api::*externals-directory* '("Audio" "libsndfile"))
;; 				    :name "libsndfile"))

;; open some soundfile

(defparameter *outchannels* 2)
(defparameter *jack-sndfile-handle* nil)

(setf somesounds
      '("/home/andersvi/lyd/andersvi/Floratone-1m.wav" "/home/andersvi/Musikk/Bruckner/Anton_Bruckner_Symphonie_Nr.7_E-Dur.ogg")
      ;'("/Users/bresson/FILES/Audio/Bassclarinet1.aif" "/Users/bresson/FILES/Audio/africa.aiff")
      )

(defun jack-open-sound (path)
  "sets the current sound read in, closes any previously open at the same handle for now"
  (if (probe-file path)
      (progn
	(sf::sf_close *jack-sndfile-handle*)
	(let* ((sfinfo (foreign-alloc '(:struct sf::SF_INFO)))
	       (handle (sf::sf_open (namestring path) sf::SFM_READ sfinfo))
	       (channels (cffi:foreign-slot-value sfinfo '(:struct sf::SF_INFO) 'sf::channels)))
	  (setf *outchannels* channels
		*jack-sndfile-handle* handle)))
      (format *error-output* "~&jack-open-sound - no such file: ~s~%" path)))

;; set up a jack ringbuffer for buffered I/O

(defconstant *sample-size* (foreign-type-size 'jack_default_audio_sample_t))
(defparameter *jack-ringbuffer* (jack-ringbuffer-create (* *outchannels* *sample-size* (ash 1 15))))
(defparameter *bytes-per-frame* (* *sample-size* *outchannels*))

;; DISK_WORK

(defparameter *can-process* nil)
(defparameter *jack-get-me-some* t)
(defparameter *producer* nil)
(defparameter *pause-reading* t)
(defparameter *stop-reading* t)
;;

(defun disk-to-ringbuffer-proc ()
  (with-foreign-object (*framebuf* '(:struct jack_ringbuffer_data_t))
    (loop
       (setf *jack-get-me-some* nil)
       (when *stop-reading* (return))
       (let ((read-frames-cnt 0))

	 (jack-ringbuffer-get-write-vector *jack-ringbuffer* *framebuf*)

	 (when (rb-data-len-p *framebuf* 0) ;fill 1st part of available ringbuffer
	   (let ((buf-available (floor (rb-data-len *framebuf* 0) *bytes-per-frame*)))
	     (setf read-frames-cnt
		   (sf::sf-readf-float *jack-sndfile-handle* (rb-data-buf *framebuf* 0) buf-available))

	     (when (rb-data-len-p *framebuf* 1) ;perhaps fill 2nd part of available ringbuffer?
	       (let ((buf-available (floor (rb-data-len *framebuf* 1) *bytes-per-frame*)))
		 (incf read-frames-cnt
		       (sf::sf-readf-float *jack-sndfile-handle* (rb-data-buf *framebuf* 1) buf-available))))))

	 (when (zerop read-frames-cnt)	;loop back to 0 in soundfile
	   (sf::sf_seek *jack-sndfile-handle* 0.d0 0))

	 ;; book-keeping
	 (jack-ringbuffer-write-advance *jack-ringbuffer* (* read-frames-cnt *bytes-per-frame*))
	 ;; wait for process-callback to poke me
	 (mp:process-wait-local (format nil "cl-jack diskin ~:[reading~;pausing~]" *pause-reading*)
				#'(lambda () *jack-get-me-some*))))))


;;(setf *stop-reading* t)

(defun jackplay-toggle-pause (&optional val)
  (if val
      (setf *pause-reading* val)
      (setf *pause-reading* (not *pause-reading*))))

;;(jackplay-toggle-pause)

;; ... disk work done

;; process-callback:

(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg (:pointer :void)))
  (declare (ignore arg))
  (cl-jack-handle-midi-seq nframes)	;plug to handle midi-seq
  (let ((read-count 0)
	(outs-arr (make-array *outchannels*)))
    (dotimes (n *outchannels*)
      (setf (aref outs-arr n) (jack-port-get-buffer (nth n *OM-jack-audio-output-ports*) nframes)))
    (with-foreign-object (outbuf 'jack_default_audio_sample_t (* nframes *outchannels*))
      (setf read-count
	    (jack-ringbuffer-read *jack-ringbuffer* outbuf (* nframes *sample-size* *outchannels*)))
      (if (and (plusp read-count) (mp:process-alive-p *producer*) (not *pause-reading*))
	  (dotimes (outframe nframes)
	    (dotimes (n *outchannels*)	;deinterleave frames
	      (setf (mem-aref (aref outs-arr n) 'jack_default_audio_sample_t outframe)
		    (mem-aref outbuf 'jack_default_audio_sample_t (+ (* outframe *outchannels*) n)))))
	  (dotimes (outframe nframes)
	    (dotimes (n *outchannels*)	
	      (setf (mem-aref (aref outs-arr n) 'jack_default_audio_sample_t outframe)
		    0.0)))
	  ))
    (setf *jack-get-me-some* t)
    (mp:process-poke *producer*))
  0)				 ;return 0 or get kicked out from jack


;; start 'disk-to-ringbuffer-proc (= play sound)

;(jack-open-sound (first somesounds))
;; (jack-open-sound (second somesounds))

#|
(progn  
  (setf *pause-reading* nil)
  (setf *stop-reading* nil)
  (setf *producer* (mp:process-run-function "cl-jack-producer-thread" '() 'disk-to-ringbuffer-proc)))
|#


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

;; 'silence' callback

(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg :pointer))
  (declare (ignore arg))
  (with-foreign-object (outbuf 'jack_default_audio_sample_t *outchannels*)
    (setf outs (loop for n in *OM-jack-audio-input-ports*
		  collect (jack-port-get-buffer n nframes)))
    (loop for i below nframes
       do (loop for out in outs
	     for n from 0
	     do
	       (setf (mem-aref out 'jack_default_audio_sample_t i) 0.0))))
  0)

|#
