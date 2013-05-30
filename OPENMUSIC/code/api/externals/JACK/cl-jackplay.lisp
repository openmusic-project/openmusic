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

(in-package :cl-jack)
(require :cffi "../FFI/load-cffi.lisp")
(require :cl-jack "cl-jack")
(require :libsndfile "../Audio/libsndfile/libsndfile")


(cffi:defcfun ("sf_seek" sf_seek) :double
  (sndfile :pointer)
  (frames size_t)
  (whence :int))

;; open some soundfile

(setf mysound "/home/andersvi/lyd/andersvi/Floratone-1m.wav")
(setf mysound "/home/andersvi/Musikk/Bruckner/Anton_Bruckner_Symphonie_Nr.7_E-Dur.ogg")

(sf::sf_close sndfile-handle)

(coerce 40000 'double-float)

(sf_seek sndfile-handle 0 0)

(progn
  (sf::sf_close sndfile-handle)
  (let* ((path mysound)
	 (datatype 'jack_default_audio_sample_t)
	 (sfinfo (foreign-alloc 'sf::SF_INFO))
	 (handle (sf::sf_open (namestring path) sf::SFM_READ sfinfo))
	 (channels (cffi:foreign-slot-value sfinfo 'sf::SF_INFO 'sf::channels)))
    (defparameter *outchannels* channels)
    (defparameter *current-loop* 0)
    (defparameter sndfile-handle handle)))

;; set up a jack ringbuffer for buffered I/O

(defconstant *sample-size* (foreign-type-size 'jack_default_audio_sample_t))
(defconstant *rb-size* 16384)
(defparameter *jack-ringbuffer* (jack-ringbuffer-create (* *outchannels* *sample-size* *rb-size*)))
 
;; DISK_WORK

(defparameter *can-process* nil)
(defparameter *jack-get-me-some* t)
(defparameter *producer* nil)

;;(setf *stop-reading* t)

(let ((bytes-per-frame (* *sample-size* *outchannels*)))
  (with-foreign-object (*framebuf* 'jack_ringbuffer_data_t)
    (setf *stop-reading* nil)
    (setf *producer*
	  (mp:process-run-function
	   "cl-jack producer thread" nil 
	   #'(lambda ()
	       (loop
		  ;; wait for process-callback to poke me
		  (mp:process-wait-local "cl-jack producing" #'(lambda () *jack-get-me-some*))

		  (when *stop-reading* (return))
		
		  (let ((read-frames-cnt 0))

		    (jack-ringbuffer-get-write-vector *jack-ringbuffer* *framebuf*)

		    (when (rb-data-len-p *framebuf* 0)	;fill 1st part of available ringbuffer
		      (let ((buf-available (floor (rb-data-len *framebuf* 0) bytes-per-frame)))
			(setf read-frames-cnt
			      (sf::sf-readf-float sndfile-handle (rb-data-buf *framebuf* 0) buf-available))

			(when (rb-data-len-p *framebuf* 1) ;perhaps fill 2nd part of available ringbuffer?
			  (let ((buf-available (floor (rb-data-len *framebuf* 1) bytes-per-frame)))
			    (incf read-frames-cnt
				  (sf::sf-readf-float sndfile-handle (rb-data-buf *framebuf* 1) buf-available))))))

		    (when (zerop read-frames-cnt) ;loop back to 0 in soundfile
		      (incf *current-loop*)
		      (sf::sf_seek sndfile-handle 0.d0 0))

		    ;; book-keeping
		    (jack-ringbuffer-write-advance *jack-ringbuffer* (* read-frames-cnt bytes-per-frame))
		    (setf *can-process* t)
		    (setf *jack-get-me-some* nil))))))))

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
      (when (and (plusp read-count) *can-process*)
	(dotimes (outframe nframes)
	  (dotimes (n *outchannels*)	;deinterleave frames
	    (setf (mem-aref (aref outs-arr n) 'jack_default_audio_sample_t outframe)
		  (mem-aref outbuf 'jack_default_audio_sample_t (+ (* outframe *outchannels*) n)))))))
    (setf *jack-get-me-some* t)
    (when (mp:process-alive-p *producer*)
      (mp:process-poke *producer*)))
  0)					;return 0 or get kicked out from jack

;; plug callback into jack-client


#|

(jack-deactivate *OMJackClient*)
(jack-set-process-callback *OMJackClient* (callback cl-jack-process-callback) 0)
(jack-activate *OMJackClient*)


;; various cleanups

(jack-deactivate *OMJackClient*)
(sf::sf_close sndfile-handle)

;; 'silence' callback

(defcallback om-play-sf :int ((nframes jack_nframes_t) (arg :pointer))
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
