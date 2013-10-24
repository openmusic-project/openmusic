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

;; open some soundfile

(defparameter *jack-sndfile-handle* nil)

(unless (find :libsndfile *features*)
  (error "jackplay depends on libsndfile"))

(defun jack-open-sound (path)
  "sets the current sound read in, closes any previously open at the same handle for now"
  (let (sf-handle sf-chans)
    (cond ((probe-file path)
	   (if (boundp '*jack-sndfile-handle*) (sf::sf_close *jack-sndfile-handle*))
	   (with-foreign-object (sfinfo '(:struct sf::SF_INFO))
	     (setf sf-handle (sf::sf_open (namestring path) sf::SFM_READ sfinfo)
		   sf-chans (cffi:foreign-slot-value sfinfo '(:struct sf::SF_INFO) 'sf::channels)))
	   (values sf-handle sf-chans))
	  (t (format *error-output* "~&jack-open-sound - no such file: ~s~%" path) nil))))

;; DISK_WORK

(defparameter *cl-jack-is-reading* t)
(defparameter *outchannels* 2)
(defparameter *outbufs-arr* (make-array *outchannels*))
(defconstant *sample-size* (foreign-type-size 'jack_default_audio_sample_t))
(defparameter *bytes-per-frame* (* *sample-size* *outchannels*))

;; struct holding various data to handle sounds

(defun print-jack-sf (obj stream depth)
  (declare (ignore depth))
  (print-unreadable-object (obj stream)
    (format stream "jack-sound: ~S playing: ~A proc: ~A"
	    (file-namestring (jack-sf-path obj))
	    (jack-sf-playing? obj)
	    (mp:process-state (jack-sf-disk-proc obj)))))

(defstruct (jack-sf (:print-function print-jack-sf))
  path
  sound-file-handle
  chans
  ringbuffer
  disk-proc
  poker
  playing?
  start					;nil or position (millisec.)
  loop?)				;nil or (start . end)


;;; global list of jack-sf objects, looked up in disk-threads and Jacks
;;; server-callback process

(defparameter *jack-sounds* nil)

;;; control: open, play, stop, pause, unpause, seek, loop, close, clenaup...

(defun cl-jack-seek (sf frame)
  (sf::sf_seek sf frame 0))

(defun cl-jack-play-sound (soundfile &optional start loop?)
  (multiple-value-bind (mysf chans) (jack-open-sound soundfile)
    (when mysf
      (let ((jack-sf (make-jack-sf :path soundfile
				   :sound-file-handle mysf
				   :chans chans
				   :ringbuffer (jack-ringbuffer-create (* *sample-size* *outchannels* (ash 1 15)))
				   :playing? t
				   :start (if start (ms->frame start))
				   :loop? loop?)))
	(when (numberp start) (cl-jack-seek mysf (ms->frame start)))
	(let ((thisproc (mp:process-run-function (format nil "cl-jack-producer ~S" (file-namestring soundfile))
						 nil
						 'disk-to-ringbuffer-proc
						 jack-sf)))
	  (setf (jack-sf-disk-proc jack-sf) thisproc)
	  (pushnew jack-sf *jack-sounds*)
	  jack-sf)))))


;; (defparameter *somesounds*
;;   '("/home/andersvi/lyd/andersvi/Floratone-1m.wav"
;;     "/home/andersvi/Musikk/Bruckner/Anton_Bruckner_Symphonie_Nr.7_E-Dur.ogg"))

;; (setf (jack-sf-playing? (first *jack-sounds*)) nil)
;; (setf (jack-sf-playing? (first *jack-sounds*)) t)

;; (cl-jack-play-sound (first *somesounds*) 50000 '(58000 . 0))
;; (cl-jack-play-sound (first *somesounds*))

;; (setf (jack-sf-loop? (first *jack-sounds*)) nil)
;; (setf (jack-sf-loop? (first *jack-sounds*)) t)
;; (setf (jack-sf-loop? (first *jack-sounds*)) '(50000 . 0))
;; (cl-jack-seek (jack-sf-sound-file-handle (first *jack-sounds*)) (ms->frame 58000))
;; (cl-jack-seek (jack-sf-sound-file-handle (first *jack-sounds*)) (ms->frame 58000))

(defun cl-jack-close-sound (sound)
  (let ((snd (find sound *jack-sounds*)))
    (cond (snd
	   (setf *jack-sounds* (remove snd *jack-sounds*))
	   (setf (jack-sf-playing? snd) nil)
	   (foreign-free (jack-sf-ringbuffer snd))
	   (sf::sf_close (jack-sf-sound-file-handle snd))
	   (mp:process-kill (jack-sf-disk-proc snd)))
	  (t (warn "didnt find sound: ~A in *jack-sounds*" snd)))))

;; (cl-jack-close-sound (first *jack-sounds*))


(defun n-sounds-playing-now (sounds)
  (count-if #'jack-sf-playing? sounds))

(defun n-sounds-pausing-now (sounds)
  (count-if-not #'jack-sf-playing? sounds))

;; (n-sounds-playing-now *jack-sounds*)
;; (n-sounds-pausing-now *jack-sounds*)
;; (setf (jack-sf-playing? (first *jack-sounds*)) t)

(defun jackplay-toggle-read (&optional sound (val nil val-provided-p))
  ;; toggles gate on read-from-disk-threads:
  (cond (sound (if val-provided-p
		   (setf (jack-sf-playing? sound) val)
		   (setf (jack-sf-playing? sound) (not (jack-sf-playing? sound))))
	       sound)
	(t (if val-provided-p	;toggle all sounds
	       (setf *cl-jack-is-reading* val)
	       (setf *cl-jack-is-reading* (not *cl-jack-is-reading*)))
	   (if *cl-jack-is-reading*
	       :reading
	       :pausing))))

;; (jackplay-toggle-read (first *jack-sounds*))
;; (jackplay-toggle-read)

(defun cl-jack-sounds-playing-now (sounds)
  (loop for snd in sounds
       when (jack-sf-playing? snd)
       collect snd))

;; (cl-jack-sounds-playing-now *jack-sounds*)
;; (jackplay-toggle-read (car (cl-jack-sounds-playing-now *jack-sounds*)))
;; (jackplay-toggle-read (car (last *jack-sounds*)))
;; (jackplay-toggle-read (first *jack-sounds*))
;; (jackplay-toggle-read (second *jack-sounds*))


;; this one: straight copy of interleaved data to ringbuffer-struct,
;; handing de-interleaving to server-thread (callback) :

(defparameter *jack-get-me-some* (make-hash-table))

(defun disk-to-ringbuffer-proc (jack-sf)
  ;;(declare (optimize (float 0) (speed 3)))
  (let ((ringbuffer (jack-sf-ringbuffer jack-sf))
	(sf-handle (jack-sf-sound-file-handle jack-sf)))
    (with-foreign-object (framebuf '(:struct jack_ringbuffer_data_t))
      (loop
	 (let ((sf-playing? (jack-sf-playing? jack-sf)))
	   (when sf-playing?
	     (let ((read-frames-cnt 0))

	       (jack-ringbuffer-get-write-vector ringbuffer framebuf)

	       ;; fill 1st part of available ringbuffer
	       (when (rb-data-len-p framebuf 0)
		 (let ((buf-available (floor (rb-data-len framebuf 0) *bytes-per-frame*)))
		   (setf read-frames-cnt
			 (sf::sf-readf-float sf-handle (rb-data-buf framebuf 0) buf-available)))

		 ;; fill 2nd part of available ringbuffer if available
		 (when (rb-data-len-p framebuf 1)
		   (let ((buf-available (floor (rb-data-len framebuf 1) *bytes-per-frame*)))
		     (incf read-frames-cnt
			   (sf::sf-readf-float sf-handle (rb-data-buf framebuf 1) buf-available)))))

	       (when (zerop read-frames-cnt) ;at end: loop or quit
		 (let ((looping (jack-sf-loop? jack-sf)))
		   (if looping
		       (cl-jack-seek sf-handle (if (consp looping) (ms->frame (car looping)) 0) 0)
		       (cl-jack-close-sound jack-sf))))

	       ;; book-keeping
	       (jack-ringbuffer-write-advance ringbuffer (* read-frames-cnt *bytes-per-frame*))

	       (setf (jack-sf-poker jack-sf) nil)))
	     ;; wait for process-callback to poke me
	     (mp:process-wait-local (format nil "cl-jack diskin ~:[pausing~;reading~]"
					    (and sf-playing? *cl-jack-is-reading*))
				    #'(lambda () (and sf-playing? (jack-sf-poker jack-sf)))))))))

(defun read-from-ringbuffer-to-outbufs (rb nframes out-channels)
  (let ((buf (foreign-alloc 'jack_default_audio_sample_t :count (* nframes out-channels)))
	read-count)
    (setf read-count (jack-ringbuffer-read rb buf (* nframes *sample-size* out-channels)))
    (list read-count buf)))


(defun cl-jack-write-samples-from-all-ringbuffers (nframes)
  ;;(declare (optimize (float 0) (speed 3)))

  ;; allocate buffer to write to, one pr. out-chan/port:
  (loop for port in *CL-jack-audio-output-ports*
     for i from 0
     do (setf (aref *outbufs-arr* i) (jack-port-get-buffer port nframes)))

  (let ((ampscaling (/ 1.0 (max 1 (n-sounds-playing-now *jack-sounds*))))
	(readbufs (mapcar #'(lambda (rb)
			      (read-from-ringbuffer-to-outbufs rb nframes *outchannels*))
			  (mapcar #'jack-sf-ringbuffer (cl-jack-sounds-playing-now *jack-sounds*)))))
    (loop for outbuf across *outbufs-arr*
       for ch from 0
       do (loop for out from 0 below nframes
	     for in from ch by *outchannels*
	     do (setf (mem-aref outbuf 'jack_default_audio_sample_t out)
		      (loop with scaling = ampscaling
			 for (read-count buf) in readbufs
			 sum (if (plusp read-count)
				 (* (mem-aref buf 'jack_default_audio_sample_t in) scaling)
				 0.0)))))
    ;; free some buffers
    (loop for (nil buf) in readbufs do (foreign-free buf))))

;; "silence" process-callback:

(defun cl-jack-write-silence (nframes)
  (dolist (outport *CL-jack-audio-output-ports*)
    (foreign-funcall "memset"
		     :pointer (jack-port-get-buffer outport nframes)
		     jack_default_audio_sample_t 0.0
		     size_t (* nframes (foreign-type-size 'size_t)))))

(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg (:pointer :void)))
  (declare (ignore arg))
  (cl-jack-handle-event-seqs nframes)	;plug to handle midi-seq
  (progn
    (cl-jack-write-silence nframes)	;fill with zero if nothing else comes in...
    (if (plusp (n-sounds-playing-now *jack-sounds*)) (cl-jack-write-samples-from-all-ringbuffers nframes))
    ;; wake up disk-threads to push into ringbuffers
    (mapc #'(lambda (this-sound)
	      (let ((readproc (jack-sf-disk-proc this-sound)))
		(setf (jack-sf-poker this-sound) t)
		(when (and (mp:process-alive-p readproc) (jack-sf-playing? this-sound) *cl-jack-is-reading*)
		  (mp:process-poke readproc))))
	  *jack-sounds*))

  ;;return 0 or get kicked out from jack
  0)

;; (mapcar #'(lambda (sf) (list (jack-sf-poker sf)
;; 			     (jack-sf-disk-proc sf)))
;; 	*jack-sounds*)

#|
;; plug new callback into jack-client. note: current callback can be
;; re-evaluated dynamically

(progn
  (jack-deactivate *CLJackClient*)
  (jack-set-process-callback *CLJackClient* (callback cl-jack-process-callback) 0)
  (jack-activate *CLJackClient*))
|#
