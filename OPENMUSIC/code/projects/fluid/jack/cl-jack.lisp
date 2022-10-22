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

;;(hqn-web::browse "http://jackaudio.org/files/docs/html/files.html")

(in-package :cl-jack)



(defparameter *jack-loaded* nil)
 
(defun cl-jack-init-jack ()
  (define-foreign-library libjack
    (t (:default "libjack")))
  (setf *jack-loaded*
	(handler-case (progn (use-foreign-library libjack) t)
	  (error () (progn (print (format nil "could not load foreign-library libjack"))
			   nil)))))

;;; MOST OF THE BELOW IS FFI-WRAPPERS FOR THE JACK-API

;;; JACK.H - CLIENTS, PORTS...

(defcfun "jack_get_version_string" :string)
;;(jack-get-version-string)

(defctype size_t :unsigned-int)
(defctype jack_nframes_t :uint32)
(defctype jack_port_t :pointer)
(defctype jack_options_t :pointer)
(defctype jack_time_t :unsigned-long)	;:unsigned-long = :uint64?
(defctype jack_midi_data_t :unsigned-char)

(defconstant *jack-default-audio-type* "32 bit float mono audio")
(defconstant *jack-default-midi-type* "8 bit raw midi")
(defctype jack_default_audio_sample_t :float)

(defcenum jackoptions
  (:jacknulloption #0x00)
  (:jacknostartserver #0x01)
  (:jackuseexactname #0x02)
  (:jackservername #0x04)
  (:JackLoadName #0x08)
  (:JackLoadInit #0x10)
  (:JackSessionID #0x20))

(defcenum jackportflags
  (:JackPortIsInput  #0x1)
  (:JackPortIsOutput  #0x2)
  (:JackPortIsPhysical  #0x4)
  (:JackPortCanMonitor  #0x8)
  (:JackPortIsTerminal  #0x10))

(defparameter JackNullOption (foreign-enum-value 'jackoptions :jacknulloption))

(defcfun "jack_client_name_size" :int)

(defcvar "input_port" jack_port_t)

(defcfun "jack_client_open" :pointer
  (name :string)
  (option :int)
  (status :int))

(defcfun "jack_get_sample_rate" :int
  (client :pointer))

(defcfun "jack_port_type_get_buffer_size" size_t
  (client :pointer)
  (port-type :string))

(defcfun "jack_get_buffer_size" jack_nframes_t
  (client :pointer))

(defcfun "jack_get_client_name" :string
  (client :pointer))

(defcfun "jack_port_get_buffer" :pointer
  (port :pointer)
  (frames jack_nframes_t))

(defcfun "jack_port_name" :string
  (port (:pointer jack_port_t)))

(defcfun "jack_connect" :int
  (client :pointer)
  (source-port :string)
  (destination-port :string))

(defcfun "jack_disconnect" :int
  (client :pointer)
  (source-port :string)
  (destination-port :string))

(defcfun "jack_get_ports" :pointer
  (client :pointer)
  (port_name_pattern :string)
  (type_name_pattern :string)
  (flags :unsigned-long))

(defcfun "jack_port_register" :pointer
  (client :pointer)
  (port-name (:string :encoding :utf-8))
  (port-type (:string :encoding :utf-8))
  (flags :unsigned-long)
  (buffer-size :unsigned-long))

(defcfun "jack_client_close" :int
  (client :pointer))

(defcfun "jack_activate" :int
  (client :pointer))

(defcfun "jack_deactivate" :int
  (client :pointer))

(defcfun "jack_set_process_callback" :int
  (client :pointer)
  (process_callback :pointer)
  (arg :int))

(defcfun "jack_midi_clear_buffer" :void
  (port-buffer :pointer))

(defcfun "jack_midi_event_reserve" :pointer
  (port-buffer :pointer)
  (time :unsigned-int)
  (data-size :unsigned-char))


;;; TIME

(defcfun "jack_get_time" jack_time_t)

(defcfun "jack_frames_to_time" jack_time_t
  (client :pointer)
  (frames jack_nframes_t))

(defcfun "jack_time_to_frames" jack_nframes_t
  (client :pointer)
  (time jack_time_t))

(defcfun "jack_last_frame_time" jack_nframes_t
  (client :pointer))

(defcfun "jack_frame_time" jack_nframes_t
  (client :pointer))

;;; JACK/RINGBUFFER.H

(defcstruct jack_ringbuffer_t
  (buf (:pointer :char))
  (write-ptr size_t)
  (read-ptr size_t)
  (size size_t)
  (size-mask size_t)
  (mlocked :int))

(defcstruct jack_ringbuffer_data_t
  (buf (:pointer :float))
  (len size_t))

;; vec[0].buf
(defun rb-data-buf (arr index)		;index is 0 or 1 from jack
  (foreign-slot-value
   (mem-aref arr 'jack_ringbuffer_data_t index)
   '(:struct jack_ringbuffer_data_t)
   'buf))

;;vec[0].len
(defun rb-data-len (arr index)
  (foreign-slot-value
   (mem-aref arr 'jack_ringbuffer_data_t index)
   '(:struct jack_ringbuffer_data_t)
   'len))

;;(rb-data-len vec 0)
(defun rb-data-len-p (arr index)	;len=0 := nothing to get
  (plusp (rb-data-len arr index)))

(defcfun "jack_ringbuffer_create" (:pointer (:struct jack_ringbuffer_t))
  (sz size_t))

(defcfun "jack_ringbuffer_reset" :void
  (rb (:pointer (:struct jack_ringbuffer_t))))

(defcfun "jack_ringbuffer_get_write_vector" :void
  (rb (:pointer (:struct jack_ringbuffer_t)))
  (vec (:pointer (:struct jack_ringbuffer_data_t))))

(defcfun "jack_ringbuffer_free" :void
  (rb (:pointer (:struct jack_ringbuffer_t))))

(defcfun "jack_ringbuffer_write_advance" :void
  (rb (:pointer (:struct jack_ringbuffer_t)))
  (cnt size_t))

(defcfun "jack_ringbuffer_write_space" size_t
  (rb (:pointer (:struct jack_ringbuffer_t))))

(defcfun "jack_ringbuffer_write" size_t
  (rb (:pointer (:struct jack_ringbuffer_t)))
  (src (:pointer :char))
  (cnt size_t))

(defcfun "jack_ringbuffer_get_read_vector" :void
  (rb (:pointer (:struct jack_ringbuffer_t)))
  (vec (:pointer (:struct jack_ringbuffer_data_t))))

(defcfun "jack_ringbuffer_read" size_t
  (rb (:pointer (:struct jack_ringbuffer_t)))
  (dest (:pointer :char))
  (cnt size_t))

(defcfun "jack_ringbuffer_read_space" size_t
  (rb (:pointer (:struct jack_ringbuffer_t))))

;;; end of wrappers for jack/ringbuffer.h


;; provide one default global client-name

(defparameter *CLJackClient* nil)


;;; TIME HANDLING USING JACK SCHEDULER

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
