(defpackage :cl-jack (:use :common-lisp :cffi))
(in-package :cl-jack)

(define-foreign-library libjack
  (t (:default "/usr/lib/libjack")))

(use-foreign-library libjack) 

(defcfun "jack_get_version_string" :string)
;; (jack-get-version-string)

(defctype size_t :unsigned-int)

(defctype jack_nframes_t :uint32)
(defctype jack_port_t :pointer)
(defctype jack_options_t :pointer)
(defctype jack_time_t :unsigned-long)	;:unsigned-long = :uint64?


(defconstant *jack-default-audio-type* "32 bit float mono audio")
(defconstant *jack-default-midi-type* "8 bit raw midi")

(defcfun "jack_client_name_size" :int)

(defcvar "input_port" jack_port_t)

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

(defcfun "jack_client_open" :pointer
  (name :string)
  (option :int)
  (status :int))

(defparameter JackNullOption (foreign-enum-value 'jackoptions :jacknulloption))

(defcfun "jack_get_sample_rate" :int
  (client :pointer))

(defcfun "jack_port_type_get_buffer_size" size_t
  (client :pointer)
  (port-type :string))

(defcfun "jack_get_buffer_size" jack_nframes_t
  (client :pointer))

(defcfun "jack_port_get_buffer" :pointer
  (port :pointer)
  (frames jack_nframes_t))

(defcfun "jack_get_ports" :pointer
  (client :pointer)
  (port_name_pattern :string)
  (type_name_pattern :string)
  (flags :unsigned-long))

(defcfun "jack_port_register" :pointer
  (client :pointer)
  (port-name :string)
  (port-type :string)
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


;;; time


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

