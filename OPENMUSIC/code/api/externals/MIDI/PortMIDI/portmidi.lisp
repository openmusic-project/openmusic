;;; **********************************************************************
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; **********************************************************************
;;; Merging Rick Taube's CFFI-PORTMIDI and PortMedi's CL-PortMIDI code
;;; Copyright (c) 2014 Jean Bresson
;;; **********************************************************************
;;; CL-PortMIDI from PortMedia
;;; Copyright (C) 2012 Christoph Finkensiep
;;; **********************************************************************
;;; CFFI-PortMidi binding from PortMIDI distribution
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;; **********************************************************************

(in-package :cl-user)

(defpackage :portmidi
  (:use :common-lisp) 
  (:nicknames :pm))


(in-package :pm)


(defvar *libportmidi-pathname* 
  #+win32
  "/WINDOWS/system32/libportmidi.dll"
  #+macosx
  "/usr/lin/libportmidi.dylib"
  #+linux
  "/usr/lib/libportmidi.so")

(defvar *libportmidi* nil)

(defun load-portmidi-lib ()
  (print (concatenate 'string "Loading PortMIDI library: " (namestring *libportmidi-pathname*)))
  (if (probe-file *libportmidi-pathname*)
      (setf *libportmidi* (fli:register-module "PortMidi" 
                                               :real-name (namestring *libportmidi-pathname*)
                                               :connection-style :immediate)
            )
    (print (format nil "Library PortMIDI not found!! [~A]" (namestring *libportmidi-pathname*)))
    ))

; (load-portmidi-lib)

;;;================
;;; TYPES
;;;================

(cffi:defctype pm-message :long)
(cffi:defctype pm-timestamp :long)
(cffi:defctype pm-device-id :int)
(cffi:defctype pm-error :int)
(cffi:defctype pm-stream :void)
(cffi:defctype pm-time-proc-ptr :pointer)

(cffi:defcstruct pm-device-info 
		 (struct-version :int) 
		 (interf :pointer) 
		 (name :pointer) 
		 (input :int) 
		 (output :int) 
		 (opened :int))

;;; accessors
(defun pm-device-info-interf (ptr)
  (cffi:foreign-string-to-lisp 
   (cffi:foreign-slot-value ptr 'pm-device-info 'interf)))

(defun pm-device-info-name (ptr)
  (cffi:foreign-string-to-lisp
   (cffi:foreign-slot-value ptr 'pm-device-info 'name)))

(defun pm-device-info-input (ptr)
  (not (= (cffi:foreign-slot-value ptr 'pm-device-info 'input) 0)))

(defun pm-device-info-output (ptr)
  (not (= (cffi:foreign-slot-value ptr 'pm-device-info 'output) 0)))

(defun pm-device-info-opened (ptr)
  (not (= (cffi:foreign-slot-value ptr 'pm-device-info 'opened) 0)))



(cffi:defcstruct pm-event 
		 (message pm-message) 
		 (timestamp pm-timestamp))

;;;================
;;; CONSTANTS
;;;================

(defconstant true 1) 
(defconstant false 0)
(defconstant pmNoError 0)
(defconstant pmHostError -10000)
(defconstant pm-no-device -1)

(defvar *host-error-text* (make-string 256 :initial-element #\*))


;;;==========================
;;; PORTTIME
;;;==========================

;;; porttime.h
(cffi:defctype pt-error :int)
(cffi:defctype pt-timestamp :long)
(cffi:defcfun ("Pt_Start" pt-start) pt-error (a :int) (b :pointer) (c :pointer))
(cffi:defcfun ("Pt_Stop" pt-stop) pt-error)
(cffi:defcfun ("Pt_Started" pt-started) :int)
(cffi:defcfun ("Pt_Time" pt-time) pt-timestamp)


;;;================
;;; FUNCTIONS
;;;================

;;; INIT / CLOSE

(cffi:defcfun ("Pm_Initialize" pm-initialize) pm-error)
(cffi:defcfun ("Pm_Terminate" pm-terminate) pm-error)


;;; ERRORS

(cffi:defcfun ("Pm_HasHostError" pm-has-host-error) :int (stream :pointer))

(cffi:defcfun ("Pm_GetErrorText" pm-get-error-text-STR) :pointer (errnum pm-error)) 
(defun pm-get-error-text (errnum)
  (cffi:foreign-string-to-lisp (pm-get-error-text-STR errnum)))

(cffi:defcfun ("Pm_GetHostErrorText" pm-get-host-error-text-STR) :void (msg :pointer) (len :unsigned-int)) 
(defun pm-get-host-error-text ()
  (cffi:with-foreign-string (host-error *host-error-text*)
    (pm-get-host-error-text-STR host-error (length *host-error-text*))
    (cffi:foreign-string-to-lisp host-error)))


;;; DEVICES

(cffi:defcfun ("Pm_CountDevices" pm-count-devices) :int) 

(cffi:defcfun ("Pm_GetDeviceInfo" pm-get-device-info-PTR) :pointer (id pm-device-id)) 

; (cffi:foreign-slot-value ptr 'pm-device-info 'name)
(defun pm-get-device-info (device-id)
  (let* ((ptr (pm-get-device-info-PTR device-id))
         (interf (pm-device-info-interf ptr))
         (name (pm-device-info-name ptr))
         (in (pm-device-info-input ptr))
         (out (pm-device-info-output ptr))
         (open (pm-device-info-opened ptr)))
    (values interf name in out open))
  )

;;; this has to do with a preference stored in 
;;; /Users/$NAME/Library/Preferences/com.apple.java.util.prefs.plist
;;; -1 = no device
(cffi:defcfun ("Pm_GetDefaultOutputDeviceID" pm-get-default-output-device-id) pm-device-id) 
;;; -1 = no device
(cffi:defcfun ("Pm_GetDefaultInputDeviceID" pm-get-default-input-device-id) pm-device-id) 


;;; OPEN/CLOSE PORTS
;;; We could use our lisp timer instead of internal PortTime process

(cffi:defcfun ("Pm_OpenOutput" pm-open-output-PTR) pm-error 
  (stream :pointer) 
  (output-device pm-device-id) 
  (output-driver-info :pointer) 
  (buffer-size :long) 
  (time-proc pm-time-proc-ptr) 
  (time-info :pointer) 
  (latency :long))

(cffi:defcfun ("Pm_OpenInput" pm-open-input-PTR) pm-error 
  (stream :pointer) 
  (input-device pm-device-id) 
  (input-driver-info :pointer) 
  (buffer-size :long) 
  (time-proc pm-time-proc-ptr) 
  (time-info :pointer))

(defun pm-open-output (device buffer-size latency)
  ;;; (unless (Started) (Start))
  (cffi:with-foreign-object (p1 :pointer)
    (let ((err (pm-open-output-PTR p1 device (cffi:null-pointer)
                               buffer-size 
                               (cffi:null-pointer) (cffi:null-pointer)
                               latency)))
      (if (= err pmNoError)
          (cffi:mem-ref p1 :pointer)
          (error (pm-get-error-text err))))))


(defun pm-open-input (device buffer-size)
  ;; portmidi: timer must be running before opening
  ;(unless (Started) (Start))
  (cffi:with-foreign-object (p1 :pointer)
    (let ((err (pm-open-input p1 device (cffi:null-pointer)
                              buffer-size 
                              (cffi:null-pointer) (cffi:null-pointer)
                              )))
        (if (= err pmNoError)
            (cffi:mem-ref p1 :pointer)
            (error (pm-get-error-text err))))))


(cffi:defcfun ("Pm_Close" pm-close) pm-error (stream :pointer)) 


;;; SEND

(cffi:defcfun ("Pm_Write" pm-write) pm-error 
  (stream :pointer) 
  (buffer :pointer) 
  (length :long)) 

(cffi:defcfun ("Pm_WriteShort" pm-write-short) pm-error 
  (stream :pointer) 
  (timetag pm-timestamp) 
  (msg :long))

(cffi:defcfun ("Pm_WriteSysEx" pm-write-sys-ex-PTR) pm-error 
  (stream :pointer) 
  (timetag pm-timestamp) 
  (msg :pointer))

(defun pm-write-sys-ex (stream timetag string)
  (cffi:with-foreign-string (ptr string)
    (pm-write-sys-ex-PTR stream timetag ptr)))


#|


;;;;=====================================================================================
;;;; FROM Taube's CFFI bindings

;;; accessors 
(defun Event.message (e &optional (v nil vp))
  (if vp
      (progn 
	(setf (cffi:foreign-slot-value e 'pm-event 'message) v)
	v)
    (cffi:foreign-slot-value e 'pm-event 'message)))
    
(defun Event.timestamp (e &optional (v nil vp))
  (if vp
      (progn 
	(setf (cffi:foreign-slot-value e 'pm-event 'timestamp) v)
	v)
    (cffi:foreign-slot-value e 'pm-event 'timestamp)))



(cffi:defcfun ("Pm_Poll" pm-poll) pm-error (stream :pointer)) 
(cffi:defcfun ("Pm_Read" pm-read) pm-error (stream :pointer) (buffer :pointer) (length :long)) 
(cffi:defcfun ("Pm_Abort" pm-abort) pm-error (stream :pointer)) 
;(cffi:defcfun ("Pm_SetChannelMask" pm-set-channel-mask) pm-error (stream :pointer) (mask :int)) 
(cffi:defcfun ("Pm_SetFilter" pm-set-filter) pm-error (stream :pointer) (filters :long)) 



(defconstant pm-default-sysex-buffer-size 1024)
(defconstant filt-active 1) 
(defconstant filt-sysex 2) 
(defconstant filt-clock 4) 
(defconstant filt-play 8) 
(defconstant filt-f9 16) 
(defconstant filt-fd 32) 
(defconstant filt-reset 64) 
(defconstant filt-note 128) 
(defconstant filt-channel-aftertouch 256) 
(defconstant filt-poly-aftertouch 512) 
(defconstant filt-program 1024) 
(defconstant filt-control 2048) 
(defconstant filt-pitchbend 4096) 
(defconstant filt-mtc 8192) 
(defconstant filt-song-position 16384) 
(defconstant filt-song-select 32768) 
(defconstant filt-tune 65536) 
(defconstant filt-tick filt-f9)
(defconstant filt-undefined (logior filt-f9 filt-fd))
(defconstant filt-realtime (logior filt-active filt-sysex
                                      filt-clock filt-play
                                      filt-undefined filt-reset))
(defconstant filt-aftertouch (logior filt-channel-aftertouch
                                        filt-poly-aftertouch ))
(defconstant filt-systemcommon (logior filt-mtc filt-song-position
                                          filt-song-select filt-tune))



(defun SetFilter (a filts) 
  (with-pm-error
    (pm-set-filter a filts)))

;(defun SetChannelMask (pms mask)
;  (with-pm-error (pm-set-channel-mask pms mask)))

(defun Abort (pms)
  (with-pm-error (pm-abort pms)))

(defun Close (pms)
  (with-pm-error (pm-close pms)))

(defun EventBufferFree (buf)
  (cffi:foreign-free buf))

(defun EventBufferNew (len)
  (cffi:foreign-alloc 'pm-event :count len))

(defun EventBufferElt (buf i)
  ;; buf is POINTER to buf
  (cffi:mem-aref buf 'pm-event i))

(defun EventBufferSet (buffer index timestamp message)
  (setf (cffi:foreign-slot-value
         (cffi:mem-aref buffer 'pm-event index) 'pm-event 'timestamp)
        timestamp)
  (setf (cffi:foreign-slot-value
         (cffi:mem-aref buffer 'pm-event index) 'pm-event 'message)
        message)
  (values))

(defun EventBufferMap (fn buf end)
  (loop for i below end
     for e = (EventBufferElt buf i)
     do (funcall fn (Event.message e) (Event.timestamp e)))
 (values))

(defun Read (pms *evbuf len) 
  (let ((res (pm-read pms *evbuf len)))
    (if (< res 0)
        (error (pm-get-error-text res))
        res)))

(defun Poll (pms)
  (let ((res (pm-poll pms)))
    (cond ((= res 0) nil)
          ((= res 1) t)
          (t (error (pm-get-error-text res))))))

(defun Write (pms *evbuf len)
  (with-pm-error (pm-write pms *evbuf len)))

(defun WriteShort (pms when msg)
  (with-pm-error (pm-write-short pms when msg)))






|#
