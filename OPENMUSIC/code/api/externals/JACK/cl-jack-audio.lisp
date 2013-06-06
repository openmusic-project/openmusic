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

(unless *OMJackClient*
  (setq *OMJackClient* (jack-client-open "OpenMusic" JackNullOption 0)))

;; (jack-client-close *OMJackClient*)
;; (jack-get-client-name *OMJackClient*)

(defparameter *OM-jack-audio-input-channels* 2)
(defparameter *OM-jack-audio-output-channels* 2)

(defparameter *OM-jack-audio-input-ports* nil)
(defparameter *OM-jack-audio-output-ports* nil)

(setf *OM-jack-audio-input-ports*
      (loop for chan from 0 below *OM-jack-audio-output-channels*
	 collect
	   (let ((port (jack-port-register
			*OMJackClient*
			(format nil "in_~A" chan)
			*jack-default-audio-type*
			(foreign-enum-value 'jackportflags :jackportisinput)
			0)))
	     (if (zerop (pointer-address port)) ;0 if not allocated
		 (error "*OM-jack-audio-input-ports* not allocated")
		 port))))

(setf *OM-jack-audio-output-ports*
      (loop for chan from 0 below *OM-jack-audio-output-channels*
	   collect
	   (let ((port (jack-port-register
			*OMJackClient*
			(format nil "out_~A" chan)
			*jack-default-audio-type*
			(foreign-enum-value 'jackportflags :jackportisoutput)
			0)))
	     (if (zerop (pointer-address port)) ;0 if not allocated
		 (error "*OM-jack-audio-output-ports* not allocated")
		 port))))

;; provide default-callback which just copies in to out:


(defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg :pointer))
  (declare (ignore arg))
  (cl-jack-handle-midi-seq nframes)
  (loop for inport in *OM-jack-audio-input-ports*
     for outport in *OM-jack-audio-output-ports*
     do
       (let ((in (jack-port-get-buffer inport nframes))
	     (out (jack-port-get-buffer outport nframes)))
	 (foreign-funcall "memcpy" :pointer out :pointer in
			  size_t (* nframes (foreign-type-size 'size_t)))))
  
  0)

(jack-set-process-callback *OMJackClient* (callback cl-jack-process-callback) 0)
(jack-activate *OMJackClient*)
;;(jack-deactivate *OMJackClient*)
