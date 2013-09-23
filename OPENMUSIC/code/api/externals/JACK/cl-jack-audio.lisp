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

;; (jack-client-close *CLJackClient*)
;; (jack-get-client-name *CLJackClient*)

(defparameter *CL-jack-audio-input-channels* 2)
(defparameter *CL-jack-audio-output-channels* 2)

(defparameter *CL-jack-audio-input-ports* nil)
(defparameter *CL-jack-audio-output-ports* nil)

(defun cl-jack-init-audio ()
  (unless *CLJackClient*
    (setf *CLJackClient* (jack-client-open "CLJack" JackNullOption 0)))
  (setf *CL-jack-audio-input-ports*
	(loop for chan from 0 below *CL-jack-audio-output-channels*
	   collect
	     (let ((port (jack-port-register
			  *CLJackClient*
			  (format nil "in_~A" chan)
			  *jack-default-audio-type*
			  (foreign-enum-value 'jackportflags :jackportisinput)
			  0)))
	       (if (zerop (pointer-address port)) ;0 if not allocated
		   (error "*CL-jack-audio-input-ports* not allocated")
		   port))))

  (setf *CL-jack-audio-output-ports*
	(loop for chan from 0 below *CL-jack-audio-output-channels*
	   collect
	     (let ((port (jack-port-register
			  *CLJackClient*
			  (format nil "out_~A" chan)
			  *jack-default-audio-type*
			  (foreign-enum-value 'jackportflags :jackportisoutput)
			  0)))
	       (if (zerop (pointer-address port)) ;0 if not allocated
		   (error "*CL-jack-audio-output-ports* not allocated")
		   port))))

  ;; provide default-callback which just copies in to out:


  (defcallback cl-jack-process-callback :int ((nframes jack_nframes_t) (arg :pointer))
    (declare (ignore arg))
    (when (fboundp 'cl-jack-handle-event-seqs) (cl-jack-handle-event-seqs nframes))
    (loop for inport in *CL-jack-audio-input-ports*
       for outport in *CL-jack-audio-output-ports*
       do
	 (let ((in (jack-port-get-buffer inport nframes))
	       (out (jack-port-get-buffer outport nframes)))
	   (foreign-funcall "memcpy" :pointer out :pointer in
			    size_t (* nframes (foreign-type-size 'size_t)))))
  
    0)

  (jack-set-process-callback *CLJackClient* (callback cl-jack-process-callback) 0)
  (jack-activate *CLJackClient*)
  ;;(jack-deactivate *CLJackClient*)
  )
