;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;
;Copyright (C) 2004 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Jean Bresson and Augusto Agon
;;===========================================================================

;;===========================================================================
;loads the OSC API
;;===========================================================================

(in-package :cl-user)

(setf *stdout* #.*standard-output*)

(load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "cl-osc")) :name "osc.asd"))
(asdf:operate 'asdf:load-op 'osc)

(push :osc *features*)

(in-package :om-api)

(export '(
         om-send-osc-bundle
         om-send-osc-message
         om-start-osc-server
         om-stop-osc-server
         om-decode-msg-or-bundle
          ) :om-api)
  

;; the representation of an osc msg is a list of string or fixnum objects
;; a bundle is a list of such. Upon reception, decode-bundle gives a list with a time-tag as its first element
;; does not handle floats and int are limited to 2^31

;; as udpsend and udpreceive tend to replace OpenSoundControl in max, there is a simplewrite-osc-msg
;; in addition to write-osc-bundle

;;; OLD WAY
;(defun write-osc-msg (message osc-stream)
;  (write-sequence (apply #'osc:encode-message message) osc-stream)
;  (force-output osc-stream))
;
;(defun write-osc-bundle (bundle osc-stream &optional (time-tag :now))
;  (write-sequence (osc:encode-bundle bundle time-tag) osc-stream)
;  (force-output osc-stream))
;
;(defun open-osc-out-stream (host port)
;  (comm+:open-udp-stream host port :direction :output :element-type '(unsigned-byte 8)))
;
;(defun close-osc-stream (osc-stream)
;  (close osc-stream))

;;; OSC OVER UDP (COMM+)

(defun write-osc-msg (message datagram)
  (comm+:send-message datagram (apply #'osc:encode-message message)))

(defun write-osc-bundle (bundle datagram &optional (time-tag :now))
  (comm+:send-message datagram (osc:encode-bundle bundle time-tag)))

(defun open-osc-out-stream (host port)
  (comm+:connect-to-udp-server host port))

(defun close-osc-stream (datagram)
  (comm+:close-datagram datagram))

;;;================
;;; OM API
;;;================

;;; SEND

(defun om-send-osc-message (port host message)
  (let ((outs (open-osc-out-stream host port)))
    (write-osc-msg message outs)
    (close-osc-stream outs)))

(defun om-send-osc-bundle (port host bundle)
  (let ((outs (open-osc-out-stream host port)))
    (write-osc-bundle bundle outs)
    (close-osc-stream outs)))

;;; RECEIVE
 
(defun om-decode-msg-or-bundle (msg)
  (osc::decode-message-or-bundle msg))

(defun om-start-osc-server (port host function &optional name)
   (comm+:start-udp-server :address host :service port :function function
                           :process-name (or name (format nil "OSC receiver on ~S ~S" host port))))

(defun om-stop-osc-server (server)
  (when server (comm+:stop-udp-server server :wait t)))



