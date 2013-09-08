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

(load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "OSC")) :name "osc.asd"))
(asdf:operate 'asdf:load-op 'osc)

(compile&load (make-pathname :directory (pathname-directory *load-pathname*) :name "oscoverudp"))

(push :om-osc-api *features*)

(in-package :om-api)

(export '(
         om-send-osc-bundle
         om-send-osc-message
         om-start-osc-server
         om-stop-osc-server
         om-decode-msg-or-bundle
          ) :om-api)
  
(defun om-send-osc-message (port host message)
  (let ((outs (osc::open-osc-out-stream host port)))
    (osc::write-osc-msg message outs)
    (osc::close-osc-stream outs)))

(defun om-send-osc-bundle (port host bundle)
  (let ((outs (osc::open-osc-out-stream host port)))
    (osc::write-osc-bundle bundle outs)
    (osc::close-osc-stream outs)))

(defun om-decode-msg-or-bundle (msg)
  (osc::decode-msg-or-bundle msg))

(defun om-start-osc-server (port host fun)
   (osc::start-osc-server host port fun))

(defun om-stop-osc-server (server)
  (when server
    (osc::stop-osc-server server)))
