;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================

(in-package :om)

;;;=====================================
;;; SEND/RECEIVE
;;;=====================================

(defmethod! om-send ((self t) &optional (target :om))
   (let ((boxes (find-receive-boxes target)))
     (mapcar #'(lambda (b)
                 (setf (value b) (list self))
                 (self-notify b nil))
             boxes)))
                 
(defmethod! om-receive (targetname) :initvals '(:om) t)

(defclass OMReceiveBox (OMBoxCall) ())
(defmethod get-boxcallclass-fun ((self (eql 'om-receive))) 'OMReceiveBox)
(defmethod omNG-box-value ((self OMReceiveBox) &optional (numout 0)) 
  (let ((inval (omng-box-value (car (inputs self)))))
    (unless (equal inval (value (car (inputs self))))
      (print (format nil "RECEIVE ID SET TO: ~A" inval))
      (setf (value (car (inputs self))) inval)))
  (if numout (nth numout (value self)) (value self)))
  
;(defmethod omNG-box-value ((self OMReceiveBox) &optional (numout 0))
;  (if numout (nth numout (value self)) (value self)))

(defun find-boxes (type)
  (loop for win in (remove-if-not 
                    #'(lambda (w) (equal 'patcheditor (type-of (editor w))))
                    (om-get-all-windows 'editorwindow)) append
        (loop for b in (boxes (object (editor win))) 
              when (equal type (reference b))
              collect b)))

(defun find-receive-boxes (target)
  (let ((boxes (find-boxes 'om-receive)))
    (remove-if-not #'(lambda (b) (equal (value (nth 0 (inputs b))) target))
                   boxes)
    ))


