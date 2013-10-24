;;; ===========================================================================
;;; JACK Player for Common Lisp/CFFI

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 2.1 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.

;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Author: Anders Vinjar

(in-package :om)

;(require :cl-jack (make-pathname :directory (append om-api::*externals-directory* '("JACK")) :name "cl-jack-load"))

;==================
; PLAYER
;==================

(defmethod player-name ((engine (eql :jackaudio))) "jackplayer")
(defmethod player-type ((engine (eql :jackaudio))) :JACK-client)
(defmethod player-desc ((engine (eql :jackaudio))) "Audio player for JACK")

(defun init-jackplayer ()
  ;; enable jackplayer for sound class:
  (pushnew :jackaudio *enabled-players*)
  (add-player-for-object sound :jackaudio))

;;(init-jackplayer)
(om-add-init-func 'init-jackplayer)


;; using 'sndlasptr-slot from 'om-sound class to hold jack-sf struct:
(defmethod jack-sf-struct ((object sound))
  (oa::sndlasptr object))

(defmethod (setf jack-sf-struct) (jack-sf (object sound))
  (setf (oa::sndlasptr object) jack-sf))

(defmethod prepare-to-play ((engine (eql :jackaudio)) (player omplayer) object at interval)
  (let ((thissound (cl-jack::cl-jack-play-sound (om-sound-file-name object) (car interval))))
    (setf (jack-sf-struct object) thissound)))  

(defmethod player-start ((engine (eql :jackaudio)) &optional play-list)
  t)

;;; PAUSE
(defmethod player-pause ((engine (eql :jackaudio)) &optional play-list)
  (if play-list
      (mapc #'(lambda (item)
		(player-pause-object engine (if (listp item) (first item) item)))
	    play-list)
      (cl-jack:jackplay-toggle-read nil nil)))

;;; CONTINUE
(defmethod player-continue ((engine (eql :jackaudio)) &optional play-list)
  (if play-list
      (mapc #'(lambda (item)
		(player-continue-object engine (if (listp item) (first item) item)))
	    play-list)
      (cl-jack:jackplay-toggle-read nil t)))

;;; STOP
(defun jack-stop-all-players ()
  (mapc #'cl-jack:cl-jack-close-sound cl-jack::*jack-sounds*))

(defmethod player-stop ((engine (eql :jackaudio)) &optional play-list)
  (if play-list
      (mapc #'(lambda (item)
		(when item
		  (player-stop-object engine (if (listp item) (first item) item))))
	    play-list)
      (jack-stop-all-players)))


;;; PLAY (NOW)
;; (defmethod player-play-object ((engine (eql :jackaudio)) (object sound) &key interval)
;;   ;;(jack-play object (car interval) (cadr interval) (tracknum object))
;;   (call-next-method))

;;; NOT IN OM PLAYER API

;;; PAUSE ONLY ONE OBJECT
(defmethod player-pause-object ((engine (eql :jackaudio)) (object sound) &key interval)
  (declare (ignore interval))
  (cl-jack:jackplay-toggle-read (jack-sf-struct object) nil))

;;; RESTART ONLY ONE OBJECT
(defmethod player-continue-object ((engine (eql :jackaudio)) (object sound) &key interval)
  (declare (ignore interval))
  (cl-jack:jackplay-toggle-read (jack-sf-struct object) t))


;;; STOP ONLY ONE OBJECT
(defmethod player-stop-object ((engine (eql :jackaudio)) (object sound) &key interval)
  (declare (ignore interval))
  (cl-jack:cl-jack-close-sound (jack-sf-struct object)))

;;; called when a box or editor attached to player is removed/closed
(defmethod player-cleanup ((player (eql :jackaudio)) snd)
  (player-stop-object player snd))

;;; SET LOOP (called before play)
(defmethod player-set-loop ((engine (eql :jackaudio)) &optional start end)
  ;;(print (format nil "~A : set loop start: ~A end: ~A" engine start end))
  t)

(defun jack-goto-start (jack-sf)
  (cl-jack::cl-jack-seek
   (cl-jack::jack-sf-sound-file-handle jack-sf)
   (cl-jack::jack-sf-start jack-sf)))

(defmethod player-loop ((engine (eql :jackaudio)) player &optional play-list)
  (if play-list
      (mapc #'(lambda (item)
		(jack-goto-start (jack-sf-struct (if (listp item) (first item) item))))
	    play-list)))



