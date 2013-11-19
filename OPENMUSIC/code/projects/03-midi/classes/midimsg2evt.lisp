;;===========================================================================
;;; midimsg2evt.lisp
;;;
;;; This program is free software;;;  you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation;;;  either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; See file LICENSE for further informations on licensing terms.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY;;;  without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program;;;  if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; Author: Anders Vinjar

;;===========================================================================
; DocFile
; Class to hold data from lisp-based midi:: messages during I/O
;;===========================================================================

(in-package :oa)

;; a general event class to hold data from midi:: messages for
;; converting to events, modelled after some struct in ms:

(defclass midimsg2evt ()
  ((type :accessor event-type :initarg :type)
   (date :accessor event-date :initarg :event-date)
   (ref :accessor event-ref :initarg :event-ref :initform 0)
   (port :accessor event-port :initarg :event-port)
   (chan :accessor event-chan :initarg :event-chan)
   (fields :accessor event-fields :initarg :event-fields)
   (dur :accessor event-dur :initarg :event-dur)
   (pitch :accessor event-pitch :initarg :event-pitch)
   (velocity :accessor event-velocity :initarg :event-velocity)
   (keypress :accessor event-keypress :initarg :event-keypress)
   (bend :accessor event-bend :initarg :event-bend)
   (tempo :accessor event-tempo :initarg :event-tempo)
   (text :accessor event-text :initarg :event-text)
   (param :accessor event-param :initarg :event-param)
   (pgm :accessor event-pgm :initarg :event-pgm)
   (kpress :accessor event-kpress :initarg :event-kpress)
   (ctrl :accessor event-ctrl :initarg :event-ctrl)
   (val :accessor event-val :initarg :event-val)
   (link :accessor event-link :initarg :event-link :initform nil)))


;;; CONVERSION

;;; metaclasses 

(defmethod update-instance-for-different-class :before ((msg midi::message) (ev midimsg2evt) &key)
  (setf (event-date ev) (midi::message-time msg)))

(defmethod update-instance-for-different-class :before ((msg midi::channel-message) (ev midimsg2evt) &key)
  (setf (event-chan ev) (midi::message-channel msg)))

(defmethod update-instance-for-different-class :before ((msg midi::text-message) (ev midimsg2evt) &key)
  (setf (event-text ev) (slot-value msg 'midi::text)))

;;; classes

(defmethod update-instance-for-different-class :before ((msg midi::tempo-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "Tempo"))
  (setf (event-tempo ev) (midi::message-tempo msg)))

(defmethod update-instance-for-different-class :before ((msg midi::note-on-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "Note"))
  (setf (event-pitch ev) (midi::message-key msg))
  (setf (event-velocity ev) (midi::message-velocity msg)))

(defmethod update-instance-for-different-class :before ((msg midi::note-off-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "keyOff"))
  (setf (event-pitch ev) (midi::message-key msg))
  (setf (event-velocity ev) (midi::message-velocity msg)))

(defmethod update-instance-for-different-class :before ((msg midi::general-text-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "Textual")))

(defmethod update-instance-for-different-class :before ((msg midi::time-signature-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "TimeSign"))
  ;; (setf (event-fields ev)
  ;; 	(let ((nn (midi::message-numerator msg))
  ;; 	      (dd (midi::message-denominator msg))
  ;; 	      (cc (slot-value msg 'midi::cc))
  ;; 	      (bb (slot-value msg 'midi::bb)))
  ;; 	  (list nn dd cc bb)))
  )

(defmethod update-instance-for-different-class :before ((msg midi::sequence/track-name-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "SeqName")))

(defmethod update-instance-for-different-class :before ((msg midi::program-change-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "ProgChange"))
  (setf (event-fields ev) (list (midi::message-program msg))))

(defmethod update-instance-for-different-class :before ((msg midi::instrument-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "InstrName")))

(defmethod update-instance-for-different-class :before ((msg midi::control-change-message) (ev midimsg2evt) &key)
  (setf (event-type ev) (om-midi-get-num-from-type "CtrlChange"))
  (setf (event-fields ev) (list (slot-value msg 'midi::controller)
				(slot-value msg 'midi::value))))
