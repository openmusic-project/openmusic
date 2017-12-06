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
;===========================================================================

;******************************************************************
;-------| CHROMA SYSTEM
;-------| This file is: $LLsys/utils-om.lisp
;-------| Implemented by Marco Stroppa
;-------| Version: 000817, updated 050221
;******************************************************************

; THIS FILE CONTAINS THE DEFINITION OF MIXED FUNCTIONS GENERALLY USEFUL FOR
;    FOR THE CHROMA SYSTEM BUT AVAILABLE TO OM USERS AS WELL.

;;;(in-package cr)
(in-package :om)

;------------------------------------------------------------------
; AVAILABLE FUNCTIONS :
;       clip
;	beat->secs
;	interval
;	dB->lin / lin->dB
;	load-files
;	nextl
;	printdate / stringdate
;	sign /invert-sign
;------------------------------------------------------------------


;------------------------------------------------------------------
; clip

(defun clip (val &optional (min 0.0) (max 1.0))
" If val is below min, return min,
  if val is above max, return max,
  otherwise return val.
" 
  (let ((from min) (to max))
    (when (> min max) (setf from max) (setf to min))
    (cond
     ((> val to) to)
     ((< val from) from)
     (t val))))


;------------------------------------------------------------------
; interval

(defun interval (val)
"Return the interval of val [cents] as a scaler"
  (nroot 12 (expt 2.0 (/ val 100.0))))

(defun nroot (root base)
"Compute the nth root of base"
  (expt base (/ 1.0 root)))



;------------------------------------------------------------------
; beat->secs

(defun beat->secs (list MM)
" List: one or a list of times (markers); MM: metronome.
  Return the same list converted into absolute seconds.
"  
  (if (listp list)
      (mapcar #'(lambda (x) (* x (/ 60.0 MM))) list)
    (* list (/ 60.0 MM))))

;------------------------------------------------------------------
; dB->lin / lin->dB


;;; fromerly lintodb ;;;
(defun lin-to-dB (x)
  (let ((y (if (= 0.0 x) 0.00000001 x)))
    (* (log y 10) 20)))

(defun db-to-lin (x)
  (expt 10.0 (/ x 20.0)))


;;;======= FROM OM2CSOUND ==========

#|
(defun lin->db1 (amp) 
  (if (zerop amp) -3.63224978306E9
      (lldecimals (* 20.0 (log amp 10)) 2)))

(om::defmethod! lin->db ((amps t))
  :icon 130
  :indoc '("linear")
  :initvals (list 10000)
  :doc "<lin->db> takes a  number <amps> and returns the corresponding value 
expressed in decibels. The input can be a list of numbers. In this case a list of 
db values is returned."
  (deep-mapcar/1 'lin->db1 amps))


(om::defmethod! db->lin ((amps t))
  :icon 130
  :indoc '("db")
  :initvals (list 80) 
  :doc "<dB->lin> takes a  number <amps> in decibels and converts it
to linear. The input can be a list of numbers. In this case a list of 
linear values is returned."
  (deep-mapcar/1  'dblin amps))

|#

;;;====================================

;------------------------------------------------------------------
; printdate / stringdate
;	nicely print the current date (calling the lisp function date)

(defun printdate (&optional (outchan t))
  "Nicely print the current date (calling the lisp function get-decoded-time).
It can send its output to another output channel if an argument is present."
  (multiple-value-bind
    (second minute hour date month year)
    (get-decoded-time)
    (format outchan "~a ~a, ~a - AT ~a:~a (~a sec)" month date year hour minute second)))

(defun stringdate ()
  "Nicely return a stream with the current date (calling the lisp function get-decoded-time)"
  (multiple-value-bind
    (second minute hour date month year)
    (get-decoded-time)
    (format () "~a ~a, ~a - AT ~a:~a (~a sec)~%"month date year hour minute second  )))

;------------------------------------------------------------------
; (sign a [b]) / (invert-sign a)
;	return the sign of a (+1.0 or -1.0)
;	if b is present, return the sign of b-a

(defun sign (a &optional (b ()))
  (if b (signum (- b a))
      (signum a)))

(defun invert-sign (num)
  (* -1.0 (sign num) (abs num)))

;------------------------------------------------------------------

(defun closest-pwr2 (val)
  "Return the closest larger power of two) of val, ex. 3.4 --> 4. Useful for csound audio tables."
  (let ((size 2))
    (loop while (> val size) do
        (setf size (* size 2)))
    size))




