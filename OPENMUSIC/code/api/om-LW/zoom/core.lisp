;=========================================================================
; OM API
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;Author: Paulo Henrique Raposo

(in-package :oa)

;;; ---------- Range ----------

(defparameter +om-zoom-min+ 0.5)
(defparameter +om-zoom-max+ 4.0)
(defparameter *om-zoom-default* 1.0)

;;; ---------- Input sensitivity ----------

(defvar *om-zoom-gesture-sensitivity* 1.0)

;;; ---------- Runtime flags ----------

(defvar *om-zoom-in-progress-p* nil
  "Bound T during om-zoom-update to suppress intermediate redraws.")

(defvar *om-zoom-drag-visual-pos* nil
  "When bound, pins CAPI to the dragged pixel and skips logical-to-visual recompute.")

(defvar *om-zoom-internal-resize* nil
  "When T, suppresses user-resize bookkeeping during a programmatic zoom relayout.")

(defvar *om-zoom-restoring-p* nil
  "Bound T while applying a persisted zoom factor; suppresses live re-persistence loops.")

(defvar *om-zoom-unscale-mouse-pos-p* nil
  "When bound T, om-event-position returns LOGICAL coords instead of VISUAL.")

(defvar *om-zoom-mini-helper-scale* nil
  "Bound to current effective zoom around miniview helper draw calls
so view-less draw routines pick up the pen/mark scale.")

(defvar *om-zoom-diag-events-p* nil)

(defvar *om-zoom-diag-apply-p* nil)

;;; ---------- Logging ----------

(defun om-zoom-log-error (where c)
  "Log condition C to *om-stream* tagged with WHERE; never swallows."
  (let ((s (or om-lisp:*om-stream* *standard-output*)))
    (format s "~&[om-zoom] ERROR in ~A: ~A~%   condition-type=~S~%"
            where (princ-to-string c) (type-of c))
    (finish-output s)))

(defun om-zoom-diag-log (fmt &rest args)
  "Diagnostic print gated on *om-zoom-diag-events-p*."
  (when *om-zoom-diag-events-p*
    (let ((s (or om-lisp:*om-stream* *standard-output*)))
      (apply #'format s
             (concatenate 'string "~&[om-zoom-diag] " fmt "~%")
             args)
      (finish-output s))))

;;; ---------- Exports ----------

(export '(+om-zoom-min+
          +om-zoom-max+
          *om-zoom-default*
          *om-zoom-gesture-sensitivity*
          *om-zoom-in-progress-p*
          *om-zoom-unscale-mouse-pos-p*
          *om-zoom-diag-events-p*
          *om-zoom-diag-apply-p*
          om-zoom-log-error
          om-zoom-diag-log)
        :oa)
