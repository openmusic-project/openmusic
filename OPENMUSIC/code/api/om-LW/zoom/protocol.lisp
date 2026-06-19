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

;;; ---------- CAPI wrappers (LW decoupling) ----------

(defgeneric om-pane-property (pane key))

(defmethod om-pane-property ((pane capi:simple-pane) key)
  (capi:capi-object-property pane key))

(defgeneric (setf om-pane-property) (value pane key))

(defmethod (setf om-pane-property) (value (pane capi:simple-pane) key)
  (setf (capi:capi-object-property pane key) value))

(defgeneric om-pane-visible-size (pane))

(defmethod om-pane-visible-size ((pane capi:simple-pane))
  (capi:simple-pane-visible-size pane))

(defgeneric om-redraw-pinboard-object (obj))

(defmethod om-redraw-pinboard-object ((obj capi:pinboard-object))
  #+win32 (capi:redraw-pinboard-object obj))

(defgeneric om-redisplay-element (elem))

(defmethod om-redisplay-element ((elem capi:simple-pane))
  #+linux (capi:redisplay-element elem))

(defparameter om-text-input-tab-complete 'capi:text-input-pane-complete-text)

;;; ---------- Zoom storage on pane (capi-object-property) ----------

(defgeneric om-zoom-of (pane)
  (:documentation "Current zoom factor of PANE; defaults to *om-zoom-default*."))

(defmethod om-zoom-of ((pane t))
  (or (capi:capi-object-property pane :om-zoom-factor) *om-zoom-default*))

(defgeneric (setf om-zoom-of) (value pane))

(defmethod (setf om-zoom-of) (value (pane t))
  (setf (capi:capi-object-property pane :om-zoom-factor) value))

(defun om-zoom-transformed-p (pane)
  "T iff PANE has a non-identity zoom transform."
  (/= (om-zoom-of pane) 1.0))

;;; ---------- Opt-in predicate (NIL-by-default) ----------

(defgeneric om-zoom-applies-p (pane)
  (:documentation "T iff zoom gestures apply to PANE; opt-outs in projects/zoom/opt-outs.lisp."))

(defmethod om-zoom-applies-p ((pane t))           nil)
(defmethod om-zoom-applies-p ((pane null))        nil)
(defmethod om-zoom-applies-p ((pane om-scroller)) t)

;;; ---------- Effective zoom (ancestor walk) ----------

(defvar *make-frame-zoom-context* nil
  "Bound during frame construction to propagate the destination panel zoom
into make-frame-from-callobj specializations.")

(defun om-zoom-find-ancestor-zoom (frame)
  "Zoom factor of FRAME's nearest opt-in scroller ancestor, or session default."
  (loop for f = frame then (om-view-container f)
        while f
        when (om-zoom-applies-p f) return (om-zoom-of f)
        finally (return *om-zoom-default*)))

(defun om-zoom-effective (self)
  "Zoom factor that applies to SELF, walking the container chain."
  (cond
   ((om-zoom-applies-p self) (om-zoom-of self))
   (t (let ((pane (om-view-container self)))
        (cond
         ((null pane)
          (if (numberp *make-frame-zoom-context*)
              *make-frame-zoom-context*
              *om-zoom-default*))
         ((om-zoom-applies-p pane) (om-zoom-of pane))
         (t (om-zoom-find-ancestor-zoom pane)))))))

;;; ---------- Frame protocol (specializers live in projects/zoom/) ----------

(defgeneric om-zoom-relayout-frame (frame)
  (:documentation "Re-apply layout to FRAME after its pane's zoom changed."))

(defmethod om-zoom-relayout-frame ((frame t)) nil)

(defgeneric om-zoom-redraw-connections (frame)
  (:documentation "Refresh FRAME's connection points after a zoom relayout."))

(defmethod om-zoom-redraw-connections ((frame t)) nil)

(defgeneric om-zoom-sync-display (pane factor)
  (:documentation "Refresh PANE's zoom-display chrome (bar/numbox) after FACTOR changed."))

(defmethod om-zoom-sync-display ((pane t) factor)
  (declare (ignore factor))
  nil)

(defgeneric om-zoom-touch-update (pane scale anchor-x anchor-y)
  (:documentation "Apply a multiplicative SCALE pinch gesture to PANE's zoom."))

(defmethod om-zoom-touch-update ((pane t) scale anchor-x anchor-y)
  (declare (ignore scale anchor-x anchor-y))
  nil)

(defgeneric om-zoom-resolve-touch-target (pane x y)
  (:documentation
   "Resolve a (:touch :zoom) event delivered to PANE at viewport-local (X, Y)
    into the scroller where the zoom should apply, with anchor translated.
    Returns (values target-pane vx vy) or NIL when PANE has no zoom-capable target."))

(defmethod om-zoom-resolve-touch-target ((pane t) x y)
  (declare (ignore x y))
  nil)

;;; ---------- Exports ----------

(export '(om-pane-property
          om-pane-visible-size
          om-redraw-pinboard-object
          om-redisplay-element
          om-text-input-tab-complete
          om-zoom-of
          om-zoom-transformed-p
          om-zoom-applies-p
          om-zoom-find-ancestor-zoom
          om-zoom-effective
          *make-frame-zoom-context*
          om-zoom-relayout-frame
          om-zoom-redraw-connections
          om-zoom-sync-display
          om-zoom-touch-update
          om-zoom-resolve-touch-target
          om-zoom-logical-pos
          om-zoom-logical-size
          om-zoom-logical-font)
        :oa)
