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

;;; ---------- Touch anchor (platform-specific) ----------

#+win32
(defun om-zoom-touch-anchor (pane x y)
  "Pinch anchor on Win32; viewport-local (X, Y) is returned unchanged."
  (declare (ignore pane))
  (values x y))

#-win32
(defun om-zoom-touch-anchor (pane x y)
  "Pinch anchor on non-Win32; shifts (X, Y) by current scroll position
so the anchor stays at the pixel under the user's fingers."
  (let ((pos (om-scroll-position pane)))
    (values (- x (om-point-h pos))
            (- y (om-point-v pos)))))

;;; ---------- Deferred bridge into kernel scroll commands ----------

(defun om-zoom-scroll-pane (pane direction)
  "Forward DIRECTION (keyword) to the kernel SCROLL-PANE handler;
no-op until :om package is built and exports SCROLL-PANE."
  (let* ((pkg (find-package :om))
         (sym (and pkg (find-symbol "SCROLL-PANE" pkg))))
    (when (and sym (fboundp sym))
      (funcall sym pane direction))))

;;; ---------- Shift + wheel = horizontal scroll (Win32) ----------

#+win32
(defvar *om-shift-wheel-hscroll-step* 50
  "Pixels per Shift+MouseWheel notch redirected into horizontal scroll on Win32.")

(defun om-shift-wheel-hscroll-handler (pane x y angle)
  "Shift+Wheel handler; converts ANGLE to a horizontal scroll on Win32,
and to a left/right keyboard scroll on other platforms."
  (declare (ignore x y))
  (when (typep pane 'om-scroller)
    #+win32
    (let* ((step  (or *om-shift-wheel-hscroll-step* 50))
           (pos   (om-scroll-position pane))
           (hpos  (om-point-h pos))
           (vpos  (om-point-v pos))
           (delta (if (plusp angle) step (- step)))
           (new-x (max 0 (+ hpos delta))))
      (om-move-scroll-position pane (om-make-point new-x vpos))
      (om-set-h-scroll-position pane new-x)
      (om-invalidate-view pane t))
    #-win32
    (cond ((plusp  angle) (om-zoom-scroll-pane pane :om-key-right))
          ((minusp angle) (om-zoom-scroll-pane pane :om-key-left)))))

;;; ---------- Touch swipe / Ctrl+arrow ----------

(defun om-zoom-touch-swipe-handler (pane x y direction)
  "Two-finger swipe handler; dispatches to the kernel scroll handler."
  (declare (ignore x y))
  (when (typep pane 'om-scroller)
    (case direction
      (:left  (om-zoom-scroll-pane pane :om-key-left))
      (:right (om-zoom-scroll-pane pane :om-key-right))
      (:up    (om-zoom-scroll-pane pane :om-key-up))
      (:down  (om-zoom-scroll-pane pane :om-key-down)))))

(defun om-zoom-ctrl-arrow-handler (pane x y gspec)
  "Ctrl+Arrow handler; reads gesture data and dispatches scroll."
  (declare (ignore x y))
  (when (typep pane 'om-scroller)
    (let ((data (sys:gesture-spec-data gspec)))
      (case data
        (:left  (om-zoom-scroll-pane pane :om-key-left))
        (:right (om-zoom-scroll-pane pane :om-key-right))
        (:up    (om-zoom-scroll-pane pane :om-key-up))
        (:down  (om-zoom-scroll-pane pane :om-key-down))))))

;;; ---------- Pinch-to-zoom entry point ----------

(defgeneric om-zoom-touch-applies-p (pane)
  (:documentation "T iff PANE accepts pinch dispatch even when om-zoom-applies-p is NIL.
Defaults to om-zoom-applies-p; specialize for panes (e.g. scorePanel) that use
om-zoom-touch-update for non-canvas zoom (music-font scaling) while staying opt-out
of canvas zoom."))

(defmethod om-zoom-touch-applies-p ((pane t))
  (om-zoom-applies-p pane))

(defun om-zoom-touch-handler (pane x y scale)
  "Pinch handler; computes effective scale, resolves target scroller,
and calls om-zoom-touch-update on it."
  (let* ((sensitivity (or *om-zoom-gesture-sensitivity* 1.0))
         (eff-scale   (+ 1.0 (* (- scale 1.0) sensitivity))))
    (multiple-value-bind (scroller sx sy)
        (om-zoom-resolve-touch-target pane x y)
      (when (and scroller (om-zoom-touch-applies-p scroller))
        (multiple-value-bind (vx vy) (om-zoom-touch-anchor scroller sx sy)
          (om-zoom-touch-update scroller eff-scale vx vy))))))

;;; ---------- Trackpad pan (Win32 only) ----------

#+win32
(defun om-zoom-touch-pan-handler (pane x y dx dy)
  "Two-finger pan handler (Win32 trackpad). DX/DY are pixel deltas."
  (declare (ignore x y))
  (when (and (typep pane 'om-scroller)
             (or (not (zerop dx)) (not (zerop dy))))
    (let* ((pos   (om-scroll-position pane))
           (hpos  (om-point-h pos))
           (vpos  (om-point-v pos))
           (new-x (max 0 (+ hpos (round dx))))
           (new-y (max 0 (+ vpos (round dy)))))
      (om-move-scroll-position pane (om-make-point new-x new-y))
      (om-set-h-scroll-position pane new-x)
      (om-set-v-scroll-position pane new-y)
      (om-invalidate-view pane t))))

;;; ---------- Exports ----------

(export '(om-zoom-touch-anchor
          om-zoom-scroll-pane
          #+win32 *om-shift-wheel-hscroll-step*
          om-shift-wheel-hscroll-handler
          om-zoom-touch-swipe-handler
          om-zoom-ctrl-arrow-handler
          om-zoom-touch-handler
          om-zoom-touch-applies-p
          #+win32 om-zoom-touch-pan-handler)
        :oa)
