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

;;; ---------- Clamp ----------

(defun om-clamp-zoom (z)
  "Clamp Z to [+om-zoom-min+, +om-zoom-max+]."
  (min +om-zoom-max+ (max +om-zoom-min+ z)))

;;; ---------- Point scaling (LOGICAL <-> VISUAL) ----------

(defun om-zoom-scale-int (n zoom)
  "Round (N * ZOOM) to integer. LOGICAL pixels -> VISUAL pixels."
  (round (* n zoom)))

(defun om-zoom-scale-int-min (n min zoom)
  "Scale N by ZOOM, clamped to a minimum of MIN. Prevents zero-pixel collapse at small zoom."
  (max min (round (* n zoom))))

(defun om-zoom-scale-point (pt factor)
  "PT scaled by FACTOR. Input LOGICAL, output VISUAL."
  (om-make-point (round (* factor (om-point-h pt)))
                 (round (* factor (om-point-v pt)))))

(defun om-zoom-unscale-point (pt factor)
  "PT divided by FACTOR. Input VISUAL, output LOGICAL."
  (om-make-point (round (/ (om-point-h pt) factor))
                 (round (/ (om-point-v pt) factor))))

;;; ---------- LOGICAL coordinate constructors ----------

(defun om-zoom-pt (x y zoom)
  "Build a VISUAL point from LOGICAL (X, Y) coords scaled by ZOOM."
  (om-make-point (om-zoom-scale-int x zoom)
                 (om-zoom-scale-int y zoom)))

(defun om-zoom-rect (x y w h zoom)
  "Return (values vx vy vw vh) as VISUAL pixels from LOGICAL X Y W H scaled by ZOOM."
  (values (om-zoom-scale-int x zoom)
          (om-zoom-scale-int y zoom)
          (om-zoom-scale-int w zoom)
          (om-zoom-scale-int h zoom)))

;;; ---------- Font scaling ----------

(defun om-zoom-scale-font (font zoom)
  "FONT with size scaled by ZOOM; FONT returned unchanged on error."
  (handler-case
      (let* ((desc  (if (typep font 'gp:font-description)
                        font
                        (gp:font-description font)))
             (attrs (gp:font-description-attributes desc))
             (size  (or (getf attrs :size) 12)))
        (gp:augment-font-description desc :size (max 1 (round (* size zoom)))))
    ;; gp:font-description signals when font is nil or a non-font keyword
    ;; (e.g. :default) before the system is fully initialised; return
    ;; original so callers never lose their reference.
    (error (c) (om-zoom-log-error :scale-font c) font)))

;;; ---------- Logical-state snapshots (idempotent on first zoom) ----------

(defun om-zoom-capture-logical-geom (frame)
  "Snapshot FRAME's current pos and size as the LOGICAL reference; idempotent."
  (unless (om-zoom-logical-pos frame)
    (setf (om-zoom-logical-pos frame) (om-view-position frame)))
  (unless (om-zoom-logical-size frame)
    (setf (om-zoom-logical-size frame) (om-view-size frame))))

(defun om-zoom-capture-logical-font (frame)
  "Snapshot FRAME's current font as the LOGICAL reference; idempotent."
  (unless (om-zoom-logical-font frame)
    (let ((current (om-get-font frame)))
      (when current
        (setf (om-zoom-logical-font frame) current)))))

;;; ---------- Exports ----------

(export '(om-clamp-zoom
          om-zoom-scale-int
          om-zoom-scale-int-min
          om-zoom-scale-point
          om-zoom-unscale-point
          om-zoom-pt
          om-zoom-rect
          om-zoom-scale-font
          om-zoom-capture-logical-geom
          om-zoom-capture-logical-font)
        :oa)
