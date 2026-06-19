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

;;; Default fonts live in :oa (graphics.lisp:721+).
;;; Box/icon/miniview fonts live in :om and ship from projects/zoom/.

(declaim (special *om-default-font0* *om-default-font1* *om-default-font2*
                  *om-default-font3* *om-default-font4*
                  *om-default-font1b* *om-default-font2b*
                  *om-default-font3b* *om-default-font4b*))

;;; ---------- Default fonts ----------

(defun om-current-default-font0 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font0*) (= zoom 1.0))
        *om-default-font0*
        (om-zoom-scale-font *om-default-font0* zoom))))

(defun om-current-default-font1 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font1*) (= zoom 1.0))
        *om-default-font1*
        (om-zoom-scale-font *om-default-font1* zoom))))

(defun om-current-default-font2 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font2*) (= zoom 1.0))
        *om-default-font2*
        (om-zoom-scale-font *om-default-font2* zoom))))

(defun om-current-default-font3 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font3*) (= zoom 1.0))
        *om-default-font3*
        (om-zoom-scale-font *om-default-font3* zoom))))

(defun om-current-default-font4 (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font4*) (= zoom 1.0))
        *om-default-font4*
        (om-zoom-scale-font *om-default-font4* zoom))))

;;; ---------- Bold variants ----------

(defun om-current-default-font1b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font1b*) (= zoom 1.0))
        *om-default-font1b*
        (om-zoom-scale-font *om-default-font1b* zoom))))

(defun om-current-default-font2b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font2b*) (= zoom 1.0))
        *om-default-font2b*
        (om-zoom-scale-font *om-default-font2b* zoom))))

(defun om-current-default-font3b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font3b*) (= zoom 1.0))
        *om-default-font3b*
        (om-zoom-scale-font *om-default-font3b* zoom))))

(defun om-current-default-font4b (frame)
  (let ((zoom (om-zoom-effective frame)))
    (if (or (null *om-default-font4b*) (= zoom 1.0))
        *om-default-font4b*
        (om-zoom-scale-font *om-default-font4b* zoom))))

;;; ---------- Exports ----------

(export '(om-current-default-font0
          om-current-default-font1
          om-current-default-font2
          om-current-default-font3
          om-current-default-font4
          om-current-default-font1b
          om-current-default-font2b
          om-current-default-font3b
          om-current-default-font4b)
        :oa)
