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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;Author: Paulo Henrique Raposo

(in-package :om)

;;; Box/icon/miniview font wrappers; targets *ombox-font* (kernel),
;;; *icon-size-factor* (kernel), *miniview-font-size* (musicproject).

(defun om-current-ombox-font (frame)
  (let ((zoom (oa::om-zoom-effective frame)))
    (if (or (null *ombox-font*) (= zoom 1.0))
        *ombox-font*
        (oa::om-zoom-scale-font *ombox-font* zoom))))

(defun om-current-icon-size-factor (frame)
  (let ((zoom (oa::om-zoom-effective frame)))
    (if (= zoom 1.0)
        *icon-size-factor*
        (* *icon-size-factor* zoom))))

(defun om-current-miniview-font-size (frame)
  (let ((zoom (oa::om-zoom-effective frame)))
    (if (= zoom 1.0)
        *miniview-font-size*
        (max 1 (round (* *miniview-font-size* zoom))))))
