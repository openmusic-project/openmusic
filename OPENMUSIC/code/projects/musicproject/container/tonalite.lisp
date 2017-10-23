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
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson
;=========================================================================

(in-package :om)

;;; soit comme ca soit en faisant deux classes diffŽrentes.. --> ?
(defclass tonal-object ()
  ((tonalite :accessor tonalite :initarg :tonalite :initform nil)
   (tonal-values :accessor tonal-values :initarg :tonal-values :initform nil)))

(defmethod tonal-object-p ((self t)) nil)
(defmethod tonal-object-p ((self tonal-object)) t)


;;; si on demande la tonalite d'un objet qui n'est pas un tonal-object :
(defmethod tonalite ((self t)) nil)

(defmethod set-tonalite ((self t) (tonalite t)) nil)


;;;====================================================

(defvar *om-tonalite* nil)

;; TO BE REDEFINED IN TONALITE
;; (si *om-tonalite* = t)

(defmethod score-set-tonalite ((self t))
  (print "sorry, harmonic project is not loaded..."))

(defmethod score-remove-tonalite ((self t))
  (om-print "sorry, harmonic project is not loaded..."))

(defmethod set-editor-tonality ((self t)) nil)

(defmethod draw-general-tonality ((self t)) nil)
(defmethod draw-tonalite ((self t) x y zoom size begin panel) nil)
(defmethod draw-modulation ((self t) (newchord t) grap-chord x y zoom size panel) nil)
(defmethod draw-degre ((self t) realpos y size) nil)
(defmethod draw-chiffrage ((self t) x y zoom size) nil)
(defmethod draw-armure ((self t) armure x top xsize fontsize deltay) nil)

