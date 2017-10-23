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

(defmethod allowed-in-maq-p ((self score-element)) t)
(defmethod allowed-in-maq-p ((self voice)) t)
(defmethod allowed-in-maq-p ((self poly)) t)
(defmethod allowed-in-maq-p ((self measure)) t)
(defmethod allowed-in-maq-p ((self chord)) t)
(defmethod allowed-in-maq-p ((self note)) t)

(defmethod allow-strech-p ((self sequence*) (factor number)) factor)






