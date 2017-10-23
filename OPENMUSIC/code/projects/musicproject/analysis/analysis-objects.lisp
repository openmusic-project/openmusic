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

;;;===========================
;;; PREDEFINED ANALYSIS CLASSES
;;;===========================

(in-package :om)

;;;============================
;;; BASIC DEFAULT SEGMENTATION
(defclass! simple-segmentation (abstract-analysis) ())

(defmethod default-segment-class ((self simple-segmentation)) 'marker-segment)

(defmethod compatible-analysis-p ((analyse simple-segmentation) (object voice)) nil)
(defmethod compatible-analysis-p ((analyse simple-segmentation) (object poly)) nil)



;;;============================
;;; MEASURE-TREES
;;; Segments = measure
;;; Segment-data = the RT
(defclass! MEASURE-TREES (ABSTRACT-ANALYSIS) ())

(defmethod compatible-analysis-p ((analyse MEASURE-TREES) (object voice)) t)
(defmethod compatible-analysis-p ((analyse MEASURE-TREES) (object t)) nil)

(defmethod default-segment-class ((self measure-trees)) 'measure-segment)

(defmethod compute-segments-p ((self measure-trees)) nil)
(defmethod analyse-segments-p ((self measure-trees)) nil)
(defmethod compute+analyse-segments-p ((self measure-trees)) t)

(defmethod compute-analysis-segments ((self measure-trees) (object voice)) 
  (loop for m in (inside object)
        for i = 0 then (+ i 1) collect
        (make-instance 'measure-segment :mesure-id i)))

(defmethod analyse-one-segment ((self measure-trees) (seg measure-segment) (object t))
  (setf (segment-data seg) (tree (mesure seg))))

(defmethod draw-segment-data ((self measure-trees) segment view)
  (let ((x1 (time-to-pixels view (segment-begin segment))))
    (om-with-font *om-default-font1*
                  (om-draw-string x1 (- (h view) (if (oddp (mesure-id segment)) 60 80))
                                  (segment-data-tostring self segment)))))

(defmethod segment-data-tostring ((self measure-trees) segment) 
  (when (segment-data segment)
    (format nil "~A" (segment-data segment))))

