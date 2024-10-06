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

(in-package :om)

(defun get-graphic-attribute (graphic-list key)
  (let ((pos (position key graphic-list)))
    (if pos (nth (+ pos 1) graphic-list)
      (def-graphic-attribute key))))

(defun def-graphic-attribute (key)
  (case key
    (:color *om-black-color*)
    (:bg-color *om-white-color*)
    (:font *om-default-font1*)
    (:line t) (:fill nil) (:size 1)))


(defclass! graphic-object () 
  ((graph-type :initform 'line :accessor graph-type :initarg :graph-type)
   (graph-points :initform nil :accessor graph-points :initarg :graph-points)
   (graph-color :initform nil :accessor graph-color :initarg :graph-color)
   (graph-line :initform t :accessor graph-line :initarg :graph-line)
   (graph-size :initform 1 :accessor graph-size :initarg :graph-size)
   (graph-fill :initform nil :accessor graph-fill :initarg :graph-fill)
   (graph-contents :initform nil :accessor graph-contents :initarg :graph-contents)
   )
  (:icon 491))

(defmethod get-slot-in-out-names ((self graphic-object))
   (values '("self" "graph-type" "graph-points" "graph-color" "graph-line" "graph-size" "graph-fill" "graph-contents")
           (list nil 'line nil *om-black-color* t 1 nil nil)
           '("object" "type of graphics" "list of (x y)" "" "normal or 'dash" "" "" "")
           '(nil ((2 (("line" 'line) ("arrow" 'arrow) ("free shape" 'pen) ("circle" 'cercle) ("rectangle" 'rect) ("polygon" 'polyg) ("test" 'text))))
                 nil nil nil nil nil nil)))


(defmethod graphics ((self picture))
  (mapcar 'get-graphics (extraobjs self)))

(defmethod (setf graphics) (graphics-list (self picture))
  (setf (slot-value self 'graphics) (list! graphics-list))
  (setf (extraobjs self) (mapcar 'graphics-to-list (list! graphics-list)))
  graphics-list)

(defun get-graphics (extra)
  (let ((tool (car extra))
        (points (second extra))
        (params (if (equal 'text (car extra))
                    (list (nth 0 (third extra))  ;; color 
                          (om-font-size (nth 3 (third extra)))
                          (om-font-face (nth 3 (third extra)))
                          (om-font-style (nth 3 (third extra))))
                  (third extra))))
    (make-instance 'graphic-object
                   :graph-type tool
                   :graph-points (if (numberp (car points)) (group-list points 2 'linear) points)
                   :graph-color (om-correct-color (nth 0 params))
                   :graph-size (nth 1 params)
                   :graph-line (nth 2 params) 
                   :graph-fill (nth 3 params)
                   :graph-contents (nth 3 extra))))


(defun graphics-to-list (graphics)
  (list (graph-type graphics)
        (if (member (graph-type graphics) '(pen polyg))
            (graph-points graphics) (flat (graph-points graphics)))
        (if (equal 'text (graph-type graphics))
            (list (or (graph-color graphics) (def-graphic-attribute :color))
                  1 t (om-make-font (graph-line graphics) (graph-size graphics) :style (graph-fill graphics)))
          (list (om-correct-color (graph-color graphics)) 
                (graph-size graphics) 
                (if (equal 'dash (graph-line graphics)) 
                    (list (* 2 (graph-size graphics)) (* 2 (graph-size graphics)))
                  (graph-line graphics))
                (graph-fill graphics)))
        (graph-contents graphics)))

            










