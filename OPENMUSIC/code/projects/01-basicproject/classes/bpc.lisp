;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)
 
(defclass* bpc (bpf)
   ()
   (:icon 402)
   (:documentation "
BREAK-POINTS CURVE: a 2D function defined by a list of [x,y] coordinates.

BPC objects are constructed from the list of X coordinates (<x-points>) and the list of Y coordinates (<y-points>)
<x-point> are NOT necesarily increasing (contrary to BPF objects).
If <x-list> and <y-list> are not of the same length, the last step in the shorter one is repeated.

<decimals> allows to specify the precision of the function (0 = integers, n > 0 = number of decimals)
")
   )
   

;; no sort by '< x like in BPF...
(defmethod cons-bpf ((self bpc) points)
   (setf (point-list self) points))

(defmethod default-point-list ((self bpc)) nil)

;==================Changes from BPF=============================== 

;-------------------insert a point in the x axe-----------------------
(defmethod insert-point ((self bpc) new-point)
    (cons-bpf self (append (point-list self)  (list new-point))))
      

(defmethod move-points-in-bpf ((self bpc) points deltax deltay)
  (let (moved)
    (loop for point in points do
          (let ((pt (om-add-points (car point) (om-make-point deltax deltay))))
            (push (list pt (second point)) moved)
            (setf (nth (second point) (point-list self)) pt)))
          moved))



;-------------------get the points fall in the interval (y1 y2) ------------------------------
           
;;;changed for windows
(defmethod give-points-in-rect ((self bpc) tl br)
  (let ((user-rect (om-pts-to-rect tl br)))
    (let (res-points)
      (loop for item in (point-list self)
            for i = 0  then (+ i 1) do
            (when (om-point-in-rect-p item user-rect) 
              (push (list item i) res-points)))
      res-points)))



(defmethod coerce2bpc ((Self bpf) (Type bpc))
  (let ((new-bpf (make-instance (type-of type) :from-file t)))
    (cons-bpf new-bpf (copy-list (point-list self)))
    (setf (bpfcolor new-bpf) (bpfcolor self))
    (setf  (decimals new-bpf) (decimals self))
    new-bpf))

(defmethod coerce2bpc ((Self bpc) (Type bpc))
  self)


(defmethod* Objfromobjs ((Self bpf) (Type bpc))
  (coerce2bpc self type))

(defmethod* objFromObjs ((self bpc) (type bpc))
  (clone self))

(defmethod* Objfromobjs ((Self bpc) (Type bpf))
  (let ((new-bpf (make-instance (type-of type) :from-file t)))
    (cons-bpf new-bpf (sort  (remove-duplicates (copy-list (point-list self)) :key 'om-point-h) '< :key 'om-point-h))
    (setf (bpfcolor new-bpf) (bpfcolor self))
    (setf  (decimals new-bpf) (decimals self))
    new-bpf))



;===============================================
;Methods for init box
;===============================================

(defmethod get-initval ((self bpc)) (make-instance (class-of self) :point-list nil))

(defmethod get-editor-class ((self bpc)) 'bpcEditor)


;drawing


(defmethod draw-obj-in-rect ((self bpc) x x1 y y1 edparams view)
  (om-with-focused-view view
    (om-with-fg-color view (bpfcolor self)
      (draw-bpc-in-rect self x x1 y y1 (give-bpf-range self)))))

(defun draw-bpc-in-rect (bpf x x1 y y1 ranges)
  (if (= 1 (length (point-list bpf)))
      (let* ((pix-point (point-to-pixel-with-sizes ranges (car (point-list bpf)) (- x1 x) (- y1 y))))
        (om-fill-rect (+ x (- (om-point-h pix-point) 1)) (+ y (- (om-point-v pix-point) 1)) 3 3))
    (loop for thepoint in (point-list bpf)
        for i = 0 then (+ i 1) do
        (let* ((pix-point (point-to-pixel-with-sizes ranges thepoint (- x1 x) (- y1 y)))
               (next-point (nth (+ i 1) (point-list bpf)))
               (lines t))
          (when lines
            (when next-point
              (let ((next-pixel (point-to-pixel-with-sizes ranges next-point (- x1 x) (- y1 y))))
                (om-draw-line (+ x (om-point-h pix-point)) (+ y (om-point-v pix-point))
                              (+ x (om-point-h next-pixel)) (+ y (om-point-v next-pixel))))))
          (unless lines
            (om-draw-rect (- (om-point-h pix-point) 1) (- (om-point-v pix-point) 1) 3 3))))))



    
;------------------
(defclass* bpc-lib (bpf-lib) 
  ((bpf-list :initform (list (make-instance 'bpc :point-list nil)) 
             :initarg :bpf-list :accessor bpf-list :type list
             :documentation "list of BPC objects"))
  (:icon 403)
  (:documentation "A list of BPC objects gathered in a same object and editor.

The precision of the BPC-Lib and editor is the maximum precision (<decimals> value) of the BPCs included."))

(defmethod draw-obj-in-rect ((self  bpc-lib) x x1 y y1 edparams view)
   (let* ((bpf-list (bpf-list self))
          (ranges (get-miniview-bpf-range bpf-list)))
     (om-with-focused-view view
       (loop for bpf in bpf-list do
             (om-with-fg-color view (bpfcolor bpf)
               (draw-bpc-in-rect bpf x x1 y y1 ranges))))))



(defmethod get-editor-class ((self bpc-lib)) 'bpcEditor)

(defmethod get-initval      ((self bpc-lib))
  (make-instance (class-of self)
    :bpf-list (list (make-instance 'bpc :point-list nil))))

   
(defmethod* Objfromobjs ((Self list) (Type bpc-lib))
  (when (list-subtypep self 'bpf)
    (make-instance (class-of Type) :bpf-list (loop for item in self
                                                   collect (coerce2bpc (Clone item) (make-instance 'bpc))))))


(defmethod* Objfromobjs ((Self bpc-lib) (Type bpf)) 
  (Clone (coerce2bpc (car (bpf-list self)) (make-instance 'bpc))))


(defmethod* Objfromobjs ((Self bpf) (Type bpc-lib))
  (make-instance (class-of Type) :bpf-list (list (coerce2bpc (Clone self) (make-instance 'bpc)))))



;; pour le slot bpf-list
(defmethod good-type-p ((self bpc) (container bpf-lib)) nil)

(defmethod good-type-p ((self bpf) (container bpc-lib)) nil)
(defmethod good-type-p ((self bpc) (container bpc-lib)) t)




