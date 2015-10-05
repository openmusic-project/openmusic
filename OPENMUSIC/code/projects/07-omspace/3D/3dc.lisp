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
;Authors: J. Bresson


(in-package :om)

(defstruct 3dpoint
   (x 0.0)
   (y 0.0)
   (z 0.0))

(defmethod om-point-h ((self 3Dpoint)) (3dpoint-x self))
(defmethod om-point-v ((self 3Dpoint)) (3dpoint-y self))
(defmethod om-point-x ((self 3Dpoint)) (3dpoint-x self))
(defmethod om-point-y ((self 3Dpoint)) (3dpoint-y self))
(defmethod om-point-z ((self 3Dpoint)) (3dpoint-z self))
(defmethod om-point-z ((self t)) 0)


(defmethod om-point-p ((self 3dpoint)) t)

(defmethod om-points-equal-p ((point1 3dpoint) (point2 3dpoint))
  (and (= (3Dpoint-x point1) (3Dpoint-x point2))
       (= (3Dpoint-y point1) (3Dpoint-y point2))
       (= (3Dpoint-z point1) (3Dpoint-z point2))))

(defmethod om-add-points ((p1 3dpoint) p2)
  (make-3dpoint :x (+ (3Dpoint-x p1) (om-point-x p2) 
                :y (+ (3Dpoint-y p1) (om-point-y p2)) 
                :z (+ (3Dpoint-z p1) (om-point-z p2)))))

(defmethod om-add-points (p1 (p2 3dpoint))
  (make-3dpoint :x (+ (om-point-x p1) (om-point-x p2) 
                :y (+ (om-point-y p1) (om-point-y p2)) 
                :z (+ (om-point-z p1) (om-point-z p2)))))

(defmethod om-subtract-points ((p1 3dpoint) p2)
  (make-3dpoint :x (- (om-point-x p1) (om-point-x p2) 
                :y (- (om-point-y p1) (om-point-y p2)) 
                :z (- (om-point-z p1) (om-point-z p2)))))

(defmethod om-subtract-points (p1 (p2 3dpoint))
  (make-3dpoint :x (- (om-point-x p1) (om-point-x p2) 
                :y (- (om-point-y p1) (om-point-y p2)) 
                :z (- (om-point-z p1) (om-point-z p2)))))


(defmethod om-point-* ((point 3dpoint) fact)
  (make-3dpoint :x (* (3Dpoint-x point) fact) 
                :y (* (3Dpoint-y point) fact) 
                :z (* (3Dpoint-z point) fact)))


(defmethod om-points-distance ((p1 3Dpoint) (p2 3Dpoint))
  (sqrt (+ (expt (- (3Dpoint-x p2) (3Dpoint-x p1)) 2)
           (expt (- (3Dpoint-y p2) (3Dpoint-y p1)) 2)
           (expt (- (3Dpoint-z p2) (3Dpoint-z p1)) 2))))

;(let ((p1 (make-3dpoint :x 10 :y 10 :z 1))
;      (p2 (make-3dpoint :x 8 :y 4 :z 0)))
;  (om-points-distance p1 p2))
;  (sqrt (+ (expt (- (3Dpoint-x p2) (3Dpoint-x p1)) 2) 
;           (expt (- (3Dpoint-y p2) (3Dpoint-y p1)) 2)
;           (expt (- (3Dpoint-z p2) (3Dpoint-z p1)) 2))))




(defmethod om-save-point ((self 3dpoint))
  (when self
    `(make-3Dpoint :x ,(3Dpoint-x self) :y ,(3Dpoint-y self) :z ,(3Dpoint-z self))))

(defmethod om-copy-point ((self 3dpoint))
  (when self
    `(make-3Dpoint :x ,(3Dpoint-x self) :y ,(3Dpoint-y self) :z ,(3Dpoint-z self))))

(defmethod copy-point ((self 3dpoint))
  (when self
    (make-3Dpoint :x (3Dpoint-x self) :y (3Dpoint-y self) :z (3Dpoint-z self))))

(defclass! 3DC (bpc)
  ((x-points :initform '(0 10) :initarg :x-points :documentation "X coordinates (list)")
   (y-points :initform '(0 100) :initarg :y-points :documentation "Y coordinates (list)")
   (z-points :initform '(0 0) :initarg :z-points :accessor z-points :documentation "Z coordinates (list)")
   (decimals :initform 0 :initarg :decimals :accessor decimals :documentation "precision (number)"))
  (:icon 500)
  (:documentation 
"3D CURVE: a 3D function defined by a list of [x,y,z] coordinates.

3DC objects are constructed from the list of X coordinates (<x-points>), the list of Y coordinates (<y-points>) and the list of Z coordinates (<z-points>).
If <x-list>, <y-list> and <z-list> are not of the same length, the last coordinate (for y and z) or the last step (for x) is repeated in the shorter lists.

<decimals> allows to specify the precision of the function (0 = integers, n > 0 = number of decimals)
"
))

(defmethod default-point-list ((self 3DC)) nil)

(defmethod make-obj-point ((self 3DC) &key (x 0) (y 0) (z 0))
  (make-3Dpoint :x x :y y :z z))

;;; redefined if 3DEditor is loaded
(defmethod get-editor-class ((self 3DC)) (call-next-method))

(defmethod z-points ((self 3DC))
   (let ((fun (if (zerop (decimals self)) #'om-point-z #'(lambda (x) (/ (om-point-z x) (expt 10.0 (decimals self)))))))
     (mapcar fun (point-list self))))

(defmethod (setf x-points) ((x-points t) (self 3DC))
   (let ((new-bpf (3Dc-from-list x-points (y-points self) (z-points self) (type-of self) (decimals self))))
     (cons-bpf self (point-list new-bpf))
     (x-points self)))

(defmethod (setf y-points) ((y-points t) (self 3DC))
   (let ((new-bpf (3Dc-from-list (x-points self) y-points (z-points self) (type-of self) (decimals self))))
     (cons-bpf self (point-list new-bpf))
     (y-points self)))

(defmethod (setf z-points) ((z-points t) (self 3DC))
   (let ((new-bpf (3Dc-from-list (x-points self) (y-points self) z-points (type-of self) (decimals self))))
     (cons-bpf self (point-list new-bpf))
     (z-points self)))

(defmethod change-precision ((self 3DC) decimals)
   (let ((new-bpf (3Dc-from-list (x-points self) (y-points self) (z-points self) (type-of self) decimals)))
     (cons-bpf self (point-list new-bpf))
     (setf (decimals self) decimals)))

(defmethod save-exepcion ((self 3DC)) 
  `(when (find-class ',(type-of self) nil)
     (let ((newbpf (3Dc-from-list ',(x-points self) ',(y-points self) ',(z-points self) ',(type-of self) ,(decimals self))))
       (setf (bpfcolor newbpf) ,(om-save-color (bpfcolor self)))
       (set-name newbpf ,(get-name self))
       newbpf)))

(defmethod default-obj-box-size ((self 3DC)) (om-make-point 60 60))

(defmethod! point-pairs ((self 3DC)) 
  (mat-trans (list (x-points self) (y-points self) (z-points self))))


(defmethod 3Dc-from-list ((listx list) (listy list) (listz list) &optional (class '3DC) (decimals 0))
    (if (and (null listx) (null listy) (null listz))
        (make-instance class :point-list nil :decimals decimals)
  (let ((new-3Dc (make-instance class :from-file t))
        (factor (expt 10 decimals))
        (listx (or listx '(0)))
        (listy (or listy '(0)))
        (listz (or listz '(0))))
    (setf (decimals new-3Dc) decimals)
    (setf listx (om-round (om* listx factor)))
    (setf listy (om-round (om* listy factor)))
    (setf listz (om-round (om* listz factor)))
    (if (and (list-subtypep listx 'number) (list-subtypep listy 'number) (list-subtypep listz 'number))
      (let ((defx (if (= 1 (length listx)) (car listx) (- (car (last listx)) (car (last listx 2)))))
            (defy (if (= 1 (length listy)) (car listy) (- (car (last listy)) (car (last listy 2)))))
            (defz (if (= 1 (length listz)) (car listz) (- (car (last listz)) (car (last listz 2))))))
        (cons-bpf new-3Dc (loop for zpoin = (if listz (pop listz) 0) then (if listz (pop listz) zpoin) ;(+ zpoin defz))
                                for ypoin = (if listy (pop listy) 0) then (if listy (pop listy) ypoin) ;(+ ypoin defy))
                                for xpoin = (if listx (pop listx) 0) then (if listx (pop listx) (+ xpoin defx))
                                while (or listy listx listz)
                                collect (make-3dpoint :x xpoin :y ypoin :z zpoin) into rep
                                finally (return (append rep (list (make-3dpoint :x xpoin :y ypoin :z zpoin)))))
                  )
      new-3Dc)
      (progn
        (om-beep-msg "This is a 3DC factory. Do not use lists of lists, only lists of numbers.")
        (get-initval new-3Dc))))))

(defmethod make-one-instance ((self 3DC) &rest slots-vals)
   (apply '3Dc-from-list (list (first slots-vals) (second slots-vals) (third slots-vals)
                                      (type-of self) 
                                      (or (fourth slots-vals) 0))))


(defmethod coerce-to-3DC (self (type (eql '3DC)))
  (let ((new-3DC (3Dc-from-list (x-points self) (y-points self) '(0 0) type (decimals self))))
    (setf (bpfcolor new-3DC) (bpfcolor self))
    (setf  (decimals new-3DC) (decimals self))
    new-3DC))
  
(defmethod Objfromobjs ((Self bpf) (Type 3DC))
  (coerce-to-3DC self (type-of type)))

(defmethod Objfromobjs ((Self bpc) (Type 3DC)) 
  (coerce-to-3DC self (type-of type)))

(defmethod* objFromObjs ((self 3DC) (type 3DC))
  (clone self))

(defmethod* objFromObjs ((self 3D-trajectory) (type 3DC))
  (let ((new-3DC (3Dc-from-list (x-points self) (y-points self) (z-points self) (type-of type) (decimals self))))
    (setf (bpfcolor new-3DC) (bpfcolor self))
    (setf  (decimals new-3DC) (decimals self))
    new-3DC))

;;;============================
;;; 3DC LIB
;;;============================

(defclass! 3DC-lib (bpc-lib) 
           ((bpf-list :initform (list (make-instance '3DC :point-list nil)) 
             :initarg :bpf-list :accessor bpf-list :type list
             :documentation "list of 3DC objects"))
  (:icon 503)
  (:documentation "A list of 3DC objects gathered in a same object and editor.

The precision of the 3DC-Lib and editor is the maximum precision (<decimals> value) of the BPCs included."))

(defmethod get-editor-class ((self 3DC-lib)) '3DEditor)

(defmethod get-initval ((self 3DC-lib))
  (make-instance (class-of self)
    :bpf-list (list (make-instance '3DC :point-list nil))))

(defmethod good-type-p ((self 3DC) (container 3DC-lib)) t)

