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
;===========================================================================


(in-package :om)

;;;=======================================
;;; FUNCTIONS FOR 3DCs and 3D-trajectories
;;;=======================================

;;;==============
;;; INTERPOLATION
;;;==============

(defmethod 3D-interpolation ((first 3DC) (second 3DC) (steps number) 
                             &optional (curve 0.0) (decimals nil) (mode 'points))
  (cond ((equal mode 'points)
         (let ((interp-x (interpolation (x-points first) (x-points second) steps curve))
               (interp-y (interpolation (y-points first) (y-points second) steps curve))
               (interp-z (interpolation (z-points first) (z-points second) steps curve)))
           (values 
            (loop for x1 in interp-x for y1 in interp-y for z1 in interp-z
                  collect (3dc-from-list x1 y1 z1 (type-of first) (or decimals (decimals first))))
            interp-x interp-y interp-z)
           ))
        ((equal mode 'sample)
         (let ((l1 (length (point-list first)))
               (l2 (length (point-list second))))
           (cond ((> l1 l2) (3D-interpolation first (om-sample second l1) steps curve decimals 'points))
                 ((> l2 l1) (3D-interpolation (om-sample first l2) second steps curve decimals 'points))
                 (t (3D-interpolation first second steps curve decimals 'points)))
           ))
        ))

(defmethod* 3D-interpol ((first 3DC) (second 3DC) (steps number) &optional (curve 0.0) (decimals nil) (mode 'points))
   :icon 213   
            :initvals '(nil nil 1 0.0 nil 'points) 
            :indoc '("a 3DC or trajectory" "a 3DC or trajectory" "number of steps" "interpolation curve" "precision" "interpolation mode")
            :outdoc '("list of 3DC" "list of x-points" "list of y-points" "list of z-points")
            :numouts 4
            :menuins '((5 (("points to point" 'points) ("resample curves" 'sample))))
            :doc 
"Interpolates between two 3D curves or trajectories).

<steps> is the number of interpolated curves wanted, including <first> and <second>.
  1 means one value between <first> and <seconds>.
  2 will return only <first> and <second>.
  3 will return <first> and <second> with one more curve in between.
  etc.

<curve> in an exponential factor for the interpolation curve (0 = linear).

<decimals> is the precision (number of decimals) in the returned curves (default NIL = precision of <first>).

<mode> determines how interpolation is done :
 - 'points is point-by-point interpolation: the curves must have the same number of points, or the bigger one will be truncated.
 - 'sample means that the curves are resampled before interpolation. In case of BPfs, x-points will be added if needed so that the interpolated curves all have the same x points. In case of BPCs, the one with fewer points is resampled, then point-by-point interpolation is applied.

Outputs
 1) list of interpolated BPFs/BPCs
 2) list of x-points
 3) list of y-points
 4) list of z-points
"
   (3D-interpolation first second steps curve decimals mode))


(defmethod* 3D-interpol ((first 3d-trajectory) (second 3d-trajectory) (steps number) 
                         &optional (curve 0.0) (decimals nil) (mode 'points))
  (multiple-value-bind (3DCs xx yy zz)
       (3D-interpolation first second steps curve decimals mode)
     (let ((times (if (> (length (point-pairs first)) (length (point-pairs second))) 
                      (times (get-full-trajectory first)) (times (get-full-trajectory second)))))
       (values 
        (loop for x1 in xx for y1 in yy for z1 in zz
              collect (traject-from-list x1 y1 z1 times (type-of first) (or decimals (decimals first))))
        xx yy zz 
        ))))

;;;==============
;;; RESAMPLE
;;;==============

(defmethod* 3D-sample ((self 3Dc) (samples number)  &optional decimals)
            :icon 910
            :initvals '(nil 1000 nil) ;
            :indoc '("object (3Dc/3D-trajectory)" "number of samples" "decimals")
            :numouts 4
            :doc "samples a 3Dc"  
            (let ((x (third (multiple-value-list (om-sample (x-points self) samples))))
                  (y (third (multiple-value-list (om-sample (y-points self) samples))))
                  (z (third (multiple-value-list (om-sample (z-points self) samples)))))
              (values (3dc-from-list x y z (type-of self) (or decimals (decimals self)))
                      x y z)))

(defmethod* 3D-sample ((self 3d-trajectory) (samples number)  &optional decimals)
  (let ((x (third (multiple-value-list (om-sample (x-points self) samples))))
        (y (third (multiple-value-list (om-sample (y-points self) samples))))
        (z (third (multiple-value-list (om-sample (z-points self) samples))))
        (times (third (multiple-value-list (om-sample (times (get-full-trajectory self)) samples)))))
    (values (traject-from-list x y z times (type-of self) (or decimals (decimals self)))
            x y z)))

;;;==============
;;; SPLINE
;;;==============

(defmethod* 3D-spline ((self 3Dc) (resolution integer) (degree integer))
  :icon 234
  :initvals '(nil 100 3)
  :indoc '("a 3DC Or 3D-Trajectory" "number of points" "interpolation degree")
  :numouts 4
  :doc "Computes a B-Spline curve from the control points in the 3DC.

B-Splines are smoothed curves where each point is computed by polynomial interpolation from a set of control points.

Returned values :
 - The result as an object (3DC or 3D-trajectory) (1st output)
 - The list of x points (2nd output)
 - The list of y points (2nd output)
 - The list of z points (2nd output)

<resolution> is the number of points in the resulting curve
<degree> is the degree of the polynomial interpolation. higher degrees give smoother curves

Note that splines are supposed to be computed from reltively few control points. "

  (let ((xy (multiple-value-list (om-spline (simple-bpf-from-list (x-points self) (y-points self) 'bpc (decimals self)) resolution degree)))
        (xz (multiple-value-list (om-spline (simple-bpf-from-list (x-points self) (z-points self) 'bpc (decimals self)) resolution degree))))
        
    (values (3dc-from-list (nth 1 xy) (nth 2 xy) (nth 2 xz) (type-of self) (decimals self))
            (nth 1 xy) (nth 2 xy) (nth 2 xz))
    ))


(defmethod* 3D-spline ((self 3d-trajectory) (resolution integer) (degree integer))
  
  (let ((xy (multiple-value-list (om-spline (simple-bpf-from-list (x-points self) (y-points self) 'bpc (decimals self)) resolution degree)))
        (xz (multiple-value-list (om-spline (simple-bpf-from-list (x-points self) (z-points self) 'bpc (decimals self)) resolution degree)))
        (times (third (multiple-value-list (om-sample (times (get-full-trajectory self)) resolution)))))
        
    (values (traject-from-list (nth 1 xy) (nth 2 xy) (nth 2 xz) times (type-of self) (decimals self))
            (nth 1 xy) (nth 2 xy) (nth 2 xz))
    ))



;;;=======================================
;;; REDEFINITION OF BPC FUNCTIONS FOR 3DCs and 3D-trajectories from OMPrisma
;;; by M.Schumacher, http://sourceforge.net/p/omprisma/
;;;=======================================

;;; ROTATION 
;;; From OMPrisma traj-rotate
(defmethod* om-rotate ((self 3dc) &key (yaw 0) (pitch 0) (roll 0))  
   (let* ((res self))
     (when (and yaw (not (zerop yaw)))
       (setf res 
             (multiple-value-bind (a e d) (xyz->aed (x-points res) (y-points res) (z-points res))
               (multiple-value-bind (x y z) (aed->xyz (om+ a yaw) e d)
                 (3dc-from-list x y z (type-of self) (decimals self))))))
     (when (and pitch (not (zerop pitch)))
       (setf res 
             (multiple-value-bind (a e d) (xyz->aed (z-points res) (y-points res) (x-points res))
               (multiple-value-bind (x y z) (aed->xyz (om+ a pitch) e d)
                 (3dc-from-list z y x (type-of self) (decimals self))))))
     (when (and roll (not (zerop roll)))
       (setf res 
             (multiple-value-bind (a e d) (xyz->aed (x-points res) (z-points res) (y-points res))
               (multiple-value-bind (x y z) (aed->xyz (om+ a roll) e d)
                 (3dc-from-list x z y (type-of self) (decimals self))))))
     (set-color res (bpfcolor self))
     res))
  
  
(defmethod* om-rotate ((self 3d-trajectory) &key (yaw 0) (pitch 0) (roll 0))  
   (let* ((res self))
     (when (and yaw (not (zerop yaw)))
       (setf res 
             (multiple-value-bind (a e d) (xyz->aed (x-points res) (y-points res) (z-points res))
               (multiple-value-bind (x y z) (aed->xyz (om+ a yaw) e d)
                 (traject-from-list x y z (times self) (type-of self) (decimals self) (sample-params self) (interpol-mode self))))))
     (when (and pitch (not (zerop pitch)))
       (setf res 
             (multiple-value-bind (a e d) (xyz->aed (z-points res) (y-points res) (x-points res))
               (multiple-value-bind (x y z) (aed->xyz (om+ a pitch) e d)
                 (traject-from-list z y x (times self) (type-of self) (decimals self) (sample-params self) (interpol-mode self))))))
     (when (and roll (not (zerop roll)))
       (setf res 
             (multiple-value-bind (a e d) (xyz->aed (x-points res) (z-points res) (y-points res))
               (multiple-value-bind (x y z) (aed->xyz (om+ a roll) e d)
                 (traject-from-list x z y (times self) (type-of self) (decimals self) (sample-params self) (interpol-mode self))))))
     (set-color res (bpfcolor self))
     res))



;;; For 3DC-libs
(defmethod! om-rotate ((self 3dc-lib) &key (yaw 0) (pitch 0) (roll 0))
  (let ((the3dc-lib (make-instance '3dc-lib)))
    (setf (bpf-list the3dc-lib) 
          (mapcar 
           #'(lambda (3Dc) (om-rotate 3DC :yaw yaw :pitch pitch :roll roll))
           (bpf-list self)))
    the3dc-lib)
  )

;;; TRANSLATION 
;;; From OMPrisma traj-translate
(defmethod! om-translate ((self 3dc) &key x y z)  
   (let (mybpf (thex x) (they y) (thez z))
     (unless (numberp x) (setf thex 0))
     (unless (numberp y) (setf they 0))
     (unless (numberp z) (setf thez 0))
     (set-color 
      (3dc-from-list (om+ (x-points self) thex) (om+ (y-points self) they) (om+ (z-points self) thez) (type-of self) (decimals self))
      (bpfcolor self))
     ))

(defmethod! om-translate ((self 3d-trajectory) &key x y z)  
  (let (mybpf (thex x) (they y) (thez z))
    (unless (numberp x) (setf thex 0))
    (unless (numberp y) (setf they 0))
    (unless (numberp z) (setf thez 0))
    (set-color 
     (traject-from-list (om+ (x-points self) thex) (om+ (y-points self) they) (om+ (z-points self) thez) 
                        (times self) (type-of self) (decimals self) 
                        (sample-params self) (interpol-mode self))
     (bpfcolor self)
     )))

(defmethod! om-translate ((self 3dc-lib) &key x y z)
  (let ((the3dc-lib (make-instance '3dc-lib)))
    (setf (bpf-list the3dc-lib) (mapcar (lambda (thelist) (om-translate thelist :x x :y y :z z)) (bpf-list self)))
    the3dc-lib))


;;; MIRROR
;;; From OMPrisma traj-mirror
(defmethod! om-mirror ((self 3D-trajectory) &key x y z)
    (set-color 
     (traject-from-list (if x (om* -1 (x-points self)) (x-points self)) 
                        (if y (om* -1 (y-points self)) (y-points self)) 
                        (if z (om* -1 (z-points self)) (z-points self)) 
                        (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
     (bpfcolor self)))


;*** for 3DCs
(defmethod! om-mirror ((self 3DC) &key x y z)
  (set-color 
   (3Dc-from-list (if x (om* -1 (x-points self)) (x-points self)) 
                  (if y (om* -1 (y-points self)) (y-points self)) 
                  (if z (om* -1 (z-points self)) (z-points self))
                  '3Dc (decimals self))
   (bpfcolor self)))

;*** For 3DC-libs
(defmethod! om-mirror ((self 3dc-lib) &key x y z)
            (let ((the3dc-lib (make-instance '3dc-lib)))
              (setf (bpf-list the3dc-lib) (mapcar (lambda (thelist) (om-mirror thelist :x x :y y :z z)) (bpf-list self)))
              the3dc-lib)
            )