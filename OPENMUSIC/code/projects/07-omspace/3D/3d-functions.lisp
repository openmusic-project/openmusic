(in-package :om)

;;;=======================================
;;; FUNCTIONS FOR 3DCs and 3D-trajectories
;;;=======================================

;;; INTERPOLATION
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

;;; RESAMPLE
(defmethod! 3D-sample ((self 3Dc) (samples number)  &optional decimals)
            :icon '(910)
            :initvals '(nil 1000 nil) ;
            :indoc '("object (3Dc)" "number of samples" "decimals")
            :numouts 4
            :doc "samples a 3Dc"  
            (let ((x (third (multiple-value-list (om-sample (x-points self) samples))))
                  (y (third (multiple-value-list (om-sample (y-points self) samples))))
                  (z (third (multiple-value-list (om-sample (z-points self) samples)))))
              (values (3dc-from-list x y z (type-of self) (or decimals (decimals self)))
                      x y z)))

(defmethod! 3D-sample ((self 3d-trajectory) (samples number)  &optional decimals)
            :icon '(910)
            :initvals '(nil 1000 nil) ;
            :indoc '("object (3Dc)" "number of samples" "decimals")
            :numouts 4
            :doc "samples a 3Dc"  
            (let ((x (third (multiple-value-list (om-sample (x-points self) samples))))
                  (y (third (multiple-value-list (om-sample (y-points self) samples))))
                  (z (third (multiple-value-list (om-sample (z-points self) samples))))
                  (times (third (multiple-value-list (om-sample (times (get-full-trajectory self)) samples)))))
              (values (traject-from-list x y z times (type-of self) (or decimals (decimals self)))
                      x y z)))

;;;=======================================
;;; REDEFINITION OF BPC FUNCTIONS FOR 3DCs and 3D-trajectories from OMPrisma
;;; by M.Schumacher, http://sourceforge.net/p/omprisma/
;;;=======================================

;;; ROTATION 
;;; From OMPrisma traj-rotate
(defmethod* om-rotate ((self 3dc) (axis symbol) (degrees number))  
             (let* ((xpoints (x-points self))
                    (ypoints (y-points self))
                    (zpoints (z-points self))
                    (aed (multiple-value-list (cond ((equal axis 'yaw) (xyz->aed xpoints ypoints zpoints))
                                                    ((equal axis 'pitch) (xyz->aed zpoints ypoints xpoints))
                                                    ((equal axis 'roll) (xyz->aed xpoints zpoints ypoints)))))
                    (e (second aed))
                    (d (third aed))
                    (a (om+ (first aed) degrees))
                    (prelim-xyz (multiple-value-list (aed->xyz a e d)))
                    (x (first prelim-xyz))
                    (y (second prelim-xyz))
                    (z (third prelim-xyz)))
               (cond ((equal axis 'yaw) (3dc-from-list x y z (type-of self) (decimals self)))
                     ((equal axis 'pitch) (3dc-from-list z y x (type-of self) (decimals self)))
                     ((equal axis 'roll) (3dc-from-list x z y (type-of self) (decimals self))))
               ))

(defmethod* om-rotate ((self 3d-trajectory) (axis symbol) (degrees number))  
            (let* ((xpoints (x-points self))
                   (ypoints (y-points self))
                   (zpoints (z-points self))
                   (aed (multiple-value-list (cond ((equal axis 'yaw) (xyz->aed xpoints ypoints zpoints))
                                                   ((equal axis 'pitch) (xyz->aed zpoints ypoints xpoints))
                                                   ((equal axis 'roll) (xyz->aed xpoints zpoints ypoints)))))
                   (e (second aed))
                   (d (third aed))
                   (a (om+ (first aed) degrees))
                   (prelim-xyz (multiple-value-list (aed->xyz a e d)))
                   (x (first prelim-xyz))
                   (y (second prelim-xyz))
                   (z (third prelim-xyz)))
              (cond ((equal axis 'yaw) (traject-from-list x y z (times self) 
                                                          (type-of self) (decimals self) 
                                                          (sample-params self) (interpol-mode self)))
                    ((equal axis 'pitch) (traject-from-list z y x (times self) 
                                                            (type-of self) (decimals self)
                                                            (sample-params self) (interpol-mode self)))
                    ((equal axis 'roll) (traject-from-list x z y (times self) 
                                                           (type-of self) (decimals self) 
                                                           (sample-params self) (interpol-mode self))))
              ))

;;; For 3DC-libs
(defmethod! om-rotate ((self 3dc-lib) (axis symbol) (degrees number))
            (let ((the3dc-lib (make-instance '3dc-lib)))
              (setf (bpf-list the3dc-lib) (mapcar (lambda (thelist) (om-rotate thelist axis degrees)) (bpf-list self)))
              the3dc-lib)
            )

;;; TRANSLATION 
;;; From OMPrisma traj-translate
(defmethod! om-translate ((self 3dc) &key x y z)  
            (let (mybpf (thex x) (they y) (thez z))
              (unless (numberp x) (setf thex 0))
              (unless (numberp y) (setf they 0))
              (unless (numberp z) (setf thez 0))
              (3dc-from-list (om+ (x-points self) thex) (om+ (y-points self) they) (om+ (z-points self) thez) (type-of self) (decimals self))
              ))

(defmethod! om-translate ((self 3d-trajectory) &key x y z)  
            (let (mybpf (thex x) (they y) (thez z))
              (unless (numberp x) (setf thex 0))
              (unless (numberp y) (setf they 0))
              (unless (numberp z) (setf thez 0))
              (traject-from-list (om+ (x-points self) thex) (om+ (y-points self) they) (om+ (z-points self) thez) 
                                 (times self) (type-of self) (decimals self) 
                                 (sample-params self) (interpol-mode self))
              ))

;*** For 3DC-libs
(defmethod! om-translate ((self 3dc-lib) &key x y z)
            (let ((the3dc-lib (make-instance '3dc-lib)))
              (setf (bpf-list the3dc-lib) (mapcar (lambda (thelist) (om-translate thelist :x x :y y :z z)) (bpf-list self)))
              the3dc-lib)
            )

;;; MIRROR
;;; From OMPrisma traj-mirror
(defmethod! om-mirror ((self 3D-trajectory) &key x y z)
            (traject-from-list (if x (om* -1 (x-points self)) (x-points self)) 
                               (if y (om* -1 (y-points self)) (y-points self)) 
                               (if z (om* -1 (z-points self)) (z-points self)) 
                               (times self) '3d-trajectory (decimals self) (sample-params self) (interpol-mode self))
                   )


;*** for 3DCs
(defmethod! om-mirror ((self 3DC) &key x y z)
              (3Dc-from-list (if x (om* -1 (x-points self)) (x-points self)) 
                             (if y (om* -1 (y-points self)) (y-points self)) 
                             (if z (om* -1 (z-points self)) (z-points self))
                             '3Dc (decimals self))
              )

;*** For 3DC-libs
(defmethod! om-mirror ((self 3dc-lib) &key x y z)
            (let ((the3dc-lib (make-instance '3dc-lib)))
              (setf (bpf-list the3dc-lib) (mapcar (lambda (thelist) (om-mirror thelist :x x :y y :z z)) (bpf-list self)))
              the3dc-lib)
            )