;OpenMusic :trajectory object
;
;Copyright (C) 1997-2010 by IRCAM-Centre Georges Pompidou, Paris, France.
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

(defstruct timedpoint 
   (point (make-3dpoint :x 0 :y 0 :z 0))
   (time nil))

(defmethod om-point-h ((self timedpoint)) (om-point-h (timedpoint-point self)))
(defmethod om-point-v ((self timedpoint)) (om-point-v (timedpoint-point self)))
(defmethod om-point-x ((self timedpoint)) (om-point-x (timedpoint-point self)))
(defmethod om-point-y ((self timedpoint)) (om-point-y (timedpoint-point self)))
(defmethod om-point-z ((self timedpoint)) (om-point-z (timedpoint-point self)))


(defmethod om-point-p ((self timedpoint)) t)

(defmethod om-points-equal-p ((point1 timedpoint) (point2 timedpoint))
  (and (om-points-equal-p (timedpoint-point point1) (timedpoint-point point2))
       (equal (timedpoint-time point1) (timedpoint-time point2))))

(defmethod om-point-* ((point timedpoint) fact)
  (make-timedpoint :point (om-point-* (timedpoint-point point) fact)
                    :time (timedpoint-time point)))

(defmethod om-points-distance ((p1 timedpoint) (p2 timedpoint))
  (om-points-distance (timedpoint-point p1) (timedpoint-point p2)))

(defmethod om-save-point ((self timedpoint))
  (when self
    `(make-timedpoint :point ,(om-save-point (timedpoint-point self)) 
                      :time ,(timedpoint-time self))))

(defmethod om-copy-point ((self timedpoint))
  (when self
    `(make-timedpoint :point ,(om-copy-point (timedpoint-point self)) 
                      :time ,(timedpoint-time self))))

(defmethod copy-point ((self timedpoint))
  (when self
    (make-timedpoint :point (copy-point (timedpoint-point self)) 
                     :time (timedpoint-time self))))


(defmethod get-point-time ((self timedpoint)) (timedpoint-time self))
(defmethod get-point-time ((self t)) nil)

(defmethod om-add-points ((p1 timedpoint) p2)
  (make-timedpoint :point (om-add-points (timedpoint-point p1) p2) :time (timedpoint-time p1)))

(defmethod om-add-points ((p1 t) (p2 timedpoint))
  (make-timedpoint :point (om-add-points p1 (timedpoint-point p2)) :time (timedpoint-time p2)))

(defmethod om-subtract-points ((p1 timedpoint) p2)
  (make-timedpoint :point (om-subtract-points (timedpoint-point p1) p2) :time (timedpoint-time p1)))

(defmethod om-subtract-points ((p1 t) (p2 timedpoint))
  (make-timedpoint :point (om-subtract-points p1 (timedpoint-point p2)) :time (timedpoint-time p2)))


;;;============================================================

(defclass! 3D-trajectory (3DC)
           ((times :initform nil :accessor times :initarg :times :documentation "time for each point (list)")
            (sample-params :initform nil :accessor sample-params :initarg :sample-params 
                           :documentation "optional computation of actual trajectory points")
            (interpol-mode :initform 'points :accessor interpol-mode :initarg :interpol-mode 
                           :documentation "optional autmatic time comptation mode")
   ;(traject-points :initform nil :accessor traject-points)
            )
  (:icon 502)
  (:documentation "
3D-trajectory represents the trajectory of a point in 3D.
each point can be associated to a time-stamp.

- <xpoints>, <y-points>, <z-points> are lsits of points x, y and z coordinates, respectively
- <decimals> is the precision of the poisition (number of decimals)

- <times> is a list of times assigned to the successive points. 
NIL is allowed, meaning that a given point has no specific time (interpolated from surrounding points)
A single atomic value is interpreted as a global duration of the trajectory

- <interpol-mode> determines how missing times are computed :
     - same time between successive point pairs (may result in accelerations or slow-downs)
     - depending on distance : constant speed

Optional params allow to compute precise trajectories from the object points. Set <sample-params> to 
- NIL : only original points are part of the trajectory
- a float : specifies a sample rate for the trajectory; positions and times will be interpolated accordingly
- a list of floats : specifies sample rates fore each individual segment (last one is repeated if the list is too short)

"
   ))

;; - a list (n order) to compute a spline trajectory of n points using the original positions as control points; 
;; Order should be one of 2,3,4 or 5)


                                                             
(defmethod get-slot-in-out-names ((self 3D-trajectory))
   (values '("self" "x-points" "y-points" "z-points" "decimals" "times" "sample-params" "interpol-mode")
           '(nil nil nil nil 0 nil nil points)
           '("object" "X coordinates (list)" "Y coordinates (list)" "Z coordinates (list)" 
                      "precision (number)" "time for each point (list)"
                      "optional computation of actual trajectory points" "optional autmatic time comptation mode")
           '(nil nil nil nil nil nil nil
             ((7 (("points (constant time)" 'points)  ("distance (constant speed)" 'dist)))))))

(defmethod make-obj-point ((self 3D-trajectory) &key (x 0) (y 0) (z 0))
  (make-timedpoint :point (make-3Dpoint :x x :y y :z z)))

(defmethod times ((self 3D-trajectory))
   (mapcar 'timedpoint-time (point-list self)))

(defmethod (setf times) ((time-list list) (self 3D-trajectory))
  (mapcar #'(lambda (pt time) (setf (timedpoint-time pt) time)) (point-list self) time-list))

(defmethod! get-points-times ((self 3D-trajectory)) 
    (mapcar 'timedpoint-time (point-list self)))

(defmethod (setf x-points) ((x-points t) (self 3D-trajectory))
   (let ((new-bpf (traject-from-list x-points (y-points self) (z-points self) (get-points-times self) 
                                     (type-of self) (decimals self)
                                     (sample-params self) (interpol-mode self))))
     (cons-bpf self (point-list new-bpf))
     ;(setf (traject-points self) (traject-points new-bpf))
     (x-points self)))

(defmethod (setf y-points) ((y-points t) (self 3D-trajectory))
   (let ((new-bpf (traject-from-list (x-points self) y-points (z-points self) (get-points-times self) 
                                     (type-of self) (decimals self)
                                     (sample-params self) (interpol-mode self))))
     (cons-bpf self (point-list new-bpf))
     ;(setf (traject-points self) (traject-points new-bpf))
     (y-points self)))

(defmethod (setf z-points) ((z-points t) (self 3D-trajectory))
   (let ((new-bpf (traject-from-list (x-points self) (y-points self) z-points (get-points-times self) 
                                     (type-of self) (decimals self)
                                     (sample-params self) (interpol-mode self))))
     (cons-bpf self (point-list new-bpf))
     ;(setf (traject-points self) (traject-points new-bpf))
     (z-points self)))

(defmethod change-precision ((self 3D-trajectory) decimals)
   (let ((new-bpf (traject-from-list (x-points self) (y-points self) (z-points self) (get-points-times self) 
                                     (type-of self) decimals 
                                     (sample-params self) (interpol-mode self))))
     (cons-bpf self (point-list new-bpf))
     ;(setf (traject-points self) (traject-points new-bpf))
     (setf (decimals self) decimals)))

(defmethod save-exepcion ((self 3D-trajectory)) 
  `(when (find-class ',(type-of self) nil)
     (let ((newbpf (traject-from-list ',(x-points self) ',(y-points self) ',(z-points self) ',(get-points-times self) 
                                      ',(type-of self) ,(decimals self) ',(sample-params self) ',(interpol-mode self))))
       (setf (bpfcolor newbpf) ,(om-save-color (bpfcolor self)))
       (set-name newbpf ,(get-name self))
       newbpf)))


(defmethod omNG-copy ((self 3D-trajectory))
  `(let ((traj ,(call-next-method))) 
     (setf (sample-params traj) ',(sample-params self))
     (setf (interpol-mode traj) ',(interpol-mode self))
     traj))




(defmethod traject-from-list ((listx list) (listy list) (listz list) listtimes
                              &optional (class '3D-trajectory) (decimals 0) (sample-params nil) (interpol-mode 'dist))
    (if (and (null listx) (null listy) (null listz))
        (make-instance class :point-list nil :decimals decimals
                       :sample-params sample-params :interpol-mode interpol-mode)
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
            (defz (if (= 1 (length listz)) (car listz) (- (car (last listz)) (car (last listz 2)))))
            (deft (if (consp listtimes) (if (and (= 1 (length listtimes)) (numberp (car listtimes))) (car listtimes) 
                                  (if (and (numberp (car (last listtimes))) (numberp (car (last listtimes 2))))
                                      (- (car (last listtimes)) (car (last listtimes 2))))))))
        (cons-bpf new-3Dc (loop for zpoin = (if listz (pop listz) 0) then (if listz (pop listz) zpoin) ;(+ zpoin defz))
                                for ypoin = (if listy (pop listy) 0) then (if listy (pop listy) ypoin) ;(+ ypoin defy))
                                for xpoin = (if listx (pop listx) 0) then (if listx (pop listx) (+ xpoin defx))
                                for tpoin = (if (consp listtimes) (pop listtimes) nil) then (if (consp listtimes) (pop listtimes) (if deft (+ tpoin deft) nil))
                                while (or listy listx listz)
                                collect (make-timedpoint :point (make-3dpoint :x xpoin :y ypoin :z zpoin)
                                                         :time tpoin)
                                into rep
                                finally (return (append rep (list (make-timedpoint :point (make-3dpoint :x xpoin :y ypoin :z zpoin)
                                                                                   :time (if (numberp listtimes) listtimes tpoin)))))
                                ))
        ;(setf (traject-points new-3DC) (make-traject-points (point-list new-3DC) sample-params interpol-mode))
        (setf (sample-params new-3DC) sample-params
              (interpol-mode new-3DC) interpol-mode)
              
      new-3Dc)
      (progn
        (om-beep-msg "This is a 3DC factory. Do not use lists of lists, only lists of numbers.")
        (get-initval new-3Dc))))))

(defmethod make-one-instance ((self 3D-trajectory) &rest slots-vals)
   (apply 'traject-from-list (list (nth 0 slots-vals) (nth 1 slots-vals) (nth 2 slots-vals) (nth 4 slots-vals)
                                      (type-of self) 
                                      (or (nth 3 slots-vals) 0)
                                      (or (nth 5 slots-vals) nil)
                                      (or (nth 6 slots-vals) 'points)
                                      )))

(defmethod* objFromObjs ((self 3D-trajectory) (type 3D-trajectory))
  (let ((new-3DC (clone self)))
    ;(setf (traject-points new-3DC) (traject-points self))
    new-3dc))

(defmethod* objFromObjs ((self 3DC) (type 3D-trajectory))
  (traject-from-list (x-points self) (y-points self) (z-points self) nil
                    '3D-trajectory (decimals self)))

(defmethod coerce-to-3DC (self (type (eql '3D-trajectory)))
  (let ((new-3DC (traject-from-list (x-points self) (y-points self) '(0 0) nil
                                   '3D-trajectory (decimals self))))
    (setf (bpfcolor new-3DC) (bpfcolor self))
    new-3DC))


;;;================================================

(defun get-segments (points) 
  ; return successive points sets of wich the first and last point times are known.
  (let ((rep ()))
    (push (list (copy-point (car points))) rep)
    (dolist (pt (cdr points)) 
      (push (copy-point pt) (car rep))
      (when (timedpoint-time pt)
        (setf (car rep) (reverse (car rep)))
        (push (list (copy-point pt)) rep)))
    (if (= 1 (length (car rep))) ;; last point had a time
        (setf rep (cdr rep))
      (setf (car rep) (reverse (car rep))))
    (reverse rep)))

; (setf p0 (make-timedpoint))
; (setf p1 (make-timedpoint :time 1))
; (mapcar #'(lambda (seg) (mapcar 'timedpoint-time seg)) (get-segments (list p1 p0 p1 p0 p0 p0 p0 p0 p1 p0 p0 p1 p0 p1 p1 p1 p0 p0)))


(defun fill-time-points (points speedparam)
  (if (= 1 (length points))
      (let ((pt (car points)))
        (unless (timedpoint-time pt) (setf (timedpoint-time pt) 0))
        points)
    (let ((last-t -1.0)
          (seg-list (get-segments points)))   
      ;; set first and last times if NIL
      (unless (timedpoint-time (car (car seg-list)))
        (setf (timedpoint-time (car (car seg-list))) 0.0))
      (unless (timedpoint-time (last-elem (last-elem seg-list)))
        (let ((prev (car (last seg-list 2))))
          (setf (timedpoint-time (last-elem (last-elem seg-list))) 
                (+ (timedpoint-time (car (last-elem seg-list)))
                   (- (or (timedpoint-time (last-elem prev)) 1.0) (or (timedpoint-time (car prev)) 0.0))))))
      
      (let ((replist (list (copy-point (car (car seg-list))))))
        ;; fill between segments
        (loop for seg in seg-list do
              (let ((timestamps (if (equal speedparam 'points)
                                    (calc-intermediate-values (timedpoint-time (car seg))
                                                            (timedpoint-time (last-elem seg))
                                                            (length seg))
                                (let* ((distances (loop for i from 0 to (- (length seg) 2) collect
                                                        (om-points-distance (nth i seg) (nth (1+ i) seg))))
                                       (total-length (reduce '+ distances :initial-value 0))
                                       (ratios (mapcar #'(lambda (l) (if (zerop total-length) 1 (/ l total-length))) distances)))
                                  (om+ (om* (dx->x 0 ratios)
                                            (- (timedpoint-time (last-elem seg)) (timedpoint-time (car seg)))) 
                                       (timedpoint-time (car seg)))
                                  ))))
              
              (loop for pt in (butlast (cdr seg)) 
                  for time in (butlast (cdr timestamps))
                  do
                  (let ((newpt (copy-point pt)))
                    (setf (timedpoint-time newpt) time)
                    (push newpt replist)
                    ))
            (push (copy-point (last-elem seg)) replist)
            (setf last-dur (- (timedpoint-time (car replist)) (timedpoint-time (cadr replist))))
            ))    
      (reverse replist)))))

(defun calc-intermediate-values (begin end n)
  (arithm-ser begin end (/ (- end begin) (1- (float n)))))

(defun resample-points (points srlist)
  (let ((lastsr nil))
    (append 
     (loop for i from 0 to (- (length points) 2) 
           for sr = (pop srlist) then (or (pop srlist) lastsr) 
           do (setf lastsr sr)
           append (let* ((nbpts (floor (- (timedpoint-time (nth (1+ i) points)) (timedpoint-time (nth i points))) sr))
                         (xsr (/ (- (om-point-x (nth (1+ i) points)) (om-point-x (nth i points))) (float (max 1 nbpts))))
                         (ysr (/ (- (om-point-y (nth (1+ i) points)) (om-point-y (nth i points))) (float (max 1 nbpts))))
                         (zsr (/ (- (om-point-z (nth (1+ i) points)) (om-point-z (nth i points))) (float (max 1 nbpts)))))
                    ;(print (list sr nbpts xsr ysr zsr))  
                    (if (> nbpts 0)  
                        (loop for tim in (arithm-ser (timedpoint-time (nth i points)) 
                                                 (timedpoint-time (nth (1+ i) points))
                                                 sr nbpts)
                          for x in (arithm-ser (om-point-x (nth i points)) 
                                               (om-point-x (nth (1+ i) points))
                                               xsr nbpts)
                          for y in (arithm-ser (om-point-y (nth i points)) 
                                               (om-point-y (nth (1+ i) points))
                                               ysr nbpts)
                          for z in (arithm-ser (om-point-z (nth i points)) 
                                               (om-point-z (nth (1+ i) points))
                                               zsr nbpts)
                          collect (make-timedpoint :point (make-3Dpoint :x x :y y :z z) :time tim))
                      (list (copy-point (nth i points))))
                    ))
     (list (copy-point (last-elem points))))))
  

(defun make-traject-points (timedpoints params speedparam)
  (let ((reflist (fill-time-points timedpoints speedparam)))
    (cond ((floatp params) (resample-points reflist (list params)))
          ((and (listp params) (floatp (car params)))
           (resample-points reflist params))
          ((and (listp params) (integerp (car params))) reflist)
          (t reflist))))


(defmethod! get-full-trajectory ((self 3D-trajectory) &key interpol-mode sample-params)
   :icon 502
   :menuins '((1 (("points (constant time)" points)  ("distance (constant speed)" dist))))
   :initvals '(nil points nil)
   :doc "Returns a 'full' trajectory by unfolding/sampling <self> given sample params and interpolation mode provided in <self> or using the <interpol-mode> or <sample-params> keyword arguments of this function."
   (make-instance '3D-trajectory 
                  :point-list (make-traject-points (point-list self) 
                                                   (or sample-params (sample-params self))
                                                   (or interpol-mode (interpol-mode self)))
                  :decimals (decimals self)))



