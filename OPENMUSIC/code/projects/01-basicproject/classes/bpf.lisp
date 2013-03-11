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

(defclass object-with-pict () 
  ((pict :accessor pict :initform nil)))
 
(defclass internalbpf (named-object object-with-pict select-object)
   ((point-list :initform nil :initarg :point-list :accessor point-list)
    (bpfcolor :initform *om-black-color* :accessor bpfcolor))
   (:documentation "This is an internal class for the break point functions, 
the idea is mask certains slots which 
will not be visibles in the graphic interface #enddoc#
#seealso# (bpf bpfeditor bpfpanel) #seealso#
#point-list# This slots contains the point list of the bpf, points are represented as MCLpoints,
points with big numbers are allowed. #point-list#
#selected-p# This slot is a flag : selected-p is true if the bpf is selected else it is nil #selected-p#
#bpfcolor# This slot contains the color associated to the bpf for its visualization #bpfcolor# "))

(defmethod initialize-instance ((self internalbpf) &rest args)
  (call-next-method)
  (unless (point-list self)
    (setf (point-list self) (default-point-list self))))

(defmethod default-point-list ((self internalbpf)) (list (om-make-point 0 0) (om-make-point 100 100)))

(defclass* bpf (internalbpf)
   ((x-points :initform '(0 100) :initarg :x-points :accessor x-points :documentation "X coordinates (list)")
    (y-points :initform '(0 100) :initarg :y-points :accessor y-points :documentation "Y coordinates (list)")
    (decimals :initform 0 :initarg :decimals :accessor decimals :documentation "precision (integer) [0 - 10]"))
   (:icon 233)
   (:documentation "
BREAK-POINTS FUNCTION: a 2D function defined as y=f(x) by a list of [x,y] coordinates.

BPF objects are constructed from the list of X coordinates (<x-points>) and the list of Y coordinates (<y-points>)
<x-point> must be stricly increasing or will be sorted at initialization.
If <x-list> and <y-list> are not of the same length, the last step in the shorter one is repeated.

<decimals> allows to specify the precision of the function (0 = integers, n > 0 = number of decimals)
"))

(defmethod bpf-p ((self bpf)) t)
(defmethod bpf-p ((self t)) nil)  
  
(defmethod (setf bpfcolor) ((c t) (self bpf))
  (when c
    (setf (slot-value self 'bpfcolor) (om-correct-color c))))


(defmethod check-decimals ((self bpf))
    (unless (and (integerp (decimals self))
                 (> (decimals self) 0) 
                 (<= (decimals self) 10))
    (cond ((not (integerp (decimals self)))
           (om-beep-msg "BPF decimals must be an integer value!")
           (setf (slot-value self 'decimals) 0))
          ((minusp (decimals self))
           (om-beep-msg "BPF decimals must be a positive integer!")
           (setf (slot-value self 'decimals) 0))
           ((> (decimals self) 10)
            (om-beep-msg "BPFs only support up to 10 decimals!")
            (setf (slot-value self 'decimals) 10)))))
  
(defmethod (setf decimals) ((decimals t) (self bpf))
  (setf (slot-value self 'decimals) decimals)
  (check-decimals self)
  (decimals self))
  
(defmethod initialize-instance :after ((self bpf) &rest args)
  (check-decimals self)
  self)
              
    
    

   
;=======================================
;CONSTRUCTORS
;=======================================

;this method cons a bpf from two list

(defmethod x-points ((self bpf))
   (let ((fun (if (zerop (decimals self)) #'om-point-h #'(lambda (x) (/ (om-point-h x) (expt 10.0 (decimals self)))))))
     (mapcar fun (point-list self))))

(defmethod y-points ((self bpf))
   (let ((fun (if (zerop (decimals self)) #'om-point-v #'(lambda (x) (/ (om-point-v x) (expt 10.0 (decimals self)))))))
     (mapcar fun (point-list self))))

(defmethod! point-pairs ((self bpf)) 
 :initvals '(nil)
  :indoc '("a BPF or BPC")
  :icon 241 
  :doc "Retruns the list of points in <self> as a list ((x1 y1) (x2 y2) ...)"
  (mat-trans (list (x-points self) (y-points self))))

;;; compat 
(defmethod! paires (self) (point-pairs self))

(defmethod (setf x-points) ((x-points t) (self bpf))
   (let ((new-bpf (simple-bpf-from-list x-points (y-points self) (type-of self) (decimals self))))
     (cons-bpf self (point-list new-bpf))
     (x-points self)))

(defmethod (setf y-points) ((y-points t) (self bpf))
   (let ((new-bpf (simple-bpf-from-list (x-points self) y-points (type-of self) (decimals self))))
     (cons-bpf self (point-list new-bpf))
     (y-points self)))




(defmethod change-precision ((self bpf) decimals)
      (let ((new-bpf (simple-bpf-from-list (x-points self) (y-points self) (type-of self) decimals)))
        (cons-bpf self (point-list new-bpf))
        (setf (decimals self) decimals)))



;==================INTERFACE=============================== 

;--------------------cons a bpf from a list of points------------------
(defmethod cons-bpf ((self bpf) points)
   (setf (point-list self) (sort points '< :key 'om-point-h)))



;-------------------insert a point in the x axe-----------------------
(defmethod insert-point ((self bpf) new-point)
   (let ((bps (point-list self))
         (x1 (om-point-h new-point))
         (bps1))
     (loop while (and bps (> x1 (om-point-h (car bps)))) do
           (push (pop bps) bps1))
     (when (and bps (= (om-point-h (car bps)) (om-point-h new-point)))
       (pop bps))
     (cons-bpf self (append  (nreverse bps1)  (list new-point) bps))))

;-------------------delete a point------------------------------
(defmethod remove-point ((self bpf) point)
  (cons-bpf self (remove point (point-list self) :test 'om-points-equal-p)))   

(defmethod remove-nth-point ((self bpf) n)
  (cons-bpf self (append (subseq (point-list self) 0 n) (subseq (point-list self) (1+ n)))))

;-------------------get the x values of  prev et  next points of point------------------------------
(defmethod give-prev+next-x ((self bpf) point)
  (let ((count (position  point (point-list self) :test #'eql))
        x1 x2)
    (when count
    (if (= count 0)
      (setq x2 (second (point-list self)))
      (setq x1 (nth (1- count) (point-list self))
            x2 (nth (1+ count) (point-list self)))))
    (setq x1 (if x1 x1 nil))  
    (setq x2 (if x2  x2 nil))  
    (list x1 x2))) 

;-------------------get the points fall in the interval (x1 x2) ------------------------------
(defmethod give-points-in-x-range ((self bpf) x1 x2)
  (let ((points (point-list self))
         res-points)
    (loop while (and points (> x1 (om-point-h (car points)))) do
          (pop points))
    (loop while (and points (> x2 (om-point-h (car points)))) do
      (push (pop points) res-points))
    (nreverse res-points)))

;-------------------get the points fall in the interval (y1 y2) ------------------------------
(defmethod give-points-in-y-range ((self bpf) y1 y2)
  (let ((points (point-list self))
        res-points)
    (loop for point in points do
          (when (and (>= y2 (om-point-v point)) (<= y1 (om-point-v point)))
            (push point res-points)))
    (nreverse res-points)))

;-------------------get the min  max points in x and y axis------------------------------
(defmethod give-bpf-range ((self bpf))
  (let* ((points (point-list self))
         (rep (loop for point in points 
                    maximize (om-point-v point) into y1
                    maximize (om-point-h point) into x1
                    minimize (om-point-h point) into x
                    minimize (om-point-v point) into y
                    finally (return (list x x1 y y1))))
         (x (first rep)) (x1 (second rep)) (y (third rep)) (y1 (fourth rep)))
    (when (< (- x1 x) 10)
      (setf x (- x 5) 
            x1 (+ x1 5)))
    (when (< (- y1 y) 10)
      (setf y (- y 5) 
            y1 (+ y1 5)))
        (list x x1 y y1)))

(defmethod real-bpf-range ((self bpf))
  (loop for point in (point-list self) 
        maximize (om-point-v point) into y1
        maximize (om-point-h point) into x1
        minimize (om-point-h point) into x
        minimize (om-point-v point) into y
        finally (return (list x x1 y y1))))

;-------------------get the prev and next points for a point not in the bpf------------------------------
(defmethod give-intervall ((self bpf) point)
  (let ((points (point-list self))
        (pointx (om-point-h point)) prev next)
    (loop while (and points (>= pointx (om-point-h (car points)))) do
          (setf prev (pop points)))
    (when points (setf next (car points)))
    (list prev next)))

(defmethod get-3-points ((self bpf) point)
  (let* ((points (point-list self))
        (pointx (om-point-h point)) 
        (one (car points)) two thre)
    (loop while (and points (> pointx (om-point-h (car points)))) do
          (setf one (pop points)))
    (when one
      (if points (setf two (pop points))
          (setf two one))
      (if points (setf thre (car points))
          (setf thre two))
      (list one two thre))))
       


;-------------------move the point in x and y------------------------------
(defmethod move-bpf-in-x-y ((self bpf)  deltax deltay)
  (let ((points (point-list self))
        bps1)
     (loop for point in points do
           (push (om-make-big-point (+ (om-point-h point) deltax)
                             (+ (om-point-v point) deltay)) bps1))
     (cons-bpf self (reverse bps1))))

(defmethod move-points-in-bpf ((self bpf) points deltax deltay)
  (let ((points-old (set-difference (point-list self) points :test 'om-points-equal-p))
        (posible-move? t)
        bps1)
    (loop for point in points
          while posible-move? do
          (let ((points-adj (give-prev+next-x self point)))
            (if (and (or (null (first points-adj)) (member (first points-adj) points  :test 'equal)
                         (< (om-point-h (first points-adj)) (+ (om-point-h point) deltax)))
                     (or (null (second points-adj)) (member (second points-adj) points :test 'equal)
                         (> (om-point-h (second points-adj)) (+ (om-point-h point) deltax))))
              (push (om-make-big-point (+ (om-point-h point) deltax)
                                (+ (om-point-v point) deltay)) bps1)
              (setf posible-move? nil))))
    (when posible-move?
      (cons-bpf self (sort (concatenate 'list bps1 points-old) '<  :key 'om-point-h))
      (reverse bps1))))

     

;===============================================
;Methods for init box
;===============================================

(defmethod Class-has-editor-p ((self bpf)) t)
(defmethod get-initval ((self bpf))
  (make-instance (class-of self) :point-list (list (om-make-point 0 0)  (om-make-point 100 100))))

(defmethod default-obj-box-size ((self bpf)) (om-make-point 40 60))

(defmethod get-editor-class ((self bpf)) 'bpfEditor)


(defmethod* Objfromobjs ((Self list) (Type bpf))
  (when (bpf-p (car self))
    (Clone (car self))))


(defmethod simple-bpf-from-list ((listx list) (listy list) &optional (class 'bpf) (decimals 0))
  (if (and (null listx) (null listy))
      (make-instance class :point-list nil :decimals decimals)
  (let ((new-bpf (make-instance class :from-file t))
        (factor (expt 10 decimals))
        (listx (or listx '(0 1)))
        (listy (or listy '(0 1))))       
    (setf (decimals new-bpf) decimals)
    (setf listx (om-round (om* listx factor)))
    (setf listy (om-round (om* listy factor)))
    (if (and (list-subtypep listx 'number) (list-subtypep listy 'number))
      (let ((defx (if (= 1 (length listx)) (car listx) (- (car (last listx)) (car (last listx 2)))))
            (defy (if (= 1 (length listy)) (car listy) (- (car (last listy)) (car (last listy 2))))))
        (cons-bpf new-bpf (loop for ypoin = (if listy (pop listy) 0) then (if listy (pop listy) (+ ypoin defy))
                                for xpoin = (if listx (pop listx) 0) then (if listx (pop listx) (+ xpoin defx))
                                while (or listy listx)
                                collect (om-make-big-point xpoin ypoin) into rep
                                finally (return (append rep (list (om-make-big-point xpoin ypoin)))))
                  )
      new-bpf)
      (progn
        (om-beep-msg "This is a bpf factory. Do not use lists of lists, only lists of numbers.")
        (get-initval new-bpf))))))

(defmethod simple-bpf-from-list ((pointx number) (listy list) &optional (class 'bpf) (decimals 0))
  (let ((new-bpf (make-instance class :from-file t))
        (factor (expt 10 decimals)))
    (setf (decimals new-bpf) decimals)
    (setf pointx (round (* pointx factor)))
    (setf listy (om-round (om* listy factor)))
    (if  (list-subtypep listy 'number)
      (progn (cons-bpf new-bpf (loop for ypoin in listy
                                     for x = 0 then (+ x pointx)
                                     collect (om-make-big-point x ypoin)))
             new-bpf)
      (progn
        (om-beep-msg "Bad y point list!")
        (get-initval new-bpf)))))

(defmethod simple-bpf-from-list ((listx list) (pointy number) &optional (class 'bpf) (decimals 0))
  (let ((new-bpf (make-instance class :from-file t))    
        (factor (expt 10 decimals)))
    (setf (decimals new-bpf) decimals)
    (setf pointy (round (* pointy factor)))
    (setf listx (om-round (om* listx factor)))
    (if  (list-subtypep listx 'number)
      (progn (cons-bpf new-bpf (loop for xpoin in listx 
                                     collect (om-make-big-point xpoin pointy)))
             new-bpf)
      (progn
        (om-beep-msg "Bad x point list!")
        (get-initval new-bpf)))))

(defmethod simple-bpf-from-list ((pointx t) (pointy t) &optional (class 'bpf) (decimals 0))
  (let ((new-bpf (make-instance class :from-file t)))
    (om-beep-msg "Bad  point list!")
    (get-initval new-bpf)))

(defmethod make-one-instance ((self bpf) &rest slots-vals)
   (apply 'simple-bpf-from-list (list (first slots-vals) (second slots-vals) (type-of self) 
                                      (or (third slots-vals) 0))))


(defmethod om-make-bpf (type xpts ypts decimals)
  (simple-bpf-from-list xpts ypts type (or decimals 0)))


(defmethod execption-save-p ((self bpf)) 'bpf)
(defmethod save-exepcion ((self bpf)) 
  `(when (find-class ',(type-of self) nil)
     (let ((newbpf (simple-bpf-from-list ',(x-points self) ',(y-points self) ',(type-of self) ,(decimals self))))
       (setf (bpfcolor newbpf) ,(om-save-color (bpfcolor self)))
       (set-name newbpf ,(get-name self))
       newbpf)))


(defmethod omNG-copy ((self bpf))
  `(let ((newbpf (make-instance ',(type-of self))))
     (cons-bpf newbpf ,(omng-copy (point-list self)))
     (setf (bpfcolor newbpf) ,(om-copy-color (bpfcolor self)))
     (setf (decimals newbpf) ,(decimals self))
     (setf (name newbpf) ,(name self))
     (setf (pict newbpf) ,(omng-copy (pict self)))
     newbpf))
    
;------------------
(defclass* bpf-lib ()
  ((bpf-list :initform (list (make-instance 'bpf :point-list (list (om-make-point 0 0)  (om-make-point 100 100)))) 
             :initarg :bpf-list :accessor bpf-list :type list 
             :documentation "list of BPF objects")
   (selected-bpf :initform 0 :accessor selected-bpf))
  (:icon 213)
  (:documentation "A list of BPF objects gathered in a same object and editor.

The precision of the BPF-Lib and editor is the maximum precision (<decimals> value) of the BPFs included.
"))



(defmethod initialize-instance :after ((self bpf-lib) &key controls)
   (declare (ignore controls))
   (let ((maxdecimals (loop for item in (bpf-list self)
                            maximize (decimals item)))
         newlist)
     (setf newlist 
           (remove nil (loop for item in (bpf-list self) collect
                 (if (good-type-p item self) 
                   (let* ((factor (expt 10  (- maxdecimals (decimals item))))
                           (newbpf (make-instance (type-of item)
                                                  :decimals maxdecimals
                                                  :point-list  (if (= factor 1) (point-list item)
                                                                (loop for point in (point-list item) 
                                                                      collect (om-point-* point factor))))))
                     (setf  (bpfcolor newbpf) (bpfcolor item))
                     (setf  (name newbpf) (name item))
                     newbpf)
                   (progn
                     (om-beep-msg (format nil "Error: bpf-list slot of ~A does not accept ~A objects !" (type-of self) (type-of item)))
                     ; (abort)
                     nil
                     )))))
     (setf (bpf-list self) newlist)
     self))

(defmethod good-type-p ((self bpf) (container bpf-lib)) t)

(defmethod get-current-bpf ((self bpf-lib))
  (nth (selected-bpf self) (bpf-list self)))

(defmethod get-current-bpf ((self bpf)) self)

(defmethod decimals ((self bpf-lib)) 
  (loop for item in (bpf-list self)
        maximize (decimals item)))

(defmethod change-precision ((self bpf-lib) decimals)
  (loop for bpf in (bpf-list self) do (change-precision bpf decimals)))


(defmethod Class-has-editor-p  ((self bpf-lib)) t)

(defmethod default-obj-box-size      ((self bpf-lib)) (om-make-point 40 60))
(defmethod get-editor-class ((self bpf-lib)) 'bpfEditor)

(defmethod get-initval      ((self bpf-lib))
  (make-instance (class-of self)
    :bpf-list (list (make-instance 'bpf :point-list (list (om-make-point  0 0)  (om-make-point 100 100))))))

(defmethod* Objfromobjs ((Self bpf) (Type bpf-lib)) 
  (make-instance (class-of Type) :bpf-list (list (Clone self))))
   
(defmethod* Objfromobjs ((Self list) (Type bpf-lib))
  (when (list-subtypep self 'bpf)
    (make-instance (class-of Type) :bpf-list (loop for item in self
                                                   collect (Clone item)))))

(defmethod* Objfromobjs ((Self bpf-lib) (Type bpf)) 
  (clone (car (bpf-list self))))


;-----------------------
;BPF miniviews
;-----------------------
(defun point-to-pixel-with-sizes (ranges point sizex sizey)
  (let* ((x (om-point-h point))
         (y (om-point-v point))
         (rangex (list (first ranges) (second ranges)))
         (rangey (list (third ranges) (fourth ranges)))
         (durpointx  (abs (- (second rangex) (first rangex) )))
         (durpointy  (abs (- (second rangey) (first rangey) )))
         (x1 (round (* x sizex) durpointx))
         (y1 (round (* y sizey) durpointy))
         (offsetx (round (* (first rangex) sizex) durpointx))
         (offsety (round (* (first rangey) sizey) durpointy)))
    (om-make-point  (- x1 offsetx)  (+ (- sizey y1) offsety))))



(defmethod draw-obj-in-rect ((self bpf) x x1 y y1 edparams view)
   (om-with-focused-view view
     (om-with-fg-color view (bpfcolor self)
       (draw-bpf-in-rect self x x1 y y1 (give-bpf-range self)))))

(defmethod draw-mini-view ((self t) (value bpf))
   (draw-obj-in-rect value 3 (- (w self) 3) 3 (- (h self) 3) (give-bpf-range value) self ))


(defun get-minmax (list i winsize)
  (let* ((points (mapcar #'om-point-v (subseq list (floor (* i winsize)) (- (floor (* (+ i 1) winsize)) 1)))))
    ;(list (apply 'min points) (apply 'max points))
    (list (list-min points) (list-max points))
    ))

(defmethod min-max-draw-bpf (points w h ranges pix0 piy0)
  (let* ((winsize (/ (length points) w )))
    (loop for i from 0 to (- w 1)
       for j = pix0 then (+ j 1) do
	 (let* ((minmax (get-minmax points i winsize))
		(min (point-to-pixel-with-sizes ranges (om-make-big-point 0 (car minmax)) w h))
		(max (point-to-pixel-with-sizes ranges (om-make-big-point 0 (second minmax)) w h)))
	   (om-draw-line j (+ piy0 (om-point-v min)) j (+ piy0 (om-point-v max)))))))


(defun draw-bpf-in-rect (bpf x x1 y y1 ranges)
  (let ((pixels (- x1 x))
        (points (length (point-list bpf))))
    (if (> points (* 4 pixels))
        (min-max-draw-bpf (point-list bpf) (- x1 x) (- y1 y) ranges x y)
        (loop for point-list on (point-list bpf) do
	     (let* ((thepoint (car point-list))
		    (pix-point (point-to-pixel-with-sizes ranges thepoint (- x1 x) (- y1 y)))
		    (points-prev-next (give-prev+next-x bpf thepoint))
		    (prev-point (first points-prev-next))
		    (next-point (second points-prev-next)))
	       (when prev-point
		 (let ((prev-pixel (point-to-pixel-with-sizes ranges prev-point (- x1 x) (- y1 y))))
		   (om-draw-line (+ x (om-point-h prev-pixel)) (+ y (om-point-v prev-pixel))
				 (+ x (om-point-h pix-point)) (+ y (om-point-v pix-point)))))
	       (when (or (and next-point (null (second point-list)))
			 (and (second point-list) next-point ;; ?
			      (not (equal next-point (second point-list)))))
		 (let ((next-pixel (point-to-pixel-with-sizes ranges next-point (- x1 x) (- y1 y))))
		   ;;(print *curstream*)
		   (om-draw-line (+ x (om-point-h pix-point)) (+ y (om-point-v pix-point))
				 (+ x (om-point-h next-pixel)) (+ y (om-point-v next-pixel))))))))))



(defun get-miniview-bpf-range  (bpflist)
  (loop for item in bpflist
                    for range = (give-bpf-range item)
                    maximize (fourth range) into y1
                    maximize (second range) into x1
                    minimize (first range) into x
                    minimize (third range) into y
                    finally (return (list x x1 y y1))))

(defmethod draw-obj-in-rect ((self  bpf-lib) x x1 y y1 edparams  view)
   (let* ((bpf-list (bpf-list self))
          (ranges (get-miniview-bpf-range bpf-list)))
     (loop for bpf in bpf-list do
           (om-with-fg-color view (bpfcolor bpf)
             (draw-bpf-in-rect bpf x x1 y y1 ranges)))))


;;; edparams
(defmethod default-edition-params ((self bpf)) 
  (pairlis '(winsize winpos picture) 
           (list (or (get-win-ed-size self) (om-make-point 370 280))
                 (or (get-win-ed-pos self) (om-make-point 400 20))
                 nil)))

(defmethod corrige-edition-params ((self bpf) params)
  (let ((rep params))
    (unless (assoc 'picture params)
      (setf rep (pairlis (list 'picture) (list nil) rep)))
    rep))

(defmethod default-edition-params ((self bpf-lib)) 
  (pairlis '(winsize winpos picture) 
           (list (or (get-win-ed-size self) (om-make-point 370 280))
                 (or (get-win-ed-pos self) (om-make-point 400 20))
                 nil)))

(defmethod corrige-edition-params ((self bpf-lib) params)
  (let ((rep params))
    (unless (assoc 'picture params)
      (setf rep (pairlis (list 'picture) (list nil) rep)))
    rep))
