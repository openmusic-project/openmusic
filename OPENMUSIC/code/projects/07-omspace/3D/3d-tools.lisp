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
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
;Authors: M. Schumacher

(in-package :om)


;;;  Euclidean-distances in 2D in 3D ***

(defmethod xy-distance ((x number) (y number))
  (sqrt (+ (* x x) (* y y))))
(defmethod xy-distance ((x list) (y list))
  (mapcar 'xy-distance x y))
(defmethod xyz-distance ((x number) (y number) (z number))
  (sqrt (+ (* x x) (* y y) (* z z))))
(defmethod xyz-distance ((x list) (y list) (z list))
  (mapcar 'xyz-distance x y z))


;;;  PHASE-UNWRAP ******************* (ported from Matlab)

(defmethod! phase-unwrap ((degrees list))
    :icon 127
    :initvals '(nil)
    :indoc '("angles")
    :numouts 1
    :doc "Output is the shortest polar distance from the last value (in degrees).

Arguments can be numbers or lists."
    (let ((loopres (loop for a in (om- (mapcar #'(lambda (value) (mod value 360))
                                               (om+ (x->dx degrees) 180)) 180)
                         for b in (x->dx degrees) 
                         collect 
                         (if (and (om= a -180) (om> b 0)) 180 a)
                         )))
      (om+ degrees (dx->x 0 (om- loopres (x->dx degrees))))))


;;;  POL->CAR ************

(defmethod! pol->car ((r number) (a number))
    :icon 141
    :initvals '(0 0)
    :indoc '("radius (magnitude)" "angle (phase)")
    :numouts 2
    :doc "Conversion from polar to cartesian coordinates. 

Arguments can be numbers or lists."
	(values (* r (cos a)) (* r (sin a))))

(defmethod! pol->car ((r list) (a list))
    :numouts 2
  (let ((tmplist (mat-trans (mapcar #'(lambda (rr aa) (multiple-value-list (pol->car rr aa))) r a))))
    (values-list tmplist))) 

(defmethod! pol->car ((r list) (a number))
    :numouts 2
  (let ((tmplist (mat-trans (mapcar #'(lambda (rr) (multiple-value-list (pol->car rr a))) r))))
    (values-list tmplist)))

(defmethod! pol->car ((r number) (a list))
    :numouts 2
  (let ((tmplist (mat-trans (mapcar #'(lambda (aa) (multiple-value-list (pol->car r aa))) a))))
    (values-list tmplist)))

;;;  CAR->POL ************

(defmethod! car->pol ((x number) (y number))
    :icon 141
    :initvals '(0 0)
    :indoc '("x" "y")
    :numouts 2
    :doc "Conversion from cartesian to polar coordinates.

Arguments can be numbers or lists."
	(values (sqrt (+ (* x x) (* y y ))) (atan y x)))

(defmethod! car->pol ((x list) (y list))
    :numouts 2
  (let ((tmplist (mat-trans (mapcar #'(lambda (xx yy) (multiple-value-list (car->pol xx yy))) x y))))
    (values-list tmplist)))     

(defmethod! car->pol ((x list) (y number))
    :numouts 2
  (let ((tmplist (mat-trans (mapcar #'(lambda (xx) (multiple-value-list (car->pol xx y))) x))))
    (values-list tmplist)))     

(defmethod! car->pol ((x number) (y list))
    :numouts 2
  (let ((tmplist (mat-trans (mapcar #'(lambda (yy) (multiple-value-list (car->pol x yy))) y))))
    (values-list tmplist)))     



;;; RAD->DEG   ************


(defmethod! rad->deg ((radians number))
    :icon 141
    :indoc '("radians")
    :doc "Converts radians to degree.

Arguments can be numbers or lists."
    ;(/ (* radians 180) pi)
    (* radians 57.29577951308232))

(defmethod! rad->deg ((radians list))
   (mapcar 'rad->deg radians))

;;; DEG->RAD   ************

(defmethod! deg->rad ((degrees number))
    :icon 141
    :indoc '("degrees")
    :doc "Donverts degrees to radians.

Arguments can be numbers or lists."
    ;(/ (* degrees pi) 180)
    (* degrees 0.017453292519943295))

(defmethod! deg->rad ((degrees list))
   (mapcar 'deg->rad degrees))



;;; XY -> AD  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod! xy->ad ((x number) (y number))
            :icon 141  
            :initvals '(0 0)
            :indoc '("x" "y")
            :numouts 2
            :doc "Converts 2D cartesian coordinates [x,y] to polar coordinates [azimuth, distance]. Navigational coordinate systems (see www.spatdif.org).

Arguments can be numbers, lists or bpfs.

Returns 2 values (numbers, lists or bpfs) for 'azimuth' and 'distance'."
   (multiple-value-bind (distance azimuth) (car->pol x y) 
       (values (- 90 (rad->deg azimuth))
               distance)
       ))      

(defmethod! xy->ad ((x list) (y list))
(let ((result (mat-trans (loop 
                       for x1 in x
                       for y1 in y collect
                       (multiple-value-list (xy->ad x1 y1))))))
    (values (phase-unwrap (first result)) (second result)))) 

(defmethod! xy->ad ((x number) (y list))
   (xy->ad (make-list (length y) :initial-element x) y))

(defmethod! xy->ad ((x list) (y number))
   (xy->ad x (make-list (length x) :initial-element y)))

(defmethod! xy->ad ((x bpf) (y bpf))
            (multiple-value-bind (a d) (xy->ad (y-points x) (y-points y))
              (values (simple-bpf-from-list (x-points x) a 'bpf (decimals x))
                      (simple-bpf-from-list (x-points y) d 'bpf (decimals x))
                      ))
            )

(defmethod! xy->ad ((x bpf) (y list))
            (multiple-value-bind (a d) (xy->ad (y-points x) y)
              (values (simple-bpf-from-list (x-points x) a 'bpf (decimals x))
                      d))
            )

(defmethod! xy->ad ((x bpf) (y number))
            (multiple-value-bind (a d) (xy->ad (y-points x) y)
              (values (simple-bpf-from-list (x-points x) a 'bpf (decimals x))
                      d))
            )

(defmethod! xy->ad ((x list) (y bpf))
            (multiple-value-bind (a d) (xy->ad x (y-points y))
              (values a
                      (simple-bpf-from-list (x-points y) d 'bpf (decimals y))))
            )
            
(defmethod! xy->ad ((x number) (y bpf))
            (multiple-value-bind (a d) (xy->ad x (y-points y))
              (values a 
                      (simple-bpf-from-list (x-points y) d 'bpf (decimals y))
                      )))

(defmethod! xy->ad ((self bpc) anything)
            (multiple-value-bind (a d) (xy->ad (x-points self) (y-points self))
              (simple-bpf-from-list a d 'bpc (decimals self))))



;;; AD -> XY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmethod! ad->xy ((a number) (d number))
            :icon 141  
            :initvals '(0 0)
            :indoc '("azimuth" "distance")
            :numouts 2
            :doc "Converts 2D polar coordinates [azimuth, distance] to 2D cartesian coordinates [x,y]. Navigational coordinate systems (see www.spatdif.org).

Arguments can be numbers, lists or bpfs.

Returns 2 values (numbers, lists or bpfs) for 'x' and 'y'."

            (multiple-value-bind (x y) (pol->car d (deg->rad (- 90 a)))
              (values x y)))


(defmethod! ad->xy ((a list) (d list))
            (let ((result (mat-trans (loop 
                                      for a1 in a
                                      for d1 in d collect
                                      (multiple-value-list (ad->xy a1 d1))))))
              (values-list result))) 

(defmethod! ad->xy ((a bpf) (d bpf))
            (multiple-value-bind (x y) (ad->xy (y-points a) (y-points d))
              (values (simple-bpf-from-list (x-points a) x 'bpf (decimals a))
                      (simple-bpf-from-list (x-points d) y 'bpf (decimals a))
                      ))
            )

(defmethod! ad->xy ((a bpf) (d list))
            (multiple-value-bind (x y) (ad->xy (y-points a) d)
              (values (simple-bpf-from-list (x-points a) x 'bpf (decimals a))
                      y))
            )

(defmethod! ad->xy ((a bpf) (d number))
            (multiple-value-bind (x y) (ad->xy (y-points a) d)
              (values (simple-bpf-from-list (x-points a) x 'bpf (decimals a))
                      y))
            )

(defmethod! ad->xy ((a list) (d bpf))
            (multiple-value-bind (x y) (ad->xy a (y-points d))
              (values x
                      (simple-bpf-from-list (x-points d) y 'bpf (decimals d)))) 
            )
            
(defmethod! ad->xy ((a number) (d bpf))
            (multiple-value-bind (x y) (ad->xy a (y-points d))
              (values x
                      (simple-bpf-from-list (x-points d) y 'bpf (decimals d))
                      ))
            )

(defmethod! ad->xy ((a number) (d list))
   (ad->xy (make-list (length d) :initial-element a) d))

(defmethod! ad->xy ((a list) (d number))
   (ad->xy a (make-list (length a) :initial-element d)))

(defmethod! ad->xy ((self bpc) anything)
            (multiple-value-bind (x y) (ad->xy (x-points self) (y-points self))
              (simple-bpf-from-list x y 'bpc (decimals self)))
              )



;;; XYZ -> AED ************

(defmethod! xyz->aed ((x number) (y number) (z number))
            :icon 141  
            :initvals '(0 0 0)
            :indoc '("x" "y" "z")
            :numouts 3
            :doc "Converts 3D cartesian coordinates [x,y,z] to spherical coordinates [azimuth, elevation, distance]. Navigational coordinate systems (see www.spatdif.org). 

Arguments can be numbers or lists.

Returns 3 values (or lists) for 'azimuth', 'elevation' and 'distance'."
   (multiple-value-bind (dist ang) (car->pol x y)
     (multiple-value-bind (distz angz) (car->pol dist z)
       (values (- 90 (rad->deg ang))
               (rad->deg angz)
               distz)
       )))      

(defmethod! xyz->aed ((self 3dc) nothing anything)
            (multiple-value-bind (a e d) (xyz->aed (x-points self) (y-points self) (z-points self))
              (3dc-from-list a e d '3dc (decimals self)))
              )

(defmethod! xyz->aed ((x number) (y number) (z list))
            (xyz->aed (make-list (length z) :initial-element x) (make-list (length z) :initial-element y) z))

(defmethod! xyz->aed ((x number) (y list) (z number))
            (xyz->aed (make-list (length y) :initial-element x) y (make-list (length y) :initial-element z)))

(defmethod! xyz->aed ((x number) (y list) (z list))
            (xyz->aed (make-list (length y) :initial-element x) y z))

(defmethod! xyz->aed ((x list) (y list) (z list))
            (let ((result (mat-trans (loop 
                          for x1 in x
                       for y1 in y
                       for z1 in z collect
                       (multiple-value-list (xyz->aed x1 y1 z1))))))
  (values (phase-unwrap (first result)) (phase-unwrap (second result)) (third result))))  
    
(defmethod! xyz->aed ((x list) (y list) (z number))
            (xyz->aed x y (make-list (length x) :initial-element z)))    

(defmethod! xyz->aed ((x list) (y number) (z list))
            (xyz->aed x (make-list (length x) :initial-element y) z))

(defmethod! xyz->aed ((x list) (y number) (z number))
            (xyz->aed x (make-list (length x) :initial-element y) (make-list (length x) :initial-element z)))





;;; AED -> XYZ ************

(defmethod! aed->xyz ((a number) (e number) (d number))
            :icon 141  
            :initvals '(0 0 0)
            :indoc '("azimuth" "elevation" "distance")
            :numouts 3
            :doc "Converts 3D spherical coordinates [azimuth, elevation, distance] to cartesian coordinates [x,y,z]. Navigational coordinate systems (see www.spatdif.org).

Arguments can be numbers or lists.

Returns 3 values (or lists) for 'x', 'y' and 'z'."
     
(values  (* d (sin (deg->rad (- 90 e))) (sin (deg->rad a))) (* d (sin (deg->rad (- 90 e))) (cos (deg->rad a))) (* d (cos (deg->rad (- 90 e))))))

(defmethod! aed->xyz ((a number) (e number) (d list))
   (aed->xyz (make-list (length d) :initial-element a) (make-list (length d) :initial-element e) d)) 

(defmethod! aed->xyz ((a number) (e list) (d number))
   (aed->xyz (make-list (length e) :initial-element a) e (make-list (length a) :initial-element d))) 

(defmethod! aed->xyz ((a number) (e list) (d list))
   (aed->xyz (make-list (length e) :initial-element a) e d))

(defmethod! aed->xyz ((a list) (e list) (d list))
(let ((result (mat-trans (loop 
                       for a1 in a
                       for e1 in e
                       for d1 in d collect
                       (multiple-value-list (aed->xyz a1 e1 d1))))))
    (values-list result))) 

(defmethod! aed->xyz ((a list) (e list) (d number))
   (aed->xyz a e (make-list (length a) :initial-element d)))    

(defmethod! aed->xyz ((a list) (e number) (d list))
   (aed->xyz a (make-list (length a) :initial-element e) d))

(defmethod! aed->xyz ((a list) (e number) (d number))
   (aed->xyz a (make-list (length a) :initial-element e) (make-list (length a) :initial-element d))) 

(defmethod! aed->xyz ((self 3dc) nothing anything)
            (multiple-value-bind (x y z) (aed->xyz (x-points self) (y-points self) (z-points self))
              (3dc-from-list x y z '3dc (decimals self)))
              )


(defmethod! gen-circles (n r n-points)
   :initvals '(1 10 200)
   :numouts 2
   (ad->xy (arithm-ser 0 (round (* n 360)) (/ (* n 360) n-points) n-points) r))
    
