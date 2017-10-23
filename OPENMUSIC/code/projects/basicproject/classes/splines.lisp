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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;;; SPLINE CURVES - JB
;;; algo adapted from P. Bourke
;;; http://astronomy.swin.edu.au/~pbourke/curves/spline/


(in-package :om)


;; This returns the point "output" on the spline curve.
;; The parameter "v" indicates the position, it ranges from 0 to n-t+2
;; u = int*, n = int, tt = int, v = double, control = XYZ*, output = XYZ*   
(defun SplinePoint (u n tt v control)
  (let ((b 0)
        (outp (list 0 0 0)))
    (loop for k = 0 then (+ k 1)
          while (<= k n) do 
          (setf b (SplineBlend k tt u v))
          (setf (nth 0 outp) (+ (nth 0 outp) (* (nth 0 (nth k control)) b)))
          (setf (nth 1 outp) (+ (nth 1 outp) (* (nth 1 (nth k control)) b)))
          (setf (nth 2 outp) (+ (nth 2 outp) (* (nth 2 (nth k control)) b)))
          )
    outp))

(defun SplinePoint2D (u n tt v control)
  (let ((b 0)
        (outp (list 0 0)))
    (loop for k = 0 then (+ k 1)
          while (<= k n) do 
          (setf b (SplineBlend k tt u v))
          (setf (nth 0 outp) (+ (nth 0 outp) (* (nth 0 (nth k control)) b)))
          (setf (nth 1 outp) (+ (nth 1 outp) (* (nth 1 (nth k control)) b)))
          )
    outp))

;; Calculate the blending value, this is done recursively.
;; If the numerator and denominator are 0 the expression is 0.
;; If the deonimator is 0 the expression is 0
;; k = int, tt = int, u = int*, v = double
(defun SplineBlend (k tt u v)
  (let ((value 0))
    (setf value
          (if (= tt 1)
            (if (and (<= (nth k u) v) (< v (nth (+ k 1) u)))
              1 0)
          
            (if (and (= (nth (+ k tt -1) u) (nth k u)) (= (nth (+ k tt) u) (nth (+ k 1) u)))
              0 
              (if (= (nth (+ k tt -1) u) (nth k u))
                (* (/ (- (nth (+ tt k) u) v) (- (nth (+ tt k) u) (nth (+ k 1) u))) (SplineBlend (+ k 1) (- tt 1) u v))
                (if (= (nth (+ tt k) u) (nth (+ k 1) u))
                  (* (/ (- v (nth k u)) (- (nth (+ k tt -1) u) (nth k u))) (SplineBlend k (- tt 1) u v))
                  (+ (* (/ (- v (nth k u)) (- (nth (+ tt k -1) u) (nth k u))) (SplineBlend k (- tt 1) u v))  
                     (* (/ (- (nth (+ k tt) u) v) (- (nth (+ k tt) u) (nth (+ k 1) u)))  (SplineBlend (+ k 1) (- tt 1) u v)))
                  )))))
    value))


;; The positions of the subintervals of v and breakpoints, the position
;; on the curve are called knots. Breakpoints can be uniformly defined
;; by setting u[j] = j, a more useful series of breakpoints are defined
;; by the function below. This set of breakpoints localises changes to
;; the vicinity of the control point being modified.
;; u = int*, n = int, tt = int
(defun SplineKnots (n tt)
  (let ((u (make-sequence 'list (+ n tt 1)))) 
    (loop for j = 0 then (+ j 1)
          while (<= j (+ n tt)) do
          (if (< j tt)
            (setf (nth j u) 0)
            (if (<= j n)
           (setf (nth j u) (+ j (- tt) 1))
           (if (> j n)
             (setf (nth j u) (+ n (- tt) 2)))))
          )
    u))	



;; Create all the points along a spline curve
;; Control points "inp", "n" of them.
;; Knots "knots", degree "t".
;; Ouput curve "outp", "res" of them.
;; inp = XYZ*, n = int, knots = int*, tt = int, outp = XYZ*, res = int
(defun SplineCurve (inp n knots tt res)
  (let ((outp (make-sequence 'list res))
        (interval 0)
        (increment (/ (+ n (- tt) 2) (float (- res 1)))))
    (loop for i = 0 then (+ i 1)
          while (< i (- res 1)) do
          (setf (nth i outp) (SplinePoint knots n tt interval inp))
          (incf interval increment))
    (setf (nth (- res 1) outp) (nth n inp))
    outp))

(defun SplineCurve2D (inp n knots tt res)
  (let ((outp (make-sequence 'list res))
        (interval 0)
        (increment (/ (+ n (- tt) 2) (float (- res 1)))))
    (loop for i = 0 then (+ i 1)
          while (< i (- res 1)) do
          (setf (nth i outp) (SplinePoint2D knots n tt interval inp))
          (incf interval increment))
    (setf (nth (- res 1) outp) (nth n inp))
    outp))


;; Example of how to call the spline functions
;;Basically one needs to create the control points, then compute
;; the knot positions, then calculate points along the curve.

;define N 3
;XYZ inp[N+1] = {0.0,0.0,0.0,   1.0,0.0,3.0,   2.0,0.0,1.0,   4.0,0.0,4.0};
;define T 3
;int knots[N+T+1];
;define RESOLUTION 200
;XYZ outp[RESOLUTION];



;;;===================
;;; BOXES OM
(defmethod! om-spline ((self bpf) (resolution integer) (degree integer))
  :icon 234
  :initvals '(nil 100 3)
  :indoc '("a BPF or BPC" "number of points" "interpolation degree")
  :numouts 3
  :doc "Computes a B-Spline curve from the points in the BPF or BPC.

B-Splines are smoothed curves where each point is computed by polynomial interpolation from a set of control points.

Returned values :
 - The result as an object (BPF or BPC) (1st output)
 - The list of x points (2nd output)
 - The list of sample values (3rd output)

<resolution> is the number of points in the resulting curve
<degree> is the degree of the polynomial interpolation. higher degrees give smoother curves

Note that splines are supposed to be computed from BPFs with reltively few control points. "


  (let* ((points (point-pairs self))
         (N (- (length points) 1))
         (knots (SplineKnots N degree))
         (splc (SplineCurve2D points N knots degree resolution))
         (xylist (mat-trans splc))
         (fact (expt 10 (decimals self))))
    (values (make-instance (class-of self) :decimals (decimals self)
                           :point-list (loop for pt in splc collect (om-make-point (round (* fact (car pt))) (round (* fact (cadr pt))))))
            (car xylist)
            (cadr xylist)
            )))

; COMPAT
(defmethod! get-spline-points ((self bpf) (resolution integer) (degre integer))
  :icon 234
  :initvals '(nil 100 3)
  (nth 2 (multiple-value-list (om-spline self resolution degre))))

(defmethod! get-spline-obj ((self bpf) (resolution integer) (degre integer))
  :icon 234
  :initvals '(nil 100 3)
  (nth 0 (multiple-value-list (om-spline self resolution degre))))
    
    



