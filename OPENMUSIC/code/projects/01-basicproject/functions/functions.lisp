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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
; Contributions Mikhail Malt, Serge Lemouton

(in-package :om)

;;;==========================
;;; FUNCTION GENERATOR

(defun linear (x0 y0 x1 y1) 
  (if (= x0 x1) ;;; cas particulier.. a refaire, pour l'instant y = 1 is x = x0 et 0 sinon..
      (let* ((xx x0))
        (eval `(function
                (lambda (x) (if (= x ,xx) 1 0)))))
  (let* ((a (/ (- y1 y0) (- x1 x0)))
         (b (- y1 (* x1 a))))
    (eval `(function
            (lambda (x) (+ ,b (* x ,a))))))))

(defmethod* linear-fun ((x0 number) (y0 number) (x1 number) (y1 number)) 
  :initvals '(0 0 1 1)
  :indoc '("x0" "y0" "x1" "y1")
  :icon 236
  :doc "Constructs a linear function passing through the points (x0 y0) and (x1 y1).
The resulting function can be connected for example to SAMPLEFUN."
  (linear x0 y0 x1 y1))


;;;==========================
;;; INTERPOLATION

(defun x-around (x paires)
  "trouve les paires en dessous et au dessus de x"
  (let ((plus-grand (find x paires :test #'(lambda (x r) (<= x (first r))))))
    (if plus-grand
        (let* ((rang (if (< (1- (first (rang-p paires plus-grand 'equalp))) 0) 
                        0 
                      (1- (first (rang-p paires plus-grand 'equalp)))))
              (plus-petit (nth rang paires)))
          (list 
           (if (< rang 0) plus-grand plus-petit)
           plus-grand))
      (let ((max (car (last paires))))
        (list max max)))))

;(x-around 70 '((0 0) (41 3) (50 6) (69 5) (100 8)))
 
; changé des > / < pour >= / <= ..
(defun y-around (y paires)
  "trouve les paires en dessous et au dessus de y"
  (let ((lst '()))
    (loop 
      for i in paires
      for r in (rest paires)
      do (if (or (and (<= (second i) y) (>= (second r) y)) 
                 (and (>= (second i) y) (<= (second r) y)))
           (push (list i r) lst)))
   (reverse lst)))
;(y-around 5.5 '((0 0) (41 3) (50 6) (69 5) (100 8)))


(defun linear-interpol (x1 x2 y1 y2 x)
  "
          ^
          |
          |
        y2|..................*
          |                  .
        y0|............X     .
          |            .     . 
        y1|......*     .     . 
          |      .     .     . 
          |      .     .     . 
          |      .     .     .    
          |______._____._____.______>
                 x1    x     x2

"
  (if (= x1 x2) y1
    (+ y1
       (* (- y2 y1)
          (/ (- x x1)
             (- x2 x1))))))


(defun interpolate (list-x list-y step)
  (loop for pointer from (first list-x) by step
        with x1
        with y1 
        with x2 = (pop list-x)
        with y2 = (pop list-y)
        if (>= pointer x2)  do (setf x1 x2 y1 y2 x2 (pop list-x) y2 (pop list-y))
        if (null x2) collect y1 and do (loop-finish)
        collect (linear-interpol x1 x2 y1 y2 pointer)))

(defun interpole (list-x list-y x-min x-max nbsamples)
  (if (= 1 nbsamples) (list (x-transfer (mat-trans (list list-x list-y)) (+ x-min (/ (- x-max x-min) 2))))
  (let ((step (/ (- x-max x-min) (1- (coerce nbsamples 'double-float)))))
    (loop with x = (pop list-x) and xx = (pop list-x) 
          and y = (pop list-y) and yy = (pop list-y)
          with x-index = x-min
          with s-index = 0
          while (and xx  (< s-index nbsamples))
          if (and (>= x-index x) (<= x-index xx))
          collect (linear-interpol x xx y yy x-index)
          and do (setf x-index (+ x-min (* (incf s-index) step)))
          else do (setf x xx xx (pop list-x) y yy yy (pop list-y))))))





;;;==================================

(defun pts-distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- y2 y1) 2) (expt (- x2 x1) 2))))

(defmethod om-points-distance (p1 p2)
  (sqrt (+ (expt (- (om-point-v p2) (om-point-v p1)) 2) 
           (expt (- (om-point-h p2) (om-point-h p1)) 2))))

(defmethod om-points-distance ((p1 list) (p2 list))
  (sqrt (apply '+ (mapcar #'(lambda (coord) (expt (- (cadr coord) (car coord)) 2)) 
                          (mat-trans (list p1 p2))))))


;;==================================
;; x/y transfer tools

(defmethod! y-transfer ((self list) (y0 number) &optional (dec nil))
  :initvals '(nil 10  0)
  :indoc '("list of points, BPF or BPC"  "Y value"  "number of decimals")
  :icon 233
  :doc "Returns a list of interpolated X values corresponding to a list of points ((x1 y1) (x2 y2) ...), or a BPF/BPC (<self>) and a Y position <y0>. 

Optional <dec> is the number of decimals in the result."

  (let* ((paires self)
         (y-around (y-around y0 paires))
         (xpts (loop for i in y-around
                     collect (linear-interpol (second (first i)) ; removed OM-ROUND here
                                              (second (second i))
                                              (first (first i))
                                              (first (second i))
                                              y0))))
    (if dec (om-round xpts dec) xpts)))

(defmethod! y-transfer ((self bpf) (y0 number) &optional (dec nil))
  (y-transfer (point-pairs self) y0 dec))


(defmethod! x-transfer ((self list) (x-val number) &optional (dec nil))    
  :icon 233
  :indoc '("a list or BPF" "X value" "number of decimals")
  :initvals '(nil (0 100) nil)
  :doc "Returns the interpolated Y value(s) in a BPF or a list ((x1 y1) (x2 y2) ...) corresponding to an X value or a list of X values (<x-val>).

Optional <dec> is the number of decimals in the result."
  (let* ((paires self) 
         (bornes (x-around x-val paires))
         (ypts (linear-interpol (first (first bornes)) 
                                (first (second bornes))
                                (second (first bornes)) 
                                (second (second bornes)) 
                                x-val)))
    (if dec (om-round ypts dec) ypts)))

(defmethod! x-transfer ((self list) (x-val list) &optional (dec nil))    
  (mapcar #'(lambda (x) (x-transfer self x dec)) x-val))

(defmethod! x-transfer ((self bpf) x-val &optional (dec nil))  
   (x-transfer (point-pairs self) x-val dec))


;;;==========================
;;; GENERAL SAMPLE FUNCTION



(defmethod! om-sample ((self t) (nbs-sr number) &optional xmin xmax dec)
      :initvals '(nil 1 nil nil nil)
      :indoc '("object to resample" "number of samples (int) or sample rate (float)" "" "" "decimals")
      :icon 910
      :numouts 3
      :outdoc '("sampled object" "x-points" "y-points")
      :doc "Resamples a function, a list, a BPF or a BPC object.

Returns :
 - The result as an object (BPF or BPC) (1st output)
 - The list of x points (2nd output)
 - The list of sample values (3rd output)

If <nbs-sr> is an integer (e.g. 100) it is interpreted as the number of samples to be returned
If <nbs-sr> is an float (e.g. 0.5, 1.0...) it is interpreted as the sample rate (or step between two samples) of the function to return

<xmin> and <xmax> allow to specify the x-range to resample.
<dec> (decimals) is the precision of the result
"   
      nil)

(defmethod! om-sample ((self function) (nbs-sr number) &optional xmin xmax dec)
   :numouts 3
   (let* ((x0 (if xmin (float xmin) 0.0))
          (x1 (if xmax (float xmax) 1.0))
          (xlist (if (integerp nbs-sr)
                     (arithm-ser x0 x1 (float (/ (- x1 x0) (max 1 nbs-sr))) nbs-sr)
                   (arithm-ser x0 x1 nbs-sr)))
          (ylist (mapcar self xlist)))
     (values (simple-bpf-from-list xlist ylist 'bpf (or dec 4))
             xlist
             (if dec (om-round ylist dec) ylist)
             )
     ))


(defmethod! om-sample ((self symbol) (nbs-sr number) &optional xmin xmax dec)
   :numouts 3
   (when (fboundp self)
     (om-sample (symbol-function self) nbs-sr xmin xmax dec)))


(defmethod! om-sample ((self list) (nbs-sr number) &optional xmin xmax dec)
   :numouts 3
   (cond ((bpf-p (car self))
          (values-list (mat-trans 
                        (mapcar #'(lambda (bpf) (multiple-value-list (om-sample bpf nbs-sr xmin xmax dec))) self))))
         ((numberp (car self))
          (let* ((x0 (if xmin (round xmin) 0))
                (x1 (or (if xmax (round xmax) (1- (length self)))))
                (lst (nthcdr x0 (if xmax (first-n self (round (1+ xmax))) self)))
                (xpts (arithm-ser 0 (1- (length lst)) 1)))
            (when (and lst xpts)
              (let* ((ylist (if (integerp nbs-sr) 
                                (interpole xpts lst x0 x1 nbs-sr)
                              (interpolate xpts lst nbs-sr)))
                     (xlist (arithm-ser 0 (1- (length ylist)) 1)))
                (values (simple-bpf-from-list xlist ylist 'bpf (or dec 4))
                        xlist
                        (if dec (om-round ylist dec) ylist) 
                        )))))
         (t nil)))
   

(defmethod! om-sample ((self bpf) (nbs-sr number) &optional xmin xmax dec)
    :numouts 3
    (let* ((x0 (or xmin (first (x-points self)))) 
           (x1 (or xmax (car (last (x-points self)))))
           (nn (if (integerp nbs-sr)
                   nbs-sr
                 (+ 1 (floor (/ (- x1 x0) nbs-sr)))
                 )))
      (when (< x0 (first (x-points self)))
        (om-beep-msg "Warning - OM-SAMPLE: xmin-xmax must be within the BPF x-points range!")
        (om-print "Warning - OM-SAMPLE: xmin will be set to the lowest x value.")
        (setf x0 (first (x-points self))))
      (when (> x1 (car (last (x-points self))))
        (om-beep-msg "Warning - OM-SAMPLE: xmin-xmax must be within the BPF x-points range!")
        (om-print "Warning - OM-SAMPLE: xmax will be set to the highest x value.")
        (setf x1 (car (last (x-points self)))))
              
      (let ((ylist (interpole (x-points self) (y-points self) x0 x1 nn))
            (xlist (if (integerp nbs-sr)
                       (cond ((> nbs-sr 1)
                              (arithm-ser x0 x1 (coerce (/ (- x1 x0) (1- nbs-sr)) 'double-float) nn))
                             ((= nbs-sr 1) 
                              (list (+ x0 (/ (- x1 x0) 2.0))))
                             (t (om-beep-msg "Number of sample must be > 0 !!!")))
                     (arithm-ser x0 x1 nbs-sr))))
        (values (and xlist ylist (simple-bpf-from-list xlist ylist (type-of self) (or dec (decimals self))))
                xlist
                (if dec (om-round ylist dec) ylist))
        )))
          

(defmethod! om-sample ((self bpf-lib) (nbs-sr number) &optional xmin xmax dec)
   :numouts 3
   (values-list (mat-trans
                 (mapcar #'(lambda (bpf) (multiple-value-list (om-sample bpf nbs-sr xmin xmax dec))) (bpf-list self)))))  


(defmethod! om-sample ((self BPC) (nbs-sr number) &optional xmin xmax dec)
 :numouts 3
 (let* ((pts (point-pairs self))
        (seg-len (loop for i from 0 to (- (length pts) 2) collect
                       (pts-distance (car (nth i pts)) (cadr (nth i pts)) (car (nth (1+ i) pts)) (cadr (nth (1+ i) pts)))))
        (total-length (reduce '+ seg-len :initial-value 0))
        (ratios (mapcar #'(lambda (l) (/ l total-length)) seg-len))
        (nsamples (if (integerp nbs-sr) nbs-sr (ceiling total-length nbs-sr)))
        (npts-per-seg (mapcar #'(lambda (r) (round (* r nsamples))) ratios))
        (samples nil) (xylist nil))
        
   (if (>= nsamples (length pts))
       (setf samples (cons (car pts)
                           (loop for p1 in pts 
                                 for p2 in (cdr pts) 
                                 for np in npts-per-seg append
                                 (cond ((< np 1) nil)
                                       ((< np 2) (list p2))
                                       (t (let (x1 x2 y1 y2 vals) 
                                            (if (= (car p1) (car p2)) ;; particular case
                                                (setf x1 (cadr p1) x2 (cadr p2) y1 (car p1) y2 (car p2))
                                              (setf x1 (car p1) x2 (car p2) y1 (cadr p1) y2 (cadr p2)))
                                            (setf vals (multiple-value-list 
                                                        (om-sample (linear-fun x1 y1 x2 y2) np x1 x2)))
                                      
                                        
                                            (mat-trans (if (= (car p1) (car p2)) 
                                                           (list (third vals) (second vals))
                                                         (list (second vals) (third vals))))))))))
     (let ((segpos (dx->x 0 seg-len))
           (samplepos (arithm-ser 0 total-length (/ total-length nsamples) nsamples)))
       (setf samples (loop for sp in samplepos collect
                           (let ((po1 (position sp segpos :test '>= :from-end t))
                                 (po2 (position sp segpos :test '<=))
                                 p1 p2 pt)
                             (if po1 (setq p1 (nth po1 pts)))
                             (if po2 (setq p2 (nth po2 pts)))
                             (if (and p1 (not p2)) (setq pt (copy-list p1)))
                             (if (and p2 (not p1)) (setq pt (copy-list p2)))
                             (if (and p1 2)
                                 (setq pt 
                                       (list (linear-interpol (nth po1 segpos) (nth po2 segpos)
                                                              (car p1) (car p2) sp)
                                             (linear-interpol (nth po1 segpos) (nth po2 segpos)
                                                              (cadr p1) (cadr p2) sp))))
                             pt))))
     
       )
   
   (setq xylist (mat-trans samples))
   (values (simple-bpf-from-list (car xylist) (cadr xylist) (type-of self) (or dec (decimals self)))
           (car xylist) (cadr xylist) npts-per-seg)
   ))

;;;====================================
;;; Interpole avec profil
;;; (todo: merge with "interpolation")
(defmethod! interpole-points ((v1 t) (v2 t) (nbsteps integer) &optional profil)
  :icon 233
  :indoc '("value 1" "value 2" "number or intermediate steps" "interpolation profile")
  :doc "Interpolates <nbsteps> values between <v1> and <v2> following a profile.

<v1> and <v2> can be either numbers or list (supposed to be of the same length)
<profile> is a BPF. If nil, a linear profile is used."
  (unless profil (setf profil (make-instance 'bpf)))
  (let ((weightfun (om-scale (nth 2 (multiple-value-list (om-sample profil (+ 2 nbsteps)))) 0.0 1.0)))
    (loop for i from 0 to (+ 1 nbsteps) collect
          (om+ (om* v2 (nth i weightfun)) (om* v1 (om- 1 (nth i weightfun)))))))


;;;====================================
;;; Function reduction tools
;;; from S. Lemouton's code for Chroma

(defmethod! reduce-points ((points list) &optional (approx 0.02))
            :indoc '("a list of (x y) points or a BPF" "a number between 0.0 and 1.0")
  :initvals '(nil 0.02)
  :icon 910
  :doc "Reduces <points> by removing all points closer than [<approx> * the amplitude range of the function] to the corresponding interpolated values.

  <approx> = 1 means the maximum reduction (all intermediate points are removed)
  <approx> = 0 means reduction without loss (only exact matching points are removed)"		
  (let* ((before (list (car points)))
        (after (cdr points))
        (ymin (cadr (car before))) (ymax (cadr (car before)))
        (amplitude 0))
    (loop for p in after do 
          (if (> (cadr p) ymax) (setf ymax (cadr p))
            (if (< (cadr p) ymin) (setf ymin (cadr p)))))
    (setf amplitude (- ymax ymin))
    (if (= 0. amplitude)
        (setf before (append before (last after)))
      (loop for listrest on after
            while (cdr listrest) 
            do (let* ((x_val (caar listrest))
                      (y_val (cadar listrest))
                      (interpolated-y (linear-interpol (car (car (last before))) (car (cadr listrest)) 
                                                       (cadr (car (last before))) (cadr (cadr listrest)) 
                                                       x_val))
                      (error (/ (abs (- interpolated-y y_val)) amplitude)))
                 (if (> error approx)
                     (setf before (append before (list (car listrest))))))
            ;do (setf after (cdr after))
            finally (setf before (append before (last points)))))
    before))

(defmethod! reduce-points ((self bpf) &optional (approx 0.02))
     (let ((reduced-points (reduce-points (point-pairs self) approx)))
       (simple-bpf-from-list (mapcar 'car reduced-points)
                             (mapcar 'cadr reduced-points)
                             (type-of self) (decimals self))))

  
;(setf testfun (car (cr::make_fun '(0 0 10 1 15 2 20 3 0 4))))
;(setf testfun (make_fun '(1 0 1 4 1 10 1 20)))
;(setf testfun'(fun 0.5 0 0.5 1 0.5 2) )
;(reduce_fun testfun 0.0)

(defmethod! reduce-n-points (points n &optional (precision 10))
  :indoc '("a list of (x y) points or a BPF" "a number (int)" "a number (int)")
  :initvals '(nil 20 10)
  :icon 910
  :doc "Reduces <points> to less than <n> points using approximations with the function REDUCE-POINTS.

<precision> sets the maximum number of iterations for searching the closest result possible.
"
  (let ((borneMax t)  ; si npoints borne superieure
        (min_factor 0)
        (max_factor 1)
        (curr_factor 0)
        result)
    (loop for i from 0 to precision
          do (setf result (reduce-points points curr_factor))
          while (not (eq (length result) n))
          do  (if  (> (length result) n)
                (setf min_factor curr_factor)
                (setf max_factor curr_factor))
          (setf curr_factor (/ (+ min_factor max_factor) 2)))
    (if (and borneMax (not (eq (length result) n))) 
        (setf result (reduce-points points max_factor)))
    (when *om-verbose*
      (print (format nil "reduce ~D -> ~D (approx. ~,2F %)"
              (length points)
              (length result)
              (* 100 curr_factor))))
    result
    ))

(defmethod! reduce-n-points ((self bpf) n &optional (precision 10))
     (let ((reduced-points (reduce-n-points (point-pairs self) n precision)))
       (simple-bpf-from-list (mapcar 'car reduced-points)
                             (mapcar 'cadr reduced-points)
                             (type-of self) (decimals self))))



;;;================================
;;; INTERPOLATION
;;;================================

(defmethod! bpf-interpol ((first bpf) (second bpf) (steps number) &optional (curve 0.0) (decimals nil) (mode 'points))
            :icon 213   
            :initvals '(nil nil 1 0.0 nil 'points) 
            :indoc '("a bpf or bpc" "a bpf or bpc" "number of steps" "interpolation curve" "precision" "interpolation mode")
            :outdoc '("list of BPFs" "list of x-points" "list of y-points")
            :numouts 3
            :menuins '((5 (("points to point" 'points) ("resample curves" 'sample))))
            :doc "Interpolates between two BPFs or two BPCs (<first> and <second>).

<steps> is the number of interpolated curves wanted, including <first> and <second>.
  1 means one value between <first> and <seconds>.
  2 will return only <first> and <second>.
  3 will return <first> and <second> with one more curve in between.
  etc.

<curve> in an exponential factor for the interpolation curve (0 = linear).

<decimals> is the precision (number of decimals) in the returned curves (default NIL = precision of <first>).

<mode> determines how interpolation is done :
 - 'points is point-by-point interpolation: the curves must have the same number of points, or the biggest one will be truncated.
 - 'sample means that the curves are resampled before interpolation. In case of BPfs, x-points will be added if needed so that the interpolated curves all have the same x points. In case of BPCs, the one with fewer points is resampled, then point-by-point interpolation is applied.

Outputs
 1) list of interpolated BPFs/BPCs
 2) list of x-points
 3) list of y-points
"
            (cond ((equal mode 'points)
                   (let ((interp-x (interpolation (x-points first) (x-points second) steps curve))
                         (interp-y (interpolation (y-points first) (y-points second) steps curve)))
                     (values 
                      (loop for x1 in interp-x for y1 in interp-y
                            collect (simple-bpf-from-list x1 y1 (type-of first) (or decimals (decimals first))))
                      interp-x interp-y)))
                  ((equal mode 'sample)
                   (let* ((allxpoints (sort (x-union (copy-list (x-points first)) (copy-list (x-points second))) '<))
                          (ypts-a (x-transfer first allxpoints))
                          (ypts-b (x-transfer second allxpoints))
                          (interp-y (interpolation ypts-a ypts-b steps curve))) 
                      (values 
                       (loop for ylist in interp-y
                             collect (simple-bpf-from-list allxpoints ylist (type-of first) (or decimals (decimals first))))
                       (repeat-n allxpoints steps)
                       interp-y)
                      )))
            )


(defmethod! bpf-interpol ((first bpc) (second bpc) (steps number) &optional (curve 0.0) (decimals nil) (mode 'points))
  (cond ((equal mode 'points)
         (let ((interp-x (interpolation (x-points first) (x-points second) steps curve))
               (interp-y (interpolation (y-points first) (y-points second) steps curve)))
           (values 
            (loop for x1 in interp-x for y1 in interp-y
                  collect (simple-bpf-from-list x1 y1 (type-of first) (or decimals (decimals first))))
            interp-x interp-y)
           ))
        ((equal mode 'sample)
         (let ((l1 (length (point-list first)))
               (l2 (length (point-list second))))
           (cond ((> l1 l2) (bpf-interpol first (om-sample second l1) steps curve decimals 'points))
                 ((> l2 l1) (bpf-interpol (om-sample first l2) second steps curve decimals 'points))
                 (t (bpf-interpol first second steps curve decimals 'points)))
           ))
        ))


(defmethod! bpf-offset ((self bpf) offset)
            :icon 213   
            :initvals '(nil 0) 
            :indoc '("a bpf" "x offset")
            :outdoc '("offset BPF")
            :numouts 1
            :doc "Generates a new BPF by addif <offset> to the x-points of <self>"
            (let ((newbpf (clone self))) 
              (setf (x-points newbpf) (om+ (x-points self) offset))
              newbpf))

(defmethod! bpf-crossfade ((bpf1 bpf) (bpf2 bpf) &key xfade-profile)
            :icon 213   
            :initvals '(nil nil nil nil) 
            :indoc '("bpf" "bpf" "crossfade profile (bpf)")
            :outdoc '("merged/crossfaded BPF")
            :numouts 1
            :doc "Generates a new BPF by crossfading the overlapping interval between BPF1 and BPF2.

- <xfade-profile> determines the general crossfade profile (default = linear). 
"
            (let* ((first bpf1)
                   (second bpf2))
              (when (< (caar (point-pairs second)) (caar (point-pairs first)))
                (setf first second)
                (setf second bpf1))
              (let* ((t1 (car (car (point-pairs second))))
                     (t2 (car (last-elem (point-pairs first))))
                     (commonxpoints (band-filter (sort (x-union (copy-list (x-points first)) 
                                                                (copy-list (x-points second))) '<)
                                                 (list (list t1 t2)) 'pass))
                     (scaled-profile (if (< t1 t2)
                                       (if xfade-profile (simple-bpf-from-list 
                                                        (om-scale (x-points xfade-profile) (car commonxpoints) (last-elem commonxpoints))
                                                        (om-scale (y-points xfade-profile) 0.0 1.0)
                                                        'bpf 3)
                                       (simple-bpf-from-list 
                                        (list (car commonxpoints) (last-elem commonxpoints))
                                        '(0.0 1.0)
                                        'bpf 3)
                                       )
                                       (simple-bpf-from-list 
                                        '(0.5 0.5)
                                        '(0.0 1.0)
                                        'bpf 3)
                                       ))
                     
                     (seg1 (loop for p in (point-pairs first)
                                 while (< (car p) t1) collect p))
                     (seg2 (loop for p in (point-pairs second)
                                 when (> (car p) t2) collect p))
                     (segx (loop for xp in commonxpoints
                                 collect 
                                 (let ((y1 (x-transfer first xp))
                                       (y2 (x-transfer second xp))
                                       (itpfact (x-transfer scaled-profile xp)))
                                   (list xp (linear-interpol 0.0 1.0 y1 y2 itpfact)))))
                     )
                    
                   (simple-bpf-from-list (mapcar 'car (append seg1 segx seg2)) 
                                         (mapcar 'cadr (append seg1 segx seg2)) 
                                         (type-of bpf1) (max (decimals bpf1) (decimals bpf2)))
                   )))



(defmethod! bpf-extract ((self bpf) x1 x2) 
  :icon 233
  :indoc '("a BPF" "x1" "x2")
  :initvals '(nil nil nil)
  :doc "Extracts a segment (between <x1> and <x2>) from <self>."
  (let* ((xpts (x-points self))
         
         (x1-exact-pos (if x1 (position x1 xpts :test '=) 0))
         (x1-pos (or x1-exact-pos (position x1 xpts :test '<) (length xpts)))

         (x2-exact-pos (if x2 (position x2 xpts :test '=) (1- (length xpts))))
         (x2-pos (or x2-exact-pos (position x2 xpts :test '> :from-end t) 0))
         )
    (simple-bpf-from-list 
     (om- (append (unless x1-exact-pos (list x1))
                  (subseq xpts x1-pos (1+ x2-pos))
                  (unless x2-exact-pos (list x2)))
          (or x1 (car xpts)))
     (append (unless x1-exact-pos (list (x-transfer self x1)))
             (subseq (y-points self) x1-pos (1+ x2-pos))
             (unless x2-exact-pos (list (x-transfer self x2))))
     
     (type-of self) (decimals self))))


;(position 4.5 '(1 2 3 4 5 6) :test '> :from-end t)
;(subseq '(1 2 3 4 5 6) 0 3)


(defmethod! bpf-scale ((self bpf) &key x1 x2 y1 y2)
  :icon 233
  :indoc '("a BPF" "xmin" "xmax" "ymin" "ymax")
  :initvals '(nil nil nil nil nil)
  :doc "Rescales <self> betwenn the supplied X (<x1>,<x2>) and/or Y (<y1>,<y2>) values."
  (let* ((xp (x-points self))
         (yp (y-points self))
         (xlist (if (or x1 x2) (om-scale xp (or x1 (car xp)) (or x2 (last-elem xp))) xp))
         (ylist (if (or y1 y2) (om-scale yp (or y1 (car yp)) (or y2 (last-elem yp))) yp)))
    (simple-bpf-from-list xlist ylist (type-of self) (decimals self))))




                   

