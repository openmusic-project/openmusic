;OpenMusic
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
;Authors: Gerard Assayag, Augusto Agon, Jean Bresson

(in-package :om)


;-------SAMPLE TOOLS COMPATIBILITY---------
; ALL Function here can be replaced by the tools in functions.lisp

(defmethod! paires ((self bpf)) :icon 128 
   (point-pairs self))


(defmethod* samplefun ((fun symbol) (step number) &optional (xmin 0) (xmax 10) (coeff 1))
  :initvals '(sqrt 1 0 10 1)
  :indoc '("function" "step" "min" "max" "coeff")
  :doc  "DEPRECATED - see OM-SAMPLE"
  :icon 236
  :numouts 2
  (samplefun (symbol-function fun) step xmin xmax coeff))

(defmethod* samplefun ((fun function) (step number) &optional (xmin 0) (xmax 7) (coeff 1000))
  (let ((rep-list (om* coeff (mapcar fun (arithm-ser xmin xmax step))))
        (thebpf (make-instance 'bpf)))
    (cons-bpf thebpf (loop for point in rep-list
                           for x from 0
                           collect (om-make-big-point x (round point))))
    (values rep-list thebpf)))

(defmethod* bpfsample ((self list) (step number))
  (mapcar #'(lambda (bpf) (bpfsample bpf step)) self))

(defmethod* bpfsample ((self bpf) (step number))
  :initvals (list (make-instance 'bpf) 10)
  :indoc '("a BPF" "a number")
  :icon 233
  :doc "DEPRECATED - see OM-SAMPLE"
  (let ((bpf  (make-instance 'bpf))
        (new-y (interpolate (x-points self) (y-points self) step)))
    (cons-bpf bpf (loop for point in new-y
                        for x from 0
                        collect (om-make-big-point x (round point))))
    bpf))

(defmethod* bpf-sample ((self bpf) xmin xmax (nbsamples integer) &optional (coeff 1) (nbdec 0))
  :initvals (list (make-instance 'bpf) nil nil 10 1 0)
  :indoc '("a BPF" "a number" "a number" "an integer" "a number" "an integer")
  :icon 233
  :doc "DEPRECATED - see OM-SAMPLE"
  (let ((min (or xmin (first (x-points self)))) 
        (max (or xmax (car (last (x-points self))))))
  (cond ((= 0 nbsamples) 0)
        ((= 1 nbsamples) min)
        (t
         (let ((interpolation (interpole (x-points self) (y-points self) min max nbsamples)))
           (unless (= coeff 1) (setf interpolation (om* interpolation coeff)))
           (om-round interpolation nbdec))))))

(defmethod! resample ((self bpf) sample-rate)
  :initvals '(nil 100)
  :indoc '("BPF" "new sample rate")
  :doc "DEPRECATED - see OM-SAMPLE"
  (nth 1 (multiple-value-list (om-sample self (float sample-rate)))))


;================================
; BPC
(defun index-points (liste)
  (loop for y in liste
        for x from 0 to (1- (length liste))
        collect (x-append x y)))

;(index-points '(a (b) (c v) d e f g))

(defun a->b%  (a b index)
  (+ (* a (- 1 index))
     (* b index)))


(defun bpc-sample_0 (bpc nb-samples)
  (let* ((paires (point-pairs bpc))
         (len-paires (length paires))
         (paires+index (index-points paires)))
    (mapcar 'rest
            (flat 
             (loop for index from 0 to (1- len-paires) by (/ (1- len-paires) nb-samples)
                   collect (sort-list (list 
                                   (x-append index
                                             (a->b% (second (flat (x-around index paires+index ))) 
                                                    (fifth (flat (x-around index paires+index ))) 
                                                    (- index (first (flat (x-around index paires+index )))))
                                             (a->b% (third (flat (x-around index paires+index ))) 
                                                    (sixth (flat (x-around index paires+index ))) 
                                                    (- index (first (flat (x-around index paires+index ))))) 
                                             ))
                                  :test '<
                                  :key 'first)) 
             1))))

(defmethod! bpc-sample ((self bpc)  (nbsamples integer) &optional (nbdec 0))
  :initvals (list (make-instance 'bpc)  10  0)
  :indoc '("a BPC"  "an integer"  "an integer")
  :icon 233
  :doc "DEPRECATED - see OM-SAMPLE"
  (om-round (bpc-sample_0 self nbsamples) nbdec))


;=======================================
;;; DEPRECATED -- see linear-interpol
(defun interpol-segment (x x1 x2 y1 y2)
  (linear-interpol x1 x2 y1 y2 x))

;; equivalent to linear-interpol
(defun linear-interpoly (y1 y2 x1 x2 y0)
  (if (= y1 y2) x2 (linear-interpol y1 y2 x1 x2 y0)))


;===========================================================================================
;by Karim Haddad

(defun f-transfer (bpf x0)            
  (let* ((paires (point-pairs bpf)) 
         (bornes  (x-around x0 paires)))
    (linear-interpol (caar bornes) 
                     (first (second bornes)) 
                     (second (first bornes)) 
                     (second (second bornes)) 
                     x0)))

(defun f-transfer-l (bpf list) 
  (let ((list (om::list! list)))
    (mapcar #'(lambda (k) (f-transfer bpf k)) list)))

(defmethod! transfer ((self bpf) (x-val t))
  :doc  "DEPRECATED -- see X-TRANSFER"
  (if (listp x-val) (f-transfer-l self x-val) (f-transfer self x-val)))

;------------------------------------------------------------------------
; by Hilbert Nono

(defmethod! trouve_index ((liste list) (coeff number) (index integer))
  (cond
   ((null liste) nil)
   ((> (car liste) coeff) index)
   (t (trouve_index (cdr liste) coeff (1+ index)))
   ))

(defmethod! table_xy ((liste_x list) (liste_y list) (alpha number))
  :indoc '("Liste des x" "Liste des y" "valeur a interpoler")
  :doc  "DEPRECATED -- see X-TRANSFER"
  ; (defun table_xy ( liste_x liste_y alpha)
  (let ((i) (xa) (xb) (ya) (yb))
    ; recherche de l'index i tel que x(i-1)< alpha < x(i)
    (setf i (trouve_index liste_x alpha 0))
    (cond 
     ; si i non trouvee , on donne la derniere valeur de la liste y
     ((null i) (last-elem liste_y))
     ; si alpha < min(liste_x) on donne la premiere valeur de liste_y
     ((= i 0) (car liste_y))
     ; sinon on fait l'interpolation lineaire
     (t 
      (setf xa (nth (1- i) liste_x))
      (setf xb (nth i liste_x))
      (setf ya (nth (1- i) liste_y))
      (setf yb (nth i liste_y))
      (/ (+ (* alpha (- yb ya)) (- (* ya xb) (* yb xa)))  (- xb xa))
      ))))

(defmethod! bpf-get-val ( (self bpf) (val null))
  :indoc '("une bpf" "une valeur ou bien une liste de valeurs a interpoler par la bpf")
  :doc  "DEPRECATED -- see X-TRANSFER"
  nil)

(defmethod! bpf-get-val ( (self bpf) (val number))
  (table_xy (x-points self) (y-points self) val))

(defmethod! bpf-get-val ((self bpf) (val list))
  (cons (table_xy (x-points self) (y-points self) (car val)) (bpf-get-val self (cdr val))))
 