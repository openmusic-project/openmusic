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


;=====================================
;TOOLS
;=====================================

; T is l is not a tree but a list of ratios (numbers)
(defun ratios-tree-p (l) (list-subtypep l '(ratio number)))

;'((4 4) ((1 (1 1 1)))) = T
(defun measure-single? (mes) (= (length (cadr mes)) 1))

;'((4 4) (5)) = T
(defun measure-super-single? (mes) (and (measure-single? mes) (numberp (caadr mes))))

;'((4 4) (3 (1 (1 1 1)) -5)) = '(3 1 5)
(defun measure-repartition (mes)
   (loop for item in (cadr mes)
                    collect (floor (if (numberp item) (abs item)
                                       (abs (car item))))))



;==========================================================================
;Rulers rewriting input trees
;==========================================================================

(defun apply-tree-rulers (tree)
  (setf tree (tree-ruler1 tree))
  (setf tree (tree-ruler2 tree))
  (setf tree (tree-ruler3 tree))
  (setf tree (tree-ruler4 tree))
  (setf tree (tree-ruler5 tree))
  tree)

;================RULER1=====================
;Check the syntax of the tree and computes the value of '? if there is in the tree
(defun tree-ruler1 (tree)
  (resolve-? tree))

(defun subtree-extent (subtree) ;verifier qui l'appele
  (cond ((listp subtree) (fullratio (first subtree)))
        ((floatp subtree) (round (abs subtree)))
        ((or (ratiop subtree)(integerp subtree))  (abs subtree)) ))

(defun resolve-? (list)
  (cond 
   ((numberp list) list)
   ((or (numberp (first list)) (listp (first list)))
    (if (listp (second list)) 
        (list (first list) (mapcar #'resolve-? (second list)))
      (error (format nil "Invalid Rhythm Tree : ~A" list))))
   ((and (symbolp (first list)) (equal (symbol-name (first list)) "?"))
    (let ((solved (mapcar #'resolve-? (second list))))
      (list (reduce #'(lambda (x y) (+  (abs x) (subtree-extent y))) 
                    solved :initial-value 0)
            solved)))
   ((symbolp (first list))
    (if (listp (second list)) 
        (list (symbol->ratio (first list)) (mapcar #'resolve-? (second list)))
      (error (format nil "Invalid Rhythm Tree : ~A" list))))))

;================RULER2=====================
;Simplify
(defun tree-ruler2 (tree)
  (symplify-measure-pgcd tree))

(defun get-only-tree-d (list)
 (loop for item in list collect
       (round (if (numberp item) item (first item)))))

(defun symplify-pgcd (list)
 (let* ((numbers (get-only-tree-d list))
        (rep (abs (reduce 'pgcd  numbers))))
   (loop for item in list collect
         (if (numberp item) (/ item rep)
           (list (/ (first item) rep) (symplify-pgcd (second item)))))))

(defun symplify-measure-pgcd (tree)
  (let* ((measures (cadr tree)))
    (list (car tree)
          (mapcar #'(lambda (mes)
                      (list (car mes) (symplify-pgcd (second mes))))
                  measures))))

;================RULER3=====================
; I do not remember

(defun tree-ruler3 (tree)
  (singleton tree))

;singleton
(defun replace-num-in-tree (old new)
  (cond
   ((minusp old) (* -1 new))
   ((floatp old) (* 1.0 new))
   (t new)))

;list est toujours un S, don une list (r1  rn) ou ri est un number or a group

(defun rw-singleton (list &optional reduction)
  (cond
   ((= (length list) 1)
    (let ((elem (first list)))
      (if (numberp elem)
        (if reduction (list (replace-num-in-tree elem reduction)) list)
        (if reduction 
          (if (= (length (second elem)) 1)
            (rw-singleton (second elem) reduction)
            (list (list reduction (rw-singleton (second elem)))))
          (if (= (length (second elem)) 1)
            (rw-singleton (second elem) (car elem))
            (list (list (first elem) (rw-singleton (second elem)))))))))
   (t
    (loop for item in list append
          (if (numberp item) 
            (rw-singleton (list item))
            (if (= (length (second item)) 1)
              (list (list (first item) (rw-singleton (second item) (first item))))
              (list (list (first item) (rw-singleton (second item))))))))))
      
(defun singleton (tree)
   (let* ((measures (cadr tree)))
     (list (car tree)
           (mapcar #'(lambda (mes)  ;pour chaque measure
                       (let ((sign (first mes))
                             (slist (second mes)))
                         (cond
                          ((measure-super-single? mes) ;un mesure de la forme ((4//4 (n))) avec n n number
                           (list sign (list (replace-num-in-tree (first slist) (first sign)))))
                          ((measure-single? mes) ;un mesure de la forme ((4//4 (g))) ou g es un grupo, donc une liste
                           (let ((group (first slist))) ;grupo es un RT
                             (if (= (length (second group)) 1)
                               (list sign (rw-singleton (second group) (caar mes)))
                               (list sign (list (list (first sign)
                                                      (rw-singleton (second group))))))))
                          (t ;un mesure de la forme ((4//4 (r1 r2 ...rn))) ou r es un grupo ou un number
                           (list sign (rw-singleton (second mes)))))))
                   measures))))

;================RULER4=====================
;a verfier

(defun tree-ruler4 (tree)
  (list-first-layer tree))

(defun list-first-layer (tree)
   (let* ((measures (cadr tree)))
     (list (car tree)
           (mapcar #'(lambda (mes)
                       (if (measure-single? mes) mes 
                           (let* ((signature (car mes))
                                  (subdivs (apply '+ (measure-repartition mes)))
                                  (ratio1 (/ subdivs (car signature))))
                             (cond
                              ((and (integerp ratio1) (power-of-two-p ratio1)) mes)
                              ((and (power-of-two-p subdivs) 
                                    (or (power-of-two-p (car signature))
                                        (and (integerp ratio1) (modulo3-p (car signature))))) mes)
                              ((not (integerp (/ (car signature) subdivs))) 
                               (list signature (list (list (car signature) (cadr mes)))))
                              ((and (= (numerator ratio1) 1) (not (power-of-two-p (denominator ratio1))) mes)
                              (list signature (list (list (car signature) (cadr mes)))))
                              (t mes)))))
                   measures))))

;================RULER5=====================
;a verfier

(defun tree-ruler5 (tree)
  (add-ties-to-tree tree))

(defun add-measure-ties (tree)
  (cond ((numberp tree)
         (let ((convert (only-one-point (abs tree))))
           (if (minusp tree)
             (setf convert (om* convert -1)) )
           convert))
        ((listp tree)
         (list (list (first tree) (mapcan #'add-measure-ties (second tree)))))))

(defun add-ties-to-tree (tree)
   (let* ((measures (cadr tree)))
     (list (car tree)
           (mapcar #'(lambda (mes)
                       (list (first mes) (mapcan #'add-measure-ties (second mes))))
                   measures))))

;==================================================================
;tree --> conrtainers
;==================================================================


(defun  symbol->ratio (symbol)
  (let ((string (copy-seq (symbol-name symbol))))
  (loop for i from 0 to (1- (length string))
        when (char= (elt string i) #\/) do (setf (elt string i) '#\Space))
  (read-from-string (format () "(~A)" string)) ))

(defun normalize-tree  (tree)
  (labels 
    ((normalize (tree)
       (cond ((numberp tree)
              (let ((convert (convert-extent (abs (round tree)))))
                (cond
                 ((listp convert)
                  (cond ((plusp tree)
                         (setf (second convert) (float (second convert)))
                         (when (floatp tree) (setf (first convert) (float (first convert)))))
                        (t (setf (first convert) (- (first convert)) (second convert) (- (second convert)))))
                  convert)
                 (t (list tree)))))
             ((listp tree)
              (list (list (first tree) (mapcan #'normalize (second tree))))))))
    (first (normalize tree))))


(defun convert-extent (extent)
  (case extent
    (5 (list 4 1))
    (9 (list 8 1))
    (10 (list 8 2))
    (11 (list 8 3))
    (13 (list 12 1))
    (17 (list 16 1))
    (18 (list 16 2))
    (19 (list 16 3))
    (20 (list 16 4))
    (21 (list 15 6))
    (22 (list 16 6))
    (23 (list 16 7))
    (25 (list 24 1))
    (t  extent)))


(defun subtree-extent (subtree)
  (cond ((listp subtree) (fullratio (first subtree)))
        ((floatp subtree) (round (abs subtree)))
        ((or (ratiop subtree)(integerp subtree))  (abs subtree)) ))
        

         
(defmethod next-metric-class ((self voice)) 'measure)
(defmethod next-metric-class ((self measure)) 'group)
(defmethod next-metric-class ((self group)) 'group)
(defmethod next-metric-class ((self symbol)) (next-metric-class (make-instance self)))
(defmethod next-metric-class ((self t)) 'nil)



(defun container-from-tree (tree &key (class 'container))
  (let ((container (list-to-container (resolve-? tree) 4 :class class)))
    (set-relative-offset container)
    (integerize container)
    container))


(defmethod list-to-container ((tree list) unit &key (class 'container))
  (if (null (rest tree)) (list-to-container (first tree) unit :class 'note)
      (let ((nbunits (* (fullratio (first tree)) unit))
            (subtrees (second tree))
            (nbsubunits (reduce  
                         #'(lambda (x y) (+ (abs x) (subtree-extent y))) ;(if (numberp y) (abs y) (first y)))) 
                         (second tree) :initial-value 0)))
        (loop 
          for subtree in subtrees
          collect (list-to-container subtree (/ nbunits nbsubunits) 
                                     :class (next-metric-class class)) into inside
          finally (return (mki class :inside inside :extent nbunits))))))

(defmethod list-to-container ((tree number) unit &key class)
  (declare (ignore class))
  (cond 
   ((>= tree 0) (mki 'note :extent  (* tree unit)))
   ((< tree 0)(mki 'rest :extent  (abs (* tree unit)))))
  )

(defun tree->group (tree)
  (container-from-tree tree :class 'group))

(defun tree->measure (tree)
  (container-from-tree tree :class 'measure))

(defun tree->voice (tree)
  (container-from-tree tree :class 'voice))

(defun tree->poly (tree)
  (let ((voices (mapcar #'tree->voice (second tree))))
    (mki 'poly 
         :inside voices
         :QValue 1
         :extent (loop for voice in voices 
                       maximize (* (extent voice) (Qvalue voice))))))



; (resolve-? '(? ( (3/4 (1 1 1 )) (7/8 (1 1 1 1 1 1 1)))))
; (inspect  (QNormalize (tree->measure '(? ( (4//4 (1 1 1 1)) (4/8 (2 2)))))))


;;; ===============================================================================================
;;; CONTAINER VERS TREE.
;;; ===============================================================================================



(defmethod container->tree ((self simple-container))
  (let ((new-cont (duplique-structure-musicale self)))
    (change-qvalue new-cont (fraction-minimale new-cont))
    (container->tree2 new-cont (qvalue new-cont))
    )
  )

(defmethod container->tree ((self simple-container))
  (let ((new-cont (duplique-structure-musicale self)))
    (QNormalize new-cont)
    (container->tree2 new-cont (* (qvalue new-cont) 4))
    )
  )

(defmethod container->tree2 ((self simple-container) &optional (div 1))
  (if (and (container-p self) (not (chord-p self)))
    (let ((mgcd  (apply #'gcd (loop for item in (inside self)
                                    collect  (extent item) ))))
      (list (/ (extent self) (* 4 (qvalue self) ))
            (loop for item in (inside self) collect (container->tree2 item mgcd ))
            )
      )
    (if (rest-p self)
      (/ (* (extent self) 1) div) ;;; mettre un -1 a la place du 1 si on veut les silences negatifs
      (/ (extent self) div)
      )
    )
  )


;(container->tree (mki 'voice :tree '(? ((4//4 (1 (1 (1 -2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))))



(defun mk-signature (ratio)
  (cond
   ((= (denominator ratio) 1) (list (* ratio 4) 4))
   (t ratio)))

(defun simplify-subtrees (subtrees)
  (let ((lcm (abs (reduce #'lcm subtrees :key #'(lambda (x) (fdenominator (if (listp x) (first x) x))))))
        (gcd (abs (reduce #'gcd subtrees :key #'(lambda (x) (fnumerator (if (listp x) (first x) x)))))))
    (mapcar #'(lambda (x) (if (listp x) 
                            (list (*  (fullratio (first x)) (/ lcm gcd)) (second x))
                            (*  (fullratio x) (/ lcm gcd))))
            subtrees)))


(defmethod build-tree ((self simple-container))
  (/ (extent self) (QValue self)))

(defmethod build-tree ((self chord))
  (/ (extent self) (QValue self)))


(defmethod build-tree ((self rest))
 (- (/ (extent self) (QValue self))))

(defmethod build-tree ((self voice)) 
  (and (inside self)
       (let ((tree (call-next-method)))
         (list (/ (first tree) 4) (second tree)))))

;add by carlos
(defmethod build-tree ((self voice)) 
  (and (inside self)(let ((tree (call-next-method)))
                      (list '? (second tree)))))

(defmethod build-tree ((self measure))
  (and (inside self)
       (let ((tree (call-next-method)))
         (if (slot-value  self 'tree)
           (list (first (slot-value  self 'tree)) (simplify-subtrees (second tree)))
           (list (mk-signature (/  (first tree) 4)) (simplify-subtrees (second tree)))))))

(defmethod build-tree ((self group)) 
  (and (inside self)
       (let ((tree (call-next-method)))
         (list (first tree) (simplify-subtrees (second tree))))))

(defmethod build-tree ((self metric-sequence))
  (and (inside self)
       (list (/ (extent self) (Qvalue self))
             (mapcar 'build-tree (inside self)))))


;(setf voice (mki 'voice :tree '(? ((4//4 (1 (1 (1 -2 1 1)) 1 1)) (7//8 (1 (1 (4 2 1)) -1 1 1 1 ))))))
;(setf (tree voice) nil)
;(build-tree voice)
;(inspect voice)


;(setf m (mki 'measure :tree '(7/12 (1 (8 (12 -11 1 5.0 6 7 -8))) 1 1)))
;(setf (tree m) nil)
;(build-tree m)
;(inspect m)

(defmethod tuplet-tree ((self simple-container))
  (let ((new-cont (duplique-structure-musicale self)))
    (change-qvalue new-cont (fraction-minimale new-cont))
    (tuplet-tree2 new-cont (qvalue new-cont))
    )
  )

(defmethod tuplet-tree2 ((self simple-container) &optional (div 1) )
  (if (and (container-p self) (not (chord-p self)))
    (let ((mgcd (apply #'gcd (loop for item in (inside self) collect (extent item)))))
      (if (tuplet-p self)
        (list (/ (extent self) div ) (loop for item in (inside self)  append (tuplet-tree2 item mgcd) ))


        (mapcar #'(lambda (x) (/ x (extent self))) 
                (loop for item in (inside self)  append (tuplet-tree2 item mgcd) ) )


        )

      )
      (list (/ (extent self) div) )
      
    )
  )


