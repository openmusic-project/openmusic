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

(defun round_0 (arg &optional div)
  (if (plusp arg)
  (truncate (+ (/ arg div) 0.5))
  (truncate (- (/ arg div) 0.5))))

(defun approx-decimals (x y nbdec)
  (let ((ndec 
         (if (> nbdec 0 ) (float (expt 10 nbdec)) (expt 10 nbdec))))
    (/ (round_0 (/ x y) (/ ndec)) ndec)))

(defun average (xs weights?)
  (let ((num 0) (den 0) ampl)
    (while xs
      (setq ampl (or (if (consp weights?) (nextl weights?) 1) 1))
      (incf num (* ampl (nextl xs)))
      (incf den ampl) )
    (/ num den) ))

(defun mulalea (n percent)
  (* n (+ 1  (om-random  (- percent) (float percent)) )))


(defmethod  tree-min ((self list) &optional (min MOST-POSITIVE-LONG-FLOAT))
  (if (null self)
    min
    (tree-min (rest self) (tree-min (first self) min))))

(defmethod  tree-min ((self number) &optional (min MOST-POSITIVE-LONG-FLOAT))
  (min self min))

(defmethod  tree-max ((self list) &optional (max MOST-NEGATIVE-LONG-FLOAT))
  (if (null self)
    max
    (tree-max (rest self) (tree-max (first self) max))))

(defmethod  tree-max ((self number) &optional (max MOST-NEGATIVE-LONG-FLOAT))
  (max self max))



;------------ADD-----------------
(defmethod* om+ ((arg1 number) (arg2 number))  
  :initvals '(0 0) :indoc '("number or list" "number or list")
  :doc "Sum of two numbers or lists.

Ex. (om+ 2 3)  => 5
Ex. (om+ 2 '(3 4))  => (5 6)
Ex. (om+ '(1 2) '(3 4))  => (4 6)
" 
  :icon 193 
  (+ arg1 arg2))


(defmethod* om+ ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
              (om+ arg1 input)) arg2))

(defmethod* om+ ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om+  input arg2)) arg1))

(defmethod* om+ ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input1 input2)
              (om+ input1 input2)) arg1 arg2))




;------------TIMES-----------------

(defmethod* om* ((arg1 number) (arg2 number))  
  :initvals '(0 0) :indoc '("number or list" "number or list") :icon 195
  :doc "Product of two numbers or lists.

Ex. (om* 2 3)  => 6
Ex. (om* 2 '(3 4))  => (6 8)
Ex. (om* '(1 2) '(3 4))  => (3 8)
"
  (* arg1 arg2))

(defmethod* om* ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
              (om* arg1 input)) arg2))

(defmethod* om* ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om*  input arg2)) arg1))

(defmethod* om* ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input1 input2)
              (om* input1 input2)) arg1 arg2))


;------------MINUS-----------------

(defmethod* om- ((arg1 number) (arg2 number))  
   :initvals '(0 0) :indoc '("number or list" "number or list") :icon 194
  :doc "Difference of two numbers or lists.

Ex. (om- 5 2)  => 3
Ex. (om- 5 '(2 4))  => (3 1)
Ex. (om- '(5 4) '2)  => (3 2)
Ex. (om- '(5 4) '(2 4))  => (3 0)
"
  (- arg1 arg2))

(defmethod* om- ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
              (om- arg1 input)) arg2))

(defmethod* om- ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om- input arg2)) arg1))

(defmethod* om- ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input input2)
              (om- input input2)) arg1 arg2))


;------------------------------------------------------------------------

(defmethod* om/ ((arg1 number) (arg2 number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") :icon 196
  :doc "Division of two  numbers or lists.

Ex. (om/ 8 2)  => 4
Ex. (om/ 8 '(2 4))  => (4 2)
Ex. (om/ '(8 5) '(2 4))  => (4 5/4)"
  (/ arg1 arg2))

(defmethod* om/ ((arg1 number) (arg2 list))  
  (mapcar #'(lambda (input)
                 (om/ arg1 input)) arg2))

(defmethod* om/ ((arg1 list) (arg2 number))   
  (mapcar #'(lambda (input)
              (om/  input arg2)) arg1))

(defmethod* om/ ((arg1 list) (arg2 list))
  (mapcar #'(lambda (input input2)
              (om/  input input2)) arg1 arg2))

;------------------------------------------------------------------------

(defmethod* om^ ((a number) (b number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") :icon 197
  :doc "Exponentiation of base a and exponent b.

Corresponds to the multiplication of <a> repeated <b> times.
Can be used on numbers or lists.

Ex. (om^ 2 3)  => 8
Ex. (om^ 2 '(3 4 5))  => (8 16 32)
Ex. (om^ '(2 3 4) 3)  => (8 27 64)
Ex. (om^ '(2 3 4) '(2 3 4))  => (4 27 256)
"
  (expt a b))

(defmethod* om^ ((a number) (b list))  
  (mapcar #'(lambda (input)
                 (om^ a input)) b))

(defmethod* om^ ((a list) (b number))   
  (mapcar #'(lambda (input)
              (om^ input b)) a))

(defmethod* om^ ((a list) (b list))
  (mapcar #'(lambda (input1 input2)
              (om^  input1 input2)) a b))

;------------------------------------------------------------------------

(defmethod* om-e ((n number)) 
  :initvals '(1) :indoc '("number or list") :icon 198
  :doc "Exponential function.

This function can be applied on numbers or lists.

Ex. (om-e 4)  => 54.59815
Ex. (om-e '(3 4))  => (20.085537 54.59815)
"
  (exp n))


(defmethod* om-e ((n list))   
  (mapcar #'(lambda (input)
              (om-e input)) n))

;------------------------------------------------------------------------

(defmethod* om-log ((n number) &optional (base (exp 1)))
  :initvals (list 1 (exp 1)) :indoc '("a number or list" "a number") :icon 199
  :doc "Logarithm function. 
(the logarithm of a number to the base is the power to which the base must be raised in order to produce the number)

The <base> argument is optional. By default, <base> is equal to the number 'e', so om-log computes the 'natural' logarithm of <n>.

This function can be applied to numbers or lists.

Ex. (om-log 3)  => 1.0986123
Ex. (om-log 3 10)  => 0.47712126
Ex. (om-log '(3 4))  => (1.0986123 1.3862944)
"
  (log n base))


(defmethod* om-log ((n list)  &optional (base (exp 1)))
  (mapcar #'(lambda (input)
              (om-log input base)) n))

;------------------------------------------------------------------------

(defmethod* om-round ((n number) &optional (decimals 0) (divisor 1))
  :initvals '(1 0 1) :indoc '("number or list" "number of decimals" "divisor") :icon 209
  :doc "Rounds a number or a list of numbers with a given number of decimals (default = 0, i.e. returns integer values) and a divisor.

This function can be applied to numbers or lists.

Ex. (om-round 4.3)  => 4
Ex. (om-round '(4.3 5.0 6.8))  => (4 5 7)
Ex. (om-round '(4.308 5.167 6.809) 2)  => (4.31 5.17 6.81)
Ex. (om-round '(4.308 5.167 6.809) 0 2)  => (2 3 3)
Ex. (om-round '(4.308 5.167 6.809) 1 2)  => (2.2 2.6 3.4)
"
  (approx-decimals n divisor decimals))


(defmethod* om-round ((n list) &optional (decimals 0) (divisor 1))
  (mapcar #'(lambda (input)
              (om-round input decimals divisor)) n))

;------------------------------------------------------------------------

(defmethod* clip ((self number) min max)
  :initvals (list nil nil nil) 
  :indoc '("number or list" "minimum value or list of values" "maximum value or list of values") 
  :icon 209
  :doc "Clips numeric values to a range defined by either <min> or <max> or both.

This function can be applied to numbers or lists.

If self is below min, return min,
if self is above max, return max,
otherwise return self
"
  (let ((result
         (cond 
          ((and min (< self min)) min)
          ((and max (> self max)) max)
          (t self))))
    result))
    
(defmethod* clip ((self list) min max)
            (cond
             ((and (consp min) (consp max))
              (mapcar #'(lambda (input mins maxs)
                          (om-clip input mins maxs)) self min max))
             ((consp min)
              (mapcar #'(lambda (input mins)
                          (om-clip input mins max)) self min))
             ((consp max)
              (mapcar #'(lambda (input maxs)
                          (om-clip input min maxs)) self max))
             (t (mapcar #'(lambda (input)
                        (om-clip input min max)) self))
            ))

;------------------------------------------------------------------------

(defmethod mat-trans-with-nil ((matrix list))
 (let ((maxl (1- (loop for elt in matrix maximize (length elt))))
        result)
    (loop for i from 0 to maxl do
         (push (mapcar #'(lambda (list) (nth i list)) matrix) result))
    (nreverse result)))


(defmethod* om//  ((n number) (divisor number))
  :initvals '(1 1) :indoc '("number or list"  "number or list") :icon 209
  :numouts 2
  :doc "Euclidean division of <n> and <divisor>. 
Yields an integer result and the rest of the division. 
When the divisor is 1, the operation is known as 'floor'.

<n> and <divisor> can be numbers or lists.

Ex. (om// 5.5 2)  =>  2 , 1.5
Ex. (om// 5.5 1)  =>  5 , 0.5
Ex. (om// '(5.5 6) 2)  =>  (2 3) , (1.5 0)
Ex. (om// 5.5 '(2 3))  =>  (2 1) , (1.5 2.5)
Ex. (om// '(5.5 6) '(2 3))  =>  (2 2) , (1.5 0)
"
  (floor n divisor))


(defmethod* om// ((n number) (divisor list))  
  (values-list 
   (mat-trans-with-nil (mapcar #'(lambda (input)
                          (multiple-value-list (om// n input)))
                      divisor))))

(defmethod* om// ((n list) (divisor number))
  (if (null n)
    (values nil nil)
    (values-list 
     (mat-trans-with-nil (mapcar #'(lambda (input)
                                    (multiple-value-list (om// input divisor)))
                                n)))))

(defmethod* om// ((n list) (divisor list))  
  (values-list
   (mat-trans-with-nil 
    (mapcar #'(lambda (input1 input2)
                (multiple-value-list (om// input1 input2)))
            n divisor))))



;------------------------------------------------------------------------

(defmethod* om-abs ((self number))
  :initvals (list 1 ) :indoc '("number or tree" ) :icon 209
  :doc "Absolute value.

This function can be applied to numbers or lists.

Ex. (om-abs 3)  => 3
Ex. (om-abs -3)  => 3
Ex. (om-abs '(3 -4 -1.5 6))  => (3 4 1.5 6)
"
  (abs  self ))


(defmethod* om-abs ((self list))
  (mapcar #'(lambda (input)
              (om-abs input)) self))


;------------------------------------------------------------------------

(defmethod* om-min ((a number) (b number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") :icon 209
  :doc "Minimum of two numbers.

This function can be applied to numbers or lists.

Ex. (om-min 3 4)  => 3
Ex. (om-min 3 '(1 2 3 4))  => (1 2 3 3)
Ex. (om-min '(4 3 2 1) '(1 2 3 4))  => (1 2 2 1)
"
  (min a b))

(defmethod* om-min ((a number) (b list))  
  (mapcar #'(lambda (input)
                 (om-min a input)) b))

(defmethod* om-min ((a list) (b number))   
  (mapcar #'(lambda (input)
              (om-min input b)) a))

(defmethod* om-min ((a list) (b list))
  (mapcar #'(lambda (input1 input2)
              (om-min input1 input2)) a b))



(defmethod* list-min ((self list))
  :initvals '((0 1 2)) :indoc '("a list") :icon 209
  :doc "Returns the minimum element in a list.

Ex. (list-min '(2 3 1 4))  => 1"
  (and (remove nil self) (list-min2 (remove nil self) MOST-POSITIVE-LONG-FLOAT)))


(defmethod* list-min ((self t))
  self)


(defun list-min2 (l minimum)
  (if (null l)
    minimum
    (if (atom (first l))
      (list-min2 (rest l) (min minimum (first l)))
      (list-min2 (first l) (min minimum (list-min2 (rest l) minimum))))))

;------------------------------------------------------------------------

(defmethod* om-max ((a number) (b number)) 
  :initvals '(1 1) :indoc '("number or list" "number or list") :icon 209
  :doc "Maximum of two numbers.

This function can be applied to numbers or lists.

Ex. (om-max 3 4)  => 4
Ex. (om-max 3 '(1 2 3 4))  => (3 3 3 4)
Ex. (om-max '(4 3 2 1) '(1 2 3 4))  => (4 3 3 4)"
  (max a b))

(defmethod* om-max ((a number) (b list))  
  (mapcar #'(lambda (input)
                 (om-max a input)) b))

(defmethod* om-max ((a list) (b number))   
  (mapcar #'(lambda (input)
              (om-max input b)) a))

(defmethod* om-max ((a list) (b list))
  (mapcar #'(lambda (input1 input2)
              (om-max input1 input2)) a b))

(defmethod* list-max ((self list) )
  :initvals '((0 1 2)) :indoc '("a list") :icon 209
  :doc "Returns the maximum element in a list.

Ex. (list-max '(2 4 1 3))  => 4"
  (and (remove nil self) (list-max2 (remove nil self) MOST-NEGATIVE-LONG-FLOAT)))

(defmethod* list-max ((self t)) self)


(defun list-max2 (l minimum)
  (if (null l)
    minimum
    (if (atom (first l))
      (list-max2 (rest l) (max minimum (first l)))
      (list-max2 (first l) (max minimum (list-max2 (rest l) minimum))))))



;------------------------------------------------------------------------

(defmethod* om-mean ((self list) &optional (weights 1))
  :initvals (list '(1) 1) :indoc '("list of numbers" "list of numbers") :icon 209
  :doc "Arithmetic mean of numbers in a list. 

The optional input <weights> is a list of weights used to ponderate the successive elements in the list.

Ex. (om-mean '(1 2 3 4))  => 2.5
Ex. (om-mean '(1 2 3 4) '(3 2 1 1))  => 2.0
"
  (average self weights))


;------------RANDOM-----------------

;;; COMPATIBILITY
(defmethod* aleanum ((high number) (low number)) 
  :initvals '(0 0) :indoc '("min" "max") :icon 200 
  (om-random high low))


(defmethod* om-random ((low number) (high number))
  :initvals '(0 1) :indoc '("min" "max") :icon 200 
  :doc "Returns a random number between <low> and <high>."
  (if (zerop (- low high))
    (+ low high (- high))
    (let ((low-min (min low high)))
      (if (or (floatp  low) (floatp high))
        (+ (om-random-value (- (max low high) low-min)) low-min)
        (+ (om-random-value (- (1+ (max low high)) low-min)) low-min)))))

;------------------------------------------------------------------------


(defmethod* perturbation ((self number) (percent number))
  :initvals '(1 0) :indoc '("number or list"  "number or list") :icon 200
  :doc "Applies to <self> a random deviation bounded by the <percent> parameter, a value in [0 1]. 
<self> and <percent> can be numbers or lists."
  (mulalea self percent))


(defmethod* perturbation ((self number) (num list)) 
  (mapcar #'(lambda (input)
                 (perturbation self input)) num))

(defmethod* perturbation ((self list) (num number))   
  (mapcar #'(lambda (input)
              (perturbation  input num)) self))

(defmethod* perturbation ((self list) (num list))
  (mapcar #'(lambda (input1 input2)
              (perturbation input1 input2)) self num))


;------------------------------------------------------------------------

(defmethod* om-scale ((self number) (minout number) (maxout number) &optional (minin 0) (maxin 0))
  :initvals '(1 0 0 0 0) :indoc '("number or list"  "a number" "a number" ) :icon 209
  :doc 
"Scales <self> (a number or list of numbers) considered to be in the interval [<minin> <maxin>] towards the interval [<minout> <maxout>].

If [<minin> <maxin>] not specified or equal to [0 0], it is bound to the min and the max of the list.

Ex. (om-scale 5 0 100 0 10)  => 50
Ex. (om-scale '(0 2 5) 0 100 0 10)  => (0 20 50)
Ex. (om-scale '(0 2 5) 0 100)  => (0 40 100)
 "
  (if (= maxin minin)
    minin
    (+ minout (/ (* (- self minin) (- maxout minout)) (- maxin minin)))))


(defmethod* om-scale ((self list) (minout number) (maxout number) &optional (minin 0) (maxin 0))
    (let ((min minin) (max maxin))
      (when (= minin maxin) 
        (setf min (list-min self) max (list-max self))) 
      (when (= min max)
        (setf min 0 max (abs max)))
      (mapcar #'(lambda (item) (om-scale item minout maxout min max)) self)))

(defmethod* om-scale ((self null) (minout number) (maxout number) &optional (minin 0) (maxin 0)) nil)

;------------------------------------------------------------------------
;;this exponent might be added as optional to the current om-scale function

(defmethod! om-scale^ ((self t) (minout number) (maxout number) (exponent number) &optional (minin 0) (maxin 0))
  :initvals '(1 0 1 1) 
  :indoc '("number or list"  "a number" "a number" "an exponent")
  :icon '(209)
  :doc 
"Scales <self> (a number or list of numbers) considered to be in the interval [<minin> <maxin>] towards the interval [<minout> <maxout>].

If [<minin> <maxin>] not specified or equal to [0 0], it is bound to the min and the max of the list.
Non-linear scaling is possible by specifying a value for <exponent>: 
For values >1 scaling is exponential, for values <1 scaling is logarithmic.

Ex. (om-scale^ '(0 2 5) 0 100 1.0 0 10)  => (0 20 50)
Ex. (om-scale^ '(0 2 5) 0 100 2.0 0 10)  => (0 4 25)
Ex. (om-scale^ '(0 2 5) 0 100 0.5 0 10)  => (0 44.72 70.71)
 "
  (om-scale (om^ (om-scale self 0. 1. minin maxin) exponent) minout maxout 0. 1.)
  )

;------------------------------------------------------------------------
;;
;;this comes directly from PW. makes some functions generic
;;
(defmethod less-tree-mapcar ((fun function) (arg1 number) (arg2 number) &optional deep)
  (funcall fun (list arg1)
           (if deep arg2 (list arg2))))
 
(defmethod less-tree-mapcar ((fun function) (arg1 cons) (arg2 number) &optional deep)
  (if (consp (first arg1))
    (cons (less-tree-mapcar fun (car arg1) arg2 deep)
          (less-tree-mapcar fun  (cdr arg1) arg2 deep))
    (funcall fun arg1 (if deep arg2 (list arg2)))))

(defmethod less-tree-mapcar ((fun function) (arg1 null) arg2 &optional deep)
  (declare (ignore arg1 arg2 deep)) nil)

(defmethod less-tree-mapcar ((fun function) (arg1 number) (arg2 cons) &optional deep)
  (if (consp (first arg2))
    (cons (less-tree-mapcar fun arg1 (car arg2) deep)
          (less-tree-mapcar fun  arg1 (cdr arg2) deep))
    (funcall fun (list arg1) (car arg2))))

(defmethod less-tree-mapcar ((fun function) arg1 (arg2 null) &optional deep)
  (declare (ignore arg1 arg2 deep)) nil)

(defmethod less-tree-mapcar ((fun function) (arg1 cons) (arg2 cons) &optional deep)
  (if (or deep (consp (first arg1)) (consp (first arg2)))
    (cons (less-tree-mapcar fun (car arg1) (car arg2) deep)
          (less-tree-mapcar fun  (cdr arg1) (cdr arg2) deep))
    (funcall fun arg1 arg2)))

(defun g-scaling/sum (list sum)
  "scales <list> (may be tree) so that its sum becomes <sum>. Trees must be
well-formed. The children of a node must be either all leaves or all
nonleaves. "
  (less-tree-mapcar #'(lambda (x y) (om* x (/ y (apply #'+ x)))) list sum t))

(defmethod* om-scale/sum ((self list) (sum number))
  :initvals (list '(1 2 3) 10) :indoc '("number or list"  "number" ) :icon 209
  :doc "Scales <self> so that the sum of its elements become <sum>

Ex. (om-scale/sum '(2 3 5) 30)  => (6 9 15) "
  (g-scaling/sum self sum))

(defmethod* om-scale/sum ((self number) (sum number) )
  (g-scaling/sum self sum))


;------------------------------------------------------------------------


(defmethod* factorize ((number number)) 
  :initvals '(100)
  :indoc '("an integer")
  :doc "Returns the prime decomposition of <number> in the form ((prime1 exponent1) (prime2 exponent2) ...)
Primes known to the system are the 1230 primes ranging from 1 to 9973.

Ex. (factorize 100) => ((2 2) (5 2))     [100 = 2^2 * 5^2]"
  :icon 209
  (prime-facts number))


;------------------------------------------------------------------------
(defmethod* reduce-tree ((self t) (function symbol) &optional (accum nil))
  :initvals (list '(10 10) '+ nil)
  :icon 209
  :indoc '("a tree (list)" "a function or a patch" "a neutral value for <function>")
  :doc "Applies the commutative binary <function> recursively throughout the list <self>.
(Applies to the first elements, then the result to the next one, and so forth until the list is exhausted.)

Function '+, for instance, makes reduce-tree computing the sum of all elements in the list.

Optional <accum> should be the neutral element for the <function> considered (i.e. initial result value).
If <accum> is nil, figures out what the neutral can be (works for +,*,min,max)."
  (reduce-tree self (symbol-function function) accum))


(defmethod* reduce-tree ((self list) (function function) &optional (accum nil))
  (unless accum (setf accum (neutral-element function)))
  (if (null self)
    accum
    (reduce-tree (rest self) function (reduce-tree (first self) function accum))))

(defmethod*  reduce-tree ((self number) (function function) &optional (accum nil))
  (funcall function self accum))


(defun neutral-element (function)
  (case (function-name function)
    (+ 0)
    (* 1)
    (min MOST-POSITIVE-LONG-FLOAT)
    (max MOST-NEGATIVE-LONG-FLOAT)
    (t 0)))



;; curve exp => 0 = lineaire
(defun number-interpolation (n1 n2 n curve)
  (+ n1 (* (- n2 n1) (expt n (exp curve)))))

(defun number-interpole-values (begin end samples curve)
  (if (<= samples 1)
      (list (number-interpolation begin end 0.5 curve))
    (let ((step (/ 1 (1- samples))))
      (loop for j from 0 to (1- samples) collect
            (number-interpolation begin end (* j step) (- curve))))))

(defmethod* interpolation ((begin number) (end number) (samples integer) (curve number))
  :initvals '(0 1 5 0.0)
  :icon 209
  :indoc '("number or list" "number or list" "integer" "number")
  :doc "Interpolates 2 numbers or lists (from <begin> to <end>) through <samples> steps.

<curve> is an exponential factor interpolation (0 = linear).
" 
  (number-interpole-values begin end samples curve))

(defmethod* interpolation ((begin number) (end list) (samples integer) (curve number))
  (mat-trans (mapcar #'(lambda (item)  (interpolation  begin item samples curve))
          end)))
          
(defmethod* interpolation ((begin list) (end number) (samples integer) (curve number))
  (mat-trans (mapcar #'(lambda (item) (interpolation item end samples  curve))
          begin)))
          
(defmethod* interpolation ((begin list) (end list) (samples integer) (curve number))
  (mat-trans (mapcar #'(lambda (item1 item2) (interpolation item1 item2 samples curve))
          begin end)))
          
;(defmethod! om::interpolation ((begin list) (end list) (samples number) (curves number))
;  (let ((int (abc-interpolation (list! begin) (list! end) samples
;                               (cond 
;                                ((consp curves) curves)
;                                ((numberp curves) (list curves))
;                                (t (error "bad curves:~S~%" curves)) )))
;       (format 1))
;    (if (eq format 2) (butlast (rest int)) int)))

;------------------------------------------------------------------------
;by M. Malt

(defmethod! rang-p ((liste list) (elem number) &optional (test 'eq) (key nil)) 
  :initvals '((6000) 2)
  :indoc '("a list"  "element to look for" "test function" "key function")
  :icon 128 
  :doc "Returns the position(s) of <elem> in <liste>.

<test> is a function or function name used to test if the elements of the list are equal to <elem>.
<key> is a function or function name that will be applied to elements before the test.

Ex. (rang-p '(0 1 2 3 4 3 2) 3)  =>  (3 5)
Ex. (rang-p '(0 1 2 3 4 3 2) 3 '<)  =>  (0 1 2 6)    [elements at positions 0, 1,2 and 6 are lower than 3]
"
  (let ((aux nil) (index 0))
    (mapcar #'(lambda (z) (progn (when (funcall test (if key (funcall key z) z) elem) (push index aux))
                                 (incf index))) 
            liste)
    (reverse aux)))


(defmethod! rang-p ((liste list) (elem list) &optional (test 'eq) (key nil)) 
  (let ((aux nil) (index 0))
    (mapcar #'(lambda (z) (progn (when (funcall test (if key (funcall key z) z) elem) (push index aux))
                                 (incf index))) 
            liste)
    (reverse aux)))


  
;------------------------------------------------------------------------
; more list operators

; list-explode list-filter table-filter band-filter range-filter posn-match

(defmethod* list-explode ((list list) (nlists integer))
  :initvals '((1 2 3 4 5 6) 2)
  :indoc '("List" "segment size")
  :icon 235
  :doc
  "Segments <list> into <nlist> sublists of (if possible) equal length.

Ex. (list-explode '(1 2 3 4 5 6 7 8 9) 2)  => ((1 2 3 4 5) (6 7 8 9))
Ex. (list-explode '(1 2 3 4 5 6 7 8 9) 5)  => ((1 2) (3 4) (5 6) (7 8) (9)).

If the number of divisions exceeds the number of elements in the list, the divisions will have one element each, and remaining divisions are repeat the last division value.

Ex. (list-explode '(1 2 3 4 5 6 7 8 9) 12)  => ((1) (2) (3) (4) (5) (6) (7) (8) (9) (9) (9) (9))
"
  (if (> nlists (length list))
    (setq list (append list (make-list (- nlists (length list))
                                       :initial-element (first (last list))))))
  (if (<= nlists 1) list
      (let* ((length (length list))
             (low (floor length nlists))
             (high (ceiling length nlists))
             (step (if (< (abs (- length (* (1- nlists) high))) (abs (-
                                                                      length (* nlists low))))
                     high  low))
             (rest (mod length nlists))
             (end (- length 1 rest))
             (ser (arithm-ser 0  (1- step) 1))
             res)
        (for (i 0 step end)
          (push (remove () (posn-match  list (om+  i ser))) res))
        (setq low (length (flat-once res)))
        (if (< low length) (setq res (cons (append (first res) (nthcdr low
                                                                       list)) (rest res))))
        (cond ((> (length res) nlists)
               (nreverse (cons (nconc (second res) (first res)) (nthcdr 2
                                                                        res))))
              ((< (length res) nlists)
               (when (= (length (first res)) 1)
                 (setq res (cons (nconc (second res) (first res)) (nthcdr 2
                                                                          res))))
               (nreverse (nconc (nreverse (list-explode (first res) (1+ (-
                                                                         nlists (length res)))))
                                (rest res))))
              (t (nreverse res))))))

(defmethod* list-filter ((test symbol) (list list) (mode symbol))
  :initvals '(numberp (1 2 3) pass)
  :indoc '("function or function name" "a list" "pass or reject")
  :menuins '((2 (("Reject" 'reject) ("Pass" 'pass))))
  :icon 235 
  :doc  "Filters out  <list> using the predicate <test>.

<test> may be a function name (a symbol) or it may be a visual function or patch in 'lambda' mode. 

If <list> is a list of lists, the filter is applied recursively in the sub-lists.

<mode> 'reject' means reject elements that verify <test>. 
<mode>'pass' means retain only elements that verify <test>.

Ex. (list-filter 'numberp '(5 6 aaa bbb 8) 'pass)  => (5 6 8)
Ex. (list-filter 'numberp '(5 6 aaa bbb 8) 'reject)  => (aaa bbb)
 "
  
  (list-filter (symbol-function test) list mode))

(defmethod* list-filter ((test function) (list list) (mode symbol))
  (if (eq mode 'reject)
    (do-filter list #'(lambda (x) (not (funcall test x))))
    (do-filter list test)))



(defmethod* table-filter ((test symbol) (list list) (numcol integer) (mode symbol))
  :initvals '(numberp ((1 2) (1 2)) 1 pass)
  :indoc '("function or function name" "list of lists" "rank" "pass or reject")
  :menuins '((3 (("Reject" 'reject) ("Pass" 'pass))))
  :icon 235 
  :doc  "Filters out <list> (a list of lists) using the predicate <test>.

<test> may be a function name (a symbol) or it may be a visual function or patch in 'lambda' mode. 

The predicate <test> is applied to the element of rank <numcol> in every sublist in <list> and filters the whole sublists.

<numcol> counts from 0.

<mode> 'reject' means reject elements whose <numcol>th element verifies <test>. 
<mode>'pass' means retain only elements whose <numcol>th element verifies <test>.

Ex. (table-filter 'oddp '((1 2 3) (4 5 6) (7 8 9)) 1 'pass)  --> ((4 5 6))       [keeps lists which first element is odd]
Ex. (table-filter 'oddp '((1 2 3) (4 5 6) (7 8 9)) 1 'reject)  --> ((1 2 3) (7 8 9))     [rejects lists which first element is odd]
"
  
  (table-filter  (symbol-function test) list numcol mode))

(defmethod* table-filter ((test function) (list list) (numcol integer) (mode symbol))
  (if (eq mode 'reject)
    (do-table-filter list #'(lambda (x) (not (funcall  test x))) numcol)
    (do-table-filter list  test numcol)))


(defmethod* band-filter ((list list) (bounds list) (mode symbol))
  :initvals '((1 2 3 4 5) ((0 2) (5 10)) pass)
  :indoc '("a list" "a list of (low high) pairs" "pass or reject" )
  :menuins '((2 (("Reject" 'reject) ("Pass" 'pass))))
  :icon 235 
  :doc  "Filters out <list> using <bounds>.
<bounds> is a pair or list of pairs (min-value max-value). 

If <list> is a list of lists, the filter is applied recursively in the sub-lists.

If <bounds> is a list of pairs, each pair is applied to each successive element in <list>

<mode> 'reject' means reject elements between the bounds 
<mode> 'pass' means retain only elements between the bounds
"
  (list-filter 
   #'(lambda (item)
       (some #'(lambda (bound) (and (>= item (first bound)) (<= item (second bound)))) bounds))
   list 
   mode))


(defmethod* range-filter ((list list) (posn list) (mode symbol))
  :initvals '((1 2 3 4 5) ((0 1) (3 4)) reject)
  :indoc '("a list" "position bounds" "pass or reject")
  :menuins '((2 (("Reject" 'reject) ("Pass" 'pass))))
  :icon 235 
  :doc  "Select elements in <list> whose positions (couting from 0) in the list are defined by <posn>
<posn> is a list of pairs (min-pos max-pos) in increasing order with no everlap.

<mode> 'reject' means reject the selected elements. 
<mode> 'pass' means retain only the selected elements.

Ex. (range-filter '(10 11 12 13 14 15 16) '((0 1) (3 4)) 'pass)  => (10 11 13 14)
Ex. (range-filter '(10 11 12 13 14 15 16) '((0 1) (3 4)) 'reject) => (12 15 16)
"
  (loop for item in list
        for i from 0 
        with bound = (pop posn)
        when (and (eq mode 'reject) (null bound)) collect item
        when (and (eq mode 'pass) bound (>= i (first bound)) (<= i (second bound))) collect item
        when (and (eq mode 'reject) bound (or (< i (first bound)) (> i (second bound)))) collect item
        when (and bound (> i (second bound))) do (setf bound (pop posn))))



(defmethod* posn-match ((list list) (positions list))
  :initvals '('(10 20 30 40 50 60 70 80 90) '((0 1) 4 (6)) )
  :indoc '("a list" "a list positions")
  :icon 235 
  :doc  "Constructs a new list by peeking elements in <list> at positions defined by <positions> (a list or tree of positions). 

<positions> supports the syntax of 'expand-lst'.

Ex. (posn-match '(10 20 30 40 50 60 70 80 90) '((0 1) 4 (6))) => ((10 20) 40 (60))
Ex. (posn-match '(10 20 30 40 50 60 70 80 90) '(3*(0) 3_6)) => (10 10 10 40 50 60 70)
"
  (do-posn-match list (expand-lst positions)))


(defmethod* posn-match ((list list) (positions integer) )
  (nth positions list ))

(defmethod do-posn-match ((self list) (positions list))
  (cond 
   ((numberp positions) (nth positions self))
   ((listp positions)
    (loop for pos in positions
          collect (do-posn-match self pos)))))

(defmethod do-posn-match ((self list) (positions number))
   (nth positions self))
   


(defmethod do-filter ((self t) (test function))
  (if (funcall test self) self 'fail))

(defmethod do-filter ((self cons) (test function)) 
  (loop for item in self 
        for elt =  (do-filter item test)
        when (not (eq elt 'fail)) collect elt))

(defmethod do-table-filter ((list list) (test function) (numcol integer))
  (loop for sublist in list
        when (funcall test (nth numcol sublist))
        collect sublist))


;==========================================================================

;--------------------------------------------
;greats common divisor

(defmethod pgcd ((a rational) (b rational))
   "Find the greats common divisor bethween 2 rational."
   (let ((x (* (denominator a) (numerator b)))
         (y (* (denominator b) (numerator a)))
         (d (* (denominator a) (denominator b))))
     (/ (gcd x y) d)))

(defmethod pgcd ((a integer) (b integer))
   "Find the greats common divisor bethween 2 rational."
   (gcd a b))

(defmethod pgcd ((a number) (b number))
   "Find the greats common divisor bethween 2 rational."
   (pgcd (rational a) (rational b)))

