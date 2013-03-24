;;; This file contains the global variables.
;;; Frank D. Valencia,16/10/96
;;; This is the version 1.1b of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda.

(defpackage :fvtempos (:use "COMMON-LISP"))
 
;;; GLOBAL VARIABLES
(defvar fvtempos::vari nil)
(defvar fvtempos::constraint-checks)
(setf fvtempos::constraint-checks 0)
(defvar fvtempos::DOMCURS)
(setf fvtempos::DOMCURS nil)
(defvar fvtempos::GM)
(setf fvtempos::GM nil)
(defvar fvtempos::Binst)
(setf fvtempos::Binst nil)
(defvar fvtempos::Primer-Soporte)
(setf fvtempos::Primer-Soporte nil) 
(defvar fvtempos::Arcs-From)
(setf fvtempos::Arcs-from NIL)
(defvar fvtempos::current-var)
(setf fvtempos::current-var 0)
(defvar fvtempos::Instancias)
(setf fvtempos::Instancias NIL)
(defvar fvtempos::G)
(setf fvtempos::G NIL)

(defvar fvtempos::RAIZ)
(setf fvtempos::RAIZ NIL)
(defvar  fvtempos::MaxChords )
(setf fvtempos::MaxChords 100)
(defvar fvtempos::Fun-Proxima)
(setf fvtempos::Fun-proxima nil)

(defvar fvtempos::current-var)
(setf fvtempos::current-var 0)

(defvar fvtempos::constr-checks-table)

(in-package :cl-user)

#|
(pw::defunp vari () list 
            "You can use this box to make constraints that uses the index of the current 
variable in the search. This index is returned by the box (i.e., (vari))."  
  fvtempos::vari)
(pw::defunp inst-i () list 
            "Returns the current (partial) solution in chord representation.
Note that you can use this box to make constraints that uses values of the previously 
instanciated variables in the search. See userconstraint box in userlib/constraints formation" 
  (chord-solution (normal-form fvtempos::Instancias)))
|#

#|
(om::defmethod! om::vari () :icon 192 
  :doc "You can use this box to make constraints that uses the index of the current 
variable in the search. This index is returned by the box (i.e., (vari))."
  fvtempos::vari)
|#

#+:OM (om::defmethod! om::inst-i () (om::csp->chords (normal-form fvtempos::Instancias)))
#+:OM
(om::defmethod! om::prev-instances () :icon 192 
  :doc "Returns the current (partial) solution in chord representation. You can use this box together with user-Cnstr to build constraints that look during the search at the values of all the previously instanciated variables.
 See the documentation of User-Cnstr"
  (normal-form  fvtempos::Instancias))

#+:PW
(pw::defunp prev-instances () list 
"Returns the current (partial) solution in chord representation.
 You can use this box together with user-Cnstr to build constraints that look during the search
 at the values of all the previously instanciated variables.
 See the documentation of User-Cnstr."
  (normal-form fvtempos::Instancias))

#+:OM
(om::defmethod! om::done-instances () :icon 192 
  :doc "Returns the current (partial) solution. You can use this box together with user-Cnstr to build constraints that look during the search at the values of all the previously instanciated variables.
 See the documentation of User-Cnstr"
  fvtempos::Instancias)

#+:PW
(pw::defunp done-instances () list 
"Returns the current (partial) solution in chord representation.
 You can use this box together with user-Cnstr to build constraints that look during the search
 at the values of all the previously instanciated variables.
 See the documentation of User-Cnstr."
  fvtempos::Instancias)

#+:OM
(om::defmethod! om::current-variable () :icon 192 
  :doc "Returns the current instanciated variable"
  fvtempos::current-var)

#+:PW
(pw::defunp current-variable () list 
"Returns the current instanciated variable"
  fvtempos::current-var)

#+:OM
(om::defmethod! om::statistics-table () :icon 192 
  :doc "Returns the table of unsuccessful checks"
  fvtempos::constr-checks-table)

#+:PW
(pw::defunp statistics-table () list 
"Returns the table of unsuccessful checks"
  fvtempos::constr-checks-table)

(defun find-maximum-conflict ()
  (let ((dim (first (array-dimensions fvtempos::constr-checks-table)))
        (max 0) (x 0) (y 0))
    (dotimes (i dim)
      (loop for j from i to (1- dim) do
            (when (< max (aref fvtempos::constr-checks-table i j))
              (setq max (aref fvtempos::constr-checks-table i j) x i y j))))
    (list max x y)))

#+:OM
(om::defmethod! om::statistics-max-conflict () :icon 192 
  :doc "Returns the variables involving maximum unsuccessful checks"
  (find-maximum-conflict))

#+:PW
(pw::defunp statistics-max-conflict () list 
"Returns the variables involving maximum unsuccessful checks"
  (find-maximum-conflict))


#|
(pw::defunp hd-constraints::interval-interp ((exp list (:value '(0 (4 7) 10 (7 11))))
                             &optional (curve fix>=0 (:value 1))) list
 "<exp> is a list of the form (index1 <list1> index2 <list1> ... indexn <list1>)
where the indexes represent positions in a sequence. Returns a list of length
indexn + 1 where each <list> is in the position given by the index to its left
and positions within two consecutive indexes are filled with interpolations
between their associated <list>'s. For example
<exp> = (0 (4 7) 10 (7 11)) gives
((4 7) (4 7) (5 8) (5 8) (5 9) (6 9) (6 9) (6 10) (6 10) (7 11) (7 11)).
<curve> is an optional value for the type of interpolation curve,
[1 = straight line, 
   <1 = convex, 
   >1 = concave].  "
  (let (interpols)
    (cons 
     (first (rest exp))
     (do* ((subexps exp (cddr subexps)) (from (first exp) (first subexps))
           (from-list (second exp) (second subexps))
           (to (third exp) (third subexps)) (to-list (fourth exp) (fourth subexps)))
          ((null to-list) interpols)
       (setq interpols 
             (nconc interpols 
                    (rest (pw::g-round (pw::interpolation  from-list to-list 
                                                       (1+ (- to from)) curve 1)))))))))
|#

#+:OM (defmethod g-round ((self number)) (round self))
#+:OM (defmethod g-round ((self list)) (cons (g-round (car self)) (g-round (cdr self))))
#+:OM (defmethod g-round ((self null)) nil)


(defun dicho-iter (xmin xmax val fun &aux (x (* .5 (+ xmin xmax))) ymin ymax)
  (when (< (setq ymax (funcall fun xmax)) (setq ymin (funcall fun xmin)))
    (psetq xmin xmax xmax xmin ymin ymax ymax ymin))
  (unless (<= ymin val ymax)
    (error "~S is not between f(~S)=~S and f(~S)=~S." val xmin ymin xmax ymax))
  (until (or (= x xmax) (= x xmin))
      (if (< (funcall fun x) val)
        (setq xmin x)
        (setq xmax x))
      (setq x (* .5 (+ xmin xmax))))
  x)


(defun hd-constraints::interval-interp (exp &optional (curve 1))
  "<exp> is a list of the form (index1 <list1> index2 <list1> ... indexn <list1>)
where the indexes represent positions in a sequence. Returns a list of length
indexn + 1 where each <list> is in the position given by the index to its left
and positions within two consecutive indexes are filled with interpolations
between their associated <list>'s. For example
<exp> = (0 (4 7) 10 (7 11)) gives
((4 7) (4 7) (5 8) (5 8) (5 9) (6 9) (6 9) (6 10) (6 10) (7 11) (7 11)).
<curve> is an optional value for the type of interpolation curve,
[1 = straight line, 
   <1 = convex, 
   >1 = concave].  "
  
  (let (interpols)
    (cons 
     (first (rest exp))
     (do* ((subexps exp (cddr subexps)) (from (first exp) (first subexps))
           (from-list (second exp) (second subexps))
           (to (third exp) (third subexps)) (to-list (fourth exp) (fourth subexps)))
          ((null to-list) interpols)
       (setq interpols 
             (nconc interpols 
                    (rest 
                     #+:OM (g-round (om::interpolation  from-list to-list 
                                                        (1+ (- to from)) curve))
                     #+:PW (pw::g-round 
                            (pw::interpolation  from-list  to-list (1+ (- to from))
                                                curve)))))))))


(defun abc-interpolation (begin end samples curves)
  (mapc #'(lambda (curve) (when (<= curve 0) (error "non-positive curve:~S~%" curve))) curves)
  (if (<= samples 1)
      (list begin)
    (let ((len (1- (min (length begin) (length end))))
          (step (/ 1 (decf samples)))
          (res ())
          (temp ())
          (lcurves (length curves)))
             (dotimes (j (1+ samples))
                (setq temp ())
                (dotimes (i (1+ len))
                   (push 
                         (-power-function
                           (nth i begin)
                           (nth i end)
                           (* j step)
                           (nth (mod i lcurves) curves) )
                         temp
                         ))
                 (push (nreverse temp) res) )
             (nreverse res))))

(defun -power-function (begin end time curve)
  (+ begin (* (- end begin) (expt time curve))))


#+:OM
(om::defmethod! om::interval-interp ((exp list) &optional (curve 1)) :initvals '(0 (2 8) 4 (7 3))
  :indoc '("expression" "curve") :icon 192
 :doc 
 "<exp> is a list of the form (index1 <list1> index2 <list1> ... indexn <list1>)
where the indexes represent positions in a sequence. Returns a list of length
indexn + 1 where each <list> is in the position given by the index to its left
and positions within two consecutive indexes are filled with interpolations
between their associated <list>'s. For example
<exp> = (0 (4 7) 10 (7 11)) gives
((4 7) (4 7) (5 8) (5 8) (5 9) (6 9) (6 9) (6 10) (6 10) (7 11) (7 11)).
<curve> is an optional value for the type of interpolation curve,
[1 = straight line, 
   <1 = convex, 
   >1 = concave].  "
  (let (interpols)
    (cons 
     (first (rest exp))
     (do* ((subexps exp (cddr subexps)) (from (first exp) (first subexps))
           (from-list (second exp) (second subexps))
           (to (third exp) (third subexps)) (to-list (fourth exp) (fourth subexps)))
          ((null to-list) interpols)
       (setq interpols 
             (nconc interpols 
                    (rest (g-round (om::interpolation  from-list to-list 
                                                       (1+ (- to from)) curve)))))))))


;;stability stuff (M. Stroppa)
(defun hd-constraints::getweights (number weights octave-scalers)
  (* (or (second (assoc (floor number 12) octave-scalers)) 1)
                   (or (second (assoc (rem number 12) weights)) 0)))

(defun hd-constraints::coef-stability (chord weights octave-scalers)
  (let ((val 0) (accum 0) (numints 0))
    (do ((subchords chord (rest subchords)) (base-int (first chord) (first subchords)))
        ((null subchords) (/ val numints))
      (incf val (hd-constraints::getweights base-int weights octave-scalers))
      (incf numints)
      (setq accum 0)
      (dolist (int (rest subchords))
        (incf numints)
        (incf val (hd-constraints::getweights (incf accum int) weights octave-scalers))))))

#+:PW
(pw::defunp hd-constraints::funct-and ((exp1 list) (exp2 list) &rest (exp* list)) list
            "usual and"
  (or (and exp1 exp2) (dolist (one-exp exp* (car (last exp*))) (unless one-exp (return nil)))))

#+:PW
(pw::defunp hd-constraints::funct-or ((exp1 list) (exp2 list) &rest (exp* list)) list
            "usual or"
  (or exp1 exp2 (dolist (one-exp exp*) (when one-exp (return one-exp)))))

#+:PW
(pw::defunp hd-constraints::funct-not ((exp1 list)) list
            "usual not"
  (not exp1))

;;;
;;;From PatchWork

#+:OM (defvar *valid-expand-chars* '(#\* #\_))

#+:OM
(defmethod expand-lst ((list list))
  "Expands a list by one (or both) of the following:

1. Repeating each item number times following a pattern
   of the form: number*

2. Creating a sequence of numbers going from n to m by steps of k, 
indicated by the pattern n-m s k. A step of 1 can be omitted. 

For example the list (3* (2 4) 0-8), returns

 (2 4 2 4 2 4 0 1 2 3 4 5 6 7 8), 

 and the list (2* (a z 2*(4 12)  (1-5 )) 0-16s2) returns

(a z 4 12 4 12 (1 2 3 4 5) a z 4 12 4 12 (1 2 3 4 5) 0 2 4 6 8 10 12 14 16)."
  
  (let ((lists list)  result)
    (do () ((null lists) nil)
      (let ((next-elem (pop lists)))
        (cond 
         ((symbolp next-elem)
          (let* ((form (coerce (format () "~A" next-elem) 'list))
                 (from-char (is-in form *valid-expand-chars*))
                 (char-symb (car from-char))
                 (third (cdr from-char))
                 (int (butlast form (length from-char)))
                 up-to)
            (cond 
             ((and (not third) char-symb (char= #\* char-symb) int
                   (numberp (setq int (read-from-string (coerce int 'string)))))
              (push (apply #'append
                           (make-list int
                                      :initial-element 
                                      (expand-lst (pop lists))))
                    result))
             ((and char-symb (char= #\_ char-symb) third
                   (numberp (setq int (read-from-string (coerce int 'string)))))
              (if (setq from-char (member #\s third :test #'char=))
                (progn (setq up-to (butlast third (length from-char))
                             char-symb (car from-char) third (cdr from-char))
                       (if (and char-symb 
                                (char= #\s char-symb)
                                (or (null third)
                                    (numberp 
                                     (setq third (read-from-string (coerce third 'string)))))
                                (numberp 
                                 (setq up-to (read-from-string (coerce up-to 'string)))))
                         (push (om::arithm-ser int  up-to (or third 1)) result)
                         (push (list next-elem) result)))
                (progn
                  (setq up-to (read-from-string (coerce third 'string)))
                  (push (om::arithm-ser int up-to  1) result))
                ))
             (t (push (list next-elem) result)))))
         ((consp next-elem)
          (push (list (expand-lst next-elem)) result))
         (t (push (list next-elem) result)))))
    (apply #'append (nreverse result)))      )

#+:OM (defmethod expand-lst ((list null)) nil)
#+:OM (defmethod expand-lst ((list t)) (expand-lst (list list)))

(defun is-in (list chars)
  (let (res)
    (dolist (ch chars res)
      (if (setq res (member ch list :test #'char=)) (return res)))))

;;;
;;; comes from PW series package
;;;

(in-package :delayed-eval)

(defmacro delay-cons (item list)
  `(cons ,item (function (lambda () ,list))))

(defun get-next (next-fun test-fun item)
  (unless (funcall test-fun item)
    (let ((new-item (funcall next-fun item)))
      (delay-cons new-item (get-next next-fun test-fun new-item)))))

(defun to-force? (exp)
"returns 't' if <exp> is a series"
  (subtypep (type-of exp) 'function))

(defun force (exp) (if (to-force? exp) (funcall exp) exp))

(defun force-all (exp) (do ((x exp (force x))) ((not (to-force? x)) x)))

(defun ser-rest (exp)
"returns a series equal to <exp> without the first element"
  (force (cdr exp)))

(defun make-series (init next test)
"constructs a series.
 <init> is a 0-ary function giving the first element. <next> is a 1-ary function which
given a series element, returns the next element in the series. <test> is a 1-ary
function which given a series element returns t if it is the last element in the
series or NIL otherwise"
  (let ((init-val (funcall init)))
    (delay-cons init-val (get-next next test init-val))))

(defmacro dolist-delayed (var-exp &body body)
  (let ((var (first var-exp)) 
        (list (second var-exp))
        (result-exp (third var-exp)))
    `(do*  ((sublists ,list (ser-rest sublists)) (,var (force-all (car ,list)) (force-all (car sublists))))
           ((null sublists) ,result-exp) ,@ body)))

(defun ser->list (series)
 "transform a series into a list (might take some time and space if the series is
infinite...)"
  (and series
       (let (res) (dolist-delayed (x series (nreverse res)) (push x res)))))


(defun combinations (list k)
"returns a series containing all combinations (groupings) of k elements out of
 the list <list>"
  (let ((list (sort (copy-list list) '<)) (n (length list)))
    (and (> n k)
         (make-series #'(lambda () (butlast list (- n k)))
                  #'(lambda (comb) (next-comb comb list k))
                  #'(lambda (comb) (test-end-comb comb list k))))))

(defun next-comb (comb list k)
  (let ((posns (nreverse 
                (mapcar #'(lambda (item) (position item list)) comb)))
        (length (length list))
        ipos n-pos)
    (setq ipos 
          (dotimes (j k)
            (if (< (nth j posns) (- length j 1)) (return j))))
    (setq n-pos (nth ipos posns))
    (nconc (subseq comb 0 (- k ipos 1)) (subseq list (1+ n-pos) (+ n-pos ipos 2)))))

(defun test-end-comb (comb list k)
   (let ((posns (nreverse (mapcar #'(lambda (item) (position item list))
                                 comb)))
        (length (length list))) 
     (dotimes (j k t)
       (if (< (nth j posns) (- length j 1)) (return nil)))))
