;;;=================================================
;;; OM streams handling
;;; by Camilo Rueda. (c) IRCAM 1998
;;;=================================================
 
(defpackage "LAZY-EVAL"
  (:use "OpenMusic" "COMMON-LISP")
  (:export "DELAY-EXP" "FORCE" "SER-REST" "DELAY-CONS" "SER-NTH" "SER-NTHCDR"
           "MAKE-SERIES" "MULTI-MAKE-SERIES" "SERIES-RANGE" "LIST->SER"
           "FILTER-SERIES" "DOLIST-DELAYED" "APPEND-SERIES"
           "ALL-PERMUTATIONS" "SHUFFLE-PERMUT" "COMBINATIONS" "COMB-WITH-DUPS"
           "CARTESIAN" "SER->LIST" "ALL-REPLACEMENTS" "LAZY-FILTER"
           "SER-FIRST" "SERIES?"))

(in-package "LAZY-EVAL")

(defmacro delay-exp (exp) `(function (lambda () ,exp)))

(defun to-force? (exp)
"returns 't' if <exp> is a series"
  (subtypep (type-of exp) 'function))

(om::defmethod! null-series? (series)
:initvals '(nil)
  :indoc '("a series")
  :doc
"returns t if series is null"
  (or (not series) (not (ser-first series))))

(om::defmethod! series? (exp)
  :initvals '(nil)
  :indoc '("an object")
  :doc "returns 't' if <exp> is a series"
  (or (not exp) (and (consp exp) (subtypep (type-of (rest exp)) 'function))))

(defun force (exp) (if (to-force? exp) (funcall exp) exp))

(defun force-all (exp) (do ((x exp (force x))) ((not (to-force? x)) x)))

(om::defmethod! ser-rest (exp)
  :initvals '(nil)
  :indoc '("a series")
  :doc "returns a series equal to <exp> without the first element"
  (force (cdr exp)))

(om::defmethod! ser-nth ((n number) exp)
  :initvals '(1 nil)
  :indoc '("position" "a series")
  :doc
"gets the n-th element of the series <exp>"
  (if (zerop n) (force-all (first exp)) (ser-nth (1- n) (ser-rest exp))))

(om::defmethod! ser-first (exp)
  :initvals '(nil)
  :indoc '("a series")
  :doc
  "gets the first element of the series <exp>"
  (force-all (first exp)))

(om::defmethod! ser-nthcdr ((n number) exp)
  :initvals '(1 nil)
  :indoc '("element position" "a series")
  :doc
  "returns a series equal to <exp> without the first n elements"
  (if (zerop n) exp (ser-nthcdr (1- n) (ser-rest exp))))

(om::defmethod! subser->list (series (from number)
                              &optional upto)
  :initvals '(nil 1 nil)
  :indoc '("a series" "from" "upto")
  :doc
  "returns a list corresponding to the subseries starting at <from> and
ending (not including) <upto>"
  (do ((i from (1+ i)) (ser (ser-nthcdr from series) (ser-rest ser)) (res))
      ((>= i upto) (nreverse res))
    (push (ser-first ser) res)))

(defmacro delay-cons (item list)
  `(cons ,item (function (lambda () ,list))))

(om::defmethod! ser-cons (item series)
  :initvals '(nil nil)
  :indoc '("an item" "a series")
  :doc
"adds <item> to the head of the series <series>"
  (delay-cons item series))

(defmacro dolist-delayed (var-exp &body body)
  (let ((var (first var-exp)) 
        (list (second var-exp))
        (result-exp (third var-exp)))
    `(do*  ((sublists ,list (ser-rest sublists)) (,var (force-all (car ,list)) (force-all (car sublists))))
           ((null sublists) ,result-exp) ,@ body)))

(defun get-next (next-fun test-fun item)
  (unless (funcall test-fun item)
    (let ((new-item (funcall next-fun item)))
      (delay-cons new-item (get-next next-fun test-fun new-item)))))

(defun null-init () )
(defun null-last-test (s) )

(om::defmethod! make-series ((init function)
                             (next function)
                             (test function))
  :initvals '(nil nil nil)
  :indoc '("function returning first element" "function returning next element"
           "predicate testing if last element")
  :doc
  "constructs a series.
 <init> is a 0-ary function giving the first element. <next> is a 1-ary function which
given a series element, returns the next element in the series. <test> is a 1-ary
function which given a series element returns t if it is the last element in the
series or NIL otherwise"
  (let ((init-val (funcall init)))
    (delay-cons init-val (get-next next test init-val))))

(defun multi-get-next (next-fun test-fun &rest item)
  (unless (apply test-fun item)
    (let ((new-items (multiple-value-list (apply next-fun item))))
    (delay-cons (first new-items) 
                (multiple-value-call #'multi-get-next next-fun test-fun (values-list new-items))))))

(defun multi-make-series (init
                          next
                          test)
"Same as 'make-series' for multiple-valued series. That is <init> returns a collection of
first elements as multiple-values. <next> takes a collection of series elements on input
and returns a collection of the respective next series elements as a multiple value.
<test> takes a collection of series elements on input and returns t or nil depending
on whether they specify the end of the series"
  (let ((inits (multiple-value-list (funcall init))))
      (delay-cons (first inits) 
                  (multiple-value-call #'multi-get-next next test  (values-list inits)))))

(om::defmethod! series-range ((from number) 
                      &optional (upto most-positive-fixnum)
                      (step 1) )
:initvals '(0 0 1)
  :indoc '("from" "upto" "step")
  :doc
"returns an arithmetic series (possibly infinite) beginning at <from>, with steps of
 <step> (default 1) and ending at <upto> (default infinite)"
  (make-series #'(lambda () from) #'(lambda (item) (+ item step))
               (if (plusp step)
                 #'(lambda (item) (>= (+ item step) upto))
                 #'(lambda (item) (<= (+ item step) upto)))))

(om::defmethod! ser->list (series)
  :initvals '(nil)
  :indoc '("a series")
  :doc
 "transform a series into a list (might take some time and space if the series is
infinite...)"
  (and series
       (let (res) (dolist-delayed (x series (nreverse res)) (push x res)))))

(om::defmethod! list->ser ((list list))
  :initvals '(nil)
  :indoc '("a list")
  :doc
"transform a list into a series"
  (multi-make-series #'(lambda () (values (car list) (cdr list)))
                     #'(lambda (item rest) 
                         (declare (ignore item)) (values (car rest) (cdr rest)))
                     #'(lambda (item rest) (declare (ignore item)) (not rest))))

(defun find-next-satisfying (pred series)
  (and series
       (let ((val (force-all (first series))))
         (if (funcall pred val)
           (cons val (cdr series))
           (find-next-satisfying pred (force (cdr series)))))))

(om::defmethod! filter-series ((pred function)
                               series)
  :initvals '(nil nil)
  :indoc '("a predicate" "a series")
  :doc
"filters a series <series> with a predicate <pred>. Only elements satisfying <pred>
are retained"
  (let ((next (find-next-satisfying pred series)))
    (and next (cons (car next) 
                    (let ((continuation (cdr next)))
                      #'(lambda () (filter-series pred (force continuation))))))))

(defun lazy-filter (pred series )
"filters a series <series> with a predicate <pred>. Only elements satisfying <pred>
are retained. The series is filtered implicitly: no element is tested unless
it is required by a later computation"
  (let ((lazy-next (delay-exp (find-next-satisfying pred series))))
    (delay-cons (delay-exp (first (setq lazy-next (force lazy-next))))
                (filter-series pred (ser-rest (force lazy-next))))))

(om::defmethod! map-series (series
                            (fun function)
                            &rest more-ser)
  :initvals '(nil nil nil)
  :indoc '("a series")
  :doc
  "returns the series resulting of applying the function <fun> to each corresponding element
 of <series> and <more-ser> (if any)"
  (let ((firsts (cons (ser-first series) (mapcar #'ser-first more-ser))))
    (if (member nil firsts) 
      nil
      (delay-cons (delay-exp (apply fun firsts))
                  (apply #'map-series (ser-rest series) fun (mapcar #'ser-rest more-ser))))))

(defun get&move-series (&rest series)
  (let* (value
         (length (1- (length series)))
         (index (dotimes (i length)
                  (if (setq value (car (nth i series))) (return i)))))
    (and index
      (values-list (list* value (ser-rest (nth index series)) (nthcdr (1+ index) series))))))

(om::defmethod! append-series (series1
                               series2 
                               &rest more-series)
  :initvals '(nil nil nil)
  :indoc '("a series""a series" "a series")
  :doc
  "concatenates two or more series"
  (multi-make-series #'(lambda () (apply #'get&move-series series1 series2 more-series))
                     #'(lambda (val &rest more-s) (declare (ignore val))
                        (apply #'get&move-series more-s))
                     #'(lambda (val &rest more-s) (declare (ignore val))
                        (dolist (x more-s t)
                          (if x (return nil))))))

(defun item->series (item )
  "makes an infinite series of <item>"
  (delay-cons item (item->series item)))

(om::defmethod! ser-reduce (series
                            (fun function)
                            &optional
                            (initial 0)
                            (option 1))
  :initvals '(nil nil 0 1)
  :indoc '("a series" "a function" "initial value" "option")
  :doc
  "Reduces a series by a function. Output can be either
 a single value or a series of partial reductions, depending on <return>.
<initial> is the initial value for the reduction (default 0)"
  (if (/= return 2)
    (do* ((subser series (ser-rest subser)) (val (ser-first subser) (ser-first subser))
          (accum initial (funcall fun accum val)))
         ((not (ser-rest subser)) accum))
    (multi-make-series  #'(lambda () (values initial series))
                        #'(lambda (accum ser)
                            (values (funcall fun accum (ser-first ser)) (ser-rest ser)))
                        #'(lambda (accum ser)
                            (declare (ignore accum))
                            (not (ser-first (ser-rest ser)))))))
  
        
 
;;;======================================================
;;utilities

(om::defmethod! all-permutations ((list list))
  :initvals '((1 2))
  :indoc '("a list")
  :doc
"returns a series containing all permutations of <list>"
  (let ((list (copy-list list)))
    (make-series #'(lambda () (sort list '<))
                 #'get-next-perm
                 #'test-end-perm)))

(om::defmethod! om::all-permutations ((list list))
  :initvals '((1 2))
  :indoc '("a list")
  :doc
  "returns a series containing all permutations of <list>"
  (ser->list (all-permutations list)))


(defun where-position (it list length)
  (let ((where (position it list :test #'>)))
    (if where (+ where (- length (length list))) most-positive-fixnum)))

(defun get-next-perm (p)
  (let* ((rp (reverse p)) (length  (length p))
         (currents (maplist #'(lambda (it) (where-position (car it) (cdr it) length))
                           rp))
         (pos-curr (position (apply #'min currents) currents :test #'=))
         (pos-in-rp (nth pos-curr currents))
         (old-item (nth pos-curr rp))
         (new-item (nth pos-in-rp rp))
         p-first)
    (setf (nth pos-curr rp) new-item (nth pos-in-rp rp) old-item)
    (setq p-first (subseq rp 0 pos-in-rp))
    (setq rp (nreverse rp))
    (nconc (subseq rp 0 (- length pos-in-rp)) (sort  p-first '<))))


(defun test-end-perm (p)
  (let (res (search t))
    (do () ((not search))
      (if (cdr p)
        (if (< (pop p) (car p)) (setq search nil))
        (setq search nil res t)))
    res))

(om::defmethod! shuffle-permut ((list list))
   :initvals '((3 5 2))
  :indoc '("a list")
  :doc
"returns a series containing all permutations of <list>. Permutations are computed in
lexicographical order starting with <list>. The list <list> should NOT have 
duplicates."
  (multi-make-series
           #'(lambda () (values list 
                                (let (res) 
                                  (dotimes (i (length list) (nreverse res))
                                    (push i res)))))
           #'get-next-s-perm
           #'test-end-s-perm))

(defun get-next-s-perm (p op)
  (let ((perm-list (get-next-perm op)))
    (values (mapcar #'(lambda (perm) (nth perm p)) perm-list) perm-list)))

(defun test-end-s-perm (p op)
  (declare (ignore p))
  (test-end-perm op))

;;;combinations

(om::defmethod! combinations ((list list) (k number))
  :initvals '((1 2 3) 2)
  :indoc '("a list" "a number")
  :doc
  "returns a series containing all combinations (groupings) of k elements out of
 the list <list>"
  (let ((list (sort (copy-list list) '<)) (n (length list)))
    (and (> n k)
         (make-series #'(lambda () (butlast list (- n k)))
                      #'(lambda (comb) (next-comb comb list k))
                      #'(lambda (comb) (test-end-comb comb list k))))))

(om::defmethod! om::combinations ((list list) (k number))
  :initvals '((1 2 3) 2)
  :indoc '("a list" "a number")
  :doc
  "returns a series containing all combinations (groupings) of k elements out of
 the list <list>"
  (ser->list (combinations list k)))

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

;;combinations with duplications

(defun resize-list (list k)
  (let ((length (length list)))
  (if (> length k) list
      (dotimes (i (-  k length)) (push (car list) list))) list))

(om::defmethod! comb-with-dups ((list list) (k number))
  :initvals '((1 2 3) 2)
  :indoc '("a list" "a number")
  :doc
  "returns a series containing all combinations (groupings) of k elements out of
 the list <list>, including repetitions of elements"
  (let* ((dup-list (sort (resize-list (copy-list list) k) '<))
         (n (length dup-list)))
    (and (>= n k)
         (make-series #'(lambda () (make-list k :initial-element (car dup-list)))
                      #'(lambda (comb) (next-comb-dups comb dup-list k))
                      #'(lambda (comb) (test-end-comb-dups comb dup-list k))))))

(om::defmethod! om::comb-with-reps ((list list) (k number))
  :initvals '((1 2 3) 2)
  :indoc '("a list" "a number")
  :doc
  "returns a series containing all combinations (groupings) of k elements out of
 the list <list>, including repetitions of elements"
  (comb-with-dups list k))

(defun next-comb-dups (comb list k)
  (let ((rcomb (reverse comb)) (in-comb (1- (length comb)))
        in-list new-elem)
    (setq new-elem
          (dolist (item  rcomb)
            (setq in-list (position item list :test #'<))
            (if in-list (return (nth in-list list)))
            (decf in-comb)))
    (nconc (subseq comb 0 in-comb) 
           (cons new-elem (make-list (- k in-comb 1)
                                     :initial-element (car list))))))

(defun test-end-comb-dups (comb list k)
  (declare (ignore k))
  (let ((last (car (last list))))
    (dolist (item comb t)
      (if (/= item last) (return nil)))))

(defun test-end-cart-prod (s1 s2)
  (and (not (ser-rest s1)) (not (ser-rest s2))))

(defun cartesian (s1  s2 )
"returns a series containing the cartesian product of the two series
s1 and s2"
    (multi-make-series
             #'(lambda () (values (list (car s1) (car s2)) 0 0 s1 s2))
             #'(lambda (item i1 i2 last1 last2)
                 (declare (ignore item))
                 (next-cartesian i1 i2 last1 last2 s2))
             #'(lambda (item i1 i2 last1 last2) 
                 (declare (ignore item i1 i2))
                 (test-end-cart-prod last1 last2))))

(defun next-cartesian (i1 i2 last1 last2 s2)
  (let ((ds2 (ser-rest last2)) 
        ds1)
   (if ds2
     (values (list (car last1) (car ds2)) i1 (1+ i2) last1 ds2)
     (values (list (car (setq ds1 (ser-rest last1))) (car s2)) (1+ i1) i2 ds1 s2))))

;;all possible replacements of a value for elements in a list

(defun get-first-replacement (item list)
  (values (sort (copy-list list) #'(lambda (x y) (declare (ignore y)) (= x item)))
          (if (position item list) item ())))



(defun get-first-rep-lists (item list reps)
  (let* ((nums (count item list))
        (length (- (length list) nums))
        (indexes (ser->list (series-range nums (+ nums length))))
        result
        (indexes-list (ser->list
                       (apply #'append-series
                              (dotimes (i (- reps nums) (nreverse result))
                                (push (combinations indexes (1+ i)) result))))))
   (get-next-reps list indexes-list item list)))

(defun get-next-reps (previous indexes item list)
  (declare (ignore previous))
  (let ((list (copy-list list)) (first (first indexes)))
    (values
     (if (numberp first)
       (progn (setf (nth first list) item) list)
       (dolist (i first list) (setf (nth i list) item)))
     (rest indexes))))


(om::defmethod! all-replacements ((list list) item
                                  &optional repetitions)
  :initvals '((1 2 3) 5)
  :indoc '("a list" "an element" "max repetitions")
  :doc
  "returns a series containig all possible sublists resulting of replacements by <item> of
 elements in <list>. The optional argument <repetitions> controls the maximum number of
 repetitions of elements allowed in each sublist"
  (cond ((= (count item list) repetitions)
         (list->ser (list (get-first-replacement item list))))
        ((< (count item list) repetitions)
         (let ((list (get-first-replacement item list)))
           (multi-make-series 
            #'(lambda () (get-first-rep-lists item list repetitions) )
            #'(lambda (previous indexes) (get-next-reps previous indexes item list))
            #'(lambda (list indexes)
                (declare (ignore list)) (null indexes)))))))

(om::defmethod! om::all-replacements ((list list) item
                                      &optional repetitions)
  :initvals '((1 2 3) 5)
  :indoc '("a list" "an element" "max repetitions")
  :doc
  "returns a series containig all possible sublists resulting of replacements by <item> of
 elements in <list>. The optional argument <repetitions> controls the maximum number of
 repetitions of elements allowed in each sublist"
  (all-replacements list item repetitions))
