;;;Constraints: Pattern matching utilities

(in-package "DELAYCONSTR")
 
(defvar *logical-keywords* '(and or))
(defvar *not-keyword* 'not)
(defvar *ignore-shorter-expressions* t)
(defvar *domain-constraint-function* 'domain-constraint-function)
(defvar *variable-name* 'var)
(defvar *interv-keyword* 'ints)
(defvar *wild-signs* '(? * v? x))
(defvar *interval-relationals* (list #\^ #\> #\< #\g #\l #\_))

(defvar *current-space-step* 1)         ;; this defines the minimum step in a space. 1 means "any" approx
(setf *current-space-step* 1)
(defvar *current-space-step-function* #'identity)

(defun get-current-space-step () *current-space-step*)

(defun round-to-approx (n) (* (round n *current-space-step*) *current-space-step*))

(defmethod approx-to-space-step ((n number)) (funcall *current-space-step-function* n))
(defmethod approx-to-space-step ((lst list)) (mapcar #'approx-to-space-step lst))

(defun set-space-step-approx (delta)
  (if (= delta most-positive-fixnum) 
    (setf *current-space-step-function* #'identity)
   (setf *current-space-step-function* #'round-to-approx))
  (setf *current-space-step* delta))

(defun approx= (x y) (zerop (approx-to-space-step (- x y))))
(defun approx> (x y) (plusp (delayconstr::approx-to-space-step (- x y))))
(defun approx< (x y) (minusp (delayconstr::approx-to-space-step (- x y))))
(defun approx>= (x y) (not (minusp (delayconstr::approx-to-space-step (- x y)))))
(defun approx<= (x y) (not (plusp (delayconstr::approx-to-space-step (- x y)))))

(defvar *pw-user-internal-package* nil)
#+:PW (setf *pw-user-internal-package* "USER-SUPPLIED-IN-OUTS")

;;;for OpenMusic, the OM package will do
#+:OM (setf  *pw-user-internal-package* "OM")

(defun translate-log-exp (exp)
  (cond 
   ((null exp) nil)
   ((numberp (car exp)) (get-match-pattern exp))
   ((functionp (car exp)) `(apply ,(car exp) ,@(append (cdr exp) (list *variable-name*))))
   ((member (car exp) *logical-keywords* :test #'string=)
    (cons (car exp) (mapcar #'translate-log-exp (cdr exp))))
   ((string= (car exp) *not-keyword*)
    (let ((*ignore-shorter-expressions* t))
      (cons 'nnot (mapcar #'translate-log-exp (cdr exp)))))
   ((string= (car exp) *interv-keyword*)
    `(apply ',*interv-keyword* ',(cdr exp) ,*variable-name*))
   ((member (car exp) *wild-signs* :test #'string=)
    (get-match-pattern exp))
   ((and (symbolp (car exp)) 
         (intersection *interval-relationals* (coerce (format () "~A" (car exp)) 'list) :test #'char=))
    (get-match-pattern exp))
   ((symbolp (car exp)) (get-user-function exp *variable-name*))
   (t  (get-match-pattern exp))))

(defun get-user-function (exp var)
  (let ((fun (read-from-string (format () "~A::~A" *pw-user-internal-package* (car exp)))))
    (if (and (eq (symbol-package (car exp)) (symbol-package 'cl-user::foo)) (fboundp fun))
      `(apply ',fun ,@(append (cdr exp) (list var)))
      `(apply ',(car exp) ,@(append (cdr exp) (list var))))))

(defun nnot (exp) (or (eq exp '*ignore*) (not exp)))

;;(setq foo (translate-log-exp '(and (not (ints 12)) (or (v? 5 8 * 7) (not (* 11 11 *))))))
;;(translate-log-exp '(and (not (foo 12)) (or (? 5 8 * 7) (not (* 11 11 *)))))

;;(defmethod match? ((pat number) (exp number)) (approx= pat  (if (minusp pat) exp (abs exp))))

;;in file "delay-pattern-match". generalize the notion of "absolute value" for any distance function
(defmethod match? ((pat number) (exp number))
  (if (< pat cl-user::*distance-neutral-element*)
    (approx= pat exp)
    (if (> exp cl-user::*distance-neutral-element*)
      (approx= pat exp)
      (approx= pat (funcall cl-user::*distance-function* cl-user::*distance-neutral-element* exp)))))

(defun apply-relation (relation data pattern-list)
  (let (interval)
    (cond ((and (char= #\^ (first pattern-list))
                (numberp (setq interval (read-from-string (coerce (rest pattern-list) 'string)))))
           (funcall relation data interval))          
          ((numberp (setq interval (read-from-string (coerce pattern-list 'string))))
           (if (< interval cl-user::*distance-neutral-element*)
             (funcall relation data interval)
             (if (> data cl-user::*distance-neutral-element*)
               (funcall relation data interval)
               (funcall relation (funcall cl-user::*distance-function* cl-user::*distance-neutral-element* data)
                          interval)))))))

(defmethod match? ((pat symbol) (exp number))
  (setq exp (approx-to-space-step exp))
  (let* ((form (coerce (format () "~A" pat) 'list))
         (interval (rest form)) interv1)
    (cond ((or (char= #\t (first (last form))) (char= #\T (first (last form))))
           (setq interval (butlast form))
           (and interval (numberp (setq interval (read-from-string (coerce interval 'string))))
                ;;(= (mod exp 12) (approx-to-space-step interval))
                (approx= (mod exp 12)  interval)))
          ((char= #\^ (first form)) (apply-relation #'approx= exp form))   
          ((char= #\g (first form)) (apply-relation #'approx>= exp interval))
           ;;(and interval (numberp (setq interval (read-from-string (coerce interval 'string))))
                ;;(approx>= exp interval)))

          ((char= #\l (first form)) (apply-relation #'approx<= exp interval))
           ;;(and interval (numberp (setq interval (read-from-string (coerce interval 'string))))
                ;;(approx<= exp interval)))

          ((char= #\> (first form)) (apply-relation #'approx> exp interval))
           ;;(and interval (numberp (setq interval (read-from-string (coerce interval 'string))))
                ;;(approx> exp interval)))

          ((char= #\< (first form)) (apply-relation #'approx< exp interval))
           ;;(and interval (numberp (setq interval (read-from-string (coerce interval 'string))))
                ;;(approx< exp interval)))
          ((setq interval (rest (member #\_ form :test #'char=)))
           (setq interv1 (butlast form (1+ (length interval))))
           (and interval (numberp (setq interval (read-from-string (coerce interval 'string))))
                (numberp (setq interv1 (read-from-string (coerce interv1 'string))))
                (approx>= (abs exp) interv1)
                (approx<= (abs exp) interval))))))

;;(approx-to-space-step 1)
(defmethod match? ((pat null) exp) (declare (ignore exp))  nil)

(defun match-pattern (pat exp &optional previous-var)
  (cond ((null exp) (or (null pat) (equal (union pat '(*)) '(*))))
        ((and (symbolp (car pat)) (string= (car pat) '*))
         (or (match-pattern (cdr pat) (cdr exp) previous-var)
             (match-pattern (cdr pat) exp previous-var)
             (match-pattern pat (cdr exp) previous-var)))
        ((and (symbolp (car pat)) (string= (car pat) '?))
         (match-pattern (cdr pat) (cdr exp) previous-var))
        ((and (symbolp (car pat)) (or (string= (car pat) 'v?) (string= (car pat) 'x)))
         (if previous-var
           (and (match? previous-var (car exp))
                (match-pattern (cdr pat) (cdr exp) previous-var))
           (match-pattern (cdr pat) (cdr exp) (car exp))))
        (t (and (match? (car pat) (car exp)) (match-pattern (cdr pat) (cdr exp) previous-var)))))

;;(match-pattern '(2 * 3 ? ? 6 7 *) '( 2 5 1 3  3 4 5 6 7 8))
;;(match-pattern '(* 11 * ? ? ? ? *) '(2 1 4 3 6 5 11 9 8 10 7))
;;(match-pattern '(* 6 * v? *  v? *) '(2 1 4 3 6 5 11 9 8 10 11 2))
;;(match-pattern '(7/28_13/28 11/28_13/28 10/28_13/28 10/28_13/28) '(12/28 11/28 13/28 10/28))
;;(match-pattern '(2 * ³3 ? ? 6 7 *) '( 2 5 1 3  3 4 5 6 7 8))

(defun get-match-pattern (pat)
  (if *ignore-shorter-expressions*
    `(if (< (length (car ,*variable-name*)) ,(min-pattern-length pat)) '*ignore*
         (apply 'match-pattern ',pat ,*variable-name*))
    `(apply 'match-pattern (min-pattern ',pat (length ,*variable-name*)) ,*variable-name*)))

(defun min-pattern (pat length)
  (cond ((null pat) pat)
        ((zerop length) nil)
        ((eq (car pat) '*) (cons '* (min-pattern (cdr pat) length)))
        (t (cons (car pat) (min-pattern (cdr pat) (1- length))))))

(defun min-pattern-length (pat)
  (cond ((null pat) 0)
        ((eq (car pat) '*) (min-pattern-length (cdr pat)))
        (t (1+ (min-pattern-length (cdr pat))))))

;;(min-pattern-length '(not (and (2 * 3 ? ? 6 7 *))))

(defun ints (intervals domain)
  (let (double)
    (if (eq (first (last intervals)) t) (setq double t intervals (butlast intervals)))
    (member t (mapcar #'(lambda (int) (contains-int? int domain double)) intervals))))


(defun contains-int? (int list &optional double)
  (if double (contains-int?-double int list)
      (let ((all-sums (all-sums list)))
        (dolist (sums all-sums nil)
          (if (member int sums :test #'=) (return t))))))


(defun contains-int?-double (int list)
  (let ((sum 0))
    (do* ((subl list (cdr subl)) ) ((null subl))
      (setq sum 0)
      (if (dolist (elem subl)
            (if (zerop (mod (incf sum elem) int)) (return t)))
        (return t)))))

;(contains-int? 24 '(1 2 3 4 6 5 8 7 9 10 11))
;(ints '(12) '(1 2 3 4 6 5 8 7 9 10 11) t)

(defun all-sums (list)
  (maplist #'(lambda (l1) (maplist #'(lambda (l2) (apply '+ l2)) (reverse l1))) list))

;(all-sums '(2 5 6 1))


(defun set-domain-constraint-fun (logical-exp)
  (setf (fdefinition *domain-constraint-function*)
        (eval `(function (lambda (&rest var) ,(translate-log-exp logical-exp))))))

#|
(set-domain-constraint-fun '(or (2 * 3 ? ? 6 7) (and (* 5 *) (not (ints 12 9)))))
(funcall (fdefinition *domain-constraint-function*) '(2 5 13 3  3 4 18 6 7 8))
(translate-log-exp '(and (not (MM-test)) (MM> 0) (* 0 * 0 * ) (not (* 0 0 0 0 0 *))))
(set-domain-constraint-fun '(and (not (ints 12)) (not (* 11 11 *))))
(funcall (fdefinition *domain-constraint-function*) '(11 7 11 11))
(match-pattern '(* 11 11 * ) '(11 7 11 11))
(apply 'delayconstr::match-pattern '(* 0 * 0 *) '((0 1 1 2 3 4)))
|#

;; Ad-hoc means of equalizing pattern lengths

(defun set-pat-length (exp)
  (let ((length (get-max-patterns-length exp)))
    (labels ((transform-patterns (exp)
               (cond ((not exp) nil)
                     ((or (numberp (car exp)) 
                          (member (car exp) *wild-signs* :test #'string=))
                      (equalize-patterns exp length))
                     ((string= (car exp) *interv-keyword*) exp)
                     (t (cons (car exp) (mapcar #'transform-patterns (cdr exp)))))))
      (transform-patterns exp))))

(defun test-eqzer (a b)
  (if (and (symbolp a) (symbolp b)) (string= a b) (eq a b)))

(defun equalize-patterns (pat length)
  (let ((n-pat (remove '* pat :test #'test-eqzer)))
    (append n-pat (make-list (- length (length n-pat)) :initial-element '?))))

(defun get-max-patterns-length (exp)
  (labels ((compute-pat-length (exp max-length)
             (cond ((null exp) max-length)
                   ((or (numberp (car exp)) 
                        (member (car exp) *wild-signs* :test #'string=)
                        (intersection *interval-relationals* (coerce (format () "~A" (car exp)) 'list) :test #'char=))
                    (max max-length (min-pattern-length exp)))
                   ((string= (car exp) *interv-keyword*) max-length)
                   (t (apply #'max 
                           (mapcar #'(lambda (one-exp) 
                                       (compute-pat-length one-exp max-length))
                                   (cdr exp)))))))
    (compute-pat-length exp 0)))

;;(set-pat-length '(or (2 * 3 ? ? 6 7) (and (* 5 *) (not (ints 12 9)))))
;;(get-max-patterns-length '(or (2 * 3 ? ? 6 7) (and (* 5 *) (not (ints 12 9)))))
