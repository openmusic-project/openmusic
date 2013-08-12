(in-package :om)



;=========================================


(defun inverse-mod (num n)
  (let (rep)
    (loop for i from 1 to (- n 1)
          while (not rep) do
          (when (= (mod* i num n) 1)
            (setf rep i)))
    rep))


(defun find-q (pj kj n)
  (let ((mod (expt  pj kj)))
    (inverse-mod (/ n mod) mod)))

(defun make-table (nline ncol f)
  (loop for i from 0 to (- nline 1)
        collect (subseq f (* i ncol) (* (+ i 1) ncol))))

(defun ensemble-plus-mod (a b n)
  (cond
   ((null a) b)
   ((null b) a)
   (t (mod+ a b n))))



(defmethod! diff-in-list ((list list) (n integer))
  :icon 420
  (append (loop for i from 1 to (- (length list) 1)
                collect (mod- (nth i list) (nth (- i 1) list) n))
          (list (mod- (car list) (car (last list)) n))))
  
(defmethod! suite-reductible ((list list) n)
  :icon 420
  (let ((fixpoint (create-list (length list) 0)))
    (append (list list)
            (loop while (not (equal list fixpoint))
                  collect (setf list (diff-in-list list n))))))
 
(defmethod! suite-reproductible ((list list) n)
  :icon 420
  (let ((fixpoint (copy-list list)))
    (append (list list) (list (setf list (diff-in-list list n)))
            (loop while (not (equal list fixpoint))
                  collect (setf list (diff-in-list list n))))))

;(suite-decomposition '(2 3 4 1 5 3 5) 6)
(defmethod! suite-decomposition ((suite list) n)
    :icon 420
  (let* ((m (length suite))
         (prime-facts (prime-facts n))
         (pjkj-list (loop for factor in prime-facts
                          collect (expt  (first factor) (second factor))))
         (qj-list (loop for factor in prime-facts
                        collect (find-q  (first factor) (second factor) n)))
         f-red-rep-list Suite-red Suite-rep)
    
    
    (setf f-red-rep-list
          (loop for factor in prime-facts  collect 
                (let* ((pj (first factor))
                       (kj (second factor))
                       (pjkj (expt  pj kj))
                       (fj (loop for el in suite collect (mod el pjkj)))
                       rj pjrj inverse table newline fjred fjrep)
                  (loop for i from 0 to m do
                        (if (divise-p (expt  pj i) m)
                          (setf rj i)))
                  (setf pjrj (expt  pj rj))
                  (setf inverse (inverse-mod (/ m pjrj) pjkj)) 
                  (setf table (make-table (/ m pjrj) pjrj fj))
                  (setf newline (car table))
                  (loop for item in (cdr table) do
                        (setf newline (om+ newline item)))
                  (setf newline (mod* newline inverse pjkj))
                  (setf fjred (loop for i from 1 to (/ m pjrj) append newline))
                  (setf fjrep (mod- fj fjred pjkj))
                  (list fjred  fjrep))))
    (loop for qj in qj-list
          for pjkj in pjkj-list
          for f-red-rep in f-red-rep-list do
          (let ((f-red (first f-red-rep))
                (f-rep (second f-red-rep)))
            (setf Suite-red (ensemble-plus-mod Suite-red (om* (om* qj (/ n pjkj)) f-red ) n))
            (setf Suite-rep (ensemble-plus-mod Suite-rep (om* (om* qj (/ n pjkj)) f-rep ) n))))
    (list Suite-red Suite-rep)))

    
#|
(suite-decomposition '(1 5 3 6 11 4 9 7 8 10 2) 12)
(suite-reductible '(2 5 3 3 2) 5)
(suite-reproductible '(3 1 10 4 11 2 7 9 10 1 3 11) 12)
(suite-reproductible '(2 5 6 3 4 1) 7)


|#

;===================================================
(defun projections-groups (n list)
  (loop for i in list collect (get-projection n i (apply '* list))))

(defun get-projection (n i mod)
  (let ((step (/ mod i))
        (rest (mod n i)))
       ))
;(projections-groups 15 '(3 4))

;====================================================
;aditions succesives0
;====================================================

(defun grow-serie (list start mod test)
  (let ((last start) rep)
    (loop for item in list do
          (let ((new (mod+ item last mod)))
            (push new rep)
            (setf last new)))
    (setf rep (reverse rep))
    (list start (butlast rep) last)
    (if (= test last)
      (cons start (butlast rep))
      (append (cons start (butlast rep)) (grow-serie list last mod test)))))



(defmethod! growing-by-add ((serie list) entries mod)
    :icon 420
  (let ((cur serie) rep)
    (loop for item in entries do
          (let ((new (grow-serie cur item mod item)))
            (push new rep)
            (setf cur new)))
    (reverse rep)))


;(statist (growing-by-add '(2 1 2 1 2 1 2 1)  (append '(11 2) (repeat-n 8 60)) 12) 8)

(defun statist (list number)
  (loop for item in list
        for i = 1 then (+ i 1) collect (/ (round (* 100 (/ (* 100 (count number item)) (length item) ))) 100.0)))

(defun grow-format (list)
  (loop for item in list
        for i = 1 then (+ i 1) do
        (format t "a    ~{~3D~}~%"  item)))

(length '(11 2 8 8 8 8 2 8 2 8 8 8 8 2 5 2 5 2 8 8 8 8 8 8 8 8 8 8 8 2 5 11))
  
;====================================================
;loops of loops
;====================================================
(defun same-elements (l1 l2)
  (when (= (length l1) (length l2))
    (let ((rep t))
      (loop for item in l1
            while rep do
            (setf rep (member item l2)))
      rep)))
       

(defun permut-equal (l1 l2)
  (when (same-elements l1 l2)
    (let ((rep nil))
      (loop for i from 0 to (- (length l1) 1)
            while (not rep) do
            (setf rep (equal l2 (rotate l1 i))))
      rep)))

;(permut-equal '(1 2 3 4 5) '(4 5 1 2 3 ))


(defun loop-of-loop (list mod)
  (let ((new (diff-in-list list mod)) rep)
    (loop while (not (permut-equal list new)) do
          (push new rep)
          (setf new (diff-in-list new mod)))
    (push new rep)
    (cons list (reverse rep))))

;(loop-of-loop '(4 2 0 2 4) 12)

;====================================================
;Modal sequences
;====================================================


(defun reduit-serie (list n)
  (loop for i from 1 to (- (length list) 1)
        collect (mod- (nth i list) (nth (- i 1) list) n)))

;(reduit-serie '(3 3 1 4 1) 12)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
    