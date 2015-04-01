(in-package :om)



;(defmethod get-group-ratio ((self group))
;   (let* ((tree (tree self))
;          (extent (car tree))
;          (addition (loop for item in (second tree) sum (floor (abs (if (listp item) (car item) item))))))
;     (if (integerp (/ extent addition)) nil addition)))


(defmethod get-group-ratio ((self group))
   (let* ((tree (tree self))
          (extent (car tree))
          (addition (loop for item in (second tree) sum (floor (abs (if (listp item) (car item) item))))))
     (cond
      ((= (round (abs addition)) 1) nil)
      ((integerp (/ extent addition)) addition)
      ;; never happen
      ((and (integerp (/ extent addition)) 
             (or (power-of-two-p (/ extent addition))
                 (and (integerp (/ addition extent)) 
                      (power-of-two-p (/ addition extent)))))  nil)
      (t addition))))


(defun powerof2? (n)
  (= n (first (before&after-bin n))))

(defun find-beat-symbol (den)
  "Find the symbolic value from mesuare denominateur."
  (let ((exp 0))
    (loop while (>= den (expt 2 exp)) do
          (setf exp (+ exp 1)))
    (expt 2 (- exp 1))))


(defun find-beat-symbol (den)
  "Find the symbolic value from mesuare denominateur."
  (let ((exp 0))
    (loop while (>= den (expt 2 exp)) do
          (setf exp (+ exp 1)))
    (expt 2 (- exp 1))))

(defun is-binaire? (durtot)
  "All power of 2 is binaire"
  (if (not (= 1 (numerator durtot))) nil
      (let ((denom (denominator  durtot))
            (exp 0))
        (loop while (> denom (expt 2 exp)) do
              (setf exp (+ exp 1)))
        (if (= denom (expt 2 exp)) t))))

(defun is-ternaire? (durtot)
  (and (= 3 (numerator durtot)) 
       (is-binaire? (/ 1 (denominator  durtot))))  
  )

;===================================
(defun find-denom (num durtot)
  "Find the rigth denom to ratio of tuplet."
  (cond
   ((is-binaire? durtot) (get-denom 'bin num))
   ((is-ternaire? durtot) (get-denom 'ter num))
   (t (get-denom-other durtot num))))


(defmethod get-denom ((type t) (num t) ))

;===Binaire
(defmethod get-denom ((type (eql 'bin)) (num (eql 3)) ) 2)
(defmethod get-denom ((type (eql 'bin)) (num (eql 4)) ) 4)
(defmethod get-denom ((type (eql 'bin)) (num (eql 5)) ) 4)
(defmethod get-denom ((type (eql 'bin)) (num (eql 6)) ) 4)
(defmethod get-denom ((type (eql 'bin)) (num (eql 7)) ) 4)
(defmethod get-denom ((type (eql 'bin)) (num (eql 8)) ) 8)
(defmethod get-denom ((type (eql 'bin)) (num (eql 9)) ) 8)
(defmethod get-denom ((type (eql 'bin)) (num (eql 10)) ) 8)
(defmethod get-denom ((type (eql 'bin)) (num (eql 11)) ) 8)
(defmethod get-denom ((type (eql 'bin)) (num (eql 12)) ) 8)
(defmethod get-denom ((type (eql 'bin)) (num (eql 13)) ) 8)
(defmethod get-denom ((type (eql 'bin)) (num (eql 14)) ) 8)
(defmethod get-denom ((type (eql 'bin)) (num (eql 15)) ) 16)
(defmethod get-denom ((type (eql 'bin)) (num (eql 16)) ) 16)

(defun before&after-bin (den)
  "Find the symbolic value from mesuare denominateur."
  (let ((exp 0))
    (loop while (>= den (expt 2 exp)) do
          (setf exp (+ exp 1)))
    (list (expt 2 (- exp 1)) (expt 2 exp))))

(defmethod get-denom ((type (eql 'bin)) (num t) )
   (let* ((powers (before&after-bin num)))
     (if (> (- num (first powers))
            (- (second powers) num))
       (second powers) (first powers))))

;===Ternaire
(defmethod get-denom ((type (eql 'ter)) (num (eql 2)) ) 3)
(defmethod get-denom ((type (eql 'ter)) (num (eql 3)) ) 3)
(defmethod get-denom ((type (eql 'ter)) (num (eql 4)) ) 3)
(defmethod get-denom ((type (eql 'ter)) (num (eql 5)) ) 6)
(defmethod get-denom ((type (eql 'ter)) (num (eql 6)) ) 6)
(defmethod get-denom ((type (eql 'ter)) (num (eql 7)) ) 6)
(defmethod get-denom ((type (eql 'ter)) (num (eql 8)) ) 6)
(defmethod get-denom ((type (eql 'ter)) (num (eql 9)) ) 6)
(defmethod get-denom ((type (eql 'ter)) (num (eql 10)) ) 12)
(defmethod get-denom ((type (eql 'ter)) (num (eql 11)) ) 12)
(defmethod get-denom ((type (eql 'ter)) (num (eql 12)) ) 12)
(defmethod get-denom ((type (eql 'ter)) (num (eql 13)) ) 12)
(defmethod get-denom ((type (eql 'ter)) (num (eql 14)) ) 12)
(defmethod get-denom ((type (eql 'ter)) (num (eql 15)) ) 12)
(defmethod get-denom ((type (eql 'ter)) (num (eql 16)) ) 12)
(defmethod get-denom ((type (eql 'ter)) (num (eql 17)) ) 12)

(defun before&after-ter (den)
   "Find the symbolic value from mesuare denominateur."
   (let ((exp 3))
     (loop while (>= den exp) do
           (setf exp (* exp 2)))
     (list (/ exp 2) exp)))


(defmethod get-denom ((type (eql 'ter)) (num t) )
   (let* ((powers (before&after-ter num)))
     (if (> (- num (first powers))
            (- (second powers) num))
       (second powers) (first powers))))

;=====Other

(defun before&after-other (den durtot)
   "Find the symbolic value from mesuare denominateur."
   (let ((exp durtot))
     (loop while (>= den exp) do
           (setf exp (* exp 2)))
     (list (/ exp 2) exp)))


;When the answer is a list its change num
(defun get-denom-other ( durtot num )
   (setf durtot (numerator durtot))
   (cond
    ((= (+ num 1) durtot) durtot)
    ((= num durtot) num)
    ((< num durtot)
     (list (* num 2) durtot))
    ;((< num (- (* 2 durtot) 1))   ;OJO OJO ESTOS CASOS HAY QUE VERLOS CON KARIM
     ;durtot)
    (t
     (let* ((powers (before&after-other num durtot)))
       (if (> (- num (first powers))
              (- (second powers) num))
         (second powers) (first powers))))))


;chif is a list or a num (i.e.  (4 5) -->   "4:5"
(defun chif2sstr (chif)
  (if (listp chif)
    (string+ (num2sstr (car chif)) 
             (if (is-binaire? (/ 1 (second chif))) "" (string+ " " (num2sstr (second chif)))))
    (num2sstr chif)))


;===================================================================
;notes longues
;===================================================================

(defun unite+fraction (n)
  (if (>= (numerator n) (denominator n))
    (let ((list (multiple-value-list (floor (numerator n) (denominator n)))))
      (list (car list) (/ (second list) (denominator n))))
    (list 0 n)))

(defun get-integer-grups (n by)
  (let* ((unites n)
         (continue t) rep)
    (if (not (listp by))
      (progn
        (loop while (>= unites by) do
              (push by rep)
              (setf unites (- unites by)))
        (if (zerop unites)
          (setf rep (reverse rep))
          (setf rep (reverse (cons unites rep)))))
      (progn
        (loop for item in by
              while continue do
              (let* ((nw-list (get-integer-grups unites item))
                     (last (car (last nw-list))))
                (if (= last item)
                  (progn
                    (setf rep (append rep nw-list))
                    (setf continue nil))
                  (progn
                    (setf rep (append rep (butlast nw-list)))
                    (setf unites last)))))
        (unless (zerop unites)
          (setf rep (append rep (list unites))))))
    rep))
    
    

(defun get-figures (n by)
  (let* ((list-by-by (get-integer-grups n by))
         (last (car (last list-by-by))))
    (if (integerp last)
      list-by-by
      (let* ((unites (unite+fraction last))
            (fraction (second unites))
            (unites (car unites)))
        (list+ (butlast list-by-by) (get-integer-grups unites nil) (list fraction))))))


(defun rewrite-long-numbers  (tree relatif)
  (cond ((numberp tree)
         (if (> (/ (abs tree) relatif) 1)
           (make-liasons (if (minusp tree)
                           (om* (* -1  relatif) (get-figures (/ (abs tree) relatif) 1))
                           (om* (get-figures (/ (abs tree) relatif) 1) relatif)))
           (list tree)))
        ((listp tree)
         (list (list  (first tree) (mapcan #'(lambda (x) 
                                               (rewrite-long-numbers x (* (subtree-list-size (second tree)) (/ relatif (first tree))))) 
                                           (second tree)))))))

(defun make-liasons  (list)
  (cdr (mapcan #'(lambda (x) 
              (list 0 x)) list)))


(defun long-numbers  (tree)
  (let ((measures (second tree)))
    (cons (car tree)
          (list (loop for item in measures collect
                      (list (first item) 
                            (loop for symb in (second item) append
                                  (rewrite-long-numbers symb (second (first item))))))))))




(defun figure-size (self)
  (cond ((numberp self) (abs self))
        (t (car self))))

(defun subtree-list-size (self)
  (apply '+ (mapcar #'figure-size self)))
  
;===============================================


(defun only-one-point (n)
  (cond
   ;((zerop n) (om-beep-msg (format nil "Warning: 0 is not allowed in rhythm trees")))
   ((floatp n) (mapcar 'float (only-one-point (round n))))  
   (t                              
    (if (member n '(0 1 2 3 4 6 8 12 16 32)) ;only for optimization
      (list n)
      (let ((bef (car (before&after-bin n))))
        (cond
         ((or (= bef n) (= (* bef 1.5) n) (= (* bef 1.75) n))  (list n))
         ((> n (* bef 1.5))
          (append (list (+ bef (/ bef 2))) (only-one-point (/ (- n (* bef 1.5)) 1.0))))
         (t (cons bef (only-one-point (/ (- n bef) 1.0))))))))))


;--------
(defun modulo3-p (n)
   (or (zerop ( mod n 3)) (= n 1))) 

;=========================================================

;get note (or rest) head and points from a ratio

(defun note-head-and-points (val &optional rest)
   (let* ((haut (numerator val))
          (bas (denominator val))
          (bef (car (before&after-bin haut)))
          (points 0) (char "*"))
     (cond
      ((= bef haut)
       (setf char (note-strict-char (/ haut bas) rest))
       (setf points 0))
      ((= (* bef 1.5) haut)
       (setf char (note-strict-char (/ bef bas) rest))
       (setf points 1))
      ((= (* bef 1.75) haut)
       (setf char (note-strict-char (/ bef bas) rest))
       (setf points 2)))
     ;(print (format nil "val ~D head ~D  points ~D" val char points))
     (list char points)))

      
(defun note-strict-char (val &optional rest)
  (if rest
      (cond
       ((> val 4) (list val))
       ((= val 4) (rest-4))
       ((= val 2) (rest-2))
       ((= val 1) (rest-1))
       ((= val 1/2)  (rest-1/2))
       ((= val 1/4)  (rest-1/4))
       ((= val 1/8)  (rest-1/8))
       ((= val 1/16)  (rest-1/16))
       ((= val 1/32)  (rest-1/32))
       ((= val 1/64)  (rest-1/64))
       ((= val 1/128)  (rest-1/128))
       (t  (rest-1/128)))
    (cond 
     ((>= val 8) (list val))
     ((= val 4) (head-4))
     ((= val 2) (head-2))
     ((= val 1) (head-1))
     ((= val 1/2)  (head-1/2)  )        
     (t  (head-1/4)))))

;=========================================================

(defun get-number-of-beams (val)
   (let* ((haut (numerator val))
          (bas (denominator val))
          (bef (car (before&after-bin haut)))
          (beams 0))
     (cond
      ((= bef haut)
       (setf beams (note-strict-beams (/ haut bas) ))
       )
      ((= (* bef 1.5) haut)
       (setf beams (note-strict-beams (/ bef bas) ))
       )
      ((= (* bef 1.75) haut)
       (setf beams (note-strict-beams (/ bef bas) ))
       ))
     ;(print (format nil "val ~D ~D" val beams ))
     beams))


(defun note-strict-beams (val)
   (cond
    ((or (= val 1/4) (= val 1/2) (>= val 1)) 0)
    ((= val 1/8)  1)
    ((= val 1/16)   2)
    ((= val 1/32)   3)
    ((= val 1/64)   4)
    ((= val 1/128)   5)
    ((= val 1/256)   6)
    ((= val 1/512)   7)
    ((is-binaire? val) (round (- (log (denominator val) 2 ) 2)))
    (t  (find-group-symbol val))))


(defun find-group-symbol (val)
  (let* ((haut (numerator val))
         (bas (denominator val))
         (bef (car (before&after-bin bas))))
    (list  (note-strict-beams (/ 1 bef)) (denominator (/ bef bas )))))

;A mejorar utilisando los algorithmos de find-num-denom




   


  