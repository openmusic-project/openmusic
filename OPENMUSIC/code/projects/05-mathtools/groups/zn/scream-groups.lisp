(in-package :om)

(defun equalrot (A B)
  (let ( resultat )
    (setf resultat (loop for i from 1 to (length A) do 
                         (when (equal (rotate A i) B)
                           (return t))))
    resultat))

(defun gamme (l) (butlast (dx->x 0 l)))

(om::om-with-redefinitions
  (defun structure (l n)
    (let ((rep (x->dx l)))
      (append rep (list (- n (apply '+ rep)))))))


;================PREDICATS=============

(defun complem (l n)
  (sort (set-difference (om::arithm-ser 0 (- n 1) 1) l) '<))

(defun autocomp-p ( n &rest l) 
  (equalrot l (structure (complem (gamme l) n) n)))

(defun autoinv-p ( &rest l)
  (equalrot l (reverse  l) ))

(defun tic-p (n &rest l)
  (equalrot l (structure (complem (gamme (reverse  l)) n) n) ))

(defun ttl-p (n &rest l)
  (let ((rep nil)
        (s (gamme l)))
    (loop for i from 1 to (- n 1)
          while (not rep) do
          (when (equalrot s (om::mod+ i s n))
            (setf rep i)))
    rep))


(defun tp-p (n &rest l)
  (let* ((s (gamme l))
         (rep s)
         (trans (/  n (length l)))
         (sprime (complem rep n) ))
    (loop for i from 1 to (- trans 1) do
          (setf rep (append rep (om::mod+ (car sprime) s n)))
          (setf sprime (complem rep n)))
    (setf rep (remove-duplicates rep))
    (= (length rep) n)))

#|

(defun tp-p (n &rest l)
  (let* ((s (gamme l))
         (trans (/  n (length l)))
         (vide (complem s n) )
         (rep (one-value (group-tp-tp n trans vide s))))
    rep))

(defun my-all-diff (&rest l)
  (= (length (remove-duplicates l)) (length l)))

(defun rempli-tout (n s &rest l)
  (let* ((s (gamme l))
         (rep s))
    (loop for item in l do
          (setf rep (append rep (om::mod+ item s n))))
    (setf rep (remove-duplicates rep))
    (= (length rep) n)))

(defun group-tp-tp (n nbVar domaine pat)
  (solution
   (let* ( (VarArray (loop for i from 1 to nbVar collect (a-member-of domaine)) ))
     (eval `(assert! (funcallv  #'my-all-diff ,. VarArray)))
     (eval `(assert! (funcallv  #'rempli-tout ,n ,pat  ,. VarArray)))
     VarArray)
   (static-ordering #'divide-and-conquer-force)))

|#


;=============================
(defmacro apply-n-time (fun params times)
  `(let ((*counter* 0) 
         (*result* nil))
     (catch 's::end
       (s::for-effects (progn (push  (remove 0 (,fun  ,.params)) *result*)
                         (setf *result* (remove-duplicates *result* :test 'om::equalrot)) 
                         (setf *counter* (length *result*))
                         (when  (= *counter* ,times) (s::throw 's::end *result*)))))))


(defun make-n-variables (n nbVar)
  (loop for k from 1 to nbVar
        collect (s::an-integer-betweenv 1 (1- n))))

;=========================
;ORBITES
;=========================
(in-package :s)

(defun group-orbites (n nbVar)
  (solution
   (let* ((VarArray (om::make-n-variables n nbVar)))
     
     (eval `(assert! (=v (+v ,. VarArray ) ,n)))
     
     VarArray)
   (static-ordering #'divide-and-conquer-force)))



;=========================
;TAC
;=========================

(defun group-tac (n nbVar)
  (when (= nbVar (/ n 2))
    (solution
     (let* ((VarArray (om::make-n-variables n nbVar)))
       
       (eval `(assert! (=v (+v ,. VarArray ) ,n)))
       
       (eval `(assert! (funcallv  #'om::autocomp-p ,n  ,. VarArray)))
       
       VarArray)
     (static-ordering #'divide-and-conquer-force))))

;=========================
;TAI
;=========================
(defun group-tai (n nbVar)
  (solution
     (let* ((VarArray (om::make-n-variables n nbVar)))
       
       (eval `(assert! (=v (+v ,. VarArray ) ,n)))
       
       (eval `(assert! (funcallv  #'om::autoinv-p ,.VarArray)))
       
       VarArray)
     (static-ordering #'divide-and-conquer-force)))

;=========================
;TIC
;=========================

(defun group-tic (n nbVar)
  (when (= nbVar (/ n 2))
    (solution
     (let* ((VarArray (om::make-n-variables n nbVar)))
       
       (eval `(assert! (=v (+v ,. VarArray ) ,n)))

       (eval `(assert! (funcallv  #'om::tic-p ,n  ,. VarArray)))
       
       VarArray)
     (static-ordering #'divide-and-conquer-force))))

;=========================
;TTL
;=========================


(defun group-ttl (n nbVar)
  (unless (om::relativelyprimes n nbVar)
    (solution
     (let* ((VarArray (om::make-n-variables n nbVar)))
       
       (eval `(assert! (=v (+v ,. VarArray ) ,n)))
       
       (eval `(assert! (funcallv  #'om::ttl-p ,n  ,. VarArray)))
       
       VarArray)
     (static-ordering #'divide-and-conquer-force))))


;=========================
;TP
;=========================
;recoubre toute le bnote avec un ensemble de transposition mais sans
;intersection
(defun group-tp (n nbVar)
  (when (integerp (/  n nbVar))
      (solution
       (let* ((VarArray (om::make-n-variables n nbVar)))
         
         (eval `(assert! (=v (+v ,. VarArray ) ,n)))
         
         (eval `(assert! (funcallv  #'om::tp-p ,n  ,. VarArray)))
         
         VarArray)
       (static-ordering #'divide-and-conquer-force))))


;=========================
;TID
;=========================
(defun group-tid (n nbVar)
  (when (integerp (/ n nbVar))
    (list (om::create-list  nbvar (/ n nbVar) ))))





  