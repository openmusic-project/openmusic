;========================================================================= 
;
;                     RANDOM WALKS
;
;========================================================================= 

 
;;          V1.1
;;                       functions by Mikhail Malt   1998 Paris IRCAM




(in-package "ALEA")

 
;============================================================================
(om::defmethod! randwalk1 ((nc number ) (binf number ) (bsup number ) (long integer ) (pamax integer )) 
  :initvals '(50 0 100 50 4) 
  :indoc '("first value" "lower boundary" "upper boundary" "sequence length" "maximum step")
  :icon 240 
  :doc "Random walk, where <pamax> indicates the maximal step of the random walk,
<nc> the initial value,<long> the length of the sequence and <binf> and <bsup>
 the inferior boundary and the superior boundary. Warning: boudaries are considered as elastic."

(let ((res) (cn nc) (pamax (+ pamax 1)))
  (dotimes (n (- long 1) res)
    (om::newl res (setf nc  
                    (let ((lap 0))
         (setf lap   (* (om-random-value pamax) (if (< (/ (om-random-value 100) 100) .5) (- 1) 1 )))
         (if (> (+ nc lap) bsup) (- (* bsup 2) (+ nc lap))
             (if (< (+ nc lap) binf) (- (* binf 2) (+ nc lap)) (+ nc lap))) )
                          )))
 (cons cn (reverse res))))




(om::defmethod! randwalk2 ((nc number ) (binf number) (bsup number) (long integer) (pamax integer)) 
  :initvals '(6000 4800 7200 20 4) 
  :indoc '("first value" "lower boundary" "upper boundary" "sequence length" "maximum step")
  :icon 240 
  :doc "Random walk, where <pamax> indicates the maximal step of the random walk in semi-tones,
<nc> the initial value in midicents,<long> the length of the
 sequence and <binf> and <bsup> the inferior boundary and the superior boundary also in midicents.
Warning: boudaries are considered as elastic."
  (let ((res) (cn nc) (pamax (+ pamax 1)))
    (dotimes (n (- long 1) res)
      (om::newl res (setf nc  
                          (let ((lap 0))
                            (setf lap   (* 100 (om-random-value pamax) (if (< (/ (om-random-value 100) 100) .5) (- 1) 1 )))
                            (if (> (+ nc lap) bsup) (- (* bsup 2) (+ nc lap))
                                (if (< (+ nc lap) binf) (- (* binf 2) (+ nc lap)) (+ nc lap))) )
                          )))
    (cons cn (reverse res))))
 


(om::defmethod! randwalkX ((nc number)  (binf number) (bsup number)(long integer)
                           (pamax integer) ( prox  integer)) 
  :initvals '(6000 4800 7200 20 4 1) 
  :indoc '("first value" "lower boundary" "upper boundary" "sequence length" "maximum step" "approximation")
  :icon 240 
  :doc "Random walk, where <pamax> indicates the maximal step of the random walk according to the index <prox>.
 If <prox> = 1, <pamax>  will be in semi-tone,
 If <prox> = 2, <pamax>  will be in quarter-tone,
 If <prox> = 4, <pamax>  will be in height-tone, 
 i.e. <pamax>  will change into 1/<prox>  of one semi-tone,
<nc> the initial value in numberents,<long> the length of the 
sequence and <binf> and <bsup> the inferior boundary and the 
superior boundary.Warning: boudaries are considered as elastic."
  (let ((res) (cn nc) (pamax (+ pamax 1)))
    (dotimes (n (- long 1) res)
      (om::newl res (setf nc  
                          (let ((lap 0))
                            (setf lap   (* 100  (/ 1 prox) (om-random-value pamax) (if (< (/ (om-random-value 100) 100) .5) (- 1) 1 )))
                            (if (> (+ nc lap) bsup) (- (* bsup 2) (+ nc lap))
                                (if (< (+ nc lap) binf) (- (* binf 2) (+ nc lap)) (+ nc lap))) )
                          )))
    (cons cn (reverse res))))
 

(om::defmethod!  brownian1 ((nc number) (binf number) (bsup number) (long integer) (sigma number)) 
  :initvals '(50 0.0 100.0 20 2) 
  :indoc '("first value" "lower boundary" "upper boundary" "sequence length" "bandwidth of the gaussian distribution")
  :icon 240 
  :doc "Random walk, simulation of the brownian motion. 
<sigma>  is associated with the <bandwidth> of the gaussian distribution,
<nc> the initial value ,<long> the length of the sequence and
 <binf> and <bsup> the inferior boundary and the superior boundary.
Warning: boudaries are considered as elastic."
  (let ((res) (cn nc))
    (dotimes (n (- long 1) res)
      (om::newl res (setf nc  
                          (let ((lap 0))
                            (setf lap   (*  (alea::gauss 0 sigma)))
                            (if (> (+ nc lap) bsup) (- (* bsup 2) (+ nc lap))
                                (if (< (+ nc lap) binf) (- (* binf 2) (+ nc lap)) (+ nc lap))) )
                          )))
    (cons cn (reverse res))))
 


(om::defmethod!  brownian2 ((nc number) (binf number) (bsup number)  (long integer)
                  (sigma number)) 
  :initvals '(6000 8400 7200 20 2) 
  :indoc '("first value" "lower boundary" "upper boundary" "sequence length" "bandwidth of the gaussian distribution")
  :icon 240 
  :doc "Random walk, simulation of the brownian motion with pitch parameters. 
<sigma> is associated with the <bandwidth> of the gaussian distribution,
<nc> the initial value in midi-cents,<long> the length of the sequence 
and <binf> and <bsup> the inferior boundary and the superior boundary in midi-cents.
Warning: boudaries are considered as elastic."
(let ((res) (cn nc))
  (dotimes (n (- long 1) res)
    (om::newl res (setf nc  
                    (let ((lap 0))
         (setf lap   (* 100 (alea::gauss 0 sigma)))
         (if (> (+ nc lap) bsup) (- (* bsup 2) (+ nc lap))
             (if (< (+ nc lap) binf) (- (* binf 2) (+ nc lap)) (+ nc lap))) )
                          )))
 (cons cn (reverse res))))
 


(om::defmethod!  achorripsis ((nc number) (binf number) (bsup number) (long integer) (g number)) 
  :initvals '(6000 8400 7200 20 2) 
  :indoc '("first value" "lower boundary" "upper boundary" "sequence length" "upper parameter for the linear distribution")
  :icon 240 
  :doc "Random walk is based on the model used by Iannis Xenakis in Achorripsis.
 ( <g> is associated to the linear distribution), <nc> the initial value 
in numberents,<long> the length of the sequence and <binf> and <bsup> 
the inferior boundary and the superior boundary in numberents.
Warning: boudaries are considered as elastic."
(let ((res) (cn nc))
  (dotimes (n (- long 1) res)
    (om::newl res (setf nc  
                    (let ((lap 0))
         (setf lap   (* 100 (* g (- 1 (sqrt (/ (om-random-value 1000) 1000))))
                        (if (< (/ (om-random-value 100) 100) .5) (- 1) 1 )))
         (if (> (+ nc lap) bsup) (- (* bsup 2) (+ nc lap))
             (if (< (+ nc lap) binf) (- (* binf 2) (+ nc lap)) (+ nc lap))) )
                          )))
   (cons cn (reverse res))))
 




(om::defmethod! i1/f ((last integer) (n integer))
  :initvals '(0 7) 
  :indoc '("first value" "scale values parameter")
  :icon 240 
  :doc "Generates a value according to the 1/f distribution with a first value <last>,
 and where <n> is a parameter which defines the output scale values,  i.e. : 
the values will be comprised between 0 and (2^n -1).
Ex:  If n=2   the values will be comprised between 0 and 3,
       If n=4   the values will be comprised between 0 and 15,
       If n=7   the values will be comprised between 0 and 127.
"
  (let ((nw 0) (j)  ( l last) (k (/ (expt 2 n) 2)) (p (/ 1.0 (expt 2 n))))
    (prog ()
      label
      (setf j (floor (/ l k)))
      (if (= j 1) (setf l (- l k)))
      (setf nw (+ nw (* k
                        (if (< (/ (om-random-value 1000.0) 1000.0) p) (- 1.0 j) j))))
      (setf k (/ k 2.0))
      (setf p (* p 2.0))
      (if (> k 1.0) (go label) (return nw)))))



(om::defmethod! seq1/f ((prim integer) (n integer) (long integer)) 
  :initvals '(7 30 0) 
  :indoc '("first value" "scale values parameter" "length size of the sequence" )
  :icon 240 
  :doc "Generates a list of values according to the 1/f distribution where <prim> is the initial value,<long> the length of the sequence, and where <n> is a parameter which defines the output scale values,  i.e. :
 the values will be comprised between 0 and (2^n -1).
Ex:  If n=2   the values will be comprised between 0 and 3,
       If n=4   the values will be comprised between 0 and 15,
       If n=7   the values will be comprised between 0 and 127. "
  (let ((list)  (last prim))
    (dotimes (count (- long 1) list)
      (setf list (cons (setf last (i1/f last n)) list)) )
    (cons prim(reverse list))))


(om::defmethod!  markov2 (( l list)  (prim integer)  (long integer)) 
  :initvals '(nil nil 50) 
  :indoc '("markovian matrix written in the list" "first element" "length size of the sequence" )
  :icon 240 
  :doc  "Generation of sequences of length <long>  from a markovian matrix 
written in the list <l>, where <prim> is the first element of the resulting sequence.
In the list <l> the transition is considered as made from 
the element of the line to the element of the column.  "
       (let ((i prim) (seq) )
         (dotimes (x (- long 1) seq)
           (setf i (do ((indice 1 (+ indice 1))
                        (u (/ (om-random-value 1000.0) 1000.0))
                        (valeur 0))      ; dŽf de var
                       ((> valeur u) (- indice 1))
                     (setf valeur (+ valeur (nth  (- indice 1) (nth  (- i 1)  l))))))
           (setf seq (cons i seq)))
         (cons prim (reverse seq))))


(om::defmethod!  markov1 (( l list)  (prim integer) ) 
  :initvals '(nil nil) 
  :indoc '("markovian matrix written in the list" "last element" )
  :icon 240 
  :doc "Generation of an index from a markovian matrix written in the list <l>,
 where <prim> is the first element of the resulting sequence.
In the list <l> the transition is considered as made from the 
element of the line to the element of the column."
  (do ((indice 1 (+ indice 1))
       (u (/ (om-random-value 1000.0) 1000.0))
       (valeur 0))      ; dŽf de var
      ((> valeur u) (- indice 1))
    (setf valeur (+ valeur (nth  (- indice 1) (nth  (- prim 1)  l))))))



;;;;________________new functions_________________________________



(defun arr-to-list (array )
  (let ((aux nil) (aux1 nil)
        (long (array-dimension  array 0)))
    (dotimes (i  long (reverse aux))
      (push (dotimes (j long (reverse aux1))
              (push (aref array i j) aux1))
            aux)
      (setf aux1 nil))))

;(arr-to-list (make-nul-matrice2 3) )



:*****************

(om::defmethod! Ana-Mark ((data list))
            :initvals '(0 1 2 3 4 5)
  :indoc '("sequence")
  :icon 240 
  :doc "construit une matrice de transition a partir d'un ensemble de donnees <data>
La sortie est une double liste ou la premiere sous-liste
est la liste de donnees de base et la 
deuxieme sous-liste est la matrice de transition"
  (let* (;(longtot (length data))  ; longueur de la liste total
         ;(ntrans (1- longtot))   ;nombre de transitions
         (basedata (om::sort-list (om::remove-dup data 'eq 1)))  ;liste de donnees de base
         
         (long (length basedata))  ;longueur de la liste de donnees de base
         (assocbase (om::mat-trans (list basedata (om::arithm-ser 0 (1- long) 1 )))) ;liste d'association
         ;des paires le premier elem-> data
         ; le deuxieme l'indice!!
         (matrice (make-array (list long long) :initial-element 0))
         (mem nil) 
         (matrilis nil) (translin))
    
    (dolist (n data  matrice)
      (if  mem 
        (progn ()
               (incf (aref matrice  (second (assoc mem  assocbase)) (second (assoc n  assocbase))))
               (setf mem  n))
        (setf mem  n)))
    (setf matrilis (arr-to-list  matrice ))
    (dotimes (l long)
      (push (let ((somme (apply '+ (nth  l matrilis)))) 
              (if (= 0 somme) 1 somme)) translin))
    (setf translin (reverse translin))
    (list basedata
    (dotimes (i long (arr-to-list  matrice ))
      (dotimes (j long)
        (setf (aref matrice i j) (om::OM-round  (/ (aref matrice i j) (nth i translin)) 3))
        )))
    ))


:*****************
;(setf matri1 (ana-mark '(1 2 3 4 5 6  2 4 5 1 2 5 4 1 00 0 0 0 0 )))
;(aref matri1 1 0)   ;linha-coluna
;(array-dimension  matri1 0)
 
:*****************



(om::defmethod! Ana-Mark1 ((data list) (espace list)) 
 :initvals '((0 1 2 3 4 5) (0 1 2 3 4 5))
  :indoc '("sequence"  "espace")
  :icon 240 
  :doc "construit une matrice de transition a partir d'un ensemble de donnees <data>
La sortie est une double liste ou la premiere sous-liste
est la liste de donnees de base et la 
deuxieme sous-liste est la matrice de transition"
  (let* (;(longtot (length data))  ; longueur de la liste total
         ;(ntrans (1- longtot))   ;nombre de transitions
         (basedata espace)  ;liste de donnŽes de base
         
         (long (length basedata))  ;longueur de la liste de donnees de base
         (assocbase (om::mat-trans (list basedata (om::arithm-ser  0 (1- long) 1 )))) ;liste d'association
         ;des paires le premier elem-> data
         ; le deuxieme l'indice!!
         (matrice (make-array (list long long) :initial-element 0))
         (mem nil) 
         (matrilis nil) (translin))
    
    (dolist (n data  matrice)
      (if  mem 
        (progn ()
               (incf (aref matrice  (second (assoc mem  assocbase)) (second (assoc n  assocbase))))
               (setf mem  n))
        (setf mem  n)))
    (setf matrilis (arr-to-list  matrice ))
    (dotimes (l long)
      (push (let ((somme (apply '+ (nth  l matrilis)))) 
              (if (= 0 somme) 1 somme)) translin))
    (setf translin (reverse translin))
    (list basedata
          (dotimes (i long (arr-to-list  matrice ))
            (dotimes (j long)
              (setf (aref matrice i j) (om::OM-round  (/ (aref matrice i j) (nth i translin)) 3))
              )))
    ))


:*****************


(om::defmethod! Ana-Mark2 ((data list) (espace list))
  :initvals '((0 1 2 3 4 5) (0 1 2 3 4 5))
  :indoc '("sequence"  "espace")
  :icon 240
  :doc "construit une matrice de transition a partir d'un ensemble de donnees <data>
La sortie est  liste qui  est la matrice de transition"
  (let* (;(longtot (length data))  ; longueur de la liste total
         ;(ntrans (1- longtot))   ;nombre de transitions
         (basedata espace)  ;liste de donnŽes de base
         
         (long (length basedata))  ;longueur de la liste de donnees de base
         (assocbase (om::mat-trans (list basedata (om::arithm-ser 0  (1- long) 1)))) ;liste d'association
         ;des paires le premier elem-> data
         ; le deuxieme l'indice!!
         (matrice (make-array (list long long) :initial-element 0))
         (mem nil) 
         (matrilis nil) (translin))
    
    (dolist (n data  matrice)
      (if  mem 
        (progn ()
               (incf (aref matrice  (second (assoc mem  assocbase)) (second (assoc n  assocbase))))
               (setf mem  n))
        (setf mem  n)))
    (setf matrilis (arr-to-list  matrice ))
    (dotimes (l long)
      (push (let ((somme (apply '+ (nth  l matrilis)))) 
              (if (= 0 somme) 1 somme)) translin))
    (setf translin (reverse translin))
    (dotimes (i long (arr-to-list  matrice ))
      (dotimes (j long)
        (setf (aref matrice i j) (om::OM-round  (/ (aref matrice i j) (nth i translin)) 3))
        ))
    ))


(defun make-nul-matrice3 (dimension)
(make-list dimension :initial-element 
            (make-list dimension :initial-element 
                       (make-list  dimension :initial-element 
                                   (make-list  3 :initial-element 0)))))

(defun make-nul-matrice2 (dimension)
(make-array (make-list 3 :initial-element dimension) :initial-element 0))







(defun arr-to-list2 (array )
  (let ((aux nil) (aux1 nil) (aux2 nil)
        (long (array-dimension  array 0)))
    (dotimes (i  long (reverse aux))
      (push (dotimes (j long (reverse aux1))
              (push (dotimes (k long (reverse aux2))
                      (push (aref array i j k) 
                            aux2)) 
                    aux1)
              (setf aux2 nil))
            aux)
      (setf aux1 nil))))

;(arr-to-list2 (make-nul-matrice2 3))



(om::defmethod! transition2 ((data list) (espace list))
  :icon 240 
  :doc "construit une matrice de transition de second ordre a partir d'un ensemble de donnees <data>
La sortie est une double liste ou la premiere sous-liste
est la liste de donnees de base et la 
deuxieme sous-liste est la matrice de transition"
  
  (let* ((dimension (1+ (om::list-max  data)))
         (matrice (make-nul-matrice2 dimension))
         (count 0))
    (mapcar #'(lambda (a b c) (incf (apply #'aref matrice (list a b c)) 1)) data (cdr data) (cddr data))
    (setf matrice (arr-to-list2 matrice))
    (om::OM-round (mapcar #'(lambda (m) (om::om/ m (let ((somme (apply '+ m)))
                                                     (if (= 0 somme) 1 somme)))) (om::flat-once matrice)) 4)
    ))



