;;; This file contains new list operations.
;;; Frank D. Valencia,16/10/96
;;; This is the version 1.1b of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda.

(in-package :cl-user)

(defun par (x) "checks if x is even" (zerop (mod x 2)))

(defun printl ( &REST L)
"print the last elemt of L"
 (last+ (print L))
) 


(defun borrar-repetidos (lis &optional test)
  "remove duplicates"
  (cond ((null lis)  NIL)
        (T (cons (car lis) (borrar-repetidos (remove  (car lis) (cdr lis) :test (or test #'equal )) test) ))     
        )
  )


;;(borrar-repetidos* '( 1 2 3 2 3 1 1 5 6 7 8))

(defun rep? (lis  )
  (cond ((null lis) nil)
        ((member (car lis) (cdr lis) :test #'equal) t)
        ( t (rep? (cdr lis))))
)
;;(rep? '(1 2 3 2)) 

(defun number-of-rep ( L ) (- (length L) (length (borrar-repetidos L))))
;;(number-of-rep '(11 11 11 11))

(defun num-ocurrences ( e L &optional (test #'=))
  (cond ((null L) 0)
        ((funcall test e (car L)) (1+ (num-ocurrences e (cdr L) test)))
        (t (num-ocurrences e (cdr L))))
  )
;;(num-ocurrences 1 '(0 1 2 1 1))

( defun borrar( E L ) ( cond 
                        ((null L ) NIL) 
                        ((equal E (car L) ) (cdr L))
                        ( t  ( cons (car L) ( borrar E (cdr L ) ) ) ) 
                       )) 

(defun Ins-set ( x L ) (if (member x L :test #'equal) L
                         (cons x L)
                         )
)
( defun Modificar ( L Pos E )   (setf (nth Pos L) E ) L )

( defun Actualizar ( L Pos E &AUX LHelp) 
  (setf LHelp (copy-list L) 
        LHelp (Modificar LHelp Pos E))
) 

( defun Empilar ( L E ) ( append (list E) L)) 


(defun butnth (Pos L &AUX Lout)
  (dotimes (i (length L) (reverse Lout))
    (unless (= i pos) (push (nth i L) Lout))
    )
  )

;;(butnth 2 '(1 2 3 4)) 


(defun RandAccesList ( L )
  (nth (om::om-random-value (list-length L)) L)
)


;;; Tree - list algorithms
;;;

(defun flatp( tree )
  (if (atom tree) nil
      (zerop (apply '+ (mapcar (function (lambda (x) (if (listp x) 1 0))) tree))))
  )

;; (flatp (list 1 '(2) 3))

(defun IsTree ( L )
  (dolist (x L NIL)
    (when (listp x) (return t))
    )
  )

(defun IsLeaf ( L )
  (not (listp L)) 
)


(defun HigthTree ( L ) (cond ((or (null L) (atom L)) -1)
                             (t (1+ (apply (function max) (mapcar (function HigthTree) L))))
                             )
       )

(defun Length-tree ( L &AUX (acum 0))
  (dolist (x L acum) 
    (if (atom x) (incf acum 1)
        (incf acum (Length-tree x)))))

(defun Max-node ( Tree Fmax initial-value &AUX (Max-node-value initial-value))
  (dolist (x Tree Max-node-value)
    (if (atom x) 
      (setf Max-node-value (max (funcall Fmax x) Max-node-value))
      (setf Max-node-value (max (funcall Fmax x) (Max-node x Fmax initial-value) 
                                Max-node-value)) 
    )
  )
)

(defun Fmax-leaf ( L ) (if (flatp L) (list-length L) 0))

;(Max-node '(not (or (ints 12 t) (* 2 2 *) (* 7 7 *) (* 11 12 11 *))) #'Fmax-leaf 0)


( defun anexar ( L E) ( append L (list E) ) )

(defun miembro ( e lis )
 (if (null lis) nil
   (or (equal e (car lis)) (miembro e (cdr lis)))
 )
)

(defun Fmax-Lista ( L f)
  (let ((fmax (car L)))
    (dolist (x (cdr L) fmax)
      (when (funcall f x fmax) (setf fmax x))
      )
    ))

(defun pos-elem-aux ( e lis pos)
  (cond ((null lis) -1)
        ((equal e (car lis)) pos)
        (t (pos-elem-aux e (cdr lis) (1+ pos)))
        )
  )

(defun pos-elem ( e lis )
  (pos-elem-aux e lis 0)
)



(defun Lpos-elem-aux ( e lis pos)
  (cond ((null lis) nil)
        ((equal e (car lis)) (cons pos (Lpos-elem-aux e (cdr lis) (1+ pos))))
        (t (Lpos-elem-aux e (cdr lis) (1+ pos)))
        )
  )

(defun Lpos-elem ( e lis )
  (Lpos-elem-aux e lis 0)
)


;;; (Factor-List (interv-lista 0 100))

(defun pos-elem-aux-gen ( e lis pos f)
  (cond ((null lis) -1)
        ((funcall f e (car lis)) pos)
        (t (pos-elem-aux-gen e (cdr lis) (1+ pos) f))
        )
  )

(defun pos-elem-gen ( e lis f )
  (pos-elem-aux-gen e lis 0 f)
)


(defun LExtract-elem ( e lis f f-extract)
  (cond ((null lis) nil)
        ((funcall f e (car lis)) (cons (funcall f-extract (car lis)) (LExtract-elem e (cdr lis) f f-extract)))
        (t (LExtract-elem e (cdr lis) f f-extract))
        )
  )

(defun LExtract-elem-gen ( e lis f )
  (Lpos-elem-gen e lis f)
)


(defun Lpos-elem-aux-gen ( e lis pos f)
  (cond ((null lis) nil)
        ((funcall f e (car lis)) (cons pos (Lpos-elem-aux-gen e (cdr lis) (1+ pos) f)))
        (t (Lpos-elem-aux-gen e (cdr lis) (1+ pos) f))
        )
  )

(defun Lpos-elem-gen ( e lis f )
  (Lpos-elem-aux-gen e lis 0 f)
)


;;(Factor-list '( 0 1 2 3 4 5 0) '=)

(defun last+ ( L ) (car (last L)))

(defun nthcdr- ( n L)
  (cond ((or (zerop n) (null L)) NIL )
        ((> n 0) (cons (car L) (nthcdr- (1- n) (cdr L))))
        (t (print "error en parametro n"))
        )
  )


(defun nth+ ( n L) (let ((e (nth n L))) (if e e (last+ L))))



(defun elem-between ( lis Inf Sup &AUX Lstout)
  (setf lis (nthcdr (1+ Inf) lis))
  (dolist (x lis )
    (incf Inf)
    (when (= inf sup) (return)
          ) 
    (push x lstout)
    )
  lstOut
  )
;; (elem-between '(1 2 3 4) 0 3)

(defun random-between ( Linf LSup ) (+ Linf (om::om-random-value (1+ (- Lsup Linf))) ))

(defun Interv-lista ( m n &optional (step 1) &AUX LstOut) ;;(break)
  (setf LstOut NIL)
  (do ((j 0 (+ j step))) ((>= j (1+ (- n m))) (reverse LstOut)) 
  ;;(dotimes (j (1+ (- n m)) (reverse LstOut))
    (push (+ j m) LstOut)
    )
  )


(defun elim-pos (pos Lst &AUX LstOut)
  (dotimes (i pos)
    (push (nth i Lst) LstOut)
    )
  (dotimes (i (- (list-length Lst) pos 1))
    (push (nth (+ i pos 1) Lst) LstOut)
    )
  (reverse LstOut)
  )


(defun filter (f L) (if (null L) nil 
                        (progn 
                          (if (funcall f (car L)) 
                            (cons (car L) (filter f (cdr L))) 
                            (filter f (cdr L)))
                          )
                        )
       )

(defun Maximal-Element ( L r ) 
  (let ((Maximal (car L)))
    (dolist (e (cdr L) Maximal) 
      (when (funcall r e Maximal) (setf Maximal e))
      )
    )
  )

(defun Invertir-Dir ( L )
  (let ((xo (car L)))
  (mapcar #'(lambda (xf) (- xo (- xf xo))) L)) 
)

;;(Invertir-Dir '( 50 60 55 ))

;; Set-list's operations
(defun Union+ ( S1 S2 &KEY test ) (cond ((null S1) S2)
                                ((null S2) S1)
                                (t (union S1 S2 :test test))))
(defun Unir ( L1 L2) (union (reverse L1) L2 ))
(defun Union* (L1 L2) (if (and L1 L2) (Union L1 L2) nil))
(defun Unir-2 ( L1 L2) (union (reverse L1) L2 :test 'equal))
(defun Inter ( L1 L2 ) (intersection (reverse L1) L2))
(defun Dif   ( L1 L2 ) (set-difference (reverse L1) L2))

(defun max-size ( DOM )
  (let ((maxi 0))
    (dolist (domi DOM maxi)
      (let ((Size-domi (list-length domi )))
        (setf maxi (MAX maxi Size-DOMi))
        )
      )
    )
  )

;;; Combination's operations on list

(defun sublistas ( L )
  (cond ((null L) (list NIL))
        (t (let ((sl (sublistas (cdr L))))
             (append sl (mapcar (eval`(function (lambda (x)  (cons ,(car L) x)
                                                        ) ) ) sl)
                     
                     )
             )
           )
        )
  )

(defun all-combins-aux ( Alfabeto n )
  (cond ((zerop n) (mapcar 'list Alfabeto))
        (t (let (Lout) 
             (dolist (e Alfabeto LOut) 
                         (setq LOut (append LOut (mapcar (eval`(function (lambda (x)  (cons ,e x)
                                                                                 ) ) ) 
                                                         (all-combins-aux Alfabeto (1- n))) )
                               )
                         )
                )
           )
        )
  )
  


(defun especial_filter ( L e )
  (dolist (x L) (when (/= e (car x)) (return)) (setf L (cdr L)) )
  L
  )

(defun all_combin_> ( Alfabeto n )
  (cond ((zerop n) (mapcar 'list Alfabeto))
        (t (let* ( (All) (LOut))
             (when Alfabeto (setq All (all_combin_> (cdr Alfabeto) (1- n))
                                  Lout (append LOut (mapcar
                                                     (eval`(function (lambda (x)  (cons ,(car Alfabeto) x)
                                                                             ) ) ) 
                                                     All) ))
                   (dolist (e (cdr Alfabeto) LOut)
                     (setf All (especial_filter All e))
                     (setq Alfabeto (cdr Alfabeto)
                           LOut (append LOut (mapcar (eval`(function (lambda (x)  (cons ,e x)
                                                                             ) ) ) 
                                                     All) )
                           )
                     
                     )
                   
                   )
             )
           )
        )
  )
 

;;(length (time (all_combin_>  (interv-lista  1 10) 3)))
;;(all-combins-aux  '(1 2 3) 2)


;;(untrace all-combins-aux)
(defun all-combins ( L )
  (all-combins-aux L (1- (length L)))
  )


(defun permut-list ( ListIn  &AUX ListAux n)
  "Retorna una copia permutada de una Lista. 
   Arg0 : Lista de Entrada"
  (setf ListAux (copy-list ListIn)
         n (list-length ListAux))
  (dotimes (i n ListAux)
    (let ((pos1 (om::om-random-value n))
          (pos2 (om::om-random-value n)))
     (psetf  (nth pos1 ListAux) (nth pos2 ListAux)
               (nth pos2 ListAux) (nth pos1 ListAux))
    )
  )
)

(defun all-permutaciones ( L )
  (cond ((null L) NIL)
        ((= 1 (length L)) (list L))
        ((= 2 (length L)) (list L (reverse L)  ))
        (t (let (Lr)  
             (dotimes (i (length L) (reverse Lr))
               (let ( (Lr1 (all-permutaciones (butnth i L)) ) )
                 (dolist  (Lp Lr1) (push (cons (nth i L) Lp) Lr)
                          )
                 )
               )
             )
           )
        )
  )
;;(length (all-permutaciones '( 1 2 3)))


(defun permutar-sublistas ( L &AUX LOut)
  (dolist (sl (sublistas L) LOut) (setf LOut (append (all-permutaciones sl) LOut)))
  )
;;(permutar-sublistas '( 4 7 11))

(defun combin-sublistas ( L &AUX LOut)
  (dolist (sl (sublistas L) LOut) (setf LOut (append (all-combins sl) LOut)))
  )
;;(length (combin-sublistas '(4 7 11 )))

 


(defun succ ( L Alf m carry )
  (cond ( (and (null L) (not (zerop carry))) (list (first Alf)))
        ((zerop carry) L)
        (t (let* ((x (car L)) (val (mod (+ (pos-elem x Alf) carry) m)))
             (if (zerop val) (setf carry 1) (setf carry 0))
             (cons  (nth val Alf) (succ (cdr L) Alf m carry) )
             )
           )
        )
  )

(defun combin-sublistas_any_arity ( Alf &OPTIONAL n (Size (length Alf)))
  (unless n (setf n Size))
  (let* ((limit 0) (l (list (first Alf))) (Lout (list l)))
    (dotimes (i (1+ n)) 
             (incf limit (expt Size i)))
    (dotimes (i (- Limit 2) (reverse Lout))
      (setf l (succ l Alf Size 1))
      (push (reverse l) LOut)
      )
    )
  )


(defun inside-ambitus? (indexes intervals first-amb last-amb)
  (or 
   (not first-amb) 
   (delayconstr::approx<= (apply #'max (dx->x first-amb (translate-to-interv indexes intervals))) last-amb)))

(defun translate-to-interv (indexes intervals)
  #+:PW (remove nil (pw::posn-match intervals (pw::g- indexes 1)))
  #+:OM (remove nil (mapcar #'(lambda (index) (nth index intervals)) (om::om- indexes 1)))
  )

(defun combin-sublistas_spec_arity ( Alf &OPTIONAL n (Size (length Alf)) first-amb last-amb intervals)
  (unless size (setq size (length Alf)))
  (unless n (setf n Size))
  (let* ((limit 0) (l ) (lout) )
    (dotimes (i n) 
      (push (first Alf) l )
      )
    (setf limit (expt Size n))
    (when (inside-ambitus? l intervals first-amb last-amb)
      (setf Lout (list l)))
    (dotimes (i (- Limit 1) (reverse Lout))
      (setf l (succ l Alf Size 1))
      (when (inside-ambitus? l intervals first-amb last-amb)
        (push (reverse l) LOut))
      )
    )
  )



;(assoc 3 (ccl::assoc-2-lists  '( 0 3 2) '(0 1 2)))
     
(defun Translate-Combin ( L Trans )
  (mapcar (function (lambda(x) (mapcar (function (lambda (y) (second (assoc y Trans)))) x))) L)
)
;;(trace Translate-Combin)

(defun combin-sublistas++ ( Alf &OPTIONAL LstAr &AUX Combinations)
  (cond ((and LstAr (listp  LstAr )) (dolist (Arity (sort LstAr #'<) Combinations ) 
                           (setf Combinations  
                                 (append Combinations (combin-sublistas_spec_arity Alf Arity) )
                                 )
                           ))
        ((atom LstAr)  (combin-sublistas_any_arity Alf LstAr))
        (t (combin-sublistas_any_arity Alf))
  )
)

(defun clas-combin-sublistas ( Alf &OPTIONAL LstAr first-amb last-amb intervals &AUX Combinations)
  (cond ((and LstAr (listp  LstAr ))
         (dolist (Arity (sort LstAr #'<) (nreverse Combinations) ) 
           (push (list Arity (combin-sublistas_spec_arity Alf Arity nil first-amb last-amb intervals))
                 Combinations)
           ))
        ((and LstAr (atom LstAr))
         (list (list LstAr (combin-sublistas_any_arity Alf LstAr))))
        (t (dolist (Arity (interv-lista 1 (length Alf)) (nreverse Combinations) ) 
             (push (list Arity (combin-sublistas_spec_arity Alf Arity nil first-amb last-amb intervals))
                   Combinations)
             ))
        )
  )

;;; (length (second (car (clas-combin-sublistas '(0 1 2 3 4) '(5)))))
;;; (trace clas-combin-sublistas)
;;; (combin-sublistas++ '( 1 2 3) nil)
(defun Sum_nk ( n j k &AUX (Suma 0))
  (do ((i j (incf i))) ((= i (1+ k)) Suma)
             (incf Suma (expt n i)))
  )


;;;(nth (1- (Sum_nk 3 0 2)) (combin-sublistas++ '( 1 2 3) nil))

(defun get-combin-pieces ( L LArities &AUX Lout)
 (if LArities (dolist (x Larities Lout)
                 (setf Lout 
                       (append  (second (assoc x L) ) Lout  )
                       )
                 )
      (dolist (x L Lout)
        (setf Lout (append  (second x)  Lout ))
        )
      )
 )

;;;(trace  get-combin-pieces Sum_nk)                          

;;; special operations



(defun Complet-list ( L e n)
 (append L (make-list (- n (length L)) :initial-element e))
)

;(complet-list '(0 1 2 3) 4 6)

(defun Factor-List-aux (L Lo f)
  (cond ((null L) nil)
        (t (cons (list (car L) (Lpos-elem-gen (car L) Lo f)) 
                 (Factor-List-aux (remove (car L) (cdr L) :test f) Lo f)
                 )
           )
        )
  )


(defun Factor-List ( L f) (Factor-List-aux L L f))


(defun Factor-List-Extract-aux (L Lo f f-extract)
  (cond ((null L) nil)
        (t (cons (list (car L) (lextract-elem (car L) Lo f f-extract)) 
                 (Factor-List-Extract-aux (remove (car L) (cdr L) :test f) Lo f f-extract )
                 )
           )
        )
  )

(defun Factor-List-Extract ( L f f-extract ) (Factor-List-extract-aux L L f f-extract ))

;;(Factor-List-Extract  '((0 0) (1 1) (3 3) (2 0) ) #'(lambda (x y) (= (apply '+ x) (apply '+ y)))  #'first)

(defun append* ( x y f) 
  (if (not (and x y)) nil (funcall f x y))
)

(defun Piece-List ( L x y ) 
 (butlast (nthcdr x L) (- (length L) y 1))
)

(defun n-list ( L n &AUX Lout (i 0))
  (when (>= n 0)
    (dolist (x L)
      (push x Lout)
      (when (= i n) (return ))
      (incf i)
      )
    (reverse Lout)
    )
  )


;;(n-list '(0 1 2 3) 100 )

(defun Piece-Array ( V x y &AUX L)
  (dotimes (i (- y x) (reverse L))
    (push (aref V (+ i x) ) L)
    )
)
