;;; This file contains the tree-structured domain implementation : Cursors and two-level trees.
;;; Frank D. Valencia,16/10/96
;;; This is the version 1.1b of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda.
(in-package :cl-user)
 
(defstruct  Cursor 
  "Each cursor represents a level of the tree-structured domain"
  (vacio   t   :type symbol) 
  (primero 0   :type integer)
  (ultimo  0   :type integer)
  (vect    NIL :type array)
  (padre   NIL :type array)
  (hijo-izq  NIL :type array)
  (hijo-der  NIL :type array)
  (exist   NIL :type  array )
  (next    NIL :type array )
  (prior   NIL :type array )
  (Cardinal 0  :type integer)
  )


(defun  Crear-Dom-Cursor-Jer ( Dom-Lista &AUX Dom-Cursor-Padre Dom-Cursor-Hijo 
                                         (i 0) (j 0) Size Size-2 (acum 0)) 
  "Builds a tree-structured domain given a tree-list 'Dom-Lista'"  
  (setf    
   Size (print (list-length Dom-Lista))
   Size-2 (print (apply (function +) (mapcar  (function (lambda (x) (list-length (second x)))) Dom-Lista)))
   
   Dom-Cursor-Padre (make-cursor :vacio (zerop Size) 
                                 :primero 0 :ultimo (1-  Size ) 
                                 :vect  (make-array  Size) 
                                 :exist (make-array Size :element-type 'symbol)
                                 :next  (make-array Size :element-type 'integer )
                                 :prior (make-array Size :element-type 'integer )
                                 :padre (make-array 0 :element-type 'integer)
                                 ;by AAA 01/12/03  I am not sure that I make the rigth thing here
                                 ;:padre NIL
                                 :hijo-izq (make-array Size :element-type 'integer)
                                 :hijo-der (make-array Size :element-type 'integer)
                                 :Cardinal Size)
   Dom-Cursor-Hijo (make-cursor :vacio (zerop Size-2) 
                                :primero 0 :ultimo (1-  Size-2 ) 
                                :vect  (make-array  Size-2) 
                                :exist (make-array Size-2 :element-type 'symbol)
                                :next  (make-array Size-2 :element-type 'integer )
                                :prior (make-array Size-2 :element-type 'integer )
                                :padre (make-array Size-2 :element-type 'integer)
                                :hijo-izq (make-array 0 :element-type 'integer)
                                :hijo-der (make-array 0 :element-type 'integer)
                                ;by AAA 01/12/03
                                ;:hijo-izq NIL
                                ;:hijo-der NIL
                                :Cardinal Size-2)
   
   )
  (dolist (v Dom-Lista)
    (setf  (aref (cursor-prior Dom-Cursor-Padre) i) (1- i)
           (aref (cursor-next  Dom-Cursor-Padre) i) (1+ i)
           (aref (cursor-vect  Dom-Cursor-Padre) i) (first v)
           (aref (cursor-exist Dom-Cursor-Padre) i) t
           (aref (cursor-hijo-izq Dom-Cursor-Padre) i) acum
           (aref (cursor-hijo-der Dom-Cursor-Padre) i) (1- (incf acum (list-length (second v))))
           )
    (dolist (w (second v))
      (setf  (aref (cursor-prior Dom-Cursor-Hijo) j) (1- j)
             (aref (cursor-next  Dom-Cursor-Hijo) j) (1+ j)
             (aref (cursor-vect  Dom-Cursor-Hijo) j) w
             (aref (cursor-exist Dom-Cursor-Hijo) j) t
             (aref (cursor-padre Dom-Cursor-Hijo) j) i
             )
      (incf j)
      )
    (incf i)  
    )
  (list Dom-Cursor-Padre Dom-Cursor-Hijo)
  )

; (Crear-Dom-Cursor-Jer    '((1 (1 2)) (2 (2 1)) (3 (3)) (4 (4 2 4))))


(defun Primer-Nodo ( Cursor Level )
  "Returns first node index at level 'Level' of 'Cursor'"
  (aref  (Primero-cursor (nth Level Cursor)))
  )

(defun Ultimo-Nodo ( Cursor Level )
  "Returns last node index at level 'Level' of 'Cursor'"
  (aref  (Ultimo-cursor (nth Level Cursor)))
  )

(defun Padre (Cursor Pos)
  "Returns the parent of the node index 'Pos' in 'Cursor'"
  (aref (cursor-padre (second Cursor)) Pos)
  )


(defun nodo-Izq-Cursor ( Cursor )
  "Returns the lefmost child index of the first node at level 1 in 'Cursor'"
  (aref (cursor-hijo-Izq (first Cursor)) (Primer-Nodo Cursor 1))
  )

(defun nodo-Der-Cursor ( Cursor)
  "Returns the rightmost child index of the first node at level 1  in 'Cursor'"
  (aref (cursor-hijo-Der (first Cursor)) (Primer-Nodo Cursor 1))
  )


(defun Rama-Izq-Cursor ( Cursor )
  "Returns the leftmost branch  in 'Cursor'"
  (list (Primer-Nodo Cursor 1) (aref (cursor-hijo-Izq (first Cursor)) (Primer-Nodo Cursor 1)))
  )


(defun Hijo-Izq-Cursor ( Cursor Pos)
  "Returns the lefmost child index of the node index 'Pos'  in 'Cursor'"
  (aref (cursor-hijo-Izq Cursor) Pos)
  )

(defun Hijo-Der-Cursor ( Cursor Pos)
"Returns the rightmost child index of the node index 'Pos'  in 'Cursor'"
  (aref (cursor-hijo-Der Cursor) Pos)
  )

(defun actualice-hijos (Dj1 Dj2 Hijo Padre)
  "Updates previously removed nodes"
  (unless (existe-cursor Dj2 (Hijo-Izq-Cursor  Dj1 Padre))
    (setf (aref (cursor-hijo-izq Dj1) Padre) Hijo)
    )
  
  (unless (existe-cursor  Dj2 (Hijo-Der-Cursor Dj1 Padre))
    (setf (aref (cursor-hijo-der Dj1) Padre) Hijo)
    )
  
  (when (< Hijo (Hijo-Izq-Cursor  Dj1 Padre))
    (setf (aref (cursor-hijo-izq Dj1) Padre) Hijo)
    )
  
  (when (> Hijo (Hijo-Der-Cursor Dj1 Padre))
    (setf (aref (cursor-hijo-Der Dj1) Padre) Hijo)
    )
  )

(defun Rev-cursor-jer (Dj Index &AUX Dj1 Dj2 Padre )
  "Undeletes a previously deleted node Index from 'Dj'"
  (setf Dj1 (first Dj) Dj2 (second Dj))
  (if (= (second Index) 1) 
    (progn
      (setf Dj2 (rev-cursor Dj2 (first Index)))
      (setf Padre (aref (cursor-padre Dj2) (first Index)))
      (unless (existe-cursor Dj1 Padre)
        (setf Dj1 (rev-cursor Dj1 Padre))
        )
      (actualice-hijos Dj1 Dj2 (first Index) Padre)
      )
    (progn 
      (setf Dj1 (rev-cursor Dj1 (first Index)))
      )
    )
  (list Dj1 Dj2)
  )




(defun Borrar-Dom-Cursor ( Dom-Cursor  Pos &AUX  Next-Cas Prior-Cas)
  "Deletes the element index 'Pos' from 'Dom-Cursor'"
  ;(when (not (aref (cursor-exist Dom-Cursor) Pos)) (break))
  (when (and  (not (cursor-vacio Dom-Cursor)) (aref (cursor-exist Dom-Cursor) Pos))
    (cond
     ( (= Pos (cursor-primero Dom-Cursor))
       (if (= (cursor-Cardinal Dom-Cursor) 1) 
         (setf (cursor-vacio Dom-Cursor) t 
               (aref (cursor-exist Dom-Cursor) Pos) NIL
               ) 
         (setf (aref (cursor-exist Dom-Cursor) Pos) NIL
               (cursor-primero Dom-Cursor) (aref (cursor-next Dom-Cursor) Pos) 
               ) 
         )
       )
     ( (= Pos (cursor-ultimo Dom-Cursor))
       (setf (aref (cursor-exist Dom-Cursor) Pos) NIL
             (cursor-ultimo Dom-Cursor) (aref (cursor-prior Dom-Cursor) Pos) )
       )
     
     ( t
       (setf (aref (cursor-exist Dom-Cursor) Pos) NIL
             Prior-Cas (aref (cursor-prior Dom-Cursor) Pos) 
             Next-Cas  (aref (cursor-next  Dom-Cursor) Pos)
             (aref (cursor-next Dom-Cursor) Prior-Cas) Next-Cas
             (aref (cursor-prior Dom-Cursor) Next-Cas) Prior-Cas )
       )
     )
    (decf (cursor-Cardinal Dom-Cursor))
    )
  Dom-Cursor
  )

(defun Ins-cursor ( Dom-cursor Pos Elem)
  "Adds 'Elem' to 'Dom-cursor' in the position 'Pos'"
  (when (not (aref (cursor-exist Dom-Cursor) Pos))
    (if (cursor-vacio Dom-Cursor) 
      (progn  
        (setf (aref (cursor-exist Dom-Cursor) Pos) t
              (aref (cursor-vect Dom-Cursor) Pos) Elem
              (cursor-primero Dom-Cursor) Pos
              (cursor-ultimo Dom-Cursor) Pos
              (cursor-vacio Dom-Cursor) NIL
              ) 
        (incf (cursor-Cardinal Dom-Cursor))
        )
      (let ( pos-enlace adelante Prior-Cas Next-Cas )
        (cond 
         ((< Pos (cursor-primero Dom-Cursor))
          (setf  pos-enlace (cursor-primero Dom-Cursor)
                 (cursor-primero Dom-Cursor) Pos
                 (aref (cursor-prior Dom-Cursor) pos-enlace) Pos
                 (aref (cursor-next Dom-Cursor) Pos) pos-enlace)
          )
         
         ((> Pos (cursor-ultimo Dom-Cursor)) 
          (setf pos-enlace (cursor-ultimo Dom-Cursor)
                (cursor-ultimo Dom-Cursor) Pos
                (aref (cursor-next Dom-Cursor) pos-enlace) Pos
                (aref (cursor-prior Dom-Cursor) Pos) pos-enlace)
          )
         (t 
          (do (( i Pos  (incf i)) ( j Pos  (decf j))) (nil)
            (when (aref (cursor-exist Dom-Cursor) i) (return (setf pos-enlace i adelante t )))
            )
          (if adelante 
            (setf 
             Prior-Cas (aref (cursor-prior Dom-Cursor) pos-enlace) 
             (aref (cursor-prior Dom-Cursor) pos-enlace) Pos
             (aref (cursor-next Dom-Cursor) Prior-Cas) Pos
             (aref (cursor-next Dom-Cursor) Pos) pos-enlace
             (aref (cursor-prior Dom-Cursor) Pos) Prior-Cas
             )
            (setf 
             Next-Cas (aref (cursor-next Dom-Cursor) pos-enlace)
             (aref (cursor-next Dom-Cursor) pos-enlace) Pos
             (aref (cursor-prior Dom-Cursor) Next-Cas) Pos-enlace
             (aref (cursor-next Dom-Cursor) Pos) Next-Cas
             (aref (cursor-prior Dom-Cursor) Pos) pos-enlace
             )
            )
          )
         )
        (setf (aref (cursor-exist Dom-Cursor) Pos) t
              (aref (cursor-vect Dom-Cursor) Pos) Elem)
        (incf (cursor-Cardinal Dom-Cursor))
        ) 
      )
    )
  Dom-cursor
  )



(defun Rev-cursor ( Dom-cursor Pos )
"Undeletes the element index 'Pos' from 'Dom-Cursor'"
  ;(when (aref (cursor-exist Dom-Cursor) Pos) (break))
  (when (not (aref (cursor-exist Dom-Cursor) Pos))
    (if (cursor-vacio Dom-Cursor) 
      (progn  
        (setf (aref (cursor-exist Dom-Cursor) Pos) t
              (cursor-primero Dom-Cursor) Pos
              (cursor-ultimo Dom-Cursor) Pos
              (cursor-vacio Dom-Cursor) NIL
              ) 
        (incf (cursor-Cardinal Dom-Cursor))
        )
      (let ( pos-enlace adelante Prior-Cas Next-Cas )
        (cond 
         ((< Pos (cursor-primero Dom-Cursor))
          (setf  pos-enlace (cursor-primero Dom-Cursor)
                 (cursor-primero Dom-Cursor) Pos
                 (aref (cursor-prior Dom-Cursor) pos-enlace) Pos
                 (aref (cursor-next Dom-Cursor) Pos) pos-enlace 
                 )
          )
         
         ((> Pos (cursor-ultimo Dom-Cursor)) 
          (setf pos-enlace (cursor-ultimo Dom-Cursor)
                (cursor-ultimo Dom-Cursor) Pos
                (aref (cursor-next Dom-Cursor) pos-enlace) Pos
                (aref (cursor-prior Dom-Cursor) Pos) pos-enlace 
                )
          )
         (t 
          (do  (( i Pos  (incf i )) ) (nil)
            (when (aref (cursor-exist Dom-Cursor) i) (return (setf pos-enlace i adelante t )))
            )
          (if adelante 
            (setf 
             Prior-Cas (aref (cursor-prior Dom-Cursor) pos-enlace) 
             (aref (cursor-prior Dom-Cursor) pos-enlace) Pos
             (aref (cursor-next Dom-Cursor) Prior-Cas) Pos
             (aref (cursor-next Dom-Cursor) Pos) pos-enlace
             (aref (cursor-prior Dom-Cursor) Pos) Prior-Cas
             )
            (setf 
             Next-Cas (aref (cursor-next Dom-Cursor) pos-enlace)
             (aref (cursor-next Dom-Cursor) pos-enlace) Pos
             (aref (cursor-prior Dom-Cursor) Next-Cas) Pos-enlace
             (aref (cursor-next Dom-Cursor) Pos) Next-Cas
             (aref (cursor-prior Dom-Cursor) Pos) pos-enlace
             )
            )
          )
         )
        (setf (aref (cursor-exist Dom-Cursor) Pos) t)
        (incf (cursor-Cardinal Dom-Cursor))
        ) 
      )
    )
  Dom-cursor
  )



(defun cursor-lista ( Dom-Cursor &AUX i salga list-temp)
  "Converts 'Dom-Cursor' to a list"
  (setf i (primero-cursor Dom-Cursor ) salga NIL )
  (when (not(vacio-cursor Dom-Cursor)) 
    (do   () (salga)
      (when (= i (ultimo-cursor Dom-Cursor)) (setf salga t))
      (push   (Info-Cursor Dom-Cursor i ) list-temp)
      (setf i (next-cursor Dom-Cursor i))
      ))
  (reverse list-temp)
  )

(defun cursor-index-lista ( Dom-Cursor &AUX i salga list-temp)
  "Returns the index list of 'Dom-Cursor'"
  (setf i (primero-cursor Dom-Cursor ) salga NIL )
  (when (not(vacio-cursor Dom-Cursor)) 
    (do   () (salga)
      (when (= i (ultimo-cursor Dom-Cursor)) (setf salga t))
      (push   i list-temp)
      (setf i (next-cursor Dom-Cursor i))
      ))
  (reverse list-temp)
  )

(defun Imprima-cursor ( Dom-Cursor &AUX i salga list-temp)
  "Prints 'Dom-Cursor'"
  (setf i (primero-cursor Dom-Cursor ) salga NIL )
  (when (not(vacio-cursor Dom-Cursor)) 
    (do   () (salga)
      (when (= i (ultimo-cursor Dom-Cursor)) (setf salga t))
      (push   (Info-Cursor Dom-Cursor i ) list-temp)
      (setf i (next-cursor Dom-Cursor i))
      ))
  (print (reverse list-temp))
  )  

(defun cursor-jer-to-list-aux ( Dom-Cursor-jer Izq Der &AUX i salga  Dom-cursor)
  "Translates the tree-structured domain 'Dom-Cursor-jer' into a list"
  (when Dom-Cursor-jer
    (setf Dom-cursor (car Dom-Cursor-jer))
    (setf i Izq salga NIL )
    (when (not (vacio-cursor Dom-Cursor)) 
      (do    ((list-temp NIL)) (salga (reverse list-temp))
        (when (= i Der) (setf salga t))
        (if (cdr Dom-Cursor-jer) 
          (push  (list (Info-Cursor Dom-Cursor i) 
                       (cursor-jer-to-list-aux (cdr Dom-Cursor-jer) (Hijo-Izq-Cursor Dom-Cursor i)
                                               (Hijo-Der-Cursor Dom-Cursor i))) list-temp )
          (push  (Info-Cursor Dom-Cursor i ) list-temp)
          )
        (setf i (next-cursor Dom-Cursor i))
        )
      )
    ) 
  )

(defun cursor-jer-to-list ( Dom-Cursor-jer )
"Translates the tree-structured domain 'Dom-Cursor-jer' into a list"
  (cursor-jer-to-list-aux Dom-Cursor-jer (Primero-Cursor (car Dom-Cursor-jer))  
                          (Ultimo-Cursor  (car Dom-Cursor-jer)))
  )

(defun Existe-Cursor ( Dom-Cursor Pos)
  "Checks if the element indexed by 'Pos' exists in 'Dom-Cursor'"
  (aref (cursor-exist Dom-Cursor) pos)
  )

(defun Info-Cursor ( Dom-Cursor Pos)
  "Returns the element indexed by 'Pos' in 'Dom-Cursor'"
  (aref (cursor-vect Dom-Cursor) pos)
  ) 

(defun next-cursor ( Dom-Cursor pos)
  "Returns the next index of the index 'pos' in 'Dom-Cursor'"
  (if (or (null pos) (= pos -1)) (primero-cursor Dom-cursor)
      (aref (cursor-next Dom-Cursor) pos)
      )
  )

(defun prior-cursor ( Dom-Cursor pos)
  "Returns the prior index of the index 'pos'  in 'Dom-Cursor'"
  (aref (cursor-prior Dom-Cursor) pos)
  )

(defun ultimo-cursor ( Dom-Cursor )
  "Returns the last index of 'Dom-Cursor'"
  (cursor-ultimo Dom-Cursor)
  )

(defun primero-cursor ( Dom-Cursor )
  "Returns the first index of 'Dom-Cursor'"
  (cursor-primero Dom-Cursor)
  )

(defun vacio-cursor ( Dom-Cursor )
  "Checks if 'Dom-Cursor' is empty"
  (cursor-vacio Dom-Cursor)
  )

(defun tama-cursor ( Dom-Cursor )
  "Returns the size of Dom-Cursor"
  (cursor-cardinal Dom-Cursor)
  )



(defun trans-cursor-jer ( DOM  &AUX DOMV )
  "Builds a list of tree-structured domains of the lists included in 'DOM'"
  (setf DOMV NIL)
  (dolist (lis DOM DOMV) 
    (unless lis (print "There is not solution") (abort))
    (push (crear-dom-cursor-jer lis) DOMV ) )
  (reverse DOMV)
  )

(defun trans-cursor-jer-array ( DOM n &AUX (DOMV (make-array n :adjustable t))  (i 0) )
   "Builds an array of tree-structured domains of the lists included in 'DOM'. n is the 
    number of domains"
  (dolist (lis DOM) 
    (unless lis (print "There is not solution") (abort))
    (setf (aref DOMV i) (crear-dom-cursor-jer lis))
    (incf i)
    )
  DOMV
  )

(defun Add-cursor-jer-array ( DOM n DOMV &AUX (i n))
   "Adds to DOMV 'an array of tree-structured domains' the domains included in 'DOM'. n is the 
    number of domains in DOMV"
  (dolist (lis DOM) 
    (unless lis (print "There is not solution") (abort))
    (setf (aref DOMV i) (crear-dom-cursor-jer lis))
    (incf i)
    )
  DOMV
  )


(defun domcursor-domlista ( DOM &AUX DOML)
  "Translates 'DOM' into a list"
  (dolist (D DOM) (push (cursor-lista D) DOML) )
  (reverse DOML))


(defun print-dom (DOM )
  "Prints the list of cursors in 'DOM'"
  (dolist (D DOM)  (imprima-cursor D) )
  )

(defun dom-jer-list (DOM )
  "Translates a tree-structured domain 'DOM' into a list"
  (dolist (D DOM)  (cursor-jer-to-list D) )
  )

(defun print-dom-jer (DOM )
  "Prints the tree-structured domains included in 'DOM'" 
  (dolist (D DOM) (print (cursor-jer-to-list D)))
  )

(defun max-size-curs ( DOM )
  "Returns the maximal size of tree-structured domains included in 'DOM'"
  (let ((maxi 0))
    (dolist (domi DOM maxi)
      (let ((Size-domi (tama-cursor domi )))
        (setf maxi (MAX maxi Size-DOMi)
              )
        )
      )
    ))

(defun total-size-cur ( DOM )
  "Returns the size of the tree-structured domain 'DOM'"
  (let ((Size 0))
    (dolist (domi DOM Size)
      (incf Size (tama-cursor domi ))
      )
    )
  )

(defun total-size-cur-jer ( DOM )
  "Returns the summation of sizes of tree-structured domains included in 'DOM'"
  (let ((Size 0))
    (dolist (domi DOM Size)
      (incf Size (+ (tama-cursor (first domi )) (tama-cursor (second domi )) ))
      )
    )
  )