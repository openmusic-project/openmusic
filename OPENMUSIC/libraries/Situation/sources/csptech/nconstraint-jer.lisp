                                     
(in-package :cl-user)

(defstruct N-Constraint
  "Defines a n-ary constraint. 'Variables' is an ordered list of
indexes denoting the variables on the constraint. 'Constraint'
is the function denoting the constraint. e.g.
In Menor(x1,x2),
Variables= (1,2) and Constraint = #'< )"
  (Variables )
  (Constraint  )
  )
 


(defun Chequeo-constraint ( arc n  ) ;&AUX (Salida t))
  "Checks if the arc 'arc' has valid indexes. 0 and n are the minimal and maximal indexes
   respectively"
  (dolist  (x (cdr arc) t) (when (>= (first x) n) (return  NIL)))
)

(defun crear-n-constraint-jer ( arc )
  "Builds the constraint data-structure of arc"
  (make-N-constraint :constraint (first arc)
                                   :Variables (cdr arc)
                                   )
  )

(defun reduccion-unaria ( const Level Di &AUX DiOut Di-1 )
  "Reduces the domain 'Di' by the application of 'const' at level 'Level'"
  ;(printl "reduccion unaria with" "const:" const "Level:" Level "length Di" (length-tree Di))
  (if (zerop Level) (filter (function (lambda (x) (funcall const (car x)))) Di)
      (dolist  ( v Di (reverse DiOut))
        (setf Di-1 (filter const (second v)))
        (when Di-1 (push (list (first v) Di-1) DiOut))  
        ) 
      )
  )
;;(untrace)
   


(defun crear-multigrafo-jer+ ( G DOM &AUX Gout Daux (n (length DOM)) )
  "Given the set of domains 'DOM' and the list representing the constraint graph
   'G', builds the multi-graph representation of the CSP"
  (setf Daux DOM)
  (dolist ( arc G (values  (nreverse Gout)  Daux))
    (cond 
     ((and (= (list-length arc) 2) (= (first (second arc)) 0))
        (setf (nth 0 Daux)  (reduccion-unaria (first arc) (second (second arc)) (nth 0 Daux))) 
        (unless (nth 0 Daux)
                (print "There is no solution: Unary constraint")
                (abort ))
      )
     ((and (= (list-length arc) 2) (Chequeo-constraint arc n))
      (let* ( (uf (first arc)) 
              (f (eval `(function (lambda (x y) (declare (ignore x)) (funcall ,uf y))))))
        (push  (list  f (list 0 0) (second arc))  Gout)
        )
      )
     ( (Chequeo-constraint arc n)   (push arc Gout))
     )
    )
  )

(defun adjoint-multigrafo-jer+ ( G DOM n &AUX Gout Daux )
  "Given the set of domains 'DOM' and the list representing the constraint graph
   'G', builds the multi-graph representation of the CSP. This is a modified form
   of the 'crear-multigrafo-jer+' function to adjoint sup-problems."
  (setf Daux DOM)
  (dolist ( arc G (values  (reverse Gout)  Daux))
    (cond 
     ((and (= (list-length arc) 2) (= (first (second arc)) 0))
        (setf (nth 0 Daux)  (reduccion-unaria (first arc) (second (second arc)) (nth 0 Daux))) 
        (unless (nth 0 Daux)
                (print "There is no solution: Unary constraint")
                (abort ))
      )
     ((and (= (list-length arc) 2) (Chequeo-constraint arc n))
      (let* ( (uf (first arc)) 
              (f (eval `(function (lambda (x y) (values (funcall ,uf y) x))))))
        (push  (list  f (list 0 0) (second arc)) Gout)
        )
      )
     ( (Chequeo-constraint arc n)   (push arc Gout))
     )
    )
  )



(defun Nivel-of-var ( j Variables )
  "Returns the level of variable 'j' in 'Variables'. e.g :
if j=1 and Variables = ( (2 1) (1 0)) Nivel-of-var will return 0"
  (dolist (x Variables)
    (when (= j (car x)) (return (second x)))
    )
  )


(defun list-values-jer ( indices vector i j v w  &AUX LstOut)
  "Returns the values assigned in vector to the variables indexed by 'indices' and i,j,v,w."
  (dolist ( ind indices (reverse LstOut))
    (cond ((= (first ind) i)  (push v LstOut))
          ((= (first ind) j)  (push w LstOut))
          (t (push (nth (second ind) (aref vector (first ind))) LstOut)) 
          )
    )
  )


(defun arc-evaluables-jer+ ( Grafo  i j Level-i Level-j) 
  "Returns the arcs evaluables in 'Grafo' having i,j at level 'Level-i' and 'Level-j' respectively"
  (when (aref Grafo i j)
    (if Level-i
      (nth (+ (* 2 Level-i) Level-j) (aref Grafo i j))
      (append (nth Level-j (aref Grafo i j)) (nth (+ 2 Level-j) (aref Grafo i j)))
      )
    )
  )


(defun max-level-instance (pair) (= (second pair) 1))

(defun get-relevant-constraint (grafo i j level-i level-j vars)
  (let ((entry (arc-evaluables-jer+ grafo i j level-i level-j)) cnstrs)
    (when entry
        (dolist (c entry cnstrs)
          (when (vars-instanciated? (n-constraint-variables c) i j level-i level-j vars)
            (push c cnstrs))))))

(defun vars-instanciated? (c-vars i j level-i level-j vars)
    (dolist (var c-vars t)
      (cond ((= (first var) i)
             (unless (= (second var) level-i) (return nil)))
            ((= (first var) j)
             (unless (= (second var) level-j) (return nil)))
            ;;((= (first var) x) t)
            ;;((= (first var) y) t)
            ((member (first var) vars :key #'first :test #'=) t)
            (t (return nil)))))

(defun arc-evaluables-jer-noOrden ( Grafo  i j Level-i Level-j &optional vardone)
  "Returns the arcs evaluables in 'Grafo' having i,j at level 'Level-i' and 'Level-j' respectively"
  (let (x y level-x level-y)
    (if (> i j)
      (setf x j y i level-x level-j level-y level-i)
      (setf x i y j level-x level-i level-y level-j)
      )
    (get-relevant-constraint grafo x y level-x level-y vardone)))


(defun Mayor+ ( L &AUX (Max (list -1 NIL)))
  "Returns the maximal index of 'L'"
  (dolist (x L)
    (when (> (first x) (first Max)) (setf Max x ))
    )
  Max
  )



 (defun Make-Matrix-n-jer+ ( Grafo n &AUX (GMatrix (make-array (list n n) :initial-element NIL :adjustable t )) 
                                  (arcs-from (make-array n :initial-element NIL :adjustable t)))
  "Builds a Matrix representation of the constraint graph"
  (dolist  (arc Grafo )
    (let* ((xj (Mayor+ (cdr arc)))
           (xi (Mayor+ (remove xj (cdr arc) :test (function (lambda (x y) (= (first x) (first y)))))
                       )))
      (unless (member (first xj) (aref  arcs-from (first xi)) :test #'=)
        (push (first xj) (aref arcs-from (first xi)) ))
      (unless (aref GMatrix (first xi) (first xj)) (setf (aref GMatrix (first xi) (first xj)) (list NIL NIL NIL NIL)))
      (push (crear-n-constraint-jer arc) (nth (+ (* 2 (second xi)) (second xj)) (aref GMatrix (first xi) (first xj))))
      )
    )
  (dotimes (i n)
    (setf (aref arcs-from i) (sort (aref arcs-from i) #'<))
    )
  (values GMatrix arcs-from)
  )

(defun Adjoint-Matrix-n-jer+ ( Grafo n GMatrix arcs-from )
  "Adds the constraints to Gmatrix and arcs-from"
  (dolist  (arc Grafo )
    (let* ((xj (Mayor+ (cdr arc)))
           (xi (Mayor+ (remove xj (cdr arc) :test (function (lambda (x y) (= (first x) (first y)))))
                       )))
      (unless (member (first xj) (aref  arcs-from (first xi)) :test #'=)
        (push (first xj) (aref arcs-from (first xi)) ))
      (unless (aref GMatrix (first xi) (first xj)) (setf (aref GMatrix (first xi) (first xj)) (list NIL NIL NIL NIL)))
      (push (crear-n-constraint-jer arc) (nth (+ (* 2 (second xi)) (second xj)) (aref GMatrix (first xi) (first xj))))
      )
    )
  (dotimes (i n)
    (setf (aref arcs-from i) (sort (aref arcs-from i) #'<))
    )
  )


(defun test-rem-i-grafo ( i x ) (assoc i (cdr x)))
(defun neg-test-rem-i-grafo ( i x) (not (test-rem-i-grafo i x)))

(defun Delete-Arcs-from ( i Grafo GMatrix Arcs-from  )
  "Deletes the arcs from 'i' in 'Grafo'."
  (let ((Rem-Graph (remove i Grafo :test #'neg-test-rem-i-grafo)))
    (dolist (arc Rem-Graph )
      (let* ((xj (Mayor+ (cdr arc)))
             (xi (Mayor+ (remove xj (cdr arc) :test (function (lambda (x y) (= (first x) (first y)))))
                         )))
        
        (setf ;(aref arcs-from (first xi)) (remove i (aref arcs-from (first xi)))
              (nth (+ (* 2 (second xi)) (second xj)) (aref GMatrix (first xi) (first xj))) 
              (remove arc (nth (+ (* 2 (second xi)) (second xj)) (aref GMatrix (first xi) (first xj)))
                      :test #'(lambda (x y) (and (equal (first x) (N-Constraint-constraint y))
                               (equal (cdr x) (N-Constraint-variables y)))))
              
              )
    
        )
      
      )
    (values (remove i Grafo :test #'test-rem-i-grafo) Gmatrix Arcs-from)
    )
  )

