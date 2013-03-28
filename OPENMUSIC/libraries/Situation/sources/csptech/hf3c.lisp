;;; This file contains the constraint solver engine.
;;; Frank D. Valencia,16/10/96
;;; This is the version 1.1b of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda.

(in-package :cl-user)

(defconstant *interp-key* 'interp)
 

;;
;;Prints solutions
;;

(defvar *chords-problem?* nil)
(defvar *default-approximation* 1)

(defvar *user-given-hvar-heuristic* nil)

(defvar *domain-permutation* t)

(defun normal-form ( Instancias )
  (let ((Lins (if *chords-problem?* (coerce  instancias 'list) (cdr (coerce  instancias 'list))))
        Lout)
    (dolist (x Lins (nreverse Lout)) (push (second x) Lout))
    )
  )

#|
(defun print-num-sol(num-sol sol) 
  (prin1 "#")
  (prin1 num-sol)
  (prin1 " : ")
  (prin1 sol)
  (print " ")
  )
|#

(defun print-num-sol(num-sol sol) t)

;; Variable's heuristics

(defun hvar-simple( LSTVAR  &AUX pos) 
  (setf pos 0)
  (nth pos LSTVAR))

(defun hvar-random( LSTVAR   &AUX pos) 
  (setf pos (om::om-random-value (list-length LSTVAR)))
  (nth pos LSTVAR))

(defun hvar-max-dom( LSTVAR DOM ) 
  (let ( (maximo 0) var)
    (dolist (v LSTVAR var)
      (let ((Size-Dom-v (list-length (nth v DOM) )))
        (when (< maximo Size-Dom-v) (setf var v maximo Size-Dom-V)))
      )
    )
  )

(defun hvar-min-dom( LSTVAR  DOM )
  (let ( (min 10000000) var)
    (dolist (v LSTVAR var)
      (let ((Size-Dom-v (tama-cursor  (second (aref DOM v)) )))
        (when (> min Size-Dom-v) (setf var v min Size-Dom-V)))
      )
    )
  )


(defun hvar-simple-order (lstvar dom) dom (hvar-simple LSTVAR))

(setf *user-given-hvar-heuristic* #'hvar-simple-order)

(defun heuristico-var ( LSTVAR  DOM )
 (funcall *user-given-hvar-heuristic* lstvar dom)
  )

#|
(defun heuristico-var ( LSTVAR  DOM )
  DOM
  (hvar-simple LSTVAR)
  )



(defun heuristico-var ( LSTVAR  DOM )
  DOM
  (hvar-min-dom LSTVAR dom)
  )


;;;; Evaluates hierchycal n-ary constraints .

(defun Evaluar ( Rjk i v Instancias j w  &AUX (Salida t))
  (dolist (n-const Rjk)
    (incf fvtempos::constraint-checks)
    (unless (if i 
              (apply (n-constraint-constraint n-const) 
                     (list-values-jer (n-constraint-variables n-const) instancias  i j v w))
              (apply (n-constraint-constraint n-const) 
                     (list-values-jer (n-constraint-variables n-const) instancias -1 j v w))
              )
      (return (setf Salida NIL))
      )
    )
  Salida
  )
|#

(defun Evaluar ( Rjk i v Instancias j w  &AUX (Salida t))
  (dolist (n-const Rjk)
    (incf fvtempos::constraint-checks)
    (unless (if i 
              (apply (n-constraint-constraint n-const) 
                     (list-values-jer (n-constraint-variables n-const) instancias  i j v w))
              (apply (n-constraint-constraint n-const) 
                     (list-values-jer (n-constraint-variables n-const) instancias -1 j v w))
              )
      (if i (incf (aref fvtempos::constr-checks-table (min i j) (max i j))) (incf (aref fvtempos::constr-checks-table j j)))
      (return (setf Salida NIL))
      )
    )
  Salida
  )



(defun setf-M ( M i Level-i j Padre e )
    (setf  (aref M i Level-i j) (list Padre e) )
    )

(defun aref-M  ( M i Level-i j Padre  )
  (if (aref M i level-i j)
    (if (= (first (aref M i level-i j)) Padre) 
      (second (aref M i level-i j)) -1)
    -1)
  )


(defun aref-Binst ( Binst i  j &optional Level-i)
  (if Level-i (aref Binst i Level-i j) (append (aref Binst i 0 j) (aref Binst i 1 j)))
)

;; Finds the first value in Dj at level Level-j (having "Padre" as parent)  
;; compatible with previous instances.

#|
(defun Arrastre ( Level-j j Dj Padre
                          inicio hasta VARDONE-level Instancias M Binst Grafo 
                          &OPTIONAL i v  Level-i &AUX Lst-Eval  R viable k posj Flag 
                          )
  (if (Vacio-cursor Dj) (setf viable NIL posj hasta)
      (progn 
        (setf viable t)
        (when i (if (= Level-i 1) (progn (push (list i 0) VARDONE-Level) 
                                         (push (list i 1) VARDONE-Level))
                    (push (list i 0) VARDONE-Level))
              )
        (when VARDONE-Level (setf flag t k (pop VARDONE-Level)))
        (do (   (ind-j inicio (next-cursor Dj ind-j)))  ()
          (do () ((or (not Flag) (not (> ind-j (aref-M M (first k) (second k) j Padre))) ))
            (setf Lst-Eval (arc-evaluables-jer+ Grafo  (first k) j (second k)  Level-j))
            (when Lst-Eval  (push (list k Lst-Eval) R)
                  )
            (if (null VARDONE-Level) (setf Flag NIL) 
                (setf k (Pop VARDONE-Level))
                ) 
            )
          (setf viable t)
          (dolist (Rjk R)
            (if (Evaluar (second Rjk) i v Instancias j (info-cursor Dj ind-j)) 
              (setf-M  M (first (first Rjk)) (second (first Rjk)) j  Padre ind-j)
              (progn
                (push  (list ind-j Level-j) (aref BInst (caar Rjk) (second (first Rjk)) j))
                (setf Dj (Borrar-dom-cursor Dj ind-j) viable NIL) 
                (return)
                )
              )
            )
          (when (or viable (= ind-j hasta)) (return (setf posj ind-j) ))
          )
        )
      )
  (values viable posj)
  )
|#

;;corrects a bug: constraint should not be invoked when level-i does not agree
;; [Camilo 25/05/98]
;;this correction is not needed anymore. Constraints are verified for relevance [Camilo 03/06/98]

(defun Arrastre ( Level-j j Dj Padre
                          inicio hasta VARDONE-level Instancias M Binst Grafo 
                          &OPTIONAL i v  Level-i &AUX Lst-Eval  R viable k posj Flag 
                          )
  (if (Vacio-cursor Dj) (setf viable NIL posj hasta)
      (progn 
        (setf viable t)
        (when i (if (= Level-i 1) (progn ;(push (list i 0) VARDONE-Level) 
                                         (push (list i 1) VARDONE-Level))
                    (push (list i 0) VARDONE-Level))
              )
        (when VARDONE-Level (setf flag t k (pop VARDONE-Level)))
        (do (   (ind-j inicio (next-cursor Dj ind-j)))  (nil)
          (do () ((or (not Flag) (not (> ind-j (aref-M M (first k) (second k) j Padre))) ))
            (setf Lst-Eval (arc-evaluables-jer-noOrden Grafo  (first k) j (second k)  Level-j vardone-level))
            ;;(when (and i (> level-i (second k)) (return)))   ;;no checking when incompatible levels
            (when Lst-Eval  (push (list k Lst-Eval) R)
                  )
            (if (null VARDONE-Level) (setf Flag NIL) 
                (setf k (Pop VARDONE-Level))
                ) 
            )
          (setf viable t)
          (dolist (Rjk R)
            (if (Evaluar (second Rjk) i v Instancias j (info-cursor Dj ind-j)) 
              (setf-M  M (first (first Rjk)) (second (first Rjk)) j  Padre ind-j)
              (progn
                (push  (list ind-j Level-j) (aref BInst (caar Rjk) (second (first Rjk)) j))
                (setf Dj (Borrar-dom-cursor Dj ind-j) viable NIL) 
                ;;(break)
                (return)
                )
              ) ;;(break)
            )
          (when (or viable (= ind-j hasta)) (return (setf posj ind-j) ))
          )
        )
      )
  (values viable posj)
  )


;;  
;; Finds the first value at level 1 and level 0 in Dj compatible with previous instances
;;

(defun Ejecutar-Rest-Jer (Level-i j v i  VARDONE  Instancias M  BInst Grafo DOM 
                                  &AUX (viable NIL) Dj Dj1 Dj2)
  (setf Dj (aref DOM j)
        Dj1 (first Dj)
        Dj2 (second Dj))
  (do () (nil)
    (multiple-value-bind (viable1 posj1) (Arrastre  0 j Dj1
                                                    fvtempos::RAIZ
                                                    (Primero-cursor  Dj1) 
                                                    (Ultimo-Cursor  Dj1)
                                                    VARDONE  
                                                    Instancias M  Binst Grafo  i v Level-i)
      (unless viable1 (return ))
      (multiple-value-bind (viable2 posj2) (Arrastre  1 j Dj2
                                                      posj1
                                                      (Hijo-izq-Cursor  Dj1 posj1 )
                                                      (Hijo-Der-Cursor Dj1 posj1)
                                                      VARDONE  
                                                      Instancias M Binst Grafo i v Level-i)
        (setf (aref (cursor-hijo-izq Dj1) posj1) posj2)        
        (if viable2 
          (return (setf viable t))
          (setf Dj1 (Borrar-dom-cursor Dj1 posj1) )
          )
        )
      )
    )
  viable
  )  

;; 
;; Reduces (pruning) the domains of futures variables (e.g.  uninstanciated variables)
;; as standard Forward Checking

(defun FC ( Level-i  v i LSTVAR VARDONE  Instancias M BInst Grafo DOM 
                     k-prunning &AUX viable DomFalla )
  (setf viable t) 
  (dolist (j LSTVAR )
    (if (numberp  k-prunning) (progn (when (zerop  k-prunning) (return (setf DomFalla j))) (decf  k-prunning)))
    (setf viable (Ejecutar-Rest-Jer Level-i j v i VARDONE Instancias M  BInst Grafo DOM))
    (unless viable
      (return (setf DomFalla j)))
    )
  (values viable DomFalla) 
  )

;;
;; Undoes the FC's effects  
;;

(defun FC-1 ( i pos-falla LSTVAR M BInst DOM &OPTIONAL Level-i &AUX Dj ) 
  (dolist  (j LSTVAR )
    (setf Dj (aref DOM j) )
    (dolist (index (aref-Binst Binst i j Level-i))
      (setf Dj (Rev-Cursor-Jer Dj index))
      )
    (if Level-i 
      (progn 
        (setf (aref M i Level-i j) NIL
              (aref Binst i Level-i j) NIL)
        )
      (progn 
        (setf (aref M i 0 j) NIL  
              (aref M i 1 j) NIL
              (aref Binst i 0 j) NIL
              (aref Binst i 1 j) NIL)
        )
      )
    (when (and Pos-falla (= j Pos-falla)) (return))
    )
  )

;;
;; Extends the current instance (current solution) trying to assign a value at level 0 to variable i 
;;  and a value at level 1, compatibles with previous instance and futures variables.
;;

(defun extender-solucion ( i VAR-LEFT VAR-DONE Instancias M  Binst Grafo DOM  k-prunning 
                             &AUX Di Di1 Di2  posi1 posi2 xi xi1 xi2 Flag Flag2 ultimo1 
                             (viable NIL))
  
  (setf Di (aref DOM i) Di1 (first Di) Di2 (second Di))
  (unless (vacio-cursor Di1)
    (setf ultimo1 (Ultimo-Cursor Di1))
    (setf posi1 (Primero-Cursor  Di1 ))
    (do () (nil)
      (multiple-value-bind ( Ok3 next ) (Arrastre  0  i Di1
                                                   fvtempos::RAIZ
                                                   posi1 
                                                   Ultimo1
                                                   VAR-DONE 
                                                   Instancias M Binst Grafo  )
        
        (if Ok3 (setf posi1 next) (return))
        )
      (setf xi1 (Info-Cursor Di1  posi1 ))
      (multiple-value-bind (Ok DomFalla)  (FC 0 xi1 i VAR-LEFT VAR-DONE Instancias M Binst Grafo DOM  k-prunning)
        (setf Flag NIL)
        (setf Flag2 t)
        (when  Ok
          (setf posi2 (Hijo-izq-cursor  Di1 posi1))
          (do   () (nil)
            (multiple-value-bind ( Ok2 next ) (Arrastre  1 i Di2
                                                         posi1
                                                         posi2 
                                                         (Hijo-Der-Cursor Di1 posi1)
                                                         VAR-DONE  
                                                         Instancias M Binst Grafo )
              (setf posi2 next
                    (aref (cursor-hijo-izq Di1) posi1) next)
              (unless Ok2  (return (setf Flag2 NIL)))
              )
            (setf Di2 (Borrar-dom-cursor Di2 posi2))
            (push  (list posi2 1) (aref BInst i 0 i))
            (setf xi2 (Info-cursor Di2 posi2)) 
            (multiple-value-bind (Ok1 DomFalla)  (FC 1 xi2 i VAR-LEFT VAR-DONE Instancias M Binst Grafo DOM  k-prunning)
              (if Ok1 (return (progn 
                                (setf Flag t)
                                (when (< posi2 (Hijo-Der-Cursor Di1 posi1)) 
                                  (setf (aref (cursor-hijo-izq Di1) posi1) (next-cursor Di2 posi2))
                                  )
                                )
                              )
                  (FC-1 i DomFalla VAR-LEFT M BInst DOM 1))
              )
            (when (= posi2 (Hijo-Der-Cursor Di1 posi1)) 
              (return )
              )
            (setf posi2 (next-cursor Di2 posi2))
            )
          )
        (when (or (not Ok) (= posi2 (Hijo-Der-Cursor Di1 posi1)))
          (setf Di1 (Borrar-dom-cursor Di1 posi1))
          (when Flag2 (push  (list posi1 0) (aref BInst i 0 i)))
          )
        (when Flag (return (setf xi (list xi1 xi2)  viable t)))
        (FC-1 i DomFalla VAR-LEFT M BInst DOM 0)
        (when (= posi1 ultimo1) (return))
        (setf posi1 (next-cursor Di1 posi1))
        )
      )
    )
  (values xi viable )
  )



;;; The function appearing below are used to check if a solution is OK.
;;;
;;;

(defun vals ( Instancias Vars )
  (mapcar #'(lambda (x) (nth (second x) (aref Instancias (first x)))) Vars) 
  )
(defun bad-inst? ( Grafo Instancias &AUX (Out nil) (n (array-total-size Instancias)))
  (dolist (arc Grafo Out)
    (when (Chequeo-constraint arc n)
      (unless (apply (first arc) (vals Instancias (cdr arc))) (push arc Out))
      )
    ))


;(bad-inst? G (coerce (mapcar #'(lambda (x) (if (atom x) (list x x) (list (apply '+ x) x))) nil) 'array))

(defun print-chord (i)
  (when (and (par i) (not (zerop i))) 
    ;;;(prin1 'ch )    ;;;too much printing (Camilo, Jan 15, 1997)
    (prin1 (/ (- i 2) 2) ) (format t " ")
    ;;;;(prin1 '->) (format t " "))
  ))

;;;
;;; Backtrack search using "First Found Forward Checking" (F3C) algorithm developed  
;;; by Rueda&Valencia. A non-recursive version is used to improve the spatial complexity.
;;; 

(defun Solucion   ( VAR-LEFT VAR-DONE  instancias M  Binst Grafo-test Grafo Arcs-from  DOM  k-prunning 
                             &OPTIONAL num-solutions cont-sol Go-Back
                             &AUX i Salida Solution-set) 
  (setf Salida NIL)
  (setf fvtempos::fun-proxima #'(lambda () 
                                  (values "Previous" fvtempos::Instancias M  Binst Grafo-test Grafo 
                                          Arcs-from DOM  k-prunning num-solutions cont-sol fvtempos::current-var
                                          )))
  
  (do () (nil)
    (if (not Go-Back)
      (BLOCK SI
        (when (null VAR-LEFT) 
          ;(print "Solution:" )
          ;(let ((Bad-Instance? (bad-inst?  Grafo-test Instancias))) (when Bad-Instance? (printl "Instanciation does not satisfy:" Bad-Instance?) (prin1 "")))
          (let ((normal-f (normal-form instancias))) (print-num-sol cont-sol normal-f)
               (push normal-f Solution-Set))
          (setf Salida 1)
          (cond ((null num-solutions) (return))
                ((numberp num-solutions)
                 (if  (> num-solutions cont-sol)
                   (RETURN-FROM SI (PROGN (setf Go-Back t) (incf cont-sol )))
                   (return)
                   ))
                ((equal num-solutions 'TODAS) 
                 (RETURN-FROM SI  (PROGN (setf Go-Back t) (incf cont-sol ))))
                )
          )
        
        (setf  i (heuristico-var VAR-LEFT DOM)
               VAR-LEFT (delete i VAR-LEFT)
               )
        ;(print-chord i)
        
        ;;(multiple-value-bind (xi viable ) (extender-solucion  i (aref Arcs-from i) VAR-DONE  Instancias M Binst Grafo DOM  k-prunning)
        (multiple-value-bind (xi viable ) (extender-solucion  i var-left VAR-DONE  Instancias M Binst Grafo DOM  k-prunning)
          (if viable (progn 
                       (setf (aref instancias i ) xi) ;;(print instancias)
                       (push (list i 0) VAR-DONE)
                       (push (list i 1) VAR-DONE)
                       (setf fvtempos::current-var i)
                       )
              (progn  
                (FC-1 i NIL (cons i NIL) M BInst  DOM)
                (setf Go-Back t)
                (push i VAR-LEFT)
                (setf (aref instancias i ) NIL)
                )
              )
          )
        )
      (progn
        (when (null VAR-DONE) (return (setf Salida 2)))
        (setf i (caar VAR-DONE)
              VAR-DONE (cddr VAR-DONE) fvtempos::current-var i)
        
        (print-chord i) 
        
        ;;(FC-1 i NIL (aref Arcs-from i) M BInst  DOM)
        (FC-1 i NIL var-left M BInst  DOM)
        ;;(multiple-value-bind (xi viable) (extender-solucion  i (aref Arcs-from i) VAR-DONE Instancias M  Binst Grafo DOM  k-prunning)
        (multiple-value-bind (xi viable) (extender-solucion  i var-left VAR-DONE Instancias M  Binst Grafo DOM  k-prunning)
          (if viable (progn 
                       (setf (aref instancias i ) xi)
                       (push (list i 0) VAR-DONE)
                       (push (list i 1) VAR-DONE)
                       (setf Go-Back NIL)
                       )
              (progn
                (FC-1 i NIL (cons i nil) M BInst  DOM)
                (push i VAR-LEFT)
                (setf (aref instancias i ) NIL)
                )
              )
          )
        )
      )
    )     
  (unless (= salida 1) (print "There is no solution"))  
  (nreverse Solution-set)
  
  )

;;;
;;; Initialices Data Structures for Backtracking Algorithm
;;;

(defun Change_Level ( Grafo &OPTIONAL (dx 1) &AUX Gout )
  (dolist (Const Grafo (reverse Gout))
    (if (listp Const)
      (let (Q var level)
        (dolist (pair (cdr Const))
          (setf var (first pair) level (second pair))
          (when (par var) (setf level 1))
          (incf var dx)
          (push (list var level) Q)
          )
        (push (cons (first Const) (reverse Q)) Gout)
        )
      (dolist (Constraint (funcall Const))
        (let (Q var level)
          (dolist (pair (cdr Constraint))
            (setf var (first pair) level (second pair))
            (when (par var) (setf level 1))
            (incf var dx)
            (push (list var level) Q)
            )
          (push (cons (first Constraint) (reverse Q)) Gout)
          )
        )
      )
    )
  )


;;;
;;; Restores the data structure and rebuild them to search further solutions 
;;;


(defun Restore ( i n current-var instancias Grafo GMatrix Arcs-from M BInst  DOM 
                   &OPTIONAL Chord-case Delete-case &AUX  VAR-DONE VAR-LEFT)
  (dotimes (j n VAR-DONE) (push (list j 0) VAR-DONE) (push (list j 1) VAR-DONE))
  (when (and VAR-DONE (or (< i 1) (> i current-var))) 
    (if (< current-var n) (setf i current-var)
        (setf i (1- n)))
    )
  (do (( k (caar VAR-DONE) (caar VAR-DONE) ) ) ((or (null VAR-DONE) (= i k) ))
    (FC-1  k NIL (cons k (aref Arcs-from k)) M BInst  DOM)
    (setf (aref instancias k ) NIL)
    (setf VAR-DONE (cddr VAR-DONE))
    (push k VAR-LEFT)
    )
 
  (when Delete-Case
    (if Chord-case  
      (progn (setf Grafo (Delete-Arcs-from (1- i) Grafo GMatrix Arcs-from))
             (setf Grafo (Delete-Arcs-from i Grafo GMatrix Arcs-from)))
      (setf Grafo (Delete-Arcs-from i Grafo GMatrix Arcs-from))
      )
    )
  (list VAR-DONE VAR-LEFT Grafo )
  )

;;
;; The functions appearing below are used to adjoint new subproblems to a previously solved problem
;;
  
(defun adjoint-problem-constructor (m-in NewDOM NewGrafo)
  (let ((n (first (array-dimensions fvtempos::Instancias)))
        (m (* 2 m-in)) )
    (setf fvtempos::Instancias (adjust-array fvtempos::Instancias (list (+ n m) ) :initial-element NIL)
          fvtempos::Primer-Soporte (adjust-array fvtempos::Primer-Soporte (list (+ n m) 2 (+ n m)) :initial-element NIL)
          fvtempos::Binst (adjust-array fvtempos::Binst (list (+ n m) 2 (+ n m)) :initial-element NIL)
          fvtempos::DOMCURS (adjust-array fvtempos::DOMCURS (list (+ n m)) :initial-element NIL)
          fvtempos::Arcs-from (adjust-array fvtempos::Arcs-from (list (+ n m)) :initial-element NIL)
          fvtempos::GM (adjust-array fvtempos::GM (list (+ n m) (+ n m)) :initial-element NIL)
          )
    (setf NewGrafo (adjoint-Multigrafo-jer+ NewGrafo NewDOM (+ n m)))
    (Add-cursor-jer-array NewDOM n fvtempos::DOMCURS)
    (Adjoint-Matrix-n-jer+  NewGrafo (+ n m) fvtempos::GM fvtempos::Arcs-from)
    (setf fvtempos::G (append fvtempos::G NewGrafo))
    (incf fvtempos::current-var)
    (append (interv-lista fvtempos::current-var (1- n)) (interv-lista n (+ n m)))
    )
  )

(defun adjoint-problem ( n-chords Densities Intervals Ambitus Constraints  
                                  &OPTIONAL fixed-chords 
                                  )
  "Adds a sub-problem to the current solved problem. 'n-chords' is the number of 
chords of the sub-problem.  'Densities', 'Intervals', 'Ambitus' and 'fixed-chords' are given in terms
of the sub-problem. 'Constraints' is given in terms of the whole problem. For instance
suppose that the previous solved problem has 20 chords. The first chord (i.e. chord 0) specified in Density
corresponds to chord 21 and so on. This in contrast with 'Constraint' parameter, where chord 21
corresponds to chord 21 in the whole problem not to chord 41.
"
  ;;;Refining a making Densities,Intervals, Ambitus and others inputs.
  (if (functionp fvtempos::fun-proxima)
    (progn
      (if (non-chord-domains? fixed-chords)
        (list n-chords (make-non-chord-domains n-chords fixed-chords) Constraints)
        (let ((fvtempos::MaxChords n-chords))
          (setf Densities (form-domain-dimension Densities n-chords))
          
          (setf Intervals (form-domain-dimension Intervals n-chords))
        
          (setf Ambitus (form-domain-dimension Ambitus n-chords t))
          
          ;;(when (flatp (first fixed-chords)) 
          ;;  (setf fixed-chords (list (list (Interv-lista 0 (1- n-chords)) fixed-chords)))
          ;;  )
          (unless (listp Constraints) (setf Constraints (list Constraints)))
          ;;(setf fixed-chords (mapcar #'(lambda (x) (if (numberp (car x)) (list (list (car x)) (second x)) x)) fixed-chords)) 
          
          (if (setf Ambitus (n-list Ambitus (1- n-chords)))
            (setf  Ambitus (complet-list Ambitus (last+ Ambitus) n-chords))
            (setf  Ambitus (complet-list Ambitus '(0 120) n-chords))
            )
          (if (setf  Intervals (n-list Intervals (1- n-chords)))
            (setf  Intervals (complet-list Intervals (last+ Intervals) n-chords))
            (setf  Intervals (complet-list Intervals *default-intervals* n-chords))
            )
          
          (multiple-value-bind (Domains chord-constraint) (make-domains  n-chords Intervals 
                                                                         (n-list Densities (1- n-chords)) Ambitus fixed-chords )
            (let ((initial-Constraints (change_level (append (Make_ambitus Ambitus) chord-constraint) 
                                                     (first (array-dimensions fvtempos::instancias))))
                  )
              (list n-chords Domains (append Initial-Constraints  (change_level Constraints)))
              )
            )
          )))
    (progn (print "You should  solve one problem before adding one!. See CSolver box") nil)
    )
  )


#+:OM
(om::defmethod! om::adjoined-problem ((n-chords number) (Densities list)
                          (Intervals list) (Ambitus  list) (Constraint t)
                          &OPTIONAL (fixed-chords  nil))
(adjoint-problem n-chords Densities Intervals Ambitus Constraint fixed-chords))
#+:OM
(om::defmethod! om::add-Csolver ((n-ch number) (Amb  list) (Dens t)
                                (vint list)  (Cnstr t) (hint list)
                                &OPTIONAL 
                                (data  nil)
                                )
  :initvals '(10 (48_72) (1) (4 7 11)  nil nil nil) 
  :indoc '("number of chords" "Ambitus" "densities" "vertical intervals"  "Constraints" "horizontal intervals"
           "Fixed chords data base")
  :icon 193
  :doc "Adds a sub-problem to the current solved problem. 'n-ch' is the number of 
chords of the sub-problem.  'Dens', 'vint', 'Amb' and 'data' are given in terms
of the sub-problem. 'Cnstr' and 'hint' are given in terms of the whole problem. For instance
suppose that the previous solved problem has 20 chords. The first chord (i.e. chord 0) specified in Density
corresponds to chord 21 and so on. This in contrast with 'Constraint' parameter, where chord 21
corresponds to chord 21 in the whole problem not to chord 41.
"
  (when hint
    (let ((fvtempos::MaxChords n-ch))
      (if (listp cnstr)
        (setf cnstr (cons (horizontal-intervals (fill-expression hint)) cnstr))
        (setf cnstr (list (horizontal-intervals (fill-expression hint)) cnstr))))) 
  (adjoint-problem n-ch Dens vint Amb cnstr data))

#+:PW
(pw::defunp  add-Csolver ((n-ch pw::fix (:value 10)) (Amb  pw::list    (:value '(0 120)))
                         (Dens pw::list  (:value '(1)))
                         (vint pw::list  (:value '(4 7 11)))
                         (Cnstr pw::list (:value '())) (hint pw::list  (:value '()))
                         &OPTIONAL 
                         (data  pw::list (:value '()))
                         ) list
  "Adds a sub-problem to the current solved problem. 'n-chords' is the number of 
chords of the sub-problem.  'Dens', 'vint', 'Amb' and 'data' are given in terms
of the sub-problem. 'Cnstr' and 'hint' are given in terms of the whole problem. For instance
suppose that the previous solved problem has 20 chords. The first chord (i.e. chord 0) specified in Density
corresponds to chord 21 and so on. This in contrast with 'Constraint' parameter, where chord 21
corresponds to chord 21 in the whole problem not to chord 41.
"
  (when hint
    (let ((fvtempos::MaxChords n-ch))
      (if (listp cnstr)
        (setf cnstr (cons (horizontal-intervals (fill-expression hint)) cnstr))
        (setf cnstr (list (horizontal-intervals (fill-expression hint)) cnstr)))))
  (adjoint-problem n-ch Dens vint Amb Cnstr data))

;;;
;;; Special addition of values in  Integers U {'todas} U {nil}   
;;;

(defun +symbol ( x y default-value) 
  (cond  ((and (numberp x) (numberp y)) (+ x y))
         ((or (equal y 'todas) (equal x 'todas))'todas)
         ((and (null y) x) (1+ x))
         ((and (null x) y) (1+ y))
         (t default-value)
         )
  )

;;;
;;; Solver engine of constraint satisfaction problems or CSPs.
;;;

(defun Solver-Engine ( Constraints Domains &OPTIONAL k-prunning  &KEY num-solutions next-solution
                                   chord-case delete-case &AUX  n   VAR-LEFT VAR-DONE (ev-if t) Out
                                  )
  "Searches for solutions to the CSP defined by Constraints and Domains.
k-prunnig specifies the level of forward checking performed:
           If not specified or  NIL full forward checking is performed. 
           if equal to zero, no forward checking is performed (simple backtracking). 
           otherwise it expresses the number of variables ahead of the current one that
           should be pruned for values compatible with the current value selected.
           A good rule of thumb is: For 'easy' problems (few constraints, lots of solutions) a
           value of 2 should be entered. For hard problems, it should be left unspecified.
num-solution is the number of solutions required. Default is 1.
next-solution, if given, is an integer defining the number of a variable (e.g. chord) whose value 
should first be changed and then another solution searched for.
" 

  (when (and (numberp next-solution) (functionp fvtempos::fun-proxima))
    (multiple-value-bind ( code Instancias-l  M-l Binst-l G-l GM-l Arcs-from-l 
                                DOMCURS-l k-prunning-l num-sol-l cont-sol-l current-var-l) (funcall fvtempos::fun-proxima)
      (when (equal code "Previous")
        (setf ev-if nil)
        (let ( (LSTVARS (Restore  next-solution (array-total-size DOMCURS-l) 
                                  current-var-l Instancias-l 
                                  G-l GM-l Arcs-from-l M-l BInst-l  DOMCURS-l
                                  chord-case delete-case)) )
          (setf VAR-DONE (first LSTVARS) VAR-LEFT (second LSTVARS) fvtempos::G (third LSTVARS)
                fvtempos::DOMCURS DOMCURS-l fvtempos::GM GM-L fvtempos::Binst Binst-l fvtempos::Primer-Soporte M-l fvtempos::Arcs-From Arcs-from-l )
          )
        (format t "Searching next solution:") 
        (setf Out (Solucion VAR-LEFT VAR-DONE fvtempos::instancias  fvtempos::Primer-Soporte  fvtempos::Binst fvtempos::G fvtempos::GM fvtempos::Arcs-From 
                            fvtempos::DOMCURS  k-prunning-l (+symbol num-sol-l num-solutions  (1+ cont-sol-l)) (1+ cont-sol-l) t ))
        )
      )
    )
  (when ev-if
    (if (and (integerp num-solutions) (< num-solutions 1) ) NIL  
        (progn
          (setf n  (list-length Domains) 
               fvtempos::Instancias (make-array  n :initial-element NIL :adjustable t)
                fvtempos::Binst (make-array (list n 2 n) :initial-element NIL :adjustable t)
                fvtempos::RAIZ -1            
                fvtempos::constraint-checks 0
                fvtempos::Primer-Soporte (make-array (list n 2 n ) :initial-element NIL :adjustable t)
                ) 
          (multiple-value-bind (G DOMAUX) (Crear-Multigrafo-jer+ Constraints Domains)
            (setf fvtempos::DOMCURS (Trans-Cursor-jer-array DOMAUX n))
            (setf fvtempos::G G)
            (multiple-value-bind (E1 E2) (make-matrix-n-jer+ G n)
              (setf fvtempos::GM E1 
                    fvtempos::Arcs-From E2) )
            (dotimes  (i n (setf VAR-LEFT (reverse VAR-LEFT)) ) (push i VAR-LEFT))
            ;(print "Searching:") 
            (setf Out (Solucion VAR-LEFT VAR-DONE fvtempos::Instancias  fvtempos::Primer-Soporte  
                                fvtempos::Binst fvtempos::G fvtempos::GM fvtempos::Arcs-From 
                                fvtempos::DOMCURS k-prunning num-solutions 1 ))
            )
          )
        )
    )
  Out
  )

#+:OM
(om::defmethod! om::csp->chords ((Ls list) &OPTIONAL (Base-n 0))
  (when (typep Ls 'array) (setf Ls (coerce Ls 'list)))
  (do*  ((Lsol Ls (cddr Lsol)) (x (first Lsol) (first Lsol)) (y (second Lsol) (second Lsol)) 
         LsolOut) ((or (null Lsol) (not x) ) 
                   (if Base-n (om::om+ (om::om* (nreverse LsolOut) 100) Base-n) 
                       (om::om* (nreverse LsolOut) 100)))
    (push  (dx->x x y) LsolOut)
    ))
#+:OM
(om::defmethod! om::chord-sol ((Ls list) &OPTIONAL (Base-n 0))
  :initvals '(nil 0) :indoc '("solution" "base note") :icon 195
  :doc "Takes a solution from the solver and transforms it into a lists of chords in midics, optionally transposed according to <Base-n>" 
  (when (typep Ls 'array) (setf Ls (coerce Ls 'list)))
  (when (consp (first Ls)) (setf Ls (first Ls)))
  (do*  ((Lsol Ls (cddr Lsol)) (x (first Lsol) (first Lsol)) (y (second Lsol) (second Lsol)) 
         LsolOut) ((or (null Lsol) (not x) ) 
                   (if Base-n (om::om+ (om::om* (nreverse LsolOut) 100) Base-n) 
                       (om::om* (nreverse LsolOut) 100)))
    (push  (dx->x x y) LsolOut)
    ))

#+:OM
(om::defmethod! om::ch-sol ((Ls list) &OPTIONAL (Base-n 0))
  :initvals '(nil 0) :indoc '("solution" "base note") :icon 195
  :doc "Takes a solution from the solver and transforms it into a lists of chords in midics, optionally transposed according to <Base-n>" 
 (om::chord-sol Ls Base-n))
 
#+:PW
(pw::defunp chord-sol ((Ls pw::list (:value '())) &OPTIONAL (Base-n fix>=0 (:value 0))) list
  "Takes a solution from the solver and transforms it into a lists of chords in midics,
 optionally transposed according to <Base-n>"
  (when (typep Ls 'array) (setf Ls (coerce Ls 'list)))
  (when (consp (first Ls)) (setf Ls (first Ls)))
  (do*  ((Lsol Ls (cddr Lsol)) (x (first Lsol) (first Lsol)) (y (second Lsol) (second Lsol)) 
         LsolOut) ((or (null Lsol) (not x) ) 
                   (if Base-n (pw::g+ (pw::g* (nreverse LsolOut) 100) Base-n) 
                       (pw::g* (nreverse LsolOut) 100)))
    (push  (dx->x x y) LsolOut)
    ))
#+:PW
(pw::defunp ch-sol ((Ls pw::list (:value '())) &OPTIONAL (Base-n fix>=0 (:value 0))) list
  "Takes a solution from the solver and transforms it into a lists of chords in midics,
 optionally transposed according to <Base-n>"
  (chord-sol Ls Base-n))

(defun trans-k ( ei  ) (if (equal ei -1) NIL ei))
(defun trans-s ( ei  ) (if (equal ei -1) 'todas ei))

;;;
;;;for compatibility
;;;

(defun musicsolver ( n-chords Densities Intervals Ambitus Constraints  
                              &OPTIONAL (num-solutions 1) fixed-chords next-solution
                              )
  (declare (IGNORE n-chords Densities Intervals Ambitus Constraints  
                              num-solutions fixed-chords next-solution))
 (print "change this box to 'Music-constraints'") nil)


(defun Music-constraints ( n-chords Densities Intervals Ambitus Constraints  
                                    &OPTIONAL (num-solutions 1) fixed-chords next-solution
                                    )
  (declare (IGNORE n-chords Densities Intervals Ambitus Constraints  
                   num-solutions fixed-chords next-solution))
  (print "change this box to 'solver'")
  nil
  )

;;
;; The solver interface.
;;
(defvar *default-intervals* '(1 2 3 4 5 6 7 8 9 10 11 12))

(defun approximation-given? (Amb) (member 'app Amb :test #'my-string=))
(defun get-approximation (Amb) (second amb))
(defun remove-approximation (Amb ambitus) (remove (first amb) (remove (second amb) ambitus :test #'equal) :test #'my-string=))

(defun distance-function? (Amb) (member 'dist Amb :test #'my-string=))
(defun get-distance-fun (Amb) (first (second Amb)))
(defun get-integrate-distance-fun (AMb) (second (second Amb)))
(defun get-neutral-element (Amb) (third (second Amb)))
(defun remove-distance-fun (Amb ambitus) (remove (first amb) (remove (second amb) ambitus :test #'equal) :test #'my-string=))

(defun variable-order-given? (amb) (member :var-order Amb :test #'my-string=))
(defun options-given? (amb) (member :options Amb :test #'my-string=))
(defun remove-var-ordering (amb ambitus) (remove (first amb) (remove (second amb) ambitus :test #'equal) :test #'my-string=))
(defun min-domain-order? (exp) (my-string= (second exp) :min-domain))
(defun user-given-order (exp) (second exp))

(defun check-variable-ordering (ambitus)
  (let (amb)
  (when (setq amb (variable-order-given? ambitus))
      (cond ((min-domain-order? amb) (setf *user-given-hvar-heuristic* #'hvar-min-dom))
            (t (setf *user-given-hvar-heuristic* (user-given-order amb))))
      )))
  
(defun stdrd-parse-options (ambitus)
  (let (amb)
  (when (setq amb (variable-order-given? ambitus))
    (check-variable-ordering ambitus)
    (setq ambitus (remove-var-ordering amb ambitus)))   ;;for compatibility only
  (when (setq amb (options-given? ambitus))
    (parse-options-form amb)
    (setq ambitus (cddr ambitus)))
  ambitus))

(defun set-up-ambitus&approx&distance (Ambitus)
  (let (amb)
    (if (setq amb (approximation-given? Ambitus))
      (progn (delayconstr::set-space-step-approx (get-approximation amb))
             (setq Ambitus (remove-approximation amb Ambitus)))
      (delayconstr::set-space-step-approx *default-approximation*))
    (if (setq amb (distance-function? Ambitus))
      (progn (setf *distance-function* (get-distance-fun amb)
                   *integrate-distance-fun* (get-integrate-distance-fun amb)
                   *distance-neutral-element* (get-neutral-element amb))
             (setq Ambitus (remove-distance-fun amb Ambitus)))
      (setf *distance-function* #'- *integrate-distance-fun* #'+ *distance-neutral-element* 0))
    (setq ambitus (stdrd-parse-options ambitus))    ;; must be called last    
    Ambitus))

(defun parse-options-form (expr)
  (when (eq (first expr) :var-order)
    (cond ((eq (second expr) :min-domain) (setf *user-given-hvar-heuristic* #'hvar-min-dom))
          (t (setf *user-given-hvar-heuristic* (second expr)))))    ;; for compatibility only
  (when (eq (first expr) :options)
    (let (options)
      (when (setq options (member :var-order (second expr) :test #'my-string=))
        (cond ((eq (second options) :min-domain) (setf *user-given-hvar-heuristic* #'hvar-min-dom))
              (t (setf *user-given-hvar-heuristic* (second options)))))
      (when (setq options (member :no-permut (second expr) :test #'my-string=))
        (setf *domain-permutation* nil)))))

(defun Solver ( n-chords Densities Intervals Ambitus Constraints    
                         &OPTIONAL (num-solutions 1) fixed-chords next-solution &REST Adjoint
                         )
  " "
  (setf *chords-problem?* nil)
  (setf *user-given-hvar-heuristic* #'hvar-simple-order)
  (setf *domain-permutation* t)
  ;(format t  "Begining the search for solutions: Building domains and constraints...")
  (when (and fixed-chords (not (non-chord-domains? fixed-chords)))
    ;;#+:OM (setf fixed-chords (expand-lst fixed-chords))
    ;;#+:PW (setf fixed-chords (pw::expand-lst fixed-chords))
    (let ((fvtempos::MaxChords n-chords))
      (unless (listp Constraints) (setf Constraints (list Constraints)))
      (when Densities (setf Constraints (cons (set-chord-length (fill-expression Densities)) Constraints)))
      (setq Ambitus (set-up-ambitus&approx&distance Ambitus))
      (when Ambitus (setf Constraints (cons (def-Ambitus (fill-expression Ambitus) ) Constraints)))
      
      (setf fixed-chords (fill-expression  fixed-chords))
      
      ))
  (if (not next-solution) 
    (if (null (last+ adjoint))
      (if (non-chord-domains? fixed-chords)
        (progn 
          (setf *chords-problem?* t)
          (setf fvtempos::constr-checks-table (make-array (list n-chords n-chords) :initial-element 0))
          (parse-options-form Ambitus)
          #|
aaa
(time (solver-engine  Constraints (make-non-chord-domains n-chords fixed-chords) ;
                                nil  
                                :num-solutions (trans-s num-solutions)))
|#
          (solver-engine  Constraints (make-non-chord-domains n-chords fixed-chords) ;
                          nil  
                          :num-solutions (trans-s num-solutions)))
        (progn 
          (let ((fvtempos::MaxChords n-chords)) ;;; Refining a making Densities,Intervals, Ambitus and others inputs.
            (setf fvtempos::constr-checks-table (make-array (list (1+ (* 2 n-chords)) (1+ (* 2 n-chords))) :initial-element 0))
            (setf Densities (form-domain-dimension Densities n-chords))
            (setf Intervals (form-domain-dimension Intervals n-chords))
            (setq Ambitus (set-up-ambitus&approx&distance Ambitus))
            (setf Ambitus (form-domain-dimension Ambitus n-chords t))
            
            (unless (listp Constraints) (setf Constraints (list Constraints)))
            
            (if (setf Ambitus (n-list Ambitus (1- n-chords)))
              (setf  Ambitus (complet-list Ambitus (last+ Ambitus) n-chords))
              (setf  Ambitus (complet-list Ambitus '(0 120) n-chords))
              )
            (if (setf  Intervals (n-list Intervals (1- n-chords)))
              (setf  Intervals (complet-list Intervals (last+ Intervals) n-chords))
              (setf  Intervals (complet-list Intervals *default-intervals* n-chords))
              )
            )           
          
          (multiple-value-bind (Domains chord-constraint)
                               (make-domains n-chords Intervals 
                                             (n-list Densities (1- n-chords)) Ambitus fixed-chords)
            #|
aaa
(time (solver-engine  (change_level (append (Make_ambitus Ambitus)
                                                        chord-constraint Constraints)) (cons '((0 (0))) Domains) ;
                                  nil  
                                  :num-solutions (trans-s num-solutions)))
|#
            (solver-engine  (change_level (append (Make_ambitus Ambitus)
                                                  chord-constraint Constraints)) (cons '((0 (0))) Domains) ;
                            nil  
                            :num-solutions (trans-s num-solutions))
            )
          )   )
      (progn
        (let*  ((Input (last+ adjoint))
                (m (first Input))
                (NewDOM (second Input))
                (NewGrafo (third Input))
                )
          (adjoint-problem-constructor m NewDOM NewGrafo)
          )
        (print "Adding sub-problem...")
        #|
aaa
(time (solver-engine  nil  nil nil :num-solutions (trans-s num-solutions) :next-solution (+ fvtempos::current-var 2)
                                :chord-case t
                                :delete-case nil))
|# 
        (solver-engine  nil  nil nil :num-solutions (trans-s num-solutions) :next-solution (+ fvtempos::current-var 2)
                        :chord-case t
                        :delete-case nil)
        )
      )
    (progn
      (let ((chord-case t) (delete-case nil))
        (cond ((listp next-solution) 
               (when (or (third next-solution) (equal (second next-solution) t)) (setf delete-case t))
               (cond ((equal (first next-solution) 'b) (setf next-solution (+ (* 2 (second next-solution) ) 1)
                                                             chord-case nil))
                     ((equal (first next-solution) 'i) (setf next-solution (+ (* 2 (second next-solution) ) 2)
                                                             chord-case nil))
                     (t (setf next-solution (+ (* 2 (first next-solution) ) 2))))
               )
              ((numberp next-solution) (setf next-solution (+ (* 2 next-solution ) 2)))
              (t (setf next-solution nil)))
        #|
aaa
(time (solver-engine    nil nil nil :num-solutions (trans-s num-solutions) :next-solution next-solution
                                :chord-case chord-case
                                :delete-case delete-case))
|#
        (solver-engine    nil nil nil :num-solutions (trans-s num-solutions) :next-solution next-solution
                          :chord-case chord-case
                          :delete-case delete-case) 
        )
      )
    )
  )


#|
#+:OM
(om::defmethod! om::Csolver ((n-ch number) (Amb  list) (Dens t) (vint list)  (Cnstr t) (hint list)
                             &OPTIONAL (data  nil) (x-sol  nil) (n-sols 1)  
                             &REST merge
                             )
  :initvals '(8 (48_72) (3) (2 7)  nil (1 3)  nil nil 1 nil) 
  :indoc '("number of chords" "Ambitus" "densities" "vertical intervals"  "Constraints" "horizontal intervals"
           "Fixed chords data base" "find next solution" "Number of solutions"
             "merge problems")
  :doc "kept for compatibility. Names have changed"  :icon 200
  ;
  (when hint
    (let ((fvtempos::MaxChords n-ch))
      (if (listp cnstr)
        (setf cnstr (cons (horizontal-intervals (fill-expression hint)) cnstr))
        (setf cnstr (list (horizontal-intervals (fill-expression hint)) cnstr)))))
  (apply #'solver n-ch Dens vint Amb cnstr n-sols data x-sol merge)
  )
|#

#+:OM
(om::defmethod! om::Csolver ((n-obj number) (p-pts  list) (n-pts t) (i-dst list)  (Cnstr t) (x-dst list)
                             &OPTIONAL (data  nil) (x-sol  nil) (n-sols 1)  
                             &REST merge
                             )
  :initvals '(8 (48_72) (3) (2 7)  nil (1 3)  nil nil 1 nil) 
  :indoc '("number of objects" "possible points" "number of points" "internal distances"  "Constraints"
 "external distances"
           "Fixed objects data base" "find next solution" "Number of solutions"
             "merge problems")
  :doc "Searches solutions for the Music Constraint-Satisfaction Problem.
1. 'n-obj' is the number of objects.
2. 'n-pts' defines the number of points in each object.
     It can be
      -A list of lists: The i-th  sublist contains the options for the number of points in
       the i-th object. e.g. (3 4 7), admits objects having 3 4 or 7 points.
      -A number: Gives the number of points for every object
      -A flat list: Gives the options for every object.
     If the length of 'n-pts' is less than n-obj, it is completed with default
     values according to the values in 'i-dst'.
3. 'i-dst' defines the possibles distances inside each object.
      It can be a (flat or 2-level) list that defines the  possible intervals 
      for an object. For instance the list ( 3 4 7 11 ) defines objects having
      (possibly repeated) consecutive distances from that list.
      The list ((4 7 11) (10 12)) defines objects with distances EITHER from the first
      or second sublist.
      When a sublist begins with the symbol 'f' distances are EXACTLY those given.
      For instance, (f (4 7 11) (10 12)) defines either
      (4 7 11) or (10 12) as the only possible distances inside the object, in that order. 
     If the length of 'i-dst' is less than n-obj, it is completed with its last 
     value if any. If 'i-dst' is NIL and no 'data' are given, 'i-dst' is set
     to (1 2 3 4 5 6 7) by default.
4. 'p-pts' defines the possible points contained by objects. It can be a list of lists, where
the i-th sublist gives the possible points for the i-th object. It can also be composed of a
sequence of object numbers followed by the possible points, as in
(0_5 (57 61 62 63 67 69 71 73 74) 6_9 (27_127)).
    If the length of 'p-pts' is less than n-obj, it is completed with its last 
    value if any, else it is completed with '(0_120).
5. 'cnstrs' is the list of constraints for the problem.
6. 'x-dst' gives possible external distances between consecutive objects. The expression is as for
i-dst, except that certain points of the object can be specified as those where the distance is
to be measured. For instance,
(0_5 (l (7 11) u (1 2 3) 2 (4 5)) 6_9 (>5))
says that the first 6 points in the sequence should be so that the distance between consecutive
'lower' points is equal to either 7 or 11, the distance between consecutive 'upper' points 
should be either 1, 2 or 3, the distance between consecutive 'third lowest' points should be
either 4 or 5 and, finally, for the objects numbered 6 to 9, the distance between consecutive
'upper' (the default if nothing given) points should be greater than 5 or less than -5.
6. 'n-sols' gives the number of solution required. Default is 1. -1 means all.
7.  'data' is a list of elements ( <index-list> <list of objects> )
     where <index-list> is the list of indexes of variables representing objects and <list of objects>
     is the list of possible values (objects) for these variables. <index-list> can also be a single
     index.
8. 'x-sol' is an index representing a object number. Starting from this object,
    (i.e. finding a new value for it, but keeping the values of previous objects, if possible) another
     solution is searched for.
     'x-sol' can also be one of the following lists:
      a. (index t) : Starting from the object represented by 'index', but IGNORING
        ITS constraint, another solution is searched for.
      b. ('b index) : Starting from the base of the object represented by 'index'
                     another solution is searched for.
      c. ('i index) : Starting from the intervals of the object represented by 'index'
                     another solution is searched for.
      d. ('b index t) or ('i index t) : Similar to  b or c., 
           except that the corresponding constraint (i.e. base constraint or interval constraint) are 
           ignored.     
    NOTE: 'x-sol' should be NIL unless at least one solution has already been found.

9,10,...Each of these entries represent a sub-problem ( given by the adjoint-problem box, see UserLib/Situation1.b menu ) 
        to be adjoint to the current solved problem. 
        Every time only the last entry is considered, because we assume that the previous ones has been
        adjointed already.    
"  
:icon 200
  ;
  (when x-dst
    (let ((fvtempos::MaxChords n-obj))
      (if (listp cnstr)
        (setf cnstr (cons (horizontal-intervals (fill-expression x-dst)) cnstr))
        (setf cnstr (list (horizontal-intervals (fill-expression x-dst)) cnstr)))))
  (apply #'solver n-obj n-pts i-dst p-pts cnstr n-sols data x-sol merge)
  )

#+:PW
(pw::defunp  Csolver ((n-ch pw::fix (:value 8)) (Amb  pw::list    (:value '(48_72)))
                      (Dens pw::list  (:value '(3))) (vint pw::list  (:value '(2 7)))                          
                      (cnstr pw::list (:value '())) (hint pw::list  (:value '(1 3)))
                      &OPTIONAL                           
                      (data  pw::list (:value '())) (x-sol  pw::list (:value '()))
                      (n-sols pw::fix (:value 1))
                      &REST (merge pw::list (:value '()))
                      ) list
"Searches solutions for the Music Constraint-Satisfaction Problem.
1. 'n-ch' is the number of chords.
2. 'Dens' defines the number of notes for each chord.
     It can be
      -A list of lists: The i-th  sublist contains the options for the number of notes in
       the i-th chord. e.g. (3 4 7), admits chords having 3 4 or 7 notes.
      -A number: Gives the number of notes for every chord.
      -A flat list: Gives the options for every chord.
     If the length of 'Dens' is less than n-ch, it is completed with default
     values according to the values in 'vint'.
3. 'vint' defines the possibles intervals for each chord.
      It can be a (flat or 2-level) list that defines the  possible intervals 
      for a chord. For instance the list ( 3 4 7 11 ) defines chords having
      (possibly repeated) consecutive intervals from that list.
      The list ((4 7 11) (10 12)) defines chords with intervals EITHER from the first
      or second sublist.
      When a sublist begins with the symbol 'f' the intervals are EXACTLY those given.
      For instance, (f (4 7 11) (10 12)) defines either
      (4 7 11) or (10 12) as the only possible intervals for the chord, in that order. 
     If the length of 'vint' is less than n-ch, it is completed with its last 
     value if any. If 'vint' is NIL and no 'data' are given, 'vint' is set
     to (1 2 3 4 5 6 7 8 9 10 11 12) by default.
4. 'Amb' has the form ((Io so) (i1 s1) ... (in sn)) where (ij sj) defines the ambitus
     for the j-th chord.
    If the length of 'Amb' is less than n-ch, it is completed with its last 
    value if any, else it is completed with '(0 120).
5. 'cnstrs' is the list of constraints for the problem.
6. 'n-sols' gives the number of solution required. Default is 1. -1 means all.
7.  'data' is a list of elements ( <index-list> <list of chords> )
     where <index-list> is the list of indexes of variables representing chords and <list of chords>
     is the list of possible values (chords) for these variables. <index-list> can also be a single
     index.
8. 'x-sol' is an index representing a chord number. Starting from this chord,
    (i.e. finding a new value for it, but keeping the values of previous chords, if possible) another
     solution is searched for.
     'x-sol' can also be one of the following lists:
      a. (index t) : Starting from the chord represented by 'index', but IGNORING
        ITS constraint, another solution is searched for.
      b. ('b index) : Starting from the base of the chord represented by 'index'
                     another solution is searched for.
      c. ('i index) : Starting from the intervals of the chord represented by 'index'
                     another solution is searched for.
      d. ('b index t) or ('i index t) : Similar to  b or c., 
           except that the corresponding constraint (i.e. base constraint or interval constraint) are 
           ignored.     
    NOTE: 'x-sol' should be NIL unless at least one solution has already been found.

9,10,...Each of these entries represent a sub-problem ( given by the adjoint-problem box, see UserLib/Situation1.b menu ) 
        to be merged to the current solved problem. 
        Every time only the last entry is considered, because we assume that the previous ones has been
        adjointed already.    
"  :icon 200
;
(when (and hint (not (non-chord-domains? data)))
    (let ((fvtempos::MaxChords n-ch))
      (if (listp cnstr)
        (setf cnstr (cons (horizontal-intervals (fill-expression hint)) cnstr))
        (setf cnstr (list (horizontal-intervals (fill-expression hint)) cnstr)))))
(apply #'solver n-ch Dens vint Amb cnstr n-sols data x-sol merge)
)

;;;
;;;utilities for parsing constraint expressions
;;;

(defmethod form-domain-dimension ((exp number) (n number) &optional fill-lists)
  (declare (ignore fill-lists))
  (Complet-list nil exp n))
(defmethod form-domain-dimension ((exp null) (n number) &optional fill-lists) (declare (ignore fill-lists)) nil)
(defmethod form-domain-dimension ((exp list) (n number) &optional fill-lists)
  #+:OM (setq exp  (expand-lst exp) )
  #+:PW (setq exp (pw::expand-lst exp))
  (cond ((flatp exp) (Complet-list nil exp n))
        ((and (symbolp (first exp)) (string= (first exp) *interp-key*))
         #+:OM (form-unary-expression (append (om::arithm-ser 0  (1- n) 1) (list exp)) n fill-lists)
         #+:PW (form-unary-expression (append (pw::arithm-ser 0  1 (1- n)) (list exp)) n fill-lists)
         )
        ((consp (first exp)) exp)    ;;just in case old syntax is being used
        (t (form-unary-expression exp n fill-lists))))

(defmethod approx-round ((self number) (app number)) (round self app))
(defmethod approx-round ((self list) (app number))
  (mapcar #'(lambda (item) (approx-round item app)) self))

(defun get-interpolated-values (from to length curve &optional fill-lists)
  (let ((lists
         #+:OM (om::om* (approx-round (om::interpolation from to length curve) (delayconstr::get-current-space-step))
                        (delayconstr::get-current-space-step))
         #+:PW (pw::g* (approx-round (pw::interpolation from to length curve) (delayconstr::get-current-space-step))
                       (delayconstr::get-current-space-step))
         ))
    (if fill-lists
      (mapcar #'(lambda (pair) 
                  #+:OM (om::arithm-ser (first pair)  (second pair) (delayconstr::get-current-space-step))
                  #+:PW (pw::arithm-ser (first pair) (delayconstr::get-current-space-step) (second pair)))
              lists)
      lists)
    ))

(defun form-unary-expression (exp &optional n fill-lists)
  (when exp
    (let ((HTable+ (make-hash-table+ ))
          (expand (nreverse exp))   ;;reading the list backwards for efficiency
          item maxkey result indexes)
      (do ((x (first expand) (first expand))) ((null expand))
        (cond ((and (consp x) (symbolp (first x)) (string= (first x) *interp-key*))
               (setf indexes nil item (pop expand))
               (do () ((or (null expand) (consp (first expand))))
                 (push (pop expand) indexes))
               (dolist (sublist 
                        (and indexes (get-interpolated-values (second item) (third item) (length indexes) (or (fourth item) 1)
                                                              fill-lists)))
                 (setf Htable+ (addhash+ (pop indexes) sublist Htable+))))
              ((consp x) (setf item x) (pop expand))
              (t (setf Htable+ (addhash+ x item Htable+)) (pop expand))))
      (if n
        (progn (setf maxkey (apply #'max (hash-table+-keys HTable+)) item (first (gethash+ maxkey Htable+)))
               (dotimes (i n (nreverse result))
                 (if (<= i maxkey) (push (first (gethash+ i Htable+)) result) (push item result))))
        HTable+)
      )) )

;;(form-domain-dimension '(0 (3 3) 1_7 (interp (3 4) (7 7)) 8_13 (interp (7 7) (3 5)) 14_15 (interp (3 5) (3 4))) 20)
;;(form-unary-expression (fill-expression '((3 4)(4 4) (4 5) (6 7))) 8)
;;(form-domain-dimension '(interp (3 4) (12 15)) 20)