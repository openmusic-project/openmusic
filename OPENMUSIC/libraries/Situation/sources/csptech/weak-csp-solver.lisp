
;;
;; Weak constraints solver
;; By Camilo Rueda (25/03/99)
;;

(in-package :cl-user)
 
;;;
;;; The Engine
;;;

(defvar *Necessary*)
(setf *Necessary* most-positive-fixnum)
(defvar *sufficient*)
(setf *sufficient* 0)
(defvar *maxlevel*)
(setf *maxlevel* 2)
(defvar *numvars*)
(setf *numvars* 2)
(defvar *best-solution*)
(setf *best-solution* (list "none"))
(defvar *num-solutions* 1)
(defvar *found-solutions* 0)
(defvar *conflicts*)
(defvar *accumulated-solutions*)
(defvar *current-solution*)
(setf *current-solution* (make-array (list 2 3) :initial-element 0))
(defvar *Hdominios*)
(setf *Hdominios* (make-array (list 2 3) :initial-element (make-array 1))
      (aref (aref *Hdominios* 1 2) 0) '(0 0))
(defvar *Extensiones*)
(setf *extensiones* (make-array (list 4 4)))
(defvar *weak-constraints*)
(setf *weak-constraints* (make-array (list 16 16)))
(defvar *unary-constraints*)
(setf *unary-constraints* (make-array 4))
(defvar *most-recent-instanced-var* 0)


(defun partialHFC (i k dist)
  (let ((upto (if (= k 1) (domain-elements i k)
                  (second (domain-extensions i (1- k) (get-partial-solution i (1- k))))))
        (desde (if (= k 1) 0 (first (domain-extensions i (1- k) (get-partial-solution i (1- k)))))))
    (loop for e from desde to upto do
          (let (ndist)
            (when (< (setf ndist (+ dist (get-conflicts e i k))) *Necessary*)
              (set-partial-solution i k e)
              (when (< (+ ndist (checkPHFC i k)) *Necessary*)
                (if (= k *maxlevel*)
                  (progn
                    (set-most-recent-instance i)
                    (if (= i *numvars*)
                    (progn
                      (princ ndist) (princ " ")
                      (put-best-solution (get-current-solution))
                      (setf *necessary* ndist)
                      (when (<= *necessary* *sufficient*)
                        (print (list "FOUND!!. Value=" ndist))
                        (accumulate-solutions *best-solution*)
                        (when (= (incf *found-solutions*) *num-solutions*)
                          (throw 'final (get-all-solutions)))))
                    (partialHFC (1+ i) 1 ndist))
                    )
                  (partialHFC i (1+ k) ndist))
                )  
              (restaurePHFC i k)
              )
            )
          ))
    
    ;;INVARIANT: (Partial solution up to i-1 is forward-weak-consistent AND
    ;;            solution for variable i at levels 1,..,k is forward-weak-consistent)
    ;;           PROPERTY "forward-weak-consistency" means that in every forward domain there
    ;;                are values such that the current distance at i plus the sum of
    ;;                all conflicts of those values is less than the current value of *necessary*
    ;; VARIANT: *maxlevel* (*numvars* - i) - k
    )


(defun checkPHFC (i k)
  (if (= i *numvars*)
    0
    (let ((accum 0))
      (loop for j from (1+ i) to *numvars* do
            (incf accum (DFSExplore nil 1 j i k)))
      accum))
  ;;RESULT:  The sum of the minimum possible number of conflicts that are to be found when
  ;;         a solution including the value S[i,k] is eventually extended to all variables.
  ;;VARIANT: *maxlevel* (*numvars* - j) - level
  )

(defun DFSExplore (root level j i k)
  (let (fromchildren tochildren (cfs 0) (res most-positive-fixnum))
    (if (= level 1)
      (setq fromchildren 0 tochildren (domain-elements j level))
      (let (extensions)
        (setq cfs (testvalue i k j (1- level) root))
        (if (> level *maxlevel*)
          (setq fromchildren 0 tochildren -1 res 0)
          (setq extensions (domain-extensions j (1- level) root)
                fromchildren (first extensions)
                tochildren (second extensions)))))
    (loop for e from fromchildren to tochildren do
          (setq res (min (DFSExplore e (1+ level) j i k) res)))
    (+ res cfs))
;;INVARIANT: for each value e at level "level" in a forward domain j, conflicts(e,j,level) is the
;;           weighted sum of constraint violations of that value with the current
;;RESULT: The minimum number of conflicts between S[i,k] and values in the subtree
;;        rooted by "root" at domain level "level-1" of variable j
  )

(defun testvalue (i k j level e &optional (fun #'+))
  (let ((constraints (get-enabled-constraints i k j level))
        (conflicts 0))
    (loop for c in constraints do
          (unless 
            (apply-weak-constraint c j (get-domain-value j level e))
            (incf conflicts (get-importance c))
            (update-conflicts e j level (funcall fun (get-importance c)))))
  conflicts))

(defun restaurePHFC (i k)
  (loop for j from (1+ i) to *numvars* do
        (loop for level from 1 to *maxlevel* do
              (loop for e from 0 to (domain-elements j level) do
                    (testValue i k j level e #'-)))))

(defun get-enabled-constraints (vari leveli varj levelj)
  (aref (aref *weak-constraints* vari varj) leveli levelj))

(defun apply-weak-constraint (c j w)
  (apply (c-predicate c)
         (mapcar #'(lambda (var-level)
                     (if (= (c-var var-level) j)
                       w
                       (get-partial-solution-value (c-var var-level) (c-level var-level))))
                 (c-var-levels c))
         ))

(defun elements-from (i level value) (domain-extensions i level value))

(defun get-conflicts (v i level) (aref (aref *conflicts* i level) v))

(defun update-conflicts (e j level increment) (incf (aref (aref *conflicts* j level) e) increment))

(defun display-conflicts ()
  (loop for i from 0 to *numvars* do
        (loop for j from 1 to *maxlevel* do
              (let (res res1)
                (loop for e from 0 to (domain-elements i j) do
                      (push (get-domain-value i j e) res)
                      (push (get-conflicts e i j) res1))
                (print (list "var=" i "level=" j
                             "values="
                             (nreverse res)
                             "conflicts=" (nreverse res1)))))))

(defun set-partial-solution (i k v)
  (setf (aref *current-solution* i k) v))    ;;;(get-domain-value i k v)))

(defun get-current-solution () *current-solution*)

(defun accumulate-solutions (sol)
  (setf *accumulated-solutions* (cons sol *accumulated-solutions*)))

(defun get-all-solutions () *accumulated-solutions*)

(defun put-best-solution (sol) (setf *best-solution* (form-user-solution sol)))

(defun get-best-solution () *best-solution*)

(defun get-current-cost () *necessary*)

(defun form-user-solution (solution &optional upto)
  (let (soli)
    (loop for i from 0 to (or upto *numvars*) do
          (push (get-domain-value i *maxlevel* (aref solution i *maxlevel*)) soli))
  (nreverse soli)))

(defun get-partial-solution (i level) (aref *current-solution* i level))

(defun get-partial-solution-value (i level)
  (get-domain-value i level (get-partial-solution i level)))

(defun set-most-recent-instance (var) (setf *most-recent-instanced-var* var))

(defun get-previous-weak-solution ()
  (form-user-solution *current-solution* *most-recent-instanced-var*))

(defun crear-all-weak-domains (dom)
  (loop for i from 0 to (1- (length dom)) do (crearDomsW i 0 (nth i dom))))

(defun crearDomsW (i level values)
  (if (= level 0)
    (progn
      (setf (aref *extensiones* i level) (make-array 1 :initial-element (list 0 (1- (length values))))
          (aref *Hdominios* i level) (make-array 1 :initial-element 0))
      (crearDomsW i (1+ level) values))
    (let ((current -1) prox)
      (setf (aref *Hdominios* i level) (make-array (length values))
            (aref *extensiones* i level) (make-array (length values))
            (aref *conflicts* i level) (make-array (length values)))
      (if (= level *maxlevel*)
        (loop for e in values and k from 0 to (1- (length values)) do
              (setf (aref (aref *Hdominios* i level) k)  e))
        (loop for e in values and k from 0 to (1- (length values)) do
              (setf (aref (aref *Hdominios* i level) k) (first e))
              (when (< level *maxlevel*)
                (setq prox (+ current (length (second e))))
                (setf (aref (aref *extensiones* i level) k) (list (1+ current) prox))
                (setf current prox))))
      (when (< level *maxlevel*) (crearDomsW i (1+ level) (apply #'append (mapcar #'second values))))))
  )

(defun put-importance-in-all (Clist imp)
  (if (null Clist) nil 
      (cons (includeConstraint-Importance (first Clist) imp) (put-importance-in-all (cdr Clist) imp))))

(defun includeConstraint-Importance (C imp)
 (when (atom C) (setq C (funcall C)))
  (loop for rel in C do
        (setf (second rel) (append (second rel) (list imp))))
  C)

(defun get-importance (C) (or (third (second C)) 1))

(defun domain-elements (i level) (1- (length (aref *Hdominios* i level))))

(defun domain-values (i level) (aref *Hdominios* i level))

(defun get-domain-value (i level kth) (aref (aref *Hdominios* i level) kth))

(defun domain-extensions (i level kth) (aref (aref *extensiones* i level) kth))

;; (loop for i from 0 to 3 do (crearDomsW i 0 (nth i om::xxx)))
;; (domain-extensions 1 1 1)

;; User Interface

(defun WeakSolver ( n-chords Densities Intervals Ambitus Constraints    
                         &OPTIONAL (num-solutions 1) fixed-chords next-solution &REST Adjoint
                         )
  " "
  (let* ((*maxlevel* 2)     ;; for the time being
         (*Necessary* (look-for-max ambitus))
         (*sufficient* (look-for-sufficiency Ambitus))
         (*numvars* (1- (* 2 n-chords)))
         (*num-solutions* num-solutions)
         (*found-solutions* 0)
         (*conflicts* (make-array (list (1+ *numvars*) (1+ *maxlevel*))))
         (*current-solution* (make-array (list (1+ *numvars*) (1+ *maxlevel*))))
         (*Hdominios* (make-array (list (1+ *numvars*) (1+ *maxlevel*))))
         (*extensiones* (make-array (list (1+ *numvars*) (1+ *maxlevel*))))
         (*weak-constraints* (make-array (list (1+ *numvars*) (1+ *numvars*))))
         (*unary-constraints* (make-array (1+ *numvars*) :initial-element nil))
         (*accumulated-solutions* nil)
         (*most-recent-instanced-var* 0)
         )
    (loop for i from 0 to *numvars* do
       (loop for j from 0 to *numvars* do
           (setf (aref *weak-constraints* i j)
                 (make-array (list (1+ *maxlevel*) (1+ *maxlevel*)) :initial-element nil))))
    (if *sufficient*
      (setq Ambitus (eliminate-accept Ambitus))
      (setq *sufficient* 0))
    (if *Necessary*
      (setq Ambitus (eliminate-max Ambitus))
      (setq *Necessary* most-positive-fixnum))
    
    (setf *chords-problem?* nil)
    (setf *user-given-hvar-heuristic* #'hvar-simple-order)
    (setf *domain-permutation* t)
    ;(format t  "Begining the search for solutions: Building domains and constraints...")
    (when (and fixed-chords (not (non-chord-domains? fixed-chords)))
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
            (time (solver-engine  Constraints (make-non-chord-domains n-chords fixed-chords) ;
                                  nil  
                                  :num-solutions (trans-s num-solutions))))
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
              (form-constraint-array 
                    (append (Make_ambitus Ambitus) chord-constraint Constraints) 2)
              
              (crear-all-weak-domains Domains)

              (apply-unary-constraints)

;(loop for i from 0 to 7 do (loop for j from 0 to 7 do (print (list i j (aref *weak-constraints* i j)))))(break)
              (print "exploring...")
              (time (catch 'final (partialHFC 0 1 0)))
              *accumulated-solutions*
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
          (time (solver-engine  nil  nil nil :num-solutions (trans-s num-solutions) :next-solution (+ fvtempos::current-var 2)
                                :chord-case t
                                :delete-case nil)) 
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
          (time (solver-engine    nil nil nil :num-solutions (trans-s num-solutions) :next-solution next-solution
                                  :chord-case chord-case
                                  :delete-case delete-case)) 
          )
        )
      )
    )
  )

#+:OM
(defun look-for-sufficiency (amb)
  (let ((accept (member 'OM::accept amb)))
    (when accept (second accept))))

#+:OM
(defun eliminate-accept (amb &optional (keywd 'OM::accept))
  (do ((items amb) (res)) ((null items) (nreverse res))
    (when (eq (first items) keywd)
      (progn (pop items) (pop items)))
    (push (pop items) res)))

#+:OM
(defun look-for-max (amb)
  (let ((nec (member 'OM::start amb)))
    (when nec (second nec))))

#+:OM
(defun eliminate-max (amb) (eliminate-accept amb 'OM::start))

#+:OM
(om::defmethod! om::Wprev-instances ()
  ::icon 192 
  :doc "Returns the current (partial) solution in standard representation
 (base point + internal distances) for weak constraint solving.
THIS DOES NOT WORK WITH THE STANDARD CSOLVER.
 You can use this box together with user-Cnstr to build constraints that look during the
 search at the values of all the previously instanciated variables.
 See the documentation of User-Cnstr"
  (get-previous-weak-solution))

#+:OM
(om::defmethod! om::Wbest-so-far ()
  ::icon 192 
  :doc "Returns the best solution found at that moment, in standard representation
 (base point + internal distances). It also returns the cost of that
solution.  Works for weak constraint solving.
THIS DOES NOT WORK WITH THE STANDARD CSOLVER."
  (values (get-best-solution) (get-current-cost)))

#+:OM
(om::defmethod! om::Wsolver ((n-obj number) (p-pts  list) (n-pts t) (i-dst list)  (Cnstr t) (x-dst list)
                             &OPTIONAL (data  nil) (x-sol  nil) (n-sols 1)  
                             &REST merge
                             )
  :initvals '(8 (48_72) (3) (2 7)  nil (1 3)  nil nil 1 nil) 
  :indoc '("number of chords" "Ambitus" "densities" "vertical intervals"  "Constraints" "horizontal intervals"
           "Fixed chords data base" "find next solution" "Number of solutions"
             "merge problems")
  :doc "Searches solutions for the Music Constraint-Satisfaction Problem.
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
        to be adjoint to the current solved problem. 
        Every time only the last entry is considered, because we assume that the previous ones has been
        adjointed already.    
"  :icon 200
  ;
  (when x-dst
    (let ((fvtempos::MaxChords n-obj))
      (if (listp cnstr)
        (setf cnstr (cons (horizontal-intervals (fill-expression x-dst)) cnstr))
        (setf cnstr (list (horizontal-intervals (fill-expression x-dst)) cnstr)))))
  (apply #'WeakSolver n-obj n-pts i-dst p-pts cnstr n-sols data x-sol merge)
  )




(defun set-importance-in-namedCnstr (name exp fun imp)
  (eval `(defun ,(gensym name) ()
              ',(includeConstraint-Importance
                 (funcall fun exp) 
                 (or imp 1))
                   ))
  )

;;(set-importance-in-namedCnstr "i-dst_filt" '(exp) 'fun 'imp)
;;
;; IN FILE MusicUser->CspTechn.lisp
;;

#+:OM
(om::defmethod! om::Wvint-filt  ((exp  list) &optional imp varMap choiceMap)
  :initvals '((0_5 (not (or (* 4 7 *) (? ? 2 *))) 6_8 (and (* 7 *))) nil nil nil)
  :indoc '("logical expression" "constraint importance" "variable Map" "choice Map")
  :icon 202 
  :doc "Vertical intervals filtering. Constraints the intervals in each of a given set of chords to follow a supplied pattern
For example, the expression (0_5 (not (or (* 4 7 *) (? ? 2 *))) 6_8 (and (* 7 *))) constraints chords number
0,1,2,3,4 and 5 each to have no third followed by a fifth and no major second in the third position. It also constraints
chords 6,7, and 8 each to contain a fifth"
  (if (or varMap choiceMap)
     (includeConstraint-Importance
      (set-pitch-logical-filter (fill-expression exp) (or varMap #'single-variable-map)
                                (if choiceMap (eval `(function (lambda (x) (x->dx (funcall ,choiceMap x))))) #'x->dx))
      (or imp 1))
     (set-importance-in-namedCnstr "i-dst_filt" (fill-expression exp) #'vert-logi-filter imp)
     )
)

(defun c-predicate (const) (first const))
(defun c-var-levels (const) (cdr const))
(defun c-var (var-level) (first var-level))
(defun c-level (var-level) (second var-level))
(defun c-imp (var-level) (cddr var-level))
(defun c-level-adjust (var-level dx)
  (if (and dx (par (c-var var-level))) dx (+ (c-level var-level) 1 )))

;;
;; Historical compatibility: constraint levels must be changed
(defun set-constraint-entries (const dx)
  (if (is-unary-constraint const)
    (let* ((var-level (second const)) (var (c-var var-level)))
      (add-unary-constraint var
         (list (c-predicate const) 
               (list* var (c-level-adjust var-level dx) (c-imp var-level)))))
    (let* ((maxi (c-var (first (c-var-levels const))))
           (maxj (c-var (second (c-var-levels const))))
           (leveli (c-level-adjust (first (c-var-levels const)) dx))
           (levelj (c-level-adjust (second (c-var-levels const)) dx))
           var level imp Q)
      (when (> maxi maxj)
        (psetq maxi maxj maxj maxi leveli levelj levelj leveli))
      (dolist (pair (cdr  Const))
        (setq var (c-var pair) level (c-level-adjust pair dx) imp (c-imp pair))
        (when (> var maxj)
          (psetq maxj var maxi maxj levelj level leveli levelj))
        (push (list* var level imp) Q)
        )
      (add-constraint maxi maxj leveli levelj (cons (first const) (nreverse Q)))))
  )

(defun is-unary-constraint (const)
  (= (length const) 2))

(defun add-unary-constraint (var C)
  (unless (> var *numvars*)
    (setf (aref *unary-constraints* var)
        (cons C (aref *unary-constraints* var)))))

(defun add-constraint (vari varj leveli levelj C)
  (unless (> varj *numvars*)
    (setf (aref (aref *weak-constraints* vari varj) leveli levelj)
          (cons C (aref (aref *weak-constraints* vari varj) leveli levelj)))))

(defun form-constraint-array (grafo &OPTIONAL dx &AUX Gout )
  (dolist (Const Grafo (reverse Gout))
    (if (listp Const)
      (set-constraint-entries const dx)
      (dolist (Constraint (funcall Const))
        (set-constraint-entries Constraint dx))
      )
    )
  )

(defun apply-unary-constraints ()
  (loop for var from 0 to *numvars* do
        (apply-weak-unary var)))

(defun exists-unary-constraint (var) (aref *unary-constraints* var))

(defun apply-weak-unary (var)
  (loop for c in (aref *unary-constraints* var) do
        (let* ((var-level (first (c-var-levels c)))
               (level (c-level var-level)))
          (loop for kth from 0 to (domain-elements var level) do
                (unless (funcall (c-predicate c) (get-domain-value var level kth))
                  (update-conflicts kth var level (get-importance c)))
           ))
   ))
  



        

      
              
              



            