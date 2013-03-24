(in-package :om)


;;
;;
;;            Librairie RepMus
;;
;;            Gerard Assayag, Claudy Malherbe  © IRCAM 1996
;;            OpenMusic Version March 98
           

 


; an alphabet A
; a collection S of sequences (S is a subset of A*) (a node list)
; a mapping r: SxS -> A* (a gweight function)

; exemple :
; A is a set of notes
; S is a collection of chords
; r (chord1, chord2) -> a-chord-representing-the-common-notes

; A is a set of chords
; S is a collection of sets of chords
; r (set1, set2) -> the-list-of-common-chords

; etc.

(defclass graph ()
  ((node-list :initarg :node-list :accessor node-list :initform ())
   (gweight-order-p :accessor gweight-order-p :initform #'<=)
   (gweight-min :accessor gweight-min :initform 0)
   (gweight-max :accessor gweight-max :initform 1000000)
   (build-edge-m :initarg :build-edge-m :accessor build-edge-m :initform 'common-notes)
   (gweight-threshold-p :initarg :gweight-threshold-p :accessor gweight-threshold-p
                               :initform #'(lambda (gweight) (> gweight 0)))
   (traverse-tree-m :initarg :traverse-tree-m :accessor traverse-tree-m :initform 'pre)))

(defclass edge ()
   ((gsource :initarg :gsource :accessor gsource :initform ())
   (gdestination :initarg :gdestination :accessor gdestination :initform ())
   (gweight :initarg :gweight :accessor gweight :initform 0)
   (content :initarg :content :accessor content :initform ()) ))

 
(defclass node ()
  ((graph :initarg :graph :accessor graph :initform ())
   (content :initarg :content :accessor content :initform ())
   (adj-list :initarg :adj-list :accessor adj-list :initform ())
   (father :initarg :father :accessor father :initform ())
   (childs :initarg :childs :accessor childs :initform ())
   (key :initarg :key :accessor key :initform 0)))

(defmethod build-edge ((u node) (v node) (edge-type symbol))
  (when (not (equal u v))
    (multiple-value-bind (edge-content edge-gweight)
                         (funcall (build-edge-m (graph u)) (content u) (content v))
      (when (funcall (gweight-threshold-p (graph u)) edge-gweight)
        (let ((edge (make-instance edge-type :gsource u :gdestination v)))
          (setf (content edge) edge-content
                (gweight edge) edge-gweight)
          edge)))))

(defmethod graph ((e edge))
  (graph (gsource e)))

(defmethod initialize-instance ((self graph) &key node-type edge-type node-content)
  (call-next-method)
  (setf (node-list self)
        (mapcar #'(lambda (s)
                    (make-instance (or node-type 'node)
                      :content s 
                      :graph self))
                node-content))
  (mapc #'(lambda (node1)
            (mapcar #'(lambda (node2)
                        (let ((edge (funcall #'build-edge node1 node2 (or edge-type 'edge))))
                          (when edge
                            (push edge (adj-list node1)))))
                    (node-list self)))
        (node-list self))
  self  )



(defmethod edge ((u node) (v node))
  (car (member v (adj-list u) :key #'gdestination)))

(defmethod nodes-weight  ((u node) (v node))
  (let ((edge (edge u v)))
    (if edge (gweight edge) 0)))
 
(defmethod reverse-tree-links ((G graph) (root node))
  (let ()
    ; (dolist (node (node-list G)) (setf (key node) nil))
    (dolist (node (node-list G)) (setf (childs node) nil))
    (dolist (node (node-list G))
      (when (father node)
        (push (cons (key node) node) (childs (father node)) )))
    G))


(defmethod tree-traversal ((G Graph) (root node) )
  (let ((traversal (x-append (tree-traversal-2 G root)
                                  (and (eq (traverse-tree-m G) 'pre)
                                       root))))
    traversal))

(defmethod tree-traversal-2 ((G Graph) (root node))
  (if (null (childs root))
    root
    (x-append 
     (and (memq (traverse-tree-m G)  '(pre full)) 
          root) 
     (mapcon #'(lambda (child-list) 
                  (x-append
                   (tree-traversal-2 G (cdr (first child-list)))
                   (and (memq (traverse-tree-m G)  '(in full))
                        (rest child-list)
                        root)))
              (childs root))
             ;(epw::permut-random (copy-list (key root))))
     (and (memq (traverse-tree-m G) '(post full))
          root))))
                  
(defmethod traversals ((graph graph) (root node) link &key statmode (root-n 0) (order-p '>=))
  (MST-prim graph root)
  (reverse-tree-links graph root)
  (let ((traversal (tree-traversal graph root)))
    (when statmode
      (let ((performance (loop for u in traversal
                               for v in (rest traversal)
                               collect  (nodes-weight u v))))
        (format t "~D : ~D~%"  root-n  
                (/ (float (apply #'+ performance)) 
                   (if (eq order-p '<=) 1 (+ 1 (count 0 performance)))))))
    (setf traversal
          (if  (eq link 1)
            (mapcar 'content traversal)
            (cons (content (first traversal))
                  (loop for node in traversal
                        for next-node in (rest traversal)
                        for weight = (nodes-weight node next-node)
                        if (> weight 0) collect (content next-node)
                        else collect (ameliorate (content next-node) (content node))))))
    (loop for object in traversal
          for rest on traversal  
          if (memq object rest) collect (clone object)
          else collect object)))

;; -------------------------------- Minimum Spanning Tree

(defun extract-min (queue order)
  (let ((min-node (car queue)))
    (dolist (u queue)
      (when (funcall order (key u) (key min-node))
        (setf min-node u)))
    min-node))

(defmacro pop-min (queue order)
  `(let ((minimum (extract-min ,queue ,order)))
     (prog1 minimum
       (setf ,queue (delete minimum ,queue)))))

(defmethod MST-Prim ((a-graph graph) (root node))
  (let (u tree (queue (copy-list (node-list a-graph))))
    (dolist (u queue)
      (setf (key u) (gweight-max a-graph)
            (father u) nil))
    (setf (key root) (gweight-min a-graph)
          (father root) nil)
    (while queue
      (setf u (pop-min queue (gweight-order-p a-graph)))
      (push u tree)
      (dolist (uv (adj-list u))
        (when (and (memq (gdestination uv) queue)
                   (funcall (gweight-order-p a-graph)
                            (gweight uv)
                            (key (gdestination uv))))
          (setf (father (gdestination uv)) u)
          (setf (key (gdestination uv)) (gweight uv)))))
    tree))



;------------------ interface OM
(defmethod!  make-graph ((coll t) &optional pred)
:initvals (list  () ()) 
:indoc '("A chordseq, a list of chords, a list of list of midics" "A predicate")
:doc "Builds a graph of chords to be explored by 'graph-tour'.
This graph defines and quantifies the relations inside the collection of chords.
By default, relations are based on common-notes.

Inputs :

coll : A chordseq, a list of chords, a list of list of midics.
pred : (optional) a predicate (e.g. a subpatch in lambda mode) with 2 arguments.

If pred is given, it defines an alternate relation to examine between chords.
pred must be ready to compare 2 numbers (e.g. midic) and answer T or Nil.
By default, pred is the equality predicate.
Example : pred = (lambda (x y) (= (abs (- x y)) 100))
will quantify the chromatic-step relation instead of common-notes.

Output :

A graph object."
:icon 250

  (let ((closure #'=))
    (when pred  (setf closure pred))
    (when (subtypep (type-of coll) 'chord-seq)
      (setf coll (loop for midics in (LMidic coll)
                       for vels in (LVel coll)
                       for durs in (LDur coll)
                       for chans in (LChan coll)
                       collect (mki 'chord :LMidic midics :LVel vels :LDur durs :Lchan chans))))
    (when (subtypep (type-of (first coll)) 'chord)
      (setf coll (mapcar #'clone coll))
      )
    (make-instance 'graph 
      :node-content coll
      :build-edge-m #'(lambda (c1 c2) (com-sub-structure c1 c2 closure))
      :gweight-threshold-p #'(lambda (w) (declare (ignore w)) t))))


(defmethod! graph-tour ((graph t) 
                        (solu integer) 
                        (link integer)
                        (order integer)
                        (trav integer)              
                        (stat integer))
  :initvals (list  () 0 1 1 1 1)
  :indoc '("a Graph" "A number" "A number""A number""A number""A number")
  :icon 250
  :menuins '( (2 ( ("with link" 1) ("without link" 2) ))
             (3 ( ("maximize" 1) ("minimize" 2) ))
             (4 ( ("short path" 1) ("long path" 2) ))
             (5 ( ("one solution" 1) ("statistics" 2) )) )
  :doc "Builds a (quasi-) optimal path between chords that have been organized into
a graph with the box 'make-graph'. If the relation used in make-graph is the amount
of common notes, graph-tour delivers a sequence of chords where the amount of common
notes between successive chords has been maximized (or minimized). There are as many different
solutions as there are nodes (i.e. chords) in the graph.

parameters

graph : the output of a 'make-graph' box
solu : positive integer. Choose a solution between 0 and n-1 (n is the number of chords)
link : (optional, menu) if 'with link' adds a low common note when there is no common notes between 2 chords.
order : (optional, menu) if 'maximize' look for path that maximizes the relations (default). If 'minimize' look for path of maximum contrast.
trav : (optional, menu) if 'short' (default) short path without repetitions. If 'long' long path with repetitions.
stat : (optional, menu) if 'one solution' (default) output the solu(nth) solution. If 'statistics' print all the solutions with an 
optimality factor.

output

Depends on the kind objects that have been put into the graph (see 'make-graph') : 
If the graph was built with a list of lists  of integers, output is a list of lists of integers.
If the graph was built with a list of chord-objects or a chord-line object, output is list of chord objects.
The output is generally connected to the 'chords' input of a 'chordseq' box.
"
  
  
  
  (let ((root (nth solu (node-list graph)))  order-p traversal-m)
    (when  (eq link 0) (setf link 1))
    (when (eq order 0) (setf order 1))
    (when (eq  trav 0) (setf trav 1))
    (setf order-p
          (case order (1 '>=) (2 '<=)))
    (setf traversal-m
          (case trav (1  'pre) (2  'full)))
    (case order-p
      (<= (setf (gweight-min graph) 0
                (gweight-max graph) 1000000))
      (>= (setf (gweight-min graph) 1000000
                (gweight-max graph) 0)))
    (setf (gweight-order-p graph) order-p)
    (setf (traverse-tree-m graph) traversal-m)
    (cond
     ((/= stat 2) (traversals graph root link))
     (t
      (loop for root-n from 0 upto (1- (length (node-list graph)))
            for root in (node-list graph)
            for traversal = (traversals  graph root link :statmode t :root-n root-n :order-p order-p)
            finally (return traversal))))))




#|
(defunp mk-pred ((val integer (:value 0)) (tol fix>=0 (:value 0))) all-types ""
  (eval
   `(let ((delta ,tol) (val ,val))
      (function (lambda (x y) (<= (abs (- val (- y x) )) delta))))))

(defunp mk-pred ((val integer (:value 0)) (tol fix>=0 (:value 0)) &rest (v integer)) all-types 
        "This box is used in conjunction with the 'make-graph' box. It defines a predicate
used to compare elements in the objects (e.g. chords) put into the graph. Each element x (e.g. note)
of each object (e.g. chord) is compared to each element y of every other object. Then (y-x) is 
compared for equality to the parameter <val>, with the tolerance <tol>.
Thus, for <val> = 0 and <tol> = 0, strict equality (e.g. common notes relation) is seek.
For <val> = 100, hal-tone upward step relation is seek. If <tol> = 25, then a quarter tone
tolerance is allowed. If you build a graph using 'make-graph' with these values, then find an optimal path
using 'graph-tour', what you get is a chord sequence where there is a maximum number of half-tone steps
between 2 consecutive chords, with a quarter tone tolerance.
If you add optional arguments (as many as you like), these values will be used to complexify the relation.
For instance, with <val> = 300, <opt-arg1> = 400, <opt-arg2> = 700,the optimisation 
will be : 'find a sequence where consecutive chords have the max amount of minor 3rd, major 3rd and
perfect 5th upward steps.'


parameters

val : integer, value to be compared with the difference between notes of chords.
tol : integer, allowed deviation in the former comparison.
arg : (optional, integer) additional value to be used like <val>

output

a predicate function object to be connected to the 'pred' input of a 'make-graph' box.
"
  (eval
   `(function (lambda (x y) 
                (or  
                 ,. (mapcar #'(lambda (value) `(<= (abs (- ,value (- y x) )) ,tol))
                            (cons val v)))))))

                      

|#

; ------------------- Application sur les objets sonores 

(defmethod com-sub-structure ((s1 list) (s2 list) (inter-p function)) 
  (let ((inter (x-intersect  s1 s2 inter-p)))
    (values inter (length inter))))

(defmethod com-sub-structure ((s1 chord) (s2 chord) (inter-p function))
  (let ((inter (x-intersect
                (LMidic s1)
                (LMidic s2)
                inter-p)))
    (values inter (length inter))))

(defmethod ameliorate ((s1 list) (s2 list))
  (cons (apply #'min s2) s1) )

(defmethod ameliorate ((s1  chord) (s2  chord)) s1)
  
(defun eq-mod-12 (x y) (= (mod x 1200) (mod y 1200)))

(defun mk-joint-step (i-min i-max)
  #'(lambda (x y)
      (and (<= (abs (- x y)) i-max)
           (>= (abs (- x y)) i-min))))


; ------------------- chord utilities




(defmethod next-same-note ((self note))
  (and (parent self)
       (let ((next-obj (next-container 
                        (if (subtypep (type-of (parent self)) 'chord) 
                          (parent self) 
                          self) 
                        '(chord note))))
         (and next-obj
              (if (subtypep (type-of next-obj) 'chord)
                (find (midic self) (inside next-obj) :key 'midic)
                next-obj)))))

(defmethod tie-same-note ((self note))
  (when (or (not (tie self)) (equal (tie self) 'end))
    (loop for note = (next-same-note self) then (next-same-note note)
          while note
          with note2
          do (setf (tie note) 'continue note2 note)
          finally (when note2 (setf (tie note2) 'end (tie self) (if (tie self) 'continue 'begin))))))

(defmethod tie-same-note ((self voice))
  (loop for chord = (first-container self '(chord)) then (next-container chord '(chord))
        while chord
        do (loop for note in (inside chord)
                 do (tie-same-note note)))
  self)
    




(defmethod! tie-all ((self voice))
  :initvals (list ())
  :indoc '("a Voice")
  :doc "Ties all notes having the same pitch"
  :icon 250
  (tie-same-note (clone self)))



;(OMAddMethod 'graph-tour *package-user* :protect t)
;(OMAddMethod 'make-graph *package-user* :protect t)
;(OMAddMethod 'tie-all *package-user* :protect t)


