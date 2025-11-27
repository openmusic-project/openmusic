(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;TOOLS;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;maybe move in tree functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;INSERT-GRACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Uniquement au niveau tree


(defmethod treeobj-p ((self treeobj)) t)
(defmethod treeobj-p ((self t)) nil)

#|
(defmethod transpose-tree ((tree list))
  (setf *indx* 0)
    (trans-index-tree tree))

(defun trans-index-tree (tree)
"transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes. This version omits gnotes (0)."
  (if (atom tree)
      (if (or (zerop tree) (floatp tree))
          tree
        (prog1
            (make-instance 'treeobj :tvalue tree :tindex *indx*)
          (incf *indx*)))
    (list (first tree) (mapcar 'trans-index-tree (second tree)))))
|#

;(setf *tree* '(3/2 (((6 4) (1 -1 1 2.0 (1 (1 1 1)))))))

(defmethod transpose-tree ((tree list))
  (setf *indx* 0)
  (setf *r-indx* nil)
  (prog1
  (trans-index-tree tree)
    (setf *indx* 0)
    (setf *r-indx* nil)))

(defun trans-index-tree (tree)
"transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes. This version omits gnotes (0)."
(if (atom tree)
    (cond 
     ((or (zerop tree) (floatp tree)) tree)
     ((minusp tree)
      (prog1
            (make-instance 'treeobj :tvalue tree :tindex *indx*)
          (setf *r-indx* 't)
          (incf *indx*)))
      (t (prog1
             (make-instance 'treeobj :tvalue tree :tindex *indx*)
           (setf *r-indx* nil)
           (incf *indx*))))
     (list (first tree) (mapcar 'trans-index-tree (second tree)))))


(defun insert-n-gn (tree pos n)
  (cond  
   ((treeobj-p tree) 
    (if (= pos (tindex tree))
        (progn
          (setf (tvalue tree) (x-append (repeat-n 0 n) (tvalue tree)))
          tree)
      tree))
   ((atom tree) tree)
   (t (list (first tree) (mapcar #'(lambda (x)(insert-n-gn x pos n)) (second tree))))))

;(render-grace (trans-obj (insert-n-gn (transpose-tree *tree*) 0 3)))

(defun flat-grace-ryt (tree)
  (let ((buff nil)
        (pos
         (position-if #'(lambda (x) (unless (atom x) 
                                      (and (all-atoms? x)
                                           (zerop (car x))
                                           ))) tree)))
    (if pos
        (progn
          (setf buff (nth pos tree))
          (setf (nth pos tree) nil)
          (delete nil tree)
          (loop for i in (reverse buff)
                do 
                  (if (zerop pos)
                      (push i tree)  
                    (push i (nthcdr pos tree))))
          (remove nil tree))
      tree
      )))
  
(defun render-grace (tree)
  (if (atom tree) tree
  (list (first tree) 
        (if (atom (second tree))
            (second tree)
          (mapcar 'render-grace (flat-grace-ryt (second tree)))))))


(defmethod* insert-graces ((tree list) (pos number) (n number))
  (let* ((trans (transpose-tree tree))
         (ins (insert-n-gn trans pos n)))
    (render-grace (trans-obj ins))))

;(insert-graces *tree* 1 5)

(defmethod* insert-graces ((tree list) (pos list) (n list))
  (let ((clone tree))
    (loop for p in pos
          for g in n
          do (setf clone (insert-graces clone p g)))
    clone))

;(insert-graces *tree* '(0 3 0) '(1 2 1) )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;FORMAT GRACE TREE;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Make grace-groups
;;doit venir toujours apres insert-graces..

#|
(defun groupsame (liste)
  (if (null liste)
    liste
    (let* ((first (car liste))
           (rest (rest liste))
           )
      (if (atom first)
       (cons (cons first (loop while (equal first (first rest))
        collect (pop rest)))
             (groupsame rest))
        (cons (groupsame first) (groupsame rest))))))
|#       

;;;;;;;
;;we list every zero
(defun groupdazero (liste)
  (cond ((and (equal 0 liste) (atom liste))
         (list liste))
        ((atom liste) liste)
        (t (list (first liste) (mapcar 'groupdazero (second liste)))
           )))

;;we concat every zero
;(setf *conne* '(1 (1 (1 1 1)) (0) (0) (0) (0) (0) 1 1 1))
;(setf *conne* '((1 (1 1 1)) 1 (0) (0) (0) (0) (0) 1 1 1))
;(setf *conne* '((1 (1 1 1)) 1 (0) (0) (0) (0) (0) 1 1 (0) 1 1))
;(setf *conne* '((1 (1 1 (0) (0) 1)) (0) (0) (0) 1 1 (0) 1))

(defun concatzero (tree)
  (let ((res))
    (loop for i in tree
          do (if (or (not (equal '(0) i)) (atom (car res)))
                 (push i res)
               (if (not (equal (caar res) 0))
                   (push i res)
                 (push (car i) (car res)))))
    (reverse res)))

;(concatzero *conne*)

;we concat recursively
(defun grpcnt (tree)
  (if (atom tree) 
      tree
    (if (listp (second tree))
        (list (first tree) 
              (mapcar 'grpcnt (concatzero (second tree))))
      tree
      )))

;we add group-gn recursively

    
(defun makegngtree (tree)
  (cond 
   ((atom tree) tree)
   ((and (listp tree) (equal 0 (car tree))) 
    (if (= (length (remove nil tree)) 1)
        (car tree)
      (let* ((lgt (length (remove nil tree)))
             (newtree (repeat-n 1 lgt)))
        (list 0 newtree))))
   (t (list (first tree) (mapcar 'makegngtree (second tree))))))

(defun format-gn-g-tree (tree)
  (if (atom tree) tree
  (if (listp tree)
      (if (atom (second tree)) 
          tree
        (if (equal 0 (car (second tree)))
            (let* ((lgt (length (second tree)))
                   (newsecondtree (repeat-n 1 lgt)))
              (list 0 newsecondtree))
          (list (car tree) (mapcar 'format-gn-g-tree (second tree))))))))


(defun format-grace-notes (tree)
  (makegngtree (grpcnt (groupdazero tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
;;faire une fonction pour mettre les indices pos qui viennent de omquantify
;; avec rien que les pos.

(defmethod* insert-sgraces ((tree list) (pos list))
  (let ((clone tree))
    (loop for p in pos
          do (setf clone (insert-graces clone p 1)))
    clone))

(defmethod insert-graces-notes ((self list) (positions list))
  "inserts grace ntoes in a tree following positions and outputs
a new tree accordingly:
- simple grace note = 0
- group of grace notes = (0 ( 1 1 ..n))"
  (let ((ins (insert-graces self positions 1)))
    (format-grace-notes ins)))
  
|#        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;old version
#|
(defun remove-tree-graces (tree)
  "removes graces (0) from tree"
  (if (atom tree) 
      (unless (zerop tree) tree)
    (list (first tree) (remove nil (mapcar 'remove-tree-graces (second tree))))))
|#

;new version

(defmethod! remove-tree-graces ((tree t))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("a rhythm tree")
  :icon 225
  :doc "removes non-destructively graces (0) and (0 ( 1 1 1)) from tree"  
   (if (atom tree) 
       (if (not (eq 0 tree)) tree)
     (if (and (atom (car tree)) (eq 0 (car tree)))
         nil
    (list (first tree) (remove nil (mapcar 'remove-tree-graces (second tree)))))))


(defun gracenotes-insert (tree pos n)
   (render-grace 
    (trans-obj (insert-n-gn (transpose-tree tree) pos n))))


(defmethod add-tree-graces ((tree list) (pos list) (num list))
  "where tree is a tree, 
pos are position of chords and rests not cont-chords!
num is number of graces BEFORE pos" 
  (let ((res tree))
    (loop for p in pos
          for n in num
          do 
            (setf res (gracenotes-insert res p n)))
    (format-grace-notes res)))

;(add-tree-graces *tree* '(1 2 4 5) '(1 3 6 4))
;(add-tree-graces *tree* '(1 2 3) '(1 3 4))
;(add-tree-graces *tree* '(1 3 4 5) '(1 3 4 5))
;(add-tree-graces *tree* '(1 2 3) '(1 3 4 ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-grace-pos ((self voice))
  (let* ((objs (collect-chords-and-rests self))
         (gtrees 
          (loop for i in objs 
                       collect (if (mus-const i) (length (mus-const i)))))
         (pos (remove nil 
                      (loop for i in gtrees
                            for n from 0 to (length gtrees)
                             collect  (if i n))))) 
    (when pos
    (list pos (remove nil gtrees)))))