;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
;;; Music package 
;=========================================================================

(in-package :om)


;;;;;;;;;;;;;;;;;;;;;;;;;REDUCETREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun grouper1 (liste)
"groups succesive floats"
  (if (null liste)
    liste
    (let* ((first (car liste))
           (rest (rest liste))
           )
      (if (numberp first)
        (if (plusp first)
          (cons (+ first (loop while (and (numberp (first rest)) (floatp (first rest)))
                               sum (round (pop rest))))
                (grouper1 rest))
          (cons first (grouper1 rest)))
        (cons (grouper1 first) (grouper1 rest))))))
                
                  

(defun grouper2  (liste)
"groups succesive rests (-1) into one"
  (if (null liste)
    liste
    (let* ((first (car liste))
           (rest (rest liste)))
      (if (numberp first)
        (if (plusp first) 
          (cons first (grouper2 rest))
          (cons (+ first (loop while (and (integerp (first rest)) (minusp (first rest)))
                               sum  (pop rest)))
                (grouper2 rest)))
        (cons (grouper2 first) (grouper2 rest))))))
 

(defun grouper3 (liste)
"reduces concatenated rests in the form of (1(-3)) into -1"
  (if (atom  liste)
    liste
    (if (and (numberp (first (second liste)))
             (minusp (first (second liste)))
             (null (rest (second liste)))
             (not (listp (first liste))))
      (- (first liste))
      (list (first liste)
            (mapcar 'grouper3 (second liste))))))

(om::defmethod! reduced-tree ((tree list))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("tree")
   :icon 134
   :doc "reduces and simplifies a tree by concatenating consecutive rests and floats
into a single correct note"
   (let ((liste (resolve-? tree)))
(grouper3 (grouper2 (grouper1 liste)))))



#|
(om::defmethod! reducetree ((self voice))
   :initvals '(t)
   :indoc '("voice")
   :icon 134
   :doc "reduces and simplifies a tree by concatenating consecutive rests and floats
into a single correct note"
   (let* ((newvoice (clone self))
          (newtree (reducetree (tree newvoice))))
     (setf (tree newvoice) newtree)
     newvoice))

|#




(om::defmethod! reducetree ((tree list))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("tree")
   :icon 160
   :doc "reduces and simplifies a tree by concatenating consecutive rests and floats
into a single correct note"
   (let ((liste (reduced-tree tree)))
     (loop
       while (not (equal liste tree))
       do 
       (setf tree liste)
       (setf liste (reduced-tree liste)))
     (remove nil liste)))


;;;;;;;;;;;;;;;;;;;;;;;;;PULSEMAKER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defmethod! pulsemaker ((measures list) (beat-unit list) (n-pulses list))
 :initvals (list '(4 4) '(8 8) '(4 (1 1 1 1)))
 :indoc '("mesures" "beat-unit" "n-pulses")
 :icon 134
 :doc "ooo"
 (let*((pulses (mapcar 'cons measures
          (mapcar #'(lambda (x) 
                      (if (listp x) (list x) (list (om::repeat-n 1 x)))) n-pulses)))

       (mes (mapcar #'(lambda (x y) (list x y)) measures beat-unit))
       (tree (mapcar #'(lambda (x y) (list x (list y))) mes pulses)))
   (list '? tree)))

(om::defmethod! pulsemaker ((measures list) (beat-unit number) (n-pulses list))
  (let* ((lght (length measures))
        (bt-unt-lst (repeat-n beat-unit lght)))
    (pulsemaker measures bt-unt-lst n-pulses)))

(om::defmethod! pulsemaker ((measures number) (beat-unit list) (n-pulses list))
  (let* ((lght (length beat-unit))
        (measure-lst (repeat-n measures lght)))
    (pulsemaker measure-lst beat-unit n-pulses)))

(om::defmethod! pulsemaker ((measures number) (beat-unit number) (n-pulses list))
  (let* ((lght (length n-pulses))
        (bt-unt-lst (repeat-n beat-unit lght))
        (measure-lst (repeat-n measures lght)))
    (pulsemaker measure-lst bt-unt-lst n-pulses)))



;;;;;;;;;;;;;;;;;;;;;;;;;PRIMEPULSE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defmethod! prime-pulse ((mesures list) (subdiv list))
 :initvals (list '(1 2) '(1 2))
 :indoc '("list1" "list2")
 :icon 134
 :doc "ooo"
  (mapcar 'cons mesures
          (mapcar #'(lambda (x) (list (om::repeat-n 1 x))) subdiv)))




;;;;;;;;;;;;;;;;;;;;;;;;;TIETREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! tietree ((tree t))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("tree")
   :icon 134
   :doc "converts all rests into floats" 

   (cond
    ((and (atom tree) (> tree 0)) tree)
    ((atom tree) (* (* tree -1) 1.0))
    (t (list (first tree)
                         (mapcar 'tietree (second tree))))))


(om::defmethod! tietree ((self voice))

  :initvals '(t ) 
  :indoc '("self")
  :icon 134
  :doc  "removes all rest and change them into continuation-chords 
in a voice or in a poly object"

(let* ((newself (clone self))
       (tempo (qtempo self))
       (tree (tree self))
       (tree (tietree tree))
       (verynewself (setf newself (make-instance 'voice :tree tree :chords (chords self)))))
 (progn 
   (change-tempo verynewself tempo)
    verynewself)))


(om::defmethod! tietree ((self poly))

  :initvals '(t ) 
  :indoc '("self")
  :icon 134
  :doc  "removes all rest and change them into continuation-chords 
in a voice or in a poly object"

(let* ((voices (mapcar #'(lambda (x) (tietree x))
          (inside self)))
       (newpoly (make-instance 'poly :voices voices)))
       newpoly ))


;;;;;;;;;;;;;;;;;;;;;;;;;REMOVE-RESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform-rests (list)
"traces a simple list coming from trans-tree and flattened according to:
if note encoutered, then floats are checked and transformed into rests, else
rests encoutered and either other rests or errounous floats are transformed 
into notes. From Gerard."
    (loop while list
        for courant =  (pop list)
        ;do (print (mapcar 'tvalue list))
        ;do (print (tvalue courant))
        do (if (and (integerp (tvalue courant)) (minusp (tvalue courant)))
             (progn
             (setf  (tvalue courant) (- (tvalue courant)))
             (loop while (and list (not (and (integerp (tvalue (first list))) (plusp (tvalue (first list))))))
                   do (setf (tvalue (car list)) (float (abs (tvalue (car list)))))
                   (pop list))))))



(om::defmethod! remove-rests ((tree t))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("tree")
   :icon 134
   :doc "All rests are transformed by durations."
(let* ((liste (if (typep tree 'voice) (tree tree) tree))
       (tree2obj (trans-tree liste))
       (tree2objclean (remove-if 'numberp (flat tree2obj)))
       (treeobjinverted (transform-rests tree2objclean)))
  (trans-obj tree2obj)))




;;;;;;;;;;;;;;;;;;;;;;;;;FILTERTREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform-notes-flt (list places)
""

  (loop while list
        for courant =  (pop list)
        do (if (and (and (integerp (tvalue courant)) (plusp (tvalue courant))) (member (tindex courant) places))
             (progn
               (setf  (tvalue courant) (- (tvalue courant)))
               (loop while (and list (floatp (tvalue (car list)))) 
                     do (setf (tvalue (car list)) (round (- (tvalue (car list)))))
                     (pop list))))))





(defun trans-note-index (treeobjlist)
  "puts index only on expressed notes and not on floats or rests (for filtering purposes)."
(if (atom treeobjlist)
  (if  (and (typep treeobjlist 'treeobj) (integerp (tvalue treeobjlist)) (plusp (tvalue treeobjlist)))
    (setf (tindex treeobjlist) (incf n)) treeobjlist)
    (list (first treeobjlist) (mapcar 'trans-note-index (second treeobjlist)))))



(om::defmethod! filtertree ((tree t) (places list))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) '(0 1))
   :indoc '("tree" "places")
   :icon 134
   :doc "replaces expressed notes in given places <places> with rests ."
(setf n -1)
(let* ((liste (if (typep tree 'voice) (tree tree) tree))
       (tree2obj (trans-tree liste))
       (tree2o (trans-note-index tree2obj))
       (tree2objclean (remove-if 'numberp (flat tree2obj)))
       (treeobjinverted (transform-notes-flt tree2objclean places )))
  (trans-obj tree2obj)))



;;;;;;;;;;;;;;;;;;;;;;;;;N-PULSES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;peut-etre probleme
(defvar *npulse-lst* nil)

(defun real-copy-list (list)
  (loop for item in list 
       collect 
       (cond
        ((listp item) (real-copy-list item))
        (t item))))



(defun give-pulse (liste )
  (if (null liste) nil
      (dolist (n (second liste) *npulse-lst*)
        (cond  
         ((atom n) (push n *npulse-lst*))
         ((listp n) (give-pulse n ))
         (t nil)))))




(defun pulse (mesure) 
  (let ((lst '()))
  (reverse (om::flat (mapcar #'(lambda (x) 
                                  (give-pulse x))
                              mesure)))))

(defun pulses (mesures)
            "retourne le nombre de pulses (pas les pauses) d'une RTM"
  
    (om::flat (mapcar #'(lambda (x) (pulse (list x))) mesures)))

(defun om-pulses (tlist)
(mapcar #'(lambda (x)
            (pulse (list (cons '() (list x)))))
        tlist))


(defun find-po (list)
(remove '()
  (mapcar #'(lambda (x y)
           (if (floatp x) nil y ))
        list (om::arithm-ser 0 (length list) 1))))
         

(om::defmethod! group-pulses ((liste list))
  
  :initvals (list  '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("tree")
  :icon 134
  :doc "groupe les pulses"
  
  (let* ((tree 
          (second 
           (om::mat-trans 
            (om::flat-once 
             (om::mat-trans (rest (real-copy-list liste)))))))
         (the-pulses (om::flat(om-pulses tree))))
         
      
(om::group-list the-pulses
                (om::x->dx (om::remove-dup
                            (om::flat 
                             (list 0 (find-po the-pulses) (length the-pulses)))
                            'eq 1))
                'linear)))


(om::defmethod! n-pulses ((tree t))
   
   :initvals (list  '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))) 
   :indoc '("tree")
   :icon 134
   :doc "retourne le nombre de pulses dans un arbre"
   (let (( liste (if (typep tree 'voice) (tree tree) tree))
         (*npulse-lst* nil))
     (length
      (remove nil
              (mapcar #'(lambda(x) (if (> (first x) 0) x nil)) (om::group-pulses liste))))))



;;;;;;;;;;;;;;;;;;;;;;;;;REVERSETREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The class is defined in order to access easily the try consecutives kind:
;plusp integers, minusp integers and floats (pulse, rests and tied notes)


(defclass! treeobj ()
((tvalue :initform 1 :accessor tvalue :initarg :tvalue :type t)
 (tindex :initform 0 :accessor tindex :initarg :tindex :type t)))

(defun trans-tree (tree)
"transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes."
  (if (atom tree)
    (make-instance 'treeobj :tvalue tree)
    (list (first tree) (mapcar 'trans-tree (second tree)))))


(defun trans-obj (tree)
  (if (atom tree)
    (if (typep tree 'treeobj) (tvalue tree) tree)
    (list (first tree) (mapcar 'trans-obj (second tree)))))


(defun group-ties (liste)
"liste is a liste of treeobjs"
(let* (res)
  (loop for i in liste
        do (cond 
            ((and (plusp (tvalue i)) (not (floatp (tvalue i))))
               (push (list i) res))
            ((floatp (tvalue i))
               (push i (car res)))
            (t))
            )
  (loop for i in res
        do (if (> (length i) 1)
             (progn 
               (setf (tvalue (first i)) (round (tvalue (first i))))
               (setf (tvalue (last-elem i)) (* 1.0 (tvalue (last-elem i)))))))))


(defun reversedties (tree)
   
(let* ((liste (if (typep tree 'voice) (tree tree) (resolve-? tree)))
       (tree2obj (trans-tree liste))
       (tree2objclean (group-ties (remove-if 'numberp (flat tree2obj)))))
  (trans-obj tree2obj)))


(defun reversedtree (tree)
  (if (atom tree)
    tree
    (list (first tree)
          (reverse (mapcar 'reversedtree (second tree))))))


(om::defmethod! reversetree ((tree t))
  :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
  :indoc '("tree")
  :icon 134
  :doc "Reverses a rhythmic tree"
(let ((tree (if (typep tree 'voice) (tree tree) tree)))
       (reversedtree (reversedties tree))))




;;;;;;;;;;;;;;;;;;;;;;;;;SUBST-RHYTHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trans-tree-index (tree)
  "transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes and 
marks the index of events."
  (if (atom tree)
    (make-instance 'treeobj :tvalue tree
                   :tindex (incf n))
    (list (first tree) (mapcar 'trans-tree-index (second tree)))))




(defun substreeall (list pos elem)
  (loop 
    for i from 0 to (length pos)
    for a in pos
    collect (if (listp (nth i elem)) 
              (setf (tvalue (nth a list)) (list (abs (floor (tvalue (nth a list)))) (nth i elem)))
              (setf (tvalue (nth a list)) (nth i elem)))))
           





;pour eviter les tree avec '?'
;ATTENTION!! numtree transforme les 5  en (4 1.0) etc...
;ceci pourrait fausser les transformations
; toutefois il est utlisé pour regler l'histoire de ?
(defun numtree (tree)
  (let* ((newvoice (make-instance 'voice :tree tree)))
    (tree newvoice)))



; This is for both trees from measures & VOICES

(defun optimize-tree (tree )
  "this function optimizes trees modified by rotate-tree and subst-rhythm methods
by grouping each measure as a group, in order to correctly read the new rhythm 
outputed."
  
  (if (or (equal '? (first tree)) (atom (first tree))) 
    (let* ((header (first tree))
           (info (second tree))
           (splitree (mat-trans info))
           (signatures (first splitree))
           (measures (second splitree))
           (opt-meas (mapcar #'(lambda (x) (list (list 1 x))) measures))
           (withmes (mapcar #'(lambda (x y) (list x y)) signatures opt-meas)))
      (list header withmes))
    (let* ((signatures (first tree))
           (measures (second tree))
           (opt-meas (list (list 1 measures))) 
           )
      (list signatures opt-meas))))



;In Om all 5 and 7 etc.. are transformed by voice and numtree function
; in (4 1.0) as tied notes. If we need to permut trees as expressed 
; i.e 5 and not (4 1.0) use reduce mode. (here we use reducetree function!)
; and finally remove this option and put it by default in reduce mode
; for accurate computation!


(om::defmethod! subst-rhythm ((tree t) 
                                 (pos list)
                                 (elem list)
                                 &optional 
                                 (option 'reduce)
                                 (output 'optimized))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) nil '(1 2)  'reduce 'optimized)
   :indoc '("tree" "pos" "list" "option" "output")
   :menuins '((3 (("reduce" 'reduce) 
                 ("tied" 'tied)))
             (4 (("optimized" 'optimized) 
                 ("not-optimized" 'not-optimized))))
   :icon 134
   :doc "substitutes elements in tree
<elem> input is a list that can accept either atoms or lists  or both.
Atoms are pulses. Lists will be proportions
creating groups . For example a list (1 1 1) substituting 2 will yield (2 (1 1 1)).
<pos> if left nil will substitute from 0 position until the end of list <elem>. 
If the positions are specified (a list) each nth elem of tree being pulses will be replaced
sequentially by elements from <elem>."
   (setf n -1)
   (let* ((liste (if (typep tree 'voice) 
                   (if (equal option 'reduce) (reducetree (tree tree)) (tree tree))
                   (if (equal option 'reduce) (reducetree (numtree tree)) (numtree tree))))
          (position (if (null pos)
                      (loop for i from 0 to (1- (length elem))
                            collect i) (first-n pos (length elem) )))
          (tree2obj (trans-tree-index liste))
          (tree2objclean (remove-if 'numberp (flat tree2obj)))
          (treeobjinverted 
           (substreeall tree2objclean position elem)))
     (case output
       (optimized (optimize-tree (trans-obj tree2obj)))
       (not-optimized (trans-obj tree2obj)))))




;;;;;;;;;;;;;;;;;;;;;;;;;INVERT-RHYTHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform (list)
"traces a simple list coming from trans-tree and flattened according to:
if note encoutered, then floats are checked and transformed into rests, else
rests encoutered and either other rests or errounous floats are transformed 
into notes. From Gerard."
    (loop while list
        for courant =  (pop list)
        ;do (print (mapcar 'tvalue list))
        ;do (print (tvalue courant))
        do (cond
            ((and   (integerp (tvalue courant)) (plusp (tvalue courant)))
             (setf (tvalue courant) (- (tvalue courant)))
             (loop while (and list (floatp (tvalue (car list))))
                   do (setf (tvalue (car list)) (round (- (tvalue (car list)))))
                    (pop list)))
            ((and (integerp (tvalue courant)) (minusp (tvalue courant)))
             (setf  (tvalue courant) (- (tvalue courant)))
             (loop while (and list (not (and (integerp (tvalue (first list))) (plusp (tvalue (first list))))))
                   do (setf (tvalue (car list)) (float (abs (tvalue (car list)))))
                   (pop list))))))



(om::defmethod! invert-rhythm ((tree t))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("tree")
   :icon 134
   :doc "Inverts the rhythm such as every note becomes a rest and every
rest becomes a note."
(let* ((liste (if (typep tree 'voice) (tree tree) (resolve-? tree)))
       (tree2obj (trans-tree liste))
       (tree2objclean (remove-if 'numberp (flat tree2obj)))
       (treeobjinverted (transform tree2objclean)))
  (trans-obj tree2obj)))




;;;;;;;;;;;;;;;;;;;;;;;;;TREERATIO;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;from jean bresson

(defun tree-to-ratios (list)
  (loop for mesure in (cadr list)
	collect
	(let* ((signature (car mesure))
	       (values (cadr mesure))
	       (ratios (mesure-ratios values)))
	  (om/ 
	   (om* ratios (car signature)) 
	   (cadr signature)))))





(defun mesure-ratios (list)
  (let ((div (round (loop for elt in list sum (abs (if (listp elt) (car elt) elt))))))
    ;(print div)
    (flat (loop for elt in list
		collect (if (listp elt)
                            (om* (/ (round (car elt)) div) (mesure-ratios (cadr elt)))
                            (/ (round elt) div)))
        )))



;;this is to get the floats working correctly
;;this is me
(defun get-s-from-meas (tree)
  (if (atom tree)
    tree
   (mapcar 'get-s-from-meas (second tree))))


(defun get-s-by-mes (tree)
  (loop 
    for elt in (cadr tree)
    collect (flat (get-s-from-meas elt))))


(defun correct-measurefloats (tree)
  (let* (res
         (pulses (flat (get-s-by-mes tree)))
         (ratios (flat (tree-to-ratios tree))))

    (loop 
      for p in pulses
      do ( if (floatp p)
              (progn 
                (setf (car res) (+ (car res) (car ratios)))
                (pop ratios))
              (progn 
              (push (car ratios) res)
              (pop ratios))))
    (reverse res)))



(om::defmethod! treeratio ((tree t))
  :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
  :indoc '("tree")
  :icon 134
  :doc "Converts a tree into a list of ratio where 
1/4 is a quarter note, 1/8 is an eight note etc.....
[modified version of tree2ratio]"
  (correct-measurefloats tree))


;;;;;;;;;;;;;;;;;;;;;;;;;ROTATE-TREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rotate-by measure la ne veut rien dire:
;;example qud on a une seconde mesure commencant par une liaison 
;;donc qui appartien t a la premiere mesure on a un probleme!!!

(om::defmethod! rotatetreepulse ((tree t) (nth integer))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) 1)
   :indoc '("tree" "nth")
   :icon 134
     :doc "Returns a circular permutation of a copy of <tree> durations starting from its <nth> 
element"

   (let* ((ratios (treeratio tree))
          (signatures (mapcar 'car (cadr tree)))
          (rotation (rotate ratios nth)))
     (mktree rotation signatures)))





(defun get-all-treeobj (tree)
  (remove nil      
          (mapcar #'(lambda (x) 
                      (if (typep x 'treeobj) x))
                  (flat tree))))




(defun permtree (list nth)
(let* ((listobj (rotate list nth))
       (values (mapcar 'tvalue listobj))
       )

  (loop 
    for i from 0 to (1- (length listobj))
    for a in values
    collect (setf (tvalue (nth i list)) a))))




;In Om all 5 and 7 etc.. are transformed by voice and numtree function
; in (4 1.0) as tied notes. If we need to permut trees as expressed 
; i.e 5 and not (4 1.0) use reduce mode. (here we use reducetree function!)
; and finally remove this option and put it by default in reduce mode
; for accurate computation!



(om::defmethod! rotate-tree ((tree t) (nth integer))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) 1)
   :indoc '("tree" "nth")
   :icon 134
   :doc "rotates tree following nth."
   (setf n -1)
   (let* ((tree2obj (trans-tree-index tree))
          (tree2objclean (get-all-treeobj tree2obj))
          (treepermute (permtree tree2objclean nth)))
     (trans-obj tree2obj)))



(om::defmethod! rotatetree ((tree t) (nth t) 
                                &optional (mode 'pulse))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) 1 'pulse)
   :indoc '("tree" "nth" "mode")
   :menuins '((2 (("pulse" 'pulse) 
                 ("prop" 'prop))))
             
   :icon 134
   :doc " Applies a rotation to the tree pulses following <nth> position.
If <mode> is equal to 'pulse' this will be applied to pulses of tree meaning 
that it is applied to expressed durations.
If <mode> is equal to 'prop' this will be applied to proportions of tree
meaning the integers of tree.
."
     (if (equal mode 'pulse)
       (rotatetreepulse tree nth)
       (rotate-tree tree nth)))
       

;;;;;;;;;;;;;;;;;;;;;;;;;TREE-CANON;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

;;pas pour le moment !!!!

(om::defmethod! tree-canon ((tree t) (unite number) (nbr number))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) 1/4 1)
   :indoc '("tree" "ratio" "nth-times")
   :icon 134
   :doc "decale le rythm de <self> de n fois donne en <nbr>
d'unitees rythmiques donne en <unite>. En sachant que :
Une ronde -> 1
Une blanche -> 1/2
Une noire -> 1/4
Une croche -> 1/8
etc.....
"
   (let* ((measures (car (mapcar 'car (cadr tree))))
          (ratios (treeratio tree))
          (offset (om* -1 (apply '+ (repeat-n unite nbr))))
          (decal (x-append offset ratios)))
     (mktree decal measures)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;MAKE-TREE-GROUPS;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun grplst (lst grp)
  (let* ((grouped (loop 
                    for a in grp
                    collect (prog1
                              (first-n lst  a)
                              (setf lst (nthcdr a lst))))))
    (remove nil (if lst (x-append grouped (list lst)) grouped))))


(defmethod! maketreegroups ((figures list)
                              (sequence list)
                              (measures list))
   :initvals (list '((1 1 1) (1 2 1) (3 4) (1 1 1 1))
                   '(0 3 0 2 0 1 0 0 3) 
                   '((4 4)))
 :indoc '("symb" "figures" "sequence" "measures")
 :icon 134
 :doc "Builds an RT starting from a <sequence> of <figures> with 
given <measures>.<sequence> is the position of figures. If a given 
position doesn't exist this will yield a rest."
   
 (let* ((mesures
         (let* ((dernieremesure (last-elem measures))
                (lgtseq (length sequence))
                (lgtbeats (apply '+ (mapcar 'car measures))))
           (if (> lgtseq lgtbeats)
             (x-append measures (repeat-n dernieremesure
                                          (ceiling (/ (- lgtseq lgtbeats) 
                                                      (car dernieremesure)))))
             measures)))
        (num (mapcar 'car mesures))
        (denom (mapcar 'cadr mesures))
        (pos (posn-match figures sequence))
        (donnes (loop for i in pos
                      collect 
                      (if (null i) -1 (list 1 i))))
        (groupment (grplst donnes num)))
     (list '? (loop
                for i in num
                for j in denom
                for a in groupment
                collect 
                (if (< (length a) i)
                  (list (list i j) (x-append  a (* -1 (- i (length a)))))
                  (list (list i j) a))))))




;;;;;;;;;;;;;;;;;;;;;;;;;MKTREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;mettre dans le meme menu MKTREE



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Utils;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;GET-SIGNATURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defmethod! get-signatures ((tree list))
   :icon 134
   :indoc '("tree")
   :initvals (list  '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))) 
   :doc "Give list of measures contained in <tree>"
   (mapcar #'first (cadr tree)))

(om::defmethod! get-signatures  ((self voice))
   :icon 134
   :indoc '("self")
   :initvals (list t)
   :doc "Give list of measures contained in <self>"
   (let* ((tree (tree self)))
     (get-signatures  tree)))

(om::defmethod! get-signatures  ((self poly))
   :icon 134
   :indoc '("self")
   :initvals (list t)
   :doc "Give list of measures contained in <self>"
   (let* ((voices (inside self)))
     (loop for i in voices
     collect (get-signatures i))))



;;;;;;;;;;;;;;;;;;;;;;;;;GET-PULSE-PLACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#|
(defun give-pulse (liste )
  (if (null liste) nil
      (dolist (n (second liste) lst)
        (cond  
         ((atom n) (push n lst))
         ((listp n) (give-pulse n ))
         (t nil)))))





(defun pulse (mesure) 
  (let ((lst '()))
  (reverse (om::flat (mapcar #'(lambda (x) 
                                  (give-pulse x))
                              mesure)))))

(defun pulses (mesures)
            "retourne le nombre de pulses (pas les pauses) d'une RTM"
  
    (om::flat (mapcar #'(lambda (x) (pulse (list x))) mesures)))

(defun om-pulses (tlist)
(mapcar #'(lambda (x)
            (pulse (list (cons '() (list x)))))
        tlist))


(defun find-po (list)
(remove '()
  (mapcar #'(lambda (x y)
           (if (floatp x) nil y ))
        list (om::arithm-ser 0 (length list) 1))))
 

(om::defmethod! group-pulses ((liste list))
  
  :initvals (list  '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("tree")
  :icon 134
  :doc "groupe les pulses"
  
  (let* ((tree 
          (second 
           (om::mat-trans 
            (om::flat-once 
             (om::mat-trans (rest (real-copy-list liste)))))))
         (the-pulses (om::flat(om-pulses tree))))
         
      
(om::group-list the-pulses
                (om::x->dx (om::remove-dup
                            (om::flat 
                             (list 0 (find-po the-pulses) (length the-pulses)))
                            'eq 1))
                'linear)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! get-pulse-places ((tree list))
  :initvals (list  '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("tree")
  :icon 134
  :doc "Returns the nth position of pulses in a tree"
  (let* (res 
         (n 0) 
         (puls (group-pulses tree))
         (places (loop 
                   for i in puls
                   do (if (plusp (car i))
                        (progn 
                          (push n res)
                          (incf n))
                        (incf n)))))
    (reverse res)))




(om::defmethod! get-pulse-places ((self voice))
  (let ((tree (tree self)))
    (get-pulse-places tree)))



;;;;;;;;;;;;;;;;;;;;;;;;;GET-REST-PLACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! get-rest-places ((tree list))
  :initvals (list  '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("tree")
  :icon 134
  :doc "Returns the nth position of rests in a tree"
  (let* (res 
         (n 0) 
         (puls (group-pulses tree))
         (places (loop 
                   for i in puls
                   do (if (minusp (car i))
                        (progn 
                          (push n res)
                          (incf n))
                        (incf n)))))
    (reverse res)))



(om::defmethod! get-rest-places ((self voice))
  (let ((tree (tree self)))
    (get-rest-places tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;GET-MEASURE-OBJ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defmethod! get-measure-obj ((self voice))
  :initvals (list  t) 
  :indoc '("voice")
  :icon 134
  :doc "Returns all measure objects"
  (inside self))








;;;;;;;;;;;;;;;;;;;;;;;;;TRUE DURATIONs;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;mettre dans le meme menu TRUE DURATIONS
;; dans le package de maquette2obj

(defvar *midifilterpackage* (omNG-protect-object (omNG-make-new-package "Filters")))
(AddPackage2Pack *midifilterpackage* *midipackage* :protect t)

(AddGenFun2Pack  '(get-midievents get-tempomap get-mf-lyrics get-midi-notes  
                   resample get-continuous-ctrl create-midiseq temporal-sort
                   gm-program gm-drumnote control-change ms-event
                   me-textinfo) *midipackage*)







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


