;;; This file contains the generators of constraint and domains for music sequences
;;; Frank D. Valencia,16/10/96
;;; This is the version 1.1b of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda

;;;;; The domains generator functions appear below.
 
(in-package :cl-user)



(defparameter *lower-voice-name* 'l)
(defparameter *upper-voice-name* 'u)
(defparameter *fixed-name* 'f)
(defvar *voice-names* (list 0 *lower-voice-name* *upper-voice-name*))
(defvar *step-name* 'step)

(defvar *distance-function*)
(setf *distance-function* #'-)
(defvar *integrate-distance-fun*)
(setf *integrate-distance-fun* #'+)
(defvar *distance-neutral-element*)
(setf *distance-neutral-element* 0)

(defun single-variable-Map (index) (list (list index 1)))

;; 
;; replaces "abs" for different distance functions
;;
(defun direction-invariance (distance-function x y)
  (funcall distance-function (max x y) (min x y)))
 
#|;; Making domains: The following functions creates domains to music 
composition. A good choise in the CSP representation is to set each chord as
a variable where its domain is a set of combination of notes. But this can be 
impractical because of the combinatorial space complexity (explicit 
domains). However, one can alleviate this by representing the i-th chord  with
two variables x2*i and x2*i+1 where x2*i represents the first note of the chord and
x2*i is a set of intervals between contiguous notes.  
|#

(defun Refine-Lalf ( LAlf )
  (mapcar (function (lambda (x) (cond ((flatp x) (list x))
                                      (t x))))  LAlf)
  )
 
;; (refine-lalf 10 '(4 2 1))
(defun Refine-Arity ( n LArity LAlf )
  (mapcar #'(lambda (e)  (let ((x (second e)) (y (first e))) 
                           (cond ((and (atom x) (numberp x))  (list (list x)))
                                 ((flatp x) (list (borrar-repetidos x)) )
                                 ((null x) (let (lp) 
                                             (dolist (alf (nth y LAlf))
                                               (push (interv-lista 1 (length alf )) lp)
                                               )
                                             (reverse lp)  ) )
                                 
                                 (t (let ((subalf  (nth y LAlf)))
                                      (mapcar #'(lambda (e)  (let ((x (second e)) (y (first e)))
                                                               (cond ((and (atom x) (numberp x)) (list x))
                                                                     ((null x) (interv-lista 1 (length (nth+ y subalf))))
                                                                     (t (borrar-repetidos x)))
                                                               )) 
                                              (reverse (om::assoc-2-lists (Interv-lista 0 (1- (length x))) x))
                                              ))
                                    )
                                 )
                           
                           )) 
          (reverse (om::assoc-2-lists (Interv-lista 0 (1- n)) Larity)))
  )

;; (Refine-Arity  5 '((3 2 1)) (refine-Lalf  '( (1 4 3) (2 3 ) (4 5) (5 6 7 8) (5 3 6))))

(defun Refine-Interval-Generation ( LAlf LArity &AUX Lout1 Lout2)
  (dolist (pair Lalf)
    (let (L) 
      (dolist (y (second pair))
        (setf L (append* L (nth y Larity) 'Union))
        )
      (push (list (first LAlf) L) Lout1)
      )
    )
  (dolist (L (reverse Lout1) (reverse Lout2))
    (push (combin-sublistas++ (car L) (second L)) Lout2)
    )
  )

(defun Asocc-AlfxArity ( LAlf LArity)
  (mapcar #'(lambda (x y) 
              (let (L (i 0) )
                (dolist (x1 x (reverse L))
                  (if (< i (length y)) (push (list x1 (nth i y)) L)
                      (push (list x1 (last+ y)) L)
                      )
                  (incf i)))
                )
               Lalf Larity))



(defun flating_1 ( L &AUX Lout (i 0))
  (dolist ( x L Lout)
    (let (Lp)
      (dolist ( y x)
        (push (cons i (cons (length (first y)) y)) Lp)
        )
      (setf Lout (append Lout (reverse Lp)))
      )
    (incf i)
    )
  )

(defmethod gg- ((self number) (y number)) (- self y))
(defmethod gg- ((self cons) (y number)) (cons (gg- (car self) y) (gg- (cdr self) y)))
(defmethod gg- ((self null) (y t)) nil)
(defmethod gg- ((self number) (y cons)) (cons (gg- self (car y)) (gg- self (cdr y))))
(defmethod gg- ((self cons) (y cons)) (cons (gg- (car self) (car y)) (gg- (cdr self) (cdr y))))
(defmethod gg- ((self t) (y null)) nil)

;; (trace Interval-Generation)
#|
(defun reduce-intervals (ints &optional Amb)
  (if amb
    (let ((maxamb (apply #'max Amb)) (minamb (apply #'min Amb)))
      (remove-if #'(lambda (x) (delayconstr::approx> (apply #'+ minamb x) maxamb)) ints))
    ints))
|#

(defun Interval-Generation ( n LAlf Larity &optional Ambitus &Aux Lout1 Lout2 Lout3)
  (let* ;; Doing Association between Alfabets and Arities.
    ((Lr (Refine-Lalf  LAlf))
     (L (Asocc-AlfxArity Lr (gg- (Refine-Arity  n  Larity Lr) 1)))
     (L1) (L2) (ambtemp ambitus) (Lalftemp  Lr)
     (D (make-array n :initial-element (make-hash-table+)))
     first-amb last-amb)
    ;;; Flatting the Association to simplify further operations
    (setf L1 (flating_1  L) )
    
    ;;; Factorizing according to  alphabet length.
    (setf L2 (factor-list L1 #'(lambda (x y) (= (second x) (second y)))))
    
    ;;; Getting all posibles arities, classified by alphabet length.
    (dolist (x L2 Lout2)
      (setf Lout1 nil)
      (dolist (y (second x))
        (setf Lout1 (union (fourth (nth y L1)) Lout1))
        )
      (push (list (second (car x)) (sort Lout1 #'<)) Lout2)
      )
    (setq Lout2 (nreverse Lout2))
    ;;;; With all possibles arities, we generate the most general combinations.
    (dolist (y Lout2)
            (setq first-amb (apply #'min (first ambtemp)) last-amb (apply #'max (pop ambtemp)))
      (push (list (car y) 
                (clas-combin-sublistas (interv-lista 1 (car y)) (second y) first-amb last-amb (first (pop lalftemp))))
            Lout3)
      )
    (setq Lout3 (nreverse Lout3))
    ;;; The most general combinations are used to generate the interval domains.
    (dolist (e (factor-list-extract L1 #'(lambda (x y) (equal (cdr x) (cdr y))) #'car ))
      (let* ((x (car e))
             (Lcombs 
               (Translate-Combin  (get-combin-pieces   (second (assoc (second x) Lout3))  
                                                             (fourth x)
                                                             )  
                                        (om::assoc-2-lists (interv-lista 1 (second x)) (third x))
                                        )
             )
             ;;(HT+ (list-to-hash-table+ Lcombs #'(lambda (x) (apply #'+ x))))
             (HT+ (list-to-hash-table+ Lcombs #'(lambda (x) (reduce *integrate-distance-fun* x))))
             )
        (dolist (i (second e))
          (setf (aref D i) (union-hash-table+ (aref D i)  HT+))
          )
        (setq ambitus (rest ambitus))
        )
      )
    D
    )
  )


(defun genere-impar+ ( HT )
  (hash-table+-tolist HT) 
  )
;;(trace genere-impar+)


;;; Creates the set of base notes for each chord.

(defun make-interv-dom ( L &AUX Lout )
  (setf L (borrar-repetidos L))
  (dolist (x (factor-list L  #'(lambda (x y) (= (apply '+ x) (apply '+ y)))))
    (let (lp)
      (dolist (y (second x))
        (push (nth y L) Lp)
        )
      (push (list (apply '+ (first x)) Lp) Lout) 
      )
    )
  Lout
  )

(defun my-string= (a b) (and (symbolp a) (symbolp b) (string= a b)))

;; When fixed intervals are specified (for instance (f (1 3 2) ....(3 4 5))

(defun Get-fixed-alf ( LAlf &AUX Lfixed (i 0) LalfOut)
  (dolist (x Lalf)
    (if (my-string= (car x) *fixed-name*)   ;;(equal (car x) 'f) 
        (progn (push (list 1) LAlfOut) (push (list i (make-interv-dom  (cdr x))) Lfixed)) 
        (push x LalfOut))
    (incf i)
    )
 (values (reverse LalfOut) Lfixed)
)

;;; When the chords are given. 
#|
(defun Get-fixed-chord ( LChord &AUX Lout Constraint-chords)
  (dolist ( ch LChord Lout)
    (dolist ( i (first ch))
      (push (fixed-chords (list i i (second ch))) Constraint-chords)
      )
    (push (list (first ch) (mapcar #'first (second ch))
                (make-interv-dom (mapcar #'x->dx (second ch))))
          Lout)
    ;(push (list (first ch) (make-interv-dom (mapcar #'x->dx ch))) Linterv) 
    )
  (values Lout (flat-once Constraint-chords))
  )
|#
(defun Get-fixed-chord ( LChord &AUX Lout Constraint-chords)
  ;(push (fixed-chords Lchord) Constraint-chords)
  (let (res indexes (form Lchord ))
    (do ((reps)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (pop form) indexes))
      (setq reps (pop form))      
      (push (list indexes (mapcar #'first reps)
                  (make-interv-dom (mapcar #'x->dx reps)))
            Lout) 
      )
    (values Lout Constraint-chords)
    )
  )
;;(get-fixed-chord '( ( ( 1 2 ) (( 63 79 85) (45  67 72))) ((0) ((3 4 5)))))
;;(get-fixed-chord '(((0 1 2 3) ((3 4 5) (4 5 6)))))
;;(assoc  2 '(((1 2) (63 45) ((27 ((22 5))) (22 ((16 6)))))) :test 'member)

              
(defun f-menor (x y) (< (first x) (first y))) 
          
;;;
;;; Returns the domains for music secuences constraints 

(defun remove-impossible-base-point (Amb maxint)
  (let ((maxamb (- (apply #'max Amb) maxint)))
    (remove-if #'(lambda (x) (delayconstr::approx> x maxamb)) Amb)))
   
(defun make-domains (n LAlf  LArity  Lambitus Fixed-chords &AUX DOM fixed-elem  LI
                        fixed-chord current-amb)
  (multiple-value-bind (L-fixed-chord chords-Constraint) (Get-fixed-chord Fixed-chords)
    (multiple-value-bind (Lalf1 Lfixed) (Get-fixed-alf Lalf)
      (setf LAlf LAlf1)
      (setf LI (Interval-generation n LAlf LArity Lambitus))
      (dotimes (i (* 2 n)  )
        (if (not (par i))
          (cond  (fixed-chord (push (third fixed-chord) DOM))
                 (fixed-elem (push (second fixed-elem) DOM))
                 (t
                  (push (genere-impar+ (aref LI (/ (1- i) 2))) DOM))
                 )
          (progn
            (setf fixed-chord nil fixed-elem nil current-amb (car Lambitus))
            (cond  ((setf fixed-chord (assoc (/ i 2) L-fixed-chord :test #'member))
                    (push (list (list 0 (permut-list (second fixed-chord)))) DOM ))                   
                   ((setf fixed-elem (assoc (/ i 2) Lfixed))
                    (push (list (list 0 (permut-list-cond 
                                         (remove-impossible-base-point current-amb
                                                                       (first (fmax-lista  (second fixed-elem) #'f-menor)) ))))
                          DOM ))
                   (t
                    (push (list (list 0 (permut-list-cond (remove-impossible-base-point 
                                                      current-amb
                                                      (fmax-lista  (hash-table+-keys (aref LI (/ i 2)) ) #'<)))))
                          DOM ))
                   )
            (setf Lambitus (cdr Lambitus))
            )
          )
        )
      )
    (values (nreverse DOM) chords-Constraint)
    )
  )

;;========================================================================
;;;So as to give users the possibility of entering  their own variable domains
;;Camilo [04/09/97]
(defun make-non-chord-domains ( n  LAlf)
  (let ((domains Lalf) (last-domain (first (last Lalf)))
        DOM)
    (dotimes (i n)
      (push (hash-table+-tolist (if domains (car domains) last-domain)) DOM)
      (when domains (pop domains)))
    (nreverse DOM) ))

(defun make-one-non-chord-domain (domain &optional fun)
  (list-to-hash-table+ (permut-list-cond (nreverse domain)) (or fun #'(lambda (x) (declare (ignore x)) 0))))

#+:OM
(om::defmethod! om::variable-domains ((domains list))
  :initvals '(((1 2) (3 4)))
  :indoc '("list of values in each variable domain") :icon 193
  :doc "creates arbitrary domains for variables from a list of list of values. If a sublist begins with a function object, it will be used as a split function for structuring the domain into a two-level tree.
In order to improve the perfomance of SITUATION, the domains of variables can be hierarchic.
For the kind of harmonic problems that SITUATION normally handles, for example, it is convenient to represent
the odd-variable domains (i.e. the interval domains) as two level lists, where the first level,
level 0, contains the addition of the intervals and the second level contains the intervals themselves. For instance,
 the intervals ((3 2) (4 1) (4 4) (5)  (5 3) (8)) are grouped as ' ( (5 ((3
2) ( 4 1) (5) ) ( 8 ( (4 4) (5 3) (8)) ) ). Constraints refering to the upper voice can then be
expressed as predicates over the first level, which improves the search a lot.
 For instance,
if domains= ((5 7 8) (#'(lambda (x) (rem x 3)) (9 2 7 6 4 12))) then the hierarchical domains will be:
 (0 (5 7 8)) for the first variale (no function given) and
 ((0 (9 6 12)) (1 (7 4)) (2 (2))) for the second variable (values grouped according to function given) "
  (build-box-variable-domains (expand-lst domains)))

#+:PW
(pw::defunp variable-domains ((domains pw::list (:value '((1 2) (3 4))))) list
"creates arbitrary domains for variables from a list of list of values.
If a sublist begins with a function object, it will be used as a split function for structuring the domain into a two-level tree.
In order to improve the perfomance of SITUATION, the domains of variables can be hierarchic.
For the kind of harmonic problems that SITUATION normally handles, for example, it is convenient to represent
the odd-variable domains (i.e. the interval domains) as two level lists, where the first level,
level 0, contains the addition of the intervals and the second level contains the intervals themselves. For instance,
 the intervals ((3 2) (4 1) (4 4) (5)  (5 3) (8)) are grouped as ' ( (5 ((3
2) ( 4 1) (5) ) ( 8 ( (4 4) (5 3) (8)) ) ). Constraints refering to the upper voice can then be
expressed as predicates over the first level, which improves the search a lot.
 For instance,
if domains= ((5 7 8) (#'(lambda (x) (rem x 3)) (9 2 7 6 4 12))) then the hierarchical domains will be:
 (0 (5 7 8)) for the first variale (no function given) and
 ((0 (9 6 12)) (1 (7 4)) (2 (2))) for the second variable (values grouped according to function given) "
  (build-box-variable-domains (pw::expand-lst domains)))

(defun build-box-variable-domains (domains)
  (let (res)
    (unless (consp (first domains)) (setf domains (list domains)))
    (dolist (dom domains (nreverse res))
      (cond 
       ((functionp (first dom)) (push (make-one-non-chord-domain (cadr dom) (first dom)) res))
       ((and (symbolp (first dom)) (fboundp (first dom)))
        (push (make-one-non-chord-domain (cadr dom) (fdefinition (first dom))) res))
       ((and (consp (first dom)) (eq (caar dom) 'function))
        (push (make-one-non-chord-domain (cadr dom) (eval (first dom))) res))
       (t (push (make-one-non-chord-domain dom) res))))))

(defun non-chord-domains? (list) (eq (type-of (first list)) 'hash-table+))

;;(om::variable-domains '((1 2) (6 7 12) (9 10)))

;;===========================================================================
;;The Constraint Generator functions appears below.
;;

(defun Var1 ( xi ) (* 2 xi))
(defun Var2 ( xi ) (1+ (* 2 xi)))


(defun dense-ambitus? (amb)
  (let ((step (delayconstr::get-current-space-step)))
    (when (= step most-positive-fixnum) (setq step 1))
   (delayconstr::approx= (1+ (/ (- (apply #'max amb) (apply #'min amb)) step))
                          (length amb))))
     
    
(defun Vert_Constraint_Ambitus_Sup ( LXS-Amb LYS-Amb  &AUX ConstGraph)
  "Defines the limit for highest notes. LXS-Amb represents the chords. 
LYS-Amb is the list of limits "
  (let (y-amb)
    (dolist (x LXS-Amb ConstGraph)
      (setq y-amb (pop LYS-Amb))
      (if (dense-ambitus? y-amb)
        (push 
         (list (eval`(function (lambda (xi xi+1) 
                                 (delayconstr::approx<= (funcall *integrate-distance-fun* xi xi+1) ',(apply #'max y-amb)) 
                                 ) ) ) 
               (list (Var1 x) 1) (list (Var2 x) 0)  )  
         ConstGraph )
      (progn
        (push 
         (list (eval`(function (lambda (xi xi+1) 
                                 (member (funcall *integrate-distance-fun*  xi xi+1) ',y-amb :test #'delayconstr::approx=) 
                                 ) ) ) 
               (list (Var1 x) 1) (list (Var2 x) 0)  )  
         ConstGraph )
        (push 
         (list (eval`(function
                      (lambda (xi xi+1)
                        (let ((accum (first xi+1)))
                          (dolist (int  (rest xi+1) t)
                            (unless (member (funcall *integrate-distance-fun* xi accum) ',y-amb :test #'delayconstr::approx=)
                              (return nil))
                            (setq accum (funcall *integrate-distance-fun* accum int))
                            ;;(incf accum int)
                            ) ) ))) 
               (list (Var1 x) 1) (list (Var2 x) 1)  )  
         ConstGraph )  ))
      )
    )
  )

(defun Vert_Constraint_Ambitus_Inf ( LXI-Amb LYI-Amb  &AUX ConstGraph)
"Defines the limit for lowest notes. LXI-Amb represents the chords. 
LYI-Amb is the list of limits "
  (dolist (x LXI-Amb ConstGraph)
    (let ((y-inf (car LYI-Amb)))
      (push  (list (eval`(function (lambda ( xi ) 
                                     (delayconstr::approx>= xi ,y-inf)
                                     ) ) ) 
                   (list (Var1 x) 0)   )
             ConstGraph )
      )
    (setf LYI-Amb (cdr LYI-Amb))
    )
  )

(defun Ambitus-Constraints (   LXS-Amb   LYS-Amb)
  (Vert_Constraint_Ambitus_Sup  LXS-Amb LYS-Amb )
  )

(defun Ambitus-Constraints* (  LXI-Amb LXS-Amb  LYI-Amb  LYS-Amb)
  (append (Vert_Constraint_Ambitus_Inf  LXI-Amb LYI-Amb )
          (Vert_Constraint_Ambitus_Sup  LXS-Amb LYS-Amb )
          )
)

;;(trace Vert_Constraint_Ambitus_Sup)

(defun greater_hor_1 ( xj xj+1 xi xi+1 )
  (delayconstr::approx>  (funcall *integrate-distance-fun* xj xj+1) (funcall *integrate-distance-fun* xi xi+1)))
(defun equal_hor_1 ( xj xj+1 xi xi+1 )
  (delayconstr::approx= (funcall *integrate-distance-fun* xj xj+1) (funcall *integrate-distance-fun* xi xi+1)))
(defun less_hor_1 ( xj xj+1 xi xi+1 )
  (delayconstr::approx< (funcall *integrate-distance-fun* xj xj+1) (funcall *integrate-distance-fun* xi xi+1)))

;;; To do revise the special case of this function
(defun build-hint-cnstr-function (i f option)
  (if option
    (eval`(function (lambda (xj xj+1 xi xi+1)
                      (let ((ch1 (dx->x xj xj+1))
                            (ch2 (dx->x xi xi+1)))
                        (if (or (>  ,i (length ch1)) (> ,i (length ch2)))
                          t
                          (funcall ,f (nth  ,i ch1)
                                   (nth  ,i ch2)
                             )) ))))
    (eval`(function (lambda (xj xj+1 xi xi+1)
                      (let ((len1 (length xj+1)) (len2 (length xi+1)))
                        (funcall ,f (nth (if (> ,i len1) len1 ,i) (dx->x xj xj+1))
                                 (nth  (if (> ,i len2) len2 ,i) (dx->x xi xi+1))
                           )))) )))
  
(defun Horizontal_Constraint_bpf (LX  LY i &OPTIONAL Option Option2 &AUX ConstGraph fun)
  "Defines the profile of the i-th voice. LX is the list of chords.
LY represents the relations between notes of the i-th voice.
For instance LX = (0 1 2 3) LY= (50 60 55 4) i = 0 means that
the higher note in chord 0 should be lower than that in chord 1, 
this in turn should be greater than that in chord 2, and so on. 
i= 0 is the higher note, i=1 the lowest, and i=k (for k>1) is the (k-1)th 
note
If Option = t then no constraint is applied when the i-th
 note does not exist
else if the i-th note is not defined the constraint is applied to the higher one.
If Option2 = t, a horizontal profile means that no constraint should be applied
in that region" 
  (let ( HLY (H1LX (copy-list LX)) H2LX x xp)
    (dolist (yj LY)
      (setf x (car H1LX))
      (let ((CLX (copy-list H2LX)))
        (dolist (yi HLY)
          (setf xp (car CLX))
          (cond ((> yj yi)
                 (cond ((and (symbolp i) (string= i *upper-voice-name*))
                        (push (list (function greater_hor_1 ) 
                                    (list (Var1 x) 0) (list (Var2 x) 0) (list (Var1 xp) 0) (list (Var2 xp) 0)) 
                              ConstGraph))
                       ((or (and (symbolp i) (string= i *lower-voice-name*)) (and (numberp i) (zerop i)))
                        (push (list (function delayconstr::approx>) (list (Var1 x) 0) (list (Var1 xp) 0) ) 
                                      ConstGraph))
                       (t (setf fun (build-hint-cnstr-function i #'delayconstr::approx> option))
                          (push (list fun (list (Var1 x) 0) (list (Var2 x) 1) (list (Var1 xp) 0) (list (Var2 xp) 1)  ) 
                                ConstGraph)
                          ) 
                       )
                 )
                ((and (= yj yi) (not Option2))
                 (cond  ((and (symbolp i) (string= i *upper-voice-name*))
                         (push (list (function equal_hor_1 ) 
                                     (list (Var1 x) 0) (list (Var2 x) 0) (list (Var1 xp) 0) (list (Var2 xp) 0)) 
                               ConstGraph))
                        ((or (and (symbolp i) (string= i *lower-voice-name*)) (and (numberp i) (zerop i)))
                         (push (list (function delayconstr::approx= ) (list (Var1 x) 0) (list (Var1 xp) 0) ) 
                               ConstGraph))
                        (t (setf fun (build-hint-cnstr-function i #'delayconstr::approx= option)) 
                           (push (list fun (list (Var1 x) 0) (list (Var2 x) 1) (list (Var1 xp) 0) (list (Var2 xp) 1)  ) 
                                 ConstGraph)
                                 )
                           )
                        )
                 ((< yj yi)
                  (cond ((and (symbolp i) (string= i *upper-voice-name*))
                         (push (list (function less_hor_1 ) 
                                     (list (Var1 x) 0)  (list (Var2 x) 0) (list (Var1 xp) 0) (list (Var2 xp) 0)) 
                               ConstGraph))
                        ((or (and (symbolp i) (string= i *lower-voice-name*)) (and (numberp i) (zerop i))) 
                         (push (list (function delayconstr::approx< ) (list (Var1 x) 0) (list (Var1 xp) 0)  ) 
                                       ConstGraph))
                        (t (setf fun (build-hint-cnstr-function i #'delayconstr::approx< option))
                           (push (list  fun
                                        (list (Var1 x) 0) (list (Var2 x) 1) (list (Var1 xp) 0) (list (Var2 xp) 1) ) 
                                 ConstGraph)
                                 )
                           )
                        )
                  )
                 (setf CLX (cdr CLX))
                 (return)
                 )
                )
          (setf H1LX (cdr H1LX))
          (push x H2LX)
          (push yj HLY)
          )
        )
      ConstGraph
      )


(defun Diferente_nota_superior ( xi xi+1 xj xj+1 )
  (/=  (+ xj xj+1) (+ xi xi+1))
)

;; This function should be revised with PatchWork devolopers: possible error found in
;; bpf functions generators
;;
(defun Refine_bpf ( LX LY &AUX LR)
  (do  ((x (car LX) (car LX)) (y (car LY) (car LY))) ((null LX))
    (setf LX (cdr LX))
    (setf LY (cdr LY))
    (unless (member x LX :test '=) (push (list x y) LR)) 
    )
  (setf LR (sort LR (function (lambda (x y) (< (first x) (first y))))))
  (values (mapcar (function first) LR) (mapcar (function second) LR))
  )


;;(refine_bpf '(0 2 3 4 5 6 7 7 8 9) '(80 84 92 103 114 103 91 91 97 83))

(defun Make_Constraint_bpf ( LxsIn LysIn &AUX ConstGraph Lxs Lys Lxys)
  " The first sublist in LysIn defines the curve for the upper limit of the ambitus,
  the second sublist defines the loweat one, the third defines the profile for the
  upper voice and the fourth sublist defines the profile for the lowest voice.
the nth list in LxsIn represents those chords associated with the nth list in LysIn.  
LxsIn LysIn can be partially defined. Lists in LxsIn should be in ascending
 order.
"
  (setf Lxys (mapcar #'(lambda (x y)  (multiple-value-bind (xr yr) (refine_bpf x y)  (list xr yr))) LxsIn LysIn) )
  (setf Lxs  (mapcar #'first Lxys) Lys (mapcar #'second Lxys))
  (dotimes (i (length Lxs))
    (setf ConstGraph (append ConstGraph (horizontal_constraint_bpf  (nth i Lxs) (nth i Lys) i t)))
    )
  (eval `(defun ,(gensym "Multi-voice-form-constraint") () ',ConstGraph))
  )


(defun Make_Constraint_ambitus* ( LxsIn LysIn &AUX ConstGraph Lxs Lys Lxys)
  " The first list in LysIn defines the curve for the upper limit of the ambitus,
the second sublist defines the loweat one. The nth list in LxsIn represents the chords associated 
to the nth list in LysIn. LxsIn LysIn can be partially defined.  Lists in LxsIn should be in ascending
 order.
"
  (setf Lxys (mapcar #'(lambda (x y)  (multiple-value-bind (xr yr) (refine_bpf x y)  (list xr yr))) LxsIn LysIn) )
  (setf Lxs  (mapcar #'first Lxys) Lys (mapcar #'second Lxys))
  (let ((Lxs-amb (first Lxs)) (Lxi-amb (second Lxs))
        (Lys-amb (first Lys)) (Lyi-amb (second Lys))
        )
    (setf ConstGraph (append ConstGraph (Ambitus-Constraints* Lxi-amb Lxs-amb Lyi-amb Lys-amb  )))
    )
  (eval `(defun ,(gensym "bpf-ambitus-constraint") () ',ConstGraph))
  )

(defun Make_Ambitus ( LAmb)
    (Ambitus-Constraints (interv-lista 0 (1- (length Lamb)))  Lamb)
    )

(defun Make_bpf_voice ( LxIn LyIn Voice Option1 Option2 &AUX ConstGraph)
  "  The first sublist in LysIn defines the curve for the upper limit of the ambitus,
  the second sublist defines the lowest one, the third defines the profile for the
  upper voice and the fourth sublist defines the profile for the lowest voice.
the nth list in LxsIn represents those chords associated with the nth list in LysIn.  
LxsIn LysIn can be partially defined. Lists in LxsIn should be in ascending
 order.
"
  (multiple-value-bind (xr yr) (refine_bpf  LxIn LyIn)
    (setf LxIn xr LyIn yr)
    (setf ConstGraph (horizontal_constraint_bpf xr yr voice Option1 Option2))
    )
  (eval `(defun ,(gensym "x-bpf_prof-constraint") () ',ConstGraph))
  )
  

;; (trace horizontal_constraint_bpf)


;; Camilo's constraint


(defun x->dx (xs)
  "Returns the list of the intervals between the contiguous values of a list <xs>. 
For example
(x->dx '(0 4 5 9 6 2 3 3))
 will return  (4 1 4 -3 -4 1 0)  "
  (mapcar *distance-function* (cdr xs) xs))

(defun rest-abs (x y dist-fun) (abs (funcall dist-fun x y)))

(defun x->dx-abs (xs)
  "Returns the list of the intervals (absolutes) between the contiguous values of a list <xs>. 
For example
(x->dx '(0 4 5 9 6 2 3 3))
 will return  (4 1 4 -3 -4 1 0)  "
  (mapcar #'(lambda (x y) (rest-abs x y *distance-function*)) (cdr xs) xs))

(defun dx->x (start dxs)
 (let ((x start))
    (cons x (mapcar #'(lambda (dx) (setf x (funcall *integrate-distance-fun*  x dx)) x) dxs))))

(defun all-lists (i j  &optional (step 1) (level 0) (alt 0)  &key (test-end #'>=))
  (let (res)
    (do ((x 0 (+ step x))) ((funcall test-end x (- j i)) (nreverse res))
      (push (list (+ x i) (if (zerop (rem x 2)) level (+ level alt))) res ))))

(defun all-lists-2 (i j  &optional (step 1) (level 0) (alt 0)  )
  (let (res)
    (do ((x 0 (+ step x))) ((> x (- j i)) (nreverse res))
      (push (list (+ x i) (if (zerop (rem x 2)) level (+ level alt))) res ))))
;;(all-lists-2   2 8)
 
;;returns a complete expanded form of a constraint expression
(defun fill-expression (exp)
  (when exp
    #+:OM (setf exp (expand-lst (if (listp exp) exp (list exp))))
    #+:PW (setf exp (pw::expand-lst (if (listp exp) exp (list exp))))
    ;;(if (and (not (flatp exp)) (numberp (first exp)))
    ;;  exp
    ;;  #+:OM (append (om::arithm-ser 0 fvtempos::MaxChords 1) (list exp))
   ;;   #+:PW (append (pw::arithm-ser 0  1 fvtempos::MaxChords) (list exp))
     ;; )
    (cond ((and (not (flatp exp)) (numberp (first exp))) exp)
          ((and (consp (first exp)) (not (flatp (first exp))))
           #+:OM (append (om::arithm-ser 0 fvtempos::MaxChords 1) exp)
           #+:PW (append (pw::arithm-ser 0  1 fvtempos::MaxChords) exp)
           )
          (t
           #+:OM (append (om::arithm-ser 0 fvtempos::MaxChords 1) (list exp))
           #+:PW (append (pw::arithm-ser 0  1 fvtempos::MaxChords) (list exp))
           )
      )) )

(defun vert-logi-filter (expression)
  "exp ::= ( Ci1 Cj1 <Logical Expression>1 ... Cik Cjk <Logical Expression>k ). 
Constraints every chord between Ciu and Cju (for all u;1<=u<=k) according to 
<Logical Expression>u. <Logical Expression>u is a vertical constraint over the 
intervals in a chord. 
For instance with  exp = (0 5 (not (or (* 2 2 *) (* 12 12 *))) 6 10 (and (* 7 *) (? 6 *)))
chords between chord 0 and chord 5 should not have two contiguous major seconds or octaves. 
Also chords between chord 6 and chord 10 should have at least one fifth and a augmented fourth
in the second interval. 
Entering only <Logical Expression> is trated as a shorthand for
(0 MaxChords <Logical Expression>)
" 
  (let (res (form (nreverse expression))  indexes)
    (do ((reps) (arity) (fun)) ((null form) res)
      (setq reps (pop form) arity 1 )
      (setq fun (DELAYCONSTR::set-domain-constraint-fun reps))
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))) (setf indexes (nreverse indexes)))
        (push (pop form) indexes))
      (setf res (append (build-sequence-constraint fun indexes 1 arity #'var2) res))
      )
    (eval `(defun ,(gensym "i-dst_filt-constraint") () ',res))
    )
  )


#+:OM
(om::defmethod! om::vertical-filter  ((exp  list))
  (vert-logi-filter (fill-expression exp)) )

#+:OM
(om::defmethod! om::i-dst_filt  ((exp  list) &optional c-imp varMap choiceMap)
  :initvals '((0_5 (not (or (* 4 7 *) (? ? 2 *))) 6_8 (and (* 7 *))) 1 nil nil)
  :indoc '("logical expression" "constraint importance" "variable Map" "choice Map")
  :icon 520 
  :doc "Internal distance filtering. Constraints the distances in each of a given set of objects to follow a supplied pattern
For example, interpreting objects as chords, the expression (0_5 (not (or (* 4 7 *) (? ? 2 *))) 6_8 (and (* 7 *))) constraints objects number
0,1,2,3,4 and 5 each to have no third followed by a fifth and no major second in the third position .
It also constraints chords 6,7, and 8 each to contain a fifth"
  (if (or varMap choiceMap)
     (includeConstraint-Importance
      (set-pitch-logical-filter (fill-expression exp) (or varMap #'single-variable-map)
                                (if choiceMap (eval `(function (lambda (x) (x->dx (funcall ,choiceMap x))))) #'x->dx))
      (or c-imp 1))
     (set-importance-in-namedCnstr "i-dst_filt" (fill-expression exp) #'vert-logi-filter c-imp)
     ))

#+:OM
(om::defmethod! om::vint-filt  ((exp  list) &optional c-imp varMap choiceMap)
  :initvals '((0_5 (not (or (* 4 7 *) (? ? 2 *))) 6_8 (and (* 7 *))) 1 nil nil)
  :indoc '("logical expression" "constraint importance" "variable Map" "choice Map")
  :icon 520 
  :doc "kept for compatibility. See i-dst_filt."
  (om::i-dst_filt exp c-imp varMap choiceMap))

#+:PW
(pw::defunp vint-filt  ((exp  pw::list (:value '(0_5 (not (or (* 4 7 *) (? ? 2 *))) 6_8 (and (* 7 *))))))
  list
"Vertical intervals filtering. Constraints the intervals in each of a given set of chords to follow a supplied pattern
For example, the expression (0_5 (not (or (* 4 7 *) (? ? 2 *))) 6_8 (and (* 7 *))) constraints chords number
0,1,2,3,4 and 5 each to have no third followed by a fifth and no major second in the third position. It also constraints
chords 6,7, and 8 each to contain a fifth"
(vert-logi-filter (fill-expression exp))
)

;;; (setf f (vert-logi-filter  '(0 5 (not (or (* 2 2 *) (* 12 12 *))) 6 10 (and (* 7 *) (? 6 *)))))
;;; (funcall (caar (funcall (vert-logi-filter  '(0 5 (not (or (* 2 2 *) (* 12 12 *))) 6 10 (and (* 7 *) (? 6 *)))))) '(4 3 2))


(defvar *min-max-spec* '(min max))

(defun build-direction-predicates (expr f+ f- f< &optional voice test)
  (let (res)
    (if (and (symbolp (first expr)) (member (first expr) *min-max-spec* :test #'string=))
      (push (list (first expr) (second expr)
                  (if voice
                    (eval `(function (lambda (&rest chords) (apply ,f< ,voice ,test chords))))
                    (eval `(function (lambda (&rest chords) (apply ,f< ,test chords))))))
            res)
      (dolist (item expr (setf res (list (cons '= res))))
        (if (numberp item)
          (push 
           (list (abs item)
                 (if voice
                   (eval `(function (lambda (&rest chords) (apply ,(if (minusp item) f- f+) ,voice ,test chords))))
                   (eval `(function (lambda (&rest chords) (apply ,(if (minusp item) f- f+) ,test chords))))))
           res)
          (error "wrong voice movement specification in constraint" expr)))
      )
    res) )

;;( build-direction-predicates '(2 -3) ''foo ''fi ''fu 4 )
#|
(set-direction-constr '(0 1 2 3 4 5 6 7 (2 -3)) #'upper-nb-dir=Ok #'upper-nb-dir=Ok #'upper-nb-dir=Ok
                        #'lower-nb-dir=Ok #'lower-nb-dir=Ok  #'lower-nb-dir=Ok
                        #'voice-nb-dir=Ok #'voice-nb-dir=Ok  #'voice-nb-dir=Ok 2 "nb-dirOK" nil)
(set-direction-constr '(0 1 2 3 4 5 6 7 (max 2 t)) #'upper-nb-dir=Ok #'upper-nb-dir=Ok #'upper-nb-dir=Ok
                        #'lower-nb-dir=Ok #'lower-nb-dir=Ok  #'lower-nb-dir=Ok
                        #'voice-nb-dir=Ok #'voice-nb-dir=Ok  #'voice-nb-dir=Ok 2 "nb-dirOK" nil)
|#

(defun set-direction-constr (expression upper+ upper- upper< lower+ lower- lower< fvoice+ fvoice- fvoice< add name
                                        &optional test vmapUpper vmapLower vmapInternal cmapUpper
                                        cmapLower cmapInternal)
  (let (res  all-indexes indexes (form expression ) last modulo?)
    (when (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
      (setq vmapUpper (or vmapUpper #'single-variable-Map)
            vmapLower (or vmapLower #'single-variable-Map)
            vmapInternal (or vmapInternal #'single-variable-Map)))
    (do ((reps) (alf) (fun1) (fun2) (fun3) ) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq fun1 NIL fun2 NIL fun3 NIL)
      (if  (flatp reps)
        (setq last (first (last reps)) modulo? (and (symbolp last) (string= last 't))
              fun1 (build-direction-predicates (if modulo? (butlast reps) reps)
                                                   upper+ upper- upper< nil
                                                   (if modulo? test nil)))
        (do ((part reps (cddr part)) (voice) ) ((null part) res)
          (setf alf (second part) voice (first part))
          (setq last (first (last alf)) modulo? (and (symbolp last) (string= last 't)))
          (cond ((and (numberp voice) (> voice 0))
                 (setq fun3 (append (build-direction-predicates
                                     (if modulo? (butlast alf) alf)
                                     fvoice+ fvoice- fvoice< voice
                                     (if modulo? test nil))
                                    fun3)))
                ((and (symbolp voice) (string= voice *upper-voice-name*))    ;(equal voice 'u)
                 (setq fun1  (append (build-direction-predicates 
                                      (if modulo? (butlast alf) alf)
                                      upper+ upper- upper< nil
                                      (if modulo? test nil))
                                     fun1)))
                ((or (and (symbolp voice) (string= voice *lower-voice-name*))   ;(equal voice 'l)
                     (equal voice 0))
                 (setq fun2 (append (build-direction-predicates
                                     (if modulo? (butlast alf) alf)
                                     lower+ lower- lower< nil
                                     (if modulo? test nil))
                                     fun2)))
                )
          )     
        )
      (dolist (triple fun1)
        (setf res 
              (append (build-piecewise-constraint all-indexes indexes triple add #'var1 #'var2 nil vmapUpper)
                      res)))
      (dolist (triple fun2)
        (setf res 
              (append (build-piecewise-constraint all-indexes indexes triple add #'var1 nil nil vmapLower)
                      res)))
      (dolist (triple fun3)
        (setf res 
              (append (build-piecewise-constraint all-indexes indexes triple add #'var1 #'var2 1 vmapInternal)
                      res)))
      )
    (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
      res
      (eval `(defun ,(gensym name) () ',res)))
    )
  )

(defun make-all-subindxs (inds) (maplist #'identity inds))

(defun build-piecewise-constraint (all-indexes indexes form add map1 &optional map2 level varMap)
  (let (size res f)
    (if (member (first form) *min-max-spec* :test #'string=) 
      (when (>= (length all-indexes) (setq size (+ (second form) add)))
        (setq f (third form))
        (do  ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)))
             ((or (< (length all-ind) size) (null ind)) res)
          (setf res (append (build-sequence-constraint f (subseq all-ind 0 size) 0 size map1 map2 level varMap) res)) ))
      (let ((local-ind (reverse indexes)) (rform (nreverse (rest form))))
        (do () ((null local-ind) res)
          (dolist (item rform res)
            (setf size (+ (first item) (1- add)) f (second item))
            (dotimes (k (1- size))
              (unless (cdr local-ind) (progn (setf local-ind nil) (return res)))
              (setf res (append (build-sequence-constraint f (list (second local-ind) (pop local-ind))
                                                           0 2 map1 map2 level varMap) res)) )
            ))))))

;;;; Repetitions constraints

(defun set-up-rep-predicate (expr f1 f2 f3 test &optional voice)
  (let* ((last (first (last expr)))
         (modulo? (and (symbolp last) (string= last 't))))
    (if modulo? (setf expr (butlast expr)))
    (if (and (symbolp (second expr)) (member (second expr) *min-max-spec* :test #'string=))
      (cons (eval
             `(function (lambda (&rest chords)
                         (apply ,f2 ,(if modulo? test #'delayconstr::approx=) ,(+ (third expr) 1) 
                                ,@ (if voice `(,voice chords) `(chords)))))    )
            expr)
      (list*
       (and f3 (eval `(function (lambda (&rest chords)
                                  (apply ,f3 ,(if modulo? test #'delayconstr::approx=) ,(- (first expr) (second expr))
                                         ,@ (if voice `(,voice chords) `(chords)))))))
       (first expr)
       (eval
             `(function (lambda (&rest chords)
                         (apply ,f2 ,(if modulo? test #'delayconstr::approx=) ,(+ (second expr) 1)
                                ,@ (if voice `(,voice chords) `(chords)))))    )
       (eval `(function (lambda (&rest chords)
                         (apply ,f1 ,(if modulo? test #'delayconstr::approx=)
                                ,(second expr) ,@ (if voice `(,voice chords) `(chords))))))
            (rest expr)))))

;;(set-up-rep-predicate '(5  3 t) 'fr1 'fr2 'fr3 'test 4)
(defun set-pitch-repetitions-constr (expression upper1 upper2 upperOPT lower1 lower2 lowerOPT
                                                fvoice1 fvoice2 fvoiceOPT add name
                                                &optional test vmapUpper vmapLower vmapInternal)
  (let (res  all-indexes indexes (form expression))
    (do ((reps) (alf) (fun1) (fun2) (fun3) ) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq fun1 NIL fun2 NIL fun3 NIL)
      (if  (flatp reps)
        (setq fun1 (set-up-rep-predicate reps upper1 upper2 upperOPT test))
        (do ((part reps (cddr part)) (voice) ) ((null part) res)
          (setf alf (second part) voice (first part))
          (cond ((and (numberp voice) (> voice 0))
                 (push (set-up-rep-predicate alf fvoice1 fvoice2 fvoiceOPT test voice) fun3 ))
                ((and (symbolp voice) (string= voice *upper-voice-name*))    ;(equal voice 'u)
                 (setq fun1 (set-up-rep-predicate alf upper1 upper2 upperOPT test)) )
                ((or (and (symbolp voice) (string= voice *lower-voice-name*))   ;(equal voice 'l)
                     (equal voice 0))
                 (setq fun2 (set-up-rep-predicate alf lower1 lower2 lowerOPT test))))
          )
        )
       (setf res (append (optimize-vrep-constraints all-indexes indexes fun1 add #'var1 #'var2 nil vmapUpper) res))
      (setf res (append (optimize-vrep-constraints all-indexes indexes fun2 add #'var1 nil nil vmapLower) res))     
      (dolist (pair fun3)
        (setf res (append (optimize-vrep-constraints all-indexes indexes pair add #'var1 #'var2 1 vmapInternal) res)))
      )
    (if (or vmapUpper vmapLower vmapInternal) res
        (eval `(defun ,(gensym name) () ', res)) )
    )
  )

#|
(defun optimize-vrep-constraints (all-indexes indexes form add map1 &optional map2 level variableMap)
  (let (res size)
     (when (and form (>= (length all-indexes) (setq size (+ (second form) add))))
       (if (functionp (third form))
         (let (f1 f2 f3)
           (setq f1 (first form) f2 (third form) f3 (fourth form))
           (do  ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)))
                ((or (< (length all-ind) size) (null ind)))
             (dolist (subindxs (butlast (make-all-subindxs (subseq all-ind 0 size))))
               (and f1 (setf res (append (build-sequence-constraint f1 subindxs 0 size map1 map2 level variableMap) res)))
               (setf res (append (build-sequence-constraint f2 subindxs 0 size map1 map2 level variableMap) res))))
           (setf indexes (reverse indexes))
           (do ((inds indexes (nthcdr (1- size) inds))) ((< (length inds) size))
             (setf res (append (build-sequence-constraint f3 (nreverse (subseq inds 0 size)) 0 size map1 map2 level variableMap)
                               res))))         
       (let (f)
         (setq f (first form))
          (do  ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)))
               ((or (< (length all-ind) size) (null ind)))
            (dolist (subindxs (butlast (make-all-subindxs (subseq all-ind 0 size))))
              (setf res (append (build-sequence-constraint f subindxs 0 size map1 map2 level variableMap) res)))))))
     res))

|#

;(make-all-subindxs '(6 5 4 3 2))
(defun optimize-vrep-constraints (all-indexes indexes form add map1 &optional map2 level variableMap)
  (let (res size)
     (when (and form (>= (length all-indexes) (setq size (+ (second form) add))))
       (if (functionp (third form))
         (let (f1 f2 f3)
           (setq f1 (first form) f2 (third form) f3 (fourth form))
           (do  ((ind indexes (cdr ind)))
                ((or (null ind) (< (length ind) size)))
             (dolist (subindxs (butlast (make-all-subindxs (subseq ind 0 size))))
               (and f1 (setf res (append (build-sequence-constraint f1 subindxs 0 size map1 map2 level variableMap) res)))
               (setf res (append (build-sequence-constraint f2 subindxs 0 size map1 map2 level variableMap) res))))
           (setf indexes (reverse indexes))
           (do ((inds indexes (nthcdr (1- size) inds))) ((< (length inds) size))
             (setf res (append (build-sequence-constraint f3 (reverse (subseq inds 0 size)) 0 size map1 map2 level variableMap)
                               res))))         
       (let (f)
         (setq f (first form))
          (do  ((ind indexes (cdr ind)))
               ((or (null ind) (< (length ind) size)))
            (dolist (subindxs (butlast (make-all-subindxs (subseq ind 0 size))))
              (setf res (append (build-sequence-constraint f subindxs 0 size map1 map2 level variableMap) res)))))))
     res))


(defun seuil-voice-exact (test num voice &rest chords)
  (let (notes note1 l1 len (reps 0))
    (do ((ch chords (cddr ch))) ((null ch) (= reps num))
      (setq l1 (second ch) len (length l1))
      (if (> voice len)
        (when (member (setq note1 (nth len (dx->x (first ch) l1)))
                      notes :test test)
          (incf reps))
        (when (member (setq note1 (nth voice (dx->x (first ch) l1)))
                      notes :test test)
          (incf reps)))
      (when (> reps num) (return nil))
      (push note1 notes) 
      ) ))

(defun construct-general-seuil-voice-exact (ChoiceMap)
  (eval `(function 
          (lambda (test num voice &rest chords)
            (let (notes note1 chvalue len (reps 0))
              (dolist (ch chords (= reps num))
                (setq chvalue (funcall ,choiceMap ch) len (length chvalue))
                (if (> voice len)
                  (when (member (setq note1 (nth len chvalue)) notes :test test)
                    (incf reps))
                  (when (member (setq note1 (nth voice chvalue)) notes :test test)
                    (incf reps)))
                (when (> reps num) (return nil))
                (push note1 notes) 
                ) )))))

(defun seuil-voice-max (test reps voice &rest chords)
  (let (notes note1 l1 len (repets 0))
    (do ((ch chords (cddr ch))) ((null ch) t)
      (setq l1 (second ch) len (length l1))
      (if (> voice len)
        (when (member (setq note1 (nth len (dx->x (first ch) l1)))
                      notes :test test)
          (incf repets))
        (when (member (setq note1 (nth voice (dx->x (first ch) l1)))
                      notes :test test)
          (incf repets)))
      (when (>= repets reps) (return nil))
      (push note1 notes) 
      ) ))

(defun construct-general-seuil-voice-max (choiceMap)
   (eval `(function 
           (lambda (test reps voice &rest chords)
             (let (notes note1 chvalue len (repets 0))
               (dolist (ch chords t)
                 (setq chvalue (funcall ,choiceMap ch) len (length chvalue))
                 (if (> voice len)
                   (when (member (setq note1 (nth len chvalue)) notes :test test)
                     (incf repets))
                   (when (member (setq note1 (nth voice chvalue)) notes :test test)
                     (incf repets)))
                 (when (>= repets reps) (return nil))
                 (push note1 notes) 
                 ) )))))

(defun seuil-voice-OPT (test num voice &rest chords)
  (let (notes note1 l1 len (repets 0) (difs (- (/ (length chords) 2) num)))
    (if (not (plusp difs)) t
        (do ((ch chords (cddr ch))) ((null ch) nil)
          (setq l1 (second ch) len (length l1))
          (if (> voice len)
            (when (member (setq note1 (nth len (dx->x (first ch) l1)))
                          notes :test test)
              (incf repets))
            (when (member (setq note1 (nth voice (dx->x (first ch) l1)))
                          notes :test test)
              (incf repets)))
          (when (= repets difs) (return t))
          (push note1 notes) 
          ) )))

(defun construct-general-seuil-voice-OPT (choiceMap)
  (eval `(function 
           (lambda (test num voice &rest chords)
             (let (notes note1 chvalue len (repets 0) (difs (- (length chords) num)))
               (if (not (plusp difs)) t
                   (dolist (ch chords nil)
                     (setq chvalue (funcall ,choiceMap ch) len (length chvalue))
                     (if (> voice len)
                       (when (member (setq note1 chvalue) notes :test test)
                         (incf repets))
                       (when (member (setq note1 chvalue) notes :test test)
                         (incf repets)))
                     (when (= repets difs) (return t))
                     (push note1 notes) 
                     ) ))))))

(defun upper-check-exact (test num &rest chords)
  (let (notes (reps 0))
    (do ((ch chords (cddr ch))) ((null ch) (= reps num))
      (when (member (+ (first ch) (second ch))
                    notes :test test) (incf reps))
      (when (> reps num) (return nil))
      (push (+ (first ch) (second ch)) notes))))

(defun construct-general-check-exact (choiceMap)
  (eval `(function 
          (lambda (test num &rest chords)
            (let (notes (reps 0))
              (dolist (ch chords (= reps num))
                (when (member (funcall ,choiceMap ch) notes :test test) (incf reps))
                (when (> reps num) (return nil))
                (push (funcall ,choiceMap ch) notes)))))))

(defun upper-check-max (test reps &rest chords)
  (let (notes (repets 0))
    (do ((ch chords (cddr ch))) ((null ch) t)
      (when (member (+ (first ch) (second ch))
                    notes :test test) (incf repets))
      (when (>= repets reps) (return nil))
      (push (+ (first ch) (second ch)) notes))))

(defun construct-general-check-max (choiceMap)
  (eval `(function 
          (lambda (test reps &rest chords)
            (let (notes (repets 0))
              (dolist (ch chords t)
                (when (member (funcall ,ChoiceMap ch) notes :test test) (incf repets))
                (when (>= repets reps) (return nil))
                (push (funcall ,ChoiceMap ch) notes)))))))

(defun upper-check-OPT (test num &rest chords)
  (let (notes (repets 0) (difs (- (/ (length chords) 2) num)) )
    (if (not (plusp difs)) t
        (do ((ch chords (cddr ch))) ((null ch) nil)
          (when (member (+ (first ch) (second ch))
                        notes :test test) (incf repets))
          (when (= repets difs) (return t))
          (push (+ (first ch) (second ch)) notes)))))

(defun construct-general-check-OPT (choiceMap)
  (eval `(function 
          (lambda (test num &rest chords)
            (let (notes (repets 0) (difs (- (length chords) num)) )
              (if (not (plusp difs)) t
                  (dolist (ch chords nil)
                    (when (member (funcall ,ChoiceMap ch) notes :test test) (incf repets))
                    (when (= repets difs) (return t))
                    (push (funcall ,ChoiceMap ch) notes))))))))

(defun lower-check-exact (test num &rest chords)
  (let (notes (reps 0))
    (dolist (ch chords (= reps num))
      (when (member  ch notes :test test) (incf reps))
      (when (> reps num) (return nil))
      (push ch notes)
      )))

(defun lower-check-max (test reps &rest chords)
  (let (notes (repets 0))
    (dolist (ch chords t)
      (when (member ch notes :test test) (incf repets))
      (when (>= repets reps) (return nil))
      (push ch notes)
      )))

(defun lower-check-OPT (test num &rest chords)
  (let (notes (repets 0) (difs (- (length chords) num)))
    (if (not (plusp difs)) t
        (dolist (ch chords nil)
          (when (member  ch notes :test test) (incf repets))
          (when (= repets difs) (return t))
          (push ch notes)
          ))))

(defun threshold-repetition (expression &optional vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (set-pitch-repetitions-constr expression 
                                  (construct-general-check-exact cmapUpper)
                                  (construct-general-check-max cmapUpper)
                                  (construct-general-check-OPT cmapUpper) 
                                  (construct-general-check-exact cmapLower)
                                  (construct-general-check-max cmapLower)
                                  (construct-general-check-OPT cmapLower)
                                  (construct-general-seuil-voice-exact cmapInternal)
                                  (construct-general-seuil-voice-max cmapInternal)
                                  (construct-general-seuil-voice-OPT cmapInternal)
                                  0  "x-pts_rnw-constraint" #'(lambda (x y) (delayconstr::approx= (rem x 12) (rem y 12)))
                                  vmapUpper vmapLower vmapInternal)
    (set-pitch-repetitions-constr expression #'upper-check-exact #'upper-check-max #'upper-check-OPT 
                                  #'lower-check-exact #'lower-check-max #'lower-check-OPT
                                  #'seuil-voice-exact #'seuil-voice-max #'seuil-voice-OPT
                                  0  "x-pts_rnw-constraint" #'(lambda (x y) (delayconstr::approx= (rem x 12) (rem y 12)))))
  )

#+:OM
(om::defmethod! om::seuil-repet ((exp  list)) (threshold-repetition (fill-expression exp)))
#+:OM
(om::defmethod! om::x-pts_rnw ((exp  list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
   :initvals '((0_5 (0 (4 max 2)) 6_12 (5 3)) 1 nil nil nil nil nil nil) 
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
  :doc "points renewal in sequences of selected points in objects. Controls repetitions of
 points in subsequences of points in contiguous objects.
For example, interpreting objects as chords, the expression (0_5 (0 (4 max 2)) 6_12 (5 3)) constraints the first six chords so
that each consecutive subsequence of 4 chords contains at most 2 equal notes in the lower voice,
and the next seven chords so that each subsequence of 5 consecutive chords contains exactly 3
equal notes in the upper voice (the default)."
  (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (includeConstraint-Importance
     (threshold-repetition (fill-expression exp)
                           (or vmapUpper #'single-variable-map) (or vmapLower #'single-variable-map)
                           (or vmapInternal #'single-variable-map) (or cmapUpper #'identity)
                           (or cmapLower #'identity) (or cmapInternal #'identity))
     (or c-imp 1))
    (set-importance-in-namedCnstr "x-pts_rnw" (fill-expression exp) #'threshold-repetition c-imp)
    ;;(threshold-repetition (fill-expression exp)) )
    )
  )

#+:OM
(om::defmethod! om::pitch/v-rnw ((exp  list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
   :initvals '((0_5 (0 (4 max 2)) 6_12 (5 3)) 1 nil nil nil nil nil nil) 
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
  :doc "kept for compatibility. See x-pts_rnw."
  (om::x-pts_rnw exp c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  )

#+:PW
(pw::defunp pitch/v-rnw ((exp  pw::list (:value '(0_5 (0 (4 max 2)) 6_12 (5 3))))) list
"Pitch renewal in voices. Controls repetitions of pitches in subsequences of a given voice.
For example, the expression (0_5 (0 (4 max 2)) 6_12 (5 3)) constraints the first six chords so
that each consecutive subsequence of 4 chords contains at most 2 equal notes in the lower voice,
and the next seven chords so that each subsequence of 5 consecutive chords contains exactly 3
equal notes in the upper voice (the default)."
 (threshold-repetition (fill-expression exp))
  )

;;;; (funcall (caar (funcall (seuil-repet '(0 5 (2) 6 12 (3))))) 1 2 1 3)
;;; (funcall (seuil-repet '(4 5 (4))))

;;; Ojo: Revisar documentacion.

(defun fix-notes (exp)
  "exp ::= ( n11 n12 ... <ListChords>1 ...  nk1 nk2 ... <ListChordsk>).
Chords in the list <ListChords>u (for all u; 1<=u<=k) should have the 
notes nu1,nu2 and so on.
n is a shorthand for ( n ( 0 1 ... MaxChords ) )
and (n1 n12 ....n1j ) for ( n1 n12 ... n1j (0 1 .... MaxChords ))
See MaxChords box in the menu UserLib/Music Engine.
 Example:
   exp = (60 (1 5 8) 70 (2 3 7))
" 
  (cond  ((numberp exp) (setf exp (list exp (Interv-lista 0 fvtempos::MaxChords))))
         ((flatp exp) (setf exp (list exp (Interv-lista 0 fvtempos::MaxChords)))))
  
  (let (res notes)
    (do ((list exp (cdr list))) ((null list) res)
      (if (consp (car list))
        (let ((fun (eval
                    `(function (lambda (base ints)
                                 (let ((chord (dx->x base ints)))
                                   (= (length (intersection chord ',notes))
                                      (min (length chord) (length ',notes)))))))) )
          (dolist (v (car list))
            (push (list fun (list (var1 v) 0) (list (var2 v) 1)) res))
          (setf notes nil))
        (push (car list) notes)))
    
    (eval `(defun ,(gensym "fixed-notes-constraint") () ',res))))

(defmethod fixed-notes ((exp  list))
"exp ::= ( n11 n12 ... <ListChords>1 ...  nk1 nk2 ... <ListChordsk>).
Chords in the list <ListChords>u (for all u; 1<=u<=k) should have the 
notes nu1,nu2 and so on.
n is a shorthand for ( n ( 0 1 ... MaxChords ) )
and (n1 n12 ....n1j ) for ( n1 n12 ... n1j (0 1 .... MaxChords ))
See MaxChords box in the menu UserLib/Music Engine.
 Example:
   exp = (60 (1 5 8) 70 (2 3 7))
"
(fix-notes exp)
)

;;; (funcall (caar (funcall (fixed-notes '(60 (1 5 8) 70 2 (2 3 7))))) 2 '( 68 5 6))


;;Ojo si dos acordes tienen todas las notas diferentes satisfacen tener at least ru notas diferentes

(defun change-pitches (expression f-exact f-max &optional varMap choiceMap)
  "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= ( mu ru symbolu ) (1<=u<=k)
if symbolu = t  then any chords secuence of length mu between Ciu-mu and Cju 
(for all u;1<=u<=k) should have at least ru different notes module 12.
if symbolu = NIL or is not specified any chords secuence of length mu between 
Ciu and Cju should have at least ru different notes.
( m r symbol ) is a shorthand for ( 0 MaxChords (m r symbol))
See MaxChords box in the menu UserLib/Music Engine.
 Example:
   exp = (0 5 (2 1) 6 10 (3 2))
" 
  (let (res  all-indexes indexes f (form expression))
    (do ((reps)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq f (set-up-rep-predicate reps f-exact f-max nil 
                                    #'(lambda (x y) (delayconstr::approx= (rem x 12) (rem y 12)))))
      (setf res (append (optimize-vrep-constraints all-indexes indexes f 0 #'var1 #'var2 1 varMap) res))
      )
    (if (or varMap choiceMap) res
        (eval `(defun  ,(gensym "pts_rnw-constraint") () ', res)) ))
  )

(defun ch-repets-max (test num &rest chords)
  (let* ((uniques-notes (dx->x (first chords) (second chords)))
         (count (length uniques-notes)))
    (do ((l (cddr chords) (cddr l))) 
        ((null l)  (< (- count (length uniques-notes)) num))
      (incf count (1+ (length (second l))))
      (setq uniques-notes
            (union  uniques-notes
                    (dx->x (first l) (second l))
                    :test test))
      )))

(defun construct-repets-max-exact (choiceMap max-or-exact)
  (eval `(function (lambda (test num &rest chords)
                     (let* ((uniques-notes (funcall ,choiceMap (first chords)))
                            (count (length uniques-notes)))
                       (dolist (l (cdr chords) (,max-or-exact (- count (length uniques-notes)) num))
                         (setq l (funcall ,choiceMap l)
                               uniques-notes (union  uniques-notes l :test test))
                         (incf count (length l))
                         ))))))  

(defun ch-repets-exact (test num &rest chords)
  (let* ((uniques-notes (dx->x (first chords) (second chords)))
         (count (length uniques-notes)))
    (do ((l (cddr chords) (cddr l))) 
        ((null l)  (= (- count (length uniques-notes)) num))
      (incf count (1+ (length (second l))))
      (setq uniques-notes
            (union  uniques-notes 
                    (dx->x (first l) (second l))
                    :test test))
      )))

#+:OM
(om::defmethod! om::renouv-haut ((exp  list) &optional variableMap choiceMap)
  (change-pitches (fill-expression exp)
                  #'ch-repets-exact #'ch-repets-max variableMap choiceMap))
#+:OM
(om::defmethod! om::pts_rnw ((exp  list) &optional c-imp variableMap choiceMap) 
  :initvals '((0_6s2 (2 max 4) 7_10 (3 5)) 1 nil nil) :icon 520
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map")
  :doc "Renewal of points in objects Specifies number of repeated points in objects subsequences
For example, interpreting objects as chords, the expression (0_6s2 (max 2 4) 6_10 (3 5)) constraints chords number 0,2,4,6 so that in each length two
subsequence of these (i.e. 0 and 2, 2 and 4, 4 and 6) there should be at most 4 equal notes. It also constraints
chords number 7,8,9 and 10 so that each length three subsequence of these (i.e. 7,8,9 and 8,9,10) there should be
EXACTLY 5 equal notes"
  (if (or variableMap choiceMap)
    (includeConstraint-Importance
     (change-pitches (fill-expression exp)
                    (construct-repets-max-exact (or choiceMap #'identity) '=)
                    (construct-repets-max-exact (or choiceMap #'identity) '<)
                    (or variableMap #'single-variable-map (or choiceMap #'identity)))
     (or c-imp 1))
    ;;(change-pitches (fill-expression exp) #'ch-repets-exact #'ch-repets-max variableMap choiceMap)
    (eval `(defun ,(gensym "pts_rnw") ()
             ',(includeConstraint-Importance
                (change-pitches (fill-expression exp) #'ch-repets-exact #'ch-repets-max) 
                (or c-imp 1))
             ))
  ))

#+:OM
(om::defmethod! om::pitch/ch-rnw ((exp  list) &optional c-imp variableMap choiceMap) 
  :initvals '((0_6s2 (2 max 4) 7_10 (3 5)) 1 nil nil) :icon 520
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map") 
  :doc "kept for compatibility. See pts_rnw"
  (om::pts_rnw exp c-imp variableMap choiceMap))

#+:PW
(pw::defunp pitch/ch-rnw ((exp  pw::list (:value '(0_6s2 (max 2 4) 7_10 (3 5))))) list
 "Renewal of pitches in chords. Specifies number of repeated notes in chord subsequences
For example, the expression (0_6s2 (max 2 4) 6_10 (3 5)) constraints chords number 0,2,4,6 so that in each length two
subsequence of these (i.e. 0 and 2, 2 and 4, 4 and 6) there should be at most 4 equal notes. It also constraints
chords number 7,8,9 and 10 so that each length three subsequence of these (i.e. 7,8,9 and 8,9,10) there should be
EXACTLY 5 equal notes"
(change-pitches (fill-expression exp))
  )

;;; (funcall (caar (funcall (renouv-haut '(0 5 (2 1) 6 10 (3 2))))) 1 '( 1 2) 1 '(1 2))
;;; (renouv-haut '(1 5 (3 1)))

(defun constraint-parallels (v1 v2 logfun Intervals &optional choiceMap)
  (let (Ns len)
    (dolist (int Intervals)
      (setq int (funcall choiceMap int) len (length int))
      (push (reduce *integrate-distance-fun* (subseq int (if (> v1 len) len v1) (if (> v2 len) len v2)))
            Ns)
      )
    (funcall logfun (remove  nil (nreverse Ns) :test #'equal))
    ))

(defun forbidden-parallel-intervals (expression &optional varMap choiceMap)
  "exp ::= ( Ci1 Cj1  <Logical Expression>1 ... Cik Cjk  <Logical Expression>k ).
The Lowest intervals of contiguous chords between Ciu and Cju should satisfy 
the <Logical Expression>u.
<Logical Expression> is a shorthand for ( 0 MaxChords <Logical Expression>)
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 5 (2 1) 6 8 (7 11))
"
  (let (res  all-indexes indexes  voice1 voice2 size (form expression ) step? end?)
    (do ((reps) (logexp) (fun1)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form) step? nil end? nil)
      (do ((reps reps (cdddr reps))) ((or (null reps) end?))
        (when (and (symbolp (first reps)) (string= (first reps) *step-name*))
          (setf step? t)
          (if (consp (second reps)) (setf reps (second reps)) (setf reps (cdr reps))))
        (when  (or (flatp reps)
                   (and (symbolp (first reps))
                        (not (member (first reps) (rest *voice-names*) :test #'string=))
                        ))
          (setq reps (list 0 1 reps) end? t))
        (setq logexp (third reps) voice1 (numerize (voice1 reps)) voice2 (numerize (voice2 reps)))
        (setq voice1 (min voice1 voice2) voice2 (max voice1 voice2))
        (setq size (Max-node (if (flatp logexp) (list logexp) logexp) #'Fmax-leaf 0))
        (when (or varMap choiceMap)
          (setq varMap (or varMap #'single-variable-Map) choiceMap (or choiceMap #'identity)))
        (setq fun1 (eval 
                     `(function 
                      (lambda (&REST Chords)
                        (constraint-parallels ,voice1 ,voice2 
                                              ,(DELAYCONSTR::set-domain-constraint-fun logexp)
                                              chords ,(or choiceMap #'identity))))))
        (if step?
          (do ((ind (nreverse indexes) (nthcdr size ind))) ((< (length ind) size))
            (setf res 
                  (append (build-sequence-constraint fun1 (nreverse (subseq ind 0 size)) 0 size nil #'var2 1 varMap)
                          res)))
          (when (>= (length all-indexes) size)
            (do  ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)))
                 ((or (< (length all-ind) size) (null ind)))
              (setf res (append (build-sequence-constraint fun1 (subseq all-ind 0 size) 0 size nil #'var2 1 varMap)
                                res)) ))
          )  )
      )
    (if (or varMap choiceMap) res (eval `(defun ,(gensym "i/i-dst_filt-constraint") () ',res)))
    ))





#+:OM
(om::defmethod! om::int//forbid ((exp  list)) (forbidden-parallel-intervals (fill-expression exp))  )

#+:OM
(om::defmethod! om::i/i-dst_filt ((exp  list) &optional c-imp varMap choiceMap) 
:initvals '((0_5 (0 1 (not (4 4)) 0 u (and (15 15)))) 1 nil nil)
  :indoc '("expression" "variable Map" "cnstr importance" "choice Map") :icon 520
  :doc "Filtering positioned internal distances. The succession of selected object distances in a given region should match a supplied pattern.
For example, if objects are interpreted as chords, the expression (0_5 (0 1 (not (4 4)) 0 u (and (15 15)))) constraints the first six chords so that
each pair of consecutive chords should not have two consecutive thirds in their lower intervals and, at the same
time, should form consecutive minor tenths between the upper and lower notes. Thus a possible solution could
be the chord sequence
(33 40 43 46 48) (34 37 39 41 43) (37 40 43 45 47) (32 37 40 42 44) (30 37 42 44 46) (32 35 37 39 41)
 I<---15------>I   I<----15---->I   I<----15---->I ...........etc. 
'varMap' is a unary function, mapping
variable indexes to internal index+level. 'choiceMap' is a unary
function that is applied to the instance before invoking the constraint."
(if (or varMap choiceMap)
    (includeConstraint-Importance 
     (forbidden-parallel-intervals (fill-expression exp) varMap
                                   (if choiceMap (eval `(function (lambda (x) (x->dx (funcall ,choiceMap x))))) nil)  )
     c-imp)
    (set-importance-in-namedCnstr "i/i-dst_filt" (fill-expression exp) #'forbidden-parallel-intervals c-imp)
    ))



#+:OM
(om::defmethod! om::vintsuc/pos-filt ((exp  list) &optional c-imp varMap choiceMap) 
:initvals '((0_5 (0 1 (not (4 4)) 0 u (and (15 15)))) 1 nil nil)
  :indoc '("expression" "variable Map" "cnstr importance" "choice Map") :icon 520
  :doc "Kept for compatibility. See i/i-dst_filt"
(om::i/i-dst_filt exp c-imp varMap choiceMap))

#+:PW
(pw::defunp vintsuc/pos-filt ((exp  pw::list (:value '(0_5 (0 1 (not (4 4)) 0 u (and (15 15))))))) list
 "Filtering positioned vertical intervals. The succession of chosen chord intervals in a given region should match a supplied pattern.
For example, the expression (0_5 (0 1 (not (4 4)) 0 u (and (15 15)))) constraints the first six chords so that
each pair of consecutive chords should not have two consecutive thirds in their lower intervals and, at the same
time, should form consecutive minor tenths between the upper and lower notes. Thus a possible solution could
be the chord sequence (33 40 43 46 48) (34 37 39 41 43) (37 40 43 45 47) (32 37 40 42 44) (30 37 42 44 46) (32 35 37 39 41)
                      I<---15------>I   I<----15---->I   I<----15---->I ...........etc. 
"
(forbidden-parallel-intervals (fill-expression exp))  )

;;(funcall (car (last+ (funcall (int//forbid '(5 8 (not (2 2 2) ) 6 8 (not (or (7 11))))))) ) '( 2 2)  '(2 2) '( 2 2 2))
;;(funcall (caar (int//forbid '(0 5 (not (or (2 2)) ))))  '( 2 2)  '())

(defun upper-nb-dir=Ok-min (test &rest chords)
  (declare (ignore test))
  (if (not (fifth chords)) t
      (let ((sign (signum (delayconstr::approx-to-space-step 
                           (- (+ (third chords) (fourth chords)) (+ (first chords) (second chords)))))))
        (do ((ch (cddr chords) (cddr ch))) ((null (cddr ch)) nil)
          (when (/= sign (signum (delayconstr::approx-to-space-step 
                                         (- (+ (third ch) (fourth ch)) (+ (first ch) (second ch))))))
            (return t))))) )

(defun construct-General-min (choiceMap)
  (eval `(function
          (lambda (test &rest chords) (declare (ignore test))
            (if (not (third chords)) t
                (let ((sign (signum (delayconstr::approx-to-space-step 
                                     (- (funcall ,choicemap (second chords)) (funcall ,choicemap (first chords)))))))
        (do ((ch (cdr chords) (cdr ch))) ((null ch) nil)
          (when (/= sign (signum (delayconstr::approx-to-space-step 
                                         (- (funcall ,choicemap (second ch)) (funcall ,choicemap (first ch))))))
            (return t))))) ))))

(defun upper-nb-dir=Ok-up (test &rest chords)
  (declare (ignore test))
  (do ((ch chords (cddr ch))) ((null (cddr ch)) t)
    (unless (delayconstr::approx> (+ (third ch) (fourth ch)) (+ (first ch) (second ch)))
      (return nil))))

(defun construct-general-up (choiceMap)
  (eval `(function
          (lambda (test &rest chords) (declare (ignore test))
            (do ((ch chords (cdr ch))) ((null (second ch)) t)
              (unless (delayconstr::approx> (funcall ,choicemap (second ch)) (funcall ,choicemap (first ch)))
                (return nil)))))))

(defun upper-nb-dir=Ok-down (test &rest chords)
  (declare (ignore test))
  (do ((ch chords (cddr ch))) ((null (cddr ch)) t)
    (unless (delayconstr::approx<  
                     (+ (third ch) (fourth ch)) (+ (first ch) (second ch)))
      (return nil))))

(defun construct-general-down (choiceMap)
  (eval `(function
          (lambda (test &rest chords) (declare (ignore test))
            (do ((ch chords (cdr ch))) ((null (second ch)) t)
              (unless (delayconstr::approx< (funcall ,choicemap (second ch)) (funcall ,choicemap (first ch)))
                (return nil)))))))

(defun lower-nb-dir=Ok-min (test &rest chords) (declare (ignore test))
  (if (not (third chords)) t
      (let ((sign (signum (delayconstr::approx-to-space-step (- (second chords) (first chords))))))
        (do ((ch (cdr chords) (cdr ch))) ((null (cdr ch)) nil)
          (when (/= sign (signum (delayconstr::approx-to-space-step (- (second ch) (first ch) ))))
            (return t))))))


(defun lower-nb-dir=Ok-up (test &rest chords) (declare (ignore test))
  (do ((ch chords (cdr ch))) ((null (cdr ch)) t)
    (when (delayconstr::approx<= (second ch)
              (first ch)) (return nil))))

(defun lower-nb-dir=Ok-down (test &rest chords) (declare (ignore test))
  (do ((ch chords (cdr ch))) ((null (cdr ch)) t)
    (when (delayconstr::approx>=  (second ch) (first ch)) (return nil))))

(defun voice-nb-dir=Ok-min (voice test &rest chords) (declare (ignore test))
   (if (not (fifth chords)) t
       (let* ((l1 (second chords)) (l2 (fourth chords)) 
              (index1 (length l1)) (index2 (length l2)) ok?)
         (when (<= voice index1) (setq index1 voice) (setq ok? t))
         (when (<= voice index2) (setq index2 voice) (setq ok? t))
         (let ((ch2 (dx->x (third chords) l2))  sign ch1)
           (setq sign (signum (delayconstr::approx-to-space-step 
                               (- (nth index2 ch2) (nth index1 (dx->x (first chords) (second chords)))))))
           (do ((ch (cddr chords) (cddr ch))) ((null (cddr ch)) (not ok?))
             (setq l1 l2 l2 (fourth ch))
             (setq index1 index2 index2 (length l2))
             (when (<= voice index2) (setq index2 voice) (setq ok? t))
             (setq ch1 ch2) (setq ch2 (dx->x (third ch) (fourth ch)))
             (when (/= sign (signum (delayconstr::approx-to-space-step (- (nth index2 ch2) (nth index1 ch1)))))
               (return t)))
           ))))

(defun construct-general-voice-min (choiceMap)
  (eval `(function 
          (lambda (voice test &rest chords) (declare (ignore test))
            (if (not (third chords)) t
                (let* ((ch1 (funcall ,choiceMap (first chords))) (ch2 (funcall ,choiceMap (second chords))) 
                       (index1 (length ch1)) (index2 (length ch2)) ok?)
                  (when (< voice index1) (setq index1 voice) (setq ok? t))
                  (when (< voice index2) (setq index2 voice) (setq ok? t))
                  (let (sign ch1)
                    (setq sign (signum (delayconstr::approx-to-space-step 
                                        (- (nth index2 ch2) (nth index1 ch1)))))
                    (do ((ch (cdr chords) (cdr ch))) ((null (cdr ch)) (not ok?))
                      (setq ch1 ch2 ch2 (funcall ,choiceMap (second ch)))
                      (setq index1 index2 index2 (length ch2))
                      (when (< voice index2) (setq index2 voice) (setq ok? t))
                      (when (/= sign (signum (delayconstr::approx-to-space-step (- (nth index2 ch2) (nth index1 ch1)))))
                        (return t)))
                    )))))))

(defun voice-nb-dir=Ok-up (voice test &rest chords) (declare (ignore test))
   (if (not (third chords)) t
       (let* ((l1 (second chords)) (l2 (fourth chords)) 
              (index1 (length l1)) (index2 (length l2)) ok?)
         (when (<= voice index1) (setq index1 voice) (setq ok? t))
         (when (<= voice index2) (setq index2 voice) (setq ok? t))
         (let ((ch2 (dx->x (third chords) l2)) ch1)
           (if (delayconstr::approx<= (nth index2 ch2) (nth index1 (dx->x (first chords) (second chords))))
             (not ok?)
             (do ((ch (cddr chords) (cddr ch))) ((null (cddr ch)) t)
               (setq l1 l2 l2 (fourth ch))
               (setq index1 index2 index2 (length l2))
               (when (<= voice index2) (setq index2 voice) (setq ok? t))
               (setq ch1 ch2) (setq ch2 (dx->x (third ch) (fourth ch)))
               (unless (delayconstr::approx> (nth index2 ch2) (nth index1 ch1))
                 (return (not ok?)))) )
           ))))

(defun construct-general-voice-updown (choiceMap up?)
  (let ((f (if up? 'delayconstr::approx> 'delayconstr::approx<)))
    (eval `(function
            (lambda (voice test &rest chords) (declare (ignore test))
              (if (not (second chords)) t
                  (let* ((ch1 (funcall ,choiceMap (first chords))) (ch2 (funcall ,choiceMap (second chords))) 
                         (index1 (length ch1)) (index2 (length ch2)) ok?)
                    (when (< voice index1) (setq index1 voice) (setq ok? t))
                    (when (< voice index2) (setq index2 voice) (setq ok? t))
                    (if (delayconstr::approx<= (nth index2 ch2) (nth index1 ch1))
                      (not ok?)
                      (do ((ch (cdr chords) (cdr ch))) ((null (cdr ch)) t)
                        (setq ch1 ch2 ch2 (funcall ,choiceMap (second ch)))
                        (setq index1 index2 index2 (length ch2))
                        (when (< voice index2) (setq index2 voice) (setq ok? t))
                        (unless (,f (nth index2 ch2) (nth index1 ch1))
                          (return (not ok?)))) )
                    )))))))

(defun voice-nb-dir=Ok-down (voice test &rest chords) (declare (ignore test))
   (if (not (third chords)) t
       (let* ((l1 (second chords)) (l2 (fourth chords)) 
              (index1 (length l1)) (index2 (length l2)) ok?)
         (when (<= voice index1) (setq index1 voice) (setq ok? t))
         (when (<= voice index2) (setq index2 voice) (setq ok? t))
         (let ((ch2 (dx->x (third chords) l2)) ch1)
           (if (delayconstr::approx>= (nth index2 ch2) (nth index1 (dx->x (first chords) (second chords))))
             (not ok?)
             (do ((ch (cddr chords) (cddr ch))) ((null (cddr ch)) t)
               (setq l1 l2 l2 (fourth ch))
               (setq index1 index2 index2 (length l2))
               (when (<= voice index2) (setq index2 voice) (setq ok? t))
               (setq ch1 ch2) (setq ch2 (dx->x (third ch) (fourth ch)))
               (unless (delayconstr::approx< (nth index2 ch2) (nth index1 ch1))
                 (return (not ok?)))) )
           ))))

(defun equal-directions (expression &optional vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  "exp ::= ( Ci1 Cj1  m1 ... Cik Cjk  mk ).
Direction of the highest voice should to chance at most
once in each secuence of mu + 2 contiguous chords (for all 1<=u<=k)  
between chords Ciu Cju.
m is a shorthand for ( 0 MaxChords m)
See MaxChords box in the menu UserLib/Music Engine.
 Example: 
   exp = (1 3 1 4 8 2 )
" 
(if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (set-direction-constr expression (construct-general-up (or cmapUpper #'identity))
                        (construct-general-down (or cmapUpper #'identity))
                        (construct-general-min (or cmapUpper #'identity))
                        (construct-general-up (or cmapLower #'identity))
                        (construct-general-down (or cmapLower #'identity))
                        (construct-general-min (or cmapLower #'identity))
                        (construct-general-voice-updown (or cmapUpper #'identity) t)
                        (construct-general-voice-updown (or cmapLower #'identity) nil)
                        (construct-general-voice-min (or cmapInternal #'identity))
                        2 "x_prof-constraint" nil vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (set-direction-constr expression #'upper-nb-dir=Ok-up #'upper-nb-dir=Ok-down #'upper-nb-dir=Ok-min
                        #'lower-nb-dir=Ok-up #'lower-nb-dir=Ok-down #'lower-nb-dir=Ok-min
                        #'voice-nb-dir=Ok-up #'voice-nb-dir=Ok-down #'voice-nb-dir=Ok-min
                        2 "x_prof-constraint")
  ))

;;(all-lists-2 4 15 1 0 1)

#+:OM (om::defmethod! om::nb-dir=OK ((exp list))  (equal-directions (fill-expression exp)))
#+:OM
(om::defmethod! om::x_prof ((exp list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
 :initvals '((0_5 (u (2 -1) 0 (max 2))) 1 nil nil nil nil nil nil)
 :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
 :icon 520
  :doc "Horizontal profile. Constrains selected points in contoguous objects to follow a given profile.
For example, interpreting objects as chords, the expression (0_5 (u (2 -1) 0 (max 2))) constraints the first six chords so that the
upper voice follows the pattern of two upward movements followed by a downward movement. This pattern
is then repeated until the 6 chords are exhausted. The lower voice is constrained to have a
maximum of two consecutive movements in the same direction in all subsequences of the first six 
chords"
  (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (includeConstraint-Importance
   (equal-directions (fill-expression exp) vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
   (or c-imp 1))
  (set-importance-in-namedCnstr "x-prof" (fill-expression exp) #'equal-directions c-imp)))

#+:OM
(om::defmethod! om::v-prof ((exp list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
 :initvals '((0_5 (u (2 -1) 0 (max 2))) 1 nil nil nil nil nil nil)
 :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
 :icon 520
  :doc "Kept for compatibility. See x_prof"
  (om::x_prof exp c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal))

#+:PW
(pw::defunp v-prof ((exp pw::list (:value '(0_5 (u (2 -1) 0 (max 2)))))) list
"Voice profile. Constraints a given voice to follow a given melodic profile.
For example, the expression (0_5 (u (2 -1) 0 (max 2))) constraints the first six chords so that the
upper voice follows the pattern of two upward movements followed by a downward movement. This pattern
is then repeated until the 6 chords are exhausted. The lower voice is constrained to have a
maximum of two consecutive movements in the same direction in all subsequences of the first six 
chords"
  (equal-directions (fill-expression exp)))


(defun voice1 (exp) (first exp))
(defun voice2 (exp) (second exp))
(defun factor (exp) (first (third exp)))
(defun test-names (x y)
  (cond ((and (symbolp x) (symbolp y)) (string= x y))
        ((and (numberp x) (numberp y)) (= x y))
        (t nil)))

(defun numerize (voice)
  (cond ((numberp voice) voice)
        ((string= voice *lower-voice-name*) 0)
        ((string= voice *upper-voice-name*) most-positive-fixnum)
        (t (error "wrong voice name"))))

(defun set-up-parallel-mv-predicate (expr f1 f2 f3 &optional voice1 voice2)
  (if (and (symbolp (first expr)) (member (first expr) *min-max-spec* :test #'string=))
    (cons (eval
           `(function (lambda (&rest chords)
                        (apply ,f2  
                               ,@ (if voice1 `(,(numerize voice1) ,(numerize voice2) chords) `(chords))))) )
          (rest expr))
    (list
     (and f3 (eval `(function (lambda (&rest chords)
                                (apply ,f3
                                       ,@ (if voice1 `(,(numerize voice1) ,(numerize voice2) chords) `(chords)))))))
     (first expr)
     (eval `(function (lambda (&rest chords)
                        (apply ,f1
                               ,@ (if voice1 `(,(numerize voice1) ,(numerize voice2) chords) `(chords))))) )
     )))

(defun number-of-parallel-movements (expression &optional vmapInternal cmapUpper cmapLower cmapInternal)
  "exp"
  (let* (res  all-indexes indexes  voice1 voice2 (form expression )
         (options? (or vmapInternal cmapUpper cmapLower cmapInternal))
         op-exact-fun op-OPT-fun op-max-fun op-exact-voice-fun op-OPT-voice-fun op-max-voice-fun)
    (if options?
      (setq op-exact-fun (construct-general-nb-mov-exact cmapUpper cmapLower)
            op-OPT-fun  (construct-general-nb-mov-OPT cmapUpper cmapLower)
            op-max-fun (construct-general-nb-mov-max cmapUpper cmapLower)
            op-exact-voice-fun (construct-general-nb-exact-voice cmapInternal)
            op-OPT-voice-fun (construct-general-nb-OPT-voice cmapInternal)
            op-max-voice-fun (construct-general-nb-max-voice cmapInternal))
      (setq op-exact-fun #'const-nb-mov-ok-exact
            op-OPT-fun  #'const-nb-mov-ok-OPT
            op-max-fun #'const-nb-mov-ok-max
            op-exact-voice-fun #'voices-nb-mov-ok-exact
            op-OPT-voice-fun #'voices-nb-mov-ok-OPT
            op-max-voice-fun #'voices-nb-mov-ok-max))
    (do ((reps) (alf) (fun1) (fun2) ) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq fun1 NIL fun2 NIL)
      (if  (flatp reps) 
        (progn
          (setq fun1 (set-up-parallel-mv-predicate reps op-exact-fun op-max-fun op-OPT-fun))
          (setf res (append (optimize-vvparallel-constraints all-indexes indexes fun1 2 #'var1 #'var2 nil vmapInternal) res)) )
        (do ((part reps (cdddr part))) ((null part))
          (setf alf (third part) voice1 (voice1 part) voice2 (voice2 part))
          (cond ((subsetp (list voice1 voice2) *voice-names* :test #'test-names)
                 (setq fun1 (set-up-parallel-mv-predicate alf op-exact-fun op-max-fun op-OPT-fun))  )
                (t (setq fun2 (set-up-parallel-mv-predicate alf op-exact-voice-fun
                                                           op-max-voice-fun op-OPT-voice-fun voice1 voice2)
                         )))
          (setf res (append (optimize-vvparallel-constraints all-indexes indexes fun1 2 #'var1 #'var2 nil vmapInternal) res))
          (setf res (append (optimize-vvparallel-constraints all-indexes indexes fun2 2 #'var1 #'var2 1 vmapInternal) res))
          ) )
      )
    (if options? res
        (eval `(defun ,(gensym "x/x_prof-constraint") () ', res)))
    )
  )

(defun optimize-vvparallel-constraints (all-indexes indexes form add map1 &optional map2 level variableMap)
  (let (res size)
    (when (and form (>= (length all-indexes) (setq size (+ (second form) add))))
      (if (functionp (third form))
        (let ((f1 (first form)) (f3 (third form)) subs)
          (do ((inds (nreverse indexes) (nthcdr (1- size) inds))) ((< (length inds) size) res)
            (when f1
              (do ((subindxs (subseq inds 0  (1- size)) (rest subindxs))) ((null (cdr subindxs)))
                (setf res (append (build-sequence-constraint f1 (list (second subindxs) (first subindxs))
                                                             0 2 map1 map2 level variableMap) res))))
            (setq subs (nreverse (subseq inds 0 size)))
            (setf res (append (build-sequence-constraint f3 (list (first subs) (second subs)) 0 2 map1 map2 level variableMap)
                              res))
            ))         
        (let (f)
          (setq f (first form))
          (do  ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)))
               ((or (< (length all-ind) size) (null ind)))
            (setf res (append (build-sequence-constraint f  (subseq all-ind 0 size)
                                                         0 size map1 map2 level variableMap) res))))))
     res))

(defun const-nb-mov-ok-max (&rest chords)
  (if (not (cddr chords)) t
      (let (sign dif1 dif2)
        (do ((ch chords (cddr ch))) ((null (cddr ch)) nil)
          (setq dif1  (delayconstr::approx-to-space-step (funcall *distance-function* (third ch) (first ch)))
                dif2 (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                 (funcall *integrate-distance-fun* (third ch) (fourth ch))
                                                                 (funcall *integrate-distance-fun* (first ch) (second ch))))
                sign (* dif1 dif2))
          (unless (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2))))
            (return t))))))

(defun construct-general-nb-mov-max (cmapUpper cmapLower)
  (eval `(function 
          (lambda (&rest chords)
            (if (not (second chords)) t
                (let (sign dif1 dif2)
                  (do ((ch chords (cdr ch))) ((null (second ch)) nil)
                    (setq dif1  (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                            (funcall ,cmapLower (second ch))
                                                                            (funcall ,cmapLower (first ch))))
                          dif2 (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                           (funcall ,cmapUpper (second ch))
                                                                           (funcall ,cmapUpper (first ch))))
                          sign (* dif1 dif2))
                    (unless (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2))))
                      (return t)))))))))

(defun const-nb-mov-ok-exact (&rest chords) 
  (if (not (cddr chords)) t
      (let (sign dif1 dif2)
        (let ((ch chords))   ;;;(nthcdr (- (length chords) 4) chords)))
          (setq dif1  (delayconstr::approx-to-space-step (funcall *distance-function* (third ch) (first ch)))
                dif2 (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                 (funcall *integrate-distance-fun* (third ch) (fourth ch))
                                                                 (funcall *integrate-distance-fun* (first ch) (second ch))))
                sign (* dif1 dif2))
          (not (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))))))))

(defun construct-general-nb-mov-exact (cmapUpper cmapLower)  
  (eval `(function (lambda (&rest chords)
                     (if (not (second chords)) t
                         (let (sign dif1 dif2)
                           (let ((ch chords))  ;;;(nthcdr (- (length chords) 2) chords)))
                             (setq dif1  (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                                     (funcall ,cmapLower (second ch))
                                                                                     (funcall ,cmapLower (first ch))))
                                   dif2 (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                                    (funcall ,cmapUpper (second ch))
                                                                                    (funcall ,cmapUpper (first ch))))
                                   sign (* dif1 dif2))
                             (not (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2))))))))))))

(defun const-nb-mov-ok-OPT (&rest ch)
  (let* ((dif1  (delayconstr::approx-to-space-step (funcall *distance-function* (third ch) (first ch))))
         (dif2 (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                 (funcall *integrate-distance-fun* (third ch) (fourth ch))
                                                                 (funcall *integrate-distance-fun* (first ch) (second ch)))))
         (sign (* dif1 dif2)))
    (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2))))))

(defun construct-general-nb-mov-OPT (cmapUpper cmapLower)
  (eval `(function (lambda (&rest chords)
                     (let* ((dif1  (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                                     (funcall ,cmapLower (second chords))
                                                                                     (funcall ,cmapLower (first chords)))))
                            (dif2 (delayconstr::approx-to-space-step (funcall *distance-function*
                                                                           (funcall ,cmapUpper (second chords))
                                                                           (funcall ,cmapUpper (first chords)))))
                            (sign (* dif1 dif2)))
                       (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))))))))
                     
(defun voices-nb-mov-ok-max (voice1 voice2 &rest chords)
  (if (not (cddr chords)) t
      (let* ((ch2  (dx->x (first chords) (second chords)))
             (len2 (1- (length ch2)))
             dif1 dif2 sign ch1 len1 ok?)
        (do ((ch chords (cddr ch))) ((null (cddr ch)) (not ok?))
          (setq ch1 ch2 len1 len2 ch2 (dx->x (third ch) (fourth ch)))
          (setq len2 (1- (length ch2)))
          (setq dif1 (delayconstr::approx-to-space-step 
                      (funcall *distance-function*
                               (nth (if (> voice1 len2) len2 voice1) ch2) (nth (if (> voice1 len1) len1 voice1) ch1)))
                dif2 (delayconstr::approx-to-space-step 
                      (funcall *distance-function*
                               (nth (if (> voice2 len2) len2 voice2) ch2) (nth (if (> voice2 len1) len1 voice2) ch1)))
                sign (* dif1 dif2))
          (when (and (not ok?) (or (<= voice1 len1) (<= voice1 len2)
                                   (<= voice2 len1) (<= voice2 len2)))
            (setq ok? t))
          (unless (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))) (return t))
          ))))

(defun construct-general-nb-max-voice (choiceMap)
  (eval `(function 
          (lambda (voice1 voice2 &rest chords)
            (if (not (cdr chords)) t
                (let* ((ch2  (funcall ,choiceMap (first chords)))
                       (len2 (1- (length ch2)))
                       dif1 dif2 sign ch1 len1 ok?)
                  (do ((ch chords (cdr ch))) ((null (cdr ch)) (not ok?))
                    (setq ch1 ch2 len1 len2 ch2 (funcall ,choiceMap (second chords)))
                    (setq len2 (1- (length ch2)))
                    (setq dif1 (delayconstr::approx-to-space-step 
                                (funcall *distance-function*
                                         (nth (if (> voice1 len2) len2 voice1) ch2) (nth (if (> voice1 len1) len1 voice1) ch1)))
                          dif2 (delayconstr::approx-to-space-step 
                                (funcall *distance-function*
                                         (nth (if (> voice2 len2) len2 voice2) ch2) (nth (if (> voice2 len1) len1 voice2) ch1)))
                          sign (* dif1 dif2))
                    (when (and (not ok?) (or (<= voice1 len1) (<= voice1 len2)
                                             (<= voice2 len1) (<= voice2 len2)))
                      (setq ok? t))
                    (unless (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))) (return t))
                    )))))))

(defun voices-nb-mov-ok-exact (voice1 voice2 &rest chords)
  (if (not (cddr chords)) t
      (let* ((chs chords)   ;;;(nthcdr (- (length chords) 4) chords))
             (ch1 (dx->x (first chs) (second chs))) (len1 (1- (length ch1)))
             (ch2 (dx->x (third chs) (fourth chs))) (len2 (1- (length ch2)))
             dif1 dif2 sign ok?)
        (setq dif1  (delayconstr::approx-to-space-step 
                     (funcall *distance-function* (nth (if (> voice1 len2) len2 (progn (setq ok? t) voice1)) ch2)
                        (nth (if (> voice1 len1) len1 (progn (setq ok? t) voice1)) ch1)))
              dif2 (delayconstr::approx-to-space-step 
                    (funcall *distance-function* (nth (if (> voice2 len2) len2 (progn (setq ok? t) voice2)) ch2)
                      (nth (if (> voice2 len1) len1 (progn (setq ok? t) voice2)) ch1)))
              sign (* dif1 dif2))
        (if (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))) (not ok?) t))
      ))

(defun construct-general-nb-exact-voice (choiceMap)
  (eval `(function 
          (lambda (voice1 voice2 &rest chords)
            (if (not (cdr chords)) t
                (let* ((ch1 (funcall ,choiceMap (first chords))) (len1 (1- (length ch1)))
                       (ch2 (funcall ,choiceMap (second chords))) (len2 (1- (length ch2)))
                       dif1 dif2 sign ok?)
                  (setq dif1  (delayconstr::approx-to-space-step 
                               (funcall *distance-function* (nth (if (> voice1 len2) len2 (progn (setq ok? t) voice1)) ch2)
                                        (nth (if (> voice1 len1) len1 (progn (setq ok? t) voice1)) ch1)))
                        dif2 (delayconstr::approx-to-space-step 
                              (funcall *distance-function* (nth (if (> voice2 len2) len2 (progn (setq ok? t) voice2)) ch2)
                                       (nth (if (> voice2 len1) len1 (progn (setq ok? t) voice2)) ch1)))
                        sign (* dif1 dif2))
                  (if (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))) (not ok?) t))  )))))

(defun voices-nb-mov-ok-OPT (voice1 voice2 &rest chords)
  (let* ((ch1 (dx->x (first chords) (second chords))) (len1 (1- (length ch1)))
         (ch2 (dx->x (third chords) (fourth chords))) (len2 (1- (length ch2)))
         dif1 dif2 sign ok?)
    (setq dif1  (delayconstr::approx-to-space-step 
                 (funcall *distance-function* (nth (if (> voice1 len2) len2 (progn (setq ok? t) voice1)) ch2)
                   (nth (if (> voice1 len1) len1 (progn (setq ok? t) voice1)) ch1)))
          dif2 (delayconstr::approx-to-space-step 
                (funcall *distance-function* (nth (if (> voice2 len2) len2 (progn (setq ok? t) voice2)) ch2)
                  (nth (if (> voice2 len1) len1 (progn (setq ok? t) voice2)) ch1)))
          sign (* dif1 dif2))
    (if (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))) t (not ok?)))
  )

(defun construct-general-nb-OPT-voice (choiceMap)
  (eval `(function 
          (lambda (voice1 voice2 &rest chords)
            (let* ((ch1 (funcall ,choiceMap (first chords))) (len1 (1- (length ch1)))
                   (ch2 (funcall ,choiceMap (second chords))) (len2 (1- (length ch2)))
                   dif1 dif2 sign ok?)
              (setq dif1  (delayconstr::approx-to-space-step 
                           (funcall *distance-function* (nth (if (> voice1 len2) len2 (progn (setq ok? t) voice1)) ch2)
                                    (nth (if (> voice1 len1) len1 (progn (setq ok? t) voice1)) ch1)))
                    dif2 (delayconstr::approx-to-space-step 
                          (funcall *distance-function* (nth (if (> voice2 len2) len2 (progn (setq ok? t) voice2)) ch2)
                                   (nth (if (> voice2 len1) len1 (progn (setq ok? t) voice2)) ch1)))
                    sign (* dif1 dif2))
              (if (or (plusp sign) (and (zerop sign) (zerop (+ dif1 dif2)))) t (not ok?)))))))

#+:OM (om::defmethod! om::nb-mov//OK ((exp list)) (number-of-parallel-movements (fill-expression exp)))
#+:OM
(om::defmethod! om::x/x_prof ((exp list) &optional c-imp vmapInternal cmapUpper cmapLower cmapInternal)
  :initvals '((0_5 (0 u (max 2) 1 2 (3))) nil nil nil nil nil ) 
  :indoc '("expression" "constr importance" "var map-internal voice" "choice map-upper voice"
          "choice map-lower voice" "choice map-internal voice")
  :icon 520
  :doc "External points-pair profile. Defines parallel movements of a given pair of sequences of selected points
in contiguous objects.
For example, interpreting objects as chords, the expression (0_5 (0 u (max 2) 1 2 (3))) constraints parallel movements
for the first six chords. The movements of the two extreme voices should not go consecutively 
in parallel more than twice in all subsequences of the first six chords. The movements
of the second and third voices (i.e. voices numbered 1 and 2) should go in parallel
exactly three times and then change directions."
  (if (or vmapInternal cmapUpper cmapLower cmapInternal)
    (includeConstraint-Importance
     (number-of-parallel-movements (fill-expression exp)
                                   (or vmapInternal #'single-variable-map) (or cmapUpper #'identity)
                                   (or cmapLower #'identity) (or cmapInternal #'identity))
     (or c-imp 1))
    (set-importance-in-namedCnstr "x/x-prof" (fill-expression exp) #'number-of-parallel-movements c-imp)
    ))

#+:OM
(om::defmethod! om::v/v-prof ((exp list) &optional c-imp vmapInternal cmapUpper cmapLower cmapInternal)
  :initvals '((0_5 (0 u (max 2) 1 2 (3))) nil nil nil nil nil ) 
  :indoc '("expression" "constr importance" "var map-internal voice" "choice map-upper voice"
          "choice map-lower voice" "choice map-internal voice")
  :icon 520
  :doc "Kept for compatibility. See x/x_prof"
  (om::x/x_prof exp c-imp vmapInternal cmapUpper cmapLower cmapInternal))


#+:PW
(pw::defunp v/v-prof ((exp  pw::list (:value '(0_5 (0 u (max 2) 1 2 (3)))))
                         ) list 
 "Voice-pair profile. Defines parallel movements of a given pair of voices.
For example, the expression (0_5 (0 u (max 2) 1 2 (3))) constraints parallel movements
for the first six chords. The movements of the two extreme voices should not go consecutively 
in parallel more than twice in all subsequences of the first six chords. The movements
of the second and third voices (i.e. voices numbered 1 and 2) should go in parallel
exactly three times and then change directions."
  (number-of-parallel-movements (fill-expression exp)))

;;;  (funcall (caar (funcall (nb-mov//OK '(1 3 1 4 8 2 ))))  2 4 5 6 7 1)
;;;  (nb-mov//OK '(5 8 3 ))



(defun build-sequence-constraint (f revindexes level arity factor-fun &optional fun2 extra-level varMap)
  (let ((size (length revindexes)) vars res nthj)
    (setq arity (min arity size))
    (do ((subind revindexes (cdr subind))) ((< (length subind) arity) res)
      (setf vars nil)
      (dotimes (j arity)
        (setf nthj (nth j subind))
        (if varMap 
          (setq vars (append (funcall varMap nthj) vars))
        (progn (when fun2  (push (list (funcall fun2 nthj) (or extra-level level)) vars))
               (when factor-fun (push (list (funcall factor-fun nthj) level) vars)))))
      (push (cons f vars) res))))

(defun horizontal-interval-test (x y) (delayconstr::match? y x))

(defun all-upper-test-fun (set)
  (eval `(function (lambda (xi xi+1 xi+2 xi+3)
                       ;;(member  (- (+ xi+2 xi+3) (+ xi xi+1)) ',set :test #'horizontal-interval-test)
                       (member  (funcall *distance-function* (funcall *integrate-distance-fun* xi+2 xi+3)
                                                     (funcall *integrate-distance-fun* xi xi+1))
                                ',set :test #'horizontal-interval-test)
                       ))))

(defun all-lower-test-fun (set)
  (eval `(function (lambda (xi  xi+2 )
                         ;;(member (- xi+2 xi) ',set :test #'horizontal-interval-test)
                         (member (funcall *distance-function* xi+2 xi) ',set :test #'horizontal-interval-test)
                         ))))

(defun upper&voice-test-fun (set voice)
  (eval `(function (lambda (xi xi+1 xi+2 xi+3 )
                           (let ((l2 (length xi+3)))
                             (if (> ,voice l2)
                               (member (funcall *distance-function* (nth l2 (dx->x xi+2 xi+3))
                                                (funcall *integrate-distance-fun* xi xi+1))
                                       ',set :test #'horizontal-interval-test)
                               (member (funcall *distance-function* (nth ,voice (dx->x xi+2 xi+3))
                                                (funcall *integrate-distance-fun* xi xi+1))
                                       ',set :test #'horizontal-interval-test)))))
              ))

(defun voice&upper-test-fun (set voice)
  (eval `(function (lambda (xi xi+1 xi+2 xi+3 )
                           (let ((l1 (length xi+1)))
                             (if (> ,voice l1)
                               (member (funcall *distance-function* 
                                                (funcall *integrate-distance-fun* xi xi+1) (nth l1 (dx->x xi+2 xi+3)))
                                       ',set :test #'horizontal-interval-test)
                               (member (funcall *distance-function* 
                                                (funcall *integrate-distance-fun* xi xi+1) (nth ,voice (dx->x xi+2 xi+3)))
                                       ',set :test #'horizontal-interval-test)))))
              ))

(defun upper&lower-test-fun (set)
  (eval `(function (lambda (xi xi+1 xi+2)
                       (member  (funcall *distance-function* xi+2 (funcall *integrate-distance-fun* xi xi+1))
                                ',set :test #'horizontal-interval-test) ))))

(defun lower&upper-test-fun (set)
  (eval `(function (lambda (xi xi+1 xi+2)
                       (member  (funcall *distance-function* (funcall *integrate-distance-fun* xi+1 xi+2) xi)
                                ',set :test #'horizontal-interval-test) ))))

(defun lower&voice-test-fun (set voice)
  (eval `(function (lambda (xi xi+1 xi+2)
                           (let ((l2 (length xi+2)))
                             (if (> ,voice l2)
                               (member (funcall *distance-function* (nth l2 (dx->x xi+2 xi+3)) xi)
                                       ',set :test #'horizontal-interval-test)
                               (member (funcall *distance-function* (nth ,voice (dx->x xi+2 xi+3)) xi)
                                       ',set :test #'horizontal-interval-test)))))
              ))

(defun voice&lower-test-fun (set voice)
  (eval `(function (lambda (xi xi+1 xi+2)
                           (let ((l2 (length xi+1)))
                             (if (> ,voice l2)
                               (member (funcall *distance-function* xi+2 (nth l2 (dx->x xi xi+1)))
                                       ',set :test #'horizontal-interval-test)
                               (member (funcall *distance-function* xi+2 (nth ,voice (dx->x xi xi+1)))
                                       ',set :test #'horizontal-interval-test)))))
              ))

(defun voice&voice-test-fun (set v1 v2)
  (eval `(function (lambda (xi xi+1 xi+2 xi+3 )
                           (let ((l1 (length xi+1)) (l2 (length xi+3)))
                             (cond ((and (<= ,v1 l1) (> ,v2 l2))
                                    (member (funcall *distance-function* (nth l2 (dx->x xi+2 xi+3))
                                                    (nth ,v1 (dx->x xi xi+1)))
                                            ',set :test #'horizontal-interval-test))
                                   ((and (<= ,v2 l2) (> ,v1 l1))
                                    (member (funcall *distance-function* (nth ,v2 (dx->x xi+2 xi+3))
                                                    (nth l1 (dx->x xi xi+1)))
                                            ',set :test #'horizontal-interval-test))
                                   ((and (> ,v2 l2) (> ,v1 l1)) t)
                                   (t (member (funcall *distance-function* (nth ,v2 (dx->x xi+2 xi+3))
                                                      (nth ,v1 (dx->x xi xi+1)))
                                              ',set :test #'horizontal-interval-test))))   ))
              ))

(defun build-int-hori-predicates (set rvoices rindexes)
  (let (result ind1 ind2 rv1 rv2)
    (setq rvoices
          (append (make-list (max 0 (- (length rindexes) (length rvoices))) :initial-element (first rvoices))
                  rvoices))
    (do () ((null rindexes) result)
      (setq ind2 (pop rindexes) ind1 (first rindexes))
      (setq rv2 (pop rvoices) rv1 (first rvoices))
      (unless ind1 (return result))
      (cond ((and (my-string= rv1 *upper-voice-name*) (my-string= rv2 *upper-voice-name*))
             (push (list (all-upper-test-fun set) (list (var1 ind1) 0) (list (var2 ind1) 0)
                                                  (list (var1 ind2) 0) (list (var2 ind2) 0))  result))
            ((and (my-string= rv1 *upper-voice-name*) (or (my-string= rv2 *lower-voice-name*) (zerop rv2)))
             (push (list (upper&lower-test-fun set) (list (var1 ind1) 0) (list (var2 ind1) 0)
                                                  (list (var1 ind2) 0))  result))
            ((my-string= rv1 *upper-voice-name*)
             (push (list (upper&voice-test-fun set rv2) (list (var1 ind1) 0) (list (var2 ind1) 0)
                                                        (list (var1 ind2) 0) (list (var2 ind2) 1))  result))
            ((and (or (my-string= rv1 *lower-voice-name*) (zerop rv1)) (my-string= rv2 *upper-voice-name*))
             (push (list (lower&upper-test-fun set) (list (var1 ind1) 0)
                                                    (list (var1 ind2) 0) (list (var2 ind2) 0))  result))
            ((my-string= rv2 *upper-voice-name*)
             (push (list (voice&upper-test-fun set rv1) (list (var1 ind1) 0) (list (var2 ind1) 1)
                                                        (list (var1 ind2) 0) (list (var2 ind2) 0))  result))
            ((and (or (my-string= rv1 *lower-voice-name*) (zerop rv1)) (or (my-string= rv2 *lower-voice-name*) (zerop rv2)))
             (push (list (all-lower-test-fun set) (list (var1 ind1) 0)
                                                    (list (var1 ind2) 0))  result))
            ((or (my-string= rv1 *lower-voice-name*) (zerop rv1))
             (push (list (lower&voice-test-fun set rv2) (list (var1 ind1) 0)
                                                    (list (var1 ind2) 0) (list (var2 ind2) 1))  result))
            ((or (my-string= rv2 *lower-voice-name*) (zerop rv2))
             (push (list (voice&lower-test-fun set rv1) (list (var1 ind1) 0) (list (var2 ind1) 1)
                                                        (list (var1 ind2) 0))  result))
            (t (push (list (voice&voice-test-fun set rv1 rv2) (list (var1 ind1) 0) (list (var2 ind1) 1)
                                                              (list (var1 ind2) 0) (list (var2 ind2) 1))  result))))))

(defun flat-list (list)
  #+:PW (pw::flat list)
  #+:OM (apply #'append (mapcar #'(lambda (x) (if (atom x) (list x) x)) list))  
  )

(defun equalize-fixed&indexes (fixed rindexes)
  (let ((l1 (length rindexes)) (l2 (length fixed)))
    (when (> l2 l1) (setq fixed (subseq fixed 0 (1- l1))))
    (append (mapcar #'list fixed)
            (make-list (max 0 (- l1 (1+ l2))) :initial-element (last fixed)))))

(defun build-hint-by-interp (reps rindexes rvoices &optional fixed)
  (let* ((lists 
          (if fixed
            (progn
              (setq reps (flat-list reps))
              (equalize-fixed&indexes (rest reps) rindexes))
            (get-interpolated-values (second reps) (third reps) (1- (length rindexes)) (or (fourth reps) 1)))
          )  res)
    (setq lists (nreverse lists))
    (setq rvoices
          (append (make-list (max 0 (- (length rindexes) (length rvoices))) :initial-element (first rvoices))
                  rvoices))
    (do () ((null rindexes) res)
      (setq res (append (build-int-hori-predicates (pop lists) (list (pop rvoices) (first rvoices))
                                                   (list (pop rindexes) (first rindexes)))
                        res)))
    res))

(defun voicenumberp (v)
  (or (numberp v) (my-string= v *upper-voice-name*) (my-string= v *lower-voice-name*)))

(defun horizontal-intervals (expression)
  "defines external object distance constraint" 
  (let (res  all-indexes indexes (form expression))
    (do ((reps) (alf)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (cond ((and (symbolp (first reps)) (string= (first reps) *interp-key*))
             (setf res (append (build-hint-by-interp reps indexes (list  *upper-voice-name*)) res)))
            ((and (symbolp (first reps)) (string= (first reps) *fixed-name*))
             (setf res (append (build-hint-by-interp reps indexes (list  *upper-voice-name*) t) res)))
            ((flatp reps) 
             (setf alf reps)
             (setf res (append (build-int-hori-predicates alf (list  *upper-voice-name*) indexes) res)))
            (t
             (do ((part reps) (voices) ) ((null part) res)
               (push (pop part) voices)
               (unless (voicenumberp (first part))
                 (cond ((my-string= (caar part) *interp-key*)
                        (setf res (append (build-hint-by-interp (pop part) indexes voices) res)))
                       ((my-string= (caar part) *fixed-name*)
                        (setf res (append (build-hint-by-interp (pop part) indexes voices t) res)))
                       (t (setf res (append (build-int-hori-predicates (pop part) voices indexes) res))))
                 (setq voices nil))))))
    (eval `(defun ,(gensym "Int-hori-constraint") () ',res) )))


#+:OM
(om::defmethod! om::hint ((exp list)) :initvals '((0_2 (i (2 3)) 7_10 (s (3 5)) )) 
  :indoc '("expression") :icon 207
:doc "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= S | ( <voice>1 S1 ...<voice>s Ss)      (1<=u<=k).
      S or St (1<=t<=s) = Set or list of integers.
If <Expression>u = S then differences between the highest and lowest notes of two contiguous 
chords (between Ciu Cju) should be in S. Else  <voice>t (1<=t<=s) can be 0,1,2 ...etc, 'u or 'l. 
0 and 'l correspond to first (or lowest) notes and 1 to second notes and so on. 'u corresponds 
to highest notes. Then, differences between notes correspondig to <voice>t of two contiguous 
chords (between Ciu Cju) should be in St.
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
   exp = (0 2 (i (2 3)) 7 10 (s (3 5)) )
"
(horizontal-intervals (fill-expression exp)))

#+:PW
(pw::defunp hint ((exp  pw::list (:value '(0_2 (i (2 3)) 7_10 (s (3 5)) )))
                         ) list
 "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= S | ( <voice>1 S1 ...<voice>s Ss)      (1<=u<=k).
      S or St (1<=t<=s) = Set or list of integers.
If <Expression>u = S then differences between the highest and lowest notes of two contiguous 
chords (between Ciu Cju) should be in S. Else  <voice>t (1<=t<=s) can be 0,1,2 ...etc, 'u or 'l. 
0 and 'l correspond to first (or lowest) notes and 1 to second notes and so on. 'u corresponds 
to highest notes. Then, differences between notes correspondig to <voice>t of two contiguous 
chords (between Ciu Cju) should be in St.
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
   exp = (0 2 (i (2 3)) 7 10 (s (3 5)) )
"
(horizontal-intervals (fill-expression exp)))

(defun hints-by-density (expression &optional vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal) 
  (let (res  all-indexes indexes (form expression ))
    (do ((reps) (alf) (fun1) (fun2) (fun3)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq fun1 NIL fun2 NIL fun3 nil)
      (when (flatp reps) (setq reps (list reps)))
      (if  (consp (first reps)) 
        (setf fun1 (append (build-hint-dens-predicate reps nil t nil cmapUpper) fun1))
        (do ((part reps (cddr part)) (voice) ) ((null part) res)
          (setf alf (second part) voice (first part))
          (when (flatp alf) (setq alf (list alf)))
          (cond ((and (numberp voice) (> voice 0))
                 (setf fun3 (append (build-hint-dens-predicate alf voice nil nil cmapInternal) fun3)))
                ((and (symbolp voice) (string= voice *upper-voice-name*))    ;(equal voice 'u)
                 (setf fun1 (append  (build-hint-dens-predicate alf nil t nil cmapUpper) fun1)))
                ((or (and (symbolp voice) (string= voice *lower-voice-name*)) (equal voice 0))
                 (setf fun2 (append  (build-hint-dens-predicate alf nil nil t cmapLower) fun2)) )
          )
        )
      )    
      (do ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)) (size) (len) (portion))
          ((or (null all-ind) (null ind)))
        (setq len (length all-ind))
        (dolist (pair fun1)
          (when (> len (rest pair))
            (setq size (1+ (rest pair)) portion (subseq all-ind 0 size))
            (dotimes (i (- size 2))
              (setf res (append (build-sequence-constraint (first pair)
                                                          (nthcdr i portion) ;;(subseq all-ind 0 size)
                                                           0 (1+ (rest pair)) #'var1 #'var2 0 vmapUpper) res)))  ))
        (dolist (pair fun2)
          (when (> len (rest pair))
            (setq size (1+ (rest pair)) portion (subseq all-ind 0 size))
            (dotimes (i (- size 2))
              (setf res (append (build-sequence-constraint (first pair)
                                                           (nthcdr i portion) ;;;(subseq all-ind 0 (1+ (rest pair)))
                                                           0 (1+ (rest pair)) #'var1 nil 0 vmapLower) res)))  ))
        (dolist (pair fun3)
          (when (> len (rest pair))
            (setq size (1+ (rest pair)) portion (subseq all-ind 0 size))
            (dotimes (i (- size 2))
              (setf res (append (build-sequence-constraint (first pair)
                                                           (nthcdr i portion) ;;;(subseq all-ind 0 (1+ (rest pair)))
                                                           0 (1+ (rest pair)) #'var1 #'var2 1 vmapInternal) res)))))  )
      )
    (if (or vmapLower vmapUpper vmapInternal) res
        (eval `(defun ,(gensym "x-dst_rnw-constraint") () ',res) )))
  )

(defun make-voice-repetition-function (voice pair choiceMap)
  (if choiceMap
    (eval `(function 
            (lambda (&rest chords)
              (let* ((ch2 (funcall ,choiceMap (first chords)))
                     (len2 (length ch2))
                     (note2 (nth (if (>= ,voice len2) len2 ,voice) ch2))
                     int ints note1 (reps 0))
                (dolist (ch (cdr chords))
                  (setq note1 note2 ch2 (funcall ,choiceMap ch))
                  (setq len2 (length ch2)
                        note2 (nth (if (>= ,voice len2) len2 ,voice) ch2))
                  (setq int (direction-invariance *distance-function* note2 note1))
                  (when (member int ints :test #'delayconstr::approx=) (incf reps))
                  (push int ints))
                (< reps ,(second pair))))))
    (eval `(function 
            (lambda (&rest chords)
              (let* ((ch2 (dx->x (first chords) (second chords)))
                     (len2 (length (second chords)))
                     (note2 (nth (if (> ,voice len2) len2 ,voice) ch2))
                     int ints note1 (reps 0))
                (do ((chs (cddr chords) (cddr chs))) ((null chs))
                  (setq note1 note2 ch2 (dx->x (first chs) (second chs)))
                  (setq len2 (length (second chs))
                        note2 (nth (if (> ,voice len2) len2 ,voice) ch2))
                  (setq int (direction-invariance *distance-function* note2 note1))
                  (when (member int ints :test #'delayconstr::approx=) (incf reps))
                  (push int ints))
                (< reps ,(second pair))))))))

(defun make-upper-repetition-function (pair choiceMap)
  (if choiceMap
    (eval `(function 
          (lambda (&rest chords)
            (let* ((note2 (funcall ,choiceMap (first chords)))
                   int ints note1 (reps 0))
              (dolist (ch (cdr chords))
                (setq note1 note2)
                (setq note2 (funcall ,choiceMap  ch))
                (setq int (direction-invariance *distance-function* note2 note1))
                (when (member int ints :test #'delayconstr::approx=) (incf reps))
                (push int ints))
              (< reps ,(second pair))))))
    (eval `(function 
            (lambda (&rest chords)
              (let* ((note2 ;;(+ (first chords) (second chords))
                      (funcall *integrate-distance-fun* (first chords) (second chords))
                      )
                     int ints note1 (reps 0))
                (do ((chs (cddr chords) (cddr chs))) ((null chs))
                  (setq note1 note2)
                  (setq note2 (funcall *integrate-distance-fun*  (first chs) (second chs)))
                  (setq int (direction-invariance *distance-function* note2 note1))
                  (when (member int ints :test #'delayconstr::approx=) (incf reps))
                  (push int ints))
                (< reps ,(second pair))))))))

(defun make-lower-repetition-function (pair choiceMap)
  (if choiceMap
    (eval `(function 
            (lambda (&rest chords)
              (let* ((note2 (funcall ,choiceMap (first chords)))
                     int ints note1 (reps 0))
                (dolist (ch (cdr chords))
                  (setq note1 note2 note2 (funcall ,choiceMap ch))
                  (setq int (direction-invariance *distance-function* note2 note1))
                  (when (member int ints :test #'delayconstr::approx=) (incf reps))
                  (push int ints))
                (< reps ,(second pair))))))
    (eval `(function 
            (lambda (&rest chords)
              (let* ((note2 (first chords))
                     int ints note1 (reps 0))
                (do ((chs (cdr chords) (cdr chs))) ((null chs))
                  (setq note1 note2 note2 (first chs))
                  (setq int (direction-invariance *distance-function* note2 note1))
                  (when (member int ints :test #'delayconstr::approx=) (incf reps))
                  (push int ints))
                (< reps ,(second pair))))))))

(defun build-hint-dens-predicate (exp &optional voice upper lower choiceMap)
  (let (res)
    (dolist (pair exp res)
      (cond (voice
             (push (cons (make-voice-repetition-function voice pair choiceMap) (first pair)) res))
            (upper
             (push (cons (make-upper-repetition-function pair choiceMap) (first pair)) res))
            (lower
             (push (cons (make-lower-repetition-function pair choiceMap)
                    (first pair)) res))))))

#+:OM
(om::defmethod! om::x-dst_rnw ((exp list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
 :initvals '((0_6 (0 (4 2)) 7_10 (u (5 1)) ) 1 nil nil nil nil nil nil) 
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
:doc "external distance renewal per sequence length. Specifies the number of equal distances allowed between selected points
in a given range of consecutive horizontal distances.
For instance, interpreting objects as chords, the expression (0_6 (0 (4 2)) 7_10 (u (3 1)) ) specifies that for the first seven chords, there
should be in the lower voice (i.e. '0') a maximum of 2 equal intervals in each subsequence of 4 consecutive
intervals, and that for chords going from the eighth up to the eleventh, there should be in the upper
voice (i.e. 'u') a maximum of 1 equal interval (i.e. no repetition) in each subsequence of 4 consecutive
intervals."
(if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (includeConstraint-Importance
     (hints-by-density (fill-expression exp)
                       (or vmapUpper #'single-variable-map) (or vmapLower #'single-variable-map)
                       (or vmapInternal #'single-variable-map) (or cmapUpper #'identity)
                       (or cmapLower #'identity) (or cmapInternal #'identity))
     c-imp)
    (set-importance-in-namedCnstr "x-dst_rnw" (fill-expression exp) #'hints-by-density c-imp)))

#+:OM
(om::defmethod! om::hintrnw/vl-spcf ((exp list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  :initvals '((0_6 (0 (4 2)) 7_10 (u (5 1)) ) 1 nil nil nil nil nil nil) 
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
:doc "kept for compatibility. See x-dst_rnw"
(om::x-dst_rnw exp c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal))


#+:PW
(pw::defunp hintrnw/vl-spcf ((exp  pw::list (:value '(0_6 (0 (4 2)) 7_10 (u (5 1)) )))
                         ) list
 "Horizontal interval renewal per voice length. Specifies the number of equal intervals allowed for a given voice in a given range of consecutive horizontal intervals
For instance, the expression (0_6 (0 (4 2)) 7_10 (u (3 1)) ) specifies that for the first seven chords, there
should be in the lower voice (i.e. '0') a maximum of 2 equal intervals in each subsequence of 4 consecutive
intervals, and that for chords going from the eighth up to the eleventh, there should be in the upper
voice (i.e. 'u') a maximum of 1 equal interval (i.e. no repetition) in each subsequence of 4 consecutive
intervals."
(hints-by-density (fill-expression exp)))

(defun build-horizontal-predicate (logexp fun &optional voice relevance-set)
  (if voice
    (if relevance-set
      (eval `(function 
              (lambda (&REST Chords)
                  (funcall ,fun ,voice ,(DELAYCONSTR::set-domain-constraint-fun logexp) ',relevance-set chords))))
      (eval `(function 
              (lambda (&REST Chords)
                (funcall ,fun ,voice ,(DELAYCONSTR::set-domain-constraint-fun logexp) chords)))))  
    (if relevance-set
      (eval `(function 
              (lambda (&REST Chords)
                (funcall ,fun ,(DELAYCONSTR::set-domain-constraint-fun logexp) ',relevance-set chords))) )
      (eval `(function 
              (lambda (&REST Chords)
                (funcall ,fun ,(DELAYCONSTR::set-domain-constraint-fun logexp) chords))))
      )))

(defun parse-horizontal-expression (expr function1 function2 function3 &optional relevance-set)
  (let (step? voice arity logexpr)
    (when  (flatp expr) (setq expr (list 'and expr)))
    (when (and (symbolp (first expr)) (string= (first expr) *step-name*))
      (setf step? t expr (cdr expr)))
    (setf voice (first expr))
    (cond
       ((and (not (numberp voice)) (not  (string= voice *upper-voice-name*))
             (not  (string= voice *lower-voice-name*)))
        (setf arity (Max-node expr #'Fmax-leaf 0))
        (values (list (build-horizontal-predicate expr function1 nil relevance-set) arity step?)
                (list (build-horizontal-predicate expr function2 nil relevance-set) arity step?) nil nil))
       ((and (numberp voice) (> voice 0))
        (setf logexpr (second expr) arity (Max-node logexpr #'Fmax-leaf 0))
        (values nil nil (list (build-horizontal-predicate logexpr function3 voice relevance-set) arity step?)
                (cddr expr)))
       ((and (symbolp voice) (string= voice *upper-voice-name*))
        (setf logexpr (second expr) arity (Max-node logexpr #'Fmax-leaf 0))
        (values (list (build-horizontal-predicate logexpr function1 nil relevance-set) arity step?)
                nil nil (cddr expr)))
       ((or (and (symbolp voice) (string= voice *lower-voice-name*))  (equal voice 0))
        (setf logexpr (second expr) arity (Max-node logexpr #'Fmax-leaf 0))
        (values nil (list (build-horizontal-predicate logexpr function2 nil  relevance-set) arity step?) 
                nil (cddr expr)))
       (t (error "wrong voice specification in constraint")))))

(defun push-constraint-into-list (all-indexes indexes pair add map1 &optional map2 level disjoint-step varMap)
  (let (res size f)
    (when (>= (length all-indexes) (setq size (+ (second pair) add)))
      (setq disjoint-step (if disjoint-step 0 1))
      (setq f (first pair))
      (if (third pair)
        (do ((ind (reverse indexes) (nthcdr (- size disjoint-step) ind))) ((< (length ind) size))
          (setf res (append (build-sequence-constraint f (nreverse (subseq ind 0 size)) 0 size map1 map2 level varMap) res)))
        (do  ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)))
             ((or (< (length all-ind) size) (null ind)))
          (setf res (append (build-sequence-constraint f (subseq all-ind 0 size) 0 size map1 map2 level varMap) res)))
        ))
   res))
  
(defun set-horizontal-logical-filter (expression function1 function2 function3 add name
                                                 &optional register-pred disjoint-step vmapUpper vmapLower vmapInternal)
  (let (res  all-indexes indexes register-set (form expression))
    (do ((reps) (fun1) (fun2) (fun3) ) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq fun1 NIL fun2 NIL fun3 NIL)
      (do ((subreg reps)) ((null subreg))
        (if register-pred
          (setf register-set (list (pop subreg) (pop subreg))
                reps (first subreg) subreg (cdr subreg))
          (setf subreg nil))
        (do ((part reps)) ((null part))
          (multiple-value-bind (f1 f2 f3 new-expr)
                               (parse-horizontal-expression part function1 function2 function3 register-set)
            (when f1 (push f1 fun1))
            (when f2 (push f2 fun2))
            (when f3 (push f3 fun3))
            (setf part new-expr)) )
        )
      (dolist (pair fun1)
        (setf res (append (push-constraint-into-list all-indexes indexes pair add #'var1 #'var2 nil disjoint-step vmapUpper)
                          res)))
      (dolist (pair fun2)
        (setf res (append (push-constraint-into-list all-indexes indexes pair add #'var1 nil nil disjoint-step vmapLower)
                          res)))
      (dolist (pair fun3)
        (setf res (append (push-constraint-into-list all-indexes indexes pair add #'var1 #'var2 1 disjoint-step vmapInternal)
                          res)))
      )
    (if (or vmapUpper vmapLower vmapInternal)
      res
      (eval `(defun ,(gensym name) () ',res)))
    )  )


(defun build-vars-for-voice (voices &optional cmapUpper cmapLower cmapInternal)
  (let ((var 0))
  (mapcar
   #'(lambda (rv1)
       (cond ((my-string= rv1 *upper-voice-name*)
              (if cmapUpper
                (list 0 (list (gensym (incf var))) rv1)
                (list 0 (list (gensym (incf var)) (gensym (incf var))) rv1)))
            ((or (my-string= rv1 *lower-voice-name*) (zerop rv1))
             (if cmapLower
               (list 0 (list (gensym (incf var))) rv1)
               (list 0 (list (gensym (incf var))) rv1)))
            ((numberp rv1)
             (if cmapInternal
               (list 1 (list (gensym (incf var))) rv1)
               (list 1 (list (gensym (incf var)) (gensym (incf var))) rv1)))))
   voices)))

;;(build-vars-for-voice '(l l l))

(defun build-partial-expr (vars voice &optional cmapUpper cmapLower cmapInternal)
  (cond ((or (my-string= voice *lower-voice-name*) (and (numberp voice) (zerop voice)))
          (if cmapLower
           `(,(first vars) (funcall ,cmapLower ,(first vars)))
           `(,(first vars) ,(first vars))))
        ((my-string= voice *upper-voice-name*)
         (if cmapUpper
           `(,(first vars) (funcall ,cmapUpper ,(first vars)))
           `(,(first vars) (funcall *integrate-distance-fun* ,(first vars) ,(second vars)))))
        (t
         (if cmapInternal
           `(,(first vars) (if (> ,voice (length (funcall ,cmapInternal ,(first vars))))
                            (first (last (cons (funcall ,cmapInternal ,(first vars)))))
                            (nth ,voice (funcall ,cmapInternal ,(first vars)))))
          `(,(first vars) (if (> ,voice (length ,(second vars)))
                            (reduce *integrate-distance-fun* (cons ,(first vars) ,(second vars)))
                           (nth ,voice (dx->x ,(first vars) ,(second vars)))))))))

#|
(defun build-hint-constraint-function (voices f &optional register-set)
  (let ((exp (build-vars-for-voice voices)))
    (values
    (eval
     `(function (lambda ,(flat-once (mapcar #'(lambda (form) (second form)) exp))
       (let  ,(mapcar #'(lambda (form1) (build-partial-expr (second form1) (third form1))) exp)
         ,(if register-set
            `(or ,@(append (flat-once (mapcar #'(lambda (form)
                                                  (list `(< ,(first (second form)) ,(first register-set))
                                                        `(> ,(first (second form)) ,(second register-set))))
                                              exp))
                           `((funcall ,f (list ,@(mapcar #'(lambda (form1 form2)
                                                             `(funcall *distance-function* ,(first (second form1)) ,(first (second form2))))
                                                         (cdr exp) exp))))))
            `(funcall ,f (list ,@(mapcar #'(lambda (form1 form2) `(funcall *distance-function* ,(first (second form1)) ,(first (second form2))))
                                        (cdr exp) exp)))))) )
     )
     exp)))
|#

(defun build-hint-constraint-function (voices f &optional register-set cmapUpper cmapLower cmapInternal)
    (let ((exp (build-vars-for-voice voices cmapUpper cmapLower cmapInternal)))
      (values
       (eval
        `(function 
          (lambda ,(flat-once (mapcar #'(lambda (form) (second form)) exp))
            (let  ,(mapcar #'(lambda (form1) (build-partial-expr (second form1) (third form1) cmapUpper cmapLower cmapInternal))
                           exp)
              ,(if register-set
                 `(or ,@(append (flat-once 
                                 (mapcar #'(lambda (form)
                                             (list `(< ,(first (second form)) ,(first register-set))
                                                   `(> ,(first (second form)) ,(second register-set))))
                                         exp))
                                `((funcall ,f 
                                           (list 
                                            ,@(mapcar #'(lambda (form1 form2)
                                                          `(funcall *distance-function*
                                                                    ,(first (second form1))
                                                                    ,(first (second form2)))) 
                                                      (cdr exp) exp))))))
                 `(funcall ,f (list 
                               ,@(mapcar 
                                  #'(lambda (form1 form2) 
                                        `(funcall *distance-function*
                                                  ,(first (second form1))
                                                  ,(first (second form2))))
                                  (cdr exp) exp)))))) )
              )
       exp)))
  
;;(build-hint-constraint-function '(4 u 2 0 1 u l 3) '+)
;;(build-hint-constraint-function '(4 u 2 0 1 u l 3) '+ '(60 72))
;;(build-vars-for-voice '(u u u))
(defun combine-indexes-voices (indexes voices logexp &optional register-set step?
                                       vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (let* ((f (DELAYCONSTR::set-domain-constraint-fun logexp))
         (long (1+ (delayconstr::get-max-patterns-length logexp)))
         (vl(length voices)) res)
    (setq voices (append voices (make-list (max 0 (- (length indexes) vl)) :initial-element (first (last voices)))))
    (do ((subvoices voices (nthcdr (if step? long 1) subvoices))
         (subindexes indexes (nthcdr (if step? long 1) subindexes)))
        ((< (length subindexes) long) res)
      (multiple-value-bind (exp vars)
                           (build-hint-constraint-function (subseq subvoices 0 long) f register-set
                                                          cmapUpper cmapLower cmapInternal)
        (push
         (cons exp
               (if (or vmapUpper vmapLower vmapInternal)
                 (flat-once
                  (mapcar #'(lambda (form index)
                              (let ((voice (third form)))
                                (cond ((or (my-string= voice *lower-voice-name*) (and (numberp voice) (zerop voice)))
                                       (funcall vmapLower index))
                                      ((my-string= voice *upper-voice-name*)
                                       (funcall vmapUpper index))
                                      (t (funcall vmapInternal index)))))
                          vars (subseq subindexes 0 long)))
               (flat-once
                (mapcar #'(lambda (form index)
                            (if (second (second form))
                              (list (list (var1 index) 0) (list (var2 index) (first form)))
                              (list (list (var1 index) 0))))
                        vars (subseq subindexes 0 long)))))
         res)))))

;;(combine-indexes-voices '(0 1 2 3 4 5 6 7 8) '(4 u 2 0 1 u l 3) '(and (7 7 ? ? 5)) nil)
;;(combine-indexes-voices '(0 1 2 3 4 5 6 7 8) '(0) '(and (7 7 ? ? 5)) nil t)
;;(funcall (caar (combine-indexes-voices '(0 1 2 3 4 5 6) '(u) '(and (3 5)) nil t)) 20 18 13 19)

(defun set-horizontal-filter (expression &optional register-pred name
                                         vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (let (res  all-indexes indexes register-set step? straight-indexes (form expression))
    (do ((reps)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form) straight-indexes (nreverse indexes))
      (do ((subreg reps)) ((null subreg))
        (setq step? nil)
        (if register-pred
          (setf register-set (list (pop subreg) (pop subreg))
                reps (first subreg) subreg (cdr subreg))
          (setf reps subreg subreg nil register-set nil))
        (when  (flatp reps) (setq reps (list *upper-voice-name* (list 'and reps))))
        (do ((part reps) (voices)) ((null part) res)
          (when (my-string= (first part) *step-name*)
            (setf step? t part (cdr part)))
          (push (pop part) voices)
          (unless (voicenumberp (first part))
                (setq res 
                      (append (combine-indexes-voices straight-indexes (nreverse voices) (pop part) register-set step?
                                                      vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
                              res))
                (setq voices nil)))))
    (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
      res
      (eval `(defun ,(gensym name) () ',res)))
    )  )

#|
(defun horizontal-filter (expression &optional vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (set-horizontal-filter expression nil "general-h-filter-constraint"
                           (or vmapUpper #'single-variable-map) (or vmapLower #'single-variable-map)
                           (or vmapInternal #'single-variable-map) (or cmapUpper #'identity)
                           (or cmapLower #'identity) (or cmapInternal #'identity))
    (set-horizontal-filter expression nil "hori-logi-filter-constraint")))
|#

(defun hint-filt-upper (f chords)
    (if (not (third chords)) t
        (let (res) 
          (do ((ch chords (cddr ch))) ((null (third ch)) res)
            (push (funcall *distance-function* 
                                        (funcall *integrate-distance-fun* (third ch) (fourth ch))
                                        (funcall *integrate-distance-fun* (first ch) (second ch)))
                  res))
          (funcall f (nreverse res)))))

(defun construct-general-hint-filt (choiceMap)
    (eval `(function (lambda (f chords)
                       (if (not (second chords)) t
                           (let (res) 
                             (do ((ch chords (cdr ch))) ((null (second ch)) res)
                               (push (funcall *distance-function* 
                                              (funcall ,choiceMap (first ch))
                                              (funcall ,choiceMap (second ch)))
                                     res))
                             (funcall f (nreverse res))))))))

(defun hint-filt-lower (f chords)
    (if (not (second chords)) t
        (let (res) 
          (do ((ch chords (cdr ch))) ((null (second ch)) res)
            (push (funcall *distance-function* (second ch) (first ch)) res))
          (funcall f (nreverse res)))))

(defun hint-filt-voice (voice f chords)
    (if (null (third chords)) t
        (let (res ch1 ch2 len1 len2)
          (do ((chs chords (cddr chs))) ((null (third chs)) res)
            (setq ch1 (dx->x (first chs) (second chs)) len1 (1- (length ch1))
                  ch2 (dx->x (third chs) (fourth chs)) len2 (1- (length ch2)))
            (push (funcall *distance-function*
                           (nth (if (> voice len2) len2 voice) ch2)
                          (nth (if (> voice len1) len1 voice) ch1))
                  res))
          (funcall f (nreverse res)))))

(defun construct-general-hint-filt-voice (choiceMap)
   (eval `(function (lambda (voice f chords)
                      (let (res len1 len2 ch1 ch2)
                        (do ((chs chords (cdr chs))) ((null (second chs)))
                          (setq ch1 (funcall ,choiceMap  (first chs)) len1 (1- (length ch1))
                                ch2 (funcall ,choiceMap  (second chs)) len2 (1- (length ch2)))
                          (push (funcall *distance-function*
                                         (nth (if (> voice len2) len2 voice) ch2)
                                         (nth (if (> voice len1) len1 voice) ch1))
                                res))
                        (funcall f (nreverse res)))))))

(defun horizontal-filter (expression &optional vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (set-horizontal-logical-filter expression
                                   (construct-general-hint-filt (or cmapUpper #'identity))
                                   (construct-general-hint-filt (or cmapLower #'identity))
                                   (construct-general-hint-filt-voice (or cmapInternal #'identity))
                                    1 "general-hint-logi-filter-constraint" nil 0
                                    (or vmapUpper #'single-variable-map) (or vmapLower #'single-variable-map)
                                    (or vmapInternal #'single-variable-map))
    (set-horizontal-logical-filter expression #'hint-filt-upper #'hint-filt-lower #'hint-filt-voice 1 
                                   "x-dst_filt-constraint" nil 0)))

#+:OM (om::defmethod! om::hori-logi-filter  ((exp  list)) (horizontal-filter (fill-expression exp)))
#+:OM
(om::defmethod! om::x-dst_filt  ((exp  list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
 :initvals '((0_6 (1 (not (4 4)) step u (and (3 5)))) 1 nil nil nil nil nil nil)
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
  :doc "External distances filtering. Constrains external distances between selected points in consecutive objects
to match a supplied pattern.
For example, if the objects are interpreted as chords, the expression (0_6 (step 3 (not (or (2 2) (4 4))) u (and (3 5)))) constraints the first seven 
chords so that voice number 1 (i.e. the second note, counting up from the base) should not form two consecutive
thirds, and so that the upper voice follows the pattern: (minor)third-fourth for chords 0,1,2 and then again
(minor)third-fourth for chords 2,3,4, etc. "
  (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (includeConstraint-Importance 
     (horizontal-filter (fill-expression exp) vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
     (or c-imp 1))
    (set-importance-in-namedCnstr "x-dst_filt" (fill-expression exp) #'horizontal-filter c-imp)))

#+:OM
(om::defmethod! om::hint-filt  ((exp  list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
 :initvals '((0_6 (1 (not (4 4)) step u (and (3 5)))) 1 nil nil nil nil nil nil)
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
  :doc "Kept for compatibility. See x-dst_filt"
  (om::x-dst_filt exp c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal))

#+:PW
(pw::defunp hint-filt  ((exp  pw::list 
                                  (:value '(0_6 (1 (not (4 4)) step u (and (3 5))))))
                                ) list 
"Horizontal intervals filtering. Constraints horizontal intervals of a given voice to match a supplied pattern
For example, the expression (0_6 (step 3 (not (or (2 2) (4 4))) u (and (3 5)))) constraints the first seven 
chords so that voice number 1 (i.e. the second note, counting up from the base) should not form two consecutive
thirds, and so that the upper voice follows the pattern: (minor)third-fourth for chords 0,1,2 and then again
(minor)third-fourth for chords 2,3,4, etc. "
  (horizontal-filter (fill-expression exp)) )


(defun horint-reg-filter1 (f register-set chords)
  (let ((Ns) (Vs) (from (first register-set))
        (to (second register-set)) )
      (or
       (do ((chs Chords (cddr chs))) ((null chs) nil)
        (push (+ (first chs) (second chs)) Ns)
        (unless (and (funcall #'delayconstr::approx>= (car Ns) from)
                     (funcall #'delayconstr::approx<= (car Ns) to)) (return t)))
      (progn ;;(setq Vs (x->dx-abs (nreverse Ns)))
             (setq Vs (x->dx (nreverse Ns)))
             (funcall f Vs)))))

(defun horint-reg-filter2 (f register-set chords)
  (let ((Ns) (Vs) (from (first register-set))
        (to (second register-set)) )
    (or
     (do ((chs Chords (cdr chs))) ((null chs) nil)
       (unless (and (funcall #'delayconstr::approx>= (car chs) from)
                    (funcall #'delayconstr::approx<= (car chs) to)) (return t))
       (push (first chs) Ns) )
     (progn ;;(setq Vs (x->dx-abs (nreverse Ns)))
       (setq Vs (x->dx (nreverse Ns)))
            (funcall f Vs) ))) )

(defun horint-reg-filter3 (voice f register-set chords)
  (let ((Ns) (Vs) len (from (first register-set))
        (to (second register-set) ))
    (or
     (do ((chs Chords (cddr chs)) (ch) ) ((null chs) nil)
      (setq ch (dx->x (first chs) (second chs)) len (1- (length ch)))
      (push (nth (if (> voice len) len voice) ch) Ns)
      (unless (and (funcall #'delayconstr::approx>= (car Ns) from)
                   (funcall #'delayconstr::approx<= (car Ns) to)) (return t))
      )
    (progn ;;(setq Vs (x->dx-abs (nreverse Ns)))
      (setq Vs (x->dx (nreverse Ns)))
           (funcall f Vs)) )
    ))

(defun set-hori-reg-logical-filter (expression &optional vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  (set-horizontal-filter expression t "x-dstreg_filt-constraint" vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal))

#+:OM
(om::defmethod! om::x-dst/reg_filt  ((exp  list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  :initvals '((0_6 (36 48 (u (not (or (2 2) (4 4)))))) 1 nil nil nil nil nil nil)
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "cmapUpper" "cmapLower" "cmapInternal") :icon 520
  :doc "Filtering of numbered point distances within a zone. Gives patterns that should hold for the horizontal distances
 between points falling within a given zone.
For example, interpreting objects aschords, the expression (0_6 (36 48 (u (not (or (2 2) (4 4)))))) constraints the first seven chords
so that in the upper voice, within the octave from 36 up to 48, there should be no sequence of two consecutive major
seconds or major thirds"
(if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (includeConstraint-Importance
     (set-hori-reg-logical-filter (fill-expression exp) (or vmapUpper #'single-variable-map)
                                  (or vmapLower #'single-variable-map) (or vmapInternal #'single-variable-map)
                                  (or cmapUpper #'identity) (or cmapLower #'identity) (or cmapInternal #'identity))
     c-imp)
    (set-importance-in-namedCnstr "x-dst/reg_filt" (fill-expression exp) #'set-hori-reg-logical-filter c-imp)))

#+:OM
(om::defmethod! om::hint/reg-filt  ((exp  list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  :initvals '((0_6 (36 48 (u (not (or (2 2) (4 4)))))) 1 nil nil nil nil nil nil)
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "cmapUpper" "cmapLower" "cmapInternal")
  :icon 520
  :doc "kept for compatibility. See x-dstreg_filt"
  (om::x-dst/reg_filt exp c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal))

#+:PW
(pw::defunp hint/reg-filt  ((exp  pw::list 
                                  (:value '(0_6 (36 48 (u (not (or (2 2) (4 4))))))))
                                ) list 
"Filtering of voice intervals within a register. Gives patterns that should hold for the horizontal intervals of a voice falling withion a given register.
For example, the expression (0_6 (36 48 (u (not (or (2 2) (4 4)))))) constraints the first seven chords
so that in the upper voice, within the octave from 36 up to 48, there should be no sequence of two consecutive major
seconds or major thirds"
  (set-hori-reg-logical-filter (fill-expression exp)) )


(defun filter-pass-band (expression &optional varMap choiceMap)
  "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= (<Note>i <Note>s <Logical Expresion>) (1<=u<=k).
Contraints according to <Logical Expression> the intervals obtained from notes between <Note>i <Note>s 
in chords between Ciu Cju.
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 (3 12 (not (or (* 2 *) (* 1 *)))))
"
  (let (res  all-indexes indexes (form expression ) (arity 1) (options (or varMap choiceMap)))
    (do ((reps) (logexp) (fun1)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq fun1 NIL)
      (when  (not (numberp (first reps))) (setq reps (list 0 127 reps)))
      (do ((part reps (cdddr part)) (low) (high)) ((null part) res)
        (setf logexp (third part) low (first part)
              high (second part))
        (push  (eval
                `(function
                  (lambda ,(if options '(object) '(base ints))
                    (let ((chord ,(if options `(funcall ,choiceMap object) `(dx->x base ints)))
                          res)
                      (dolist (note chord)
                        (when (and (delayconstr::approx>=  note ,low)
                                   (delayconstr::approx<=  note ,high))
                          (push note res)))
                      (if (> (length res) 1)
                        (funcall ,(DELAYCONSTR::set-domain-constraint-fun logexp)
                                 ;;(x->dx-abs (nreverse res))
                                 (x->dx (nreverse res))
                                 ) t)) )))
               fun1)
        )
      (dolist (f fun1)
          (do  ((ind indexes (cdr ind)) (all-ind all-indexes (cdr all-ind)))
               ((or (null all-ind) (null ind)))
            (setf res (append (build-sequence-constraint f (subseq all-ind 0 arity) 0 arity #'var1 #'var2 1 varMap) res))))
      )
    (if options res
        (eval `(defun ,(gensym "i-dstreg_filt") () ', res)))
    )
  )

#+:OM (om::defmethod! om::filt-pass-band   ((exp list)) (filter-pass-band (fill-expression exp)))
#+:OM
(om::defmethod! om::i-dst/reg_filt ((exp list) &optional c-imp varMap choiceMap)
  :initvals '((0_3 (48 60 (and (7 *) (not (or (* 2 *) (* 1 *)))))) 1 nil nil)
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map") 
  :icon 520
  :doc "Filtering of object distances in a zone. Gives patterns that should hold for the distances in an object
falling within a given zone.
For example, interpreting objects as chords, the expression (0_3 (48 60 (and (7 *) (not (or (* 2 *) (* 1 *)))))) constraints the first four chords
so that within the octave from 48 up to 60, their lower interval should be a fifth and there should be no
major or minor second"
  (if (or varMap choiceMap)
    (includeConstraint-Importance
     (filter-pass-band (fill-expression exp) (or varMap #'single-variable-map) (or choiceMap #'identity))
     c-imp)
    (set-importance-in-namedCnstr "i-dst/reg_filt" (fill-expression exp) #'filter-pass-band c-imp)))

#+:OM
(om::defmethod! om::vint/reg-filt ((exp list) &optional c-imp varMap choiceMap)
  :initvals '((0_3 (48 60 (and (7 *) (not (or (* 2 *) (* 1 *)))))) 1 nil nil)
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map") 
  :icon 520
  :doc "Kept for compatibility. See i-dstreg_filt"
  (om::i-dst/reg_filt exp c-imp varMap choiceMap))

#+:PW
(pw::defunp vint/reg-filt   ((exp  pw::list (:value '(0_3 (48 60 (and (7 *) (not (or (* 2 *) (* 1 *))))))))
                                ) list
 "Filtering of chord intervals in a register. Gives patterns that should hold for the intervals in a chord falling within a given register.
For example, the expression (0_3 (48 60 (and (7 *) (not (or (* 2 *) (* 1 *)))))) constraints the first four chords
so that within the octave from 48 up to 60, their lower interval should be a fifth and there should be no
major or minor second"
  (filter-pass-band (fill-expression exp)) )

;;; (setf foo (caar (funcall (filt-pass-band '( 5_8 (3 12 (not (or (* 2 *) (* 1 *))))) )) )))

(defun chord-filt1 (f chords)
  (let (res)
    (do ((ch chords (cddr ch))) ((null ch) res)
      (push (funcall *integrate-distance-fun* (first ch) (second ch)) res))
    (funcall f (nreverse res))))

(defun construct-general-chord-filt (choiceMap)
  (if (equal choiceMap #'identity)
    #'(lambda (f chords) (funcall f chords))
    (eval `(function (lambda (f chords)
                     (funcall f (mapcar ,choiceMap chords)))))))

#| ;;luces perdidas! oula!
(defun chord-filt2 (f chords)
  (let (res)
    (dolist (ch chords)
      (push ch res))
    (funcall f (nreverse res))))
|#

(defun chord-filt2 (f chords)
 (funcall f chords))

(defun chord-filt3 (voice f chords)
  (let (res ch len)
    (do ((chs chords (cddr chs))) ((null chs) res)
      (setq ch (dx->x (first chs) (second chs)) len (1- (length ch)))
      (push (nth (if (> voice len) len voice) ch)  res))
    (funcall f (nreverse res))))

(defun construct-general-chord-filt-voice (choiceMap)
   (eval `(function (lambda (voice f chords)
                      (let (res len)
                        (dolist (ch chords)
                          (setq ch (funcall ,choiceMap  ch) len (1- (length ch)))
                          (push (nth (if (> voice len) len voice) ch)  res))
                        (funcall f (nreverse res)))))))

(defun chord-logical-filter (expression &optional vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
  " "
  (if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (set-horizontal-logical-filter expression
                                   (construct-general-chord-filt (or cmapUpper #'identity))
                                   (construct-general-chord-filt (or cmapLower #'identity))
                                   (construct-general-chord-filt-voice (or cmapInternal #'identity))
                                    0 "general-chord-logi-filter-constraint" nil 0
                                    (or vmapUpper #'single-variable-map) (or vmapLower #'single-variable-map)
                                    (or vmapInternal #'single-variable-map))
     (set-horizontal-logical-filter expression #'chord-filt1 #'chord-filt2 #'chord-filt3
                                    0 "x-pts_filt-constraint" nil 0)))

#+:OM (om::defmethod! om::chord-logi-filter   ((exp list)) (chord-logical-filter (fill-expression exp)  ))
#+:OM
(om::defmethod! om::x-pts_filt   ((exp list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal) 
  :initvals '((0_5 (u (not (or (60 ? 65) (72 77 72))))) 1 nil nil nil nil nil nil)
  :indoc '("expression" "cnstr importance" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
  :doc "Filtering sequences of selected points in objects. Constrains a given points in consecutive objects to match
 a given pattern.
For example, interpreting objects as chords, the expression (0_5 (u (not (or (60 ? 65) (72 77 72))))) constraints the
first six chords so that the upper voice never contains pitches 60 and 65 
separated by some other pitch, neither the sequence of pitches 72 77 72"
(if (or vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal)
    (includeConstraint-Importance
     (chord-logical-filter (fill-expression exp) vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal )
     c-imp)
    (set-importance-in-namedCnstr "x-pts_filt" (fill-expression exp) #'chord-logical-filter c-imp)))

#+:OM
(om::defmethod! om::pitch/v-filt   ((exp list) &optional c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal) 
  :initvals '((0_5 (u (not (or (60 ? 65) (72 77 72))))) nil nil nil nil nil nil)
  :indoc '("expression" "vmapUpper" "vmapLower" "vmapInternal" "choice mapUpper" "choice mapLower" "choice mapInternal")
  :icon 520
  :doc "kept for compatibility. See x-pts_filt"
(om::x-pts_filt exp c-imp vmapUpper vmapLower vmapInternal cmapUpper cmapLower cmapInternal))

#+:PW
(pw::defunp pitch/v-filt   ((exp  pw::list (:value '(0_5 (u (not (or (60 ? 65) (72 77 72)))))))
                                ) list
"Filtering voice pitches. Constraints a given voice's pitches to match a given pattern.
For example, the expression (0_5 (u (not (or (60 ? 65) (72 77 72))))) constraints the
first six chords so that the upper voice never contains pitches 60 and 65 
separated by some other pitch, neither the sequence of pitches 72 77 72"
(chord-logical-filter (fill-expression exp)  ))

(defun build-mapped-sequence-constraint (f indexes varMap) 
  (let (res)
    (dolist (ind indexes res)
      (push `(,f ,@(funcall varMap ind)) res)))
)

(defun invent-variables (lista)
  (let (res)
    (dotimes (i (length lista) res) (push (gensym) res))))

(defun set-pitch-logical-filter (expression &optional varMap choiceMap)
  (let (res  all-indexes indexes f (form expression))
    (do ((reps) ) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))
      (if (not (or varMap choiceMap))
        (progn
          (setq f 
              (eval `(function 
                      (lambda (base Ints)
                        (funcall ,(DELAYCONSTR::set-domain-constraint-fun reps)
                                 #+:OM (dx->x base ints)
                                 #+:PW (dx->x base ints))))))
          (setf res (append (build-sequence-constraint  f indexes 0 1 #'var1 #'var2 1) res)))
        (let* ((varMapfunction (or varMap #'single-variable-Map))
               (internal-vars (invent-variables (funcall varMapfunction 1))))
          (setq f 
              (eval `(function 
                      (lambda ,internal-vars
                        (funcall ,(DELAYCONSTR::set-domain-constraint-fun reps)
                                 (funcall ,(or choiceMap #'identity) ,@internal-vars))))))
          (setf res (append (build-mapped-sequence-constraint  f indexes varMapfunction) res))))
       )
    (if (not (or varMap choiceMap))
      (eval `(defun ,(gensym "pts_filt-constraint") () ', res))
      res)
    )  )

;;(set-pitch-logical-filter (expand-lst '(0_6 (not (* 55 *)))) nil #'identity)

#+:OM
(om::defmethod! om::pts_filt   ((exp list) &optional c-imp varMap choiceMap) 
 :initvals '((0 2 7 (not (or (? 60 *) (* 62 64 *)))) 1 nil nil)
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map")
  :icon 520
  :doc "constructs a filter for an object (list of points) with the
boolean pattern 'expr'. 'varMap' is a unary function mapping
variable indexes to internal index+level. 'choiceMap' is a unary
function that is applied to the instance before invoking the constraint. Constraints object 
(e.g. chords) values to match a supplied pattern.
For example, interpreting objects as chords, the expression (0 2 7 (and (or (? 60 *) (* 62 64 *)))) says that object numbers 0 2 and 7
should each contain either a 60 (a C, thus) as the second note or a 62 followed by a 64 (D followed by a E)
possibly together with some other notes."
(if (or varMap choiceMap)
  (includeConstraint-Importance (set-pitch-logical-filter (fill-expression exp) varMap choiceMap ) (or c-imp 1))
  (set-importance-in-namedCnstr "pts_filt" (fill-expression exp) #'set-pitch-logical-filter c-imp)
))

#+:OM
(om::defmethod! om::pitch/ch-filt   ((exp list) &optional c-imp varMap choiceMap) 
  :initvals '((0 2 7 (not (or (? 60 *) (* 62 64 *)))) 1 nil nil)
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map")
  :icon 520
  :doc "kept for compatibility. See pts_filt"
(om::pts_filt exp c-imp varMap choiceMap))


#+:PW
(pw::defunp pitch/ch-filt   ((exp  pw::list (:value '(0 2 7 (not (or (? 60 *) (* 62 64 *))))))
                                ) list
"Filtering of pitches in a chord. Constraints chord's pitches to match a supplied pattern.
For example, the expression (0 2 7 (and (or (? 60 *) (* 62 64 *)))) says that chord numbers 0 2 and 7
should each contain either a 60 (a C, thus) as the second note or a 62 followed by a 64 (D followed by a E)
possibly together with some other notes"
(set-pitch-logical-filter (fill-expression exp)  ))

;;;;; (setf foo (caar (funcall (chord-logi-filter  '(0 3 (and (60 61 63))) )) ))
;;; (funcall foo 50 '(23 ))

(defun set-chord-length (exp)
  "exp ::= ( Ci1 Cj1 <Logical Expression>1 ... Cik Cjk <Logical Expression>k ). 
where <Expression>u ::= ( length1 length2 ...) (1<=u<=k).
The Length of each chord between Ciu Cju should have in ( length1 length2 ...) .
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 ( 2 3 ) 6 7 ( 3 5 ))
"
  (let ((htable+ (form-unary-expression exp)) indexes res)
    (setq indexes (sort (hash-table+-keys HTable+) '< ))
    (dolist (ind indexes)
      (push (list 
             (eval
              `(function
                (lambda (ints) 
                  (member (1+ (length ints)) ',(first (gethash+ ind Htable+)) ) )))
             (list (var2 ind) 1))
            res))
    (eval `(defun ,(gensym "chord-length-constraint") () ',res)))
  )

#+:OM
(om::defmethod! om::chord-length   ((exp list))
  (set-chord-length (fill-expression exp))  )
#+:OM
(om::defmethod! om::dens-filt   ((exp list)) :initvals '((0_3 ( 2 3 ))) 
  :indoc '("expression") :icon 207
  :doc "exp ::= ( Ci1 Cj1 <Logical Expression>1 ... Cik Cjk <Logical Expression>k ). 
where <Expression>u ::= ( length1 length2 ...) (1<=u<=k).
The Length of each chord between Ciu Cju should have in ( length1 length2 ...) .
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 ( 2 3 ) 6 7 ( 3 5 ))
"
(set-chord-length (fill-expression exp))  )

#+:PW
(pw::defunp dens-filt   ((exp  pw::list (:value '(0_3 ( 2 3 ))))
                                ) list"exp ::= ( Ci1 Cj1 <Logical Expression>1 ... Cik Cjk <Logical Expression>k ). 
where <Expression>u ::= ( length1 length2 ...) (1<=u<=k).
The Length of each chord between Ciu Cju should have in ( length1 length2 ...) .
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 ( 2 3 ) 6 7 ( 3 5 ))
"
(set-chord-length (fill-expression exp))  )


;;; (setf foo (caar (funcall (chord-length  '(0 3 ( 4 3 )) ))) )
;;; (funcall foo '(23 ))

(defun fixed-chords-data-Base (expression)
  "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= ( Chord1 Chord2 ....) (1<=u<=k).
Each chord between Ciu Cju should be in ( Chord1 Chord2 ....).
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 ((60 65 80) (70 72)))
"
  (let (res indexes (form expression ))
    (do ((reps) (fun1)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (pop form) indexes))
      (setq reps (pop form))
      (when (flatp reps) (setq reps (list reps)))
      (cond  ((and (symbolp (first reps)) (string= (first reps) *fixed-name*))
              (setq fun1  (eval
                           `(function
                             (lambda (base ints) 
                               (let ((chord (dx->x base ints)))
                                 (member chord ',(rest reps) :test #'equal ) ) )))))
             ((consp (first reps))
              (setq fun1 (eval
                          `(function (lambda (base ints)
                                       (let ((chord (dx->x base ints))
                                             (len1 (1+ (length ints)) ))
                                         (dolist (ch ',reps nil)
                                           (when (= (length (intersection chord ch))
                                                    (min len1 (length ch)))
                                             (return t)))))))))
             (t (error "wrong expression in constraint")))  
     (setf res (append (build-sequence-constraint fun1 indexes 0 1 #'var1 #'var2 1) res))
      )
    (eval `(defun ,(gensym "fixed-chords-constraint") () ',res))
    )  )

#+:OM (om::defmethod! om::given-chords   ((exp list)) (fixed-chords-data-Base (fill-expression exp)))
#+:OM
(om::defmethod! om::pitch/ch-sel   ((exp list))
   :initvals '((0_3 ((60 65 80) (70 72)))) 
   :indoc '("expression") :icon 207
   :doc 
"exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= ( Chord1 Chord2 ....) (1<=u<=k).
Each chord between Ciu Cju should be in ( Chord1 Chord2 ....).
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 ((60 65 80) (70 72)))
"
  (fixed-chords-data-Base (fill-expression exp)))

#+:PW
(pw::defunp pitch/ch-sel   ((exp  pw::list (:value '(0_3 ((60 65 80) (70 72)))))
                                ) list
"exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= ( Chord1 Chord2 ....) (1<=u<=k).
Each chord between Ciu Cju should be in ( Chord1 Chord2 ....).
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 ((60 65 80) (70 72)))
"
  (fixed-chords-data-Base (fill-expression exp)))

;;for compatibility with PW version
(defun fixed-chords (exp) (fixed-chords-data-Base exp))

;;; (setf foo (caar (funcall (fixed-chords '(0 3 ((60 65 80) (70 72)))) )) )
;;; (funcall foo 60 '(5 15))
;; (x->dx '(60 65 80))

(defun def-Ambitus (exp)
  "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= (iu su) (1<=u<=k).
The highest and lowest note of each chord between Ciu Cju shouldn't be
greater than iu and lower than su respectively .
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 (60 120) 5 10 (50 100))
"
  (let ((htable+ (form-unary-expression exp)) indexes res set low high)
    (setq indexes (hash-table+-keys HTable+))
    (dolist (ind indexes)
      (setq set (first (gethash+ ind Htable+)) low (apply #'min set) high (apply #'max set))
      (push (list 
             (eval`(function
                    (lambda (lower-note) 
                      (delayconstr::approx>= lower-note ,low))))
             (list (var1 ind) 0))
            res)
      (push (list 
             (eval`(function
                    (lambda (lower-note desp) 
                      (delayconstr::approx<= (funcall *integrate-distance-fun* desp lower-note) ,high))))
             (list (var1 ind) 0) (list (var2 ind) 0))
            res))
    (eval `(defun ,(gensym "ambitus-constraint") () ',res))
    ) )


#+:OM (om::defmethod! om::ambitus ((exp  list)) (def-Ambitus (fill-expression exp) ))
#+:OM
(om::defmethod! om::amb-prof ((exp  list)) :initvals '((0_3 (60 120) 5_10 (50 100)))
  :indoc '("expression") :icon 207
  :doc "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= (iu su) (1<=u<=k).
The highest and lowest note of each chord between Ciu Cju shouldn't be
greater than iu and lower than su respectively .
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 (60 120) 5 10 (50 100))
"
(def-Ambitus (fill-expression exp) ))

#+:PW
(pw::defunp amb-prof   ((exp  pw::list (:value '(0_3 (60 120) 5_10 (50 100))))
                                ) list 
"exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= (iu su) (1<=u<=k).
The highest and lowest note of each chord between Ciu Cju shouldn't be
greater than iu and lower than su respectively .
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example:
  exp = (0 3 (60 120) 5 10 (50 100))
"
(def-Ambitus (fill-expression exp) ))

(defun notes-by-density (expression &optional variableMap choiceMap)
  "exp ::= ( Ci1 Cj1  <Expression>1 ... Cik Cjk  <Expression>k ).
where <Expression>u ::= ((#N1 #R1)...(#Nr #Rr)) (for all u;1<=u<=k).
Every chords between Ciu Cju having #Ns should have at most #Rs 
ocurrences of each interval.  (for all s; 1<=s<=r).
<Expression> is a shorthand for (0 MaxChords <Expression>).
See MaxChords box in the menu UserLib/Music Engine.
Example :
 exp = (0 3 ( (4 3) ( 5 4) ) 5 8 ( (5 4) ))
"  
  (let (res indexes (form expression ))
    (do ((reps) (fun1)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (pop form) indexes))
      (setq reps (pop form))
      (setq fun1 
            (eval `(function (lambda ( Chord )
                               ,(if choiceMap `(setq Chord (x->dx (funcall ,choiceMap Chord))) nil)
                               (let* ((out t) 
                                      (num-notas
                                       ,(if choiceMap `(length Chord) `(1+ (length Chord))))
                                      (const-ocurr  (second (assoc num-notas ',reps))))
                                 (when const-ocurr
                                   (dolist (x (borrar-repetidos Chord #'delayconstr::approx=))
                                     (when (> (num-ocurrences x Chord #'delayconstr::approx=) const-ocurr)
                                       (return (setf out NIL))))
                                   )
                                 out )
                               )
                             )))  
     (setf res (append (build-sequence-constraint fun1 indexes 0 1 nil #'var2 1 variableMap) res))
      )
    (if (or choiceMap variableMap) res
        (eval `(defun  ,(gensym "i-dst_rnw-constraint") () ', res)))
    )
  )

#+:OM (om::defmethod! om::rep-par-dens   ((exp list)) (notes-by-density (fill-expression exp)))
#+:OM
(om::defmethod! om::i-dst_rnw   ((exp list) &optional c-imp variableMap choiceMap)
   :initvals '((0_6 ( (4 2) ( 5 3) ) 7_10 ( (5 1) )) 1 nil nil)
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map")
  :icon 520
  :doc "internal distance renewal. Specifies the number of equal distances allowed in an object of a given number
of points.
For instance, the expression (0_6 ( (4 2) ( 5 3) ) 7_10 ( (5 1) )) specifies that for the first seven objects, there
should be a maximum of 2 equal intervals in each object having 4 points and a maximum of 3 equal distances in each
object having 5 points For the following four objects, objects having 5 points should have no repeated distance
(i.e. 1 equal distance)."
(if (or variableMap choiceMap)
  (includeConstraint-Importance (notes-by-density (fill-expression exp)
                    (or variableMap #'single-variable-map) (or choiceMap #'identity)) (or c-imp 1))
  
  (set-importance-in-namedCnstr "i-dst_rnw" (fill-expression exp) #'notes-by-density c-imp)
;(notes-by-density (fill-expression exp))
))

#+:OM
(om::defmethod! om::vintrnw/dens-spcf   ((exp list) &optional c-imp variableMap choiceMap)
   :initvals '((0_6 ( (4 2) ( 5 3) ) 7_10 ( (5 1) )) 1 nil nil)
  :indoc '("expression" "cnstr importance" "variable Map" "choice Map")
  :icon 520
  :doc "Kept for compatibility. See i-dst_rnw"
(om::i-dst_rnw exp c-imp variableMap choiceMap))

#+:PW
(pw::defunp vintrnw/dens-spcf   ((exp  pw::list (:value '(0_6 ( (4 2) ( 5 3) ) 7_10 ( (5 1) ))))
                                ) list
 "vertical (i.e. chord) interval renewal per density. Specifies the number of equal intervals allowed in a chord of a given density.
For instance, the expression (0_6 ( (4 2) ( 5 3) ) 7_10 ( (5 1) )) specifies that for the first seven chords, there
should be a maximum of 2 equal intervals in each chord having 4 notes and a maximum of 3 equal intervals in each
chord having 5 notes. For the following four chords, chords having 5 notes should have no repeated interval
(i.e. 1 equal interval)."
(notes-by-density (fill-expression exp)))

(defun homogeneidad (exp)
  "exp ::= ( Ci1 Cj1  <Set>1 ... Cik Cjk  <Set>k ).
The difference between Maximal and Minimal interval in chords between Ciu and Cju (for all
u;1<=u<=k) should be in the set (or list) <Set>u. 
<Set> is a shorthand for (0 MaxChords <Set>).
See MaxChords box in the menu UserLib/Music Engine.
 Example:
   exp = (0 3 (0 1 2 3 4 5 6 7 8 9 10 11 12) 8 10 
              (0 1 2 4 5 6 7 8 9 10 11 12 13 14 15))
" 
  (cond  ((flatp exp) (setf exp (list 0 fvtempos::MaxChords exp))))

  (let (res)
    (do ((form exp (cdddr form))  (from) (to) (reps) (fun)) ((null form) res)
      (setq from  (car form) to  (second form) reps (third form)
            )
      (setf fun (eval `(function (lambda ( Chord )
                                  (if Chord
                                   (member (- (Maximal-Element Chord (function >))
                                              (Maximal-Element Chord (function <)))
                                           ',reps :test (function =)) t)
                                   )
                                 )))
      (do ((x from (1+ x))) ((> x to) res)
        (push (list fun (list (Var2 x) 1)) res)
        )
      )
     (eval `(defun ,(gensym "homogeneidad-constraint") () ',res))
    )
  )


(defmethod homogenity  ((exp  list))
 "exp ::= ( Ci1 Cj1  <Set>1 ... Cik Cjk  <Set>k ).
The difference between Maximal and Minimal interval in chords between Ciu and Cju (for all
u;1<=u<=k) should be in the set (or list) <Set>u. 
<Set> is a shorthand for (0 MaxChords <Set>).
See MaxChords box in the menu UserLib/Music Engine.
 Example:
   exp = (0 3 (0 1 2 3 4 5 6 7 8 9 10 11 12) 8 10 
              (0 1 2 4 5 6 7 8 9 10 11 12 13 14 15))
"
(homogeneidad exp)
)

;;; (setf foo (caar (funcall (homogeneidad '(0 3 (0 1 2 3 4 5 6 7 8 9 10 11 12)  8 10 (0 1 2 4 5 6 7 8 9 10 11 12 13 14 15)) )) ))
;;; (funcall foo  '(3 4 5 7))
;;; (homogeneidad '(0 1 2 3 4 5 6 7 8 9 10 11

(defun make-fixed-constraint (Const Ls &AUX  Gout)
  (cond ((flatp Ls) (setf Ls (list Ls)))
        ((numberp Ls) (setf Ls (list (list Ls)))))
  (let ((f (eval `(function (lambda (&REST Chords)
                              (let ((Ns) (Vs) )
                                (do ((chs Chords (cddr chs))) ((null chs))
                                  (push (dx->x (first chs) (second chs)) Ns)
                                  )
                                (setq Vs (nreverse Ns))
                                (apply ,Const Vs)
                                )
                              )))))
    (dolist (L Ls GouT)
      (let (larg)
        (dolist (x (reverse L))
          (setf Larg (append  (list (list (Var1 x) 0) (list (Var2 x) 1)) Larg))
          )
        (push (cons f Larg) Gout)
        )
      )
    )
  )

;;(defun foo ( x) (= (car x) 70))
;;(funcall (caar (anyconstraint (function foo) 0 '2)) 70 '(68 30))

(defun Make-Interv-Constraint (Const L )
  (cond ((flatp L) (setf L (list L)))
        ((null L) (setf L (list (list 0 fvtempos::MaxChords)))))
  (let (f )
    (setq f (eval `(function (lambda (&REST Chords)
                               (let ((Ns) (Vs) )
                                 (do ((chs Chords (cddr chs))) ((null chs))
                                   (push (dx->x (first chs) (second chs)) Ns)
                                   )
                                 (setq Vs (nreverse Ns))
                                 (apply ,Const Vs)
                                 )
                               ))))
    (multiple-value-bind (a1 a2 a3) (function-args (om::closure-function Const ))
      (do ((form L (cdr form))  (from) (to) (tup) (Arity) (Gout)) ((null form) Gout)
        (setq tup (car form) from  (first tup) to  (second tup)  
              )
        (if a3 
          (progn (setf Arity (third tup))
                 (unless Arity (setf Arity (1+ (- to from))))
                 )
          (setf Arity (+ a1 a2))
          ) 
        (when (and  (> Arity 0) (>= Arity (+ a1 a2)))
          (decf arity)
          (do ((x from (incf x)) ) ((> x to) )
            (when (>= (- x arity) 0)
              (push (cons f (all-lists-2 (var1 (- x  arity))  (var2 x) 1 0 1)) Gout)
              )
            )
          )
        )
      )
    )
  )

;;; (defun foo (x y))
;;;(make-interv-constraint  #'foo '())

(defun Make-index-Constraint (Const L )
  (cond ((flatp L) (setf L (list L)))
        ((null L) (setf L (list (list 0 fvtempos::MaxChords)))))
  (let (f )
    (multiple-value-bind (a1 a2 a3) (function-args (om::closure-function Const ))
      (do ((form L (cdr form))  (from) (to) (tup) (Arity) (Gout)) ((null form) Gout)
        (setq tup (car form) from  (first tup) to  (second tup)  
              )
        (if a3 
          (progn (setf Arity (third tup))
                 (unless Arity (setf Arity (1+ (- to from))))
                 )
          (setf Arity (+ a1 a2))
          )
        (when (and  (> Arity 0) (>= Arity (+ a1 a2)))
          (decf arity)
          (do ((x from (incf x)) ) ((> x to) )
            (when (>= (- x arity) 0)
              (setq f (eval `(function (lambda (&REST Chords)
                                         (let ((Ns) (Vs) )
                                           (do ((chs Chords (cddr chs))) ((null chs))
                                             (push (dx->x (first chs) (second chs)) Ns)
                                             )
                                           (setq Vs (nreverse Ns))
                                           (setf fvtempos::vari ,(1+ x))
                                           (apply ,Const Vs)
                                           )
                                         ))))
              (push (cons f (all-lists-2 (var1 (- x  arity))  (var2 x) 1 0 1)) Gout)
              )
            )
          )
        )
      )
    )
  )
;;; (defun foo (x y))
;;;(make-index-constraint  #'foo '( 2 10))

(defun Make-all-Constraint (Const L )
  (cond ((flatp L) (setf L (list L)))
        ((null L) (setf L (list (list 0 fvtempos::MaxChords)))))
  (let (f )
    (setq f (eval `(function (lambda (&REST Chords)
                               (let ((Ns) (Vs) )
                                 (do ((chs Chords (cddr chs))) ((null chs))
                                   (push (dx->x (first chs) (second chs)) Ns)
                                   )
                                 (setq Vs (nreverse Ns))
                                 (apply ,Const Vs)
                                 )
                               ))))
    (multiple-value-bind (a1 a2 a3) (function-args (om::closure-function Const ))
      (do ((form L (cdr form))  (from) (to) (tup) (Arity) (Gout)) ((null form) Gout)
        (setq tup (car form) from  (first tup) to  (second tup)  
              )
        (if a3 
          (progn (setf Arity (third tup))
                 (unless Arity (setf Arity (1+ (- to from))))
                 )
          (setf Arity (+ a1 a2))
          ) 
        (when (and (<= Arity (1+ (- to from))) (> Arity 0) (>= Arity (+ a1 a2)))
          (dolist  (Lc (all_combin_> (Interv-lista from to) (1- Arity)))
            (let (Larg)
              (dolist (x (reverse Lc))
                (setf Larg (append  (list (list (Var1 x) 0) (list (Var2 x) 1)) Larg))
                )
              (push (cons f Larg) Gout)
              )
            )
          )
        )
      )
    )
  )
;;; (defun foo (x y))
;;; (make-all-constraint   #'foo '())

(defun UserConstraint (const expression &optional varMap choiceMap) 
  (let (res  step all-indexes indexes arity (form expression))
    (setq arity (function-args (om::closure-function Const)))
    (do ((reps)) ((null form) res)
      (setq indexes nil)
      (do () ((or (null form) (consp (first form))))
        (push (car form) all-indexes)
        (push (pop form) indexes))
      (setq reps (pop form))      
      (cond ((string= (car reps) 's)                 ;;;sequence constraint
             (when (numberp (second reps)) (setq step (second reps) reps (cdr reps)))
             (setf res 
                   (append (Make-sequence-Constraint Const arity (second reps) indexes step varMap choiceMap) res)))
            ((string= (car reps) *fixed-name*)                 ;;;fixed indexes constraint
             (setf res (append (Make-fix-Constraint Const arity (second reps) indexes varMap choiceMap) res)))
            ((string= (car reps) 'a)                 ;;;all combinations constraint
             (setf res (append (Make-allcomb-Constraint Const arity (second reps) indexes varMap choiceMap) res))))
      )
    (if (or varMap choiceMap) res
        (eval `(defun ,(gensym "UserConstraint")  () ', res)) ))
  )

(defun Make-sequence-Constraint (Const arity vars? indexes &optional step varMap choiceMap)
  (let (res size)
    (unless step (setq step 1))
    (if (and (symbolp vars?) (string= vars? 'i))
      (progn (setq size (floor arity 2))
             (do  ((ind indexes (nthcdr step ind)))
                  ((or (< (length ind) size) (null ind)) res)
               (push (set-up-predicate Const (subseq ind 0 size) t varMap choiceMap) res)))
      (do  ((ind indexes (nthcdr step ind)))
           ((or (< (length ind) arity) (null ind)) res)
        (push (set-up-predicate Const (subseq ind 0 arity) nil varMap choiceMap) res)))))

(defun set-up-predicate (Const vars &optional include? varMap choiceMap)
  (let (res )
    (if (or varMap choiceMap)
      (cons (eval `(function (lambda (&REST Chords)
                               (let ((Ns)  )
                                 (dolist (ch Chords)
                                   (push (funcall ,choiceMap ch) Ns) )
                                 (apply ,Const ,@ (when include? (reverse vars)) (nreverse Ns))))))
            (dolist (i vars (flat-once res))
              (push (funcall varMap i) res)))
      (cons (eval `(function (lambda (&REST Chords)
                               (let ((Ns)  )
                                 (do ((chs Chords (cddr chs))) ((null chs))
                                   (push (dx->x (first chs) (second chs)) Ns) )
                                 (apply ,Const ,@ (when include? (reverse vars)) (nreverse Ns))))))
            (dolist (i vars res)
              (push (list (var2 i) 1) res)
              (push (list (var1 i) 0) res))))))

(defun Make-fix-Constraint (Const arity vars? indexes &optional varMap choiceMap)
  (if (and (symbolp vars?) (string= vars? *lower-voice-name*))
    (when (/= (/ arity 2) (length indexes)) (error "wrong number of arguments in user constraint"))
    (when (/= arity (length indexes)) (error "wrong number of arguments in user constraint")))
  (list (set-up-predicate Const indexes (and (symbolp vars?) (string= vars? 'i)) varMap choiceMap)))

;(all_combin_> '(8 5 4 2) 1)
(defun Make-allcomb-Constraint (Const arity vars? indexes &optional varMap choiceMap)
  (if (and (symbolp vars?) (string= vars? 'i))
    (let* (res (size (floor arity 2)) (combins (all_combin_>  indexes (1- size))))
           (dolist  (inds combins res)
             (push (set-up-predicate Const inds t varMap choiceMap) res)))
    (let* (res (combins (all_combin_>  indexes (1- arity))))
           (dolist  (inds combins res)
             (push (set-up-predicate Const inds nil varMap choiceMap) res)))))


#+:OM
(om::defmethod! om::UserConst  ((const t) (exp   list)) 
  (setq exp (expand-lst exp))
  (if (flatp exp) (UserConstraint const (append exp (list '(s))))
      (UserConstraint const exp)))
#+:OM
(om::defmethod! om::User-Cnstr  ((const t) (exp   list) &optional c-imp varMap choiceMap)
  :initvals '(nil (0_7s2 (a) 7_12 (s)) 1 nil nil)
  :indoc '("constraint predicate" "chord indexes expression" "cnstr importance" "variable Map" "choice map")
  :icon 520
  :doc "User defined constraint. 'Const' is a predicate and 'exp' is a list specifying those chords the predicate constraints.
 'exp' is of the form: (range1 (choice1) range2 (choice2)....rangeN (choiceN)),
  where 'range' is the set of chord indexes delimiting the region in which the constraint will
 be applied, and 'choice' is either:
   s     a constraint applying to subsequences in the region,
   f     a constraint for the fixed set of chords given in the region
   a     a constraint applying to all combinations of chords in the region.
Each of the above can be followed (optionally) by the symbol 'i'. In this case
the predicate in Const should have twice as much arguments as chords it constraints. The
extra arguments (which go all before the arguments refering to chords) represent the
indexes (i.e. chord number) of each chord in the constraint. The symbol 'i' is thus
used to allow constraints that must know not only the chords they test but also their numbers.
For instance,
  (0_10 (s)) means that 'Const' should be applied to all subsequences in the region from 
chords 0 to 10. The length of each subsequence is given by the number of arguments of 'Const'. If 'Const'
is a two argument predicate, for instance, then it will be applied to the following pairs of chords
 (0,1), (1,2),(2,3)...(9,10).
On the other hand, (1_7s2 (s i)) means that 'const' should be applied to subsequences of chords 1,3,5,7.
The 'i' in (s i) means that in this case 'const' should contain the indexes as arguments. for example
  (defun const (i j ch1 ch2) (= (abs (- i j)) (abs (- (first ch1) (first (ch2)))))),
which makes the horizontal interval between the base notes of chords equal to the difference of
their position in the sequence.
 's' can also be followed by an integer specifying the increment for constructing the subsequences. For
example, (0_10 (s 2)) and a predicate of two arguments apply 'Const' to each one of the pairs:
(0,1),(2,3),(4,5),...(8,9).
"
  (if (or varMap choiceMap)
    (progn
      (setq varMap (or varMap #'single-variable-map) choiceMap (or choiceMap #'identity))
      (includeConstraint-Importance 
       (let ((exp (expand-lst exp)))
         (if (flatp exp)
           (UserConstraint const (append exp (list '(s))) varMap choiceMap)
           (UserConstraint const exp varMap choiceMap)))
       c-imp))
    (eval `(defun ,(gensym "User-Cnstr") ()
             ',(includeConstraint-Importance
                (let ((exp (expand-lst exp)))
                  (if (flatp exp)
                    (UserConstraint const (append exp (list '(s))) varMap choiceMap)
                    (UserConstraint const exp varMap choiceMap))) 
                c-imp)
             )))
  
  )


#+:PW
(pw::defunp User-Cnstr  ((const pw::list (:value 'indentity))
                        (exp   pw::list (:value '(0_7s2 (a) 7_12 (s))))) list
"User defined constraint. 'Const' is a predicate and 'exp' is a list specifying those chords the predicate constraints.
 'exp' is of the form: (range1 (choice1) range2 (choice2)....rangeN (choiceN)),
  where 'range' is the set of chord indexes delimiting the region in which the constraint will
 be applied, and 'choice' is either:
   s     a constraint applying to subsequences in the region,
   f     a constraint for the fixed set of chords given in the region
   a     a constraint applying to all combinations of chords in the region.
Each of the above can be followed (optionally) by the symbol 'i'. In this case
the predicate in Const should have twice as much arguments as chords it constraints. The
extra arguments (which go all before the arguments refering to chords) represent the
indexes (i.e. chord number) of each chord in the constraint. The symbol 'i' is thus
used to allow constraints that must know not only the chords they test but also their numbers.
For instance,
  (0_10 (s)) means that 'Const' should be applied to all subsequences in the region from 
chords 0 to 10. The length of each subsequence is given by the number of arguments of 'Const'. If 'Const'
is a two argument predicate, for instance, then it will be applied to the following pairs of chords
 (0,1), (1,2),(2,3)...(9,10).
On the other hand, (1_7s2 (s i)) means that 'const' should be applied to subsequences of chords 1,3,5,7.
The 'i' in (s i) means that in this case 'const' should contain the indexes as arguments. for example
  (defun const (i j ch1 ch2) (= (abs (- i j)) (abs (- (first ch1) (first (ch2)))))),
which makes the horizontal interval between the base notes of chords equal to the difference of
their position in the sequence.
 's' can also be followed by an integer specifying the increment for constructing the subsequences. For
example, (0_10 (s 2)) and a predicate of two arguments apply 'Const' to each one of the pairs:
(0,1),(2,3),(4,5),...(8,9).
"
  (setq exp (pw::expand-lst exp))
  (if (flatp exp) (UserConstraint const (append exp (list '(s))))
      (UserConstraint const exp))
)

#+:OM
(om::defmethod! om::GeneralConstr  ((const t) variable-pairs)
  (if (listp (car variable-pairs))
    (apply 'list const variable-pairs)
    (list const variable-pairs) ))

#|
#+:OM
(om::defmethod! om::General-Cnstr  ((const t) variable-pairs)
  :initvals '(nil nil)
  :indoc '("constraint predicate" "a pair: variable-index level" "more pairs") :icon 207
  :doc ""

(if (listp (car variable-pairs))
  (apply 'list const variable-pairs)
  (list const variable-pairs) )
  
  )
|#
#+:OM
(om::defmethod! om::Generic-Cnstr  ((cnstr t) (var-inds list) (option t) &optional levels arity)
  :initvals '(nil (0 1) nil nil nil)
  :indoc '("constraint predicate" "a list of variable indexes" "fix, all or step" "include-inds?" "levels" "arity") :icon 520
  :doc "Defines an arbitrary constraint which can be independent of the intended chord interpretation of domains in SITUATION.
'cnstr' must be a predicate. 'var-inds' are the variable indexes where the constraint is applied. If
'indexes?' is not NIL, the variable numbers are included in the call to cnstr. 'levels' are the levels
of the domains of the variables where the constraint is applied. Default is 0. 'arity' is the
number of variables that 'cnstr' constraints. If NIL, the number of arguments of the predicate 'cnstr'
is taken. 'option' can be one of the following:
f or (f i)       the predicate is applied exactly to the variables in 'var-inds'.
a or (a i)       the predicate is applied to all combinations of variables in 'var-ind'
s or (s i) or (s number i)            
              the predicate is applied to all sequences of variables in 'var-ind'.`
              When given as a list (s n), subsequences used for applying
              the constraint are taken from the sequence in length-n steps.
For instance a constraint requiring the sequence of pitches 60 64 67 in the lower voice could be
specified as follows:
'cnstr' =  #'(lambda (p1 p2 p3) (equal (list p1 p2 p3) (list 60 64 67)))
'var-inds' = (0_16s2)         even variables (thus lower note) for the first 8 chords
'option'   = (s 3)
'levels'   = NIL   or, equivalently, (8* 0)
'arity'    = NIL"
  
  (parse-generic-constraint cnstr (expand-lst var-inds) option (expand-lst levels) arity)
  )

#+:PW
(pw::defunp Generic-Cnstr  ((cnstr pw::list (:value 'indentity)) (var-inds pw::list (:value '(0 1)))
                            (option list (:value '(s 1)))
                            &optional
                            (levels list (:value '()))
                            (arity list (:value '()))) list
  "Defines an arbitrary constraint which can be independent of the intended chord interpretation of domains in SITUATION.
'cnstr' must be a predicate. 'var-inds' are the variable indexes where the constraint is applied. If
'indexes?' is not NIL, the variable numbers are included in the call to cnstr. 'levels' are the levels
of the domains of the variables where the constraint is applied. Default is 0. 'arity' is the
number of variables that 'cnstr' constraints. If NIL, the number of arguments of the predicate 'cnstr'
is taken. 'option' can be one of the following:
f or (f i)       the predicate is applied exactly to the variables in 'var-inds'.
a or (a i)       the predicate is applied to all combinations of variables in 'var-ind'
s or (s i) or (s number i)            
              the predicate is applied to all sequences of variables in 'var-ind'.`
              When given as a list (s n), subsequences used for applying
              the constraint are taken from the sequence in length-n steps.
For instance a constraint requiring the sequence of pitches 60 64 67 in the lower voice could be
specified as follows:
'cnstr' =  #'(lambda (p1 p2 p3) (equal (list p1 p2 p3) (list 60 64 67)))
'var-inds' = (0_16s2)         even variables (thus lower note) for the first 8 chords
'option'   = (s 3)
'levels'   = NIL   or, equivalently, (8* 0)
'arity'    = NIL"
  
  (parse-generic-constraint cnstr (pw::expand-lst var-inds) option (pw::expand-lst levels) arity)
  )

(defun parse-generic-constraint (cnstr var-indexes option levels arity)
  (cond ((consp option)
         (cond ((string= (first option) 's)
                (set-box-all-sequences cnstr var-indexes levels
                                       (and (numberp (second option)) (second option))
                                       (symbolp (first (last (cdr option)))) arity))
               ((string= (first option) 'a)
                (set-box-all-pairs cnstr var-indexes levels (symbolp (first (last (cdr option)))) arity))
               ((string= (first option) 'f)
                (set-box-all-sequences cnstr var-indexes levels (length var-indexes)
                                       (symbolp (first (last (cdr option)))) (length var-indexes)))))           
        ((and (symbolp option) (string= option 's))
         (set-box-all-sequences cnstr var-indexes levels 1 nil arity))
        ((and (symbolp option) (string= option 'a))
         (set-box-all-pairs cnstr var-indexes levels nil arity))
        ((and (symbolp option) (string= option 'f))
         (set-box-all-sequences cnstr var-indexes levels (length var-indexes) nil (length var-indexes)))
        (t (error "wrong option in generic-cnstr" option))))
         

(defun set-box-all-pairs (constr var-indexes levels indexes? arity)
  (let* (res pair-set fun index-level-pairs 
            (arity (if indexes?
                     (floor (or arity (function-args (om::closure-function constr))) 2)
                     (or arity (function-args (om::closure-function constr)))))
            (all-tuples (delayed-eval::ser->list (delayed-eval::combinations var-indexes arity))))
    (when (zerop arity) (error "a constraint predicate should have at least one argument"))
    (when indexes?
      (dotimes (i arity) (push (gensym i) fun)))
    (unless levels (setq levels (make-list (length var-indexes) :initial-element 1)))
    (setq index-level-pairs (mapcar #'list var-indexes levels))
    (dolist (inds all-tuples res)
      (setq pair-set (mapcar #'(lambda (i) (list i (second (assoc i index-level-pairs)))) inds))
      (if indexes?
        (push 
         (cons (eval
                `(function (lambda ,fun (funcall ,constr ,@(mapcar #'car pair-set) ,@fun)))  )
               pair-set) res)
        (push (cons constr pair-set) res)
        )
      )))

(defun set-box-all-sequences (constr var-indexes levels step indexes? arity)
  (let (res pair-set fun ind-set lev-set 
            (arity (if indexes?
                     (floor (or arity (function-args (closure-function constr))) 2)
                     (or arity (function-args (om::closure-function constr))))) )
    (when (zerop arity) (error "a constraint predicate should have at least one argument"))
    (unless levels (setq levels (make-list (length var-indexes) :initial-element 1)))
    (unless step (setq step 1))
    (when indexes?
      (dotimes (i arity) (push (gensym i) fun)))
    (do ((inds var-indexes (nthcdr step inds)) (levs levels (nthcdr step levs))) ((< (length inds) arity) (nreverse res))
      (setq ind-set inds lev-set levs pair-set nil)
      (dotimes (k arity)
        (push (list (first ind-set) (first lev-set)) pair-set)
        (pop ind-set)
        (pop lev-set))         
      (if indexes?
        (progn 
          (setf pair-set (nreverse pair-set))
          (push 
           (cons (eval
                  `(function
                    (lambda ,fun (funcall ,constr ,@(mapcar #'car pair-set) ,@fun)) ) )
                 pair-set) res))
        (push (cons constr (nreverse pair-set)) res) )
     ))
 )

;; Some boxes ...
(defun n-sample-fun  ( xmin step xmax f &REST Lg &AUX LstOut)
  "applies sample-fun to a set of common-lisp functions"
  (push 
   #+:OM (om::samplefun   f  xmin step (1- xmax) )
   #+:PW (pw::sample-fun   f  xmin step (1- xmax) )
   LstOut)
  (dolist (g Lg)
    (push 
     #+:OM (om::samplefun g xmin step (1- xmax) )
     #+:PW (pw::sample-fun g xmin step (1- xmax) )
     LstOut) 
    )
  (reverse LstOut)
  )


;;; Expresses the n-queens problem in chords.
(defun no-atacan-chord ( ch1 ch2)
  (let (
        (ind1 (- (second ch1) (first ch1)))
        (ind2 (- (second ch2) (first ch2)))
        )
    (and (not (= (first ch1) (first ch2)))
         (not (= (abs (- (first ch1) (first ch2)))
                 (- ind2 ind1)))
         )
    )
  )

