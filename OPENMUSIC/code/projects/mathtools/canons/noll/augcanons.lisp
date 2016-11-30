(in-package :om)

;=================
;TOOLS
;=================


;T if S and R are de same sets
(defun set-equal (S R)
   (if (atom (car S))
     (equal S R)
     (and (= (length S) (length R))
          (let ((rep t))
            (loop for item in R
                  while rep do
                  (unless (member item S :test 'set-equal)
                    (setf rep nil)))
            rep))))

;T if S and R are de same sets
(defmethod! mod-op ((self integer) (num integer) (n integer) fun) 
   :initvals '(0 0) :indoc '("first input" "second input")
   :icon 420
   :doc "Apply op to two numbers or trees modulo n."
   (mod (funcall fun self num) n))

(defmethod! mod-op ((self integer) (num list) n fun)  
   (mapcar #'(lambda (input)
               (mod-op self input n fun)) num))

(defmethod! mod-op ((self list) (num integer) n fun)   
   (mapcar #'(lambda (input)
               (mod-op  input num n fun)) self))

(defmethod! mod-op ((self list) (num list) n fun)
   (mapcar #'(lambda (input1 input2)
               (mod-op input1 input2 n fun)) self num))


(defun TUnion (S &rest arg )
  (if arg
    (apply 'union (list S (car arg)) (cdr arg) '(:test 'equal))
    (union S nil :test 'equal)))

(defun applyUnion (test args)
   (let (rep)
     (loop for item in args do
           (when item
             (setf rep (union item rep :test test))))
     rep ))

 
(defun equal-set (R S)
  (and (= (length R) (length S))
       (let ((rep t))
         (loop for item in R
               while rep do
               (unless (member item S :test 'equal)
                 (setf rep nil)))
         rep)))

(defun fixedpoint (func exp)
  (let ((rep exp )
        (aplication (funcall func exp)))
    (loop while (not (equal-set aplication rep)) do
          (setf rep aplication)
          (setf aplication (funcall func rep)))
    rep))

(defun cartesian (l1 l2 fun) 
  (mapcar #'(lambda (x) (mapcar #'(lambda (y) (funcall fun x y))  l2)) l1))

(defun CartesianProduct (l1 l2)
  (flat (cartesian l1 l2 'list) 1))


(defun i-nextKSubset (subset n)
   (let ((l (length subset))
         rep)
     (loop for k from 1 to l
           while (not rep) do
           (let ((elem (car (last subset k))))
             (if (<= elem (- n k))
               (setf rep (list k (+ elem 1))))))
     (if rep
       (progn 
         (loop for k from (- l (first rep)) to (- l 1)
               for val = (second rep) then (+ val 1) do
               (setf (nth k subset) val))
         subset)
       (arithm-ser 0 (- (length subset) 1) 1))))
  
                
;============
; (Rhythm2Num '(0 1 4)) = 2^0 + 2^1 + 2^4 = 19
(defun Rhythm2Num (R)
  (loop for i in R sum (expt 2 i)))

; un ordre sur les rhythmes
(defun Smaller (R1 R2)
  (<= (Rhythm2Num R1) (Rhythm2Num R2)))


(defun zn (n) (arithm-ser 0 (- n 1) 1))

;
(defun aff (n trans k)
  (mod (+ (* k (first trans)) (second trans)) n))


(defun representative (n R)
  (let ((candidate R))
    (loop for a from 1 to n do
          (if (relativelyprimes a n)
            (loop for b from 1 to n do
                  (let ((candidate1 (Tunion 
                                     (loop for af in R collect (aff n (list a b) af)))))
                    (when (Smaller candidate1 candidate)
                      (setf candidate candidate1))))))
    (sort candidate '<)))

;(representative 12 '(0 2 8 11))
#|

(defun ConcatenateFromLeft (n trans1 trans2)
  (list (mod (* (first trans1) (first trans2)) n)
        (mod (+ (* (first trans1) (second trans2)) (second trans1)) n)))

(defun PairwiseProducts (n X Y)
  (Tunion (loop for item in Y append
        (loop for elem in X
             collect  (ConcatenateFromLeft n elem item)))))

(defun GenerationStep (n X )
  (union x (PairwiseProducts n x x) :test 'equal))


(defun GenerateSubgroup (n X )
  (fixedpoint #'(lambda (k) (GenerationStep n k)) X))

(defun CyclicSubgroup (n x )
  (GenerateSubgroup n (list x)))


(defun AllCyclicSubgroups (n)
  (let (subgrL)
    (loop for a from 0 to (- n 1) do
          (if (relativelyprimes a n)
            (loop for b from 0 to (- n 1) do
                  (setf subgrL
                        (union (append subgrL (list (CyclicSubgroup n (list a b)))) nil :test 'equal-set)))))
    subgrL))


(defun OneMoreElement (n X)
  (let (OneMoreList)
    (loop for a from 0 to (- n 1) do
          (if (relativelyprimes a n)
            (loop for b from 0 to (- n 1) do
                  (unless (member (list a b) X :test 'equal)
                    (setf OneMoreList  (Union (append OneMoreList (list (GenerateSubgroup n (Union X (list (list a b)))))) nil
                                              :test 'equal-set))))))
    OneMoreList))


(defun GenerateAllSubgroups(n)
  (let ((Box1 (AllCyclicSubgroups n))
        Box2)
    (loop while box1 do
          (let ((Workgroup (first Box1)))
		(setf Box2 (Union Box2 (list Workgroup) :test 'equal-set))
		(setf Box1 (cdr Box1))
		(setf Box1 (Union Box1 (OneMoreElement n Workgroup) :test 'equal-set))))
    Box2))


(defun SubgroupRepresentative (n G R)
  (let ((Candidate R))
    (loop for m in G do
          (let ((candidate1 (TUnion (loop for elem in R
                                          collect (aff n (nth m G) elem)))))
            
            (when (Smaller Candidate1 (TUnion Candidate))
              (setf Candidate Candidate1))))
    Candidate))
|#

;==================================
;All classes and all signatures

(defmethod! AllClasses ((n integer) k)
  :initvals '(12 6) :indoc '("n" "cardinality")
  :doc "It gives the collection of equivalent classes of chords of a given cardinality up to affine transformations. By default it gives the 34 affine classes of hexachords inside of the division of the octave in 12 parts  " 
  :icon 424
  (let* ((ClassList nil)
	 (ThisZn (Zn  n))
         (ThisR (Zn k))
         (ThisRStandard ThisR)
         (LastR (loop for l from 0 to (- k 1) collect (* (/ n k) l)))
         (ThisNum 0)
         (ThisStandardNum 0)
         (MaxNum  (Rhythm2Num LastR)))
    (loop While (not (equal ThisR LastR)) do
          (setf ClassList (Union ClassList (list ThisRStandard) :test 'equal)
                ThisR (i-NextKSubset  ThisR (- n 1))	
                ThisRStandard (Representative n ThisR)
                ThisNum (Rhythm2Num ThisR)
                ThisStandardNum (Rhythm2Num ThisRStandard)))
    (Append ClassList (list LastR))))


(defun PrimeModn (n a) (= (GCD n a) 1))

(defun Augmentations (n)
  (loop for k from 1 to (- n 1)
        when (PrimeModn n k) collect k))

(defmethod!  Signatures ((n integer) k)
  :initvals '(12 6) :indoc '("module" "length")
  :doc "It provides the list of potential multiplicative factors which have to be applied to a rhythmic pattern of a given length in order to strech it according to the tiling process. To be noticed that these multiplicative factors are all relatively primes with the period n of the initial rhythmic pattern."
  :icon 424
  (let ((AugList (Augmentations n))
	(SigList '((1)) ))
    (loop for l from 2 to k do
          (setf SigList (Tunion (mapcar #'(lambda (x) (sort x '<)) 
                                        (mapcar 'flat
                                                (CartesianProduct SigList AugList))))))
    SigList))


;====

(defun DisjointCond (Y X) 
  (null (Intersection Y X :test 'equal)))

(defun Vereinige (Y X) (Union Y X :test 'equal))

(defun SomeTrans (n Tselect R)
  (loop for j in Tselect
        collect (mod-op j R n '+)))

(defun TryVoice (n R Y)
  (let ((Tcomp (set-difference  (Zn n) Y :test 'equal)))
    (Tunion (mapcar #'(lambda (k) (Vereinige k Y))
                    (loop for item in (sometrans n TComp R)
                          when (DisjointCond Y item) collect item)))))

(defun FullCondition (n Y R)
   (and (= (length Y) (length R))
        (TryNewVoice n Y R)))


(defun TryVoiceWithX (n R X)
   (let ((sets (mapcar #'(lambda (k) (TryVoice n R k)) X)) rep)
     (loop for item in sets do
           (when item
             (setf rep (union item rep :test 'equal-set))))
     rep ))


(defun TryCanonModn (n R S)
   (let ((pot (list (mod-op (car S) R n '*))))
     (loop for k from 1 to (- (length S) 1)  do
           (setf pot (TryVoiceWithX n (Mod-op (nth k S)  R n '*) pot)))
     (equal-set (car pot) (zn n))))




;============================


(defun DisjointCondition (X)
  (= (length (applyUnion 'equal X))
     (apply '+ (mapcar 'length X))))


(defun GiveMeAllCanons (n R S)
  (let* ((AugS (loop for item in S collect (mod-op item R n '*)))
         (Pot (list (list (car AugS)))))
    (loop for l from 1 to (- (length S) 1) do
          (let ((CandList (loop for j from 0 to (- n 1) collect (mod-op (nth l Augs) j n '+))))
            (setf pot (flat (loop for elem in pot
                                  collect (loop for k from 0 to (- n 1)
                                                collect (append elem (list (nth k CandList))))) 1))
            (setf pot (loop for item in pot   
                            when (DisjointCondition item) collect item))))
    pot))



;=======================================

;=======================================
(defun get-translation (r r1 n)
  (let (rep)
    (loop for i from 0 to (- n 1)
          while (not rep) do
          (when (equal-set r1 (mod-op r i n '+) )
            (setf rep  i)))
    rep))


(defun get-trans-from-canon (canon s)
  (let* ((n (* (length canon) (length (car canon))))
         (rhythm (car canon))
         (translations (loop for m in s collect (mod-op m rhythm  n '*)))
         )
    (list rhythm
          (loop for m in s
                for tr in translations
                for item in canon
                collect (list m (get-translation tr  item n))))))

(defmethod! AllCanons-aff (n R S)
  :initvals '(12 (0 1 2 4 5 7) (1 5)) :indoc '("module" "pattern" "factors")
  :doc "It provides for the original pattern the list of couples (a1 b1), (a2, b2) etc where ai are the multiplicative factors and bi are the translation factors. For example, the solution (((0 1 2 4 5 7) ((1 0) (5 10)))) means that the 6 element rhythmic pattern (0 1 2 4 5 7) can generate a tiling canon via multiplications by 1 and translation by 0 (i.e. the original pattern) and multiplication by 5 and translations by 10 units."
  :icon 424
  (let* ((canons (remove-duplicates (GiveMeAllCanons n R S) :test 'set-equal)))
    (loop for canon in canons
          collect (get-trans-from-canon canon S ))))



(defun *sym (s1 s2 n)
  (list (mod (* (first s1) (first s2)) n)
        (mod (+ (* (second s2) (first s1)) (second s1)) n)))

(defun affin-sym (s1 x n)
  (mod (+ (* (first s1) x) (second s1)) n))

;(*sym '(5 3) '(5 1) 12)


(defun R*S (r s)
  (let ((n (* (length r) (length s))))
    (loop for item in r
          collect
          (loop for elem in s
                collect
                (*sym item elem n)))))

(defun mat-entries (mat) (flat mat 1))

(defun mat-translations (mat)
  (mapcar 'second (mat-entries mat)))

(defun R*S*x (r s x)
  (let ((n (* (length r) (length s)))
        (s*r (R*S s r)))
    (loop for line in s*r
          collect
          (loop for elem in line
                collect
                (affin-sym elem x n)))))

;=======================

;n = period
;k = attacks in R

;pour toute les paires de clases rhythmiques et signatures
; test TRYCANONMODN

(defmethod! ag-canonInfo ((n integer) (k integer))
  :initvals '(12 6) :indoc '("module" "length")
  :doc "It provides the list of rhythmic patterns of a given length together with the multiplicative factors which have to be applied to them in order to build tiling canons in which each voice is an augmentation of the original pattern according to the multiplicative factors. For example, the solution ((0 1 2 4 5 7) ((1 5))) means that the 6 element rhythmic pattern (0 1 2 4 5 7) can generate a tiling canon via multiplications by 1 (the original pattern) and by 5."
  :icon 424
  (let ((RList (AllClasses n k))
        (SList (Signatures n (/ n k)))
        Sammle)
    (Print (format nil "~D rhythm classes calculated." (length RList)))
    (Print (format nil "~D signature candidates calculated." (length SList)))
    (loop for r in RList do
          (let ((SammleR nil))
            (loop for s in SList do
                  (when (TryCanonModn n r s)    
                    (setf SammleR (append SammleR (list S)))))
            (when SammleR
              (setf Sammle (append Sammle (list (list r SammleR)))))))
    Sammle))




;==============
(defun augm (n list)
  (+ (* n (car list) ) (second list)))


(defmethod! voice-from-onsets ((self list) &key (midic 6000) (tempo 120) (channel 1) (mindiv 8) (sign '(4 4)))
   :icon 420
   (if (zerop (car self))
     (setf self  (x->dx self))
     (setf self  (cons (* -1 (car self)) (x->dx self))))
   (let* ((ratios  (om/ self mindiv))
          (tree (mktree ratios sign)) rep)
     (setf rep (make-instance 'voice
                 :tree (reducetree tree)
                 :chords  (if (consp midic) midic (create-list (length ratios) midic))
                 :tempo 120))
     (set-channel rep channel) 
     rep))




(defun make-voice-from-onsets (onsets mindiv sign tempo channel)
   (let* ((silence (* -1 (car onsets)))
          (ratios  (om/ (if (zerop silence) (x->dx onsets) (cons silence (x->dx onsets))) mindiv))
          (tree (mktree ratios sign)) rep)
     (setf rep (make-instance 'voice
                 :tree tree
                 :chords  (create-list (length ratios) (* (+ 48 channel) 100))
                 :tempo tempo)) 
     rep))


(defmethod! augmented-canon ((r list) (factor list)  (times integer)
                             &key poly? (beats 250) (mindiv 8) (sign '(4 4)) (tempo 60))
  :initvals '((0 1 2 4 5 7) ((1 0) (5 10)) 3) :indoc '("pattern" "factors" "times")
  :doc "It uses the informations of the function AllCanons-aff in order to concretely build a multi-seq representing a rhythmic tiling canon in which all voices are augmentations of the original rhythmic pattern. The augmented voices are repeated a given number of times. "
  :icon 424
  
  (let* ((period (* (length r) (length factor)))
         (voices (loop for item in factor
                       collect (loop for val in r collect (augm val item))))
         (aug-multiple (apply 'lcm (mapcar 'first factor)))
         (long-period (* period aug-multiple)) 
         onsets)
    (setf voices (loop for voice in voices
                       for fac in (mapcar 'first factor)
                       collect
                       (let* ((taille (- (car (last voice)) (first voice)))
                              (howmany (/ (/ long-period fac) period))
                              (realhowmany (* howmany times))
                              (step (- (/ long-period  howmany) taille)))
                         (loop for k from 0 to (- realhowmany 1) append (om+ voice (* k (+ step taille)))))))
    (setf onsets (loop for voice in voices
                       for i = 0 then (+ i 1) append
                       (loop for j from 0 to (- (car (nth i factor)) 1) collect
                             (om+ voice (* period j) ))))
    (if poly?
      (loop for item in onsets
            for k = 1 then (+ k 1)
            collect (make-voice-from-onsets item mindiv sign tempo k))
      (loop for item in onsets
            for k = 1 then (+ k 1)
            collect (make-instance 'chord-seq
                      :Lmidic (create-list (length item) 6000)
                      :Lchan (list k)
                      :Lonset (om* item 250))))))



#|

(CanonInfo  18 3)

(((0 2 6) ((1 7 11 11) (1 1 11 11) (1 5 7 11) (1 1 5 11) (1 5 5 7) (1 1 5 5))) 
 ((0 1 2) ((1 11 11 11) (1 1 11 11) (1 5 7 11) (1 7 7 11) (1 1 7 7) (1 1 5 7) (1 5 5 11) (1 1 5 5) (1 1 1 11) (1 1 1 1))) ((0 1 6) ((1 7 11 11) (1 1 11 11) (1 5 7 11) (1 1 5 11) (1 5 5 7) (1 1 5 5))) ((0 2 4) ((1 11 11 11) (1 7 11 11) (1 5 11 11) (1 1 11 11) (1 1 7 11) (1 5 7 11) (1 7 7 11) (1 7 7 7) (1 5 7 7) (1 1 7 7) (1 1 5 11) (1 1 5 7) (1 5 5 11) (1 5 5 7) (1 5 5 5) (1 1 5 5) (1 1 1 11) (1 1 1 7) (1 1 1 5) (1 1 1 1))) ((0 1 3) ((1 7 11 11) (1 1 11 11) (1 5 7 11) (1 1 5 11) (1 5 5 7) (1 1 5 5))) ((0 1 5) ((1 5 11 11) (1 1 11 11) (1 1 7 11) (1 5 7 11) (1 5 7 7) (1 1 7 7) (1 5 5 5) (1 1 5 5) (1 1 1 5) (1 1 1 1))) ((0 1 4) ((1 1 11 11) (1 5 7 11) (1 1 5 5))) ((0 4 8) ((1 11 11 11) (1 7 11 11) (1 5 11 11) (1 1 11 11) (1 1 7 11) (1 5 7 11) (1 7 7 11) (1 7 7 7) (1 5 7 7) (1 1 7 7) (1 1 5 11) (1 1 5 7) (1 5 5 11) (1 5 5 7) (1 5 5 5) (1 1 5 5) (1 1 1 11) (1 1 1 7) (1 1 1 5) (1 1 1 1))))

(allcanons-aff 12 '(0 2 6) '(1 5 7 11))

(((0 2 6) ((1 0) (5 10) (7 3) (11 1))) 
 ((0 2 6) ((1 0) (5 10) (7 5) (11 3))) 
 ((0 2 6) ((1 0) (5 10) (7 7) (11 5))) 
 ((0 2 6) ((1 0) (5 10) (7 9) (11 7))) 
 ((0 2 6) ((1 0) (5 10) (7 11) (11 9))) 
 ((0 2 6) ((1 0) (5 11) (7 1) (11 10))))
(canons

|#


