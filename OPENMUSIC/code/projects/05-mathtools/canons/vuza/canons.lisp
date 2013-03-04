(in-package :om)

;================================================
;Good N for canons
;================================================

(defmethod! good-n-p ((n integer))
  :icon 420
  "List all numbers in the interval (low, hihg)."
  (let ((primes (sort (prime-facts n) '> :key 'second)))
    (not (or
          (= 1 (length primes))  ;N0
          (and                   ;N1
           (= 2 (length primes))
           (= 1 (second (second primes))))
          (= 2 (length primes) (second (second primes)) (second (first primes))) ;N2
          (and
           (= 3 (length primes))  ;N3
           (or (= (second (first primes)) 1) (= (second (first primes)) 2))
           (= 1  (second (second primes)) (second (third primes))))
          (and
           (= 4 (length primes))
           (= 1  (second (first primes)) (second (second primes)) 
              (second (third primes)) (second (fourth primes))))))))
         
         

(defmethod! canon-n ((a integer) (b integer))
   :initvals '(0 500) :indoc '("min" "max")
   :icon 420
   :doc "Liste les periodes n compris entre a et b correspondants a un pattern rythmique.
Etant donnes deux entiers quelconques a et b, avec a<b, cette fonctione donne tous les periodes n compris entre a et b correspondants
a un pattern rythmique qui qui genere par des translations temporelles, un canon regulier complementaire de categorie maximale."
     (loop for i from a to b
         when (good-n-p i) collect i))


;=====================================================
; CANON-MAKER
;=====================================================


(defun get-repres (lnum llist  n)
  (if (null llist) (if (not (periodic? (sort lnum '<) n)) (sort lnum '<))
      (let (rep)
        (loop for item in (car llist)
              while (not rep) do
              (setf rep (append rep (get-repres (append lnum (list item)) (cdr llist) n))))
        (remove-duplicates  rep :test 'equal))))

(defun periodic? (set n)
  (let ((periodic? nil))
    (loop for i from 1 to (- n 1)
          while (not periodic?) do
          (when (equal set (sort (mod+ i set n) '<))
            (setf periodic? t)))
    periodic?))

(defun g1234 (n num1 num2)
  (loop for i from 0 to (- n 1) by (/ n (* num1 num2)) collect i))

(defun n3Zn (n n3)
  (loop for i from 0 to (- n 1) by n3 collect i))

(defun cosets (n g1 g2)
  (remove-duplicates (loop for item in g1
                           collect (sort  (mod+ item g2 n) '<))))


(defun nx->dx (n list)
  (let ((rep  (x->dx (sort list '<))))
    (append rep (list (- n (apply '+ rep))))))

(defun consS (n g1 g2 g3 g4)
  (let ((cosetsg1g2 (cosets n g1 g2))
        (cosetsg3g4 (cosets n g3 g4))
        rep1 rep2 M)
    (setf rep1 (get-repres nil cosetsg1g2 n))
    (setf rep2 (get-repres nil cosetsg3g4 n))
    (setf M (sort (remove-duplicates (flat (loop for item in rep1 collect (mod+ item rep2 n))) :test 'equal) '<))
    (nx->dx n M)))


(defun algebrique+ (l1 l2 n)
  (sort (remove-duplicates (flat (loop for item in l1 collect (mod+ item l2 n)))) '<))


(defun cons-sub-R (g1 g2 g3 n)
  (algebrique+ (union (remove 0 g1) (list (nth-random (x-diff  g2 g3  'equal))))
               g3
               n))

(defun cons-N3-R (n n3Zn n2)
  (algebrique+ n2
               (x-diff (get-repres nil (cosets n (arithm-ser 0 (- n 1) 1) n3Zn) n) n3Zn 'equal)
               n))
  

(defun consR (n g1 g2 g3 g4 n3Zn)
  (let* ((n1 (cons-sub-R g4 g1 g2 n))
         (n2 (cons-sub-R g2 g3 g4 n))
         (n3 (cons-N3-R n n3Zn n2)))
    (nx->dx n (union n1 n3 :test 'equal))))
    
(defmethod! cons-S-R ((n integer))
  :icon 420
  ""
  (if (good-n-p n)
    (loop for item in (decompo n)
          collect (cons-one-S-R n item))
    (om-beep-msg (format nil "~D is not a good period number." n))))

(defmethod! cons-one-S-R ((n integer) decompo)
  :icon 420
  ""
  (if (good-n-p n)
    (let* ((g1 (g1234 n (first decompo) (third decompo)))
           (g2 (g1234 n (first decompo) 1))
           (g3 (g1234 n (second decompo) (fourth decompo)))
           (g4 (g1234 n (second decompo) 1))
           (n3Zn (n3Zn n (fifth decompo))) S  R)
      (setf S (consS n g1 g2 g3 g4))
      (setf R (consR n g1 g2 g3 g4 n3Zn))
      (list S R)
      )
    (om-beep-msg (format nil "~D is not a good period number." n))))

;(cons-S-R 200)



(defmethod! decompo ((n integer))
  :initvals '(72) :indoc '("periode")
  :doc "Etant donne un n correspondent  la periode dÕun pattern rythmique  qui genere  par des translations temporelles, 
un canon regulier de categorie maximale, cette fonction le decompose dans un produit de cinq nombres (p1, p2, n1, n2, n3).
attaches  un canon ayant comme nombre des voix le produit de n1 et n2 et 
comme nombre dÕattaques du pattern rythmique de base le produit des trois nombres restants." 
   :icon 420 
  (let* ((prime-facts (prime-factors n))
         (Q (mapcar 'car prime-facts)) rep)
    (loop for qi in Q do
          (let ((Q* (reverse (set-difference Q (list qi)))))
            (loop for qj in Q* do
                  (let* ((n* (/ n (* qi qj)))
                         (Q** (mapcar 'car (prime-factors n*))) )
                    (loop for n1 in (reverse (set-difference Q** (list qj))) do
                          (loop for n2 in (reverse (set-difference Q** (list qi))) do
                                (unless (= n1 n2)
                                  (push (list qi qj n1 n2 (/ n* (* n1 n2))) rep))))))))
    rep))


;============================================

(defun make-chordseq-canon (start R times period &optional (channel 1))
   (let ((onsets (loop for i from 0 to (- times 1)
                       append (om+ start (om+ (* i period) R)))))
     (make-instance 'chord-seq
       :Lmidic (create-list (length onsets) (* (+ 60 channel) 100))
       :LOnset onsets
       :Lchan (list channel)))
   )


(defun make-voice-canon (start R mindiv n sign &optional (channel 1))
   (let* ((ratios  (om/ (if (zerop start) R (cons (* -1 start) R)) mindiv))
          (tree (mktree ratios sign)))
     
     (make-instance 'voice
       :tree tree
       :chords  (create-list (length ratios) (* (+ 60 channel) 100))
       :tempo 120)))
  

(defmethod! CRRCCM ((n integer) times &key poly? inverse?  (beats 500) (mindiv 8) (sign '(4 4)))
   :icon 420
   ""
   (let* ((RS (cons-one-S-R n (nth-random (decompo n))))
          (R (if inverse? (car RS) (second RS)))
          (S (if inverse? (second RS) (car RS)))
          period)
     (if poly?
       (progn
         (setf S (butlast (dx->x 0 S)))
         (setf R (loop for i from 0 to (- times 1) append R))
         (make-instance 'poly
           :voices (loop for item in S
                         for i = 1 then (+ i 1)
                         collect (make-voice-canon item R mindiv n sign i)))
         )
       (progn
         (setf period (* n beats))
         (setf R (butlast (om* (dx->x 0 R) beats))
               S (butlast (om* (dx->x 0 S) beats)))
         (make-instance 'multi-seq
           :chord-seqs (loop for item in S
                             for i = 1 then (+ i 1)
                             collect (make-chordseq-canon item R times period i)))))))





(defmethod! Canons ((R list) (S list) (times integer) &key poly? (beats 250) (mindiv 8) (sign '(4 4)))
   :icon 420
   "Construit un canon rythmique regulier complementaire de categorie maximale.
R =  pattern rythmique
S = comme structure gerant les decalages temporelles.
times nombre de repetitions de R

keywords
poly? if t le resultat sera un poly sinon un multi-seq (default)

pour un multi-seq 
beats = la duree en ms de la pulsation 250 par default
pour un poly
mindiv = l'unite rythmique minimale par default (8 = la croche)
sign = la signature."
   (let* ((n (* (length R) (length S)))
          period)
     (if poly?
       (progn
         (setf S (butlast (dx->x 0 S)))
         (setf R (loop for i from 0 to (- times 1) append R))
         (make-instance 'poly
           :voices (loop for item in S
                         for i = 1 then (+ i 1)
                         collect (make-voice-canon item R mindiv n sign i )))
         )
       (progn
         (setf period (* n beats))
         (setf R (butlast (om* (dx->x 0 R) beats))
               S (butlast (om* (dx->x 0 S) beats)))
         (make-instance 'multi-seq
           :chord-seqs (loop for item in S
                             for i = 1 then (+ i 1)
                             collect (make-chordseq-canon item R times period i)))))))


(defmethod! Infocanons ((n integer))
     :initvals '(72) :indoc '("periode")
   :icon 420
   "Donne une liste ayant comme arguments le nombre de voix du canon et le nombre dÕattaques du pattern rythmique de base  lÕinterieur dÕun periode."
   (if (good-n-p n)
     (let* ((decompo (decompo n)) rep)
       
       (setf rep (remove-duplicates 
                  (loop for item in decompo 
                        collect (list (* (third item) (second item)) (* (first item) (fourth item) (fifth item))))
                  :test 'equal))
       (print (format nil "Canons"))
       (loop for item in rep do
             (print (format nil "Voices = ~D    Number of attacks = ~D" 
                            (first item) (second item))))
       rep)
     
     (om-beep-msg (format nil "~D is not a good period number." n))))


(defmethod! Patterns ((n integer) (numvoices integer))
   :initvals '(72 6) :indoc '("periode" "numbre de voix")
   :icon 420
   :numouts 2
   :doc "Etant donne une periode n et un nombre  de voix numvoices, il construit, si possible, deux structures rythmiques (respectivement R et S).
,correspondants au pattern rythmique R qui genere par des translations temporelles determinees par le pattern S, 
un canon regulier complementaire de categorie maximale."
  (if (good-n-p n)
     (let* ((decompo (decompo n))
            gooddec)
       (loop for item in decompo
             while (not gooddec) do
             (when (= (* (third item) (second item)) numvoices)
               (setf gooddec item)))
       (if gooddec
         (let ((rs (cons-one-S-R n gooddec)))
           (values (car rs) (second rs)))
         (format nil "There are not canons of ~D voices for n = ~D~%" numvoices n) ))
     (om-beep-msg (format nil "~D is not a good period number." n))))


(defmethod! Patterns ((n integer) (numvoices list))
   (if (good-n-p n)
     (let* ((decompo (decompo n))
            gooddec)
       (if (not (member numvoices decompo :test 'equal))
         (om-beep-msg (format nil "~D is not a good decomposition for ~D." numvoices n))
         (let ((rs (cons-one-S-R n numvoices)))
           (values (car rs) (second rs)))))
     (om-beep-msg (format nil "~D is not a good period number." n))))

;=============================================

(defun flat-canon (M S n)
  (let ((rep (make-list n :initial-element nil))
        (points (butlast (dx->x 0 M))))
    (loop for item in (butlast (dx->x 0 S))
          for i = 1 then (+ 1 i) do
          (loop for k in (mod+ points item n) do
                (setf (nth k rep) i)))
    rep))


;==============

#|
(defmethod! set-melody ((n integer) (numvoices integer))
  :initvals '(72 6) :indoc '("periode" "numbre de voix")
  :icon 420
  :numouts 2
  :doc "Etant donne une periode n et un nombre  de voix numvoices, il construit, si possible, deux structures rythmiques (respectivement R et S).
,correspondants au pattern rythmique R qui genere par des translations temporelles determinees par le pattern S, 
un canon regulier complementaire de categorie maximale."
  "a faire")
|#

;(flat-canon '(3 1 5 6 9 4 11 6 3 3 1 20) '(8 8 2 8 8 38) 72)
