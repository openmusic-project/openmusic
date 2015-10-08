(in-package ::om)


;;


;;            Librairie CRIBLES
;;
;;            A Partir de la librairie RepMus PW
;;            Gerard Assayag, Claudy Malherbe, Andre Riotte, Franck Avitabile  © IRCAM 1996
;;            Portage OM: Karim HADDAD  © IRCAM 2001

 


; postulat : on suppose toutes les listes triees par ordre croissant

; le crible modulaire n'est pas un crible. C'est un ensemble succeptible de subir
; un harcelement criblidien.

;*********************************************************************************************************
;* definition des operateurs binaires et unaires                                                         *
;*********************************************************************************************************
(setq operateur_binaire
      '((+ unie)
        (- intersecte)         ; i.e. difference ensembliste
        (* compose)            ;   &   intersection
        (/ difference)
        (// difference_symetrique)
        ))
; l'affectation, operateur binaire est traite comme cas particulier dans l'evaluation de l'expression lc

(setq operateur_unaire
      '((c complementaire)
        (d definit)
        (a aleatoire)
        (p partition)
        (e definit_avec_evaluation))
      )

; l'operateur d sert a definir un crible a priori exemple cr1=d(2 5 8 9 11 19)
; la suite doit imperativement etre strictement croissante sous peine d'explosion immediate

; variables globales indiquant la borne inf et la borne sup de tout crible
; si le crible a un bornage, la borne inf et la borne sup sont sans signification
; quand on applique un crible sur une structure, les bornes sont mis a jour en fonction de
; la taille de ctte structure
(setf b> 200)
(setf b< 0)

;*********************************************************************************************************
;* fonction de creation et de manipulation de cribles proprement dit                                     *
;*********************************************************************************************************

; genere un crible aleatoire . liste = (pas &optional b< b>) ainsi aleatoire(1 0 10) -> (0 1 2 3 4 5 6 7 8 9 10)


(defun aleatoire (pas)
  (let ((pas (pop pas))
        (b1 (or (pop pas) b<))
        (b2 (or (pop pas) b>)))
    (if (< pas 1) (format t " aleatoire '(x) , x>0 ")
        (let ((i (1+ b2)) (j 0))
          (do ((z nil ))
              ((< i  b1) (if (< (first z)  b1) (cdr z) z))
            (setf j (1+ (om-random-value pas)))
            (setf i (- i j))
            (push i z))))))

; genere (classe residu b< b>) : le crible classe residu. Le bornage est optionel
(defun genere (class res &optional (b1 b<) (b2 b>))
  ; la ligne ci-dessous sert uniquement dans le cas de partition gouffre
  (if (> b1 3639) (progn (setf b1 (- b1 3640)) (setf b2 (- b2 3640))))
  (let ((a (+ res (* class (floor ( / b2 class))))))
    (if (> a b2) (setf a (- a class)))
    (let ((x (list a)))
      (dotimes (n (floor (/ (- b2 b1) class)))
        (push (- (first x) class) x)
        )
      (if (< (first x) b1) (cdr x) x)
      )))

; fonction union : unie 2 cribles au sens ensemblistes
(defun unie (crible1 crible2)
  (if (or (null crible2) (null crible1)) (append crible1 crible2)
      (reverse (do   ((z nil))
                     ((or (null crible2) (null crible1)) (append (reverse crible1) (reverse crible2) z))
                 (if (equal (first crible1) (first crible2)) (pop crible1)             
                     (if (< (first crible1) (first crible2)) (setq z (cons (pop crible1) z)) (setq z (cons (pop crible2) z))))
                 ))))

; fonction intersection : intersecte 2 cribles au sens ensemblistes
(defun intersecte (crible1 crible2)
  (if (or (null crible2) (null crible1)) (append crible1 crible2)
      (reverse (do   ((z nil))
                     ((or (null crible2) (null crible1)) z)
                 (if (equal (first crible1) (first crible2)) (setq z (cons (pop crible1) z))             
                     (if (< (first crible1) (first crible2)) (pop crible1) (pop crible2)))
                 ))))

; version optimise. On applique  crible1 sur crible2. ordre des parametres a verifier
(defun compose ( crible1 crible2)
  (let ((z nil))
    (let ((a 0))      
      (do ((i 0 (1+ i)))
          ((null crible2) (reverse z))
        (setf a (pop crible2))
        (if (equal i (first crible1)) (and (pop crible1) (setf z ( cons a z))))))))

; composition non optimise . peut se faire en o(#x+#y)
(defun compose2  (x y)
  (let ((z nil))
    (dolist  (a x) (setq z (cons (nth a y) z)))
    (reverse z)))

; difference au sens ensembliste : x/y=x-y=intersecte(x complementaire(y))
; = tout ce qui appartient ˆ x et qui n'appartient pas ˆ y
; question : peut on traiter directement sur ce qui suit le test si vide
; exemple : (5 1)-(5 0)=vide car ...
(defun difference ( crible2 crible1)
  (do ((z nil))
      ((or (null crible1) (null crible2)) (reverse (append (reverse crible1) z)))
    (do ((a 0))
        ((or (null crible1) (null crible2) (equal 1 a)))
      (if (< (first crible1) (first crible2))
        (progn (setf a 1) (push (pop crible1) z))
        (if (equal (first crible1) (first crible2)) 
          (progn (pop crible1) (pop crible2)) 
          (pop crible2))))))

; fonction sur des ensembles
(defun difference_symetrique (crible1 crible2)
  (unie (difference crible1 crible2) (difference crible2 crible1)))

; fonction pour definir un crible a priori. Dans lc, tapez d(0 1 2 3 8 14) pour definir le crible (0 1 2 3 8 14)
(defun definit (crible)
  crible)

; pour inserer des expressions lisp dans lc, il suffit d'evaluer l'expression


(defun definit_avec_evaluation (prog-lisp)
  (eval prog-lisp))

; complementaire de (x y) = union (x z), 0<z<x, z<>y !
; ou des trucs du type c(a+b)=c(a)-c(b)
; complementaire de x : 
; ens. des y tels que min x < y < max x 
;         et tels que y n'appartiennent pas a x
(defun complementaire (crible)
  (do ((y (1+ (pop crible)) (1+ y)) (z nil))
      ((null crible) (reverse z))
    (if (< y (first crible)) (push y z) (pop crible))))

;*********************************************************************************************************
;*********************************************************************************************************
;****************************petites fonctions utiles***************************************************** 
;*********************************************************************************************************
;*********************************************************************************************************

; renvoie true si la ligne est un commentaire et l'affiche sinon
(defun traiter_commentaire(y)
  (if (or (null y) (equal (first y) '¤)) nil y))

;calcule la longueur d'une liste . Cette fonction a ete sujette a des annees de recherche
(defun long (liste)
  (if (atom liste) 0 (1+  (long (cdr liste)))))

; calcule la longueur dune structure rtm ( structure recursive )
(defun long-rtm (beats)
  (do ((i 0) (a nil) (b nil) (c (first (second (first beats)))))  
      ((null (first beats)) i)
    (setf a (pop   beats) )
    (setf b (second a))
    (do ((z nil) )
        ((null b) (reverse z))
      (setf c (pop b)) 
      (if (typep c 'list)
        (setf i (+ i (long-rtm (list c)))))
      (setf i (1+ i)))))



; revoie vrai si l'expression passe est de la forme (x y)
(defun couple (liste)
  (if (typep liste 'list )
    (if (or (equal (long liste ) 2) (equal (long liste) 4))
      (if (typep (first liste ) 'number )
        (if (typep (second liste) 'number ) t )))))

; renvoie vrai si x est de type symbole ou entier
(defun atome (x)
  (if (or (typep x 'number) (typep x 'symbol)) t))

;**************************************************************************************
;* version 2 plus joli et surtout plus complete ( operateur unaire et binaire )       *
;**************************************************************************************
(defun inpre (ligne)
  (if (not(null ligne))
    (cond 
     
     ((assoc (first ligne) operateur_binaire) 
      (list (second (assoc (first ligne) operateur_binaire)) (inpre (cdr ligne) )))
     
     ((assoc (first ligne) operateur_unaire)   
      (if (null (cdr(cdr ligne)))
        (list (second (assoc (first ligne) operateur_unaire))
              (if (and (listp (second ligne))
                       (null (rest (second ligne)))
                       (symbolp (first (second ligne))))
                (list 'funcall (list 'fdefinition (cons 'quote  (second ligne))))
                (cons 'quote (list (second ligne)))))
        (list (second (assoc (first ligne) operateur_unaire))  
              (cons 'quote (list (second ligne))) 
              (inpre (cdr (cdr ligne)))))) 
     
     ((couple (first ligne))
      (if (null (second ligne))
        (if (equal (long (first ligne)) 2)
          (list 'genere (first ( first ligne)) (second (first ligne)))
          (list 'genere (nth 0 (first ligne)) (nth 1 (first ligne)) (nth 2 (first ligne)) (nth 3 (first ligne))))
        
        (if (equal (long (first ligne)) 2)
          (append (inpre(cdr ligne)) (list (list 'genere (first ( first ligne)) (second (first ligne)))))
          (append (inpre(cdr ligne)) (list (list 'genere (nth 0 (first ligne)) (nth 1 (first ligne)) (nth 2 (first ligne)) (nth 3 (first ligne)
                                                                                                                                )) ))
          )))
     
     ((typep (first ligne) 'list )
      (if (null (second ligne)) 
        ( append   (inpre(cdr ligne ))    (inpre(first ligne )))
        ( append   (inpre(cdr ligne ))    (list  (inpre(first ligne ))))))
     
     ((atome (first ligne))
      (if (null (second ligne)) (list 'funcall (list 'quote (first ligne)))
          (reverse (cons (list 'funcall  (list 'quote (first ligne))) (reverse (inpre (cdr ligne)))))))
     )))



;*********************************** 
;* embryon de la boite lc          *
;*********************************** 


(defun put-in-package (list package)
  (if (atom list)
    (if (symbolp list)
      (intern (symbol-name list) ::om)
      list)
    (cons (put-in-package (first list) package)
          (put-in-package (rest list) package))))





; partition construit la partition de n ensembles : retour = prefixe + indice de l'ensemble
; cf exemple un autre jour
; p = (prefixe e(1) e(2) ... e(n) )
(defun partition (p)
  (let ((a nil))
    (let ((nom1 nil))
      (let ((nom nil))
        (let ((prefix (string (pop p))))
          (let ((n (long p))) 
            (dotimes (b (1- ( round (exp (* n (log 2))))))       ;   2^n
              (setf a (1+ b))
              (let ((ligne))      
                (let ((res
                       (let ((y 
                              (do ((z nil))
                                  ((< a 2) z)
                                (push  (mod a 2) z)
                                (setf a (floor (/ a 2)))) 
                              ))
                         (push a y)
                         (dotimes (i (- n (long y)))
                           (push 0 y))
                         (setf a (1+ (long y)))
                         (do ((i 1 (1+ i)) (z (list nil nil)))
                             ((equal i a) (list (reverse (first z)) (reverse (second z))))
                           (if (equal (pop y) 1) (setf z (list (cons (1- i) (first z)) (second z)))
                               (setf z (list (first z) (cons (1- i) (second z))))))
                         )))
                  (setf nom1 (reverse (first res)))
                  (setf res (list (compose (first res) p) (second res)))
                  (setf res (list (first res) (compose (second res) p)))
                  (setf res (list (transfo (first res) '-) (transfo (second res) '+)))
                  (if (null (first res)) (setf ligne nil)
                      (if (null (second res)) (setf ligne (first res))
                          (if (null (second (first res))) (setf ligne (list (first (first res)) '/ (second res)))
                              (if (null (second (second res))) (setf ligne (list (first   res ) '/ (first (second res))))
                                  (setf ligne (list (first res) '/ (second res))))))) 
                  (push '= ligne)
                  (setf nom
                        (do ((nom2 ""))
                            ((null nom1) nom2)
                          (setf nom2 (format () "~A~A"  (1+ (pop nom1)) nom2)))
                        ) 
                  (setf nom (format () "~A~A"  prefix nom)) 
                  (push (intern nom ::om) ligne )
                  (push '% ligne)
                  (lc ligne) 
                  )))))))))

; primitive servant a l'elaboration de la partition
(defun transfo (list s)
  (do ((a nil))
      ((null list) (reverse (cdr a)))
    (push (pop list) a)
    (push s a)))

;*********************************************************************************************************
; boite d'evaluation 
;*********************************************************************************************************

; appel de la fonction crible qui calcule effectivement le crible entre b< et b>
(defun eval-simple (crible) 
  (funcall (fdefinition (intern (string crible) ::om))))

; meme chose que eval-simple avec un bornage automatique
(defun evaluer (crible)
  (setf b> 200)
  (setf b< 0)
  (funcall (fdefinition  (intern (string crible) ::om))))

; on utilise evaluer lors de la conception du crible et eval-simple une fois l'avoir appliquer 
; sur une structure particuliere



; la fonction crible crible la liste donnee avec un crible definie dans lc 
(defun crible (list crible)
  ""
  (setf b> (long list))
  (compose (eval-simple crible) list))


(defun sieve-rtm-tree (tree sieve mode)
  (let ((counter -1) (begin t))
    (labels ( (parse-rtm-tree (tree )
                (loop for beat-group in tree
                      if (consp beat-group) 
                      collect
                      (list (first beat-group) (parse-rtm-tree (second beat-group)))
                      else collect 
                      (progn (cond
                              ((null sieve) (- (floor (abs beat-group))))
                              ((and (not begin) (minusp beat-group)) (if mode (float (abs beat-group)) beat-group))
                              ((and (not begin) (floatp beat-group)) (if mode beat-group (- (floor beat-group))))
                              ((and begin (minusp beat-group)) beat-group)
                              ((and begin (floatp beat-group)) (- (floor (abs beat-group))))
                              (t (incf counter)
                                 (cond
                                  ((= counter (first sieve))
                                   (pop sieve) 
                                   (setf begin nil) (abs (floor beat-group)))
                                  ;(if sieve  beat-group (- (abs beat-group))))
                                  (t (if begin (- (abs beat-group)) 
                                         (if mode (float beat-group) (- (abs beat-group))))))))))))
      (parse-rtm-tree tree))))



#|
(defun mk-rtm-with-new-beats (meas-line new-beats)
  (let ((signs  (first (pw::rtm-signs (pw::list! meas-line))))
        (tempi (first (pw::rtm-tempo  (pw::list! meas-line)))))
    (build-rtm-voice tempi signs new-beats)))
|#




#|
(defun build-rtm-voice (tempi in-signs beats)
  (let* ((chords ())
         measures
         (x-signs (pw::expand-lists in-signs))
         (signs (if (consp (car x-signs)) x-signs (list x-signs)))
         (default-sign (car (last signs)))
         (default-beat (car (last beats)))
         (def-tempo (car (last tempi)))
         (meas-beat-count (caar signs))
         beat-objs
         (meas-sign (pop signs))
         the-beat already-warned)
    (when (and (numberp default-beat) (minusp default-beat))
      (setq default-beat (abs default-beat)))
    (while (or (setq the-beat (pop beats)) chords)
      (unless the-beat
        (setq the-beat default-beat))
      (cond ( ; (zerop meas-beat-count)
             
             ; ----- GAS 110493. alllow for small rounding error
             (or (zerop meas-beat-count) (< (abs meas-beat-count) 1.0e-10))
             (setf meas-beat-count 0)
             ; ----- GAS 110493. alllow for small rounding error
             
             (push (pw::make-measure (pw::low-of meas-sign) (nreverse beat-objs) 
                                     (or (pop tempi) def-tempo)) measures)
             (setq meas-sign (or (pop signs) default-sign) meas-beat-count (car meas-sign)
                   beat-objs nil))
            ((minusp meas-beat-count)
             (if (cdr signs)
               (error "measures ~S and beats do not agree" meas-sign)
               (progn
                 (unless already-warned
                   (ed-beep)
                   (warn "measures ~S and beats do not agree. Measure(s) will be changed" meas-sign)
                   (setq already-warned t))
                 (push (pw::make-measure (pw::low-of meas-sign) (nreverse beat-objs) 
                                         (or (pop tempi) def-tempo)) measures)
                 (setq meas-sign (or (pop signs) default-sign) meas-beat-count (car meas-sign)
                       beat-objs nil)))))
      (push (multiple-value-bind (beat rest-chords)
                                 (pw::beat-constructor (pw::beats-of the-beat) (pw::division-of the-beat) chords)
              (setq chords rest-chords) beat) beat-objs)
      (decf meas-beat-count (pw::beats-of the-beat)))
    (if beat-objs (push (pw::make-measure (pw::low-of meas-sign) (nreverse beat-objs) def-tempo) measures))
    (setq measures (nreverse measures))
    ;  (when (not (wptr (application-object self)))
    ;    (setf (application-object self) (make-application-object self))
    ;    (put-window-state self (application-object self) (window-state self)))
    (unless (listp measures) (setq measures (list measures)))
    ;(setf (measures measure-line) measures)
    ;;;to be refined for keeping note durs!!
    (let ((result (make-instance 'pw::C-measure-line :measures measures)))
      (pw::rtm-dim result 7)
      result)
    ))
|#


(defun partition2 (p)
  (let  ((prefix (string (pop p))))
    (let ((nom1 nil))
      (let ((nom nil))
        (let ((a nil))
          (let ((n (long p))) 
            (dotimes (b (1- ( round (exp (* n (log 2))))))       ;   2^n
              (setf a (1+ b))
              (let ((ligne))      
                (let ((res
                       (let ((y 
                              (do ((z nil))
                                  ((< a 2) z)
                                (push  (mod a 2) z)
                                (setf a (floor (/ a 2)))) 
                              ))
                         (push a y)
                         (dotimes (i (- n (long y)))
                           (push 0 y))
                         (setf a (1+ (long y)))
                         (do ((i 1 (1+ i)) (z (list nil nil)))
                             ((equal i a) (list (reverse (first z)) (reverse (second z))))
                           (if (equal (pop y) 1) (setf z (list (cons (1- i) (first z)) (second z)))
                               (setf z (list (first z) (cons (1- i) (second z))))))
                         )))
                  (setf nom1 (reverse (first res)))
                  (setf res (list (compose (first res) p) (second res)))
                  (setf res (list (first res) (compose (second res) p)))
                  (setf res (list (transfo (first res) '-) (transfo (second res) '+)))
                  (if (null (first res)) (setf ligne nil)
                      (if (null (second res)) (setf ligne (first res))
                          (if (null (second (first res))) (setf ligne (list (first (first res)) '/ (second res)))
                              (if (null (second (second res))) (setf ligne (list (first   res ) '/ (first (second res))))
                                  (setf ligne (list (first res) '/ (second res))))))) 
                  (push '= ligne)
                  (setf nom
                        (do ((nom2 ""))
                            ((null nom1) nom2)
                          (setf nom2 (format () "~A~A"  (1+ (pop nom1)) nom2)))
                        ) 
                  (setf nom (format () "~A~A"  prefix nom)) 
                  (push (intern nom ::om) ligne )
                  (push '% ligne)
                  (print ligne) 
                  )))))))))

(defun nuances ()
  (let ((nuance
         '(( fff 135)
           (   ff 112)
           (   f  80)
           (   mf 60)
           (   mp 40)
           (   p  20)
           (   pp 6))))
    (let ((z1 (second (assoc 'pp nuance)))
          (z2 (second (assoc 'mp nuance)))
          (z3 (second (assoc 'mf nuance)))
          (z4 (second (assoc 'p nuance)))
          (z5 (second (assoc 'p nuance)))
          (z6 (second (assoc 'mf nuance)))
          (z7 (second (assoc 'f nuance)))
          (z8 (second (assoc 'mp nuance)))
          (z9 (second (assoc 'mp nuance)))
          (z10 (second (assoc 'f nuance)))
          (z11 (second (assoc 'mp nuance)))
          (z12 (second (assoc 'pp nuance)))
          (z13 (second (assoc 'pp nuance)))
          (z14 (second (assoc 'mf nuance)))
          (z15 (second (assoc 'f nuance)))
          (z16 (second (assoc 'p nuance)))
          (z17 (second (assoc 'p nuance)))
          (z18 (second (assoc 'f nuance)))
          (z19 (second (assoc 'mf nuance)))
          (z20 (second (assoc 'pp nuance)))
          (z21 (second (assoc 'p nuance)))
          (z22 (second (assoc 'ff nuance)))
          (z23 (second (assoc 'ff nuance)))
          (z24 (second (assoc 'mf nuance)))
          (z25 (second (assoc 'mf nuance)))
          (z26 (second (assoc 'ff nuance)))
          (z27 (second (assoc 'f nuance)))
          (z28 (second (assoc 'fff nuance)))
          (z nil))
      (do ((i 3640 (1+ i)))
          ((equal i 4088))
        (cond
         ((and (> i 3639) (< i 3652))
          (progn 
            (setf z1 (+ z1 (/ (- z2 z1) (- 3651 3640))))
            (push (floor z1) z)))  
         ((and (> i 3671) (< i 3684))
          (progn 
            (setf z3 (+ z3 (/ (- z4 z3) (- 3683 3672))))
            (push (floor z3) z)))  
         ((and (> i 3703) (< i 3717))
          (progn 
            (setf z5 (+ z5 (/ (- z6 z5) (- 3716 3704))))
            (push (floor z5) z)))  
         ((and (> i 3735) (< i 3750))
          (progn 
            (setf z7 (+ z7 (/ (- z8 z7) (-  3749 3736))))
            (push (floor z7) z)))  
         ((and (> i 3767) (< i 3785))
          (progn 
            (setf z9 (+ z9 (/ (- z10 z9) (-  3784 3768))))
            (push (floor z9) z)))  
         ((and (> i 3799) (< i 3816))
          (progn 
            (setf z11 (+ z11 (/ (- z12 z11) (-  3815 3800))))
            (push (floor z11) z)))  
         ((and (> i 3831) (< i 3849))
          (progn 
            (setf z13 (+ z13 (/ (- z14 z13) (-  3848 3832))))
            (push (floor z13) z)))  
         ((and (> i 3863) (< i 3883))
          (progn 
            (setf z15 (+ z15 (/ (- z16 z15) (- 3882 3864 ))))
            (push (floor z15) z)))  
         ((and (> i 3895) (< i 3917))
          (progn 
            (setf z17 (+ z17 (/ (- z18 z17) (- 3916 3896 ))))
            (push (floor z17) z)))  
         ((and (> i 3927) (< i 3951))
          (progn 
            (setf z19 (+ z19 (/ (- z20 z19) (- 3950 3928 ))))
            (push (floor z19) z)))  
         ((and (> i 3959) (< i 3985))
          (progn 
            (setf z21 (+ z21 (/ (- z22 z21) (- 3984 3960))))
            (push (floor z21) z)))  
         ((and (> i 3991) (< i 4019))
          (progn 
            (setf z23 (+ z23 (/ (- z24 z23) (- 3992 4018))))
            (push (floor z23) z)))  
         ((and (> i 4023) (< i 4053))
          (progn 
            (setf z25 (+ z25 (/ (- z26 z25) (- 4052 4024))))
            (push (floor z25) z)))  
         ((and (> i 4055) (< i 4087))
          (progn 
            (setf z27 (+ z27 (/ (- z28 z27) (- 4086 4056))))
            (push (floor z27) z)))   
         (t (push i z))))
      (reverse z))))

;;;;;; OM Interface



(om::defmethod! lc ((prog-lc  list))


   :initvals '('()) 
   :indoc '("prog-lc") 
   :icon 250
   :doc  "Computes a set of sieves (cribles) from a set of sieve expressions contained
in a 'text-win' box connected to it. 
A sieve is a list of increasing positive integers.
See the tutorial for examples of 
the language (lc) used for writing sieve expressions.
Once evaluated, all the symbols that appear on the left side of the '=' operator
(e.g. c1 in the expression 'c1 = c2 + c3') inside the text-win are defined and can be used 
in the 'eval-crible' 'crible-list' and 'crible-rtm' modules, in the 'crible' parameter.

simple sieve : (step offset begin end)
example : c = (2 0 0 8) defines a sieve with a period 2 between 0 and 8:  (0 2 4 6 8)
c = (2 1 4 10) defines (5 7 9)

binary operators : 

+ union
- intersection
* sieve composition
/ set difference
// set symetrical difference

unary operators : 

c (x) complementary sieve of the sieve 'x'

d(i1 i2 .. in) defines an arbitrary sieve (i1 i2 ... in) with i1,i2... increasing integers

a(s b e) defines a random sieve with step close to 'a', between values 'b' and 'e'

e <lisp form> evaluates <lisp form>
examples :  c = e (append (c1) (reverse (c1)) computes
a palindrome from the sieve c1 and puts it into c. If you use sieve-symbols in <lisp form>
put them between parentheses (e.g. (c1)).


p(s c1 c2 ... cn) where 's' is a symbol, 'c1'...'cn' are previously defined sieves. Computes
a set partition of the set c1 U c2 U ... cn. Then the subsets are put in symbols built from 's'.
Example : after evaluating p(x c1 c2 c3), the symbol x1 (resp. x2, x3) is set to contain
the element of c1 (resp. c2 c3) that are not elements of the 2 other sets. The symbol x12 contains
elements common to c1 and c2 but not members of c3. x13 and x23 follow the same model.
x123 is the intersection of the three sets.


parameters : 

list : the output of a text-win box

output : 

nil
"

        
  (setf prog-lc (put-in-package prog-lc ::om))
  (let  ((y 2))
    (while y
      (setf y (if  (or (equal prog-lc '(%)) (not (equal (pop prog-lc) '%)))
                nil
                (if (equal (first prog-lc) '%)
                  nil
                  (let ((ret ()))      
                    (do
                      ((pop prog-lc))
                      ((or (null prog-lc) (equal (first prog-lc) '%)))
                      (push (pop prog-lc) ret)
                      )
                    (reverse ret) 
                    ))))
      
      (if (traiter_commentaire y)  
        (progn
          (funcall (setf (fdefinition (first y)) (eval (list 'function (list 'lambda nil  (inpre (cdr (cdr y))))))))
          (if (not (null (first y))) (format t (string  (first y))))
          )
        )
      )))


(om::defmethod! crible-list ((list list )  (crible list ))

   :initvals '('() '()) 
   :indoc '("list" "crible") 
   :icon 250
   :doc "Apply a sieve defined with the 'lc' box to any list.

parameters:

crible : a symbol or a list of symbols defined with a 'lc' box
list : a list

output : 

a list."

  (if (symbolp crible) (crible list crible)
      (mapcar #'(lambda (c)  (crible list c)) crible)))











;-------------------------------GET-MIDICS-FROM-objs---------------------------
;-------------------------------------------------------------------------------



(om::defmethod! getmidics ((self poly))
(mapcar #'(lambda (x) (getmidics x))
          (inside self)))

(om::defmethod! getmidics ((self voice))
(mapcar #'(lambda (x) (getmidics x))
          (inside self)))

(om::defmethod! getmidics ((self chord-seq))
(mapcar #'(lambda (x) (getmidics x))
          (inside self)))

(om::defmethod! getmidics ((self multi-seq))
(mapcar #'(lambda (x) (getmidics x))
          (inside self)))

(om::defmethod! getmidics ((self measure))
(mapcar #'(lambda (x) (getmidics x))
          (inside self)))

(om::defmethod! getmidics ((self chord))
(mapcar #'(lambda (x) (getmidics x))
          (inside self)))

(om::defmethod! getmidics ((self rest))
())

(om::defmethod! getmidics ((self group))
(mapcar #'(lambda (x) (getmidics x))
          (inside self)))

(om::defmethod! getmidics ((self continuation-chord))
())


(om::defmethod! getmidics ((self note))
(midic self))


;Gives the same as above for VOICE & POLY but without leaves 
; works for chords!!!!

(om::defmethod! getpitch ((self voice))
(let ((pitchtree (chords self)))
  (mapcar #'(lambda (x) (getmidics x)) pitchtree)))


(om::defmethod! getpitch ((self poly))
   (let ((voices (inside self)))
(mapcar #'(lambda (x) (getpitch x)) voices)))












;--------------------------------------crible-voice-------------------------------------------



;----------------------------------------grouper-utility------------------------------------
; This functions groups all tied notes and rest into a single one
; in order to avoid  a bug in Cmn and to output a clear score


;Already defined in Karim.lisp

;=====================================reducetree==============================================

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

(om::defmethod! reducetree ((tree t))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("tree")
   :icon 250
   :doc "reduces and simplifies a tree by concatenating consecutive rests and floats
into a single correct note"
(grouper3 (grouper2 (grouper1 tree))))





(defun recombine-tree-norm (pulses groups signatures)
  (mapcar #'(lambda (pulses)
              (list '? (mapcar #'(lambda (x y) (list x y)) signatures 
                               (group-list pulses groups 'linear))))
          pulses))

(defun recombine-tree-opt (pulses groups signatures)
  (mapcar #'(lambda (pulses)
              (list '? (mapcar #'(lambda (x y) (reducetree (list x y))) signatures 
                               (group-list pulses groups 'linear))))
          pulses))


(om::defmethod! crible-voice ((metrique t)
                                 (crible list)
                                 (option symbol)
                                 (mode symbol)
                                 &optional (midics '()))
   
   
   
   :initvals '( t '() 'silence 'opt '())
   :menuins '((2 (("silence" 'silence) ("liaison" 'liaison)))
             (3 (("opt" 'opt) ("norm" 'norm)))) 
   :indoc '("metrique" "crible" "option" "mode" "midics") 
   :icon 250
   :doc "
Apply a sieve defined with the 'lc' box to a metric/rythmic structure.

parameters : 

metrique : a c-measure-line (output from a 'rtm' box)
crible : a symbol or a list of symbols defined with a 'lc' box
option: menu, 'silence' means impulsions ignored by the sieve are made silent,
'liaison' means a selected impulsion is linked to following until next selected impulsion

output :

a c-measure-line or a list of c-measure line, depending on the 'crible' parameter.
connect to a 'rtm' or 'poly-rtm' depending on the type of output.
 "
   
   
   
   (when (and metrique crible)
     (let* ((tree (if (listp metrique) metrique (tree metrique)))
            (length-crible (length crible))
            (pitches (cond
                      ((and (null midics) (listp metrique)) 
                       (repeat-n (getpitch metrique) length-crible))
                      ((and (null midics) (not (listp metrique))) (repeat-n '(6000) length-crible))
                      ((and (atom midics) (listp metrique)) 
                       (repeat-n midics length-crible))
                      ((and (atom midics) (not (listp metrique))) (repeat-n midics length-crible))
                      (t midics)))
            (split-tree (mat-trans (cadr tree)))
            (pulses (flat-once (second split-tree)))
            (pulse-segments (mapcar #'(lambda (x) (length x))(second split-tree)))
            (meas-sign (first split-tree))
            single res )
       
       (setf b> (long-rtm   pulses ))
       (unless (listp crible) (setf single t))
       (setf crible (om::list! crible))
       (setf res
             (setf res (case mode
                         (opt (recombine-tree-opt 
                               (mapcar #'(lambda (one-crible)
                                           (case option
                                             (silence (sieve-rtm-tree pulses (eval-simple one-crible) nil))
                                             (liaison (sieve-rtm-tree pulses (eval-simple one-crible) t))))
                                       crible) pulse-segments meas-sign))
                         (norm (recombine-tree-norm 
                                (mapcar #'(lambda (one-crible)
                                            (case option
                                              (silence (sieve-rtm-tree pulses (eval-simple one-crible) nil))
                                              (liaison (sieve-rtm-tree pulses (eval-simple one-crible) t))))
                                        crible) pulse-segments meas-sign)))))
       
       
       (if (= 1 (length crible)) 
         (make-instance 'poly
           :voices (mapcar #'(lambda (x) (make-instance 'voice :tree x :chords pitches)) res))
         (make-instance 'poly
           :voices (mapcar #'(lambda (x y) (make-instance 'voice :tree x :chords y)) res pitches))
         ))))




(om::defmethod! eval-crible ((crible list))


   :initvals '( t )
   :indoc '("list") 
   :icon 250
   :doc "Evaluates a symbol or a list of symbols defined with the 'lc' box.

parameter:

crible : a symbol or a list of symbols

output :

a sieve (a list of increasing integers) or a list of sieve
"        
  (if (symbolp crible) (eval-simple crible)
      (mapcar #'(lambda (c)  (eval-simple c)) crible)))


(om::defmethod! crible-pitches ((cribles list) (pitches list))
   :initvals '( '() '(6000 7200))
   :indoc '("cribles" "pitches") 
   :icon 250
   :doc " distributes from a given list of cribles each note to a crible
from a given list of <pitches>."
   
   (remove nil
           (let*((crible (eval-crible cribles))
                 (notes pitches)
                 (lst (repeat-n '() (1+ (list-max (flat crible))))))
             (loop for i in crible
                   for a in notes
                   do (loop for x in i
                            do (push a (nth x lst))))
             lst)))



;=======================================PERSONNAL METHODS=====================================
(om::defmethod! rytm-pair ((lst list))
  :initvals (list '(1 2)) 
  :indoc '("list")
  :icon 250
  :doc "donne des paires periodiques"
  (om::x->dx 
   (om::remove-dup 
    (om::sort. 
     (om::flat 
      (mapcar #'(lambda (x) (om::arithm-ser 0  (apply '* lst) x) ) lst))) 'eq 1)))


    