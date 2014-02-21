;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  IRCAM - Music Represetation Team
;  Copyright (C) 1997-2010 IRCAM-Centre Georges Pompidou, Paris, France.
;
;  Harmonic Project
;  Tonal transpositions and harmonic progressions
;  C. Truchet, J. Bresson
;=========================================================================

(in-package :om)

(defun mktonalite (mode note alt)
  (let ((res 
         (make-instance 'tonalite)))
    (setf (tonmidi res) (notes->midic note alt))
    (setf (tonnote res) note)
    (setf (tonalt res) alt)
    (setf (mode res) mode)
    res))


;;;========================
;;; TRANSPOSITION EN DEGRES


(defmethod transpose-tonal ((self note) ecartdeg)
  (let* ((tonalite (get-tonalite self))
         mode lmode degre)
    (if (and tonalite (tonal-values self) 
             (setf degre (get-degre self)))
      (let* ((mode (mode tonalite))
             (lmode (length mode))
             (nouveaudegre (degre- (degre+ degre ecartdeg) 1))
             (nombreoctaves (div nouveaudegre lmode))
             (interv (- (+ (* nombreoctaves 1200)
                           (nth (degremod nouveaudegre lmode)
                                mode)
                           )
                        (nth (degremod (degre- degre 1) lmode) mode)))
             (nouveaumidic (+ (midic self) interv))
             (new (clone self)))
        (setf (midic new) nouveaumidic)
        new)
      self)))

(defmethod transpose-tonal ((self chord) ecartdeg)
  (let ((tonalite (get-tonalite self))
        (new (clone self)))
    (when tonalite
      (let* ((mode (mode tonalite))
             (lmode (length mode))
             (notes (inside self))
             (nouveau-lmidic (loop for el in notes
                              collect (midic (transpose-tonal el ecartdeg))))
             )
        (setf (lmidic new) nouveau-lmidic)))
    new))

;; ajout pour transpose les chord-seq
(defmethod transpose-tonal ((self chord-seq) ecartdeg)
  (let ((tonalite (get-tonalite self))
        (new (clone self)))
    (when tonalite
      (let* ((mode (mode tonalite))
             (lmode (length mode))
             (accords (inside self))
             (nouveau-lmidic (loop for el in accords
                              collect (lmidic (transpose-tonal el ecartdeg))))
             )
        (setf (lmidic new) nouveau-lmidic)))
    new))

;; NEW 26 MARS

(defun complement-deg (degre mode)
  (if (equal mode *mineur*)
    (cond ((equal degre '(6 a)) 3)
          ((equal degre '(6 d)) 3)
          ((equal degre '(7 a)) 2)
          ((equal degre '(7 d)) 2)
          ((equal degre 3) '(6 d))
          ((equal degre 2) '(7 d))
          (t (- 9 degre)))
    (- 9 degre)))

(defun versdescendant (degre)
  (if (listp degre) (car degre) degre))

(defun transposedegre (degre interv mode)
   (let* ((nouveaudegre (degre+ degre (- interv 2)))
          (nouveadegmode (degre+ 1 (degremod nouveaudegre 7)))
          (degdesc (versdescendant nouveaudegre))
          (nboct (div degdesc 7)))
     (if (and (equal mode *mineur*) (or (= nouveadegmode 6) (= nouveadegmode 7)))
       (if (listp degre)
         (list (list nouveadegmode (cadr degre)) nboct)
         (list (list nouveadegmode 'd) nboct))
       (list nouveadegmode nboct))))


(defun mode-degre (degre mode)
  (let ((autredeg (deploiemineur degre)))
    (nth (1- autredeg) mode)))

(defun tonreference (midic degre mode)
  (- midic (mode-degre degre mode)))

;; parametres :
;; interv est un intervalle musical, 1 pour unisson, 2 pour seconde, 2 pour tierce etc
;; nombreoctaves est le nombre d'octaves (positif ou negatif)
;; ascendant vaut vrai si on monte, faux si on descend
;; attention, si 'lobject n'est pas tonal on transpose d'un nombre de demi tons (donnŽ par interv)

(defmethod newtranspose-tonal ((self note) interv nombreoctaves ascendant?)
  (let* ((tonalite (or (get-tonalite self)
                       (default-tonality)))
         (degre (get-degre self)))
    (if (and tonalite (tonal-values self));;; degre)
      (if (not ascendant?) 
        (newtranspose-tonal self (complement-deg interv (mode tonalite)) (1- nombreoctaves) t)
        (let* ((mode (mode tonalite))
               (new (clone self))
               nouveaumidic)
          (if degre 
            (let* ((newdegoct (transposedegre degre interv mode))
                  (midic (midic self))
                  (nouveaudegre (car newdegoct))
                  (nboct (cadr newdegoct)))
              (setf nouveaumidic (+ (tonreference midic degre mode)
                                   (* 1200 (+ nombreoctaves nboct))
                                   (mode-degre nouveaudegre mode)))
              )
            ;;; jean
            (let* ((diff-init (- (midic self) (tonmidi tonalite)))
                   (newdegoct (transposedegre 1 interv mode))
                   (nouveaudegre (car newdegoct))
                   (midic (midic self))
                   (nboct (cadr newdegoct))
                   (nouveaumidic1 (+ (tonreference (tonmidi tonalite) 1 mode)
                                    (* 1200 (+ nombreoctaves nboct))
                                    (mode-degre nouveaudegre mode))))
              (setf nouveaumidic (+ nouveaumidic1 diff-init)) 
              )
            ;;;
            )
          (setf (midic new) nouveaumidic)
          new))
      ;; si l'objet n'est pas tonal on transpose d'un multiple deu demi-ton (?!)
      (if (not ascendant?) 
        (newtranspose-tonal self (- 12 interv) (1- nombreoctaves) t)
        (let ((new (clone self)))
          (setf (midic new) (+ (midic self) (* interv 100) (* nombreoctaves 1200)))
          new)
        ))))

            

(defmethod newtranspose-tonal ((self chord) interv nombreoctaves ascendant?)
  (let ((tonalite (get-tonalite self))
        (new (clone self)))
    ;(when tonalite   ;;; now notes transposes in C maj by default...
    (setf (lmidic new) (loop for el in (inside self)
                             collect (midic (newtranspose-tonal el interv nombreoctaves ascendant?))))
    (actualise-tonalite new)
    new))

(defmethod newtranspose-tonal ((self chord-seq) interv nombreoctaves ascendant?)
  (let ((tonalite (get-tonalite self))
        (new (clone self)))
    ;(when tonalite   ;;; now notes transposes in C maj by default...
    (setf (lmidic new) (loop for el in (inside self)
                             collect (lmidic (newtranspose-tonal el interv nombreoctaves ascendant?))))
    new))

;; ;;


;;;============
;;; MODULATION


;;ATTENTION TONMIDI DOIT ETRE A LA BONNE VALEUR DANS LA NOUVELLE TONALITE - A VERIFIER SI C'EST TOUJOURS LE CAS

(defmethod transpose-modulant ((self note) (nouvelletonalite tonalite))
  (let ((tself (get-tonalite self)))
    (if tself
      (let* ((new (clone self))
             (diff (- (midic self) (tonmidi tself)))
             (nouveaumidic (+ (tonmidi nouvelletonalite) diff)))
        (setf (midic new) nouveaumidic)
       ; (set-tonalite new nouvelletonalite)
        (actualise-tonalite new)
        new)
      self)))


(defmethod transpose-modulant ((self chord) (nouvelletonalite tonalite))
  (let* ((notes (inside self))
         (nouvellesnotes (mapcar #'(lambda (x) (transpose-modulant x nouvelletonalite)) notes))
         (new (clone self)))
    (setf (inside new) nouvellesnotes)
    (set-tonalite new nouvelletonalite)
    (actualise-tonalite new)
    new))

;;  si mutation les accords transposes-mutes gardent leur ancienne tonalite (ils sont transposes tonal)
;; l'entree mutation? est un booleen qui dit si on veut muter ou pas
(defmethod transpose-modulantmutation ((self chord-seq) (nouvelletonalite tonalite) &optional mutation?)
  (let* ((accords (inside self)))
    (if (and mutation? (not (null accords)) (not (null (cdr accords))))
      (let* ((longueur (length accords))
             (deuxderniersdeg (list (car (get-degre (nth (- longueur 2) accords))) 
                                    (car (get-degre (nth (- longueur 1) accords)))))
             (deuxpremiersdeg (list (car (get-degre (car accords))) 
                                    (car (get-degre (cadr accords)))))
             (new (clone self))
             (nouveauxaccords (mapcar #'(lambda (x) (transpose-modulant x nouvelletonalite)) accords))
             (nouveauxaccords1 (progn (if (equal deuxpremiersdeg '(1 5))
                                        (progn
                                          (setf (car nouveauxaccords) (transpose-tonal (car accords) 4))
                                          (setf (cadr nouveauxaccords) (transpose-tonal (cadr accords) 3))))
                                      nouveauxaccords))
             (nouveauxaccords2 (progn (if (equal deuxderniersdeg '(1 5))
                                        (progn 
                                          (setf (nth (- longueur 2) nouveauxaccords1) (transpose-tonal (nth (- longueur 2) accords) 4))
                                          (setf (nth (- longueur 1) nouveauxaccords1) (transpose-tonal (nth (- longueur 1) accords) 3))))
                                      nouveauxaccords1)))
        ;(print `(premiers ,deuxpremiersdeg))
        ;(print `(derniers ,deuxderniersdeg))
        (setf (inside new) nouveauxaccords)
        (set-tonalite new nouvelletonalite)
        (actualise-tonalite new)
        new)
      (let* ((nouveauxaccords (mapcar #'(lambda (x) (transpose-modulant x nouvelletonalite)) accords))
             (new (clone self)))
        (setf (inside new) nouveauxaccords)
        (set-tonalite new nouvelletonalite)
        (actualise-tonalite new)
        new))))


;;DEBUTMODIF 26 MARS

(defmethod newtranspose-modulant ((self note) (nouvelletonalite tonalite) nombreoctaves)
  (let ((tself (get-tonalite self)))
    (if tself
      (let* ((new (clone self))
             (diff (- (midic self) (tonmidi tself)))
             (nouveautonmidi (if (> (tonmidi nouvelletonalite) (tonmidi tself)) (tonmidi nouvelletonalite)
                                 (+ 1200 (tonmidi nouvelletonalite))))
             (nouveaumidic1 (+ nouveautonmidi diff))
             (nouveaumidic2 (if (> nouveaumidic1 (midic self)) nouveaumidic1
                               (+ nouveaumidic1 1200)))
             (nouveaumidic (+ nouveaumidic2 (* nombreoctaves 1200))))
        
        
        (setf (midic new) nouveaumidic)
        
        (actualise-tonalite new)
        new)
      self)))


;;; NEW JEAN reformate le degre pour la nouvelle tonalite
(defun convert-degre (deg mode)
  (if (equal *mineur* mode)
    (if (listp deg) 
      deg 
      (if (or (= deg 6) (= deg 7)) 
        (list deg 'd)
        deg)
      )
    (if (listp deg) (car deg) deg)))

;;; NEW JEAN 
(defmethod newtranspose-modulant ((self note) (nouvelletonalite tonalite) nombreoctaves)
  (let ((tself (or (get-tonalite self)
                   (default-tonality))))
    (if tself
      (let* ((new (clone self))
             ;;;
             (deg (get-degre self))
             (base1 (if deg 
                      (- (midic self) (degre->midic deg (mode tself)))
                      (midic self)))
             (diff (- base1 (tonmidi tself)))
             ;;;
             ;;(diff (- (midic self) (tonmidi tself)))
             ;(nouveautonmidi (if (> (tonmidi nouvelletonalite) (tonmidi tself)) (tonmidi nouvelletonalite)
             ;                    (+ 1200 (tonmidi nouvelletonalite))))
             (nouveautonmidi (tonmidi nouvelletonalite))
             (nouveaumidic1 (+ nouveautonmidi diff))
             (nouveaumidic2 (if (> nouveaumidic1 (midic self)) nouveaumidic1
                               (+ nouveaumidic1 1200)))
             (nouveaumidic (+ nouveaumidic1 (* nombreoctaves 1200))))
        (when deg
          (setf deg (convert-degre deg (mode nouvelletonalite)))
          ;(print (list deg nouveaumidic))
          (setf nouveaumidic (+ nouveaumidic (degre->midic deg (mode nouvelletonalite)))))
        
        (setf (midic new) nouveaumidic)
        
        (actualise-tonalite new)
        new)
      self)))





(defmethod newtranspose-modulant ((self chord) (nouvelletonalite tonalite) nombreoctaves)
  (let* ((notes (inside self))
         (nouvellesnotes (mapcar #'(lambda (x) (newtranspose-modulant x nouvelletonalite nombreoctaves)) notes))
         (new (clone self)))
    (setf (inside new) nouvellesnotes)
    (set-tonalite new nouvelletonalite)
    (actualise-tonalite new)
    new))

;;  si mutation les accords newtransposes-mutes gardent leur ancienne tonalite (ils sont transposes tonal)
;; l'entree mutation? est un booleen qui dit si on veut muter ou pas
(defmethod newtranspose-modulantmutation ((self chord-seq) (nouvelletonalite tonalite) nombreoctaves &optional mutation?)
  (let* ((accords (inside self)))
    (if (and mutation? (not (null accords)) (not (null (cdr accords))))
      (let* ((longueur (length accords))
             (deuxderniersdeg (list (car (get-degre (nth (- longueur 2) accords))) 
                                    (car (get-degre (nth (- longueur 1) accords)))))
             (deuxpremiersdeg (list (car (get-degre (car accords))) 
                                    (car (get-degre (cadr accords)))))
             (new (clone self))
             (nouveauxaccords (mapcar #'(lambda (x) (newtranspose-modulant x nouvelletonalite nombreoctaves)) accords))
             (nouveauxaccords1 (progn (if (and (equal deuxpremiersdeg '(1 5)) (= (length (car accords)) 1)
                                               (= (length (cadr accords)) 1))
                                        (progn
                                          (setf (car nouveauxaccords) (newtranspose-tonal (car accords) 4))
                                          (setf (cadr nouveauxaccords) (newtranspose-tonal (cadr accords) 3))))
                                      nouveauxaccords))
             (nouveauxaccords2 (progn (if (and (equal deuxderniersdeg '(1 5)) (= (length (nth (- longueur 2) accords)) 1)
                                               (= (length (nth (- longueur 1) accords)) 1))
                                        (progn 
                                          (setf (nth (- longueur 2) nouveauxaccords1) (newtranspose-tonal (nth (- longueur 2) accords) 4))
                                          (setf (nth (- longueur 1) nouveauxaccords1) (newtranspose-tonal (nth (- longueur 1) accords) 3))))
                                      nouveauxaccords1)))
        ;(print `(premiers ,deuxpremiersdeg))
        ;(print `(derniers ,deuxderniersdeg))
        (setf (inside new) nouveauxaccords)
        (set-tonalite new nouvelletonalite)
        (actualise-tonalite new)
        new)
      (let* ((nouveauxaccords (mapcar #'(lambda (x) (newtranspose-modulant x nouvelletonalite nombreoctaves)) accords))
             (new (clone self)))
        (setf (inside new) nouveauxaccords)
        (set-tonalite new nouvelletonalite)
        (actualise-tonalite new)
        new))))

(defmethod intervalle->tonalite ((self tonalite) intervmidi)
  (let* ((res (make-instance 'tonalite))
         (nouveaumidic (+ (tonmidi self) intervmidi))
         (nouvellenote (midic->notes nouveaumidic)))
    ;(print nouvellenote)
    (setf (tonnote res) (car nouvellenote))
    (setf (tonalt res) (cadr nouvellenote))
    (setf (mode res) (mode self))
    (setf (tonmidi res) (notes->midic (tonnote res) (tonalt res)))
    res))

;; fonction transposition modulante quand ce qui est donne est un intervalle, sous la forme :
;; interv est un intervalle en nombre de demi-tons
;; nombreoctaves est le nombre d'octaves (positif ou negatif)
;; ascendant vaut vrai si on monte, faux si on descend(defmethod newtransposemodulant ((self chord-seq) interv nombreoctaves ascendant?)
  

(defmethod newtransposemodulantintervalle ((self note) interv nombreoctaves ascendant)
  (if ascendant
    (newtranspose-modulant self (intervalle->tonalite (get-tonalite self) (* interv 100))  nombreoctaves)
    (newtranspose-modulant self (intervalle->tonalite (get-tonalite self) (* (- 12 interv) 100)) nombreoctaves)))

(defmethod newtransposemodulantintervalle ((self chord) interv nombreoctaves ascendant)
  (if ascendant
    (newtranspose-modulant self (intervalle->tonalite (get-tonalite self) (* interv 100)) nombreoctaves)
    (newtranspose-modulant self (intervalle->tonalite (get-tonalite self) (* (- 12 interv) 100)) nombreoctaves)))

(defmethod newtransposemodulantintervallemutation ((self chord-seq) interv nombreoctaves ascendant &optional mutation?)
  (if ascendant
    (newtranspose-modulantmutation self (intervalle->tonalite (get-tonalite self) (* interv 100)) nombreoctaves mutation?)
    (newtranspose-modulantmutation self (intervalle->tonalite (get-tonalite self) (* (- 12 interv) 100)) nombreoctaves mutation?)))

;;FIN MODIF 26 MARS


;;;===============
;;; MARCHES


;; sert a calculer les tonalites successives dans le cas des marches modulantes

(defmethod transposetonalite ((self tonalite) ecart)
  (let* (;(ecart2 (if (< ecart 0) (+ 7 ecart) ecart))
         (ecartmod (mod ecart 7))
         (ecartdiv (div ecart 7))
         (ecartmidi (mode-degre (degre+ 1 (abs ecartmod)) (mode self)))
         (nomnote (midic->notes (+ (tonmidi self) ecartmidi)))
         (newtonalite (make-instance 'tonalite
                        :tonnote (car nomnote)
                        :tonalt (cadr nomnote)
                        :mode (mode self)))
         (newtonmidi  (+ (* 1200 ecartdiv)(apply #'notes->midic nomnote))))
    ;;(print `(ecartmod ,ecartmod ecartdiv ,ecartdiv))
    (setf (tonmidi newtonalite) newtonmidi)
    (list newtonalite (> (tonmidi self) newtonmidi))
    ))


(defmethod marche-harmonie-old ((self chord-seq) longueur &optional (intdeg 1) modulante?)
  (if modulante?
    (let* ((accords (inside self))
           (tself (tonalite self))
           (nouveauxaccords
            (loop for i from 1 to longueur
                  append (loop for el in accords
                               collect (transpose-modulant el (transposetonalite tself (* 100 i intdeg))))))
           (chord-list (append accords nouveauxaccords)))
      (setf new-cseq (objfromobjs chord-list (make-instance 'chord-seq)))
      ;; ici : la tonalitŽ pour le nouveau chord-seq
      (set-tonalite new-cseq (tonalite self))
      new-cseq)

    (let* ((accords (inside self))
           (nouveauxaccords
            (loop for i from 1 to longueur
                  append (loop for el in accords
                               collect (transpose-tonal el (* i intdeg)))))
           (chord-list (append accords nouveauxaccords)))
      (setf new-cseq (objfromobjs chord-list (make-instance 'chord-seq)))
      (set-tonalite new-cseq (tonalite self))
      new-cseq)))


(defun signe (n)
  (if (>= n 0) 1 -1))

(defmethod* concat-juste-apres ((s1 chord-seq) (s2 chord-seq))
  :initvals (list (make-instance 'chord-seq) (make-instance 'chord-seq)) 
  :indoc '("a music sequence" "a music sequence")
  :icon 230
  :doc "Concatenates 2 music sequences into a new one.
" 
  (let ((rep (make-instance 'chord-seq)))
    (setf (lmidic rep) (append (lmidic s1) (lmidic s2))
          (lonset rep) (append (butlast (lonset s1)) (om+ (butlast (lonset s2)) (get-obj-dur s1)))
          (ldur rep) (append (ldur s1) (ldur s2))
          (lvel rep) (append (lvel s1) (lvel s2))
          (loffset rep) (append (loffset s1) (loffset s2))
          (lchan rep) (append (lchan s1) (lchan s2))
          )
    (loop for chord in (inside rep) 
          for tonalite in (append (loop for c1 in (inside s1) collect (get-tonalite c1)) 
                                  (loop for c2 in (inside s2) collect (get-tonalite c2)))
          do (set-tonalite chord tonalite)) 
    rep))

(defmethod marche-harmonie ((self chord-seq) nb-marches &optional (intdeg 1) modulante? brise?)
  (if modulante?
      
      ;;; MODULANT
    (let* ((accords (inside self))
           (tself (tonalite self))
           (transitoire accords)
           (hautbas t)
           (sens (signe intdeg))
           (nouveauxaccords
            (loop for i from 1 to nb-marches
                  collect (progn (setf hautbas (not hautbas))
                                 (setf transitoire 
                                       (loop for el in accords ;;; transitoire
                                             collect (let* ((newtonalite (transposetonalite (get-tonalite el) (* i intdeg)))   ;;;intdeg 
                                                            (newt (car newtonalite))
                                                            (newoct (if (cadr newtonalite)
                                                                      (if (> intdeg 0) 1 0)
                                                                      (if (>= intdeg 0) 0 -1)))
                                                            ;(testprint (print (list (list (tonnote (get-tonalite el)) (tonalt (get-tonalite el)))
                                                            ;                        intdeg (list (tonnote newt) (tonalt newt))  
                                                            ;                        (list (cadr newtonalite) newoct))))
                                                            (newc (if (and brise? hautbas)
                                                                    (newtranspose-modulant el newt (- newoct sens))
                                                                    (newtranspose-modulant el newt newoct))
                                                                  )
                                                            )
                                                       newc)))
                                 (let ((cs (objfromobjs transitoire (make-instance 'chord-seq))))
                                   (setf (ldur cs) (ldur self))
                                   (setf (lonset cs) (lonset self))
                                   (loop for c1 in (inside cs) 
                                         for c2 in transitoire do
                                         (set-tonalite c1 (get-tonalite c2)))
                                   cs)
                                 )))
           (new-cseq (clone self))
           )
      
      (loop for cs in nouveauxaccords do (setf new-cseq (concat-juste-apres new-cseq cs)))
      
      
      (set-tonalite new-cseq (tonalite self))
      new-cseq)
    
    ;;; NON MODULANT
    (let* ((accords (inside self))
           (hautbas t)
           (transitoire accords)
           (nouveauxaccords
            (loop for i from 1 to nb-marches
                collect (progn 
                         (setf hautbas (not hautbas))
                         (setf transitoire
                                      (loop for el in transitoire
                                          collect (let* ((sens (signe intdeg))
                                                         (accord 
                                                          (if (and brise? hautbas) 
                                                              ;; jean 03-04 : +1
                                                              (newtranspose-tonal el (+ intdeg 1) (- 0 sens) t)
                                                            (newtranspose-tonal el (+ intdeg 1) 0 t))))
                                                    (set-tonalite accord (get-tonalite self))
                                                    accord)))
                         (let ((cs (objfromobjs transitoire (make-instance 'chord-seq))))
                           (setf (ldur cs) (ldur self))
                           (setf (lonset cs) (lonset self))
                           cs))
                  ))
           
           (new-cseq (clone self)))
      
      
      (loop for cs in nouveauxaccords do (setf new-cseq (concat-juste-apres new-cseq cs)))
      
      (set-tonalite new-cseq (tonalite self))
      new-cseq)))
