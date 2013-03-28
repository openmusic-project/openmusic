(in-package :om)
 

;*********  Partie TRAMM

(defmethod! k-poids->segments ((liste list)(lon list))
   ;liste est la melodie de poids
   ;les maximum locaux de liste determinent les onsets a selectionner
   :icon 128
   (mapcan #'(lambda(a b) (if (equal 0 b) nil (list a)))
           lon
           (k-element-frontiere (k-parenthesage liste ") poids localement fort" nil "separer val egales"))))


(defun k-verif (&rest list)
  ;:icon 128
  ;:doc "renvoie la liste des associations"

   (let ((resul ""))
     (mapcar #'(lambda (a)
                 (setq resul (string+ resul (str a) (string #\Newline))))
             (mat-trans-bis (remove 'nil list)))
     resul))


(defun k-mat-trans-bis (matrix)
  (apply 'mapcar (cons 'list matrix)))

(defun k-reverseall (liste)
  (unless (null liste) 
    (reverse (mapcar #'(lambda(a) (if (listp a) (k-reverseall a) a)) liste))))


(defun k-element-frontiere (liste)
;renvoie les elements aux frontieres de parenthesage
  (flat (mapcar #'(lambda(a) (if (listp a) 
                               (if (null (cdr a)) a (cons (car a) (make-list (- (length a) 1) :initial-element 0)))
                               a)) liste)))

(defmethod! k-parenthesage ((liste list) &optional place-parentheses liste-positions separer-val)
   :initvals '((1 2 3 2 1) ") poids localement fort" nil "ne pas separer")
   :icon 128
   :menuins '((1
              ((") poids localement fort" ") poids localement fort")
               ("poids localement fort )" "poids localement fort )")))
             (3
              (("ne pas separer" "ne pas separer")
               ("separer val egales" "separer val egales")
               ("separer val non nulles" "separer val non nulles"))))
   
   :indoc '("liste de poids" "position des parentheses en fonction des poids forts"
            "liste de 0 et de 1. Les 1 determinent les positions des parentheses voulues"
            "booleen indiquant si deux memes valeurs consecutives doivent etre separees par des parenthèses"
            "booleen indiquant si deux valeurs non nulles consecutives doivent etre separees par des parenthèses")
   :doc "renvoie le parenthesage de la liste de poids : 
une parenthese fermante se trouve apres chaque poids localement fort"
   (unless (null liste)
     (if (listp (car liste))
       (cons (k-parenthesage (car liste) place-parentheses liste-positions separer-val)
             (k-parenthesage (cdr liste) place-parentheses liste-positions separer-val))
       (progn
         (if (null place-parentheses) (setq place-parentheses ") poids localement fort"))
         (if (null separer-val) (setq separer-val "ne pas separer"))
         (if (equal place-parentheses ") poids localement fort")
           (k-reverseall (k-parenthesage (reverse liste) "poids localement fort )" liste-positions separer-val))
           (let ((max (if (null liste-positions)
                        (if (equal separer-val "separer val non nulles")
                          liste
                          (maximum-local liste 1 "ne pas separer valeurs egales"))
                        liste-positions))
                 (numero 0)(result '(nil)))
             (loop for item in max do
                   (if (and (> item 0) 
                            (if (null (nth (+ numero 1) max)) t 
                                (if (or (equal separer-val "separer val non nulles") (equal separer-val "separer val egales"))
                                  t (= (nth (+ numero 1) max) 0))))
                     (setq result (append (append (butlast result) (list (append (car (last result)) (list (nth numero liste))))) '(nil)))
                     (setq result (append (butlast result) (list (append (car (last result)) (list (nth numero liste)))))))
                   (setq numero (+ numero 1)))
             (remove 'nil result)))))))



(defmethod! k-appliquer-parenthesage (parenthesage (valeurs list))
   :initvals '(nil nil)
   :icon 128
   :indoc '("parenthesage" "liste de valeurs ou termes devant être parentheses")
   :doc "applique le parenthesage 'parenthesage aux termes du champ 'valeur"
   (car (k-appliquer-parenthesage-1 parenthesage valeurs)))


(defmethod! k-appliquer-parenthesage-1 (parenthesage (valeurs list))
   :initvals '(nil nil)
   :icon 128
   :indoc '("parenthesage" "liste de valeurs ou termes devant être parentheses")
   :doc "applique le parenthesage 'parenthesage aux termes du champ 'valeur"
   (cond ((null parenthesage) (list nil valeurs))
         ((null valeurs) (list parenthesage nil))
         ((atom parenthesage) (list (car valeurs) (cdr valeurs)))
         (t (let ((val (list nil valeurs)))
              (list (loop for item in parenthesage collect 
                          (progn (setq val (k-appliquer-parenthesage-1 item (cadr val)))
                                 (car val)))
                    (cadr val))))))



(defmethod! k-melodie-de-poids  ((l-poids list)(melodie list) &rest l-melodie)
   :initvals '(nil (1 2 3 4 1 2))
   :icon 128
   :indoc '("liste des ponderations pour chaque marquage"  "melodie de poids (une par entree)")
   :doc "Prend plusieurs lignes de valeurs de marquages (l-melodie) 
et une liste de coefficients de ponderation (l-poids) pour chacune des lignes de poids
et en renvoie la somme ponderee"
   (let* ((melo (cons melodie l-melodie))
          (poids (if (null l-poids)
                   (create-list (length melo) 1) l-poids))
          (result (somme (om* melo poids))))
     (om-round (if (< (list-min result) 0) (om+ (abs (list-min result)) result) result))))


(defmethod! k-ponderation-maximale ((l-melodies list) &optional poids-min poids-max)
  :initvals '(nil -1 1)
  :icon 128
  :indoc '("liste des melodies de poids" "ponderation minimale autorisee" "ponderation maximale autorisee")
  :doc "Renvoie une ponderation par melodie de poids de sorte que la
melodie issue de la somme de toutes les melodies ait des differences 
maximales entre ses termes consecutifs."

  (unless (null l-melodies)
    (let* ((poid-min (if (null poids-min) (* (list-max (flat l-melodies)) -1) poids-min))
           (poid-max (if (null poids-max) (list-max (flat l-melodies)) poids-max))
           (l-poids (k-combinatoire poid-min poid-max (length l-melodies)))
           (resultat nil)(valeur 0))
      (loop for item in l-poids do
            (let ((val (k-sigma-diff l-melodies item)))
              (if (> val valeur)
                (progn
                   (setq valeur val)
                   (setq resultat item)))))
      resultat)))

(defmethod! k-ponderation-minimale ((l-melodies list) &optional poids-min poids-max)
  :initvals '(nil -1 1)
  :icon 128
  :indoc '("liste des melodies de poids" "poids minimal autorise" "poids maximal autorise")
  :doc "Renvoie une ponderation minimale par melodie de poids de sorte que la
melodie issue de la somme de toutes les melodies ait des differences minimales
entre ses termes consecutifs."

  (unless (null l-melodies)
    (let* ((poid-min (if (null poids-min) (* (list-max (flat l-melodies)) -1) poids-min))
           (poid-max (if (null poids-max) (list-max (flat l-melodies)) poids-max))
           (l-poids (k-combinatoire poid-min poid-max (length l-melodies)))
           (resultat nil)(valeur (k-sigma-diff l-melodies (car l-poids))))
      (loop for item in (cdr l-poids) do
            (let ((val (k-sigma-diff l-melodies item)))
              (if (< val valeur)
                (progn
                   (setq valeur val)
                   (setq resultat item)))))
      resultat)))

(defmethod! k-sigma-diff ((l-melodies list)(l-poids list))
  :initvals '(nil nil)
  :icon 128
  :indoc '("liste des melodies de poids" "liste des poids respectifs")
  :doc "renvoie la somme des valeurs absolues des differences ponderees"
  (somme (mapcar #'(lambda(melo)
                     (abs (somme melo)))
                 (mat-trans (om* l-poids (mapcar #'(lambda(a)
                                                     (x->dx a)) l-melodies))))))

(defmethod! k-combinatoire ((poids-min integer)(poids-max integer)(taille integer))
   :initvals '(nil)
   :icon 128
   :indoc '("poids maximal" "taille des melodies")
   :doc "renvoie les combinaisons possibles de 'taille entiers entre - abs(poids) et abs(poids) "
   (reverse (k-combinatoire1 (remove nil (loop for item from poids-min to poids-max collect (if (equal 0 item) nil (list item))))
                           poids-min poids-max taille)))

(defun k-combinatoire1 (liste poids-min poids-max taille)
  (cond ((equal 1 taille) liste)
        (t (k-combinatoire1 (k-combi-elt liste poids-min poids-max) poids-min poids-max (- taille 1)))))

(defun k-combi-elt (liste poids-min poids-max)
  (flat (remove 'nil (loop for item from poids-min to poids-max collect
                           (if (equal 0 item) nil (loop for elt in liste collect (cons item elt))))) 1))



;************************** quelques fonctions de marquage  **********************************************************
;22/01
;sur une suite d'onset, on recherche des regularites
;marche assez bien
;reste a faire :
;les pulsations peuvent tomber sur des notes non marquees, mais sont induites uniquement a partir de marquages
;-> confirmer les pulsations lorsqu'elles tombent sur des divisions simples d'inter-onsets (syncopes)
;s'occuper des pulsations dupliquees ou tres proches : probleme : determiner quand elles sont proches, 
;peut-etre utiliser make-regular ?
;le poids attribue n'est pas tres fin, et pourtant, il determine l'emplacement des marques
;mieux definir quels poids considerer dans la melodie (les maxi locaux, tous les poids non nuls...)
;differencier poids de marquage et ponderation de marquage : importance attribuee au marquage
;


;fonctionnement :
;'marquage-pulse : intuite les pulsations avec extract-pulse-nvel-onset : intervalles entre maxi locaux de la melodie de poids
;pour l'onset passe. Les maxi locaux sont entendus comme evenements superieurs ou egaux aux deux evenements voisins. 
;verifie si l'onset courant tombe sur une pulsation et eventuellement actualise la pulsation
;verifie si les autres pulsations sont toujours d'actualite
;renvoie un poids pour l'onset courant si il tombe sur une pulsation puis itere le processus

;parametres : 
;contexte : liste des couples poids-onset deja analyses
;taille-contexte : taille maximale du contexte autorise en ms 
;taille-approx : pourcentage d'ecart par rapport a la valeur de la pulsation autorise
;pulse-max : valeur maximale de la pulsation en ms
;contexte-pulse : nombre de pulsations autorisees entre deux coincidences pulsation-onset


;03/01/01 marquages de la phase II

;FAIT**introduire peut-etre un changement dynamique de pulsation : on prend la nouvelle valeur 
;FAIT**a partir de l'ancienne par la moyenne avec la meilleure valeur pour l'onset courant
;FAIT**probleme des notes qui tombent autour d'une pulsation : laquelle choisir ? : celle qui convient le mieux
;FAIT**empecher le decalage qui peut se produire quand plusieurs choix sont possibles, et le mauvais est choisis

(defun extract-pulse (lon lmelo pulse-max)
;extrait toutes les pulsations possibles de la melodie de poids (a partir des maxi locaux de la melodie)
  (let ((ldeb-pulse (k-poids->segments lmelo lon))
        (lpoids-pulse (k-poids->segments lmelo lmelo)))
    (mapcon #'(lambda(a a1) (let ((origine (car a)))
                           (mapcan #'(lambda(b b1) (unless (> (- b origine) pulse-max)
                                                     (list (list (- b origine)  b1 b))))
                                   (cdr a) (cdr a1))))
            ldeb-pulse lpoids-pulse)))


(defun extract-pulse-nvel-onset (lon lmelod pulse-max)
  ;extrait toutes les pulsations possibles se terminant sur le dernier onset de lon
  (unless (null lon)
    (let* ((lmelo (if (and (not (null (cdr lmelod))) (>= (car lmelod) (cadr lmelod))) lmelod (cons 0 (cdr lmelod))))
          (ldeb-pulse (butlast (k-poids->segments lmelo lon)))
          (lpoids-pulse (butlast (k-poids->segments lmelo lmelo))))
      (mapcon #'(lambda(a a1) (let ((origine (car a)))
                                (unless (> (- (car (last lon)) origine) pulse-max)
                                  (list (list 
                                         (- (car (last lon)) origine)  
                                         (car (last lmelo)) (car (last lon)) (car (last lon)) 
                                         (- (car (last lon)) origine) (car (last lmelo)) 0 1)))))
              ldeb-pulse lpoids-pulse))))


(defun remove-pulse-duplicate (lpuls-new on lpuls taille-approx)
  ;compare les differentes pulsations et elimine les pulsations très proches dupliquees
  (append (mapcan #'(lambda(a) (let ((puls (nth 2 (car (member (car (member (car a) (nth 4 (mat-trans lpuls))
                                                                            :test (lambda(a b) 
                                                                                    (< (/ (abs (- a b)) b) taille-approx)))) lpuls :test 'member)))))
                                 (unless (and puls (< (nth-value 1 (round (/ puls on))) 0.11)) (list a))))
                  lpuls-new) lpuls))

(defun new-id (lpuls-new lpuls)
  ;attribue aux nouvelles pulsation un identifiant selon ceux deja attribues
  (let ((id (nth 7 (mat-trans lpuls))))
    (if (null id) (setq id 0) (setq id (list-max (sort id '<)))) 
    (mapcar #'(lambda(a) (setq id (+ id 1)) (append (first-n a 7) (list id))) lpuls-new)))


(defun member-approx (elt liste taille-approx)
  (unless (null liste)
    (if (< (/ (abs (- elt (car liste))) (car liste)) taille-approx) (car liste) (member-approx elt (cdr liste) taille-approx))))

(defun equal-approx (elt1 elt2 taille-approx)
  (< (/ (abs (- elt1 elt2)) elt2) taille-approx))

(defun remove-pulse-duplicate2 (lpuls taille-approx)
;elimine les redondances dans (car lpuls)
)

(defun affiche-pulses (lpulses))
  ;renvoie un

;*****fonction principale

(defun marquage-pulse (lon lmelo contexte l-pulses taille-contexte taille-approx pulse-max contexte-pulse)
  ;marque les onset tombant sur la pulsation par un poids de pulsation
  ;contexte-pulse est le nb de fois ou la pulsation attendue ne se produit pas avant qu'elle disparaisse
  ;contexte contient la liste onset-poids des elements deja analyses 
  (unless (or (null lon) (null lmelo))
    (print (list "melo" lmelo "lon" lon))
    (let* ((resul (marquage-pulse0 (car lon) (car lmelo)
                                   (if (and (and (not (null (cdadr contexte))) 
                                                 (>= (car (last (cadr contexte))) (car lmelo))
                                                 (>= (car (last (cadr contexte))) (car (last (butlast (cadr contexte))))))
                                            (< (length l-pulses) 4))
                                     (remove-pulse-duplicate 
                                      (new-id (extract-pulse-nvel-onset (car contexte) (cadr contexte) pulse-max)
                                              l-pulses) (car (last (car contexte)))
                                      l-pulses taille-approx)
                                     l-pulses)
                                   contexte-pulse taille-approx contexte)))
      ;(print (list "contexte" contexte))
      ;(print (list "resul" resul))
      
      (cons resul (marquage-pulse (cdr lon) (cdr lmelo) 
                                  (cond ((null contexte) (list (list (car lon)) (list (car lmelo))))
                                        ((< (- (car lon) (car (car contexte))) taille-contexte)
                                         (list (append (car contexte) (list (car lon)))
                                               (append (cadr contexte) (list (car lmelo)))))
                                        (t (list (append (cdar contexte) (list (car lon)))
                                                 (append (cdadr contexte) (list (car lmelo))))))
                                  resul taille-contexte taille-approx pulse-max contexte-pulse)))))


(defun marquage-pulse0 (on melo l-pulses contexte-pulse taille-approx contexte)
  ;l-pulses est au format : ((pulsation poids dernier-onset avant-dernier-onset ancienne-pulse poids-dernier-onset identifiant flag)...())
  (print (list "on" on))
  (print (list "poids" melo))
  (print (list "pulses" l-pulses))
  (let* ((l-puls0 (dans-intervalle-prediction on l-pulses taille-approx melo))
         (l-puls (append (verif-contexte on (x-xor l-puls0 l-pulses 'equal #'(lambda(a) (nth 4 a))) contexte-pulse)
                         l-puls0))
         (resul (if l-puls0 (marquage-pulse1 on melo (car (mat-trans l-puls0)) l-puls))))
    (if l-puls0 resul l-puls)))


(defun verif-contexte (onset l-pulses contexte-pulse)
;verifie que les pulsations ne sortent pas du "contexte de pulsation"
  (unless (null l-pulses)
    (if (< (+ (* contexte-pulse (nth 0 (car l-pulses))) (nth 2 (car l-pulses))) onset)
      (verif-contexte onset (cdr l-pulses) contexte-pulse)
      (cons (car l-pulses)
            (verif-contexte onset (cdr l-pulses) contexte-pulse)))))


(defun dans-intervalle-prediction (onset l-pulses taille-approx poids-melo)
  ;on parcours chaque pulse pour savoir si le nouvel onset correspond a une pulsation
  ;renvoie la liste des pulsations dans l'intervalle de prediction
  (unless (null l-pulses)
    (let ((nb-puls (/ (- onset (nth 2 (car l-pulses))) (nth 4 (car l-pulses))))
          (nb-puls1 (/ (- onset (nth 3 (car l-pulses))) (nth 4 (car l-pulses))))
          (nb-puls2 (/ (- (nth 2 (car l-pulses)) (nth 3 (car l-pulses))) (nth 4 (car l-pulses)))))
      ;nb-puls  :     -
      ;nb-puls1 :  ----
      ;nb-puls2 :  ---
      (cond 
       ((equal 0 (round nb-puls))
        ;la priorite est donnee aux onset de poids fort
        (if (and (equal (round nb-puls1) (round nb-puls2))
                 (<= (abs (nth-value 1 (round nb-puls1))) (* (round nb-puls1) taille-approx))
                 (or 
                  (< (nth 5 (car l-pulses)) poids-melo)
                  (and 
                   (< (abs (nth-value 1 (round nb-puls1))) (abs (nth-value 1 (round nb-puls2))))
                   (= (nth 5 (car l-pulses)) poids-melo))))
          ;on revient en arriere, car le nouvel onset est meilleur que le dernier pour le dernier emplacement de la pulsatin
          ;on reprend donc le contexte de l'ancien onset et l'ancienne valeur de pulse
          (cons (list (nth 4 (car l-pulses)) (+ 1 (nth 1 (car l-pulses))) 
                      (nth 3 (car l-pulses)) (nth 3 (car l-pulses)) (nth 4 (car l-pulses)) poids-melo 1 (nth 7 (car l-pulses)))
                (dans-intervalle-prediction onset (cdr l-pulses) taille-approx poids-melo))
          (dans-intervalle-prediction onset (cdr l-pulses) taille-approx poids-melo)))
       ;cas ou le nouvel onset correspond a un emplacement de pulse
       ((<= (abs (nth-value 1 (round nb-puls))) (* (round nb-puls) taille-approx))
        (cons (append (first-n (car l-pulses) 6) (list 0 (car (last (car l-pulses)))))
              (dans-intervalle-prediction onset (cdr l-pulses) taille-approx poids-melo)))
       (t (dans-intervalle-prediction onset (cdr l-pulses) taille-approx poids-melo))))))

(defun marquage-pulse1 (onset poids l-puls l-pulses)
  ;l-puls contient (uniquement) les pulsations marquant l'onset de poids 'poids
  ;renvoie 'nouveau poids et 'nouvelles pulsation (pulse poids onset)
  (unless (null l-pulses)
    (if (member (nth 0 (car l-pulses)) l-puls)
      (cons (list (new-pulse (print (nth 0 (car l-pulses))) (nth 2 (car l-pulses)) onset) 
                        (+ (nth 1 (car l-pulses)) 1) onset (nth 2 (car l-pulses)) 
                        (nth 0 (car l-pulses)) poids (nth 6 (car l-pulses)) (nth 7 (car l-pulses)))
            (marquage-pulse1 onset poids l-puls (cdr l-pulses)))
      (cons (car l-pulses)
            (marquage-pulse1 onset poids l-puls (cdr l-pulses))))))

(defun new-pulse (pulse onset1 onset2)
  ;renvoie la moyenne entre (la meilleure pulse entre onset1 et onset2 proche de pulse), et pulse
  (if (equal 0 (round (/ (- onset2 onset1) pulse))) pulse
      (round (/ (om+ (round (/ (- onset2 onset1) (round (/ (- onset2 onset1) pulse)))) pulse) 2))))


;******* interpretation de la liste des differentes pulses trouvees

(defun debut-pulse (liste)
  ;ajoute a liste le premier onset qui a induit la pulse
  (cons (list (nth 0 (car liste)) (nth 1 (car liste)) (- (nth 2 (car liste)) (nth 0 (car liste))) (nth 3 (car liste)) (nth 4 (car liste)))
        liste))

(defun regularise-tempo (liste)
  (mapcon #'(lambda(a)
              (if (null (cdr a)) (list (car a)) 
                  (append (list (car a)) (loop for num from 1 to (- (round (/ (- (nth 2 (cadr a)) (nth 2 (car a))) (nth 0 (car a)))) 1) collect
                                               (append (first-n (car a) 2) (list (+ (nth 2 (car a)) (* num (nth 0 (car a))))) (last-n (car a) 2))))))
          (debut-pulse liste)))


(defun courbe-pulse-poids (l-pulses)
  ;(print (list l-pulses nb-pulse))
  ;renvoie la melodie de poids, marquage des differentes occurences des pulsations
  (unless (null l-pulses)  (mapcar #'(lambda(a)
                                       (mapcon #'(lambda(c)
                                                   (unless (or (equal 1 (nth 3 (cadr c))) (null (nth 0 (car c)))) (list (car c))))
                                               (remove-dup a #'(lambda(a b) (equal (caddr a) (caddr b))) 1)))
                                   (mat-trans (mapcar #'(lambda(a) 
                                                          (let* ((l-id nil)
                                                                 (resul (mapcar #'(lambda(b) 
                                                                                    (setq l-id (cons (nth 7 b) l-id))
                                                                                    (list (nth 0 b) (nth 1 b) (nth 2 b) (nth 6 b) (nth 7 b))) a)))
                                                            (loop for item in (x-xor l-id (arithm-ser 1 (list-max l-id) 1)) do
                                                                  (setq resul (cons (list nil nil nil nil item) resul)))
                                                            (setq resul (sort resul '< :key #'(lambda(a) (nth 4 a))))))
                                                      l-pulses)))))

(defun courbe-pulse (l-pulses nb-pulse)
  ;(print (list l-pulses nb-pulse))
  ;renvoie les differents bpf issus des differentes pulsations (association pulsation-onset)
  (mapcar #'(lambda(a)
              (remove-dup a #'(lambda(a b) (equal (cadr a) (cadr b))) 1))
          (mat-trans (mapcar #'(lambda(a) 
                                 (let* ((mat-tr (mat-trans a))
                                        (resul (first-n (sort (mat-trans (list (car mat-tr) (nth 2 mat-tr))) '< :key 'car) nb-pulse)))
                                   (append resul (create-list (- nb-pulse (length resul)) (car (last resul))))))
                             l-pulses))))

(defun courbe-pulse-channel (l-pulses nb-pulse)
  ;(print (list l-pulses nb-pulse))
  ;renvoie les differents bpf issus des differentes pulsations 
  (mapcar #'(lambda(a)
              (remove-dup a #'(lambda(a b) (equal (cadr a) (cadr b))) 1))
          (mat-trans (mapcar #'(lambda(a) 
                                 (let* ((mat-tr (mat-trans a))
                                        (resul (first-n (sort (mat-trans (list (nth 7 (mat-trans a)) (nth 2 mat-tr))) '< :key 'car) nb-pulse)))
                                   (append resul (create-list (- nb-pulse (length resul)) (car (last resul))))))
                             l-pulses))))

#|
(defun courbe-pulse-channel (l-pulses nb-chan)
  ;(print (list l-pulses nb-chan))
  ;renvoie les differents bpf issus des differentes pulsations 
          (mat-trans (mapcar #'(lambda(a) 
                                 (let ((resul (list-n-elt (sort (nth 6 (mat-trans a)) '<) nb-chan)))
                                   (append resul (create-list (- nb-chan (length resul)) (nth 5 (last resul)))))) 
                             l-pulses)))
|#
              
(defun list-n-elt (liste long)
  (let ((resul nil))
    (loop for item from 1 to long do
          (setq resul (append resul (list (if (member item liste :test 'equal) item 0)))))
    resul))


;*************

;15/12/00 marquages de la phase I


(defun marquage-marq1 (lon contexte)
  ;donne un poids de x a l'onset repetant l'i-o-i le precedant pour la x ième fois
  (cond ((null (cdr lon)) nil)
        ((null contexte)
         (cons 0 
               (marquage-marq1 (cdr lon) (cons (car lon) contexte))))        
        ((null (cdr contexte))
         (cons 0 
               (marquage-marq1 (cdr lon) (cons (car lon) contexte))))
        (t (cons (proche (reverse (x->dx (reverse (if (> (length contexte) 10) 
                                                    (cons (car lon) (butlast contexte))
                                                    (cons (car lon) contexte))))))
                 (marquage-marq1 (cdr lon) (if (> (length contexte) 10) 
                                             (cons (car lon) (butlast contexte))
                                             (cons (car lon) contexte)))))))

(defun marquage-marq2 (lon contexte)
;donne un poids de x a l'onset dont l'i-o-i est superieur aux x i-o-i le precedant
  (cond ((null (cdr lon)) 'nil)
        ((null contexte) 
         (cons 0 
               (marquage-marq2 (cdr lon) (cons (car lon) contexte))))
        (t (cons (superieur (- (cadr lon) (car lon)) (reverse (x->dx (reverse 
                                                                      (if (> (length contexte) 10) 
                                                                        (cons (car lon) (butlast contexte))
                                                                        (cons (car lon) contexte))))))
                 (marquage-marq2 (cdr lon) (cons (car lon) contexte))))))


(defun proche (liste)
  (if (null (cdr liste)) 0 
      (if (<= (abs (- (cadr liste) (car liste))) 
              (* (/ (+ (cadr liste) (car liste)) 2) 0.2))
        (+ 1 (proche (cdr liste))) 0)))
         
(defun proche2 (liste)
  (if (null (cdr liste)) 0 
      (if (<= (abs (- (cadr liste) (car liste))) 
              (* (/ (+ (cadr liste) (car liste)) 2) 0.2))
        (+ 1 (proche (cdr liste))) 0)))


(defun different (onset liste)
  (if (null liste) 0
      (if (or (> onset (* 1.2 (car liste)))
              (< onset (* 0.8 (car liste))))
        (+ 1 (different onset (cdr liste))) 0)))

(defun superieur (onset liste)
  (if (null liste) 0
      (if (> onset (* 1.2 (car liste)))
        (+ 1 (superieur onset (cdr liste))) 0)))


(defun marquage-marq3 (lmid contexte)
;donne un poids de n - 1 a l'accord contenant n notes
  (mapcar #'(lambda(a) (- (length a) 1)) lmid))

;**********


(defun marquage-variation-quant (liste p1 p2 p3)
  (marquage-variation (dx->x (car liste) (make-regular (x->dx liste) 5)) p1 p2 p3))

(defun marquage-variation-quant2 (liste p1)
  (marquage-variation2 (dx->x (car liste) (make-regular (x->dx liste) 5)) p1))

(defmethod! marquage-variation ((liste list)(p1 integer)(p2 integer)(p3 integer))
  :initvals '(nil 0 1 2)
  ;:icon 128
  :indoc '("liste" "poids - -" "poids -- -" "poids - --")
  :doc "Attribue a chaque element de la liste un poids. Les intervalles consideres sont les distances
qui separent un element de ses voisins"
  (append (cons 0 (mapcar #'(lambda(a b)
                              (cond ((equal a b) p1)
                                    ((> a b) p2)
                                    (t p3)))
                          (distance-kant (butlast (cdr liste)) (butlast (butlast liste)))
                          (distance-kant (butlast (cdr liste)) (cddr liste)))) '(0)))

(defmethod! marquage-variation2 ((liste list)(p1 integer))
  :initvals '(nil 1)
  ;:icon 128
  :indoc '("liste" "poids")
  :doc "Attribue a chaque element de la liste un poids. Les intervalles consideres sont les distances
qui separent un element de ses voisins"
  (append (cons 0 (mapcar #'(lambda(a b) 
                              (print (list a b))
                              (print (cond ((< b (/ a 2)) (/ (abs p1) 2))
                                           ((> b (* a 2)) (abs p1))
                                           ((equal 0 b) 0)
                                           ((> a b) (/ (* (abs p1) (/ (abs (- b a)) a)) 2))
                                           (t (* (abs p1) (/ (- b a) a))))))
                          (distance-kant (butlast (cdr liste)) (butlast (butlast liste)))
                          (distance-kant (butlast (cdr liste)) (cddr liste)))) '(0)))

(defun distance-kant (l1 l2)
  (om-abs (om- l1 l2)))

(defmethod! poids-distance ((liste list))
  :initvals '(nil)
  ;:icon 128
  :indoc '("liste")
  :doc "marque chaque element non nul de liste par la distance 
qui le separe de l'eventuel element non nul consecutif"
  (poids-distance-1 liste '1))

(defun poids-distance-1 (liste distance)
  (let ((d distance))
    (reverse (mapcar #'(lambda (a)
                         (cond 
                          ((equal '0 a) 
                           ((lambda () 
                              (setq d (+ d 1))
                              '0)))
                          (t 
                           ((lambda () 
                              (let ((result d))
                                (setq d '1)
                                result))))))
                     (reverse liste)))))

(defmethod! weighting ((l-poids list)(l-onsets list))
  :initvals '(nil nil)
  ;:icon 128
  :indoc '("liste de poids" "liste a ponderer")
  :doc "renvoie l-onsets dont les elements NON NULS ont
ete ponderes par les elements de l-poids"
  (let ((l-poids-1 (cons '0 l-poids)))
    (mapcar #'(lambda (a)
                (cond ((null (cdr l-poids-1)) 0)
                ((= 0 a) a)
                (t ((lambda ()
                     (setq l-poids-1 (cdr l-poids-1))
                     (* a (car l-poids-1))))))) l-onsets)))

(defmethod! weighting-const ((l-poids list)(l-onsets list))
  :initvals '(nil nil)
  ;:icon 128
  :indoc '("liste de poids" "liste a ponderer")
  :doc "renvoie l-onsets dont les elements (nuls et non nuls)
ont ete ponderes des elements de l-poids. Des elements successifs 
egaux sont ponderes par la même valeur  "
  (let ((l-poids-1 (cons '0 l-poids))(elt nil))
    (mapcar #'(lambda (a)
                (cond ((equal a elt) (* a (car l-poids-1)))
                ((null (cdr l-poids-1)) '0)
                (t (progn
                     (setq l-poids-1 (cdr l-poids-1))
                     (setq elt a)
                     (* a (car l-poids-1)))))) l-onsets)))

(defmethod! weighting-constbis ((l-poids list)(l-onsets list))
  :initvals '(nil nil)
  ;:icon 128
  :indoc '("liste de poids" "liste a ponderer")
  :doc "renvoie l-onsets dont les elements (nuls et non nuls)
ont ete ponderes des elements de l-poids. De plusieurs elements successifs 
egaux seul le dernier est pondere  "
  (cond ((null l-onsets) nil)
        ((equal (car l-onsets) (cadr l-onsets)) (cons 0 (weighting-constbis l-poids (cdr l-onsets))))
        (t (cons (om* (car l-poids) (car l-onsets)) (weighting-constbis (if (null (cdr l-poids)) l-poids (cdr l-poids))
                                                                        (cdr l-onsets))))))


; ************************** fonctions de marquage cambouropoulos ****************************************************

;******** partie sensible : on rentre la liste des intervalles pour chaque propriete 
;******** = pb : quels intervalles considerer ? 

(defun detect-var (lmidic lonset ldur lvel)
  (list (filtre0+- (marquage0+- (om-abs (x->dx (mapcar #'(lambda(a)
                                                           (round (/ (+ (list-max a) (list-min a)) 2)))
                                                       lmidic)))))
        (filtre0+- (marquage0+- (make-regular (om-abs (x->dx lonset)) 5 1)))
        (filtre0+- (marquage0+- (make-regular (om-abs (butlast (mapcar #'(lambda(a)
                                                                           (round (/ (+ (list-max a) (list-min a)) 2)))
                                                                       ldur))) 5 1)))
        (filtre0+- (marquage0+- (om-abs (x->dx (mapcar #'(lambda(a)
                                                           (car a))
                                                       lvel)))))
        (filtre0+- (marquage0+- (make-regular (om-abs (om- (x->dx lonset) 
                                                           (mapcar #'(lambda(a)
                                                                       (round (/ (+ (list-max a) (list-min a)) 2)))
                                                                   (butlast ldur)))) 5 1)))))
;********





(defun marquage0+- (liste)
  (unless (null (cdr liste))
    (mapcar #'signum 
            (om* -1 (x->dx liste)))))






#|
(defun marquage0+- (liste)
  (unless (null (cdr liste))
    (mapcar #'signum 
            (x->dx liste))))

|#


(defun filtre0+- (liste)
  (om* (binary-transformation (om+ (om-abs (cons 0 (x->dx liste)))
                              (om-abs (append (om* -1 (x->dx liste)) '(0)))))
       liste))

(defun marquageICR (liste nb)
  (cond ((null liste) (list nb))
        ((equal 1 (car liste)) (cons (+ nb 1) (marquageICR (cdr liste) 1)))
        ((equal -1 (car liste)) (cons (+ nb 1) (marquageICR (cdr liste) 1)))
        (t (cons nb (marquageICR (cdr liste) 0)))))

(defun marquagePR (liste nb)
  (cond ((null liste) (list nb))
        ((equal 1 (car liste)) (cons (+ nb 1) (marquagePR (cdr liste) 0)))
        ((equal -1 (car liste)) (cons nb (marquagePR (cdr liste) 1)))
        (t (cons nb (marquagePR (cdr liste) 0)))))

(defun marquageG-ICR (liste nb)
  (cond ((null liste) (list nb))
        ((equal 1 (car liste)) (cons (+ nb 2) (marquageG-ICR (cdr liste) 0)))
        ((equal -1 (car liste)) (cons (+ nb 2) (marquageG-ICR (cdr liste) 0)))
        (t (cons nb (marquageG-ICR (cdr liste) 0)))))


(defun marquageICR-PR-GICR (lmidic lonset ldur lvel)
  (let* ((lvar (detect-var lmidic lonset ldur lvel))
         (lmid (nth 0 lvar))
         (lons (nth 1 lvar))
         (ldu (nth 2 lvar))
         (lve (nth 3 lvar))
         (lrest (nth 4 lvar))
         (lmarq nil))
    
    (setq lmarq (list      
                 (marquageICR lmid 0)
                 (marquagePR lmid 0)
                 (marquageICR lons 0)
                 (marquagePR lons 0)
                 (marquageG-ICR ldu 0)
                 (marquageICR lve 0)
                 (marquagePR lve 0)
                 (marquageICR lrest 0)
                 (marquagePR lrest 0)))
    (print (string+ (string #\Newline) 
                    (str (om+ (nth 0 lmarq) (nth 1 lmarq))) (string #\Newline)
                    (str (om+ (nth 2 lmarq) (nth 3 lmarq))) (string #\Newline)
                    (str (nth 4 lmarq)) (string #\Newline)
                    (str (om+ (nth 5 lmarq) (nth 6 lmarq)))
                    (str (om+ (nth 7 lmarq) (nth 8 lmarq))) (string #\Newline)))
    (om+bis lmarq)))


(defun om+bis (liste)
  (cond ((null liste) nil)
        ((null (cdr liste)) (car liste))
        (t (om+ (car liste) (om+bis (cdr liste))))))



; ********************** marquages d'accords  ***************************************************************  

(defmethod! abstract-accord ((liste list))
  :initvals '((6000 6400 6700) (6400 6700 7200) (6400 6900 7200) (6500 6900 7200 7550) (7050 7400 7700))
  :icon 128
  :indoc '("liste de hauteurs en midicents")
  :doc "ramène toutes les hauteurs dans la même octave (definie par la note la plus basse) et elimine les elements redondants
exprime les intervalles entre la basse et les autres notes de l'accord (en midicents)"
  (if (listp (car liste))
    (mapcar #'abstract-accord liste)
    (let ((acc (reduce-accord liste)))
      (unless (null (cdr acc))
        (mapcar #'(lambda(a)
                    (- a (car acc))) (cdr acc))))))


(defun reduce-accord (liste)
  ;ramene toutes les hauteurs dans la meme octave (definie par la note la plus basse) et elimine les elements redondants
  (let* ((val (list-min liste)))
    (sort (remove-dup (mapcar #'(lambda(a) (+ val (mod (- a val) 12))) liste) '= 1) '<)))


;************************** reconnaissance de formes **********************************************************


(defmethod! maximum-local ((liste list) &optional n type)
  :initvals '(nil 1 "separer valeurs egales")
  :indoc '("liste" "nombre d'iterations")
  :menuins '((2 (("separer valeurs egales" "separer valeurs egales")
                ("ne pas separer valeurs egales" "ne pas separer valeurs egales"))))
  :doc "renvoie la liste des maximum locaux, obtenue apres 0 ou n iterations
selon la valeur du parametre optionnel (par defaut egal a 1)
Les elements de la liste doivent etre positifs.
Exemples :
(4 3 5 5 3 2) -> (4 0 0 5 0 0)
(2 3 5 5 6) -> (0 0 0 0 6)"
  (if (null type) (setq type "separer valeurs egales"))
  (if (null n) (setq n 1))
  (cond ((null liste) nil)
        ((< n 1) liste)
        (t (maximum-local (if (equal "ne pas separer valeurs egales" type)
                            (weighting-const (maximum-local-1 (sans-repetitions liste)) liste)
                            (weighting-constbis (maximum-local-1 (sans-repetitions liste)) liste)) (- n 1) type))))

(defun maximum-local-1 (liste)
  "renvoie les maximum locaux de la liste"
  (if (null liste) nil
      (om* 
       (maximum-local-2 (om- liste (append (cdr liste) '(0))))
       (maximum-local-2 (om- liste (cons '0 (butlast liste)))))))

(defun maximum-local-2 (liste)
  (mapcar #'(lambda (a) 
              (if (< a '0) '0 '1)) liste))

(defun membre (l1 l2)
  (member l1 l2 :test 'equal))



; ************************** fonctions de transformation de listes ****************************************************

(defun isole-elements (liste)
  (mapcon #'(lambda(a)
               (if (null (cddr a)) nil
                   (let ((l (list (car a) (cadr a) (caddr a))))
                     (if (equal (om* '(0 1 0) l) l) (list (cadr a)) (list 0)))))
           (append (cons 0 liste) '(0))))

(defmethod! sans-repetitions ((liste list))
  :initvals '(nil)
  ;:icon 128
  :indoc '("liste")
  :doc "renvoie liste après avoir elimine les repetitions
de termes non nuls"
  
  (cond ((null liste) nil)
        ((null (cdr liste)) (cons (car liste) (sans-repetitions (cdr liste))))
        ((equal (car liste) (cadr liste)) (sans-repetitions (cdr liste)))
        (t (cons (car liste) (sans-repetitions (cdr liste))))))


(defmethod! filter-micro-variation ((liste list)(variation integer))
  :initvals '(nil 0)
  ;:icon 128
  :indoc '("liste" "variation a filtrer en pourcentage de la plus grande valeur de liste")
  :doc "renvoie liste dont les elements plus petits que le
 pourcentage precise par rapport a 
la plus grande valeur de liste sont mis a 0"
  (mapcar #'(lambda (a)
              (if (< (abs a) (/ (* (list-max liste) variation) 100)) 
                '0
                a)) liste))


(defmethod! binary-transformation ((liste list))
  :initvals '(nil)
  ;:icon 128
  :indoc '("liste a transformer")
  :doc "renvoie la liste binaire correspondante :
0 pour une valeur 0
1 pour toute autre valeur"
  
  (mapcar #'(lambda (a) 
              (if (= a 0) 0 1)) liste))

(defmethod! barre-min ((liste list)(valeur integer))
  :initvals '(nil 0)
  ;:icon 128
  :indoc '("liste" "valeur minimale")
  :doc "remplace par zero les valeurs de liste
inferieures a valeur"
(mapcar #'(lambda (a)
            (if (< a valeur) '0 a))liste))


;*******FONCTIONS GENERALES sur les listes********

(defmethod! multi-liste ((liste-entier list)(liste list))
  :initvals '(nil nil)
  ;:icon 128
  :indoc '("liste d'entiers" "liste quelconque")
  :doc "pour chaque element de la liste d'entiers :
- si l'element est non nul, la fonction renvoie l'element correspondant de liste
- sinon, la fonction passe a l'element de liste-entier et de liste suivant"
  (cond ((null liste-entier) nil)
        ((equal '0 (car liste-entier)) (multi-liste (cdr liste-entier) (cdr liste)))
        (t (cons (car liste) (multi-liste (cdr liste-entier) (cdr liste))))))

(defmethod! somme ((liste list))
  :initvals '(nil)
  ;:icon 128
  :indoc '("liste")
  :doc "renvoie la somme de chaque terme de la liste" 

  (cond ((null liste) '0)
        ((listp (car liste)) (om+ (car liste) (somme (cdr liste))))
        (t (+ (car liste) (somme (cdr liste))))))

(defmethod! pos-char ((liste list)(valeur integer))
  :initvals '(nil 0)
  ;:icon 128
  :indoc '("liste" "valeur dont on recherche la position")
  :doc "renvoie la première position qu'occupe 
valeur dans liste, nil si valeur n'apparaît pas"
  (let ((result (pos-char-1 liste valeur)))
    (if (null result) nil (- (length liste) result))))

(defun pos-char-1 (liste valeur)
  (cond ((null liste) nil)
        ((equal valeur (car liste)) (length liste))
        (t (pos-char-1 (cdr liste) valeur))))


(defmethod! filter-atom ((liste list)(l list))
  :initvals '(nil (0 nil))
  ;:icon 128
  :indoc '("liste" "liste du ou des elements a supprimer de liste")
  :doc "renvoie liste après avoir elimine
les elements specifies en deuxième entree"

  (cond ((null liste) nil)
        ((member (car liste) l :test 'equal) (filter-atom (cdr liste) l))
        (t (cons (car liste) (filter-atom (cdr liste) l)))))


(defmethod! sup-1ers-zeros ((liste list))
:initvals '(nil)
  ;:icon 128
  :indoc '("liste")
  :doc "renvoie la liste a partir du premier coefficient non nul"

  (cond ((null liste) nil)
        ((= '0 (car liste)) (sup-1ers-zeros (cdr liste)))
        (t liste)))

(defun last-not-0 (liste)
  (first-not-0 (reverse liste)))

(defun first-not-0 (liste)
  (cond ((null liste) nil)
        ((equal 0 (car liste)) (first-not-0 (cdr liste)))
        (t (car liste))))

(defun pos-last-not-0 (liste)
  (- (length liste) (pos-first-not-0 (reverse liste))))

(defun pos-first-not-0 (liste)
  (cond ((null liste) nil)
        ((equal 0 (car liste)) (+ 1 (pos-first-not-0 (cdr liste))))
        (t 0)))



;********* REGULARISATION DE GRAPHES ******

(defun nb-occ (liste elt)
  (nb-occ1 liste elt 0))

(defun nb-occ1 (liste elt nb)
  (cond ((null liste) nb)
        ((equal (car liste) elt) (nb-occ1 (cdr liste) elt (+ nb 1)))
        (t (nb-occ1 (cdr liste) elt nb))))

(defclass groupe ()
 ((l-element :initform nil :accessor l-element :initarg :l-element :type list)
  (val-grille :initform 0 :accessor val-grille :initarg :val-grille :type integer)))

(defclass element ()
 ((val :initform 0 :accessor val :initarg :val :type integer)
  (nb :initform 0 :accessor nb :initarg :nb :type integer)))

;**CONSTRUCTEURS***

(defmethod! groupe-instancier ((l-element list)(val-grille number))
  ;"instancie un objet groupe"
  (let* ((jock (if (null val-grille) '0 val-grille))
        (g (make-instance 'groupe :l-element nil :val-grille jock))
        (l-elt (remove-dup (copy-list l-element) 'equal 1)))
    (setf (l-element g) (mapcar #'(lambda(a)
                                    (make-instance 'element :val a :nb (nb-occ l-element a)))
                                l-elt))
    g))
                                    
(defmethod! groupe-copie ((l-groupe list))
  (mapcar #'groupe-copie l-groupe))

(defmethod! groupe-copie ((g groupe))
  (make-instance 'groupe 
    :l-element (element-copie (l-element g))
    :val-grille (val-grille g)))

(defmethod! element-copie ((l list))
  (mapcar #'element-copie l))

(defmethod! element-copie ((el element))
  (make-instance 'element 
    :val (val el)
    :nb (nb el)))



;**METHODES***

(defmethod! groupe-taille ((liste list))
  ;:icon 138
  (mapcar #'groupe-taille liste))

(defmethod! groupe-taille ((self groupe))
  "renvoie la taille du groupe "
  (somme (mapcar #'nb (l-element self))))

(defmethod! groupe-element ((liste list))
  ;:icon 138
  (mapcar #'groupe-element liste))

(defmethod! groupe-element ((self groupe))
  "renvoie la liste des elements du groupe "
  (l-element self))

(defmethod! groupe-element-val ((liste list))
  ;:icon 138
  (mapcar #'groupe-element-val liste))

(defmethod! groupe-element-val ((self groupe))
  "renvoie la liste des elements du groupe "
  (mapcar #'val (l-element self)))


(defmethod! groupe-val-grille ((liste list))
  ;:icon 138
  (mapcar #'(lambda (element)
              (groupe-val-grille element))
          liste))

(defmethod! groupe-val-grille ((self groupe))
  "renvoie la liste des valeurs val-grille des elements du groupe "
  (val-grille self))


(defmethod! groupe-moyenne ((liste list))
  ;:icon 138
  (if (null liste) '0
      (mapcar #'(lambda (element)
                  (groupe-moyenne element))
              liste)))

(defmethod! groupe-moyenne ((self groupe))
  "renvoie la valeur de l'element moyen du groupe "
  (if (> (groupe-taille self) 0)
    (round (/ (somme (mapcar #'(lambda(a)
                                 (* (val a) (nb a))) (l-element self))) (groupe-taille self)))
    0))

; ********************** methodes de classification, reconnaissance de patterns  ***************************************************************  

(defmethod! make-regular ((liste list)(tolerance integer) &optional nombre-min nombre-de-groupes)
   :initvals '(nil '5 '1 '100)
   :indoc '("liste de valeurs" 
            "ecart tolere en pourcentage des valeurs de 'liste"
            "nombre-minimum d'elements pouvant former un groupe"
            "nombre de points de recherche entre les deux valeurs extêmes de la liste (par defaut initialise a 100)")
   :doc "Modifie les valeurs de 'liste' de manière a mettre en evidence des regularites.
Procedure :
Cherche les elements de la liste appartenant a un même intervalle puis leur donne une même
valeur moyenne.
Un grand nombre de groupes de valeurs est d'abord cree,
specifie par l'entree optionelle 'nombre-de-groupes.
(Par defaut on prend 100 valeurs caracteristiques reparties uniformement entre les deux 
     valeurs extrêmes de la liste donnee en entree).
Chaque groupe est centre sur une valeur caracteristique.
Ces groupes sont ensuite filtres par les critères suivants :
1) sont retenus les groupes ayant le plus grand nombre de valeurs
2) sont retenus les groupes dont la moyenne des valeurs est la plus proche
de la valeur caracteristique du groupe
3) les groupes ayant plus de la moitie de leurs elements en commun sont fusionnes
les elements communs aux groupes restants sont retires de l'un des groupes."
   (unless (null liste)
     (let* ((max (list-max liste))
            (min (list-min liste))
            (nombre-min (round (if (null nombre-min) '1 nombre-min)))
            (nb-groupes (round (if (null nombre-de-groupes) 
                                 (if (and (< max 100) (> max 1)) max  '100)
                                 nombre-de-groupes)))
            (groupe nil)
            (resultat liste)
            (unite (/ (- max min) nb-groupes)))
       
       (if (or (null unite) (<= unite 0)) (setq unite 1))
       ;recherche des groupes de base
       (setq groupe (groupe-reconnaissance liste 
                                           tolerance 
                                           nombre-min 
                                           nb-groupes min max unite))
       ;filtrage des groupes de base
       (setq groupe (remove 'nil (groupe-traitement groupe unite tolerance)))
       
       ;modifications des valeur de liste en fonction des groupes trouves
       (if (null groupe) 
         liste
         (setq resultat (om-round (graphe-traitement resultat groupe)))))))
  

(defun groupe-reconnaissance (liste tolerance nombre-min nb-groupes min max unite)
  ;la grille de groupe est a la fois une serie arithmetique et une grille de densite :
  ;les valeurs mises en sequence sont selectionnees tous les n pas 
  (let ((g-g (sort (append (groupe-grille liste nb-groupes) (om-round (arithm-ser min max unite))) '<))
        (l (copy-list liste)))
    (setq g-g (remove-dup g-g 'equal 1))
    ;(setq l (remove-dup l 'equal 1))
    (loop for item in g-g          ;numero from '0 to nb-groupes
          collect (groupe-reconnaissance-1 l
                                           tolerance 
                                           item     ;(+ min (* (sqr (/ numero nb-groupes)) (- max min)))
                                           nombre-min))))


(defun groupe-grille (liste nb-groupe)
  (let* ((l (copy-list liste))
        (long (length l))
        (unite (om-round (/ long nb-groupe))))
    (if (< unite 1) (setq unite 1))
    (setq l (sort l '<))
    (remove 0 (om* l (first-n (flat (create-list (+ nb-groupe 1) (cons 1 (create-list (- unite 1) 0)))) long)))))




(defun groupe-grille1 (liste unite min)
  (cond ((null liste) nil)
        (t (cons (round (/ (- (car liste) min) unite)) (groupe-grille1 (cdr liste) unite min)))))


(defun groupe-reconnaissance-1 (liste tolerance valeur nombre-min)
  (let ((resultat nil))
    (setq resultat 
          (groupe-reconnaissance-2 liste tolerance valeur nil))
    (if (>= (length resultat) nombre-min)
      (groupe-instancier resultat valeur)
      (groupe-instancier nil valeur))))



;7/04 modif de facon a enlever les nil en trop
(defun groupe-reconnaissance-2 (liste tolerance valeur resul)
    (cond ((null liste) resul)
          (t (let ((borne-inf (- valeur (* (/ valeur 100) tolerance)))
                   (borne-sup (+ valeur (* (/ valeur 100) tolerance))))
               (if (and (>= (car liste) borne-inf) (<= (car liste) borne-sup))
                 (groupe-reconnaissance-2 (cdr liste) tolerance valeur (cons (car liste) resul))
                 (groupe-reconnaissance-2 (cdr liste) tolerance valeur resul))))))
  


(defun groupe-traitement (groupe unite tolerance)
  ;en cas de conflit, un filtrage est effectue
  (let ((liste-tailles (groupe-taille groupe))
        (resultat (groupe-copie groupe)))
    (if (groupe-traitement-separation-zero liste-tailles nil) 
      groupe
      (progn
        ;(print (list "1" (groupe-element-val resultat)))
        ;on retient les groupes dont la moyenne est la plus proche de la valeur caracteristique du groupe
        (setq resultat (groupe-traitement-moyenne resultat unite))
        ;(print (list "2" (groupe-element-val resultat)))
        ;on traite l'intersection des groupes
        (setq resultat (groupe-traitement-intersection resultat))))
    ;(print (list "3" (groupe-element-val resultat)))
    (if (equal 1 (length (groupe-element-val resultat)))
      (let* ((max (list-max (groupe-element-val resultat)))
             (min (list-min (groupe-element-val resultat)))
             (moy (/ (+ max min) 2)))
        (if (or (< min (/ (* moy (- 100 tolerance)) 100))
                (> max (/ (* moy (+ 100 tolerance)) 100)))
          (setq resultat nil))))
    resultat))


(defun groupe-traitement-separation-zero (liste non-nul-element-courant)
;vrai si toute valeur non nulle est encadree de deux zeros
  (cond ((null liste) t)
        ((and non-nul-element-courant (not (equal (car liste) '0))) nil)
        ((equal (car liste) '0) (groupe-traitement-separation-zero (cdr liste) nil))
        (t (groupe-traitement-separation-zero (cdr liste) t))))


(defun groupe-traitement-moyenne (groupe unite)
;etude du cas de deux groupes de meme taille consecutifs :
;la methode retient les groupes dont la moyenne est la plus proche de la valeur caracteristique du groupe
  (cond ((null groupe) nil)
        ((null (cdr groupe)) (list (car groupe)))
        ((and (equal (groupe-taille (car groupe)) (groupe-taille (cadr groupe)))
              (equal (+ unite (groupe-val-grille (car groupe))) (groupe-val-grille (cadr groupe))))
         ;groupe-val-grille est la valeur caracteristique du groupe dans la liste initiale des groupes
         (if (> (abs (- (groupe-val-grille (car groupe)) (groupe-moyenne (car groupe))))
                (abs (- (groupe-val-grille (cadr groupe)) (groupe-moyenne (cadr groupe)))))
           (groupe-traitement-moyenne (cdr groupe) unite)
           (groupe-traitement-moyenne (cons (car groupe) (cddr groupe)) unite)))
        (t (cons (car groupe) (groupe-traitement-moyenne (cdr groupe) unite)))))

(defun groupe-traitement-intersection (groupe)
  ;cas des groupes ayant des elements en commun
  (cond ((null (cdr groupe)) (list (car groupe)))
        (t (let ((filtre (intersection (l-element (car groupe)) (l-element (cadr groupe)) 
                                       :key #'val)))
             (if (null filtre) 
               (cons (car groupe) (groupe-traitement-intersection (cdr groupe)))
               (if (or (> (somme (mapcar #'nb filtre)) (round (/ (groupe-taille (car groupe)) '2)))
                       (null (set-difference filtre 
                                             (l-element (cadr groupe))
                                             :key #'val)))
                 (progn
                   (setf (val-grille (car groupe))
                         (/ (+ (groupe-val-grille (car groupe)) (groupe-val-grille (cadr groupe))) '2))
                   (setf (l-element (car groupe)) 
                         (union (groupe-element (car groupe)) (groupe-element (cadr groupe))
                                         :key #'val))
                   (groupe-traitement-intersection (cons (car groupe) (cddr groupe))))
                 (progn
                   (setf (l-element (car groupe)) 
                         (set-difference (groupe-element (car groupe)) filtre 
                                         :key #'val))
                   (setf (val-grille (car groupe))
                         (groupe-moyenne (car groupe)))
                   (cons (car groupe) (groupe-traitement-intersection (cdr groupe))))))))))
                 


(defun graphe-traitement (liste groupe)
  (let ((resultat liste))
    (loop for numero from '0 to (length groupe)
          do (setq resultat 
                   (graphe-traitement-1 resultat (nth numero groupe))))
    resultat))


(defun graphe-traitement-1 (liste groupe)
  (mapcar #'(lambda (a)
              (if (member a (groupe-element-val groupe) :test 'equal)
                (groupe-moyenne groupe) 
                a))
          liste))


;******************************************************************************
;fonctions de conversion string-entier



(defun str (syl)
  (prin1-to-string syl))

(defun entier (syl)
  (entier-1 (reverse syl) 0))

(defun entier-1 (syl number)
  (cond ((equal number (length syl)) '0)
        (t (+ (entier-2 (string (char syl number))) (* (entier-1 syl (+ number 1)) 10)))))

(defun entier-2 (syl)
  (cond
   ((null syl) '0)
   ((equal syl "0") '0)
   ((equal syl "1") '1)
   ((equal syl "2") '2)
   ((equal syl "3") '3)
   ((equal syl "4") '4)
   ((equal syl "5") '5)
   ((equal syl "6") '6)
   ((equal syl "7") '7)
   ((equal syl "8") '8)
   ((equal syl "9") '9)
   (t '-1)))

;************ info midi

(defmethod! info-midi ((self midifile)(chan integer) &rest channel)
   :initvals '(nil '1)
   :indoc '("fichier midi" 
            "channel choisi")
   :doc "renvoie un chord-seq contenant les channels choisis"
   (let ((l (mat-trans (classer-liste (flat (mapcar #'(lambda (a) (unless (null a) (nth (- a 1) (mf-info self))))
                                       (cons chan channel)) 1)))))
     (make-instance 'chord-seq 
       :lmidic (om* (nth 0 l) 100)
       :lonset (nth 1 l)
       :ldur (nth 2 l)
       :lvel (nth 3 l)
       :lchan (nth 4 l))))


(defmethod classer-liste ((liste list))
   (let ((l nil))
     (loop for item in liste do
           (setq l (placer-liste item l)))
     l))


(defun placer-liste (objet liste)
   (cond ((null liste) (list objet))
         ((<= (nth 1 objet) (nth 1 (car liste))) (cons objet liste))
         (t (cons (car liste) (placer-liste objet (cdr liste))))))


;************ fonctions bizarres !

;****** fonctions diverses

(defun l-np (nombre)
  ;renvoie la decomposition en nombres premiers de nombre
  (let ((n 1)(prime-list nil))
    (remove 'nil (while (<= (setq n (1+ n)) nombre)
                        (setq prime-list (append prime-list
                                                 (if (and (or (null prime-list)
                                                              (not (member 'nil
                                                                           (mapcar #'(lambda (x)
                                                                                       (/= 0 (rem n x)))
                                                                                   prime-list) :test 'equal)))
                                                          (equal 0 (rem nombre n)))
                                                   (list n)
                                                   'nil)))))
    prime-list))

(defun prox-pgcd (nombre diviseur)
  (cond ((> diviseur nombre) nil)
        ((equal 0 diviseur) 0)
        ((equal 0 (rem nombre diviseur)) diviseur)
        (t (prox-pgcd1 nombre diviseur 1))))

(defun prox-pgcd1 (nombre diviseur n)
  (cond ((and (> (+ diviseur n) nombre) (< (- diviseur n) 0)) nil)
        ((and (not (> (+ diviseur n) nombre))
              (equal 0 (rem nombre (+ diviseur n)))) (+ diviseur n))
        ((and (not (< (- diviseur n) 0))
              (equal 0 (rem nombre (- diviseur n)))) (- diviseur n))
        (t (prox-pgcd1 nombre diviseur (+ n 1)))))



;*************************

(defun multi (liste1 liste2)
  (cond ((or (null liste1) (null liste2)) nil)
        ((equal 0 (car liste1)) (cons 0 (multi (cdr liste1) (cdr liste2))))
        (t (cons (car liste2) (multi (cdr liste1) (cdr liste2))))))

(defun assoce-parenth (parenth liste)
  (let ((numero -1))
    (mapcar #'(lambda(a)
                (mapcar #'(lambda(b)
                            (declare (ignore b))
                            (setq numero (+ numero 1))
                            (nth numero liste))a))parenth)))

(defun enleve-elt (liste pos nb)
  (cond ((null liste) nil)
        ((< pos 0) "nil")
        ((< nb 1) liste)
        ((equal 0 pos) (last-n liste (- (length liste) nb)))
        (t (cons (car liste) (enleve-elt (cdr liste) (- pos 1) nb)))))


(defun position! (item liste)
  (position item liste :test 'equal))

(defun marquer-groupe (liste numero)
  (let ((l nil))
    (loop for item in liste do 
          (unless (member item l :test 'equal) (setq l (cons item l))))
    (setq l (sort l '<))
    (loop for item in liste collect (if (equal item (nth numero l)) 1 0))))
 

;calcule les diviseurs de nb
(defun div (nb)
  (div1 nb 1))

(defun div1 (nb num)
  (cond ((>= num nb) nil)
        ((integerp (/ nb num)) (cons num (div1 nb (+ num 1))))
        (t (div1 nb (+ num 1)))))

(defun nth-value-bis (n val)
  (nth-value n (round val)))

(defun ecart (liste)
  (let ((gcd (mk-auto-to (first-n liste 60) 0.05 0.4 0.05 0.4 0.02 1 1)))
    (mapcar #'(lambda (a)
                (om* (nth-value-bis 1 (/ (mod a gcd) gcd)) gcd)) liste)))