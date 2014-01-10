;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;Jacopo Baboni Schilingi   &    Mikhail Malt;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Profile;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;18-11-1998;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "Profile")

(om::defmethod! om-floor ((self list) )
  (mapcar #'(lambda (input)
              (om-floor input ))
          self))

(om::defmethod! om-floor ((self number))
  (floor self))




(om::defmethod! my-explode ((list list) (nlists integer)) 
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon 128
  :doc 
  "list-explode divides a list into <nlist> sublists of consecutives elements.  
For example, if list is (1 2 3 4 5 6 7 8 9), and ncol is 2, the result is ((1 2 3 4 5) 
(6 7 8 9)),
if list is (1 2 3 4 5 6 7 8 9), and ncol is 5, the result is: ((1 2) (3 4) (5 6) (7 8) (9)). 
If the number of divisions exceeds the number of elements in the list, the 
remaining divisions are returned as nil."
  
  (if (> nlists (length list)) 
    (setq list (append list (make-list (- nlists (length list)) :initial-element (first (last list))))))
  (if (<= nlists 1) list
      (let* ((length (length list))
             (low (floor length nlists))
             (high (ceiling length nlists))
             (step (if (< (abs (- length (* (1- nlists) high))) (abs (- length (* nlists low))))
                     high  low))
             (rest (mod length nlists))
             (end (- length 1 rest)) 
             (ser (arithm-ser 0  (1- step) 1))
             res)
        (for (i 0 step end)
          (push (remove () (posn-match     list (om+ i ser))) res))
        (setq low (length (flat-once res)))
        (if (< low length) (setq res (cons (append (first res) (nthcdr low list)) (rest res))))
        (cond ((> (length res) nlists) 
               (nreverse (cons (nconc (second res) (first res)) (nthcdr 2 res))))
              ((< (length res) nlists)
               (when (= (length (first res)) 1)
                 (setq res (cons (nconc (second res) (first res)) (nthcdr 2 res))))
               (nreverse (nconc (nreverse (my-explode (first res) (1+ (- nlists (length res)))))
                                (rest res))))
              (t (nreverse res))))))








(om::defmethod! notes-change  ((pits list) (scale t) &optional (mod 12))
  
:initvals '('(6100 6300) '(6000) 12) 
:indoc '("list" "number") 
:icon 152
:doc "
<Notes-Change> permet l'ajustement d'une hauteur ou d'une liste
de hauteurs <pits> a un champs harmonique defini par une autre liste
de hauteurs <scale>.
Ce module possede une troisieme entree, optionnelle, <mod> pour
indiquer le modulo du champs harmonique utilise
"
  
  (let* ((pits (om::list! pits))
         (scale (om::list! scale))
         (modsca (om-floor (om::sort. 
                               (om::remove-dup (second (multiple-value-list (om::om// (om::om/ scale (/ 100 (/ mod 12))) mod))) '= 1))))
         
         
         (pitmods (second (multiple-value-list (om::om// (om::om/ pits (/ 100 (/ mod 12))) mod))))
         (octa (octave pits))
         (posdifs (mapcar #'(lambda (p) (position (om::list-min (om::om-abs (om::om- modsca p)))
                                                  (om::om-abs (om::om- modsca p))))
                          pitmods)))
    (mapcar #'(lambda (index octave) (makenote index octave mod))
            (om::posn-match modsca posdifs)
            octa)))
;
;
;--------------------------------------
;
;
(om::defmethod! octave ((list list))
  
  :initvals '('(1 2 3 4 5) 2) 
  :indoc '("list" "number") :icon  128
  :doc "retourne l'octave a partir de c3=octave 3"
  
  (let ((list (om::list! list)))
    (mapcar #'(lambda (x) (om::om- (om::om// x 1200) 2) ) list)))
;
;
;--------------------------------------
;
;
(om::defmethod! makenote ((index number) (octave number) &optional (mod 12)) 
  :initvals '('(1 2 3 4 5) 2) 
  :indoc '("list" "number") :icon  128
  :doc" construction d'une note a partir des donnees 
             de index, octave e modulo du index"
  (+ (/ (* index 100 12) mod) (* (+ 2 octave) 1200)))
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Compressione ed espansione del profilo;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! compr/expan ((list list) 
                             (value number)
                             &optional (note? nil))
  :initvals '('(6000 7200 6600) 2 nil) 
  :indoc '("list" "number") :icon 152
  :doc 
  "Ce module opere une transformation de compression ou d'expansion
sur les intervalles de la liste de hauteurs <list> en les multipliant
par la valeur de l'entree <value>. 
<list> est une liste simple, a un niveau, de hauteurs en listents
<value> est facteur multiplicateur, il peut être un  entier ou flottant.
<note?> entree optionnelle qui permet l'ajustement de la forme generee
       a un champs harmonique quelconque.
       <note?> peut etre soit une echelle soit un accord.
Si <value> est egal a '1' les intervalles de <liste> seront maintenus les mêmes.
Si <value> est plus petit que '1' les intervalles de <liste> seront compresses.
Si <value> est plus grand que '1' les intervalles de <liste> seront elargis.
Des valeurs negatives de <value>  occasionnent des renversement d'intervalles."
  (let* ((calcolo (om::dx->x
                   (first list) 
                   (mapcar #'(lambda (x) (* x value)) (om::x->dx list))))
         (corretto (when note?
                     (notes-change calcolo note? 48))))
    (if note? corretto calcolo)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Interferenza di una lista;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! alea-pertb ((list list) (range number))
  :initvals '('(1 2 3 4 5) 2) 
  :indoc '("list" "number") :icon 152
  :doc
  "Applique une perturbation aleatoire sur une liste
de hauteurs <list>. La perturbation est faite en valeurs absolues,
c'est-a-dire, chaque hauteur est modifiee par l'addition d'une valeur 
aleatoire prise entre -range e +range.
<list> est une liste simple, a un niveau, de hauteurs en listents
<range> est le registre de la variation. 
         Si range est un nombre entier la variation se fera 
         par l'addition ou la soustraction de valeurs entieres.
         Si range est un nombre flottant la variation se fera 
         par l'addition ou la soustraction de valeurs flottantes"
  (mapcar #' (lambda (x) (+ x (om::om-random (- range) range))) list))



;
;
;--------------------------------------
;
;
(om::defmethod! control-pertb ((list list) 
                               (index list)
                               &optional (fact 1))
  :initvals '('(1 2 3 4 5) '(-10 10 -2 2) 1) 
  :indoc '("list" "number") 
  :icon 152
  :doc "Ce module permet d'appliquer une perturbation contrôlee sur 
certaines hauteurs de la liste  <list> en leurs ajoutant 
la valeur 'fact*index'.
<list> est une liste simple, a un niveau, de hauteurs en listents.
<fact> est soit une liste d'index soit un module <multi-bpf>.

La liste <fact> indique quels seront les elements 'perturbes'. 
Par exemple soit la liste <list> 
->>(6000 6600 6800 6700 6200 6300 5900),
 la liste de <fact> 
->>(0 100 -200 0 0 500 -300)
et <fact> egal a '1'
la liste resultante sera
->>(6000* 6700 6600 6700* 6200* 6800 5600)
*elements inchanges
Un zero '0' dans la liste <fact> indique que l'element correspondant
de la liste <list> sera inchange.

Dans certains cas l'entree <fact> peut être utilise.
Exemple, soit la meme liste <list> 
->>(6000 6600 6800 6700 6200 6300 5900),
et la liste de <index> 
->>(0 25 -50 0 0 5 -75)
en fonction de la valeur de <fact> il est possible
de contrôler plus finement la perturbation appliquee.
si <fact> egal 25 le resultat  sera
->>(6000 6625 6750 6700 6200 6425 5825)

si <fact> egal 25 le resultat  sera
->>(6000 6650 6700 6700 6200 6550 5750)


L'entree <index> accepte aussi un module <multi-bpf>.
Dans ce cas precis, le module <control-pertb> opere 
un echantillonnage (avec le même nombre de pas que la longueur de <list>)
de la fonction par segments et considere les valeurs obtenues comme etant
une liste <index>."
  (om::om+ list (om::om-round (om::om*  fact index) 0)))


;ATTENZIONE om::bpf-sample &optionnel 1 2
(om::defmethod! control-pertb ((list list) (index om::bpf) &optional (fact 1) )
  (let* ((xpoints (om::x-points index))
         (index (om::om*  fact (om::bpf-sample index  (om::list-min xpoints) (om::list-max xpoints) (length list) 1 2 )))
         (calcolo-bpf (om::om+ list (om::om-round index 0))))
    calcolo-bpf))
;
;
;--------------------------------------
;
;
(defun pr (lista1 lista2)
  "Se lista1 e piu corta di lista2 allora lista1 viene riletta per il numero di volte
   identico a lista 2."
  (let ((ros nil))
    (dotimes (y (length lista2) (nreverse ros))
      (push 
       (if (< (length lista1) (length lista2))
         (nth 
          (mod y (length lista1))
          lista1)
         (nth y lista1)) ros))))
;
;
;
;;;;;;;;;;;;;;;;;;;Profilo-melodico con stessi intervalli;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun prx (lista1 lista2)
  "Crea la lettura modulare di lista1 in funzione della lunghezza di lista2.
   Se lista1 e piu corta di lista2 allora lista1 viene riletta per il numero di volte
   identico a lista 2. Ma viene riletto il om::x->dx di lista1 in funzione del
   om::x->dx di lista2." 
  (let ((ros nil))  
    (dotimes (y (length (om::x->dx lista2)) (nreverse ros))   
      (push 
       (if (< (length (om::x->dx lista1)) (length (om::x->dx lista2)))
         (nth 
          (mod y (length (om::x->dx lista1)))
          (om::x->dx lista1))
         (nth y lista1)) ros))))
;
;
;--------------------------------------
;
;
(defun dillox (lista1 lista2)

  "Questa funzione cambia gli intervalli di lista2 in funzione degli intervalli di lista1.
   Se un intervallo di lista1 e discendente e quello corrispondente di lista2 e ascendente, 
   allora l'intervallo di lista2 viene cambiato e reso ascendente."
  
  (let ((ris nil)
        (test1 (prx2 lista1 lista2))
        (test2 (om::x->dx lista2)))

    (dotimes (x (- (length lista2) 1) (nreverse ris))
      (push 
       (if (> (nth x test1) 0)
         (abs (nth x test2))
         (- (abs (nth x test2)))) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! cambia-prof-mel ((lista1 list) (lista2 list))
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc "Modifica il profilo melodico di lista2 in funzione del profilo melodico
             di mista1. Il 'length' del risultato e sempre uguale al 'length' di 
             lista2. Se lista2 e piu corta di lista1 allora il processo si attua per 
             la lunghezza di lista2. Ma se lista1 e piu corta di lista2, allora 
             lista2 torna a rileggere i valori di lista1 ogni volta che lista1 finisce."
  
  (om::dx->x (first lista2) (dillox lista1 lista2)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;Profilo-melodico con stesse note;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun prx2 (lista1 lista2)
  
  "Crea la lettura modulare di lista1 in funzione della lunghezza di lista2.
   Se lista1 e piu corta di lista2 allora lista1 viene riletta per il numero di volte
   identico a lista 2. Ma viene riletto il om::x->dx di lista1 in funzione del
   om::x->dx di lista2."
  
  (let ((ros nil))
    
    (dotimes (y (length (om::x->dx lista2)) (nreverse ros))   
      (push 
       (if (< (length (om::x->dx lista1)) (length (om::x->dx lista2)))
         (nth 
          (mod y (length (om::x->dx lista1)))
          (om::x->dx  lista1))
         (nth y (om::x->dx  lista1))) ros))))
;
;
;--------------------------------------
;
;
(defun int-com (lista)
  
  "Restituisce l'intervallo complementare di un intervallo dato in funzione della
   prima nota dell'intervallo stesso. Questo significa che se ho SOL3 DO4, la 
   funzione restituisce do4 sol4."
  
  (let ((ris nil))
    
    (om::flat
     (dotimes (x (- (length lista) 1) (nreverse ris))
       (push 
        (om::x->dx (append
                    (list (nth x lista))
                    (list (-  (- (nth x lista)
                                 (*  (- 12 (mod 
                                            (/ (- (first (om::x->dx lista)) 
                                                  (* 
                                                   (first 
                                                    (om::om// (om::x->dx lista) 1200)) 1200)) 100) 12)) 100))
                              (* (first (om::om// (om::x->dx lista) 1200)) 1200)))))
        ris)))))
;
;
;--------------------------------------
;
;
(defun prof (lista1 lista2)

  "restituisce l'intervallo complentare tenendo le altezze assolute."
  
  (let ((ris nil))
    
    (om::flat 
     (dotimes (x (length (prx2 lista1 lista2)) (nreverse ris))
       (push (if (equalp (signum (nth x (prx2 lista1 lista2)))
                         (signum (nth x (om::x->dx lista2))))
               (nth x (om::x->dx lista2))
               (int-com (list
                         (nth x lista2)
                         (nth (1+ x) lista2))))
             ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! prof-fix-note ((lista1 list) (lista2 list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Modifica il profilo melodico di lista2 in funzione del profilo melodico
             di mista1. Il 'length' del risultato e sempre uguale al 'length' di 
             lista2. Se lista2 e piu corta di lista1 allora il processo si attua per 
             la lunghezza di lista2. Ma se lista1 e piu corta di lista2, allora 
             lista2 torna a rileggere i valori di lista1 ogni volta che lista1 finisce." 
  (om::dx->x (first lista2) (prof lista1 lista2)))
;
;
;--------------------------------------
;
(om::defmethod! prof-change ((prof list) 
                             (pitch list)
                             mode?)
  :initvals '((6000 6600) (6000 6600) 1)
  :menuins '((2 (("intrv" 1) ("note" 2))))
  :indoc '("number or tree" "" "") 
  :icon 152
  :doc "Ce module transforme le profil melodique des hauteurs d'une liste
de hauteurs <pitch> par le profil d'une deuxieme liste de hauteurs <prof>.
<prof> est une liste simple, a un niveau, de hauteurs en listents, qui
       definie un profil melodique.
<pitch> est une liste simple, a un niveau, de hauteurs en listents, qui
       definie soit un  reservoir de hauteurs, soit un  reservoir d'intervalles.
<mode?> est un menu deroulant qui permet de choisir le mode d'action du module.
        Si <mode?> est egal a 'note' la liste  <pitch> sera utilisee comme
        un reservoir  de hauteurs.
        Si <mode?> est egal a 'intrv' la liste  <pitch> sera utilisee comme
        un reservoir  d'intervalles.

Pratiquement la structure generee est constituee par les directions de <prof>
et les  hauteurs ou les intervalles de <pitch> en fonction de <mode?>."
  (funcall (case mode?
             (1 'cambia-prof-mel)
             (2 'prof-fix-note))
           prof pitch))
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Modulo;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! malt-mod+ ((list list) limit)
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 

            ""

  (let ((ris nil)
        (limite (first (om::list! limit))))
    
    (dolist (y list (nreverse ris))
      (push (if (< y limite)
              (- (* 2 limite) y) y) ris))))

;
;
;--------------------------------------
;
;
(om::defmethod! malt-mod- ((list list) limit )
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  
  ""
  
  (let ((ris nil)
        (limite (first (om::list! limit))))
    
    (dolist (y list (nreverse ris))
      (push (if (> y limite)
              (- (* 2 limite) y) y) ris))))
;
;
;--------------------------------------
;
(om::defmethod! reflex-int ((ls list) (value t) up/down)
  :initvals '('(1 2 3 4 5) 2 1)
  :menuins '((2 (("up" 1) ("down" 2))))
  :indoc '("list" "number" "") 
  :icon  128
  :doc  "Restituisce la rifleesione delle note che sono superiori o inferiori
             al valore indicato con 'value'. Il menu permette di selezionare se si
             vuole una riflessione superiore o inferiore"
  (case up/down
    (1 (malt-mod+ ls value))
    (2 (malt-mod- ls value))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Modulo con altezze fisse;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun mod-fix- (ls asse)
  ""
  (let ((ris nil)
        (asse (om::list! asse)))
    (dotimes (x (length ls) (nreverse ris))
      (push
       (if (<= (nth x ls) (first asse)) (nth x ls)
           (+ (first asse) (first (int-com (list
                                            (first asse)
                                            (nth x ls)))))) 
       ris))))
;
;
;--------------------------------------
;
;
(defun mod-fix+ (ls asse)

  ""
  (let ((ris nil)
        (asse (om::list! asse)))
    (dotimes (x (length ls) (nreverse ris))
      (push
       (if (>= (nth x ls) (first asse)) (nth x ls)
           (+ (first asse) (first (int-com (list
                                            (first asse)
                                            (nth x ls)))))) ris))))
;
;
;--------------------------------------
;

(om::defmethod! reflex-note ((ls list) (value t) up/down)
  :initvals '('(1 2 3 4 5) 2 1)
  :menuins '((2 (("up" 1) ("down" 1))))
  :indoc '("list" "number" "") 
  :icon  128
  :doc  "Restituisce per la riflessione superiore con <UP> e quella
             inferiore con <DOWN>."
  
  (case up/down
    (1 (mod-fix+ ls value))
    (2 (mod-fix- ls value))))
;
;
;--------------------------------------
;
;
(om::defmethod! doppio-reflex-note ((list list)
                                (value list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Restituisce due volte <REFLEX-NOTE> la prima volta a <LIST>
             la seconda volta al risultato della prima volta."
  
  (reflex-note (reflex-note list (om::list-min value) 1) 
               (om::list-max value) 2))
;
;
;--------------------------------------
;
;
(om::defmethod! doppio-reflex-int ((list list)
                               (value list)) 
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Restituisce due volte <REFLEX-INT> la prima volta a <LIST>
             la seconda volta al risultato della prima volta."
  
  
  (reflex-int (reflex-int list (om::list-min value) 1)
              (om::list-max value) 2))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Filtra banda;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun int (elt coppia)
  (if (< (first coppia) elt (second coppia)) elt nil))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Filtro passa banda;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! pass-band ((lista list) (alt list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 

            "Restituisce i valori inclusi in ALT."

  (let ((ris nil))

    (dolist (x lista (nreverse ris))
      (if (equalp (int x alt) nil) (int x alt) (push x ris)))))
;
;
;--------------------------------------
;
(om::defmethod! correttore-doppio-reflex-note ((list list)
                                               (value list)
                                               inclu?)
  :initvals '('(1 2 3 4 5) 2 1)
  :menuins '((2 (("yes" 1) ("no!" 2))))
  :indoc '("list" "number" "") 
  :icon  128
  :doc "Corregge il risultato di 'DOPPIO-REFLEX-NOTE' in modo che se la 
             riflessione supera i limiti con <YES> abbiamo una trasposizione
             oltre i limiti stessi ma con TRANS-APPROX altrimenti le note 
             che non sono incluse nei limiti vengono escluse dalla funzione
             COMP-OCTAVE."
  
  (let ((risultato (doppio-reflex-note list value )))
    
    (case inclu?
      (1 (trans-approx risultato value))
      (2 (comp-octave risultato value)))))
;
;
;--------------------------------------
;
;
(om::defmethod! correttore-doppio-reflex-int ((list list)
                                              (value list))
  
:initvals '('(1 2 3 4 5) 2) 
:indoc '("list" "number") 
:icon  128
:doc 
  
  "Corregge il risultato di 'DOPPIO-REFLEX-INT' in modo che 
             se il risultato di 'DOPPIO-REFLEX-INT supera i limiti dati
             ripete l'operazione di adattamento fino a che non soddisfa 
             i limiti di esistenza."
  
  (let ((risultato (doppio-reflex-int list value))
        (ris nil))
    
    
    (dolist (y risultato (om::flat (nreverse ris)))
      (push (if (int y value) y
                (correttore-doppio-reflex-int (om::list! (1+ y)) value)) ris))))
;
;
;--------------------------------------
;
(om::defmethod! reflexion ((list list) 
                           (axis list)
                           mode?
                           up/down)
  :initvals '('(1 2 3 4 5) 2 1 1)
  :menuins '((2 (("intrv" 1) ("note" 2)))
            (3 (("up" 1) ("down" 2))))
  :indoc '("list" "number" "" "") 
  :icon  152
  :doc "operation de symetrie au tour d'un axe <axis>.
Cette operation considere l'ensemble de hauteurs definies par <list> comme etant
un profil geometrique.

Exemple:
-----------------------
-----*-----------------
---*---*------------*--
-*------*--------*-----
---------*-----*------<<<<<<<<<<<<<axis>>>>>>>>>>>>>>>>>>>>>>>-
----------*---*--------
------------*----------
-----------------------
-----------------------

Il est possible alors de reflechir une partie des hauteurs de <list>
soit vers le haut:

------------------------
-----*------------------
---*---*----*-------*---
-*------*-*---*---*-----
---------*-----*------<<<<<<<<<<<<<axis>>>>>>>>>>>>>>>>>>>>>>>-
----------0---0---------
------------0-----------
------------------------
------------------------

soit vers le bas:

------------------------
-----0------------------
---0---0----0-------0---
-0------0-0---0---0-----
---------*-----*------<<<<<<<<<<<<<axis>>>>>>>>>>>>>>>>>>>>>>>-
-*------*-*---*---*-----
---*---*----*-------*---
-----*------------------
------------------------


<list>     est une liste simple, a un niveau, de hauteurs en listents.
<axis>     valeur en listents determinant l'axe au tour duquel s'opere la symetrie.
<mode?>    est un menu deroulant qui permet de choisir le mode d'action du module.
           Si <mode?> est egal a 'note' la liste  <list> sera utilisee comme
           un reservoir  de hauteurs, c'est-a-dire que la reflexion au tour de
           <axis> respectera les valeurs des notes de <list>.
           Si <mode?> est egal a 'intrv' la liste  <list> sera utilisee comme
           un reservoir  d'intervalles, c'est-a-dire que la reflexion au tour de
           <axis> respectera les valeurs des intervalles de <list>.
<up/down>  est un menu deroulant qui permet de choisir la direction de la reflexion.
           Si <up/down> est egal a 'up' la reflexion au tour de
           <axis>  se fera vers le haut.
           Si <up/down> est egal a 'down' la reflexion au tour de
           <axis>  se fera vers le bas.



"
  
  (funcall (case mode?
             (1 'reflex-int)
             (2 'reflex-note))
           list axis up/down))                     
;
;
;--------------------------------------
;
(om::defmethod! double-reflect ((list list) (limits list) mode? inclu?)
  :initvals '('(1 2 3 4 5) 2 1 1)
  :menuins '((2 (("intrv" 1) ("note" 2)))
            (3 (("yes" 1) ("no!" 2))))
  :indoc '("list" "number" "" "") 
  :icon  152
  :doc "operation de symetrie en relation a deux bornes. 
Cette operation considere l'ensemble de hauteurs definies par <list> comme etant
un profil geometrique.

Exemple:
*-------------------------
-*-------------*----------
--*-----------*-*---------
---*---------*---*--------
----*-------*-----*-------
-----*-----*-------*------
------*---*---------*-----
-------*-*-----------*----
--------*-------------*---

Il est possible alors de reflechir une partie des hauteurs de <list>
en relation a deux bornes

0-------------------------
-0-------------0----------
--0-----------0-0---------
---*---------*---*--------<-<-<-<borne inferieure
--*-*---*---*-*-*-*-------
-*---*-*-*-*---*---*------
*-----*---*---------*-----<-<-<-<borne superieure
-------0-0-----------0----
--------0-------------0---



<list>     est une liste simple, a un niveau, de hauteurs en listents.
<limits>   est une liste de deux valeurs en listents determinant les bornes 
           inferieur et superieur  au tour desquels s'opere la symetrie.
<mode?>    est un menu deroulant qui permet de choisir le mode d'action du module.
           Si <mode?> est egal a 'note' la liste  <list> sera utilisee comme
           un reservoir  de hauteurs, c'est-a-dire que la reflexion au tour de
           <axis> respectera les valeurs des notes de <list>.
           Si <mode?> est egal a 'intrv' la liste  <list> sera utilisee comme
           un reservoir  d'intervalles, c'est-a-dire que la reflexion au tour de
           <axis> respectera les valeurs des intervalles de <list>.
<inclu?>   est un menu deroulant qui permet de choisir l'inclusion ou non
           des notes qui ne seraient pas incluses dans l'intervalle delimite par <limits>
           Si <inclu?> est egal a 'yes' les notes non incluses dans 
           l'intervalle delimite par <limits> sont ajoutees a la borne la plus proche..
           Si <inclu?> est egal a 'no' les notes non incluses dans 
           l'intervalle delimite par <limits> sont exclues.


"
  
  (case mode?
    (1 (correttore-doppio-reflex-int list limits))
    (2 (correttore-doppio-reflex-note list limits inclu?))))
;
;
;--------------------------------------
;
;
(defun interno (elmt range)

          "Restituisce l'elemento se e incluso nel 'range' e nil 
           se non e incluso."

  (if (<= (om::list-min range) elmt (om::list-max range)) elmt nil))
;
;
;--------------------------------------
;
;
(om::defmethod! comp-octave ((list list) (range list))
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  
  "Restituisce una trasposizione della lista mantenendo le altezze
             assolute all'interno del 'range. Se un elemento non e incluso 
             nel 'range', allora viene tolto dal risultato."
  
  (let ((ris nil))
    
    (dolist (y (mio-transpoct list range) (nreverse ris))
      (if (equalp (interno y range) nil) (interno y range) (push y ris)))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;Riflessione che riparte dal limite;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Filtro passa alto;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! pass-alto ((lista list) (alt number))
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  
  "Restituisce tutti i valori superiori al vaore segnato in ALT."
  
  (let ((ris nil))
    
    (dolist (x lista (nreverse ris))
      (when (> x alt) (push x ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod!  pass-basso ((lista list) (alt number))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            "Restituisce tutti i valori inferiori al valore segnato in ALT."

  (let ((ris nil))

    (dolist (x lista (nreverse ris))
      (when (< x alt) (push x ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! quarta-reflexio ((list list) (limit list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Se un elemento di <LIST> e superiore al limite superiore allora viene trasposto 
             al di sotto di tale limite della differenza tra l'elemento stesso ed il limite 
             (if (> y maxim) (+ minim  (- y maxim))). 
             Se un elemento di <LIST> e inferiore al limite inferiore allora viene trasposto 
             al di sopra di tale limite della differenza tra il limite e l'elemento stesso 
             (if (< y minim) (- maxim  (-  minim y)))."
  
  
  (let ((ris nil)
        (minim (om::list-min limit))
        (maxim (om::list-max limit)))
    
    (dolist (y list (om::flat (nreverse ris)))
      (push (cond
             ((> y maxim) (+ minim  (- y maxim)))
             ((< y minim) (- maxim  (-  minim y)))
             (t y))
            ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! quasi-multi-reflexions ((list list) 
                                        (limit list))
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  
  "Se un elemento di <LIST> e superiore al limite superiore allora viene trasposto 
             al di sotto di tale limite della differenza tra l'elemento stesso ed il limite 
             (if (> y maxim) (+ minim  (- y maxim))). 
             Se un elemento di <LIST> e inferiore al limite inferiore allora viene trasposto 
             al di sopra di tale limite della differenza tra il limite e l'elemento stesso 
             (if (< y minim) (- maxim  (-  minim y))).
             Se la soluzione non soddisfa i limiti di esistenza dei limiti stessi, il risultato
             viene trattato ricorsivamente fino a soddisfare i due limiti."
  
  
  (let ((ris nil)
        (calcolo (quarta-reflexio list limit)))
    
    (dolist (y calcolo (om::flat (nreverse ris)))
      (push (if (int y limit) 
              y
              (quasi-multi-reflexions (om::list! (1+ y)) limit)) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! multi-reflect ((list list) (limits list) &optional (note? nil))
:initvals '('(1 2 3 4 5) 2 nil) 
:indoc '("list" "number")
:icon  152
:doc 
            
            "Operation de symetrie en relation a deux bornes. 
Cette operation considere l'ensemble de hauteurs definies par <list> comme etant
un profil geometrique.

Exemple:
*-------------------------
-*-------------*----------
--*-----------*-*---------
---*---------*---*--------
----*-------*-----*-------
-----*-----*-------*------
------*---*---------*-----
-------*-*-----------*----
--------*-------------*---

Il est possible alors de reflechir une partie des hauteurs de <list>
en relation a deux bornes

0-------------------------
-0-------------0----------
--0-----------0-0---------
---*---*-*---*---*--------<-<-<-<borne inferieure
*---*---*---*-----*-------
-*---*-----*---*---*------
--*---*---*---*-*---*-----<-<-<-<borne superieure
-------0-0-----------0----
--------0-------------0---

La difference entre ce module <multi-reflect> et <double-reflect>
est que ce module garde les directions au moment de la reflexion.



<list>     est une liste simple, a un niveau, de hauteurs en listents.
<limits>   est une liste de deux valeurs en listents determinant les bornes 
           inferieur et superieur  au tour desquels s'opere la symetrie.
<note?>    entree optionnelle qui permet l'ajustement de la forme generee
           a un champs harmonique quelconque.
           <note?> peut être soit une echelle soit un accord.


"
  
  (let* ((calcolone (quasi-multi-reflexions list limits))
         (con-note (when note?
                     (notes-change calcolone note? 48))))
    
    (if note? con-note calcolone)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Media fissa;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun primo-passo (lista n) 
  (let (f)                             ;prende n elementi della lista
    (dotimes (x n)                     ;di partenza
      (push (nth x lista) f))
    (nreverse f)))
;
;
;
;--------------------------------------
;
;
;
;(defun primo-passo (lista n) 
;  (let ((n (when (> n (length lista)) (length lista))))
;    (subseq lista 0 n)))
;
;--------------------------------------
;
;
(om::defmethod! scomp ((list1 list) (list2 list)) 
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 

            "Restituisce la nostra lista1 di partenza suddivisa nei segmenti
             da noi scelti tramite lista2."
   
  (let (ris)                                          ;Prende ogni elemento di lista2 e lo
    (dotimes (x (length list2) (nreverse ris))        ;usa per estrarre i valori da lista1.
      (push (primo-passo                              ;Per fare in modo che (nth x lista2)
             (nthcdr                                  ;non prenda il successivo di lista1
              (let (ros)                              ;uso (nthcdr) la cui y e data da
                (dotimes (y (length ris))             ;(apply '+ ros) cioe dalla somma
                  (push (nth y list2) ros))           ;delle y precedenti.
                (apply '+ ros))                       ;
              list1)                                  ;
             (nth x list2))                           ;
            ris))))              
;
;
;--------------------------------------
;
;
(defun med-fix (lista)

            "Restitusce la derivata data dalla media tra una nota e la successiva."

  (let ((ris nil))

    (dotimes (x (- (length lista) 1) (nreverse ris))
      (push (/ (+ (nth x lista) (nth (1+ x) lista)) 
               2) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! mean-derivation ((list list) 
                             (gr° number)
                             &optional (note? nil))
:initvals '('(6000 6600 7100) 1 nil) 
:indoc '("list" "number") 
:icon  152
:doc 
            
           "Operation de simplification de profils melodiques.
Ce module genere un profil melodique resultant d'un processus
de calcul de moyenne entre des notes consecutives de <list>.
Exemple:
soit <list> = (6000 5700 5100 5200 4900 5800 6400 7200 7800 7500 6600 6800)
et <gr°> = 1
Le resultat sera:
(5850 5400 5150 5050 5350 6100 6800 7500 7650 7050 6700)
ou

5850 est la moyenne entre 6000 et 5700,
5400 est la moyenne entre 5700 et 5100,
et ainsi de suite.
et  que le nombre d'elements est reduit de un
Si nous gardons la même <list> et nous faisons <gr°> = 2,
le resultat sera:
(5625 5275 5100 5200 5725 6450 7150 7575 7350 6875)
ou
5625 est la moyenne entre 5850 et 5400, 
5275 est la moyenne entre 5400 et 5150,  
et ainsi de suite.

(se rappeler que (5850 5400 5150...)
sont les resultats du processus avec  <gr°> = 1)

Nous pouvons calculer ce processus avec plusieurs degres de profondeur,
mais en se souvenant que  a chaque fois la liste resultante sera reduite d'un element

Il est possible aussi d'utiliser l'entree optionnelle <note?>
pour ajuster la forme generee a un champs harmonique particulier.

<list>     est une liste simple de hauteurs en listents.
<gr°>      est le niveau de profondeur du processus.
<note?>    entree optionnelle qui permet l'ajustement de la forme generee
           a un champs harmonique quelconque.
           <note?> peut être soit une echelle soit un accord."
  
  (let* ((calcolo (if (= 1 gr°) (med-fix list)
                     (mean-derivation (med-fix list) (- gr° 1)  note?)))
         (con-note (when note?
                     (notes-change calcolo note? 48))))
    
    (if note? con-note calcolo)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Derivate;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Derivata fissa;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Baricentro;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! baricentro ((valori list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 

            "Calcola il baricentro di una sequenza di valori: il valore medio." 
  
  (om::om-mean valori 1.0))
;
;
;--------------------------------------
;
;
(om::defmethod! deriv ((list list) start )
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  
  "Calcola la differenza (delta y) tra una altezza e la precedente e
             se si considera (= (delta x) 1) allora il risultato e la 'derivazione
             della lista <LIST>."
  
  (om::om+ (first (om::list! start)) (om::x->dx list)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Derivate successive;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! quasi-der-suc ((list list)
                               start
                               (note? list)
                               (gr° number))
  
  
  :initvals '('(1 2 3 4 5) 2 1 10)
  :menuins '((1 (("first" 0) ("orig" 1))))
  :indoc '("list" "number" "" "") 
  :icon  128
  :doc "Calcola la differenza (delta y) tra una altezza e la precedente e
             se si considera (= (delta x) 1) allora il risultato e la 'derivazione
             della lista <LIST>. Con <gr°> possiamo decidere di sapere quante volte
             vogliamo applicare la stessa operazione a <LIST>, in altre parole
             decidiamo il grado della derivazione da fare a <LIST>."
  
  (let ((partenza (case start
                    (1 (first list))
                    (2 note?))))
    (if (= gr° 1) 
      (deriv list partenza)
      (quasi-der-suc (deriv list partenza)
                     start note?
                     (- gr° 1)))))
;
;
;--------------------------------------
;
;
(om::defmethod! prime-note ((list list)
                        (gr° number))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Prende la prima nota di ogni derivata e ne fa una lista.
             Se <GR°> e uguale a 1 allora restituisce la prima nota 
             di <LIST>. Questo serve per la funzione <INTEGR-SUC> che
             abbisogna di ogni passo delle derivate per risalire alla
             lista <LIST> di partenza."
  
  (let ((ris (first list))
        (partenza (first list)))
    (if (= gr° 1) 
      (om::list! ris)
      (append (prime-note (deriv list partenza)
                          (- gr° 1)) (list  ris)))))
;
;
;--------------------------------------
;
(om::defmethod! derivation ((list list)
                            (start integer) 
                            (gr° number))
  :initvals '('(1 2 3 4 5) 1 1)
  :menuins '((1 (("first" 1) ("orig" 2))))
  :indoc '("list" "number" "") 
  :icon 152
  :doc "
--------*-*--------------------------
------*------------------------------
--*---------*------------------------
----*---------*---------*-*----------
*---------------*-----*--------------
------------------*-*----------------
-------------------------------------
-------------------------------------
-------------------------------------
Ce module est la transcription musicale de l'operation de derivation, appliquee
a un profil melodique. Dans ce cas precis l'intervalle tempporel entre les
hauteurs est considere egal a un (1). Le resultat de ce module est toujours une
liste de listes ou le premier element est le resultat de l'operation de derivation
represente comme une liste de hauteurs en listents. Et le reste des elements representent
le 'baricentre' des structures derivees. Si on procede a une derivation du premier degre
cette deuxieme partie de la liste contiendra un element, on procede a une derivation du 
deuxieme degre cette deuxieme partie de la liste contiendra deux elements, et ainsi de suite.


<list>     est soit une liste simple de hauteurs en listents representant un profil,
           spoit une liste de listes originaire d'un module <integration>.
<start>    menu derroulant qui permet de definir le mode de fonctionement de ce module.
           Si <start> egal a 'first' l'entree <list> doit être une liste simple de hauteurs,
           en listents, representant un profil, le resultat de l'operation sera la derivation
           de ce profil.
           Si <start> egal a 'orig' l'entree <list> doit être une liste de listes,
           originaire d'un module <integration>. Ce mode sert a la reconstitution d'un profil
           suite a des derivations successives.
          

<gr°>      est l'ordre de l'operation de derivation.




                  "
  (when (or (and (= start 1) (listp (first list))) (and (= start 2) (atom  (first list))))
    (error "ATTENTION!! Au format de la liste d'entree <list> et au menu <start>"))
  (let ((value (if (= start 2) (rest list) nil))
        (list (if (= start 1) list (first list))))
    
    (append (list (quasi-der-suc list start value gr°))
            (prime-note list gr°))))        
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Integrali;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! integr ((list list) start )
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "E' la funzione inversa alla funzione <DERIV>. In altre parole
             prende una lista <LIST> gli sottrae un elemento scelto in <START>
             e costruisce una lista con (om::dx->x) a partire da <START>."
  
  (om::dx->x (first (om::list! start)) (om::om- list (first (om::list! start)))))
;
;
;--------------------------------------
;
(om::defmethod! quasi-integr-suc ((list list)
                                  (start integer)
                                  (value list)
                                  (gr° number))
 
  :initvals '('(1 2 3 4 5) 1 nil 2)
  :menuins '((1 (("baric" 1) ("orig" 2))))
  :indoc '("list" "number" "" "") 
  :icon  128
  :doc "E' la funzione inversa alla funzione <DERIV-SUC>. In altre parole
             prende una lista <LIST> gli sottrae un elemento scelto in <START>
             e costruisce una lista con (om::dx->x) a partire da <START>.
             Con <GR°> possiamo decidere il numero di integrazioni successive
             da applicare ad una lista.
             Con <START> uguale BARIC allora la nota di partenza sara il BARICENTRO
             della lista.
             Con <START> uguale a NOTE? possiamo scegliere noi il valore su cui
             ricostruire la curva mettendo il valore in <VALUE>.
             Nel caso in cui avessimo fatto delle operazioni di derivazione
             successive su una lista di partenza e volessimo risalire alla lista 
             originaria allora <START> deve essere uguale ad ORIG."
  
  (let ((ris nil)
        (partenza (case start
                    (1 (baricentro list))
                    (2 (pop value)))))
    
    (if (= gr° 1)
      (push
       (integr list partenza) ris)
      (quasi-integr-suc (integr list partenza) start value (- gr° 1)))))
;
;
;--------------------------------------
;
(om::defmethod! integration ((list list)
                             (start integer)
                             (gr° number))
  :initvals '('(1 2 3 4 5) 1 1)
  :menuins '((1 (("baric" 1) ("orig" 2))))
  :indoc '("list" "number") 
  :icon 152
  :doc "
Ce module est la transcription musicale de l'operation d'integration, appliquee
a un profil melodique. Dans ce cas precis l'intervalle tempporel entre les
hauteurs est considere egal a un (1). Le resultat de ce module est toujours une
liste de listes ou le premier element est le resultat de l'operation d'integration
represente comme une liste de hauteurs en listents. Et le second  element represent
le 'baricentre' des structures integrees. Si on procede a une integration du premier degre
cette deuxieme partie de la liste contiendra un element, on procede a une integration du 
deuxieme degre cette deuxieme partie de la liste contiendra toujours un element, et ainsi de suite.


<list>     est soit une liste simple de hauteurs en listents representant un profil,
           spoit une liste de listes originaire d'un module <integration>.
<start>    menu derroulant qui permet de definir le mode de fonctionement de ce module.
           Si <start> egal a 'baric' l'entree <list> doit etre une liste simple de hauteurs,
           en listents, representant un profil, le resultat de l'operation sera l'integration
           de ce profil.
           Si <start> egal a 'orig' l'entree <list> doit etre une liste de listes,
           originaire d'un module <derivation>. Ce mode sert a la reconstitution d'un profil
           suite a des integrations successives.        
<gr°>      est l'ordre de l'operation d'integration. "
  (when (or (and (= start 1) (listp (first list))) (and (= start 2) (atom  (first list))))
    (error "ATTENTION!! Au format de la liste d'entree <list> et au menu <start>"))
  (let ((value (if (= start 2) (rest list) nil))
        (list (if (= start 1) list (first list))))
    (append (quasi-integr-suc list start value gr°)
            (list (baricentro list)))))
;
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Interposizione di due liste;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun lettura-modulare (lista1 lista2)

            "Se la prima lista e piu grande della seconda lista, allora legge 
             modularmente la seconda lista restituendo un length uguale al length
             di lista1."

  (let ((ros nil))
    (dotimes (y (length lista1) (nreverse ros))
      (push 
       (if (< (length lista2) (length lista1))
         (nth 
          (mod y (length lista2))
          lista2)
         (nth y lista2)) ros))))
;
;
;--------------------------------------
;
;
(om::defmethod! inter-profile ((list1 list) (list2 list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 

            ""

  (let ((ris nil)
        (y (lettura-modulare list1 list2)))
    
    (om::flat 
     (append 
      (dotimes (x (1- (length list1)) (nreverse ris))
        (push
         (om::mat-trans (list (list (nth x list1))
                              (list (trans-approx (list (nth x y))
                                                  (list (nth x list1) 
                                                        (nth (1+ x) list1)))))) ris))
     (last list1)))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;Interposizione di due liste completa;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
(om::defmethod! prof-inter ((list1 list) (list2 list) 
                            (total integer))
  :initvals '('(1 2 3 4 5) 1 1)
  :menuins '((2 (("ltd" 1) ("copl" 2))))
  :indoc '("list" "number" "") 
  :icon  128
  :doc "Restituisce l'interposizione di list1 con list2. Se list1 e piu piccola
             di list2 allora la funzione crea un'interposizione di n elementi di list2
             dove (= n (- (length list1) 1)). In questo caso si puo' decidere con il 
             menu se avere la prima ricorsione per completare list2."
  
  (case total
    (1 (inter-profile list1 list2))
    (2 (inter-profile (inter-profile list1 list2)
                      (om::rotate list2 (1- (length list1)))))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;Interposizione di due liste ricorsiva;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! pr-interlock ((list1 list) 
                           (list2 list) 
                           (gr number))
:initvals '('(1 2 3 4 5) 2) 
:indoc '("list" "number") 
:icon  152
:doc  "Enchevetrement des notes de la liste <list2> entre les notes de la liste <list1>,
avec un degre <gr> de profondeur variable.
L'interet de ce module reside dans le fait que n'importe quelles que soient
les notes de <list2> chaque fois qu'elles sont enchevetrees entre les notes
de <list1>, le module a le soin de changer la note d'octave de faÁon
qu'elle puisse etre entre les deux notes de <list1>.
Exemple:
soit un f#1 de <list2> qui devrait etre enchevetree entre une paire
e3 et a3 de <list1>. Le f#1 de <list2> serait, alors, transpose a f#3
pour que la sequence devienne: e3 f#3 a3. Au cas ou la note a etre enchevetree
ne soit pas contenu entre la paire de <list1>, elle sera alors transposee
vers l'octave la plus proche d'une des notes de la paire en question.
Exemple:
Soit le même f#1 de <list2> qui devrait etre enchevetree entre une paire
g3 et a#3 de <list1>. Le f#1 de <list2> serait, alors, transpose a f#3
pour que la sequence devienne:  f#3 g3 a#3. Le f#1 a ete transpose
le plus proche d'une des bornes de cette paire, dans ce cas precis a f#3
qui ete proche de g3.

<gr> est le niveau de profondeur du processus.
Exemple:
soit <list1> (6000* 4300* 6900* 5900*) et (pour des questions pedagogiques)
<list2> (6100 6300 6500 6600 6800 7000 7200) et <gr> = 1.
Le resultat sera:
(6000* 4900 4300* 6300 6900* 6500 5900*)  
                             [le * a ete ajoute pour marquer les notes de <list1>] 
Il est possible de remarquer deux choses:
 1) C'est la longueur de <list1> qui determine la fin du processus
 2) Le c#3 (6100) a ete transpose a c#2 (4900) pour pouvoir etre
    insere entre les deux notes de la premiere paire de <list1> ->(6000* 4300*)

Si on maintient les memes listes, <list1> et  <list2> mais on change
<gr°> a 2 nous aurons:
(6000* 5400 4900! 4400 4300* 5800 6300! 6000 6900* 7300 6500! 6300 5900*)
          [le * a ete ajoute pour marquer les notes de <list1> et ! pour 
           marquer les notes deja ajoutes lors du premier processus]
nous percevons que les notes ajoutees sont 5400 (f#2 transposition de f#3 6600
pour pouvoir être inseree entre 6000 et 4900),
4400 (g#1 transposition de g#3 6800 pour pouvoir inseree entre 4900 et 4300)
et ainsi de suite. C'est-a-dire a chaque niveau du processus le module
lit les notes de <list2> qui n'ont pas ete lues lors de l'operation du niveau precedant.
Au cas ou on aurait finit de lire toutes les notes de <list2> et il aurait encore des paires
a enchevetrer e module lirait circulairement <list2>.
Ce processus est une transcription musicale de l'algorithme nomme <Midpoint-
Displacement>,
utilise pour la construction de courbes fractales.


NOTE:  THIS FUNCTION HAS BEEN RENAMED 'PR-INTERLOCK' DUE TO A NAME CONFLICT WITH AN EXISTING OM FUNCTION
"
  
  (if (= gr 1) (inter-profile list1 list2)
      (pr-interlock (inter-profile list1 list2)
                 (om::rotate list2 (1- (length list1)))
                 (- gr 1))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Interpolazione con BPF;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! lin-list ((init number) (end number) (steps number))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "interpolation lineaire!!!!"
  
  (if (eq init end) (make-list  steps :initial-element init)
      (om::arithm-ser init end (/ (-  end init ) (+ 1 steps)))))
;
;(om::arithm-ser 20.0 21 5)
;--------------------------------------
;
;
(om::defmethod! cambia-ogni-accordo ((list list) (note? list)) 
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Effettua la trasposizione per ogni accordo o nota del materiale in <LIST>
             con ogni accordo o materiale in <NOTE?>. Se il length di NOTE? e inferiore
             a quello di list allora NOTE? viene riletto modularmente."
  (let ((ris nil)) 
    (dotimes (x (length list) (nreverse ris))
      (push (notes-change 
             (nth x list)
             (nth x (pr note? list)) 48) ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! inter-dyn0 ((init number) (end number) (steps number)
                            &optional (tab (make-instance 'om::bpf)) (inclu? 1))
 
  :initvals '(0 10 1 (make-instance 'om::bpf) 1)
  :menuins '((4 (("yes" 1) ("no" 2))))
  :icon  128
  :doc "Interpolation dynamique entre deux points avec la possibilite de definir
le parcours.

<begin>    valeur initiale, peut etre soit une valeur simple soit une liste,
<end>      valeur finale, peut etre soit une valeur simple soit une liste,
<steps>    nombre de pas pour l'interpolation
             
<tab>      cette entree accepte un module bpf  et realise l'interpolation 
           entre <begin> et <end> avec le profil dessine dans la bpf.
           Si aucun module <multi-bpf> est connecte l'interpolation sera lineaire.
<inclu?>   est un menu deroulant qui permet de choisir l'inclusion ou non
           des objets <begin> et <end>.
           Si <inclu?> est egal a 'yes' les objets  seront inclus dans 
           la liste de sortie.
           Si <inclu?> est egal a 'no' les objets  seront  supprimes de 
           la liste de sortie.
"
  
  (let* ((x-points (when tab (om::x-points tab)))
         (listetab (when tab (om::om-scale (om::bpf-sample tab (om::list-min x-points) (om::list-max x-points) (+ 2 steps)
                                                        1
                                                        4)
                                        0.0
                                        1.0)))
         (liste1 (if tab listetab (lin-list 0 1 steps)))
         (liste2 (om::om+ (om::om* liste1 (- end init)) init)))
    (case inclu?
      (1 liste2)
      (2 (butlast (rest liste2))))))
;
;
;--------------------------------------
;
;
(om::defmethod! inter-dyn ((begin list) 
                           (end list) 
                           (steps number)
                           (tab om::bpf) 
                           (inclu? integer)
                           &optional note? )
  
  :initvals '((0) (10) 1 (make-instance 'om::bpf) 1 nil)
  :menuins '((4 (("yes" 1) ("no" 2))))
  :icon  152
  :doc "Interpolation dynamique entre deux points avec la possibilite de definir
le parcours.

<begin>    valeur initiale, peut etre soit une valeur simple soit une liste,
<end>      valeur finale, peut etre soit une valeur simple soit une liste,
<steps>    nombre de pas pour l'interpolation
             
<tab>      cette entree accepte un module bpf  et realise l'interpolation 
           entre <begin> et <end> avec le profil dessine dans la bpf.
           Si aucun module <multi-bpf> est connecte l'interpolation sera lineaire.
<inclu?>   est un menu deroulant qui permet de choisir l'inclusion ou non
           des objets <begin> et <end>.
           Si <inclu?> est egal a 'yes' les objets  seront inclus dans 
           la liste de sortie.
           Si <inclu?> est egal a 'no' les objets  seront  supprimes de 
           la liste de sortie.

Entree optionnelle

<note?>    entree optionnelle qui permet l'ajustement de la sequence generee
           (exception faite a <begin> et <end>) a un (ou plusieurs) champs harmoniques.
           <note?> peut etre soit une liste simple ou soit une liste de listes.
           Si <note?> est une liste simple tous les objets seront ajustes
           en fonction des notes de cette liste.
           Si <note?> est une liste de listes a chaque objet genere sera
           fait correspondre une des sous listes. Si le nombre de sous listes
           est plus petit que la longueur de la liste des interpolations, la
           liste de listes <note?> sera lue circulairement.


"
  
  (let* ((initio (om::list! begin))
         (endio (om::list! end))
         (le-note (when note?
                    (if (atom (first note?))
                      (list note?) note?)))
         (interpo (om::mat-trans (mapcar #'(lambda 
                                             (liste1 liste2) 
                                             (inter-dyn0 liste1 liste2 steps tab 2))
                                         initio endio)))
         (con-scala (when note?
                      (if (= (length begin) 1)
                        (om::my-explode
                         (notes-change (om::flat interpo) (om::flat le-note) 48)
                         (length interpo))
                        (cambia-ogni-accordo interpo le-note))))
         (fine-calcolo (append (list begin)
                               (if note? con-scala interpo)
                               (list end))))
    
    
    (case inclu?
      (1 fine-calcolo)
      (2 (butlast (rest fine-calcolo))))))
;
;
;         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Interpolazione ad n punti;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
(om::defmethod! dyn-mult ((elmt list) 
                          (steps list) 
                          (tab om::bpf))
  :initvals '('(1 2 3 4 5) 1 (make-instance 'om::bpf))
  :indoc '("list" "number" "bpf") 
  :icon  128
  :doc "Restituisce l'interpolazione tra tutti gli elementi di 'elmt'.
             I passi e le curve sono definibili per ogni tratto dell'interpolazione
             con l'utilizzo di una o piu curve BPF."
  
  (om::flat-once
   (let ((ris nil)
         (passi (pr steps elmt))
         (curve (when tab (pr (om::list! tab) elmt))))
     
     
     (dotimes (x (- (length elmt) 1))
       (push (list (nth x elmt)) ris)
       (push  
        (inter-dyn (nth x elmt)
                   (nth (1+ x) elmt)
                   (nth x passi)
                   (nth x curve)
                   2)
        ris))
     (nreverse (push (list (first (om::last-elem elmt))) ris)))))
;
;
;--------------------------------------
;
(om::defmethod! interpol-multipla ((elmt list) 
                                   (steps list) 
                                   (tab om::bpf))
  :initvals '('(1 2 3 4 5) 2 (make-instance 'om::bpf)) :indoc '("list" "number" "bpf") :icon  128
  :doc "Restituisce l'interpolazione tra tutti gli elementi di 'elmt'.
             I passi e le curve sono definibili per ogni tratto dell'interpolazione
             con l'utilizzo di una o piu curve BPF."
  
  (om::flat-once
   (let ((ris nil)
         (passi (pr steps elmt))
         (curve (when tab (pr (om::list! tab) elmt))))
     
     
     (dotimes (x (- (length elmt) 1) (nreverse ris))
       
       (push  
        (list (inter-dyn (nth x elmt)
                         (nth (1+ x) elmt)
                         (nth x passi)
                         (nth x curve)
                         2))
        ris)))))
;
;
;--------------------------------------
;
;
(om::defmethod! our-interlock ((liste1 list) (liste2 list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "l'interlock se fait un a un et commence avec le
             premier element de liste1"
  
  (let ((longs (mapcar #'(lambda (x) (length x)) liste2))
        (aux nil)
        (int-list (om::flat-once liste2)) 
        (k 0) (p 0))
    
    (dotimes (n (1- (length liste1)) (om::x-append (reverse aux) (last liste1)))
      (push (nth n liste1) aux)
      
        (dotimes (m (nth n longs))
          (push (nth (+ k m) int-list) aux)
          (setf p m))
      (setf k (+ k p 1)))))
;
;
;--------------------------------------
;
(om::defmethod! interpol-mult-note? ((elmt list) 
                                     (steps list) 
                                     (tab om::bpf)
                                     (note? list))
  :initvals '('(1 2 3 4 5) 2 (make-instance 'om::bpf) (6000) ) :indoc '("list" "number" "bpf" "number") :icon  128
  :doc "Restituisce l'interpolazione tra tutti gli elementi di 'elmt'.
             I passi e le curve sono definibili per ogni tratto dell'interpolazione
             con l'utilizzo di una o piu curve BPF."
  
  (let* ((note? (if (atom (first note?)) (list note?) note?))
         (calcolo (interpol-multipla elmt steps tab))
         (longs (mapcar #' (lambda (x) (length x)) calcolo))
         (interpolazione (om::flat-once calcolo)))
    
    (our-interlock elmt (scomp (cambia-ogni-accordo interpolazione
                                                    note?) longs))))
;
;
;--------------------------------------
;
;
(om::defmethod! multi-interpol ((prof list) 
                                (n°elm list) 
                                (tab om::bpf)
                                &optional (note? '(6000 6100 6200 6300 6400 6500 6600 6700 6800 6900 7000 7100 )))
  
  :initvals '((0) (10) (make-instance 'om::bpf) (6000 6100 6200 6300 6400 6500 6600 6700 6800 6900 7000 7100 ))
  :icon  152
  :doc "Interpolation dynamique entre les elements d'un profil de base.
Ce module permet l'interpolation entre les elements d'une liste qu'ils soient
des notes ou des accords.

<prof>    est soit une liste simple, soit une liste de listes, de hauteurs en listents.
           
<n°elm>   est soit un nombre, soit une liste. Il est possible de choisir
          le nombre de pas d'interpolation entre les elements de <prof>.
          Si <n°elm> est un nombre, par exemple '3', nous allons ajouter 
          trois pas d'interpolation entre chaque paire d'elements de <prof>.
          Dans ce cas <n°elm> a une action globale.
          Mais il est aussi possible de rentrer dans <n°elm> une liste
          qui definirait un nombre de pas d'interpolation differents
          pour chaque paire d'elements de <prof>, ce qui nous permet
          un contrôle locale.
          Par exemple si <n°elm> egal a (3 4 5) nous aurons trois elements
          interpoles entre la premiere paire de valeurs, quatre entre 
          la deuxieme et cinq entre la troisieme.
          Si le nombre de points de <n°elm> est plus petit que le nombre 
          d'elements de <prof> moins un, la liste <n°elm> sera lue circulairement.
          Dans l'exemple au-dessus si <prof> a plus de quatre elements,
          le module recommencera la lecture du debut de <n°elm>, c'est-a-dire,
          entre la quatrieme paire d'elements de <prof> nous aurons trois elements,
          apres quatre, etc..
          
          
<tab>     cette entree accepte un module bpf  et realise l'interpolation 
          entre <begin> et <end> avec le profil dessine dans la bpf.
          Si aucun module <multi-bpf> est connecte l'interpolation sera lineaire.
          De la même maniere que <n°elm> il est possible de connecter soit un
          objet <multi-bpf>, soit une liste d'objets <multi-bpf>.
          Si <tab> est un objet <multi-bpf> l'interpolation entre toutes les paires
          de <prof> sera toujours avec la forme designee par <tab>.
          Dans ce cas <tab> a une action globale.
          Mais il est aussi possible de rentrer dans <tab> une liste
          qui definirait une direction d'interpolation differente
          pour chaque paire d'elements de <prof>, ce qui nous permet
          un contrôle locale.
          Si le nombre d'elements de <tab>> est plus petit que le nombre 
          d'elements de <prof> moins un, la liste <tab> sera lue circulairement.
         

<note?>    entree optionnelle qui permet l'ajustement de la sequence generee
           (exception faite a <begin> et <end>) a un (ou plusieurs) champs harmoniques.
           <note?> peut être soit une liste simple ou soit une liste de listes.
           Si <note?> est une liste simple tous les objets seront ajustes
           en fonction des notes de cette liste.
           Si <note?> est une liste de listes a chaque objet genere sera
           fait correspondre une des sous listes. Si le nombre de sous listes
           est plus petit que la longueur de la liste des interpolations, la
           liste de listes <note?> sera lue circulairement.


"
  
  (let* ((steps (om::list! n°elm))
         (interpo (dyn-mult prof steps tab))
         (interpo-note (when note?
                         (if (atom (first prof))
                           (interpol-mult-note? (om::my-explode prof (length prof)) steps tab note?)
                           (interpol-mult-note? prof steps tab note?)))))
    
    (if note? interpo-note interpo)))
         
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Interferenza di una lista;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! prof-pert ((ls list) (range number))
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  
  "Restituisce una perturbazione casuale 'random' della lista 
            aggiungendo o sottraendo dei valori scelti casualmente tra 
            +range e -range."
  
  (mapcar #' (lambda (x) (+ x (om::random (- range) range))) ls))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;Interpolazione con note prestabilite;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! tutti-int ((list list) (ref number)) 
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Calcola gli intervalli che ci sono fra una lista di note ed
             un'unica nota di riferimento."
  
  
  
  (let ((ris nil))
    
    (dolist (y list)
      (push (om::x->dx (list ref y)) ris))
    (om::flat (nreverse ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! segno+picc ((list list)) 
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 

            "Trasforma tutta la lista in valori tutti positivi e prende il valore
             piu piccolo."

  (om::list-min (mapcar #' (lambda (x) (abs x)) list)))
;
;
;--------------------------------------
;
;
(om::defmethod! nota-vicina ((list list) (ref number)) 
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "Prende l'intervallo piu piccolo di una lista."
  
  (let* ((intervalli (tutti-int list ref))
         (piccolo (segno+picc intervalli)))
    
    
    (if
      (equalp (abs (first intervalli)) piccolo)
      (first intervalli)
      (nota-vicina (rest list) ref))))
;
;
;--------------------------------------
;
;
(om::defmethod! tieni-nota ((list list) (ref number)) 
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
            
            "tiene la nota piu vicina."
  
  (om::om+ ref (nota-vicina list ref)))
;
;
;--------------------------------------
;
;
(om::defmethod! vicine-note ((list1 list) (refs list)) 
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  
  "Prende le note piu vicine di list per ogni nota di refs."
  
  
  (let ((ris nil))
    
    (dotimes (x (length refs) (nreverse ris))
      (push (tieni-nota list1 (nth x refs)) ris))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Trasposizione controllata;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun int-com-ottava (lista)
  
  "Restituisce l'intervallo complementare ad ull'intervallo in 'lista'
   ma all'interno di un'ottava."
  
  
  (let ((ris nil))
    
    (om::flat
     (dotimes (x (- (length lista) 1) (nreverse ris))
       (push 
        (om::x->dx (append
                    (list (nth x lista))
                    (list  
                     (- (nth x lista)
                        (* (- 12 (mod 
                                  (/ (- (first (om::x->dx lista)) 
                                        (* 
                                         (first 
                                          (om::om// (om::x->dx lista) 1200)) 1200)) 100) 12)) 100)))))
        ris)))))
;
;
;--------------------------------------
;
;
(defun mio-transpoct (list range)

            "Restituisce lo stesso risultato di 'transpoct' della libreria Esquisse"

  (let ((ris nil))

    (dolist (y list (nreverse ris))
      (push 
       (cond ((< y (om::list-min range))
              (+ (om::list-min range)
                 (+ 1200 (first (int-com-ottava (list (om::list-min range) y))))))
             ((> y (om::list-max range))
              (+ 
               (om::list-max range) 
               (first (int-com-ottava (list (om::list-max range) y)))))
             (y))
       ris))))
;
;
;--------------------------------------
;
;
(om::defmethod! correttore ((elmt number) (range list))
:initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 

          "Restituisce un elemento se questo compare all'interno del range.
           Se l'elemento e escluso allora lo traspone in modo tale che sia
           il piu vicino possibile o al limite superiore o a quello inferiore.
           Se il limite e DO-SOL allora Mi viene incluso, SI viene trasposto
           sotto il DO e il SOL# viene trasposto sopra il SOL."

  (let ((max (om::list-max range))
        (min (om::list-min range)))
    (cond ((<= (om::list-min range) elmt max) 
           elmt)
          ((cond ((< elmt min)
                  (cond ((<= (- min elmt) (- (+ 1200 elmt) max))
                         elmt)
                        ((> (- min elmt) (- (+ 1200 elmt) max))
                         (+ 1200 elmt))))
                 ((> elmt max)
                  (cond ((<= (- elmt max) (- min (- elmt 1200)))
                         elmt)
                        ((> (- elmt max) (- min (- elmt 1200)))
                         (- elmt 1200)))))))))
;
;
;--------------------------------------
;
;
(defun cor-ott-list (elmt range)
        
          "Restituisce un elemento se questo compare all'interno del range.
           Se l'elemento e escluso allora lo traspone in modo tale che sia
           il piu vicino possibile o al limite superiore o a quello inferiore.
           Se il limite e DO-SOL allora Mi viene incluso, SI viene trasposto
           sotto il DO e il SOL# viene trasposto sopra il SOL.La differenza
           con 'CORRETTORE' e che questo modulo agisce su una lista intera."
        
        (let ((ris nil))
          (dolist (y elmt)
            (push (correttore y range) ris))
          (nreverse ris)))
;
;
;--------------------------------------
;
;
(om::defmethod! trans-approx ((list list) (range list))
  :initvals '('(1 2 3 4 5) 2) :indoc '("list" "number") :icon  128
  :doc 
  "E' meglio di transpoct di Esquisse. Infatti attua lo stesso
             procedimento ma traspone una nota non inclusa nel range il piu
             vicino o al limite superiore o a quello inferiore."
  
  (cor-ott-list (mio-transpoct list range) range))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;Interpolazione con note prestabilite;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
(om::defmethod! interpol-prof  ((prof1 list)  
                                (prof2 list)
                                (steps number)
                                (nbr-n list)
                                (tab om::bpf)
                                (note? list)
                                (precis number)
                                (approx number)) 
  :initvals '('(6000 6600 7100) (4200 3200 5000 6400 7200) 10 nil (make-instance 'om::bpf) nil 10 2) 
  :icon  152
  :doc "Interpolation entre deux profils melodiques <prof1> et <prof2> de tailles quelconques!!!.
Ce module se prete a l'interpolation quand le profil est important.
La sortie de ce module est une liste de listes ou chaque sous-liste
correspond a un profil.

<prof1>    est une liste simple, de hauteurs en listents, qui
           definie un profil melodique. 
<prof2>    est une liste simple, de hauteurs en listents, qui
           definie un deuxieme profil melodique.    
<steps>    nombre de pas d'interpolation

<nbr-n>    nombre de notes dans chaque profil genere. 
           Si aucun nombre ou liste est donnee, chaque profil intermediaire
           aura un nombre de notes correspondant a une interpolation lineaire
           entre le nombre de notes de <prof1> et le nombre de notes de <prof2>
           Si <nbr-n> est un entier, comme par exemple '5', tous les profils generes (a 
l'exception de
          <prof1> et <prof2>) auront cinq notes.
           Si <nbr-n> est une liste, chaque profil genere aura  un nombre d'elements
           correspondant a une valeur de <nbr-n>. Par exemple si <nbr-n> est
           (2 3 4 5 6 1 2), le premier profil aura deux notes, le deuxieme profil
           aura trois notes, le troisieme profil aura quatre notes, et ainsi de suite.

<tab>      cette entree accepte un module bpf  et realise l'interpolation 
           entre <prof1> et <prof2> avec le profil dessine dans la bpf.
           Si aucun module <multi-bpf> est connecte l'interpolation entre <prof1> et <prof2>
           sera lineaire.


<note?>    entree  qui permet l'ajustement de la sequence generee
           (exception faite a <prof1> et <prof2>) a un (ou plusieurs) champs harmoniques.
           <note?> peut etre soit une liste simple ou soit une liste de listes.
           Si <note?> est une liste simple tous les objets seront ajustes
           en fonction des notes de cette liste.
           Si <note?> est une liste de listes a chaque objet genere sera
           fait correspondre une des sous listes. Si le nombre de sous listes
           est plus petit que la longueur de la liste des interpolations, la
           liste de listes <note?> sera lue circulairement.

<precis>   Comme ce module n'interpole pas des notes, mais des profils,
           un des pas dans sont processus est la conversion de la liste,
           soit <prof1> ou <prof2>, en un profil. Pour cela il est necessaire
           de definir un 'taux d'echantillonnage'. Le parametre <precis> 
           defini un 'taux d'echantillonnage' egal a  
           <precis>*(plus grande longueur entre <prof1> et <prof2>).
           Ce qui defini une valeur minimale de '1' pour  <precis>.
           En fonction de l'utilisation il est necessaire d'ajuster ce parametre.
           Une valeur egale a '5' nous a paru largement suffisante dans tous les essais
           preliminaires.

<approx>  approximation des resultats.
          <approx> = 4 l'approximation sera de quart de ton,
          <approx> = 2 l'approximation sera de demi  ton,
          <approx> = 8 l'approximation sera de huitieme de ton, et ainsi de suite.

           "
  
  (let* ((prof1 (om::list! prof1))
         (prof2 (om::list! prof2))
         (lungo1 (length prof1))
         (lungo2 (length prof2))
         (le-note (when note?
                    (if (atom (first note?))
                      (list note?) note?)))
         (bpf1 (om::simple-bpf-from-list  (om::arithm-ser 0 (1- (* precis 10 lungo1)) (* 10 precis) ) prof1))
         (bpf2 (om::simple-bpf-from-list  (om::arithm-ser 0 (1- (* precis 10 lungo2)) (* 10 precis) ) prof2))
         (liste-interpolations (bpf-interpolx bpf1 bpf2 
                                              (* precis 
                                                 (om::list-max (list lungo1 lungo2)))
                                              2 steps tab 1))
         (nbr-n (cond 
                 ((null nbr-n) (lin-list lungo1 lungo2 steps))
                 ((numberp nbr-n) (om::create-list (- steps 2) nbr-n))
                 (t (om::x-append lungo1 (pr nbr-n (om::create-list (- steps 2) nbr-n)) lungo2))))
         (calcolone (mapcar #'(lambda (x ech) (om::approx-m (om::bpf-sample x  
                                                            (om::list-min (om::x-points x))
                                                            (om::list-max (om::x-points x))
                                                            (round ech)
                                                            1
                                                            4) approx))  liste-interpolations nbr-n))
         (con-note (when note?
                     (append (list prof1)
                             (cambia-ogni-accordo (rest (butlast calcolone)) le-note)
                             (list prof2)))))

    (if note? con-note calcolone)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UTILITA';;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Range-approx;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
(om::defmethod! range-approx ((list list)
                              (limit list)
                              (inclu? integer))
  :initvals '('(1 2 3 4 5) '(1 2 3 4 5) 2)
  :menuins '((2 (("yes" 1) ("non" 2))))
  :indoc '("list" "number" "") 
  :icon  152
  :doc 
  
  "Transposition des notes (en listents) de <list> dans le registre
defini par la liste <limit>.
<list>    liste de hauteurs en listents
<limit>   liste de deux elements, en listents, qui definissent le registre
          dans lequel les notes de <list> seront transposees.
<inclu?>   est un menu deroulant qui permet de choisir l'inclusion ou non
           des notes qui ne seraient pas incluses dans le registre defini par <limit>.
           Si <inclu?> est egal a 'yes' les notes non incluses dans 
           cet intervalle  sont ajoutees a la borne la plus proche.
           Si <inclu?> est egal a 'no' les notes non incluses dans 
           cet intervalle  sont exclues.

"
  
  (funcall (case inclu?
             (1 'trans-approx)
             (2 'comp-octave))
           list limit))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Scompositore;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
(om::defmethod! segment ((liste list) 
                         (place number) 
                         (n-elem number)
                         (lecture integer))
  
  :initvals '('(1 2 3 4 5) 2 1 1) 
  :menuins '( (3 (("lin" 1) ("circ" 2))))
  :indoc '("list" "number" "" "") 
  :icon  128
  :doc "retire les <n-elem> elements de la liste <liste> a partir de la place
            <place>. OBS: place=0 c'est-a-dire premier element de liste.
            Il est possible de choisir deux types de lectures -lineaire- ou -circulaire-"
  
  (case lecture
    (1 (if (> (length liste) (+ place n-elem)) 
         (subseq  liste place  (+ place n-elem))
         (nthcdr place (butlast liste 0))))
    (2 (let ((aux))
         (dotimes (n n-elem (reverse aux))
           (push (nth (mod (+ n place ) (length liste)) liste) aux))))))
;
;
;--------------------------------------
;
;
(defun crea-spazio (list segm)
  (let ((place (om::dx->x 0 segm)))
    (remove nil (mapcar #'(lambda (pl el) (segment list pl el 1)) place segm))))
;
;
;--------------------------------------
;


(om::defmethod! pr-group-list ((list list) 
                            (group list)
                            mode?) 
  :initvals '('(1 2 3 4 5) 2 1) 
  :menuins '( (2 (("stop" 1) ("circ" 2) ("scal" 3))))
  :indoc '("list" "number" "") 
  :icon  152
  :doc  "Articulation d'une liste <list> en segments de longueurs definis
par une deuxieme liste de nombres entiers <group>.

<list>  est une liste quelconque
<group> est une liste de nombres qui definissent des longueurs
        de segments.

Par exemple soit la liste <list>
(a b c d e f g h i j k l m)
et la liste <group>
(4 2 1 3 3)
le resultat sera:
PW->((a b c d) (e f) (g) (h i j) (k l m))

<mode?> est un menu deroulant qui defini le mode de fonctionnement de 
        ce module.
        Si <mode?> egal 'stop' la segmentation se fera lineairement, 
        c'est-a-dire, meme si la liste <group> contient plus d'element
        ou si la somme de ses elements est superieure a la longueur
        de <list> la segmentation se fera jusqu'a la fin des elements
        de <list>. Exemple: soit <list> (a b c d e f g h i j k l m)
        et <group> (4 2 1 3 5) le resultat sera toujours:
        PW->((a b c d) (e f) (g) (h i j) (k l m)). Ou si <group> egal (4 2 1 3 5 2)
        le resultat sera toujours:
        PW->((a b c d) (e f) (g) (h i j) (k l m)).

        Si <mode?> egal 'circ' la segmentation se fera circulairement.
        c'est-a-dire, que pour constituer les segments imposes par <group>,
        le module ajoutera des elements du debut de <list>.
        Exemple: soit <list> (a b c d e f g h i j k l m)
        et <group> (4 2 1 3 5 2) le resultat sera alors
        PW->((a b c d) (e f) (g) (h i j) (k l m a b) (c d)).

        Si <mode?> egal 'scal' la segmentation se fera proportionnellement.
        L'articulation prendra en compte plutôt les proportion entre les
        elements de <group> et constituera des segments avec tous les 
        elements de <list>.Exemple soit la liste <list> de 12 elements
        (a b c d e f g h i j k l) et la liste <group> (5 3 4).
        Logiquement le resultat sera 
        PW->((a b c d e) (f g h) (i j k l)).
        Mais si nous gardons la meme <list> et nous changeons <group>
        pour (10 3 8) le resultat sera toujours le meme:
        PW->((a b c d e) (f g h) (i j k l)).       


NOTE: THIS FUNCTION HAS BEEN RENAMED PR-GROUP-LIST DUE TO A NAME CONFLICT WITH OM GROUP LIST.
"
  
  (let ((calcolo (pr list (om::create-list (apply'+ group) 0))))
    
    (case mode?
      (1 (crea-spazio list group))
      (2 (scomp calcolo group))
      (3 (crea-spazio list (om::om-round 
                      (om::scale/sum group (length list))))))))
;
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;substitute;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
(om::defmethod! subst-list ((list list) 
                            (new list)
                            (old list)
                            (start number)
                            (count number)
                            &optional (test 'equal))
  
  
  
  :initvals '('(1 2 3 4 5) '(1 2 3 4 5) '(1 2 3 4 5) 1 2 'equal) 
  :indoc '("list" "number" "" "" "" "") 
  :icon  152
  :doc  "Ce module permet la substitution de tout element <old>
appartenant a <list> par un nouveau element <new>.
<list>  est la liste de reference
<old>   est l'element a retirer de <list>,
        il peut être un nombre, une liste ou un symbole  
<new>   est l'element a substituer dans <list> a la place de <old>,
        il peut être un nombre, une liste ou un symbole                           
<start> est un indice qui indique a partir de quel element de la liste
        se fera la substitution. '0' (zero) est le premier element.
<count> est un indice qui indique combien d'elements <old> de <list>
        seront substitues.
<test>  est un predicat optionnel pour la comparaison des elements.
        Il est possible que pour certaines applications les elements a
        substituer et implicitement a comparer soient de types diverses,
        ce qui peut exiger l'utilisation de predicats de comparaison
        speciaux. Le predicat par defaut est 'equalp' qui est un predicat
        d'egalite faible, se pretant bien a la comparaison entre nombres,
        listes et symboles.
"
  (let ((old (cond  
              ((atom old) old) 
              ((listp old) (if (= 1 (length old)) (first old) old)))))
    (substitute new
                old
                list
                :start start
                :count count
                :test test)))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
(om::defmethod! interpol-tab ((begin list) 
                              (end list) 
                              (steps number)
                              (tab om::bpf) 
                              (inclu? integer))

  :initvals '('(1 2 3 4 5) 2 1 (make-instance 'om::bpf) 1) 
  :menuins '( (4 (("yes" 1) ("no" 2))))
  :indoc '("list" "number" "" "" "") 
  :icon  152
  :doc "Interpolation dynamique entre deux points avec la possibilite de definir
le parcours.

<begin>    valeur initiale, peut etre soit une valeur simple soit une liste,
<end>      valeur finale, peut etre soit une valeur simple soit une liste,
<steps>    nombre de pas pour l'interpolation
             
<tab>      cette entree accepte un module bpf  et realise l'interpolation 
           entre <begin> et <end> avec le profil dessine dans la bpf.
           Si aucun module <multi-bpf> est connecte l'interpolation sera lineaire.
<inclu?>   est un menu deroulant qui permet de choisir l'inclusion ou non
           des objets <begin> et <end>.
           Si <inclu?> est egal a 'yes' les objets  seront inclus dans 
           la liste de sortie.
           Si <inclu?> est egal a 'no' les objets  seront  supprimes de 
           la liste de sortie.

"
  
  (let ((initio (om::list! begin))
        (endio (om::list! end)))
   (om::mat-trans (mapcar #'(lambda  (liste1 liste2) 
                               (inter-dyn0  liste1 liste2 steps tab inclu?))
                           initio endio))))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(om::defmethod! weight-average ((list list))
  :icon 152
  :doc "Calcule le barycentre de l'ensemble de hauteurs <list>"
  
  (baricentro list))
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Interpolazione di BPF;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;
;
(om::defmethod! BPF-interpolx  ((bpf1 om::bpf)  (bpf2 om::bpf)
                                (echantillons number)
                                (approx number) 
                                (steps number)
                                (tab om::bpf)
                                (mode integer)) 
  :initvals '((make-instance 'om::bpf) (make-instance 'om::bpf) 10 10 10 (make-instance 'om::bpf) 1) 
  :menuins '( (6 (("bpf" 1) ("list" 2))))
  :icon  152
  :doc  "Interpolation entre deux profils  <bpf1> et <bpf2> de tailles quelconques!!!.
Ce module se prete a l'interpolation quand le profil est important.
La sortie de ce module est soit une liste de listes ou chaque sous-liste
contient les coordonnees correspondantes a un profil, soit une liste
d'objets bpf.

<bpf1>    est une liste simple, de hauteurs en listents, qui
           definie un profil melodique. 
<bpf2>    est une liste simple, de hauteurs en listents, qui
           definie un deuxieme profil melodique.  
 
<echantillons>   Comme ce module n'interpole pas des points, mais des profils,
                 un des pas dans son processus est la conversion de la liste,
                 soit <bpf1> ou <bpf2>, en un profil. Pour cela il est necessaire
                 de definir un 'taux d'echantillonnage'. Le parametre <echantillons> 
                 defini un nombre de points necessaires pour cet echantillonnage.
                 <echantillons> est une espece de 'taux d'echantillonnage' .
                 Pour des utilisations courantes la valeur '20' nous a semble suffisante.
 
<steps>    nombre de pas d'interpolation

<tab>      cette entree accepte un module bpf  et realise l'interpolation 
           entre <bpf1> et <bpf2> avec le profil dessine dans la bpf.
           Si aucun module <multi-bpf> est connecte l'interpolation entre <bpf1> et <bpf2>
           sera lineaire.

<mode>     menu deroulant qui defini la sortie de ce module.
           Si <mode> egal a <bpf> la sortie sera une liste d'objets BPF.
           Si <mode> egal a <liste> la sortie sera une liste de
           listes ou chaque sous liste contient deux sous listes.
           La premiere etant les abscisses et la deuxieme les coordonnees
           de chaque profil.
           "
  
  (let* ((xbpf1 (om::x-points bpf1))
         (xbpf2 (om::x-points bpf2))
         (x-max-bpf1 (om::list-max xbpf1))
         (x-min-bpf1 (om::list-min xbpf1))
         (x-max-bpf2 (om::list-max xbpf2))
         (x-min-bpf2 (om::list-min xbpf2))
         (databpfy1 
          (om::bpf-sample bpf1  x-min-bpf1 x-max-bpf1 echantillons 1 2))
         (databpfx1 (om::arithm-ser x-min-bpf1
                                    x-max-bpf1
                                    (/ (- x-max-bpf1 
                                          x-min-bpf1) 
                                       (- echantillons 1))))
         (databpfy2 
          (om::bpf-sample bpf2  x-min-bpf2 x-max-bpf2 echantillons 1 2))
         (databpfx2 (om::arithm-ser x-min-bpf2
                                    x-max-bpf2
                                    (/ (- x-max-bpf2 
                                          x-min-bpf2) 
                                       (- echantillons 1))))
         (coordonx (om::om-round (interpol-tab databpfx1 databpfx2  steps tab 1) approx))
         (coordony (om::om-round (interpol-tab databpfy1 databpfy2  steps tab  1) approx)))
    
    (case mode
      (1 (mapcar #'(lambda (x  y color)
                     (let ((newbpf (om::simple-bpf-from-list x  y)))
                       (setf (om::bpfcolor newbpf) color)
                       newbpf)) (om::om-round coordonx) (om::om-round coordony) 
                 (om::om-interpole-colors (om::bpfcolor bpf1) (om::bpfcolor bpf2) steps)))
      (2 (om::mat-trans (list coordonx coordony))))))


;
;
;
