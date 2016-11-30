;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  IRCAM - Music Represetation Team
;  Copyright (C) 1997-2010 IRCAM-Centre Georges Pompidou, Paris, France.
;
;  Harmonic Project
;  Construction of apreggios
;  C. Truchet, J. Bresson
;=========================================================================

(in-package :om)


(defun aux-expand (liste liste1 maxamb &optional prov)
  (cond ((null liste) (aux-expand (om+ liste1 1200) (om+ liste1 1200) maxamb prov))
        ((> (car liste) maxamb) prov)
        (t (aux-expand (cdr liste) liste1 maxamb (cons (car liste) prov)))))

(defun expand (sliste ambitus)
  (let* ((min (car sliste))
         (maxamb (+ min ambitus)))
    (remove nil (aux-expand sliste sliste maxamb))))

(defun diviseentier (n p &optional prov)
  (if (< n p) (append prov (list (- 0 n)))
      (let ((r (1+ (om-random-value (div p 2)))))
        (diviseentier (- n (* 2 r)) p
                      (cons (- 0 r) (cons r prov))))))

;; les notes de passage sont uniquement entre les extremes de la tierce

(defun passage (listenotes ambitus)
  (let* ((tonalite (get-tonalite (car listenotes))))
    (when tonalite
      (let* ((mode (mode tonalite))
             (l (length mode))
             (listeoctave
              (loop for i from 1 to (1- (length listenotes))
                    append (let* ((note (nth i listenotes))
                                  (noteprec (nth (1- i) listenotes))
                                  (d (get-degre note))
                                  (dprec (get-degre noteprec)))
                     (if (= 2 (mod (degre- d dprec) l))
                       (list (om- (midic note) (om- (nth (degre- d 1) mode)
                                                    (nth (mod (degre- d 2) l) mode)))))))))
        (expand listeoctave ambitus)))))

 
;; on les ajoute apres, la broderie est uniquement entre la note a broder

#|
(defun etrangeres (listenotes ambitus)
  (let* ((tonalite (get-tonalite (car listenotes))))
    (when tonalite
      (let* ((mode (mode tonalite))
             (l (length mode))
             (listeoctave
              (loop for i from 1 to (1- (length listenotes))
                    append (let* ((note (nth i listenotes))
                                  (d (get-degre note)))
                             (list (om- (midic note) (om- (nth (degre- d 1) mode)
                                                          (nth (mod (degre- d 2) l) mode)))
                                   (om+ (midic note) (om- (nth (degremod d (length mode)) mode)
                                                          (nth (degre- d 1) mode))))))))
        (expand listeoctave ambitus)))))
|#

(defun tireunhasard (reservoir taille)
  (nth (om-random-value taille) reservoir))


(defun tirehasard (n reservoir taille)
  (if (zerop n) nil
      (let ((h (tireunhasard reservoir taille)))
        (cons h
              (tirehasard (1- n) (remove-un h reservoir) (1- taille))))))

(defun tirehasard2 (n reservoir precedent &optional hautbas)
  (if (zerop n) nil
      (let* ((h 
              (if hautbas
                (+ 1 precedent (om-random-value (1- (- (length reservoir) precedent))))
                (om-random-value precedent)))
             (noteh (nth h reservoir)))
           (cons noteh
                 (tirehasard2 (1- n) reservoir h (not hautbas))))))


(defun alterne (l)
  (cond ((null l) nil)
        ((null (cdr l)) (list (car l)))
        (t (cons (cadr l) (cons (car l) (alterne (cddr l)))))))     

;;DEBUTMODIF 27MARS


(defun remove-un (el l)
  (cond ((null l) nil)
        ((equal (car l) el) (cdr l))
        (t (cons (car l) (remove-un el (cdr l))))))


(defun alterne-k (l k)
  (cond ((or (null l) (>= k (length l)) (< k 1)) nil)
        ((or (null (cdr l)) (>= k (length l)) (< k 1)) nil)
        (t (let ((el (nth k l)))
             (cons el (cons (car l) (alterne-k (remove-un el (cdr l)) (1- k))))))))

(defun mode-degre2 (degre mode)
  (let ((autredeg (deploiemineur degre)))
    (if (zerop autredeg) (- (car (last mode)) 1200)
        (nth (1- autredeg) mode))
    ))

(defun ajoute-etrangeres (self nbetrangeres listemidi brise?)
  (if (> nbetrangeres (length listemidi))
    (ajoute-etrangeres self (length listemidi) listemidi brise?)
    (let* ((mode (mode self))
           (tonmidi (tonmidi self))
           ;  (listehasard (tirehasardindice nbetrangeres listemidi (length listemidi)))
           ;  (notesetr (mapcar #'car listehasard))
           (llistemidi (length listemidi))
           (indicesetr (tirehasard nbetrangeres (arithm-ser 0 (1- llistemidi) 1) llistemidi))
           (midicetr (loop for i from 0 to (1- llistemidi)
                           append (if (member i indicesetr)
                                    (let* ((note (nth i listemidi))
                                           (hautbas (om-random-value 2))
                                           (d (degre-mode (mod (- note tonmidi) 1200) mode))
                                           (liste (list
                                                   note
                                                   (if (= hautbas 0)
                                                     (progn
                                                       ;(print "----------------")
                                                       ;(print (degre+ d 1))
                                                       ;(print (mode-degre2 (degre+ d 1) (print mode)))
                                                       (om+ (tonreference note d mode)
                                                            (mode-degre2 (degre+ d 1) mode))
                                                       )
                                                     (om+ (tonreference note d mode)
                                                          (mode-degre2 (degremod (degre- d 1) 7) mode)))
                                                   note)))
                                      ;(if brise? (sort liste #'<) (sort liste #'>))
                                      liste)
                                    (list (nth i listemidi))))))
      midicetr)))


(defun ajoute-broderies (self nbbroderies listemidi reservoirnotes asc?)
  (if (> nbbroderies (length listemidi))
    (ajoute-broderies self (length listemidi) listemidi reservoirnotes asc?)
    (let* ((mode (mode self))
           (tonmidi (tonmidi self))
           ;  (listehasard (tirehasardindice nbetrangeres listemidi (length listemidi)))
           ;  (notesetr (mapcar #'car listehasard))
           (llistemidi (length listemidi))
           (indicesetr (tirehasard nbbroderies (arithm-ser 0 (1- llistemidi) 1) llistemidi))
           (restereservoir reservoirnotes)
           (midicetr (loop for i from 0 to (1- llistemidi)
                           append (if (member i indicesetr)
                                    (let* ((note (nth i listemidi))
                                           (d (degre-mode (mod (- note tonmidi) 1200) mode))
                                           (autrenote (om+ (tonreference note d mode)
                                                           (mode-degre2 (degremod (degre+ d 2) 7) mode))))
                                      
                                      (if (member autrenote restereservoir)
                                        (let ((liste (list note
                                                           (om+ (tonreference note d mode)
                                                                (mode-degre2 (degre+ d 1) mode))
                                                           autrenote
                                                           )))
                                          (setf restereservoir (remove note (remove autrenote restereservoir)))
                                          (if asc? (sort liste #'<) (sort liste #'>))
                                          )
                                        (list note)))
                                    (list (nth i listemidi))))))
      midicetr)))


(defun ajoute-redoublements (self reservoir)
  (let ((mode (mode self))
        (tonmidi (tonmidi self)))
    (loop for note in reservoir
          append (let ((d (degre-mode (mod (- note tonmidi) 1200) mode)))
                   (cond ((= d 5) (list note note))
                         ((= d 3) (list note note))
                         (t (list note)))))))




(defun removesucc (l)
  (cond ((null l) nil)
        ((null (cdr l)) (list (car l)))
        ((equal (car l) (cadr l)) (removesucc (cdr l)))
        (t (cons (car l) (removesucc (cdr l))))))

#|
(defmethod unarpege ((self chord) reservoirnotes nbnotes asc? brise? etrangeres broderies redoublement)
  (let* ((tonalite (or (get-tonalite self) (make-instance (get-tonalite-class self))))
         (taille (length reservoirnotes))
         (nbnormales (- (- nbnotes etrangeres) broderies))
         (premier (car reservoirnotes))
         (dernier (car (last reservoirnotes)))
         (reservoirnotes2 (remove-dup (cdr (butlast reservoirnotes)) #'equal 1))
         (reservoirnotes3 (if redoublement (ajoute-redoublements tonalite reservoirnotes2) reservoirnotes2))
         (listemidi (cons premier (cons dernier
                                        (remove nil (tirehasard (if (< nbnotes taille) nbnotes taille) reservoirnotes3 taille)))))
         (listemiditriee (if asc? (sort listemidi #'<) (sort listemidi #'>)))
         (listemidibrisee (if brise? (cons (car listemiditriee) 
                                           (alterne-k (cdr listemiditriee) (1+ (om-random-value 4))))
                              listemiditriee))
         (listemidibrodee (if (> broderies 0) (ajoute-broderies tonalite broderies listemidibrisee reservoirnotes asc?) listemidibrisee))
         (listemidifin2 (if (> etrangeres 0) (ajoute-etrangeres tonalite etrangeres listemidibrodee asc?) listemidibrodee))
         (listemidifin (removesucc listemidifin2)))
    listemidifin))
|#
         
(defmethod unarpege ((self chord) reservoirnotes nbnotes asc? brise? etrangeres broderies redoublement)
  (let* ((tonalite (or (get-tonalite self) (make-instance (get-tonalite-class self))))
         (taille (length reservoirnotes))
         (reservoirnotes (sort reservoirnotes #'<))
         (nbnormales (- (- nbnotes etrangeres) broderies))
         (premier (car reservoirnotes))
         (dernier (car (last reservoirnotes)))
         ; (reservoirnotes2 (remove-dup (cdr (butlast reservoirnotes)) #'equal 1))
         (reservoirnotes2 (remove-dup reservoirnotes #'equal 1)))
    ;(print premier)
    (if brise?
      (let*
        ((listemidi (cons premier (append 
                                   (remove nil (tirehasard2 nbnotes reservoirnotes2 0))
                                   (list dernier))))
         (listemidibrodee (if (> broderies 0) (ajoute-broderies tonalite broderies listemidi reservoirnotes asc?) listemidi))
         (listemidifin2 (if (> etrangeres 0) (ajoute-etrangeres tonalite etrangeres listemidibrodee asc?) listemidibrodee))
         (listemidifin (removesucc listemidifin2)))
        listemidifin)
      
      (let*
        ((listemidi (cons premier (append 
                                   (remove nil (tirehasard (if (< nbnotes taille) nbnotes taille) reservoirnotes2 taille))
                                   (list dernier))))
         (listemiditriee (if asc? (sort listemidi #'<) (sort listemidi #'>)))
         (listemidibrodee (if (> broderies 0) (ajoute-broderies tonalite broderies listemiditriee reservoirnotes asc?) listemiditriee))
         (listemidifin2 (if (> etrangeres 0) (ajoute-etrangeres tonalite etrangeres listemidibrodee brise?) listemidibrodee))
         (listemidifin3 (if asc? (sort listemidifin2 #'<) (sort listemidifin2 #'>)))
         (listemidifin (removesucc listemidifin3)))
        listemidifin))))

(defmethod arpeges ((self chord) ambitus rythme mesure &optional (asc? t) brise? (etrangeres 0) (broderies 0) redoublement)
  (let* ((tonalite (or (get-tonalite self) (make-instance (get-tonalite-class self))))
         (lmidic (sort (lmidic self) #'<))
         (notes (inside self))
         (mode (mode tonalite))
         (degres (mapcar #'(lambda (x) (degre (tonal-values x))) notes))
         (ambitusmidic (degre->midic ambitus mode))
         (reservoirnotes (expand lmidic ambitusmidic))
         ;  (taille (length reservoirnotes))
         (new (clone self))
         (nbnotes (if (numberp rythme) 
                    (/ (car mesure) rythme)
                    (count-if  #'(lambda (n) (> n 0)) (tree2ratio rythme)))) ;;; j
         (listemidi (unarpege self reservoirnotes nbnotes asc? brise? etrangeres broderies redoublement))
         (r (if (numberp rythme) (loop for i from 1 to (length listemidi)
                                       collect (/ rythme (cadr mesure)))))
         (arbre
          (if (numberp rythme) (mktree r mesure) rythme))
         (new (make-instance 'voice
                :tree arbre
                :chords (loop for el in listemidi
                              collect (make-instance 'chord
                                        :lmidic (list el))))))
    new))





;;; conversion rythme
;;; conversion proba
(defmethod mk-arpeges ((self chord) ambitus figure-rythm mesure nbmesures &optional (asc? t) brise? (proba-etrangeres 0) (proba-passage 0) redoublement)
  (let* ((tonalite (or (get-tonalite self) (make-instance (get-tonalite-class self))))
         (lmidic (sort (lmidic self) #'<))
         (notes (inside self))
         (mode (mode tonalite))
         (degres (mapcar #'(lambda (x) (degre (tonal-values x))) notes))
         (ambitusmidic (degre->midic ambitus mode))
         (reservoirnotes (expand lmidic ambitusmidic))
         (taille-reserve (length reservoirnotes))
         (etrangeres (round (* taille-reserve (/ proba-etrangeres 100.0))))
         (broderies (round (* taille-reserve (/ proba-passage 100.0))))
         
         (symb/puls (/ figure-rythm (second mesure)))
         (numsymboles (* (car mesure) symb/puls))
         (nbnotes (* numsymboles nbmesures))

         (listemidi (unarpege self reservoirnotes nbnotes asc? brise? etrangeres broderies redoublement))
         (taille (length listemidi))
         
         (j 0)
         (r (flat (loop for m from 1 to nbmesures collect
                     (loop for i from 1 to numsymboles 
                           collect (* (/ 1 figure-rythm) (if (<= (incf j) taille) 1 -1))))))
         (arbre (mktree r mesure))
         
         (new (make-instance 'voice
                :tree arbre
                :chords (loop for el in listemidi
                              collect (make-instance 'chord
                                        :lmidic (list el))))))
    (set-tonalite new (tonalite self))
    new))
  
  



;;;==== CONSTRUCTION D'ARPEGE - JEAN


(defun random-bool (proba)
  (< (om-random 0.0 1.0) proba))

;;; ajoute une note de passage (si possible) sur la note pos de list
(defun add-passage (list pos notes tonalite)
  (let* ((mode (mode tonalite))
         (tonmidi (tonmidi tonalite))
         (note (car (last (list! (nth pos list)))))
         (d (degre-mode (mod (- note tonmidi) 1200) mode))
         (autrenote+ 
          (+ note (- (degre->midic (degre+ d 2) mode) (degre->midic d mode))))
         (autrenote- 
          (+ note (- (degre->midic (degre- d 2) mode) (degre->midic d mode))))
         passage note2)
    (unless (member autrenote+ notes) (setf autrenote+ nil))
    (unless (member autrenote- notes) (setf autrenote- nil))
    (cond 
     ((and autrenote+ autrenote-)
      (if (= (om-random-value 2) 0)
        (progn 
          (setf passage (+ note (- (degre->midic (degre+ d 1) mode) (degre->midic d mode))))
          (insert-passage list pos passage autrenote+)
          )
        (progn
          (setf passage (+ note (- (degre->midic (degre- d 1) mode) (degre->midic d mode))))
          (insert-passage list pos passage autrenote-)
          )))
     (autrenote+ (setf passage (+ note (- (degre->midic (degre+ d 1) mode) (degre->midic d mode))))
                 (insert-passage list pos passage autrenote+)
                 )
     (autrenote- (setf passage (+ note (- (degre->midic (degre- d 1) mode) (degre->midic d mode))))
                 (insert-passage list pos passage autrenote-))
     (t nil))
    (remove nil list))
)

(defun insert-passage (list pos passage autrenote)
  (if (and (< pos (- (length list) 1)) 
           (= autrenote (car (list! (nth (+ pos 1) list)))))
    (progn
      (setf (nth pos list) (append (list! (nth pos list)) (list passage) (list! (nth (+ pos 1) list))))
      (setf (nth (+ pos 1) list) nil)
      (setf list (remove nil list)))
    (setf (nth pos list) 
          (append (list! (nth pos list))
                  (list passage autrenote)))
    ))

   
(defun +ou- (a b)
  (if (random-bool 0.5)
    (+ a b) (- a b)))

;;; ajoute une broderie sur la note pos de list
(defun add-broderie (list pos tonalite)
  (let* ((mode (mode tonalite))
        (tonmidi (tonmidi tonalite))
        (hautbas (om-random-value 2))
        (note (car (list! (nth pos list))))
        (d (degre-mode (mod (- note tonmidi) 1200) mode))
        (broderie
         (if (= hautbas 0)
           (+ note (- (degre->midic (degre+ d 1) mode) (degre->midic d mode)))
           ;(om+ (tonreference note d mode) (mode-degre2 (degre+ d 1) mode))
           (+ note (- (degre->midic (degre- d 1) mode) (degre->midic d mode)))
           ;(om+ (tonreference note d mode) (mode-degre2 (degremod (degre- d 1) 7) mode))
           )
         )
        )
    (when (and broderie (not (= broderie note)))
      (setf (nth pos list) 
            (append 
             (if (and (> pos 0) (= note (car (last (list! (nth (- pos 1) list))))))
               (list broderie)
               (list note broderie))
             (list! (nth pos list)))
            ))
            ))



(defun arpege-jean (lista num sens pass brod ton)
  (setf lista (remove-duplicates lista :test '=))
  (let ((rep nil))
    (cond 
     ;;; 1 : on prend au hasard
     ((= 1 num) (setf rep (list (nth-random lista))))
     ;;; moins que le reservoir
     ((<= num (length lista)) 
      (let ((copilist (copy-list lista))
            (tmp nil))
        ;;; si on veut un arpege asc ou desc : on prend deja la premiere et la derniere
        (when sens 
          (setf rep (list (car lista) (car (last lista))))
          (setf copilist (cdr (butlast copilist))))
        ;;; ensuite on puise dans le reservoir une de chaque maxi 

        (loop while (< (length rep) num) do
              (setf tmp (tireunhasard copilist (length copilist)))
              (setf copilist (remove tmp copilist))
              (if rep
                (setf rep (insert-in-list rep tmp (om-random 0 (- (length rep) 1))))
                (setf rep (list tmp)))
              )
        
        ;;; si asc ou desc : trier
        (when sens
          (setf rep (if (equal 'asc sens)
                      (sort rep '<) (sort rep '>)))
          )
        ))
     ;;; plus que le reservoir
     ((> num (length lista))
      ;;; comme le cas d'avant
      (setf rep (arpege-jean lista (length lista) sens pass brod ton))
      ;;;ajouter broderies ou passages
      (when (>= (length lista) 2)
      (loop for i from 0 to (- (length rep) 2)
            while (< i (- (length rep) 2))
            while (< (length (flat rep)) num) do
            (when (degre-mode (mod (- (car (list! (nth i rep))) (tonmidi ton)) 1200) (mode ton))
              (when (random-bool brod)
                (add-broderie rep i ton))
              (when (and (< (length (flat rep)) num) (random-bool pass))
                (setf rep (add-passage rep i lista ton))))
            ))
      
      ;;; + piocher dans le reservoir pour remplir
      
      (loop while (< (length (flat rep)) num) do
            (let ((pos (om-random 1 (length rep)))
                  (note (tireunhasard lista (length lista))))
              ;(print (list pos note))
              ;(print lista)
              ;(print rep) 
              (setf rep (insert-in-list rep note pos))
              (when (and (degre-mode (mod (- note (tonmidi ton)) 1200) (mode ton))
                         (< pos (- (length rep) 1)))
                (when (and (< (length (flat rep)) num) (random-bool pass))
                  (setf rep (add-passage rep pos lista ton))
                  )
                (when (and (< (length (flat rep)) num) (random-bool brod))
                  (add-broderie rep pos ton))
                )
              (unless (< (length lista) 2) 
                (when (= (car (list! (nth pos rep))) (car (last (list! (nth (- pos 1) rep)))))
                  (setf (nth (- pos 1) rep) (butlast (list! (nth (- pos 1) rep)))))
                (when (and (< pos (- (length rep) 1)) (= (car (last (list! (nth pos rep)))) (car (list! (nth (+ pos 1) rep)))))
                  (setf (nth pos rep) (butlast (list! (nth pos rep)))))
                )
              (setf rep (remove nil rep))
              )
            )
      ))
    (flat rep)
    ))
      

(defmethod mk-arpege ((self chord) ambitus figure-rythm tempo mesure nbmesures &optional (sens nil) (proba-broderies 0) (proba-passage 0) redoublement)
  
  (let* ((tonalite (or (get-tonalite self) (make-instance (get-tonalite-class self))))
         (lmidic (sort (lmidic self) #'<))
         (notes (inside self))
         (mode (mode tonalite))
         (degres (mapcar #'(lambda (x) (degre (tonal-values x))) notes))
         (ambitusmidic (degre->midic ambitus mode))
         (reservoirnotes (expand lmidic ambitusmidic))
         (broderies (/ proba-broderies 100.0))
         (passage (/ proba-passage 100.0))
         (symb/puls (/ figure-rythm (second mesure)))
         (numsymboles (* (car mesure) symb/puls))
         (nbnotes (* numsymboles nbmesures))
         (listemidi (arpege-jean reservoirnotes nbnotes sens passage broderies tonalite))
         (taille (length listemidi))
         (j 0)
         (r (flat (loop for m from 1 to nbmesures collect
                     (loop for i from 1 to numsymboles 
                           collect (* (/ 1 figure-rythm) (if (<= (incf j) taille) 1 -1))))))
         (arbre (mktree r mesure))
         
         (new (make-instance 'voice
                :tree arbre
                :tempo tempo
                :chords (loop for el in listemidi
                              collect (make-instance 'chord
                                        :lmidic (list el)))))
         )
    (set-tonalite new (tonalite self))
    new))

  