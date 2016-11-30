;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  IRCAM - Music Represetation Team
;  Copyright (C) 1997-2010 IRCAM-Centre Georges Pompidou, Paris, France.
;
;  Harmonic Project
;  Automatic figure & cadence detection 
;  C. Truchet, 2004
;=========================================================================


(in-package :om)

(defmethod lmod12 ((l list))
  (mapcar #'(lambda (x) (mod x 1200)) l))

(defmethod modnil (a b)
  (if (and a b) (mod a b)
      nil))

(defun contientnil (l)
  (if (null l) nil
      (if (car l) (contientnil (cdr l))
          t)))

(defmethod renverse ((self chord))
  (let* ((lm (sort (copy-list (lmidic self)) '<))
         (new (clone self)))
    (setf (lmidic new) (append (cdr lm) (list (om+ 1200 (car lm)))))
    (actualise-tonalite new)
    new))

(defmethod renverse-n ((self chord) &optional (n 1))
  (if (zerop n) self
      (renverse-n (renverse self) (- n 1))))

;; On enleve des notes doublees
(defmethod chiffrage-midi ((self chord))
  (let* ((lm (sort  (lmidic self) '<))
         (diff (remove-dup (lmod12 (om- lm (car lm))) #'equal 1)))
    diff))

(defmethod chiffrages-midi ((self chord-seq))
  (let* ((accords (inside self)))
    (mapcar #'chiffrage-midi accords)))


;; idem on enleve les notes doublees
(defmethod chiffrage ((self chord))
  (let* ((notes (sort (copy-list (inside self)) #'< :key #'midic))
         ;(basse (degre (tonal-values (car notes))))
         (basse (get-degre (car notes)))
         (preres (mapcar 
                  #'(lambda (x) (let ((ddd (modnil (degre- (get-degre x) basse) 7)))
                                  (and ddd (om+ 1 ddd))))
                  notes))
         (res (remove-dup preres #'equal 1)))
    (list res basse)))

(defmethod chiffrage ((self chord-seq))
  (let* ((accords (inside self)))
    (mapcar #'chiffrage accords)))

(defmethod chiff-renversement ((self chord))
  (let* ((notes (inside self))
         (nbnotes (length notes))
         (lmrenv (loop for i from 1 to nbnotes
                       collect (let ((re (set-do-majeur (renverse-n self i))))
                                  (chiffrage re)
                                 re)))
         )
    lmrenv))

(defmethod tous-renversements ((self chord))
  (when (get-tonalite self)
    (let* ((notes (inside self))
           (nbnotes (length notes))
           (lmrenv (loop for i from 0 to (1- nbnotes)
                         collect (let ((re (renverse-n self i)))
                                   re)))
           )
      lmrenv)))


(defmethod get-note-in-chord ((self chord) notemidi)
  (let* ((notes (inside self))
         (tonalite (get-tonalite self)))
    (when notemidi
      (car (loop for el in notes
                 when (equal (mod notemidi 1200) (mod (midic el) 1200))
                 collect (get-degre el))))))


;; utilitaire
(defun debutliste (l1 l2 &optional (prov t))
  (if (null l2) prov
      (debutliste (cdr l1) (cdr l2) (and (equal (car l1) (car l2)) prov))))



(defmethod chiffrage-chiffre ((self chord))
  (let* ((ch (chiffrage self))
         (prech-deg (car ch)))
    (if (contientnil prech-deg) nil
        (let* ((ch-deg  (cdr (sort prech-deg #'<)))
               (ch-midi (cdr (sort (chiffrage-midi self) #'<)))
               (basse (cadr ch))
               ;(basse (car (list! (cadr ch))))
               (lm (sort (lmidic self) #'<))
               (mini (apply #'min lm))
               (tonalite (get-tonalite self))
               (ch
                (cond 
                 ((equal ch-deg '(3 5))
                  (cond 
                   ((or (debutliste ch-midi '(400 700)) (debutliste ch-midi '(300 700))) (list mini "5"))
                   ((and (debutliste ch-midi '(300 600)) (equal basse 7)) (list mini "5/"))))
                 ((equal ch-deg '(3 6))
                  (cond 
                   ((debutliste ch-midi '(300 800)) (list (+ mini 800) "6"))
                   ((debutliste ch-midi '(400 900)) (list (+ mini 900) "6"))
                   ((and (debutliste ch-midi '(300 900)) (equal basse 2)) (list (+ mini 900) "+6 3"))))
                 ((equal ch-deg '(4 6))
                  (cond
                   ((or (debutliste ch-midi '(500 900)) (debutliste ch-midi '(500 800))) (list (+ mini 500) "6 4"))
                   ((and (debutliste ch-midi '(600 900)) (equal basse 4)) (list (+ mini 600) "6 +4"))))
                 ((equal ch-deg '(3 5 7))
                  (cond 
                   ((and (debutliste ch-midi '(300 700 1000)) (equal basse 2)) (list mini "7"))
                   ((and (debutliste ch-midi '(400 700 1100)) (equal basse 4)) (list mini "7"))
                   ((and (debutliste ch-midi '(400 700 1000)) (equal basse 5)) (list mini "7 +"))
                   ((and (debutliste ch-midi '(400 700 1000)) (equal basse '(7 d))) (list mini "7/"))
                   ((and (debutliste ch-midi '(300 600 1000)) (equal basse 7)) (list mini "7 5/"))))
                 ((equal ch-deg '(3 7))
                  (cond 
                   ((and (debutliste ch-midi '(300 1000)) (equal basse 2)) (list mini "7"))
                   ((and (debutliste ch-midi '(400 1100)) (equal basse 4)) (list mini "7"))
                   ((and (debutliste ch-midi '(400 1000)) (equal basse 5)) (list mini "7 +"))
                   ((and (debutliste ch-midi '(400 1000)) (equal basse '(7 d))) (list mini "7/"))
                   ((and (debutliste ch-midi '(300 1000)) (equal basse 7)) (list mini "7 5/"))))
                 ((equal ch-deg '(3 5 6))
                  (cond ((and (debutliste ch-midi '(400 700 900)) (equal basse 4)) (list (+ mini 900) "6 5"))
                        ((and (debutliste ch-midi '(300 700 800)) (equal basse 6)) (list (+ mini 800) "6 5"))
                        ((and (debutliste ch-midi '(300 600 800)) (equal basse 7)) (list (+ mini 800) "6 5/"))
                        ;; new
                        ((and (debutliste ch-midi '(300 600 800)) (equal basse '(7 a))) (list (+ mini 800) "6 5/"))
                        ;;
                        ((and (debutliste ch-midi '(300 600 800)) (equal basse 2)) (list (+ mini 800) "+6 5/"))
                        ((and (debutliste ch-midi '(300 700 900)) (equal basse 2)) (list (+ mini 900) "+6 5"))))
                 ((equal ch-deg '(5 6))
                  (cond ((and (debutliste ch-midi '( 700 900)) (equal basse 4)) (list (+ mini 900) "6 5"))
                        ((and (debutliste ch-midi '( 700 800)) (equal basse 6)) (list (+ mini 800) "6 5"))
                        ((and (debutliste ch-midi '( 600 800)) (equal basse 7)) (list (+ mini 800) "6 5/"))
                        ;; new
                        ((and (debutliste ch-midi '( 600 800)) (equal basse '(7 a))) (list (+ mini 800) "6 5/"))
                        ;;
                        ((and (debutliste ch-midi '( 600 800)) (equal basse 2)) (list (+ mini 800) "+6 5/"))
                        ((and (debutliste ch-midi '( 700 900)) (equal basse 2)) (list (+ mini 900) "+6 5"))))
                 ((equal ch-deg '(3 4 6))
                  (cond
                   ((and (debutliste ch-midi '(300 500 800)) (equal basse 6)) (list (+ mini 500) "4 3"))
                   ((and (debutliste ch-midi '(400 500 900)) (equal basse 1)) (list (+ mini 500) "4 3"))
                   ((and (debutliste ch-midi '(300 500 900)) (equal basse 2)) (list (+ mini 500) "+6"))
                   ((and (debutliste ch-midi '(300 500 900)) (equal basse 4)) (list (+ mini 500) "+4 3"))
                   ((and (debutliste ch-midi '(400 600 900)) (equal basse 4)) (list (+ mini 600) "+4 3"))))
                 ((equal ch-deg '(3 6))
                  (cond
                   ((and (debutliste ch-midi '(300 800)) (equal basse 6)) (list (+ mini 500) "4 3"))
                   ((and (debutliste ch-midi '(400 900)) (equal basse 1)) (list (+ mini 500) "4 3"))
                   ((and (debutliste ch-midi '(300 900)) (equal basse 2)) (list (+ mini 500) "+6"))
                   ((and (debutliste ch-midi '(300 900)) (equal basse 4)) (list (+ mini 500) "+4 3"))
                   ((and (debutliste ch-midi '(400 900)) (equal basse 4)) (list (+ mini 600) "+4 3"))))
                 ((equal ch-deg '(2 4 6))
                  (cond
                   ((and (debutliste ch-midi '(200 500 900)) (equal basse 1)) (list (+ mini 200) "2"))
                   ;; new
                   ((and (debutliste ch-midi '(200 500 800)) (equal basse 1)) (list (+ mini 200) "2"))
                   ;;
                   ((and (debutliste ch-midi '(100 500 800)) (equal basse 3)) (list (+ mini 100) "2"))
                   ((and (debutliste ch-midi '(200 600 900)) (equal basse 4)) (list (+ mini 200) "+4"))
                   ((and (debutliste ch-midi '(200 600 900)) (equal basse '(6 d))) (list (+ mini 200) "+2"))
                   ((and (debutliste ch-midi '(200 500 800)) (equal basse 6) (list (+ mini 200) "+2")))))
                 ((equal ch-deg '(2 4))
                  (cond
                   ((and (debutliste ch-midi '(200 500)) (equal basse 1)) (list (+ mini 200) "2"))
                   ((and (debutliste ch-midi '(100 500)) (equal basse 3)) (list (+ mini 100) "2"))
                   ((and (debutliste ch-midi '(200 600)) (equal basse 4)) (list (+ mini 200) "+4"))
                   ((and (debutliste ch-midi '(200 600)) (equal basse '(6 d))) (list (+ mini 200) "+2"))
                   ((and (debutliste ch-midi '(200 500)) (equal basse 6) (list (+ mini 200) "+2")))))
                 ((and (equal ch-deg '(2 3 5 7)) (debutliste ch-midi '(200 400 700 1000)) (equal basse 5)) (list mini "9 7 +"))
                 ((and (equal ch-deg '(3 5 6 7)) (debutliste ch-midi '(300 600 800 1000)) (equal basse 7)) (list (+ mini 800) "7 6 5"))
                 ((and (equal ch-deg '(3 4 5 6)) (debutliste ch-midi '(300 500 700 900)) (equal basse 2)) (list (+ mini 500) "5 +6 4"))
                 ((and (equal ch-deg '(2 3 4 6)) (debutliste ch-midi '(200 400 600 900)) (equal basse 4)) (list (+ mini 200) "10 +4")))))
          (list (get-note-in-chord self (car ch)) (cadr ch))))))




;;;;==============================================================
(defmethod reset-cadence ((self chord-seq))
  (loop for el in (inside self)
        do (setf (cadence (tonal-values el)) nil)))

(defmethod cadence? ((self chord-seq))
  (when (get-tonalite self)
    (reset-cadence self)
    (let* ((accords (inside self))
           (predegres (mapcar #'(lambda (x) (get-degre x)) accords))
           (degres (mapcar #'(lambda (x) (if (consp x) (car x) x)) predegres)))
      (loop for i from 1 to (1- (length degres))
            do (let* ((degi (nth (1- i) degres))
                      (degi1 (nth i degres)))
                 (cond 
                  ((and (equal degi 5) (equal degi1 1))
                   (progn
                     (setf (cadence (tonal-values (nth i accords))) "parfaite")
                     (setf (cadence (tonal-values (nth i accords))) "parfaite")))
                  ;; I IV I pour la plagale
                  ((and (equal degi 4) (equal degi1 1))
                   (progn
                     (setf (cadence (tonal-values (nth i accords))) "plagale")
                     (setf (cadence (tonal-values (nth i accords))) "plagale")))
                  ((and (equal degi 5) (equal degi1 6))
                   (progn
                     (setf (cadence (tonal-values (nth i accords))) "rompue")
                     (setf (cadence (tonal-values (nth i accords))) "rompue")))
                  ((and (or (equal degi1 4) (equal 2 degi1)) (equal degi 5))
                   (setf (cadence (tonal-values (nth i accords))) "demi")
                   )))))))





