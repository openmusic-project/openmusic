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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================
;  Harmonic Project
;  Utilities
;  C. Truchet, J. Bresson
;=========================================================================


(in-package :om)

;;;====== UTILS======

(defun div (n p)
  (/ (- n (mod n p)) p))


(defun altere (midi alteration)
  (cond ((equal alteration 'becarre) midi)
        ((equal alteration 'diese) (+ midi 100))
        ((equal alteration 'bemol) (- midi 100))
        ((consp alteration) (loop for item in alteration do (setf midi (altere midi item))) midi)
        (t midi)
))

(defun notes->midic (note alteration)
  (altere
   (cond ((equal note 'do) 6000)
         ((equal note 're) 6200)
         ((equal note 'mi) 6400)
         ((equal note 'fa) 6500)
         ((equal note 'sol) 6700)
         ((equal note 'la) 6900)
         ((equal note 'si) 7100))
   alteration))
    
;;MODIF
;; utilitaire pour transpose-modulante, pour connaitre les noms de notes de la tonalite d'arrivee
;; pas terrible : on tient pas compte des dieses et des bemols 
(defun midic->notes (midic)
  (let ((midic4 (+ 6000 (mod midic 1200))))
    (cond ((= midic4 6000) '(do becarre))
          ((= midic4 6100) '(do diese))
          ((= midic4 6200) '(re becarre))
          ((= midic4 6300) '(re diese))
          ((= midic4 6400) '(mi becarre))
          ((= midic4 6500) '(fa becarre))
          ((= midic4 6600) '(fa diese))
          ((= midic4 6700) '(sol becarre))
          ((= midic4 6800) '(sol diese))
          ((= midic4 6900) '(la becarre))
          ((= midic4 7000) '(la diese))
          ((= midic4 7100) '(si becarre)))))
          


;; MODIF utilitaire pour recuperer le bon intervale en mode mineur
(defun deploiemineur (deg)
  (cond ((equal deg '(6 d)) 6)
        ((equal deg '(6 a)) 7)
        ;((equal deg '(7 a)) 8)
        ;((equal deg '(7 d)) 9)
        ((equal deg '(7 d)) 8)
        ((equal deg '(7 a)) 9)
        (t deg)))

;; MODIF correction bug : elle se plantait d'un degre
;;; NEW JEAN
;;; degre = 1 2 3 4 5 6 7 ...
;;; longuer d'un mode = 7
;;; --> nboctaves = (div (- deg 1) longueur)
(defun degre->midic (deg mode)  
   (let* ((deg (if (listp deg) (deploiemineur deg) deg))
          (longueur (length mode))
          ;(nboctaves (div deg longueur))
          (nboctaves (div (1- deg) longueur))
          (nbreste (mod (1- deg) longueur)))
     (+ (* 1200 nboctaves) (nth nbreste mode))))




(defun num-liste (a l &optional (prov 1))
  (cond ((null l) nil)
        ((= (car l) a) prov)
        ((< (car l) a) (num-liste a (cdr l) (1+ prov)))))

;;;======= DEGRES =======;;;

;;MODIF correction bug : on avait pas les bons a et d
(defun degre-mode (a l)
  (let* ((numero (num-liste a l)))
    (cond
     ((null numero) nil)
     ((equal l *mineur*)
      (cond ((= numero 6) (list 6 'd))
            ((= numero 7) (list 6 'a))
            ((= numero 8) (list 7 'd))
            ((= numero 9) (list 7 'a))
            (t numero)))
     (t numero))))

(defun degre+ (a b)
  (if (or (null a) (null b)) nil
      (let ((aa (if (listp a) (car a) a))
            (bb (if (listp b) (car b) b)))
        (+ aa bb))))

(defun degre- (a b)
  (if (or (null a) (null b)) nil
    (let ((aa (if (listp a) (car a) a))
          (bb (if (listp b) (car b) b)))
      (- aa bb))))

(defun degremod (a b)
  (let ((deg (if (listp a) (car a) a)))
    (mod deg b)))

(defmethod set-degre ((self tonal-object) degre)
  (if (tonal-values self)
    (setf (degre (tonal-values self)) degre)
    (progn (setf (tonal-values self)
                 (make-instance 'tonal-prop
                   :degre degre))))
  self)

(defmethod get-degre ((self tonal-object))
  (let ((tv (tonal-values self)))
    (if tv
      (degre tv)
      nil)))
              
              


;;;======= TONALITE =======;;; recupre la tonalite d'un objet.
;;; si nil -> la tonalite de son container

(defmethod get-tonalite ((self t)) nil)

(defmethod get-tonalite ((self tonal-object))
  (or (tonalite self)
      (if (and (parent self) (tonal-object-p (parent self))) 
        (get-tonalite (parent self))
          nil)))

;;; une note c'est different : sa tonalite est celle de son parent
(defmethod get-tonalite ((self note))
  (if (parent self) 
    (get-tonalite (parent self))
    nil))
                                      

;;;======= SET-TONALITE =======
;;; on fixe une tonalite a un objet.
;;; ca change pas la tonalite des sous objets : 
;;; si ils en ont pas, il viennent chercher celle la (get-tonalite)

;;; en general : on change la tonalite de l'objet
(defmethod set-tonalite ((self tonal-object) (tonalite tonalite))
  (let ((ton (make-instance (get-tonalite-class self) 
              :tonnote (tonnote tonalite)
              :tonalt (tonalt tonalite)
              :mode (mode tonalite))))
  (setf (tonmidi ton) (notes->midic (tonnote ton) (tonalt ton)))
  (setf (tonalite self) ton)
  (actualise-tonalite self)))

(defmethod set-tonalite ((self tonal-object) (tonalite null))
  (setf (tonalite self) nil)
  (actualise-tonalite self))

;;; renvoie diese ou bemol si note est diese ou bemol a la clef
(defun find-note-alteration-in-armure (my-note armure)
  (let* ((armure-alt (cond ((equal (diese) (car armure)) 'diese)
                           ((equal (bemol) (car armure)) 'bemol)
                           (t nil)))
         (pos-in-armure (cond ((equal armure-alt 'diese) (position my-note *diese-list*))
                              ((equal armure-alt 'bemol) (position my-note *bemol-list*))
                              (t nil)))
        (alt-in-armure (if (and pos-in-armure (< pos-in-armure (length (second armure))))
                         armure-alt
                         nil)))
    alt-in-armure))


;;; regler l'alteration d'une note en fonction de l'armure.
(defmethod arrange-note-tonalite-alteration ((self tonalite) (tonal-ref t))
  (when tonal-ref
    (let* ((my-alt (tonalt self))
          (my-note (tonnote self))
          (armure (get-armure (find-tonalite-num tonal-ref)))
          (alt-in-armure (find-note-alteration-in-armure my-note armure))
          (final-alt nil))
      
      (setf final-alt
            (if (consp my-alt) my-alt
                (if (equal my-alt alt-in-armure) nil
                    (if my-alt my-alt 'becarre)
                    ))
            )
      (setf (tonalt self) final-alt)
      
      self)))



;;;======== ACTUALISE =======
;;; ça veut pas dire qu'on change la tonalite mais juste
;;; qu'on met a jour les valeurs de tonal-values (degre, etc...)

(defun init-tonal-values (tonal-prop)
  (when tonal-prop
    (setf (degre tonal-prop) nil)
    (setf (accident tonal-prop) nil)
    (setf (ornement tonal-prop) nil)
    t
))


(defmethod actualise-tonalite ((self note))
  (if (tonal-values self)
      (init-tonal-values (tonal-values self))
    (setf (tonal-values self) (make-instance 'tonal-prop)))
  (when (get-tonalite self)
    (let* ((tv (tonal-values self))
           (tonalite (get-tonalite self))
           (mode (mode tonalite))
           (tonmidi (tonmidi tonalite))
           (hauteur (midic self))
           (deg (mod (- hauteur tonmidi) 1200)))
      (set-degre self (degre-mode deg mode))
      (setf (accident tv) (not (degre tv)))
      self)))



(defmethod set-degre-accord ((self chord))
  (unless (tonal-values self) (setf (tonal-values self) (make-instance 'tonal-prop)))
  ;(if (tonal-values self)
  ;  (init-tonal-values (tonal-values self))
  ;  (setf (tonal-values self) (make-instance 'tonal-prop)))
  (set-degre self (chiffrage-chiffre self))
  self)


(defmethod actualise-tonalite ((self chord))
  (if (get-tonalite self)
      (let* ((tonalite (get-tonalite self))
             (tonalt (tonalt tonalite))
             (tonnote (tonnote tonalite))
             (mode (mode tonalite)))
        (setf (tonmidi tonalite) (notes->midic tonnote tonalt))
        (loop for el in (inside self)
              do (actualise-tonalite el))
        (set-degre-accord self))
    (loop for el in (inside self)
          do (actualise-tonalite el)))
  self)

(defmethod set-cadences ((self chord-seq))
  (cadence? self))

(defmethod actualise-tonalite ((self chord-seq))
  (loop for el in (inside self) do (actualise-tonalite el))
  (set-cadences self))

(defmethod actualise-tonalite ((self tonal-object)) t)



