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

;;; Harmonic project by J. Bresson, C. Truchet


(in-package :om)


;;;=======================================
;;; liste des tonalites maj/min relative 
;;;=======================================

(defvar *ton-list* nil)
(setf *ton-list* (list 
                   (list (list 'do 'diese) (list 'la 'diese))
                   (list (list 'fa 'diese) (list 're 'diese))
                   (list (list 'si nil) (list 'sol 'diese))
                   (list (list 'mi nil) (list 'do 'diese))
                   (list (list 'la nil) (list 'fa 'diese))
                   (list (list 're nil) (list 'si nil))
                   (list (list 'sol nil) (list 'mi nil))
                   (list (list 'do nil) (list 'la nil))
                   (list (list 'fa nil) (list 're nil))
                   (list (list 'si 'bemol) (list 'sol nil))
                   (list (list 'mi 'bemol) (list 'do nil))
                   (list (list 'la 'bemol) (list 'fa nil))
                   (list (list 're 'bemol) (list 'si 'bemol))
                   (list (list 'sol 'bemol) (list 'mi 'bemol))
                   (list (list 'do 'bemol) (list 'la 'bemol))))

;;; conversions entre les formats int (position dans *ton-list*) et ceux de la classe tonalite
(defun convertnummode (num)
  (cond 
   ((equal num 0) *majeur*)
   ((equal num 1) *mineur*)
   (t *majeur*)))

(defun convertmodenum (mode)
  (if (equal *majeur* mode) 0 1))

;;; conversion de l'alteration "becarre" (nil dans *ton-list*)
(defun convertnilalt (alt)
  (if (null alt) 'becarre alt))

(defun convertaltnil (alt)
  (if (equal 'becarre alt) nil alt))


;;; retrouve le numero de la tonalite dans la *ton-list*
;;; nil --> il va chercher l'approx scale par defaut pour les microtonalites
(defmethod find-tonalite-num ((self t)) nil)

(defmethod find-tonalite-num ((self tonalite))
  (let ((ton (tonnote self))
        (mode (convertmodenum (mode self)))
        (alt (convertaltnil (tonalt self))))
    (loop for item in *ton-list*
        for i = 0 then (+ i 1) do
          (when (and (equal ton (car (nth mode item))) (equal alt (cadr (nth mode item))))
            (return i)))))



;;;=======================================
;; armure des 15 tonalites de *ton-list* 
;; (places des alterations sur la portee en partant du fa d'en bas - 1ere ligne)
;;;=======================================

(defun get-armure (n)
  (case n
    (0 (list (diese) '(8 5 9 6 3 7 4))) 
    (1 (list (diese) '(8 5 9 6 3 7)))
    (2 (list (diese) '(8 5 9 6 3)))
    (3 (list (diese) '(8 5 9 6))) 
    (4 (list (diese) '(8 5 9)))
    (5 (list (diese) '(8 5))) 
    (6 (list (diese) '(8)))
    (7 (list nil nil))
    (8 (list (bemol) '(4))) 
    (9 (list (bemol) '(4 7))) 
    (10 (list (bemol) '(4 7 3))) 
    (11 (list (bemol) '(4 7 3 6))) 
    (12 (list (bemol) '(4 7 3 6 2))) 
    (13 (list (bemol) '(4 7 3 6 2 5))) 
    (14 (list (bemol) '(4 7 3 6 2 5 1)))
    ))


(defmethod get-armure-from-tonalite ((self t)) 
  (get-armure (find-tonalite-num self)))

(defmethod get-armure-from-tonalite ((self tonal-object)) 
  (get-armure (find-tonalite-num (tonalite self))))


;;;=======================================
;; Echelles d'alterations
;;;=======================================

(defvar *diese-list* nil)
(setf *diese-list* (list 'fa 'do 'sol 're 'la 'mi 'si))
(defvar *bemol-list* nil)
(setf *bemol-list* (list 'si 'mi 'la 're 'sol 'do 'fa))

(setf *7#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil nil (beca) nil (beca) nil nil (beca) nil (beca) nil (beca))
    :lines-list
    (list  6 0 1 1 2 2 3 4 4 5 5 6)
    :approx-factor 100))

(setf *6#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  (beca) nil (beca) nil (beca) nil nil (beca) nil (beca) nil nil)
    :lines-list
    (list  0 0 1 1 2 2 3 4 4 5 5 6)
    :approx-factor 100))

(setf *5#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  (beca) nil (beca) nil nil (beca) nil (beca) nil (beca) nil nil)
    :lines-list
    (list  0 0 1 1 2 3 3 4 4 5 5 6)
    :approx-factor 100))

(setf *4#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  (beca) nil (beca) nil nil (beca) nil (beca) nil nil (diese) nil)
    :lines-list
    (list  0 0 1 1 2 3 3 4 4 5 5 6)
    :approx-factor 100))

(setf *3#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  (beca) nil nil (diese) nil (beca) nil (beca) nil nil (diese) nil)
    :lines-list
    (list  0 0 1 1 2 3 3 4 4 5 5 6)
    :approx-factor 100))

(setf *2#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  (beca) nil nil (diese) nil (beca) nil nil (diese) nil (diese) nil)
    :lines-list
    (list  0 0 1 1 2 3 3 4 4 5 5 6)
    :approx-factor 100))

(setf *1#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil (diese) nil (diese) nil (beca) nil nil (diese) nil (diese) nil)
    :lines-list
    (list  0 0 1 1 2 3 3 4 4 5 5 6)
    :approx-factor 100))

(setf *0#-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil (diese) nil (diese) nil nil (diese) nil (diese) nil (diese) nil)
    :lines-list
    (list  0 0 1 1 2 3 3 4 4 5 5 6)
    :approx-factor 100))

(setf *7b-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  (beca) nil (beca) nil nil (beca) nil (beca) nil (beca) nil nil)
    :lines-list
    (list  0 1 1 2 3 3 4 4 5 5 6 7)
    :approx-factor 100))

(setf *6b-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  (beca) nil (beca) nil (beca) nil nil (beca) nil (beca) nil nil)
    :lines-list
    (list  0 1 1 2 2 3 4 4 5 5 6 7)
    :approx-factor 100))

(setf *5b-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil nil (beca) nil (beca) nil nil (beca) nil (beca) nil (beca))
    :lines-list
    (list  0 1 1 2 2 3 4 4 5 5 6 6)
    :approx-factor 100))

(setf *4b-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil nil (beca) nil (beca) nil (diese) nil nil (beca) nil (beca))
    :lines-list
    (list  0 1 1 2 2 3 3 4 5 5 6 6)
    :approx-factor 100))

(setf *3b-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil (diese) nil nil (beca) nil (diese) nil nil (beca) nil (beca))
    :lines-list
    (list  0 0 1 2 2 3 3 4 5 5 6 6)
    :approx-factor 100))

(setf *2b-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil (diese) nil nil (beca) nil (diese) nil (diese) nil nil (beca))
    :lines-list
    (list  0 0 1 2 2 3 3 4 4 5 6 6)
    :approx-factor 100))

(setf *1b-scale* 
  (make-instance 'scale 
    :alteration-list
    (list  nil (diese) nil (diese) nil nil (diese) nil (diese) nil nil (beca))
    :lines-list
    (list  0 0 1 1 2 3 3 4 4 5 6 6)
    :approx-factor 100))



(defmethod get-scale-from-tonality ((self tonal-object))
  (get-scale (find-tonalite-num (tonalite self))))

(defmethod get-scale-from-tonality ((self t)) nil)


(defmethod get-scale (n)
  (case n
   (0 *7#-scale*)
   (1 *6#-scale*)
   (2 *5#-scale*)
   (3 *4#-scale*)
   (4 *3#-scale*)
   (5 *2#-scale*)
   (6 *1#-scale*)
   (7 *0#-scale*)
   (8 *1b-scale*)
   (9 *2b-scale*)
   (10 *3b-scale*)
   (11 *4b-scale*)
   (12 *5b-scale*)
   (13 *6b-scale*)
   (14 *7b-scale*)
   (t nil)))




;;;=======================================
;; Enharmonies
;;;=======================================

(defvar *alt-table* nil)
(setf *alt-table* (list 
                   (list (list 'si 1) (list 'do 0) (list 're -2))
                   (list (list 'si 2) (list 'do 1) (list 're -1))
                   (list (list 'do 2) (list 're 0) (list 'mi -2))
                   (list (list 're 1) (list 'mi -1) (list 'fa -2))
                   (list (list 're 2) (list 'mi 0) (list 'fa -1))
                   (list (list 'mi 1) (list 'fa 0) (list 'sol -2))
                   (list (list 'mi 2) (list 'fa 1) (list 'sol -1))
                   (list (list 'fa 2) (list 'sol 0) (list 'la -2))
                   (list (list 'sol 1) (list 'la -1) (list nil 0))
                   (list (list 'sol 2) (list 'la 0) (list 'si -2))
                   (list (list 'la 1) (list 'si -1) (list 'do -2))
                   (list (list 'la 2) (list 'si 0) (list 'do -1))))

(defun get-note-name (symb)
  (case symb
    ('do (om-str :c))
    ('re (om-str :d))
    ('mi (om-str :e))
    ('fa (om-str :f))
    ('sol (om-str :g))
    ('la (om-str :a))
    ('si (om-str :b))
    (otherwise "?")))

;(setf *alt-table* (list 
;                   (list (list 'b 1) (list 'c 0) (list 'd -2))
;                   (list (list 'b 2) (list 'c 1) (list 'd -1))
;                   (list (list 'c 2) (list 'd 0) (list 'e -2))
;                   (list (list 'd 1) (list 'e -1) (list 'f -2))
;                   (list (list 'd 2) (list 'e 0) (list 'f -1))
;                   (list (list 'e 1) (list 'f 0) (list 'g -2))
;                   (list (list 'e 2) (list 'f 1) (list 'g -1))
;                   (list (list 'f 2) (list 'g 0) (list 'a -2))
;                   (list (list 'g 1) (list 'a -1) (list nil 0))
;                   (list (list 'g 2) (list 'a 0) (list 'b -2))
;                   (list (list 'a 1) (list 'b -1) (list 'c -2))
;                   (list (list 'a 2) (list 'b 0) (list 'c -1)))) 

(defun find-alt-list (midic)
  (let ((n (mod (round midic 100) 12)))
    (nth n *alt-table*)))