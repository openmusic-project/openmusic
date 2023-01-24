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

;;; MATHTOOLS by C. Agon, M. Andreatta et al.

(in-package :om)

(defun equalrot (A B)
  (let (resultat)
    (setf resultat (loop for i from 1 to (length A) do 
                         (when (equal (om::rotate A i) B)
                           (return t))))
    resultat))

(defmethod! n-scale ((l list))
  :initvals '((2 2 2 6) ) :indoc '("values" )
  :doc "It builds subset of the cyclic group Z/nZ starting from its interval structure. Notice that the period n is the sum of the elements of the interval structure" 
  :icon 420 
  (butlast (dx->x 0 l)))
  
  
(defmethod! n-structure ((l list) (n integer))
  :initvals '((5 7 9) 12) :indoc '("values" "n")
  :doc "It builds the interval structure of a given subset of the cyclic group Z/nZ." 
  :icon 420 
  (let ((rep (x->dx l)))
    (append rep (list (- n (apply '+ rep))))))


;============================


(defun factorial (n)
  (if (zerop n) 1 (* n (factorial (- n 1))) ))

(defun binomial (m n)
  (/ (factorial m) (* (factorial n) (factorial (- m n)))))

(defun divisors (m)
  (loop for i from 1 to m
        when (zerop (mod m i)) collect i))

(defun euler (m)
  (if (= m 1) 1
      (length (loop for i from 1 to (- m 1)
            when (relativelyprimes  m i) collect i))))

(defmethod! card ((n integer) (k integer))
  :initvals '(12 6) :indoc '("Zn" "elments")
  :doc "Nombre de structures intervallaires ayant k elements a une transposition pres
Par exemple il y a 66 gammes de sept notes a l'intÂrieur du total chromatique a une transposition pres."
   :icon 420 
  (let ((pgcd (pgcd n k)))
    (* (/ 1 n) (loop for item in (divisors pgcd)
                     sum (* (binomial (/ n item) (/ k item)) (euler item))))))

;======================================

(defmethod! orbites ((n integer) (k integer) howmany)
  :initvals '(12 6 4) :indoc '("Zn" "elments" "how many answers")
  :doc "Ensembles de toutes structures intervallaires ayant k elements a une transposition pres." 
  :icon 420 
  (let ((rep (apply-n-time  s::group-orbites (n k) (min howmany (card n k)))))
    (loop for item in rep collect (n-scale item))))


;======================================


(defmethod! famille ((n integer) (familly string) (k integer) (howmany integer))
  :initvals '(12 "tac" 6 5) :indoc '("Zn" "familly" "number of elements" "howmany answers")
  :menuins '( ( 1 ( ("tac" "tac") ("tai" "tai") ("tic" "tic") ("ttl" "ttl")
                   ("tp" "tp") ("tid" "tid"))))
  :doc "Ensembles de  structures intervallaires ayant k éléments à une transposition près appartenant a une famille donnée.


Tac
Structures intervallaires auto- complémentaires. 
Les éléments du total chromatique qui n'appartiennent pas à la gamme (ou accord) déterminée par une structure auto- complémentaire, forment la même structure intervallaire.
Par exemple, (2 2 2 2 2 2) avec n = 12.

Tai
Structures intervallaires auto-inverses. 
N'importe quelle inversion conduit à une gamme (ou accord) ayant la même structure intervallaire.
Par exemple le bicord (8 4) avec n =12.

Tic
Structure intervallaires inverses- complémentaires.
 Le passage au complémentaire de n'importe quelle inversion d'une telle structure ne change pas la structure intervallaire. 
Par exemple, (2 2 3 2 2 1), (7 1 1 1 1 1) et (2 5 2 1 1 1) pour n =12.

Ttl
Structures intervallaires à transposition limitée. 
Une structure  ayant les mêmes notes de certaines de ses transpositions.
Par exemple la structure (2 3 1 2 3 1) et son inverse (3 2 1 3 2 1)  pour n=12.

Tp
Structures intervallaires partitionnantes. 
Une telle structure génère le total chromatique dans le sens que ce dernier est une union disjointe de transpositions dÕaccords ayant la même classe intervallaire.
Par exemple pour (3 3 3 3) Les transpositions seront  1 ,2 et 3.

Tid
Structures intervallaires idempotentes. 
Une telle structure est telle que composée avec elle même  elle reste invariée.  
Par exemple, (2 2 2 2 2 2) dans n=12 avec k = 6
" 
  :icon 420 
  (let ((rep (sfamille n (interne familly) k howmany)))
    (loop for item in rep collect (n-scale item)))) 


(defmethod sfamille ((n integer) (familly t) k howmany)
  (print "no conozco"))

(defmethod sfamille ((n integer) (familly (eql 'tac)) k howmany) 
  (apply-n-time  s::group-tac (n k) howmany))

(defmethod sfamille ((n integer) (familly (eql 'tai)) k howmany)
  (apply-n-time  s::group-tai (n k) howmany) )

(defmethod sfamille ((n integer) (familly (eql 'tic)) k howmany) 
  (apply-n-time  s::group-tic (n k) howmany))

(defmethod sfamille ((n integer) (familly (eql 'ttl)) k howmany) 
  (apply-n-time  s::group-ttl (n k) howmany))

(defmethod sfamille ((n integer) (familly (eql 'tp)) k howmany) 
  (apply-n-time  s::group-tp (n k) howmany))

(defmethod sfamille ((n integer) (familly (eql 'tid)) k howmany) 
  (s::group-tid n k))