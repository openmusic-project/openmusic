;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)

;-------SORT---------
;;; compat
(defmethod* sort. ((lst list)  &optional (test '<) (key nil))
  :initvals '(nil < nil)
  :indoc '("the list" "test" "key")
  :doc "Sorts a list. 

(Deprecated - Use sort-list instead)

<test> is a binary function or function name indicating how to compare elements.
<key> is a function or function name that will be applied to elements before the test.
"
  :icon 176
  (sort-list lst :test test :key key))

 
(defmethod! sort-list ((lst list) &key (test '<) (key nil) (rec nil))
  :initvals '(nil '< nil nil)
  :menuins '((3 (("Yes" t) ("No" nil))))
  :indoc '("a list" "test function" "test key" "recursive sort")
  :doc "Sorts a list. 
<test> is a binary function or function name indicating how to compare elements.
<key> is a function or function name that will be applied to elements before the test.

If <rec> is T, then the sort will be applied recursively to the sub-lists in <lst>.

Ex. (sort-list '(3 6 7 2 1)) => (1 2 3 6 7)
Ex. (sort-list '(3 6 7 2 1) :test '>) => (7 6 3 2 1)
Ex. (sort-list '((a 3) (b 6) (c 7) (d 2) (e 1)) :test '> :key 'second) => ((c 7) (b 6) (a 3) (d 2) (e 1))
Ex. (sort-list '((7 8 9 1) (5 2 4 3)) :test '< :key 'first) => ((5 2 4 3) (7 8 9 1))
Ex. (sort-list '((7 8 9 1) (5 2 4 3)) :test '< :rec t) => ((1 7 8 9) (2 3 4 5))
"
  :icon 176
  (let ((l (copy-list lst)))
    (if rec
        (cond ((null lst) nil)
              ((atom (first lst)) (sort lst (or test #'<) :key key))
              (t (cons (sort-list (first l) :test (or test #'<) :key key :rec rec) 
                       (sort-list (rest l) :test (or test #'<) :key key :rec rec))))
      (sort l (or test #'<) :key key))
 
  ))

;-------CIRCULAR PERMUTATION---------

(defmethod permut-circn ((list list) &optional (nth 1))
  (when list
    (let ((length (length list)) n-1thcdr)
      (setq nth (mod nth length))
      (if (zerop nth) list
          (prog1
            (cdr (nconc (setq n-1thcdr (nthcdr (1- nth) list)) list))
            (rplacd n-1thcdr ()))))))

(defmethod* rotate ((list list) &optional (nth 1))
  :initvals '(nil 1)
  :indoc '("the list" "nth")
  :doc "Returns a circular permutation of <list> starting from its <nth> element.

Ex. (rotate '(a b c d e) 1) => (b c d e a)
Ex. (rotate '(a b c d e) 3) => (d e a b c)"
  :icon 176
  (permut-circn (copy-list list) nth))

;-------NTH-RANDOM---------

(defmethod* nth-random ((list list))
  :initvals '(nil)
  :indoc '("a list")
  :doc "Returns a randomly chosen element from <list>."
  :icon 176
  (nth (om-random-value (length list)) list))

;-------PERMUT-RANDOM---------

(defmethod npermut-random ((list list))
  "destructive random permutation of <list>."
   (let ((result ()) 
           (length (length list)) 
           (list (copy-list list)))
    (nconc list list)
    (loop for i from 0 to length do
              (setq list (nthcdr (om-random-value length) list)
            result (rplacd (prog1 (cdr list) (rplacd list (cddr list))) result)
            length (1- length)))
    result))


(defmethod* permut-random ((list list))
  :initvals '(nil)
  :indoc '("a list")
  :doc "Returns a random permutation of <list>

Ex. (permut-random '(1 2 3 4 5)) => (4 5 2 3 1)"
  :icon 176
  (if (or (null list) (= (length list) 1)) list
      (npermut-random (copy-list list))))



;-------MAT-TRANS---------

(defmethod* posn-order ((list list) (test symbol))
  :initvals '(nil '<)
  :indoc '("a list" "test function")
  :doc  "Returns a list of indices according to a sort function.
The indexes of items in <list> (from 0 to length-1) will be sorted according to <test>.
<test> may be a function or function name (symbol) for a binary comparison function.

Ex. (posn-order '(4 5 6 7 8) '<) => (0 1 2 3 4)
Ex. (posn-order '(4 5 6 7 8) '>) => (4 3 2 1 0)
"
  :icon 176
  (do-posn-order list test))

(defmethod* posn-order ((list list)  (test function))
  (do-posn-order list test))


(defun do-posn-order (list test)
  (let* ((thelist (sort-list list :test test))  rep)
    (mapc #'(lambda (x)
              (let* ((repons (position x thelist)) (i 0))
                (loop while (position repons rep) do
                      (incf i)
                      (setf repons (position  x thelist :start (+ (position x thelist) i))))
                (setf rep (cons repons rep)))) list)
    (reverse rep)))

;-------PERMUTATIONS---------

(defmethod* permutations ((bag list))
  :initvals '(nil ) 
  :indoc '("a list")
  :icon 176
  :doc "Return a list of all the permutations of <bag>.

Ex. (permutations '(a b c)) => ((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))"
  
  (if (null bag)
    '(())
    ;; Otherwise, take an element, e, out of the bag.
    ;; Generate all permutations of the remaining elements,
    ;; And add e to the front of each of these.
    ;; Do this for all possible e to generate all permutations.
    (mapcan #'(lambda (e)
                (mapcar #'(lambda (p) (cons e p))
                        (permutations (remove e bag :count 1 :test #'eq))))
            bag)))

 
