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

(defmethod* last-elem ((list list))
  :initvals '(nil) 
  :indoc '("a list")
  :doc 
"Returns the last element of <list>

Ex. (last-elem '(1 2 3 4 5))  => 5
"
  :icon 235
  (first (last (list! list))))

(defmethod* last-n ((list list) (n integer))
  :initvals '(nil 0) 
  :indoc '("a list" "number of elements")
  :icon 235
  :doc  
"Returns the <n> last elements of <list>

Ex. (last-n '(1 2 3 4 5) 3)  => (3 4 5)
"
  (last list n))


(defmethod* first-n ((list list) (n integer))
  :initvals '(nil 0) 
  :indoc '("a list" "number of elements")
  :icon 235
  :doc  
"Returns the <n> first elements of <list>

Ex. (first-n '(1 2 3 4 5) 3)  => (1 2 3)
"
  (cond
   ((< (length list)  n) list )
   (t  (butlast list (- (length list) n)))))


(defmethod* x-append ((l1? list) (l2? list) &rest lst?)
  :initvals '(nil nil nil) 
  :indoc '("first element" "second element" "additional elements")
  :icon 235
  :doc "Appends lists or atoms together to form a new list.

Ex. (x-append '(1 2 3) '(4 5)) => (1 2 3 4 5)
Ex. (x-append 1 '(4 5)) => (1 4 5)
Ex. (x-append '(1 2 3) 4) => (1 2 3 4)

This function also works with additional elements.

Ex. (x-append '(1 2 3) 4 '(5 6 7)) => (1 2 3 4 5 6 7)

"
  (apply 'append l1? l2? (mapcar #'list! lst?)))

(defmethod* x-append ((l1? t) (l2? list) &rest lst?)
  (apply 'append (list l1?) l2? (mapcar #'list! lst?)))

(defmethod* x-append ((l1? list) (l2? t) &rest lst?)
  (apply 'append l1? (list l2?) (mapcar #'list! lst?)))

(defmethod* x-append ((l1? t) (l2? t) &rest lst?)
  (apply 'append (list l1?) (list l2?) (mapcar #'list! lst?)))

;-----------FLAT


(defun rev-flat (lst)  
  (let ((l ()))
    (while lst
           (if (not (consp (car lst)))
             (newl l (nextl lst))
             (setq l (nconc (rev-flat (nextl lst)) l))))
    l ))


(defun lo-flat (list) 
  (cond ((atom list) list)
        ((atom (car list)) (cons (car list) (lo-flat (cdr list))))
        ((atom (caar list)) (apply 'append list))
        (t (cons (lo-flat (car list)) (lo-flat (cdr list))))))

(defmethod flat-low ((list list)) 
  (lo-flat list))

(defmethod flat-once ((list list))
  (if (consp (car list))
    (apply 'append list)  list))

(defmethod flat-one ((list list))
  (loop for item in list
        append (list! item)))

(defmethod n-flat-one ((list list) (level integer))
  (let ((rep list))
    (loop for i from 1 to level do
          (setf rep (flat-one rep)))
    rep))


(defmethod* flat ((lst list) &optional (level nil)) 
  :initvals '(nil nil) 
  :indoc '("a list" "level of parenthesis")
  :icon 235
  :doc "Transforms a tree-list (i.e. a list of lists) into a flat list.
If <level> is 1 (resp n) remove 1 (resp. n) level(s) of list imbrication.
If <level> is NIL (default) remove all levels of imbrication, down to a purely flat list.

Ex. (flat '((a b) c ((d e) f)))  =>  (a b c d e f)
Ex. (flat '((a b) c ((d e) f)) 1)  =>  (a b c (d e) f)    [1 level of parenthesis]
"
  (cond
   ((null level) (nreverse (rev-flat lst)))
   ((= level 0) lst)
   ((and (integerp level) (plusp level))
    (n-flat-one lst level))
   (t lst)))

;-----------CREATE-LIST

(defmethod* create-list ((count integer) (elem t))
  :initvals '(10 nil) 
  :indoc '("number of elements" "initial element")
  :icon 235
  :doc "Returns a list of length <count> filled with repetitions of element <elem>

Ex. (create-list 4 'a)  =>  (a a a a)"
  (make-list count :initial-element elem))

(defmethod* mat-trans ((matrix list))
  :initvals '(nil)
  :indoc '("a list of lists")
  :doc "Matrix transposition. 
The matrix is represented by a list of rows. Each row is a list of items.
Rows and columns are interchanged.

Ex. (mat-tran '((1 2 3) (a b c) (4 5 6))  =>  ((1 a 4) (2 b 5) (3 c 6))"
  :icon 235
  (let ((maxl (1- (loop for elt in matrix maximize (length elt))))
        result)
    (loop for i from 0 to maxl do
          (push ;;; (remove nil    ; (why this remove NIL here ??)
                       (mapcar #'(lambda (list) (nth i list)) matrix)
                ;;;  )
          result))
    (nreverse result)))

;----------------EXPAND LIST

(defvar *valid-expand-chars* '(#\* #\_))

(defun is-in (list chars)
  (let (res)
    (dolist (ch chars res)
      (if (setq res (member ch list :test #'char=)) (return res)))))


(defmethod* expand-lst ((list list))
  :icon 235 
  :initvals '((3*(2 4) 0_8))
  :indoc '("a list to expand")
  :doc  "Expands a list following repetition patterns.

1. <number>* (x1 ...x2)
repeats the pattern x1...x2 <number> times.

2. <n>_<m>s<k>
appends an arithmetic series counting from <n> to <m> by step <k>.
s<k> can be omitted (k=1). 

Ex. (expand-lst (3* (2 4) 0_8))  =>  (2 4 2 4 2 4 0 1 2 3 4 5 6 7 8)
Ex. (2* (a z 2* (4 12) (1_5 )) 0_16s2)  =>  (a z 4 12 4 12 (1 2 3 4 5) a z 4 12 4 12 (1 2 3 4 5) 0 2 4 6 8 10 12 14 16)"

  (and list
       (let ((lists (list! list)) result)
         (while lists
                (let ((next-elem (pop lists)))
                  (cond 
                   ((symbolp next-elem)
                    (let* ((form (coerce (format () "~A" next-elem) 'list))
                           (from-char (is-in form *valid-expand-chars*))
                           (char-symb (car from-char))
                           (third (cdr from-char))
                           (int (butlast form (length from-char)))
                           up-to)
                      (cond 
                       ((and (not third) char-symb (char= #\* char-symb) int
                             (numberp (setq int (read-from-string (coerce int 'string)))))
                        (push (apply #'append
                                     (make-list int
                                                :initial-element 
                                                (expand-lst (pop lists))))
                              result))
                       ((and char-symb (char= #\_ char-symb) third
                             (numberp (setq int (read-from-string (coerce int 'string)))))
                        (if (setq from-char (member #\s  ;;;[CR, 14/01/99] #\S
                                                    third :test #'char=))
                          (progn (setq up-to (butlast third (length from-char))
                                       char-symb (car from-char) third (cdr from-char))
                                 (if (and char-symb 
                                          (char= #\s ;;;[CR, 14/01/99] #\S
                                                 char-symb)
                                          (or (null third)
                                              (numberp 
                                               (setq third (read-from-string (coerce third 'string)))))
                                          (numberp 
                                           (setq up-to (read-from-string (coerce up-to 'string)))))
                                   (push (arithm-ser int up-to (or third 1)) result)
                                   (push (list next-elem) result)))
                          (progn
                            (setq up-to (read-from-string (coerce third 'string)))
                            (push (arithm-ser int up-to 1) result))
                          ))
                       (t (push (list next-elem) result)))))
                   ((consp next-elem)
                    (push (list (expand-lst next-elem)) result))
                   (t (push (list next-elem) result)))))
         ;(apply #'append (nreverse result))  ;;; does not support long lists
         (loop for item in (nreverse result) append item))))




;;;-----------------GROUP-LIST


(defmethod* group-list ((list list) (segmentation list) mode)
   :icon 235 
   :initvals (list '(1 2 3 4) '(1 3) 'linear)
   :indoc '("list to group" "list of group lengths" "normal or circular")
   :doc  "Segments a <list> in successives sublists which lengths are successive values of the list <segmentation>.
 <mode> indicates if <list> is to be read in a circular way.

Ex. (group-list '(1 2 3 4) '(1 3) 'linear)  => ((1) (2 3 4))
Ex. (group-list '(1 2 3 4) '(1 2 3) 'linear)  => ((1) (2 3) (4))
Ex. (group-list '(1 2 3 4) '(1 2 3) 'circular)  => ((1) (2 3) (4 1 2))
"
   :menuins '( (2 ( ("linear" 'linear) ("circular" 'circular))))
   (let ((list2 list) (res nil))
     (catch 'gl
      (loop for segment in segmentation
            while (or list2 (eq mode 'circular))
            do (push (loop for i from 1 to segment
                           when (null list2)
                           do (case mode
                                (linear (push sublist res) (throw 'gl 0))
                                (circular (setf list2 list)))
                           end
                           collect (pop list2) into sublist
                           finally (return sublist))
                              res))
     )
     (nreverse res)
     ))

(defmethod* group-list ((list list) (segmentation number) mode)
    (group-list list (repeat-n segmentation (ceiling (length list) segmentation)) 'linear))


;;;;-----------------Remove-dup
(defmethod* remove-dup ((list list) (test symbol) (depth integer))
  :icon 235 
  :initvals (list '(1 2 3 4) 'eq 1)
  :indoc '("list" "equality test (function or function name)" "an integer")
  :doc  "Removes duplicates elements from <list>.
If <depth> is more than 1 duplicates are removed from sublists of level <depth>.

Ex. (remove-dup '(1 2 3 2 2 4) '= 1) => (1 3 2 4)
Ex. (remove-dup '((1 2) (3 2 2) 4) '= 2) => ((1 2) (3 2) 4)
"
  (remove-dup list (symbol-function test) depth))


(defmethod* remove-dup ((list list) (test function) (depth integer))
  (if (<= depth 1)
    (remove-duplicates list :test test)
    (mapcar #'(lambda (x) (remove-dup x test (1- depth))) list)))


(defmethod* remove-dup ((list t) (test t) (depth integer)) list)

;;;-----------------LIST-MODULO

(defmethod* list-modulo ((list list) (ncol integer))
  :initvals '(nil 2) 
  :indoc '("a list" "modulo")
  :icon 235
  :doc 
  "Groups the elements of a list distant of a regular interval <ncol> and returns these groups as a list of lists. 

Ex. (list-modulo '(1 2 3 4 5 6 7 8 9) 2)  => ((1 3 5 7 9) (2 4 6 8))
Ex. (list-modulo '(1 2 3 4 5 6 7 8 9) 3)  => ((1 4 7) (2 5 8) (3 6 9))
"
  (when (and (> ncol 0) (< ncol (length list))) (list-part list ncol)))

(defun list-part (list ncol)  
  (let ((vector (make-array  ncol )) res)
    (while list 
      (for (i 0 1 (1- ncol))
        (and list (vset vector i (push (pop list) (vref vector i))))))
    (for (i 0 1 (1- ncol))
      (push (remove nil (nreverse (vref vector i))) res))
    (nreverse res)))

;;;-----------------INTERLOCK
(defmethod* interlock ((lis1 list) (lis2 list) (plc1 list))
   :initvals '((0 1 2 3) (a b) (1 3))
   :indoc '("a list" "a list" "a list of indexes")
   :icon 235
   :doc "Inserts the successive elements of <lis2> in <lis1> before the elements of <lis1> of respective positions from <plc1>.

Ex. (interlock '(0 1 2 3 ) '(a b) '(1 3))  =>  (0 a 1 2 b 3)"
   (let ((aux) (pointeur 0))
     (dotimes (n (length lis1) (reverse aux))
       (when  (member n plc1)
         (progn ()
                (push (nth pointeur lis2) aux)
                (incf pointeur)))
       (push (nth n lis1) aux))))

;;;-----------------SUBS-POSN

(defmethod* subs-posn ((lis1 list) posn val)
  :initvals '((0 1 2 3)  (1 3) (a b))
  :indoc '("a list" "a list of indices" "a list or value")
  :icon 235
  :doc "Substitutes the elements of <lis1> at position(s) <posn> (if they exist) with the corresponding elements in <val>.

Ex. (subs-posn '(0 1 2 3) 2 'a)  => (0 1 a 3)
Ex. (subs-posn '(0 1 2 3) '(1 3) '(a b))  => (0 a 2 b)
"
  (let ((copy (copy-list lis1)))
    (if (listp posn)
        (loop for item in posn
              for i = 0 then (+ i 1) do
              (setf (nth item copy) (if (listp val) (nth i val) val)))
      (setf (nth posn copy) val))
    copy))


;;; old
;;(setf posn (sort posn '<))
;;   (loop for elt in lis1
;;         for counter from 0
;;         if (and posn (= counter (first posn))) collect (pop val) and do 
;;         (pop posn) else collect elt)





