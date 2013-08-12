;;; This file contains an extension of the common-lisp hash tables allowing to know the list 
;;; of keys of each hash table. 
;;; Frank D. Valencia,16/10/96
;;; This is the version 1.1b of the musical nconstraint satisfaction solver 
;;; situation ( © IRCAM ) by Bonnet & Rueda.

(in-package :cl-user)
 
#|
(defstruct  hash-table+
  "Keys is the list of keys of the table. Table is a the hash-table"
  (Keys  nil :type list)
  (Table (make-hash-table) :type hash)
  )
|#


;changed by AAA hash does not work with MAcos X
(defstruct  hash-table+
  "Keys is the list of keys of the table. Table is a the hash-table"
  (Keys  nil :type list)
  (Table (make-hash-table) :type hash-table)
  )

(defun sethash+ ( key elem Htable+  )
  "Sets an element 'elem' with key 'key' in Htable+"
  (setf (gethash key (hash-Table+-Table Htable+)) elem)
  (setf (hash-Table+-keys Htable+) (Ins-Set key (hash-Table+-keys Htable+)))
  Htable+
  )


(defun gethash+ ( key Htable+ ) 
  "Returns the value of 'key' in Htable+"
  (gethash key (hash-Table+-Table Htable+)))

(defun addhash+ ( key elem HTable+ )
"Adds an element 'elem' with key 'key' in Htable+"
  (setf (gethash key (hash-Table+-Table Htable+)) 
        (cons elem (gethash+ key Htable+) ) 
        )
  (setf (hash-Table+-keys Htable+) (Ins-Set key (hash-Table+-keys Htable+)))
  HTable+ 
  )

(defun remhash+ ( key  Htable+ )
  "Removes the entry for 'key' from the hash-table 'Htable+'."
  (remhash key (hash-Table+-Table Htable+))
  (setf  (hash-Table+-keys Htable+) (remove key (hash-Table+-keys Htable+) :test 'equal))
  HTable+ 
  )

(defun permut-list-cond (list)
  (if *domain-permutation* (permut-list list) list))

(defun hash-table+-tolist ( HTable+ &AUX L)
  "Translates a hash-table into a list"
  (dolist (key (permut-list-cond (hash-table+-keys HTable+) ) L) (push (list key (gethash+ key Htable+)) L))
)

(defun list-to-hash-table+ ( List fkey &AUX (HTable+ (make-hash-table+ )))
  "Translate a list 'List' into a hash-table+. fkey is function wich returns key 
given an element of 'List'"
  (dolist (x List Htable+) (setf Htable+ (addhash+ (funcall fkey x) x Htable+)))
)

(defun Hash-table+-Size ( HT )
 "Returns the number of elements in the hash-table+ HT"
       (hash-table-count  (hash-table+-table HT)))

(defun Hash-table+-Empty? ( HT )
  "Checks if HT is an empty hash-table+"
  (zerop (Hash-table+-size HT)))

(defun union-hash-table+ ( HT1 HT2 )
"Adjoins two hash-table+ 'HT1' and 'HT2'"
  (cond ((hash-table+-empty? HT1) HT2)
        ((hash-table+-empty? HT2) HT1)
        ( t
          (progn 
            (when (< (hash-table+-size HT1) (hash-table+-size HT2)) 
              (let ((temp HT1)) (setf HT1 HT2 HT2 temp) )
              )
            (dolist (key1 (hash-table+-keys HT2) HT1)
              (setf HT1 (sethash+ key1 (union+  (gethash+ key1 HT1) (gethash+ key1 HT2) :test 'equal) HT1))
              )
            )
          )
        )
  )


