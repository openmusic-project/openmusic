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

;DocFile
; Box online documentation
;DocFile

(in-package :om)

;;; get doc as text
(defmethod om-get-documentation ((self t)) nil)

;(function-documentation 'cons)
;(documentation 'cons 'function)


(defmethod om-get-documentation ((symbol symbol)) 
  (let ((*print-case* :downcase)
        (doc-list nil))
    (cond 
     ((and (fboundp symbol) (not (typep (fdefinition symbol) 'generic-function)))
      (setf doc-list (fun-to-doclist symbol)))
     ((find-class symbol nil)
      (setf doc-list (class-to-doclist symbol)))
     ((fboundp symbol)
      (setf doc-list (fun-to-doclist symbol)))
     (t nil))
    doc-list
    ))

(defun fun-to-doclist (symbol)
  (let ((doc-list nil)
        (fun (fdefinition symbol))
        (args ())) 
    (setf doc-list (list symbol))
    (if (typep fun 'generic-function)
        (setf doc-list (append doc-list (list 'GENERIC-FUNCTION)))
      (setf doc-list (append doc-list (list 'FUNCTION))))
    (dolist (arg (arglist symbol))
      ;;; ce serait bien de mettre les keywords en italic
      (if (member arg lambda-list-keywords :test 'eq) 
          (setf args (append args (list arg)))
        (setf args (append args (list arg)))))
    (setf doc-list (append doc-list (list args)))
    (setf doc-list (append doc-list (list (function-documentation symbol))))
    doc-list))

(defun class-to-doclist (symbol)
  (let ((doc-list nil)
        (class (find-class symbol))
        (slots ()))
    (setf doc-list (list symbol))
    (setf doc-list (append doc-list (list 'CLASS)))
    (if (subtypep (type-of class) 'omclass)
        (multiple-value-bind (names vals docs)
            (get-slot-in-out-names (make-instance symbol))
          (let ((slpairs (mat-trans (list names docs)))
                (defslots (get-all-initargs-of-class symbol)))
                     ;(if defslots 
                         ;(dolist (slot defslots)
                         ;  (setf slots (append slots (list (list (name slot) 
                         ;                                    (cadr (find (name slot) slpairs :test 'string-equal :key 'car)))))))
                         
            (setf slots slpairs)
                       ;)
            ))
      (dolist (slot (class-slots (find-class symbol)))
        (setf slots (append slots (list (list (slot-name slot) nil))))))
    (setf doc-list (append doc-list (list slots)))
    (setf doc-list (append doc-list (list (class-documentation (find-class symbol)))))
    doc-list))
    
; (get-slot-in-out-names (make-instance 'textfile))
;(om-get-documentation 'bpf)
;(subtypep (class-of (find-class 'chord)) 'omclass)
;(mapcar 'name (get-all-initargs-of-class 'om::add-1))

(defmethod om-show-reference-doc ((symbol t) &optional dir)
  (om-beep))

;(om-show-reference-doc 'om+)

(defmethod om-show-reference-doc ((symbol symbol) &optional dir)
  (let ((file (om-make-pathname :directory (or dir *om-reference-dir*)
                                :name (special-path-check (string-downcase (string symbol)))
                                :type "html")))
    (if (probe-file file)
        ;(om-shell (concatenate 'string "open " (namestring file)))
      (sys:open-url (namestring file))
      (om-show-documentation symbol))))

;;; show doc in win
(defun om-show-documentation (symbol) 
  (let ((doc-list (om-get-documentation symbol))
        (lineslist nil))
    (if (or (null doc-list) (null (nth 3 doc-list)))
        (om-beep)
      (let ((*print-case* :downcase))
        (setf lineslist (append lineslist (list (string-upcase (format nil "~%~S~%" (car doc-list))))))
        (cond ((member (nth 1 doc-list) (list 'GENERIC-FUNCTION 'FUNCTION))
               (let ((argstring "    Arguments: "))
                 (if (null (nth 2 doc-list))
                     (setf argstring (concatenate 'string argstring "()"))
                   (loop for arg in (nth 2 doc-list) do (setf argstring (concatenate 'string argstring (format nil " ~a" arg)))))
                 (setf lineslist (append lineslist (list argstring))))
               (setf lineslist (append lineslist  (list (format nil "    [~S]" (nth 1 doc-list)))))
               (setf lineslist (append lineslist (list ""))) (setf lineslist (append lineslist (list "")))
               (setf lineslist (append lineslist (list (nth 3 doc-list)))))
              ((equal (nth 1 doc-list) 'CLASS)
               (setf lineslist (append lineslist  (list (format nil "    [~S]" (nth 1 doc-list)))))
               (let ((argstring "    Slots: "))
                 (if (null (nth 2 doc-list))
                     (setf argstring (concatenate 'string argstring "()"))
                   (loop for arg in (nth 2 doc-list) do (setf argstring (concatenate 'string argstring (format nil " ~a" arg)))))
                 (setf lineslist (append lineslist (list argstring))))
               (setf lineslist (append lineslist (list "")))
               (setf lineslist (append lineslist (list (nth 3 doc-list)))))
              (t (setf lineslist (append lineslist (list (format nil "Function or class ~S could not be found" symbol)))))
              )
        (om-show-output-lines lineslist
                              (concatenate 'string "Documentation for " (string-upcase (string symbol))))
        ))))



;;;; DOCUMENTATION FOR THE LISP FUNCTIONS IN OM PACKAGE

(setf (documentation 'first 'function) 
      "Returns the 1st element in <list>.

(Equivalent to Lisp CAR)

Ex. (first '(1 2 3 4 5 6)) ==> 1")

(setf (documentation 'second 'function) 
      "Returns the 2nd element in <list>.

Ex. (second '(1 2 3 4 5 6)) ==> 2")

(setf (documentation 'third 'function) 
      "Returns the 3rd element in <list>.

Ex. (third '(1 2 3 4 5 6)) ==> 3")

(setf (documentation 'nth 'function) 
      "Returns the <n>th element in <list>.
The count starts from 0, i.e. (nth 0 list) is the first element of the list.

Ex. (nth 0 '(1 2 3 4 5 6)) ==> 1
Ex. (nth 2 '(1 2 3 4 5 6)) ==> 3")

(setf (documentation 'rest 'function) 
      "Returns the tail of <list>, i.e. the same list without irts first element.

(Equivalent to Lisp CDR)

Ex. (rest '(1 2 3 4 5 6)) ==> (2 3 4 5 6)")

(setf (documentation 'nthcdr 'function) 
      "Returns the tail of <list> that would be obtained by calling REST <n> times in succession, i.e. without its <n> first elements.

Ex. (nthcdr 2 '(1 2 3 4 5 6)) ==> (3 4 5 6)")

(setf (documentation 'butlast 'function) 
      "Returns a copy of <list> without its last element or without its last <n> elements if <n> is supplied.

Ex. (butlast '(1 2 3 4 5 6)) ==> (1 2 3 4 5)
Ex. (butlast '(1 2 3 4 5 6)) ==> (1 2 3)")

(setf (documentation 'reverse 'function) 
      "Returns a new sequence or list of the same kind as <sequence>, containing the same elements but in reverse order.

Ex. (reverse '(1 2 3 4 5 6)) ==> (6 5 4 3 2 1)")

(setf (documentation 'length 'function) 
      "Returns the number of elements in <sequence> (a list, a string, ...)

Ex. (length '(1 2 3 4 5 6)) ==> 6
Ex. (length \"hello\") ==> 5")

(setf (documentation 'list 'function) 
      "Returns a list containing the supplied objects (<args>).

Ex. (list 1 2 'a 7) ==> (1 2 a 7)


LIST also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.
")

(setf (documentation 'remove 'function) 
      "Returns a new sequence that has the same elements as <sequence> except those that satisfy the test <test> with <item>.
By default the test is 'eql so the items that are equal to <item> are removed.

<test> can be a function or a function name.
<start> and <end> determine bounding indices in the original sequence for removing elements.
<count> allows to specify a maximal number of items to remove.
<from-end> if T, starts removing items from end of the sequence
<key> is a function applyed to each item before to be tested
<test-not> is used to remove elemet that do not satistfy the test (deprecated use)

Ex. (remove 5 '(2 5 6 7 5 3)) ==> (2 6 7 3)
Ex. (remove 5 '(2 5 6 7 5 3) :test '>) ==> (2 3)
Ex. (remove 5 '((b 2) (c 5) (d 6) (e 7) (f 5) (g 3))) :key 'second) ==> ((b 2) (d 6) (e 7) (g 3))
")


(setf (documentation 'cons 'function)
      "Creates a new CONS with <car> and <cdr>. 
A CONS is a basic compound data object having two components called the CAR and the CDR
A LIST is recursively defined as a CONS which CDR is a list.

Ex. (cons 'a 'b) ==> (a . b)
Ex. (cons 'a nil) ==> (a)
Ex. (cons 'a '(b c)) ==> (a b c)
Ex. (cons 'a (cons 'b nil)) ==> (a b)")


(setf (documentation 'append 'function) 
      "Returns a new list that is the concatenation of the <lists>.

Ex. (append '(1 2 3) '(4 5)) ==> (1 2 3 4 5)")

(setf (documentation 'apply 'function) 
     "Applies <function> to the arguments in <arg>.

<function> is a function or function name.
<arg> is a list of arguments.

Ex. (apply '+ '(3 4)) ==> 7
")

(setf (documentation 'funcall 'function) 
      "Applies <function> to <args>.

<function> is a function or function name.
<args> are the arguments.

Ex. (funcall '+ 3 5) ==> 8")

(setf (documentation 'mapcar 'function) 
      "Operates on successive elements of <list> (and of the other lists of <more-lists>). 

<function> can be a function or function name. It is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the list runs out, or when the shorter list runs out if various lists are supplied. Excess elements in other lists are ignored. 

The value returned is a list of the results of successive calls to <function>.")

(setf (documentation 'mapcan 'function) 
      "Operates on successive elements of <list> (and of the other lists of <more-lists>). 

<function> can be a function or function name. It is applied to the first element of each list, then to the second element of each list, and so on. The iteration terminates when the list runs out, or when the shorter list runs out if various lists are supplied. Excess elements in other lists are ignored. 

The value returned is a the concatenation of the results of successive calls to <function>. <function> should therefore return lists.")


(setf (system::class-documentation (find-class 't))
      "Special constant in Lisp, meaning 'TRUE'.

T can be used as a type specifier in class slots or method arguments in order to describe any object type (all objects inherit from the Lisp class 'T').")

(setf (system::class-documentation (find-class 'integer))
      "Integer number.

This class is mainly used in OM to set types in other class slots or method arguments.")

(setf (system::class-documentation (find-class 'number))
      "Any type of number.

This class is mainly used in OM to set types in other class slots or method arguments.")


(setf (documentation 'float 'function)
      "Float number (decimal).

FLOAT exists in Lisp as a functions, which converts its input (a real number) to a float number.
Ex. (float 4)  =>  4.0
Ex. (flat 1/2) =>  0.5

FLOAT also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.")

(setf (documentation 'rational 'function)
      "Rational number (P/Q).

RATIONAL exists in Lisp as a functions, which converts its input (a real number) to a rational number.

It also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.")

(setf (documentation 'string 'function)
      "Vector of characters.

STRING exists in Lisp as a functions, which converts its input (another string, a symbol or a character) into a string.
Ex. (string 'hello) => \"hello\"

It also exists as a type specifier (or class).
The type specifier is mainly used in OM to set types in other class slots or method arguments.

Strings are represented as characters between double-quotes (\"\").

")


(setf (documentation 'null 'function)
      "In Common Lisp, NIL, also notated '(), represents the empty list.

The function NULL tests if something is equal to NIL or not.
This function returns T if the argument is NIL, and NIL if not.

Ex. (null 4) => NIL
Ex. (null NIL) => T
Ex. (null '()) => T
Ex. (null '(5 6 7)) => NIL

NULL is also the name of a class (the 'class of NIL'), and can be used in OM to specialize method arguments.")


;;; General Documentations

(defmethod help-items ((self editorview)) 
  (let ((funref (om-make-pathname :directory *om-reference-dir*
                                  :name "index" :type "html"))
        ;(usermanual "http://support.ircam.fr/docs/om/om6-manual/")
        (usermanual "https://openmusic-project.github.io/openmusic/doc/om-manual/OM-Documentation")
        )
    (list 
     (list 
   ;(om-new-leafmenu "Box Info" #'(lambda() (editor-show-infowindow self)))
      (om-new-leafmenu "Online User Manual" #'(lambda() (sys:open-url usermanual)) nil)
      (om-new-leafmenu "Function Reference" #'(lambda() (sys:open-url (namestring funref))) nil (probe-file funref))
      ))))
  