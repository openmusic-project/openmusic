;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
;This file contains the abstract classes for the system definition.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defvar *genfun-att-list* '(name icon documentation numouts inputs-default 
                            inputs-doc inputs-menus ))

;Used to cons the pop-up-menu for special inputs

(defmethod get-menu-list ((self t)) self)
(defmethod get-menu-list ((self symbol)) (eval self))


(defun get-menu-input (indice list)
   (let (rep i)
     (loop for item in list
           while (not rep) do
           (setf i (if (integerp (car item)) (car item)
                     (read-from-string (format nil "~d" (car (eval item))))))
           (when (= i indice)
             (setf rep item)))
     rep))




(defun parse-defgeneric* (args)
   (let* (icon numouts initvals doc menuins indoc)
     (loop for theargs in args do
           (cond
            ((equal (car theargs) :numouts)  (setf numouts  (second theargs)))
            ((equal (car theargs) :initvals)  (setf initvals (second theargs)))
            ((equal (car theargs) :indoc)     (setf indoc (second theargs)))
            ((equal (car theargs) :doc)       (setf doc (second theargs)))
            ((equal (car theargs) :documentation) (setf doc (second theargs)))
            ((equal (car theargs) :icon)      (setf icon (second theargs)))
            ((equal (car theargs) :menuins)   (setf menuins (second theargs)))))
     (unless numouts (setf numouts 1))
     (unless doc (setf doc "no documentation for this function"))
     (unless icon    (setf icon 150))   ;an icon by default
     (values numouts initvals icon indoc doc menuins)))

(defun remove-om-options (list)
  (loop for item in list
        when (not (or (member (string (car item)) *genfun-att-list* 
                              :test 'string-equal :key 'string) 
                      (equal (car item) :doc)
                      (equal (car item) :generic-function-class)
                      (equal (car item) :method-class))) collect item))


(defmacro defgeneric* (function-name lambda-list &rest options-and-methods &environment env)
   (multiple-value-bind (numouts initvals icon indoc doc menuins)
                        (parse-defgeneric* options-and-methods)
     (unless initvals
         (setf initvals `',(make-list (length lambda-list) :initial-element nil)))
       (unless indoc
         (setf indoc `',(make-list (length lambda-list) :initial-element "no documentation")))
     `(let* ((gen-fun (defgeneric ,function-name ,lambda-list  
                               (:documentation ,doc) 
                               (:generic-function-class ,*def-metaclass-genfun*)
                               (:method-class ,*def-metaclass-method*)
                               ,.(remove-om-options options-and-methods))))
               (setf (numouts gen-fun) ,numouts)
               (setf (inputs-default gen-fun) ,initvals)
               (setf (inputs-doc gen-fun) ,indoc)
               (setf (inputs-menus gen-fun) ',menuins)
               (setf (icon gen-fun) ,icon)
               (setf (name gen-fun) ,(string function-name))
        gen-fun)))
