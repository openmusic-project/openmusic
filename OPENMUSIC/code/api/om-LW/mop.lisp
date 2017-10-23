;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;;===========================================================================
;DocFile
; METAOBJECT PROGRAMMING UTILITIES 
;DocFile
;;===========================================================================


(in-package :om-api)
;;;=========================
;;; export :
;;;=========================
(export '(
          ;class-slots
          class-instance-slots
          class-class-slots
          class-direct-instance-slots
          class-direct-class-slots
          slot-name
          get-class-default-initargs
          ;;;save-initargs
          get-class-precedence-list
          function-spec-p
          function-name
          method-name
          arglist
          om-remove-method-definition
          copy-instance
          eql-specializer?
          eql-specializer-object

          function-documentation
          class-documentation
          ) :om-api)
;;;=========================


(defun method-name (method)
  (clos::generic-function-name (clos::method-generic-function method)))


(defun class-slot? (slot)
  (equal :CLASS (clos::slot-definition-allocation slot)))

(defun slot-name (slot)
  (clos::slot-definition-name slot))

;(defmethod class-slots ((class t))
;  (hcl::class-slots class))

(defmethod class-instance-slots ((class t))
  (loop for item in (hcl::class-slots class)
        when (not  (class-slot? item)) collect item))

(defmethod class-class-slots ((class t))
  (loop for item in (hcl::class-slots class)
        when (class-slot? item) collect item))

(defmethod class-direct-instance-slots ((class t))
  (loop for item in (hcl::class-direct-slots class)
        when (not  (class-slot? item)) collect item))

(defmethod class-direct-class-slots ((class t))
  (loop for item in (hcl::class-direct-slots class)
        when (class-slot? item) collect item))

(defun OM-REMOVE-METHOD-DEFINITION (method) t)

;(defun OM-REMOVE-METHOD-DEFINITION (method)
;  (remove-method (fdefinition (method-name method)) method))
    
(defun get-class-default-initargs (class)
  (hcl::class-default-initargs class))


(defun get-class-precedence-list (class)
   (hcl::class-precedence-list class))

(defun function-spec-p (spec) t)

(defun eql-specializer? (method-specializer)
  (consp method-specializer))

(defun arglist (fun)
  (if (symbolp fun) (setf fun (fdefinition fun)))
  (if (clos::generic-function-p fun)
      (hcl::method-lambda-list (car (hcl::generic-function-methods fun)))
    (clos::extract-lambda-list-names-and-keys (function-lambda-list fun))))

(defmethod function-name (fun)
  (if (symbolp fun) (setf fun (fdefinition fun)))
  (if (clos::generic-function-p fun)
      (hcl::generic-function-name fun)
    (system::function-name fun)))

(defun function-documentation (function)
  (or (system::function-documentation function)
      (documentation function 'function)))

(defun class-documentation (class)
  (let ((realclass (if (symbolp class) (find-class class nil) class)))
    (if realclass
        (system::class-documentation class)
      (concatenate 'string "Class " (string class) " not found"))))
  



