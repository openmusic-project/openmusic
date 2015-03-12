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
;This file contains the meta-object OMMethod.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)

;-----------------
;The Methods in OM
;-----------------
#|
(defclass OMMethod (standard-method OMPersistantObject) 
   ((saved-connections :initform nil :accessor saved-connections)
    (graph-fun :initform nil :accessor graph-fun)
    (compiled? :initform t :accessor compiled?)
    (pictu-list :initform nil :accessor pictu-list)
    (class-method-p :initform nil :accessor class-method-p))
   (:documentation "The class of the OM method metaobject. #enddoc#
#seealso# (omgenericfunction) #seealso#
#saved-connections# Used to save connections beetween boxes in the method definition. #saved-connections#
#graph-fun# A list of the boxes which define the method. #graph-fun#
#compiled?# Nil if the method was modified and not compiled. #compiled?#
#class-method-p# T if the method is a class method. 
Class methods are the init-instance method and slot reader and writer. #class-method-p#")
   (:default-initargs :protected-p t)
   (:metaclass omstandardclass))
|#


;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------
(defmethod allow-alias ((self OMMethod))
   "Methods do not allow alias" nil)

(defmethod get-documentation ((self OMMethod))
   ;"Methods have not documentation, see the generic fonction of the method."
  (get-documentation (method-generic-function self)))

(defmethod get-elements ((self OMMethod))
   "Elements of a method are boxes."
   (graph-fun self))

(defmethod get-object-insp-name ((self OMMethod)) "Method")

(defmethod obj-file-type ((self OMMethod)) :METH)
(defmethod obj-file-extension ((self OMMethod)) "ome")

(defmethod omNG-add-element ((self OMMethod) elem)
   "Add the box 'elem' to the elements of 'self'."
   (setf (mycontainer elem) self)
   (push elem (graph-fun self)))

(defmethod omng-remove-element ((self OMMethod) elem)
   "Delete the box 'elem' from the elements of 'self'."
   (setf (graph-fun self) (remove elem (graph-fun self) :test 'equal)))


(defmethod method-name-ID ((self OMMethod))
  "Cons a unique name for a method : 'name-qualifier-spec1x...xspecn'."
  (let ((qualy (car (method-qualifiers self)))
        (spec (loop for item in (method-specializers  self)
                  collect (subseq (string (class-name item)) 0 1))))
    (if qualy
        (format nil "~D-~D~{x~D~}" (name self) (string qualy) spec)
      (format nil "~D~{x~D~}" (name self)  spec))))
       
        
(defmethod omNG-save ((self OMMethod) &optional (thepath nil))
  "Save the method 'self'."
  (setf *libs-to-load* nil)
  (setf *resources-to-load* nil)
  (let ((newpath (make-pathname :directory (pathname-directory thepath) 
                                :name (method-name-ID self)
                                :type "ome"))
        (method-code (om-save-methods self)))
    ;(loop while (probe-file newpath)
    ;      for i = 0 then (+ i 1) do
    ;      (setf newpath (make-pathname :directory (pathname-directory newpath)
    ;                                   :name (string+ (pathname-name newpath) (format nil "~D" i))  ;; ??
    ;                                   :type (pathname-type newpath))))
    (delete-file-protection newpath)
    (WITH-OPEN-FILE (out newpath :direction :output :if-does-not-exist :create 
                         :if-exists :supersede ) ;;;        :external-format :METH)
      (write-header self  out)
      (write-resources self *resources-to-load* out)
      (prin1 '(in-package :om) out)
      (prin1 `(load-lib-for-first ',(remove-duplicates *libs-to-load* :test 'string-equal)) out)
      (let ((*package* (find-package :om)))
        (prin1 method-code out)))
    )
  (setf *libs-to-load* nil)
  (setf (saved? self) t))



(defmethod get-editor-class ((self OMMethod)) 'MethodEditor)

(defmethod OpenEditorframe ((self OMMethod))
   "Open the method editor, The method is redefined when this window is close."
   (or (editorframe (print self))
       (cond
        ((class-method-p self)
         (open-class-ommethode self))
        (t (if (graph-fun self)
             (panel (open-new-RelationFrame self 
                                            (format nil "OM Method -  ~A ~A" 
                                                    (name self) 
                                                    (if (car (method-qualifiers self)) 
                                                        (string+ "(:" (string (car (method-qualifiers self))) ")")
                                                      ""))
                                            (graph-fun self)))
             (not (dialog-message (format nil "This is a Lisp method. It can not be open as a visual program.~% [If it is available, you can access the source code of this function or method using the shortcut 'e' in the patch editors]"))))))))

;--------------------------------------------------
;Other Methods
;--------------------------------------------------
(defmethod ommethod-p ((self OMMethod)) t)
(defmethod ommethod-p ((self t)) nil)

;The icon of a method is the same as its generic function.
(defmethod icon ((self OMMethod))
   (icon (fdefinition (method-name self))))
 
(defmethod make-remove-method ((self OMMethod))
   "Delete the method 'self' from its generic function;"
   (when (mypathname self)
     (om-delete-file (mypathname self)))
   (remove-method (fdefinition (method-name self)) self))


(defmethod open-class-ommethode ((self OMMethod))
   "Open the editor of a special method (i.e. init-instance, slot's reader and writer)"
   (let* ((speci (method-specializers self))
          (theclass (car speci)))
     (when (editorframe theclass) (om-close-window (window (editorframe theclass))))
     (unless (graph-fun self)
       (init-graphfun-method 'init self))
     (if (graph-fun self)
       (panel (open-new-RelationFrame self 
                                      (string+ "Special Method - " (name self))
                                      (graph-fun self)))
       (not (dialog-message (string+ "I can not open this method, because it is empty !!"))))))

(defmethod method-equal ((self OMMethod) (obj OMMethod))
   "T if 'self' and 'obj' are the same method"
   (and (string-equal (name self) (name obj))
        (equal (method-specializers self) (method-specializers obj))
        (equal (method-qualifiers self) (method-qualifiers obj))))

(defmethod method-equal ((self t) (obj t))
   "T if 'self' and 'obj' are the same method" nil)

(defmethod qualifieurs ((self OMMethod))
  (let ((qual (method-qualifiers self)) rep)
    (when qual
      (cond 
       ((string-equal (string (car qual)) "BEFORE") (setf rep (list :before)))
       ((string-equal (string (car qual)) "AFTER") (setf rep (list :after)))
       ((string-equal (string (car qual)) "AROUND") (setf rep (list :around)))
       (t (setf rep (list (car qual)))))
      rep)))

(defmethod put-boxes-in-method ((self OMMethod) boxes)
   (let ((boxeslist (reverse boxes)))
     (setf (graph-fun self) nil)
     (loop for item in boxeslist do
          (when item (omng-add-element self item)))))


;-----------------
;Method Definition
;-----------------

;---------------------------------------------------
;select the optional keys in the function definition
;---------------------------------------------------
;(defun parse-defmethod* (name args)
;   (unless (function-spec-p name) (error "Illegal arg ~S" name))
;   (let* ((theargs args)
;          (body? nil)
;          qualy lambda-list icon numouts initvals doc menuins body indoc)
;     (when (or (equal (car theargs) :after) (equal (car theargs) :before) (equal (car theargs) :around))
;       (setf qualy (pop theargs)))
;     (setf lambda-list (pop theargs))
;     (loop while (and theargs (not body?))
;           do
;           (cond
;            ((equal (car theargs) :numouts)  (pop theargs) (setf numouts  (pop theargs)))
;            ((equal (car theargs) :initvals) (pop theargs) (setf initvals (pop theargs)))
;            ((equal (car theargs) :indoc)    (pop theargs) (setf indoc (pop theargs)))
;            ((equal (car theargs) :doc)      (pop theargs) (setf doc (pop theargs)))
;            ((equal (car theargs) :icon)     (pop theargs) (setf icon     (pop theargs)))
;            ((equal (car theargs) :menuins)  (pop theargs) (setf menuins   (pop theargs)))
;            ((stringp (car theargs))         (setf doc (pop theargs)))
;            (t                               (setf body theargs) (setf body? t))))
;     (unless numouts (setf numouts 1))
;     (unless doc (setf doc "no documentation for this function"))
;     (unless icon (setf icon 150))   ;an icon by default
;     (values qualy lambda-list numouts initvals icon indoc doc menuins body)))

(defun parse-defmethod* (name args)
   (unless (function-spec-p name) (error "Illegal arg ~S" name))
   (let* ((theargs args)
          (body? nil)
          qualy lambda-list icon numouts initvals doc menuins body indoc outdoc)
     (when (or (equal (car theargs) :after) (equal (car theargs) :before) (equal (car theargs) :around))
       (setf qualy (list (pop theargs))))
     (setf lambda-list (pop theargs))
     (loop while (and theargs (not body?))
           do
           (cond
            ((equal (car theargs) :numouts)  (pop theargs) (setf numouts  (pop theargs)))
            ((equal (car theargs) :initvals) (pop theargs) (setf initvals (pop theargs)))
            ((equal (car theargs) :indoc)    (pop theargs) (setf indoc (pop theargs)))
            ((equal (car theargs) :outdoc)    (pop theargs) (setf outdoc (pop theargs)))
            ((equal (car theargs) :doc)      (pop theargs) (setf doc (pop theargs)))
            ((equal (car theargs) :icon)     (pop theargs) (setf icon     (pop theargs)))
            ((equal (car theargs) :menuins)  (pop theargs) (setf menuins   (pop theargs)))
            ((stringp (car theargs))         (setf doc (pop theargs)))
            (t                               (setf body theargs) (setf body? t))))
     (values qualy lambda-list numouts initvals icon indoc outdoc doc menuins body)))

;---------------------------------------------
;Take only the name parameter without the type
;---------------------------------------------
(defun get-lambda-var (list)
  (mapcar #'(lambda (nom) (first? nom)) list))


;-------------------------------------------------------------------------
;Define an OM method and an OM Generic function if this last doesn't exist
;-------------------------------------------------------------------------
;(defmacro defmethod* (name &rest args)
;   (multiple-value-bind (qualy lambda-list numouts initvals icon indoc doc menuins body)
;                        (parse-defmethod* name args)
;     (let ((lambda-var (get-lambda-var lambda-list)))
;       (unless initvals
;         (setf initvals `',(make-list (length lambda-var) :initial-element nil)))
;       (unless indoc
;         (setf indoc `',(make-list (length lambda-var) :initial-element "no documentation")))
;       `(progn
;          (unless (fboundp ',name)
;            (let* (gen-fun)
;              (setf gen-fun (defgeneric ,name ,lambda-var  
;                              (:documentation ,doc) 
;                              (:generic-function-class ,*def-metaclass-genfun*)
;                              (:method-class ,*def-metaclass-method*)))
;              (setf (numouts gen-fun) ,numouts)
;              (setf (inputs-default gen-fun) ,initvals)
;              (setf (inputs-doc gen-fun) ,indoc)
;              (setf (inputs-menus gen-fun) ,menuins)
;              (setf (icon gen-fun) ,icon)
;              (setf (name gen-fun) ,(string name))))
;          (let ((method (if ,qualy
;                          (defmethod ,name ,qualy ,lambda-list  ,.body)
;                          (defmethod ,name ,lambda-list  ,.body))))
;            (setf (name method) ,(string name))
;            method)))))


(defun def-method-icon () 150)
 
(defmacro defmethod* (name &rest args)
   (multiple-value-bind (qualy lambda-list numouts initvals icon indoc outdoc doc menuins body)
                        (parse-defmethod* name args)
     (let ((lambda-var (get-lambda-var lambda-list))
            iv id od)
       (setf iv (or initvals `',(make-list (length lambda-var) :initial-element nil)))
       (setf id (or indoc `',(make-list (length lambda-var) :initial-element "")))
       (setf od (or outdoc `',(make-list (length lambda-var) :initial-element nil)))
       `(let (gen-fun nouts method)
          (unless (fboundp ',name)
            (progn
              (setf gen-fun (defgeneric ,name ,lambda-var
                              (:documentation ,doc)
                              (:generic-function-class ,*def-metaclass-genfun*)
                              (:method-class ,*def-metaclass-method*)))
              
              (setf (numouts gen-fun) (or ,numouts 1))
              (setf (inputs-default gen-fun) ,iv)
              (setf (inputs-doc gen-fun) ,id)
              (setf (outputs-doc gen-fun) ,od)
              (setf (icon gen-fun) ,icon) ;;; (or ,icon (def-method-icon)))
              (setf (name gen-fun) ,(string name))))
          
          (unless gen-fun 
            (setf gen-fun (fdefinition ',name))
            (when ,doc (setf (documentation gen-fun 'function) ,doc)))
          
          (when ,icon (setf (icon gen-fun) ,icon))
          (when ,numouts (setf (numouts gen-fun) ,numouts))
          (when ,initvals (setf (inputs-default gen-fun) ,initvals))
          (when ,indoc (setf (inputs-doc gen-fun) ,indoc))
          (when ,outdoc (setf (outputs-doc gen-fun) ,outdoc))
          (when ,menuins (setf (inputs-menus gen-fun) ,menuins))
          
          (setf method (defmethod ,name ,.qualy ,lambda-list  ,.body))
          (setf (name method) ,(string name))
          
          method))))




;--------------------------Tools

;-----------save methods as patches.
;When you delete a class, methods specialized by it, are deleted also.
;OM creates a folder with this methods saved as patches.
(defun save-methods-as-patch (methodlist class)
   (declare (special *current-workspace*))
   (let* ((scrollframe (editorframe *current-workSpace*))
          (foldername (mk-unique-name  scrollframe (string+ class "methods")))
          (new-object (omNG-make-new-folder foldername (om-make-point 10 10)))
          new-frame)
     (setf new-frame (make-icon-from-object  new-object 10 10 1 1))
     (omg-add-element scrollframe new-frame)
     (loop for met in methodlist do
           (let ((newpatch (make-patch-from-method met)))
             (omNG-add-element new-object newpatch)
             (make-remove-method met)))
     foldername))

(defmethod make-patch-from-method ((self OMMethod))
   (let ((new-object (omNG-make-new-patch (string (method-name self)) (om-make-point 10 10)))
         (newboxes (mapcar #'(lambda (box) (special-omNG-save box)) (graph-fun self)))
         (connections (mk-connection-list (graph-fun self))))
     (setf (boxes new-object) nil)
     (mapc #'(lambda (box) (omNG-add-element new-object (eval box)))  newboxes)
     (setf (boxes new-object) (reverse (boxes new-object)))
     (remk-connections (boxes new-object) (loop for con in connections collect (load-connection con)))
     (make-remove-method self)
     new-object))

(defmethod special-omNG-save ((self t))
   "Only OMtypedin must be changed to save methods as patches"
   (omNG-save self t))







