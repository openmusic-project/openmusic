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
;This file contains the meta-class OMClass, which extend the standard-class
;We define also the class Omstandardclass, a subclass of omclass, that is the meta-class
;of all omobjects.
;Last Modifications :
;18/10/97 first date.
;DocFile

;;; JB - Notes 
;;; redef-class : use ccl function class-default-initargs

(in-package :om)


#|
(defclass omclass (standard-class ompersistantobject) 
   ((lib-class-p :initform nil :accessor lib-class-p)
    (internal-met :initform nil :accessor internal-met)
    (slot-docs :initform nil :accessor slot-docs))
   (:documentation "OM meta-class. #enddoc#
#seealso# (omslot ommethod omstandardclass) #seealso#
#lib-class-p# Non nil, if the function was defined in a Library. #lib-class-p#
#internal-met# A list with initialization and R/W slots methods. #internal-met#
#doc# Doc of somes persistants object are saved also. #doc#"))

(defclass omstandardclass (omclass)  ()
   (:documentation "This is the current OM meta-class, you can sub-class it and used the new class as current meta-class.
This class is a OMClass a diferencia of OMClass.#enddoc#
#seealso# (omclass) #seealso#")
   (:metaclass OMClass))
|#

;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------
(defmethod allow-alias ((Self Omclass))
   "Alias can be made from omboxclass objects"  (om-beep-msg "Make class alias only in the class tree"))

;;; + doc-type pour version ACL 7.0
(defmethod get-documentation ((Self Omclass))
   "Doc for classes is managed by MCL" 
   (documentation self t))

(defmethod get-elements ((Self Omclass))
   "Elements of a class are the direct slots"
   (get-direct-slots-of-class (class-name self)))

(defmethod get-class-icon ((Self Omclass)) 'class-icon-frame)

(defmethod get-object-insp-name ((Self Omclass)) "class")

(defmethod obj-file-type ((Self Omclass)) :CLAS)
(defmethod obj-file-extension ((self Omclass)) "omc")

(defmethod omng-add-element ((Self Omclass) Elem)
   "Add a slot and redefine the class"
   (let ((Theslots (get-elements self))
         Newclass)
     (setf theslots (list+ theslots (list elem)))
     (setf newclass (redef-class self (make-super-class-list self) (make-slot-list theslots)))
     newclass))

;;;(defmethod omng-change-container ((Self Omclass) Oldcont Newcont)
;;;   "The container of a class is a package, change of container can be made only in the user package"
;;;   (setf (classes oldcont) (remove self (classes oldcont) :test 'equal))
;;;   (push self (classes newcont))
;;;   (when (mypathname self)
;;;     (rename-file (mypathname self) (special-pathname (corrige-pack-path  newcont) (name self)))
;;;     (setf (mypathname self) (special-pathname (mypathname newcont) (name self))))
;;;   (upDateUsermenu))

(defmethod omng-change-container ((Self Omclass) Oldcont Newcont)
   "The container of a class is a package, change of container can be made only in the user package"
   (setf (classes oldcont) (remove self (classes oldcont) :test 'equal))
   (push self (classes newcont))
   (let ((newpath (make-pathname :directory (pathname-directory (corrige-pack-path newcont)) 
                                 :name (name self) :type (obj-file-extension self))))
   (when (mypathname self)
     (rename-file (mypathname self) newpath)
     (setf (mypathname self) newpath))
   ;(upDateUsermenu)
   t))


(defmethod omng-copy ((Self Omclass)) "Classes can not be duplicated" (om-abort))

(defmethod omng-remove-element ((Self Omclass) Elem)
   "Remove a slot from the class 'self' and redefine the class"
   (let ((Theslots (get-elements self))
         Newclass)
     (setf theslots (remove-if #'(lambda (Slot) (string-equal (name slot) (name elem))) theslots))
     (setf newclass (redef-class self (make-super-class-list self) (make-slot-list theslots)))
     newclass))



(defmethod omng-save ((Self Omclass) &optional (Thepath nil))
  "Save in disk the class 'self'."
  (setf *libs-to-load* (class-needed-libraries self))
  (setf *resources-to-load* nil)
  (let ((Newpath (make-pathname :directory (pathname-directory thepath) :name (name self) :type "omc"))
        (Class-Code (om-save-class self))
        (Initial-Code (list (class-name self) (save-init-met-class self))))
    (delete-file-protection newpath)
    (with-open-file (out newpath :direction :output :if-does-not-exist :create 
                         :if-exists :supersede)  ;;;; :external-format :CLAS)
      (write-header self out)
      (write-resources self *resources-to-load* out)
      (prin1 '(in-package :om) out)
      (prin1 `(load-lib-for-first ',(remove-duplicates *libs-to-load* :test 'string-equal)) out)
      (let ((*Package* (find-package :om)))
        (prin1 `(,(name self) ,class-code ,initial-code) out)))
    (setf *libs-to-load* nil)
    (setf (saved? self) t)))


(defmethod save-init-met-class ((Self Omclass))
   "Generete code for save the internal methods (init, set slots, etc.) of the class 'self'."
   (loop for met in (internal-met self) 
         when (graph-fun met) 
         collect (om-save-methods met)))


(defmethod get-editor-class ((Self Omclass)) 'ClassEditor)

(defmethod openeditorframe ((Self Omclass))
   "Option-key +double click to open the initialize instance method, else open the slot editor."
   (or (editorframe self)
       (let* (Thescroller)
         (when (editorframe (get-class-init-method self)) 
           (om-close-window (window (editorframe (get-class-init-method self)))))
         (setf thescroller (panel (open-new-nonrelationFrame self (string+ "OM Class - "(name self)) (get-elements self))))
         (add-titles thescroller)
         thescroller)))

;;; + doctype pour documentation (pour ACL 7.0)
(defmethod set-doc ((Self Omclass) Newdoc)
   "Doc for classes is managed by MCL"
   (setf (documentation self t) newdoc)
   (resave-class self))


;------------Other methods-----------------------------
(defmethod omclass-p ((Self T))     nil)
(defmethod omclass-p ((Self Omclass)) t)

(defmethod class-equal ((Self Omclass) (Obj Omclass))
   "T if 'self' is equal to 'obj'"
   (string-equal (name self) (name obj)))

(defmethod make-super-class-list ((Self Omclass))
   "Return a list with the name of the superclasses od 'self' (only the omclass superclasses)"
   (let ((Thelist (class-direct-superclasses self)))
     (setf thelist (remove-if #'(lambda (Class) 
                                  (not (omclass-p class))) thelist))
     (mapcar #'(lambda (Class)
                 (class-name class)) thelist)))




;;; get-default-initagrs defined in mop system-specific files

(defmethod redef-class ((Self Omclass) Superclasses Slots &optional (Udt? nil))
   "Redefine the class 'self' with news slots 'slots' and new superclass list 'superclasses'
The 'udt? flag say if it nexessary a graphic update for atteched objects"
   (let* ((Iconid (icon self))
          (New-Object (eval `(defclass* ,(class-name self)
                               ,superclasses
                               ,slots
                               (:icon ,(car (list! iconID)))
                               (:documentation ,(documentation self t))
                               (:metaclass ,(type-of self))
                               (:default-initargs ,.(save-initargs (get-class-default-initargs self)))
                               (:update ,udt?)))))
     (when (listp iconID)
       (icon-for-user-package new-object (second iconID)))
     (setf (cadr (create-info self)) (om-get-date))
     (resave-class new-object)
     new-object)
  )


;;;(defmethod resave-class ((Self Omclass))
;;;   (when (mypathname self)
;;;     (omng-save self (directory-namestring (translate-logical-pathname (mypathname self))))))
;????
(defmethod resave-class ((Self Omclass))
   (when (mypathname self)
       (omng-save self (make-pathname :directory (pathname-directory (mypathname self))))))

;=====================================
;Deleting a class
;=====================================

(defvar *class-deleted* nil "This list keeps a class that is being erased.")

(defmethod omng-delete ((Self Omclass))
   "OMclasses are not really erased,they are keept in a list and will be deleted in the next OM session."
   (let ((Attached (attached-objs self))
         (Themethods (methods-with-class self)))
     (when (or themethods attached)
       ;(when (or themethods (and  attached  (not (and (null (cdr attached)) (OMBoxClass? (car attached))))))
       
       (if (om-y-or-n-dialog (string+ "Warning, there are some elements attached to class "
                                      (name self)
                                      ". Do you want to delete it ?"))
         (let (Foldername)
           (setf *class-deleted* self)
           (mapc #'(lambda (El) (dead-reference el)) (attached-objs self))
           (when themethods
             (setf foldername  (save-methods-as-patch themethods (name self)))
             (om-beep-msg (format nil "The methods containing inputs of the class ~D have been deleted.~%These methods are saved as patches in the folder ~D in your workspace." (name self) foldername)))
           (setf *class-deleted* nil))
         (om-abort)))
     t))

(defmethod dead-reference ((Self Omclass))
   "The class 'self' know that one of its superclasses was deleted, 
(it is keept in the global var *class-deleted*), this method redifine the class."
   (let ((Theslots (get-elements self))
         (Superclasses (make-super-class-list self))
         Newclass)
     (setf superclasses (remove (class-name *class-deleted*) superclasses :test 'equal))
     (setf newclass (redef-class self superclasses (make-slot-list theslots)))
     newclass))


(defmethod methods-with-class ((Self Omclass))
   "When you delete a class the OMMethods specialized by the class are saved as patches,
This method return a list of these methods."
   (declare (special *package-user*))
   (let ((Allmethods (get-methods *package-user*)) Rep)
     (loop for item in allmethods do
           (when (member self (method-specializers item) :test 'equal)
             (push item rep)))
     rep))


;--------------------------------------------------
;OM Class definition : DEFCLASS* 
;--------------------------------------------------
(defvar *om-class-list* nil "This list contains all classes defined with defclass*")

(defun general-type-list () (list+ (mapcar #'(lambda (X) (name x)) *OM-class-list*) 
                                   (mapcar #'(lambda (X) (name x)) *Basic-Lisp-Types*)))

(defun exist-class-p (Name)
   (position name (general-type-list) :test 'string-equal))

(defun om-parse-class-options (Theargs)
   (let* (Icon Meta Doc Newargs Update?)
     (loop while theargs do
           (cond
            ((equal (caar theargs) :icon)            (setf icon  (second (pop theargs))))
            ((equal (caar theargs) :documentation)   (setf doc  (second (pop theargs))))
            ((equal (caar theargs) :metaclass)       (setf meta  (second (pop theargs))))
            ((equal (caar theargs) :update)          (setf update?  (second (pop theargs))))
            (t      (push (pop theargs) newargs ))))
     (values newargs icon meta doc update?)))
 
;Defclass* use standard defclass, but set automaticly the metaclass to the OMmeta-class.
;This macro fill the omslot of the class object i.e. icon, name.
;If the option :udt? is set to T the macro update all objects (classes, frames, etc.) attached to the class."

#|
(defmacro defclass* (Name Superclass Slots &rest Class-Options)
   (multiple-value-bind (new-options icon metaclass doc up?) (om-parse-class-options class-options)
     (unless icon (setf icon 136))
     (unless metaclass (setf metaclass *def-metaclass-class*))
     (unless doc (setf doc "no doc"))
     `(let ((new-class (defclass ,name ,superclass ,slots 
                         (:metaclass ,metaclass)
                         (:documentation ,doc)
                         ,.new-options)))
        (push new-class *OM-class-list*)
        (setf *OM-class-list* (remove-duplicates *OM-class-list*))
        (setf (name new-class) (string ',name))
        (setf (icon new-class) ,icon)
        (attache-to-superclasses new-class)
        (update-from-reference new-class ,(not up?))
        new-class)))
|#
;new definition adding slot documentation


(defmacro defclass* (Name Superclass Slots &rest Class-Options)
   (multiple-value-bind (new-options icon metaclass doc up?) (om-parse-class-options class-options)
     (unless icon (setf icon 136))
     (unless metaclass (setf metaclass *def-metaclass-class*))
     (unless doc (setf doc ""))
     `(let ((new-class (defclass ,name ,superclass ,slots 
                         (:metaclass ,metaclass)
                         (:documentation ,doc)
                         ,.new-options)))
        (set-slot-documentation new-class ',slots)
        (push new-class *OM-class-list*)
        (setf *OM-class-list* (remove-duplicates *OM-class-list*))
        (setf (name new-class) (string ',name))
        (setf (icon new-class) ,icon)
        (attache-to-superclasses new-class)
        (om-lisp::add-class-definition new-class *load-pathname*)
        (update-from-reference new-class ,(not up?))
        new-class)))


(defmethod set-slot-documentation ((self omclass) slots)
  (setf (slot-docs self)
        (loop for item in slots
              collect (list (string (car item)) (doc-from-list item)))))

(defun doc-from-list (list)
  (let ((position (position :documentation list :test 'equal)))
    (if position
      (nth (+ position 1) list)
      "")))
 
(defmethod all-slot-docs  ((self OMClass))
   (loop for item in (omcpl self)
         append (slot-docs item)))


(defmethod find-slot-doc  ((self OMClass) name)
   (let ((slot-doc (find name (all-slot-docs self) :test 'string-equal :key 'car)))
     (if slot-doc
       (second slot-doc)
       "no doc")))

(defmethod set-slot-doc  ((self OMClass) name doc)
   (let ((position (position name (slot-docs self) :test 'string-equal :key 'car)))
     (when position 
       (setf (nth position (slot-docs self)) (list name doc)))))

      

(defmethod update-from-reference  ((self OMClass) &optional (udt? t))
   "This method is called by defclass* when you redifine the class 'self' and you can update the attached objects."
   (when udt?
     (make-init-instance-method self))
   (mapc #'(lambda (frame)
             (update-from-reference frame udt?)) (attached-objs self)))
   
(defmethod attache-to-superclasses ((self OMClass))
   "Called by defclass* this method put 'self' in each superclass. So if you redifine one superclass, 
it know that it is necessary to update 'self'."
   (let ((thelist (class-direct-superclasses self)))
     (loop for item in thelist do
           (when (and (omclass-p item) (not (member self (attached-objs item)  :test 'equal)))
             (push self (attached-objs item))))))

(defmethod need-initmethod-p ((self OMClass))
   (let ((thelist (class-direct-superclasses self))
         need-initmethod-p)
     (loop for item in thelist do
           (when (omclass-p item)
             (setf need-initmethod-p (or need-initmethod-p (class-has-init-method-p item)))))
     need-initmethod-p))
    
;--------------------------Factory definition----------------

(defmethod make-init-instance-method ((self OMClass))
   (let* ((classname (class-name self))
            (methodname (internp (string+ "om-init-class-def-" (string classname)) (symbol-package classname)))
            ;;;(methodname (interne (string+ "om-init-class-def-" (string classname))))
            class-was-init? initslots newmethod initdocs) 
     (when (fboundp methodname) 
         (setf class-was-init? (and (car (get-elements (fdefinition methodname))) 
                                    (graph-fun (car (get-elements (fdefinition methodname))))
                                    ;(find-class-boxes (graph-fun newmethod) 'OMBoxCallNextInit) ;; new jean: if no box next init, redef ll
                                    ))
         (om-with-redefinitions
          (fmakunbound methodname)))
     (setf initslots (get-initargs-of-class classname))
     (setf initdocs (get-initargs-docs classname))
     (setf newmethod (eval `(defmethod* ,methodname ((self ,classname) ,.initslots)
                                                    :initvals ',(get-initform-initargs classname)
                                                    :indoc (list "object" ,.(mapcar #'(lambda (input) input) initdocs))
                                                    :icon 189
                                                    ,(string+ "Make an instance of the class " (string-downcase (string classname)) (format nil "~%")
                                                                    (get-documentation self))
                                                    t)))
     (setf (graph-fun newmethod) nil)
     (push/replace-init self newmethod)
     (setf (class-method-p newmethod) 'init)
     (if class-was-init?
         (corrige-init-method self newmethod class-was-init?)
       (when (need-initmethod-p self)
         (push/replace-init self (init-graphfun-method 'init newmethod))))))

;;;(defun get-initform-initargs (class)
;;;   (let ((rep (mapcar #'(lambda (slot) (theinitform slot)) (get-all-initargs-of-class class))))
;;;     (push nil rep) rep))
(defun get-initform-initargs (class)
  (let ((rep (mapcar #'(lambda (slot)
                                      (unless (equal :class (alloc slot))
                                                               (theinitform slot))) (get-all-initargs-of-class class))))
    (setf rep (remove nil rep))
    (push nil rep) rep))



;-----------------Defaualt Initialize-instance method----------------

(defmethod init-graphfun-method ((type t) met) (declare (ignore met)) nil)


;;; method-function
(defmethod init-graphfun-method ((type (eql 'init)) met)
   (let* ((outbox (make-new-output "self" 0 (om-make-point 10 240)))
          (speci (method-specializers met))
          (callnext (omNG-make-boxcallnext (car speci) (om-make-point 10 100) "self")))
     (push outbox (graph-fun met))
     (push callnext (graph-fun met))
     (connect-ctrl callnext (first (inputs outbox)) 0)
     (loop for input in (arglist (method-function met))
           for spec in speci
           for i = 0 then (+ i 1) do
           (let* ((inbox (make-new-typed-init (string-downcase (string input))
                                              (class-name spec) i (om-make-point (+ 5 (* i 55)) 45) (zerop i) (class-name (car speci)))))
             (push inbox (graph-fun met))
             (connect-ctrl inbox (nth i (inputs callnext)) 0))))
   (boxes2method met))

;;; method-function
(defmethod corrige-init-method ((self OMClass) newmethod oldboxes)
   "When you redefine a class if it exixst you must update the init method;"
   (om-print (format nil "WARNING the init method for the class ~D has been modified." (class-name self)))
   (let ((names (cons "SELF" (mapcar #'(lambda (input) (string (car input)))
                                     (get-initargs-of-class (class-name self)))))
         (inputs (get-typed-boxes oldboxes))
         (speci (method-specializers newmethod))
         new-method)     
     (setf (graph-fun newmethod) oldboxes)
     (loop for item in inputs do
           (unless (member (name item) names :test 'string-equal)
             (omng-remove-element newmethod item)))
     (setf inputs (get-typed-boxes oldboxes))
     
     (loop for input in (arglist (method-function newmethod))
           for spec in speci
           for i = 0 then (+ i 1) do
           (if (member (string-downcase (string input)) (mapcar #'name inputs) :test 'string-equal)
             (let* ((exist-input (find-if  #'(lambda (x) (string-equal (name x) (string-downcase (string input)))) inputs))
                    (gentype (find-if #'(lambda (x) (equal (class-name x) (class-name spec))) 
                                      (list+ *Basic-Lisp-Types* *OM-class-list*)))
                    (icon (icon gentype)))
               (setf (icon exist-input) icon)
               (setf (reference exist-input) (class-name spec))
               (setf (indice exist-input) i)
               
               (setf (frame-position exist-input) (om-make-point (+ 5 (* i 55)) 45))) 
             (let* ((inbox (make-new-typed-init (string-downcase (string input))
                                                (class-name spec) i (om-make-point (+ 5 (* i 55)) 45) (zerop i) (class-name (car speci)))))
               (push inbox (graph-fun newmethod)))))
     (when (find-class-boxes (graph-fun newmethod) 'OMBoxCallNextInit)
       (redef-boxcallnext (car (find-class-boxes (graph-fun newmethod) 'OMBoxCallNextInit))))
     (loop for item in (get-typed-boxes (graph-fun newmethod)) do
           (unless (equal (reference item) (class-name self))
             (setf (defval item) (get-super-default-value (reference item)))))
     (setf new-method (boxes2method newmethod))
     new-method))

(defmethod class-has-init-method-p ((self OMClass))
   (let ((initmethod (find-if #'(lambda (x) (equal (class-method-p x) 'init)) 
                             (internal-met self))))
     (and initmethod (graph-fun initmethod))))

(defmethod push/replace-init ((self OMClass) met)
   (setf (internal-met self)
         (remove-if #'(lambda (x) (equal (class-method-p x) 'init)) (internal-met self)))
   (push met (internal-met self)))

(defmethod get-class-init-method ((self OMClass))
  (first (get-elements (fdefinition (internp (string+ "om-init-class-def-" (name self)) (symbol-package (class-name self)))))))

(defmethod get-init-method-class-name ((init-meth-name string))
   (subseq init-meth-name 18))
    

;========================================
;Tools
;========================================
(defun omcpl (class)
   "Return the class precedence list, but only with omclasses"
   (loop for item in (get-class-precedence-list class)
         when (omclass-p item) collect item))

(defmethod class-needed-libraries ((self t)) nil)

(defmethod class-needed-libraries ((self OMClass))
   (remove-duplicates (append (list! (lib-class-p self))
                              (loop for item in (remove self (omcpl self))
                                    append (class-needed-libraries item))
                              (libraries-from-slots self))))

(defmethod libraries-from-slots ((self OMClass))
   (let ((slots (get-elements self)) rep)
     (loop for item in slots do
           (let ((typeclass (find-class (thetype item) nil))
                 (thetype (type-of (initformfromslot item)))
                 valclass)
             (when (symbolp thetype)
                 (setf valclass (find-class thetype nil)))
             (when (and typeclass (omclass-p typeclass) (not (equal self typeclass)))
               (setf rep (append rep (class-needed-libraries typeclass))))
             (when (and valclass (omclass-p valclass) (not (equal self valclass)))
               (setf rep (append rep (class-needed-libraries valclass))))))
     (remove-duplicates rep)))



  

;========================================
;Loading classes from disk

(defun test-class (x y)
  (string-equal y (car x)))

(defun test-class1 (x y)
  (string-equal x (car y)))

(defun sort-class-name-list (list visited)
   "Sort a list of classes, where a class precedes another one if it is a superclass"
   (if (null list) nil
       (let (rep)
         (loop for item in list do
               (let* ((superclasses (mapcar #'(lambda (x) (string x)) (second item)))
                      (allowed t))
                 (loop for super in superclasses
                       while allowed do
                       (unless (or (member super visited :test 'string-equal)
                                   (not (member super list :test 'test-class1)))
                         (setf allowed nil)))
                 (when allowed
                   (push (first item) rep)
                   (push (first item) visited))))
         (setf rep (reverse rep))
         (if (null rep)
           (error "gravisimo error organizando las clases")
           (concatenate 'list rep (sort-class-name-list (set-difference list rep :test 'test-class) visited))))))

(defun eval-initial-classes (list)
   "This function is called in order to load the user-package's classes or when you import a package."
   (let* ((list-to-sort (loop for item in list
                              collect (list (second item) (eval (fourth item)))))
          (ordered-list (sort-class-name-list list-to-sort nil)) badclasses)
     (loop for item in ordered-list do
           (let ((classdef (catch 'omclass-def-error 
                             (handler-bind ((error #'(lambda (err) 
                                                       (om-message-dialog (format nil "The class ~A could not be defined because of the following error: ~s" 
                                                                                  item (om-report-condition err)))
                                                       (throw 'omclass-def-error nil))))
                            (eval (find-if  #'(lambda (x) (string-equal (second x) item)) list))))))
             (unless classdef (push item badclasses))))
     badclasses))

(defun detect-class-redefinition (list)
   "This function is used to detect if a class exists already when you import a package."
   (let (Rep)
     (loop for item in list do
           (when (exist-class-p (second item))
             (push (second item) rep)))
     rep))



               