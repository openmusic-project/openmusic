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
;This file defines the meta-object Package. Packages are collections of Classes and Generic Functions.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;-----------------
;OM Package class
;-----------------
#|
(defclass OMpackage (OMPersistantFolder) 
   ((subpackages :initform nil :accessor subpackages)
    (classes  :initform nil :accessor classes)
    (classeswin  :initform nil :accessor classeswin)
    (aliasclasses  :initform nil :accessor aliasclasses)
    (father :initform nil :initarg :father :accessor father)
    (functions  :initform nil :accessor functions)
    (icon-resources  :initform nil :accessor icon-resources))
   (:documentation "This is the class of the OMPackage metaobject.
OMPackages are collections of classes and generic functions.#enddoc#
#seealso# (ombasicobject omclass omgenericfunction omlib) #seealso#
#subpackages# A list of sub-packages. #subpackages#
#classes# The OM classes in the package. #classes#
#classeswin# A list of box classes. Box classes allow a hierarchical representation and inheritance edition.#classeswin#
#aliasclasses# Alias classes refer classes in other package, they are used for inheritance.#aliasclasses#
#father# The package containing the instance of OMpackage.#father#
#functions# The OM generic functions in the package.#functions#
#icon-resources# User package and libraries store their icon resources in this slot.#icon-resources#"))
|#


;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------

(defmethod get-elements ((self OMPackage))
   "Return only the classes and sub-packages in 'self'."
   (concatenate 'list (classes self) (subpackages self)))

(defmethod get-browser-elements ((self t))
   "Return the elements to be displayed inbrowser windows"
   (get-elements self))

(defmethod get-browser-elements ((self OMPackage))
   "Return the elements to be displayed inbrowser windows"
   (subpackages self))

(defmethod elements-with-path ((self OMPackage))
   "Return only the classes methods and sub-packages in 'self'."
   (concatenate 'list (classes self) (direct-methods self) (subpackages self)))

(defmethod get-class-icon ((self OMPackage)) 'package-icon-frame)

(defmethod get-object-insp-name ((self OMPackage)) "Package")

(defmethod really-add ((self OMPackage) (elem OMPackage))
  ;(print (list self elem))
  (pushr elem (subpackages self))
  (setf (father elem) self))

(defmethod really-add ((self OMPackage) (elem OMClass))
   (let ((classbox (OMNG-make-new-boxclass (class-name elem) (get-icon-pos elem))))
     (push classbox (classeswin self))
     (pushr elem (classes self))
     (correct-connection classbox (get-class+alias self) self)
     classbox))

(defmethod really-add ((self OMPackage) (elem OMMethod))
   (if (protected-p (fdefinition (method-name elem)))
     (om-beep-msg (format nil "The method ~D is not in the user package because it
belongs to a protected generic function." (name elem)))
     (unless (find (fdefinition (method-name elem)) (functions self) :test 'equal)
       (pushr (fdefinition (method-name elem)) (functions self)))))


(defmethod set-path-element ((self OMPackage) (elem OMClass))
   (unless (protected-p self)
     (let ((path (om-make-pathname :directory (corrige-pack-path self) :name (name elem) :type "omc")))
       (unless (mypathname elem)
         (omng-save elem (mypathname self)))
       (setf (mypathname elem) path))))


(defmethod set-path-element ((self OMPackage) (elem OMMethod))
   (unless (protected-p self)
     (let ((path (om-make-pathname :directory (corrige-pack-path self) :name (method-name-ID elem) :type "ome")))
       (unless (mypathname elem)
         (omng-save elem (mypathname self)))
       (setf (mypathname elem) path)))
   elem)

(defmethod omNG-add-element ((self OMPackage) (elem OMPackage))
   (if (add-subpackages-p self) (call-next-method)
       (really-add self elem)))

(defmethod omNG-add-element ((self OMPackage) (elem OMLispFun))
   "Add the OM Lisp function 'elem' to the package 'self'."
   (pushr elem (functions self))) 

(defmethod omNG-add-element ((self OMPackage) (elem OMGenericFunction))
   "Add the OM generic function 'elem' to the package 'self'."
   (pushr elem (functions self)))

(defmethod omNG-add-element ((self OMPackage) (elem OMBoxClass))
   "Add the box class 'elem' to the package 'self'."
   (push elem (classeswin self)))

(defmethod omNG-add-element ((self OMPackage) (elem OMBoxAlias))
   "Add the box class alias 'elem' to the package 'self'."
   (push elem (aliasclasses self)))

(defmethod omNG-add-element ((self OMPackage) (elem OMClass))
   (call-next-method))
;; next = ompersistantfolder ompersistantobject

;------------------------------------
(defmethod really-change-container ((self OMPackage) oldcont newcont)
  (print (list self (subpackages oldcont)))
  (setf (subpackages oldcont) (remove self (subpackages oldcont)))
  (print (list self (subpackages oldcont)))
  (push self (subpackages newcont)))

(defmethod omNG-change-container ((self OMPackage) oldcont newcont)
  (call-next-method)
  (setf (father self) newcont))


;-----------------------------------

(defmethod omng-remove-element ((self OMPackage) (elem OMBoxAlias))
   "Remove the box class alias 'elem' from the package 'self'."
   (setf (aliasclasses self) (remove elem (aliasclasses self) :test 'equal)))

(defmethod really-remove ((self OMPackage)  (elem OMPackage))
   (setf (subpackages self) (remove elem (subpackages self) :test 'equal))
   (when (equal elem *package-user*)
       (setf *package-user* (make-instance 'OMPackage :name "User" :icon 22))
       (om-close-window (window (EditorFrame self)))
       ;;;(omng-add-element *om-package-tree* *package-user*)
       (addPackage2Pack *package-user* *om-package-tree* :protect nil)
        ))

;Remove the class 'elem' from the package 'self'.
(defmethod really-remove ((self OMPackage)  (elem OMClass))
   (let ((pack (get-real-container elem)))
     (when pack
       (setf (classes pack) (remove elem (classes pack) :test 'equal))
       (setf *OM-class-list* (remove elem *OM-class-list*))
       (classes pack))))


;Remove the generic function 'elem' from the package 'self'.
(defmethod really-remove ((self OMPackage) (elem OMGenericFunction))
   (delete-all-functions *package-user* elem)
   (make-remove-function elem))

(defmethod really-remove ((self OMPackage) (elem OMMethod)) t)


(defmethod omng-remove-element ((self OMPackage) (elem OMboxClass))
   "Remove the box class 'elem' from the package 'self'."
   (let ((class (find-class (reference elem) nil)))
     (setf (attached-objs class) (remove elem (attached-objs class)))
     (setf (classeswin self) (remove elem (classeswin self) :test 'equal))
     (omng-remove-element self class)
     ;(setf (classes self) (remove class (classes self) :test 'equal))
     (really-remove self class)))


(defmethod get-editor-class ((self OMPackage)) 'PackageEditor)

;;; never called anymore (en principe..)
(defmethod OpenEditorframe ((self OMPackage))
   "Open a browser showing classes and subpackages of 'self'."
   ;;; new if class window open, close it!
   (when (and (editorframe self) (equal (class-name (class-of (EditorFrame self))) 'classTreePanel))
       (om-close-window (window (EditorFrame self))))
   (or (editorframe self) 
       (panel (open-new-nonrelationFrame self (name self) (get-elements self)))))

(defmethod get-win-size ((self OMPackage)) 
  (or nil ; (third (wsparams self))
      (om-make-point 250 (min (max 120 (+ 80 (* (+ (length (classes self)) (length (subpackages self))) 28))) 600))))

;--------------------------------------------------
;Other methods
;--------------------------------------------------

;Used to prevent to put a folder in self.
(defmethod ancestor-p ((self OMPackage) (container OMPackage))
   (if (eq self container) t
       (let ((list (subpackages self))
             (rep nil))
         (loop while list do
               (when (ancestor-p (pop list) container)
                 (setf rep t)
                 (setf list nil)))
         rep)))

(defmethod corrige-pack-path ((self OMPackage))
  "Set a pathname to 'self' if it has not one." 
  (unless (mypathname self)
    (let ((fatherpath (corrige-pack-path (father self))))
    (setf (mypathname self)
          (om-make-pathname :device fatherpath  :directory (append (pathname-directory fatherpath)
                                                                   (list (name self)))))))
  (mypathname self))

(defmethod direct-pack-methods ((self OMPackage))
   "Return a list of the graphic methods in 'self'."
     (loop for item in (functions self) 
           append (loop for met in (get-elements item) 
                        when (not (protected-p met)) collect met)))

(defmethod delete-all-functions ((self OMPackage) fun)
   (setf (functions self) (remove fun (functions self) :test 'equal))
   (loop for item in (subpackages self) do
         (delete-all-functions item fun)))

(defmethod get-class+alias ((self OMPackage))
   "Get all boxes in the hierarchical tree of the package 'self'."
   (concatenate 'list (classeswin self) (aliasclasses self)))

(defmethod get-functions ((self OMPackage))
   "Get functions in 'self' + functions in its sub-packages."
  (let (rep)
    (mapc #'(lambda (elem)
              (setf rep (list+ rep (get-functions elem)))) 
          (subpackages self))
    (list+ rep (functions self))))

(defmethod get-classes ((self OMPackage))
   "Get classes in 'self' + classes in its sub-packages."
   (let (rep)
     (mapc #'(lambda (elem)
               (setf rep (list+ rep (get-classes elem)))) 
           (subpackages self))
     (list+ rep (classes self))))

(defmethod direct-methods ((self OMPackage))
   "Get a list of all methods contained in 'self' and its sub-packages."
   (loop for item in (functions self) 
         append (loop for met in (get-elements item) 
                      when (not (protected-p met)) collect met)))

(defmethod get-methods ((self OMPackage))
   "Get a list of all methods contained in 'self' and its sub-packages."
   (loop for item in (get-functions self) 
         append (loop for met in (get-elements item) 
                      when (not (protected-p met)) collect met)))


(defmethod get-subpackages ((self OMPackage))
   "Get classes in its sub-packages"
   (let (rep)
     (mapc #'(lambda (elem)
               (setf rep (list+ rep (subpackages elem)))) 
           (subpackages self))
     (list+ rep (subpackages self))))

(defmethod find-subpackage ((self OMPackage) name)
   "Find the subpackage <name> in the package <OMPackage>"
  (let ((packs (get-subpackages self)))
     (find-if #'(lambda (x) (string-equal name (name x))) packs)))


;USER can add sub-packages only in the User's package.
(defmethod add-subpackages-p ((self t)) nil)

(defmethod add-subpackages-p ((self OMPackage))
   (declare (special *package-user*))
   (or (equal self *package-user*) (add-subpackages-p (father self))))

(defmethod depending-p ((self OMPackage))
   "Test if there are classes or function used in other package the 'self' and its sub-package."
  (let ((classes (get-classes self))
        (rep nil))
    (loop for item in classes
          while (not rep) do
          (let ((superclass-item (remove-if #'(lambda (class) 
                                                (not (omclass-p  (class-of class))))
                                            (class-direct-superclasses item))))
            (loop for super in superclass-item
                  while (not rep) do
                  (when (and (not (kernel-p super))
                             (not (member super classes :test 'equal)))
                    (setf rep (string+ "class " (string (name item)) " inherite from class "
                                       (string (name super))))))))
    (when (not rep)
      (let ((genfuns (get-functions self)))
        (loop for item in genfuns
              while (not rep) do
              (let ((methods (get-elements item)))
                (loop for met in methods
                      while (not rep) do
                      (let ((quali (method-specializers met)))
                        (loop for qua in quali
                              while (not rep) do
                              (when (and (omclass-p  (class-of qua))
                                         (not (kernel-p qua))
                                         (not (member qua classes :test 'equal)))
                                (setf rep (string+ "A method of the generic function " (string (name item))
                                                   " is specialized by the class "
                                                   (string (name qua))))))))))))
    rep))


(defmethod funfromkernel ((self OMPackage) fun)
   "Add the generic function 'fun' to 'self' at the first method definition."
   (unless (member fun (get-functions self) :test 'equal)
     (omng-add-element self fun)))


(defmethod def-icon-var ((self OMPackage) path)
  "Set the icon-resources slot of 'self' with the 'cicn' resources in the resources fork of 'path'."
  (let* ((iconlist (om-get-all-icons-id path)) 
        (reslist nil))
    (loop for id in iconlist do 
          (push (list id (om-load-icon id path)) reslist))
    (setf (icon-resources self) reslist)))


;This function is called when you drag the user package to the library package
;This function is not enough tested, but it works....
;;; ajouter pour les resources !!!
(defmethod package2userLib ((self OMPackage))
   "Save a package as a Librarie."
  (let ((continue-p  (om-create-directory (om-make-pathname :device *om-lib-dir*
                                                            :directory (append (pathname-directory *om-lib-dir*)
                                                                               (list (name self))))
                                       :if-exists nil)))
    (if (not continue-p) 
        (om-beep-msg "This lib name already exists, change the package name")
      (om-with-cursor *om-wait-cursor*
        (let* ((elements (mapcar #'(lambda (elem) (omNG-save-ws elem)) (subpackages self)))
               (classes (get-classes self))
               (code (omNG-save-packlist self))
               (method-list (get-methods self))
               (list-to-sort (loop for item in classes
                                   collect (list (name item) (make-super-class-list item))))
               (ordered-list (sort-class-name-list  list-to-sort nil)))
          (setf elements (remove-if 'null elements))
          (om-create-directory (om-make-pathname :device *om-lib-dir*  :directory (append (pathname-directory *om-lib-dir*)
                                                                         (list (name self) "patches"))))
          (om-create-directory (om-make-pathname :device *om-lib-dir*   :directory (append (pathname-directory *om-lib-dir*)
                                                                         (list (name self) "sources"))))
          (WITH-OPEN-FILE (out (om-make-pathname :device *om-lib-dir*   :directory (append (pathname-directory *om-lib-dir*)
                                                                 (list (name self)))
                                              
                                              :name (string+ (name self) ".lib"))
                               :direction :output ) 
            (prin1 '(in-package :om) out)
            (prin1 `(load-om ,(string+ (car (last (pathname-directory *om-lib-dir*))) ";" (name self) ";sources;" (name self)) out)))
          (WITH-OPEN-FILE (out  (om-make-pathname :device *om-lib-dir*   :directory (pathname-directory
                                                          (OMRoot (string+ (car (last (pathname-directory *om-lib-dir*))) ";" (name self) ";sources;")))
                                              :name (string+ (name self) ".lisp"))
                               :direction :output ) 
            (prin1 '(in-package :om) out)
            (prin1 `(let* ((classes ',(mapcar #'(lambda (elem) (om-save-class (find-class (interne elem)))) ordered-list))
                           (methods ',(loop for met in method-list  collect (om-save-methods met)))
                           (subpack ',code)
                           (init-class-met ',(save-initial-class-methods classes)) badclasses smethods)
                      (setf badclasses (eval-initial-classes classes))
                      (setf smethods (mapcar #'(lambda (elem) (eval elem)) methods)) 
                      (mapc #'(lambda (elem) (when elem (define-really-method elem))) smethods)
                      (initial-methods-for-classes init-class-met badclasses)
                      (add-new-packages (eval subpack) *current-lib* badclasses 21)) out))
          t)))))

(defmethod omNG-save ((self OMPackage) &optional (thepath nil))
   "Save the package 'self' its subpackages classes and methods."
   (unless thepath (setf thepath (corrige-pack-path self)))
   (unless (probe-file thepath) (om-create-file thepath))
   (loop for class in (classes self) do
         (omNG-save class thepath))
   (loop for met in (direct-pack-methods self) do
         (omNG-save met thepath))
   (loop for pack in (subpackages self) do
         (omNG-save pack (mypathname pack))))





;--------------------------------------------------
;Tools
;--------------------------------------------------


;-------Builder
(defun omNG-make-new-package (name &key doc icon)
   "Make an instance of the OMPackage class."
   (let ((pack (make-instance 'OMPackage :name name :icon (or icon 20))))
     (when doc (setf (doc pack) doc))
     pack))

;----------------------------------
;Initialize the om-package-tree 
;----------------------------------

(defvar *package-user* nil "The USER package contains user-defined classes and generic functions.")

(defvar *library-package* nil)
(setf *library-package* (omng-protect-object (omNG-make-new-package "Libraries" :icon 21
                                                                    :doc "Dynamic loaded libraries")))

(defvar *om-library-package* nil)
(setf *om-library-package* (omng-protect-object (omNG-make-new-package "OM Libraries" :icon 21
                                                                    :doc "Dynamic loaded OM libraries")))

(defvar *external-library-package* nil)
(setf *external-library-package* (omng-protect-object (omNG-make-new-package "External Libraries" :icon 21
                                                                    :doc "Dynamic loaded extra libraries")))

(defvar *om-package-tree* nil "Root of the OM and User packages.")
(setf *om-package-tree* (omNG-protect-object 
                         (omNG-make-new-package "OM Packages Library" :icon 204 
                                                :doc "Root of the OM and User packages.")))


;Init the package root
(defun init-om-package ()
  (declare (special *library-package*))
  (init-basic-lisp-types)
  (setf (icon-resources *om-package-tree*) (load-default-icon-list)) 
  (AddPackage2Pack *library-package* *om-package-tree*)
  (sort-package-tree)
  (setf (wsparams *om-package-tree*) 
        (list (om-make-point 24 200) (om-make-point 50 50) (om-make-point 200 400))))

(defun sort-package-tree ()
   (let ((ordre '( "Audio" "Midi" "MathsTools" "Score" "Basic Tools" "Kernel"))
         (list (subpackages *om-package-tree*))
         rep)
     (loop for item in ordre do
           (let ((pack (find item list :key 'name :test 'string-equal)))
             (when pack
               (push pack rep)
               (setf list (remove pack list :test 'equal)))))
     (setf (subpackages *om-package-tree*) (append rep list))))
           
(defun init-user-pathname ()
   (unless (mypathname *package-user*)
     (setf (mypathname *package-user*) (om-make-pathname :device (mypathname *current-workSpace*) 
                                                         :directory (append (pathname-directory (mypathname *current-workSpace*))
                                                                            (list "user"))))))


; (setf *current-lib* (exist-lib-p "OMChaos"))
;Cons a package tree, with pack as root, from a list of subpackage names, class names and function names.
(defun add-new-packages  (package pack &optional (badclasses nil) (iconID 22) (protect? nil))
  (let ((new-pack (if (first package) 
                      (or (find (first package) (subpackages pack) :key 'name :test 'string-equal)
                          (make-instance 'OMPackage :name (first package) :icon iconID))
                    pack
                    )))
    ;; CLASS ALIASES
    (mapc #'(lambda (name)
              (when (and (find-class (second name) nil) (not (member (string (second name)) badclasses :test 'string-equal)))  
                (let* ((classbox (omNG-make-new-boxclass (second name) (third name)))
                       (newalias (omNG-make-new-boxalias classbox (third name) (first name))))
                  ;;;(omNG-add-element new-pack newalias)
                  ;;; !!! new
                  (addClass2Pack newalias new-pack :protect nil) 
                  )))
               (fifth package))
    ;;; CLASSES
    (mapc #'(lambda (name) 
              (when (and (find-class name nil) (not (member (string name) badclasses :test 'string-equal))
                         (not (member (string name) (classes new-pack) :test 'string-equal :key 'name)))
                (really-add new-pack (find-class name)))) (third package))
    ;;; FUNCTIONS
    (mapc #'(lambda (name) 
              (when (fboundp name)
                (if (OMGenfun-p (fdefinition name))
                    ;;;(omNG-add-element new-pack (fdefinition name))
                    ;;; !!! new
                    (addGenFun2Pack name new-pack :protect nil)
                  ;;;(omNG-add-element new-pack (omNG-make-new-lispfun name))
                  (addLispFun2Pack name new-pack :protect nil)
                  ))) (fourth package))
    ;;; SUBPACKAGES
    (mapc #'(lambda (pk) 
              (add-new-packages pk new-pack badclasses iconID protect?)) (second package))
    ;;; THIS PACK
    (when (and (first package) 
               (not (find (first package) (subpackages pack) :key 'name :test 'string-equal)))
      (really-add pack new-pack)
      (when protect?
        (omng-protect-object new-pack)))
    new-pack))


;;; -------------------------------
(defun export-symbol-from-om (symb pack)
  (unless (or *current-lib* 
              (ancestor-p *library-package* pack)
              (ancestor-p *package-user* pack))
    (export symb :om)))


(defmethod AddPackage2Pack ((new-Package OMPackage) inPackage &key (protect t))
  (let ((subpackages (subpackages inPackage)))
    (unless (member (name new-Package) subpackages :test 'string-equal :key 'name)
      (when protect (omNG-protect-object new-package))
      (omNG-add-element inPackage new-package)
      new-package)))

(defmethod AddPackage2Pack ((name string) inPackage &key (protect t))
  (let* ((subpackages (loop for item in (subpackages inPackage) collect (name item))) 
         new-package)
    (unless (member name subpackages :test 'string-equal)
      (setf new-package (omNG-make-new-package name))
      (when protect (setf new-package (omNG-protect-object new-package)))
      (omNG-add-element inPackage  new-package)
      new-package)))


;--------


(defmethod AddClass2Pack ((classname symbol) inPackage &key (protect t) (position (om-make-point 50 50)))
  (let* ((classes (loop for item in (classes inPackage) collect (name item)))) 
    (unless (member (string classname) classes :test 'string-equal)
      (export-symbol-from-om classname inPackage)
      (set-icon-pos (find-class classname) position)
      (omNG-add-element inPackage 
                        (if protect 
                            (omNG-protect-object (find-class classname))
                          (find-class classname)))
      )))

(defmethod AddClass2Pack ((classname list) inPackage &key (protect t) (position (om-make-point 50 50)))
   (when position (setf position (list! position)))
   (mapc #'(lambda (fun)
             (AddClass2Pack fun inPackage :protect protect :position (or (pop position) (om-make-point 50 50)))) classname))






;-----
(defmethod AddGenFun2Pack ((funName symbol) inPackage &key (protect t))
  (let* ((funs (loop for item in (functions inPackage) collect (name item)))) 
    (unless (member (string funname) funs :test 'string-equal)
      (export-symbol-from-om funName inPackage)
      (omNG-add-element
       inPackage 
       (if protect 
           (omNG-protect-object  (fdefinition funName))
         (fdefinition funName))))))

(defmethod AddGenFun2Pack ((funName list) inPackage &key (protect t))
   (mapc #'(lambda (fun)
             (AddGenFun2Pack fun inPackage :protect protect)) funname))


;-----

(defmethod AddLispFun2Pack ((funName symbol) inPackage &key (protect t))
  (let* ((funs (loop for item in (functions inPackage) collect (name item)))) 
    (unless (member funname funs :test 'string-equal)
      (omNG-add-element
       inPackage 
       (if protect 
           (omNG-protect-object  (omNG-make-new-lispfun funName))
         (omNG-make-new-lispfun funName))))))

(defmethod AddLispFun2Pack ((funName list) inPackage &key (protect t))
   (mapc #'(lambda (fun)
             (AddLispFun2Pack fun inPackage :protect protect)) funname))

;;-----

;The icon ID in user packages and libraries is store as a list (iconID package)
(defun icon-for-user-package (obj second)
   (setf (icon obj) (list (icon obj) second)))


