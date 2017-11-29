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
;This file defines the meta-object Library. Libraries are OMpackages load dinamicly.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


(export '(*current-lib*
          doc-library
          fill-library
          require-library
          find-library
          exist-lib-p
          add-lib-alias
          lib-true-name
          
          defclass!
          defgeneric!
          defmethod!) :om)


(defvar *current-lib* nil "This variable containts the current library that is being loaded.")

;--------------------
;The Lib Class in OM
;--------------------

#|
(defclass OMLib (OMpackage) 
   ((lib-pathname :initform nil :initarg :lib-pathname :accessor lib-pathname)
    (loaded? :initform nil  :accessor loaded?))
   (:documentation "The class for the OM Library  metaobjects.
One OMlib is a collection of classes and generic functions loaded dinamiclly.#enddoc#
#seealso# (ombasicobject ompackage) #seealso#
#lib-pathname# The pathname of the librarie folder. #lib-pathname#
#loaded?# T is the LIbrary is already loaded. #loaded?#"))
|#

;;; called in the lib deliver script 
;;; why both release and version ???
(defmethod lib-release (&optional lib) 
  ;(release (or lib *current-lib*))
  (version (or lib *current-lib*))
  )

(defmethod set-lib-release (version &optional lib) 
  (let ((thelib (or lib *current-lib*)))
    ;(setf (release thelib) version)
    (setf (version thelib) version)
    ))


;--------------------------------------------------
;Method redefinition of OMPackage
;--------------------------------------------------

(defmethod get-object-insp-name ((self OMLib)) "Library")

(defmethod get-class-icon ((self OMLib)) 'lib-icon-frame)

;--------------------------------------------------
;Other methods
;--------------------------------------------------

;;; returns the folder of the library's icons
(defmethod lib-icons-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (lib-pathname self)) (list "resources" "icon"))
                     :host (pathname-host (lib-pathname self))
                     :device (pathname-device (lib-pathname self))))

(defmethod lib-resources-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (lib-pathname self)) (list "resources"))
                     :host (pathname-host (lib-pathname self))
                     :device (pathname-device (lib-pathname self))))


;;; LOADS AN OM LIB IF NOT LOADED
;;; LOADS LIB PICTURES
;;; SETS *current-lib*
(defmethod load-om-lib ((self OMLib) &optional (winmessage t))
  "Load the librarie 'self'."
 (handler-bind ((error #'(lambda (c)
                           (hide-message-win)
                           (when *msg-error-label-on*
                             (om-message-dialog (format nil "Error while loading the library ~A:~%~s" 
                                                         (name self) (om-report-condition c))
                                                :size (om-make-point 300 200))
                            (om-abort)))))
    (unless (loaded? self)
      (if (probe-file (lib-pathname self))
          (om-with-cursor *om-wait-cursor* 
            (let ((*current-lib* self))
              (def-icon-var self (lib-icons-folder self))
              (show-message-win (string+ "Loading library " (name self) (if (version self) (string+ " - v. " (number-to-string (version self)))  "")))
              (load (lib-pathname self))
              (load-lib-pictures self)
              (hide-message-win)
              (setf (loaded? self) t)
              ))
        (om-message-dialog (format nil "Error while loading the library ~A:~%The init file ~s does not exist." 
                                   (name self) (namestring (lib-pathname self)))
                           :size (om-make-point 300 200))
        ))))


;;; LOADS ALL REGISTERED LIBRARIES
;;; GENERATES THE DOCUMENTATION
(defun load-all-om-libs ()
  (mapcar #'(lambda (lib) 
              (load-om-lib lib)
              (gen-lib-reference lib))
          (subpackages *library-package*)))
          
;; (load-all-om-libs)

(defmethod load-om-lib-without-menu ((self OMLib))
  (unless (loaded? self)
    (om-with-cursor *om-wait-cursor* 
      (let ((*current-lib* self))
        (def-icon-var self (lib-icons-folder self))
        (load (lib-pathname self))
        (setf (loaded? self) t)))))

(defmethod omlib-p ((self OMLib)) t)
(defmethod omlib-p ((self t)) nil)

;--------------------------------------------------
;Tools
;--------------------------------------------------

(defvar *om-lib-dir* )

(defun init-omlib-directory ()
  ; (setf *om-lib-dir* (OMroot (string+ "libraries;")))
  (setf *om-lib-dir* (merge-pathnames (make-pathname :directory '(:relative "OM" "Libraries")) (om-user-home)))
  (unless (probe-file *om-lib-dir*)
    (om-create-directory *om-lib-dir*)))

(om-add-init-func 'init-omlib-directory)

;----Builder

(defun def-lib-doc-string (lib)
  (format nil "Version: ~A.~%~% - Location: ~A"
          (or (version lib) "?")
          (namestring (om-make-pathname :directory (om-make-pathname :directory (lib-pathname lib))))))

(defun reset-lib-doc (&optional lib)
  (let ((thelib (or lib *current-lib*)))
    (when thelib 
      (setf (doc thelib) (def-lib-doc-string thelib)))))

(defun omNG-make-new-lib (name)
  "Make an instance of the class OMLib."
  (let* ((libname (if (pathnamep name)
                      (car (last (pathname-directory name)))
                    name))
         (libdir (if (pathnamep name)
                     (om-make-pathname :directory name)
                   (om-make-pathname :host (pathname-host *om-lib-dir*)
                                     :device (pathname-device *om-lib-dir*)
                                     :directory (append (pathname-directory *om-lib-dir*) (list name)))))
         (basename (string-until-space libname))
         (version (when (search " " libname) (read-from-string (string-from-space libname))))
         (lib (make-instance 'OMLib 
                             :name basename
                             :version version
                             :lib-pathname (om-make-pathname 
                                            :directory libdir
                                            :name (string-until-space libname) 
                                            :type "lisp")
                             :icon 21)))
    (reset-lib-doc lib)
    lib))


;----Method definition
;See defmethod* the differences are
;  -Set the flag lib-fun-p of the generic function to the name of the librarie
;  -If the icon is in the resource fork file of the lib store icon as (IconID OMLiB)
(defmacro defmethod! (name &rest args)
  `(let* ((funb (fboundp ',name))
          (themethod (defmethod* ,name ,.args))
          (thelib (or *current-lib*
                      (and funb (lib-fun-p (fdefinition ',name)) (find-library (lib-fun-p (fdefinition ',name)))))))
     ;;; function is in a user library and did not exist before
     (when (and thelib (not funb))
       (setf (lib-fun-p (fdefinition ',name)) (string-until-space (name *current-lib*))))
     (if (find :icon ',args)
       (if (listp (icon (fdefinition ',name)))
           (setf (icon (fdefinition ',name)) (car (icon (fdefinition ',name))))
         (when (and thelib (lib-fun-p (fdefinition ',name)))
           (setf (icon (fdefinition ',name))
                 (list (icon (fdefinition ',name)) thelib)))
         )
       )
     themethod))





 
 ;----Class definition
;See defclass* the differences are
;  -Set the flag lib-fun-p of the generic function to the name of the librarie
;  -If the icon is in the resource fork file of the lib store icon as (IconID OMLiB)
(defmacro defclass! (name superclass slots &rest class-options)
   `(let ((newclass (defclass* ,name ,superclass ,slots ,.class-options)))
      (when  (and *current-lib* t) ;;; (not (lib-class-p (find-class ',name))))
        (if (listp (icon (find-class ',name)))
          (setf (icon (find-class ',name)) (car (icon (find-class ',name))))
          (setf (icon (find-class ',name)) 
                (list (icon (find-class ',name)) *current-lib*)))
        (setf (lib-class-p (find-class ',name)) (string-until-space (name *current-lib*))))
      newclass))

;----Generic function definition
;   See defgeneric* the differences are
;  -Set the flag lib-fun-p of the generic function to the name of the librarie
;  -If the icon is in the resource fork file of the lib store icon as (IconID OMLiB)
(defmacro defgeneric! (name lambda-list &rest options-and-methods &environment env)
   `(let ((thegenfun (defgeneric* ,name ,lambda-list ,.options-and-methods)))
      (when (and *current-lib* (not (lib-fun-p (fdefinition ',name))))
        (if (listp (icon (fdefinition ',name)))
          (setf (icon (fdefinition ',name)) (car (icon (fdefinition ',name))))
          (setf (icon (fdefinition ',name)) 
                (list (icon (fdefinition ',name)) *current-lib*)))
        (setf (lib-fun-p (fdefinition ',name)) (string-until-space (name *current-lib*))))
      themethod))

 
;Used when you have two or more more libraries with the same name, but different version.
#|
(defun choise-a-lib (Libs name)
  (let* ((dialog (om-make-window 'om-dialog
                                 :position :centered
                                 :size (om-make-point 230 185)
                                 :window-title "Multiple versions of lib"
                                 :font *om-default-font1*
                                 :close nil
                                 :bg-color (om-make-color 0.874 0.874 0.874)))
         (liblist (om-make-dialog-item 'om-single-item-list (om-make-point 60 50) (om-make-point 120 80) ""
                                        :range (loop for lib in LIbs collect (name-of-directory (lib-pathname LIb)))
                                        )))
    (om-set-selected-item-index liblist 0)
    (om-add-subviews dialog liblist 
                     (om-make-dialog-item 'om-static-text (om-make-point 10 15) (om-make-point 200 16) 
                                          (string+ "Choose a version of " name))
                     (om-make-dialog-item 'om-button (om-make-point 40 140) (om-make-point 80 25) "Cancel" 
                                          :di-action (om-dialog-item-act item (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog nil)))
                     (om-make-dialog-item 'om-button (om-make-point 130 140) (om-make-point 80 25) "OK" 
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (let ((selected-index (om-get-selected-item-index liblist)))
                                                         (om-return-from-modal-dialog dialog (nth selected-index Libs))))))
    (om-modal-dialog dialog)))
|#

(defun choise-a-lib (Libs name)
  (om-message-dialog
   (format nil (reduce 'string+ (append 
                                 (list "Several versions of library " name " are installed and currently visible in OM.~%Please choose one of the following and remove the other one(s) from the OM registered search paths: ~%~%")
                                 (loop for lib in libs collect 
                                       (string+ (namestring (om-make-pathname :directory (lib-pathname lib)))
                                                " ~%~%"))))))
  
  (om-abort))

;Called when you start OM if the workspace needs the libraries in the list 'lib'
(defun load-lib-for-first (list)
  (om-without-interrupts 
   (loop for lib in list do
         (unless (member lib *skip-libs* :test 'string-equal)
           (let ((thelib (exist-lib-p (lib-true-name lib))))
             (if thelib
               (load-om-lib-without-menu thelib)
               (progn
                 (dialog-message (string+ "This workspace can not be loaded because i don't find the library " lib))
                 (om-quit)
                 )))))))



(defvar *lib-aliases* nil)

(defun add-lib-alias (old new)
  (push (list old new) *lib-aliases*))

(defun lib-true-name (name)
  (let ((new (find name *lib-aliases* :test 'string-equal :key 'car)))
    (if new (cadr new) name)))



;Return the OMLib instance with name 'name' if there two or more call the choise-a-lib function.
(defun exist-lib-p (name)
  (let ((newname (lib-true-name name)) 
        (libs))
     (loop for lib in (subpackages *library-package*) do
           (when (string-equal (string-until-space (name lib)) name)
             (push lib libs)))
     (when libs
       (if (= 1 (length libs)) 
           (car libs)
         (choise-a-lib libs name)))))

(defun find-library (name)
  (exist-lib-p name))
;  (let ((newname (lib-true-name name)) 
;        (thelib nil))
;    (loop for lib in (subpackages *library-package*)
;         while (not thelib) do
;          (when (string-equal (string-until-space (name lib)) name)
;            (setf thelib lib)))
;    thelib))
    

(defvar *user-lib-dir* nil)
(defvar *user-libs* nil)


;;; REGISTERS LIBS IN OM AND USER FOLDERS
(defun load-om-libs ()
  (let ((om-elements (om-directory *om-lib-dir*
                                :files nil :directories t)))
    (mapc #'(lambda (x)
                      (AddPackage2Pack (omNG-make-new-lib (car (last (pathname-directory x)))) *library-package*))
           om-elements)
    (load-user-libs)))

(defun relocate-lib-info (lib rootpath)
  (setf (lib-pathname lib) (om-make-pathname :directory rootpath 
                                             :name (pathname-name (lib-pathname lib))
                                             :type (pathname-type (lib-pathname lib))))
  ;(setf (doc lib) (def-lib-doc-string lib))
  )

(defmethod get-documentation ((self OMLib))
  (def-lib-doc-string self))
  
(defun load-user-libs (&optional really-load)
  (let ((user-elements 
         (remove nil 
                 (loop for folder in (list! *user-lib-dir*) append
                       (when (probe-file folder)
                         (remove-if-not 
                          #'(lambda (elt)
                              (and (directoryp elt)
                                   (find (string-until-space (car (last (pathname-directory elt)))) (om-directory elt :directories nil :files t :type "lisp")
                                         :key 'pathname-name :test 'string-equal)))
                          (om-directory folder :files nil :directories t)))))))
    (setf *user-libs* nil)  
    (mapc #'(lambda (pathname)
            (let* ((newlib (omNG-make-new-lib pathname))
                   (loaded (find (name newlib) really-load :test 'string-equal :key 'car)))
              (when loaded
                (setf really-load (remove (name newlib) really-load :test 'string-equal :key 'car))
                (setf newlib (cadr loaded))
                ;; in case the lib was just moved
                (relocate-lib-info newlib pathname)
                (reload-om-lib newlib))

              
              (AddPackage2Pack newlib *library-package*)
              (push newlib *user-libs*)
              ))
        user-elements)
  *user-libs*))

(defmethod AddPackage2Pack ((new-Package OMLib) inPackage &key (protect t))
  (let ((subpackages (subpackages inPackage)))
    (if (find new-Package subpackages :test #'(lambda (lib1 lib2) 
                                                    (and (string-equal (name lib1) (name lib2))
                                                         (cond ((and (version lib1) (version lib2))
                                                                (= (version lib1) (version lib2)))
                                                               ((or (version lib1) (version lib2)) t)
                                                               (t nil)))))
        (om-beep-msg (format nil "Library ~A~A already exists!!" (name new-Package)
                             (if (version new-Package) (format nil " (~D)" (version new-Package)) "")))
      (progn
        (when protect (omNG-protect-object new-package))
        (omNG-add-element inPackage new-package)
        new-package))))


(defmethod reload-om-lib ((self OMLib))
  (unless (loaded? self)
    (def-icon-var self (lib-icons-folder self))
    (setf (loaded? self) t)))

; (reload-user-libs)

(defun reload-user-libs ()
  (let ((really-load nil))
    (setf (subpackages *library-package*) 
          (remove-if #'(lambda (elt) (when (member elt *user-libs* :test 'equal)
                                       (if (loaded? elt)
                                           (push (list (name elt) elt) really-load))
                                       t)) 
                     (subpackages *library-package*)))
    (load-user-libs really-load)))

;(mapcar 'name (subpackages *library-package*))
;(mapcar 'name *user-libs*)

(defun add-one-lib (pathname &optional (show t))
  (let* ((libname (string-until-space (car (last (pathname-directory pathname)))))
         (initfile (om-make-pathname :directory pathname
                                     :name libname
                                     :type "lisp")))
    (if (probe-file initfile)
        (AddPackage2Pack (omNG-make-new-lib pathname) *library-package*)
      (om-message-dialog (format nil "Library ~s not valid.~%The init file ~s does not exist." 
                                 libname initfile)
                         :window-title "Error!"))
    (when show (show-libs))))
    
(defun show-libs ()
  (unless (editorframe *om-package-tree*)
    (openobjecteditor *om-package-tree*))
  (loop for sub in (get-subframes (editorframe *om-package-tree*))
        do (when (equal (object sub) *library-package*)
             (when (and (triangle sub) (open? (triangle sub)))
               (open/close-triangle (triangle sub) nil))
             (unless (triangle sub) 
               (add-triangle sub (editorframe *om-package-tree*)))
             (open/close-triangle (triangle sub) t)))
  )

;Init the subpackages of the current library from a list of the form :
; ("sub-pack-name" subpack-list class-list function-list class-alias-list)
(defun fill-library (list &optional lib)
  (let ((thelib (if (omlib-p lib) lib
                  (if (stringp lib) (find-library lib)
                    *current-lib*))))
    (when thelib
      (loop for item in list do
            (add-new-packages item thelib nil 21 t)))
    ))

(defun doc-library (text &optional lib)
  (let ((thelib (if (omlib-p lib) lib
                  (if (stringp lib) (find-library lib)
                    *current-lib*))))
    (when (and text thelib)
      (setf (doc thelib) text)
      ;(when (and (stringp (doc thelib)) (not (string-equal (doc thelib) "")))
      ;  (setf (doc thelib) (string+ (doc thelib) (format nil "~%"))))
      ;(setf (doc thelib) (string+ (doc thelib) text))
      )))
    

;Mount functions, classes and sub-package of 'lib' to the menubar.
; Not used in OM 6
;(defun lib2menu (lib)
;   (let ((menufun (om-new-menu (name lib)))
;         (menuclass (if (or (subpackages lib) (classes lib)) (om-new-menu (name lib)))))
;     (when (and menufun (om-find-menu-item *patch-menu-classes* "Libraries"))
;       (om-add-menu-items (om-find-menu-item *patch-menu-functions* "Libraries") t menufun)
;       (when menuclass
;         (om-add-menu-items (om-find-menu-item *patch-menu-classes* "Libraries") t menuclass))
;      (packages2menu menufun menuclass lib))))


(defmethod require-library ((libs list) &optional (abort-if-not-found nil))
  (mapcar #'(lambda (lib) (require-library lib abort-if-not-found)) libs))

(defmethod require-library (lib &optional (abort-if-not-found nil))
  (let ((thelib (find-library lib)))
    (if thelib
        (progn 
          (unless (loaded? thelib)
            (load-om-lib thelib))
          t)
      (when abort-if-not-found
        (dialog-message (string+ "Library " lib " not found."))
        (om-abort)))))



