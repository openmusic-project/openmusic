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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson. Karim Haddad
;=========================================================================

;DocFile
;This file implementss the meta-object OMFolder.
;Folder are not computable objects in OM.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;-----------------
;The Folder Class in OM
;-----------------
#|
(defclass OMFolder (OMPersistantFolder) 
   ((elements :initform nil :initarg :elements :accessor elements))
   (:documentation "The class of the OM folders. #enddoc#
#seealso# (OMPersistantObject OMWorkspace) #seealso#
#elements# A list of objects contained in the Folder i.e. patches, maquettes or anothers folders. #elements#"))
|#

(defmethod obj-file-type ((self OMFolder)) :FOLD)

(defmethod format-folder-size ((self omfolder))
  (let* ((size (om-view-size (editorframe self)))
         (x (om-point-x size))
         (y (om-point-y size)))
    (format nil "(om-make-point ~D ~D)" x y)))
      
(defmethod header-comment-from-obj ((self OMFolder))
  (let* ((icon (icon self))
         (ftype (obj-file-type self))
         (version  *om-version*)
         (wspar (om-save-point-list (ensure-ws-params self)))
         (size (if (editorframe self) 
                   (read-from-string (format-folder-size self))
                 (third wspar)))
         (params (list version ftype (first wspar) (second wspar) 
                       size
                       (str-without-nl (doc self)) (save-icon icon) (presentation self)
                       (car (create-info self)) nil)))
    (string+ ";" (format nil " ~S" params))))

;--------------------------------------------------
;Method redefinition of OMpersistantObject
;--------------------------------------------------

(defmethod get-elements ((self OMFolder))
   "Folder's elements are patches, maquettes or another folders."
   (elements self))

(defmethod get-class-icon ((self OMfolder)) 'folder-icon-frame)

(defmethod get-object-insp-name ((self OMFolder)) "folder")

(defmethod get-editor-class ((self OMFolder)) 'FolderEditor)

(defmethod OpenEditorframe ((self OMFolder)) 
   "Show the folder as a container."
   (or (editorframe self) 
       (panel (open-new-folder self (name self) (get-elements self)))
       ))

(defun open-new-folder (object name elements)
     (let* ((i 0) newwindow)
       (setf newwindow (make-editor-window (get-editor-class object)
                                           object name nil 
                                           :winsize `,(eval (fifth (get-finder-comment (mypathname object)))) ;(get-win-size object)
                                           :winpos (get-win-position object)
                                           :winshow nil
                                           :wintype (wintype-from-obj object)))
       (set-editor-presentation (editor newwindow))
       (om-with-delayed-redraw (panel newwindow)
         (mapc #'(lambda (elem)
                  (add-icon-finder (make-icon-from-object elem  
                                                           (om-point-h (get-icon-pos elem)) (om-point-v (get-icon-pos elem)) 
                                                           1 (+ i 1))
                                    (panel newwindow))
                   (incf i)) (sort-subframes (panel newwindow) elements))
         (set-field-size (panel newwindow)))
       (setf (changed-wsparams? object) nil)
       newwindow
       ))


;--------------------------------------------------
;Other Methods
;--------------------------------------------------
(defmethod folder-p ((self OMFolder)) t)
(defmethod folder-p ((self t)) nil)

(defmethod save-all-persistants ((self OMFolder))
   "Save a folder and its elements at the end of the OM session."
   (loop for item in (elements self) do
         (save-all-persistants item)))

(defmethod om-paste ((self t) (elem OMFolder))
   (push elem (elements self))
   (let ((path (make-pathname :directory (append (pathname-directory (elements-pathname self)) (list (name elem)))))
         (old-elemts (elements elem)))
     (setf (mypathname elem) path)
     (om-create-directory path)
     (setf (elements elem) nil)
     (mapc #'(lambda (el) (om-paste elem el)) old-elemts)))


;--------------------------------------------------
;Tools
;--------------------------------------------------
(defun omNG-make-new-folder (name &optional (posi (om-make-point 0 0)))
   (let ((rep (make-instance 'OMFolder :name name  :icon 186 :presentation *default-folder-pres*)))
     (setf (wsparams rep) (list (om-make-point 24 24) (om-make-point 50 50) (om-make-point 250 320)))
     (set-icon-pos rep posi)
     (setf (changed-wsparams? rep) t)
     rep))


;Used to prevent to put a folder in self.
(defmethod ancestor-p ((self t) (container t)) nil)

(defmethod ancestor-p ((self OMFolder) (container OMFolder))
   (cond
    ((eq self container) t)
    (t (let ((list (get-elements self))
             (rep nil))
         (loop while list do
               (when (ancestor-p (pop list) container)
                 (setf rep t)
                 (setf list nil)))
         rep))))

;;=========THIS IS THE SPECIAL FOLDER WITH global vars===================

(defvar *om-globalsfolder* nil "The globals folder in the workspace.")

(defclass OMglobalsFolder (OMFolder OMPackage) ()
   (:documentation "This is the special folder which contains globals variables. #enddoc#
#seealso# (OMFolder OMinstance) #seealso#"))

;--------------------------------------------------
;INTERFACE
;--------------------------------------------------

(defmethod get-documentation ((self OMglobalsFolder)) 
  "A special package used to define global instances")

(defmethod get-elements ((self OMglobalsFolder))
   "Global folder's elements are OMinstances." (elements self))

(defmethod get-class-icon ((self OMglobalsFolder)) 'globals-folder-icon)

(defmethod get-editor-class ((self OMglobalsFolder)) 'GlobalsFolderEditor)

(defmethod OpenEditorframe ((self OMglobalsFolder))
   "Show the globals folder 'self' as a container."
   (or (editorframe self)
       (panel (open-new-nonrelationFrame self "Global Variables" (get-elements self)))))

;;; we need to precise this because the behaviour of OMFolder and OMPackage is different!
(defmethod really-add ((self OMglobalsFolder)  (elem t))
  (push elem (elements self)))

;--------------------------------------------------
;Tools
;--------------------------------------------------
(defmethod mus-patch ((self t)) nil)


(defun init-globalsfolder ()
   "Make an instance of global folder and store it in the *om-globalsfolder* variable." 
   (setf *om-globalsfolder* (omNG-protect-object (make-instance 'OMglobalsFolder
                                                   :Name "globals"
                                                   :icon 23
                                                   :elements (getglobalslist)))))


(defun getglobalslist ()
  "Called when you start OM, this function load all globals variables saved in the current workspace."
  (declare (special *current-workspace* *instance-to-load*))
  (let ((directories (om-directory (make-pathname :directory  (append (pathname-directory (mypathname *current-workSpace*))
                                                                      (list "globals"))) 
                                   :directories nil :files t)))
    (loop for path in directories  
          when 
          (and  (om-persistant-p path)
                (equal (file-type path) :INST))
          collect (let (newobj wspar)
                    (setf wspar (subseq (get-init-wsparams path) 2 5))
                    (setf *loaading-stack* nil)
                    (eval-non-text-file path)
                    (setf newobj *instance-to-load*)
                    (when newobj
                      (setf (mypathname newobj) path)
                      (setf *instance-to-load* nil)
                      (update-patches-pile)
                          ;(when (mus-patch (instance newobj))
                          ;  (compat-get-old-picts (mypathname newobj) (mus-patch (instance newobj))))
                      (setf (loaded? newobj) t)
                      (setf (saved? newobj) t)
                      (setf (wsparams newobj) (loop for item in wspar collect (eval item)))
                      newobj)))))






