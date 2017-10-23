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
;This file contains the abstract classes for the system definition.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;--------------------------------------
;All object in OM System is an OMObject
;the name is the ID for all object
;--------------------------------------
#|
(defclass OMObject () 
   ((name :initform nil :initarg :name :accessor name))
   (:documentation "All object in Open Music is an OMObject.#enddoc#
#seealso# (omBasicObject omFrame) #seealso#
#name# Open Music is a label based system, so all object has a name. #name#"))
|#

;-----------------Protocole--------------------


(defmethod omNG-rename ((self OMObject) name)
   "This method changes the name of the object self with the new name NAME"
   (setf (name self) name))
;----------------------------------------------
(defmethod initialize-instance  ((self OMObject) &rest initargs &key from-file)
      (call-next-method))
  

;--------------------------------------------------------------------
;The internal basic objects of the system
;(i.e. type class genfun method instance slot patch maquette package)
;--------------------------------------------------------------------
#|
(defclass OMBasicObject (OMObject) 
   ((icon :initform nil :initarg :icon :accessor icon)
    (frames :initform nil :accessor frames)
    (EditorFrame :initform nil :accessor EditorFrame)
    (attached-objs :initform nil :accessor attached-objs)
    (protected-p :initform nil :initarg :protected-p :accessor protected-p))
   (:documentation "This is the class for OM metaobjects, like patches, classes, generic functions,etc
metaobject definitions are independent from their visualization. #enddoc#
#seealso# (omclass ommethod omgenericfunction omslot ompatch ommaquette ombasictype ombox ominstance omlispfun) #seealso#
#icon# This slot stocks an ID for Cicn resources in the aplication. #icon#
#frames# Frames keeps a list of OMSIMPLEFRAME instances which visualize the ombasicobject instance. #frames#
#editorframe# Nil if there is not an open editor, Only one editor can be open at one time,
If there is a editor open this slot stocks the scroller of the window whiwh visualize the editor. #editorframe#
#attached-objs# This slot contains a list of OMBasicObject instances which are atached to this instances,
for exemple alias, ominstances of a class, a box referencing a patch, etc. #attached-objs#
#protected-p# True if this object is protected. #protected-p#"))
|#

;-------------------Protocole------------------------------
(defgeneric allow-alias (self)  
   (:documentation "T if 'self' allows an alias the default is nil"))

(defgeneric get-documentation (self)  
   (:documentation "Return a string with the doc of the object 'self'."))


(defgeneric get-class-icon (self)  
   (:documentation "Return the class name that vizsualize 'self' in simple frame mode."))

(defgeneric get-object-insp-name (self)
   (:documentation "Return a string for graphic inspector."))

(defgeneric omG-change-icon (self newicon)  
   (:documentation "Changes the icon ID of 'self' with the new ID NEW-ICON,
 and update the current frames"))

(defgeneric omG-rename (self newname)  
   (:documentation "This method changes graphicly the name of 'self' with the new name 'newname',
 and updates the current frames"))

(defgeneric omNG-add-element (self elem)  
   (:documentation "Add 'elem' to 'self'"))

(defgeneric omNG-change-container (self oldcont newcont)  
   (:documentation "Move 'self' from the container 'oldcont' to the container 'newcont'"))

(defgeneric omNG-copy (self)  
   (:documentation "Return an expresion that produce a copy of 'self' when it is valued."))

(defgeneric omNG-delete (self)  
   (:documentation "Delete self if possible, retunr NIL otherwise."))

(defgeneric omng-remove-element (self elem)  
   (:documentation "remove 'elem' from 'self'"))

(defgeneric omNG-protect-object (self)  
   (:documentation "Protect the object 'self', so this object can not be modified"))


(defgeneric OpenEditorframe (self)  
   (:documentation "Create a new EditorFrame for 'self', if it is posible."))

(defgeneric OpenObjectEditor (self)  
   (:documentation "Open or select an editor for 'self'"))
;---------------

(defmethod allow-alias ((self OMBasicObject))
   "T if SELF allows an alias the default is nil"
   nil)

(defmethod get-documentation ((self OMBasicObject))
   "Return a string with the doc of the object"
   "An OMBasicObject")

(defmethod get-elements ((self OMBasicObject)) "The default value is NIL" nil)

(defmethod get-class-icon ((self OMBasicObject)) 
   "The default class for simple frames is 'icon-finder'."
   'icon-finder)

(defmethod get-object-insp-name ((self OMBasicObject)) "To redefine."
             "OM Basic Obj")

(defmethod omG-change-icon ((self OMBasicObject)  new-icon)
   "Changes the icon ID of SELF with the new ID NEW-ICON,
 and update the current frames"
   (unless new-icon (setf new-icon (choise-icon t t)))
   (when new-icon
     (setf (icon self) new-icon)
     (mapcar #'(lambda (frame)
                 (omg-change-icon frame new-icon)) (frames self))
     (mapcar #'(lambda (obj)
                 (omg-change-icon obj new-icon)) (attached-objs self))))
  
(defmethod omG-rename ((self OMBasicObject) new-name)
   "This method changes graphicly the name of SELF with the new name NEWNAME,
 and updates the current frames"
   (when (EditorFrame self)
     (omG-rename (EditorFrame self) new-name))
   (omNG-rename self new-name))

 
(defmethod omNG-add-element ((self OMBasicObject) elem)
   "Method to specialize" 
   (declare (ignore elem))
   self)

(defmethod omNG-change-container ((self OMBasicObject) oldcont newcont)
   "Method to specialize" self)

(defmethod omNG-copy ((self OMBasicObject))
   "This method is called by patches, folders, etc."
   `(make-instance ',(class-name (class-of self))
      :name ,(name self)
      :icon ,(copy-icon (icon self))))

(defmethod omng-remove-element ((self OMBasicObject) elem)
   "Method to specialize" self)

(defmethod omNG-protect-object ((self OMBasicObject))
   "A general method for all OMBasicObjects"
   (setf (protected-p self) t) 
   self)

(defmethod OpenEditorframe ((self T)) 
   "Create a new EditorFrame for SELF if it is posible" nil)

(defmethod OpenObjectEditor ((self OMBasicObject)) 
   "If there is a EditorFrame open  for SELF select the window of EditorFrame, 
else create a new Editor frame, and select its window."
   (setf (EditorFrame self) (OpenEditorframe self))
   (when (EditorFrame self)
     (om-select-window (window (Editorframe self))))
   )
   
  
(defmethod OpenObjectEditor ((self T)) (om-beep))

(defmethod omNG-delete ((self OMBasicObject)) t)
;---------------------------------------------------------


;-----------------Other methods------------------
(defmethod get-name-icon ((self OMBasicObject)) 
   (name self))

(defmethod kernel-p ((self OMBasicObject))
   (protected-p self))

(defmethod name-exist-p ((self OMBasicObject) name) 
   (find-if #'(lambda (x) (string-equal name (name x))) (get-elements self)))

(defmethod metaObject-p ((self OMBasicObject)) t)
(defmethod metaObject-p ((self t)) nil)

;-----------------------------------------------
;Persistant BasicObjects
;-----------------------------------------------
#|
(defclass OMPersistantObject (OMBasicObject) 
   ((mypathname :initform nil :initarg :mypathname :accessor mypathname)
    (omversion :initform *om-version* :accessor omversion)
    (doc :initform "no documentation" :accessor doc)
    (loaded? :initform t  :accessor loaded?)
    (saved? :initform t  :accessor saved?)
    (wsparams :initform (list (om-make-point 22 22) (om-make-point 40 40) (om-make-point 500 400)) :accessor wsparams))
   (:documentation "Persistants object are the subset of the metaobjects which have one owner pathname associated. #enddoc#
#seealso# (ombasicobject omclass ommethod ompatch ommaquette) #seealso#
#mypathname# A pathname of the file saving the object. #mypathname#
#omversion# The OM version of the last saved file. #omversion#
#doc# Store the documentation of some metaobjects (patches, maquettes, etc).
Documentation for other metaobjects like omClasses are handled by MCL. #doc#
#loaded?# T if the file is already loaded from file. #loaded?#
#saved?# Nil if the obj was modified and not saved.#saved?#
#wsparams# A list ('pos' 'edpos' 'edsize') where 
'pos' is the position of the simpleframe of self
'edpos' is the position of the editorframe of self
'edsize' the size of the editorframe of self.#wsparams#"))
|#

;-------------------------------------------------
;;; WS PARAMS = ITEM POS, WINDOW POS, WINDOW SIZE

(defmethod PersistantObject-p ((self OMPersistantObject)) t)
(defmethod PersistantObject-p ((self t)) nil)

(defun ensure-ws-params (obj)
  (unless (wsparams obj) (setf (wsparams obj) (def-ws-params obj)))
  (wsparams obj))

(defmethod def-ws-params ((self t))
  (list (om-make-point 24 24) (om-make-point 50 50) (om-make-point 500 400)))
  
(defmethod omNG-rename ((self OMPersistantObject) name) 
  "Rename 'self' but also the file saving the patch."
  (when (mypathname self)
    (let* ((oldpath (mypathname self))
           (newpath (make-pathname :directory (pathname-directory oldpath)
                                   :name name
                                   :type (obj-file-extension self))))
      (rename-file oldpath newpath)
      (setf (mypathname self) newpath)))
  (call-next-method))

(defmethod omNG-rename :after ((self OMPersistantObject) name) 
  (mapc #'(lambda (el) (change-name el)) (attached-objs self)))

(defmethod omng-copy ((self OMPersistantObject))
  `(let ((copy ,(call-next-method)))
     (setf (create-info copy) (list (om-get-date) "--"))
     (setf (doc copy) ,(doc self))
     copy))

;-------------------Protocole------------------------------
(defgeneric get-documentation (self)  
   (:documentation "Return a string with the self's documenation"))

(defgeneric obj-file-type (self)  
   (:documentation "Return the mac-file-type saving the object 'self'"))

(defgeneric get-icon-pos (self)  
   (:documentation "Get the position of the self's simpleframe"))

(defgeneric get-win-position (self)  
   (:documentation "Get the position of the self's editors"))

(defgeneric get-win-size (self)  
   (:documentation "Get the size of the self's editors"))

(defgeneric set-doc (self newdoc)  
   (:documentation "Set the doc of 'self' to 'newdoc'"))

(defgeneric set-icon-pos (self newpos)  
   (:documentation "Set the position of the self's simpleframe to 'newpos'"))

(defgeneric set-win-position (self newpos)  
   (:documentation "Set the position of the self's editor's to 'newpos'"))

(defgeneric set-win-size (self newsize)  
   (:documentation "Set the size of the self's editor to 'newsize'"))

;----------------
(defmethod get-documentation ((self OMPersistantObject))
   "Objects like patches or maquettes keep their doc in the 'doc' slot." (doc self))


(defmethod obj-file-type ((self OMPersistantObject)) :????)
(defmethod obj-file-extension ((self OMPersistantObject)) "om")

(defmethod get-icon-pos ((self t)) "Get the simpleframe's pos" (om-make-point 22 22))
(defmethod get-icon-pos ((self OMPersistantObject)) (first (ensure-ws-params self)))

(defmethod get-win-position ((self t)) "Get the editor's pos" (om-make-point 40 40))
(defmethod get-win-position ((self OMPersistantObject)) (second (ensure-ws-params self)))

(defmethod get-win-size ((self t)) "Get the editor's size" (om-make-point 500 400))
(defmethod get-win-size ((self OMPersistantObject)) (third (ensure-ws-params self)))

(defmethod set-doc ((self t) newdoc) "Set the doc of 'self' to 'newdoc'" newdoc)
(defmethod set-doc ((self OMPersistantObject) newdoc)
  (setf (changed-wsparams? self) t) 
  (setf (doc self) newdoc))

;;; test...
(defmethod set-doc :after ((self OMPersistantObject) newdoc)
  (when (frames self)
    (om-view-set-help (iconview (car (frames self))) (get-documentation self))))

(defmethod set-icon-pos ((self t) newpos) (declare (ignore newsize)) "Set the simpleframe's pos to 'newpos'" t)
(defmethod set-icon-pos ((self OMPersistantObject) newpos) 
  (ensure-ws-params self)
  (setf (changed-wsparams? self) t)
  (setf (nth 0 (wsparams self)) newpos))

(defmethod set-win-position ((self t) newpos) (declare (ignore newpos)) "Set the editor's pos to 'newpos'" t)
(defmethod set-win-position ((self OMPersistantObject) newpos) 
  (ensure-ws-params self)
  (setf (changed-wsparams? self) t)
  (setf (nth 1 (wsparams self)) newpos))

(defmethod set-win-size ((self t) newsize) (declare (ignore newsize)) "Set the editor's size to 'newsize'" t)
(defmethod set-win-size ((self OMPersistantObject) newsize) 
  (ensure-ws-params self)
  (setf (changed-wsparams? self) t)
  (setf (nth 2 (wsparams self)) newsize))




;-------------------Other Methods-----------------------------
;For workspaces the elements are not in the pathname, this is the only exception.
(defmethod elements-pathname ((self OMPersistantObject))
   (mypathname self))

;Used when you change of folder or paste elements in a new folder
;;;(defmethod omNG-change-container ((self OMPersistantObject) oldcont newcont)
;;;   "Change also the pathname of 'self'."
;;;   (setf (elements oldcont) (remove self (elements oldcont) :test 'equal))
;;;   (push self (elements newcont))
;;;   (rename-file (mypathname self) (special-pathname (elements-pathname newcont) (name self)))
;;;   (setf (mypathname self) (special-pathname (elements-pathname newcont) (name self))))

(defmethod omNG-change-container ((self OMPersistantObject) oldcont newcont)
  "Change also the pathname of 'self'."
  (let (newpath) 
    (setf (elements oldcont) (remove self (elements oldcont) :test 'equal))
    (push self (elements newcont))
    (setf newpath (make-pathname :directory (pathname-directory (elements-pathname newcont)) 
                                 :name (name self)
                                 :type (pathname-type (mypathname self))))
    (rename-file (mypathname self) newpath)
    (setf (mypathname self) newpath)))

;-----------------------------------------------
;Persistant FoldersObjects
;-----------------------------------------------
#|
(defclass OMPersistantFolder (OMPersistantObject) ()
   (:documentation "This is yhe class of persistant objects that are saved as folders.
(i.e. omfolders ompackages and omworkspaces)#enddoc#
#seealso# (OMPersistantObject OMfolder OMpackage omworkspace) #seealso#"))
|#

(defmethod really-add ((self OMPersistantFolder)  (elem t))
  (push elem (elements self)))

(defmethod really-remove ((self OMPersistantFolder)  (elem t))
  (setf (elements self) (remove elem (elements self) :test 'equal)))


(defmethod elements-with-path ((self OMPersistantFolder))
   "Return a list of all the peristant objects in 'self'.
   By default return all elements in 'self'."
   (get-elements self))


(defmethod omNG-add-element ((self OMPersistantFolder) (elem OMPersistantObject))
  "Add 'elem' to the persistant folder 'self'."
  (let ((rep (really-add self elem)))
     (setf (loaded? elem) nil)
     (set-path-element self elem)
     rep))

(defmethod omNG-add-element ((self OMPersistantFolder) (elem OMPatch))
  (let ((rep (call-next-method)))
    ;;;(om-message-dialog (string+ (name self) " " (name elem)))
    (load-patch elem)
    (omng-save elem)
    rep))

(defmethod omNG-add-element ((self OMPersistantFolder) (elem OMPersistantFolder))
   "Add the persistentfolder 'elem' to the persistentfolder 'self'."
   (really-add self elem)
   (let ((path (make-pathname :directory (append (pathname-directory (elements-pathname self)) (list (name elem))))))
     (setf (mypathname elem) path)
     (om-create-directory path)
     (create-om-file elem path :file-type (obj-file-type elem))
     (mapc #'(lambda (el) (change-father-name el (elements-pathname self))) (elements-with-path elem))))


(defmethod set-path-element ((self OMPersistantFolder) elem)
   "Create a new file when you add an element in the folder."
   (let ((path (om-make-pathname
                              :directory (elements-pathname self) :name (name elem)
                              :type (obj-file-extension elem))))
     (if (and (mypathname elem) (probe-file (mypathname elem)))
         (copy-file-sp (mypathname elem) path)
       (create-om-file elem path :file-type (obj-file-type elem)))
     (setf (mypathname elem) path)))


(defmethod omNG-rename ((self OMPersistantFolder) name) 
   "When you rename a folder you must rename ALL elements in it."
   (let* ((path (elements-pathname (get-real-container self)))
          new-path)
     (setf new-path (make-pathname :directory (append (pathname-directory path) (list name))))
     (rename-file (mypathname self) new-path)
     (setf (name self) name)
     (setf (mypathname self) new-path)
     (mapc #'(lambda (el) (change-father-name el (mypathname self)))
           (elements-with-path self)) t))

(defmethod omng-remove-element ((self OMPersistantFolder) elem)
   "Remove  'elem' from the folder 'self'."
   (when (omNG-delete elem)
     (when (Editorframe elem) (om-close-window (window (Editorframe elem))))
     (really-remove self elem) t))

(defmethod omng-remove-element ((self OMPersistantFolder) (elem OMPersistantObject))
   "Remove  'elem' from the folder 'self'."
   (when (call-next-method)
     (om-delete-file (mypathname elem))))

(defmethod omng-remove-element ((self OMPersistantFolder) (elem OMPersistantFolder))
   "Remove the persistantfolder 'elem' from the persistantfolder 'self'."
   (when (Editorframe elem) (om-close-window (window (Editorframe elem))))
   (mapc #'(lambda (el) (omng-remove-element elem el)) (elements-with-path elem))
   (really-remove self elem)
   (when (mypathname elem)
     ;(om-deep-delete-file (mypathname elem))
     (om-delete-directory (mypathname elem))
     ))

(defmethod really-change-container ((self OMPersistantFolder)  oldcont newcont)
   (setf (elements oldcont) (remove self (elements oldcont) :test 'equal))
   (push self (elements newcont)))

(defmethod omNG-change-container ((self OMPersistantFolder) oldcont newcont)
   "Move 'self' from 'oldcont' to 'newcont'. 'oldcont' and 'newcont' are also persistantfolders." 
   (let* ((path1 (elements-pathname newcont))
          new-path)
     (setf new-path (make-pathname :directory (append (pathname-directory path1) (list (name self)))))
     (rename-file (mypathname self) new-path)
     (setf (mypathname self) new-path)
     
     ;(print (list "Move" self "from" oldcont "to" newcont))
     (really-change-container self oldcont newcont)
     (mapc #'(lambda (el) (change-father-name el (mypathname self))) (elements-with-path self))))




(defmethod change-father-name ((self OMPersistantFolder) path)
   (setf (mypathname self) (make-pathname :directory (append (pathname-directory path)  (list (name self)))))
   (mapc #'(lambda (el) (change-father-name el (mypathname self))) 
         (elements-with-path self)))

(defmethod change-father-name ((self t) path)
  (when (pathnamep (loaded? self))
    (progn (copy-file-sp (loaded? self) path) (setf (loaded? self) nil)))
  (when (mypathname self)
    (setf (mypathname self) (make-pathname :directory (pathname-directory path)  
                                           :name (name self)
                                           :type (pathname-type  (mypathname self))))))

;-----------------------------------------------
;The graphic visualization of the OMBasicObjects
;-----------------------------------------------
#|
(defclass OMFrame (OMObject) 
   ((object :initform nil :initarg :object :accessor object))
   (:documentation "This is the class of frames which visualize the OMBasicObjects. 
Frames can be simple frames (icons, boxes, etc.) or container frames (patch editor, folder editor, etc.).#enddoc#
#seealso# (omSimpleFrame omContainerFrame omBasicObject) #seealso#
#object# This slots points to the object represented by the frame.#object#"))
|#

;-----------------------------------------------
;Containers
;-----------------------------------------------
#|
(defclass OMContainerFrame (OMFrame) ()
   (:documentation "Container frames are frames which can containt OMSimpleFrames within.
In general EditorFrames are instances of the class metaobj-panel, this class inherites from the 'View' class 
and the 'OMcontainerFrame' class.#enddoc#
#seealso# (metaobj-panel) #seealso#"))
|#

(defmethod mk-unique-name ((self OMContainerFrame) string)
   "Return an new unique for an object contained in 'self'."
   (unique-name-from-list-new string (get-elements (object self))))

(defmethod match-name ((self OMContainerFrame) name)
   "True if exist an element in SELF with name NAME"
   (let* ((lista (get-subframes self))
          (next t))
     (loop while (and lista next) do
           (if (string-equal name (name (pop lista)))
             (setf next nil)))
     (if next nil t)))

;-----------------------------------------------
;Simple Frames
;-----------------------------------------------
#|
(defclass OMSimpleFrame (view OMFrame) 
   ((active-mode :initform nil :accessor active-mode))
   (:documentation "Simple frames visualize objects as simple views.
in difference of OMContainerFrame this class is not graphic independent, 
so this class inherite from view. #enddoc#
#seealso# (boxframe icon-finder icon-method) #seealso#
#active-mode# T if the object of the frame is selected.#active-mode#"))
|#


;----------

;;; !!! ICONVIEW ???
(defmethod omG-select ((self OMSimpleFrame))
   "Set the frame SELF and the object pointed for frame in selected mode"  
   (when (not (active-mode self)) 
     (setf (active-mode self) t)
     (when (iconView self)
       (setf (selected-p (iconView self)) t))
     (om-invalidate-view self)))

(defmethod omG-unselect ((self OMSimpleFrame))
   "Set the frame SELF and the object pointed for frame in unselected mode"
   (when (active-mode self)
     (setf (active-mode self) nil)
     (when (iconView self)
       (setf (selected-p (iconView self)) nil))
     (om-invalidate-view self)))

(defmethod toggle-icon-active-mode ((self OMSimpleFrame))
   "Select or unselect 'self'."
   (cond ((om-shift-key-p) 
          (if (active-mode self)
              (omG-unselect self) 
            (omG-select self)))
         (t (unless (active-mode self)
              (mapc #'(lambda (icon)
                        (omG-unselect icon)) (get-actives (om-view-container self)))
              (omG-select self))))
   t)

;----------------------------------------------------


