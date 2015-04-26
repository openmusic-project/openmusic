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

;======================================
;METAOBJECT CLASSES ABSTRACT
;======================================

;===========================
;Environnement
;===========================

(defclass OMObject () 
   ((name :initform nil :initarg :name :accessor name))
   (:documentation "All object in Open Music is an OMObject.#enddoc#
#seealso# (omBasicObject omFrame) #seealso#
#name# Open Music is a label based system, so all object has a name. #name#"))

(defclass OMBasicObject (OMObject) 
   ((icon :initform nil :initarg :icon :accessor icon)
    (frames :initform nil :accessor frames)
    (EditorFrame :initform nil :accessor EditorFrame)
    (attached-objs :initform nil :accessor attached-objs)
    (protected-p :initform nil :initarg :protected-p :accessor protected-p)
    (infowin :initform nil :accessor infowin))
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

(defclass OMPersistantObject (OMBasicObject) 
  ((mypathname :initform nil :initarg :mypathname :accessor mypathname)
   (omversion :initform *om-version* :accessor omversion)
   (doc :initform "" :accessor doc)
   (loaded? :initform t :accessor loaded?)
   (saved? :initform t  :accessor saved?)
   (wsparams :initform nil :accessor wsparams)
   (create-info :initform '(nil nil) :accessor create-info)
   (changed-wsparams? :initform nil :accessor changed-wsparams?))
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

;;; separate this for later (see edition-params.lisp)
(defclass object-with-persistant-params ()
  ((edition-params :initform nil  :accessor edition-params)))

(defclass OMPersistantFolder (OMPersistantObject) ()
  (:documentation "This is yhe class of persistant objects that are saved as folders.
(i.e. omfolders ompackages and omworkspaces)#enddoc#
#seealso# (OMPersistantObject OMfolder OMpackage omworkspace) #seealso#"))


;===========================
;METAOBJECTS
;===========================

;======= multiple heritage

(defclass OMClass (standard-class OMPersistantObject) 
   ((lib-class-p :initform nil :accessor lib-class-p)
    (internal-met :initform nil :accessor internal-met)
    (slot-docs :initform nil :accessor slot-docs))
   (:documentation "OM meta-class. #enddoc#
#seealso# (omslot ommethod omstandardclass) #seealso#
#lib-class-p# Non nil, if the function was defined in a Library. #lib-class-p#
#internal-met# A list with initialization and R/W slots methods. #internal-met#
#doc# Doc of somes persistants object are saved also. #doc#"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((class omclass) (super standard-class)) t)
)

(defclass OMStandardClass (OMClass)  ()
   (:documentation "This is the current OM meta-class, you can sub-class it and used the new class as current meta-class.
This class is a OMClass a diferencia of OMClass.#enddoc#
#seealso# (omclass) #seealso#")
   (:metaclass OMClass))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmethod validate-superclass ((class omstandardclass) (super standard-class)) t)
  (defmethod validate-superclass ((class standard-class) (super omstandardclass)) t)
  (defmethod validate-superclass ((class standard-class) (super omclass)) t)

)

(defclass OMGenericFunction (OMFuncallableBasicObject) 
   ((numouts :initform nil :accessor numouts)
    (protected-p :initform nil :accessor protected-p :initarg :protected-p)
    (inputs-default :initform nil :accessor inputs-default)
    (lib-fun-p :initform nil :accessor lib-fun-p)
    (inputs-doc :initform nil :accessor inputs-doc)
    (outputs-doc :initform nil :accessor outputs-doc)
    (inputs-menus :initform nil :accessor inputs-menus))
  (:default-initargs :protected-p t)
  (:documentation "The generic function meta-object in OM. #enddoc#
#seealso# (ommethod) #seealso#
#numouts# Multiple values are allowed in OM, but it must be specified in the definition of the function. #numouts#
#inputs-default# This slot containt a list of default values for each arg in the function's lambda list. #inputs-default#
#lib-fun-p# Non nil, if the function was defined in a Library. #lib-fun-p#
#inputs-doc# This slot containt a list of string doc for each arg in the function's lambda list. #inputs-doc#
#inputs-menus# Some arg choose values from a pop-up-menu, this slot contains this information #inputs-menus#")
  (:metaclass  funcallable-standard-class)
  )


(defclass OMMethod (OMPersistantObject standard-method) 
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

;==========OM

(defclass OMPatch (OMPersistantObject)          
   ((boxes :initform nil :initarg :boxes :accessor boxes)
    (code :initform (gensym) :accessor code)
    (connec :initform nil  :accessor connec)
    (compiled? :initform nil  :accessor compiled?)
    (pictu-list :initform nil :accessor pictu-list)
    (lisp-exp-p :initform nil :accessor lisp-exp-p)
    (show-connections? :initform t :accessor show-connections?)
    (animation :initform nil :accessor animation))
   (:documentation "The class of the OM patch metaobjects. #enddoc#
#seealso# (OMPersistantObject OMmaquette OMTemporalPatch OMPatchAbs) #seealso#
#boxes# A list of the boxes which define the patch. #boxes#
#code# A symbol to reference the code definition of the patch. #code#
#connec# Used to save connections beetween boxes in the patch definition. #connec#
#compiled?# Nil if the patch was modified and not compiled.#compiled?#
#pictu-list# This slot has a list of pictures's names pointed to pictures resources in the patch file.#pictu-list#
#lisp-exp-p# Patches can be defined by a lambda expression in place of graphicly,
in this case this slot keeps the lambda expression.#lisp-exp-p#
#show-connections?# Flag to show or hide connections.#show-connections?#")
   (:metaclass omstandardclass))

(defclass OMMaquette (OMPatch) 
   ((pictu :initform (make-new-om-pict) :accessor pictu)
    (params :initform (new-maquette-params) :accessor params)
    (eval-func :initform nil :accessor eval-func)
    (value :initform nil :accessor value))
   (:documentation "The class of the om maquette metaobjects.#enddoc#
#seealso# (OMmaquette TemporalBox OMPatch OMmaqabs) #seealso#
#pictu# If there exists this slot store a background picture for the maquette. #pictu#
#params# A list of parameters for maquette edition and visualitation. #params#")
   (:metaclass omstandardclass))


(defclass OMBox (OMBasicObject) 
   ((inputs :initform nil :initarg :inputs :accessor inputs)
    (reference :initform nil :initarg :reference :accessor reference)
    (frame-position :initform nil :accessor frame-position)
    (frame-size :initform nil :accessor frame-size)
    (frame-name :initform nil :accessor frame-name))
   (:documentation "The OM meta-object OMBox is is the more general class for connectable objects.
There are two main type the connectable objects OMboxCall and OMBoxClass.
The first ones are boxes in a Path (Maquette), the second ones are class-references in a class hierarchical tree.#enddoc#
#seealso# (OMboxCall OMBoxClass) #seealso#
#inputs# A list of input instances objects see input-funbox class.#inputs#
#reference# The reference specifies the box's type, for exemple if the reference is a class the box is a factory of instances,
if the reference is a generic function the box is a call of generic function. Differents sub-classes of OMBox are defined
in relation with the reference.#reference#
#frame-position# Store the position of the graphic frame. #frame-position#
#frame-size# Store the size of the graphic simple frame. #frame-size#
#frame-name# Store the name of the graphic simple frame, it is not necessary the same as the box.#frame-name#")
   (:metaclass omstandardclass))

;==========building

(defclass OMSlot (OMBasicObject)
   ((classname  :initarg :classname :accessor classname)
    (thetype :initform t :initarg :thetype :accessor thetype)
    (theinitform :initarg :theinitform :accessor theinitform)
    (alloc  :initarg :alloc :accessor alloc)
    (slot-package  :initarg :slot-package :accessor slot-package)
    (io-p  :initarg :io-p :accessor io-p)
    (doc  :initarg :doc :accessor doc))
   (:documentation "Instance of this class allow define graphicly slot of omClasses. #enddoc#
#seealso# (omclass OMBasicObject) #seealso#
#classname# A name with the class which contains the slot. #classname#
#thetype# OM slots are typed, the default type if T, however, there are not type checking in OM. #thetype#
#theinitform# Initform value for the slot. #theinitform#
#alloc# 'class or 'instance #alloc#
#io-p# Public or private slot flag. #io-p#")
   (:metaclass omstandardclass))

(defclass OMLispFun (OMBasicObject) 
   ((funname :initform nil :initarg :funname :accessor funname))
   (:documentation "This class implements the lisp functions.#enddoc#
#seealso# (OMBoxlispCall) #seealso#
#funname# The symbol name of the function.#funname#"))

(defmethod function-name ((self OMLispFun)) (funname self))

(defclass OMBasictype (OMBasicObject)
   ((defval :initform nil :initarg :defval :accessor defval))
   (:documentation "This class implements building class in Common Lisp
because we can not inherite for building class we use delegation.
There are not all basic classes from lisp, you can add eassy new basic types if
they have a building class correspondant, for this see the function initbasic-lisp-types.#enddoc#
#seealso# (OMBasicObject) #seealso#
#defval# The default value for the Basic Type.#defval#"))

(defclass OMInstance (OMPersistantObject object-with-persistant-params) 
   ((loaded? :initform t  :accessor loaded?)
    (instance :initform nil :initarg :instance :accessor instance)
    (saved? :initform nil  :accessor saved?)
    (setfInfo :initform nil :accessor setfInfo)
    (pictu-list :initform nil :accessor pictu-list))
   (:documentation "This is the class used to implement instances,
we use the technique of delegation. #enddoc#
#seealso# (OMConstant OMListInstance) #seealso#
#setfInfo# When you edit an instance graphically this slot help us to keep the path for modifications. #setfInfo#
#loaded?# This slot is a flag T if the instance was already loaded from memory, nil otherwise. #loaded?#
#instance# The really instance is in this slot. #instance#
#saved?# This slot is a flag T if the instance was already saved in memory, nil otherwise. #saved?#
#edition-params# Some instances can need extra info for edition, specialize the generic function 'set-edition-params' for the class of this instance. #edition-params#"))


;===========================
;FRAMES
;===========================
(defclass OMFrame (OMObject) 
   ((object :initform nil :initarg :object :accessor object))
   (:documentation "This is the class of frames which visualize the OMBasicObjects. 
Frames can be simple frames (icons, boxes, etc.) or container frames (patch editor, folder editor, etc.).#enddoc#
#seealso# (omSimpleFrame omContainerFrame omBasicObject) #seealso#
#object# This slots points to the object represented by the frame.#object#"))

(defclass OMSimpleFrame (OMFrame) 
   ((active-mode :initform nil :accessor active-mode))
   (:documentation "Simple frames visualize objects as views. #enddoc#
#seealso# (boxframe icon-finder icon-method) #seealso#
#active-mode# T if the object of the frame is selected.#active-mode#"))

;;; OMSimpleFrame and OMCompoundFrame are identical in OM 
;;; but they are implemented differently in the underlying API
(defclass OMAtomicFrame (om-item-view OMSimpleFrame) ())
(defclass OMCompoundFrame (om-internal-view OMSimpleFrame) ())


(defclass OMContainerFrame (om-view OMFrame) ()
   (:documentation "Container frames are frames which can containt OMSimpleFrames within.
In general EditorFrames are instances of the class metaobj-panel, this class inherites from the 'View' class 
and the 'OMcontainerFrame' class.#enddoc#
#seealso# (metaobj-panel) #seealso#"))

;===========================
;Environnement
;===========================

(defclass OMWorkSpace (OMPersistantFolder) 
   ((elements :initform nil :accessor elements)
    (packages :initform nil :accessor packages))
   (:documentation "The class of the OM Workspace. #enddoc#
#seealso# (OMFolder OMPackage) #seealso#
#elements# A list of objects contained in the Folder i.e. patches, maquettes or  folders. #elements#
#packages# Point to the main package of the session. #packages#"))

(defclass OMFolder (OMPersistantFolder) 
   ((elements :initform nil :initarg :elements :accessor elements)
    (presentation :initform 1 :initarg :presentation :accessor presentation))
   (:documentation "The class of the OM folders. #enddoc#
#seealso# (OMPersistantObject OMWorkspace) #seealso#
#elements# A list of objects contained in the Folder i.e. patches, maquettes or anothers folders. #elements#"))

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

(defclass OMLib (OMpackage) 
   ((lib-pathname :initform nil :initarg :lib-pathname :accessor lib-pathname)
    (loaded? :initform nil :accessor loaded?)
    (release :initform nil :accessor release)
    (version :initform nil :initarg :version :accessor version))
   (:documentation "The class for the OM Library  metaobjects.
One OMlib is a collection of classes and generic functions loaded dinamiclly.#enddoc#
#seealso# (ombasicobject ompackage) #seealso#
#lib-pathname# The pathname of the librarie folder. #lib-pathname#
#loaded?# T is the LIbrary is already loaded. #loaded?#"))


