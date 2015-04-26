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
;This file implements the OMInstance class.
;OMInstance class allow us to handle instances graphicly.
;There are to types of instance : instance from classes (OMClass) and
;instance from building-classes (OMBasicTYpes), there exist subclasses which handle this difference.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)


#|
(defclass OMInstance (OMPersistantObject) 
   ((loaded? :initform t  :accessor loaded?)
    (instance :initform nil :initarg :instance :accessor instance)
    (saved? :initform nil  :accessor saved?)
    (setfInfo :initform nil :accessor setfInfo)
    (pictu-list :initform nil :accessor pictu-list)
    (edition-params :initform nil  :accessor edition-params))
   (:documentation "This is the class used to implement instances,
we use the technique of delegation. #enddoc#
#seealso# (OMConstant OMListInstance) #seealso#
#setfInfo# When you edit an instance graphicly this slot help us to keep the path for modifications. #setfInfo#
#loaded?# This slot is a flag T if the instance was already loaded from memory, nil otherwise. #loaded?#
#instance# The really instance is in this slot. #instance#
#saved?# This slot is a flag T if the instance was already saved in memory, nil otherwise. #saved?#
#edition-params# Some instances can need extra info for edition, specialize the generic function 'set-edition-params' for the class of this instance. #edition-params#"))
|#


;-------------Inits
(defun omNG-make-new-instance (instance name &optional (posi (om-make-point 0 0)))
   "Build one OMInstance from a real instance, you can specifie a name and a position for the frame.
if the instance is a list or a constant a subclass of OMInstance will be create."
   (let (newins)
     (cond
      ((omclass-p (class-of (class-of instance)))
       (setf newins (make-instance 'OMInstance 
                      :name name 
                      :instance instance
                      :icon (icon (class-of instance))))
       (setf (edition-params newins) (set-edition-params instance newins)))
      ((listp instance)
       (setf newins (make-instance 'OMlistInstance 
                      :name name 
                      :instance instance
                      :icon (if (null instance) 177 129)))
       (setf (edition-params newins) (set-edition-params instance newins)))
      (t (setf newins (make-instance 'OMConstant 
                        :name name 
                        :value instance))
         (setf (edition-params newins) (set-edition-params instance newins))))
     (set-icon-pos newins posi)
     (setf (create-info newins) (list (om-get-date) "--"))
     newins))

(defmethod ominstance-p ((self OMInstance)) t)
(defmethod ominstance-p ((self t)) nil)

(defmethod global-p ((self OMInstance)) (mypathname self))

(defmethod omNG-add-element ((self OMInstance) elem)
   "The slot addition is made at class level not at instance level"
   (declare (ignore elem)) nil)

(defmethod get-class-icon ((self OMInstance)) 'instance-icon-frame)

(defmethod omng-remove-element ((self OMInstance) elem)
   "Remove a slot is made at class level not at instance level"
   (declare (ignore elem)) nil)

(defmethod omNG-rename ((self OMInstance) name)
   "Rename an instance must also change the name of all atached-objs"
   (declare (ignore name))
   (let ((rep (call-next-method)))
      ; now in ompersistantobject
      ;  (mapc #'(lambda (el) (change-name el)) (attached-objs self))
     rep))

(defmethod omNG-delete ((self OMInstance))
   "Delete an instance must also update all atached-objs"
   (when (attached-objs self)
     (if (om-y-or-n-dialog (string+ "Warning, there are some elements attached to the instance "
                                    (name self)
                                    ". Do you want to delete it ?"))
       (mapc #'(lambda (el) (dead-reference el)) (attached-objs self))
       (om-abort))) t)

(defmethod get-object-insp-name ((self OMInstance)) "Instance")

(defmethod get-documentation ((self OMInstance))
  ;(string+ "This is an instance of the class " (string (type-of (instance self))))
  (doc self))

(defmethod get-elements ((self OMInstance))
   "Instances don't have elements" nil)

(defmethod obj-file-type ((self OMInstance)) :INST)
(defmethod obj-file-extension ((self OMInstance)) "omi")

(defmethod OpenEditorframe ((self OMInstance))
   "For instance there are to type of Editorframe:
if the instance's class have an editor (for exemple note) you can open this editor,
but in all case you can open an editor for the instance slots, see the class InstanceWinFrame."
   (setf (saved? self) nil)
   (cond
    ;; ???
    ((om-command-key-p)
     (if (editorframe self)
       (if (instsroller-p (editorframe self)) (editorframe self)
           (progn
             (om-close-window (window (editorframe self)))
             (open-struct-editor self)))
       (open-struct-editor self)))
    ((Class-has-editor-p (instance self))
     (if (editorframe self)
       (if (instsroller-p (editorframe self))
         (progn
           (om-close-window (window (editorframe self)))
           (editor (make-editor-window (get-editor-class (instance self)) (instance self)
                                       (name self) self)))
         (editorframe self))
       (editor (make-editor-window (get-editor-class (instance self)) (instance self)
                                   (name self) self))))
    (t (or (editorframe self) (open-struct-editor self)))))


(defmethod make-slots-ins-boxes ((self OMInstance))
   "Return a list of simple frames for each slot value of the instance slot of 'self'."
   (slots-inst-boxes self (instance self)))

(defun slots-inst-boxes (self instance)
   (let* ((class-list (get-om-superclasses (class-of instance)))
          (i 0)
          checked-list rep)
     (mapc #'(lambda (class)
               (let* ((slots (get-direct-slots-of-class (class-name class)))
                      (y1 (* i 80)) (j 0))
                 (push (om-make-dialog-item 'om-static-text (om-make-point 0 (+ 5 y1)) (om-make-point 1000 18) 
                                                                       (string-downcase (class-name class))
                                                                       :font *om-default-font2*
                                                                       :bg-color *azulito* 
                                                                       ) rep)
                 (mapc #'(lambda (slot)
                           (when (and (io-p slot) (null (position (name slot)  checked-list :test 'string-equal)))
                             (let* ((slot-name (internp (name slot) (slot-package slot)))
                                    (x1 (+ 7 (* j 60)))
                                    (new-instance (omNG-make-new-instance (eval `(,slot-name ,instance))
                                                                          (name slot) (om-make-point x1 (+ 35 y1))))
                                    new-frame)
                               (setf (setfInfo new-instance) (list slot-name self))
                               (setf new-frame (make-inst-from-object  new-instance x1  (+ 35 y1) 1 1))
                               (push (name slot) checked-list)
                               (push new-frame rep))
                             (incf j))) slots)
                 (incf i))) class-list)
     rep))


(defmethod get-editor-class ((self ominstance)) 'InstanceEditor)

(defmethod open-instance-editor ((self OMInstance))
   (let* ((thewindow (make-editor-window 'InstanceEditor
                                         self (name self) nil 
                                         :winpos (om-make-point 150 100)
                                         :winsize (om-make-point 250 280)))
          (slot-boxes (make-slots-ins-boxes self)))
     (setf (presentation (editor thewindow)) 0)
     (mapc #'(lambda (frame)
                       (omG-add-element (panel (editor thewindow)) frame)) slot-boxes)
     ;;; test 
     (set-field-size (panel (editor thewindow)))
     ;;;
     (panel (editor thewindow))))

(defmethod open-struct-editor ((self OMInstance))
   (if (listp (instance self))
     (open-instance-list-editor self 0 0) 
     (open-instance-editor self)))

(defmethod make-inst-from-object ((self OMInstance) x y x1 y1)
  (let ((icon (make-icon-from-object self x y x1 y1)))
    (setf (change-name-p icon) nil)
    icon))

;----------------------------------------------------------
;Instances from Basic Types
;----------------------------------------------------------

(defvar *size-list-inx* 10)
(defvar *size-list-iny* 10)

(defclass OMlistInstance (OMInstance) ()
   (:documentation "Used to make the differences with other instance,
the only reason is that  list are draw as a tabler.#enddoc#
#seealso# (OMInstance) #seealso#"))

(defmethod propagate-changes ((self t) accse val)
   "When you edit graphicly an instance whiwh is an slot of another one you must propagate
these changements, this method is called for OMInstances and OMconstants"
   (eval `(setf (,accse ,(instance self)) ',val))
   ;;; ???
   ; (when  (setfInfo self)
    ; (propagate-changes (second (setfInfo self)) (first (setfInfo self)) (instance self)))
   )

(defmethod propagate-changes ((self OMlistinstance) accse val)
   "When you edit graphicly an instance whiwh is an slot of another one you must propagate
these changements, this method is called for OMlistInstance"
   (if (listp accse)
     (setf (nth (first accse) (nth (second accse) (instance self))) val)
     (setf (nth accse (instance self)) val))
   (when (setfInfo self)
     (propagate-changes (second (setfInfo self)) (first (setfInfo self)) (instance self))))

(defmethod get-editor-class ((self omlistinstance)) 'listEditor)

(defmethod open-instance-list-editor ((self OMlistinstance) startx starty)
   "When the instance slot of 'self' is a list, this method is called in order to open the editor."
   (if (null (instance self))
     (om-beep-msg "NIL has no editor")
     (let* ((thewindow (make-editor-window 'listEditor
                                           self (name self) nil 
                                           :winpos (om-make-point 100 100)
                                           :winsize (om-make-point 640 140)))
            (boxes (make-elemnts-list-boxes self startx starty)))
       (setf (presentation (editor thewindow)) 0)
       (mapc #'(lambda (frame)
                         (omG-add-element (panel (editor thewindow)) frame)) boxes)
       ;;; jb
       (set-field-size (panel (editor thewindow)))
       ;;;
       (panel (editor thewindow)))))


(defmethod make-elemnts-list-boxes ((self OMlistinstance) startx starty)
   "Return a list of simple frames for each slot value of the instance slot of 'self'."
   (loop for j from startx to (+ startx (- *size-list-inx* 1))
         for i = 0 then (incf i) 
         for item = (nth j (instance self))
         while (< j (length (instance self)))
         when (or (null item) (not (listp item)))
         collect (let* ((new-instance (omNG-make-new-instance item (format nil "~D" j) (om-make-point (+ 15 (* i 60)) 15 )))
                        (new-frame (make-inst-from-object  new-instance (+ 15 (* i 60))  15 1 1)))
                   (setf (setfInfo new-instance) (list i self))
                   (setf (change-name-p new-frame) nil)
                   new-frame)
         else
         nconc (make-col-list-boxes item i self starty j)))


;to display list in 2 dimension
(defun make-col-list-boxes (list x self starty col)
   (loop for j from starty to (+ starty (- *size-list-iny* 1))
         for i = 0 then (incf i) 
         for item = (nth j list)
         while (< j (length list))
         collect (let* ((new-instance (omNG-make-new-instance item (format nil "(~D,~D)" col j) (om-make-point (+ 15 (* x 60)) (+ 15 (* 60 i)))))
                        (new-frame (make-inst-from-object  new-instance (+ 15 (* x 60))  (+ 15 (* 60 i)) 1 1)))
                   (setf (setfInfo new-instance) (list (list i x) self))
                   (setf (change-name-p new-frame) nil)
                   new-frame)))

;---------------------------------------
 
(defclass OMConstant (OMPersistantObject object-with-persistant-params) 
   ((value :initform nil :initarg :value :accessor value)
    (setfInfo :initform nil :accessor setfInfo))
   (:documentation "Constants are instance of the OMBasictype class.#enddoc#
#seealso# (OMBasictype OMInstance) #seealso#
#value# The value of the constant #value#
#setfInfo# When you edit an instance graphicly this slot help us to keep the path for modifications. #setfInfo#
#edition-params# Some instances can need extra info for edition, specialize the generic function 'set-edition-params' for the class of this instance. #edition-params#"))


(defmethod Constantobj-p ((self OMConstant)) t)
(defmethod Constantobj-p ((self t)) nil)

;If self is an OMConstant the frame is not a iconframe, but a constant-simple-frame"
(defmethod make-inst-from-object ((self OMConstant) x y x1 y1)
   (declare (ignore x1 y1))
   (let ((frame (om-make-view 'constant-simple-frame
                  :position (om-make-point x y)
                  :size (om-make-point 55 46))))
     (unless (listp (name self))
        (setf (nameview frame) (om-make-dialog-item 'icon-finder-name
                                                    (om-make-point 2 30) (om-make-point 50 20) 
                                                    (name self) 
                                                    :container frame
                                                    :font (get-view-font-icon self))))
     (setf (iconview frame) (om-make-dialog-item 'constant-ttybox
                                                 (om-make-point 2 2)
                                                 (om-make-point 50 27)
                                                 (format () "~S" (value self))
                                                 :container frame
                                                 :font (get-view-font-icon self)
                                                 ))
     (setf (object frame) self)
     frame))

;============================================================
;The simple frame for OMConstants
;============================================================

(omg-defclass constant-simple-frame (OMCompoundFrame)  
   ((nameview :initform nil :accessor nameview)
    (iconview :initform nil :accessor iconview)
    (change-name-p :initform nil :accessor change-name-p))
   (:documentation "This class implementes the frame used to constants.#enddoc#
#seealso# (boxframe icon-finder constant-ttybox) #seealso#
#nameview# The slot is a icon-finder-name with the slot name.#nameview#
#iconview# We replace the icon view by a constant-ttybox that show the value and allow edition.#iconview#
#change-name-p# non used #change-name-p#"))

(defmethod add-icon-finder ((self constant-simple-frame) container)
   (om-add-subviews container self))

(defmethod om-draw-contents :before ((self constant-simple-frame))
   (om-with-focused-view self 
       (om-with-fg-color self *om-light-gray-color*
         (om-fill-rect 0 0 (w self) (h self)))))

(defmethod om-view-click-handler ((self constant-simple-frame) position)
  (toggle-icon-active-mode self))

(defmethod om-drag-selection-p ((self constant-simple-frame) mouse-position) nil)

;============================================================
(omg-defclass constant-ttybox (ttybox om-static-text-drag) ()
   (:documentation "This class is a normal ttybox but used to show and edit slots values.#enddoc#
#seealso# (cons-enter-view) #seealso#"))



(defmethod open-ttybox-class ((self constant-ttybox)) 'cosnt-enter-view)
(defmethod initial-text-ttybox ((self constant-ttybox)) 
   (format () "~S" (value (object (om-view-container self)))))

(defmethod om-view-click-handler ((self constant-ttybox) position)
  (toggle-icon-active-mode (om-view-container self)))
;  (mapcar #'(lambda (frame) (when (subtypep (class-of frame) 'constant-simple-frame)
;                              (setf (selected-p (iconview frame)) nil))) (om-subviews (om-view-container (om-view-container self))))
;  (setf (selected-p self) t))

;Change the dialog-item-text and call propagate-changes
(defmethod set-value ((self constant-ttybox) value)
  (let ((Info (setfInfo (object (om-view-container self)))))
    (om-set-dialog-item-text self (if (not (stringp value))
                                      (format () "~D" value)
                                    (string-downcase value)))
     (setf (value (object (om-view-container self))) value)
     (propagate-changes (second info) (first info) value)
     (report-modifications (editor (om-view-container (om-view-container self))))
     ))

 
;============================================================
(omg-defclass cosnt-enter-view (edit-text-enter-view) ()
   (:documentation "This class allow edition of constant slots.#enddoc#
#seealso# (constant-ttybox) #seealso#")) 

;;;OOO
(defmethod exit-from-dialog ((self cosnt-enter-view) newtext)
   (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                            (setf (text-view (editor (om-view-container self))) nil)
                            (om-remove-subviews (om-view-container self) self)
                            (om-beep-msg "Error while setting the new value")
                            (om-abort))))    
     (let ((*package* (find-package :om))
           (newval (read-from-string newtext)))
       (set-value (object self) newval)
       (setf (text-view (editor (om-view-container self))) nil)
       (om-remove-subviews (om-view-container self) self)
       )))

;============================================


(defmethod save-edition-params ((self OMInstance))
   (save-value-params (instance self) (edition-params self)))
