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
;Editor for metaobjects without connections i.e. folders generic functions, workspaces, etc.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)


;============================
;SLOT
;============================
(omg-defclass slot-icon-frame (pckbrowser-icon-frame) ()
   (:documentation "The class of simple frames for OMSlot meta objects.#enddoc#
#seealso# (OMSlot) #seealso#"))

(omg-defclass slot-finder-icon (pckbrowser-icon) ())
(defmethod get-class-icon-icon ((self OMSlot)) 'slot-finder-icon)

(defvar *size-slot-icon-finder* nil)
(setf *size-slot-icon-finder* 20)

(defmethod change-icon-position ((self slot-icon-frame) x y)
  (setf (fil self) x)
  (setf (col self) y)
  (om-set-view-position self (om-make-point (+ 15 (* *x-offset-icon-finder* x))
                                            (* (+ 5 *size-slot-icon-finder*) y)))
  (change-sub-views-position self)
  )

(defmethod get-view-font-icon ((self omslot)) *om-default-font2*)

(defmethod set-size ((self slot-icon-frame) container)
   (let (x y sizex sizey)
     (om-set-view-size (iconView self) (om-make-point *size-slot-icon-finder* *size-slot-icon-finder*))
     (setf x (+ 15 (* *x-offset-icon-finder* (fil self))))
     (setf y (* (+ 5 *size-slot-icon-finder*) (col self)))
     (setf sizex (+ 5 *size-slot-icon-finder* (get-length-name (nameView self))))
     (setf sizey *size-slot-icon-finder*)
     (om-set-view-size self (om-make-point sizex sizey))
     (om-set-view-position self (om-make-point x y))
     (om-set-view-position (iconView self) (get-position-icon (iconView self)))
     (om-set-view-position (nameView self) (om-add-points (om-make-point 2 2) (get-position-name (nameView self))))
     (om-set-view-size (nameview self) (om-make-point (+ 4 (get-length-name (nameView self))) *size-slot-icon-finder*))))


(defmethod make-extra-info ((self slot-icon-frame) (object OMSlot))
   (setf (change-name-p self) (not (protected-p (find-class (classname object))))))

(defmethod open-icon-win ((self slot-icon-frame) where)
   "No editor for 'self'."
   (declare (ignore where))
   (om-beep) nil)


(defun change-allocation-from-frame (frame alloc)
   (let ((newslot (omG-change-allocation (object frame) alloc)))
     (when newslot
       (setf (object frame) newslot))))


(defun initformfromslot (slot)
   (let* ((classname (classname slot))
          (initorallocval (if (equal (alloc slot) ':instance) (theinitform slot)
                              (slot-value (make-instance classname) 
                                          (internp (name slot)  (symbol-package classname))))))
     (valued-val initorallocval)))
          
(defmethod add-extra-subviews ((self slot-icon-frame))
  (let* ((val (initformfromslot (object self)))

         (thearg (om-make-dialog-item 'om-check-box
                                      (om-make-point 10 (y self)) (om-make-point 14 12) "" 
                                      :di-action (om-dialog-item-act item
                                                          (omG-change-initarg (object self) (om-checked-p item)))
                                      :checked-p (io-p (object self))
                                      :enable (and (not (protected-p (object self)))
                                                   (not (equal (alloc (object self)) :class)))))
        
         (instORclass-but (om-make-dialog-item 'om-pop-up-dialog-item
                                            (om-make-point 140 14)
                                            (om-make-point 100 20)
                                            (string-downcase (string (alloc (object self))))
                                            :range '("Instance" "Class")
                                            :enable (not (protected-p (object self)))
                                            :value (if (equal (alloc (object self)) :class)
                                                       "Class" "Instance")
                                            :di-action (om-dialog-item-act item
                                                         (change-allocation-from-frame self (if (string-equal (om-get-selected-item item) "Class")
                                                                                                :class :instance))
                                                         (om-enable-dialog-item thearg (and (not (protected-p (object self)))
                                                                                            (not (equal (alloc (object self)) :class))))
                                                         (re-sort-slots (om-view-container self)))
                                            ))
         
         (theinitform 
          (if (omclass-p (class-of (class-of val)))
              (om-make-view 'initform-button 
                            :position (om-make-point 10 10) 
                            :size (om-make-point 16 16) 
                            :iconID (icon (class-of val))
                            :help-spec "double click to edit initform <command> to see"
                            :object self
                            :val val)
            (om-make-dialog-item 'initform-ttybox 
                                 (om-make-point 10 10) (om-make-point (min 200 (+ 5 (get-name-size (format () "~S" val))))  20) 
                                 (format () "~S" val) 
                                 :font *om-default-font2*
                                 :help-spec "double click to edit initform <command> to see"
                                 :object self :show-rect-p nil))))
    ;(if (equal (alloc (object self)) :class)
    ;    (om-set-part-color instORclass-item :BODY *om-light-gray-color*))
    (setf (newviews self) (list thearg instORclass-but theinitform ))))



(defmethod add-extras-infos ((self slot-icon-frame))
   (if (and (null (big-icon-p (editor (om-view-container  self))))
            (equal (type-of (editor (om-view-container  self))) 'classeditor))
     (let ((x (+ (x self) 100))
           (y (y self)))
       (if (equal (alloc (object self)) :class)
         (om-set-fg-color (nameView self) *om-light-gray-color*))
       (om-set-font (nameview self) *om-default-font2*)
       (set-size self (om-view-container self))
       (mapcar #'(lambda (new-view deltax)
                   (om-set-view-position new-view (om-make-point (incf x deltax) y))
                   (om-add-subviews (panel (om-view-container self)) new-view)) 
         (newviews self) '(20 40 120)))))

(defmethod change-sub-views-position ((self slot-icon-frame))
   (add-extras-infos self))

(defmethod omG-rename ((self slot-icon-frame) new-name)
   "Some names are not corrects (i.e. car)."
   (let ((oldname (name (object self)))
         (newslot (omG-rename (object self) new-name)))
     (if newslot
       (progn
         (setf (object self) newslot)
         (om-set-dialog-item-text (nameView self) new-name)
         (om-set-view-size (nameView self) (om-make-point (+ 5 (get-name-size new-name)) 20))
         (om-add-subviews self (nameView self))
         (set-size self (om-view-container self))
         (setf (name self) new-name)
         (object self))
       (progn
         (om-beep-msg "Bad slot name !")
         (setf (name (object self)) new-name)
         (omG-rename (object self) oldname) nil))))


;INITFORM FOR SLOTS

(omg-defclass initform-button (icon-view om-view-drop) 
   ((object :initform nil :initarg :object :accessor object)
    (val :initform nil :initarg :val :accessor val)
    (editorframe :initform nil :accessor editorframe)))

(defmethod slot-initform-p ((self initform-button)) t)
(defmethod slot-initform-p ((self t)) nil)

; Events
(defmethod om-view-click-handler ((self initform-button) where)
   (declare (ignore where)))
   ;;;(if (command-key-p) (om-set-help (not (om-help-on?)))))

(defmethod om-view-doubleclick-handler ((self initform-button) where)
   (declare (ignore where))
   (open-initform-editor self))


; Edition
(defmethod open-initform-editor ((self initform-button))
  (let ((container (editor (om-view-container self))))
    (if (protected-p (object container))
        (om-beep-msg "protected object")
      (if (initform-editor container)
          (om-beep-msg "Another initform editor is open, close it and try again")
        (if (Class-has-editor-p (val self))
            (setf (initform-editor container) (make-editor-window (get-editor-class (val self)) (val self) 
                                                                  (format nil "Initform for ~D slot" (name (object self)))
                                                                  self))
          (setf (initform-editor container) (open-instance-editor self)))))))


(defmethod open-instance-editor ((self initform-button))
  (let* ((ominstance (omNG-make-new-instance (val self) (name (object self))))
          (thewindow (make-editor-window 'InstanceEditor
                                        ominstance 
                                        (format nil "Initform for ~D slot" (name (object self))) self 
                                        :winpos (om-make-point 150 100)
                                        :winsize (om-make-point 250 280)))
         (slot-boxes (slots-inst-boxes ominstance (val self))))
    (setf (presentation (editor thewindow)) 0)
    (mapc #'(lambda (frame)
              (omG-add-element (panel (editor thewindow)) frame)) slot-boxes)
    (editor thewindow)))


(defmethod initform-from-val ((self t)) self)
(defmethod initform-from-val ((self ominstance)) (instance self))

(defmethod change-initform-ed ((self initform-button) newval)
  (handler-bind ((error #'(lambda (c) (declare (ignore c)) 
                            (om-beep)
                            (om-abort))))
    (let ((container (editor (om-view-container  self)))
          (*package* (find-package :om))
          (slot (object (object self))))
      (omG-change-initform slot (initform-from-val newval))
      (setf (initform-editor container) nil))))


; D&D
(defmethod om-drag-selection-p ((self initform-button) mouse-position)
  (declare (ignore mouse-position))
  nil)

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((self initform-button))
   (setf (selected-p self) t)
   (om-draw-contents self))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((self initform-button)) 
  (setf (selected-p self) nil)
  (om-draw-contents self))


(defmethod slot-menu-context ((self slot-icon-frame)) 
  (let* ((theslot (object self))
         (name (internp (name theslot) (symbol-package (classname theslot)))))
    (list (list 
           (om-new-leafmenu "Edit GET Method" #'(lambda () (open-get-slot (find-class (classname theslot) nil) name)))
           (om-new-leafmenu "Edit SET Method" #'(lambda () (open-set-slot (find-class (classname theslot) nil) name (thetype theslot)))))
          (om-new-leafmenu "Get Info" #'(lambda () (get-info-window (om-view-window self)))))))