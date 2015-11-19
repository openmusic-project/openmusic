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
;Icon interface for OMMethods
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defclass icon-method (OMAtomicFrame)   ; om-transparent-view 
   ((iconview :initform nil :accessor iconview)
    (nameview :initform nil :accessor nameview))
   (:documentation "This is the class used to visulisate OMMethods meta objects as a simple frame.
patches, folders, maquettes in the workspace are icon-finder instances.
in difference of OMContainerFrame this class is not graphic independent, 
so this class inherits from view. #enddoc#
#seealso# (OMMethod method-icon-view method-name-text-dialog ) #seealso#
#nameView# The name subview of the icon-method, it is a method-icon-view instance.#nameView#
#iconView# The icon subview of the icon-method, it is a method-name-text-dialog instance.#iconView#"))

(defmethod icon-method-p ((self icon-method)) t)
(defmethod icon-method-p ((self t)) nil)

(defmethod get-obj-for-info ((self icon-method)) (object self))

(defmethod fil ((self icon-method)) 0)

(defmethod delete-icon-finder ((self icon-method) container &optional (update t))
   (om-remove-subviews container self))

(defmethod omG-select :before ((self icon-method)) 
   "Select all subviews of 'self'."
  (when (not (active-mode self)) 
    (mapc #'(lambda (item)
              (setf (selected-p item) t)) (om-subviews self))))

(defmethod omG-unselect :before ((self icon-method)) 
   "Unselect all subviews of 'self'."
   (when (active-mode self)
     (mapc #'(lambda (item)
               (setf (selected-p item) nil)) (om-subviews self))))

(defmethod OMGMoveObject ((self icon-method) new-position)
   "I think that this method is never called;"
   (om-set-view-position self (borne-position new-position)))

;-------Drag and drop methods
(defmethod make-drag-region ((self icon-method) region x0 y0 view)
   (let* ((reg1 (om-new-region))
          (reg2 (om-new-region))
          (name (nameView self))
          (icon (iconView self))
          (x (- (x self) x0 (x view)))
          (y (- (y self) y0 (y view))))
     (om-set-rect-region reg1 (+ x (x icon)) (+ y (y icon)) (+ x (x+w icon)) (+ y (y+h icon)))
     (om-set-rect-region reg2  (+ x (x name))  (+ y  (y name)) (+ x (x+w name)) (+ y  (y+h name)))
     (setf region (om-union-region reg1 reg2))
     (om-dispose-region reg1)
     (om-dispose-region reg2))
   region)

;--------------------------
;icon view
;--------------------------

(defclass method-icon-view (icon-view om-view-drag) ())

(defmethod get-drag-object ((self method-icon-view)) (om-view-container self))

(defmethod om-drag-selection-p ((self method-icon-view) mouse-position)
  (declare (ignore mouse-position)) 
t)



;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view method-icon-view))
  (om-highlight-view (om-view-container view) t))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view method-icon-view)) 
  (om-highlight-view (om-view-container view) nil))

(defmethod get-pos-in-object ((self method-icon-view) where)
  (om-add-points (om-view-position self) where))

(defmethod om-view-click-handler ((self method-icon-view) where)
  (declare (ignore where))
    (call-next-method)
   (toggle-icon-active-mode (om-view-container self)))

(defmethod om-view-doubleclick-handler ((self method-icon-view) where)
    (declare (ignore where))
  (OpenObjectEditor (object (om-view-container self))))




;--------------------------
;icon name
;--------------------------

(defclass method-name-text-dialog (om-static-text-drag)  ())

(defmethod om-drag-selection-p ((self method-name-text-dialog) mouse-position)
  (declare (ignore mouse-position))
   t)

(defmethod get-drag-object ((self method-name-text-dialog)) (om-view-container self))

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view method-name-text-dialog))
  (om-highlight-view (om-view-container view) t))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view method-name-text-dialog)) 
  (om-highlight-view (om-view-container view) nil))


(defmethod get-pos-in-object ((self method-name-text-dialog) where)
  (om-add-points (om-view-position self) where))


;--------------

(defun make-icon-from-method (method position)
  (let* ((thespe (method-specializers method))
         (numtypes (length thespe))
         (index 0)
         (quali (method-qualifiers method))
         input-views module sizex name sizename help-str)
    (setf name (if quali (string+  (string-downcase (string (car quali))) 
                                      ":" (string-downcase (name method)))
                   (string-downcase (name method))))
    (setf help-str (format nil  "Method ~A~%(" name ))
    (setf sizename (get-name-size name))
    (setf sizex (apply #'max (list 24 (* 25 numtypes) sizename)))
    (loop while thespe do
          (let* ((sp (pop thespe))
                 (thetype (string (if (eql-specializer? sp) 
                                      (cadr sp)
                                    (class-name sp))))
                 omtype?)
            (setf help-str (string+ help-str thetype " "))
            (setf index (+ index 1))
            (setf omtype? (find-if #'(lambda (item) (string-equal (name item) thetype))
                                   (list+ *Basic-Lisp-Types* *OM-class-list*)))
            (cond (omtype? 
                   (push (om-make-view 'method-icon-view 
                                       :position (om-make-point (- (* index  (round sizex (+ numtypes 1))) 8) 1) 
                                       :size (om-make-point 16 16) 
                                       :help-spec thetype
                                       :iconID (icon omtype?)) input-views))
                  ((eql-specializer? sp)
                   (push (om-make-dialog-item 'om-static-text 
                                              (om-make-point (- (* index  (round sizex (+ numtypes 1))) 8) 0)
                                              (om-make-point 20 20) thetype
                                              :help-spec thetype
                                              :font (om-make-font "Courier" 8)
                                              ) input-views))
                  (t (push (om-make-view 'method-icon-view 
                                       :position (om-make-point (- (* index  (round sizex (+ numtypes 1))) 8) 1) 
                                       :size (om-make-point 16 16) 
                                       :iconID 180) input-views)))
            ))
    (setf help-str (string+ (subseq help-str 0 (- (length help-str) 1)) ")"))
    
    (setq module
          (om-make-view 'icon-method
            :position position 
            :size  (om-make-point sizex 44)
            :object  method
            :name name
            ))
    (setq input-views (nreverse input-views))
    (loop for input-f in input-views do (om-add-subviews module input-f))
    (om-add-subviews module (setf (iconview module)
                               (om-make-view 'method-icon-view
                                 :iconID (icon (fdefinition (method-name method)))
                                 :size (om-make-point 16 16)
                                 :position (om-make-point (- (round (w module) 2) 8) 16)
                                 :help-spec help-str)))
    (om-add-subviews module (setf (nameview module)
                                  (om-make-dialog-item 'method-name-text-dialog
                                                       (om-make-point (- (round (w module) 2) (round sizename 2)) 28 )
                                                       (om-make-point sizename 18)
                                                       name 
                                                       :font *om-default-font1b*
                                                       )))
    
    module))




(defmethod om-get-menu-context ((self icon-method)) 
  (list (list 
         (om-new-leafmenu "Open" #'(lambda () (open-icon-win self (om-make-point 0 0)))))
        (om-new-leafmenu "Get info" #'(lambda () (show-info-window (get-obj-for-info self))))))

(defmethod om-get-menu-context ((self method-icon-view)) (om-get-menu-context (om-view-container self)))

