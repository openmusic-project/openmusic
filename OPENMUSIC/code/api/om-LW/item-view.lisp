;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; INTERNAL SUBVIEW CLASSES OPTIMIZED FOR DRAWING
;DocFile
;;===========================================================================

(export '(om-item-view om-internal-view) :om-api)

(in-package :oa)

;;; no subview
;;; optimized display
(defclass om-item-view (om-graphic-object capi::pinboard-object) 
  ((item-container :initarg :item-container :accessor item-container :initform nil)
   (item-x  :initarg :item-x :accessor item-x :initform 0)
   (item-y  :initarg :item-y :accessor item-y :initform 0)))
;  (:default-initargs :display-callback 'item-draw-callback)

(defmethod initialize-instance :after ((self om-item-view) &rest args)
  (om-create-callback self))

(defmethod om-item-view-p ((self t)) nil)
(defmethod om-item-view-p ((self om-item-view)) t)


#+cocoa
(defclass om-internal-view (om-transparent-view) ())
#-cocoa
(defclass om-internal-view (om-item-view) ())


(defmethod capi::pane-has-focus-p ((self om-item-view)) nil)

(defmethod update-for-subviews-changes ((self om-item-view) &optional (recursive nil)) 
  (when (and (item-container self) (initialized-p (item-container self)))
    (update-for-subviews-changes (item-container self) nil)))

(defmethod internal-add-subview ((self om-view) (subview om-item-view))
  (call-next-method)
  (setf (item-container subview) self)
  (setf (item-subviews self) (append (item-subviews self) (list subview)))
  (update-po-position subview)
  (mapcar #'(lambda (sv) (po-add-subview subview sv)) (vsubviews subview)))


(defmethod internal-add-subview ((self om-abstract-window) (subview om-item-view))
  (call-next-method)
  (setf (item-container subview) (pane-layout self))
  (setf (item-subviews (pane-layout self)) (append (item-subviews (pane-layout self)) (list subview)))
  (mapcar #'(lambda (sv) (po-add-subview subview sv)) (vsubviews subview)))

(defmethod internal-add-subview ((self om-item-view) (subview om-item-view))
  (call-next-method)
  (po-add-subview self subview))

;;; recursively set the top-level layout for pinboard-objects
(defmethod po-add-subview ((self om-item-view) (subview om-item-view))
  (setf (item-container subview) (item-container self))
  (when (item-container self)
    (setf (item-subviews (item-container self))
	  (append (item-subviews (item-container self))
		  (list subview)))
    (update-po-position subview)
    )
  (mapcar #'(lambda (sv) (po-add-subview subview sv)) (vsubviews subview)))


(defmethod internal-remove-subview ((self om-view) (subview om-item-view))  
  (mapcar #'(lambda (sv) (po-remove-subview subview sv)) (vsubviews subview))
  (setf (item-container subview) nil)
  (setf (item-subviews self) (remove subview (item-subviews self)))
  (capi::apply-in-pane-process self
                               (lambda () (capi::manipulate-pinboard self subview :delete)))
  (call-next-method))

(defmethod internal-remove-subview ((self om-abstract-window) (subview om-item-view))  
  (mapcar #'(lambda (sv) (po-remove-subview subview sv)) (vsubviews subview))
  (setf (item-container subview) nil)
  (setf (item-subviews (pane-layout self)) (remove subview (item-subviews (pane-layout self))))
  (capi::apply-in-pane-process (pane-layout self)
                               (lambda () (capi::manipulate-pinboard (pane-layout self) subview :delete)))
  (call-next-method))

(defmethod internal-remove-subview ((self om-item-view) (subview om-item-view))
  (po-remove-subview self subview)
  (call-next-method))

(defmethod po-remove-subview ((self om-item-view) (subview om-item-view))
  (mapcar #'(lambda (sv) (po-remove-subview subview sv)) (vsubviews subview))
  (setf (item-subviews (item-container self)) (remove subview (item-subviews (item-container self))))
  (capi::apply-in-pane-process (item-container self)
                               (lambda () (capi::manipulate-pinboard (item-container self) subview :delete)))
  (setf (item-container subview) nil))



(defmethod om-subviews ((self om-item-view)) 
  (vsubviews self))

;;; (capi::highlight-pinboard-object (item-container self) self t)
(defmethod om-create-callback ((self om-item-view)) nil)
  ;(setf (initialized-p self) t)


(defun update-po-position (self)
  (when (and (vcontainer self) (item-container self))
    (capi::apply-in-pane-process (item-container self)
                                 (lambda ()
                                   (let ((abs-pos (om-convert-coordinates (om-view-position self) (vcontainer self) (item-container self))))
                                     (setf (item-x self) (om-point-h abs-pos)
				     	   (item-y self) (om-point-v abs-pos))
                                     (setf (capi::pinboard-pane-position self)
				     	   (values (item-x self) (item-y self)))
                                     (capi::set-hint-table self (list :x (om-point-h abs-pos) :y (om-point-v abs-pos)
                                                                      :visible-min-width (vw self) :visible-min-height (vh self)
                                                                      :visible-max-width t :visible-max-height t))
                                     (mapc 'update-po-position (vsubviews self))
				     ))
                                 )))


(defmethod om-set-view-position ((self om-item-view) pos-point) 
  (setf (vx self) (om-point-h pos-point)
        (vy self) (om-point-v pos-point))  
  (update-po-position self)
  ;(when (item-container self) (om-invalidate-view (item-container self)))
  )

;; pinboard-object-at-position

(defmethod om-view-position ((self om-item-view))
  (om-make-point (vx self) (vy self)))

(defmethod om-set-view-size ((self om-item-view) size-point)
  ;(capi::resize-pinboard-object self :width (om-point-h size-point) :height (om-point-v size-point))
  (setf (vw self) (om-point-h size-point))
  (setf (vh self) (om-point-v size-point))
  ;;(setf *om-locked-draw* t)
    (setf (capi::pinboard-pane-size self) (values (om-point-h size-point) (om-point-v size-point)))
    (capi::set-hint-table self (list :visible-min-width (vw self) :visible-min-height (vh self)
  ;                           ;:default-width (vw self) :default-height (vh self)
                                     :visible-max-width t :visible-max-height t))
    ;;(setf *om-locked-draw* nil)
    )


(defmethod om-view-size ((self om-item-view))
  (om-make-point (vw self) (vh self)))

;(defmethod om-invalidate-view ((self om-graphic-object) &optional (erase t))
;  (when (interface-visible-p self)
;   (gp::invalidate-rectangle (om-get-view self))))

;;; GETS THE REAL VIEW WHERE WE CAN DRAW IN ETC.
(defmethod om-get-view ((self om-item-view)) 
  (or 
   (capi::pinboard-object-pinboard self)
   (item-container self)))

;;; GET THE VIEW WE SHOULD USE IN INTERACTIONS
(defmethod om-get-real-view ((self om-item-view)) self)
(defmethod om-get-real-view ((self t)) (om-get-view self))

 
(defmethod om-set-bg-color ((self om-item-view) color)
  (let ((col (when color (c color))))
    (setf (capi::pinboard-object-graphics-arg self :background) (if (equal col :transparent) nil col))
    (capi:redraw-pinboard-object self)))
      
(defmethod om-get-bg-color ((self om-item-view))
  (make-instance 'omcolor :c (capi::pinboard-object-graphics-arg self :background)))

(defmethod om-set-fg-color ((self om-item-view) color)
  (let ((col (when color (c color))))
    (setf (capi::pinboard-object-graphics-arg self :foreground) col)
    (capi:redraw-pinboard-object self)))
      
(defmethod om-get-fg-color ((self om-item-view))
  (make-instance 'omcolor :c (capi::pinboard-object-graphics-arg self :foreground)))

(defmethod om-get-font ((self om-item-view))
  (let ((font (capi::pinboard-object-graphics-arg self :font)))
    (when font 
      (if (gp::font-description-p font) font
        (gp::font-description font))
      )))

(defmethod om-set-font ((self om-item-view) font) 
  #-linux (setf (capi::pinboard-object-graphics-arg self :font) font)
  #+linux (setf (capi::pinboard-object-graphics-arg self :font) (or font *om-default-font1*))
  (capi:redraw-pinboard-object self))



