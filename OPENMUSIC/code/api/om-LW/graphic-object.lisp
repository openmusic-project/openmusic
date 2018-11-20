;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;;===========================================================================
;DocFile
; GRAPHIC OBJECT SUPERCLASS AND METHODS 
;DocFile
;;===========================================================================

(in-package :om-api)

;;;=====================
;;; export :
;;;=====================
(export '(
          omg-defclass     
          om-graphic-object
          om-width
          om-height
          om-get-help-spec
          om-view-size
          om-view-position
          om-set-view-size
          om-set-view-position
          om-change-view-position
          om-set-bg-color
          om-get-bg-color
          om-set-fg-color
          om-get-fg-color
          om-get-font
          om-set-font
          om-get-view-name
          om-view-window
          om-view-container
          om-subviews
          om-add-subviews
          om-remove-subviews
          om-with-delayed-update
          om-view-set-help
         
          ) :om-api)
;;;=====================

;;;=======================
;;; GENERAL GRAPHIC OBJECT
;;;=======================
;;; EQUIVALENT SIMPLE-PANE IN LISPWORK/CAPI
;;; manages general callbacks and behaviors

;;; GTK+ Compatibility
;;; used to instanciate om-graphic-object subclasses
(defmacro omg-defclass  (Name Superclass Slots &rest Class-Options)
  `(defclass ,name ,superclass ,slots ,.class-options))


(defclass om-graphic-object ()
  ((vcontainer :initform nil :initarg :vcontainer :accessor vcontainer)
   (vsubviews :initform nil :initarg :vsubviews :accessor vsubviews)
   (locked :initform nil :initarg :locked :accessor locked)
   (vx :initform 0 :initarg :vx :accessor vx)
   (vy :initform 0 :initarg :vy :accessor vy)
   (vw :initform 32 :initarg :vw :accessor vw)
   (vh :initform 32 :initarg :vh :accessor vh)
   (help-spec :initform nil :initarg :help-spec :accessor help-spec)
   (initialized-p :initform nil :accessor initialized-p)
   (highlight :initform nil :accessor highlight)
   (temp-data :initform nil :accessor temp-data)
   (images :initarg :images :accessor images :initform nil))
  (:default-initargs 
   :create-callback 'om-create-callback
   :geometry-change-callback 'om-resize-callback
   :destroy-callback 'om-destroy-callback
   :input-model '(
		  (:post-menu om-context-menu-callback)
                   
		  ((:button-1 :motion :shift #+macosx :hyper #-macosx :control :meta)  om-clic-motion-callback (:shift :alt :cmd))
		  ((:button-1 :motion :shift #+macosx :hyper #-macosx :control)  om-clic-motion-callback (:shift :cmd))
		  ((:button-1 :motion :shift :meta)  om-clic-motion-callback (:shift :alt))
		  ((:button-1 :motion :meta #+macosx :hyper #-macosx :control)  om-clic-motion-callback (:alt :cmd))
		  ((:button-1 :motion :shift)  om-clic-motion-callback (:shift))
		  ((:button-1 :motion #+macosx :hyper #-macosx :control)  om-clic-motion-callback (:cmd))
		  ((:button-1 :motion :meta)  om-clic-motion-callback (:alt))
		  ((:button-1 :motion)  om-clic-motion-callback nil)
                   
		  ((:button-1 :press :shift #+macosx :hyper #-macosx :control :meta) om-clic-callback (:shift :alt :cmd))
		  ((:button-1 :press :shift #+macosx :hyper #-macosx :control) om-clic-callback '(:shift :cmd))
		  ((:button-1 :press :shift :meta) om-clic-callback (:shift :alt))
		  ((:button-1 :press :meta #+macosx :hyper #-macosx :control) om-clic-callback (:alt :cmd))
		  ((:button-1 :press :shift) om-clic-callback (:shift))
		  ((:button-1 :press #+macosx :hyper #-macosx :control) om-clic-callback (:cmd))
		  ((:button-1 :press :meta) om-clic-callback (:alt))
		  ((:button-1 :press) om-clic-callback nil)
                   
                   
		  ((:motion :shift #+macosx :hyper #-macosx :control) om-motion-callback (:shift :cmd))
		  ((:motion :shift) om-motion-callback (:shift))
		  ((:motion #+macosx :hyper #-macosx :control) om-motion-callback (:cmd))
		  (:motion om-motion-callback nil)
                   
		  ((:button-1 :release :shift #+macosx :hyper #-macosx :control :meta)  om-clic-release-callback (:shift :alt :cmd))
		  ((:button-1 :release :shift #+macosx :hyper #-macosx :control)  om-clic-release-callback (:shift :cmd))
		  ((:button-1 :release :shift :meta)  om-clic-release-callback (:shift :alt))
		  ((:button-1 :release :meta #+macosx :hyper #-macosx :control)  om-clic-release-callback (:alt :cmd))
		  ((:button-1 :release :shift)  om-clic-release-callback (:shift))
		  ((:button-1 :release #+macosx :hyper #-macosx :control)  om-clic-release-callback (:cmd))
		  ((:button-1 :release :meta)  om-clic-release-callback (:alt))
		  ((:button-1 :release)  om-clic-release-callback nil)
                   
		  ;; test
		  ((:button-3 :release)  om-clic-release-callback nil)
                   
		  ((:button-1 :second-press :shift #+macosx :hyper #-macosx :control :meta) om-double-clic-callback (:shift :alt :cmd))
		  ((:button-1 :second-press :shift #+macosx :hyper #-macosx :control) om-double-clic-callback (:shift :cmd))
		  ((:button-1 :second-press :shift :meta) om-double-clic-callback (:shift :alt ))
		  ((:button-1 :second-press :meta #+macosx :hyper #-macosx :control) om-double-clic-callback (:alt :cmd))
		  ((:button-1 :second-press :shift ) om-double-clic-callback (:shift))
		  ((:button-1 :second-press #+macosx :hyper #-macosx :control) om-double-clic-callback (:cmd))
		  ((:button-1 :second-press :meta) om-double-clic-callback (:alt))
		  ((:button-1 :second-press) om-double-clic-callback nil)
                  
                  ;((:button-3 :press) om-right-clic-callback)
                   
		  (:gesture-spec om-char-spec-callback)
                  ;(:character  om-char-callback nil)
		  )
   ))
 
(defmethod om-get-help-spec ((self om-graphic-object))  (help-spec self))

(defmethod om-create-callback (self))
(defmethod om-destroy-callback (self))
(defmethod om-draw-contents-callback (self x y w h))
(defmethod om-resize-callback (self x y w h))

(defmethod om-clic-callback (self x y mods))
(defmethod om-clic-release-callback (self x y mods))
(defmethod om-motion-callback (self x y mods) t)
(defmethod om-clic-motion-callback (self x y mods))
(defmethod om-double-clic-callback (self x y mods))
;;;(defmethod om-right-clic-callback (self x y))

(defmethod om-char-callback (self x y c mods))
(defmethod om-char-spec-callback (self x y spec))

(defmethod om-context-menu-callback (self x y))


(defun maybe-call-update (graphic-obj)
  (when (and (initialized-p graphic-obj) (not (locked graphic-obj))) ;; (interface-visible-p graphic-obj)
    (update-for-subviews-changes graphic-obj t)))
  
(defmethod update-for-subviews-changes ((self om-graphic-object) &optional (recursive nil)) nil)

(defmacro om-with-delayed-update (view &body body)
   `(progn 
      (setf (locked ,view) t)
      ,@body
      (setf (locked ,view) nil)
      (maybe-call-update ,view)
      ))

(defmethod om-add-subviews ((self om-graphic-object) &rest subviews)
  "Adds subviews to a graphicbject"
  (loop for item in subviews do (internal-add-subview self item))
  (maybe-call-update self))

(defmethod internal-add-subview ((self om-graphic-object) (subview om-graphic-object))
  (setf (vcontainer subview) self)
  (setf (vsubviews self) (append (vsubviews self) (list subview)))
  )

(defmethod om-remove-subviews ((self om-graphic-object) &rest subviews)
  "Removes subviews from graphic object"
    (capi::apply-in-pane-process 
     (om-get-view self) 
     #'(lambda ()
         (loop for item in (remove nil subviews) do (internal-remove-subview self item))
         (maybe-call-update self)))
    )

(defmethod internal-remove-subview ((self om-graphic-object) (subview om-graphic-object))
  (setf (vcontainer subview) nil)
  (setf (vsubviews self) (remove subview (vsubviews self))))



;;; different pour om-window
(defmethod om-get-view ((self om-graphic-object))  (or *default-printer-port* self))

(defmethod om-subviews ((self om-graphic-object)) 
  (vsubviews self))


;;; for windows, skip default window layout
(defmethod container-skip-layout ((self t)) self)

(defmethod om-view-container ((self om-graphic-object))
 (container-skip-layout (vcontainer self))
 ;(container-skip-layout (or (vcontainer self) (capi::element-parent self)))
 )

(defun rec-top-level (obj)
  (if (om-view-container obj) (rec-top-level (om-view-container obj)) obj))

(defmethod om-view-window ((self om-graphic-object))
  (let ((rep (capi::top-level-interface self)))
    (or rep (rec-top-level self))
    ;(while (null rep) (setf rep (capi::top-level-interface self)))
    ;rep
    ))

(defmethod om-width ((item om-graphic-object)) (om-point-h (om-view-size item)))
(defmethod om-height ((item om-graphic-object)) (om-point-v (om-view-size item)))

(defmethod om-interior-size ((self om-graphic-object)) (om-view-size self))

(defmethod om-resize-callback ((self om-graphic-object) x y w h)
  (setf (vx self) x (vy self) y (vw self) w (vh self) h))

(defmethod om-get-view-name ((self om-graphic-object))
  (capi::capi-object-name self))

(defmethod om-set-bg-color ((self om-graphic-object) color)
  (let ((col (when color (c color))))
    #-cocoa 
    (if (and col (equal col :transparent) (om-view-container self))
        ;;;(c (om-get-bg-color (om-view-container self)))
        ;;; (setf col (c (om-get-bg-color (om-view-container self))))
        nil ;;; do nothing...
      (setf (simple-pane-background (om-get-view self)) col))
    #+cocoa
    (setf (simple-pane-background (om-get-view self)) col)))

(defmethod om-get-bg-color ((self om-graphic-object))
  (let ((c (simple-pane-background (om-get-view self))))
    (when c (make-instance 'omcolor :c c))))

(defmethod om-set-fg-color ((self om-graphic-object) color)
  (let ((col (when color (c color))))
    (setf (simple-pane-foreground (om-get-view self)) col)))
      
(defmethod om-get-fg-color ((self om-graphic-object))
  (make-instance 'omcolor :c (simple-pane-foreground (om-get-view self))))

(defmethod om-get-font ((self om-graphic-object))
  (let ((font (capi::simple-pane-font self)))
    (when font 
      (if (gp::font-description-p font) font
        (gp::font-description font)))))

(defmethod om-set-font ((self om-graphic-object) font) 
  (setf (capi::simple-pane-font self) font))

(defmethod om-view-set-help ((self om-graphic-object) (text string))
  (setf (help-spec self) text))


