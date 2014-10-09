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
; OM-VIEW CLASSES : container panes for GUI windows
;DocFile
;;===========================================================================

(export '(

          om-view
          om-make-view
          om-transparent-view
          om-scroller
          om-field-size
          om-set-field-size
          om-scroll-position
          om-set-scroll-position
          om-h-scroll-position
          om-v-scroll-position
          om-view-scrolled
          om-view-contains-point-p
          om-find-view-containing-point
          om-convert-coordinates
          om-view-origin) :om-api)



(in-package :oa)


;;;======================
;;; VIEW
;;; General graphic pane
;;;======================

(defclass om-view (om-graphic-object capi::pinboard-layout)
  ((item-subviews :initarg :item-subviews :initform nil :accessor item-subviews)
   (main-pinboard-object :accessor main-pinboard-object :initarg :main-pinboard-object :initform nil))
  (:default-initargs
   ;; #+win32 :draw-pinboard-objects #+win32  :once ;:local-buffer :buffer :once 
   ;; #+win32 :draw-with-buffer #+win32 t
   :highlight-style :standard
   :scroll-if-not-visible-p nil
   :fit-size-to-children nil
   ))

(defmethod om-view-p ((self om-view)) t)
(defmethod om-view-p ((self t)) nil)

;;; background pinboard for drawing on the view
;;; must keep the same size as the view
(defclass om-view-pinboard-object (capi::pinboard-object) ())

(defmethod om-get-view ((self om-view-pinboard-object)) (capi::element-parent self))
;;; GET THE VIEW WE SHOULD USE IN INTERACTIONS
(defmethod om-get-real-view ((self om-view-pinboard-object)) (capi::element-parent self))

(defmethod set-layout ((view om-view))
  (setf (capi:layout-description view)
        (remove-duplicates (remove nil (cons (main-pinboard-object view)
                                             (append (vsubviews view) (item-subviews view)))))))

(defmethod set-layout ((view t)) nil)

(defmethod update-for-subviews-changes ((self om-view) &optional (recursive nil))
  (apply-in-pane-process self (lambda () (set-layout self)))
  (when recursive (mapc #'(lambda (view) (if (om-view-p view) (update-for-subviews-changes view t)))
                        (vsubviews self))))

(defmacro om-make-view (class &rest attributes
                        &key (position (om-make-point 0 0)) (size (om-make-point 32 32))
			owner subviews name font bg-color scrollbars retain-scrollbars field-size 
                        &allow-other-keys)
  `(let*  ((x (om-point-h ,position))
           (y (om-point-v ,position))
           (w (om-point-h ,size))
           (h (om-point-v ,size))
           (fw (if ,field-size (om-point-h ,field-size) 0))
           (fh (if ,field-size (om-point-v ,field-size) 0))
           (view (make-instance ,class
                               :name ,name
                               ;:x x :y y ;; for pinboard-objects
                               :width nil :height nil
                               :default-x x :default-y y :default-width w :default-height h
                               :visible-min-width nil :visible-min-height nil
                               :horizontal-scroll (or (equal ,scrollbars t) (equal ,scrollbars :h))
                               :vertical-scroll (or (equal ,scrollbars t) (equal ,scrollbars :v))
                               :scroll-width fw :scroll-height fh
                               :accepts-focus-p t
                               :vx x :vy y :vw w :vh h
                               :vcontainer ,owner
                               :allow-other-keys t
                               #+linux :foreground #+linux :black
                               ,.attributes
                               )))
     (when ,bg-color (om-set-bg-color view ,bg-color))
     (when ,owner (om-add-subviews ,owner view))
     (when ,subviews (mapc (lambda (sv) (om-add-subviews view sv)) ,subviews))
     
     (when (om-view-p view) 
       #+(or win32 linux) (unless (or ,bg-color (simple-pane-background view)) (om-set-bg-color view *om-white-color*))
       #+(or win32 linux) (setf (main-pinboard-object view) (make-instance 'om-view-pinboard-object))
       #+(or win32 linux) (setf (capi::pinboard-pane-size (main-pinboard-object view)) (values w h))
       #+cocoa (setf (capi::output-pane-display-callback view) 'om-draw-contents-callback)
       )
     (when (scroller-p view) 
       (setf (capi::simple-pane-scroll-callback view) 'scroll-update)
       (setf (fw view) fw (fh view) fh))
     (when (om-item-view-p view) 
       (om-set-font view (or ,font *om-default-font1*)))
     view))

(defmethod om-create-callback ((self om-view))
  (update-for-subviews-changes self nil)
  (setf (initialized-p self) t))

(defmethod om-set-view-position ((self om-graphic-object) pos-point) 
  (capi:apply-in-pane-process self #'(lambda ()
                                       (setf (pinboard-pane-position self) (values (om-point-h pos-point) (om-point-v pos-point)))))
  ;(set-hint-table self (list :default-x (om-point-h pos-point) :default-x (om-point-v pos-point))))
  (setf (vx self) (om-point-h pos-point)
        (vy self) (om-point-v pos-point)))

(defmethod om-view-position ((self om-graphic-object))
  (if (capi::interface-visible-p self)
      (capi:apply-in-pane-process self #'(lambda ()
             (multiple-value-bind (x y) (pinboard-pane-position self)
               (setf (vx self) x) (setf (vy self) y)))))
  (om-make-point (vx self) (vy self)))

(defmethod om-set-view-size ((self om-graphic-object) size-point) 
  (let ((w (or (om-point-h size-point) (vw self)))
        (h (or (om-point-v size-point) (vh self))))
  (capi:apply-in-pane-process self 
                              #'(lambda ()                                  
                                  (setf (pinboard-pane-size self) (values  w h))
                                  #+win32(setf (pinboard-pane-size (main-pinboard-object self)) (values w h))
                                  ))
  ; (set-hint-table self (list :default-width (om-point-h size-point) :default-height (om-point-v size-point))))
  (setf (vw self) w)
  (setf (vh self) h)))

(defmethod om-view-size ((self om-graphic-object)) 
  (if (interface-visible-p self)
      (capi:apply-in-pane-process self #'(lambda ()
                (multiple-value-bind (w h) (pinboard-pane-size self)
                  (setf (vw self) w)
                  (setf (vh self) h)))))
    (om-make-point (vw self) (vh self)))



;;;======================
;;; OM-TRANSPARENT-VIEW
;;;======================
(defclass om-transparent-view (om-view) ()
  (:default-initargs :background :transparent))

#-cocoa
(defmethod (setf vcontainer) :around ((cont om-graphic-object) (view om-transparent-view)) 
  (call-next-method)
  (om-set-bg-color view (om-get-bg-color cont))
  (mapc #'(lambda (v) (setf (vcontainer v) view)) (om-subviews view)))

#-cocoa
(defmethod (setf vcontainer) :around ((cont om-graphic-object) (view om-view)) 
  (call-next-method)
  (when (or (null (om-get-bg-color view)) (equal :transparent (c (om-get-bg-color view))))
    (om-set-bg-color view (om-get-bg-color cont))
    (mapc #'(lambda (v) (setf (vcontainer v) view)) (om-subviews view))))


;;;======================
;;; OM-SCROLLER
;;;======================
(defclass om-scroller (om-view) 
   ((fw :initform 0 :initarg :fw :accessor fw)
    (fh :initform 0 :initarg :fh :accessor fh)
    (scrollbars :initform t :initarg :scrollbars :accessor scrollbars)
    (retain-scrollbars :initform nil :initarg :retain-scrollbars :accessor retain-scrollbars)
    (displayed-p :initform nil :initarg :displayed-p :accessor displayed-p)
    )
   ;(:default-initargs :simple-pane-scroll-callback 'scroll-update)
   (:default-initargs :background :white)
   )

;;; :simple-pane-scroll-callback
(defmethod scroll-update ((self om-scroller) dimension operation pos-list &rest options)
  ;(print (list dimension operation pos-list))
  (case dimension
    (:vertical (setf pos-list (list 0 pos-list)))
    (:horizontal (setf pos-list (list pos-list 0)))
    (otherwise nil))
  (case operation
    (:move 
     (when (and (car pos-list) (cadr pos-list)) 
       (let ((x (om-h-scroll-position self))
             (y (om-v-scroll-position self)))
         (om-invalidate-rectangle self x y (vw self) (vh self))
         #+(or linux win32) (setf (pinboard-pane-position (main-pinboard-object self)) 
				  (values x y))
         ))
     (om-view-scrolled self (car pos-list) (cadr pos-list)))
    #+(or win32 linux) (:step
			(setf (pinboard-pane-position (main-pinboard-object self)) 
			      (values (om-h-scroll-position self) (om-v-scroll-position self)))
			(om-view-scrolled self (om-h-scroll-position self) (om-h-scroll-position self))
			;;(om-invalidate-view self)
			)
    #+(or win32 linux) (otherwise ;; :drag
			(setf (pinboard-pane-position (main-pinboard-object self)) 
			      (values (om-h-scroll-position self) (om-v-scroll-position self)))
			(om-view-scrolled self (car pos-list) (cadr pos-list))
			;;(om-invalidate-view self)
			)
    ))

(defmethod om-view-scrolled ((self om-scroller) x y) nil)

(defmethod om-view-size ((self om-scroller)) 
  (call-next-method))

(defmethod om-set-view-size ((self om-scroller) size-point) 
  (capi:apply-in-pane-process self  #'(lambda ()
       (setf (pinboard-pane-size self) (values (om-point-h size-point) (om-point-v size-point)))
       ;; pas la peine ??
       #+win32(setf (pinboard-pane-size (main-pinboard-object self)) (values (om-point-h size-point) (om-point-v size-point)))
       ))
  (setf (vw self) (om-point-h size-point))
  (setf (vh self) (om-point-v size-point)))

(defmethod scroller-p ((self om-scroller)) t)
(defmethod scroller-p ((self t)) nil)

(defmethod om-field-size ((self om-scroller))
  (if (and (interface-visible-p self) (not (locked self)))
      (om-make-point (or (capi::get-horizontal-scroll-parameters self :max-range) (vw self))
                     (or (capi::get-vertical-scroll-parameters self :max-range) (vh self)))
    (om-make-point (fw self) (fh self))))

(defmethod om-field-size ((self om-graphic-object))
  (om-view-size self))

(defmethod om-set-field-size ((self om-scroller) size)
  (setf (fw self) (om-point-h size) (fh self) (om-point-v size))
  ;(print (list self (om-point-v size) (om-height self)))
  (capi:apply-in-pane-process self #'(lambda ()
                                       (when (capi::simple-pane-horizontal-scroll self)
                                         (capi::set-horizontal-scroll-parameters self :min-range 0 :max-range (om-point-h size)))
                                       (when (capi::simple-pane-vertical-scroll self)
                                         (capi::set-vertical-scroll-parameters self :min-range 0 :max-range (om-point-v size)))))
  t)

#|
(capi::contain 
 (let ((pl (make-instance 'capi::pinboard-layout
                              :vertical-scroll t
                              :horizontal-scroll nil
                              :scroll-width 200 :scroll-height 200)))
   (capi:apply-in-pane-process pl #'(lambda ()
                                  (capi::set-horizontal-scroll-parameters pl :min-range 0 :max-range 200)
                                  (capi::set-vertical-scroll-parameters pl :min-range 0 :max-range 200)
                                  ))
   pl))
|#

(defmethod om-scroll-position ((self om-scroller))
  (om-make-point (om-h-scroll-position self) (om-v-scroll-position self)))

(defmethod om-set-scroll-position ((self t) pos) nil)

(defmethod om-set-scroll-position ((self om-scroller) pos)
  (capi::apply-in-pane-process 
   self
   'capi::scroll self :pan :move 
   (list (om-point-h pos) (om-point-v pos))))

(defmethod om-h-scroll-position ((self om-scroller))
  (or (capi::get-horizontal-scroll-parameters self :slug-position) 0))

(defmethod om-v-scroll-position ((self om-scroller))
  (or (capi::get-vertical-scroll-parameters self :slug-position) 0))

;;; au cas ou...
(defmethod om-h-scroll-position ((self om-graphic-object)) 0)
(defmethod om-v-scroll-position ((self om-graphic-object)) 0)
(defmethod om-scroll-position ((self om-graphic-object)) (om-make-point 0 0))


(defmethod update-for-subviews-changes ((self om-scroller) &optional (recursive nil)) 
  (if (initialized-p self)
      (capi::apply-in-pane-process self (lambda () 
					(let ((v (capi::get-vertical-scroll-parameters self :max-range))
                                              (h (capi::get-horizontal-scroll-parameters self :max-range))
                                              (x (capi::get-horizontal-scroll-parameters self :slug-position))
                                              (y (capi::get-vertical-scroll-parameters self :slug-position)))
                                          (set-layout self)
                                          (capi::set-vertical-scroll-parameters self :min-range 0 :max-range v)
                                          (capi::set-horizontal-scroll-parameters self :min-range 0 :max-range h)
                                          (capi::scroll self :pan :move (list x y))
                                          )))
    (apply-in-pane-process self (lambda () (set-layout self))))
    (when recursive (mapc #'(lambda (view) (if (om-view-p view) (update-for-subviews-changes view t))) (vsubviews self)))
    )

;;; pour faire appaitre les scrollbars
;;; !!! plus appelé
(defmethod om-draw-contents-callback :before ((self om-scroller) x y w h)
  (unless (displayed-p self)
    (om-window-resized (om-view-window self) (om-view-size (om-view-window self)))
    (setf (displayed-p self) t)))

;;;(om-subtract-points (om-view-size self) (om-make-point 30 30))

#+(or win32 linux)
(defmethod om-interior-size ((self om-scroller))
  (om-subtract-points 
   (om-view-size self)
   (om-make-point (if (and (capi::simple-pane-vertical-scroll self) 
                           (> (capi::get-vertical-scroll-parameters self :max-range) (vh self)))  17 0)
                  (if (and (capi::simple-pane-horizontal-scroll self)
                           (> (capi::get-horizontal-scroll-parameters self :max-range) (vw self))) 17 0))))

;;;======================
;;; TOOLS
;;;======================
(defmethod om-view-contains-point-p ((view om-graphic-object) point)
  (let* ((x (om-point-h point))
        (y (om-point-v point))
        (brx  (om-width view))
        (bry  (om-height view))
        (vpos (om-view-position view))
        (rx  (+ (om-point-h vpos) (om-h-scroll-position view)))
        (ry  (+ (om-point-v vpos) (om-v-scroll-position view)))
        )
    (and (> x rx) (> y ry) (< x (+ brx rx)) (< y (+ bry ry)))))

(defun om-find-view-containing-point (view point &optional (recursive t))
  (if view 
      (let ((subviews (om-subviews view)))
	(do ((i (- (length subviews) 1) (- i 1)))
            ((< i 0))
          (let ((subview (nth i subviews)))
            (when (om-view-contains-point-p subview point)
	      (return-from om-find-view-containing-point
                (progn
		  (when recursive (om-find-view-containing-point subview (om-convert-coordinates point view subview))))
                ))))
        view)))

(defun om-convert-coordinates (point view1 view2)           
  (if (and view1 view2)
       (om-add-points point
                      (om-subtract-points (om-view-origin view2)
                                          (om-view-origin view1)))
   (progn
      (print (format nil "Warning: Can not convert position with NULL views: ~A, ~A." view1 view2))
      point)))
  
;  (if (and (initialized-p view1) (initialized-p view2))
;    (multiple-value-bind (x y)
;        (capi::convert-relative-position view1 view2 (om-point-h point) (om-point-v point))
;      (om-make-point x y))
;    (om-add-points point
;                   (om-subtract-points (om-view-origin view2)
;                                       (om-view-origin view1))))

(defun om-view-origin (view)
   (let ((container (om-view-container view)))
     (if container
       (let ((position (om-view-position view))
             (container-origin (om-view-origin container)))
         (if position
	     (om-subtract-points container-origin position)
           container-origin))
       (om-make-point 0 0)
       )))


(defun find-pane-with-focus (layout)
  (capi:map-pane-descendant-children 
   layout
   #'(lambda (p)
       (when (capi:pane-has-focus-p p)
         (return-from find-pane-with-focus p)))))


;;;=============================================

(defclass om-tab-layout (capi::tab-layout om-graphic-object) ())

(defun om-make-tab-layout (view-list &key (size (om-make-point 200 200)) (position (om-make-point 0 0))
                                     (selection 0))
  (let ((x (om-point-v position))
        (y (om-point-v position))
        (w (om-point-h size))
        (h (om-point-v size))
        tl)
    (setf tl (make-instance 'om-tab-layout
                   :width nil :height nil
                   :default-x x :default-y y :default-width w :default-height h
                   :visible-min-width nil :visible-min-height nil
                   :accepts-focus-p t
                   :vx x :vy y :vw w :vh h
                   :items view-list
                   :print-function #'(lambda (pane) (or (capi::capi-object-name pane) "Untitled Tab"))
                   :visible-child-function 'identity
                   :selected-item (nth selection view-list)
                   ))
    tl))

(defmethod om-current-view ((self om-tab-layout)) 
  (capi::tab-layout-visible-child self))

(export '(om-tab-layout om-make-tab-layout om-current-view) :om-api)



