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
; MOVABLE OBJECTS : mobile graphics on LW panes
;DocFile
;;===========================================================================


(in-package :om-api)

(export '(om-init-motion-functions
          om-selection-rectangle
          om-movable-line
          om-movable-rectangle
          om-movable-cercle

          om-new-movable-object
          om-update-movable-object
          om-set-movable-object-data
          om-erase-movable-object
          om-get-rect-movable-object
          om-init-point-movable-object
          om-handle-motion

          om-cursor-line
          om-view-cursor-play
          om-new-movable-cursor
          om-update-movable-cursor 
          om-erase-movable-cursor
          
           movable-extra-line
           om-movable-lines
           movable-extra-rect
           movable-extra-cercle 
           om-movable-cresc 
           om-movable-decresc 
           movable-extra-polygon
           movable-extra-slur 

           om-new-movable-points-geometry 
           compute-points-geometry-rect 
           om-update-points-geometry 

          ) :om-api)
;=======================
;utilities
;=======================
(defmethod pane-geometry ((pane pinboard-object))
  (multiple-value-bind (x y) (pinboard-pane-position pane)
     (multiple-value-bind (w h) (pinboard-pane-size pane)
       (values x y w h))))

(defun rectangle-intersection-p (rect1 rect2)
  (destructuring-bind (x y w h) rect1
    (destructuring-bind (x1 y1 w1 h1) rect2
      (and (< x (+ x1 w1))
           (< x1 (+ x w))
           (< y (+ y1 h1))
           (< y1 (+ y h))))))


;=======================
;the basic class of the movable object
;=======================
(defclass movable-object (pinboard-object)
  ((restore-image :initform nil :accessor restore-image) ;the image
   (geometry :initform '(0 0)))) ;used by the system

;the generic for drawing the object : the basic method does nothing, it must be specialized

(defmethod set-motion-object-position ((obj movable-object) x y)
  (setf (capi::pinboard-pane-position obj) (values x  y))
  )

(defmethod get-motion-object-position ((obj movable-object))
  (capi::pinboard-pane-position obj) 
  )

(defmethod get-motion-object-size ((obj movable-object))
  (capi::pinboard-pane-size obj)
  )

(defmethod set-motion-object-size ((obj movable-object) x y)
  (setf (capi::pinboard-pane-size obj) (values x  y))
)



(defgeneric draw-movable-object (pinboard object &optional x y w h))

(defmethod draw-movable-object (pinboard (object movable-object) &optional x y w h) (declare (ignore pinboard object x y w h)) nil)


;the four steps in the capi method specialized for the class of "movable-object". NOrmally this is called into the "mouse-motion-callback" method

(defmethod draw-pinboard-object (pane (self movable-object) &key x y width height erase)
  (ignore-errors 
  (multiple-value-bind (ox oy ow oh) (pane-geometry self)
    (when (or erase (or (not (and x y width height)) (rectangle-intersection-p (list x y width height) (list ox oy ow oh))))
      (when (slot-value self 'restore-image) ;step 1 and 2 are naturally conditionned by the fact an image already exists
        (draw-movable-object-image pane self) ;STEP 1
        (free-movable-object-image pane self)) ;STEP 2
      
      (unless erase ;in an erase process STEP 3 & 4 are not used
        (when (and (> ow 0) (> oh 0))
          (make-movable-object-image pane self ox oy ow oh) ;STEP 3om-create-motion-obj
          (draw-movable-object pane self x y width height) ;STEP 4
          ))))
  )
  )

;onely usefull in some very specific situations

(defmethod erase-movable-object (pane (self movable-object))
  (draw-pinboard-object pane self :erase t)
  )

;=======================
;-----MAIN PROCEDURE
;=======================

(defun valid-image-p (image)
  #-cocoa (and image (gp::image-representation image))
  #+cocoa (and image (when-let (representation (gp::image-representation image))
                       (slot-value representation 'image))))   ; does the cocoa pointer for the image exists ?


;-----MAIN PROCEDURE

;STEP 1 : draw the stored image and restore the port. If the type of the object is "selection" it works with four images rather than one

(defmethod draw-movable-object-image (pane obj) 
 (with-slots (restore-image geometry) obj
   (with-geometry pane
        (let* ((w (port-width pane))
               (h (port-height pane))
               (left (if (capi::simple-pane-horizontal-scroll pane) (or %scroll-x% 0) 0))
               (top (if (capi::simple-pane-vertical-scroll pane) (or %scroll-y% 0) 0)))
          (flet ((draw (img a b c d)
                   (when (and a b c d 
                              (valid-image-p img)
                              ;(< a (+ left w)) (>= c left) (< b (+ top h)) (>= d top)
                              )
                     (draw-image pane img a b))))
           (destructuring-bind (x y w h) geometry
             (draw restore-image x y (+ x w) (+ y h))
             )
           )))))



;STEP 2 : destroy the stored image. If the type of the object is "selection" it works with four images rather than one

(defun free-movable-object-image (pane obj &optional (restore-Image nil value-p))
  (when-let (restore-image (or restore-image (slot-value obj 'restore-image)))
    (if (listp restore-image)
      (loop for image in restore-image when (valid-image-p image) do (free-Image pane image))
      (when (valid-image-p restore-image) (free-Image pane restore-image)))
    (unless value-p (setf (slot-value obj 'restore-image) nil))))


;STEP 3 : make a new image of the port region before drawing the object itself. If the type of the object is "selection" it works with four images rather than one

(defmethod make-movable-object-image (pane obj x y w h)  ;la nouvelle géométrie de l'objet
  (with-slots (restore-image geometry) obj
    (with-geometry pane
      (let* ((width (port-width pane))
             (height (port-height pane))
             (left (if (capi::simple-pane-horizontal-scroll pane) (or %scroll-x% 0) 0))
             (top (if (capi::simple-pane-vertical-scroll pane) (or %scroll-y% 0) 0))
             (right (+ left width))
             (bottom (+ top height))
             (x4 (+ x w))
             (y4 (+ y h))
             images geometries)
        (flet ((test (a b c d) (and (< a right) (> b left) (< c bottom) (> d top)))
               (make-image (a b c d)
                 (let (ox oy ow oh) ;l'ancienne géométrie de l'objet
                   (setf ox (max left a) oy (max top c) ow (- (min right b) ox) oh (- (min bottom d) oy))
                   (values (when (and (> ow 0) (> oh 0))
                             (make-image-from-port pane ox oy ow oh))
                           (list ox oy ow oh)))))
         (when (test x x4 y y4) 
           (multiple-value-bind (image geom) (make-image x x4 y y4)
             (setf restore-image image 
                   geometry geom))))))))

;================
;API
;================
;THE PANE 

(defclass om-movable-view () 
  ((current-movable-object :initform nil :accessor current-movable-object)
   (initial-position :initform nil :accessor initial-position)
   (last-position :initform nil :accessor last-position)
   (click-motion-action :initform nil :accessor click-motion-action)
   (release-motion-action :initform nil :accessor release-motion-action)))


(defmethod om-init-motion-functions ((self om-movable-view) click-motion release)
  (setf *clicked-view* self)
  (setf (click-motion-action self) click-motion)
  (setf (release-motion-action self) release))

(defmethod om-click-release-handler :after ((self om-movable-view) pos)
 (when (release-motion-action self)
    (funcall (release-motion-action self) self pos))
  (setf (release-motion-action self) nil)
  (setf (click-motion-action self) nil))

(defmethod om-click-motion-handler :after ((self om-movable-view) pos)
  (om-handle-motion self pos))

(defmethod om-handle-motion ((self om-movable-view) pos)
  (when (click-motion-action self)
      (funcall (click-motion-action self) self pos)
      t))

(defmethod om-editor-has-movable-p ((self om-movable-view))
  (current-movable-object self))

(defmethod om-new-movable-object ((self om-movable-view) x y w h class)
  (with-slots (current-movable-object  initial-position) self
    (setf current-movable-object (make-instance class)
          initial-position (list x y))
    (set-motion-object-position current-movable-object x y)
    (set-motion-object-size current-movable-object w h)))     

(defmethod om-update-movable-object ((self om-movable-view)  x y w h)
  (with-slots (current-movable-object movable-object-type initial-position ) self
    (when current-movable-object
      (set-motion-object-size current-movable-object  (max 4 w) (max 4 h))
      (set-motion-object-position current-movable-object  x y)
      (capi::draw-pinboard-object self current-movable-object x y w h))))

(defmethod om-set-movable-object-data ((self om-movable-view) data)
  (with-slots (current-movable-object) self
    (set-movable-object-data current-movable-object data)))

(defmethod set-movable-object-data ((self t) data) nil)

(defmethod om-erase-movable-object ((self om-movable-view))
  (with-slots (current-movable-object) self
    (when current-movable-object
    (when (oa::restore-image current-movable-object)
      (oa::draw-movable-object-image self current-movable-object)
      (oa::free-movable-object-image self current-movable-object)
      (setf current-movable-object nil)))))


(defmethod om-init-point-movable-object ((self om-movable-view))
  (with-slots (current-movable-object initial-position) self
    (when current-movable-object 
      (destructuring-bind (ox oy) initial-position
        (list ox oy)))))

(defmethod om-get-rect-movable-object ((self om-movable-view) x y)
   (with-slots (current-movable-object initial-position) self
     (when current-movable-object
      (multiple-value-bind (w h) (get-motion-object-size current-movable-object)
        (destructuring-bind (ox oy) initial-position
           (let* ((rect (om-pts-to-rect (om-make-point ox oy) (om-make-point x y)))
                  (rx  (om-rect-left rect))
                  (ry (om-rect-top rect))
                  (rw (om-rect-w rect))  
                  (rh (om-rect-h rect)))
             (list rx ry rw rh)))))))


;SELECTION RECTANGLE

(defclass om-selection-rectangle (movable-object) ())

(defmethod draw-movable-object (self (object om-selection-rectangle) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    #-cocoa (gp::draw-rectangle self (1+ x) (1+ y) (- w 3) (- h 3) :filled t :foreground (c *om-select-color-alpha*) :operation boole-orc1)
    #+cocoa (gp::draw-rectangle self (1+ x) (1+ y) (- w 3) (- h 3) :filled t :foreground (c *om-select-color-alpha*))
    (gp::draw-rectangle self (1+ x) (1+ y) (- w 3) (- h 3) :filled nil :foreground (c (om-make-color 0.5 0.5 0.5)) :thickness 1)
    ))


; RECTANGLE

(defclass om-movable-rectangle (movable-object) ())

(defmethod draw-movable-object (self (object om-movable-rectangle) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    (draw-rectangle self (1+ x) (1+ y) (- w 3) (- h 3) :filled nil :foreground (c *om-gray-color*) 
                    :thickness 1 :dashed t :dash '(1 1))))

; CERCLE

(defclass om-movable-cercle (movable-object) ())

(defmethod draw-movable-object (self (object om-movable-cercle) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    (gp:draw-ellipse self (+ x (round w 2)) (+ y (round h 2) -1) 
                     (max (- (round w 2) 2) 1) (max (- (round h 2) 2) 1)
                     :filled nil :thickness 1 :dashed t :dash '(1 1))
    ))

; LINE
(defclass om-movable-line (movable-object) ())

(defmethod draw-movable-object (self (object om-movable-line) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    (let ((x1 (+ x w))
          (y1 (+ y h))
          (x0 (car (initial-position self)))
          (y0 (second (initial-position self))))
     ;(print (list x0 x x1 y0 y y1))
     (cond
       ((and (= x0 x) (< y0 y1))
        (draw-line self x0 y0 (if  (<= (abs (- x1 x0)) 2) x0 (- x1 1)) (if (<= (abs (- y1 y0)) 2) y0 (-  y1 1))) )
       ((and (= x0 x1) (< y0 y1))
        (draw-line self (if  (<= (abs (- x1 x0)) 2) (- x0 1) x0) y0 (if  (<= (abs (- x x0)) 2) (-  x0 1) (+ x 1)) (if (<= (abs (- y1 y0)) 2) y0 (-  y1 1))))
       ((and (= x0 x1) (> y0 y))
        (draw-line self (if  (<= (abs (- x1 x0)) 2) (- x0 1) x0) (if (<= (abs (- y1 y0)) 2) (- y0 1) y0)
                   (if  (<= (abs (- x x0)) 2) (-  x0 1) (+ x 1)) (if (<= (abs (- y y0)) 2) (- y0 1) (+  y 1))) )
       ((and (= x0 x) (> y0 y))
        (draw-line self x0 (if (<= (abs (- y1 y0)) 2) (- y0 1) y0) (if  (<= (abs (- x1 x0)) 2) x0 (- x1 1)) (if (<= (abs (- y y0)) 2) (- y0 1) (+  y 1))) )
       ;((and (= y0 y) (< x0 x1))  (print "o") (draw-line self x0 y0 x1 y1))
       ;((and (= y0 y) (> x0 x))  (print "o") (draw-line self x0 y0 x1 y1))
       (t nil)))))


; MULTI LINES
(defclass om-movable-lines (movable-object) 
  ((pts :accessor pts :initform nil)))

(defmethod set-movable-object-data ((self om-movable-lines) data) 
  (let ((rep (copy-list data)))
    ; (loop for p on data while (cdr p) do (push (list (car p) (cadr p)) rep))
    (setf (pts self) (reverse rep))))


(defmethod draw-movable-object (self (object om-movable-lines) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    (let ((x1 (+ x w))
          (y1 (+ y h))
          (x0 (car (initial-position self)))
          (y0 (second (initial-position self))))
     (cond
       ((and (= x0 x) (< y0 y1))
        (draw-line self x0 y0 (if  (<= (abs (- x1 x0)) 2) x0 (- x1 1)) (if (<= (abs (- y1 y0)) 2) y0 (-  y1 1))) )
       ((and (= x0 x1) (< y0 y1))
        (draw-line self (if  (<= (abs (- x1 x0)) 2) (- x0 1) x0) y0 (if  (<= (abs (- x x0)) 2) (-  x0 1) (+ x 1)) (if (<= (abs (- y1 y0)) 2) y0 (-  y1 1))))
       ((and (= x0 x1) (> y0 y))
        (draw-line self (if  (<= (abs (- x1 x0)) 2) (- x0 1) x0) (if (<= (abs (- y1 y0)) 2) (- y0 1) y0)
                   (if  (<= (abs (- x x0)) 2) (-  x0 1) (+ x 1)) (if (<= (abs (- y y0)) 2) (- y0 1) (+  y 1))) )
        ((and (= x0 x) (> y0 y))
        (draw-line self x0 (if (<= (abs (- y1 y0)) 2) (- y0 1) y0) (if  (<= (abs (- x1 x0)) 2) x0 (- x1 1)) (if (<= (abs (- y y0)) 2) (- y0 1) (+  y 1))) )))))

;play cursor


(defclass om-view-cursor-play () 
   ((movable-cursor :initform nil :accessor movable-cursor)))

(defmethod om-new-movable-cursor ((self om-view-cursor-play) x y w h class)
  (with-slots (movable-cursor  initial-position) self
    (setf movable-cursor (make-instance class)
          initial-position (list x y))
    (set-motion-object-position movable-cursor x y)
    (set-motion-object-size movable-cursor w h)))    

(defmethod om-update-movable-cursor ((self om-view-cursor-play)  x y w h)
  (capi:apply-in-pane-process self 
                              #'(lambda ()
                                  (with-slots (movable-cursor) self
                                    (when movable-cursor
                                      (set-motion-object-size movable-cursor  (max 4 w) (max 4 h))
                                      (set-motion-object-position movable-cursor x y)
                                      (capi::draw-pinboard-object self movable-cursor x y w h)
                                      )))))


(defmethod om-erase-movable-cursor ((self om-view-cursor-play))
  (capi:apply-in-pane-process self 
                              #'(lambda ()
                                  (with-slots (movable-cursor) self
                                    (when movable-cursor
                                      (when (oa::restore-image movable-cursor)
                                        (multiple-value-bind (x y w h) (pane-geometry movable-cursor)
                                          (om-invalidate-rectangle self x y w h))
                                        (oa::free-movable-object-image self movable-cursor)
                                        (setf movable-cursor nil)))))))


(defclass om-cursor-line (movable-object) ())

(defmethod draw-movable-object (self (object om-cursor-line) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    (gp:draw-line self (+ x 0.5) (+ y 0.5) (+ x 0.5) (+ h y -0.5) :foreground (color::make-rgb 0.6 0.2 0.2) :thickness 1 :dashed t :dash '(1 1))))

;===============
;================
(defclass om-points-geometry (movable-object) 
  ((list-of-points :initform nil :accessor list-of-points)))

(defmethod om-new-movable-points-geometry ((self om-movable-view) class points)
  (with-slots (current-movable-object  initial-position) self
    (multiple-value-bind (x y w h) (compute-points-geometry-rect points)
      (setf current-movable-object (make-instance class)
            initial-position (list x y))
      (setf (list-of-points current-movable-object) points)
      (set-motion-object-position current-movable-object x y)
      (set-motion-object-size current-movable-object w h))) )


(defun compute-points-geometry-rect (points)
  (loop for item in points 
                     maximize (om-point-v item) into y1
                     maximize (om-point-h item) into x1
                     minimize (om-point-h item) into x
                     minimize (om-point-v item) into y
                     finally (return (values (- x 7) (- y 7) (+ 14 (- x1 x)) (+ 14 (- y1 y))))))

(defmethod om-update-points-geometry ((self om-movable-view) points)
  (with-slots (current-movable-object   ) self
    (multiple-value-bind (x y w h) (compute-points-geometry-rect points)
      (when current-movable-object
        (setf (list-of-points current-movable-object) points)
        (set-motion-object-size current-movable-object  w h)
        (set-motion-object-position current-movable-object  x y)
        (capi::draw-pinboard-object self current-movable-object x y w h)))))



;-----line
(defclass movable-extra-line (om-points-geometry) ())

(defmethod draw-movable-object (self (object movable-extra-line) &optional x y w h)
  (declare (ignore x y w h))
  (let* ((points (list-of-points object))
         (selec-size 6))
    (loop for item in points do
          (draw-rectangle self (om-point-h item) (om-point-v item) selec-size selec-size))
    (gp::with-graphics-state (self :foreground (c *om-gray-color*))
      (gp:draw-line self (om-point-h (car points)) (om-point-v (car points)) (om-point-h (second points)) (om-point-v (second points))))
    ))

;------rect
(defclass movable-extra-rect (om-points-geometry) ())

(defmethod draw-movable-object (self (object movable-extra-rect) &optional x y w h)
  (declare (ignore x y w h))
  (let* ((points (list-of-points object))
         (selec-size 6))
    (loop for item in points do
          (draw-rectangle self (om-point-h item) (om-point-v item) selec-size selec-size))
    (gp::with-graphics-state (self :foreground (c *om-gray-color*))
      (gp:draw-rectangle self (om-point-h (car points)) (om-point-v (car points)) 
                         (- (om-point-h (second points)) (om-point-h (car points)))
                         (- (om-point-v (second points)) (om-point-v (car points)))))))

;------cercle

(defclass movable-extra-cercle (om-points-geometry) ())

(defmethod draw-movable-object (self (object movable-extra-cercle) &optional x y w h)
  (declare (ignore x y w h))
  (let* ((points (list-of-points object))
         (selec-size 6)
         x0 y0 w0 h0)
    (loop for item in points do
          (draw-rectangle self (om-point-h item) (om-point-v item) selec-size selec-size))
    (setf w0 (max 1 (round (- (om-point-h (second points)) (om-point-h (car points))) 2))
           h0 (max 1 (round (- (om-point-v (second points)) (om-point-v (car points))) 2))
           x0 (+ (om-point-h (car points)) w0)
           y0 (+ (om-point-v (car points)) h0))
    (gp::with-graphics-state (self :foreground (c *om-gray-color*))
      (gp:draw-ellipse self x0 y0 w0  h0))))

;---cresc
(defclass om-movable-cresc (movable-object) ())

(defmethod draw-movable-object (self (object om-movable-cresc) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    (draw-line self x (round (+ y (/ h 2))) (- (+ x w) 1) (+ 2 y))
    (draw-line self x (round (+ y (/ h 2))) (- (+ x w) 1) (- (+ y h) 2))))

;---decresc
(defclass om-movable-decresc (movable-object) ())

(defmethod draw-movable-object (self (object om-movable-decresc) &optional x y w h)
  (declare (ignore x y w h))
  (multiple-value-bind (x y w h) (pane-geometry object)
    (draw-line self x y (- (+ x w) 1) (round (+ y (/ h 2))))
    (draw-line self x  (- (+ y h) 1) (- (+ x w) 1) (round (+ y (/ h 2))))))

(defmethod draw-decresc ( x x1 y y1   )
  (om-draw-line x y  x1 (round (+ y (/ (- y1 y) 2))))
  (om-draw-line  x y1  x1 (round (+ y (/ (- y1 y) 2)))))

;------polygon
(defclass movable-extra-polygon (om-points-geometry) ())

(defmethod draw-movable-object (self (object movable-extra-polygon) &optional x y w h)
  (declare (ignore x y w h))
  (let* ((points (list-of-points object))
         (selec-size 6))
    (loop for item in points do
          (draw-rectangle self (om-point-h item) (om-point-v item) selec-size selec-size))
    (gp::with-graphics-state (self :foreground (c *om-gray-color*))
      (gp:draw-polygon  self (loop for item in points append (list (om-point-h item) (om-point-v item))) :closed t))))

;-------
(defvar *slur-accuracy1* 5)
(setf *slur-accuracy1* 20)

(defun output-bezier1 (self x0 y0 x1 y1 x2 y2 x3 y3 n)
  (let* ((cx (* 3 (- x1 x0)))
	 (cy (* 3 (- y1 y0)))
	 (bx (- (* 3 (- x2 x1)) cx))
	 (by (- (* 3 (- y2 y1)) cy))
	 (ax (- x3 x0 cx bx))
	 (ay (- y3 y0 cy by))
	 (incr (/ 1.0 n))
         (lastx x0)
         (lasty y0))
    (loop for i from 0 to 1 by incr do
      (draw-line self lastx lasty
		(+ x0 (* i (+ cx (* i (+ bx (* i ax))))))
		(+ y0 (* i (+ cy (* i (+ by (* i ay)))))))
      (setf lastx (+ x0 (* i (+ cx (* i (+ bx (* i ax))))))
            lasty (+ y0 (* i (+ cy (* i (+ by (* i ay))))))))))

(defun b-curveto (self x0 y0 x1 y1 x2 y2 x3 y3)
  (output-bezier1 self x0 y0 x1 y1 x2 y2 x3 y3 *slur-accuracy1*)
  (output-bezier1 self  x0 y0 x1 (- y1 1) x2 (- y2 1) x3 y3 *slur-accuracy1*))

(defclass movable-extra-slur (om-points-geometry) ())

(defmethod draw-movable-object (self (object movable-extra-slur) &optional x y w h)
  (declare (ignore x y w h))
  (let* ((points (list-of-points object))
         (selec-size 6))
    (loop for item in points do
          (draw-rectangle self (om-point-h item) (om-point-v item) selec-size selec-size))
    (gp::with-graphics-state (self :foreground (c *om-gray-color*))
      (gp:draw-polygon  self (loop for item in points append (list (om-point-h item) (om-point-v item)))))
    (apply 'b-curveto self (loop for item in points append (list (om-point-h item) (om-point-v item))) )))

