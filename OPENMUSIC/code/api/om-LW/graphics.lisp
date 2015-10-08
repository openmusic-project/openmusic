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
; GRAPHIC STRUCTURES (points, rects, regions, colors, fonts) 
; DRAWING TOOLS
;DocFile
;;===========================================================================



(in-package :om-api)

;;;=========================
;;; export :
;;;=========================
(export '(
                om-make-point
                om-make-big-point
                om-point-p
                om-points-equal-p
                om-add-points
                om-subtract-points
                om-add-big-points
                om-subtract-big-points
                om-point-h
                om-point-x
                om-point-v
                om-point-y
                om-point-in-line-p 
                om-point-*

                om-make-rect
                om-pts-to-rect
                om-sect-rect
                om-rect-empty
                om-rect-top
                om-rect-left
                om-rect-bottom
                om-rect-right
                om-rect-w
                om-rect-h
                om-rect-topleft
                om-rect-bottomright
                om-point-in-rect-p
                
                om-open-region
                om-close-region
                om-open-region-add-rect
                om-open-region-add-line
                om-new-region
                om-dispose-region
                om-region-add-rect
                om-region-add-line
                om-set-rect-region
                om-union-region
                om-point-in-region-p
                om-copy-region
                om-difference-region
                om-paint-region
                om-offset-region
                
                om-get-current-port
                
                om-with-focused-view
                om-with-clip-rect
                om-with-bg-color
                om-with-fg-color
                
                om-draw-char
                om-draw-string
                om-draw-line
                om-draw-point
                om-draw-rect
                om-draw-round-rect
                om-fill-round-rect
                om-fill-rect
                om-draw-ellipse
                om-fill-ellipse
                om-draw-ellipse-arc
                
                om-erase-rect-content
                om-erase-rect
                om-erase-line
                
                om-draw-polygon
                om-fill-polygon

                *om-select-color*
                *om-text-select-color*
                om-draw-selection-rect
                om-draw-hilite-rect
                om-draw-hilite-icon

                om-draw-rect-outline
                
                *om-black-pattern*
                *om-darkgray-pattern*
                *om-gray-pattern*
                *om-lightgray-pattern*
                *om-dark-red-color*
                *om-white-pattern*
                
                om-with-pen
                om-with-line-size
                om-with-dashline
                om-with-line
                om-with-hilite-foreground
                
                om-make-color
                om-make-color-alpha
                om-color-p
                om-color-r
                om-color-g
                om-color-b
                *om-black-color* 
                *om-dark-gray-color* 
                *om-gray-color* 
                *om-light-gray-color* 
                *om-white-color* 
                *om-window-def-color*
                *om-transparent-color*

                *om-default-font1*
                *om-default-font2*
                *om-default-font3*
                *om-default-font4*
                *om-default-font1b*
                *om-default-font2b*
                *om-default-font3b*
                *om-default-font4b*
	        *om-score-font-face*
                *om-def-font-face*
                *om-def-font-sizes*
                *om-controls-font*
                
                om-make-font
                om-make-music-font
                om-font-p
                om-font-face
                om-font-family
                om-font-size
                om-font-mode
                om-font-style
                om-string-size
                om-string-h
                om-with-font
                om-make-font-object

                om-correct-point
                om-correct-font
                om-correct-color

                ) :om-api)



;;;=========================
;;;COMPAT TYPES
;;;=========================
(defun om-correct-color (color) 
  (if (om-color-p color) color *om-gray-color*))

(defun om-correct-point (point) 
  (cond ((om-point-p point) point)
        ((null point) point)
        ((numberp point) (om-make-point (- point (ash (ash point -16) 16)) (ash point -16)))
        ((consp point) (om-make-point (car point) (cadr point)))
        (t nil)))

(defun om-correct-font (font) 
  (if (om-font-p font) font *om-default-font1*))


;;;=========================
;;;POINTS
;;;=========================

(defstruct ompoint 
  (x 0)
  (y 0))


(defmethod make-load-form ((self ompoint) &optional env)
  (declare (ignore env))
  `(make-ompoint :x ,(ompoint-x self) :y ,(ompoint-x self)))

(defun om-make-point (x y) 
  (make-ompoint :x x :y y)) 

(defmethod om-point-p ((self t)) (ompoint-p self))

(defun om-make-big-point (x y)
  (make-ompoint :x x :y y))

(defmethod om-point-h ((point ompoint))
  (ompoint-x point))

(defmethod om-point-v ((point ompoint))
  (ompoint-y point))

(defmethod om-point-x ((point ompoint))
  (ompoint-x point))

(defmethod om-point-y ((point ompoint))
  (ompoint-y point))

(defmethod om-add-points (point1 point2)
   (om-make-point (+ (om-point-h point1) (om-point-h point2))
                  (+ (om-point-v point1) (om-point-v point2))))


(defmethod om-subtract-points (point1 point2)
   (om-make-point (- (om-point-h point1) (om-point-h point2))
                  (- (om-point-v point1) (om-point-v point2))))



(defun om-add-big-points (point1 point2)
  (om-add-points point1 point2))


(defun om-subtract-big-points (point1 point2)
  (om-subtract-points point1 point2) )

(defmethod om-points-equal-p (point1 point2) nil)

(defmethod om-points-equal-p ((point1 ompoint) (point2 ompoint))
  (and (equal (om-point-h point1) (om-point-h point2)) 
       (equal (om-point-v point1) (om-point-v point2))))

(defmethod print-point ((point ompoint))
  (let ((str (format nil "ompoint >>> h:~A v:~A" (om-point-h point) (om-point-v point))))
    (print str))
  point)

(defun dot-prod-2D (p1 p2) (+ (* (om-point-h p1) (om-point-h p2)) (* (om-point-v p1) (om-point-v p2))))
(defun norm-2D (p)  (sqrt (dot-prod-2D p p))) 
(defun dist2D (p1 p2) (norm-2D (om-subtract-points p1 p2)))

(defun dist-to-line (pt lp1 lp2)
  (let  ((v (om-subtract-points lp2 lp1))
         (w (om-subtract-points pt lp1)))
    (let ((c1 (dot-prod-2D w v)))
      (if (<= c1 0 )
          (dist2D pt lp1)
        (let ((c2 (dot-prod-2D v v)))
          (if (<= c2 c1)
              (dist2D pt lp2)
            (let* ((b (/ c1 c2))
                   (pb  (om-add-points lp1 (om-make-point (round (* b (om-point-h v))) 
                                                          (round (* b (om-point-v v)))))))
              (dist2D pt pb))))))))


(defun om-point-in-line-p (pt lp1 lp2 delta)
  (<= (dist-to-line pt lp1 lp2) delta))

(defmethod om-point-* ((point ompoint) fact)
  (om-make-point (* (om-point-h point) fact)
                 (* (om-point-v point) fact)))

;;;=========================
;;; RECTS 
;;;=========================


(defclass om-rect ()
  ((rx :accessor rx :initarg :rx :initform 0)
   (ry :accessor ry :initarg :ry :initform 0)
   (rw :accessor rw :initarg :rw :initform 0)
   (rh :accessor rh :initarg :rh :initform 0)))

(defun om-make-rect (left top right bottom)
  (make-instance 'om-rect
                 :rx left
                 :ry top
                 :rw (- right left)
                 :rh (- bottom top)))

(defun om-pts-to-rect (ompoint1 ompoint2)
 (let ((left (min (om-point-h ompoint1)(om-point-h ompoint2)))
        (top (min (om-point-v ompoint1)(om-point-v ompoint2)))  
        (right (max (om-point-h ompoint1)(om-point-h ompoint2)))
        (bottom (max (om-point-v ompoint1)(om-point-v ompoint2))))
   (make-instance 'om-rect
		:rx left
		:ry top
		:rw (- right left)
		:rh (- bottom top))))
 
(defun om-rect-top (rect)
  (ry rect))

(defun om-rect-bottom (rect)
  (+ (ry rect) (rh rect)))  

(defun om-rect-left (rect)
  (rx rect))

(defun om-rect-right (rect)
  (+ (rx rect) (rw rect)))

(defun om-rect-w (rect)
  (rw rect))

(defun om-rect-h (rect)
  (rh rect))

(defun om-rect-topleft (rect)
  (om-make-point (rx rect) (ry rect)))

(defun om-rect-bottomright (rect)
  (om-make-point (+ (rx rect) (rw rect)) (+ (ry rect) (rh rect))))


(defmethod rect-p ((self t)) nil)
(defmethod rect-p ((self om-rect)) t)

(defun om-sect-rect (rect1 rect2)
  (let* ((tx (max (rx rect1) (rx rect2)))
	 (ty (max (ry rect1) (ry rect2)))
	 (t2x (min (+ (rx rect1) (rw rect1))  (+ (rx rect2) (rw rect2))))
	 (t2y (min (+ (ry rect1) (rh rect1))  (+ (ry rect2) (rh rect2)))))
    (if (or (< t2x tx) (< t2y ty))
	nil
	(om-make-rect tx ty t2x t2y))))

(defun om-rect-empty (rect)
  (equal rect nil))

(defun om-point-in-rect-p (point rect)
  (and (and (>= (om-point-h point) (rx rect))
	    (<= (om-point-h point) (+ (rx  rect) (rw rect))))
       (and (>= (om-point-v point) (ry rect))
	    (<= (om-point-v point) (+ (ry rect) (rh rect))))))

(defun om-copy-rect (rect)
  (make-instance 'om-rect
	       :rx (rx rect)
	       :ry (ry rect)
	       :rw (rw rect)
	       :rh (rh rect)))


(defun print-rect (rect)
  (print (list (rx rect) (ry rect) (rw rect) (rh rect))))

;;;=========================
;;;REGIONS 
;;;=========================
(defclass om-region () 
  ((viewport :initform nil :initarg :viewport :accessor viewport)
   (elements :initform nil :initarg :elements :accessor elements)))

;;;====================================
;;; 2 MANIERES DE DEFINIR DES REGIONS... peut être à unifier...

;;; ====================================
;;; ouvrir la region, ajouter des elements, et fermer (close retourne la region) :

(defvar *current-region* nil)

(defun om-open-region (view)
  (setf *current-region* (make-instance 'om-region :viewport view))) 

(defun om-close-region (view)
   (declare (ignore view))
   *current-region*)

(defun om-open-region-add-rect (left top right bottom)
   (if *current-region* (push (om-make-rect left top right bottom) (elements *current-region*))))

(defun om-open-region-add-line (p1 p2 &optional (approx nil))
   (if *current-region* (push (list p1 p2 approx) (elements *current-region*))))

(defun om-region-bounds ( region)
  (let ((minx 10000000000000000) ;ayay
        (miny 10000000000000000)
        (maxx -1000000000000000)
        (maxy -1000000000000000))
   (loop for elt in (elements region) do
             (if (rect-p elt) 
                 (progn
                   (setf minx (min minx (rx elt)))
                   (setf miny (min miny (ry elt)))
                   (setf maxx (max maxx (+ (rx elt) (rw elt))))
                   (setf maxy (max maxy (+ (ry elt) (rh elt)))))
               (progn
                   (setf minx (min minx (om-point-h (first elt))))
                   (setf miny (min miny (om-point-v (first elt))))
                   (setf maxx (max maxx (om-point-h (second elt))))
                   (setf maxy (max maxy (om-point-v (second elt)))))))
   (list minx miny maxx maxy)))

(defun print-region (region)
  (mapcar 'print-rect (elements region))
  region)

;;; ====================================
;;; methode OO

(defun om-new-region ()
   (make-instance 'om-region))

(defmethod om-dispose-region ((self om-region))
   (setf self nil))


(defmethod om-region-add-rect ((self om-region) left top right bottom)
   (push (om-make-rect (min left right) (min top bottom)
                       (max left right) (max top bottom)) (elements self)))


(defmethod om-region-add-line ((self om-region) p1 p2 &optional (approx nil))
   (push (list p1 p2 approx) (elements self)))

(defun om-set-rect-region (region left top right bottom)
     (setf (elements region) (list (om-make-rect left top right bottom)))
     region)

;;; =====================================
;;; Test region

(defun om-union-region (reg1 reg2)
   (let ((reg-union (make-instance 'om-region)))
     (setf (elements reg-union) (append (elements reg1) (elements reg2)))
     reg-union))

(defmethod om-point-in-region-p ((self om-region) where)
  (let ((in nil))
     (loop for partie in (elements self)
            while (not in) do
               (cond ((rect-p partie) 
                      (if (om-point-in-rect-p where partie) (setf in t)))
                     (t (let ((p1 (first partie)) (p2 (second partie)) (approx (third partie)))
                          (unless (om-points-equal-p p1 p2)  
                            (if (om-point-in-line-p where p1 p2 (or approx 2))
                                    ;(gp::point-in-line-hull (om-point-h p1) (om-point-v p1)
                                    ;                        (om-point-h p2) (om-point-v p2)
                                    ;                        (om-point-h where) (om-point-v where) approx)
                                (setf in t))
                            )))))
     in))


(defun om-copy-region (region) 
   (let ((r (om-new-region)))
     (setf (elements r) 
        (loop for elt in (elements region) collect
                  (if (rect-p elt) (om-copy-rect elt)
                    (copy-list elt))))
     r))



;;; A FAIRE 
(defun om-difference-region (r1 r2 r3) r1)


(defun om-paint-region (view region)
   (loop for elt in (elements region) do
             (if (rect-p elt) 
                 (gp:draw-rectangle (om-get-view view) (rx elt) (ry elt) (rw elt) (rh elt) :filled nil)
               (gp::draw-line (om-get-view view) (om-point-h (first elt)) (om-point-v (first elt)) (om-point-h (second elt)) (om-point-v (second elt))))
             ))
             

(defun om-offset-region (region h v) 
   (let ((newelts (loop for elt in (elements region) collect 
                                    (if (rect-p elt) (om-make-rect (+ (rx elt) h) (+ (ry elt) v)
                                                                   (+ (rx elt) (rw elt)  h)
                                                                   (+ (ry elt) (rh elt) v))
                                      (list (om-add-points (first elt) (om-make-point h v))
                                             (om-add-points (second elt) (om-make-point h v))
                                             (third elt))))))
     (setf (elements region) newelts)
     region))





;;;=========================
;;;COLORS
;;;=========================

(defclass omcolor () 
  ((c :accessor c :initarg :c)))
;;; modifs dans graphics.lisp, windows.lisp, dialog-items.lisp
;;; user-interface.lisp; movable-object.lisp

(defmethod c ((self symbol)) (or self :black))

(defmacro om-make-color (r g b)
  `(make-instance 'omcolor :c (color:make-rgb ,r ,g ,b)))

(defmacro om-make-color-alpha (r g b a)
  `(make-instance 'omcolor :c (color:make-rgb ,r ,g ,b ,a)))

(defmethod make-load-form ((self omcolor) &optional env)
  (declare (ignore env))
  `(make-instance 'omcolor :c ,(c self)))

;(defmethod om-color-p ((self t)) (color::color-spec-p self))
(defmethod om-color-p ((self t)) nil)
(defmethod om-color-p ((self omcolor)) t)

(defun om-color-r (color)
  (color::color-red (c color)))

(defun om-color-g (color)
  (color::color-green (c color)))

(defun om-color-b (color)
   (color::color-blue (c color)))

(defun om-color-alpha (color)
  (color::color-alpha (c color)))

(defmethod print-object ((self omcolor) stream)
  (format stream "color R:~D G:~D B:~D" (om-color-r self) (om-color-g self) (om-color-b self)))

;;; system colors :
(defparameter *om-black-color* (make-instance 'omcolor :c (color::get-color-spec :black)))

(defparameter *om-light-gray-color* #-win32 (om-make-color 0.9 0.9 0.9)  #+win32 (om-make-color 0.96 0.96 0.96))
(defparameter *om-gray-color* (om-make-color 0.6 0.6 0.6))
(defparameter *om-dark-gray-color* (om-make-color 0.3 0.3 0.3))
(defparameter *om-dark-red-color* (om-make-color 0.9 0.3 0.3))
(defparameter *om-white-color* (make-instance 'omcolor :c (color::get-color-spec :white)))

(defvar *om-window-def-color* "The default background color for windows")
(setf *om-window-def-color* 
      #+win32  (make-instance 'omcolor :c (color::get-color-spec :gray90))
      #+linux (make-instance 'omcolor :c (color::get-color-spec :transparent))
      #+cocoa (make-instance 'omcolor :c :transparent)
      )

 
(defvar *om-transparent-color* (make-instance 'omcolor :c :transparent))

;(om-choose-color-dialog :color (om-make-color 0.803 0.854 0.855))


;;;=========================
;;;FONTS
;;;=========================

;;; view bidon
(defvar *record-view* nil)
(defvar *dummy-view* nil)
;; capi:create-dummy-graphics-port

(defun init-record-view ()
  (let* ((pl (make-instance 'capi:pinboard-layout))
         (win (capi:display (make-instance 'capi:interface 
                                           :display-state :hidden
                                           :layout pl))))
    (setf *record-view* pl)
    (setf *dummy-view* 
          #+lispworks7 (capi:create-dummy-graphics-port) 
          #-lispworks7 *record-view*)))



(om-api-add-init-func 'init-record-view)

(defmethod om-font-p ((self t)) (gp::font-description-p self))

;(defvar *om-font-modes* '(:srcCopy :srcOr :srcXor :srcBic :srcPatCopy :srcPatOr :srcPatXor :srcPatBic))
;(defvar *om-font-styles* '(:plain :bold :condense :extend :italic :outline :shadow :underline))

(defun om-make-font (face size &key (family nil) (style nil) (mode nil))
  (declare (ignore mode))
  (gp::make-font-description 
   ;:name face   ; --> name is not portable for find-best-font process
   :family face
   :size #+cocoa size #+win32 (* size 2/3) #+linux(* size 3/4)
   :slant (if (member :italic style) :italic :roman)
   :weight (if (member :bold style) :bold :normal)
   :pitch :variable
   :underline nil  ; souligne
   :strikeout nil ; barre
   ;:width nil 
   :charset :ansi 
   :devicep nil 
   :type :truetype 
   ;:w-family :swiss
   ))

;(gp::font-description-attributes (gp::make-font-description :family "Times" :size 14))
;(setf fo (capi::prompt-for-font nil))

;;; simplified font creation for music fonts
;;; so special case for platforms
(defun om-make-music-font (face size)	; mac=72dpi, windows+linux=96dpi (normally) AV
  (declare (ignore mode))
  (gp::make-font-description 
   :family face
   :size #+(or linux win32) (* size 3/4) #-(or linux win32) size
   :pitch :variable
   :charset :ansi 
   :devicep nil 
   :type :truetype 
   ))


(defun om-font-face (font) 
  (gp::font-description-attribute-value font :family))

(defun om-font-size (font) 
   #+win32(* (gp::font-description-attribute-value font :size) 3/2)
   #+linux (* (gp::font-description-attribute-value font :size) 4/3)
   #-(or linux win32 ) (gp::font-description-attribute-value font :size)
   )
   

(defun om-font-family (font) 
  (gp::font-description-attribute-value font :family))

(defun om-font-style (font) 
  (cond ((and (equal (gp::font-description-attribute-value font :weight) :bold)
              (equal (gp::font-description-attribute-value font :slant) :italic))
         '(:bold :italic))
        ((equal (gp::font-description-attribute-value font :slant) :italic) 
         '(:italic))
        ((equal (gp::font-description-attribute-value font :weight) :bold)
         '(:bold))
        (t '(:plain))))

(defun om-font-mode (font) nil)

(defun om-string-size (str &optional font)
  (if str 
      (multiple-value-bind (x1 y1 x2 y2) 
          (gp::get-string-extent *record-view* str 
                                 (and font (if (gp::font-p font) font (gp::find-best-font *record-view* font))))
        (- x2 x1))
    0))


(defun om-string-h (&optional (font *om-default-font2*))
  (multiple-value-bind (x1 y1 x2 y2) 
      (gp::get-string-extent *record-view* "ABC" 
                             (and font (if (gp::font-p font) font (gp::find-best-font *record-view* font))))
    (- y2 y1)))
  
; (om-string-size "Hello" (om-make-font "arial" 20))
; GP:GET-STRING-EXTENT 




(defvar *om-def-font-face* nil)
(defvar *om-def-bold-font-face* nil)
(defvar *om-def-font-sizes* nil)

(defvar *om-controls-font* nil)
(defvar *om-default-font0* nil)
(defvar *om-default-font1* nil)
(defvar *om-default-font2* nil)
(defvar *om-default-font3* nil)
(defvar *om-default-font4* nil)
(defvar *om-default-font1b* nil)
(defvar *om-default-font2b* nil)
(defvar *om-default-font3b* nil)
(defvar *om-default-font4b* nil)

(defvar *om-score-font-face* nil)


#-linux (setf *om-def-font-face* "Verdana")
#+linux (setf *om-def-font-face* "Liberation Sans")
;;; #+win32(setf *om-def-font-face* "MS Shell Dlg")
 

#-linux(setf *om-def-bold-font-face* "Verdana")
#+linux(setf *om-def-bold-font-face* "Liberation Sans")

;(setf *om-def-font-sizes* 
;      #+win32'(8 10 11 13 20) 
;      #-win32'(11 12 14 16 24))

(setf *om-def-font-sizes* '(11 12 14 16 24))

;;;#+win32(setf *om-default-font1* (gp::font-description capi-win32-lib::*win32-default-gui-font*))
(setf *om-default-font1* (om-make-font *om-def-font-face* (nth 0 *om-def-font-sizes*)))

(setf *om-default-font2* (om-make-font *om-def-font-face* (nth 1 *om-def-font-sizes*)))
(setf *om-default-font3* (om-make-font *om-def-font-face* (nth 2 *om-def-font-sizes*)))
(setf *om-default-font4* (om-make-font *om-def-font-face* (nth 3 *om-def-font-sizes*)))
(setf *om-default-font1b* (om-make-font *om-def-bold-font-face* (nth 0 *om-def-font-sizes*) :style '(:bold)))
(setf *om-default-font2b* (om-make-font *om-def-bold-font-face* (nth 1 *om-def-font-sizes*) :style '(:bold)))
(setf *om-default-font3b* (om-make-font *om-def-bold-font-face* (nth 2 *om-def-font-sizes*) :style '(:bold)))
(setf *om-default-font4b* (om-make-font *om-def-bold-font-face* (nth 3 *om-def-font-sizes*) :style '(:bold)))

#+(or win32 linux) (setf *om-controls-font* (om-make-font *om-def-font-face* (nth 0 *om-def-font-sizes*)))
#-(or linux win32) (setf *om-controls-font* (om-make-font "LucidaGrande" 13))

(setf *om-score-font-face* "Times New Roman")

(defun om-make-font-object (font)
  (gp::find-best-font oa::*record-view* font))


;====== DRAW =====
;==========================================

(defvar *curstream* nil)
(defvar *curfocus* nil)

(defvar *pox* 0)
(defvar *poy* 0)

(defmacro om-with-focused-view (view &body body)
  `(if (and (om-item-view-p ,view) (not (equal *curfocus* ,view)))
       (let ((*curstream* (om-get-view ,view)))
         (when *curstream*
           ;;; ça doit plus marcher ! cf. draw item-view
           ;(multiple-value-bind (x y) (capi::pinboard-pane-position ,view)
           ;  (set-graphics-port-coordinates *curstream* :left x :top y))
           (multiple-value-bind (*pox* *poy*) (capi::pinboard-pane-position ,view)
             ,@body
             )))
     (let ((*curstream* (om-get-view ,view)))
       (when *curstream*
         ,@body))))

(defun om-get-current-port ()
  *curstream*)


(defmacro om-with-font (font &rest body)
  `(gp::with-graphics-state (*curstream* :font (if (gp::font-description-p ,font)
                                                   (gp::find-best-font *curstream* ,font)
                                                 ,font)
                                         
                                         )
     ,@body))



; TESTR AVEC CAPI::AREA-VISIBLE-P ?
#+win32
(defmacro om-with-clip-rect (view rect &body body)
  `(let ((posx (om-h-scroll-position ,view))
         (posy (om-v-scroll-position ,view)))
     ;(print (om-rect-left ,rect))
     (gp::with-graphics-state ((om-get-view ,view) :mask (list 
                                                          ;(- (om-rect-left ,rect) posx) (- (om-rect-top ,rect) posy) 
                                                          (om-rect-left ,rect) (om-rect-top ,rect) 
                                                          (om-rect-w ,rect) (om-rect-h ,rect)))
       ,@body)))

#-win32
(defmacro om-with-clip-rect (view rect &body body)
  `(gp::with-graphics-state ((om-get-view ,view) :mask (list (+ *pox* (om-rect-left ,rect)) (+ *poy* (om-rect-top ,rect))
                                                               (om-rect-w ,rect) (om-rect-h ,rect)))
     ,@body))


(defun om-set-clip-rect (view rect)
  (gp::set-graphics-state (om-get-view view) :mask (list (om-rect-left rect) (om-rect-top rect)  (om-rect-bottom rect) (om-rect-right rect))))

(defmacro om-with-fg-color (view color &body body)
  `(let ((port (if ,view (om-get-view ,view) *curstream*)))
     (gp::with-graphics-state (port :foreground (c ,color))
       ,@body)))

(defmacro om-with-bg-color (view color &body body)
  `(let ((port (if ,view (om-get-view ,view) *curstream*)))
     (gp::with-graphics-state (port :background (c ,color))
       ,@body)))

;-----------------------------------
(defun om-draw-char (x y cn)
  (gp:draw-character *curstream* cn (+ x *pox*) (+ y *poy*)))

(defun om-draw-string (x y str)
  (gp:draw-string *curstream* (substitute  #\Space #\Tab str) (+ x *pox*) (+ y *poy*) ))

(defun om-draw-line (x1 y1 x2 y2  &key (erasable nil))  
  (gp:draw-line *curstream* (+ x1 *pox* 0.5) (+ y1 *poy* 0.5) (+ x2 *pox* 0.5) (+ y2 *poy* 0.5)
		#-cocoa :operation #-cocoa (if erasable boole-eqv boole-1)
                :shape-mode :best
		))


(defun om-erase-line (x1 y1 x2 y2) 
  (gp:draw-line *curstream* (+ x1 *pox*) (+ y1 *poy*) (+ x2 *pox*) (+ y2 *poy*)
                #+cocoa :foreground #+cocoa (simple-pane-background *curstream*) 
                #+cocoa :thickness #+cocoa 4
                #-cocoa :operation #-cocoa boole-eqv
                ))

(defun om-draw-point (x y)  
   (gp:draw-point *curstream* (+ x *pox*) (+ y *poy*)))

(defun om-draw-rect (x &optional y (w  0) (h 0) &key (erasable nil) (pensize 1))
  (let (top left wi he)
    (if y
        (setf left x top y wi w he h)
      (setf left (rx x) top (ry x) wi (rw x) he (rh x)))
    (gp::with-graphics-state (*curstream* :thickness pensize)
    (gp:draw-rectangle *curstream* (+ left *pox* 0.5) (+ top *poy* 0.5) wi he :filled nil
                       #-cocoa :operation #-cocoa (if erasable boole-eqv boole-1)
                       ))))

(defun om-draw-rect-outline (x y w h &optional (pensize 1))
  (gp::with-graphics-state (*curstream* :thickness pensize)
    (om-draw-rect x y (- w 1) (- h 1))))


(defun om-fill-rect (x &optional y (w 0) (h 0)  &key (erasable nil))
  (let (top left wi he)
    (if y
	(setf left x top y wi w he h)
	(setf left (rx x) top (ry x) wi (rw x) he (rh x)))
    (gp:draw-rectangle *curstream* (+ left *pox*) (+ top *poy*) wi he
		       :filled t
		       #-cocoa :operation #-cocoa (if erasable boole-eqv boole-1)
		       #+linux :compositing-mode #+linux :copy
		       )
    ))

(defun om-erase-rect-content (x &optional y (w 0) (h 0))
  (let (top left wi he)
    (if y (setf left x top y wi w he h)
      (setf left (rx x) top (ry x) wi (rw x) he (rh x)))
    (gp:draw-rectangle *curstream* (+ left *pox*) (+ top *poy*) wi he :filled t 
                       #+cocoa :foreground #+cocoa (simple-pane-background *curstream*)
                       #-cocoa :operation #-cocoa boole-eqv
                       )))

(defun om-erase-rect (x &optional y (w 0) (h 0))
  (let (top left wi he)
    (if y (setf left x top y wi w he h)
      (setf left (rx x) top (ry x) wi (rw x) he (rh x)))
    #+cocoa(gp:draw-rectangle *curstream* (- (+ left *pox*) 1) (- (+ top *poy*) 1) (+ wi 3) (+ he 3) 
                              :filled t :foreground (simple-pane-background *curstream*))
    #-cocoa(gp:draw-rectangle *curstream* (+ left *pox*) (+ top *poy*) wi he
                       :filled nil :operation boole-eqv
                       )))

(defun om-draw-ellipse (x y rx ry)  
  (gp:draw-ellipse *curstream* (+ x *pox*) (+ y *poy*) rx ry :filled nil))

(defun om-fill-ellipse (x y rx ry)  
   (gp:draw-ellipse *curstream* (+ x *pox*) (+ y *poy*) rx ry :filled t))


(defun om-draw-ellipse-arc (x y width height start-angle sweep-angle) 
  (gp::draw-arc *curstream* (+ x *pox*) (+ y *poy*) (max 1 width) (max 1 height) start-angle sweep-angle))


;------------

(defun om-draw-polygon (points)  
  (gp:draw-polygon oa::*curstream* (loop for item in points  
                                         for i = 0 then (mod (+ i 1) 2) append
                                         (if (om-point-p item) 
                                             (list (+ (om-point-h item) *pox*) (+ (om-point-v item) *poy*)) 
                                           (if (listp item)
                                               (list (+ (car item ) *pox*) (+ (cadr item) *poy*))
                                             (+ item (if (zerop i) *pox* *poy*)))))
                   :closed t :filled nil))

(defun om-fill-polygon (points)  
  (gp:draw-polygon oa::*curstream* (loop for item in points 
                                         for i = 0 then (mod (+ i 1) 2) append 
                                         (if (om-point-p item) 
                                             (list (+ (om-point-h item) *pox*) (+ (om-point-v item) *poy*)) 
                                           (if (listp item)
                                               (list (+ (car item ) *pox*) (+ (cadr item) *poy*))
                                             (+ item (if (zerop i) *pox* *poy*)))))
                   :closed t :filled t))

;;;=========================
;;; PATTERNS DE SURFACE
;;;=========================

(defvar *om-black-pattern* nil)
(defvar *om-darkgray-pattern* nil)
(defvar *om-gray-pattern* nil)
(defvar *om-lightgray-pattern* nil)
(defvar *om-white-pattern* nil)

(defun om-init-patterns ()
  (setf *om-black-pattern* *black-pattern*)
  (setf *om-darkgray-pattern* *dark-gray-pattern*)
  (setf *om-gray-pattern* *gray-pattern*)
  (setf *om-lightgray-pattern* *light-gray-pattern*)
  (setf *om-white-pattern* *white-pattern*))

;(om-api-add-init-func 'om-init-patterns)

;(setf ccc (om-choose-color-dialog))
;(om-color-r ccc)

(defvar *om-select-color* nil)
;(setf *om-select-color* (make-instance 'omcolor :c (color::make-rgb 0.3 0.45 0.5 1)))
(setf *om-select-color* (make-instance 'omcolor :c (color::make-rgb 0.5 0.5 0.5 1)))
#+win32 (setf *om-select-color* (make-instance 'omcolor :c (color::make-rgb 0.87058825 0.87058825 0.87058825 1)))

(defvar *om-text-select-color* nil)
(setf *om-text-select-color* (om-make-color-alpha (/ (om-color-r *om-select-color*) 2)
                                         (/ (om-color-g *om-select-color*) 2)
                                         (/ (om-color-b *om-select-color*) 2)
                                         0.7))


(defvar *om-select-color-alpha* nil)
;(setf *om-select-color-alpha* (make-instance 'omcolor :c (color::make-rgb 0.2 0.35 0.6 0.2)))
(setf *om-select-color-alpha* (make-instance 'omcolor :c
                                             #-win32 (color::make-rgb 0.7 0.7 0.7 0.2)
                                             #+win32  (color::make-rgb 0.2 0.35 0.6 0.2)))

(defun om-draw-selection-rect (x y w h &key (mode :xor))
  (let ((left (if (minusp w) (+ x w) x))
        (width (abs w))
        (top (if (minusp h) (+ y h) y))
        (heigth (abs h)))
     (om-draw-rect left top width heigth)))

; :foreground *om-select-color*
;(defvar *om-select-color-complement* nil)
;(setf *om-select-color-complement* (make-instance 'omcolor :c (color::make-rgb 0.2 0.2 0.2 0.2)))

;;; hilites on a graphics
(defun om-draw-hilite-icon (x y w h &optional color) 
  (let* ((c (if color (c color)))
         (cc (if c
		 (color::make-rgb (color::color-red c)  (color::color-green c) (color::color-blue c) 1)
		 (c *om-select-color-alpha*))))
    (gp::with-graphics-state (*curstream* :foreground cc)
      (gp:draw-rectangle *curstream* (+ x *pox*) (+ y *poy*) (- w 1) (- h 1) :filled t))))


;; hilites rect
(defun om-draw-hilite-rect (x y w h &optional color) 
  (let* ((c (if color (c color)))
	 (cc (if c
		 (color::make-rgb (color::color-red c)  (color::color-green c) (color::color-blue c) 0.4)
		 (c *om-select-color-alpha*))))
    (gp::with-graphics-state (*curstream* :foreground cc)
      (gp:draw-rectangle *curstream* (+ x *pox*) (+ y *poy*) (- w 1) (- h 1) :filled t))))

;;; NOT USED ANYMORE
(defmacro om-with-pen ((view &key mode pattern (line :black-line) (size 1)) &body body)
  `(let () ,@body))

(defmacro om-with-line-size (size &body body)
  `(let ((siz #-win32 ,size #+win32 (max 1 (round ,size))))
     (gp::with-graphics-state (*curstream* :thickness siz
                                           :shape-mode :plain
                                           :scale-thickness t
					   :line-joint-style :miter   ; :bevel :round
                                           :line-end-style :round)    ; :butt :projecting 
    ,@body)))

(defmacro om-with-hilite-foreground (&body body)
  `(gp::with-graphics-state (*curstream* :foreground (c *om-select-color-alpha*))
    ,@body))

(defmacro om-with-dashline (&optional (pattern '(2 2)) &body body)
  `(gp::with-graphics-state (*curstream* :dashed t :dash ,pattern)
     ,@body))

(defmacro om-with-line (style &body body)
  `(cond ((consp ,style)
          (gp::with-graphics-state (*curstream* :dashed t :dash  (list (car ,style) (cadr ,style))) ,@body))
         ((equal (intern (string ,style) :oa) 'dash)
          (gp::with-graphics-state (*curstream* :dashed t :dash '(2 2)) ,@body))
         (t (progn ,@body))))

