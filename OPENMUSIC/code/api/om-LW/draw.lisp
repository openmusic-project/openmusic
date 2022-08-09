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
; Authors: Jean Bresson, Carlos Agon, Karim Haddad
;=========================================================================

;;===========================================================================
;DocFile
; OM-GRAPHIC OBJECTS DISPLAY CALLBACKS
;DocFile
;;===========================================================================



(export '(

           om-draw-contents
           om-component-border
           om-highlight-view
           om-invalidate-view
           om-invalidate-rectangle
           om-invalidate-corners
           om-with-delayed-redraw
           om-draw-view-outline
           om-redraw-view
         
          ) :om-api)

(in-package :om-api)

(defmethod om-draw-contents ((self om-graphic-object)) t)

(defmethod om-component-border ((self om-graphic-object)) nil)

;;; ONLY FOR WINDOWS
;;; draws a pinboard-object on top of the layout
(defmethod capi::draw-pinboard-object (pane (po om-view-pinboard-object) &key x y w h)

  (declare (ignore x y w h))
  
  (capi::apply-in-pane-process pane 
                               (lambda (pa) 
                                 (let ((posx (or (capi::get-horizontal-scroll-parameters pa :slug-position) 0))
                                       (posy (or (capi::get-vertical-scroll-parameters pa :slug-position) 0)))
                                      ;(set-graphics-port-coordinates pa :left 0 :top 0)
                                      ; (gp::with-graphics-state (pa :mask (list posx posy (om-width pa) (om-height pa)))
                                      ;  (om-with-focused-view pa
                                      ;    (om-draw-rect 2 2 (- (om-width pa) 4) (- (om-height pa) 4)))
                                   (om-draw-contents pa)
                                   (when (highlight pa) 
                                     (om-with-focused-view pa
                                       (om-draw-hilite-rect posx posy (om-width pa) (om-height pa))))
                                     ; )
                                   ))
                               pane))


;;; ONLY FOR COCOA
(defmethod om-draw-contents-callback ((self om-graphic-object) x y w h)
  (om-with-error-handle 
    (set-graphics-port-coordinates (om-get-view self) :left 0 :top 0)
    ;(gp::clear-rectangle (om-get-view self) 0 0 (om-width self) (om-height self))
    (om-draw-contents self)
    ;(print (list self (om-get-view self)))
    (mapcar #'(lambda (po) 
                ;(print (list po (item-x po) (item-y po)))
                ;(gp::set-graphics-port-coordinates (om-get-view self) :left (- (item-x po)) :top (- (item-y po)))
                (when (capi::pinboard-object-overlap-p po x y (+ x w) (+ y h))
                ;  (gp::with-graphics-state ((om-get-view self) :mask  (list (item-x po) (item-y po) (vw po) (vh po)))
                  (capi::draw-pinboard-object (om-get-view self) po 
                                              :x (item-x po) :y (item-y po) :width (vw po) :height (vh po))
                ;(gp::set-graphics-port-coordinates (om-get-view self) :left 0 :top 0)
                 ;   )
                  ))
            (remove-if-not 'om-item-view-p (item-subviews (om-get-view self)) ))
    (when (highlight self) 
      (om-with-focused-view (om-get-view self)
        (om-draw-hilite-rect 0 0 (om-width self) (om-height self))))
      ))


(defmethod item-draw-callback (pane (obj t) x y w h) nil)

;(setf *break-on-signals* nil)
(defvar *om-locked-draw* nil)

(defmethod capi::draw-pinboard-object (pane (self om-item-view) &key x y w h)
  (call-next-method)
  (unless (initialized-p self) (setf (initialized-p self) t))
  (capi::apply-in-pane-process pane 'draw-po pane self)
  )

(defun draw-po (pane po)
   (multiple-value-bind (*pox* *poy*) (capi::static-layout-child-position po)
    (let ((fff (or (if (gp::font-description-p (om-get-font po))
		       (gp::find-best-font pane (om-get-font po))
		       (om-get-font po))
		   (gp::graphics-state-font (gp::port-graphics-state pane))))) ;; when po class is text-enter-view returns nil for fff... AV
      (gp::with-graphics-state (pane :font fff 
                                     :mask 
                                    ; #-linux
                                     (list (item-x po) (item-y po) (vw po) (vh po))
                                     ;(list (item-x po) (item-y po) 20 20)
                                    ; #+linux nil
                                     ;(list (- (item-x po) (om-h-scroll-position pane)) (- (item-y po) (om-v-scroll-position pane))
                                     ;      (vw po) (vh po))
                                     :foreground (or (capi::pinboard-object-graphics-arg po :foreground) :black)
                                     :background 
                                     #+linux (or (capi::pinboard-object-graphics-arg po :background) :white)
                                     #-linux (capi::pinboard-object-graphics-arg po :background)
                                     )
     ;(gp::set-graphics-port-coordinates pane :left (- x) :top (- y))
     (setf *curfocus* po) 
     (om-with-focused-view po
       (om-draw-contents po))
     ;switching to this will only display frames without icons and fonts (for testing purposes).
     ;(om-with-focused-view po
     ;  (om-draw-rect 0 0 (- (om-width po) 1) (- (om-height po) 1)))
     (when (highlight po) 
       (om-with-focused-view (om-get-view po)
        (om-draw-hilite-rect 0 0 (om-width po) (om-height po))))
     (setf *curfocus* nil)
     ;(set-graphics-port-coordinates pane :left 0 :top 0)
     ))
    )
  )

; (capi::update-pixmap-pinboard-geometry 

;(defmethod capi::draw-pinboard-object-highlighted (pane (self om-item-view) &key x y w h)
;  (call-next-method))



(defmethod om-invalidate-view ((self om-graphic-object) &optional erase)
  (declare (ignore erase))
  (when (and (interface-visible-p self) (om-get-view self))
    ;(capi::with-atomic-redisplay ((om-get-view self))
    (capi::apply-in-pane-process (om-get-view self) 'gp::invalidate-rectangle (om-get-view self))
    #+(or win32 linux) (mapcar 'om-invalidate-view (om-subviews self))
    ;)
    ))

(defmethod om-invalidate-view ((self om-item-view) &optional erase)
  (declare (ignore erase))
  ;(capi:redraw-pinboard-object self)
  (when (item-container self)
    (capi::apply-in-pane-process (item-container self)
                                 'gp::invalidate-rectangle (item-container self) (item-x self) (item-y self) (vw self) (vh self))
  ;(mapc 'capi:redraw-pinboard-object (vsubviews self))
    ))

(defmethod om-invalidate-corners ((self om-item-view) topleft bottomright)
  (when (item-container self)
    (capi::apply-in-pane-process (om-get-view self)
				 'gp::invalidate-rectangle
				 (item-container self) 
				 (+ (item-x self) (om-point-h topleft)) 
				 (+ (item-y self) (om-point-v topleft))
				 (+ (item-x self) (- (om-point-h bottomright) (om-point-h topleft)))
				 (+ (item-y self) (- (om-point-v bottomright) (om-point-v topleft)))
				 )))

;; deprecated
(defmethod om-invalidate-corners ((self om-graphic-object) topleft bottomright)
  ;;(print (list 'drawing-mode=quality? (gp::port-drawing-mode-quality-p self)))
  (when (interface-visible-p self)
    (capi::apply-in-pane-process (om-get-view self)
				 'gp::invalidate-rectangle
				 (om-get-view self)
				 ;; (om-point-h topleft) (om-point-v topleft) 
				 ;; (om-point-h bottomright) (om-point-v bottomright) 
				 ;; (- (om-point-h bottomright) (om-point-h topleft)) 
				 ;; (- (om-point-v bottomright) (om-point-v topleft))
				 )))

(defmethod om-invalidate-rectangle ((self om-graphic-object) x y w h)
  (when (interface-visible-p self)
    (capi::apply-in-pane-process 
     (om-get-view self) 
     'gp::invalidate-rectangle 
     (om-get-view self) x y w h
     )
     #+lispworks8(capi::update-drawing-with-cached-display self x y w h)
    ))

(defmethod om-invalidate-rectangle ((self om-item-view) x y w h)
  (when (item-container self)
    (gp::invalidate-rectangle (item-container self) 
                              (+ (item-x self) x) 
                              (+ (item-y self) y)
                              (+ (item-x self) w)
                              (+ (item-y self) h))
     #+lispworks8(capi::update-drawing-with-cached-display self x y w h)
    ))


(defmacro om-with-delayed-redraw (view &body body)
   `(capi:with-atomic-redisplay (,view)
      ,@body))

;;; nouveau.. 
(defmethod om-redraw-view ((self om-graphic-object))
  (capi:redraw-pinboard-layout (om-get-view self) 0 0 (om-width self) (om-height self) t))

(defmethod om-draw-view-outline ((self om-graphic-object) &optional (pensize 1))
  (om-with-focused-view self
                        (gp::with-graphics-state (*curstream* :thickness pensize)
                          (om-draw-rect 0 0 (- (om-width self)  1) (- (om-height self) 1)))))

(defmethod om-redraw-view ((self om-scroller))
  (let* ((x0  (om-h-scroll-position self))
         (y0  (om-v-scroll-position self)))
    (capi::apply-in-pane-process self
				 (lambda (pane) (capi:redraw-pinboard-layout (om-get-view pane) x0 y0 (om-width pane) (om-height pane) t))
				 self)
    ))

(defmethod om-highlight-view ((self om-graphic-object) t-or-nil)
  (unless (equal (highlight self) t-or-nil)
    (setf (highlight self) t-or-nil)
    t))

(defmethod om-highlight-view ((self om-view) t-or-nil)
  (when (call-next-method)
    (om-invalidate-view self)
  ;(om-redraw-view self)
  ))

(defmethod om-redraw-view ((self om-item-view))
  (capi:redraw-pinboard-object self t))


(defmethod om-highlight-view ((self om-item-view) t-or-nil) 
  (when (call-next-method) 
  (om-invalidate-view self))
  ;(om-redraw-view self)
  ;(if t-or-nil
  ;    (capi::highlight-pinboard-object (item-container self) self :redisplay t)
  ;  (capi::unhighlight-pinboard-object (item-container self) self :redisplay t))
  )


