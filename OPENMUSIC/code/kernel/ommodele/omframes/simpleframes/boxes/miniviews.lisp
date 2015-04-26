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
;The miniview is the view in a factory that shows graphicly the instance.
;Class and methods for miniviews are defined in this file
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;================================================
;Miniview for boxes of editors
;================================================
(defclass general-miniview (om-view-drag select-object) 
   ((minipict :initform nil :accessor minipict)))

(defmethod get-box-frame ((self general-miniview)) (om-view-container self))

;===================DRAG&DROP=============================

(defmethod om-drag-selection-p ((self general-miniview) mouse-position)
   (declare (ignore mouse-position))
   ;(not (om-control-key-p))
   t)

(defmethod get-drag-object ((self general-miniview)) (om-view-container self))

(defmethod get-pos-in-object ((self general-miniview) where)
   (om-add-points (om-view-position self) where))

;======================
;======================
(defclass miniview (general-miniview om-item-view) ())

;====================== EVENTS==========================

(defmethod om-view-click-handler ((self miniview) where)
  (toggle-icon-active-mode (om-view-container self)))


;(defmethod om-view-doubleclick-handler ((self miniview) where)
;  (when (equal self (call-next-method))  ;;; new for click in lock-button
;    (OpenObjectEditor (object (om-view-container self)))))

(defmethod om-view-doubleclick-handler ((self miniview) where) nil)


(defmethod get-fonde-pict ((self t)) *boxedit-pict*)
(defmethod get-fonde-color ((self t)) nil)

(defmethod draw-only-select ((self general-miniview))
  #+win32
  (om-with-focused-view self
    (om-with-line-size (if (selected-p self) 2 3)
      (om-with-fg-color self (if (selected-p self) *om-black-color* *om-white-color*)
        (if (selected-p self)
            (om-draw-rect 1 1 (- (w self) 1) (- (h self) 1))
          (om-draw-rect 1 1 (- (w self) 2) (- (h self) 2)))))
    (unless (selected-p self)
      (om-draw-rect 0 0 (w self) (h self))))
  #-win32
  (om-invalidate-view self)
  ;(progn
  ;  (om-invalidate-rectangle self 0 0 (w self) 3)
  ;  (om-invalidate-rectangle self 0 0 3 (h self))
  ;  (om-invalidate-rectangle self 0 (- (h self) 3) (w self) 3)
  ;  (om-invalidate-rectangle self (- (w self) 3) 0 3 (h self)))
  )


(defmethod om-draw-contents ((self miniview))
  (let ((box (object (om-view-container self))))
    (om-with-focused-view self
      (if (showpict box)
          (progn
            (om-with-fg-color self *om-white-color*
              (om-fill-rect 0 0 (w self) (h self)))
            (draw-mini-view self (value box))
            )
        (let* ((icon (icon (reference box)))
               (sizeicn (icon-sizes icon (def-icon-size box)))
               (xi (car sizeicn)) (yi (cadr sizeicn))
               (iconhdlr (second (get&corrige-icon icon)))
               (pic (get-fonde-pict (value box)))
               posi)
          (if pic
              (om-draw-picture self pic 
                               :size (om-make-point (w self) (h self)))
            (om-with-fg-color self (or (get-fonde-color (value box))
                                       *om-light-gray-color*)
              (om-fill-rect 0 0 (w self) (h self))))
          (when iconhdlr
            (setf posi (om-make-point (- (round (w self) 2) (round xi 2)) (- (round (h self) 2) (round yi 2))))
            (om-draw-picture self iconhdlr :pos posi :size (om-make-point xi yi)))
          ))
      (when (show-name box)
        (om-draw-string 4 (- (h self) 5) (name box)))
      (when (play-state box)
        (om-with-focused-view self
          (om-with-fg-color self *om-green2-color*
            (om-fill-polygon '((15 5) (20 8) (15 11))))))
      (let ((line (if (selected-p self) 2 1)))
        (om-with-fg-color self (om-make-color 0 0 0) 
          (om-draw-rect 0 0 (1- (w self)) (1- (h self)) :pensize line))
        )
      )
    ))




(defmethod move-miniview ((self t) dir)
  (when (get-mini-param self 'deltapict)
    (case dir
      (0 (set-mini-param self 'deltapict (om-add-points (get-mini-param self 'deltapict) (om-make-point 0 -1))))
      (1 (set-mini-param self 'deltapict (om-add-points (get-mini-param self 'deltapict) (om-make-point 0 1))))
      (2 (set-mini-param self 'deltapict (om-add-points (get-mini-param self 'deltapict) (om-make-point 1 0))))
      (3 (set-mini-param self 'deltapict (om-add-points (get-mini-param self 'deltapict) (om-make-point -1 0)))))
    (om-invalidate-view self t)))

(defmethod scroll-miniview ((Self t))
  (when (get-mini-param self 'deltapict)
    (setf *maq-last-click* (om-mouse-position self))
    (setf *maq-first-click* *maq-last-click*)
    (setf *maq-offset-click* (om-make-point (om-point-h (get-mini-param self 'deltapict)) 
                                            (om-point-v (get-mini-param self 'deltapict))))
    (om-init-motion-functions self 'make-scroll-miniview nil)))
  

(defmethod make-scroll-miniview ((Self t) Where)
  (let* ((old-Mouse *maq-last-click*)
         (first-mouse *maq-first-click*)
         (new-mouse where)
         (inx (om-point-h *maq-offset-click*))
         (iny (om-point-v *maq-offset-click*))
         (offx (- (om-point-h new-mouse) (om-point-h first-mouse)))
         (offy (- (om-point-v first-mouse) (om-point-v new-mouse))))
    (set-mini-param self 'deltapict (om-make-point (+ inx offx) (- iny offy)))
    (om-invalidate-view self)
    (setq *maq-last-click* where)))


(defmethod update-miniview ((self t) (type t))  (om-invalidate-view self t))
(defmethod init-miniview   ((self t) (val t ))   (update-miniview self val))


(defmethod name-values-list ((self t))
   (mapcar #'(lambda (slot)
               (let* ((slot-name (internp (name slot) (slot-package slot))))
                 (list (name  slot) (eval `(,slot-name ,self))))) 
           (get-all-slots-instances (type-of self))))


(defmethod draw-mini-view  ((self t) (value t)) 
   (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))

(defmethod draw-obj-in-rect ((self t) x x1 y y1 edparams view)
   (declare (ignore edparams))
   (om-with-focused-view view
     (if (omclass-p (class-of (class-of self)))
       (let* ((thelist (name-values-list self))
              (length (length thelist))
              (y (+ y 15)))
         (loop for item in thelist
               for i = 0 then (+ i 1)
               while (< y y1) do
               (om-draw-string (+ x 3) (- y 3) (format nil "~D : ~D" (car item) (second item)))
               (unless (= i (- length 1))
                 (om-with-fg-color view *om-gray-color*   
                   (om-draw-line x y x1 y)))
               (setf y (+ y 15))))
       (om-draw-string (+ x 2) (+ y (round (- y1 y) 2) 4) (format nil "~D"  self)))))

(defmethod print-mini-view  ((self t) (value t) tl br)
  (draw-obj-in-rect value (om-point-h tl) (om-point-h br) (om-point-v tl)  (om-point-v br) (view-get-ed-params self) self))


(defmethod default-obj-box-size ((self t)) 
  (om-make-point 70 50))

(defmethod get-boxsize ((self ombox)) 
  (let ((size (default-obj-box-size (value self))))
    (om-make-point (max (om-point-h size)
                        (* 8 (+ 1 (length (get-all-initargs-of-class (type-of (value self)))))))
                   (om-point-v size))))

 
(defvar *size-miniview* 18)

(defmethod get-mini-param ((self t) param)
  (get-edit-param (object (om-view-container self)) param))

(defmethod set-mini-param ((self t) param val)
  ;(rplacd (assoc param (edition-params (object (om-view-container self)))) val)
  (set-edit-param (object (om-view-container self)) param val)
  )


(defmethod initx ((self t))
  (let ((dp (get-mini-param self 'deltapict)))
    (or (and dp (om-point-h dp)) 0)))

(defmethod inity ((self t))
   (let ((dp (get-mini-param self 'deltapict)))
    (or (and dp (om-point-v dp)) 0)))

(defmethod view-get-ed-params ((self t))
  (let ((obj (object (om-view-container self))))
    (corrige-edition-params (value obj) (edition-params obj))
    ))

(defmethod view-get-ed-params ((self tempobjframe))
  (default-edition-params (car (value (object self))))
  ;(edition-params (object self)))
  )  

  
;===============================draw-in maquettes=======================================

  
;======================================================
;USED IN MAQUETTES
;======================================================
(defmethod update-miniview ((self tempobjframe) (type t))  (call-next-method))

(defun draw-carre (view &optional onlysel)
  (unless (and onlysel (and (iconview view) (not (selected-p (iconview view)))))
    (om-with-focused-view view
      (let ((line (if (and (iconview view) (selected-p (iconview view))) 2 1)))
        (om-draw-rect 0 0 (1- (w view)) (1- (h view)) :pensize line))
      )))

(defmethod draw-editor-mode ((self t) view) 
  (draw-carre view nil)
  (draw-obj-in-rect self 0 (w view) 0  (h view) (view-get-ed-params view) view))

;---Maquette


(defmethod max-y-item ((self ommaquette))
  (let ((min 0) (max 0))
    (loop for item in (boxes self) do
          (when (boxtempobj-p item) 
            (when (or (not max) (> (posy item) max)) (setf max (posy item)))
            (when (or (not min) (< (- (posy item) (sizey item)) min)) (setf min (- (posy item) (sizey item))))))
    (list max min)))

(defmethod draw-editor-mode ((self ommaquette) view)
  (let ((delta (+ 5 (if (eval-func self) (round (h view) 4) 0))))
    (om-with-focused-view view
      (om-with-fg-color nil (colorframe (object view)) 
        (om-fill-rect 0 0 (w view) (h view)))
      (let ((durmaq (get-internal-dur self))
            (rangey (max-y-item self)))
        (loop for item in (boxes self) do
              (when (boxtempobj-p item)
                (draw-rect-in-rect item (w view) (- (h view) delta) 0 5 durmaq (first rangey) (second rangey) 1 view)
                )))
        
        (setf delta (- delta 10))
        (when (and (eval-func self) (value self))
          (om-with-fg-color nil *om-white-color*
            (om-fill-rect 0 (- (h view) delta) (w view) delta))
          (unless (maq-obj-p (value self))
            (if (draw-with-mini-pict (value self))
                (if (minipict view)
                  (om-draw-picture view (minipict view) :pos (om-make-point 0 (- (h view) delta)) :size (om-make-point (w view) delta))
                  (om-draw-string 10 (- (h view) delta -20) "?"))
              (draw-obj-in-rect (value self) 0 (w view) (- (h view) delta) (h view) (view-get-ed-params view) view)
            ))
         )
        )
      (draw-carre view nil)))

(defmethod update-miniview ((self t) (value ommaquette)) 
  ;(when (minipict self) 
  ;  ;(om-kill-picture (minipict self))
   ; (setf (minipict self) nil)
    ;)
  (when (and ;(not (minipict self))
             (eval-func value) (value value)
             (not (maq-obj-p (value value)))
             (draw-with-mini-pict (value value))
             )
    (setf (minipict self) (cons-maq-mini-pict (car (list! (value value))) self 20 (om-make-point (om-width self) 40))))
  (om-invalidate-view self t))


(defun draw-rect-in-rect (item w h x0 y0 durmaq maxy miny i view)
  (let* ((rangey (- maxy miny))
         (rx (+ x0 (round (* w (slot-value item 'offset)) durmaq)))
         (ry (+ y0 (- h (round (* h (- (posy item) miny)) rangey))))
         (rw (round (* w (extend item) (* (strech-fact item))) durmaq))
         (rh (round (* h (sizey item)) rangey)))
    (om-with-fg-color nil (colorframe item) 
      (om-fill-rect rx ry rw rh))   
    (om-with-fg-color nil *om-black-color* 
      (om-draw-rect rx ry rw rh))
    (when (show-name item)
      (om-with-clip-rect view (om-make-rect rx ry (+ rx rw) (+ ry rh))
      (om-with-fg-color nil *om-dark-gray-color* 
        (om-with-font *om-default-font1*
                    (om-draw-string (+ rx 4) (+ ry rh -5) (name item))))))
    (when (maquette-p (reference item))
      (let* ((maq (reference item))
             (durmaq1 (get-internal-dur maq))
             (maxy1 (max-y-item maq)))
        (loop for it in (boxes maq) do
              (when (boxtempobj-p it)
                (draw-rect-in-rect it rw rh rx ry durmaq1 (first maxy1) (second maxy1) (+ i 1) view)))
        ))))


(defmethod draw-with-mini-pict ((self t)) nil)
(defmethod draw-with-mini-pict ((self simple-container)) t)

(defmethod cons-maq-mini-obj ((self t) (view t) sizefont size)
  (draw-mini-obj self view sizefont size))

(defmethod cons-maq-mini-pict ((self t) frame fontsize size) nil)

(defmethod cons-maq-mini-pict ((self simple-container) frame fontsize size)
  (let ((newfont (om-make-music-font *heads-font* fontsize)))
    (om-record-pict newfont size
      (cons-maq-mini-obj self frame fontsize size))))


