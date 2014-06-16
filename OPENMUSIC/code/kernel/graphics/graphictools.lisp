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
;This File contains functions and macros used for graphical porpuse.
;Documentations are not in the html doc.
;In particular this files implements QuickDraw high level functions
;and view methods.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


;========= OM COLORS ===========

(defun color2list (color)
   (list (float (om-color-r color)) (float (om-color-g color)) (float (om-color-b color))))

(defun make-color-255 (r g b)
  (om-make-color (/ r 255.0) (/ g 255.0) (/ b 255.0)))


(defvar *om-red-color* nil)
(defvar *om-red2-color* nil) 
(defvar *om-blue-color* nil)
(defvar *om-green-color* nil)
(defvar *om-green2-color* nil)
(defvar *om-orange-color* nil)
(defvar *om-pink-color* nil) 
(defvar *om-wine-color* nil)
(defvar *om-dark-blue-color* nil)
(defvar *om-yellow-color* nil) 
(defvar *om-purple-color* nil)
(defvar *om-light-blue-color* nil) 
(defvar *om-cafe-color* nil)
(defvar *om-salmon-color* nil)
(defvar *om-steel-blue-color* nil)
(defvar *om-beige-color* nil)
(defvar *workspace-color* nil "the default color of the workspace")
(defvar *maq-color* nil "the default bg color of the maquette")
(defvar *azulito* nil)
(defvar *azulote* nil) 
(defvar *def-tempbox-color* nil)
(defvar *undefbox-color* nil)
(defvar *16-color-list* nil)
(defvar *controls-color* nil)
(defvar *controls-color+* nil)
(defvar *controls-color++* nil)
(defvar *controls-color-* nil)
(defvar *controls-color--* nil)
(defvar *editor-bar-color* nil)
(defvar *editor-bar-color++* nil)
(defvar *editor-bar-color+* nil)
(defvar *editor-bar-color--* nil)
(defvar *editor-bar-color-* nil)
(defvar *instboxframe-color* nil "def color for instance boxes")
(defvar *patch-box-color* nil)
(defvar *maquette-box-color* nil)
(defvar *obj-box-color* nil)
(defvar *global-box-color* nil)
(defvar *scorepatch-color* nil "la couleur du score panel en mode patch")
(defvar *note-color* nil)
(defvar *system-color* nil)
(defvar *select-color* nil)
(defvar *ornement-color* nil)
(defvar *accident-color* nil)
(defvar *obje-color-list* nil)

(defun init-om-color-vars ()
  (setf *accident-color* (om-make-color 0.1 0.7 0.6))
  (setf *ornement-color* (om-make-color 0.9 0.2 0.2))
  (setf *scorepatch-color* (om-make-color 0.5 0.62 0.55))
  (setf *patch-box-color* (om-make-color 0.5 0.5 0.6))
  (setf *maquette-box-color* (om-make-color 0.6 0.5 0.5))
  (setf *obj-box-color* (om-make-color 0.5 0.6 0.6))
  (setf *global-box-color* (om-make-color 0.5 0.6 0.5))
  (setf *instboxframe-color* (om-make-color 0.68 0.79 0.9))
  (setf *editor-bar-color* (make-color-255 210 210 210))
  (setf *editor-bar-color++* (make-color-255 170 170 170))
  (setf *editor-bar-color+* (make-color-255 235 235 235))
  (setf *editor-bar-color--* (make-color-255 110 110 110))
  (setf *editor-bar-color-* (make-color-255 175 175 175))
  (setf *om-white-color* (om-make-color 1 1 1))
  (setf *om-black-color* (om-make-color 0 0 0))
  (setf *om-gray-color* (om-make-color 0.5 0.5 0.5))
  (setf *om-red-color* (om-make-color 1 0 0))
  (setf *om-red2-color* (om-make-color 0.7 0.3 0.3)) 
  (setf *om-blue-color* (om-make-color 0 0 1))
  (setf *om-green-color* (om-make-color 0 1 0))
  (setf *om-orange-color* (om-make-color 1 0.5 0.25))
  (setf *om-pink-color* (om-make-color 1 0 1)) 
  (setf *om-wine-color* (om-make-color 0.50 0 0))
  (setf *om-dark-blue-color* (om-make-color 0 0 0.5))
  (setf *om-yellow-color* (om-make-color 1 1 0)) 
  (setf *om-purple-color* (om-make-color 0.75 0.61 0.98))
  (setf *om-light-blue-color* (om-make-color 0 1 1)) 
  (setf *om-cafe-color* (om-make-color 0.5 0.25 0.25))
  (setf *om-green2-color* (om-make-color 0.37 0.73 0.62))
  (setf *om-salmon-color* (om-make-color 0.99 0.62 0.45))
  (setf *om-steel-blue-color* (om-make-color 0.41 0.54 0.67))
  (setf *om-beige-color* (om-make-color 0.94 0.94 0.85))
  ;(setf *workspace-color*  (om-make-color 0.803 0.854 0.855))
  (setf *workspace-color*  (om-make-color 0.85 0.87 0.87))
  ;(setf *maq-color* (om-make-color 0.53333336 0.60784316 0.6))
  (setf *maq-color* (om-make-color 0.85 0.85 0.83))
  (setf *azulito* (om-make-color 0.772 0.855 0.788))
  (setf *azulote* (om-make-color 0.8 0.86 0.97))
  (setf *def-tempbox-color* (om-make-color 0.772 0.855 0.788))
  (setf *undefbox-color*(om-make-color 0.801 0.208 0.098))
  (setf *controls-color* (make-color-255 230 230 230)) 
  (setf *controls-color+* (make-color-255 240 240 240)) 
  (setf *controls-color++* (make-color-255 180 180 180)) 
  (setf *controls-color-* (make-color-255 155 155 155)) 
  (setf *controls-color--* (make-color-255 135 135 135))
  (setf *16-color-list* 
	(list *om-blue-color* *om-red-color* *om-green-color* *om-orange-color*
	      *om-pink-color* *om-wine-color* *om-dark-blue-color* *om-yellow-color*
	      *om-black-color* *om-gray-color* *om-purple-color* *om-light-blue-color*
	      *om-cafe-color* *om-green2-color* *om-salmon-color* *om-steel-blue-color*))
  (setf *obje-color-list* (list (om-make-color 0.525 0.769 0.549) 
				(om-make-color 0.51 0.49 0.788) 
				(om-make-color 0.851 0.851 0.475)
				(om-make-color 0.671 0.768 0.749)
				(om-make-color 0.812 1.0 1.0)
				(om-make-color 0.773 0.969 0.729)
				(om-make-color 0.812 1.0 1.0)))
  (setf *note-color* *om-black-color*)
  (setf *system-color* *om-black-color*)
  (setf *select-color* *om-select-color*))

(om-add-init-func 'init-om-color-vars)

(defun om-interpole-colors (begin end steps)
  (if (< steps 2)
    (list begin end)
  (let* ((difR (/ (- (om-color-r end) (om-color-r begin)) steps))
         (difG (/ (- (om-color-g end) (om-color-g begin)) steps))
         (difB (/ (- (om-color-b end) (om-color-b begin)) steps)))
    (loop for i from 0 to (- steps 1)
          collect (om-make-color
                   (float (+ (* i difR) (om-color-r begin)))
                   (float (+ (* i difG) (om-color-g begin)))
                   (float (+ (* i difB) (om-color-b begin))))))))

(defun om-random-color (&optional (alpha 1.0))
  (om-make-color-alpha (om-random 0.0 1.0) (om-random 0.0 1.0) (om-random 0.0 1.0) alpha)) 
  
(defun om-color-alpha (color alpha)
  (om-make-color-alpha (om-color-r color) (om-color-g color) (om-color-b color) alpha))

;=====FONTS

(defvar *controls-font* nil)
(defvar *controls-fonti* nil)
(defvar *ombox-font* nil)

(defun init-om-fonts-vars ()
  (setf *controls-font* *om-controls-font*)
  (setf *controls-fonti* (om-make-font (om-font-face *controls-font*) (om-font-size *controls-font*) :style '(:italic)))
  (setf *ombox-font* *om-default-font1*))

(om-add-init-func 'init-om-fonts-vars)



;;;;======================================
;;;; Graphic components utilities 
;;;; muste be called on graphic objects only (window, view,dialog-item,...)
;;;;======================================
(defmethod x   ((self t)) (om-point-h (om-view-position self)))
(defmethod y   ((self t)) (om-point-v (om-view-position self)))
(defmethod w   ((self t)) (om-point-h (om-view-size self)))
(defmethod h   ((self t)) (om-point-v (om-view-size self)))
(defmethod x+w ((self t)) (+ (x self)(w self)))
(defmethod y+h ((self t)) (+ (y self)(h self)))



;;;=== position in window ===
(defun view-position-win (view)
  (let ((rep (om-make-point 0 0))
        (init view)
        (win (om-view-window view)))
    (loop while (not (equal init win)) do
          (setf rep (om-add-points rep (om-view-position init)))
          (setf init (om-view-container init)))
    rep))


(defmethod window ((self t))
  (om-view-window self))


;;;====== POSITIONS AND BOXES ====

(defun point-max (point-1 point-2)
  (om-make-point (max (om-point-h point-1) (om-point-h point-2))
                                (max (om-point-v point-1) (om-point-v point-2))))

(defun rect-from-points (x y x1 y1)
   (om-pts-to-rect (om-make-point x y) (om-make-point x1 y1)))
   
(defun points-from-rect (rect)
  (list (om-rect-left rect)
         (om-rect-top rect)
         (om-rect-right rect) 
         (om-rect-bottom rect)))

;;;True if the point (x1 y1) is into the rectangle ((x,y) (x+w,y+h))
(defun inside-rectangle? (x1 y1 x y w h)
  (let* ((topleft (om-make-point x y))
           (rect (om-pts-to-rect topleft (om-add-points topleft (om-make-point w h))))
           (point (om-make-point x1 y1)))
          (om-point-in-rect-p point rect)))


;;;;-------select in x --------

(defun draw-h-rectangle (rec &optional fill mode)
  (when rec
    (om-with-fg-color nil *om-select-color*
      (if fill
          (om-draw-hilite-rect (car rec) (second rec) (- (third rec) (car rec) ) (- (fourth rec) (second rec)));  *om-select-color*)
        (om-draw-rect (car rec) (second rec) (- (third rec) (car rec) ) (- (fourth rec) (second rec)))))))



;;;===== SPECIAL DIALOG ITEMS =====

;;; size for names in dialog items
(defun get-name-size (name &optional font)
  (+ 4 (om-string-size (concatenate 'string " " name) font)))


;=======================================================================
;  invisible resize box
;=======================================================================
(omg-defclass c-resize-box (om-item-view) ()  ; (om-transparent-view) ()
   (:documentation "This subclass the view allow resize another view by click and draw within it.#enddoc#
#seealso# (box-frame) #seealso#"))

(defmethod om-view-cursor ((self c-resize-box))
   *om-resize-cursor*)

(defvar *init-resize-pos* nil)


(defmethod get-box-frame ((self c-resize-box)) (get-box-frame (om-view-container self)))

(defmethod om-view-click-handler ((self c-resize-box) where)
   (declare (ignore where))
   (let* ((boxframe (get-box-frame self))
          (theeditor (editor (om-view-container boxframe)))
          (panel (om-view-container (get-box-frame self)))
          (rx (x boxframe)) 
          (ry (y  boxframe)))
     (when (text-view theeditor)
       (exit-from-dialog (text-view theeditor) 
                                       (om-dialog-item-text (text-view theeditor))))
     (setf *init-resize-pos* where)
     (om-new-movable-object panel rx ry (w boxframe) (h boxframe) 'om-movable-rectangle)
     (om-init-motion-functions self 'resize-box-motion 'resize-box-release)
    ))
      

(defmethod resize-box-motion ((self c-resize-box) pos)
  (when *init-resize-pos*
  (let ((panel (om-view-container (get-box-frame self))))
    (when panel
      (let* ((initpoint (om-convert-coordinates pos self panel))
            (initsize (om-add-points (om-add-points (om-view-size (get-box-frame self)) (om-view-position (get-box-frame self)))
                                     (om-subtract-points pos *init-resize-pos*)))
            (rx (om-point-h initsize))
            (ry (om-point-v initsize))
            (rect  (om-init-point-movable-object panel)))
        (om-update-movable-object panel (first rect) (second rect) (max 4  (- rx (first rect))) (max 4 (- ry (second rect) )))
        )))))

(defmethod resize-box-release ((self c-resize-box) pos) 
  (let* ((boxframe (get-box-frame self))
         (panel (om-view-container boxframe)))
    (when (and boxframe panel)
      (let* ((initpoint (om-convert-coordinates pos self panel ))
             (initsize (om-add-points (om-view-size boxframe) (om-view-position boxframe)))
             (initsizepos (om-add-points initsize (om-subtract-points pos *init-resize-pos*)))         
             (rx (om-point-h initsizepos))
             (ry (om-point-v initsizepos))
             (rect  (om-init-point-movable-object panel)))
     (om-erase-movable-object panel)
     (change-boxframe-size boxframe (om-make-point (- rx (first rect) ) (- ry (second rect) )))
     (om-invalidate-rectangle panel (x boxframe) (y boxframe) (w boxframe) (h boxframe)) 
     (setf *init-resize-pos* nil)
     ))))

(defmethod add-box-resize ((self om-graphic-object))
   "add a resize view to the wiew 'self'"
   (om-add-subviews self
     (om-make-view 'c-resize-box
                   ;:bg-color (om-make-color-alpha 0.8 0.8 0.8 0.2)
                   :size (om-make-point 10 10)
                   :position (om-make-point (- (w self) 10) (- (h self) 10)))))



;;; resizes an object to fit its container size without loosing proportions

(defun resize-to-fit (object-size container-size)
   (let (new-size new-pos fact fact2)
     (cond ((and (< (om-point-h object-size) (om-point-h container-size)) (< (om-point-v object-size) (om-point-v container-size)))
                  (setf new-size object-size)
                  (setf new-pos (om-make-point (- (round (om-point-h container-size) 2) (round (om-point-h object-size) 2))
                                                                           (- (round (om-point-v container-size) 2) (round (om-point-v object-size) 2)))))
                 ((and (>= (om-point-h object-size) (om-point-h container-size)) (< (om-point-v object-size) (om-point-v container-size)))
                  (setf fact (/ (om-point-h container-size) (om-point-h object-size)))
                  (setf new-size (om-make-point (round (* fact (om-point-h object-size))) (round (* fact (om-point-v object-size)))))
                  (setf new-pos (om-make-point 0 (- (round (om-point-v container-size) 2) (round (om-point-v new-size) 2)))))
                 ((and (< (om-point-h object-size) (om-point-h container-size)) (>= (om-point-v object-size) (om-point-v container-size)))
                  (setf fact (/ (om-point-v container-size) (om-point-v object-size)))
                  (setf new-size (om-make-point (round (* fact (om-point-h object-size))) (round (* fact (om-point-v object-size)))))
                  (setf new-pos (om-make-point (- (round (om-point-h container-size) 2) (round (om-point-h new-size) 2)) 0)))
                 ((and (>= (om-point-h object-size) (om-point-h container-size)) (>= (om-point-v object-size) (om-point-v container-size)))
                  (setf fact (/ (om-point-h container-size) (om-point-h object-size)))
                  (setf fact2 (/ (om-point-v container-size) (om-point-v object-size)))
                  (if (< fact fact2)
                      (progn 
                         (setf new-size (om-make-point (round (* fact (om-point-h object-size))) (round (* fact (om-point-v object-size)))))
                         (setf new-pos (om-make-point 0 (- (round (om-point-v container-size) 2) (round (om-point-v new-size) 2)))))
                    (progn 
                         (setf new-size (om-make-point (round (* fact2 (om-point-h object-size))) (round (* fact2 (om-point-v object-size)))))
                       (setf new-pos (om-make-point (- (round (om-point-h container-size) 2) (round (om-point-h new-size) 2)) 0)))
                    ))
                 )
     (list new-size new-pos)))


;=========================================================================
; POP-UP MENUS
;========================================================================

(omg-defclass pair-pop-up-menu (om-pop-up-menu) 
  ((dialo :initform nil :accessor dialo)))

(defmethod update-dailo ((self pair-pop-up-menu) text)
  (when (dialo self)
    (om-set-dialog-item-text (dialo self) text)
    (om-invalidate-view (dialo self) t)))


(defun cons-pair-pop-menu (itemtext fun container list-vals default-item pos size)
  (let (menu itemlist default)
    (loop for item in list-vals 
        for i = 0 then (+ i 1) do
          (let ((title (car item))
                (arg (second item)))
            (if (equal arg default-item)
                (setf default i))
            (push (om-new-leafmenu  title #'(lambda ()
                                              (when (funcall fun container arg)  
                                                (update-dailo menu title))))
                  itemlist)))
    (setf menu (om-create-menu 'pair-pop-up-menu (reverse itemlist)))  
    (when default
        (om-set-menu-default-item menu default))
    (when itemtext
        (setf (dialo menu) itemtext))
    (om-open-pop-up-menu menu container)))





;;;===================
;;; OM cursors 
;;;===================

(defvar *om-box-cursor* nil)
(defvar *om-contex-cursor* nil)
(defvar *om-loupe-cursor* nil)
(defvar *om-tree-cursor* nil)
(defvar *om-pack-cursor* nil)
(defvar *om-point-cursor* nil)
(defvar *om-hand-cursor* nil)
(defvar *om-hand-bpf-cursor* nil)
(defvar *om-addbpf-cursor* nil)
(defvar *om-pen-cursor* nil)


(defun init-curs ()
   (setf *om-box-cursor* (om-make-cursor "box-cursor"))
   (setf *om-tree-cursor* (om-make-cursor "tree-cursor"))
   (setf *om-pack-cursor* (om-make-cursor "pack-cursor"))
   (setf *om-contex-cursor* (om-make-cursor "contex-cursor"))
   (setf *om-loupe-cursor* (om-make-cursor "loupe-cursor" (om-make-point 6 6)))
   (setf *om-pen-cursor* (om-make-cursor "pen-cursor" (om-make-point 2 10)))
   (setf *om-addbpf-cursor* (om-make-cursor "add-bpf-cursor" (om-make-point 2 10)))
   (setf *om-point-cursor* (om-make-cursor "point-cursor" (om-make-point 4 4)))
   (setf *om-hand-bpf-cursor* (om-make-cursor "main-bpf-cursor" (om-make-point 10 10)))
   )

(om-add-init-func 'init-curs)

;;;=======================
;;; d&d classes
;;;=======================

(defclass om-view-drop (om-drop-view) ()
   ;(:default-initargs nil
     ;;;:drag-allow-move-p t
     ;;;:drag-accepted-flavor-list (list :|OMVW| :|hfs | #$flavorTypePromiseHFS :|PICT| :|OMSC|))
     ;:drag-accepted-flavor-list (list :|OMVW| :|PICT| :|OMSC|)
   ;  )
   (:documentation "Abstract class, all view which wants to accept drops must inherit from this class.#enddoc#")
   )

(defclass om-view-drag (om-drag-view om-view-drop) () 
   (:documentation "Abstract class, all view which wants to be draggable and droppable must inherit from this class.#enddoc#")
   )

;=======================================================================
; Text dialog item which allow drop used for names in boxes and ws icons
; this class allow its text edition used in controls
;=======================================================================
(omg-defclass om-static-text-drag (om-view-drag om-static-text select-object) ()
   (:documentation "This is the class for all graphic-text which allow drag and drop
i.e. boxes'names, default values in slots, etc. #enddoc#
It's posible also to edit this text by double click into it,
i.e. inputs edition, change the name for a folder, etc.
#seealso# (box-dialog-name ttybox icon-finder-name) #seealso#
#selected-p# used to hilite the text if this is selected.#selected-p#"))

                                        
(defmethod (setf selected-p) (selected-p (self om-static-text-drag)) 
   "Set the slot selected-p and hilite the text if selected-p equal True"
   (if selected-p
       (progn
         (om-set-bg-color self *om-text-select-color*)
         (om-set-fg-color self *om-white-color*))
     (progn
       (om-set-fg-color self *om-black-color*)
       (om-set-bg-color self *om-transparent-color*)
     )))


(defmethod om-drag-selection-p ((self om-static-text-drag) mouse-position)
   "The Drag is allowed for om-static-text-drag if the control key is not down"
   (declare (ignore mouse-position))
   (not (om-control-key-p)))


;;;=======================
;;; drop view
;;;=======================


(omg-defclass drop-area (OM-View om-view-drop) 
              ((object :accessor object :initarg :object :initform nil)))

(defmethod get-drag-object ((self drop-area)) self)

(defmethod om-drag-selection-p ((self drop-area) mouse-position)
  (declare (ignore mouse-position)) nil)

(defmethod om-view-drag-hilite-p ((self drop-area)) t)

(defmethod om-drag-enter-view ((self drop-area))
  (call-next-method))


;==========================================================
; DRAG&DROP --> ACTION

(omg-defclass unaire-fun-view (OMSimpleFrame om-view-drop) 
   ((drop-action :initform nil :initarg :drop-action :accessor drop-action))
   (:documentation "The unaire-fun-view is a special view which perform an action
when you drag a OMFrame in it.#enddoc#
#seealso# (unaire-fun-icon) #seealso#
#action# This slot contains a function with one parameter (the OMFrame dragged),
the object of the unaire-fun-view is itself,
this function is executed each time that the user drag an OMFrame 
into the unaire-fun-view.#action#"))

(defmethod om-draw-contents ((self unaire-fun-view))
   (call-next-method)
   ;(om-draw-view-outline self)
   )

(defmethod do-default-action ((self t) icon)
  (declare (ignore icon)) t)

(defmethod om-view-click-handler ((self unaire-fun-view) where)
   "Execute the method do-default-action specified for the object of SELF"
   (declare (ignore where))
   (call-next-method)
   (do-default-action (object self) self))

(defmethod initialize-instance :after ((self unaire-fun-view) &key controls)
   "Put the object of self to self"
   (declare (ignore controls))
   (unless (object self) (setf (object self) self)))



;=== simple horizontal bar component ===
(omg-defclass bar-item (om-item-view) 
  ((fg-color :accessor fg-color :initarg :fg-color :initform *om-black-color*)))

(defmethod om-draw-contents ((self bar-item))
   (call-next-method)
   (om-with-focused-view  self 
     (om-with-fg-color self (fg-color self)
       (om-draw-line 0 0 (w self) 0)
       )
     ))

;;;=====================
;;; 3DBORDER VIEW
;;;=====================

(omg-defclass 3Dborder-view (om-view)
  ((c+ :accessor c+ :initform (om-make-color 0.835 0.835 0.843) :initarg :c+)
   (c++ :accessor c++ :initform (om-make-color 0.87 0.87 0.88) :initarg :c++)
   (c- :accessor c- :initform (om-make-color 0.604 0.604 0.604) :initarg :c-)
   (c-- :accessor c-- :initform (om-make-color 0.514 0.514 0.514) :initarg :c--))
  )

;(defmethod om-subviews ((self 3Dborder-view))
;  (remove 'view-border (call-next-method) :key 'type-of))
  
;(defclass view-border (om-item-view)
;  ((mainpane :accessor mainpane :initform nil :initarg :mainpane)))

;(defmethod initialize-instance :after ((self 3Dborder-view) &rest args)
  ;(om-add-subviews self  (om-make-view 'view-border :mainpane self
  ;                                     :size (om-view-size self)))
;  )
 

;(defmethod mainpane ((self 3Dborder-view)) self)

(defmethod om-draw-contents ((self 3Dborder-view))
  (call-next-method)
  (let ((x (om-h-scroll-position self))
        (y (om-v-scroll-position self))
        (w (om-point-h (om-interior-size self)))
        (h (om-point-v (om-interior-size self))))
    (draw-3D-border self x y (+ x w) (+ y h))
    ))

(defun draw-3D-border (self x y xx yy)
  ;(print (list self x y xx yy))
  (om-with-focused-view self
        ;(om-fill-rect 0 0 (w self) (h self))
        (om-with-fg-color self (c++ self)
          (om-draw-line (+ x 1) y (- xx 1) y) 
          (om-draw-line x (+ y 1) x (- yy 1)))
        (om-with-fg-color self (c+ self)
          (om-draw-line (+ x 2) (+ y 1) (- xx 2) (+ y 1)) 
          (om-draw-line (+ x 1) (+ y 2) (+ x 1) (- yy 2)))
        (om-with-fg-color self (c-- self)
          (om-draw-line (+ x 1) (- yy 1) (- xx 1) (- yy 1)) 
          (om-draw-line (- xx 1) (+ y 1) (- xx 1) (- yy 1)))
        (om-with-fg-color self (c- self)
          (om-draw-line (+ x 2) (- yy 2) (- xx 2) (- yy 2)) 
          (om-draw-line (- xx 2) (+ y 2) (- xx 2) (- yy 2)))
        ))

;;; ====================
;;; TITLE BAR 
;;;=====================

(defvar *titlebars-h* nil)
(setf *titlebars-h* 26)

(defclass editor-titlebar (3dborder-view) ()
  (:default-initargs :draw-with-buffer t))

;=====================================
;   MIXIN CLASSES
;=====================================

(defclass select-object () 
  ((id :initform nil :accessor id :initarg :id)
   (selected-p :initform nil :accessor selected-p :initarg :selected-p)))

(defmethod selected-p ((self t)) nil)
(defmethod (setf selected-p) (val (self t)) nil)

;;; ====================
;;; NEW SUPERCLASS 
;;;=====================

(omg-defclass object-editor (om-view) 
  ((title-bar :initarg :title-bar :accessor title-bar :initform nil)))

(defmethod get-control-h ((self object-editor)) *titlebars-h*)

(defmethod editor-minimum-size ((self object-editor)) (om-make-point 200 100))

(defmethod get-titlebar-class ((self object-editor)) 'editor-titlebar)

(defmethod init-titlebar ((self object-editor))
  (let ((name (string (class-name (class-of (object self))))))
    (om-add-subviews (title-bar self)
                     (om-make-dialog-item 'om-static-text (om-make-point 10 2) 
                                          (om-make-point 160 ;(+ (om-string-size name *om-default-font2b*) 4)
                                                         18)
                                          name
                                          :bg-color *editor-bar-color*
                                          :fg-color *om-dark-gray-color*
                                          :font *om-default-font1b*
                                          ))))

(defmethod update-titlebar ((self object-editor)) 
  (when (title-bar self)
    (apply 'om-remove-subviews (cons (title-bar self) (om-subviews (title-bar self))))
    (init-titlebar self)))


(defmethod change-text ((self editor-titlebar) text)
  (om-set-dialog-item-text (car (om-subviews self)) text)
  (om-invalidate-view self))

(defmethod initialize-instance :after ((self object-editor) &rest initargs)
  (om-add-subviews self
                   (setf (title-bar self) (om-make-view (get-titlebar-class self) :position (om-make-point 0 0)
                                                       :size (om-make-point (w self) *titlebars-h*)
                                                       :bg-color *editor-bar-color*
                                                       :c++ *editor-bar-color++*
                                                       :c+ *editor-bar-color+*
                                                       :c-- *editor-bar-color--*
                                                       :c- *editor-bar-color-*
                                                       )))
  (init-titlebar self)
  )

(defmethod update-subviews ((self object-editor))
   (when (title-bar self)
     (om-set-view-size  (title-bar self) (om-make-point (w self) *titlebars-h*)))
   (om-invalidate-view self))



;==========================================================
; BUTTON with pict in "resources/di/"
 
(omg-defclass om-icon-button (om-item-view select-object) ;om-transparent-view
              ((icon1 :initform nil :accessor icon1 :initarg :icon1)
               (icon2 :initform nil :accessor icon2 :initarg :icon2)
               (action :initform nil :accessor action :initarg :action)
               (lock-push :initform nil :accessor lock-push :initarg :lock-push)
               (enabled :initform t :accessor enabled :initarg :enabled)
               (text :initform nil :accessor text :initarg :text)
               (fg-color :initform nil :accessor fg-color :initarg :fg-color)
               (font :initform nil :accessor font :initarg :font)))
             

(defmethod om-set-fg-color ((self om-icon-button) color)
  (setf (fg-color self) color)
  (om-invalidate-view self t))

(defmethod om-view-doubleclick-handler ((self om-icon-button) where)
  (om-view-click-handler self where))
 
(defmethod om-view-click-handler ((self om-icon-button) where)
   "this function call the slot action of SELF with the parameter SELF"
   (declare (ignore where))
   (when (enabled self)
     (setf (selected-p self) t)
     (om-redraw-view self)
     (om-init-motion-functions self nil 'release-button-action)))

(defmethod release-button-action ((self om-icon-button) where)
  (when (action self)
    (om-with-error-handle 
      (apply (action self) (list self))))
  (unless (lock-push self) (setf (selected-p self) nil))
  (om-invalidate-view self t))

(defmethod om-draw-contents ((self om-icon-button))
   (call-next-method)
   (om-without-interrupts
     (let* ((icon (or (and (selected-p self) (icon2 self)) (icon1 self)))
            (iconhdlr (om-load-and-store-picture icon 'di)))
       (when iconhdlr
         (om-with-focused-view self
           ;(print (list self iconhdlr))
           (om-draw-picture self iconhdlr :size (om-view-size self)))
         ))
     (when (text self)
       (let* ((ff (or (font self) *om-default-font1*))
              (cc (or (fg-color self) *om-black-color*))
              (yy (round (+ (- (om-string-h ff) (if (selected-p self) 5 6)) (h self)) 2))
              (xx (max 0 (- (round (w self) 2) (ceiling (om-string-size (text self) ff) 2)))))
         (om-with-focused-view self
           (om-with-fg-color self cc
          (om-with-font ff
                        (om-draw-string xx yy (text self)))))))
     (when (and (lock-push self) (selected-p self) (not (icon2 self)))
       (om-with-focused-view self
         (om-draw-hilite-rect 0 0 (w self) (h self) *om-black-color*)))))


;;;===========================================

(omg-defclass picture-view (om-item-view)
   ((pict :initform nil :initarg :pict :accessor pict)))

(defmethod om-draw-contents ((self picture-view))
   (call-next-method)
   (when (pict self)
     (om-draw-picture self (pict self) :size (om-view-size self))))





