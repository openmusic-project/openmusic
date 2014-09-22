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
;This file defines a simple frame class for temporal boxes in a maquette.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defvar *shad-white*)
(defvar *shad-black*)
(defvar *shad-gray*)

(setf *shad-white* *om-white-color*)  
(setf *shad-black* *om-black-color*)  
(setf *shad-gray* *om-gray-color*)

(defmethod draw-frame-shadows ((self om-graphic-object))
   (let ((y1 (h self)) 
         (x1 (w self)))
     ;(om-with-fg-color self *shad-gray*
     ;      (om-draw-line 1 (- y1 2) (- x1 2) (- y1 2))
     ;      (om-draw-line (- x1 2) (- y1 2) (- x1 2) 0))
    ;(om-with-fg-color self *shad-white*
    ;  (om-draw-line 0 0 x1 0)
    ;  (om-draw-line 0 0 0 y1))
    ; (om-with-fg-color self *shad-black*
     ; (om-draw-line 0 (- y1 1) x1 (- y1 1))
     ; (om-draw-line (- x1 1) y1 (- x1 1) 0))
     (om-with-fg-color self *om-dark-gray-color*
       (om-draw-rect 0 0 (- x1 1) (- y1 1)))
     ))

;=========================================================
;TEMPORAL OBJECTS FRAME
;=========================================================
;===========================
;INPUTS
;===========================

(omg-defclass input-tempobj-frame (input-funboxframe) ())

(defmethod initialize-instance :after ((self input-tempobj-frame) &key controls)
  (declare (ignore controls))
  (setf (iconID self) 146))

;;; new :text-view container = panel
(defmethod om-view-click-handler ((self input-tempobj-frame) where)
  (declare (ignore where))
  (cond
   ;;;((om-control-key-p) (om-set-help t))
   ((om-command-key-p) (when (connected? (object self))
                        (disconnect-box (om-view-container self) self)))
   ((menu-input-p (object self))
    (om-set-view-position (thepopup (object self))
                          (om-make-point (+ (x (om-view-container self)) (x self))
                                         (- (y (om-view-container self)) 15)))
    (om-add-subviews (om-view-container (om-view-container self)) (thepopup (object self)))
    (om-view-click-handler (thepopup (object self)) (om-make-point 5 5))
    (om-remove-subviews (om-view-container (om-view-container self)) (thepopup (object self)))
    (setf (value (object self)) (in-value (thepopup (object self)))))
   (t (let* ((panel (om-view-container (om-view-container self)))
             (container (editor panel)))
        (unless (connected? (object self))
          (when (text-view container)
            (om-remove-subviews panel (text-view container))
            (setf (text-view container) nil))
          (let ((thetext (format () "~S" (value (object self)))))
            (setf (text-view container) (om-make-dialog-item 'edit-text-enter-view
                                                             ;(pos-panel2editor panel
                                                             (om-make-point (+ (x (om-view-container self)) (x self))
                                                                            (- (y (om-view-container self)) 20))
                                                              (om-make-point (get-name-size thetext) 12)
                                                             thetext
                                                             
                                                             :allow-returns t
                                                             :object self
                                                             :container panel
                                                             :font *om-default-font2*))))))))


;===========================
;OUTPUTS
;===========================
(omg-defclass outtempobj (outfleche) ())

(defmethod initialize-instance :after ((self outtempobj) &key controls)
  (declare (ignore controls))
  (setf (iconID self) 401))

(defmethod om-view-click-handler ((self outtempobj) where)
   (if (show-con? (om-view-container self))
     (unless (om-shift-key-p) 
       (call-next-method))
     (click-in-temp-box (om-view-container self) (om-add-points (om-view-position self) where))))

(defmethod om-view-doubleclick-handler ((self outtempobj) where)
   (if (show-con? (om-view-container self)) (om-view-click-handler self where)
       (doubleclick-in-temp-box (om-view-container self) (om-add-points (om-view-position self) where))))

;===========================
;FRAME
;===========================
(omg-defclass tempobjframe (om-view-drag boxframe om-view-drop) 
   ((mode :initform 'normal :accessor mode)
    (minipict :initform nil :accessor minipict))
   (:documentation "Simple frame for temporalbox boxes in maquettes. #enddoc#
#seealso# (OMBoxEditCall) #seealso#"))

(defmethod close-frame ((box tempobjframe))
  "Close all editors and kill pictures if 'box' is in pict mode."
  (when (reference (object box))
    (when (and (EditorFrame (reference (object box))) 
               (not (have-persistant-patch? (object box))))
      (om-close-window (window (EditorFrame (reference (object box)))))))
  (when (EditorFrame (object box))
    (om-close-window (window (EditorFrame (object box)))))
  (when (minipict box) 
    (om-kill-picture (minipict box))
    (setf (minipict box) nil))
  (setf (frames (object box)) nil))


(defmethod change-edit-mode ((self tempobjframe))
   "Show or hide pict for temporal boxes."
   (if (showpict (object self))
     (progn
       (setf (showpict (object self)) nil)
       (when (and (minipict self) (not (maquette-p (reference (object self)))))
         (om-kill-picture (minipict self))
         (setf (minipict self) nil))
       (om-invalidate-view self t))
     (progn
       (setf (showpict (object self)) t)
       (update-miniview self (soft-get-mus-ob (object self))))))

(defmethod set-show-box-name ((self tempobjframe))
  (setf (show-name (object self)) (not (show-name (object self))))
  (om-invalidate-view self))


(defclass maq-resize-box-class () ())

(defclass maq-c-resize-box (maq-resize-box-class c-resize-box) 
  ((bottomresize :accessor bottomresize :initform nil)
   (rightresize :accessor rightresize :initform nil)))

(defclass maq-b-resize-box (maq-resize-box-class c-resize-box) ())

(defmethod om-view-cursor ((self maq-b-resize-box)) *om-verti-size-cursor*)

(defclass maq-r-resize-box (maq-resize-box-class c-resize-box) ())

(defmethod om-view-cursor ((self maq-r-resize-box)) *om-horiz-size-cursor*)

(defmethod om-set-view-position :after ((self maq-c-resize-box) pos)
  (when (bottomresize self)
    (om-set-view-position (bottomresize self) (om-make-point 0 (y self)))
    (om-set-view-size (bottomresize self) (om-make-point (x self) (h self))))
  (when (rightresize self)
    (om-set-view-position (rightresize self) (om-make-point (x self) 0))
    (om-set-view-size (rightresize self) (om-make-point (w self) (y self)))))
  

(defmethod om-view-click-handler ((self maq-resize-box-class) pos) (call-next-method))

(defmethod om-view-cursor ((self maq-c-resize-box)) (call-next-method))

(defmethod resize-box-motion ((self maq-b-resize-box) pos)
  (let* ((panel (om-view-container (get-box-frame self)))
           (initpoint (om-convert-coordinates pos self panel))
           (initsize (om-add-points (om-add-points (om-view-size (get-box-frame self)) (om-view-position (get-box-frame self)))
                                     (om-make-point 0 (- (om-point-v pos) (om-point-v *init-resize-pos*)))))
           (rx (om-point-h initsize))
           (ry (om-point-v initsize))
           (rect  (om-init-point-movable-object panel)))
     (om-update-movable-object panel (first rect) (second rect) (max 4  (- rx (first rect))) (max 4 (- ry (second rect) )))))

(defmethod resize-box-release ((self maq-b-resize-box) pos) 
  (let* ((boxframe (get-box-frame self))
         (panel (om-view-container boxframe))
         (initpoint (om-convert-coordinates pos self panel ))
         (initsize (om-add-points (om-view-size boxframe) (om-view-position boxframe)))
         (initsizepos (om-add-points initsize (om-make-point 0 (- (om-point-v pos) (om-point-v *init-resize-pos*)))))         
         (rx (om-point-h initsizepos))
         (ry (om-point-v initsizepos))
         (rect  (om-init-point-movable-object panel)))
     (om-erase-movable-object panel)
     (change-boxframe-size boxframe (om-make-point (- rx (first rect) ) (- ry (second rect) )))
     (om-invalidate-rectangle panel (x boxframe) (y boxframe) (w boxframe) (h boxframe)) 
     (setf *init-resize-pos* nil)
     ))

(defmethod resize-box-motion ((self maq-r-resize-box) pos)
  (let* ((panel (om-view-container (get-box-frame self)))
           (initpoint (om-convert-coordinates pos self panel))
           (initsize (om-add-points (om-add-points (om-view-size (get-box-frame self)) (om-view-position (get-box-frame self)))
                                     (om-make-point (- (om-point-h pos) (om-point-h *init-resize-pos*)) 0)))
           (rx (om-point-h initsize))
           (ry (om-point-v initsize))
           (rect  (om-init-point-movable-object panel)))
     (om-update-movable-object panel (first rect) (second rect) (max 4  (- rx (first rect))) (max 4 (- ry (second rect) )))))

(defmethod resize-box-release ((self maq-r-resize-box) pos) 
  (let* ((boxframe (get-box-frame self))
         (panel (om-view-container boxframe))
         (initpoint (om-convert-coordinates pos self panel ))
         (initsize (om-add-points (om-view-size boxframe) (om-view-position boxframe)))
         (initsizepos (om-add-points initsize (om-make-point (- (om-point-h pos) (om-point-h *init-resize-pos*)) 0)))         
         (rx (om-point-h initsizepos))
         (ry (om-point-v initsizepos))
         (rect  (om-init-point-movable-object panel)))
     (om-erase-movable-object panel)
     (change-boxframe-size boxframe (om-make-point (- rx (first rect) ) (- ry (second rect) )))
     ;(om-invalidate-rectangle panel (x boxframe) (y boxframe) (w boxframe) (h boxframe)) 
     (setf *init-resize-pos* nil)
     ))

(defmethod add-box-resize ((self tempobjframe))
   (om-add-subviews self
                    (setf (resize-box self)
                          (om-make-view 'maq-c-resize-box
                                        :size (om-make-point 8 8)
                                        :position (om-make-point (- (w self) 8) (- (h self) 8))))
                    ;(setf (bottomresize (resize-box self))
                    ;      (om-make-view 'maq-b-resize-box
                    ;                    :size (om-make-point (- (w self) 8) 8)
                    ;                    :position (om-make-point 0 (- (h self) 8))
                    ;                    ))
                    ;(setf (rightresize (resize-box self))
                    ;      (om-make-view 'maq-r-resize-box
                    ;                    :size (om-make-point 8 (- (h self) 8))
                    ;                    :position (om-make-point (- (w self) 8) 0)
                    ;                    ))
                    ))


;(defmethod add-box-resize ((self tempobjframe))
;   (om-add-subviews self
;                 (setf (resize-box self)
;                       (om-make-view 'c-resize-box
;                         :size (om-make-point 8 8)
;                         :position (om-make-point (- (w self) 8) (- (h self) 8))))))


   
;-------DRAW ------

(defmethod get-impulsion-pict ((self t)) *impulsion-pict*)

(defvar *maq-show-icons* t)
(defvar *minipict-bg* :white)
(defvar *minipict-mode* :score)

(defmethod om-draw-contents ((self tempobjframe))
   (om-with-focused-view self
     (if (zerop (extend (object self)))
         (om-draw-picture self (get-impulsion-pict (car (value (object self)))) :size (om-make-point (w self) (h self)))
       (if (showpict (object self))
           (progn
             (if *minipict-bg* 
                 (om-with-fg-color self (if (equal *minipict-bg* :white) *om-white-color* (colorframe (object self)))
                   (om-fill-rect 0 0 (w self) (h self))))
             (draw-editor-mode (get-obj-to-draw (object self)) self)
             )
         (let* ((durinx (norme2pixel (om-view-container self) 'x (extend (object self)))))
           (cond ((has-picture-p self)
                  (om-draw-picture self (thepict (pictu (object self))) :size (om-make-point (w self) (h self)))
                  (draw-carre self t))
                 ((and (pictu (object self)) (pict-pathname (pictu (object self))))
                  (draw-lost-picture (pictu (object self)) self)
                  (draw-carre self t))
                 (t
                  (om-with-fg-color self (colorframe (object self))
                    (draw-frame self (object self) 0 0  durinx (h self)))
                  (if (active-mode self)
                      (draw-carre self t)
                    (draw-frame-shadows self))))
           )))
     (when (show-name (object self))
       (om-with-fg-color self *om-dark-gray-color*
       (om-with-font *om-default-font1b*
       (om-draw-string 4 (- (h self) 5) (name (object self))))))
     )
     (when (mute (object self)) 
       (let ((iconparams (get&corrige-icon 341))
             (size (om-make-point 16 16)))       ;;(om-make-point (round (w self) 4) (round (h self) 4))))
         (om-draw-picture self (second iconparams) :pos (om-make-point 
                                                  (- (round (w self) 2) (round (om-point-h size) 2)) 
                                                  (- (round (h self) 2) (om-point-v size)))
                        :size size)
                     ))
   (when (lock (object self)) 
     (let ((iconparams (get&corrige-icon 340))
           (size (om-make-point 16 16)))       ;;(om-make-point (round (w self) 4) (round (h self) 4))))
       (om-draw-picture self (second iconparams)
                        :pos (om-make-point 
                         (- (round (w self) 2) (round (om-point-h size) 2)) 
                         (round (h self) 2))
                        :size size)
                     ))
   
   (when *maq-show-icons*
     (let ((iconparams (get&corrige-icon (icon (reference (object self)))))
           (size (om-make-point 10 10)))
       (om-draw-picture self (second iconparams) :pos (om-make-point (- (w self) 16) 2) :size size)))

  (when (show-con? self)
    (call-next-method)))


(defmethod score-draw-mini-view ((self tempobjframe) value)
  (if (equal *minipict-mode* :pianoroll)
      (draw-mini-piano-roll value self (mv-view-size value self))
    (if (minipict self)
        (let ((x0 (initx self)) (y0 (inity self)) (pictsize (om-get-picture-size (minipict self))))
          (om-draw-picture self (minipict self) :pos (om-make-point x0 y0) :size pictsize))
      (om-with-focused-view self
        (draw-mini-obj value self (mv-font-size value) (mv-view-size value self))))))


(defmethod om-draw-contents ((self outtempobj))
  (when (show-con? (om-view-container self))
    (call-next-method)))

(defmethod om-draw-contents ((self input-tempobj-frame))
  (when (show-con? (om-view-container self))
    (call-next-method)))


(defmethod draw-before-box ((self tempobjframe)))
(defmethod draw-after-box  ((self tempobjframe)))
(defmethod centre-icon     ((self tempobjframe)))
(defmethod has-picture-p   ((self tempobjframe))
  (and (pictu (object self)) (thepict (pictu (object self)))))

(defmethod draw-frame ((self tempobjframe) obj x0 y0 x1 y1)
  (om-fill-rect x0 y0 x1 y1)
  ;;; new : dessiner le reste du tempobj
  (om-with-pen (self :pattern *om-gray-pattern*)
    (om-fill-rect x1 0 (- (w self) x1) (h self)))
  )


(defmethod box-draw-connections ((self tempobjframe) val)
   (declare (ignore val))
   (when (show-con? self) (call-next-method)))

(defmethod add-subview-extra ((self tempobjframe))
   (when (showpict (object self))
     (init-miniview self (soft-get-mus-ob (object self)))))

(defmethod move-frame-delta ((self tempobjframe) dir) 
  (if (om-option-key-p)
     (move-miniview self dir) 
     (let* ((params (params (object (editor (om-view-container self)))))
            (snapmodex (car (snap params)))
            (snapmodey (cadr (snap params)))
            (xsnap (car (xparam params)))
            (ysnap (car (yparam params)))
            (metricpar (metricparam params)))
       
      (if (or (= dir 0) (= dir 1))
           ;;; VERTICAL MOVE
           (if snapmodey 
               ;;; SNAP Y
               (let* ((py (posy (object self)))
                      (roundedpy (* ysnap (funcall (if (= 0 dir) 'ceiling 'floor) py ysnap)))
                      (factor (if (om-shift-key-p) 10 1))
                      (newpy (+ roundedpy (* (if (= 0 dir) (- ysnap) ysnap) factor)))
                      (pixnum (norme2pixel (om-view-container self) 'y (- newpy py))))
                 (loop while (< (abs pixnum) 1) do
                   (setf factor (* 10 factor)
                         newpy (+ roundedpy (* (if (= 0 dir) (- ysnap) ysnap) factor))
                         pixnum (norme2pixel (om-view-container self) 'y (- newpy offset))))
                 (omGMoveObject self (om-make-point (om-point-x (om-view-position self))
                                                    (+ (om-point-y (om-view-position self)) pixnum))))
             ;;; PIXEL MOVE
           (let* ((move 1)
                  (pixnum (norme2pixel (om-view-container self) 'y move)))
             (loop while (< pixnum 1) do
                   (setf move (* 10 move))
                   (setf pixnum (norme2pixel (om-view-container self) 'y move)))
             (when (om-shift-key-p) 
               (setf move (* 10 move))
               (setf pixnum (norme2pixel (om-view-container self) 'y move)))
             (omGMoveObject self (om-make-point (om-point-x (om-view-position self))
                                                (+ (om-point-y (om-view-position self)) (if (= 0 dir) (- pixnum) pixnum))))
             ))

         ;;; HORIZONTAL MOVE
         (cond 
          ((equal snapmodex 'abs)
           (let* ((offset (offset (object self)))
                 (roundedoffset (* xsnap (funcall (if (= 3 dir) 'ceiling 'floor) offset xsnap)))
                 (factor (if (om-shift-key-p) 10 1))
                 (newoffset (+ roundedoffset (* (if (= 3 dir) (- xsnap) xsnap) factor)))
                 (pixnum (norme2pixel (om-view-container self) 'x (- newoffset offset))))
             (loop while (< (abs pixnum) 1) do
                   (setf factor (* 10 factor)
                         newoffset (+ roundedoffset (* (if (= 3 dir) (- xsnap) xsnap) factor))
                         pixnum (norme2pixel (om-view-container self) 'x (- newoffset offset))))
             (omGMoveObject self (om-make-point (+ (om-point-x (om-view-position self)) pixnum)
                                                (om-point-y (om-view-position self))))
             ))
          
          ((equal snapmodex 'metric)
           (let* ((mesure (car (second metricpar)))
                  (tempo (first metricpar))
                  (offset (offset (object self)))
                  (snap (if (om-shift-key-p)
                               (* 1000 (size-pulsation tempo mesure) (car mesure))
                           (* 1000 (size-pulsation tempo mesure) (/ 4 (third metricpar)))))
                  (factor 1)
                  (roundedoffset (* snap (funcall (if (= 3 dir) 'ceiling 'floor) offset snap)))
                  (newoffset (+ roundedoffset (* (if (= 3 dir) (- snap) snap) factor)))
                  (pixnum (norme2pixel (om-view-container self) 'x (- newoffset offset))))

             (loop while (< (abs pixnum) 1) do
                   (setf factor (* 2 factor)
                         newoffset (+ roundedoffset (* (if (= 3 dir) (- snap) snap) factor))
                         pixnum (norme2pixel (om-view-container self) 'x (- newoffset offset))))
             (omGMoveObject self (om-make-point (+ (om-point-x (om-view-position self)) pixnum)
                                             (om-point-y (om-view-position self))))
             ))
      
          (t 
           (let ((move 1))
             (setf pixnum (norme2pixel (om-view-container self) 'x move))
             (loop while (< pixnum 1) do
                   (setf move (* 10 move))
                   (setf pixnum (norme2pixel (om-view-container self) 'x move)))
             (when (om-shift-key-p)
               (setf move (* 10 move))
               (setf pixnum (norme2pixel (om-view-container self) 'x move)))
             (omGMoveObject self (om-add-points (om-view-position self) 
                                                (om-make-point (if (= dir 2) pixnum (- pixnum)) 0)))
             ))
          ))
       )
     ))


;(ms2u 1500 '(4 60) 8)
;(u2ms 2 '(4 60) 8)

;---------Events
(defmethod om-view-cursor ((self tempobjframe))
  (when (om-view-container self)
    (if (om-control-key-p)
        *om-contex-cursor*
      (case (cursor-mode (om-view-container self))
        (:zoom *om-loupe-cursor*)
        (:move *om-cross-cursor*)
        (otherwise *om-arrow-cursor*)))))
  

(defmethod om-drag-start ((self tempobjframe))
  (when (equal (cursor-mode (om-view-container self)) :normal)
    (call-next-method)))

(defmethod do-click-inbox ((self tempobjframe) where)
   (call-next-method)
   (if (equal (cursor-mode (om-view-container self)) :move) (scroll-miniview self)))

;;; dbleclick
(defmethod click-in-temp-box ((self tempobjframe) where)
   (cond ((equal (cursor-mode (om-view-container self)) :move) (scroll-miniview self))
         (t (toggle-icon-active-mode self))))

(defmethod doubleclick-in-temp-box ((self tempobjframe) where)
   (cond ((equal (cursor-mode (om-view-container self)) :move) (scroll-miniview self))
         ;;;((om-command-key-p) (om-set-help t))
         (t (OpenObjectEditor (object self)))
         ))

(defmethod eval-box ((self tempobjframe)) 
   (eval+redraw self)
   (format *om-stream* "OM ==> ~S~%" (nth 0 (value (object self)))))

(defmethod reinit-size ((self tempobjframe))
  (set-mini-param self 'deltapict (om-make-point 0 0))
  (if (maquette-p (reference (object self)))
    (setf (extend (object self)) (get-obj-dur (reference (object self))))
    (when (allowed-in-maq-p (nth 0 (value (object self))))
      (setf (extend (object self)) (get-obj-dur (nth 0 (value (object self)))))))
  (unless (=  (strech-fact (object self)) 1)
    (let* ((container (om-view-container self)))
      (setf (slot-value (object self) 'strech-fact) 1)
      (om-set-view-size self (om-make-point  (norme2pixel container 'x  (extend (object self)))
                                             (norme2pixel container 'y  (sizey (object self)))))
      (make-move-after (om-view-container self) (list self))))
  (om-invalidate-view self t))

(defmethod reinit-contents ((self tempobjframe)) 
  (setf (car (value (object self))) (get-super-default-value (type-of (car (value (object self))))))
  (update-after-evaluation self)
  (make-move-after (om-view-container self) (list self))
  (om-invalidate-view self t))

     

;----- STRECH ------------------


(defmethod change-boxframe-size ((view tempobjframe) new-size)
  (let* ((panel (om-view-container view))
         (mark-list (get-maquette-markers panel)))
    (if (is-marketed (object view) 1 mark-list)
        (om-set-view-size view (om-make-point (w view) (om-point-v new-size))))
    (when (setf new-size (allow-new-size view new-size))
      (setf new-size (snap-size panel new-size (om-view-position view)))
      (when (> (om-point-h new-size) 0)
        (om-set-view-size view new-size)
        ;; new
        (when (om-view-container view)
          (setf (slot-value (object view) 'sizey) (pixel2norme (om-view-container view) 'y (h view)))) 
        ;;
        (if (zerop (extend (object view)))
            (setf (slot-value (object view) 'strech-fact) 1000)
          (setf (slot-value (object view) 'strech-fact)
                (/ (om-point-h new-size) (norme2pixel panel 'x (extend (object view))))))
          ;(draw-mark-lines panel nil)
        (update-after-mark panel)
          ;(draw-mark-lines panel)
          ;;; !!!
          (when (and (allow-strech-p (soft-get-mus-ob (object view)) 1) (showpict (object view)))
            (update-miniview view (soft-get-mus-ob (object view))))
          (when (patch-temp-p (reference (object view)))
            (setf (mode view) 'changed))))
    (om-invalidate-view view t)
    (make-move-after panel (list view))))

(defun snap-size (maqpanel size pos)
  (let* ((maqpos (pixel2point maqpanel (om-add-points size pos)))
         (posy (om-point-v maqpos))
         (newx2 (om-point-h maqpos))
         (themaquette (object (editor maqpanel)))
         (metricpar (metricparam (params themaquette)))
         (xpar (xparam (params themaquette)))
         (ypar (yparam (params themaquette))))
         
    (if (and (rulermetric (editor maqpanel)) (equal 'metric (car (snap (params themaquette)))))
        ;(setf newoffset (u2ms (ms2u newoffset (first metricpar) (third metricpar)) (first metricpar) (third metricpar)))
        (let* ((mesure (get-ieme-measure (car (ms2listmetric newx2 (first metricpar) (second metricpar) (third metricpar) (fourth metricpar)))
                                         (second metricpar) (fourth metricpar)))
              (minstep (* 1000 (size-pulsation (first metricpar) mesure) (/ 4 (third metricpar)))))
          (setf newx2 (* minstep (round newx2 minstep)))
          )
      (if (equal 'abs (car (snap (params themaquette))))
          (setf newx2 (* (car xpar) (round newx2 (car xpar))))))
    (when (cadr (snap (params themaquette)))
      (setf posy (calcule-y-pos posy (car ypar))))
    (let ((res (om-subtract-points 
                (point2pixel maqpanel (om-make-point (max 0 newx2) posy) (get-system-etat maqpanel))
                pos)))
      (when (= 0 (om-point-y res))
        (setf res (om-make-point (om-point-x res) 10)))
      res
    )))


(defmethod allow-new-size ((self tempobjframe) new-pos)
   (let (rep)
     (if (and (allow-strech-p (soft-get-mus-ob (object self)) 0) (not (om-shift-key-p)))
       (if (and (> (om-point-h new-pos) 0) (> (om-point-v new-pos) 0))
         (setf rep new-pos))
       (if (> (om-point-v new-pos) 0)
         (setf rep (om-make-point (w self) (om-point-v new-pos)))))
     rep))

(defmethod om-set-view-size :after ((self tempobjframe) size) 
  (declare (ignore size))
  (if (not (zerop (extend (object self))))
     (let ((numins (length (inputframes self)))
           (numouts (length (outframes self))))
       ;(when (om-view-container self)
       ;  (setf (slot-value (object self) 'sizey) (pixel2norme (om-view-container self) 'y (h self)))) 
       (om-set-view-position (resize-box self) (om-make-point (- (w self) 8) (- (h self) 8)))
       (loop for input in (inputframes self)
             for i from 1 to numins do
             (om-set-view-position input (om-make-point (- (* i  (round (w self) (+ numins 1))) 6) 0)))
       (loop for output in (outframes self)
             for i from 1 to numouts do
             (om-set-view-position output (om-make-point (- (* i (round (w self) (+ numouts 1))) 6) 
                                                         (- (h self) 8))))
       )))


;--------------------------------
;MOVE
;--------------------------------

(defmethod OMGMoveObject ((self tempobjframe) new-position)
   (when (< (om-point-h (get-offset/posy-from-pixel (om-view-container self) new-position)) 0)
       ;;;(om-beep-msg "Object must start after 0")
       (setf new-position (om-make-point 0 (om-point-v new-position))))
   (let* ((container (om-view-container self))
          (mark-list (get-maquette-markers container)))
       ;(draw-mark-lines container nil)
     (if (is-marketed (object self) 0 mark-list)
         (MoveTempObject self (om-make-point (x self) (om-point-v new-position)))
       (update-movement self new-position))
     (update-after-mark container)
     ;; ***
     ;;(draw-mark-lines container)
     
     (omng-MoveObject (object self) new-position)
     ;(om-invalidate-view container)
     ))
    
(defmethod MoveTempObject ((self tempobjframe) new-position)
   (let ((maqpos))
     (mapc #'(lambda (conection)
               (draw-connection conection nil)) (connections self))
     (om-set-view-position self new-position)
     (setf maqpos (get-offset/posy-from-pixel (om-view-container self) new-position))
     (setf (slot-value (object self) 'offset) (om-point-h maqpos))
     (setf (slot-value (object self) 'posy)  (om-point-v maqpos))
     (om-set-view-position self (get-offset/posy-in-pixel self (om-view-container self)))))

(defmethod update-movement ((self tempobjframe) new-position)
   (let* ((container (om-view-container self))
          (mark-list (get-maquette-markers container))
          (finmarker (is-marketed (object self) 1 mark-list)))
     (if (and finmarker (not (allow-strech-p (soft-get-mus-ob (object self)) 0)))
       (om-beep-msg "Sorry i can not strech this object")
       (unless (and finmarker (>= (om-point-h new-position) (x+w self)))
         (MoveTempObject self new-position)
         (when finmarker 
           (setf (slot-value (object self) 'strech-fact)
                 (/ (- (x (car (frames finmarker))) (x self)) (norme2pixel container 'x (extend (object self)))))
           (om-set-view-size self (om-make-point (- (x (car (frames finmarker))) (x self)) (h self))))
         (when (patch-temp-p (reference (object self)))
           (setf (mode self) 'changed))
         ;(make-move-after container (list self)) ;; test
         )))
   (om-invalidate-view self t))


;--------------------------------
;Size and position update
;--------------------------------

(defmethod size-offset-frame-update ((self tempobjframe) container)
   (if (zerop (extend (object self)))
     (om-set-view-size self (om-make-point 10 (norme2pixel container 'y (sizey (object self)))))
     (om-set-view-size self (om-make-point  (norme2pixel container 'x (extend (object self)))
                                      (norme2pixel container 'y (sizey (object self))))))
   (om-set-view-position self (get-offset/posy-in-pixel self container))
   (when (showpict (object self))
     (update-miniview self (soft-get-mus-ob (object self))))
   (setf (mode self) 'normal))

(defmethod init-size&position ((self tempobjframe) container)
  (let* ((obj (object self))
         (y-size (max (norme2pixel container 'y (sizey obj)) 1)))
    (cond
     ((zerop (extend obj))
      (om-set-view-position self (get-offset/posy-in-pixel self container))
      (om-set-view-size self (om-make-point 10 y-size))
      (let* ((mark-list (get-maquette-markers container))
             (finmarker (is-marketed (object self) 1 mark-list)))
        (when finmarker
          (del-mark (object self) finmarker 1))))
     
     (t (om-set-view-position self (get-offset/posy-in-pixel self container))
        (om-set-view-size self (om-make-point  (norme2pixel container 'x (* (extend (object self))
                                                                      (strech-fact (object self))))
                                               y-size))
        (when (and (allow-strech-p (soft-get-mus-ob (object self)) 1) (showpict (object self)))
          (update-miniview self (soft-get-mus-ob (object self))))
        ))))


(defmethod get-mini-param ((self tempobjframe) param)
   (cdr (assoc param (edition-params (object self)))))

(defmethod set-mini-param ((self tempobjframe) param val)
  (when (and (edition-params (object self))
             (assoc param (edition-params (object self))))
    (rplacd (assoc param (edition-params (object self))) val)))


;------------------------LOCK

(defmethod allowed-lock-modes ((self temporalbox)) '("x" "&"))

(defmethod add-lock-button ((self tempobjframe) &optional (mode "x"))
   "Mode lambda 'l' and refernce 'o' are forbidden for temporal boxes;"
   (when (find mode (allowed-lock-modes (object self)) :test 'string-equal)
     (setf (lock-button self)  (om-make-view 'lock-button
                                             :IconID (get-icon-lock mode)
                                             :size (om-make-point 10 10)
                                             :position (om-make-point 0 0)
                                             :owner self
                                             :action #'(lambda (item)
                                                         (let* ((modes (allowed-lock-modes (object self)))
                                                                (mpos (position (mode item) modes :test 'string-equal))
                                                                (newmode (nth (mod (1+ mpos) (length modes)) modes)))
                                                           (setf (mode item) newmode
                                                                 (iconID item) (get-icon-lock newmode))
                                                           (setf (allow-lock (object self)) newmode)
                                                           (om-draw-contents item)))
                                             ))
     (om-invalidate-view self)
     (setf (allow-lock (object self)) mode)))

(defmethod remove-lock-button ((self tempobjframe))
   (om-remove-subviews self (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil))

(defmethod get-mini-param ((self tempobjframe) param)
   (cdr (assoc param (edition-params (object self)))))



  
;--------------------------------
;EVAL
;--------------------------------

(defmethod eval+redraw ((self tempobjframe))
   (omNG-box-value (object self) 0)
   (update-after-mark (om-view-container self))
  )

(defmethod update-after-evaluation ((self tempobjframe))
   (let* ((themusobj (nth 0 (value (object self))))
          (container (om-view-container self)))
     (setf (mode self) 'normal)
     (if (allowed-in-maq-p themusobj)
       (if (Maquette-p (reference (object self)))
         (setf (slot-value (object self) 'extend) (get-obj-dur (reference (object self))))
         (setf (slot-value (object self) 'extend) (get-obj-dur themusobj)))
       (if (and (consp themusobj) 
                (eval `(and .,(mapcar 'allowed-in-maq-p themusobj)))) 
           (setf (slot-value (object self) 'extend) (get-obj-dur themusobj))
       (setf (slot-value (object self) 'extend) (pixel2norme container 'x (om-point-h (om-view-size self))))))
     (if (= (slot-value (object self) 'extend) 0)
       (om-set-view-size self (om-make-point 4 (om-point-v (om-view-size self))))
       (when (= 1 (strech-fact (object self)))
         (om-set-view-size self (om-make-point (norme2pixel container 'x (extend (object self)))
                                         (om-point-v (om-view-size self))))))
     (om-invalidate-view self t)))

(defmethod non-connected ((self tempobjframe) list)
   (let ((rep t))
     (loop for item in list 
           while list do
           (when (is-connected? (object self) (object item))
             (setf rep nil)
             (setf list nil))) 
     rep))

(defmethod non-connected ((self inframe) list) t)
(defmethod non-connected ((self outframe) list) t)
(defmethod update-after-evaluation ((self inframe)) nil)
(defmethod update-after-evaluation ((self outframe)) nil)



(defun normalize-points (container point-list)
   (loop for point in  point-list
         collect (pixel2point container  point)))

(defun denormalize-points (container point-list)
   (let ((sys-etat (get-system-etat container)))
     (loop for point in  point-list
           collect (point2pixel container point sys-etat))))

(defmethod class-of-connection ((self tempobjframe)) 'maq-connection)

(defmethod get-connection-lines ((self tempobjframe) i)
   (let* ((container (om-view-container self))
            (ctrl (nth i (inputframes self)))
            (connection (connected? (object ctrl)))
            (boxsource (first connection))
            possource sizesource
            (x-self (x self))
            (y-self (y self))
            (in-xs (x ctrl))
            x1 y1 xi yi)
     (if (car (frames boxsource))
         (progn
            (setf possource (om-view-position (car (frames boxsource))))
            (setf sizesource (om-view-size (car (frames boxsource))))
            (setq x1 (round (+ (om-point-h possource)  
                                              (- (* (+ (second connection) 1) (/ (om-point-h sizesource) (+ (numouts boxsource) 1))) 2))))
            (setq y1 (- (y+h (car (frames boxsource))) 2))
            (setq xi (+ x-self in-xs 4))
            (setq yi  y-self)
            (normalize-points container (list (om-make-point x1 y1)
                                                                         (om-make-point xi yi))))
       nil)
     ))

;-------D&D------

(defmethod get-relative-position ((self tempobjframe)) 
   (om-make-big-point (slot-value (object self) 'offset) (posy (object self))))

(defmethod om-drag-selection-p ((self tempobjframe) mouse-position)
   (declare (ignore mouse-position))
   (and (not (om-control-key-p)) 
        (= (mode (om-view-container self)) 7)))

(defmethod make-drag-region ((self tempobjframe) region x0 y0 view)
   (declare (ignore view))
   (om-set-rect-region region (- (x self) x0) (- (y self) y0) (- (x+w self) x0) (- (y+h self) y0))
   region)



;;;------------

(defmethod internalize-patch ((self tempobjframe))
  ;(inspect self))
   "A blue patch becomes a red patch."
   (when (or (equal (type-of (reference (object self))) 'OMPatch)
             (equal (type-of (reference (object self))) 'OMMaquette))
     (let* ((container (om-view-container self))
            (object (object self))
            newpatch newbox
            frame conec-to-me)
       
       (setf newpatch (if (maquette-p (reference object))
                        (maq2abs (reference object))
                        (patch2abs (reference object))))
       (setf newbox (omNG-make-tempobj newpatch
                                       (om-view-position self)
                                       (name self)))
       
       ;(setf (frame-position newbox) (borne-position (frame-position object)))
       (setf (offset newbox) (offset object))
       (setf (posy newbox) (posy object))
       (setf (show-name newbox) (show-name object))
       (setf (pictu newbox) (copy-picture (pictu object)))
       
       (setf (extend newbox) (extend object))
       (setf (strech-fact newbox) (strech-fact object))
       
       (setf (sizey newbox) (sizey object))
       ;(setf (frame-name newbox) (frame-name object))
       (setf (allow-lock newbox) (allow-lock object))
       (setf (value newbox) (eval (omNG-copy (value object))))
       (setf (inputs newbox) (eval (omNG-copy (inputs object))))
       (set-box-to-inputs (inputs newbox) newbox)
       (loop for input in (inputs object)
             for in in (inputs newbox) do
             (setf (connected? in) (connected? input)))
       (setf conec-to-me (get-conect-to-me object))
       (loop for item in conec-to-me do
             (change-conections  object item newbox))
       (omg-remove-element container self)
       (setf frame (make-frame-from-callobj newbox))
       (omg-add-element  container frame)
       (compile-patch newpatch)
       (update-graphic-connections frame (get-elements (object container))))))


(defmethod externalize ((self tempobjframe) newpatch) 
   "A red patch becomes a blue patch."
  (when (or 
         (equal (type-of (reference (object self))) 'OMPatchAbs)
         (equal (type-of (reference (object self))) 'OMMaqAbs))
    (let* ((container (om-view-container self))
           (object (object self))
           (newbox (omNG-make-tempobj newpatch
                                          (om-view-position self)
                                          (name self)))
           frame conec-to-me)
      ;(setf (frame-position newbox) (borne-position (frame-position object)))
      ;(setf (frame-size newbox) (frame-size object))
      
      (setf (offset newbox) (offset object))
      
      (setf (posy newbox) (posy object))
      (setf (sizey newbox) (sizey object))
      
      (setf (strech-fact newbox) (strech-fact object))
      (setf (extend newbox) (extend object))
      
      (setf (show-name newbox) (show-name object))
       (setf (pictu newbox) (copy-picture (pictu object)))
       
      
      (setf (frame-name newbox) (frame-name object))
      (setf (allow-lock newbox) (allow-lock object))
      (setf (value newbox) (eval (omNG-copy (value object))))
      (setf (inputs newbox) (eval (omNG-copy (inputs object))))
      (loop for input in (inputs object)
            for in in (inputs newbox) do
            (setf (connected? in) (connected? input)))
      (setf conec-to-me (get-conect-to-me object))
      (loop for item in conec-to-me do
            (change-conections  object item newbox))
      (setf frame (make-frame-from-callobj newbox))
      (omg-remove-element  container self)
      (compile-patch newpatch)
      (omg-add-element  container frame)
      (update-graphic-connections frame (get-elements (object container)))
      (omng-save newpatch)
)))


(defmethod show-big-doc ((self tempobjframe))
  (om-beep))

;===========================
;FRAME
;===========================


(defmethod get-offset/posy-in-pixel (tempobj container)
  (let ((sys-etat (get-system-etat container)))
    (point2pixel container (get-relative-position tempobj) sys-etat)))


(defmethod get-offset/posy-from-pixel ((container t) pointpixel)
  (let* ((maqpos (pixel2point container pointpixel))
         (posy (om-point-v maqpos))
         (newoffset (om-point-h maqpos))
         (themaquette (object container))
         (metricpar (metricparam (params themaquette)))
         (xpar (xparam (params themaquette)))
         (ypar (yparam (params themaquette))))
         
    (if (and (rulermetric (editor container)) (equal 'metric (car (snap (params themaquette)))))
        ;(setf newoffset (u2ms (ms2u newoffset (first metricpar) (third metricpar)) (first metricpar) (third metricpar)))
        (let* ((mesure (get-ieme-measure (car (ms2listmetric newoffset (first metricpar) (second metricpar) (third metricpar) (fourth metricpar)))
                                        (second metricpar) (fourth metricpar)))
              (minstep (* 1000 (size-pulsation (first metricpar) mesure) (/ 4 (third metricpar)))))
          ;(setf newoffset (* minstep (round newoffset minstep)))
          (setf newoffset (round (* minstep newoffset) minstep))
          )
          
      (if (equal 'abs (car (snap (params themaquette))))
          (setf newoffset (* (car xpar) (round newoffset (car xpar))))))
    
    (when (cadr (snap (params themaquette)))
      (setf posy (calcule-y-pos posy (car ypar))))
    
    (om-make-big-point (max 0 newoffset) posy)))

(defun calcule-y-pos (y step)
   (* step (round y step)))

(defun ms2u (ms tempo min)
  (round (ms2noir ms tempo) (/ 4 min)))

(defun ms2noir (ms tempo)
  (* ms (/ (* (second tempo) (/ 4 (first tempo))) 60000)))

(defun u2ms (u tempo min)
  (round (* (u2noir u min) (/  60000 (* (second tempo) (/ 4 (first tempo)))))))

(defun u2noir (u min)
  (* u (/ 4 min)))

;=================================


(defmethod get-colorframe ((self tempobjframe))
  (colorframe (object self)))

(defmethod change-colorframe ((self tempobjframe) newcolor)
   "set the frame color of 'self' to 'newcolor'."
   (setf (colorframe (object self)) newcolor)
   (om-invalidate-view self t))


(defmethod tempframe-p ((self tempobjframe)) t)
(defmethod tempframe-p ((self t)) nil)


(defmethod connect-box ((self markerframe) (ctrl tempobjframe))
   (let* ((framesource ctrl)
          (boxsource (object framesource))
          (mark-list (get-maquette-markers (om-view-container framesource)))
          (finmarker (is-marketed boxsource 1 mark-list))
          (startmarker (is-marketed boxsource 0 mark-list))
          (allow-mark t)
          (pos (om-point-h (om-mouse-position ctrl)))
          (s-or-e (if (< pos (round (w ctrl) 2)) 0 1)))
     (cond
      ((= s-or-e 0)
       (if finmarker
         (if (not (allow-strech-p (soft-get-mus-ob boxsource) 0))
           (om-beep-msg "Sorry this object can not be streched")
           (if (< (slot-value (object self) 'offset) (slot-value finmarker 'offset))
             (progn
               (when startmarker
                 (del-mark boxsource startmarker 0))
               (add-mark boxsource (object self) 0))
             (progn
               (setf allow-mark nil)
               (om-beep-msg "End of temporal object must be after start"))))
         (progn
           (when startmarker
             (del-mark boxsource startmarker 0))
           (add-mark boxsource (object self) 0))))
      ((= s-or-e 1)
       (if startmarker
         (if (not (allow-strech-p (soft-get-mus-ob boxsource) 0))
           (om-beep-msg "Sorry this object can not be stretched")
           (if (> (slot-value (object self) 'offset) (slot-value startmarker 'offset))
             (progn
               (when finmarker
                 (del-mark boxsource finmarker 1))
               (add-mark boxsource (object self) 1))
             (progn
               (setf allow-mark nil)
               (om-beep-msg "End of temporal object must be after start"))))
         (if (> (- (slot-value (object self) 'offset) (extend boxsource)) 0)
           (progn
             (when finmarker
               (del-mark boxsource finmarker 1))
             (MoveTempObject framesource  (om-make-point (+ (x framesource) (- (x self) (x+w framesource))) (y framesource))) 
             (add-mark boxsource (object self) 1))
           (progn
             (setf allow-mark nil)
             (om-beep-msg  "A temporal object must  start after 0"))))))
     (when allow-mark
       ;(draw-mark-lines (om-view-container framesource) nil)
       (update-after-mark (om-view-container framesource))
       ;(draw-mark-lines (om-view-container framesource))
       )))


;;; CONNECT BOXES

(defmethod drag-out-line ((self tempobjframe) where)
   (let* ((panel (panel (om-view-window self)))
          (initpoint (om-convert-coordinates where self panel))
          (rx (om-point-h initpoint))
          (ry (om-point-v initpoint)))
     (om-init-motion-functions self 'make-connection-marker-motion 'release-connection-marker-motion)
     (om-new-movable-object panel rx ry 4 4 'om-movable-line)))




