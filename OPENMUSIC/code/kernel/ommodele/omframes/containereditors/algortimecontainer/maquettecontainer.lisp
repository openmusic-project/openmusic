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
;Maquette Editor definition.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)


 
;================================================================
;THE MAQUETTE WINDOW
;================================================================

(defclass maquette-titlebar (editor-titlebar) 
  ((play-buttons :accessor play-buttons :initarg :play-buttons :initform nil)
   (mode-buttons :accessor mode-buttons :initarg :mode-buttons :initform nil)))

;-------------------------------------------------
;EDITOR
;-------------------------------------------------

(defclass MaquetteEditor (object-editor patchEditor play-editor-mixin) 
   ((rulermetric :initform nil :accessor rulermetric)
    (patchview :accessor patchview :initform nil :initarg :patchview)))

(defmethod get-control-h ((self MaquetteEditor)) (call-next-method))

(defmethod get-titlebar-class ((self MaquetteEditor)) 'maquette-titlebar)

(defmethod init-titlebar ((self MaquetteEditor))
  (setf (play-buttons (title-bar self))
        (list (om-make-view 'om-icon-button :position (om-make-point 50 2) :size (om-make-point 22 22)
                            :icon1 "play" :icon2 "play-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-play self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 71 2) :size (om-make-point 22 22)
                            :icon1 "pause" :icon2 "pause-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-pause self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 92 2) :size (om-make-point 22 22)
                            :icon1 "stop" :icon2 "stop-pushed"
                            :action #'(lambda (item) (editor-stop self)))
               
              (om-make-view 'om-icon-button :position (om-make-point -10 -10) :size (om-make-point 1 1)
                            :icon1 "rec" :icon2 "rec-pushed") ;; dummy rec
              
              (om-make-view 'om-icon-button :position (om-make-point 123 2) :size (om-make-point 22 22)
                            :icon1 "loopbutton" :icon2 "loopbutton-pushed"
                            :lock-push t
                            :selected-p (loop-play self)
                            :action #'(lambda (item) 
                                        (setf (loop-play self)
                                              (not (loop-play self)))
                                        (setf (selected-p item) (loop-play self))
                                        ))
              
              ))
   
  (setf (mode-buttons (title-bar self))
        (loop for mode in '(:normal :interval :move :zoom)
              for icon in '("mousecursor" "beamcursor" "handcursor" "zoomcursor")
              for xx = 180 then (+ xx 21) 
              collect 
              (let ((m mode))
                (om-make-view 'om-icon-button :position (om-make-point xx 2) :size (om-make-point 22 22)
                              :id mode
                              :icon1 icon :icon2 (string+ icon "-pushed")
                              :lock-push t
                              :selected-p (and (panel self) ;;; before initialization...
                                               (equal m (cursor-mode (panel self))))
                              :action #'(lambda (item) 
                                          (setf (cursor-mode (panel self)) m)
                                         
                                          (loop for b in (mode-buttons (title-bar self)) do 
                                                (setf (selected-p b) nil))
                                          (setf (selected-p item) t)
                                          (om-invalidate-view self)))
                )))

  (apply 'om-add-subviews (cons (title-bar self)
                                (append (play-buttons (title-bar self))
                                        (mode-buttons (title-bar self))
                                        (list 
                                         (om-make-view 'om-icon-button :position (om-make-point 270 2) :size (om-make-point 22 22)
                                                       :id :resize
                                                       :icon1 "resize" :icon2 "resize-pushed"
                                                       :lock-push nil
                                                       :action #'(lambda (item) 
                                                                   (init-coor-system (panel self))
                                                                   (om-invalidate-view (rulerx (panel self)) t)
                                                                   (om-invalidate-view (rulery (panel self)) t)
                                                                   (when (rulermetric self) 
                                                                     (om-invalidate-view (rulermetric self) t))))
                                         
                                         (om-make-view 'om-icon-button :position (om-make-point 350 2) :size (om-make-point 22 22)
                                                       :id :resize
                                                       :icon1 "eval" :icon2 "eval-pushed"
                                                       :lock-push nil
                                                       :action #'(lambda (item) 
                                                                   (om-eval-enqueue 
                                                                    `(eval-maquette ,(panel self)) (panel self))
                                                                    ))
                                         )
                                        )))
  )


(defvar *maquette-play* nil)

(defmethod editor-play ((self MaquetteEditor))
  (setf *maquette-play* T)
  (cons-copy-maquette-object (object self) (boxestoplay self))
  (call-next-method)
  (update-play-buttons (title-bar self)))

(defmethod editor-pause ((self MaquetteEditor))
  (call-next-method)
  (update-play-buttons (title-bar self)))

(defmethod editor-stop ((self MaquetteEditor))
  (setf *maquette-play* NIL)
  (call-next-method)
  (update-play-buttons (title-bar self)))


(defmethod draw-maq-tempo ((self MaquetteEditor) tempo)
  (om-with-fg-color self *om-dark-gray-color*
          (om-with-font (om-make-music-font *heads-font* 12)
                        (om-draw-string 1 14 (fig-1/4)))
          (om-with-font (om-make-font "verdana" 8)
                        ;(om-make-font "verdana" #+cocoa 8 #-cocoa 6)
                        (om-draw-string 6 12 (string+ "=" (number-to-string tempo))))
          ))

(defmethod om-draw-contents ((self MaquetteEditor))
  (when (rulermetric self)
    (let* ((params (params (object self)))
           (tempo (cadr (car (metricparam params)))))   
      (om-with-focused-view self
        (draw-maq-tempo self tempo)
        )))
    (call-next-method))

;;;=========================================
(omg-defclass drop-patch-view (om-transparent-view om-view-drop) 
  ((selected :accessor selected :initform nil :initarg :selected)))

(defmethod om-view-drag-hilite-p ((self drop-patch-view)) t)

(defmethod om-drag-enter-view ((self drop-patch-view))
  (om-with-focused-view self
    (om-with-fg-color self *azulito*
      (om-fill-rect 3 3 (- (w self) 6) (- (h self) 6))) 
    ))

(defmethod om-drag-leave-view ((self drop-patch-view))
  (om-invalidate-view self t))

(defmethod om-drag-receive ((view drop-patch-view) 
                            (dragged-view t) position &optional (effect nil)) 
  (declare (ignore data-size item-reference))
  (let ((fun (if (icon-finder-p (dragged-view *OM-drag&drop-handler*))
               (object (dragged-view *OM-drag&drop-handler*))
               (reference (object (dragged-view *OM-drag&drop-handler*))))))
    (cond 
     ((and (patch-p fun) (not (maquette-p fun)))
      (set-eval-func (object (om-view-container view)) fun)
      t)
     ((and (symbolp fun) (fboundp fun))
      (set-eval-func (object (om-view-container view)) (if (omgenfun-p (fdefinition fun)) 
                                                         (fdefinition fun)
                                                         (omNG-make-new-lispfun fun)))
      t)
     (t nil))
    ))

(defmethod om-draw-contents ((self drop-patch-view))
  (let ((evalf (eval-func (object (om-view-container self)))))
    (om-with-focused-view self
      (if evalf
        (om-draw-picture self (cadr (get&corrige-icon (icon evalf))) :pos (om-make-point 2 2) :size (om-make-point 20 20) :selected (selected self))
        (om-draw-picture self (cadr (get&corrige-icon 1000)) :pos (om-make-point 2 2) :size (om-make-point 20 20) :selected (selected self)) 
        ))))

(defmethod om-view-click-handler ((self drop-patch-view) pos)
  (mapc #'(lambda (control) 
            (omG-unselect control)) (get-actives (panel (om-view-container self))))
  (setf (selected self) t)
  (om-invalidate-view self t))

(defmethod om-view-doubleclick-handler ((self drop-patch-view) pos)
  (unless (eval-func (object (om-view-container self)))
    (let ((newpatch (make-instance 'OMPatchAbs 
                                   :name (string+ (name (object (om-view-container self))) " - internal eval patch") :icon 210)))  
      (omNG-add-element newpatch (make-maq-temp-input "self" (om-make-point 90 50)))
      (omNG-add-element newpatch (make-new-temp-output "tempout" (om-make-point 220 300)))
      (set-eval-func (object (om-view-container self)) newpatch)))
  (openobjecteditor (reference (eval-func (object (om-view-container self))))))

(defmethod om-view-click-handler ((self maquetteeditor) pos)
  (unselect-eval-func self)
  (call-next-method))

(defun unselect-eval-func (maquetteeditor)
  (when (patchview maquetteeditor)
    (setf (selected (patchview maquetteeditor)) nil)
    (om-invalidate-view (patchview maquetteeditor) t)))

(defmethod close-editor-before ((self maquetteeditor))  
  (call-next-method)
  (when (and (eval-func (object self)) (EditorFrame (reference (eval-func (object self)))))
    (om-close-window (window (EditorFrame (reference (eval-func (object self))))))))

;;;=========================================


(defmethod get-editor-panel-class ((self MaquetteEditor))  'maquettePanel)

(defmethod metaobj-scrollbars-params ((self maquetteEditor))  '(nil nil))

(defmethod set-clipboard ((self MaquetteEditor) val)
   (let* ((container (panel self)))
     (setf (scroll-scrap-ma container) val)))

(defmethod get-clipboard ((self MaquetteEditor))
   (let* ((container (panel self)))
     (scroll-scrap-ma container)))  

(defmethod maq-x-ruler-class ((self MaquetteEditor)) 'maq-ruler)
(defmethod maq-y-ruler-class ((self MaquetteEditor)) 'maq-y-ruler)
(defmethod maq-metric-ruler-class ((self MaquetteEditor)) 'metric-ruler)
(defmethod maq-drop-patch-p ((self MaquetteEditor)) t)
(defmethod maq-scrollers-p ((self MaquetteEditor)) t)


(defparameter x-ruler-h 20)  
(defparameter x-scroll-h 8)  
(defparameter y-ruler-w 20)  
(defparameter y-scroll-w 8)  

(defmethod initialize-instance :after ((self MaquetteEditor) &rest l)
  (declare (ignore l))
  (let* ((panel (panel self))
         (rulery (om-make-view (maq-y-ruler-class self)
                               :owner self
                               :axe 'y
                               :assoc-view panel
                               :position (om-make-point 0 (get-control-h self)) 
                               :bg-color *om-light-gray-color*
                               :size (om-make-point y-ruler-w (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h)))
                               :y-step (car (yparam (params (object panel))))))
                         
         (rulerx (om-make-view (maq-x-ruler-class self) 
                               :owner self
                               :axe 'x
                               :zoom 1000
                               :minzoom 0.001
                               :assoc-view panel
                               :bg-color *om-light-gray-color*
                               :position (om-make-point (+ y-ruler-w y-scroll-w) (- (h self) x-scroll-h))
                               :size (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) x-scroll-h)
                               :x-step (car (xparam (params (object panel))))
                               ))
         scrollx scrolly     
         )
    
    (setf (rulerx panel) rulerx)
    (setf (rulery panel) rulery)

    (when (maq-scrollers-p self)
      (setf (scrollerx panel) (om-make-view 'maq-scroller
                                            :owner self
                                            :scrolldirection :h
                                            :referenceview panel
                                            :position (om-make-point (+ y-ruler-w y-scroll-w) (- (h self) (+ x-ruler-h x-scroll-h)))
                                            :size (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) x-scroll-h)))
      (setf (scrollery panel) (om-make-view 'maq-scroller
                                            :owner self
                                            :scrolldirection :v
                                            :referenceview panel
                                            :position (om-make-point y-ruler-w (get-control-h self))
                                            :size (om-make-point y-scroll-w (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h))))))

    (om-set-view-position panel (om-make-point 25 (get-control-h self)))
    (om-set-view-size panel (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h))))
    (when (equal (maq-show-ruler (object self)) :on)
      (add-ruler-metric self))
    (when (maq-drop-patch-p self) 
      (om-add-subviews self (setf (patchview self) (om-make-view 'drop-patch-view
                                                                 :position (om-make-point 0 0)
                                                                 :size (om-make-point 24 24)
                                                                 ))))
    (om-set-bg-color self *om-light-gray-color*)
    (add-more-information panel)
    
    (om-invalidate-view (panel self))))



(defmethod add-ruler-metric ((self MaquetteEditor))
  (if (find  :om-musicproject *features*)
  (let* ((panel (panel self))
         (editor (editor self))
         (themaq (object panel))
         (params (metricparam (params themaq))))
    (setf (rulermetric editor) (om-make-view (maq-metric-ruler-class self) 
                                             :axe 'x
                                  :zoom 1000
                                  :minzoom 0.001
                                  :tempo (first params)
                                  :meslist (second params)
                                  :bg-color *om-light-gray-color*
                                  :maxsub (third params)
                                  :loop-mes-p (fourth params)
                                  :assoc-view panel
                                  :position (om-make-point (+ y-ruler-w y-scroll-w) (get-control-h self)) 
                                  :size (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) x-scroll-h)))
    (om-add-subviews editor (rulermetric editor))
    (maq-show-ruler themaq :on)
    (update-all-offsets editor)
    (update-subviews self))
  (om-beep-msg "The music project is not loaded")))


(defmethod delete-ruler-metric ((self MaquetteEditor))
   (om-remove-subviews self (rulermetric self))
   (setf (rulermetric self) nil)
   (maq-show-ruler (object self) :off)
   (update-all-offsets self)
   (update-subviews self)
   )

(defmethod update-all-offsets ((self MaquetteEditor))
   (let* ((container (panel self))
          (themaquette (object container))
          (metric-pa (metricparam (params themaquette))))
     (loop for frame in (get-subframes container) do
           (let ((tempobj (object frame)))
             (when (rulermetric self)
               (setf (slot-value tempobj 'offset)
                     (u2ms (ms2u (offset tempobj) (first metric-pa) (third metric-pa))
                           (first metric-pa) (third metric-pa))))))))


(defmethod give-editor-list-range ((self MaquetteEditor))
   (let ((subframes (get-subframes (panel self))))
     (setf subframes
           (loop for item in subframes
                 when (tempframe-p item) collect item))
     (cond
      ((null subframes) (list 0 10000 0 127))
      (t (loop for item in subframes
               maximize (+ (* (strech-fact (object item)) (extend (object item))) (offset (object item))) into x
               maximize (posy (object item)) into y
               minimize (- (posy (object item)) (sizey (object item))) into y0
               finally (return (list 0 (round (* x 1.1)) (- y0 (round (* 0.1 (- y y0)))) (+ y (round (* 0.1 (- y y0)))))))))))



(defmethod mode ((self MaquetteEditor))
   (cursor-mode (panel self)))

(defmethod update-subviews ((self MaquetteEditor))
  (call-next-method)
  (let ((pane (panel self)))
    (om-with-delayed-redraw pane
      (om-set-view-size (rulerx pane) (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) x-ruler-h))
      (om-set-view-position (rulerx pane) (om-make-point (+ y-ruler-w y-scroll-w) (- (h self) x-ruler-h)))
      (when (scrollerx pane) 
        (om-set-view-size (scrollerx pane) (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) x-scroll-h))
        (om-set-view-position (scrollerx pane) (om-make-point (+ y-ruler-w y-scroll-w) (- (h self) (+ x-ruler-h x-scroll-h)))))
      (if (not (rulermetric self))
          (progn
            (om-set-view-position (rulery pane) (om-make-point 0 (get-control-h self)))
            (om-set-view-size (rulery pane) (om-make-point y-ruler-w (- (h self) (+ x-ruler-h x-scroll-h (get-control-h self)))))
            (when (scrollery pane)
              (om-set-view-size (scrollery pane) (om-make-point y-scroll-w (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h))))
              (om-set-view-position (scrollery pane) (om-make-point y-ruler-w (get-control-h self))))
            
            (om-set-view-position pane (om-make-point (+ y-ruler-w y-scroll-w) (get-control-h self)))
            (om-set-view-size pane (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h))))
        
            )
       (progn
         (om-set-view-position (rulery pane) (om-make-point 0 (+ (get-control-h self) x-ruler-h)))
         (om-set-view-size (rulery pane) (om-make-point y-ruler-w  (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h x-ruler-h))))
         
         (when (scrollery pane)
           (om-set-view-size (scrollery pane) (om-make-point y-scroll-w (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h x-ruler-h))))
           (om-set-view-position (scrollery pane) (om-make-point y-ruler-w (+ (get-control-h self) x-ruler-h))))
         
         (om-set-view-position pane (om-make-point (+ y-ruler-w y-scroll-w) (+ (get-control-h self) x-ruler-h)))
         (om-set-view-size pane (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) (- (h self) (+ (get-control-h self) x-ruler-h x-scroll-h x-ruler-h))))
         
         (om-set-view-size (rulermetric self) (om-make-point (- (w self) (+ y-ruler-w y-scroll-w)) x-ruler-h))
         (om-set-view-position (rulermetric self) (om-make-point (+ y-ruler-w y-scroll-w) (get-control-h self)))
         ))
    (when (patchview self) (om-set-view-position (patchview self) (om-make-point 0 (- (h self) 24))))
    (update-size+pos-frames pane)
    (redraw-after pane (get-subframes pane))

    ))
  )

;--------------------------
;PANEL
;--------------------------

(defclass MaquettePanel (patchPanel view-with-ruler-xy cursor-play-view-mixin) 
   (;(mode :initform 7 :accessor mode)
    (selected-mark-lines :initform nil :accessor selected-mark-lines)
    (scroll-scrap-ma :initform nil :allocation :class :accessor scroll-scrap-ma))
   (:documentation "This is the class for editors of Maquette meta objects.
 Elements of these editors are tempobjframe instances.#enddoc#
#seealso# (OMMaquette tempobjframe temporalbox) #seealso#")
   )

(defmethod delete-general :after ((self maquettepanel))
  (when (and (patchview (editor self)) (selected (patchview (editor self))))
    (set-eval-func (object (editor self)) nil)
    (om-invalidate-view (patchview (editor self)) t)))

(defmethod om-view-click-handler ((self maquettepanel) pos)
  (unselect-eval-func (editor self))
  (call-next-method))

(defmethod add-window-buttons ((self MaquettePanel))
  "There are not buttons in a maquette it has a palette." nil)


(defmethod maq-panel-set-color ((self  MaquettePanel) color)
   (om-set-bg-color self color)
   ;(om-set-bg-color (rulerx self) color)
   ;(om-set-bg-color (rulery self) color)
   ;(om-set-bg-color (om-view-container self) color)
   ;(when (rulermetric (editor self))
   ;  (om-set-bg-color (rulermetric (editor self)) color))
   )

(defmethod add-more-information ((self  MaquettePanel))
   (let ((ranges (range (params (object self)))))
     (setf (rangex self) (list (first ranges) (second ranges)))
     (setf (rangey self) (list (third ranges) (fourth ranges)))
     (set-ranges self (rangex self) (rangey self))
     (update-scrollers self)
     ;(om-set-font self *om-default-font4b*)
     (let ((color (maq-color (params (object self)))))
       (when color
           (maq-panel-set-color self color)
         ))))

(defmethod save-ranges ((self  MaquettePanel))
   (setf (range (params (object self))) (list (car (rangex self)) (second (rangex self))
                                              (car (rangey self)) (second (rangey self)))))

(defmethod set-ranges ((self  MaquettePanel) rangex rangey)
   (declare (ignore rangex rangey))
   (call-next-method)
   (save-ranges self))

(defmethod show-con? ((self MaquettePanel))
   (show-conect (params (object self))))

(defmethod delta-scroll ((self MaquettePanel))
   "Return a point with the scroll positions"
   (om-make-point 0 0))


(defmethod scroll-play-window ((self maquettepanel)) 
  (call-next-method)
  (update-scrollers self))

;-------------------------------
;MCL methods
;-------------------------------

(defmethod om-add-subviews :before ((self MaquettePanel) &rest subviews)
  (mapc #'(lambda (elem) (init-size&position elem self)) subviews))

(defmethod om-view-cursor ((self MaquettePanel))
   (if (om-control-key-p) 
     *om-contex-cursor*)
   (case (cursor-mode self)
     (:zoom *om-loupe-cursor*)
     (:move *om-hand-cursor*)
     (:interval *om-i-beam-cursor*)
     (otherwise *om-arrow-cursor*)))

(defmethod om-view-doubleclick-handler ((Self maquettepanel) Where) nil)


(defmethod control-actives ((view MaquettePanel) where)
   (when (text-view (editor view))
     (exit-from-dialog (text-view (editor view)) (om-dialog-item-text (text-view (editor view)))))
   (cond
    ((equal (cursor-mode view) :zoom) (zoom-system view where))
    ((equal (cursor-mode view) :move) (scroll-system view where))
    ((equal (cursor-mode view) :interval) (new-interval-cursor view where))
    (t ; (equal (cursor-mode view) :normal)
     (cond
      ((and (show-con? view) (click-in-connection view where))
       (mapc #'(lambda (control) 
                 (omG-unselect control)) (get-actives  view))
       (when (selected-mark-lines view)
         (draw-mark-lines view nil)
         (setf (selected-mark-lines view) nil)
         (draw-mark-lines view)
         ))
      ((and (show-con? view) (click-in-marker-line view where))
       (mapc #'(lambda (control) 
                 (omG-unselect control)) (get-actives  view))
       (mapc #'(lambda (control) 
                 (deactivate-connect control)) (get-connections view)))
      (t (let* ((float (om-subtract-points (om-mouse-position view) where)))
           (mapc #'(lambda (control) 
                     (deactivate-connect control)) (get-connections view))
           (when (selected-mark-lines view)
             (draw-mark-lines view nil)
             (setf (selected-mark-lines view) nil)
             (draw-mark-lines view)
             )
           (cond 
            ((om-command-key-p)
              (om-init-motion-functions view 'make-selection-rectangle 'release-maquette-new-box)
              (om-new-movable-object view (om-point-h where) (om-point-v where) 4 4 'om-selection-rectangle))
            (t (call-next-method)))))))))

(defmethod make-selection-rectangle ((self MaquettePanel) pos)
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) (second rect) (max 4 (third rect)) (max 4 (fourth rect))))))

(defmethod release-maquette-new-box ((self MaquettePanel) pos)   
  (let* ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-erase-movable-object self)
      (let (user-rect )
        (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
        (if (om-shift-key-p)
                   (make-maq-tempobj self (om-make-point  (first rect) (second rect)) (om-rect-bottomright user-rect))
                 (make-tempobj self (om-make-point  (first rect) (second rect)) (om-rect-bottomright user-rect)))))))

(defmethod do-select-items-in-rect ((self MaquettePanel) rect) 
   (let (user-rect scratch-rect-i scratch-rect-n i-rect n-rect)
      (when rect
        (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
        (dolist (item (get-subframes self))
          (setf i-rect (om-pts-to-rect (om-view-position item) (om-add-points (om-view-position item) (om-view-size item))))
          (setf scratch-rect-i (om-sect-rect user-rect i-rect))
          (unless (om-rect-empty scratch-rect-i)
            (omG-select item))))))


;-------------------------------
;EVENTS
;-------------------------------

(defmethod change-colorframe ((self MaquettePanel) newcolor)
  (maq-panel-set-color self newcolor)
  (setf (maq-color (params (object self))) newcolor))

(defun maquette-color-boxes (maquettepanel)
  "If there are selected boxes set their frame color to 'newcolor', else set the back color of 'self' to newcolor."
  (let ((new-color (om-choose-color-dialog :color (om-get-bg-color maquettepanel)))
        (frames (get-actives maquettepanel)))
    (when new-color
      (loop for item in frames do
            (change-colorframe item new-color)))))


(defun maquette-color (maquettepanel)
  "If there are selected boxes set their frame color to 'newcolor', else set the back color of 'self' to newcolor."
  (let ((new-color (om-choose-color-dialog :color (om-get-bg-color maquettepanel))))
    (when new-color
      (change-colorframe maquettepanel new-color))))


(defmethod move-and-not-action ((self MaquettePanel) (cible MaquettePanel)) t)
(defmethod move-and-not-action ((self MaquettePanel) (cible t)) nil)
(defmethod move-and-not-action ((self t) (cible MaquettePanel)) nil)

(defmethod get-colorframe ((view MaquettePanel))
   (maq-color (params (object view))))

(defmethod make-tempobj ((self MaquettePanel) x y)
   "Add a new empty temporalbox to 'self'. This method is called when you make ALT+CLICK+DRAG in 'self'."
   (let* ((thename (mk-unique-name self "tempobj"))
          (new-patch (make-instance 'OMPatchAbs 
                       :name thename :icon 210))
          (pixsizex (max 20 (- (om-point-h y) (om-point-h x))))
          (pixsizey (max 10 (- (om-point-v y) (om-point-v x))))
          (maqpos (get-offset/posy-from-pixel self  (om-make-point (om-point-h x) (om-point-v x))))
          (y-size (pixel2norme self 'y  pixsizey))
          (tempobj (omNG-make-tempobj new-patch maqpos thename))
          new-frame)
         ;; *
     (add-temp-boxes new-patch)
     (setf (slot-value tempobj 'extend) (pixel2norme  self 'x pixsizex))
     (setf (slot-value tempobj 'sizey)  y-size)
     (setf new-frame (make-frame-from-callobj tempobj))
     (omG-add-element self new-frame)))

;; ***
(defun get-absmaqclass () 'OMMaqAbs)

;; ***
(defmethod get-maq-obj-name ((class t)) "maquette")
(defmethod get-maq-obj-name ((class (eql 'ommaqabs))) "internal-maq")

;; ***
(defmethod make-maq-tempobj ((self MaquettePanel) x y)
   "Add a new empty temporal maq to 'self'. This method is called when you make ALT+CLICK+DRAG in 'self'."
   (let* ((class (get-absmaqclass))
          (thename (mk-unique-name self (get-maq-obj-name class)))
          (new-patch (make-instance class 
                       :name thename :icon 265))
          (pixsizex (max 20 (- (om-point-h y) (om-point-h x))))
          (pixsizey (max 10 (- (om-point-v y) (om-point-v x))))
          (maqpos (get-offset/posy-from-pixel self  (om-make-point (om-point-h x) (om-point-v x))))
          (y-size (pixel2norme self 'y  pixsizey))
          (tempobj (omNG-make-tempobj new-patch maqpos thename))
          new-frame)
     (setf (slot-value tempobj 'extend) (pixel2norme  self 'x pixsizex))
     (setf (slot-value tempobj 'sizey)  y-size)
     (setf new-frame (make-frame-from-callobj tempobj))
     (omG-add-element self new-frame)))
       


(defmethod maquette-extra-eval ((self ommaquette)) 
  (setf (value self) 
        (let ((boxestoplay (loop for b in (boxes self) when (and (boxtempobj-p b) (not (mute b))) collect b)))
          (cons-copy-maquette-object self boxestoplay))))

(defmethod eval-maquette ((self MaquettePanel))
   "Eval all boxes in 'self'."
   (loop for item in (get-objects-to-value (get-subframes self)) do
         (eval+redraw item))
   (handler-bind ((error #'(lambda (c) 
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the maquette " (string (name (object (editor self)))) " " 
                                                        (om-report-condition c))
                                               :size (om-make-point 300 200))
                               (print "Evaluation ABORTED")
                               (om-abort)))))
     (maquette-extra-eval (object (editor self))))
   (value (object (editor self)))
   (clear-ev-once self))

(defun get-objects-to-value (list)
   (let (rep)
     (mapc #'(lambda (frame)
               (when (and (not (markerframe-p frame)) (non-connected frame list))
                 (push frame rep))) list) 
     rep))


(defmethod omg-remove-element ((self MaquettePanel) frame)
   "Frame can be a tempobjframe or a marker."
   (let ((x0 (x frame))
         (xf (x+w frame)))
     (call-next-method)
     (cond 
      ((markerframe-p frame)
       (om-invalidate-rectangle self (- x0 2) 0 4 (h self)))
      (t
       (let* ((mark-list (get-maquette-markers self))
              (finmarker (is-marketed (object frame) 1 mark-list))
              (startmarker (is-marketed (object frame) 0 mark-list)))
         (when startmarker
           (del-mark (object frame) startmarker 0))
         (when finmarker
           (del-mark (object frame) finmarker 1))
         (om-invalidate-rectangle self (- x0 2) 0 4 (h self)))))))
 
;-------------------------------------------
;DRAW
;-------------------------------------------
(defvar *scrolling-ruler* nil)

(defmethod priority-connecctions ((self MaquettePanel)) t)
 

(defmethod om-draw-contents :before ((self MaquettePanel))
   (when (pictu (object self))
     (draw-om-picture (pictu (object self)) self)))
     

(defmethod om-draw-contents ((self MaquettePanel))
   (unless  *scrolling-ruler*
     (when (show-con? self) (draw-mark-lines self))
     (when (grille-p self)
       (draw-axis self)
       (if (rulermetric (editor self))
         (draw-grille-metric self (rangex self))
         (draw-grille self)))
     (when (cursor-p self)
       (draw-interval-cursor self))
     ;;; !!  boucle quand on ajoute le ruler metric...
     ;;;(when (rulermetric (editor self))
     ;;;  (om-invalidate-view (rulermetric (editor self)) t))
     (call-next-method)))


(defmethod change-view-ranges ((self t)) t)

(defmethod change-view-ranges ((self MaquettePanel))
   (update-size+pos-frames self))

(defmethod update-view-of-ruler  ((self MaquettePanel))
   (setf *scrolling-ruler* t)
   (update-scrollers self)
   (update-size+pos-frames self)
   (setf *scrolling-ruler* nil)
   (save-ranges self)
   ;; TEST
   (om-invalidate-view self)
   )
  
(defmethod zoom-system-release ((self maquettepanel) pos)
  (call-next-method)
  (when (< (car (rangex self)) 0) 
    (set-ranges self (list 0  (cadr (rangex self))) (rangey self))
    (update-view-of-ruler self)
    (om-invalidate-view (rulerx self) t))
  (when (rulermetric (editor self))
    (om-invalidate-view (rulermetric (editor self)) t)))

(defmethod update-size+pos-frames ((self MaquettePanel)) 
  (let* ((frames (get-subframes self)))
    (om-with-delayed-redraw self
     (loop for frame in frames do
           (init-size&position frame self)
           ))))

(defmethod get-string-nom  ((self MaquettePanel)  num axe)
   (if (equal axe 'y)
     (call-next-method)
     (cond
      ((integerp (/ num 1000)) (format () "~D" (round num 1000)))
      ((integerp (/ num 100)) (format () "~,1F" (/ num 1000)))
      (t (format () "~,2F" (/ num 1000))))))

(defvar *maq-last-click* nil)
(defvar *maq-first-click* nil)
(defvar *maq-offset-click* nil)
(defvar *maq-old-grille* nil)

(defmethod scroll-system ((Self MaquettePanel) where)
  (setf *maq-last-click* where)
  (setf *maq-first-click* where)
  (setf *maq-offset-click* (om-make-point 0 0))
  (setf *maq-old-grille*  (grille-p self))
  ;(setf (grille-p self) nil)
  (om-init-motion-functions self 'make-scroll-system 'release-scroll-system))

(defmethod release-scroll-system ((Self MaquettePanel) Where) 
  (setf (grille-p self) *maq-old-grille*)
  (save-ranges self)
  (change-view-ranges self)
  (update-scrollers self)
  (om-invalidate-view self t))

(defmethod make-scroll-system ((Self MaquettePanel) Where)
  (let* ((old-Mouse *maq-last-click*)
         (Initmouse old-mouse)
         (Initx (om-point-h *maq-offset-click*))
         (Inity (om-point-v *maq-offset-click*))
         (Initrangex (rangex self))
         (Initrangey (rangey self))
         (new-mouse where)
         (frames (get-subframes self))
         Deltax Deltay)
    (setf deltax (pixel2norme self 'x (- (om-point-h initmouse) (om-point-h new-mouse))))
    (setf deltay (pixel2norme self 'y (- (om-point-v new-mouse) (om-point-v initmouse))))
    (if (minusp (+ (first initrangex) deltax))
        (setf (rangex self) (list 0 (- (second initrangex) (first initrangex))))
      (setf (rangex self) (list (+ (first initrangex) deltax)
                                (+ (second initrangex) deltax))))
    (setf (rangey self) (list (+ (first initrangey) deltay)
                                       (+ (second initrangey) deltay)))
    (when (rulermetric (editor self))
      (om-redraw-view (rulermetric (editor self))))
    (draw-items-frames self frames)
    (om-redraw-view (rulerx self))
    (om-redraw-view (rulery self))
    (om-redraw-view self)
    (show-position (om-view-container self))
    (setq *maq-last-click* where)))



(defun draw-items-frames (cont items)
  (om-with-focused-view cont
     (om-with-dashline
      (loop for item in items do
            (let ((maqpos (get-offset/posy-in-pixel item cont)))
              (om-draw-rect (om-point-h maqpos) (om-point-v maqpos)  (w item) (h item)))))))


  
;======================
; MAQ SCROLLER
;======================


(in-package :om)


(defclass maq-scroller (om-view) 
  ((scrolldirection :initarg :scrolldirection :accessor scrolldirection :initform :h)
   (scrollitem :initarg :scrollitem :accessor scrollitem :initform nil)
   (referenceview :initarg :referenceview :accessor referenceview :initform nil)
   (scrollpos :initarg :scrollpos :accessor scrollpos :initform 0)
   (scrollsize :initarg :scrollsize :accessor scrollsize :initform 1)
   ))

(defclass scroller-item (om-item-view) ())

(defmethod initialize-instance ((self maq-scroller) &rest args)
  (call-next-method)
  (om-add-subviews self 
                   (setf (scrollitem self) 
                         (om-make-view 'scroller-item 
                                       :bg-color (om-make-color 0.7 0.7 0.7))))
  (set-scroll-pos self 0)
  (set-scroll-size self 1)
  (om-set-bg-color self *om-light-gray-color*))
  
;(defmethod om-draw-contents ((self maq-scroller))
  ;(om-with-focused-view self
  ;  (om-with-fg-color self *om-dark-gray-color*
  ;      (if (equal (scrolldirection self) :h)
  ;          (om-draw-line 0 0 (w self) 0)
  ;        (om-draw-line (- (w self) 1) 0 (- (w self) 1) (h self))
  ;        )))
;  )
                       
(defmethod set-scroll-pos ((self maq-scroller) posratio)
  (setf (scrollpos self) posratio)
  (update-scroll-item self))

(defmethod set-scroll-size ((self maq-scroller) sizeratio)
  (setf (scrollsize self) sizeratio)
  (update-scroll-item self))

(defmethod update-scroll-item ((self maq-scroller))
 (if (equal (scrolldirection self) :h)
     (progn 
       (om-set-view-position (scrollitem self) (om-make-point (round (* (scrollpos self) (om-width self))) 1))
       (om-set-view-size (scrollitem self) (om-make-point (round (* (scrollsize self) (om-width self))) (- (om-height self) 1)))
       )
   (progn
     (om-set-view-position (scrollitem self) (om-make-point 0 (* (scrollpos self) (om-height self))))
     (om-set-view-size (scrollitem self) (om-make-point (- (om-width self) 1) (round (* (scrollsize self) (om-height self)))))
     )))

(defmethod om-set-view-size ((self maq-scroller) size)
  (call-next-method)
  (update-scroll-item self))

(defvar *initrange* '(0 0))
(defvar *initscrollpos* '(0 0))

(defmethod om-view-click-handler ((self scroller-item) pos)
  (let ((scroll (om-view-container self)))
    (setf *ruler-last-click* (om-convert-coordinates pos self scroll))
    (setf *initrange*  (if (equal :h (scrolldirection scroll))
                           (rangex (referenceview scroll))
                         (rangey (referenceview scroll))))
    (setf *initscrollpos*  (if (equal :h (scrolldirection scroll))
                               (list (om-point-h (om-view-position self))
                                     (+ (om-point-h (om-view-position self)) (om-width self)))
                             (list (om-point-v (om-view-position self))
                                   (+ (om-point-v (om-view-position self)) (om-height self)))))
    (om-init-motion-functions scroll 'move-maq-scroller 'release-maq-scroller)))
  
(defmethod move-maq-scroller ((self maq-scroller) pos) 
  (let* ((panel (referenceview self))
         (maqranges (give-editor-list-range (om-view-container self)))
         (deltapix (if (equal :h (scrolldirection self))
                       (- (om-point-h pos) (om-point-h *ruler-last-click*))
                     (- (om-point-v pos) (om-point-v *ruler-last-click*))))
         (maxpix (if (equal :h (scrolldirection self)) (w self) (h self))))
         
    (if (< (+ (car *initscrollpos*) deltapix) 0)
              (setf deltapix (- (car *initscrollpos*)))
            (if (> (+ (cadr *initscrollpos*) deltapix) maxpix)
                (setf deltapix (- maxpix (cadr *initscrollpos*)))
              ))
          
    (if (equal :h (scrolldirection self))
        (progn 
          (setf (rangex panel)
                (om-round (om+ *initrange*
                               (* (/ deltapix (w self)) (max (cadr maqranges) (cadr *initrange*))))))
          (om-redraw-view (rulerx panel))
          (when (rulermetric (editor panel)) (om-redraw-view (rulermetric (editor panel))))   
          )
      (progn 
          (setf (rangey panel)
                (om-round (om+ *initrange*
                               (* (/ (- deltapix) (h self)) (- (max (fourth maqranges) (cadr *initrange*))
                                                               (min (third maqranges) (car *initrange*)))))))
          (om-redraw-view (rulery panel))
          )
      )
    (update-view-of-ruler panel)
    ;(setf *ruler-last-click* (om-add-points *ruler-last-click* (om-make-point deltapixx deltapixy)))
    ))



(defmethod release-maq-scroller ((self maq-scroller) pos)
  (om-invalidate-view self)
  (om-invalidate-view (referenceview self)))

(defmethod om-view-click-handler ((self maq-scroller) pos)
  (let* ((panel (referenceview self))
         (item (scrollitem self))
         (maqranges (give-editor-list-range (om-view-container self)))
         (pix (if (equal :h (scrolldirection self)) (om-point-h pos) (om-point-v pos)))
         (deltapix 0))
        
    (if (equal :h (scrolldirection self))
        (let ((x (om-point-h (om-view-position item))))
          (if (< pix x)
              (setf deltapix (- pix x))
            (if (> pix (+ x (om-width item)))
                (setf deltapix (- pix (+ x (om-width item))))))
          (setf (rangex panel)
                (om-round (om+ (rangex panel)
                              (* (/ deltapix (w self)) (max (cadr maqranges) (cadr (rangex panel)))))))
          (om-redraw-view (rulerx panel))
          (when (rulermetric (editor panel)) (om-redraw-view (rulermetric (editor panel))))
          )
      (let ((y (om-point-v (om-view-position item))))
          (if (< pix y)
              (setf deltapix (- pix y))
            (if (> pix (+ y (om-height item)))
                (setf deltapix (- pix (+ y (om-height item))))))
          (setf (rangey panel)
                (om-round (om+ (rangey panel)
                              (* (/ (- deltapix) (h self)) (- (max (fourth maqranges) (cadr (rangey panel)))
                                                              (min (third maqranges) (car (rangey panel))))))))
          (om-redraw-view (rulery panel))
         )
      )
    (update-view-of-ruler panel)
    ))



          
             


(defmethod update-scrollers ((self maquettepanel))
  (let* ((ranges (give-editor-list-range (om-view-container self)))
         (ratioxpos (/ (- (car (rangex self)) (car ranges))
                       (- (max (cadr ranges) (cadr (rangex self)))  (car ranges))))
         (ratioxsize (/ (- (cadr (rangex self)) (car (rangex self)))
                        (- (max (cadr ranges) (cadr (rangex self))) (car ranges))))
         
         (ratioypos (/ (-  (max (fourth ranges) (second (rangey self))) (second (rangey self)))
                       (max 1 (- (max (fourth ranges) (second (rangey self))) (min (third ranges) (first (rangey self)))))))
         (ratioysize (/ (- (second (rangey self)) (first (rangey self)))
                       (max 1 (- (max (fourth ranges) (second (rangey self))) (min (third ranges) (first (rangey self))))))))
    
    (when (scrollerx self)
      (set-scroll-pos (scrollerx self) ratioxpos)
      (set-scroll-size (scrollerx self) ratioxsize))
    (when (scrollery self)
      (set-scroll-pos (scrollery self) ratioypos)
      (set-scroll-size (scrollery self) ratioysize))
    ))




;(when *palette*
;  (if (Idle-p *general-player*)
;      (palette-act *palette* (if (selection-to-play-? self) 4 0))
;    (palette-act *palette* 1)))

(defmethod handle-key-event ((self MaquettePanel) char) 
   (case char
     (#\SPACE
      (editor-play/stop (editor self)))
     (:om-key-esc (reset-cursor self))
     (#\x (mute-boxes self))
     (#\p (editor-play (editor self)))
     (#\g (setf (grille-p self) (not (grille-p self)))
          (om-invalidate-view self))
     (#\s (editor-stop (editor self)))
     (#\z (lock-boxes self))
     (#\I (mapc 'reinit-contents (get-actives self)) 
          (reinit-connections self))
     (#\C (maquette-color self))
     (#\c (if (get-actives self) (maquette-color-boxes self)
            (call-next-method)))
     (#\a (mapc 'internalize-patch (get-actives self)))
     (#\y (align-boxes self))
     (#\q (memesize-boxes self))
     (#\t (time-align self))
     (#\v  (om-eval-enqueue 
             `(progn
                (setf *cur-eval-panel* ,self)
                (mapc 'eval-box ',(get-actives self))
                (clear-ev-once ,self))
             self
             ))
     (#\V (om-eval-enqueue  
           `(eval-maquette ,self) self))
     
     (otherwise (call-next-method))))

(defmethod get-help-list ((self maquettepanel))
  (list '(("space" "Play / Stop")
          ("cmd+box" "Create Temporal Patch")
          ("cmd+shift+box" "Create Internal Maq.")
          ("lrud" "Move")
          ("shift+lrud" "Move faster")
          ("del" "Delete")
          (("g") "show/hide Grid")
          (("v") "Eval selected boxes")
          (("b") "lock or change eval mode Button")
          ;("z") "Lock")
          (("x") "mute")
          )
        '((("c") "change Color")
          (("C") "change background Color")
          ;(("A") "Align")
          ;(("D") "Redraw All")
          (("i") "reInitialize box")
          (("I") "reInitialize contents")
          (("y") "align box Y")
          (("q") "set identical box ySize")
          (("t") "time align")
          (("m") "show/hide Miniview")
          (("M") "show/hide all Miniviews")
          (("n") "show/hide Name")
          (("a") "internalize patch Abstraction")
          )))
  
(defmethod help-items ((self maquetteeditor)) (call-next-method))



;======
(defmethod mute-boxes ((self MaquettePanel))
  (let ((boxes (get-actives self 'tempobjframe)))
    (loop for box in boxes do 
          (setf (mute (object box)) (not (mute (object box))))
          (om-invalidate-view box))
    ))


(defmethod lock-boxes ((self MaquettePanel))
  (let ((boxes (get-actives self)))
    (loop for box in boxes do 
          (setf (lock (object box)) (not (lock (object box))))
          (om-invalidate-view box))
    )) 

;======

(defmethod get-memesize (boxes)
  (let ((mino 1000000)
        (mins 1000000))
    (loop for box in boxes do 
          (when (< (offset (object box)) mino) 
            (setf mins (sizey (object box)))
            (setf mino (offset (object box)))))
    mins))

(defmethod get-memepos (boxes)
  (let ((mino 1000000)
        (minpos 1000000))
    (loop for box in boxes do 
          (when (< (offset (object box)) mino) 
            (setf minpos (posy (object box)))
            (setf mino (offset (object box)))))
     minpos))

(defmethod memesize-boxes ((self MaquettePanel))
   (let* ((boxes (get-actives self))
          (news (get-memesize boxes)))
     (loop for box in boxes do (setf (sizey (object box)) news))
     ))


(defmethod align-boxes ((self MaquettePanel))
   (let* ((boxes (get-actives self))
          (newp (get-memepos boxes)))
     (loop for box in boxes do (setf (posy (object box)) newp))
     ))

(defmethod time-align ((self MaquettePanel))
   (let* ((boxlist (sort (get-actives self) '< 'offset)))
     (loop for boxes on boxlist 
           when (cdr boxes) do
           (let* ((box1 (object (car boxes)))
                  (box2 (object (cadr boxes)))
                  (beg1 (offset box1))
                  (beg2 (offset box2))
                  (end1 (+ beg1 (* (extent box1) (strech-fact box1)))))
             (if (< (- beg2 beg1) (abs (- end1 beg2)))
                 ;;; align to begin
                 (setf (offset box2) beg1)
               (setf (offset box2) end1))))))
               
          


;;; play maquette : construit le maquette-obj (en copiant les objs contenus)
(defmethod cons-play-maquette-object ((self OMMaquette) objs)
  (if *maquette-play*
      (or (value self)
          (cons-copy-maquette-object self objs))
    (cons-copy-maquette-object self objs)))


;;;==========================

;;; applies to all the boxes
(defmethod select-maquette-player ((self ommaquette))
  (if (boxes self)
      (let ((player (select-player (car (boxes self)))))
        (when player 
          (maq-changeparams self (list 'player 'outport) 
                            (list (get-edit-param (car (boxes self)) 'player)
                                  (get-edit-param (car (boxes self)) 'outport)))
          ))
    (om-beep-msg "This maquette has no playable contents.")))

(defmethod reference-object ((self temporalbox)) (car (value self)))

(defmethod select-maquette-player ((self temporalbox))
  (let ((previousplayer (get-edit-param self 'player))
        (newplayer (select-player self)))
    (when (and newplayer (not (equal previousplayer newplayer)))
      (player-special-action newplayer)
      (player-init newplayer)
      )))




(defmethod cursor-panes ((self maquetteeditor)) (list (panel self)))


;(defmethod schedule-editor-contents ((self maquetteeditor))
;  (loop for object in (inside (value (object self)))
;        for param in (param-list (value (object self))) do
;        (let ((objstart (offset->ms object))
;              (pl (cdr (assoc 'player param))))
;          ;(print (list object pl objstart (get-interval-to-play self)))
;          (player-schedule (player self) 
;                           object
;                           pl 
;                           :at objstart
;                           :interval (get-interval-to-play self)))
;        ))


(defmethod boxestoplay ((self maquetteeditor))
  (if (and (get-actives (panel self)) (not (equal :interval (cursor-mode (panel self)))))
      (mapcar 'object (get-actives (panel self)))
    (loop for b in (boxes (object self)) when (and (boxtempobj-p b) (not (mute b))) collect b)))
  

;;; computes the value...
;(defmethod get-obj-to-play ((self maquetteeditor)) (get-obj-to-play (panel self)))

(defmethod get-obj-to-play ((self maquetteeditor))
  (if (and (eval-func (object self)) (value (object self)))
    ; ??? why (commented since OM 6.7.1)
    ;(if *midi-microplay* 
    ;  (list (value (object self)))
    ;  (list (value (object self)) :approx 8))
    ;(list (value (object self)))
      (value (object self))
    ;(list (cons-play-maquette-object (object self) (boxestoplay self)))
    (cons-play-maquette-object (object self) (boxestoplay self))    
    ))

(defmethod get-interval-to-play ((self maquetteeditor))
  (if (equal (cursor-mode (panel self)) :interval)
      (call-next-method)
    (if (get-actives (panel self))
        (list (loop for boxf in (get-actives (panel self)) minimize (offset (object boxf)))
              (loop for boxf in (get-actives (panel self)) maximize (+ (offset (object boxf)) (round (* (extend (object boxf)) (strech-fact (object boxf))))))))))
       
#|            

(defun selection-interval (selection)
  (cond ((track-p (car selection))
         (list 0 
               (loop for track in selection maximize (get-obj-dur track))))
        (t (list (loop for obj in selection minimize (start-t obj))
                 (loop for obj in selection maximize (end-t obj))))))


(defmethod get-obj-to-play ((self sheeteditor))
  (let* ((scorepanel (panel (score-view self)))
         (selection (selection? scorepanel)))
    (if selection
        (cond ((track-p (car selection))
               (make-instance 'omsheet :voices (clone selection)))
              (t (make-instance 'omsheet 
                                :voices (list (make-instance 'sheet-track 
                                                             :objs (clone selection))))))
      (call-next-method))))

(defmethod get-selection-to-play ((self MaquettePanel))
    (cond
    ((cursor-p self)
     (let ((interval (cursor-interval self)))
       (values (list (if (and (eval-func (object self)) (value (object self)))
                       (list (value (object self)))
                       (let ((boxestoplay (loop for b in (boxes (object self)) when (and (boxtempobj-p b) (not (mute b))) collect b)))
                         (list (cons-play-maquette-object (object self) boxestoplay)))
                       )
                     :interval interval)
               (first interval)
               (second interval))))
    ((get-actives self)
     (let* ((minim (round (loop for item in (get-actives self)
                                minimize (offset (object item)))))
            (maxim (round (loop for item in (get-actives self)
                         maximize (+ (offset (object item)) (round (* (extend (object item)) (strech-fact (object item)))))))))
       (values (list (if (and (eval-func (object self)) (value (object self)))
                       (list (value (object self)))
                       (let ((boxestoplay (loop for b in (get-actives self) when (and (boxtempobj-p (object b)) (not (mute (object b)))) collect (object b))))
                         (list (cons-play-maquette-object (object self) boxestoplay)))
                       ) 
                     :interval (list minim maxim))
               minim
               maxim)))))
|#






