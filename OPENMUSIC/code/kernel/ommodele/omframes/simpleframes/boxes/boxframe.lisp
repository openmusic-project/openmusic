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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;DocFile
;Simple frame classes for OMBox objects
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;=========================VIEWS IN BOX FRAMES============================
; i.e.  outputs, ev-once, resize, name
;========================================================================

;==================================================
;General outputs class
;==================================================

(defclass outfleche (icon-view) 
   ((index :initform 0 :initarg :index :accessor index)))

;------------INITS
(defmethod initialize-instance :after ((self outfleche) &key controls)
   (declare (ignore controls))
   (setf (iconID self) 185))

;------------EVENTS

; for autoconnections:
(defparameter *target-out* nil)
(defparameter *target-in* nil)
(defparameter *auto-create-connect* nil)

(defmethod om-view-click-handler ((self outfleche) where)
  (setf *make-connection* nil)
  (cond 
   ;((and (om-command-key-p) (om-option-key-p)) (setf *target-out* self)) ; for autoconnections 
   ((om-option-key-p) (setf *target-out* self)) ; for autoconnections 
   ((and (om-command-key-p) (om-shift-key-p))
    (om-with-cursor *om-wait-cursor*
      (om-eval-enqueue `(let (rep)
                          (setf *cur-eval-panel* ,(panel (om-view-container self)))
                          (setf rep (omNG-box-value ',(object (om-view-container self))  ',(index self)))
                          (format *om-stream* "OM => ~S~%"  rep)
                          (clear-ev-once ,(om-view-container (om-view-container self)))
                          (make-graph-instance rep ,(om-view-container (om-view-container self)) 
                                               ,(om-make-point (x (om-view-container self))  
                                                               (+ 10 (y self) (y (om-view-container self)))) 
                                               ,(om-view-container self))))))
   ((om-command-key-p) 
    (om-with-cursor *om-wait-cursor*
      (om-eval-enqueue `(let (rep)
                          (handler-bind ((error #'(lambda (c) 
                                                    (when *msg-error-label-on*
                                                      (om-message-dialog (string+ "Error while evaluating the box " ,(string (name (object (om-view-container self)))) " : " 
                                                                                  (om-report-condition c ))
                                                                         :size (om-make-point 300 200))
                                                      (clear-after-error ,(object (om-view-container self)))
                                                      (om-abort)
                                                      ))))
                            (setf *cur-eval-panel* ,(panel (om-view-container self)))
                            (setf rep (omNG-box-value ',(object (om-view-container self))  ',(index self)))
                            (format *om-stream* "OM => ~S~%"  rep)
                            (clear-ev-once ,(om-view-container (om-view-container self)))
                            )))))
   ((om-shift-key-p)
    (let* ((panel (panel (om-view-container self)))
           (initpoint (om-convert-coordinates where self panel)))
      (om-init-motion-click self where
                            :motion-draw 'draw-auto-connection-drag :draw-pane panel :display-mode 5
                            :motion-action 'connection-drag
                            :release-action 'release-auto-connection-drag
                            )
      (setf *show-input-vals* nil)))

   (t (let* ((panel (panel (om-view-container self)))
             (initpoint (om-convert-coordinates where self panel)))
       
        (om-init-motion-click self where
                              :motion-draw 'draw-connection-drag :draw-pane panel :display-mode 5
                              :motion-action 'connection-drag
                              :release-action 'release-connection-drag
                              )
        (setf *show-input-vals* nil))))
  t)


(defmethod connection-drag ((self outfleche) pos prevpos) 
   (let* ((panel (panel (om-view-container self)))
          (ppos (om-convert-coordinates pos self panel))
          (myview (om-find-view-containing-point panel ppos)))
     #|
     (when (and (input? myview) *mag-in-out*)
       (om-view-mouse-enter-handler myview)
       (om-show-tooltip myview t t)
       (redraw-frame (om-view-container myview)))
     |#
     (if (input? myview)
         (om-show-tooltip myview t t)
       (om-hide-tooltip myview))))

(defmethod release-connection-drag ((self outfleche) init-pos pos)
  (let* ((panel (panel (om-view-container self)))
         (ppos (om-convert-coordinates pos self panel))
         (ctrl (om-find-view-containing-point panel ppos)))
    (om-hide-tooltip ctrl)
    (setf *show-input-vals* t)
    (connect-box self ctrl)
    (om-hide-tooltip panel)
    #+linux
    (when (and ctrl (typep ctrl 'input-funboxframe)(not (typep panel 'maquettepanel)))
      (update-miniview (iconview (om-view-container ctrl)) (value (object (om-view-container ctrl)))))
    ))

#|
(defmethod draw-connection-drag ((self om-view) init-pos pos)
  (om-with-line-size 2
    (om-with-line '(2 3) ; #+macosx '(2 3) #-macosx '(1 1)
      (om-with-fg-color self (om-make-color-alpha 0 0 0 0.5)
        (om-draw-line (om-point-x init-pos) (om-point-y init-pos) (om-point-x pos) (om-point-y pos))))))
|#

(defmethod draw-connection-drag ((self om-view) init-pos pos) 
  (if (input? (om-find-view-containing-point self pos))
      (om-with-line-size 4
        (om-with-line #+(or linux macosx) '(2 5) #+win32 '(1 1) 
          (om-with-fg-color self 
              (om-make-color-alpha 0 0 1 0.7)
            (om-draw-line (om-point-x init-pos) (om-point-y init-pos) (om-point-x pos) (om-point-y pos)))))

    (om-with-line-size 2
        (om-with-line '(2 3) ; #+macosx '(2 3) #-macosx '(1 1)
          (om-with-fg-color self 
              (om-make-color-alpha 0 0 0 0.5)
            (om-draw-line (om-point-x init-pos) (om-point-y init-pos) (om-point-x pos) (om-point-y pos)))))))
 

(defmethod draw-auto-connection-drag ((self om-view) init-pos pos)
    (om-with-line-size 3
        (om-with-line '(2 3) ; #+macosx '(2 5) #-macosx '(1 1)
          (om-with-fg-color self 
              (om-make-color-alpha 1 0 0 0.5)
            (om-draw-line (om-point-x init-pos) (om-point-y init-pos) (om-point-x pos) (om-point-y pos))))))


(defmethod release-auto-connection-drag ((self outfleche) init-pos pos) 
  (let* ((panel (panel (om-view-container self)))
         (ppos (om-convert-coordinates pos self panel))
         (ctrl (om-find-view-containing-point panel ppos)))
    (setf *auto-create-connect* self)
    (make-undefined-box panel ppos)
    (om-hide-tooltip ctrl)
    (setf *show-input-vals* t)
    (connect-box self ctrl)
    (om-hide-tooltip panel)
    ))


;Only for magnification
(defmethod om-view-mouse-enter-handler ((self outfleche)) 
  (when *mag-in-out*
    (let* ((pos (om-view-position self))
           (ypos (om-point-y pos))
           (xpos (om-point-x pos)))
      (om-set-view-size self (om-make-point 12 12))
      (om-set-view-position self (om-make-point (- xpos 2) (+ ypos 0)))
      )))

(defmethod om-view-mouse-leave-handler ((self outfleche)) 
  (when *mag-in-out*
  (let* ((parsize (om-view-size (om-view-container self)))
         (psizey (om-point-y parsize)))
  (om-set-view-size self (om-make-point 8 8))
  (om-set-view-position self (om-make-point (+ (om-point-x (om-view-position self)) 2) (- psizey 9)))
  ;(redraw-frame (om-view-container self)) ;init all ?
  )))




;--------------CONNECTION
(defmethod connect-box ((self t) (ctrl t)) nil)

(defmethod connect-box ((self outfleche) (ctrl input-funboxframe))
  (let* ((boxframe (om-view-container self))
         (boxcall (object boxframe))
         (boxtarget (om-view-container ctrl)))
    (if (not (recursive-connection  boxtarget boxframe))
      (progn
        (when (connected? (object ctrl))
          (remove-connection boxtarget (position ctrl (inputframes boxtarget) :test 'equal))) 
        (connect-ctrl boxcall (object ctrl) (index self))
        (push (new-connection boxtarget (position ctrl (inputframes boxtarget) :test 'equal))
              (connections boxtarget))
        (redraw-connections boxtarget)
        (modify-patch (panel boxtarget))
        (if (equal (type-of boxtarget) 'sendframe)
            (progn
              (setf (value (object boxtarget)) 
                    (car (omng-box-value (object boxtarget))));stores value in omsend once connected
              )))
      (om-beep-msg "Impossible to connect, this would create a cycle."))))

    


;==================================================
; The main icon in a box
;==================================================
(defclass icon-box (icon-view om-view-drag) ())
;-------------EVENTS (command-key-p)

(defmethod om-view-click-handler ((self icon-box) where) 
  ;(call-next-method))
  ;(when (equal (call-next-method) self)  ;;; new for click in lock-button
  (let ((box (om-view-container self)))
    (cond 
     ((and (om-command-key-p) (om-option-key-p) (om-shift-key-p))
      (loop for i in (inputframes box)
            do (disconnect-box box i)))
     ((and (om-option-key-p) (om-shift-key-p))
      (loop for i in (inputframes box)
            do (connect-box *target-out* i)))
     ((om-option-key-p) (setf *target-out* (car (outframes box))))
     ;((om-command-key-p) (connect-box *target-out* (1st-not-con-input box)))
     (t ))
    (toggle-icon-active-mode box)
   ;)
    ))

(defmethod om-view-doubleclick-handler ((self icon-box) where)
   (declare (ignore where))
   (let ((clicked (call-next-method)))
     (when (equal self clicked)
       (OpenObjectEditor (object (om-view-container self))))
     clicked))
               


;-----------For Drag and Drop
(defmethod om-drag-selection-p ((self icon-box) mouse-position)
   (declare (ignore mouse-position))
   #+cocoa(not (om-command-key-p))
   #-cocoa t)

(defmethod get-drag-object ((self icon-box))
  (om-view-container self))

(defmethod get-pos-in-object ((self icon-box) where)
  (om-add-points (om-view-position self) where))


;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view icon-box))
  (om-highlight-view (om-view-container view) t))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view icon-box))
  (om-highlight-view (om-view-container view) nil))

; (trace om-drag-enter-view)

;==================================================
; The name dialog-item in a box
;==================================================
(defclass edit-boxframe-name (om-editable-text) 
  ((object :initform nil :initarg :object :accessor object)))

(defclass box-dialog-name (om-static-text-drag) ())

(defmethod get-drag-object ((self box-dialog-name))
  (om-view-container self))

(defmethod get-pos-in-object ((self box-dialog-name) where)
  (om-add-points (om-view-position self) where))

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view box-dialog-name))
  (om-highlight-view (icon-finder view) t))

;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view box-dialog-name)) 
  (om-highlight-view (icon-finder view) nil))


(defmethod icon-finder ((self box-dialog-name)) (om-view-container self))

;;; maintenant c'est avec un double click
;;;(defmethod om-dialog-item-action ((self box-dialog-name))
;;;  (change-name-box (icon-finder self)))

(defmethod allow-rename ((self t)) t)
(defmethod allow-rename ((self OMBoxcall)) nil)
(defmethod allow-rename ((self OMBoxInstance)) t)
(defmethod allow-rename ((self OMBoxPatch)) t)
(defmethod allow-rename ((self OMBoxMaquette)) t)



(defmethod om-view-doubleclick-handler ((self box-dialog-name) where)
   (declare (ignore where))
   (if (allow-rename (object (icon-finder self)))
     (change-name-box (icon-finder self))
     (om-beep-msg (format nil "~A boxes cannot be renamed!" (type-of (object (icon-finder self))))))
   t)
  
(defmethod om-view-click-handler ((self box-dialog-name) where) 
   (declare (ignore where))
(let ((box (om-view-container self)))
    (cond 
     ((and (om-command-key-p) (om-option-key-p) (om-shift-key-p))
      (loop for i in (inputframes box)
            do (disconnect-box box i)))
     ((and (om-option-key-p) (om-shift-key-p))
      (loop for i in (inputframes box)
            do (connect-box *target-out* i)))
     ((om-option-key-p) (setf *target-out* (car (outframes box))))
     ;((om-command-key-p) (connect-box *target-out* (1st-not-con-input box)))
     (t ))
    (toggle-icon-active-mode box)
   ;)
    ))
  
  
(defmethod om-view-key-handler ((self edit-boxframe-name) char) 
  (let ((size-name (get-name-size (om-dialog-item-text self) (om-get-font self))))
    
    (om-set-view-position self 
                         (om-make-point (+  (x (object self)) (x (iconView (object self)))  
                                            (- (round size-name 2)) 10)
                                        (om-point-v (om-view-position self))))
    (om-set-view-size self (om-make-point (om-point-h (om-make-point size-name 10)) (om-height self)))
    ))

    

(defmethod om-dialog-item-action ((self edit-boxframe-name))
  (exit-from-dialog self (om-dialog-item-text self)))


;;;textview on panel
(defmethod exit-from-dialog  ((self edit-boxframe-name) new-text-item)
  (let ((theeditor (editor (om-view-container self))))
    (if (equal new-text-item "")
      (om-add-subviews (object self) (nameView (object self)))
      (omG-rename (object self) new-text-item))
    (setf (text-view theeditor) nil)
    (om-remove-subviews (panel theeditor) self)))



  

;=========================BOX FRAMES AND SUBCLASSES============================
;==============================================================================

;;; NOT FOR INSTANCIATE
;;; SUPERCLASS OF BOXFRAME AND SIMPLEBOXFRAME
(defclass omboxframe (om-view-drop) ;
;(defclass boxframe (OMSimpleFrame om-view-drop)
   ((outframes :initform nil :accessor outframes)
    (inputframes :initform nil :accessor inputframes)
    (iconview :initform nil :accessor iconview)
    (nameview :initform nil :accessor nameview)
    (connections :initform nil :accessor connections)
    (lock-button :initform nil :accessor lock-button)
    (resize-box :initform nil :accessor resize-box))
   (:documentation "Simple frame for OMBoxcall meta objects. #enddoc#
#seealso# (OMBoxcall icon-box box-dialog-name c-connection) #seealso#
#outframes# A list with the outputs of the box.#outframes#
#inputframes# A list with the inputs of the box.#inputframes#
#iconview# The icon subview of the box, it is a icon-box instance.#iconview#
#nameview# The name subview of the box, it is a box-dialog-name instance.#nameview#
#connections# A list of connections where the box is the target.#connections#
#lock-button# The lock-button subview if exists.#lock-button#"))

;(defclass boxframe (omboxframe OMCompoundFrame) ())
;(defclass simpleboxframe (omboxframe OMAtomicFrame) ())

(defclass boxframe (omboxframe OMAtomicFrame) ())

(defmethod get-box-frame ((self omboxframe)) self)


;---------Predicats
(defmethod boxframe-p ((self omboxframe)) t)
(defmethod boxframe-p ((self t)) nil)

(defmethod recursive-connection ((target omboxframe) (source omboxframe))
   (if (equal target source) t 
       (recursive-connection? (object target) (object source))))


(defmethod om-draw-contents ((self omboxframe))
   (om-with-focused-view self
     (draw-before-box self)
     (call-next-method)
     (draw-after-box self)
   ;;;(if (nameview self) (om-invalidate-view (nameview self)))
   ))

;;; CARRE GRIS TOUT AUTOUR
(defmethod draw-before-box ((self omboxframe))
  (when (frame-size (object self))
    (om-with-focused-view self
      (om-with-fg-color nil (om-make-color 0.921 0.921 0.921)
        (om-fill-rect 0 8 (w self) (- (h self) 17))))))
    
(defmethod draw-after-box ((self omboxframe)) nil)

(defmethod 1st-not-con-input ((self omboxframe))
  (let* ((ins (inputframes self))
         (objs (mapcar 'object ins))
         (con? (mapcar 'connected? objs))
         (lst (mapcar 'list con? ins)))
    (loop for i in lst
          when (not (car i))
            return (second i))))

(defmethod om-view-click-handler ((self omboxframe) where) ;(om-inspect self)
  ;(print (first-not-connected-input self))
  (do-click-inbox self where)
  (cond 
   ((and (om-command-key-p) (om-option-key-p) (om-shift-key-p)) 
    (loop for i in (inputframes self)
          do (disconnect-box self i)))
   ((and (om-option-key-p) (om-shift-key-p))
    (loop for i in (inputframes self)
          do (connect-box *target-out* i)))
   ((om-option-key-p) (setf *target-out* (car (outframes self))))
   ;((om-command-key-p) (connect-box *target-out* (1st-not-con-input self)))
   (t ))
  self)

(defmethod om-view-doubleclick-handler ((self omboxframe) where)
   (declare (ignore where))
   (when (eq (call-next-method) self)
     (when (frame-size (object self))
       (OpenObjectEditor (object self))
       self)))


(defmethod do-click-inbox ((self omboxframe) where)
   (declare (ignore where))
   (toggle-icon-active-mode self))


;;; bizarre : on doit mettre les size des in/outs et lock button a la main sinon ils sont modifies.
;;; aussi pour les "lisp" icon...
(defmethod om-set-view-size ((self omboxframe) size)
  (let* ((w (om-point-x size))
         (h (om-point-y size))
         (outs (copy-list (outframes self)))
         (ins (copy-list (inputframes self)))
         (outputs (sort outs  '< :key 'index))
         (numouts (length outputs))
         (numins (length ins)) 
         (i 0))
    (loop for out in outputs do
          (setf i (+ i 1))
          (om-set-view-position out (om-make-point (- (* i (round w (+ numouts 1))) 4) 
                                                   (- h 9)))
          (om-set-view-size out (om-make-point 8 8)))
    (setf i 0)
     (loop for in in ins do
           (setf i (+ i 1))
           (om-set-view-position in (om-make-point (- (* i  (round w (+ numins 1))) 4) 1))
           (om-set-view-size in (om-make-point 8 8)))
    
    (setf (frame-size (object self)) size)
    (when (lock-button self) (om-set-view-size (lock-button self) (om-make-point 10 10)))
    (when (resize-box self) (om-set-view-position (resize-box self) (om-make-point (- w 10) (- h 10))))
    )
  (call-next-method)
  (centre-icon self))


(defmethod add-box-resize ((self omboxframe))
   "add a resize view to the wiew 'self'"
   (om-add-subviews self
     (setf (resize-box self)
           (om-make-view 'c-resize-box
                         ;:bg-color (om-make-color-alpha 0.8 0.8 0.8 0.2)
                         :size (om-make-point 10 10)
                         :position (om-make-point (- (w self) 10) (- (h self) 10))))))


;---------------EVENTS

(defmethod add-subview-extra ((self omboxframe)) nil)

(defmethod close-frame ((self omboxframe))
   "Called when you close the patch containing self."
   (setf (frames (object self)) nil))

(defmethod redraw-frame ((self omboxframe))
   "Update graphically 'self'."
   (let ((thescroll (om-view-container self))
         (object (object self)) frame)
     (when thescroll
       (om-remove-subviews thescroll self)
       (setf frame (make-frame-from-callobj object))
       (om-add-subviews thescroll frame)
       (update-graphic-connections frame (get-elements (object thescroll)))
       (make-move-after thescroll (list frame))
       frame)))

(defmethod update-graphic-connections ((self omboxframe) boxes)
   (let ((i 0) rep exist?)
     (mapc #'(lambda (input)     
               (if (connected? (object input))
                 (if (and (setf exist? (member (first (connected? (object input))) boxes :test 'equal))
                          (>= (- (numouts (car exist?)) 1) (second (connected? (object input))))) 
                   (let ((newcon (new-connection self i)))
                     (when (nth 2 (connected? (object input)))
                       (setf (points newcon) (nth 2 (connected? (object input)))))
                     (when (nth 3 (connected? (object input)))
                       (setf (ccolor newcon) (nth 3 (connected? (object input)))))
                     (push newcon rep))
                   (setf (connected? (object input)) nil)))
               (incf i)) (inputframes self))
     (setf (connections self) rep)))

(defmethod disconnect-box ((self omboxframe) (ctrl input-funboxframe))
  (let ((nth-count (position ctrl (inputframes self) :test 'equal)))
    (remove-connection self nth-count)
    (setf (connected? (object ctrl)) nil)
    (modify-patch (om-view-container self))))

(defmethod OMGMoveObject ((self omboxframe) new-position)
   "Move 'self' to 'new-position'."
   ;(mapc #'(lambda (conection)
   ;          (draw-connection conection nil)) (connections self))
   (setf new-position (borne-position new-position))
   (om-set-view-position self new-position)
   (setf (frame-position (object self)) new-position)
   (om-highlight-view self nil)
   )

(defmethod move-frame-delta ((self omboxframe) dir)
   "Move self by 1 or 10 pixel to the left/rigth/up/down."
   (let ((pixnum (if (om-shift-key-p) 10 1)))
     (case dir
       (0 (omGMoveObject self (om-subtract-points (om-view-position self) (om-make-point 0 pixnum))))
       (1 (omGMoveObject self (om-add-points (om-view-position self) (om-make-point 0 pixnum))))
       (2 (omGMoveObject self (om-add-points (om-view-position self) (om-make-point pixnum 0))))
       (3 (omGMoveObject self (om-subtract-points (om-view-position self) (om-make-point pixnum 0)))))))

(defmethod reinit-size ((box omboxframe))
   "Set the size of self to the initial size."
   (when (frame-size (object box))
     (setf (frame-size (object box)) nil)
     (box-draw-connections box nil)
     (omG-select (redraw-frame box))   
     ;(setf (frame-size (object box)) (om-view-size box))
     ))
(defmethod box-resize-x-plus  ((box omboxframe))
  "Enlarge box horizontally with increment of 10pt."
  (let* ((size (om-view-size box))
         (x (om-point-h size))
         (y (om-point-v size)))
    (change-boxframe-size box (om-make-point (+ x 10) y))))

(defmethod box-resize-x-minus ((box omboxframe))
  "Decrease size of self horizontally."
  (let* ((size (om-view-size box))
         (x (om-point-h size))
         (y (om-point-v size)))
    (if (>= x 40);30
        (change-boxframe-size box (om-make-point (- x 10) y)) 
      (om-beep-msg "Minimum size reached!"))))

(defmethod box-resize-y-plus ((box omboxframe))
  "Enlarge box vertically with increment of 10pt."
     (let* ((size (om-view-size box))
           (x (om-point-h size))
           (y (om-point-v size)))
(change-boxframe-size box (om-make-point x (+ y 10)))))

(defmethod box-resize-y-minus ((box omboxframe)) 
  "Decrease size of self vertically."
  (let* ((size (om-view-size box))
         (x (om-point-h size))
         (y (om-point-v size)))
     (if (>= y 60)
         (change-boxframe-size box (om-make-point x (- y 10)))
      (om-beep-msg "Minimum size reached!"))
     ))


(defmethod reinit-contents ((box omboxframe))
   "Set the contents of box to the initial value"
   nil)

(defmethod update-doc ((box omboxframe)) nil)

(defmethod add-one-input ((self omboxframe)) 
   "Add an optional inputframe to 'self'."
  (when (do-add-one-input (object self))
    (box-draw-connections self nil)
    ;;; !! frame may have changed
    (omG-select (redraw-frame (car (frames (object self))))) ; omG-select
    ))

(defmethod add-all-inputs ((self omboxframe)) 
   "Add all optional inputframe to 'self'."
   (if (do-add-all-inputs (object self))
    (progn (box-draw-connections self nil)
      (omG-select (redraw-frame self)))
      (add-one-input self)
      ))

(defmethod delete-one-input ((self omboxframe))
   "Delete an optional inputframe from 'self'."
  (when (do-delete-one-input (object self))
    (box-draw-connections self nil)
    (omG-select (redraw-frame (car (frames (object self)))))
    ))
#|
(defmethod delete-all-inputs ((self omboxframe))
  (let* ((obj (object self))
         (init (min-inp-number (reference obj)))
         (curr (length (inputs obj))))
    (loop while (not (= init curr))
          do (progn 
               (delete-one-input self)
               (setf curr (length (inputs obj)))))))
|#

(defmethod delete-all-inputs ((self omboxframe)) 
  (if (do-delete-all-inputs (object self))
      (do-delete-one-input (object self))
    (unless (equal (type-of  self) 'boxtypeframe) 
      (let* ((obj (object self))
             (init (min-inp-number (reference obj)))
             (curr (length (inputs obj))))
        (loop while (not (= init curr))
              do (progn 
                   (delete-one-input self)
                   (setf curr (length (inputs obj)))))))))


(defmethod add-keywords ((self omboxframe)) 
   "Add a keyword inputframe to 'self'."
   (when (do-add-one-keyword (object self))
     (box-draw-connections self nil)
     (omG-select (redraw-frame self))))

(defmethod erase-keywords ((self omboxframe)) 
   "Delete a keyword inputframe from 'self'."
   (when (do-delete-one-keyword (object self))
     (box-draw-connections self nil)
     (omG-select (redraw-frame self))))


(defmethod eval-box ((self omboxframe))
  (omng-box-value (object self))
  (let ((v (current-box-value (object self) nil)))
    (if (consp v) 
        (if (> (length v) 1)
            (format *om-stream* "OM => [ ~{~S ~}]~%" v)
          (format *om-stream* "OM => ~S~%" (car v)))
      (format *om-stream* "OM => ~S~%" v)))
  )





;;; container de text-view = panel
(defmethod change-name-box ((self omboxframe))
  "Open a text dialog item and read a new name for the box."
  (let ((container (editor (om-view-container self)))
        (selftext (nameview self)))
    (when (text-view container)
      (exit-from-dialog (text-view container) (om-dialog-item-text (text-view container)) ))
    
    (setf (text-view container) (om-make-dialog-item 'edit-boxframe-name
                                                     (om-add-points (om-view-position self) (om-view-position selftext))
                                                     (om-make-point (get-name-size (om-dialog-item-text selftext) (om-get-font selftext)) 20)
                                                     ;(om-view-size selftext)
                                                     (om-dialog-item-text selftext)
                                                     :allow-returns nil
                                                     :di-selected-p t
                                                     :focus t
                                                     :object self
                                                     :container (panel container)
                                                     :font *om-default-font1*))
    (om-remove-subviews self selftext)
    ))


(defmethod show-online-tutorial ((self omboxframe))
   (show-online-tutorial (object self)))

(defmethod show-info-window ((self omboxframe) &optional (i 0))
   (show-info-window (object self) i))

(defmethod align-one-boxframe ((self omboxframe))
  (let* ((dx 20) (dy 20))
    (OMGMoveObject self (om-make-point (* (round (x self) dx) dx) (* (round (y self) dy) dy)))))

;--------------------INTERFACE
(defmethod omG-select :before ((self omboxframe)) 
   "Select also the nameView of self."
  (when (and (not (active-mode self)) (nameview self)) 
      (setf (selected-p (nameView self)) t)
      ))

(defmethod omG-select :after ((self omboxframe)) 
   "If there are selected connections it unselect them."
  (mapc #'(lambda (control) 
            (deactivate-connect control)) (get-connections (om-view-container self))))

(defmethod omG-unselect :before ((self omboxframe)) 
  (when (and  (active-mode self) (nameview self))
    (setf (selected-p (nameView self)) nil)))

(defmethod omG-rename ((self omboxframe) new-name)
   "Change the graphic name, not the name of the box."
  (setf (frame-name (object self)) new-name)
  (box-draw-connections self nil)
  (redraw-frame self)
  )

(defmethod omG-change-icon ((self omboxframe)  new-icon)
   "Set the icon ID of self to 'new-icon'."
   (setf (iconID (iconview self))  new-icon)
   (om-invalidate-view self))


(defmethod centre-icon ((self omboxframe))
   (let ((nameview (nameView self)))
     (om-set-view-position (iconview self) 
                        (om-make-point (- (round (w self) 2) (round (om-point-h (om-view-size (iconview self))) 2)) 10))
     (when nameview
       (om-set-view-position nameview
                             (om-make-point (- (round (w self) 2) (round (om-point-h (om-view-size nameview)) 2))
                                            (- (h self) 10 (h nameview)))))))
     
  

(defmethod allow-new-size ((self omboxframe) new-pos)
   (when (> (om-point-h new-pos) 10)
     (om-make-point (om-point-h new-pos) (om-point-v (om-view-size self)))))

(defmethod change-boxframe-size ((view omboxframe) new-position)
    (when (setf new-position (allow-new-size view new-position))
       (om-set-view-size view new-position)
       (make-move-after (om-view-container view) (list view))
       (om-invalidate-view view)))

(defmethod box-print-connections ((self omboxframe))
  (mapc #'(lambda (conection) (print-connection conection)) (connections self)))


(defmethod panel ((self omboxframe))
   (om-view-container self))

(defmethod editor ((self omboxframe))
   (editor (panel self)))

;==================================================
; BUTTON for ev-once lambda or lock
;==================================================

(defclass lock-button (button-icon) 
  ((mode :accessor mode :initarg :mode :initform "x")))

(defun get-icon-lock (str)
  (cond
   ((string= str "x") 167)
   ((string= str "&") 168)
   ((string= str "l") 145)
   ((string= str "o") 166)))

(defun get-str-lock (num)
  (case num
    (167 "x") (168 "&")  (145 "l") (166 "o")))

(defmethod om-view-doubleclick-handler ((self lock-button) where)
  (funcall (action self) self)
  (funcall (action self) self)
  t)

(defmethod add-rem-lock-button ((self omboxframe))
   (if (lock-button self)
     (remove-lock-button self)
     (add-lock-button self)))

(defmethod add-rem-lambda-button ((self omboxframe))
   (cond ((and (lock-button self) (string-equal (allow-lock (object self)) "l")
               (mode-allowed-p (object self) "l"))
          (remove-lock-button self))
         ((and (lock-button self) (mode-allowed-p (object self) "l"))
          (remove-lock-button self)
          (add-lock-button self "l"))
         (t (add-lock-button self "l"))))

(defmethod add-rem-evonce-button ((self omboxframe))
   (cond ((and (lock-button self) (string-equal (allow-lock (object self)) "&")
               (mode-allowed-p (object self) "&"))
          (remove-lock-button self))
         ((and (lock-button self) (mode-allowed-p (object self) "&"))
          (remove-lock-button self)
          (add-lock-button self "&"))
         (t (add-lock-button self "&"))))

(defmethod add-rem-itself-button ((self omboxframe))
   (cond ((and (lock-button self) (string-equal (allow-lock (object self)) "o")
               (mode-allowed-p (object self) "o"))
          (remove-lock-button self))
         ((and (lock-button self) (mode-allowed-p (object self) "o"))
          (remove-lock-button self)
          (add-lock-button self "o"))
         (t (add-lock-button self "o"))))

(defun mode-allowed-p (box mode)
  (find mode (allowed-lock-modes box) :test 'string-equal))

(defmethod allowed-lock-modes ((self omboxcall)) '("x" "&" "l" "o"))

(defmethod make-lock-button ((self omboxframe) mode)
  (om-make-view 'lock-button
                :IconID (get-icon-lock mode)
                :size (om-make-point 10 10)
                :position (om-make-point 0 0) ;;; (x (iconview self)) 8)
                :action #'(lambda (item)
                            (let* ((modes (allowed-lock-modes (object self)))
                                   (mpos (position (mode item) modes :test 'string-equal))
                                   (newmode (when mpos (nth (mod (1+ mpos) (length modes)) modes))))
                              (setf (mode item) newmode (iconID item) (get-icon-lock newmode))
                              (setf (allow-lock (object self)) newmode)
                              (om-invalidate-view self)
                              ))))
  
(defmethod add-lock-button ((self omboxframe) &optional (mode "x"))
   "Add a lock button, ff the box referenced by 'self' allow it."
   (when (allow-lock-button (object self))
     (setf (lock-button self) (make-lock-button self mode))
     (om-add-subviews (iconview self) (lock-button self))
     (om-invalidate-view self)
     (setf (allow-lock (object self)) mode)))

   
(defmethod remove-lock-button ((self omboxframe))
   "Remove the button subview from self."
   (om-remove-subviews (iconview self) (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil)
   ;;; (setf (value (object self)) nil)
   )

(defmethod show-fun-code ((self omboxframe))
  (edit-definition (reference (object self))))


(defmethod lib-ref-location ((self t)) nil)

(defmethod show-big-doc ((self symbol))
  (let ((refdir (and (symbolp self) (fboundp self)
                     (omgenfun-p (fdefinition self))
                     (lib-ref-location (fdefinition self)))))
    (om-show-reference-doc self refdir)))

(defmethod show-big-doc ((self omboxframe))
  (show-big-doc (reference (object self))))

(defmethod show-big-doc ((self t))
  (om-beep))



(defmethod show-con? ((self omboxframe)) (show-con? (om-view-container self)))



;--------------DRAG AND DROP

(defmethod make-drag-region ((self omboxframe) region x0 y0 view)
   (let* ((reg1 (om-new-region))
          (reg2 (om-new-region))
          (name (nameView self))
          (icon (iconView self))
          (x (- (x self) x0 (x view)))
          (y (- (y self) y0 (y view))))
     (om-set-rect-region reg1  (+ x (x icon))  (+ y  (y icon)) (+ x (x+w icon)) (+ y  (y+h icon)))
     (when name
       (om-set-rect-region reg2  (+ x (x name))  (+ y  (y name)) (+ x (x+w name)) (+ y  (y+h name))))
     
     (setf region (om-union-region reg1 reg2))

     (om-dispose-region reg1)
     (om-dispose-region reg2))
   region)


(defmethod get-pos-in-object ((self omboxframe) where)
  (om-add-points (om-view-position self) where))

;;; formerly drag-tracking-enter-view
(defmethod om-drag-enter-view ((view omboxframe)) nil)
;;; formerly drag-tracking-leave-view
(defmethod om-drag-leave-view ((view omboxframe)) nil)

  


;***********************************
; BOXFRAME SUBCLASSES
;***********************************

;;; Mixin class : 
(defclass ev-onceboxframe (boxframe) ()
   (:documentation "Simple frame for boxes that allow the lock button only in ev-once mode.#enddoc#
#seealso# (OMBoxCall lock-button) #seealso#"))

(defmethod add-lock-button ((self ev-onceboxframe) &optional (mode "&"))
  "Allow only lock button in mode ev-once."
  (declare (ignore icon))
  (when (allow-lock-button (object self))
    (setf (lock-button self)  (om-make-view 'lock-button
                                            :IconID (get-icon-lock mode)
                                            :size (om-make-point 10 10)
                                            :position (om-make-point (x (iconview self)) 8)
                                            :owner (iconview self)    ;;; self
                                            :action #'(lambda (item)
                                                        (declare (ignore item)) (om-beep))))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) "&")))

;-------------------------------------------------
;;; Mixin class : 
(defclass nonbuttonboxframe () ()
   (:documentation "Simple frame for boxes that do not allow a lock button.#enddoc#
#seealso# (OMBoxCall lock-button) #seealso#"))

(defmethod add-lock-button ((self nonbuttonboxframe) &optional (icon 167))
   "Self does not allow a lock button."
   (declare (ignore icon)) nil)

;-------------------------------------------------
(defclass boxTypeFrame (nonbuttonboxframe boxframe) ()
   (:documentation "Simple frame for OMBoxTypeCall boxes. #enddoc#
#seealso# (OMBoxTypeCall) #seealso#"))

(defmethod draw-before-box ((self boxTypeFrame)) nil)

(defmethod show-big-doc ((self boxTypeFrame)) nil)

(defmethod make-drag-region ((self boxTypeFrame) region x0 y0 view)
   (declare (ignore view))
   (let* ((icon (iconView self))
          (x (- (x self) x0))
          (y (- (y self) y0)))
     (om-set-rect-region region (+ x (x icon)) (+ y (y icon)) (+ x (x+w icon)) (+ y (y+h icon))))
   region)

;   Change only x-size.
(defmethod allow-new-size ((self boxTypeFrame) new-pos)
   (when (and (> (om-point-h new-pos) 10) (> (om-point-v new-pos) 0))
     (om-make-point (om-point-h new-pos) (max 20 (om-point-v new-pos)))))

(defmethod centre-icon ((self boxTypeFrame))
  (om-set-view-size (iconview self) (om-make-point (- (w self) 2) (- (h self) 11))))

(defmethod draw-after-box ((self boxtypeframe))
   (let ((deltay (if (zerop (numouts (object self))) 1 9)))
    (om-with-focused-view self
      (om-draw-rect 0 0 (- (w self) 1) (- (h self) 1 deltay) :pensize 1)
      )
    t))



(defmethod reinit-size ((box boxtypeframe))
   "Set the size of self to the initial size."
   (let ((goodsize (good-text-box-size (om-dialog-item-text (iconview box)) (om-get-font (iconview box)))))
     (setf (frame-size (object box)) goodsize)
     (box-draw-connections box nil)
     (omG-select (redraw-frame box))   
     ;(setf (frame-size (object box)) (om-view-size box))
     ))

;;; test : pour eviter d'ouvrir un editeur quand on clique juste au bord d'un undefined par ex.
;;; new : rajoute callnextmethod
(defmethod om-view-doubleclick-handler ((self boxTypeFrame) where)
   (declare (ignore where))
   (call-next-method))


;-------------------------------------------------
(defclass aliasBoxframe (boxframe) ()
   (:documentation "Simple frame for OMBoxAlias meta objects. #enddoc#
#seealso# (OMBoxAlias) #seealso#"))

(defmethod add-box-resize ((self aliasBoxframe)) nil)

(defmethod draw-before-box ((self aliasBoxframe)) t)



;Draw a special pattern for alias boxes.
(defmethod draw-after-box ((self aliasBoxframe))
   (let ((view (iconView self)))
     (om-with-focused-view self
       (om-with-dashline 
         (om-with-line-size 1.5
           (om-with-fg-color view *om-gray-color*
             (let ((r (om-make-rect (x view) (y view) (+ (x view) (w view)) (+ (y view) (h view)))))
               (om-draw-rect 2 2 (- (w self) 4) (- (h self) 4))
               )))))))

;--------------------------------------

(defclass input-instframe (input-funboxframe) ())

(defmethod initialize-instance :after ((self input-instframe) &key controls)
  (declare (ignore controls))
  (setf (iconID self) 145))

;--------------------------------------

(defclass instBoxframe (boxframe) ()
   (:documentation "Simple frame for OMBoxinstance boxes. #enddoc#
#seealso# (OMBoxinstance) #seealso#"))


(defmethod add-lock-button ((self instBoxframe) &optional icon )
   "Not lock button for OMBoxinstance."
   (declare (ignore icon)))

(defmethod initialize-instance  ((self instboxframe) &rest initargs)
  (call-next-method))
;;  (om-set-part-color (nameview self) :body *instboxframe-color*))

(defmethod draw-before-box ((self instBoxframe))
   (call-next-method)
   (om-draw-picture self (if (mypathname (reference (object self)))
                                                    *glass-pict-per* *glass-pict*)
                                    ;(om-make-point (- (x (iconview self)) 5) (y (iconview self))) 
                                    ;(om-make-point (+ (w (iconview self)) 5) (- (h self) 19))
                    :size (om-subtract-points (om-view-size self) (om-make-point 0 11))
                    )
   ;(om-draw-view-outline (nameview self))
   ;(om-with-fg-color self *instboxframe-color*
     ;(om-fill-rect (x (nameview self)) 0 (w (nameview self)) (- (h self) 18))
    ; (om-fill-rect 0 0 (w self) (- (h self) 9))
    ; )
   ;(unless (selected-p (nameview self))
   ;  (om-set-part-color (nameview self) :body *instboxframe-color*))
)

(defmethod draw-after-box ((self instBoxframe)) nil)
;  (om-with-fg-color self *om-dark-gray-color*
;     (om-draw-rect-outline 0 0 (- (w self) 1) (- (h self) 9) 
;                           (if (selected-p (iconview self)) 2 1))))

(defmethod change-name-box ((self instBoxframe))
   "If 'self is a global variable you can not change its name."
   (if (mypathname (reference (object  self)))
       (om-beep-msg "This is a persistant global variable. It can only be renamed from the Globals folder")
     (call-next-method)))

(defmethod omG-rename ((self instBoxframe) new-name)
    (call-next-method)
    (setf (name (reference (object self))) new-name)
    (setf (name (object self)) new-name)
    new-name)
#|  
(defmethod close-frame ((self instBoxframe))
  "Called when you delete an instboxframe will close listeditor."
  (when (attached-objs (object self)) 
    (mapcar #'om-close-window (attached-objs (object self))))
  (setf (frames (object self)) nil))
|#

(defmethod close-frame ((self instBoxframe))
  "Called when you delete an instboxframe will close listeditor."
  (let ((objs (attached-objs (object self))))
  (when (car objs)   
    (if (and (editorframe (reference (object self)))
             (atom (car objs)))
        (loop for i in (om-subviews (editorframe (reference (object self))))
              do (if (editorframe (object i))
                     (om-close-window (window (editorframe (object i))))    
                   (om-close-window (editorframe (reference (object self)))) ))
      (let ((obj (car (flat objs))))
        (if obj 
            (om-close-window (window obj))
          (om-close-window (editorframe (reference (object self))))))))
   (om-close-window (editorframe (reference (object self))))
   (setf (frames (object self)) nil)))

;----------------------------------------

(defclass slotboxFrame (boxframe) ()
   (:documentation "Simple frame for OMSlotsBox boxes. #enddoc#
#seealso# (OMSlotsBox) #seealso#"))

(defmethod remove-lock-button ((self slotboxFrame))
   "Do not set value to nil."
   (om-remove-subviews (iconview self) (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil))

;;Callable slotsframe:
(defclass slotsFrame (boxframe) ()
   (:documentation "Simple frame for OMOut boxes. #enddoc#
#seealso# (OMIN) #seealso#")
   (:default-initargs :view-font (list *signs-font* 18)))

;peut-etre pas besoin
(defmethod omg-remove-element ((self methodpanel) (box slotsFrame))
   "Remove only for the generic function definition."
   (if (equal (win-mod (editor self)) :abs)
     (call-next-method)
     (om-beep-msg "You can not modify the outputs of a function already defined")))

(defmethod omg-remove-element ((self patchPanel) (box slotsFrame))
   "When you remove 'box' from 'self' you must update all omboxpatch instances having 'self' as reference."
   (call-next-method))

(defmethod omG-rename ((self slotsFrame) new-name)
  "rename send and all receives"
  (setf (name (object self)) new-name) 
  (call-next-method))

(defmethod show-big-doc ((self slotsFrame)) nil)
(defmethod allow-rename ((self OMSlotsBox)) nil)

;----------------------------------------
;Comments draw + edition
;----------------------------------------
(defclass commentboxframe (boxframe) ()
   (:documentation "Simple frame for OMBoxcomment boxes. #enddoc#
#seealso# (OMBoxcomment) #seealso#"))

(defmethod commentframep ((self commentboxframe)) t)
(defmethod commentframep ((self t)) nil)

(defmethod comment-new-color ((self commentboxframe) newcolor)
   "Change de color font of 'self'."
   (om-set-fg-color (iconview self) newcolor)
   (setf (textcolor (object self)) newcolor)
   (om-invalidate-view self))

(defmethod comment-new-style ((self commentboxframe) newfont)
   "Change de style font of 'self'."
   (om-set-font (iconview self) newfont)
   ;(print (list "BEFORE" (textstyle (object self))))
   ;(print (list "AFTER" newlist))
   (setf (textstyle (object self)) newfont)
   (om-invalidate-view self))


  
(defmethod reinit-size ((box commentboxframe))
   "Set the size of self to the initial size."
   (let ((goodsize (get-good-size (reference (object box)) (textstyle (object box)))))
     (setf (frame-size (object box)) goodsize)
     (box-draw-connections box nil)
     (omG-select (redraw-frame box))   
     ;(setf (frame-size (object box)) (om-view-size box))
     ))


(defun comment-style (comments style) 
    (when comments
      (let* ((fontstyle (om-font-style (textstyle (object (car comments)))))
             (oldstyle (or (find style fontstyle) (find :plain fontstyle)))
             (newstyle (if (equal style oldstyle) :plain style)))
        (mapcar #'(lambda (co)
                    (let ((fo (textstyle (object co))))
                      (comment-new-style co (om-make-font (om-font-face fo) (om-font-size fo)
                                                          :family (om-font-family fo)
                                                          :style (if (find oldstyle (om-font-style fo))
                                                                     (substitute newstyle oldstyle (om-font-style fo))
                                                                   (cons newstyle (om-font-style fo)))))))
                comments)
        )))

;------------------the comment contents-----------------------
(defclass commentview (box-dialog-name) ())

(defmethod (setf selected-p) (selected-p (self commentview))
  (let ((color (when (om-view-container self) (textcolor (object (om-view-container self)))))
        (bgcolor nil)
        ;(if (om-view-container self) 
        ;    (get-bg-color (object (om-view-container self)))
        ;  *om-transparent-color*)
        )
     (if selected-p
       (progn
         (om-set-bg-color self (or color *om-select-color*))
         (om-set-fg-color self *om-white-color*))
       (progn
         (om-set-fg-color self (or color *om-black-color*))
         (om-set-bg-color self nil)))
     ))

(defmethod allow-new-size ((self commentboxframe) new-pos)
   (when (and (> (om-point-h new-pos) 10) (> (om-point-v new-pos) 5))
     new-pos))

(defmethod centre-icon ((self commentboxframe))
   (om-set-view-size (iconview self) (om-subtract-points (om-view-size self) (om-make-point 6 6))))

;--------EVENTS
(defmethod draw-before-box ((self commentboxframe)) t)

(defmethod draw-after-box ((self commentboxframe))
   nil)


(defmethod comments-locked ((self t)) nil)

(defmethod om-view-click-handler ((self commentview) where)
  (let ((panel (om-view-container (om-view-container self))))
    (if (comments-locked panel)
        (om-view-click-handler panel (om-convert-coordinates where self panel))
      (toggle-icon-active-mode (om-view-container self)))
    ))

(defmethod om-view-click-handler ((self commentboxframe) where)
  (let ((panel (om-view-container self)))
    (if (comments-locked panel)
        (om-view-click-handler panel (om-convert-coordinates where self panel))
      (call-next-method))
    ))

(defmethod om-handle-motion ((self commentboxframe) where)
  (let ((panel (om-view-container self)))
    (if (comments-locked panel)
        (om-handle-motion panel (om-convert-coordinates where self panel))
       (call-next-method))))

(defmethod om-drag-enter-view ((self commentview))
    (let ((panel (om-view-container (om-view-container self))))
      (unless (comments-locked panel)
        (call-next-method))))

(defmethod om-drag-receive ((self commentview) (dragged-view t) position &optional (effect nil))
  (let ((panel (om-view-container (om-view-container self))))
      (if (comments-locked panel)
          (om-drag-receive panel dragged-view position effect)
        (call-next-method))))

(defmethod show-big-doc ((self commentboxframe)) nil)


;--------EDITION
(defclass edit-comment (om-text-edit-view) 
   ((object :initform nil :initarg :object :accessor object)))

(defmethod om-dialog-item-action ((self commentview)) (call-next-method))


(defmethod om-view-doubleclick-handler ((self commentview) where)
  (declare (ignore where))
  (comment-box-edit (icon-finder self))
  self)

;;; container de text-view = panel
(defmethod comment-box-edit ((self commentboxframe))
  (let* ((panel (om-view-container self))
         (container (editor panel)))
    (when (text-view container)
      (exit-from-dialog (text-view container) (om-dialog-item-text (text-view container))))
    (unless (comments-locked panel)
      (setf (text-view container) (om-make-dialog-item 'edit-comment
                                                       (om-view-position self) 
                                                       (om-view-size self)
                                                       (om-dialog-item-text (iconview self))
                                                       :allow-returns t :scrollbars nil :focus t
                                                       :object self :container panel
                                                       :font *om-default-font2*))
      (om-remove-subviews self (iconview self))
      )))

(defmethod om-view-doubleclick-handler ((self commentboxframe) where) nil)
  
(defmethod om-view-key-handler ((self edit-comment) char) nil)

;;; marche pas.. il faut cliquer sur le panel pour sortir
(defmethod om-dialog-item-action ((self edit-comment)) 
 (exit-from-dialog self (om-dialog-item-text self)))

;;; remove text-view from the panel
(defmethod exit-from-dialog  ((self edit-comment) new-text-item)

   (let ((theeditor (editor (om-view-container self))))
     (if (equal new-text-item "")
       (om-set-dialog-item-text (iconView (object self)) "no comments")
       (om-set-dialog-item-text (iconView (object self)) new-text-item))
     (om-add-subviews (object self) (iconView (object self)))
     (setf (reference (object (object self))) (om-dialog-item-text (iconView (object self)))) 
     (setf (text-view theeditor) nil)
     (reinit-size (object self))
     (om-remove-subviews (panel theeditor) self)
     (om-invalidate-view (panel theeditor))
     (setf (selected-p (iconView (object self))) nil)
     ))


;-----------------------------------------
;Frame for editor boxes
;-----------------------------------------

(defclass boxEditorFrame (boxframe) ()
   (:documentation "Simple frame for OMBoxEditCall meta objects. #enddoc#
#seealso# (OMBoxEditCall) #seealso#"))

(defmethod boxeditorframe-p ((self boxeditorframe)) t)
(defmethod boxeditorframe-p ((self t)) nil)

(defmethod show-fun-code ((self boxEditorFrame))
  (edit-definition (class-name (reference (object self)))))

(defmethod special-ref-location ((self t)) nil)

(defmethod show-big-doc ((self boxEditorFrame))
  (let ((refdir (or (special-ref-location (value (object self)))
                    (lib-ref-location (reference (object self))))))
    (om-show-reference-doc (class-name (reference (object self))) refdir)))

(defmethod change-edit-mode ((box t))
   "Used only for editors and temporal boxes" nil)

(defmethod change-edit-mode ((self boxEditorFrame))
   "Showpict T Showpict nil Showpict T Showpict nil,..."
   (om-without-interrupts 
    (if (showpict (object self))
      (progn
        (setf (showpict (object self)) nil)
        (when (minipict (iconview self))
          (om-kill-picture (minipict (iconview self)))
          (setf (minipict (iconview self)) nil))
        (om-invalidate-view self))
      (progn
        (setf (showpict (object self)) t)
        (update-miniview (iconview self) (value (object self)))))))

(defmethod set-show-box-name ((box t))
   "Used only for editors and temporal boxes" nil)

(defmethod set-show-box-name ((self boxEditorFrame))
  (setf (show-name (object self)) (not (show-name (object self))))
  (om-invalidate-view self))
        
(defmethod move-frame-delta ((self boxEditorFrame) dir)
   "If option-key is down move the pict in the self's miniview and not 'self'."
   (if (om-option-key-p)
     (move-miniview (iconview self) dir) 
     (let ((pixnum (if (om-shift-key-p) 10 1)) new-position)
       (setf new-position
             (borne-position (case dir
               (0 (om-subtract-points (om-view-position self) (om-make-point 0 pixnum)))
               (1 (om-add-points (om-view-position self) (om-make-point 0 pixnum)))
               (2 (om-add-points (om-view-position self) (om-make-point pixnum 0)))
               (3 (om-subtract-points (om-view-position self) (om-make-point pixnum 0))))))
       ; pas la peine ?
       ;(mapc #'(lambda (conection)
       ;          (draw-connection conection nil)) (connections self))
       (om-set-view-position self new-position)
       (setf (frame-position (object self)) new-position))))


(defmethod change-boxframe-size ((view boxEditorFrame) new-size)
   (when (setf new-position (allow-new-size view new-size))
       (om-set-view-size view new-position)
       (make-move-after (om-view-container view) (list view))
       (if (showpict (object view))
         (update-miniview (iconview view) (value (object view))))
       (om-invalidate-view view)
       ))

(defmethod reinit-size ((self boxEditorFrame)) 
   (when (get-edit-param (object self) 'deltapict)
     (set-edit-param (object self) 'deltapict (om-make-point 0 0)))
   (setf (frame-size (object self)) (get-boxsize (object self)))
   (change-boxframe-size self (frame-size (object self)))
   (om-invalidate-view self))

(defmethod reinit-contents ((self boxEditorFrame)) 
  (setf (value (object self)) (get-super-default-value (type-of (value (object self)))))
  (update-if-editor (object self))
  (if (showpict (object self))
      (update-miniview (iconview self) (value (object self))))
  (when (get-name (value (object self)))
    (setf (name (object self)) (get-name (value (object self)))))
  (om-invalidate-view self))
   
(defmethod remove-lock-button ((self boxEditorFrame))
   "Do not set value to nil."
   (om-remove-subviews (iconview self) (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil))

(defmethod add-subview-extra ((self boxEditorFrame))
   (when (showpict (object self))
     (init-miniview (iconview self) (value (object self)))
     ))

(defmethod allow-new-size ((self boxEditorFrame) new-pos) 
   (om-make-point (max 30 (om-point-h new-pos )) (max 30 (om-point-v new-pos ))))

(defmethod centre-icon ((self boxEditorFrame))
   (om-set-view-size (iconview self) 
                  (om-subtract-points (om-view-size self) 
                                      (if (minieditor? (object self)) (om-make-point 4 21) (om-make-point 0 17)))))

(defmethod make-drag-region ((self boxEditorFrame) region x0 y0 view)
  (declare (ignore view))
  (let* ((x (- (x self) x0))
         (y (- (y self) y0)))
    (om-set-rect-region region x y  (+ x (w self)) (- (+ y (h self)) 16)))
   region)

(defmethod draw-before-box ((self boxEditorFrame))  nil)

(defmethod draw-after-box ((self boxEditorFrame)) 
   (when (view-of-patch (object self))
     (om-with-focused-view self
          (om-draw-hilite-rect 0 0 (w self) (h self))))
   )

(defmethod close-frame ((box boxEditorFrame))
   "If miniview show a picture we must kill it."
   (when (EditorFrame (object box))
     (om-close-window (window (EditorFrame (object box)))))
   (if (minieditor? (object box)) 
     (close-editorFrame (iconview box))
     (when (minipict (iconview box)) 
       (om-kill-picture (minipict (iconview box)))
       (setf (minipict (iconview box)) nil)))
   (setf (frames (object box)) nil))

(defmethod OMGMoveObject ((self boxEditorFrame) new-position)
   "If shift-key is down when drag self it do not move but it create and slot box."
   (if (or (om-shift-key-p) (shift-key-p *OM-drag&drop-handler*))
     (let* ((target (om-view-container self)) newobj)
       (when target
         (setf newobj (omNG-make-new-boxcall-slots (reference (object self))  (borne-position new-position) (mk-unique-name target "slot")))
         (omG-add-element target (make-frame-from-callobj newobj))))
     (call-next-method)))


(defmethod omG-select ((self boxEditorFrame))
   "Set the frame SELF and the object pointed for frame in selected mode"
   (when (not (active-mode self)) 
     (setf (active-mode self) t)
     (setf (selected-p (iconView self)) t)
     (if (showpict (object self))
       (draw-only-select (iconView self))
       (om-invalidate-view self))
     ;(om-invalidate-view self)
     ))

(defmethod omG-unselect ((self boxEditorFrame))
   "Set the frame SELF and the object pointed for frame in unselected mode"
   (when (active-mode self)
     (setf (active-mode self) nil)
     (setf (selected-p (iconView self)) nil)
     (if (showpict (object self))
       (draw-only-select (iconView self))
       (om-invalidate-view self))
     ;(om-invalidate-view self)
     ))

(defmethod change-view-of-patch ((self boxEditorFrame))
   (if (view-of-patch (object self))
     (progn
       (setf (view-of-patch (object self)) nil)
       (om-invalidate-view self))
     (let ((oldview (find-the-view-of-patch (om-view-container self))))
       (when oldview
         (setf (view-of-patch (object oldview)) nil)
         (om-invalidate-view oldview))
       (setf (view-of-patch (object self)) t)
       (om-invalidate-view self))))
     

(defmethod 1st-not-con-input ((self boxeditorframe))
  (let* ((ins (inputframes self))
         (objs (mapcar 'object ins))
         (con? (mapcar 'connected? objs))
         (lst (mapcar 'list con? ins)))
    (loop for i in lst
          when (not (car i))
            return (second i))))

;----------------------------------------

(defclass patchboxFrame (boxframe) ()
   (:documentation "Simple frame for OMBoxpatch boxes. #enddoc#
#seealso# (OMBoxpatch) #seealso#"))

(defmethod update-doc ((box patchboxFrame))
   (update-from-reference (object box)))

(defmethod change-name-box ((self patchboxFrame))
   "If 'self is a persistant patch you can not change its name."
   (if (mypathname (reference (object self)))
       (om-beep-msg "!! Rename abstractions from workspace folders.")
     (call-next-method)))

(defmethod show-big-doc ((self patchboxFrame))
  (om-beep-msg "Patches have no referenced documentation. See Info Window for a possible documentation about this box."))

;-------------------

(defclass patchboxabsFrame (patchboxFrame) ()
   (:documentation "Simple frame for OMBoxAbspatch boxes (red patches). #enddoc#
#seealso# (OMBoxAbspatch) #seealso#"))

(defmethod omG-rename :after ((self patchboxabsFrame) new-name)
  (declare (ignore new-name))
  (set-patch-box-name (object self)))

(defmethod set-patch-box-name (box &optional name)
  (let ((new-name (or name (frame-name box)))
        (thepatch (reference box)))
    (when (EditorFrame thepatch)
      (omG-rename (EditorFrame thepatch) new-name))
    (setf ;; new . OK ?
     (name box) new-name
     (name thepatch) new-name)))


;-------------- AUTO INPUT DEFAULT SETTING

(defmethod get-patch-of-frame ((self omboxframe)) (reference (object self)))

(defmethod om-inputs-to-patch-defaults ((self omboxframe))
  "evaluate patch inputs and set default values"
  (print 'working)
  (let ((args (eval-box-inputs (object self))))
    (loop for input in (get-patch-inputs (get-patch-of-frame self))
          for arg in args
          do (setf (defval input)
                   (if (listp arg)
                      `(quote ,arg)
                     arg)))))

;----------------------------------------

(defclass maquetteframe (boxframe) ()
   (:documentation "Simple frame for OMBoxmaquette boxes. #enddoc#
#seealso# (OMBoxmaquette) #seealso#"))

(defmethod allowed-lock-modes ((self omboxmaquette)) '("x" "&" "o"))

(defmethod add-lock-button ((self maquetteframe) &optional (mode "x"))
  "Not lambda mode for maquette boxes."
  (when (and (allow-lock-button (object self)) 
             (find mode (allowed-lock-modes (object self)) :test 'string-equal))
    (setf (lock-button self)  (om-make-view 'lock-button
                                            :IconID (get-icon-lock mode)
                                            :size (om-make-point 10 10)
                                            :position (om-make-point 0 0) ;;;(om-make-point (x (iconview self)) 8)
                                            :owner (iconview self)   ;;; self
                                            :action #'(lambda (item)
                                                        (let* ((modes (allowed-lock-modes (object self)))
                                                               (mpos (position (mode item) modes :test 'string-equal))
                                                               (newmode (nth (mod (1+ mpos) (length modes)) modes)))
                                                          (setf (mode item) newmode
                                                                (iconID item) (get-icon-lock newmode))
                                                          (setf (allow-lock (object self)) newmode)
                                                          ;#+(or linux win32)(om-draw-contents item)
                                                          ))))
    (om-invalidate-view self)
    (setf (allow-lock (object self)) mode)))

(defclass maquetteabsframe (maquetteframe) ()
              (:documentation "Simple frame for OMBoxAbsmaq boxes (red maquettes). #enddoc#
#seealso# (OMBoxAbsmaq) #seealso#"))

(defmethod change-name-box ((self maquetteframe))
   "If 'self is a persistant maquette you can not change its name."
   (if (mypathname (reference (object self)))
       (om-beep-msg "This is a persistant maquette. It can only be renamed from workspace folders.")
     (call-next-method)))

(defmethod omG-rename :after ((self maquetteabsframe) new-name)
  (declare (ignore new-name))
  (set-patch-box-name (object self)))

;----------------------------------------

(defmethod make-graph-instance ((self t) container posi box)
   (cond 
    ((omclass-p (class-of (class-of self)))
     (let ((theclass (class-of self)))
       (let* ((instance (omNG-make-new-instance (clone self) (mk-unique-name container (name theclass))))
              (obj (omNG-make-new-boxcall instance posi (name instance)))
              (frame (make-frame-from-callobj obj)))
          ;; attention l'instance n'est pas forcement du meme type que celle de la boite d'ou elle sort...
          (if  (and  (edition-params (object box))
                    (equal (type-of (value (object box))) (type-of (instance instance))))
              (setf (edition-params instance) (eval (copy-value-params self (object box))))
            (setf (edition-params instance) (default-edition-params (instance instance))))
         (omG-add-element container frame))))
    ((listp self)
     (let* ((instance (omNG-make-new-instance (clone self) (mk-unique-name container (if (null self) "nil" "list"))))
            (obj (omNG-make-new-boxcall instance posi (name instance)))
            (frame (make-frame-from-callobj obj)))
       (omG-add-element container frame)))))


;=================================================================
;CLASSES BOXES
;=================================================================



(defclass outflecheclass (outfleche) ())

(defmethod initialize-instance :after ((self outflecheclass) &key controls)
  (declare (ignore controls))
  (setf (iconID self) 169))



(defmethod connect-box ((self outflecheclass) (ctrl icon-view))
  (let ((frametarget (om-view-container ctrl)))
    (when (equal (class-name (class-of frametarget)) 'classboxframe)
      (let* ((framesource (om-view-container self))
             (classsource (object framesource))
             (classtarget (object frametarget)))
        (if (protected-p (find-class (get-reference classtarget)))
          (om-beep-msg (string+ "The class " (string (get-reference classtarget)) " is protected"))
          (if (subtypep (get-reference classsource) (get-reference classtarget))
              (om-beep-msg "Error: cyclic inheritance.")
            (progn
              (connect-class classtarget classsource)
              (connect-define-class classtarget)
              (redraw-frame frametarget)
              ;(print (om-view-container frametarget))
              ;(om-invalidate-view (print (om-view-container frametarget))
              )
            ))))))

;-------------------------------------

(defclass classboxFrame (boxframe) ()
   (:documentation "Simple frame for OMBoxclass meta objects. #enddoc#
#seealso# (OMBoxclass) #seealso#"))


(defmethod add-lock-button ((self classboxFrame) &optional icon)
   "Class boxes do not allow buttons."
   (declare (ignore icon)))

(defmethod add-box-resize ((self classboxFrame)) nil)

(defmethod OMGMoveObject ((self classboxFrame) new-position)
   "When you move a class box you must save its position."
  (call-next-method)
  (set-icon-pos (find-class (reference (object self))) (borne-position new-position)))

(defmethod redraw-frame ((self classboxFrame))
  (let ((thescroll (om-view-container self))
        (object (object self)) frame)
    (om-remove-subviews thescroll self)
    (setf frame (make-frame-from-callobj object))
    (om-add-subviews thescroll frame)
    (update-graphic-connections frame (get-class+alias (object thescroll)))))

(defmethod change-name-box ((self classboxFrame))
   "A Class does not change its name."
   nil)

(defmethod omg-remove-element ((self classTreePanel) (frame classboxFrame))
   "Class can not be erased from the class hierarchical tree."
   ;;; --> why not ?   cf. hierarchiecontainer.lisp
   (call-next-method))

;------------------------------------------------------------------
;------------------------------------------------------------------
(defvar *function-without-name* (list 'om+ 'om* 'om- 'om/ 'om^ 'omand 'omor 'om-e 'om-log 'om< 'om> 'om<= 'om>= 'om= 'om/=)
   "This list contains the name of generic functions that do not show their name in the box.")




;=================================================================
; ENCAPSULATION
;=================================================================

(defmethod is-inout-p (box)
  (member (type-of box) '(omin omout)))

(defmethod get-inactives ((self relationPanel))
  (remove-if #'(lambda (f)
                 (or (not (subtypep (type-of f) 'omboxframe))
                     (active-mode f)))
             (om-subviews self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; box re-positioning 

(defmethod normalize-positions ((self ompatch))  
  (let ((minx (loop for box in (boxes self)
                    minimize (om-point-h (frame-position box))))
        (miny (loop for box in (boxes self)
                    minimize (om-point-v (frame-position box)))))

    (loop for box in (boxes self)
          do
          (setf (frame-position box)
                (om-make-point (+ (- (om-point-h (frame-position box)) minx) 50)
                               (+ (- (om-point-v (frame-position box)) miny) 50))))))


(defmethod center-positions-around ((self ompatch) boxes-1 position)
  (let ((boxes (remove-if #'is-inout-p boxes-1)))
    (let ((average-pos (loop for box in boxes
                             sum (om-point-h (frame-position box)) into hsum
                             sum (om-point-v (frame-position box)) into vsum
                             finally (return (om-make-point (floor (/ hsum (length boxes)))
                                                           (floor (/ vsum (length boxes))))))))    ;;; should reuse average-position
      (loop for box in boxes
            do (setf (frame-position box)
                     (om-add-points (om-subtract-points (frame-position box)
                                                        average-pos)
                                    position))))))
   
(defun average-position (frames)
  (om-make-point (average (mapcar #'(lambda (frame)
                                      (om-point-h (om-view-position frame)))
                                              frames)
                                      nil)
                 (average (mapcar #'(lambda (frame)
                                      (om-point-v (om-view-position frame)))
                                  frames)
                          nil)))  
                             

(defmethod shrink-patch-window ((self ompatch))
  (setf (w-size self)
        (om-make-point (+ (loop for box in (boxes self)
                                maximize (om-point-h (frame-position box))) 
                          125)
                       (+ (loop for box in (boxes self)
                                maximize (om-point-v (frame-position box))) 
                          125))))


(defun mk-bridge-list (sources destinations)
  ;;; return a list of existing connections from sources to destinations, 
  ;;; each connection is as the arglist for omng-connect, except that source and target are positions within lists of boxes 
  ;;;   (source-pos numout target-pos numin lines color)

  (loop for dest in destinations
        for d from 0
        append (loop for input in (inputs dest)
                     for i from 0
                     if (and (connected? input)
                             (member (first (connected? input)) sources))
                     collect (list (position (first (connected? input)) sources)
                                   (second (connected? input))
                                   d
                                   i
                                   (om-save-point-list (third (connected? input))) 
                                   (fourth (connected? input))))))



(defmethod make-incoming-threshold-connections ((patch OMPatchAbs) bridges outside-boxes inside-boxes)
  (loop for conn in bridges
        for source = (nth (first conn) outside-boxes)

        with index = 1
        with entries  ;; keep track of new input boxes, with entries of the form (input-obj inactives-ordinal output-ordinal input-ordinal)

        with input-ord

        for input = (or (let ((match (find conn 
                                           entries 
                                           :test #'(lambda (c entry)
                                                     (and (= (first c) (second entry))
                                                          (= (second c) (third entry)))))))
                          (when match
                            (setf input-ord (fourth match))
                            (first match)))
                                  

                        (let ((newin (make-new-patch-input (string+ "input" 
                                                                    (if (= index 1)
                                                                        ""
                                                                      (string+ " " (prin1-to-string index)))) 
                                                           (1- index)
                                                           (om-make-point (+ (om-point-h (om-view-position source)) (* (second conn) 35))
                                                                          (om-point-v (om-view-position source))))))
                          (omng-add-element patch newin)
                          (push (list newin (first conn) (second conn) index) entries)
                          (setf input-ord (1- index))
                          (incf index)
                          newin))

        do 
            
        ;make connection inside patch
        (omng-connect input 0
                      (nth (third conn) inside-boxes) (fourth conn)
                      nil)

        ;make connection outside patch
        (omng-connect (object source) (second conn) 
                      (first (attached-objs patch)) input-ord 
                      nil)

       
        finally 
        (loop for item in (attached-objs patch) do
              (update-from-reference item))
        ))           



(defmethod make-outgoing-threshold-connections ((patch OMPatchAbs) bridges outside-boxes inside-boxes)
  (loop for conn in bridges
        for destination = (nth (third conn) outside-boxes)
        for index from 1
        for newout = (make-new-output (string+ "output" 
                                               (if (= index 1)
                                                   ""
                                                 (string+ " " (prin1-to-string index)))) 
                                      (1- index)
                                      (om-make-point (+ (om-point-h (om-view-position destination)) (* (fourth conn) 35))
                                                     (om-point-v (om-view-position destination))))

        do
        (omng-add-element patch newout)

        (omng-connect (nth (first conn) inside-boxes) (second conn)
                      newout 0
                      nil)

        (omng-connect (first (attached-objs patch)) (1- index) 
                      (object destination) (fourth conn) 
                      nil)

        finally 
        (loop for item in (attached-objs patch) do
              (update-from-reference item))
        ))


;;;;;;;;;;;;;;;;;;;;;;
;;; ENCAPSULATE

(defmethod om-encapsulate ((self patchPanel) actives)
  (modify-patch self)

  (let* ((inactives (get-inactives self))

         ;make new patch
         (patchabs (make-instance 'OMPatchAbs :name (mk-unique-name self "mypatch") :icon 210))
         (pboxcall (omNG-make-new-boxcall patchabs
                                          (average-position actives)
                                          (mk-unique-name self "mypatch")))
         (pframe (make-frame-from-callobj pboxcall)))

    ;insert new patch in current window
    (omG-add-element self pframe)
    

    (let ((copies (loop for frame in actives
                        collect (eval (omng-copy (object frame)))))

          (clist (mk-connection-list (mapcar 'object actives)))

          (coming-in (sort (mk-bridge-list (mapcar 'object inactives) (mapcar 'object actives))
                           #'<
                           :key #'(lambda (bridge)
                                    (let ((source-frame (nth (car bridge) inactives)))
                                      (om-point-h (om-view-position source-frame))))))

          (going-out (sort (mk-bridge-list (mapcar 'object actives) (mapcar 'object inactives))
                           #'<
                           :key #'(lambda (bridge)
                                    (let ((dest-frame (nth (third bridge) inactives)))
                                      (om-point-h (om-view-position dest-frame)))))))

      ;add the copies to the new patch
      (loop for copy in copies
            do (omng-add-element patchabs copy))

      ;remake the connections from the original boxes
      (loop for connect in clist
            do
            (omng-connect  (nth (nth 0 connect) copies)
                           (nth 1 connect)
                           (nth (nth 2 connect) copies)
                           (nth 3 connect)
                           (nth 4 connect)
                           (nth 5 connect)))
       
      ;MAKE THRESHOLD CONNECTIONS
      (make-incoming-threshold-connections patchabs coming-in inactives copies)
      (make-outgoing-threshold-connections patchabs going-out inactives copies)

      (normalize-positions patchabs)
      (shrink-patch-window patchabs)

      ;remove the original boxes
      (loop for frame in actives
            do
            (omg-remove-element self frame))
           
      (omg-select (car (frames (car (attached-objs patchabs)))))

      )))


;;;;;;;;;;;;;;;;;;;;;;;
;;;; UN-ENCAPSULATE

(defmethod om-unencapsulate ((self patchPanel) (active list))
  (mapc #'(lambda (a) (om-unencapsulate self a))
        active))

(defmethod om-unencapsulate ((self patchPaneL) (active t)) nil)

(defmethod om-unencapsulate ((self patchPanel) (active patchboxframe))
  (modify-patch self)

  (load-patch (reference (object active)))   ;; make sure the subpatch is loaded
  
  (let* ((sub-patch (reference (object active)))
         (parent-patch (object self))

         ;;; make copies of boxes inside the subpatch
         (copies (loop for box in (boxes sub-patch)
                       collect (eval (omng-copy box))))

         inlet-alist
         outlet-alist)

    ; build list of inlet and outlet list positions in the subpatch
    (loop for box in (boxes sub-patch)
          for k from 0
          if (equalp (type-of box) 'omin) do (push (list k (indice box)) inlet-alist)
          if (equalp (type-of box) 'omout) do (push (list k (indice box)) outlet-alist))

    ; add the copies of all boxes except inlets and outlets into the parent patch, preserving the order (necessary?)
    (loop for box in (reverse copies)  
          unless (is-inout-p box)
          do (omg-add-element self (make-frame-from-callobj box)))
  
    ; make internal (non threshold) connections
    (remk-connections copies (mk-connection-list (boxes sub-patch)))

    ; make threshold connections
    (let ((parent-connections (mk-connection-list (boxes parent-patch)))
          (sub-patch-position (position (object active) (boxes parent-patch))))

      (loop for conn in (mk-connection-list (boxes sub-patch))
            for inlet-number = (assoc (first conn) inlet-alist)    ; source-position
            for outlet-number = (assoc (third conn) outlet-alist)   ; target-position
          
            do 
            (when inlet-number
              (let ((outer-connection (connected? (nth (second inlet-number) 
                                                       (inputs (object active))))))
                (when outer-connection
                  (omNG-connect (first outer-connection)
                                (second outer-connection)
                                (nth (third conn) copies)
                                (fourth conn)
                                (fifth conn)
                                (sixth conn)))))

            (when outlet-number
              (loop for pc in parent-connections
                    do
                    (when (and (= (first pc) sub-patch-position)
                               (= (second pc) (second outlet-number)))
                      (omNG-connect (nth (first conn) copies)
                                    (second conn)
                                    (nth (third pc) (boxes parent-patch))
                                    (fourth pc)
                                    (fifth pc)
                                    (sixth pc)))))))                       

    ;reposition new boxes
    (center-positions-around (object self) copies (frame-position (object active)))

    ;remove the subpatcher
    (omg-remove-element self active)


    ;draw the new connections
    (mapc #'(lambda (copy)
              (when (frames copy)
                (redraw-frame (first (frames copy)))))
          copies)

    ;select new boxes
    (mapc #'(lambda (c) (let ((frame (car (frames c))))
                          (when frame
                            (omg-select frame))))
          copies)
))






