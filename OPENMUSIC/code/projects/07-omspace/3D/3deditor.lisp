;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
;Authors: J. Bresson


(in-package :om)


;;; 3OpenGL

(defvar *OM-GL-DEFAULT-LINEWIDTH* 1.0)
(defvar *OM-GL-DEFAULT-COLOR* (list 0.0 0.0 0.0 1.0))
(defparameter *OM-DEFAULT-ROOM-COLOR* (list 0.9 0.9 0.9 1.0))

;editors default parameters
(defvar *OM-DEFAULT-ROOM-SIZE* 2)
(defvar *OM-DEFAULT-SHOW-ROOM* 1)
(defvar *OM-DEFAULT-SHOW-AXES* 1)


(defmethod restore-om-gl-colors-and-attributes ()
  (opengl:gl-color4-f (nth 0 *OM-GL-DEFAULT-COLOR*) (nth 1 *OM-GL-DEFAULT-COLOR*) (nth 2 *OM-GL-DEFAULT-COLOR*) (nth 3 *OM-GL-DEFAULT-COLOR*))
  (opengl:gl-line-width *OM-GL-DEFAULT-LINEWIDTH*)
  (opengl:gl-enable opengl:*gl-point-size*)
  (opengl:gl-enable opengl:*gl-point-smooth*)
  )

(defmethod om-color-to-single-float-list (color)
  (list (coerce (om-color-r color) 'single-float)
        (coerce (om-color-g color) 'single-float)
        (coerce (om-color-b color) 'single-float)
        (coerce 1.0 'single-float)))

(defclass 3D-cube (om-3D-object) 
  ((center :accessor center :initarg :center :initform nil)
   (faces :accessor faces :initarg :faces :initform t)
   (size :accessor size :initarg :size :initform nil)))

(defmethod initialize-instance :after ((self 3D-cube) &rest initargs)
  (when (and (center self) (size self))
    (let ((hs (/ (size self) 2))
          (x (car (center self)))
          (y (cadr (center self)))
          (z (caddr (center self))))
      (om-set-3Dobj-points self 
            (list (list (- x hs) (- y hs) (- z hs))
                  (list (+ x hs) (- y hs) (- z hs))
                  (list (+ x hs) (- y hs) (+ z hs))
                  (list (- x hs) (- y hs) (+ z hs))
                  (list (- x hs) (+ y hs) (- z hs))
                  (list (+ x hs) (+ y hs) (- z hs))
                  (list (+ x hs) (+ y hs) (+ z hs))
                  (list (- x hs) (+ y hs) (+ z hs)))))))
                  

(defmethod om-draw-contents ((self 3D-cube))
  (let* ((vertices (om-get-gl-points self)))
    (if (om-3Dobj-color self)
        (opengl:gl-color4-f (car (om-3Dobj-color self)) (cadr (om-3Dobj-color self)) (caddr (om-3Dobj-color self)) 1.0))

    (if (faces self)
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINE-LOOP*))
    (opengl:gl-normal3-i 0 1 0) 
    (opengl:gl-vertex4-dv (aref vertices 0))
    (opengl:gl-vertex4-dv (aref vertices 1))
    (opengl:gl-vertex4-dv (aref vertices 2))
    (opengl:gl-vertex4-dv (aref vertices 3))
    (opengl:gl-end)
    
    (if (faces self)
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINE-LOOP*))
    
    (opengl:gl-vertex4-dv (aref vertices 7))
    (opengl:gl-vertex4-dv (aref vertices 6))
    (opengl:gl-vertex4-dv (aref vertices 5))
    (opengl:gl-vertex4-dv (aref vertices 4))

  (opengl:gl-end)
    
    (if (faces self)
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINES*))
    (opengl:gl-vertex4-dv (aref vertices 3))
    (opengl:gl-vertex4-dv (aref vertices 7))
    (opengl:gl-vertex4-dv (aref vertices 4))
    (opengl:gl-vertex4-dv (aref vertices 0))

    (opengl:gl-vertex4-dv (aref vertices 5))
    (opengl:gl-vertex4-dv (aref vertices 1))
    (opengl:gl-vertex4-dv (aref vertices 0))
    (opengl:gl-vertex4-dv (aref vertices 4))
    
    (opengl:gl-vertex4-dv (aref vertices 6))
    (opengl:gl-vertex4-dv (aref vertices 2))
    (opengl:gl-vertex4-dv (aref vertices 1))
    (opengl:gl-vertex4-dv (aref vertices 5))

    (opengl:gl-vertex4-dv (aref vertices 2))
    (opengl:gl-vertex4-dv (aref vertices 6))
    (opengl:gl-vertex4-dv (aref vertices 7))
    (opengl:gl-vertex4-dv (aref vertices 3))

    (opengl:gl-end)
    ))


(defclass 3D-curve (om-3D-object) 
  ((selected-points :accessor selected-points :initform nil)
   (lines :accessor lines :initarg :lines :initform t)
   (line-width :accessor line-width :initarg :line-width :initform *OM-GL-DEFAULT-LINEWIDTH*)))
   
(defmethod om-draw-contents ((self 3d-curve))
  (let* ((vertices (om-get-gl-points self))
         (size (- (length vertices) 1))
         (selection (selected-points self))
         (vertices-colors (get-vertices-colors self))
         (sel-rgb (om-color-to-single-float-list *om-dark-red-color*)))
    (opengl:gl-enable opengl:*gl-light0*)
    (opengl:gl-line-width (float (line-width self)))

    ;draw the lines first
    (when (and (lines self) (> size 1))
      (opengl:gl-begin opengl:*GL-LINE-STRIP*)
      (loop for i from 0 to size do 
            (let ((rgb (or (nth i vertices-colors) (om-color-to-single-float-list *om-light-gray-color*))))
              (opengl:gl-color4-f (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 0.8)
              (opengl:gl-vertex4-dv (aref vertices i))))
      (opengl:gl-end))
  
    ;draw the points an the selection (as bigger opaque points)
    (loop for i from 0 to size do
          (let* ((rgb (or (nth i vertices-colors) (om-color-to-single-float-list *om-light-gray-color*)))
                 (selected (or (equal '(t) selection) (find i selection)))
                 (alpha (if selected 1.0 0.7))
                 (point (nth i (om-3dobj-points self)))
                 (x (float (car point)))
                 (y (float (cadr point)))
                 (z (float (caddr point))))
            (if selected
                (opengl:gl-color4-f (nth 0 sel-rgb) (nth 1 sel-rgb) (nth 2 sel-rgb) alpha)
              (opengl:gl-color4-f (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) alpha))
            (opengl:gl-point-size (* 3.0 (line-width self)))
            (opengl:gl-begin opengl:*gl-points*)
            (opengl:gl-vertex3-f x y z)
            (opengl:gl-end))))
    ;restore gl params
    (restore-om-gl-colors-and-attributes)
    )


(defun default-color-vertices (obj)
  (make-list (length (om-3dobj-points obj)) :initial-element (oa::color obj)))

;jgarcia
(defmethod get-vertices-colors ((self 3d-curve))
  "create a vector of colors for a 3D-curve depending on the mode selected"
  (default-color-vertices self))

(defun draw-point-cube (point size faces)
  (let* ((hs (/ size 2))
         (x (car point))
         (y (cadr point))
         (z (caddr point))
         (cube-points 
          (list (list (- x hs) (- y hs) (- z hs))
                (list (+ x hs) (- y hs) (- z hs))
                (list (+ x hs) (- y hs) (+ z hs))
                (list (- x hs) (- y hs) (+ z hs))
                (list (- x hs) (+ y hs) (- z hs))
                (list (+ x hs) (+ y hs) (- z hs))
                (list (+ x hs) (+ y hs) (+ z hs))
                (list (- x hs) (+ y hs) (+ z hs)))))
    (if faces
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINE-LOOP*))
    (opengl:gl-normal3-i 0 1 0) 
    (opengl:gl-vertex3-f (car (nth 0 cube-points)) (cadr (nth 0 cube-points)) (caddr (nth 0 cube-points))) 
    (opengl:gl-vertex3-f (car (nth 1 cube-points)) (cadr (nth 1 cube-points)) (caddr (nth 1 cube-points)))
    (opengl:gl-vertex3-f (car (nth 2 cube-points)) (cadr (nth 2 cube-points)) (caddr (nth 2 cube-points)))
    (opengl:gl-vertex3-f (car (nth 3 cube-points)) (cadr (nth 3 cube-points)) (caddr (nth 3 cube-points)))
    (opengl:gl-end)
    
    (if faces
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINE-LOOP*))
    
    (opengl:gl-vertex3-f (car (nth 7 cube-points)) (cadr (nth 7 cube-points)) (caddr (nth 7 cube-points)))
    (opengl:gl-vertex3-f (car (nth 6 cube-points)) (cadr (nth 6 cube-points)) (caddr (nth 6 cube-points)))
    (opengl:gl-vertex3-f (car (nth 5 cube-points)) (cadr (nth 5 cube-points)) (caddr (nth 5 cube-points)))
    (opengl:gl-vertex3-f (car (nth 4 cube-points)) (cadr (nth 4 cube-points)) (caddr (nth 4 cube-points)))

  (opengl:gl-end)
    
    (if faces
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINES*))
    
    (opengl:gl-vertex3-f (car (nth 3 cube-points)) (cadr (nth 3 cube-points)) (caddr (nth 3 cube-points)))
    (opengl:gl-vertex3-f (car (nth 7 cube-points)) (cadr (nth 7 cube-points)) (caddr (nth 7 cube-points)))
    (opengl:gl-vertex3-f (car (nth 4 cube-points)) (cadr (nth 4 cube-points)) (caddr (nth 4 cube-points)))
    (opengl:gl-vertex3-f (car (nth 0 cube-points)) (cadr (nth 0 cube-points)) (caddr (nth 0 cube-points)))

    (opengl:gl-vertex3-f (car (nth 5 cube-points)) (cadr (nth 5 cube-points)) (caddr (nth 5 cube-points)))
    (opengl:gl-vertex3-f (car (nth 1 cube-points)) (cadr (nth 1 cube-points)) (caddr (nth 1 cube-points)))
    (opengl:gl-vertex3-f (car (nth 0 cube-points)) (cadr (nth 0 cube-points)) (caddr (nth 0 cube-points)))
    (opengl:gl-vertex3-f (car (nth 4 cube-points)) (cadr (nth 4 cube-points)) (caddr (nth 4 cube-points)))

    (opengl:gl-vertex3-f (car (nth 6 cube-points)) (cadr (nth 6 cube-points)) (caddr (nth 6 cube-points)))
    (opengl:gl-vertex3-f (car (nth 2 cube-points)) (cadr (nth 2 cube-points)) (caddr (nth 2 cube-points)))
    (opengl:gl-vertex3-f (car (nth 1 cube-points)) (cadr (nth 1 cube-points)) (caddr (nth 1 cube-points)))
    (opengl:gl-vertex3-f (car (nth 5 cube-points)) (cadr (nth 5 cube-points)) (caddr (nth 5 cube-points)))

    (opengl:gl-vertex3-f (car (nth 2 cube-points)) (cadr (nth 2 cube-points)) (caddr (nth 2 cube-points)))
    (opengl:gl-vertex3-f (car (nth 6 cube-points)) (cadr (nth 6 cube-points)) (caddr (nth 6 cube-points)))
    (opengl:gl-vertex3-f (car (nth 7 cube-points)) (cadr (nth 7 cube-points)) (caddr (nth 7 cube-points)))
    (opengl:gl-vertex3-f (car (nth 3 cube-points)) (cadr (nth 3 cube-points)) (caddr (nth 3 cube-points)))

    (opengl:gl-end)))

;;;=============================
;;; 3DC editor 
;;;=============================

(defclass 3DEditor (EditorView)
  ((xyp :accessor xyp :initform nil)
   (xzp :accessor xzp :initform nil)
   (yzp :accessor yzp :initform nil)
   (3Dp :accessor 3Dp :initform nil)
   (ctrlp :accessor ctrlp :initform nil)
   (selected-component :accessor selected-component :initarg :selected-component :initform 0)
   (sc-label :accessor sc-label :initarg :sc-label :initform nil)
   (display-mode :accessor display-mode :initarg :display-mode :initform 0)
   (multibpf? :accessor multibpf? :initarg :multibpf? :initform nil)
   (show-back-p :accessor show-back-p :initarg :show-back-p :initform t)
   ;(show-axes :accessor show-axes :initarg :show-axes :initform t)
   (lines-p :accessor lines-p :initarg :lines-p :initform t)
   (tmpview-objs :accessor tmpview-objs :initarg :tmpview-objs :initform nil)
   (focus :accessor focus :initform nil)
   (mode :accessor mode :initform :normal)
 )
  (:default-initargs :drawing-mode :quality))

;not used anymore
(defmethod scaled-3D-points  ((self 3DC) xmi xma ymi yma zmi zma maxrange)
  (mat-trans (list (om-scale (om- (x-points self) (round (+ xmi xma) 2)) 1.0 -1.0 (- (/ maxrange 2)) (/ maxrange 2))
                   (om-scale (om- (y-points self) (round (+ ymi yma) 2)) 1.0 -1.0 (- (/ maxrange 2)) (/ maxrange 2))
                   (om-scale (om- (z-points self) (round (+ zmi zma) 2)) -1.0 1.0 (- (/ maxrange 2)) (/ maxrange 2)))))

(defmethod get-current-object ((self 3Deditor))
  (if (multibpf? self) 
      (nth (selected-component self) (bpf-list (object self)))
    (object self)))


(defmethod editor-3Dobj ((self 3Deditor))
  (when (3Dp self) 
    (if (and (multibpf? self) (show-back-p self))
        (nth (selected-component self) (om-get-3D-objects (om-get-gl-object (3Dp self))))
      (om-get-gl-object (3Dp self)))))

;trywithou transformation
(defmethod format-3D-points  ((self 3DC))
  (mat-trans (list (x-points self) (y-points self) (z-points self))))
  
(defun 3Dobj-from-points (points drawmode color line-width)
  (let ((clist (when color (list (float (om-color-r color)) 
                                 (float (om-color-g color)) 
                                 (float (om-color-b color))))))  
    (make-instance '3D-curve :points points :color clist :lines drawmode :line-width line-width)))
                   

(defmethod all-points ((self 3DC) axe)
    (case axe
      ('x (x-points self))
      ('y (y-points self))
      ('z (z-points self))))

(defmethod all-points ((self 3DC-lib) axe)
  (remove nil (reduce #'append (mapcar #'(lambda (3DC) (all-points 3DC axe)) (bpf-list self)))))

(defmethod set-lines ((self om-3D-object-list) val)
  (mapcar #'(lambda (curve) (set-lines curve val)) (om-get-3D-objects self)))

(defmethod set-lines ((self 3D-curve) val)
  (setf (lines self) val))

(defmethod set-lines ((self t) val) nil)
    
(defmethod gl-3DC-from-obj ((self 3Deditor))
  (let* ((obj (if (and (multibpf? self) (not (show-back-p self)))
                  (get-current-object self)
                (object self))))
    (let ((newobj (gen-3D-obj obj (lines-p self) (param-line-width self))))
      newobj)))

(defmethod gen-3D-obj ((obj 3DC) mode line-width)
  (let ((glpoints (format-3d-points obj)))
    (3Dobj-from-points glpoints mode (bpfcolor obj) line-width)))

(defmethod gen-3D-obj ((obj 3DC-lib) mode line-width)
  (make-instance 'om-3D-object-list 
                 :objects (mapcar #'(lambda (o) 
                                      (3Dobj-from-points (format-3d-points o) mode (bpfcolor o) line-width))
                                  (bpf-list obj))))


(defmethod get-win-ed-size ((self 3DC)) (om-make-point 800 800))

(defmethod default-edition-params ((self 3DC)) 
  (pairlis '(winsize winpos mode show-axes show-room room-size line-width bg-color) 
           (list (om-make-point 800 800) (om-make-point 600 200) 0 *OM-DEFAULT-SHOW-AXES* *OM-DEFAULT-SHOW-ROOM* *OM-DEFAULT-ROOM-SIZE* *OM-GL-DEFAULT-LINEWIDTH* nil)))

(defmethod get-editor-class ((self 3DC)) '3DEditor)


;parameters stored with the editor
(defmethod param-room-size ((self 3DEditor) &optional (set-val nil set-val-supplied-p))
  (if set-val 
      (set-edit-param self 'room-size set-val)
    (get-edit-param self 'room-size)))

(defmethod param-show-room ((self 3DEditor) &optional (set-val nil set-val-supplied-p))
  (if set-val-supplied-p 
      (set-edit-param self 'show-room set-val)
    (get-edit-param self 'show-room)))

(defmethod param-show-axes ((self 3DEditor) &optional (set-val nil set-val-supplied-p))
  (if set-val 
      (set-edit-param self 'show-axes set-val)
    (get-edit-param self 'show-axes)))

(defmethod param-line-width ((self 3DEditor) &optional (set-val nil set-val-supplied-p))
  (if set-val 
      (set-edit-param self 'line-width set-val)
    (get-edit-param self 'line-width)))


;util to store boolean parameters as values
(defun param-value-to-boolean (val)
  (equal 1 val))

;util to convert boolean as values for storage
(defun boolean-to-param-value (boolean)
  (if boolean 1 0))

(defmethod metaobj-scrollbars-params ((self 3DEditor))  '(nil nil))







;;;=============================
;;;3D Panel
;;;=============================

(defclass 3DPanel (om-opengl-view) ())

(defmethod om-draw-contents ((self 3DPanel))
  (when (param-value-to-boolean (param-show-room (om-view-container self)))
    (opengl:gl-push-matrix) 
    (draw-3D-room (om-view-container self))
    (opengl:gl-pop-matrix))
  (when (param-value-to-boolean (param-show-axes (om-view-container self)))
    (opengl:gl-push-matrix) 
    (draw-3D-axes (om-view-container self))
    (opengl:gl-pop-matrix)))

(defmethod draw-3D-axes ((self 3DEditor))
  (let ((l (/ (float (param-room-size self)) 2.0)))
    (opengl:gl-begin opengl:*GL-LINES*)
    (opengl:gl-color3-f 0.8 0.3 0.3)
    (opengl:gl-vertex3-f (- l) 0.0 0.0) 
    (opengl:gl-vertex3-f l 0.0 0.0)
    (opengl:gl-color3-f 0.3 0.6 0.3)
    (opengl:gl-vertex3-f 0.0 (- l) 0.0) 
    (opengl:gl-vertex3-f 0.0 l 0.0) 
    (opengl:gl-color3-f 0.3 0.3 0.6)
    (opengl:gl-vertex3-f 0.0 0.0 (- l)) 
    (opengl:gl-vertex3-f 0.0 0.0 l) 
    (opengl:gl-end))
  (restore-om-gl-colors-and-attributes))

;jgarcia
(defmethod draw-3D-room ((self 3DEditor))
  "Draw the room"
  (opengl:gl-color4-f 0.5 0.5 0.5 0.5)
  (draw-point-cube (list 0.0 0.0 0.0) (param-room-size self) nil)
  (restore-om-gl-colors-and-attributes))

(defmethod update-3D-view ((self 3Deditor))
  (let ((sel (when (editor-3Dobj self) (selected-points (editor-3Dobj self))))
        (3D-obj (gl-3DC-from-obj self)))    
    (setf (selected-points (if (and (multibpf? self) (show-back-p self))
                               (nth (selected-component self) (om-get-3D-objects 3D-obj)) 
                             3D-obj)) sel)
    (om-set-gl-object (3Dp self) 3D-obj)
    ))


;;;=============================
;;; Controls
;;;=============================

(defclass 3Dcontrols (3Dborder-view) 
  ((mode-buttons :accessor mode-buttons :initform nil :initarg :mode-buttons)
   (curve-buttons :accessor curve-buttons :initform nil :initarg :mode-button))
  (:default-initargs :drawing-mode :quality))

(defmethod om-draw-contents ((self 3Dcontrols)) 
 (call-next-method)
 (when (param-show-axes (om-view-container self))
   (om-with-focused-view self
     (om-with-fg-color self (om-make-color 0.8 0.3 0.3)
       (om-draw-line 45 80 95 80)
       (om-draw-string 35 84 "X"))
     (om-with-fg-color self (om-make-color 0.3 0.6 0.3)
       (om-draw-line 45 90 95 90)
       (om-draw-string 35 94 "Y"))
     (om-with-fg-color self (om-make-color 0.3 0.3 0.6)
       (om-draw-line 45 100 95 100)
       (om-draw-string 35 104 "Z")))))


(defmethod update-3D-controls ((self 3Deditor))
  (add-curve-edit-buttons self (ctrlp self))
  (om-invalidate-view (ctrlp self)))


;;;=============================
;;; AUX BPC editor (2D views)
;;;=============================

(defclass internalbpceditor (bpceditor) 
  ((x-label :accessor x-label :initarg :x-label :initform "")
   (y-label :accessor y-label :initarg :y-label :initform "")))

(defclass internalbpcpanel (bpcpanel) 
  ((show-time :accessor show-time :initarg :show-time :initform nil)))

(defmethod bpc-editors ((self 3Deditor))
  (list (xyp self) (xzp self) (yzp self)))


(defmethod get-panel-class ((self internalbpceditor)) 'internalbpcpanel)

(defmethod x-label ((self internalbpcpanel)) (string (x-label (om-view-container self))))
(defmethod y-label ((self internalbpcpanel)) (string (y-label (om-view-container self))))

(defmethod initialize-instance :after ((Self internalbpceditor) &rest L) 
   (declare (ignore l))
   (om-remove-subviews self (title-bar self) (control self))
   (setf (grille-p (panel self)) t)
   (setf (show-back-p (panel self)) t))


(defmethod update-subviews ((self internalbpceditor))
  (let ((rulersize 18))
   (om-set-view-position (panel self) (om-make-point (+ 10 rulersize) 10))
   (om-set-view-size  (panel self) (om-make-point (- (w self) 20 rulersize) (- (h self) 20 rulersize)))
   (om-set-view-position (rulerx (panel self)) (om-make-point (+ 10 rulersize) (- (h self) 10 rulersize)))
   (om-set-view-size (rulerx (panel self)) (om-make-point (- (w self) 20 rulersize) rulersize))
   (om-set-view-position (rulery (panel self)) (om-make-point 10 10))
   (om-set-view-size (rulery (panel self)) (om-make-point rulersize (- (h self) rulersize 20)))
   (om-invalidate-view self)))

(defmethod om-draw-view-outline ((self internalbpceditor) &optional pensize) nil)
(defmethod om-draw-view-outline ((self internalbpcpanel) &optional pensize) nil)

(defmethod om-draw-contents  ((self internalbpceditor)) 
  (call-next-method)
  (when (and (x-label self) (y-label self))
  (om-with-focused-view self
    (om-with-fg-color self *om-black-color*
      (om-draw-string (- (x (panel self)) 20) (+ (y (panel self)) (h (panel self)) 14) 
                      (string+ (string (y-label self)) "/" (string (x-label self))))))
  ))


(defmethod ruleroffsety-from-editor ((self internalbpceditor)) 0)

(defmethod om-view-click-handler ((self internalbpcpanel) pos)
  (setf (focus (editor (om-view-window self))) self)
  (call-next-method))


(defmethod do-after-move ((Self internalbpcpanel)) 
  (report-modifications (editor self)))

(defmethod handle-key-event ((self internalbpcpanel) char) 
  (cond ((and (equal char :om-key-delete) (listp (selection? self)))
         (let ((del (mapcar 'cadr (slot-value self 'selection?)))
               (ed (om-view-container (editor self))))
           (loop for i in del do
                 (remove-nth-point (get-current-object ed) i))
           (set-points-selection ed nil)
           (init-tmp-objs ed)
           (update-editor-contents ed)
           (report-modifications ed)
           (om-invalidate-view (3Dp ed))
           ))
        ((equal char #\t) (setf (show-time self) (not (show-time self)))
             (om-invalidate-view self))
        (t (call-next-method))))

(defmethod add-point-to-bpf ((self internalbpcpanel) where)
   (let* ((ed (om-view-container (editor self)))
         (length (length (point-list (currentbpf self))))
         (new-point (pixel2point self where))
         (position-seg (segment-in-bpf self (currentbpf self) where))
         (prevpt (if position-seg 
                     (nth (1- position-seg) (point-list (get-current-object ed)))
                   (or (car (last (point-list (get-current-object ed))))
                       (make-obj-point (get-current-object ed) :x 0.0 :y 0.0 :z 0.0))))
         (3DP (cond ((and (equal (x-label (editor self)) 'x) (equal (y-label (editor self)) 'y))
                     (make-obj-point (get-current-object ed) :x (om-point-x new-point) :y (om-point-y new-point) 
                                     :z (om-point-z prevpt)))
                    ((and (equal (x-label (editor self)) 'x) (equal (y-label (editor self)) 'z)) 
                     (make-obj-point (get-current-object ed) :x (om-point-x new-point) :z (om-point-y new-point) 
                                     :y (om-point-v prevpt)))
                    ((and (equal (x-label (editor self)) 'y) (equal (y-label (editor self)) 'z)) 
                     (make-obj-point (get-current-object ed) :y (om-point-x new-point) :z (om-point-y new-point) 
                                     :x (om-point-x prevpt))))))
         (if position-seg
             (cons-bpf (get-current-object ed) (insert-in-list (point-list (get-current-object ed)) 3DP position-seg))
           (insert-point (get-current-object ed) 3DP))

     (init-tmp-objs ed)
     (update-editor-contents ed)
     (report-modifications ed)
     (om-invalidate-view (3Dp ed))
     
     (if position-seg
         (setf (selection? self) (list (list 3DP position-seg)))
       (setf (selection? self) (list (list 3DP length))))
     
     (scroll-point self 3DP)
     ))


;;; DISPLAY-MODE = 1 : BPC EDITS
(defmethod init-tmp-objs ((self 3Deditor))
  (let ((curr3dc (if (multibpf? self)
                     (if (selected-component self) (nth (selected-component self) (bpf-list (object self)))
                       (car (bpf-list (object self))))
                   (object self))))
    (setf (tmpview-objs self)
          (if (> (length (point-list curr3dc)) 1)
              (list (simple-bpf-from-list (x-points curr3Dc) (y-points curr3Dc) 'bpc (decimals curr3Dc))
                    (simple-bpf-from-list (x-points curr3Dc) (z-points curr3Dc) 'bpc (decimals curr3Dc))
                    (simple-bpf-from-list (y-points curr3Dc) (z-points curr3Dc) 'bpc (decimals curr3Dc)))
            (if (point-list curr3Dc)
                (let ((pt (car (point-list curr3Dc))))
                  (list (make-instance 'bpc :point-list (list (om-make-point (om-point-x pt) (om-point-y pt))) :decimals (decimals curr3Dc))
                        (make-instance 'bpc :point-list (list (om-make-point (om-point-x pt) (om-point-z pt))) :decimals (decimals curr3Dc))
                        (make-instance 'bpc :point-list (list (om-make-point (om-point-y pt) (om-point-z pt))) :decimals (decimals curr3Dc))))
              (list (make-instance 'bpc :point-list nil :decimals (decimals curr3Dc))
                    (make-instance 'bpc :point-list nil :decimals (decimals curr3Dc))
                    (make-instance 'bpc :point-list nil :decimals (decimals curr3Dc)))
              )))
          ))

(defmethod update-editor-contents ((self 3DEditor))
  (setf (object (xyp self)) (nth 0 (tmpview-objs self)))
  (setf (currentbpf (panel (xyp self))) (if (multibpf? (xyp self)) 
                                            (car (bpf-list (nth 0 (tmpview-objs self)))) 
                                          (nth 0 (tmpview-objs self))))
 (setf (object (xzp self)) (nth 1 (tmpview-objs self)))
  (setf (currentbpf (panel (xzp self))) (if (multibpf? (xzp self)) 
                                            (car (bpf-list (nth 1 (tmpview-objs self)))) 
                                          (nth 1 (tmpview-objs self))))
  (setf (object (yzp self)) (nth 2 (tmpview-objs self)))
  (setf (currentbpf (panel (yzp self))) (if (multibpf? (yzp self)) 
                                            (car (bpf-list (nth 2 (tmpview-objs self)))) 
                                          (nth 2 (tmpview-objs self))))
  (om-set-gl-object (3Dp self) (gl-3DC-from-obj self))
  (om-invalidate-view self t))


(defun set-points-selection (ed indices)
  (mapcar #'(lambda (bpced)
              (when bpced
                (setf (slot-value (panel bpced) 'selection?)
                      (if (consp indices)
                          (mapcar #'(lambda (i) (list (nth i (point-list (object bpced))) i)) indices)
                        indices))
                (om-invalidate-view (panel bpced))))
          (bpc-editors ed))
  (let ((obj (editor-3Dobj ed)))
    (setf (selected-points obj) indices))
  (update-3D-view ed)
  (om-invalidate-view (3Dp ed))
  )
  
(defmethod (setf selection?) (sel (self internalbpcpanel))
  ;(call-next-method)
  (let ((indices (mapcar 'cadr sel))
        (ed (om-view-container (editor self))))
    (set-points-selection ed indices)
    (selection? self)))
 
(defmethod report-modifications ((self internalbpceditor)) 
  (let* ((sel (slot-value (panel self) 'selection?))
        (selectedpts (if (consp sel) (mapcar 'cadr sel) sel))
        (editor (om-view-container self)))
    (cond ((and (equal (x-label self) 'x) (equal (y-label self) 'y))
           (report-bpfmodif (om-view-container self) :xpts (x-points (object self)) :ypts (y-points (object self))
                            :times (mapcar 'get-point-time (point-list (object self)))))
          ((and (equal (x-label self) 'x) (equal (y-label self) 'z))
           (report-bpfmodif (om-view-container self) :xpts (x-points (object self)) :zpts (y-points (object self))
                            :times (mapcar 'get-point-time (point-list (object self)))))
          ((and (equal (x-label self) 'y) (equal (y-label self) 'z))
           (report-bpfmodif (om-view-container self) :ypts (x-points (object self)) :zpts (y-points (object self))
                            :times (mapcar 'get-point-time (point-list (object self)))))
          (t nil))
    (set-points-selection editor selectedpts)
    (report-modifications editor)
    ))

(defmethod record-new-bpf ((Self internalbpceditor)) 
  (let* ((ed (om-view-container self))
         (3DCobj (if (multibpf? ed) 
                    (nth (selected-component ed) (bpf-list (object ed)))
                  (object ed))))
    (cons-bpf 3DCobj nil)
    (init-tmp-objs ed)
    (update-editor-contents ed)
    ))

(defmethod add-bpc-editors ((self 3Deditor))
  (om-add-subviews self
                   (setf (xyp self) (om-make-view 'internalbpceditor 
                                                  :owner self
                                                  :bg-color *om-light-gray-color*
                                                  :x-label 'x :y-label 'y
                                                  :object (nth 0 (tmpview-objs self))
                                                  ))
                   (setf (xzp self) (om-make-view 'internalbpceditor 
                                                  :owner self
                                                  :bg-color *om-light-gray-color*
                                                  :x-label 'x :y-label 'z
                                                  :object (nth 1 (tmpview-objs self))
                                                  ))
                   (setf (yzp self) (om-make-view 'internalbpceditor 
                                                  :owner self
                                                  :bg-color *om-light-gray-color*
                                                  :x-label 'y :y-label 'z
                                                  :object (nth 2 (tmpview-objs self))
                                                  )))
  (add-edit-buttons self))


(defmethod remove-bpc-editors ((self 3Deditor))
  (om-remove-subviews self (xyp self) (xzp self)(yzp self))
  (remove-edit-buttons self))

(defun init-bpc-editors (ed)
  (update-editor-after-eval (xyp ed) (nth 0 (tmpview-objs ed)))
  (update-editor-after-eval (xzp ed) (nth 1 (tmpview-objs ed)))
  (update-editor-after-eval (yzp ed) (nth 2 (tmpview-objs ed)))
  (mapcar #'(lambda (2Ded)
              (setf (lines-p (panel 2Ded)) (lines-p ed))
              ) (bpc-editors ed)))


(defmethod report-bpfmodif ((self 3Deditor) &key xpts ypts zpts times)
  (declare (ignore times))
  (let ((3DCobj (if (multibpf? self) 
                    (nth (selected-component self) (bpf-list (object self)))
                  (object self))))
    (unless xpts (setf xpts (x-points 3DCobj)))
    (unless ypts (setf ypts (y-points 3DCobj)))
    (unless zpts (setf zpts (z-points 3DCobj)))
    (let ((new-bpf (3Dc-from-list xpts ypts zpts (type-of 3DCobj) (decimals 3DCobj))))
      (cons-bpf 3DCobj (point-list new-bpf)))
    (init-tmp-objs self)
    (update-editor-contents self)))
  


;;;=========================
;;; INIT & UPDATE
;;;=========================

(defmethod update-editor-after-eval ((self 3DEditor) val)
  (call-next-method)
  (when (= (display-mode self) 1)
    (init-tmp-objs self)
    (init-bpc-editors self))
  (update-3D-view self)
  (om-invalidate-view (3Dp self)))

;anchor-init
(defmethod initialize-instance :after ((self 3DEditor) &rest l)
  (declare (ignore l))

  (om-set-bg-color self *om-light-gray-color*)
  (unless (bpf-p (object self))
    (setf (multibpf? self) t))
 
  ;compatibilité versions précédentes.. 
  ;à enlever si get-parameter retourne valeur par defaut au lieu de nil
  ;(unless (param-room-size self)
  ;  (param-room-size self *OM-DEFAULT-ROOM-SIZE*))
  ;(unless (param-show-room self)
  ;  (param-show-room self *OM-DEFAULT-SHOW-ROOM*))
  ;(unless (param-show-axes self)
  ;  (param-show-axes self *OM-DEFAULT-SHOW-AXES*))
  ;(unless (param-line-width self)
  ;  (param-line-width self *OM-GL-DEFAULT-LINEWIDTH*))


  (init-tmp-objs self)
  (om-add-subviews self
                   (setf (ctrlp self) (om-make-view '3Dcontrols
                                                     :owner self
                                                     :title "3D Controls"
                                                     :bg-color *om-light-gray-color*
                                                     :c++ *om-gray-color*
                                                     :c+ *om-light-gray-color*
                                                     :c-- *om-dark-gray-color*
                                                     :c- *om-gray-color*))

                   (setf (3Dp self) (om-make-view '3Dpanel
                                                  :owner self
                                                  :bg-color (om-make-color 0.8 0.8 0.8) ; (om-make-color 0.38 0.32 0.42)
                                                  :g-object (gl-3DC-from-obj self)
                                                  ))
                   )
  (when (get-edit-param self 'bg-color)
    (om-set-bg-color (3Dp self) (get-edit-param self 'bg-color)))
  
  (om-add-subviews (ctrlp self)
                   (om-make-dialog-item 'om-button (om-make-point 5 20) (om-make-point 100 20)
                                        "Init View"
                                        :di-action (om-dialog-item-act item
                                                     (declare (ignore item))
                                                     (om-init-3D-view (3Dp self))))
                   
                   (om-make-dialog-item 'om-check-box (om-make-point 5 50) (om-make-point 100 20)
                                        " Axes"
                                        :font *controls-font*
                                        :checked-p (param-value-to-boolean (param-show-axes self))
                                        :fg-color *om-black-color*
                                        :di-action (om-dialog-item-act item 
                                                     (param-show-axes self (boolean-to-param-value (om-checked-p item)))
                                                     (om-invalidate-view (3Dp self))
                                                     (om-invalidate-view (ctrlp self))
                                                     ))
                   (om-make-dialog-item 'om-check-box (om-make-point 5 110) (om-make-point 100 20)
                                        " Room"
                                        :font *controls-font*
                                        :checked-p (param-value-to-boolean (param-show-room self))
                                        :fg-color *om-black-color*
                                        :di-action (om-dialog-item-act item
                                                     (param-show-room self (boolean-to-param-value (om-checked-p item)))
                                                     (om-set-gl-object (3Dp self) (gl-3DC-from-obj self))
                                                     (om-invalidate-view (3Dp self))
                                                     )
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 5 140) (om-make-point 70 40)
                                        "Room Size"
                                        :font *controls-font*
                                        :fg-color *om-black-color*)
                   (om-make-dialog-item 'edit-numbox (om-make-point 80 140) (om-make-point 30 20) (format nil " ~D" (param-room-size self))
                                        :font *controls-font*
                                        :bg-color *om-white-color*
                                        :value (param-room-size self)
                                        :min-val 0.
                                        :di-action #'(lambda (item)
                                                       (param-room-size self (value item))
                                                       (om-set-gl-object (3Dp self) (gl-3DC-from-obj self))
                                                       (om-invalidate-view (3Dp self))
                                                       )
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 5 170) (om-make-point 70 30)
                                        "Back color"
                                        :font *controls-font*
                                        :fg-color *om-black-color*)
                   (om-make-view 'om-color-view 
                                 :position (om-make-point 80 170) 
                                 :size (om-make-point 30 22) 
                                 :color (om-get-bg-color (3Dp self))
                                 :after-fun #'(lambda (item) 
                                                (set-edit-param (ref self) 'bg-color (color item))
                                                (om-set-bg-color (3Dp self) (color item))
                                                (om-invalidate-view (3Dp self)))
                                 )
                   ;lines optionS
                   (om-make-dialog-item 'om-check-box (om-make-point 5 200) (om-make-point 100 20)
                                        " Lines"
                                        :font *controls-font*
                                        :checked-p (lines-p self)
                                        :fg-color *om-black-color*
                                        :di-action (om-dialog-item-act item
                                                     (setf (lines-p self) (om-checked-p item))
                                                     (if (lines-p self)
                                                         (add-curve-edit-buttons self (ctrlp self))
                                                       (remove-curve-edit-buttons self (ctrlp self)))
                                                     (when (= (display-mode self) 1)
                                                       (mapcar #'(lambda (ed)
                                                                   (setf (lines-p (panel ed)) (om-checked-p item))
                                                                   (om-invalidate-view (panel ed) t)) (bpc-editors self)))
                                                     (om-set-gl-object (3Dp self) (gl-3DC-from-obj self))
                                                     (om-invalidate-view (3Dp self))
                                                     )
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 5 360) (om-make-point 70 40)
                                        "Precision (decimals)"
                                        :font *controls-font*
                                        :fg-color *om-black-color*)
                   (om-make-dialog-item 'numbox (om-make-point 80 370) (om-make-point 30 20) (format nil " ~D" (decimals (object self)))
                                      :di-action nil
                                      :font *controls-font*
                                      :bg-color *om-white-color*
                                      :value (decimals (object self))
                                      :afterfun #'(lambda (item)
                                                    (editor-change-precision self (value item))
                                                    (om-set-gl-object (3Dp self) (gl-3DC-from-obj self))
                                                    (om-invalidate-view (3Dp self))
                                                    )
                                      :min-val 0
                                      :max-val 10)
                   (om-make-dialog-item 'om-check-box (om-make-point 5 400) (om-make-point 100 20)
                                        " 2D Editors"
                                        :font *controls-font*
                                        :checked-p (= (display-mode self) 1)
                                        :fg-color *om-black-color*
                                        :di-action (om-dialog-item-act item 
                                                     (setf (display-mode self) (if (om-checked-p item) 1 0))
                                                     (if (= (display-mode self) 1)
                                                         (progn 
                                                           (init-tmp-objs self)
                                                           (add-bpc-editors self)
                                                           (init-bpc-editors self))
                                                       (progn 
                                                         (remove-bpc-editors self)
                                                         (setf (focus self) nil)))
                                                     (update-subviews self))
                                        )
                   )
  (when (multibpf? self)
    (om-add-subviews (ctrlp self) 
                     (setf (sc-label self) (om-make-dialog-item 'om-static-text (om-make-point 5 500) (om-make-point 100 40)
                                                                "Selected curve:"
                                                                :font *controls-font*
                                                                :fg-color *om-black-color*))
                     (om-make-view 'om-icon-button :position (om-make-point 10 540) :size (om-make-point 18 18)
                                   :icon1 "+" :icon2 "+-pushed"
                                   :action #'(lambda (item) (declare (ignore item)) (add-new-curve self))
                                   )
                     (om-make-view 'om-icon-button :position (om-make-point 30 540) :size (om-make-point 18 18)
                                   :icon1 "-" :icon2 "--pushed"
                                   :action #'(lambda (item) (declare (ignore item)) (remove-current-curve self))
                                   )
                     (om-make-dialog-item 'om-check-box (om-make-point 8 560) (om-make-point 100 40)
                                          " Show all"
                                          :font *controls-font*
                                          :checked-p (show-back-p self)
                                          :fg-color *om-black-color*
                                          :di-action (om-dialog-item-act item 
                                                       (setf (show-back-p self) (om-checked-p item))
                                                       (om-set-gl-object (3Dp self) (gl-3DC-from-obj self))
                                                       (om-invalidate-view (3Dp self)))))
    (set-sc-label self))
  (when (= (display-mode self) 1)
    (add-bpc-editors self))
  (when (lines-p self)
    (add-curve-edit-buttons self (ctrlp self)))

  (om-init-3D-view (3Dp self))
  )

(defmethod remove-curve-edit-buttons ((self 3DEditor) panel)
  (apply 'om-remove-subviews (cons panel (curve-buttons panel))))

(defmethod add-curve-edit-buttons ((self 3DEditor) panel)
  (when (curve-buttons panel) (remove-curve-edit-buttons self panel))
  (setf (curve-buttons panel)
        (list 
         (om-make-dialog-item 'om-static-text (om-make-point 5 230) (om-make-point 70 20)
                              "Line width"
                              :font *controls-font*
                              :fg-color *om-black-color*)
         (om-make-dialog-item 'edit-numbox (om-make-point 80 230) (om-make-point 30 20) (format nil " ~D" (param-line-width self))
                              :font *controls-font*
                              :bg-color *om-white-color*
                              :value (param-line-width self)
                              :min-val 1.0
                              :max-val 6.0
                              :di-action #'(lambda (item)
                                             (param-line-width  self (value item))
                                             (update-3D-view self)
                                             (om-invalidate-view (3Dp self)))
                              )   
         (om-make-dialog-item 'om-static-text (om-make-point 5 260) (om-make-point 70 40)
                              "Curve color"
                              :font *controls-font*
                              :fg-color *om-black-color*)
         (om-make-view 'om-color-view 
                       :position (om-make-point 80 260) 
                       :size (om-make-point 30 22) 
                       :color (bpfcolor (get-current-object self))
                       :after-fun #'(lambda (item) 
                                      (setf (bpfcolor (get-current-object self)) (color item))
                                      (update-3D-view self)
                                      (om-invalidate-view (3Dp self)))
                       )
         ))
  (apply 'om-add-subviews (cons panel (curve-buttons panel))))



(defmethod add-edit-buttons ((self 3DEditor))
  (let ((ed (om-view-container (ctrlp self)))
        (x 24) (y 440))
    (setf (mode-buttons (ctrlp self))
          (append 
           (loop for mode in '(:normal :pen :move :zoom :scroll)
                 for icon in '("mousecursor" "pencursor" "handbpfcursor" "zoomcursor" "handcursor")
                 for i = 0 then (+ i 1)
                 collect 
                 (let ((m mode) button)
                   (when (= i 3) (setf x 24 y (+ y 30)))
                   (setf button (om-make-view 'om-icon-button :position (om-make-point x y) :size (om-make-point 22 22)
                                              :id mode
                                              :icon1 icon :icon2 (string+ icon "-pushed")
                                              :lock-push t
                                              :selected-p (and (bpc-editors ed) ;;; before initialization...
                                                               (equal m (mode (car (bpc-editors ed)))))
                                              :action #'(lambda (item) (declare (ignore item))
                                                          (mapcar #'(lambda (ed)
                                                                                   (when ed
                                                                                     (setf (mode (panel ed)) m)))
                                                                               (bpc-editors ed))
                                                          (update-cursor-mode-buttons (ctrlp self))
                                                          )))
                   (setf x (+ x 22))
                   button)
                 )
           (list (om-make-view 'om-icon-button :position (om-make-point x 470) :size (om-make-point 22 22)
                         :id :resize
                         :icon1 "resize" :icon2 "resize-pushed"
                         :lock-push nil
                         :action #'(lambda (item) (declare (ignore item))
                                     (mapcar #'(lambda (bpc-ed)
                                                 (init-coor-system (panel bpc-ed)))
                                             (bpc-editors ed)))))
           ))
    (apply 'om-add-subviews (cons (ctrlp self) (mode-buttons (ctrlp self))))
    ))

(defmethod remove-edit-buttons ((self 3DEditor))
  (apply 'om-remove-subviews (cons (ctrlp self) (mode-buttons (ctrlp self)))))

(defmethod update-cursor-mode-buttons ((self 3Dcontrols))
  (when (bpc-editors (om-view-container self))
    (loop for button in (mode-buttons self) do
          (setf (selected-p button) (equal (mode (car (bpc-editors (om-view-container self)))) (id button))))
    (om-invalidate-view self)))


(defmethod editor-change-precision ((self 3Deditor) newvalue)
  (let ((oldvalue (decimals (object self))))
  (change-precision (object self) newvalue)
  (when (= (display-mode self) 1)
    (mapcar #'(lambda (ed)
                (change-precision (object ed) newvalue)
                (adapt-coor-system (panel ed) (- newvalue oldvalue)))
            (bpc-editors self)))))
  

(defun set-sc-label (ed)
  (when (multibpf? ed)
    (let ((name (get-name (nth (selected-component ed) (bpf-list (object ed))))))
      (om-set-dialog-item-text (sc-label ed)
                               (if name
                                   (format nil "Selected curve:~%~D: ~A" (selected-component ed) name)
                                 (format nil "Selected curve: ~D" (selected-component ed))))
      )))
                                 

(defmethod update-subviews ((self 3DEditor))
  (let ((ctrlw 120))
    (om-set-view-position (ctrlp self) (om-make-point 0 0))
    (om-set-view-size (ctrlp self) (om-make-point ctrlw (h self)))
    
    (if (= (display-mode self) 0)
        (progn
          (om-set-view-position (3dp self) (om-make-point (+ 10 ctrlw) 10))
          (om-set-view-size (3dp self) (om-make-point (- (w self) ctrlw 20) (- (h self) 20))))
    (progn
    (om-set-view-position (3dp self) (om-make-point (+ 10 ctrlw) 10))
    (om-set-view-size (3dp self) (om-make-point (- (ceiling (- (w self) ctrlw) 2) 20) (- (ceiling (h self) 2) 20)))
    
    (om-set-view-position (xyp self) (om-make-point (+ ctrlw (floor (- (w self) ctrlw) 2)) 0))
    (om-set-view-size (xyp self) (om-make-point (floor (- (w self) ctrlw) 2) (ceiling (h self) 2)))

    (om-set-view-position (xzp self) (om-make-point ctrlw (floor (h self) 2)))
    (om-set-view-size (xzp self) (om-make-point (ceiling (- (w self) ctrlw) 2) (floor (h self) 2)))
  
    (om-set-view-position (yzp self) (om-make-point (+ ctrlw (floor (- (w self) ctrlw) 2)) (floor (h self) 2)))
    (om-set-view-size (yzp self) (om-make-point (floor (- (w self) ctrlw) 2) (- (h self) (floor (h self) 2))))
  ))))


;;;=========================
;;; ACTIONS
;;;=========================

(defmethod handle-key-event ((self 3DEditor) char)
  (cond 
   ;((equal char #\k) (add-new-curve self))
   ((equal char #\c)
    (let ((new-color (om-choose-color-dialog :color (bpfcolor (get-current-object self)))))
      (when new-color
        (setf (bpfcolor (get-current-object self)) new-color)
        (update-3D-view self)
        (om-invalidate-view (3Dp self)))))
   ((equal char #\b)
    (let ((new-color (om-choose-color-dialog :color (om-get-bg-color (3Dp self)))))
      (when new-color
        (set-edit-param (ref self) 'bg-color new-color)
        (om-set-bg-color (3Dp self) new-color)
        (om-invalidate-view (3Dp self)))))
   ((equal char #\h) (show-help-window (format nil "Commands for ~A Editor" 
                                               (string-upcase (class-name (class-of (object (editor self)))))) 
                                       (get-help-list (editor self))))
   ((equal char #\n) 
        (when (multibpf? (editor self))
          (let ((new-name (om-get-user-string "Object name:" :initial-string (get-name (get-current-object self)))))
           (when new-name
             (set-name (get-current-object self) new-name)
             (set-sc-label self)
             (om-invalidate-view self t)))))
   ((equal char :om-key-tab)
    (when (multibpf? self)
      (setf (selected-component self) (mod (1+ (selected-component self)) (length (bpf-list (object self)))))
      (when (= (display-mode self) 1)
        (init-tmp-objs self)
        (init-bpc-editors self))
      (update-3D-view self)
      (set-sc-label self)
      (update-3D-controls self)))
   ((focus self)
    (handle-key-event (focus self) char))
   (t nil)))


(defmethod add-new-curve ((self 3DEditor))
  (when (multibpf? self)
    (let* ((pos (selected-component self))
           (new3DC (make-instance (type-of (nth pos (bpf-list (object self)))) :point-list nil)))
      (setf (bpf-list (object self))
            (concatenate 'list (subseq (bpf-list (object self)) 0 (+ pos 1))
                         (list new3DC)
                         (subseq (bpf-list (object self)) (+ pos 1))
                         ))
      (setf (selected-component self) (+ pos 1))
      (when (= (display-mode self) 1)
        (init-tmp-objs self)
        (init-bpc-editors self))
      (set-sc-label self)
      (update-3D-view self)
      (om-invalidate-view (3Dp self)))))
    
(defmethod remove-current-curve ((self 3DEditor))
  (if (and (multibpf? self) (> (length (bpf-list (object self))) 1))
      (let* ((pos (selected-component self)))
        (setf (bpf-list (object self))
              (concatenate 'list 
                           (subseq (bpf-list (object self)) 0 pos)
                           (subseq (bpf-list (object self)) (+ pos 1))
                           ))
      (setf (selected-component self) (mod pos (length (bpf-list (object self)))))
      (when (= (display-mode self) 1)
        (init-tmp-objs self)
        (init-bpc-editors self))
      (set-sc-label self)
      (update-3D-view self)
      (om-invalidate-view (3Dp self)))
    (om-beep)))

(defmethod editor-select-all ((self 3DEditor))
  (set-points-selection self t)
  (om-invalidate-view self t))

(defmethod get-menubar ((self 3DEditor)) 
  (list (om-make-menu "File" 
                      (list (om-new-leafmenu  "Close" #'(lambda () (om-close-window (window self))) "w")))
        (om-make-menu "Edit" 
                      (list (om-new-leafmenu  "Select All" #'(lambda () (editor-select-all self)) "a")))
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self :disable '())))

(defmethod get-help-list ((self  3DEditor))
  (remove nil 
          (list '((#+cocoa "cmd+clic" #-cocoa "ctrl+clic" "Add point / Draw")
                  ("lrud" "Move selected points")
                  ("del" "Delete selected points")
                  (("c") "Change curve color")
                  (("b") "Change 3D background color")
                  (("p") "Show point indices")
                  )
                (when (multibpf? self)
                  '(("tab" "Change current curve")
                    (("n") "Change curve name")
                    )))))
