;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;Macintosh version (Digitool Macintosh Common Lisp - MCL)
;
;Copyright (C) 2004 IRCAM-Centre Georges Pompidou, Paris, France.
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
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Jean Bresson
;;===========================================================================

;;===========================================================================
;DocFile
;OM OpenGL api
;;===========================================================================


(in-package :oa)


;;=========================
;;  FROM LW EXAMPLE    ====
;;=========================

;;; ------------------------------------------------------------
;;; Types and creators.


(deftype gl-double () 'double-float)
(deftype gl-double-vector (n) `(opengl:gl-vector :double ,n))
(deftype gl-single () 'single-float)
(deftype gl-single-vector (n) `(opengl:gl-vector :float ,n))

(defun gl-float-vector (type contents)
  (opengl:make-gl-vector type (length contents) :contents contents))

(defun gl-double-vector (&rest contents)
  (gl-float-vector :double contents))

(defun gl-single-vector (&rest contents)
  (gl-float-vector :float contents))

(defun make-gl-double-vector (size)
  (opengl:make-gl-vector :double size))

(defun make-gl-single-vector (size)
  (opengl:make-gl-vector :float size))


;;; ------------------------------
;;; Vertex can be pass through to 'C'
;;; vertexes list of gl-vertexes (not passed to 'C')
;;; ------------------------------

(declaim (inline gl-vertex gl-vertexes))
(defun gl-vertex (x y z w)
  (gl-double-vector x y z w))

(defun gl-vertexes (contents)
  (mapcar #'(lambda (c) (apply 'gl-double-vector c)) contents))


;;; ------------------------------
;;; XYZ coordinate
;;; ------------------------------

(defstruct xyz 
  (x  0.0d0 :type double-float)
  (y  0.0d0 :type double-float)
  (z  0.0d0 :type double-float))


;;; ------------------------------------------------------------
;;; Geometry Utilities

(defun vector-difference (v1 v2 res)
  (loop for i fixnum below 3 do
        (setf (opengl:gl-vector-aref res i) (- (opengl:gl-vector-aref v1 i)
                                               (opengl:gl-vector-aref v2 i))))
  res)

(defun vector-sum (v1 v2 res)
  (loop for i fixnum below 3 do
        (setf (opengl:gl-vector-aref res i) (+ (opengl:gl-vector-aref v1 i)
                                               (opengl:gl-vector-aref v2 i))))
  res)

(defun normalize (vector)
  (let* ((x (opengl:gl-vector-aref vector 0))
         (y (opengl:gl-vector-aref vector 1))
         (z (opengl:gl-vector-aref vector 2))
         (d (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (zerop d)
        (error "Can't normalize a zero-length vector! ~s" vector)
      (setf (opengl:gl-vector-aref vector 0) (/ x d)
            (opengl:gl-vector-aref vector 1) (/ y d)
            (opengl:gl-vector-aref vector 2) (/ z d)))
    vector))

(defun normalized-cross-product (v1 v2 result)
  (let ((res (or result (make-gl-double-vector (length v1)))))
    (declare (type (gl-double-vector (*)) res))
    (setf (opengl:gl-vector-aref res 0) (- (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 2))
                                           (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 1)))
          (opengl:gl-vector-aref res 1) (- (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 0))
                                           (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 2)))
          (opengl:gl-vector-aref res 2) (- (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 1))
                                           (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 0))))
    (normalize res)
    res))




;;;=============================
;;; TEXT

(defun set-up-gl-fonts (pane obj)
  #+Win32
  (when (name obj)
    (unless (assoc :font (extra-display-lists obj))
      (push (list :font
                  (win32::wgl-use-font pane
                                       :start 0
                                       :count 256
                                       :outlinep t)
                  256)
            (extra-display-lists obj)))))

#+Win32
(defmacro with-3d-text-state-saved (&body body)
  `(opengl:with-matrix-pushed
     (opengl:gl-push-attrib opengl:*gl-all-attrib-bits*)
     ,@body
     (opengl:gl-pop-attrib)))

#+Win32
(defun draw-3d-text (obj text)
  (let* ((base (second (assoc :font (extra-display-lists obj)))))
    ;; Set up for a string-drawing display list call.
    (opengl:gl-list-base base)
    ;; Draw a string using font display lists.
    (fli:with-foreign-string (ptr elts bytes
                                  :external-format win32:*multibyte-code-page-ef*
                                  :null-terminated-p nil)
        text
      (declare (ignore bytes))
      (opengl:gl-call-lists elts
                            opengl:*gl-unsigned-byte*
                            ptr))))

#+Win32
(defun draw-positioned-3d-text (obj text
                                    x-pos y-pos z-pos
                                    x-rotation y-rotation z-rotation
                                    scale)
  (with-3d-text-state-saved
    (opengl:gl-translated x-pos y-pos z-pos)
    (opengl:gl-scaled scale scale scale)
    (opengl:gl-rotated x-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated y-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated z-rotation 0.0d0 0.0d0 1.0d0)
    ;; Draw the text.
    (draw-3d-text obj text)))

;;; ------------------------------------------------------------
;;; TEXTURE
;;; 

(defvar *texture-color* nil)

;;; Setup code
(defun get-texture-color ()
  (unless *texture-color*
    (setf *texture-color* (gl-single-vector 1.0 1.0 1.0 1.0)))
  *texture-color*)

;;; Cleanup code
(defun cleanup-texture-color ()
  (setf *texture-color* nil))

(defun make-object-colors (npolys &optional style)
  (case style
    (:uniform (make-array npolys
                          :initial-element (gl-single-vector (float (random 1.0) 1.0)
                                                             (float (random 1.0) 1.0)
                                                             (float (random 1.0) 1.0) 1.0)))
    (otherwise (coerce (loop for i below npolys collect
                             (gl-single-vector (float (random 1.0) 1.0)
                                               (float (random 1.0) 1.0)
                                               (float (random 1.0) 1.0) 1.0))
                       'vector))))

(defun make-object-vertexes (vertex-list)
  (coerce (gl-vertexes vertex-list) 'simple-vector))

(defun make-object-indexes (index-list)
  (let* ((biggest (loop for i in index-list maximize (length i)))
         (indexes (make-array (list (length index-list) biggest)
                              :element-type 'fixnum
                              :initial-element -1)))
    (loop for id in index-list
          for i from 0 do
          (loop for jd in id
                for j from 0 do
                (setf (aref indexes i j) jd)))
    indexes))

;;; ------------------------------------------------------------
;;; Class object
;;; 
;;; This superclass just manages display lists for the subclasses.

(defclass gl-object ()
  ((name :initform nil
                     :initarg :name
                     :accessor name)
   (use-display-list :initform nil
                     :initarg :use-display-list
                     :accessor use-display-list)
   (display-list :initform nil
                 :initarg :display-list
                 :accessor display-list)
   (extra-display-lists :initform nil
                        :accessor extra-display-lists)
   (viewer :accessor viewer)))

(defmethod draw :around ((object gl-object))
  (if (use-display-list object)
      (if (display-list object)
          (progn
            (opengl:gl-call-list (display-list object))
            (let ((draw (sys:cdr-assoc :draw (extra-display-lists object))))
              (mapc 'opengl:gl-call-list draw)))
        (progn
          (set-up-gl-fonts (viewer object) object)
          (let ((n (opengl:gl-gen-lists 1)))
	    (if (plusp n)
                (progn
	          (opengl:gl-new-list n opengl:*gl-compile-and-execute*)
                  (call-next-method)
	          (opengl:gl-end-list)
	          (setf (display-list object) n))
	      (progn 
	        (format t "~%~s:No more display list indexes!" object)
                (call-next-method))))))
    (call-next-method)))

(defmethod (setf use-display-list) :after (value (object gl-object))
  (unless value
    (delete-display-list object)))

(defmethod delete-display-list ((object gl-object))
  (when (display-list object)
    (opengl:gl-delete-lists (display-list object) 1)
    (setf (display-list object) nil))
  (loop for (nil start length) in (extra-display-lists object)
        do (opengl:gl-delete-lists start length))
  (setf (extra-display-lists object) nil))

;;; ------------------------------------------------------------
;;; Class projection
;;; 
;;; A class which defines the fovy, aspect, near and far
;;; values for a call to glu-perspective to define the projection
;;; matrix.

(defparameter *fovy* 45.0d0)
(defparameter *aspect* 1.0d0)
(defparameter *near* 0.001d0)
(defparameter *far* 100.0d0)


(defclass projection (gl-object)
   ((fovy :initform *fovy* :initarg :fovy :accessor fovy)
    (aspect :initform *aspect* :initarg :aspect :accessor aspect)
    (near :initform *near* :initarg :near :accessor near)
    (far :initform *far* :initarg :far :accessor far)))

(defmethod draw ((projection projection))
  (opengl:glu-perspective (fovy projection) (aspect projection) (near projection) (far projection)))

(defun make-projection (&key fovy aspect near far)
  (make-instance 'projection
                 :fovy (or fovy *fovy*)
                 :aspect (or aspect *aspect*)
                 :near (or near *near*)
                 :far (or far *far*)))
                                       

;;; ------------------------------------------------------------
;;; Class camera
;;; 
;;; Defines an eye point, a center point and an up vector.
;;; The draw method calls GLU-LOOK-AT to install the camera values.

(defparameter *eye* (make-xyz :y 5.0d0))
(defparameter *center* (make-xyz))
(defparameter *up* (make-xyz :z  1.0d0))

(defclass camera (gl-object)
  ((eye :initform (copy-structure *eye*)
        :initarg :eye
        :accessor eye
        :type xyz)
   (center :initform (copy-structure *center*)
           :initarg :center
           :accessor center
           :type xyz)
   (up :initform (copy-structure *up*)
       :initarg :up
       :accessor up
       :type xyz)
   (projection :initform (make-projection)
               :initarg :projection
               :accessor projection)
   (bgcolor :initform nil
            :initarg :bgcolor
            :accessor bgcolor)))


(defmethod draw ((camera camera))
  (let ((eye (eye camera))
        (center (center camera))
        (up (up camera))
        (projection (projection camera)))
    (declare (type xyz up eye center))

    (opengl:gl-matrix-mode opengl:*gl-projection*)
    (opengl:gl-load-identity)
    (draw projection)

    (opengl:gl-matrix-mode opengl:*gl-modelview*)

    (opengl:gl-load-identity)

    (opengl:glu-look-at (xyz-x eye) (xyz-y eye) (xyz-z eye)
                        (xyz-x center) (xyz-y center) (xyz-z center)
                        (xyz-x up) (xyz-y up) (xyz-z up))

    ;(opengl:gl-enable opengl:*gl-lighting*)
    
    (when (bgcolor camera)
      (opengl:gl-clear-color (car (bgcolor camera)) (cadr (bgcolor camera)) (caddr (bgcolor camera)) (cadddr (bgcolor camera))))
    (opengl:gl-clear opengl:*gl-color-buffer-bit*)

    (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
    (opengl:gl-depth-func opengl:*gl-less*)
    (opengl:gl-enable opengl:*gl-depth-test*)))


(defun make-camera (&key eye center up projection color)
  (make-instance 'camera
                 :eye (copy-structure (or eye *eye*))
                 :center (copy-structure (or center *center*))
                 :up (copy-structure (or up *up*))
                 :projection (or projection (make-projection))
                 :bgcolor (or color '(0.9 0.9 0.9 1.0))))

(defun init-camera (camera)
  (setf (eye camera) (copy-structure *eye*))
  (setf (center camera) (copy-structure *center*))
  (setf (up camera) (copy-structure *up*))
  (setf (projection camera) (make-projection)))
  
  
;;; ------------------------------------------------------------
;;; Geometric objects

(defclass geom-object (gl-object)
  ((colors :initform :uniform :initarg :colors :accessor colors :initarg :colors)
   (vertexes :initform nil :initarg :vertexes :accessor vertexes)
   (indexes :initform nil :initarg :indexes :accessor indexes)
   (smoothp :initform nil :initarg :smoothp :initarg smooth :accessor smoothp)
   (texturep :initform nil :initarg :texturep :accessor texturep)
   (subdivision :initform 0 :initarg :subdivision :accessor subdivision)
   (name :initform nil :initarg :name :accessor name))
  (:default-initargs
   :use-display-list T))

(defmethod initialize-instance :after ((geom-object geom-object)
                                       &key colors indexes vertexes &allow-other-keys)
  (setf (colors geom-object) (make-object-colors (length indexes) colors))
  (setf (vertexes geom-object) (make-object-vertexes vertexes))
  (setf (indexes geom-object) (make-object-indexes indexes)))

(defmethod draw ((obj geom-object))
  (let* ((indexes (indexes obj))
         (vertexes (vertexes obj))
         (colors (colors obj))
         (n (array-dimension indexes 1))
         (m (array-dimension indexes 0))
         (smoothp (smoothp obj))
         (texturep (texturep obj)))
    (labels ((texgen (param x y z w)
                     (opengl:with-gl-vectors ((v :type :float :length 4))
                       (setf (opengl:gl-vector-aref v 0) x
                             (opengl:gl-vector-aref v 1) y
                             (opengl:gl-vector-aref v 2) z
                             (opengl:gl-vector-aref v 3) w)
                       (opengl:gl-tex-geni param opengl:*gl-texture-gen-mode* opengl:*gl-object-linear*)
                       (opengl:gl-tex-genfv param opengl:*gl-object-plane* v))))
      (when texturep
        (opengl:gl-enable opengl:*gl-texture-gen-s*)
        (opengl:gl-enable opengl:*gl-texture-gen-t*)
        (texgen opengl:*gl-s* 1.0 0.0 0.0 1.0)
        (texgen opengl:*gl-t* 0.0 1.0 0.0 1.0)
        (opengl:gl-color4-fv (get-texture-color)))
      (if smoothp
          (progn 
            (opengl:gl-shade-model opengl:*gl-smooth*)
            (loop for ii below m do
                  (unless texturep
                    (opengl:gl-color4-fv (aref colors ii)))
                  (opengl:gl-begin opengl:*gl-polygon*)
                  (loop for i below n 
                        as index = (aref indexes ii i)
                        unless (minusp index) do
                        (let ((v (aref vertexes index)))
                          (opengl:gl-normal3-dv v)
                          (opengl:gl-vertex4-dv v)))
                  (opengl:gl-end)))
        (opengl:with-gl-vectors ((n1 :type :double :length 3)
                                 (n2 :type :double :length 3)
                                 (n3 :type :double :length 3))
          (opengl:gl-shade-model opengl:*gl-flat*)
          (loop for ii below m do
                (let ((v1 (aref vertexes (aref indexes ii 0)))
                      (v2 (aref vertexes (aref indexes ii 1)))
                      (v3 (aref vertexes (aref indexes ii 2))))
                  (unless texturep 
                    (opengl:gl-color4-fv (aref colors ii)))
                  (opengl:gl-begin opengl:*gl-polygon*)
                  (vector-difference v1 v2 n1)
                  (vector-difference v2 v3 n2)
                  (normalized-cross-product n1 n2 n3)
                  (opengl:gl-normal3-dv n3)
                  (loop for i below n
                        as index = (aref indexes ii i) 
                        unless (minusp index) do
                        (opengl:gl-vertex4-dv (aref vertexes index)))
                  (opengl:gl-end)))))
      (when texturep
        (opengl:gl-disable opengl:*gl-texture-gen-s*)
        (opengl:gl-disable opengl:*gl-texture-gen-t*)))))

#+Win32
(defmethod draw :after ((obj geom-object))
  (let* ((text (name obj)))
    (when text
      (if (listp text)
          (dolist (spec text)
            (apply 'draw-positioned-3d-text obj spec))
        (let* ((vertexes (vertexes obj))
               (vertex (aref vertexes 0)))
          (draw-positioned-3d-text obj text
                                   (opengl:gl-vector-aref vertex 0)
                                   (opengl:gl-vector-aref vertex 1)
                                   (opengl:gl-vector-aref vertex 2)
                                   -90d0 0d0 180d0
                                   0.5d0))))))


(defmethod (setf smoothp) :before (value (obj geom-object))
  (delete-display-list obj))

(defmethod (setf subdivision) :before (value (obj geom-object))
  (delete-display-list obj))

(defmethod (setf texturep) :before (value (obj geom-object))
  (delete-display-list obj))


;;; ------------------------------------------------------------
;;; The CAPI Interface
    
(defun initialize-viewer (canvas)
  ;; Initialize the icotransform to unity.
  (opengl:rendering-on (canvas)
    (setf (icotransform canvas) (make-gl-double-vector 16))
    (setf (light-transform canvas) (make-gl-double-vector 16))
    (initialize-transform (icotransform canvas))
    (initialize-transform (light-transform canvas))
    (reset-lights-and-materials)))

(defun initialize-transform (transform)
  (opengl:gl-matrix-mode opengl:*gl-modelview*)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defparameter *pointer-rotation-gain* 0.2d0)
(defparameter *pointer-translation-gain* 0.02d0)

(defun polar-rotate (transform dx dy)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-rotated  (float (* dx *pointer-rotation-gain*) 1.0d0) 0.0d0 0.0d0 1.0d0)
    (opengl:gl-rotated  (float (* dy *pointer-rotation-gain*) 1.0d0) 1.0d0 0.0d0 0.0d0) 
    (opengl:gl-mult-matrixd transform)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun translate (transform dx dy)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-translated (float dx 1.0d0) 0.0d0 (float dy 1.0d0))
    (opengl:gl-mult-matrixd transform)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun polar-rotate-light (viewer dx dy)
  (polar-rotate (light-transform viewer) dx dy))

(defun polar-rotate-icosahedron (viewer dx dy)
  (polar-rotate (icotransform viewer) dx dy))

(defun translate-icosahedron (viewer dx dy)
  (let ((factor (/ (xyz-y (eye (camera viewer))) 1500)))
    (translate (icotransform viewer) (* dx factor) (* dy factor))))

(defparameter *light-model-ambient* nil)
(defparameter *light-position* nil)
(defparameter *light-ambient* nil)
(defparameter *light-diffuse* nil)
(defparameter *light-specular* nil)

(defparameter *material-specular* nil)
(defparameter *material-shininess* 0.0)
(defparameter *material-emission* nil)

(defun reset-lights-and-materials ()
  (setf *light-model-ambient* (gl-single-vector 0.0 0.0 0.0 1.0))
  (setf *light-position* (gl-single-vector 1.0 2.0 3.0 1.0))
  (setf *light-ambient* (gl-single-vector 0.1 0.1 0.1 1.0))
  (setf *light-diffuse* (gl-single-vector 0.8 0.8 0.8 1.0))
  (setf *light-specular* (gl-single-vector 0.8 0.8 0.8 1.0))

  (setf *material-specular* (gl-single-vector 0.1 0.0 0.0 1.0))
  (setf *material-shininess* 64.0)
  (setf *material-emission* (gl-single-vector 0.0 0.0 0.0 1.0)))

(defun change-material-property (property inc)
  (let ((val (max -1.0 (min 1.0 (float (+ (opengl:gl-vector-aref  (symbol-value property)  0) inc) 1.0)))))
    (setf (symbol-value property) (gl-single-vector val val val 1.0))))

(defun nice-texture-on (viewer)
  (setf (texture-filter viewer) opengl:*gl-linear*))

(defun nice-texture-off (viewer)
  (setf (texture-filter viewer) opengl:*gl-nearest*))

(defun nice-texture-p (viewer)
  (eq (texture-filter viewer) opengl:*gl-linear*))

(defun turn-off-texture (viewer)
  (opengl:rendering-on ((canvas viewer))
    (setf (texturep (icosahedron viewer)) nil)
    (setf (texturep viewer) nil)
    (opengl:gl-disable opengl:*gl-texture-2d*)))

(defun turn-on-texture (viewer)
  (setf (texturep (object viewer)) t)
  (setf (texturep viewer) t)
  (opengl:rendering-on ((canvas viewer))
    (opengl:gl-pixel-storei opengl:*gl-unpack-alignment* 1)
    (opengl:gl-tex-image2-d opengl:*gl-texture-2d* 0 3 64 64 0 opengl:*gl-rgba* opengl:*gl-unsigned-byte* 
			    (texture-image viewer))
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-s* opengl:*gl-repeat*)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-t* opengl:*gl-clamp*)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-mag-filter* (texture-filter viewer))
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-min-filter* (texture-filter viewer))
    (opengl:gl-tex-envi opengl:*gl-texture-env* opengl:*gl-texture-env-mode* opengl:*gl-modulate*)
    (opengl:gl-enable opengl:*gl-texture-2d*)))

;;; camera in canvas à la place de interface
(defun opengl-resize-canvas (canvas x y width height)
  x y
  (when #+Win32 (win32:is-window-visible (win32:pane-hwnd (capi-internals:representation canvas)))
	#-Win32 T
    (opengl:rendering-on (canvas)
      (opengl:gl-viewport 0 0 width height))
      (setf (aspect (projection (camera canvas)))
            (coerce (/ width height) 'double-float))
    (opengl-redisplay-canvas canvas)))

(defun opengl-redisplay-canvas (canvas &rest ignore)
  ignore
  (unless (icotransform canvas)
    (initialize-viewer canvas))
  (opengl:rendering-on (canvas)
    
    (draw (camera canvas))
    
    (opengl:with-matrix-pushed
      (opengl:gl-mult-matrixd (light-transform canvas))
      
      (opengl:gl-light-modelfv opengl:*gl-light-model-ambient* *light-model-ambient*)
      (opengl:gl-light-modelf opengl:*gl-light-model-local-viewer* 0.0)
      (opengl:gl-light-modelf opengl:*gl-light-model-two-side* 0.0)
      
      (opengl:gl-enable opengl:*gl-light0*)
      )
    
    (opengl:with-matrix-pushed
      (opengl:gl-mult-matrixd (icotransform canvas))
      
      (opengl:gl-enable opengl:*gl-color-material*)
      (opengl:gl-color-material opengl:*gl-front* opengl:*gl-ambient-and-diffuse*)
      
      
      (om-draw-contents canvas)
      (if (g-object canvas)
        (draw (g-object canvas)))
      )
    
    (when (double-buffered-p canvas)
      (opengl:swap-buffers canvas))))

(defparameter *axes* nil)

(defun init-gl-axes ()
  (setf *axes*
        (let ((x 2.2d0))
          (make-object-vertexes
           (list (list (- x) 0.0d0 0.0d0 1.0d0) (list x     0.0d0 0.0d0 1.0d0) 
                 (list 0.0d0 (- x) 0.0d0 1.0d0) (list 0.0d0 x 0.0d0 1.0d0)
                 (list 0.0d0 0.0d0 (- x) 1.0d0) (list 0.0d0 0.0d0 x 1.0d0))))))

(om-api-add-init-func 'init-gl-axes)

(defmethod axes (view) t)

(defun draw-axes (view)
  (let ((axes (axes view)))
    (when axes
    (if (listp axes)
        (opengl:gl-color4-f (car axes) (cadr axes) (caddr axes) 1.0)
      (opengl:gl-color4-f 0.7 0.7 0.7 1.0)))
  (opengl:gl-begin opengl:*GL-LINES*)
  (loop for i from 0 to (- (length *axes*) 1) do
        (opengl:gl-vertex3-dv (aref *axes* i))) 
  (opengl:gl-end)))

;;======================
;;  OM INTERFACE    ====
;;======================

(export '(om-opengl-view 
          om-set-gl-object
          om-get-gl-object
          om-3D-object om-3D-object-list
          om-get-3D-objects
          om-init-3D-view
          om-get-gl-points
          om-set-3Dobj-points
          om-3Dobj-points
          om-3Dobj-color) :om-api)

(defclass om-opengl-view (opengl:opengl-pane om-view)
  ((g-object :initarg :g-object :accessor g-object :initform nil)
   (icotransform :initform nil :initarg :icotransform :accessor icotransform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   (double-buffered-p :initform t :initarg :double-buffered-p :accessor double-buffered-p)
   (lastxy :initform nil :initarg :lastxy :accessor lastxy)
   (camera :initform (make-camera :color '(0.9 0.9 0.9 1.0)) :initarg :camera :accessor camera))
  (:default-initargs 
   :configuration
      #-linux (list :rgba t :depth t :double-buffered t :depth-buffer 32) ;depth buffer allows to have depth in 3D drawing
      #+linux (list :rgba t :depth nil :double-buffered t)
   :use-display-list t
   :display-callback 'opengl-redisplay-canvas
   :resize-callback 'opengl-resize-canvas
   :input-model '(((:button-1 :press) opengl-viewer-button-1)
                  ((:button-2 :press) opengl-viewer-button-2)
                  ((:button-1 :shift :press) opengl-viewer-button-1-shift)
                  ((:button-1 :meta :press) opengl-viewer-button-1-alt)
                  ((:motion :button-1) opengl-viewer-motion-button-1)
                  ((:motion :button-2) opengl-viewer-motion-button-2)
                  ((:motion :button-1 :shift) opengl-viewer-motion-button-1-shift)
                  ((:motion :button-1 :meta) opengl-viewer-motion-button-1-alt)
                  ((:button-1 :second-press) opengl-viewer-button-1-second-press))
   
   ))


(defmethod (setf g-object) :before (new-object (viewer om-opengl-view))
  (opengl:rendering-on (viewer)
    (delete-display-list (g-object viewer))))

(defmethod (setf g-object) :after (new-object (viewer om-opengl-view))
  (when new-object
    (setf (viewer new-object) viewer))
  (opengl-redisplay-canvas viewer))

(defmethod om-set-gl-object ((self om-opengl-view) globject)
  (setf (g-object self) globject))

(defmethod om-get-gl-object ((self om-opengl-view))
  (g-object self))

(defmethod initialize-instance :after ((self om-opengl-view) &key &allow-other-keys)
  (setf (viewer (g-object self)) self)
  (when (om-get-bg-color self)
    (setf (bgcolor (camera self)) (list (float (om-color-r (om-get-bg-color self)))
                                        (float (om-color-g (om-get-bg-color self)))
                                        (float (om-color-b (om-get-bg-color self)))
                                        1.0)))
  (om-init-3d-view self))

(defmethod om-set-bg-color ((self om-opengl-view) color)
  (call-next-method)
  (setf (bgcolor (camera self)) (list (float (om-color-r color))
                                      (float (om-color-g color))
                                      (float (om-color-b color))
                                      1.0)))

(defmethod om-invalidate-view ((self om-opengl-view) &optional erase)
  (opengl-redisplay-canvas self))

(defmethod om-draw-contents ((self om-opengl-view)) nil)

(defun opengl-viewer-button-1 (canvas x y)
  (setf (lastxy canvas) (cons x y)))

(defun opengl-viewer-button-1-second-press (canvas x y)
  (om-init-3d-view canvas))

(defun opengl-viewer-motion-button-1 (canvas x y)
  (let ((last (lastxy canvas)))
    (when last
      (opengl:rendering-on (canvas)
        (polar-rotate-icosahedron canvas (- x (car last)) (- y (cdr last))))
      (opengl-redisplay-canvas canvas))
    (setf (lastxy canvas) (cons x y))))

(defun opengl-viewer-button-2 (canvas x y)
  (setf (lastxy canvas) (cons x y)))

(defun opengl-viewer-motion-button-2 (canvas x y)
  (let ((last (lastxy canvas)))
    (when last
      (opengl:rendering-on (canvas)
        (polar-rotate-light canvas (- x (car last)) (- y (cdr last))))
      (opengl-redisplay-canvas canvas))
    (setf (lastxy canvas) (cons x y))))

(defun opengl-viewer-button-1-shift (canvas x y)
  (setf (lastxy canvas) (cons x y)))

(defun opengl-viewer-motion-button-1-shift (canvas x y)
  (let ((last (lastxy canvas)))
    (when last
      (let ((eye (eye (camera canvas))))
        (setf (xyz-y eye)
              (min (- (xyz-y eye) (* (/ (- (cdr last) y) 20) (/ (xyz-y eye) 20))) -0.011d0))
        )
      (opengl-redisplay-canvas canvas))
    (setf (lastxy canvas) (cons x y))))

(defun opengl-viewer-button-1-alt (canvas x y)
  (setf (lastxy canvas) (cons x y)))

(defun opengl-viewer-motion-button-1-alt (canvas x y)
  (let ((last (lastxy canvas)))
    (when last
      (opengl:rendering-on (canvas)
        (translate-icosahedron canvas (- (car last) x) (- y (cdr last)) ))
      (opengl-redisplay-canvas canvas))
    (setf (lastxy canvas) (cons x y))))

(defun opengl-viewer-character-callback (canvas x y character)
  x y
  (with-slots ((viewer capi:interface)) canvas
    (process-character viewer character)))

(defun process-character (viewer character)
  (when 
      (case character
        (#\o (opengl:rendering-on ((canvas viewer))
               (setf (smoothp viewer)
                     (setf (smoothp (icosahedron viewer))
                           (not (smoothp (icosahedron viewer)))))
               t))
        (#\> (incf-subdivision viewer))
        (#\< (decf-subdivision viewer))
        (#\s (change-material-property '*material-specular* 0.05))
        (#\S (change-material-property '*material-specular* -0.05))
        (#\e (change-material-property '*material-emission* 0.05))
        (#\E (change-material-property '*material-emission* -0.05))
        (#\a (change-material-property '*light-ambient* 0.05))
        (#\A (change-material-property '*light-ambient* -0.05))
        (#\p (let ((new (max 0.0 (min 128.0 (+ *material-shininess* 8.0)))))
               (unless (= new *material-shininess*)
                 (setf *material-shininess* new))))
        (#\P (let ((new (max 0.0 (min 128.0 (+ *material-shininess* -8.0)))))
               (unless (= new *material-shininess*)
                 (setf *material-shininess* new))))
        (#\t (if (texturep (icosahedron viewer))
                 (turn-off-texture viewer)
               (turn-on-texture viewer))
             t)
        (#\n (if (nice-texture-p viewer)
                 (nice-texture-off viewer)
               (nice-texture-on viewer))
             (when (texturep (icosahedron viewer))
               (turn-off-texture viewer)
               (turn-on-texture viewer))
             t)
        #-lispworks7 ((#\begin #\home) 
		      (opengl:rendering-on ((canvas viewer))
			(initialize-transform (icotransform viewer))
			(initialize-transform (light-transform viewer)) t)
		      (setf (xyz-y (eye (camera (canvas viewer))))
			    (xyz-y *eye*)))
        #-lispworks7 (#\Insert (opengl:rendering-on ((canvas viewer)) (reset-lights-and-materials) t))
        #-lispworks7 (#\escape (opengl:rendering-on ((canvas viewer)) (delete-display-list (icosahedron viewer)))
			       (capi:quit-interface viewer))
	
	#+lispworks7 (("Begin" "Home") 
		      (opengl:rendering-on ((canvas viewer))
			(initialize-transform (icotransform viewer))
			(initialize-transform (light-transform viewer)) t)
		      (setf (xyz-y (eye (camera (canvas viewer))))
			    (xyz-y *eye*)))
        #+lispworks7 ("Insert" (opengl:rendering-on ((canvas viewer)) (reset-lights-and-materials) t))
        #+lispworks7 ("Escape" (opengl:rendering-on ((canvas viewer)) (delete-display-list (icosahedron viewer)))
			       (capi:quit-interface viewer))

	)
    (set-button-states viewer)
    (capi:with-busy-interface (viewer)
      (opengl-redisplay-canvas (canvas viewer)))))


(defmethod om-init-3D-view ((self om-opengl-view))
  (initialize-viewer self)
  (om-adapt-camera-to-object self)
  (opengl:rendering-on (self)
    (polar-rotate (icotransform self) -30 20))
  (opengl-redisplay-canvas self))
  
(defmethod om-adapt-camera-to-object ((self om-opengl-view))
  (let* ((dist-y (compute-max-extent (om-get-gl-object self)))
         (far-z (max 20.0d0 (* 5.0d0 dist-y))))
    (setf (xyz-y (eye (camera self))) (* dist-y -1.0 ))
    (setf (far (projection (camera self))) far-z)))

(defun compute-max-extent (gl-object)
  (let* ((points (om-3Dobj-points gl-object))
         (xpts nil) (ypts nil) (zpts nil)
          (xmi 0) (xma 0) (ymi 0) (yma 0) (zmi 0) (zma 0)
          (maxextent 0))
    (loop for point in points do
          (push (nth 0 point) xpts)
          (push (nth 1 point) ypts)
          (push (nth 2 point) zpts))
    (setq xpts (reverse xpts))
    (setq ypts (reverse ypts))
    (setq zpts (reverse zpts))  
    (when xpts 
      (setf xmi (reduce 'min xpts))
      (setf xma (reduce 'max xpts)))
    (when ypts 
      (setf ymi (reduce 'min ypts))
      (setf yma (reduce 'max ypts)))
    (when zpts 
      (setf zmi (reduce 'min zpts))
      (setf zma (reduce 'max zpts)))
    (setf maxextent (reduce 'max (list (abs xmi) (abs xma) (abs ymi) (abs yma) (abs zmi) (abs zma))))
    (max 5.0d0 (* 4.0d0 maxextent))))

  
;;; ------------------------------------------------------------

(defclass om-3D-object (gl-object)
  ((color :accessor color :initarg :color :initform nil)
   (points :initarg :points :accessor points :initform nil)
   (glvertexes :accessor glvertexes :initarg :glvertexes :initform nil))
  (:default-initargs
   :use-display-list T))


(defun points2vertex (points)
  (make-object-vertexes 
   (mapcar #'(lambda (p)
               (list
                (coerce (car p) 'double-float) 
                (coerce (cadr p) 'double-float) 
                (coerce (caddr p) 'double-float)
                1.0d0))
           points)))

(defmethod initialize-instance :after ((self om-3D-object) &key points &allow-other-keys)
  (setf (glvertexes self) (points2vertex points)))

  ;anti aliasing things. Warning: if depth enable it may not work...
(defun activate-anti-aliasing-parameters ()
  (opengl:gl-enable opengl:*gl-blend*)
  (opengl:gl-enable opengl:*gl-line-smooth*)
  (opengl:gl-blend-func opengl:*gl-src-alpha* opengl:*gl-one-minus-src-alpha*)
  (opengl:gl-hint opengl:*gl-line-smooth-hint* opengl:*gl-dont-care*)
  )

(defmethod draw ((self om-3D-object))
  (activate-anti-aliasing-parameters)
  (om-draw-contents self))

(defmethod om-draw-contents ((self om-3D-object)) nil)
           
(defmethod om-get-gl-points ((self om-3D-object))
  (glvertexes self))

(defmethod om-3Dobj-points ((self om-3D-object))
  (points self))

(defmethod om-set-3Dobj-points ((self om-3D-object) points)
  (setf (points self) points)
  (setf (glvertexes self) (points2vertex points)))

(defmethod om-3Dobj-color ((self om-3D-object))
  (color self))


(defclass om-3D-object-list (gl-object)
  ((objects :accessor objects :initarg :objects :initform nil)))

(defmethod om-get-3D-objects ((self om-3D-object-list))
  (objects self))

(defmethod om-3Dobj-points ((self om-3D-object-list))
  (apply 'append (mapcar 'om-3Dobj-points (objects self))))

(defmethod draw ((self om-3D-object-list)) 
  (mapcar 'draw (objects self)))

(defmethod (setf viewer) :after (view (self om-3D-object-list))
  (mapcar #'(lambda (o) (setf (viewer o) view)) (objects self)))
