;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPopengl-examples/RCS/icosahedron.lisp,v 1.20.3.3 2014/11/19 16:14:40 martin Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.


(in-package "USER")

;;; This file contains a demonstration of the LispWorks FLI for
;;; OpenGL.  To see the demonstration, compile and load the system
;;; OPENGL-EXAMPLES by
;;;
;;;    (load "<opengl-directory>/host")
;;;    (load "OPENGL:EXAMPLES;load")
;;;
;;; then evaluate
;;;
;;;    (setf v (capi:display (make-instance 'icosahedron-viewer)))
;;;
;;; An icosahedron is drawn with faces colored randomly and
;;; illuminated by a single light source.  The faces can be subdivided
;;; to approximate a sphere. The following keys and mouse gestures
;;; control it:
;;;
;;;   < and > decrease and increase the number of triangles.
;;;
;;;   Left mouse button rotates the sphere.
;;;
;;;   Middle mouse button rotates the light position.
;;;
;;;   Home resets the rotation state.
;;;
;;;   o toggles smooth shading.
;;;
;;;   p and P increase and decrease material shininess.
;;;
;;;   s and S increase and decrease material specular reflection component.
;;;
;;;   e and E increase and decrease material emission component.
;;;
;;;   a and A increase and decrease light source ambient component.
;;;
;;;   t toggles a texture map applied to the icosahedron
;;;
;;;   Insert resets the light and material parameters.
;;;
;;;   Escape quits the interface.
;;;
;;;
;;; The material ambient and diffuse components are not controllable.
;;; Note that light source ambient, material emission and material
;;; specular reflection can all have negative values, resulting in
;;; some strange effects, such as negative light specular reflections.
;;;


;;; ------------------------------------------------------------
;;; Programmming notes:
;;;
;;; - Vectors and arrays are converted to gl-vectors which is a
;;;   platform independent mechanism for passing through arrays.
;;;   gl-vectors behave in much the same way as normal vectors
;;;   except that:
;;;   1. They are allocated statically - the memory location of vector
;;;      contents will not be moved by the Lisp garbage collector. 
;;;      (Note that the garbage collector will still reclaim the data if
;;;      no longer referenced by Lisp.)
;;;   2. gl-vectors are not guaranteed to survive image-saving. The
;;;      implementation of gl-vectors can be found in "OPENGL:vectors.lisp"
;;;
;;; - The drawing of a heavily subdivided icosahedron is quite slow.
;;;   That code is not optimized.  However, a display list is used,
;;;   which means that its redisplay is very fast.
;;;
;;; - The OpenGL window is by default double-buffered, so you do not
;;;   see the drawing in progress. If you want to see the drawing
;;;   process, use the call
;;;
;;;   (setf v (capi:display (make-instance 'icosahedron-viewer
;;;                                        :double-buffered-p nil)))
;;;
;;; - This demo works on 8-bit screens only when double-buffering is
;;;   turned off.


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
;;; vertexes list of gl-vertexes (not passed to 'C'
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
;;; Class object
;;; 
;;; This superclass just manages display lists for the subclasses.

(defclass object ()
  ((use-display-list :initform nil
                     :initarg :use-display-list
                     :accessor use-display-list)
   (display-list :initform nil
                 :initarg :display-list
                 :accessor display-list)
   (extra-display-lists :initform nil
                        :accessor extra-display-lists)
   (viewer :accessor viewer)))

(defmethod draw :around ((object object))
  (if (use-display-list object)
      (if (display-list object)
          (progn
            (opengl:gl-call-list (display-list object))
            (let ((draw (sys:cdr-assoc :draw (extra-display-lists object))))
              (mapc 'opengl:gl-call-list draw)))
        (progn
          (set-up-gl-fonts (canvas (viewer object)) object)
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

(defmethod (setf use-display-list) :after (value (object object))
  (unless value
    (opengl:rendering-on ((canvas (viewer object)))
        (delete-display-list object))))

(defmethod delete-display-list ((object object))
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
(defparameter *near* 1.0d0)
(defparameter *far* 100.0d0)


(defclass projection (object)
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

(defclass camera (object)
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
               :accessor projection)))


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

    (opengl:gl-enable opengl:*gl-lighting*)

    (opengl:gl-clear-color 0.0 0.0 0.3  1.0)
    (opengl:gl-clear opengl:*gl-color-buffer-bit*)

    (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
    (opengl:gl-depth-func opengl:*gl-less*)
    (opengl:gl-enable opengl:*gl-depth-test*)))


(defun make-camera (&key eye center up projection)
  (make-instance 'camera
                 :eye (copy-structure (or eye *eye*))
                 :center (copy-structure (or center *center*))
                 :up (copy-structure (or up *up*))
                 :projection (or projection (make-projection))))


;;; ------------------------------------------------------------
;;; Geometric objects


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

(defclass geom-object (object)
  ((colors :initform nil :initarg :colors :accessor colors :initarg :color)
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

(defmethod (setf smoothp) :before (value (obj geom-object))
  (delete-display-list obj))

(defmethod (setf subdivision) :before (value (obj geom-object))
  (delete-display-list obj))

(defmethod (setf texturep) :before (value (obj geom-object))
  (delete-display-list obj))

        

;;; ------------------------------------------------------------
;;; Icosahedron

(defvar *icosahedron-vertexes*
  (let ((x 0.525731112119133606d0)
        (z 0.850650808352039932d0))
    (list (list (- x) 0.0d0 z     1.0d0) (list x     0.0d0 z     1.0d0) 
          (list (- x) 0.0d0 (- z) 1.0d0) (list x     0.0d0 (- z) 1.0d0)
          (list 0.0d0 z     x     1.0d0) (list 0.0d0 z     (- x) 1.0d0)
          (list 0.0d0 (- z) x     1.0d0) (list 0.0d0 (- z) (- x) 1.0d0)
          (list z     x     0.0d0 1.0d0) (list (- z) x     0.0d0 1.0d0)
          (list z     (- x) 0.0d0 1.0d0) (list (- z) (- x) 0.0d0 1.0d0))))

(defconstant *icosahedron-cw-indexes*
  '((0 4 1) (0 9 4) (9 5 4) (4 5 8) (4 8 1)
    (8 10 1) (8 3 10) (5 3 8) (5 2 3) (2 7 3)
    (7 10 3) (7 6 10) (7 11 6) (11 0 6) (0 1 6)
    (6 1 10) (9 0 11) (9 11 2) (9 2 5) (7 2 11)))

(defconstant *icosahedron-indexes*
  '((1 4 0) (4 9 0) (4 5 9) (8 5 4) (1 8 4)
    (1 10 8) (10 3 8) (8 3 5) (3 2 5) (3 7 2)
    (3 10 7) (10 6 7) (6 11 7) (6 0 11) (6 1 0)
    (10 1 6) (11 0 9) (2 11 9) (5 2 9) (11 2 7)))

(defun make-icosahedron-vertexes ()
  (make-object-vertexes *icosahedron-vertexes*))

(defun make-icosahedron-indexes ()
  (make-object-indexes *icosahedron-indexes*))

(defclass icosahedron (geom-object)
  ()
  (:default-initargs
   :vertexes *icosahedron-vertexes*
   :indexes *icosahedron-indexes*
   :name "Icosahedron"
   ))

(defun spherical-texture-coords (v va vb)
  (let ((x (opengl:gl-vector-aref v 0))
        (y (opengl:gl-vector-aref v 1))
        (z (opengl:gl-vector-aref v 2)))
    (if (and (zerop x) (zerop y))
        ;; vertex is at pole, so choose s coord based on other two vertexes.
        (let* ((ts1 (spherical-texture-coords va v vb))
               (ts2 (spherical-texture-coords vb v va))
               (tss (cond ((zerop ts1) (if (>= ts2 0.5) (* 0.5 (+ 1.0 ts2)) (* 0.5 ts2)))
                          ((= 1.0 ts1) (if (< ts2 0.5) (* 0.5 ts2) (* 0.5 (+ 1.0 ts2))))
                          ((zerop ts2) (if (>= ts1 0.5) (* 0.5 (+ 1.0 ts1)) (* 0.5 ts1)))
                          ((= 1.0 ts2) (if (< ts1 0.5) (* 0.5 ts1) (* 0.5 (+ 1.0 ts1))))
                          (t (* 0.5 (+ ts1 ts2))))))
            (if (plusp z)
                (values tss 1.0)
              (values tss 0.0)))
      (let* ((theta (atan y x))
             (d (sqrt (+ (* x x) (* y y))))
             (phi (atan z d))
             (ts (float (/ (+ theta pi) (* 2.0 pi)) 1.0))
             (tt (float (/ (+ phi (* pi 0.5)) pi) 1.0)))
        (values ts tt)))))

(defun tri-texture-coords (v1 v2 v3)
  ;; Compute the texture coordinates for each of 3 vertexes of a triangle.
  ;; Each value in each pair is in the inclusive range 0 -> 1.
  ;; The S coordinate values wrap around the object, so the choice of
  ;; 0 or 1 for each vertex in the boundary needs to be made according
  ;; to the S coordinate values of the other vertexes.
  (multiple-value-bind (ts1 tt1) (spherical-texture-coords v1 v2 v3)
    (multiple-value-bind (ts2 tt2) (spherical-texture-coords v2 v1 v3)
      (multiple-value-bind (ts3 tt3) (spherical-texture-coords v3 v1 v2)
        (if (or (>= (abs (- ts1 ts2)) 0.5) (>= (abs (- ts1 ts3)) 0.5))
            (let ((ss (list ts1 ts2 ts3)))
              (declare (dynamic-extent ss))
              (labels ((unitp (x) (or (zerop x) (= 1.0 x)))
                       (find-base (s)
                                  (let ((y (find-if-not #'unitp s)))
                                    (cond ((< y 0.5) 0.0)
                                          ((> y 0.5) 1.0)
                                          ((= y 0.5)
                                           (let ((y (find-if #'(lambda (x) (and (not (unitp x)) (/= 0.5 x))) s)))
                                             (if (or (null y) (< y 0.5)) 0.0 1.0)))))))                                                         
                (let ((u (find-base ss)))
                  (mapl #'(lambda (x) (when (unitp (car x)) (setf (car x) u))) ss)
                  (values (first ss) tt1 (second ss) tt2 (third ss) tt3))))
          (values ts1 tt1 ts2 tt2 ts3 tt3))))))

(defmethod draw ((icosahedron icosahedron))
  (let* ((colors (colors icosahedron))
         (vertexes (vertexes icosahedron))
         (indexes (indexes icosahedron))
         (n1 (make-gl-double-vector 3))
         (n2 (make-gl-double-vector 3))
         (n3 (make-gl-double-vector 3))
         (texturep (texturep icosahedron)))
    
    (when texturep
      (setf colors nil) 
      (opengl:gl-color4-fv (get-texture-color)))       
    (labels ((tri-simple (v1 v2 v3 normalp)                         ;; Draw a basic textured triangle
                         (multiple-value-bind (ts1 tt1 ts2 tt2 ts3 tt3) 
                             (tri-texture-coords v1 v2 v3)
                           (labels ((v-simple (ts tt v)
                                              (opengl:gl-tex-coord2-f (float ts 1.0) (float tt 1.0))
                                              (when normalp (opengl:gl-normal3-dv v))
                                              (opengl:gl-vertex4-dv v)))
                             (opengl:gl-begin opengl:*gl-polygon*)
                             (v-simple ts1 tt1 v1)
                             (v-simple ts2 tt2 v2)
                             (v-simple ts3 tt3 v3)
                             (opengl:gl-end))))

             ;; Test if 3 vertexes cross the XZ plane and if they do draw two textured
             ;; triangles which don't. 
             (tri-XZ-cross (v1 v2 v3 normalp)
                           (labels ((XZ-cross (v1 v2)
                                              (flet ((lindiv (a b i) (* 0.5d0 (+ (opengl:gl-vector-aref a i)
                                                                                 (opengl:gl-vector-aref b i)))))
                                                (let* ((s1 (signum (opengl:gl-vector-aref v1 1)))
                                                       (s2 (signum (opengl:gl-vector-aref v2 1))))
                                                  (when (minusp (* s1 s2))         ; return the new interpolated vertex
                                                    (gl-vertex (lindiv v1 v2 0) (lindiv v1 v2 1) (lindiv v1 v2 2) 1.0d0)))))
                                    (triv (va vb vc)
                                          (let ((vv (XZ-cross va vb)))
                                            (when vv
                                              (tri-simple va vv vc normalp)
                                              (tri-simple vb vc vv normalp)
                                              T))))
                             (or (triv v1 v2 v3)
                                 (triv v2 v3 v1)
                                 (triv v3 v1 v2))))

             ;; Draw a textured triangle, breaking it into two triangles when it intersects the XZ plane
             ;; in order to get the texture coords right.
             (texture-tri (v1 v2 v3 normalp)
                          (or (tri-XZ-cross v1 v2 v3 normalp)
                              (tri-simple v1 v2 v3 normalp)))

             ;; Draw a single triangle, maybe with texture, include per-vertex normals if normalp
             (vertex3 (v1 v2 v3 normalp)
                      (if texturep
                          (texture-tri v1 v2 v3 normalp)
                        (progn
                          (opengl:gl-begin opengl:*gl-polygon*)
                          (when normalp (opengl:gl-normal3-dv v1))
                          (opengl:gl-vertex4-dv v1)
                          (when normalp (opengl:gl-normal3-dv v2))
                          (opengl:gl-vertex4-dv v2)
                          (when normalp (opengl:gl-normal3-dv v3))
                          (opengl:gl-vertex4-dv v3)
                          (opengl:gl-end))))

             ;; Draw a single triangle, either with each vertex having its own normal (smoothp)
             ;; or with a single shared normal.
             (draw-tri (v1 v2 v3 color smoothp)
                       (if smoothp
                           (progn
                             (when color
                               (opengl:gl-color4-fv color))
                             (vertex3 v1 v2 v3 T))
                         (progn
                           (vector-difference v1 v2 n1)
                           (vector-difference v2 v3 n2)
                           (normalized-cross-product n1 n2 n3)
                           (when color
                             (opengl:gl-color4-fv color))
                           (opengl:gl-normal3-dv n3)
                           (vertex3 v1 v2 v3 nil)))))
      (if (smoothp icosahedron)
          (opengl:gl-shade-model opengl:*gl-smooth*)
        (opengl:gl-shade-model opengl:*gl-flat*))

      ;; The following draws an icosahedron, each triangle of which may be subdivided n times.
      (if (subdivision icosahedron)
          (labels ((subdivide (a b c depth i)
                              (if (zerop depth)
                                  (draw-tri a b c (when colors
                                                    (aref colors i)) (smoothp icosahedron))
                                (opengl:with-gl-vectors 
                                    ((ab :type :double :length 4 :contents '(0.0d0 0.0d0 0.0d0 1.0d0))
                                     (bc :type :double :length 4 :contents '(0.0d0 0.0d0 0.0d0 1.0d0))
                                     (ca :type :double :length 4 :contents '(0.0d0 0.0d0 0.0d0 1.0d0)))
                                  (let ((d (1- depth)))
                                    (vector-sum a b ab) (normalize ab)
                                    (vector-sum b c bc) (normalize bc) 
                                    (vector-sum c a ca) (normalize ca)
                                    (subdivide a ab ca d i)
                                    (subdivide b bc ab d i)
                                    (subdivide c ca bc d i)
                                    (subdivide ab bc ca d i))))))
            (loop for i below 20 do
                  (subdivide (aref vertexes (aref indexes i 0))
                             (aref vertexes (aref indexes i 1))
                             (aref vertexes (aref indexes i 2))
                             (subdivision icosahedron)
                             i)))
        (loop for i below 20 do
              (draw-tri (aref vertexes (aref indexes i 0))
                        (aref vertexes (aref indexes i 1))
                        (aref vertexes (aref indexes i 2)) 
                        (when colors
                          (aref colors i))
                        (smoothp icosahedron))
              )))))


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




;;; ------------------------------------------------------------
;;; The CAPI Interface
    
(defun make-default-background ()
  (gl-single-vector 0.5 0.5 0.5 1.0))

(eval-when (:load-toplevel :execute)
  (gp:register-image-translation :up *up-arrow*)
  (gp:register-image-translation :down *down-arrow*)
  (gp:register-image-translation :up-disabled *up-disabled*)
  (gp:register-image-translation :down-disabled *down-disabled*))
  

(defun make-ico-button-panel (items title &key (images (list :up :down)) (disabled-images (list :up-disabled :down-disabled))
                                    (interaction :no-selection))
  (make-instance 'capi:button-panel
                 :title title
                 :interaction interaction
                 :callback-type :interface-data
                 :selection-callback 'process-character
                 :layout-class 'capi:column-layout
                 :items items
                 :images images
                 :disabled-images disabled-images))

(capi:define-interface icosahedron-viewer (capi:interface)
  ((double-buffered-p :initform t :initarg :double-buffered-p :accessor double-buffered-p)
   (background :initform (make-default-background) :initarg :background :accessor background)
   (camera :initform (make-camera) :initarg :camera :accessor camera)
   (lastxy :initform nil :initarg :lastxy :accessor lastxy)
   (icotransform :initform nil :initarg :icotransform :accessor icotransform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   (geom-object :initform (make-instance 'icosahedron :use-display-list t)
                :initarg :icosahedron :initarg :object :accessor icosahedron :accessor object)
   (texture-image :initform (get-icosahedron-texture-data) :initarg :texture-image :accessor texture-image)
   (texture-filter :initform opengl:*gl-nearest* :initarg :texture-filter :accessor texture-filter)
   (texturep :initform nil :initarg :texturep :accessor texturep)
   (smoothp :initform nil :initarg :smoothp :accessor smoothp)
   (subdivision-buttons :initform (make-ico-button-panel  (list #\> #\<) "Subdiv."))
   (specular-buttons :initform (make-ico-button-panel (list #\s #\S) "Spec."))
   (emission-buttons :initform (make-ico-button-panel (list #\e #\E) "Em."))
   (ambient-buttons :initform (make-ico-button-panel (list #\a #\A) "Ambient"))
   (shine-buttons :initform (make-ico-button-panel (list #\p #\P) "Shine.")))
  (:panes 
   (canvas opengl:opengl-pane
	   :configuration (list :rgba t :depth nil :double-buffered double-buffered-p)
           :min-width 400
           :min-height 400
           :message "Mouse-L spins the object, Mouse-R moves the light, Shift Mouse-L moves your view."
	   :display-callback 'redisplay-canvas
	   :resize-callback 'resize-canvas
	   :input-model '(((:button-1 :press) viewer-button-1)
			  ((:button-2 :press) viewer-button-2)
			  ((:button-3 :press) viewer-button-2)
			  ((:button-1 :shift :press) viewer-button-1-shift)
			  ((:motion :button-1) viewer-motion-button-1)
			  ((:motion :button-2) viewer-motion-button-2)
			  ((:motion :button-3) viewer-motion-button-2)
			  ((:motion :button-1 :shift) viewer-motion-button-1-shift))
           :reader canvas
           :font (gp:make-font-description :family "Arial"))
   (shade-buttons capi:check-button-panel
                  :callback-type :interface-data
                  :selection-callback 'process-character
                  :retract-callback 'process-character
                  :layout-class 'capi:column-layout
                  :title "Shading"
                  :title-position :frame
                  :items (list #\o)
                  :print-function #'(lambda (x) (getf '(#\o "Smooth") x)))
   (texture-buttons capi:check-button-panel
                    :callback-type :interface-data
                    :selection-callback 'process-character
                    :retract-callback 'process-character
                    :layout-class 'capi:row-layout
                    :title "Texture"
                    :title-position :frame
                    :items (list #\t #\n)
                    :print-function #'(lambda (x) (getf '(#\t "On"  #\n "Nice") x)))
   (reset-buttons capi:button-panel
                  :interaction :no-selection
                  :title "Reset"
                  :title-position :frame
                  :callback-type :interface-data
                  :selection-callback 'process-character
                  :layout-class 'capi:row-layout
                  :items (list :home :insert)
                  :print-function #'(lambda (x) (getf '(:home "View" :insert "Material") x)))
   (quit-button capi:button-panel
                :interaction :no-selection
                :callback-type :interface-data
                :selection-callback 'process-character
                :layout-class 'capi:column-layout
                :items (list #\escape)
                :print-function #'(lambda (x) x "Quit")))

  (:layouts
   (main capi:column-layout '(top bottom canvas))
   (top capi:row-layout '(reset-buttons shade-buttons texture-buttons) :x-adjust :right :y-adjust :center :max-width T)
   (object capi:row-layout '(subdivision-buttons) :title "Object" :title-position :frame)
   (material capi:row-layout '(specular-buttons emission-buttons shine-buttons)
             :title "Material" :title-position :frame)
   (light capi:row-layout '(ambient-buttons)
             :title "Light" :title-position :frame)

   (buttons capi:row-layout '(object light material))
   (bottom capi:row-layout '(buttons quit-button)))
  (:default-initargs :auto-menus NIL :title "OpenGL Viewer"))

(defmethod initialize-instance :after ((self icosahedron-viewer) &key &allow-other-keys)
  (setf (viewer (object self)) self))

(defun initialize-viewer (icosahedron-viewer)
  ;; Initialize the icotransform to unity.
  (opengl:rendering-on ((canvas icosahedron-viewer))
    (setf (icotransform icosahedron-viewer) (make-gl-double-vector 16))
    (setf (light-transform icosahedron-viewer) (make-gl-double-vector 16))
    (initialize-transform (icotransform icosahedron-viewer))
    (initialize-transform (light-transform icosahedron-viewer))
    (reset-lights-and-materials)))

(defun initialize-transform (transform)
  (opengl:gl-matrix-mode opengl:*gl-modelview*)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun viewer-button-1 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-button-2 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-motion-button-1 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (opengl:rendering-on (canvas)
	  (polar-rotate-icosahedron viewer (- x (car last)) (- y (cdr last))))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defun viewer-motion-button-2 (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (opengl:rendering-on (canvas)
	  (polar-rotate-light viewer (- x (car last)) (- y (cdr last))))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defun viewer-button-1-shift (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (setf (lastxy viewer) (cons x y))))

(defun viewer-motion-button-1-shift (canvas x y)
  (with-slots ((viewer capi:interface)) canvas
    (let ((last (lastxy viewer)))
      (when last
        (let ((eye (eye (camera viewer))))
          (setf (xyz-y eye)
                (max (+ (xyz-y eye) (/ (- (car last) x) 20)) 2d0)))
        (redisplay-canvas canvas))
      (setf (lastxy viewer) (cons x y)))))

(defparameter *pointer-rotation-gain* 0.4d0)

(defun polar-rotate (transform dx dy)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-rotated (float (* dx *pointer-rotation-gain*) 1.0d0) 0.0d0 0.0d0 1.0d0)
    (opengl:gl-rotated (float (* (- dy) *pointer-rotation-gain*) 1.0d0) 1.0d0 0.0d0 0.0d0)
    (opengl:gl-mult-matrixd transform)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun polar-rotate-light (viewer dx dy)
  (polar-rotate (light-transform viewer) dx dy))

(defun polar-rotate-icosahedron (viewer dx dy)
  (polar-rotate (icotransform viewer) dx dy))

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

(defun viewer-character-callback (canvas x y character)
  x y
  (with-slots ((viewer capi:interface)) canvas
    (process-character viewer character)))

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
  (opengl:rendering-on ((canvas viewer))
    (setf (texturep (object viewer)) t)
    (setf (texturep viewer) t)
    (opengl:gl-pixel-storei opengl:*gl-unpack-alignment* 1)
    (opengl:gl-tex-image2-d opengl:*gl-texture-2d* 0 3 64 64 0 opengl:*gl-rgba* opengl:*gl-unsigned-byte* 
			    (texture-image viewer))
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-s* opengl:*gl-repeat*)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-wrap-t* opengl:*gl-clamp*)
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-mag-filter* (texture-filter viewer))
    (opengl:gl-tex-parameteri opengl:*gl-texture-2d* opengl:*gl-texture-min-filter* (texture-filter viewer))
    (opengl:gl-tex-envi opengl:*gl-texture-env* opengl:*gl-texture-env-mode* opengl:*gl-modulate*)
    (opengl:gl-enable opengl:*gl-texture-2d*)))

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
        (:home
         (opengl:rendering-on ((canvas viewer))
           (initialize-transform (icotransform viewer))
           (initialize-transform (light-transform viewer)) t)
         (setf (xyz-y (eye (camera viewer)))
               (xyz-y *eye*)))
        (:insert (opengl:rendering-on ((canvas viewer)) (reset-lights-and-materials) t))
        (#\escape (opengl:rendering-on ((canvas viewer)) (delete-display-list (icosahedron viewer)))
	          (capi:quit-interface viewer)))
    (set-button-states viewer)
    (capi:with-busy-interface (viewer)
      (redisplay-canvas (canvas viewer)))))

(defun incf-subdivision (viewer &optional (factor 2))
  (let* ((old (subdivision (icosahedron viewer)))
         (new (if (zerop old) 1 (min 3 (* factor old)))))
    (and (/= old new)
         (opengl:rendering-on ((canvas viewer))
           (setf (subdivision (icosahedron viewer)) new)))))

(defun decf-subdivision (viewer &optional (factor 2))
  (let* ((old (subdivision (icosahedron viewer)))
         (new (round old factor)))
    (and (/= old new)
         (opengl:rendering-on ((canvas viewer))
           (setf (subdivision (icosahedron viewer)) new)))))

(defun set-button-states (viewer)
  (with-slots (subdivision-buttons specular-buttons emission-buttons ambient-buttons shine-buttons) viewer
    (if (typep (object viewer) 'icosahedron)
        (capi:set-button-panel-enabled-items 
         subdivision-buttons
         :set T
         :disable (append (and (= (subdivision (icosahedron viewer)) 0) (list #\<))
                          (and (= (subdivision (icosahedron viewer)) 3) (list #\>))))
      (capi:set-button-panel-enabled-items 
       subdivision-buttons
       :set T
       :disable (list #\< #\>)))

    (capi:set-button-panel-enabled-items
     specular-buttons
     :set T
     :disable (append (and (<= (opengl:gl-vector-aref *material-specular* 0) -1.0) (list #\S))
                      (and (>= (opengl:gl-vector-aref *material-specular* 0) 1.0) (list #\s))))
    (capi:set-button-panel-enabled-items
     emission-buttons
     :set T
     :disable (append (and (<= (opengl:gl-vector-aref *material-emission* 0) -1.0) (list #\E))
                      (and (>= (opengl:gl-vector-aref *material-emission* 0) 1.0) (list #\e))))
    (capi:set-button-panel-enabled-items
     ambient-buttons
     :set T
     :disable (append (and (<= (opengl:gl-vector-aref *light-ambient* 0) -1.0) (list #\A))
                      (and (>= (opengl:gl-vector-aref *light-ambient* 0) 1.0) (list #\a))))
    (capi:set-button-panel-enabled-items
     shine-buttons
     :set T
     :disable (append (and (= *material-shininess* 0.0) (list #\P))
                      (and (= *material-shininess* 128.0) (list #\p))))
    ))
     

(defun redisplay-canvas (canvas &rest ignore)
  ignore
  (with-slots ((viewer capi:interface)) canvas
    (unless (icotransform viewer)
      (initialize-viewer viewer))
    (opengl:rendering-on (canvas)

      (draw (camera viewer))

      (opengl:with-matrix-pushed
        (opengl:gl-mult-matrixd (light-transform viewer))

        (opengl:gl-light-modelfv opengl:*gl-light-model-ambient* *light-model-ambient*)
        (opengl:gl-light-modelf opengl:*gl-light-model-local-viewer* 0.0)
        (opengl:gl-light-modelf opengl:*gl-light-model-two-side* 0.0)

        (opengl:gl-enable opengl:*gl-light0*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-position* *light-position*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-ambient* *light-ambient*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-diffuse* *light-diffuse*)
        (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-specular* *light-specular*))

      (opengl:with-matrix-pushed
        (opengl:gl-mult-matrixd (icotransform viewer))
        (opengl:gl-cull-face opengl:*gl-back*)
        (opengl:gl-enable opengl:*gl-cull-face*)

        (opengl:gl-enable opengl:*gl-color-material*)
        (opengl:gl-color-material opengl:*gl-front* opengl:*gl-ambient-and-diffuse*)

        (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-specular* *material-specular*)
        (opengl:gl-materialf opengl:*gl-front* opengl:*gl-shininess* *material-shininess*)
        (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-emission* *material-emission*)

        (draw (icosahedron viewer)))

      (when (double-buffered-p viewer)
        (opengl:swap-buffers canvas)))))

(defun resize-canvas (canvas x y width height)
  x y
  (when #+mswindows (win32:is-window-visible (win32:pane-hwnd (capi-internals:representation canvas)))
	#-mswindows t
    (opengl:rendering-on (canvas)
      (opengl:gl-viewport 0 0 width height))
    (with-slots ((viewer capi:interface)) canvas
      (setf (aspect (projection (camera viewer)))
            (coerce (/ width height) 'double-float)))
    (redisplay-canvas canvas)))


(defmethod (setf object) :before (new-object (viewer icosahedron-viewer))
  (opengl:rendering-on ((canvas viewer))
    (delete-display-list (object viewer))))

(defmethod (setf object) :after (new-object (viewer icosahedron-viewer))
  (setf (viewer new-object) viewer
        (texturep new-object) (texturep viewer)
        (smoothp new-object) (smoothp viewer))
  (set-button-states viewer)
  (redisplay-canvas (canvas viewer)))

(defvar *ico* nil)

(defun new-object (vertex-list index-list &key (colors :random))
  (setf (object *ico*) (make-instance 'geom-object 
                                      :vertexes vertex-list
                                      :indexes index-list
                                      :colors colors)))

(defun reset-object ()
  (setf (object *ico*) (make-instance 'icosahedron)))

