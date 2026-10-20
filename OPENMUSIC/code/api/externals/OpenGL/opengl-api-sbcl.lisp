;;===========================================================================
;DocFile
;OM OpenGL api -- SBCL / Alloy / GLFW.  The twin of opengl-api.lisp:
;bridges the GL 1.x names of opengl-sbcl/ into the OPENGL package and
;defines the om-* layer.
;;===========================================================================

(in-package :cl-user)

(unless (find-package "IMMEDIATE-GL")
  (error "opengl-api-sbcl: no IMMEDIATE-GL package; opengl-sbcl/load.lisp did not run."))

;;;====================================================================
;;; (a) SYMBOL BRIDGE -- IMMEDIATE-GL -> the OPENGL package
;;;====================================================================
;;; OPENGL is a nickname of CL-OPENGL, which the Alloy renderer already loads.
;;; The library's own lifecycle names are NOT bridged: FLUSH collides with
;;; cl-opengl's GL:FLUSH.

(defparameter *opengl-api-bridge-exclusions*
  '("INIT" "SHUTDOWN" "FLUSH" "IMMEDIATE-GL-ERROR"
    ;; per-context lifecycle: library machinery, not GL 1.x names
    "*CONTEXT*" "CONTEXT-READY-P" "SHUTDOWN-CONTEXT" "FORGET-CONTEXT"
    "LIVE-CONTEXT-COUNT"))

(defun %opengl-api-bridge-names ()
  (let ((igl (find-package "IMMEDIATE-GL"))
        (names '()))
    (do-external-symbols (s igl (sort names #'string<))
      (unless (member (symbol-name s) *opengl-api-bridge-exclusions* :test #'string=)
        (push (symbol-name s) names)))))

(defun %opengl-api-install-bridge ()
  "Alias every GL 1.x name of IMMEDIATE-GL into OPENGL.  Refuses to touch a name
that package already owns -- an alias silently shadowing a cl-opengl export
would be a bug that only shows up as a wrong draw much later."
  (let* ((igl (find-package "IMMEDIATE-GL"))
         (target (find-package "OPENGL"))
         (names (%opengl-api-bridge-names)))
    (unless target
      (error "opengl-api-sbcl: no OPENGL package; cl-opengl is not loaded."))
    (let ((collisions (loop for name in names
                            for existing = (find-symbol name target)
                            when (and existing (not (eq (symbol-package existing) igl)))
                              collect (list name (package-name (symbol-package existing))))))
      (when collisions
        (error "opengl-api-sbcl: ~D name(s) already owned by another package, ~
the bridge would shadow them: ~S" (length collisions) collisions)))
    (dolist (name names (length names))
      (let ((symbol (find-symbol name igl)))
        (import symbol target)
        (export symbol target)))))

(format *standard-output* "~&; opengl-api-sbcl: bridged ~D GL names into OPENGL~%"
        (%opengl-api-install-bridge))

;;;====================================================================
;;; (b) THE OM LAYER
;;;====================================================================

(in-package :om-api)

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

;;;--------------------------------------------------------------------
;;; Vertices
;;;--------------------------------------------------------------------
;;; OM-GET-GL-POINTS must return something the kernel can AREF and hand to
;;; OPENGL:GL-VERTEX4-DV (projects 3deditor.lisp: om-draw-contents for 3D-cube), i.e. a vector of 4-element
;;; homogeneous coordinates -- the shape LispWorks' MAKE-GL-VECTOR produced.

(defun gl-vertex (x y z w)
  (make-array 4 :element-type 'double-float
                :initial-contents (list (float x 1.0d0) (float y 1.0d0)
                                        (float z 1.0d0) (float w 1.0d0))))

(defun points2vertex (points)
  "POINTS is a list of (x y z); returns a simple-vector of homogeneous vertices."
  (coerce (mapcar (lambda (p) (gl-vertex (first p) (second p) (third p) 1.0d0))
                  points)
          'simple-vector))

;;;--------------------------------------------------------------------
;;; XYZ, projection, camera  (opengl-api.lisp: from defstruct xyz to init-camera)
;;;--------------------------------------------------------------------

(defstruct xyz
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float)
  (z 0.0d0 :type double-float))

(defparameter *fovy* 45.0d0)
(defparameter *aspect* 1.0d0)
(defparameter *near* 0.001d0)
(defparameter *far* 100.0d0)

(defclass projection ()
  ((fovy :initform *fovy* :initarg :fovy :accessor fovy)
   (aspect :initform *aspect* :initarg :aspect :accessor aspect)
   (near :initform *near* :initarg :near :accessor near)
   (far :initform *far* :initarg :far :accessor far)))

(defun make-projection (&key fovy aspect near far)
  (make-instance 'projection
                 :fovy (or fovy *fovy*) :aspect (or aspect *aspect*)
                 :near (or near *near*) :far (or far *far*)))

(defparameter *eye* (make-xyz :y 5.0d0))
(defparameter *center* (make-xyz))
(defparameter *up* (make-xyz :z 1.0d0))

(defclass camera ()
  ((eye :initform (copy-structure *eye*) :initarg :eye :accessor eye :type xyz)
   (center :initform (copy-structure *center*) :initarg :center :accessor center :type xyz)
   (up :initform (copy-structure *up*) :initarg :up :accessor up :type xyz)
   (projection :initform (make-projection) :initarg :projection :accessor projection)
   (bgcolor :initform nil :initarg :bgcolor :accessor bgcolor)))

(defun make-camera (&key eye center up projection color)
  (make-instance 'camera
                 :eye (copy-structure (or eye *eye*))
                 :center (copy-structure (or center *center*))
                 :up (copy-structure (or up *up*))
                 :projection (or projection (make-projection))
                 :bgcolor (or color '(0.9 0.9 0.9 1.0))))

;;;--------------------------------------------------------------------
;;; Lights and materials  (opengl-api.lisp: from *light-model-ambient* to change-material-property)
;;;--------------------------------------------------------------------
;;; Kept as the vanilla has them.  Only *LIGHT-MODEL-AMBIENT* is read back by
;;; the redisplay; the vanilla sets the other six and never reads them, because
;;; OPENGL-REDISPLAY-CANVAS calls no GL-LIGHTFV and no GL-MATERIALFV.  They are
;;; still reset here so a later port of the material path finds them.

(defparameter *light-model-ambient* nil)
(defparameter *light-position* nil)
(defparameter *light-ambient* nil)
(defparameter *light-diffuse* nil)
(defparameter *light-specular* nil)
(defparameter *material-specular* nil)
(defparameter *material-shininess* 0.0)
(defparameter *material-emission* nil)

(defun gl-single-vector (&rest contents)
  (make-array (length contents) :element-type 'single-float
                                :initial-contents (mapcar (lambda (x) (float x 1.0)) contents)))

(defun reset-lights-and-materials ()
  (setf *light-model-ambient* (gl-single-vector 0.0 0.0 0.0 1.0))
  (setf *light-position* (gl-single-vector 1.0 2.0 3.0 1.0))
  (setf *light-ambient* (gl-single-vector 0.1 0.1 0.1 1.0))
  (setf *light-diffuse* (gl-single-vector 0.8 0.8 0.8 1.0))
  (setf *light-specular* (gl-single-vector 0.8 0.8 0.8 1.0))
  (setf *material-specular* (gl-single-vector 0.1 0.0 0.0 1.0))
  (setf *material-shininess* 64.0)
  (setf *material-emission* (gl-single-vector 0.0 0.0 0.0 1.0)))

;;;--------------------------------------------------------------------
;;; Transforms  (opengl-api.lisp: from initialize-viewer to translate-icosahedron)
;;;--------------------------------------------------------------------
;;; Ported literally.  The emulated matrix stack is pure Lisp state, so unlike
;;; the vanilla these need NO GL context and no OPENGL:RENDERING-ON -- which is
;;; what lets them run straight from a mouse callback.

(defparameter *pointer-rotation-gain* 0.2d0)
(defparameter *pointer-translation-gain* 0.02d0)

(defun make-gl-double-vector (size)
  (make-array size :element-type 'double-float :initial-element 0.0d0))

(defun initialize-transform (transform)
  (immediate-gl:gl-matrix-mode immediate-gl:*gl-modelview*)
  (immediate-gl:with-matrix-pushed
    (immediate-gl:gl-load-identity)
    (immediate-gl:gl-get-doublev immediate-gl:*gl-modelview-matrix* transform)))

(defun polar-rotate (transform dx dy)
  (immediate-gl:gl-matrix-mode immediate-gl:*gl-modelview*)
  (immediate-gl:with-matrix-pushed
    (immediate-gl:gl-load-identity)
    (immediate-gl:gl-rotated (float (* dx *pointer-rotation-gain*) 1.0d0) 0.0d0 0.0d0 1.0d0)
    (immediate-gl:gl-rotated (float (* dy *pointer-rotation-gain*) 1.0d0) 1.0d0 0.0d0 0.0d0)
    (immediate-gl:gl-mult-matrixd transform)
    (immediate-gl:gl-get-doublev immediate-gl:*gl-modelview-matrix* transform)))

(defun translate (transform dx dy)
  (immediate-gl:gl-matrix-mode immediate-gl:*gl-modelview*)
  (immediate-gl:with-matrix-pushed
    (immediate-gl:gl-load-identity)
    (immediate-gl:gl-translated (float dx 1.0d0) 0.0d0 (float dy 1.0d0))
    (immediate-gl:gl-mult-matrixd transform)
    (immediate-gl:gl-get-doublev immediate-gl:*gl-modelview-matrix* transform)))

;;;--------------------------------------------------------------------
;;; OM-OPENGL-VIEW
;;;--------------------------------------------------------------------

(defclass om-opengl-view (om-view)
  ((g-object :initarg :g-object :accessor g-object :initform nil)
   (icotransform :initform nil :initarg :icotransform :accessor icotransform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   (double-buffered-p :initform t :initarg :double-buffered-p :accessor double-buffered-p)
   (lastxy :initform nil :initarg :lastxy :accessor lastxy)
   ;; which drag the press started, latched at press exactly as the vanilla's
   ;; :input-model latches it by picking a different callback (opengl-api.lisp: the
   ;; :input-model of the defclass om-opengl-view)
   (gesture :initform nil :accessor gesture)
   ;; Match the effective CAPI/display-list lifecycle: construction initializes
   ;; the camera, while the first user interaction materializes the GL scene.
   (gl-scene-materialized-p :initform nil :accessor gl-scene-materialized-p)
   (camera :initform (make-camera :color '(0.9 0.9 0.9 1.0)) :initarg :camera :accessor camera)))

(defmethod om-set-gl-object ((self om-opengl-view) globject)
  (setf (g-object self) globject))

(defmethod om-get-gl-object ((self om-opengl-view))
  (g-object self))

(defun %materialize-opengl-view (view)
  "Activate VIEW's GL scene once it is attached to a live Alloy tree."
  (when (and (alloy:layout-tree view)
             (not (gl-scene-materialized-p view)))
    (setf (gl-scene-materialized-p view) t))
  (om-invalidate-view view)
  view)

(defmethod (setf g-object) :after (new-object (viewer om-opengl-view))
  (declare (ignore new-object))
  (om-invalidate-view viewer))

(defun %camera-color-list (color)
  (list (float (om-color-r color)) (float (om-color-g color))
        (float (om-color-b color)) 1.0))

(defmethod initialize-instance :after ((self om-opengl-view) &key &allow-other-keys)
  (when (om-get-bg-color self)
    (setf (bgcolor (camera self)) (%camera-color-list (om-get-bg-color self))))
  (om-init-3D-view self))

(defmethod om-set-bg-color ((self om-opengl-view) color)
  (call-next-method)
  (setf (bgcolor (camera self)) (%camera-color-list color)))

(defmethod om-draw-contents ((self om-opengl-view)) nil)

;;;--------------------------------------------------------------------
;;; Camera fitting  (opengl-api.lisp: om-init-3D-view for om-opengl-view and compute-max-extent)
;;;--------------------------------------------------------------------

(defun initialize-viewer (canvas)
  (setf (icotransform canvas) (make-gl-double-vector 16))
  (setf (light-transform canvas) (make-gl-double-vector 16))
  (initialize-transform (icotransform canvas))
  (initialize-transform (light-transform canvas))
  (reset-lights-and-materials))

(defun compute-max-extent (gl-object)
  (let ((points (and gl-object (om-3Dobj-points gl-object)))
        (xpts nil) (ypts nil) (zpts nil)
        (xmi 0) (xma 0) (ymi 0) (yma 0) (zmi 0) (zma 0)
        (maxextent 0))
    (loop for point in points do
      (push (nth 0 point) xpts)
      (push (nth 1 point) ypts)
      (push (nth 2 point) zpts))
    (when xpts (setf xmi (reduce 'min xpts)) (setf xma (reduce 'max xpts)))
    (when ypts (setf ymi (reduce 'min ypts)) (setf yma (reduce 'max ypts)))
    (when zpts (setf zmi (reduce 'min zpts)) (setf zma (reduce 'max zpts)))
    (setf maxextent (reduce 'max (list (abs xmi) (abs xma) (abs ymi)
                                       (abs yma) (abs zmi) (abs zma))))
    (max 5.0d0 (* 4.0d0 maxextent))))

(defmethod om-adapt-camera-to-object ((self om-opengl-view))
  (let* ((dist-y (compute-max-extent (om-get-gl-object self)))
         (far-z (max 20.0d0 (* 5.0d0 dist-y))))
    (setf (xyz-y (eye (camera self))) (* dist-y -1.0d0))
    (setf (far (projection (camera self))) far-z)))

(defmethod om-init-3D-view ((self om-opengl-view))
  (initialize-viewer self)
  (om-adapt-camera-to-object self)
  (polar-rotate (icotransform self) -30 20)
  ;; During construction there is no live layout tree; Init View later uses the
  ;; same call and materializes the already attached pane.
  (%materialize-opengl-view self))

;;;--------------------------------------------------------------------
;;; Drawing
;;;--------------------------------------------------------------------
;;; The vanilla drives this from a CAPI :display-callback
;;; (opengl-api.lisp: opengl-redisplay-canvas).  Here the framework
;;; already calls OM-DRAW-CONTENTS on the view during the render pass
;;; (om-SBCL/draw.lisp: %render-om-draw-contents), so the camera setup wraps that call in an
;;; :AROUND.  The order inside is the vanilla's: camera, light transform,
;;; object transform, OM-DRAW-CONTENTS of the pane, then the g-object.
;;;
;;; The scene renders DIRECTLY into the window's framebuffer, restricted to the
;;; view's rectangle with glViewport + glScissor.  Alloy clips with the stencil
;;; buffer and never touches the scissor (alloy renderers/opengl/renderer.lisp: the
;;; simple:clip for (renderer simple:shape)),
;;; so the scissor is free.  The rejected alternative was an FBO with a depth
;;; attachment (om-SBCL/icons-picts.lisp: the %om-begin-record/%om-end-record pair):
;;; that pair reads pixels BACK for a
;;; picture, and compositing its texture into the live frame would need a shader
;;; that does not exist here.

(defun %gl-view-pixel-rect (view)
  "The view's rectangle in framebuffer pixels.  Alloy's global position is
already Y-up from the bottom-left, which is also GL's viewport origin, so no
flip is needed (om-SBCL/graphic-object.lisp: %place-subview is the Y-DOWN direction).
*CURRENT-RENDERER* is the window itself (om-SBCL/window.lisp: the defclass
om-look-window makes OM-LOOK-WINDOW a glfw renderer), and om-SBCL/draw.lisp
(%render-om-draw-contents) binds it for the whole
render pass -- which is the only context in which this can be called at all."
  (multiple-value-bind (gx gy)
      (alloy:with-unit-parent view (alloy::compute-global-position view))
    (let ((bounds (alloy:bounds view))
          (ratio (alld:framebuffer-ratio *current-renderer*)))
      (values (round (* ratio gx)) (round (* ratio gy))
              (max 1 (round (* ratio (alloy:pxw bounds))))
              (max 1 (round (* ratio (alloy:pxh bounds))))))))

(defun %gl-view-pixel-clip-rect (view)
  "Intersect VIEW's framebuffer rectangle with the renderer's active clip.
VISIBLE-BOUNDS is the clip stack already established by Alloy and OM scroll
containers.  If it is not finite, keep VIEW's full rectangle."
  (multiple-value-bind (x y w h) (%gl-view-pixel-rect view)
    (let* ((bounds (alloy:visible-bounds *current-renderer*))
           (ratio (alld:framebuffer-ratio *current-renderer*))
           (clip (ignore-errors
                   (list (round (* ratio (alloy:pxx bounds)))
                         (round (* ratio (alloy:pxy bounds)))
                         (max 0 (round (* ratio (alloy:pxw bounds))))
                         (max 0 (round (* ratio (alloy:pxh bounds))))))))
      (if (and clip (every #'integerp clip))
          (destructuring-bind (cx cy cw ch) clip
            (let ((x0 (max x cx))
                  (y0 (max y cy))
                  (x1 (min (+ x w) (+ cx cw)))
                  (y1 (min (+ y h) (+ cy ch))))
              (values x0 y0 (max 0 (- x1 x0)) (max 0 (- y1 y0)))))
          (values x y w h)))))

(defun %draw-camera (camera aspect)
  (immediate-gl:gl-matrix-mode immediate-gl:*gl-projection*)
  (immediate-gl:gl-load-identity)
  (immediate-gl:glu-perspective (fovy (projection camera)) aspect
                                (near (projection camera)) (far (projection camera)))
  (immediate-gl:gl-matrix-mode immediate-gl:*gl-modelview*)
  (immediate-gl:gl-load-identity)
  (let ((eye (eye camera)) (center (center camera)) (up (up camera)))
    (immediate-gl:glu-look-at (xyz-x eye) (xyz-y eye) (xyz-z eye)
                              (xyz-x center) (xyz-y center) (xyz-z center)
                              (xyz-x up) (xyz-y up) (xyz-z up)))
  (when (bgcolor camera)
    (apply #'immediate-gl:gl-clear-color (bgcolor camera)))
  (immediate-gl:gl-clear (logior immediate-gl:*gl-color-buffer-bit*
                                 immediate-gl:*gl-depth-buffer-bit*))
  (immediate-gl:gl-depth-func immediate-gl:*gl-less*)
  (immediate-gl:gl-enable immediate-gl:*gl-depth-test*))

(defun %release-gl-emulation (window)
  "Release WINDOW's immediate-mode objects while its GL context is current."
  (immediate-gl:shutdown-context window))

(defun %forget-gl-emulation (window)
  "Drop WINDOW's immediate-mode context bookkeeping."
  (immediate-gl:forget-context window))

(alld:register-gl-context-cleanup
 'om-gl-emulation
 :release #'%release-gl-emulation
 :forget #'%forget-gl-emulation)

(defmethod om-draw-contents :around ((self om-opengl-view))
  (when (gl-scene-materialized-p self)
    (multiple-value-bind (x y w h) (%gl-view-pixel-rect self)
    (multiple-value-bind (clip-x clip-y clip-w clip-h)
        (%gl-view-pixel-clip-rect self)
      (let ((saved-viewport (gl:get-integer :viewport 4))
            (saved-scissor (gl:enabledp :scissor-test))
            (saved-scissor-box (gl:get-integer :scissor-box 4))
            (saved-depth-func (gl:get-integer :depth-func))
            (saved-program (gl:get-integer :current-program))
            (saved-vao (gl:get-integer :vertex-array-binding))
            (saved-array-buffer (gl:get-integer :array-buffer-binding))
            ;; A VAO cannot cross GL contexts, so the emulation is keyed on the
            ;; renderer -- which IS the window (om-SBCL/window.lisp: the defclass
            ;; om-look-window) and is what
            ;; the window-retirement lifecycle later hands to the cleanup.
            (immediate-gl:*context* *current-renderer*))
        (unwind-protect
             (progn
               (unless (immediate-gl:context-ready-p *current-renderer*)
                 (immediate-gl:init *current-renderer*))
             (gl:scissor clip-x clip-y clip-w clip-h)
             (gl:enable :scissor-test)
             (immediate-gl:gl-viewport x y w h)
             (%draw-camera (camera self) (/ (float w 1.0d0) (max 1.0d0 (float h 1.0d0))))
             (immediate-gl:with-matrix-pushed
               (immediate-gl:gl-mult-matrixd (light-transform self))
               (immediate-gl:gl-light-modelfv immediate-gl:*gl-light-model-ambient*
                                              *light-model-ambient*)
               (immediate-gl:gl-light-modelf immediate-gl:*gl-light-model-local-viewer* 0.0)
               (immediate-gl:gl-light-modelf immediate-gl:*gl-light-model-two-side* 0.0)
               (immediate-gl:gl-enable immediate-gl:*gl-light0*))
             (immediate-gl:with-matrix-pushed
               (immediate-gl:gl-mult-matrixd (icotransform self))
               (immediate-gl:gl-enable immediate-gl:*gl-color-material*)
               (immediate-gl:gl-color-material immediate-gl:*gl-front*
                                               immediate-gl:*gl-ambient-and-diffuse*)
               (call-next-method)
               (when (g-object self)
                 (draw (g-object self)))))
        ;; Hand the pipeline back exactly as Alloy set it up
        ;; (alloy renderers/glfw/renderer.lisp: the initialize-instance :around for renderer).  The depth values this
        ;; scene left behind would fail Alloy's :LEQUAL for anything drawn over
        ;; the same rect later, so the rect's depth goes back to 1.0 first --
        ;; while the scissor still confines the clear.
        (gl:clear :depth-buffer)
        (gl:scissor (aref saved-scissor-box 0) (aref saved-scissor-box 1)
                    (aref saved-scissor-box 2) (aref saved-scissor-box 3))
        (if saved-scissor
            (gl:enable :scissor-test)
            (gl:disable :scissor-test))
        (gl:viewport (aref saved-viewport 0) (aref saved-viewport 1)
                     (aref saved-viewport 2) (aref saved-viewport 3))
        (immediate-gl:gl-depth-func saved-depth-func)
        (gl:use-program saved-program)
        (gl:bind-vertex-array saved-vao)
        (gl:bind-buffer :array-buffer saved-array-buffer)))))))

;;;--------------------------------------------------------------------
;;; Gestures  (opengl-api.lisp: the opengl-viewer-button-1 gesture functions)
;;;--------------------------------------------------------------------
;;; The vanilla routes each modifier combination to its own :input-model entry.
;;; Here OM-VIEW's pointer-down already arms the pointer capture
;;; (om-SBCL/actions.lisp: the alloy:handle for (pointer-down om-view)), so motion and release keep
;;; arriving even outside the pane; only the dispatch has to be written.
;;;
;;; Button-2 belongs to the viewer in vanilla and rotates the light
;;; (opengl-api.lisp: opengl-viewer-button-2 and opengl-viewer-motion-button-2).
;;; Specializing only OM-OPENGL-VIEW preserves the
;;; normal OM-VIEW context-menu route everywhere else.

(defmethod alloy:handle :before ((event alloy:pointer-down) (self om-opengl-view))
  (declare (ignore event))
  (%materialize-opengl-view self))

(defmethod alloy:handle ((event alloy:pointer-down) (self om-opengl-view))
  (if (eq :right (alloy:kind event))
      (let ((position (%alloy-location-to-om-point (alloy:location event) self)))
        (%release-text-focus self)
        (%capture-gesture-pane self)
        (setf (alld:clicked-element (alld:pointer-state)) self
              (lastxy self) (cons (om-point-h position) (om-point-v position))
              (gesture self) :light-rotate)
        t)
      (call-next-method)))

(defmethod om-view-click-handler ((self om-opengl-view) position)
  (setf (lastxy self) (cons (om-point-h position) (om-point-v position)))
  (setf (gesture self) (cond ((om-shift-key-p) :zoom)
                             ((om-option-key-p) :pan)
                             (t :rotate)))
  self)

(defmethod om-view-doubleclick-handler ((self om-opengl-view) position)
  (declare (ignore position))
  (setf (lastxy self) nil (gesture self) nil)
  (om-init-3D-view self)
  self)

(defmethod om-click-release-handler ((self om-opengl-view) position)
  (declare (ignore position))
  (setf (lastxy self) nil (gesture self) nil)
  nil)

(defmethod om-click-motion-handler ((self om-opengl-view) position)
  (let ((last (lastxy self))
        (x (om-point-h position))
        (y (om-point-v position)))
    (when last
      (case (gesture self)
        (:rotate (polar-rotate (icotransform self) (- x (car last)) (- y (cdr last))))
        (:light-rotate
         (polar-rotate (light-transform self) (- x (car last)) (- y (cdr last))))
        (:pan (let ((factor (/ (xyz-y (eye (camera self))) 1500)))
                (translate (icotransform self)
                           (* (- (car last) x) factor) (* (- y (cdr last)) factor))))
        (:zoom (let ((eye (eye (camera self))))
                 (setf (xyz-y eye)
                       (min (- (xyz-y eye)
                               (* (/ (- (cdr last) y) 20) (/ (xyz-y eye) 20)))
                            -0.011d0)))))
      (om-invalidate-view self))
    (setf (lastxy self) (cons x y))
    t))

;;;--------------------------------------------------------------------
;;; OM-3D-OBJECT  (opengl-api.lisp: the defclass om-3D-object and the (setf viewer) :after of om-3D-object-list)
;;;--------------------------------------------------------------------
;;; Legacy display-list names are absent in the core-profile backend. Static OM
;;; curve geometry is compiled to context-local VBOs by the SBCL compatibility
;;; layer; camera, light and object transforms remain dynamic shader uniforms.

(defgeneric draw (object)
  (:documentation "Render OBJECT through the emulated fixed-function pipeline.")
  (:method ((object t)) nil))

(defun activate-anti-aliasing-parameters ()
  (immediate-gl:gl-enable immediate-gl:*gl-blend*)
  (immediate-gl:gl-enable immediate-gl:*gl-line-smooth*)
  (immediate-gl:gl-blend-func immediate-gl:*gl-src-alpha*
                              immediate-gl:*gl-one-minus-src-alpha*)
  (immediate-gl:gl-hint immediate-gl:*gl-line-smooth-hint* immediate-gl:*gl-dont-care*))

(defclass om-3D-object ()
  ((color :accessor color :initarg :color :initform nil)
   (points :initarg :points :accessor points :initform nil)
   (glvertexes :accessor glvertexes :initarg :glvertexes :initform nil)))

(defmethod initialize-instance :after ((self om-3D-object) &key points &allow-other-keys)
  (setf (glvertexes self) (points2vertex points)))

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

(defclass om-3D-object-list ()
  ((objects :accessor objects :initarg :objects :initform nil)))

(defmethod om-get-3D-objects ((self om-3D-object-list))
  (objects self))

(defmethod om-3Dobj-points ((self om-3D-object-list))
  (reduce 'append (mapcar 'om-3Dobj-points (objects self))))

(defmethod draw ((self om-3D-object-list))
  (mapc 'draw (objects self)))
