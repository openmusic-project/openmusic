(in-package #:immediate-gl)

;;;; Errors

(define-condition immediate-gl-error (error)
  ((message :initarg :message :reader immediate-gl-error-message))
  (:report (lambda (condition stream)
             (write-string (immediate-gl-error-message condition) stream))))

(defun igl-error (format &rest args)
  (error 'immediate-gl-error :message (apply #'format NIL format args)))

(defun f (number)
  (float number 1.0f0))

;;;; Float accumulator
;;;; A plain simple-array is required: CFFI:WITH-POINTER-TO-VECTOR-DATA cannot
;;;; pin a vector that has a fill pointer.

(defstruct (fbuf (:constructor make-fbuf
                     (&optional capacity
                      &aux (data (make-array capacity :element-type 'single-float)))))
  (data NIL :type (simple-array single-float (*)))
  (count 0 :type fixnum))

(defun fbuf-clear (buf)
  (setf (fbuf-count buf) 0))

(defun fbuf-ensure (buf size)
  (let ((data (fbuf-data buf)))
    (when (< (length data) size)
      (let ((new (make-array (max size (* 2 (length data))) :element-type 'single-float)))
        (replace new data)
        (setf (fbuf-data buf) new)))))

(defun fbuf-push (buf value)
  (fbuf-ensure buf (1+ (fbuf-count buf)))
  (setf (aref (fbuf-data buf) (fbuf-count buf)) value)
  (incf (fbuf-count buf)))

;;;; State
;;;; Every initialized GL context owns an independent fixed-function state.
;;;; Pure matrix helpers use the scratch state before a context exists.

(defconstant +vertex-floats+ 11)        ; position(4) normal(3) colour(4)
(defconstant +stack-depth+ 32)

(defun %make-stack ()
  (let ((stack (make-array +stack-depth+)))
    (dotimes (i +stack-depth+ stack)
      (setf (aref stack i) (m:meye 4)))))

(defvar *identity* (m:meye 4))

(defun %rgba (r g b a)
  (make-array 4 :element-type 'single-float
                :initial-contents (list (f r) (f g) (f b) (f a))))

(defstruct (fixed-state (:conc-name fixed-state-))
  (matrices (vector (m:meye 4) (m:meye 4)))
  (stacks (vector (%make-stack) (%make-stack)))
  (depths (make-array 2 :element-type 'fixnum :initial-element 0))
  (mode 0 :type fixnum)
  (color (make-array 4 :element-type 'single-float :initial-element 1.0))
  (normal (make-array 3 :element-type 'single-float :initial-element 0.0))
  (line-width 1.0)
  (line-smooth NIL)
  (line-smooth-hint :dont-care)
  (point-size 1.0)
  (point-smooth NIL)
  (lighting NIL)
  (light0 NIL)
  (light-position (%rgba 0 0 1 0))
  (light-ambient (%rgba 0 0 0 1))
  (light-diffuse (%rgba 1 1 1 1))
  (light-specular (%rgba 1 1 1 1))
  (light-model-ambient (%rgba 0.2 0.2 0.2 1))
  (material-specular (%rgba 0 0 0 1))
  (material-emission (%rgba 0 0 0 1))
  (material-shininess 0.0)
  (primitive NIL)
  (vertices (make-fbuf 4096))
  (batch (make-fbuf 4096)))

(defvar *context* :default
  "Key identifying the GL context in force; supplied by the caller.")
(defvar *fixed-states* (make-hash-table :test 'eq)
  "Context key to emulated fixed-function state.")
(defvar *scratch-fixed-state* (make-fixed-state)
  "Pure Lisp fixed-function state used before a GL context has been initialized.")

(defun current-fixed-state ()
  (or (gethash *context* *fixed-states*) *scratch-fixed-state*))


;;;; Fixed-function lighting state.
;;;; Shading needs BOTH GL_LIGHTING and GL_LIGHT0, exactly as in GL 1.x.  All
;;;; initial values below are the OpenGL defaults, so an untouched emulation
;;;; with lighting enabled shades like an untouched GL 1.x pipeline.

(defvar *primitive-modes*
  (list *gl-points* *gl-lines* *gl-line-loop* *gl-line-strip*
        *gl-triangles* *gl-quads* *gl-polygon*))

(defun current-matrix ()
  (let ((state (current-fixed-state)))
    (aref (fixed-state-matrices state)
          (fixed-state-mode state))))

(defun reset-state ()
  "Return the emulated fixed-function state to its OpenGL defaults."
  (let ((state (current-fixed-state)))
    (dotimes (i 2)
      (replace (m:marr4 (aref (fixed-state-matrices state) i))
               (m:marr4 *identity*))
      (setf (aref (fixed-state-depths state) i) 0))
    (setf (fixed-state-mode state) 0)
    (fill (fixed-state-color state) 1.0)
    (fill (fixed-state-normal state) 0.0)
    (setf (fixed-state-line-width state) 1.0
          (fixed-state-line-smooth state) NIL
          (fixed-state-line-smooth-hint state) :dont-care
          (fixed-state-point-size state) 1.0
          (fixed-state-point-smooth state) NIL
          (fixed-state-lighting state) NIL
          (fixed-state-light0 state) NIL
          (fixed-state-primitive state) NIL)
    (replace (fixed-state-light-position state) (%rgba 0 0 1 0))
    (replace (fixed-state-light-ambient state) (%rgba 0 0 0 1))
    (replace (fixed-state-light-diffuse state) (%rgba 1 1 1 1))
    (replace (fixed-state-light-specular state) (%rgba 1 1 1 1))
    (replace (fixed-state-light-model-ambient state)
             (%rgba 0.2 0.2 0.2 1))
    (replace (fixed-state-material-specular state) (%rgba 0 0 0 1))
    (replace (fixed-state-material-emission state) (%rgba 0 0 0 1))
    (setf (fixed-state-material-shininess state) 0.0)
    (fbuf-clear (fixed-state-vertices state))
    (fbuf-clear (fixed-state-batch state))))

;;;; Capabilities

(defun %capability (cap)
  (cond ((eql cap *gl-depth-test*) :depth-test)
        ((eql cap *gl-blend*) :blend)
        ((eql cap *gl-cull-face*) :cull-face)
        ((eql cap *gl-lighting*) :lighting)
        ((eql cap *gl-light0*) :light0)
        ;; GL_LINE_SMOOTH survives into the core profile -- what the profile drops
        ;; is glLineWidth > 1, not line antialiasing -- so it is honoured, in
        ;; %ISSUE.  GL_POINT_SMOOTH is absent from the core profile, so the
        ;; point-sprite shader below emulates its circular coverage.  Normals are
        ;; normalised in that shader as well.
        ((eql cap *gl-line-smooth*) :line-smooth)
        ((eql cap *gl-point-smooth*) :point-smooth)
        ((eql cap *gl-normalize*) :ignore)
        ;; Vertex colour IS the material colour in this shader, which is what
        ;; GL_COLOR_MATERIAL with GL_AMBIENT_AND_DIFFUSE selects; enabling it
        ;; therefore changes nothing here, and disabling it is not emulated.
        ((eql cap *gl-color-material*) :ignore)
        ;; GL_POINT_SIZE is a query enum, not a capability: glEnable of it is a
        ;; GL_INVALID_ENUM that changes no state.  OpenMusic's 3D editor passes
        ;; it anyway (projects 3deditor.lisp: restore-om-gl-colors-and-attributes), so reproducing the
        ;; real pipeline means accepting it and doing nothing -- not erroring.
        ((eql cap *gl-point-size*) :ignore)
        (T (igl-error "Unsupported capability #x~4,'0X." cap))))

(defun %set-capability (cap enabled)
  (let ((key (%capability cap))
        (state (current-fixed-state)))
    (case key
      (:lighting (setf (fixed-state-lighting state) enabled))
      (:light0 (setf (fixed-state-light0 state) enabled))
      (:line-smooth (setf (fixed-state-line-smooth state) enabled))
      (:point-smooth (setf (fixed-state-point-smooth state) enabled))
      (:ignore NIL)
      (T (if enabled (gl:enable key) (gl:disable key))))
    enabled))

(defun gl-enable (cap)
  "Enable the capability CAP, one of the *GL-...* capability constants."
  (%set-capability cap T))

(defun gl-disable (cap)
  "Disable the capability CAP, one of the *GL-...* capability constants."
  (%set-capability cap NIL))

;;;; Matrix stacks

(defun gl-matrix-mode (mode)
  "Select the matrix the matrix operations apply to: *GL-MODELVIEW* or *GL-PROJECTION*."
  (let ((state (current-fixed-state)))
    (setf (fixed-state-mode state)
          (cond ((eql mode *gl-modelview*) 0)
                ((eql mode *gl-projection*) 1)
                ((eql mode *gl-texture*)
                 (igl-error "GL-MATRIX-MODE: the texture matrix is not emulated."))
                (T
                 (igl-error "GL-MATRIX-MODE: unknown matrix mode #x~4,'0X."
                            mode))))))

(defun gl-load-identity ()
  "Replace the current matrix with the identity matrix."
  (replace (m:marr4 (current-matrix)) (m:marr4 *identity*))
  (current-matrix))

(defun gl-push-matrix ()
  "Push a copy of the current matrix onto its stack."
  (let* ((state (current-fixed-state))
         (mode (fixed-state-mode state))
         (depth (aref (fixed-state-depths state) mode)))
    (when (<= +stack-depth+ depth)
      (igl-error "GL-PUSH-MATRIX: matrix stack overflow at depth ~D." depth))
    (replace (m:marr4 (aref (aref (fixed-state-stacks state) mode) depth))
             (m:marr4 (current-matrix)))
    (setf (aref (fixed-state-depths state) mode) (1+ depth))))

(defun gl-pop-matrix ()
  "Restore the current matrix from the top of its stack."
  (let* ((state (current-fixed-state))
         (mode (fixed-state-mode state))
         (depth (1- (aref (fixed-state-depths state) mode))))
    (when (minusp depth)
      (igl-error "GL-POP-MATRIX: matrix stack underflow."))
    (replace (m:marr4 (current-matrix))
             (m:marr4 (aref (aref (fixed-state-stacks state) mode) depth)))
    (setf (aref (fixed-state-depths state) mode) depth)))

(defmacro with-matrix-pushed (&body body)
  "Run BODY between GL-PUSH-MATRIX and GL-POP-MATRIX, after opengl-lw/capi.lisp."
  `(unwind-protect
        (progn (gl-push-matrix) ,@body)
     (gl-pop-matrix)))

(defun gl-get-doublev (pname array)
  "Read the current MODELVIEW or PROJECTION matrix into ARRAY, sixteen elements in
OpenGL column-major order.  Only those two names are emulated: the rest of the GL
state lives in the driver, not here, and reading it back would return the core
profile's answer rather than the emulated one."
  (let ((mode (cond ((eql pname *gl-modelview-matrix*) 0)
                    ((eql pname *gl-projection-matrix*) 1)
                    (T
                     (igl-error
                      "GL-GET-DOUBLEV: #x~4,'0X is not an emulated state name."
                      pname)))))
    (unless (= 16 (length array))
      (igl-error "GL-GET-DOUBLEV: expected a 16-element array, got ~D."
                 (length array)))
    (let* ((state (current-fixed-state))
           (source (m:marr4
                    (aref (fixed-state-matrices state) mode))))
      (dotimes (col 4)
        (dotimes (row 4)
          (setf (elt array (+ (* 4 col) row))
                (float (aref source (+ (* 4 row) col)) 1.0d0)))))
    array))

(defun gl-translated (x y z)
  "Multiply the current matrix by a translation of (X Y Z)."
  (m:nmtranslate (current-matrix) (m:vec (f x) (f y) (f z))))

(defun gl-scaled (x y z)
  "Multiply the current matrix by a scaling of (X Y Z)."
  (m:nmscale (current-matrix) (m:vec (f x) (f y) (f z))))

(defun gl-rotated (angle x y z)
  "Multiply the current matrix by a rotation of ANGLE degrees around the axis (X Y Z)."
  (if (and (zerop x) (zerop y) (zerop z))
      (current-matrix)
      (m:nmrotate (current-matrix) (m:vec (f x) (f y) (f z))
                  (f (* angle (/ pi 180))))))

(defun gl-mult-matrixd (matrix)
  "Multiply the current matrix by MATRIX, sixteen numbers in OpenGL column-major order."
  (unless (= 16 (length matrix))
    (igl-error "GL-MULT-MATRIXD: expected 16 elements, got ~D." (length matrix)))
  (let* ((other (m:meye 4))
         (array (m:marr4 other)))
    (dotimes (col 4)
      (dotimes (row 4)
        (setf (aref array (+ (* 4 row) col))
              (f (elt matrix (+ (* 4 col) row))))))
    (m:nm* (current-matrix) other)))

(defun glu-perspective (fovy aspect z-near z-far)
  "Multiply the current matrix by a perspective projection; FOVY is in degrees."
  (m:nm* (current-matrix)
         (m:nmperspective (m:meye 4) (f fovy) (f aspect) (f z-near) (f z-far))))

(defun glu-look-at (eye-x eye-y eye-z center-x center-y center-z up-x up-y up-z)
  "Multiply the current matrix by a viewing transform for an eye at EYE-* looking at CENTER-*."
  (m:nm* (current-matrix)
         (m:nmlookat (m:meye 4)
                     (m:vec (f eye-x) (f eye-y) (f eye-z))
                     (m:vec (f center-x) (f center-y) (f center-z))
                     (m:vec (f up-x) (f up-y) (f up-z)))))

;;;; Immediate mode

(defun gl-color4-f (red green blue alpha)
  "Set the current vertex colour."
  (let ((state (current-fixed-state)))
    (setf (aref (fixed-state-color state) 0) (f red)
          (aref (fixed-state-color state) 1) (f green)
          (aref (fixed-state-color state) 2) (f blue)
          (aref (fixed-state-color state) 3) (f alpha))))

(defun gl-color3-f (red green blue)
  "Set the current vertex colour, with an alpha of one."
  (gl-color4-f red green blue 1))

(defun %check-length (name wanted sequence)
  (unless (= wanted (length sequence))
    (igl-error "~A: expected ~D elements, got ~D." name wanted (length sequence)))
  sequence)

(defun gl-color4-fv (vector)
  "Set the current vertex colour from VECTOR, a sequence of four components."
  (%check-length "GL-COLOR4-FV" 4 vector)
  (gl-color4-f (elt vector 0) (elt vector 1) (elt vector 2) (elt vector 3)))

(defun gl-color3-fv (vector)
  "Set the current vertex colour from VECTOR, a sequence of three components."
  (%check-length "GL-COLOR3-FV" 3 vector)
  (gl-color4-f (elt vector 0) (elt vector 1) (elt vector 2) 1))

(defun gl-normal3-f (nx ny nz)
  "Set the current vertex normal."
  (let ((state (current-fixed-state)))
    (setf (aref (fixed-state-normal state) 0) (f nx)
          (aref (fixed-state-normal state) 1) (f ny)
          (aref (fixed-state-normal state) 2) (f nz))))

(defun gl-normal3-i (nx ny nz)
  "Set the current vertex normal from three integers."
  (gl-normal3-f nx ny nz))

(defun gl-normal3-fv (vector)
  "Set the current vertex normal from VECTOR, a sequence of three components."
  (%check-length "GL-NORMAL3-FV" 3 vector)
  (gl-normal3-f (elt vector 0) (elt vector 1) (elt vector 2)))

(defun gl-normal3-dv (vector)
  "Set the current vertex normal from VECTOR, a sequence of three components."
  (%check-length "GL-NORMAL3-DV" 3 vector)
  (gl-normal3-f (elt vector 0) (elt vector 1) (elt vector 2)))

(defun gl-line-width (width)
  "Set the line width in pixels; a width above one is emulated by quad expansion."
  (let ((state (current-fixed-state)))
    (setf (fixed-state-line-width state) (f width))))

(defun gl-point-size (size)
  "Set the point size in pixels."
  (let ((state (current-fixed-state)))
    (setf (fixed-state-point-size state) (f size))))

(defun gl-begin (mode)
  "Open a primitive block of MODE; vertices accumulate until GL-END draws them."
  (let ((state (current-fixed-state)))
    (when (fixed-state-primitive state)
      (igl-error "GL-BEGIN: a block of mode #x~4,'0X is already open."
                 (fixed-state-primitive state)))
    (unless (member mode *primitive-modes*)
      (igl-error "GL-BEGIN: unsupported primitive mode #x~4,'0X." mode))
    (fbuf-clear (fixed-state-vertices state))
    (setf (fixed-state-primitive state) mode)))

(defun %vertex (x y z w)
  (let ((state (current-fixed-state)))
    (unless (fixed-state-primitive state)
      (igl-error "A vertex was emitted outside of a GL-BEGIN block."))
    (fbuf-push (fixed-state-vertices state) (f x))
    (fbuf-push (fixed-state-vertices state) (f y))
    (fbuf-push (fixed-state-vertices state) (f z))
    (fbuf-push (fixed-state-vertices state) (f w))
    (dotimes (i 3)
      (fbuf-push (fixed-state-vertices state)
                 (aref (fixed-state-normal state) i)))
    (dotimes (i 4)
      (fbuf-push (fixed-state-vertices state)
                 (aref (fixed-state-color state) i)))))

(defun gl-vertex3-d (x y z)
  "Emit a vertex at (X Y Z) with the current colour and normal."
  (%vertex x y z 1))

(defun gl-vertex3-f (x y z)
  "Emit a vertex at (X Y Z) with the current colour and normal."
  (%vertex x y z 1))

(defun gl-vertex3-dv (vector)
  "Emit a vertex from VECTOR, a sequence of three coordinates."
  (%check-length "GL-VERTEX3-DV" 3 vector)
  (%vertex (elt vector 0) (elt vector 1) (elt vector 2) 1))

(defun gl-vertex3-fv (vector)
  "Emit a vertex from VECTOR, a sequence of three coordinates."
  (%check-length "GL-VERTEX3-FV" 3 vector)
  (%vertex (elt vector 0) (elt vector 1) (elt vector 2) 1))

(defun gl-vertex4-dv (vector)
  "Emit a vertex from VECTOR, a sequence of four homogeneous coordinates."
  (%check-length "GL-VERTEX4-DV" 4 vector)
  (%vertex (elt vector 0) (elt vector 1) (elt vector 2) (elt vector 3)))

(defun gl-end ()
  "Close the current primitive block and draw it."
  (let ((state (current-fixed-state)))
    (unless (fixed-state-primitive state)
      (igl-error "GL-END without a matching GL-BEGIN."))
    (let ((mode (fixed-state-primitive state)))
      (setf (fixed-state-primitive state) NIL)
      (%draw mode))))

(defun flush ()
  "Force the GL command queue out. Primitives are drawn by GL-END, not buffered."
  (let ((state (current-fixed-state)))
    (when (fixed-state-primitive state)
      (igl-error "FLUSH was called inside an open GL-BEGIN block."))
    (gl:flush)))

;;;; Framebuffer and raster state
;;;; These have core-profile equivalents, so they are passed straight through.

(defun gl-viewport (x y width height)
  "Set the viewport rectangle, in framebuffer pixels."
  (gl:viewport (round x) (round y) (round width) (round height)))

(defun gl-clear-color (red green blue alpha)
  "Set the colour the colour buffer is cleared to."
  (gl:clear-color (f red) (f green) (f blue) (f alpha)))

(defun gl-clear (mask)
  "Clear the buffers named by MASK, an ior of the *GL-...-BUFFER-BIT* constants."
  (let ((buffers '()))
    (when (logtest mask *gl-stencil-buffer-bit*) (push :stencil-buffer buffers))
    (when (logtest mask *gl-depth-buffer-bit*) (push :depth-buffer buffers))
    (when (logtest mask *gl-color-buffer-bit*) (push :color-buffer buffers))
    (unless buffers
      (igl-error "GL-CLEAR: mask #x~X names no buffer." mask))
    (apply #'gl:clear buffers)))

(defun %enum (name table value)
  (or (cdr (assoc value table))
      (igl-error "~A: #x~4,'0X is not one of the accepted values." name value)))

(defun gl-depth-func (func)
  "Set the depth comparison function."
  (gl:depth-func
   (%enum "GL-DEPTH-FUNC"
          (list (cons *gl-never* :never) (cons *gl-less* :less)
                (cons *gl-equal* :equal) (cons *gl-lequal* :lequal)
                (cons *gl-greater* :greater) (cons *gl-notequal* :notequal)
                (cons *gl-gequal* :gequal) (cons *gl-always* :always))
          func)))

(defun %blend-factor (value)
  (%enum "GL-BLEND-FUNC"
         (list (cons *gl-zero* :zero) (cons *gl-one* :one)
               (cons *gl-src-alpha* :src-alpha)
               (cons *gl-one-minus-src-alpha* :one-minus-src-alpha)
               (cons *gl-dst-alpha* :dst-alpha)
               (cons *gl-one-minus-dst-alpha* :one-minus-dst-alpha))
         value))

(defun gl-blend-func (source destination)
  "Set the source and destination blending factors."
  (gl:blend-func (%blend-factor source) (%blend-factor destination)))

(defun gl-polygon-mode (face mode)
  "Select point, line or fill rasterisation for FACE, which must be *GL-FRONT-AND-BACK*."
  (unless (eql face *gl-front-and-back*)
    (igl-error "GL-POLYGON-MODE: the core profile only accepts GL_FRONT_AND_BACK."))
  (gl:polygon-mode :front-and-back
                   (%enum "GL-POLYGON-MODE"
                          (list (cons *gl-point* :point) (cons *gl-line* :line)
                                (cons *gl-fill* :fill))
                          mode)))

;;;; Accepted, but with no effect -- each for a stated reason, never silently.

(defun gl-hint (target mode)
  "The LINE hint is kept and handed to GL in %ISSUE, since GL_LINE_SMOOTH is
honoured; the POINT and POLYGON hints steer smoothing the core profile does not
have, so those two are accepted and ignored."
  (let ((which (%enum "GL-HINT" (list (cons *gl-point-smooth-hint* :point)
                                      (cons *gl-line-smooth-hint* :line)
                                      (cons *gl-polygon-smooth-hint* :polygon))
                      target))
        (quality (%enum "GL-HINT" (list (cons *gl-dont-care* :dont-care)
                                        (cons *gl-fastest* :fastest)
                                        (cons *gl-nicest* :nicest))
                        mode))
        (state (current-fixed-state)))
    (when (eq which :line)
      (setf (fixed-state-line-smooth-hint state) quality))
    NIL))

(defun gl-shade-model (mode)
  "Accepted and ignored: this emulation interpolates colour across every
primitive.  GL_FLAT would need a second shader program with a flat-qualified
colour, which no consumer asks for; a GL_FLAT scene renders smooth here."
  (%enum "GL-SHADE-MODEL"
         (list (cons *gl-flat* :flat) (cons *gl-smooth* :smooth))
         mode)
  NIL)

(defun gl-color-material (face mode)
  "Accepted and ignored: the vertex colour always drives the ambient and diffuse
material terms of the shader, which is what GL_AMBIENT_AND_DIFFUSE selects."
  (%enum "GL-COLOR-MATERIAL"
         (list (cons *gl-front* :front) (cons *gl-back* :back)
               (cons *gl-front-and-back* :front-and-back))
         face)
  (%enum "GL-COLOR-MATERIAL"
         (list (cons *gl-ambient* :ambient) (cons *gl-diffuse* :diffuse)
               (cons *gl-specular* :specular) (cons *gl-emission* :emission)
               (cons *gl-ambient-and-diffuse* :ambient-and-diffuse))
         mode)
  NIL)

;;;; Lights and materials

(defun %store-rgba (place vector name)
  (%check-length name 4 vector)
  (dotimes (i 4 place)
    (setf (aref place i) (f (elt vector i)))))

(defun %transform-point (matrix x y z w)
  "MATRIX times the column (X Y Z W); MARR4 is row-major."
  (let ((a (m:marr4 matrix)))
    (flet ((row (r) (+ (* (aref a (* 4 r)) x)
                       (* (aref a (+ (* 4 r) 1)) y)
                       (* (aref a (+ (* 4 r) 2)) z)
                       (* (aref a (+ (* 4 r) 3)) w))))
      (list (row 0) (row 1) (row 2) (row 3)))))

(defun %light0 (light name)
  (unless (eql light *gl-light0*)
    (igl-error "~A: only GL_LIGHT0 is emulated (got #x~4,'0X)." name light))
  T)

(defun gl-lightfv (light pname params)
  "Set a parameter of GL_LIGHT0 from PARAMS, a sequence of four components.
GL_POSITION is transformed by the current modelview matrix, as in GL 1.x."
  (%light0 light "GL-LIGHTFV")
  (let ((state (current-fixed-state)))
    (cond ((eql pname *gl-position*)
           (%check-length "GL-LIGHTFV" 4 params)
           (%store-rgba
            (fixed-state-light-position state)
            (%transform-point (aref (fixed-state-matrices state) 0)
                              (f (elt params 0)) (f (elt params 1))
                              (f (elt params 2)) (f (elt params 3)))
            "GL-LIGHTFV"))
          ((eql pname *gl-ambient*)
           (%store-rgba (fixed-state-light-ambient state)
                        params "GL-LIGHTFV"))
          ((eql pname *gl-diffuse*)
           (%store-rgba (fixed-state-light-diffuse state)
                        params "GL-LIGHTFV"))
          ((eql pname *gl-specular*)
           (%store-rgba (fixed-state-light-specular state)
                        params "GL-LIGHTFV"))
          (T
           (igl-error "GL-LIGHTFV: parameter #x~4,'0X is not emulated."
                      pname)))))

(defun gl-lightf (light pname param)
  "Accepted and ignored: the scalar light parameters are the distance attenuation
factors and the spotlight cutoff, and this emulation has neither."
  (declare (ignore pname param))
  (%light0 light "GL-LIGHTF")
  NIL)

(defun gl-materialfv (face pname params)
  "Set a material parameter from PARAMS, a sequence of four components.  FACE is
accepted and ignored: the shading here is two-sided."
  (declare (ignore face))
  (let ((state (current-fixed-state)))
    (cond ((eql pname *gl-specular*)
           (%store-rgba (fixed-state-material-specular state)
                        params "GL-MATERIALFV"))
          ((eql pname *gl-emission*)
           (%store-rgba (fixed-state-material-emission state)
                        params "GL-MATERIALFV"))
          ;; ambient and diffuse come from the vertex colour, see GL-COLOR-MATERIAL
          ((or (eql pname *gl-ambient*) (eql pname *gl-diffuse*)
               (eql pname *gl-ambient-and-diffuse*))
           NIL)
          (T
           (igl-error "GL-MATERIALFV: parameter #x~4,'0X is not emulated."
                      pname)))))

(defun gl-materialf (face pname param)
  "Set a scalar material parameter; only GL_SHININESS is emulated."
  (declare (ignore face))
  (let ((state (current-fixed-state)))
    (cond ((eql pname *gl-shininess*)
           (setf (fixed-state-material-shininess state) (f param)))
          (T
           (igl-error "GL-MATERIALF: parameter #x~4,'0X is not emulated."
                      pname)))))

(defun gl-light-modelfv (pname params)
  "Set the global ambient light from PARAMS, a sequence of four components."
  (let ((state (current-fixed-state)))
    (cond ((eql pname *gl-light-model-ambient*)
           (%store-rgba (fixed-state-light-model-ambient state)
                        params "GL-LIGHT-MODELFV"))
          (T
           (igl-error "GL-LIGHT-MODELFV: parameter #x~4,'0X is not emulated."
                      pname)))))

(defun gl-light-modelf (pname param)
  "Accepted and ignored.  GL_LIGHT_MODEL_TWO_SIDE has no effect because this
shader is always two-sided (it lights with the absolute value of the dot
product, so a mesh seen from behind is never black); GL_LIGHT_MODEL_LOCAL_VIEWER
has none because the specular half-vector is always computed per fragment."
  (declare (ignore param))
  (%enum "GL-LIGHT-MODELF"
         (list (cons *gl-light-model-two-side* :two-side)
               (cons *gl-light-model-local-viewer* :local-viewer))
         pname)
  NIL)

;;;; Primitive assembly

(defun %copy-vertex (index &optional dx dy dz)
  "Append vertex INDEX of the accumulator to the batch, overriding its normal with
the direction (DX DY DZ) when one is given."
  (let* ((state (current-fixed-state))
         (vertices (fixed-state-vertices state))
         (batch (fixed-state-batch state))
         (source (fbuf-data vertices))
         (base (* index +vertex-floats+))
         (at (fbuf-count batch)))
    (fbuf-ensure batch (+ at +vertex-floats+))
    (let ((target (fbuf-data batch)))
      (replace target source
               :start1 at :end1 (+ at +vertex-floats+)
               :start2 base :end2 (+ base +vertex-floats+))
      (when dx
        (setf (aref target (+ at 4)) dx
              (aref target (+ at 5)) dy
              (aref target (+ at 6)) dz)))
    (setf (fbuf-count batch) (+ at +vertex-floats+))))

(defun %copy-vertices (count)
  (dotimes (i count)
    (%copy-vertex i)))

(defun %emit-quads (count)
  "Two triangles per quad; the core profile has no GL_QUADS."
  (loop for base from 0 below (- count 3) by 4
        do (%copy-vertex base)
           (%copy-vertex (+ base 1))
           (%copy-vertex (+ base 2))
           (%copy-vertex base)
           (%copy-vertex (+ base 2))
           (%copy-vertex (+ base 3))))

(defun %emit-fan (count)
  "A triangle fan around vertex zero; correct for the convex polygons GL_POLYGON is used for."
  (loop for i from 1 below (1- count)
        do (%copy-vertex 0)
           (%copy-vertex i)
           (%copy-vertex (1+ i))))

(defun %segments (mode count)
  (cond ((eql mode *gl-lines*)
         (loop for i from 0 below (1- count) by 2 collect (cons i (1+ i))))
        ((eql mode *gl-line-strip*)
         (loop for i from 0 below (1- count) collect (cons i (1+ i))))
        (T
         (append (loop for i from 0 below (1- count) collect (cons i (1+ i)))
                 (when (< 1 count) (list (cons (1- count) 0)))))))

(defun %emit-thick-segment (a b)
  "Six vertices forming the screen-space quad of the segment A-B, after trial/lines.lisp."
  (let* ((state (current-fixed-state))
         (source (fbuf-data (fixed-state-vertices state)))
         (ai (* a +vertex-floats+))
         (bi (* b +vertex-floats+))
         (dx (- (aref source ai) (aref source bi)))
         (dy (- (aref source (+ ai 1)) (aref source (+ bi 1))))
         (dz (- (aref source (+ ai 2)) (aref source (+ bi 2)))))
    (%copy-vertex a dx dy dz)
    (%copy-vertex b dx dy dz)
    (%copy-vertex a (- dx) (- dy) (- dz))
    (%copy-vertex b dx dy dz)
    (%copy-vertex b (- dx) (- dy) (- dz))
    (%copy-vertex a (- dx) (- dy) (- dz))))

(defvar *batch-sink* nil)

(defun %submit (primitive line-mode)
  (if *batch-sink*
      (funcall *batch-sink* primitive line-mode)
      (%issue primitive line-mode)))

(defun %emit-lines (mode count)
  (let ((segments (%segments mode count))
        (state (current-fixed-state)))
    (cond ((<= (fixed-state-line-width state) 1.0)
           (loop for (a . b) in segments
                 do (%copy-vertex a) (%copy-vertex b))
           (%submit :lines NIL))
          (T
           (loop for (a . b) in segments
                 do (%emit-thick-segment a b))
           (%submit :triangles T)))))

(defun %draw (mode)
  (let* ((state (current-fixed-state))
         (vertices (fixed-state-vertices state))
         (batch (fixed-state-batch state))
         (count (floor (fbuf-count vertices) +vertex-floats+)))
    (when (plusp count)
      (fbuf-clear batch)
      (cond ((eql mode *gl-points*)
             (%copy-vertices count)
             (%submit :points NIL))
            ((eql mode *gl-triangles*)
             (%copy-vertices (* 3 (floor count 3)))
             (%submit :triangles NIL))
            ((eql mode *gl-quads*)
             (%emit-quads count)
             (%submit :triangles NIL))
            ((eql mode *gl-polygon*)
             (%emit-fan count)
             (%submit :triangles NIL))
            (T
             (%emit-lines mode count))))))

;;;; GL resources

(defvar *vertex-shader* "#version 330 core
layout (location = 0) in vec4 position;
layout (location = 1) in vec3 aux;
layout (location = 2) in vec4 color;

uniform mat4 modelview;
uniform mat4 projection;
uniform int mode;
uniform float line_width;
uniform float point_size;
uniform vec2 view_size;

out vec4 v_color;
out vec3 v_eye;
out vec3 v_normal;

void main(){
  vec4 eye = modelview * position;
  v_eye = eye.xyz;
  v_color = color;
  gl_PointSize = mode == 2 ? point_size + 1.41421356 : point_size;
  if (mode == 1){
    float aspect = view_size.x / view_size.y;
    vec4 s1 = projection * eye;
    vec4 s2 = projection * (modelview * vec4(position.xyz + aux, position.w));
    vec2 c1 = s1.xy / max(0.00001, s1.w);
    vec2 c2 = s2.xy / max(0.00001, s2.w);
    c1.x *= aspect;
    c2.x *= aspect;
    vec2 n = normalize(c2 - c1);
    n = vec2(-n.y, n.x);
    v_normal = vec3(0.0, 0.0, 1.0);
    gl_Position = s1 + s1.w * vec4(n * line_width / view_size, 0.0, 0.0);
  } else {
    v_normal = mat3(modelview) * aux;
    gl_Position = projection * eye;
  }
}
")

(defvar *fragment-shader* "#version 330 core
in vec4 v_color;
in vec3 v_eye;
in vec3 v_normal;

uniform float lighting;         // GL_LIGHTING and GL_LIGHT0 both on
uniform vec4 light_pos;         // eye space; w == 0 is a directional light
uniform vec3 light_ambient;     // GL_LIGHT_MODEL_AMBIENT + GL_LIGHT0's GL_AMBIENT
uniform vec3 light_diffuse;
uniform vec3 light_specular;
uniform vec3 mat_specular;
uniform vec3 mat_emission;
uniform float shininess;
uniform int mode;
uniform float point_size;

out vec4 out_color;

vec3 safe_normalize(vec3 v, vec3 fallback){
  return dot(v, v) > 1e-12 ? normalize(v) : fallback;
}

void main(){
  // The derivatives must be taken outside the branch: control flow that is not
  // uniform across the quad makes them undefined.
  vec3 face = cross(dFdx(v_eye), dFdy(v_eye));
  vec3 rgb = v_color.rgb;
  if (lighting > 0.5){
    vec3 n = dot(v_normal, v_normal) > 1e-6 ? normalize(v_normal)
           : safe_normalize(face, vec3(0.0, 0.0, 1.0));
    vec3 l = light_pos.w > 0.5 ? safe_normalize(light_pos.xyz - v_eye, vec3(0.0, 0.0, 1.0))
                               : safe_normalize(light_pos.xyz, vec3(0.0, 0.0, 1.0));
    // abs(): two-sided, so a mesh seen from behind is not black.
    float diffuse = abs(dot(n, l));
    float spec = 0.0;
    if (dot(mat_specular, mat_specular) > 0.0){
      vec3 eye = safe_normalize(-v_eye, vec3(0.0, 0.0, 1.0));
      spec = pow(abs(dot(n, safe_normalize(l + eye, vec3(0.0, 0.0, 1.0)))),
                 max(1.0, shininess));
    }
    rgb = mat_emission
        + v_color.rgb * (light_ambient + light_diffuse * diffuse)
        + mat_specular * light_specular * spec;
  }
  float alpha = v_color.a;
  if (mode == 2){
    float radius = point_size / (2.0 * (point_size + 1.41421356));
    float d = length(gl_PointCoord - vec2(0.5));
    float coverage = 1.0 - smoothstep(radius, radius + 0.5 * fwidth(d), d);
    if (coverage <= 0.0) discard;
    alpha *= coverage;
  }
  out_color = vec4(clamp(rgb, 0.0, 1.0), alpha);
}
")

;;;; ALL GL state is PER CONTEXT: VAO, VBO and program.
;;;; A VAO is a container object the GL spec never shares.  A buffer and a
;;;; program are shared, but only inside a share group and only while that group
;;;; lives: destroy the window that owns the group and the objects go with it,
;;;; leaving dangling names.  Keeping them global therefore only works in the
;;;; narrow case "one share group that outlives every window", and fails as a
;;;; late, silent GL_INVALID_OPERATION everywhere else -- including a consumer
;;;; that opens unshared windows at all.  One program and one ~4 KB buffer per
;;;; context is the price of not having that class of bug.

(defstruct (glres (:conc-name res-))
  vao vbo program (uniforms (make-hash-table :test 'equal)) (static-batches nil))

(defstruct (static-batch (:conc-name static-batch-))
  context vao vbo primitive count line-mode)

;;;; One VAO PER GL CONTEXT.
;;;; A VAO is a container object and the GL spec excludes it from sharing, so a
;;;; name generated in one context raises GL_INVALID_OPERATION when bound in
;;;; another -- even between contexts of the same share group, which DO share
;;;; the buffer and the program.  The library cannot know what a "context" is
;;;; for its host, so the caller supplies the key: bind *CONTEXT* to whatever
;;;; identifies the current context (a window, a renderer, anything compared by
;;;; EQ) and call INIT once per context.
(defvar *contexts* (make-hash-table :test 'eq)
  "context key -> GLRES.")

(defun current-resources ()
  (or (gethash *context* *contexts*)
      (igl-error "IMMEDIATE-GL:INIT has not been called for context ~S." *context*)))

(defun %compile-shader (type source)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (unless (gl:get-shader shader :compile-status)
      (let ((log (gl:get-shader-info-log shader)))
        (gl:delete-shader shader)
        (igl-error "Failed to compile the ~(~A~):~%~A" type log)))
    shader))

(defun %make-program (vertex-source fragment-source)
  (let ((vertex (%compile-shader :vertex-shader vertex-source))
        (fragment NIL))
    (unwind-protect
         (progn
           (setf fragment (%compile-shader :fragment-shader fragment-source))
           (let ((program (gl:create-program)))
             (handler-case
                 (progn
                   (gl:attach-shader program vertex)
                   (gl:attach-shader program fragment)
                   (gl:link-program program)
                   (gl:detach-shader program vertex)
                   (gl:detach-shader program fragment)
                   (unless (gl:get-program program :link-status)
                     (igl-error "Failed to link the emulation program:~%~A"
                                (gl:get-program-info-log program)))
                   program)
               (serious-condition (condition)
                 (gl:delete-program program)
                 (error condition)))))
      (when fragment (gl:delete-shader fragment))
      (gl:delete-shader vertex))))

(defun %uniform (name)
  (let ((res (current-resources)))
    (or (gethash name (res-uniforms res))
        (setf (gethash name (res-uniforms res))
              (gl:get-uniform-location (res-program res) name)))))

(defun %make-vao (vbo)
  "A VAO wired to VBO with this emulation's vertex layout, in the CURRENT context."
  (let ((vao (gl:gen-vertex-array)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (let ((stride (* 4 +vertex-floats+)))
      (gl:vertex-attrib-pointer 0 4 :float NIL stride 0)
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 1 3 :float NIL stride 16)
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 2 4 :float NIL stride 28)
      (gl:enable-vertex-attrib-array 2))
    (gl:bind-vertex-array 0)
    (gl:bind-buffer :array-buffer 0)
    vao))

(defun init (&optional (context *context*))
  "Create this emulation's GL objects for CONTEXT; that GL 3.3 core context must
be CURRENT.  Call once per context, and SHUTDOWN-CONTEXT when it closes.
Program, buffer and VAO are all made per context -- see the note above the
GLRES definition for why sharing them is not safe."
  (unless (gethash context *contexts*)
    (setf (gethash context *fixed-states*) (make-fixed-state))
    (handler-case
        (let ((*context* context))
          (reset-state)
          (let* ((program (%make-program *vertex-shader* *fragment-shader*))
                 (vbo (gl:gen-buffer))
                 (vao (%make-vao vbo)))
            (setf (gethash context *contexts*)
                  (make-glres :vao vao :vbo vbo :program program))))
      (serious-condition (condition)
        (remhash context *fixed-states*)
        (error condition))))
  (gl:enable :program-point-size)
  (res-program (gethash context *contexts*)))

(defun context-ready-p (&optional (context *context*))
  "True when INIT has already run for CONTEXT."
  (and (gethash context *contexts*) t))

(defun shutdown-context (context)
  "Delete every GL object of CONTEXT and forget it.  CONTEXT must be the CURRENT
GL context: glDelete* silently ignores names of another context, which would
leak them.  When the context is already destroyed its objects died with it --
call FORGET-CONTEXT instead, and do not touch GL."
  (let ((res (gethash context *contexts*)))
    (when res
      (unwind-protect
           (progn
             (dolist (batch (res-static-batches res))
               (gl:delete-vertex-arrays (list (static-batch-vao batch)))
               (gl:delete-buffers (list (static-batch-vbo batch))))
             (gl:delete-vertex-arrays (list (res-vao res)))
             (gl:delete-buffers (list (res-vbo res)))
             (gl:delete-program (res-program res))
             t)
        (remhash context *contexts*)
        (remhash context *fixed-states*)))))

(defun forget-context (context)
  "Drop CONTEXT's registration WITHOUT calling GL: for a context that is already
gone, whose objects went with it.  Deleting them would be a no-op at best and a
delete against the wrong context at worst."
  (prog1 (remhash context *contexts*)
    (remhash context *fixed-states*)))

(defun live-context-count ()
  "How many contexts hold GL objects; the leak oracle for a host that opens and
closes windows."
  (hash-table-count *contexts*))

(defun shutdown ()
  "Delete the objects of the CURRENT context.  Other contexts must each be made
current and passed to SHUTDOWN-CONTEXT: GL gives no way to delete from here."
  (shutdown-context *context*))

(defun %draw-vao (vao primitive line-mode count)
  "Draw COUNT vertices from VAO with the current matrices and fixed-function state."
  (let ((res (current-resources))
        (viewport (gl:get* :viewport)))
    (when (plusp count)
      (gl:use-program (res-program res))
      (let ((state (current-fixed-state)))
        (gl:uniform-matrix-4fv
         (%uniform "modelview")
         (m:marr4 (aref (fixed-state-matrices state) 0))
         T)
        (gl:uniform-matrix-4fv
         (%uniform "projection")
         (m:marr4 (aref (fixed-state-matrices state) 1))
         T)
        (%gl:uniform-1i (%uniform "mode")
                        (cond (line-mode 1)
                              ((and (eq primitive :points)
                                    (fixed-state-point-smooth state))
                               2)
                              (t 0)))
        (%gl:uniform-1f (%uniform "line_width")
                        (fixed-state-line-width state))
        (%gl:uniform-1f (%uniform "point_size")
                        (fixed-state-point-size state))
        (%gl:uniform-1f (%uniform "lighting")
                        (if (and (fixed-state-lighting state)
                                 (fixed-state-light0 state))
                            1.0
                            0.0))
        (%gl:uniform-4f (%uniform "light_pos")
                        (aref (fixed-state-light-position state) 0)
                        (aref (fixed-state-light-position state) 1)
                        (aref (fixed-state-light-position state) 2)
                        (aref (fixed-state-light-position state) 3))
        (%gl:uniform-3f (%uniform "light_ambient")
                        (+ (aref (fixed-state-light-model-ambient state) 0)
                           (aref (fixed-state-light-ambient state) 0))
                        (+ (aref (fixed-state-light-model-ambient state) 1)
                           (aref (fixed-state-light-ambient state) 1))
                        (+ (aref (fixed-state-light-model-ambient state) 2)
                           (aref (fixed-state-light-ambient state) 2)))
        (%gl:uniform-3f (%uniform "light_diffuse")
                        (aref (fixed-state-light-diffuse state) 0)
                        (aref (fixed-state-light-diffuse state) 1)
                        (aref (fixed-state-light-diffuse state) 2))
        (%gl:uniform-3f (%uniform "light_specular")
                        (aref (fixed-state-light-specular state) 0)
                        (aref (fixed-state-light-specular state) 1)
                        (aref (fixed-state-light-specular state) 2))
        (%gl:uniform-3f (%uniform "mat_specular")
                        (aref (fixed-state-material-specular state) 0)
                        (aref (fixed-state-material-specular state) 1)
                        (aref (fixed-state-material-specular state) 2))
        (%gl:uniform-3f (%uniform "mat_emission")
                        (aref (fixed-state-material-emission state) 0)
                        (aref (fixed-state-material-emission state) 1)
                        (aref (fixed-state-material-emission state) 2))
        (%gl:uniform-1f (%uniform "shininess")
                        (fixed-state-material-shininess state))
        (%gl:uniform-2f (%uniform "view_size")
                        (f (elt viewport 2)) (f (elt viewport 3)))
        (gl:bind-vertex-array vao)
        ;; GL_LINE_SMOOTH is what gives a 1-pixel line the fractional per-pixel
        ;; coverage LispWorks' lines have.  GL IGNORES it while multisampling is
        ;; on, and this framebuffer is multisampled, so the two are exclusive:
        ;; multisample goes off for the line draw and back on immediately after,
        ;; because the polygons and points of the same scene do rely on it.
        (let ((smooth (and (fixed-state-line-smooth state)
                           (eq primitive :lines)))
              (had-ms (gl:enabledp :multisample)))
          (when smooth
            (gl:disable :multisample)
            (gl:enable :line-smooth)
            (gl:hint :line-smooth-hint
                     (fixed-state-line-smooth-hint state)))
          (gl:draw-arrays primitive 0 count)
          (when smooth
            (gl:disable :line-smooth)
            (when had-ms
              (gl:enable :multisample))))
        (gl:bind-vertex-array 0)))
    count))

(defun %issue (primitive line-mode)
  "Upload the assembled transient batch and draw it with the current matrices."
  (let* ((res (current-resources))
         (state (current-fixed-state))
         (batch (fixed-state-batch state))
         (count (floor (fbuf-count batch) +vertex-floats+)))
    (when (plusp count)
      (gl:bind-vertex-array (res-vao res))
      (gl:bind-buffer :array-buffer (res-vbo res))
      (cffi:with-pointer-to-vector-data (data (fbuf-data batch))
        (%gl:buffer-data :array-buffer
                         (* 4 (fbuf-count batch))
                         data
                         :stream-draw)))
    (%draw-vao (res-vao res) primitive line-mode count)))

(defun make-static-batch-from-current (primitive line-mode)
  "Upload the current assembled batch once and return its context-local GPU handle."
  (let* ((res (current-resources))
         (state (current-fixed-state))
         (vertex-batch (fixed-state-batch state))
         (count (floor (fbuf-count vertex-batch) +vertex-floats+))
         (vbo (gl:gen-buffer))
         (vao nil))
    (handler-case
        (progn
          (gl:bind-buffer :array-buffer vbo)
          (cffi:with-pointer-to-vector-data
              (data (fbuf-data vertex-batch))
            (%gl:buffer-data :array-buffer
                             (* 4 (fbuf-count vertex-batch))
                             data
                             :static-draw))
          (setf vao (%make-vao vbo))
          (let ((batch (make-static-batch :context *context*
                                          :vao vao
                                          :vbo vbo
                                          :primitive primitive
                                          :count count
                                          :line-mode line-mode)))
            (push batch (res-static-batches res))
            batch))
      (serious-condition (condition)
        (when vao
          (gl:delete-vertex-arrays (list vao)))
        (gl:delete-buffers (list vbo))
        (error condition)))))

(defun draw-static-batch (batch)
  "Draw BATCH without rebuilding or uploading its immutable vertex data."
  (unless (eq (static-batch-context batch) *context*)
    (igl-error "Static batch belongs to context ~S, not current context ~S."
               (static-batch-context batch) *context*))
  (%draw-vao (static-batch-vao batch) (static-batch-primitive batch)
             (static-batch-line-mode batch) (static-batch-count batch)))

(defun delete-static-batch (batch)
  "Delete BATCH in its current GL context; harmless when already retired."
  (when (and batch (eq (static-batch-context batch) *context*))
    (let ((res (gethash *context* *contexts*)))
      (when (and res (member batch (res-static-batches res) :test #'eq))
        (gl:delete-vertex-arrays (list (static-batch-vao batch)))
        (gl:delete-buffers (list (static-batch-vbo batch)))
        (setf (res-static-batches res)
              (delete batch (res-static-batches res) :test #'eq))
        t))))

(defun compile-static-batches (thunk)
  "Compile THUNK's immediate-mode geometry into immutable context-local VBOs.
Nothing is drawn during compilation; matrices and shader uniforms stay dynamic."
  (let ((batches nil))
    (let ((*batch-sink*
            (lambda (primitive line-mode)
              (push (make-static-batch-from-current primitive line-mode) batches))))
      (funcall thunk))
    (nreverse batches)))
