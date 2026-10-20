(in-package :alloy-desktop)

;;; ---- Pictures -----------------------------------------------------------------
;;; A decoded image kept on the CPU, uploaded to an OpenGL texture on its first draw.

(defstruct (picture (:constructor make-picture))
  (path    nil)
  (image   nil)
  (texture nil)
  (width   0)
  (height  0))

(defun ensure-ub8-data (img)
  "Coerce the data of imagine image IMG to a simple (unsigned-byte 8) vector."
  (let ((d (org.shirakumo.fraf.imagine:data img)))
    (unless (typep d '(simple-array (unsigned-byte 8) (*)))
      (let ((new (make-array (length d) :element-type '(unsigned-byte 8))))
        (loop for i below (length d) do (setf (aref new i) (aref d i)))
        (setf (org.shirakumo.fraf.imagine:data img) new))))
  img)

(defun decode-image-file (path)
  "The imagine image of the file at PATH as bottom-left RGBA bytes, or NIL when it cannot be decoded."
  (handler-bind ((error
                   (lambda (c)
                     (format *error-output* "[image] failed to decode ~A: ~A~%"
                             path (princ-to-string c))
                     (return-from decode-image-file nil))))
    (let ((img (let ((babel-encodings:*suppress-character-coding-errors* t))
                 (org.shirakumo.fraf.imagine:load-image path t))))
      (ensure-ub8-data img)
      (org.shirakumo.fraf.imagine:adjust img
                                         :layout :rgba
                                         :channel-type :unsigned-byte
                                         :origin :bottom-left)
      img)))

(defun load-picture (path)
  "The picture of the image file at PATH, or NIL when it cannot be decoded (DECODE-IMAGE-FILE)."
  (let ((img (decode-image-file path)))
    (when img
      (make-picture :path path
                    :image img
                    :width (org.shirakumo.fraf.imagine:w img)
                    :height (org.shirakumo.fraf.imagine:h img)))))

(defun make-rgba-picture (data width height &key (origin :top-left))
  "A picture of DATA, WIDTH x HEIGHT RGBA octets whose first row is the top one
(ORIGIN :top-left) or the bottom one (:bottom-left)."
  (make-picture :image (org.shirakumo.fraf.imagine:adjust
                        (org.shirakumo.fraf.imagine:make-image
                         :data data :channel-type :unsigned-byte :layout :rgba
                         :w width :h height :origin origin)
                        :origin :bottom-left)
                :width width :height height))

(defun set-window-icon-from-picture (window picture)
  "Set WINDOW's title-bar/taskbar icon to the RGBA PICTURE (SET-WINDOW-ICON)."
  (when (window-icon-supported-p)
    (let* ((img (picture-image picture))
           (data (org.shirakumo.fraf.imagine:data img))
           (w (picture-width picture))
           (h (picture-height picture)))
      (if (eq :top-left (org.shirakumo.fraf.imagine:origin img))
          (set-window-icon window data w h)
          ;; the window system takes the top row first
          (let* ((row (* 4 w))
                 (pixels (make-array (* row h) :element-type '(unsigned-byte 8))))
            (dotimes (y h)
              (replace pixels data :start1 (* y row)
                                   :start2 (* (- h y 1) row) :end2 (* (- h y) row)))
            (set-window-icon window pixels w h))))))

;;; ---- Textures -------------------------------------------------------------------

(defclass image (simple:image)
  ((texture :initarg :texture :reader image-texture)
   ;; GL texture names live in ONE context: the owner is the only renderer that may
   ;; free this one, and the picture table must not keep that window alive.
   (renderer-ref :initarg :renderer-ref :initform nil :reader image-renderer-ref)))

(defun image-renderer (image)
  "The live renderer owning IMAGE's texture, or NIL."
  (let ((ref (image-renderer-ref image)))
    (and ref (sb-ext:weak-pointer-value ref))))

(defmethod org.shirakumo.alloy.renderers.opengl:bind ((image image))
  (org.shirakumo.alloy.renderers.opengl:bind (image-texture image)))

#+darwin
(defun mipmap-texture (texture)
  "Give TEXTURE a mipmap chain and the minification filter that reads it."
  ;; only :texture-min-filter accepts a mipmap mode; the mag filter rejects one
  (gl:bind-texture :texture-2d
                   (org.shirakumo.alloy.renderers.opengl:gl-name texture))
  (%gl:generate-mipmap :texture-2d)
  (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
  (gl:bind-texture :texture-2d 0))

(defun ensure-texture (picture)
  "PICTURE's texture image, uploaded to *current-renderer* on first use."
  (unless (picture-texture picture)
    (unless *current-renderer*
      (error "picture not allocated: *current-renderer* is NIL; call inside a draw context"))
    (let* ((img  (picture-image picture))
           (data (org.shirakumo.fraf.imagine:data img))
           (w    (picture-width picture))
           (h    (picture-height picture))
           (tex  (org.shirakumo.alloy.renderers.opengl:make-texture
                  *current-renderer* w h data :channels 4)))
      ;; the quad is sized by the caller, not by the file, so minification is common
      #+darwin (mipmap-texture tex)
      (setf (picture-texture picture)
            (make-instance 'image
                           :texture tex
                           :renderer-ref (sb-ext:make-weak-pointer *current-renderer*)
                           :size (alloy:px-size w h)
                           :data data
                           :channels 4))))
  (picture-texture picture))

;;; ---- Picture table (name -> picture) ----------------------------------------------

(defvar *pictures* nil
  "Loaded pictures as (key picture): a string key, or (name source) for a named source.")

(defun find-picture (name src)
  "The picture stored under NAME (case-insensitive string) or under (NAME SRC)."
  (let ((pos (if (stringp name)
                 (position name *pictures*
                           :key #'car
                           :test (lambda (a b) (and (stringp b) (string-equal a b))))
                 (position (list name src) *pictures*
                           :key #'car :test #'equal))))
    (when pos (second (nth pos *pictures*)))))

(defun store-picture (handle name src)
  "Store HANDLE under NAME (and SRC) unless that key already holds a picture."
  (unless (find-picture name src)
    (if (stringp name)
        (push (list name handle) *pictures*)
        (push (list (list name src) handle) *pictures*))))

(defun forget-picture (picture)
  "Remove every table entry holding PICTURE."
  (setf *pictures*
        (remove picture *pictures* :test #'equal :key #'cadr)))

(defun loaded-pictures (&optional typetest)
  "The (name source) entries of the table whose source satisfies TYPETEST."
  (remove nil
          (loop for item in *pictures*
                collect (when (and (listp (car item))
                                   (if typetest (funcall typetest (cadr (car item))) t))
                          item))))

;;; ---- Freeing ----------------------------------------------------------------------

(defun delete-gl-texture (renderer name)
  "Free texture NAME in RENDERER's context, restoring the previously current one."
  ;; a destroyed renderer already took its textures down with its context
  (when (and (typep renderer '%glfw:window) (%glfw:pointer renderer))
    (call-with-gl-context renderer
                          (lambda () (gl:delete-texture name)))))

(defun free-image-texture (image fallback-renderer)
  "Free IMAGE's texture on the UI thread, in its owner's context or else FALLBACK-RENDERER's."
  ;; a GL free is only legal where the context lives; another thread queues it
  (let ((renderer (or (image-renderer image) fallback-renderer))
        (name     (org.shirakumo.alloy.renderers.opengl:gl-name
                   (image-texture image))))
    (if (or (null (ui-thread)) (on-ui-thread-p))
        (delete-gl-texture renderer name)
        (post-to-ui-thread (lambda () (delete-gl-texture renderer name))))))

(defun kill-picture (picture fallback-renderer)
  "Forget PICTURE and free its texture."
  (forget-picture picture)
  (let ((image (picture-texture picture)))
    (when image
      ;; drop the reference first: a failing GL free must not leave a dangling image
      (setf (picture-texture picture) nil)
      (free-image-texture image fallback-renderer))))

;;; ---- Drawing ------------------------------------------------------------------------

(defvar *image-shaders* (make-hash-table :test 'eq)
  "renderer -> image shader with uv-crop and global-alpha uniforms.")

(defun image-shader (renderer)
  "RENDERER's image shader with source-crop and alpha uniforms."
  (or (gethash renderer *image-shaders*)
      (setf (gethash renderer *image-shaders*)
            (org.shirakumo.alloy.renderers.opengl:make-shader
             renderer
             :vertex-shader "layout (location=0) in vec2 pos;
uniform mat4 transform;
out vec2 uv;
void main(){ gl_Position = transform*vec4(pos, 0.0, 1.0); uv = pos; }"
             :fragment-shader "out vec4 out_color;
uniform sampler2D image;
uniform vec2 uv_offset = vec2(0,0);
uniform vec2 uv_scale = vec2(1,1);
uniform float alpha = 1.0;
in vec2 uv;
void main(){ vec4 color = texture(image, (uv/uv_scale)+uv_offset); out_color = vec4(color.rgb, color.a*alpha); }"))))

(defun render-image-region (renderer tex extent fx fy fw fh pw ph alpha)
  "Blit the (FX FY FW FH) top-left sub-rectangle of a PWxPH texture into EXTENT."
  (let ((shader (image-shader renderer)))
    (simple:with-pushed-transforms (renderer)
      (org.shirakumo.alloy.renderers.opengl:bind shader)
      (org.shirakumo.alloy.renderers.opengl:bind tex)
      (setf (org.shirakumo.alloy.renderers.opengl:uniform shader "uv_scale")
            (alloy:px-size (/ pw fw) (/ ph fh)))
      (setf (org.shirakumo.alloy.renderers.opengl:uniform shader "uv_offset")
            (alloy:px-point (float (/ fx pw)) (float (/ (- ph fy fh) ph))))
      (setf (org.shirakumo.alloy.renderers.opengl:uniform shader "alpha")
            (float alpha 0f0))
      (simple:translate renderer (alloy:px-point (alloy:pxx extent) (alloy:pxy extent)))
      (simple:scale renderer (alloy:px-size (alloy:pxw extent) (alloy:pxh extent)))
      (setf (org.shirakumo.alloy.renderers.opengl:uniform shader "transform")
            (simple:transform-matrix renderer))
      (org.shirakumo.alloy.renderers.opengl:draw-vertex-array
       (org.shirakumo.alloy.renderers.opengl:resource
        'org.shirakumo.alloy.renderers.opengl::rect-fill-vao renderer)
       :triangles 0 6))))

(defgeneric port-draw-image (port picture x y width height src-x src-y src-width src-height
                             alpha highlight))

(defmethod port-draw-image ((port screen-port) picture x y width height src-x src-y
                            src-width src-height alpha highlight)
  (when *current-renderer*
    (let* ((tex (ensure-texture picture))
           (pw  (picture-width  picture))
           (ph  (picture-height picture))
           (px  (+ x *pox*))
           (py  (+ y *poy*))
           (extent (alloy:px-extent px (flip-y-rect py height) width height)))
      (if (or alpha src-width (/= 0 src-x) (/= 0 src-y))
          (render-image-region *current-renderer* tex extent
                               src-x src-y
                               (or src-width pw)
                               (or src-height ph)
                               pw ph (or alpha 1.0))
          (alloy:render *current-renderer*
                        (simple:icon *current-renderer* extent tex)))
      (when highlight
        (alloy:render *current-renderer*
                      (simple:rectangle *current-renderer* extent :pattern highlight))))))

;;; ---- Files and CPU rasters -----------------------------------------------------------

(defun save-picture (picture path type)
  "Write PICTURE's image to PATH in format TYPE."
  (let ((img (picture-image picture)))
    ;; file encoders write data as-is and expect top-left rows
    (org.shirakumo.fraf.imagine:adjust img :origin :top-left)
    (unwind-protect
         (org.shirakumo.fraf.imagine:save-image img path type)
      (org.shirakumo.fraf.imagine:adjust img :origin :bottom-left)))
  path)

(defun rasterize-picture (picture width height bg-r bg-g bg-b bg-a alpha-p)
  "A new WIDTHxHEIGHT picture of PICTURE scaled over the background BG-R BG-G BG-B BG-A, on the CPU."
  ;; CPU-side on purpose: no GL context, no temporary FBO, no other thread's renderer
  (let* ((source (picture-image picture))
         (source-data (and source (org.shirakumo.fraf.imagine:data source)))
         (source-width (picture-width picture))
         (source-height (picture-height picture))
         (source-top-left-p
           (and source
                (eq :top-left (org.shirakumo.fraf.imagine:origin source))))
         (target-data
           (make-array (* width height 4)
                       :element-type '(unsigned-byte 8))))
    (dotimes (target-y height)
      (let* ((logical-source-y
               (min (1- source-height)
                    (floor (* target-y source-height) height)))
             (source-y (if source-top-left-p
                           (- source-height 1 logical-source-y)
                           logical-source-y)))
        (dotimes (target-x width)
          (let* ((source-x
                   (min (1- source-width)
                        (floor (* target-x source-width) width)))
                 (source-index
                   (* 4 (+ source-x (* source-y source-width))))
                 (target-index
                   (* 4 (+ target-x (* target-y width))))
                 (source-r (if source-data (aref source-data source-index) 0))
                 (source-g (if source-data
                               (aref source-data (+ source-index 1)) 0))
                 (source-b (if source-data
                               (aref source-data (+ source-index 2)) 0))
                 (source-a (if source-data
                               (/ (aref source-data (+ source-index 3)) 255.0)
                               0.0))
                 (base-a (if alpha-p (/ bg-a 255.0) 1.0))
                 (result-a (+ source-a (* base-a (- 1.0 source-a))))
                 (result-r
                   (if (zerop result-a) 0
                       (/ (+ (* source-r source-a)
                             (* bg-r base-a (- 1.0 source-a)))
                          result-a)))
                 (result-g
                   (if (zerop result-a) 0
                       (/ (+ (* source-g source-a)
                             (* bg-g base-a (- 1.0 source-a)))
                          result-a)))
                 (result-b
                   (if (zerop result-a) 0
                       (/ (+ (* source-b source-a)
                             (* bg-b base-a (- 1.0 source-a)))
                          result-a))))
            (setf (aref target-data target-index)
                  (round (min 255 (max 0 result-r)))
                  (aref target-data (+ target-index 1))
                  (round (min 255 (max 0 result-g)))
                  (aref target-data (+ target-index 2))
                  (round (min 255 (max 0 result-b)))
                  (aref target-data (+ target-index 3))
                  (if alpha-p
                      (round (* 255 (min 1.0 (max 0.0 result-a))))
                      255))))))
    (make-picture
     :image (org.shirakumo.fraf.imagine::make-image*
             target-data :unsigned-byte :rgba width height 1 :bottom-left)
     :width width :height height)))

;;; ---- Offscreen recording (framebuffer) ---------------------------------------------

(defvar *record-framebuffers* (make-hash-table :test 'eq)
  "renderer -> reusable offscreen framebuffer for picture recording.")

(defun release-image-gl-resources (window)
  "Delete the image shader and record framebuffer owned by WINDOW."
  (let ((shader (gethash window *image-shaders*)))
    (when shader
      (deallocate-gl-resource shader)))
  (let ((fbo (gethash window *record-framebuffers*)))
    (when fbo
      (deallocate-gl-resource fbo))))

(defun forget-image-gl-resources (window)
  "Drop the image bookkeeping keyed by WINDOW."
  (remhash window *image-shaders*)
  (remhash window *record-framebuffers*))

(register-gl-context-cleanup
 'image-gl-resources
 :release #'release-image-gl-resources
 :forget #'forget-image-gl-resources)

(defstruct (record-state (:conc-name record-))
  renderer fbo width height
  saved-viewport saved-identity saved-transform
  saved-clip-depth saved-stencil-func saved-stencil-ref saved-stencil-mask)

(defun begin-record (renderer w h scale)
  "Redirect RENDERER's drawing to a white WxH offscreen framebuffer sampled at SCALE device pixels per unit."
  (let* ((fbo (or (gethash renderer *record-framebuffers*)
                  (setf (gethash renderer *record-framebuffers*)
                        (org.shirakumo.alloy.renderers.opengl:make-framebuffer renderer))))
         ;; the framebuffer is sized from the CURRENT viewport when it is bound
         ;; (alloy renderers/opengl/unmanaged.lisp: bind of an fbo), never after
         (dw (max 1 (round (* w scale))))
         (dh (max 1 (round (* h scale))))
         (state (make-record-state
                 :renderer renderer :fbo fbo :width dw :height dh
                 :saved-viewport (gl:get-integer :viewport 4)
                 :saved-identity (copy-seq (simple:identity-matrix renderer))
                 :saved-transform (copy-seq (simple:transform-matrix renderer))
                 :saved-clip-depth org.shirakumo.alloy.renderers.opengl::*clip-depth*
                 :saved-stencil-func (gl:get-integer :stencil-func)
                 :saved-stencil-ref (gl:get-integer :stencil-ref)
                 :saved-stencil-mask (gl:get-integer :stencil-value-mask))))
    (gl:viewport 0 0 dw dh)
    (org.shirakumo.alloy.renderers.opengl:bind fbo)
    (gl:clear-color 1.0 1.0 1.0 1.0)
    (gl:clear :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit)
    (setf org.shirakumo.alloy.renderers.opengl::*clip-depth* 0)
    (gl:stencil-func :lequal 0 #xff)
    (simple:orthographic-matrix w h (simple:transform-matrix renderer))
    (setf (simple:identity-matrix renderer)
          (copy-seq (simple:transform-matrix renderer)))
    state))

(defun record-texture (renderer w h)
  "A WxH texture holding what the bound read framebuffer shows."
  ;; the recording already sits on the GPU, so the picture takes it from there
  (let ((texture (org.shirakumo.alloy.renderers.opengl:make-texture
                  renderer w h nil :channels 4)))
    (gl:bind-texture :texture-2d
                     (org.shirakumo.alloy.renderers.opengl:gl-name texture))
    (gl:copy-tex-sub-image-2d :texture-2d 0 0 0 0 0 w h)
    (gl:bind-texture :texture-2d 0)
    texture))

(defun end-record (state)
  "The picture recorded since begin-record, after restoring the on-screen render target."
  (let* ((renderer (record-renderer state))
         (w (record-width state))
         (h (record-height state))
         (fbo (record-fbo state)))
    ;; the texture copy is issued first: glReadPixels waits for every command
    ;; issued before it, so the copy is complete before another context binds it
    (gl:bind-framebuffer :read-framebuffer
                         (org.shirakumo.alloy.renderers.opengl:gl-name fbo))
    (let* ((texture (record-texture renderer w h))
           (pixels (gl:read-pixels 0 0 w h :rgba :unsigned-byte)))
      (gl:bind-framebuffer :framebuffer 0)
      (let ((v (record-saved-viewport state)))
        (gl:viewport (aref v 0) (aref v 1) (aref v 2) (aref v 3)))
      (replace (simple:transform-matrix renderer) (record-saved-transform state))
      (setf (simple:identity-matrix renderer) (record-saved-identity state))
      (setf org.shirakumo.alloy.renderers.opengl::*clip-depth*
            (record-saved-clip-depth state))
      (gl:stencil-func (record-saved-stencil-func state)
                       (record-saved-stencil-ref state)
                       (record-saved-stencil-mask state))
      ;; read-pixels already answers a fresh simple (unsigned-byte 8) vector
      ;; (cl-opengl framebuffer.lisp: read-pixels), so darwin holds it as it is
      (let* ((data #+darwin pixels
                   #-darwin (replace (make-array (* w h 4)
                                                 :element-type '(unsigned-byte 8))
                                     pixels))
             (picture (make-picture
                       :image (org.shirakumo.fraf.imagine::make-image*
                               data :unsigned-byte :rgba w h 1 :bottom-left)
                       :width w :height h)))
        (setf (picture-texture picture)
              (make-instance 'image
                             :texture texture
                             :renderer-ref (sb-ext:make-weak-pointer renderer)
                             :size (alloy:px-size w h)
                             :data data
                             :channels 4))
        picture))))

(defclass metafile-port ()
  ()
  (:documentation "Marker class for the drawing port of an offscreen recording; the caller instantiates it."))

(defun call-with-recorded-picture (renderer width height scale function)
  "Call FUNCTION with no arguments while RENDERER draws into a white WIDTHxHEIGHT
recording sampled at SCALE device pixels per unit, and return the picture. Must run
on the thread that owns RENDERER's GL context. Draw-pass state is bound to the
recording's own: the renderer, a height of HEIGHT, origin (0, 0), and the current
colours defaulting to black on white. The recording ends, and the on-screen render
target comes back, however FUNCTION exits."
  (call-with-gl-context
   renderer
   (lambda ()
     ;; unit resolution reads (ui (layout-tree *unit-parent*)), which only the
     ;; on-screen render binds; an offscreen recording has to bind it itself
     (alloy:with-unit-parent renderer
       ;; a NIL colour reaches the shape as pattern NIL, which alloy then looks up
       ;; in the (also NIL) palette and signals, so the on-screen defaults apply
       (let ((*current-renderer* renderer)
             (*current-view-height* (float height))
             (*pox* 0) (*poy* 0)
             (*current-fg-color* (or *current-fg-color* colors:black))
             (*current-bg-color* (or *current-bg-color* colors:white))
             (state nil) (result nil))
         (unwind-protect
              (progn (setf state (begin-record renderer width height scale))
                     (funcall function))
           (when state (setf result (end-record state))))
         result)))))
