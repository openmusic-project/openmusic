(in-package :om-api)

(export '(*om-resources-folder*
          *om-pict-folder*
          *om-pict-type*
          *om-cursor-type*
          om-set-default-resource-folder
          om-get-resource-file

          om-pict-p
          om-load-pixmap
          om-store-picture
          om-loaded-pictures
          om-picture-values
          om-picture-array
          om-create-picture
          om-get-picture-loaded
          om-kill-picture
          om-pict-width
          om-pict-height
          om-get-picture-size
          om-draw-picture
          om-record-pict
          *om-record-pict-scale*
          om-internal-picture-to-pict
          om-record-picture-in-pict
          om-save-picture

          om-make-cursor
          om-with-cursor
          *om-wait-cursor*
          *om-arrow-cursor*
          *om-verti-size-cursor*
          *om-horiz-size-cursor*
          *om-resize-cursor*
          *om-i-beam-cursor*
          *om-cross-cursor*
          *om-hand-cursor*
          om-init-cursors

          om-open-score-page
          om-close-score-page)
        :om-api)

;;; Resource folders

(defvar *om-resources-folder* nil)
(defvar *om-pict-folder*      nil)
(defvar *om-cursor-folder*    nil)

(defvar *om-pict-type* '("tif" "tiff" "gif" "jpg" "jpeg" "png" "bmp"))

;; win32 tries png first: no .cur/.ani decoder exists in the imagine stack.
(defvar *om-cursor-type*
  #+win32  '("png" "cur" "ani")
  #-win32  '("png"))

(defun om-set-default-resource-folder (resource-type path)
  (case resource-type
    (:rsrc (setf *om-resources-folder* path)
           (init-sub-rsrc))
    (:pict (setf *om-pict-folder* path))
    (:curs (setf *om-cursor-folder* path))))

(declaim (special *version-str*))

(defun init-def-rsrc ()
  (when *om-root*
    (setf *om-resources-folder*
          (make-pathname :device (pathname-device *om-root*)
                         :host (pathname-host *om-root*)
                         :directory (append (pathname-directory *om-root*)
                                            #-darwin (list "resources")
                                            #+darwin (if (member :om-deliver *features*)
                                                         (list (concatenate 'string "OM " *version-str* ".app")
                                                               "Contents" "Resources")
                                                         (list "resources")))))))

(defun init-sub-rsrc ()
  (when *om-resources-folder*
    (om-set-default-resource-folder
     :pict (make-pathname :device (pathname-device *om-resources-folder*)
                          :host (pathname-host *om-resources-folder*)
                          :directory (append (pathname-directory *om-resources-folder*)
                                             (list "pict"))))
    (om-set-default-resource-folder
     :curs (make-pathname :device (pathname-device *om-resources-folder*)
                          :host (pathname-host *om-resources-folder*)
                          :directory (append (pathname-directory *om-resources-folder*)
                                             (list "curs"))))))

(om-api-add-init-func 'init-def-rsrc)
(om-api-add-init-func 'init-sub-rsrc)

(defun om-pict-p (pathname)
  (let ((type (and pathname (pathname-type pathname))))
    (and type (member type *om-pict-type* :test #'string-equal) t)))

(defun om-get-resource-file (name folder types)
  (let ((rep   nil)
        (types (if (listp types) types (list types))))
    (loop for typ in types
          while (not rep) do
            (setf rep (probe-file
                       (make-pathname
                        :directory (when folder
                                     (if (pathnamep folder)
                                         (pathname-directory folder)
                                         folder))
                        :device (when (pathnamep folder) (pathname-device folder))
                        :host   (when (pathnamep folder) (pathname-host folder))
                        :name name :type typ))))
    rep))

;;; Pictures are alld:picture (alloy-desktop/graphics/image.lisp: picture): an
;;; imagine image uploaded to an OpenGL texture on its first draw.

(defvar *om-window-icon* :unloaded)

(defun %om-window-icon-picture ()
  "OM logo (om.png under *om-pict-folder*, the OM picture-resource path) as a
picture, cached on first success; NIL while the picture folder is unset (e.g.
before OM init) or the logo cannot be decoded."
  (when (eq *om-window-icon* :unloaded)
    (let* ((file (and *om-pict-folder*
                      (om-get-resource-file "om" *om-pict-folder* "png")))
           (pict (and file (alld:load-picture file))))
      (when pict
        (setf *om-window-icon* pict))))
  (if (eq *om-window-icon* :unloaded) nil *om-window-icon*))

(defun om-load-pixmap (name type &optional folder mask-p)
  "Load an image file resolved via om-get-resource-file. Signature matches om-LW."
  (declare (ignore mask-p))
  (let* ((path (if folder folder *om-pict-folder*))
         (file (om-get-resource-file name path type)))
    (when file
      (alld:load-picture file))))

;;; Picture cache (name -> handle): alloy-desktop/graphics/image.lisp: *pictures*.

(defun om-store-picture (pict name &optional source)
  (alld:store-picture pict name source))

(defun om-get-picture-loaded (name source)
  (alld:find-picture name source))

(defun om-loaded-pictures (&optional typetest)
  (alld:loaded-pictures typetest))

(defmethod om-kill-picture ((image t))
  ;; the share partner frees a texture whose owner window is gone
  (when (alld:picture-p image)
    (alld:kill-picture image (alld:share-window)))
  t)

;;; Dimensions.

(defmethod om-pict-width ((pict alld:picture))  (alld:picture-width pict))
(defmethod om-pict-height ((pict alld:picture)) (alld:picture-height pict))
(defmethod om-pict-width ((pict t))  0)
(defmethod om-pict-height ((pict t)) 0)
(defmethod om-get-picture-size (pict)
  (om-make-point (om-pict-width pict) (om-pict-height pict)))

;;; Drawing.

(defmethod om-draw-picture (view pict &key (pos (om-make-point 0 0)) size
                                           (srctopleft (om-make-point 0 0)) srcsize
                                           selected alpha)
  "VIEW names the graphics port in vanilla ((om-get-view view),
om-LW/icons-picts.lisp: om-draw-picture for (view pict));
here every draw goes through *CURRENT-RENDERER*, so the port is already selected."
  (declare (ignore view))
  (when (and pict (alld:picture-p pict))
    (alld:port-draw-image alld:*current-port* pict
                          (om-point-h pos) (om-point-v pos)
                          (if size (om-point-h size) (alld:picture-width pict))
                          (if size (om-point-v size) (alld:picture-height pict))
                          (om-point-h srctopleft) (om-point-v srctopleft)
                          (and srcsize (om-point-h srcsize))
                          (and srcsize (om-point-v srcsize))
                          alpha
                          (and selected
                               (%omcolor->chroma
                                (om-make-color-alpha
                                 (om-color-r *om-select-color*)
                                 (om-color-g *om-select-color*)
                                 (om-color-b *om-select-color*)
                                 0.1))))))

;;; capi-capi-ref-101.htm:60 — of the whole graphics-state only TRANSFORM, MASK and
;;; MASK-TRANSFORM affect draw-metafile; everything else comes from the metafile.
;;; Vanilla carries the mask in the internal-picture method of om-draw-picture
;;; (om-LW/icons-picts.lisp: om-draw-picture for (view internal-picture)); here a
;;; metafile IS an alld:picture, so it lives here.

(defun %om-metafile-mask (pane)
  "Vanilla mask (dx dy pw ph) for PANE (om-LW/icons-picts.lisp: om-draw-picture for (view internal-picture)), or NIL."
  (when pane
    (let ((dx 0) (dy 0) (pw (om-width pane)) (ph (om-height pane)))
      (cond ((om-item-view-p pane)
             (setf dx (item-x pane) dy (item-y pane)))
            ((scroller-p pane)
             (let ((field (om-field-size pane)))
               (setf pw (om-point-h field) ph (om-point-v field)))))
      (when (and (realp pw) (realp ph) (plusp pw) (plusp ph))
        (list dx dy pw ph)))))

(defun %om-draw-metafile (pane metafile x y width height)
  (let ((mask (%om-metafile-mask pane)))
    (flet ((draw ()
             (alld:call-with-transform
              (lambda ()
                (om-draw-picture pane metafile
                                 :pos (om-make-point x y)
                                 :size (om-make-point width height))))))
      (if mask
          (destructuring-bind (dx dy pw ph) mask
            (om-with-clip-rect pane (om-make-rect dx dy (+ dx pw) (+ dy ph))
              (draw)))
          (draw))))
  nil)

;;; Pixel-data construction (kernel uses for picture box save/restore).

(defun %list-to-imagine (color-array)
  "Convert nested ((r g b a) ...) list to imagine image."
  (let* ((h (length color-array))
         (w (length (car color-array)))
         (data (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (loop for line in color-array
          for j from 0
          do (loop for pix in line
                   for i from 0
                   do (let ((r (if (consp pix) (nth 0 pix) pix))
                            (g (if (consp pix) (nth 1 pix) pix))
                            (b (if (consp pix) (nth 2 pix) pix))
                            (a (if (and (consp pix) (cadddr pix)) (cadddr pix) 1)))
                        (let ((base (* 4 (+ i (* j w)))))
                          (setf (aref data base)       (round (* 255 (min 1 (max 0 r))))
                                (aref data (+ base 1)) (round (* 255 (min 1 (max 0 g))))
                                (aref data (+ base 2)) (round (* 255 (min 1 (max 0 b))))
                                (aref data (+ base 3)) (round (* 255 (min 1 (max 0 a)))))))))
    (alld:make-rgba-picture data w h)))

(defun %array-to-imagine (array)
  "Convert (h w 4) BGRA (vanilla order) or (h w) grayscale array to imagine image."
  (let* ((h (array-dimension array 0))
         (w (array-dimension array 1))
         (data (make-array (* w h 4) :element-type '(unsigned-byte 8))))
    (dotimes (j h)
      (dotimes (i w)
        (let ((base (* 4 (+ i (* j w)))))
          (if (= (array-rank array) 2)
              ;; vanilla writes alpha byte 1 here (invisible); 255 is the documented deviation
              (let ((p (aref array j i)))
                (setf (aref data base)       p
                      (aref data (+ base 1)) p
                      (aref data (+ base 2)) p
                      (aref data (+ base 3)) 255))
              (setf (aref data base)       (aref array j i 2)
                    (aref data (+ base 1)) (aref array j i 1)
                    (aref data (+ base 2)) (aref array j i 0)
                    (aref data (+ base 3)) (aref array j i 3))))))
    (alld:make-rgba-picture data w h)))

(defun om-create-picture (array)
  (cond ((arrayp array) (%array-to-imagine array))
        ((consp array)  (%list-to-imagine array))
        (t nil)))

(defun om-picture-values (pict)
  "List form: ((r g b a) ...) 0..1 floats per pixel, rows top-to-bottom."
  (when (and pict (alld:picture-p pict))
    (let* ((img  (alld:picture-image pict))
           (w    (alld:picture-width pict))
           (h    (alld:picture-height pict))
           (data (org.shirakumo.fraf.imagine:data img)))
      ;; image data is bottom-left; emit rows top-to-bottom like vanilla
      (loop for j from (1- h) downto 0
            collect (loop for i from 0 below w
                          for base = (* 4 (+ i (* j w)))
                          collect (list (/ (aref data base)       255.0)
                                        (/ (aref data (+ base 1)) 255.0)
                                        (/ (aref data (+ base 2)) 255.0)
                                        (/ (aref data (+ base 3)) 255.0)))))))

(defun om-picture-array (pict)
  "(h w 4) BGRA byte array displaced to a flat vector, rows top-to-bottom (vanilla shape)."
  (when (and pict (alld:picture-p pict))
    (let* ((img  (alld:picture-image pict))
           (w    (alld:picture-width pict))
           (h    (alld:picture-height pict))
           (data (org.shirakumo.fraf.imagine:data img))
           (bgra (make-array (* w h 4) :element-type '(unsigned-byte 8))))
      (dotimes (j h)
        (let ((src (* 4 w (- h 1 j)))
              (dst (* 4 w j)))
          (dotimes (i w)
            (let ((s (+ src (* 4 i)))
                  (d (+ dst (* 4 i))))
              (setf (aref bgra d)       (aref data (+ s 2))
                    (aref bgra (+ d 1)) (aref data (+ s 1))
                    (aref bgra (+ d 2)) (aref data s)
                    (aref bgra (+ d 3)) (aref data (+ s 3)))))))
      (make-array (list h w 4) :element-type '(unsigned-byte 8)
                               :displaced-to bgra))))

(defun %om-raster-background-rgba (pane background background-p alpha-p)
  (let* ((spec (if background-p
                   background
                   (and pane (om-get-bg-color pane))))
         (color (%omcolor->chroma (or spec *om-white-color*))))
    (values (round (* 255 (chroma:r color)))
            (round (* 255 (chroma:g color)))
            (round (* 255 (chroma:b color)))
            (if alpha-p
                (round (* 255 (chroma:a color)))
                255))))

(defun %om-rasterize-metafile
    (pane metafile width height background background-p alpha-p)
  "Create an independent RGBA raster by scaling METAFILE over BACKGROUND, on the CPU
(alloy-desktop/graphics/image.lisp: rasterize-picture)."
  (unless (alld:picture-p metafile)
    (error "~S is not a rasterizable OM metafile." metafile))
  (multiple-value-bind (background-r background-g background-b background-a)
      (%om-raster-background-rgba pane background background-p alpha-p)
    (alld:rasterize-picture metafile width height
                            background-r background-g background-b background-a
                            alpha-p)))

(defmethod om-save-picture (pict path &optional (type :tiff))
  (when (and pict (alld:picture-p pict))
    (alld:save-picture pict path type)))

;;; Cursors.
;;;
;;; om-make-cursor returns an alld:cursor-image (custom image + hotspot, realised as a
;;; GLFW cursor on first apply) or NIL; built-in vars hold alloy/glfw keywords.

(defvar *om-wait-cursor*       nil)
(defvar *om-arrow-cursor*      nil)
(defvar *om-verti-size-cursor* nil)
(defvar *om-horiz-size-cursor* nil)
(defvar *om-resize-cursor*     nil)
(defvar *om-i-beam-cursor*     nil)
(defvar *om-cross-cursor*      nil)
(defvar *om-hand-cursor*       nil)

(defun om-make-cursor (name &optional (click-pos (om-make-point 0 0)))
  (let ((file (and *om-cursor-folder*
                   (om-get-resource-file name *om-cursor-folder* *om-cursor-type*))))
    (when file
      (alld:load-cursor-image file (om-point-h click-pos) (om-point-v click-pos)))))

(defun om-init-cursors ()
  (setf *om-wait-cursor*       (or (om-make-cursor "wait-cursor" (om-make-point 8 8)) :arrow)
        *om-arrow-cursor*      nil
        *om-horiz-size-cursor* :resize-ew
        *om-verti-size-cursor* :resize-ns
        ;; vanilla ships the NAMED system cursor off cocoa (om-LW/icons-picts.lisp:
        ;; om-init-cursors, #-cocoa :bottom-right-corner); the kernel puts the image
        ;; back on linux only (kernel graphics/graphictools.lisp: init-curs)
        *om-resize-cursor*     #+macosx (or (om-make-cursor "resize-cursor" (om-make-point 8 8)) :resize-nwse)
                               #-macosx :resize-nwse
        *om-i-beam-cursor*     :ibeam
        *om-cross-cursor*      (or (om-make-cursor "croix" (om-make-point 8 8)) :crosshair)
        *om-hand-cursor*       (or (om-make-cursor "hand-cursor") :pointing-hand)))

(om-api-add-init-func 'om-init-cursors)

(defmacro om-with-cursor (cursor &body body)
  "Stub matching om-LW upstream behaviour — body runs unchanged; cursor argument ignored.
The upstream macro is a no-op as well (commented capi calls)."
  (declare (ignore cursor))
  `(progn ,@body))

;;; Offscreen recording (vanilla metafile/pixmap-port family) via OpenGL FBO:
;;; alloy-desktop/graphics/image.lisp: call-with-recorded-picture.

(defun %om-record-renderer ()
  "The renderer an offscreen record draws into: the one the current draw pass
already owns, else the front window. Acquiring its GL context is the caller's
job, so that the caller can also give it back."
  (or *current-renderer* (om-front-window)))

(defvar *om-record-pict-scale* 1
  "Device pixels per drawing unit of an offscreen record; the drawing keeps its own coordinates.")

(defun %om-record-pict-call (font size fun &optional port)
  ;; vanilla records into a thread-agnostic GDI port from whatever process the
  ;; kernel evaluates on (om-LW/icons-picts.lisp: the om-record-pict macro); a WGL context belongs
  ;; to one thread, so here the whole record runs where the context lives
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (%om-record-pict-call font size fun port)))
      (let ((renderer (%om-record-renderer))
            (w (round (om-point-h size)))
            (h (round (om-point-v size))))
        (unless renderer
          (error "om-record-pict: no window/renderer available"))
        (alld:call-with-recorded-picture
         renderer w h *om-record-pict-scale*
         (lambda ()
           ;; vanilla records through capi:with-internal-metafile :pane nil, so the
           ;; current port is a metafile-port here (om-LW/icons-picts.lisp:
           ;; the with-internal-metafile of om-record-pict)
           (let ((*curstream* (or port (make-instance 'capi::metafile-port)))
                 (*current-font* (or font *current-font*)))
             (funcall fun)))))))

(defmacro om-record-pict (font size &body body)
  `(%om-record-pict-call ,font ,size (lambda () ,@body)))

(defun %om-call-with-internal-metafile (pane bounds fun)
  "Record FUN's drawing offscreen and return the metafile (capi-capi-ref-545.htm:68).
PANE NIL makes the port a metafile-port (capi-capi-ref-543.htm:70); BOUNDS is (x y w h)."
  (let ((port (or pane (make-instance 'capi::metafile-port)))
        (size (cond (bounds (om-make-point (max 1 (round (third bounds)))
                                           (max 1 (round (fourth bounds)))))
                    (pane   (om-view-size pane))
                    (t (error "capi:with-internal-metafile: :bounds is required when :pane is NIL")))))
    (%om-record-pict-call nil size (lambda () (funcall fun port)) port)))

(defmethod om-internal-picture-to-pict (self view)
  (declare (ignore view))
  (when (alld:picture-p self) self))

(defmethod om-record-picture-in-pict (pict &optional (pos (om-make-point 0 0)) size)
  (when pict
    (om-draw-picture nil pict :pos pos :size size)))

;;; A score page is an open record plus the OM drawing globals it displaced.
(defstruct (%om-score-page)
  record saved-gl-context saved-renderer saved-view-height saved-pox saved-poy saved-font
  saved-fg-color saved-bg-color
  ;; :UNBOUND records "there was no unit parent", the state outside any render
  (saved-unit-parent :unbound))

(defun %save-score-page-state (state previous)
  "Record in STATE everything om-close-score-page has to put back."
  (setf (%om-score-page-saved-gl-context state)   previous
        (%om-score-page-saved-renderer state)     *current-renderer*
        (%om-score-page-saved-view-height state)  *current-view-height*
        (%om-score-page-saved-pox state)          *pox*
        (%om-score-page-saved-poy state)          *poy*
        (%om-score-page-saved-font state)         *current-font*
        (%om-score-page-saved-fg-color state)     *current-fg-color*
        (%om-score-page-saved-bg-color state)     *current-bg-color*
        ;; open\close is a PAIR (om-LW/icons-picts.lisp: om-open-score-page and om-close-score-page), so the unit
        ;; parent cannot ride a dynamic binding: save here, put back on close
        (%om-score-page-saved-unit-parent state)  (if (boundp 'alloy::*unit-parent*)
                                                      alloy::*unit-parent*
                                                      :unbound))
  state)

(defun %enter-score-page (renderer h font)
  "Point the drawing globals at the page just opened."
  (setf *current-renderer* renderer
        *current-view-height* (float h)
        *pox* 0 *poy* 0
        *current-fg-color* (or *current-fg-color* colors:black)
        *current-bg-color* (or *current-bg-color* colors:white)
        alloy::*unit-parent* renderer)
  (when font (setf *current-font* font))
  renderer)

(defun %restore-score-page-globals (state)
  "Put back what %save-score-page-state recorded."
  (setf *current-renderer*    (%om-score-page-saved-renderer state)
        *current-view-height* (%om-score-page-saved-view-height state)
        *pox*                 (%om-score-page-saved-pox state)
        *poy*                 (%om-score-page-saved-poy state)
        *current-font*        (%om-score-page-saved-font state)
        *current-fg-color*    (%om-score-page-saved-fg-color state)
        *current-bg-color*    (%om-score-page-saved-bg-color state))
  (let ((saved (%om-score-page-saved-unit-parent state)))
    (if (eq saved :unbound)
        (makunbound 'alloy::*unit-parent*)
        (setf alloy::*unit-parent* saved))))

(defun om-open-score-page (size font)
  (let ((renderer (%om-record-renderer))
        (w (round (om-point-h size)))
        (h (round (om-point-v size))))
    (unless renderer
      (error "om-open-score-page: no window/renderer available"))
    ;; the GL context, like the unit parent below, cannot ride a dynamic binding
    ;; across the pair: borrow it here, give it back in om-close-score-page
    (let ((previous (alld:acquire-gl-context renderer))
          (state nil) (saved nil) (delivered nil))
      (unwind-protect
           (progn
             (setf state (make-%om-score-page
                          :record (alld:begin-record renderer w h *om-record-pict-scale*)))
             (%save-score-page-state state previous)
             (setf saved t)
             (%enter-score-page renderer h font)
             (setf delivered t)
             state)
        ;; only a state that REACHED the caller will ever be closed; on any other
        ;; exit the recorder, the globals and the context are owed back right here
        (unless delivered
          (unwind-protect (when state (alld:end-record (%om-score-page-record state)))
            (when saved (%restore-score-page-globals state))
            (alld:release-gl-context previous)))))))

(defun om-close-score-page (port size)
  (declare (ignore size))
  (when (%om-score-page-p port)
    (unwind-protect (alld:end-record (%om-score-page-record port))
      (%restore-score-page-globals port)
      (alld:release-gl-context (%om-score-page-saved-gl-context port)))))
