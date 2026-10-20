(in-package :alloy-desktop)

;;; ---- Drawing pass state ---------------------------------------------------
;;; The port counts rows DOWN from the top left of the pane it draws on; alloy counts
;;; them up, so every primitive flips against *CURRENT-VIEW-HEIGHT*.

(defvar *current-renderer* nil
  "The renderer the primitives draw on, bound around one drawing pass.")
(defvar *current-fg-color* nil
  "The pattern the primitives draw with.")
(defvar *current-bg-color* nil
  "The pattern an erase paints.")
(defvar *current-pen-size* 1
  "The pen size of stroked primitives, in port units.")
(defvar *current-line-style* :solid
  "The line style of stroked primitives, :solid or :dashed.")
(defvar *current-dash-pattern* nil
  "Dash on/off lengths in px of stroked lines, tessellated by the port; NIL is solid.")
(defvar *current-view-height* 0
  "Pixel height of the pane drawn on, which flips port Y-down to alloy Y-up.")
(defvar *pox* 0
  "Port offset added to every x.")
(defvar *poy* 0
  "Port offset added to every y.")

;;; ---- Ports ------------------------------------------------------------------

(defclass graphics-port () ())

(defclass screen-port (graphics-port) ())

(defvar *screen-port* (make-instance 'screen-port)
  "The port every frame draws on: the primitives render through alloy.")

(defvar *current-port* *screen-port*
  "The port the primitives draw on; bound to a printer port around one page.")

(defgeneric port-draw-text (port x y string font size entry))
(defgeneric port-draw-line (port x1 y1 x2 y2))
(defgeneric port-draw-line-segments (port segments))
(defgeneric port-draw-rect (port x y w h pensize))
(defgeneric port-fill-rect (port x y w h))
(defgeneric port-draw-ellipse (port x y rx ry filled))
(defgeneric port-draw-ellipse-arc (port x y width height start sweep))
(defgeneric port-draw-polygon (port points))

;;; ---- Configuration ------------------------------------------------------------

(defparameter *pen-min-px* 1.5f0
  "Minimum visible line width in px, the floor of pen-width against the line shader's feather.")
(defparameter *pen-scale* 1.5f0
  "Multiplier on the pen size before clamping, which keeps thin and thick lines apart.")
(defparameter *round-corner-segments* 8
  "Line segments per quarter circle of an outlined rounded rectangle.")
(defparameter *glyph-shape-cache-limit* 256
  "Distinct strings memoized per font entry before its table is emptied rather than grown.")
(defparameter *glyph-shape-max-length* 64
  "Longest string whose glyph shape is memoized.")

;;; ---- Coordinates and pen --------------------------------------------------------

(declaim (inline flip-y flip-y-rect pen-width))
(defun flip-y (y) (- *current-view-height* y))
(defun flip-y-rect (y h) (- *current-view-height* y h))
(defun pen-width (pen) (max *pen-min-px* (* *pen-scale* (float pen 0f0))))

(defun rounded-rect-points (x y w h radius)
  "Closed point-vector for an outlined rounded rectangle in Alloy coords."
  (let* ((segs (max 1 *round-corner-segments*))
         (r    (max 0f0 (min (float radius 0f0)
                             (/ (min (float w 0f0) (float h 0f0)) 2f0))))
         (n    (1+ (* 4 (1+ segs))))
         (pts  (make-array n))
         (i    0)
         (centers `((,(+ x w (- r)) ,(+ y h (- r)) 0f0)
                    (,(+ x r)       ,(+ y h (- r)) ,(float (/ pi 2) 0f0))
                    (,(+ x r)       ,(+ y r)       ,(float pi 0f0))
                    (,(+ x w (- r)) ,(+ y r)       ,(float (* 3/2 pi) 0f0)))))
    (dolist (c centers)
      (loop for k from 0 to segs
            for a = (+ (third c) (* (/ (float k 0f0) (float segs 0f0)) (float (/ pi 2) 0f0)))
            do (setf (aref pts i)
                     (alloy:px-point (+ (first c)  (* r (cos a)))
                                     (+ (second c) (* r (sin a)))))
               (incf i)))
    (setf (aref pts i) (aref pts 0))
    pts))

;;; ---- Clip ---------------------------------------------------------------------
;;; A GDI port rejects a clipped-out call before it costs anything; here the
;;; primitives ask first whether their box can reach the clip.

(defun active-clip ()
  "The clip in force as an Alloy px extent, or NIL when the transform it was taken under is no longer the current one (alloy renderers/opengl/renderer.lisp: the simple:clip for (renderer shape))."
  (when (boundp 'org.shirakumo.alloy.renderers.opengl::*clip-region*)
    (let ((region org.shirakumo.alloy.renderers.opengl::*clip-region*))
      (when (and (cdr region)
                 (equalp (car region) (simple:transform-matrix *current-renderer*)))
        (cdr region)))))

(declaim (inline box-outside-clip-p))
(defun box-outside-clip-p (clip left bottom right top)
  "True when the Alloy px box lies wholly outside CLIP."
  (let ((cl (alloy:pxx clip)) (cb (alloy:pxy clip)))
    (or (>= left (+ cl (alloy:pxw clip))) (<= right cl)
        (>= bottom (+ cb (alloy:pxh clip))) (<= top cb))))

(defun clipped-out-p (left bottom right top)
  "True when the Alloy px box can produce no pixel the clip in force would keep."
  (let ((c (active-clip)))
    (and c (box-outside-clip-p c left bottom right top))))

(defun call-with-clip (left top width height thunk)
  "Call THUNK with the drawing clipped to the port rectangle LEFT TOP WIDTH HEIGHT."
  (simple:with-pushed-transforms (*current-renderer*)
    (simple:clip *current-renderer*
                 (alloy:px-extent (+ left *pox*)
                                  (flip-y-rect (+ top *poy*) height)
                                  width
                                  height))
    (funcall thunk)))

;;; ---- Text ---------------------------------------------------------------------

(defun baseline-descent (font px)
  "Px from the :bottom anchor of a text shape of FONT at PX up to its baseline, 0 for a font that is not MSDF."
  (if (typep font 'org.shirakumo.alloy.renderers.opengl.msdf::font)
      (progn
        (org.shirakumo.alloy.renderers.opengl.msdf::partial-allocate font)
        (baseline-rise font px))
      0))

(defun entry-baseline-descent (entry font)
  "ENTRY's memoized baseline descent of FONT, re-derived whenever the entry's draw size in px changes."
  (let ((px (alloy:to-px (font-entry-size-unit entry))))
    (if (eql px (font-entry-descent-px entry))
        (font-entry-descent entry)
        (setf (font-entry-descent-px entry) px
              (font-entry-descent entry) (baseline-descent font px)))))

(defun glyph-shape (entry font str x y)
  "ENTRY's memoized MSDF shape for STR moved to the Alloy origin (X Y), or a fresh shape when STR is too long to memoize."
  (let ((px (alloy:to-px (font-entry-size-unit entry))))
    (unless (eql px (font-entry-glyphs-px entry))
      (setf (font-entry-glyphs-px entry) px
            (font-entry-glyphs entry) (make-hash-table :test 'equal)))
    (let ((table (font-entry-glyphs entry))
          (fx (float x 0f0))
          (fy (float y 0f0)))
      (let ((memo (gethash str table)))
        (if memo
            (let ((shape (svref memo 0)))
              (setf (org.shirakumo.alloy.renderers.opengl.msdf::location shape)
                    (alloy:px-point (+ fx (svref memo 1)) (+ fy (svref memo 2))))
              shape)
            (let ((shape (simple:text *current-renderer* (alloy:px-extent x y 0 0) str
                                      :font    font
                                      :size    (font-entry-size-unit entry)
                                      :pattern *current-fg-color*)))
              (when (and (<= (length str) *glyph-shape-max-length*)
                         (typep shape 'org.shirakumo.alloy.renderers.opengl.msdf::text))
                (when (>= (hash-table-count table) *glyph-shape-cache-limit*)
                  (clrhash table))
                (let ((location (org.shirakumo.alloy.renderers.opengl.msdf::location shape)))
                  (setf (gethash (copy-seq str) table)
                        (vector shape
                                (- (alloy:pxx location) fx)
                                (- (alloy:pxy location) fy)))))
              shape))))))

(defun string-clipped-out-p (x y length px)
  "True when no glyph of a LENGTH-character string drawn at PX with its baseline origin at the Alloy px point (X Y) can reach the active clip."
  (let ((c (active-clip)))
    (and c
         (let ((em (* 4 (max 1 px))))
           (box-outside-clip-p c (- x em) (- y (* 2 em))
                               (+ x em (* em length)) (+ y (* 2 em)))))))

(defmethod port-draw-text ((port screen-port) x y string font size entry)
  (let* ((string (if (find #\Tab string) (substitute #\Space #\Tab string) string))
         (px     (alloy:to-px size))
         (memo   (and entry (eq font (font-entry-font entry)) entry))
         (xx     (+ x *pox*))
         (yy     (flip-y (+ y *poy* (if memo
                                        (entry-baseline-descent memo font)
                                        (baseline-descent font px))))))
    (unless (string-clipped-out-p xx yy (length string) px)
      (let ((shape (and memo (glyph-shape memo font string xx yy))))
        (if shape
            (progn (setf (simple:pattern shape) *current-fg-color*)
                   (alloy:render *current-renderer* shape))
            (alloy:render *current-renderer*
                          (simple:text *current-renderer*
                                       (alloy:px-extent xx yy 0 0)
                                       string
                                       :font    font
                                       :size    size
                                       :pattern *current-fg-color*)))))))

;;; ---- Lines ----------------------------------------------------------------------

(defun dash-line-segments (x1 y1 x2 y2 pattern)
  "Solid sub-segments (sx sy ex ey) tessellating the line by PATTERN (on off ...) from its start, the whole line for an empty PATTERN or a zero length, the phase restarting at every call as in CAPI (capi-gp-ref-78.htm:106-109)."
  (let* ((dx (- x2 x1)) (dy (- y2 y1))
         (len (sqrt (+ (* dx dx) (* dy dy))))
         (pat (remove-if-not #'realp pattern)))
    (if (or (null pat) (zerop len))
        (list (list x1 y1 x2 y2))
        (let ((ux (/ dx len)) (uy (/ dy len)) (segs nil) (pos 0f0) (i 0) (on t))
          (loop while (< pos len) do
            (let* ((step (float (nth (mod i (length pat)) pat) 1f0))
                   (end  (min len (+ pos step))))
              (when (and on (> end pos))
                (push (list (+ x1 (* ux pos)) (+ y1 (* uy pos))
                            (+ x1 (* ux end)) (+ y1 (* uy end)))
                      segs))
              (when (<= step 0f0) (return))
              (setf pos end i (1+ i) on (not on))))
          (nreverse segs)))))

(defun draw-line-1 (x1 y1 x2 y2 style)
  (let* ((ax1 (+ x1 *pox*)) (ax2 (+ x2 *pox*))
         (ay1 (flip-y (+ y1 *poy*))) (ay2 (flip-y (+ y2 *poy*)))
         (pen (pen-width *current-pen-size*)))
    (unless (clipped-out-p (- (min ax1 ax2) pen) (- (min ay1 ay2) pen)
                           (+ (max ax1 ax2) pen) (+ (max ay1 ay2) pen))
      (alloy:render *current-renderer*
                    (simple:line-strip *current-renderer*
                                       (list (alloy:px-point ax1 ay1)
                                             (alloy:px-point ax2 ay2))
                                       :pattern *current-fg-color*
                                       :line-width pen
                                       :line-style style)))))

(defmethod port-draw-line ((port screen-port) x1 y1 x2 y2)
  (if *current-dash-pattern*
      (dolist (seg (dash-line-segments x1 y1 x2 y2 *current-dash-pattern*))
        (destructuring-bind (sx sy ex ey) seg (draw-line-1 sx sy ex ey :solid)))
      (draw-line-1 x1 y1 x2 y2 *current-line-style*)))

(defun opengl-render-line-segments (segments)
  "Render opaque SEGMENTS in one OpenGL submission, preserving port coordinates."
  (let* ((renderer *current-renderer*)
         (shape (make-instance
                 'org.shirakumo.alloy.renderers.opengl::line-strip
                 :points (list (alloy:px-point 0 0) (alloy:px-point 1 0))
                 :pattern *current-fg-color*
                 :line-width (pen-width *current-pen-size*)
                 :line-style *current-line-style*))
         (data (make-array 0 :element-type 'single-float
                             :adjustable t :fill-pointer 0)))
    (dolist (segment segments)
      (destructuring-bind (x1 y1 x2 y2) segment
        (multiple-value-bind (line-data point-data)
            (org.shirakumo.alloy.renderers.opengl::make-line-array
             (list (alloy:px-point (+ x1 *pox*) (flip-y (+ y1 *poy*)))
                   (alloy:px-point (+ x2 *pox*) (flip-y (+ y2 *poy*))))
             (simple:line-width shape)
             (simple:cap-style shape)
             (simple:join-style shape))
          (declare (ignore point-data))
          (loop for value across line-data do (vector-push-extend value data)))))
    (setf (org.shirakumo.alloy.renderers.opengl::line-data shape) data
          (org.shirakumo.alloy.renderers.opengl::point-data shape)
          (make-array 0 :element-type 'single-float))
    (org.shirakumo.alloy.renderers.opengl::render-direct
     shape renderer *current-fg-color*)))

(defun draw-line-segments (segments)
  "Draw SEGMENTS, each (x1 y1 x2 y2), on the current port, batching only on OpenGL."
  (port-draw-line-segments *current-port* segments))

(defmethod port-draw-line-segments ((port screen-port) segments)
  (cond ((null segments) nil)
        ((and (typep *current-renderer*
                     'org.shirakumo.alloy.renderers.opengl::renderer)
              (null *current-dash-pattern*))
         (opengl-render-line-segments segments))
        (t
         (dolist (segment segments)
           (apply #'port-draw-line *current-port* segment)))))

(defun draw-point (x y)
  "Paint the one pixel at port (X Y)."
  (alloy:render *current-renderer*
                (simple:rectangle *current-renderer*
                                  (alloy:px-extent (+ x *pox*) (flip-y-rect (+ y *poy*) 1) 1 1)
                                  :pattern *current-fg-color*)))

;;; ---- Rectangles -------------------------------------------------------------------

(defun normalize-rectangle (x y w h)
  "X Y W H as a rectangle with a non-negative width and height."
  (if (or (minusp w) (minusp h))
      (values (min x (+ x w)) (min y (+ y h)) (abs w) (abs h))
      (values x y w h)))

(defun rect-outline-bands (left top wi he n)
  "The four non-overlapping whole-pixel bands of an N px outline of the port rectangle, growing inward, or the whole box when it is too small for two bands."
  (let ((x0 (round left)) (y0 (round top))
        (bw (1+ (round wi))) (bh (1+ (round he))))
    (if (or (<= bw (* 2 n)) (<= bh (* 2 n)))
        (list (list x0 y0 bw bh))
        (list (list x0 y0 bw n)
              (list x0 (+ y0 bh (- n)) bw n)
              (list x0 (+ y0 n) n (- bh (* 2 n)))
              (list (+ x0 bw (- n)) (+ y0 n) n (- bh (* 2 n)))))))

(defmethod port-draw-rect ((port screen-port) left top wi he pensize)
  (let ((ax (+ left *pox*)) (ay (flip-y-rect (+ top *poy*) he)) (pen (pen-width pensize)))
    (unless (clipped-out-p (- ax pen) (- ay pen) (+ ax wi pen) (+ ay he pen))
      (if (eq *current-line-style* :solid)
          ;; solid outline: N whole-pixel bands, as a GDI pen of that thickness resolves
          (fill-rectangles
           (rect-outline-bands left top wi he (max 1 (round pensize))))
          (alloy:render *current-renderer*
                        (simple:rectangle *current-renderer*
                                          (alloy:px-extent ax ay wi he)
                                          :pattern *current-fg-color*
                                          :line-width pen
                                          :line-style *current-line-style*))))))

(defmethod port-fill-rect ((port screen-port) left top wi he)
  (let ((ax (+ left *pox*)) (ay (flip-y-rect (+ top *poy*) he)))
    (unless (clipped-out-p ax ay (+ ax wi) (+ ay he))
      (alloy:render *current-renderer*
                    (simple:rectangle *current-renderer*
                                      (alloy:px-extent ax ay wi he)
                                      :pattern *current-fg-color*)))))

(defun opengl-render-filled-rectangles (rectangles)
  "Render opaque RECTANGLES in one OpenGL submission, preserving port coordinates."
  (let* ((renderer *current-renderer*)
         (data (make-array 0 :element-type 'single-float
                             :adjustable t :fill-pointer 0)))
    (labels ((vertex (x y)
               (vector-push-extend (float x 0f0) data)
               (vector-push-extend (float y 0f0) data)))
      (dolist (rectangle rectangles)
        (destructuring-bind (x y width height) rectangle
          (let* ((left (+ x *pox*))
                 (bottom (flip-y-rect (+ y *poy*) height))
                 (right (+ left width))
                 (top (+ bottom height)))
            (vertex left bottom) (vertex right bottom) (vertex right top)
            (vertex left bottom) (vertex right top) (vertex left top)))))
    (let ((shader (org.shirakumo.alloy.renderers.opengl::resource
                   'org.shirakumo.alloy.renderers.opengl::basic-shader renderer)))
      (org.shirakumo.alloy.renderers.opengl::update-vertex-buffer
       (org.shirakumo.alloy.renderers.opengl::resource
        'org.shirakumo.alloy.renderers.opengl::stream-vbo renderer)
       data)
      (org.shirakumo.alloy.renderers.opengl::bind shader)
      (setf (org.shirakumo.alloy.renderers.opengl::uniform shader "transform")
            (simple:transform-matrix renderer)
            (org.shirakumo.alloy.renderers.opengl::uniform shader "color")
            *current-fg-color*
            (org.shirakumo.alloy.renderers.opengl::uniform shader "feather")
            0.0f0)
      (org.shirakumo.alloy.renderers.opengl::draw-vertex-array
       (org.shirakumo.alloy.renderers.opengl::resource
        'org.shirakumo.alloy.renderers.opengl::stream-vao renderer)
       :triangles 0 (/ (length data) 2)))))

(defun fill-rectangles (rectangles)
  "Fill RECTANGLES, each (x y w h), with the current state, batching only on OpenGL."
  (cond ((null rectangles) nil)
        ((typep *current-renderer*
                'org.shirakumo.alloy.renderers.opengl::renderer)
         (opengl-render-filled-rectangles rectangles))
        (t
         (dolist (rectangle rectangles)
           (multiple-value-call #'port-fill-rect *current-port*
             (apply #'normalize-rectangle rectangle))))))

(defun fill-rectangle (x y w h pattern)
  "Paint the port rectangle X Y W H with PATTERN, whatever the clip."
  (alloy:render *current-renderer*
                (simple:rectangle *current-renderer*
                                  (alloy:px-extent (+ x *pox*) (flip-y-rect (+ y *poy*) h) w h)
                                  :pattern pattern)))

(defun draw-round-rect (x y w h radius pensize)
  "Outline the port rectangle X Y W H with corners of RADIUS."
  (let ((pts (rounded-rect-points (+ x *pox*) (flip-y-rect (+ y *poy*) h) w h radius)))
    (alloy:render *current-renderer*
                  (simple:line-strip *current-renderer* pts
                                     :pattern    *current-fg-color*
                                     :line-width (pen-width pensize)
                                     :line-style *current-line-style*
                                     :closed     t))))

(defun fill-round-rect (x y w h radius)
  "Fill the port rectangle X Y W H with corners of RADIUS."
  (alloy:render *current-renderer*
                (simple:rectangle *current-renderer*
                                  (alloy:px-extent (+ x *pox*) (flip-y-rect (+ y *poy*) h) w h)
                                  :pattern *current-fg-color*
                                  :corner-radii (float radius 0f0))))

;;; ---- Ellipses ---------------------------------------------------------------------

(defmethod port-draw-ellipse ((port screen-port) x y rx ry filled)
  (if filled
      (alloy:render *current-renderer*
                    (simple:ellipse *current-renderer*
                                    (alloy:px-extent (- (+ x *pox*) rx)
                                                     (flip-y-rect (- (+ y *poy*) ry) (* 2 ry))
                                                     (* 2 rx) (* 2 ry))
                                    :pattern *current-fg-color*))
      (alloy:render *current-renderer*
                    (simple:ellipse *current-renderer*
                                    (alloy:px-extent (- (+ x *pox*) rx)
                                                     (flip-y-rect (- (+ y *poy*) ry) (* 2 ry))
                                                     (* 2 rx) (* 2 ry))
                                    :pattern *current-fg-color*
                                    :line-width (pen-width *current-pen-size*)
                                    :line-style *current-line-style*))))

(defun ellipse-arc-points (x y w h start sweep)
  "Point-vector along the elliptical arc inscribed in the Alloy box (X Y W H), angles counter-clockwise from +x in Alloy's Y-up space."
  (let* ((rx (/ (float w 0f0) 2f0))
         (ry (/ (float h 0f0) 2f0))
         (cx (+ (float x 0f0) rx))
         (cy (+ (float y 0f0) ry))
         (segs (max 8 (min 256 (ceiling (* (abs sweep) (max rx ry)) 3))))
         (pts (make-array (1+ segs))))
    (dotimes (i (1+ segs) pts)
      (let ((a (+ start (* sweep (/ (float i 0f0) (float segs 0f0))))))
        (setf (aref pts i) (alloy:px-point (+ cx (* rx (cos a)))
                                           (+ cy (* ry (sin a)))))))))

;;; SIMPLE:ELLIPSE normalises the stroke by the larger axis (alloy renderers/opengl/
;;; renderer.lisp: the render-direct for simple:outlined-ellipse), so an arc is a polyline.
(defmethod port-draw-ellipse-arc ((port screen-port) x y width height start-angle sweep-angle)
  (let ((w (max 1 width))
        (h (max 1 height)))
    (alloy:render *current-renderer*
                  (simple:line-strip *current-renderer*
                                     (ellipse-arc-points (+ x *pox*) (flip-y-rect (+ y *poy*) h) w h
                                                         (float start-angle 0f0)
                                                         (float sweep-angle 0f0))
                                     :pattern    *current-fg-color*
                                     :line-width (pen-width *current-pen-size*)
                                     :line-style *current-line-style*))))

;;; ---- Polygons -----------------------------------------------------------------------

(defun polygon-point (p)
  (if (consp p)
      (alloy:px-point (+ (first p) *pox*) (flip-y (+ (second p) *poy*)))
      p))

(defun polygon-points (points)
  "POINTS -- (x y) lists, a flat x y list, or alloy points -- as alloy points of the pane."
  (if (and points (numberp (first points)))
      (loop for (px py) on points by #'cddr
            collect (alloy:px-point (+ px *pox*) (flip-y (+ py *poy*))))
      (mapcar #'polygon-point points)))

(defmethod port-draw-polygon ((port screen-port) points)
  (let ((pts (polygon-points points)))
    (alloy:render *current-renderer*
                  (simple:line-strip *current-renderer*
                                     (append pts (list (first pts)))
                                     :pattern *current-fg-color*
                                     :line-width (pen-width *current-pen-size*)
                                     :line-style *current-line-style*))))

(defun proper-segment-intersection (a b c d)
  "The interior intersection point of AB and CD, or NIL, endpoints excluded so that a merely touching quad is not a bow-tie."
  (let* ((ax (alloy:pxx a)) (ay (alloy:pxy a))
         (rx (- (alloy:pxx b) ax)) (ry (- (alloy:pxy b) ay))
         (cx (alloy:pxx c)) (cy (alloy:pxy c))
         (sx (- (alloy:pxx d) cx)) (sy (- (alloy:pxy d) cy))
         (den (- (* rx sy) (* ry sx))))
    (unless (zerop den)
      (let* ((qx (- cx ax)) (qy (- cy ay))
             (u (/ (- (* qx ry) (* qy rx)) den))
             (tt (/ (- (* qx sy) (* qy sx)) den)))
        (when (and (< 0 tt 1) (< 0 u 1))
          (alloy:px-point (+ ax (* tt rx)) (+ ay (* tt ry))))))))

(defun even-odd-quad-polygons (points)
  "For a self-intersecting quadrilateral, return one triangle-fan polygon: the even-odd fill of a bow-tie in one draw call."
  (when (= 4 (length points))
    (destructuring-bind (a b c d) points
      (or (let ((i (proper-segment-intersection a b c d)))
            (and i (list (list i b c i d a))))
          (let ((i (proper-segment-intersection b c d a)))
            (and i (list (list i a b i c d))))))))

(defun fill-polygon (points)
  "Fill the polygon POINTS (see polygon-points) with the even-odd rule of a GDI port."
  (let* ((pts (polygon-points points))
         (polygons (even-odd-quad-polygons pts)))
    (dolist (polygon (or polygons (list pts)))
      (alloy:render *current-renderer*
                    (simple:polygon *current-renderer* polygon
                                    :pattern *current-fg-color*)))))

;;; SIMPLE:POLYGON is one triangle FAN per shape, so a figure of thousands of
;;; same-coloured quads costs a draw call each; a strip submits the whole figure.
(defun fill-triangle-strip (coords)
  "Draw COORDS -- port x y pairs already in triangle-strip order -- as ONE strip in the current foreground."
  (declare (type (simple-array single-float (*)) coords))
  (let* ((renderer *current-renderer*)
         (n (length coords))
         (data (make-array n :element-type 'single-float)))
    (loop for i from 0 below n by 2
          do (setf (aref data i)      (float (+ (aref coords i) *pox*) 0f0)
                   (aref data (1+ i)) (float (flip-y (+ (aref coords (1+ i)) *poy*)) 0f0)))
    (let ((shader (org.shirakumo.alloy.renderers.opengl:resource
                   'org.shirakumo.alloy.renderers.opengl::basic-shader renderer)))
      (org.shirakumo.alloy.renderers.opengl:update-vertex-buffer
       (org.shirakumo.alloy.renderers.opengl:resource
        'org.shirakumo.alloy.renderers.opengl::stream-vbo renderer)
       data)
      (org.shirakumo.alloy.renderers.opengl:bind shader)
      (setf (org.shirakumo.alloy.renderers.opengl:uniform shader "transform")
            (simple:transform-matrix renderer))
      (setf (org.shirakumo.alloy.renderers.opengl:uniform shader "color")
            *current-fg-color*)
      (org.shirakumo.alloy.renderers.opengl:draw-vertex-array
       (org.shirakumo.alloy.renderers.opengl:resource
        'org.shirakumo.alloy.renderers.opengl::stream-vao renderer)
       :triangle-strip 0 (floor n 2)))))
