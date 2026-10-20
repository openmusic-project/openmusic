(in-package :alloy-desktop)

;;; ---- Graphics-state transform ---------------------------------------------------

(defun polygon-vertices (points)
  "CAPI point description: a flat sequence of alternating x y (capi-gp-ref-32.htm:54)."
  (loop for (x y) on (coerce points 'list) by #'cddr
        while y collect (list x y)))

(defvar *port-transform* nil
  "Current port transform, a six-element list (a b c d e f) or NIL for the unit transform (capi-gp-ref-141.htm:44, capi-gp-ref-107.htm:40,60-65).")

(defun transform-xy (x y)
  "Apply *PORT-TRANSFORM* to the port (y-down) point X,Y: x' = ax+cy+e, y' = bx+dy+f."
  (let ((m *port-transform*))
    (if (and (consp m) (= 6 (length m)))
        (destructuring-bind (a b c d e f) m
          (values (+ (* a x) (* c y) e)
                  (+ (* b x) (* d y) f)))
        (values x y))))

(defun transform-matrix ()
  "*PORT-TRANSFORM* conjugated by the y-flip y_alloy = H - y as an Alloy 4x4 -- x' = ax - cy + (cH + e), y' = -bx + dy + (H(1-d) - f) -- or NIL."
  (let ((m *port-transform*))
    (when (and (consp m) (= 6 (length m)))
      (destructuring-bind (a b c d e f) m
        (let ((h (float *current-view-height* 0f0)))
          (simple:matrix a       (- c) 0 (+ (* c h) e)
                         (- b)   d     0 (- h (* d h) f)
                         0       0     1 0
                         0       0     0 1))))))

(defun call-with-transform (thunk)
  (let ((mm (transform-matrix)))
    (if mm
        (simple:with-pushed-transforms (*current-renderer*)
          (simple:add-matrix *current-renderer* mm)
          (funcall thunk))
        (funcall thunk))))

;;; ---- Compositing, line ends and joints ------------------------------------------

(defparameter *composite-modes*
  '((:over . :source-over) (:copy . :source) (:clear . :clear)
    (:in . :source-in) (:out . :source-out)
    (:dest-over . :destination-over) (:dest-in . :destination-in)
    (:dest-out . :destination-out) (:dest-atop . :destination-atop)
    (:xor . :xor) (:add . :add))
  "CAPI compositing-mode -> Alloy simple:composite-mode (capi-gp-ref-78.htm:186-197 against alloy renderers/opengl/renderer.lisp: the (setf simple:composite-mode) :before for renderer), without :ATOP, which Alloy does not name.")

(defun composite-mode (mode)
  (cdr (assoc mode *composite-modes*)))

(defun call-with-composite-mode (mode thunk)
  (let ((new (and mode (composite-mode mode))))
    (cond
      ;; Source-atop is the one documented CAPI mode Alloy does not name.
      ;; Its Porter-Duff factors are nevertheless available in OpenGL.
      ((eq mode :atop)
       (let ((old (simple:composite-mode *current-renderer*)))
         (unwind-protect
              (progn
                (gl:blend-func-separate
                 :dst-alpha :one-minus-src-alpha :one :one-minus-src-alpha)
                (gl:blend-equation :func-add)
                (funcall thunk))
           (setf (simple:composite-mode *current-renderer*) old))))
      (new
        (let ((old (simple:composite-mode *current-renderer*)))
          (unwind-protect
               (progn (setf (simple:composite-mode *current-renderer*) new)
                      (funcall thunk))
            (setf (simple:composite-mode *current-renderer*) old))))
      (t (funcall thunk)))))

(defun cap-style (style)
  "capi-gp-ref-78.htm:110-111 :butt (default) / :round / :projecting; NIL is the butt end, for which Alloy emits no cap geometry (alloy renderers/opengl/renderer.lisp: the cap in make-line-array)."
  (ecase style
    ((nil :butt) nil)
    (:round :round)
    (:projecting :square)))

(defun join-style (style)
  "capi-gp-ref-78.htm:112-113 :bevel / :miter (default) / :round, one to one with Alloy's join styles (alloy renderers/opengl/renderer.lisp: the join-style in make-line-array)."
  (ecase style
    ((nil :miter) :miter)
    (:bevel :bevel)
    (:round :round)))

;;; ---- Mask -------------------------------------------------------------------------

(defun call-with-mask (mask thunk)
  "Apply the documented rectangle or quality-path graphics-state mask."
  (cond
    ((null mask) (funcall thunk))
    ((and (consp mask) (= 4 (length mask)) (every #'realp mask))
     (destructuring-bind (x y w h) mask
       (call-with-clip x y w h thunk)))
    ((and (consp mask) (eq (first mask) :path))
     (call-with-path-mask mask thunk))
    (t (error "Invalid GP graphics-state mask ~S." mask))))

;;; ---- Tessellation -------------------------------------------------------------------

(defun segment-intersection-y (edge-a edge-b)
  "Interior intersection Y of two non-parallel polygon edges, or NIL."
  (destructuring-bind ((ax ay) (bx by)) edge-a
    (destructuring-bind ((cx cy) (dx dy)) edge-b
      (let* ((rx (- bx ax)) (ry (- by ay))
             (sx (- dx cx)) (sy (- dy cy))
             (den (- (* rx sy) (* ry sx))))
        (unless (zerop den)
          (let* ((qx (- cx ax)) (qy (- cy ay))
                 (tt (/ (- (* qx sy) (* qy sx)) den))
                 (u (/ (- (* qx ry) (* qy rx)) den)))
            (when (and (< 0 tt 1) (< 0 u 1))
              (+ ay (* tt ry)))))))))

(defun polygon-edges (points)
  (loop for tail on points
        for a = (first tail)
        for b = (or (second tail) (first points))
        unless (equal a b) collect (list a b)))

(defun edge-x-at-y (edge y)
  (destructuring-bind ((x0 y0) (x1 y1)) edge
    (+ x0 (* (- y y0) (/ (- x1 x0) (- y1 y0))))))

(defun crossing-groups (crossings)
  (let ((groups nil))
    (dolist (crossing crossings (nreverse groups))
      (if (and groups
               (< (abs (- (first crossing)
                          (first (first (first groups)))))
                  1.0e-6))
          (push crossing (first groups))
          (push (list crossing) groups)))))

(defun filled-edge-pairs (edges y fill-rule)
  "Return left/right boundary edge pairs for the filled intervals at Y."
  (let* ((crossings
           (sort
            (loop for edge in edges
                  for y0 = (second (first edge))
                  for y1 = (second (second edge))
                  when (or (< y0 y y1) (< y1 y y0))
                    collect (list (edge-x-at-y edge y)
                                  (if (< y0 y1) 1 -1) edge))
            #'< :key #'first))
         (groups (crossing-groups crossings))
         (inside nil)
         (winding 0)
         (left-edge nil)
         (pairs nil))
    (dolist (group groups (nreverse pairs))
      (ecase fill-rule
        (:even-odd
         (when (oddp (length group))
           (if inside
               (progn
                 (push (list left-edge (third (first group))) pairs)
                 (setf inside nil left-edge nil))
               (setf inside t left-edge (third (first group))))))
        (:winding
         (let ((old winding))
           (incf winding (loop for crossing in group sum (second crossing)))
           (cond ((and (zerop old) (not (zerop winding)))
                  (setf left-edge (third (first group))))
                 ((and (not (zerop old)) (zerop winding))
                  (push (list left-edge (third (first group))) pairs)
                  (setf left-edge nil)))))))))

(defun edge-fill-cells (edges fill-rule)
  "Tessellate closed EDGES under FILL-RULE into trapezoids of horizontal slabs split at every vertex and proper edge intersection, each safe for Alloy's triangle fan."
  (let* ((events
           (sort
            (remove-duplicates
             (append (loop for edge in edges
                           append (list (second (first edge))
                                        (second (second edge))))
                     (loop for tail on edges
                           append (loop for other in (rest tail)
                                        for y = (segment-intersection-y
                                                 (first tail) other)
                                        when y collect y)))
             :test #'=)
            #'<))
         (cells nil))
    (loop for (y0 y1) on events
          while y1
          when (< y0 y1)
            do (let ((mid (/ (+ y0 y1) 2)))
                 (dolist (pair (filled-edge-pairs edges mid fill-rule))
                   (destructuring-bind (left right) pair
                     (let ((lx0 (edge-x-at-y left y0))
                           (lx1 (edge-x-at-y left y1))
                           (rx0 (edge-x-at-y right y0))
                           (rx1 (edge-x-at-y right y1)))
                       (unless (and (= lx0 rx0) (= lx1 rx1))
                         (push (list (list lx0 y0) (list rx0 y0)
                                     (list rx1 y1) (list lx1 y1))
                               cells)))))))
    (nreverse cells)))

(defun polygon-fill-cells (points fill-rule)
  (edge-fill-cells (polygon-edges points) fill-rule))

;;; ---- Paths --------------------------------------------------------------------------

(defun affine-point (matrix x y)
  (destructuring-bind (a b c d e f) matrix
    (list (+ (* a x) (* c y) e)
          (+ (* b x) (* d y) f))))

(defun affine-compose (outer inner)
  "Return the affine transform that applies INNER and then OUTER."
  (destructuring-bind (a b c d e f) outer
    (destructuring-bind (p q r s u v) inner
      (list (+ (* a p) (* c q)) (+ (* b p) (* d q))
            (+ (* a r) (* c s)) (+ (* b r) (* d s))
            (+ (* a u) (* c v) e) (+ (* b u) (* d v) f)))))

(defun path-elements (path)
  (cond
    ((and (symbolp path) (fboundp path)) (path-elements (symbol-function path)))
    ((functionp path)
     (let ((elements nil))
       (funcall path (lambda (&rest element) (push element elements)))
       (nreverse elements)))
    ((typep path 'sequence) (coerce path 'list))
    (t (error "Invalid GP path specification ~S." path))))

(defun path-figures (path)
  "Flatten a LispWorks path specification into closed polygonal figures."
  (let ((figures nil) (figure nil) (current '(0 0)) (start nil))
    (labels ((finish-figure ()
               (when (and figure (cdr figure))
                 (push (nreverse figure) figures))
               (when start (setf current start))
               (setf figure nil start nil))
             (add-point (point)
               (unless figure
                 (setf start current figure (list current)))
               (unless (equal point current) (push point figure))
               (setf current point))
             (standalone (points)
               (when (cdr points) (push points figures)))
             (walk (elements matrix)
               (dolist (element (path-elements elements))
                 (unless (consp element)
                   (error "Invalid GP path element ~S." element))
                 (case (first element)
                   (:close (finish-figure))
                   (:move
                    (destructuring-bind (tag x y) element
                      (declare (ignore tag))
                      (finish-figure)
                      (setf current (affine-point matrix x y)
                            start current figure (list current))))
                   (:line
                    (destructuring-bind (tag x y) element
                      (declare (ignore tag))
                      (add-point (affine-point matrix x y))))
                   (:rectangle
                    (destructuring-bind (tag x y width height) element
                      (declare (ignore tag))
                      (standalone
                       (mapcar (lambda (point)
                                 (affine-point matrix (first point) (second point)))
                               (list (list x y) (list (+ x width) y)
                                     (list (+ x width) (+ y height))
                                     (list x (+ y height)))))))
                   (:ellipse
                    (destructuring-bind (tag cx cy rx ry) element
                      (declare (ignore tag))
                      (standalone
                       (loop for index below 48
                             for angle = (* 2 pi (/ index 48))
                             collect (affine-point
                                      matrix (+ cx (* rx (cos angle)))
                                      (- cy (* ry (sin angle))))))))
                   (:arc
                    (destructuring-bind
                        (tag x y width height begin sweep &optional move-p) element
                      (declare (ignore tag))
                      (let* ((cx (+ x (/ width 2))) (cy (+ y (/ height 2)))
                             (rx (/ width 2)) (ry (/ height 2))
                             (steps (max 1 (ceiling (abs sweep) (/ pi 24))))
                             (points
                               (loop for index from 0 to steps
                                     for angle = (+ begin (* sweep (/ index steps)))
                                     collect (affine-point
                                              matrix (+ cx (* rx (cos angle)))
                                              (- cy (* ry (sin angle)))))))
                        (when move-p
                          (finish-figure)
                          (setf current (first points) start current
                                figure (list current)))
                        (dolist (point points) (add-point point)))))
                   (:bezier
                    (destructuring-bind (tag x1 y1 x2 y2 x3 y3) element
                      (declare (ignore tag))
                      (let ((p0 current)
                            (p1 (affine-point matrix x1 y1))
                            (p2 (affine-point matrix x2 y2))
                            (p3 (affine-point matrix x3 y3)))
                        (loop for index from 1 to 32
                              for u = (/ index 32)
                              for v = (- 1 u)
                              do (add-point
                                  (list (+ (* v v v (first p0))
                                           (* 3 v v u (first p1))
                                           (* 3 v u u (first p2))
                                           (* u u u (first p3)))
                                        (+ (* v v v (second p0))
                                           (* 3 v v u (second p1))
                                           (* 3 v u u (second p2))
                                           (* u u u (second p3)))))))))
                   (:scale
                    (destructuring-bind (tag sx sy nested) element
                      (declare (ignore tag))
                      (walk nested (affine-compose matrix
                                                   (list sx 0 0 sy 0 0)))))
                   (:rotate
                    (destructuring-bind (tag angle nested) element
                      (declare (ignore tag))
                      (let ((c (cos angle)) (s (sin angle)))
                        (walk nested (affine-compose
                                      matrix (list c s (- s) c 0 0))))))
                   (:translate
                    (destructuring-bind (tag dx dy nested) element
                      (declare (ignore tag))
                      (walk nested (affine-compose
                                    matrix (list 1 0 0 1 dx dy)))))
                   (:transform
                    (destructuring-bind (tag transform nested) element
                      (declare (ignore tag))
                      (unless (and (consp transform) (= 6 (length transform))
                                   (every #'realp transform))
                        (error "Invalid GP path transform ~S." transform))
                      (walk nested (affine-compose matrix transform))))
                   (otherwise (error "Unknown GP path element ~S." element))))))
      (walk path '(1 0 0 1 0 0))
      (finish-figure)
      (nreverse figures))))

(defun render-stencil-cells (cells)
  (dolist (cell cells)
    (alloy:render *current-renderer*
                  (simple:polygon *current-renderer* (polygon-points cell)
                                  :pattern colors:black))))

(defun call-with-path-mask (mask thunk)
  (let* ((path (getf mask :path))
         (fill-rule (getf mask :fill-rule :even-odd)))
    (unless (member fill-rule '(:even-odd :winding))
      (error "GP path mask fill-rule must be :EVEN-ODD or :WINDING, not ~S."
             fill-rule))
    (let* ((figures (path-figures path))
           (edges (loop for figure in figures append (polygon-edges figure)))
           (cells (edge-fill-cells edges fill-rule)))
      (when cells
        (let ((depth org.shirakumo.alloy.renderers.opengl::*clip-depth*))
          (gl:stencil-func :equal depth #xff)
          (gl:stencil-op :keep :incr :incr)
          (gl:color-mask nil nil nil nil)
          (gl:depth-mask nil)
          (render-stencil-cells cells)
          (incf org.shirakumo.alloy.renderers.opengl::*clip-depth*)
          (gl:stencil-op :keep :keep :keep)
          (gl:stencil-func :lequal
                           org.shirakumo.alloy.renderers.opengl::*clip-depth* #xff)
          (gl:color-mask t t t t)
          (gl:depth-mask t)
          (unwind-protect
               (funcall thunk)
            (gl:stencil-func :equal
                             org.shirakumo.alloy.renderers.opengl::*clip-depth* #xff)
            (gl:stencil-op :keep :decr :decr)
            (gl:color-mask nil nil nil nil)
            (gl:depth-mask nil)
            (render-stencil-cells cells)
            (decf org.shirakumo.alloy.renderers.opengl::*clip-depth*)
            (gl:stencil-op :keep :keep :keep)
            (gl:stencil-func :lequal
                             org.shirakumo.alloy.renderers.opengl::*clip-depth* #xff)
            (gl:color-mask t t t t)
            (gl:depth-mask t)))))))

;;; ---- Patterns, shape mode and rendering ----------------------------------------------

(defun image-paint (texture width height)
  "Repeating Alloy paint of TEXTURE, an image of WIDTH x HEIGHT px, and the texture whose wrapping must be temporary repeat."
  (let* ((viewport (gl:get-integer :viewport 4))
         (viewport-width (max 1 (aref viewport 2)))
         (viewport-height (max 1 (aref viewport 3)))
         (paint
           (simple:image-pattern
            *current-renderer* texture
            :scaling (alloy:px-size
                      (/ (float width 0f0) viewport-width)
                      (/ (float height 0f0) viewport-height))
            :offset (alloy:px-point 0 0) :mode :repeat)))
    (values paint texture)))

(defun call-with-pattern-repeat (texture thunk)
  (if (null texture)
      (funcall thunk)
      (progn
        (org.shirakumo.alloy.renderers.opengl:bind texture)
        (let ((old-s (gl:get-tex-parameter :texture-2d :texture-wrap-s))
              (old-t (gl:get-tex-parameter :texture-2d :texture-wrap-t)))
          (unwind-protect
               (progn
                 (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
                 (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
                 (funcall thunk))
            (org.shirakumo.alloy.renderers.opengl:bind texture)
            (gl:tex-parameter :texture-2d :texture-wrap-s old-s)
            (gl:tex-parameter :texture-2d :texture-wrap-t old-t))))))

(defun call-with-shape-mode (mode thunk)
  (if (null mode)
      (funcall thunk)
      (let* ((plain-p (member mode '(:plain :fastest)))
             (old (gl:enabledp :multisample)))
        (unwind-protect
             (progn
               (if plain-p
                   (gl:disable :multisample)
                   (gl:enable :multisample))
               (funcall thunk))
          (if old
              (gl:enable :multisample)
              (gl:disable :multisample))))))

(defun call-with-pattern-clip-scope (pattern thunk)
  "Call THUNK with Alloy's single-region clip record rebound, so an image-pattern clip inside a mask intersects the inherited stencil depth and leaves the outer record intact."
  (if (and pattern
           (boundp
            'org.shirakumo.alloy.renderers.opengl::*clip-region*)
           (cdr org.shirakumo.alloy.renderers.opengl::*clip-region*))
      (let* ((matrix (make-array 16 :element-type 'single-float))
             (org.shirakumo.alloy.renderers.opengl::*clip-region*
               (cons matrix nil)))
        (declare (dynamic-extent matrix))
        (funcall thunk))
      (funcall thunk)))

(defun render-filled-polygon (points fill-rule paint)
  (dolist (cell (polygon-fill-cells points fill-rule))
    (alloy:render *current-renderer*
                  (simple:polygon *current-renderer* (polygon-points cell)
                                  :pattern paint))))

(defun stroke-polyline (points closed cap-style join-style &optional paint)
  "Stroke POINTS, port (x y) pairs, as ONE line strip, since a join only exists between consecutive segments of the same strip (alloy renderers/opengl/renderer.lisp: make-line-array)."
  (let ((pts (polygon-points
              (if closed (append points (list (first points))) points))))
    (alloy:render *current-renderer*
                  (simple:line-strip *current-renderer* pts
                                     :pattern (or paint *current-fg-color*)
                                     :line-width (pen-width *current-pen-size*)
                                     :line-style *current-line-style*
                                     :cap-style (cap-style cap-style)
                                     :join-style (join-style join-style)))))

(defun transform-scale (transform)
  "Area-preserving scalar fallback used only for dashed transformed strokes."
  (if (and (consp transform) (= 6 (length transform)))
      (destructuring-bind (a b c d e f) transform
        (declare (ignore e f))
        (sqrt (abs (- (* a d) (* b c)))))
      1))
