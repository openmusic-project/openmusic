(in-package :om-api)

(export '(om-make-point
          om-make-big-point
          om-point-p
          om-points-equal-p
          om-add-points
          om-subtract-points
          om-add-big-points
          om-subtract-big-points
          om-point-h
          om-point-x
          om-point-v
          om-point-y
          om-point-in-line-p
          om-point-*
          om-points-to-rect
          om-make-rect
          om-pts-to-rect
          om-sect-rect
          om-rect-empty
          om-rect-top
          om-rect-left
          om-rect-bottom
          om-rect-right
          om-rect-w
          om-rect-h
          om-rect-topleft
          om-rect-bottomright
          om-point-in-rect-p
          om-open-region
          om-close-region
          om-open-region-add-rect
          om-open-region-add-line
          om-new-region
          om-dispose-region
          om-region-add-rect
          om-region-add-line
          om-set-rect-region
          om-union-region
          om-point-in-region-p
          om-copy-region
          om-difference-region
          om-paint-region
          om-offset-region
          om-get-current-port
          om-with-focused-view
          om-with-clip-rect
          om-with-bg-color
          om-with-fg-color
          om-draw-char
          om-draw-string
          om-draw-line
          om-draw-point
          om-draw-rect
          om-draw-round-rect
          om-fill-round-rect
          om-fill-rect
          om-draw-ellipse
          om-fill-ellipse
          om-draw-ellipse-arc
          om-erase-rect-content
          om-erase-rect
          om-erase-line
          om-draw-polygon
          om-fill-polygon
          om-draw-selection-rect
          om-draw-hilite-rect
          om-draw-hilite-icon
          om-draw-rect-outline
          om-with-pen
          om-with-line-size
          om-with-dashline
          om-with-line
          om-with-hilite-foreground
          *om-select-color*
          *om-text-select-color*
          *om-black-pattern*
          *om-darkgray-pattern*
          *om-gray-pattern*
          *om-lightgray-pattern*
          *om-white-pattern*
          *om-dark-red-color*
          om-make-color
          om-make-color-alpha
          om-color-p
          om-color-r
          om-color-g
          om-color-b
          om-output-color
          om-color-to-capi
          om-correct-color
          *om-black-color*
          *om-dark-gray-color*
          *om-gray-color*
          *om-gray1-color*
          *om-light-gray-color*
          *om-white-color*
          *om-window-def-color*
          *om-transparent-color*
          *om-red-color*
          *om-red2-color*
          *om-blue-color*
          *om-green-color*
          *om-green2-color*
          *om-orange-color*
          *om-pink-color*
          *om-wine-color*
          *om-dark-blue-color*
          *om-yellow-color*
          *om-purple-color*
          *om-light-blue-color*
          *om-cafe-color*
          *om-salmon-color*
          *om-steel-blue-color*
          *om-beige-color*
          *om-button-bg-color*
          *om-button-hover-color*
          *om-button-active-color*
          *om-default-font1*
          *om-default-font2*
          *om-default-font3*
          *om-default-font4*
          *om-default-font1b*
          *om-default-font2b*
          *om-default-font3b*
          *om-default-font4b*
          *om-score-font-face*
          *om-def-font-face*
          *om-def-bold-font-face*
          *om-def-font-sizes*
          *om-controls-font*
          *om-string-avg-char-ratio*
          *om-string-line-height-ratio*
          om-make-font
          om-make-music-font
          om-make-font-px
          om-font-p
          om-font-face
          om-font-family
          om-font-size
          om-font-px-size
          om-font-mode
          om-font-style
          om-string-size
          om-string-h
          om-with-font
          om-make-font-object
          om-correct-point
          om-correct-font)
        :om-api)

;;; Compat type coercion

(defun om-correct-point (point)
  (cond ((om-point-p point) point)
        ((null point) point)
        ((numberp point)
         (om-make-point (- point (ash (ash point -16) 16)) (ash point -16)))
        ((consp point) (om-make-point (car point) (cadr point)))
        (t nil)))

;;; Points

(defstruct ompoint
  (x 0)
  (y 0))

(defmethod make-load-form ((self ompoint) &optional env)
  (declare (ignore env))
  `(make-ompoint :x ,(ompoint-x self) :y ,(ompoint-y self)))

(defun om-make-point (x y)
  (make-ompoint :x x :y y))

(defmethod om-point-p ((self t)) (ompoint-p self))

(defun om-make-big-point (x y)
  (make-ompoint :x x :y y))

(defmethod om-point-h ((point ompoint)) (ompoint-x point))
(defmethod om-point-v ((point ompoint)) (ompoint-y point))
(defmethod om-point-x ((point ompoint)) (ompoint-x point))
(defmethod om-point-y ((point ompoint)) (ompoint-y point))

(defmethod om-add-points (point1 point2)
  (om-make-point (+ (om-point-h point1) (om-point-h point2))
                 (+ (om-point-v point1) (om-point-v point2))))

(defmethod om-subtract-points (point1 point2)
  (om-make-point (round (- (om-point-h point1) (om-point-h point2)))
                 (round (- (om-point-v point1) (om-point-v point2)))))

(defun om-add-big-points (point1 point2)
  (om-add-points point1 point2))

(defun om-subtract-big-points (point1 point2)
  (om-subtract-points point1 point2))

(defmethod om-points-equal-p (point1 point2)
  (declare (ignore point1 point2))
  nil)

(defmethod om-points-equal-p ((point1 ompoint) (point2 ompoint))
  (and (equal (om-point-h point1) (om-point-h point2))
       (equal (om-point-v point1) (om-point-v point2))))

(defmethod print-point ((point ompoint))
  (let ((str (format nil "ompoint >>> h:~A v:~A" (om-point-h point) (om-point-v point))))
    (print str))
  point)

(defun dot-prod-2d (p1 p2)
  (+ (* (om-point-h p1) (om-point-h p2)) (* (om-point-v p1) (om-point-v p2))))

(defun norm-2d (p) (sqrt (dot-prod-2d p p)))

(defun dist2d (p1 p2) (norm-2d (om-subtract-points p1 p2)))

(defun dist-to-line (pt lp1 lp2)
  (let ((v (om-subtract-points lp2 lp1))
        (w (om-subtract-points pt lp1)))
    (let ((c1 (dot-prod-2d w v)))
      (if (<= c1 0)
          (dist2d pt lp1)
          (let ((c2 (dot-prod-2d v v)))
            (if (<= c2 c1)
                (dist2d pt lp2)
                (let* ((b (/ c1 c2))
                       (pb (om-add-points
                            lp1 (om-make-point (round (* b (om-point-h v)))
                                               (round (* b (om-point-v v)))))))
                  (dist2d pt pb))))))))

(defun om-point-in-line-p (pt lp1 lp2 delta)
  (<= (dist-to-line pt lp1 lp2) delta))

(defmethod om-point-* ((point ompoint) fact)
  (om-make-point (* (om-point-h point) fact)
                 (* (om-point-v point) fact)))

(defmethod om-points-to-rect (p1 p2)
  (let ((x1 (min (om-point-x p1) (om-point-x p2)))
        (y1 (min (om-point-y p1) (om-point-y p2)))
        (x2 (max (om-point-x p1) (om-point-x p2)))
        (y2 (max (om-point-y p1) (om-point-y p2))))
    (values x1 y1 (- x2 x1) (- y2 y1))))

;;; Rects

(defclass om-rect ()
  ((rx :accessor rx :initarg :rx :initform 0)
   (ry :accessor ry :initarg :ry :initform 0)
   (rw :accessor rw :initarg :rw :initform 0)
   (rh :accessor rh :initarg :rh :initform 0)))

(defun om-make-rect (left top right bottom)
  (make-instance 'om-rect
                 :rx left :ry top
                 :rw (- right left) :rh (- bottom top)))

(defun om-pts-to-rect (ompoint1 ompoint2)
  (let ((left   (min (om-point-h ompoint1) (om-point-h ompoint2)))
        (top    (min (om-point-v ompoint1) (om-point-v ompoint2)))
        (right  (max (om-point-h ompoint1) (om-point-h ompoint2)))
        (bottom (max (om-point-v ompoint1) (om-point-v ompoint2))))
    (make-instance 'om-rect
                   :rx left :ry top
                   :rw (- right left) :rh (- bottom top))))

(defun om-rect-top (rect) (ry rect))
(defun om-rect-bottom (rect) (+ (ry rect) (rh rect)))
(defun om-rect-left (rect) (rx rect))
(defun om-rect-right (rect) (+ (rx rect) (rw rect)))
(defun om-rect-w (rect) (rw rect))
(defun om-rect-h (rect) (rh rect))
(defun om-rect-topleft (rect) (om-make-point (rx rect) (ry rect)))
(defun om-rect-bottomright (rect)
  (om-make-point (+ (rx rect) (rw rect)) (+ (ry rect) (rh rect))))

(defmethod rect-p ((self t)) nil)
(defmethod rect-p ((self om-rect)) t)

(defun om-sect-rect (rect1 rect2)
  (let* ((tx  (max (rx rect1) (rx rect2)))
         (ty  (max (ry rect1) (ry rect2)))
         (t2x (min (+ (rx rect1) (rw rect1)) (+ (rx rect2) (rw rect2))))
         (t2y (min (+ (ry rect1) (rh rect1)) (+ (ry rect2) (rh rect2)))))
    (if (or (< t2x tx) (< t2y ty))
        nil
        (om-make-rect tx ty t2x t2y))))

(defun om-rect-empty (rect) (equal rect nil))

(defun om-point-in-rect-p (point rect)
  (and (>= (om-point-h point) (rx rect))
       (<= (om-point-h point) (+ (rx rect) (rw rect)))
       (>= (om-point-v point) (ry rect))
       (<= (om-point-v point) (+ (ry rect) (rh rect)))))

(defun om-copy-rect (rect)
  (make-instance 'om-rect
                 :rx (rx rect) :ry (ry rect)
                 :rw (rw rect) :rh (rh rect)))

(defun print-rect (rect)
  (print (list (rx rect) (ry rect) (rw rect) (rh rect))))

;;; Regions

(defclass om-region ()
  ((viewport :initform nil :initarg :viewport :accessor viewport)
   (elements :initform nil :initarg :elements :accessor elements)))

(defvar *current-region* nil)

(defun om-open-region (view)
  (setf *current-region* (make-instance 'om-region :viewport view)))

(defun om-close-region (view)
  (declare (ignore view))
  *current-region*)

(defun om-open-region-add-rect (left top right bottom)
  (when *current-region*
    (push (om-make-rect left top right bottom) (elements *current-region*))))

(defun om-open-region-add-line (p1 p2 &optional (approx nil))
  (when *current-region*
    (push (list p1 p2 approx) (elements *current-region*))))

(defun print-region (region)
  (mapcar 'print-rect (elements region))
  region)

(defun om-region-bounds (region)
  "Bounding box (left top right bottom) over REGION's elements, or NIL if empty."
  (let (minx miny maxx maxy)
    (dolist (elt (elements region))
      (multiple-value-bind (l tp r b)
          (if (rect-p elt)
              (values (rx elt) (ry elt) (+ (rx elt) (rw elt)) (+ (ry elt) (rh elt)))
              (let ((x1 (om-point-h (first elt)))  (y1 (om-point-v (first elt)))
                    (x2 (om-point-h (second elt))) (y2 (om-point-v (second elt))))
                (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2))))
        (setf minx (if minx (min minx l) l)
              miny (if miny (min miny tp) tp)
              maxx (if maxx (max maxx r) r)
              maxy (if maxy (max maxy b) b))))
    (when minx (list minx miny maxx maxy))))

(defun om-new-region ()
  (make-instance 'om-region))

(defmethod om-dispose-region ((self om-region))
  (setf self nil))

(defmethod om-region-add-rect ((self om-region) left top right bottom)
  (push (om-make-rect (min left right) (min top bottom)
                      (max left right) (max top bottom))
        (elements self)))

(defmethod om-region-add-line ((self om-region) p1 p2 &optional (approx nil))
  (push (list p1 p2 approx) (elements self)))

(defun om-set-rect-region (region left top right bottom)
  (setf (elements region) (list (om-make-rect left top right bottom)))
  region)

(defun om-union-region (reg1 reg2)
  (let ((reg-union (make-instance 'om-region)))
    (setf (elements reg-union) (append (elements reg1) (elements reg2)))
    reg-union))

(defmethod om-point-in-region-p ((self om-region) where)
  (let ((in nil))
    (loop for partie in (elements self)
          while (not in)
          do (cond ((rect-p partie)
                    (when (om-point-in-rect-p where partie) (setf in t)))
                   (t (let ((p1 (first partie)) (p2 (second partie)) (approx (third partie)))
                        (unless (om-points-equal-p p1 p2)
                          (when (om-point-in-line-p where p1 p2 (or approx 2))
                            (setf in t)))))))
    in))

(defun om-copy-region (region)
  (let ((r (om-new-region)))
    (setf (elements r)
          (loop for elt in (elements region)
                collect (if (rect-p elt) (om-copy-rect elt) (copy-list elt))))
    r))

(defun om-difference-region (r1 r2 r3)
  (declare (ignore r2 r3))
  r1)

(defun om-paint-region (view region)
  (%call-with-focused-view view
   (lambda ()
     (loop for elt in (elements region)
           do (if (rect-p elt)
                  (om-draw-rect (rx elt) (ry elt) (rw elt) (rh elt))
                  (om-draw-line (om-point-h (first elt)) (om-point-v (first elt))
                                (om-point-h (second elt)) (om-point-v (second elt))))))))

(defun om-offset-region (region h v)
  (setf (elements region)
        (loop for elt in (elements region)
              collect (if (rect-p elt)
                          (om-make-rect (+ (rx elt) h) (+ (ry elt) v)
                                        (+ (rx elt) (rw elt) h) (+ (ry elt) (rh elt) v))
                          (list (om-add-points (first elt) (om-make-point h v))
                                (om-add-points (second elt) (om-make-point h v))
                                (third elt)))))
  region)

;;; Colors

(defclass omcolor ()
  ((c :accessor c :initarg :c)))

(defmethod c ((self symbol)) (or self :black))

(defmacro om-make-color (r g b &optional (a 1.0))
  `(make-instance 'omcolor :c (alld:rgb ,r ,g ,b ,a)))

(defmacro om-make-color-alpha (r g b a)
  `(make-instance 'omcolor :c (alld:rgb ,r ,g ,b ,a)))

(defgeneric om-color-p (object))
(defmethod om-color-p ((self t))       nil)
(defmethod om-color-p ((self omcolor)) t)

(defun om-color-r     (color) (chroma:r (c color)))
(defun om-color-g     (color) (chroma:g (c color)))
(defun om-color-b     (color) (chroma:b (c color)))
(defun om-color-alpha (color) (chroma:a (c color)))

(defmethod make-load-form ((self omcolor) &optional env)
  (declare (ignore env))
  `(make-instance 'omcolor
                  :c (chroma:rgb ,(om-color-r self) ,(om-color-g self)
                                 ,(om-color-b self) ,(om-color-alpha self))))

(defmethod print-object ((self omcolor) stream)
  (format stream "color R:~D G:~D B:~D" (om-color-r self) (om-color-g self) (om-color-b self)))

(defun %omcolor->chroma (oc)
  ;; NIL degrades to black, as vanilla does through (c color) + (c (self symbol))
  ;; (om-LW/graphics.lisp: om-with-fg-color, om-with-bg-color and the c method for
  ;; symbol); NIL is never a valid alloy palette key.
  (cond ((typep oc 'omcolor) (c oc))
        ((null oc)           colors:black)
        (t                   oc)))

(defun %chroma->omcolor (cc)
  (if (typep cc 'omcolor) cc (make-instance 'omcolor :c cc)))

(defparameter *om-black-color*       (om-make-color 0.0 0.0 0.0))
(defparameter *om-white-color*       (om-make-color 1.0 1.0 1.0))
(defparameter *om-light-gray-color*
  #+win32 (om-make-color 0.96 0.96 0.96)
  #-win32 (om-make-color 0.9 0.9 0.9))
(defparameter *om-gray1-color*       (om-make-color 0.85 0.85 0.85))
(defparameter *om-interface-blue-color* (om-make-color 0.0 0.47 0.83))
(defparameter *om-gray-color*        (om-make-color 0.6 0.6 0.6))
(defparameter *om-dark-gray-color*   (om-make-color 0.3 0.3 0.3))
(defparameter *om-dark-red-color*    (om-make-color 0.9 0.3 0.3))
;; vanilla uses :gray90 (win32) / :transparent (linux+cocoa); CAPI :transparent
;; falls through to the OS window's native grey, but GLFW has no backdrop behind
;; the framebuffer (transparent renders black), so render the grey explicitly.
(defparameter *om-window-def-color*
  #+win32 (om-make-color 0.9 0.9 0.9)
  #-win32 (om-make-color 0.9 0.9 0.9))
;;; Menu bar: its own colours, apart from the greys init-om-colors resets at boot
;;; (kernel/graphics/graphictools.lisp: init-om-colors); the tab strip's live in the
;;; library (alloy-desktop/view/tabs.lisp: *tab-face* and siblings).
(defparameter *om-menubar-bg-color*
  (om-make-color (/ 255 255.0) (/ 255 255.0) (/ 255 255.0))
  "Background of the menu bar.")
(defparameter *om-menubar-line-color*
  (om-make-color (/ 241 255.0) (/ 241 255.0) (/ 241 255.0))
  "Rule drawn under the menu bar.")

(defparameter *om-transparent-color* (om-make-color 0.0 0.0 0.0 0.0))

(defparameter *om-red-color*         (om-make-color 1.0 0.0 0.0))
(defparameter *om-red2-color*        (om-make-color 0.7 0.3 0.3))
(defparameter *om-blue-color*        (om-make-color 0.0 0.0 1.0))
(defparameter *om-green-color*       (om-make-color 0.0 1.0 0.0))
(defparameter *om-green2-color*      (om-make-color 0.37 0.73 0.62))
(defparameter *om-orange-color*      (om-make-color 1.0 0.5 0.25))
(defparameter *om-pink-color*        (om-make-color 1.0 0.0 1.0))
(defparameter *om-wine-color*        (om-make-color 0.50 0.0 0.0))
(defparameter *om-dark-blue-color*   (om-make-color 0.0 0.0 0.5))
(defparameter *om-yellow-color*      (om-make-color 1.0 1.0 0.0))
(defparameter *om-purple-color*      (om-make-color 0.75 0.61 0.98))
(defparameter *om-light-blue-color*  (om-make-color 0.0 1.0 1.0))
(defparameter *om-cafe-color*        (om-make-color 0.5 0.25 0.25))
(defparameter *om-salmon-color*      (om-make-color 0.99 0.62 0.45))
(defparameter *om-steel-blue-color*  (om-make-color 0.41 0.54 0.67))
(defparameter *om-beige-color*       (om-make-color 0.94 0.94 0.85))

(defparameter *om-select-color*       (om-make-color 0.5 0.5 0.5))
(defparameter *om-text-select-color*
  (om-make-color-alpha (/ (om-color-r *om-select-color*) 2)
                       (/ (om-color-g *om-select-color*) 2)
                       (/ (om-color-b *om-select-color*) 2)
                       0.7))
(defparameter *om-select-color-alpha*
  #+win32 (om-make-color-alpha 0.2 0.35 0.6 0.2)
  #-win32 (om-make-color-alpha 0.7 0.7 0.7 0.2))

;;; the push button look's colours (alloy-desktop/widgets/controls.lisp: *button-face* and siblings)
(defparameter *om-button-bg-color*           (%chroma->omcolor alld:*button-face*))
(defparameter *om-button-hover-color*        (%chroma->omcolor alld:*button-hover-face*))
(defparameter *om-button-active-color*       (%chroma->omcolor alld:*button-active-face*))

(defparameter *om-black-pattern*     *om-black-color*)
(defparameter *om-darkgray-pattern*  *om-dark-gray-color*)
(defparameter *om-gray-pattern*      *om-gray-color*)
(defparameter *om-lightgray-pattern* *om-light-gray-color*)
(defparameter *om-white-pattern*     *om-white-color*)

(defun om-output-color (color)
  (let ((r (om-color-r color))
        (g (om-color-g color))
        (b (om-color-b color)))
    `(om-make-color ,r ,g ,b)))

(defun om-color-to-capi (color)
  ;; vanilla rebuilds a 3-component rgb: alpha is dropped
  (if (typep color 'omcolor)
      (let ((cc (c color)))
        (chroma:rgb (chroma:r cc) (chroma:g cc) (chroma:b cc) 1.0))
      color))

(defun om-correct-color (color)
  (if (and color (typep color 'omcolor)) color *om-gray-color*))

;;; Drawing state -- bound by the OM-VIEW render pass (om-LW draw.lisp [10]); the rest of
;;; the pass state is imported from alloy-desktop/graphics/drawing.lisp.

(defvar *current-font*     nil)
(defvar *current-view* nil
  "The OM-VIEW whose om-draw-contents is running; om-with-focused-view redirects from it.")
(defvar *curstream* nil
  "The graphics port being drawn into (om-LW/graphics.lisp: *curstream*); read by om-get-current-port.")
(defvar *printer-port* nil
  "The printer device the OM drawing primitives must go to, or NIL for the screen.
Vanilla selects the printer port into *CURSTREAM* and the same primitives then
draw on paper (om-LW/print.lisp: om-print-document); here the primitives draw on
alld:*current-port*, which %draw-print-page binds to this port around one page;
kernel patches read this variable as the printing flag.")

;;; The ports the primitives dispatch on are the library's; this generic stays
;;; here because all its methods read OM types.
(defgeneric %port-call-with-focused-view (port view thunk))

;;; Fonts

(defun %ensure-list (x) (if (listp x) x (list x)))

;;; Font units.  The SIZE slot carries the size in the terms of the descriptor's
;;; own UNIT, which for every unit the fork creates is the REALIZED POINT SIZE --
;;; what vanilla stores in the :size of the gp:font-description it builds
;;; (om-LW/graphics.lisp: om-make-font).  UNIT records which contract produced the
;;; descriptor, so a point size chosen by the OS or by the user is never sent
;;; through the OM logical conversion a second time:
;;;
;;;   :om-logical    om-make-font         the OM size ladder (11 12 14 16 24)
;;;   :music         om-make-music-font   the score-font ladder
;;;   :device-px     om-make-font-px      the size is already in device pixels
;;;   :native-point  %om-make-font-pt     a point size taken verbatim
;;;   :native-role   %om-make-font-role   a point size supplied by the OS toolkit
;;;
;;; NIL in UNIT marks a descriptor persisted before the slot existed: its SIZE is
;;; the pre-unit OM number and its semantic is the one PX-MODE-P/MUSIC-P encode.
;;; Those two slots exist only so the standard #S reader can still build such a
;;; form -- it calls the structure's default keyword constructor, whose keywords
;;; are exactly the slot names (sbcl src/code/sharpm.lisp: sharp-S).  Nothing in
;;; the fork writes them.

(defun %om-logical->point (size)
  "OM logical size -> realized point (om-LW/graphics.lisp: om-make-font)."
  #+win32 (* size 2/3)
  #+linux (* size 3/4)
  #-(or win32 linux) size)

(defun %om-music->point (size)
  "OM music size -> realized point (om-LW/graphics.lisp: om-make-music-font)."
  #+(or win32 linux) (* size 3/4)
  #-(or win32 linux) size)

(defun %om-point->logical (size)
  "Realized point -> OM logical size (om-LW/graphics.lisp: om-font-size)."
  #+win32 (* size 3/2)
  #+linux (* size 4/3)
  #-(or win32 linux) size)

;;; SBCL equivalent of gp:font-description (capi-gp-ref-51): an externalizable,
;;; self-evaluating object -- kernel preferences store fonts and restore via
;;; (eval font) (default-prefs.lisp), which a plist would break.
(defstruct (om-font-description
            (:constructor %make-om-font-description)
            (:conc-name omfd-)
            (:predicate %om-font-description-p))
  family size (slant :roman) (weight :regular) (unit nil)
  (px-mode-p nil) (music-p nil))

(defmethod make-load-form ((self om-font-description) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(defun %omfd-unit (font)
  "FONT's semantic, reading a pre-unit descriptor through its old flags."
  (or (omfd-unit font)
      (cond ((omfd-px-mode-p font) :device-px)
            ((omfd-music-p font)   :music)
            (t                     :om-logical))))

(defun %omfd-point (font)
  "FONT's realized point size.  A pre-unit descriptor stored the OM number
instead of the point, so it is converted here."
  (let ((size (omfd-size font)))
    (if (omfd-unit font)
        size
        (case (%omfd-unit font)
          (:device-px size)
          (:music     (%om-music->point size))
          (t          (%om-logical->point size))))))

(defgeneric om-font-p (object)
  (:documentation "True if OBJECT is an OM font."))
(defmethod om-font-p ((object om-font-description)) t)
(defmethod om-font-p ((object t)) nil)

(defun %om-fd (family size unit style)
  "Build a descriptor of UNIT; SIZE is already in that unit's own terms."
  (%make-om-font-description
   :family family
   :size   size
   :slant  (if (member :italic (%ensure-list style)) :italic :roman)
   :weight (if (member :bold   (%ensure-list style)) :bold   :regular)
   :unit   unit))

(defun om-make-font (face size &key family style mode)
  (declare (ignore mode))
  (%om-fd (or family face) (%om-logical->point size) :om-logical style))

(defun om-make-music-font (face size)
  (%om-fd face (%om-music->point size) :music nil))

(defun %om-make-font-pt (family points &key style)
  "Descriptor for a point size taken verbatim, the way vanilla's editor and
listener defvars call gp::make-font-description directly (om-LW
lw-lisp-tools/texteditor.lisp: defvar *def-text-edit-font*)."
  (%om-fd family points :native-point style))

(defun om-font-face (font)   (omfd-family font))
(defun om-font-family (font) (omfd-family font))

(defun om-font-size (font)
  "FONT's OM logical size (om-LW/graphics.lisp: om-font-size).  Vanilla applies the
inverse platform factor to EVERY descriptor, music included, and the kernel
rebuilds fonts from this value (kernel save.lisp: om-save-font, kernel
boxframe.lisp: comment-style), so save/reload and the style toggle preserve the
realized point while the semantic collapses to :om-logical -- as in vanilla."
  (%om-point->logical (%omfd-point font)))

(defun om-make-font-px (face size &key family style mode)
  "Create a font whose :size is already in device pixels, bypassing pt->px."
  (declare (ignore mode))
  (%om-fd (or family face) size :device-px style))

(defun om-font-px-size (font)
  "FONT's draw size in pixels: the realized point through the typography DPI.
A :device-px descriptor already carries pixels.  No rounding -- a fractional
point size survives (om-LW/graphics.lisp: om-make-font keeps a ratio in :size)."
  (let ((point (%omfd-point font)))
    (if (eq (%omfd-unit font) :device-px)
        point
        (/ (* point (alld:typography-dpi)) 72))))

(defun om-font-style (font)
  (let ((bold-p   (eql (omfd-weight font) :bold))
        (italic-p (eql (omfd-slant font)  :italic)))
    (cond ((and bold-p italic-p) '(:bold :italic))
          (italic-p              '(:italic))
          (bold-p                '(:bold))
          (t                     '(:plain)))))

(defun om-font-mode (font) (declare (ignore font)) nil)

(defparameter *om-def-font-face*
  #+linux "Liberation Sans" #-linux "Verdana")

(defparameter *om-def-bold-font-face*
  #+linux "Liberation Sans" #-linux "Verdana")

(defparameter *om-def-font-sizes* '(11 12 14 16 24))

(defvar *om-default-font0* nil)

(defparameter *om-score-font-face* "Times New Roman")

(defparameter *om-controls-font*
  #+win32 (om-make-font *om-def-font-face* (nth 0 *om-def-font-sizes*))
  #+linux (om-make-font *om-def-font-face* (nth 1 *om-def-font-sizes*))
  #-(or linux win32) (om-make-font "LucidaGrande" 13))

(defparameter *om-default-font1* (om-make-font *om-def-font-face* (nth 0 *om-def-font-sizes*)))
(defparameter *om-default-font2* (om-make-font *om-def-font-face* (nth 1 *om-def-font-sizes*)))
(defparameter *om-default-font3* (om-make-font *om-def-font-face* (nth 2 *om-def-font-sizes*)))
(defparameter *om-default-font4* (om-make-font *om-def-font-face* (nth 3 *om-def-font-sizes*)))
(defparameter *om-default-font1b*
  (om-make-font *om-def-bold-font-face* (nth 0 *om-def-font-sizes*) :style '(:bold)))
(defparameter *om-default-font2b*
  (om-make-font *om-def-bold-font-face* (nth 1 *om-def-font-sizes*) :style '(:bold)))
(defparameter *om-default-font3b*
  (om-make-font *om-def-bold-font-face* (nth 2 *om-def-font-sizes*) :style '(:bold)))
(defparameter *om-default-font4b*
  (om-make-font *om-def-bold-font-face* (nth 3 *om-def-font-sizes*) :style '(:bold)))

;;; ---- Font atlas pre-baking: resolve fonts via pre-baked MSDF atlases so the
;;; render path never calls font-discovery (CoreText faults hard on macOS). ----

(defparameter *om-fonts-dir*
  (when *load-truename*
    ;; OPENMUSIC/resources/fonts/ -- three levels up from code/api/om-SBCL/.
    ;; *om-root* is unusable here: it is set by an init-func, not at load time.
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory (append (butlast (pathname-directory *load-truename*) 3)
                                      '("resources" "fonts"))))
  "OPENMUSIC/resources/fonts/, holding the bundled fonts in per-OS subdirs (win/mac/linux).")

(defparameter *om-music-families* '("omicron" "omheads" "omextras" "omsign")
  "Bundled OM music-symbol atlas names; distinct from the text fallback families.")

;;; The atlases OpenMusic bakes and the fallbacks it trusts, handed to the font
;;; registry (alloy-desktop/graphics/font.lisp: the defvar *font-atlases*); DejaVu
;;; Sans is bundled (always present) as the guaranteed text fallback.
;;; Both REPLACE the library defaults, which serve stand-alone use: this list is the
;;; set of faces OpenMusic is validated with on each platform.
(setf alld:*font-atlases*
  '(
    #-linux ("Verdana"         "Verdana"         :roman  :regular :system "verdana.ttf" "Verdana.ttf")
    #-linux ("VERDANAB"        "Verdana"         :roman  :bold    :system "verdanab.ttf" "Verdana Bold.ttf")
    #-linux ("VERDANAI"        "Verdana"         :italic :regular :system "verdanai.ttf" "Verdana Italic.ttf")
    #-linux ("VERDANAZ"        "Verdana"         :italic :bold    :system "verdanaz.ttf" "Verdana Bold Italic.ttf")
    #+linux ("Liberation Sans" "Liberation Sans" :roman :regular :system "LiberationSans-Regular.ttf")
    #+linux ("Liberation Sans Bold" "Liberation Sans" :roman :bold :system "LiberationSans-Bold.ttf")
    #-linux ("Arial"           "Arial"           :roman  :regular :system "arial.ttf" "Arial.ttf")
    #+linux ("Arial Liberation"      "Arial" :roman :regular :system "LiberationSans-Regular.ttf")
    #+linux ("Arial Liberation Bold" "Arial" :roman :bold    :system "LiberationSans-Bold.ttf")
    #-linux ("Consolas"        "Consolas"        :roman  :regular :system "consola.ttf" "Consolas.ttf" #+darwin "Consolas-Regular.ttf")
    #-linux ("Times New Roman" "Times New Roman" :roman  :regular :system "times.ttf" "Times New Roman.ttf")
    #-linux ("Courier New"     "Courier New"     :roman  :regular :system "cour.ttf" "Courier New.ttf")
    #+win32 ("Segoe UI"        "Segoe UI"        :roman  :regular :system "segoeui.ttf")
    ;; the face the "MS Shell Dlg" alias resolves to, i.e. what GDI realizes for
    ;; the stock DEFAULT_GUI_FONT every CAPI dialog item without a font gets
    #+win32 ("Microsoft Sans Serif" "Microsoft Sans Serif" :roman :regular :system "micross.ttf")
    ;; substitute file: the system font's own file is variable, and baking it
    ;; yields its default optical size, a display design
    #+darwin ("AppleSystemUI"  ".AppleSystemUIFont" :roman  :regular :system "HelveticaNeue.ttc")
    #+darwin ("Helvetica"      "Helvetica"          :roman  :regular :system "Helvetica.ttc")
    ("omicron"         "omicron"         :roman  :regular :bundled "omicron.ttf")
    ("omheads"         "omheads"         :roman  :regular :bundled "omheads.ttf")
    ("omextras"        "omextras"        :roman  :regular :bundled "omextras.ttf")
    ("omsign"          "omsign"          :roman  :regular :bundled "omsign.ttf")
    ("DejaVu Sans"     "DejaVu Sans"     :roman  :regular :bundled "DejaVuSans.ttf"))
  alld:*bundled-font-directory* *om-fonts-dir*
  alld:*fallback-families*
  '(#-linux "Verdana" #+linux "Liberation Sans" "Arial" #+win32 "Segoe UI" #-linux "Times New Roman" #-linux "Courier New" "DejaVu Sans")
  alld:*native-role-fallback* (list (omfd-family *om-controls-font*) (%omfd-point *om-controls-font*)
                                    (omfd-weight *om-controls-font*) (omfd-slant *om-controls-font*))
  alld:*baked-metrics-names* *om-music-families*)

(defun %om-music-font-p (font)
  "T if FONT is a bundled OM music-symbol atlas (matched by atlas-name)."
  (and (typep font 'org.shirakumo.alloy.renderers.opengl.msdf::font)
       (member (pathname-name (org.shirakumo.alloy.renderers.opengl.msdf:file font))
               *om-music-families* :test #'string-equal)
       t))

(defmethod org.shirakumo.alloy.renderers.opengl.msdf::compute-fallback-chain :around
    ((font org.shirakumo.alloy.renderers.opengl.msdf::font))
  "Music fonts fall back only to themselves: a missing music glyph renders notdef
   (blank) as CAPI does, never a text-font letter. Text fonts keep the full chain."
  (if (%om-music-font-p font)
      (vector font)
      (call-next-method)))

;;; ---- Native OS font roles as OM descriptors -------------------------------
;;; The roles are read from the OS and memoized per provider generation by the
;;; font registry (alloy-desktop/graphics/font.lisp: native-role); the descriptor
;;; the OM callers draw with is built once per role record.

(defun %om-native-font (role)
  "The font the OS toolkit dresses ROLE with, as a descriptor of unit
:native-role carrying the provider's exact point size.  One descriptor per role
per provider generation, so repeated calls answer the same object."
  (let ((r (alld:native-role role)))
    (or (alld:native-role-host-font r)
        (setf (alld:native-role-host-font r)
              (%make-om-font-description :family (alld:native-role-family r)
                                         :size (alld:native-role-points r)
                                         :slant (alld:native-role-slant r)
                                         :weight (alld:native-role-weight r)
                                         :unit :native-role)))))

(defun %om-native-fonts-invalidate ()
  "Drop every cached native role, so the next request re-reads the OS."
  (alld:invalidate-native-fonts))

(defun %om-make-font-role (role &key family points style)
  "Descriptor for the font the OS toolkit supplies for ROLE, the way CAPI dresses
its own chrome.  FAMILY, POINTS and STYLE override the provider for a caller that
already knows the pair; with none of them, the provider answers."
  (if (or family points style)
      (let ((native (%om-native-font role)))
        (%om-fd (or family (omfd-family native))
                (or points (%omfd-point native))
                :native-role style))
      (%om-native-font role)))

(defun %om-font-resolve (renderer om-font)
  "Resolve an OM font descriptor to an alloy font instance (never a string, never
via font-discovery); no descriptor means the dialog-default role.  On error the
registry logs and degrades to a cached fallback font
(alloy-desktop/graphics/font.lisp: resolve-font)."
  (let ((f (or om-font (%om-native-font :dialog-default))))
    (when f
      (alld:resolve-font renderer (om-font-family f) (omfd-slant f) (omfd-weight f)))))

(defun om-correct-font (font)
  (if (om-font-p font) font *om-default-font1*))

(defparameter *om-string-avg-char-ratio* 0.6f0
  "Heuristic ratio of character advance width to font size.")

(defparameter *om-string-line-height-ratio* 1.25f0
  "Heuristic ratio of line-height to font size.")

(defun %metric-renderer ()
  "Renderer to measure text under: the draw-pass renderer, else any open window
(windows are the presentation renderers), so metrics match rendering even when
the kernel measures outside a draw pass (e.g. get-name-size at icon creation)."
  (or *current-renderer*
      (first (om-get-all-windows))))

(defun %om-font-size-unit (font)
  "Alloy unit carrying FONT's draw size.  Only a :device-px descriptor resolves in
raw device pixels; every point-based semantic, MUSIC INCLUDED, resolves in the
DPI-scaled unit.

Vanilla realizes the music point at the display DPI while the kernel's score
geometry stays in device px, and those two spaces differ there as well: a
miniview asks for 20, om-make-music-font realizes 15 pt (om-LW/graphics.lisp:
om-make-music-font), the toolkit draws that at the screen's own DPI, and the
staff step remains size/4 raw pixels (projects/musicproject/editor/
musminiboxes.lisp: draw-mini-obj)."
  (let ((f (or font *om-default-font2*)))
    (if (eq (%omfd-unit f) :device-px)
        (alloy:px (om-font-px-size f))
        (alloy:un (om-font-px-size f)))))

(defun %font-draw-px (font parent)
  "Draw size of FONT in WHOLE device px, resolved under PARENT's window unit
context -- the same context label shapes resolve in (base-scale aware, alloy
units.lisp: the define-unit un).

The rounding is not a tidy-up: it is how the toolkit this port matches realizes a
font. CAPI hands GDI a logical font whose lfHeight is -MulDiv(points, dpi, 72),
an INTEGER (om-LW/graphics.lisp: om-make-font builds the descriptor CAPI then
realizes), so at 120 dpi the Verdana 22/3 of *om-default-font1* is realized at 12
px and never at 12.2222. Measured: at that em the face own advances answer
65.17 / 44.78 / 57.49 / 78.12 px for OpenMusic, Classes, Functions and
Output Files: -- the four live vanilla widths, to 0.01 px. Leaving the fraction
in place made every string about 1.85 percent too wide and moved the line breaks
of every label that sits near its box width.
One px is the floor: a zero draw size would divide by nothing downstream."
  (max 1 (round (if parent
                    (alloy:with-unit-parent parent
                      (alloy:to-px (%om-font-size-unit font)))
                    (alloy:to-px (%om-font-size-unit font))))))

(defun %om-font-draw-unit (renderer font)
  "The unit a text shape carries for FONT under RENDERER: the WHOLE device pixel of
%font-draw-px, so what is drawn is exactly what om-string-size measured. A shape
given the raw point-based unit re-resolves it per render and lands back on the
fractional size. Argument order mirrors %om-font-resolve, the sibling every
realization calls next to it."
  (alloy:px (%font-draw-px font renderer)))

(defun %om-font-entry (renderer font)
  "Memoized resolution of FONT under RENDERER, NIL when either is missing
(alloy-desktop/graphics/font.lisp: cached-font-entry).  The descriptors are
immutable and compared by content, so the score editor's per-redraw rebuild of its
music descriptors (kernel graphictools.lisp: init-fonts-to-draw) still hits."
  (flet ((make-entry ()
           (alld:make-font-entry
            :font (alld:request-font-variant renderer (omfd-family font)
                                             (omfd-slant font) (omfd-weight font))
            :size-unit (%om-font-draw-unit renderer font))))
    ;; the builder runs at most once per key and is never kept
    (declare (dynamic-extent #'make-entry))
    (alld:cached-font-entry renderer font #'make-entry)))

(defun %om-measured-font (spec)
  "The realized MSDF font of SPEC under the metric renderer and its whole-pixel
draw size, or NIL."
  (let* ((rend  (%metric-renderer))
         (mfont (and rend
                     (let ((*current-renderer* rend) (*current-font* spec))
                       (%resolve-font)))))
    (when (typep mfont 'org.shirakumo.alloy.renderers.opengl.msdf::font)
      ;; line-advance reads the atlas data only for a face with no native line box
      (org.shirakumo.alloy.renderers.opengl.msdf::partial-allocate mfont)
      (values mfont (%font-draw-px spec rend)))))

(defun om-string-h (&optional (font *om-default-font2*))
  "Pixel line-height of FONT, the face's own line box at the realized draw size
(gp:get-string-extent equivalent); ratio heuristic only when no renderer/atlas
resolves."
  (let ((spec (or font *om-default-font2*)))
    (multiple-value-bind (mfont px) (%om-measured-font spec)
      (if mfont
          (round (alld:line-advance mfont px))
          (round (* *om-string-line-height-ratio*
                    (if (omfd-size spec) (om-font-px-size spec) 12)))))))

(defun om-make-font-object (font)
  "gp::find-best-font equivalent: kernel passes the result opaquely to
om-with-font/draw/measure; the MSDF atlas resolves per draw, so the concrete
font object here is the (size-carrying) description itself."
  (om-correct-font font))

(defun om-string-size (str &optional font)
  "Pixel advance width of STR in FONT (or *current-font*), the gp:get-string-extent
equivalent.
A newline starts a new line rather than advancing: vanilla measures the BOX of a
multi-line string (om-LW/graphics.lisp: om-string-size, over gp:get-string-extent),
so the answer is the widest line, not the sum across the break.
The answer is FRACTIONAL, and it is the LAST element of the corrected whole-pixel
walk (alloy-desktop/graphics/text-measure.lisp: apply-extent-residual): the face's
own advance sum whenever that walk overshoots it, and the walk itself when it falls
short.  Both branches follow the toolkit -- \"Output Files:\" is 78.117 px in a box
of 80, where the walk totals 80 and rounding filled the box and moved the break,
while \"iiii\" is 12.0, the walk, where the face's own sum is 13.17.
Falls back to an average-ratio estimate only when no atlas resolves (no renderer)."
  (let ((spec (or font *current-font* *om-default-font2*)))
    (multiple-value-bind (mfont px) (%om-measured-font spec)
      (if mfont
          (alld:string-advance mfont px str)
          (float (* *om-string-avg-char-ratio* (om-font-px-size spec) (length (or str ""))) 0f0)))))

(defun %om-font-baseline-px (&optional (font *om-default-font1*))
  "Pixels from the top of a rendered line box down to its baseline, for FONT --
what separates a shape anchored at its own bounds from a baseline the OM places
itself.
The distance is the atlas ASCENDER, and 3b-bmfont's base is the ascender MINUS
the padding the bake adds below it (sdf bmfont.lisp: to-bmfont), so the padding
has to be added back: measured against the live vanilla, reading base alone put
the first baseline 2 px too low on every label. 0 when no atlas resolves."
  (multiple-value-bind (mfont px) (%om-measured-font (or font *om-default-font1*))
    (if mfont
        (alld:ascender-px mfont px)
        0f0)))

;;; with-* macros

(defun %view-window-xy (view)
  "Window-relative OM position of VIEW: sum of container-relative positions."
  (let ((x 0) (y 0))
    (loop for v = view then parent
          for parent = (om-view-container v)
          while parent
          do (let ((p (om-view-position v)))
               (incf x (om-point-x p))
               (incf y (om-point-y p))))
    (values x y)))

(defun %focused-view-renderer (view)
  (or (and (typep *current-renderer* 'alloy:renderer)
           *current-renderer*)
      (and (typep view 'alloy:renderer)
           view)
      (and (typep view 'alloy:layout-element)
           (let ((window (om-view-window view)))
             (and (typep window 'alloy:renderer)
                  window)))))

(defun %renderer-live-p (renderer)
  "NIL when RENDERER is a GLFW window whose native handle is already gone."
  (or (not (typep renderer '%glfw:window))
      (and (%glfw:pointer renderer) t)))

(defun %mark-focused-view-dirty (view renderer)
  "Request the frame that puts a draw made OUTSIDE the render pass on screen.
In CAPI such a draw reaches the port immediately; here it lands in the back
buffer, and the production loop swaps only a window it was asked to render
(window.lisp: %frame-windows-guarded).  The kernel relies on this: on Windows
draw-only-select (kernel miniviews.lisp) paints the box selection this way and
never invalidates the view."
  (let ((target (if (typep view 'alloy:renderable) view renderer)))
    (when (typep target 'alloy:renderable)
      (alloy:mark-for-render target))))

(defun %call-with-focused-view (view thunk)
  "Vanilla *curstream*/*pox*/*poy* redirection: draw THUNK in VIEW's coord space."
  (%port-call-with-focused-view alld:*current-port* view thunk))

(defmethod %port-call-with-focused-view ((port alld:screen-port) view thunk)
  (let ((renderer (%focused-view-renderer view))
        ;; vanilla binds *curstream* to (om-get-view view) here (om-LW/graphics.lisp: om-with-focused-view);
        ;; a NIL view keeps the enclosing port current instead of blanking it
        (*curstream* (or view *curstream*)))
    (cond ((null renderer) nil)
          ;; a destroyed window owns no GL context: emitting GL there kills the process
          ((not (%renderer-live-p renderer)) nil)
          (*current-renderer*
           (if (or (null view) (null *current-view*) (eq view *current-view*))
               (funcall thunk)
               (multiple-value-bind (tx ty) (%view-window-xy view)
                 (multiple-value-bind (cx cy) (%view-window-xy *current-view*)
                   (let ((*pox* (+ *pox* (- tx cx)))
                         (*poy* (+ *poy* (- ty cy))))
                     (funcall thunk))))))
          (t
           (alld:call-with-gl-context
            renderer
            (lambda ()
              ;; same pen state the render pass binds (draw.lisp: %render-om-draw-contents): outside a
              ;; frame these globals are NIL and every shape would carry pattern NIL
              (let ((*current-renderer* renderer)
                    (*current-view* view)
                    (*current-view-height* (om-height view))
                    (*current-fg-color* (or *current-fg-color* colors:black))
                    (*current-bg-color* (or *current-bg-color*
                                            (%omcolor->chroma (om-get-bg-color view))))
                    (*current-font* (or (om-get-font view) *current-font*))
                    (*pox* 0)
                    (*poy* 0))
                (multiple-value-prog1 (funcall thunk)
                  (%mark-focused-view-dirty view renderer)))))))))

(defmacro om-with-focused-view (view &body body)
  `(%call-with-focused-view ,view (lambda () ,@body)))

(defmacro om-with-fg-color (view color &body body)
  (declare (ignore view))
  `(let ((*current-fg-color* (%omcolor->chroma ,color))) ,@body))

(defmacro om-with-bg-color (view color &body body)
  (declare (ignore view))
  `(let ((*current-bg-color* (%omcolor->chroma ,color))) ,@body))

(defmacro om-with-font (font &body body)
  `(let ((*current-font* ,font)) ,@body))

;;; NOT USED ANYMORE
(defmacro om-with-pen ((view &key mode pattern (line :black-line) (size 1)) &body body)
  (declare (ignore view mode pattern line size))
  `(let () ,@body))

(defmacro om-with-clip-rect (view rect &body body)
  "Clip BODY's drawing to RECT, an om-rect in OM (y-down) view coordinates."
  (declare (ignore view))
  (let ((r (gensym "RECT")) (thunk (gensym "BODY")))
    `(let ((,r ,rect))
       (flet ((,thunk () ,@body))
         (declare (dynamic-extent #',thunk))
         (alld:call-with-clip (om-rect-left ,r) (om-rect-top ,r) (om-rect-w ,r) (om-rect-h ,r)
                              #',thunk)))))

;; vanilla's body is inert (the :mask args fall outside the call): kept as no-op
(defun om-set-clip-rect (view rect)
  (declare (ignore view rect))
  nil)

(defmacro om-with-line-size (size &body body)
  `(let ((*current-pen-size* #+win32 (max 1 (round ,size)) #-win32 ,size))
     ,@body))

(defmacro om-with-dashline (&optional (pattern '(2 2)) &body body)
  ;; PATTERN is a dash spec (quoted list of lengths) OR, when callers omit it,
  ;; the first body form -- run every non-spec form as body (never drop it).
  (multiple-value-bind (dash forms)
      (if (and (consp pattern) (eq (car pattern) 'quote)
               (listp (cadr pattern)) (cadr pattern) (every #'realp (cadr pattern)))
          (values pattern body)
          (values ''(2 2) (cons pattern body)))
    `(let ((*current-line-style* :dashed) (*current-dash-pattern* ,dash))
       ,@forms)))

(defmacro om-with-line (style &body body)
  (let ((s (gensym "STYLE")))
    `(let ((,s ,style))
       (cond ((consp ,s)
              (let ((*current-line-style* :dashed)
                    (*current-dash-pattern* (list (car ,s) (cadr ,s))))
                ,@body))
             ((and ,s (symbolp ,s) (string-equal (symbol-name ,s) "DASH"))
              (let ((*current-line-style* :dashed) (*current-dash-pattern* '(2 2)))
                ,@body))
             (t (progn ,@body))))))

(defmacro om-with-hilite-foreground (&body body)
  `(let ((*current-fg-color* (c *om-select-color-alpha*)))
     ,@body))

(defun %resolve-font ()
  (when *current-font*
    (let ((entry (%om-font-entry *current-renderer* *current-font*)))
      (if entry
          (alld:font-entry-font entry)
          (alld:request-font-variant *current-renderer* (omfd-family *current-font*)
                                     (omfd-slant *current-font*)
                                     (omfd-weight *current-font*))))))

(defun %resolve-font-size ()
  "Alloy unit for the current font's draw size (see %om-font-size-unit)."
  (let ((entry (%om-font-entry *current-renderer* *current-font*)))
    (if entry
        (alld:font-entry-size-unit entry)
        (%om-font-size-unit *current-font*))))

(defun %erase-color ()
  (let ((bg (or *current-bg-color* (%omcolor->chroma *om-window-def-color*))))
    (if (typep bg 'omcolor) (c bg) bg)))

;;; Drawing primitives -- the OM types (om-rect, ompoint, font descriptor) resolve here;
;;; the drawing itself is alloy-desktop/graphics/drawing.lisp.

(defun om-get-current-port () *curstream*)

(defun %om-rect-args (x y w h)
  "X Y W H, or the om-rect X when Y is NIL, as a normalized port rectangle."
  (if y
      (alld:normalize-rectangle x y w h)
      (values (rx x) (ry x) (rw x) (rh x))))

(defun %om-polygon-points (points)
  "POINTS as (x y) lists where they are om-points or a flat x y list (om-LW/graphics.lisp: om-draw-polygon), else unchanged."
  (cond ((and points (numberp (first points)))
         (loop for (px py) on points by #'cddr collect (list px py)))
        ((some #'om-point-p points)
         (loop for item in points
               collect (if (om-point-p item) (list (om-point-h item) (om-point-v item)) item)))
        (t points)))

(defmethod om-draw-string (x y str &key (end nil))
  (let* ((entry (%om-font-entry *current-renderer* *current-font*))
         ;; a printer page is drawn with no renderer, and its port needs no MSDF font
         (font  (cond (entry (alld:font-entry-font entry))
                      (*current-renderer* (%resolve-font))))
         (size  (if entry
                    (alld:font-entry-size-unit entry)
                    (%om-font-size-unit *current-font*))))
    (alld:port-draw-text alld:*current-port* x y (if end (subseq str 0 end) str) font size entry)))

(defun om-draw-char (x y cn)
  (om-draw-string x y (string cn)))

(defun om-draw-line (x1 y1 x2 y2 &key erasable)
  ;; :erasable = CAPI XOR raster op; mapped to Alloy per-frame redraw (no-op)
  (declare (ignore erasable))
  (alld:port-draw-line alld:*current-port* x1 y1 x2 y2))

(defun om-erase-line (x1 y1 x2 y2)
  (let ((*current-fg-color* (%erase-color)))
    (om-draw-line x1 y1 x2 y2)))

(defun om-draw-point (x y)
  (alld:draw-point x y))

(defun om-draw-rect (x &optional y (w 0) (h 0) &key erasable (pensize 1))
  ;; vanilla :erasable = XOR raster op; unneeded: Alloy redraws every frame
  (declare (ignore erasable))
  (multiple-value-bind (left top wi he) (%om-rect-args x y w h)
    (alld:port-draw-rect alld:*current-port* left top wi he pensize)))

(defun om-draw-rect-outline (x y w h &optional (pensize 1))
  (om-draw-rect x y (- w 1) (- h 1) :pensize pensize))

(defun om-fill-rect (x &optional y (w 0) (h 0) &key erasable)
  (declare (ignore erasable))
  (multiple-value-bind (left top wi he) (%om-rect-args x y w h)
    (alld:port-fill-rect alld:*current-port* left top wi he)))

(defun om-erase-rect-content (x &optional y (w 0) (h 0))
  (multiple-value-bind (left top wi he) (%om-rect-args x y w h)
    (alld:fill-rectangle left top wi he (%erase-color))))

(defun om-erase-rect (x &optional y (w 0) (h 0))
  (om-erase-rect-content x y w h))

(defun om-draw-round-rect (x y w h &key (radius 4) (pensize 1))
  (alld:draw-round-rect x y w h radius pensize))

(defun om-fill-round-rect (x y w h &key (radius 4))
  (alld:fill-round-rect x y w h radius))

(defun om-draw-ellipse (x y rx ry)
  (alld:port-draw-ellipse alld:*current-port* x y rx ry nil))

(defun om-fill-ellipse (x y rx ry)
  (alld:port-draw-ellipse alld:*current-port* x y rx ry t))

(defun om-draw-ellipse-arc (x y width height start-angle sweep-angle)
  (alld:port-draw-ellipse-arc alld:*current-port* x y width height start-angle sweep-angle))

(defun om-draw-polygon (points)
  (alld:port-draw-polygon alld:*current-port* (%om-polygon-points points)))

(defun om-fill-polygon (points)
  (alld:fill-polygon (%om-polygon-points points)))

(defun %gp-pattern-paint (pattern)
  "Return Alloy paint and the texture whose wrapping must be temporary repeat."
  (cond
    ((null pattern) (values *current-fg-color* nil))
    ((alld:picture-p pattern)
     (alld:image-paint (alld:ensure-texture pattern)
                       (alld:picture-width pattern) (alld:picture-height pattern)))
    ((typep pattern 'simple:image-pattern)
     (values pattern (simple:image pattern)))
    (t (error "GP:DRAW-POLYGON :PATTERN must be an OM image, not ~S."
              pattern))))

(defun gp::draw-polygon (port points &rest args
                         &key filled closed fill-rule foreground thickness
                              (dashed nil dashed-p) (dash nil dash-p)
                              transform line-end-style line-joint-style
                              compositing-mode mask background operation
                              (scale-thickness t) pattern
                              (shape-mode nil shape-mode-p)
                         &allow-other-keys)
  "capi-gp-ref-36.htm:60 — CLOSED adds the edge from the last vertex to the first;
FILLED draws a filled closed polygon and then CLOSED is ignored.
Honoured graphics-state overrides (capi-gp-ref-36.htm:62): foreground, thickness,
dashed, dash, line-end-style, line-joint-style, transform, compositing-mode, mask,
scale-thickness, pattern and shape-mode. BACKGROUND and OPERATION have no drawing
effect on shapes in the Windows quality backend, as documented by GRAPHICS-STATE."
  (declare (ignore args background))
  (unless (member (or fill-rule :even-odd) '(:even-odd :winding))
    (error "GP:DRAW-POLYGON :FILL-RULE must be :EVEN-ODD or :WINDING, not ~S."
           fill-rule))
  (when (and shape-mode-p
             (not (member shape-mode
                          '(:plain :antialias :fastest :best :default))))
    (error "Invalid GP shape-mode ~S." shape-mode))
  ;; This bridge exposes a quality port. LispWorks ignores the compatible-mode
  ;; BOOLE operation there, but the documented domain must still be enforced.
  (when (and operation
             (not (and (integerp operation) (<= 0 operation 15))))
    (error "GP operation must be an integer from 0 through 15, not ~S."
           operation))
  (when (and thickness (not (and (realp thickness) (not (minusp thickness)))))
    (error "GP thickness must be a non-negative real, not ~S." thickness))
  (when (and transform
             (not (and (consp transform) (= 6 (length transform))
                       (every #'realp transform))))
    (error "GP transform must contain six real values, not ~S." transform))
  (let ((vertices (alld:polygon-vertices points)))
    (when (cdr vertices)
      (om-with-focused-view port
        (let* ((effective-transform (or transform alld:*port-transform*))
               (dashing (if dashed-p dashed (and *current-dash-pattern* t)))
               ;; capi-gp-ref-78.htm:109 — DASH defaults to (4 4)
               (dash-pattern (cond ((not dashing) nil)
                                   (dash-p dash)
                                   (*current-dash-pattern* *current-dash-pattern*)
                                   (t '(4 4))))
               ;; A quality transform scales the complete solid stroke geometry.
               ;; Dash lengths are device pixels, so dashed paths remain tessellated
               ;; after coordinate transformation and use a scalar pen fallback.
               (renderer-transform-p
                 (and effective-transform scale-thickness (not dashing)))
               (pts (if renderer-transform-p
                        vertices
                        (let ((alld:*port-transform* effective-transform))
                          (loop for (x y) in vertices
                                collect (multiple-value-list
                                         (alld:transform-xy x y))))))
               (base-thickness (or thickness *current-pen-size*))
               (effective-thickness
                 (if (and dashing effective-transform scale-thickness)
                     (* base-thickness (alld:transform-scale effective-transform))
                     base-thickness)))
          (let ((*current-fg-color* (if foreground
                                        (%omcolor->chroma foreground)
                                        *current-fg-color*))
                (*current-pen-size* effective-thickness)
                (*current-line-style* (if dashing :dashed :solid))
                (*current-dash-pattern* dash-pattern)
                (alld:*port-transform* effective-transform))
            (multiple-value-bind (paint texture) (%gp-pattern-paint pattern)
              (labels
                  ((draw-shape ()
                     (alld:call-with-pattern-clip-scope
                      pattern
                      (lambda ()
                        (cond
                          (filled
                           (alld:render-filled-polygon
                            pts (or fill-rule :even-odd) paint))
                          ;; Dash tessellation is per segment, so each visible mark is
                          ;; an independent solid stroke and keeps only its end cap.
                          (dashing
                           (loop for (a b) on (if closed
                                                  (append pts (list (first pts)))
                                                  pts)
                                 while b
                                 do (if pattern
                                        (dolist (segment
                                                  (alld:dash-line-segments
                                                   (first a) (second a)
                                                   (first b) (second b)
                                                   dash-pattern))
                                          (destructuring-bind (sx sy ex ey) segment
                                            (alld:stroke-polyline
                                             (list (list sx sy) (list ex ey)) nil
                                             line-end-style line-joint-style paint)))
                                        (om-draw-line (first a) (second a)
                                                      (first b) (second b)))))
                          (t
                           (alld:stroke-polyline pts closed line-end-style
                                                 line-joint-style paint))))))
                   (draw-transformed ()
                     (if renderer-transform-p
                         (alld:call-with-transform #'draw-shape)
                         (draw-shape)))
                   (draw-masked ()
                     (alld:call-with-mask mask #'draw-transformed))
                   (draw-composited ()
                     (alld:call-with-composite-mode
                      compositing-mode #'draw-masked)))
                (alld:call-with-shape-mode
                 (and shape-mode-p shape-mode)
                 (lambda ()
                   (alld:call-with-pattern-repeat texture
                                                  #'draw-composited)))))))))))

(defun om-draw-selection-rect (x y w h &key (mode :xor))
  (declare (ignore mode))
  (let ((left   (if (minusp w) (+ x w) x))
        (width  (abs w))
        (top    (if (minusp h) (+ y h) y))
        (height (abs h)))
    (om-draw-rect left top width height)))

(defun om-draw-hilite-rect (x y w h &optional color)
  (let* ((cc (if color
                 (let ((rgb (c color)))
                   (chroma:rgb (chroma:r rgb) (chroma:g rgb) (chroma:b rgb) 0.4))
                 (c *om-select-color-alpha*)))
         (w* (max 0 (- w 1)))
         (h* (max 0 (- h 1))))
    (alld:fill-rectangle x y w* h* cc)))

(defun om-draw-hilite-icon (x y w h &optional color)
  (let* ((cc (if color
                 (let ((rgb (c color)))
                   (chroma:rgb (chroma:r rgb) (chroma:g rgb) (chroma:b rgb) 1.0))
                 (c *om-select-color-alpha*)))
         (w* (max 0 (- w 1)))
         (h* (max 0 (- h 1))))
    (alld:fill-rectangle x y w* h* cc)))

(declaim (ftype (function (t) single-float) %as-single-float))
(defun %as-single-float (x)
  (float x 0f0))
