(in-package :alloy-desktop)

;;; OS-style scrollbar: a thin handle pinned to the outer edge that thickens under the
;;; pointer and shows its two arrows, arrow zones that step and auto-repeat while held,
;;; and a track that never takes the input focus.

(defparameter *scrollbar-track* (chroma:rgb 0.88 0.88 0.88)
  "Shared background of scrollbar gutters and of their bottom-right intersection.")
(defparameter *scrollbar-grip* (chroma:rgb 0.749 0.749 0.749)
  "Dimples of the corner size grip: (191 191 191), measured off the Win32 filler.")
(defparameter *scrollbar-handle* (chroma:rgb 0.5 0.5 0.5)
  "Scrollbar handle and arrows.")

(defparameter *scrollbar-handle-inset* 3.0)
(defparameter *scrollbar-handle-min-length* 30.0)
(defparameter *scrollbar-collapsed-thickness* 6.0)
(defparameter *scrollbar-expanded-thickness* 12.0)
(defparameter *scrollbar-anim-speed* 8.0)
(defparameter *scrollbar-arrow-zone* 14.0)

(defmethod initialize-instance :around ((sv alloy:scroll-view) &key)
  (call-next-method)
  (let ((layout (alloy:layout-element sv)))
    (when (typep layout 'alloy:border-layout)
      (alloy:do-elements (el layout)
        (when (typep el 'alloy:scrollbar)
          (alloy:update el layout
                        :place (alloy:element-index el layout)
                        :size (alloy:px *scrollbar-track-px*)))))))

(defparameter *scrollbar-repeat-delay* 0.4)
(defparameter *scrollbar-repeat-interval* 0.06)

;;; State of alloy's own bars lives here: a scroll-view creates its bars itself, so
;;; there is no subclass to hold it.
(defvar *scrollbar-anim-progress* (make-hash-table :test 'eq :weakness :key))
(defvar *scrollbar-repeat* (make-hash-table :test 'eq :weakness :key))

(defun scrollbar-progress (sb)
  "Hover progress of SB in [0,1]."
  (gethash sb *scrollbar-anim-progress* 0.0))

(defun ease-out (p)
  "Quadratic ease-out of P in [0,1]."
  (- 1.0 (expt (- 1.0 p) 2)))

(defun scrollbar-step-repeat (sb dt)
  (let ((rep (gethash sb *scrollbar-repeat*)))
    (when (and rep (alloy:layout-tree sb))
      (let ((window (alloy:ui (alloy:layout-tree sb))))
        (if (eql :press (%glfw:mouse-button-state :left window))
            (destructuring-bind (dir timer) rep
              (let ((nt (- timer dt)))
                (cond ((<= nt 0.0)
                       (incf (alloy:value sb) (* dir (alloy:step sb)))
                       (setf (gethash sb *scrollbar-repeat*)
                             (list dir *scrollbar-repeat-interval*)))
                      (t
                       (setf (gethash sb *scrollbar-repeat*) (list dir nt))))))
            (remhash sb *scrollbar-repeat*))))))

(defmethod org.shirakumo.alloy.animation:update :after ((sb alloy:scrollbar) dt)
  ;; a drag outside the bar has lost the hover focus (input/pointer.lisp: clear-stale-weak-focus)
  (let ((target (if (or (alloy:focus sb) (alloy:active-p sb)) 1.0 0.0))
        (p (gethash sb *scrollbar-anim-progress* 0.0)))
    (unless (= p target)
      (setf (gethash sb *scrollbar-anim-progress*)
            (if (< p target)
                (min target (+ p (* dt *scrollbar-anim-speed*)))
                (max target (- p (* dt *scrollbar-anim-speed*)))))
      (alloy:mark-for-render sb)))
  (scrollbar-step-repeat sb dt))

(defun scrollbar-track-px (sb)
  "(values track-length track-thickness) in px along SB's orientation."
  (ecase (alloy:orientation sb)
    (:horizontal (values (alloy:pxw sb) (alloy:pxh sb)))
    (:vertical   (values (alloy:pxh sb) (alloy:pxw sb)))))

(defun scrollbar-thickness (sb)
  "Handle thickness of SB at its current hover progress."
  (multiple-value-bind (track thick) (scrollbar-track-px sb)
    (declare (ignore track))
    (min thick (+ *scrollbar-collapsed-thickness*
                  (* (ease-out (scrollbar-progress sb))
                     (- *scrollbar-expanded-thickness* *scrollbar-collapsed-thickness*))))))

(defun scrollbar-handle-cross (sb)
  "(values cross-offset thickness) of SB's handle, pinned to the outer window edge."
  (multiple-value-bind (track thick) (scrollbar-track-px sb)
    (declare (ignore track))
    (let ((ct (scrollbar-thickness sb)))
      (ecase (alloy:orientation sb)
        (:vertical   (values (max 0.0 (- thick *scrollbar-handle-inset* ct)) ct))
        (:horizontal (values *scrollbar-handle-inset* ct))))))

(defun scrollbar-handle-length (sb)
  "Length of SB's handle along the track, between the two arrow zones."
  (multiple-value-bind (track) (scrollbar-track-px sb)
    (let ((avail (max 1.0 (- track (* 2 *scrollbar-arrow-zone*)))))
      (min avail (max (min *scrollbar-handle-min-length* avail)
                      (* avail (alloy:scroll-handle-size sb)))))))

(defun scrollbar-thumb-start (sb)
  "Distance in px from the start of SB's track (its left or bottom edge) to its handle, which
travels between the two arrow zones."
  (let ((az *scrollbar-arrow-zone*))
    (+ az (* (- (scrollbar-track-px sb) (* 2 az) (scrollbar-handle-length sb))
             (alloy:slider-unit sb)))))

(defun scrollbar-along (sb location)
  "Distance in px of the global LOCATION from the start of SB's track (its left or bottom edge)."
  (alloy::with-global-bounds (gb sb)
    (ecase (alloy:orientation sb)
      (:vertical   (- (alloy:pxy location) (alloy:pxy gb)))
      (:horizontal (- (alloy:pxx location) (alloy:pxx gb))))))

(defun scrollbar-arrow-points (sb which)
  "Triangle for the :min (bottom/left) or :max (top/right) arrow of SB."
  (multiple-value-bind (track thick) (scrollbar-track-px sb)
    (let ((az   *scrollbar-arrow-zone*)
          (half (/ *scrollbar-expanded-thickness* 2))
          (c    (- thick *scrollbar-handle-inset*
                   (/ *scrollbar-expanded-thickness* 2))))
      (ecase (alloy:orientation sb)
        (:vertical
         (ecase which
           (:max (list (alloy:px-point (- c half) (+ (- track az) 4.0))
                       (alloy:px-point (+ c half) (+ (- track az) 4.0))
                       (alloy:px-point c (- track 4.0))))
           (:min (list (alloy:px-point (- c half) (- az 4.0))
                       (alloy:px-point (+ c half) (- az 4.0))
                       (alloy:px-point c 4.0)))))
        (:horizontal
         (let ((cy (+ *scrollbar-handle-inset* (/ *scrollbar-expanded-thickness* 2))))
           (ecase which
             (:max (list (alloy:px-point (+ (- track az) 4.0) (- cy half))
                         (alloy:px-point (+ (- track az) 4.0) (+ cy half))
                         (alloy:px-point (- track 4.0) cy)))
             (:min (list (alloy:px-point (- az 4.0) (- cy half))
                         (alloy:px-point (- az 4.0) (+ cy half))
                         (alloy:px-point 4.0 cy))))))))))

(defmethod alloy:handle :around ((event alloy:pointer-down) (sb alloy:scrollbar))
  (alloy::with-global-bounds (gb sb)
    (let ((loc (alloy:location event)))
      (if (alloy:contained-p loc gb)
          (multiple-value-bind (track) (scrollbar-track-px sb)
            (let ((along (scrollbar-along sb loc))
                  (az *scrollbar-arrow-zone*))
              (cond ((< along az)
                     (decf (alloy:value sb) (alloy:step sb))
                     (setf (gethash sb *scrollbar-repeat*) (list -1 *scrollbar-repeat-delay*))
                     t)
                    ((> along (- track az))
                     (incf (alloy:value sb) (alloy:step sb))
                     (setf (gethash sb *scrollbar-repeat*) (list 1 *scrollbar-repeat-delay*))
                     t)
                    ;; scroller bars are native decorations of the pane and never take the
                    ;; input focus; the drag rides the pointer capture
                    ((let ((chain (%focus-chain sb)))
                       (and chain (content-chain-p chain)))
                     (capture-pointer sb)
                     (setf (alloy::state sb) :dragging)
                     t)
                    (t (call-next-method)))))
          (call-next-method)))))

;;; The thumb of a pane's bar behaves as the native one. alloy's slider maps the pointer over
;;; the whole bar (alloy components/slider.lisp: the handle for (pointer-move slider)), while
;;; this look draws the handle between the arrow zones, so the two only meet at one point.
;;; Here a press on the handle keeps the pressed point of it under the pointer for the whole
;;; drag, and a press in the trough beside it pages one visible area toward the pointer.

;;; A press reaches the focused bar before the layout (alloy ui.lisp: handle for layout-event); only a press inside the bar is its own,
;;; the others keep alloy's path (alloy component.lisp: handle for pointer-down component).
(defmethod alloy:handle ((event alloy:pointer-down) (sb scrollbar))
  (if (alloy:contained-p (alloy:location event) sb)
      (let ((along (scrollbar-along sb (alloy:location event)))
            (start (scrollbar-thumb-start sb)))
        (if (<= start along (+ start (scrollbar-handle-length sb)))
            ;; the drag rides the pointer capture: alloy's press activates the slider
            ;; (alloy components/slider.lisp: the handle for (pointer-down slider)), a focus a native bar never takes
            (progn
              (setf (scrollbar-grab sb) (- along start))
              (capture-pointer sb)
              (setf (alloy::state sb) :dragging)
              t)
            (multiple-value-bind (page-w page-h) (scroll-viewport (%scrollbar-scroller sb))
              (incf (alloy:value sb) (* (if (< along start) -1 1)
                                        (ecase (alloy:orientation sb)
                                          (:horizontal page-w)
                                          (:vertical   page-h))))
              t)))
      (call-next-method)))

;;; A release ends the drag and never activates the bar (alloy component.lisp: the handle for
;;; (pointer-up component)).
(defmethod alloy:handle ((event alloy:pointer-up) (sb scrollbar))
  (if (alloy:active-p sb) (call-next-method) t))

;;; A plain hover does not take alloy's path, or the chain would go :strong.
(defmethod alloy:handle ((event alloy:pointer-move) (sb scrollbar))
  (cond ((alloy:active-p sb)
         (let ((travel (- (scrollbar-track-px sb) (* 2 *scrollbar-arrow-zone*)
                          (scrollbar-handle-length sb))))
           ;; a handle as long as the track has nowhere to go
           (when (plusp travel)
             (destructuring-bind (low . high) (alloy:range sb)
               ;; the slider clamps the value to the range (alloy components/slider.lisp:
               ;; (setf value) :around for slider)
               (setf (alloy:value sb)
                     (+ low (* (- high low)
                               (/ (- (scrollbar-along sb (alloy:location event))
                                     *scrollbar-arrow-zone* (scrollbar-grab sb))
                                  travel))))))
           t))
        ((chrome-hover-light sb event) nil)
        (t (call-next-method))))

(defun scrollbar-arrow-pattern (sb)
  "Arrow colour of SB: the handle colour, faded in with the hover."
  (let ((c *scrollbar-handle*))
    (chroma:rgb (chroma:r c) (chroma:g c) (chroma:b c)
                (ease-out (scrollbar-progress sb)))))

;;; A bar hosted by the PARENT layout (view/scroll.lisp: scroll-chrome-host) sits over a
;;; dialog item whose subtree is raised, so it has to outrank that elevation.
(defmethod alloy:render :around ((renderer window) (sb scrollbar))
  (simple:with-pushed-transforms (renderer)
    (incf (simple:z-index renderer) (1+ *dialog-item-z-elevation*))
    (call-next-method)))

(presentations:define-realization (window alloy:scrollbar)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:handle simple:rectangle)
   (ecase (alloy:orientation alloy:renderable)
     (:horizontal (alloy:extent 0 3 30 (alloy:ph)))
     (:vertical   (alloy:extent 3 0 (alloy:pw) 30)))
   :corner-radii (vector (alloy:px 3) (alloy:px 3) (alloy:px 3) (alloy:px 3)))
  ((:arrow-min simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :hidden-p T)
  ((:arrow-max simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :hidden-p T))

(presentations:define-update (window alloy:scrollbar)
  (:background
   :pattern *scrollbar-track*)
  (:handle
   :bounds (let ((len (scrollbar-handle-length alloy:renderable)))
             (multiple-value-bind (cross ct) (scrollbar-handle-cross alloy:renderable)
               (ecase (alloy:orientation alloy:renderable)
                 (:horizontal (alloy:px-extent 0 cross len ct))
                 (:vertical   (alloy:px-extent cross 0 ct len)))))
   :corner-radii (let ((r (max 1.0 (/ (nth-value 1 (scrollbar-handle-cross alloy:renderable)) 2))))
                   (vector (alloy:px r) (alloy:px r) (alloy:px r) (alloy:px r)))
   :offset (let ((start (scrollbar-thumb-start alloy:renderable)))
             (ecase (alloy:orientation alloy:renderable)
               (:horizontal (alloy:px-point start 0))
               (:vertical   (alloy:px-point 0 start))))
   :pattern *scrollbar-handle*)
  (:arrow-min
   :points (scrollbar-arrow-points alloy:renderable :min)
   :pattern (scrollbar-arrow-pattern alloy:renderable)
   :hidden-p (< (scrollbar-progress alloy:renderable) 0.02))
  (:arrow-max
   :points (scrollbar-arrow-points alloy:renderable :max)
   :pattern (scrollbar-arrow-pattern alloy:renderable)
   :hidden-p (< (scrollbar-progress alloy:renderable) 0.02)))

;;; Scroller: where its two bars meet, the corner square, with the size grip of the
;;; native Win32 corner filler when that corner is the window's own.

(defun scroll-corner-bounds (scroller)
  "Local bounds of the square shared by SCROLLER's horizontal and vertical bars."
  (let* ((bounds (alloy:bounds scroller))
         (gutter *scrollbar-track-px*))
    (alloy:px-extent (max 0 (- (alloy:pxw bounds) gutter))
                     0 gutter gutter)))

(defun scroll-corner-visible-p (scroller)
  "True when SCROLLER shows both bars."
  (multiple-value-bind (horizontal vertical) (scrollbar-axes scroller)
    (and horizontal vertical)))

(defun scroll-grip-dot-bounds (scroller col row)
  "One dimple of the size grip, COL from the right and ROW from the bottom of the corner square."
  ;; the native Win32 corner filler: 3 px dot, 4 px pitch, 3 px right and 2 px bottom margin
  (let ((corner (scroll-corner-bounds scroller)))
    (alloy:px-extent (- (+ (alloy:pxx corner) (alloy:pxw corner)) 6.0 (* 4.0 col))
                     (+ (alloy:pxy corner) 2.0 (* 4.0 row))
                     3.0 3.0)))

(defparameter *scroll-grip-window-tolerance* 3.0
  "How far the corner square may sit from the window edges and still count as the window's own corner.")

(defun scroll-grip-visible-p (scroller)
  "True when SCROLLER's corner square is the window's own bottom-right corner, the only place Win32 draws the grip."
  ;; window coordinates, y-up: the square's right edge must reach the window width and
  ;; its base must reach y = 0
  (and (scroll-corner-visible-p scroller)
       (let* ((tree   (alloy:layout-tree scroller))
              (window (and tree (alloy:ui tree))))
         (and window
              (alloy:with-unit-parent scroller
                (let ((g (alloy:global-location scroller))
                      (c (scroll-corner-bounds scroller))
                      (tol *scroll-grip-window-tolerance*))
                  (and (<= (abs (- (+ (alloy:pxx g) (alloy:pxx c) (alloy:pxw c))
                                   (alloy:pxw (alloy:bounds window))))
                           tol)
                       (<= (abs (+ (alloy:pxy g) (alloy:pxy c))) tol))))))))

(presentations:define-realization (window scroller)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern colors:transparent)
  ((:scroll-corner simple:rectangle)
   (alloy:px-extent 0 0 0 0)
   :pattern *scrollbar-track*
   :z-index 1001)
  ((:grip-1 simple:rectangle)
   (alloy:px-extent 0 0 0 0)
   :pattern *scrollbar-grip*
   :z-index 1002)
  ((:grip-2 simple:rectangle)
   (alloy:px-extent 0 0 0 0)
   :pattern *scrollbar-grip*
   :z-index 1002)
  ((:grip-3 simple:rectangle)
   (alloy:px-extent 0 0 0 0)
   :pattern *scrollbar-grip*
   :z-index 1002)
  ((:grip-4 simple:rectangle)
   (alloy:px-extent 0 0 0 0)
   :pattern *scrollbar-grip*
   :z-index 1002)
  ((:grip-5 simple:rectangle)
   (alloy:px-extent 0 0 0 0)
   :pattern *scrollbar-grip*
   :z-index 1002)
  ((:grip-6 simple:rectangle)
   (alloy:px-extent 0 0 0 0)
   :pattern *scrollbar-grip*
   :z-index 1002)
  ((:border simple:rectangle)
   (alloy:margins)
   :line-width (alloy:un 1)
   :pattern colors:transparent
   :z-index 1000))

(presentations:define-update (window scroller)
  (:background
   :pattern (or (widget-background alloy:renderable) colors:transparent))
  (:scroll-corner
   :bounds (scroll-corner-bounds alloy:renderable)
   :pattern *scrollbar-track*
   :hidden-p (not (scroll-corner-visible-p alloy:renderable)))
  (:grip-1
   :bounds (scroll-grip-dot-bounds alloy:renderable 0 0)
   :hidden-p (not (scroll-grip-visible-p alloy:renderable)))
  (:grip-2
   :bounds (scroll-grip-dot-bounds alloy:renderable 1 0)
   :hidden-p (not (scroll-grip-visible-p alloy:renderable)))
  (:grip-3
   :bounds (scroll-grip-dot-bounds alloy:renderable 2 0)
   :hidden-p (not (scroll-grip-visible-p alloy:renderable)))
  (:grip-4
   :bounds (scroll-grip-dot-bounds alloy:renderable 0 1)
   :hidden-p (not (scroll-grip-visible-p alloy:renderable)))
  (:grip-5
   :bounds (scroll-grip-dot-bounds alloy:renderable 1 1)
   :hidden-p (not (scroll-grip-visible-p alloy:renderable)))
  (:grip-6
   :bounds (scroll-grip-dot-bounds alloy:renderable 0 2)
   :hidden-p (not (scroll-grip-visible-p alloy:renderable)))
  (:border
   :hidden-p (null (widget-border alloy:renderable))
   :pattern (or (widget-border alloy:renderable) colors:transparent)
   :line-width (alloy:un (widget-border-width alloy:renderable))))
