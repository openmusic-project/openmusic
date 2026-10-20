(in-package :alloy-desktop)

;;; Pane scrolling as CAPI gives it (:horizontal-scroll/:vertical-scroll, native bars, scroll-callback).
;;; SCROLL-OFFSET holds x = scrolled right, y = scrolled DOWN; children's bounds are never rewritten.

(defclass scroll-mixin ()
  ((scrollbars        :initarg :scrollbars        :initform nil :accessor scrollbars)
   ;; stored and never read: CAPI has no :retain-scrollbars initarg
   (retain-scrollbars :initarg :retain-scrollbars :initform nil :accessor retain-scrollbars)
   (field-width       :initarg :field-width       :initform 0   :accessor field-width)
   (field-height      :initarg :field-height      :initform 0   :accessor field-height)
   (scroll-offset     :initform nil :accessor scroll-offset)))

(defparameter *scrollbar-track-px* 17.0
  "Gutter a visible scrollbar takes from the scrolled element.")

(defclass scroller (scroll-mixin view) ()
  (:documentation "A view whose two bars meet in a corner square."))

(defun scroll-active-p (element)
  "True when ELEMENT was configured to scroll, i.e. its SCROLLBARS are non-NIL."
  (and (scrollbars element) t))

(defun scroll-axis-h-p (scroll)
  "True when the SCROLLBARS value SCROLL allows the horizontal axis."
  (or (equal scroll :h) (equal scroll t)))

(defun scroll-axis-v-p (scroll)
  "True when the SCROLLBARS value SCROLL allows the vertical axis."
  (or (equal scroll :v) (equal scroll t)))

(defun scroll-x (element)
  "ELEMENT's horizontal scroll offset, 0 when it never scrolled."
  (let ((o (scroll-offset element))) (if o (alloy:pxx o) 0)))

(defun scroll-y (element)
  "ELEMENT's vertical scroll offset (Y-down), 0 when it never scrolled."
  (let ((o (scroll-offset element))) (if o (alloy:pxy o) 0)))

(defgeneric scroll-passive-p (element)
  (:documentation "True for an element that configured no scroll: no clip, bars, child translate or wheel.")
  (:method ((element t)) nil))

(defgeneric scroll-chrome-p (element)
  (:documentation "True for the scrollbars: chrome never scrolls with the content.")
  (:method ((element t)) nil))

(defgeneric scroll-offsets-children-p (element)
  (:documentation "True when ELEMENT translates its child elements by its scroll offset.")
  (:method ((element t)) nil)
  (:method ((element alloy:clip-view)) t)
  (:method ((element scroll-mixin))
    (and (typep element 'alloy:layout) (not (scroll-passive-p element)) t)))

(defgeneric scroll-content-size (element)
  (:documentation "(values width height) of the scrollable content: how far the children reach.")
  (:method ((element t)) (element-size element))
  (:method ((element view))
    ;; the right and Y-down bottom edges of the children, the bars excluded, never less than
    ;; the view; retained until a child enters, leaves, moves or is resized
    (let ((extent (content-extent element)))
      (if extent
          (values (car extent) (cdr extent))
          (multiple-value-bind (w h) (element-size element)
            (let ((height (alloy:pxh (alloy:bounds element))))
              (alloy:do-elements (child element)
                (unless (scroll-chrome-p child)
                  (let ((bounds (alloy:bounds child)))
                    (setf w (max w (round (+ (alloy:pxx bounds) (alloy:pxw bounds))))
                          h (max h (round (- height (alloy:pxy bounds))))))))
              (setf (content-extent element) (cons w h))
              (values w h)))))))

;;; Children change place through these four doors: entering and leaving, ALLOY:UPDATE (PLACE,
;;; PLACE-ELEMENT and the refit of a retained child) and the refit the setters run.

(defmethod alloy:enter :after ((element alloy:layout-element) (layout view) &key)
  (setf (content-extent layout) nil))

(defmethod alloy:leave :after ((element alloy:layout-element) (layout view))
  (setf (content-extent layout) nil))

(defmethod alloy:update :after ((element alloy:layout-element) (layout view) &key)
  (setf (content-extent layout) nil))

(defmethod alloy:refit :after ((layout view))
  (setf (content-extent layout) nil))

(defgeneric scroll-view-size (element)
  (:documentation "(values width height) of the area the scrolled element occupies.")
  (:method ((element t)) (element-size element)))

(defgeneric scroll-applied (element)
  (:documentation "Hook run after ELEMENT's scroll offset changed, for content the child translate does not reach.")
  (:method ((element t)) nil))

(defgeneric scrollbars-forced-p (element)
  (:documentation "True when ELEMENT shows every axis its SCROLLBARS allow, overflowing or not.")
  (:method ((element t)) nil))

(defgeneric scroll-changed (element dimension operation position)
  (:documentation "Notification that ELEMENT scrolled to POSITION along DIMENSION (:horizontal or :vertical).")
  (:method ((element t) dimension operation position)
    (declare (ignore dimension operation position))
    nil))

(defun field-size (element)
  "(values width height) of ELEMENT's field: its content, never less than the configured field."
  (multiple-value-bind (cw ch) (scroll-content-size element)
    (values (max cw (field-width element)) (max ch (field-height element)))))

(defun scrollbar-axes (self)
  "(values h-p v-p) of the bars SELF shows now, in two passes since each bar steals space from the other."
  (let ((allowed (scrollbars self)))
    (if (null allowed)
        (values nil nil)
        (multiple-value-bind (fw fh) (field-size self)
          (multiple-value-bind (vw vh) (scroll-view-size self)
            (let* ((h-ok (and (scroll-axis-h-p allowed) t))
                   (v-ok (and (scroll-axis-v-p allowed) t))
                   (h    (and h-ok (> fw vw)))
                   (v    (and v-ok (> fh vh))))
              (when (and h (not v) v-ok) (setf v (> fh (- vh *scrollbar-track-px*))))
              (when (and v (not h) h-ok) (setf h (> fw (- vw *scrollbar-track-px*))))
              (when (scrollbars-forced-p self)
                (setf h h-ok v v-ok))
              (values h v)))))))

(defun scroll-viewport (self)
  "(values width height) of the area available to SELF's content: the element minus the gutters."
  (multiple-value-bind (h v) (scrollbar-axes self)
    (multiple-value-bind (vw vh) (scroll-view-size self)
      (values (- vw (if v *scrollbar-track-px* 0))
              (- vh (if h *scrollbar-track-px* 0))))))

(defun scroll-content-global-extent (self)
  "Global clipping extent of SELF's scrollable content, the scrollbar gutters excluded."
  ;; alloy is y-up: the horizontal gutter is at the bottom, the vertical one at the right
  (multiple-value-bind (h v) (scrollbar-axes self)
    (alloy::with-global-bounds (bounds self)
      (let ((gutter *scrollbar-track-px*))
        (alloy:px-extent (alloy:pxx bounds)
                         (+ (alloy:pxy bounds) (if h gutter 0))
                         (max 0 (- (alloy:pxw bounds) (if v gutter 0)))
                         (max 0 (- (alloy:pxh bounds) (if h gutter 0))))))))

(defun scroll-maxima (self)
  "(max-x . max-y): how far the field exceeds the viewport, never negative."
  (multiple-value-bind (fw fh) (field-size self)
    (multiple-value-bind (vw vh) (scroll-viewport self)
      (cons (max 0 (round (- fw vw)))
            (max 0 (round (- fh vh)))))))

(defun clamp-scroll (self x y)
  "X and Y rounded and clamped to SELF's scroll range."
  (let ((m (scroll-maxima self)))
    (values (min (max 0 (round x)) (car m))
            (min (max 0 (round y)) (cdr m)))))

;;; alloy:offset speaks ALLOY's convention: the translation applied to the content, y-up.

(defmethod alloy:offset ((self scroll-mixin))
  (alloy:px-point (- (scroll-x self)) (scroll-y self)))

(defmethod (setf alloy:offset) (value (self scroll-mixin))
  (set-scroll self (- (alloy:pxx value)) (alloy:pxy value))
  value)

;;; The offset reaches the geometry pipeline through alloy's composition hook
;;; (alloy layout.lisp: the defgeneric global-position-offset), summed once per ancestor.
(defmethod alloy::global-position-offset ((element scroll-mixin))
  (if (scroll-offsets-children-p element)
      (alloy:offset element)
      (load-time-value (alloy:px-point 0 0) t)))

(defclass scroll-model (org.shirakumo.alloy.observables:observable)
  ((scroller :initarg :scroller :reader model-scroller)))

(defvar *scroller-bars* (make-hash-table :test 'eq :weakness :key)
  "Scrollable -> (model h-bar v-bar geometry-key host); weak, so a closed pane drops its bars.")

(defun mark-scrollbars (self)
  "Mark SELF's bars for render: their thumb derives from alloy:slider-unit, recomputed only when marked."
  (let ((set (gethash self *scroller-bars*)))
    (when set
      (when (second set) (alloy:mark-for-render (second set)))
      (when (third set)  (alloy:mark-for-render (third set))))))

(defun set-scroll (self new-x new-y &key (operation :move))
  "Clamp, store and redraw SELF's offset, then notify SCROLL-CHANGED per moved axis."
  (multiple-value-bind (x y) (clamp-scroll self new-x new-y)
    (let ((old-x (scroll-x self))
          (old-y (scroll-y self)))
      (unless (and (= x old-x) (= y old-y))
        (setf (scroll-offset self) (alloy:px-point x y))
        (alloy:mark-for-render self)
        (mark-scrollbars self)
        (scroll-applied self)
        (unless (= x old-x) (scroll-changed self :horizontal operation x))
        (unless (= y old-y) (scroll-changed self :vertical   operation y)))
      (values x y))))

(defun clamp-scroll-to-range (self)
  "Bring SELF's scroll back into its range and repaint SELF and its bars, after its field or its view changed."
  (set-scroll self (scroll-x self) (scroll-y self))
  (alloy:mark-for-render self)
  (mark-scrollbars self))

(defmethod (setf field-width) :after (value (self scroll-mixin))
  (declare (ignore value))
  (clamp-scroll-to-range self))

(defmethod (setf field-height) :after (value (self scroll-mixin))
  (declare (ignore value))
  (clamp-scroll-to-range self))

(defun chrome-hover-light (element event)
  "Light ELEMENT under the pointer without alloy's :strong promotion of the chain; true when ELEMENT owns the move."
  ;; chrome is never a focus target (capi-common-elements-ug-1.htm:108), and a :strong chrome
  ;; chain stops declining input (alloy focus-tree.lisp: the handle for (input-event focus-chain))
  (when (and (slot-boundp element 'alloy:focus-parent)
             (alloy:contained-p (alloy:location event) element))
    (when (null (alloy:focus element))
      (setf (alloy:focus element) :weak))
    t))

(defclass scrollbar (alloy:scrollbar)
  ;; px from the start of the handle to the point of it the pointer pressed
  ((grab :initform 0 :accessor scrollbar-grab)))

(defmethod scroll-chrome-p ((element scrollbar)) t)

(defmethod alloy::compute-global-position ((element scrollbar))
  "Take back the offset of the layout hosting the bar, which is the layout the bar scrolls."
  (multiple-value-bind (x y) (call-next-method)
    (if (alloy:layout-tree element)
        (let ((o (alloy::global-position-offset (alloy:layout-parent element))))
          (values (- x (alloy:pxx o)) (- y (alloy:pxy o))))
        (values x y))))

(defun %scrollbar-scroller (sb) (model-scroller (alloy:data sb)))

;;; alloy's slider consumes arrows whatever its orientation (alloy components/slider.lisp:
;;; the handle for (key-down slider)), so a bar would swallow the plain arrows of the content.
(defmethod alloy:handle ((event alloy:key-down) (sb scrollbar))
  (alloy:decline))

(defmethod alloy:handle ((event alloy:scroll) (sb scrollbar))
  (declare (ignore event sb))
  ;; pointer handlers retain drag ownership; wheel belongs to the clip or window
  (alloy:decline))

;;; Alloy's vertical axis grows UP but scroll-y grows DOWN, so the vertical value is inverted.

(defmethod alloy:value ((sb scrollbar))
  (let ((s (%scrollbar-scroller sb)))
    (ecase (alloy:orientation sb)
      (:horizontal (scroll-x s))
      (:vertical   (- (cdr (scroll-maxima s)) (scroll-y s))))))

(defmethod (setf alloy:value) (v (sb scrollbar))
  (let ((s (%scrollbar-scroller sb)))
    (ecase (alloy:orientation sb)
      (:horizontal (set-scroll s v (scroll-y s)))
      (:vertical   (set-scroll s (scroll-x s) (- (cdr (scroll-maxima s)) v)))))
  v)

(defmethod alloy:range ((sb scrollbar))
  (let ((m (scroll-maxima (%scrollbar-scroller sb))))
    (ecase (alloy:orientation sb)
      (:horizontal (cons 0 (car m)))
      (:vertical   (cons 0 (cdr m))))))

(defmethod alloy:scroll-handle-size ((sb scrollbar))
  "Thumb length as a fraction of the track: viewport / field."
  (let ((s (%scrollbar-scroller sb)))
    (multiple-value-bind (fw fh) (field-size s)
      (multiple-value-bind (vw vh) (scroll-viewport s)
        (max 0.08
             (min 1.0
                  (ecase (alloy:orientation sb)
                    (:horizontal (/ vw (max 1 fw)))
                    (:vertical   (/ vh (max 1 fh))))))))))

;;; The bars live in the layout that can place them freely: the scroller itself when it
;;; is a fixed-layout, else its parent (a list would stack a bar as content).

(defun scroll-chrome-host (self)
  (if (typep self 'alloy:fixed-layout) self (layout-attached-p self)))

(defun enter-chrome (bar host)
  ;; hover-expand and thumb drag need a focus parent too
  ;; (alloy component.lisp: the handle for (pointer-move component))
  (alloy:enter bar host)
  (let ((tree (alloy:layout-tree host)))
    (when (and tree (not (focus-attached-p bar)))
      (let ((focus (chrome-focus-parent (alloy:ui tree))))
        (when focus (alloy:enter bar focus))))))

(defun leave-chrome (bar host)
  (when (focus-attached-p bar)
    (alloy:leave bar (alloy:focus-parent bar)))
  (alloy:leave bar host))

(defun ensure-scrollbars (view)
  "Create, place or drop VIEW's scrollbars to match its field, a no-op once the geometry settled."
  (let ((host (scroll-chrome-host view)))
    (when (and host (alloy:layout-tree host))
      (multiple-value-bind (want-h want-v) (scrollbar-axes view)
        (let* ((set (or (gethash view *scroller-bars*)
                        (setf (gethash view *scroller-bars*)
                              (list (make-instance 'scroll-model :scroller view)
                                    nil nil nil nil))))
               (bounds (alloy:bounds view))
               (own    (eq host view))
               (x0     (if own 0.0 (alloy:pxx bounds)))
               (y0     (if own 0.0 (alloy:pxy bounds)))
               (vw     (alloy:pxw bounds))
               (vh     (alloy:pxh bounds))
               (w      *scrollbar-track-px*)
               (key    (list host want-h want-v x0 y0 vw vh w)))
          (unless (equal key (fourth set))
            (let ((old-host (fifth set)))
              (when (and old-host (not (eq old-host host)))
                (when (second set) (leave-chrome (second set) old-host) (setf (second set) nil))
                (when (third set)  (leave-chrome (third set)  old-host) (setf (third set) nil))))
            (setf (fourth set) key
                  (fifth set)  host)
            (let ((model (first set)))
              (when (and want-h (null (second set)))
                (let ((bar (make-instance 'scrollbar :data model :orientation :horizontal)))
                  (enter-chrome bar host)
                  (setf (second set) bar)))
              (when (and want-v (null (third set)))
                (let ((bar (make-instance 'scrollbar :data model :orientation :vertical)))
                  (enter-chrome bar host)
                  (setf (third set) bar))))
            (when (and (not want-h) (second set))
              (leave-chrome (second set) host)
              (setf (second set) nil))
            (when (and (not want-v) (third set))
              (leave-chrome (third set) host)
              (setf (third set) nil))
            ;; alloy is y-up: y=0 is the BOTTOM edge, where the horizontal bar lives
            (let ((hbar (second set))
                  (vbar (third set)))
              (alloy:with-unit-parent host
                (when vbar
                  (alloy:update vbar host
                                :x (+ x0 (- vw w)) :y (+ y0 (if hbar w 0))
                                :w w :h (- vh (if hbar w 0))))
                (when hbar
                  (alloy:update hbar host
                                :x x0 :y y0
                                :w (- vw (if vbar w 0)) :h w))))
            ;; a view that grew shrinks the range under the scroll
            (clamp-scroll-to-range view)))))))

(defun drop-scrollbars (view)
  "Detach VIEW's bars from their host and forget them, so a removed pane leaves no orphan bars."
  (let ((set (gethash view *scroller-bars*)))
    (when set
      (let ((host (fifth set)))
        (when host
          (when (second set) (leave-chrome (second set) host))
          (when (third set)  (leave-chrome (third set)  host))))
      (remhash view *scroller-bars*))))

;;; The wheel. A pane hosted in an alloy:scroll-view has TWO candidate scrollers, and the one
;;; the scrollbar reads and writes is the clip-view's offset (alloy components/scroll.lisp:
;;; value and range of y-scrollbar), so there the wheel moves the CLIP: one owner, the bar
;;; follows. A pane with no clip ancestor keeps the offset itself. A pane that configured no
;;; scroll (scroll-passive-p) only hands the wheel to its children and to PANE-SCROLLED.

(defun modifier-list ()
  "The modifiers of the event being dispatched, as a list of :shift, :command and :option."
  (let ((mods '()))
    (when *option-key-p* (push :option mods))
    (when *command-key-p* (push :command mods))
    (when *shift-key-p* (push :shift mods))
    mods))

(defmethod alloy:handle ((event alloy:scroll) (view scroll-mixin))
  (let ((passive (scroll-passive-p view)))
    (or (and (or (scroll-offsets-children-p view) passive)
             (dispatch-to-children event view))
        (pane-scrolled view event (modifier-list))
        (and (not passive)
             (let ((clip (find-ancestor-clip-view view)))
               (if clip
                   (scroll-clip-by-wheel clip event)
                   (let ((m (scroll-maxima view)))
                     (unless (and (zerop (car m)) (zerop (cdr m)))
                       (set-scroll view
                                   (- (scroll-x view) (* *wheel-step* (alloy:dx event)))
                                   (- (scroll-y view) (* *wheel-step* (alloy:dy event))))
                       t)))))
        (alloy:decline))))

;;; fixed-layout's ensure-visible ROTATEFs the elements so the focused one draws last
;;; (alloy layouts/fixed.lisp); sibling panes keep a stable z-order as CAPI's do.
(defmethod alloy:ensure-visible :around ((element alloy:layout-element) (layout view))
  element)
