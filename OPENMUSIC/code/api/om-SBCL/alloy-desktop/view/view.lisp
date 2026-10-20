(in-package :alloy-desktop)

(defclass retaining-layout (alloy:fixed-layout)
  ((child-specs :initform (make-hash-table) :reader child-specs)
   (focus-children :initform (make-hash-table) :reader focus-children))
  (:documentation "A fixed layout that keeps its children where they were put. The geometry
each child entered with is retained and re-applied on every refit; the last geometry the
program gave a child, by PLACE, PLACE-ELEMENT, SET-ELEMENT-POSITION or SET-ELEMENT-SIZE, is
re-placed Y-down from the layout's top edge (a PLACEd child stretched and anchored by its
RIGHT and BOTTOM distances until a setter moves or resizes it), so the content stays put
when the layout grows or shrinks. REFIT-CHILD lets the host re-place a child from geometry
of its own. The focus element of every child given the window's focus by JOIN-WINDOW-FOCUS
is kept too: it joins the focus list of each window the layout enters, and leaves it with
the child."))

(defclass fixed-layout (retaining-layout)
  ()
  (:documentation "A RETAINING-LAYOUT whose children keep a stable z-order and are hit in
pane-before-pinboard-object order."))

(defclass retained-fixed-layout (fixed-layout) ())

;;; alloy's fixed-layout ensure-visible ROTATEFs the elements vector so the focused element is
;;; drawn last (alloy layouts/fixed.lisp: ensure-visible :before for (layout-element fixed-layout)),
;;; and hover alone focuses (alloy component.lisp: handle for (pointer-move component), (setf focus)
;;; :after for (t component)); toolkit dialog items are sibling child windows with a stable
;;; z-order, so a label that overlaps its neighbour must not repaint over it just because the
;;; pointer crossed it.
(defmethod alloy:ensure-visible :around ((element alloy:layout-element) (layout fixed-layout))
  element)

(defmethod alloy:enter :before ((element alloy:layout-element) (layout retaining-layout)
                                &key x y w h extent)
  (setf (gethash element (child-specs layout))
        (list :x x :y y :w w :h h :extent extent)))

(defmethod alloy:leave :before ((element alloy:layout-element) (layout retaining-layout))
  (let ((focus (gethash element (focus-children layout))))
    (remhash element (child-specs layout))
    (remhash element (focus-children layout))
    (when focus
      (let ((window-focus (tree-focus-list (alloy:layout-tree layout))))
        (when (and window-focus (eq (focus-attached-p focus) window-focus))
          (alloy:leave focus window-focus))))))

;;; A layout joins a window's layout tree through set-layout-tree, which alloy propagates to
;;; every element below it (alloy layout.lisp: set-layout-tree :before for layout).
(defmethod alloy:set-layout-tree :after ((tree alloy:layout-tree) (layout retaining-layout))
  (let ((children (focus-children layout)))
    (when (plusp (hash-table-count children))
      (let ((focus (tree-focus-list tree)))
        (maphash (lambda (element child-focus)
                   (declare (ignore element))
                   (attach-to-focus child-focus focus))
                 children)))))

(defgeneric refit-child (layout child)
  (:documentation "Re-place CHILD during a refit of the RETAINING-LAYOUT LAYOUT and answer true,
or answer NIL to let the geometry LAYOUT retained for CHILD decide.")
  (:method ((layout t) (child t))
    nil))

(defmethod alloy:refit ((layout retaining-layout))
  (when (alloy:layout-tree layout)
    (alloy:with-unit-parent layout
      (maphash (lambda (element specs)
                 (cond
                   ((refit-child layout element))
                   ((eq (first specs) :place)
                    (apply-place-spec layout element (rest specs)))
                   ((eq (first specs) :px)
                    (apply #'set-placed-bounds layout element (rest specs)))
                   (t
                    (destructuring-bind (&key x y w h extent) specs
                      ;; plain numbers were applied once, on entry; only units and
                      ;; extents depend on the layout's live size
                      (when (or extent
                                (typep x 'alloy:unit)
                                (typep y 'alloy:unit)
                                (typep w 'alloy:unit)
                                (typep h 'alloy:unit))
                        (let ((e (alloy::copy-extent (alloy:bounds element))))
                          (setf (alloy:bounds element)
                                (alloy:px-extent
                                 (or x (and extent (alloy:extent-x extent)) (alloy:extent-x e))
                                 (or y (and extent (alloy:extent-y extent)) (alloy:extent-y e))
                                 (or w (and extent (alloy:extent-w extent)) (alloy:extent-w e))
                                 (or h (and extent (alloy:extent-h extent)) (alloy:extent-h e))))
                          ;; as placement.lisp: set-placed-bounds marks a re-placed child
                          (unless (alloy:extent= e (alloy:bounds element))
                            (invalidate element))))))))
               (child-specs layout)))))

;;; CAPI hit order: a pinboard object (pinboard-object-p) is swept after the panes
;;; (widgets/controls.lisp: dispatch-in-capi-order).
(defmethod alloy:handle ((event alloy:pointer-event) (layout fixed-layout))
  (or (dispatch-in-capi-order event layout) (alloy:decline)))

;;; alloy dispatches pointer-move through its own layout method (alloy layout.lisp: handle for
;;; (pointer-move layout)), more specific than the pointer-event one, so the ordering needs its
;;; own method.
(defmethod alloy:handle ((event alloy:pointer-move) (layout fixed-layout))
  (or (dispatch-in-capi-order event layout) (alloy:decline)))

(defclass view (retaining-layout alloy:renderable)
  ;; (width . height) SCROLL-CONTENT-SIZE answered last, NIL once a child entered, left, moved or resized
  ((content-extent :initform nil :accessor content-extent)
   ;; what WIDGET-BACKGROUND, WIDGET-BORDER and WIDGET-BORDER-WIDTH answer for the view
   ;; (widgets/dialog-item.lisp): chroma colours or NIL for none, and the border width
   (background :initarg :background :initform nil :accessor view-background)
   (border :initarg :border :initform nil :accessor view-border)
   (border-width :initarg :border-width :initform 1 :accessor view-border-width)))

(defmethod (setf view-background) :after (value (view view))
  (invalidate view))

(defmethod (setf view-border) :after (value (view view))
  (invalidate view))

(defmethod (setf view-border-width) :after (value (view view))
  (invalidate view))

;; Alloy asks every renderable for its focus (alloy default.lisp: define-update of (default-look-and-feel
;; alloy:renderable)); a view answers as never focused unless a subclass mixes in alloy:focus-element.
(defmethod alloy:focus ((view view))
  (if (next-method-p) (call-next-method) nil))
(defmethod (setf alloy:focus) (val (view view))
  (if (next-method-p) (call-next-method) val))
(defmethod alloy:set-focus-tree (tree (view view))
  (if (next-method-p) (call-next-method) tree))

(defclass item-view (alloy:layout-element alloy:renderable) ()
  (:documentation "A pinboard object: no layout of its own, realized flat in its host (see enter-flat)."))

(defun element-at-location (window location &key (test (constantly t)))
  "The deepest element of WINDOW's layout tree that satisfies TEST and contains LOCATION, or NIL."
  (let ((found nil))
    (labels ((walk (el)
               (when (and (funcall test el)
                          (alloy:contained-p location el))
                 (setf found el))
               (when (typep el 'alloy:container)
                 (alloy:do-elements (child el) (walk child)))))
      (walk (alloy:root (alloy:layout-tree window)))
      found)))

(defun map-elements (function root)
  "Call FUNCTION on ROOT and on every element below it, each parent before its children."
  (funcall function root)
  (when (typep root 'alloy:container)
    (alloy:do-elements (child root) (map-elements function child))))

;;; Invalidation. The frame loop renders a window only when the window itself answers
;;; render-needed-p (core/frame-scheduling.lisp: frame-desktop-windows), so every element
;;; that asks for a repaint, and every layout that needs a refit, flags its window too.

(defun mark-owner-window-for-render (element)
  "Propagate ELEMENT invalidation to its owning Alloy window; a UI that renders nothing
itself (a bare alloy:ui) has nothing to flag."
  (let* ((tree (alloy:layout-tree element))
         (window (and tree
                      (slot-boundp tree 'alloy:ui)
                      (alloy:ui tree))))
    (when (and (typep window 'alloy:renderable) (not (eq window element)))
      (alloy:mark-for-render window))))

(defmethod alloy:mark-for-render :after ((element alloy:layout-element))
  (mark-owner-window-for-render element))

(defmethod (setf alloy:layout-needed-p) :after
    ((value (eql t)) (layout alloy:layout))
  (mark-owner-window-for-render layout))

(defun invalidate (element)
  "Ask for ELEMENT to be repainted on the next frame, with its window; answer ELEMENT.
The whole element repaints: Alloy has no damage region."
  (when (typep element 'alloy:renderable)
    (alloy:mark-for-render element))
  element)

(defun layout-owner (element)
  "ELEMENT's parent in the layout tree; NIL at the root, which owns itself
(alloy layout.lisp: the notice-size for (layout-element (eql T)))."
  (when (and (typep element 'alloy:layout-element)
             (slot-boundp element 'alloy::layout-parent))
    (let ((parent (alloy:layout-parent element)))
      (unless (eq parent element) parent))))
