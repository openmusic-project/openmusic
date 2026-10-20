(in-package :alloy-desktop)

(defvar *size-constraint* nil
  "Function of (element width height) answering the constrained (values width height), or NIL for no constraint.")

(defun constrain-size (element width height)
  "WIDTH and HEIGHT as *SIZE-CONSTRAINT* allows them for ELEMENT."
  (if *size-constraint*
      (funcall *size-constraint* element width height)
      (values width height)))

(defun focus-attached-p (element)
  "The focus parent ELEMENT is entered into, or NIL (focus-parent has no initform)."
  (and (typep element 'alloy:focus-element)
       (slot-boundp element 'alloy:focus-parent)
       (alloy:focus-parent element)))

(defun layout-part (element)
  "ELEMENT's layout element: ELEMENT itself, or the layout element of a structure, which
enters a layout through it (alloy structure.lisp: the enter deferral for layout-element)."
  (if (typep element 'alloy:structure)
      (alloy:layout-element element)
      element))

(defun layout-attached-p (element)
  "The layout ELEMENT (a structure: its layout element) is entered into, or NIL."
  (let ((element (layout-part element)))
    (and (typep element 'alloy:layout-element)
         (slot-boundp element 'alloy:layout-parent)
         (alloy:layout-parent element))))

(defun set-element-position (element x y)
  "Move ELEMENT so its top-left corner sits at Y-down (X, Y) in its layout parent; answer ELEMENT."
  (let* ((bounds   (alloy:bounds element))
         (w        (alloy:pxw bounds))
         (h        (alloy:pxh bounds))
         ;; flat pinboard objects have no layout-parent
         (parent   (layout-attached-p element))
         (parent-h (when parent (alloy:pxh (alloy:bounds parent))))
         (ay       (if parent-h (- parent-h y h) y)))
    (retain-geometry parent element x y nil nil)
    ;; on the pixel grid whatever the point holds, as CAPI places a pane
    ;; (capi:pinboard-pane-position answers integers for a ratio)
    (setf (slot-value element 'alloy:bounds)
          (alloy:px-extent (round x) (round ay) (round w) (round h)))
    (when parent (alloy:refit parent))
    (when (typep element 'alloy:layout) (alloy:refit element))
    (alloy:mark-for-render element))
  element)

(defun element-position (element)
  "ELEMENT's Y-down top-left corner in its layout parent, as (values x y) in integer px."
  (let* ((bounds   (alloy:bounds element))
         (ax       (alloy:pxx bounds))
         (ay       (alloy:pxy bounds))
         (h        (alloy:pxh bounds))
         (parent   (layout-attached-p element))
         (parent-h (when parent (alloy:pxh (alloy:bounds parent))))
         (ky       (if parent-h (- parent-h ay h) ay)))
    (values (round ax) (round ky))))

(defun set-element-size (element width height)
  "Resize ELEMENT to WIDTH x HEIGHT keeping its Y-down top edge fixed; answer (values width height)."
  (let* ((bounds (alloy:bounds element))
         (ax     (alloy:pxx bounds))
         (old-ay (alloy:pxy bounds))
         (old-h  (alloy:pxh bounds))
         (parent (layout-attached-p element))
         ;; only an attached element sits Y-down over a Y-up parent; a detached
         ;; one has no such transform, so shifting it would corrupt its position
         (new-ay (if parent (+ old-ay (- old-h height)) old-ay)))
    (retain-geometry parent element nil nil width height)
    (setf (slot-value element 'alloy:bounds)
          (alloy:px-extent (round ax) (round new-ay) (round width) (round height)))
    (when parent (alloy:refit parent))
    (when (typep element 'alloy:layout) (alloy:refit element))
    (alloy:mark-for-render element))
  (values width height))

(defun element-size (element)
  "ELEMENT's size as (values width height) in integer px."
  (let ((bounds (alloy:bounds element)))
    (values (round (alloy:pxw bounds)) (round (alloy:pxh bounds)))))

(defun set-placed-bounds (panel child x y w h)
  "Set CHILD's bounds in PANEL from Y-down px X, Y, W, H, flipped over PANEL's live height. A
CHILD whose bounds change is marked for render: a look recomputes the geometry it derives from
the bounds only for a marked renderable (alloy renderers/simple/presentations/protocol.lisp:
render :before for renderable), and new bounds alone do not mark it."
  (let* ((element (layout-part child))
         (old     (alloy::copy-extent (alloy:bounds element))))
    (alloy:with-unit-parent panel
      (alloy:update child panel
                    :x (round x)
                    :y (round (- (alloy:pxh (alloy:bounds panel)) y h))
                    :w (round w) :h (round h))
      (unless (alloy:extent= old (alloy:bounds element))
        (invalidate element)))))

(defun place-element (panel child x y w h)
  "Set CHILD's bounds in PANEL from Y-down px X, Y, W, H, flipped over PANEL's live height,
while PANEL is in a window; answer CHILD. A PANEL that retains geometry (a FIXED-LAYOUT or a
VIEW) keeps CHILD there: every refit re-places it, the first one in a window included. A
focusable CHILD joins the focus list of PANEL's window, as by JOIN-WINDOW-FOCUS."
  (when (typep panel 'retaining-layout)
    (setf (gethash (layout-part child) (child-specs panel)) (list :px x y w h)))
  (when (alloy:layout-tree panel)
    (set-placed-bounds panel child x y w h))
  (join-window-focus panel child))

(defun retain-geometry (parent element x y w h)
  "Make the RETAINING-LAYOUT PARENT re-apply Y-down px X, Y, W, H to ELEMENT on every refit;
a NIL coordinate keeps the one retained in px, or else ELEMENT's current one. A PLACEd
ELEMENT stays in logical units, so it keeps following the window's scale, and loses the
far-edge anchors and the stretching of PLACE. Any other PARENT retains nothing."
  (when (typep parent 'retaining-layout)
    (let ((old (gethash element (child-specs parent))))
      (setf (gethash element (child-specs parent))
            (case (first old)
              (:px
               (destructuring-bind (ox oy ow oh) (rest old)
                 (list :px (or x ox) (or y oy) (or w ow) (or h oh))))
              (:place
               (multiple-value-bind (cx cy) (element-position element)
                 (multiple-value-bind (cw ch) (element-size element)
                   (let ((scale (layout-scale parent)))
                     (list :place
                           :x (/ (or x cx) scale) :y (/ (or y cy) scale)
                           :w (/ (or w cw) scale) :h (/ (or h ch) scale))))))
              (t
               (multiple-value-bind (cx cy) (element-position element)
                 (multiple-value-bind (cw ch) (element-size element)
                   (list :px (or x cx) (or y cy) (or w cw) (or h ch))))))))))

;;; Retained Y-down placement on a RETAINING-LAYOUT, in logical units.

(defun layout-scale (layout)
  "Px per logical unit in LAYOUT's window: its UI's base scale, 1 outside a window."
  (let* ((tree (alloy:layout-tree layout))
         (ui   (and tree (slot-boundp tree 'alloy:ui) (alloy:ui tree))))
    (if ui (alloy:base-scale ui) 1.0)))

(defun place-span (scale start size end parent)
  "(values START SIZE) in px along one axis of a PARENT px long: START and END both
given stretch the element between them, otherwise the one given anchors it and SIZE
is fixed. START, SIZE and END are logical units."
  (cond ((and start end)
         (let ((a (* scale start)))
           (values a (max 1 (- parent a (* scale end))))))
        (start (values (* scale start) (* scale size)))
        (t (let ((n (* scale size)))
             (values (- parent n (* scale end)) n)))))

(defun apply-place-spec (layout element spec)
  (destructuring-bind (&key x y w h right bottom) spec
    (let ((scale  (layout-scale layout))
          (bounds (alloy:bounds layout)))
      (multiple-value-bind (px pw) (place-span scale x w right (alloy:pxw bounds))
        (multiple-value-bind (py ph) (place-span scale y h bottom (alloy:pxh bounds))
          (set-placed-bounds layout element px py pw ph))))))

(defun place (parent child x y w h &key right bottom)
  "Put CHILD into the FIXED-LAYOUT or VIEW PARENT at Y-down X Y W H, in logical units (px times
the window's scale), and keep it there: every refit of PARENT re-places it. RIGHT and
BOTTOM are the distances to PARENT's right and bottom edges: given with X (Y) they
stretch CHILD between the two edges, given in place of X (Y) they anchor it to the far
edge with W (H) fixed. Each axis needs X or RIGHT (Y or BOTTOM). CHILD enters PARENT
when it is not there yet, and a focusable CHILD joins the focus list of PARENT's window, as
by JOIN-WINDOW-FOCUS; answer CHILD."
  (retain-place parent child (list :x x :y y :w w :h h :right right :bottom bottom))
  (join-window-focus parent child))

(defun retain-place (parent child spec)
  "Enter CHILD into the RETAINING-LAYOUT PARENT unless it is there, keep the PLACE geometry
SPEC for it and apply SPEC while PARENT is in a window."
  (unless (eq (layout-attached-p child) parent)
    (attach-to-layout child parent))
  (setf (gethash (layout-part child) (child-specs parent)) (cons :place spec))
  (when (alloy:layout-tree parent)
    (apply-place-spec parent child spec)))

(defun tree-focus-list (tree)
  "The content focus list of the window whose layout tree is TREE (WINDOW-FOCUS-LIST); NIL
for no tree, a tree with no UI, or a UI that is not a registered window."
  (and tree
       (slot-boundp tree 'alloy:ui)
       (window-focus-list (alloy:ui tree))))

(defun join-window-focus (parent child)
  "Enter the focus element of CHILD (CHILD itself, or the focus element of a structure) into
the content focus list of PARENT's window (WINDOW-FOCUS-LIST), leaving a different focus
parent first: now while PARENT is in a window's layout tree, and again each time a
FIXED-LAYOUT or VIEW PARENT joins one. CHILD leaving such a PARENT takes it out of that
list again. A CHILD with no focus element is left alone; answer CHILD."
  (let ((focus (typecase child
                 (alloy:focus-element child)
                 (alloy:structure (alloy:focus-element child)))))
    (when focus
      (when (typep parent 'retaining-layout)
        (setf (gethash (layout-part child) (focus-children parent)) focus))
      (attach-to-focus focus (tree-focus-list (alloy:layout-tree parent)))))
  child)

(defun add-element (parent child &key x y w h right bottom (focus t))
  "Add CHILD to the layout PARENT, place it and give it the keyboard focus chain; answer CHILD.
With X or RIGHT and Y or BOTTOM given, CHILD is placed as by PLACE: kept there on a
FIXED-LAYOUT or a VIEW, placed once elsewhere (and only while PARENT is in a window); without them
CHILD keeps its bounds. FOCUS is the focus parent a focusable CHILD enters: T (the default)
is the content focus list of PARENT's window, as by JOIN-WINDOW-FOCUS; another focus parent
is entered only while PARENT is in a window's layout tree; NIL none, neither now nor when
PARENT joins a window later.
CHILD is not registered with the renderer here: a window realizes every unrealized
element of its tree when it next renders it (alloy renderers/simple/presentations/
protocol.lisp: prepare-for-render :before for renderable)."
  (let ((placed-p (and (or x right) (or y bottom))))
    (cond ((and placed-p (typep parent 'retaining-layout))
           (retain-place parent child (list :x x :y y :w w :h h :right right :bottom bottom)))
          (t
           (attach-to-layout child parent)
           (when (and placed-p (alloy:layout-tree parent))
             (apply-place-spec parent child
                               (list :x x :y y :w w :h h :right right :bottom bottom))))))
  (cond ((eq focus t)
         (join-window-focus parent child))
        ((and focus (alloy:layout-tree parent) (typep child 'alloy:focus-element))
         (attach-to-focus child focus)))
  child)

(defun attach-to-layout (child parent)
  "Enter CHILD into PARENT, leaving a different old parent first as CAPI re-parents implicitly
(alloy layout.lisp: the enter :before for (layout-element layout) signals element-has-different-parent)."
  (let ((old (layout-attached-p child)))
    (when (and old (not (eq old parent)))
      (alloy:leave child old)))
  (alloy:enter child parent))

(defun attach-to-focus (child focus)
  "Enter CHILD into FOCUS, leaving a different old focus parent first; a no-op when already there."
  (let ((old (focus-attached-p child)))
    (when (and focus (not (eq old focus)))
      (when old (alloy:leave child old))
      (alloy:enter child focus))))

(defun detach-focus-subtree (element &optional (extra-children (constantly nil)))
  "Take ELEMENT, its container descendants and those EXTRA-CHILDREN answers out of their focus parents."
  (labels ((walk (el)
             (dolist (child (funcall extra-children el)) (walk child))
             (when (typep el 'alloy:container)
               (alloy:do-elements (child el) (walk child)))
             (let ((fparent (focus-attached-p el)))
               (when fparent (alloy:leave el fparent)))))
    (walk element)))
