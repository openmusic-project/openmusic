(in-package :om-api)

(export '(om-draw-contents
          om-component-border
          om-highlight-view
          om-invalidate-view
          om-invalidate-rectangle
          om-invalidate-corners
          om-with-delayed-redraw
          om-draw-view-outline
          om-redraw-view)
        :om-api)

;;; Draw callback — kernel overrides per view class to emit shapes.

(defgeneric om-draw-contents (self)
  (:method ((self t)) nil))

(defmethod om-draw-contents ((self om-graphic-object)) t)

(defgeneric om-component-border (self)
  (:method ((self t)) nil))

(defmethod om-component-border ((self om-graphic-object)) nil)

;;; Render bridge — Alloy renders a graphic-object by binding pen state and
;;; invoking om-draw-contents; kernel draws via om-draw-rect/-line/etc.

(defun %render-om-draw-contents (renderer view bg-color)
  (let ((*current-font* (or (om-get-font view) *current-font*))
        (*current-view* view)
        ;; an output-pane is itself a graphics port (capi-gp-ref-77.htm:44); binding it
        ;; here gives om-get-current-port a port for the whole pass
        (*curstream*    view))
    (flet ((draw ()
             (om-draw-contents view)
             (when (highlight view)
               (om-with-focused-view view
                 (om-draw-hilite-rect 0 0 (om-width view) (om-height view))))))
      ;; runs once per pane per frame: the thunk must not cons
      (declare (dynamic-extent #'draw))
      (alld:call-with-drawing-pass renderer view bg-color #'draw))))

;;; The paint order, clip and scroll translate live in alloy-desktop view/render.lisp;
;;; the bridge supplies the pane's own drawing.

(defmethod alld:draws-contents-p ((view om-view)) t)

(defmethod alld:render-contents (renderer (view om-view))
  (%render-om-draw-contents renderer view (om-view-bg-color view)))

(defmethod alld:render-contents (renderer (view om-item-view))
  (%render-om-draw-contents renderer view (om-get-bg-color view)))

;;; Highlight toggle — slot mutator with redraw side-effect.

(defgeneric om-highlight-view (self t-or-nil))

(defmethod om-highlight-view ((self om-graphic-object) t-or-nil)
  (unless (equal (highlight self) t-or-nil)
    (setf (highlight self) t-or-nil)
    t))

(defmethod om-highlight-view ((self om-view) t-or-nil)
  (when (call-next-method)
    (om-invalidate-view self)))

(defmethod om-highlight-view ((self om-item-view) t-or-nil)
  (when (call-next-method)
    (om-invalidate-view self)))

;;; Invalidate — Alloy collapses CAPI invalidate-region/rectangle into a whole-element
;;; repaint on the next frame (alloy-desktop view/view.lisp: invalidate).

(defgeneric om-invalidate-view (self &optional erase))

(defmethod om-invalidate-view ((self om-graphic-object) &optional erase)
  (declare (ignore erase))
  (alld:invalidate self))

(defmethod om-invalidate-view ((self om-item-view) &optional erase)
  (declare (ignore erase))
  ;; vanilla invalidates the CONTAINER itself (om-LW/draw.lisp: om-invalidate-view for
  ;; om-item-view, om-get-view of a pinboard object is its parent); an item added to a
  ;; window has alloy's root layout there
  (alld:invalidate (item-container self))
  self)

(defgeneric om-invalidate-rectangle (self x y w h))

(defmethod om-invalidate-rectangle ((self om-graphic-object) x y w h)
  (declare (ignore x y w h))
  (om-invalidate-view self))

(defmethod om-invalidate-rectangle ((self om-item-view) x y w h)
  (declare (ignore x y w h))
  (let ((container (item-container self)))
    (when container (om-invalidate-view container))))

(defgeneric om-invalidate-corners (self topleft bottomright))

(defmethod om-invalidate-corners ((self om-graphic-object) topleft bottomright)
  (declare (ignore topleft bottomright))
  (om-invalidate-view self))

(defmethod om-invalidate-corners ((self om-item-view) topleft bottomright)
  (declare (ignore topleft bottomright))
  (let ((container (item-container self)))
    (when container (om-invalidate-view container))))

;;; Delayed redraw — Alloy renders once per frame; bracket body and
;;; mark-for-render at the end so coalesced repaints behave like CAPI
;;; capi:with-atomic-redisplay.

(defmacro om-with-delayed-redraw (view &body body)
  (let ((v (gensym "VIEW")))
    `(let ((,v ,view))
       (multiple-value-prog1 (progn ,@body)
         (when (and ,v (typep ,v 'alloy:renderable))
           (alloy:mark-for-render ,v))))))

;;; Outline — single-pixel border around the view; uses graphics primitives.

(defgeneric om-draw-view-outline (self &optional pensize))

(defmethod om-draw-view-outline ((self om-graphic-object) &optional (pensize 1))
  (om-with-focused-view self
    (let ((*current-pen-size* pensize))
      (om-draw-rect 0 0 (- (om-width self) 1) (- (om-height self) 1)))))

;;; Redraw — request a fresh render of the entire renderable.

(defgeneric om-redraw-view (self))

(defmethod om-redraw-view ((self om-graphic-object))
  (om-invalidate-view self))

(defmethod om-redraw-view ((self om-scroller))
  (om-invalidate-view self))

(defmethod om-redraw-view ((self om-item-view))
  (om-invalidate-view self))
