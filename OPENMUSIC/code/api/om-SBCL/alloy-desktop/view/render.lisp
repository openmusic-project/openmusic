(in-package :alloy-desktop)

;;; The paint order of a CAPI pane: its background and border, then its own drawing, then
;;; its child panes -- all clipped to its own rect and, when it scrolls, with the content
;;; translated by the scroll offset while the chrome stays put.

(defgeneric render-contents (renderer element)
  (:documentation "The element's own drawing, painted after its shapes and before its children.")
  (:method (renderer element)
    (declare (ignore renderer element))
    nil))

(defgeneric draws-contents-p (element)
  (:documentation "True when ELEMENT owns a drawing pass of its own (see render-contents).")
  (:method (element)
    (declare (ignore element))
    nil))

(defun call-with-drawing-pass (renderer element background thunk)
  "Call THUNK with the drawing pass state bound for ELEMENT on RENDERER; BACKGROUND is the
erase pattern when none is bound. An error inside THUNK is logged as [render] ERROR and then
propagates to the caller's handlers."
  (let ((*current-renderer*    renderer)
        (*current-fg-color*    (or *current-fg-color* colors:black))
        (*current-bg-color*    (or *current-bg-color* background))
        (*pox*                 0)
        (*poy*                 0)
        (*current-view-height* (alloy:pxh (alloy:bounds element))))
    (handler-bind ((error
                     (lambda (c)
                       (format *error-output* "[render] ERROR class=~S: ~A~%"
                               (type-of element) (princ-to-string c))
                       (force-output *error-output*))))
      (funcall thunk))))

;; The presentations primary (renderer x renderable) outranks the layout one, so it
;; is what call-next-method reaches: draw the shapes here and render children directly.
(defun render-presentation-shapes (renderer element)
  "Render ELEMENT's visible presentation shapes, each under its own transforms."
  (loop for (nil . shape) across (presentations:shapes element)
        unless (presentations:hidden-p shape)
        do (simple:with-pushed-transforms (renderer)
             (setf (simple:composite-mode renderer) (presentations:composite-mode shape))
             (incf (simple:z-index renderer) (presentations:z-index shape))
             (simple:translate renderer (presentations:offset shape))
             (simple:translate renderer (presentations:pivot shape))
             (simple:rotate renderer (presentations:rotation shape))
             (simple:scale renderer (presentations:scale shape))
             (simple:translate-by renderer
                                  (- (alloy:pxx (presentations:pivot shape)))
                                  (- (alloy:pxy (presentations:pivot shape))))
             (alloy:render renderer shape))))

(defun render-translated-contents (renderer element)
  "render-contents for ELEMENT, under its scroll translate when ELEMENT scrolls."
  (if (typep element 'scroll-mixin)
      (simple:with-pushed-transforms (renderer)
        (simple:translate-by renderer (- (scroll-x element)) (scroll-y element))
        (render-contents renderer element))
      (render-contents renderer element)))

;;; A scrollable's own chrome (background, border) stays put and only its CONTENT is
;;; translated. The clip goes through constrain-visibility, whose extent is global, so it
;;; lands on the true (unscrolled) rect and is unaffected by the translate.

;;; Bars are decided and prepared before the frame's realization pass (alloy ui.lisp: render
;;; for ui), so the first overflowing frame shows them.
(defmethod alloy:prepare-for-render :before ((element scroll-mixin) (renderer alloy:renderer))
  (unless (scroll-passive-p element)
    (ensure-scrollbars element)
    (let ((set (gethash element *scroller-bars*)))
      (when set
        (when (second set) (alloy:prepare-for-render (second set) renderer))
        (when (third set)  (alloy:prepare-for-render (third set) renderer))))))

(defmethod alloy:render :around ((renderer alloy:renderer) (element scroll-mixin))
  ;; EVERY pane clips its drawing to its own rect under CAPI, whether or not it
  ;; scrolls: what its own drawing paints past the pane is cut by the pane, not by
  ;; the window.
  (alloy:constrain-visibility element renderer)
  (call-next-method))

(defmethod alloy:render ((renderer presentations:renderer) (element scroll-mixin))
  (cond ((scroll-offsets-children-p element)
         (render-presentation-shapes renderer element)
         ;; The pane's background and border cover its full bounds, but its drawing and
         ;; children stop before the scrollbar gutters.  Otherwise content remains
         ;; visible underneath a transparent track and in the bottom-right corner.
         ;; constrain-visibility updates both the GL stencil and the renderer's
         ;; logical visible-bounds.  with-pushed-transforms restores the former,
         ;; but not the latter; restore it explicitly before rendering chrome or
         ;; the gutters themselves are culled as outside the content viewport.
         (let ((old-visible-bounds (alloy:visible-bounds renderer)))
           (unwind-protect
                (simple:with-pushed-transforms (renderer)
                  (alloy:constrain-visibility (scroll-content-global-extent element) renderer)
                  ;; scroll-mixin is also mixed into widgets that own no drawing pass
                  (when (draws-contents-p element) (render-translated-contents renderer element))
                  (simple:with-pushed-transforms (renderer)
                    (simple:translate-by renderer (- (scroll-x element)) (scroll-y element))
                    (alloy:do-elements (child element)
                      (unless (scroll-chrome-p child)
                        (alloy:render renderer child)))))
             (setf (alloy:visible-bounds renderer) old-visible-bounds)))
         (alloy:do-elements (child element)
           (when (scroll-chrome-p child)
             (alloy:render renderer child))))
        ;; CAPI order: pane background, then the pane's own drawing, then child panes.
        ;; The children are rendered here rather than by call-next-method: CLOS orders
        ;; primaries by argument left to right, so (presentations:renderer renderable)
        ;; outranks (alloy:renderer layout) and would repaint the presentation shapes --
        ;; the opaque :background -- over everything the own drawing just painted.
        ;; An element without a drawing pass keeps its own call-next-method.
        (t (if (draws-contents-p element)
               (progn (render-presentation-shapes renderer element)
                      (render-translated-contents renderer element)
                      (alloy:do-elements (child element)
                        (alloy:render renderer child)))
               (call-next-method)))))

(defmethod alloy:render :around ((renderer alloy:renderer) (element item-view))
  ;; CAPI clips pane drawing to its bounds; restored by the enclosing pushed-transforms
  (alloy:constrain-visibility element renderer)
  (call-next-method)
  (render-contents renderer element))
