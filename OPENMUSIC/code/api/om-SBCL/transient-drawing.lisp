(in-package :om-api)

(export
 '(om-transient-drawing-view
   om-start-transient-drawing
   om-stop-transient-drawing
   om-update-transient-drawing
   om-init-motion-click)
 :oa)

;;; Transient overlay drawing on top of an om-graphic-object — used by
;;; cursor lines during playback and drag/feedback rectangles. om-LW used
;;; CAPI drawn-pinboard-object + manipulate-pinboard. Alloy has no
;;; pinboard, so the draw-fun + extent are stored in the `animation` slot
;;; and replayed by om-draw-contents :after, after the normal contents.

(defclass om-transient-drawing-view (alld:transient-drawing-view om-graphic-object) ())

(defstruct %transient-anim
  draw-fun x y w h display-mode)

(defmethod om-start-transient-drawing ((self om-graphic-object)
                                       draw-fun position size
                                       &key display-mode)
  (om-stop-transient-drawing self)
  (setf (animation self)
        (make-%transient-anim
         :draw-fun     draw-fun
         :x            (om-point-h position)
         :y            (om-point-v position)
         :w            (om-point-h size)
         :h            (om-point-v size)
         :display-mode display-mode))
  (when (typep self 'alloy:renderable)
    (alloy:mark-for-render self)))

(defmethod om-stop-transient-drawing ((self om-graphic-object))
  (when (animation self)
    (setf (animation self) nil)
    (when (typep self 'alloy:renderable)
      (alloy:mark-for-render self))))

(defmethod om-update-transient-drawing ((self om-graphic-object) &key x y w h)
  (let ((a (animation self)))
    (when a
      (when x (setf (%transient-anim-x a) x))
      (when y (setf (%transient-anim-y a) y))
      (when w (setf (%transient-anim-w a) w))
      (when h (setf (%transient-anim-h a) h))
      (when (typep self 'alloy:renderable)
        (alloy:mark-for-render self)))))

;;; Click-and-drag motion drawing — vanilla keeps (mode x1 y1 x2 y2) and calls
;;; motion-draw with (view initpos currentpos) (om-LW/transient-drawing.lisp:
;;; start-transient-click-drawing for om-view and the om-click-motion-handler :around
;;; for om-graphic-object), as a mechanism SEPARATE from the cursor animation.

(defstruct %click-drag
  draw-fun x1 y1 x2 y2)

(defvar *click-drag-anims* (make-hash-table :test 'eq :weakness :key))

(defun %start-click-drawing (pane motion-draw position)
  (setf (gethash pane *click-drag-anims*)
        (make-%click-drag :draw-fun motion-draw
                          :x1 (om-point-h position) :y1 (om-point-v position)
                          :x2 (om-point-h position) :y2 (om-point-v position)))
  (when (typep pane 'alloy:renderable) (alloy:mark-for-render pane)))

(defun %stop-click-drawing (pane)
  (when (remhash pane *click-drag-anims*)
    (when (typep pane 'alloy:renderable) (alloy:mark-for-render pane))))

(defmethod om-draw-contents :after ((self om-graphic-object))
  (let ((a (animation self)))
    (when a
      (funcall (%transient-anim-draw-fun a)
               self
               (om-make-point (%transient-anim-x a) (%transient-anim-y a))
               (om-make-point (%transient-anim-w a) (%transient-anim-h a)))))
  (let ((cd (gethash self *click-drag-anims*)))
    (when cd
      (funcall (%click-drag-draw-fun cd)
               self
               (om-make-point (%click-drag-x1 cd) (%click-drag-y1 cd))
               (om-make-point (%click-drag-x2 cd) (%click-drag-y2 cd))))))

;;; Global click-motion state replicating om-LW behaviour.

(defparameter *click-motion-view*   nil)
(defparameter *click-motion-action* nil)

(defmethod om-init-motion-click ((self om-graphic-object) position
                                 &key motion-draw draw-pane
                                      motion-action release-action display-mode)
  (declare (ignore display-mode))
  (setf *click-motion-view*   self
        *click-motion-action* t)
  (when (or motion-action release-action)
    (setf (temp-data self)
          (list motion-action release-action
                (om-point-h position) (om-point-v position)
                (om-point-h position) (om-point-v position)
                draw-pane)))
  (when motion-draw
    (%start-click-drawing
     (or draw-pane self) motion-draw
     (if draw-pane
         (om-convert-coordinates position self draw-pane)
         position)))
  t)

(defmethod om-click-motion-handler :around ((self om-graphic-object) position)
  (if *click-motion-action*
      (let* ((view *click-motion-view*)
             (info (when view (temp-data view)))
             (x    (om-point-h position))
             (y    (om-point-v position))
             (draw-pane nil))
        (when info
          (destructuring-bind (motion-action release-action x0 y0 old-x old-y pane)
              info
            (declare (ignore release-action x0 y0))
            (setf draw-pane pane)
            (unless (and (= old-x x) (= old-y y))
              (when motion-action
                (funcall motion-action view position (om-make-point old-x old-y)))
              (setf (nth 4 (temp-data view)) x
                    (nth 5 (temp-data view)) y))))
        (when view
          (let* ((pane (or draw-pane view))
                 (pp   (om-convert-coordinates position self pane))
                 (cd   (gethash pane *click-drag-anims*)))
            (when cd
              (setf (%click-drag-x2 cd) (om-point-h pp)
                    (%click-drag-y2 cd) (om-point-v pp))
              (when (typep pane 'alloy:renderable) (alloy:mark-for-render pane)))))
        t)
      (call-next-method)))

(defmethod om-click-release-handler :after ((self om-graphic-object) pos)
  (when *click-motion-action*
    (let* ((view *click-motion-view*)
           (info (when view (temp-data view))))
      (setf *click-motion-action* nil)
      (cond (info
             (destructuring-bind (motion-action release-action x0 y0 old-x old-y draw-pane)
                 info
               (declare (ignore motion-action old-x old-y))
               (%stop-click-drawing (or draw-pane view))
               (when release-action
                 (funcall release-action view
                          (om-make-point x0 y0)
                          (om-convert-coordinates pos self view)))))
            (view
             (%stop-click-drawing view)))
      (when view
        (setf (temp-data view) nil)))))
