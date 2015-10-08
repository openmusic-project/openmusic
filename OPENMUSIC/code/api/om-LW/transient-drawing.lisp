

(in-package :om-api)

(export '(om-init-motion-click) :oa)

(defparameter *click-motion-view* nil)
(defparameter *click-motion-action* nil)

;;; typically called in a click-handler
(defmethod om-init-motion-click ((self om-graphic-object) position &key motion-draw draw-pane motion-action release-action display-mode)
  (setf *click-motion-view* self) ;; not used
  (setf *click-motion-action* t)
  (when (or motion-action release-action)
    (setf (temp-data self) 
          (list motion-action release-action
                (om-point-x position) (om-point-y position)
                (om-point-x position) (om-point-y position)
                draw-pane)))
  (when motion-draw
    (start-transient-drawing (or draw-pane self) 
                             motion-draw 
                             (if draw-pane (om-convert-coordinates position self draw-pane) position) 
                             display-mode)))

(defmethod start-transient-drawing ((self om-view) motion-draw position display-mode)
  (capi:start-drawing-with-cached-display 
   self 
   #'(lambda (view x y w h) 
       (let ((dragging-info (capi:output-pane-cached-display-user-info view)))
         (when dragging-info
           (destructuring-bind (mode x1 y1 x2 y2)
               dragging-info
             (om-with-focused-view view 
               (funcall motion-draw view (om-make-point x1 y1) (om-make-point x2 y2)))))))
   :user-info (list display-mode
                    (om-point-x position) (om-point-y position)
                    (om-point-x position) (om-point-y position))))

(defmethod om-click-motion-handler :after ((self om-graphic-object) position)
  (when (equal *click-motion-view* self)
    (let ((motion-info (temp-data self))
          (x (om-point-x position)) (y (om-point-y position))
          (pane self))
      (when motion-info
        (destructuring-bind (motion-action release-action x0 y0 old-x old-y draw-pane)
            motion-info 
          (unless (and (= old-x x) (= old-y y))
            (when motion-action
              (funcall motion-action self position (om-make-point old-x old-y)))
            (setf (nth 4 (temp-data self)) x (nth 5 (temp-data self)) y))
          (when draw-pane 
            (let ((pp (om-convert-coordinates position self draw-pane)))
              (setf pane draw-pane
                    x (om-point-x pp)
                    y (om-point-y pp))))
          ))
      (let ((dragging-info (capi:output-pane-cached-display-user-info pane)))
        (when dragging-info
          (destructuring-bind (mode x0 y0 old-x old-y)
              dragging-info 
            (unless (and (= old-x x) (= old-y y))
              (if mode
                  (capi:update-drawing-with-cached-display-from-points pane x0 y0 x y :extend (if (numberp mode) mode 0))
                (capi:update-drawing-with-cached-display pane))
              (setf (nth 3 dragging-info) x (nth 4 dragging-info) y))
            ))))))

(defmethod om-click-release-handler :after ((self om-graphic-object) pos)
  (declare (ignore pos))
  (when (equal *click-motion-view* self) 
    (let* ((view self)
           (motion-info (temp-data view)))
      (when motion-info
        (destructuring-bind (motion-action release-action x0 y0 old-x old-y draw-pane)
            motion-info  
          (let ((dragging-info (capi:output-pane-free-cached-display (or draw-pane view))))
            ;; nothing more to do...
            )
          (when release-action
            (funcall release-action view (om-make-point x0 y0) pos)
            )))
      (setf *click-motion-action* nil)
      (setf (temp-data view) nil))))

