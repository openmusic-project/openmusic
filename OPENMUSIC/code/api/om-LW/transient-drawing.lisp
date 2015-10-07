

(in-package :om-api)

(export '(om-init-motion-draw) :oa)

;;; typically called in a click-handler
(defmethod om-init-motion-draw ((self om-view) position &key motion-draw motion-action release-action mode)
  (capi:start-drawing-with-cached-display 
   self 
   (when motion-draw
     #'(lambda (view x y w h) 
         (let ((dragging-info (capi:output-pane-cached-display-user-info view)))
           (when dragging-info
             (destructuring-bind (mode motion-action release-action x1 y1 x2 y2)
                 dragging-info
               (om-with-focused-view view 
                 (funcall motion-draw view (om-make-point x1 y1) (om-make-point x2 y2))))))))
   :user-info (list mode motion-action release-action
                    (om-point-x position) (om-point-y position)
                    (om-point-x position) (om-point-y position))))

(defmethod om-click-motion-handler :after ((self om-view) pos)
  (let ((dragging-info (capi:output-pane-cached-display-user-info self))
        (x (om-point-x pos)) (y (om-point-y pos)))
    (when dragging-info
      (destructuring-bind (mode motion-action release-action x0 y0 old-x old-y)
          dragging-info 
        (unless (and (= old-x x)
                     (= old-y y))
          (when motion-action
            (funcall motion-action self pos (om-make-point old-x old-y)))
          (if mode
              (capi:update-drawing-with-cached-display-from-points self x0 y0 x y :extend (if (numberp mode) mode 0))
            (capi:update-drawing-with-cached-display self))
          (setf (nth 5 dragging-info) x (nth 6 dragging-info) y))
        ))))

(defmethod om-click-release-handler :after ((self om-view) pos)
  (declare (ignore pos))
  (let ((dragging-info (capi:output-pane-free-cached-display self)))
    (when dragging-info
      (destructuring-bind (mode motion-action release-action x0 y0 old-x old-y)
          dragging-info     
        (when release-action
          (funcall release-action self (om-make-point x0 y0) pos)
          )))))
