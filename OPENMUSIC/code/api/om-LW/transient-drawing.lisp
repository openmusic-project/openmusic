



(in-package :om-api)

(export '(om-init-motion-draw) :oa)

(defun cached-display-draw  (pane x y width height)
  (declare (ignore x y width height))
  (let ((dragging-info (capi:output-pane-cached-display-user-info pane)))
    (when dragging-info
      (destructuring-bind (action x1 y1 x2 y2)
          dragging-info
        (om-with-focused-view pane (om-draw-rect x1 y1 (- x2 x1) (- y2 y1)))))))

;;; typically called in a click-handler
(defmethod om-init-motion-draw ((self om-view) position motion-draw release-action)
  (capi:start-drawing-with-cached-display 
   self 
   #'(lambda (view x y w h) 
       (declare (ignore x y width height)) 
       (let ((dragging-info (capi:output-pane-cached-display-user-info view)))
         (when dragging-info
           (destructuring-bind (action x1 y1 x2 y2)
               dragging-info
             (om-with-focused-view view 
               (funcall motion-draw view x1 y1 x2 y2))))))
   :user-info (list release-action
                    (om-point-x position) (om-point-y position)
                    (om-point-x position) (om-point-y position))))

(defmethod om-click-motion-handler :after ((self om-view) pos)
  (let ((dragging-info (capi:output-pane-cached-display-user-info self))
        (x (om-point-x pos)) (y (om-point-y pos)))
    (when dragging-info
      (destructuring-bind (release-action center-x center-y old-where-to-x old-where-to-y)
          dragging-info 
        (unless (and (= old-where-to-x x)
                     (= old-where-to-y y))
          (capi:update-drawing-with-cached-display-from-points
           self center-x center-y x y :extend 15)
          (setf (fourth dragging-info) x (fifth dragging-info) y))
        ))))

(defmethod om-click-release-handler :after ((self om-view) pos)
  (declare (ignore pos))
  (let ((dragging-info (capi:output-pane-free-cached-display self)))
    (when dragging-info
      (funcall (car dragging-info) self (om-make-point (nth 1 dragging-info) 
                                                       (nth 2 dragging-info))
               pos))))
