

(in-package :om-api)

(export '(om-start-transient-drawing
          om-stop-transient-drawing
          om-update-transient-drawing-geometry
          om-update-transient-drawing
          om-init-motion-click) :oa)

(defparameter *click-motion-view* nil)
(defparameter *click-motion-action* nil)

(defmethod om-start-transient-drawing ((self om-view) draw-fun position size &key display-mode)
  ;(print (list "start" self))
  ;(capi:output-pane-free-cached-display self)
  (capi:start-drawing-with-cached-display 
   self 
   #'(lambda (view x y w h)
       ;(print "draw") ; not after click ???
       (let ((dragging-info (capi:output-pane-cached-display-user-info self)))
         (when dragging-info
           (destructuring-bind (mode x y w h)
               dragging-info 
             (om-with-focused-view view 
               (funcall draw-fun view (om-make-point x y) (om-make-point w h)))))))
   :user-info (list display-mode
                    (om-point-x position) (om-point-y position)
                    (om-point-x size) (om-point-y size)
                    )))

(defmethod om-stop-transient-drawing ((self om-view))
  (capi:output-pane-free-cached-display self)
  (om-invalidate-view self))

(defmethod om-update-transient-drawing ((self om-view))
  (let ((dragging-info (capi:output-pane-cached-display-user-info self)))
    (when dragging-info
      (destructuring-bind (mode x y w h)
          dragging-info 
        (if mode
            (capi:update-drawing-with-cached-display-from-points self x y (+ x w) (+ y h) :extend (if (numberp mode) mode 1))
          (capi:update-drawing-with-cached-display self))
        ))))

(defmethod om-update-transient-drawing-geometry ((self om-view) &key x y w h)
  (let ((dragging-info (capi:output-pane-cached-display-user-info self)))
    (when dragging-info
      ;(print x)
      (when x (setf (nth 1 dragging-info) x))
      (when y (setf (nth 2 dragging-info) y))
      (when w (setf (nth 3 dragging-info) w))
      (when h (setf (nth 4 dragging-info) h))
      )))



;;; typically called in a click-handler
(defmethod om-init-motion-click ((self om-graphic-object) position &key motion-draw draw-pane motion-action release-action display-mode)
  ;(print (list "start" self))
  (setf *click-motion-view* self)
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
                             display-mode))
  t)

(defmethod start-transient-drawing ((self om-view) motion-draw position display-mode) 
  ;(capi:output-pane-free-cached-display self)
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

(defmethod om-click-motion-handler :around ((self om-graphic-object) position)
  ;(print (list self position *click-motion-view*))
  (if *click-motion-action*
    (let* ((view *click-motion-view*)
           (motion-info (temp-data view))
           (x (om-point-x position)) (y (om-point-y position))
           (pane view))
      (when motion-info
        (destructuring-bind (motion-action release-action x0 y0 old-x old-y draw-pane)
            motion-info 
          (unless (and (= old-x x) (= old-y y))
            (when motion-action
              (funcall motion-action view position (om-make-point old-x old-y)))
            (setf (nth 4 (temp-data view)) x (nth 5 (temp-data view)) y))
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
            ))))
    (call-next-method)))

(defmethod om-click-release-handler :after ((self om-graphic-object) pos)
  (declare (ignore pos))
  ;(print (list self *click-motion-action* *click-motion-view*))
  (when *click-motion-action* ; (equal *click-motion-view* self) 
    (let* ((view *click-motion-view*)
           (motion-info (temp-data view)))
      (when motion-info
        (destructuring-bind (motion-action release-action x0 y0 old-x old-y draw-pane)
            motion-info  
          (let ((dragging-info (capi:output-pane-free-cached-display (or draw-pane view))))
           ;; nothing more to do...
              )
          (when release-action
            (funcall release-action view (om-make-point x0 y0) (om-convert-coordinates pos self view))
            )))
      (setf *click-motion-action* nil)
      (setf (temp-data view) nil))))

