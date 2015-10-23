

(in-package :om-api)

(export 
 '(om-transient-drawing-view
   om-start-transient-drawing
   om-stop-transient-drawing
   om-update-transient-drawing
   om-init-motion-click) 
 :oa)

(defclass om-transient-drawing-view (om-graphic-object)
  ((animation :initform nil :accessor animation))
  (:default-initargs :destroy-callback 'transient-drawing-view-destroy-calback))

(defmethod transient-drawing-view-destroy-calback ((self om-transient-drawing-view))
  (om-stop-transient-drawing self)
  (om-destroy-callback self))

(defparameter *click-motion-view* nil)
(defparameter *click-motion-action* nil)


;;;=====================================
;;; CURSORS AND OTHER MOVING DRAWINGS
;;;=====================================
#|
(defun start-transient-drawing-fun (view draw-fun position size &key display-mode)
  (loop 
   ;;; this loop breaks when killed 
   (capi:start-drawing-with-cached-display 
    view     
    #'(lambda (view x y w h)
        (let ((user-info (capi:output-pane-cached-display-user-info view)))
          (when user-info
            (destructuring-bind (mode x y w h)
                user-info 
              (om-with-focused-view view 
                (funcall draw-fun view (om-make-point x y) (om-make-point w h)))))))
    :user-info (list display-mode
                     (om-point-x position) (om-point-y position)
                     (om-point-x size) (om-point-y size))
    :automatic-cancel nil
    :resize-automatic-cancel #'(lambda (pane)
                                 ;; will break the loop and restart
                                 (setf (capi:output-pane-cached-display-user-info pane) nil))
    )
   (print 'start)
   (loop 
    ;;; this loop breaks when user-info is NIL (caused by a resize)
    (sleep 0.2)
    (when (not (capi:output-pane-cached-display-user-info view))
      (return)))
   ))


(defmethod om-start-transient-drawing ((self om-transient-drawing-view) draw-fun position size &key display-mode)
  (om-stop-transient-drawing self)
  (setf (animation self)
        (mp:process-run-function (format nil "Animation for ~A" self) NIL
                                 'start-transient-drawing-fun 
                                 self draw-fun position size :display-mode display-mode)))


(defmethod om-stop-transient-drawing ((self om-transient-drawing-view))
  (when (animation self)
    (capi:output-pane-free-cached-display self)
    (mp:process-kill (animation self))
    (setf (animation self) nil)
    (om-invalidate-view self)))

(defun update-transient-drawing-fun (view &key x y w h)
  (let ((user-info (capi:output-pane-cached-display-user-info view)))
    (when user-info
      (when x (setf (nth 1 user-info) x))
      (when y (setf (nth 2 user-info) y))
      (when w (setf (nth 3 user-info) (+ (nth 1 user-info) w)))
      (when h (setf (nth 4 user-info) (+ (nth 2 user-info) h)))
      (if (car user-info)
          (capi:update-drawing-with-cached-display-from-points 
           view      
           (nth 1 user-info) (nth 2 user-info)
           (nth 3 user-info) (nth 4 user-info)
           :extend (if (numberp (car user-info)) (car user-info) 1))
        (capi:update-drawing-with-cached-display view))
      )))

(defmethod om-update-transient-drawing ((self om-transient-drawing-view) &key x y w h)
  (when (and (animation self) (not *click-motion-action*))
    (capi::apply-in-pane-process 
     self
     'update-transient-drawing-fun
     self :x x :y y :w w :h h)))
|#

;;;=====================================
;;; SAME USING A SIMPLE PINBOARD OBJECT
;;;=====================================

(defmethod om-draw-contents-callback ((self om-transient-drawing-view) x y w h)
  (call-next-method)
  (when (animation self)
    (capi::draw-pinboard-object self (animation self))))

(defmethod om-start-transient-drawing ((self om-transient-drawing-view) draw-fun position size &key display-mode)
  (om-stop-transient-drawing self)
  (setf (animation self)
        (make-instance 'drawn-pinboard-object
                       :display-callback #'(lambda (pane obj x y w h)
                                             (om-with-focused-view pane
                                               (funcall draw-fun pane (om-make-point x y) (om-make-point w h))))
                       :x (om-point-x position) :y (om-point-y position)
                       :visible-min-width (om-point-x size) :visible-min-height (om-point-y size)
                       ))
  (capi:manipulate-pinboard self (animation self) :add-top)
  )

(defmethod om-stop-transient-drawing ((self om-transient-drawing-view))
  (when (animation self)
    (capi:manipulate-pinboard self (animation self) :delete)
    (setf (animation self) nil)
    (om-invalidate-view self)))


(defmethod om-update-transient-drawing ((self om-transient-drawing-view) &key x y w h)
  (when (animation self)
     (capi::apply-in-pane-process 
      self
      #'(lambda ()
          (capi:with-geometry (animation self)
            (when (or x y)
              (setf (capi:pinboard-pane-position (animation self)) 
                    (values (or x capi:%x%) (or y capi:%y%))))
            (when (or w h)
              (setf (capi:pinboard-pane-size (animation self)) 
                    (values (or w capi:%width%) (or h capi:%height%))))
            )
          ;(capi::redraw-pinboard-object (animation self))
          ))
    ))



;;;=====================================
;;; CLICK-AND-DRAG DRAWING
;;;=====================================

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
    (setf (capi:output-pane-cached-display-user-info self) nil) ;;; will break the animation loop if any
    (start-transient-click-drawing (or draw-pane self) 
                             motion-draw 
                             (if draw-pane (om-convert-coordinates position self draw-pane) position) 
                             display-mode))
  t)

(defmethod start-transient-click-drawing ((self om-view) motion-draw position display-mode) 
  ;(capi:output-pane-free-cached-display self)
  (capi::apply-in-pane-process 
   self 
   'capi:start-drawing-with-cached-display 
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
              (capi::apply-in-pane-process (om-get-view view) motion-action view position (om-make-point old-x old-y)))
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
                  (capi::apply-in-pane-process (om-get-view view) 'capi:update-drawing-with-cached-display-from-points 
                                               pane x0 y0 x y 
                                               :extend (if (numberp mode) mode 0))
                (capi::apply-in-pane-process (om-get-view view) 'capi:update-drawing-with-cached-display pane))
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
           (setf (capi:output-pane-cached-display-user-info (or draw-pane view)) nil))
          (when release-action
            (capi::apply-in-pane-process (om-get-view view) release-action view (om-make-point x0 y0) (om-convert-coordinates pos self view))
            )))
      (setf *click-motion-action* nil)
      (setf (temp-data view) nil))))

