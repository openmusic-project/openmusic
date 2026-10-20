(in-package :om-api)

(export '(om-item-view
          om-internal-view)
        :om-api)

;;; Lightweight visual primitive (no own subview pinboard); kernel-spec class.

(defclass om-item-view (om-graphic-object alld:item-view)
  ((item-container :initarg :item-container :initform nil :accessor item-container)
   (item-x         :initarg :item-x         :initform 0   :accessor item-x)
   (item-y         :initarg :item-y         :initform 0   :accessor item-y)))

(defmethod initialize-instance :after ((self om-item-view) &rest args)
  (declare (ignore args))
  (om-create-callback self))

(defgeneric om-item-view-p (object)
  (:method ((self om-item-view)) t)
  (:method ((self t))            nil))

#+(or linux darwin)
(defclass om-internal-view (om-transparent-view) ())

#-(or linux darwin)
(defclass om-internal-view (om-item-view) ())

;;; Subview protocol — propagate up to item-container.

(defmethod update-for-subviews-changes ((self om-item-view) &optional (recursive nil))
  (declare (ignore recursive))
  (when (and (item-container self) (initialized-p (item-container self)))
    (update-for-subviews-changes (item-container self) nil)))

(defmethod internal-add-subview ((self om-view) (subview om-item-view))
  (call-next-method)
  (setf (item-container subview) self)
  (setf (item-subviews self) (append (item-subviews self) (list subview)))
  (update-po-position subview)
  (dolist (sv (vsubviews subview))
    (po-add-subview subview sv)))

(defmethod internal-add-subview ((self om-look-window) (subview om-item-view))
  (call-next-method)
  (let ((root (om-window-root self)))
    (setf (item-container subview) root)
    (when (typep root 'om-view)
      (setf (item-subviews root) (append (item-subviews root) (list subview))))
    (dolist (sv (vsubviews subview))
      (po-add-subview subview sv))))

(defmethod internal-add-subview ((self om-item-view) (subview om-graphic-object))
  (call-next-method)
  (po-add-subview self subview))

(defmethod internal-remove-subview ((self om-view) (subview om-item-view))
  (dolist (sv (vsubviews subview))
    (po-remove-subview subview sv))
  (setf (item-container subview) nil)
  (setf (item-subviews self) (remove subview (item-subviews self)))
  (call-next-method))

(defmethod internal-remove-subview ((self om-look-window) (subview om-item-view))
  (let ((root (om-window-root self)))
    (dolist (sv (vsubviews subview))
      (po-remove-subview subview sv))
    (setf (item-container subview) nil)
    (when (typep root 'om-view)
      (setf (item-subviews root) (remove subview (item-subviews root))))
    (call-next-method)))

(defmethod internal-remove-subview ((self om-item-view) (subview om-graphic-object))
  (po-remove-subview self subview)
  (call-next-method))

;;; Pinboard-object propagation — upstream CAPI keeps a flat list of item-subviews
;;; on the topmost owner and draws them there. In Alloy the flat model is realized
;;; by entering each descendant into the HOST view's layout at absolute coordinates.

(defun %po-abs-xy (child host)
  "Absolute OM x/y of CHILD in HOST coords via the vx/vy slot chain (never reads bounds)."
  (let ((x (vx child)) (y (vy child)) (v (vcontainer child)))
    (loop while (and v (not (eq v host)) (typep v 'om-graphic-object))
          do (incf x (vx v))
             (incf y (vy v))
             (setf v (vcontainer v)))
    (values x y)))

(defun %po-place-in-host (host child)
  "Set CHILD's bounds in HOST's layout from its absolute OM position (Y-down flipped)."
  (multiple-value-bind (ax ay) (%po-abs-xy child host)
    (alloy:with-unit-parent host
      (alloy:update child host
                    :x (round ax)
                    :y (round (- (alloy:pxh (alloy:bounds host)) ay (vh child)))
                    :w (round (vw child)) :h (round (vh child))))))

(defun %po-realize-in-host (host child)
  "Enter CHILD flat into HOST's layout (CAPI pinboard semantics) and place it."
  (when (and (typep host 'alloy:layout)
             (typep child 'alloy:layout-element))
    (when (alld:enter-flat host child (let ((tree (alloy:layout-tree host)))
                                        (and tree (om-window-focus (alloy:ui tree)))))
      (setf (initialized-p child) t))
    (%po-place-in-host host child)))

(defmethod po-add-subview ((self om-item-view) (subview om-graphic-object))
  (let ((host (item-container self)))
    (when (typep subview 'om-item-view)
      (setf (item-container subview) host))
    (when host
      (setf (item-subviews host)
            (append (remove subview (item-subviews host)) (list subview)))
      (when (typep subview 'om-item-view)
        (update-po-position subview))
      (%po-realize-in-host host subview)))
  (when (typep subview 'om-item-view)
    (dolist (sv (vsubviews subview))
      (po-add-subview subview sv))))

(defmethod po-remove-subview ((self om-item-view) (subview om-graphic-object))
  (when (typep subview 'om-item-view)
    (dolist (sv (vsubviews subview))
      (po-remove-subview subview sv)))
  (let ((host (item-container self)))
    (when host
      (setf (item-subviews host) (remove subview (item-subviews host)))
      (alld:leave-flat host subview)))
  (when (typep subview 'om-item-view)
    (setf (item-container subview) nil)))

;;; Defaults — upstream allows non-item-view arguments to silently no-op.

(defmethod po-add-subview    ((self om-item-view) (subview t)) nil)
(defmethod po-remove-subview ((self om-item-view) (subview t)) nil)
(defmethod item-container    ((self t)) nil)

(defmethod om-subviews ((self om-item-view))
  (vsubviews self))

(defmethod om-create-callback ((self om-item-view)) nil)

(defmethod om-get-view ((self om-item-view))
  (let ((tree (alloy:layout-tree self)))
    (or (and tree (alloy:ui tree))
        (item-container self))))

(defgeneric om-get-real-view (self)
  (:documentation "Interaction target for SELF — the item-view itself for hit-tests, or its host otherwise."))

(defmethod om-get-real-view ((self om-item-view)) self)
(defmethod om-get-real-view ((self t)) (om-get-view self))

(defun update-po-position (self)
  "Sync ITEM-X/ITEM-Y (absolute coords on ITEM-CONTAINER) for kernel consumers
\(e.g. scorepatch update-box-po-position), and re-place flat-realized descendants.
Recurses into vsubviews."
  (when (and (vcontainer self) (item-container self))
    (let ((abs-pos (if (typep (item-container self) 'om-graphic-object)
                       (om-convert-coordinates (om-view-position self)
                                               (vcontainer self)
                                               (item-container self))
                       (om-view-position self))))
      (setf (item-x self) (om-point-h abs-pos)
            (item-y self) (om-point-v abs-pos)))
    (let ((host (item-container self)))
      (when (and (typep host 'alloy:layout) (alld:layout-attached-p self))
        (%po-place-in-host host self)))
    (dolist (sv (vsubviews self))
      (if (typep sv 'om-item-view)
          (update-po-position sv)
          (let ((host (item-container self)))
            (when (and (typep host 'alloy:layout) (alld:layout-attached-p sv))
              (%po-place-in-host host sv)))))))

(defmethod om-set-view-position ((self om-item-view) pos-point)
  (setf (vx self) (%pt-x pos-point)
        (vy self) (%pt-y pos-point))
  (call-next-method)
  (update-po-position self)
  pos-point)

(defmethod om-view-position ((self om-item-view))
  (om-make-point (vx self) (vy self)))

(defmethod om-set-view-size ((self om-item-view) size-point)
  (setf (vw self) (%pt-x size-point)
        (vh self) (%pt-y size-point))
  (call-next-method)
  size-point)

(defmethod om-view-size ((self om-item-view))
  (om-make-point (vw self) (vh self)))
