(in-package :om-api)

(export '(om-view
          om-view-p
          om-make-view
          om-transparent-view
          om-scroller
          om-field-size
          om-set-field-size
          om-scroll-position
          scroll-update
          om-set-scroll-position
          om-move-scroll-position
          om-h-scroll-position
          om-v-scroll-position
          om-set-h-scroll-position
          om-set-v-scroll-position
          om-view-scrolled
          update-for-subviews-changes
          om-view-contains-point-p
          om-find-view-containing-point
          om-convert-coordinates
          om-view-origin
          om-tab-layout
          om-make-tab-layout
          om-current-view)
        :om-api)

(defparameter *default-view-size* (alloy:px-size 200 100))

(defparameter *om-transparent* (chroma:rgb 0 0 0 0))

;;; om-scroll-mixin — scroll state shared by om-view (hence om-scroller) and by the
;;; dialog-items vanilla scrolls through :scrollbars. The :scrollbars/:retain-scrollbars
;;; slots mirror om-LW/view.lisp (the defclass om-scroller); only :scrollbars decides
;;; whether a view scrolls, as vanilla turns it into CAPI's :horizontal-scroll /
;;; :vertical-scroll (om-LW/view.lisp: the om-make-view macro).

(defclass om-scroll-mixin (alld:scroll-mixin) ())

;;; om-view base class. Vanilla's om-make-view feeds :horizontal-scroll/:vertical-scroll
;;; and :scroll-width/:scroll-height to the make-instance of EVERY class
;;; (om-LW/view.lisp: the om-make-view macro), so the scroll state belongs to om-view, not to om-scroller.
;;; om-scroll-mixin FIRST, as in every other consumer (dialog-items.lisp: the defclass
;;; of om-editable-text, om-text-edit-view and om-item-list):
;;; behind om-graphic-object its scroll methods are shadowed by that class's fallbacks
;;; (om-field-size and om-h/v-scroll-position for om-graphic-object) and nothing
;;; scrolls any more.

(defclass om-view (om-scroll-mixin om-graphic-object alld:view)
  ((om-bg-color  :accessor om-view-bg-color)   ; share om-graphic-object's slot so om-set-bg-color repaints
   (border-color :initarg :border-color :initform nil :accessor om-view-border-color)
   (border-width :initarg :border-width :initform 0   :accessor om-view-border-width)
   (item-subviews :initarg :item-subviews :initform nil :accessor item-subviews)))

;;; An om-view that configured NO scroll keeps the exact pre-unification behaviour (no
;;; clip, bars, child translate or wheel); the dialog-items mixing the mixin never are.
(defmethod alld:scroll-passive-p ((view om-view))
  (not (alld:scroll-active-p view)))

(defmethod om-view-name ((self om-view))
  (vname self))

(defmethod alloy:refit ((view om-view))
  (call-next-method)
  ;; flat-realized item-view descendants live in item-subviews, not vsubviews
  (dolist (child (item-subviews view))
    (when (and (typep child 'alloy:layout-element)
               (not (member child (vsubviews view))))
      (%po-place-in-host view child))))

(defgeneric om-view-p (object)
  (:method ((object om-view)) t)
  (:method ((object t)) nil))

;;; om-make-view: constructor preserving upstream LW kwargs surface.

(defun %coerce-px-w (g)
  (etypecase g
    (ompoint     (om-point-h g))
    (alloy:point (alloy:pxx g))
    (alloy:size  (alloy:pxw g))))

(defun %coerce-px-h (g)
  (etypecase g
    (ompoint     (om-point-v g))
    (alloy:point (alloy:pxy g))
    (alloy:size  (alloy:pxh g))))

;;; om-scroller — vanilla: (defclass om-scroller (om-view) ...). A scrollable
;;; multi-child pinboard (om-view = fixed-layout/vector-container), NOT a clip-view:
;;; clip-view is a single-container and clashed with OMContainerFrame's vector-container
;;; border-color/border-width and om-scroll-mixin come from
;;; om-view — a class can never precede its own superclass in a CPL.

(defclass om-scroller (alld:scroller om-view)
  ((displayed-p :initarg :displayed-p :initform nil :accessor displayed-p))
  ;; vanilla om-LW/view.lisp (the scrollbars slot and the :default-initargs of
  ;; om-scroller) -- scrollbars default T, scroller pane white
  (:default-initargs :scrollbars t :bg-color *om-white-color*))

;;; the scroller look reads the view's style state (alloy-desktop/widgets/chrome.lisp: the
;;; define-update for (window scroller)); the OM slots keep the OM values and hand them over
(defmethod initialize-instance :after ((self om-view) &key)
  (setf (alld:view-background self) (%resolve-pattern self #'om-bg-color nil)
        (alld:view-border self) (let ((c (om-view-border-color self)))
                                  (and c (%omcolor->chroma c)))
        (alld:view-border-width self) (om-view-border-width self)))

(defmethod (setf om-bg-color) :after (color (self om-view))
  (declare (ignore color))
  (setf (alld:view-background self) (%resolve-pattern self #'om-bg-color nil)))

;;; om-transparent-view
(defclass om-transparent-view (om-view) ()
  (:default-initargs :bg-color nil))

(defun om-make-view (class &rest attributes
                     &key (position (alloy:px-point 0 0))
                          (size *default-view-size*)
                          owner subviews name font bg-color border-color
                          (border-width 0) field-size scrollbars retain-scrollbars
                     &allow-other-keys)
  ;; scrollbars/retain-scrollbars are DECLARED for surface parity with the vanilla macro
  ;; (om-LW/view.lisp: the lambda-list of the om-make-view macro) and reach
  ;; make-instance through &rest ATTRIBUTES; re-passing
  ;; them here would push an explicit NIL that beats a class :default-initargs
  (declare (ignorable scrollbars retain-scrollbars))
  ;; vanilla om-LW/view.lisp (the :vcontainer and the om-add-subviews of
  ;; om-make-view): vcontainer is set AT make-instance time so
  ;; initialize-instance :after methods can already reach (om-view-container self)
  ;; (e.g. scorePanel's init-music-patch), then the owner add realizes the view.
  (let* ((fw   (if field-size (%coerce-px-w field-size) 0))
         (fh   (if field-size (%coerce-px-h field-size) 0))
         (view (apply #'make-instance class
                      :name name
                      :bg-color bg-color
                      :border-color border-color
                      :border-width border-width
                      :vx (%pt-x position) :vy (%pt-y position)
                      :vw (%coerce-px-w size) :vh (%coerce-px-h size)
                      :vcontainer owner
                      :allow-other-keys t attributes)))
    ;; CAPI geometry initargs are live constraints, not inert constructor
    ;; decoration.  Apply them before attachment so the first Alloy placement
    ;; already uses the constrained bounds.
    (cl-user::%capi-initialize-hints view attributes)
    (when owner
      (om-add-subviews owner view))
    (when subviews
      (apply #'om-add-subviews view subviews))
    ;; om-LW/view.lisp (the om-make-view macro) -- :scroll-width/:scroll-height
    ;; reach EVERY class, so the
    ;; :field-size lands on any om-scroll-mixin, not only on a scroller
    (when (typep view 'om-scroll-mixin)
      (setf (alld:field-width view) fw (alld:field-height view) fh))
    ;; om-LW/view.lisp (the white background default of om-make-view) -- a bg-less,
    ;; non-transparent view defaults to white
    (when (and (om-view-p view)
               (null (om-bg-color view))
               (not (typep view 'om-transparent-view)))
      (om-set-bg-color view *om-white-color*))
    ;; om-LW/view.lisp (the item-view font default of om-make-view) -- item-views
    ;; take *om-default-font1* unless given :font
    (when (om-item-view-p view)
      (om-set-font view (or font *om-default-font1*)))
    view))

;;; presentation: border + background derived from slots.

(defun %om-view-pattern (renderable reader fallback)
  (let ((color (funcall reader renderable)))
    (cond ((null color) fallback)
          ((typep color 'omcolor) (c color))
          (t color))))

(presentations:define-realization (presentations:default-look-and-feel om-view)
  ((:border-outer simple:rectangle)
   (alloy:margins)
   :pattern *om-transparent*)
  ((:background simple:rectangle)
   (alloy:margins 2)
   :pattern *om-transparent*))

(presentations:define-update (presentations:default-look-and-feel om-view)
  (:border-outer
   :pattern (%om-view-pattern alloy:renderable
                              (lambda (r)
                                (or (om-view-border-color r)
                                    (om-view-bg-color     r)))
                              *om-transparent*))
  (:background
   :pattern (%om-view-pattern alloy:renderable #'om-view-bg-color *om-transparent*)))

;;; Update for subviews changes (specialized override for om-view).

(defmethod update-for-subviews-changes ((self om-view) &optional (recursive nil))
  (alloy:mark-for-render self)
  (when recursive
    (mapc (lambda (v)
            (when (om-view-p v) (update-for-subviews-changes v t)))
          (vsubviews self))))

;;; Containment / geometry helpers

(defmethod om-view-contains-point-p ((view om-graphic-object) point)
  (let* ((x    (%pt-x point))
         (y    (%pt-y point))
         (brx  (om-width view))
         (bry  (om-height view))
         (vpos (om-view-position view))
         (rx   (+ (%pt-x vpos) (om-h-scroll-position view)))
         (ry   (+ (%pt-y vpos) (om-v-scroll-position view))))
    (and (> x rx) (> y ry) (< x (+ brx rx)) (< y (+ bry ry)))))

(defun om-find-view-containing-point (view point &optional (recursive t))
  (when view
    (let ((subviews (om-subviews view)))
      (dolist (subview (reverse subviews))
        (when (om-view-contains-point-p subview point)
          (return-from om-find-view-containing-point
            (if recursive
                (om-find-view-containing-point
                 subview
                 (om-convert-coordinates point view subview))
                subview))))
      view)))

(defun om-convert-coordinates (point view1 view2)
  (cond
    ((and view1 view2)
     (om-add-points point
                    (om-subtract-points (om-view-origin view2)
                                        (om-view-origin view1))))
    (t
     (warn "om-convert-coordinates: NULL view: ~A, ~A" view1 view2)
     point)))

(defun om-view-origin (view)
  (let ((container (om-view-container view)))
    (cond
      ((null container) (om-make-point 0 0))
      (t
       (let ((position (om-view-position view))
             (container-origin (om-view-origin container)))
         (if position
             (om-subtract-points container-origin position)
             container-origin))))))

(defun om-view-absolute-position (view)
  (let ((container (om-view-container view)))
    (cond
      ((null container) (om-make-point 0 0))
      (t
       (let ((position (om-view-position view))
             (container-origin (om-view-origin container)))
         (if position
             (om-add-points container-origin position)
             container-origin))))))

(defun find-pane-with-focus (layout)
  "The descendant of LAYOUT holding keyboard focus, or NIL.
   om-LW/view.lisp (find-pane-with-focus) -- CAPI map-pane-descendant-children + pane-has-focus-p."
  (labels ((descendants (pane)
             (when (typep pane 'om-graphic-object)
               (append (om-subviews pane)
                       (when (typep pane 'om-view) (item-subviews pane)))))
           (walk (pane)
             (when (and (typep pane 'alloy:focus-element) (alloy:focus pane))
               (return-from find-pane-with-focus pane))
             (mapc #'walk (descendants pane))))
    (mapc #'walk (descendants layout))
    nil))

;;; om-scroller methods (class defined above, before om-make-view).

(defmethod om-view-p ((self om-scroller)) t)

;;; scroller-p stays T for om-scroller ONLY (om-LW/view.lisp: the scroller-p methods): its live caller
;;; is the metafile mask (icons-picts.lisp: %om-metafile-mask, vanilla
;;; om-LW/icons-picts.lisp: the scroller-p test of om-draw-picture for
;;; (view internal-picture)).
(defmethod scroller-p ((self om-scroller)) t)
(defmethod scroller-p ((self t)) nil)

;;; om-make-scroller — thin wrapper; om-scroller is now an om-view, so the unified
;;; om-make-view path realizes it and maps :field-size to the field-width/field-height slots.
(defun om-make-scroller (&key (position (alloy:px-point 0 0))
                              (size *default-view-size*)
                              owner
                              (scrollbars t)
                              field-size
                              retain-scrollbars
                              bg-color
                              border-color
                              (border-width 1)
                              (class 'om-scroller)
                              extra-initargs)
  (apply #'om-make-view class
         :position position :size size :owner owner
         :scrollbars scrollbars :retain-scrollbars retain-scrollbars
         :bg-color bg-color :border-color border-color :border-width border-width
         :field-size field-size
         extra-initargs))

;;; ---------------------------------------------------------------- scroll policy
;;; The scroll engine is alloy-desktop's (view/scroll.lisp: scroll-mixin, set-scroll,
;;; ensure-scrollbars); what stays here is OM policy on the OM classes.

;;; the scrolled area is what om-view-size answers, which for a dialog item is its
;;; slot size (dialog-items.lisp: om-view-size for om-standard-dialog-item)
(defmethod alld:scroll-view-size ((self om-scroll-mixin))
  (let ((size (om-view-size self)))
    (values (om-point-h size) (om-point-v size))))

(defmethod alld:scroll-changed ((self om-scroll-mixin) dimension operation position)
  (scroll-update self dimension operation position))

(defmethod om-field-size ((self om-scroll-mixin))
  ;; om-LW/view.lisp (om-field-size for om-scroller): a visible scroller reports the scroller's REAL range
  ;; and only an invisible one falls back to the configured field, so a configured field must never
  ;; shrink the range below what the children actually occupy.
  (multiple-value-bind (w h) (alld:field-size self)
    (om-make-point w h)))

(defmethod om-field-size ((self om-graphic-object))
  (om-view-size self))

(defmethod om-interior-size ((self om-scroller))
  ;; om-LW/view.lisp (om-interior-size for om-scroller, under #+win32): the interior
  ;; is the view minus the gutters.
  (multiple-value-bind (vw vh) (alld:scroll-viewport self)
    (om-make-point (round vw) (round vh))))

;;; ------------------------------------------- kernel-facing scroll accessors
;;; The kernel's pixels are integers (CAPI's are) and Alloy's are floats, so the
;;; boundary rounds on the way out; alld:scroll-x/scroll-y stay raw for alloy:offset.

(defmethod om-scroll-position ((self om-scroll-mixin))
  (om-make-point (round (alld:scroll-x self)) (round (alld:scroll-y self))))

(defmethod om-scroll-position ((self om-graphic-object))
  (om-make-point 0 0))

(defmethod om-h-scroll-position ((self om-scroll-mixin)) (round (alld:scroll-x self)))
(defmethod om-v-scroll-position ((self om-scroll-mixin)) (round (alld:scroll-y self)))

(defmethod om-h-scroll-position ((self om-graphic-object)) 0)
(defmethod om-v-scroll-position ((self om-graphic-object)) 0)

(defmethod om-set-scroll-position ((self t) pos)
  (declare (ignore pos))
  nil)

(defmethod om-set-scroll-position ((self om-scroll-mixin) pos)
  (if (alld:scroll-passive-p self)
      (call-next-method)
      (progn (alld:set-scroll self (%pt-x pos) (%pt-y pos))
             pos)))

(defmethod om-move-scroll-position ((self om-scroll-mixin) pos)
  (om-set-scroll-position self pos))

(defmethod om-set-h-scroll-position ((self om-scroll-mixin) pos)
  (if (alld:scroll-passive-p self)
      (call-next-method)
      (progn (alld:set-scroll self pos (alld:scroll-y self))
             pos)))

(defmethod om-set-v-scroll-position ((self om-scroll-mixin) pos)
  (if (alld:scroll-passive-p self)
      (call-next-method)
      (progn (alld:set-scroll self (alld:scroll-x self) pos)
             pos)))

(defmethod om-set-h-scroll-position ((self om-graphic-object) pos)
  (declare (ignore pos))
  t)

(defmethod om-set-v-scroll-position ((self om-graphic-object) pos)
  (declare (ignore pos))
  t)

(defmethod om-set-field-size ((self om-scroll-mixin) size)
  (setf (alld:field-width self) (%coerce-px-w size))
  ;; scroll clamped, view and bars repainted (alloy-desktop view/scroll.lisp: (setf field-height) :after for scroll-mixin)
  (setf (alld:field-height self) (%coerce-px-h size))
  t)

;;; Scroll callbacks

(defgeneric om-view-scrolled (self x y)
  (:method ((self om-scroller) x y) (declare (ignore x y)) nil)
  (:method ((self t) x y) (declare (ignore x y)) nil))

(defgeneric scroll-update (self dimension operation pos-list &rest options)
  ;; om-LW/view.lisp (scroll-update for om-scroller) is the CAPI scroll callback: the pane has ALREADY
  ;; scrolled, so it only invalidates and notifies. Calling a setter back from here
  ;; zeroes the other axis and the cross axis never scrolls.
  (:method ((self om-scroll-mixin) dimension operation pos-list &rest options)
    (declare (ignore options))
    (case dimension
      (:vertical   (setf pos-list (list 0 pos-list)))
      (:horizontal (setf pos-list (list pos-list 0)))
      (otherwise nil))
    (case operation
      (:move
       (when (and (consp pos-list) (car pos-list) (cadr pos-list))
         (om-invalidate-rectangle self
                                  (om-h-scroll-position self) (om-v-scroll-position self)
                                  (vw self) (vh self)))
       (om-view-scrolled self (car pos-list) (cadr pos-list)))
      (otherwise
       (om-view-scrolled self
                         (om-h-scroll-position self)
                         (om-v-scroll-position self))))))

(defmethod internal-remove-subview :before ((self om-graphic-object) (subview om-scroll-mixin))
  (alld:drop-scrollbars subview))

;;; om-tab-layout — native alloy:tab-view; tab headers switch the visible panel.
;;; alloy:tab-view requires a focus-element per tab: each tab carries the panel as
;;; layout-element and a focus-list of the panel's focusable children as focus-element.

(defclass om-tab-layout (alld:tab-layout om-graphic-object) ())

(defun %panel-focus-list (panel)
  "A focus-list holding PANEL's focusable vsubviews, for an alloy:tab focus-element."
  (alld:make-focus-list (and (typep panel 'om-graphic-object) (vsubviews panel))))

(defmethod om-current-view ((self om-tab-layout))
  (alld:current-page self))

(defmethod %map-shown-subviews (function (view om-tab-layout))
  "A tab layout shows its CURRENT page and no other, which is also the only page CAPI
has realized native controls for."
  (call-next-method)
  (let ((panel (om-current-view view)))
    (when panel (%map-shown-subviews function panel))))

(defun om-make-tab-layout (view-list &key (size (om-make-point 200 200))
                                          (position (om-make-point 0 0))
                                          (selection 0)
                                          tab-names
                                          owner
                                          border-color corner-radius bg-color active-color)
  (let* ((style (list :border-color (%omcolor->chroma (or border-color alld:*tab-border*))
                      :radius        (or corner-radius 0)
                      :bg            (%omcolor->chroma (or bg-color alld:*tab-face*))
                      :active-bg     (%omcolor->chroma (or active-color alld:*tab-active-face*))))
         (tabs (loop for view in view-list
                     for i from 0
                     for tab = (alld:make-tab (or (nth i tab-names)
                                                  (and (typep view 'om-view) (om-view-name view))
                                                  (format nil "Tab ~D" i))
                                              view
                                              :focus (%panel-focus-list view))
                     do (alld:store-tab-style tab style)
                     collect tab))
         (tl   (make-instance 'om-tab-layout
                              :tabs tabs
                              :vx (%pt-x position) :vy (%pt-y position)
                              :vw (%coerce-px-w size) :vh (%coerce-px-h size))))
    (when (and tabs (numberp selection) (<= 0 selection (1- (length tabs))))
      (setf (alloy:current tl) (nth selection tabs)))
    (when owner (om-add-subviews owner tl))
    tl))

(defun %retire-view-subtree-resources (root)
  "Release the per-view registry entries under ROOT before it detaches.
Tabs are not VSUBVIEWS of their layout, so the walk descends through them too."
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((walk (obj)
               (unless (or (null obj) (gethash obj seen))
                 (setf (gethash obj seen) t)
                 (when (typep obj 'om-scroll-mixin)
                   (alld:drop-scrollbars obj))
                 (when (typep obj 'om-tab-layout)
                   (alld:forget-tab-styles obj)
                   (alloy:do-elements (tab obj)
                     (walk (alloy:layout-element tab))))
                 (when (typep obj 'om-graphic-object)
                   (dolist (child (vsubviews obj)) (walk child))))))
      (walk root)))
  root)

(defmethod internal-remove-subview :before ((self om-graphic-object) (subview om-tab-layout))
  (alld:forget-tab-styles subview))
