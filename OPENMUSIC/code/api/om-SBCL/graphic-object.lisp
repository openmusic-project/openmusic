(in-package :om-api)

;; the CAPI hint table (lw-compat.lisp: %capi-constrain-element-size) bounds every pane resize
(setf alld:*size-constraint* #'cl-user::%capi-constrain-element-size)

(export '(omg-defclass
          om-graphic-object
          om-width
          om-height
          om-get-help-spec
          om-view-size
          om-view-position
          om-set-view-size
          om-set-view-position
          om-change-view-position
          om-set-bg-color
          om-get-bg-color
          om-set-fg-color
          om-get-fg-color
          om-get-font
          om-set-font
          om-get-view-name
          om-view-window
          om-view-container
          om-subviews
          om-add-subviews
          om-remove-subviews
          om-with-delayed-update
          om-view-set-help
          om-view-cursor
          om-set-view-cursor
          om-interior-size
          om-create-callback
          om-destroy-callback
          om-draw-contents-callback
          om-resize-callback
          om-clic-callback
          om-clic-release-callback
          om-motion-callback
          om-clic-motion-callback
          om-double-clic-callback
          om-char-callback
          om-char-spec-callback
          om-context-menu-callback)
        :om-api)

(defmacro omg-defclass (name superclass slots &rest class-options)
  `(defclass ,name ,superclass ,slots ,@class-options))

(defclass om-graphic-object ()
  ((vcontainer    :initarg :vcontainer    :initform nil   :accessor vcontainer)
   (vsubviews     :initarg :vsubviews     :initform nil   :accessor vsubviews)
   (locked        :initarg :locked        :initform nil   :accessor locked)
   (vx            :initarg :vx            :initform 0     :accessor vx)
   (vy            :initarg :vy            :initform 0     :accessor vy)
   (vw            :initarg :vw            :initform 32    :accessor vw)
   (vh            :initarg :vh            :initform 32    :accessor vh)
   (help-spec     :initarg :help-spec     :initform nil   :accessor help-spec)
   (initialized-p                         :initform nil   :accessor initialized-p)
   (highlight                             :initform nil   :accessor highlight)
   (temp-data                             :initform nil   :accessor temp-data)
   (animation                             :initform nil   :accessor animation)
   (images        :initarg :images        :initform nil   :accessor images)
   (vname         :initarg :name          :initform nil   :accessor vname)
   (om-bg-color   :initarg :bg-color      :initform nil   :accessor om-bg-color)
   (om-fg-color   :initarg :fg-color      :initform nil   :accessor om-fg-color)
   (om-font       :initarg :font          :initform nil   :accessor om-font)
   (om-cursor     :initarg :cursor        :initform nil   :accessor om-cursor)))

(defmethod om-bg-color ((self t)) nil)
(defmethod om-fg-color ((self t)) nil)
(defmethod om-font     ((self t)) nil)
(defmethod om-cursor   ((self t)) nil)
(defmethod help-spec   ((self t)) nil)

(defgeneric om-set-bg-color    (self color))
(defgeneric om-get-bg-color    (self))
(defgeneric om-set-fg-color    (self color))
(defgeneric om-get-fg-color    (self))
(defgeneric om-set-font        (self font))
(defgeneric om-get-font        (self))
(defgeneric om-view-cursor     (self))
(defgeneric om-set-view-cursor (self cursor))
(defgeneric om-set-view-position (self pt))
(defgeneric om-view-position     (self))
(defgeneric om-set-view-size     (self pt))
(defgeneric om-view-size         (self))
(defgeneric om-change-view-position (self pt))
(defgeneric om-width             (self))
(defgeneric om-height            (self))
(defgeneric om-view-window       (self))
(defgeneric om-view-container    (self))
(defgeneric om-subviews          (self))
(defgeneric om-view-set-help     (self text))
;; internal (not exported): vanilla has no such symbol; only the fork's
;; om-text-edit-window uses it to flag an edited buffer.
(defgeneric om-mark-modified     (window))
(defgeneric om-get-view-name     (self))
(defgeneric om-interior-size     (self))
(defgeneric om-get-help-spec     (self))
(defgeneric om-show-tooltip (self &optional remove short))
(defgeneric om-hide-tooltip (self))

(defmethod om-mark-modified ((window t)) nil)
(defmethod om-get-help-spec ((self om-graphic-object)) (help-spec self))

(defmethod om-set-bg-color ((self om-graphic-object) color)
  (setf (om-bg-color self) color)
  (when (typep self 'alloy:renderable)
    (alloy:mark-for-render self))
  color)

(defmethod om-get-bg-color ((self om-graphic-object))
  ;; vanilla returns the live pane background: never nil for a realized view
  (or (om-bg-color self)
      (let ((container (om-view-container self)))
        (and container (om-get-bg-color container)))
      *om-window-def-color*))

(defmethod om-set-fg-color ((self om-graphic-object) color)
  (setf (om-fg-color self) color)
  (when (typep self 'alloy:renderable)
    (alloy:mark-for-render self))
  color)

(defmethod om-get-fg-color ((self om-graphic-object))
  ;; vanilla returns the live pane foreground (default black), never nil
  (or (om-fg-color self) *om-black-color*))

(defmethod om-set-font ((self om-graphic-object) font)
  (setf (om-font self) font)
  (when (typep self 'alloy:renderable)
    (alloy:mark-for-render self))
  font)

(defmethod om-get-font ((self om-graphic-object))
  (om-font self))

(defmethod om-view-set-help ((self om-graphic-object) (text string))
  (setf (help-spec self) text)
  text)

(defun %pt-x (pt)
  "Extract X coord from OMPOINT (kernel) or alloy:point (internal callers)."
  (typecase pt
    (ompoint     (om-point-h pt))
    (alloy:point (alloy:pxx pt))
    (otherwise   (error "The value ~A is not of type ompoint or alloy:point" pt))))

(defun %pt-y (pt)
  "Extract Y coord from OMPOINT (kernel) or alloy:point (internal callers)."
  (typecase pt
    (ompoint     (om-point-v pt))
    (alloy:point (alloy:pxy pt))
    (otherwise   (error "The value ~A is not of type ompoint or alloy:point" pt))))

(defmethod om-set-view-position ((self alloy:layout-element) pt)
  (let ((kx (%pt-x pt))
        (ky (%pt-y pt)))
    ;; the kernel reads and writes vx/vy, so they follow the geometry the library retains (alloy-desktop view/placement.lisp: set-element-position)
    (when (typep self 'om-graphic-object)
      (setf (vx self) kx (vy self) ky))
    (alld:set-element-position self kx ky))
  pt)

(defmethod om-view-position ((self alloy:layout-element))
  (multiple-value-bind (x y) (alld:element-position self)
    (om-make-point x y)))

(defmethod om-set-view-size ((self alloy:layout-element) pt)
  (multiple-value-bind (w h)
      (alld:constrain-size self
                           (or (%pt-x pt) (alloy:pxw (alloy:bounds self)))
                           (or (%pt-y pt) (alloy:pxh (alloy:bounds self))))
    ;; the kernel reads and writes vw/vh, so they follow the geometry the library retains (alloy-desktop view/placement.lisp: set-element-size)
    (when (typep self 'om-graphic-object)
      (setf (vw self) w (vh self) h))
    (alld:set-element-size self w h))
  pt)

(defmethod om-view-size ((self alloy:layout-element))
  (multiple-value-bind (w h) (alld:element-size self)
    (om-make-point w h)))

(defmethod om-view-size ((self om-graphic-object))
  ;; vanilla (om-LW/view.lisp: om-view-size for om-graphic-object) refreshes vw/vh
  ;; from the live geometry whenever the
  ;; interface is visible, and answers from the slots. This bridge does not write
  ;; that refresh back -- vw/vh mirror the geometry the program gave, which the
  ;; parent retains and re-applies (alloy-desktop view/placement.lisp: place-element) --
  ;; so it answers with the live geometry itself, and falls back to the slots
  ;; while no geometry exists yet: the kernel reads om-width inside
  ;; initialize-instance (kernel graphics/default-prefs.lisp: the
  ;; initialize-instance :after for loadlibs-view), when alloy:bounds is still 0x0.
  (let ((bounds (and (typep self 'alloy:layout-element) (alloy:bounds self))))
    (if (and bounds (or (plusp (alloy:pxw bounds)) (plusp (alloy:pxh bounds))))
        (call-next-method)
        (om-make-point (vw self) (vh self)))))

(defmethod om-change-view-position ((self alloy:layout-element) delta)
  (let ((pos (om-view-position self)))
    (om-set-view-position self
                          (om-make-point (+ (%pt-x pos) (%pt-x delta))
                                         (+ (%pt-y pos) (%pt-y delta))))))

(defmethod om-width  ((self alloy:layout-element))
  (om-point-h (om-view-size self)))

(defmethod om-height ((self alloy:layout-element))
  (om-point-v (om-view-size self)))

(defmethod om-interior-size ((self alloy:layout-element))
  (om-view-size self))

(defun rec-top-level (obj)
  (if (om-view-container obj) (rec-top-level (om-view-container obj)) obj))

(defmethod om-view-window ((self alloy:layout-element))
  (or (let ((tree (alloy:layout-tree self)))
        (when tree (alloy:ui tree)))
      (when (typep self 'om-graphic-object)
        (rec-top-level self))))

(defmethod om-view-window ((self om-graphic-object))
  (or (when (typep self 'alloy:layout-element)
        (let ((tree (alloy:layout-tree self)))
          (when tree (alloy:ui tree))))
      (rec-top-level self)))

;;; for windows, skip default window layout
(defmethod container-skip-layout ((self t)) self)

(defmethod om-view-container ((self om-graphic-object))
  (container-skip-layout (vcontainer self)))

(defmethod om-subviews ((self om-graphic-object))
  (vsubviews self))

(defvar *default-printer-port* nil)

(defgeneric om-get-view (self)
  (:documentation "Drawable host for SELF — the UI (window) that owns its renderer."))

(defmethod om-get-view ((self t)) nil)

(defmethod om-get-view ((self om-graphic-object))
  (or *default-printer-port* self))

(defgeneric %dialog-item-extra-subviews (di)
  (:documentation "Satellite renderables placed alongside DI when it enters a window (e.g. pop-up chevron); not part of om-subviews."))
(defmethod %dialog-item-extra-subviews ((di t)) nil)

(defmethod internal-add-subview ((self om-graphic-object) (subview om-graphic-object))
  (setf (vcontainer subview) self)
  (setf (vsubviews self) (append (vsubviews self) (list subview)))
  ;; only real containers can alloy:enter; item-view parents realize flat via po-add-subview
  (when (and (typep self 'alloy:layout) (typep subview 'alloy:layout-element))
    ;; the layout and the window's content focus list; a value the kernel keeps across
    ;; editor open/close arrives still attached to the destroyed frame (kernel
    ;; dialog-item-boxes.lisp: make-frame-from-callobj for OMDIebox), which add-element leaves
    (alld:add-element self subview)
    ;; realization point: vanilla's om-create-callback sets initialized-p here
    (setf (initialized-p subview) t)
    ;; CAPI places at add time from default-x/y/width/height
    ;; (om-LW/dialog-items.lisp: om-make-dialog-item); the parent retains this
    ;; geometry and re-applies it over its live height on every refit, so an
    ;; off-tree parent (e.g. a kernel pref pane) places SUBVIEW once it is in a window
    (alld:place-element self subview (vx subview) (vy subview) (vw subview) (vh subview))))

(defmethod internal-remove-subview ((self om-graphic-object) (subview om-graphic-object))
  ;; symmetric with internal-add-subview, which enters every descendant into the
  ;; WINDOW's focus list: leaving only the layout strands them there, still taking keys
  (alld:detach-focus-subtree subview (lambda (el) (and (typep el 'om-graphic-object) (vsubviews el))))
  (when (and (typep self 'alloy:layout) (typep subview 'alloy:layout-element)
             (alld:layout-attached-p subview))
    (alloy:leave subview self))
  (setf (vcontainer subview) nil)
  (setf (vsubviews self) (remove subview (vsubviews self))))

(defun maybe-call-update (graphic-obj)
  (when (and (initialized-p graphic-obj) (not (locked graphic-obj)))
    (update-for-subviews-changes graphic-obj t)))

(defgeneric update-for-subviews-changes (self &optional recursive))

(defmethod update-for-subviews-changes ((self om-graphic-object) &optional (recursive nil))
  (declare (ignore recursive))
  nil)

(defmethod om-add-subviews ((self om-graphic-object) &rest subviews)
  ;; CAPI treats a repeated description of a pane in the same parent as a
  ;; restack-to-top operation.  Alloy signals ELEMENT-ALREADY-CONTAINED instead.
  ;; Several original OM dialogs rely on the CAPI behaviour (notably the EDO
  ;; preference dialog lists TITLE1 twice), so enforce the invariant at the
  ;; public subview protocol before any parent-specific INTERNAL-ADD method can
  ;; attempt a second ALLOY:ENTER.
  (loop for item in subviews
        when item do
          (if (and (eq (vcontainer item) self)
                   (member item (vsubviews self) :test #'eq))
              (capi::manipulate-pinboard self item :add-top)
              (internal-add-subview self item)))
  (maybe-call-update self)
  self)

(defmethod om-remove-subviews ((self om-graphic-object) &rest subviews)
  (loop for item in subviews when item do (internal-remove-subview self item))
  (maybe-call-update self)
  self)

(defmethod om-resize-callback ((self om-graphic-object) x y w h)
  (setf (vx self) x (vy self) y (vw self) w (vh self) h))

(defmethod om-get-view-name ((self om-graphic-object))
  (vname self))

(defmethod capi::capi-object-name ((self om-graphic-object))
  (om-get-view-name self))

(defmethod (setf capi::capi-object-name) (name (self om-graphic-object))
  (setf (vname self) name))

;;; A capi:element IS a capi-object (capi-capi-ref-26.htm:46), so it answers the
;;; PLIST accessor too; om-graphic-object has no such slot, hence the side table.

(defmethod capi::capi-object-plist ((self om-graphic-object))
  (gethash self cl-user::*capi-object-plists*))

(defmethod (setf capi::capi-object-plist) (plist (self om-graphic-object))
  (setf (gethash self cl-user::*capi-object-plists*) plist))

(defmethod slot-missing (class (self om-graphic-object)
                         (slot-name (eql 'capi::name))
                         (operation (eql 'slot-value)) &optional new-value)
  (declare (ignore class new-value))
  (vname self))

(defmethod slot-missing (class (self om-graphic-object)
                         (slot-name (eql 'capi::name))
                         (operation (eql 'setf)) &optional new-value)
  (declare (ignore class))
  (setf (vname self) new-value))

(defmethod slot-missing (class (self om-graphic-object)
                         (slot-name (eql 'capi::name))
                         (operation (eql 'slot-boundp)) &optional new-value)
  (declare (ignore class self new-value))
  t)

;;; A capi:element carries an INTERFACE slot naming the capi:interface that
;;; contains it; the kernel reads it in om-get-interface (kernel editorwindow.lisp: om-get-interface for editorview).

(defmethod slot-missing (class (self om-graphic-object)
                         (slot-name (eql 'capi::interface))
                         (operation (eql 'slot-value)) &optional new-value)
  (declare (ignore class new-value))
  (om-view-window self))

(defmethod slot-missing (class (self om-graphic-object)
                         (slot-name (eql 'capi::interface))
                         (operation (eql 'slot-boundp)) &optional new-value)
  (declare (ignore class self new-value))
  t)

(defmethod om-view-cursor ((self om-graphic-object))
  (om-cursor self))

(defmethod om-set-view-cursor ((self om-graphic-object) cursor)
  (setf (om-cursor self) cursor)
  ;; om-view-window may return a detached top container (vanilla rec-top-level)
  (let ((window (om-view-window self)))
    (when (typep window 'windowing:window)
      (setf (alloy:cursor window) (or cursor :default))))
  cursor)

(defmacro om-with-delayed-update (view &body body)
  (let ((v (gensym "VIEW")))
    `(let ((,v ,view))
       (setf (locked ,v) t)
       (unwind-protect (progn ,@body)
         (setf (locked ,v) nil)
         (maybe-call-update ,v)))))

;;; Empty callback methods (upstream signatures preserved for kernel callsites)

(defmethod om-create-callback         ((self t)) nil)
(defmethod om-destroy-callback        ((self t)) nil)
(defmethod om-draw-contents-callback  ((self t) x y w h)
  (declare (ignore x y w h))
  nil)
(defmethod om-resize-callback         ((self t) x y w h)
  (declare (ignore x y w h))
  nil)
(defmethod om-clic-callback           ((self t) x y mods)
  (declare (ignore x y mods))
  nil)
(defmethod om-clic-release-callback   ((self t) x y mods)
  (declare (ignore x y mods))
  nil)
(defmethod om-motion-callback         ((self t) x y mods)
  (declare (ignore x y mods))
  t)
(defmethod om-clic-motion-callback    ((self t) x y mods)
  (declare (ignore x y mods))
  nil)
(defmethod om-double-clic-callback    ((self t) x y mods)
  (declare (ignore x y mods))
  nil)
(defmethod om-char-callback           ((self t) x y c mods)
  (declare (ignore x y c mods))
  nil)
(defmethod om-char-spec-callback      ((self t) x y spec)
  (declare (ignore x y spec))
  nil)
(defmethod om-context-menu-callback   ((self t) x y)
  (declare (ignore x y))
  nil)

;;; Auto-coerce OMCOLOR -> chroma:color in alloy patterned-shape pattern slot.

(defun %resolve-pattern (renderable slot-reader fallback-key)
  (let ((color (funcall slot-reader renderable)))
    (cond ((null color)         fallback-key)
          ((typep color 'omcolor) (c color))
          (t                    color))))

(defmethod (setf simple:pattern)
    :around (value (shape simple:patterned-shape))
  (call-next-method (%omcolor->chroma value) shape))

(defmethod shared-initialize :around
    ((shape simple:patterned-shape) slot-names &rest initargs)
  (let ((needs-fix (loop for (k v) on initargs by #'cddr
                         thereis (and (eq k :pattern) (typep v 'omcolor)))))
    (if needs-fix
        (apply #'call-next-method shape slot-names
               (loop for (k v) on initargs by #'cddr
                     collect k
                     collect (if (and (eq k :pattern) (typep v 'omcolor))
                                 (c v)
                                 v)))
        (call-next-method))))
