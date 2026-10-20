(in-package :alloy-desktop)

;;; Style protocol: what a widget look asks its host for on every update.

(defgeneric widget-background (widget)
  (:documentation "Fill painted under WIDGET: a chroma:color or palette key, or NIL for none.")
  (:method ((widget t)) nil))

(defgeneric widget-foreground (widget)
  (:documentation "Colour of WIDGET's text: a chroma:color or palette key, or NIL for the look's own.")
  (:method ((widget t)) nil))

(defgeneric widget-font-entry (widget renderer)
  (:documentation "The font-entry WIDGET's text is drawn with under RENDERER.")
  (:method ((widget t) renderer) (role-font-entry renderer :dialog-default)))

(defgeneric widget-enabled-p (widget)
  (:documentation "True while WIDGET is enabled; a disabled widget draws its text in *disabled-text*.")
  (:method ((widget t)) t))

(defgeneric widget-label (widget)
  (:documentation "The string WIDGET's label shape draws.")
  (:method ((widget t)) (alloy:text widget)))

(defgeneric widget-border (widget)
  (:documentation "Colour of WIDGET's border, or NIL for none.")
  (:method ((widget t)) nil))

(defgeneric widget-border-width (widget)
  (:documentation "Width of WIDGET's border in units.")
  (:method ((widget t)) 1))

;;; The style a dialog item carries for itself, set by initargs and by the setters of the
;;; generics above; a subclass may still answer a generic with a method of its own.

(defclass dialog-item ()
  ((foreground   :initarg :foreground   :initform nil :accessor widget-foreground)
   (background   :initarg :background   :initform nil :accessor widget-background)
   (border       :initarg :border       :initform nil :accessor widget-border)
   (border-width :initarg :border-width :initform 1   :accessor widget-border-width)
   ;; not ENABLED: that slot of a menu holds the menu's own state (menu/menu.lisp: defclass pop-up-menu)
   (enabled-p    :initarg :enabled      :initform t   :accessor widget-enabled-p)
   (font         :initarg :font         :initform :dialog-default :accessor widget-font))
  (:documentation "A dialog pane: drawn above its container's own drawing, answering keys only with the strong
focus, and carrying its own style: colours as chroma colours or palette keys, NIL for the look's own; the font as
a native role keyword or a list (FAMILY POINTS &key SLANT WEIGHT)."))

(defparameter *dialog-item-z-elevation* 10
  "Z the whole subtree of a dialog item is raised by, above the container's own drawing; exceeds the deepest negative shape z in use (-3).")

;;; A dialog pane is an OS child pane and always composites above the parent's drawing;
;;; alloy has one framebuffer shared with the container's own drawing, so the subtree is raised instead.
(defmethod alloy:render :around ((renderer window) (item dialog-item))
  (simple:with-pushed-transforms (renderer)
    (incf (simple:z-index renderer) *dialog-item-z-elevation*)
    (call-next-method)))

(defmethod alloy:handle :around ((event alloy:key-down) (item dialog-item))
  "Answer a key only with the strong focus, as a pane does; text panes (alloy:text-input-component) keep their own rule."
  ;; alloy would otherwise report a key its chain-focused child ignores as handled
  ;; (alloy events.lisp: handle :around for (event handler))
  (if (or (eql :strong (alloy:focus item))
          (typep item 'alloy:text-input-component))
      (call-next-method)
      (alloy:decline)))

;;; A role resolves through the native role table, an explicit font like a role of its
;;; own size; both are memoized per renderer (graphics/font.lisp: cached-font-entry).
(defmethod widget-font-entry ((widget dialog-item) renderer)
  (let ((font (widget-font widget)))
    (if (keywordp font)
        (role-font-entry renderer font)
        (flet ((make-entry ()
                 (destructuring-bind (family points &key slant weight) font
                   ;; a style with no baked atlas falls back to the family's regular face
                   (unless (or (variant-atlas-name family slant weight)
                               (and (member slant '(nil :roman)) (member weight '(nil :regular))))
                     (warn "[font-variant ~S] no ~(~A ~A~) atlas: drawn in the regular face"
                           family (or slant :roman) (or weight :regular)))
                   (make-font-entry :font (resolve-font renderer family slant weight)
                                    :size-unit (alloy:px (points-draw-px renderer points))))))
          (declare (dynamic-extent #'make-entry))
          (cached-font-entry renderer font #'make-entry)))))

;;; A new font reaches a realized label at once: a look that sets the font only at
;;; realization would otherwise keep the old one.
(defmethod (setf widget-font) :after (value (widget dialog-item))
  (declare (ignore value))
  (let ((tree (alloy:layout-tree widget)))
    (when (and tree (alloy:ui tree))
      (apply-font-to-label-shape widget (widget-font-entry widget (alloy:ui tree))))))

(defmethod (setf widget-enabled-p) :after (value (widget dialog-item))
  (declare (ignore value))
  (invalidate widget))

(defmethod (setf widget-foreground) :after (value (widget dialog-item))
  (declare (ignore value))
  (invalidate widget))

(defmethod (setf widget-background) :after (value (widget dialog-item))
  (declare (ignore value))
  (invalidate widget))

(defmethod (setf widget-border) :after (value (widget dialog-item))
  (declare (ignore value))
  (invalidate widget))

(defmethod (setf widget-border-width) :after (value (widget dialog-item))
  (declare (ignore value))
  (invalidate widget))

;;; A view answers from its own style slots (view/view.lisp: the defclass of view).

(defmethod widget-background ((widget view)) (view-background widget))

(defmethod widget-border ((widget view)) (view-border widget))

(defmethod widget-border-width ((widget view)) (view-border-width widget))

(defgeneric widget-mnemonic-index (widget)
  (:documentation "Position of the mnemonic character in WIDGET's label, or NIL.")
  (:method ((widget t)) nil))

(defgeneric widget-frame (widget)
  (:documentation "Colour of the frame the host paints under WIDGET, or NIL for the look's own face.")
  (:method ((widget t)) nil))

(defgeneric widget-frame-border (widget)
  (:documentation "Colour of the border of the frame the host paints under WIDGET, or NIL.")
  (:method ((widget t)) nil))

(defgeneric label-drop (widget)
  (:documentation "Px the top of WIDGET's label shape is pushed down so the baseline lands where the host puts it.")
  (:method ((widget t)) 0f0))

(defparameter *disabled-text* (chroma:rgb 0.5 0.5 0.5)
  "Colour of the text of a disabled widget.")

(defun mnemonic-markup (widget window)
  "simple:text markup underlining WIDGET's mnemonic character, or NIL while WINDOW's keyboard cues are off."
  ;; Windows keeps mnemonics hidden until the keyboard is used
  ;; (LW-lib/8-1-0-0/manual/html-w/capi/capi-common-elements-ug-1.htm:93-95)
  (when (keyboard-cues-p window)
    (let ((index (widget-mnemonic-index widget))
          (text  (widget-label widget)))
      (when (and index (stringp text) (< index (length text)))
        (list (list index (1+ index) :underline))))))

(defun apply-font-to-label-shape (widget entry)
  "Write ENTRY's font and size into WIDGET's :label shape and mark WIDGET; NIL without a shape or entry."
  (let ((label (and entry (presentations:find-shape :label widget nil))))
    (when label
      (setf (simple:font label) (font-entry-font entry))
      (setf (simple:size label) (font-entry-size-unit entry))
      (alloy:mark-for-render widget))))
