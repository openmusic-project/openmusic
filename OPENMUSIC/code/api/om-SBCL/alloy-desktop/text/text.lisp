(in-package :alloy-desktop)

(defclass text-buffer (editor:buffer) ())

(defclass text-document (editor:document) ())

;;; A text pane of a desktop window: the editor's text view under its :CLASSIC
;;; family, given what a pane of a window owes the desktop around it -- the press
;;; it owns and the click that leaves it, the pointer it holds while it drags, the
;;; context menu of its right button, the I-beam, the desktop's clipboard, a caret
;;; that blinks and a marking that shows only while its window may show it, the
;;; selection the window tracks, a frame.
;;;
;;; Every method here is specialised on TEXT-PANE: the view's own methods (on
;;; EDITOR:TEXT-VIEW) keep running as the next method, none is replaced.

(defclass text-pane (editor:text-view)
  ;; the view's own slots, given the defaults of a pane as INITFORMs: an argument the
  ;; caller passes, NIL included, still wins, and so do a subclass's default initargs
  ((editor::gesture-policy :initform :classic)
   (editor::wrap-p :initform nil)
   (editor::buffer :initform (editor:make-buffer ""))
   (frame-color :initarg :frame-color :initform nil :accessor text-pane-frame-color
                :documentation "Colour of the 1-px frame drawn inside the pane's edges, or NIL for none.")
   (frame-shapes :initform nil
                 :documentation "(WIDTH HEIGHT COLOR . RECTANGLES) of the frame last drawn.")
   (window-focused :initform :unknown
                   :documentation "Whether the pane's window held the desktop's focus when the pane was last drawn.")
   (caret-shown :initform :unknown
                :documentation "Whether the pane showed its caret when it was last drawn.")
   (keyboard-held :initform nil
                  :documentation "Whether the pane held the strong focus after its last focus change."))
  (:documentation "A text view that answers the desktop as a pane of its window."))

(defclass editable-text (text-pane)
  ((editor::single-line-p :initform t))
  (:documentation "A text pane held to one line, as a field of a form."))

(defclass text-edit-view (text-pane) ()
  (:documentation "A text pane over a whole document."))

(defclass editor-scrollbar (editor:scrollbar) ())

;;; ------------------------------------------------------------------ the hooks

(defgeneric text-pane-focus-changed (pane focus)
  (:documentation "Called after PANE took the keyboard (FOCUS :STRONG) or lost it (FOCUS NIL); the toolkit's own passes through :WEAK while it repairs its chains are not reported.")
  (:method ((pane text-pane) focus)
    (declare (ignore focus))
    nil))

(defgeneric text-pane-written (pane character)
  (:documentation "Called after a key typed into PANE changed its text, with the character that stood before the caret as the key arrived (NIL when there was none).")
  (:method ((pane text-pane) character)
    (declare (ignore character))
    nil))

(defmethod (setf alloy:focus) :after (focus (pane text-pane))
  (let ((held (eql focus :strong)))
    (unless (eq held (slot-value pane 'keyboard-held))
      (setf (slot-value pane 'keyboard-held) held)
      (text-pane-focus-changed pane (and held :strong)))))

(defun %pane-character-before (pane index)
  "The character of PANE's text before INDEX, or NIL at the start."
  (when (and index (plusp index))
    (let ((buffer (editor:text-view-buffer pane)))
      (when (<= index (editor:buffer-size buffer))
        (editor:buffer-char buffer (1- index))))))

(macrolet ((written-after (class)
             `(defmethod alloy:handle :around ((event ,class) (pane text-pane))
                (let* ((buffer (editor:text-view-buffer pane))
                       (revision (editor:buffer-revision buffer))
                       (caret (editor:text-view-caret pane)))
                  (multiple-value-prog1 (call-next-method)
                    (unless (= revision (editor:buffer-revision buffer))
                      (text-pane-written pane (%pane-character-before pane caret))))))))
  (written-after alloy:text-event)
  (written-after alloy:key-down))

;;; ------------------------------------------------------------------ the pointer

;;; A press inside is the pane's; a press outside is noted so the release that ends
;;; it, with the pane still holding the keyboard, takes the focus away (the click
;;; that leaves the pane), while the release of a press that began inside never does.
(defvar *text-pane-press* (make-hash-table :test 'eq :weakness :key)
  "Pane -> :INSIDE or :OUTSIDE, where the press it is still waiting the release of landed.")

(defun text-pane-press (pane)
  "Where the press PANE is waiting the release of landed: :INSIDE, :OUTSIDE or NIL."
  (values (gethash pane *text-pane-press*)))

(defgeneric text-pane-menu-items (pane)
  (:documentation "Entries of the menu PANE's right button opens, in the form the desktop's popup menus take."))

(defgeneric open-text-pane-menu (pane items)
  (:documentation "Open ITEMS as PANE's context menu at the pointer."))

(defun %pane-take (pane)
  "Give PANE the keyboard of its window and make it the window's selection owner, undoing the selection of the pane that owned it when the window asks for it."
  (let ((window (element-window pane)))
    (when window
      (let ((owner (window-selected-text-pane window)))
        (when (and owner (not (eq owner pane)) (click-clears-selection-p window))
          (drop-text-selection owner)))
      (setf (window-selected-text-pane window) pane))
    (focus-pane window pane)))

(defmethod alloy:handle :around ((event alloy:pointer-down) (pane text-pane))
  (let ((inside (alloy:contained-p (alloy:location event) pane)))
    (case (alloy:kind event)
      (:right
       (cond (inside
              (let ((items (text-pane-menu-items pane)))
                (when items (open-text-pane-menu pane items)))
              t)
             (t
              (when (eql :strong (alloy:focus pane))
                (unfocus-pane (element-window pane) pane))
              (alloy:decline))))
      ;; every other button presses as the left one does
      (t
       (setf (gethash pane *text-pane-press*) (if inside :inside :outside))
       (cond (inside
              (call-next-method)
              (%pane-take pane)
              (capture-pointer pane)
              t)
             (t (call-next-method)))))))

(defmethod alloy:handle :around ((event alloy:pointer-up) (pane text-pane))
  ;; the release of any button ends the press, as it ends the view's own
  (let ((press (gethash pane *text-pane-press*)))
    (remhash pane *text-pane-press*)
    (cond ((eq pane (pointer-capture (pointer-state)))
           (release-pointer-capture pane)
           (call-next-method)
           t)
          ((and (eq press :outside)
                (eql :strong (alloy:focus pane))
                (not (alloy:contained-p (alloy:location event) pane)))
           (unfocus-pane (element-window pane) pane)
           (alloy:decline))
          (t (call-next-method)))))

;;; The desktop hands the second press of a double click as an event of its own, never
;;; as a press (input/pointer.lisp, %glfw:mouse-button-changed for window): it marks
;;; the run around the caret, whichever the button -- the first press of the left or
;;; middle one put the caret under the pointer, the right one leaves it where it was.
(defmethod alloy:handle ((event alloy:pointer-double-click) (pane text-pane))
  (cond ((alloy:contained-p (alloy:location event) pane)
         (editor:text-view-select-run pane (editor:text-view-caret pane))
         t)
        (t (call-next-method))))

;;; the shape the toolkit's own text input gives the pointer over it
(defmethod alloy:handle :before ((event alloy:pointer-move) (pane text-pane))
  (when (and (alloy:contained-p (alloy:location event) pane) (alloy:ui pane))
    (setf (alloy:cursor (alloy:ui pane)) :text)))

;;; ------------------------------------------------------------------ the menu

(defclass text-pane-menu-item (menu-item)
  ((title :initarg :title :reader menu-entry-title)
   (action :initarg :action :reader menu-entry-action))
  (:documentation "One row of a text pane's context menu."))

(defmethod menu-entry-accelerator ((entry text-pane-menu-item)) nil)
(defmethod menu-entry-enabled ((entry text-pane-menu-item)) t)
(defmethod menu-entry-selected ((entry text-pane-menu-item)) nil)
(defmethod menu-entry-items ((entry text-pane-menu-item)) nil)

(defmethod text-pane-menu-items ((pane text-pane))
  (flet ((row (title command)
           (make-instance 'text-pane-menu-item
                          :title title
                          :action (lambda () (editor:run-command pane command)))))
    (list (row "Cut" :cut) (row "Copy" :copy) (row "Paste" :paste) (row "Select All" :select-all))))

(defmethod open-text-pane-menu ((pane text-pane) items)
  (let ((window (element-window pane))
        (at (cursor-location (pointer-state))))
    (when window
      (popup-close-stack window)
      (multiple-value-bind (left top) (popup-owner-point-to-screen window (alloy:pxx at) (alloy:pxy at))
        (popup-open-menu-at (popup-normalize-entries items window) 0 window left top
                            :return-focus pane)))))

;;; ------------------------------------------------------------------ the clipboard

;;; The desktop's clipboard, for these panes only: every other view keeps the
;;; reader and the writer the editor is given.
(defmethod editor:clipboard-text ((pane text-pane))
  (clipboard-read-text (default-clipboard-service)))

(defmethod (setf editor:clipboard-text) (text (pane text-pane))
  (clipboard-write-text (default-clipboard-service) text)
  text)

;;; ------------------------------------------------------------------ caret and marking

(defmethod editor:text-view-caret-shown-p ((pane text-pane))
  (not (caret-hidden-p pane (element-window pane))))

(defun %pane-marking-hidden-p (pane)
  (let ((window (element-window pane)))
    (and window (not (window-focused-p window)) (not (selection-survives-blur-p window)))))

(defmethod editor:text-view-marking-shown-p ((pane text-pane))
  (not (%pane-marking-hidden-p pane)))

(defmethod editor:text-view-marking-inked-p ((pane text-pane))
  (not (%pane-marking-hidden-p pane)))

;;; The caret blinks with the desktop's cycle (the host's TICK-CARET-BLINK marks the
;;; pane on each flip) and the marking follows the window's focus; neither moves the
;;; text, so a frame only lays out again what changed since the last one.  The pane's
;;; frame is drawn here too, under the glyphs.
(defmethod alloy:render :before ((renderer alloy:renderer) (pane text-pane))
  (let* ((window (element-window pane))
         (focused (and window (window-focused-p window) t))
         (shown (editor:text-view-caret-shown-p pane)))
    (cond ((not (eq focused (slot-value pane 'window-focused)))
           (setf (slot-value pane 'window-focused) focused
                 (slot-value pane 'caret-shown) shown)
           (editor:text-view-refresh pane))
          ((not (eq shown (slot-value pane 'caret-shown)))
           (setf (slot-value pane 'caret-shown) shown)
           (editor:text-view-refresh-caret pane))))
  (%draw-pane-frame renderer pane))

(defmethod alloy:handle :after ((event alloy:text-event) (pane text-pane))
  (when (eql :strong (alloy:focus pane)) (reset-caret-blink)))

(defmethod alloy:handle :after ((event alloy:key-down) (pane text-pane))
  (when (eql :strong (alloy:focus pane)) (reset-caret-blink)))

;;; ------------------------------------------------------------------ the frame

(defun %pane-frame-edges (pane)
  "The four 1-px rects (x y w h) just inside PANE's box."
  (let* ((bounds (alloy:bounds pane))
         (w (alloy:pxw bounds))
         (h (alloy:pxh bounds)))
    (list (list 0 (- h 1) w 1) (list 0 0 w 1) (list 0 0 1 h) (list (- w 1) 0 1 h))))

(defun %pane-frame (renderer pane color)
  "The frame's four rectangles for PANE's present size, made again only when the size or COLOR changed."
  (let* ((bounds (alloy:bounds pane))
         (w (alloy:pxw bounds))
         (h (alloy:pxh bounds))
         (kept (slot-value pane 'frame-shapes)))
    (if (and kept (= w (first kept)) (= h (second kept)) (eq color (third kept)))
        (cdddr kept)
        (cdddr (setf (slot-value pane 'frame-shapes)
                     (list* w h color
                            (loop for (x y ew eh) in (%pane-frame-edges pane)
                                  collect (simple:rectangle renderer (alloy:px-extent x y ew eh)
                                                            :pattern color))))))))

(defmethod (setf text-pane-frame-color) :after (color (pane text-pane))
  (declare (ignore color))
  (alloy:mark-for-render pane))

(defun %draw-pane-frame (renderer pane)
  "Draw PANE's frame, if it has one, before the view's own shapes: the ground, drawn at a lower depth, leaves it standing, and the glyphs, drawn at a higher one, go over it."
  (let ((color (text-pane-frame-color pane)))
    ;; inside the toolkit's own render :around of a layout element, which has already
    ;; moved the origin to the pane's corner
    (when color
      (alloy:with-unit-parent pane
        (dolist (shape (%pane-frame renderer pane color))
          (alloy:render renderer shape))))))
