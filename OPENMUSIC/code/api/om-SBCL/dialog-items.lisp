(in-package :om-api)

(export '(om-make-dialog-item
          om-dialog-item-act
          om-dialog-item-action
          om-dialog-item-action-function
          om-set-dialog-item-action-function
          om-funcall-dialog-item-action
          om-dialog-item-text
          om-set-dialog-item-text
          om-enable-dialog-item
          om-dialog-item-enabled
           commit
           make-action
          om-standard-dialog-item
          om-static-text
          om-text-view
          om-editable-text
          om-text-edit-view
          om-make-edit-view
          om-scroll-text-edit-view
          om-copy-command
          om-cut-command
          om-paste-command
          om-button
          om-check-box
          om-radio-button
          om-checked-p
          om-set-check-box
          om-single-item-list
          om-multi-item-list
          om-pop-up-dialog-item
          om-set-item-list
          om-get-item-list
          om-get-selected-item-index
          om-set-selected-item-index
          om-select-item-index
          om-unselect-item-index
          om-get-selected-item
          om-set-selected-item
          om-slider
          om-slider-value
          om-slider-increment
          om-set-slider-value
          om-get-slider-range
          om-get-slider-orientation
          om-zoom-pop-up
          om-zoom-pop-up-value
          om-set-zoom-pop-up-value
          om-zoom-pop-up-presets
          om-set-zoom-pop-up-presets)
        :om-api)

;;; Abstract base

(defclass om-standard-dialog-item (alld:dialog-item om-graphic-object)
  ((di-action :accessor di-action :initform nil :initarg :di-action)
   (dialog-item-after-fun :accessor dialog-item-after-fun :initform nil
                          :initarg :dialog-item-after-fun)
   ;; vanilla reads the text back from the CAPI pane (om-LW/dialog-items.lisp: om-dialog-item-text for om-standard-dialog-item,
   ;; capi::item-text), which every dialog item carries because om-make-dialog-item
   ;; hands :text to each pane (om-LW/dialog-items.lisp: the :text of om-make-dialog-item)
   (item-text :accessor di-text :initarg :text :initform "")
   ;; LispWorks chooses the mnemonic itself for every push button, check box and
   ;; radio button of an interface, so the slot belongs to all of them and
   ;; %ASSIGN-MNEMONICS is its only writer. It holds the POSITION in the label, not
   ;; the character: two words of a title can start with the same letter and the
   ;; vanilla underlines a precise one ("Enable Listener &input").
   (mnemonic-index :initform nil :accessor %item-mnemonic-index)))

(defmethod om-subviews ((self om-standard-dialog-item)) nil)

;;; CAPI ends a gesture on the pane that took the press, so a widget under the cursor
;;; never steals it; Alloy routes by containment alone (alloy layout.lisp: handle for (pointer-event layout)).
(defmethod alloy:handle :around ((event alloy:pointer-up) (self om-standard-dialog-item))
  "Hand a release back to the view that received the press, as om-item-view does."
  (let ((owner (alld:clicked-element (alld:pointer-state))))
    (if (and owner
             (not (eql owner :abort))
             (not (eql owner self))
             (not (eql owner (vcontainer self))))
        (progn
          (om-click-release-handler
           owner (%alloy-location-to-om-point (alloy:location event) owner))
          (setf (alld:clicked-element (alld:pointer-state)) nil)
          t)
        (call-next-method))))

(defmethod di-after-settings ((self t)) nil)

(defmethod di-set-focus ((self t))
  (when (and (typep self 'alloy:focus-element)
             (alloy:focus-tree self))
    (%om-focus-pane (om-view-window self) self)))

(defmethod special-bg ((self t)) nil)

;;; Rect a dialog item paints under itself, as a CAPI pane does: NIL keeps the
;;; drawing unchanged; kernel-patch-after.lisp answers it for the interface boxes.
(defmethod %di-frame-color ((self t)) nil)
(defmethod %di-frame-border-color ((self t)) nil)

;;; The widget looks read the library's style state (alloy-desktop/widgets/dialog-item.lisp:
;;; defclass dialog-item). The OM colour slots keep what om-get-fg-color and om-get-bg-color
;;; answer; every write of them is handed to the library in its own colour type.

(defmethod initialize-instance :after ((self om-standard-dialog-item) &key)
  (setf (alld:widget-foreground self) (%resolve-pattern self #'om-fg-color nil)
        (alld:widget-background self) (%resolve-pattern self #'om-bg-color nil)))

(defmethod (setf om-fg-color) :after (color (self om-standard-dialog-item))
  (declare (ignore color))
  (setf (alld:widget-foreground self) (%resolve-pattern self #'om-fg-color nil)))

(defmethod (setf om-bg-color) :after (color (self om-standard-dialog-item))
  (declare (ignore color))
  (setf (alld:widget-background self) (%resolve-pattern self #'om-bg-color nil)))

(defmethod alld:widget-font-entry ((self om-standard-dialog-item) renderer)
  (%om-font-entry renderer (or (om-font self) (%om-native-font :dialog-default))))

(defmethod alld:widget-mnemonic-index ((self om-standard-dialog-item))
  (%item-mnemonic-index self))

;;; what om-dialog-item-text answers: the push button's value, the caption of a check
;;; box or of a radio button
(defmethod alld:widget-label ((self om-standard-dialog-item))
  (om-dialog-item-text self))

(defmethod alld:widget-frame ((self om-standard-dialog-item))
  (let ((f (%di-frame-color self)))
    (and f (%omcolor->chroma f))))

(defmethod alld:widget-frame-border ((self om-standard-dialog-item))
  (let ((f (%di-frame-border-color self)))
    (and f (%omcolor->chroma f))))

(defmethod om-set-font :around ((self om-standard-dialog-item) font)
  (when font (call-next-method)))

(defmethod om-set-view-size :after ((self om-standard-dialog-item) size-point)
  (declare (ignore size-point))
  (di-after-settings self))

;;; Action protocol

(defmacro om-dialog-item-act (var &body body)
  (let ((decls (loop while (and (consp (car body)) (eq (caar body) 'declare))
                     collect (pop body))))
    `#'(lambda (,var)
         ,@decls
         (handler-bind ((error
                          #'(lambda (err)
                              (om-message-dialog
                               (format nil "An error of type ~a occurred: ~a"
                                       (type-of err) (princ-to-string err)))
                              ;; abort restart may be absent outside the toplevel loop
                              (let ((restart (find-restart 'abort err)))
                                (when restart (invoke-restart restart))))))
           ,@body))))

;;; vanilla answers geometry from the OM slots (om-LW/dialog-items.lisp: om-view-position through om-set-view-size for om-standard-dialog-item);
;;; bounds are host-absolute in the flat model and would double-count containers.
;;; PRIMARY *and* :around: alloy superclasses precede om-standard-dialog-item in the
;;; CPL (so the :around is what wins for a backed widget), while a dialog item with no
;;; alloy backing at all (menu.lisp: defclass om-pop-up-menu) has no other primary.

(defun %di-slot-position (di) (om-make-point (vx di) (vy di)))
(defun %di-slot-size     (di) (om-make-point (vw di) (vh di)))

(defmethod om-view-position ((self om-standard-dialog-item)) (%di-slot-position self))
(defmethod om-view-position :around ((self om-standard-dialog-item)) (%di-slot-position self))

(defmethod om-view-size ((self om-standard-dialog-item)) (%di-slot-size self))
(defmethod om-view-size :around ((self om-standard-dialog-item)) (%di-slot-size self))

(defmethod om-set-view-position ((self om-standard-dialog-item) pos-point)
  (setf (vx self) (%pt-x pos-point) (vy self) (%pt-y pos-point))
  pos-point)

(defmethod om-set-view-size ((self om-standard-dialog-item) size-point)
  (setf (vw self) (%pt-x size-point) (vh self) (%pt-y size-point))
  size-point)

(defmethod om-change-view-position ((self om-standard-dialog-item) delta)
  (om-set-view-position self (om-make-point (+ (vx self) (%pt-x delta))
                                            (+ (vy self) (%pt-y delta)))))

(defmethod om-width  ((self om-standard-dialog-item)) (om-point-h (om-view-size self)))
(defmethod om-height ((self om-standard-dialog-item)) (om-point-v (om-view-size self)))
(defmethod om-interior-size ((self om-standard-dialog-item)) (om-view-size self))

;;; a dialog item is a native scrolling pane in vanilla (om-LW/dialog-items.lisp:
;;; om-make-dialog-item): the axis it asked for shows whether or not there is overflow
(defmethod alld:scrollbars-forced-p ((self om-standard-dialog-item)) t)

(defmethod om-dialog-item-action ((self om-standard-dialog-item))
  (when (di-action self) (funcall (di-action self) self)))

(defmethod om-set-dialog-item-action-function ((self om-standard-dialog-item) action)
  (setf (di-action self) action))

(defmethod om-dialog-item-action-function ((self om-standard-dialog-item))
  (di-action self))

(defmethod om-funcall-dialog-item-action ((self om-standard-dialog-item))
  (when (di-action self) (funcall (di-action self) self)))

(defmethod om-dialog-item-after-action ((self om-standard-dialog-item))
  (when (dialog-item-after-fun self) (funcall (dialog-item-after-fun self) self)))

;;; vanilla (om-LW/dialog-items.lisp: om-dialog-item-text, om-set-dialog-item-text)
;;; defines both on om-standard-dialog-item, so lists / slider / pop-up answer them too
;;; (kernel boxes/dialog-item-boxes.lisp: set-dialog-item-params for d-i-box).
(defmethod om-dialog-item-text ((self om-standard-dialog-item))
  (di-text self))

(defmethod om-set-dialog-item-text ((self om-standard-dialog-item) text)
  (setf (di-text self) (or text ""))
  (when (typep self 'alloy:renderable) (alloy:mark-for-render self))
  text)

;;; CAPI prints a pane as #<class <summary> <address>>; alloy's own method leaks bounds
(defmethod print-object ((self om-standard-dialog-item) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (let ((text (di-text self)))
      (when (and text (plusp (length text)))
        (format stream "~S " text)))))

(defmethod om-enable-dialog-item ((self om-standard-dialog-item) t-or-nil)
  (setf (alld:widget-enabled-p self) t-or-nil))

(defmethod om-dialog-item-enabled ((self om-standard-dialog-item))
  (alld:widget-enabled-p self))

;;; Shared helpers

(defun %plist-remove (plist key)
  (loop for (k v) on plist by #'cddr unless (eq k key) append (list k v)))

(defun %string-designator-p (x)
  (or (stringp x) (characterp x) (and x (symbolp x))))

(defun %popup-item-position (value items)
  "Index in ITEMS whose label is STRING-EQUAL to VALUE, or NIL. Vanilla matches with
string-equal (om-LW/dialog-items.lisp: initialize-instance :after for om-pop-up-dialog-item), so a keyword designates its name."
  (when (%string-designator-p value)
    (position value items
              :test (lambda (v item)
                      (let ((label (if (consp item) (car item) item)))
                        (and (%string-designator-p label) (string-equal v label)))))))

(defun %apply-font-to-label-shape (widget font)
  (let ((w (om-view-window widget)))
    ;; before its realization a widget has no label to dress and no window to resolve under
    (when (and w font (presentations:find-shape :label widget nil))
      (let ((r (%om-font-resolve w font)))
        (when (and r (not (stringp r)))
          (alld:apply-font-to-label-shape
           widget (alld:make-font-entry :font r :size-unit (%om-font-draw-unit w font))))))))

(defun %text-cursor-pos-from-event (pane event)
  "Char index under EVENT's window-absolute location, via the corrected msdf estimator.
The origin is where the text is DRAWN, so a scrolled pane adds its label offset."
  (let ((label (presentations:find-shape :label pane)))
    (when label
      (alloy:with-unit-parent pane
        (let* ((origin (alloy:global-location pane))
               (offset (if (typep pane 'om-scroll-mixin)
                           (presentations:offset label)
                           (alloy:px-point 0 0))))
          (org.shirakumo.alloy.renderers.opengl.msdf::estimate-cursor-pos
           label (alloy:location event)
           (alloy:px-point (+ (alloy:pxx origin) (alloy:pxx offset))
                           (+ (alloy:pxy origin) (alloy:pxy offset)))))))))

(defparameter *text-drag-scroll-step* 12
  "Pixels a text pane scrolls per motion event while the selection drag is outside it.")

(defun %text-drag-scroller (pane)
  "What scrolls PANE while its text is selected by dragging: the clip-view that hosts
it (editor panes live in an alloy:scroll-view), else its own om-scroll-mixin offset."
  (or (alld:find-ancestor-clip-view pane)
      (and (typep pane 'om-scroll-mixin) pane)))

(defun %text-drag-autoscroll (pane location)
  "CAPI editor parity: a selection drag that leaves the visible text area scrolls it.
Both scrollers speak alloy:offset with the same signs -- x negative = scrolled right,
y positive = scrolled down (alloy-desktop/view/scroll.lisp: alloy:offset and (setf alloy:offset) for scroll-mixin, alloy layouts/clip-view.lisp: clamp-offset)."
  (let ((scroller (%text-drag-scroller pane)))
    (when scroller
      (alloy:with-unit-parent scroller
        (let* ((origin (alloy:global-location scroller))
               (bounds (alloy:bounds scroller))
               (x0     (alloy:pxx origin))
               (y0     (alloy:pxy origin))
               (x      (alloy:pxx location))
               (y      (alloy:pxy location))
               (dx     (cond ((< x x0)                        (- *text-drag-scroll-step*))
                             ((> x (+ x0 (alloy:pxw bounds))) *text-drag-scroll-step*)
                             (t 0)))
               (dy     (cond ((< y y0)                        *text-drag-scroll-step*)
                             ((> y (+ y0 (alloy:pxh bounds))) (- *text-drag-scroll-step*))
                             (t 0))))
          ;; one axis per write: alloy's clip-view drops an offset whose two axes both
          ;; change (alloy layouts/clip-view.lisp: (setf offset) :around for (point clip-view) guards with AND instead of OR)
          (unless (zerop dx)
            (let ((offset (alloy:offset scroller)))
              (setf (alloy:offset scroller)
                    (alloy:px-point (- (alloy:pxx offset) dx) (alloy:pxy offset)))))
          (unless (zerop dy)
            (let ((offset (alloy:offset scroller)))
              (setf (alloy:offset scroller)
                    (alloy:px-point (alloy:pxx offset)
                                    (+ (alloy:pxy offset) dy))))))))))

(defgeneric %syntax-ranges (component)
  (:documentation "Markup elements (START END (:COLOR c)) colouring COMPONENT's
own text by syntax, ascending and disjoint.  NIL for every pane whose content is
not source code; the panes that hold Lisp source supply their own (sbcl-lisp-tools).")
  (:method ((component t)) nil))

(defun %merge-selection-markup (ranges start end)
  "RANGES with [START,END) forced to the selection style.  Overlapping pieces are
clipped rather than stacked, so the result stays ascending and disjoint: two
elements sharing an end offset would leak their style onto the following text
(alloy renderers/simple/defaults.lisp: flatten-markup pops one style per offset, not all)."
  (let ((out (list (list start end (list :color colors:white)))))
    (dolist (range ranges)
      (destructuring-bind (s e style) range
        (when (< s (min e start))
          (push (list s (min e start) style) out))
        (when (< (max s end) e)
          (push (list (max s end) e style) out))))
    (sort out #'< :key #'first)))

(defun %selection-markup (component)
  "Markup for COMPONENT's label: its syntax colours, with the selected character
range painted white on top.  The white only applies while the :selection rect
under it is painted too, else a hidden highlight would leave white text on a
white field (alloy-desktop text/selection.lisp: selection-hidden-p); the syntax colours stay either
way.  Selection wins over syntax on the characters it covers, keeping the rule
that one range carries exactly one style."
  (let* ((syntax (%syntax-ranges component))
         (cursor (alloy:cursor component))
         (anchor (and cursor (alloy:anchor cursor))))
    (if (and anchor (not (alld:selection-hidden-p component (om-view-window component))))
        (let ((start (min anchor (alloy:pos cursor)))
              (end   (max anchor (alloy:pos cursor))))
          (if (< start end)
              (%merge-selection-markup syntax start end)
              syntax))
        syntax)))

;;; Undo / Redo (linear-history pattern)

(defclass history () ())

(defgeneric commit (action history))
(defgeneric actions (history))
(defgeneric clear (history))

(defgeneric om-history-redo (action region))
(defgeneric om-history-undo (action region))

(defclass action ()
  ((description :initarg :description :initform "<unknown change>" :accessor description)
   (index :initform 0 :accessor index)))

(defmethod print-object ((action action) stream)
  (print-unreadable-object (action stream :type T)
    (format stream "~a ~a" (index action) (description action))))

(defclass linear-history (history)
  ((actions         :initform (make-array 0 :adjustable t :fill-pointer t) :accessor actions)
   (history-pointer :initform 0 :accessor history-pointer)
   (capacity        :initarg  :capacity :initform 200 :accessor capacity)))

(defmethod commit ((action action) (history linear-history))
  (setf (index action) (history-pointer history))
  (setf (fill-pointer (actions history)) (history-pointer history))
  (vector-push-extend action (actions history))
  (incf (history-pointer history))
  (let ((excess (- (fill-pointer (actions history)) (capacity history))))
    (when (plusp excess)
      (loop repeat excess do
        (loop for i from 0 below (1- (fill-pointer (actions history)))
              do (setf (aref (actions history) i)
                       (aref (actions history) (1+ i)))))
      (decf (fill-pointer (actions history)) excess)
      (decf (history-pointer history) excess)
      (loop for i from 0 below (fill-pointer (actions history))
            do (setf (index (aref (actions history) i)) i)))))

(defmethod om-history-redo ((history linear-history) region)
  (when (< (history-pointer history) (fill-pointer (actions history)))
    (om-history-redo (aref (actions history) (history-pointer history)) region)
    (incf (history-pointer history))))

(defmethod om-history-undo ((history linear-history) region)
  (when (< 0 (history-pointer history))
    (decf (history-pointer history))
    (om-history-undo (aref (actions history) (history-pointer history)) region)))

(defmethod clear ((history linear-history))
  (loop for i from 0 below (length (actions history))
        do (setf (aref (actions history) i) nil))
  (setf (fill-pointer (actions history)) 0)
  (setf (history-pointer history) 0))

(defclass closure-action (action)
  ((redo :initarg :redo)
   (undo :initarg :undo)))

(defmethod om-history-redo ((action closure-action) (region (eql t)))
  (funcall (slot-value action 'redo)))

(defmethod om-history-undo ((action closure-action) (region (eql t)))
  (funcall (slot-value action 'undo)))

(defmacro make-action (redo undo &rest initargs)
  (let ((redog (gensym "REDO"))
        (undog (gensym "UNDO")))
    `(flet ((,redog () ,redo)
            (,undog () ,undo))
       (make-instance 'closure-action :redo #',redog :undo #',undog ,@initargs))))

;;; om-static-text — backed by alld:static-text

(defun text-wrap-pix (text-str font width-pix)
  "The lines TEXT-STR occupies in the OM FONT inside WIDTH-PIX pixels, by the vanilla
rule (om-LW/dialog-items.lisp: text-wrap-pix), which the library implements
(alloy-desktop/widgets/controls.lisp: text-wrap-pix); measured under the metric renderer."
  (alld:text-wrap-pix text-str
                      (%om-font-entry (%metric-renderer) (or font *current-font* *om-default-font2*))
                      width-pix))

;;; :text is the vanilla initarg carrying the label (om-LW/dialog-items.lisp: the :text of om-make-dialog-item, slot text of om-static-text);
;;; the inherited :value initarg stays free for the kernel's own value slot.
;;; The value slot keeps the string the kernel wrote and om-dialog-item-text hands
;;; it back untouched, exactly as vanilla keeps text apart from wrapped-text
;;; (om-LW/dialog-items.lisp: defclass om-static-text): the broken lines live only
;;; in the label shape, so a caller that re-reads the text never sees the breaks.
(defclass om-static-text (alld:static-text om-standard-dialog-item) ())

;;; a static text is drawn by the OM rules, and vanilla promotes a NIL font to the OM
;;; default rather than to the toolkit's (om-LW/dialog-items.lisp: om-set-font for om-static-text)
(defmethod alld:widget-font-entry ((self om-static-text) renderer)
  (%om-font-entry renderer (or (om-font self) *om-default-font1*)))

(defmethod alld:widget-border ((self om-static-text))
  (let ((border (om-component-border self)))
    (when border
      (%omcolor->chroma (if (om-color-p border) border *om-black-color*)))))

;;; an om-static-text always breaks its text at its full width, as vanilla recomputes
;;; wrapped-text on every write, resize and font change (om-LW/dialog-items.lisp: update-text);
;;; the library draws the break (alloy-desktop/widgets/controls.lisp: widget-label for static-text)
(defmethod alloy:wrap ((self om-static-text)) t)

(defmethod alld:label-drop ((st om-static-text))
  "Px that put the baseline of line 0 of ST's label exactly om-string-h below the
TOP of the item, where vanilla draws it (om-LW/dialog-items.lisp: om-draw-contents
for om-static-text, y = lineh*(k+1)); the left edge stays at dx = 1
(alloy-desktop/widgets/controls.lisp: static-text-label-bounds).
The shape itself is top-anchored, and a top-anchored MSDF block puts its baseline
one atlas base below its own top, not one line height -- the two differ by the
descent -- so the top of the shape is pushed down by that difference. A font
whose base already exceeds om-string-h keeps the shape at the item top rather
than riding above it."
  (let* ((*current-renderer* (or (om-view-window st) *current-renderer*))
         (font (or (om-font st) *om-default-font1*)))
    (max 0f0 (- (float (om-string-h font) 0f0) (%om-font-baseline-px font)))))

(defmethod om-dialog-item-text ((self om-static-text))
  (alld:widget-text self))

(defmethod om-set-dialog-item-text ((self om-static-text) text)
  (setf (alld:widget-text self) text))

;;; vanilla (om-LW/dialog-items.lisp: om-set-font for om-static-text) forces the default
;;; font when none is given, so om-get-font is never NIL for a static text the kernel
;;; measures (kernel boxframe.lisp: change-name-box for omboxframe, reinit-size for boxtypeframe).
(defmethod om-set-font :around ((self om-static-text) font)
  (call-next-method self (or font *om-default-font1*)))

;;; the OM descriptor is handed to the library's font state, which re-dresses the label
;;; through the translation above (alld:widget-font-entry for om-static-text)
(defmethod om-set-font :after ((self om-static-text) font)
  (setf (alld:widget-font self) font))

;;; vanilla om-static-text is a pinboard object (om-LW/dialog-items.lisp: defclass om-static-text):
;;; pointer events route to the kernel handlers on it (e.g. icon-finder-name
;;; click/doubleclick, kernel iconfinder.lisp: om-view-click-handler and
;;; om-view-doubleclick-handler for icon-finder-name), mirroring om-item-view.

(defmethod alloy:handle ((event alloy:pointer-down) (self om-static-text))
  (let ((pos (%alloy-location-to-om-point (alloy:location event) self)))
    (case (alloy:kind event)
      (:right
       (om-context-menu-callback self (om-point-h pos) (om-point-v pos))
       t)
      (otherwise
       ;; vanilla om-static-text IS an om-item-view (om-LW/dialog-items.lisp: defclass om-static-text), so the
       ;; press grabs the pane that draws it, exactly as (actions.lisp: %capture-gesture-pane) does -- never the
       ;; object itself, which has no pointer-move handler.
       (alld:capture-pointer (vcontainer self))
       (setf (alld:clicked-element (alld:pointer-state)) self)
       (om-view-click-handler self pos)
       t))))

(defmethod alloy:handle ((event alloy:pointer-double-click) (self om-static-text))
  (let ((pos (%alloy-location-to-om-point (alloy:location event) self)))
    (om-view-doubleclick-handler self pos)
    t))

(defmethod alloy:handle ((event alloy:pointer-up) (self om-static-text))
  (cond ((and (alld:clicked-element (alld:pointer-state)) (not (eql (alld:clicked-element (alld:pointer-state)) :abort)))
         (let* ((target (alld:clicked-element (alld:pointer-state)))
                (pos    (%alloy-location-to-om-point (alloy:location event) target)))
           (om-click-release-handler target pos)
           (setf (alld:clicked-element (alld:pointer-state)) nil)
           t))
        (t (alloy:decline))))

;;; CAPI hit order: an om-item-view -- om-static-text included (om-LW/item-view.lisp: defclass om-item-view,
;;; om-LW/dialog-items.lisp: defclass om-static-text) -- is a pinboard OBJECT, swept after the panes
;;; of a window root (alloy-desktop view/view.lisp: alloy:handle for (pointer-event fixed-layout)).

;;; om-text-view — read-only display text, backed by alloy:label*

(defclass om-text-view (alld:text-view om-standard-dialog-item) ())

(defmethod om-dialog-item-text ((self om-text-view))
  (alld:widget-text self))

(defmethod om-set-dialog-item-text ((self om-text-view) text)
  (setf (alld:widget-text self) text))

(defmethod om-set-font :after ((self om-text-view) font)
  (setf (alld:widget-font self) font))

;;; om-button — backed by alld:button; activation funnels to om-dialog-item-action
;;; (a disabled one never gets here: alloy-desktop/widgets/controls.lisp: alloy:activate :around for button).

(defclass om-button (alld:button om-standard-dialog-item) ())

(defmethod alloy:activate ((self om-button))
  (om-dialog-item-action self)
  self)

(defmethod om-dialog-item-text ((self om-button))
  (alld:widget-text self))

(defmethod om-set-dialog-item-text ((self om-button) text)
  (setf (alld:widget-text self) text))

(defmethod om-set-font :after ((self om-button) font)
  (setf (alld:widget-font self) font))

;;; om-editable-text — backed by alloy:input-line; di-action on change/accept.

;;; vanilla splits these on #+cocoa (om-LW/dialog-items.lisp: om-get-text-edit-size,
;;; om-get-text-edit-pos); :cocoa is a LispWorks feature, so macOS reads it as :darwin.
(defun om-get-text-edit-size (init-size)
  #+(or darwin linux) (om-add-points init-size (om-make-point 12 8))
  #-(or darwin linux) (om-add-points init-size (om-make-point 8 2)))

(defun om-get-text-edit-pos (init-pos)
  #+(or darwin linux) (om-add-points init-pos (om-make-point -4 -4))
  #-(or darwin linux) (om-add-points init-pos (om-make-point -2 -1)))

(defun %normalize-editable-text (text multiline-p)
  "Copy TEXT into the representation exposed by an editable text pane.
Single-line panes discard CR/LF. Multiline panes canonicalize CRLF and lone
CR to LF, matching the platform-independent text returned by CAPI."
  (let ((source (or text "")))
    (with-output-to-string (out)
      (loop with length = (length source)
            for index from 0 below length
            for character = (char source index)
            do (cond
                 ((char= character #\Return)
                  (when multiline-p
                    (write-char #\Newline out))
                  (when (and (< (1+ index) length)
                             (char= (char source (1+ index)) #\Linefeed))
                    (incf index)))
                 ((char= character #\Linefeed)
                  (when multiline-p
                    (write-char #\Newline out)))
                 (t
                  (write-char character out)))))))

;;; om-scroll-mixin first in the CPL so its scroll methods outrank the om-graphic-object
;;; fallbacks; vanilla hands :horizontal-scroll to this pane too
;;; (om-LW/dialog-items.lisp: om-make-dialog-item). With
;;; the default :scrollbars NIL no chrome appears and the intrinsic clip still holds.
(defclass om-editable-text (om-scroll-mixin alloy:input-line om-standard-dialog-item)
  ((dragging-p
    :accessor dragging-p :initform nil)
   ;; vanilla :allows-newline-p (om-LW/dialog-items.lisp: om-make-dialog-item),
   ;; kept for signature parity; governs only
   ;; whether programmatic text keeps newlines (%normalize-editable-text) — no key
   ;; inserts one, the pane is single-line
   (allow-returns
    :accessor allow-returns :initarg :allow-returns :initform nil)
   (completion-function
    :accessor completion-function
    :initarg :completion-function :initform nil)
   (in-place-completion-function
    :accessor in-place-completion-function
    :initarg :in-place-completion-function :initform nil)
   (complete-do-action
    :accessor complete-do-action
    :initarg :complete-do-action :initform nil))
  (:default-initargs
   :data (make-instance 'alloy:value-data :value "")))

;;; vanilla text-edit-changed-action (om-LW/dialog-items.lisp: text-edit-changed-action): change funcalls
;;; the di-action SLOT, not the action method, and CAPI fires it on any text write,
;;; programmatic included (capi-capi-ref-488.htm:148) — hence no programmatic guard.
;;; observables notifies :BEFORE the write (observables protocol.lisp: macro define-observable), so the
;;; commit point is this :after.
(defmethod (setf alloy:value) :after (new-value (self om-editable-text))
  (when (di-action self) (funcall (di-action self) self))
  (let ((text (or new-value ""))
        (pos  (alloy:pos (alloy:cursor self))))
    (unless (or (string= text "") (> pos (length text)))
      (om-view-key-handler self (elt text (max 0 (1- pos)))))))

;;; Read by the kernel as oa::*edited-self* to pick which of an info-window's text
;;; fields cut/copy/paste act on (kernel graphics/infowindow.lisp: cut and copy for info-window, paste for info+offset-window).
(defvar *edited-self* nil)

;;; Caught on the generic function, not through alloy:observe: the notifier that
;;; define-observable installs for (setf focus) (alloy focus-tree.lisp: the define-observable of (setf focus)) carries
;;; the same qualifier and specializers as the check-type :before of (setf focus)
;;; for (t focus-element), which
;;; therefore REPLACES it -- no observer of 'alloy:focus is ever notified.
(defmethod (setf alloy:focus) :after (focus (self om-editable-text))
  (case focus
    ((nil)   (om-dialog-item-after-action self))
    ;; CAPI marks it when editing starts, i.e. on the focus change
    ;; (capi-capi-ref-488.htm:236); a click with no typing must count.
    (:strong (setf *edited-self* self))))

(defmethod alloy:accept :after ((self om-editable-text))
  (om-dialog-item-action self))

(defmethod om-dialog-item-text ((self om-editable-text))
  (copy-seq (or (alloy:value self) "")))

;;; through (setf alloy:text): its :after clamps the cursor to the new length
;;; (alloy components/text-input.lisp: (setf text) :after for (t text-input-component)), as CAPI does when the pane text is replaced.
(defmethod om-set-dialog-item-text ((self om-editable-text) text)
  (setf (alloy:text self)
        (%normalize-editable-text text (allow-returns self)))
  text)

(defmethod special-bg ((self om-editable-text))
  *om-white-color*)

(defmethod di-set-focus ((self om-editable-text))
  (when (alloy:focus-tree self)
    (%om-focus-pane (om-view-window self) self)
    (let ((cursor (alloy:cursor self)))
      (setf (alloy:anchor cursor) 0)
      (alloy:move-to :end cursor))
    (alloy:mark-for-render self)))

;;; With nothing selected alloy copies the WHOLE text (alloy components/text-input.lisp: handle for (copy-event text-input-component)),
;;; while its own cut is a no-op there (handle for (cut-event text-input-component)). Copying no selection takes nothing,
;;; and must leave whatever the clipboard already held.
(defmethod alloy:handle :around ((event alloy:copy-event) (pane alloy:text-input-component))
  (when (alloy:anchor (alloy:cursor pane))
    (call-next-method)))

;;; alloy's input-line override (alloy components/text-input.lisp: the second handle
;;; for (paste-event text-input-component)) redefines the same GF and specializer as
;;; the first, dropping its selection delete; CAPI replaces.
(defmethod alloy:handle ((event alloy:paste-event) (pane om-editable-text))
  (let ((cursor (alloy:cursor pane)))
    (when (and (stringp (alloy:content event)) (alloy:anchor cursor))
      (alloy::delete-text (min (alloy:anchor cursor) (alloy:pos cursor))
                          (max (alloy:anchor cursor) (alloy:pos cursor))
                          pane)
      (setf (alloy:anchor cursor) nil)))
  (call-next-method))

(defmethod om-copy-command ((self om-editable-text))
  (alloy:handle (make-instance 'alloy:copy-event) self))

(defmethod om-cut-command ((self om-editable-text))
  (alloy:handle (make-instance 'alloy:cut-event) self))

(defmethod om-paste-command ((self om-editable-text))
  (let* ((win  (om-view-window self))
         (clip (and win (alloy:clipboard win))))
    (when (stringp clip)
      (alloy:handle (make-instance 'alloy:paste-event :content clip) self))))

;;; CAPI input grab: a click inside the field is consumed by it (the panel
;;; never sees it); the release of the click that CREATED the field must not
;;; unfocus it — only a full press+release outside exits (click-away).
(defvar *text-pointer-down* (make-hash-table :test 'eq :weakness :key))

(defmethod alloy:handle ((event alloy:pointer-down) (pane om-editable-text))
  (cond ((alloy:contained-p (alloy:location event) pane)
         (setf (gethash pane *text-pointer-down*) :inside)
         t)
        (t
         (setf (gethash pane *text-pointer-down*) :outside)
         (alloy:decline))))

(defmethod alloy:handle ((event alloy:pointer-up) (pane om-editable-text))
  (let ((down (gethash pane *text-pointer-down*)))
    (remhash pane *text-pointer-down*)
    (cond ((alloy:contained-p (alloy:location event) pane)
           (%om-focus-pane (om-view-window pane) pane)
           t)
          ((and (eql :strong (alloy:focus pane)) (eq down :outside))
           (%om-unfocus-pane (om-view-window pane) pane)
           (alloy:decline))
          (t (alloy:decline)))))

(defmethod alloy:handle :after ((event alloy:pointer-down) (pane om-editable-text))
  (unless (eq (alloy:kind event) :right)
    (%om-focus-pane (om-view-window pane) pane)
    (let ((cursor (alloy:cursor pane)))
      (setf (alloy:anchor cursor) (alloy:pos cursor)))
    (alloy:mark-for-render pane)
    (setf (dragging-p pane) t)
    (alld:capture-pointer pane)))

(defmethod alloy:handle :around ((event alloy:pointer-down) (pane om-editable-text))
  (case (alloy:kind event)
    (:right
     (cond ((alloy:contained-p (alloy:location event) pane)
            (let ((tree (om-get-menu-context pane)))
              (when tree (om-open-pop-up-menu tree pane)))
            t)
           (t
            (when (eql :strong (alloy:focus pane))
              (%om-unfocus-pane (om-view-window pane) pane))
            (alloy:decline))))
    (otherwise (call-next-method))))

(defmethod alloy:handle :before ((event alloy:pointer-move) (pane om-editable-text))
  (when (dragging-p pane)
    (%text-drag-autoscroll pane (alloy:location event))
    (let ((new-pos (%text-cursor-pos-from-event pane event)))
      (when new-pos
        (alloy:move-to new-pos (alloy:cursor pane))
        (alloy:mark-for-render pane)))))

(defmethod alloy:handle :around ((event alloy:pointer-up) (pane om-editable-text))
  (if (dragging-p pane)
      (progn
        (setf (dragging-p pane) nil)
        (alld:release-pointer-capture pane)
        (let ((cursor (alloy:cursor pane)))
          (when (and (alloy:anchor cursor)
                     (= (alloy:anchor cursor) (alloy:pos cursor)))
            (setf (alloy:anchor cursor) nil)))
        (%om-focus-pane (om-view-window pane) pane)
        t)
      (call-next-method)))

(defmethod alloy:handle :around ((event alloy:pointer-event) (pane om-editable-text))
  ;; the clip hides events aimed at what is scrolled out of sight -- but not from the
  ;; pane that owns the gesture, whose drag must keep tracking the pointer outside
  (let ((clip (alld:find-ancestor-clip-view pane)))
    (if (and clip
             (not (eq pane (alld:pointer-capture (alld:pointer-state))))
             (not (alloy:contained-p (alloy:location event) clip)))
        (alloy:decline)
        (call-next-method))))

(defun %select-word-at-cursor (pane)
  "Select the non-whitespace run around the cursor, as a native double-click does."
  (let* ((text (or (alloy:value pane) ""))
         (len  (length text))
         (pos  (min (alloy:pos (alloy:cursor pane)) len)))
    (flet ((wordp (i) (and (< -1 i len)
                           (not (member (char text i) '(#\Space #\Tab #\Newline #\Return))))))
      (let ((start pos) (end pos))
        (loop while (wordp (1- start)) do (decf start))
        (loop while (wordp end) do (incf end))
        (when (< start end)
          (setf (alloy:anchor (alloy:cursor pane)) start)
          (alloy:move-to end (alloy:cursor pane)))
        (alloy:mark-for-render pane)))))

;;; A declined double-click bubbles to the container -- in a patch that is the
;;; kernel's create-a-box gesture; a native text pane consumes it instead.
(defmethod alloy:handle ((event alloy:pointer-double-click) (pane om-editable-text))
  (cond ((alloy:contained-p (alloy:location event) pane)
         (%select-word-at-cursor pane)
         t)
        (t (alloy:decline))))

(defmethod om-get-menu-context ((self om-editable-text))
  (list (om-new-leafmenu "Cut"
                         (lambda () (alloy:handle (make-instance 'alloy:cut-event) self)))
        (om-new-leafmenu "Copy"
                         (lambda () (alloy:handle (make-instance 'alloy:copy-event) self)))
        (om-new-leafmenu "Paste"
                         (lambda ()
                           (let* ((win  (om-view-window self))
                                  (clip (and win (alloy:clipboard win))))
                             (when (stringp clip)
                               (alloy:handle (make-instance 'alloy:paste-event :content clip)
                                             self)))))
        (om-new-leafmenu "Select All"
                         (lambda ()
                           (let ((cursor (alloy:cursor self)))
                             (setf (alloy:anchor cursor) 0)
                             (alloy:move-to :end cursor)
                             (alloy:mark-for-render self))))))

;;; The inherited text-input-component look, with the frame as 4 filled 1 px edge
;;; rects: an outlined rectangle is centred on the bounds and half of it is clipped.
(presentations:define-realization (alld:window om-editable-text)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern :input-background)
  ((:border-top simple:rectangle)    (alloy:margins) :z-index 1)
  ((:border-bottom simple:rectangle) (alloy:margins) :z-index 1)
  ((:border-left simple:rectangle)   (alloy:margins) :z-index 1)
  ((:border-right simple:rectangle)  (alloy:margins) :z-index 1)
  ((:label simple:text)
   (alloy:margins 1)
   alloy:text
   :pattern :input-foreground)
  ((:cursor simple:cursor)
   (presentations:find-shape :label alloy:renderable)
   0
   :composite-mode :source-over
   :pattern :input-foreground)
  ((:selection simple:selection)
   (presentations:find-shape :label alloy:renderable)
   0 0)
  ((:placeholder simple:text)
   (alloy:margins 1)
   (alloy:placeholder alloy:renderable)
   :pattern :placeholder-foreground))

(presentations:define-update (alld:window om-editable-text)
  (:background
   :hidden-p nil
   :pattern (%resolve-pattern alloy:renderable #'om-bg-color *om-white-color*)
   :z-index 0)
  (:border-top
   :hidden-p nil
   :bounds (alld:static-text-edge-bounds alloy:renderable :border-top)
   :pattern (%omcolor->chroma *om-gray-color*))
  (:border-bottom
   :hidden-p nil
   :bounds (alld:static-text-edge-bounds alloy:renderable :border-bottom)
   :pattern (%omcolor->chroma *om-gray-color*))
  (:border-left
   :hidden-p nil
   :bounds (alld:static-text-edge-bounds alloy:renderable :border-left)
   :pattern (%omcolor->chroma *om-gray-color*))
  (:border-right
   :hidden-p nil
   :bounds (alld:static-text-edge-bounds alloy:renderable :border-right)
   :pattern (%omcolor->chroma *om-gray-color*))
  (:selection
   :hidden-p (alld:selection-hidden-p alloy:renderable (om-view-window alloy:renderable))
   :pattern :notice-background
   :z-index 0
   :start (let ((anchor (alloy:anchor (alloy:cursor alloy:renderable))))
            (if anchor (min anchor (alloy:pos (alloy:cursor alloy:renderable))) 0))
   :end (let ((anchor (alloy:anchor (alloy:cursor alloy:renderable))))
          (if anchor (max anchor (alloy:pos (alloy:cursor alloy:renderable))) 0)))
  (:label
   :pattern (%resolve-pattern alloy:renderable #'om-fg-color :input-foreground)
   :z-index 2
   :markup (%selection-markup alloy:renderable)
   :font (%om-font-resolve alloy:renderer (or (om-font alloy:renderable) (%om-text-pane-native-font alloy:renderable)))
   :size (%om-font-draw-unit alloy:renderer (or (om-font alloy:renderable) (%om-text-pane-native-font alloy:renderable)))
   :valign :middle)
  (:cursor
   :hidden-p (alld:caret-hidden-p alloy:renderable (om-view-window alloy:renderable))
   :z-index 3))

(defmethod presentations:update-shape :after
    ((renderer t)
     (renderable om-editable-text)
     (cursor org.shirakumo.alloy.renderers.opengl.msdf::cursor))
  (let* ((wb (alloy:bounds renderable))
         (cb (simple:bounds cursor))
         (wh (alloy:pxh wb))
         (cy (alloy:pxy cb))
         (ch (alloy:pxh cb)))
    (when (or (< cy 0.0) (> (+ cy ch) wh))
      (setf (simple:bounds cursor)
            (alloy:px-extent (alloy:pxx cb)
                             (max 0.0 (/ (- wh ch) 2.0))
                             (alloy:pxw cb)
                             ch)))))

;; alloy:input-line has no native clip; the containment now comes from the
;; om-scroll-mixin render :around (draw.lisp: alloy:render :around for (alloy:renderer om-scroll-mixin)), which clips the same widget.

(defmethod om-view-position :around ((self om-editable-text))
  (om-subtract-points (call-next-method) (om-get-text-edit-pos (om-make-point 0 0))))

(defmethod om-view-size :around ((self om-editable-text))
  (om-subtract-points (call-next-method) (om-get-text-edit-size (om-make-point 0 0))))

(defmethod om-set-view-position ((self om-editable-text) pos-point)
  (call-next-method self (om-get-text-edit-pos pos-point)))

(defmethod om-set-view-size ((self om-editable-text) size-point)
  (call-next-method self (om-get-text-edit-size size-point)))

;;; single line: the scrollable extent is the measured text width; the height never
;;; exceeds the field, so only :horizontal-scroll can ever produce a bar.
(defmethod alld:scroll-content-size ((self om-editable-text))
  (let ((font (or (om-get-font self) (%om-text-pane-native-font self)))
        (text (or (alloy:value self) "")))
    (values (round (if (plusp (length text)) (om-string-size text font) 0))
            (round (om-point-v (om-view-size self))))))

;;; the :cursor, :selection and paren-match shapes take their geometry from the
;;; :label's LOCATION, never from its render offset (alloy renderers/opengl/msdf.lisp: shared-initialize :after for cursor, and
;;; sbcl-lisp-tools/lisp-mode.lisp: %paren-px-bounds), so they must all move
;;; together or the caret and the match cell detach from the text (measured: 20 px).
(defun %apply-text-scroll-offset (self)
  "Stamp SELF's scroll offset on its text shapes; T when one of them changed.
A pane without the paren-match shapes simply has none to stamp."
  (let ((offset (alloy:px-point (- (alld:scroll-x self)) (alld:scroll-y self)))
        (changed nil))
    (dolist (name '(:label :cursor :selection :paren-open :paren-close) changed)
      (let ((shape (presentations:find-shape name self nil)))
        (when shape
          (let ((old (presentations:offset shape)))
            (unless (and (= (alloy:pxx old) (alloy:pxx offset))
                         (= (alloy:pxy old) (alloy:pxy offset)))
              (setf (presentations:offset shape) offset)
              (setf changed t))))))))

(defmethod alld:scroll-applied ((self om-editable-text))
  (%apply-text-scroll-offset self)
  (alloy:mark-for-render self))

(defparameter *text-caret-follow-margin* 2
  "Pixels kept between the caret and the edge of the visible text area.")

(defun %text-caret-follow (self)
  "CAPI parity: a text-input-pane scrolls its own text so the caret stays visible
in a box that never grows -- om-LW has no code for it, it is native behaviour.
Driven by RENDER because the arrows move the caret without writing alloy:value;
the equality guard of alloy-desktop/view/scroll.lisp: set-scroll makes a settled frame a no-op."
  (let ((label (presentations:find-shape :label self nil)))
    (when label
      ;; the shape lags the buffer by a frame, so the caret index can be past its
      ;; text and compute-cursor-location arefs out of range (alloy renderers/opengl/msdf.lisp: compute-cursor-location)
      (handler-bind ((type-error (lambda (c)
                              (format *error-output* "[caret-follow] ~A~%" (princ-to-string c))
                              (return-from %text-caret-follow nil))))
        (alloy:with-unit-parent self
          (let* ((rect    (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location
                           label (alloy:pos (alloy:cursor self))))
                 (caret-x (alloy:pxx rect))
                 (caret-w (alloy:pxw rect))
                 (margin  *text-caret-follow-margin*)
                 (visible (- caret-x (alld:scroll-x self)))
                 ;; the same width the scroll maxima are measured against, so the
                 ;; target can never ask for more than clamp-scroll allows (alloy-desktop/view/scroll.lisp)
                 (view-w  (alld:scroll-viewport self)))
            (cond ((< visible margin)
                   (alld:set-scroll self (- caret-x margin) (alld:scroll-y self)))
                  ((> (+ visible caret-w margin) view-w)
                   (alld:set-scroll self (- (+ caret-x caret-w margin) view-w)
                                (alld:scroll-y self))))))))))

;;; the offset is STATE (alloy-desktop/view/scroll.lisp: the scroll-offset slot of scroll-mixin): shapes rebuilt by realize-renderable
;;; start at zero and must be re-stamped from it before the caret is measured.
(defmethod alloy:render :after ((renderer simple:renderer) (self om-editable-text))
  (when (%apply-text-scroll-offset self) (alloy:mark-for-render self))
  (%text-caret-follow self))

(defun %completion-anchor (pane pos)
  "(values X Y CARET-H) of the caret at POS in PANE's window, alloy y-up. Y is
the caret's BOTTOM: the in-place window hangs from there downwards."
  ;; same aref-out-of-range as %text-caret-follow: POS may be past the shape's text
  (handler-bind ((type-error (lambda (c)
                          (format *error-output* "[completion-anchor] ~A~%" (princ-to-string c))
                          (return-from %completion-anchor nil))))
    (alloy:with-unit-parent pane
      (let* ((label (presentations:find-shape :label pane))
             (rect  (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location label pos))
             ;; rect comes from the text LOCATION, so a scrolled pane must add the
             ;; offset its label is really rendered with (0,0 while it never scrolled)
             (off   (presentations:offset label))
             (gp    (alloy:global-location pane)))
        (values (+ (alloy:pxx gp) (alloy:pxx rect) (alloy:pxx off))
                (+ (alloy:pxy gp) (alloy:pxy rect) (alloy:pxy off))
                (alloy:pxh rect))))))

(defun %text-completion-do-action-p (pane)
  (and (typep pane 'om-editable-text) (complete-do-action pane)))

;;; capi-dialogs-ug-6.htm:118 — once the completion is performed, a pane made with
;;; complete-do-action true invokes its callback.
(defun %insert-completion (pane start end name)
  (alloy::delete-text start end pane)
  (alloy:move-to start (alloy:cursor pane))
  (alloy:insert-text name pane)
  (%om-focus-pane (om-view-window pane) pane)
  (when (%text-completion-do-action-p pane)
    (om-dialog-item-action pane))
  t)

;;; capi-capi-ref-488.htm:183 — an in-place completion function returns a list of
;;; strings, a single string, or the symbol :destroy, which means stop and close
;;; the in-place window; :173 — a completion-function returns NIL, the completed
;;; string, or a list of candidates.  :destroy is a sentinel, never a candidate.
(defun %text-completion-candidates (pane fn)
  "Call the in-place function for PANE's text left of the caret and return
(values COMPLETIONS START END) with the span the result replaces."
  (let* ((text (or (alloy:value pane) ""))
         (pos  (alloy:pos (alloy:cursor pane)))
         (left (subseq text 0 (min pos (length text)))))
    (multiple-value-bind (completions start end) (funcall fn pane left)
      (values completions (or start 0) (or end pos)))))

(defun %trigger-text-completion (pane fn)
  "Invoke completion FN on PANE and act on its value per the CAPI contract."
  ;; FN is the kernel's completion function: its errors belong to the kernel and must
  ;; reach the debugger hook, exactly as the six editor callbacks do
  (progn
    (multiple-value-bind (completions start end) (%text-completion-candidates pane fn)
      (cond
        ((null completions) nil)
        ((eq completions :destroy)
         (%completion-close)
         nil)
        ((stringp completions)
         (%insert-completion pane start end completions))
        ((and (consp completions) (every #'stringp completions))
         (if (null (rest completions))
             (%insert-completion pane start end (first completions))
             (let ((window (om-view-window pane)))
               (when window
                 (%open-completion-popup
                  pane window completions start end
                  :bg-color *om-white-color*
                  :refresh-fn (lambda () (%text-completion-candidates pane fn)))
                 t))))
        (t nil)))))

(defmethod capi::text-input-pane-completion-function ((self om-editable-text))
  (completion-function self))

(defmethod (setf capi::text-input-pane-completion-function)
    (function (self om-editable-text))
  (setf (completion-function self) function))

(defmethod capi::text-input-pane-in-place-complete ((self om-editable-text))
  (let ((function (in-place-completion-function self)))
    (when function
      (%trigger-text-completion self function))))

(defun %completing-text-panes (view)
  "Every om-editable-text under VIEW that was given an in-place completion."
  (let ((found nil))
    (labels ((walk (v)
               (when (and (typep v 'om-editable-text)
                          (in-place-completion-function v))
                 (push v found))
               (dolist (sub (om-subviews v)) (walk sub))))
      (walk view))
    (nreverse found)))

(defun %editing-text-pane (item)
  "The om-editable-text currently editing ITEM.  In CAPI completion belongs to a
text-input-pane; the kernel invokes it on a ttybox (om-static-text), whose editing
is done by the focused pane it opens over the box (kernel controls.lisp: open-ttybox for ttybox)."
  (let* ((window (om-view-window item))
         (strong (and window (%om-strong-pane window))))
    (if (typep strong 'om-editable-text)
        strong
        (let ((candidates (and window (%completing-text-panes window))))
          (when (and candidates (null (rest candidates)))
            (first candidates))))))

;;; capi-capi-ref-494.htm:50-52 — in-place completion belongs to the pane created
;;; with an in-place-completion-function; a static text delegates to that pane.
(defmethod capi::text-input-pane-in-place-complete ((self om-static-text))
  (let ((pane (%editing-text-pane self)))
    (when pane
      (capi::text-input-pane-in-place-complete pane))))

;;; A native text pane collapses an active selection onto its start/end instead of
;;; stepping one character (vanilla panes are capi:text-input-pane and
;;; capi:multi-line-text-input-pane, so this came from the OS); alloy's own handler
;;; only clears the anchor and steps (alloy components/text-input.lisp: handle for (key-down text-input-component)).
(defmethod alloy:handle :around ((event alloy:key-down) (pane alloy:text-input-component))
  (let* ((cursor (alloy:cursor pane))
         (anchor (alloy:anchor cursor))
         (mods   (alloy:modifiers event)))
    (if (and anchor
             (member (alloy:key event) '(:left :right))
             (not (find :shift mods))
             (not (find :control mods)))
        (progn
          (alloy:move-to (if (eql (alloy:key event) :left)
                             (min anchor (alloy:pos cursor))
                             (max anchor (alloy:pos cursor)))
                         cursor)
          (setf (alloy:anchor cursor) nil)
          (alloy:mark-for-render pane)
          t)
        (call-next-method))))

;;; Single :around for this GF+specializers (a second definition would silently
;;; replace this one).  CAPI parity twice over: vanilla text-edit-special-action
;;; (om-LW/dialog-items.lisp: text-edit-special-action) for the two Return keys, and a focused CAPI
;;; text input consumes typing keys while :tab/:escape stay window-level.
(defmethod alloy:handle :around ((event alloy:key-down) (pane om-editable-text))
  (when (%completion-route-key pane event)
    (return-from alloy:handle t))
  (case (alloy:key event)
    ;; CAPI ttyboxes are single-line and BOTH Return keys act: the :enter newline
    ;; branch of text-edit-special-action (om-LW/dialog-items.lisp) reads an internal accessor
    ;; absent from the CAPI manual and never fires on a real text-input-pane.
    ((:enter :return :kp-enter)
     (om-dialog-item-action pane)
     t)
    ;; capi-capi-ref-488.htm:167 binds Tab to the completion-function, and om-LW
    ;; gives every om-editable-text that gesture (om-LW/dialog-items.lisp: the :gesture-callbacks of om-editable-text). A
    ;; pane that carries only the in-place function completes through it.
    (:tab
     (let ((fn (or (completion-function pane) (in-place-completion-function pane))))
       (cond (fn (%trigger-text-completion pane fn) t)
             (t  (call-next-method)))))
    ((:up :down)
     (let ((fn (in-place-completion-function pane)))
       (cond (fn (%trigger-text-completion pane fn) t)
             (t  (call-next-method)))))
    (:escape (call-next-method))
    (t (call-next-method)
       (%completion-after-key pane event)
       t)))

;;; om-text-edit-view — backed by alloy:input-box; multi-line + undo + context menu.

(defclass om-text-edit-view (om-scroll-mixin alloy:input-box om-standard-dialog-item)
  ((dragging-p      :accessor dragging-p      :initform nil)
   (history         :accessor history         :initform (make-instance 'linear-history))
   (previous-text   :accessor previous-text   :initform "")
   (suppress-undo-p :accessor suppress-undo-p :initform nil)
   (goto-line        :accessor goto-line        :initform nil)
   (goto-last-innerh :accessor goto-last-innerh :initform nil)
   ;; vanilla passes :wrap-p straight to the CAPI multi-line pane (kernel
   ;; in-out-boxes.lisp: the :wrap-p of set-input-dialog): the text wraps visually and the extent follows it
   (wrap-p           :accessor wrap-p           :initarg :wrap-p :initform nil)
   (wrap-cache       :accessor %wrap-cache      :initform nil)
   (caret-follow-pos :accessor %caret-follow-pos :initform nil)
   ;; document-anchored scroll (%text-view-anchor-tick): the char index at the
   ;; viewport top, the scroll state it was read from, and the settle detector
   (view-anchor           :accessor %view-anchor           :initform nil)
   (view-anchor-state     :accessor %view-anchor-state     :initform nil)
   (view-anchor-restoring :accessor %view-anchor-restoring :initform nil)
   (view-caret-visible    :accessor %view-caret-visible    :initform nil)
   ;; the line count of the last painted text, so a change in it resizes the pane
   (painted-lines         :accessor %painted-lines         :initform nil)))

;;; A text pane with no font of its own wears the role its native toolkit dresses
;;; that KIND of pane with: Cocoa gives the multi-line view the user font and the
;;; single-line field the system font, Win32 dresses both from DEFAULT_GUI_FONT
;;; (om-LW/dialog-items.lisp: om-text-edit-view, om-editable-text; os-fonts
;;; macos.lisp: query-state, os-fonts windows.lisp: query-state).
(defgeneric %om-text-pane-role (pane)
  (:documentation "Native font role PANE wears while it carries no font of its own.")
  (:method ((pane om-editable-text))  #+darwin :system #-darwin :dialog-default)
  (:method ((pane om-text-edit-view)) #+darwin :text   #-darwin :dialog-default))

(defun %om-text-pane-native-font (pane)
  "The descriptor PANE is drawn and measured with when om-font is NIL."
  (%om-native-font (%om-text-pane-role pane)))

(defvar *pane-text-override* nil
  "Text the pane is being set to, while the pre-write value notification runs.")

(defmethod initialize-instance :around ((self om-text-edit-view)
                                       &rest initargs
                                       &key text (data nil data-p)
                                       &allow-other-keys)
  (declare (ignore data))
  (if (not data-p)
      (apply #'call-next-method self
             :data (make-instance 'alloy:value-data
                                  :value (%normalize-editable-text text t))
             initargs)
      (call-next-method)))

(defmethod initialize-instance :after ((self om-text-edit-view) &key)
  (let ((data (alloy:data self)))
    (when data
      ;; Alloy edits strings through adjustable vectors. Keep the comparison
      ;; baseline detached from that live buffer so every in-place edit is seen.
      (setf (previous-text self)
            (copy-seq (or (alloy:value data) "")))
      (alloy:observe 'alloy:value data
        (lambda (new-value observ)
          (declare (ignore observ))
          (unless (suppress-undo-p self)
            (let ((old (previous-text self))
                  (new (copy-seq (or new-value ""))))
              (unless (string= old new)
                (setf (previous-text self) new)
                ;; the notification runs BEFORE the write, so the pane still holds
                ;; the previous text: hand the incoming one to the buffer sync
                (let ((*pane-text-override* new))
                  (om-mark-modified (om-view-window self)))
                (when (di-action self) (funcall (di-action self) self))
                (commit (make-action
                         (progn
                            (setf (suppress-undo-p self) t)
                            (unwind-protect
                                 (setf (alloy:value (alloy:data self))
                                       (copy-seq new))
                              (setf (previous-text self) (copy-seq new))
                              (setf (suppress-undo-p self) nil)))
                          (progn
                            (setf (suppress-undo-p self) t)
                            (unwind-protect
                                 (setf (alloy:value (alloy:data self))
                                       (copy-seq old))
                              (setf (previous-text self) (copy-seq old))
                              (setf (suppress-undo-p self) nil)))
                         :description (format nil "edit (~D -> ~D chars)" (length old) (length new)))
                        (history self))))))
        :name (gensym "TEXT-VALUE")))
  (alloy:observe 'alloy:focus self
    (lambda (focus observ)
      (declare (ignore observ))
      (when (null focus)
        (om-dialog-item-after-action self)))
    :name (gensym "TEXT-FOCUS"))))

(defun %clamp-edit-cursor (widget)
  "After undo/redo restores a (possibly shorter) snapshot, clamp the cursor and drop
any stale selection so selection/paren render never indexes past the buffer."
  (let ((cursor (and widget (alloy:cursor widget)))
        (len    (length (or (alloy:value widget) ""))))
    (when cursor
      (when (> (alloy:pos cursor) len) (alloy:move-to len cursor))
      (setf (alloy:anchor cursor) nil)
      (alloy:mark-for-render widget))))

;; the undo/redo restore runs under suppress-undo-p, so the value observer skips
;; om-mark-modified: fire it here or the window's text buffer keeps the stale text
(defun om-text-undo (widget)
  (when widget
    (om-history-undo (history widget) t)
    (%clamp-edit-cursor widget)
    (om-mark-modified (om-view-window widget))))

(defun om-text-redo (widget)
  (when widget
    (om-history-redo (history widget) t)
    (%clamp-edit-cursor widget)
    (om-mark-modified (om-view-window widget))))

;;; A text pane draws its own content instead of child elements, so the offset can
;;; not enter through the child translate: it rides on the label shape, which the
;;; presentation renders through (simple:translate renderer (offset shape)).

;;; alloy's text-input-component has no suggest-size method, so the text block has
;;; to be measured with the fork's own font metrics: hard newlines x line height.
(defun %text-wrap-width (self)
  "Pixels available to wrapped text: the view minus the vertical gutter when vertical
scrolling is allowed. Slot-only, so it never recurses back into om-field-size."
  (max 0 (- (om-point-h (om-view-size self))
            (if (alld:scroll-axis-v-p (alld:scrollbars self)) alld:*scrollbar-track-px* 0))))

(defun %wrapped-row-count (line font width)
  "Visual rows LINE occupies wrapped at WIDTH px: greedy on spaces, a word wider than
WIDTH spilling over several rows (vanilla om-LW/dialog-items.lisp: text-wrap-pix)."
  (let ((n (length line)))
    (if (or (<= width 0) (zerop n))
        1
        (let ((rows 1) (run 0f0) (start 0))
          (loop while (< start n)
                do (let* ((sp   (position #\Space line :start start))
                          (end  (if sp (1+ sp) n))
                          (ww   (float (om-string-size (subseq line start end) font) 0f0)))
                     (cond ((<= (+ run ww) width)
                            (incf run ww))
                           ((> ww width)
                            (let ((spill (ceiling ww width)))
                              (incf rows (if (plusp run) spill (1- spill)))
                              (setf run (float (mod ww width) 0f0))))
                           (t (incf rows) (setf run ww)))
                     (setf start end)))
          rows))))

;;; The position-0 caret rectangle IS the renderer's first line box (alloy renderers/opengl/msdf.lisp: the px-extent returned by compute-cursor-location):
;;; it gives the painted line height and the label's own padding, which om-string-h only
;;; estimates -- a fraction of a pixel per line, a whole line over a screenful.
(defun %text-line-box (self)
  "(values PAD-X PAD-TOP LINE-H CARET-W) of SELF's text as the renderer lays it out,
or NIL while the label shape does not exist yet."
  (let ((label (presentations:find-shape :label self nil)))
    (when label
      (alloy:with-unit-parent self
        (let ((r (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location label 0)))
          (values (alloy:pxx r)
                  (- (om-point-v (om-view-size self)) (alloy:pxy r) (alloy:pxh r))
                  (alloy:pxh r)
                  (alloy:pxw r)))))))

(defmethod alld:scroll-content-size ((self om-text-edit-view))
  (let* ((font  (or (om-get-font self) (%om-text-pane-native-font self)))
         (text  (or (alloy:value self) ""))
         (lh    (om-string-h font))
         (wrapw (and (wrap-p self) (%text-wrap-width self)))
         (key   (list text font wrapw lh))
         (hit   (%wrap-cache self)))
    (multiple-value-bind (rows width)
        (if (equal key (first hit))
            (values (second hit) (third hit))
            (let ((start 0) (rows 0) (width 0))
              (loop for pos = (position #\Linefeed text :start start)
                    for line = (subseq text start (or pos (length text)))
                    do (if wrapw
                           (incf rows (%wrapped-row-count line font wrapw))
                           (progn (incf rows)
                                  (setf width (max width (om-string-size line font)))))
                       (when (null pos) (return))
                       (setf start (1+ pos)))
              ;; the key keeps a COPY: alloy edits the value string in place (alloy
              ;; components/text-input.lisp: insert-text for text-input-component), so a
              ;; key holding the live buffer would compare equal to itself forever
              (setf (%wrap-cache self) (list (list (copy-seq text) font wrapw lh) rows width))
              (values rows width)))
      ;; the range has to cover the CARET BOX plus the follow margin, or clamp-scroll
      ;; cuts off exactly what %text-caret-follow-mv aims at; the margin is counted
      ;; TWICE because om-string-size is a CAPI estimate of the widest line, not the
      ;; renderer's measurement.  Wrapped text never overflows horizontally.
      (multiple-value-bind (pad-x pad-top line-h caret-w) (%text-line-box self)
        (let ((slack (* 2 *text-caret-follow-margin*)))
          (values (round (if wrapw
                                 wrapw
                                 (+ (or pad-x 0) width (or caret-w 0) slack)))
                  (round (+ (or pad-top 0)
                                (* (max 1 rows) (or line-h lh))
                                slack))))))))

;; breathing room kept under the last painted line, in painted line heights
(defgeneric %trailing-blank-lines (view)
  (:method ((view t)) 0)
  ;; msdf's line-breaks do not count the empty line a trailing #\Linefeed opens, but
  ;; compute-cursor-location does (alloy renderers/opengl/msdf.lisp: compute-cursor-location, over the msdf-line-height method of alloy-desktop/graphics/text-render.lisp): without one spare line the
  ;; caret at the very end falls outside the block, and outside the clip-view.  A text
  ;; that opens no such line must not gain blank space under it.
  (:method ((view om-text-edit-view))
    (let ((text (alloy:value view)))
      (if (and text (plusp (length text))
               (char= (char text (1- (length text))) #\Linefeed))
          1
          0))))

(defmethod alloy:suggest-size (size (self om-text-edit-view))
  "Ideal height of the text block as PAINTED, plus %trailing-blank-lines of empty space
when the block already overflows the viewport. The renderer measures the block at the
height it paints it (both divide by msdf-em-size of alloy renderers/opengl/msdf.lisp), so the ideal height is taken verbatim.
The pad must never create the overflow
itself: a block that fits keeps reporting the viewport height, or the clip-view's initial
most-negative offset would clamp to a negative value and never return to the end.
The spare space is measured in PAINTED line heights -- the unit compute-cursor-location
steps the caret by -- and carries the caret-follow margin, so the last line is not only
laid out but REACHABLE with the margin %caret-follow-in-clip insists on.  Sized in the
font's om-string-h instead, the block ends 0.45 px short of the caret box and the caret
at the end of the document cannot be scrolled into view."
  (let* ((ideal (call-next-method))
         (label (presentations:find-shape :label self nil))
         (bounds (and label (simple:bounds label))))
    (if (typep bounds 'alloy:margins)
        (alloy:with-unit-parent self
          (let* ((pad     (+ (alloy:pxu bounds) (alloy:pxb bounds)))
                 (block-h (alloy:pxh ideal))
                 ;; transient error while the MSDF atlas/line-breaks warm up -> the
                 ;; font metric still sizes the spare space until the bake lands
                 (lines   (or (ignore-errors
                               (length (org.shirakumo.alloy.renderers.opengl.msdf::line-breaks
                                        label)))
                              0))
                 (line-h  (if (plusp lines)
                              (/ (- block-h pad) lines)
                              (om-string-h (or (om-get-font self) (%om-text-pane-native-font self)))))
                 (view-h  (alloy:pxh size))
                 (n       (%trailing-blank-lines self))
                 (h       (if (and (plusp n) (> block-h view-h))
                              (+ block-h (* n line-h) *text-caret-follow-margin*)
                              block-h)))
            (alloy:px-size (alloy:pxw ideal) (max view-h h))))
        ideal)))

(defmethod alld:scroll-applied ((self om-text-edit-view))
  ;; :cursor and :selection take their geometry from the :label's LOCATION, never from
  ;; its render offset, so all three must carry the same offset or the caret detaches
  ;; from the text it points at (alloy renderers/opengl/msdf.lisp: shared-initialize :after for cursor, same reason as om-editable-text).
  (%apply-text-scroll-offset self)
  (alloy:mark-for-render self))

(defun %caret-follow-own-scroll (self rect)
  "Scroll SELF's own offset until the caret box RECT (pane pixels, y-up, no offset) is
inside the visible band.  Both axes: with wrap-p NIL a long line runs off to the right."
  (let* ((margin *text-caret-follow-margin*)
         (sx     (alld:scroll-x self))
         (sy     (alld:scroll-y self))
         (x      (alloy:pxx rect))
         (w      (alloy:pxw rect))
         (view-h (om-point-v (om-view-size self)))
         (nx sx) (ny sy))
    (multiple-value-bind (vw vh) (alld:scroll-viewport self)
      ;; y-up: the shapes ride +scroll-y, and a horizontal gutter eats the bottom band
      (let ((bottom (+ (alloy:pxy rect) sy))
            (top    (+ (alloy:pxy rect) (alloy:pxh rect) sy))
            (base   (- view-h vh)))
        (cond ((< (- x sx) margin)
               (setf nx (- x margin)))
              ((> (+ (- x sx) w margin) vw)
               (setf nx (- (+ x w margin) vw))))
        (cond ((> top (- view-h margin))
               (setf ny (- sy (- top (- view-h margin)))))
              ((< bottom (+ base margin))
               (setf ny (+ sy (- (+ base margin) bottom)))))
        (alld:set-scroll self nx ny)))))

(defun %caret-follow-in-clip (self cv rect)
  "Scroll the enclosing clip-view CV until SELF's caret box is inside it.  One axis per
call: alloy drops an offset change that moves BOTH axes (alloy layouts/clip-view.lisp: (setf offset) :around for (point clip-view))."
  (alloy:with-unit-parent cv
    (let* ((margin *text-caret-follow-margin*)
           (cg (alloy:global-location cv))
           (cb (alloy:bounds cv))
           (x0 (alloy:pxx cg))
           (y0 (alloy:pxy cg))
           (x1 (+ x0 (alloy:pxw cb)))
           (y1 (+ y0 (alloy:pxh cb)))
           (gp (alloy:global-location self))
           (cx (+ (alloy:pxx gp) (alloy:pxx rect)))
           (cy (+ (alloy:pxy gp) (alloy:pxy rect)))
           (cw (alloy:pxw rect))
           (ch (alloy:pxh rect))
           (dx 0) (dy 0))
      (cond ((< cy (+ y0 margin))        (setf dy (- (+ y0 margin) cy)))
            ((> (+ cy ch) (- y1 margin)) (setf dy (- (- y1 margin) (+ cy ch)))))
      (cond ((< cx (+ x0 margin))        (setf dx (- (+ x0 margin) cx)))
            ((> (+ cx cw) (- x1 margin)) (setf dx (- (- x1 margin) (+ cx cw)))))
      (flet ((shift (ddx ddy)
               (let ((o (alloy:offset cv)))
                 (setf (alloy:offset cv)
                       (alloy:px-point (+ (alloy:pxx o) ddx) (+ (alloy:pxy o) ddy))))))
        (unless (zerop dy) (shift 0 dy))
        (unless (zerop dx) (shift dx 0))
        (unless (and (zerop dx) (zerop dy)) (alloy:mark-for-render cv))))))

(defun %text-caret-follow-mv (self)
  "CAPI parity: a multi-line text pane keeps the caret visible while typing; om-LW carries
no code for it because the CAPI pane does it natively.  Whoever owns the scroll does the
work: inside a clip-view that is the clip, otherwise the pane's own offset."
  (let ((label (presentations:find-shape :label self nil)))
    (when label
      ;; the shape lags the buffer by a frame, so the caret index can be past its text
      ;; and compute-cursor-location arefs out of range (alloy renderers/opengl/msdf.lisp: compute-cursor-location)
      (handler-bind ((type-error (lambda (c)
                                   (format *error-output* "[caret-follow-mv] ~A~%"
                                           (princ-to-string c))
                                   (return-from %text-caret-follow-mv nil))))
        (alloy:with-unit-parent self
          (let ((rect (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location
                       label (alloy:pos (alloy:cursor self))))
                (cv   (alld:find-ancestor-clip-view self)))
            (if cv
                (%caret-follow-in-clip self cv rect)
                (%caret-follow-own-scroll self rect))))))))

;;; Document-anchored scroll.  A refit changes geometry, never WHERE in the document
;;; the reader is, so the state is the char index at the viewport top and the pixel
;;; offset is derived from it: with wrap-p the inner height is a function of the
;;; WIDTH, and a viewport-relative pixel offset therefore names a different line after
;;; a resize.  CAPI's editor
;;; pane keeps the view natively, so om-LW carries no code for it -- same reason as
;;; %text-caret-follow-mv above.

(defun %text-scroll-to-index (pane cv index &optional (lines 1.0))
  "Scroll CV so the visual line holding INDEX sits LINES line-heights below the
viewport top (1.0 puts its top exactly at the top).  Y only: alloy drops an offset
whose two axes both change (alloy layouts/clip-view.lisp: (setf offset) :around for (point clip-view)).  Returns the inner height it laid
out against, or NIL while the text is not baked."
  (let ((label (presentations:find-shape :label pane nil)))
    (when (and label index)
      (alloy:with-unit-parent pane
        (let ((innerh (alloy:pxh (alloy:bounds (alloy:inner cv))))
              ;; transient error while the MSDF atlas/line-breaks warm up -> retry next frame
              (ext    (ignore-errors
                       (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location
                        label index))))
          (when (and ext (plusp innerh))
            (let ((o (alloy:offset cv)))
              (setf (alloy:offset cv)
                    (alloy:px-point (alloy:pxx o)
                                    (- (alloy:pxh (alloy:bounds cv))
                                       (* lines (alloy:pxh ext))
                                       (alloy:pxy ext)))))
            innerh))))))

(defun %text-scroll-anchor-index (pane cv)
  "The char index opening the visual line at CV's viewport top -- the exact inverse of
%text-scroll-to-index with LINES 1.0.  compute-cursor-location's y is linear in the
visual line and its height is constant (alloy renderers/opengl/msdf.lisp: compute-cursor-location, over the msdf-line-height method of alloy-desktop/graphics/text-render.lisp), so line 0 gives both
constants and the line at the top is one division.  NIL while the text is not baked."
  (let ((label (presentations:find-shape :label pane nil)))
    (when label
      (alloy:with-unit-parent pane
        ;; transient error while the MSDF atlas/line-breaks warm up -> retry next frame
        (let* ((breaks (ignore-errors
                        (org.shirakumo.alloy.renderers.opengl.msdf::line-breaks label)))
               (ext    (and breaks (plusp (length breaks))
                            (ignore-errors
                             (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location
                              label 0)))))
          (when (and ext (plusp (alloy:pxh ext)))
            (let* ((line-h (alloy:pxh ext))
                   (cury   (- (alloy:pxh (alloy:bounds cv)) line-h
                              (alloy:pxy (alloy:offset cv))))
                   (line   (max 0 (min (1- (length breaks))
                                       (round (- (alloy:pxy ext) cury) line-h)))))
              (if (zerop line) 0 (aref breaks (1- line))))))))))

(defun %text-caret-in-clip-p (pane cv)
  "Is PANE's caret box inside CV's visible band?  Same composition
%caret-follow-in-clip corrects against, so the two agree on what 'visible' means."
  (let ((label (presentations:find-shape :label pane nil)))
    (when label
      (alloy:with-unit-parent pane
        ;; transient error while the MSDF atlas/line-breaks warm up -> retry next frame
        (let ((rect (ignore-errors
                     (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location
                      label (alloy:pos (alloy:cursor pane))))))
          (when rect
            (let* ((cy (+ (alloy:pxy (alloy:global-location pane)) (alloy:pxy rect)))
                   (y0 (alloy:pxy (alloy:global-location cv)))
                   (y1 (+ y0 (alloy:pxh (alloy:bounds cv)))))
              (and (>= cy y0) (<= (+ cy (alloy:pxh rect)) y1)))))))))

(defun %text-view-scroll-state (pane cv)
  "What a refit moves (inner height, viewport width and height) plus the text length,
which a refit does NOT move: growing text belongs to whoever scrolls for it."
  (alloy:with-unit-parent cv
    (list (alloy:pxh (alloy:bounds (alloy:inner cv)))
          (alloy:pxw (alloy:bounds cv))
          (alloy:pxh (alloy:bounds cv))
          (length (or (alloy:value pane) "")))))

(defun %text-view-anchor-tick (pane)
  "Per frame: record the document line at the viewport top, and while a refit settles
put that line back.  GOTO-LINE has precedence -- it IS the positioned open."
  (let ((cv (alld:find-ancestor-clip-view pane)))
    (when cv
      (let ((state (%text-view-scroll-state pane cv))
            (prev (%view-anchor-state pane)))
        (setf (%view-anchor-state pane) state)
        (cond
          ((goto-line pane)
           (setf (%view-anchor-restoring pane) nil
                 (%view-anchor pane) (%text-scroll-anchor-index pane cv)))
          ((%view-anchor-restoring pane)
           (let* ((innerh (%text-scroll-to-index pane cv (%view-anchor pane)))
                  (settled (and innerh
                                (eql innerh (%view-anchor-restoring pane)))))
             (setf (%view-anchor-restoring pane) (if settled nil (or innerh t)))
             (when (and settled
                        (%view-caret-visible pane)
                        (eql :strong (alloy:focus pane)))
               (%text-caret-follow-mv pane))
             (alloy:mark-for-render cv)))
          ((and prev (%view-anchor pane)
                (not (equal state prev))
                (eql (fourth state) (fourth prev)))
           (setf (%view-anchor-restoring pane)
                 (or (%text-scroll-to-index pane cv (%view-anchor pane)) t))
           (alloy:mark-for-render cv))
          (t
           (setf (%view-anchor pane) (%text-scroll-anchor-index pane cv)
                 (%view-caret-visible pane) (%text-caret-in-clip-p pane cv))))))))

(defun %text-view-anchor-reset (pane)
  "Drop PANE's recorded scroll state so the next tick RECORDS the current offset:
a deliberate repositioning is a new baseline, not a refit to be pulled back."
  (setf (%view-anchor-state pane) nil
        (%view-anchor-restoring pane) nil))

;;; Driven by RENDER because the arrows move the caret without writing alloy:value;
;;; gated on the caret POSITION so a wheel gesture is never yanked back to the caret.
;;; The offset is STATE: shapes rebuilt by realize-renderable start at zero and must be
;;; re-stamped from it before the caret is measured.
(defmethod alloy:render :before ((renderer simple:renderer) (self om-text-edit-view))
  (when (%apply-text-scroll-offset self) (alloy:mark-for-render self))
  (%text-view-anchor-tick self)
  (let ((pos (alloy:pos (alloy:cursor self))))
    (unless (eql pos (%caret-follow-pos self))
      (setf (%caret-follow-pos self) pos)
      (when (alloy:focus self)
        (%text-caret-follow-mv self)))))

(defun %scroll-end-into-view (pane)
  "Refit the enclosing clip-view against the grown text and schedule the re-render.
The shapes are baked before the refit so whoever reads them before the frame sees the
new text; the post-refit bake is the render pass's own, which is why PANE is marked
here whatever the caller did. The shapes carry alloy units, so the unit parent is bound
here: a caller outside a render or event pass has none (alloy units.lisp: the define-unit un)."
  (let ((cv  (alld:find-ancestor-clip-view pane))
        (win (om-view-window pane)))
    (when (and cv win)
      (alloy:with-unit-parent pane
        (presentations:update-shape win pane t)
        (alloy:refit cv))
      (alloy:mark-for-render pane)
      (alloy:mark-for-render cv))))

(defmethod alloy:ensure-visible :around ((pane om-text-edit-view) (cv alloy:clip-view))
  "Suppress alloy's auto-scroll-to-top for text-edit panes that are the inner of a clip-view; %scroll-end-into-view manages position."
  (let ((parent (alloy:layout-parent cv)))
    (when (and parent (not (eq parent cv)))
      (alloy:ensure-visible pane parent))))

(defmethod alloy:handle :after ((event alloy:text-event) (pane om-text-edit-view))
  (alloy:notice-size pane t))

(defmethod alloy:handle :after ((event alloy:key-down) (pane om-text-edit-view))
  (setf (goto-line pane) nil)
  (when (member (alloy:key event) '(:backspace :delete :enter :return :kp-enter))
    (alloy:notice-size pane t)))

;;; CAPI parity: a focused text pane consumes typing keys; :tab/:escape stay
;;; window-level (focus navigation / dialog cancel)
(defmethod alloy:handle :around ((event alloy:key-down) (pane om-text-edit-view))
  (if (member (alloy:key event) '(:tab :escape))
      (call-next-method)
      (progn (call-next-method) t)))

(defmethod alloy:handle ((event alloy:pointer-down) (pane om-text-edit-view))
  (cond ((alloy:contained-p (alloy:location event) pane)
         (setf (gethash pane *text-pointer-down*) :inside)
         t)
        (t
         (setf (gethash pane *text-pointer-down*) :outside)
         (alloy:decline))))

(defmethod alloy:handle ((event alloy:pointer-up) (pane om-text-edit-view))
  (let ((down (gethash pane *text-pointer-down*)))
    (remhash pane *text-pointer-down*)
    (cond ((alloy:contained-p (alloy:location event) pane)
           (%om-focus-pane (om-view-window pane) pane)
           t)
          ((and (eql :strong (alloy:focus pane)) (eq down :outside))
           (%om-unfocus-pane (om-view-window pane) pane)
           (alloy:decline))
          (t (alloy:decline)))))

(defmethod alloy:handle :after ((event alloy:pointer-down) (pane om-text-edit-view))
  (setf (goto-line pane) nil)
  (unless (eq (alloy:kind event) :right)
    (%om-focus-pane (om-view-window pane) pane)
    (let ((cursor (alloy:cursor pane)))
      (setf (alloy:anchor cursor) (alloy:pos cursor)))
    (alloy:mark-for-render pane)
    (setf (dragging-p pane) t)
    (alld:capture-pointer pane)))

(defmethod alloy:handle :around ((event alloy:pointer-down) (pane om-text-edit-view))
  (case (alloy:kind event)
    (:right
     (cond ((alloy:contained-p (alloy:location event) pane)
            (let ((tree (om-get-menu-context pane)))
              (when tree (om-open-pop-up-menu tree pane)))
            t)
           (t
            (when (eql :strong (alloy:focus pane))
              (%om-unfocus-pane (om-view-window pane) pane))
            (alloy:decline))))
    (otherwise (call-next-method))))

(defmethod alloy:handle :before ((event alloy:pointer-move) (pane om-text-edit-view))
  (when (dragging-p pane)
    (%text-drag-autoscroll pane (alloy:location event))
    (let ((new-pos (%text-cursor-pos-from-event pane event)))
      (when new-pos
        (alloy:move-to new-pos (alloy:cursor pane))
        (alloy:mark-for-render pane)))))

(defmethod alloy:handle :around ((event alloy:pointer-up) (pane om-text-edit-view))
  (if (dragging-p pane)
      (progn
        (setf (dragging-p pane) nil)
        (alld:release-pointer-capture pane)
        (let ((cursor (alloy:cursor pane)))
          (when (and (alloy:anchor cursor)
                     (= (alloy:anchor cursor) (alloy:pos cursor)))
            (setf (alloy:anchor cursor) nil)))
        (%om-focus-pane (om-view-window pane) pane)
        t)
      (call-next-method)))

(defmethod alloy:handle ((event alloy:pointer-double-click) (pane om-text-edit-view))
  (cond ((alloy:contained-p (alloy:location event) pane)
         (%select-word-at-cursor pane)
         t)
        (t (alloy:decline))))

(defmethod alloy:handle ((event alloy:paste-event) (pane om-text-edit-view))
  (let ((content (alloy:content event)))
    (typecase content
      (string
       (let ((cursor (alloy:cursor pane)))
         (when (alloy:anchor cursor)
           (alloy::delete-text (min (alloy:anchor cursor) (alloy:pos cursor))
                               (max (alloy:anchor cursor) (alloy:pos cursor))
                               pane)
           (setf (alloy:anchor cursor) nil)))
       (alloy:insert-text (remove #\Return content) pane))
      (T (call-next-method)))))

(defmethod alloy:handle :around ((event alloy:pointer-event) (pane om-text-edit-view))
  ;; the clip hides events aimed at what is scrolled out of sight -- but not from the
  ;; pane that owns the gesture, whose drag must keep tracking the pointer outside
  (let ((clip (alld:find-ancestor-clip-view pane)))
    (if (and clip
             (not (eq pane (alld:pointer-capture (alld:pointer-state))))
             (not (alloy:contained-p (alloy:location event) clip)))
        (alloy:decline)
        (call-next-method))))

(defmethod om-get-menu-context ((self om-text-edit-view))
  (list (om-new-leafmenu "Cut"
                         (lambda () (alloy:handle (make-instance 'alloy:cut-event) self)))
        (om-new-leafmenu "Copy"
                         (lambda () (alloy:handle (make-instance 'alloy:copy-event) self)))
        (om-new-leafmenu "Paste"
                         (lambda ()
                           (let* ((win  (om-view-window self))
                                  (clip (and win (alloy:clipboard win))))
                             (when (stringp clip)
                               (alloy:handle (make-instance 'alloy:paste-event :content clip)
                                             self)))))
        (om-new-leafmenu "Select All"
                         (lambda ()
                           (let ((cursor (alloy:cursor self)))
                             (setf (alloy:anchor cursor) 0)
                             (alloy:move-to :end cursor)
                             (alloy:mark-for-render self))))))

(defmethod om-dialog-item-text ((self om-text-edit-view))
  (copy-seq (or (alloy:value self) "")))

;; (setf alloy:text) clamps cursor and anchor to the new length
;; (alloy components/text-input.lisp: (setf text) :after for (t text-input-component)); a raw value write leaves them stale
(defmethod om-set-dialog-item-text ((self om-text-edit-view) text)
  (setf (alloy:text self)
        (%normalize-editable-text text t))
  text)

(defmethod special-bg ((self om-text-edit-view))
  *om-white-color*)

(defmethod di-set-focus ((self om-text-edit-view))
  (when (alloy:focus-tree self)
    (%om-focus-pane (om-view-window self) self)
    (let ((cursor (alloy:cursor self)))
      (setf (alloy:anchor cursor) 0)
      (alloy:move-to :end cursor))
    (alloy:mark-for-render self)))

(defmethod om-copy-command ((self om-text-edit-view))
  (alloy:handle (make-instance 'alloy:copy-event) self))

(defmethod om-cut-command ((self om-text-edit-view))
  (alloy:handle (make-instance 'alloy:cut-event) self))

(defmethod om-paste-command ((self om-text-edit-view))
  (let* ((win  (om-view-window self))
         (clip (and win (alloy:clipboard win))))
    (when (stringp clip)
      (alloy:handle (make-instance 'alloy:paste-event :content clip) self))))

(defmethod om-set-font :after ((self om-text-edit-view) font)
  (%apply-font-to-label-shape self (or font (%om-text-pane-native-font self))))

(presentations:define-update (alld:window om-text-edit-view)
  (:background
   :hidden-p nil
   :pattern (%resolve-pattern alloy:renderable #'om-bg-color *om-white-color*)
   :z-index -3)
  (:border
   :hidden-p nil
   :pattern (%omcolor->chroma *om-gray-color*)
   :line-width (alloy:un 1)
   :z-index -2)
  (:selection
   :hidden-p (alld:selection-hidden-p alloy:renderable (om-view-window alloy:renderable))
   :pattern :notice-background
   :z-index 0
   :start (let ((anchor (alloy:anchor (alloy:cursor alloy:renderable))))
            (if anchor (min anchor (alloy:pos (alloy:cursor alloy:renderable))) 0))
   :end (let ((anchor (alloy:anchor (alloy:cursor alloy:renderable))))
          (if anchor (max anchor (alloy:pos (alloy:cursor alloy:renderable))) 0)))
  (:label
   :pattern (%resolve-pattern alloy:renderable #'om-fg-color :input-foreground)
   :z-index 2
   :markup (%selection-markup alloy:renderable)
   :wrap (wrap-p alloy:renderable)
   :font (%om-font-resolve alloy:renderer (or (om-font alloy:renderable) (%om-text-pane-native-font alloy:renderable)))
   :size (%om-font-draw-unit alloy:renderer (or (om-font alloy:renderable) (%om-text-pane-native-font alloy:renderable))))
  ;; also the caret of every lisp-tools pane (gutter / paren / lisp-edit / repl /
  ;; eval-output subclass this one), since update-shape-arguments is a PROGN
  ;; combination and their own updates carry no :cursor key to override it
  (:cursor
   :hidden-p (alld:caret-hidden-p alloy:renderable (om-view-window alloy:renderable))
   :z-index 3))

;;; :selection is painted UNDER the text (z-index 0 against the label's 2) and
;;; update-shape T walks the shapes already sorted by z-index (presentations
;;; protocol.lisp: update-shape for (renderer renderable (eql T))), so the rects bake against the PREVIOUS pass's label --
;;; one pass behind its location and line-breaks (alloy renderers/opengl/msdf.lisp: shared-initialize :after for text).  After a growth
;;; refit that lag paints the selection off the text it selects.  Re-stamp it once
;;; this pass's label is in place (same fix as the gutter, lisp-mode.lisp: presentations:update-shape :after for om-gutter-edit-view-widget).
;;;
;;; The enclosing clip-view sizes the pane from this same label (alloy:suggest-size for
;;; om-text-edit-view), and its refit runs before the pane's render bakes a new text: a text
;;; written by any path leaves the extent one text behind. A bake that changes the line count
;;; therefore has the pane notice its size on the next UI tick -- a mark set inside the render is
;;; cleared when the render ends -- and that frame refits against the painted text.
(defmethod presentations:update-shape :after
    ((renderer t) (renderable om-text-edit-view) (all (eql t)))
  (when (alloy:anchor (alloy:cursor renderable))
    (let ((selection (presentations:find-shape :selection renderable nil)))
      (when selection
        (alloy:with-unit-parent renderable
          (presentations:update-shape renderer renderable selection)))))
  (let* ((label (presentations:find-shape :label renderable nil))
         ;; transient error while the MSDF atlas/line-breaks warm up -> retry on the next bake
         (lines (and label
                     (ignore-errors
                      (length (org.shirakumo.alloy.renderers.opengl.msdf::line-breaks label))))))
    (when (and lines (not (eql lines (%painted-lines renderable))))
      (setf (%painted-lines renderable) lines)
      (alld:post-to-ui-thread (lambda () (alloy:notice-size renderable t))))))

(defmethod presentations:update-shape :after
    ((renderer t)
     (renderable om-text-edit-view)
     (cursor org.shirakumo.alloy.renderers.opengl.msdf::cursor))
  (let* ((widget-bounds (alloy:bounds renderable))
         (cursor-bounds (simple:bounds cursor))
         (wh (alloy:pxh widget-bounds))
         (cy (alloy:pxy cursor-bounds))
         (ch (alloy:pxh cursor-bounds))
         (max-cy (max 0 (- wh ch))))
    (when (> (+ cy ch) wh)
      (setf (simple:bounds cursor)
            (alloy:px-extent (alloy:pxx cursor-bounds)
                             max-cy
                             (alloy:pxw cursor-bounds)
                             ch)))))

(defun om-make-edit-view (initial-text &rest args &key &allow-other-keys)
  (apply #'om-make-dialog-item 'om-text-edit-view
         (om-make-point 0 0) (om-make-point 200 100) initial-text args))

(defun om-scroll-text-edit-view (self) (%scroll-end-into-view self))

;;; om-check-box — the library check box (alloy-desktop/widgets/controls.lisp: check-box)
;;; carrying the OM text, state and action.

(defclass om-check-box (alld:check-box om-standard-dialog-item) ())

(defmethod om-checked-p ((self om-check-box)) (alloy:value self))

(defmethod om-set-check-box ((self om-check-box) check?)
  (setf (alloy:value self) (and check? t))
  (alloy:mark-for-render self)
  check?)

(defmethod om-dialog-item-text ((self om-check-box)) (alld:indicator-label self))

(defmethod om-set-dialog-item-text ((self om-check-box) text)
  (setf (alld:indicator-label self) (or text ""))
  (alloy:mark-for-render self)
  text)

(defmethod alld:indicator-activated ((self om-check-box))
  (om-dialog-item-action self))

;;; A click off the indicator reaches the enclosing view, so a dialog-item box gets
;;; selected by its label in a patch (kernel boxframe.lisp: om-view-click-handler for omboxframe). Windows are not
;;; om-views (window.lisp: defclass om-look-window) and plain views decline (actions.lisp: the alloy:decline of handle for (pointer-down om-view)), so the
;;; dialog/preferences behaviour is unchanged.
(defun %di-click-through-to-container (self event)
  (let ((container (vcontainer self)))
    (when (typep container 'om-view)
      (let ((pos (%alloy-location-to-om-point (alloy:location event) container)))
        (%capture-gesture-pane container)
        (setf (alld:clicked-element (alld:pointer-state)) container)
        (or (om-view-click-handler container pos)
            (progn (alld:release-pointer-capture container)
                   (setf (alld:clicked-element (alld:pointer-state)) nil)
                   nil))))))

(defun %di-release-through-to-container (self event)
  "Close a press routed by %di-click-through-to-container (actions.lisp: handle for (pointer-down om-view))."
  (let ((container (vcontainer self)))
    (when (eql (alld:clicked-element (alld:pointer-state)) container)
      (om-click-release-handler
       container (%alloy-location-to-om-point (alloy:location event) container))
      (setf (alld:clicked-element (alld:pointer-state)) nil)
      t)))

(defmethod alld:click-outside-indicator ((self om-standard-dialog-item) event)
  (unless (and (eql :left (alloy:kind event))
               (%di-click-through-to-container self event))
    (alld:focus-on-click self)))

(defmethod alld:release-outside-indicator ((self om-standard-dialog-item) event)
  (%di-release-through-to-container self event))

;;; om-radio-button — the library radio button (alloy-desktop/widgets/controls.lisp:
;;; radio-button); kernel cluster semantics.

(defclass om-radio-button (alld:radio-button om-standard-dialog-item) ())

(defmethod om-radio-button-p ((self om-radio-button)) t)
(defmethod om-radio-button-p ((self t)) nil)

(defmethod om-checked-p ((self om-radio-button)) (alloy:value self))

(defmethod om-set-check-box ((self om-radio-button) check?)
  (setf (alloy:value self) (and check? t))
  (alloy:mark-for-render self)
  check?)

(defmethod om-dialog-item-text ((self om-radio-button)) (alld:indicator-label self))

(defmethod om-set-dialog-item-text ((self om-radio-button) text)
  (setf (alld:indicator-label self) (or text ""))
  (alloy:mark-for-render self)
  text)

;;; a cluster is exclusive among the kernel's subviews of the radio's container
(defmethod alld:radio-cluster-candidates ((self om-radio-button))
  (let ((container (vcontainer self)))
    (and container (vsubviews container))))

;;; om-dialog-item-action is public and a direct call selects too, so exclusivity runs here;
;;; a click or an activation reaches it through alld:indicator-activated below
(defmethod om-dialog-item-action ((self om-radio-button))
  (alld:uncheck-radio-cluster-siblings self)
  (call-next-method))

(defmethod alld:indicator-activated ((self om-radio-button))
  (om-dialog-item-action self))

;;; om-slider — the library slider (alloy-desktop/widgets/controls.lisp: slider).

(defclass om-slider (alld:slider om-standard-dialog-item) ())

(defmethod om-slider-value ((self om-slider))
  (* (round (alloy:value self) (alld:slider-increment self)) (alld:slider-increment self)))

(defmethod om-slider-increment ((self om-slider)) (alld:slider-increment self))

(defmethod om-set-slider-value ((self om-slider) value)
  (alld:with-programmatic-set
    (setf (alloy:value self) value)))

(defmethod om-get-slider-range ((self om-slider))
  (let ((r (alloy:range self)))
    (list (car r) (cdr r))))

(defmethod om-get-slider-orientation ((self om-slider))
  (alloy:orientation self))

;;; the light bevel follows the background preference, written at runtime
;;; (kernel graphics/default-prefs.lisp: the :after-fun of the general bg om-color-view)
(defmethod alld:slider-bevel-light ((self om-slider))
  (%omcolor->chroma *om-white-color*))

;;; CAPI commits the slider value and only then calls the callback; observables
;;; notifies :BEFORE the write (observables protocol.lisp: macro define-observable), so the commit point
;;; is this :after, which already sees the clamp applied
;;; (alloy components/slider.lisp: (setf value) :around for (t slider)).
(defmethod (setf alloy:value) :after (new-value (self om-slider))
  (declare (ignore new-value))
  (unless alld:*programmatic-set* (om-dialog-item-action self)))

;;; om-item-list — single/multi selection lists over the library lists
;;; (alloy-desktop/widgets/choice.lisp: item-list, single-item-list, multi-item-list);
;;; OM keeps the class of the rows, the defaults that name OM functions and the
;;; exported list API.

(defclass om-list-label (alld:list-label om-standard-dialog-item) ())

;;; om-scroll-mixin first in the CPL so its scroll methods outrank the
;;; om-graphic-object fallbacks; :scrollbars is what om-make-dialog-item passes.
;;; The callbacks CAPI's choice gets by default here name OM functions
;;; (om-LW/dialog-items.lisp: the :default-initargs of om-item-list).
(defclass om-item-list (om-scroll-mixin alld:item-list om-standard-dialog-item)
  ((on-change-fn :initarg :on-change-fn :initform nil :accessor on-change-fn))
  (:default-initargs
   :callback-type '(:collection)
   :selection-callback 'om-dialog-item-action
   :action-callback 'double-click-on-list
   :test-function 'string-equal))

(defclass om-single-item-list (alld:single-item-list om-item-list) ()
  (:default-initargs
   :retract-callback 'item-list-unselect))

;;; vanilla om-LW/dialog-items.lisp: the defclass om-multi-item-list -- both extend
;;; and retract report through the OM action.
(defclass om-multi-item-list (alld:multi-item-list om-item-list) ()
  (:default-initargs
   :extend-callback 'om-dialog-item-action
   :retract-callback 'om-dialog-item-action))

(defmethod alld:list-changed ((list om-item-list) value)
  (let ((fn (on-change-fn list)))
    (when fn (funcall fn value))))

(defmethod special-bg ((self om-single-item-list))
  *om-white-color*)
(defmethod special-bg ((self om-multi-item-list))
  *om-white-color*)

;;; the rows are OM dialog items: an OM font and the dialog-item elevation
(defmethod alloy::represent-for ((component om-item-list) data &rest initargs)
  (apply #'alloy:represent-with 'om-list-label data initargs))

;;; selecting a row hands the keyboard to the list through the OM focus policy
(defmethod alld:choice-focus-pane ((list om-item-list))
  (%om-focus-pane (om-view-window list) list))

;;; the window a callback is told about is the OM window of the pane, and the focus
;;; the one of its OM content focus list
(defmethod alld:callback-argument ((choice om-standard-dialog-item) (spec (eql :interface)))
  (om-view-window choice))

(defmethod alld:callback-argument ((choice om-standard-dialog-item) (spec (eql :focus)))
  (let ((focus-list (om-window-focus (om-view-window choice))))
    (and focus-list (alloy:focused focus-list))))

;;; a row measures its text the way every OM string is measured
(defmethod alld:list-label-text-width ((self om-list-label) text)
  (om-string-size text (or (om-font self) (%om-native-font :dialog-default))))

(defmethod om-set-font :after ((self om-list-label) font)
  (%apply-font-to-label-shape self (or font (%om-native-font :dialog-default))))

;;; vanilla's default :action-callback (om-LW/dialog-items.lisp: the :action-callback of om-item-list, double-click-on-list for om-item-list); projects specialize it
;;; (projects musicproject/analysis/scoreeditor-analysis.lisp: oa::double-click-on-list for select-class-item-list).
(defgeneric double-click-on-list (list))
(defmethod double-click-on-list ((list om-item-list))
  (om-dialog-item-after-action list))

;;; vanilla's default :retract-callback for a single-selection list
;;; (om-LW/dialog-items.lisp: the defclass om-single-item-list and its
;;; item-list-unselect):
;;; an emptied selection falls back to the first row.
(defgeneric item-list-unselect (list))
(defmethod item-list-unselect ((list om-single-item-list))
  (unless (om-get-selected-item-index list)
    (om-set-selected-item-index list 0)))

;;; The exported list API: the library list under the programmatic flag, since the
;;; vanilla setters are plain setf that CAPI never reports to the callbacks
;;; (om-LW/dialog-items.lisp: om-set-selected-item-index for om-item-list).

(defgeneric om-get-item-list (list))
(defgeneric om-set-item-list (list items))

(defmethod om-get-item-list ((list om-single-item-list))
  (alld:items list))

(defmethod om-get-item-list ((list om-multi-item-list))
  (alld:items list))

;;; vanilla returns capi choice-selection, which is NIL for an empty list
;;; (om-LW/dialog-items.lisp: om-get-selected-item-index for om-item-list) -- the
;;; library resets the selection to match.
(defmethod om-set-item-list ((list om-single-item-list) items)
  (alld:set-items list items))

(defmethod om-set-item-list ((list om-multi-item-list) items)
  (alld:set-items list items))

(defun (setf om-get-item-list) (items list) (om-set-item-list list items))

(defgeneric om-get-selected-item-index (list))
(defmethod om-get-selected-item-index ((list om-single-item-list)) (alld:selected-index list))
(defmethod om-get-selected-item-index ((list om-multi-item-list)) (alld:selected-indices list))

(defgeneric om-set-selected-item-index (list value))
(defmethod om-set-selected-item-index ((list om-single-item-list) value)
  (alld:with-programmatic-set (alld:set-selected-index list value)))
(defmethod om-set-selected-item-index ((list om-multi-item-list) value)
  (alld:with-programmatic-set (alld:set-selected-indices list value)))

(defgeneric om-select-item-index (list index))
(defmethod om-select-item-index ((list om-single-item-list) index)
  (alld:with-programmatic-set (alld:select-index list index)))
(defmethod om-select-item-index ((list om-multi-item-list) index)
  (alld:with-programmatic-set (alld:select-index list index)))

(defgeneric om-unselect-item-index (list index))
(defmethod om-unselect-item-index ((list om-single-item-list) index)
  (alld:with-programmatic-set (alld:unselect-index list index)))
(defmethod om-unselect-item-index ((list om-multi-item-list) index)
  (alld:with-programmatic-set (alld:unselect-index list index)))

(defgeneric om-get-selected-item (list))
(defmethod om-get-selected-item ((list om-single-item-list))
  (alld:selected-item list))
(defmethod om-get-selected-item ((list om-multi-item-list))
  (alld:selected-item list))

;;; vanilla resolves the item through the widget's :test-function, which CAPI applies for
;;; (setf choice-selected-item) -- 'string-equal for a list-panel
;;; (om-LW/dialog-items.lisp: the :test-function of the defclass om-item-list, and
;;; om-set-selected-item for om-single-item-list and for om-multi-item-list).
(defgeneric om-set-selected-item (list value))
(defmethod om-set-selected-item ((list om-single-item-list) value)
  (alld:with-programmatic-set (alld:set-selected-item list value)))
(defmethod om-set-selected-item ((list om-multi-item-list) value)
  (alld:with-programmatic-set (alld:set-selected-item list value)))

;;; om-pop-up-dialog-item — the library pop-up (alloy-desktop/widgets/choice.lisp:
;;; pop-up-dialog-item); its rows are OM graphic objects.

(defclass om-combo-item (alld:combo-item om-graphic-object) ())

(defmethod alld:widget-font-entry ((self om-combo-item) renderer)
  (%om-font-entry renderer (or (om-font self) (%om-native-font :dialog-default))))

(defclass om-pop-up-dialog-item (alld:pop-up-dialog-item om-standard-dialog-item)
  ((value :initform nil :initarg :value :accessor value))
  (:default-initargs
   :callback-type '(:collection)
   :selection-callback 'om-dialog-item-action
   :test-function 'equal))

(defmethod alloy:combo-item ((item t) (combo om-pop-up-dialog-item))
  (make-instance 'om-combo-item :value item))

(defmethod alloy:combo-item ((item cons) (combo om-pop-up-dialog-item))
  (make-instance 'om-combo-item :value (car item) :display-text (cdr item)))

;;; the chevron mark follows the background preference, written at runtime
;;; (kernel graphics/default-prefs.lisp: the :after-fun of the general bg om-color-view)
(defmethod alld:popup-chevron-mark ((self om-pop-up-dialog-item))
  (%omcolor->chroma *om-white-color*))

(defparameter *om-popup-chevron-hover-color* (om-make-color 0.2 0.62 0.93))

(defmethod om-get-item-list ((self om-pop-up-dialog-item))
  (coerce (alloy:value-set self) 'list))

(defmethod om-set-item-list ((self om-pop-up-dialog-item) items)
  (setf (alloy:value-set self) items)
  items)

;;; CAPI prints a collection pane as #<class [N items] <address>>
(macrolet ((collection-printer (class)
             `(defmethod print-object ((self ,class) stream)
                (print-unreadable-object (self stream :type t :identity t)
                  (format stream "[~D items] " (length (om-get-item-list self)))))))
  (collection-printer om-single-item-list)
  (collection-printer om-multi-item-list)
  (collection-printer om-pop-up-dialog-item))

;;; specialized per concrete class: the alloy superclasses precede om-standard-dialog-item
;;; in the CPL, so alloy's own print-object shadows a method on the mixin
(macrolet ((text-printer (class)
             `(defmethod print-object ((self ,class) stream)
                (print-unreadable-object (self stream :type t :identity t)
                  (let ((text (di-text self)))
                    (when (and text (plusp (length text)))
                      (format stream "~S " text)))))))
  (text-printer om-button)
  (text-printer om-check-box)
  (text-printer om-radio-button)
  (text-printer om-editable-text)
  (text-printer om-text-edit-view)
  (text-printer om-slider))

(defmethod om-get-selected-item ((self om-pop-up-dialog-item))
  (alloy:value self))

(defmethod om-get-selected-item-index ((self om-pop-up-dialog-item))
  (position (alloy:value self) (om-get-item-list self) :test (alld:test-function self)))

(defmethod om-set-selected-item-index ((self om-pop-up-dialog-item) pos)
  (alld:with-programmatic-set
    (let ((items (om-get-item-list self)))
      (when (and pos (< -1 pos (length items)))
        (setf (alloy:value self) (nth pos items)))))
  pos)

(defmethod om-set-selected-item ((self om-pop-up-dialog-item) str)
  ;; vanilla resolves the item by string-equal (om-LW/dialog-items.lisp:
  ;; om-set-selected-item for om-pop-up-dialog-item), so a keyword matches
  (alld:with-programmatic-set
    (let* ((items (om-get-item-list self))
           (pos   (%popup-item-position str items)))
      (setf (alloy:value self) (if pos (nth pos items) str))))
  str)

(defmethod om-select-item-index ((self om-pop-up-dialog-item) index)
  (om-set-selected-item-index self index))

(defmethod om-set-font :after ((self om-combo-item) font)
  (%apply-font-to-label-shape self (or font (%om-native-font :dialog-default))))

(defmethod om-set-font :after ((self om-pop-up-dialog-item) font)
  (%apply-font-to-label-shape self (or font (%om-native-font :dialog-default))))

;;; The dropdown is not alloy's in-window combo-list any more: it is the shared popup
;;; chain, so it inherits the alld:popup-frame ring (see alloy-desktop/widgets/choice-popup.lisp: popup-open-choice-list).

;;; Multi-line selection rectangles for input-box panes (line-aware).

(defun %selection-measure-end-no-linefeed (string pos)
  (if (and (< 0 pos (1+ (length string)))
           (char= #\Linefeed (char string (1- pos))))
      (1- pos)
      pos))

(defmethod shared-initialize :after
    ((selection org.shirakumo.alloy.renderers.opengl.msdf::selection) slots &key)
  (declare (ignore slots))
  (let ((rects (org.shirakumo.alloy.renderers.opengl.msdf::line-rectangles selection))
        (start (simple:start selection))
        (end   (simple:end selection)))
    (setf (fill-pointer rects) 0)
    (when (/= start end)
      (let* ((text   (simple:text-object selection))
             (string (alloy:text text))
             (breaks (org.shirakumo.alloy.renderers.opengl.msdf::line-breaks text))
             (font   (org.shirakumo.alloy.renderers.opengl.msdf::data (simple:font text)))
             ;; one rectangle per line of the SHAPE, so it walks the pitch the glyph
             ;; pass walks (alloy-desktop/graphics/font-metrics.lisp: line-advance, behind the msdf-line-height
             ;; method of alloy-desktop/graphics/text-render.lisp that map-glyphs and compute-cursor-location of alloy
             ;; renderers/opengl/msdf.lisp call) and never the baked line height:
             ;; a rectangle on its own pitch drifts a fraction of a pixel per line
             ;; until the white selected text lands beside its blue.
             (px     (alloy:to-px (simple:size text)))
             (pitch  (alld:line-advance (simple:font text) px))
             (loc    (org.shirakumo.alloy.renderers.opengl.msdf::location text))
             (lx     (alloy:pxx loc))
             (ly     (alloy:pxy loc)))
        (loop for line from 0 below (length breaks)
              for line-start = (if (zerop line) 0 (aref breaks (1- line)))
              for line-end   = (aref breaks line)
              for sel-start  = (max start line-start)
              for sel-end    = (min end line-end)
              when (< sel-start sel-end)
                do (let* ((measure-end (%selection-measure-end-no-linefeed string sel-end))
                          ;; the horizontal grid the glyph pass walks
                          ;; (alloy-desktop/graphics/font-metrics.lisp: pen-advance), so the blue band
                          ;; ends exactly where the selected ink does
                          (x-left  (alld:pen-advance font string line-start sel-start px))
                          (x-right (alld:pen-advance font string line-start measure-end px))
                          (x1 (+ lx x-left))
                          (x2 (+ lx x-right))
                          (y  (- ly (* pitch line)))
                          (h  pitch)
                          (rect (alloy:px-extent x1 y (max 0 (- x2 x1)) h)))
                     (vector-push-extend rect rects)))))))

;;; Constructor — kernel calls (om-make-dialog-item 'class position size text …).

(defun %make-di-instance (class text extra &key checked-p range (value 0) (direction :horizontal)
                                                radio-button-cluster scrollbars)
  "EXTRA is the caller's whole initarg plist, appended like vanilla
om-make-dialog-item does (om-LW/dialog-items.lisp: the make-instance of om-make-dialog-item, :allow-other-keys t)."
  (flet ((make (&rest fixed)
           ;; vanilla hands :text/:items/:horizontal-scroll to EVERY pane (om-LW
           ;; dialog-items.lisp: the fixed initargs of om-make-dialog-item); a class without the slot ignores
           ;; them, and a fixed initarg outranks the caller's (leftmost wins)
           (apply #'make-instance class
                  (append fixed
                          (list :text text :items range :scrollbars scrollbars
                                :allow-other-keys t)
                          extra))))
    (cond
      ((subtypep class 'om-button)
       (make :on-activate (constantly nil)))
      ((subtypep class 'om-check-box)
       (make :data (make-instance 'alloy:value-data :value (and checked-p t))
             :label-text (or text "")))
      ((subtypep class 'om-radio-button)
       (make :data (make-instance 'alloy:value-data :value (and checked-p t))
             :label-text (or text "")
             :radio-button-cluster radio-button-cluster))
      ((subtypep class 'om-slider)
       (make :data (make-instance 'alloy:value-data :value value)
             :range (cons (or (first range) 0) (or (second range) 100))
             :orientation direction))
      ((subtypep class 'om-pop-up-dialog-item)
       ;; vanilla pre-selects the range item string-equal to :value (om-LW/dialog-items.lisp: initialize-instance :after for om-pop-up-dialog-item);
       ;; the kernel stores keywords there (projects midi/tools/midi-preferences.lisp: make-new-pref-scroll for (eql :midi)), not strings
       (let* ((pos     (%popup-item-position value range))
              (initial (if pos (nth pos range) (first range))))
         (make :data (make-instance 'alloy:value-data
                                    :value (if (consp initial) (car initial) initial))
               :value-set range)))
      ((subtypep class 'om-item-list)
       (make :data (make-instance 'alloy::list-data :list range)
             :cell-margins (alloy:margins 0)))
      ((subtypep class 'om-editable-text)
       (make :data
             (make-instance 'alloy:value-data
                            :value (%normalize-editable-text
                                    text (getf extra :allow-returns)))))
      ((subtypep class 'om-text-edit-view)
       (make :data
             (make-instance 'alloy:value-data
                            :value (%normalize-editable-text text t))))
      (t
       (make)))))

(defun om-make-dialog-item (class position size text &rest attributes
                            &key container font bg-color fg-color (enable t)
                                 di-action after-action modify-action
                                 default-button cancel-button focus
                                 checked-p range (value 0) (direction :horizontal)
                                 radio-button-cluster allow-returns (scrollbars nil)
                                 completion-function in-place-completion-function
                                 complete-do-action
                            &allow-other-keys)
  ;; allow-returns is declared for signature parity with vanilla
  ;; (om-LW/dialog-items.lisp: om-make-dialog-item); the
  ;; om-editable-text slot of that name is fed from the attribute plist below
  (declare (ignorable allow-returns))
  (let* ((position (if (subtypep class 'om-editable-text)
                       (om-get-text-edit-pos position)
                       position))
         (size     (if (subtypep class 'om-editable-text)
                       (om-get-text-edit-size size)
                       size))
         ;; the caller's WHOLE attribute plist flows to make-instance like vanilla
         ;; (om-LW/dialog-items.lisp: the attributes append of om-make-dialog-item): kernel subclasses read :value/:enable/... there
         (extra (loop for (k v) on attributes by #'cddr
                      ;; :focus is withheld: alloy focus-element takes an initarg of that
                      ;; name (alloy focus-tree.lisp: the :focus initarg of defclass focus-element) and T is not a valid focus value
                      unless (eq k :focus)
                        append (list k v)))
         (di (%make-di-instance class (or text "") extra
                                :checked-p checked-p :range range :value value
                                :direction direction :radio-button-cluster radio-button-cluster
                                :scrollbars scrollbars)))
    (setf (vx di) (om-point-h position)
          (vy di) (om-point-v position)
          (vw di) (om-point-h size)
          (vh di) (om-point-v size))
    (when di-action (setf (di-action di) di-action))
    (when modify-action
      (setf (dialog-item-after-fun di) modify-action
            (di-action di) modify-action))
    (when after-action (setf (dialog-item-after-fun di) after-action))
    (when (and default-button (typep di 'om-button))
      (setf (alld:button-default-p di) t))
    (when (and cancel-button (typep di 'om-button))
      (setf (alld:button-cancel-p di) t))
    (when (and completion-function (typep di 'om-editable-text))
      (setf (completion-function di) completion-function))
    (when (and in-place-completion-function (typep di 'om-editable-text))
      (setf (in-place-completion-function di) in-place-completion-function))
    (when (and complete-do-action (typep di 'om-editable-text))
      (setf (complete-do-action di) complete-do-action))
    ;; unconditional like vanilla (om-LW/dialog-items.lisp: om-make-dialog-item):
    ;; the om-static-text :around turns a NIL
    ;; font into *om-default-font1*, so om-get-font is never NIL for a static text
    (om-set-font di font)
    (when fg-color (om-set-fg-color di fg-color))
    (let ((bg (or bg-color (special-bg di))))
      (when bg (om-set-bg-color di bg)))
    (om-enable-dialog-item di enable)
    (when container (om-add-subviews container di))
    (di-after-settings di)
    (when focus (di-set-focus di))
    di))


;;; Mnemonics. LispWorks picks the letter itself for every push button, check box and
;;; radio button of an interface -- the kernel passes none anywhere -- and it does so
;;; when the native control is created, resolving collisions across the whole window.
;;; The rule below was read off the live vanilla, from the `&` LispWorks writes into
;;; the Win32 control text: first character of each word in word order, then any other
;;; alphanumeric character, first free one wins, case-insensitively unique per window,
;;; blank title gets none. It reproduces the ten controls of the Preferences General
;;; page exactly, down to the lowercase r of "Enable &reactivity" and the s of
;;; "Re&store...".
;;; Where the vanilla RE-SOLVES the whole interface every time another tab page is
;;; realized -- which strips OK and Cancel of their letters once a page claims O and
;;; C -- the fork solves over what is on screen. That churn is an artefact of the
;;; LispWorks solver, not a behaviour to reproduce.

(defun %mnemonic-item-p (item)
  (or (typep item 'om-button)
      (typep item 'om-check-box)
      (typep item 'om-radio-button)))

(defun %mnemonic-items (view)
  "The push buttons, check boxes and radio buttons VIEW shows, in creation order.
Two sources, subviews first so the letters already solved never move. The subview
tree covers everything built with om-add-subviews; what it cannot see are the
controls the score editors enter straight into an alloy layout and into the window's
focus root, never making them subviews of anything
(editors/commenteditor.lisp: open-comment-editor-pane). Those are exactly the
focus-root elements NO view owns -- vcontainer is written by internal-add-subview
alone. The focus root is not a screen list and cannot stand on its own: a tab page's
controls are re-parented into that same root (alloy-desktop view/view.lisp: the
set-layout-tree :after for retaining-layout), so taking it whole would hand a hidden
page its letters, and CAPI realizes no control there at all."
  (let ((out '()))
    (%map-shown-subviews (lambda (v) (when (%mnemonic-item-p v) (push v out))) view)
    (let ((focus (om-window-focus view)))
      (when focus
        (alloy:do-elements (e focus)
          (when (and (%mnemonic-item-p e) (null (vcontainer e)))
            (push e out)))))
    (nreverse out)))

(defun %mnemonic-candidates (text)
  "Positions in TEXT the vanilla tries as a mnemonic, in its order of preference: the
first character of every word, then every other alphanumeric character."
  (let ((firsts '()) (others '()) (start t))
    (loop for i from 0
          for ch across text
          do (cond ((member ch '(#\Space #\Tab #\Newline)) (setf start t))
                   ((not (alphanumericp ch)) (setf start nil))
                   (start (push i firsts) (setf start nil))
                   (t (push i others))))
    (append (nreverse firsts) (nreverse others))))

(defun %item-mnemonic (item)
  "ITEM's mnemonic CHARACTER, or NIL. Derived from the stored position, so a label
rewritten under it can never underline someone else's letter."
  (let ((index (%item-mnemonic-index item))
        (text  (om-dialog-item-text item)))
    (when (and index (stringp text) (< index (length text)))
      (char text index))))

(defun %assign-mnemonics (window &optional (items (%mnemonic-items window)))
  "Give every visible push button, check box and radio button of WINDOW its mnemonic,
as LispWorks does when it creates the native controls of an interface."
  (let ((taken '()))
    (dolist (item items window)
      (let* ((text (om-dialog-item-text item))
             (text (if (stringp text) text ""))
             (index (loop for i in (%mnemonic-candidates text)
                          unless (member (char-downcase (char text i)) taken)
                            return i)))
        (when index (push (char-downcase (char text index)) taken))
        (unless (eql index (%item-mnemonic-index item))
          (setf (%item-mnemonic-index item) index)
          (alloy:mark-for-render item))))))

(defvar *mnemonics-assigned* (make-hash-table :test 'eq :weakness :key)
  "Window -> the control list its mnemonics were solved on; weakly keyed.")

(defun %ensure-mnemonics (window)
  "Solve WINDOW's mnemonics unless the answer still holds. The solve is DEFERRED to
the first moment a letter is needed -- drawing an underline or pressing Alt+letter --
because om-make-window puts the window on screen BEFORE anything fills it, so there
is no earlier point at which the controls are all there.
The answer is kept against the CONTROL LIST it was solved on rather than a flag, so
every way of filling a window is covered and not only the ones that announce
themselves: the score editors enter their buttons into the window's focus root alone
(editors/commenteditor.lisp: open-comment-editor-pane), which raises no
om-add-subviews. The default distinguishes a window solved over NO control from one
never solved at all -- both hold NIL otherwise, and the editors are exactly the first
case."
  (when window
    (let ((items (%mnemonic-items window)))
      (unless (equal items (gethash window *mnemonics-assigned* :none))
        (setf (gethash window *mnemonics-assigned*) items)
        (%assign-mnemonics window items)))))

(defun %invalidate-mnemonics (window)
  "The set of controls changed, so the answer no longer holds. A window that is
SHOWING its cues re-solves at once, since nothing else would before the next repaint;
any other window just drops the answer until a letter is asked for."
  (when window
    (remhash window *mnemonics-assigned*)
    (when (alld:keyboard-cues-p window) (%ensure-mnemonics window))))

(defmethod alld:window-shown ((window om-look-window))
  "A window is displayed WITH its controls, which is when LispWorks creates them and picks their letters."
  ;; om-make-window shows an empty window first, so the show that counts is the one
  ;; the caller makes after filling it (editors/commenteditor.lisp:
  ;; open-comment-editor-pane); the cue state itself is settled once and stays sticky.
  ;; Every other shown-window hook goes through window.lisp: %install-window-gestures,
  ;; since a second method with these specializers would replace this one.
  (alld:init-keyboard-cues window)
  (%invalidate-mnemonics window)
  (%install-window-gestures window))

(defmethod alld:keyboard-cues-changed ((window om-look-window))
  "Solve the mnemonics before the repaint, then mark every shown widget, not just the window."
  ;; solving from inside an update happens in the middle of the render that would
  ;; show it; and a renderable re-runs its update only when its OWN render-needed-p
  ;; is set (alloy renderers/simple/presentations/protocol.lisp: render :before for
  ;; (renderer renderable)), so marking the window alone would repaint old shapes
  (%ensure-mnemonics window)
  (%map-shown-subviews (lambda (element)
                         (when (typep element 'alloy:renderable)
                           (alloy:mark-for-render element)))
                       window)
  (call-next-method))

(defmethod om-add-subviews :after ((self om-graphic-object) &rest subviews)
  (declare (ignore subviews))
  (%invalidate-mnemonics (om-view-window self)))

(defmethod om-remove-subviews :after ((self om-graphic-object) &rest subviews)
  (declare (ignore subviews))
  (%invalidate-mnemonics (om-view-window self)))

(defmethod (setf alloy:current) :after ((tab alloy:tab) (self om-tab-layout))
  "Another page is on screen: its controls take part in the interface now, and the
ones that left it stop doing so."
  (%invalidate-mnemonics (om-view-window self)))

(defun %snap-mnemonic-underline (renderable shape)
  "Put the mnemonic underline of SHAPE on a whole DEVICE row of RENDERABLE, and inside
it. The quads are laid out in the shape's own space (alloy-desktop/graphics/text-render.lisp:
snap-underline-rows), which cannot see two things: the widget itself may sit on a
fractional row, and a 1 px line across two rows is resolved by the window MSAA into two
grey ones; and a caption that fills its widget puts baseline+1 BELOW it, where CAPI's
clip of a check button to its own pane (the render :before for (simple:renderer
om-check-box)) scissors the cue away entirely."
  (let* ((data (org.shirakumo.alloy.renderers.opengl.msdf::vertex-data shape))
         (base (* 90 (length (alloy:text shape)))))
    (when (< base (length data))
      (let* ((local  (alloy:pxy (org.shirakumo.alloy.renderers.opengl.msdf::location shape)))
             (origin (+ (alloy:pxy (alloy:global-location renderable)) local))
             (bottom (loop for i from (1+ base) below (length data) by 15
                           minimize (aref data i)))
             (shift  (- (fround (+ origin bottom)) origin bottom))
             (shift  (if (minusp (+ local bottom shift)) (+ shift 1.0) shift)))
        (unless (zerop shift)
          (loop with delta = (%as-single-float shift)
                for i from (1+ base) below (length data) by 15
                do (incf (aref data i) delta)))))))

(defmethod presentations:update-shape :after
    ((renderer t) (renderable om-standard-dialog-item) (shape simple:text))
  (%snap-mnemonic-underline renderable shape))

(defun %activate-mnemonic-item (item)
  "Press ITEM the way its Windows control answers its access key: a push button fires,
a check box toggles, and a radio button selects but never clears."
  (unless (and (typep item 'om-radio-button) (om-checked-p item))
    (alloy:activate item))
  item)

(defun %find-mnemonic-button (window char)
  "The push button, check box or radio button WINDOW shows whose mnemonic is CHAR.
Searched over the same set the letters were solved on, and independently of the focus:
an access key presses its control wherever the focus happens to be."
  (let ((match nil)
        (down (char-downcase char)))
    (dolist (item (%mnemonic-items window) match)
      (let ((mnem (%item-mnemonic item)))
        (when (and mnem (char= down (char-downcase mnem)))
          (setf match item))))))

(defmethod alloy:handle ((event om-mnemonic-action) (window om-look-window))
  (%ensure-mnemonics window)
  (let ((item (%find-mnemonic-button window (mn-char event))))
    (cond (item
           (%activate-mnemonic-item item)
           t)
          (t (alloy:decline)))))

;;; RET and ESC go to the dialog's default and cancel buttons by the window's own rule
;;; (alloy-desktop/widgets/controls.lisp: alloy:handle :around for (alloy:activate window)).

;;; OM-ZOOM-POP-UP

(defclass om-zoom-pop-up (om-view)
  ((zoom-value     :initarg :value         :initform 24   :accessor zoom-value)
   (zoom-presets   :initarg :presets       :initform '(50 100 150 200 300 400) :accessor zoom-presets)
   (zoom-action    :initarg :di-action     :initform nil  :accessor zoom-action)
   (zoom-text-font :initarg :font          :initform nil  :accessor zoom-text-font)
   (zoom-fg-color  :initarg :fg-color      :initform nil  :accessor zoom-fg-color)
   (display-format :initarg :format-string :initform "~D" :accessor display-format)
   (hover-p        :initform nil           :accessor zoom-hover-p)))

(defparameter *om-zoom-chevron-width* 24)

;;; The kernel builds this control with om-make-view (projects scoreeditors.lisp: build-zoom-font-widget), so it is
;;; an om-view and would draw below the dialog items the kernel places over it in the
;;; same strip; it is a control, so it shares their layer
;;; (see alloy-desktop/widgets/dialog-item.lisp: alloy:render :around for dialog-item).
(defmethod alloy:render :around ((renderer alld:window) (self om-zoom-pop-up))
  (simple:with-pushed-transforms (renderer)
    (incf (simple:z-index renderer) alld:*dialog-item-z-elevation*)
    (call-next-method)))

(defmethod om-draw-contents ((self om-zoom-pop-up))
  (let* ((w       (om-width self))
         (h       (om-height self))
         ;; vanilla resolves the zoom text through the OM default, not the
         ;; toolkit role (om-LW/dialog-items.lisp: om-draw-contents for om-zoom-pop-up)
         (font    (or (zoom-text-font self) *om-controls-font*))
         (fg      (or (zoom-fg-color self) *om-black-color*))
         (txt     (format nil (display-format self) (zoom-value self)))
         (cx      (- w (/ *om-zoom-chevron-width* 2.0)))
         (cy      (/ h 2.0))
         (chevron (if (zoom-hover-p self) *om-popup-chevron-hover-color* *om-interface-blue-color*)))
    (om-with-fg-color self *om-white-color*
      (om-fill-rect 0 0 w h))
    (om-with-fg-color self chevron
      (om-fill-rect (- w *om-zoom-chevron-width*) 0 *om-zoom-chevron-width* h))
    (om-with-fg-color self *om-white-color*
      (om-fill-polygon (list (list (- cx 5) (- cy 3))
                             (list (+ cx 5) (- cy 3))
                             (list cx (+ cy 3)))))
    (om-with-fg-color self *om-black-color*
      (om-draw-rect 0 0 (- w 1) (- h 1) :pensize 2))
    (om-with-fg-color self fg
      (om-with-font font
        (alloy:render *current-renderer*
                      (simple:text *current-renderer*
                                   (alloy:px-extent 0 0 (max 1 (- w *om-zoom-chevron-width*)) h)
                                   txt
                                   :font    (%resolve-font)
                                   :size    (%resolve-font-size)
                                   :pattern *current-fg-color*
                                   :halign  :center
                                   :valign  :middle))))))

(defun %zoom-popup-update-hover (window loc)
  "Walk WINDOW's layout-tree updating each om-zoom-pop-up's hover-p based on LOC."
  (labels ((walk (el)
             (when (typep el 'om-zoom-pop-up)
               (let ((h (and (alloy:contained-p loc el) t)))
                 (unless (eq h (zoom-hover-p el))
                   (setf (zoom-hover-p el) h)
                   (alloy:mark-for-render el))))
             (when (typep el 'alloy:container)
               (alloy:do-elements (child el) (walk child)))))
    (walk (alloy:root (alloy:layout-tree window)))))

(defmethod om-zoom-pop-up-value ((self om-zoom-pop-up))
  (zoom-value self))

(defmethod om-set-zoom-pop-up-value ((self om-zoom-pop-up) new-pct)
  (setf (zoom-value self) new-pct)
  (om-invalidate-view self))

(defmethod om-zoom-pop-up-presets ((self om-zoom-pop-up))
  (zoom-presets self))

(defmethod om-set-zoom-pop-up-presets ((self om-zoom-pop-up) preset-list)
  (setf (zoom-presets self) preset-list))

;;; Ahead of the clear-hover method below, which names this type; the widget itself is built
;;; further down, with the choice-popup machinery its dropdown needs.
(defclass om-history-text (om-editable-text)
  ((history-source :initarg :history-source :initform nil :accessor history-source)
   ;; :weak focus marks the WHOLE widget (alloy component.lisp: handle for (pointer-move component)) and cannot
   ;; tell the strip from the text, so the strip keeps its own hover state
   (chevron-hovered-p :initform nil :accessor chevron-hovered-p))
  (:documentation "Editable field with a recall chevron at its right edge.
HISTORY-SOURCE is a function of no arguments returning the list of strings, read
at open time so the list is never stale."))

;;; Leaving the window drops these hover states too (alloy-desktop/input/pointer.lisp:
;;; the handle :before for (pointer-leave window)).
(defmethod alld:clear-hover ((el om-zoom-pop-up))
  (when (zoom-hover-p el)
    (setf (zoom-hover-p el) nil)
    (alloy:mark-for-render el)))

;; %history-set-hover already tests, clears and marks
(defmethod alld:clear-hover ((el om-history-text))
  (%history-set-hover el nil))

(defun %zoom-open-popup (self)
  (let ((fmt (display-format self)))
    (alld:popup-open-choice-list
     self
     (mapcar (lambda (pct)
               (cons (format nil fmt pct)
                     (lambda ()
                       (om-set-zoom-pop-up-value self pct)
                       (when (zoom-action self)
                         (funcall (zoom-action self) self pct)))))
             (zoom-presets self))
     :selected-label (format nil fmt (zoom-value self)))))

(defun %popup-choice-label (item)
  "Row text of an om-pop-up-dialog-item entry: vanilla shows the cdr of a cons and
the item itself otherwise (om-LW/dialog-items.lisp: initialize-instance :after for om-pop-up-dialog-item)."
  (cond ((consp item) (princ-to-string (cdr item)))
        ((null item)  "")
        (t            (princ-to-string item))))

;;; CAPI shows an option-pane as a native drop-down list (capi-choice-ug-7.htm:28),
;;; which the owner window never clips and which drops downwards.  Alloy's combo
;;; instead renders combo-list inside the owner (alloy components/combo.lisp: render :after for (renderer combo)) and scrolls it so
;;; the focused row lands on the control, which is what made the direction alternate.
(defmethod alloy:activate :around ((self om-pop-up-dialog-item))
  (let ((items (om-get-item-list self)))
    (if (null items)
        (call-next-method)
        (alld:popup-open-choice-list
         self
         (mapcar (lambda (item)
                   (let ((value (if (consp item) (car item) item)))
                     (cons (%popup-choice-label item)
                           (lambda () (setf (alloy:value self) value)))))
                 items)
         :selected-label
         (let ((current (alloy:value self)))
           (%popup-choice-label
            (or (find current items :test (lambda (v i)
                                            (eql v (if (consp i) (car i) i))))
                current)))))))

(defmethod om-view-click-handler ((self om-zoom-pop-up) position)
  (declare (ignore position))
  (%zoom-open-popup self)
  t)

;;; om-history-text — the CAPI TEXT-INPUT-CHOICE effect (om-LW
;;; lw-lisp-tools/editor-find-dialogs.lisp: the find-pane of define-interface find-dialog):
;;; ONE widget that is both an
;;; editable field and a recall list. Alloy has no such component, so the field is
;;; a single white box that draws the chevron inside its own border and gives up
;;; the strip under it — text, caret and dropdown are all this class's own.

(defparameter *om-history-chevron-width* 10
  "Width in px of the v glyph drawn at the field's right edge.")
(defparameter *om-history-chevron-height* 6
  "Ink height in px of the v glyph; stays well under the field height.")
(defparameter *om-history-chevron-weight* 1.6
  "Stroke thickness in px of the v glyph.")
(defparameter *om-history-chevron-margin* 8
  "Px kept between the v glyph and the field's right border.")
(defparameter *om-history-text-inset* 2
  "Px the text keeps from the border and from the chevron strip.")
(defparameter *om-history-chevron-color* *om-dark-gray-color*
  "Ink of the v glyph at rest.")
(defparameter *om-history-chevron-hover-color* *om-interface-blue-color*
  "Ink of the v glyph while the pointer is over the strip: the same accent the
focused border uses, so hover changes the glyph and nothing else.")

;;; the defclass sits above the clear-hover method, which has to name this type

(defun %history-strip-width ()
  "Px at the right edge that belong to the chevron and never to the text."
  (+ *om-history-chevron-width* *om-history-chevron-margin*))

(defun %history-usable-width (field)
  "X of the right edge of FIELD's text area, in field px."
  (max 0 (- (alloy:pxw (alloy:bounds field))
            (%history-strip-width) *om-history-text-inset*)))

(defun %history-chevron-points (field side)
  "One arm of the thin v as a convex quad: the renderer fills a polygon with a
triangle fan (alloy renderers/opengl/renderer.lisp: render-direct for polygon), which cannot close a v."
  (let* ((b   (alloy:bounds field))
         (hw  (/ *om-history-chevron-width* 2.0))
         (hh  (/ *om-history-chevron-height* 2.0))
         (cx  (- (alloy:pxw b) *om-history-chevron-margin* hw))
         (cy  (/ (alloy:pxh b) 2.0))
         (x   (ecase side (:left (- cx hw)) (:right (+ cx hw))))
         (top (+ cy hh))
         (bot (- cy hh)))
    (list (alloy:px-point x top)
          (alloy:px-point cx (+ bot *om-history-chevron-weight*))
          (alloy:px-point cx bot)
          (alloy:px-point x (- top *om-history-chevron-weight*)))))

(defun %history-chevron-ink (field)
  (if (chevron-hovered-p field)
      *om-history-chevron-hover-color*
      *om-history-chevron-color*))

(defun %history-set-hover (field state)
  "Store the strip's hover state; repaint only on a real transition."
  (let ((state (and state t)))
    (unless (eq state (chevron-hovered-p field))
      (setf (chevron-hovered-p field) state)
      (alloy:mark-for-render field))
    state))

(defun %history-border-pattern (field)
  "Frame ink: the focused accent, as a native field's focus ring."
  (%omcolor->chroma (if (alloy:focus field)
                        *om-interface-blue-color*
                        *om-gray-color*)))

(defun %history-cover-extent (field)
  "The strip repainted in FIELD's own background colour, so text scrolled past the
text area stops being visible there — what a native edit box clips away."
  (let* ((b (alloy:bounds field))
         (w (alloy:pxw b))
         (h (alloy:pxh b))
         (x (%history-usable-width field)))
    (alloy:px-extent x 0 (max 0 (- w x)) h)))

(presentations:define-realization (alld:window om-history-text t)
  ((:chevron-cover simple:rectangle)
   (alloy:margins)
   :z-index 4)
  ((:chevron-left simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 5)
  ((:chevron-right simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 5))

(presentations:define-update (alld:window om-history-text)
  ;; px units, not the default un: the strip is measured in the same pixels the
  ;; chevron points and the cover extent use
  (:label
   :bounds (alloy:margins (alloy:px *om-history-text-inset*)
                          (alloy:px 1)
                          (alloy:px (+ (%history-strip-width) *om-history-text-inset*))
                          (alloy:px 1)))
  ;; topmost, as a native frame is: a glyph clipped at the widget edge must not
  ;; break the border line, and the cover must not paint over it either
  (:border-top
   :z-index 6
   :pattern (%history-border-pattern alloy:renderable))
  (:border-bottom
   :z-index 6
   :pattern (%history-border-pattern alloy:renderable))
  (:border-left
   :z-index 6
   :pattern (%history-border-pattern alloy:renderable))
  (:border-right
   :z-index 6
   :pattern (%history-border-pattern alloy:renderable))
  (:chevron-cover
   :bounds (%history-cover-extent alloy:renderable)
   :pattern (%resolve-pattern alloy:renderable #'om-bg-color *om-white-color*))
  ;; hover repaints the GLYPH, never a rectangle behind it
  (:chevron-left
   :points (%history-chevron-points alloy:renderable :left)
   :pattern (%omcolor->chroma (%history-chevron-ink alloy:renderable)))
  (:chevron-right
   :points (%history-chevron-points alloy:renderable :right)
   :pattern (%omcolor->chroma (%history-chevron-ink alloy:renderable))))

;;; the scrollable extent exceeds the text by the strip the label gives up, so the
;;; end of the text can still reach the right edge of the text area
(defmethod alld:scroll-content-size ((self om-history-text))
  (multiple-value-bind (w h) (call-next-method)
    (values (+ w (%history-strip-width) *om-history-text-inset*) h)))

(defun %history-caret-follow (self)
  "Keep the caret inside the text area. %text-caret-follow aims at the
whole widget, which would park the caret under the chevron."
  (let ((label (presentations:find-shape :label self nil)))
    (when label
      ;; the shape lags the buffer by a frame, so the caret index can be past its
      ;; text and compute-cursor-location arefs out of range (alloy renderers/opengl/msdf.lisp: compute-cursor-location)
      (handler-bind ((type-error (lambda (c)
                                   (format *error-output* "[history-caret-follow] ~A~%"
                                           (princ-to-string c))
                                   (return-from %history-caret-follow nil))))
        (alloy:with-unit-parent self
          (let* ((rect    (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location
                           label (alloy:pos (alloy:cursor self))))
                 (caret-x (alloy:pxx rect))
                 (caret-w (alloy:pxw rect))
                 (margin  *text-caret-follow-margin*)
                 (visible (- caret-x (alld:scroll-x self)))
                 (view-w  (%history-usable-width self)))
            (cond ((< visible margin)
                   (alld:set-scroll self (- caret-x margin) (alld:scroll-y self)))
                  ((> (+ visible caret-w margin) view-w)
                   (alld:set-scroll self (- (+ caret-x caret-w margin) view-w)
                                (alld:scroll-y self))))))))))

;;; runs after om-editable-text's own alloy:render :after, so the last aim of the frame
;;; is this one; the settled state is a no-op for both.
(defmethod alloy:render :after ((renderer simple:renderer) (self om-history-text))
  (%history-caret-follow self))

(defun %history-in-chevron-p (field location)
  "True when LOCATION (global) is inside FIELD and in its chevron strip."
  (and (alloy:contained-p location field)
       (let ((x0 (+ (alloy:pxx (alloy:global-location field))
                    (max 0 (- (alloy:pxw (alloy:bounds field))
                              (%history-strip-width))))))
         (>= (alloy:pxx location) x0))))

;;; The recall list is a WHITE list, not a menu: alld:popup-frame paints its interior
;;; with a light gray (alloy-desktop/menu/menu.lisp: the define-update of (window popup-frame)), the one key this look changes.
(defclass om-history-popup-frame (alld:choice-popup-frame) ())

(presentations:define-update (alld:window om-history-popup-frame)
  (:background :pattern (%omcolor->chroma *om-white-color*)))

(defun %history-open-choice-list (widget rows &key (min-width 60))
  "Sibling of alld:popup-open-choice-list: same native surface chain, own frame
class so the recall list carries this widget's look and not the menu's."
  (let ((window (om-view-window widget)))
    (when (and window rows (not (alld:popup-consume-opener-suppression-p widget)))
      (alld:popup-close-stack window)
      (let* ((ih      alld:*popup-menu-item-height*)
             (count   (length rows))
             (visible (min count alld:*choice-popup-max-visible-items*))
             (height  (* visible ih))
             (width   (max (alloy:pxw (alloy:bounds widget)) min-width))
             (row-w   (if (< visible count)
                          (- width alld:*popup-menu-border-width* alld:*scrollbar-track-px*)
                          width)))
        (multiple-value-bind (ax ay) (alld:popup-drop-anchor window widget height)
          (multiple-value-bind (left top) (alld:popup-fit-screen-rect ax ay width height)
            (let* ((frame (make-instance 'om-history-popup-frame
                                         :owner-window window :depth 0
                                         :opener        widget
                                         ;; captured BEFORE the frame takes :strong,
                                         ;; restored by alld:popup-retire-frame
                                         :return-focus  (%om-strong-pane window)
                                         :screen-left   left
                                         :screen-top    top
                                         :screen-width  width
                                         :screen-height height))
                   (surface (alld:popup-attach-surface frame window width height))
                   (items   '())
                   (focus-row nil))
              (setf (alld:choice-popup-row-height frame) ih
                    (alld:choice-popup-row-width frame)  row-w
                    (alld:choice-popup-visible frame)    visible
                    (alld:choice-popup-top frame)        0)
              (dolist (row rows)
                (let* ((label (car row))
                       (thunk (cdr row))
                       (item (make-instance 'alld:choice-popup-item
                                            :value      label
                                            :enabled-p  t
                                            :on-activate
                                            (lambda ()
                                              (alld:popup-close-stack window)
                                              (funcall thunk)))))
                  (alloy:enter item frame :x 0 :y 0 :w row-w :h ih)
                  (push item items)
                  (unless focus-row (setf focus-row item))))
              (setf (alld:choice-popup-rows frame) (coerce (nreverse items) 'vector))
              (alld:choice-popup-place-rows frame)
              (when (< visible count)
                (alld:choice-popup-add-bar frame width height))
              (setf (alloy:focused frame) focus-row)
              (alld:popup-present-frame frame surface))))))))

(defun %history-open-list (field)
  "Drop FIELD's recall list under it; the source is read now, never cached."
  ;; the list takes the pointer to another surface, from which no move ever comes
  ;; back to clear the strip
  (%history-set-hover field nil)
  (let ((items (let ((source (history-source field))) (and source (funcall source)))))
    (when items
      (%history-open-choice-list
       field
       (loop for item in items
             collect (let ((label (princ-to-string item)))
                       (cons label
                             (lambda () (om-set-dialog-item-text field label)))))))))

(defmethod alloy:handle :around ((event alloy:pointer-down) (pane om-history-text))
  (if (and (not (eq (alloy:kind event) :right))
           (%history-in-chevron-p pane (alloy:location event)))
      (progn (%history-open-list pane) t)
      (call-next-method)))

;;; alloy's text input claims the I-beam for the whole component (alloy
;;; components/text-input.lisp: handle for (pointer-move text-input-component));
;;; the strip is a button and takes the arrow.
;;; A drag started in the text keeps the text machinery, strip or not.
(defmethod alloy:handle :around ((event alloy:pointer-move) (pane om-history-text))
  (let ((hot (and (not (dragging-p pane))
                  (%history-in-chevron-p pane (alloy:location event)))))
    (%history-set-hover pane hot)
    (if hot
        (progn (setf (alloy:cursor (alloy:ui pane)) :arrow) t)
        (call-next-method))))

;;; CAPI filtering-layout (capi-capi-ref-156.htm): a text field plus a dropdown of
;;; three INDEPENDENT axes — regexp/plain, exclude/include, case — that combine
;;; freely into eight states. Alloy has no equivalent, so the parts are dialog
;;; items wired to one filter state, shared by every host that needs a filter.
;;; The state, the decision and the strip's measures are
;;; alloy-desktop/widgets/filter.lisp: defclass filter-host; the widgets are here.

(defclass om-filter-host (alld:filter-host) ()
  (:documentation "State and widget handles of one CAPI filtering-layout."))

(defclass om-filter-field (om-editable-text) ())
(defclass om-filter-clear (om-button) ())

;;; CAPI's filter menu is a MENU of three checkable rows, not an option-pane, so
;;; the widget is a button that drops that menu — the generic om-button look
;;; (rounded border, hover fill) belongs to dialogs, not to a filter strip.
(defclass om-filter-mode (om-button) ())

(defun %filter-chevron-points (combo)
  (let* ((bounds (alloy:bounds combo))
         (w (alloy:pxw bounds))
         (h (alloy:pxh bounds))
         (cx (- w 12.0))
         (cy (/ h 2.0)))
    (list (alloy:px-point (- cx 4.0) (+ cy 2.0))
          (alloy:px-point (+ cx 4.0) (+ cy 2.0))
          (alloy:px-point cx (- cy 3.0)))))

(presentations:define-realization (alld:window om-filter-mode)
  ((:mode-bg simple:rectangle)
   (alloy:margins)
   :z-index 0)
  ((:mode-label simple:text)
   (alloy:margins 4 1 22 1)
   alloy:text
   :font (%om-font-resolve alloy:renderer (%om-native-font :dialog-default))
   :size (%om-font-draw-unit alloy:renderer (%om-native-font :dialog-default))
   :halign :start :valign :middle
   :z-index 10)
  ((:mode-chevron simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 12))

(presentations:define-update (alld:window om-filter-mode)
  (:mode-bg :pattern (if (alloy:focus alloy:renderable)
                         alld:*filter-clear-hover*
                         (%omcolor->chroma *om-light-gray-color*)))
  (:mode-label :text alloy:text :pattern alld:*filter-text-color*)
  (:mode-chevron :points (%filter-chevron-points alloy:renderable)
                 :pattern alld:*filter-text-color*))

(defun %filter-clear-bar (clear which)
  "One diagonal bar of the clear glyph, in CLEAR's local box (alloy y-up)."
  (let* ((bounds (alloy:bounds clear))
         (cx (/ (alloy:pxw bounds) 2.0))
         (cy (/ (alloy:pxh bounds) 2.0))
         (r  5.0)
         (d  1.6))
    (ecase which
      (:up   (list (alloy:px-point (- cx r d) (- cy r))
                   (alloy:px-point (+ (- cx r) d) (- cy r))
                   (alloy:px-point (+ cx r d) (+ cy r))
                   (alloy:px-point (- (+ cx r) d) (+ cy r))))
      (:down (list (alloy:px-point (- cx r d) (+ cy r))
                   (alloy:px-point (+ (- cx r) d) (+ cy r))
                   (alloy:px-point (+ cx r d) (- cy r))
                   (alloy:px-point (- (+ cx r) d) (- cy r)))))))

(presentations:define-realization (alld:window om-filter-clear)
  ((:clear-bg simple:rectangle)
   (alloy:margins)
   :z-index 0)
  ((:clear-up simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 5)
  ((:clear-down simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 5))

(presentations:define-update (alld:window om-filter-clear)
  (:clear-bg :pattern (if (alloy:focus alloy:renderable)
                          alld:*filter-clear-hover*
                          (%omcolor->chroma *om-light-gray-color*)))
  (:clear-up   :points (%filter-clear-bar alloy:renderable :up)
               :pattern alld:*filter-clear-color*)
  (:clear-down :points (%filter-clear-bar alloy:renderable :down)
               :pattern alld:*filter-clear-color*))

(defun %filter-changed (host)
  (setf (alld:filter-text host)
        (let ((field (alld:filter-field host)))
          (if field (om-dialog-item-text field) "")))
  (alld:filter-apply host))

(defun %filter-clear-text (host)
  (let ((field (alld:filter-field host)))
    (when field (om-set-dialog-item-text field "")))
  (setf (alld:filter-text host) "")
  (alld:filter-apply host))

(defun %filter-mode-width (label)
  "Snug width for the mode dropdown, so the strip reads like CAPI's `Filter v'."
  (+ (om-string-size (or label "Filter") (%om-native-font :dialog-default)) 34))

(defun %filter-refresh-mode (host)
  "Re-label the mode button for the current axes and refit the strip to it."
  (let ((button (alld:filter-mode-menu host))
        (label  (alld:filter-mode-label host)))
    (when button
      (om-set-dialog-item-text button label)
      (when (alloy:layout-parent button)
        (alloy:update button (alloy:layout-parent button)
                      :place (alld:filter-mode-place host)
                      :size (alloy:un (%filter-mode-width label)))))))

(defun %filter-toggle (host axis)
  "Flip ONE axis. The three are independent booleans and combine freely, so all
eight states are reachable (capi-capi-ref-156.htm:74-78)."
  (ecase axis
    (:regexp  (setf (alld:filter-regexp-p host)  (not (alld:filter-regexp-p host))))
    (:exclude (setf (alld:filter-exclude-p host) (not (alld:filter-exclude-p host))))
    (:case    (setf (alld:filter-case-p host)    (not (alld:filter-case-p host)))))
  (%filter-refresh-mode host)
  (alld:filter-apply host))

(defun %filter-mode-menu-items (host)
  "One CHECKABLE leaf per axis; the tick is a live selected-predicate, the way
vanilla builds a checked item (om-LW/menu.lisp: om-new-leafmenu)."
  (loop for (title . axis) in alld:*filter-mode-items*
        collect (let ((axis axis))
                  (om-new-leafmenu title
                                   (lambda () (%filter-toggle host axis))
                                   nil t
                                   (lambda () (alld:filter-axis-on-p host axis))))))

(defun %filter-open-mode-menu (host)
  "Drop the checkable menu under the mode button, where CAPI anchors it."
  (let ((button (alld:filter-mode-menu host))
        (window (alld:filter-window host)))
    (when (and button window)
      (alld:popup-close-stack window)
      (let ((origin (alloy:global-location button)))
        (multiple-value-bind (left top)
            (alld:popup-owner-point-to-screen window (alloy:pxx origin) (alloy:pxy origin))
          (alld:popup-open-menu-at
           (alld:popup-normalize-entries (%filter-mode-menu-items host) window)
           0 window left top
           :opener button
           :return-focus (om-window-focus window)))))))

(defun %filter-make-mode-button (host)
  (let ((label (alld:filter-mode-label host)))
    (om-make-dialog-item
     'om-filter-mode (om-make-point 0 0)
     (om-make-point (%filter-mode-width label) 20) label
     :font (%om-native-font :dialog-default)
     :di-action (om-dialog-item-act item
                  (declare (ignore item))
                  (%filter-open-mode-menu host)))))

(defun %filter-make-field (host &optional (width alld:*filter-field-width*))
  (om-make-dialog-item
   'om-filter-field (om-make-point 0 0) (om-make-point width 20) ""
   :font (%om-native-font :dialog-default)
   :bg-color *om-white-color*
   :di-action (om-dialog-item-act item
                (declare (ignore item))
                (%filter-changed host))))

(defun %filter-make-clear-button (host)
  (om-make-dialog-item
   'om-filter-clear (om-make-point 0 0)
   (om-make-point alld:*filter-clear-width* 20) ""
   :di-action (om-dialog-item-act item
                (declare (ignore item))
                (%filter-clear-text host))))
