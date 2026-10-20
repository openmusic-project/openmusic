(in-package :alloy-desktop)

;;; The CAPI choice widget (list-panel, option-pane): data model, selection, gestures,
;;; keyboard navigation and the callback channel shaped by callback-type.

;;; CAPI fires a choice callback on the USER's gesture only (capi-capi-ref-378.htm:61,
;;; capi-capi-ref-30.htm:100); a setter run inside WITH-PROGRAMMATIC-SET stays silent.
(defvar *programmatic-set* nil)

(defmacro with-programmatic-set (&body body)
  `(let ((*programmatic-set* t)) ,@body))

;;; The CAPI choice channel, shared by list-panel and option-pane (capi:choice): a
;;; caller's callbacks REPLACE the defaults, :callback-type shapes their arglist and
;;; :interaction decides which channel a gesture uses.
(defclass choice-mixin ()
  ((selection-callback :initarg :selection-callback :initform nil :accessor selection-callback)
   (retract-callback   :initarg :retract-callback   :initform nil :accessor retract-callback)
   (extend-callback    :initarg :extend-callback    :initform nil :accessor extend-callback)
   (callback-type      :initarg :callback-type      :initform nil :accessor callback-type)
   (test-function      :initarg :test-function      :initform 'equal :accessor test-function)
   (interaction        :initarg :interaction        :initform :single-selection :accessor interaction)))

;;; CAPI builds each callback argument from callback-type; for a list-panel the
;;; element IS the collection and its items carry no capi item wrapper, so the
;;; item-data of the selection answers :data and :item (capi-capi-ref-23.htm:93-131).

(defgeneric selected-item (choice)
  (:documentation "The item CHOICE has selected: an item for a single choice, a list of items for a multiple one."))

(defgeneric callback-argument (choice spec)
  (:documentation "The callback argument SPEC names for CHOICE.")
  (:method ((choice t) spec)
    ;; every known spec has its own method below; an unknown one fails this ECASE
    (ecase spec
      ((:collection :element) choice)
      ((:data :item)          choice)
      (:interface             choice)
      (:focus                 choice))))

(defmethod callback-argument ((choice t) (spec (eql :collection))) choice)
(defmethod callback-argument ((choice t) (spec (eql :element))) choice)
(defmethod callback-argument ((choice t) (spec (eql :data))) (selected-item choice))
(defmethod callback-argument ((choice t) (spec (eql :item))) (selected-item choice))

(defmethod callback-argument ((choice t) (spec (eql :interface)))
  (let ((tree (alloy:layout-tree choice)))
    (and tree (alloy:ui tree))))

(defmethod callback-argument ((choice t) (spec (eql :focus)))
  (let* ((tree (alloy:layout-tree choice))
         (root (and tree (window-focus-root (alloy:ui tree)))))
    (and root (alloy:focused root))))

(defparameter *callback-arg-specs*
  '((:collection-data :collection :data) (:data :data) (:data-element :data :element)
    (:data-interface :data :interface)   (:element :element)
    (:element-data :element :data)       (:element-item :element :item)
    (:interface-data :interface :data)   (:item :item)
    (:item-element :item :element)       (:item-interface :item :interface)
    (:interface-item :interface :item)   (:interface :interface)
    (:full :data :item :interface)       (:focus :focus) (:none))
  "callback-type -> argument specs, from capi-capi-ref-23.htm:93-126.")

(defun callback-args (type choice)
  "A list TYPE is its own spec sequence (capi-capi-ref-23.htm:129); NIL means
:data-interface, the documented fallback (:135)."
  (mapcar (lambda (spec) (callback-argument choice spec))
          (cond ((null type) '(:data :interface))
                ((listp type) type)
                (t (let ((entry (assoc type *callback-arg-specs*)))
                     (unless entry (error "Unknown CAPI callback-type ~S" type))
                     (cdr entry))))))

(defun funcall-choice-callback (choice callback)
  (when callback
    (apply callback (callback-args (callback-type choice) choice))))

(defgeneric list-changed (list value)
  (:documentation "Told of every gesture-driven change of LIST's selection, before its callback.")
  (:method ((list t) value)
    (declare (ignore value))
    nil))

(defun notify-list-change (list value &optional (callback (selection-callback list)))
  (unless *programmatic-set*
    (list-changed list value)
    (funcall-choice-callback list callback)))

;;; Lists: a vertical stack of row labels over a list-data, scrolled by its own offset.

(defclass list-label (alloy:label) ())

(defmethod alloy:refresh ((label list-label))
  (alloy:mark-for-render label))

;;; A list-panel is a pane: its :border and its scrollbar (widgets/chrome.lisp: alloy:render
;;; :around for scrollbar) are ranked against the dialog-item elevation of its subtree.
(defclass item-list (scroll-mixin alloy:vertical-linear-layout alloy:focus-list alloy:component choice-mixin dialog-item)
  ((action-callback :initarg :action-callback :initform nil :accessor action-callback))
  (:default-initargs
   :data (make-instance 'alloy::list-data :list nil)))

(defclass single-item-list (item-list)
  ((committed-index :initarg :committed-index :initform nil :accessor committed-index))
  (:default-initargs
   :interaction :single-selection))

;;; On win32 the gesture toggles (:multiple-selection), elsewhere it replaces and
;;; command/shift extend (:extended-selection); both retract on deselect.
(defclass multi-item-list (item-list)
  ((selected-indices :initarg :selected-indices :initform nil :accessor selection-indices)
   (selection-anchor :initform nil :accessor selection-anchor))
  (:default-initargs
   :interaction #+win32 :multiple-selection #-win32 :extended-selection))

;;; A list's scrollable extent is its own ideal size: the stacked rows.
(defun widget-content-size (self)
  (let ((ideal (alloy:suggest-size (alloy:bounds self) self)))
    (values (round (alloy:pxw ideal)) (round (alloy:pxh ideal)))))

(defmethod scroll-content-size ((self item-list)) (widget-content-size self))

(defmethod alloy::represent-for ((component item-list) data &rest initargs)
  (apply #'alloy:represent-with 'list-label data initargs))

(defmethod alloy:refresh ((list item-list))
  (alloy:clear list)
  (let ((data (alloy:data list)))
    (loop for i below (alloy::count data)
          do (let ((i i))
               (alloy:enter (alloy::represent-for list (alloy:place-data (alloy:element data i))) list)))))

;;; Only the element observer is installed: push/pop-element cannot complete on
;;; list-data (its value slot is reader-only, so their PUSH/POP signal
;;; no-primary-method before any observer could matter).
(defun install-list-observers (list data)
  (when data
    (alloy:observe 'alloy:element data
      (lambda (new-value observ idx)
        (declare (ignore new-value observ))
        (alloy:refresh (alloy:index-element idx list)))
      :name 'list-element-observer)))

(defmethod initialize-instance :after ((list single-item-list) &key)
  (alloy:refresh list)
  (when (and (alloy:data list) (plusp (alloy::count (alloy:data list))))
    (setf (committed-index list) 0))
  (install-list-observers list (alloy:data list)))

(defmethod initialize-instance :after ((list multi-item-list) &key)
  (alloy:refresh list)
  (install-list-observers list (alloy:data list)))

(defun mark-list-children-for-render (list)
  (alloy:do-elements (element list)
    (alloy:mark-for-render element)))

;;; CAPI multi-choice gestures (capi-capi-ref-30.htm, choice/interaction): :multiple-selection
;;; toggles, :extended-selection replaces and lets command/shift extend; deselect always retracts.
(defun multi-selection-gesture (list idx)
  (let* ((sel      (selection-indices list))
         (selected (and (member idx sel) t))
         (extended (eql :extended-selection (interaction list)))
         (modifier (and extended (or (command-key-p) (shift-key-p))))
         (callback nil))
    (cond ((and extended (not modifier))
           (setf (selection-indices list) (list idx)
                 (selection-anchor list) idx
                 callback (selection-callback list)))
          ((and modifier (shift-key-p))
           (let* ((anchor (or (selection-anchor list) idx))
                  (lo     (min anchor idx))
                  (hi     (max anchor idx)))
             (setf (selection-indices list) (loop for i from lo to hi collect i)
                   callback (extend-callback list))))
          (selected
           (setf (selection-indices list) (remove idx sel)
                 (selection-anchor list) idx
                 callback (retract-callback list)))
          (t
           (setf (selection-indices list) (cons idx sel)
                 (selection-anchor list) idx
                 callback (if extended (extend-callback list) (selection-callback list)))))
    (mark-list-children-for-render list)
    (notify-list-change list (sort (copy-list (selection-indices list)) #'<) callback)))

(defun scroll-to-row (list row)
  "Scroll LIST so ROW lies inside its viewport, as a native list-panel does when the
selection moves. The list scrolls by its own offset, not through a clip-view."
  (when (and (typep list 'scroll-mixin) (scrollbars list))
    (alloy:with-unit-parent list
      (let* ((rg (alloy:global-location row))
             (lg (alloy:global-location list))
             (rh (alloy:pxh (alloy:bounds row)))
             (lh (alloy:pxh (alloy:bounds list)))
             (ry (alloy:pxy rg))
             (ly (alloy:pxy lg))
             (cur (round (scroll-y list))))
        (flet ((scroll-to (pos)
                 (unless (scroll-passive-p list)
                   (set-scroll list (scroll-x list) pos))))
          (cond ((< ry ly)
                 (scroll-to (+ cur (- ly ry))))
                ((> (+ ry rh) (+ ly lh))
                 (scroll-to (- cur (- (+ ry rh) (+ ly lh)))))))))))

(defgeneric choice-focus-pane (list)
  (:documentation "Give LIST the keyboard, as a native list-panel takes it when a row is selected.")
  (:method ((list t))
    (let ((*hover-move-p* nil))
      (focus-pane nil list))))

(defmethod alloy:activate :after ((label list-label))
  (let ((parent (alloy:layout-parent label)))
    ;; selecting a row hands the keyboard to the LIST, as a native list-panel does:
    ;; this is the single point both the click and the arrow keys go through
    (when parent
      (choice-focus-pane parent)
      (scroll-to-row parent label))
    (cond ((typep parent 'single-item-list)
           (let ((idx (alloy:element-index label parent)))
             (when idx
               (unless (eql idx (committed-index parent))
                 (setf (committed-index parent) idx)
                 (mark-list-children-for-render parent)
                 (notify-list-change parent idx)))))
          ((typep parent 'multi-item-list)
           (let ((idx (alloy:element-index label parent)))
             (when idx (multi-selection-gesture parent idx)))))))

;;; CAPI selects the row UNDER THE POINTER; alloy's component pointer-up activates the
;;; LIST, and a focus-list with no focused element then auto-picks index 0
;;; (alloy component.lisp: handle for (pointer-up component), alloy focus-tree.lisp: activate :around for focus-chain and (setf focus) :after for ((eql :strong) focus-list)).
;;; A native list-panel moves the selection with the arrow keys; they go through the
;;; very activation the click uses, so callback and repaint are identical to a mouse selection.
(defun list-labels (list)
  (let ((acc nil))
    (alloy:do-elements (e list :result (nreverse acc))
      (when (typep e 'list-label) (push e acc)))))

(defgeneric list-nav-index (list)
  (:documentation "Index the arrow keys move from, or NIL when nothing is selected.")
  (:method ((list single-item-list)) (committed-index list))
  (:method ((list multi-item-list)) (first (last (selection-indices list)))))

(defmethod alloy:handle ((event alloy:key-down) (list item-list))
  (let ((key (alloy:key event)))
    (if (not (member key '(:up :down)))
        (call-next-method)
        (let* ((labels (list-labels list))
               (n      (length labels)))
          (if (zerop n)
              (call-next-method)
              (let* ((step (if (eql key :down) 1 -1))
                     (cur  (list-nav-index list))
                     (next (if cur
                               (max 0 (min (1- n) (+ cur step)))
                               (if (eql key :down) 0 (1- n)))))
                (alloy:activate (nth next labels))
                t))))))

(defmethod alloy:handle ((event alloy:pointer-up) (list item-list))
  (let ((row (when (eql :left (alloy:kind event))
               (alloy:do-elements (e list :result nil)
                 (when (and (typep e 'list-label)
                            (alloy:contained-p (alloy:location event) e))
                   (return e))))))
    (if row
        (alloy:activate row)
        (call-next-method))))

;;; Double-click fires the list's :action-callback; single-click already fired the
;;; :selection-callback. Primary (not :after): the focus-element layout-event handler
;;; declines, and a non-local decline would skip an :after method.
(defmethod alloy:handle ((event alloy:pointer-double-click) (label list-label))
  (let ((parent (alloy:layout-parent label)))
    (when (typep parent 'item-list)
      (funcall-choice-callback parent (action-callback parent)))))

;;; The list API. The setters notify as a gesture would; a host setting the selection
;;; for the program wraps them in WITH-PROGRAMMATIC-SET.

(defun items (list)
  (alloy:value (alloy:data list)))

;;; Swapping the data moves the observer with it and resets the selection: CAPI's
;;; choice-selection is NIL for an empty list, so 0 would invent a selection.
(defun swap-list-data (list items)
  (let ((old (alloy:data list))
        (data (make-instance 'alloy::list-data :list items)))
    (when old (alloy:remove-observers 'alloy:element old))
    (setf (alloy:data list) data)
    (install-list-observers list data)
    (alloy:refresh list)))

(defgeneric set-items (list items)
  (:documentation "Replace LIST's items and reset its selection; returns ITEMS."))

(defmethod set-items ((list single-item-list) items)
  (swap-list-data list items)
  (setf (committed-index list) (if items 0 nil))
  items)

(defmethod set-items ((list multi-item-list) items)
  (swap-list-data list items)
  (setf (selection-indices list) nil)
  items)

(defun selected-index (list)
  "Index of the selected row of the single LIST, or NIL."
  (committed-index list))

(defun selected-indices (list)
  "Indices of the selected rows of the multiple LIST, ascending."
  (sort (copy-list (selection-indices list)) #'<))

(defun set-selected-index (list value)
  (cond ((null value)
         (setf (committed-index list) nil))
        (t
         (setf (committed-index list) value)
         (notify-list-change list value)))
  (mark-list-children-for-render list)
  value)

(defun set-selected-indices (list value)
  (setf (selection-indices list)
        (cond ((null value) nil)
              ((listp value) (copy-list value))
              (t (list value))))
  (mark-list-children-for-render list)
  (notify-list-change list (sort (copy-list (selection-indices list)) #'<))
  value)

(defgeneric select-index (list index)
  (:documentation "Add INDEX (or the indices of a list INDEX) to LIST's selection."))

(defmethod select-index ((list single-item-list) index)
  (setf (committed-index list) index)
  (mark-list-children-for-render list)
  (notify-list-change list index)
  index)

(defmethod select-index ((list multi-item-list) index)
  (let ((new (if (listp index) index (list index))))
    (dolist (i new) (pushnew i (selection-indices list))))
  (mark-list-children-for-render list)
  (notify-list-change list (sort (copy-list (selection-indices list)) #'<))
  (selection-indices list))

(defgeneric unselect-index (list index)
  (:documentation "Remove INDEX (or the indices of a list INDEX) from LIST's selection."))

(defmethod unselect-index ((list single-item-list) index)
  (when (eql (committed-index list) index)
    (setf (committed-index list) nil)
    (mark-list-children-for-render list)
    (notify-list-change list nil)))

(defmethod unselect-index ((list multi-item-list) index)
  (setf (selection-indices list)
        (if (listp index)
            (set-difference (selection-indices list) index)
            (remove index (selection-indices list))))
  (mark-list-children-for-render list)
  (notify-list-change list (sort (copy-list (selection-indices list)) #'<))
  (selection-indices list))

(defmethod selected-item ((list single-item-list))
  (let ((idx (committed-index list)))
    (when idx (nth idx (items list)))))

(defmethod selected-item ((list multi-item-list))
  (let ((items (items list)))
    (loop for i in (sort (copy-list (selection-indices list)) #'<)
          collect (nth i items))))

;;; The item is resolved through the widget's :test-function, which CAPI applies for
;;; (setf choice-selected-item).
(defgeneric set-selected-item (list value)
  (:documentation "Select the row(s) of LIST whose item matches VALUE under its test-function."))

(defmethod set-selected-item ((list single-item-list) value)
  (let ((pos (position value (items list) :test (test-function list))))
    (when pos
      (setf (committed-index list) pos)
      (mark-list-children-for-render list)
      (notify-list-change list pos))
    pos))

(defmethod set-selected-item ((list multi-item-list) value)
  (let* ((items (items list))
         (values (if (listp value) value (list value)))
         (indices (loop for v in values
                        for p = (position v items :test (test-function list))
                        when p collect p)))
    (setf (selection-indices list) indices)
    (mark-list-children-for-render list)
    (notify-list-change list (sort (copy-list indices) #'<))
    indices))

(defun list-label-selected-p (label)
  (let ((parent (alloy:layout-parent label)))
    (cond ((typep parent 'multi-item-list)
           (let ((idx (alloy:element-index label parent)))
             (and idx (member idx (selection-indices parent)))))
          ((typep parent 'single-item-list)
           (let ((idx (alloy:element-index label parent)))
             (and idx (eql idx (committed-index parent))))))))

(presentations:define-realization (window list-label)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:label simple:text)
   (alloy:margins 6 1)
   alloy:text
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :valign :middle))

;; list-label text has no native clip; constrain-visibility contains overflow to the row box.
(defmethod alloy:render :before ((renderer simple:renderer) (self list-label))
  (alloy:constrain-visibility self renderer))

(defgeneric list-label-text-width (label text)
  (:documentation "Px advance of TEXT in LABEL's font, which the label's intrinsic width is built from.")
  (:method ((label t) text)
    (let ((tree (alloy:layout-tree label)))
      (if (and tree (alloy:ui tree))
          (let ((e (widget-font-entry label (alloy:ui tree))))
            (alloy:with-unit-parent label
              (string-advance (font-entry-font e) (alloy:to-px (font-entry-size-unit e)) text)))
          0))))

;; Report intrinsic text width so an enclosing clip-view/scroll-view detects horizontal overflow.
(defmethod alloy:suggest-size (size (self list-label))
  (let* ((text (alloy:text self))
         (tw   (if (and text (plusp (length text))) (list-label-text-width self text) 0)))
    (alloy:px-size (max (alloy:pxw size) (+ (float tw 0f0) 14f0))
                   (alloy:pxh size))))

(presentations:define-update (window list-label)
  (:background
   :pattern (if (list-label-selected-p alloy:renderable)
                :notice-background
                :notice-foreground))
  (:label
   :pattern (if (list-label-selected-p alloy:renderable)
                :notice-foreground
                :foreground)
   :text alloy:text))

;;; The rows are dialog items too, so each is raised by another
;;; *dialog-item-z-elevation* over the list (widgets/dialog-item.lisp:
;;; alloy:render :around for dialog-item) and their opaque background
;;; would bury the frame -- the border has to outrank one such elevation.
(presentations:define-realization (window single-item-list)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins)
   :line-width (alloy:un 2)
   :z-index (1+ *dialog-item-z-elevation*)))

(presentations:define-update (window single-item-list)
  (:background
   :pattern (or (widget-background alloy:renderable) :notice-foreground))
  (:border :pattern colors:black :hidden-p nil))

(presentations:define-realization (window multi-item-list)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border simple:rectangle)
   (alloy:margins)
   :line-width (alloy:un 2)
   :z-index (1+ *dialog-item-z-elevation*)))

(presentations:define-update (window multi-item-list)
  (:background
   :pattern (or (widget-background alloy:renderable) :notice-foreground))
  (:border :pattern colors:black :hidden-p nil))

(defmethod alloy:refit :around ((list item-list))
  (let* ((original (alloy:bounds list))
         (shrunk (alloy:px-extent (+ (alloy:pxx original) 1)
                                  (+ (alloy:pxy original) 1)
                                  (max 0 (- (alloy:pxw original) 2))
                                  (max 0 (- (alloy:pxh original) 2)))))
    (setf (slot-value list 'alloy:bounds) shrunk)
    (unwind-protect (call-next-method)
      (setf (slot-value list 'alloy:bounds) original))))

;;; Pop-up (option-pane): a combo-set whose closed face shows the selected item and a
;;; chevron zone.

(defclass combo-item (alloy:combo-item)
  ((display-text :initarg :display-text :initform nil :accessor display-text)))

(defmethod alloy:text ((item combo-item))
  (or (display-text item) (call-next-method)))

(defclass pop-up-dialog-item (alloy:combo-set choice-mixin dialog-item) ()
  (:default-initargs
   :data (make-instance 'alloy:value-data :value nil)
   :value-set nil))

(defmethod selected-item ((combo pop-up-dialog-item))
  (alloy:value combo))

(defparameter *popup-chevron-width* 24
  "Width in px of the chevron zone at the right edge of a pop-up.")
(defparameter *popup-chevron* (chroma:rgb 0.0 0.47 0.83)
  "Fill of the chevron zone at rest.")
(defparameter *popup-chevron-hover* (chroma:rgb 0.2 0.62 0.93)
  "Fill of the chevron zone under the pointer.")
(defparameter *popup-chevron-mark* (chroma:rgb 1.0 1.0 1.0)
  "Colour of the chevron triangle.")

(defgeneric popup-chevron-mark (combo)
  (:documentation "Colour of COMBO's chevron triangle; the host may follow its own background preference.")
  (:method ((combo t)) *popup-chevron-mark*))

(defun popup-chevron-extent (combo)
  (let* ((b (alloy:bounds combo)) (w (alloy:pxw b)) (h (alloy:pxh b)))
    (alloy:px-extent (max 0 (- w *popup-chevron-width*)) 0 *popup-chevron-width* h)))

(defun popup-chevron-points (combo)
  "Downward-pointing triangle centered in the chevron zone (alloy y-up)."
  (let* ((b (alloy:bounds combo)) (w (alloy:pxw b)) (h (alloy:pxh b))
         (cx (- w (/ *popup-chevron-width* 2.0))) (cy (/ h 2.0))
         (hw 5.0) (hh 3.0))
    (list (alloy:px-point (- cx hw) (+ cy hh))
          (alloy:px-point (+ cx hw) (+ cy hh))
          (alloy:px-point cx (- cy hh)))))

(defmethod alloy:combo-item ((item t) (combo pop-up-dialog-item))
  (make-instance 'combo-item :value item))

(defmethod alloy:combo-item ((item cons) (combo pop-up-dialog-item))
  (make-instance 'combo-item :value (car item) :display-text (cdr item)))

(defun popup-sync-focused (combo)
  "Re-point FOCUSED at the element holding the current value: alloy does it from
value-changed (alloy components/combo.lisp: value-changed :after for combo), which observables fires BEFORE the write.
Matching goes through the widget's test-function, never eql: a string value rebuilt
outside the range is equal to its item but not eql."
  (let ((test (or (test-function combo) 'eql))
        (match nil))
    (setf match (alloy:do-elements (item combo :result nil)
                  (when (funcall test (alloy:value combo) (alloy:value item))
                    (return item))))
    (cond ((null match)
           ;; (setf focused) nil would break (alloy components/combo.lisp: (setf focused) :after for (t combo)) -- write the slot
           (setf (slot-value combo 'alloy::focused) nil))
          ((not (eq match (alloy:focused combo)))
           (setf (alloy:focused combo) match))))
  (alloy:mark-for-render combo))

;;; The label shown by the closed popup: alloy resolves it with eql (alloy components/combo.lisp: text for combo),
;;; which for strings only holds by identity, so a value from outside the range would
;;; fall back to printing the raw value instead of the item's text.
(defmethod alloy:text ((self pop-up-dialog-item))
  (if (alloy:focused self)
      (alloy:text (alloy:focused self))
      (let ((value (alloy:value self))
            (test  (or (test-function self) 'eql)))
        (alloy:do-elements (item self :result (princ-to-string value))
          (when (funcall test value (alloy:value item))
            (return (alloy:text item)))))))

;;; CAPI commits the choice-selection and only THEN calls the selection-callback;
;;; observables notifies :BEFORE the write (observables protocol.lisp: macro
;;; define-observable), so the commit point is this :after, not an observer.
(defmethod (setf alloy:value) :after (new-value (self pop-up-dialog-item))
  (declare (ignore new-value))
  (popup-sync-focused self)
  (unless *programmatic-set*
    (funcall-choice-callback self (selection-callback self))))

;;; alloy:refit sizes the dropdown, whose 20un min-size resolves through
;;; (ui (layout-tree *unit-parent*)); alloy guards its linear-layout refit the same
;;; way (alloy layouts/linear.lisp: refit for vertical-linear-layout) but not this one (alloy components/combo.lisp: refit for combo).
(defmethod alloy:refit :around ((self pop-up-dialog-item))
  (when (alloy:layout-tree self)
    (call-next-method)))

(defmethod alloy:handle :before ((ev alloy:pointer-move) (c pop-up-dialog-item))
  (when (and (null (alloy::state c)) (eql :strong (alloy:focus c)))
    (let ((*hover-move-p* nil))
      (unfocus-pane (let ((tree (alloy:layout-tree c))) (and tree (alloy:ui tree))) c))))

(presentations:define-realization (window combo-item)
  ((:background simple:rectangle)
   (alloy:margins)
   :z-index 100)
  ((:label simple:text)
   (alloy:margins 6 1)
   alloy:text
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :valign :middle
   :z-index 100))

(presentations:define-update (window combo-item)
  (:background
   :pattern (if (alloy:focus alloy:renderable) :notice-background :notice-foreground))
  (:label
   :pattern (if (alloy:focus alloy:renderable) :notice-foreground :foreground)))

(presentations:define-realization (window pop-up-dialog-item)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:chevron-bg simple:rectangle)
   (alloy:margins)
   :z-index 1)
  ((:label simple:text)
   (alloy:margins 6 1 26 1)
   alloy:text
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :valign :middle)
  ((:chevron simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 2)
  ((:border simple:rectangle)
   (alloy:margins)
   :line-width (alloy:un 2)
   :z-index 5))

(defmethod alloy:render :before ((renderer simple:renderer) (self pop-up-dialog-item))
  (alloy:constrain-visibility self renderer))

(presentations:define-update (window pop-up-dialog-item)
  (:background
   :pattern (or (widget-background alloy:renderable) colors:white))
  (:border
   :pattern colors:black
   :hidden-p nil)
  (:label
   :pattern (or (widget-foreground alloy:renderable) colors:black)
   :text alloy:text)
  (:chevron-bg
   :bounds (popup-chevron-extent alloy:renderable)
   :pattern (if (eql (alloy:focus alloy:renderable) :weak)
                *popup-chevron-hover*
                *popup-chevron*))
  (:chevron
   :points (popup-chevron-points alloy:renderable)
   :pattern (popup-chevron-mark alloy:renderable)))
