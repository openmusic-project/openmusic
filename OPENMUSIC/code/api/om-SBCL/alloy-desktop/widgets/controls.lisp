(in-package :alloy-desktop)

(defun wrap-text (string max-chars)
  "STRING in lines of at most MAX-CHARS characters, the capi:wrap-text contract (LW-lib/8-1-0-0/manual/html-w/capi/capi-capi-ref-551.htm:60)."
  ;; break at whitespace where one fits, cut the word where none does, drop the break
  ;; whitespace; the cap is a CHARACTER count, not a width
  (let ((n (length string)) (start 0) (lines '()))
    (flet ((spacep (c) (member c '(#\Space #\Tab))))
      (loop while (< start n)
            do (cond ((<= (- n start) max-chars)
                      (push (subseq string start) lines)
                      (setf start n))
                     (t
                      (let ((cut (position-if #'spacep string
                                              :start start
                                              :end (min n (+ start max-chars 1))
                                              :from-end t)))
                        (cond ((and cut (> cut start))
                               (push (subseq string start cut) lines)
                               (setf start (or (position-if-not #'spacep string :start cut) n)))
                              (t
                               (push (subseq string start (+ start max-chars)) lines)
                               (setf start (+ start max-chars)))))))))
    (or (nreverse lines) (list ""))))

(defun text-wrap-pix (string entry width-px)
  "The lines STRING occupies in the font-entry ENTRY inside WIDTH-PX device px.
A line is measured ONCE: the first character whose ending x passes WIDTH-PX fixes a
CHARACTER count, and the whole line is word-wrapped one character below it (wrap-text),
so every line of a paragraph carries the same cap. An embedded newline splits first and
each segment carries its own cap. Unbroken when ENTRY is NIL.
The measured walk (char-extents) can fall short of the glyph grid the line is drawn on
(graphics/font-metrics.lisp: pen-advance), so the cap is the first character past WIDTH-PX on
either; a later line still drawn wider than WIDTH-PX is broken again at the last blank that
fits on that grid, or inside a word with none."
  (cond ((null string) (list ""))
        ((find #\Newline string)
         (loop for line in (uiop:split-string string :separator '(#\Newline))
               append (text-wrap-pix line entry width-px)))
        ((null entry) (list string))
        (t
         (let* ((font (font-entry-font entry))
                (px (alloy:to-px (font-entry-size-unit entry)))
                (p-measured (position width-px (char-extents font px string) :test #'<))
                (data (org.shirakumo.alloy.renderers.opengl.msdf::data font))
                (n (length string))
                ;; first character whose drawn end passes WIDTH-PX: the pen travel only grows
                (p-drawn (when (> (pen-advance data string 0 n px) width-px)
                           (let ((lo 1) (hi n))
                             (loop while (< lo hi)
                                   do (let ((mid (floor (+ lo hi) 2)))
                                        (if (> (pen-advance data string 0 mid px) width-px)
                                            (setf hi mid)
                                            (setf lo (1+ mid)))))
                             (1- hi))))
                ;; the cap is fixed by whichever of the two walks passes WIDTH-PX first
                (p (cond ((and p-measured p-drawn) (min p-measured p-drawn))
                         (p-measured p-measured)
                         (t p-drawn))))
           (labels ((fit (line)
                      (let ((n (length line)))
                        (if (or (<= n 1) (<= (pen-advance data line 0 n px) width-px))
                            (list line)
                            (let* ((k (or (loop for i from (1- n) downto 1
                                                when (<= (pen-advance data line 0 i px) width-px)
                                                  return i)
                                          1))
                                   (blank (position #\Space line :end k :from-end t)))
                              (if (and blank (plusp blank))
                                  (let ((rest (string-left-trim " " (subseq line blank))))
                                    (cons (subseq line 0 blank)
                                          (and (plusp (length rest)) (fit rest))))
                                  (cons (subseq line 0 k) (fit (subseq line k)))))))))
             (loop for line in (if p (wrap-text string (max 1 (- p 1))) (list string))
                   append (fit line)))))))

;;; Static text: a label drawn by the host's rules (its own line breaks, font and
;;; baseline), with an optional one-pixel border on each edge.

(defclass static-text (alloy:label* dialog-item)
  ((alloy:value :initarg :text)))

;;; With :wrap (alloy:label, NIL by default) the text breaks at the widget's own width in
;;; the font it is drawn with, recomputed on each update: a text, font or size change.
(defmethod widget-label ((st static-text))
  (let ((text (call-next-method))
        (tree (alloy:layout-tree st)))
    (if (and (alloy:wrap st) tree (alloy:ui tree))
        (format nil "~{~A~^~%~}"
                (text-wrap-pix text
                               (widget-font-entry st (alloy:ui tree))
                               (alloy:pxw (alloy:bounds st))))
        text)))

(defun static-text-edge-bounds (st which)
  "Local bounds of ST's one-pixel border on edge WHICH (:border-top, :border-bottom, :border-left, :border-right)."
  (let* ((bb (alloy:bounds st))
         (w  (alloy:pxw bb))
         (h  (alloy:pxh bb)))
    (ecase which
      (:border-top    (alloy:px-extent 0 (max 0 (- h 1)) w 1))
      (:border-bottom (alloy:px-extent 0 0 w 1))
      (:border-left   (alloy:px-extent 0 0 1 h))
      (:border-right  (alloy:px-extent (max 0 (- w 1)) 0 1 h)))))

(defun static-text-label-bounds (st)
  "Bounds of ST's top-anchored label shape: left edge at 1, top pushed down by label-drop."
  (let* ((bb (alloy:bounds st))
         (w  (alloy:pxw bb))
         (h  (alloy:pxh bb)))
    (alloy:px-extent 1 0 w (max 0f0 (- h (label-drop st))))))

(presentations:define-realization (window static-text)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:border-top simple:rectangle)    (alloy:margins) :z-index 5)
  ((:border-bottom simple:rectangle) (alloy:margins) :z-index 5)
  ((:border-left simple:rectangle)   (alloy:margins) :z-index 5)
  ((:border-right simple:rectangle)  (alloy:margins) :z-index 5)
  ((:label simple:text)
   (static-text-label-bounds alloy:renderable)
   (widget-label alloy:renderable)
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   ;; the label already carries its line breaks
   :wrap nil
   :halign :start
   :valign :top))

(presentations:define-update (window static-text)
  (:background
   :hidden-p (null (widget-background alloy:renderable))
   :pattern (or (widget-background alloy:renderable) :background))
  (:border-top
   :hidden-p (null (widget-border alloy:renderable))
   :bounds (static-text-edge-bounds alloy:renderable :border-top)
   :pattern (or (widget-border alloy:renderable) colors:black))
  (:border-bottom
   :hidden-p (null (widget-border alloy:renderable))
   :bounds (static-text-edge-bounds alloy:renderable :border-bottom)
   :pattern (or (widget-border alloy:renderable) colors:black))
  (:border-left
   :hidden-p (null (widget-border alloy:renderable))
   :bounds (static-text-edge-bounds alloy:renderable :border-left)
   :pattern (or (widget-border alloy:renderable) colors:black))
  (:border-right
   :hidden-p (null (widget-border alloy:renderable))
   :bounds (static-text-edge-bounds alloy:renderable :border-right)
   :pattern (or (widget-border alloy:renderable) colors:black))
  (:label
   :pattern (if (widget-enabled-p alloy:renderable)
                (or (widget-foreground alloy:renderable) :foreground)
                *disabled-text*)
   :bounds (static-text-label-bounds alloy:renderable)
   :text (widget-label alloy:renderable)
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :wrap nil
   :halign :start
   :valign :top))

(defmethod alloy:render :around ((renderer alloy:renderer) (st static-text))
  ;; a pane clips its drawing to its bounds; restored by the enclosing pushed-transforms
  (alloy:constrain-visibility st renderer)
  (call-next-method))

;;; Hit order of a native dialog: a static text is a pinboard OBJECT drawn on the layout
;;; surface, while every other pane is a native child stacked above it, so a pane always
;;; takes the click where the two overlap.

(defun pinboard-object-p (element)
  "True for an element drawn on its layout's surface rather than as a child pane."
  (typep element '(or item-view static-text)))

(defun dispatch-in-capi-order (event layout)
  "Hand EVENT to LAYOUT's children, panes first and pinboard objects last; T when one handled it."
  ;; a declining child does not abort the sweep (alloy layout.lisp: handle for
  ;; (pointer-event layout) and handle for (pointer-move layout) do the opposite on both counts)
  (flet ((sweep (pinboard)
           (let ((handled nil))
             (alloy:do-elements (child layout :from-end t)
               (unless handled
                 (when (and (eq pinboard (and (pinboard-object-p child) t))
                            (event-touches-p event child)
                            (alloy:handle event child))
                   (setf handled t))))
             handled)))
    (or (sweep nil) (sweep t))))

;;; Text view: read-only display text.

(defclass text-view (alloy:label* dialog-item)
  ((alloy:value :initarg :text)))

(presentations:define-realization (window text-view)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:label simple:text)
   (alloy:margins 6 1)
   alloy:text
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :wrap (alloy:wrap alloy:renderable)
   :halign :start
   :valign :middle))

(presentations:define-update (window text-view)
  (:background
   :hidden-p (null (widget-background alloy:renderable))
   :pattern (or (widget-background alloy:renderable) :background))
  (:label
   :pattern (or (widget-foreground alloy:renderable) :foreground)
   :text alloy:text
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))))

;;; Push button: a rounded face that fades toward the hover colour, a border that marks
;;; the focus and the default button, and an underlined mnemonic while the cues are on.

(defparameter *hover-anim-speed* 7.0
  "Progress per second of a hover fade.")

(defun approach (value target dt &optional (speed *hover-anim-speed*))
  "VALUE moved toward TARGET by SPEED * DT, never past it."
  (if (< value target)
      (min target (+ value (* dt speed)))
      (max target (- value (* dt speed)))))

(defun lerp-color (a b u)
  "The opaque chroma colour at U in [0,1] between colours A and B."
  (chroma:rgb (+ (* (- 1.0 u) (chroma:r a)) (* u (chroma:r b)))
              (+ (* (- 1.0 u) (chroma:g a)) (* u (chroma:g b)))
              (+ (* (- 1.0 u) (chroma:b a)) (* u (chroma:b b)))
              1.0))

(defparameter *button-face* (chroma:rgb 0.96 0.96 0.96)
  "Face of a push button at rest.")
(defparameter *button-hover-face* (chroma:rgb 0.91 0.94 0.99)
  "Face of a push button under the pointer.")
(defparameter *button-active-face* (chroma:rgb 0.83 0.91 0.99)
  "Face of a push button while pressed.")
(defparameter *button-focus-border* (chroma:rgb 0.10 0.45 0.85)
  "Border of the focused push button and of the default one.")
(defparameter *button-border* (chroma:rgb 0.65 0.65 0.65)
  "Border of a push button at rest.")

(defclass button (alloy:button* dialog-item)
  ((default-p      :initarg :default-p :initform nil :accessor button-default-p)
   (cancel-p       :initarg :cancel-p  :initform nil :accessor button-cancel-p)
   (hover-progress :initform 0.0 :accessor hover-progress)
   (alloy:value    :initarg :text))
  (:default-initargs
   :on-activate (constantly nil)))

(presentations:define-realization (window button)
  ((:border-bg simple:rectangle)
   (alloy:margins)
   :z-index 0
   :corner-radii (vector (alloy:px 5) (alloy:px 5) (alloy:px 5) (alloy:px 5)))
  ((:background simple:rectangle)
   (alloy:margins 1 1 1 1)
   :z-index 10
   :corner-radii (vector (alloy:px 4) (alloy:px 4) (alloy:px 4) (alloy:px 4)))
  ((:label simple:text)
   (alloy:margins 6 1)
   alloy:text
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :valign :middle
   :halign :middle
   :z-index 20))

;;; The label carries NO vertical offset: :valign :middle centres the font's line box,
;;; which puts the cap box in the middle of the painted face, as a native push button does.
(presentations:define-update (window button)
  (:border-bg
   ;; the default button carries the focus-coloured border even unfocused, the way the
   ;; toolkit marks the button Return will press
   :pattern (if (or alloy:focus (button-default-p alloy:renderable))
                *button-focus-border*
                (or (widget-frame-border alloy:renderable) *button-border*))
   :hidden-p nil)
  (:background
   :pattern (or (widget-background alloy:renderable)
                (if (alloy:active-p alloy:renderable)
                    *button-active-face*
                    (lerp-color (or (widget-frame alloy:renderable) *button-face*)
                                *button-hover-face*
                                (hover-progress alloy:renderable)))))
  (:label
   :pattern (if (widget-enabled-p alloy:renderable)
                (or (widget-foreground alloy:renderable) colors:black)
                *disabled-text*)
   :text (widget-label alloy:renderable)
   :markup (mnemonic-markup alloy:renderable alloy:renderer)))

;;; A disabled push button ignores every activation, by pointer or by key.
(defmethod alloy:activate :around ((b button))
  (if (widget-enabled-p b)
      (call-next-method)
      b))

;;; The text a static text, a text view or a push button shows: read, and replaced from
;;; the next frame on.

(defgeneric widget-text (widget)
  (:documentation "The string WIDGET shows.")
  (:method ((widget static-text)) (alloy:value widget))
  (:method ((widget text-view)) (alloy:value widget))
  (:method ((widget button)) (alloy:value widget)))

(defgeneric (setf widget-text) (text widget)
  (:documentation "Make WIDGET show TEXT from its next frame on; answer TEXT.")
  (:method (text (widget static-text))
    (setf (alloy:value widget) text)
    (alloy:mark-for-render widget)
    text)
  (:method (text (widget text-view))
    (setf (alloy:value widget) text)
    (alloy:mark-for-render widget)
    text)
  (:method (text (widget button))
    (setf (alloy:value widget) text)
    (alloy:mark-for-render widget)
    text))

;;; RET goes to the pane that consumes it or to the default button, ESC to the cancel button, never down
;;; the focus tree; a popup surface answers both on the key-down (menu/menu.lisp: handle for (key-down popup-frame)).

(defgeneric consumes-return-p (element)
  (:documentation "True when RET belongs to ELEMENT itself, so it must not reach the window's
default button: a multi-line text pane spends it on the newline.")
  (:method ((element t)) nil)
  (:method ((element alloy:input-box)) t))

(defmethod alloy:handle :around ((event alloy:activate) (window window))
  (if (typep window 'popup-window)
      (call-next-method)
      (let ((pane (strong-focus-pane window)))
        (if (consumes-return-p pane)
            t
            (let ((default nil)
                  (focus-list (window-focus-list window)))
              (when focus-list
                (alloy:do-elements (e focus-list)
                  (when (and (typep e 'button) (button-default-p e))
                    (setf default e))))
              (when default
                (alloy:activate default))
              t)))))

(defmethod alloy:handle :around ((event alloy:exit) (window window))
  (if (typep window 'popup-window)
      (call-next-method)
      (let ((cancel nil)
            (focus-list (window-focus-list window)))
        (when focus-list
          (alloy:do-elements (e focus-list)
            (when (and (typep e 'button) (button-cancel-p e))
              (setf cancel e))))
        (when cancel
          (alloy:activate cancel))
        t)))

;;; Arrow keys never move the focus between the window's own panes, as in CAPI, where only a click,
;;; Tab and set-pane-focus do (capi-common-elements-ug-1.htm:104-124); the raw key-down still reaches the host.
;;; While a popup chain owned by the window is up, the chain navigates (menu/menu.lisp: the alloy:handle
;;; :around for (key-down window)) and the owner's own widgets keep their focus.
(defmethod alloy:handle :around ((event alloy:focus-event) (window window))
  "Consume a navigation that would move the owner's focus under an open popup chain, or an arrow reaching the root list."
  (let ((tree (alloy:focus-tree window)))
    (if (and (not (typep window 'popup-window))
             tree
             (or (popup-key-surface-for window)
                 (and (typep event '(or alloy:focus-up alloy:focus-down alloy:focus-left alloy:focus-right))
                      (eq (alloy:focused tree) (alloy:root tree)))))
        t
        (call-next-method))))

(defmethod org.shirakumo.alloy.animation:update :after ((b button) dt)
  (let ((target (if (member (alloy:focus b) '(:weak :strong)) 1.0 0.0))
        (p (hover-progress b)))
    (unless (= p target)
      (setf (hover-progress b) (approach p target dt))
      (alloy:mark-for-render b))))

;;; Leaving a button drops the :weak focus its hover took.
(defmethod alloy:handle :before ((ev alloy:pointer-move) (b alloy:button*))
  (unless (alloy:contained-p (alloy:location ev) b)
    (when (eql :weak (alloy:focus b))
      (setf (alloy:focus b) nil))))

(presentations:define-update (window alloy:button)
  (:border
   :pattern colors:black
   :hidden-p nil))

;;; Check box and radio button: a native-size indicator flush left and centred
;;; vertically, the caption after it; a click toggles only when it presses and
;;; releases inside the indicator.

(defparameter *indicator-un*
  #+win32  13
  #+darwin 16
  #-(or win32 darwin) 15
  "Side of the check-box square and of the radio-button circle, in LOGICAL units:
the size drawn at the unscaled resolution, scaled by the window's own content
scale like every other alloy un. The indicator sits flush against the left edge
of the item and is centred vertically, whatever the height of the item box.")

(defparameter *indicator-gap-un* 5
  "Clear space between the right edge of the indicator and the left edge of the
caption of a check box and of a radio button, in LOGICAL units. The same on every
platform, so the caption never crowds an indicator of any size.")

(defparameter *indicator-label-un* (+ *indicator-un* *indicator-gap-un*)
  "Left edge of the caption of a check box and of a radio button, taken from the
left edge of the item, in LOGICAL units: the indicator plus the gap.")

(defun indicator-px (element)
  "*indicator-un* in whole device pixels under ELEMENT's own unit context."
  (let ((sq (max 1 (round (alloy:with-unit-parent element
                            (alloy:to-px (alloy:un *indicator-un*)))))))
    ;; An indicator taller than its item is pinned to the item's floor by
    ;; indicator-y0 and loses its top row to the item's own clip; 0 = unallocated.
    #+darwin
    (let ((h (floor (alloy:pxh (alloy:bounds element)))))
      (when (plusp h) (setf sq (min sq h))))
    sq))

(defun indicator-label-px (element)
  "*indicator-label-un* in whole device pixels under ELEMENT's own unit context:
the left edge of the caption, taken from the left edge of the item.  NIL for an element
outside a realized layout tree, which carries no unit context to convert against."
  (let ((tree (alloy:layout-tree element)))
    (when (and tree (alloy:ui tree))
      (max 1 (round (alloy:with-unit-parent element
                      (alloy:to-px (alloy:un *indicator-label-un*))))))))

(defun indicator-y0 (element)
  "Bottom Y of the indicator inside ELEMENT: vertically centred, which is where
the toolkit puts it in an item of any height."
  (max 0.0 (/ (- (alloy:pxh (alloy:bounds element)) (indicator-px element)) 2.0)))

(defun indicator-extent ()
  (alloy:extent (alloy:px 0) (alloy:px 0)
                (alloy:un *indicator-un*)
                (alloy:un *indicator-un*)))

(defun indicator-bounds (cb)
  (let ((sq (indicator-px cb)))
    (alloy:px-extent 0 (indicator-y0 cb) sq sq)))

(defun indicator-inner-bounds (cb)
  (let ((sq (indicator-px cb)))
    (alloy:px-extent 1 (+ (indicator-y0 cb) 1) (- sq 2) (- sq 2))))

(defun event-in-square-p (cb event-location)
  "Hit-test EVENT-LOCATION (window-absolute px-point) against the visual square of CB."
  (let* ((gloc (alloy:global-location cb))
         (sq   (indicator-px cb))
         (sx0  (alloy:pxx gloc))
         (sy0  (+ (alloy:pxy gloc) (indicator-y0 cb)))
         (ex   (alloy:pxx event-location))
         (ey   (alloy:pxy event-location)))
    (and (<= sx0 ex (+ sx0 sq)) (<= sy0 ey (+ sy0 sq)))))

(defun event-in-circle-p (rb event-location)
  "Hit-test EVENT-LOCATION (window-absolute px-point) against the visual circle of RB."
  (let* ((gloc (alloy:global-location rb))
         (sq   (indicator-px rb))
         (cx   (+ (alloy:pxx gloc) (* 0.5 sq)))
         (cy   (+ (alloy:pxy gloc) (indicator-y0 rb) (* 0.5 sq)))
         (r    (* 0.5 sq))
         (dx   (- (alloy:pxx event-location) cx))
         (dy   (- (alloy:pxy event-location) cy)))
    (<= (+ (* dx dx) (* dy dy)) (* r r))))

(defclass indicator-mixin ()
  ((label               :initarg :label-text :initform "" :accessor indicator-label)
   (indicator-hovered-p :initform nil :accessor indicator-hovered-p)
   (pressed-p           :initform nil :accessor indicator-pressed-p)
   (hover-progress      :initform 0.0 :accessor hover-progress)
   (mark-progress       :initform 0.0 :accessor mark-progress))
  (:documentation "A toggle drawn as an indicator plus a caption; the progress slots drive its hover and mark animations."))

(defmethod widget-label ((w indicator-mixin))
  (indicator-label w))

(defclass check-box (indicator-mixin alloy:checkbox dialog-item) ()
  (:default-initargs
   :data (make-instance 'alloy:value-data :value nil)))

(defclass radio-button (indicator-mixin alloy:checkbox dialog-item)
  ((cluster :initarg :radio-button-cluster :initform nil :accessor radio-button-cluster))
  (:default-initargs
   :data (make-instance 'alloy:value-data :value nil)))

;;; Policy: what the host does with a gesture off the indicator and with a toggle.

(defgeneric indicator-hit-p (widget location)
  (:documentation "True when LOCATION (window-absolute px-point) is over WIDGET's indicator."))

(defmethod indicator-hit-p ((w check-box) location)
  (event-in-square-p w location))

(defmethod indicator-hit-p ((w radio-button) location)
  (event-in-circle-p w location))

(defun focus-on-click (widget)
  "Give WIDGET's focus parent the strong focus and WIDGET the weak one, as a click on a pane does."
  (when (slot-boundp widget 'alloy::focus-parent)
    (setf (alloy:focus (alloy:focus-parent widget)) :strong)
    (setf (alloy:focus widget) :weak)))

(defgeneric click-outside-indicator (widget event)
  (:documentation "Answer a press on WIDGET that misses its indicator.")
  (:method ((widget t) event)
    (declare (ignore event))
    (focus-on-click widget)))

(defgeneric release-outside-indicator (widget event)
  (:documentation "Answer a release on WIDGET whose press did not land on its indicator.")
  (:method ((widget t) event)
    (declare (ignore event))
    nil))

(defgeneric radio-may-uncheck-p (widget)
  (:documentation "True when a second click on the checked radio WIDGET clears it; a native radio never does.")
  (:method ((widget t)) nil))

(defgeneric indicator-activated (widget)
  (:documentation "Called after an enabled WIDGET toggled by a click or by activation. On a
radio button the default clears the other radio buttons of its cluster; a method that
reports the toggle calls the next method to keep that.")
  (:method ((widget t)) nil))

;;; Radio cluster exclusivity.

(defgeneric radio-cluster-candidates (radio)
  (:documentation "The elements among which RADIO's cluster is exclusive: by default the
children of RADIO's layout parent.")
  (:method ((radio radio-button))
    (let ((parent (layout-attached-p radio))
          (elements '()))
      (when parent
        (alloy:do-elements (element parent) (push element elements)))
      (nreverse elements))))

(defun uncheck-radio-cluster-siblings (radio)
  "Clear every other radio button of RADIO's cluster (RADIO-BUTTON-CLUSTER, compared with
EQUAL) among RADIO-CLUSTER-CANDIDATES."
  (dolist (item (radio-cluster-candidates radio))
    (when (and (typep item 'radio-button)
               (not (eq item radio))
               (equal (radio-button-cluster item) (radio-button-cluster radio)))
      (setf (alloy:value item) nil)
      (alloy:mark-for-render item))))

(defmethod indicator-activated ((radio radio-button))
  (uncheck-radio-cluster-siblings radio))

(defmethod clear-hover ((c check-box))
  (when (indicator-hovered-p c)
    (setf (indicator-hovered-p c) nil)
    (alloy:mark-for-render c)))

;;; The toolkit check button toggles on click; alloy:switch only activates on Enter
;;; (alloy components/button.lisp: a switch has no pointer-down or pointer-up handler).
(defmethod alloy:handle ((event alloy:pointer-down) (w indicator-mixin))
  (cond ((and (eql :left (alloy:kind event))
              (indicator-hit-p w (alloy:location event)))
         (setf (indicator-pressed-p w) t))
        (t (click-outside-indicator w event)))
  t)

(defmethod alloy:handle ((event alloy:pointer-up) (w indicator-mixin))
  (cond ((indicator-pressed-p w)
         (setf (indicator-pressed-p w) nil)
         (when (and (eql :left (alloy:kind event))
                    (indicator-hit-p w (alloy:location event)))
           (alloy:activate w)))
        (t (release-outside-indicator w event)))
  t)

(defmethod alloy:activate :after ((c check-box))
  (when (widget-enabled-p c) (indicator-activated c)))

;;; A disabled check box keeps its state against every activation, by pointer or by key.
(defmethod alloy:activate :around ((c check-box))
  (if (widget-enabled-p c)
      (call-next-method)
      c))

;;; A native radio selects but never clears on a second click; the host may opt a
;;; radio into clearing through radio-may-uncheck-p.
(defmethod alloy:activate ((r radio-button))
  (when (widget-enabled-p r)
    (let ((checked-p (alloy:value r)))
      (when (or (not checked-p) (radio-may-uncheck-p r))
        (setf (alloy:value r) (not checked-p))
        (indicator-activated r))))
  r)

;;; Hover: only the check box lights its ring.

(defun update-indicator-hover (widget location)
  "Synchronize WIDGET's indicator hover with LOCATION, independent of child dispatch."
  (let ((in-square (event-in-square-p widget location)))
    (unless (eq in-square (indicator-hovered-p widget))
      (setf (indicator-hovered-p widget) in-square)
      (alloy:mark-for-render widget))))

(defmethod alloy:handle :after ((event alloy:pointer-move) (c check-box))
  (update-indicator-hover c (alloy:location event)))

;;; Layout dispatch is allowed to stop at the first child that handles a move.
;;; Normalize checkbox hover once at the window boundary as well, so a sibling,
;;; overlay, or native-window edge cannot leave the indicator in its blue state.
(defmethod alloy:handle :after ((event alloy:pointer-move) (window window))
  (labels ((walk (el)
             (when (typep el 'check-box)
               (update-indicator-hover el (alloy:location event)))
             (when (typep el 'alloy:container)
               (alloy:do-elements (child el) (walk child)))))
    (walk (alloy:root (alloy:layout-tree window)))
    (walk (alloy:popups (alloy:layout-tree window)))))

;;; Animation: hover ring and check mark fade on the check box, the dot grows on the radio.

(defparameter *radio-anim-speed* 7.0
  "Progress per second of the radio dot.")

(defmethod org.shirakumo.alloy.animation:update :after ((c check-box) dt)
  (let ((target (if (indicator-hovered-p c) 1.0 0.0))
        (p (hover-progress c)))
    (unless (= p target)
      (setf (hover-progress c) (approach p target dt))
      (alloy:mark-for-render c)))
  (let ((target (if (alloy:value c) 1.0 0.0))
        (p (mark-progress c)))
    (unless (= p target)
      (setf (mark-progress c) (approach p target dt))
      (alloy:mark-for-render c))))

(defmethod org.shirakumo.alloy.animation:update :after ((r radio-button) dt)
  (let ((target (if (alloy:value r) 1.0 0.0))
        (p      (mark-progress r)))
    (unless (= p target)
      (setf (mark-progress r)
            (if (< p target)
                (min target (+ p (* dt *radio-anim-speed*)))
                (max target (- p (* dt *radio-anim-speed*)))))
      (alloy:mark-for-render r))))

(defun checkmark-points (cb)
  "Return the animated checkmark points inside CB's indicator bounds."
  (let* ((bounds (indicator-bounds cb))
         (x (alloy:pxx bounds))
         (y (alloy:pxy bounds))
         (w (alloy:pxw bounds))
         (h (alloy:pxh bounds))
         (cx (+ x (* 0.5 w)))
         (cy (+ y (* 0.5 h)))
         (p (mark-progress cb)))
    (mapcar (lambda (point)
              (destructuring-bind (rx ry) point
                (alloy:px-point (+ cx (* p (- (+ x (* rx w)) cx)))
                                (+ cy (* p (- (+ y (* ry h)) cy))))))
            '((0.25 0.5) (0.4375 0.3125) (0.75 0.6875)))))

(defun checkmark-line-width (cb)
  (alloy:px (* 1.5 (/ (alloy:pxw (indicator-bounds cb)) 16.0))))

(defun check-fill-bounds (cb)
  "Centered animated fill growing inside the inner box (ring stays visible)."
  (let* ((p   (mark-progress cb))
         (sq  (indicator-px cb))
         (cx  (* 0.5 sq))
         (cy  (+ (indicator-y0 cb) (* 0.5 sq)))
         (w   (* (- sq 2) p))
         (h   (* (- sq 2) p)))
    (alloy:extent (alloy:px (- cx (* 0.5 w)))
                  (alloy:px (- cy (* 0.5 h)))
                  (alloy:px w)
                  (alloy:px h))))

(defun radio-dot-bounds (r)
  "The dot, centred on the indicator circle and sized from IT, not from the item
box: the indicator is a fixed native size whatever the item's height."
  (let* ((sq   (indicator-px r))
         (p    (mark-progress r))
         (ease (- 1.0 (expt (- 1.0 p) 2)))
         (size (* ease 0.34 sq))
         (cx   (* 0.5 sq))
         (cy   (+ (indicator-y0 r) (* 0.5 sq))))
    (alloy:px-extent (- cx (/ size 2.0)) (- cy (/ size 2.0)) size size)))

;;; Colours of the native controls: fixed, not the host's preferred palette.

(defparameter *indicator-ring* (chroma:rgb 0.3 0.3 0.3)
  "Ring of a check box at rest; it fades toward *button-focus-border* under the pointer.")
(defparameter *radio-ring* (chroma:rgb 0.5 0.5 0.5)
  "Ring of a radio button.")
(defparameter *slider-track* (chroma:rgb 0.85 0.85 0.85)
  "Track of a slider.")
(defparameter *slider-line* (chroma:rgb 0.5 0.5 0.5)
  "Track border, ticks and dark bevel edges of a slider.")
(defparameter *slider-bevel-light* (chroma:rgb 1.0 1.0 1.0)
  "Light bevel edges of a slider.")
(defparameter *slider-thumb* (chroma:rgb 0.0 0.47 0.83)
  "Thumb of a slider at rest.")
(defparameter *slider-thumb-hover* (chroma:rgb 0.0 0.0 0.0)
  "Thumb of a slider under the pointer.")
(defparameter *slider-thumb-active* (chroma:rgb 0.7 0.7 0.7)
  "Thumb of a slider while dragged.")

(presentations:define-realization (window check-box)
  ;; Pane rect: first at z 0, so the later z-0 shapes paint over it (same shape
  ;; order as the realization of static-text).
  ((:frame simple:rectangle)
   (alloy:margins)
   :z-index 0)
  ;; Ring = 2 filled rects (border behind, bg inset 1px): outlined-rectangle bakes
  ;; geometry at shared-initialize and may never show.
  ((:box-border simple:rectangle)
   (indicator-extent)
   :corner-radii (alloy:un 2)
   :z-index 0)
  ((:box-bg simple:rectangle)
   (indicator-extent)
   :corner-radii (alloy:un 1)
   :z-index 1)
  ((:fill simple:rectangle)
   (indicator-extent)
   :corner-radii (alloy:un 1)
   :z-index 6)
  ((:check simple:line-strip)
   (checkmark-points alloy:renderable)
   :pattern :notice-foreground
   :line-width (checkmark-line-width alloy:renderable)
   :z-index 10)
  ((:label simple:text)
   (alloy:margins *indicator-label-un* 0 0 0)
   ""
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :halign :start :valign :middle))

(presentations:define-update (window check-box)
  (:frame
   :hidden-p (null (widget-frame alloy:renderable))
   :pattern (or (widget-background alloy:renderable)
                (widget-frame alloy:renderable)
                colors:white))
  (:box-bg
   :pattern (or (widget-background alloy:renderable) *button-face*)
   :bounds (indicator-inner-bounds alloy:renderable))
  (:box-border
   :pattern (lerp-color *indicator-ring* *button-focus-border* (hover-progress alloy:renderable))
   :bounds (indicator-bounds alloy:renderable))
  (:fill
   :hidden-p (zerop (mark-progress alloy:renderable))
   :pattern :notice-background
   :bounds (check-fill-bounds alloy:renderable))
  (:check
   :hidden-p (zerop (mark-progress alloy:renderable))
   :pattern :notice-foreground
   :points (checkmark-points alloy:renderable)
   :line-width (checkmark-line-width alloy:renderable))
  (:label
   :text (widget-label alloy:renderable)
   :markup (mnemonic-markup alloy:renderable alloy:renderer)
   :pattern (if (widget-enabled-p alloy:renderable)
                (or (widget-foreground alloy:renderable) :foreground)
                *disabled-text*)))

;; A native check button clips its drawing to its pane; the MSDF caption has no native clip.
(defmethod alloy:render :before ((renderer simple:renderer) (c check-box))
  (alloy:constrain-visibility c renderer))

(presentations:define-realization (window radio-button)
  ;; Pane rect: first at z 0, so the later z-0 shapes paint over it (same shape
  ;; order as the realization of static-text).
  ((:frame simple:rectangle)
   (alloy:margins)
   :z-index 0)
  ((:box-border simple:ellipse)
   (indicator-extent)
   :z-index 0)
  ((:box-bg simple:ellipse)
   (indicator-extent)
   :z-index 1)
  ((:inner simple:ellipse)
   (indicator-extent)
   :z-index 2)
  ((:label simple:text)
   (alloy:margins *indicator-label-un* 0 0 0)
   ""
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :halign :start :valign :middle))

(presentations:define-update (window radio-button)
  (:frame
   :hidden-p (null (widget-frame alloy:renderable))
   :pattern (or (widget-background alloy:renderable)
                (widget-frame alloy:renderable)
                colors:white))
  (:box-border
   :pattern *radio-ring*
   :bounds (indicator-bounds alloy:renderable))
  (:box-bg
   :pattern (if (alloy:value alloy:renderable)
                :notice-background
                (or (widget-background alloy:renderable) *button-face*))
   :bounds (indicator-inner-bounds alloy:renderable))
  (:inner
   :hidden-p (zerop (mark-progress alloy:renderable))
   :pattern :notice-foreground
   :bounds (radio-dot-bounds alloy:renderable))
  (:label
   :text (widget-label alloy:renderable)
   :markup (mnemonic-markup alloy:renderable alloy:renderer)
   :pattern (if (widget-enabled-p alloy:renderable)
                (or (widget-foreground alloy:renderable) :foreground)
                *disabled-text*)))

;; A native radio button clips its drawing to its pane; the MSDF caption has no native clip.
(defmethod alloy:render :before ((renderer simple:renderer) (r radio-button))
  (alloy:constrain-visibility r renderer))

;;; Slider: pentagon thumb pointing at the ticks, ticks, bevelled edge.

(defclass slider (alloy:ranged-slider dialog-item)
  ((increment :initarg :increment :initform 1 :accessor slider-increment)
   (hovered-p :initform nil :accessor slider-hovered-p))
  (:default-initargs
   :data (make-instance 'alloy:value-data :value 0)))

(defparameter *slider-tick-count* 17)

(defun slider-thumb-points (sl)
  "Flag pentagon thumb, pointing toward the tick marks."
  (let ((w (alloy:pxw sl))
        (h (alloy:pxh sl))
        (frac (alloy:slider-unit sl)))
    (ecase (alloy:orientation sl)
      (:horizontal
       (let* ((th  (max 12.0 (* 0.40 h)))
              (tw  (max 6.0 (* 0.6 th)))
              (top (+ (* 0.65 h) (/ th 2)))
              (sh  (* 0.73 th))
              (x0  (* frac (- w tw))))
         (list (alloy:px-point x0 top)
               (alloy:px-point (+ x0 tw) top)
               (alloy:px-point (+ x0 tw) (- top sh))
               (alloy:px-point (+ x0 (/ tw 2)) (- top th))
               (alloy:px-point x0 (- top sh)))))
      (:vertical
       (let* ((tw   (max 12.0 (* 0.40 w)))
              (th   (max 6.0 (* 0.6 tw)))
              (left (- (* 0.35 w) (/ tw 2)))
              (sh   (* 0.73 tw))
              (y0   (* frac (- h th))))
         (list (alloy:px-point left y0)
               (alloy:px-point left (+ y0 th))
               (alloy:px-point (+ left sh) (+ y0 th))
               (alloy:px-point (+ left tw) (+ y0 (/ th 2)))
               (alloy:px-point (+ left sh) y0)))))))

(defparameter *slider-dark-edge-px* 2.0)

(defun slider-edge-dark-p (sl which)
  (declare (ignore sl))
  (and (member which '(:edge-top :edge-left)) t))

(defun slider-edge-thick (sl which)
  (if (slider-edge-dark-p sl which) *slider-dark-edge-px* 1.0))

(defun slider-edge-bounds (sl which)
  (let* ((b (alloy:bounds sl)) (w (alloy:pxw b)) (h (alloy:pxh b))
         (tk (slider-edge-thick sl which)))
    (ecase which
      (:edge-top    (alloy:px-extent 0 (- h tk) w tk))
      (:edge-bottom (alloy:px-extent 0 0 w tk))
      (:edge-left   (alloy:px-extent 0 0 tk h))
      (:edge-right  (alloy:px-extent (- w tk) 0 tk h)))))

(defgeneric slider-bevel-light (slider)
  (:documentation "Colour of SLIDER's light bevel edges; the host may follow its own background preference.")
  (:method ((slider t)) *slider-bevel-light*))

(defun slider-edge-pattern (sl which)
  (if (slider-edge-dark-p sl which) *slider-line* (slider-bevel-light sl)))

(defun slider-track-extent (sl)
  (ecase (alloy:orientation sl)
    (:horizontal (alloy:extent (alloy:pw 0.02) (alloy:ph 0.58) (alloy:pw 0.96) (alloy:ph 0.14)))
    (:vertical   (alloy:extent (alloy:pw 0.28) (alloy:ph 0.02) (alloy:pw 0.14) (alloy:ph 0.96)))))

(defmethod presentations:realize-renderable ((renderer window) (sl slider))
  (presentations:clear-shapes sl)
  (setf (presentations:find-shape :background sl)
        (simple:rectangle renderer (alloy:margins) :name :background :palette sl))
  (dolist (edge '(:edge-top :edge-bottom :edge-left :edge-right))
    (setf (presentations:find-shape edge sl)
          (simple:rectangle renderer (alloy:margins) :name edge :palette sl)))
  (setf (presentations:find-shape :track sl)
        (simple:rectangle renderer (slider-track-extent sl) :name :track :palette sl))
  (setf (presentations:find-shape :track-border sl)
        (simple:rectangle renderer (slider-track-extent sl) :line-width (alloy:px 1)
                          :name :track-border :palette sl))
  (loop with n = *slider-tick-count*
        for i from 0 below n
        for f = (+ 0.02 (* 0.96 (/ i (float (1- n)))))
        do (setf (presentations:find-shape (intern (format nil "TICK-~D" i) :keyword) sl)
                 (simple:rectangle renderer
                                   (ecase (alloy:orientation sl)
                                     (:horizontal (alloy:extent (alloy:pw f) (alloy:ph 0.15)
                                                                (alloy:px 1) (alloy:ph 0.13)))
                                     (:vertical   (alloy:extent (alloy:pw 0.72) (alloy:ph f)
                                                                (alloy:pw 0.13) (alloy:px 1))))
                                   :name :tick :palette sl)))
  (setf (presentations:find-shape :handle sl)
        (simple:polygon renderer
                        (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
                        :name :handle :palette sl))
  sl)

(defmethod alloy:handle :before ((ev alloy:pointer-move) (sl slider))
  (let ((h (and (alloy:contained-p (alloy:location ev) sl) t)))
    (unless (eq h (slider-hovered-p sl))
      (setf (slider-hovered-p sl) h)
      (alloy:mark-for-render sl))))

(defmethod clear-hover ((s slider))
  (when (slider-hovered-p s)
    (setf (slider-hovered-p s) nil)
    (alloy:mark-for-render s)))

(presentations:define-update (window slider)
  (:background
   :hidden-p (null (widget-background alloy:renderable))
   :pattern (or (widget-background alloy:renderable) :background)
   :z-index -3)
  (:edge-top
   :bounds (slider-edge-bounds alloy:renderable :edge-top)
   :pattern (slider-edge-pattern alloy:renderable :edge-top))
  (:edge-right
   :bounds (slider-edge-bounds alloy:renderable :edge-right)
   :pattern (slider-edge-pattern alloy:renderable :edge-right))
  (:edge-bottom
   :bounds (slider-edge-bounds alloy:renderable :edge-bottom)
   :pattern (slider-edge-pattern alloy:renderable :edge-bottom))
  (:edge-left
   :bounds (slider-edge-bounds alloy:renderable :edge-left)
   :pattern (slider-edge-pattern alloy:renderable :edge-left))
  (:track
   :pattern *slider-track*)
  (:track-border
   :pattern *slider-line*)
  (:tick
   :pattern *slider-line*)
  (:handle
   :points (slider-thumb-points alloy:renderable)
   :offset (alloy:px-point 0 0)
   :pattern (cond ((alloy:active-p alloy:renderable) *slider-thumb-active*)
                  ((slider-hovered-p alloy:renderable) *slider-thumb-hover*)
                  (t *slider-thumb*))))

;;; Wheel routing: a slider takes the wheel only under its track or thumb; a scrollbar
;;; never takes it (the clip or the window does), as with native sliders and scroll bars.

(defun slider-wheel-hit-p (slider event)
  "Whether EVENT is over SLIDER's rendered track or thumb, in window coordinates."
  (alloy:with-unit-parent slider
    (let* ((origin (alloy:global-location slider))
           (x (- (alloy:pxx (alloy:location event)) (alloy:pxx origin)))
           (y (- (alloy:pxy (alloy:location event)) (alloy:pxy origin)))
           (track (slider-track-extent slider))
           (thumb (slider-thumb-points slider)))
      (flet ((inside-extent-p (extent)
               (and (<= (alloy:pxx extent) x (+ (alloy:pxx extent) (alloy:pxw extent)))
                    (<= (alloy:pxy extent) y (+ (alloy:pxy extent) (alloy:pxh extent)))))
             (inside-thumb-p ()
               (and thumb
                    (<= (reduce #'min thumb :key #'alloy:pxx) x
                        (reduce #'max thumb :key #'alloy:pxx))
                    (<= (reduce #'min thumb :key #'alloy:pxy) y
                        (reduce #'max thumb :key #'alloy:pxy)))))
        (or (inside-extent-p track) (inside-thumb-p))))))

(defmethod alloy:handle ((event alloy:scroll) (slider slider))
  (if (slider-wheel-hit-p slider event)
      (call-next-method)
      (alloy:decline)))
