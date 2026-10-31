(in-package :om-api)

(export '(om-view-key-handler
          om-view-click-handler
          om-view-doubleclick-handler
          om-view-mouse-enter-handler
          om-view-mouse-leave-handler
          om-window-mouse-moved-handler

          om-init-motion
          om-click-motion-handler
          om-click-release-handler

          om-shift-key-p
          om-command-key-p
          om-option-key-p

          om-mouse-position
          om-zoom-touch-callback)
        :om-api)

;;; om-LW get-om-spec-modifiers: the snapshots live in alloy-desktop
;;; (input/modifiers.lisp: shift-key-p, command-key-p, option-key-p).
(defun om-shift-key-p   () (alld:shift-key-p))
(defun om-command-key-p () (alld:command-key-p))
(defun om-option-key-p  () (alld:option-key-p))

;;; The hover view keeps vanilla's special (om-LW/actions.lisp: defvar *last-containing-view*):
;;; projects read it by name (projects midi/tools/midi-save.lisp: save-as-midi for maquette-obj).
(defvar *last-containing-view* nil)

(defun %retire-hover-view (window)
  "Forget the hover view when it belongs to the retiring WINDOW."
  (when (and *last-containing-view*
             (eq (alld:element-window *last-containing-view*) window))
    (setf *last-containing-view* nil)))

(defun %capture-gesture-pane (view)
  "Grab the pane the press belongs to: an om-item-view is a pinboard-object drawn on
its container pane, so the gesture is the container's."
  (alld:capture-pointer (or (and (om-item-view-p view) (item-container view)) view)))

(defun %leave-hover-view (view)
  "om-LW internal-motion-callback leave step: the frame gets its leave handler and an
item-view hands the cursor back to its container
(om-LW/actions.lisp: internal-motion-callback for om-graphic-object)."
  (om-view-mouse-leave-handler view)
  (when (om-item-view-p view)
    (update-view-cursor (item-container view))))

(defun %update-hover-tooltip (target)
  "om-LW internal-motion-callback tooltip trigger: show TARGET's help while the
command key is held, hide otherwise; rebuilt only when the hovered frame changes."
  (let ((want  (and target (om-command-key-p) target))
        (state (alld:pointer-state)))
    (unless (eql want (alld:tooltip-hover-element state))
      (om-hide-tooltip (or (alld:tooltip-hover-element state) target))
      (when want (om-show-tooltip want))
      (setf (alld:tooltip-hover-element state) want))))

;;; An om-item-view is no layout element, so the window that owns a view is the
;;; OM one; the pointer state is retired through it at the window's funeral.
(defmethod alld:element-window ((element om-graphic-object))
  (om-view-window element))

(alld:register-window-retirement :om-hover-view #'%retire-hover-view :order 10)


(defgeneric om-view-click-handler         (view position))
(defgeneric om-view-doubleclick-handler   (view position))
(defgeneric om-view-mouse-enter-handler   (view))
(defgeneric om-view-mouse-leave-handler   (view))
(defgeneric om-window-mouse-moved-handler (window position))
(defgeneric om-init-motion                (view x y))
(defgeneric om-click-motion-handler       (view position))
(defgeneric om-click-release-handler      (view position))
(defgeneric om-view-key-handler           (view key))
(defgeneric om-view-scroll-handler        (view dx dy modifiers))
(defgeneric om-zoom-touch-callback        (self &rest args))
(defgeneric update-view-cursor            (view))

(defmethod om-view-click-handler         ((self t) position) (declare (ignore position)) nil)
(defmethod om-view-doubleclick-handler   ((self t) position) (declare (ignore position)) nil)

;;; vanilla returns SELF (om-LW/actions.lisp: om-view-doubleclick-handler for
;;; om-abstract-window and om-graphic-object); kernel tests
;;; (equal self (call-next-method)) to run panel default actions
(defmethod om-view-doubleclick-handler ((self om-abstract-window) pos) (declare (ignore pos)) self)
(defmethod om-view-doubleclick-handler ((self om-graphic-object) pos) (declare (ignore pos)) self)

(defmethod om-view-doubleclick-handler :before ((self om-graphic-object) pos)
  (declare (ignore pos))
  (setf (alld:clicked-element (alld:pointer-state)) nil))
(defmethod om-view-mouse-enter-handler   ((self t))                                       nil)
(defmethod om-view-mouse-leave-handler   ((self t))                                       nil)
(defmethod om-window-mouse-moved-handler ((self t) position) (declare (ignore position)) nil)

;;; vanilla base returns (om-LW/actions.lisp: om-view-click-handler for
;;; om-abstract-window, om-window-mouse-moved-handler, om-view-mouse-leave-handler):
;;; a window consumes its own click, and enter/leave report handled, so kernel
;;; :before/:after methods run.
(defmethod om-view-click-handler ((self om-abstract-window) position)
  (declare (ignore position))
  self)
(defmethod om-view-mouse-enter-handler   ((self om-graphic-object))                       t)
(defmethod om-view-mouse-leave-handler   ((self om-graphic-object))                       t)
(defmethod om-window-mouse-moved-handler ((self om-abstract-window) position)
  (declare (ignore position))
  t)
(defmethod om-init-motion                ((self t) x y)      (declare (ignore x y))      nil)
(defmethod om-click-motion-handler       ((self t) position) (declare (ignore position)) t)
(defmethod om-click-release-handler      ((self t) position) (declare (ignore position)) nil)
(defmethod om-view-key-handler           ((self t) key)      (declare (ignore key))      nil)
(defmethod om-view-scroll-handler        ((self t) dx dy modifiers) (declare (ignore dx dy modifiers)) nil)
(defmethod om-zoom-touch-callback        ((self om-graphic-object) &rest args) (declare (ignore args)) nil)
(defmethod om-zoom-touch-callback        ((self t) &rest args) (declare (ignore args)) nil)
(defmethod om-view-cursor                ((self t))                                       nil)
(defmethod om-set-view-cursor            ((self t) cursor)   (declare (ignore cursor))   nil)
(defmethod update-view-cursor            ((self t))                                       nil)

(defmethod update-view-cursor ((self om-view))
  (let ((window (om-view-window self)))
    (when window
      (setf (alloy:cursor window) (or (om-view-cursor self) :default)))))

(defmethod update-view-cursor ((self om-item-view))
  (let* ((container (item-container self))
         (window    (and container (om-view-window container))))
    (when window
      (setf (alloy:cursor window) (or (om-view-cursor self) :default)))))

(defun %alloy-location-to-om-point (location view)
  "LOCATION (window-absolute, y-up) as an OM point in VIEW's CONTENT coordinates
(y-down, integers): the geometry pipeline supplies the ancestors' scroll and VIEW's
own scroll is added last, as om-LW does (om-LW/actions.lisp: apply-in-item-subview and apply-in-subview).
CAPI ordinary scrolling: input-gesture coordinates are canvas-relative, not
viewport-relative (capi-output-panes-ug-4.htm:32)."
  (multiple-value-bind (gx gy)
      (alloy:with-unit-parent view (alloy::compute-global-position view))
    (let ((lx (- (alloy:pxx location) gx))
          (ly (- (alloy:pxy location) gy))
          (vh (alloy:pxh (alloy:bounds view))))
      (om-make-point (+ (round lx) (om-h-scroll-position view))
                     (+ (round (- vh ly)) (om-v-scroll-position view))))))

(defparameter *om-modifier-keys*
  '(:left-shift :right-shift :left-control :right-control
    :left-alt :right-alt :left-super :right-super
    :caps-lock :num-lock :scroll-lock :menu)
  "Keys CAPI never turns into a gesture: pressing a modifier alone is not a keystroke.")

(defun %glfw-key-to-om-key (alloy-key)
  (case alloy-key
    (:up        :om-key-up)
    (:down      :om-key-down)
    (:left      :om-key-left)
    (:right     :om-key-right)
    (:home      :om-key-home)
    (:end       :om-key-end)
    (:page-up   :om-key-pageup)
    (:page-down :om-key-pagedown)
    (:backspace :om-key-delete)
    (:delete    :om-key-delete)
    ;; the main Return key carries char 13 in a CAPI gesture-spec, which vanilla maps
    ;; to :om-key-return (om-LW/actions.lisp: get-om-character); only the keypad key
    ;; is :om-key-enter (om-LW/actions.lisp: get-om-spec-character). The kernel
    ;; branches on the difference (kernel nonrelationcontainer.lisp: the
    ;; :om-key-return branch of handle-key-event for nonrelationPanel;
    ;; projects basicproject/classes/picture.lisp: handle-key-event for pictpanel).
    (:enter     :om-key-return)
    (:return    :om-key-return)
    (:kp-enter  :om-key-enter)
    (:escape    :om-key-esc)
    (:tab       :om-key-tab)
    ;; vanilla get-om-spec-character ends in NIL for an unmapped gesture (om-LW/actions.lisp: get-om-spec-character)
    (t          nil)))

(defun %printable-key-char (alloy-key)
  "Base character for a printable ALLOY-KEY keyword, or NIL. Used to feed the
kernel char-based key chain (handle-key-event) when a modifier suppresses
text-events. Shift is ignored: the base glyph is returned."
  (case alloy-key
    (:equal #\=) (:minus #\-) (:comma #\,) (:period #\.) (:slash #\/)
    (:semicolon #\;) (:apostrophe #\') (:backslash #\\)
    (:left-bracket #\[) (:right-bracket #\]) (:grave-accent #\`)
    (:space #\Space)
    ;; an unmapped key arrives as its raw code (Windows sends 162 for Control)
    (t (when (symbolp alloy-key)
         (let ((name (symbol-name alloy-key)))
           (when (= 1 (length name))
             (char-downcase (char name 0))))))))

(defmethod alloy:handle :before ((event alloy:key-event) (window om-look-window))
  ;; a window opened from here on shows its keyboard cues, as UIS_INITIALIZE decides
  ;; them from the last input event (alloy-desktop window/cues.lisp: init-keyboard-cues)
  (alld:note-input-source t)
  (alld:note-modifiers (alloy:modifiers event))
  (alld:note-keyboard-cue-event window event))

;;; the cursor snapshot is taken by alloy-desktop (input/pointer.lisp: handle :before
;;; for (pointer-event window))
(defmethod alloy:handle :before ((event alloy:pointer-event) (window om-look-window))
  (when (typep event 'alloy:pointer-down)
    (alld:note-input-source nil)))

;;; the :weak focus cleanup and the capture redirection follow in alloy-desktop
;;; (input/pointer.lisp: handle for (pointer-move window))
(defmethod alloy:handle :before ((event alloy:pointer-move) (window om-look-window))
  (let ((loc (alloy:location event)))
    (when (and *last-containing-view*
               (not (alloy:contained-p loc *last-containing-view*)))
      (%leave-hover-view *last-containing-view*)
      (%update-hover-tooltip nil)
      (setf *last-containing-view* nil))))

(defmethod alloy:handle ((event windowing:pointer-leave) (window om-look-window))
  (%update-hover-tooltip nil)
  (when *last-containing-view*
    (%leave-hover-view *last-containing-view*)
    (setf *last-containing-view* nil))
  (alloy:decline))

;;; Vanilla om-view answers capi::accepts-focus-p (om-LW/actions.lisp: capi::accepts-focus-p for om-view), so
;;; clicking a panel or a box frame takes the focus off the text input NATIVELY.
;;; Alloy never does: om-view is no focus-element (view.lisp: the defclass om-view) and the layout only
;;; routes a pointer to whoever contains it (alloy layout.lisp: handle for (pointer-event layout)), so the pane
;;; keeps :strong and swallows every key — DELETE included
;;; (alloy components/text-input.lisp: handle for (key-down text-input-component)).

(defun %release-text-focus (view)
  "Drop the strong focus of VIEW's window text pane, as a CAPI click does."
  (let* ((window (om-view-window view))
         (pane   (and window (%om-strong-pane window))))
    (when (typep pane 'alloy:text-input-component)
      (%om-unfocus-pane window pane))))

(defun %clear-selection-on-inner-click (view)
  "A click anywhere inside a patch editor undoes the text selection.
Must run BEFORE the children dispatch: a box consumes the click in its own
widget, so the background branch below never sees it."
  (let ((window (om-view-window view)))
    (when (and window (alld:click-clears-selection-p window))
      (alld:release-text-selection window))))

(defmethod alloy:handle ((event alloy:pointer-down) (view om-view))
  (%clear-selection-on-inner-click view)
  (or (alld:dispatch-to-children event view)
      (let ((pos (%alloy-location-to-om-point (alloy:location event) view)))
        (case (alloy:kind event)
          (:right (om-context-menu-callback view (om-point-h pos) (om-point-v pos)))
          (otherwise
           (%release-text-focus view)
           (%capture-gesture-pane view)
           (setf (alld:clicked-element (alld:pointer-state)) view)
           (om-view-click-handler view pos))))
      (alloy:decline)))

;;; om-item-views are CAPI pinboard-objects: a click on one is consumed by it
;;; (kernel handlers live on the subviews, e.g. icon-finder-icon), never falling
;;; through to the panel's rubber-band.

(defmethod alloy:handle ((event alloy:pointer-down) (view om-item-view))
  (let ((pos (%alloy-location-to-om-point (alloy:location event) view)))
    (case (alloy:kind event)
      (:right
       (om-context-menu-callback view (om-point-h pos) (om-point-v pos))
       t)
      (otherwise
       (%capture-gesture-pane view)
       (setf (alld:clicked-element (alld:pointer-state)) view)
       (om-view-click-handler view pos)
       t))))

(defmethod alloy:handle ((event alloy:pointer-double-click) (view om-item-view))
  (let ((pos (%alloy-location-to-om-point (alloy:location event) view)))
    (om-view-doubleclick-handler view pos)
    t))

(defmethod alloy:handle ((event alloy:pointer-up) (view om-item-view))
  (cond ((and (alld:clicked-element (alld:pointer-state)) (not (eql (alld:clicked-element (alld:pointer-state)) :abort)))
         (let* ((target (alld:clicked-element (alld:pointer-state)))
                (pos    (%alloy-location-to-om-point (alloy:location event) target)))
           (om-click-release-handler target pos)
           (setf (alld:clicked-element (alld:pointer-state)) nil)
           t))
        (t (alloy:decline))))

(defun %om-scroll-actions (event view)
  "om-view's vanilla wheel semantics: cmd+wheel zooms through the kernel's
om-zoom-touch-callback, otherwise om-view-scroll-handler."
  (let ((pos (%alloy-location-to-om-point (alloy:location event) view))
        (dx  (alloy:dx event))
        (dy  (alloy:dy event)))
    (if (and (om-command-key-p) (not (zerop dy)))
        ;; Ctrl+wheel IS the zoom channel -- the touchpad pinch CAPI carries on an
        ;; input-model line of its own (om-LW/graphic-object.lisp: the
        ;; :default-initargs of om-graphic-object) -- so it always consumes: the
        ;; callback's value is a result, not a flag, and a pane with no zoom is a no-op.
        (progn (om-zoom-touch-callback view (om-point-h pos) (om-point-v pos)
                                       (+ 1.0 (* dy 0.05)))
               t)
        (om-view-scroll-handler view dx dy
                                (remove nil (list (and (om-shift-key-p)   :shift)
                                                  (and (om-command-key-p) :control)
                                                  (and (om-option-key-p)  :alt)))))))

;;; The wheel over a scrolled pane is alloy-desktop's (view/scroll.lisp: alloy:handle for
;;; (scroll scroll-mixin)): children first, then the kernel's wheel semantics here, then the
;;; pane's own scroll -- the clip-view's offset when the pane sits in an alloy:scroll-view,
;;; the offset that goto-line (sbcl-lisp-tools/texteditor.lisp: %eval-string-on-process)
;;; and the selection drag (%text-drag-scroller) already write. An om-view with no scroll
;;; configured only dispatches to its children: it has no clip, no bars and no child
;;; translate, so moving its offset would only desync om-draw-contents (draw.lisp) and the
;;; hit-test (om-view-contains-point-p) from the children.
(defmethod alld:pane-scrolled ((view om-scroll-mixin) event modifiers)
  (declare (ignore modifiers))
  (%om-scroll-actions event view))

(defmethod alloy:handle ((event alloy:pointer-double-click) (view om-view))
  (restart-case (call-next-method)
    (decline ()
      (let* ((pos     (%alloy-location-to-om-point (alloy:location event) view))
             (handled (om-view-doubleclick-handler view pos)))
        (if handled t (alloy:decline))))))

(defmethod alloy:handle ((event alloy:pointer-up) (view om-view))
  (or (alld:dispatch-to-children event view)
      ;; pressed-element fallback for a release that landed outside the dragged view
      (when (and (alld:clicked-element (alld:pointer-state)) (not (eql (alld:clicked-element (alld:pointer-state)) :abort)))
        (let* ((target (alld:clicked-element (alld:pointer-state)))
               (pos    (%alloy-location-to-om-point (alloy:location event) target))
               (r      (om-click-release-handler target pos)))
          (setf (alld:clicked-element (alld:pointer-state)) nil)
          r))
      (alloy:decline)))

(defmethod alloy:handle ((event alloy:pointer-move) (view om-view))
  (if (and (alld:clicked-element (alld:pointer-state)) (not (eql (alld:clicked-element (alld:pointer-state)) :abort)))
      ;; A panel drag is in progress: keep routing motion to the dragged view.
      (let* ((pos     (%alloy-location-to-om-point
                       (alloy:location event) (alld:clicked-element (alld:pointer-state))))
             (handled (om-click-motion-handler (alld:clicked-element (alld:pointer-state)) pos)))
        (if handled t (alloy:decline)))
      ;; Otherwise child widgets get hover first; panel mouse-tracking on decline.
      (or (alld:dispatch-to-children event view)
          ;; Mirror om-LW internal-motion-callback: dispatch motion side-effects on the
          ;; DEEPEST frame under the cursor (apply-in-subview), so a kernel om-item-view box
          ;; that specializes om-view-mouse-enter/leave-handler/update-view-cursor fires.
          (let* ((window (om-view-window view))
                 (pos    (%alloy-location-to-om-point
                          (alloy:location event) view))
                 (target (or (om-find-view-containing-point view pos) view)))
            ;; om-LW/actions.lisp (internal-motion-callback for om-graphic-object)
            ;; updates the cursor on EVERY move, and its leave step hands it back
            ;; to the container when the pointer leaves an item-view
            (update-view-cursor target)
            (unless (eql *last-containing-view* target)
              (when *last-containing-view*
                (%leave-hover-view *last-containing-view*))
              (om-view-mouse-enter-handler target)
              (setf *last-containing-view* target))
            (when window
              (om-window-mouse-moved-handler window pos))
            (%update-hover-tooltip target)
            ;; om-LW internal-motion-callback reports the move HANDLED
            ;; (om-LW/actions.lisp: internal-motion-callback for om-graphic-object).
            ;; A declined move makes alloy reset the window cursor to NIL
            ;; (alloy layout.lisp: handle for (layout-event layout-tree)),
            ;; undoing the update-view-cursor above: the system arrow comes back.
            t))))

;;; om-char-spec-callback (om-LW/actions.lisp: om-char-spec-callback for
;;; om-abstract-window and om-view): keyboard entry the kernel may
;;; specialize. SPEC is the decoded OM key (Alloy has no LW gesture-spec).
;;; A both-case-p character carries Shift in its own case, never as a modifier: a
;;; gesture-spec of a letter may not hold the lone Shift bit (LW manual lw-sys-86.htm,
;;; make-gesture-spec). The kernel reads that -- OMGMoveObject on a boxEditorFrame
;;; makes a SLOTS box instead of moving while the flag is up (kernel boxframe.lisp: OMGMoveObject for boxEditorFrame),
;;; which is the align of #\A (kernel patchcontainer.lisp: the #\A branch of handle-key-event for patchPanel).
(defmethod om-char-spec-callback ((self om-look-window) x y spec)
  (declare (ignore x y))
  (let ((alld:*shift-key-p* (and (not (and (characterp spec) (both-case-p spec)))
                               alld:*shift-key-p*)))
    (om-view-key-handler self spec)))

(defmethod om-char-spec-callback ((self om-view) x y spec)
  (om-char-spec-callback (om-view-window self) x y spec))

;;; CAPI delivers ONE gesture per keystroke; Alloy delivers two streams (key-down +
;;; text-event). Special keys and modified characters ride the key-down, plain
;;; characters ride the text-event, and :NONE means this bridge stays silent.
(defun %om-key-for-kernel (alloy-key)
  "What a key-down hands the kernel: a character, an :om-key-*, NIL for an unmapped
gesture (vanilla beeps on those too), or :NONE when the bridge must stay silent."
  (cond ((member alloy-key *om-modifier-keys*)
         :none)
        ((%glfw-key-to-om-key alloy-key))
        ((%printable-key-char alloy-key)
         (if (or (om-command-key-p) (om-option-key-p))
             (%printable-key-char alloy-key)
             :none))
        (t nil)))

;;; CAPI parity: the focused native input consumes keys BEFORE any handler;
;;; otherwise vanilla routes to the WINDOW only (om-LW/actions.lisp:
;;; om-char-spec-callback for om-abstract-window and om-view).
(defmethod alloy:handle ((event alloy:key-down) (window om-look-window))
  (%om-repair-window-focus window)
  (let ((wkey (%om-key-for-kernel (alloy:key event))))
    (or (alloy:handle event (alloy:focus-tree window))
        (and (not (eq wkey :none))
             (om-char-spec-callback window 0 0 wkey))
        (alloy:decline))))

(alloy:define-action-event om-mnemonic-action
  :slots ((mn-char :initarg :mn-char :reader mn-char))
  (default "M-a" :mn-char #\a) (default "M-b" :mn-char #\b)
  (default "M-c" :mn-char #\c) (default "M-d" :mn-char #\d)
  (default "M-e" :mn-char #\e) (default "M-f" :mn-char #\f)
  (default "M-g" :mn-char #\g) (default "M-h" :mn-char #\h)
  (default "M-i" :mn-char #\i) (default "M-j" :mn-char #\j)
  (default "M-k" :mn-char #\k) (default "M-l" :mn-char #\l)
  (default "M-m" :mn-char #\m) (default "M-n" :mn-char #\n)
  (default "M-o" :mn-char #\o) (default "M-p" :mn-char #\p)
  (default "M-q" :mn-char #\q) (default "M-r" :mn-char #\r)
  (default "M-s" :mn-char #\s) (default "M-t" :mn-char #\t)
  (default "M-u" :mn-char #\u) (default "M-v" :mn-char #\v)
  (default "M-w" :mn-char #\w) (default "M-x" :mn-char #\x)
  (default "M-y" :mn-char #\y) (default "M-z" :mn-char #\z)
  ;; digits too: LispWorks gives "12 EDO_#" the mnemonic 1, so an underline the fork
  ;; draws on a digit has to be pressable
  (default "M-0" :mn-char #\0) (default "M-1" :mn-char #\1)
  (default "M-2" :mn-char #\2) (default "M-3" :mn-char #\3)
  (default "M-4" :mn-char #\4) (default "M-5" :mn-char #\5)
  (default "M-6" :mn-char #\6) (default "M-7" :mn-char #\7)
  (default "M-8" :mn-char #\8) (default "M-9" :mn-char #\9))

;;; A text-event is a direct-event: the tree hands it to (alloy:focused tree) and
;;; nowhere else (alloy focus-tree.lisp: handle for (event focus-tree)), and a
;;; focus-element that is not the focused one sends it UP to its parent (the
;;; handle :around for (event focus-element)). So a tree that drifted off its
;;; :strong pane turns every typed character into a kernel shortcut. Vanilla cannot
;;; drift: each char re-asserts the pane focus (om-LW/actions.lisp: om-char-spec-callback for om-view).
(defmethod alloy:handle ((event alloy:text-event) (window om-look-window))
  (%om-repair-window-focus window)
  (let* ((text (alloy:text event))
         (char (when (and text (plusp (length text))) (char text 0))))
    (or (alloy:handle event (alloy:focus-tree window))
        (and char (om-char-spec-callback window 0 0 char))
        (alloy:decline))))

(defun %mouse-position-primitive (view)
  "Cursor position in VIEW's coordinates; never calls OM-MOUSE-POSITION."
  (typecase view
    (null (let ((loc (alld:cursor-location (alld:pointer-state))))
            (om-make-point (round (alloy:pxx loc)) (round (alloy:pxy loc)))))
    ;; om-item-view is a subclass of om-graphic-object, so it must be tested first.
    (om-item-view
     (let ((container (item-container view)))
       (if container
           (om-convert-coordinates (%alloy-location-to-om-point (alld:cursor-location (alld:pointer-state)) container)
                                   container view)
           (om-make-point 0 0))))
    (om-graphic-object
     (if (typep view 'alloy:layout-element)
         (%alloy-location-to-om-point (alld:cursor-location (alld:pointer-state)) view)
         ;; top-level window: cursor is already window-local (Y-up); flip to OM Y-down.
         (let ((loc (alld:cursor-location (alld:pointer-state))))
           (om-make-point (round (alloy:pxx loc))
                          (round (- (alloy:pxh (alloy:bounds view)) (alloy:pxy loc)))))))
    (t (error 'type-error :datum view
                          :expected-type '(or null om-graphic-object)))))

(defmethod om-mouse-position ((view null))
  (%mouse-position-primitive view))

(defmethod om-mouse-position ((view om-graphic-object))
  (%mouse-position-primitive view))

(defmethod om-mouse-position ((view om-item-view))
  (%mouse-position-primitive view))

;;; macOS pinch: the magnification alloy-desktop native/gestures.lisp: install-magnify-handler
;;; delivers becomes the kernel zoom the cmd+wheel gesture feeds (om-zoom-touch-callback).

#+darwin
(progn

(defparameter *magnify-gain* 1.0d0
  "Multiplies the NSEvent magnification before it becomes an OM zoom scale.")

(defun %magnify-target-view ()
  "The deepest view under the cursor in the focused window, or that window."
  (let ((window (alld:focused-window)))
    (when (and window (om-window-open-p window))
      (let ((point (%mouse-position-primitive window)))
        (or (om-find-view-containing-point window point) window)))))

(defun %magnify-dispatch (scale)
  "Feed SCALE to the kernel through the same entry the cmd+wheel zoom uses."
  (let ((view (%magnify-target-view)))
    (when view
      (let ((point (%mouse-position-primitive view)))
        (om-zoom-touch-callback view (om-point-h point) (om-point-v point) scale)))))

(defun %magnify-from-event (magnification)
  "Turn one NSEvent MAGNIFICATION into the multiplicative scale the kernel's zoom entry takes."
  (%magnify-dispatch (+ 1.0d0 (* *magnify-gain* magnification))))

(defmethod %install-window-gestures ((window om-look-window))
  (alld:install-magnify-handler window #'%magnify-from-event)))
