(in-package :alloy-desktop)

;;; Menu facility: the neutral concrete model, in-window menu bars, popup chains
;;; (menu-bar dropdowns, context menus and widget dropdowns), and accelerator
;;; dispatch. Popup surfaces and focus policy remain protocol seams so the same
;;; model and lifecycle can be used by different window backends.

;;; ---- Model ------------------------------------------------------------------

(defclass menu-object () ())

(defgeneric menu-entry-title (entry)
  (:documentation "Title of the menu or leaf ENTRY."))

(defgeneric menu-entry-items (entry)
  (:documentation "Entries of the menu or component ENTRY."))

(defgeneric menu-entry-action (entry)
  (:documentation "Thunk the leaf ENTRY runs when chosen, or NIL."))

(defgeneric menu-entry-accelerator (entry)
  (:documentation "Accelerator of the leaf ENTRY: a character, a one-character string or (:shift x)."))

(defgeneric menu-entry-enabled (entry)
  (:documentation "Enabled state of ENTRY: a boolean, or a function or fbound symbol evaluated live."))

(defgeneric menu-entry-selected (entry)
  (:documentation "Selected state of the leaf ENTRY: a boolean, or a function or fbound symbol evaluated live."))

(defclass menu-item (menu-object)
  ((title
    :initarg :title
    :initform ""
    :accessor menu-entry-title)
   (action
    :initarg :action
    :initform nil
    :accessor menu-entry-action)
   (accelerator
    :initarg :accelerator
    :initform nil
    :accessor menu-entry-accelerator)
   (enabled
    :initarg :enabled
    :initform t
    :accessor menu-entry-enabled)
   (selected
    :initarg :selected
    :initform nil
    :accessor menu-entry-selected)))

(defclass menu (menu-object)
  ((title
    :initarg :title
    :initform ""
    :accessor menu-entry-title)
   (items
    :initarg :items
    :initform nil
    :accessor menu-entry-items)
   (enabled
    :initarg :enabled
    :initform t
    :accessor menu-entry-enabled)))

(defclass menu-component (menu-object)
  ((items
    :initarg :items
    :initform nil
    :accessor menu-entry-items)))

(defclass pop-up-menu (dialog-item menu) ())

(defun make-menu-item (title action &key accelerator (enabled t) (selected nil))
  "Create a menu leaf with TITLE, ACTION and optional accelerator and state."
  (make-instance 'menu-item
                 :title title
                 :action action
                 :accelerator accelerator
                 :enabled enabled
                 :selected selected))

(defun make-menu (title items &key (enabled t))
  "Create a menu with TITLE and ITEMS."
  (make-instance 'menu :title title :items items :enabled enabled))

(defun make-menu-component (items)
  "Create a grouped menu component containing ITEMS."
  (make-instance 'menu-component :items items))

(defun add-menu-items (menu end &rest menu-items)
  "Add MENU-ITEMS to MENU. When END is true append, otherwise prepend."
  (setf (menu-entry-items menu)
        (if end
            (append (menu-entry-items menu) menu-items)
            (append menu-items (menu-entry-items menu)))))

(defun remove-menu-items (menu &rest menu-items)
  "Remove each object in MENU-ITEMS from MENU using EQUAL comparison."
  (loop for item in menu-items do
    (setf (menu-entry-items menu)
          (remove item (menu-entry-items menu) :test #'equal))))

(defun find-menu-item (menu title)
  "Find the first direct menu or menu-item in MENU whose title equals TITLE."
  (find title (menu-entry-items menu)
        :key (lambda (item)
               (when (or (typep item 'menu) (typep item 'menu-item))
                 (menu-entry-title item)))
        :test #'string-equal))

(defun menu-item-at (menu index)
  "Return the direct menu entry at INDEX in MENU."
  (nth index (menu-entry-items menu)))

;;; The menu bar items assigned to a window. Weak on the window, so a window that
;;; is no longer referenced loses its entry with it. A NIL assignment is bound: it
;;; represents an explicitly empty menu bar.

(defvar *window-menu-items* (make-hash-table :test 'eq :weakness :key))
(defvar *window-menu-providers* (make-hash-table :test 'eq :weakness :key))

(defun window-menu-items (window)
  "The menu bar items assigned to WINDOW, or NIL."
  (values (gethash window *window-menu-items*)))

(defun (setf window-menu-items) (items window)
  "Assign ITEMS as the menu bar items of WINDOW without changing realization."
  (setf (gethash window *window-menu-items*) items))

(defun window-menu-items-bound-p (window)
  "True when WINDOW has been assigned menu bar items, NIL included."
  (nth-value 1 (gethash window *window-menu-items*)))

(defmethod window-menu-model ((window window))
  (let ((provider (gethash window *window-menu-providers*)))
    (when provider
      (setf (window-menu-items window) (funcall provider window))))
  (when (window-menu-items-bound-p window)
    (window-menu-items window)))

(defun menu-entry-enabled-now (item)
  "Evaluate ITEM's enabled state now; a function or fbound symbol is a live
predicate (the toolkit's :enabled-function)."
  (let ((e (menu-entry-enabled item)))
    (cond ((functionp e) (and (funcall e) t))
          ((and e (symbolp e) (not (keywordp e)) (fboundp e)) (and (funcall e) t))
          (t (and e t)))))

(defun menu-entry-selected-now (item)
  "Evaluate ITEM's selected state now (the toolkit's :selected-function live predicate)."
  (and (typep item 'menu-item)
       (let ((s (menu-entry-selected item)))
         (cond ((functionp s) (and (funcall s) t))
               ((and s (symbolp s) (not (keywordp s)) (fboundp s)) (and (funcall s) t))
               (t (and s t))))))

(defun format-accelerator-label (title accelerator)
  "Accelerator case encodes Shift: an uppercase letter requires the Shift modifier."
  (cond ((null accelerator) title)
        ((and (consp accelerator) (eq (first accelerator) :shift))
         (format nil "~A    Ctrl+Shift+~A" title (string-upcase (second accelerator))))
        (t (let* ((s (string accelerator))
                  (c (and (= 1 (length s)) (char s 0))))
             (if (and c (alpha-char-p c) (upper-case-p c))
                 (format nil "~A    Ctrl+Shift+~A" title s)
                 (format nil "~A    Ctrl+~A" title (string-upcase s)))))))

(defun menu-entry-label (item)
  "Display label for a menu entry: title plus accelerator suffix. The selection
marker is a drawn tick in the row's left gutter, not text — the Verdana MSDF
atlas has no U+2713, and a text prefix would also shift the label."
  (if (typep item 'menu-item)
      (format-accelerator-label (menu-entry-title item)
                                (menu-entry-accelerator item))
      (menu-entry-title item)))

(defparameter *accelerator-key-aliases*
  '((#\. :period)
    (#\, :comma)
    (#\; :semicolon)
    (#\/ :slash)
    (#\\ :backslash)
    (#\= :equal)
    (#\- :minus)
    (#\[ :left-bracket)
    (#\] :right-bracket)
    (#\space :space))
  "Map of non-alphabetic accelerator characters to alloy:key keywords.")

(defun accelerator-modifier-p (mods &optional (command (command-modifier)))
  "True iff MODS carry the accelerator prefix: the command modifier for the
platform (Command on macOS, Control elsewhere -- input/modifiers.lisp: command-modifier)."
  (and (find command mods) t))

(defun accelerator-matches-key-p (rac alloy-key shift-p)
  "True iff RAC (a char or 1-char string, or (:shift x)) matches ALLOY-KEY with
SHIFT-P state. The accelerator letter's CASE encodes the Shift requirement:
an UPPERCASE letter needs Shift (Ctrl+Shift+L for \"L\"),
lowercase/digit/punctuation does not (Ctrl+n for \"n\"). This disambiguates
colliding pairs like Ctrl+i vs Ctrl+Shift+I."
  ;; a key alloy could not map to a keyword arrives as its raw code (Windows sends
  ;; 162 for Control itself); no character accelerator can match one
  (unless (symbolp alloy-key)
    (return-from accelerator-matches-key-p nil))
  (labels ((base-char (x)
             (cond ((characterp x) x)
                   ((and (stringp x) (= 1 (length x))) (char x 0))))
           (letter-matches (char)
             (cond ((null char) nil)
                   ((alpha-char-p char)
                    (string-equal (symbol-name alloy-key) (string (char-upcase char))))
                   ((digit-char-p char)
                    (string-equal (symbol-name alloy-key) (string char)))
                   (t (let ((alias (cadr (assoc char *accelerator-key-aliases* :test #'char=))))
                        (and alias (eq alloy-key alias)))))))
    (if (and (consp rac) (eq (first rac) :shift))
        (and shift-p (letter-matches (base-char (second rac))))
        (let ((char (base-char rac)))
          (and (letter-matches char)
               (if (and char (alpha-char-p char) (upper-case-p char))
                   shift-p
                   (not shift-p)))))))

(defun find-accelerator-in-items (items key shift-p)
  (dolist (item items)
    (cond ((typep item 'menu-component)
           (let ((sub (find-accelerator-in-items (menu-entry-items item) key shift-p)))
             (when sub (return-from find-accelerator-in-items sub))))
          ((typep item 'menu-item)
           (when (and (menu-entry-enabled-now item)
                      (accelerator-matches-key-p
                       (menu-entry-accelerator item) key shift-p))
             (return-from find-accelerator-in-items
               (menu-entry-action item)))))))

(defun find-accelerator-action (menubar key shift-p)
  "Walk MENUBAR (list of menus) and return matching item action for KEY, or NIL."
  (dolist (menu menubar)
    (when (and (typep menu 'menu) (menu-entry-enabled-now menu))
      (let ((found (find-accelerator-in-items (menu-entry-items menu) key shift-p)))
        (when found (return-from find-accelerator-action found))))))

;;; ---- Policy seams -------------------------------------------------------------

(defgeneric popup-make-surface (owner left top width height)
  (:documentation
   "Native surface for one popup level of OWNER, at the virtual-screen rectangle
LEFT TOP WIDTH HEIGHT (y-down), not yet shown. The default method on WINDOW makes
a hidden undecorated MAKE-WINDOW there, sharing OWNER's GL context so it draws with
OWNER's font atlases, at the surface's own content scale."))

(defgeneric popup-hide-surface (surface)
  (:documentation "Hide the popup SURFACE being retired.")
  (:method ((surface t)) (hide-window surface)))

(defgeneric popup-repair-focus (owner)
  (:documentation "Re-point OWNER's focus at its surviving strong pane; that pane, or NIL.")
  (:method ((owner t)) (repair-window-focus owner)))

(defgeneric popup-focus-pane (owner pane)
  (:documentation "Give PANE, which a popup of OWNER returns the focus to, the strong focus.")
  (:method ((owner t) pane) (focus-pane nil pane)))

(defgeneric menubar-return-focus (window)
  (:documentation "The element a menu-bar dropdown of WINDOW returns the focus to.")
  (:method ((window t)) (window-focus-root window)))

;;; ---- Menu bar -----------------------------------------------------------------

(defparameter *disabled-menu-items* (make-hash-table :test 'eq)
  "Set (instance -> T) marking alloy:menu-items rendered as disabled (gray).")

(defparameter *menu-item-padding-v* 6
  "Vertical padding (px) above/below menu-item label text.")

(defparameter *menu-line-height-ratio* 1.25f0
  "Heuristic ratio of line-height to font size.")

(defun menubar-height-un ()
  (let* ((size-px (role-points-px :menu))
         (text-h (ceiling (* size-px *menu-line-height-ratio*))))
    (+ text-h (* 2 *menu-item-padding-v*))))

(defun menu-item-disabled-p (item)
  (gethash item *disabled-menu-items*))

(defclass menubar-title (alloy:menu-item)
  ((menu         :initarg :menu         :initform nil :accessor menubar-title-menu)
   (owner-window :initarg :owner-window :initform nil :accessor menubar-title-window))
  (:documentation
   "Top-level menubar title. It carries no in-window alloy:submenu: its dropdown
is opened on a popup surface of its own, the way the toolkit delegates the menu
bar to a native menu that may leave the window."))

(defvar *menubar-open-title* nil
  "The menubar-title whose dropdown chain currently occupies the satellite.")

(defvar *menubar-suppress-title* nil
  "Title whose chain was torn down by the pointer-down half of a click on itself;
the matching pointer-up must leave the menu closed instead of re-opening it.")

(defun menubar-dropdown-open-p ()
  "True while a menu-bar dropdown chain is open."
  (and *menubar-open-title* t))

(defun build-menubar-tree (menus window)
  (loop for m in menus
        when (typep m 'menu)
          collect (make-instance 'menubar-title
                                 :value        (menu-entry-title m)
                                 :menu         m
                                 :owner-window window)))

(defvar *window-menubars* (make-hash-table :test 'eq))

(defun window-menubar (window)
  "The alloy:menu realized as WINDOW's menu bar, or NIL."
  (gethash window *window-menubars*))

(defun collect-disabled-titles (menus)
  (let ((acc nil))
    (labels ((walk (items)
               (dolist (item items)
                 (cond ((typep item 'menu)
                        ;; a disabled top-level/sub menu grays its title node (the toolkit's enabled-function)
                        (unless (menu-entry-enabled-now item)
                          (push (menu-entry-title item) acc))
                        (walk (menu-entry-items item)))
                       ((typep item 'menu-component)
                        (walk (menu-entry-items item)))
                       ((typep item 'menu-item)
                        (unless (menu-entry-enabled-now item)
                          (push (menu-entry-label item) acc)))))))
      (walk menus))
    acc))

(defun walk-and-disable (alloy-element disabled-titles)
  (cond ((typep alloy-element 'alloy:menu-item)
         (let ((title (alloy:value alloy-element)))
           (when (member title disabled-titles :test #'string-equal)
             (setf (gethash alloy-element *disabled-menu-items*) t)
             (alloy:mark-for-render alloy-element))))
        ((or (typep alloy-element 'alloy:layout)
             (typep alloy-element 'alloy:focus-list))
         (alloy:do-elements (child alloy-element)
           (walk-and-disable child disabled-titles)))))

(defun update-realized-item-enabled (item enabled-p)
  "Reflect ITEM's ENABLED-P on every realized menubar item whose label matches, across
every window's menu bar (the host toggles shared menu objects after realization)."
  (let ((label (menu-entry-label item)))
    (labels ((walk (el)
               (cond ((typep el 'alloy:menu-item)
                      (when (string-equal (alloy:value el) label)
                        (if enabled-p
                            (remhash el *disabled-menu-items*)
                            (setf (gethash el *disabled-menu-items*) t))
                        (alloy:mark-for-render el)))
                     ((or (typep el 'alloy:layout)
                          (typep el 'alloy:focus-list))
                      (alloy:do-elements (child el) (walk child))))))
      (maphash (lambda (window menu)
                 (declare (ignore window))
                 (walk (alloy:layout-element menu)))
               *window-menubars*))))

(defun set-menu-entry-enabled (entry enabled)
  "Set ENTRY's enabled state and synchronize already realized menu surfaces."
  (setf (menu-entry-enabled entry) enabled)
  (let ((enabled-now (menu-entry-enabled-now entry)))
    #-darwin (update-realized-item-enabled entry enabled-now)
    #+darwin (macos-update-item-enabled (menu-entry-title entry) enabled-now))
  entry)

(defun attach-window-menubar (window chrome menus)
  "Realize MENUS as WINDOW's menu bar on top of CHROME; the alloy:menu, or NIL when
MENUS holds no menu."
  (let* ((tree (build-menubar-tree menus window)))
    (when tree
      (let ((menu (make-instance 'alloy:menu
                                 :tree          tree
                                 :focus-parent  (chrome-focus-parent window))))
        (alloy:enter (alloy:layout-element menu) chrome
                     :place :north :size (alloy:un (menubar-height-un)))
        (alloy:refit chrome)
        (setf (gethash window *window-menubars*) menu)
        (let ((disabled (collect-disabled-titles menus)))
          (when disabled
            (walk-and-disable (alloy:layout-element menu) disabled)))
        menu))))

(defun detach-window-menubar (window menu)
  "Drop MENU, WINDOW's menu bar, from the layout and the focus tree."
  ;; (leave structure T) drops BOTH the layout and the focus registration
  ;; (alloy structure.lisp: the leave for (structure (eql T))); leaving only the layout leaks a ghost
  ;; menubar into the window focus list on every rebuild.
  (alloy:leave menu t)
  (remhash window *window-menubars*)
  (clrhash *disabled-menu-items*))

(defun %menu-client-size (window)
  (let ((bounds (alloy:bounds (window-root-layout window))))
    (values (round (alloy:pxw bounds)) (round (alloy:pxh bounds)))))

(defun update-window-menubar (window &key (menus (window-menu-model window))
                                         app-menu
                                         (resize-function #'set-window-size))
  "Realize WINDOW's menu model, preserving its client size.
RESIZE-FUNCTION retains a host's window-size constraints and resize callbacks.
On macOS WINDOW-MENU-MODEL remains the live menu provider; APP-MENU supplies
application-specific commands for the native application menu."
  (declare (ignorable menus app-menu resize-function))
  #+darwin
  (progn
    (macos-install-window-menu window
                               (lambda () (remove nil (window-menu-model window)))
                               app-menu))
  #-darwin
    (multiple-value-bind (width height) (%menu-client-size window)
      (let* ((chrome (window-chrome window))
             (existing (window-menubar window)))
        (when existing
          (when (menubar-dropdown-open-p) (popup-close-stack window))
          (detach-window-menubar window existing)
          (alloy:refit chrome))
        (prog1 (attach-window-menubar window chrome (remove nil menus))
          ;; A realized text bar can settle after the first resize/refit.
          (loop repeat 3
                do (multiple-value-bind (w h) (%menu-client-size window)
                     (let ((dw (- width w)) (dh (- height h)))
                       (when (and (zerop dw) (zerop dh)) (return))
                       (let ((outer (alloy:bounds window)))
                         (funcall resize-function window
                                  (+ (round (alloy:pxw outer)) dw)
                                  (+ (round (alloy:pxh outer)) dh))))))))))

(defun set-window-menu-items (window menus &key app-menu provider
                                               (resize-function #'set-window-size))
  "Assign MENUS and realize the bar immediately without shrinking its client.
PROVIDER, when supplied, refreshes the effective model on later reads."
  (setf (window-menu-items window) menus)
  (if provider
      (setf (gethash window *window-menu-providers*) provider)
      (remhash window *window-menu-providers*))
  (update-window-menubar window :menus menus :app-menu app-menu :resize-function resize-function)
  menus)

(defun retire-window-menubar (window)
  "Drop the strong WINDOW -> MENU entry during WINDOW's funeral.
Deliberately unlike a host's menubar removal: no refit and no interior-size
compensation, because the native window is already on its way out."
  (let ((menu (gethash window *window-menubars*)))
    (when menu
      (when *menubar-open-title* (popup-close-stack window))
      (alloy:leave menu t)
      (remhash window *window-menubars*)
       (clrhash *disabled-menu-items*)
       menu)))

(register-window-retirement :menubar
                            #'retire-window-menubar
                            :order 20)

;;; Wrapping menu bar. The native Win32 bar the toolkit delegates to folds its
;;; titles onto extra rows when the window is too narrow and grows downward,
;;; pushing the client area down; alloy's horizontal-linear-layout only advances
;;; x (alloy layouts/linear.lisp: the refit of horizontal-linear-layout), so the
;;; titles would run off the window edge.
;;; The two methods below reproduce the fold: SUGGEST-SIZE answers rows x row
;;; height for the width the border layout offers (alloy layouts/border.lisp: the
;;; refit of border-layout passes the real available width in the hint and then
;;; MAXes the height in),
;;; and REFIT places the titles row by row.

(defun menubar-wrap (bar avail mw mh l u r b)
  "Fold BAR's titles into rows AVAIL px wide, with the linear layout's own cell
advance. Returns (values rows placements row-height), a placement being
(element x row w); w = 0 marks the zero-width element the linear refit collapses
to nothing, and row-height is the tallest cell, as the linear suggest-size grows
its own height to (alloy layouts/linear.lisp: the suggest-size of
horizontal-linear-layout)."
  (let ((x l) (row 0) (tall 0) (places '()))
    (alloy:do-elements (element bar)
      (let* ((size (alloy:suggest-size (alloy:px-size mw mh) element))
             (raw  (alloy:pxw size)))
        (setf tall (max tall (+ (alloy:pxh size) u b)))
        (cond ((<= raw 0)
               (push (list element 0 0 0) places))
              (t
               (let ((ew (max mw raw)))
                 (when (and (> x l) (> (+ x ew) avail))
                   (incf row)
                   (setf x l))
                 (push (list element x row ew) places)
                 (incf x (+ l r ew)))))))
    (values (1+ row) (nreverse places) tall)))

(defmethod alloy:suggest-size (size (bar alloy:menubar))
  (alloy:with-unit-parent bar
    (let* ((cm (alloy:cell-margins bar))
           (l  (alloy:pxl cm)) (u (alloy:pxu cm))
           (r  (alloy:pxr cm)) (b (alloy:pxb cm))
           (w  (alloy:pxw size))
           (mw (alloy:pxw (alloy:min-size bar)))
           (mh (if (alloy:stretch bar)
                   (- (alloy:pxh size) u b)
                   (alloy:pxh (alloy:min-size bar)))))
      (multiple-value-bind (rows places tall) (menubar-wrap bar w mw mh l u r b)
        (declare (ignore places))
        (alloy:px-size w (* rows (max (alloy:pxh size) tall)))))))

(defmethod alloy:refit ((bar alloy:menubar))
  (alloy:with-unit-parent bar
    (let* ((cm (alloy:cell-margins bar))
           (l  (alloy:pxl cm)) (u (alloy:pxu cm))
           (r  (alloy:pxr cm)) (b (alloy:pxb cm))
           (bounds (alloy:bounds bar))
           (w  (alloy:pxw bounds))
           (h  (alloy:pxh bounds))
           (mw (alloy:pxw (alloy:min-size bar))))
      ;; the border layout already sized BAR to the height suggest-size asked
      ;; for, so dividing it by the row count gives that row height back
      (multiple-value-bind (rows places) (menubar-wrap bar w mw (- h u b) l u r b)
        (let* ((row-h (/ h rows))
               (mh    (if (alloy:stretch bar)
                          (- row-h u b)
                          (alloy:pxh (alloy:min-size bar)))))
          (dolist (place places)
            (destructuring-bind (element x row ew) place
              (setf (alloy:bounds element)
                    (if (<= ew 0)
                        (alloy:px-extent 0 0 0 0)
                        ;; alloy y grows UP: row 0 is the TOP row of the bar
                        (alloy:px-extent
                         x (+ (- h (* (1+ row) row-h)) b) ew
                         (min mh (alloy:pxh
                                  (alloy:suggest-size
                                   (alloy:px-size mw mh) element))))))))))
      bar)))

;;; ---- Popups -------------------------------------------------------------------

(defparameter *popup-menu-corner-radius*    8)
(defparameter *popup-menu-border-width*     1)
(defparameter *popup-menu-item-height*      26)
(defparameter *popup-menu-separator-height* 9)
(defparameter *popup-menu-min-width*        220)

(defparameter *popup-surface-samples*
  #+darwin 0
  #-darwin 4
  "Default-framebuffer sample count of a popup surface.")

(defclass popup-window (window) ()
  (:documentation "A transient native surface owned by a document window."))

(defmethod window-activated ((surface popup-window) active-p)
  (declare (ignore surface active-p))
  t)

(defmethod window-close-allowed-p ((surface popup-window))
  (declare (ignore surface))
  nil)

(defmethod window-scale-changed ((window popup-window) scale)
  ;; a popup's scale change does not replace the recorded display scale
  (declare (ignore window scale))
  nil)

(defun make-popup-window (owner left top width height
                          &key (class 'popup-window) (resizable-p nil))
  "Create an owned, hidden popup. Resizable surfaces also serve completion lists."
  (let ((surface (make-window (desktop-screen)
                              :class class
                              :title ""
                              :x left :y top :width width :height height
                              :decorated-p nil
                              :resizable-p resizable-p
                              :always-on-top-p (eq :wayland (%glfw:platform))
                              :background-color (chroma:rgb 0 0 0 0)
                              :share owner
                              :native-hints (list :samples *popup-surface-samples*
                                                  :transparent-framebuffer t
                                                  :auto-iconify nil)
                              ;; scaled below, once the owner is set
                              :initialize-content-scale-p nil)))
    (setf (window-owner surface) owner)
    (apply-content-scale surface (or (window-content-scale surface) 1.0))
    surface))

(defmethod popup-make-surface ((owner window) left top width height)
  (make-popup-window owner left top width height))

(defun close-window-popup-stack (owner)
  "Retire popups before destroying OWNER, never when reaping a popup surface."
  (unless (typep owner 'popup-window)
    (popup-close-stack owner)))

(defmethod %close-owned-windows :before ((owner window))
  ;; Runs before owned native windows die, also on forced close and deallocation.
  (close-window-popup-stack owner))

(defmethod alloy:deallocate :after ((surface popup-window))
  (forget-popup-surface surface))

(defmethod %glfw:window-focused :after ((surface popup-window) focused)
  (let ((owner (window-owner surface)))
    (when (and focused owner)
      (adopt-focused-window owner))))

(defmethod hide-window :after ((surface popup-window))
  ;; Hiding during a press transfers native capture; consume its orphan release.
  (when (eq surface (pointer-press-window (pointer-state)))
    (arm-orphan-release)))

(defmethod alloy:handle :around ((event alloy:pointer-event) (window window))
  (let ((target (popup-pointer-target window event)))
    (cond (target (alloy:handle event target) t)
          (t
           (when (typep event 'alloy:pointer-down)
             (popup-note-pointer-down window (alloy:location event)))
           (call-next-method)))))

(defparameter *popup-menu-label-left*  24
  "Left gutter of a row label: the selection tick's lane.")

(defparameter *popup-menu-label-right* 28
  "Right gutter of a row label: the submenu chevron's lane.")

(defparameter *popup-menu-label-slack* 8
  "Px added to a measured label: the MSDF glyph quad overhangs the advance sum.")

(defparameter *popup-menu-border-color*    (chroma:rgb 0.5 0.5 0.5)
  "Ring of a popup frame.")
(defparameter *popup-menu-background*      (chroma:rgb 0.9 0.9 0.9)
  "Interior of a popup frame.")
(defparameter *menu-highlight*             (chroma:rgb 0.85 0.85 0.85)
  "Background of the focused row of a popup or of the menu bar.")
(defparameter *menu-text*                  (chroma:rgb 0.0 0.0 0.0)
  "Label, tick and chevron of an enabled row.")
(defparameter *menu-disabled-text*         (chroma:rgb 0.5 0.5 0.5)
  "Label, tick and chevron of a disabled row.")
(defparameter *popup-menu-separator-color* (chroma:rgb 0.5 0.5 0.5)
  "Rule of a popup separator.")
(defparameter *menubar-background*
  (chroma:rgb (/ 255 255.0) (/ 255 255.0) (/ 255 255.0))
  "Background of the menu bar.")
(defparameter *menubar-rule*
  (chroma:rgb (/ 241 255.0) (/ 241 255.0) (/ 241 255.0))
  "Rule drawn under the menu bar.")

(defvar *popup-menu-stack* nil
  "Stack of currently open popup-frame instances; deepest first.")

(defvar *popup-menu-window* nil
  "Native surface hosting the chain's ROOT frame, or NIL when no chain is open.")

(defvar *popup-menu-closing-p* nil
  "Reentrancy guard for focus-loss/native-close driven popup teardown.")

(defvar *popup-suppress-opener* nil
  "Widget whose chain was torn down by the pointer-down half of a click on itself;
the matching pointer-up must leave the menu closed instead of re-opening it.")

(defun popup-consume-opener-suppression-p (opener)
  "True when OPENER armed the one-shot guard, clearing it: a second click on the
widget that opened a chain closes it instead of re-opening."
  (when (and opener (eq opener *popup-suppress-opener*))
    (setf *popup-suppress-opener* nil)
    t))

(defclass popup-frame (alloy:popup
                       alloy:fixed-layout
                       alloy:focus-list
                       alloy:renderable)
  ((depth        :initarg :depth        :initform 0   :accessor popup-depth)
   (owner-window :initarg :owner-window :initform nil :accessor popup-owner-window)
   (parent-item  :initarg :parent-item  :initform nil :accessor popup-parent-item)
   (return-focus :initarg :return-focus :initform nil :accessor popup-return-focus)
   ;; Native surface this level owns; it fills the surface exactly and neither
   ;; ever moves nor resizes while visible.
   (surface      :initarg :surface      :initform nil :accessor popup-frame-surface)
   ;; Widget that opened this chain, when the chain hangs off one (menu button).
   (opener       :initarg :opener       :initform nil :accessor popup-opener)
   ;; Virtual-screen rectangle, y-down; identical to the surface geometry.
   (screen-left   :initarg :screen-left :initform 0 :accessor popup-screen-left)
   (screen-top    :initarg :screen-top  :initform 0 :accessor popup-screen-top)
   (screen-width  :initarg :screen-width :initform 0 :accessor popup-screen-width)
   (screen-height :initarg :screen-height :initform 0 :accessor popup-screen-height)))

(defclass popup-menu-item (alloy:menu-item)
  ((submenu-tree :initarg :submenu-tree :initform nil :accessor popup-item-submenu-tree)
   (enabled-p    :initarg :enabled-p    :initform t   :accessor popup-item-enabled-p)
   (selected-p   :initarg :selected-p   :initform nil :accessor popup-item-selected-p)))

(defclass popup-menu-separator (alloy:layout-element alloy:renderable)
  ())

(defmethod alloy:suggest-size (size (sep popup-menu-separator))
  (alloy:px-size (alloy:pxw size) *popup-menu-separator-height*))

(defmethod alloy:activate ((sep popup-menu-separator)))
(defmethod alloy:focus ((sep popup-menu-separator)) nil)
(defmethod (setf alloy:focus) (val (sep popup-menu-separator)) val)
(defmethod alloy:set-focus-tree (tree (sep popup-menu-separator)))
(defmethod alloy:handle ((event alloy:pointer-move) (sep popup-menu-separator)))

;;; :line-width would ignore the corner radii (alloy renderers/opengl/renderer.lisp:
;;; the shared-initialize :around of outlined-rectangle);
;;; the inset must be px, since a bare number is an UN (alloy units.lisp: the define-unit un).
(presentations:define-realization (window popup-frame)
  ((:border simple:rectangle)
   (alloy:margins)
   :z-index 999
   :corner-radii (vector (alloy:px *popup-menu-corner-radius*)
                         (alloy:px *popup-menu-corner-radius*)
                         (alloy:px *popup-menu-corner-radius*)
                         (alloy:px *popup-menu-corner-radius*)))
  ((:background simple:rectangle)
   (alloy:margins (alloy:px *popup-menu-border-width*))
   :z-index 1000
   :corner-radii (let ((r (max 0 (- *popup-menu-corner-radius*
                                    *popup-menu-border-width*))))
                   (vector (alloy:px r) (alloy:px r) (alloy:px r) (alloy:px r)))))

(presentations:define-update (window popup-frame)
  (:border     :hidden-p nil :pattern *popup-menu-border-color*)
  (:background :hidden-p nil :pattern *popup-menu-background*))

(defun popup-check-stroke (item which)
  "One 45° stroke of the tick marking a selected row, in ITEM's left gutter
\(item-local px, alloy y-up). Same thick-diagonal construction as the table
filter's clear glyph: the two ends stay vertical, the width comes from D."
  (let* ((cy (/ (alloy:pxh (alloy:bounds item)) 2.0))
         (x0 7.0)
         (d  1.5))
    (flet ((quad (ax ay bx by)
             (list (alloy:px-point (- ax d) ay) (alloy:px-point (+ ax d) ay)
                   (alloy:px-point (+ bx d) by) (alloy:px-point (- bx d) by))))
      (ecase which
        (:down (quad x0 (+ cy 1.5) (+ x0 3.5) (- cy 2.5)))
        (:up   (quad (+ x0 3.5) (- cy 2.5) (+ x0 10.0) (+ cy 5.0)))))))

(presentations:define-realization (window popup-menu-item)
  ((:background simple:rectangle)
   (alloy:margins 4 1 4 1)
   :z-index 1000
   :corner-radii (vector (alloy:px 4) (alloy:px 4) (alloy:px 4) (alloy:px 4)))
  ((:check-down simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 1020)
  ((:check-up simple:polygon)
   (list (alloy:px-point 0 0) (alloy:px-point 1 0) (alloy:px-point 0 1))
   :z-index 1020)
  ((:label simple:text)
   (alloy:margins *popup-menu-label-left* 0 *popup-menu-label-right* 0) alloy:text
   :font (font-entry-font (role-font-entry alloy:renderer :menu))
   :size (font-entry-size-unit (role-font-entry alloy:renderer :menu))
   :halign :start :valign :middle
   :z-index 1020)
  ((:chevron simple:text)
   (alloy:margins 0 0 10 0) ""
   :font (font-entry-font (role-font-entry alloy:renderer :menu))
   :size (font-entry-size-unit (role-font-entry alloy:renderer :menu))
   :halign :end :valign :middle
   :z-index 1020))

(presentations:define-update (window popup-menu-item)
  (:background
   :hidden-p (not (and (popup-item-enabled-p alloy:renderable)
                       (alloy:focus alloy:renderable)))
   :pattern *menu-highlight*)
  (:check-down
   :points (popup-check-stroke alloy:renderable :down)
   :hidden-p (not (popup-item-selected-p alloy:renderable))
   :pattern (if (popup-item-enabled-p alloy:renderable)
                *menu-text*
                *menu-disabled-text*))
  (:check-up
   :points (popup-check-stroke alloy:renderable :up)
   :hidden-p (not (popup-item-selected-p alloy:renderable))
   :pattern (if (popup-item-enabled-p alloy:renderable)
                *menu-text*
                *menu-disabled-text*))
  (:label
   :text alloy:text
   :pattern (if (popup-item-enabled-p alloy:renderable)
                *menu-text*
                *menu-disabled-text*))
  (:chevron
   :text (if (popup-item-submenu-tree alloy:renderable)
             (string (code-char #x25B6))
             "")
   :pattern (if (popup-item-enabled-p alloy:renderable)
                *menu-text*
                *menu-disabled-text*)))

(presentations:define-realization (window popup-menu-separator)
  ((:line simple:rectangle)
   (alloy:extent (alloy:px 12) (alloy:ph 0.5) (alloy:pw) 1)
   :pattern *popup-menu-separator-color*
   :z-index 1000))

(presentations:define-update (window popup-menu-separator)
  (:line :pattern *popup-menu-separator-color*))

(defmethod alloy:activate ((item popup-menu-item))
  (let ((handler (alloy:on-activate item)))
    (when handler (funcall handler))))

(defmethod alloy:activate :around ((item popup-menu-item))
  (when (popup-item-enabled-p item)
    (call-next-method)))

(presentations:define-realization (window alloy:menubar)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:separator simple:rectangle)
   (alloy:extent 0 0 (alloy:pw) 1)
   :pattern *menubar-rule*))

(presentations:define-update (window alloy:menubar)
  (:background :pattern *menubar-background*)
  (:separator  :pattern *menubar-rule* :hidden-p nil))

(presentations:define-realization (window alloy:menu-item)
  ((:background simple:rectangle)
   (alloy:margins 4 1 4 1)
   :z-index 1000
   :corner-radii (vector (alloy:px 4) (alloy:px 4) (alloy:px 4) (alloy:px 4)))
  ((:label simple:text)
   (alloy:margins 10 *menu-item-padding-v*) alloy:text
   :font (font-entry-font (role-font-entry alloy:renderer :menu))
   :size (font-entry-size-unit (role-font-entry alloy:renderer :menu))
   :halign :start :valign :middle
   :z-index 1020))

(presentations:define-update (window alloy:menu-item)
  (:background
   :hidden-p (not (and (not (menu-item-disabled-p alloy:renderable))
                       (alloy:focus alloy:renderable)))
   :pattern *menu-highlight*)
  (:label
   :text alloy:text
   :pattern (if (menu-item-disabled-p alloy:renderable)
                *menu-disabled-text*
                *menu-text*)))

(defun menubar-open-dropdown (bar)
  "The title of BAR whose dropdown currently occupies the satellite, or NIL."
  (let ((open *menubar-open-title*))
    (when (and open
               (slot-boundp open 'alloy:layout-parent)
               (eq bar (alloy:layout-parent open)))
      open)))

(defun menubar-switch-dropdown (item)
  "Hover over a menubar title swaps the open dropdown for ITEM's, and does nothing
while no dropdown is open, matching conventional native menu-bar behaviour."
  (let* ((bar  (alloy:layout-parent item))
         (open (menubar-open-dropdown bar)))
    (when (and open (not (eq open item)))
      (menubar-open-title-popup item))))

(defmethod alloy:handle :before ((event alloy:pointer-move) (item alloy:menu-item))
  (cond ((not (alloy:contained-p (alloy:location event) item))
         ;; the title holding the open dropdown stays lit while the pointer
         ;; travels to the satellite, as a native menu bar does
         (unless (or (eq item *menubar-open-title*)
                     (member item (focus-path (alloy:focused (alloy:focus-tree item)))))
           (when (eql :weak (alloy:focus item))
             (setf (alloy:focus item) nil))))
        ((and (typep item 'menubar-title)
              (not (menu-item-disabled-p item)))
         (menubar-switch-dropdown item))))

;;; The dropdown ITEMS keep alloy's promotion: inside an open chain the focus IS
;;; the keyboard position. A bar title must not, or hovering the bar would arm
;;; the menubar chain and the next RET would open it.
(defmethod alloy:handle ((event alloy:pointer-move) (item menubar-title))
  (unless (chrome-hover-light item event)
    (call-next-method)))

(defun broadcast-pointer-move (event container)
  "Deliver EVENT to the child being ENTERED and to the children being LEFT,
resolved in ONE pass before any handler runs. Alloy's layout dispatch stops at the
first child that merely holds the OLD location (alloy layout.lisp: the handle for
(pointer-move layout)), so the
child being entered — the one that must open or switch the menu — would never see
the move. The pass is a snapshot because the handlers mutate the open chain:
re-testing membership while iterating would hit-test one pointer position against
several successive states of the menu stack."
  (let ((entered nil)
        (left    nil))
    (alloy:do-elements (item container)
      (cond ((alloy:contained-p (alloy:location event) item)
             (unless entered (setf entered item)))
            ((alloy:contained-p (alloy:old-location event) item)
             (push item left))))
    (let ((handled nil))
      (dolist (item (nreverse left))
        (when (alloy:handle event item) (setf handled t)))
      (when entered
        (when (alloy:handle event entered) (setf handled t))
        (when (typep entered 'popup-menu-item)
          (popup-hover-enter-row entered event)))
      (or handled (alloy:decline)))))

(defmethod alloy:handle ((event alloy:pointer-move) (bar alloy:menubar))
  (broadcast-pointer-move event bar))

(defun focus-skippable-p (element)
  "True if ELEMENT should be skipped during keyboard menu navigation (separator or disabled item)."
  (cond ((typep element 'popup-menu-separator) t)
        ((typep element 'popup-menu-item)
         (not (popup-item-enabled-p element)))
        (t nil)))

(defun skip-non-focusable (chain advance-fn)
  "Call ADVANCE-FN on CHAIN repeatedly until focused element is focusable, or full loop reached."
  (let* ((total (alloy:element-count chain))
         (limit total))
    (when (plusp total)
      (funcall advance-fn chain)
      (loop while (and (plusp limit)
                       (focus-skippable-p (alloy:focused chain)))
            do (funcall advance-fn chain)
               (decf limit))
      (alloy:focused chain))))

(defmethod alloy:focus-next :around ((chain popup-frame))
  (skip-non-focusable chain #'call-next-method))

(defmethod alloy:focus-prev :around ((chain popup-frame))
  (skip-non-focusable chain #'call-next-method))

;; fixed-layout's ensure-visible ROTATEFs the shared elements vector for z-order
;; (alloy layouts/fixed.lisp: the ensure-visible :before for (layout-element
;; fixed-layout)) — inside a popup that scrambles the focus chain; items never overlap.
(defmethod alloy:ensure-visible :around ((element alloy:layout-element) (layout popup-frame))
  element)

;; RET produces an activate action-event AND a raw key-down; the key-down handler
;; below performs the activation, so the action-event is consumed as a no-op.
(defmethod alloy:handle ((event alloy:activate) (chain popup-frame))
  t)

(defmethod alloy:handle ((event alloy:key-down) (chain popup-frame))
  (case (alloy:key event)
    ((:enter :return :kp-enter)
     (let ((item (alloy:focused chain)))
       (when item
         (if (and (typep item 'popup-menu-item)
                  (popup-item-enabled-p item)
                  (popup-item-submenu-tree item))
             (unless (popup-submenu-for-item-p item)
               (popup-close-deeper-than
                (popup-depth chain) (popup-owner-window chain))
               (popup-open-submenu-from
                item
                (popup-item-submenu-tree item)
                (1+ (popup-depth chain))
                (popup-owner-window chain)))
             (alloy:activate item))))
     t)
    (:escape
     (let ((pane  (popup-return-focus chain))
           (owner (popup-owner-window chain)))
       (popup-close-stack owner)
       (when pane
         ;; prefer re-pointing at the surviving :strong pane; the stored
         ;; return-focus is the fallback when none survived
         (or (and owner (popup-repair-focus owner))
             (popup-focus-pane owner pane))))
     t)
    (t (call-next-method))))

(defun menubar-neighbour-title (title delta)
  "The enabled menu-bar title DELTA places from TITLE, wrapping around the bar."
  (let ((bar (and (slot-boundp title 'alloy:layout-parent)
                  (alloy:layout-parent title)))
        (titles '()))
    (when bar
      (alloy:do-elements (element bar)
        (when (and (typep element 'menubar-title)
                   (not (menu-item-disabled-p element)))
          (push element titles)))
      (setf titles (nreverse titles))
      (let ((index (position title titles)))
        (when index
          (nth (mod (+ index delta) (length titles)) titles))))))

(defun popup-menubar-title-for (chain)
  "The menu-bar title whose dropdown CHAIN belongs to, or NIL for a context menu."
  (let ((title *menubar-open-title*))
    (when (and title
               (eq (menubar-title-window title) (popup-owner-window chain)))
      title)))

(defun menubar-open-neighbour (chain delta)
  "Move the open menu-bar dropdown DELTA titles along the bar, leaving the new
dropdown keyboard-ready, the way a native menu bar answers an arrow key that runs
past the ends of a menu. NIL when CHAIN is not a menu-bar dropdown or when the bar
carries no other enabled title."
  (let* ((title (popup-menubar-title-for chain))
         (next  (and title (menubar-neighbour-title title delta))))
    (when (and next (not (eq next title)))
      (let ((frame (menubar-open-title-popup next)))
        (when frame
          ;; the new chain opens focused on its TOP row; a disabled one is stepped over
          (when (focus-skippable-p (alloy:focused frame))
            (alloy:focus-next frame))
          t)))))

(defmethod alloy:handle ((event alloy:focus-right) (chain popup-frame))
  "Native vertical-menu semantics: Right opens the focused nested menu instead of
advancing to the next row; with no nested menu to open it moves the menu bar to
the next title, at any depth."
  (declare (ignore event))
  (let ((item (alloy:focused chain)))
    (cond ((and (typep item 'popup-menu-item)
                (popup-item-enabled-p item)
                (popup-item-submenu-tree item))
           (unless (popup-submenu-for-item-p item)
             (popup-close-deeper-than
              (popup-depth chain) (popup-owner-window chain))
             (popup-open-submenu-from
              item
              (popup-item-submenu-tree item)
              (1+ (popup-depth chain))
              (popup-owner-window chain)))
           t)
          ((menubar-open-neighbour chain 1) t)
          (t (alloy:decline)))))

(defmethod alloy:handle ((event alloy:focus-left) (chain popup-frame))
  "Left in a nested popup returns to its parent row; at the root of a menu-bar
dropdown it moves the bar to the previous title, and is otherwise declined."
  (declare (ignore event))
  (let ((depth (popup-depth chain)))
    (cond ((plusp depth)
           (let* ((parent-item (popup-parent-item chain))
                  (parent-menu (and parent-item
                                    (alloy:layout-parent parent-item)))
                  (owner (popup-owner-window chain)))
             (popup-close-deeper-than (1- depth) owner)
             (when (typep parent-menu 'popup-frame)
               (setf (alloy:focused parent-menu) parent-item)
               (setf (alloy:focus parent-menu) :strong))
             t))
          ((menubar-open-neighbour chain -1) t)
          (t (alloy:decline)))))

(defmethod alloy:activate :around ((item alloy:menu-item))
  (cond ((menu-item-disabled-p item) nil)
        (t (call-next-method))))

(defun popup-surface-live-p (surface)
  "SURFACE when its native window is still alive and not being reaped, else NIL."
  (when (and surface
             (window-registered-p surface)
             (%glfw:pointer surface)
             (not (%glfw:should-close-p surface)))
    surface))

(defun popup-live-surface ()
  "Native surface of the chain's ROOT level while alive, or NIL."
  (popup-surface-live-p *popup-menu-window*))

(defun popup-sync-root-surface ()
  (setf *popup-menu-window*
        (let ((root (find 0 *popup-menu-stack* :key #'popup-depth)))
          (and root (popup-frame-surface root)))))

(defun forget-popup-surface (surface)
  "Forget SURFACE as the chain's root surface once it is deallocated."
  (when (eq surface *popup-menu-window*)
    (setf *popup-menu-window* nil)))

(defun popup-resolve-focus-loss ()
  "Dismiss the chain once the owner's application is no longer active. An owned
popup never takes the native focus, so the decision belongs to the owner, exactly
as a native menu is dismissed when its application is deactivated."
  (when (and *popup-menu-stack*
             (not *popup-menu-closing-p*)
             (not (desktop-active-p)))
    (popup-close-stack)))

(defun popup-retire-frame (menu)
  "Detach MENU from its own surface and retire that surface. A GLFW window must
not be destroyed from inside an input callback: hide it now and let the normal
event-loop reaper deallocate it. A surfaceless frame (the zoom dropdown) lives
inside its owner window: it leaves both trees there and hands the focus back,
else the window keeps drawing it and routing keys at a retired chain."
  (let ((surface (popup-frame-surface menu)))
    (cond ((popup-surface-live-p surface)
           ;; a level can be retired twice (key + focus loss): the second pass finds
           ;; the frame already out of the surface, and the surface already reaped
           (handler-case (alloy:leave menu surface)
             (alloy:hierarchy-error (c)
               (warn "[popup-leave depth=~A] ~A"
                     (popup-depth menu) (princ-to-string c))))
           (handler-case (popup-hide-surface surface)
             (%glfw:glfw-error (c)
               (warn "[popup-hide depth=~A] ~A"
                     (popup-depth menu) (princ-to-string c))))
           (setf (%glfw:should-close-p surface) t))
          ((null surface)
           (let ((owner        (popup-owner-window menu))
                 (return-focus (popup-return-focus menu)))
             (handler-case
                 (progn
                   (let ((fp (and (slot-boundp menu 'alloy::focus-parent)
                                  (alloy:focus-parent menu))))
                     (when (and (typep fp 'alloy:focus-chain) (not (eq fp menu)))
                       (alloy:leave menu fp)))
                   (let ((lp (and (slot-boundp menu 'alloy::layout-parent)
                                  (alloy:layout-parent menu))))
                     (when (and lp (not (eq lp menu)))
                       (alloy:leave menu lp))))
               (alloy:hierarchy-error (c)
                 (warn "[popup-leave in-window] ~A" (princ-to-string c))))
             (when (typep return-focus 'alloy:focus-element)
               (popup-focus-pane owner return-focus))
             (when owner (alloy:mark-for-render owner)))))
    (setf (popup-frame-surface menu) nil)))

;;; The lambda list is the one every caller uses (the owner, optional, and the
;;; :suppress-title key); SBCL flags that mix as a style warning.
(declaim (sb-ext:muffle-conditions sb-kernel:&optional-and-&key-in-lambda-list))

(defun popup-close-stack (&optional owner &key suppress-title)
  (declare (ignore owner))
  (unless *popup-menu-closing-p*
    (let ((*popup-menu-closing-p* t)
          (menus *popup-menu-stack*)
          (title *menubar-open-title*))
      (when title
        (when (alloy:focus title) (setf (alloy:focus title) nil))
        (alloy:mark-for-render title))
      (setf *menubar-suppress-title* (and suppress-title title)
            *menubar-open-title* nil)
      (setf *popup-menu-stack* nil
            *popup-menu-window* nil)
      (dolist (menu menus) (popup-retire-frame menu)))))

(declaim (sb-ext:unmuffle-conditions sb-kernel:&optional-and-&key-in-lambda-list))

(defun popup-close-deeper-than (depth window)
  (declare (ignore window))
  (let ((to-close
          (remove-if-not (lambda (m) (> (popup-depth m) depth))
                         *popup-menu-stack*))
        (remaining
          (remove-if (lambda (m) (> (popup-depth m) depth))
                     *popup-menu-stack*)))
    ;; Nothing to close means nothing to touch: every surviving level keeps its
    ;; own untouched native surface.
    (when to-close
      (setf *popup-menu-stack* remaining)
      (dolist (menu to-close) (popup-retire-frame menu))
      (popup-sync-root-surface)
      ;; the deepest surviving level is re-ordered on screen without activation,
      ;; the way a native menu returns to its parent
      (let* ((deepest (first remaining))
             (surface (and deepest
                           (popup-surface-live-p
                            (popup-frame-surface deepest)))))
        (when surface (show-window-without-focus surface))))))

(defun popup-submenu-for-item-p (item)
  (some (lambda (m) (eq (popup-parent-item m) item)) *popup-menu-stack*))

(defun popup-call-items-function (fn window)
  "Dynamic-submenu generator: a function called with the window returning
menu items. Log and drop on error so one
bad generator does not kill the whole context menu."
  (when window
    (handler-case (funcall fn window)
      (error (c)
        (warn "[menu items-function] ~A" (princ-to-string c))
        nil))))

(defun popup-normalize-entries (raw window)
  "Flatten menu lists into entries: menu-item, menu, or :separator.
Sub-lists and menu-component are groups with separators around them,
recursively, like the toolkit's grouping of menu components. A function item is
a dynamic-items generator: call it with WINDOW and splice its result."
  (let ((result nil)
        (first-p t)
        (prev-group-p nil))
    (flet ((maybe-separator (group-p)
             (when (and (not first-p) (or group-p prev-group-p))
               (push :separator result))))
      (dolist (item raw)
        (cond
          ((null item))
          ((eq item :separator)
           (push :separator result)
           (setf first-p nil prev-group-p nil))
          ((functionp item)
           (let ((subs (popup-normalize-entries
                        (popup-call-items-function item window) window)))
             (when subs
               (maybe-separator t)
               (dolist (sub subs) (push sub result))
               (setf first-p nil prev-group-p t))))
          ((or (consp item) (typep item 'menu-component))
           (let ((subs (popup-normalize-entries
                        (if (consp item) item (menu-entry-items item)) window)))
             (when subs
               (maybe-separator t)
               (dolist (sub subs) (push sub result))
               (setf first-p nil prev-group-p t))))
          (t
           (maybe-separator nil)
           (push item result)
           (setf first-p nil prev-group-p nil)))))
    (nreverse result)))

(defun popup-count-items-and-separators (entries)
  (let ((items 0) (seps 0))
    (dolist (entry entries)
      (if (eq entry :separator) (incf seps) (incf items)))
    (values items seps)))

(defun popup-compute-height (entries)
  (multiple-value-bind (n-items n-seps) (popup-count-items-and-separators entries)
    (+ (* n-items *popup-menu-item-height*)
       (* n-seps  *popup-menu-separator-height*))))

(defun popup-make-item (entry window return-focus)
  "ENTRY is a menu-item (leaf) or a menu (submenu parent)."
  (let* ((title    (menu-entry-title entry))
         (enabled  (menu-entry-enabled-now entry))
         (submenu? (typep entry 'menu))
         (action   (and (typep entry 'menu-item)
                        (menu-entry-action entry)))
         (subtree  (when submenu? (menu-entry-items entry)))
         (label    (menu-entry-label entry)))
    (make-instance 'popup-menu-item
                   :value        label
                   :enabled-p    enabled
                   :selected-p   (menu-entry-selected-now entry)
                   :submenu-tree subtree
                   :on-activate
                   (lambda ()
                     (cond
                       ((not enabled) nil)
                       (subtree       nil)
                       (t
                        (popup-close-stack window)
                        (unwind-protect
                             (when action
                               (handler-bind
                                   ((error (lambda (c)
                                             (format *error-output*
                                                     "~&[ERR] popup-action ~S: ~A~%"
                                                     title (princ-to-string c))
                                             (finish-output *error-output*))))
                                 (funcall action)))
                          (when return-focus
                            ;; prefer the surviving :strong pane; the
                            ;; stored return-focus is the fallback, focusable only
                            ;; while still hooked into a focus tree -- the action
                            ;; may have demolished it (alloy focus-tree.lisp: the
                            ;; (setf focus) for (:strong focus-element))
                            (or (and window (popup-repair-focus window))
                                (popup-focus-pane window return-focus))))))))))

(defun popup-fit-screen-rect (left top width height)
  "Clamp a menu rectangle to the selected monitor's usable work area. Return
LEFT, TOP and the monitor. TOP is y-down virtual-screen coordinates."
  ;; glfw-error: the monitor may be unplugged between the lookup and this call
  (let* ((monitor (%popup-monitor-for-screen-point left top))
         (work (and monitor (handler-case (%glfw:work-area monitor)
                              (%glfw-unavailable () nil)))))
    (if work
        (destructuring-bind (wx wy ww wh) work
          (values
           (if (<= width ww)
               (max wx (min left (- (+ wx ww) width)))
               wx)
           (if (<= height wh)
               (max wy (min top (- (+ wy wh) height)))
               wy)
           monitor))
        (values left top monitor))))

(defun popup-owner-point-to-screen (window px py)
  "Convert an Alloy point in WINDOW (origin bottom-left) to GLFW virtual-screen
coordinates (origin top-left)."
  (let ((position (%glfw:location window))
        (size (%glfw:size window)))
    (values (+ (first position) px)
            (+ (second position) (- (second size) py)))))

(defun popup-frame-screen-rect (frame)
  (values (popup-screen-left frame)
          (popup-screen-top frame)
          (+ (popup-screen-left frame) (popup-screen-width frame))
          (+ (popup-screen-top frame) (popup-screen-height frame))))

(defun popup-label-width (entries window)
  "Widest row label of ENTRIES in px, measured the way the label shape is drawn
\(MSDF advances of the :menu role font) under WINDOW's unit context. WINDOW, the
chain's owner, is the renderer the font resolves under; the average-advance
estimate stands in only when no MSDF font resolves."
  (flet ((measure (label)
           (let ((font (font-entry-font (role-font-entry window :menu))))
             (if (typep font 'org.shirakumo.alloy.renderers.opengl.msdf::font)
                 (string-advance font (role-draw-px window (native-role :menu)) label)
                 (float (* 0.6f0 (role-points-px :menu) (length (or label ""))) 0f0)))))
    (alloy:with-unit-parent window
      (let ((w 0))
        (dolist (entry entries w)
          (unless (eq entry :separator)
            (setf w (max w (ceiling (measure (menu-entry-label entry)))))))))))

(defun popup-menu-width (entries window screen-left screen-top)
  "Width that fits every row label between the tick and chevron gutters, never
below the minimum and never wider than the target monitor's usable width.
The window system sizes a native menu from its content; this popup mirrors that
behaviour while clamping the result to the monitor work area."
  (let* ((want (+ (popup-label-width entries window)
                  *popup-menu-label-slack*
                  *popup-menu-label-left*
                  *popup-menu-label-right*))
         (monitor (%popup-monitor-for-screen-point screen-left screen-top))
         ;; glfw-error: the monitor may be unplugged between the lookup and this call
         (work    (and monitor (handler-case (%glfw:work-area monitor)
                                 (%glfw-unavailable () nil))))
         (cap     (if work (third work) want)))
    (max *popup-menu-min-width* (min cap want))))

(defun popup-drop-anchor (window widget height)
  "Virtual-screen (LEFT TOP, y-down) for a dropdown hanging off WIDGET: BELOW it,
flipped ABOVE only when the rectangle would leave the monitor work area. The
placement is resolved independently on each axis and clamped to the monitor's
usable work area."
  (let* ((origin (alloy:global-location widget))
         (x      (alloy:pxx origin))
         (y-bot  (alloy:pxy origin))
         (y-top  (+ y-bot (alloy:pxh (alloy:bounds widget)))))
    (multiple-value-bind (sx below) (popup-owner-point-to-screen window x y-bot)
      (multiple-value-bind (ignored above) (popup-owner-point-to-screen window x y-top)
        (declare (ignore ignored))
        (let* ((monitor (%popup-monitor-for-screen-point sx below))
               ;; glfw-error: the monitor may be unplugged between lookup and call
               (work    (and monitor (handler-case (%glfw:work-area monitor)
                                       (%glfw-unavailable () nil))))
               (fits    (or (null work)
                            (<= (+ below height) (+ (second work) (fourth work))))))
          (values sx (if fits below (- above height))))))))

(defun popup-attach-surface (menu window width height)
  "Give MENU its own native surface at its recorded screen rect and register it in
the chain. One surface per level, born at its final size and never resized: a
resize rebuilds the swap chain, which the compositor shows as a blank frame over
every level already on screen."
  (let ((surface (popup-make-surface window
                                     (popup-screen-left menu)
                                     (popup-screen-top menu)
                                     width height)))
    (setf (popup-frame-surface menu) surface)
    (when (null *popup-menu-stack*)
      (setf *popup-menu-window* surface))
    (push menu *popup-menu-stack*)
    (alloy:enter menu surface :x 0 :y 0 :w width :h height)
    surface))

(defun popup-present-frame (menu surface)
  "Take the strong focus, draw once, and order on screen without activation so the
owner keeps the native input focus. Drawing before the first appearance matters: a
transparent surface that has not been rendered exposes one empty frame."
  ;; NO-APPLICABLE-METHOD raised when prior focus-list element lacks (setf focus) method
  (handler-case (setf (alloy:focus menu) :strong)
    (error (c)
      (warn "[popup focus :strong] ~A" (princ-to-string c))))
  (alloy:mark-for-render surface)
  (alloy:render surface surface)
  (show-window-without-focus surface)
  menu)

(defun popup-open-menu-at (entries depth window screen-left screen-top
                           &key parent-item return-focus opener)
  (when (popup-consume-opener-suppression-p opener)
    (return-from popup-open-menu-at nil))
  ;; a menu with nothing to show opens with one disabled "(empty)" item, as CAPI does
  (let* ((entries (or entries (list (make-menu-item "(empty)" nil :enabled nil))))
         (width  (popup-menu-width entries window screen-left screen-top))
         (height (popup-compute-height entries))
         (left   nil)
         (top    nil)
         (menu   nil)
         (surface nil))
    (multiple-value-setq (left top)
      (popup-fit-screen-rect screen-left screen-top width height))
    (setf menu (make-instance 'popup-frame
                              :depth         depth
                              :owner-window  window
                              :parent-item   parent-item
                              :return-focus  return-focus
                              :opener        opener
                              :screen-left   left
                              :screen-top    top
                              :screen-width  width
                              :screen-height height))
    (setf surface (popup-attach-surface menu window width height))
    (let ((cur-y height)
          (top-item nil))
      (dolist (entry entries)
        (cond
          ((eq entry :separator)
           (decf cur-y *popup-menu-separator-height*)
           (let ((sep (make-instance 'popup-menu-separator)))
             (alloy:enter sep menu :x 0 :y cur-y :w width :h *popup-menu-separator-height*)))
          (t
           (decf cur-y *popup-menu-item-height*)
           (let ((item (popup-make-item entry window return-focus)))
             (alloy:enter item menu :x 0 :y cur-y :w width :h *popup-menu-item-height*)
             (unless top-item (setf top-item item))))))
      (when top-item
        (setf (alloy:focused menu) top-item)))
    (popup-present-frame menu surface)))

(defun open-popup-menu (window items x y &key return-focus)
  "Open ITEMS as a popup menu at WINDOW-local X/Y and return its popup frame."
  (when window
    (let ((raw (if (typep items 'menu) (menu-entry-items items) items)))
      (popup-close-stack window)
      (multiple-value-bind (screen-left screen-top)
          (popup-owner-point-to-screen window x y)
        (popup-open-menu-at (popup-normalize-entries raw window)
                            0 window screen-left screen-top
                            :return-focus return-focus)))))

(defun menubar-title-screen-anchor (item)
  "Virtual-screen point (LEFT TOP, y-down) of ITEM's bottom-left corner, where the
dropdown must start."
  (let ((window (menubar-title-window item))
        (origin (alloy:global-location item)))
    (when window
      (popup-owner-point-to-screen window (alloy:pxx origin) (alloy:pxy origin)))))

(defun menubar-open-title-popup (item)
  "Open ITEM's dropdown as a popup chain, tearing down the menubar chain already
open. Every title switch retires the current levels and creates a new native
surface for the new dropdown."
  (let ((window (menubar-title-window item))
        (menu   (menubar-title-menu item)))
    (when (and window menu)
      (let ((previous *menubar-open-title*))
        (when (and previous (not (eq previous item)) (alloy:focus previous))
          (setf (alloy:focus previous) nil)
          (alloy:mark-for-render previous)))
      (if *popup-menu-stack*
          (popup-close-deeper-than -1 window)
          (popup-close-stack window))
      (setf *menubar-open-title*     nil
            *menubar-suppress-title* nil)
      (multiple-value-bind (left top) (menubar-title-screen-anchor item)
        (when left
          (let ((frame (popup-open-menu-at
                        (popup-normalize-entries (menu-entry-items menu) window)
                        0 window left top
                        :return-focus (menubar-return-focus window))))
            (when frame
              (setf *menubar-open-title* item)
              ;; keep the title lit while its dropdown is up (native menu bar)
              ;; the :after of (setf focus) reaches notice-focus, which signals
              ;; element-not-contained -- or unbound-slot when the title already left
              (handler-case (setf (alloy:focus item) :weak)
                ((or alloy:hierarchy-error unbound-slot) (c)
                  (warn "[menubar title focus ~S] ~A"
                        (alloy:value item) (princ-to-string c))))
              (alloy:mark-for-render item))
            frame))))))

(defmethod alloy:activate ((item menubar-title))
  "Click on a title opens its dropdown; clicking the open one closes it. The
pointer-down half of that second click already tore the chain down, so the
suppress flag is what distinguishes closing from re-opening."
  (let ((suppress *menubar-suppress-title*))
    (setf *menubar-suppress-title* nil)
    (cond ((eq item suppress) nil)
          ((eq item *menubar-open-title*)
           (popup-close-stack (menubar-title-window item))
           (setf *menubar-suppress-title* nil)
           nil)
          (t (menubar-open-title-popup item)))))

(defun menubar-clear-suppress-unless-hit (window point)
  "The one-shot re-click guard survives only a click on the very title it was
recorded for; any other click re-arms normal activation."
  (let ((title *menubar-suppress-title*))
    (when (and title
               (not (and (eq window (menubar-title-window title))
                         (alloy:contained-p point title))))
      (setf *menubar-suppress-title* nil))))

(defun popup-open-submenu-from (parent-item subtree depth window)
  (let* ((parent-menu (alloy:layout-parent parent-item))
         (rf          (when (typep parent-menu 'popup-frame)
                         (popup-return-focus parent-menu)))
         ;; anchor in the PARENT level's own surface, the only one whose
         ;; coordinates the parent item lives in
         (surface  (when (typep parent-menu 'popup-frame)
                     (popup-surface-live-p (popup-frame-surface parent-menu))))
         (origin   (and surface (alloy:global-location parent-item)))
         (bounds   (alloy:bounds parent-item))
         (entries  (popup-normalize-entries subtree window)))
    (when window
      (when (and surface origin)
        (let* ((surface-pos (%glfw:location surface))
               (surface-size (%glfw:size surface))
               (item-left (+ (first surface-pos) (alloy:pxx origin)))
               (item-top (+ (second surface-pos)
                            (- (second surface-size)
                               (+ (alloy:pxy origin)
                                  (alloy:pxh bounds)))))
               (item-right (+ item-left (alloy:pxw bounds)))
               ;; the cascade side depends on the width, so it is measured here
               (width (popup-menu-width entries window item-right item-top))
               (right (- item-right 4))
               (left (+ (- item-left width) 4))
               (monitor (%popup-monitor-for-screen-point item-right item-top))
               (work (and monitor (%glfw:work-area monitor)))
               (work-left (and work (first work)))
               (work-right (and work (+ (first work) (third work))))
               ;; Prefer the conventional right cascade. Flip to the left only
               ;; when it fits better there; the final monitor clamp is shared.
               (screen-left
                 (if (and work-right
                          (> (+ right width) work-right)
                          (or (null work-left) (>= left work-left)))
                     left
                     right)))
          (popup-open-menu-at entries depth window screen-left item-top
                              :parent-item parent-item :return-focus rf))))))

(defmethod alloy:handle ((event alloy:pointer-move) (frame popup-frame))
  (broadcast-pointer-move event frame))

(defun popup-immediate-child (menu)
  "The currently open child level belonging directly to MENU, or NIL."
  (find-if
   (lambda (candidate)
     (let ((parent-item (popup-parent-item candidate)))
       (and (= (popup-depth candidate) (1+ (popup-depth menu)))
            parent-item
            (eq (alloy:layout-parent parent-item) menu))))
   *popup-menu-stack*))

(defun popup-local-point-to-screen (frame point)
  "Convert POINT from FRAME's Alloy y-up surface coordinates to screen y-down."
  (values (+ (popup-screen-left frame) (alloy:pxx point))
          (+ (popup-screen-top frame)
             (- (popup-screen-height frame) (alloy:pxy point)))))

(defun popup-pointer-heading-into-child-p (event menu)
  "True only while EVENT's physical trajectory crosses into MENU's open child.

This is the native menu-aim corridor: a diagonal trip to a submenu can briefly
cross a sibling row in the parent.  Projecting the motion vector onto the
child's near edge preserves that child without a timer; every trajectory that
misses its vertical span still closes immediately."
  (let ((child (popup-immediate-child menu)))
    (when (and child event)
      (multiple-value-bind (old-x old-y)
          (popup-local-point-to-screen menu (alloy:old-location event))
        (multiple-value-bind (new-x new-y)
            (popup-local-point-to-screen menu (alloy:location event))
          (let* ((child-left   (popup-screen-left child))
                 (child-top    (popup-screen-top child))
                 (child-right  (+ child-left (popup-screen-width child)))
                 (child-bottom (+ child-top (popup-screen-height child)))
                 (menu-left    (popup-screen-left menu))
                 (menu-right   (+ menu-left (popup-screen-width menu)))
                 (child-left-p (< (+ child-left child-right)
                                  (+ menu-left menu-right)))
                 (edge-x       (if child-left-p child-right child-left))
                 (dx           (- new-x old-x))
                 (dy           (- new-y old-y)))
            (when (and (if child-left-p (minusp dx) (plusp dx))
                       (not (zerop dx)))
              (let ((fraction (/ (- edge-x old-x) dx)))
                (when (<= 0 fraction)
                  (let ((projected-y (+ old-y (* fraction dy))))
                    (<= child-top projected-y child-bottom)))))))))))

(defun popup-hover-enter-row (item &optional event)
  "Drive the chain for the row the pointer just entered: open ITEM's submenu, or
close every deeper level when ITEM has none. Called once per pointer-move by
broadcast-pointer-move, for the single row it resolved as entered."
  (let* ((menu       (alloy:layout-parent item))
         (menu-depth (when (typep menu 'popup-frame) (popup-depth menu)))
         (window     (when (typep menu 'popup-frame) (popup-owner-window menu)))
         (subtree    (popup-item-submenu-tree item))
         (enabled    (popup-item-enabled-p   item)))
    (when (and menu-depth window)
      (cond ((and subtree enabled)
             (unless (popup-submenu-for-item-p item)
               (popup-close-deeper-than menu-depth window)
               (popup-open-submenu-from item subtree (1+ menu-depth) window)))
            (t
             (unless (and event
                          (popup-pointer-heading-into-child-p event menu))
               (popup-close-deeper-than menu-depth window)))))))

(defun popup-point-in-element-p (point element)
  "POINT is in the window's own coordinates; an element nested in layouts carries
bounds relative to its layout parent, so the test needs the global origin."
  (let* ((origin (alloy:global-location element))
         (bounds (alloy:bounds element))
         (ox (alloy:pxx origin))
         (oy (alloy:pxy origin)))
    (and (<= ox (alloy:pxx point) (+ ox (alloy:pxw bounds)))
         (<= oy (alloy:pxy point) (+ oy (alloy:pxh bounds))))))

(defun popup-record-opener-hit (window point)
  "Arm the one-shot re-click guard when the pointer-down that tears the chain down
lands on the very widget that opened it; any other pointer-down disarms it."
  (let* ((root   (car (last *popup-menu-stack*)))
         (opener (and root (popup-opener root))))
    (setf *popup-suppress-opener*
          (when (and opener
                     (eq window (element-window opener))
                     (popup-point-in-element-p point opener))
            opener))))

(defun popup-pointer-target (window event)
  "The open popup level whose own surface is WINDOW and which holds EVENT's
location, or NIL. Each level's coordinates belong exclusively to its own surface:
comparing them with another window's local event coordinates can route one
physical click twice."
  (let* ((stack *popup-menu-stack*)
         (px    (alloy:pxx (alloy:location event)))
         (py    (alloy:pxy (alloy:location event)))
         (pt    (alloy:px-point px py)))
    (and stack
         (find-if (lambda (m)
                    (and (eq window (popup-frame-surface m))
                         (alloy:layout-tree m)
                         (alloy:contained-p pt m)))
                  stack))))

(defun popup-note-pointer-down (window point)
  "A pointer-down at POINT of WINDOW that no popup level took: arm or disarm the
opener's re-click guard, tear the open chain down, and keep the menu-bar title's
re-click guard only for a click on that very title."
  (popup-record-opener-hit window point)
  (when *popup-menu-stack* (popup-close-stack window :suppress-title t))
  (menubar-clear-suppress-unless-hit window point))

;;; ---- Accelerators -------------------------------------------------------------

(defgeneric handles-accelerator-key-p (element key)
  (:documentation
   "True when KEY, under the command modifier, is ELEMENT's OWN text-editing gesture
and therefore must not reach the menu: a focused text pane consumes Ctrl+X/C/V/A
before the menubar sees them. Alloy's emacs bindings C-b/e/f/n/p
(alloy components/text-input.lisp: the emacs shortcuts of the handle for (key-down
text-input-component)) are never claimed by this menu layer. Undo is not claimed
either: it belongs to the window's Edit menu.")
  (:method ((element t) key) (declare (ignore key)) nil)
  (:method ((element alloy:text-input-component) key)
    (and (member key '(:x :c :v :a)) t)))

(defgeneric handle-accelerator-key (element key)
  (:documentation
   "Perform KEY's editing gesture on ELEMENT. Alloy maps cut/copy/paste to Control
only (alloy actions.lisp: the default mappings of paste-event, copy-event and
cut-event) and gates select-all on :control (alloy components/text-input.lisp: the
select-all gate of the handle for (key-down text-input-component)),
so when the event carries a command modifier that is NOT Control -- Command on
macOS (input/modifiers.lisp: command-modifier) -- the gesture must be issued here, or the key
would reach neither the widget nor the menu.")
  (:method ((element t) key) (declare (ignore key)) nil)
  (:method ((element alloy:text-input-component) key)
    (case key
      (:x (alloy:handle (make-instance 'alloy:cut-event) element) t)
      (:c (alloy:handle (make-instance 'alloy:copy-event) element) t)
      (:v (let ((clip (handler-case (alloy:clipboard (element-window element))
                        ;; GLFW signals FORMAT-UNAVAILABLE when the OS clipboard
                        ;; holds no text; a paste with nothing to paste is a no-op
                        (%glfw-unavailable () nil))))
            (when (stringp clip)
              (alloy:handle (make-instance 'alloy:paste-event :content clip) element))
            t))
      (:a (let ((cursor (alloy:cursor element)))
            (setf (alloy:anchor cursor) 0)
            (alloy:move-to :end cursor)
            (alloy:mark-for-render element)
            t)))))

(defun popup-key-surface-for (window)
  "Live surface of the deepest open popup level owned by WINDOW, or NIL.
*POPUP-MENU-STACK* is pushed, so its first matching element is the deepest."
  (let ((frame (find window *popup-menu-stack* :key #'popup-owner-window)))
    (and frame (popup-surface-live-p (popup-frame-surface frame)))))

(defmethod alloy:handle :around ((event alloy:key-down) (window window))
  "While a popup chain owned by WINDOW is up the keyboard belongs to the chain: an
owned surface never takes the native focus, so the OS delivers every key to the
owner, which forwards it on. Otherwise a command-key press goes to the focused pane
that claims it (HANDLES-ACCELERATOR-KEY-P), else to the matching accelerator of
WINDOW-MENU-MODEL."
  (let ((popup-surface (popup-key-surface-for window)))
    (if (and popup-surface
             (not (eq popup-surface window))
             (alloy:handle event popup-surface))
        t
        (let* ((mods    (alloy:modifiers event))
               (key     (alloy:key event))
               (shift-p (and (find :shift mods) t))
               (accel-p (accelerator-modifier-p mods))
               (action  (and accel-p
                             (find-accelerator-action (window-menu-model window) key shift-p)))
               (pane    (and accel-p (not shift-p) (strong-focus-pane window)))
               (owner   (and pane (handles-accelerator-key-p pane key) pane)))
          (cond
            ;; the focused widget owns this key: alloy's input map delivers it only under
            ;; Control (alloy actions.lisp: the default mappings of paste-event,
            ;; copy-event and cut-event), so any other command modifier is ours
            ((and owner (not (find :control mods)))
             (handle-accelerator-key owner key)
             t)
            (owner (call-next-method))
            (action (funcall action) t)
            (t (call-next-method)))))))
