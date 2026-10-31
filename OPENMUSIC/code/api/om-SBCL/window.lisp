(in-package :om-api)

(export '(om-window
          om-make-window
          om-window-title
          om-set-window-title
          om-interior-size
          om-set-interior-size
          om-resize-callback
          om-def-higher-level-win
          om-front-window
          om-get-all-windows
          om-fullscreen-window
          om-screen-size
          om-window-maximized
          om-maximize-window
          om-dialog
          om-textured-dialog
          om-modal-dialog
          om-return-from-modal-dialog
          om-windoid
          om-no-border-win
          om-window-check-before-close
          om-window-close-event
          om-close-window
          om-window-open-p
          om-window-visible-p
          om-select-window
          om-hide-window
          om-show-window
          om-window-activate
          om-window-resized
          om-minimum-size
          om-look-window
          om-with-screen
          *default-window-size*
          om-window-root
          om-window-focus
          om-run-loop)
        :om-api)

;;; Screen + default size

(defparameter *default-window-size* (alloy:px-size 800 600))

(defvar *gui-startup* nil
  "True on the Emacs-mode GUI thread from its start until om-run-loop.")

(defmacro om-with-screen (&body body)
  ;; the session shell is alloy-desktop's; what stays here is OM's: the initial
  ;; bindings an OM process has and the trap set OM runs with.
  ;; In Emacs mode the GUI runs in its own thread, so the SLIME REPL stays free.
  (let ((screen (gensym "SCREEN")))
    `(flet ((run-gui ()
              (alld:with-desktop-application (,screen :initial-bindings *process-initial-bindings*
                                                      :armed-traps '(:divide-by-zero))
                (declare (ignorable ,screen))
                ,@body)))
       (if *emacs-mode*
           (bordeaux-threads:make-thread (lambda ()
                                           (let ((*gui-startup* t))
                                             (alld:post-to-ui-thread
                                              (lambda () (om-emacs-eval '(activate-om))))
                                             (run-gui)))
                                         :name "OpenMusic GUI")
           (run-gui)))))

(defvar *printing-window* nil
  "The window a print job is drawing, while it draws it; NIL otherwise.
Set and cleared by the print command (print.lisp: %run-vector-print-job and
%finish-print-job). The window is not repainted and cannot be closed while the
job holds it, because the job walks the same graphic tree from its own thread.")

(defun %window-frozen-p (window)
  "True while WINDOW must not be repainted from this thread.

A print job draws its pages from the SAME graphic tree the screen draws from,
writing the layout rectangles back before each page (kernel-patch-after.lisp:
%score-restore-rectangles), and it does that on a thread of its own. Every path
that paints a window answers to this: the frame loop, and the refresh the window
manager asks for when something passes over it. The window is disabled for the
whole job, so it takes no input and owes no frame."
  (eq window *printing-window*))

;;; om-look-window: GLFW backend window with content-scale + refresh hooks.

;; OM-level window slots remain on om-look-window so EVERY window class
;; (incl. fork editor windows that subclass it directly) carries them.
(defclass om-look-window (om-graphic-object
                          alld:window
                          capi:interface)
  ((resizable           :initarg :resizable :initform t   :accessor resizable)
   (fs                  :initarg :fs        :initform nil :accessor fs)
   (destroy-callback    :initarg :destroy-callback :initform nil :accessor window-destroy-callback)
   (name                :initarg :name      :initform nil :accessor window-name)
   (instance-menu-items :initarg :menu-items :initform nil :accessor window-instance-menu-items)
   (last-w              :initform nil :accessor window-last-w)
   (last-h              :initform nil :accessor window-last-h)))

;;; Focus authority. CAPI has two native levels: the ACTIVE interface
;;; and the focused pane. The reusable authority and Alloy focus-tree mechanics
;;; now live in ALLOY-DESKTOP; this bridge keeps only OM hover, selection, caret,
;;; and diagnostics policy.

(defvar *om-focus-repairs* 0
  "INVARIANT 1 violations healed so far; each one is reported on *error-output*.")

(defun %om-window-active-p (window)
  (and window (alld:window-focused-p window)))

(defun %om-focus-pane (window pane)
  "Give PANE the strong desktop focus, then apply OM selection/caret policy.
The low-level focus-tree operation is owned by ALLOY-DESKTOP; the hover guard
stands down here exactly as before because this is an intentional focus change."
  (let ((alld:*hover-move-p* nil))
    (alld:focus-pane
     window pane
     :after-focus
     (lambda (pane)
       ;; measured matrix: moving the focus inside the window undoes the
       ;; previous field's selection in a PATCH only. A dialog keeps both alive,
       ;; so the owner is only re-pointed at another TEXT pane -- focusing a button
       ;; must not lose the pane whose highlight still follows the OS focus.
       (let ((win (%om-pane-window window pane)))
         (when win
           (let ((owner (alld:window-selected-text-pane win)))
             (when (and owner (not (eq owner pane))
                        (alld:click-clears-selection-p win))
               (alld:release-text-selection win)))
           (when (typep pane 'alloy:text-input-component)
             (setf (alld:window-selected-text-pane win) pane))))
       (alld:reset-caret-blink)))))

(defun %om-unfocus-pane (window pane)
  "Remove PANE's strong desktop focus while preserving OM selection policy."
  (let ((alld:*hover-move-p* nil))
    (alld:unfocus-pane window pane)))

(defun %om-strong-pane (window)
  "The :strong leaf under WINDOW's OM content focus root, or NIL."
  (alld:strong-focus-pane window))

;; CAPI: "sets the input focus to pane or one of its children"
;; (capi-capi-ref-439.htm:50); returns the pane, never NIL.
(defmethod capi::set-pane-focus ((pane alloy:focus-element))
  (%om-focus-pane nil pane)
  pane)

;; An om-view is deliberately no focus-element (view.lisp: defclass om-view), so the CAPI effect
;; the kernel depends on is the other half: the text pane loses the input focus.
(defmethod capi::set-pane-focus ((pane om-graphic-object))
  (if (typep pane 'alloy:focus-element)
      (%om-focus-pane nil pane)
      (let* ((window (om-view-window pane))
             (strong (and window (%om-strong-pane window))))
        (when strong (%om-unfocus-pane window strong))))
  pane)

(defun %om-repair-window-focus (window)
  "Repair generic focus-tree drift and retain the existing OM diagnostic counter."
  (let ((alld:*hover-move-p* nil))
    (multiple-value-bind (pane repaired-p)
        (alld:repair-window-focus window :focus-function #'%om-focus-pane)
      (when repaired-p
        (incf *om-focus-repairs*)
        (format *error-output* "~&[om-focus] repair #~D: ~A re-pointed at ~A~%"
                *om-focus-repairs* (type-of window) (type-of pane)))
      pane)))

;;; Caret blink. CAPI blinks the caret natively (om-LW carries zero code for it);
;;; the cycle is alloy-desktop's (text/caret.lisp: tick-caret-blink), and this
;;; bridge only names the pane that owns the caret.

(defun %window-focused-text (window)
  "WINDOW's strongly focused text widget, or NIL. Only the ACTIVE window's pane
owns a caret; a tree that drifted off its :strong pane is repaired before
answering, so a stale (alloy:focused tree) can never freeze the blink."
  (when (%om-window-active-p window)
    (let* ((tree    (alloy:focus-tree window))
           (focused (or (%om-repair-window-focus window)
                        (and tree (alloy:focused tree)))))
      (when (and (typep focused 'alloy:text-input-component)
                 (eql :strong (alloy:focus focused)))
        focused))))

(defmethod org.shirakumo.alloy.animation:update :after
    ((window om-look-window) dt)
  (declare (ignore dt))
  (alld:tick-caret-blink window #'%window-focused-text))

;;; Subview protocol: the kernel calls (om-add-subviews <window> …) directly
;;; (kernel tools/lisptools.lisp: om-y-or-n-option-dialog). om-add-subviews/om-subviews
;;; dispatch via the om-graphic-object mixin; here internal-add-subview routes + REALIZES
;;; into om-window-root.

(defmethod internal-add-subview ((self om-look-window) (subview om-graphic-object))
  (setf (vcontainer subview) self)
  (setf (vsubviews self) (append (vsubviews self) (list subview)))
  (let ((root (om-window-root self)))
    (flet ((%enter (element)
             ;; the root retains the Y-down geometry and re-applies it over its live
             ;; height on every refit; a focusable element joins the window's focus list
             (when root
               (alld:add-element root element)
               (alld:place-element root element (vx element) (vy element) (vw element) (vh element)))))
      (%enter subview)
      ;; realization point: vanilla's om-create-callback sets initialized-p here
      (setf (initialized-p subview) t)
      (dolist (extra (%dialog-item-extra-subviews subview))
        (%enter extra))
      (when root (alloy:mark-for-render self))))
  subview)

(defmethod internal-remove-subview ((self om-look-window) (subview om-graphic-object))
  (setf (vcontainer subview) nil)
  (setf (vsubviews self) (remove subview (vsubviews self)))
  (let ((root (om-window-root self)))
    (flet ((%leave (element)
             ;; leave signals on a foreign parent (alloy layout.lisp: leave :before for
             ;; (layout-element layout)); leaving the root also takes the element out of
             ;; the window's focus list
             (when (and root (eq (alld:layout-attached-p element) root))
               (alloy:leave element root))))
      (%leave subview)
      (dolist (extra (%dialog-item-extra-subviews subview))
        (%leave extra))
      (when root (alloy:mark-for-render self))))
  subview)

;; the window's base-scale and font cache are already updated (alloy-desktop
;; window/assembly.lisp: glfw window-content-scale-changed :after for window)
(defmethod alld:window-scale-changed ((window om-look-window) scale)
  (alld:note-content-scale scale)
  ;; the OS reports its native metrics in device pixels, so a DPI change makes
  ;; the whole native-role snapshot stale
  (%om-native-fonts-refresh))

;; neither the frame loop nor a repaint asked by the window system paints a window
;; a print job is drawing (alloy-desktop window/assembly.lisp: glfw window-refreshed
;; :after for window; core/event-pump.lisp: frame-once)
(defmethod alld:window-render-suspended-p ((window om-look-window))
  (%window-frozen-p window))

;; Minimizing fires resize(0,0)/move(-32009) on win32; CAPI keeps reporting the
;; restored geometry, so an iconified window must not update bounds (wsparams save).
(defmethod org.shirakumo.fraf.glfw:window-resized :around ((window om-look-window) w h)
  (declare (ignore w h))
  (unless (org.shirakumo.fraf.glfw:iconified-p window)
    (call-next-method)))

(defmethod org.shirakumo.fraf.glfw:window-moved :around ((window om-look-window) x y)
  (declare (ignore x y))
  (unless (org.shirakumo.fraf.glfw:iconified-p window)
    (call-next-method)))

;; the chrome and the content are already laid out (alloy-desktop window/assembly.lisp: glfw window-resized :after for window)
(defmethod org.shirakumo.fraf.glfw:window-resized :after ((window om-look-window) w h)
  (om-resize-callback window 0 0 w h)
  ;; Cocoa redraws a live resize through the view's damage callback, which reaches window-refreshed.
  #+darwin
  (when (alld::%live-resize-p window)
    (return-from org.shirakumo.fraf.glfw:window-resized))
  ;; X11 queues several ConfigureNotify per gesture; rendering each one here pairs
  ;; a stale layout with a newer framebuffer. Mark, and let the frame loop render
  ;; once from the final geometry.
  (if (eq :x11 (%glfw:platform))
      (alloy:mark-for-render window)
      (alloy:render window window)))

;;; X11 answers the framebuffer size from the live server state, which can be ahead
;;; of the last ConfigureNotify the callbacks delivered. One frame must use a single
;;; geometry for layout, projection and viewport.

#+linux
(defvar *x11-render-window* nil)

#+linux
(defvar *x11-render-framebuffer-size* nil)

#+linux
(defmethod %glfw:framebuffer-size :around ((window om-look-window))
  (if (and (eq window *x11-render-window*)
           *x11-render-framebuffer-size*)
      *x11-render-framebuffer-size*
      (call-next-method)))

#+linux
(defun %sync-x11-render-geometry (window framebuffer-size)
  "Bring WINDOW's bounds, projection, chrome and OM size to FRAMEBUFFER-SIZE without writing the native size or the size hints."
  (destructuring-bind (w h) framebuffer-size
    (let ((bounds (alloy:bounds window)))
      (unless (and (= w (round (alloy:pxw bounds)))
                   (= h (round (alloy:pxh bounds))))
        (let ((extent (alloy:px-extent (alloy:pxx bounds) (alloy:pxy bounds) w h)))
          (setf (slot-value window 'alloy:bounds) extent)
          (alloy:suggest-size extent window)
          (let ((target (simple:transform-matrix window)))
            (simple:orthographic-matrix w h target)
            (setf (simple:identity-matrix window) (copy-seq target)))
          (let ((chrome (om-window-chrome window)))
            (when chrome (alloy:refit chrome)))
          (setf (vw window) w (vh window) h)
          (om-window-resized window (om-make-point w h))))))
  window)

#+linux
(defmethod alloy:render :around ((window om-look-window) (renderer om-look-window))
  (if (and (eq :x11 (%glfw:platform))
           (alld:window-registered-p window))
      (let ((framebuffer-size (%glfw:framebuffer-size window)))
        (%sync-x11-render-geometry window framebuffer-size)
        ;; a size the server reaches while this frame is drawn belongs to the next frame
        (let ((*x11-render-window* window)
              (*x11-render-framebuffer-size* framebuffer-size))
          (call-next-method)))
      (call-next-method)))

(defmethod (setf alloy:clipboard) ((text string) (window om-look-window))
  (setf (%glfw:clipboard-string window) text))

(defun %resolve-window-bg-color (bg-color)
  (cond ((null bg-color) alld:*default-window-background*)
        ((typep bg-color 'omcolor) (c bg-color))
        (t bg-color)))

;; keep the native surface background (alloy-desktop native/native.lisp:
;; set-native-window-background) in the window's colour
(defmethod om-set-bg-color :after ((self om-look-window) color)
  (alld:set-native-window-background self (%resolve-window-bg-color color)))

;;; om-abstract-window — shared window base (vanilla om-abstract-window):
;;; hosts the OM-level resizable/fs slots. om-look-window supplies the GLFW mixin.

;; vanilla name-parity class; OM window slots now live on om-look-window (above).
(defclass om-abstract-window (om-look-window) ())

;;; om-window — base concrete window class.

(defclass om-window (om-abstract-window) ())

;;; Dialog hierarchy

(defclass om-dialog (om-abstract-window) ())

(defclass om-textured-dialog (om-dialog) ())

(defclass om-windoid (om-window)
  ((accept-key-evt :initarg :accept-key-evt :initform nil :accessor accept-key-evt)))

(defclass om-no-border-win (om-window) ())

(defgeneric window-dialog-p (window)
  (:documentation "T if WINDOW is an om-dialog subclass."))

(defmethod window-dialog-p ((self t)) nil)
(defmethod window-dialog-p ((self om-dialog)) t)

(defgeneric make-window-layout (window &optional color)
  (:documentation "Build WINDOW's class-specific content at creation time (om-LW/window.lisp: the make-window-layout call of om-make-window)."))

(defmethod make-window-layout ((window t) &optional color)
  (declare (ignore color))
  nil)

;;; om-make-window — preserves upstream LW kwargs surface.

(defun om-make-window (&rest args)
  (multiple-value-bind (class kwargs)
      (if (and args (symbolp (car args)) (not (keywordp (car args))))
          (values (car args) (cdr args))
          (values 'om-window args))
    ;; GLFW windows are main-thread-only; alld:call-on-ui-thread inlines when already there
    (alld:call-on-ui-thread (lambda () (apply #'%om-make-window-impl class kwargs)))))

(defun %class-decorated-default (class)
  "NIL (borderless) only for no-border-win (vanilla :borderless); windoid keeps a title bar (vanilla :toolbox)."
  (not (subtypep class 'om-no-border-win)))

(defun %class-topmost-default (class)
  "T (always-on-top) for windoid classes; NIL otherwise (vanilla :always-on-top)."
  (and (subtypep class 'om-windoid) t))

(defun %om-apply-window-icon (window)
  "Set WINDOW's title-bar/taskbar icon to the OM logo (alloy-desktop
graphics/image.lisp: set-window-icon-from-picture). The logo is only looked up
where the window system shows a per-window icon."
  (when (alld:window-icon-supported-p)
    (let ((pict (%om-window-icon-picture)))
      (when pict
        (alld:set-window-icon-from-picture window pict)))))

#+darwin
(defun %om-msaa-safe-samples (samples x y w h)
  "SAMPLES, or 0 for a W x H content area at X/Y whose framebuffer size is one the
Darwin multisample resolve fills with memory that is not the surface's."
  ;; Framebuffer pixels, not window pixels: the resolve target is padded to a multiple
  ;; of 128 wide and a power of two high, and two padded sizes hit the bad 64 KiB.
  (let* ((monitor (alld::%popup-monitor-for-screen-point x y))
         (scale   (or (and monitor
                           (handler-case (first (%glfw:content-scale monitor))
                             (alld::%glfw-unavailable () nil)))
                      1))
         (fw (round (* w scale)))
         (fh (round (* h scale))))
    (if (or (and (<= 129 fw 256) (<= 33 fh 64))
            (and (<= 385 fw 512) (<= 17 fh 32)))
        0
        samples)))

(defun %om-make-window-impl (class &rest attrs
                             &key (screen (alld:desktop-screen))
                                  (size *default-window-size*)
                                  min-size
                                  position
                                  name
                                  window-title
                                  owner
                                  font
                                  bg-color
                                  (window-show t)
                                  subviews
                                  (resizable t)
                                  (close t)
                                  (minimize t)
                                  (maximize t)
                                  (topmost (%class-topmost-default class))
                                  ;; Default framebuffer sample count; a class that must not be
                                  ;; multisampled overrides it (menu.lisp: %popup-make-surface).
                                  (samples 4)
                                  (toolbox nil)
                                  menu-items
                                  (decorated-p (%class-decorated-default class))
                                  (palette :light)
                             &allow-other-keys)
  (declare (ignorable close maximize))
  (unless screen
    (error "OM-MAKE-WINDOW called outside OM-WITH-SCREEN: no desktop screen is active."))
  (let* ((effective-bg (%resolve-window-bg-color bg-color))
         ;; vanilla (om-LW/window.lisp: binding title of om-make-window): (title (or window-title name "OM Window"))
         (title  (or window-title name "OM Window"))
         ;; Windows never honors iconified-sentinel geometry (0x0 / -32000) saved in
         ;; wsparams; degenerate sizes fall back to the default, as under CAPI.
         (w      (let ((w0 (%coerce-px-w size)))
                   (if (plusp w0) w0 (%coerce-px-w *default-window-size*))))
         (h      (let ((h0 (%coerce-px-h size)))
                   (if (plusp h0) h0 (%coerce-px-h *default-window-size*))))
         ;; vanilla (om-LW/window.lisp: om-make-window): default position (200,200);
         ;; :centered centers on the owner (winparent) when given, else on the screen.
         (bounds (multiple-value-bind (sw sh) (alld:screen-size)
                   (let* ((base (and owner
                                     (typep owner 'om-look-window)
                                     (om-window-open-p owner)
                                     owner))
                          (bx (if base (om-point-h (om-view-position base)) 0))
                          (by (if base (om-point-v (om-view-position base)) 0))
                          (bw (if base (om-point-h (om-view-size base)) sw))
                          (bh (if base (om-point-v (om-view-size base)) sh))
                          (px (cond ((eql position :centered) (+ bx (round (- bw w) 2)))
                                    (position (let ((v (%coerce-px-w position)))
                                                (if (<= v -30000) 200 v)))
                                    (t 200)))
                          (py (cond ((eql position :centered) (+ by (round (- bh h) 2)))
                                    (position (let ((v (%coerce-px-h position)))
                                                (if (<= v -30000) 200 v)))
                                    (t 200))))
                     (alloy:px-extent px py w h))))
         #+darwin
         (samples (%om-msaa-safe-samples samples (alloy:pxx bounds) (alloy:pxy bounds) w h))
         ;; class-specific initargs (e.g. ompref-window :local-prefs) reach the
         ;; instance the way vanilla om-make-window does: forwarded to make-instance
         ;; (windowing:make-window remf*'s its own keys, passing the rest through)
         (extra-initargs (loop for (k v) on attrs by #'cddr
                               unless (or
                                       (member k '(:screen :size :min-size :position :name
                                                   :window-title :owner :font :bg-color
                                                   :window-show :subviews :resizable :close
                                                   :minimize :maximize :topmost :toolbox
                                                   :samples
                                                   :menu-items :decorated-p :palette
                                                   :initial-constraints))
                                       (cl-user::%capi-geometric-hint-key-p k))
                                 append (list k v)))
         ;; the hidden window, its chrome, a Y-down root and the content focus list,
         ;; placed and sized before the first poll, registered (alloy-desktop
         ;; window/assembly.lisp: make-window)
         (window (multiple-value-bind (window fx fy)
                     (alld:make-window
                      screen
                      :class class
                      :title title
                      :x (alloy:pxx bounds) :y (alloy:pxy bounds)
                      :width (alloy:pxw bounds) :height (alloy:pxh bounds)
                      :background-color effective-bg
                      :decorated-p decorated-p
                      :always-on-top-p topmost
                      :resizable-p resizable
                      :palette palette
                      ;; LispWorks GP rasterizes narrow/self-intersecting
                      ;; polygons with coverage antialiasing.  Request an MSAA
                      ;; default framebuffer so the OpenGL bridge preserves
                      ;; that contract (most visibly in sound waveforms).
                      :native-hints (list :samples samples)
                      :class-initargs extra-initargs
                      ;; scaled below, once the layout and subviews exist
                      :initialize-content-scale-p nil)
                   (setf (vx window) fx (vy window) fy)
                   window)))
    (setf (resizable window) resizable)
    (when min-size
      (alld:set-window-min-size window (%coerce-px-w min-size) (%coerce-px-h min-size)))
    ;; OM-MAKE-WINDOW is also a public creation path for CAPI-compatible
    ;; interfaces.  Preserve its explicit owner immediately, so owner teardown
    ;; reaches hidden children even if CAPI:DISPLAY is never called for them.
    (when owner
      (%capi-set-interface-owner window owner))
    (cl-user::%capi-initialize-hints window attrs)
    ;; realization point: vanilla's window om-create-callback sets initialized-p
    (when (typep window 'om-graphic-object)
      (setf (initialized-p window) t))
    (let ((dc (getf attrs :destroy-callback)))
      (when dc (setf (window-destroy-callback window) dc)))
    (setf (window-name window) name)
    (setf (window-instance-menu-items window) menu-items)
    ;; vanilla (om-LW/window.lisp: om-make-window) passes :font to the INTERFACE; CAPI does not
    ;; inherit it into the panes, which carry their own fonts.
    (when font (om-set-font window font))
    (make-window-layout window effective-bg)
    ;; vanilla (om-LW/window.lisp: the om-add-subviews loop of om-make-window): subviews enter after the layout exists
    (when subviews (apply #'om-add-subviews window subviews))
    (when (or (not resizable) (window-dialog-p window))
      (set-not-resizable window w h))
    (%apply-minimum-size window)
    (alloy:mark-for-render window)
    (let ((scale (alld:window-content-scale window)))
      (when scale
        (alld:note-content-scale scale)
        (alld:apply-window-content-scale window scale)))
    (alloy:render window window)
    (%om-apply-window-icon window)
    ;; vanilla (om-LW/window.lisp: om-make-window): :never-iconic (:minimize nil) and
    ;; :toolbox (palette, no min/max buttons); decorated-p guards borderless popups.
    (when (alld::%window-decorated-p window)
      (cond ((subtypep class 'om-windoid)
             (%apply-native-window-style window :minimize nil :maximize nil))
            ((or toolbox (not minimize))
             (%apply-native-window-style window
                                         :minimize (and minimize (not toolbox) t)
                                         :maximize (not toolbox)))))
    ;; vanilla (om-LW/window.lisp: the window-dialog-p guard of om-make-window): dialogs are displayed by om-modal-dialog, not here
    (when (and window-show (not (window-dialog-p window)))
      (om-show-window window))
    ;; vanilla (om-LW/window.lisp: the final om-set-view-position of om-make-window) re-applies the requested position after the
    ;; display; on a backend that publishes the frame only once mapped this is where
    ;; the conversion above becomes exact.
    (om-set-view-position window (om-make-point (vx window) (vy window)))
    window))

;;; Window state accessors

(defun om-window-root (window)
  (alld:window-root-layout window))

(defun om-window-focus (window)
  (alld:window-focus-list window))

(defmethod alld:window-focus-root ((window om-look-window))
  (om-window-focus window))

(defun om-window-chrome (window)
  (alld:window-chrome window))

(defun %om-pane-window (window pane)
  "WINDOW when given, else PANE's own -- asked only of a layout-element: a bare
focus-list (a window's focus root) has no om-view-window method."
  (or window
      (and (typep pane 'alloy:layout-element) (om-view-window pane))))

(defmethod update-for-subviews-changes
    ((window om-look-window) &optional (recursive nil))
  "Reconcile a window's OM child graph with its live Alloy root layout."
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread
       (lambda () (update-for-subviews-changes window recursive)))
      (let ((root (om-window-root window)))
        (when root (alloy:refit root))
        (when recursive
          (dolist (view (vsubviews window))
            (when (om-view-p view)
              (update-for-subviews-changes view t))))
        (alloy:mark-for-render window)
        window)))

(defgeneric %map-shown-subviews (function view)
  (:documentation "Call FUNCTION on VIEW and on everything VIEW currently SHOWS.
The subview tree, not the focus tree: a tab panel keeps its children in a focus-list
of its own (view.lisp: %panel-focus-list), so walking the window focus reaches the
window's own controls and nothing inside a tab.")
  (:method (function (view t))
    (funcall function view))
  (:method (function (view om-graphic-object))
    (funcall function view)
    (dolist (child (vsubviews view))
      (%map-shown-subviews function child))))

;;; Focus tracking + front window resolution

;; last step of every focus change (alloy-desktop window/assembly.lisp: glfw
;; window-focused :after for window)
(defmethod alld:window-activated ((window om-look-window) active-p)
  ;; vanilla's om-activate-callback fires om-window-activate on activate AND deactivate
  (om-window-activate window active-p))

(defun om-front-window ()
  (alld:front-window))

(defun om-get-all-windows (&optional (class t))
  (remove-if-not (lambda (window) (typep window class))
                 (alld:registered-windows)))

(defmethod interface-match-p ((self om-look-window) &rest initargs &key name)
  (declare (ignore initargs))
  (and name (window-name self) (string-equal (window-name self) name)))

(defun om-window-open-p (window)
  (alld:window-registered-p window))

(defun om-window-visible-p (window)
  (alld:window-visible-p window))

(defun %capi-interface-class (proto)
  "Resolve the CAPI COLLECT-INTERFACES class designator PROTO."
  (cond ((symbolp proto)
         (or (find-class proto nil)
             (error "~S does not name an interface class." proto)))
        ((typep proto 'class) proto)
        ((typep proto 'capi:interface) (class-of proto))
        (t (error "~S is not a CAPI interface class designator." proto))))

(defun %capi-screen-matches-p (screen)
  "The GLFW port owns one screen/process; recognize CAPI's screen designators."
  (or (null screen)
      (eq screen :any)
      (eq screen (alld:desktop-screen))
      #+win32 (eq screen :win32)
      #+darwin (eq screen :cocoa)
      #+linux (eq screen :motif)))

(defun %capi-remove-initarg (plist key)
  (loop for (name value) on plist by #'cddr
        unless (eq name key)
          append (list name value)))

(defun %capi-interface-window (object)
  (cond ((typep object 'om-look-window) object)
        ((typep object 'om-graphic-object)
         (let ((window (om-view-window object)))
           (and (typep window 'om-look-window) window)))
        (t nil)))

(defmethod capi:interface-owner ((interface om-look-window))
  (alld:window-owner interface))

(defmethod (setf capi:interface-owner) (owner (interface om-look-window))
  (setf (alld:window-owner interface) owner))

(defun %capi-set-interface-owner (interface owner)
  (let ((owner-window (%capi-interface-window owner)))
    (when (and owner (null owner-window))
      (error "CAPI interface owner ~S has no live OM top-level interface." owner))
    (setf (capi:interface-owner interface) owner-window)))

(defun %capi-locate-interface-now
    (class-spec initargs screen no-busy-interface)
  (declare (ignore no-busy-interface))
  (when (%capi-screen-matches-p screen)
    (let ((class (%capi-interface-class class-spec))
          (clean-initargs
            (%capi-remove-initarg (%capi-remove-initarg initargs :screen)
                                  :no-busy-interface)))
      (or (alld:find-window class
                            :test (lambda (interface)
                                    (apply #'capi:interface-match-p
                                           interface clean-initargs)))
          (when (capi:reuse-interfaces-p)
            (let ((reusable
                    (alld:find-window class
                                      :test (lambda (interface)
                                              (apply #'capi:interface-reuse-p
                                                     interface clean-initargs)))))
              (when reusable
                (apply #'capi:reinitialize-interface reusable clean-initargs)
                reusable)))))))

(defun %capi-make-interface (class-spec initargs screen)
  (let* ((class (%capi-interface-class class-spec))
         (class-name (class-name class))
         (without-screen (%capi-remove-initarg initargs :screen))
         (creation-args (if screen
                            (append without-screen (list :screen screen))
                            without-screen)))
    (unless (subtypep class-name 'om-look-window)
      (error "CAPI interface class ~S has no OM/GLFW window implementation."
             class-name))
    (apply #'om-make-window class-name creation-args)))

(defun %capi-find-interface-now (class-spec initargs screen)
  (let ((interface
          (%capi-locate-interface-now class-spec initargs screen nil)))
    (if interface
        (om-select-window interface)
        (let* ((created (%capi-make-interface class-spec initargs screen))
               (owner (getf initargs :owner))
               (styles (getf initargs :window-styles))
               (process (getf initargs :process)))
          (capi:display created :screen screen :owner owner
                       :window-styles styles :process process)))))

(defun %capi-display-interface
    (interface &key screen owner window-styles process)
  (unless (typep interface 'om-look-window)
    (error "~S is not an OM/GLFW CAPI interface." interface))
  (unless (%capi-screen-matches-p screen)
    (error "CAPI screen ~S is not the active OM screen." screen))
  (when (and process
             (not (eq process t))
             (not (eq process (alld:ui-thread))))
    (error "CAPI process ~S does not own the OM GLFW event loop." process))
  (let ((display-now
          (lambda ()
            (%capi-set-interface-owner interface owner)
            (setf (capi::%interface-screen interface)
                  (or screen (alld:desktop-screen))
                  (capi::%interface-process interface) (alld:ui-thread)
                  (capi::%interface-window-styles interface) window-styles)
            (capi:interface-display interface)
            (om-select-window interface)
            interface)))
    (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
        (alld:call-on-ui-thread display-now)
        (funcall display-now))))

(defun %capi-collect-interfaces
    (proto &key screen (current-process-first t) (sort-by :create))
  "All live OM/GLFW interfaces satisfying CAPI:COLLECT-INTERFACES."
  (unless (member sort-by '(:create :visible))
    (error "CAPI:COLLECT-INTERFACES :SORT-BY must be :CREATE or :VISIBLE, not ~S."
           sort-by))
  (unless (typep current-process-first 'boolean)
    (error "CAPI:COLLECT-INTERFACES :CURRENT-PROCESS-FIRST must be boolean."))
  ;; There is only one Lisp process and one GLFW screen, so process grouping is
  ;; already satisfied after validating CURRENT-PROCESS-FIRST.
  (when (%capi-screen-matches-p screen)
    (let ((class (%capi-interface-class proto)))
      (alld:call-on-ui-thread
       (lambda ()
         (remove-if-not (lambda (window) (typep window class))
                        (alld:registered-windows :sort-by sort-by)))))))

;;; Universal funeral

(defun %capi-destroy-interface (interface)
  "Destroy INTERFACE at once without consulting its veto, running its destroy callback as CAPI:DESTROY does."
  (unless (typep interface 'om-look-window)
    (error "~S is not an OM/GLFW CAPI interface." interface))
  (alld:call-on-ui-thread
   (lambda ()
     ;; deallocating runs the funeral -- owned windows, closing hook, retirement
     ;; (alloy-desktop/window/close.lisp: alloy:deallocate :before for window);
     ;; a window whose native handle is already gone only leaves the registry
     (if (alld:window-alive-p interface)
         (progn (alloy:deallocate interface) t)
         (alld:close-window interface :force t)))))

(defun %capi-quit-interface (pane force)
  (let ((interface (%close-target-window pane)))
    (and interface
         (alld:call-on-ui-thread (lambda () (alld:close-window interface :force force))))))

(defun %detach-window-children (window)
  "Undo the OM->Alloy realization edges while WINDOW's root and focus trees are
still live; one child failing must not leave the rest attached."
  (unless (typep window 'om-look-window)
    (return-from %detach-window-children window))
  (dolist (child (copy-list (vsubviews window)))
    (handler-case
        (progn (%retire-view-subtree-resources child)
               (internal-remove-subview window child))
      (serious-condition (c)
        (format *error-output* "[om-window-funeral] detach ~A from ~A: ~A~%"
                (type-of child) (type-of window) (princ-to-string c)))))
  (setf (vsubviews window) nil)
  window)

(alld:register-window-retirement :om-children
                                 #'%detach-window-children
                                 :order 50)

(defun %fire-window-destroy-event (window)
  "Run WINDOW's user :destroy-callback, or the default om-window-close-event.
Callable directly by an om-destroy-callback method that must NOT chain through
its own superclass method (vanilla om-LW/om-text-editor.lisp: om-destroy-callback for om-text-edit-window does the same)."
  (let ((dc (window-destroy-callback window)))
    (if dc
        (funcall dc window)
        (om-window-close-event window))))

(defmethod om-destroy-callback ((self om-look-window))
  ;; a user :destroy-callback replaces the default close-event (vanilla CAPI semantics)
  (%fire-window-destroy-event self))

;; vanilla wires the veto via :confirm-destroy-function, which CAPI consults BEFORE
;; any destruction (om-LW/window.lisp: :confirm-destroy-function of om-abstract-window,
;; om-confirm-destroy-function, om-window-check-before-close). A print job draws
;; this window's document from another thread; tearing the window down under it
;; would free the tree being read, and disabling the window stops the pointer, not
;; the close a taskbar entry or Alt+F4 still sends -- so the request is refused and
;; the user closes the window once the job has ended.
(defmethod alld:window-close-allowed-p ((window om-look-window))
  (and (not (%window-frozen-p window))
       (om-window-check-before-close window)))

;; vanilla fires the destroy callback while the interface is still alive
;; (om-LW/window.lisp: om-destroy-callback for om-abstract-window); the retirement
;; chain runs after it, because the callback still reads the subviews.
(defmethod alld:window-closing ((window om-look-window))
  (setf (initialized-p window) nil)
  (om-destroy-callback window))

(defmethod alld:window-closed ((window om-look-window))
  (setf (initialized-p window) nil))

;; vanilla takes any PANE and lets capi:quit-interface walk up to its interface
;; (om-LW/window.lisp: om-close-window for t); the kernel closes attached EditorViews, not windows
;; (kernel editorwindow.lisp: om-window-close-event :around for EditorWindow). om-view-window returns SELF when unrealized -> eq guard.
(defun %close-target-window (obj &optional (depth 8))
  (cond ((or (null obj) (typep obj 'om-look-window)) obj)
        ((and (plusp depth) (typep obj 'om-graphic-object))
         (let ((up (om-view-window obj)))
           (unless (eq up obj) (%close-target-window up (1- depth)))))
        (t nil)))

(defmethod om-close-window ((window t))
  (let ((window (%close-target-window window)))
    (when window
      (alld:close-window window))))

(defmethod om-window-close-event ((self t)) nil)

(defmethod om-window-check-before-close ((self t)) t)

(defmethod om-help-callback ((interface t) pane type key)
  (declare (ignore pane type key))
  nil)

;; vanilla om-window/correct-win-h added +200px to compensate LW window chrome;
;; GLFW has no such chrome, so this is a no-op returning t (symbol kept for parity).
(defmethod correct-win-h ((win t)) nil)
(defmethod correct-win-h ((win om-window)) t)

(defgeneric %install-window-gestures (window)
  (:documentation "Hook run once WINDOW is on screen, for the platform gesture handlers
the toolkit does not deliver by itself. NIL everywhere but macOS, where
actions.lisp: %install-window-gestures for om-look-window routes the pinch through
alloy-desktop native/gestures.lisp: install-magnify-handler.")
  (:method ((window t)) nil))

(defmethod om-show-window ((window om-look-window))
  (alld:show-window window))

(defmethod om-hide-window ((window om-look-window))
  (alld:hide-window window))

(defmethod om-select-window ((window om-look-window))
  ;; vanilla shows on select: capi:display (dialogs) / capi:find-interface
  ;; raises+displays any window (om-LW/window.lisp: om-select-window for om-abstract-window)
  (unless (om-window-visible-p window)
    (om-show-window window))
  (when (om-window-open-p window)
    (alld:raise-window window))
  window)

(defmethod om-window-activate ((self t) &optional (activatep t))
  (declare (ignore self activatep))
  t)

;;; Modal dialog: native Win32 EnableWindow disables siblings;
;;; Linux and macOS use the :mouse-passthrough attribute instead.

(defun %apply-native-window-style (window &key (minimize t) (maximize t))
  "Strip the minimize/maximize title-bar buttons from WINDOW as requested (keep
close), matching CAPI :never-iconic/:toolbox, via a per-OS native style edit
below GLFW. Cosmetic: a native failure is logged and does not abort creation."
  (handler-case
      (progn
        #+win32
        ;; GWL_STYLE=-16; clear WS_MINIMIZEBOX/WS_MAXIMIZEBOX, keep WS_SYSMENU; SWP flags #x37.
        (let ((hwnd  (alld::%window-hwnd window))
              (clear (logior (if minimize 0 #x00020000) (if maximize 0 #x00010000))))
          (when (and hwnd (not (cffi:null-pointer-p hwnd)) (plusp clear))
            (let* ((style (alld::%get-window-long-ptr hwnd -16))
                   (new   (logand style (lognot clear))))
              (alld::%set-window-long-ptr hwnd -16 new)
              (alld::%set-window-pos hwnd (cffi:null-pointer) 0 0 0 0 #x0037))))
        #+darwin
        ;; NSWindowStyleMaskMiniaturizable=4; clear it, keep Titled|Closable.
        (unless minimize
          (let ((nswin (alld::%glfw-get-cocoa-window (%glfw::pointer window))))
            (when (and nswin (not (cffi:null-pointer-p nswin)))
              (let* ((mask (cffi:foreign-funcall "objc_msgSend"
                                                 :pointer nswin
                                                 :pointer (alld::%sel-register-name "styleMask")
                                                 :unsigned-long))
                     (new  (logand mask (lognot 4))))
                (cffi:foreign-funcall "objc_msgSend"
                                      :pointer nswin
                                      :pointer (alld::%sel-register-name "setStyleMask:")
                                      :unsigned-long new :void)))))
        #+(or linux freebsd)
        ;; _MOTIF_WM_HINTS functions=MOVE|RESIZE|CLOSE(38) + MIN(8)/MAX(16) as kept.
        (when (eq :x11 (%glfw:platform))
         (let ((dpy (alld::%glfw-get-x11-display))
              (xid (alld::%glfw-get-x11-window (%glfw::pointer window))))
          (when (and dpy (not (cffi:null-pointer-p dpy)) (/= xid 0))
            (let ((atom (alld::%x-intern-atom dpy "_MOTIF_WM_HINTS" 0)))
              (cffi:with-foreign-object (hints :long 5)
                (setf (cffi:mem-aref hints :long 0) 1   ; MWM_HINTS_FUNCTIONS
                      (cffi:mem-aref hints :long 1) (logior 38 (if minimize 8 0)
                                                            (if maximize 16 0))
                      (cffi:mem-aref hints :long 2) 0
                      (cffi:mem-aref hints :long 3) 0
                      (cffi:mem-aref hints :long 4) 0)
                (alld::%x-change-property dpy xid atom atom 32 0 hints 5)
                (alld::%x-flush dpy)))))))
    ;; the per-OS symbol may not exist on this build (undefined-alien-error) and a
    ;; foreign call may reject the handle (type-error); both mean "no native style"
    ((or sb-alien:undefined-alien-error type-error) (c)
      (format *error-output* "[native-window-style] ~A~%" (princ-to-string c)))))

(defun %screen-window-list ()
  "Only windows that still have a native handle: a frame renders and makes a
context current, both of which reach the FFI."
  (loop for w being the hash-values of (windowing::windows (alld:desktop-screen))
        when (alld:window-alive-p w) collect w))

;;; ---- Native OS font roles: refresh and propagation ------------------------

(defun %om-mark-unrealized (element)
  "Mark ELEMENT and its descendants for re-realization, so the shapes are rebuilt
from the realization forms -- which is where a native role font is read.  The
re-realization itself happens on the next dirty render (alloy
renderers/simple/presentations/protocol.lisp: prepare-for-render :before)."
  (when (typep element 'alloy:renderable)
    (setf (presentations::realized-p element) nil)
    (alloy:mark-for-render element))
  (when (typep element 'alloy:layout)
    (alloy:do-elements (child element) (%om-mark-unrealized child))))

(defun %om-native-fonts-repaint ()
  "Re-dress every live window after the native roles changed.  One window that
cannot be walked must not stop the others."
  (alld:invalidate-native-fonts)
  (dolist (window (om-get-all-windows))
    (let ((window window))
      (alld:call-guarded-on-window
       window
       (lambda ()
         (let ((tree (alloy:layout-tree window)))
           (when tree
             (%om-mark-unrealized (alloy:root tree))
             (%om-mark-unrealized (alloy:popups tree)))))))))

(defun %om-native-fonts-refresh ()
  "Re-read the native font roles now and re-dress the live windows if they moved.
Called only where the display configuration is known to have changed (the screen
opening, a content-scale change): one re-read is ~290 ms of GDI calls, so it is
never polled from the frame loop."
  (when (alld:refresh-native-fonts)
    (%om-native-fonts-repaint)))

(defun %frame-windows-guarded (windows-fn dt)
  "Run one desktop frame (alloy-desktop core/event-pump.lisp: frame-once) with the
OM UI-task runner: the frame of the modal loop, the same one om-run-loop runs. A
window frozen by a print job gets neither animation nor rendering (window.lisp:
%window-frozen-p through alld:window-render-suspended-p), and a failure in one
window must not stop the others."
  (alld:frame-once windows-fn dt :ui-task-runner #'%run-ui-task))

(defun %modal-owner-window (owner dialog)
  (let ((window (cond ((typep owner 'om-look-window) owner)
                      ((typep owner 'om-graphic-object) (om-view-window owner))
                      (t nil))))
    (and window (not (eq window dialog)) (om-window-open-p window) window)))

(defun %run-modal-dialog (dialog owner &key focus timeout dismiss-on-input-p)
  "Run DIALOG through ALLOY-DESKTOP's modal facility and return CAPI RESULT, OKP."
  (alld:run-modal-dialog
   dialog
   :owner owner
   :focus focus
   :focus-function (lambda (pane) (capi:set-pane-focus pane))
   :timeout timeout
   :dismiss-on-input-p dismiss-on-input-p
   ;; These are deliberately narrow OM policy seams from the pre-migration loop.
   ;; ALLOY-DESKTOP owns the iteration order and modal state/result/cleanup.
   :before-iteration #'alld:mask-float-traps
   :reap-before-poll #'alld:reap-dead-windows
   :after-poll #'alld:popup-resolve-focus-loss
   :windows-function #'%screen-window-list
   :frame-function (lambda (dt)
                     (%frame-windows-guarded #'%screen-window-list dt))
   :iteration-guard #'alld:call-guarded-iteration))

(defun %om-modal-dialog (dialog owner &key focus)
  "om-modal-dialog with FOCUS, the pane that takes the input focus once DIALOG is shown.
An explicit OWNER centres DIALOG on it; without one DIALOG keeps its position and the
front window only owns it."
  (let ((window (%modal-owner-window owner dialog)))
    (if window
        (progn
          (alld:place-window-relative-to dialog window nil nil)
          (%run-modal-dialog dialog window :focus focus))
        (%run-modal-dialog dialog
                           (%modal-owner-window (om-front-window) dialog)
                           :focus focus))))

(defun om-modal-dialog (dialog &optional owner)
  (%om-modal-dialog dialog owner))

(defun om-return-from-modal-dialog (dialog value)
  (if (eq dialog (alld:current-modal-window))
      (alld:exit-modal value)
      (warn "om-return-from-modal-dialog called outside its modal context")))

(defun %position-capi-dialog (dialog owner x y position-relative-to)
  (unless (member position-relative-to '(:owner nil))
    (error "CAPI:DISPLAY-DIALOG :POSITION-RELATIVE-TO must be :OWNER or NIL."))
  (alld:place-window-relative-to
   dialog (if (and (eq position-relative-to :owner) owner) owner :screen) x y))

(defun %call-capi-continuation (continuation callback-error-handler result okp)
  (if callback-error-handler
      (handler-case (funcall continuation result okp)
        (error (condition) (funcall callback-error-handler condition)))
      (funcall continuation result okp)))

(defun %capi-display-dialog
    (interface &key screen focus (modal t) timeout owner x y
                    (position-relative-to :owner) continuation
                    callback-error-handler)
  "Alloy/GLFW implementation of the CAPI DISPLAY-DIALOG contract used by OM."
  (unless (typep interface 'capi:interface)
    (error "~S is not a CAPI interface." interface))
  (unless (%capi-screen-matches-p screen)
    (error "CAPI screen ~S is not the active OM screen." screen))
  (unless (member modal '(t nil :dismiss-on-input))
    (error "CAPI:DISPLAY-DIALOG :MODAL must be T, NIL, or :DISMISS-ON-INPUT."))
  (unless (or (null timeout) (and (realp timeout) (not (minusp timeout))))
    (error "CAPI:DISPLAY-DIALOG :TIMEOUT must be NIL or a non-negative real."))
  (let* ((owner-window (or (%modal-owner-window owner interface)
                           (%modal-owner-window (om-front-window) interface)))
         (invoke
           (lambda ()
             (%position-capi-dialog interface owner-window x y position-relative-to)
             (if modal
                 (%run-modal-dialog
                  interface owner-window :focus focus :timeout timeout
                  :dismiss-on-input-p (eq modal :dismiss-on-input))
                 (progn
                   (%capi-set-interface-owner interface owner-window)
                   (om-show-window interface)
                   (when focus (capi:set-pane-focus focus))
                   (values interface t))))))
    (if continuation
        (progn
          (alld:post-to-ui-thread
           (lambda ()
             (multiple-value-bind (result okp) (funcall invoke)
               (%call-capi-continuation continuation callback-error-handler
                                        result okp))))
          (values :continuation nil))
        (funcall invoke))))

;;; Title + geometry getters/setters

(defmethod om-window-title ((window om-look-window))
  (windowing:title window))

(defmethod om-set-window-title ((window om-look-window) title)
  (setf (windowing:title window) title))

(defmethod om-view-size ((window om-look-window))
  (let ((bounds (alld:window-bounds window)))
    (om-make-point (round (alloy:pxw bounds)) (round (alloy:pxh bounds)))))

(defun %apply-capi-window-hints (window)
  "Publish all four CAPI size limits to GLFW atomically.

A single write avoids the transient min>max state that GLFW rejects when a
fixed or newly contradictory constraint replaces an older pair."
  (multiple-value-bind (min-width max-width min-width-p max-width-p)
      (cl-user::%capi-axis-limits window :width)
    (multiple-value-bind (min-height max-height min-height-p max-height-p)
        (cl-user::%capi-axis-limits window :height)
      (alld:set-window-size-limits window
                               (and min-width-p min-width)
                               (and min-height-p min-height)
                               (and max-width-p max-width)
                               (and max-height-p max-height))))
  window)

(defmethod set-not-resizable ((window t) &optional w h)
  "Lock WINDOW to a fixed size: the four GLFW limits in ONE write, as vanilla's single
set-hint-table (om-LW/window.lisp: set-not-resizable for t) -- a split write passes through min>max."
  (let ((w (round (or w (alloy:pxw (alloy:bounds window)))))
        (h (round (or h (alloy:pxh (alloy:bounds window))))))
    (alld:set-window-size-limits window w h w h))
  window)

(defmethod om-set-view-size ((window om-look-window) pt)
  (let* ((bounds (alld:window-bounds window))
         (requested-w (or (%coerce-px-w pt) (alloy:pxw bounds)))
         (requested-h (or (%coerce-px-h pt) (alloy:pxh bounds))))
    (multiple-value-bind (w h)
        (cl-user::%capi-constrain-element-size
         window requested-w requested-h)
      ;; vanilla (om-LW/window.lisp: om-set-view-size for om-abstract-window) re-pins BEFORE the geometry: glfwSetWindowSize
      ;; is clamped by the limits still in force, so the old pin would win.
      (unless (resizable window)
        (set-not-resizable window w h))
      (multiple-value-bind (actual-w actual-h)
          (if (eq :x11 (%glfw:platform))
              ;; X11 applies the resize asynchronously: install the requested
              ;; geometry now and let the native resize callback adjust it
              (multiple-value-bind (aw ah) (alld:set-window-size window w h)
                (setf (vw window) aw (vh window) ah)
                (values aw ah))
              (progn
                (setf (alloy:bounds window)
                      (alloy:px-extent (alloy:pxx bounds) (alloy:pxy bounds) w h))
                (setf (vw window) w (vh window) h)
                (let ((root (om-window-root window)))
                  (when root (alloy:refit root)))
                (values w h)))
        (om-window-resized window (om-make-point actual-w actual-h))
        (om-make-point actual-w actual-h)))))

(defmethod om-set-view-size ((self om-windoid) pt)
  (set-not-resizable self (%coerce-px-w pt) (%coerce-px-h pt))
  (call-next-method))

(defmethod om-view-position ((window om-look-window))
  (let ((bounds (alld:window-bounds window)))
    (multiple-value-bind (fx fy)
        (alld:window-content-to-frame window (alloy:pxx bounds) (alloy:pxy bounds))
      (om-make-point (round fx) (round fy)))))

(defmethod om-set-view-position ((window om-look-window) pt)
  (let ((bounds (alld:window-bounds window)))
    (multiple-value-bind (fx fy cx cy)
        (alld:place-window-frame window (%pt-x pt) (%pt-y pt)
                             (alloy:pxw bounds) (alloy:pxh bounds))
      (let ((extent (alloy:px-extent cx cy (alloy:pxw bounds) (alloy:pxh bounds))))
        (setf (alloy:bounds window) extent)
        ;; only the async glfw:window-moved callback refreshes the bounds SLOT
        ;; (alloy renderers/glfw/windowing.lisp: glfw:window-moved for window); write it here so the read-back in
        ;; the same turn -- how the kernel saves the .omp position
        ;; (kernel editorwindow.lisp: close-editor-before for EditorView) -- sees the position just applied.
        (setf (slot-value window 'alloy:bounds) extent))
      (setf (vx window) fx (vy window) fy)
      (om-make-point fx fy))))

;;; Interior size: the client layout below the menubar (vanilla: pane-layout size).

(defmethod om-interior-size ((window om-look-window))
  (let ((root (om-window-root window)))
    (if (typep root 'alloy:layout-element)
        (let ((b (alloy:bounds root)))
          (om-make-point (round (alloy:pxw b)) (round (alloy:pxh b))))
        (om-view-size window))))

(defmethod om-set-interior-size ((window om-look-window) size)
  (om-set-view-size window size))

;;; Resize callback hook + emitted event for kernel handlers.

(defun %apply-minimum-size (window)
  "Push (om-minimum-size window), when non-nil, into the live GLFW min-size constraint."
  (let ((msize (om-minimum-size window)))
    (when msize
      (alld:set-window-min-size window (om-point-h msize) (om-point-v msize)))))

(defmethod om-resize-callback ((window om-look-window) x y w h)
  (declare (ignore x y))
  (unless (and (window-last-w window) (= w (window-last-w window))
               (window-last-h window) (= h (window-last-h window)))
    (setf (window-last-w window) w (window-last-h window) h)
    (%apply-minimum-size window)
    (om-window-resized window (om-make-point w h))))

(defmethod om-window-resized ((window t) size)
  (declare (ignore window size))
  nil)

;;; Maximize / fullscreen / screen size.

(defmethod om-window-maximized ((window om-look-window))
  (alld:window-maximized-p window))

(defmethod om-maximize-window ((window om-look-window))
  (alld:maximize-window window))

(defun om-screen-size ()
  (multiple-value-bind (w h) (alld:screen-size)
    (om-make-point w h)))

(defmethod om-fullscreen-window ((window om-look-window))
  (setf (fs window) t)
  (multiple-value-bind (sw sh) (alld:screen-size)
    (om-set-view-position window (om-make-point 0 0))
    (om-set-view-size window (om-make-point sw (- sh 20))))
  window)

(defmethod om-minimum-size ((window t))
  (declare (ignore window))
  nil)

;;; GLFW window ops called off the UI thread (kernel eval-thread paths) marshal to
;;; it. The ALLOY-DESKTOP UI-owner nil / alld:on-ui-thread-p guard runs inline during load and on
;;; the UI thread, so the alld:call-on-ui-thread re-entry cannot loop.

(defmethod om-select-window :around ((window om-look-window))
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-select-window window)))
      (call-next-method)))

(defmethod om-close-window :around ((window om-look-window))
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-close-window window)))
      (call-next-method)))

(defmethod om-set-window-title :around ((window om-look-window) title)
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-set-window-title window title)))
      (call-next-method)))

(defmethod om-set-view-position :around ((window om-look-window) pt)
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-set-view-position window pt)))
      (call-next-method)))

(defmethod om-set-view-size :around ((window om-look-window) pt)
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-set-view-size window pt)))
      (call-next-method)))

(defmethod om-maximize-window :around ((window om-look-window))
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-maximize-window window)))
      (call-next-method)))

(defmethod om-fullscreen-window :around ((window om-look-window))
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-fullscreen-window window)))
      (call-next-method)))

(defmethod om-show-window :around ((window om-look-window))
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-show-window window)))
      (call-next-method)))

(defmethod om-hide-window :around ((window om-look-window))
  (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
      (alld:call-on-ui-thread (lambda () (om-hide-window window)))
      (call-next-method)))

(defun om-run-loop ()
  "Block on GLFW event loop, rendering all open OM windows ~60 Hz, until every window is closed or close-requested."
  ;; CAPI parity: an error in a GLFW callback, window close or render aborts THAT
  ;; iteration only -- the interface event loop keeps running, as LispWorks never
  ;; crashes the application on a single callback error.
  (let ((*gui-startup* nil))
    (alld:run-event-loop (alld:desktop-screen) :ui-task-runner #'%run-ui-task)))

