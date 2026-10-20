(in-package :alloy-desktop)

;;; An assembled desktop window: the hidden native window, its chrome border
;;; layout, a Y-down content root at the chrome's centre and the content focus
;;; list, registered with a window-state that carries all three.

(defun window-root-layout (window)
  "The content root layout of the registered WINDOW, or NIL."
  (let ((state (registered-window-state window)))
    (and state (%state-root-layout state))))

(defun window-focus-list (window)
  "The content focus list of the registered WINDOW, or NIL."
  (let ((state (registered-window-state window)))
    (and state (%state-focus-list state))))

(defun window-chrome (window)
  "The chrome border layout of the registered WINDOW, or NIL. Menu bars and
other window chrome go on its edges; the content root sits at its centre."
  (let ((state (registered-window-state window)))
    (and state (%state-chrome state))))

(defun make-window (screen &key (class 'window)
                                (title windowing:*default-window-title*)
                                (x 200) (y 200) (width 800) (height 600)
                                (resizable-p t) (decorated-p t) always-on-top-p
                                (background-color *default-window-background*)
                                (palette :light)
                                (share (share-window)) native-hints class-initargs
                                (state (make-instance 'window-state))
                                (initialize-content-scale-p t))
  "Create a hidden desktop window of CLASS on SCREEN whose external frame has its
top-left at X/Y and whose content is WIDTH x HEIGHT, assemble it and register it
with STATE. Return the window and the frame origin actually used, as three values.

The window gets a chrome border layout, a FIXED-LAYOUT content root at the
chrome's centre and a content focus list (WINDOW-CHROME, WINDOW-ROOT-LAYOUT,
WINDOW-FOCUS-LIST). PALETTE is :LIGHT (DESKTOP-LIGHT), :DARK or a palette name.
SHARE, NATIVE-HINTS and CLASS-INITARGS are those of MAKE-NATIVE-WINDOW; SHARE
defaults to the session's share partner (SHARE-WINDOW), so every window draws
with the textures and font atlases the others uploaded.

Bounds and projection are set here, before the window is shown or any event is
polled, so content added right away lays out at the real size; the size the OS
granted then replaces the one requested (a request above the display's maximum
is applied smaller).

With INITIALIZE-CONTENT-SCALE-P (the default) the window's OS content scale, or
1.0 when none can be read, is recorded (NOTE-CONTENT-SCALE) and applied
(APPLY-WINDOW-CONTENT-SCALE) once the window is registered; WINDOW-SCALE-CHANGED
is not called. A caller passing NIL applies the scale itself."
  (let* ((window (make-native-window
                  screen
                  :class class
                  :title title
                  :background-color background-color
                  :decorated-p decorated-p
                  :always-on-top-p always-on-top-p
                  :resizable-p resizable-p
                  ;; X11 applies a resize asynchronously, so there the native surface is
                  ;; created at the requested content size instead of the backend default
                  :bounds (if (eq :x11 (%glfw:platform))
                              (alloy:px-size width height)
                              windowing:*default-window-bounds*)
                  :state :hidden
                  :share share
                  ;; GLFW 3.4 position hints create at the final position,
                  ;; skipping Cocoa's center+cascade (glfw lib/src/cocoa_window.m).
                  :native-hints (append native-hints
                                        (list :position-x (round x)
                                              :position-y (round y)))
                  :class-initargs class-initargs))
         (chrome (make-instance 'alloy:border-layout))
         (root   (make-instance 'fixed-layout))
         (focus  (make-instance 'alloy:focus-list :focus-parent window))
         (bounds nil))
    ;; X11 renders inside resize callbacks: a vblank wait on every ConfigureNotify
    ;; would put each frame of a resize burst one more frame behind the pointer.
    ;; glfwSwapInterval acts on the current context, so make this window's current.
    (when (eq :x11 (%glfw:platform))
      (%glfw:make-current window)
      (setf (%glfw:swap-interval window) 0))
    ;; X11 leaves a resized window's new strip unpainted until the next GL frame;
    ;; a native background lets the server fill it at once.
    (set-native-window-background window background-color)
    ;; X/Y is the external box; the creation hints put the CONTENT there. The
    ;; frame is only measurable once the window exists, so the correction runs
    ;; here, while the window is still hidden.
    (multiple-value-bind (fx fy cx cy) (place-window-frame window x y width height)
      (setf bounds (alloy:px-extent cx cy width height))
      (setf (simple:palette window)
            (simple:palette (case palette
                              (:light 'desktop-light)
                              (:dark  'simple:dark)
                              (t palette))))
      (alloy:enter chrome window)
      (alloy:enter root chrome :place :center)
      (setf (alloy:bounds window) bounds)
      ;; X11 delivers glfw:window-resized asynchronously; the bounds slot and the
      ;; projection stay 0x0 until the loop pumps. Set them synchronously (as
      ;; alloy renderers/glfw/windowing.lisp: glfw:window-resized for window does)
      ;; so content added before the first poll lays out and renders at the real size.
      (setf (slot-value window 'alloy:bounds) bounds)
      ;; ... and then the size the OS granted wins over the one requested
      (setf bounds (adopt-os-window-size window))
      (alloy:suggest-size bounds window)
      (let ((m (simple:transform-matrix window)))
        (simple:orthographic-matrix (alloy:pxw bounds) (alloy:pxh bounds) m)
        (setf (simple:identity-matrix window) (copy-seq m)))
      (alloy:refit chrome)
      (setf (%state-root-layout state) root
            (%state-focus-list state) focus
            (%state-chrome state) chrome)
      (register-window window state)
      (when initialize-content-scale-p
        (let ((scale (or (window-content-scale window) 1.0)))
          (note-content-scale scale)
          (apply-window-content-scale window scale)))
      (values window fx fy))))

;;; Window icon.

(defun window-icon-supported-p ()
  "True when the window system shows a per-window icon set by the application:
Win32 and X11. Cocoa and Wayland refuse it. A runtime query: the same image
runs under X11 or Wayland on Linux."
  (and (member (%glfw:platform) '(:win32 :x11)) t))

(defun set-window-icon (window pixels width height)
  "Set WINDOW's title-bar/taskbar icon to PIXELS, WIDTH x HEIGHT RGBA octets with
the top row first. Return T when set, NIL where the window system has no
per-window icon (WINDOW-ICON-SUPPORTED-P).

The pixels go to glfwSetWindowIcon through an :unsigned-char buffer: the glfw
wrapper's (setf icon) stores them as :char and so rejects every octet above 127,
and a call on a platform that refuses icons would leave its GLFW error pending
for the next wrapped call to signal."
  (when (window-icon-supported-p)
    (let ((n (length pixels)))
      (cffi:with-foreign-objects ((image '(:struct org.shirakumo.fraf.glfw.cffi:image))
                                  (buf :unsigned-char n))
        (dotimes (i n) (setf (cffi:mem-aref buf :unsigned-char i) (aref pixels i)))
        (setf (org.shirakumo.fraf.glfw.cffi:image-width image) width
              (org.shirakumo.fraf.glfw.cffi:image-height image) height
              (org.shirakumo.fraf.glfw.cffi:image-pixels image) buf)
        (org.shirakumo.fraf.glfw.cffi:set-window-icon (%glfw:pointer window) 1 image)))
    t))

;;; Window events every desktop window answers the same way.

(defmethod alloy:handle ((event alloy:drop-event) (window window))
  ;; files dropped from another application (alloy renderers/glfw/windowing.lisp:
  ;; glfw:file-dropped for window) go to the host; a drop nobody takes is declined
  (or (files-dropped window (alloy:paths event) (alloy:location event))
      (alloy:decline)))

(defmethod %glfw:window-refreshed :after ((window window))
  ;; the window system asks for a repaint during a live resize (WM_PAINT) and when
  ;; something passes over the window. X11 interleaves Expose with queued
  ;; ConfigureNotify events, so rendering here would pair an older layout with a
  ;; newer framebuffer; there the event loop renders once from the final geometry.
  (unless (or (window-render-suspended-p window)
              (eq :x11 (%glfw:platform)))
    (alloy:render window window)))

(defmethod %glfw:window-resized :after ((window window) width height)
  (declare (ignore width height))
  ;; the whole layout settles here, parents first, before any frame decides the scrollbars from the
  ;; bounds (view/render.lisp: prepare-for-render :before for scroll-mixin), which runs before the render's refits
  (let ((chrome (window-chrome window)))
    (when chrome
      (alloy:refit chrome)
      (map-elements (lambda (element)
                      (when (and (typep element 'alloy:layout) (alloy:layout-needed-p element))
                        (alloy:refit element)))
                    chrome))))

(defmethod %glfw:window-content-scale-changed :after ((window window) xscale yscale)
  (declare (ignore yscale))
  (let ((scale (float xscale 1f0)))
    (apply-window-content-scale window scale)
    (window-scale-changed window scale)))

(defmethod %glfw:window-focused :after ((window window) focused)
  (multiple-value-bind (previous current was-active active-p)
      (note-window-focus window focused)
    ;; order: top-level state and z-order, the macOS menu bar, modal refocus,
    ;; caret/selection repaint, and last the host's activation callback
    #+darwin (when focused (macos-focus-window-menu window))
    (when focused
      (refocus-modal-window window))
    ;; the caret belongs to the active window alone, so both sides of the switch
    ;; repaint and the pane taking over starts on the visible half; the tracked
    ;; selection owner joins it because its highlight follows the OS focus too
    ;; (selection-hidden-p) and it is rarely the pane holding :strong
    (unless (and (eq previous current)
                 (eq was-active active-p))
      (reset-caret-blink)
      (dolist (w (list previous current))
        (when (and w (window-registered-p w))
          (dolist (pane (list (strong-focus-pane w) (window-selected-text-pane w)))
            (when (typep pane 'alloy:renderable)
              (alloy:mark-for-render pane))))))
    (window-activated window focused)))
