(in-package :alloy-desktop)

(defun make-native-window (screen
                           &key
                             (class 'window)
                             (title windowing:*default-window-title*)
                             (icon windowing:*default-window-icon*)
                             (bounds windowing:*default-window-bounds*)
                             min-size
                             max-size
                             preferred-size
                             (state :normal)
                             (decorated-p t)
                             always-on-top-p
                             (resizable-p t)
                             (background-color colors:black)
                             parent
                             share
                             native-hints
                             class-initargs)
  "Create a desktop window and its native GLFW context on SCREEN.

CLASS, TITLE, ICON, BOUNDS, MIN-SIZE, MAX-SIZE, PREFERRED-SIZE, STATE,
DECORATED-P, ALWAYS-ON-TOP-P, RESIZABLE-P, BACKGROUND-COLOR, and PARENT are
the Alloy/GLFW window creation contract. SHARE selects an existing GLFW window
whose context share group the new context joins. NATIVE-HINTS is a plist of
GLFW window-hint keyword/value pairs. CLASS-INITARGS is a plist of additional
initargs forwarded to CLASS.

GLFW window hints are process-global and persist between creations, so reset
them before delegating to the Alloy windowing backend."
  (check-type native-hints list)
  (check-type class-initargs list)
  (unless (evenp (length native-hints))
    (error "NATIVE-HINTS must be a property list: ~S" native-hints))
  (unless (evenp (length class-initargs))
    (error "CLASS-INITARGS must be a property list: ~S" class-initargs))
  (org.shirakumo.fraf.glfw.cffi:default-window-hints)
  (apply #'windowing:make-window screen
         :class class
         :title title
         :icon icon
         :bounds bounds
         :min-size min-size
         :max-size max-size
         :preferred-size preferred-size
         :state state
         :decorated-p decorated-p
         :always-on-top-p always-on-top-p
         :resizable-p resizable-p
         :background-color background-color
         :parent parent
         (append (when share (list :share share))
                 native-hints
                 class-initargs)))


(defun acquire-gl-context (renderer)
  "Make RENDERER's GLFW context current and return a restoration token.

When RENDERER is already current, or is not a GLFW window, return :NONE.
Otherwise the returned token identifies the previously current context and must
be passed to RELEASE-GL-CONTEXT."
  (if (typep renderer '%glfw:window)
      (let ((previous (%glfw:get-current)))
        (if (eq previous renderer)
            :none
            (progn
              (%glfw:make-current renderer)
              previous)))
      :none))

(defun release-gl-context (previous)
  "Restore the context represented by PREVIOUS.

PREVIOUS is the token returned by ACQUIRE-GL-CONTEXT. If that previous window
was destroyed while another context was borrowed, clear the current context
instead of attempting to reactivate a dead native handle."
  (unless (eq previous :none)
    (%glfw:make-current
     (and previous (%glfw:pointer previous) previous))))

(defun call-with-gl-context (renderer thunk)
  "Call THUNK with RENDERER's GLFW context current, then restore the previous one."
  (let ((previous (acquire-gl-context renderer)))
    (unwind-protect
         (funcall thunk)
      (release-gl-context previous))))
