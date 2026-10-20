(in-package :alloy-desktop)

(defun typography-dpi ()
  "Logical DPI the toolkit lays text out at, at 100% display scaling."
  #+(or win32 linux) 96
  #-(or win32 linux) 72)

(defun framebuffer-ratio (window)
  "Framebuffer px per window px; >1 where the OS realizes DPI structurally (macOS Retina).
On GLFW/X11 the logical window size and framebuffer size callbacks may describe
different resize generations while a window is being realized.  Alloy already
uses the logical size for projection and the framebuffer size for glViewport,
so folding that transient ratio into BASE-SCALE corrupts the whole window."
  ;; Win32 framebuffer px and client px are the same unit (GLFW win32 reads
  ;; both from GetClientRect); a ratio other than 1 only ever comes from the
  ;; wrapper's size caches disagreeing -- e.g. a requested width below the
  ;; native minimum leaves the size cache at the refused value while the
  ;; framebuffer reports the real one -- and folding that into BASE-SCALE
  ;; shrinks every un-based glyph in the window.
  #+win32
  (return-from framebuffer-ratio 1.0)
  #+linux
  (when (eq :x11 (org.shirakumo.fraf.glfw:platform))
    (return-from framebuffer-ratio 1.0))
  (let ((win (org.shirakumo.fraf.glfw:size window))
        (fb  (org.shirakumo.fraf.glfw:framebuffer-size window)))
    (if (and (numberp (first win)) (plusp (first win))
             (numberp (first fb))  (plusp (first fb)))
        (/ (float (first fb) 1f0) (float (first win) 1f0))
        1.0)))

(defun apply-content-scale (window scale)
  "Set alloy:base-scale of WINDOW's UI to SCALE net of the framebuffer ratio.
Returns the base-scale written, or NIL when WINDOW has no UI yet."
  (let* ((layout-tree (alloy:layout-tree window))
         (ui          (and layout-tree (alloy:ui layout-tree))))
    (when ui
      ;; alloy ui.lisp: (setf base-scale) returns the scale-changed handling, not the value
      (let ((base-scale (/ (float scale 0f0) (framebuffer-ratio window))))
        (setf (alloy:base-scale ui) base-scale)
        base-scale))))

(defun apply-window-content-scale (window scale)
  "Apply SCALE to WINDOW and invalidate its realized font entries.
Does not notify the application of a native DPI change."
  (apply-content-scale window scale)
  ;; a memoized font entry was realized to whole pixels at the old scale
  (forget-font-cache window)
  scale)

(defvar *last-content-scale* 1.0
  "The last display content scale recorded by NOTE-CONTENT-SCALE, as the OS reports it.")

(defun note-content-scale (scale)
  "Record the current display content scale for fallback use."
  (setf *last-content-scale* scale))

(defun window-content-scale (window)
  "The OS content scale of WINDOW as a float, or NIL when GLFW reports an error."
  (handler-case
      (destructuring-bind (x y)
          (org.shirakumo.fraf.glfw:content-scale window)
        (declare (ignore y))
        (float x 1f0))
    ;; WINDOW is a live GLFW window, so the only failure is GLFW's own error check
    (%glfw:glfw-error (c)
      (warn "[content-scale] ~A" (princ-to-string c))
      nil)))
