(in-package :alloy-desktop)

(defun %os-window-size (window)
  "The client size the window system REALLY applied, from glfwGetWindowSize.
The wrapper's own reader answers from a cache written with the size that was
REQUESTED (glfw wrapper.lisp: (setf size) for window), so it reports a size the OS
may have refused."
  (cffi:with-foreign-objects ((w :int) (h :int))
    (org.shirakumo.fraf.glfw.cffi:get-window-size (%glfw:pointer window) w h)
    (values (cffi:mem-ref w :int) (cffi:mem-ref h :int))))

(defun adopt-os-window-size (window)
  "Replace WINDOW's cached geometry with the size the OS actually applied, and
return the extent now in the bounds slot.

The OS caps a window at the display's maximum tracking size, so any larger
request is applied smaller -- and the wrapper still caches the value requested
(glfw wrapper.lisp: (setf size) for window writes the cache AFTER the OS call, over
the real size its own glfw:window-resized :before had just stored). Every pointer
event is then flipped with that too-tall height (alloy renderers/glfw/windowing.lisp:
glfw:mouse-moved for window), while drag routing tests containment with the same
cached geometry. The mismatch persists until a real OS resize rewrites the cache."
  (multiple-value-bind (rw rh) (%os-window-size window)
    (let ((bounds (alloy:bounds window)))
      (if (and (plusp rw) (plusp rh))
          (progn
            (setf (slot-value window 'org.shirakumo.fraf.glfw::width) rw
                  (slot-value window 'org.shirakumo.fraf.glfw::height) rh)
            (if (and (= rw (round (alloy:pxw bounds)))
                     (= rh (round (alloy:pxh bounds))))
                bounds
                (setf (slot-value window 'alloy:bounds)
                      (alloy:px-extent (alloy:pxx bounds) (alloy:pxy bounds) rw rh))))
          bounds))))

(defmethod (setf %glfw:size) :after (size (window window))
  "Every geometry write through the GLFW size writer reaches the OS. Where the
resize is observable before the call returns, adopt the size the OS actually
applied rather than the wrapper's requested-size cache. X11 applies the resize
asynchronously, so there the native resize callback stays authoritative instead
of adopting a stale pre-resize size."
  (declare (ignore size))
  (unless (eq :x11 (%glfw:platform))
    (adopt-os-window-size window)))

(defun set-window-size (window width height)
  "Resize WINDOW's content area to WIDTH x HEIGHT px and synchronize its cached
bounds, layout and projection, at once, also while WINDOW is hidden and so gets
no resize callback. On X11 the request is asynchronous: the requested size is
installed now and the later native resize callback may replace it with the size
the window manager granted. Return the synchronized size as (values width height)."
  (let ((requested-w (round width))
        (requested-h (round height)))
    (setf (%glfw:size window) (list requested-w requested-h))
    ;; XResizeWindow only queues the request, so on X11 the size read back here
    ;; can still be the old geometry until ConfigureNotify is handled
    (let* ((x11 (eq :x11 (%glfw:platform)))
           (bounds (alloy:bounds window))
           (w (if x11 requested-w (round (alloy:pxw bounds))))
           (h (if x11 requested-h (round (alloy:pxh bounds)))))
      ;; the handler of an OS resize: bounds, size suggestion and projection (alloy renderers/glfw/windowing.lisp:
      ;; glfw:window-resized for window), then the content laid out (window/assembly.lisp: its :after for window)
      (%glfw:window-resized window w h)
      (values w h))))

(deftype %glfw-unavailable ()
  "The two ways a GLFW query can fail to answer: GLFW itself reports the failure
through its error check (glfw-error: not initialised, monitor unplugged), or the
shared library was never loaded, which SBCL reports as undefined-alien-error."
  '(or %glfw:glfw-error sb-alien:undefined-alien-error))

(defun screen-size ()
  "Primary-monitor (width height) in screen coords; (1920 1080) fallback.
PRIMARY-MONITOR signals glfw-error when no monitor is connected / glfw not init."
  (let ((vm (handler-case (%glfw:video-mode (%glfw:primary-monitor))
              (%glfw-unavailable () nil))))
    (if (and (consp vm) (numberp (first vm)) (numberp (second vm)))
        (values (first vm) (second vm))
        (values 1920 1080))))

;;; GLFW positions the content area. These helpers translate between content-area
;;; coordinates and the external native frame used by desktop window placement.

(defun %window-decorated-p (window)
  "T when WINDOW carries a native frame. Read straight from GLFW: alloy's
windowing:decorated-p tests (< 1 attribute) and so answers NIL for every window
(alloy renderers/glfw/windowing.lisp: get-window-bool-attribute)."
  (let ((ptr (%glfw:pointer window)))
    (and ptr
         (not (cffi:null-pointer-p ptr))
         (plusp (%glfw:attribute :decorated window)))))

(defun %window-frame-offsets (window)
  "Frame thickness around WINDOW's content area, (values LEFT TOP RIGHT BOTTOM) in
screen px; all zero for an undecorated surface. X11 publishes the extents only
once the window is mapped, so a hidden window may answer zero and callers may
retry after display."
  (if (%window-decorated-p window)
      (let ((frame (%glfw:frame-size window)))
        (values (or (first frame) 0) (or (second frame) 0)
                (or (third frame) 0) (or (fourth frame) 0)))
      (values 0 0 0 0)))

(defun %clamp-window-frame-origin (fx fy fw fh)
  "Keep a window's external box origin inside the target monitor work area.
Only the corner moves: a window larger than the work area keeps its size and
overflows right/bottom."
  (let* ((monitor (%popup-monitor-for-screen-point fx fy))
         (work (and monitor (%glfw:work-area monitor))))
    (if (and (consp work) (= 4 (length work)))
        (destructuring-bind (wx wy ww wh) work
          (values (max wx (min fx (- (+ wx ww) (min fw ww))))
                  (max wy (min fy (- (+ wy wh) (min fh wh))))))
        (values fx fy))))

(defun place-window-frame (window fx fy w h)
  "Place WINDOW by external-box top-left FX/FY with content size W/H. Return
(values FRAME-X FRAME-Y CONTENT-X CONTENT-Y): the clamped external origin and
the content-area origin passed to GLFW."
  (multiple-value-bind (l u r b) (%window-frame-offsets window)
    (multiple-value-bind (cfx cfy)
        (if (%window-decorated-p window)
            (%clamp-window-frame-origin fx fy (+ w l r) (+ h u b))
            (values fx fy))
      (values (round cfx) (round cfy)
              (round (+ cfx l)) (round (+ cfy u))))))

(defun window-content-to-frame (window cx cy)
  "Convert GLFW content-area origin CX/CY to the external-box origin."
  (multiple-value-bind (l u) (%window-frame-offsets window)
    (values (- cx l) (- cy u))))

(defun %adjusted-axis (spec origin span size low-key high-key)
  (labels ((base (kind)
             (cond ((or (eq kind low-key) (eq kind :left) (eq kind :top)) origin)
                   ((member kind '(:center :centre))
                    (+ origin (round (- span size) 2)))
                   ((or (eq kind high-key) (eq kind :right) (eq kind :bottom))
                    (+ origin (- span size)))
                   (t (error "Unsupported window position anchor ~S." kind)))))
    (cond ((null spec) (base :center))
          ((realp spec) (+ origin spec))
          ((keywordp spec) (base spec))
          ((and (consp spec) (keywordp (first spec))
                (realp (second spec)) (null (cddr spec)))
           (+ (base (first spec)) (second spec)))
          (t (error "Invalid window position ~S." spec)))))

(defun place-window-relative-to (window reference x y)
  "Move WINDOW's external box relative to REFERENCE, a window or :SCREEN (the
primary monitor). X and Y each are NIL (centred), a number (offset from the
reference origin), an anchor keyword (:LEFT/:TOP, :RIGHT/:BOTTOM, :CENTER/:CENTRE)
or (ANCHOR OFFSET). The reference spans its external origin and content size.
Runs on the UI thread. Return the external origin as (values FRAME-X FRAME-Y)."
  (call-on-ui-thread
   (lambda ()
     (let ((size (window-bounds window)))
       (multiple-value-bind (origin-x origin-y span-w span-h)
           (if (eq reference :screen)
               (multiple-value-bind (screen-w screen-h) (screen-size)
                 (values 0 0 screen-w screen-h))
               (let ((bounds (window-bounds reference)))
                 (multiple-value-bind (fx fy)
                     (window-content-to-frame reference (alloy:pxx bounds) (alloy:pxy bounds))
                   (values (round fx) (round fy)
                           (round (alloy:pxw bounds)) (round (alloy:pxh bounds))))))
         (multiple-value-bind (fx fy cx cy)
             (place-window-frame
              window
              (%adjusted-axis x origin-x span-w (round (alloy:pxw size)) :left :right)
              (%adjusted-axis y origin-y span-h (round (alloy:pxh size)) :top :bottom)
              (alloy:pxw size) (alloy:pxh size))
           (let ((extent (alloy:px-extent cx cy (alloy:pxw size) (alloy:pxh size))))
             (setf (alloy:bounds window) extent)
             ;; only the asynchronous move callback refreshes the bounds slot; a
             ;; read-back in the same turn must see the position just applied
             (setf (slot-value window 'alloy:bounds) extent))
           (values fx fy)))))))

(defun %popup-point-distance-to-work-area (x y work)
  (destructuring-bind (wx wy ww wh) work
    (let ((dx (cond ((< x wx) (- wx x))
                    ((> x (+ wx ww)) (- x (+ wx ww)))
                    (t 0)))
          (dy (cond ((< y wy) (- wy y))
                    ((> y (+ wy wh)) (- y (+ wy wh)))
                    (t 0))))
      (+ (* dx dx) (* dy dy)))))

(defun %popup-monitor-for-screen-point (x y)
  "Monitor whose work area contains X/Y, or the closest monitor for a point in
the inter-monitor gap. Uses virtual-screen coordinates and therefore supports
negative monitor origins."
  ;; GLFW may be unavailable or unable to answer about monitors.
  (let ((monitors (handler-case (%glfw:list-monitors) (%glfw-unavailable () nil))))
    (or (find-if
         (lambda (monitor)
           (destructuring-bind (wx wy ww wh) (%glfw:work-area monitor)
             (and (<= wx x (+ wx ww))
                  (<= wy y (+ wy wh)))))
         monitors)
        (when monitors
          (reduce
           (lambda (a b)
             (if (<= (%popup-point-distance-to-work-area
                      x y (%glfw:work-area a))
                     (%popup-point-distance-to-work-area
                      x y (%glfw:work-area b)))
                 a b))
           monitors))
        (handler-case (%glfw:primary-monitor) (%glfw-unavailable () nil)))))

(defun set-window-size-limits (window min-w min-h max-w max-h)
  "Write all four GLFW size limits atomically, resolving max<min by keeping the
minimum before GLFW sees the pair."
  (flet ((limit (v) (and (realp v) (plusp v) (round v))))
    (let ((mnw (limit min-w)) (mnh (limit min-h))
          (mxw (limit max-w)) (mxh (limit max-h)))
      (when (and mnw mxw (> mnw mxw)) (setf mxw mnw))
      (when (and mnh mxh (> mnh mxh)) (setf mxh mnh))
      (setf (%glfw:size-limits window)
            (list (or mnw -1) (or mnh -1) (or mxw -1) (or mxh -1)))))
  window)

(defun set-window-min-size (window w h)
  "Override the minimum size alone while keeping the live maximum limits."
  (destructuring-bind (min-w min-h max-w max-h) (%glfw:size-limits window)
    (declare (ignore min-w min-h))
    (set-window-size-limits window w h max-w max-h)))

(defmethod window-bounds ((window window))
  (alloy:bounds window))

;;; Window under a virtual-screen point. Overlapping windows are the whole
;;; question: the user points at the one on TOP, and creation order says nothing
;;; about that, so each backend asks the window system for its stacking order.

(defun window-contains-screen-point-p (window sx sy)
  "True when the virtual-screen point SX/SY falls inside WINDOW's client area."
  (let ((position (%glfw:location window))
        (size     (%glfw:size window)))
    (and (<= (first position) sx (+ (first position) (first size)))
         (<= (second position) sy (+ (second position) (second size))))))

#+darwin
(defun %cocoa-window-number (window)
  (let ((nswindow (%glfw-get-cocoa-window (%glfw:pointer window))))
    (and nswindow (not (cffi:null-pointer-p nswindow))
         (cffi:foreign-funcall "objc_msgSend"
                               :pointer nswindow
                               :pointer (%sel-register-name "windowNumber")
                               :long))))

#+(or linux freebsd)
(defun %x11-query-tree (display xwindow)
  "XWINDOW's parent and its children bottom to top, as two values; NIL when the server refuses."
  (cffi:with-foreign-objects ((root :unsigned-long) (parent :unsigned-long)
                              (children :pointer) (count :unsigned-int))
    (unless (zerop (%x-query-tree display xwindow root parent children count))
      (let* ((array (cffi:mem-ref children :pointer))
             (stack (unless (cffi:null-pointer-p array)
                      (loop for i below (cffi:mem-ref count :unsigned-int)
                            collect (cffi:mem-aref array :unsigned-long i)))))
        (unless (cffi:null-pointer-p array)
          (%x-free array))
        (values (cffi:mem-ref parent :unsigned-long) stack)))))

#+(or linux freebsd)
(defun %x11-top-level (display xwindow root)
  "The child of ROOT that holds XWINDOW: a reparenting window manager stacks its
frame, never the client window itself."
  (loop with window = xwindow
        for parent = (%x11-query-tree display window)
        do (cond ((null parent) (return nil))
                 ((or (zerop parent) (= parent root)) (return window))
                 (t (setf window parent)))))

(defun %native-topmost-window (windows sx sy)
  "First of WINDOWS in the window system's front-to-back order at the
virtual-screen point SX/SY, NIL when the platform does not answer."
  (declare (ignorable sx sy))
  (when (rest windows)
    #+win32
    (loop for hwnd = (%get-top-window (cffi:null-pointer))
            then (%get-window hwnd 2)
          while (and hwnd (not (cffi:null-pointer-p hwnd)))
          thereis (find-if (lambda (window)
                             (let ((h (%window-hwnd window)))
                               (and h (cffi:pointer-eq h hwnd))))
                           windows))
    #+darwin
    ;; the virtual-screen point is Y-down from the primary display's top; Cocoa's
    ;; is Y-up from its bottom. Each answer is the topmost window at the point
    ;; below the previous one, so a covering foreign window is stepped over.
    (with-autorelease-pool
      (let ((numbers (mapcar (lambda (window) (cons (%cocoa-window-number window) window))
                             windows))
            (class (%objc-get-class "NSWindow"))
            (selector (%sel-register-name "windowNumberAtPoint:belowWindowWithWindowNumber:"))
            (x (float sx 1d0))
            (y (float (- (nth-value 1 (screen-size)) sy) 1d0)))
        (loop repeat 64
              for number = (cffi:foreign-funcall "objc_msgSend"
                                                 :pointer class :pointer selector
                                                 :double x :double y :long 0 :long)
                then (cffi:foreign-funcall "objc_msgSend"
                                           :pointer class :pointer selector
                                           :double x :double y :long number :long)
              until (zerop number)
              thereis (cdr (assoc number numbers :test #'eql)))))
    #+(or linux freebsd)
    (let ((display (and (eq :x11 (%glfw:platform)) (%glfw-get-x11-display))))
      (when (and display (not (cffi:null-pointer-p display)))
        (let* ((root (%x-default-root-window display))
               (frames (mapcar (lambda (window)
                                 (cons (%x11-top-level display
                                                       (%glfw-get-x11-window (%glfw:pointer window))
                                                       root)
                                       window))
                               windows)))
          (loop for xwindow in (reverse (nth-value 1 (%x11-query-tree display root)))
                thereis (cdr (assoc xwindow frames :test #'eql))))))))

(defun window-at-screen-point (sx sy &key (windows (live-windows)) exclude)
  "The window of WINDOWS, EXCLUDE apart, whose client area holds the virtual-screen
point SX/SY and is on top there; the most recently focused one when the window
system does not say, then the first; NIL when none holds the point."
  (let ((candidates (remove-if-not (lambda (window)
                                     (and (not (eq window exclude))
                                          (window-contains-screen-point-p window sx sy)))
                                   windows)))
    (or (%native-topmost-window candidates sx sy)
        (find-if (lambda (window) (member window candidates :test #'eq))
                 (window-z-order))
        (first candidates))))

