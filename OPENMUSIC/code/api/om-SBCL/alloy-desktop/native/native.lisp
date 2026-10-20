(in-package :alloy-desktop)

(defclass native-backend () ())
(defclass win32-backend (native-backend) ())
(defclass darwin-backend (native-backend) ())
(defclass linux-backend (native-backend) ())

#+win32
(progn
  (cffi:define-foreign-library user32 (:windows "user32.dll"))
  (unless (cffi:foreign-library-loaded-p 'user32)
    (cffi:load-foreign-library 'user32))
  (cffi:defcfun ("glfwGetWin32Window" %glfw-get-win32-window) :pointer
    (window :pointer))
  (cffi:defcfun ("EnableWindow" %enable-window) :int
    (hwnd :pointer) (enable :int))
  (cffi:defcfun ("GetWindowLongPtrW" %get-window-long-ptr) :long-long
    (hwnd :pointer) (index :int))
  (cffi:defcfun ("SetWindowLongPtrW" %set-window-long-ptr) :long-long
    (hwnd :pointer) (index :int) (new-long :long-long))
  (cffi:defcfun ("SetWindowPos" %set-window-pos) :int
    (hwnd :pointer) (after :pointer) (x :int) (y :int) (cx :int) (cy :int) (flags :uint))
  ;; front-to-back walk of the top-level z-order (GetTopWindow NULL, then
  ;; GW_HWNDNEXT): used by a host that needs the native overlap order.
  (cffi:defcfun ("GetTopWindow" %get-top-window) :pointer
    (hwnd :pointer))
  (cffi:defcfun ("GetWindow" %get-window) :pointer
    (hwnd :pointer) (cmd :uint))
  (cffi:defcfun ("SystemParametersInfoW" %system-parameters-info-w) :boolean
    (action :uint) (param :uint) (data :pointer) (flags :uint)))

(defun %window-hwnd (window)
  ;; a destroyed window has no native handle; NIL reaches the FFI as a null SAP
  #+win32 (let ((ptr (and window (%glfw::pointer window))))
            (and ptr (%glfw-get-win32-window ptr)))
  #-win32 (declare (ignore window))
  #-win32 nil)

(defun keyboard-cues-setting ()
  "True when the system option keeps keyboard cues permanently visible (Windows SPI_GETKEYBOARDCUES, NIL elsewhere)."
  #+win32 (cffi:with-foreign-object (on :int)
            (setf (cffi:mem-ref on :int) 0)
            (and (%system-parameters-info-w #x100A 0 on 0)
                 (not (zerop (cffi:mem-ref on :int)))))
  #-win32 nil)

#+darwin
(progn
  (cffi:defcfun ("glfwGetCocoaWindow" %glfw-get-cocoa-window) :pointer
    (window :pointer))
  (cffi:defcfun ("sel_registerName" %sel-register-name) :pointer
    (name :string))
  (cffi:defcfun ("objc_getClass" %objc-get-class) :pointer
    (name :string)))

(defmacro with-autorelease-pool (&body body)
  "Run BODY inside a fresh Objective-C autorelease pool on macOS, and as a plain PROGN elsewhere."
  #+darwin
  (let ((pool (gensym "POOL")))
    `(let ((,pool (cffi:foreign-funcall "objc_autoreleasePoolPush" :pointer)))
       (unwind-protect (progn ,@body)
         (cffi:foreign-funcall "objc_autoreleasePoolPop" :pointer ,pool :void))))
  #-darwin
  `(progn ,@body))

#+darwin
(defun %live-resize-p (window)
  "T while WINDOW's NSWindow is inside a Cocoa live-resize gesture."
  ;; ObjC BOOL is a signed char, so the return is read as :char, not :bool.
  (let ((nswin (%glfw-get-cocoa-window (%glfw::pointer window))))
    (and nswin (not (cffi:null-pointer-p nswin))
         (not (zerop (cffi:foreign-funcall "objc_msgSend"
                                           :pointer nswin
                                           :pointer (%sel-register-name "inLiveResize")
                                           :char))))))

#+(or linux freebsd)
(progn
  (cffi:define-foreign-library libx11 (t (:or "libX11.so.6" "libX11.so")))
  ;; X11 is optional; a Wayland-only host without libX11 must not abort the load.
  (unless (cffi:foreign-library-loaded-p 'libx11)
    (handler-case (cffi:load-foreign-library 'libx11)
      (cffi:load-foreign-library-error (c)
        (format *error-output* "[libx11] ~A~%" (princ-to-string c)))))
  (cffi:defcfun ("glfwGetX11Display" %glfw-get-x11-display) :pointer)
  (cffi:defcfun ("glfwGetX11Window" %glfw-get-x11-window) :unsigned-long
    (window :pointer))
  (cffi:defcfun ("XInternAtom" %x-intern-atom) :unsigned-long
    (display :pointer) (name :string) (only-if-exists :int))
  (cffi:defcfun ("XChangeProperty" %x-change-property) :int
    (display :pointer) (window :unsigned-long) (property :unsigned-long)
    (type :unsigned-long) (format :int) (mode :int) (data :pointer) (nelements :int))
  (cffi:defcfun ("XSetTransientForHint" %x-set-transient-for-hint) :int
    (display :pointer) (window :unsigned-long) (owner :unsigned-long))
  (cffi:defcfun ("XFlush" %x-flush) :int
    (display :pointer))
  (cffi:defcfun ("XDefaultRootWindow" %x-default-root-window) :unsigned-long
    (display :pointer))
  (cffi:defcfun ("XQueryTree" %x-query-tree) :int
    (display :pointer) (window :unsigned-long) (root-return :pointer)
    (parent-return :pointer) (children-return :pointer) (nchildren-return :pointer))
  (cffi:defcfun ("XFree" %x-free) :int
    (data :pointer))
  (cffi:defcfun ("XDefaultScreen" %x-default-screen) :int
    (display :pointer))
  (cffi:defcfun ("XDefaultColormap" %x-default-colormap) :unsigned-long
    (display :pointer) (screen :int))
  (cffi:defcfun ("XAllocColor" %x-alloc-color) :int
    (display :pointer) (colormap :unsigned-long) (color :pointer))
  (cffi:defcfun ("XChangeWindowAttributes" %x-change-window-attributes) :int
    (display :pointer) (window :unsigned-long) (valuemask :unsigned-long)
    (attributes :pointer))
  ;; Xlib's XColor and XSetWindowAttributes, field for field.
  (cffi:defcstruct %x-color
    (pixel :unsigned-long)
    (red :unsigned-short) (green :unsigned-short) (blue :unsigned-short)
    (flags :char) (pad :char))
  (cffi:defcstruct %x-set-window-attributes
    (background-pixmap :unsigned-long) (background-pixel :unsigned-long)
    (border-pixmap :unsigned-long) (border-pixel :unsigned-long)
    (bit-gravity :int) (win-gravity :int) (backing-store :int)
    (backing-planes :unsigned-long) (backing-pixel :unsigned-long)
    (save-under :int) (event-mask :long) (do-not-propagate-mask :long)
    (override-redirect :int) (colormap :unsigned-long) (cursor :unsigned-long)))

(defun set-native-window-background (window color)
  "Give WINDOW's native surface a background in COLOR so the window system paints newly exposed areas itself (X11: background pixel + NorthWestGravity). No-op where the window system has no such attribute; return T when applied."
  #-(or linux freebsd) (declare (ignore window color))
  #+(or linux freebsd)
  (let* ((x11 (and (eq :x11 (%glfw:platform))
                   (cffi:foreign-library-loaded-p 'libx11)))
         (pointer (and x11 window (%glfw:pointer window)))
         (display (and pointer (%glfw-get-x11-display)))
         (xid (if (and display (not (cffi:null-pointer-p display)))
                  (%glfw-get-x11-window pointer)
                  0)))
    (unless (zerop xid)
      (flet ((channel (value) (round (* 65535 (max 0 (min 1 value))))))
        (cffi:with-foreign-objects ((xcolor '(:struct %x-color))
                                    (attributes '(:struct %x-set-window-attributes)))
          (cffi:with-foreign-slots ((red green blue flags) xcolor (:struct %x-color))
            (setf red (channel (chroma:red color))
                  green (channel (chroma:green color))
                  blue (channel (chroma:blue color))
                  ;; DoRed | DoGreen | DoBlue
                  flags 7))
          (cond
            ((zerop (%x-alloc-color display
                                    (%x-default-colormap display (%x-default-screen display))
                                    xcolor))
             (format *error-output* "[set-native-window-background] XAllocColor refused ~S~%" color)
             nil)
            (t
             (cffi:with-foreign-slots ((background-pixel bit-gravity) attributes
                                       (:struct %x-set-window-attributes))
               (setf background-pixel (cffi:foreign-slot-value xcolor '(:struct %x-color) 'pixel)
                     ;; NorthWestGravity
                     bit-gravity 1))
             ;; CWBackPixel | CWBitGravity
             (%x-change-window-attributes display xid (logior 2 16) attributes)
             (%x-flush display)
             t))))))
  #-(or linux freebsd) nil)

(defun system-beep ()
  #+win32  (cffi:foreign-funcall "MessageBeep" :uint #xFFFFFFFF :int)
  #+darwin (cffi:foreign-funcall "NSBeep" :void)
  #+(or linux freebsd)
  (let ((dpy (and (eq :x11 (%glfw:platform)) (%glfw-get-x11-display))))
    (cond
      ((and dpy (not (cffi:null-pointer-p dpy)))
       (cffi:foreign-funcall "XBell" :pointer dpy :int 0 :int)
       (cffi:foreign-funcall "XFlush" :pointer dpy :int))
      ((and (probe-file "/usr/bin/paplay")
            (probe-file "/usr/share/sounds/freedesktop/stereo/bell.oga"))
       (uiop:launch-program '("/usr/bin/paplay"
                              "/usr/share/sounds/freedesktop/stereo/bell.oga")))
      (t
       (write-char (code-char 7) *standard-output*)
       (force-output *standard-output*))))
  nil)

