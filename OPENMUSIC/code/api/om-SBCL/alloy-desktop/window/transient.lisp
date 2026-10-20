(in-package :alloy-desktop)

(defun %apply-native-window-owner (surface owner)
  "Make SURFACE a transient/owned native window of OWNER where the platform
backend exposes that relation. Failure is cosmetic: GLFW floating remains the
portable fallback and the caller still owns the transient lifetime explicitly."
  (handler-case
      (progn
        #+win32
        ;; GWLP_HWNDPARENT=-8 turns a top-level HWND into an owned window. Owned
        ;; windows stay above their owner and do not acquire a taskbar entry.
        (let ((popup-hwnd (%window-hwnd surface))
              (owner-hwnd (%window-hwnd owner)))
          (when (and popup-hwnd owner-hwnd
                     (not (cffi:null-pointer-p popup-hwnd))
                     (not (cffi:null-pointer-p owner-hwnd)))
            (%set-window-long-ptr
             popup-hwnd -8 (cffi:pointer-address owner-hwnd))))
        #+darwin
        ;; NSWindowAbove = 1.
        (let ((popup-ns (%glfw-get-cocoa-window (%glfw::pointer surface)))
              (owner-ns (%glfw-get-cocoa-window (%glfw::pointer owner))))
          (when (and popup-ns owner-ns
                     (not (cffi:null-pointer-p popup-ns))
                     (not (cffi:null-pointer-p owner-ns)))
            (cffi:foreign-funcall
             "objc_msgSend"
             :pointer owner-ns
             :pointer (%sel-register-name "addChildWindow:ordered:")
             :pointer popup-ns
             :long 1
             :void)))
        #+(or linux freebsd)
        ;; X11 transient-for is the window-manager ownership contract. Wayland
        ;; has no public GLFW parent API, so the floating fallback is retained.
        (when (eq :x11 (%glfw:platform))
          (let ((display (%glfw-get-x11-display))
                (popup-xid (%glfw-get-x11-window (%glfw::pointer surface)))
                (owner-xid (%glfw-get-x11-window (%glfw::pointer owner))))
            (when (and display (not (cffi:null-pointer-p display))
                       (/= popup-xid 0) (/= owner-xid 0))
              (%x-set-transient-for-hint display popup-xid owner-xid)
              (%x-flush display)))))
    (error (c)
      (warn "[popup native owner] ~A" (princ-to-string c))))
  surface)

