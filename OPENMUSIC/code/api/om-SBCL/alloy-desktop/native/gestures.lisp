(in-package :alloy-desktop)

;;; Pinch-to-zoom on macOS. GLFW delivers no magnification event, so the handler
;;; is added at runtime to the ObjC class of the GLFW content view.

(cffi:defcfun ("object_getClass" %object-get-class) :pointer
  (object :pointer))

(cffi:defcfun ("class_addMethod" %class-add-method) :char
  (class :pointer) (selector :pointer) (imp :pointer) (types :string))

(defvar *magnify-handler-installed* nil
  "NIL, T once the IMP is in place, or :REFUSED when class_addMethod said no.")

(defvar *magnify-callback* nil
  "Function of one argument, the raw NSEvent magnification (a double), called on every pinch step.")

(cffi:defcallback %magnify-with-event :void
    ((self :pointer) (cmd :pointer) (event :pointer))
  (declare (ignore self cmd)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  ;; the one legitimate swallow: a Lisp condition must never unwind across the ObjC frame
  (handler-case
      (let ((magnification
              (cffi:foreign-funcall "objc_msgSend"
                                    :pointer event
                                    :pointer (%sel-register-name "magnification")
                                    :double)))
        (when (and *magnify-callback* (realp magnification) (/= magnification 0))
          (let ((traps (armed-float-traps)))
            (mask-float-traps)
            (unwind-protect (funcall *magnify-callback* magnification)
              (set-float-traps traps)))))
    (error (c)
      ;; if even *error-output* is gone there is nowhere left to report
      (handler-case
          (format *error-output* "[magnify] ~A~%" (princ-to-string c))
        (stream-error () nil)))))

(defun install-magnify-handler (window callback)
  "Route WINDOW's pinch gestures to CALLBACK, adding magnifyWithEvent: to the GLFW
content view's ObjC class once per session. Return T, :REFUSED or NIL."
  (setf *magnify-callback* callback)
  (unless *magnify-handler-installed*
    (let ((pointer (and window (%glfw:pointer window))))
      (when (and pointer (not (cffi:null-pointer-p pointer)))
        (let ((nswindow (%glfw-get-cocoa-window pointer)))
          (when (and nswindow (not (cffi:null-pointer-p nswindow)))
            (let ((view (cffi:foreign-funcall "objc_msgSend"
                                              :pointer nswindow
                                              :pointer (%sel-register-name "contentView")
                                              :pointer)))
              (when (and view (not (cffi:null-pointer-p view)))
                (let ((added (plusp (%class-add-method
                                     (%object-get-class view)
                                     (%sel-register-name "magnifyWithEvent:")
                                     (cffi:callback %magnify-with-event)
                                     "v@:@"))))
                  (setf *magnify-handler-installed* (if added t :refused))
                  (unless added
                    (format *error-output*
                            "[magnify] class_addMethod magnifyWithEvent: refused~%"))))))))))
  *magnify-handler-installed*)
