;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/cocoa.lisp,v 1.5.1.1 2007/10/23 22:17:07 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

;; Support for OpenGL with CAPI/Cocoa.
;; Symbols in the CAPI-COCOA-LIB package are not part of a supported API.

(in-package "OPENGL")

(defun opengl-pane-representation-view (rep)
  (capi-cocoa-lib::representation-main-view rep))

(defstruct cocoa-context
  context
  pixel-format
  set-view)

(defmethod %make-context ((rep capi-cocoa-lib::output-pane-representation)
                          opengl-configuration)
  (let* ((view (opengl-pane-representation-view rep))
         (pixel-format (choose-cocoa-pixel-format
                        view
                        opengl-configuration)))
    (if pixel-format
        (let ((nscontext (objc:invoke (objc:invoke "NSOpenGLContext" "alloc")
                                      "initWithFormat:shareContext:"
                                      pixel-format
                                      nil)))
          ;; Invoking setView here does not work for some reason so do it in
          ;; %start-rendering using the set-view slot instead.
          #+comment
          (objc:invoke nscontext "setView:" view)
          (make-cocoa-context :context nscontext
                              :pixel-format pixel-format
                              :set-view view))
      (error "Can't make context for ~s.~%Pixel-format not set"
             opengl-configuration))))

(defmethod %start-rendering ((rep capi-cocoa-lib::output-pane-representation)
                             context)
  (let ((nscontext (cocoa-context-context context)))
    (when (objc:null-objc-pointer-p (objc:invoke nscontext "view"))
      (objc:invoke nscontext "setView:" (cocoa-context-set-view context)))
    (objc:invoke nscontext "makeCurrentContext")
    t))

(defmethod %stop-rendering ((rep capi-cocoa-lib::output-pane-representation))
  (objc:invoke "NSOpenGLContext" "clearCurrentContext")
  t)

(defmethod %swap-buffers ((rep capi-cocoa-lib::output-pane-representation)
                          context)
  (let ((nscontext (cocoa-context-context context)))
    (objc:invoke nscontext "flushBuffer")
    t))

(defmethod %free-opengl-resources ((rep capi-cocoa-lib::output-pane-representation)
                                   context)
  (when context
    (let ((nscontext (cocoa-context-context context)))
      (objc:invoke nscontext "clearDrawable")
      (objc:release nscontext)
      (objc:release (cocoa-context-pixel-format context))))
  t)

(defmethod %resize-opengl-context ((rep capi-cocoa-lib::output-pane-representation)
                                   context width height)
  (declare (ignore width height))
  (when context
    (let ((nscontext (cocoa-context-context context)))
      (objc:invoke nscontext "update"))))

(defmethod %describe-configuration ((rep capi-cocoa-lib::output-pane-representation)
                                    context &optional (stream *standard-output*) collectp)
  (let ((results (descibe-cocoa-pixel-format
                  (cocoa-context-pixel-format context))))
    (if collectp
        results
      (format stream
              "~&Color buffer size : ~d~%Uses ~:[Color Index~;RGBA~]~%Is ~:[single~;double~]-buffered~@[~%Accumulator buffer size (per channel) = ~d bits~]~@[~%Depth buffer size = ~d bits~]~@[~%Stencil buffer size = ~d bits~]~@[~%Has ~d aux buffers~]" 
              (getf results :buffer-size) 
              (getf results :rgba)
              (getf results :double-buffer)
              (getf results :accum)
              (getf results :depth-buffer)
              (getf results :stencil-size)
              (getf results :aux)))))
                                           

;; NSOpenGLPixelFormatAttribute
(defconstant ns-open-gl-pfa-all-renderers 1)
(defconstant ns-open-gl-pfa-double-buffer 5)
(defconstant ns-open-gl-pfa-stereo 6)
(defconstant ns-open-gl-pfa-aux-buffers 7)
(defconstant ns-open-gl-pfa-color-size 8)
(defconstant ns-open-gl-pfa-alpha-size 11)
(defconstant ns-open-gl-pfa-depth-size 12)
(defconstant ns-open-gl-pfa-stencil-size 13)
(defconstant ns-open-gl-pfa-accum-size 14)
(defconstant ns-open-gl-pfa-minimum-policy 51)
(defconstant ns-open-gl-pfa-maximum-policy 52)
(defconstant ns-open-gl-pfa-off-screen 53)
(defconstant ns-open-gl-pfa-full-screen 54)
(defconstant ns-open-gl-pfa-sample-buffers 55)
(defconstant ns-open-gl-pfa-samples 56)
(defconstant ns-open-gl-pfa-aux-depth-stencil 57)
(defconstant ns-open-gl-pfa-renderer-id 70)
(defconstant ns-open-gl-pfa-single-renderer 71)
(defconstant ns-open-gl-pfa-no-recovery 72)
(defconstant ns-open-gl-pfa-accelerated 73)
(defconstant ns-open-gl-pfa-closest-policy 74)
(defconstant ns-open-gl-pfa-robust 75)
(defconstant ns-open-gl-pfa-backing-store 76)
(defconstant ns-open-gl-pfa-mp-safe 78)
(defconstant ns-open-gl-pfa-window 80)
(defconstant ns-open-gl-pfa-multi-screen 81)
(defconstant ns-open-gl-pfa-compliant 83)
(defconstant ns-open-gl-pfa-screen-mask 84)
(defconstant ns-open-gl-pfa-virtual-screen-count 128)

(defun ns-open-gl-pixel-format-attribute-type ()
  (if (> (floor (cocoa:ns-app-kit-version-number))
         cocoa:ns-app-kit-version-number-10_4)
      '(:unsigned :int)
    ':int))
  

(defun choose-cocoa-pixel-format (view configuration)
  "Returns the NSOpenGLPixelFormat for rep which supports the 
   requested configuration. Returns NIL if it fails.
   Configuration is a plist with the following allowed
   indicators: 
      :double-buffer, :double-buffered, - synonyms, value T or NIL."
  (fli:with-dynamic-foreign-objects ()
    (let* ((attributes-list
            (nconc (and (or (getf configuration :double-buffer)
                            (getf configuration :double-buffered))
                        (list ns-open-gl-pfa-double-buffer))
                   (let ((depth-buffer (getf configuration :depth-buffer)))
                     (and depth-buffer
                          (list ns-open-gl-pfa-depth-size depth-buffer)))
                   (list 0)))
           (attributes (fli:allocate-dynamic-foreign-object
                        :type (ns-open-gl-pixel-format-attribute-type)
                        :initial-contents attributes-list)))
      (let ((format (objc:invoke (objc:invoke "NSOpenGLPixelFormat" "alloc")
                                 "initWithAttributes:"
                                 attributes)))
        (if (objc:null-objc-pointer-p format)
            nil
          format)))))

(defun pixel-format-attribute-value (pixel-format
                                     attribute
                                     &optional screen)
  (fli:with-dynamic-foreign-objects ()
    (let ((value (fli:allocate-dynamic-foreign-object
                  :type (ns-open-gl-pixel-format-attribute-type))))
      (objc:invoke pixel-format "getValues:forAttribute:forVirtualScreen:"
                   value ; this is an out parameter
                   attribute
                   (or screen 0))
      (fli:dereference value))))

(defun descibe-cocoa-pixel-format (pixel-format)
  (list :double-buffer (not (zerop (pixel-format-attribute-value
                                    pixel-format
                                    ns-open-gl-pfa-double-buffer)))
        :depth-buffer (pixel-format-attribute-value
                       pixel-format
                       ns-open-gl-pfa-depth-size)
        ))
