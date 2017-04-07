;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/xfns.lisp,v 1.6.3.1 2014/05/27 20:56:57 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")


;; Names for attributes to glXGetConfig.
(defconstant *GLX-USE-GL*		1)	;; support GLX rendering */
(defconstant *GLX-BUFFER-SIZE*		2)	;; depth of the color buffer */
(defconstant *GLX-LEVEL*		3)	;; level in plane stacking */
(defconstant *GLX-RGBA*		        4)	;; true if RGBA mode */
(defconstant *GLX-DOUBLEBUFFER*	        5)	;; double buffering supported */
(defconstant *GLX-STEREO*		6)	;; stereo buffering supported */
(defconstant *GLX-AUX-BUFFERS* 	        7)	;; number of aux buffers */
(defconstant *GLX-RED-SIZE*		8)	;; number of red component bits */
(defconstant *GLX-GREEN-SIZE*		9)	;; number of green component bits */
(defconstant *GLX-BLUE-SIZE*		10)	;; number of blue component bits */
(defconstant *GLX-ALPHA-SIZE*		11)	;; number of alpha component bits */
(defconstant *GLX-DEPTH-SIZE*		12)	;; number of depth bits */
(defconstant *GLX-STENCIL-SIZE*	        13)	;; number of stencil bits */
(defconstant *GLX-ACCUM-RED-SIZE*	14)	;; number of red accum bits */
(defconstant *GLX-ACCUM-GREEN-SIZE*	15)	;; number of green accum bits */
(defconstant *GLX-ACCUM-BLUE-SIZE*	16)	;; number of blue accum bits */
(defconstant *GLX-ACCUM-ALPHA-SIZE*	17)	;; number of alpha accum bits */
(defconstant *glx-get-config-attributes* `((,*GLX-USE-GL*      *GLX-USE-GL* "~[Does NOT support~;Supports~] OpenGL rendering")
                                           (,*GLX-BUFFER-SIZE* *GLX-BUFFER-SIZE* "Color buffer size : ~d")
                                           (,*GLX-LEVEL* *GLX-LEVEL* "Frame buffer level : ~d")
                                           (,*GLX-RGBA* *GLX-RGBA* "Uses ~[Color Index~;RGBA~] mode")
                                           (,*GLX-DOUBLEBUFFER* *GLX-DOUBLEBUFFER* "Is ~[single~;double~]-buffered")
                                           (,*GLX-STEREO* *GLX-STEREO* "~[Does not support~;Supports~] stereo buffers")
                                           (,*GLX-AUX-BUFFERS* *GLX-AUX-BUFFERS* "Has ~d aux buffers")
                                           (,*GLX-RED-SIZE* *GLX-RED-SIZE* "Has ~d bits in the red component")
                                           (,*GLX-GREEN-SIZE* *GLX-GREEN-SIZE* "Has ~d bits in the green component")
                                           (,*GLX-BLUE-SIZE* *GLX-BLUE-SIZE* "Has ~d bits in the blue component")
                                           (,*GLX-ALPHA-SIZE* *GLX-ALPHA-SIZE* "Has ~d bits in the alpha component")
                                           (,*GLX-DEPTH-SIZE* *GLX-DEPTH-SIZE* "Depth buffer size = ~d bits")
                                           (,*GLX-STENCIL-SIZE* *GLX-STENCIL-SIZE* "Has ~d stencil bits")
                                           (,*GLX-ACCUM-RED-SIZE* *GLX-ACCUM-RED-SIZE* "Accumulation buffer red channel is ~d bits")
                                           (,*GLX-ACCUM-GREEN-SIZE* *GLX-ACCUM-GREEN-SIZE* "Accumulation buffer green channel is ~d bits")
                                           (,*GLX-ACCUM-BLUE-SIZE* *GLX-ACCUM-BLUE-SIZE* "Accumulation buffer blue channel is ~d bits")
                                           (,*GLX-ACCUM-ALPHA-SIZE* *GLX-ACCUM-ALPHA-SIZE* "Accumulation buffer alpha channel is ~d bits")))
;;
;; Error return values from glXGetConfig.  Success is indicated by
;; a value of 0.

(defconstant *GLX-BAD-SCREEN*           1)	;; screen # is bad */
(defconstant *GLX-BAD-ATTRIBUTE*	2)	;; attribute to get is bad */
(defconstant *GLX-NO-EXTENSION*	        3)	;; no glx extension on server */
(defconstant *GLX-BAD-VISUAL*		4)	;; visual # not known by GLX */

(fli:define-c-typedef (xid (:foreign-name "XID"))
    (:unsigned :long))


(fli:define-c-typedef (visual (:foreign-name "Visual"))
  (:struct
   (ext-data :pointer)
   (visualid xid)
   (class :int)
   (red-mask (:unsigned :long))
   (green-mask (:unsigned :long))
   (blue-mask (:unsigned :long))
   (bits-per-rgb :int)
   (map-entries :int)))

(fli:define-c-typedef (x-visual-info (:foreign-name "XVisualInfo"))
  (:struct
   (visual (:pointer visual))
   (visualid xid)
   (screen-number :int)
   (depth :int)
   (class :int)
   (red-mask (:unsigned :long))
   (green-mask (:unsigned :long))
   (blue-mask (:unsigned :long))
   (colormap-size :int)
   (bits-per-rgb :int)))




;;; because this file is used for xm-lib and gtk-lib, the actual type of
;;; the pointer ae are going to get is different. So we use this
;;; definitions to accept any pointer, 

(fli:define-foreign-type x-display-pointer () :pointer)
(fli:define-foreign-type x-visual-info-pointer () '(:pointer x-visual-info))
(fli:define-foreign-type x-font-pointer () :pointer)

(fli:define-c-typedef (glxcontext-id (:foreign-name "GLXContextID"))
  xid)

(fli:define-c-typedef (glxpixmap (:foreign-name "GLXPixmap"))
  xid)

(fli:define-c-typedef (glxdrawable (:foreign-name "GLXDrawable"))
  xid)

(fli:define-c-typedef (glxcontext (:foreign-name "GLXContext"))
  (:pointer (:struct --glxcontext-rec)))


(fli:define-foreign-function (glx-choose-visual "glXChooseVisual" :source)
    ((dpy x-display-pointer)
     (screen-number :int)
     (attrib-list (gl-vector :signed-32)))
  :result-type  x-visual-info-pointer
  :language :ansi-c)

(fli:define-foreign-function (glx-copy-context "glXCopyContext" :source)
    ((dpy x-display-pointer)
     (src glxcontext)
     (dst glxcontext)
     (mask gluint))
  :language :ansi-c)

(fli:define-foreign-function (glx-create-context "glXCreateContext" :source)
    ((dpy x-display-pointer)
     (vis x-visual-info-pointer)
     (share-list glxcontext)
     (direct :boolean))
  :result-type (:wrapper glxcontext
                :foreign-to-lisp (lambda (x) (if (fli:null-pointer-p x) nil x)))
  :language :ansi-c)

(fli:define-foreign-function (glx-create-glxpixmap "glXCreateGLXPixmap" :source)
    ((dpy x-display-pointer)
     (vis x-visual-info-pointer)
     (pixmap xid))
  :result-type glxpixmap
  :language :ansi-c)

(fli:define-foreign-function (glx-destroy-context "glXDestroyContext" :source)
    ((dpy x-display-pointer)
     (ctx glxcontext))
  :language :ansi-c)

(fli:define-foreign-function (glx-destroy-glxpixmap "glXDestroyGLXPixmap" :source)
    ((dpy x-display-pointer)
     (pix glxpixmap))
  :language :ansi-c)

(fli:define-foreign-function (glx-get-config "glXGetConfig" :source)
    ((dpy x-display-pointer)
     (visual-info x-visual-info-pointer)
     (attribute (:signed :int))
     (value (:reference-return (:signed :int))))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glx-get-current-context "glXGetCurrentContext" :source) ()
  :result-type glxcontext
  :language :ansi-c)

(fli:define-foreign-function (glx-get-current-drawable "glXGetCurrentDrawable" :source) ()
  :result-type glxdrawable
  :language :ansi-c)

(fli:define-foreign-function (glx-is-direct "glXIsDirect" :source)
    ((dpy x-display-pointer) 
     (ctx glxcontext))
  :result-type :boolean
  :language :ansi-c)

(fli:define-foreign-function (glx-make-current "glXMakeCurrent" :source)
    ((dpy x-display-pointer)
     (drawable glxdrawable)
     (ctx glxcontext))
  :result-type :boolean
  :language :ansi-c)

(fli:define-foreign-function (glx-query-extension "glXQueryExtension" :source)
    ((dpy x-display-pointer)
     (error-base (:pointer (:signed :int)))
     (event-base (:pointer (:signed :int))))
  :result-type :boolean
  :language :ansi-c)

(fli:define-foreign-function (glx-query-version "glXQueryVersion" :source)
    ((dpy x-display-pointer)
     (major (:pointer (:signed :int)))
     (minor (:pointer (:signed :int))))
  :result-type :boolean
  :language :ansi-c)

(fli:define-foreign-function (glx-swap-buffers "glXSwapBuffers" :source)
    ((dpy x-display-pointer)
     (drawable glxdrawable))
  :language :ansi-c)

(fli:define-foreign-function (glx-use-xfont "glXUseXFont" :source) 
    ((font x-font-pointer)
     (first  (:signed :int))
     (count (:signed :int))
     (list-base (:signed :int)))
  :language :ansi-c)

(fli:define-foreign-function (glx-wait-gl "glXWaitGL" :source) ()
  :language :ansi-c)

(fli:define-foreign-function (glx-wait-x "glXWaitX" :source) ()
  :language :ansi-c)

(fli:define-foreign-function (glx-query-extensions-string "glXQueryExtensionsString" :source)
    ((dpy x-display-pointer) 
     (screen :int))
  :result-type (:pointer (:const :char))
  :language :ansi-c)

(fli:define-foreign-function (glx-get-client-string "glXGetClientString" :source)
    ((dpy x-display-pointer)
     (name (:signed :int)))
  :result-type (:pointer (:const :char))
  :language :ansi-c)

(fli:define-foreign-function (glx-query-server-string "glXQueryServerString" :source)
    ((dpy x-display-pointer)
     (screen :int)
     (name (:signed :int)))
  :result-type (:pointer (:const :char))
  :language :ansi-c)

(fli:define-foreign-function (glx-get-video-sync-sgi "glXGetVideoSyncSGI" :source)
    ((count (:pointer (:unsigned :int))))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glx-wait-video-sync-sgi "glXWaitVideoSyncSGI" :source)
    ((divisor (:signed :int))
     (remainder (:signed :int))
     (count (:pointer (:unsigned :int))))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glx-swap-interval-sgi "glXSwapIntervalSGI" :source)
    ((interval (:signed :int)))
  :result-type (:signed :int)
  :language :ansi-c)

(fli:define-foreign-function (glx-swap-mode-sgi "glXSwapModeSGI" :source)
    ((mode (:signed :int))
     (params (:pointer (:signed :int))))
  :result-type (:signed :int)
  :language :ansi-c)


(fli:define-foreign-function (x-free "XFree" :source)
    ((data :pointer))
  :language :c)





(fli:define-foreign-function (%x-get-visual-info "XGetVisualInfo" :source)
    ((display x-display-pointer)
     (vinfo-mask :long)
     (vinfo-template x-visual-info-pointer)
     (:ignore (:reference-return :int))) ;nitems-return
  :result-type x-visual-info-pointer
  :language :c)

;;; Caller needs to x-free the result (if non-nil)

(defun x-visual-info-from-visual (display visual)
  (fli:with-coerced-pointer (visual-poi :type 'visual)
      visual
    (let ((id (fli:foreign-slot-value visual-poi 'visualid)))
      (multiple-value-bind (x-info num)
          (fli:with-dynamic-foreign-objects ((visual-info-template x-visual-info))
            (setf (fli:foreign-slot-value visual-info-template 'visualid)
                  id)
            (%x-get-visual-info display 
                                1  ;;;; VisualIDMask               
                                visual-info-template))
        (if (= num 1)
            x-info
          nil)))))
              

(defun call-glx-create-context (xdisplay visual share directp)
  (when-let (xvi (x-visual-info-from-visual xdisplay visual))
    (prog1 
        (glx-create-context xdisplay xvi share  directp)
      (x-free xvi))))



(defun indexed-colormap-p (configuration)
  (not (or (getf configuration :rgb) (getf configuration :rgba))))

;;; Note: This is called inside the library lock.

(defun call-glx-Choose-Visual (display screen-number configuration)
  "Return a visual info structure which supports the 
   requested configuration. Configuration is a plist with the following allowed
   indicators: 
      :double-buffer, :double-buffered, - synonyms, value T or NIL.
      :buffer-size - color buffer size for indexed colormap visuals
      :red-size, :green-size, :blue-size, :alpha-size - sizes of color buffer channels
                                                        for RGB visuals.
      :accum - accumulator buffer size (per channel), or NIL.
      :accum-red-size, accum-green-size, accum-blue-size, accum-alpha-size -
          sizes of accumulator buffer channels, which default to the :accum value.
      :depth-buffer - value is a depth buffer size or NIL
      :stencil-size - stencil buffer size or NIL.
      :aux - aux buffer size or NIL."
  (let ((params
	 (append
	  (when (or (getf configuration :double-buffer) (getf configuration :double-buffered))
	    (list *GLX-DOUBLEBUFFER*))
	  (if (indexed-colormap-p configuration)
	      (list *GLX-BUFFER-SIZE* (getf configuration :buffer-size 1))
	    (append (list *GLX-RGBA*
			  *GLX-RED-SIZE* (getf configuration :red-size 1)
			  *GLX-GREEN-SIZE* (getf configuration :green-size 1)
			  *GLX-BLUE-SIZE* (getf configuration :blue-size 1))
		    (when (getf configuration :alpha-size)
		      (list *GLX-ALPHA-SIZE* (getf configuration :alpha-size)))
                    (let ((accum (getf configuration :accum)))
		      (when accum
		        (list *GLX-ACCUM-RED-SIZE* (getf configuration :accum-red-size accum)
			      *GLX-ACCUM-GREEN-SIZE* (getf configuration :accum-green-size accum)
			      *GLX-ACCUM-BLUE-SIZE* (getf configuration :accum-blue-size accum)
			      *GLX-ACCUM-ALPHA-SIZE* (getf configuration :accum-alpha-size accum))))))
	  (let ((depth (getf configuration :depth-buffer)))
	    (when depth
	      (list *GLX-DEPTH-SIZE* depth)))
	  (let ((stencil (getf configuration :stencil-size)))
	    (when stencil
	      (list *GLX-STENCIL-SIZE* stencil)))
          (let ((aux (getf configuration :aux 0)))
            (list *GLX-AUX-BUFFERS* aux))
	  (list 0))))

      (fli:with-dynamic-foreign-objects ()
        (let* ((vparams #+:use-fli-gl-vector
                        (fli:allocate-dynamic-foreign-object :type '(:signed :int) :initial-contents params)
                        #-:use-fli-gl-vector
                        (sys:in-static-area
                          (make-array (length params) :element-type '(signed-byte 32)
                                      :initial-contents params)))
               (visual-info
                (glx-Choose-Visual display screen-number vparams)))
          (if (fli:null-pointer-p visual-info)
              nil
            visual-info))
    )))
