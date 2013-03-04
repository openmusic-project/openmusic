;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/xfns.lisp,v 1.4.6.1 2007/10/23 22:17:07 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

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


(fli:define-c-typedef (glxcontext-id (:foreign-name "GLXContextID"))
  x-lib:xid)

(fli:define-c-typedef (glxpixmap (:foreign-name "GLXPixmap"))
  x-lib:xid)

(fli:define-c-typedef (glxdrawable (:foreign-name "GLXDrawable"))
  x-lib:xid)

(fli:define-c-typedef (glxcontext (:foreign-name "GLXContext"))
  (:pointer (:struct --glxcontext-rec)))


(fli:define-foreign-function (glx-choose-visual "glXChooseVisual" :source)
    ((dpy x-lib:display)
     (screen-number (:wrapper :int
                     :lisp-to-foreign (lambda (scr) 
                                        (x-lib::check-valid-screen-number scr dpy)
                                        scr)))
     (attrib-list (gl-vector :signed-32)))
  :result-type (:pointer x-lib:x-visual-info)
  :language :ansi-c)

(fli:define-foreign-function (glx-copy-context "glXCopyContext" :source)
    ((dpy x-lib:display)
     (src glxcontext)
     (dst glxcontext)
     (mask gluint))
  :language :ansi-c)

(fli:define-foreign-function (glx-create-context "glXCreateContext" :source)
    ((dpy x-lib:display)
     (vis (:pointer x-lib:x-visual-info))
     (share-list glxcontext)
     (direct x-lib:bool))
  :result-type (:wrapper glxcontext
                :foreign-to-lisp (lambda (x) (if (fli:null-pointer-p x) nil x)))
  :language :ansi-c)

(fli:define-foreign-function (glx-create-glxpixmap "glXCreateGLXPixmap" :source)
    ((dpy x-lib:display)
     (vis (:pointer x-lib:x-visual-info))
     (pixmap x-lib:pixmap))
  :result-type glxpixmap
  :language :ansi-c)

(fli:define-foreign-function (glx-destroy-context "glXDestroyContext" :source)
    ((dpy x-lib:display)
     (ctx glxcontext))
  :language :ansi-c)

(fli:define-foreign-function (glx-destroy-glxpixmap "glXDestroyGLXPixmap" :source)
    ((dpy x-lib:display)
     (pix glxpixmap))
  :language :ansi-c)

(fli:define-foreign-function (glx-get-config "glXGetConfig" :source)
    ((dpy x-lib:display)
     (visual-info (:pointer x-lib:x-visual-info))
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
    ((dpy x-lib:display) 
     (ctx glxcontext))
  :result-type x-lib:bool
  :language :ansi-c)

(fli:define-foreign-function (glx-make-current "glXMakeCurrent" :source)
    ((dpy x-lib:display)
     (drawable glxdrawable)
     (ctx glxcontext))
  :result-type x-lib:bool
  :language :ansi-c)

(fli:define-foreign-function (glx-query-extension "glXQueryExtension" :source)
    ((dpy x-lib:display)
     (error-base (:pointer (:signed :int)))
     (event-base (:pointer (:signed :int))))
  :result-type x-lib:bool
  :language :ansi-c)

(fli:define-foreign-function (glx-query-version "glXQueryVersion" :source)
    ((dpy x-lib:display)
     (major (:pointer (:signed :int)))
     (minor (:pointer (:signed :int))))
  :result-type x-lib:bool
  :language :ansi-c)

(fli:define-foreign-function (glx-swap-buffers "glXSwapBuffers" :source)
    ((dpy x-lib:display)
     (drawable glxdrawable))
  :language :ansi-c)

(fli:define-foreign-function (glx-use-xfont "glXUseXFont" :source) 
    ((font x-lib:font)
     (first  (:signed :int))
     (count (:signed :int))
     (list-base (:signed :int)))
  :language :ansi-c)

(fli:define-foreign-function (glx-wait-gl "glXWaitGL" :source) ()
  :language :ansi-c)

(fli:define-foreign-function (glx-wait-x "glXWaitX" :source) ()
  :language :ansi-c)

(fli:define-foreign-function (glx-query-extensions-string "glXQueryExtensionsString" :source)
    ((dpy x-lib:display) 
     (screen (:wrapper :int
              :lisp-to-foreign (lambda (scr) 
                                 (x-lib::check-valid-screen-number scr dpy)
                                 scr))))
  :result-type (:pointer (:const :char))
  :language :ansi-c)

(fli:define-foreign-function (glx-get-client-string "glXGetClientString" :source)
    ((dpy x-lib:display)
     (name (:signed :int)))
  :result-type (:pointer (:const :char))
  :language :ansi-c)

(fli:define-foreign-function (glx-query-server-string "glXQueryServerString" :source)
    ((dpy x-lib:display)
     (screen (:wrapper :int
              :lisp-to-foreign (lambda (scr) 
                                 (x-lib::check-valid-screen-number scr dpy)
                                 scr)))
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

