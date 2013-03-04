;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/xm-lib.lisp,v 1.18.1.1 2007/10/23 22:17:08 davef Exp $" -*-

;; Copyright (c) 1987--2008 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")

;;; capi-motif-library back-end for OpenGL
;;; TBD: proper mechanism for sharing contexts.  Currently shared contexts don't ever
;;; get freed.
;;; PJG 9Feb1999 - Fixed code to cope with color::visual-info objects. Previously
;;;                it would use XVisualInfo (which could cause memory leaks)

(defun indexed-colormap-p (configuration)
  (not (or (getf configuration :rgb) (getf configuration :rgba))))

(defun find-x11-visual-info (display screen configuration)
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
             (glx-Choose-Visual display (x-lib:x-screen-number-of-screen screen) vparams)))
        (when (fli:null-pointer-p visual-info)
        (error "Unable to find a suitable visual for OpenGL configuration ~s" configuration))
      visual-info))
    ))

(defun display-connection-local-maybe (xdisplay)
  "Heuristic attempt at determining if a display connection is to
   a server on the same host as the client, which would allow
   OpenGL direct connections, which are much faster."
  (let ((host (x-lib::parse-display-name-string (x-lib::x-display-string xdisplay))))
    (not (null (member host (list "" "localhost" "unix" (machine-instance)) :test #'string=)))))


(defmethod %make-context ((widget xt-lib:widget) opengl-configuration)
  (let* ((xscreen (xt-lib::xt-screen widget))
         (xdisplay (x-lib:x-display-of-screen xscreen))
         (visual (xt-lib:xt-get-values widget :visual))
         (visual-info (color::visual-info-for-visual xdisplay visual))
         (directp (if (eq (getf opengl-configuration :direct) :force)
                      T
                    (display-connection-local-maybe xdisplay))))
    (fli:with-dynamic-foreign-objects ((x-visual-info x-lib:x-visual-info))
      (if (color::get-x-visual-info-from-visual xdisplay visual-info x-visual-info)
          (let ((context
                 (glx-create-context xdisplay 
                                     x-visual-info
                                     (let ((share (getf opengl-configuration :share)))
                                       (if (and share (typep share 'glxcontext))
                                           share 
                                         nil))
                                     (if directp 1 0))))
            (unless context
              (error "Failed to create GLX Context for ~s: ~s ~s" widget xdisplay visual-info))
            context)
        (error "Failed to locate x-visual-info for : ~s" visual-info)))))

(defmethod %start-rendering ((widget xt-lib:widget) context)
  ;; Returns boolean (success) value returned by glx-make-current
  ;; This method is called within without-interrupts.
  (let ((xid (xt-lib:xt-window widget))
        (xdisplay (xt-lib:xt-display widget)))
    (glx-make-current xdisplay xid context)))

(defmethod %stop-rendering ((widget xt-lib:widget))
  (let ((xdisplay (xt-lib:xt-display widget)))
    (glx-make-current xdisplay x-lib:none nil)))

(defmethod %swap-buffers ((widget xt-lib:widget) context)
  (declare (ignore context))
  (let ((xid (xt-lib:xt-window widget))
        (xdisplay (xt-lib:xt-display widget)))
    (glx-swap-buffers xdisplay xid)))

(defmethod %resize-opengl-context ((rep xt-lib:widget)
                                   context width height)
  (declare (ignore context width height))
  )

(defmethod %describe-visual-info-configuration ((widget xt-lib:widget) context visual-info 
                                                &optional (stream *standard-output*))
  (let* ((xscreen (xt-lib::xt-screen widget))
         (xdisplay (x-lib:x-display-of-screen xscreen))
         (directp (glx-is-direct xdisplay context))
         (results (append 
                   (list (format nil "Connection to the graphics subsystem is ~:[via the X server.~;direct.~]" directp))
                   (mapcar #'(lambda (attrib)
                               (destructuring-bind (glx-attrib name descr) attrib
                                 (multiple-value-bind (error value)
                                     (glx-get-config xdisplay visual-info glx-attrib 0)
                                   (if (zerop error)
                                       (format nil "~@?" descr value)
                                     (cerror "continue" "Failed to get gl context attribute ~a for ~s" name widget)))))
                           *glx-get-config-attributes*))))
    results))
  

;;; ------------------------------------------------------------
;;; CAPI-MOTIF-LIBRARY specific

(defmethod cm-lib::color-requirements-resources ((pane capi::opengl-pane) (rep cm-lib::output-pane-representation) color-reqs)
  color-reqs
  (with-slots (configuration) pane
    (if configuration
      (let* ((xdisplay (cm-lib::representation-x-display rep))
             (xscreen (cm-lib::representation-x-screen rep))
             (visual-info (find-x11-visual-info xdisplay xscreen configuration))
             (color-visual-info (color::set-visual-info-from-x-visual-info visual-info)))
        (when visual-info
          (prog1
              (list :visual (fli:foreign-slot-value visual-info 'x-lib:visual)
                    :colormap (color::colormap-for-visual-info xscreen color-visual-info :name :opengl)
                    :depth (fli:foreign-slot-value visual-info 'x-lib:depth))
            (x-lib:x-free visual-info))))
      (call-next-method))))
          
(defmethod %make-context ((rep cm-lib::output-pane-representation) opengl-configuration)
  (%make-context (cm-lib::representation-work-widget rep) opengl-configuration))

(defmethod %free-opengl-resources ((rep cm-lib::output-pane-representation) context)
  ;; called by the capi-internals:representation-destroy method.
  (when context
    (with-slots (configuration) (cm-lib::representation-capi-object rep)
      (unless (getf configuration :share)
        ;; Unsatisfactory. We need a way to share information about shared contexts.
        (let ((xdisplay (cm-lib::representation-x-display rep)))
          (glx-destroy-context xdisplay context))))))

(defmethod %start-rendering ((rep cm-lib::output-pane-representation) context)
  (%start-rendering (cm-lib::representation-work-widget rep) context))

(defmethod %stop-rendering ((rep cm-lib::output-pane-representation))
  (%stop-rendering (cm-lib::representation-work-widget rep)))

(defmethod %swap-buffers ((rep cm-lib::output-pane-representation) context)
  (%swap-buffers (cm-lib::representation-work-widget rep) context))

(defmethod %resize-opengl-context ((rep cm-lib::output-pane-representation)
                                   context width height)
  (%resize-opengl-context (cm-lib::representation-work-widget rep)
                          context width height))

(defmethod %describe-configuration ((rep cm-lib::output-pane-representation) context
                                    &optional (stream *standard-output*) collectp)
  (let* ((widget (cm-lib::representation-work-widget rep))
         (display (cm-lib::representation-x-display rep))
         (visual (xt-lib:xt-get-values widget :visual))
         (visual-info (color::visual-info-for-visual display visual)))
    (let ((results 
           (%describe-visual-info-configuration widget
                                                context
                                                visual-info
                                                stream)))
      (x-lib:x-free visual-info)

      (if collectp
          results
        (dolist (r results)
          (format stream "~%~a" r))))))

