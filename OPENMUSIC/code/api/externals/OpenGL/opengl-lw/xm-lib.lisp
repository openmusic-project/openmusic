;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/xm-lib.lisp,v 1.25.3.1 2014/05/27 20:56:57 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")

;;; capi-motif-library back-end for OpenGL
;;; TBD: proper mechanism for sharing contexts.  Currently shared contexts don't ever
;;; get freed.
;;; PJG 9Feb1999 - Fixed code to cope with color::visual-info objects. Previously
;;;                it would use XVisualInfo (which could cause memory leaks)


(defun find-x11-visual-info (display screen configuration)
  (if-let (vi (x-utilities::with-x11-foreign-calls-lock-rm-only(call-glx-Choose-Visual display (x-lib:x-screen-number-of-screen screen) configuration)))
      (fli:copy-pointer vi :type 'x-lib:x-visual-info)
    (error "Unable to find a suitable visual for OpenGL configuration ~s" configuration)))

(defun display-connection-local-maybe (xdisplay)
  "Heuristic attempt at determining if a display connection is to
   a server on the same host as the client, which would allow
   OpenGL direct connections, which are much faster."
  (let ((host (x-lib::parse-display-name-string (x-lib::x-display-string xdisplay))))
    (not (null (member host (list "" "localhost" "unix" (machine-instance)) :test #'string=)))))


(defmethod %make-context ((widget xt-lib:widget) opengl-configuration)
  (let* ((xscreen (xt-lib:xt-screen widget))
         (xdisplay (x-lib:x-display-of-screen xscreen))
         (visual (xt-lib:xt-get-values widget :visual))
         (directp (if (eq (getf opengl-configuration :direct) :force)
                      T
                    (display-connection-local-maybe xdisplay))))
    (let ((context
           (x-utilities::with-x11-foreign-calls-lock-rm-only
             (call-glx-create-context xdisplay visual 
                                      (let ((share (getf opengl-configuration :share)))
                                        (if (and share (typep share 'glxcontext))
                                            share 
                                          nil))
                                      directp))))
      (unless context
        (error "Failed to create GLX Context for ~s: ~s ~s" widget xdisplay visual))
      context)))

(defmethod %start-rendering ((widget xt-lib:widget) context)
  ;; Returns boolean (success) value returned by glx-make-current
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

(defmethod %describe-visual-info-configuration ((widget xt-lib:widget) context visual
                                                &optional (stream *standard-output*))
  (let* ((xscreen (xt-lib:xt-screen widget))
         (xdisplay (x-lib:x-display-of-screen xscreen))
         (x-visual-info nil))
    (unwind-protect
        (or (x-utilities::with-x11-foreign-calls-lock-rm-only
              (and (setq x-visual-info (x-visual-info-from-visual xdisplay visual))
                   (let ((directp (glx-is-direct xdisplay context)))
                     (list* (format nil "Connection to the graphics subsystem is ~:[via the X server.~;direct.~]" directp)
                            (mapcar #'(lambda (attrib)
                                        (destructuring-bind (glx-attrib name descr) attrib
                                          (multiple-value-bind (error value)
                                              (glx-get-config xdisplay x-visual-info glx-attrib 0)
                                            (if (zerop error)
                                                (format nil "~@?" descr value)
                                              (format nil "Failed to get GL context attribute ~a" name)))))
                                    *glx-get-config-attributes*)))))
            (error "Failed to locate x-visual-info for : ~s" visual))
      (when x-visual-info
        (x-lib:x-free x-visual-info)))))
  

;;; ------------------------------------------------------------
;;; CAPI-MOTIF-LIBRARY specific


(defmethod cm-lib::color-requirements-resources ((pane opengl-pane) (rep cm-lib::output-pane-representation) color-reqs)
  color-reqs
  (with-slots (configuration) pane
    (if configuration
        (let* ((xdisplay (cm-lib::representation-x-display rep))
               (xscreen (cm-lib::representation-x-screen rep))
               (x-visual-info (find-x11-visual-info xdisplay xscreen configuration)))
          (when x-visual-info
            (let ((color-visual-info (color::set-visual-info-from-x-visual-info x-visual-info)))
              (prog1
                  (list :visual (fli:foreign-slot-value x-visual-info 'x-lib:visual)
                        :colormap (color::colormap-for-visual-info xscreen color-visual-info :name :opengl)
                        :depth (fli:foreign-slot-value x-visual-info 'x-lib:depth))
                (x-lib:x-free x-visual-info)))))
      (call-next-method))))
          
(defmethod %make-context ((rep cm-lib::output-pane-representation) opengl-configuration)
  (%make-context (cm-lib::representation-work-widget rep) opengl-configuration))

(defmethod %free-opengl-resources ((rep cm-lib::output-pane-representation) context)
  ;; called by the capi-internals:representation-destroy method.
  (when context
    (with-slots (configuration) (cm-lib::representation-capi-object rep)
      (unless (getf configuration :share)
        (x-utilities::with-x11-foreign-calls-lock-rm-only
          ;; Unsatisfactory. We need a way to share information about shared contexts.
          (let ((xdisplay (cm-lib::representation-x-display rep)))
            (glx-destroy-context xdisplay context)))))))

(defmethod %start-rendering ((rep cm-lib::output-pane-representation) context)
  (x-utilities::lock-x11-foreign-calls-lock)
  (let ((res nil))
    (unwind-protect 
        (setq res (%start-rendering (cm-lib::representation-work-widget rep) context))
      (unless res (x-utilities::unlock-x11-foreign-calls-lock)))
    res))
      


(defmethod %stop-rendering ((rep cm-lib::output-pane-representation))
  (unwind-protect (%stop-rendering (cm-lib::representation-work-widget rep))
    (x-utilities::unlock-x11-foreign-calls-lock)))

(defmethod %swap-buffers ((rep cm-lib::output-pane-representation) context)
  (x-utilities::with-x11-foreign-calls-lock-rm-only
    (%swap-buffers (cm-lib::representation-work-widget rep) context)))

(defmethod %resize-opengl-context ((rep cm-lib::output-pane-representation)
                                   context width height)
  (x-utilities::with-x11-foreign-calls-lock-rm-only
    (%resize-opengl-context (cm-lib::representation-work-widget rep)
                            context width height)))

(defmethod %describe-configuration ((rep cm-lib::output-pane-representation) context
                                    &optional (stream *standard-output*) collectp)
  (let ((results
         (let* ((widget (cm-lib::representation-work-widget rep))
                (visual (xt-lib:xt-get-values widget :visual)))
           (%describe-visual-info-configuration widget
                                                context
                                                visual
                                                stream))))

      (if collectp
          results
        (dolist (r results)
          (format stream "~%~a" r)))))


(defmethod %get-debug-entry-hook ((x cm-lib::output-pane-representation))
  'x-utilities::x11-foreign-calls-lock-debug2-hook)
