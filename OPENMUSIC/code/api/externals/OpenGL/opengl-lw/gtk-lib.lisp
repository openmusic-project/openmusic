;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/gtk-lib.lisp,v 1.5.3.1 2014/05/27 20:56:57 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.

(in-package "OPENGL")

(defun gtk-representation-x-display (rep)
  (let* ((screen-rep (cg-lib::representation-screen-representation rep))
         (display-rep  (cg-lib::screen-representation-display screen-rep)))
    (fli:make-pointer :address
                      (lwgtk:gdk-x11-display-get-display-address
                       (cg-lib::display-representation-display display-rep)))))


(defmethod %make-context ((representation cg-lib::widget-representation) opengl-configuration)
  (when-let (SETUP-GLX-COLOR-MAP-error (capi:capi-object-property (cg-lib::representation-element representation)
                                                                  'SETUP-GLX-COLOR-MAP-error))
    (error "%MAKE-CONTEXT failed for ~s because SETUP-GLX-COLOR-MAP failed : ~a" 
           representation SETUP-GLX-COLOR-MAP-error))
  (when-let (drawable (gp::gdk-port-gdkdrawable representation))
    (let* ((screen-rep (cg-lib::representation-screen-representation representation))
           (display-rep  (cg-lib::screen-representation-display screen-rep))
           (directp (if (eq (getf opengl-configuration :direct) :force)
                        T
                      (let ((host (cg-lib::display-representation-host display-rep)))
                        (or (not host)
                            (string= host "localhost")
                            (string= host "unix")
                            (string= host "")
                            (string= host (machine-instance)))))))
      (let ((context
             (lwgtk:with-gdk-locked ()
               (let ((xdisplay (fli:make-pointer :address
                                                 (lwgtk:gdk-x11-display-get-display-address
                                                  (cg-lib::display-representation-display display-rep))))
                     (visual (lwgtk:gdk-x11-visual-get-xvisual 
                              (lwgtk:gdk-drawable-get-visual drawable))))
                 (call-glx-create-context xdisplay visual 
                                          (let ((share (getf opengl-configuration :share)))
                                            (if (and share (typep share 'glxcontext))
                                                share 
                                              nil))
                                          directp)))))
        (unless context
          (error "Failed to create GLX Context for ~s" representation))
        context))))

  


(defmethod %free-opengl-resources ((rep cg-lib::output-pane-representation) context)
  ;; called by the capi-internals:representation-destroy method.
  (when context
    (with-slots (configuration) (cg-lib::representation-element rep)
      (unless (getf configuration :share)
        (lwgtk:with-gdk-locked()
          ;; Unsatisfactory. We need a way to share information about shared contexts.
          (let ((xdisplay (gtk-representation-x-display rep)))
            (glx-destroy-context xdisplay context)))))))

(defmethod %start-rendering ((rep cg-lib::output-pane-representation) context)
  (when (gp::gdk-port-gdkdrawable rep)
    (lwgtk:lock-gdk-lock)
    (let ((xid (lwgtk:gdk-x11-drawable-get-xid (gp::gdk-port-gdkdrawable rep)))
                (xdisplay (gtk-representation-x-display rep)))
      (let ((cg-lib::*last-x-error-string* nil))
        (values (glx-make-current xdisplay xid context)
                cg-lib::*last-x-error-string*)))))
      


(defmethod %stop-rendering ((rep cg-lib::output-pane-representation))
  (unwind-protect 
      (let ((xdisplay (gtk-representation-x-display rep)))
        (glx-make-current xdisplay 0 nil)) ;; 0 is none 
    (lwgtk:unlock-gdk-lock)))


(defmethod %swap-buffers ((rep cg-lib::output-pane-representation) context)
  (declare (ignore context))
  (lwgtk:with-gdk-locked ()
    (let ((xid (lwgtk:gdk-x11-drawable-get-xid (gp::gdk-port-gdkdrawable rep)))
          (xdisplay (gtk-representation-x-display rep)))
      (glx-swap-buffers xdisplay xid))))

(defmethod %resize-opengl-context ((rep cg-lib::output-pane-representation)
                                   context width height)
  #+do-nothing
  (lwgtk:with-gdk-locked()))



(defmethod %describe-configuration ((rep cg-lib::widget-representation) context
                                    &optional (stream *standard-output*) collectp)
  (let ((results
         (lwgtk:with-gdk-locked ()
           (let* ((screen-rep (cg-lib::representation-screen-representation rep))
                  (display-rep  (cg-lib::screen-representation-display screen-rep))
                  (xdisplay (fli:make-pointer :address
                                              (lwgtk:gdk-x11-display-get-display-address
                                               (cg-lib::display-representation-display display-rep))))
                  
                  (visual (lwgtk:gdk-x11-visual-get-xvisual 
                           (lwgtk:gdk-drawable-get-visual (gp::gdk-port-gdkdrawable rep))))
                  (x-visual-info (x-visual-info-from-visual xdisplay visual)))
             (prog1 
                 (let ((directp (glx-is-direct xdisplay context)))
                   (list* (format nil "Connection to the graphics subsystem is ~:[via the X server.~;direct.~]" directp)
                          (mapcar #'(lambda (attrib)
                                      (destructuring-bind (glx-attrib name descr) attrib
                                        (multiple-value-bind (error value)
                                            (glx-get-config xdisplay x-visual-info glx-attrib 0)
                                          (if (zerop error)
                                              (format nil "~@?" descr value)
                                            (format nil "Failed to get GL context attribute ~a" name)))))
                                  *glx-get-config-attributes*)))
               (x-free x-visual-info))))))

      (if collectp
          results
        (dolist (r results)
          (format stream "~%~a" r)))))


(defmethod %get-debug-entry-hook ((x cg-lib::output-pane-representation))
  'lwgtk:gdk-lock-debug2-hook)

;;; Returns an error-string if failed rather than error, so can 
;; be used inside lock. 

(defun setup-glx-color-map (rep pane widget)
  (let* ((screen-rep (cg-lib::representation-screen-representation rep))
         (display-rep  (cg-lib::screen-representation-display screen-rep))
         (xdisplay (fli:make-pointer :address
                                     (lwgtk:gdk-x11-display-get-display-address
                                      (cg-lib::display-representation-display display-rep))))
         (gdk-screen (cg-lib::screen-representation-screen screen-rep))
         (screen-number (lwgtk:gdk-screen-get-number gdk-screen))
         (configuration (slot-value pane 'configuration))
         
         )
    (when-let (error
               (if-let  (xvi (call-glx-choose-visual xdisplay screen-number configuration))
                   (prog1
                       (if-let (gdk-visual (lwgtk:gdk-x11-screen-lookup-visual 
                                            gdk-screen 
                                            (fli::foreign-slot-value xvi 'visualid)))
                           (let ((color-map (lwgtk:gdk-colormap-new gdk-visual nil)))
                             (lwgtk:gtk-widget-set-colormap widget
                                                            color-map)
                             (lwgtk:g-object-unref color-map)
                             nil)  ;;; no error
                         "failed to lookup visual")
                     (x-free xvi))
                 "glXChooseVisual failed"))
      (format nil
              "~a for display ~a.~d with configuration ~s"
              error
              (cg-lib::display-representation-display-spec display-rep)
              screen-number 
              configuration))))




(defmethod cg-lib::create-main-widget :around  ((rep cg-lib::output-pane-representation)
                                               (pane opengl-pane)
                                               parent-widget)
  (let ((widget (call-next-method)))
    (when-let (err-string (setup-glx-color-map rep pane widget))
      (setf (capi:capi-object-property pane 'SETUP-GLX-COLOR-MAP-error) err-string) ;; block %make-context
      (cg-lib::gtk-error err-string))
    widget))
