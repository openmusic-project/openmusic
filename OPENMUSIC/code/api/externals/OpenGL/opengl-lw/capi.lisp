;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/61/LISPopengl/RCS/capi.lisp,v 1.29.1.1 2014/05/27 20:56:57 davef Exp $" -*-

;; Copyright (c) 1987--2015 LispWorks Ltd. All rights reserved.


(in-package "OPENGL")



(defparameter *open-gl-debug* nil)

(defun debug-print (format-string &rest args)
  (when *open-gl-debug*
    (format *terminal-io* "~?" format-string args)))

(defvar *default-opengl-pane-configuration* (list :rgba t))

(defclass opengl-pane (capi:output-pane)
  ((configuration :initform *default-opengl-pane-configuration*
                  :initarg :configuration
                  :reader configuration)
   (context :initform nil :initarg :context :accessor context)
   (render-state :initform nil :accessor opengl-pane-render-state)))
  

(defun ensure-context (opengl-pane)
  (or (context opengl-pane)
      (setf (context opengl-pane)
            (%make-context (capi-internals:representation opengl-pane)
                           (configuration opengl-pane)))))
  
(defmethod swap-buffers ((opengl-pane opengl-pane))
  (%swap-buffers (capi-internals:representation opengl-pane)
                 (context opengl-pane)))

(defmethod describe-configuration ((opengl-pane opengl-pane)
                                   &optional
                                   (stream *standard-output*)
                                   collectp)
  (%describe-configuration (capi-internals:representation opengl-pane)
                           (context opengl-pane)
                           stream collectp))

;;; ------------------------------------------------------------
;;; Locking
;;; Only one GL context can be current at any time, so we need a
;;; locking mechanism to stop threads interfering with each other.

(defvar *current-opengl-window* nil)

(defvar *opengl-window-lock* (mp:make-lock :name "OpenGL"))

(defun current-process-has-openGL-lock-p ()
  (mp:lock-owned-by-current-process-p *opengl-window-lock*))

(defun process-has-openGL-lock-p (&optional (process nil p-p))
  (if p-p
      (eq process (process-with-openGL-lock))
    (current-process-has-openGL-lock-p)))

(defun process-with-openGL-lock ()
  (mp:lock-owner *opengl-window-lock*))

(defvar *rendering-on-debug-level* 0)

(defmethod start-rendering ((opengl-pane opengl-pane))
  "Cause future OpenGL rendering calls to go to this window.
   This function takes care of MP locking."
  (when-let (rep (capi-internals:representation opengl-pane))
    (unless (eq (opengl-pane-render-state opengl-pane) :error)
      (when-let (context (ensure-context opengl-pane))
        ;; If the process already has the lock, it may be a recursive call or a reentrant call.
        ;; Recursive calls are okay.  Reentrant call arise only when the previous call failed to
        ;; set the *current-opengl-window* which means an error occurred, so just return.
        (if (current-process-has-openGL-lock-p)
            (cond ((null *current-opengl-window*)
                   (debug-print "OpenGL error: Reentrant call to start-rendering in process: ~s" rep)
                   (return-from start-rendering (values nil)))
                  ((not (eq *current-opengl-window* rep))
                   (debug-print "OpenGL error: Nested call to start-rendering on different window: current : ~s, requested ~s" 
                                *current-opengl-window* rep)
                   (return-from start-rendering (values nil)))     

                  ((/= *rendering-on-debug-level*  (dbg:get-debug-level))
                   ;;; entered the debugger while rendering, do't try to
                   ;;; render. 
                   nil)
                  (t t))
    
          (let ((res nil)
                error
                (%start-rendering-returned nil))
            (mp:process-lock *opengl-window-lock*)
            (unwind-protect-blocking-interrupts-in-cleanups
                (progn (multiple-value-setq (res error) (%start-rendering rep context))
                  (setq %start-rendering-returned t)
                  (when res
                    (setf (opengl-pane-render-state opengl-pane) t)
                    (setq res :lock *current-opengl-window* rep)))
              (unless res
                (%stop-rendering rep)
                (when error 
                  (setf (opengl-pane-render-state opengl-pane) :error))
                (unless %start-rendering-returned
                  (setf (context opengl-pane) nil))
                (mp:process-unlock *opengl-window-lock*)))
            (unless res 
              (if error
                  (error "Error when trying to render OPENGL: ~s" 
                         error)
                (debug-print "openGL error: Failed to set current OpenGL context for ~s" rep)))
            res))))))

(defmethod stop-rendering ((opengl-pane opengl-pane))
  (%stop-rendering (capi-internals:representation opengl-pane))
  (setf *current-opengl-window* nil)
  (mp:process-unlock *opengl-window-lock*))

;;; The hook takes one argument, the condition.

(defmethod %get-debug-entry-hook ((x t))
  'identity)


(defun get-debug-entry-hook (pane)
  (%get-debug-entry-hook (capi-internals:representation pane)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro rendering-on ((opengl-pane) &body body)
  (let ((lock (gensym)))
    (rebinding (opengl-pane)
      `(let (,lock)
         (unwind-protect-blocking-interrupts-in-cleanups
          (progn 
            (setf ,lock (start-rendering ,opengl-pane))
            (when ,lock                        ; lock is :lock or T. T when nested rendering on same window
              (let ((*rendering-on-debug-level*  (dbg:get-debug-level)))
                (dbg:with-added-debugger-entry-hook
                    (get-debug-entry-hook ,opengl-pane)
                  ,@body))))
          (when (eq ,lock :lock)  ; when start-rendering returned the lock, unlock.
            (stop-rendering ,opengl-pane))))))))

(defun release-opengl-pane-context (opengl-pane)
  (when-let (rep (capi-internals:representation opengl-pane))
    (%free-opengl-resources rep
                            (shiftf (context opengl-pane) nil))))

;; This is a primary method to come after the output-pane-destroy-callback.
(defmethod capi-internals:representation-destroy ((opengl-pane opengl-pane))
  (release-opengl-pane-context opengl-pane)
  (setf (opengl-pane-render-state opengl-pane) nil)
  (call-next-method))

(defmethod capi::output-pane-resize :before ((opengl-pane opengl-pane)
                                             x y width height)
  (%resize-opengl-context (capi-internals:representation opengl-pane)
                          (context opengl-pane)
                          width height))

(defmacro with-matrix-pushed (&body body)
  `(unwind-protect
       (progn
         (gl-push-matrix)
         ,@body)
     (gl-pop-matrix)))
  

#|| Example of basic interface:
(capi:define-interface opengl-interface ()
  ()
  (:panes (opengl opengl-pane :configuration (list :rgba t)))
  (:layouts 
   (main capi:column-layout '(opengl) :default t)))
||#
