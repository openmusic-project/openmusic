(in-package :alloy-desktop)

(defvar *desktop-screen* nil)

(defvar *iteration-error-signature* nil
  "Signature of the last event-loop iteration error reported in this session.")

(defun desktop-screen ()
  *desktop-screen*)

(defun call-with-desktop-session (function &optional (screen-class 'glfw:screen) &rest initargs)
  (check-type function function)
  (when *desktop-screen*
    (error "A desktop session is already active."))
  ;; popups and dialogs are placed in screen coordinates, which GLFW's Wayland
  ;; backend does not provide (glfw wl_window.c: _glfwGetWindowPosWayland)
  #+(or linux freebsd) (%glfw:init :platform :x11)
  (apply #'glfw::call-with-screen
         (lambda (screen)
           (setf *desktop-screen* screen
                 *iteration-error-signature* nil)
           (let ((owner (adopt-ui-thread)))
             (unwind-protect
                  (funcall function screen)
               ;; upstream call-with-screen keeps polling until the SCREEN is closed;
               ;; closing it here lets its event loop exit and glfw:shutdown run.
               (unwind-protect
                    (windowing:close screen)
                 (setf *desktop-screen* nil)
                 (release-ui-thread owner)))))
         screen-class initargs))

(defmacro with-desktop-session ((screen &optional (screen-class ''glfw:screen) &rest initargs)
                                &body body)
  `(call-with-desktop-session (lambda (,screen) ,@body)
                              ,screen-class ,@initargs))
