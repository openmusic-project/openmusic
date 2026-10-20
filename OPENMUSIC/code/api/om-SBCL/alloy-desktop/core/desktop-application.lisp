(in-package :alloy-desktop)

;;; The shell an application needs around its UI thread: private bindings, float
;;; traps, the desktop session and a clean session state. It loads after every
;;; definer it calls (the registry, focus, modal, graphics, font and cue
;;; facilities), so it sits behind graphics/font in alloy-desktop.asd.

(defun call-with-desktop-application (function &key initial-bindings armed-traps)
  "Run FUNCTION on the screen of a fresh desktop session, on the calling thread, which becomes the UI thread.
The thread takes a private binding of each INITIAL-BINDINGS special, valued as it is at this call, and exactly ARMED-TRAPS enabled; the traps it had are restored on exit. The session starts with no window, focus, modal or graphics state left by a previous one, reads the native fonts and the keyboard-cue option once, holds a hidden share partner (*SHARE-WINDOW*) for its whole life, and closes every window, then the partner, when FUNCTION returns."
  (check-type function function)
  (progv initial-bindings (mapcar #'symbol-value initial-bindings)
    (let ((traps (armed-float-traps)))
      (unwind-protect
           (progn
             (set-float-traps armed-traps)
             (with-desktop-session (screen)
               (clear-window-registry)
               (%clear-window-z-order)
               (clear-focused-window)
               (clear-modal-state)
               (forget-session-graphics)
               ;; first native-font read of the session, and never before this point:
               ;; GLFW makes the process DPI-aware as it initializes, and the OS
               ;; hands a DPI-unaware process virtualized metrics
               (when (refresh-native-fonts)
                 (invalidate-native-fonts))
               ;; and the OS options the drawing depends on, by the same rule
               (refresh-keyboard-cues-setting)
               ;; the session's share partner: a hidden context outside the registry, so
               ;; textures and font atlases outlive every window until FUNCTION returns.
               ;; Same context hints as the windows (alloy renderers/glfw/renderer.lisp:
               ;; initialize-instance :around for renderer): only compatible contexts share.
               (let ((partner (make-instance '%glfw:window :width 1 :height 1 :visible nil
                                                           :title "share partner"
                                                           :opengl-forward-compat t
                                                           :opengl-profile :opengl-core-profile
                                                           :context-version-major 3
                                                           :context-version-minor 3)))
                 (setf *share-window* partner)
                 (unwind-protect (funcall function screen)
                   ;; every window must be destroyed while its GL context is still alive
                   (close-all-windows)
                   (setf *share-window* nil)
                   (%glfw:destroy partner)))))
        (set-float-traps traps)))))

(defmacro with-desktop-application ((screen &key initial-bindings armed-traps) &body body)
  "Run BODY with SCREEN bound to the screen of a fresh desktop session; see CALL-WITH-DESKTOP-APPLICATION."
  `(call-with-desktop-application (lambda (,screen) ,@body)
                                  :initial-bindings ,initial-bindings
                                  :armed-traps ,armed-traps))
