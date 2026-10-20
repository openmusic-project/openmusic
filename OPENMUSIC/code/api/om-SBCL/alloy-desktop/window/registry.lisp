(in-package :alloy-desktop)

(defvar *window-states* (make-hash-table :test 'eq)
  "Authoritative registry of live desktop windows and their WINDOW-STATE objects.")

(defvar *window-creation-order* nil
  "Registered desktop windows in oldest-to-newest creation order.")

(defvar *window-z-order* nil
  "Registered desktop windows in front-to-back order.")

(defun registered-window-state (window)
  "Return WINDOW's registered state and whether WINDOW is registered."
  (gethash window *window-states*))

(defun window-registered-p (window)
  "True when WINDOW is present in the desktop window registry."
  (and window (nth-value 1 (gethash window *window-states*))))

(defun unregister-window (window)
  "Remove WINDOW from the desktop registry. Idempotent."
  (remhash window *window-states*)
  (setf *window-creation-order*
        (delete window *window-creation-order* :test #'eq))
  window)

(defvar *share-window* nil
  "The window whose GL context share group every new window joins, or NIL.
Textures and font atlases uploaded under one context of the group are valid in
every other, so the session's atlases are uploaded once. Under
CALL-WITH-DESKTOP-APPLICATION it is a hidden window outside the registry that
lives as long as the application.")

(defun register-window (window state)
  "Register WINDOW with STATE and place it last in creation order. The first
window registered while no share partner is live becomes the partner."
  (check-type state window-state)
  (unregister-window window)
  (setf (gethash window *window-states*) state
        *window-creation-order*
        (nconc *window-creation-order* (list window)))
  ;; the registry keys any object; only a native window owns a GL context to share
  (when (and (typep window '%glfw:window) (not (share-window)))
    (setf *share-window* window))
  window)

(defun registered-windows (&key (sort-by :create))
  "Return registered windows in creation order, or with SORT-BY :VISIBLE the visible ones front to back followed by the rest in creation order."
  (let ((ordered (copy-list (remove-if-not #'window-registered-p *window-creation-order*))))
    ;; Preserve live registry entries across image/class redefinition even if the
    ;; auxiliary order list was lost.
    (loop for window being the hash-keys of *window-states*
          unless (member window ordered :test #'eq)
            do (setf ordered (nconc ordered (list window))))
    (ecase sort-by
      (:create ordered)
      (:visible
       ;; windows never raised have no native Z position and keep creation order
       (let ((front (remove-if-not (lambda (window)
                                     (and (member window ordered :test #'eq)
                                          (window-visible-p window)))
                                   *window-z-order*)))
         (append front
                 (remove-if (lambda (window) (member window front :test #'eq))
                            ordered)))))))

(defun find-window (class &key (test (constantly t)) (sort-by :create))
  "Return the first registered window of CLASS that satisfies TEST, or NIL."
  (find-if (lambda (window) (and (typep window class) (funcall test window)))
           (registered-windows :sort-by sort-by)))

(defun window-alive-p (window)
  "True while WINDOW still owns a native GLFW handle."
  (and window
       (%glfw:pointer window)
       (not (cffi:null-pointer-p (%glfw:pointer window)))))

(defun live-windows ()
  "Return the registered windows that still own a native handle, in creation order."
  (remove-if-not #'window-alive-p (registered-windows)))

(defun share-window ()
  "The live window a new window shares its GL context with, or NIL; a partner
whose native window is gone is forgotten here."
  (let ((window *share-window*))
    (if (window-alive-p window)
        window
        (setf *share-window* nil))))

(defun retire-share-window (window)
  "When WINDOW is the share partner, hand the role to the first live registered
window. With no live partner left, the session's graphics caches name textures of
contexts that are gone -- a window outside the registry uploads into them too, and
never holds the role -- so they are dropped and the next window uploads afresh."
  (when (eq window *share-window*)
    (setf *share-window* (first (live-windows))))
  (unless (share-window)
    (forget-session-graphics)))

(defun %window-label (window)
  (or (and (typep window 'window)
           ;; unbound-slot: a half-initialised window has no title yet
           (handler-case (window-title window) (unbound-slot () nil)))
      (type-of window)))

(defun call-guarded-on-window (window thunk)
  "Run THUNK for WINDOW so a failure in one window never stops the others, with one backtrace per distinct failure of that window."
  (let ((state (registered-window-state window)))
    (flet ((last-signature ()
             (and state (window-last-error-signature state))))
      (call-guarded thunk
                    (lambda (condition)
                      (not (equal (condition-signature condition) (last-signature))))
                    (lambda (condition backtrace)
                      (format *error-output* "~&[desktop-window] ~A: ~A~%"
                              (%window-label window) (princ-to-string condition))
                      (let ((signature (condition-signature condition)))
                        (unless (equal signature (last-signature))
                          (when state
                            (setf (window-last-error-signature state) signature))
                          (write-string (or backtrace (capture-backtrace)) *error-output*)))
                      (finish-output *error-output*))))))

(defun clear-window-registry ()
  "Remove every registered window and reset creation order."
  (clrhash *window-states*)
  (setf *window-creation-order* nil
        *share-window* nil)
  nil)

(defun window-registry-count ()
  "Return the number of registered desktop windows."
  (hash-table-count *window-states*))

(defun %forget-window-z-order (window)
  (setf *window-z-order* (delete window *window-z-order* :test #'eq))
  window)

(defun %note-window-front (window)
  (setf *window-z-order*
        (cons window (delete window *window-z-order* :test #'eq)))
  window)

(defun %clear-window-z-order ()
  (setf *window-z-order* nil))

(defun window-z-order ()
  "Return desktop windows in front-to-back order."
  (copy-list *window-z-order*))

(defun raise-window (window)
  (windowing:move-to-front window)
  (%note-window-front window)
  window)

(defmethod window-visible-p ((window window))
  "True when WINDOW is shown and not minimized; a maximized window is visible."
  ;; WINDOWING:STATE reads the native handle (alloy renderers/glfw/windowing.lisp: window:state for window).
  (and (window-registered-p window)
       (%glfw:pointer window)
       (member (windowing:state window) '(:normal :maximized))
       t))

(defmethod show-window ((window window))
  ;; Plain orderFront (vanilla show-interface parity); GLFW's :normal state also calls
  ;; restore, whose Cocoa zoom: branch can move a visible window.
  (when (%glfw:iconified-p window)
    (%glfw:restore window))
  (%glfw:show window)
  (raise-window window)
  (window-shown window)
  window)

(defun show-window-without-focus (window)
  "Order WINDOW on screen while the input focus stays where it is, whatever
focus-on-show hint WINDOW was created with. SHOW-WINDOW raises via
WINDOWING:MOVE-TO-FRONT, which is GLFW:FOCUS, so a non-focus satellite cannot go
through it. glfwShowWindow focuses a window whose GLFW_FOCUS_ON_SHOW attribute is
true (the GLFW default), so the attribute is cleared for the show and restored."
  (let ((focus-on-show (%glfw:attribute :focus-on-show window)))
    (setf (%glfw:attribute :focus-on-show window) nil)
    (unwind-protect
         (progn
           (when (%glfw:iconified-p window)
             (%glfw:restore window))
           (%glfw:show window))
      (setf (%glfw:attribute :focus-on-show window) focus-on-show)))
  window)

(defmethod hide-window ((window window))
  (setf (windowing:state window) :hidden)
  window)

;;; Maximized, minimized and normal size.

(defun maximize-window (window)
  "Maximize WINDOW; return WINDOW."
  (setf (windowing:state window) :maximized)
  window)

(defun minimize-window (window)
  "Minimize (iconify) WINDOW; return WINDOW."
  (setf (windowing:state window) :minimized)
  window)

(defun restore-window (window)
  "Show WINDOW at its normal size, out of a maximized or minimized state; return WINDOW."
  (setf (windowing:state window) :normal)
  window)

(defun window-maximized-p (window)
  "True while WINDOW is shown maximized."
  (eql :maximized (windowing:state window)))

(defun window-minimized-p (window)
  "True while WINDOW is minimized (iconified)."
  (eql :minimized (windowing:state window)))
