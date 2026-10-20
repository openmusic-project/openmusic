(in-package :alloy-desktop)

(defun window-owner (window)
  "Return WINDOW's registered owner, or NIL when it has none or is unregistered."
  (let ((state (registered-window-state window)))
    (and state (%window-state-owner state))))

(defun (setf window-owner) (owner window)
  "Set WINDOW's owner while rejecting self-ownership and ownership cycles, and
apply the native owned-window relation where the platform exposes it, when both
are live native windows. A NIL owner clears the registry entry only: an existing
native relation is left in place."
  (let ((state (registered-window-state window)))
    (unless state
      (error "Window ~S is not registered." window))
    (when (eq window owner)
      (error "A window cannot own itself."))
    (when owner
      (loop for candidate = owner then (window-owner candidate)
            while candidate
            when (eq candidate window)
              do (error "Window ownership would form a cycle.")))
    (setf (%window-state-owner state) owner)
    ;; the registry keys any object; only native windows carry the relation
    (when (and (typep window '%glfw:window) (typep owner '%glfw:window)
               (window-alive-p window) (window-alive-p owner))
      (%apply-native-window-owner window owner))
    owner))

(defun owned-windows (owner)
  "Return OWNER's directly owned windows in creation order."
  (loop for window in (registered-windows)
        when (eq owner (window-owner window))
          collect window))
