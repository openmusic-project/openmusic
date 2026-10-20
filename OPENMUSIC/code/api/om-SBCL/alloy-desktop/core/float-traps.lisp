(in-package :alloy-desktop)

(defun set-float-traps (traps)
  "Enable exactly TRAPS on the current thread, masking every other float trap, and return TRAPS."
  (sb-int:set-floating-point-modes :traps traps)
  traps)

(defun mask-float-traps ()
  "Mask every float trap on the current thread, for a tick that enters native code expecting IEEE infinities back."
  (set-float-traps '()))

(defun armed-float-traps ()
  "Return the float traps enabled on the current thread."
  (getf (sb-int:get-floating-point-modes) :traps))
