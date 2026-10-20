(in-package :alloy-desktop)

;;; Keyboard cues. Windows keeps mnemonic underlines hidden until the keyboard is
;;; used (LW manual capi/capi-common-elements-ug-1.htm:93-95), and the reveal is a
;;; STICKY property of the window: nothing puts UISF_HIDEACCEL back once Alt has
;;; cleared it -- not the key-up, not the loss of the focus. A window opens in the
;;; state the LAST INPUT EVENT dictates, the keyboard showing the cues and the
;;; pointer hiding them, which is what the system answers to UIS_INITIALIZE.
;;; SPI_GETKEYBOARDCUES overrides all of it: with that option on, every window shows
;;; its cues permanently. macOS has no mnemonics at all.

(defvar *keyboard-cues-always-p* nil
  "True when the system keeps keyboard cues permanently visible; read once per session by REFRESH-KEYBOARD-CUES-SETTING.")

(defvar *last-input-keyboard-p* nil
  "True while the last input event of the session came from the keyboard.")

(defun refresh-keyboard-cues-setting ()
  "Read the system keyboard-cue option into *KEYBOARD-CUES-ALWAYS-P*."
  (setf *keyboard-cues-always-p* (keyboard-cues-setting)))

(defun note-input-source (keyboard-p)
  "Record whether the last input event came from the keyboard."
  (setf *last-input-keyboard-p* (and keyboard-p t)))

(defun %window-keyboard-cues (window)
  (let ((state (and window (registered-window-state window))))
    (if state (%state-keyboard-cues state) :unshown)))

(defun keyboard-cues-p (window)
  "True when WINDOW must draw its keyboard cues."
  #+darwin (declare (ignore window))
  #+darwin nil
  #-darwin (and window
                (or *keyboard-cues-always-p*
                    (eq t (%window-keyboard-cues window)))))

(defun window-shown-p (window)
  "True once WINDOW has been on screen."
  (not (eq :unshown (%window-keyboard-cues window))))

(defgeneric keyboard-cues-changed (window)
  (:documentation "Called after WINDOW's keyboard-cue state flips, to repaint what depends on it.")
  (:method ((window t)) (alloy:mark-for-render window)))

(defun set-keyboard-cues (window on)
  "Set WINDOW's keyboard cues revealed or hidden, calling KEYBOARD-CUES-CHANGED when the state changes."
  (let ((on (if on t :off))
        (state (registered-window-state window)))
    (when (and state (not (eq on (%state-keyboard-cues state))))
      (setf (%state-keyboard-cues state) on)
      (keyboard-cues-changed window))))

(defun init-keyboard-cues (window)
  "Give WINDOW, on its first appearance only, the cue state of the last input source."
  (unless (window-shown-p window)
    (set-keyboard-cues window *last-input-keyboard-p*)))

(defun note-keyboard-cue-event (window event)
  "Reveal WINDOW's keyboard cues when EVENT is a key press carrying Alt."
  (when (and (typep event 'alloy:key-down)
             (or (member (alloy:key event) '(:left-alt :right-alt))
                 (find :alt (alloy:modifiers event))))
    (set-keyboard-cues window t)))
