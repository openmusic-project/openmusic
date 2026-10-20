(in-package :alloy-desktop)

(defclass window (glfw:window) ())

(defclass window-state ()
  ((owner :initform nil :accessor %window-state-owner)
   (closing-fired-p :initform nil :accessor window-closing-fired-p)
   (last-error-signature :initform nil :accessor window-last-error-signature)
   (caret-painted-phase :initform :unset :accessor %state-caret-painted-phase)
   (keyboard-cues :initform :unshown :accessor %state-keyboard-cues)
   (selected-text-pane :initform nil :accessor %state-selected-text-pane)
   (root-layout :initform nil :accessor %state-root-layout)
   (focus-list :initform nil :accessor %state-focus-list)
   (chrome :initform nil :accessor %state-chrome)))

(defmethod window-focus-root ((window window))
  (alloy:root (alloy:focus-tree window)))

(defmethod window-title ((window window))
  (windowing:title window))
