(in-package :alloy-desktop)

;;; Modifier snapshots. Every input event refreshes them, so a callback reads the
;;; modifiers of the event that is being dispatched.

(defvar *shift-key-p*   nil)
(defvar *command-key-p* nil)
(defvar *option-key-p*  nil)

(defun shift-key-p   () *shift-key-p*)
(defun command-key-p () *command-key-p*)
(defun option-key-p  () *option-key-p*)

(defun command-modifier ()
  "The \"command\" modifier: Command (Super) on macOS, Control on Windows and Linux."
  #+darwin :super #-darwin :control)

(defun note-modifiers (alloy-modifiers)
  "Refresh the modifier snapshots from ALLOY-MODIFIERS, the modifier list of an alloy event."
  (setf *shift-key-p* (and (find :shift alloy-modifiers) t))
  (setf *command-key-p* (and (find (command-modifier) alloy-modifiers) t))
  (setf *option-key-p* (and (find :alt alloy-modifiers) t)))
