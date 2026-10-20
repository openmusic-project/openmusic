(in-package :alloy-desktop)

(defclass text-editor (window) ())

(defclass lisp-editor (text-editor) ())

(defclass output-window (window) ())

(defclass output-pane (text-edit-view) ())

(defclass eval-output-pane (output-pane) ())

(defclass completion-popup (popup-frame) ())

(defclass completion-list (single-item-list) ())

(defclass repl (window) ())

(defclass repl-pane (text-edit-view) ())

(defclass listener (window) ())

(defclass listener-divider (alloy:resizer) ())

(defclass listener-stream (editor:text-view-stream) ())
