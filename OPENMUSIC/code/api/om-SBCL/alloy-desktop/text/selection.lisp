(in-package :alloy-desktop)

;;; Text-selection owner. Tracked per window, never read off alloy:focus: hovering a
;;; never-focused neighbour promotes a flat focus-list to :strong (alloy
;;; component.lisp: handle for (pointer-move component)), which unwinds the pane that
;;; held it :strong -> NIL -> :weak with no click at all (alloy focus-tree.lisp:
;;; (setf focused) :before for (focus-element focus-tree)).
;;;
;;; The selection is never undone by focus or blur. What varies is the VISIBILITY of
;;; the highlight, and which explicit gestures kill it; both generics dispatch on the
;;; WINDOW class, since the pane class cannot tell window families apart.

(defun window-selected-text-pane (window)
  "The text pane whose selection WINDOW tracks, or NIL."
  (let ((state (registered-window-state window)))
    (when state (%state-selected-text-pane state))))

(defun (setf window-selected-text-pane) (pane window)
  (let ((state (registered-window-state window)))
    (when state (setf (%state-selected-text-pane state) pane)))
  pane)

(defun drop-text-selection (pane)
  "Hide PANE's selection by clearing its anchor, returning T when PANE is a text component or a text view."
  ;; the :selection shape hides on a NIL anchor (alloy presentations/default.lisp:
  ;; define-update for (default-look-and-feel alloy:text-input-component))
  (typecase pane
    (alloy:text-input-component
     (setf (alloy:anchor (alloy:cursor pane)) nil)
     (alloy:mark-for-render pane)
     t)
    (editor:text-view
     (setf (editor:text-view-anchor pane) nil)
     t)))

(defgeneric selection-survives-blur-p (window)
  (:documentation "True for a window whose selection highlight stays visible while it has no OS focus.")
  (:method ((window t)) nil))

(defgeneric click-clears-selection-p (window)
  (:documentation "True for a window where a click elsewhere inside it undoes the text selection.")
  (:method ((window t)) nil))

(defun selection-hidden-p (widget window)
  "True when WIDGET, shown in WINDOW, must not paint its selection highlight."
  (or (null (alloy:anchor (alloy:cursor widget)))
      (and window
           (not (window-focused-p window))
           (not (selection-survives-blur-p window))
           t)))

(defun release-text-selection (window)
  "Undo the selection WINDOW tracks and forget its owner, returning the former owner."
  (let ((owner (window-selected-text-pane window)))
    (when owner
      (drop-text-selection owner)
      (setf (window-selected-text-pane window) nil)
      owner)))
