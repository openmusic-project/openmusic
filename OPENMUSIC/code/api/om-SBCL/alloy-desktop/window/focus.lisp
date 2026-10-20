(in-package :alloy-desktop)

;;; Generic desktop focus authority.
;;;
;;; This is the reusable part of the focus machinery formerly hosted by the
;;; OpenMusic bridge: one authoritative top-level focused window plus the
;;; low-level Alloy focus-tree operations needed to focus, unfocus, and repair
;;; a pane. Host policy such as caret blinking, text-selection lifetime,
;;; mnemonics, and application callbacks remains outside this file.

(defvar *focused-window* nil
  "The desktop window currently considered to own the OS input focus.")

(defvar *desktop-active-p* t
  "NIL after the focused desktop window loses OS focus without another desktop window taking it.")

(defvar *modal-refocusing-p* nil
  "Reentrancy guard for restoring focus to the active modal window.")

(defun focused-window ()
  *focused-window*)

(defun front-window ()
  "The focused registered window, or else the first registered one."
  (let ((focused (focused-window)))
    (or (and focused (window-registered-p focused) focused)
        (first (registered-windows)))))

(defun desktop-active-p ()
  *desktop-active-p*)

(defun adopt-focused-window (window)
  "Make WINDOW the logical focused desktop window without changing active state.

This is intentionally a state-only operation. It supports implementation-only
native surfaces whose OS focus must still be represented by their logical owner."
  (setf *focused-window* window))

(defun clear-focused-window ()
  "Forget the logical focused window without changing desktop active state."
  (setf *focused-window* nil))

(defun forget-window-focus (window)
  "Forget WINDOW if it is the current logical focused window."
  (when (eq window *focused-window*)
    (setf *focused-window* nil))
  window)

(defun note-window-focus (window focused-p)
  "Record the native focus transition for WINDOW.

Return four values: the previous focused window, the current focused window,
the previous desktop-active state, and the current desktop-active state. The
caller may use those values for host-specific repaint/caret policy."
  (let ((previous *focused-window*)
        (was-active *desktop-active-p*))
    (cond (focused-p
           (setf *desktop-active-p* t
                 *focused-window* window)
           (%note-window-front window))
          ((eq window *focused-window*)
           (setf *desktop-active-p* nil)))
    (values previous *focused-window* was-active *desktop-active-p*)))

(defmethod window-focused-p ((window window))
  (and window *desktop-active-p* (eq window *focused-window*)))

(defun refocus-modal-window (focused-window)
  "If FOCUSED-WINDOW displaced an active modal, raise that modal again.

This preserves the existing desktop behavior and only owns the generic native
refocus mechanism; host focus/caret/menu policy remains outside the facility."
  (let ((modal-window (current-modal-window)))
    (when (and modal-window
               (not (eq focused-window modal-window))
               (not *modal-refocusing-p*))
      (let ((*modal-refocusing-p* t))
        (windowing:move-to-front modal-window))
      modal-window)))

;;; Hover policy: pointer motion never moves the keyboard focus of a window's
;;; CONTENT; the component's own :weak is left alone.

(defvar *hover-move-p* nil
  "True while a pointer-move is being dispatched (hover or drag).")

(defun focus-path (element)
  "ELEMENT and its focus-parent ancestors. Their :weak focus is STRUCTURAL (it is
the path alloy keeps to the :strong focused element, e.g. an open menu chain),
never hover state, so hover-leave cleanup must not clear it."
  (let ((path nil))
    (loop for el = element
            then (and (slot-boundp el 'alloy::focus-parent) (alloy:focus-parent el))
          while el
          do (push el path)
             (when (and (slot-boundp el 'alloy::focus-parent)
                        (eq el (alloy:focus-parent el)))
               (return)))
    path))

(defun chrome-focus-parent (window)
  "Where window chrome (menu bar, scroll bars) is realized in WINDOW's focus tree:
the POPUPS sub-tree, never the root chain that holds the content panes. CAPI
gives an interactive element the focus but never a menu
(capi-common-elements-ug-1.htm:108), so neither keyboard navigation nor an
unconsumed RET may reach chrome. It stays inside the tree, because an element
with no focus-tree receives no event at all, clicks included
(alloy focus-tree.lisp: the handle :around for (event focus-element))."
  (let ((tree (alloy:focus-tree window)))
    (and tree (alloy:popups tree))))

(defun content-chain-p (chain)
  "True when CHAIN hangs off a window's CONTENT focus root: neither the popups
sub-tree that holds window chrome (chrome-focus-parent) nor an open popup frame,
where the focus IS the keyboard position."
  (let ((tree (alloy:focus-tree chain)))
    (when tree
      (let ((root (first (focus-path chain))))
        (not (or (eq root (alloy:popups tree))
                 (typep root 'alloy:popup)))))))

(defmethod (setf alloy:focus) :around ((focus (eql :strong)) (chain alloy:focus-chain))
  (if (and *hover-move-p* (content-chain-p chain))
      (alloy:focus chain)
      (call-next-method)))

(defmethod alloy:notice-focus :around ((element alloy:focus-element) (chain alloy:focus-chain))
  (unless (and *hover-move-p* (content-chain-p chain))
    (call-next-method)))

(defun pane-editing-p (pane)
  "True while PANE holds the strong focus, i.e. it owns the keystrokes."
  (eql :strong (alloy:focus pane)))

(defun %focus-chain (element)
  "ELEMENT's enclosing focus-chain, or NIL when unhooked or already the root."
  (when (and (typep element 'alloy:focus-element)
             (slot-boundp element 'alloy::focus-parent))
    (let ((parent (alloy:focus-parent element)))
      (when (and (typep parent 'alloy:focus-chain)
                 (not (eq parent element)))
        parent))))

(defun focus-pane (window pane &key after-focus)
  "Give PANE the strong focus of WINDOW and repair its enclosing focus-chain.

WINDOW may be NIL when the caller already knows PANE belongs to the desired
focus tree. AFTER-FOCUS, when supplied, runs after the tree/chain update and
before render invalidation. Returns PANE when focus was established, NIL otherwise."
  (let ((tree (and (typep pane 'alloy:focus-element)
                   (alloy:focus-tree pane))))
    (when (and tree (or (null window) (eq tree (alloy:focus-tree window))))
      (setf (alloy:focused tree) pane)
      (unless (eql :strong (alloy:focus pane))
        (setf (alloy:focus pane) :strong))
      (let ((chain (%focus-chain pane)))
        (when (and chain (not (eq pane (alloy:focused chain))))
          (alloy:notice-focus pane chain)))
      (when after-focus
        (funcall after-focus pane))
      (when (typep pane 'alloy:renderable)
        (alloy:mark-for-render pane))
      pane)))

(defun unfocus-pane (window pane)
  "Remove strong focus from PANE in WINDOW, preserving Alloy focus-tree order."
  (let ((tree (and (typep pane 'alloy:focus-element)
                   (alloy:focus-tree pane))))
    (when (and tree (or (null window) (eq tree (alloy:focus-tree window))))
      (let ((chain (%focus-chain pane)))
        (when (and chain (eq pane (alloy:focused chain)))
          (setf (alloy:focused chain) nil)))
      (setf (alloy:focused tree) nil)
      (setf (alloy:focus pane) nil)
      (when (typep pane 'alloy:renderable)
        (alloy:mark-for-render pane))
      t)))

(defun strong-focus-pane (window)
  "Return the :STRONG focus leaf under WINDOW's focus root, or NIL."
  (labels ((walk (element)
             (cond ((not (typep element 'alloy:focus-element)) nil)
                   ((typep element 'alloy:focus-chain)
                    (let ((hit nil))
                      (alloy:do-elements (child element)
                        (unless hit (setf hit (walk child))))
                      hit))
                   ((eql :strong (alloy:focus element)) element))))
    (let ((root (and window (window-focus-root window))))
      (and root (walk root)))))

(defun repair-window-focus (window &key (focus-function #'focus-pane))
  "Repair a focus tree that drifted away from its :STRONG pane.

FOCUS-FUNCTION receives WINDOW and the pane selected for repair; it defaults to
the neutral FOCUS-PANE operation so a host can retain narrow focus-side policy.
Return the repaired pane and true as two values when a repair happened;
otherwise return NIL, NIL."
  (let* ((tree (and window (alloy:focus-tree window)))
         (focused (and tree (alloy:focused tree))))
    (when (and tree
               (not (and focused (eql :strong (alloy:focus focused)))))
      (let ((pane (strong-focus-pane window)))
        (when (and pane (not (eq pane focused)))
          (funcall focus-function window pane)
          (values pane t))))))
