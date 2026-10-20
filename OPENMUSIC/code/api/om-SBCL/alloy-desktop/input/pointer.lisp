(in-package :alloy-desktop)

;;; Pointer state: one desktop-wide service object, as there is one mouse.

(defclass pointer-state ()
  ((capture :initform nil :accessor pointer-capture
            :documentation "The element holding the pointer until the release, or NIL.")
   (clicked :initform nil :accessor clicked-element
            :documentation "The element pressed, NIL, or :ABORT when a double-click replaced the press.")
   (tooltip-hover :initform nil :accessor tooltip-hover-element
                  :documentation "The element whose tooltip is shown, or NIL.")
   (press-window :initform nil :accessor pointer-press-window
                 :documentation "The window whose PRESS is being dispatched, while it is dispatching.")
   (orphan-release-p :initform nil :accessor orphan-release-p
                     :documentation "True when the release of the current press must be swallowed once.")
   (cursor-location :initform (alloy:px-point 0 0) :accessor cursor-location
                    :documentation "Window-local location of the last pointer event.")))

(defvar *pointer-state* (make-instance 'pointer-state))

(defun pointer-state ()
  *pointer-state*)

;;; Pointer capture: the OS holds the mouse on the pane that was pressed until the
;;; release, so a gesture that leaves the pane still ends on it. Alloy routes by
;;; containment only (alloy layout.lisp: handle for (pointer-event layout)), hence
;;; this window-level redirection.

(defun capture-pointer (element)
  (setf (pointer-capture (pointer-state)) element))

(defun release-pointer-capture (&optional element)
  (when (or (null element) (eq element (pointer-capture (pointer-state))))
    (setf (pointer-capture (pointer-state)) nil)))

(defun pointer-capture-target (event)
  "The capture owner when EVENT falls outside it -- inside, the ordinary layout
dispatch already reaches it (and its children). A detached owner captures nothing,
and a container that is no layout element (a window) holds no capture at all."
  (let ((element (pointer-capture (pointer-state))))
    (when (and (typep element 'alloy:layout-element)
               (alloy:layout-tree element)
               (not (alloy:contained-p (alloy:location event) element)))
      element)))

;;; A popup surface retired from inside its OWN press leaves the release of that
;;; press with nowhere to go. The surface holds the OS mouse capture while the
;;; button is down; hiding it hands the capture back, so the release is delivered
;;; to whatever window is then under the pointer -- the owner, which the popup was
;;; drawn over. That release is the tail of a gesture the popup already consumed,
;;; never a click on the owner, and acting on it moves the input focus to whatever
;;; the popup covered. CAPI has nothing equivalent because its non-focus window
;;; keeps the whole mouse gesture (capi-capi-ref-369.htm:106).

(defun arm-orphan-release ()
  "Swallow the release that ends the press being dispatched."
  (setf (orphan-release-p (pointer-state)) t))

(defun claim-orphan-release ()
  "Consume the pending orphaned release, if there is one."
  (when (orphan-release-p (pointer-state))
    (setf (orphan-release-p (pointer-state)) nil)
    t))

(defgeneric element-window (element)
  (:documentation "The window that owns ELEMENT, or NIL when ELEMENT is detached."))

(defmethod element-window ((element t))
  (let ((tree (and (typep element 'alloy:layout-element)
                   (alloy:layout-tree element))))
    (when tree
      (let ((ui (alloy:ui tree)))
        (and (typep ui 'window) ui)))))

(defun retire-window-pointer-state (window)
  "Forget the click/tooltip/capture elements owned by WINDOW during its funeral: each
of these roots the dead window, and the next pointer event would route to an
element whose container chain has already been cut."
  (let ((state (pointer-state)))
    (flet ((mine-p (element)
             (and element (eq (element-window element) window))))
      (when (mine-p (clicked-element state))       (setf (clicked-element state) nil))
      (when (mine-p (tooltip-hover-element state)) (setf (tooltip-hover-element state) nil))
      (when (mine-p (pointer-capture state))       (setf (pointer-capture state) nil)))))

(register-window-retirement :pointer-state
                            #'retire-window-pointer-state
                            :order 10)

;;; Alloy's layout dispatch aborts the WHOLE layout on the first declining child
;;; (alloy layout.lisp: handle for (pointer-event layout)), so a passive sibling
;;; swallows the event for everyone -- including the scrollbars, whose drag and
;;; hover need pointer-move to reach them.

(defgeneric event-touches-p (event element)
  (:method ((event alloy:pointer-event) element)
    (alloy:contained-p (alloy:location event) element))
  (:method ((event alloy:pointer-move) element)
    ;; alloy also routes a move to the element the pointer just LEFT, so hover-out
    ;; fires (alloy layout.lisp: handle for (pointer-move layout))
    (or (alloy:contained-p (alloy:location event) element)
        (alloy:contained-p (alloy:old-location event) element))))

(defun dispatch-to-children (event container)
  "Reverse iter that continues past children who decline. T if some child handled EVENT."
  (let ((handled nil))
    (alloy:do-elements (child container :from-end t)
      (unless handled
        (when (event-touches-p event child)
          (when (alloy:handle event child)
            (setf handled t)))))
    handled))

;;; Hover cleanup of the :weak focus.

(defun find-ancestor-clip-view (element)
  "Return the nearest alloy:clip-view ancestor of ELEMENT, or NIL. A detached
element (layout-parent unbound) has no ancestors."
  (loop for e = element then (alloy:layout-parent e)
        while (and e (slot-boundp e 'alloy:layout-parent)
                   (not (eq e (alloy:layout-parent e))))
        when (typep e 'alloy:clip-view) return e
        finally (return nil)))

(defun hover-contains-p (element location)
  "Is LOCATION over the VISIBLE part of ELEMENT? A pane inside a clip-view is as tall
as its content, so its own bounds also cover the neighbours it never draws over; the
bounds test alone leaves such a pane lit after the pointer has left it."
  (and (alloy:contained-p location element)
       (let ((clip (find-ancestor-clip-view element)))
         (or (null clip) (alloy:contained-p location clip)))))

(defun clear-stale-weak-focus (window location)
  "Drop the :weak focus of every element of WINDOW (root and popups) the pointer at
LOCATION is no longer over, sparing the focus path of the focused element."
  (let ((keep (focus-path (alloy:focused (alloy:focus-tree window)))))
    (labels ((walk (el)
               (when (and (typep el 'alloy:focus-element)
                          (eql :weak (alloy:focus el))
                          (not (member el keep))
                          (not (hover-contains-p el location)))
                 (setf (alloy:focus el) nil)
                 (alloy:mark-for-render el))
               (when (typep el 'alloy:container)
                 (alloy:do-elements (child el) (walk child)))))
      (walk (alloy:root (alloy:layout-tree window)))
      ;; popups (completion overlay, menus) need the same hover-leave cleanup
      (walk (alloy:popups (alloy:layout-tree window))))))

(defgeneric clear-hover (element)
  (:documentation "Drop ELEMENT's own hover flag when the pointer leaves its window; NIL when it has none.")
  (:method ((element t)) nil))

(defmethod alloy:handle :before ((event windowing:pointer-leave) (window window))
  (labels ((walk (el)
             (when (and (typep el 'alloy:focus-element)
                        (eql :weak (alloy:focus el)))
               (setf (alloy:focus el) nil))
             (clear-hover el)
             (when (typep el 'alloy:container)
               (alloy:do-elements (child el) (walk child)))))
    (walk (alloy:root (alloy:layout-tree window)))))

(defmethod alloy:handle :before ((event alloy:pointer-event) (window window))
  (setf (cursor-location (pointer-state)) (alloy:location event)))

;; *hover-move-p* is bound for the WHOLE dispatch, captured target included.
(defmethod alloy:handle ((event alloy:pointer-move) (window window))
  (let ((*hover-move-p* t))
    (clear-stale-weak-focus window (alloy:location event))
    (let ((target (pointer-capture-target event)))
      (if target
          (or (alloy:handle event target) (alloy:decline))
          (call-next-method)))))

(defmethod alloy:handle ((event alloy:pointer-up) (window window))
  ;; the capture always ends with the release, whoever handles it
  (let ((target (pointer-capture-target event)))
    (release-pointer-capture)
    (cond ((claim-orphan-release) t)
          (target (or (alloy:handle event target) (alloy:decline)))
          (t (call-next-method)))))

;;; Double-click synthesis. CAPI and Windows fire the double on the SECOND PRESS,
;;; replacing it, with the pressed element set to :ABORT first, so the second
;;; click's release and motion never leak into release/drag handlers. Upstream
;;; alloy-glfw instead synthesizes on release with a 2 px tolerance -- too strict
;;; for real mice (Windows default: 4 logical px) and too late for that protocol
;;; (alloy renderers/glfw/windowing.lisp: glfw:mouse-button-changed for window).

(defparameter *double-click-max-distance* 6.0)
(defparameter *double-click-max-time* 0.5d0)

(defmethod %glfw:mouse-button-changed ((window window) button action mods)
  ;; the modifiers are synced in every mouse callback
  (note-modifiers mods)
  (case action
    (:press
     (let* ((click (glfw::last-click window))
            (last  (glfw::last-click-location click))
            (loc   (glfw::cursor-location window))
            (time  (%glfw:time))
            (double-p (and (< (- time (glfw::last-click-time click))
                              *double-click-max-time*)
                           (eql (glfw::last-click-button click) button)
                           (< (sqrt (+ (expt (- (alloy:pxx last) (alloy:pxx loc)) 2)
                                       (expt (- (alloy:pxy last) (alloy:pxy loc)) 2)))
                              *double-click-max-distance*))))
       ;; a detected double resets the cycle: a third press starts a new one
       (setf (glfw::last-click-button click) button)
       (setf (glfw::last-click-time click) (if double-p 0d0 time))
       (setf (glfw::last-click-location click) loc)
       ;; a new press retires any release still owed by the previous one
       (setf (orphan-release-p (pointer-state)) nil)
       (let* ((state (pointer-state))
              (outer (pointer-press-window state)))
         (setf (pointer-press-window state) window)
         (unwind-protect
              (cond (double-p
                     (setf (clicked-element state) :abort)
                     (alloy:handle (make-instance 'alloy:pointer-double-click
                                                  :location loc :kind button)
                                   window))
                    (t
                     (alloy:handle (make-instance 'alloy:pointer-down
                                                  :location loc :kind button)
                                   window)))
           (setf (pointer-press-window state) outer)))))
    (:release
     (alloy:handle (make-instance 'alloy:pointer-up
                                  :location (glfw::cursor-location window)
                                  :kind button)
                   window))))
