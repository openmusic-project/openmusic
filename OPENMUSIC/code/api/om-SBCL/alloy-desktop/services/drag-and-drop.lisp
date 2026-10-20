(in-package :alloy-desktop)

(defclass drag-view () ())
(defclass drop-view () ())

;;; Drag cursor. A native drag session owns the pointer shape for the whole
;;; gesture; here the application does, so every window the gesture visits takes
;;; the drag cursor and gets its own back at cleanup. One drag at a time, as there
;;; is one mouse.

(defclass drag-cursor-session ()
  ((windows :initform nil :accessor drag-cursor-windows
            :documentation "(WINDOW . ORIGINAL-CURSOR) for every window the live drag has visited.")))

(defvar *drag-cursor-session* (make-instance 'drag-cursor-session))

(defun drag-cursor-session ()
  *drag-cursor-session*)

(defun take-drag-cursor (window cursor)
  "Write CURSOR on WINDOW, remembering WINDOW's own cursor on the first visit.
Callers rewrite it on EVERY move, because hover handlers and alloy's reset for a
declined pointer-move (alloy layout.lisp: handle for (layout-event layout-tree))
overwrite it mid-gesture."
  (when (and window (window-alive-p window))
    (let ((session (drag-cursor-session)))
      (unless (assoc window (drag-cursor-windows session))
        (push (cons window (windowing:icon (windowing:cursor window)))
              (drag-cursor-windows session))))
    (setf (alloy:cursor window) cursor)))

(defun feedback-drag-cursor (effect cursor &rest windows)
  "Show CURSOR on WINDOWS only for an actually negotiated operation, :NOT-ALLOWED otherwise."
  (dolist (window (remove-duplicates windows :test #'eq))
    (when (and window (window-alive-p window))
      (take-drag-cursor window cursor)
      (setf (alloy:cursor window)
            (if (member effect '(:copy :move :link) :test #'eq)
                cursor
                :not-allowed)))))

(defun release-drag-cursors ()
  "Give every visited window its own cursor back and forget the visits."
  (let ((session (drag-cursor-session)))
    (dolist (entry (drag-cursor-windows session))
      (when (window-alive-p (car entry))
        (setf (alloy:cursor (car entry)) (or (cdr entry) :default))))
    (setf (drag-cursor-windows session) nil)))

(defun drag-cursor-window-p (window)
  "True when the live drag has written its cursor on WINDOW."
  (and (assoc window (drag-cursor-windows (drag-cursor-session)) :test #'eq) t))

;;; Drag session. The host starts it (sets *DRAG-SOURCE* and *DRAG-ACTIVE-P* when
;;; its source begins to move) and runs DRAG-MOVE-EPILOGUE / DRAG-UP-EPILOGUE after
;;; every pointer move / left release of any of its windows. Everything that knows
;;; what a target is, what it accepts and what a drop does is a generic function
;;; dispatched on the drag SOURCE, so a program defines its methods on its source class.

(defvar *drag-source* nil "The object being dragged, or NIL.")
(defvar *drag-active-p* nil "True while a drag gesture is live.")
(defvar *drag-last-target* nil "The target the pointer is over, or NIL.")
(defvar *drag-drop-object* nil
  "The host's drop object for the current target, opaque to the library: DRAG-ENTER
sets it, DRAG-OVER and DRAG-EFFECT only run while it is non-NIL.")

(defgeneric drag-target-at (source window location)
  (:documentation "The drop target under LOCATION of WINDOW, or NIL.")
  (:method ((source t) window location)
    (declare (ignore window location))
    nil))

(defgeneric drag-candidate-windows (source window)
  (:documentation "The windows a drag leaving WINDOW may be routed to.")
  (:method ((source t) window)
    (declare (ignore window))
    (live-windows)))

(defgeneric drag-cursor (source)
  (:documentation "The pointer shown while SOURCE is carried.")
  (:method ((source t)) :pointing-hand))

(defgeneric drag-enter (source target location)
  (:documentation "The pointer entered TARGET at LOCATION; sets *DRAG-DROP-OBJECT*.")
  (:method ((source t) target location)
    (declare (ignore target location))
    nil))

(defgeneric drag-leave (source target)
  (:documentation "The pointer left TARGET for another target or none.")
  (:method ((source t) target)
    (declare (ignore target))
    nil))

(defgeneric drag-leave-final (source target)
  (:documentation "The session ended while the pointer was over TARGET.")
  (:method ((source t) target)
    (declare (ignore target))
    nil))

(defgeneric drag-over (source target location)
  (:documentation "The pointer moved over TARGET, now at LOCATION.")
  (:method ((source t) target location)
    (declare (ignore target location))
    nil))

(defgeneric drag-effect (source)
  (:documentation "The negotiated effect over the current target: :COPY, :MOVE, :LINK or :NONE.")
  (:method ((source t)) :none))

(defgeneric drag-commit (source target location)
  (:documentation "The left button was released over TARGET at LOCATION.")
  (:method ((source t) target location)
    (declare (ignore target location))
    nil))

(defun drag-cleanup ()
  "End the drag session: cursors back, the last target left, pointer capture and
the pressed element released."
  (release-drag-cursors)
  (when *drag-last-target*
    (drag-leave-final *drag-source* *drag-last-target*))
  ;; A cancelled native drag also releases mouse capture.  Leaving Alloy's
  ;; gesture owner armed after Escape/focus loss would route the next unrelated
  ;; pointer event back into the old source.
  (release-pointer-capture)
  (setf *drag-source*      nil
        *drag-active-p*    nil
        *drag-last-target* nil
        *drag-drop-object* nil
        (clicked-element (pointer-state)) nil))

(defmacro with-cursor-location ((location) &body body)
  "Run BODY with the pointer snapshot at LOCATION, restored afterwards."
  (let ((state (gensym "STATE")) (saved (gensym "SAVED")))
    `(let* ((,state (pointer-state))
            (,saved (cursor-location ,state)))
       (setf (cursor-location ,state) ,location)
       (unwind-protect (progn ,@body)
         (setf (cursor-location ,state) ,saved)))))

;;; Cross-window routing. The OS holds the mouse on the press window for the whole
;;; gesture, so a drop over ANOTHER window reaches the epilogues with out-of-client
;;; coordinates. A native drag session posts the drop to the window under the
;;; pointer whatever window owns the press.

(defun drag-route (source window location)
  "WINDOW and LOCATION when LOCATION is inside WINDOW's client area; otherwise the
candidate window under the pointer and LOCATION in ITS coordinates."
  (let ((size (%glfw:size window))
        (x    (alloy:pxx location))
        (y    (alloy:pxy location)))
    (if (and (<= 0 x (first size)) (<= 0 y (second size)))
        (values window location)
        (multiple-value-bind (sx sy) (popup-owner-point-to-screen window x y)
          (let ((target (window-at-screen-point
                         sx sy :exclude window
                               :windows (drag-candidate-windows source window))))
            (if target
                (let ((position (%glfw:location target))
                      (tsize    (%glfw:size target)))
                  (values target
                          (alloy:px-point (- sx (first position))
                                          (- (second tsize) (- sy (second position))))))
                (values window location)))))))

(defun drag-move-epilogue (event-window location)
  "Track the live drag after a pointer move delivered to EVENT-WINDOW."
  (when *drag-active-p*
    (handler-case
        (let ((source *drag-source*))
          (take-drag-cursor event-window (drag-cursor source))
          (multiple-value-bind (window location) (drag-route source event-window location)
            (take-drag-cursor window (drag-cursor source))
            ;; the host reads the pointer snapshot inside its drop protocol, so use
            ;; the coordinates of the routed target window.
            (with-cursor-location (location)
              (let ((target (drag-target-at source window location)))
                (unless (eql target *drag-last-target*)
                  (when *drag-last-target*
                    (drag-leave source *drag-last-target*))
                  (when target
                    (drag-enter source target location))
                  (setf *drag-last-target* target))
                (when (and target *drag-drop-object*)
                  (drag-over source target location))
                (feedback-drag-cursor
                 (if (and target *drag-drop-object*)
                     (drag-effect source)
                     :none)
                 (drag-cursor source)
                 event-window window)))
            ;; The ghost lives on the press window and must keep repainting while
            ;; the cursor visits another one.
            (alloy:mark-for-render event-window)
            (unless (eq window event-window) (alloy:mark-for-render window))))
      (error (condition)
        (drag-cleanup)
        (error condition)))))

(defun drag-up-epilogue (event-window location)
  "Commit the live drag on the left release delivered to EVENT-WINDOW, then end it."
  (when *drag-active-p*
    (unwind-protect
         (multiple-value-bind (window location) (drag-route *drag-source* event-window location)
           (with-cursor-location (location)
             (let* ((target (drag-target-at *drag-source* window location))
                    (source *drag-source*))
               (when (and target source)
                 ;; The destination commits first.  Only its successful callback
                 ;; can make the host mutate/move the source.
                 (drag-commit source target location))))
           (alloy:mark-for-render window))
      (drag-cleanup))))

(defun drag-involves-window-p (window view-window-fn)
  "True when the live drag's source or last target lives in WINDOW, or the drag
wrote its cursor there. VIEW-WINDOW-FN maps a source or target to its window."
  (or (and *drag-source* (eq window (funcall view-window-fn *drag-source*)))
      (and *drag-last-target* (eq window (funcall view-window-fn *drag-last-target*)))
      (drag-cursor-window-p window)))

;;; Drop object. What a drop target is offered: the pointer position inside the
;;; target, the effects the source allows, the effect the platform proposes, the
;;; formats the target accepts and the payload the source carries. Source,
;;; proposal and the final choice are separate slots so that hover feedback never
;;; widens what the source allows.

(defclass drop-object ()
  ((pane :initarg :pane :initform nil :reader drop-object-pane
         :documentation "The target the object is offered to.")
   (x :initarg :x :initform 0 :accessor drop-object-pane-x
      :documentation "Pointer X inside the target.")
   (y :initarg :y :initform 0 :accessor drop-object-pane-y
      :documentation "Pointer Y inside the target.")
   (source-effects :initarg :source-effects :initform '(:copy :move :link)
                   :accessor drop-object-source-effects
                   :documentation "The effects the source allows.")
   (proposed-effect :initarg :proposed-effect :initform nil
                    :accessor drop-object-proposed-effect
                    :documentation "The effect proposed for the drop, or NIL.")
   (origin :initarg :origin :initform :internal :reader drop-object-origin
           :documentation ":INTERNAL for a drag of the program's own objects, :EXTERNAL otherwise.")
   (effect :initarg :effect :initform :none :accessor drop-object-drop-effect
           :documentation "The effect the target committed: :COPY, :MOVE, :LINK, :NONE, or NIL when the target cleared it.")
   (formats :initarg :formats :initform nil :accessor drop-object-formats
            :documentation "The formats the target accepts.")
   (payload :initarg :payload :initform nil :accessor drop-object-payload
            :documentation "A property list from format to the object carried in that format.")))

(defun valid-drop-effect-p (effect)
  "True when EFFECT is :COPY, :MOVE or :LINK."
  (not (null (member effect '(:copy :move :link) :test #'eq))))

(defun drop-object-allows-drop-effect-p (drop-object effect)
  "True when EFFECT is valid and DROP-OBJECT's source allows it."
  (and (valid-drop-effect-p effect)
       (not (null (member effect (drop-object-source-effects drop-object)
                          :test #'eq)))))

(defun negotiate-drop-effect (drop-object target-effects)
  "Return the unique effect common to source and target, without committing it:
the proposed effect when valid, listed in TARGET-EFFECTS and allowed by the source,
else the first of TARGET-EFFECTS the source allows, else :NONE."
  (let ((proposed (drop-object-proposed-effect drop-object)))
    (or (and (valid-drop-effect-p proposed)
             (member proposed target-effects :test #'eq)
             (drop-object-allows-drop-effect-p drop-object proposed)
             proposed)
        (find-if (lambda (effect)
                   (drop-object-allows-drop-effect-p drop-object effect))
                 target-effects)
        :none)))

(defun set-drop-object-supported-formats (drop-object formats)
  "Record FORMATS as the ones the target accepts."
  (setf (drop-object-formats drop-object) formats))

(defun drop-object-provides-format (drop-object format)
  "True when FORMAT is accepted by the target and present in the payload."
  (and (member format (drop-object-formats drop-object))
       (loop for tail on (drop-object-payload drop-object) by #'cddr
             thereis (eq (first tail) format))))

(defun drop-object-get-object (drop-object format)
  "The payload object carried in FORMAT, or NIL."
  (getf (drop-object-payload drop-object) format))
