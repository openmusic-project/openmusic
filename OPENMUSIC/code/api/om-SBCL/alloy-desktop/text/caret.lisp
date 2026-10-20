(in-package :alloy-desktop)

;;; Caret blink. Alloy's cursor visibility is static -- the default update is
;;; (:cursor :hidden-p (null alloy:focus)) (alloy presentations/default.lisp:
;;; define-update for (default-look-and-feel alloy:text-input-component)) --
;;; recomputed only when the renderable is dirty (alloy presentations/protocol.lisp:
;;; alloy:render :before for (renderer renderable)). The phase therefore comes from
;;; the wall clock, so every window blinks in step and a stalled frame loop cannot
;;; drift it, and each window state records the phase it last painted so the
;;; focused pane is marked exactly once per flip.
;;;
;;; The cycle is measured from an EPOCH rather than from zero, because an OS caret
;;; does not free-run: every edit or cursor move restarts it at the visible half,
;;; so the caret stays solid while the user types and only resumes blinking once
;;; they pause.

(defparameter *caret-blink-half-period* 0.5
  "Seconds the caret stays visible, then hidden: an OS-style caret cadence.")

(defvar *caret-blink-on* t
  "Current global blink phase, cached by TICK-CARET-BLINK for the :cursor updates.")

(defvar *caret-blink-epoch* (get-internal-real-time)
  "Start of the current blink cycle; RESET-CARET-BLINK moves it to now.")

(defun caret-blink-phase ()
  "True during the visible half of the current blink cycle."
  (evenp (floor (/ (- (get-internal-real-time) *caret-blink-epoch*)
                   internal-time-units-per-second)
                *caret-blink-half-period*)))

(defun reset-caret-blink ()
  "Restart the cycle at the visible half and let every window re-mark its caret on the next tick."
  (setf *caret-blink-epoch* (get-internal-real-time)
        *caret-blink-on* t)
  (loop for state being the hash-values of *window-states*
        do (setf (%state-caret-painted-phase state) :unset)))

;;; Typing and cursor movement both reach the focused component as an event, so the
;;; reset rides the event rather than alloy's move-to :after, which already exists
;;; (alloy components/text-input.lisp: move-to :after for (t cursor)) and would be
;;; silently replaced. Specializing on the alloy base class collides with nothing:
;;; host :after methods belong on subclasses.

(defmethod alloy:handle :after ((event alloy:text-event) (self alloy:text-input-component))
  (when (eql :strong (alloy:focus self)) (reset-caret-blink)))

(defmethod alloy:handle :after ((event alloy:key-down) (self alloy:text-input-component))
  (when (eql :strong (alloy:focus self)) (reset-caret-blink)))

(defun tick-caret-blink (window focused-text)
  "Cache the blink phase for focused WINDOW and, on a flip, mark the pane FOCUSED-TEXT returns for WINDOW."
  ;; FOCUSED-TEXT is a function so the host's pane lookup runs once per flip, not per frame
  (when (window-focused-p window)
    (let ((on (caret-blink-phase))
          (state (registered-window-state window)))
      (setf *caret-blink-on* on)
      (when (and state (not (eq on (%state-caret-painted-phase state))))
        (setf (%state-caret-painted-phase state) on)
        (let ((pane (funcall focused-text window)))
          (when pane (alloy:mark-for-render pane)))))))

(defun caret-hidden-p (widget window)
  "True when WIDGET, shown in WINDOW, must not paint its caret."
  (or (not (eql :strong (alloy:focus widget)))
      (null window)
      (not (window-focused-p window))
      (not *caret-blink-on*)))
