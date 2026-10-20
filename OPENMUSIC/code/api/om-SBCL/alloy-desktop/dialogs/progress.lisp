(in-package :alloy-desktop)

;;; A progress window: what a long operation is doing and a Cancel button, shown
;;; while the operation runs on a thread of its own and the event loop keeps
;;; running.  It is NOT modal; the caller decides what else to disable.  The
;;; window itself comes from MAKE-PROGRESS-SURFACE, so a host can draw it with its
;;; own widgets; the default builds it from this library's.

(defparameter *progress-margin* 8
  "Gap between a progress window and the corner of the work area it opens in.")

(defclass progress-window ()
  ((window :initform nil :accessor progress-window-native
           :documentation "The native window MAKE-PROGRESS-SURFACE built.")
   (message-function :initform nil :accessor %progress-message-function)
   (cancelled-p :initform nil :accessor progress-cancelled-p
                :documentation "Set by the Cancel button on the UI thread and read by the
operation on its own: a boolean handed over that way needs no lock of its own."))
  (:documentation "A shown progress window and the Cancel request it carries."))

(defgeneric make-progress-surface (host owner x y title message cancel-function)
  (:documentation "Build and SHOW the window of a progress window for OWNER whose
external frame has its top-left at the virtual-screen point X Y: TITLE, the text
MESSAGE and a Cancel button that calls CANCEL-FUNCTION with no arguments.  Answer
the window and a function of one string that replaces the message.  HOST is the
object the progress window was opened for; the default method builds the window
from this library's own window and widgets."))

(defmethod make-progress-surface (host owner x y title message cancel-function)
  (declare (ignore host))
  ;; the window shares OWNER's GL objects -- the font atlases among them -- as
  ;; every other window of the session does
  (let* ((window (make-window (desktop-screen) :title title :x x :y y
                                               :width 320 :height 120 :resizable-p nil
                                               :share (or owner (front-window))))
         (root (window-root-layout window))
         (text (make-instance 'static-text :text message)))
    (add-element root text :x 20 :y 22 :w 280 :h 38 :focus nil)
    (add-element root (make-instance 'button :text "Cancel"
                                             :on-activate (lambda () (funcall cancel-function)))
                 :x 120 :y 72 :w 80 :h 24)
    (show-window window)
    (values window (lambda (string) (setf (value text) string)))))

(defun progress-window-position (owner)
  "Top left corner of the work area OWNER stands on, inset by *PROGRESS-MARGIN*, as
(values X Y).

A progress window goes in that corner rather than over the document (LispWorks,
its print progress window), so what the operation works on stays visible.  The
monitor is the one carrying OWNER's frame (window/geometry.lisp:
%popup-monitor-for-screen-point), and the work area is the screen without the
taskbar, so the window lands where the user can reach it."
  (let* ((origin (and owner (window-registered-p owner)
                      (let ((bounds (window-bounds owner)))
                        (multiple-value-bind (fx fy)
                            (window-content-to-frame owner (alloy:pxx bounds) (alloy:pxy bounds))
                          (list (round fx) (round fy))))))
         (monitor (%popup-monitor-for-screen-point (if origin (first origin) 0)
                                                   (if origin (second origin) 0)))
         (work (and monitor (handler-case (%glfw:work-area monitor)
                              (%glfw-unavailable () nil)))))
    (if (and (consp work) (= 4 (length work)))
        (values (+ (first work) *progress-margin*)
                (+ (second work) *progress-margin*))
        ;; GLFW answers about no monitor: the corner of the primary screen is
        ;; still the corner the user is shown
        (values *progress-margin* *progress-margin*))))

(defun open-progress-window (host owner message &key (title "Document"))
  "Show a progress window for HOST in the corner of the work area OWNER stands on
and answer it.  The corner is decided BEFORE the window is shown: a window is built
where it belongs and shown once, never moved once the user can see it."
  (let ((progress (make-instance 'progress-window)))
    (multiple-value-bind (x y) (progress-window-position owner)
      (multiple-value-bind (window setter)
          (make-progress-surface host owner x y title message
                                 (lambda () (setf (progress-cancelled-p progress) t)))
        (setf (progress-window-native progress) window
              (%progress-message-function progress) setter)))
    progress))

(defun set-progress-message (progress text)
  "Replace the message of PROGRESS.  Call on the UI thread: a worker posts it there
(core/ui-thread.lisp: post-to-ui-thread)."
  (funcall (%progress-message-function progress) text))

(defun close-progress-window (progress)
  "Destroy the window of PROGRESS at once.  DEALLOCATE, not a deferred close: a
deferred close only raises GLFW's should-close flag and leaves the native window on
the screen until some later event-loop iteration reaps it."
  (let ((window (progress-window-native progress)))
    (when (and window (%glfw:pointer window))
      (alloy:deallocate window)))
  nil)
