(in-package :alloy-desktop)

(defvar *default-timer-service* nil)

(defun default-timer-service ()
  (or *default-timer-service*
      (setf *default-timer-service* (make-instance 'timer-service))))

(defmethod start-timer ((service timer-service) interval function &key repeat)
  "Schedule FUNCTION after INTERVAL seconds and return the native timer object.

When REPEAT is true, reschedule with the same interval. The callback runs on a
separate timer thread, matching the independent-process behavior expected from
LispWorks MP timers."
  (declare (ignore service))
  (check-type interval (real 0 *))
  (let ((timer (sb-ext:make-timer (coerce function 'function) :thread t)))
    (if repeat
        (sb-ext:schedule-timer timer interval :repeat-interval interval)
        (sb-ext:schedule-timer timer interval))
    timer))

(defmethod stop-timer ((service timer-service) timer)
  (declare (ignore service))
  (sb-ext:unschedule-timer timer)
  timer)
