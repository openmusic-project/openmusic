(in-package :alloy-desktop)

(defstruct (window-retirement
            (:constructor %make-window-retirement (name function order)))
  name function order)

(defvar *window-retirements* nil)

(defun register-window-retirement (name function &key (order 50))
  "Run FUNCTION on a retiring window, ordered by ORDER and identified by NAME."
  (check-type name symbol)
  (check-type function function)
  (check-type order real)
  (unregister-window-retirement name)
  (push (%make-window-retirement name function order) *window-retirements*)
  (setf *window-retirements*
        (stable-sort *window-retirements* #'< :key #'window-retirement-order))
  name)

(defun unregister-window-retirement (name)
  "Remove the retirement callback identified by NAME."
  (setf *window-retirements*
        (delete name *window-retirements* :key #'window-retirement-name :test #'eq))
  name)

(defun window-retirement-names ()
  "Return retirement callback names in execution order."
  (mapcar #'window-retirement-name *window-retirements*))

(defun run-window-retirement (window &key (continue-on-error-p t))
  "Run every registered retirement callback for WINDOW in order.

When CONTINUE-ON-ERROR-P is true, report a failing callback and continue with
the remaining callbacks. When it is false, preserve the callback condition and
stop at the failing retirement step."
  (dolist (retirement *window-retirements* window)
    (if continue-on-error-p
        (handler-bind
            ((serious-condition
               (lambda (condition)
                 (format *error-output* "[window-retirement] ~S: ~A~%"
                         (window-retirement-name retirement) condition)
                 (uiop:print-condition-backtrace condition *error-output*)
                 (finish-output *error-output*))))
          (handler-case
              (funcall (window-retirement-function retirement) window)
            (serious-condition () nil)))
        (funcall (window-retirement-function retirement) window))))

(register-window-retirement :gl-context-resources
                            #'retire-gl-context-resources
                            :order 40)
