(in-package :oa)

;;; SYMBOL-AT-POINT

(defun %symbol-char-p (c)
  (or (alphanumericp c)
      (find c "+-*/<>=!?_$%&^.:" :test #'char=)))

(defun om-symbol-at-point (text position &key package)
  "Return the symbol name string under or just left of POSITION within TEXT, or NIL."
  (declare (ignore package))
  (when (and text (plusp (length text)))
    (let* ((len  (length text))
           (pos  (max 0 (min position len)))
           (end  pos))
      (loop while (and (< end len)
                       (%symbol-char-p (char text end)))
            do (incf end))
      (let ((start end))
        (loop while (and (> start 0)
                         (%symbol-char-p (char text (1- start))))
              do (decf start))
        (when (< start end)
          (subseq text start end))))))

;;; EMACS MODE: the listener and the Lisp editor are Emacs buffers (SLIME)

(defvar *emacs-mode* nil
  "True when OpenMusic's listener and Lisp editor are Emacs buffers (set by the Emacs-mode executable).")

(defun om-emacs-eval (form &optional wait)
  "Evaluate the Emacs Lisp FORM in the connected Emacs, without waiting unless WAIT (swank swank.lisp: eval-in-emacs;
Emacs must set slime-enable-evaluate-in-emacs). Without a connection the request is reported, not signalled."
  (let ((connection (or swank::*emacs-connection* (swank::default-connection))))
    (if connection
        (handler-case (swank::with-connection (connection) (swank:eval-in-emacs form (not wait)))
          (error (c) (format *error-output* "~&[emacs-mode] ~A~%" (princ-to-string c))))
        (format *error-output* "~&[emacs-mode] no Emacs connection for ~S~%" form))))

;;; BACKTRACE

(defvar *error-backtrace* nil)

(defvar *ae-error-log* nil
  "Pathname of the OM Text Editor error log; NIL = computed lazily under
om-user-pref-folder (vanilla om-LW texteditor.lisp: defvar *ae-error-log* used local-appdata).")

(defun %ae-error-log-path ()
  (or *ae-error-log*
      (merge-pathnames (make-pathname :name "OM Text Editor" :type "log")
                       (om-user-pref-folder))))

(defun report-ae-error (cc)
  "Append condition CC + a backtrace to the editor error log (vanilla report-ae-error)."
  ;; file-error/full disk: logging must never compound the original condition
  (ignore-errors
    (with-open-file (log (%ae-error-log-path)
                         :direction :output :external-format :utf-8
                         :if-exists :append :if-does-not-exist :create)
      (format log "~&~A  Error ~S in OM Text Editor Process~%" (om-get-date) cc)
      (write-string (alld:capture-backtrace) log)
      (terpri log)))
  cc)

(defun om-show-error-backtrace ()
  "Last Error Backtrace: the stored error in the output window (om-LW lw-lisp-tools/eval-process.lisp:
om-show-error-backtrace), in Emacs mode in an Emacs buffer of its own."
  (cond ((null *error-backtrace*)
         (capi::beep-pane)
         (print "no backtrace recorded")
         nil)
        (*emacs-mode*
         (om-emacs-eval `(with-current-buffer (get-buffer-create "*OM Error Backtrace*")
                           (let ((inhibit-read-only t))
                             (erase-buffer)
                             (insert ,*error-backtrace*))
                           (goto-char (point-min))
                           (special-mode)
                           (display-buffer (current-buffer)))))
        (t
         (om-show-output-lines *error-backtrace* "Error Backtrace"))))

(defun %om-report-error (condition)
  "What a delivered image shows for an error (om-LW lw-lisp-tools/eval-process.lisp: om-error-handler): the box,
then the error and its backtrace, kept for Last Error Backtrace and printed in the listener."
  (let ((backtrace (alld:capture-backtrace)))
    (capi::display-message "ERROR: ~A~%" condition)
    (setf *error-backtrace* (print (format nil "ERROR: ~A~%~%~A" condition backtrace)))))

(defun om-error-handler (&rest l)
  (%om-report-error (car l))
  (abort))

(defun set-om-debugger ()
  "A delivered image reports errors with om-error-handler (om-LW lw-lisp-tools/eval-process.lisp: set-om-debugger).
In Emacs mode SLIME's own debugger answers the errors of the evaluations made from Emacs, in Emacs."
  (when (and (member :om-deliver *features*) (not *emacs-mode*))
    (setf *debugger-hook* 'om-error-handler)))

;;; Editor-support evaluation layer (swank-backed; internal to the bridge,
;;; consumed by texteditor/listener -- not part of the OM kernel API).

(defun %resolve-package (designator)
  ;; CLHS: NIL is a SYMBOL, so the null clause must precede the symbolp clause.
  (cond ((packagep designator) designator)
        ((null designator) *om-default-package*)
        ((or (stringp designator) (symbolp designator))
         (or (find-package designator) *om-default-package*))
        (t *om-default-package*)))

(defmacro with-om-eval-context ((&optional (package '*om-default-package*)) &body body)
  `(let* ((pkg (%resolve-package ,package))
          (*package* pkg)
          (swank::*buffer-package*   pkg)
          (swank::*buffer-readtable* *readtable*))
     ,@body))

(defun %capture-error-backtrace (condition)
  "Snapshot CONDITION + (limited) backtrace string into *ERROR-BACKTRACE*."
  (setf *error-backtrace*
        (format nil "ERROR: ~A~%~%~A" (princ-to-string condition) (alld:capture-backtrace))))

(defun om-eval-string (string &key (package *om-default-package*))
  "Evaluate every form in STRING in order, what it writes and what it returns going
to the listener as it happens -- vanilla never captures output, its redefined PRINT
is global (om-LW lw-lisp-tools/listener.lisp: init-listener). Evaluation stops at the first error: its
ERROR line reaches the listener and *ERROR-BACKTRACE* is set."
  (with-om-eval-context (package)
    (unless (%listener-output-target)
      (om-make-new-listener :input *listener-input*))
    ;; the stream publishes per line, so a form whose last write has no newline
    ;; would sit in the buffer until the next one
    (unwind-protect
         (with-input-from-string (in string)
           (handler-bind ((error (lambda (c)
                                   (%capture-error-backtrace c)
                                   (format *om-stream* "ERROR: ~A~%" (princ-to-string c))
                                   (return-from om-eval-string nil))))
             (%honoring-redefinition-warnings
               (loop for form = (read in nil in)
                     until (eq form in)
                     do (%report-eval-result
                         (let ((*standard-output* *om-stream*)
                               (*error-output*    *om-stream*)
                               (*trace-output*    *om-stream*))
                           (multiple-value-list (eval form))))))))
      (finish-output *om-stream*))))

(defun om-eval-region (string &key (package *om-default-package*) (lines 5) (width 80))
  "Evaluate a region of one or more forms. Returns the result string formatted for an echo area."
  (with-om-eval-context (package)
    (swank:interactive-eval-region string lines width)))

(defun om-eval-interactive (string &key (package *om-default-package*) (lines 5) (width 80))
  "Evaluate a single form for interactive feedback. Returns echo-area string."
  (with-om-eval-context (package)
    (swank:interactive-eval string lines width)))

(defun om-arglist-string (name &key (package *om-default-package*))
  "Return NAME's lambda list formatted as a string, or NIL when unknown or empty."
  (let ((pkg (%resolve-package package)))
    ;; swank/backend:arglist is sb-introspect:function-lambda-list: it signals
    ;; undefined-function when the fdefinition FBOUNDP just saw is redefined away
    (handler-bind ((undefined-function
                     (lambda (c)
                       (format *error-output* "[om-arglist-string] ~A~%" (princ-to-string c))
                       (return-from om-arglist-string nil))))
      (multiple-value-bind (sym found) (swank::parse-symbol name pkg)
        (when (and found sym (fboundp sym))
          (let ((args (swank/backend:arglist sym)))
            (when (and (listp args) args)
              (format nil "~(~a~)" args))))))))

(defun om-completions (prefix &key (package *om-default-package*))
  "Return (values names triples) for PREFIX (string).
NAMES is a list of completion strings; TRIPLES is the raw swank list of
\(name flags qualified-name) entries."
  (with-om-eval-context (package)
    (let* ((pkg-name (package-name (%resolve-package package)))
           (triples (swank:simple-completions prefix pkg-name))
           (names   (mapcar #'first triples)))
      (values names triples))))

(defun om-describe-symbol (name &key (kind :function) (package *om-default-package*))
  "Return full description string for NAME of KIND (:function :variable :class :type)."
  (with-om-eval-context (package)
    ;; swank's parse-symbol-or-lose signals a plain error for a name that does not
    ;; resolve; parse-symbol answers it as a value (swank swank.lisp: parse-symbol), so ask it
    (when (nth-value 1 (swank::parse-symbol name (%resolve-package package)))
      (swank:describe-definition-for-emacs name kind))))

