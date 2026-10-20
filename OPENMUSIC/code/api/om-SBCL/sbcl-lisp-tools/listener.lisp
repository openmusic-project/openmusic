(in-package :oa)

;;; ============================================================
;;; LISTENER
;;; ============================================================

;;;===================
;;; Globals
;;;===================

(defvar *om-app*     nil)
(defvar *om-listener* nil)
(defvar *om-repl*     nil)
(defvar *om-prompt*   "OM > ")

(defvar *listener-on-top* nil
  "When non-nil the OM Listener is created always-on-top (vanilla listener.lisp: defparameter *listener-on-top*);
the kernel reads/sets this as om-lisp::*listener-on-top* and recreates the listener.")

(defvar *listener-input* nil
  "Toggle that controls whether OM-MAKE-NEW-LISTENER builds the REPL pane.")

;;; Direct point sizes (om-LW lw-lisp-tools/listener.lisp: defvar *listener-font*).
(defparameter *listener-font*
  #+linux  (%om-make-font-pt "Liberation Mono" 9)
  #+darwin (%om-make-font-pt "Verdana" 12 :style '(:plain))
  #-(or linux darwin) (%om-make-font-pt "Verdana" 8 :style '(:plain)))

;;;===================
;;; OM-STREAM (gray output stream routing to live listener output pane)
;;;===================

(defclass om-listener-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((column :initform 0 :accessor om-stream-column)
   (target :initarg :target :initform #'%listener-output-append
           :reader om-stream-target)
   ;; text waits here until the UI publishes it, so the pane is touched once per
   ;; DRAIN instead of once per write (vanilla's collector-pane-stream is a native
   ;; buffered pane stream, om-LW listener.lisp: the collector-pane-stream of init-listener). COLUMN counts what was PUSHED,
   ;; not what was published, so FRESH-LINE sees the pending text too.
   (buffer :initform (make-array 256 :element-type 'character
                                     :adjustable t :fill-pointer 0)
           :reader om-stream-buffer)
   ;; at most one publish task may be queued; it drains the whole buffer when it runs
   (pending :initform nil :accessor om-stream-pending)
   (lock   :initform (bordeaux-threads:make-lock "om-listener-stream")
           :reader om-stream-lock)))

(defun %listener-output-target ()
  (when (and *om-listener* (alld:window-alive-p *om-listener*))
    (op *om-listener*)))

(defun %growable-text (string)
  "STRING itself when it can already grow in place, otherwise a growable copy."
  (if (and (adjustable-array-p string) (array-has-fill-pointer-p string))
      string
      (let ((new (make-array (max 1024 (* 2 (length string)))
                             :element-type 'character
                             :adjustable t :fill-pointer (length string))))
        (replace new string)
        new)))

(defun %listener-output-append (text)
  "Append TEXT to the listener output pane in place. The pane's value is kept
growable so a line costs the line, not the whole history; and the append is not an
edit, so it must not feed the undo history (which would retain two copies of the
whole text per line)."
  (let ((pane (%listener-output-target)))
    (when (and pane text (plusp (length text)))
      (let ((new (%growable-text (or (alloy:value pane) ""))))
        (loop for char across text do (vector-push-extend char new))
        (let ((suppressed (suppress-undo-p pane)))
          (setf (suppress-undo-p pane) t)
          (unwind-protect
               (setf (alloy:value pane) new)
            (setf (suppress-undo-p pane) suppressed)))
        (alloy:move-to (length new) (alloy:cursor pane))
        (alloy:mark-for-render pane)
        (alloy:notice-size pane t)
        (%scroll-end-into-view pane)))
    pane))

(defun %stream-drain (stream &optional (target (om-stream-target stream)))
  "Publish everything STREAM has buffered by now as ONE call to TARGET. Clearing PENDING in
the same critical section as the swap is what lets the next write queue a new task."
  (let ((text (bordeaux-threads:with-lock-held ((om-stream-lock stream))
                (setf (om-stream-pending stream) nil)
                (let ((buffer (om-stream-buffer stream)))
                  (when (plusp (fill-pointer buffer))
                    (prog1 (subseq buffer 0)
                      (setf (fill-pointer buffer) 0)))))))
    (when text
      (funcall target text))))

(defun %stream-flush (stream)
  "Ask the UI to publish the buffer, keeping at most ONE task queued: a burst then
costs one append/notice-size/refit per UI cycle instead of one per line, and a lone
line still goes out on the very next cycle (the drain is per task, not per volume).
In Emacs mode the text goes at once to the Emacs buffer *OM Listener*, the listener's
output pane (the SLIME REPL is its input pane)."
  (if *emacs-mode*
      ;; every window showing the buffer follows the end, as comint does (comint.el:
      ;; comint-output-filter); a window's point stays behind text inserted from elsewhere
      (%stream-drain stream (lambda (text)
                              (om-emacs-eval `(with-current-buffer (get-buffer-create "*OM Listener*")
                                                (goto-char (point-max))
                                                (insert ,text)
                                                (dolist (w (get-buffer-window-list (current-buffer) nil t))
                                                  (set-window-point w (point-max)))))))
      (when (bordeaux-threads:with-lock-held ((om-stream-lock stream))
              (and (plusp (fill-pointer (om-stream-buffer stream)))
                   (not (om-stream-pending stream))
                   (setf (om-stream-pending stream) t)))
        (alld:post-to-ui-thread (lambda () (%stream-drain stream))))))

(defmethod trivial-gray-streams:stream-write-char
    ((stream om-listener-stream) char)
  (bordeaux-threads:with-lock-held ((om-stream-lock stream))
    (vector-push-extend char (om-stream-buffer stream))
    (if (char= char #\Newline)
        (setf (om-stream-column stream) 0)
        (incf (om-stream-column stream))))
  (when (char= char #\Newline)
    (%stream-flush stream))
  char)

(defmethod trivial-gray-streams:stream-write-string
    ((stream om-listener-stream) string &optional (start 0) end)
  (let* ((real-end (or end (length string)))
         (newline-p nil))
    (bordeaux-threads:with-lock-held ((om-stream-lock stream))
      (let ((buffer (om-stream-buffer stream))
            (last-nl nil))
        (loop for i from start below real-end
              for char = (char string i)
              do (vector-push-extend char buffer)
                 (when (char= char #\Newline) (setf last-nl i)))
        (if last-nl
            (setf newline-p t
                  (om-stream-column stream) (- real-end last-nl 1))
            (incf (om-stream-column stream) (- real-end start)))))
    (when newline-p
      (%stream-flush stream))
    string))

(defmethod trivial-gray-streams:stream-line-column
    ((stream om-listener-stream))
  ;; the evaluation threads write COLUMN under this lock; FRESH-LINE reads it here
  (bordeaux-threads:with-lock-held ((om-stream-lock stream))
    (om-stream-column stream)))

(defmethod trivial-gray-streams:stream-finish-output
    ((stream om-listener-stream))
  (%stream-flush stream)
  nil)

(defmethod trivial-gray-streams:stream-force-output
    ((stream om-listener-stream))
  (%stream-flush stream)
  nil)

(defparameter *om-stream*
  (make-instance 'om-listener-stream)
  "Canonical OM output stream; format/print/write target this stream.")

(defun %om-print-value (object &optional stream)
  "The body vanilla installs as CL:PRINT (om-LW listener.lisp: the print redefined in init-listener): the requested
STREAM is ignored, *om-prompt* goes out unescaped, OBJECT escaped, then TERPRI, and
OBJECT is returned. FRESH-LINE is the fork's own guarantee that the prefix opens a
line even when another writer left the column dirty; the scroll to the end is
already done by every append (%listener-output-append)."
  (declare (ignore stream))
  (let ((s *om-stream*))
    (fresh-line s)
    (write *om-prompt* :stream s :escape nil)
    (write object :stream s :escape t)
    (terpri s)
    object))

(defun %install-om-print ()
  "Point CL:PRINT at %om-print-value, the redefinition vanilla does in init-listener
(om-LW listener.lisp: the print redefinition of init-listener); SBCL guards the COMMON-LISP package with a lock."
  (sb-ext:without-package-locks
    (setf (fdefinition 'cl:print) #'%om-print-value)))

;;;===================
;;; OM-REPL (standalone REPL window)
;;;===================

(defclass om-repl (om-look-window)
  ((input-pane :accessor input-pane :initform nil)))

(defclass om-repl-pane (om-paren-edit-view-widget)
  ((prompt-start    :accessor prompt-start    :initform 0)
   (prompt-number   :accessor prompt-number   :initform 1)
   (eval-history    :accessor eval-history    :initform nil)
   (history-index   :accessor history-index   :initform nil)
   (transient-input :accessor transient-input :initform nil)
   (eval-busy-p     :accessor eval-busy-p     :initform nil)
   (output-position :accessor repl-output-position :initform 0)
   (executor        :accessor repl-executor   :initform nil)
   ;; the REPL's own package cell: a CAPI listener-pane evaluates in its process,
   ;; whose *package* binding is private to that process
   (eval-package    :accessor eval-package    :initarg :eval-package :initform *package*)
   (oa::wrap-p      :initform t)))

(defstruct (om-listener-evaluator
             (:constructor %make-listener-evaluator
                 (pane &aux (loop-bindings
                             (lambda ()
                               (list (cons '*package*
                                           (%resolve-package (eval-package pane))))))))
             (:include alld:serial-worker
              (name "CAPI Execution OM Listener")
              (lock (bordeaux-threads:make-lock "om-listener-evaluator"))
              (condition (bordeaux-threads:make-condition-variable
                          :name "om-listener-evaluator"))
              (initial-bindings *process-initial-bindings*)
              (armed-traps '(:divide-by-zero))
              (job-function '%evaluate-listener-job)))
  "Persistent execution context owned by one Listener input pane."
  pane)

(defstruct (om-listener-job (:constructor %make-listener-job (pane expression)))
  pane
  expression)

(defun %repl-insert-output (pane text)
  "Insert evaluator TEXT at PANE's output marker, before any type-ahead text.
This function is called only by the Alloy UI task queue."
  (let ((window (om-view-window pane)))
    (when (and text (plusp (length text)) window (alld:window-alive-p window))
      (let* ((old (or (alloy:value pane) ""))
             (marker (min (max 0 (repl-output-position pane)) (length old)))
             (amount (length text))
             (cursor (alloy:cursor pane))
             (position (alloy:pos cursor))
             (anchor (alloy:anchor cursor))
             (new (concatenate 'string (subseq old 0 marker) text
                               (subseq old marker))))
        (let ((suppressed (suppress-undo-p pane)))
          (setf (suppress-undo-p pane) t)
          (unwind-protect
               (setf (alloy:value pane) new)
            (setf (suppress-undo-p pane) suppressed)))
        (setf (repl-output-position pane) (+ marker amount))
        (when (>= position marker)
          (alloy:move-to (+ position amount) cursor))
        (when (and anchor (>= anchor marker))
          (setf (alloy:anchor cursor) (+ anchor amount)))
        (alloy:mark-for-render pane)
        (alloy:notice-size pane t)
        (%scroll-end-into-view pane)))
    text))

(defun %finish-listener-job (pane stream package)
  "Publish STREAM's tail, restore PANE's package and insert the next prompt."
  (when (and pane (om-view-window pane)
             (alld:window-alive-p (om-view-window pane)))
    (%stream-drain stream)
    (setf (eval-package pane) package)
    (incf (prompt-number pane))
    (%repl-insert-output pane (%build-prompt pane))
    (setf (prompt-start pane) (repl-output-position pane)
          (eval-busy-p pane) nil)))

(defun %evaluate-listener-job (job)
  "Evaluate JOB in the persistent Listener thread and marshal all pane writes."
  (let* ((pane (om-listener-job-pane job))
         (expression (om-listener-job-expression job))
         (stream (make-instance
                  'om-listener-stream
                  :target (lambda (text) (%repl-insert-output pane text)))))
    (unwind-protect
         (with-simple-restart (abort "Return to the OM Listener prompt.")
           (block eval-forms
             (with-input-from-string (input expression)
               (handler-bind
                   ((error
                      (lambda (condition)
                        ;; a delivered image reports through its debugger hook and the prompt only
                        ;; comes back (eval-support.lisp: om-error-handler)
                        (cond ((om-standalone-p)
                               (%om-report-error condition))
                              (t
                               (%capture-error-backtrace condition)
                               (format stream "ERROR: ~A~%~%"
                                       (princ-to-string condition))))
                        (return-from eval-forms nil))))
                 (loop for form = (read input nil input)
                       until (eq form input)
                       do
                          (let ((values
                                  (let ((*standard-output* stream)
                                        (*error-output* stream)
                                        (*trace-output* stream))
                                    (multiple-value-list (eval form)))))
                            (let ((*print-right-margin* most-positive-fixnum)
                                  (*print-miser-width* nil))
                              (format stream "~{~S~^~%~}~%~%" values))))))))
      (finish-output stream)
      (let ((package *package*))
        (alld:post-to-ui-thread
         (lambda () (%finish-listener-job pane stream package)))))))

(defun %ensure-listener-evaluator (pane)
  "Return PANE's live persistent evaluator, creating it when necessary."
  (let ((evaluator (repl-executor pane)))
    (unless (and evaluator
                 (om-listener-evaluator-thread evaluator)
                 (bordeaux-threads:thread-alive-p
                  (om-listener-evaluator-thread evaluator)))
      (setf evaluator (%make-listener-evaluator pane)
            (repl-executor pane) evaluator)
      (alld:with-serial-worker-lock (evaluator)
        (alld:serial-worker-start evaluator)))
    evaluator))

(defun %enqueue-listener-expression (pane expression)
  "Queue EXPRESSION on PANE's persistent evaluator without blocking Alloy."
  (let ((evaluator (%ensure-listener-evaluator pane)))
    (setf (eval-busy-p pane) t
          (repl-output-position pane) (length (or (alloy:value pane) "")))
    (alld:serial-worker-enqueue evaluator (%make-listener-job pane expression))
    t))

(defun %stop-listener-evaluator (pane)
  "Terminate PANE's evaluator and discard work belonging to the closing pane."
  (let ((evaluator (and pane (repl-executor pane))))
    (when evaluator
      (alld:serial-worker-request-stop evaluator)
      (alld:serial-worker-destroy-thread evaluator)
      (setf (om-listener-evaluator-thread evaluator) nil
            (repl-executor pane) nil
            (eval-busy-p pane) nil)))
  nil)

;; 3 blank lines under the prompt, for reading comfort and scroller travel
(defmethod %trailing-blank-lines ((pane om-repl-pane)) 3)

(defun %build-prompt (pane)
  (format nil "~A ~D > "
          (package-name (%resolve-package (eval-package pane)))
          (prompt-number pane)))

(defun %append-to-repl (pane text)
  (let* ((cur (or (alloy:value pane) ""))
         (new (concatenate 'string cur text)))
    (setf (alloy:value pane) new)
    (alloy:move-to (length new) (alloy:cursor pane))
    (alloy:mark-for-render pane)
    (alloy:notice-size pane t)
    (%scroll-end-into-view pane)
    new))

(defmethod alloy:handle :after ((event alloy:text-event) (pane om-repl-pane))
  (alloy:notice-size pane t)
  (%recompute-paren-match pane))

(defmethod alloy:handle :after ((event alloy:key-down) (pane om-repl-pane))
  (when (member (alloy:key event) '(:backspace :delete :enter :return :kp-enter))
    (alloy:notice-size pane t))
  (%recompute-paren-match pane))

(defmethod (setf alloy:value) :after (new-value (pane om-repl-pane))
  "Keep the pane's markers inside its buffer. Every edit -- typing, delete, cut,
paste -- reaches the text through (setf text), which ends in (setf value)
(alloy components/text-input.lisp: (setf text) and delete-text for text-input-component), so a buffer that shrinks passes here. PROMPT-START and
REPL-OUTPUT-POSITION index that buffer: past its end, input reads as empty and the
next SUBSEQ signals."
  (let ((len (length (or new-value ""))))
    (when (> (prompt-start pane) len)
      (setf (prompt-start pane) len))
    (when (> (repl-output-position pane) len)
      (setf (repl-output-position pane) len))))

(defmethod alloy:handle :after ((event alloy:pointer-up) (pane om-repl-pane))
  (%recompute-paren-match pane))

(defun %insert-prompt (pane)
  (let ((new (%append-to-repl pane (%build-prompt pane))))
    (setf (prompt-start pane) (length new))))

(defun %reset-repl-prompt (pane)
  "Drop PANE's contents and open a fresh prompt. Output is inserted at
REPL-OUTPUT-POSITION and input is read from PROMPT-START, so both must name the end of
the new prompt -- the state %finish-listener-job leaves behind. Every slot is written
before the layout calls, which reach the MSDF cursor metrics and can fail outside a
render pass: a pane left with a stale PROMPT-START reads no input at all."
  (let* ((prompt (%build-prompt pane))
         (end    (length prompt))
         (cursor (alloy:cursor pane)))
    (setf (alloy:value pane) prompt)
    (setf (prompt-start pane) end)
    (setf (repl-output-position pane) end)
    (setf (history-index pane) nil)
    (setf (transient-input pane) nil)
    (setf (alloy:anchor cursor) nil)
    (alloy:move-to end cursor)
    (alloy:mark-for-render pane)
    (alloy:notice-size pane t)
    (%scroll-end-into-view pane)))

(defun %expression-complete-p (string)
  "True when STRING holds AT LEAST ONE form and nothing left unfinished. The reader
decides: an END-OF-FILE inside an object is the CLHS-guaranteed signal of an
unfinished form (f_rd_rd.htm:91, Exceptional Situations) -- that one named case is the mechanism here, and
every other reader condition means complete-but-invalid, which must reach the eval.
Blank or comment-only input holds no form to evaluate, so ENTER only breaks the
line. The detection read must not run #. side effects, hence *READ-EVAL* NIL."
  (let ((*read-eval* nil)
        (forms 0))
    (handler-case
        (with-input-from-string (in string)
          (loop for form = (read in nil in)
                until (eq form in)
                do (incf forms))
          (plusp forms))
      (end-of-file () nil)
      (error () t))))

(defun %eval-current-expression (pane)
  (let* ((buf   (or (alloy:value pane) ""))
         (start (prompt-start pane))
         (expr  (if (<= start (length buf))
                     (subseq buf start)
                     ""))
         ;; the history holds the EXPRESSION, never the Linefeed the Enter key put
         ;; before it: %replace-input parks the caret at (length new), which
         ;; must be the closing paren and not the empty line under it. Trimming also
         ;; subsumes the blank-input test -- whitespace alone trims to "".
         (trimmed (string-right-trim '(#\Linefeed #\Return #\Space #\Tab) expr)))
    (when (plusp (length trimmed))
      (push trimmed (eval-history pane)))
    (setf (history-index pane)   nil)
    (setf (transient-input pane) nil)
    (%enqueue-listener-expression pane expr)))

(defun %replace-input (pane new-text)
  (let* ((buf    (or (alloy:value pane) ""))
         (start  (prompt-start pane))
         (prefix (subseq buf 0 start))
         (new    (concatenate 'string prefix (or new-text ""))))
    (setf (alloy:value pane) new)
    (alloy:move-to (length new) (alloy:cursor pane))
    (alloy:mark-for-render pane)
    (alloy:notice-size pane t)
    new))

(defun %current-input (pane)
  (let* ((buf   (or (alloy:value pane) ""))
         (start (prompt-start pane)))
    (if (<= start (length buf))
        (subseq buf start)
        "")))

(defun %navigate-history (pane direction)
  (let* ((history (eval-history pane))
         (len     (length history))
         (idx     (history-index pane)))
    (when (plusp len)
      (case direction
        (:back
         (cond ((null idx)
                (setf (transient-input pane) (%current-input pane))
                (setf (history-index pane) 0)
                (%replace-input pane (nth 0 history)))
               ((< idx (1- len))
                (incf (history-index pane))
                (%replace-input pane (nth (history-index pane) history)))))
        (:forward
         (cond ((null idx))
               ((zerop idx)
                (setf (history-index pane) nil)
                (%replace-input pane (or (transient-input pane) ""))
                (setf (transient-input pane) nil))
               (t
                (decf (history-index pane))
                (%replace-input pane (nth (history-index pane) history)))))))))

(defun %repl-lowercase-region (pane)
  "Ctrl+L in the listener: LW binds it to Lowercase Region under Microsoft editor
emulation (LispWorks config/msw-key-binds.lisp: the pc-bind-key of Lowercase Region), which lowercases the text between point
and mark and complains when no mark is set. The alloy cursor's ANCHOR is that mark."
  (let* ((cursor (alloy:cursor pane))
         (anchor (alloy:anchor cursor)))
    (if (null anchor)
        (om-repl-echo "No mark has been set in buffer OM Listener" t)
        (let* ((start (prompt-start pane))
               (pos   (alloy:pos cursor))
               (from  (max start (min anchor pos)))
               (to    (max start (max anchor pos)))
               (text  (alloy:value pane)))
          (when (< from to)
            (setf (alloy:value pane)
                  (concatenate 'string
                               (subseq text 0 from)
                               (string-downcase (subseq text from to))
                               (subseq text to)))
            (alloy:mark-for-render pane))))))

(defmethod alloy:handle :around ((event alloy:key-down) (pane om-repl-pane))
  (cond ((%completion-route-key pane event) t)
        (t (multiple-value-prog1 (call-next-method)
             (%completion-after-key pane event)))))

(defmethod alloy:handle ((event alloy:key-down) (pane om-repl-pane))
  (let* ((start  (prompt-start pane))
         (cursor (alloy:cursor pane))
         (pos    (alloy:pos cursor))
         (mods   (alloy:modifiers event)))
    (case (alloy:key event)
      ((:enter :return :kp-enter)
       (cond ((find :control mods)
              (alloy:insert-text (string #\Linefeed) pane))
             ((eval-busy-p pane)
              ;; CAPI's interface remains editable while its Listener process is
              ;; evaluating. Keep type-ahead behind the evaluator output marker.
              (alloy:insert-text (string #\Linefeed) pane))
             (t
              (alloy:insert-text (string #\Linefeed) pane)
              (when (%expression-complete-p (%current-input pane))
                (%eval-current-expression pane)))))
      (:backspace
       (cond ((zerop pos)
              (om-repl-echo "No previous character." t) t)
             ((<= pos start)
              (om-repl-echo "Trying to delete text from the prompt." t) t)
             (t (call-next-method))))
      (:left
       (if (<= pos start) t (call-next-method)))
      (:home
       (alloy:move-to start cursor)
       t)
      (:up
       (%navigate-history pane :back)
       t)
      (:down
       (%navigate-history pane :forward)
       t)
      (:end
       (alloy:move-to :end cursor)
       t)
      (:tab
       (%show-completion-popup pane :package (eval-package pane) :min-start start)
       t)
      ((:l :|L|)
       (cond ((find :control mods)
              (%repl-lowercase-region pane)
              t)
             (t (call-next-method))))
      (t
       (call-next-method)))))

(defmethod alloy:handle ((event alloy:focus-next) (pane om-repl-pane))
  t)

(defmethod alloy:handle ((event alloy:text-event) (pane om-repl-pane))
  (let* ((start  (prompt-start pane))
         (cursor (alloy:cursor pane))
         (pos    (alloy:pos cursor)))
    (when (< pos start)
      (alloy:move-to (length (or (alloy:value pane) "")) cursor)))
  (call-next-method))

(defun %install-repl-panes (listener)
  (let* ((root   (om-window-root  listener))
         (focus  (om-window-focus listener))
         (data   (make-instance 'alloy:value-data :value ""))
         (repl   (make-instance 'om-repl-pane :data data))
         (scroll (make-instance 'alloy:scroll-view
                                :layout repl
                                :focus  repl
                                :focus-parent focus
                                :scroll :y)))
    (alloy:enter (alloy:layout-element scroll) root
                 :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1))
    (alloy:refit (om-window-chrome listener))
    (%om-focus-pane listener repl)
    (setf (input-pane listener) repl)
    listener))

(defun %run-initial-lambda (initial-lambda pane)
  "Call INITIAL-LAMBDA under a private *package* binding and hand the package it
leaves to PANE -- what capi:execute-with-interface gives vanilla by running the
lambda in the listener's own process (om-LW listener.lisp: the :create-callback of make-om-listener)."
  (handler-bind ((error
                   (lambda (c)
                     (format *om-stream* "~%[initial-lambda] ~A~%" (princ-to-string c))
                     (return-from %run-initial-lambda nil))))
    (let ((*package* *package*))
      (funcall initial-lambda)
      (when pane (setf (eval-package pane) *package*)))))

(defun om-make-new-repl (&key title x y width height initial-lambda (input t))
  (declare (ignore input))
  (when *om-repl*
    (om-close-window *om-repl*))
  (let* ((listener (om-make-window 'om-repl
                                   :window-title (or title "OM REPL")
                                   :position (when (and x y) (om-make-point x y))
                                   :size (om-make-point (or width 600) (or height 400))
                                   :bg-color (or *text-bg-color* *om-white-color*))))
    (setf *om-repl* listener)
    (%install-repl-panes listener)
    (om-show-window listener)
    (when initial-lambda
      (%run-initial-lambda initial-lambda (input-pane listener)))
    (%insert-prompt (input-pane listener))
    listener))

(defun %echo-hide (strip)
  "Remove the echo label from its border-layout (alloy:leave) so the south band
collapses fully. A border-layout south cell clamps to max(size, suggest-size)
(alloy layouts/border.lisp: refit for border-layout), so resizing to 0 only shrinks; leaving truly hides."
  (when (and (echo-strip-p strip) (echo-strip-box strip) (echo-strip-shown strip))
    (let ((box (echo-strip-box strip)))
      (alloy:leave (echo-strip-label strip) box)
      (alloy:refit box)
      (alloy:mark-for-render box)
      (setf (echo-strip-shown strip) nil))))

(defun %echo-show (strip str &optional error-p)
  "Show STR in STRIP. ERROR-P => the editor error highlight (white on orangered, as in
the LispWorks editor); otherwise the normal echo face (black on bg)."
  (when (and (echo-strip-p strip) (echo-strip-box strip) str (plusp (length str)))
    (let ((label (echo-strip-label strip))
          (box   (echo-strip-box strip)))
      (if error-p
          (progn (om-set-bg-color label *echo-error-bg-color*)
                 (om-set-fg-color label *om-white-color*))
          (progn (om-set-bg-color label (or *text-bg-color* *om-white-color*))
                 (om-set-fg-color label *om-black-color*)))
      (setf (alloy:value label) str)
      (unless (echo-strip-shown strip)
        (alloy:enter label box :place :south :size (alloy:un *echo-strip-height*))
        (setf (echo-strip-shown strip) t))
      (alloy:refit box)
      (alloy:mark-for-render box))))

(defun om-repl-echo (str &optional error-p)
  "Flash STR in the REPL (input) pane's echo strip (cleared on the next command).
ERROR-P renders it as an editor-error (white on orangered)."
  (when (and *om-listener* (alld:window-alive-p *om-listener*))
    (alld:post-to-ui-thread (lambda () (%echo-show (ip-echo *om-listener*) str error-p)))))

(defun om-repl-abort ()
  nil)

(defun set-bg-repl-color (color)
  (setf *text-bg-color* color)
  (when *om-repl*
    (om-set-bg-color *om-repl* color)))

;;;=============================================================
;;; OM LISTENER (aggregate window: MENU + REPL? + OUTPUT + BUFFER)
;;;=============================================================

(defclass om-listener (om-look-window)
  ((ip      :accessor ip      :initform nil)
   (op      :accessor op      :initform nil)
   (op-echo :accessor op-echo :initform nil)
   (ip-echo :accessor ip-echo :initform nil)))

;;; The EDITOR family of the measured selection matrix: here the highlight
;;; stays painted while the window sits in the background. An eval fired from a
;;; patch extends the output selection to the end of what it prints, and the very
;;; blur that hands the patch the focus must not blank it. om-text-editor covers
;;; the standalone Lisp editor, OMLispPatch and the TEXTFILE document window
;;; (om-text-edit-window / om-lisp-edit-window subclass it).
(defmethod alld:selection-survives-blur-p ((window om-text-editor)) t)
(defmethod alld:selection-survives-blur-p ((window om-repl)) t)
(defmethod alld:selection-survives-blur-p ((window om-listener)) t)

(defclass om-listener-divider (alloy:resizer)
  ((parent-layout :initarg :parent-layout :accessor divider-parent)
   (target-place  :initarg :target-place  :accessor divider-place :initform :north)))

(presentations:define-realization (alld:window om-listener-divider)
  ((:background simple:rectangle)
   (alloy:margins))
  ((:edge simple:rectangle)
   (alloy:margins 0 0 0 1)
   :pattern *om-gray-color*))

(presentations:define-update (alld:window om-listener-divider)
  (:background :pattern *om-window-def-color*)
  (:edge       :pattern *om-gray-color*))

(defmethod alloy:drag-to (location (d om-listener-divider))
  (let* ((parent      (divider-parent d))
         (target      (alloy:data d))
         (init-pos    (alloy:initial-pos d))
         (init-bounds (alloy::initial-bounds d)))
    (when (and parent target init-pos init-bounds)
      (let* ((dy       (- (alloy:pxy location) (alloy:pxy init-pos)))
             (init-h   (alloy:pxh init-bounds))
             (parent-h (alloy:pxh (alloy:bounds parent)))
             (max-h    (max 60 (- parent-h 60)))
             (new-h    (max 40 (min max-h (- init-h dy)))))
        (alloy:update target parent
                      :place (divider-place d)
                      :size  (alloy:px new-h))
        (alloy:notice-size target parent)))))

(defun %deep-focused (focus)
  "Walk through nested focus-lists (scroll-view internals) to the actual focused element."
  (loop for current = focus then sel
        for sel = (and current (alloy:focused current))
        while (typep sel 'alloy:focus-list)
        finally (return sel)))

(defun %listener-focused-pane (listener)
  (let* ((focus (om-window-focus listener))
         (sel   (%deep-focused focus)))
    (cond ((and (ip listener) (eq sel (ip listener))) (ip listener))
          ((and (op listener) (eq sel (op listener))) (op listener))
          (t (or (op listener) (ip listener))))))

(defun %listener-dispatch (listener event-class)
  (let ((pane (%listener-focused-pane listener)))
    (when pane
      (alloy:handle (make-instance event-class) pane))))

(defun %listener-paste (listener)
  (let* ((pane (%listener-focused-pane listener))
         (clip (and pane (alloy:clipboard listener))))
    (when (and pane clip)
      (alloy:handle (make-instance 'alloy:paste-event :content clip) pane))))

(defun %listener-select-all (listener)
  (let ((pane (%listener-focused-pane listener)))
    (when pane
      (let ((cursor (alloy:cursor pane)))
        (setf (alloy:anchor cursor) 0)
        (alloy:move-to :end cursor)
        (alloy:mark-for-render pane)))))

(defun %listener-clear-all (listener)
  (let ((pane (%listener-focused-pane listener)))
    (when pane
      ;; CAPI's listener-pane owns its prompt and reopens one after the kill
      ;; (om-LW listener.lisp: listener-clear-all); here the prompt lives in the pane's own
      ;; buffer, so clearing it without reopening one leaves the pane unable to read.
      (if (typep pane 'om-repl-pane)
          (%reset-repl-prompt pane)
          (let ((cursor (alloy:cursor pane)))
            (setf (alloy:value pane) "")
            (setf (alloy:anchor cursor) nil)
            (alloy:move-to 0 cursor)
            (alloy:mark-for-render pane)
            (alloy:notice-size pane t))))))

(defun %listener-set-font (listener font)
  (when font
    (setf *listener-font* font)
    (when (ip listener) (om-set-font (ip listener) font))
    (when (op listener) (om-set-font (op listener) font))
    (when (op-echo listener) (om-set-font (echo-strip-label (op-echo listener)) font))
    (when (ip-echo listener) (om-set-font (echo-strip-label (ip-echo listener)) font))))

(defun %listener-text-font (listener)
  (let ((font (om-choose-font-dialog)))
    (when font (%listener-set-font listener font))))

(defun %listener-load-file (listener)
  (declare (ignore listener))
  (let ((path (om-choose-file-dialog
               :prompt "Load Lisp File"
               :types '("Lisp Files" "lisp" "Compiled Files" "fasl"))))
    (when (and path (probe-file path))
      (handler-bind ((error
                       (lambda (c)
                         (format *om-stream* "~%ERROR loading ~A: ~A~%"
                                 (namestring path) (princ-to-string c))
                         (return-from %listener-load-file nil))))
        (let ((*package* *om-default-package*))
          (load path :verbose nil :print nil))
        (format *om-stream* "~%; loaded ~A~%" (namestring path))))))

(defun %listener-find-definition (listener)
  (let* ((pane   (%listener-focused-pane listener))
         (text   (and pane (alloy:value pane)))
         (pos    (and pane (alloy:pos (alloy:cursor pane))))
         (name   (and text pos (om-symbol-at-point text pos)))
         (symbol (and name (%best-symbol-for-name name *package*))))
    (cond ((null name)
           (format *om-stream* "~%no symbol at point~%"))
          ((not (and symbol (symbolp symbol)))
           (format *om-stream* "~%~A: not a resolvable symbol~%" name))
          (t
           (edit-definition symbol nil
                            (lambda (msg) (format *om-stream* "~%~A~%" msg)))))))

(defun %listener-new-text-editor (listener)
  (declare (ignore listener))
  (om-make-text-edit-window :title "Untitled" :lisp-mode t))

(defun %listener-open-text-file (listener)
  (declare (ignore listener))
  (let ((path (om-choose-file-dialog :prompt "Open File")))
    (when path
      (let ((existing (%find-open-editor-for-file path)))
        (if existing
            (%focus-existing-editor existing)
            (let ((window (om-make-text-edit-window
                           :title (file-namestring path)
                           :lisp-mode t)))
              (when window
                (%load-text-into-window window path))
              window))))))

(defun %listener-file-menu (listener)
  (om-make-menu
   "File"
   (list (list (om-new-leafmenu "Close Listener" (lambda () (om-close-window listener)) #\w))
         (list (om-new-leafmenu "New..."  (lambda () (%listener-new-text-editor listener)) #\n)
               (om-new-leafmenu "Open..." (lambda () (%listener-open-text-file listener)) #\o)))))

(defun %listener-edit-menu (listener)
  (om-make-menu
   "Edit"
   (list (list (om-new-leafmenu "Cut"   (lambda () (%listener-dispatch listener 'alloy:cut-event)) #\x)
               (om-new-leafmenu "Copy"  (lambda () (%listener-dispatch listener 'alloy:copy-event)) #\c)
               (om-new-leafmenu "Paste" (lambda () (%listener-paste listener)) #\v))
         (list (om-new-leafmenu "Select All" (lambda () (%listener-select-all listener)) #\a)
               (om-new-leafmenu "Clear All"  (lambda () (%listener-clear-all listener)) #\z))
         (list (om-new-leafmenu "Text Font"  (lambda () (%listener-text-font listener)))))))

(defun %listener-lisp-menu (listener)
  (om-make-menu
   "Lisp"
   (list (list (om-new-leafmenu "Find Definition"      (lambda () (%listener-find-definition listener)) #\.)
               (om-new-leafmenu "Abort"                (lambda () (om-listener-abort)) #\A)
               (om-new-leafmenu "Last Error Backtrace"
                                (lambda () (om-show-error-backtrace)) #\B
                                (lambda () (and *error-backtrace* t))))
         (list (om-new-leafmenu "Load File..."         (lambda () (%listener-load-file listener)))))))

(defun %listener-windows-menu (listener)
  (declare (ignore listener))
  (om-make-menu
   "Windows"
   (list
    (list (om-new-leafmenu "Preferences"   (constantly nil) #\,))
    (list (om-new-leafmenu "Workspace"     (constantly nil) #\W)
          (om-new-leafmenu "Library"       (constantly nil) #\P))
    (list (om-new-leafmenu "MIDI Mixer"    (constantly nil)))
    (list (om-new-leafmenu "Lisp Listener" (constantly nil) #\L nil)
          (om-new-leafmenu "Lisp Editor"
                           (lambda ()
                             (om-make-text-edit-window :title "Untitled" :lisp-mode t)))))))

(defmethod internal-window-class-menubar ((self om-listener))
  (list (%listener-file-menu self)
        (%listener-edit-menu self)
        (%listener-lisp-menu self)))

(defmethod om-window-class-menubar ((self om-listener))
  (list (%listener-windows-menu self)))

(defun %make-output-pane (focus bg-color)
  (let* ((data   (make-instance 'alloy:value-data :value ""))
         (pane   (make-instance 'om-eval-output-pane :data data))
         (scroll (make-instance 'alloy:scroll-view
                                :layout pane
                                :focus  pane
                                :focus-parent focus
                                :scroll :y)))
    (when bg-color (om-set-bg-color pane bg-color))
    (om-set-font pane *listener-font*)
    (values pane scroll)))

(defun %make-repl-pane (focus bg-color)
  (let* ((data   (make-instance 'alloy:value-data :value ""))
         (pane   (make-instance 'om-repl-pane :data data))
         (scroll (make-instance 'alloy:scroll-view
                                :layout pane
                                :focus  pane
                                :focus-parent focus
                                :scroll :y)))
    (when bg-color (om-set-bg-color pane bg-color))
    (om-set-font pane *listener-font*)
    (values pane scroll)))

(defun %make-echo-label (focus)
  "A per-pane echo minibuffer: a 1-line EDITABLE field; om-editable-text
self-clips, so collapsing the strip to 0px truly hides it. The status
strip is gray on EVERY OS -- the echo area belongs to the window chrome, not to
a white text field, so the gray is the correct look everywhere and not a
Windows-only adjustment."
  (let ((field (make-instance 'om-editable-text
                              :data (make-instance 'alloy:value-data :value ""))))
    (om-set-bg-color field *om-light-gray-color*)
    (om-set-font field *listener-font*)
    (when focus (alloy:enter field focus))
    field))

(defun %make-pane-section (scroll-element focus &optional initial-text)
  "Wrap SCROLL-ELEMENT with a per-pane echo minibuffer (editable field). With
INITIAL-TEXT the strip starts VISIBLE showing it (the output status bar, e.g.
\"Ready.\"); without it the strip starts hidden/collapsed (the REPL minibuffer,
shown only on a message). Returns (values section echo-strip)."
  (let* ((section (make-instance 'alloy:border-layout :padding (alloy:margins 0)))
         (field   (%make-echo-label focus))
         (strip   (%make-echo-strip field section)))
    (alloy:enter scroll-element section :place :center)
    (if initial-text
        (progn
          (setf (alloy:value field) initial-text
                (echo-strip-shown strip) t)
          (alloy:enter field section :place :south :size (alloy:un *echo-strip-height*)))
        (setf (echo-strip-shown strip) nil))
    (values section strip)))

(defun %install-listener-content (listener input-mode bg-color)
  (let* ((root  (om-window-root  listener))
         (focus (om-window-focus listener))
         (outer (make-instance 'alloy:border-layout :padding (alloy:margins 0))))
    (alloy:enter outer root :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1))
    (cond
      (input-mode
       (let ((inner (make-instance 'alloy:border-layout :padding (alloy:margins 0))))
         (alloy:enter inner outer :place :center)
         (multiple-value-bind (out-pane out-scroll)
             (%make-output-pane focus bg-color)
           (setf (op listener) out-pane)
           (multiple-value-bind (out-section out-echo)
               (%make-pane-section (alloy:layout-element out-scroll) focus "Ready.")
             (setf (op-echo listener) out-echo)
             (alloy:enter out-section inner :place :center)))
         (multiple-value-bind (in-pane in-scroll)
             (%make-repl-pane focus bg-color)
           (setf (ip listener) in-pane)
           (let* ((repl-section (make-instance 'alloy:border-layout :padding (alloy:margins 0)))
                  (divider      (make-instance 'om-listener-divider
                                               :side :south
                                               :data repl-section
                                               :parent-layout inner
                                               :target-place :north)))
             ;; no initial-text: this strip starts collapsed and shows only on a
             ;; message, unlike the output pane's persistent "Ready." bar
             (multiple-value-bind (in-section in-echo)
                 (%make-pane-section (alloy:layout-element in-scroll) focus)
               (setf (ip-echo listener) in-echo)
               (alloy:enter in-section repl-section :place :center))
             (alloy:enter divider repl-section :place :south :size (alloy:un 5))
             (alloy:enter divider focus)
             (alloy:enter repl-section inner :place :north :size (alloy:un 80))))))
      (t
       (multiple-value-bind (out-pane out-scroll)
           (%make-output-pane focus bg-color)
         (setf (op listener) out-pane)
         (multiple-value-bind (out-section out-echo)
             (%make-pane-section (alloy:layout-element out-scroll) focus "Ready.")
           (setf (op-echo listener) out-echo)
           (alloy:enter out-section outer :place :center)))))
    (alloy:refit (om-window-chrome listener))
    (let ((target (or (ip listener) (op listener))))
      (when target (%om-focus-pane listener target)))
    listener))

(defun %close-emacs-buffers ()
  "Exit hook of Emacs mode: kill the buffer *OM Listener* and end the inferior Lisp with its buffer (the process
SLIME names inferior-lisp, slime.el: slime-start-lisp): its buffer goes when it dies, and it is killed if still
alive 2 s later. Waits for Emacs so the request leaves before the process."
  (om-emacs-eval '(progn (when (get-buffer "*OM Listener*") (kill-buffer "*OM Listener*"))
                         (let ((proc (get-process "inferior-lisp")))
                           (when proc
                             (set-process-sentinel proc (lambda (p event)
                                                          (unless (process-live-p p)
                                                            (kill-buffer (process-buffer p)))))
                             ;; SLIME evaluates with dynamic binding (slime.el: slime-eval-for-lisp):
                             ;; the process reaches the timer as its argument, not as a closure
                             (run-with-timer 2 nil (lambda (p) (when (process-live-p p) (kill-process p)))
                                             proc))))
                 t))

(defun om-make-new-listener (&key title x y width height initial-lambda
                                  (input t))
  "Create (or focus the existing) OM Listener window.
INPUT: T => with REPL pane; NIL => only OUTPUT + echo BUFFER.
In Emacs mode the listener is Emacs: the buffer *OM Listener* for the output and the SLIME
REPL for the input, whatever INPUT; no window, NIL. When the process exits, *OM Listener*, the inferior
Lisp and its buffer are closed."
  (setf *trace-output* *om-stream*)
  (%install-om-print)
  (cond
    (*emacs-mode*
     ;; a SLIME evaluation result is written as the listener writes values (%om-print-value):
     ;; one line per value, *om-prompt* then the value with ~S, uncut
     (sb-int:encapsulate 'swank::format-values-for-echo-area 'om-listener
                         (lambda (function values max-lines width)
                           (declare (ignore function max-lines width))
                           (format nil "~{~A~S~^~%~}"
                                   (loop for value in values append (list *om-prompt* value)))))
     ;; the result of a SLIME evaluation (slime.el: slime-eval-with-transcript runs
     ;; slime-transcript-stop-hook, then shows the value through slime-message-function) goes
     ;; to *OM Listener*; every other SLIME message keeps its own function
     (om-emacs-eval '(progn
                      (defvar om-listener-eval-result nil)
                      (defvar om-listener-previous-message-function nil)
                      (defun om-listener-message (format &rest args)
                        (if (not om-listener-eval-result)
                            (apply (or om-listener-previous-message-function (function message)) format args)
                          (setq om-listener-eval-result nil)
                          (let ((text (apply (function format) format args)))
                            (with-current-buffer (get-buffer-create "*OM Listener*")
                              (goto-char (point-max))
                              (unless (string= text "") (insert text (string 10)))
                              (dolist (w (get-buffer-window-list (current-buffer) nil t))
                                (set-window-point w (point-max)))))))
                      (defun om-listener-mark-eval-result ()
                        (setq om-listener-eval-result t)
                        (run-at-time 0 nil (lambda () (setq om-listener-eval-result nil))))
                      (unless (eq slime-message-function (quote om-listener-message))
                        (setq om-listener-previous-message-function slime-message-function)
                        (setq slime-message-function (quote om-listener-message)))
                      (add-hook (quote slime-transcript-stop-hook) (quote om-listener-mark-eval-result))
                      (display-buffer (get-buffer-create "*OM Listener*"))
                      (display-buffer (slime-output-buffer))))
     (unless *gui-startup*
       (%bring-emacs-to-front))
     (pushnew '%close-emacs-buffers sb-ext:*exit-hooks*)
     nil)
    ((and *om-listener* (alld:window-alive-p *om-listener*))
     (om-select-window *om-listener*) *om-listener*)
    (t
      (let* ((bg     (or *text-bg-color* *om-white-color*))
             (window (om-make-window 'om-listener
                                     :window-title (or title "OM Listener")
                                     :position (when (and x y) (om-make-point x y))
                                     :size (om-make-point (or width 600) (or height 400))
                                     :topmost *listener-on-top*
                                     :bg-color bg)))
        (setf *om-listener* window)
        (setf (capi::interface-menu-bar-items window)
              (append (internal-window-class-menubar window)
                      (om-window-class-menubar window)))
        (om-add-menu-to-win window)
        (%install-listener-content window input *text-bg-color*)
        (om-show-window window)
        ;; before the prompt: the prompt must show the package it establishes
        (when initial-lambda
          (%run-initial-lambda initial-lambda (ip window)))
        (when input
          (%insert-prompt (ip window)))
        window))))

;; vanilla (om-LW listener.lisp: the :destroy-callback of make-om-listener) clears *om-listener*
(defmethod om-close-window :before ((self om-listener))
  (when (ip self) (%stop-listener-evaluator (ip self))))

(defmethod om-close-window :after ((self om-listener))
  (when (eq self *om-listener*) (setf *om-listener* nil)))

(defmethod alloy:deallocate :before ((self om-listener))
  (when (ip self) (%stop-listener-evaluator (ip self))))

(defmethod alloy:deallocate :after ((self om-listener))
  (when (eq self *om-listener*) (setf *om-listener* nil)))

(defmethod om-close-window :before ((self om-repl))
  (when (input-pane self) (%stop-listener-evaluator (input-pane self))))

(defmethod alloy:deallocate :before ((self om-repl))
  (when (input-pane self) (%stop-listener-evaluator (input-pane self))))

(defun om-listener-echo (str)
  "Flash STR in the OUTPUT pane's echo strip (cleared on the next command); in Emacs mode, in the echo area."
  (cond (*emacs-mode*
         (om-emacs-eval `(message "%s" ,str)))
        ((and *om-listener* (alld:window-alive-p *om-listener*))
         (alld:post-to-ui-thread (lambda () (%echo-show (op-echo *om-listener*) str))))))

(defun %listener-clear-echoes (listener)
  "Collapse the REPL echo strip on the next command, the way the LispWorks editor
clears a temporary echo message. The OUTPUT echo is a persistent status bar and is
not cleared here."
  (when listener
    (%echo-hide (ip-echo listener))))

(defmethod alloy:handle :before ((event alloy:key-down) (pane om-repl-pane))
  (when *om-listener* (%listener-clear-echoes *om-listener*)))

(defmethod alloy:handle :before ((event alloy:key-down) (pane om-eval-output-pane))
  (when *om-listener* (%listener-clear-echoes *om-listener*)))

(defmethod alloy:handle :before ((event alloy:pointer-down) (pane om-repl-pane))
  (when *om-listener* (%listener-clear-echoes *om-listener*)))

(defmethod alloy:handle :before ((event alloy:pointer-down) (pane om-eval-output-pane))
  (when *om-listener* (%listener-clear-echoes *om-listener*)))

(defun om-listener-abort ()
  (om-abort-eval-process)
  (om-listener-echo "Aborted"))

(defun set-bg-listener-color (color)
  (setf *text-bg-color* color)
  (when (and *om-listener* (alld:window-alive-p *om-listener*))
    (when (op *om-listener*) (om-set-bg-color (op *om-listener*) color))
    (when (ip *om-listener*) (om-set-bg-color (ip *om-listener*) color))))

