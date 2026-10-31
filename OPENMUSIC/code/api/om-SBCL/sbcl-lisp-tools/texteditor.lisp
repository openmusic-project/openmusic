(in-package :oa)

;;; ============================================================
;;; TEXT EDITOR
;;; ============================================================

;;; Text editor windows

;;; Vanilla hierarchy: om-text-editor is the BASE (om-LW lw-lisp-tools/texteditor.lisp: defclass om-text-editor,
;;; slots editor-buffer / editor-file / lisp-editor?) and om-text-edit-window is
;;; its SUBCLASS (om-LW/om-text-editor.lisp: defclass om-text-edit-window, adding save-callback / echoarea).
;;; The direction is load-bearing: it is what makes the two close guards dispatch —
;;; the base prompts on a modified buffer with no file (om-LW texteditor.lisp: check-close-buffer for om-text-editor),
;;; the document window only prompts for an attached file
;;; (om-LW/om-text-editor.lisp: check-close-buffer for om-text-edit-window).
;;; EVAL-PACKAGE sits on the base because the standalone Lisp editor IS the base
;;; class in vanilla (om-LW texteditor.lisp: defvar *editor-class*, open-new-text-editor).
(defclass om-text-editor (om-look-window)
  ((edit-pane       :accessor edit-pane       :initform nil)
   (echo-area       :accessor echoarea        :initarg :echoarea      :initform nil)
   (save-callback   :accessor save-callback   :initarg :save-callback :initform nil)
   (modified-p      :accessor modified-p      :initform nil)
   (editor-buffer   :accessor editor-buffer   :initarg :editor-buffer :initform nil)
   (editor-file     :accessor editor-file     :initarg :editor-file   :initform nil)
   (lisp-editor?    :accessor lisp-editor?    :initarg :lisp-editor?  :initform nil)
   ;; fallback package when the buffer holds no (in-package ...): the package of
   ;; whoever opened the editor, as a CAPI editor inherits its process's binding
   (eval-package    :accessor eval-package    :initarg :eval-package  :initform *package*)))

;;; Public face for kernel callers (projects basicproject/classes/textfile.lisp: defclass TextEditorWindow):
;;; the document-window boundary, whose contents belong to the enclosing OM object.
(defclass om-text-edit-window (om-text-editor) ())

(defclass om-lisp-edit-window (om-text-edit-window) ()
  (:default-initargs :lisp-editor? t))

;;; Record of the STANDALONE editors, and the only place "is this file already
;;; open?" may look. Vanilla pushes here in open-new-text-editor
;;; (om-LW texteditor.lisp: open-new-text-editor) and searches it in find-open-file;
;;; document windows never enter it -- om-LW/om-text-editor.lisp: om-destroy-callback for om-text-edit-window only REMOVES.
(defvar *editor-files-open* nil)

(defmethod (setf editor-buffer) :before (buffer (self om-text-editor))
  (declare (ignore buffer))
  (when (editor-buffer self) (setf (window (editor-buffer self)) nil)))

(defmethod (setf editor-buffer) :after (buffer (self om-text-editor))
  (when (typep buffer 'ombuffer) (setf (window buffer) self)))

(defmethod (setf editor-file) :after (path (self om-text-editor))
  (om-set-window-title self (if path (namestring path) "New Buffer")))

;; every text-editing WINDOW wraps at the visible width, like the CAPI panes it
;; ports; the gutter keeps the REAL line numbers and marks the continuations.
;; om-text-edit-view stays unwrapped by default because the kernel's in-patch boxes
;; decide it themselves (kernel in-out-boxes.lisp: the :wrap-p of set-input-dialog and set-input-typed-dialog).
(defclass om-gutter-edit-view-widget (oa::om-text-edit-view)
  ((oa::wrap-p :initform t))
  (:documentation "Plain text pane with line-number gutter; no Lisp editing features."))

(defclass om-paren-edit-view-widget (oa::om-text-edit-view)
  ((paren-match :accessor paren-match :initform nil))
  (:documentation "Plain text pane with paren-match highlight; no other Lisp features."))

(defclass om-lisp-edit-view-widget (om-gutter-edit-view-widget om-paren-edit-view-widget)
  ())

;; the listener panes wrap at the visible width and have no horizontal scroller,
;; like the CAPI collector/listener panes they port; om-text-edit-view leaves
;; wrap-p NIL for the kernel's boxes, which pass :wrap-p themselves
(defclass om-eval-output-pane (oa::om-text-edit-view)
  ((oa::wrap-p :initform t)))

(defclass om-output-collector-pane (om-eval-output-pane) ()
  (:documentation
   "Read-only selectable collector used by OM-SHOW-OUTPUT-LINES."))

(defmethod alloy:handle ((event alloy:text-event) (pane om-output-collector-pane))
  (declare (ignore event pane))
  t)

(defmethod alloy:handle ((event alloy:paste-event) (pane om-output-collector-pane))
  (declare (ignore event pane))
  t)

(defmethod alloy:handle ((event alloy:cut-event) (pane om-output-collector-pane))
  (declare (ignore event pane))
  t)

(defmethod alloy:handle ((event alloy:key-down) (pane om-output-collector-pane))
  (if (or (member (alloy:key event) '(:backspace :delete :enter :return :kp-enter))
          (and (find :control (alloy:modifiers event))
               (member (alloy:key event) '(:x :|X| :v :|V|))))
      t
      (call-next-method)))

(defmethod om-get-menu-context ((pane om-output-collector-pane))
  (list (om-new-leafmenu
         "Copy" (lambda () (alloy:handle (make-instance 'alloy:copy-event) pane)))
        (om-new-leafmenu
         "Select All"
         (lambda ()
           (let ((cursor (alloy:cursor pane)))
             (setf (alloy:anchor cursor) 0)
             (alloy:move-to :end cursor)
             (alloy:mark-for-render pane))))))

;; 4 blank lines under the last output line, for reading comfort and scroller travel
(defmethod %trailing-blank-lines ((pane om-eval-output-pane)) 4)

(defvar *line-numbers* t)


;;; Direct point sizes, as in vanilla (om-LW lw-lisp-tools/texteditor.lisp: defvar
;;; *def-text-edit-font*): a point request, never an OM logical size.  The darwin
;;; branch carries the pixel size this editor renders at on win32 (editors/om-text-editor.lisp:
;;; %score-editor-init-defaults).
(defvar *def-text-edit-font*
  #+linux  (%om-make-font-pt "Liberation Mono" 11)
  #+darwin (%om-make-font-pt "Consolas" 44/3)
  #-(or linux darwin) (%om-make-font-pt "Consolas" 11))

(defstruct (echo-strip (:constructor %make-echo-strip (label box)))
  "A per-pane minibuffer (LW echo-area): collapsed (0px) until a message is shown;
cleared on the next command, as the LispWorks editor clears a temporary echo message."
  label box (shown nil))

(defparameter *echo-strip-height* 22)

(defparameter *echo-error-bg-color* (om-make-color 1.0 0.271 0.0)
  "Background of the editor error highlight (orangered), as in the LispWorks editor.")

;;; Background of every text pane, white on every platform; the kernel writes the
;;; user's Appearance preference here through set-bg-listener-color
;;; (vanilla om-LW/lw-lisp-tools/listener.lisp: the defvar *text-bg-color*).
(defvar *text-bg-color* nil)
(setf *text-bg-color* *om-white-color*)

(defun %install-text-edit-pane (window &key (initial-text ""))
  (let* ((lisp-mode (or (lisp-editor? window) (typep window 'om-lisp-edit-window)))
         (root      (om-window-root  window))
         (focus     (om-window-focus window))
         (data      (make-instance 'alloy:value-data :value (or initial-text "")))
         (pane      (make-instance (if lisp-mode 'om-lisp-edit-view-widget 'om-gutter-edit-view-widget)
                                   :data data))
         (scroll    (make-instance 'alloy:scroll-view
                                   :layout pane :focus pane
                                   :focus-parent focus :scroll :y))
         (outer     (make-instance 'alloy:border-layout)))
    (alloy:with-unit-parent root
      (alloy:enter outer root :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1)))
    (alloy:enter (alloy:layout-element scroll) outer :place :center)
    ;; vanilla gives every om-text-editor a message area and its pane an echo area
    ;; (om-LW texteditor.lisp: the :message-area of om-text-editor and the :echo-area of
    ;; open-new-text-editor + om-text-editor.lisp: slot echoarea of om-text-edit-window
    ;; and the :echo-area of make-window-layout); the kernel asks for one explicitly at
    ;; kernel lisppatcheditor.lisp: the :default-initargs of patch-lambda-exp-window
    (let* ((label (%make-echo-label focus))
           (strip (%make-echo-strip label outer)))
      ;; CAPI's :ECHO-AREA T is part of the editor pane from creation onward.
      ;; Keeping one empty line allocated makes status/arglist changes geometry-free.
      (setf (echo-strip-shown strip) t
            (echoarea window) strip)
      (alloy:enter label outer :place :south
                   :size (alloy:un *echo-strip-height*)))
    (om-set-font pane *def-text-edit-font*)
    ;; white on every platform (om-LW/lw-lisp-tools/texteditor.lisp: the
    ;; simple-pane-background of open-new-text-editor); *text-bg-color* holds the
    ;; native colour the kernel writes, so it is wrapped back into an omcolor here
    (om-set-bg-color pane (%chroma->omcolor (or *text-bg-color* *om-white-color*)))
    (%om-focus-pane window pane)
    (setf (edit-pane window) pane)
    (alloy:refit (om-window-chrome window))
    pane))

(defun %init-text-editor (window)
  "Install the editing pane and bind WINDOW to its ombuffer: file contents win over
a supplied buffer, a missing buffer is created (vanilla init-text-editor, om-LW
texteditor.lisp)."
  (let* ((file (editor-file window))
         (text (when (and file (probe-file file)) (%read-file-text file)))
         (buf  (editor-buffer window)))
    (unless (typep buf 'ombuffer)
      (setf buf (om-make-buffer))
      (setf (editor-buffer window) buf))
    (when text (%om-buffer-set-contents buf text))
    (setf (window buf) window)
    ;; the pane font is *def-text-edit-font*, never the window's :font — CAPI does
    ;; not inherit it (om-LW/om-text-editor.lisp: the :font of the editor-pane in make-window-layout,
    ;; texteditor.lisp: the :font of the editor-pane in open-new-text-editor)
    (%install-text-edit-pane window :initial-text (om-buffer-text buf))
    (om-buffer-mark-render buf)
    (edit-pane window)))

(defmethod make-window-layout ((window om-text-editor) &optional color)
  (declare (ignore color))
  (%init-text-editor window))

(defmethod %om-buffer-render-in-window ((buf ombuffer) (window om-text-editor))
  (let ((pane (edit-pane window)))
    (when pane
      (let ((text (om-buffer-text buf)))
        (unless (string= text (or *pane-text-override* (alloy:value pane) ""))
          ;; alloy components/text-input.lisp clamps the cursor in its (setf text)
          ;; :after; a direct value write leaves it past the end of a shorter text
          (setf (alloy:text pane) text)
          ;; vanilla keeps ONE point for buffer and pane (om-LW lw-lisp-tools/textbuffer.lisp: om-buffer-insert
          ;; inserts at editor:buffer-point): the caret follows the model's point
          (alloy:move-to (point buf) (alloy:cursor pane))))
      (alloy:mark-for-render pane))))

(defun %sync-pane-to-buffer (window)
  "Write the pane's text back into the window's ombuffer — the kernel reads that
buffer directly (projects textfile.lisp: fill-buffer-textfile for OMTextFilebox). Returns the buffer."
  (let ((buf  (editor-buffer window))
        (pane (edit-pane window)))
    (when (and (typep buf 'ombuffer) pane)
      (let ((text (or *pane-text-override* (alloy:value pane) "")))
        (unless (string= text (om-buffer-text buf))
          (%om-buffer-set-contents buf text))))
    buf))

(defun om-eval-output-append (text)
  (unless (%listener-output-target)
    (om-make-new-listener :input *listener-input*))
  (%listener-output-append text)
  text)

(defun %report-eval-result (values)
  "Print each value of an editor eval in the listener output, prefixed like the
vanilla PRINT (om-LW listener.lisp: the print redefined in init-listener)."
  (dolist (value values) (%om-print-value value)))

(defun %report-eval-error (expr condition)
  (let ((header (typecase expr
                  (string   expr)
                  (symbol   (symbol-name expr))
                  (pathname (namestring expr))
                  (t        (princ-to-string expr)))))
    (om-eval-output-append
     (format nil "~%~A~%" (string-trim '(#\Space #\Newline #\Tab) header)))
    (om-eval-output-append (format nil "ERROR: ~A~%" (princ-to-string condition)))))

(defun %selection-string (pane)
  "Selected text, plus its start offset as a second value."
  (let* ((cursor (alloy:cursor pane))
         (anchor (and cursor (alloy:anchor cursor))))
    (when (and anchor cursor)
      (let* ((p1 (min anchor (alloy:pos cursor)))
             (p2 (max anchor (alloy:pos cursor)))
             (text (alloy:value pane)))
        (when (and text (< p1 p2))
          (values (subseq text p1 p2) p1))))))

(defparameter *lisp-eval-mode* :buffer
  "Editor 'Eval All' mode: :buffer = the persistent editor eval server;
:process = OM EVAL PROCESS (vanilla texteditor.lisp: defparameter *lisp-eval-mode*, eval-lisp-buffer for om-text-editor).")

(defstruct (om-editor-eval-job
             (:constructor %make-editor-eval-job (window source package)))
  window
  source
  package)

(defstruct (om-editor-evaluator
             (:constructor %make-editor-evaluator ())
             (:include alld:serial-worker
              (name "Background execute 0")
              (lock (bordeaux-threads:make-lock "om-editor-evaluator"))
              (condition (bordeaux-threads:make-condition-variable
                          :name "om-editor-evaluator"))
              (initial-bindings *process-initial-bindings*)
              (armed-traps '(:divide-by-zero))
              (job-function '%evaluate-editor-job)))
  "Persistent eval server shared by Lisp Editor windows.")

(defvar *om-editor-evaluator* nil)
(defvar *om-editor-evaluator-lock*
  (bordeaux-threads:make-lock "om-editor-evaluator-creation"))

(defun %editor-job-window-alive-p (window)
  (and window (alld:window-alive-p window)))

(defun %publish-editor-eval-result (job condition)
  "Publish JOB completion on its editor interface without touching Alloy here."
  (let ((window (om-editor-eval-job-window job)))
    (alld:post-to-ui-thread
     (lambda ()
       (when (and (%editor-job-window-alive-p window) (null condition))
         (%editor-echo window "Finished evaluating"))))))

(defun %evaluate-editor-job (job)
  "Read and evaluate one editor JOB in its buffer package and default streams."
  (let ((*package* (%resolve-package (om-editor-eval-job-package job)))
        (condition nil))
    (block evaluate
      ;; reported where it is signalled, as the debugger hook of a delivered image does
      ;; (eval-support.lisp: om-error-handler), then the job ends
      (handler-bind ((error (lambda (caught)
                              (%om-report-error caught)
                              (report-ae-error caught)
                              (setf condition caught)
                              (return-from evaluate))))
        (with-input-from-string (input (om-editor-eval-job-source job))
          (%honoring-redefinition-warnings
            (loop for form = (read input nil input)
                  until (eq form input)
                  do (%report-eval-result
                      (multiple-value-list (eval form))))))))
    ;; IN-PACKAGE may change this job's dynamic *PACKAGE*, including for later
    ;; forms in the same job.  Vanilla starts every new editor command again in
    ;; the buffer package, so that transient value must never be persisted.
    (%publish-editor-eval-result job condition)))

(defun %ensure-editor-evaluator ()
  "Return the live shared Lisp Editor eval server, recreating it if necessary."
  (bordeaux-threads:with-lock-held (*om-editor-evaluator-lock*)
    (unless (and *om-editor-evaluator*
                 (om-editor-evaluator-thread *om-editor-evaluator*)
                 (bordeaux-threads:thread-alive-p
                  (om-editor-evaluator-thread *om-editor-evaluator*)))
      (let ((evaluator (%make-editor-evaluator)))
        (alld:with-serial-worker-lock (evaluator)
          (alld:serial-worker-start evaluator))
        (setf *om-editor-evaluator* evaluator)))
    *om-editor-evaluator*))

(defun %enqueue-editor-evaluation (window source package)
  "Queue SOURCE for WINDOW's shared editor evaluator and return immediately."
  (alld:serial-worker-enqueue (%ensure-editor-evaluator)
                              (%make-editor-eval-job window source package))
  t)

(defun %eval-string-on-process (forms-as-string &optional package)
  "Read each top-level form from FORMS-AS-STRING and enqueue it on the eval
worker, output bound to *om-stream* (vanilla eval-string-on-process)."
  (let ((pkg (or (and package (find-package package)) *package*)))
    (handler-bind ((error (lambda (condition)
                            (om-message-dialog
                             (format nil "error ~A" condition))
                            (abort))))
      (with-input-from-string (in forms-as-string)
        (loop for form = (let ((*package* pkg)) (read in nil in))
              until (eq form in)
              do (let ((f form))
                   ;; the worker evaluates in ITS package, like vanilla, which enqueues
                   ;; the bare form (om-LW texteditor.lisp: eval-string-on-process)
                   (%enqueue-on-eval-process
                    (lambda ()
                      (let ((*standard-output* *om-stream*)
                            (*error-output*    *om-stream*)
                            (*trace-output*    *om-stream*))
                        (eval f))))))))))

(defun %buffer-package-to-use (window &optional position)
  "Package WINDOW must evaluate in at POSITION (default: the caret): the one named
by the last (IN-PACKAGE X) form CLOSED before POSITION, outside strings, comments
and character literals; else WINDOW's EVAL-PACKAGE. Indentation and nesting are
irrelevant -- what the user wrote counts, and an unclosed form is incomplete text
that names nothing."
  (let* ((pane     (edit-pane window))
         (fallback (%resolve-package (eval-package window)))
         (text     (and pane (alloy:value pane))))
    (if (null text)
        fallback
        (let* ((len   (length text))
               (limit (min (max 0 (or position
                                      (and (alloy:cursor pane) (alloy:pos (alloy:cursor pane)))
                                      0))
                           len))
               (found nil)
               (i 0))
          (flet ((breakp (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page))))
            (loop while (< i limit)
                  do (let ((c (char text i)))
                       (cond
                         ((char= c #\") (setf i (%lisp-skip-string text i len)))
                         ((char= c #\;) (setf i (%lisp-skip-line-comment text i len)))
                         ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
                          (setf i (%lisp-skip-block-comment text i len)))
                         ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
                          (setf i (%lisp-skip-char-literal text i len)))
                         ((and (char= c #\()
                               (<= (+ i 11) len)
                               (string-equal "(in-package" text :start2 i :end2 (+ i 11))
                               (or (= (+ i 11) len) (breakp (char text (+ i 11)))))
                          (let ((span (%lisp-find-matching-paren text i)))
                            (when (and span (< (cdr span) limit))
                              (let ((a (or (position-if-not #'breakp text
                                                            :start (+ i 11) :end (cdr span))
                                           (cdr span))))
                                (when (< a (cdr span))
                                  (let* ((quoted (char= #\" (char text a)))
                                         (b (or (position-if (lambda (ch)
                                                               (or (breakp ch)
                                                                   (char= ch #\() (char= ch #\))))
                                                             text :start (1+ a) :end (cdr span))
                                                (cdr span)))
                                         (raw (string-trim "#:\"" (subseq text a b)))
                                         (pkg (and (plusp (length raw))
                                                   (find-package (if quoted raw (string-upcase raw))))))
                                    (when pkg (setf found pkg)))))))
                          (incf i))
                         (t (incf i))))))
          (or found fallback)))))

;;; Lisp-mode commands live on the base: vanilla defines them for om-text-editor
;;; and gates the menu items by lisp-operations-enabled = lisp-editor?
;;; (om-LW texteditor.lisp: the Lisp menu of internal-window-class-menubar,
;;; lisp-operations-enabled for om-text-editor).
(defmethod om-eval-all-buffer ((window om-text-editor))
  (let* ((pane (edit-pane window))
         (text (and pane (alloy:value pane))))
    (when (and text (plusp (length text)))
      (if (eq *lisp-eval-mode* :process)
          (%eval-string-on-process text (eval-package window))
          (%enqueue-editor-evaluation window text (eval-package window))))))

(defun %toplevel-form-string (pane)
  "Source of the top-level form enclosing/preceding the cursor, or NIL, plus its
start offset as a second value.
Beginning-of-defun = nearest column-0 open paren at/before point (Emacs/LW
evaluate-defun heuristic); the matching close paren ends the form."
  (let* ((text (alloy:value pane))
         (pos  (and text (alloy:pos (alloy:cursor pane)))))
    (when (and text pos (plusp (length text)))
      (let ((start (loop for j downfrom (min pos (1- (length text))) to 0
                         when (and (char= (char text j) #\()
                                   (= j (%line-start text j)))
                           do (return j))))
        (when start
          (let ((span (%lisp-find-matching-paren text start)))
            (when span
              (values (subseq text (car span)
                              (min (length text) (1+ (cdr span))))
                      (car span)))))))))

(defmethod om-eval-region-buffer ((window om-text-editor))
  (let ((pane (edit-pane window)))
    (when pane
      (multiple-value-bind (text start)
          (multiple-value-bind (sel sel-start) (%selection-string pane)
            (if sel (values sel sel-start) (%toplevel-form-string pane)))
        (when (and text (plusp (length text)))
          (%enqueue-editor-evaluation
           window text (%buffer-package-to-use window start)))))))

(defun %definition-symbol-p (s)
  (and s (symbolp s)
       (or (fboundp s) (macro-function s) (find-class s nil) (boundp s))))

(defun %best-symbol-for-name (name eval-package)
  "Resolve NAME to a DEFINED symbol for definition lookup: read it in EVAL-PACKAGE,
   and if that symbol has no definition, search the OM bridge packages and CL for a
   same-named defined symbol (mirrors swank's cross-package name resolution)."
  (let ((sym (let ((*package* (%resolve-package eval-package)))
               (handler-case (read-from-string name nil nil)
                 (reader-error () nil)))))
    (if (%definition-symbol-p sym)
        sym
        (or (loop for p in '(:om-api :om-lisp :om-edit :common-lisp)
                  for s = (find-symbol (string-upcase name) p)
                  when (%definition-symbol-p s) return s)
            sym))))

(defun %editor-echo (window str &optional error-p)
  "Show STR in WINDOW's own echo area, the way vanilla ECHO-STRING sends
EDITOR:MESSAGE to the editor pane (om-LW texteditor.lisp: echo-string). Returns T when
the window has one."
  (let ((strip (and (typep window 'om-text-editor) (echoarea window))))
    (when (echo-strip-p strip)
      (%echo-show strip str error-p)
      t)))

(defun echo-string (interface string)
  "Publish STRING in INTERFACE's editor echo area."
  (%editor-echo interface string))

(defun report-file-operation (interface operation file)
  "Report a successful vanilla editor file OPERATION."
  (when operation
    (echo-string interface (format nil "~A file ~A" operation file))))

(defun report-failed-file-operation (interface operation file)
  "Report the operation-specific failure selected by the local ABORT restart."
  (declare (ignore interface))
  (om-message-dialog (format nil "Failed to ~A file ~A" operation file)))

(defmacro with-safe-file-operation (interface path present past &body forms)
  "Run FORMS with the file operation's local ABORT restart, as in LispWorks."
  `(restart-case
       (prog1 (progn ,@forms)
         (report-file-operation ,interface ,past ,path))
     (abort ()
       (report-failed-file-operation ,interface ,present ,path))))

(defun %editor-echo-clear (window)
  "Clear WINDOW's attached echo area without changing editor geometry."
  (let ((strip (and (typep window 'om-text-editor) (echoarea window))))
    (when (echo-strip-p strip)
      (let ((label (echo-strip-label strip)))
        (setf (alloy:value label) "")
        (om-set-bg-color label *om-light-gray-color*)
        (om-set-fg-color label *om-black-color*)
        (alloy:mark-for-render label)))))

(defmethod alloy:handle :before ((event alloy:key-down) (pane oa::om-text-edit-view))
  (%editor-echo-clear (om-view-window pane)))

(defmethod alloy:handle :before ((event alloy:pointer-down) (pane oa::om-text-edit-view))
  (%editor-echo-clear (om-view-window pane)))

(defmethod om-find-definition-at-point ((window om-text-editor))
  (let* ((pane (edit-pane window))
         (text (and pane (alloy:value pane)))
         (pos  (and pane (alloy:pos (alloy:cursor pane))))
         (name (and text pos (om-symbol-at-point text pos))))
    (flet ((report (msg) (%editor-echo window msg t)))
      (let ((symbol (and name (%best-symbol-for-name name (%buffer-package-to-use window pos)))))
        (cond ((null name)
               (report "no symbol at point"))
              ((not (and symbol (symbolp symbol)))
               (report (format nil "~A: not a resolvable symbol" name)))
              (t
               (edit-definition symbol nil #'report)))))))

(defun %clear-selection (pane)
  (let ((cursor (alloy:cursor pane)))
    (when (alloy:anchor cursor)
      (setf (alloy:anchor cursor) nil)
      (alloy:mark-for-render pane))))

(defvar *om-text-editor-initial-xy* #(100 100))
(defvar *om-text-editor-count* 0)

(defun %next-text-editor-position ()
  "Cascading placement for successive editor windows (vanilla texteditor.lisp: the :best-x / :best-y of om-text-editor)."
  (let ((n (incf *om-text-editor-count*)))
    (om-make-point
     (+ (aref *om-text-editor-initial-xy* 0)
        (* 15 (mod n 10))
        (mod (* 200 (floor n 10)) 800))
     (+ (aref *om-text-editor-initial-xy* 1)
        (* 15 (mod n 10))))))

(defun om-make-text-edit-window (&key title (initial-text "") (size (om-make-point 600 400))
                                      (lisp-mode nil) font on-save (show t))
  ;; a standalone editor is the BASE class with :lisp-editor? as a flag — vanilla
  ;; *editor-class* = 'om-text-editor + :lisp-editor? t (om-LW texteditor.lisp: defvar *editor-class*, the make-instance of open-new-text-editor).
  ;; om-text-edit-window is reserved for document windows owned by an OM object.
  ;; SHOW NIL hands back an unmapped window so the caller can settle its scroll before
  ;; the first frame is presented; the caller then owns the om-show-window.
  (let* ((window (om-make-window 'om-text-editor
                                 :window-title (or title (if lisp-mode "Lisp Editor" "Text Editor"))
                                 :lisp-editor? lisp-mode
                                 :size  (alloy:px-size (om-point-h size) (om-point-v size))
                                 :position (%next-text-editor-position)
                                 :window-show show
                                 :bg-color *om-window-def-color*)))
    (setf (save-callback window) on-save)
    (push window *editor-files-open*)
    (setf (capi::interface-menu-bar-items window)
          (append (internal-window-class-menubar window)
                  (om-window-class-menubar window)))
    (om-add-menu-to-win window)
    (when font (om-set-font (edit-pane window) font))
    (when (and initial-text (plusp (length initial-text)))
      (om-set-text window initial-text))
    (when show (om-show-window window))
    (%refresh-editor-menubars window)
    window))

(defvar *emacs-pid* nil
  "The process id of the connected Emacs, once known in this session.")

(defun %bring-emacs-to-front ()
  "Bring Emacs to the front from a thread of its own (alloy-desktop process.lisp: bring-process-to-front)."
  (bordeaux-threads:make-thread
   (lambda ()
     (handler-case
         (progn
           (unless *emacs-pid*
             (let ((pid (om-emacs-eval '(emacs-pid) t)))
               (unless (integerp pid)
                 (format *error-output* "~&[emacs-mode] no Emacs pid from the connection, searching by name~%")
                 (setf pid (getf (alld:find-application-window "emacs") :id)))
               (setf *emacs-pid* pid)))
           (when *emacs-pid*
             (alld:bring-process-to-front *emacs-pid*)))
       (error (c) (format *error-output* "~&[emacs-mode] ~A~%" (princ-to-string c)))))
   :name "Bring Emacs to front"))

(defun om-open-new-text-editor (&optional path)
  "Open a lisp-mode editor: reuse/open PATH, else a fresh New Buffer (vanilla texteditor.lisp: open-new-text-editor).
In Emacs mode the new buffer is an empty Emacs one in lisp-mode."
  (cond (path
         (om-open-new-text-editor-at path))
        (*emacs-mode*
         (om-emacs-eval '(progn (switch-to-buffer (generate-new-buffer "*OM Lisp*"))
                                (lisp-mode)))
         (%bring-emacs-to-front))
        (t
         (om-make-text-edit-window :title "New Buffer" :lisp-mode t))))

(defun %read-file-text (path)
  "The text of PATH, decoded in the file's own encoding (system.lisp: om-file-external-format)."
  (with-open-file (stream path :direction :input :external-format (om-file-external-format path))
    (let ((out (make-string-output-stream)))
      ;; read-line's second value keeps a file without a final newline intact
      (loop (multiple-value-bind (line missing-newline-p) (read-line stream nil nil)
              (unless line (return))
              (write-string line out)
              (unless missing-newline-p (write-char #\Newline out))))
      (get-output-stream-string out))))

(defvar *written-files* nil
  "Files already saved this session; only the first save of each makes a backup.")

(defun %backup-file (path)
  "The first time PATH is saved this session, rename it to PATH~ (LispWorks src/editor/filecoms.lisp:
maybe-backup-file, make-backup-filename). If the rename fails the user chooses between saving
without a backup and aborting."
  (let ((name (namestring path)))
    (when (and (probe-file path)
               (not (member name *written-files* :test #'string-equal))
               (not (char= #\~ (char name (1- (length name))))))
      (let ((backup (concatenate 'string name "~")))
        (handler-case (rename-file path backup)
          (file-error (condition)
            (unless (om-y-or-n-dialog (format nil "Could not create backup file ~A:~%~A~%Save anyway?" backup condition))
              (abort)))))
      (push name *written-files*))))

(defun %write-file-text (path text)
  "Write TEXT to PATH in the file's own encoding, as the LispWorks editor saves a buffer in the format
it was read with (editor-commands-5.htm:231); a new file is UTF-8. A character that encoding cannot hold
is an error, never a replacement (editor-commands-5.htm:241), and it is signalled while encoding, before
PATH is opened, so the file on disk stays intact; the user is told which character, and the file
operation is aborted. The first save of a session keeps the original as PATH~, and the file is
opened :rename-and-delete, so a write that fails restores the previous contents (LispWorks
src/editor/files.lisp: in-write-file)."
  (let* ((ef (om-file-external-format path))
         (encoding (if (consp ef) (first ef) ef))
         (octets (handler-case (sb-ext:string-to-octets text :external-format encoding)
                   ;; sbcl src/code/octets.lisp: octets-encoding-error, which carries the offset
                   (sb-impl::octets-encoding-error (condition)
                     (let ((pos (sb-impl::octets-encoding-error-position condition)))
                       (om-message-dialog
                        (format nil "~A was not saved: the character ~A at offset ~D cannot be written in the file's encoding ~A."
                                (file-namestring path) (char-name (char text pos)) pos encoding)))
                     (abort)))))
    (%backup-file path)
    (with-open-file (stream path :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :rename-and-delete :if-does-not-exist :create)
      (write-sequence octets stream)))
  t)

(defun %load-text-into-window (window path &optional (text (%read-file-text path)))
  "OPEN/REVERT: show PATH's contents (TEXT when already read) and ATTACH the file, leaving the document
clean (vanilla open-new-file + display-text-file, om-LW texteditor.lisp: display-text-file and open-new-file,
whose fresh find-file-buffer is unmodified, and revert-text-file for om-text-editor)."
  (let ((pane (edit-pane window)))
    (when (and text pane)
      (setf (alloy:text pane) text)
      (let ((buf (editor-buffer window)))
        (when (typep buf 'ombuffer) (%om-buffer-set-contents buf text)))
      (setf (editor-file window) path)
      (setf (modified-p window) nil)
      (om-add-menu-to-win window)
      (alloy:mark-for-render pane)
      ;; the clip-view is sized from the painted text (dialog-items.lisp: alloy:suggest-size for om-text-edit-view)
      (%scroll-end-into-view pane)
      t)))

(defun %import-text-into-window (window path)
  "IMPORT: replace the CURRENT buffer's text with PATH's and leave the document
MODIFIED, never adopting PATH (vanilla import-text-from-file, om-LW
texteditor.lisp: import-text-from-file -- clear-buffer + insert-string into the editor's own
buffer, buffer-modified T, and no write to EDITOR-FILE)."
  (let ((text (%read-file-text path))
        (pane (edit-pane window)))
    (when (and text pane)
      (setf (alloy:text pane) text)
      (let ((buf (editor-buffer window)))
        (when (typep buf 'ombuffer) (%om-buffer-set-contents buf text)))
      (om-mark-modified window)
      (alloy:mark-for-render pane)
      (%scroll-end-into-view pane)
      t)))

(defun %pane-find-and-select (pane target &optional (from 0))
  "Search PANE's text for TARGET from offset FROM; on a hit, select the match
\(anchor at start, cursor at end) and mark for render. Returns match start or NIL.
Case-insensitive, like the LW editor default (Default Search Kind =
:string-insensitive, measured live)."
  (let* ((text (and pane (alloy:value pane)))
         (pos  (and text target (plusp (length target))
                    (search target text :test #'char-equal
                            :start2 (min (max from 0) (length text))))))
    (when pos
      (let ((cursor (alloy:cursor pane)))
        (setf (alloy:anchor cursor) pos)
        (alloy:move-to (+ pos (length target)) cursor))
      (alloy:mark-for-render pane)
      pos)))

(defun %definition-regex (name &optional specializers)
  "cl-ppcre pattern for the (defXXX NAME ...) line (a method whose head line mentions all
SPECIALIZERS, in order, when given). Case-insensitive; NAME is capture group 1. Scoped
to one line like vanilla's regexp forward search (om-LW find-definition.lisp:
show-definitions-dialog), so a specializer named in a body or docstring never matches."
  (format nil "(?i)\\(def\\S*\\s+(~A)~{[^\\n]*?~A~}"
          (cl-ppcre:quote-meta-chars name)
          (mapcar #'cl-ppcre:quote-meta-chars specializers)))

(defun %pane-regex-select (pane regex)
  "Select the first REGEX match in PANE (capture group 1 if present, else the whole
match) and return its start, or NIL."
  (let ((text (and pane regex (alloy:value pane))))
    (when text
      (multiple-value-bind (ms me rs re)
          ;; REGEX is typed by the user in the Find field, so a bad pattern is input
          (handler-case (cl-ppcre:scan regex text)
            (cl-ppcre:ppcre-syntax-error (c)
              (format *error-output* "[regex-select] ~A~%" (princ-to-string c)) nil))
        (when ms
          (let* ((grp    (and rs (plusp (length rs)) (aref rs 0)))
                 (start  (or grp ms))
                 (end    (or (and grp re (plusp (length re)) (aref re 0)) me))
                 (cursor (alloy:cursor pane)))
            (setf (alloy:anchor cursor) start)
            (alloy:move-to end cursor)
            (alloy:mark-for-render pane)
            start))))))

(defun %goto-line-offset (pane)
  "Scroll PANE's clip-view so the cursor (placed by open-at-definition) sits ~3 lines
below the viewport top. One-shot: clears once the laid-out height has settled."
  (when (goto-line pane)
    (let ((cv (alld:find-ancestor-clip-view pane)))
      (when cv
        (let ((innerh (%text-scroll-to-index pane cv (alloy:pos (alloy:cursor pane)) 3.0)))
          (when innerh
            (if (eql innerh (goto-last-innerh pane))
                (setf (goto-line pane) nil)
                (setf (goto-last-innerh pane) innerh))))))))

(defmethod alloy:render :after ((renderer simple:renderer) (pane om-text-edit-view))
  (%goto-line-offset pane))

(defparameter *editor-open-settle-passes* 8
  "Cap on the bake+refit+goto passes of a positioned open: two settle it, and the
cap only stops a pathological layout from looping.")

(defun %settle-goto-line (window pane)
  "Drive the positioned open to its final offset WITHOUT presenting a frame, and
return T once it settled. %goto-line-offset reads the baked shapes plus the layout,
both forced here (the idiom of dialog-items.lisp: %scroll-end-into-view)."
  (let ((cv (and pane (alld:find-ancestor-clip-view pane))))
    (when (and window pane cv)
      ;; the shapes carry alloy units (un/ph) and are baked here outside any
      ;; render or event pass, which is the only context that binds the unit
      ;; parent on its own (alloy units.lisp: the define-unit un)
      (alloy:with-unit-parent pane
        (loop repeat *editor-open-settle-passes*
              while (goto-line pane)
              do (presentations:update-shape window pane t)
                 (alloy:refit cv)
                 (%goto-line-offset pane))
        (%text-view-anchor-reset pane))
      (not (goto-line pane)))))

(defun %find-open-editor-for-file (path)
  "An already-open STANDALONE editor editing PATH, or NIL (vanilla find-open-file,
om-LW texteditor.lisp, matched on the window's EDITOR-FILE slot).
Scanning every om-text-editor instead would answer with a document window: a
TextFile box open on the same path would be raised by File > Open."
  (when path
    (let ((want (namestring path)))
      (loop for w in *editor-files-open*
            when (and (editor-file w)
                      (string-equal want (namestring (editor-file w))))
              return w))))

(defun %focus-existing-editor (window)
  (om-show-window window)
  (om-select-window window)
  (%glfw:focus window)
  window)

(defun lisp-file? (path)
 "The existing .lisp source for PATH: itself when .lisp, the sibling when .fasl; NIL otherwise."
  (let ((type (and path (pathname-type path))))
    (cond ((not (stringp type)) nil)
          ((string-equal type "lisp")
           (and (probe-file path) (pathname path)))
          ((string-equal type "fasl")
           (let ((src (make-pathname :type "lisp" :defaults path)))
             (and (probe-file src) src)))
          (t nil))))

(defun om-open-new-text-editor-at (file &optional anchor)
  "Open FILE in a lisp editor and scroll to ANCHOR (a name string, or a list
(name specializer-string*) for a specific method) via a regexp search. The scroll is
settled before the first frame reaches the screen, so no intermediate position shows.
In Emacs mode FILE opens in Emacs at the line of ANCHOR."
 (let ((lisp-file (lisp-file? file)))
  (when lisp-file
   (if *emacs-mode*
    ;; Emacs opens and saves the file itself; this read only finds the line, in the file's own
    ;; encoding (system.lisp: om-read-file)
    (let* ((text  (om-read-file lisp-file))
           (name  (and anchor (if (consp anchor) (car anchor) (string anchor))))
           (start (and name
                       (or (cl-ppcre:scan (%definition-regex name (and (consp anchor) (cdr anchor))) text)
                           (cl-ppcre:scan (format nil "(?i)~A" (cl-ppcre:quote-meta-chars name)) text)))))
      (om-emacs-eval `(progn (find-file ,(namestring lisp-file))
                             (goto-char (point-min))
                             (forward-line ,(if start (count #\Newline text :end start) 0))))
      lisp-file)
    (let* ((existing (%find-open-editor-for-file lisp-file))
           (window   (or existing
                         (let ((new (om-make-text-edit-window
                                     :title (file-namestring lisp-file) :lisp-mode t
                                     :show nil)))
                           (%load-text-into-window new lisp-file)
                           new))))
      (when anchor
        (let* ((name (if (consp anchor) (car anchor) (string anchor)))
               (specs (and (consp anchor) (cdr anchor)))
               (pane  (edit-pane window)))
          (when (or (%pane-regex-select pane (%definition-regex name specs))
                    (%pane-regex-select pane (format nil "(?i)~A" (cl-ppcre:quote-meta-chars name))))
            (setf (alloy:anchor (alloy:cursor pane)) nil)
            (setf (goto-last-innerh pane) nil
                  (goto-line pane) t)
            (%settle-goto-line window pane))))
      ;; draw the settled state first, then map (new window) or raise (existing one)
      (alloy:mark-for-render window)
      (alloy:render window window)
      (if existing
          (%focus-existing-editor window)
          (om-show-window window))
      window)))))

(defun om-open-new-text-file (&optional path)
  (let ((file (or path (om-choose-file-dialog :prompt "Open File"))))
    (when file (om-open-new-text-editor-at file))))

;;; Enable gates of the fixed menubar, split by class exactly as vanilla
;;; (om-LW texteditor.lisp: lisp-operations-enabled and save-operation-enabled for
;;; om-text-editor + om-text-editor.lisp: file-operations-enabled and
;;; save-operation-enabled for om-text-edit-window, lisp-operations-enabled for
;;; om-lisp-edit-window).

(defgeneric file-operations-enabled (window)
  (:documentation "Whether File > New.../Open... may run from WINDOW."))

(defmethod file-operations-enabled ((self t)) t)
(defmethod file-operations-enabled ((self om-text-editor)) t)

(defmethod file-operations-enabled ((self om-text-edit-window))
  "A document window edits ONE object, given at creation: opening another file
into it is meaningless (om-LW/om-text-editor.lisp: file-operations-enabled for om-text-edit-window)."
  nil)

(defgeneric lisp-operations-enabled (window)
  (:documentation "Whether the Lisp menu items may run from WINDOW."))

(defmethod lisp-operations-enabled ((self t)) t)
(defmethod lisp-operations-enabled ((self om-text-editor)) (and (lisp-editor? self) t))
(defmethod lisp-operations-enabled ((self om-lisp-edit-window)) t)

(defgeneric save-operation-enabled (window)
  (:documentation "Whether File > Save may run from WINDOW."))

(defmethod save-operation-enabled ((self om-text-editor))
  "Standalone editor: a modified buffer is saveable even with no file attached --
Save falls through to Save As (om-LW texteditor.lisp: save-operation-enabled and save-text-file for om-text-editor)."
  (and (modified-p self) t))

(defmethod save-operation-enabled ((self om-text-edit-window))
  "Document window: the kernel's save hook always enables Save; without one only
an ATTACHED disk file does (om-LW/om-text-editor.lisp: save-operation-enabled for om-text-edit-window). A TextFile box with
no file has neither, and its contents are persisted by the enclosing OM object."
  (and (or (save-callback self)
           (and (editor-file self) (modified-p self)))
       t))

(defun %save-current (window)
  ;; the kernel's save hook replaces the file write (vanilla om-text-editor.lisp: save-text-file for om-text-edit-window)
  (let ((cb   (save-callback window))
        (path (editor-file window))
        (pane (edit-pane window)))
    (cond (cb (%sync-pane-to-buffer window)
              (funcall cb window)
              t)
          ((and path pane)
           (with-safe-file-operation window path "save" "Saved"
             (when (%write-file-text path (or (alloy:value pane) ""))
               (setf (modified-p window) nil)
               (om-add-menu-to-win window)
               t)))
          (t (%save-as window)))))

(defun %save-as (window)
  (let ((path (om-choose-new-file-dialog :prompt "Save As"))
        (pane (edit-pane window)))
    (when (and path pane)
      (when (%write-file-text path (or (alloy:value pane) ""))
        (setf (editor-file window) path)
        (setf (modified-p window) nil)
        (om-add-menu-to-win window)
        t))))

;; "New..." is open-new-text-editor with no path (om-LW texteditor.lisp: the New
;; item of internal-window-class-menubar), and that window is :lisp-editor? T too
;; (the :lisp-editor? t of open-new-text-editor), so in the vanilla a fresh
;; buffer comes back in major mode "Lisp".
(defun %file-action-new () (om-make-text-edit-window :title "Untitled" :lisp-mode t))

(defun %file-action-open (window)
  (let ((path (om-choose-file-dialog :prompt "Open File")))
    (when path
      (let ((existing (%find-open-editor-for-file path)))
        (if existing
            (%focus-existing-editor existing)
            (let ((text (with-safe-file-operation window path "open" nil
                          (%read-file-text path))))
              (when text
                ;; EVERY window the editor's File menu opens is a Lisp editor,
                ;; whatever the extension: "Open..." routes to open-text-file ->
                ;; open-new-text-editor, which builds the window with
                ;; :lisp-editor? T (om-LW texteditor.lisp: the Open item of
                ;; internal-window-class-menubar, open-text-file, the :lisp-editor? t
                ;; of open-new-text-editor), and displaying it then puts the buffer in
                ;; Lisp mode (capi:interface-display :after for om-text-editor).
                ;; In the vanilla, a .txt opened this way comes back in Lisp
                ;; mode with its comments, keywords, strings and parens
                ;; actually coloured.
                ;; built hidden and shown once the text is laid out, as om-open-new-text-editor-at
                (let ((new (om-make-text-edit-window
                            :title (file-namestring path) :lisp-mode t :show nil)))
                  (%load-text-into-window new path text)
                  (alloy:mark-for-render new)
                  (alloy:render new new)
                  (om-show-window new)
                  new))))))))

(defun %file-action-import (window &optional path)
  ;; PATH optional and prompted only when absent, as vanilla
  ;; import-text-from-file (om-LW texteditor.lisp: import-text-from-file)
  (let ((file (or path (om-choose-file-dialog :prompt "Import From"))))
    (when file
      (with-safe-file-operation window file "open" nil
        (%import-text-into-window window file)))))

(defun %file-action-revert (window)
  (let ((path (editor-file window)))
    (if path
        (with-safe-file-operation window path "revert" "Reverted"
          (%load-text-into-window window path))
        (error "No file is attached to this editor"))))

(defun %edit-action-dispatch-event (window event-class)
  (let ((pane (edit-pane window)))
    (when pane (alloy:handle (make-instance event-class) pane))))

(defun %edit-action-paste (window)
  (let* ((pane (edit-pane window))
         (clip (and pane (alloy:clipboard window))))
    (when (and pane clip)
      (alloy:handle (make-instance 'alloy:paste-event :content clip) pane))))

(defun %edit-action-select-all (window)
  (let ((pane (edit-pane window)))
    (when pane
      (let ((cursor (alloy:cursor pane)))
        (setf (alloy:anchor cursor) 0)
        (alloy:move-to :end cursor)
        (alloy:mark-for-render pane)))))

(defun %edit-action-text-font (window)
  (let ((font (om-choose-font-dialog))
        (pane (edit-pane window)))
    (when (and font pane)
      (om-set-font pane font)
      (alloy:mark-for-render pane))))

(defparameter *lisp-file-extensions* '("lisp" "lsp" "cl" "asd" "fasl"))

(defun %lisp-file-p (path)
  (and path (member (pathname-type path) *lisp-file-extensions* :test #'string-equal)))

(defun %lisp-action-load-file (window)
  "Load the Lisp file attached to WINDOW; the Listener owns prompt-based Load File."
  (let ((path (editor-file window)))
    (cond ((null path)
           (echo-string window "This buffer is not attached to a file."))
          ((not (probe-file path))
           (error "File ~A not found." path))
          (t
           (with-safe-file-operation window path "load" "Loaded"
             (let ((*package* (%resolve-package (eval-package window))))
               ;; LispWorks lets the editor operation's ABORT restart receive a
               ;; LOAD error. SBCL installs a nearer LOAD-specific ABORT restart,
               ;; so first unwind that implementation detail through a private
               ;; restart, then invoke the surrounding editor restart.
               (restart-case
                   (handler-bind
                       ((error (lambda (condition)
                                 (declare (ignore condition))
                                 (invoke-restart '%abort-editor-file-load))))
                     (load path :verbose nil :print nil))
                 (%abort-editor-file-load ()
                   (invoke-restart 'abort)))))))))

(defun %window-action-listener ()
  (cond ((and *om-listener* (alld:window-alive-p *om-listener*))
         (om-show-window *om-listener*)
         (%glfw:focus *om-listener*)
         (let ((target (or (ip *om-listener*) (op *om-listener*))))
           (when target (%om-focus-pane *om-listener* target))))
        (t
         (om-make-new-listener :input *listener-input*))))

(defun %window-action-new-buffer (window)
  (let ((pane (edit-pane window)))
    (when pane
      (setf (alloy:value pane) "")
      (let ((cursor (alloy:cursor pane)))
        (setf (alloy:anchor cursor) nil)
        (alloy:move-to 0 cursor))
      (alloy:mark-for-render pane)))
  (setf (editor-file window) nil)
  (setf (modified-p window) nil))

(defmethod om-mark-modified ((window om-text-editor))
  (%sync-pane-to-buffer window)
  ;; vanilla fires the kernel hook on EVERY change (om-LW/om-text-editor.lisp: the :change-callback of make-window-layout, texteditor-change-callback)
  (om-texteditor-modified window)
  ;; rebuild only on the nil->t transition so Save's enable state refreshes
  (unless (modified-p window)
    (setf (modified-p window) t)
    (om-add-menu-to-win window)))

(defgeneric %disk-file-save-prompt-required-p (window)
  (:documentation "Whether closing WINDOW must ask 'Save before closing?'.
Vanilla splits this by CLASS, not by slot state — the two cases are otherwise
indistinguishable (a new standalone buffer and a fileless textfile box have the
same modified-p / editor-file / save-callback)."))

(defmethod %disk-file-save-prompt-required-p ((window om-text-editor))
  "Standalone editor: a modified buffer prompts even with NO file attached.
Vanilla guard (om-LW texteditor.lisp: the guard of check-close-buffer for om-text-editor) is (and buffer-modified-p
save-operation-enabled), and the base save-operation-enabled IS buffer-modified-p
\(texteditor.lisp: save-operation-enabled for om-text-editor); answering YES without a file opens 'Save file as:'
\(texteditor.lisp: the Save file as branch of check-close-buffer) — here %save-current falls through to %save-as."
  (and (modified-p window) t))

(defmethod %disk-file-save-prompt-required-p ((window om-text-edit-window))
  "Document window (TextFile box, OMLispPatch): only an ATTACHED disk file prompts.
Without one the contents are persisted by the enclosing OM object/workspace.
Vanilla guard (om-LW/om-text-editor.lisp: check-close-buffer for om-text-edit-window)."
  (and (modified-p window) (editor-file window) t))

(defun %check-buffer-before-close (window)
  (cond ((not (%disk-file-save-prompt-required-p window)) t)
        (t
         (let* ((path (editor-file window))
                (name (if path (pathname-name path) "this text buffer"))
                (answer (om-y-n-cancel-dialog
                         (format nil "Changes on ~A were not saved.~%Save before closing?" name))))
           (case answer
             ((t)      (and (%save-current window) t))
             ((nil)    t)
             (:cancel  nil)
             (t        nil))))))

(defun %close-text-edit-window (window)
  (when (%check-buffer-before-close window) (om-close-window window)))

(defun check-buffers-before-close ()
  "Sweep every open text editor, focusing and prompting to save each saveable
one; NIL if the user cancelled (abort the quit). Vanilla (om-LW texteditor.lisp: check-buffers-before-close)
collects the BASE class and filters on SAVE-OPERATION-ENABLED, not on the
modified flag: a document window with nothing to save must not even be raised."
  (loop for ed in (om-get-all-windows 'om-text-editor)
        do (when (save-operation-enabled ed)
             (%focus-existing-editor ed)
             (unless (%check-buffer-before-close ed)
               (return-from check-buffers-before-close nil)))
        finally (return t)))

(defmethod alloy:handle :around ((event org.shirakumo.alloy.windowing:close)
                                  (window om-text-editor))
  ;; the save prompt runs a modal loop, and every reaper collects on
  ;; should-close-p, so the flag must stay down while the prompt is up; the
  ;; window :around re-arms it once its own check confirms
  ;; (alloy-desktop/window/close.lisp: alloy:handle :around for close and window).
  (when (%glfw:pointer window)
    (setf (org.shirakumo.fraf.glfw:should-close-p window) nil))
  (when (%check-buffer-before-close window)
    (call-next-method)
    ;; deferred: by the next UI tick the window left the desktop registry
    (alld:post-to-ui-thread (lambda () (%refresh-editor-menubars window)))))

(defun %file-menu (window)
  (om-make-menu
   "File"
   (list (list (om-new-leafmenu "New..."          (lambda () (%file-action-new))                #\n
                                (lambda () (file-operations-enabled window)))
               (om-new-leafmenu "Open..."         (lambda () (%file-action-open window))        #\o
                                (lambda () (file-operations-enabled window))))
         (list (om-new-leafmenu "Import From..."  (lambda () (%file-action-import window)))
               (om-new-leafmenu "Revert to Saved" (lambda () (%file-action-revert window))
                                nil (lambda () (and (editor-file window) t))))
         (list (om-new-leafmenu "Save"            (lambda () (%save-current window))
                                #\s (lambda () (save-operation-enabled window)))
               (om-new-leafmenu "Save As..."      (lambda () (%save-as window))))
         (list (om-new-leafmenu "Close"           (lambda () (%close-text-edit-window window)) #\w)))))

(defun %edit-menu (window)
  (om-make-menu
   "Edit"
   ;; Redo is a fork extension (absent from om-LW texteditor.lisp: the Edit menu of internal-window-class-menubar); Ctrl+Shift+Y keeps Ctrl+Y for Eval All.
   (list (list (om-new-leafmenu "Undo" (lambda () (oa::om-text-undo (edit-pane window))) #\z)
               (om-new-leafmenu "Redo" (lambda () (oa::om-text-redo (edit-pane window))) #\Y))
         (list (om-new-leafmenu "Cut"   (lambda () (%edit-action-dispatch-event window 'alloy:cut-event))  #\x)
               (om-new-leafmenu "Copy"  (lambda () (%edit-action-dispatch-event window 'alloy:copy-event)) #\c)
               (om-new-leafmenu "Paste" (lambda () (%edit-action-paste window))                            #\v))
         (list (om-new-leafmenu "Select All"  (lambda () (%edit-action-select-all window)) #\a))
         (list (om-new-leafmenu "Text Font" (lambda () (%edit-action-text-font window))))
         (list (om-new-leafmenu "Find..."    (lambda () (find-in-file window))    #\f)
               (om-new-leafmenu "Replace..." (lambda () (replace-in-file window)) #\r))
         (list (om-new-leafmenu "Search..." (constantly nil) nil nil)))))

(defun %lisp-menu (window)
  (om-make-menu
   "Lisp"
   (flet ((with-clear (thunk)
            (lambda ()
              (funcall thunk)
              (let ((pane (edit-pane window)))
                (when pane (%clear-selection pane))))))
     (flet ((lisp-gate () (lambda () (lisp-operations-enabled window))))
       (list (list (om-new-leafmenu "Eval All"
                                    (with-clear (lambda () (om-eval-all-buffer window))) #\y
                                    (lisp-gate))
                   (om-new-leafmenu "Eval Region"
                                    (with-clear (lambda () (om-eval-region-buffer window))) #\e
                                    (lisp-gate)))
             (list (om-new-leafmenu "Find Definition"
                                    (with-clear (lambda () (om-find-definition-at-point window))) #\.
                                    (lisp-gate)))
             (list (om-new-leafmenu "Load File..."
                                    (lambda () (%lisp-action-load-file window)) nil
                                    (lambda () (save-operation-enabled window)))))))))

(defun %open-editor-windows-menu-items (window)
  "One leafmenu per open text editor; the current WINDOW's item is disabled
(mirror of the kernel's dynamic Windows submenu, kernel graphics/ommenu.lisp: the Windows submenu of make-om-menu)."
  (mapcar (lambda (w)
            (om-new-leafmenu (om-window-title w)
                             (lambda () (when (om-window-open-p w)
                                          (%focus-existing-editor w)))
                             nil
                             (lambda () (not (eq w window)))))
          (om-get-all-windows 'om-text-editor)))

(defun %refresh-editor-menubars (&optional except)
  "Rebuild the menubar of every open text editor (except EXCEPT) so dynamic
menu content (open-windows list, enable states) stays current."
  (dolist (w (om-get-all-windows 'om-text-editor))
    (unless (eq w except)
      (om-add-menu-to-win w))))

(defun %windows-menu (window)
  (om-make-menu
   "Windows"
   (list (list (om-new-leafmenu "Listener"   (lambda () (%window-action-listener)) '(:shift #\L)))
         (list (om-new-leafmenu "New Buffer" (lambda () (%window-action-new-buffer window))))
         #'%open-editor-windows-menu-items)))

;;; ONE FIXED menubar on the base, as in vanilla (om-LW texteditor.lisp: internal-window-class-menubar for om-text-editor):
;;; the three menus are always built, whatever the window class. What varies is
;;; the enable state of the ITEMS, through the gates above -- hiding a menu is not
;;; the same as disabling its items.
(defmethod oa::internal-window-class-menubar ((self om-text-editor))
  (list (%file-menu self) (%edit-menu self) (%lisp-menu self)))

(defmethod oa::om-window-class-menubar ((self om-text-editor))
  (list (%windows-menu self)))

;;; The ombuffer is the model the kernel reads/writes; the pane is its view
;;; (vanilla om-text-editor.lisp: om-set-text-buffer and om-kill-window-buffer for
;;; om-text-edit-window, where the two are a single buffer
;;; object rather than a model/view pair).

(defmethod om-set-text ((self om-text-editor) (text string))
  (let ((buf (editor-buffer self)))
    (cond ((typep buf 'ombuffer)
           (om-buffer-delete buf)
           (om-buffer-insert buf text))
          ((edit-pane self) (setf (alloy:text (edit-pane self)) text))))
  text)

(defmethod om-get-text ((self om-text-editor))
  (let ((buf (%sync-pane-to-buffer self)))
    (cond ((typep buf 'ombuffer) (om-buffer-text buf))
          ((edit-pane self)      (or (alloy:value (edit-pane self)) ""))
          (t                     ""))))

(defmethod om-set-text-buffer ((self om-text-editor) newbuffer &optional kill)
  (cond ((typep newbuffer 'ombuffer)
         (let ((previous (editor-buffer self)))
           (setf (editor-buffer self) newbuffer)
           (om-buffer-mark-render newbuffer)
           (when (and kill (typep previous 'ombuffer) (not (eq previous newbuffer)))
             (om-kill-buffer previous))))
        (t (om-set-text self (if newbuffer (princ-to-string newbuffer) ""))))
  newbuffer)

(defmethod om-kill-window-buffer ((self om-text-editor))
  (let ((buf (editor-buffer self)))
    (if (typep buf 'ombuffer)
        (om-kill-buffer buf)
        (when (edit-pane self) (setf (alloy:text (edit-pane self)) ""))))
  (setf (modified-p self) nil))

(defmethod om-texteditor-modified ((self om-text-editor)) nil)

(defmethod om-get-lisp-expression ((self om-lisp-edit-window))
  (let ((text (om-get-text self)))
    (when (and text (plusp (length text)))
      (let ((*read-eval* nil)
            ;; the kernel reads the Lisp Patch through here (kernel lisppatcheditor.lisp: compile-before-close and compile-without-close for patch-lambda-exp-window):
            ;; it must resolve in the editor's package, not in the ambient one
            (*package* (%buffer-package-to-use self 0)))
        ;; TEXT is what the user typed: an incomplete form is end-of-file, everything
        ;; else the reader rejects (bad macro char, missing package) is reader-error
        (handler-case (read-from-string text nil nil)
          ((or end-of-file reader-error) (c)
            (format *error-output* "[om-get-lisp-expression] ~A~%" (princ-to-string c))
            nil))))))

(defmethod om-get-editor-panel ((self om-text-editor))
  (edit-pane self))

(defun %unbind-editor-buffer (window)
  "Drop the buffer's backpointer to WINDOW and take WINDOW out of the standalone
registry — the part both funerals share (om-LW texteditor.lisp: om-destroy-callback for om-text-editor and
om-text-editor.lisp: om-destroy-callback for om-text-edit-window)."
  (let ((buf (editor-buffer window)))
    (when (typep buf 'ombuffer) (setf (window buf) nil)))
  (setf *editor-files-open* (remove window *editor-files-open*)))

(defmethod om-destroy-callback ((self om-text-editor))
  "Standalone editor: the buffer is the window's own, so closing KILLS it
(om-LW texteditor.lisp: om-destroy-callback for om-text-editor)."
  (let ((buf (editor-buffer self)))
    ;; unbind before killing: om-kill-buffer re-renders into the pane through the
    ;; backpointer, and this runs while that pane is being torn down
    (%unbind-editor-buffer self)
    (when (typep buf 'ombuffer) (om-kill-buffer buf)))
  (call-next-method))

(defmethod om-destroy-callback ((self om-text-edit-window))
  "Document window: DOES NOT KILL THE BUFFER (om-LW/om-text-editor.lisp: om-destroy-callback for om-text-edit-window) —
the contents belong to the enclosing OM object, which reads them after the close."
  (%unbind-editor-buffer self)
  ;; NOT call-next-method: the base method above would kill the buffer. Vanilla
  ;; likewise bypasses it and fires the close event directly (om-LW/om-text-editor.lisp: om-destroy-callback for om-text-edit-window).
  (%fire-window-destroy-event self))

