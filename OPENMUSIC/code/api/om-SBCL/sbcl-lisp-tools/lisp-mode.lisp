(in-package :oa)

;;; Lisp mode

(defun %line-start (text pos)
  (loop for i downfrom (min pos (length text)) above 0
        do (when (char= #\Newline (char text (1- i)))
             (return-from %line-start i))
        finally (return 0)))

(defun %line-end (text pos)
  (loop for i from pos below (length text)
        do (when (char= #\Newline (char text i))
             (return-from %line-end i))
        finally (return (length text))))

(defun %column-at (text pos) (- pos (%line-start text pos)))

(defun %find-enclosing-open (text pos)
  "Innermost open paren enclosing POS, ignoring parens inside strings, comments
and character literals. Second value: :STRING/:COMMENT when POS itself sits
inside one, else NIL."
  (let* ((len   (length text))
         (limit (min (max pos 0) len))
         (stack nil)
         (inside nil)
         (i 0))
    (flet ((skip (end kind)
             (when (> end limit) (setf inside kind))
             (setf i (max end (1+ i)))))
      (loop while (< i limit)
            do (let ((c (char text i)))
                 (cond ((char= c #\") (skip (%lisp-skip-string text i len) :string))
                       ((char= c #\;) (skip (%lisp-skip-line-comment text i len) :comment))
                       ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
                        (skip (%lisp-skip-block-comment text i len) :comment))
                       ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
                        (skip (%lisp-skip-char-literal text i len) nil))
                       ((char= c #\() (push i stack) (incf i))
                       ((char= c #\)) (pop stack) (incf i))
                       (t (incf i))))))
    (values (car stack) inside)))

(defun %symbol-name-after (text pos)
  "The symbol name starting at or after POS (blanks skipped), and, as a second
value, the index just past it."
  (let ((len (length text))
        (start pos))
    (loop while (and (< start len)
                     (or (char= (char text start) #\Space)
                         (char= (char text start) #\Tab)))
          do (incf start))
    (let ((end start))
      (loop while (and (< end len) (oa::%symbol-char-p (char text end)))
            do (incf end))
      (when (< start end) (values (subseq text start end) end)))))

(defun %lisp-skip-datum (text i len)
  "Index just past the datum starting at I; reader prefixes (' ` , @ #) bind to
the datum that follows them."
  (let ((j i))
    (loop while (and (< j len)
                     (or (find (char text j) "'`,@")
                         (and (char= (char text j) #\#)
                              (< (1+ j) len)
                              (not (find (char text (1+ j)) "\\|")))))
          do (incf j))
    (cond ((>= j len) len)
          ((char= (char text j) #\") (%lisp-skip-string text j len))
          ((and (char= (char text j) #\#) (char= (char text (min (1+ j) (1- len))) #\\))
           (%lisp-skip-char-literal text j len))
          ((char= (char text j) #\()
           (let ((span (%lisp-find-matching-paren text j)))
             (if span (1+ (cdr span)) len)))
          (t (loop for k from j below len
                   until (find (char text k) '(#\Space #\Tab #\Newline #\Return #\Page
                                               #\( #\) #\" #\;))
                   finally (return k))))))

(defun %count-args-between (text start limit)
  "Number of COMPLETE data between START and LIMIT — the arguments the head has
already received before the line being indented."
  (let ((len (length text))
        (n 0)
        (i (min (or start limit) limit)))
    (loop while (< i limit)
          do (let ((c (char text i)))
               (cond ((find c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
                     ((char= c #\;) (setf i (max (%lisp-skip-line-comment text i len) (1+ i))))
                     ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
                      (setf i (max (%lisp-skip-block-comment text i len) (1+ i))))
                     ((char= c #\)) (return))
                     (t (let ((end (%lisp-skip-datum text i len)))
                          (when (<= end limit) (incf n))
                          (setf i (max end (1+ i))))))))
    n))

(defparameter *lisp-body-indent* 2
  "Column a body form gets past its open paren.")

(defparameter *lisp-special-indent* 4
  "Column a distinguished argument gets.")

(defparameter *lisp-lambda-indent-heads*
  '("DEFUN" "DEFMACRO" "DEFMETHOD" "DEFTYPE" "DEFCLASS" "LAMBDA" "DEFSETF")
  "Heads whose distinguished arguments align under the first argument's column
instead of *LISP-SPECIAL-INDENT* (measured live).")

(defparameter *lisp-loop-hanging-indent* 3
  "Column LOOP gets when no clause shares its line: open+3, where every other
unregistered head gets open+1 (measured live — LOOP has a rule of its own).
With a clause on the line it aligns under it, like any call.")

(defparameter *lisp-indent-rules*
  '((defun . 2) (defmacro . 2) (defmethod . 2) (defgeneric . 2)
    (defmethod! . 2) (defmethod* . 2)
    (defclass . 2) (defstruct . 1) (deftype . 2) (defvar . 1)
    (defparameter . 1) (defconstant . 1) (defpackage . 1)
    (lambda . 1) (let . 1) (let* . 1) (flet . 1) (labels . 1)
    (macrolet . 1) (symbol-macrolet . 1)
    (when . 1) (unless . 1) (if . 2) (case . 1)
    (ecase . 1) (typecase . 1) (etypecase . 1)
    (dolist . 1) (dotimes . 1) (do . 2) (do* . 2) (do-symbols . 1)
    (block . 1) (return-from . 1) (catch . 1)
    (handler-case . 1) (handler-bind . 1) (ignore-errors . 0)
    (restart-case . 1) (restart-bind . 1) (with-simple-restart . 1)
    (define-condition . 2) (assert . 2)
    (unwind-protect . 1) (with-open-file . 1) (with-output-to-string . 1)
    (with-input-from-string . 1) (with-slots . 2) (with-accessors . 2)
    (print-unreadable-object . 1)
    (destructuring-bind . 2) (multiple-value-bind . 2)
    (multiple-value-call . 1) (multiple-value-prog1 . 1) (multiple-value-setq . 2)
    (progn . 0) (prog1 . 1) (prog2 . 2) (prog . 1) (progv . 2)
    (define-setf-expander . 2) (defsetf . 3)
    (eval-when . 1) (locally . 0))
  "Head -> number of DISTINGUISHED arguments. N was obtained by measurement
(live, varying 0..3 arguments on the head's line): N is the first count whose
column becomes the BODY indent. Only heads that actually occur in the OpenMusic
sources are listed -- coverage is by real use, not by catalogue. COND and THROW
are deliberately absent: there they align under the first argument, like any
other call, which is exactly the unregistered branch. LOOP is absent because it
has *LISP-LOOP-HANGING-INDENT*. DO* is a deliberate ADDITION: the reference
registers DO and not DO*, an omission not copied.")

(defvar *lisp-indent-table* (make-hash-table :test 'equal)
  "Head NAME (upcased) -> distinguished-argument count. Keyed by NAME, not by
symbol, so the buffer's package cannot hide a rule.")

(defun %populate-indent-rules ()
  (clrhash *lisp-indent-table*)
  (dolist (rule *lisp-indent-rules*)
    (setf (gethash (symbol-name (car rule)) *lisp-indent-table*) (cdr rule))))

(%populate-indent-rules)

(defun %first-argument-column (text head-end)
  "Column of the first argument when it sits on the head's own line, else NIL."
  (when head-end
    (let ((len (length text))
          (i head-end))
      (loop while (and (< i len) (find (char text i) '(#\Space #\Tab))) do (incf i))
      (when (and (< i len) (not (find (char text i) '(#\Newline #\Return))))
        (%column-at text i)))))

(defun %compute-lisp-indent (text pos)
  "Column the line starting at POS must be indented to (measured live): an
unregistered head aligns its arguments under the first one, a registered head
puts its first N arguments at *LISP-SPECIAL-INDENT* and everything after at
*LISP-BODY-INDENT*."
  (let ((open (%find-enclosing-open text pos)))
    (if (null open)
        0
        (multiple-value-bind (head head-end) (%symbol-name-after text (1+ open))
          (let ((open-col (%column-at text open))
                (rule     (and head (gethash (string-upcase head) *lisp-indent-table*))))
            (cond ((null rule)
                   (or (%first-argument-column text head-end)
                       (+ open-col (if (and head (string-equal head "LOOP"))
                                       *lisp-loop-hanging-indent*
                                       1))))
                  ((>= (%count-args-between text head-end pos) rule)
                   (+ open-col *lisp-body-indent*))
                  ((member head *lisp-lambda-indent-heads* :test #'string-equal)
                   (+ open-col 1 (length head) 1))
                  (t (+ open-col *lisp-special-indent*))))))))

(defun %indent-line (pane)
  "Indent the caret's line to its Lisp column. Returns T when the indentation
CHANGED, NIL when the line was already correct — the gate Tab uses to fall
through to symbol completion (LispWorks editor-lisp-3.htm:390-392)."
  (let* ((text (or (alloy:value pane) ""))
         (cur  (alloy:cursor pane))
         (pos  (alloy:pos cur))
         (lstart (%line-start text pos))
         (lend   (%line-end text pos))
         (current-indent (loop for i from lstart below lend
                               while (or (char= (char text i) #\Space)
                                         (char= (char text i) #\Tab))
                               count i))
         (target (%compute-lisp-indent text lstart))
         (content-start (+ lstart current-indent))
         (cursor-offset (max 0 (- pos content-start)))
         (changed (/= current-indent target)))
    (when changed
      (when (> content-start lstart)
        (alloy::delete-text lstart content-start pane))
      (alloy:move-to lstart cur)
      (alloy::insert-text (make-string target :initial-element #\Space) pane))
    (alloy:move-to (+ lstart target cursor-offset) cur)
    (alloy:mark-for-render pane)
    changed))

(defun %indent-region (pane start end)
  "Re-indent every line the region [START,END) touches and return the region's
new end offset. Blank lines and lines that begin inside a string are left
untouched (measured live). The whole region is rewritten in ONE value write, so
Undo takes it back in one step."
  (let* ((text (or (alloy:value pane) ""))
         (i    (%line-start text (min (max start 0) (length text))))
         (stop (min (max end 0) (length text))))
    (loop while (<= i (min stop (length text)))
          do (let* ((lend    (%line-end text i))
                    (content (loop for k from i below lend
                                   unless (find (char text k) '(#\Space #\Tab #\Return))
                                     return k)))
               (multiple-value-bind (open inside) (%find-enclosing-open text i)
                 (declare (ignore open))
                 (cond ((or (null content) (eq inside :string))
                        (setf i (1+ lend)))
                       (t
                        (let ((target (%compute-lisp-indent text i))
                              (cur    (- content i)))
                          (unless (= target cur)
                            (setf text (concatenate 'string
                                                    (subseq text 0 i)
                                                    (make-string target :initial-element #\Space)
                                                    (subseq text content)))
                            (incf stop (- target cur)))
                          (setf i (1+ (%line-end text i)))))))))
    (setf (alloy:value pane) text)
    (min (max stop 0) (length text))))

(defun %indent-selection (pane)
  "Indent the visible region and leave it selected, so Tab can be repeated."
  (let* ((text   (or (alloy:value pane) ""))
         (cur    (alloy:cursor pane))
         (anchor (alloy:anchor cur))
         (a      (min anchor (alloy:pos cur)))
         (b      (max anchor (alloy:pos cur)))
         ;; the first affected line's start never moves: everything before it is
         ;; copied verbatim by %indent-region
         (new-a  (%line-start text (min a (length text))))
         (new-b  (%indent-region pane a b)))
    (setf (alloy:anchor cur) new-a)
    (alloy:move-to (max new-a new-b) cur)
    (alloy:mark-for-render pane)
    (alloy:notice-size pane t)
    t))

(defun %insert-newline-and-indent (pane)
  (let* ((text (or (alloy:value pane) ""))
         (cur  (alloy:cursor pane))
         (pos  (alloy:pos cur))
         (target (%compute-lisp-indent text pos)))
    (alloy::insert-text (concatenate 'string (string #\Linefeed)
                                    (make-string target :initial-element #\Space))
                       pane)))

(defun %lisp-skip-string (text i len)
  (loop with j = (1+ i)
        while (< j len)
        do (let ((c (char text j)))
             (cond ((char= c #\\) (incf j 2))
                   ((char= c #\") (return-from %lisp-skip-string (1+ j)))
                   (t (incf j))))
        finally (return j)))

(defun %lisp-skip-line-comment (text i len)
  (loop for j from i below len
        do (when (char= (char text j) #\Newline)
             (return-from %lisp-skip-line-comment (1+ j)))
        finally (return len)))

(defun %lisp-skip-block-comment (text i len)
  (loop with j = (+ i 2)
        with depth = 1
        while (< j (1- len))
        do (cond ((and (char= (char text j) #\|) (char= (char text (1+ j)) #\#))
                  (decf depth) (incf j 2)
                  (when (zerop depth) (return-from %lisp-skip-block-comment j)))
                 ((and (char= (char text j) #\#) (char= (char text (1+ j)) #\|))
                  (incf depth) (incf j 2))
                 (t (incf j)))
        finally (return len)))

(defun %lisp-skip-char-literal (text i len)
  "Index just past #\\X. The character itself is consumed unconditionally --
CLHS 2.4.8.1: #\\( names a paren, it does not open a list."
  (let ((j (min len (+ i 3))))
    (loop while (and (< j len)
                     (let ((c (char text j)))
                       (or (alphanumericp c) (char= c #\-) (char= c #\_))))
          do (incf j))
    j))

(defparameter *paren-match-pattern* (chroma:rgb 0.55 1.0 0.15 1.0))
(defparameter *line-color-numbers* (chroma:rgb 0.772 0.855 0.788 1.0))
(defparameter *om-lisp-gutter-width* 40)
(defparameter *line-numbers-color* (chroma:rgb 0.0 0.0 0.0 1.0))

(defparameter *gutter-continuation-mark* "---"
  "What the gutter shows on a visual line that continues a wrapped real line, so the
line count stays the REAL one, matching CAPI/LispWorks line-count semantics.")

(defun %gutter-numbers-string (text breaks)
  (with-output-to-string (s)
    (let ((line 0))
      (loop for i from 0 below (length breaks)
            for start = (if (zerop i) 0 (aref breaks (1- i)))
            do (if (or (zerop i)
                       (and (plusp start)
                            (char= (char text (1- start)) #\Newline)))
                   (format s "~D~%" (incf line))
                   (format s "~A~%" *gutter-continuation-mark*)))
      (when (and (plusp (length text))
                 (char= (char text (1- (length text))) #\Newline))
        (format s "~D~%" (incf line)))
      (when (zerop line) (format s "1~%")))))

(defun %paren-px-bounds (label index)
  (handler-case
      (let* ((cell (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location label index))
             (next (org.shirakumo.alloy.renderers.opengl.msdf::compute-cursor-location label (1+ index)))
             (x (alloy:pxx cell))
             (y (alloy:pxy cell))
             (h (alloy:pxh cell))
             (advance (if (and next (= (alloy:pxy next) y) (> (alloy:pxx next) x))
                          (- (alloy:pxx next) x)
                          (* h 0.5))))
        (alloy:px-extent (- x 1) (- y 2) (+ advance 2) (+ h 4)))
    ;; the shape lags the buffer by a frame: with LINE-BREAKS still empty compute-cursor-
    ;; location arefs index -1, and INDEX past the text arefs past the end (alloy renderers/opengl/msdf.lisp: compute-cursor-location)
    (type-error (c)
      (format *error-output* "[paren-px-bounds] ~A~%" (princ-to-string c))
      nil)))

(defun %update-paren-shapes (pane)
  (let ((open-shape  (presentations:find-shape :paren-open  pane))
        (close-shape (presentations:find-shape :paren-close pane))
        (label       (presentations:find-shape :label       pane))
        (match       (paren-match pane)))
    (cond ((and match label open-shape close-shape)
           (let ((ob (%paren-px-bounds label (car match)))
                 (cb (%paren-px-bounds label (cdr match))))
             (cond ((and ob cb)
                    (setf (simple:bounds open-shape)  ob)
                    (setf (simple:bounds close-shape) cb)
                    (setf (presentations:hidden-p open-shape)  nil)
                    (setf (presentations:hidden-p close-shape) nil))
                   (t
                    (setf (presentations:hidden-p open-shape)  t)
                    (setf (presentations:hidden-p close-shape) t)))))
          ((and open-shape close-shape)
           (setf (presentations:hidden-p open-shape)  t)
           (setf (presentations:hidden-p close-shape) t)))))

(defun %paren-match-state (pane)
  (let ((text (alloy:value pane))
        (pos  (and (alloy:cursor pane) (alloy:pos (alloy:cursor pane)))))
    (setf (paren-match pane)
          (and text pos (%lisp-find-matching-paren text pos)))))

(defun %recompute-paren-match (pane)
  (%paren-match-state pane)
  (alloy:mark-for-render pane))

(defmethod presentations:update-shape :after
    ((renderer t) (renderable om-gutter-edit-view-widget) (all (eql t)))
  (let ((gutter (presentations:find-shape :gutter-text renderable nil))
        (label  (presentations:find-shape :label renderable nil)))
    (when (and gutter label)
      (let ((new (%gutter-numbers-string
                  (or (alloy:value renderable) "")
                  (org.shirakumo.alloy.renderers.opengl.msdf::line-breaks label))))
        (when (string/= new (or (alloy:text gutter) ""))
          (reinitialize-instance gutter :renderer renderer :text new))))))

(defmethod presentations:update-shape :after
    ((renderer t) (renderable om-paren-edit-view-widget) (all (eql t)))
  (%paren-match-state renderable)
  (%update-paren-shapes renderable))

(defun %current-operator-string (text pos)
  (let ((open (%find-enclosing-open text pos)))
    (when open (%symbol-name-after text (1+ open)))))

(defvar *arglist-timer* nil
  "Pending arglist timer, cancelled before a new one is scheduled: SBCL timers own
a thread each (om-delayed-funcall), so typing would pile threads up.")

(defun %schedule-editor-arglist (pane)
  "Show PANE's current function arglist in its attached echo area after 1 second."
  (let ((win (om-view-window pane)))
    (when (and win (typep win 'om-text-editor))
      (let* ((text (or (alloy:value pane) ""))
             (pos  (and (alloy:cursor pane) (alloy:pos (alloy:cursor pane))))
             (op   (and pos (%current-operator-string text pos)))
             (arglist
               (when op
                 (om-arglist-string
                  op :package (%buffer-package-to-use win pos)))))
        (when arglist
          (when *arglist-timer*
            (sb-ext:unschedule-timer *arglist-timer*))
          (setf *arglist-timer*
                (om-delayed-funcall
                 1000
                 (lambda ()
                   (alld:post-to-ui-thread
                    (lambda ()
                      (when (%editor-job-window-alive-p win)
                        (%editor-echo win (format nil "ARGS: ~A" arglist)))))))))))))

(defmethod alloy:handle :after ((event alloy:key-down) (pane om-lisp-edit-view-widget))
  (%recompute-paren-match pane))

(defmethod alloy:handle :after ((event alloy:text-event) (pane om-lisp-edit-view-widget))
  (%recompute-paren-match pane)
  (when (string= (alloy:text event) " ")
    (%schedule-editor-arglist pane)))

(defmethod alloy:handle :after ((event alloy:paste-event) (pane om-lisp-edit-view-widget))
  (%recompute-paren-match pane))

(defmethod alloy:handle :after ((event alloy:cut-event) (pane om-lisp-edit-view-widget))
  (%recompute-paren-match pane))

(defmethod alloy:handle :after ((event alloy:pointer-up) (pane om-lisp-edit-view-widget))
  (%recompute-paren-match pane))

(defmethod alloy:handle :around ((event alloy:key-down) (pane om-lisp-edit-view-widget))
  (when (%completion-route-key pane event)
    (return-from alloy:handle t))
  (multiple-value-prog1
      (let ((ctrl (find :control (alloy:modifiers event))))
        (case (alloy:key event)
          (:tab
           ;; "Indent Selection or Complete Symbol": a visible region is indented,
           ;; otherwise the current line; a line already correct falls through to
           ;; completion (LispWorks editor-lisp-3.htm:390-392).
           (let* ((cur    (alloy:cursor pane))
                  (anchor (alloy:anchor cur)))
             (cond ((and (not ctrl) anchor (/= anchor (alloy:pos cur)))
                    (%indent-selection pane))
                   ((%indent-line pane) t)
                   (ctrl t)
                   (t (let ((win (om-view-window pane)))
                        (%show-completion-popup
                         pane
                         :package (if (typep win 'om-text-editor)
                                      (%buffer-package-to-use win)
                                      oa::*om-default-package*)))
                      t))))
          ((:enter :return :kp-enter)
           (cond (ctrl (call-next-method))
                 (t (%insert-newline-and-indent pane)
                    (alloy:notice-size pane t)
                    t)))
          ((:k :|K|)
           (cond (ctrl (%kill-line pane) t)
                 (t (call-next-method))))
          ((:slash :|/|)
           (cond (ctrl (%toggle-comment-line pane) t)
                 (t (call-next-method))))
          (:semicolon
           (cond (ctrl (%toggle-comment-line pane) t)
                 (t (call-next-method))))
          (t (call-next-method))))
    (%completion-after-key pane event)))

(defun %kill-line (pane)
  (let* ((text (or (alloy:value pane) ""))
         (cur  (alloy:cursor pane))
         (pos  (alloy:pos cur))
         (lend (%line-end text pos)))
    (when (< pos lend)
      (alloy::delete-text pos lend pane)
      (alloy:mark-for-render pane))))

(defun %toggle-comment-line (pane)
  (let* ((text (or (alloy:value pane) ""))
         (cur  (alloy:cursor pane))
         (pos  (alloy:pos cur))
         (lstart (%line-start text pos))
         (lend   (%line-end text pos)))
    (cond ((and (< lstart lend)
                (>= (- lend lstart) 2)
                (char= (char text lstart) #\;)
                (char= (char text (1+ lstart)) #\;))
           (alloy::delete-text lstart (+ lstart 2) pane))
          (t
           (alloy:move-to lstart cur)
           (alloy::insert-text ";; " pane)))
    (alloy:mark-for-render pane)))

(defun %symbol-bounds-at-point (text pos &key (min-start 0))
  (when (and text (plusp (length text)))
    (let* ((len   (length text))
           (pos   (max min-start (min pos len)))
           (end   pos)
           (start pos))
      (loop while (and (< end len) (oa::%symbol-char-p (char text end)))
            do (incf end))
      (loop while (and (> start min-start) (oa::%symbol-char-p (char text (1- start))))
            do (decf start))
      (when (< start end) (values start end)))))

