(in-package :alloy-editor)

;;; ------------------------------------------------------------------ the faces

(defun %ink (r g b)
  (org.shirakumo.alloy.chroma:rgb (/ r 255.0) (/ g 255.0) (/ b 255.0) 1.0))

(defparameter *paren-faces*
  #(:paren-1 :paren-2 :paren-3 :paren-4 :paren-5 :paren-6)
  "The steps of the colour cycle a parenthesis is drawn in, in the order they are taken.")

(defparameter *face-inks*
  (list (cons :comment       (%ink 178  34  34))
        (cons :string        (%ink 188 143 143))
        (cons :keyword       (%ink 218 112 214))
        (cons :lambda-key    (%ink  34 139  34))
        (cons :special       (%ink 160  32 240))
        (cons :function-name (%ink   0   0 255))
        (cons :variable-name (%ink 184 134  11))
        (cons :type-name     (%ink  34 139  34))
        (cons :paren-1       (%ink 255   0   0))
        (cons :paren-2       (%ink   0   0   0))
        (cons :paren-3       (%ink   0 100   0))
        (cons :paren-4       (%ink 205 102   0))
        (cons :paren-5       (%ink   0   0 255))
        (cons :paren-6       (%ink 160  32 240)))
  "Face to the colour it is drawn in; the host puts its own colours here.")

(defparameter *paren-match-ground* (%ink 0 255 0)
  "Ground the two parentheses of the pair around the caret stand on.")

(defun face-ink (face)
  "The colour FACE is drawn in."
  (cdr (assoc face *face-inks*)))

(defparameter *definers*
  '(("defun" . :function-name) ("defmacro" . :function-name)
    ("defmethod" . :function-name) ("defgeneric" . :function-name)
    ("define-compiler-macro" . :function-name) ("defsetf" . :function-name)
    ("defvar" . :variable-name) ("defparameter" . :variable-name)
    ("defconstant" . :variable-name)
    ("defclass" . :type-name) ("defstruct" . :type-name) ("deftype" . :type-name)
    ("define-condition" . :type-name) ("defpackage" . :type-name))
  "Operator that NAMES what it defines, and the face that name is drawn in.")

(defparameter *special-forms*
  '("and" "block" "case" "catch" "ccase" "cond" "ctypecase" "declaim" "declare"
    "destructuring-bind" "do" "do*" "dolist" "dotimes" "ecase" "etypecase" "eval-when"
    "flet" "function" "go" "handler-bind" "handler-case" "if" "in-package" "labels"
    "let" "let*" "load-time-value" "locally" "loop" "macrolet" "multiple-value-bind"
    "multiple-value-call" "multiple-value-prog1" "multiple-value-setq" "or" "prog1"
    "prog2" "progn" "progv" "psetq" "quote" "restart-bind" "restart-case" "return"
    "return-from" "setf" "setq" "symbol-macrolet" "tagbody" "the" "throw" "typecase"
    "unless" "unwind-protect" "when" "with-accessors" "with-open-file"
    "with-output-to-string" "with-slots"
    "defun" "defmacro" "defmethod" "defgeneric" "defvar" "defparameter" "defconstant"
    "defclass" "defstruct" "deftype" "define-condition" "defpackage"
    "define-compiler-macro" "defsetf")
  "Operators drawn in the face of a special form; LAMBDA is not one of them.")

;;; ponytail: the two tables are walked, which is a few dozen length comparisons per symbol.
(defun %named-p (text start end name)
  (string-equal text name :start1 start :end1 end))

(defun %special-form-p (text start end)
  (and (member-if (lambda (name) (%named-p text start end name)) *special-forms*) t))

(defun %definer-face (text start end)
  (cdr (assoc-if (lambda (name) (%named-p text start end name)) *definers*)))

;;; ------------------------------------------------------------------ the state

(defstruct (lex-state (:constructor %make-lex-state (kind depth count stack))
                      (:conc-name lex-state-)
                      (:copier nil)
                      (:predicate nil))
  "What one line leaves the next."
  kind depth count stack)

(defun %fresh-lex-state () (%make-lex-state :code 0 0 '()))

(defun %same-lex-state-p (a b)
  (and (eq (lex-state-kind a) (lex-state-kind b))
       (eql (lex-state-depth a) (lex-state-depth b))
       (eql (lex-state-count a) (lex-state-count b))
       (equal (lex-state-stack a) (lex-state-stack b))))

;;; ------------------------------------------------------------------ the tokens

(defparameter *delimiters*
  '(#\Space #\Tab #\Return #\( #\) #\" #\' #\` #\, #\;)
  "What ends a symbol.")

(defun %string-close (text start)
  "Index past the quotation mark that closes a string opened before START, or NIL when the line ends inside it."
  (let ((n (length text))
        (i start))
    (loop while (< i n)
          do (let ((character (char text i)))
               (cond ((char= character #\\) (incf i 2))
                     ((char= character #\") (return-from %string-close (1+ i)))
                     (t (incf i)))))
    nil))

(defun %block-close (text start depth)
  "(values END LEFT): the index past the delimiter that takes the nesting of a block comment back to nothing, or NIL and the nesting the line ends at."
  (let ((n (length text))
        (i start))
    (loop while (< i n)
          do (let ((pair (and (< (1+ i) n) (char text (1+ i)))))
               (cond ((and pair (char= (char text i) #\|) (char= pair #\#))
                      (decf depth)
                      (incf i 2)
                      (when (<= depth 0) (return-from %block-close (values i 0))))
                     ((and pair (char= (char text i) #\#) (char= pair #\|))
                      (incf depth)
                      (incf i 2))
                     (t (incf i)))))
    (values nil depth)))

(defun %character-close (text start)
  "Index past the character a #\\ names, whose first character is always its own whatever it is."
  (let ((n (length text)))
    (if (<= n start)
        n
        (let ((i (1+ start)))
          (when (alphanumericp (char text start))
            (loop while (and (< i n) (not (member (char text i) *delimiters*)))
                  do (incf i)))
          i))))

(defun %word-close (text start)
  "Index past the symbol opening at START."
  (let ((n (length text))
        (i start))
    (loop while (and (< i n) (not (member (char text i) *delimiters*)))
          do (incf i))
    i))

(defun %map-line-tokens (text in function)
  "Call FUNCTION with (KIND START END STEP) for every token of the single line TEXT entered in the state IN."
  (let* ((n (length text))
         (kind (lex-state-kind in))
         (depth (lex-state-depth in))
         (count (lex-state-count in))
         (stack (lex-state-stack in))
         (i 0))
    (macrolet ((token (what start end &optional step)
                 `(funcall function ,what ,start ,end ,step)))
      (case kind
        (:string
         (let ((end (%string-close text 0)))
           (token :string 0 (or end n))
           (if end (setf i end kind :code) (setf i n))))
        (:block
         (multiple-value-bind (end left) (%block-close text 0 depth)
           (token :comment 0 (or end n))
           (if end
               (setf i end kind :code depth 0)
               (setf i n depth left)))))
      (loop while (< i n)
            do (let ((character (char text i))
                     (next (when (< (1+ i) n) (char text (1+ i)))))
                 (cond
                   ((char= character #\;)
                    (token :comment i n)
                    (setf i n))
                   ((char= character #\")
                    (let ((end (%string-close text (1+ i))))
                      (token :string i (or end n))
                      (if end (setf i end) (setf i n kind :string))))
                   ((and (char= character #\#) (eql next #\|))
                    (multiple-value-bind (end left) (%block-close text (+ i 2) 1)
                      (token :comment i (or end n))
                      (if end
                          (setf i end)
                          (setf i n kind :block depth left))))
                   ((and (char= character #\#) (eql next #\\))
                    (let ((end (%character-close text (+ i 2))))
                      (token :char i end)
                      (setf i end)))
                   ((and (char= character #\#) (member next '(#\+ #\-)))
                    (let ((end (%word-close text (+ i 2))))
                      (token :comment i end)
                      (setf i end)))
                   ((char= character #\()
                    ;; the cycle starts over at a list opened in the first column,
                    ;; which is where a form of its own starts
                    (when (zerop i) (setf count 0))
                    (let ((step (mod count 6)))
                      (token :open i (1+ i) step)
                      (push step stack)
                      (setf count (1+ count)))
                    (incf i))
                   ((char= character #\))
                    (token :close i (1+ i) (if stack (pop stack) 0))
                    (incf i))
                   ((member character *delimiters*) (incf i))
                   (t
                    (let ((end (%word-close text i)))
                      (token :atom i end)
                      (setf i (max end (1+ i)))))))))
    (%make-lex-state kind depth count stack)))

(defun %atom-face (text start end operator pending)
  "(values FACE PENDING) of the symbol [START, END) of TEXT."
  (let ((first (char text start)))
    (cond ((char= first #\:) (values :keyword nil))
          ((char= first #\&) (values :lambda-key nil))
          (pending (values pending nil))
          ((and operator (%definer-face text start end))
           (values :special (%definer-face text start end)))
          ((%special-form-p text start end) (values :special nil))
          (t (values nil nil)))))

(defun %scan-line (text in collect)
  "(values OUT-STATE RUNS) of the single line TEXT entered in the state IN."
  (let ((runs '())
        (operator nil)
        (pending nil))
    (let ((out (%map-line-tokens
                text in
                (lambda (what start end step)
                  (let ((face
                          (case what
                            (:comment :comment)
                            (:string :string)
                            (:char (setf operator nil pending nil) nil)
                            (:open (setf operator t pending nil)
                                   (aref *paren-faces* step))
                            (:close (setf operator nil pending nil)
                                    (aref *paren-faces* step))
                            (t
                             (cond (collect
                                    (multiple-value-bind (face next)
                                        (%atom-face text start end operator pending)
                                      (setf operator nil pending next)
                                      face))
                                   (t (setf operator nil) nil))))))
                    (when (and collect face)
                      (push (list start end face) runs)))))))
      (values out (nreverse runs)))))

;;; ------------------------------------------------------------- what is kept

(defconstant +reading-cache-lines+ 65536
  "Line states one reading keeps before it starts over.")

(defstruct (reading (:constructor %make-reading ())
                    (:conc-name reading-)
                    (:copier nil)
                    (:predicate nil))
  "What reading one view's text keeps."
  (states (make-hash-table :test 'eql))
  (valid 0)
  (top 0)
  (open-shape nil)
  (close-shape nil))

(defun %reading (view)
  (or (slot-value view 'mode-state)
      (setf (slot-value view 'mode-state) (%make-reading))))

(defun %line-text (buffer line)
  (buffer-substring buffer (buffer-line-start buffer line) (buffer-line-end buffer line)))

(defun %line-state (view line)
  "The state LINE is entered in."
  (let* ((reading (%reading view))
         (buffer (text-view-buffer view))
         (count (buffer-line-count buffer)))
    (when (<= +reading-cache-lines+ (hash-table-count (reading-states reading)))
      (clrhash (reading-states reading))
      (setf (reading-valid reading) 0
            (reading-top reading) 0))
    (let ((states (reading-states reading)))
      (loop while (and (<= (reading-valid reading) line)
                       (< (reading-valid reading) count))
            do (let* ((k (reading-valid reading))
                      (in (if (zerop k)
                              (%fresh-lex-state)
                              (cdr (gethash (1- k) states))))
                      (old (gethash k states)))
                 (unless (and old (%same-lex-state-p (car old) in))
                   (setf (gethash k states)
                         (cons in (%scan-line (%line-text buffer k) in nil))))
                 (setf (reading-valid reading) (1+ k)
                       (reading-top reading) (max (reading-top reading) (1+ k)))))
      (car (gethash (min line (1- count)) states)))))

(defmethod mode-invalidate ((mode (eql :lisp)) view first delta)
  "Keep what the edit left alone and read again from the first line it touched."
  (declare (ignore mode))
  (let ((reading (slot-value view 'mode-state)))
    (when reading
      (let ((states (reading-states reading)))
        (cond ((eql delta 0)
               (remhash first states))
              (t
               ;; the lines under the edit moved, and no state of theirs is theirs at
               ;; its new number; the window reads back what it shows
               (loop for line from first below (reading-top reading)
                     do (remhash line states))
               (setf (reading-top reading) (min (reading-top reading) first)))))
      (setf (reading-valid reading) (min (reading-valid reading) first)))))

;;; ------------------------------------------------------------------ the runs

(defmethod row-markup-key ((mode (eql :lisp)) view line row)
  "The measured line, the row of it and the state that line is entered in."
  (declare (ignore mode))
  (list (layout-line (text-view-layout view) line) row (%line-state view line)))

(defmethod row-markup ((mode (eql :lisp)) view line row)
  (declare (ignore mode))
  (let* ((layout (text-view-layout view))
         (line-layout (layout-line layout line))
         (open (line-layout-row-start line-layout row))
         (close (line-layout-row-end line-layout row)))
    (multiple-value-bind (out runs)
        (%scan-line (%line-text (layout-buffer layout) line) (%line-state view line) t)
      (declare (ignore out))
      (loop for (start end face) in runs
            for lo = (max start open)
            for hi = (min end close)
            when (< lo hi)
              collect (list (- lo open) (- hi open) (list :color (face-ink face)))))))

;;; ------------------------------------------------------- the pair around the caret

(defun %line-parens (text in)
  "One (COLUMN . OPENP) per parenthesis of the single line TEXT that is code, in the order they stand."
  (let ((parens '()))
    (%map-line-tokens text in
                      (lambda (what start end step)
                        (declare (ignore end step))
                        (case what
                          (:open (push (cons start t) parens))
                          (:close (push (cons start nil) parens)))))
    (nreverse parens)))

(defun %paren-at (view index openp)
  "(values LINE COLUMN) of the parenthesis at INDEX when it is code and opens the list or closes it as OPENP says, and NIL otherwise."
  (let* ((buffer (text-view-buffer view))
         (line (buffer-line-of-index buffer index))
         (column (- index (buffer-line-start buffer line)))
         (cell (assoc column (%line-parens (%line-text buffer line)
                                           (%line-state view line)))))
    (when (and cell (eq (not (cdr cell)) (not openp)))
      (values line column))))

(defun %match-backward (view line column)
  "Document index of the parenthesis that opens the one at COLUMN of LINE, or NIL when none does."
  (let ((buffer (text-view-buffer view))
        (depth 0))
    (loop for l from line downto 0
          do (let ((parens (%line-parens (%line-text buffer l) (%line-state view l)))
                   (limit (if (= l line) column most-positive-fixnum)))
               (dolist (paren (reverse parens))
                 (when (< (car paren) limit)
                   (cond ((not (cdr paren)) (incf depth))
                         ((zerop depth)
                          (return-from %match-backward
                            (+ (buffer-line-start buffer l) (car paren))))
                         (t (decf depth)))))))
    nil))

(defun %match-forward (view line column)
  "Document index of the parenthesis that closes the one at COLUMN of LINE, or NIL."
  (let* ((buffer (text-view-buffer view))
         (count (buffer-line-count buffer))
         (depth 0))
    (loop for l from line below count
          do (let ((parens (%line-parens (%line-text buffer l) (%line-state view l)))
                   (limit (if (= l line) column -1)))
               (dolist (paren parens)
                 (when (> (car paren) limit)
                   (cond ((cdr paren) (incf depth))
                         ((zerop depth)
                          (return-from %match-forward
                            (+ (buffer-line-start buffer l) (car paren))))
                         (t (decf depth)))))))
    nil))

(defun %match-pair (view index)
  "(values OPEN CLOSE) of the pair the caret at INDEX stands against."
  (let* ((buffer (text-view-buffer view))
         (size (buffer-size buffer)))
    (cond ((and (plusp index) (<= index size)
                (char= (buffer-char buffer (1- index)) #\)))
           (multiple-value-bind (line column) (%paren-at view (1- index) nil)
             (when line
               (let ((open (%match-backward view line column)))
                 (when open (values open (1- index)))))))
          ((and (< index size) (char= (buffer-char buffer index) #\())
           (multiple-value-bind (line column) (%paren-at view index t)
             (when line
               (let ((close (%match-forward view line column)))
                 (when close (values index close)))))))))

(defun %char-width (view index)
  "Device pixels the character at INDEX travels: the cell one of the pair stands on."
  (let ((context (text-view-context view)))
    (pen-advance (org.shirakumo.alloy.renderers.opengl.msdf::data (context-font context))
                 (buffer-substring (text-view-buffer view) index (1+ index))
                 0 1 (context-scale context))))

(defun %make-paren-shape (view id)
  (let ((shape (org.shirakumo.alloy.renderers.simple:rectangle
                (text-view-renderer view)
                (org.shirakumo.alloy:px-extent 0 0 0 0)
                :pattern *paren-match-ground*
                :z-index 1)))
    (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape id view) shape)
    shape))

(defun %place-paren (view shape id index)
  "Put the whole cell of the parenthesis at INDEX under it and answer the shape, making it the first time and hiding it while that parenthesis is on no drawn row."
  (let ((shape (or shape (%make-paren-shape view id))))
    (multiple-value-bind (x y) (when index (text-view-caret-location view index))
      (cond (x
             (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) nil)
             (reinitialize-instance
              shape
              :bounds (org.shirakumo.alloy:px-extent x y (%char-width view index)
                                                     (text-view-pitch view))))
            (t
             (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) t))))
    shape))

(defmethod mode-place ((mode (eql :lisp)) view)
  (declare (ignore mode))
  (let ((reading (%reading view))
        (caret (text-view-caret view)))
    (multiple-value-bind (open close)
        ;; the pair is looked for only with the caret standing against a parenthesis,
        ;; which is what keeps every other caret move free of it
        (when (and caret (text-view-layout view)) (%match-pair view caret))
      (setf (reading-open-shape reading)
            (%place-paren view (reading-open-shape reading) :paren-open open))
      (setf (reading-close-shape reading)
            (%place-paren view (reading-close-shape reading) :paren-close close)))))

(defmethod mode-run-at ((mode (eql :lisp)) view index)
  "A multiple click on a parenthesis takes the balanced form it opens or closes."
  (declare (ignore mode))
  (when (eq (text-view-gesture-policy view) :editor)
    (let* ((buffer (text-view-buffer view))
           (size (buffer-size buffer)))
      (when (< index size)
        (let ((character (buffer-char buffer index)))
          (cond ((char= character #\()
                 (multiple-value-bind (open close) (%match-pair view index)
                   (when open (values open (1+ close)))))
                ((char= character #\))
                 (multiple-value-bind (open close) (%match-pair view (1+ index))
                   (when open (values open (1+ close)))))))))))

;;; ------------------------------------------------------------------ the columns

(defparameter *body-indent* 2
  "Columns in from its own parenthesis the body of a form stands.")

(defparameter *special-indent* 4
  "Columns in from its own parenthesis an argument a form treats apart stands.")

(defparameter *indent-forms*
  '(("progn" 0) ("prog1" 1) ("prog2" 2) ("tagbody" 0) ("locally" 0)
    ("let" 1) ("let*" 1) ("flet" 1) ("labels" 1) ("macrolet" 1) ("symbol-macrolet" 1)
    ("when" 1) ("unless" 1) ("if" 2) ("lambda" 1) ("block" 1) ("catch" 1)
    ("dolist" 1) ("dotimes" 1) ("do" 2) ("do*" 2) ("eval-when" 1)
    ("case" 1) ("ecase" 1) ("ccase" 1) ("typecase" 1) ("etypecase" 1) ("ctypecase" 1)
    ("unwind-protect" 1) ("handler-case" 1) ("handler-bind" 1)
    ("restart-case" 1) ("restart-bind" 1)
    ("multiple-value-bind" 2) ("destructuring-bind" 2)
    ("with-open-file" 1) ("with-output-to-string" 1) ("with-input-from-string" 1)
    ("with-slots" 2) ("with-accessors" 2)
    ("defun" 2 2 7) ("defmacro" 2 2 7) ("defmethod" 2 2 7) ("defgeneric" 2 2 7)
    ("defclass" 2) ("defstruct" 1) ("define-condition" 2) ("deftype" 2)
    ("defvar" 1) ("defparameter" 1) ("defconstant" 1) ("defpackage" 1))
  "Operator, how many of its arguments it treats apart, and the two columns it puts its body and those arguments in.")

(defparameter *loop-indent* 2
  "Columns in from its own parenthesis a clause stands when the first one opens a line of its own.")

(defparameter *loop-body-indent* 2
  "Columns further in the clause a conditional head is still waiting for stands.")

(defparameter *loop-conditionals* '("when" "unless" "if")
  "Clause heads that put the clause after them further in.")

(defparameter *form-scan-lines* 2000
  "Lines the reading looks back over for the start of the form a line sits in.")

(defun %indent-entry (head)
  (assoc head *indent-forms* :test #'string-equal))

(defstruct (frame (:constructor %make-frame (line column))
                  (:conc-name frame-)
                  (:copier nil)
                  (:predicate nil))
  "One list still open: the LINE and COLUMN its parenthesis stands at, the HEAD it opens with and where that head stands, the column of the first thing after the head, and how many things follow it."
  line column (head nil) (head-column nil) (first-column nil) (args 0))

(defun %note-token (frame column text start end)
  "Take one thing of FRAME's list into account."
  (cond ((null (frame-head-column frame))
         (setf (frame-head-column frame) column
               (frame-head frame) (when text (subseq text start end))))
        ((null (frame-first-column frame))
         (setf (frame-first-column frame) column
               (frame-args frame) 1))
        (t (incf (frame-args frame)))))

(defun %walk-to (source from line)
  "(values STACK STATE) at the start of LINE, read down from line FROM."
  (let ((state (%fresh-lex-state))
        (stack '()))
    (loop for l from from below line
          do (let ((text (funcall source l)))
               (setf state
                     (%map-line-tokens
                      text state
                      (lambda (what start end step)
                        (declare (ignore step))
                        (case what
                          (:open
                           (when stack (%note-token (first stack) start nil 0 0))
                           (push (%make-frame l start) stack))
                          (:close (pop stack))
                          (:comment nil)
                          (:atom
                           (when stack (%note-token (first stack) start text start end)))
                          (t
                           (when stack
                             (%note-token (first stack) start nil 0 0)))))))))
    (values stack state)))

(defun %form-start (source line)
  "The line the form holding LINE is taken to start at."
  (let ((floor (max 0 (- line *form-scan-lines*))))
    (loop for l from (1- line) downto floor
          do (let ((text (funcall source l)))
               (when (and (plusp (length text)) (char= (char text 0) #\())
                 (return-from %form-start l))))
    floor))

(defun %blank-length (text)
  (or (position-if-not (lambda (character) (member character '(#\Space #\Tab))) text)
      (length text)))

(defun %lone-semicolon-p (text)
  "True for a line holding nothing but a comment opened with ONE semicolon, which is a comment about the line beside it and stays in the column it was put in."
  (let ((start (%blank-length text)))
    (and (< start (length text))
         (char= (char text start) #\;)
         (or (<= (length text) (1+ start))
             (char/= (char text (1+ start)) #\;)))))

(defun %first-word (text)
  "The symbol a line opens with, or NIL when it opens with anything else."
  (let ((start (%blank-length text)))
    (when (and (< start (length text))
               (not (member (char text start) *delimiters*))
               (char/= (char text start) #\#))
      (subseq text start (%word-close text start)))))

(defun %previous-word (source line floor)
  "The symbol the nearest line above LINE opens with, down to FLOOR."
  (loop for l from (1- line) downto floor
        do (let ((text (funcall source l)))
             (when (< (%blank-length text) (length text))
               (return-from %previous-word (%first-word text)))))
  nil)

(defun %loop-column (frame source line)
  "The column a clause belongs in."
  (let ((base (or (frame-first-column frame) (+ (frame-column frame) *loop-indent*)))
        (previous (%previous-word source line (frame-line frame))))
    (if (and previous (member previous *loop-conditionals* :test #'string-equal))
        (+ base *loop-body-indent*)
        base)))

(defun %frame-column (frame source line)
  "The column the next line of FRAME's list belongs in."
  (let* ((open (frame-column frame))
         (head (frame-head frame))
         (entry (and head (%indent-entry head))))
    (cond ((null head) (or (frame-head-column frame) (1+ open)))
          ((string-equal head "loop") (%loop-column frame source line))
          (entry
           (let ((special (second entry))
                 (body (or (third entry) *body-indent*))
                 (apart (or (fourth entry) *special-indent*)))
             (if (< (frame-args frame) special) (+ open apart) (+ open body))))
          ;; an operator nothing is known about heads a call, whose arguments stand
          ;; under the first of them
          (t (or (frame-first-column frame) (1+ open))))))

(defun %indent-column (source line)
  "The column the first character of LINE belongs in, or NIL when the line is not the reading's to move."
  (let ((text (funcall source line)))
    (multiple-value-bind (stack state) (%walk-to source (%form-start source line) line)
      (cond ((not (eq (lex-state-kind state) :code)) nil)
            ((%lone-semicolon-p text) nil)
            ((null stack) 0)
            (t (%frame-column (first stack) source line))))))

;;; ------------------------------------------------------------------ the writing

(defun %indent-source (buffer becoming first)
  "A reader of line texts answering what BECOMING holds for the lines from FIRST and the buffer's own for every other, so a run of lines is read off what the lines above them are BECOMING and not off what they still are."
  (lambda (line)
    (let ((i (- line first)))
      (if (and (<= 0 i) (< i (length becoming)) (aref becoming i))
          (aref becoming i)
          (%line-text buffer line)))))

(defun %indent-lines (view first last)
  "Put the lines FIRST through LAST in the columns they belong in as ONE step of the history, writing spaces alone and leaving the caret the characters it stood past."
  (let ((buffer (text-view-buffer view)))
    (%check-user-range view (buffer-line-start buffer first)
                       (buffer-line-end buffer last)))
  (let* ((buffer (text-view-buffer view))
         (becoming (make-array (1+ (- last first)) :initial-element nil))
         (source (%indent-source buffer becoming first))
         (caret (or (text-view-caret view) 0))
         (caret-line (buffer-line-of-index buffer caret))
         (caret-rest 0))
    (loop for line from first to last
          do (let* ((text (%line-text buffer line))
                    (blank (%blank-length text))
                    (column (%indent-column source line)))
               (when (eql line caret-line)
                 (setf caret-rest
                       (max 0 (- caret (buffer-line-start buffer line) blank))))
               (setf (aref becoming (- line first))
                     (if column
                         (concatenate 'string
                                      (make-string column :initial-element #\Space)
                                      (subseq text blank))
                         text))))
    (let* ((start (buffer-line-start buffer first))
           (end (buffer-line-end buffer last))
           (old (buffer-substring buffer start end))
           (new (with-output-to-string (out)
                  (loop for line from first to last
                        for i from 0
                        do (when (plusp i) (terpri out))
                           (write-string (aref becoming i) out)))))
      (unless (string= old new)
        (buffer-replace buffer start end new
                        :point caret :anchor (text-view-anchor view))
        (%view-move-mark view 'anchor nil)
        (let ((line-start (buffer-line-start buffer caret-line)))
          (%view-move-mark
           view 'caret
           (min (buffer-size buffer)
                (+ line-start (%blank-length (%line-text buffer caret-line))
                   caret-rest))))))))

(define-command :indent-line (view)
  "Put the line the caret is on in the column the text around it gives it."
  (let ((line (buffer-line-of-index (text-view-buffer view) (%point view))))
    (%indent-lines view line line)))

(define-command :indent-region (view)
  "Put every line the marking covers in the column the text around it gives it, the whole of it one step of the history."
  (let ((buffer (text-view-buffer view)))
    (multiple-value-bind (lo hi) (%view-marked-range view)
      (%indent-lines view
                     (buffer-line-of-index buffer (or lo (%point view)))
                     (buffer-line-of-index buffer (or hi (%point view)))))))

(defparameter *lisp-keymap*
  '((:tab () :indent-line))
  "What reading the text as code binds in front of the view's own table.")

(defmethod mode-keymap ((mode (eql :lisp)))
  (declare (ignore mode))
  *lisp-keymap*)
