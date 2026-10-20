(in-package :oa)

;;; Common Lisp syntax colouring for the Lisp editor panes.
;;;
;;; Two passes.  The SYNTACTIC one owns comments, strings, #|...|#, #\c and
;;; #+/#-; the SEMANTIC one colours only what the first left untouched.  The
;;; invariant is that a range carries exactly ONE category, the syntactic one
;;; winning: here the two passes run interleaved in a single walk, and the
;;; semantic branch only ever sees characters the syntactic branch declined, so
;;; the invariant holds by construction rather than by a check.
;;;
;;; One style per range is also what the renderer supports: simple::flatten-markup
;;; keeps a single style per element and drops only one style per shared end
;;; offset (alloy renderers/simple/defaults.lisp: flatten-markup), so ranges are emitted
;;; strictly ascending and disjoint, which makes shared end offsets impossible.
;;;
;;; The analyser never reads: it is a character walk, so no reader macro, no
;;; #. evaluation and no package error can fire on half-typed text.

;;; ---------------------------------------------------------------- palette
;;;
;;; THE ONE PLACE COLOURS ARE DEFINED.  Columns: reference value, applied value,
;;; measured contrast against the #FFFFFF pane background.
;;;
;;; Reference = the colour each category has in the editor this one is a port of,
;;; read out of the running OpenMusic 8.0.  That editor keeps a light/dark pair
;;; per category and picks by background; the values here are the LIGHT-background
;;; half of each pair, since this pane is white.  The X11 colour names in the
;;; trailing comments are what those values are called.
;;;
;;; Applied = the reference value untouched when it already clears the 4.5:1 WCAG
;;; floor.  The four that do not are scaled down uniformly in sRGB, which
;;; preserves the HSV hue, until they clear 5.5:1 -- a margin, because a colour
;;; sitting on the 4.5 boundary still reads as washed out.  The contrast column
;;; is computed from these very numbers.
(defparameter *om-lisp-syntax-palette*
  ;; category          reference applied   contrast
  '((:comment          #xB22222  #xB22222  6.68) ; firebrick
    (:string           #xBC8F8F  #x806161  5.53) ; rosybrown
    (:keyword-operator #xA020F0  #xA020F0  5.30) ; purple
    (:function-name    #x0000FF  #x0000FF  8.59) ; blue
    (:variable-name    #xB8860B  #x876208  5.55) ; darkgoldenrod
    (:type-name        #x228B22  #x1E791E  5.51) ; forestgreen
    (:keyword-symbol   #xDA70D6  #x974D94  5.52) ; orchid
    ;; numbers get no colour of their own in the reference either; the category
    ;; exists so the scanner does not mistake 1.0e-3 for a symbol.  NIL =
    ;; painted in the pane's normal foreground.
    (:number           nil       nil       nil)
    ;; Rainbow parens: the reference cycles 6 colours.  Structural glyphs, so
    ;; the bar is 3:1 rather than 4.5:1 -- all six clear it and none is altered.
    (:paren-0          #xFF0000  #xFF0000  4.00) ; red
    (:paren-1          #x000000  #x000000 21.00) ; black
    (:paren-2          #x006400  #x006400  7.44) ; darkgreen
    (:paren-3          #xCD6600  #xCD6600  3.82) ; darkorange3
    (:paren-4          #x0000FF  #x0000FF  8.59) ; blue
    (:paren-5          #xA020F0  #xA020F0  5.30)) ; purple
  "Category -> (reference-rgb applied-rgb contrast).  Replace the APPLIED column
to retune; nothing else in this file carries a colour.")

(defparameter *om-lisp-syntax-colors*
  (let ((table (make-hash-table :test 'eq)))
    (dolist (row *om-lisp-syntax-palette* table)
      (destructuring-bind (category lw applied contrast) row
        (declare (ignore lw contrast))
        (when applied
          (setf (gethash category table)
                (chroma:rgb (/ (ldb (byte 8 16) applied) 255.0)
                            (/ (ldb (byte 8 8) applied) 255.0)
                            (/ (ldb (byte 8 0) applied) 255.0)
                            1.0))))))
  "Category -> chroma colour, built from the APPLIED column above.")

;;; ---------------------------------------------------------------- scanning

(defun %lisp-symbol-char-p (c)
  (not (or (char= c #\Space) (char= c #\Tab) (char= c #\Newline)
           (char= c #\Return) (char= c #\Page)
           (char= c #\() (char= c #\)) (char= c #\") (char= c #\;)
           (char= c #\') (char= c #\`) (char= c #\,))))

(defun %lisp-token-end (text i len)
  (loop while (and (< i len) (%lisp-symbol-char-p (char text i)))
        do (incf i))
  i)

(defun %lisp-number-token-p (token)
  "TOKEN reads as a number without consulting the reader: a digit somewhere, no
package marker, and only characters a numeric literal may carry."
  (let ((len (length token)))
    (and (plusp len)
         (find-if #'digit-char-p token)
         (not (find #\: token))
         (let ((head (char token 0)))
           (or (digit-char-p head) (find head "+-.#")))
         (every (lambda (c)
                  (or (alphanumericp c) (find c "+-./^_")))
                token))))

;;; The head of a form is the symbol right after an open paren, tracked
;;; STRUCTURALLY: a definition indented inside another form is still a
;;; definition, so nothing here depends on the open paren sitting at column 0.

(defun %lisp-def-name-category (head)
  "Category for the NAME that HEAD defines, or NIL when HEAD opens no definition.

CONTRACT, and it is exhaustive by construction rather than by enumeration: a head
whose name begins with \"def\" opens a definition, and the name it defines falls
into exactly one of three groups, tested by PREFIX:

  function  head starts with defun / defmacro / defmethod / defgeneric
  variable  head starts with defvar / defparameter
  type      every other def* head

So the type group absorbs defclass, defstruct, deftype, defconstant,
define-condition, defpackage, defsetf, define-modify-macro, define-symbol-macro,
define-method-combination and any library-defined def* alike -- no list to keep
in sync.  Because the test is a prefix and not an exact name, OM's own def-forms
need no special case either: defmethod! / defmethod* / defgeneric! / defgeneric*
land in the function group, defclass! / defclass* / defclas in the type group.
That is load-bearing -- the OM kernel declares its generics with defmethod*, and
the reference editor paints such a name in the function colour, e.g.
(defmethod* omNG-box-value ...) at kernel omloop.lisp (omNG-box-value for loopIterators)."
  (flet ((starts-with-any (h prefixes)
           (some (lambda (p)
                   (and (>= (length h) (length p))
                        (string= p h :end2 (length p))))
                 prefixes)))
    (let ((h (string-downcase head)))
      (when (and (> (length h) 3) (string= "def" h :end2 3))
        (cond ((starts-with-any h '("defun" "defmacro" "defmethod" "defgeneric"))
               :function-name)
              ((starts-with-any h '("defvar" "defparameter"))
               :variable-name)
              (t :type-name))))))

;;; The control structures that take the operator colour.
;;;
;;; This list is CURATED, and that is the point -- it is NOT an approximation to
;;; be "improved".  SETF, LAMBDA, INCF, FUNCALL, MIN and LIST carry NO colour in
;;; the reference, unlike a SPECIAL-OPERATOR-P / MACRO-FUNCTION classification on
;;; the live image, which would paint every one of those as an operator -- more
;;; colour than the reference gives them.  Ordinary calls and macros stay
;;; uncoloured on purpose.
(defparameter *om-lisp-control-structures*
  '("when" "unless" "cond" "if"
    "case" "ecase" "typecase" "etypecase"
    "loop" "loop-finish" "do" "do*" "dotimes" "dolist"
    "do-symbols" "do-external-symbols" "do-all-symbols"
    "proclaim" "declaim" "declare"
    "flet" "labels" "macrolet"
    "block" "return" "return-from"
    "catch" "throw" "tagbody"
    "handler-case" "handler-bind"
    "restart-case" "restart-bind" "with-simple-restart"
    "let" "let*" "prog" "destructuring-bind"
    "multiple-value-bind" "multiple-value-prog1" "values"
    "progn" "progv" "prog1" "prog2"
    "inline" "unwind-protect"
    "eval-when"
    "with-slots" "with-accessors"
    "with-input-from-string" "with-output-to-string"
    "with-open-file" "with-open-stream"
    "with-hash-table-iterator" "with-package-iterator"
    "with-compilation-unit" "with-standard-io-syntax"
    "pprint-logical-block" "print-unreadable-object")
  "Heads that take the operator colour. Curated on purpose -- see the note above.")

(defun %lisp-operator-p (name)
  (member name *om-lisp-control-structures* :test #'string-equal))

(defparameter *om-lisp-paren-categories*
  #(:paren-0 :paren-1 :paren-2 :paren-3 :paren-4 :paren-5)
  "Cycle length 6, matching the reference editor's parenthesis colour cycle.")

(defun %lisp-paren-category (depth)
  (svref *om-lisp-paren-categories*
         (mod depth (length *om-lisp-paren-categories*))))

(defun %lisp-syntax-tokens (text)
  "TEXT -> list of (START END CATEGORY), strictly ascending and disjoint.

Classification is purely textual: a def* head and its name, the curated
control-structure list, a leading : or &, and the nesting depth for parens.  No
symbol is looked up, so nothing depends on which package or thread happens to be
current, and no half-typed name can signal.

Total function: every input, however incomplete or malformed, yields the ranges
that could be determined.  An unterminated string or #| runs to end of text;
nothing signals."
  (let ((len (length text))
        (i 0)
        (out '())
        (depth 0)
        ;; the symbol about to be read is the head of its form
        (head-p nil)
        ;; category owed to the name that follows a def* head
        (name-owed nil)
        ;; a name may arrive wrapped, as in (defmethod (setf slot) ...)
        (name-depth nil))
    (flet ((emit (start end category)
             (when (and category (< start end))
               (push (list start end category) out))))
      (loop while (< i len)
            do (let ((c (char text i)))
                 (cond
                   ;; ---- syntactic pass (owns these ranges outright)
                   ;; to line END; the newline itself stays outside the range
                   ((char= c #\;)
                    (let ((e (or (position #\Newline text :start i) len)))
                      (emit i e :comment)
                      (setf i (max e (1+ i)))))
                   ((char= c #\")
                    (let ((e (min len (%lisp-skip-string text i len))))
                      (emit i e :string)
                      (setf i (max e (1+ i)))
                      (setf head-p nil)))
                   ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\|))
                    (let ((e (min len (%lisp-skip-block-comment text i len))))
                      (emit i e :comment)
                      (setf i (max e (1+ i)))))
                   ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\))
                    (let ((e (min len (%lisp-skip-char-literal text i len))))
                      (emit i e :number)
                      (setf i (max e (1+ i)))
                      (setf head-p nil)))
                   ;; #+feature / #-feature: the feature expression itself takes
                   ;; the comment colour, the guarded form does not
                   ((and (char= c #\#) (< (1+ i) len)
                         (find (char text (1+ i)) "+-"))
                    (let ((e (if (and (< (+ i 2) len) (char= (char text (+ i 2)) #\())
                                 ;; #+(or a b): the whole feature form
                                 (let ((m (%lisp-find-matching-paren text (+ i 2))))
                                   (if m (1+ (cdr m)) len))
                                 (%lisp-token-end text (+ i 2) len))))
                      (emit i (min e len) :comment)
                      (setf i (max e (1+ i)))))
                   ;; |escaped symbol|: skipped, and takes no colour
                   ((char= c #\|)
                    (let ((e (or (position #\| text :start (1+ i)) (1- len))))
                      (setf i (+ e 1))
                      (setf head-p nil)))
                   ((char= c #\\)
                    (incf i 2))
                   ;; ---- structure.  The cycle runs on the REAL nesting
                   ;; depth, which is what "one colour per level" means.  The
                   ;; reference editor instead advances per open paren and
                   ;; restarts at every newline -- a line-scanner artefact, not
                   ;; a requirement, so it is deliberately not reproduced.  A
                   ;; closer takes the colour of the opener it matches.
                   ((char= c #\()
                    (emit i (1+ i) (%lisp-paren-category depth))
                    (incf depth)
                    (incf i)
                    ;; (defmethod (setf x) ...): the name is inside this paren
                    (if (and name-owed (null name-depth))
                        (setf name-depth depth)
                        (setf head-p t)))
                   ((char= c #\))
                    (when (and name-depth (= depth name-depth))
                      (setf name-depth nil))
                    (decf depth)
                    (emit i (1+ i) (%lisp-paren-category (max 0 depth)))
                    (incf i)
                    (setf head-p nil))
                   ((member c '(#\Space #\Tab #\Newline #\Return #\Page))
                    (incf i))
                   ((find c "'`,")
                    (incf i))
                   ;; ---- semantic pass (only reaches uncoloured characters)
                   (t
                    (let* ((e     (%lisp-token-end text i len))
                           (token (subseq text i (max e (1+ i))))
                           (category
                             (cond
                               ;; :foo keyword symbols get their own colour
                               ((and (> (length token) 1) (char= (char token 0) #\:))
                                :keyword-symbol)
                               ;; &optional &key ... share the type colour
                               ((and (> (length token) 1) (char= (char token 0) #\&))
                                :type-name)
                               ((%lisp-number-token-p token) :number)
                               ;; the name a def* form is defining; a name
                               ;; written (setf foo) belongs to FOO, not to the
                               ;; setf
                               ((and name-owed name-depth (string-equal token "setf"))
                                nil)
                               (name-owed (prog1 name-owed (setf name-owed nil)))
                               (head-p
                                (let ((def (%lisp-def-name-category token)))
                                  (cond (def (setf name-owed def) :keyword-operator)
                                        ((%lisp-operator-p token) :keyword-operator)
                                        (t nil))))
                               ;; NOTE: no rule for *earmuffs* at a usage site.
                               ;; In the vanilla, *package* and *counter*
                               ;; referenced inside a form come back with NO
                               ;; colour -- an earmuffed name takes
                               ;; the variable colour only where defvar or
                               ;; defparameter DEFINES it.
                               (t nil))))
                      (emit i e category)
                      (setf head-p nil)
                      (setf i (max e (1+ i))))))))
      (nreverse out))))

;;; ------------------------------------------------------------------ bridge

(defun %lisp-syntax-markup (text)
  "Markup elements (START END (:COLOR c)) for TEXT, ascending and disjoint."
  (loop for (start end category) in (%lisp-syntax-tokens text)
        for color = (gethash category *om-lisp-syntax-colors*)
        when color collect (list start end (list :color color))))

;;; The pane hands the same string to every render pass, and Alloy edits it IN
;;; PLACE (alloy components/text-input.lisp: insert-text and delete-text of
;;; text-input-component reuse one adjustable array), so
;;; object identity cannot tell two buffer states apart.  The key is therefore
;;; the CONTENT: a private copy compared with STRING=, which is a memcmp and
;;; costs far less than rescanning, and which no mutation path -- typing, paste,
;;; undo, programmatic (setf value) -- can leave stale.  Weak keys, so the entry
;;; dies with the pane and adds no window root of its own.
(defvar *om-lisp-syntax-cache*
  (make-hash-table :test 'eq :weakness :key :synchronized t))

;;; Every pane that holds LISP SOURCE is coloured, which is the paren-match family:
;;; the OMLispPatch / Lisp editor, the Listener REPL, and the TREE, TEMPO and
;;; TREE-TEMPO editors -- the editors vanilla builds with :lisp-editor? t
;;; (om-LW editors/treeeditor.lisp, tempoeditor.lisp, treetempoeditor.lisp,
;;; commenteditor.lisp), plus the listener pane, whose Lisp font-lock CAPI supplies
;;; natively.  A pane whose content is not source -- the Listener OUTPUT, the
;;; kernel's plain text boxes -- is not in this family and keeps the NIL default.
(defmethod %syntax-ranges ((pane om-paren-edit-view-widget))
  (let ((text  (or (alloy:value pane) ""))
        (entry (gethash pane *om-lisp-syntax-cache*)))
    (if (and entry
             (= (length text) (length (car entry)))
             (string= text (car entry)))
        (cdr entry)
        (let ((markup (%lisp-syntax-markup text)))
          (setf (gethash pane *om-lisp-syntax-cache*)
                (cons (copy-seq text) markup))
          markup))))
