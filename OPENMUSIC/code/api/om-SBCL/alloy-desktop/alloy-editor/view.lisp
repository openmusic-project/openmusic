(in-package :alloy-editor)

(defparameter *unwrapped-width* 100000.0
  "Width a line is laid out in when it does not wrap: past any line that is drawn.")

(defparameter *wheel-rows* 3.0
  "Visual rows one wheel notch travels.")

(defparameter *default-font-size* (org.shirakumo.alloy:un 12)
  "Draw size a view takes when the host names none.")

(defparameter *default-foreground* org.shirakumo.alloy.chroma.colors:black
  "Ink a view takes when the host names no foreground.")

(defparameter *default-background* org.shirakumo.alloy.chroma.colors:white
  "Ground a view takes when the host names no background.")

(defparameter *selection-foreground* org.shirakumo.alloy.chroma.colors:white
  "Ink the characters a marking covers are drawn in, whatever they would be drawn in without it.")

(defvar *modifiers* '()
  "Modifier keys held down, as the last key event any view saw reported them.")

;;; --------------------------------------------------------- the frame of a row

(defparameter *gutter-digits* 5
  "Digits wide a column of line numbers is when the host names no count.")

(defparameter *gutter-sample-digit* #\8
  "Digit the fixed width of that column is measured with.")

(defparameter *gutter-right-gap* (org.shirakumo.alloy:un 2)
  "Room between the last digit of the column and the rule closing it.")

(defparameter *gutter-rule-width* 1.0
  "Device pixels of the rule closing the column of line numbers.")

(defparameter *continuation-number* "---"
  "What the column shows on a row that continues the line above it.")

(defparameter *wrap-marker* (string (code-char 187))
  "Glyph closing every row a line was broken at.")

(defparameter *wrap-marker-gap* (org.shirakumo.alloy:un 2)
  "Room between the last character of a broken row and that glyph.")

(defparameter *default-margin-left* (org.shirakumo.alloy:un 2)
  "Room left of the first character of a row when the host names none.")

(defparameter *default-margin-top* (org.shirakumo.alloy:un 0)
  "Room above the first row when the host names none.")

(defparameter *default-gutter-background*
  org.shirakumo.alloy.chroma.colors:white-smoke
  "Ground the column of line numbers takes when the host names none.")

(defparameter *default-gutter-rule*
  org.shirakumo.alloy.chroma.colors:light-gray
  "Colour of the rule closing that column when the host names none.")

(defun %italic-markup ()
  "Style a line number is drawn in."
  (list (list 0 #xFFFF :italic)))

;;; ------------------------------------------------- the ring and the candidates

(defparameter *input-history-size* 100
  "Entries a view keeps of what was handed over through it, when the host names no count.")

(defparameter *completion-rows* 10
  "Candidates the list shows at once; the rest are reached by walking it.")

(defparameter *completion-background*
  org.shirakumo.alloy.chroma.colors:white-smoke
  "Ground the list of candidates stands on.")

(defparameter *completion-choice-background*
  org.shirakumo.alloy.chroma.colors:light-sky-blue
  "Ground behind the candidate the walk is standing on.")

(defparameter *completion-padding* 2.0
  "Device pixels of room the list leaves around its candidates.")

(defparameter *completion-keymap*
  '((:down () :completion-next)
    (:up () :completion-previous)
    (:enter () :completion-insert)
    (:kp-enter () :completion-insert)
    (:tab () :completion-insert)
    (:escape () :completion-close))
  "Keys the list of candidates takes while it stands open, ahead of every other table.")

;;; ------------------------------------------------------------------ the view

(defclass text-view (org.shirakumo.alloy:component)
  ((buffer     :initarg :buffer :initform nil   :reader text-view-buffer)
   (layout     :initform nil                    :reader text-view-layout)
   (context    :initform nil                    :reader text-view-context)
   (renderer   :initform nil                    :reader text-view-renderer)
   (top-line   :initform 0                      :reader text-view-top-line)
   (top-row    :initform 0                      :reader text-view-top-row)
   (x-offset   :initform 0.0                    :reader text-view-scroll-x)
   (scroll-y   :initform 0.0
               :documentation "Device pixels the first visible row stands above the place it takes unscrolled; a :CLASSIC view scrolls by them, every other family keeps them at zero.")
   (anchor-index :initform nil
                 :documentation "Document index of the row a refit of a :CLASSIC view puts back at the top.")
   (anchor-u :initform 0.0
             :documentation "Device pixels the viewport top stood below the top of that row as it was taken.")
   (anchor-caret-p :initform nil
                   :documentation "True when the caret was in view as that row was taken.")
   (settled :initform nil
            :documentation "(WIDTH HEIGHT LINE ROW SCROLL-Y AT-END) a :CLASSIC view last laid out at, AT-END true while a refit holds it to the end of its content.")
   (pending-show :initform nil
                 :documentation "(INDEX . ROWS) of a placement asked for before the view was laid out.")
   (trailing-lines :initform nil :accessor text-view-trailing-lines
                   :documentation "Empty rows a :CLASSIC view's content keeps under its last line once it is taller than the view, or NIL for one when the text ends with a Linefeed and none otherwise.")
   (wrap-p     :initarg :wrap-p :initform t     :reader text-view-wrap-p)
   (font       :initarg :font                   :reader text-view-font)
   (font-size  :initarg :font-size              :reader text-view-font-size)
   (line-pitch :initarg :line-pitch             :reader text-view-line-pitch)
   (bg-color   :initarg :bg-color               :accessor text-view-bg-color)
   (fg-color   :initarg :fg-color               :accessor text-view-fg-color)
   (pitch      :initform 0.0                    :reader text-view-pitch)
   (visible    :initform #()                    :reader text-view-visible-lines)
   (pool       :initform (make-array 0 :adjustable t :fill-pointer t))
   (pool-keys  :initform (make-array 0 :adjustable t :fill-pointer t))
   (pool-styles :initform (make-array 0 :adjustable t :fill-pointer t))
   (order      :initform #())
   (mode       :initarg :mode :reader text-view-mode
               :documentation "How the text is read, or NIL for plain text.")
   (mode-state :initform nil
               :documentation "Whatever that reading keeps for THIS view.")
   (gutter-p   :initarg :gutter-p :reader text-view-gutter-p
               :documentation "True while a column of line numbers stands left of the rows.")
   (gutter-digits :initarg :gutter-digits :reader text-view-gutter-digits
                  :documentation "Digits that column is wide, whatever it shows.")
   (gutter-bg  :initarg :gutter-bg  :accessor text-view-gutter-background)
   (gutter-fg  :initarg :gutter-fg  :accessor text-view-gutter-foreground)
   (gutter-rule :initarg :gutter-rule :accessor text-view-gutter-rule
                :documentation "Colour of the rule closing that column.")
   (marker-p   :initarg :wrap-marker-p
               :documentation ":POLICY, or whether a row a line was broken at is closed by a glyph.")
   (marker-color :initarg :wrap-marker-color :accessor text-view-wrap-marker-color)
   (margin-left :initarg :margin-left :reader text-view-margin-left)
   (margin-top  :initarg :margin-top  :reader text-view-margin-top)
   (gutter-width :initform 0.0 :reader text-view-gutter-width)
   (gutter-digits-width :initform 0.0)
   (gutter-band-width :initform 0.0)
   (marker-width :initform 0.0)
   (text-x     :initform 0.0)
   (text-inset :initform 0.0)
   (gutter-sample :initform nil)
   (gutter-band :initform nil)
   (marker-band :initform nil)
   (gutter-ground :initform nil)
   (gutter-rule-shape :initform nil)
   (caret      :initform nil)
   (anchor     :initform nil)
   (press      :initform nil)
   (dragging   :initform nil)
   (word       :initform nil)
   (click      :initform nil)
   (scroll-at  :initform nil)
   (scroll-rate :initform nil)
   (scroll-residue :initform 0.0)
   (caret-shape :initform nil)
   (sel-pool   :initform (make-array 0 :adjustable t :fill-pointer t))
   (goal-column :initform nil)
   (last-command :initform nil)
   (keymap     :initarg :keymap     :initform nil :accessor text-view-keymap
               :documentation "The table this view reads its keys through, or NIL to take the one of its family.")
   (overwrite-p :initarg :overwrite-p :initform nil :accessor text-view-overwrite-p
                :documentation "True while what is typed takes the place of the characters after the caret instead of pushing them along.")
   (single-line-p :initarg :single-line-p :initform nil
                  :accessor text-view-single-line-p
                  :documentation "True for a view that holds ONE line.")
   (after-change :initarg :after-change :initform nil :accessor text-view-after-change
                 :documentation "Called with the view once for each command that changed the text.")
   (unhandled-key :initarg :unhandled-key :initform nil
                  :accessor text-view-unhandled-key
                  :documentation "Called with the view and the key event the table binds nothing to.")
   (accept     :initarg :accept     :initform nil :accessor text-view-accept
               :documentation "Called with the view and the text handed over.")
   (message    :initarg :message    :initform nil :accessor text-view-message
               :documentation "Called with the view, a keyword saying what kind of thing happened and a line of text, whenever the editor has something to tell the person.")
   (max-characters :initarg :max-characters :initform nil
                   :accessor text-view-max-characters
                   :documentation "Characters the document is held to, or NIL for no limit.")
   (select-all-on-focus-p :initarg :select-all-on-focus-p :initform nil
                          :accessor text-view-select-all-on-focus-p
                          :documentation "True for a view that marks its whole document the moment it takes the keyboard focus, the way a field of a form does.")
   (focused-p  :initform nil)
   (pending    :initform nil
               :documentation "Text handed in from other threads, newest first, waiting for the thread that draws to put it in.")
   (pending-lock :initform (bordeaux-threads:make-lock "text-view pending text"))
   (input-start :initform nil
                :documentation "Mark opening the stretch of the document the person may change, or NIL while the whole of it is theirs.")
   (input-prompt :initform ""
                 :documentation "What stands immediately before that mark and was put there for them to write after.")
   (history    :initform (make-array 0 :adjustable t :fill-pointer t)
               :reader text-view-history
               :documentation "What was handed over through this view before, oldest first.")
   (history-size :initarg :history-size :accessor text-view-history-size
                 :documentation "Entries that ring keeps before the oldest goes.")
   (history-at :initform nil
               :documentation "Where the walk through the ring stands, or NIL while it is not walking.")
   (history-draft :initform nil
                  :documentation "What was being written when the walk started.")
   (completions :initarg :completions :initform nil :accessor text-view-completions
                :documentation "Called with the view and the prefix under the caret.")
   (completion :initform nil
               :documentation "The list of candidates standing open, or NIL.")
   (completion-ground :initform nil)
   (completion-choice :initform nil)
   (completion-pool :initform (make-array 0 :adjustable t :fill-pointer t))
   (gesture-policy :initarg :gesture-policy :initform :editor
                   :accessor text-view-gesture-policy
                   :documentation "Which family of pane this view answers as."))
  (:default-initargs :data (make-instance 'org.shirakumo.alloy:value-data :value "")
                     :font nil :font-size nil :line-pitch nil
                     :bg-color nil :fg-color nil :keymap nil
                     :after-change nil :unhandled-key nil :accept nil
                     :message nil :max-characters nil :select-all-on-focus-p nil
                     :mode nil :completions nil
                     :history-size *input-history-size*
                     :gutter-p nil :gutter-digits *gutter-digits*
                     :gutter-bg nil :gutter-fg nil :gutter-rule nil
                     :wrap-marker-p :policy :wrap-marker-color nil
                     :margin-left *default-margin-left*
                     :margin-top *default-margin-top*)
  (:documentation "A read-only view of one BUFFER through its LAYOUT."))

(defun make-text-view (buffer &key (wrap-p t wrap-p-supplied-p) font font-size line-pitch
                                   bg-color fg-color (gesture-policy :editor)
                                   keymap single-line-p after-change unhandled-key
                                   accept message max-characters select-all-on-focus-p
                                   completions (history-size *input-history-size*)
                                   mode gutter-p (gutter-digits *gutter-digits*)
                                   gutter-bg gutter-fg gutter-rule
                                   (wrap-marker-p :policy) wrap-marker-color
                                   (margin-left *default-margin-left*)
                                   (margin-top *default-margin-top*))
  "Fresh view of BUFFER, opened at its first line.  A :CLASSIC view not told WRAP-P lays its lines out unwrapped; every other family wraps unless told otherwise, as it always has."
  (make-instance 'text-view :buffer buffer
                            :wrap-p (if (or wrap-p-supplied-p (not (eq gesture-policy :classic)))
                                        wrap-p
                                        nil)
                            :font font :font-size font-size :line-pitch line-pitch
                            :bg-color bg-color :fg-color fg-color
                            :gesture-policy gesture-policy
                            :keymap keymap :single-line-p single-line-p
                            :after-change after-change :unhandled-key unhandled-key
                            :accept accept :message message
                            :max-characters max-characters
                            :select-all-on-focus-p select-all-on-focus-p
                            :completions completions :history-size history-size
                            :mode mode
                            :gutter-p gutter-p :gutter-digits gutter-digits
                            :gutter-bg gutter-bg :gutter-fg gutter-fg
                            :gutter-rule gutter-rule
                            :wrap-marker-p wrap-marker-p
                            :wrap-marker-color wrap-marker-color
                            :margin-left margin-left :margin-top margin-top))

(defmethod print-object ((view text-view) stream)
  (print-unreadable-object (view stream :type t :identity t)
    (format stream "line ~D row ~D, ~D visible"
            (text-view-top-line view) (text-view-top-row view)
            (length (text-view-visible-lines view)))))

;;; ------------------------------------------------------------------ the mode

(defgeneric row-markup (mode view line row)
  (:documentation "Disjoint ascending (START END STYLE) runs, in characters of the row, that ROW of LINE is drawn in under MODE.")
  (:method (mode view line row) (declare (ignore mode view line row)) nil))

(defgeneric row-markup-key (mode view line row)
  (:documentation "What those runs are decided by, compared with EQUAL.")
  (:method (mode view line row) (declare (ignore mode view line row)) nil))

(defgeneric mode-place (mode view)
  (:documentation "Put what MODE draws of its own where it belongs, once per layout pass.")
  (:method (mode view) (declare (ignore mode view)) nil))

(defgeneric mode-invalidate (mode view first delta)
  (:documentation "The lines of VIEW from FIRST changed and the ones under them moved by DELTA, or nothing from FIRST down is known when DELTA is NIL.")
  (:method (mode view first delta) (declare (ignore mode view first delta)) nil))

(defgeneric mode-run-at (mode view index)
  (:documentation "(values LO HI) of what a multiple click takes around INDEX under MODE, or NIL to take the run the family takes.")
  (:method (mode view index) (declare (ignore mode view index)) nil))

(defgeneric mode-keymap (mode)
  (:documentation "Key bindings MODE puts in front of the view's own table.")
  (:method (mode) (declare (ignore mode)) nil))

(defparameter *stale-style*
  '(:stale)
  "A style key no row can ever answer, put in the pool when the READING changed.")

(defun (setf text-view-mode) (mode view)
  "Read the text as MODE from now on, or as plain text when MODE is NIL."
  (setf (slot-value view 'mode) mode
        (slot-value view 'mode-state) nil)
  (fill (slot-value view 'pool-styles) *stale-style*)
  (%view-relayout view)
  mode)

(defun %override-runs (runs lo hi style)
  "RUNS with [LO, HI) taken to STYLE."
  (let ((out '()))
    (dolist (run runs)
      (destructuring-bind (start end run-style) run
        (when (< start (min end lo)) (push (list start (min end lo) run-style) out))
        (when (< (max start hi) end) (push (list (max start hi) end run-style) out))))
    (push (list lo hi style) out)
    (sort (nreverse out) #'< :key #'first)))

(defun %row-bounds (view line row)
  "(values OPEN CLOSE) document indices of the characters ROW of LINE shows."
  (let* ((layout (slot-value view 'layout))
         (line-layout (layout-line layout line))
         (base (buffer-line-start (layout-buffer layout) line)))
    (values (+ base (line-layout-row-start line-layout row))
            (+ base (line-layout-row-end line-layout row)))))

(defun %row-marking (view open close)
  "(values LO HI) of the marked characters of the row [OPEN, CLOSE), in characters of that row, or NIL when the marking does not reach it."
  (let ((anchor (text-view-anchor view))
        (caret (text-view-caret view)))
    (when (and anchor caret (/= anchor caret) (text-view-marking-inked-p view))
      (let ((lo (max open (min anchor caret)))
            (hi (min close (max anchor caret))))
        (when (< lo hi) (values (- lo open) (- hi open)))))))

(defun %row-style-key (view line row)
  "What ROW of LINE is drawn in, as the cheap answer the view compares."
  (multiple-value-bind (open close) (%row-bounds view line row)
    (multiple-value-bind (lo hi) (%row-marking view open close)
      (let ((key (row-markup-key (slot-value view 'mode) view line row)))
        (when (or key lo) (list key lo hi))))))

(defun %row-style (view line row)
  "The runs ROW of LINE is drawn in."
  (multiple-value-bind (open close) (%row-bounds view line row)
    (let ((runs (row-markup (slot-value view 'mode) view line row)))
      (multiple-value-bind (lo hi) (%row-marking view open close)
        (if lo
            (%override-runs runs lo hi (list :color *selection-foreground*))
            runs)))))

;;; ------------------------------------------------------------------ measuring

(defun %view-face (view)
  "The face VIEW measures and draws with."
  (or (slot-value view 'font)
      (org.shirakumo.alloy.renderers.simple:request-font
       (slot-value view 'renderer) :default)))

(defun text-view-wrap-marker-p (view)
  "True while a row a line was broken at is closed by a glyph."
  (let ((value (slot-value view 'marker-p)))
    (if (eq value :policy)
        (eq (text-view-gesture-policy view) :editor)
        (and value t))))

(defun %gutter-sample (view)
  "The string the fixed width of the column is measured with, kept so a face is never measured for it twice."
  (let ((digits (slot-value view 'gutter-digits))
        (sample (slot-value view 'gutter-sample)))
    (if (and sample (= (length sample) digits))
        sample
        (setf (slot-value view 'gutter-sample)
              (make-string digits :initial-element *gutter-sample-digit*)))))

(defun %view-text-x (view)
  "Device pixel the first character of every row opens at."
  (slot-value view 'text-x))

(defun %view-text-top (view)
  "Device pixel the TOP of the first visible row sits at."
  (+ (- (org.shirakumo.alloy:pxh (org.shirakumo.alloy:bounds view))
        (slot-value view 'text-inset))
     (slot-value view 'scroll-y)))

(defun %view-text-width (view)
  "Device pixels a row has to itself."
  (max 1.0 (- (org.shirakumo.alloy:pxw (org.shirakumo.alloy:bounds view))
              (%view-text-x view)
              (slot-value view 'marker-width))))

(defun %italic-lean (data)
  "Device pixels the toolkit draws a slanted glyph to the RIGHT of where its pen put it (alloy renderers/opengl/msdf.lisp."
  (/ (float (3b-bmfont:base data) 0f0) 16.0))

(defun %view-sync-frame (view font scale)
  "Fold the column of line numbers, the margins and the marker band into the device-pixel frame the rows are laid out in, and answer the width left for a row."
  (let ((data (org.shirakumo.alloy.renderers.opengl.msdf::data font))
        (digits (slot-value view 'gutter-digits)))
    (setf (slot-value view 'gutter-band-width)
          (if (text-view-gutter-p view)
              (pen-advance data (%gutter-sample view) 0 digits scale)
              0.0)
          (slot-value view 'gutter-digits-width)
          (if (text-view-gutter-p view)
              (+ (slot-value view 'gutter-band-width) (%italic-lean data))
              0.0)
          (slot-value view 'gutter-width)
          (if (text-view-gutter-p view)
              ;; a whole number of pixels: the rule closing the column is one pixel
              ;; wide, and a column edge on half a pixel is two blurred ones
              (float (ceiling (+ (slot-value view 'gutter-digits-width)
                                 (org.shirakumo.alloy:to-px *gutter-right-gap*)
                                 *gutter-rule-width*))
                     0f0)
              0.0)
          (slot-value view 'marker-width)
          (if (text-view-wrap-marker-p view)
              (+ (pen-advance data *wrap-marker* 0 1 scale)
                 (float (org.shirakumo.alloy:to-px *wrap-marker-gap*) 0f0))
              0.0)
          (slot-value view 'text-inset)
          (float (org.shirakumo.alloy:to-px (slot-value view 'margin-top)) 0f0)
          (slot-value view 'text-x)
          (+ (slot-value view 'gutter-width)
             (float (org.shirakumo.alloy:to-px (slot-value view 'margin-left)) 0f0))))
  (%view-text-width view))

(defun %view-build-context (view)
  "The measurement context of VIEW, built the way a text shape is handed its own."
  (let ((font (%view-face view)))
    ;; a face answers its fallback chain and its metrics only once allocated,
    ;; which is what a text shape does for itself before it lays anything out
    (org.shirakumo.alloy:allocate font)
    (org.shirakumo.alloy:with-unit-parent view
      (let* ((scale (float (org.shirakumo.alloy:to-px
                            (or (slot-value view 'font-size) *default-font-size*))
                           0f0))
             (width (%view-sync-frame view font scale)))
        (make-measure-context font scale
                              :wrap-width (when (slot-value view 'wrap-p) width)
                              :halign :start)))))

(defun %view-sync-context (view)
  "Re-measure from a context that matches the current width and font, and hand the layout over to it."
  (let ((new (%view-build-context view))
        (old (slot-value view 'context)))
    (cond ((and old (%same-context-p new old) (slot-value view 'layout))
           nil)
          (t
           (setf (slot-value view 'context) new
                 (slot-value view 'pitch)
                 (float (or (slot-value view 'line-pitch)
                            (font-line-height (context-font new) (context-scale new)))
                        0f0))
           (if (slot-value view 'layout)
               (setf (layout-context (slot-value view 'layout)) new)
               (when (slot-value view 'buffer)
                 (setf (slot-value view 'layout)
                       (make-layout (slot-value view 'buffer) new))
                 (setf (layout-observer (slot-value view 'layout))
                       (lambda (first delta)
                         (mode-invalidate (slot-value view 'mode) view first delta)))))
           t))))

;;; ------------------------------------------------------------------ the shapes

(defun %row-text (view line row)
  "The characters ROW of logical LINE shows, and only those."
  (let* ((layout (slot-value view 'layout))
         (buffer (layout-buffer layout))
         (line-layout (layout-line layout line))
         (base (buffer-line-start buffer line)))
    (buffer-substring buffer
                      (+ base (line-layout-row-start line-layout row))
                      (+ base (line-layout-row-end line-layout row)))))

(defun %view-pattern (color fallback)
  "Drawing pattern for COLOR, or for FALLBACK when the host named no colour."
  (or color fallback))

(defun %row-shape-width (view)
  (let ((context (slot-value view 'context)))
    (or (context-wrap-width context) *unwrapped-width*)))

(defun %make-row-shape (view)
  "One visual row's shape."
  (let ((context (slot-value view 'context)))
    (org.shirakumo.alloy.renderers.simple:text (slot-value view 'renderer)
                 (org.shirakumo.alloy:px-extent 0 0 (%row-shape-width view) 0)
                 ""
                 :font (context-font context)
                 :size (org.shirakumo.alloy:px (context-scale context))
                 :wrap nil
                 :halign (context-halign context)
                 :valign :top
                 :pattern (%view-pattern (slot-value view 'fg-color) *default-foreground*)
                 :z-index 2)))

(defun %view-ensure-pool (view n)
  "Have at least N row shapes."
  (let ((pool (slot-value view 'pool))
        (keys (slot-value view 'pool-keys))
        (styles (slot-value view 'pool-styles)))
    (loop while (< (length pool) n)
          do (let ((shape (%make-row-shape view)))
               (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape (length pool) view) shape)
               (vector-push-extend shape pool)
               (vector-push-extend nil keys)
               (vector-push-extend nil styles)))))

(defun %row-key-p (key line-layout row)
  (and (consp key) (eq (car key) line-layout) (eql (cdr key) row)))

(defun %assign-shapes (wanted keys test)
  "Which pooled shape shows each of the WANTED things, KEYS holding what each shape of the pool shows NOW and TEST answering whether a key still shows a wanted thing."
  (let* ((n (length wanted))
         (order (make-array n :initial-element 0))
         (placed (make-array n :initial-element nil))
         (taken (make-array (length keys) :initial-element nil)))
    ;; ponytail: the pool is scanned once per wanted thing, which is the square of the viewport in rows and is a few hundred comparisons.
    (dotimes (i n)
      (dotimes (j (length keys))
        (when (and (not (aref taken j)) (funcall test (aref keys j) (aref wanted i)))
          (setf (aref order i) j (aref placed i) t (aref taken j) t)
          (return))))
    (let ((free 0))
      (dotimes (i n)
        (unless (aref placed i)
          (loop while (and (< free (length taken)) (aref taken free)) do (incf free))
          (setf (aref order i) free)
          (when (< free (length taken)) (setf (aref taken free) t))
          (incf free))))
    order))

(defun %view-assign-shapes (view rows layout)
  "Which pooled shape shows each visible row, ROWS holding one (LINE ROW TOP) per row from the top down."
  (%assign-shapes rows (slot-value view 'pool-keys)
                  (lambda (key cell)
                    (%row-key-p key (layout-line layout (first cell)) (second cell)))))

;;; ------------------------------------------------------------------ the bands

(defstruct (band (:constructor %make-band (name ink markup z-index))
                 (:conc-name band-)
                 (:copier nil)
                 (:predicate nil))
  "One column of one-row texts beside the rows."
  name ink markup z-index
  (shapes (make-array 0 :adjustable t :fill-pointer t))
  (keys (make-array 0 :adjustable t :fill-pointer t)))

(defun %band-shape-id (band index)
  (intern (format nil "~A-~D" (band-name band) index) :keyword))

(defun %make-band-shape (view band width)
  "One text of BAND, as wide as its column and no higher than its origin, aligned to the RIGHT of that column."
  (let ((context (slot-value view 'context)))
    (org.shirakumo.alloy.renderers.simple:text
     (slot-value view 'renderer)
     (org.shirakumo.alloy:px-extent 0 0 (max 1.0 width) 0)
     ""
     :font (context-font context)
     :size (org.shirakumo.alloy:px (context-scale context))
     :wrap nil
     :halign :end
     :valign :top
     :markup (band-markup band)
     :pattern (funcall (band-ink band) view)
     :z-index (band-z-index band))))

(defun %view-place-band (view band rows x width text-of fresh)
  "Put one shape of BAND over every visible row, at X and as wide as WIDTH, showing what TEXT-OF answers for that row and nothing where it answers NIL."
  (let* ((shapes (band-shapes band))
         (keys (band-keys band))
         (n (length rows))
         (texts (make-array n)))
    (dotimes (i n)
      (setf (aref texts i) (funcall text-of (first (aref rows i)) (second (aref rows i)))))
    (loop while (< (length shapes) n)
          do (let ((shape (%make-band-shape view band width)))
               (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape
                      (%band-shape-id band (length shapes)) view)
                     shape)
               (vector-push-extend shape shapes)
               (vector-push-extend nil keys)))
    (when fresh (fill keys nil))
    (dotimes (j (length shapes))
      (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p (aref shapes j)) t))
    (let ((order (%assign-shapes texts keys
                                 (lambda (key wanted) (and key (equal key wanted)))))
          (context (slot-value view 'context)))
      (dotimes (i n)
        (let ((text (aref texts i))
              (shape (aref shapes (aref order i))))
          (when text
            (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) nil)
            (unless (equal text (aref keys (aref order i)))
              (reinitialize-instance shape
                                     :text text
                                     :bounds (org.shirakumo.alloy:px-extent
                                              0 0 (max 1.0 width) 0)
                                     :font (context-font context)
                                     :size (org.shirakumo.alloy:px (context-scale context))
                                     :wrap nil)
              (setf (aref keys (aref order i)) text))
            (setf (org.shirakumo.alloy.renderers.simple.presentations:offset shape)
                  (org.shirakumo.alloy:px-point x (third (aref rows i))))))))))

(defun %view-gutter-band (view)
  (or (slot-value view 'gutter-band)
      (setf (slot-value view 'gutter-band)
            (%make-band "GUTTER"
                        (lambda (view)
                          (%view-pattern (or (slot-value view 'gutter-fg)
                                             (slot-value view 'fg-color))
                                         *default-foreground*))
                        (%italic-markup)
                        6))))

(defun %view-marker-band (view)
  (or (slot-value view 'marker-band)
      (setf (slot-value view 'marker-band)
            (%make-band "WRAP-MARKER"
                        (lambda (view)
                          (%view-pattern (or (slot-value view 'marker-color)
                                             (slot-value view 'fg-color))
                                         *default-foreground*))
                        nil
                        2))))

(defun %make-gutter-shape (view pattern z-index)
  "One filled rectangle of the column of line numbers."
  (org.shirakumo.alloy.renderers.simple:rectangle
   (slot-value view 'renderer)
   (org.shirakumo.alloy:px-extent 0 0 0 0)
   :pattern pattern :z-index z-index))

(defun %view-place-gutter-chrome (view)
  "Put the ground of the column of line numbers and the rule closing it over the whole height of the view, and hide both while there is no column."
  (flet ((chrome (slot id pattern z-index)
           (let ((shape (or (slot-value view slot)
                            (let ((new (%make-gutter-shape view pattern z-index)))
                              (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape
                                     id view)
                                    new)
                              (setf (slot-value view slot) new)))))
             shape)))
    (let* ((width (slot-value view 'gutter-width))
           (height (org.shirakumo.alloy:pxh (org.shirakumo.alloy:bounds view)))
           (ground (chrome 'gutter-ground :gutter-ground
                           (or (slot-value view 'gutter-bg) *default-gutter-background*)
                           4))
           (rule (chrome 'gutter-rule-shape :gutter-rule
                         (or (slot-value view 'gutter-rule) *default-gutter-rule*)
                         5)))
      (cond ((text-view-gutter-p view)
             (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p ground) nil
                   (org.shirakumo.alloy.renderers.simple.presentations:hidden-p rule) nil)
             (reinitialize-instance
              ground
              :bounds (org.shirakumo.alloy:px-extent 0 0 width height)
              :pattern (or (slot-value view 'gutter-bg) *default-gutter-background*))
             (reinitialize-instance
              rule
              :bounds (org.shirakumo.alloy:px-extent
                       (- width *gutter-rule-width*) 0 *gutter-rule-width* height)
              :pattern (or (slot-value view 'gutter-rule) *default-gutter-rule*)))
            (t
             (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p ground) t
                   (org.shirakumo.alloy.renderers.simple.presentations:hidden-p rule) t))))))

(defun %view-row-number (view line row)
  "What the column shows on ROW of LINE."
  (declare (ignore view))
  (if (zerop row)
      (princ-to-string (1+ line))
      *continuation-number*))

(defun %view-row-marker (view line row)
  "The glyph closing ROW of LINE, or NIL on a row the line was not broken at."
  (let ((line-layout (layout-line (slot-value view 'layout) line)))
    (when (< (1+ row) (line-layout-row-count line-layout))
      *wrap-marker*)))

(defun %view-place-frame (view rows fresh)
  "Put what stands BESIDE the rows where it belongs."
  (%view-place-gutter-chrome view)
  ;; a band that never opened costs nothing; one the host turned off keeps being
  ;; walked, which is what holds the shapes it made down
  (flet ((place (band on-p x width text-of)
           (when (or on-p (plusp (length (band-shapes band))))
             (%view-place-band view band rows x width
                               (if on-p text-of (constantly nil))
                               fresh))))
    (place (%view-gutter-band view) (text-view-gutter-p view)
           0.0 (max 1.0 (slot-value view 'gutter-band-width))
           (lambda (line row) (%view-row-number view line row)))
    (place (%view-marker-band view) (text-view-wrap-marker-p view)
           (+ (%view-text-x view) (%view-text-width view))
           (max 1.0 (slot-value view 'marker-width))
           (lambda (line row) (%view-row-marker view line row)))))

;;; ------------------------------------------------------------------ the state

(defun %view-clamp-top (view)
  (let* ((layout (slot-value view 'layout))
         (count (buffer-line-count (layout-buffer layout)))
         (line (max 0 (min (slot-value view 'top-line) (1- count))))
         (rows (line-layout-row-count (layout-line layout line))))
    (setf (slot-value view 'top-line) line
          (slot-value view 'top-row) (max 0 (min (slot-value view 'top-row) (1- rows))))))

(defun %view-choose-visible (view)
  "The lines the viewport shows, each with the rows of it that fall above the top."
  (let* ((layout (slot-value view 'layout))
         (count (buffer-line-count (layout-buffer layout)))
         (pitch (max 1.0 (slot-value view 'pitch)))
         (wanted (+ 1 (ceiling (%view-text-top view) pitch)))
         (visible (make-array 0 :adjustable t :fill-pointer t))
         (rows 0))
    (loop for line from (slot-value view 'top-line) below count
          while (< rows wanted)
          for skip = (slot-value view 'top-row) then 0
          do (let ((n (line-layout-row-count (layout-line layout line))))
               (vector-push-extend (cons line skip) visible)
               (incf rows (- n skip))))
    (setf (slot-value view 'visible) visible)))

(defun %map-visible-rows (view function)
  "Call FUNCTION with the visible ROW index counting from the top of the viewport, the logical line that row belongs to, the row of that line, and the device-pixel y its TOP sits at."
  (let* ((layout (slot-value view 'layout))
         (pitch (slot-value view 'pitch))
         (visible (slot-value view 'visible))
         (top (%view-text-top view))
         (i 0))
    (dotimes (k (length visible))
      (let* ((cell (aref visible k))
             (line (car cell))
             (skip (cdr cell))
             (rows (line-layout-row-count (layout-line layout line))))
        (loop for row from skip below rows
              do (funcall function i line row (- top (* pitch i)))
                 (incf i))))))

(defun %view-visible-rows (view)
  "One (LINE ROW TOP) per visible row, topmost first."
  (let ((out (make-array 0 :adjustable t :fill-pointer t)))
    (%map-visible-rows view (lambda (i line row top)
                              (declare (ignore i))
                              (vector-push-extend (list line row top) out)))
    out))

(defun %view-place-shapes (view &optional fresh-context)
  "Put each visible row's shape where that row belongs, laying out again only the shapes whose row or document changed."
  (let* ((layout (slot-value view 'layout))
         (rows (%view-visible-rows view))
         (pool (slot-value view 'pool))
         (keys (slot-value view 'pool-keys))
         (styles (slot-value view 'pool-styles))
         (x (- (%view-text-x view) (slot-value view 'x-offset))))
    (%view-place-frame view rows fresh-context)
    (%view-ensure-pool view (length rows))
    (let ((order (%view-assign-shapes view rows layout)))
      (setf (slot-value view 'order) order)
      (dotimes (j (length pool))
        (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p (aref pool j)) t))
      (dotimes (i (length rows))
        (destructuring-bind (line row top) (aref rows i)
          (let* ((j (aref order i))
                 (shape (aref pool j))
                 (line-layout (layout-line layout line))
                 (stale (not (%row-key-p (aref keys j) line-layout row)))
                 (style-key (%row-style-key view line row))
                 (restyle (not (equal style-key (aref styles j)))))
            (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) nil)
            (when (or stale restyle)
              ;; the runs are asked for only here: a row that kept its key is drawn
              ;; in what it was already drawn in, and nothing reads the text for it
              (let ((markup (when style-key (%row-style view line row))))
                (cond (fresh-context
                       (let ((context (slot-value view 'context)))
                         (reinitialize-instance
                          shape
                          :bounds (org.shirakumo.alloy:px-extent 0 0 (%row-shape-width view) 0)
                          :font (context-font context)
                          :size (org.shirakumo.alloy:px (context-scale context))
                          :wrap nil
                          :text (%row-text view line row)
                          :markup markup)))
                      (stale
                       (reinitialize-instance shape :text (%row-text view line row)
                                                    :markup markup))
                      (t
                       (reinitialize-instance shape :markup markup))))
              (setf (aref keys j) (cons line-layout row)
                    (aref styles j) style-key))
            (setf (org.shirakumo.alloy.renderers.simple.presentations:offset shape)
                  (org.shirakumo.alloy:px-point x top))))))))

;;; ------------------------------------------------------- caret and selection

(defun %view-caret-width (view)
  "Device-pixel width of the caret bar, the one a text input of the toolkit draws (alloy renderers/opengl/msdf.lisp."
  (let ((context (slot-value view 'context)))
    (* 4.0 (org.shirakumo.alloy.renderers.opengl.msdf::msdf-font-scale
            (context-font context) (context-scale context)))))

(defparameter *editor-caret-row-share* (/ 20.0 21.0)
  "Share of the row an :EDITOR caret spans: it stops one pixel short of the top.")

(defparameter *empty-row-selection-share* (/ 3.0 21.0)
  "Share of a row's height an :EDITOR paints WIDE for a selected row that holds no character.")

(defun %view-caret-height (view)
  "Device-pixel height of the caret bar."
  (let ((pitch (slot-value view 'pitch)))
    (if (eq (text-view-gesture-policy view) :field)
        pitch
        (* pitch *editor-caret-row-share*))))

(defun text-view-caret-location (view index)
  "(values X Y W H) of the caret at document INDEX, in device pixels of VIEW's own box, or NIL when the line holding INDEX is not one of the visible ones."
  (let ((layout (slot-value view 'layout)))
    (when layout
      (multiple-value-bind (line row x) (find-location layout index)
        (let ((pitch (slot-value view 'pitch))
              (height (%view-caret-height view))
              (width (%view-caret-width view)))
          (block found
            (%map-visible-rows
             view
             (lambda (i cell-line cell-row top)
               (declare (ignore i))
               (when (and (eql cell-line line) (eql cell-row row))
                 (return-from found
                   (values (+ (%view-text-x view) (- x (slot-value view 'x-offset)))
                           (- top pitch)
                           width height)))))
            nil))))))

(defun %view-hit-row (view y)
  "(values LINE ROW OUTSIDE) of the visual row of VIEW at device-pixel Y, OUTSIDE :ABOVE or :BELOW when Y falls past every row the viewport holds."
  (let ((pitch (max 1.0 (slot-value view 'pitch)))
        (hit nil)
        (last nil)
        (ceiling nil))
    (%map-visible-rows
     view
     (lambda (i line row top)
       (when (zerop i) (setf ceiling top))
       (setf last (list line row))
       (unless hit
         (when (> y (- top pitch)) (setf hit (list line row))))))
    (values (first (or hit last)) (second (or hit last))
            (cond ((null hit) :below)
                  ((and ceiling (> y ceiling)) :above)))))

(defun %view-last-line-visible-p (view)
  (let ((visible (slot-value view 'visible)))
    (and (plusp (length visible))
         (eql (car (aref visible (1- (length visible))))
              (1- (buffer-line-count (layout-buffer (slot-value view 'layout))))))))

(defun %view-first-line-visible-p (view)
  (and (zerop (slot-value view 'top-line)) (zerop (slot-value view 'top-row))))

(defun text-view-index-at (view x y)
  "Document index at the point (X Y) in device pixels of VIEW's own box."
  (let ((layout (slot-value view 'layout))
        (policy (text-view-gesture-policy view)))
    (multiple-value-bind (line row outside) (%view-hit-row view y)
      (cond ((and (text-view-gutter-p view) (< x (slot-value view 'gutter-width)))
             (buffer-line-start (layout-buffer layout) line))
            (t
             (or (and (eq policy :editor)
                      (case outside
                        (:below (and (%view-last-line-visible-p view)
                                     (buffer-size (layout-buffer layout))))
                        (:above (and (%view-first-line-visible-p view) 0))))
                 (find-index layout line row
                             (+ (- x (%view-text-x view))
                                (slot-value view 'x-offset)))))))))

(defun text-view-selection-rects (view start end)
  "(X Y W H) in device pixels of VIEW's own box for every VISIBLE visual row the document range [START, END) covers, topmost first."
  (let ((layout (slot-value view 'layout))
        (lo (min start end))
        (hi (max start end))
        (pitch (slot-value view 'pitch))
        (dx (- (slot-value view 'x-offset) (%view-text-x view)))
        (sliver (and (eq (text-view-gesture-policy view) :editor)
                     (* (slot-value view 'pitch) *empty-row-selection-share*)))
        (out '()))
    (when (and layout (< lo hi))
      (let ((buffer (layout-buffer layout)))
        (%map-visible-rows
         view
         (lambda (i line row top)
           (declare (ignore i))
           (let* ((line-layout (layout-line layout line))
                  (base (buffer-line-start buffer line))
                  (open (+ base (line-layout-row-start line-layout row)))
                  (close (+ base (line-layout-row-end line-layout row)))
                  (from (max lo open))
                  (to (min hi close))
                  (y (- top pitch)))
             (cond ((< from to)
                    (let ((x1 (if (= from open)
                                  0.0
                                  (nth-value 2 (find-location layout from))))
                          (x2 (if (= to close)
                                  (line-layout-row-width line-layout row)
                                  (nth-value 2 (find-location layout to)))))
                      (push (list (- x1 dx) y (max 0.0 (- x2 x1)) pitch) out)))
                   ((and sliver (= open close) (<= lo open) (< open hi))
                    (push (list (- 0.0 dx) y sliver pitch) out))))))))
    (nreverse out)))

(defun %view-say (view kind string)
  "Tell the host something in one line, and nothing at all when it left no hook."
  (let ((hook (text-view-message view)))
    (when hook (funcall hook view kind string))))

(defun text-view-caret (view)
  "Document index the caret sits at, or NIL when the view carries none yet."
  (let ((mark (slot-value view 'caret)))
    (when mark (mark-index mark))))

(defun text-view-anchor (view)
  "Document index the selection is held from, or NIL when nothing is selected."
  (let ((mark (slot-value view 'anchor)))
    (when mark (mark-index mark))))

;;; ------------------------------------------------- the stretch that is not theirs

(defun text-view-input-start (view)
  "Document index the person's own text opens at, or NIL while the whole document is theirs to change."
  (let ((mark (slot-value view 'input-start)))
    (when mark (mark-index mark))))

(defun %view-input-floor (view)
  "The first index the person may change: the mark, or the start of the document."
  (let ((mark (slot-value view 'input-start)))
    (if mark (mark-index mark) 0)))

(defun %check-user-range (view start end)
  "Refuse an edit of the person's that would reach text they may not change."
  (let ((mark (slot-value view 'input-start)))
    (when (and mark (< (min start end) (mark-index mark)))
      (error 'editor-error :text "That text cannot be changed."))))

(defun %view-drop-marks (view)
  (dolist (slot '(caret anchor input-start))
    (let ((mark (slot-value view slot)))
      (when mark (delete-mark mark) (setf (slot-value view slot) nil))))
  (bordeaux-threads:with-lock-held ((slot-value view 'pending-lock))
    (setf (slot-value view 'pending) nil))
  (setf (fill-pointer (slot-value view 'history)) 0)
  (setf (slot-value view 'input-prompt) ""
        (slot-value view 'completion) nil
        (slot-value view 'history-at) nil
        (slot-value view 'history-draft) nil
        (slot-value view 'press) nil
        (slot-value view 'dragging) nil
        (slot-value view 'word) nil
        (slot-value view 'click) nil
        (slot-value view 'goal-column) nil
        (slot-value view 'last-command) nil
        (slot-value view 'scroll-at) nil
        (slot-value view 'scroll-rate) nil
        (slot-value view 'scroll-residue) 0.0))

(defun %view-forget-command-state (view)
  "Forget what the last command left standing."
  (setf (slot-value view 'goal-column) nil
        (slot-value view 'last-command) nil))

(defun %view-move-mark (view slot index)
  "Put VIEW's mark in SLOT at document INDEX, making it if it is not there yet and forgetting it when INDEX is NIL."
  (let ((mark (slot-value view slot)))
    (cond ((null index)
           (when mark (delete-mark mark) (setf (slot-value view slot) nil)))
          (mark (setf (mark-index mark) index))
          (t (setf (slot-value view slot)
                   (make-mark (slot-value view 'buffer) index))))))

(defun %make-mark-shape (view pattern z-index)
  "One filled rectangle of the caret or of the selection."
  (org.shirakumo.alloy.renderers.simple:rectangle
   (slot-value view 'renderer)
   (org.shirakumo.alloy:px-extent 0 0 0 0)
   :pattern pattern :palette view :z-index z-index))

(defun %view-ensure-caret-shape (view)
  (or (slot-value view 'caret-shape)
      (let ((shape (%make-mark-shape view :input-foreground 3)))
        (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape :caret view) shape)
        (setf (slot-value view 'caret-shape) shape))))

(defun %view-ensure-selection-pool (view n)
  "Have at least N selection rectangles."
  (let ((pool (slot-value view 'sel-pool)))
    (loop while (< (length pool) n)
          do (let ((shape (%make-mark-shape view :notice-background 0)))
               (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape
                      (- -1 (length pool)) view)
                     shape)
               (vector-push-extend shape pool)))
    pool))

(defgeneric text-view-caret-shown-p (view)
  (:documentation "True while VIEW draws its caret; a view draws it while it holds the focus.")
  (:method ((view text-view))
    (and (org.shirakumo.alloy:focus view) t)))

(defgeneric text-view-marking-shown-p (view)
  (:documentation "True while VIEW draws the rectangles behind what is marked; a view draws them while it holds the focus, an :EDITOR view always.")
  (:method ((view text-view))
    (or (and (org.shirakumo.alloy:focus view) t)
        (eq (text-view-gesture-policy view) :editor))))

(defgeneric text-view-marking-inked-p (view)
  (:documentation "True while VIEW draws the characters it marks in the ink of a marking; a view always does.")
  (:method ((view text-view))
    t))

(defun %view-place-caret (view)
  "Put the caret's own shape where its index falls, hidden while that index is off the visible rows or the view does not show its caret."
  (let ((shape (%view-ensure-caret-shape view))
        (mark (slot-value view 'caret)))
    (multiple-value-bind (x y w h)
        (when (and mark (text-view-caret-shown-p view))
          (text-view-caret-location view (mark-index mark)))
      (cond (x
             (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) nil)
             (reinitialize-instance shape :bounds (org.shirakumo.alloy:px-extent x y w h)))
            (t
             (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) t))))))

(defun text-view-refresh-caret (view)
  "Show or hide VIEW's caret as TEXT-VIEW-CARET-SHOWN-P answers now, laying nothing else out."
  (when (slot-value view 'renderer)
    (%view-place-caret view)
    (org.shirakumo.alloy:mark-for-render view)))

(defun text-view-refresh (view)
  "Lay VIEW out again, so the caret and the marking follow what the methods deciding them answer now."
  (%view-relayout view))

(defun %view-place-selection (view)
  "Put one rectangle behind every visible row the selection covers, and hide the rest of the pool."
  (let* ((anchor (slot-value view 'anchor))
         (caret (slot-value view 'caret))
         (rects (when (and anchor caret (text-view-marking-shown-p view))
                  (text-view-selection-rects view (mark-index anchor) (mark-index caret))))
         (pool (%view-ensure-selection-pool view (length rects))))
    (dotimes (j (length pool))
      (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p (aref pool j)) t))
    (loop for rect in rects
          for j from 0
          do (destructuring-bind (x y w h) rect
               (let ((shape (aref pool j)))
                 (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) nil)
                 (reinitialize-instance shape
                                        :bounds (org.shirakumo.alloy:px-extent x y w h)))))))

;;; ------------------------------------------------------- the list of candidates

(defstruct (candidate-list (:constructor %make-candidate-list (start all candidates))
                           (:conc-name candidates-)
                           (:copier nil)
                           (:predicate nil))
  "START the index the prefix opens at, ALL the names the host answered, CANDIDATES what is left of them, INDEX the one being stood on, FIRST the one at the top of what is shown and BOX where the whole of it was last put."
  start all candidates (index 0) (first 0) (box nil))

(defun text-view-completing-p (view)
  "True while a list of candidates stands open over the rows."
  (and (slot-value view 'completion) t))

(defun text-view-completion-candidates (view)
  "The names that list holds, or NIL when none stands open."
  (let ((state (slot-value view 'completion)))
    (when state (candidates-candidates state))))

(defun text-view-completion-index (view)
  "Which of those names is being stood on, or NIL when none stands open."
  (let ((state (slot-value view 'completion)))
    (when state (candidates-index state))))

(defun text-view-completion-bounds (view)
  "(values X Y W H) in device pixels of VIEW's own box for the open list, or NIL when none stands open or it has not been put anywhere yet."
  (let* ((state (slot-value view 'completion))
         (box (and state (candidates-box state))))
    (when box (values-list box))))

(defun %view-completion-keymap (view)
  "The table the open list reads its keys through, or NIL while none stands open."
  (when (slot-value view 'completion) *completion-keymap*))

(defun %view-completion-prefix (view state)
  "The characters between where the prefix opens and the caret."
  (let ((caret (or (text-view-caret view) 0))
        (start (candidates-start state)))
    (if (<= start caret)
        (buffer-substring (text-view-buffer view) start caret)
        "")))

(defun %carries-on-p (prefix name)
  (and (<= (length prefix) (length name))
       (string= prefix name :end2 (length prefix))))

(defun %make-completion-shape (view pattern z-index)
  (org.shirakumo.alloy.renderers.simple:rectangle
   (slot-value view 'renderer)
   (org.shirakumo.alloy:px-extent 0 0 0 0)
   :pattern pattern :palette view :z-index z-index))

(defun %make-completion-text (view)
  (let ((context (slot-value view 'context)))
    (org.shirakumo.alloy.renderers.simple:text
     (slot-value view 'renderer) (org.shirakumo.alloy:px-extent 0 0 1.0 0) ""
     :font (context-font context)
     :size (org.shirakumo.alloy:px (context-scale context))
     :wrap nil :halign :start :valign :top
     :pattern (%view-pattern (slot-value view 'fg-color) *default-foreground*)
     :z-index 8)))

(defun %view-completion-shapes (view n)
  "Have the ground, the band behind the name being stood on, and N texts."
  (flet ((chrome (slot id pattern z-index)
           (or (slot-value view slot)
               (let ((shape (%make-completion-shape view pattern z-index)))
                 (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape
                        id view)
                       shape)
                 (setf (slot-value view slot) shape)))))
    (chrome 'completion-ground :completion-ground *completion-background* 6)
    (chrome 'completion-choice :completion-choice *completion-choice-background* 7))
  (let ((pool (slot-value view 'completion-pool)))
    (loop while (< (length pool) n)
          do (let ((shape (%make-completion-text view)))
               (setf (org.shirakumo.alloy.renderers.simple.presentations:find-shape
                      (intern (format nil "COMPLETION-~D" (length pool)) :keyword) view)
                     shape)
               (vector-push-extend shape pool)))
    pool))

(defun %view-hide-completion (view)
  "Hide whatever a list left drawn; a view that never opened one has nothing to hide."
  (dolist (slot '(completion-ground completion-choice))
    (let ((shape (slot-value view slot)))
      (when shape
        (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) t))))
  (let ((pool (slot-value view 'completion-pool)))
    (dotimes (j (length pool))
      (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p (aref pool j))
            t))))

(defun %view-string-width (view string)
  "Device pixels the pen travels over STRING in the face the view draws in."
  (let ((context (slot-value view 'context)))
    (pen-advance (org.shirakumo.alloy.renderers.opengl.msdf::data (context-font context))
                 string 0 (length string) (context-scale context))))

(defun %view-completion-box (view state rows)
  "(X Y W H) the list takes."
  (multiple-value-bind (cx cy cw ch)
      (text-view-caret-location view (or (text-view-caret view) 0))
    (declare (ignore cw))
    (when cx
      (let* ((pad *completion-padding*)
             (pitch (max 1.0 (slot-value view 'pitch)))
             (names (candidates-candidates state))
             (height (+ (* rows pitch) (* 2 pad)))
             (span (org.shirakumo.alloy:pxw (org.shirakumo.alloy:bounds view)))
             (widest 0.0))
        (loop for i from (candidates-first state)
              repeat rows
              do (setf widest (max widest (%view-string-width view (nth i names)))))
        (let* ((width (min (max 1.0 span) (+ widest (* 2 pad))))
               (x (max 0.0 (min cx (- span width))))
               (below (- cy height)))
          (list x (if (<= 0.0 below) below (+ cy ch)) width height))))))

(defun %view-draw-completion (view state box rows first)
  (destructuring-bind (x y width height) box
    (let* ((pad *completion-padding*)
           (pitch (max 1.0 (slot-value view 'pitch)))
           (pool (%view-completion-shapes view rows))
           (names (candidates-candidates state))
           (top (- (+ y height) pad)))
      (let ((ground (slot-value view 'completion-ground))
            (choice (slot-value view 'completion-choice)))
        (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p ground) nil
              (org.shirakumo.alloy.renderers.simple.presentations:hidden-p choice) nil)
        (reinitialize-instance ground
                               :bounds (org.shirakumo.alloy:px-extent x y width height))
        (reinitialize-instance
         choice
         :bounds (org.shirakumo.alloy:px-extent
                  x (- top (* (1+ (- (candidates-index state) first)) pitch))
                  width pitch)))
      (dotimes (j (length pool))
        (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p (aref pool j))
              t))
      (dotimes (i rows)
        (let ((shape (aref pool i)))
          (setf (org.shirakumo.alloy.renderers.simple.presentations:hidden-p shape) nil)
          (reinitialize-instance shape
                                 :text (nth (+ first i) names)
                                 :bounds (org.shirakumo.alloy:px-extent
                                          0 0 (max 1.0 (- width (* 2 pad))) 0))
          (setf (org.shirakumo.alloy.renderers.simple.presentations:offset shape)
                (org.shirakumo.alloy:px-point (+ x pad) (- top (* i pitch)))))))))

(defun %view-place-completion (view)
  "Narrow the open list to the names the prefix under the caret still carries on into and put it where it belongs."
  (let ((state (slot-value view 'completion)))
    (cond ((null state)
           (when (slot-value view 'completion-ground) (%view-hide-completion view)))
          (t
           (let* ((prefix (%view-completion-prefix view state))
                  (kept (remove-if-not (lambda (name) (%carries-on-p prefix name))
                                       (candidates-all state))))
             (cond ((null kept)
                    (setf (slot-value view 'completion) nil)
                    (%view-hide-completion view))
                   (t
                    (unless (equal kept (candidates-candidates state))
                      (setf (candidates-candidates state) kept
                            (candidates-index state) 0
                            (candidates-first state) 0))
                    (let* ((n (length kept))
                           (rows (min *completion-rows* n))
                           (index (candidates-index state))
                           (first (max 0 (min (candidates-first state) index
                                              (- n rows)))))
                      (when (<= (+ first rows) index)
                        (setf first (1+ (- index rows))))
                      (setf (candidates-first state) first)
                      (let ((box (%view-completion-box view state rows)))
                        (setf (candidates-box state) box)
                        (if box
                            (%view-draw-completion view state box rows first)
                            (%view-hide-completion view)))))))))))

(defun %view-relayout (view)
  "Fold in whatever the gesture or the layout pass changed, then make the shapes agree with it."
  (when (and (slot-value view 'renderer) (slot-value view 'buffer))
    (let ((fresh (%view-sync-context view))
          (pending (slot-value view 'pending-show)))
      (when pending
        (setf (slot-value view 'pending-show) nil)
        (let ((layout (slot-value view 'layout))
              (rows (cdr pending)))
          (multiple-value-bind (line row) (find-location layout (car pending))
            (if (eq (text-view-gesture-policy view) :classic)
                (progn (%classic-place view line row (- (* rows (slot-value view 'pitch))))
                       (%classic-clamp view))
                (multiple-value-bind (top-line top-row) (%rows-advance layout line row (- rows))
                  (setf (slot-value view 'top-line) top-line
                        (slot-value view 'top-row) top-row))))))
      (%view-clamp-top view)
      (when (eq (text-view-gesture-policy view) :classic)
        (%classic-settle view pending))
      (%view-choose-visible view)
      (%view-place-shapes view fresh)
      (%view-place-selection view)
      (%view-place-caret view)
      (mode-place (slot-value view 'mode) view)
      (%view-place-completion view))
    (org.shirakumo.alloy:mark-for-render view)))

(defun (setf text-view-caret) (index view)
  "Put the caret at document INDEX, keeping whatever the selection holds."
  (%view-forget-command-state view)
  (%view-move-mark view 'caret index)
  (%view-relayout view)
  index)

(defun (setf text-view-anchor) (index view)
  "Hold the selection from document INDEX, or release it when INDEX is NIL."
  (%view-forget-command-state view)
  (%view-move-mark view 'anchor index)
  (%view-relayout view)
  index)

(defun text-view-selection (view)
  "(values START END) of what VIEW has marked, in document indices, or NIL when it has marked nothing."
  (let ((anchor (text-view-anchor view))
        (caret (text-view-caret view)))
    (when (and anchor caret (/= anchor caret))
      (values (min anchor caret) (max anchor caret)))))

(defun text-view-set-selection (view start end)
  "Mark [START, END), the anchor at START and the caret at END."
  (%view-forget-command-state view)
  (cond ((null start)
         (%view-move-mark view 'anchor nil))
        (t
         (%view-move-mark view 'anchor start)
         (%view-move-mark view 'caret end)))
  (%view-relayout view)
  view)

;;; ------------------------------------------------------------------ the document

(defun (setf text-view-buffer) (buffer view)
  "Show BUFFER from its first line."
  (%view-drop-marks view)
  (setf (slot-value view 'mode-state) nil)
  (fill (slot-value view 'pool-styles) *stale-style*)
  (setf (slot-value view 'buffer) buffer
        (slot-value view 'layout) nil
        (slot-value view 'top-line) 0
        (slot-value view 'top-row) 0
        (slot-value view 'x-offset) 0.0
        (slot-value view 'scroll-y) 0.0
        (slot-value view 'anchor-index) nil
        (slot-value view 'anchor-u) 0.0
        (slot-value view 'anchor-caret-p) nil
        (slot-value view 'settled) nil
        ;; a placement asked for names an index of the document it was asked for
        (slot-value view 'pending-show) nil)
  (%view-relayout view)
  buffer)

(defmethod (setf text-view-gesture-policy) :after (policy (view text-view))
  ;; the pixel offset and the place a refit comes back to belong to :CLASSIC alone
  (unless (eq policy :classic)
    (let ((offset (slot-value view 'scroll-y)))
      (setf (slot-value view 'scroll-y) 0.0
            (slot-value view 'anchor-index) nil
            (slot-value view 'anchor-u) 0.0
            (slot-value view 'anchor-caret-p) nil
            (slot-value view 'settled) nil)
      (unless (zerop offset)
        (%view-relayout view)))))

(defun (setf text-view-wrap-p) (wrap-p view)
  "Lay the lines out to the viewport width, or not at all."
  (setf (slot-value view 'wrap-p) wrap-p
        (slot-value view 'x-offset) 0.0)
  (%view-relayout view)
  wrap-p)

(defun (setf text-view-gutter-p) (gutter-p view)
  "Open or close the column of line numbers."
  (setf (slot-value view 'gutter-p) gutter-p
        (slot-value view 'context) nil
        (slot-value view 'x-offset) 0.0)
  (%view-relayout view)
  gutter-p)

(defun (setf text-view-wrap-marker-p) (marker-p view)
  "Close every broken row with a glyph, or none.  :POLICY leaves that to the family."
  (setf (slot-value view 'marker-p) marker-p
        (slot-value view 'context) nil)
  (%view-relayout view)
  marker-p)

(defun (setf text-view-font) (font view)
  "Draw and measure with the face FONT from now on."
  (setf (slot-value view 'font) font)
  (%view-relayout view)
  font)

(defun (setf text-view-font-size) (size view)
  "Draw and measure at the unit SIZE from now on, or at the default when NIL."
  (setf (slot-value view 'font-size) size)
  (setf (slot-value view 'context) nil)
  (%view-relayout view)
  size)

(defun (setf text-view-line-pitch) (pitch view)
  "Put the visual rows PITCH device pixels apart, or at the face's own distance when PITCH is NIL."
  (setf (slot-value view 'line-pitch) pitch)
  (setf (slot-value view 'context) nil)
  (%view-relayout view)
  pitch)

;;; ------------------------------------------------------------------ scrolling

(defun text-view-scroll-by-rows (view rows)
  "Move the top of the viewport ROWS visual rows down the document, or up when ROWS is negative, stopping at either end."
  (let* ((layout (slot-value view 'layout))
         (count (buffer-line-count (layout-buffer layout)))
         (line (slot-value view 'top-line))
         (row (slot-value view 'top-row)))
    (if (plusp rows)
        (loop repeat rows
              do (let ((n (line-layout-row-count (layout-line layout line))))
                   (cond ((< (1+ row) n) (incf row))
                         ((< (1+ line) count) (incf line) (setf row 0))
                         (t (return)))))
        (loop repeat (- rows)
              do (cond ((plusp row) (decf row))
                       ((plusp line)
                        (decf line)
                        (setf row (1- (line-layout-row-count (layout-line layout line)))))
                       (t (return)))))
    (setf (slot-value view 'top-line) line
          (slot-value view 'top-row) row)
    (%view-relayout view)
    view))

(defun text-view-scroll-to-line (view line)
  "Put logical LINE at the top of the viewport, on its first visual row."
  (setf (slot-value view 'top-line) line
        (slot-value view 'top-row) 0)
  (%view-relayout view)
  view)

(defun text-view-row-extent (view)
  "Widest visual row among the VISIBLE lines, in device pixels."
  (let ((layout (slot-value view 'layout))
        (widest 0.0))
    (when layout
      (loop for cell across (slot-value view 'visible)
            do (let ((line-layout (layout-line layout (car cell))))
                 (dotimes (row (line-layout-row-count line-layout))
                   (setf widest (max widest (line-layout-row-width line-layout row)))))))
    widest))

(defun %view-held-scroll-x (view x)
  "X held within what the unwrapped lines span plus the caret's own bar, and in a :CLASSIC view the follow margin beyond it."
  (max 0.0 (min (float x 0f0)
                (max 0.0 (- (+ (text-view-row-extent view) (%view-caret-width view)
                               (if (eq (text-view-gesture-policy view) :classic)
                                   *classic-follow-margin*
                                   0))
                            (%view-text-width view))))))

(defun (setf text-view-scroll-x) (x view)
  "Travel the unwrapped lines X device pixels to the left, within what they span plus the caret's own bar, and in a :CLASSIC view the follow margin beyond it."
  (setf (slot-value view 'x-offset) (%view-held-scroll-x view x))
  (%view-relayout view)
  (slot-value view 'x-offset))

(defun text-view-row-shape (view index)
  "Shape showing the INDEXth visible visual row, or NIL past what the viewport holds."
  (let ((order (slot-value view 'order))
        (pool (slot-value view 'pool)))
    (when (< -1 index (length order)) (aref pool (aref order index)))))

;;; ------------------------------------------------------------------ the rows

(defun %rows-advance (layout line row count)
  "(values LINE ROW MOVED) of the visual row COUNT rows down from ROW of LINE, or up when COUNT is negative, stopping at either end of the document."
  (let ((lines (buffer-line-count (layout-buffer layout)))
        (moved 0))
    (if (plusp count)
        (loop repeat count
              do (let ((rows (line-layout-row-count (layout-line layout line))))
                   (cond ((< (1+ row) rows) (incf row) (incf moved))
                         ((< (1+ line) lines) (incf line) (setf row 0) (incf moved))
                         (t (return)))))
        (loop repeat (- count)
              do (cond ((plusp row) (decf row) (incf moved))
                       ((plusp line)
                        (decf line)
                        (setf row (1- (line-layout-row-count (layout-line layout line))))
                        (incf moved))
                       (t (return)))))
    (values line row moved)))

(defun %rows-between (layout from-line from-row to-line to-row limit)
  "How many rows down from one position the other sits, or NIL when that is more than LIMIT rows or the second sits above the first."
  (let ((line from-line)
        (row from-row))
    (dotimes (n (1+ limit))
      (when (and (eql line to-line) (eql row to-row))
        (return-from %rows-between n))
      (multiple-value-bind (next-line next-row moved) (%rows-advance layout line row 1)
        (when (zerop moved) (return))
        (setf line next-line row next-row)))
    nil))

;;; ------------------------------------------------------------- the :classic scroller
;;; A :CLASSIC view scrolls its content by the device pixel, the way a text pane in a
;;; clip does: row K of the document has its top at MARGIN-TOP + K * pitch of the
;;; content, the same margin stands under the last row, and the viewport top runs from
;;; the content's top to its height less the view's.  The place it stands at is the
;;; first visible row plus U, the pixels that row's top stands above the viewport top
;;; (negative only over the margin of the very first row).

(defun %classic-normal (view line row u)
  "(values LINE ROW U) of the place U device pixels below the top of ROW of LINE, named from the row that holds it; above the first row it stops at the top of the content."
  (let ((layout (slot-value view 'layout))
        (pitch (max 1.0 (slot-value view 'pitch))))
    (loop while (>= u pitch)
          do (multiple-value-bind (next-line next-row moved) (%rows-advance layout line row 1)
               (if (zerop moved)
                   (return)
                   (setf line next-line row next-row u (- u pitch)))))
    (loop while (< u 0.0)
          do (multiple-value-bind (next-line next-row moved) (%rows-advance layout line row -1)
               (if (zerop moved)
                   (return)
                   (setf line next-line row next-row u (+ u pitch)))))
    (values line row (max u (- (slot-value view 'text-inset))))))

(defun %classic-place (view line row u)
  "Put the viewport top U device pixels below the top of ROW of LINE."
  (multiple-value-bind (line row u) (%classic-normal view line row u)
    (setf (slot-value view 'top-line) line
          (slot-value view 'top-row) row
          (slot-value view 'scroll-y) (float (+ u (slot-value view 'text-inset)) 0f0))))

(defun %classic-trailing (view painted)
  "(values ENDS-WITH-LINEFEED EXTRA) of VIEW's text: whether it ends with a Linefeed, and the device pixels of empty rows and margin its content keeps under PAINTED rows, which only a block taller than the view keeps."
  (let* ((buffer (text-view-buffer view))
         (size (buffer-size buffer))
         (linefeed (and (plusp size) (char= #\Linefeed (buffer-char buffer (1- size)))))
         (rows (or (text-view-trailing-lines view) (if linefeed 1 0)))
         (pitch (slot-value view 'pitch))
         (inset (slot-value view 'text-inset)))
    (values linefeed
            (if (and (plusp rows)
                     (< (org.shirakumo.alloy:pxh (org.shirakumo.alloy:bounds view))
                        (+ inset inset (* (- painted (if linefeed 1 0)) pitch))))
                (+ (* rows pitch) *classic-follow-margin*)
                0.0))))

(defun %classic-end (view)
  "(values LINE ROW U) of the lowest place the viewport top reaches: the content's last pixel on the view's last one."
  (let* ((layout (slot-value view 'layout))
         (pitch (max 1.0 (slot-value view 'pitch)))
         (inset (slot-value view 'text-inset))
         (height (org.shirakumo.alloy:pxh (org.shirakumo.alloy:bounds view)))
         (line (1- (buffer-line-count (layout-buffer layout))))
         (row (1- (line-layout-row-count (layout-line layout line))))
         ;; enough rows counted back from the end to know whether the block is taller
         ;; than the view, which is all the trailing rows depend on
         (painted (1+ (nth-value 2 (%rows-advance layout line row
                                                  (- (+ 2 (ceiling height pitch))))))))
    (multiple-value-bind (linefeed extra) (%classic-trailing view painted)
      (%classic-normal view line row
                       (- (+ (if linefeed 0.0 pitch) inset extra) height)))))

(defun %classic-clamp (view)
  "Take the viewport top back to the lowest place it reaches when it stands past it."
  (multiple-value-bind (end-line end-row end-u) (%classic-end view)
    (let ((line (slot-value view 'top-line))
          (row (slot-value view 'top-row))
          (u (- (slot-value view 'scroll-y) (slot-value view 'text-inset))))
      (when (or (> line end-line)
                (and (= line end-line)
                     (or (> row end-row) (and (= row end-row) (> u end-u)))))
        (%classic-place view end-line end-row end-u)))))

(defun %classic-note (view)
  "Take the row nearest the viewport top as the one a refit puts back, and whether the caret is in view."
  (let* ((layout (slot-value view 'layout))
         (line (slot-value view 'top-line))
         (row (slot-value view 'top-row))
         (u (- (slot-value view 'scroll-y) (slot-value view 'text-inset)))
         (caret (text-view-caret view)))
    (when (>= u (/ (slot-value view 'pitch) 2.0))
      (multiple-value-bind (next-line next-row moved) (%rows-advance layout line row 1)
        (when (plusp moved)
          (setf line next-line row next-row u (- u (slot-value view 'pitch))))))
    (setf (slot-value view 'anchor-u) u
          (slot-value view 'anchor-index)
          (+ (buffer-line-start (layout-buffer layout) line)
             (line-layout-row-start (layout-line layout line) row))
          (slot-value view 'anchor-caret-p)
          (and caret
               (multiple-value-bind (x y w h) (text-view-caret-location view caret)
                 (declare (ignore x w))
                 (and y (<= 0.0 y) (<= (+ y h) (org.shirakumo.alloy:pxh
                                               (org.shirakumo.alloy:bounds view)))))))))

(defun %classic-follow (view)
  "Travel the least that puts the caret's row, and the caret across it, the follow margin inside the view."
  (let ((layout (slot-value view 'layout))
        (caret (text-view-caret view)))
    (when (and layout caret)
      (multiple-value-bind (line row x) (find-location layout caret)
        (unless (text-view-single-line-p view)
          (let* ((margin *classic-follow-margin*)
                 (pitch (slot-value view 'pitch))
                 (height (org.shirakumo.alloy:pxh (org.shirakumo.alloy:bounds view)))
                 (top-line (slot-value view 'top-line))
                 (top-row (slot-value view 'top-row))
                 (above (or (< line top-line) (and (= line top-line) (< row top-row))))
                 (down (and (not above)
                            (%rows-between layout top-line top-row line row
                                           (+ 2 (ceiling height (max 1.0 pitch))))))
                 (top (and down (- (* down pitch)
                                   (- (slot-value view 'scroll-y) (slot-value view 'text-inset))))))
            (cond ((or above (and top (< top margin)))
                   (%classic-place view line row (- margin)))
                  ((or (null down) (> (+ top pitch) (- height margin)))
                   (%classic-place view line row (- (+ pitch margin) height))))
            (%classic-clamp view)))
        (unless (text-view-wrap-p view)
          (let ((margin *classic-follow-margin*)
                (left (slot-value view 'x-offset))
                (width (%view-text-width view))
                (bar (%view-caret-width view)))
            ;; the offset only: whoever moved the caret lays the view out once after
            (cond ((< (- x left) margin)
                   (setf (slot-value view 'x-offset) (%view-held-scroll-x view (- x margin))))
                  ((< width (+ (- x left) bar margin))
                   (setf (slot-value view 'x-offset)
                         (%view-held-scroll-x view (- (+ x bar margin) width)))))))))))

(defun %classic-settle (view placed)
  "Put a :CLASSIC view where it belongs as it is laid out: after a refit, the end of the document when the whole of it stood in the view (and on every refit after that until the view is moved), otherwise the row taken last back at the top; laid out for the first time and PLACED nowhere, its first row flush with the top; after any other move, that place taken as the one to come back to; never past the end."
  (let* ((bounds (org.shirakumo.alloy:bounds view))
         (size (list (org.shirakumo.alloy:pxw bounds) (org.shirakumo.alloy:pxh bounds)))
         (settled (slot-value view 'settled))
         (anchor (slot-value view 'anchor-index)))
    (cond ((text-view-single-line-p view)
           (%classic-place view 0 0 (- (slot-value view 'text-inset))))
          ((and settled anchor (not (equal size (subseq settled 0 2))))
           (if (sixth settled)
               ;; nothing could be scrolled: the view holds on to the end of the document
               (multiple-value-call #'%classic-place view (%classic-end view))
               (multiple-value-bind (line row)
                   (find-location (slot-value view 'layout)
                                  (min anchor (buffer-size (text-view-buffer view))))
                 ;; the row goes back where it stood in its own box while the width holds,
                 ;; and to the top of the view once the width changed, unless it is the
                 ;; first row with the margin above it in sight
                 (let ((u (slot-value view 'anchor-u)))
                   (%classic-place view line row
                                   (if (or (= (first size) (first settled))
                                           (and (zerop line) (zerop row) (minusp u)))
                                       u
                                       0.0)))))
           (%classic-clamp view)
           (when (slot-value view 'anchor-caret-p)
             (%classic-follow view)))
          ((and (null settled) (null placed)
                (zerop (slot-value view 'top-line)) (zerop (slot-value view 'top-row))
                (zerop (slot-value view 'scroll-y)))
           ;; a document laid out for the first time and not placed anywhere rests with
           ;; its first row flush with the top of the view, the margin above it out of sight
           (%classic-place view 0 0 0.0)
           (%classic-clamp view)
           (%classic-note view))
          (t
           (%classic-clamp view)
           (unless (equal (list (slot-value view 'top-line) (slot-value view 'top-row)
                                (slot-value view 'scroll-y))
                          (and settled (subseq settled 2 5)))
             (%classic-note view))))
    (setf (slot-value view 'settled)
          (list (first size) (second size)
                (slot-value view 'top-line) (slot-value view 'top-row) (slot-value view 'scroll-y)
                ;; held on to the end: the whole content stands in the view, or a refit
                ;; held on to it before and nothing moved the view since
                (or (and settled (sixth settled) (not (equal size (subseq settled 0 2))))
                    (multiple-value-bind (line row u) (%classic-end view)
                      (and (zerop line) (zerop row) (<= u (- (slot-value view 'text-inset))))))))))

(defun %classic-scroll (view pixels)
  "Take a :CLASSIC view's text PIXELS device pixels up, down when negative, within its content."
  (%classic-place view (slot-value view 'top-line) (slot-value view 'top-row)
                  (+ (- (slot-value view 'scroll-y) (slot-value view 'text-inset)) pixels))
  (%classic-clamp view))

(defun %classic-index-at (view x y)
  "Document index at the point (X Y) of VIEW's box, rows past the viewport included; a point on the line between two rows belongs to the upper one."
  (let ((layout (slot-value view 'layout)))
    (multiple-value-bind (line row)
        (%rows-advance layout (slot-value view 'top-line) (slot-value view 'top-row)
                       (1- (ceiling (- (%view-text-top view) y) (max 1.0 (slot-value view 'pitch)))))
      (find-index layout line row
                  (+ (- x (%view-text-x view)) (slot-value view 'x-offset))))))

(defun text-view-show-index (view index rows)
  "Put the visual row holding document INDEX ROWS rows below the top of the viewport; a view not laid out yet does it as it is laid out, before anything of it is drawn."
  ;; the placement is made by the layout pass itself, so it is one pass either way
  (setf (slot-value view 'pending-show) (cons index rows))
  (when (slot-value view 'layout)
    (%view-relayout view))
  view)

(defun text-view-extent (view)
  "(values WIDTH HEIGHT) in device pixels of what VIEW holds as a :CLASSIC view scrolls over it: the widest row with the caret and the follow margin beyond it, or the view's own width when the lines wrap, and every row with the margin above and below it and the trailing rows of a block taller than the view.  Every line of the document is measured on each call."
  (unless (eq (text-view-gesture-policy view) :classic)
    (error "TEXT-VIEW-EXTENT answers for a :CLASSIC view; ~S answers as ~S."
           view (text-view-gesture-policy view)))
  (unless (slot-value view 'layout)
    (error "TEXT-VIEW-EXTENT: ~S has not been laid out yet." view))
  (let* ((layout (slot-value view 'layout))
         (count (buffer-line-count (layout-buffer layout)))
         (rows 0)
         (widest 0.0))
    (dotimes (line count)
      (let ((line-layout (layout-line layout line)))
        (incf rows (line-layout-row-count line-layout))
        (unless (text-view-wrap-p view)
          (dotimes (row (line-layout-row-count line-layout))
            (setf widest (max widest (line-layout-row-width line-layout row)))))))
    (multiple-value-bind (linefeed extra) (%classic-trailing view rows)
      (values (if (text-view-wrap-p view)
                  (org.shirakumo.alloy:pxw (org.shirakumo.alloy:bounds view))
                  (+ (%view-text-x view) widest (%view-caret-width view) *classic-follow-margin*))
              (+ (* 2 (slot-value view 'text-inset))
                 (* (- rows (if linefeed 1 0)) (slot-value view 'pitch))
                 extra)))))

;;; ------------------------------------------------------------------ the scrollbar

(defclass scrollbar (org.shirakumo.alloy:scrollbar)
  ((view :initarg :view :reader scrollbar-view))
  (:default-initargs :orientation :vertical
                     ;; the travel a key or a focus move asks the bar for, and this
                     ;; bar counts LOGICAL LINES rather than pixels
                     :step 1
                     :data (make-instance 'org.shirakumo.alloy:value-data :value 0))
  (:documentation "A scrollbar over the LOGICAL lines of a text-view's buffer."))

(defun make-scrollbar (view &key (orientation :vertical))
  "Scrollbar driving VIEW, in logical lines."
  (make-instance 'scrollbar :view view :orientation orientation))

(defmethod org.shirakumo.alloy:value ((sb scrollbar))
  (text-view-top-line (scrollbar-view sb)))

(defmethod (setf org.shirakumo.alloy:value) (value (sb scrollbar))
  (text-view-scroll-to-line (scrollbar-view sb) (round value))
  value)

(defmethod org.shirakumo.alloy:range ((sb scrollbar))
  (let ((view (scrollbar-view sb)))
    (cons 0 (max 0 (1- (buffer-line-count (text-view-buffer view)))))))

(defmethod org.shirakumo.alloy:scroll-handle-size ((sb scrollbar))
  (let* ((view (scrollbar-view sb))
         (shown (length (text-view-visible-lines view)))
         (total (buffer-line-count (text-view-buffer view))))
    (max 0.08 (min 1.0 (/ (float shown 0f0) (max 1 total))))))

;;; ------------------------------------------------------------------ Alloy

(defmethod org.shirakumo.alloy:suggest-size (size (view text-view))
  "A view takes the room it is given."
  size)

(defmethod (setf org.shirakumo.alloy:bounds) :after (value (view text-view))
  (declare (ignore value))
  (%view-relayout view))

(defmethod org.shirakumo.alloy:refit :after ((view text-view))
  (%view-relayout view))

(org.shirakumo.alloy.renderers.simple.presentations:define-realization
    (org.shirakumo.alloy.renderers.simple.presentations:default-look-and-feel text-view)
  ((:background org.shirakumo.alloy.renderers.simple:rectangle)
   (org.shirakumo.alloy:margins)
   :z-index -3))

(org.shirakumo.alloy.renderers.simple.presentations:define-update
    (org.shirakumo.alloy.renderers.simple.presentations:default-look-and-feel text-view)
  (:background
   :hidden-p nil
   :pattern (%view-pattern (text-view-bg-color org.shirakumo.alloy:renderable)
                           *default-background*)))

(defmethod org.shirakumo.alloy.renderers.simple.presentations::realize-renderable :after
    ((renderer org.shirakumo.alloy.renderers.simple.presentations:default-look-and-feel)
     (view text-view))
  ;; realizing clears the shape table, so the pool is gone with it
  (setf (slot-value view 'renderer) renderer)
  (setf (fill-pointer (slot-value view 'pool)) 0
        (fill-pointer (slot-value view 'pool-keys)) 0
        (fill-pointer (slot-value view 'pool-styles)) 0
        (fill-pointer (slot-value view 'sel-pool)) 0
        (slot-value view 'mode-state) nil
        (slot-value view 'caret-shape) nil
        (slot-value view 'gutter-band) nil
        (slot-value view 'marker-band) nil
        (slot-value view 'gutter-ground) nil
        (slot-value view 'gutter-rule-shape) nil
        (slot-value view 'completion-ground) nil
        (slot-value view 'completion-choice) nil
        (fill-pointer (slot-value view 'completion-pool)) 0
        (slot-value view 'order) #())
  (setf (slot-value view 'context) nil)
  (%view-relayout view))

(defmethod org.shirakumo.alloy:render :around ((renderer org.shirakumo.alloy:renderer) (view text-view))
  ;; a pane cuts its own drawing at its rect, so a line the viewport only half
  ;; holds is half drawn rather than drawn whole
  (org.shirakumo.alloy:constrain-visibility view renderer)
  (call-next-method))

(defmethod org.shirakumo.alloy:handle ((event org.shirakumo.alloy:scroll) (view text-view))
  ;; the wheel belongs to the pane it stands over, not to the one holding the keyboard
  (cond ((not (org.shirakumo.alloy:contained-p (org.shirakumo.alloy:location event) view))
         (call-next-method))
        ((and (eq (text-view-gesture-policy view) :classic)
              (not (text-view-single-line-p view)))
         ;; a notch held with Control or Command is a gesture of its own: taken, no travel
         (unless (intersection '(:control :super) *modifiers*)
           (let ((dy (org.shirakumo.alloy:dy event)))
             (%classic-scroll view (* *classic-wheel-step*
                                      (if (zerop dy) (org.shirakumo.alloy:dx event) (- dy))))
             (%view-relayout view)))
         t)
        (t
         (let ((rows (round (* *wheel-rows* (- (org.shirakumo.alloy:dy event))))))
           (if (zerop rows)
               (call-next-method)
               (text-view-scroll-by-rows view rows))))))

(defun %view-local-point (view location)
  "(values X Y) of LOCATION, which arrives in the window's own coordinates, in VIEW's box."
  (org.shirakumo.alloy:with-unit-parent view
    (let ((origin (org.shirakumo.alloy:global-location view)))
      (values (- (org.shirakumo.alloy:pxx location) (org.shirakumo.alloy:pxx origin))
              (- (org.shirakumo.alloy:pxy location) (org.shirakumo.alloy:pxy origin))))))

(defun %view-index-of-event (view event)
  (multiple-value-bind (x y) (%view-local-point view (org.shirakumo.alloy:location event))
    (text-view-index-at view x y)))

;;; ------------------------------------------------------------- the host

(defparameter *multi-click-seconds* 0.5d0
  "Seconds within which consecutive presses still count as one multiple click.")

(defparameter *multi-click-distance* 6.0
  "Device pixels within which consecutive presses still count as one multiple click.")

(defun %host-take-focus (view)
  "Give VIEW the keyboard focus of the tree it is in."
  (setf (org.shirakumo.alloy:focus view) :strong))

(defun %host-shift-down-p ()
  "True while a Shift key is held."
  (and (member :shift *modifiers*) t))

(defun %host-now ()
  "Seconds on a monotonic clock, for telling consecutive presses apart."
  (/ (float (get-internal-real-time) 0d0) internal-time-units-per-second))

;; ponytail: a pointer event carries no modifiers, so the state is taken off the
;; key events instead; a host whose focus sits outside every view sets *modifiers*
(defmethod org.shirakumo.alloy:handle :before ((event org.shirakumo.alloy:key-event)
                                               (view text-view))
  (setf *modifiers* (org.shirakumo.alloy:modifiers event)))

;;; ------------------------------------------------------------------ runs

(defparameter *field-run-delimiters* '(#\Space #\Tab #\- #\Return #\Linefeed)
  "Characters a :FIELD run stops at.")

(defparameter *field-run-takes-delimiter* nil
  "True when a :FIELD run reaches past its last character to the delimiter that ends it.")

(defun %run-constituent-p (character policy)
  (case policy
    (:field (not (member character *field-run-delimiters*)))
    (:classic (not (member character *classic-run-delimiters*)))
    (t (or (alphanumericp character) (char= character #\-)))))

(defun %view-run-at (view index)
  "(values LO HI) of the run of characters a multiple click takes around document INDEX, never leaving the logical line."
  (multiple-value-bind (lo hi) (mode-run-at (slot-value view 'mode) view index)
    (when lo (return-from %view-run-at (values lo hi))))
  (let* ((buffer (slot-value view 'buffer))
         (policy (text-view-gesture-policy view))
         (line (buffer-line-of-index buffer index))
         (start (buffer-line-start buffer line))
         (end (buffer-line-end buffer line)))
    (if (<= end start)
        (values index index)
        (let ((at (max start (min index (1- end)))))
          (if (and (eq policy :editor)
                   (not (%run-constituent-p (buffer-char buffer at) policy)))
              (values at (1+ at))
              (let ((lo at) (hi at))
                (loop while (and (< start lo)
                                 (%run-constituent-p (buffer-char buffer (1- lo)) policy))
                      do (decf lo))
                (loop while (and (< hi end)
                                 (%run-constituent-p (buffer-char buffer hi) policy))
                      do (incf hi))
                (when (and (eq policy :field) *field-run-takes-delimiter* (< hi end))
                  (incf hi))
                (values lo hi)))))))

(defun %view-line-run (view index)
  "(values LO HI) of the whole logical line holding document INDEX, its terminator included."
  (let* ((buffer (slot-value view 'buffer))
         (line (buffer-line-of-index buffer index)))
    (values (buffer-line-start buffer line)
            (if (< (1+ line) (buffer-line-count buffer))
                (buffer-line-start buffer (1+ line))
                (buffer-size buffer)))))

(defun %view-set-range (view lo hi)
  (%view-move-mark view 'anchor lo)
  (%view-move-mark view 'caret hi))

(defun %view-click-leg (view event)
  "Which leg of a multiple click the press EVENT opens."
  (let* ((location (org.shirakumo.alloy:location event))
         (x (org.shirakumo.alloy:pxx location))
         (y (org.shirakumo.alloy:pxy location))
         (now (%host-now))
         (previous (slot-value view 'click))
         (legs (if (member (text-view-gesture-policy view) '(:field :classic)) 2 3))
         (button (org.shirakumo.alloy:kind event))
         (leg (if (and previous
                       (< (- now (second previous)) *multi-click-seconds*)
                       ;; a :CLASSIC pane counts presses of one button only
                       (or (not (eq (text-view-gesture-policy view) :classic))
                           (eql button (fifth previous)))
                       (< (sqrt (+ (expt (- x (third previous)) 2)
                                   (expt (- y (fourth previous)) 2)))
                          *multi-click-distance*))
                  (1+ (mod (first previous) legs))
                  1)))
    (setf (slot-value view 'click) (list leg now x y button))
    leg))

(defun text-view-select-run (view index)
  "Mark the run of VIEW's text around document INDEX, as a double click does, and close the view's own count of a multiple click: the press after it starts afresh."
  (when (slot-value view 'layout)
    (multiple-value-bind (lo hi) (%view-run-at view index)
      (%view-set-range view lo hi))
    (setf (slot-value view 'click) nil)
    (when (eq (text-view-gesture-policy view) :classic)
      (%classic-follow view))
    (%view-relayout view)
    t))

(defun %view-extend-by-runs (view index)
  "(values ANCHOR CARET) taking the selection to document INDEX by whole runs, the anchor at the far end of the run the multiple click opened."
  (let ((word (slot-value view 'word)))
    (multiple-value-bind (lo hi) (%view-run-at view index)
      (cond ((<= (cdr word) index) (values (car word) (if (< lo index) hi index)))
            ((<= index (car word)) (values (cdr word) (if (< index hi) lo index)))
            (t (values (car word) (cdr word)))))))

(defun %view-drag-to (view x y &optional (index (text-view-index-at view x y)) (relayout t))
  "Take the selection to the point (X Y) of VIEW's box, which is document INDEX, laying the view out again when RELAYOUT and a mark moved."
  (if (slot-value view 'word)
      (multiple-value-bind (anchor caret) (%view-extend-by-runs view index)
        (unless (and (eql anchor (text-view-anchor view))
                     (eql caret (text-view-caret view)))
          (%view-move-mark view 'anchor anchor)
          (%view-move-mark view 'caret caret)
          (when relayout (%view-relayout view))))
      ;; a travel begins only once the pointer has LEFT the index the press landed on.
      (let ((press (slot-value view 'press)))
        (when (or (slot-value view 'dragging) (not (eql index press)))
          (setf (slot-value view 'dragging) t)
          (when (null (slot-value view 'anchor))
            (%view-move-mark view 'anchor press))
          (unless (eql index (text-view-caret view))
            (%view-move-mark view 'caret index)
            (when relayout (%view-relayout view)))))))

;;; ------------------------------------------------------------- travel outside

(defparameter *field-autoscroll-rows* 1
  "Visual rows a :FIELD travels for each pointer move that lands outside it.")

(defparameter *autoscroll-near* '(7.0 . 0.2)
  "Device pixels outside an :EDITOR and the seconds one row takes at that distance.")

(defparameter *autoscroll-far* '(400.0 . 0.01)
  "Device pixels at which an :EDITOR's travel saturates, and the seconds one row takes from there on.")

(defun %autoscroll-rate (distance)
  "Visual rows per second for a pointer DISTANCE device pixels outside the view."
  (let* ((near (car *autoscroll-near*))
         (far (car *autoscroll-far*))
         (held (max near (min far distance))))
    (/ 1.0 (+ (cdr *autoscroll-near*)
              (* (- (cdr *autoscroll-far*) (cdr *autoscroll-near*))
                 (/ (- held near) (- far near)))))))

(defun %view-disarm-autoscroll (view)
  (setf (slot-value view 'scroll-rate) nil
        (slot-value view 'scroll-residue) 0.0))

(defun %view-arm-autoscroll (view x y)
  "Answer a travel that left VIEW with the button down."
  (let* ((height (org.shirakumo.alloy:pxh (org.shirakumo.alloy:bounds view)))
         (out (cond ((< height y) (- y height))
                    ((< y 0.0) y)
                    (t 0.0))))
    (cond ((zerop out)
           (%view-disarm-autoscroll view))
          ((eq (text-view-gesture-policy view) :field)
           (%view-disarm-autoscroll view)
           (text-view-scroll-by-rows view (if (plusp out)
                                              (- *field-autoscroll-rows*)
                                              *field-autoscroll-rows*)))
          (t
           (setf (slot-value view 'scroll-at) (cons x y)
                 (slot-value view 'scroll-rate)
                 (* (if (plusp out) -1.0 1.0) (%autoscroll-rate (abs out))))))))

(defun %view-autoscroll-tick (view dt)
  "One frame of the travel an :EDITOR keeps up while the pointer rests outside it."
  (let* ((residue (+ (slot-value view 'scroll-residue)
                     (* dt (slot-value view 'scroll-rate))))
         (rows (truncate residue))
         (at (slot-value view 'scroll-at)))
    (setf (slot-value view 'scroll-residue) (- residue rows))
    (unless (zerop rows)
      (text-view-scroll-by-rows view rows)
      (%view-drag-to view (car at) (cdr at)))))

;;; ------------------------------------------------------------------ gestures

(defmethod org.shirakumo.alloy:handle ((event org.shirakumo.alloy:pointer-down) (view text-view))
  (cond ((not (org.shirakumo.alloy:contained-p (org.shirakumo.alloy:location event) view))
         ;; the pane holding the keyboard is handed every event first, so a press is
         ;; this pane's own only once the point it landed on is inside it
         (call-next-method))
        ((not (if (eq (text-view-gesture-policy view) :classic)
                  ;; a :CLASSIC pane presses with every button but the right one
                  (not (eql :right (org.shirakumo.alloy:kind event)))
                  (eql :left (org.shirakumo.alloy:kind event))))
         ;; another button breaks the cycle, the way it does under a real mouse
         (setf (slot-value view 'click) nil)
         (call-next-method))
        ((null (slot-value view 'layout))
         (org.shirakumo.alloy:decline))
        (t
         (let ((index (%view-index-of-event view event))
               ;; a :CLASSIC press always starts afresh, Shift held or not
               (shift (and (not (eq (text-view-gesture-policy view) :classic))
                           (%host-shift-down-p)))
               (leg (%view-click-leg view event)))
           (%host-take-focus view)
           (%view-forget-command-state view)
           ;; a press anywhere is an answer other than one of the candidates
           (setf (slot-value view 'completion) nil)
           (setf (slot-value view 'press) index
                 (slot-value view 'dragging) nil
                 (slot-value view 'word) nil)
           (%view-disarm-autoscroll view)
           (cond (shift
                  (%view-move-mark view 'anchor
                                   (or (text-view-anchor view) (text-view-caret view)))
                  (%view-move-mark view 'caret index))
                 ((eql leg 2)
                  (multiple-value-bind (lo hi) (%view-run-at view index)
                    (setf (slot-value view 'word) (cons lo hi))
                    (%view-set-range view lo hi)))
                 ((eql leg 3)
                  (multiple-value-bind (lo hi) (%view-line-run view index)
                    (%view-set-range view lo hi)))
                 (t
                  (%view-move-mark view 'anchor nil)
                  (%view-move-mark view 'caret index)))
           ;; a :CLASSIC press brings the caret's row the follow margin inside the view
           (when (eq (text-view-gesture-policy view) :classic)
             (%classic-follow view))
           (%view-relayout view))
         t)))

(defmethod org.shirakumo.alloy:handle ((event org.shirakumo.alloy:pointer-move) (view text-view))
  "Travel a drag; pointing at a pane never moves the keyboard focus to it."
  (cond ((slot-value view 'press)
         (multiple-value-bind (x y)
             (%view-local-point view (org.shirakumo.alloy:location event))
           (cond ((eq (text-view-gesture-policy view) :classic)
                  ;; each move outside takes the text one step toward the pointer,
                  ;; then the caret to the character under it, then the caret's row
                  ;; into view
                  (let ((box (org.shirakumo.alloy:bounds view)))
                    (cond ((< (org.shirakumo.alloy:pxh box) y)
                           (%classic-scroll view (- *classic-drag-step*)))
                          ((< y 0.0)
                           (%classic-scroll view *classic-drag-step*)))
                    (unless (text-view-wrap-p view)
                      (cond ((< x (%view-text-x view))
                             (setf (slot-value view 'x-offset)
                                   (%view-held-scroll-x
                                    view (- (slot-value view 'x-offset) *classic-drag-step*))))
                            ((< (org.shirakumo.alloy:pxw box) x)
                             (setf (slot-value view 'x-offset)
                                   (%view-held-scroll-x
                                    view (+ (slot-value view 'x-offset) *classic-drag-step*)))))))
                  ;; the step, the character under the pointer and the follow all read
                  ;; the rows and the offsets, not the shapes: one layout after them
                  (%view-drag-to view x y (%classic-index-at view x y) nil)
                  (%classic-follow view)
                  (%view-relayout view))
                 (t
                  (%view-arm-autoscroll view x y)
                  (%view-drag-to view x y))))
         t)
        ((org.shirakumo.alloy:contained-p (org.shirakumo.alloy:location event) view) t)
        (t (call-next-method))))

(defmethod org.shirakumo.alloy:handle ((event org.shirakumo.alloy:pointer-up) (view text-view))
  (cond ((null (slot-value view 'press))
         (call-next-method))
        (t
         (setf (slot-value view 'press) nil
               (slot-value view 'dragging) nil
               (slot-value view 'word) nil)
         (%view-disarm-autoscroll view)
         (let ((anchor (text-view-anchor view)))
           (when (and anchor (eql anchor (text-view-caret view)))
             (setf (text-view-anchor view) nil)))
         t)))

(defmethod org.shirakumo.alloy.animation:update :after ((view text-view) dt)
  (when (slot-value view 'scroll-rate)
    (%view-autoscroll-tick view dt)))
