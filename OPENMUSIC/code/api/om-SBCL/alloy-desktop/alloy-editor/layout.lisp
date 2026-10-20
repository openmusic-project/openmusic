(in-package :alloy-editor)

(defconstant +layout-cache-lines+ 2048
  "Measured lines a LAYOUT keeps before it starts over.")

;;; ------------------------------------------------------------------ metrics

(defun atlas-em (data)
  "Em size in device pixels the atlas DATA was baked at."
  (org.shirakumo.alloy.renderers.opengl.msdf::msdf-em-size data))

(defun pen-advance (data string start end px)
  "Pen travel from START to END of STRING in device pixels, on the same grid the glyph pass draws on."
  (let* ((scale (/ (float px 0f0) (atlas-em data)))
         (from  (max 0 (min start (length string))))
         (to    (max from (min end (length string)))))
    (* scale (float (3b-bmfont:measure-glyphs data string :start from :end to) 0f0))))

(defun font-line-height (font px)
  "Baseline-to-baseline distance of FONT at PX device pixels, as the atlas was baked with it."
  (let ((data (org.shirakumo.alloy.renderers.opengl.msdf::data font)))
    (* (/ (float px 0f0) (atlas-em data))
       (float (3b-bmfont:line-height data) 0f0))))

;;; ------------------------------------------------------------------ context

(defstruct (measure-context
            (:constructor make-measure-context (font scale &key wrap-width (halign :start)))
            (:conc-name context-)
            (:copier nil)
            (:predicate nil))
  "What one line has to be laid out in to come out exactly as the bake lays it out."
  font scale wrap-width halign)

(defun %same-context-p (a b)
  (and a b
       (eq (context-font a) (context-font b))
       (eql (context-scale a) (context-scale b))
       (eql (context-wrap-width a) (context-wrap-width b))
       (eq (context-halign a) (context-halign b))))

;;; ------------------------------------------------------------------ one line

(defstruct (line-layout (:constructor %make-line-layout (length breaks widths))
                        (:conc-name line-layout-)
                        (:copier nil)
                        (:predicate nil))
  "One logical line laid out."
  length breaks widths)

(defun line-layout-row-count (line-layout)
  "Visual rows LINE-LAYOUT occupies; an empty line still occupies one."
  (length (line-layout-breaks line-layout)))

(defun %check-row (line-layout row)
  (%check-index row (1- (line-layout-row-count line-layout)) 'line-layout-row))

(defun line-layout-row-start (line-layout row)
  "Index, inside the line, of the first character of ROW."
  (%check-row line-layout row)
  (if (zerop row) 0 (aref (line-layout-breaks line-layout) (1- row))))

(defun line-layout-row-end (line-layout row)
  "Index, inside the line, one past the last character of ROW."
  (%check-row line-layout row)
  (aref (line-layout-breaks line-layout) row))

(defun line-layout-row-width (line-layout row)
  "Pen width of ROW in device pixels."
  (%check-row line-layout row)
  (aref (line-layout-widths line-layout) row))

(defun %row-of (line-layout local)
  "Visual row holding LOCAL."
  (let ((breaks (line-layout-breaks line-layout))
        (row 0))
    (loop for i from 0 below (1- (length breaks))
          while (<= (aref breaks i) local)
          do (incf row))
    row))

;;; ------------------------------------------------------------------ measuring

(defun %line-breaks (text context)
  "Row end indices of TEXT laid out on its own, from the glyph pass itself, so the count, the positions and the pen can never disagree with the bake."
  (let ((chain (org.shirakumo.alloy.renderers.opengl.msdf::fallback-chain
                (context-font context)))
        (extent (org.shirakumo.alloy:px-extent
                 0 0 (or (context-wrap-width context) most-positive-single-float) 1.0)))
    (flet ((skip (c x y x- y- x+ y+ u- v- u+ v+)
             (declare (ignore c x y x- y- x+ y+ u- v- u+ v+))))
      (declare (dynamic-extent #'skip))
      (org.shirakumo.alloy.renderers.opengl.msdf::map-glyphs
       (org.shirakumo.alloy.renderers.opengl.msdf::compute-glyph-font-sequence text chain)
       #'skip text extent (context-scale context)
       (and (context-wrap-width context) t)
       :halign (context-halign context)))))

(defun %measure-text (text context)
  "LINE-LAYOUT of TEXT, a single logical line carrying no line terminator."
  (let* ((n (length text))
         (breaks (if (zerop n)
                     (make-array 1 :element-type '(unsigned-byte 32) :initial-element 0)
                     (coerce (%line-breaks text context)
                             '(simple-array (unsigned-byte 32) (*)))))
         (data (org.shirakumo.alloy.renderers.opengl.msdf::data (context-font context)))
         (scale (context-scale context))
         (widths (make-array (length breaks) :element-type 'single-float)))
    (loop for row from 0 below (length breaks)
          for from = 0 then to
          for to = (aref breaks row)
          do (setf (aref widths row) (pen-advance data text from to scale)))
    (%make-line-layout n breaks widths)))

;;; ------------------------------------------------------------------ the layout

(defclass layout ()
  ((buffer    :initarg :buffer  :reader layout-buffer)
   (context   :initarg :context :reader layout-context)
   (observer  :initarg :observer :initform nil :accessor layout-observer
              :documentation "Called with the first line an edit invalidated and the amount the lines under it moved, so anything else kept per line is renumbered by the same reading of that edit.")
   (cache     :accessor %cache  :initform (make-hash-table :test 'eql))
   (revision  :accessor %revision-at)
   (generation :accessor %generation-at)
   (line-count :accessor %line-count-at))
  (:documentation "Line layouts of one buffer under one measurement context, measured on demand and kept per line."))

(defun make-layout (buffer context)
  "Fresh layout of BUFFER measured in CONTEXT, with nothing measured yet."
  (let ((layout (make-instance 'layout :buffer buffer :context context)))
    (setf (%revision-at layout) (buffer-revision buffer)
          (%generation-at layout) (buffer-generation buffer)
          (%line-count-at layout) (buffer-line-count buffer))
    layout))

(defun (setf layout-context) (context layout)
  "Measure from now on in CONTEXT."
  (unless (%same-context-p context (layout-context layout))
    (clrhash (%cache layout)))
  (setf (slot-value layout 'context) context))

(defun %resync-cache (layout first delta)
  "Keep what the edit left alone."
  (let ((cache (%cache layout)))
    (if (null delta)
        (let ((stale '()))
          (maphash (lambda (line entry)
                     (declare (ignore entry))
                     (when (>= line first) (push line stale)))
                   cache)
          (dolist (line stale) (remhash line cache)))
        (let ((last-dirty (+ first (max 0 (- delta))))
              (fresh (make-hash-table :test 'eql :size (hash-table-count cache))))
          (maphash (lambda (line entry)
                     (cond ((< line first) (setf (gethash line fresh) entry))
                           ((> line last-dirty) (setf (gethash (+ line delta) fresh) entry))))
                   cache)
          (setf (%cache layout) fresh)))))

(defun %sync (layout)
  "Fold into the cache whatever the buffer changed since the last look, and hand the observer that same reading of it."
  (let* ((buffer (layout-buffer layout))
         (revision (buffer-revision buffer)))
    (unless (= revision (%revision-at layout))
      (let ((first (buffer-take-first-changed-line buffer))
            (steps (- revision (%revision-at layout))))
        (multiple-value-bind (from delta)
            (cond ((null first) (values 0 nil))
                  ;; a document put in place of the one before it renumbers nothing:
                  ;; it replaces every line, and no first line and delta describe that
                  ((/= (buffer-generation buffer) (%generation-at layout)) (values 0 nil))
                  ;; one mutation moved the lines below it by one known amount;
                  ;; several did not, and their net amount describes none of them
                  ((= steps 1)
                   (values first (- (buffer-line-count buffer) (%line-count-at layout))))
                  (t (values first nil)))
          (%resync-cache layout from delta)
          (let ((observer (layout-observer layout)))
            (when observer (funcall observer from delta)))))
      (setf (%revision-at layout) revision
            (%generation-at layout) (buffer-generation buffer)
            (%line-count-at layout) (buffer-line-count buffer)))))

(defun %measure-line (layout line)
  (let ((buffer (layout-buffer layout)))
    (%measure-text (buffer-substring buffer
                                     (buffer-line-start buffer line)
                                     (buffer-line-end buffer line))
                   (layout-context layout))))

(defun layout-line (layout line)
  "LINE-LAYOUT of logical LINE, measured now if it is not in the cache."
  (%sync layout)
  (%check-line (layout-buffer layout) line 'layout-line)
  (let ((cache (%cache layout)))
    (or (gethash line cache)
        (progn
          ;; ponytail: the ceiling is cleared whole rather than evicted line by line.
          (when (>= (hash-table-count cache) +layout-cache-lines+)
            (clrhash cache))
          (setf (gethash line cache) (%measure-line layout line))))))

;;; ------------------------------------------------------------------ queries

(defun find-line (layout index)
  "Logical line containing document INDEX."
  (buffer-line-of-index (layout-buffer layout) index))

(defun find-location (layout index)
  "(values LINE ROW X) of document INDEX."
  (let* ((buffer (layout-buffer layout))
         (line (buffer-line-of-index buffer index))
         (start (buffer-line-start buffer line))
         (line-layout (layout-line layout line))
         (local (min (- index start) (line-layout-length line-layout)))
         (row (%row-of line-layout local))
         (open (line-layout-row-start line-layout row))
         (context (layout-context layout)))
    (values line row
            (pen-advance
             (org.shirakumo.alloy.renderers.opengl.msdf::data (context-font context))
             (buffer-substring buffer start (+ start (line-layout-length line-layout)))
             open local (context-scale context)))))

(defun find-index (layout line row x)
  "Document index whose pen travel on ROW of LINE is nearest to X device pixels."
  (let* ((buffer (layout-buffer layout))
         (line-layout (layout-line layout line))
         (start (buffer-line-start buffer line))
         (open (line-layout-row-start line-layout row))
         (end (line-layout-row-end line-layout row))
         (text (buffer-substring buffer start (+ start (line-layout-length line-layout))))
         (context (layout-context layout))
         (data (org.shirakumo.alloy.renderers.opengl.msdf::data (context-font context)))
         (scale (context-scale context))
         (best open)
         (best-distance most-positive-single-float))
    ;; ponytail: the pen is walked again for every candidate, which is the square of the row in the length of the row.
    (loop for i from open to end
          for distance = (abs (- (pen-advance data text open i scale) x))
          do (when (< distance best-distance)
               (setf best-distance distance best i)))
    (+ start best)))
