(in-package :alloy-editor)

(define-condition buffer-index-error (error)
  ((operation :initarg :operation :reader buffer-index-error-operation)
   (index     :initarg :index     :reader buffer-index-error-index)
   (limit     :initarg :limit     :reader buffer-index-error-limit))
  (:report (lambda (condition stream)
             (format stream "~A: index ~S outside 0..~D."
                     (buffer-index-error-operation condition)
                     (buffer-index-error-index condition)
                     (buffer-index-error-limit condition))))
  (:documentation "An index left the buffer's bounds; out-of-range is signalled, never clamped."))

(define-condition editor-error (error)
  ((text :initarg :text :reader editor-error-text))
  (:report (lambda (condition stream) (write-string (editor-error-text condition) stream)))
  (:documentation "Something the editor was asked for and would not do."))

(defclass buffer ()
  ((data              :accessor %data              :initform (make-string 64))
   (gap-start         :accessor %gap-start         :initform 0)
   (gap-end           :accessor %gap-end           :initform 64)
   (lines             :accessor %lines
                      :initform (make-array 16 :element-type 'fixnum :initial-element 0))
   (line-gap-start    :accessor %line-gap-start    :initform 1)
   (line-gap-end      :accessor %line-gap-end      :initform 16)
   (line-shift        :accessor %line-shift        :initform 0)
   (revision          :accessor %revision          :reader buffer-revision :initform 0)
   (generation        :accessor %generation        :reader buffer-generation :initform 0)
   (first-changed-line :accessor %first-changed-line :reader buffer-first-changed-line
                       :initform nil)
   (modified-p        :accessor buffer-modified-p  :initform nil)
   (read-only-p       :accessor buffer-read-only-p :initform nil
                      :documentation "True while every edit is refused.")
   (marks             :accessor %marks             :initform '())
   (undo              :accessor %undo              :initform '())
   (redo              :accessor %redo              :initform '())
   (boundary-p        :accessor %boundary-p        :initform nil))
  (:documentation "A text document with a line index, marks and an operation-based history."))

;;; ------------------------------------------------------------------ characters

(defun %check-index (index limit operation)
  (unless (and (integerp index) (<= 0 index limit))
    (error 'buffer-index-error :operation operation :index index :limit limit))
  index)

(defun %check-writable (buffer)
  "Refuse the edit about to happen when BUFFER is read only."
  (when (buffer-read-only-p buffer)
    (error 'editor-error :text "The text is read only.")))

(defun buffer-size (buffer)
  "Number of characters in BUFFER."
  (- (length (%data buffer)) (- (%gap-end buffer) (%gap-start buffer))))

(defun %raw-index (buffer index)
  (if (< index (%gap-start buffer))
      index
      (+ index (- (%gap-end buffer) (%gap-start buffer)))))

(defun %move-gap (buffer position)
  (let ((data (%data buffer))
        (gap-start (%gap-start buffer))
        (gap-end (%gap-end buffer)))
    (declare (type simple-string data) (type fixnum gap-start gap-end position))
    (cond ((< position gap-start)
           (let ((moved (- gap-start position)))
             (replace data data :start1 (- gap-end moved) :end1 gap-end
                                :start2 position :end2 gap-start)
             (setf (%gap-start buffer) position
                   (%gap-end buffer) (- gap-end moved))))
          ((> position gap-start)
           (let ((moved (- position gap-start)))
             (replace data data :start1 gap-start :end1 position
                                :start2 gap-end :end2 (+ gap-end moved))
             (setf (%gap-start buffer) position
                   (%gap-end buffer) (+ gap-end moved)))))))

(defun %ensure-gap (buffer needed)
  (when (< (- (%gap-end buffer) (%gap-start buffer)) needed)
    (let* ((data (%data buffer))
           (gap-start (%gap-start buffer))
           (gap-end (%gap-end buffer))
           (size (- (length data) (- gap-end gap-start)))
           (capacity (max 64 (* 2 (+ size needed))))
           (fresh (make-string capacity))
           (tail (- (length data) gap-end)))
      (replace fresh data :end2 gap-start)
      (replace fresh data :start1 (- capacity tail) :start2 gap-end)
      (setf (%data buffer) fresh
            (%gap-end buffer) (- capacity tail)))))

(defun %check-range (buffer start end operation)
  (let ((size (buffer-size buffer)))
    (unless (and (integerp start) (integerp end) (<= 0 start) (<= start end) (<= end size))
      (error 'buffer-index-error :operation operation
                                 :index (if (and (integerp start) (<= 0 start size)) end start)
                                 :limit size))))

(defun buffer-char (buffer index)
  "Character at INDEX."
  (%check-index index (1- (buffer-size buffer)) 'buffer-char)
  (char (%data buffer) (%raw-index buffer index)))

(defun buffer-substring (buffer start end)
  "Fresh string with the characters in [START, END)."
  (%check-range buffer start end 'buffer-substring)
  (let* ((data (%data buffer))
         (gap-start (%gap-start buffer))
         (gap (- (%gap-end buffer) gap-start))
         (out (make-string (- end start))))
    (cond ((<= end gap-start)
           (replace out data :start2 start :end2 end))
          ((>= start gap-start)
           (replace out data :start2 (+ start gap) :end2 (+ end gap)))
          (t
           (replace out data :start2 start :end2 gap-start)
           (replace out data :start1 (- gap-start start)
                             :start2 (+ gap-start gap) :end2 (+ end gap))))
    out))

(defun buffer-text (buffer)
  "Fresh string with the whole document; consumers never see the live storage."
  (buffer-substring buffer 0 (buffer-size buffer)))

;;; ------------------------------------------------------------------ line index

(defun buffer-line-count (buffer)
  "Number of logical lines; a trailing Linefeed opens a last, empty line."
  (- (length (%lines buffer)) (- (%line-gap-end buffer) (%line-gap-start buffer))))

(defun %check-line (buffer line operation)
  (%check-index line (1- (buffer-line-count buffer)) operation))

(defun %line-ref (buffer line)
  (let ((gap-start (%line-gap-start buffer)))
    (if (< line gap-start)
        (aref (%lines buffer) line)
        (+ (aref (%lines buffer) (+ line (- (%line-gap-end buffer) gap-start)))
           (%line-shift buffer)))))

(defun %move-line-gap (buffer line)
  (let ((lines (%lines buffer))
        (gap-start (%line-gap-start buffer))
        (gap-end (%line-gap-end buffer))
        (shift (%line-shift buffer)))
    (declare (type (simple-array fixnum (*)) lines)
             (type fixnum gap-start gap-end shift line))
    (cond ((< line gap-start)
           (let ((moved (- gap-start line)))
             (replace lines lines :start1 (- gap-end moved) :end1 gap-end
                                  :start2 line :end2 gap-start)
             (loop for i of-type fixnum from (- gap-end moved) below gap-end
                   do (decf (aref lines i) shift))
             (setf (%line-gap-start buffer) line
                   (%line-gap-end buffer) (- gap-end moved))))
          ((> line gap-start)
           (let ((moved (- line gap-start)))
             (loop for i of-type fixnum from gap-end below (+ gap-end moved)
                   do (incf (aref lines i) shift))
             (replace lines lines :start1 gap-start :end1 line
                                  :start2 gap-end :end2 (+ gap-end moved))
             (setf (%line-gap-start buffer) line
                   (%line-gap-end buffer) (+ gap-end moved)))))))

(defun %ensure-line-gap (buffer needed)
  (when (< (- (%line-gap-end buffer) (%line-gap-start buffer)) needed)
    (let* ((lines (%lines buffer))
           (gap-start (%line-gap-start buffer))
           (gap-end (%line-gap-end buffer))
           (count (- (length lines) (- gap-end gap-start)))
           (capacity (max 16 (* 2 (+ count needed))))
           (fresh (make-array capacity :element-type 'fixnum :initial-element 0))
           (tail (- (length lines) gap-end)))
      (replace fresh lines :end2 gap-start)
      (replace fresh lines :start1 (- capacity tail) :start2 gap-end)
      (setf (%lines buffer) fresh
            (%line-gap-end buffer) (- capacity tail)))))

(defun %find-line (buffer index)
  ;; the gap sits where the last edit was, so during a typing burst the answer is
  ;; the line just before it and no search runs at all
  (let ((gap-start (%line-gap-start buffer))
        (count (buffer-line-count buffer)))
    (when (plusp gap-start)
      (let ((line (1- gap-start)))
        (when (and (<= (%line-ref buffer line) index)
                   (or (= gap-start count) (< index (%line-ref buffer gap-start))))
          (return-from %find-line line))))
    (let ((low 0) (high (1- count)))
      (loop while (< low high)
            do (let ((middle (ceiling (+ low high) 2)))
                 (if (<= (%line-ref buffer middle) index)
                     (setf low middle)
                     (setf high (1- middle)))))
      low)))

(defun buffer-line-of-index (buffer index)
  "Logical line containing INDEX."
  (%check-index index (buffer-size buffer) 'buffer-line-of-index)
  (%find-line buffer index))

(defun buffer-line-start (buffer line)
  "Index of the first character of LINE."
  (%check-line buffer line 'buffer-line-start)
  (%line-ref buffer line))

(defun buffer-line-end (buffer line)
  "Index of the Linefeed that ends LINE, or the buffer size for the last line."
  (%check-line buffer line 'buffer-line-end)
  (if (< (1+ line) (buffer-line-count buffer))
      (1- (%line-ref buffer (1+ line)))
      (buffer-size buffer)))

(defun %rebuild-lines (buffer string)
  (let* ((count (1+ (count #\Linefeed string)))
         (capacity (max 16 (* 2 count)))
         (lines (make-array capacity :element-type 'fixnum :initial-element 0))
         (next 1))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Linefeed)
          do (setf (aref lines next) (1+ i))
             (incf next))
    (setf (%lines buffer) lines
          (%line-gap-start buffer) count
          (%line-gap-end buffer) capacity
          (%line-shift buffer) 0)))

(defun %note-changed-line (buffer line)
  (let ((current (%first-changed-line buffer)))
    (setf (%first-changed-line buffer) (if current (min current line) line))))

(defun buffer-take-first-changed-line (buffer)
  "First logical line invalidated since the last call, then forgets it; NIL when none."
  (prog1 (%first-changed-line buffer)
    (setf (%first-changed-line buffer) nil)))

;;; ------------------------------------------------------------------ marks

(defstruct (mark (:constructor %make-mark (buffer index gravity))
                 (:conc-name %mark-)
                 (:copier nil)
                 (:predicate nil))
  buffer index gravity)

(defun make-mark (buffer index &key (gravity :right))
  "Index that follows the edits; GRAVITY :RIGHT moves it past text inserted at it, :LEFT leaves it."
  (%check-index index (buffer-size buffer) 'make-mark)
  (let ((mark (%make-mark buffer index gravity)))
    (push mark (%marks buffer))
    mark))

(defun mark-index (mark)
  "Current index of MARK."
  (%mark-index mark))

(defun (setf mark-index) (index mark)
  "Move MARK to INDEX."
  (%check-index index (buffer-size (%mark-buffer mark)) 'mark-index)
  (setf (%mark-index mark) index))

(defun delete-mark (mark)
  "Detach MARK from its buffer; it stops being adjusted."
  (let ((buffer (%mark-buffer mark)))
    (setf (%marks buffer) (delete mark (%marks buffer) :test #'eq)))
  nil)

(defun %marks-insert (buffer index length)
  (dolist (mark (%marks buffer))
    (let ((at (%mark-index mark)))
      (when (or (> at index)
                (and (= at index) (eq (%mark-gravity mark) :right)))
        (setf (%mark-index mark) (+ at length))))))

(defun %marks-delete (buffer start end)
  (let ((removed (- end start)))
    (dolist (mark (%marks buffer))
      (let ((at (%mark-index mark)))
        (cond ((>= at end) (setf (%mark-index mark) (- at removed)))
              ((> at start) (setf (%mark-index mark) start)))))))

;;; ------------------------------------------------------------------ mutation

(defun %lines-insert (buffer index string length)
  (let ((line (%find-line buffer index))
        (breaks (count #\Linefeed string)))
    (%move-line-gap buffer (1+ line))
    (%ensure-line-gap buffer breaks)
    (incf (%line-shift buffer) length)
    ;; new starts land on the head side of the gap, which holds absolute indices
    (let ((lines (%lines buffer)))
      (loop for i from 0 below length
            when (char= (char string i) #\Linefeed)
            do (setf (aref lines (%line-gap-start buffer)) (+ index i 1))
               (incf (%line-gap-start buffer))))
    (%note-changed-line buffer line)))

(defun %lines-delete (buffer start end)
  (let ((first-line (%find-line buffer start))
        (last-line (%find-line buffer end)))
    (%move-line-gap buffer (1+ first-line))
    (incf (%line-gap-end buffer) (- last-line first-line))
    (decf (%line-shift buffer) (- end start))
    (%note-changed-line buffer first-line)))

(defun %apply-insert (buffer index string)
  (let ((length (length string)))
    (when (plusp length)
      (%lines-insert buffer index string length)
      (%move-gap buffer index)
      (%ensure-gap buffer length)
      (replace (%data buffer) string :start1 (%gap-start buffer))
      (incf (%gap-start buffer) length)
      (%marks-insert buffer index length)
      (incf (%revision buffer))
      (setf (buffer-modified-p buffer) t))))

(defun %apply-delete (buffer start end)
  (when (< start end)
    (%lines-delete buffer start end)
    (%move-gap buffer end)
    (decf (%gap-start buffer) (- end start))
    (%marks-delete buffer start end)
    (incf (%revision buffer))
    (setf (buffer-modified-p buffer) t)))

;;; ------------------------------------------------------------------ history

;;; TEXT grows by VECTOR-PUSH-EXTEND so a typing burst costs the characters typed and not the square of them.
(defstruct (edit (:constructor %make-edit (kind index text point-before point-after
                                           anchor-before))
                 (:copier nil)
                 (:predicate nil))
  kind index text point-before point-after anchor-before
  (reversed-p nil) (newlines-p nil) (removed nil))

(defun %edit-storage (string)
  (let ((storage (make-array (length string) :element-type 'character
                                             :adjustable t :fill-pointer (length string))))
    (replace storage string)
    storage))

(defun %edit-append (edit string)
  (when (find #\Linefeed string)
    (setf (edit-newlines-p edit) t))
  (loop for character across string
        do (vector-push-extend character (edit-text edit))))

(defun %new-edit (kind index string point-before point-after anchor-before)
  (let ((edit (%make-edit kind index (%edit-storage string) point-before point-after
                          anchor-before)))
    (setf (edit-newlines-p edit) (and (find #\Linefeed string) t))
    edit))

(defun %edit-length (edit)
  (length (edit-text edit)))

(defun %edit-string (edit)
  "Fresh simple string with the recorded characters in document order."
  (let ((text (coerce (edit-text edit) 'simple-string)))
    (if (edit-reversed-p edit) (nreverse text) text)))

(defun %mergeable-p (buffer top kind string)
  (and top
       (not (%boundary-p buffer))
       (eq (edit-kind top) kind)
       (not (edit-newlines-p top))
       (not (find #\Linefeed string))))

(defun %whitespace-crossed-p (top string)
  "True when what is being typed leaves whitespace for text."
  (let ((text (edit-text top)))
    (and (plusp (length text))
         (plusp (length string))
         (member (aref text (1- (length text))) '(#\Space #\Tab))
         (not (member (char string 0) '(#\Space #\Tab))))))

(defun %record-insert (buffer index string point-before point-after anchor-before)
  (setf (%redo buffer) '())
  (let ((top (first (%undo buffer))))
    (if (and (%mergeable-p buffer top :insert string)
             (not (edit-reversed-p top))
             (not (%whitespace-crossed-p top string))
             (= index (+ (edit-index top) (%edit-length top))))
        (progn (%edit-append top string)
               (setf (edit-point-after top) point-after))
        (push (%new-edit :insert index string point-before point-after anchor-before)
              (%undo buffer))))
  (setf (%boundary-p buffer) nil))

(defun %record-delete (buffer index removed point-before point-after anchor-before)
  (setf (%redo buffer) '())
  (let ((top (first (%undo buffer))))
    (cond ((and (%mergeable-p buffer top :delete removed)
                (= (+ index (length removed)) (edit-index top)))
           ;; backspace burst: the removed text precedes what is already recorded
           (unless (edit-reversed-p top)
             (setf (edit-text top) (%edit-storage (nreverse (%edit-string top)))
                   (edit-reversed-p top) t))
           (%edit-append top (reverse removed))
           (setf (edit-index top) index
                 (edit-point-after top) point-after))
          ((and (%mergeable-p buffer top :delete removed)
                (not (edit-reversed-p top))
                (= index (edit-index top)))
           ;; forward-delete burst: the removed text follows it
           (%edit-append top removed)
           (setf (edit-point-after top) point-after))
          (t
           (push (%new-edit :delete index removed point-before point-after anchor-before)
                 (%undo buffer)))))
  (setf (%boundary-p buffer) nil))

(defun %record-replace (buffer index string removed point-before point-after
                        anchor-before merge)
  (setf (%redo buffer) '())
  (let ((top (first (%undo buffer))))
    (if (and merge
             (%mergeable-p buffer top :replace string)
             (not (find #\Linefeed removed))
             (= index (+ (edit-index top) (%edit-length top))))
        ;; ponytail: the removed text is copied whole at every step, the square of the burst in its length.
        (progn (%edit-append top string)
               (setf (edit-removed top)
                     (concatenate 'string (edit-removed top) removed)
                     (edit-point-after top) point-after))
        (let ((edit (%new-edit :replace index string point-before point-after
                               anchor-before)))
          (setf (edit-removed edit) removed)
          (push edit (%undo buffer)))))
  (setf (%boundary-p buffer) (not merge)))

(defun buffer-undo-boundary (buffer)
  "Close the current typing burst so the next edit starts its own undo step."
  (setf (%boundary-p buffer) t)
  buffer)

(defun buffer-clear-history (buffer)
  "Forget every step there is to undo and to redo, leaving the text as it is."
  (setf (%undo buffer) '()
        (%redo buffer) '()
        (%boundary-p buffer) t)
  buffer)

(defun %trim-history (buffer size)
  "Let go of every step of BUFFER's history past the SIZE most recent ones."
  (if (plusp size)
      (let ((tail (nthcdr (1- size) (%undo buffer))))
        (when tail (setf (cdr tail) nil)))
      (setf (%undo buffer) '())))

(defun buffer-undo (buffer)
  "Undo one operation and return the caret index and the marking anchor to restore, or NIL when the history is empty."
  (%check-writable buffer)
  (let ((edit (pop (%undo buffer))))
    (when edit
      (ecase (edit-kind edit)
        (:insert (%apply-delete buffer (edit-index edit)
                                (+ (edit-index edit) (%edit-length edit))))
        (:delete (%apply-insert buffer (edit-index edit) (%edit-string edit)))
        (:replace (%apply-delete buffer (edit-index edit)
                                 (+ (edit-index edit) (%edit-length edit)))
                  (%apply-insert buffer (edit-index edit) (edit-removed edit))))
      (push edit (%redo buffer))
      (setf (%boundary-p buffer) t)
      (values (edit-point-before edit) (edit-anchor-before edit)))))

(defun buffer-redo (buffer)
  "Redo one undone operation and return the caret index to restore and NIL for the marking, which every operation drops, or NIL when there is nothing to redo."
  (%check-writable buffer)
  (let ((edit (pop (%redo buffer))))
    (when edit
      (ecase (edit-kind edit)
        (:insert (%apply-insert buffer (edit-index edit) (%edit-string edit)))
        (:delete (%apply-delete buffer (edit-index edit)
                                (+ (edit-index edit) (%edit-length edit))))
        (:replace (%apply-delete buffer (edit-index edit)
                                 (+ (edit-index edit) (length (edit-removed edit))))
                  (%apply-insert buffer (edit-index edit) (%edit-string edit))))
      (push edit (%undo buffer))
      (setf (%boundary-p buffer) t)
      (values (edit-point-after edit) nil))))

;;; ------------------------------------------------------------------ operations

(defun buffer-insert (buffer index string &key point anchor)
  "Insert STRING at INDEX as one undoable operation."
  (%check-writable buffer)
  (%check-index index (buffer-size buffer) 'buffer-insert)
  (let ((length (length string)))
    (when (plusp length)
      (%record-insert buffer index string (or point index) (+ index length) anchor)
      (%apply-insert buffer index string)))
  (%revision buffer))

(defun buffer-delete (buffer start end &key point anchor)
  "Delete [START, END) as one undoable operation and return the removed text."
  (%check-writable buffer)
  (%check-range buffer start end 'buffer-delete)
  (let ((removed (buffer-substring buffer start end)))
    (when (plusp (length removed))
      (%record-delete buffer start removed (or point start) start anchor)
      (%apply-delete buffer start end))
    removed))

(defun buffer-replace (buffer start end string &key point anchor merge)
  "Put STRING in place of [START, END) as ONE undoable operation, so a marking typed over comes back in a single undo."
  (%check-writable buffer)
  (%check-range buffer start end 'buffer-replace)
  (let ((removed (buffer-substring buffer start end)))
    (cond ((and (zerop (length removed)) (zerop (length string)))
           removed)
          (t
           (%record-replace buffer start string removed (or point start)
                            (+ start (length string)) anchor merge)
           (%apply-delete buffer start end)
           (%apply-insert buffer start string)
           removed))))

(defun buffer-set-text (buffer text)
  "Replace the whole document."
  (let* ((string (coerce text 'simple-string))
         (length (length string))
         (capacity (max 64 (* 2 length)))
         (data (make-string capacity)))
    (replace data string)
    (setf (%data buffer) data
          (%gap-start buffer) length
          (%gap-end buffer) capacity)
    (%rebuild-lines buffer string)
    (dolist (mark (%marks buffer))
      (setf (%mark-index mark) 0))
    (setf (%undo buffer) '()
          (%redo buffer) '()
          (%boundary-p buffer) t
          (buffer-modified-p buffer) nil
          (%first-changed-line buffer) 0)
    (incf (%generation buffer))
    (incf (%revision buffer)))
  (%revision buffer))

(defun make-buffer (&optional (text ""))
  "Fresh buffer holding TEXT, unmodified, with an empty history and nothing to relayout."
  (let ((buffer (make-instance 'buffer)))
    (buffer-set-text buffer text)
    (setf (%first-changed-line buffer) nil
          (%revision buffer) 0)
    buffer))
