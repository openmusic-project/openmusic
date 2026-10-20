(in-package :alloy-editor)

(defun append-text (view string)
  "Queue STRING to go in at the end of what VIEW shows."
  (let ((text (if (simple-string-p string) string (coerce string 'simple-string))))
    (when (plusp (length text))
      (bordeaux-threads:with-lock-held ((slot-value view 'pending-lock))
        (push text (slot-value view 'pending)))))
  string)

(defun %view-queued-text (view)
  "Everything queued, in the order it was queued, as one string."
  (let ((chunks (bordeaux-threads:with-lock-held ((slot-value view 'pending-lock))
                  (shiftf (slot-value view 'pending) nil))))
    (when chunks
      (setf chunks (nreverse chunks))
      (if (null (rest chunks))
          (first chunks)
          (with-output-to-string (out)
            (dolist (chunk chunks) (write-string chunk out)))))))

(defun %view-queue-index (view)
  "Where queued text goes in."
  (let ((mark (slot-value view 'input-start)))
    (if mark
        (max 0 (- (mark-index mark) (length (slot-value view 'input-prompt))))
        (buffer-size (text-view-buffer view)))))

(defun %view-trim-to-limit (view)
  "Take the OLDEST characters out until the document is no longer than the view is held to, never reaching into the stretch the person is writing in."
  (let ((limit (text-view-max-characters view))
        (buffer (text-view-buffer view)))
    (when limit
      (let ((cut (min (- (buffer-size buffer) limit) (%view-queue-index view))))
        (when (plusp cut)
          (%apply-delete buffer 0 cut))))))

(defun %view-scroll-to-end (view)
  "Put the last visual row of the document on the last whole row of the viewport."
  (let* ((layout (slot-value view 'layout))
         (buffer (layout-buffer layout))
         (line (1- (buffer-line-count buffer)))
         (row (1- (line-layout-row-count (layout-line layout line)))))
    (multiple-value-bind (top-line top-row)
        (%rows-advance layout line row (- (1- (%view-whole-rows view))))
      (setf (slot-value view 'top-line) top-line
            (slot-value view 'top-row) top-row))))

(defun %view-put-queued-text (view text)
  "Put TEXT in as ONE operation, before the stretch the person is writing in when there is one and at the very end when there is not."
  (let* ((buffer (text-view-buffer view))
         (mark (slot-value view 'input-start))
         (at (and mark (mark-index mark)))
         (index (%view-queue-index view))
         (following (and (slot-value view 'layout) (%view-last-line-visible-p view))))
    (%apply-insert buffer index text)
    ;; the mark does not move for text put in AT it, and the text always goes in at or
    ;; before it, so its new place is its old one plus what went in
    (when mark (setf (mark-index mark) (+ at (length text))))
    (%view-trim-to-limit view)
    (%view-relayout view)
    (when (and following (slot-value view 'layout))
      (%view-scroll-to-end view)
      (%view-relayout view))
    (let ((hook (text-view-after-change view)))
      (when hook (funcall hook view)))
    (length text)))

(defun drain-pending (view)
  "Put everything queued for VIEW in, on the thread that draws it, and answer how many characters went in."
  (when (slot-value view 'pending)
    (let ((text (%view-queued-text view)))
      (when text (%view-put-queued-text view text)))))

;;; ------------------------------------------------------------------ the stream

(defclass text-view-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((view :initarg :view :reader text-view-stream-view)
   (column :initform 0))
  (:documentation "A character output stream putting what is printed to it at the end of one view's text."))

(defun make-text-view-stream (view)
  "Fresh character output stream printing into VIEW."
  (make-instance 'text-view-stream :view view))

(defun %stream-note (stream string)
  "Keep how far along the line the last write left the pen."
  (let ((break (position #\Linefeed string :from-end t)))
    (if break
        (setf (slot-value stream 'column) (- (length string) break 1))
        (incf (slot-value stream 'column) (length string)))))

(defmethod trivial-gray-streams:stream-write-char ((stream text-view-stream) character)
  ;; ponytail: one queue entry per call, so a print that goes character by character queues that many strings.
  (let ((text (string character)))
    (append-text (text-view-stream-view stream) text)
    (%stream-note stream text))
  character)

(defmethod trivial-gray-streams:stream-write-string
    ((stream text-view-stream) string &optional (start 0) end)
  (let ((text (subseq string start end)))
    (append-text (text-view-stream-view stream) text)
    (%stream-note stream text))
  string)

(defmethod trivial-gray-streams:stream-line-column ((stream text-view-stream))
  (slot-value stream 'column))

(defmethod trivial-gray-streams:stream-force-output ((stream text-view-stream))
  (declare (ignore stream))
  nil)

(defmethod trivial-gray-streams:stream-finish-output ((stream text-view-stream))
  (declare (ignore stream))
  nil)
