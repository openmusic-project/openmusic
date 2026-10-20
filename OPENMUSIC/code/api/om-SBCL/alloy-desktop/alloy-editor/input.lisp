(in-package :alloy-editor)

;;; --------------------------------------------------------- the prompt and the text

(defun text-view-write-prompt (view string)
  "Put STRING at the end of the text, outside the history, and open the person's own text right after it."
  (let ((buffer (text-view-buffer view)))
    (setf (slot-value view 'input-prompt) (coerce string 'simple-string))
    (cond ((zerop (length string))
           (let ((mark (slot-value view 'input-start)))
             (when mark
               (delete-mark mark)
               (setf (slot-value view 'input-start) nil))))
          (t
           (%apply-insert buffer (buffer-size buffer) string)
           (let ((index (buffer-size buffer))
                 (mark (slot-value view 'input-start)))
             (if mark
                 (setf (mark-index mark) index)
                 ;; the person and the queue both put text in AT this index, and only one of them means to push the mark along.
                 (setf (slot-value view 'input-start)
                       (make-mark buffer index :gravity :left)))
             (buffer-clear-history buffer)
             (%view-move-mark view 'anchor nil)
             (%view-move-mark view 'caret index)))))
  (%view-relayout view)
  string)

(defun text-view-input (view)
  "What the person has written since the prompt, or NIL while the whole document is theirs."
  (let ((mark (slot-value view 'input-start))
        (buffer (text-view-buffer view)))
    (when mark
      (buffer-substring buffer (mark-index mark) (buffer-size buffer)))))

(defun text-view-set-input (view string)
  "Put STRING in place of what the person has written, as ONE step of the history."
  (%view-replace view (%view-input-floor view)
                 (buffer-size (text-view-buffer view))
                 (%view-text-to-insert view string))
  string)

;;; ------------------------------------------------------------ handing it over

(defun %form-complete-p (text)
  "True when TEXT closes every list it opens and leaves no quotation and no comment running."
  (let ((state (%fresh-lex-state))
        (from 0))
    (loop
      (let ((break (position #\Linefeed text :start from)))
        (setf state (%scan-line (subseq text from (or break (length text))) state nil))
        (if break (setf from (1+ break)) (return))))
    (and (eq (lex-state-kind state) :code)
         (null (lex-state-stack state)))))

(defun %view-hand-over (view text)
  "Close the line, take the person's own text out of their hands, open the next stretch after it and tell the host what was handed over."
  (%view-remember-input view text)
  (let ((buffer (text-view-buffer view))
        (mark (slot-value view 'input-start)))
    (%apply-insert buffer (buffer-size buffer) (string #\Linefeed))
    (setf (mark-index mark) (buffer-size buffer))
    ;; nothing stands before the mark for them to write after until the host writes
    ;; the next prompt, so queued text goes in right where the mark is
    (setf (slot-value view 'input-prompt) "")
    ;; what was handed over is no longer theirs to change, so there is nothing left
    ;; to take back: a step of the history reaching into it would edit it
    (buffer-clear-history buffer)
    (%view-move-mark view 'anchor nil)
    (%view-move-mark view 'caret (buffer-size buffer)))
  (let ((hook (text-view-accept view)))
    (when hook (funcall hook view text)))
  text)

(define-command :accept-input (view)
  "Hand the person's own text over and open the next stretch after it."
  (let ((text (text-view-input view)))
    (cond ((null text) (%view-insert-text view (string #\Linefeed)))
          ((zerop (length text)) nil)
          ((and (eq (text-view-mode view) :lisp) (not (%form-complete-p text)))
           (%view-insert-text view (string #\Linefeed)))
          (t (%view-hand-over view text)))))

;;; ------------------------------------------------------------------ the ring

(defun %view-remember-input (view text)
  "Keep TEXT at the end of the ring, unless the last entry already is it."
  (let ((ring (slot-value view 'history))
        (size (max 0 (text-view-history-size view))))
    (unless (or (zerop (length text))
                (zerop size)
                (and (plusp (fill-pointer ring))
                     (string= text (aref ring (1- (fill-pointer ring))))))
      ;; ponytail: the ring is a vector shifted down when it is full, which costs its own length once per entry.
      (loop while (<= size (fill-pointer ring))
            do (replace ring ring :start2 1)
               (decf (fill-pointer ring)))
      (vector-push-extend text ring)))
  (setf (slot-value view 'history-at) nil
        (slot-value view 'history-draft) nil))

(defun %view-walk-ring (view direction)
  "Put the entry DIRECTION steps along the ring in the person's own text."
  (let* ((ring (slot-value view 'history))
         (n (fill-pointer ring))
         (at (slot-value view 'history-at)))
    (unless (member (slot-value view 'last-command)
                    '(:history-previous :history-next))
      (setf at nil))
    (when (plusp n)
      (when (null at)
        (setf (slot-value view 'history-draft) (or (text-view-input view) ""))
        (setf at n))
      (let ((next (max 0 (min n (+ at direction)))))
        (setf (slot-value view 'history-at) next)
        (unless (eql next at)
          (text-view-set-input view (if (< next n)
                                        (aref ring next)
                                        (slot-value view 'history-draft))))))))

(define-command :history-previous (view)
  "Put the entry before the one showing in the person's own text."
  (%view-walk-ring view -1))

(define-command :history-next (view)
  "Put the entry after the one showing there, and what was being written at the end."
  (%view-walk-ring view 1))

;;; ------------------------------------------------------------------ the keys

(defparameter *input-keymap*
  (append '((:enter () :accept-input)
            (:kp-enter () :accept-input)
            (:up (:control) :history-previous)
            (:down (:control) :history-next))
          *editor-keymap*)
  "The keys of a view the person writes into the END of.")

;;; ------------------------------------------------------------ the candidates

(defparameter *completion-limit* 200
  "Candidates the editor's own source answers at most.")

(defun %sorted-unique (names)
  (let ((sorted (sort names #'string<)))
    (loop for tail on sorted
          unless (and (rest tail) (string= (first tail) (second tail)))
            collect (first tail))))

(defun %symbol-completions (prefix)
  "Names in the running image carrying on from PREFIX, in order and no more of them than the editor answers."
  (let ((wanted (string-upcase prefix))
        (names '()))
    (when (plusp (length wanted))
      (dolist (symbol (apropos-list prefix))
        (let ((name (symbol-name symbol)))
          (when (and (<= (length wanted) (length name))
                     (string= wanted name :end2 (length wanted)))
            (push (string-downcase name) names)))))
    (let ((sorted (%sorted-unique names)))
      (if (< *completion-limit* (length sorted))
          (subseq sorted 0 *completion-limit*)
          sorted))))

(defun %view-symbol-start (view)
  "Where the symbol under the caret opens."
  (let ((buffer (text-view-buffer view))
        (index (or (text-view-caret view) 0))
        (floor (%view-input-floor view)))
    (loop while (and (< floor index)
                     (not (member (buffer-char buffer (1- index)) *delimiters*)))
          do (decf index))
    index))

(define-command :complete (view)
  "Open the names the symbol under the caret carries on into."
  (let* ((start (%view-symbol-start view))
         (caret (or (text-view-caret view) 0))
         (prefix (buffer-substring (text-view-buffer view) start caret))
         (hook (text-view-completions view))
         (names (if hook
                    (funcall hook view prefix)
                    (when (eq (text-view-mode view) :lisp)
                      (%symbol-completions prefix)))))
    (cond ((null names)
           (%view-say view :not-found (format nil "Nothing carries on from ~S." prefix))
           nil)
          ((null (rest names))
           (%view-replace view start caret (first names))
           t)
          (t
           (setf (slot-value view 'completion) (%make-candidate-list start names names))
           t))))

(defun %view-step-candidates (view direction)
  (let ((state (slot-value view 'completion)))
    (when state
      (let ((n (length (candidates-candidates state))))
        (setf (candidates-index state)
              (max 0 (min (1- n) (+ (candidates-index state) direction))))))))

(define-command :completion-next (view)
  "Stand on the name after the one being stood on."
  (%view-step-candidates view 1))

(define-command :completion-previous (view)
  "Stand on the name before the one being stood on."
  (%view-step-candidates view -1))

(define-command :completion-insert (view)
  "Put the name being stood on in place of the prefix, as ONE step of the history, and take the list away."
  (let ((state (slot-value view 'completion)))
    (when state
      (let ((name (nth (candidates-index state) (candidates-candidates state)))
            (start (candidates-start state))
            (caret (or (text-view-caret view) 0)))
        (setf (slot-value view 'completion) nil)
        (%view-replace view start caret name)
        name))))

(define-command :completion-close (view)
  "Take the list away and change nothing."
  (setf (slot-value view 'completion) nil))
