(in-package :alloy-editor)

;;; ------------------------------------------------------------------ the registry

(defvar *commands* (make-hash-table :test 'eq)
  "Command name to the symbol naming the function that runs it.")

(defparameter *vertical-commands*
  '(:next-line :next-line-extending :previous-line :previous-line-extending
    :next-row :next-row-extending :previous-row :previous-row-extending
    :next-page :next-page-extending :previous-page :previous-page-extending)
  "The commands that may keep the target column; every other one forgets it.")

(defmacro define-command (name lambda-list &body body)
  "Define the command NAME as a function of a view and register it under that name."
  (let ((function (intern (format nil "COMMAND-~:@(~A~)" name))))
    `(progn
       (defun ,function ,lambda-list ,@body)
       (setf (gethash ,name *commands*) ',function)
       ,name)))

(defun find-command (name)
  "The symbol naming the function command NAME runs, or NIL when there is no such command."
  (gethash name *commands*))

(defun command-names ()
  "Every command name the editor answers to."
  (loop for name being the hash-keys of *commands* collect name))

;;; ------------------------------------------------------------------ the marks

(defun %point (view)
  "Where the caret is, or the start of the document while the view carries none."
  (or (text-view-caret view) 0))

(defun %view-marked-range (view)
  "(values LO HI) of what the view has marked, or NIL when it has marked nothing."
  (let ((anchor (text-view-anchor view))
        (caret (text-view-caret view)))
    (when (and anchor caret (/= anchor caret))
      (values (min anchor caret) (max anchor caret)))))

(defun %move-caret (view index extend)
  "Take the caret to INDEX, keeping the marking behind it when EXTEND and dropping it otherwise."
  (cond (extend
         (unless (text-view-anchor view)
           (%view-move-mark view 'anchor (%point view)))
         (%view-move-mark view 'caret index))
        (t
         (%view-move-mark view 'anchor nil)
         (%view-move-mark view 'caret index))))

;;; ------------------------------------------------------------------ the rows

(defun %view-whole-rows (view)
  "Visual rows the viewport holds WHOLE."
  (max 1 (floor (org.shirakumo.alloy:pxh (org.shirakumo.alloy:bounds view))
                (max 1.0 (slot-value view 'pitch)))))

(defun %view-caret-row-index (view)
  "Which of the rows on screen the caret sits on, counting from the top, or NIL when it sits on none of them."
  (let ((layout (slot-value view 'layout))
        (found nil))
    (when layout
      (multiple-value-bind (line row) (find-location layout (%point view))
        (%map-visible-rows view
                           (lambda (i cell-line cell-row top)
                             (declare (ignore top))
                             (when (and (null found)
                                        (eql cell-line line) (eql cell-row row))
                               (setf found i))))))
    found))

(defun %view-put-caret-row (view relative)
  "Put the top of the viewport where the caret's own row is the RELATIVEth on screen."
  (let ((layout (slot-value view 'layout)))
    (multiple-value-bind (line row) (find-location layout (%point view))
      (multiple-value-bind (top-line top-row) (%rows-advance layout line row (- relative))
        (setf (slot-value view 'top-line) top-line
              (slot-value view 'top-row) top-row)))))

;;; ------------------------------------------------------------------ the motions

(defun %goal-column (view index)
  "The column a vertical move aims at."
  (or (slot-value view 'goal-column)
      (setf (slot-value view 'goal-column)
            (- index (buffer-line-start (text-view-buffer view)
                                        (buffer-line-of-index (text-view-buffer view)
                                                              index))))))

(defun %line-step (view index direction)
  "INDEX one LOGICAL line down, or up, at the column being aimed at.  A :CLASSIC view aims at the column INDEX itself stands in, remembering none, and with no line that way goes to the start or the end of its own line, as the toolkit's text input does (alloy components/text-input.lisp, move-to :prev-line and :next-line)."
  (let* ((buffer (text-view-buffer view))
         (line (buffer-line-of-index buffer index))
         (classic (eq (text-view-gesture-policy view) :classic))
         (goal (if classic
                   (- index (buffer-line-start buffer line))
                   (%goal-column view index)))
         (target (+ line direction)))
    (cond ((and (<= 0 target) (< target (buffer-line-count buffer)))
           (min (+ (buffer-line-start buffer target) goal)
                (buffer-line-end buffer target)))
          ((not classic) index)
          ((minusp direction) (buffer-line-start buffer line))
          (t (buffer-line-end buffer line)))))

(defun %row-step (view index direction)
  "INDEX one VISUAL row down, or up, at the same place across the row in pixels."
  (let ((layout (slot-value view 'layout)))
    (multiple-value-bind (line row x) (find-location layout index)
      (multiple-value-bind (next-line next-row moved)
          (%rows-advance layout line row direction)
        (if (zerop moved) index (find-index layout next-line next-row x))))))

(defun %vertical-step (view index direction)
  "One step of the vertical move the view's family makes."
  (if (eq (text-view-gesture-policy view) :field)
      (%row-step view index direction)
      (%line-step view index direction)))

(defmacro define-motion (name (view) &body body)
  "Define the two commands one motion makes."
  (let ((motion (intern (format nil "MOTION-~:@(~A~)" name)))
        (extending (intern (format nil "~A-EXTENDING" (symbol-name name)) "KEYWORD")))
    `(progn
       (defun ,motion (,view) ,@body)
       (define-command ,name (view) (%move-caret view (,motion view) nil))
       (define-command ,extending (view) (%move-caret view (,motion view) t))
       ',name)))

(define-motion :forward-word (view)
  "The end of the word after the caret."
  (let* ((buffer (text-view-buffer view))
         (size (buffer-size buffer))
         (index (%point view)))
    (loop while (and (< index size) (not (alphanumericp (buffer-char buffer index))))
          do (incf index))
    (loop while (and (< index size) (alphanumericp (buffer-char buffer index)))
          do (incf index))
    index))

(define-motion :backward-word (view)
  "The start of the word before the caret."
  (let ((buffer (text-view-buffer view))
        (index (%point view)))
    (loop while (and (plusp index) (not (alphanumericp (buffer-char buffer (1- index)))))
          do (decf index))
    (loop while (and (plusp index) (alphanumericp (buffer-char buffer (1- index))))
          do (decf index))
    index))

(define-motion :forward-token (view)
  "The next character after the caret the toolkit's text input stops at, or the end of the document (alloy components/text-input.lisp, move-to :next-token and *token-stops*)."
  (let* ((buffer (text-view-buffer view))
         (size (buffer-size buffer)))
    (loop for i from (1+ (%point view)) below size
          when (find (buffer-char buffer i) org.shirakumo.alloy::*token-stops*)
            return i
          finally (return size))))

(define-motion :backward-token (view)
  "The last character before the caret the toolkit's text input stops at, or the start of the document (alloy components/text-input.lisp, move-to :prev-token)."
  (let ((buffer (text-view-buffer view)))
    (loop for i downfrom (1- (%point view)) above 0
          when (find (buffer-char buffer i) org.shirakumo.alloy::*token-stops*)
            return i
          finally (return 0))))

(define-motion :next-line (view)
  "One logical line down, at the column being aimed at."
  (%line-step view (%point view) 1))

(define-motion :previous-line (view)
  "One logical line up, at the column being aimed at."
  (%line-step view (%point view) -1))

(define-motion :next-row (view)
  "One visual row down, at the same place across the row."
  (%row-step view (%point view) 1))

(define-motion :previous-row (view)
  "One visual row up, at the same place across the row."
  (%row-step view (%point view) -1))

(define-motion :beginning-of-line (view)
  "The first character of the logical line the caret is on, or."
  (let* ((buffer (text-view-buffer view))
         (line (buffer-line-of-index buffer (%point view)))
         (start (buffer-line-start buffer line))
         (floor (%view-input-floor view)))
    (if (<= start floor (buffer-line-end buffer line)) floor start)))

(define-motion :end-of-line (view)
  "The end of the logical line the caret is on, before its terminator."
  (let ((buffer (text-view-buffer view)))
    (buffer-line-end buffer (buffer-line-of-index buffer (%point view)))))

(define-motion :beginning-of-row (view)
  "The first character of the visual row the caret is on."
  (let* ((layout (slot-value view 'layout))
         (buffer (layout-buffer layout))
         (index (%point view)))
    (multiple-value-bind (line row) (find-location layout index)
      (+ (buffer-line-start buffer line)
         (line-layout-row-start (layout-line layout line) row)))))

(define-motion :end-of-row (view)
  "One past the last character of the visual row the caret is on."
  (let* ((layout (slot-value view 'layout))
         (buffer (layout-buffer layout))
         (index (%point view)))
    (multiple-value-bind (line row) (find-location layout index)
      (+ (buffer-line-start buffer line)
         (line-layout-row-end (layout-line layout line) row)))))

(define-motion :beginning-of-buffer (view)
  "The start of the document."
  (declare (ignore view))
  0)

(define-motion :end-of-buffer (view)
  "The end of the document."
  (buffer-size (text-view-buffer view)))

;;; A character motion is not one of those two.

(defun %character-move (view direction extend)
  (let ((size (buffer-size (text-view-buffer view)))
        (index (%point view)))
    (multiple-value-bind (lo hi) (%view-marked-range view)
      (if (and lo (not extend) (member (text-view-gesture-policy view) '(:field :classic)))
          (%move-caret view (if (plusp direction) hi lo) nil)
          (%move-caret view (max 0 (min size (+ index direction))) extend)))))

(define-command :previous-character (view)
  "One character back, whatever is marked, which the move drops rather than collapses (alloy components/text-input.lisp, move-to :prev-char)."
  (%move-caret view (max 0 (1- (%point view))) nil))

(define-command :forward-character (view) (%character-move view 1 nil))
(define-command :forward-character-extending (view) (%character-move view 1 t))
(define-command :backward-character (view) (%character-move view -1 nil))
(define-command :backward-character-extending (view) (%character-move view -1 t))

(defun %page-move (view direction extend)
  "A page is the whole rows the viewport holds, less one."
  (let ((relative (or (%view-caret-row-index view) 0))
        (index (%point view)))
    (dotimes (step (1- (%view-whole-rows view)))
      (setf index (%vertical-step view index direction)))
    (%move-caret view index extend)
    (%view-put-caret-row view relative)))

(define-command :next-page (view) (%page-move view 1 nil))
(define-command :next-page-extending (view) (%page-move view 1 t))
(define-command :previous-page (view) (%page-move view -1 nil))
(define-command :previous-page-extending (view) (%page-move view -1 t))

(define-command :select-all (view)
  "Mark the whole document, the caret at its START and the anchor at its end; a :CLASSIC view holds the anchor at the start and puts the caret at the end."
  (let ((size (buffer-size (text-view-buffer view))))
    (cond ((eq (text-view-gesture-policy view) :classic)
           (%view-move-mark view 'anchor 0)
           (%view-move-mark view 'caret size))
          (t
           (%view-move-mark view 'anchor size)
           (%view-move-mark view 'caret 0)))))

;;; ------------------------------------------------------------------ the text

(defun %normalise-newlines (string)
  "The terminator of another system turned into the one the buffer keeps."
  (if (find #\Return string)
      (with-output-to-string (out)
        (loop for i from 0 below (length string)
              for character = (char string i)
              do (cond ((char/= character #\Return) (write-char character out))
                       ((and (< (1+ i) (length string))
                             (char= (char string (1+ i)) #\Linefeed)))
                       (t (write-char #\Linefeed out)))))
      string))

(defun %view-text-to-insert (view string)
  (let ((text (%normalise-newlines string)))
    (if (text-view-single-line-p view)
        (remove-if (lambda (character) (char= character #\Linefeed)) text)
        text)))

(defun %view-fitting-text (view start end string)
  "As much of STRING as the characters the view is held to leave room for, once [START, END) has gone."
  (let ((limit (text-view-max-characters view)))
    (if (null limit)
        string
        (let ((room (max 0 (- limit (- (buffer-size (text-view-buffer view))
                                       (- end start))))))
          (cond ((<= (length string) room) string)
                (t (%view-say view :limit
                              (format nil "Only ~D more character~:P fit." room))
                   (subseq string 0 room)))))))

(defun %view-replace (view start end string &key merge)
  "Put STRING in place of [START, END) as ONE step of the history and leave the caret after it with nothing marked."
  (let ((buffer (text-view-buffer view)))
    (%check-writable buffer)
    (%check-user-range view start end)
    (let ((text (%view-fitting-text view start end string))
          (caret (text-view-caret view))
          (anchor (text-view-anchor view)))
      (cond ((< start end)
             (buffer-replace buffer start end text :point caret :anchor anchor
                                                   :merge merge))
            ((plusp (length text))
             (buffer-insert buffer start text :point caret :anchor anchor))
            (t (return-from %view-replace nil)))
      (%view-move-mark view 'anchor nil)
      (%view-move-mark view 'caret (+ start (length text))))))

(defun %view-insert-text (view string)
  "Put STRING where the caret is, or in place of what is marked: one step of the history, or, in a :CLASSIC view, the marking taken out and the text put in as two (alloy components/text-input.lisp, handle of text-event for a text-input-component)."
  (let ((text (%view-text-to-insert view string)))
    (multiple-value-bind (lo hi) (%view-marked-range view)
      (cond ((and lo (eq (text-view-gesture-policy view) :classic))
             (%view-delete-range view lo hi)
             (%view-replace view lo lo text))
            (lo (%view-replace view lo hi text))
            (t (%view-replace view (%point view) (%point view) text))))))

(defun %view-overwrite-text (view string)
  "Put STRING over the characters after the caret, one for one, stopping at the end of the line; a :CLASSIC view goes over the line's end as over any character (alloy components/text-input.lisp, insert-text in :replace mode)."
  (let* ((text (%view-text-to-insert view string))
         (buffer (text-view-buffer view))
         (size (buffer-size buffer))
         (start (%point view))
         (end start)
         (classic (eq (text-view-gesture-policy view) :classic)))
    (loop repeat (length text)
          while (and (< end size)
                     (or classic (char/= (buffer-char buffer end) #\Linefeed)))
          do (incf end))
    (%view-replace view start end text :merge t)))

(define-command :insert-text (view string)
  "Put STRING where the caret is, or in place of what is marked."
  (if (and (text-view-overwrite-p view) (null (%view-marked-range view)))
      (%view-overwrite-text view string)
      (%view-insert-text view string)))

(define-command :toggle-overwrite (view)
  "Turn overwrite on, or off again."
  (setf (text-view-overwrite-p view) (not (text-view-overwrite-p view))))

(define-command :new-line (view)
  "Open a line; a view held to one line hands its text over instead and takes no terminator."
  (if (text-view-single-line-p view)
      (let ((hook (text-view-accept view)))
        (when hook (funcall hook view (text-view-text view))))
      (%view-insert-text view (string #\Linefeed))))

(define-command :insert-tab (view)
  "Put a Tab where the caret is."
  (%view-insert-text view (string #\Tab)))

(defun %view-delete-range (view start end)
  (%check-writable (text-view-buffer view))
  (%check-user-range view start end)
  (when (< start end)
    (buffer-delete (text-view-buffer view) start end
                   :point (text-view-caret view) :anchor (text-view-anchor view))
    (%view-move-mark view 'anchor nil)
    (%view-move-mark view 'caret start)))

(define-command :delete-previous-character (view)
  "Take out what is marked, or the character before the caret."
  (multiple-value-bind (lo hi) (%view-marked-range view)
    (if lo
        (%view-delete-range view lo hi)
        (%view-delete-range view (max (%view-input-floor view) (1- (%point view)))
                            (%point view)))))

(define-command :delete-next-character (view)
  "Take out what is marked, or the character after the caret."
  (multiple-value-bind (lo hi) (%view-marked-range view)
    (if lo
        (%view-delete-range view lo hi)
        (%view-delete-range view (%point view)
                            (min (buffer-size (text-view-buffer view))
                                 (1+ (%point view)))))))

(define-command :delete-previous-word (view)
  "Take out what is marked, or the word before the caret."
  (multiple-value-bind (lo hi) (%view-marked-range view)
    (if lo
        (%view-delete-range view lo hi)
        (%view-delete-range view (motion-backward-word view) (%point view)))))

;;; ------------------------------------------------------------------ the clipboard

(defvar *clipboard* nil
  "Text the editor's own store holds.")

(defun store-clipboard-text (view text)
  "Put TEXT in the editor's own store."
  (declare (ignore view))
  (setf *clipboard* text))

(defun stored-clipboard-text (view)
  "What the editor's own store holds."
  (declare (ignore view))
  *clipboard*)

;;; The toolkit declares a writer for its clipboard (alloy ui.lisp.
(defvar *clipboard-writer* 'store-clipboard-text
  "Function of a view and a string, called with what a copy, a cut or a kill puts on the clipboard.")

(defvar *clipboard-reader* 'stored-clipboard-text
  "Function of a view answering the text a paste puts in.")

(defgeneric clipboard-text (view)
  (:documentation "What a paste into VIEW would put in; a view asks *CLIPBOARD-READER*.")
  (:method ((view t))
    (funcall *clipboard-reader* view)))

(defgeneric (setf clipboard-text) (text view)
  (:documentation "Put TEXT on the clipboard VIEW copies to; a view hands it to *CLIPBOARD-WRITER*.")
  (:method (text (view t))
    (funcall *clipboard-writer* view text)
    text))

(define-command :copy (view)
  "Put what is marked on the clipboard, leaving the text as it is."
  (multiple-value-bind (lo hi) (%view-marked-range view)
    (when lo
      (setf (clipboard-text view) (buffer-substring (text-view-buffer view) lo hi)))))

(define-command :cut (view)
  "Put what is marked on the clipboard and take it out of the text."
  ;; the clipboard is asked LAST: a cut that is refused must leave what the person
  ;; copied before it standing
  (%check-writable (text-view-buffer view))
  (multiple-value-bind (lo hi) (%view-marked-range view)
    (when lo
      (%check-user-range view lo hi)
      (setf (clipboard-text view) (buffer-substring (text-view-buffer view) lo hi))
      (%view-delete-range view lo hi))))

(define-command :paste (view)
  "Put what the clipboard holds where the caret is, or in place of what is marked.  A :CLASSIC view drops every Return of it first, so a Return before a Linefeed leaves the Linefeed and one standing alone leaves nothing."
  (let ((text (clipboard-text view)))
    (when (and (stringp text) (eq (text-view-gesture-policy view) :classic))
      (setf text (remove #\Return text)))
    (when (and (stringp text) (plusp (length text)))
      (%view-insert-text view text))))

(define-command :kill-line (view)
  "Take the rest of the line, and the terminator alone when the caret is already at the end of it, which joins the line after it."
  (%check-writable (text-view-buffer view))
  (let* ((buffer (text-view-buffer view))
         (index (%point view))
         (line-end (buffer-line-end buffer (buffer-line-of-index buffer index)))
         (end (if (< index line-end)
                  line-end
                  (min (buffer-size buffer) (1+ line-end))))
         (killed (buffer-substring buffer index end))
         (piling (eq (slot-value view 'last-command) :kill-line)))
    (%check-user-range view index end)
    (when (plusp (length killed))
      (setf (clipboard-text view)
            (if piling
                (concatenate 'string (or (clipboard-text view) "") killed)
                killed))
      (%view-delete-range view index end))))

;;; ------------------------------------------------------------------ the history

(defun %view-after-history-step (view caret point anchor)
  "Put the marks where a step of the history leaves them: back where they stood before it, or, in a :CLASSIC view, the caret where it was before the step and no further than the end, and nothing marked."
  (cond ((eq (text-view-gesture-policy view) :classic)
         (%view-move-mark view 'anchor nil)
         (%view-move-mark view 'caret (min (or caret 0) (buffer-size (text-view-buffer view)))))
        (t
         (%view-move-mark view 'anchor anchor)
         (%view-move-mark view 'caret point))))

(define-command :undo (view)
  "Undo one step, putting the caret and the marking back where they were before it."
  (let ((caret (text-view-caret view)))
    (multiple-value-bind (point anchor) (buffer-undo (text-view-buffer view))
      (when point (%view-after-history-step view caret point anchor)))))

(define-command :redo (view)
  "Redo one undone step."
  (let ((caret (text-view-caret view)))
    (multiple-value-bind (point anchor) (buffer-redo (text-view-buffer view))
      (when point (%view-after-history-step view caret point anchor)))))

(define-command :leave (view)
  "Hand the keyboard focus back to what holds the view, changing nothing in the text."
  (org.shirakumo.alloy:exit view))

;;; ------------------------------------------------------------------ the search

(defun %view-find (view string backward case-sensitive wrap)
  "Mark the next STRING from the caret and answer (START END), or tell the host that there is none and leave every mark where it stands."
  (let ((buffer (text-view-buffer view))
        (from (%point view)))
    (multiple-value-bind (lo hi)
        (if backward
            (find-backward buffer string from :case-sensitive case-sensitive :wrap wrap)
            (find-forward buffer string from :case-sensitive case-sensitive :wrap wrap))
      (cond ((null lo)
             (%view-say view :not-found (format nil "~S is not in the text." string))
             nil)
            (t
             (%view-move-mark view 'anchor (if backward hi lo))
             (%view-move-mark view 'caret (if backward lo hi))
             (list lo hi))))))

(define-command :find-next (view string &key case-sensitive wrap)
  "Mark the next STRING at or after the caret; repeating takes the one after that."
  (%view-find view string nil case-sensitive wrap))

(define-command :find-previous (view string &key case-sensitive wrap)
  "Mark the last STRING opening before the caret; repeating takes the one before that."
  (%view-find view string t case-sensitive wrap))

(define-command :replace-found (view string)
  "Put STRING in place of what is marked, as one step of the history, and leave the caret after it."
  (multiple-value-bind (lo hi) (%view-marked-range view)
    (when lo (%view-replace view lo hi (%view-text-to-insert view string)))))

(defun %occurrences (buffer string case-sensitive)
  "((START . END) ...) of every STRING in BUFFER, in order and never overlapping."
  (let ((hits '())
        (at 0))
    (loop (multiple-value-bind (lo hi)
              (find-forward buffer string at :case-sensitive case-sensitive)
            (unless lo (return))
            (push (cons lo hi) hits)
            (setf at hi)))
    (nreverse hits)))

(defun %spliced (buffer hits text from)
  "The stretch of BUFFER from FROM to the end of the last of HITS, with TEXT standing in the place of each of them."
  (with-output-to-string (out)
    (let ((at from))
      (dolist (hit hits)
        (write-string (buffer-substring buffer at (car hit)) out)
        (write-string text out)
        (setf at (cdr hit))))))

(define-command :replace-all (view string replacement &key case-sensitive)
  "Put REPLACEMENT in place of every STRING in the document and answer how many there were."
  (let* ((buffer (text-view-buffer view))
         (text (%view-text-to-insert view replacement))
         (hits (%occurrences buffer string case-sensitive)))
    (cond ((null hits) 0)
          (t
           (let ((from (car (first hits)))
                 (to (cdr (car (last hits)))))
             (%view-replace view from to (%spliced buffer hits text from))
             (length hits))))))

;;; ------------------------------------------------------------------ the whole text

(defun text-view-text (view)
  "The whole document VIEW shows."
  (buffer-text (text-view-buffer view)))

(defun %view-changed (view)
  "Tell the host the text changed; a :CLASSIC view first lets go of the steps of its history past the ones it keeps."
  (when (eq (text-view-gesture-policy view) :classic)
    (%trim-history (text-view-buffer view) *classic-history-size*))
  (let ((hook (text-view-after-change view)))
    (when hook (funcall hook view))))

(defun (setf text-view-text) (text view)
  "Put TEXT in place of the whole document as ONE step of the history, which an undo takes back.  A :CLASSIC view tells the host of it as of any other write."
  (let* ((buffer (text-view-buffer view))
         (revision (buffer-revision buffer)))
    (%view-replace view 0 (buffer-size buffer) (%view-text-to-insert view text))
    (when (and (eq (text-view-gesture-policy view) :classic)
               (/= revision (buffer-revision buffer)))
      (%view-changed view)))
  text)

;;; ------------------------------------------------------------- following the caret

(defun %view-follow-caret (view)
  "Travel the LEAST that puts the caret back inside the viewport; a :CLASSIC view travels by the pixel and keeps a margin."
  (let ((layout (slot-value view 'layout))
        (caret (text-view-caret view)))
    (when (eq (text-view-gesture-policy view) :classic)
      (%classic-follow view)
      (return-from %view-follow-caret))
    (when (and layout caret)
      (%view-clamp-top view)
      (multiple-value-bind (line row x) (find-location layout caret)
        (let ((whole (%view-whole-rows view))
              (top-line (slot-value view 'top-line))
              (top-row (slot-value view 'top-row)))
          (cond ((or (< line top-line) (and (= line top-line) (< row top-row)))
                 (setf (slot-value view 'top-line) line
                       (slot-value view 'top-row) row))
                ((null (%rows-between layout top-line top-row line row (1- whole)))
                 (multiple-value-bind (new-line new-row)
                     (%rows-advance layout line row (- (1- whole)))
                   (setf (slot-value view 'top-line) new-line
                         (slot-value view 'top-row) new-row)))))
        (unless (text-view-wrap-p view)
          (let ((left (slot-value view 'x-offset))
                (width (%view-text-width view))
                (bar (%view-caret-width view)))
            (cond ((< x left)
                   (setf (text-view-scroll-x view) x))
                  ((< (+ left width) (+ x bar))
                   (setf (text-view-scroll-x view) (- (+ x bar) width))))))))))

;;; ------------------------------------------------------------------ the dispatcher

(defun run-command (view name &rest arguments)
  "Run the command NAME on VIEW and answer true when there is one, and as a second value whatever that command answered."
  (let ((function (gethash name *commands*))
        (buffer (text-view-buffer view)))
    (when (and function buffer (slot-value view 'layout))
      ;; a :CLASSIC view keeps every write as a step of its own, a burst included
      (unless (and (eq name (slot-value view 'last-command))
                   (not (eq (text-view-gesture-policy view) :classic)))
        (buffer-undo-boundary buffer))
      (unless (member name *vertical-commands*)
        (setf (slot-value view 'goal-column) nil))
      (let ((revision (buffer-revision buffer))
            (answer nil))
        (handler-case (setf answer (apply function view arguments))
          (editor-error (condition)
            (%view-say view :error (editor-error-text condition))))
        (setf (slot-value view 'last-command) name)
        (%view-follow-caret view)
        (%view-relayout view)
        (unless (= revision (buffer-revision buffer))
          (%view-changed view))
        (values t answer)))))

;;; A view that marks its whole document when it takes the focus does it HERE, where the toolkit says the focus arrived, and only when it ARRIVES.
(defmethod (setf org.shirakumo.alloy:focus) :after (value (view text-view))
  (let ((held (slot-value view 'focused-p)))
    (setf (slot-value view 'focused-p) (and value t))
    (when (and value (not held) (text-view-select-all-on-focus-p view))
      (run-command view :select-all))))

;;; ------------------------------------------------------------------ the keymaps

(defparameter *editor-keymap*
  '((:right () :forward-character)
    (:left () :backward-character)
    (:down () :next-line)
    (:up () :previous-line)
    (:right (:shift) :forward-character-extending)
    (:left (:shift) :backward-character-extending)
    (:down (:shift) :next-line-extending)
    (:up (:shift) :previous-line-extending)
    (:right (:control) :forward-word)
    (:left (:control) :backward-word)
    (:right (:control :shift) :forward-word-extending)
    (:left (:control :shift) :backward-word-extending)
    (:home () :beginning-of-line)
    (:end () :end-of-line)
    (:home (:shift) :beginning-of-line-extending)
    (:end (:shift) :end-of-line-extending)
    (:home (:control) :beginning-of-buffer)
    (:end (:control) :end-of-buffer)
    (:home (:control :shift) :beginning-of-buffer-extending)
    (:end (:control :shift) :end-of-buffer-extending)
    (:page-up () :previous-page)
    (:page-down () :next-page)
    (:page-up (:shift) :previous-page-extending)
    (:page-down (:shift) :next-page-extending)
    (:backspace () :delete-previous-character)
    (:delete () :delete-next-character)
    (:backspace (:control) :delete-previous-word)
    (:delete (:control) :kill-line)
    (:insert () :toggle-overwrite)
    (:insert (:shift) :paste)
    (:insert (:control) :copy)
    (:a (:control) :select-all)
    (:c (:control) :copy)
    (:x (:control) :cut)
    (:v (:control) :paste)
    (:z (:control) :undo)
    (:enter () :new-line)
    (:kp-enter () :new-line)
    (:tab () :insert-tab))
  "The keys of a pane that holds a document.")

(defparameter *field-keymap*
  '((:right () :forward-character)
    (:left () :backward-character)
    (:down () :next-row)
    (:up () :previous-row)
    (:right (:shift) :forward-character-extending)
    (:left (:shift) :backward-character-extending)
    (:down (:shift) :next-row-extending)
    (:up (:shift) :previous-row-extending)
    (:right (:control) :forward-word)
    (:left (:control) :backward-word)
    (:right (:control :shift) :forward-word-extending)
    (:left (:control :shift) :backward-word-extending)
    (:home () :beginning-of-row)
    (:end () :end-of-row)
    (:home (:shift) :beginning-of-row-extending)
    (:end (:shift) :end-of-row-extending)
    (:home (:control) :beginning-of-buffer)
    (:end (:control) :end-of-buffer)
    (:page-up () :previous-page)
    (:page-down () :next-page)
    (:backspace () :delete-previous-character)
    (:delete () :delete-next-character)
    (:backspace (:control) :delete-previous-word)
    (:delete (:control) :kill-line)
    (:insert (:shift) :paste)
    (:insert (:control) :copy)
    (:a (:control) :select-all)
    (:c (:control) :copy)
    (:x (:control) :cut)
    (:v (:control) :paste)
    (:z (:control) :undo)
    (:enter () :new-line)
    (:kp-enter () :new-line))
  "The keys of a pane that holds a field.")

(defparameter *held-modifiers* '(:shift :control :alt :super)
  "The modifiers a keymap tells keys apart by; a lock is not one of them.")

(defun %held (modifiers)
  (remove-if-not (lambda (modifier) (member modifier *held-modifiers*)) modifiers))

(defun %same-modifiers-p (wanted held)
  (and (= (length wanted) (length held))
       (every (lambda (modifier) (member modifier held)) wanted)))

(defun keymap-command (keymap key modifiers)
  "The command KEYMAP binds KEY held with MODIFIERS to, or NIL when it binds none."
  (let ((held (%held modifiers)))
    (loop for (entry-key entry-modifiers command) in keymap
          when (and (eq entry-key key) (%same-modifiers-p entry-modifiers held))
          return command)))

(defun %view-keymap (view)
  (or (text-view-keymap view)
      (case (text-view-gesture-policy view)
        (:field *field-keymap*)
        (:classic (if (text-view-single-line-p view) *classic-field-keymap* *classic-keymap*))
        (t *editor-keymap*))))

;;; ------------------------------------------------------------------ the keys

(defmethod org.shirakumo.alloy:handle ((event org.shirakumo.alloy:key-down)
                                       (view text-view))
  (let ((command (or (keymap-command (%view-completion-keymap view)
                                     (org.shirakumo.alloy:key event)
                                     (org.shirakumo.alloy:modifiers event))
                     (keymap-command (mode-keymap (text-view-mode view))
                                     (org.shirakumo.alloy:key event)
                                     (org.shirakumo.alloy:modifiers event))
                     (keymap-command (%view-keymap view)
                                     (org.shirakumo.alloy:key event)
                                     (org.shirakumo.alloy:modifiers event)))))
    (cond ((and command (run-command view command)))
          ((let ((hook (text-view-unhandled-key view)))
             (and hook (funcall hook view event))))
          ;; a :CLASSIC view holding the strong focus takes every key it was handed
          ((and (eq (text-view-gesture-policy view) :classic)
                (eq :strong (org.shirakumo.alloy:focus view))))
          (t (org.shirakumo.alloy:decline)))))

(defmethod org.shirakumo.alloy:handle ((event org.shirakumo.alloy:text-event)
                                       (view text-view))
  (let ((text (org.shirakumo.alloy:text event)))
    (if (and (plusp (length text)) (run-command view :insert-text text))
        t
        (org.shirakumo.alloy:decline))))

;;; The toolkit turns some of the keys these tables bind into events of its own and delivers them BEFORE the key itself, so an arrow would move the caret twice and Tab would take the focus away before the key that types it ever arrived.
(macrolet ((consume (&rest classes)
             `(progn
                ,@(loop for class in classes
                        collect `(defmethod org.shirakumo.alloy:handle
                                     ((event ,class) (view text-view))
                                   (declare (ignore event view))
                                   t)))))
  (consume org.shirakumo.alloy:focus-up
           org.shirakumo.alloy:focus-down
           org.shirakumo.alloy:focus-left
           org.shirakumo.alloy:focus-right
           org.shirakumo.alloy:focus-next
           org.shirakumo.alloy:focus-prev
           org.shirakumo.alloy:activate
           ;; the key that closes a list of candidates also arrives as the toolkit's
           ;; own way of giving the focus back to the parent: a text pane keeps it
           org.shirakumo.alloy:exit
           org.shirakumo.alloy:copy-event
           org.shirakumo.alloy:cut-event
           org.shirakumo.alloy:paste-event))
