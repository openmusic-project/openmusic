(in-package :oa)

(defvar *find-strings*       nil)
(defvar *replace-strings*    nil)
(defvar *search-targets*     nil)
(defvar *search-directories* nil)

;;; Find / Replace over om-text-edit-view (SBCL+Alloy port of the ANVITA editor
;;; find-dialog / find-replace-dialog). Search uses cl:search on the pane's value;
;;; a hit is shown as a selection (anchor=start, cursor=end). Non-modal: the editor
;;; stays interactive while the dialog is open.

(defun %pane-find-backward (pane target end)
  "Search PANE's text for TARGET ending before END (backward). On a hit, select the
match and return its start; else NIL."
  (let* ((text (and pane (alloy:value pane)))
         (pos  (and text target (plusp (length target))
                    (search target text :from-end t :test #'char-equal
                            :end2 (min (max end 0) (length text))))))
    (when pos
      (let ((cursor (alloy:cursor pane)))
        (setf (alloy:anchor cursor) pos)
        (alloy:move-to (+ pos (length target)) cursor))
      (alloy:mark-for-render pane)
      pos)))

(defun %find-next (window find-text &optional backward)
  "Find FIND-TEXT in WINDOW's editor pane from the cursor. Returns the match start
or NIL. It does NOT wrap around: the LW editor search stops at the buffer end in
both directions, and reaching it is the signal the caller reports
(om-LW lw-lisp-tools/editor-find-dialogs.lisp: find-replace-callback and
do-editor-find-replace-callback)."
  (let* ((pane (edit-pane window))
         (text (and pane (alloy:value pane))))
    (when (and pane text (plusp (length find-text)))
      (let* ((cursor (alloy:cursor pane))
             (pos    (alloy:pos cursor)))
        (if backward
            (%pane-find-backward pane find-text (max 0 (1- pos)))
            (%pane-find-and-select pane find-text pos))))))

(defun %case-matched-replacement (match replacement)
  "REPLACEMENT adjusted to MATCH's case, as the LW/Emacs case-replace does
(measured live): a replacement that already carries an uppercase letter is
literal; otherwise an all-uppercase match upcases it and a capitalized match
capitalizes it. Case-insensitive search without this silently downcases the
user's text on every replacement."
  (cond ((some #'upper-case-p replacement) replacement)
        ((and (some #'upper-case-p match) (notany #'lower-case-p match))
         (string-upcase replacement))
        ((and (some #'upper-case-p match) (string= match (string-capitalize match)))
         (string-capitalize replacement))
        (t replacement)))

(defun %replace-current (window find-text replace-text)
  "If the editor pane's selection matches FIND-TEXT, replace it with REPLACE-TEXT
and leave the cursor after the replacement. Returns T on replacement."
  (let* ((pane (edit-pane window))
         (sel  (and pane (%selection-string pane))))
    (when (and sel (string-equal sel find-text))
      (let* ((cursor (alloy:cursor pane))
             (anchor (alloy:anchor cursor))
             (p1     (min anchor (alloy:pos cursor)))
             (p2     (max anchor (alloy:pos cursor)))
             (text   (alloy:value pane))
             (new    (%case-matched-replacement sel replace-text)))
        (setf (alloy:value pane)
              (concatenate 'string (subseq text 0 p1) new (subseq text p2)))
        (setf (alloy:anchor cursor) nil)
        (alloy:move-to (+ p1 (length new)) cursor)
        (setf (modified-p window) t)
        (alloy:mark-for-render pane)
        t))))

(defun %replace-all (window find-text replace-text &optional backward)
  "Replace every occurrence of FIND-TEXT from the caret to the end of the buffer
-- or to its start, BACKWARD -- and leave the caret after the last replacement.
The caret start and the direction are what vanilla passes to its replace command
(om-LW lw-lisp-tools/editor-find-dialogs.lisp: the query-replace-string of
do-editor-find-replace-callback); both measured live. Returns
the number of replacements."
  (let* ((pane (edit-pane window))
         (text (and pane (alloy:value pane))))
    (if (and pane text (plusp (length find-text)))
        (let* ((n      (length text))
               (flen   (length find-text))
               (caret  (min (max 0 (alloy:pos (alloy:cursor pane))) n))
               (lo     (if backward 0 caret))
               (hi     (if backward caret n))
               (out    (make-string-output-stream))
               (count  0)
               (delta  0)
               (new-caret nil))
          (write-string (subseq text 0 lo) out)
          (let ((i lo))
            (loop while (< i hi) do
              (let ((hit (search find-text text :start2 i :end2 hi :test #'char-equal)))
                (cond (hit
                       (write-string (subseq text i hit) out)
                       (let ((new (%case-matched-replacement
                                   (subseq text hit (+ hit flen)) replace-text)))
                         (write-string new out)
                         ;; the caret lands after the LAST replacement made in the
                         ;; direction of travel: rightmost forward, leftmost backward
                         (when (or (not backward) (null new-caret))
                           (setf new-caret (+ hit delta (length new))))
                         (incf delta (- (length new) flen)))
                       (setf i (+ hit flen))
                       (incf count))
                      (t (write-string (subseq text i hi) out)
                         (setf i hi))))))
          (write-string (subseq text hi) out)
          (when (plusp count)
            (setf (alloy:value pane) (get-output-stream-string out))
            (setf (modified-p window) t)
            (%clear-selection pane)
            (alloy:move-to (min new-caret (length (alloy:value pane))) (alloy:cursor pane))
            (alloy:mark-for-render pane))
          count)
        0)))

(defun %find-replace-dialog (window replace-p)
  "Build and show the Find (REPLACE-P nil) or Find/Replace (REPLACE-P t) dialog
bound to the editor WINDOW. Enter runs Find Next and Esc closes (vanilla
:default-button :find-next / :cancel-button :cancel, om-LW find-dialog); the
Direction radio (om-LW find-dialog) governs Find, Replace and Replace All alike
(om-LW find-replace-callback and do-editor-find-replace-callback); the status goes
to a line INSIDE the dialog — the CAPI interface has no status area and can only
pop a message box for it (om-LW find-replace-callback)."
  (let* ((labels   (if replace-p '("Find what:" "Replace with:") '("Find what:")))
         ;; the label column is measured, never guessed: a narrow box is what wrapped
         ;; "Replace with:" onto a second line and over the field
         (dialog-font (%om-native-font :dialog-default))
         (label-w  (+ 6 (reduce #'max labels
                                :key (lambda (s) (om-string-size s dialog-font)))))
         (field-x  (+ 12 label-w 6))
         (field-w  210)
         (btn-w    108)
         (btn-x    (+ field-x field-w 12))
         (w        (+ btn-x btn-w 12))
         (y-find   12)
         (y-repl   46)
         (y-dir    (if replace-p 82 48))
         (dir-h    56)
         (h        (+ y-dir dir-h 12))
         (dlg (om-make-window 'om-dialog
                              :window-title (if replace-p "Replace" "Find")
                              :size (om-make-point w h)
                              :position :centered
                              :resizable nil :maximize nil :minimize nil
                              :bg-color *om-window-def-color*))
         (find-field    (om-make-dialog-item 'om-history-text
                                             (om-make-point field-x y-find)
                                             (om-make-point field-w 24)
                                             (or (car *find-strings*) "")
                                             :history-source (lambda () *find-strings*)))
         (replace-field (when replace-p
                          (om-make-dialog-item 'om-history-text
                                               (om-make-point field-x y-repl)
                                               (om-make-point field-w 24)
                                               (or (car *replace-strings*) "")
                                               :history-source (lambda () *replace-strings*))))
         (up-radio   (om-make-dialog-item 'om-radio-button
                                          (om-make-point 14 22) (om-make-point 80 20) "Up"
                                          :radio-button-cluster :find-direction))
         (down-radio (om-make-dialog-item 'om-radio-button
                                          (om-make-point 100 22) (om-make-point 90 20) "Down"
                                          :radio-button-cluster :find-direction
                                          :checked-p t))
         ;; the CAPI radio-button-panel draws its own frame from :title/:title-position
         ;; (om-LW find-dialog); here the frame is the view's border and the title is a label
         ;; laid over it, which is what breaks the line the way a group box does
         ;; frame + interior painted with the SAME constant the dialog uses, so the
         ;; group box reads as a frame drawn ON the dialog, not as a panel of its own
         (dir-group  (om-make-view 'om-view
                                   :position (om-make-point field-x y-dir)
                                   :size (om-make-point field-w dir-h)
                                   :bg-color *om-window-def-color*
                                   :border-color *om-gray-color*
                                   :border-width 1
                                   :subviews (list up-radio down-radio)))
         (dir-title  (om-make-dialog-item 'om-static-text
                                           (om-make-point (+ field-x 10) (- y-dir 8))
                                           (om-make-point 70 16) "Direction"
                                           :font dialog-font
                                           :wrap nil
                                           :bg-color *om-window-def-color*)))
    (flet ((find-text () (string (om-dialog-item-text find-field)))
           (repl-text () (if replace-field (string (om-dialog-item-text replace-field)) ""))
           (backward-p () (and (om-checked-p up-radio) t))
           ;; end of buffer is a MESSAGE BOX, as in vanilla (display-message-for-pane,
           ;; om-LW find-replace-callback) -- not a status line invented for the fork
           (not-found (f) (om-message-dialog (format nil "Cannot find ~S" f)))
           ;; dedupe is case-insensitive in vanilla (pushnew :test 'string-equal, om-LW find-replace-callback);
           ;; the fields read the list at open time, so there is nothing to refresh
           (remember-find (f)
             (when (plusp (length f))
               (pushnew f *find-strings* :test #'string-equal)))
           (remember-repl (r)
             (when replace-field
               (pushnew r *replace-strings* :test #'string-equal))))
      (flet ((search-action ()
               (let ((f (find-text)))
                 (remember-find f)
                 (unless (%find-next window f (backward-p))
                   (not-found f))))
             (replace-action ()
               ;; vanilla replaces what the region is sitting on, then runs the same
               ;; find loop, which is what reports the end of buffer (om-LW do-editor-find-replace-callback)
               (let* ((f (find-text)) (r (repl-text)))
                 (remember-find f) (remember-repl r)
                 (%replace-current window f r)
                 (unless (%find-next window f (backward-p))
                   (not-found f))))
             (replace-all-action ()
               ;; vanilla returns right after query-replace-string and says NOTHING,
               ;; however many it replaced (om-LW do-editor-find-replace-callback)
               (let ((f (find-text)) (r (repl-text)))
                 (remember-find f) (remember-repl r)
                 (%replace-all window f r (backward-p)))))
        ;; buttons stack in a column on the right, in the vanilla order
        ;; (push-button-panel :layout-class 'column-layout, om-LW find-dialog,
        ;; find-in-file and replace-in-file)
        (let* ((btn-y 12)
               (items
                (list
                  (om-make-dialog-item 'om-static-text (om-make-point 12 (+ y-find 3))
                                       (om-make-point label-w 20) "Find what:"
                                       :font dialog-font :wrap nil)
                 find-field dir-group dir-title
                 (om-make-dialog-item 'om-button (om-make-point btn-x btn-y)
                                      (om-make-point btn-w 26) "Find Next"
                                      :default-button t
                                      :di-action (om-dialog-item-act item (declare (ignore item))
                                                   (search-action))))))
          (incf btn-y 34)
          (when replace-p
            (setf items
                  (append items
                          (list
                           (om-make-dialog-item 'om-static-text (om-make-point 12 (+ y-repl 3))
                                                 (om-make-point label-w 20) "Replace with:"
                                                 :font dialog-font :wrap nil)
                           replace-field
                           (om-make-dialog-item 'om-button (om-make-point btn-x btn-y)
                                                (om-make-point btn-w 26) "Replace"
                                                :di-action (om-dialog-item-act item (declare (ignore item))
                                                             (replace-action)))
                           (om-make-dialog-item 'om-button (om-make-point btn-x (+ btn-y 34))
                                                (om-make-point btn-w 26) "Replace All"
                                                :di-action (om-dialog-item-act item (declare (ignore item))
                                                             (replace-all-action))))))
            (incf btn-y 68))
          (setf items
                (append items
                        (list (om-make-dialog-item 'om-button (om-make-point btn-x btn-y)
                                                   (om-make-point btn-w 26) "Cancel"
                                                   :cancel-button t
                                                   :di-action (om-dialog-item-act item (declare (ignore item))
                                                                (om-close-window dlg))))))
          (apply #'om-add-subviews dlg items)
          (om-show-window dlg)
          ;; CAPI focuses a dialog's first field on display; Alloy does not, so the
          ;; caret has to be placed here or typing is dropped until the user clicks.
          (%om-focus-pane dlg find-field)
          dlg)))))

;;;==================
;;; ENTRY POINTS
;;;==================

(defun find-in-file (window)
  (%find-replace-dialog window nil))

(defun replace-in-file (window)
  (%find-replace-dialog window t))

;;; search-anvita-files: multi-file directory search. Vanilla ships its menu entry
;;; :enabled-function 'disabled, so this remains a no-op until om-choose-directory-dialog
;;; is exposed in this port.
(defun search-anvita-files (interface)
  (declare (ignore interface))
  (warn "search-anvita-files: multi-file search is disabled (matches vanilla 'disabled menu entry)."))
