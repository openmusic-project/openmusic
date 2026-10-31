(in-package :oa)

(defvar *table-help-window* nil)

(defparameter *table-window-title* "OM-TABLE")

(defclass om-table-editor (om-look-window om-filter-host)
  ((ep              :accessor ep              :initarg :ep              :initform nil)
   (table           :accessor table           :initarg :table           :initform nil)
   (columns         :accessor columns         :initarg :columns         :initform nil)
   (contents        :accessor contents        :initarg :contents        :initform nil)
   (box             :accessor box             :initarg :box             :initform nil)
   (row-labels      :accessor table-editor-row-labels
                    :initarg :row-labels      :initform nil)
   (column-labels   :accessor table-editor-column-labels
                    :initarg :column-labels   :initform nil)
   (row-label-fn    :accessor table-editor-row-label-fn
                    :initarg :row-label-fn    :initform #'identity)
   (column-label-fn :accessor table-editor-column-label-fn
                    :initarg :column-label-fn :initform #'identity)))

(defclass om-table-window (om-table-editor)
  ((save-callback :accessor save-callback :initarg :save-callback :initform nil)))

;;; The grid, its cells, selection, navigation and look are the library's
;;; (alloy-desktop/widgets/grid.lisp: grid-view, grid-cell); OM keeps the editor
;;; link, the sort state and the three policy answers below.
(defclass om-table-grid (om-scroll-mixin alld:grid-view om-graphic-object)
  ((editor          :accessor table-grid-editor          :initarg :editor)
   (sort-column     :accessor table-sort-column          :initform nil)
   (sort-descending :accessor table-sort-descending-p    :initform nil))
  (:default-initargs :cell-class 'om-table-cell))

(defclass om-table-cell (alld:grid-cell om-button) ())

(defmethod alld:grid-focus-root ((grid om-table-grid))
  (om-window-focus (table-grid-editor grid)))

(defmethod alld:grid-header-activated ((grid om-table-grid) value)
  (tab-header-callback (table-grid-editor grid) value))

(defmethod alld:grid-edit-cell ((grid om-table-grid) row column)
  (run-dialog (table-grid-editor grid) column row))

(defmethod om-select-window ((self om-table-editor))
  (call-next-method))

(defmethod om-view-window ((self om-table-editor)) self)
(defmethod om-view-window ((self om-table-window)) self)

(defmethod om-set-window-title ((self om-table-editor) (title string))
  (call-next-method))

(defmethod om-get-editor-panel ((self om-table-editor)) (ep self))

(defmethod interface-match-p ((self om-table-editor) &rest initargs &key name)
  (declare (ignore initargs))
  (and name (window-name self) (string-equal (window-name self) name)))

(defun disabled (window)
  (declare (ignore window))
  nil)

(defun %table-inactive-menu-item (title &optional accelerator)
  (om-new-leafmenu title (lambda () nil) accelerator nil))

(defmethod internal-window-class-menubar ((self om-table-editor))
  (list
   (om-make-menu
    "File"
    (list
     (list (%table-inactive-menu-item "New..." #\n)
           (%table-inactive-menu-item "Open..."))
     (list (om-new-leafmenu "Import Csv File"
                            (lambda () (import-csv-from-file self)) #\o)
           (om-new-leafmenu "Export Csv File"
                            (lambda () (export-csv-from-file self)) #\e))
     (list (%table-inactive-menu-item "Save" #\s)
           (%table-inactive-menu-item "Save As..."))
     (list (om-new-leafmenu "Get Info" (lambda () (get-table-info self)) #\i))
     (om-new-leafmenu "Close" (lambda () (om-close-window self)) #\w)))
   (om-make-menu
    "Edit"
    (list (%table-inactive-menu-item "Undo" #\z)
          (list (%table-inactive-menu-item "Cut" #\x)
                (%table-inactive-menu-item "Copy" #\c)
                (%table-inactive-menu-item "Paste" #\v))
          (%table-inactive-menu-item "Select All" #\a)
          (list (%table-inactive-menu-item "Text Font"))
          (list (%table-inactive-menu-item "Find..." #\f)
                (%table-inactive-menu-item "Replace..." #\r))
          (%table-inactive-menu-item "Search...")))))

(defmethod om-window-class-menubar ((self om-table-editor))
  (list
   (om-make-menu
    "Windows"
    (cons (om-new-leafmenu "Listener"
                           (lambda ()
                             (om-make-new-listener
                              :initial-lambda (lambda () (in-package :om))))
                           #\l)
          (mapcar (lambda (window)
                    (om-new-leafmenu (om-window-title window)
                                     (lambda () (om-select-window window))))
                  (remove self (om-get-all-windows 'om-table-editor)))))
   (om-make-menu
    "Help"
    (list (om-new-leafmenu "Editor Command Keys..."
                           (lambda ()
                             (when (functionp *table-help-window*)
                               (funcall *table-help-window*)))
                           #\h)))))

(defun %table-row-data (row)
  (cond ((null row) nil)
        ((listp row) (copy-list row))
        ((vectorp row) (coerce row 'list))
        (t (list row))))

(defun matrans (matrix)
  (let* ((rows (mapcar #'%table-row-data (coerce (or matrix nil) 'list)))
         (width (loop for row in rows maximize (length row) into maximum
                      finally (return (or maximum 0)))))
    (loop for column below width
          collect (mapcar (lambda (row) (nth column row)) rows))))

(defun %table-normalize (rows columns sequence)
  (let* ((data (mapcar #'%table-row-data (coerce (or sequence nil) 'list)))
         (given-rows (and rows (coerce rows 'list)))
         (labels (if given-rows
                     (loop for row in data
                           for label in given-rows
                           collect label)
                     (loop for index from 1 to (length data) collect index)))
         (paired (loop for row in data
                       for label in labels
                       collect (cons label row)))
         (derived-width (loop for row in data maximize (length row) into maximum
                              finally (return (or maximum 0))))
         (column-labels (if columns
                            (coerce columns 'list)
                            (loop for index from 1 to derived-width collect index))))
    (values paired labels column-labels)))

(defun %table-label-columns (labels function)
  (mapcar (lambda (label) (funcall function label)) labels))

(defun test-nth (n &optional reverse)
  (lambda (x y)
    (let ((left (nth n (if reverse y x)))
          (right (nth n (if reverse x y))))
      (cond ((and (numberp left) (numberp right)) (> left right))
            (t (string-greaterp (princ-to-string left)
                                (princ-to-string right)))))))

(defun spreadsheet-sort-descriptions (columns &key (column-label-fn #'identity))
  (cons (list :type :| | :sort (test-nth 0) :reverse-sort (test-nth 0 t))
        (loop for column in columns
              for index from 1
              collect (list :type (intern (princ-to-string
                                            (funcall column-label-fn column))
                                           :keyword)
                            :sort (test-nth index)
                            :reverse-sort (test-nth index t)))))

;;; CAPI's :filter 't (om-LW open-new-table-editor) hides rows without touching the collection
;;; items; CONTENTS is kernel data (om-table rows/cols/data), so filtering only
;;; ever narrows what the grid shows and ROW-MAP relates a visual row back to it.

(defun %table-row-display-text (row)
  "The row as the user reads it — what the filter matches against."
  (format nil "~{~A~^ ~}" row))

(defun %table-visible-rows (editor)
  "((source-index . row) ...) for the rows the filter lets through."
  (loop for row in (contents editor)
        for index from 0
        when (alld:filter-pass-p editor (%table-row-display-text row))
          collect (cons index row)))

(defun %table-source-row (grid row)
  "Index in the editor CONTENTS of GRID's visual row ROW."
  (let ((map (alld:grid-row-map grid)))
    (if map (nth row map) row)))

(defun %table-update-matches (editor)
  (let ((label (alld:filter-matches editor))
        (grid  (table editor)))
    (when label
      (om-set-dialog-item-text
       label (format nil "Matches ~D" (if grid (length (alld:grid-row-map grid)) 0))))))

(defun %table-refresh-grid (grid)
  (alld:grid-clear-cells grid)
  (let* ((editor (table-grid-editor grid))
         (visible (%table-visible-rows editor))
         (rows (mapcar #'cdr visible)))
    (setf (alld:grid-row-map grid) (mapcar #'car visible)
          (alld:grid-row-labels grid)
          (%table-label-columns (mapcar #'car rows)
                                (table-editor-row-label-fn editor)))
    (alld:grid-build-cells grid rows)
    (%table-update-matches editor)
    (alloy:refit grid)
    (alloy:mark-for-render grid)
    grid))

(defun %table-ensure-row-width (row width)
  (if (< (length row) width)
      (nconc row (make-list (- width (length row))))
      row))

(defun %table-openmusic-slot (name)
  ;; late lookup: this file compiles before the kernel's OPENMUSIC package exists
  (find-symbol name "OPENMUSIC"))

(defun %table-slot-exists-p (object slot)
  ;; OBJECT is an instance, so its class is finalised and CLASS-SLOTS always answers
  (and object slot
       (find slot (sb-mop:class-slots (class-of object))
             :key #'sb-mop:slot-definition-name)))

(defun %table-box-slot-value (object name)
  (let ((slot (%table-openmusic-slot name)))
    (when (and (%table-slot-exists-p object slot) (slot-boundp object slot))
      (slot-value object slot))))

(defun %table-set-box-slot (object name value)
  (let ((slot (%table-openmusic-slot name)))
    (when (%table-slot-exists-p object slot)
      (setf (slot-value object slot) value))))

(defun %table-editor-source-object (editor)
  (or (box editor)
      (%table-box-slot-value editor "OBJECT")))

(defun %table-sync-box (editor)
  (let ((object (%table-editor-source-object editor)))
    (when object
      (%table-set-box-slot object "ROWS" (mapcar #'car (contents editor)))
      (%table-set-box-slot object "COLS" (copy-list (table-editor-column-labels editor)))
      (%table-set-box-slot object "DATA" (mapcar (lambda (row) (copy-list (cdr row)))
                                                  (contents editor)))))
  editor)

(defun %table-edit-cell-value (grid row column value)
  (let* ((editor (table-grid-editor grid))
         (content-row (nth (%table-source-row grid row) (contents editor))))
    (when content-row
      (setf content-row (%table-ensure-row-width content-row (+ column 2)))
      (setf (nth (1+ column) content-row) value)
      (%table-sync-box editor)
      (%table-refresh-grid grid)
      (alld:grid-select-cell grid row column)
      value)))

(defun %table-sort-column (grid column)
  (let* ((editor (table-grid-editor grid))
         (descending (if (eql column (table-sort-column grid))
                         (not (table-sort-descending-p grid))
                         nil)))
    (setf (table-sort-column grid) column
          (table-sort-descending-p grid) descending)
    (setf (slot-value editor 'contents)
          (stable-sort (copy-list (contents editor))
                       (lambda (left right)
                         (alld:grid-order-before-p (nth (1+ column) left)
                                                   (nth (1+ column) right)
                                                   descending))))
    (setf (table-editor-row-labels editor) (mapcar #'car (contents editor)))
    (%table-sync-box editor)
    (%table-refresh-grid grid)
    (when (contents editor) (alld:grid-select-cell grid 0 column))
    (contents editor)))

(defun %table-read-cell-value (text)
  (let ((*read-eval* nil))
    (read-from-string text)))

(defclass dialog-test (om-dialog)
  ((ep         :accessor ep         :initarg :ep    :initform nil)
   (ncol       :accessor ncol       :initarg :ncol  :initform nil)
   (nrow       :accessor nrow       :initarg :nrow  :initform nil)
   (entry      :accessor entry      :initarg :entry :initform nil)
   (input-pane :accessor dialog-test-input-pane     :initform nil)))

(defun %table-make-cell-dialog (ep ncol nrow)
  "Vanilla's dialog-test interface (om-LW dialog-test): a text input prefilled with
the cell, over OK/Cancel."
  (let* ((grid   (table ep))
         (source (nth (%table-source-row grid nrow) (contents ep)))
         (text   (format nil "~A" (and source (nth (1+ ncol) source))))
         (w 280)
         (h 108)
         (dialog (om-make-window 'dialog-test
                                 :window-title "Enter some text:"
                                 :size (om-make-point w h)
                                 :position :centered
                                 :resizable nil :maximize nil :minimize nil
                                 :bg-color *om-window-def-color*
                                 :ep ep :ncol ncol :nrow nrow :entry text))
         (input  (om-make-dialog-item 'om-editable-text
                                      (om-make-point 15 16)
                                      (om-make-point (- w 30) 22) text
                                      :font *om-default-font2*
                                      :bg-color *om-white-color*)))
    (setf (dialog-test-input-pane dialog) input)
    (om-add-subviews
     dialog
     input
     (om-make-dialog-item 'om-button (om-make-point 22 (- h 42))
                          (om-make-point 100 24) "OK"
                          :default-button t
                          :di-action (om-dialog-item-act item
                                       (declare (ignore item))
                                       (exit-dialog-test dialog)))
     (om-make-dialog-item 'om-button (om-make-point 152 (- h 42))
                          (om-make-point 100 24) "Cancel"
                          :cancel-button t
                          :di-action (om-dialog-item-act item
                                       (declare (ignore item))
                                       (om-return-from-modal-dialog dialog nil))))
    dialog))

(defun exit-dialog-test (interface)
  "Commit the typed value into the cell the dialog was opened on (om-LW exit-dialog-test)."
  (let* ((editor (ep interface))
         (grid   (and editor (table editor)))
         (row    (nrow interface))
         (column (ncol interface))
         (text   (om-dialog-item-text (dialog-test-input-pane interface))))
    (when (and grid row column)
      (handler-case
          (%table-edit-cell-value grid row column (%table-read-cell-value text))
        (error (condition)
          (om-message-dialog (format nil "Invalid cell value: ~A" condition)
                             :owner editor))))
    (om-return-from-modal-dialog interface text)
    text))

(defun run-dialog (ep ncol nrow)
  (om-modal-dialog (%table-make-cell-dialog ep ncol nrow) ep))

(defun tab-header-callback (interface item)
  (let* ((grid (table interface))
         (column (position item (alld:grid-column-labels grid) :test #'equal))
         (row (or (alld:grid-selected-row grid) 0)))
    (when column
      (cond ((om-command-key-p) (run-dialog interface column row))
            ((om-shift-key-p)
             (format t "~&table selection row=~D column=~D~%" row column))
            ((om-option-key-p)
             (format t "~&table cell ~S~%"
                     (nth (1+ column) (nth row (contents interface)))))
            (t (%table-sort-column grid column))))))

(defun open-row-in-editor (interface item)
  (declare (ignore interface))
  (format t "~&row: ~A ~A~%" (car item) (cdr item))
  item)

(defun get-table-rows (table)
  (copy-tree (contents (table-grid-editor table))))

(defun change-item-in-table (table new)
  (let ((editor (table-grid-editor table)))
    (setf (slot-value editor 'contents) (copy-tree new)
          (table-editor-row-labels editor) (mapcar #'car new))
    (%table-sync-box editor)
    (%table-refresh-grid table)
    new))

(defun update-table (table)
  (%table-refresh-grid table))

(defun update-table-val (table row)
  (dolist (cell (alld:grid-cells table))
    (when (eql row (alld:grid-cell-row cell)) (alloy:mark-for-render cell)))
  row)

(defun %table-box-data (editor)
  (let* ((object (%table-editor-source-object editor))
         (data-slot (%table-openmusic-slot "DATA")))
    (when (and (%table-slot-exists-p object data-slot)
               (slot-boundp object data-slot))
      (values (%table-box-slot-value object "ROWS")
              (%table-box-slot-value object "COLS")
              (slot-value object data-slot)
              t))))

(defun %table-refresh-from-box (editor)
  (multiple-value-bind (rows columns data present-p) (%table-box-data editor)
    (when present-p
      (multiple-value-bind (normalized row-labels column-labels)
          (%table-normalize rows columns data)
        (setf (slot-value editor 'contents) normalized
              (table-editor-row-labels editor) row-labels
              (table-editor-column-labels editor) column-labels
              (columns editor) (mapcar (lambda (label)
                                         (intern (princ-to-string label) :keyword))
                                       column-labels))
        (when (table editor)
          (setf (alld:grid-column-labels (table editor))
                (%table-label-columns column-labels
                                      (table-editor-column-label-fn editor)))
          (%table-refresh-grid (table editor))))))
  editor)

;;; Filter bar — CAPI builds it from the single :filter 't initarg (om-LW open-new-table-editor):
;;; mode dropdown, live text field, clear button and match counter. The widgets,
;;; the three axes and the dropdown are the shared om-filter-host machinery
;;; (alloy-desktop/widgets/filter.lisp: defclass filter-host, and the filter
;;; widgets of dialog-items.lisp); only the data source and the strip layout are the
;;; table's own.

(defmethod alld:filter-apply ((editor om-table-editor))
  (let ((grid (table editor)))
    (when grid (%table-refresh-grid grid))))

;;; More specific than the menubar-accelerator :around (menu.lisp: the
;;; alloy:handle :around for (key-down om-look-window)), which
;;; would otherwise consume Ctrl+E as "Export Csv File" while the user types.
(defmethod alloy:handle :around ((event alloy:key-down) (window om-table-editor))
  (let ((field (alld:filter-field window))
        (what  (alld:filter-chord-axis event)))
    (if (and field what
             (find :control (alloy:modifiers event))
             (eql :strong (alloy:focus field)))
        (progn (%filter-toggle window what) t)
        (call-next-method))))

(defun %table-build-filter-bar (editor focus)
  (let* ((bar   (make-instance 'alloy:border-layout))
         (right (make-instance 'alloy:border-layout))
         (mode  (%filter-make-mode-button editor))
         (field (%filter-make-field editor))
         (clear (%filter-make-clear-button editor))
         (matches (om-make-dialog-item
                   'om-static-text (om-make-point 0 0)
                   (om-make-point alld:*filter-matches-width* 20) "Matches 0"
                   :font (%om-native-font :dialog-default)
                   :bg-color *om-light-gray-color*
                   :fg-color *om-black-color*)))
    (setf (alloy:cell-margins bar) (alloy:margins 2)
          (alloy:cell-margins right) (alloy:margins 0))
    (alloy:enter mode  bar :place :west
                 :size (alloy:un (%filter-mode-width (om-dialog-item-text mode))))
    (alloy:enter field bar :place :center)
    (alloy:enter right bar :place :east
                 :size (alloy:un (+ alld:*filter-clear-width* alld:*filter-matches-width*)))
    (alloy:enter clear   right :place :west :size (alloy:un alld:*filter-clear-width*))
    (alloy:enter matches right :place :center)
    (dolist (widget (list mode field clear))
      (alloy:enter widget focus))
    (setf (alld:filter-mode-menu editor) mode
          (alld:filter-field editor)     field
          (alld:filter-clear editor)     clear
          (alld:filter-matches editor)   matches
          (alld:filter-bar editor)       bar)
    bar))

(defun %table-build-grid (editor)
  (%table-refresh-from-box editor)
  (let* ((root  (om-window-root editor))
         (focus (om-window-focus editor))
         (outer (make-instance 'alloy:border-layout))
         (filter (%table-build-filter-bar editor focus))
         (clip (make-instance 'alloy:clip-view
                              :offset (alloy:px-point 0 0)))
         (grid (make-instance 'om-table-grid
                              :editor editor
                              :row-labels (%table-label-columns
                                           (table-editor-row-labels editor)
                                           (table-editor-row-label-fn editor))
                              :column-labels (%table-label-columns
                                              (table-editor-column-labels editor)
                                              (table-editor-column-label-fn editor)))))
    (setf (alloy:cell-margins outer) (alloy:margins 0))
    (alloy:with-unit-parent root
      (alloy:enter outer root :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1)))
    (alloy:enter filter outer :place :north
                 :size (alloy:un alld:*filter-bar-height*))
    (alloy:enter clip outer :place :center)
    (alloy:enter grid clip)
    (setf (ep editor) editor
          (table editor) grid)
    (%table-refresh-grid grid)
    (alloy:refit (om-window-chrome editor))
    grid))

(defmethod make-window-layout ((editor om-table-editor) &optional color)
  (declare (ignore color))
  (%table-build-grid editor))

(defmethod init-table-editor ((self om-table-editor))
  (or (table self) (%table-build-grid self)))

;;; TITLE is part of the kernel signature but vanilla never uses it: the window
;;; always shows the class default-initarg :title "OM-TABLE" (om-LW the defclass
;;; om-table-editor, read by open-new-table-editor).
(defun open-new-table-editor (rows columns sequence
                              &key (title "Spreadsheet")
                                   (row-label-fn #'identity)
                                   (column-label-fn #'identity))
  (declare (ignore title))
  (multiple-value-bind (normalized row-labels column-labels)
      (%table-normalize rows columns sequence)
    (let ((editor (om-make-window
                   'om-table-editor
                   :window-title *table-window-title*
                   :name *table-window-title*
                   :size (om-make-point 500 400)
                   :position :centered
                   :contents normalized
                   :row-labels row-labels
                   :column-labels column-labels
                   :columns (mapcar (lambda (label)
                                      (intern (princ-to-string
                                               (funcall column-label-fn label))
                                              :keyword))
                                    column-labels)
                   :row-label-fn row-label-fn
                   :column-label-fn column-label-fn)))
      ;; om-LW editors/om-table-editor.lisp (open-new-table-editor) builds the bar
      ;; from these two lists only, so no app menu is prepended; the setf hook
      ;; realizes it (lw-compat.lisp: the (setf capi::interface-menu-bar-items)).
      (setf (capi::interface-menu-bar-items editor)
            (append (internal-window-class-menubar editor)
                    (om-window-class-menubar editor)))
      (when normalized (alld:grid-select-cell (table editor) 0 0))
      editor)))

(defmethod (setf box) :after (value (editor om-table-editor))
  (declare (ignore value))
  (%table-refresh-from-box editor))

(defmethod (setf contents) :after (value (editor om-table-editor))
  (declare (ignore value))
  (if (box editor)
      (%table-refresh-from-box editor)
      (when (table editor)
        (%table-refresh-grid (table editor)))))

(defun get-table-info (interface)
  (let* ((column-count (length (columns interface)))
         (row-count (length (contents interface)))
         (cell-count (loop for row in (contents interface) sum (length (cdr row)))))
    (om-message-dialog
     (format nil "Rows: ~D~%~%Columns: ~D~%~%Cells: ~D"
             row-count column-count cell-count)
     :owner interface)
    (values row-count column-count cell-count)))

(defun %csv-escape-field (value)
  (let* ((text (prin1-to-string value))
         (quoted (with-output-to-string (out)
                   (loop for char across text
                         do (if (char= char #\")
                                (write-string "\"\"" out)
                                (write-char char out))))))
    (if (find-if (lambda (char)
                   (member char '(#\, #\; #\" #\Newline #\Return)))
                 text)
        (format nil "\"~A\"" quoted)
        text)))

(defun export-csv-file (list path)
  (let ((pathname (or path (om-choose-new-file-dialog :prompt "Export Csv file"))))
    (when pathname
      (with-open-file (out pathname :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
        (loop for row in list
              do (loop for value in row
                       for first = t then nil
                       do (unless first (write-char #\, out))
                          (write-string (%csv-escape-field value) out))
                 (terpri out)))
      pathname)))

(defmethod export-csv-from-file ((self om-table-editor) &optional path)
  (export-csv-file (contents self) path))

(defun replace-all-str (string part replacement &key (test #'char=))
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-position = 0 then (+ position part-length)
          for position = (search part string :start2 old-position :test test)
          do (write-string string out :start old-position
                                     :end (or position (length string)))
          when position do (write-string replacement out)
          while position)))

(defun %csv-split-line (line)
  (let ((fields nil)
        (quoted nil)
        (buffer (make-string-output-stream)))
    (labels ((emit ()
               (push (get-output-stream-string buffer) fields)
               (setf buffer (make-string-output-stream))))
      (loop for index from 0 below (length line)
            for char = (char line index)
            do (cond ((char= char #\")
                      (if (and quoted (< (1+ index) (length line))
                               (char= #\" (char line (1+ index))))
                          (progn (write-char #\" buffer) (incf index))
                          (setf quoted (not quoted))))
                     ((and (not quoted) (member char '(#\, #\;))) (emit))
                     (t (write-char char buffer))))
      (emit)
      (nreverse fields))))

(defun %csv-read-value (field)
  (if (zerop (length field))
      nil
      (handler-case
          (let ((*read-eval* nil))
            (multiple-value-bind (value position) (read-from-string field)
              (if (every (lambda (char)
                           (member char '(#\Space #\Tab #\Return #\Newline)))
                         (subseq field position))
                  value
                  field)))
        ;; FIELD is a CSV cell: an unreadable cell is text, not a defect
        ((or end-of-file reader-error) () field))))

(defun import-csv-file (path)
  (let ((pathname (or path (om-choose-file-dialog :prompt "Open Csv file"))))
    (when pathname
      (with-open-file (file pathname :direction :input)
        (loop for line = (read-line file nil nil)
              while line
              collect (mapcar #'%csv-read-value (%csv-split-line line)))))))

(defmethod import-csv-from-file ((self om-table-editor) &optional path)
  (let* ((pathname (or path (om-choose-file-dialog :prompt "Open File"
                                                   :types '("CSV file"  "*.csv"
                                                            "Lisp file" "*.lisp"
                                                            "Text file" "*.txt"))))
         (data (and pathname (import-csv-file pathname))))
    (when data
      (om-close-window self)
      (open-new-table-editor nil nil data))))

(defclass om-vector ()
  ((data :initform nil :accessor data :initarg :data)
   (lgt  :initform nil :accessor lgt  :initarg :lgt)))

(defun make-vector (list)
  (coerce list 'simple-vector))

(defmethod set-vector-elt ((self om-vector) (element t)
                           (row-position number) (column-position number))
  (when (and (integerp row-position) (not (minusp row-position))
             (< row-position (length (data self))))
    (let ((row (svref (data self) row-position)))
      (when (and (integerp column-position) (not (minusp column-position))
                 (< column-position (length row)))
        (setf (nth column-position row) element)))))

(defmethod get-vector-elt ((self om-vector)
                           (row-position number) (column-position number))
  (when (and (integerp row-position) (not (minusp row-position))
             (< row-position (length (data self))))
    (let ((row (svref (data self) row-position)))
      (when (and (integerp column-position) (not (minusp column-position))
                 (< column-position (length row)))
        (nth column-position row)))))
