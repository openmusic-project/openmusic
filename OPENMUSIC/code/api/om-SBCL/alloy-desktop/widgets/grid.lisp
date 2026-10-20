(in-package :alloy-desktop)

;;; Grid: a scrolled table of cells -- a corner, one header per column, one header per
;;; row and the data cells -- with one selected data cell, keyboard navigation and a
;;; look, the role of the toolkit's multi-column list panel. The host answers three
;;; policy seams: the focus root of the cells, a click on a column header and the
;;; request to edit a data cell.

(defparameter *grid-row-height* 24
  "Height of every grid row, the header row included, in px.")
(defparameter *grid-row-label-width* 144
  "Width of the row-header column in px.")
(defparameter *grid-column-width* 72
  "Width of every data column in px.")

(defparameter *grid-header-color*         (chroma:rgb 1.0 1.0 1.0)
  "Background of the corner and of the header cells.")
(defparameter *grid-cell-color*           (chroma:rgb 1.0 1.0 1.0)
  "Background of a data cell on an even row.")
(defparameter *grid-alt-cell-color*       (chroma:rgb 0.96 0.96 0.96)
  "Background of a data cell on an odd row.")
(defparameter *grid-selection-color*      (chroma:rgb 0.10 0.48 0.82)
  "Background of the selected data cell.")
(defparameter *grid-separator-color*      (chroma:rgb 0.83 0.83 0.83)
  "Rules of the header cells.")
(defparameter *grid-text-color*           (chroma:rgb 0.08 0.08 0.08)
  "Text of a cell.")
(defparameter *grid-selected-text-color*  (chroma:rgb 1.0 1.0 1.0)
  "Text of the selected data cell.")

(defclass grid-view (scroll-mixin alloy:fixed-layout)
  ((content-layout  :initform nil :accessor grid-layout)
   (row-labels      :initarg :row-labels      :initform nil :accessor grid-row-labels)
   (column-labels   :initarg :column-labels   :initform nil :accessor grid-column-labels)
   (selected-row    :initarg :selected-row    :initform nil :accessor grid-selected-row)
   (selected-column :initarg :selected-column :initform nil :accessor grid-selected-column)
   (row-map         :initform nil :accessor grid-row-map)
   (cells           :initform nil :accessor grid-cells)
   (cell-class      :initarg :cell-class :initform 'grid-cell :accessor grid-cell-class))
  (:default-initargs :scrollbars t :retain-scrollbars nil))

(defclass grid-cell (button)
  ((grid   :initarg :grid :accessor grid-cell-grid)
   (row    :initarg :row    :initform nil :accessor grid-cell-row)
   (column :initarg :column :initform nil :accessor grid-cell-column)
   (kind   :initarg :kind :accessor grid-cell-kind)
   ;; not named VALUE: that is the button's own label slot (alloy components/base.lisp:
   ;; defclass direct-value-component)
   (cell-value :initarg :cell-value :initform nil :accessor grid-cell-value)))

;;; Policy: what the host does with the focus, a column header and an edit request.

(defgeneric grid-focus-root (grid)
  (:documentation "Focus parent of GRID's cells, whose focused child is the selected cell; NIL leaves the cells outside a focus tree.")
  (:method ((grid t)) nil))

(defgeneric grid-header-activated (grid value)
  (:documentation "Answer the activation of the column header of GRID whose value is VALUE.")
  (:method ((grid t) value)
    (declare (ignore value))
    nil))

(defgeneric grid-edit-cell (grid row column)
  (:documentation "Answer a request to edit the data cell at ROW, COLUMN of GRID.")
  (:method ((grid t) row column)
    (declare (ignore row column))
    nil))

(defun grid-content-width (grid)
  (+ *grid-row-label-width*
     (* *grid-column-width* (length (grid-column-labels grid)))))

(defun grid-content-height (grid)
  (* *grid-row-height* (1+ (length (grid-row-labels grid)))))

(defmethod scroll-content-size ((grid grid-view))
  (values (grid-content-width grid) (grid-content-height grid)))

(defmethod alloy:refit ((grid grid-view))
  "Keep GRID at viewport size and place its inner Alloy grid at content size.
The y origin aligns the first row with the viewport top; positive scroll-y
then moves the inner layout upward to reveal later rows."
  (let ((layout (grid-layout grid)))
    (when layout
      (let* ((bounds (alloy:bounds grid))
             (content-width (grid-content-width grid))
             (content-height (grid-content-height grid)))
        (setf (alloy:bounds layout)
              (alloy:px-extent 0
                               (- (alloy:pxh bounds) content-height)
                               content-width content-height))
        (alloy:refit layout))))
  grid)

(defun grid-cell-selected-p (cell)
  (let ((grid (grid-cell-grid cell)))
    (and (eq :data (grid-cell-kind cell))
         (eql (grid-cell-row cell) (grid-selected-row grid))
         (eql (grid-cell-column cell) (grid-selected-column grid)))))

(defun grid-cell-header-p (cell)
  (and (member (grid-cell-kind cell) '(:corner :column-header)) t))

(defun grid-cell-background (cell)
  (case (grid-cell-kind cell)
    ((:corner :column-header :row-header) *grid-header-color*)
    (:data
     (cond ((grid-cell-selected-p cell) *grid-selection-color*)
           ((oddp (or (grid-cell-row cell) 0)) *grid-alt-cell-color*)
           (t *grid-cell-color*)))
    (t *grid-cell-color*)))

(defun grid-cell-foreground (cell)
  (if (grid-cell-selected-p cell)
      *grid-selected-text-color*
      *grid-text-color*))

(defun grid-cell-edge-bounds (cell edge)
  "1px separator strip in CELL's local box (alloy y-up)."
  (let* ((bounds (alloy:bounds cell))
         (w (alloy:pxw bounds))
         (h (alloy:pxh bounds)))
    (ecase edge
      (:right  (alloy:px-extent (max 0 (- w 1)) 0 1 h))
      (:bottom (alloy:px-extent 0 0 w 1)))))

;;; Only the header row carries rules, the way a native list view separates its
;;; columns; rows are told apart by the alternating background.
(presentations:define-realization (window grid-cell)
  ((:background simple:rectangle)
   (alloy:margins)
   :z-index 0)
  ((:sep-right simple:rectangle)
   (alloy:margins)
   :z-index 5)
  ((:sep-bottom simple:rectangle)
   (alloy:margins)
   :z-index 5)
  ((:label simple:text)
   (alloy:margins 6 1)
   alloy:text
   :font (font-entry-font (widget-font-entry alloy:renderable alloy:renderer))
   :size (font-entry-size-unit (widget-font-entry alloy:renderable alloy:renderer))
   :halign :start :valign :middle
   :z-index 10))

(presentations:define-update (window grid-cell)
  (:background :pattern (grid-cell-background alloy:renderable))
  (:sep-right :bounds (grid-cell-edge-bounds alloy:renderable :right)
              :pattern *grid-separator-color*
              :hidden-p (not (grid-cell-header-p alloy:renderable)))
  (:sep-bottom :bounds (grid-cell-edge-bounds alloy:renderable :bottom)
               :pattern *grid-separator-color*
               :hidden-p (not (grid-cell-header-p alloy:renderable)))
  (:label :text alloy:text
          :pattern (grid-cell-foreground alloy:renderable)))

(presentations:define-realization (window grid-view)
  ((:background simple:rectangle)
   (alloy:margins)
   :pattern (chroma:rgb 1.0 1.0 1.0)))

(presentations:define-update (window grid-view)
  (:background :pattern (chroma:rgb 1.0 1.0 1.0)))

(defun grid-cell-text (value)
  (princ-to-string value))

(defun grid-ensure-layout (grid)
  (or (grid-layout grid)
      (let ((layout (make-instance 'alloy:grid-layout
                                   :cell-margins (alloy:margins 0)
                                   :stretch t)))
        (setf (grid-layout grid) layout)
        (alloy:enter layout grid :x 0 :y 0 :w 1 :h 1)
        layout)))

(defun grid-make-cell (grid kind value &key row column)
  (let ((cell (make-instance (grid-cell-class grid)
                             :grid grid :kind kind :row row :column column
                             :cell-value value :text (grid-cell-text value)
                             :on-activate (lambda () nil)
                             :layout-parent (grid-ensure-layout grid)
                             :focus-parent (grid-focus-root grid))))
    (push cell (grid-cells grid))
    cell))

(defun grid-clear-cells (grid)
  (let ((layout (grid-ensure-layout grid)))
    (dolist (cell (copy-list (grid-cells grid)))
      (when (or (and (slot-boundp cell 'alloy:layout-parent)
                     (alloy:layout-parent cell))
                (and (slot-boundp cell 'alloy:focus-parent)
                     (alloy:focus-parent cell)))
        (alloy:leave cell t)))
    (setf (grid-cells grid) nil)
    (alloy:clear layout)))

(defun grid-build-cells (grid rows)
  "Size GRID's inner layout and fill it with the corner, the column headers and one
row header plus the data cells per element of ROWS (a list of (label value ...)),
the row headers labelled from GRID-ROW-LABELS; a selection past the last row is dropped."
  (let ((layout (grid-ensure-layout grid))
        (row-count (length rows))
        (column-count (length (grid-column-labels grid))))
    (setf (alloy:row-sizes layout)
          (make-list (1+ row-count)
                     :initial-element (alloy:px *grid-row-height*))
          (alloy:col-sizes layout)
          (cons (alloy:px *grid-row-label-width*)
                (make-list column-count
                           :initial-element (alloy:px *grid-column-width*))))
    (grid-make-cell grid :corner " ")
    (loop for label in (grid-column-labels grid)
          for column-index from 0
          do (grid-make-cell grid :column-header label :column column-index))
    (loop for row in rows
          for label in (grid-row-labels grid)
          for row-index from 0
          do (grid-make-cell grid :row-header label :row row-index)
             (loop for column-index below column-count
                   do (grid-make-cell grid :data (nth (1+ column-index) row)
                                      :row row-index :column column-index)))
    (when (and (grid-selected-row grid)
               (>= (grid-selected-row grid) row-count))
      (setf (grid-selected-row grid) nil
            (grid-selected-column grid) nil))
    grid))

(defun grid-cell-at (grid row column)
  (find-if (lambda (cell)
             (and (eq :data (grid-cell-kind cell))
                  (eql row (grid-cell-row cell))
                  (eql column (grid-cell-column cell))))
           (grid-cells grid)))

(defun grid-ensure-cell-visible (grid row column)
  (multiple-value-bind (viewport-width viewport-height) (scroll-viewport grid)
    (let* ((left (+ *grid-row-label-width* (* column *grid-column-width*)))
           (right (+ left *grid-column-width*))
           (top (* (1+ row) *grid-row-height*))
           (bottom (+ top *grid-row-height*))
           (scroll-x (scroll-x grid))
           (scroll-y (scroll-y grid))
           (new-x (cond ((< left scroll-x)
                         (if (zerop column) 0 left))
                        ((> right (+ scroll-x viewport-width))
                         (- right viewport-width))
                        (t scroll-x)))
           (new-y (cond ((< top scroll-y)
                         (if (zerop row) 0 top))
                        ((> bottom (+ scroll-y viewport-height))
                         (- bottom viewport-height))
                        (t scroll-y))))
      (set-scroll grid new-x new-y))))

(defun grid-select-cell (grid row column)
  (let ((row-count (length (grid-row-labels grid)))
        (column-count (length (grid-column-labels grid))))
    (when (and (plusp row-count) (plusp column-count))
      (setf (grid-selected-row grid) (max 0 (min row (1- row-count)))
            (grid-selected-column grid) (max 0 (min column (1- column-count))))
      (dolist (cell (grid-cells grid)) (alloy:mark-for-render cell))
      (let ((cell (grid-cell-at grid (grid-selected-row grid)
                                (grid-selected-column grid))))
        (when cell
          (setf (alloy:focused (grid-focus-root grid)) cell)
          (grid-ensure-cell-visible grid
                                    (grid-selected-row grid)
                                    (grid-selected-column grid))))
      (values (grid-selected-row grid) (grid-selected-column grid)))))

(defun grid-move-selection (grid delta-row delta-column)
  (grid-select-cell grid
                    (+ (or (grid-selected-row grid) 0) delta-row)
                    (+ (or (grid-selected-column grid) 0) delta-column)))

(defun grid-order-before-p (left right descending)
  (let ((less (cond ((and (numberp left) (numberp right)) (< left right))
                    (t (string-lessp (princ-to-string left)
                                     (princ-to-string right))))))
    (if descending
        (and (not less) (not (equal left right)))
        less)))

(defmethod alloy:activate ((cell grid-cell))
  (let ((grid (grid-cell-grid cell)))
    (case (grid-cell-kind cell)
      (:column-header
       (grid-header-activated grid (grid-cell-value cell)))
      (:row-header
       (grid-select-cell grid (grid-cell-row cell)
                         (or (grid-selected-column grid) 0)))
      (:data
       (grid-select-cell grid (grid-cell-row cell) (grid-cell-column cell)))))
  cell)

(defmethod alloy:handle ((event alloy:pointer-double-click) (cell grid-cell))
  (if (and (eql :left (alloy:kind event))
           (eq :data (grid-cell-kind cell)))
      (let ((grid (grid-cell-grid cell)))
        (grid-select-cell grid (grid-cell-row cell) (grid-cell-column cell))
        (grid-edit-cell grid (grid-cell-row cell) (grid-cell-column cell))
        t)
      (call-next-method)))

(defmethod alloy:handle ((event alloy:key-down) (cell grid-cell))
  (let ((grid (grid-cell-grid cell)))
    (case (alloy:key event)
      (:left  (grid-move-selection grid 0 -1))
      (:right (grid-move-selection grid 0 1))
      (:up    (grid-move-selection grid -1 0))
      (:down  (grid-move-selection grid 1 0))
      (:tab   (grid-move-selection grid 0
                                   (if (member :shift (alloy:modifiers event)) -1 1)))
      ((:enter :return :kp-enter)
       (when (and (grid-selected-row grid) (grid-selected-column grid))
         (grid-edit-cell grid (grid-selected-row grid) (grid-selected-column grid))))
      (otherwise (call-next-method)))))
