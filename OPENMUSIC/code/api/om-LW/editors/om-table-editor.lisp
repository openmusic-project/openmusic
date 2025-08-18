;;===========================================================================
;LW General Editors 
;Interface tools 
;;===========================================================================

;===========================================================================
; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; See file LICENSE for further informations on licensing terms.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
; Author: Karim Haddad
;;===========================================================================

;;===========================================================================
;DocFile
;This file loads the LW Score Editors
;;===========================================================================

;;Table editor widget

(in-package :om-edit)

(export '(om-table-window
          open-new-table-editor
          *table-help-window*
          om-vector
          make-vector
          set-vector-elt
          get-vector-elt
         ) :om-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;equivalent a om-text-edit-window

(defclass om-table-window (om-table-editor om-abstract-window) ;ajoute om-table-editor
  ((save-callback :accessor save-callback :initarg :save-callback :initform nil)))

;;;;;;;;;;



;;this is the direct LW editor.
(defclass om-table-editor (capi::interface)
  ((ep :initform nil :accessor ep :initarg :ep)
   (table :initform nil :accessor table :initarg :table)
   (columns :initform nil :accessor columns :initarg :columns)
   (contents :initform t :accessor contents :initarg :contents)
   (box :initform nil :accessor box :initarg :box)
   )
  (:default-initargs
   :title "OM-TABLE"
   :layout (make-instance 'capi:simple-layout)))

(defmethod om-select-window ((self om-table-editor)) 
  (capi::display self))

(defmethod internal-window-class-menubar ((self om-table-editor)) 
  (list (make-instance 'capi::menu 
                       :title "File"
                       :items 
                       (list (make-instance 'capi::menu-component 
                                            :items (list 
                                                    (make-instance 'capi::menu-item :title "New..."
                                                                   :callback-type :none
                                                                                     ;:callback 'open-new-text-editor
                                                                   :accelerator #\n
                                                                                     ;:enabled-function 'file-operations-enabled
                                                                   )
                                                    (make-instance 'capi::menu-item :title "Open..."
                                                                   :callback-type :none
                                                                                  ;:callback 'open-text-file
                                                                  ; :accelerator #\o
                                                                                  ;:enabled-function 'file-operations-enabled
                                                                   )))
                             (make-instance 'capi::menu-component 
                                            :items (list 
                                                    (make-instance 'capi::menu-item :title "Import Csv File"
                                                                   :callback-type :interface
                                                                   :callback 'import-csv-from-file
                                                                   :accelerator #\o
                                                                   )
                                                    (make-instance 'capi::menu-item :title "Export Csv File"
                                                                   :callback-type :interface
                                                                   :callback 'export-csv-from-file
                                                                   :accelerator #\e
                                                                   )
                                                    ))
                             (make-instance 'capi::menu-component 
                                            :items (list 
                                                    (make-instance 'capi::menu-item :title "Save"
                                                                   :callback-type :interface
                                                                                    ; :callback 'save-text-file
                                                                   :accelerator #\s
                                                                                     ;:enabled-function 'save-operation-enabled
                                                                   )
                                                    (make-instance 'capi::menu-item :title "Save As..."
                                                                   :callback-type :interface
                                                                                  ;:callback 'save-as-text-file
                                                                   )))
                             (make-instance 'capi::menu-component 
                                            :items (list 
                                                    (make-instance 'capi::menu-item :title "Get Info"
                                                                   :callback-type :interface
                                                                   :callback 'get-table-info
                                                                   :accelerator #\i
                                                                   )
                                                    
                                                    ))
                             (make-instance 'capi::menu-item :title "Close"
                                            :callback-type :interface
                                            :callback 'close-table-editor-window
                                            :accelerator #\w)
                                       
                             ))
        (make-instance 'capi::menu :title "Edit"
                       :items (list 
                               (make-instance 'capi::menu-item :title "Undo"
                                              :callback-type :interface
                                                            ;:callback 'text-edit-undo
                                              :accelerator #\z)
                               (make-instance 'capi::menu-component 
                                              :items (list 
                                                      (make-instance 'capi::menu-item :title "Cut"
                                                                     :callback-type :interface
                                                                                              ;:callback 'text-edit-cut
                                                                     :accelerator #\x)
                                                      (make-instance 'capi::menu-item :title "Copy"
                                                                     :callback-type :interface
                                                                                              ;:callback 'text-edit-copy
                                                                     :accelerator #\c)
                                                      (make-instance 'capi::menu-item :title "Paste"
                                                                     :callback-type :interface
                                                                                              ;:callback 'text-edit-paste
                                                                     :accelerator #\v)))
                               (make-instance 'capi::menu-item :title "Select All" 
                                                             ;:callback 'text-select-all 
                                              :accelerator #\a
                                              :callback-type :interface)
                               (make-instance 'capi::menu-component 
                                              :items (list (make-instance 'capi::menu-item :title "Text Font"
                                                                          :callback-type :interface
                                                                                              ;:callback 'change-text-edit-font
                                                                          :accelerator nil)
                                                           ))
                               (make-instance 'capi::menu-component 
                                              :items (list 
                                                      (make-instance 'capi::menu-item :title "Find..."
                                                                     :callback-type :interface
                                                                                              ;:callback 'find-in-file
                                                                     :accelerator #\f)
                                                      (make-instance 'capi::menu-item :title "Replace..."
                                                                     :callback-type :interface
                                                                                              ;:callback 'replace-in-file
                                                                     :accelerator #\r)
                                                      ))
                               (make-instance 'capi::menu-item :title "Search..." 
                                                             ;:callback 'search-files 
                                              :accelerator nil
                                                             ;:enabled-function 'disabled
                                              :callback-type :interface)))
        ))

(defparameter *help-window* nil)

(defmethod om-window-class-menubar ((self om-table-editor))  
  (list (make-instance 'capi::menu 
                       :title "Windows" 
                       :callback-type :none
                       :items (list
                               (make-instance 'capi::menu-item 
                                              :title "Listener" 
                                              :accelerator "accelerator-L"
                                              :setup-callback-argument :item
                                              :callback-type :none
                                              :callback #'(lambda () (om-make-new-listener :initial-lambda #'(lambda () (in-package :om)))))
                               (make-instance 
                                      'capi:menu-component
                                      :items (mapcar 
                                              #'(lambda (w) 
                                                  (make-instance 'capi::menu-item 
                                                                 :title (capi::interface-title w)
                                                                 :setup-callback-argument :item
                                                                 :callback-type :none
                                                                 :callback #'(lambda () (capi::find-interface 
                                                                                         (type-of w) 
                                                                                         :name (capi::capi-object-name w)))))
                                              (capi::collect-interfaces 'om-table-editor))
                                      :callback-type :item
                                      :interaction :no-selection)
                               )
                       )
        (make-instance 'capi::menu 
                       :title "Help" 
                       :callback-type :none
                       :items (list
                               (make-instance 'capi::menu-item 
                                              :title "Editor Command Keys..." 
                                              :accelerator "h"
                                              :setup-callback-argument :item
                                              :callback-type :none
                                              :callback *table-help-window* ;defined in om package
                                              )))
        ))


(defmethod om-window-class-menubar ((self t)) nil)


(defun disabled (win) nil)

;; used to get ep (editor panel) for bg customization in :om package
(defmethod om-get-editor-panel ((self om-table-editor)) (ep self))

;; used for finding windows by name
(defmethod capi::interface-match-p ((self om-table-editor) &rest initargs  &key name)
  (string-equal (capi::capi-object-name self) name))

;;; INIT FUNC : depends on the attributes of the window
;;; if file -> open file
;;; if buffer --> edit buffer
;;; else --> create and edit new buffer
;nothing to init. Maybe remove.
(defmethod init-table-editor ((self om-table-editor))
  (capi::execute-with-interface self
                                #'(lambda () 
                                    (with-slots (ep) self
                                      ))
                                ))

;;;=====================
;;; CREATE-WINDOW
;;;=====================

(defun test-nth (n &optional reverse)
  #'(lambda (x y) 
      (funcall (if (numberp (funcall #'nth n x)) #'> #'string-greaterp) 
               (funcall #'nth n (if reverse y x))
               (funcall #'nth n (if reverse x y)))))

(defun spreadsheet-sort-descriptions (columns &key (column-label-fn #'identity))
  (let ((n 0))
    (cons
     (capi:make-sorting-description
      :type :| |
      :sort (test-nth 0)
      :reverse-sort (test-nth 0 t))
     (map 'list #'(lambda (column)
                    (incf n)
                    (capi:make-sorting-description
                     :type (intern (princ-to-string (funcall column-label-fn column)) :keyword)
                     :sort (test-nth n)
                     :reverse-sort (test-nth n t)))
          columns))))


;;;;capi table functions

(defun get-table-rows (table)
  "Returns rows of table (multiple-column-list-panel)"
  (capi:map-collection-items table #'identity t))


(defun change-item-in-table (table new)
  (loop for i in new
        for n from 0 to (length new)
        do (capi:replace-items table (list (capi:get-collection-item table n)) 
                               :start n  
                               :new-selection (list i))))

;NO very expensive with big vectors and unecessary
(defun update-table (table)
  (let ((data (coerce (capi::collection-items table) 'list)))
  (loop for i in data
        for n from 0 to (length data)
        do (capi:replace-items table (list (capi:get-collection-item table n)) 
                               :start n  
                               :new-selection (list i)))))

;use this instead:
(defun update-table-val (table row)
  (let ((data (coerce (capi::collection-items table) 'list)))
    (capi:replace-items table (list (capi:get-collection-item table row)) 
                               :start row  
                               :new-selection (list (nth row data)))))

;same as om::mat-trans. For package comp.
(defun matrans (matrix)
  (let ((maxl (1- (loop for elt in matrix maximize (length elt))))
        result)
    (loop for i from 0 to maxl do
            (push (mapcar #'(lambda (list) (nth i list)) matrix) 
                  result))
    (nreverse result)))

(defun tab-header-callback (interface item)
  (with-slots (ep table contents columns) interface
    (let* ((ncol (position item columns :test 'equal))
           (nrow (position (car (capi::choice-selected-item table)) contents :test 'equal :key 'car))
           (cell (when ncol  (nth (1+ ncol) (capi::get-collection-item table nrow))))
           (cols (matrans contents)))
      (cond  ((= 1 (capi:pane-modifiers-state ep));shift
              (print (list "res" (list nrow ncol)))
              )
             ((= 2 (capi:pane-modifiers-state ep));ctrl
              (run-dialog ep ncol nrow))
             ((= 4 (capi:pane-modifiers-state ep));alt
              (print (list "alt" "cell:" cell)))
             (t 
              (when ncol (capi:display-message (format nil "Clicked on ~c ~a ~%~% item is:~c  ~a ~%~% column is: ~c  ~a" 
                                                        #\Space item  #\Space cell  #\Space
                                                        (nth (1+ ncol) (matrans contents)))))))
      )))


(defun open-row-in-editor (interface item)
    ;(with-slots (ep table contents columns) interface
      ;(print (list "row:" item))
      (print (format nil "row: ~a ~a" (car  item) (cdr item)))
      )



;;; NEW EDITOR
;;; (called from menu "New")

;faire une methode pour generer la sequence
;quand il y a les labels (rows columns) ou pas...

(defun open-new-table-editor (rows columns seq 
                                   &key (title "Spreadsheet") (row-label-fn #'identity) 
                                   (column-label-fn #'identity))
  "Draws a table by applying cell-fn to each combination of an element from the
    rows and columns lists."
  (let* ((sequence 
          (if rows
          (loop for i in seq
                for n in rows
                collect (cons n i))
           (loop for i in seq
                for n from 1 to (length seq)
                 collect (cons n i))))
         (cols (if columns columns (loop for i from 1 to (length (matrans seq)) collect i)))
         (table (make-instance 'capi:multi-column-list-panel 
                               :interaction :single-selection
                               :alternating-background 't
                               :items sequence
                               :header-args '(:selection-callback tab-header-callback ;:sort
                                              )
                               :auto-reset-column-widths 't
                               :reorderable-columns 't ;only for gtk
                               :filter 't ;type in the panel the row label (or index) to get there
                               :columns 
                               (cons '(:width 144 
                                       :title :| |) 
                                     (map 'list 
                                          #'(lambda (column) 
                                              (list 
                                               :default-width 72 
                                               :title (intern
                                                       (princ-to-string 
                                                        (funcall column-label-fn column)) 
                                                       :keyword))) 
                                          cols))
                               :callback-type :collection-data 
                               :action-callback 'open-row-in-editor ;a faire!
                        ; :accelerator #\q
                               :sort-descriptions (spreadsheet-sort-descriptions
                                                   cols 
                                                   :column-label-fn column-label-fn)
                               :external-min-width (+ 144 (* 76 (length cols))) 
                               :external-max-width nil :external-min-height 240))
         
         (win (make-instance 'om-table-editor
                             :contents sequence
                             :layout (make-instance 'capi:column-layout :description (list table)))))
    
    (setf (ep win) win)
    (setf (capi::interface-menu-bar-items win) 
          (append (internal-window-class-menubar win)
                  (om-window-class-menubar win)))
    (setf (table win) table)
    (setf (contents win) sequence)
    (setf (columns win) (loop for i in cols
                              collect (intern (princ-to-string i) :keyword)))
    (capi:display win)))

;Not used...
#|
(defmethod update-editor-after-eval ((self om-table-editor) val)
   ;(setf (object self) val)
   (print (list "update" self val (data val)))
   ;self: om-table-editor
   ;val: om-table 
   )
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;edit-cell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this one all in LW

(capi:define-interface dialog-test ()
  ((ep :initform nil :accessor ep :initarg :ep)
   (ncol :initform nil :accessor ncol :initarg :ncol)
   (nrow :initform nil :accessor nrow :initarg :nrow)
   (entry :initform nil :accessor entry :initarg :entry))
  (:panes
   (input-pane
    capi:text-input-pane
    :accessor dialog-test-input-pane
    :callback-type :interface
    :callback 'exit-dialog-test
    :text  entry  
    )
   (buttons
    capi:push-button-panel
    :items '("OK" "Cancel")
    :layout-args '(:x-uniform-size-p t)
    :callback-type :interface
    :callbacks '(exit-dialog-test capi:abort-dialog)))
  (:layouts
   (default
    capi:column-layout
    '(input-pane buttons)
    :x-adjust :centre
    
    ))
  (:default-initargs
   :title "Enter some text:"
   :visible-min-width 200
   ))

(defun exit-dialog-test (interface);
  (with-slots (input-pane) interface
    (let* ((row (nrow interface))
           (col (ncol interface))
           (tab (table (ep interface)))
           (val (read-from-string (capi:text-input-pane-text input-pane)))
           (vec (make-instance 'om-vector :data (make-vector (get-table-rows tab)))))
      (print (list "check:" val))
      (set-vector-elt vec val row (+ 1 col))
      (update-table-val tab row)   
      (capi:exit-dialog
       (capi:text-input-pane-text
        (dialog-test-input-pane interface))))))

(defun run-dialog (ep ncol nrow)
  (let ((txt (format nil "~a" (nth (1+ ncol) (nth nrow (contents ep))))))
    (capi:display-dialog
   (make-instance 'dialog-test 
                  :ep ep
                  :entry txt
                  :ncol ncol
                  :nrow nrow))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Get-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;displays : number of row, nuber of columns and total number of cells

(defun get-table-info (interface)
  (with-slots (ep) interface
    (let* ((cols (length (columns ep)))
           (data (contents ep))
           (rows (length data))
           cells)
      (loop for i in data
            do (setf cells (append (cdr i) cells)))
      (print (list cols rows (length cells)))
      (capi:display-message 
       (format nil "Rows: ~c ~a ~%~% Columns:~c  ~a ~%~% Cells: ~c  ~a" 
               #\Space rows  #\Space cols  #\Space (length cells)
      )))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;om-table->csv

(defun export-csv-file (list path)
  (let ((pathname (or path (om-choose-new-file-dialog))))
    (WITH-OPEN-FILE (out pathname :direction :output  :if-does-not-exist :create :if-exists :supersede)
      (loop for elt in list 
            do
             (progn
              (loop for e in elt
                      do (format out "~A," e))
              (format out "~%")
              ))
      pathname)))

;(setf *data* '((11 2 3 4 5) (44 55 66 77) (88 99 0 55) (66 77 33 44 66)))
;(export-csv-file *data* nil)

(defmethod export-csv-from-file ((self om-table-editor) &optional path)
  (export-csv-file (contents self) path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;csv->om-table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun replace-all-str (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 

(defun import-csv-file (path)
  (let ((pathname (or path (om-choose-file-dialog))))
    (with-open-file (file pathname :direction :input)
      (let ((current-line (read-line file nil nil)))
        (let* ((str 
                (reduce #'(lambda (s1 s2) (concatenate 'string s1 s2
                                                       ;(quote-str s2)
                                                       ))
                        (cons  (concatenate 'string "(" current-line ")" )
                               (loop while current-line 
                                     collect
                                       (concatenate 'string 
                                                    "(" (setf current-line (read-line file nil nil))")")
                                       ))))
               (str (replace-all-str str "," " "))
               (str (replace-all-str str ";" ""))
               (str-out (remove nil
                                (with-input-from-string (s str)
                                  (let ((r nil))
                                    (do ((line (read s nil 'eof)
                                               (read s nil 'eof)))
                                        ((eql line 'eof))
                                      (push line r))
                                    (reverse r))))))
          ;returns a data list for <data> slot
          str-out
          )))))


;(defmethod om-select-window ((self om-table-editor))  (print (list "self" self ))
;  (capi::find-interface (type-of self) :name (capi::capi-object-name self)))

(defmethod import-csv-from-file ((self om-table-editor) &optional path)
  (with-slots (ep) self
    (when-let* ((path (or path (capi:prompt-for-file "Open File:" 
                                                     :if-does-not-exist :error 
                                                     :filter "*.*"
                                                     :filters '("Csv Files" "*.csv" "Lisp Files" "*.lisp" "Text files" "*.txt" "All Files" "*.*"))))
                (data (import-csv-file path))
                )
      (setf (contents ep) data)
      (setf (columns ep) (loop for i from 1 to (length data) collect i))
      (oa::om-close-window ep)
      ;(display self)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;OM-VECTOR;;;;;;;;;;;;;;;;;;;;;;

(defclass om-vector ()
  ((data :initform nil :accessor data :initarg :data)
   (lgt :initform nil :accessor lgt :initarg :lgt))
  )


(defun make-vector (lst)
  "in order not to have limitation with apply!"
  (let ((vec (make-array (length lst) :fill-pointer 0)))
    (loop for i in lst
          do (vector-push i vec))
    ;then transform array into simple-vector
    (coerce vec 'simple-vector)))

;(type-of (apply #'vector '((1 2 3 4) (1 2 3 4 ) (1 2 3 4))))
;(type-of (make-vector '((1 2 3 4) (1 2 3 4 ) (1 2 3 4))))
;(coerce (make-vector '((1 2 3 4) (1 2 3 4 ) (1 2 3 4))) 'list)

(defmethod set-vector-elt ((self om-vector) (elt t) (rowpos number) (colpos number))
  (when (< rowpos (length (data self)))
  (setf (nth colpos (svref (data self) rowpos)) elt)))

(defmethod get-vector-elt ((self om-vector) (rowpos number) (colpos number))
  (when (< rowpos (length (data self)))
  (nth colpos (svref (data self) rowpos))))

;(setf *myvector* (make-instance 'om-vector :data (vector '(1 2 3 4)  '(20 21 22 32) '(22 33 44 55) '(10 100 1000 100000))))
;(set-vector-elt *myvector* "toto" 3 0)
;(get-vector-elt *myvector* 2 1)
;(data *myvector*)

