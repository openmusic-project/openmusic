;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================
; Author: Karim Haddad

(in-package :om)

;;the OM object

(defclass* om-table (object-in-box)
   ((rows :initform nil :initarg :rows :accessor rows)
    (cols :initform nil :initarg :cols :accessor cols)
    (data :initform nil :initarg :data :accessor data))
   (:icon 958)
   (:documentation "etc."))


(defmethod get-super-default-value ((type (eql 'om-table)))
  (let ((tf (make-instance 'om-table)))
    tf))
 
(defmethod get-type-of-ed-box ((self om-table))  'OMTablebox)
(defmethod editor ((self t)) self)

(defmethod get-slot-in-out-names ((self om-table))
   (values '("self"  "rows" "cols" "data")
           '(nil nil nil nil)
           '("object" "row names" "column names"  "data")
           '(nil nil nil nil)))

;==================EDITOR===============
;;; OM-TABLE EDITOR IS A SPECIAL CASE OF EDITOR 
;;; DIRECTLY MANAGED BY OM-API

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass tabeleditorwindow (om-table-window)
  ((object :initform nil :initarg :object :accessor object)
   (ref :initform nil :initarg :ref :accessor ref)))

(defmethod editor ((self tabeleditorwindow)) self)

(defmethod Class-has-editor-p ((self om-table)) t)

(defmethod get-editor-class ((self om-table)) 'tabeleditorwindow)

(defmethod make-editor-window ((class (eql 'tabeleditorwindow)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
  (open-new-tableedit object ref))



;;; no EDITOR slot
;;; editorframe is the window itself

(defmethod open-new-tableedit ((self om-table) box) 
  (setf (editorframe box) (om-make-window 'tabeleditorwindow
                                          :object self
                                          :ref box
                                          :font (om-make-font "courier" 20)
                                          :window-title (if (file-name self) 
                                                            (namestring (file-name self))
                                                          "TableEditor")
                                          :size (om-make-point 500 400)
                                          :position :centered
                                          :ask-for-save nil))
  (om-set-bg-color (editorframe box) *text-bg-color*)
  (om-add-menu-to-win (editorframe box))
  (editorframe box))

(defmethod om-window-close-event :after ((self tabeleditorwindow))
  (cond ((null (ref self)) nil)
        ((slot-initform-p (ref self))
         (change-initform-ed (ref self) (object self)) )
        ((EditorView-p (ref self))
         (setf (attached-editors (ref self)) 
               (remove self (attached-editors (ref self)) :test 'equal))
         (om-invalidate-view (ref self) t))
         ((ominstance-p (ref self))
          (setf (Editorframe (ref self)) nil))
         ((is-boxpatch-p (ref self)) 
          (setf (Editorframe (ref self)) nil)
          (om-invalidate-view (car (frames (ref self)))))))

;;;PROBLEM ICI si on mets les args dans l'objet
;Not Used
#|
(defmethod update-editor-after-eval ((self tabeleditorwindow) val)(print "update")
   (setf (object self) val))


(defmethod update-editor-after-eval ((self om-edit::om-table-editor) val)
   ;(setf (object self) val)
   (print (list "update" self val (data val)))
   ;self: om-table-editor
   ;val: om-table 
   )
|#

(defmethod update-editor-after-eval ((self oe::om-table-editor) val)
  (setf (oe::box self) val)
  (setf (oe::contents self) (data val))
 ; (print (list "update prob" self val))
nil)

;==================BOX EDITOR================
(defclass OMTablebox (OMBoxEditCall) ())

; ((get-slot-in-out-names-from-class 'textfile)
(defmethod correct-box-inputs ((class (eql 'om-table)) inputs)
  (loop for input in (get-inputs-from-inst (make-instance class))
        for i = 0 then (+ i 1) collect
        (let ((inp 
               (if (and (nth i inputs) (string-equal (name input) (name (nth i inputs))))
                   (nth i inputs) input)))
          ;;; ici : mettre a jour les items (thepopup) pour les input-funmenu
          inp)))



(defmethod OpenEditorframe ((self OMTablebox))
  (let* ((val (value self))
         (rows (rows val))
         (cols (cols val))
         (data (data val)))
  (unless (lock-button (car (frames self)))
    (add-lock-button (car (frames self))))
  (open-new-table-editor rows cols data)))


(defmethod objfromobjs ((self textfile) (type om-table))
  (make-instance (class-of type) :data (exp-list self)))

(defmethod objfromobjs ((self list) (type om-table))
  (make-instance (class-of type) :data self))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass tableEditorFrame (boxEditorFrame) ()
   (:documentation "Simple frame for OMTextFilebox meta objects. #enddoc#
#seealso# (OMTextFilebox) #seealso#"))

(defmethod get-frame-class ((self OMTablebox)) 'tableEditorFrame)

(defmethod close-frame ((box tableEditorFrame))
   "If miniview show a picture we must kill it."   
   (setf (frames (object box)) nil))

(defmethod om-get-menu-context ((object tableeditorframe))
   "Uhmmm If this method had been specialized by the class and not by the frame ?"
   (list+  (boxframe-default-list object)
           (list (list 
                  (om-new-leafmenu "Import Csv From File" #'(lambda () (import-csv-in-box (object object))))
                  (om-new-leafmenu "Export Csv To File" #'(lambda () (export-csv-from-box (object object))))
                  ))))

(defmethod import-csv-in-box ((self OMTablebox))
  (let ((name (om-choose-file-dialog)))
      (when name 
        (let ((data  (import-csv-file name)))
          (setf (data (value self)) data)
          (open-new-table-editor nil nil data)))))

(defmethod export-csv-from-box ((self OMTablebox))
  (let ((name (om-choose-new-file-dialog :prompt "Export Csv file")))
    (when name
      (delete-file-protection name)
      (when name 
        (export-csv-file (data (value self)) name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod OpenObjectEditor ((self OMTablebox))
  "If there is a EditorFrame open for SELF select the window of EditorFrame, 
else create a new Editor frame, and select its window."
  (setf (EditorFrame self) (OpenEditorframe self))
  (setf (oe::box (EditorFrame self)) (value self))
  (when (EditorFrame self)
    (oe::om-select-window (Editorframe self))
    ))

(defmethod numouts ((self OMTablebox)) 4)

(defmethod remove-extra ((self OMPatch) (box OMTablebox))
  (when (EditorFrame box) (om-close-window (EditorFrame box))))

(defmethod (setf value) :before ((val om-table) (self OMTablebox)) nil)

(defmethod fill-buffer-textfile ((self OMTablebox)) nil )

(defparameter *om-table-editor-help*
  (list '((#-macosx("Ctrl + o") #+macosx("Cmd + o") "Import CSV file")
          (#-macosx("Ctrl + e") #+macosx("Cmd + e") "Export CSV file")
          (#-macosx("Ctrl + i") #+macosx("Cmd + i") "Get Info")
          ("ctrl+clic" "Edit cell")
          ;("del" "Delete Selection")
          )
        ))

;;help callback (exported to om-edit package)
(setf *table-help-window* #'(lambda () (show-help-window 
                                        (format nil "Commands for OM-Table Editor") 
                                        *om-table-editor-help*)))