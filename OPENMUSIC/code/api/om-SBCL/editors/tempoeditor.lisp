(in-package :oa)

(defclass om-tempo-editor (om-editor-window)
  ((ep           :accessor ep           :initform nil)
   (score        :accessor score        :initform nil)
   (sel          :accessor sel          :initform "")
   (intfunc      :accessor intfunc      :initform (constantly nil))
   (lisp-editor? :accessor lisp-editor? :initform t :initarg :lisp-editor?)))

(defmethod om-window-class-menubar ((self om-tempo-editor))
  (list
   (om-make-menu
    "File"
    (list (list (om-new-leafmenu "Save As..."     (lambda () (%editor-save-as (edit-pane self) "Save Tempo As"  '*tempo-editor-text*)) #\S))
          (list (om-new-leafmenu "Import From..." (lambda () (%editor-import  (edit-pane self) "Import Tempo"  '*tempo-editor-text*)) #\I))))
   (om-make-menu
    "Edit"
    (list (list (om-new-leafmenu "Undo" (lambda () (oa::om-text-undo (edit-pane self))) #\Z)
                (om-new-leafmenu "Redo" (lambda () (oa::om-text-redo (edit-pane self))) #\Y))
          (list (om-new-leafmenu "Cut"   (lambda () (%editor-dispatch-event (edit-pane self) 'alloy:cut-event))  #\X)
                (om-new-leafmenu "Copy"  (lambda () (%editor-dispatch-event (edit-pane self) 'alloy:copy-event)) #\C)
                (om-new-leafmenu "Paste" (lambda () (%editor-paste-from-clipboard self (edit-pane self)))        #\V))
          (list (om-new-leafmenu "Select All" (lambda () (%editor-select-all (edit-pane self))) #\A))
          (list (om-new-leafmenu "Text Font" (lambda () (%editor-change-font (edit-pane self) '*def-tree-edit-font*))))))
   (om-make-menu
    "Actions"
    (list (list (om-new-leafmenu "Set Tempo" (lambda () (%tempo-action-set self)) #\T))
          (list (om-new-leafmenu "Close"     (lambda () (om-close-window self)) #\W))))))

(defun %tempo-action-set (window)
  (let* ((pane (edit-pane window))
         (txt  (and pane (alloy:value pane))))
    (setf *tempo-editor-text* (or txt ""))
    (setf (sel window) *tempo-editor-text*)
    (funcall (intfunc window) (score window) *tempo-editor-text*)))

(defun open-tempo-editor (panel tree &key (intfunc (constantly nil)))
  (%score-editor-init-defaults)
  (let ((window (om-make-window 'om-tempo-editor
                                :window-title "TEMPO EDITOR"
                                :size      (alloy:px-size 480 380)
                                :min-size  (alloy:px-size 300 260)
                                :bg-color *om-window-def-color*)))
    (setf (ep      window) window)
    (setf (score   window) panel)
    (setf (sel     window) (or tree ""))
    (setf (intfunc window) intfunc)
    (setf (capi::interface-menu-bar-items window)
          (om-window-class-menubar window))
    (om-add-menu-to-win window)
    (%install-tree-tempo-style-editor window
                                      (or tree *tempo-editor-text*)
                                      *def-tree-edit-font*
                                      '("Beginning Of Buffer" "End Of Buffer" "Kill Line" "Undo")
                                      (lambda () (%tempo-action-set window)))
    (om-show-window window)
    window))
