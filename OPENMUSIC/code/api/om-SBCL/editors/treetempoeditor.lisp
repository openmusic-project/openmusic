(in-package :oa)

(defclass om-tree-tempo-editor (om-look-window)
  ((ep            :accessor ep            :initform nil)
   (tree-pane     :accessor tree-pane     :initform nil)
   (tempo-pane    :accessor tempo-pane    :initform nil)
   (tree-echo     :accessor tree-echo     :initform nil)
   (tempo-echo    :accessor tempo-echo    :initform nil)
   (score         :accessor score         :initform nil)
   (lisp-editor?  :accessor lisp-editor?  :initform t :initarg :lisp-editor?)))

(defparameter *os-key*
  #+darwin "CMD" #-darwin "Ctrl")

(defparameter *tree-tempo-about*
  "ABOUT:

This editor operates on VOICEs and POLYs in mode MEASURE or VOICE.
It is important that all trees are well formed.")

(defun %tree-tempo-helpinfo ()
  (with-output-to-string (s)
    (mapcar (lambda (key line)
              (format s "~%~A~A~VT~A~%" *os-key* key 30 line))
            '("+E" "+T" "+S" "+SHIFT+S" "+I" "+SHIFT+I" "+W" "+H")
            '("Set Rhythm Tree" "Set Tempo"
              "Save Rhythm Tree As..." "Save Tempo As..."
              "Import Rhythm Tree"     "Import Tempo"
              "Close Window"           "This Help"))))

(defun %tree-tempo-the-info ()
  (concatenate 'string *tree-tempo-about*
               (format nil "~%SHORTCUTS:~%")
               (%tree-tempo-helpinfo)))

(defun open-help (help)
  (%score-editor-init-defaults)
  (let* ((window    (om-make-window 'om-look-window
                                    :window-title "INFO"
                                    :size      (alloy:px-size 520 430)
                                    :min-size  (alloy:px-size 480 430)
                                    :bg-color *om-window-def-color*))
         (root      (om-window-root window))
         (focus     (om-window-focus window))
         (data      (make-instance 'alloy:value-data :value help))
         (pane      (make-instance 'oa::om-text-edit-view :data data :wrap-p t))
         (scroll    (make-instance 'alloy:scroll-view
                                   :layout pane :focus pane :focus-parent focus :scroll :y))
         (outer     (make-instance 'alloy:border-layout))
         (footer    (make-instance 'alloy:horizontal-linear-layout
                                   :align :start
                                   :cell-margins (alloy:margins 4 *editor-margin*)))
         (close-btn (om-make-dialog-item 'om-button
                                         (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                         "Close"
                                         :di-action (om-dialog-item-act _
                                                      (declare (ignore _))
                                                      (om-close-window window)))))
    (when *def-help-font* (om-set-font pane *def-help-font*))
    (alloy:with-unit-parent root
      (alloy:enter outer root :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1)))
    (alloy:enter (alloy:layout-element scroll) outer :place :center)
    (alloy:enter footer outer :place :south
                 :size (alloy:un (+ *editor-button-h* (* 2 *editor-margin*))))
    (alloy:enter close-btn footer)
    (alloy:enter close-btn focus)
    (%om-focus-pane window pane)
    (alloy:refit (om-window-chrome window))
    (om-show-window window)
    window))

(defun %tree-tempo-set-tree (window)
  (let* ((pane (tree-pane window))
         (txt  (and pane (alloy:value pane)))
         (inner (and (consp (ep window)) (first  (ep window)))))
    (setf *editor-text* (or txt ""))
    (when inner (setf (sel inner) *editor-text*))
    (when inner
      (funcall (intfunc inner) (score inner) *editor-text*))))

(defun %tree-tempo-set-tempo (window)
  (let* ((pane (tempo-pane window))
         (txt  (and pane (alloy:value pane)))
         (inner (and (consp (ep window)) (second (ep window)))))
    (setf *tempo-editor-text* (or txt ""))
    (when inner (setf (sel inner) *tempo-editor-text*))
    (when inner
      (funcall (intfunc inner) (score inner) *tempo-editor-text*))))

(defmethod om-window-class-menubar ((self om-tree-tempo-editor))
  (list
   (om-make-menu
    "File"
    (list (list (om-new-leafmenu "Save Tree As..."   (lambda () (%editor-save-as (tree-pane self)  "Save Tree As"  '*editor-text*))  #\S)
                (om-new-leafmenu "Save Tempo As..."  (lambda () (%editor-save-as (tempo-pane self) "Save Tempo As" '*tempo-editor-text*))))
          (list (om-new-leafmenu "Import Tree From..."  (lambda () (%editor-import (tree-pane self)  "Import Tree"  '*editor-text*))  #\I)
                (om-new-leafmenu "Import Tempo From..." (lambda () (%editor-import (tempo-pane self) "Import Tempo" '*tempo-editor-text*))))))
   (om-make-menu
    "Edit"
    (list (list (om-new-leafmenu "Undo" (lambda () (oa::om-text-undo (tree-pane self))) #\Z)
                (om-new-leafmenu "Redo" (lambda () (oa::om-text-redo (tree-pane self))) #\Y))
          (list (om-new-leafmenu "Cut"   (lambda () (%editor-dispatch-event (tree-pane self) 'alloy:cut-event))  #\X)
                (om-new-leafmenu "Copy"  (lambda () (%editor-dispatch-event (tree-pane self) 'alloy:copy-event)) #\C)
                (om-new-leafmenu "Paste" (lambda () (%editor-paste-from-clipboard self (tree-pane self)))        #\V))
          (list (om-new-leafmenu "Select All" (lambda () (%editor-select-all (tree-pane self))) #\A))
          (list (om-new-leafmenu "Text Font" (lambda ()
                                               (let ((font (om-choose-font-dialog :font *def-tree-edit-font*)))
                                                 (when font
                                                   (setf *def-tree-edit-font* font)
                                                   (om-set-font (tree-pane  self) font)
                                                   (om-set-font (tempo-pane self) font))))))))
   (om-make-menu
    "Actions"
    (list (list (om-new-leafmenu "Set Rhythm Tree" (lambda () (%tree-tempo-set-tree  self)) #\E))
          (list (om-new-leafmenu "Set Tempo"       (lambda () (%tree-tempo-set-tempo self)) #\T))
          (list (om-new-leafmenu "Close"           (lambda () (om-close-window self)) #\W))))
   (om-make-menu
    "Help"
    (list (list (om-new-leafmenu "Shortcuts" (lambda () (open-help (%tree-tempo-the-info))) #\H))))))

(defun open-tree-tempo-editor (panel tree tempo &key (intfunc-tree (constantly nil))
                                                     (intfunc-tempo (constantly nil)))
  (%score-editor-editor-tree-tempo panel tree tempo intfunc-tree intfunc-tempo))

(defun %score-editor-editor-tree-tempo (panel tree tempo intfunc-tree intfunc-tempo)
  (%score-editor-init-defaults)
  (let* ((window      (om-make-window 'om-tree-tempo-editor
                                      :window-title "EDITOR"
                                      :size      (alloy:px-size 1100 340)
                                      :min-size  (alloy:px-size 1100 340)
                                      :bg-color *om-window-def-color*))
         (tree-inner  (make-instance 'om-tree-editor))
         (tempo-inner (make-instance 'om-tempo-editor)))
    (setf (score        window) panel)
    (setf (ep           window) (list tree-inner tempo-inner))
    (setf (score        tree-inner)  panel)
    (setf (sel          tree-inner)  (or tree  ""))
    (setf (intfunc      tree-inner)  intfunc-tree)
    (setf (score        tempo-inner) panel)
    (setf (sel          tempo-inner) (or tempo ""))
    (setf (intfunc      tempo-inner) intfunc-tempo)
    (setf (capi::interface-menu-bar-items window)
          (om-window-class-menubar window))
    (om-add-menu-to-win window)
    (let* ((root            (om-window-root  window))
           (focus           (om-window-focus window))
           (data-t          (make-instance 'alloy:value-data :value (or tree  "")))
           (data-p          (make-instance 'alloy:value-data :value (or tempo "")))
           (tree-pane       (make-instance 'oa::om-paren-edit-view-widget :data data-t
                                           :bg-color *om-white-color*))
           (tempo-pane      (make-instance 'oa::om-paren-edit-view-widget :data data-p
                                           :bg-color *om-white-color*))
           (tree-label      (om-make-dialog-item 'om-static-text
                                                 (om-make-point 0 0) (om-make-point 100 18)
                                                 "TREE:"))
           (tempo-label     (om-make-dialog-item 'om-static-text
                                                 (om-make-point 0 0) (om-make-point 100 18)
                                                 "TEMPO:"))
           (scroll-t        (make-instance 'alloy:scroll-view
                                           :layout tree-pane  :focus tree-pane
                                           :focus-parent focus :scroll :y))
           (scroll-p        (make-instance 'alloy:scroll-view
                                           :layout tempo-pane :focus tempo-pane
                                           :focus-parent focus :scroll :y))
           (outer           (make-instance 'alloy:border-layout))
           (panes-row       (make-instance 'alloy:grid-layout
                                           :col-sizes (list T T)
                                           :row-sizes (list T)
                                           :cell-margins (alloy:margins 2 0)))
           (tree-side       (make-instance 'alloy:border-layout))
           (tempo-side      (make-instance 'alloy:border-layout))
           (tree-footer     (make-instance 'alloy:border-layout))
           (tempo-footer    (make-instance 'alloy:border-layout))
           (tree-defbar     (make-instance 'alloy:horizontal-linear-layout
                                           :align :start
                                           :cell-margins (alloy:margins 2 2)))
           (tempo-defbar    (make-instance 'alloy:horizontal-linear-layout
                                           :align :start
                                           :cell-margins (alloy:margins 2 2)))
           (tree-buttons    (make-instance 'alloy:horizontal-linear-layout
                                           :align :start
                                           :cell-margins (alloy:margins 4 2)))
           (tempo-buttons   (make-instance 'alloy:horizontal-linear-layout
                                           :align :start
                                           :cell-margins (alloy:margins 4 2)))
           (echo            (om-make-dialog-item 'om-static-text
                                                 (om-make-point 0 0) (om-make-point 200 *editor-echo-h*)
                                                 ""
                                                 :bg-color *om-light-gray-color*
                                                 :fg-color *om-black-color*))
           (def-labels      '("Beginning Of Buffer" "End Of Buffer" "Kill Line" "Undo"))
           (set-tree-btn    (om-make-dialog-item 'om-button
                                                 (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                                 "Set"
                                                 :di-action (om-dialog-item-act _
                                                              (declare (ignore _))
                                                              (%tree-tempo-set-tree window))))
           (close-btn       (om-make-dialog-item 'om-button
                                                 (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                                 "Close"
                                                 :di-action (om-dialog-item-act _
                                                              (declare (ignore _))
                                                              (om-close-window window))))
           (set-tempo-btn   (om-make-dialog-item 'om-button
                                                 (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                                 "Set"
                                                 :di-action (om-dialog-item-act _
                                                              (declare (ignore _))
                                                              (%tree-tempo-set-tempo window)))))
      (when *def-tree-edit-font*
        (om-set-font tree-pane  *def-tree-edit-font*)
        (om-set-font tempo-pane *def-tree-edit-font*))
      (alloy:with-unit-parent root
        (alloy:enter outer root :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1)))
      (alloy:enter panes-row outer :place :center)
      (alloy:enter echo      outer :place :south :size (alloy:un *editor-echo-h*))
      (alloy:enter tree-side  panes-row)
      (alloy:enter tempo-side panes-row)
      (alloy:enter tree-label  tree-side  :place :north :size (alloy:un 18))
      (alloy:enter tempo-label tempo-side :place :north :size (alloy:un 18))
      (alloy:enter tree-footer  tree-side  :place :south
                   :size (alloy:un (+ *editor-defbar-h* *editor-button-h*)))
      (alloy:enter tempo-footer tempo-side :place :south
                   :size (alloy:un (+ *editor-defbar-h* *editor-button-h*)))
      (alloy:enter (alloy:layout-element scroll-t) tree-side  :place :center)
      (alloy:enter (alloy:layout-element scroll-p) tempo-side :place :center)
      (alloy:enter tree-defbar   tree-footer  :place :north :size (alloy:un *editor-defbar-h*))
      (alloy:enter tree-buttons  tree-footer  :place :south :size (alloy:un *editor-button-h*))
      (alloy:enter tempo-defbar  tempo-footer :place :north :size (alloy:un *editor-defbar-h*))
      (alloy:enter tempo-buttons tempo-footer :place :south :size (alloy:un *editor-button-h*))
      (dolist (label def-labels)
        (let* ((cmd label)
               (b1 (om-make-dialog-item 'om-button
                                        (om-make-point 0 0) (om-make-point 120 *editor-button-h*)
                                        label
                                        :di-action (om-dialog-item-act _
                                                     (declare (ignore _))
                                                     (%editor-def-buttons-action tree-pane cmd))))
               (b2 (om-make-dialog-item 'om-button
                                        (om-make-point 0 0) (om-make-point 120 *editor-button-h*)
                                        label
                                        :di-action (om-dialog-item-act _
                                                     (declare (ignore _))
                                                     (%editor-def-buttons-action tempo-pane cmd)))))
          (alloy:enter b1 tree-defbar)
          (alloy:enter b1 focus)
          (alloy:enter b2 tempo-defbar)
          (alloy:enter b2 focus)))
      (alloy:enter set-tree-btn  tree-buttons)
      (alloy:enter close-btn     tree-buttons)
      (alloy:enter set-tempo-btn tempo-buttons)
      (alloy:enter set-tree-btn  focus)
      (alloy:enter close-btn     focus)
      (alloy:enter set-tempo-btn focus)
      (%om-focus-pane window tree-pane)
      (setf (tree-pane   window) tree-pane)
      (setf (tempo-pane  window) tempo-pane)
      (setf (tree-echo   window) echo)
      (setf (tempo-echo  window) echo)
      (alloy:refit (om-window-chrome window)))
    (om-show-window window)
    window))
