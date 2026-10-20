;;===========================================================================
;DocFile
; OM TEXT EDIT WINDOW
; & exported functions
;DocFile
;;===========================================================================

(in-package :om-api)

;;; ---- Shared editor defaults (consumed by all editor files) ----

(defparameter *def-comment-edit-font* nil)
(defparameter *def-tree-edit-font*    nil)
(defparameter *def-objinfo-font*      nil)
(defparameter *def-help-font*         nil)
(defparameter *comment-color*         nil)

(defvar *comment-text*       "no comment")
(defvar *comment-frame*      nil)
(defvar *editor-text*        "")
(defvar *tempo-editor-text*  "")
(defvar *info-pack*          nil)

(defparameter info-x       375)
(defparameter info-y       325)
(defparameter info-width   415)
(defparameter info-height  315)

(defparameter *editor-button-h* 22)
(defparameter *editor-defbar-h* 26)
(defparameter *editor-echo-h*   20)
(defparameter *editor-margin*    6)

;;; Direct point sizes, as in vanilla (om-LW editors/commenteditor.lisp: defvar
;;; *def-comment-edit-font*, treeeditor.lisp: defvar *def-tree-edit-font*,
;;; infoeditor.lisp: defvar *def-objinfo-font*, treetempoeditor.lisp: defvar
;;; *def-help-font*): point requests, never OM logical sizes.  Darwin lays text out
;;; at 72 dpi (alloy-desktop/graphics/scale.lisp: typography-dpi): its branches carry the pixel size
;;; these editors render at on win32.
(defun %score-editor-init-defaults ()
  (unless *def-comment-edit-font*
    (setf *def-comment-edit-font*
          #+linux  (%om-make-font-pt "Liberation Sans" 9)
          #+darwin (%om-make-font-pt "Consolas" 44/3)
          #-(or linux darwin) (%om-make-font-pt "Consolas" 11)))
  (unless *def-tree-edit-font*
    (setf *def-tree-edit-font*
          #+linux  (%om-make-font-pt "Liberation Mono" 11)
          #+darwin (%om-make-font-pt "Consolas" 44/3)
          #-(or linux darwin) (%om-make-font-pt "Consolas" 11)))
  (unless *def-objinfo-font*
    (setf *def-objinfo-font*
          #+linux  (%om-make-font-pt "Liberation Mono" 11)
          #+darwin (%om-make-font-pt "Consolas" 44/3)
          #-(or linux darwin) (%om-make-font-pt "Consolas" 11)))
  (unless *def-help-font*
    (setf *def-help-font*
          #+linux  (%om-make-font-pt "Courier" 10)
          #+darwin (%om-make-font-pt "Consolas" 16)
          #-(or linux darwin) (%om-make-font-pt "Consolas" 12)))
  (unless *comment-color* (setf *comment-color* (om-make-color 0 0 0))))

;;; ---- Generic editor pane operations ----

(defun %editor-dispatch-event (pane event-class)
  (when pane (alloy:handle (make-instance event-class) pane)))

(defun %editor-paste-from-clipboard (window pane)
  (let ((clip (and window pane (alloy:clipboard window))))
    (when (and pane clip (stringp clip))
      (alloy:handle (make-instance 'alloy:paste-event :content clip) pane))))

(defun %editor-select-all (pane)
  (when pane
    (let ((cursor (alloy:cursor pane)))
      (setf (alloy:anchor cursor) 0)
      (alloy:move-to :end cursor)
      (alloy:mark-for-render pane))))

(defun %editor-change-font (pane font-var)
  (let* ((font (om-choose-font-dialog
                :font (or (symbol-value font-var) (%om-make-font-pt "Consolas" 11)))))
    (when (and pane font)
      (setf (symbol-value font-var) font)
      (om-set-font pane font))))

(defun %editor-change-fg-color (pane color-var)
  (let* ((color (om-choose-color-dialog :color (or (symbol-value color-var) (om-make-color 0 0 0))
                                        :full-open nil)))
    (when (and pane color)
      (setf (symbol-value color-var) color)
      (om-set-fg-color pane color))))

(defun %editor-save-as (pane prompt text-var)
  (let ((path (om-choose-new-file-dialog :prompt prompt)))
    (when (and path pane)
      (let ((txt (or (alloy:value pane) "")))
        (when (%write-file-text path txt)
          (when text-var (set text-var txt)))))))

(defun %editor-import (pane prompt text-var)
  (let ((path (om-choose-file-dialog :prompt prompt)))
    (when (and path pane)
      (let ((text (%read-file-text path)))
        (when text
          (setf (alloy:value pane) text)
          (alloy:mark-for-render pane)
          (when text-var (set text-var text)))))))

;;; ---- om-editor-window: base class for all single-pane editors ----

(defclass om-editor-window (om-look-window)
  ((edit-pane    :accessor edit-pane    :initform nil)
   (def-buttons  :accessor def-buttons  :initform nil)
   (bottom-bar   :accessor bottom-bar   :initform nil)
   (echo-area    :accessor echo-area    :initform nil)))

(defun %score-place (widget root x kernel-y w h)
  "Place WIDGET in ROOT at kernel Y-DOWN coords (x,kernel-y) sized (w,h)."
  (alloy:enter widget root :x x :y 0 :w w :h h)
  (om-set-view-position widget (om-make-point x kernel-y))
  widget)

(defun %editor-def-buttons-action (pane cmd)
  (when pane
    (let ((cursor (alloy:cursor pane))
          (text   (or (alloy:value pane) "")))
      (cond ((string= cmd "Beginning Of Buffer")
             (alloy:move-to 0 cursor)
             (setf (alloy:anchor cursor) nil)
             (alloy:mark-for-render pane))
            ((string= cmd "End Of Buffer")
             (alloy:move-to (length text) cursor)
             (setf (alloy:anchor cursor) nil)
             (alloy:mark-for-render pane))
            ((string= cmd "Kill Line")
             (let* ((pos (alloy:pos cursor))
                    (eol (or (position #\Linefeed text :start pos) (length text))))
               (when (> eol pos)
                 (alloy::delete-text pos eol pane)
                 (alloy:mark-for-render pane))))
            ((string= cmd "Undo")
             nil)))))

(defun %install-tree-tempo-style-editor (window initial-text font def-labels set-action)
  "Install border-layout chrome for tree/tempo editors: text-pane + def-buttons row + 2 bottom-buttons + echo."
  (let* ((root          (om-window-root  window))
         (focus         (om-window-focus window))
         (data          (make-instance 'alloy:value-data :value (or initial-text "")))
         (pane          (make-instance 'oa::om-paren-edit-view-widget :data data
                                       :bg-color *om-white-color*))
         (scroll        (make-instance 'alloy:scroll-view
                                       :layout pane :focus pane
                                       :focus-parent focus :scroll :y))
         (outer         (make-instance 'alloy:border-layout))
         (footer        (make-instance 'alloy:border-layout))
         (scroll-spacer (make-instance 'alloy:fixed-layout))
         (middle        (make-instance 'alloy:border-layout))
         (defbar-row    (make-instance 'alloy:horizontal-linear-layout
                                       :align :start
                                       :cell-margins (alloy:margins 2 2)))
         (buttons-row   (make-instance 'alloy:horizontal-linear-layout
                                       :align :start
                                       :cell-margins (alloy:margins 4 2)))
         (echo          (om-make-dialog-item 'om-static-text
                                              (om-make-point 0 0) (om-make-point 200 *editor-echo-h*)
                                              ""
                                              :bg-color *om-light-gray-color*
                                              :fg-color *om-black-color*))
         (set-btn       (om-make-dialog-item 'om-button
                                              (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                              "Set"
                                              :di-action (om-dialog-item-act _
                                                           (declare (ignore _))
                                                           (funcall set-action))))
         (close-btn     (om-make-dialog-item 'om-button
                                              (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                              "Close"
                                              :di-action (om-dialog-item-act _
                                                           (declare (ignore _))
                                                           (om-close-window window)))))
    (when font (om-set-font pane font))
    (alloy:with-unit-parent root
      (alloy:enter outer root :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1)))
    (alloy:enter (alloy:layout-element scroll) outer :place :center)
    (alloy:enter footer outer :place :south
                 :size (alloy:un (+ *editor-echo-h* *editor-button-h* *editor-defbar-h*)))
    (alloy:enter echo          footer :place :south :size (alloy:un *editor-echo-h*))
    (alloy:enter scroll-spacer footer :place :east  :size (alloy:un 20))
    (alloy:enter middle        footer :place :center)
    (alloy:enter defbar-row    middle :place :north :size (alloy:un *editor-defbar-h*))
    (alloy:enter buttons-row   middle :place :south :size (alloy:un *editor-button-h*))
    (let ((def-buttons nil))
      (loop for label in def-labels
            do (let* ((cmd label)
                      (b (om-make-dialog-item 'om-button
                                              (om-make-point 0 0) (om-make-point 120 *editor-button-h*)
                                              label
                                              :di-action (om-dialog-item-act _
                                                           (declare (ignore _))
                                                           (%editor-def-buttons-action pane cmd)))))
                 (alloy:enter b defbar-row)
                 (alloy:enter b focus)
                 (push b def-buttons)))
      (setf (def-buttons window) (nreverse def-buttons)))
    (alloy:enter set-btn   buttons-row)
    (alloy:enter close-btn buttons-row)
    (alloy:enter set-btn   focus)
    (alloy:enter close-btn focus)
    (%om-focus-pane window pane)
    (setf (edit-pane   window) pane)
    (setf (bottom-bar  window) (list set-btn close-btn))
    (setf (echo-area   window) echo)
    (alloy:refit (om-window-chrome window))
    pane))
