(in-package :oa)

(defclass om-comment-editor (om-editor-window)
  ((ep           :accessor ep           :initform nil)
   (panel        :accessor panel        :initform nil)
   (mycomment    :accessor mycomment    :initform "")
   (intfunc      :accessor intfunc      :initform (constantly nil))
   (fontfunc     :accessor fontfunc     :initform (constantly nil))
   (fontcolfunc  :accessor fontcolfunc  :initform (constantly nil))
   (resizefunc   :accessor resizefunc   :initform (constantly nil))))

(defmethod om-window-class-menubar ((self om-comment-editor))
  (list
   (om-make-menu
    "File"
    (list (list (om-new-leafmenu "Save As..."     (lambda () (%editor-save-as (edit-pane self) "Save Comment As" '*comment-text*)) #\S))
          (list (om-new-leafmenu "Import Text..." (lambda () (%editor-import  (edit-pane self) "Import Text"     '*comment-text*)) #\I))))
   (om-make-menu
    "Edit"
    (list (list (om-new-leafmenu "Undo" (lambda () (oa::om-text-undo (edit-pane self))) #\Z)
                (om-new-leafmenu "Redo" (lambda () (oa::om-text-redo (edit-pane self))) #\Y))
          (list (om-new-leafmenu "Cut"   (lambda () (%editor-dispatch-event (edit-pane self) 'alloy:cut-event))  #\X)
                (om-new-leafmenu "Copy"  (lambda () (%editor-dispatch-event (edit-pane self) 'alloy:copy-event)) #\C)
                (om-new-leafmenu "Paste" (lambda () (%editor-paste-from-clipboard self (edit-pane self)))        #\V))
          (list (om-new-leafmenu "Select All" (lambda () (%editor-select-all (edit-pane self))) #\A))
          (list (om-new-leafmenu "Text Font"  (lambda () (%editor-change-font     (edit-pane self) '*def-comment-edit-font*)) #\F)
                (om-new-leafmenu "Text Color" (lambda () (%editor-change-fg-color (edit-pane self) '*comment-color*))))))
   (om-make-menu
    "Actions"
    (list (list (om-new-leafmenu "Set Comment Text..."  (lambda () (%comment-action-set self t))  #\Q))
          (list (om-new-leafmenu "Edit Comment Text..." (lambda () (%comment-action-set self nil)) #\E))
          (list (om-new-leafmenu "Close"                (lambda () (om-close-window self))         #\W))))))

(defun %comment-action-set (window close-after-p)
  ;; every action rebuilds the box frame and the text view inside it (kernel
  ;; boxframe.lisp: reinit-size for commentboxframe), so the panel is re-aimed at
  ;; the live one before and after the chain
  (let* ((pane (edit-pane window))
         (txt  (and pane (alloy:value pane))))
    (setf *comment-text* (or txt ""))
    (setf (mycomment window) *comment-text*)
    (let ((live (comment-live-panel)))
      (when live (setf (panel window) live)))
    (when (panel window)
      (om-set-dialog-item-text (panel window) *comment-text*)
      (funcall (intfunc window)     (panel window) *comment-text*)
      (funcall (fontfunc window)    (panel window) *def-comment-edit-font*)
      (funcall (fontcolfunc window) (panel window) *comment-color*)
      ;; resizefunc alone takes the FRAME, not the panel: the caller binds it to
      ;; a method specialized on the box frame and leaves the frame in
      ;; *comment-frame* (om-LW editors/commenteditor.lisp: comment-button-action)
      (funcall (resizefunc window)  *comment-frame*)
      (let ((live (comment-live-panel)))
        (when live (setf (panel window) live))))
    (when close-after-p
      (om-close-window window))))

(defun open-comment-editor-pane (panel &key (initial-text nil)
                                            (intfunc     (constantly nil))
                                            (fontfunc    (constantly nil))
                                            (fontcolfunc (constantly nil))
                                            (resizefunc  (constantly nil)))
  (%score-editor-init-defaults)
  (let ((window (om-make-window 'om-comment-editor
                                :window-title "Add comment..."
                                :size      (alloy:px-size 460 360)
                                :min-size  (alloy:px-size 240 230)
                                :bg-color *om-window-def-color*)))
    (setf (ep          window) window)
    (setf (panel       window) panel)
    (setf (intfunc     window) intfunc)
    (setf (fontfunc    window) fontfunc)
    (setf (fontcolfunc window) fontcolfunc)
    (setf (resizefunc  window) resizefunc)
    (setf (capi::interface-menu-bar-items window)
          (om-window-class-menubar window))
    (om-add-menu-to-win window)
    (let* ((root        (om-window-root  window))
           (focus       (om-window-focus window))
           (data        (make-instance 'alloy:value-data
                                       :value (or initial-text *comment-text*)))
           ;; the paren-match family, which is also the syntax-coloured one: vanilla
           ;; builds this editor with :lisp-editor? t (om-LW editors/commenteditor.lisp:
           ;; open-comment-editor-pane)
           (pane        (make-instance 'oa::om-paren-edit-view-widget :data data :wrap-p t))
           (scroll      (make-instance 'alloy:scroll-view
                                       :layout pane :focus pane
                                       :focus-parent focus :scroll :y))
           (outer        (make-instance 'alloy:border-layout))
           (footer       (make-instance 'alloy:border-layout))
           (scroll-spacer (make-instance 'alloy:fixed-layout))
           (buttons-row  (make-instance 'alloy:horizontal-linear-layout
                                        :align :start
                                        :cell-margins (alloy:margins 4 2)))
           (echo         (om-make-dialog-item 'om-static-text
                                              (om-make-point 0 0) (om-make-point 200 *editor-echo-h*)
                                              ""
                                              :bg-color *om-light-gray-color*
                                              :fg-color *om-black-color*))
           (set-btn      (om-make-dialog-item 'om-button
                                              (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                              "Set"
                                              :di-action (om-dialog-item-act _
                                                           (declare (ignore _))
                                                           (%comment-action-set window t))))
           (edit-btn     (om-make-dialog-item 'om-button
                                              (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                              "Edit"
                                              :di-action (om-dialog-item-act _
                                                           (declare (ignore _))
                                                           (%comment-action-set window nil))))
           (close-btn    (om-make-dialog-item 'om-button
                                              (om-make-point 0 0) (om-make-point 80 *editor-button-h*)
                                              "Close"
                                              :di-action (om-dialog-item-act _
                                                           (declare (ignore _))
                                                           (om-close-window window)))))
      (when *def-comment-edit-font* (om-set-font pane *def-comment-edit-font*))
      (alloy:with-unit-parent root
        (alloy:enter outer root
                     :x 0 :y 0 :w (alloy:pw 1) :h (alloy:ph 1)))
      (alloy:enter (alloy:layout-element scroll) outer :place :center)
      (alloy:enter footer outer :place :south
                   :size (alloy:un (+ *editor-echo-h* *editor-button-h*)))
      (alloy:enter echo footer :place :south :size (alloy:un *editor-echo-h*))
      (alloy:enter scroll-spacer footer :place :east :size (alloy:un 20))
      (alloy:enter buttons-row footer :place :center)
      (dolist (btn (list set-btn edit-btn close-btn))
        (alloy:enter btn buttons-row)
        (alloy:enter btn focus))
      (%om-focus-pane window pane)
      (setf (edit-pane  window) pane)
      (setf (bottom-bar window) (list set-btn edit-btn close-btn))
      (setf (echo-area  window) echo)
      (om-set-bg-color pane (om-make-color 1.0 1.0 0.0))
      (when *comment-color* (om-set-fg-color pane *comment-color*))
      (alloy:refit (om-window-chrome window)))
    (om-show-window window)
    window))
