(in-package :oa)

;;; OUTPUT

(defvar *out-buffer* nil)
(defvar *out-window* nil)

(defclass output-win (oa::om-look-window)
  ((output-pane   :accessor output-pane   :initform nil)
   (output-scroll :accessor output-scroll :initform nil)))

(defmethod alloy:handle :around
    ((event org.shirakumo.alloy.windowing:close) (window output-win))
  "Close hides the reusable vanilla output interface instead of destroying it."
  (declare (ignore event))
  (when (%glfw:pointer window)
    (setf (%glfw:should-close-p window) nil))
  (om-hide-window window)
  t)

(defmethod internal-window-class-menubar ((window output-win))
  (list (om-make-menu
         "File"
         (list (list (om-new-leafmenu "Close"
                                      (lambda () (om-hide-window window))
                                      #\w))))))

(defun %output-render-text ()
  (or (and *out-buffer* (om-buffer-text *out-buffer*)) ""))

(defun show-output-window (&optional windowtitle)
  (alld:call-on-ui-thread
   (lambda ()
     (unless (and *out-window* (om-window-open-p *out-window*))
       (setf *out-window*
             (let* ((win   (om-make-window 'output-win
                                           :window-title "Documentation Window"
                                           :size     (alloy:px-size 500 400)
                                           :min-size (alloy:px-size 240 180)
                                           :bg-color *om-window-def-color*))
                    (root  (om-window-root win))
                    (focus (om-window-focus win))
                    (data  (make-instance 'alloy:value-data
                                          :value (%output-render-text)))
                    (pane  (make-instance 'om-output-collector-pane :data data))
                    (scroll (make-instance 'alloy:scroll-view
                                           :layout pane :focus pane
                                           :focus-parent focus :scroll :y))
                    (outer (make-instance 'alloy:border-layout)))
               (alloy:with-unit-parent root
                 (alloy:enter outer root :x 0 :y 0
                              :w (alloy:pw 1) :h (alloy:ph 1)))
               (alloy:enter (alloy:layout-element scroll) outer :place :center)
               ;; direct point size (om-LW lw-lisp-tools/output.lisp: the :font of
               ;; the collector-pane in show-output-window)
               (om-set-font pane (%om-make-font-pt "Verdana" 10))
               (om-set-bg-color pane *om-white-color*)
               (om-set-fg-color pane *om-black-color*)
               (setf (output-pane win) pane
                     (output-scroll win) scroll)
               (alloy:refit (om-window-chrome win))
               (setf (capi::interface-menu-bar-items win)
                     (internal-window-class-menubar win))
               (om-add-menu-to-win win)
               win)))
     (let ((pane (output-pane *out-window*)))
       (when pane
         (setf (alloy:text pane) (%output-render-text))
         (let ((cursor (alloy:cursor pane)))
           (setf (alloy:anchor cursor) nil)
           (alloy:move-to 0 cursor))
         (alloy:notice-size pane t)
         (alloy:mark-for-render pane)))
     (when windowtitle (om-set-window-title *out-window* windowtitle))
     (om-show-window *out-window*)
     *out-window*)))

(defun om-show-output-lines (lines &optional windowtitle)
  (unless *out-buffer*
    (setf *out-buffer* (om-make-buffer)))
  (om-buffer-delete *out-buffer*)
  (cond ((consp lines)
         (mapc #'(lambda (line) (om-buffer-insert *out-buffer* (format nil "~A~%" line))) lines))
        (t
         (om-buffer-insert *out-buffer* lines)))
  (show-output-window windowtitle))

