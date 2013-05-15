(in-package :om)

(defvar *general-mixer-window* nil)

(defclass omgenmixer-window (om-dialog)
  ())


(defun make-general-mixer-win ()
  (let ((newwindow (om-make-window 'omgenmixer-window :window-title "OpenMusic General Mixer" 
                                   :size (om-make-point (* *channel-w* 10) 560) 
                                   :position (om-make-point 100 50) :close t :resizable nil)))
    newwindow))

(defun omG-make-genmixer-dialog ()
   (if (and *general-mixer-window* (om-window-open-p *general-mixer-window*))
     (om-select-window *general-mixer-window*)
     (setf *general-mixer-window* (om-select-window (make-general-mixer-win)))))