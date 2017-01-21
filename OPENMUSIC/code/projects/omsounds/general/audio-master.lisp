
(in-package :om)

(defvar *audio-master-gain* 1.0)
(defvar *audio-master-window* nil)

;;; do nothing for the moment..

(defun show-audio-master-win ()
  (if (and *audio-master-window* (om-window-open-p *audio-master-window*)) 
      (om-select-window *audio-master-window*)
    (setf *audio-master-window* 
          (let ((win (om-make-window 
                      'om-window
                      :window-title "Master"
                      :size (om-make-point 140 230)
                      :resizable nil
                      :bg-color *om-window-def-color*))
                (text  (om-make-dialog-item 
                        'om-static-text
                        (om-make-point 80 100)
                        (om-make-point 60 60)
                        (format nil "~,2f" *audio-master-gain*)
                        :font *om-default-font3b*)))

            (om-add-subviews win
                             text
                             (om-make-dialog-item 
                              'om-slider
                              (om-make-point 35 10)
                              (om-make-point 20 200)
                              "Gain"
                              :range '(0 100)
                              :value (round (* *audio-master-gain* 100))
                              :di-action #'(lambda (item)
                                             (setf *audio-master-gain* (* (om-slider-value item) 0.01))
                                             (om-set-dialog-item-text text (format nil "~,2f" *audio-master-gain*)))
                              ))
            win))
    ))