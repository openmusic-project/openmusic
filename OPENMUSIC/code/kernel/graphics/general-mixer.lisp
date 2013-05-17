(in-package :om)

(defvar *general-mixer-window* nil)

(defclass omgenmixer-window (om-window)
  ())

(defclass omgenmixer-view (om-view) ())

(defun make-general-mixer-win ()
  (let ((newwindow (om-make-window 'omgenmixer-window :window-title "OpenMusic General Mixer" 
                                   :size (om-make-point (+ 5 (* *channel-w* 10)) 560) 
                                   :scrollbars :h
                                   :position (om-make-point 100 50) :close t :resizable nil))
        panel)
    (setf panel (om-make-view 'omgenmixer-view
                                             :owner newwindow
                                             :position (om-make-point 0 0) 
                                             :scrollbars :h
                                             :retain-scrollbars t
                                             :field-size  (om-make-point (+ 5 (* *channel-w* las-channels)) 540)
                                             :size (om-make-point (w newwindow) (h newwindow))))
    (om-add-subviews newwindow panel)
    (loop for i from 0 to (- las-channels 1) do
          (om-add-subviews panel (genmixer-make-single-channel-view panel i)
                           ;(om-make-view 'om-view 
                           ;              :owner panel
                           ;              :position (om-make-point (+ 5 (* i *channel-w*)) 5) 
                           ;              :scrollbars nil
                           ;              :retain-scrollbars nil
                           ;              :field-size  (om-make-point (- *channel-w* 5) 535)
                           ;              :size (om-make-point (- *channel-w* 5) 535)
                           ;              :bg-color *om-light-gray-color*)
                           ))
    newwindow))

(defun omG-make-genmixer-dialog ()
   (if (and *general-mixer-window* (om-window-open-p *general-mixer-window*))
     (om-select-window *general-mixer-window*)
     (setf *general-mixer-window* (om-select-window (make-general-mixer-win)))))

(defun genmixer-make-single-channel-view (panel channel)
  (let ((main-view (om-make-view 'om-view 
                                 :owner panel
                                 :position (om-make-point (+ 5 (* channel *channel-w*)) 5) 
                                 :scrollbars nil
                                 :retain-scrollbars nil
                                 :field-size  (om-make-point (- *channel-w* 5) 535)
                                 :size (om-make-point (- *channel-w* 5) 535)
                                 :bg-color *om-light-gray-color*))
        (pos 8)
        channel-text
        bar1
        pan-text
        pan-val
        pan-slider
        bar2
        vol-text
        vol-val
        bar3
        )
    (setf channel-text (om-make-dialog-item 'om-static-text
                                            (if (< channel 10) (om-make-point 4 pos) (om-make-point 1 pos))
                                            (om-make-point 80 20) (format nil "CHANNEL ~A" (+ 1 channel)) 
                                            :font *om-default-font1*))
    (incf pos 20)
    (setf bar1 (om-make-view 'bar-item 
                             :position (om-make-point 3 pos) 
                             :size (om-make-point 69 10)
                             :bg-color *om-light-gray-color*))
    (incf pos 10)
    (setf pan-text (om-make-dialog-item 'om-static-text
                                        (om-make-point 14 pos) 
                                        (om-make-point 40 16)
                                        "Pan"
                                        :font *om-default-font2*
                                        ))
    (setf pan-val (om-make-dialog-item 'om-static-text
                                             (om-make-point 45 pos) 
                                             (om-make-point 30 16)
                                             "64"
                                             :font *om-default-font2*
                                             ))
    (incf pos 16)
    (setf pan-slider (om-make-view 'graphic-numbox :position (om-make-point 28 pos) 
                                   :size (om-make-point 20 20)
                                   :pict (om-load-and-store-picture "dial" 'di)
                                   :nbpict 65
                                   :pict-size (om-make-point 24 24)
                                   :di-action (om-dialog-item-act item
                                                )
                                   :font *om-default-font2*
                                   :value 64
                                   :min-val 0
                                   :max-val 127))
    (incf pos 25)
    (setf bar2 (om-make-view 'bar-item 
                             :position (om-make-point 3 pos) 
                             :size (om-make-point 69 10)
                             :bg-color *om-light-gray-color*))

    (om-add-subviews main-view 
                     channel-text
                     bar1
                     pan-text
                     pan-val
                     pan-slider
                     bar2
                     ;vol-text
                     ;vol-val
                     ;bar3
                     )
    main-view)
  )
