(in-package :om)

(defvar *general-mixer-window* nil)
(defvar *general-mixer-values* (make-hash-table))
(defun init-genmixer-values ()
  (loop for i from 0 to (- las-channels 1) do
        (setf (gethash i *general-mixer-values*) (list 0 100))))

(init-genmixer-values)

(defun build-faust-pool-list ()
  (let ((n (- (las-get-number-faust-effects-register) 1))
        (final-list (list "-" "No Effect"))) 
    (loop for i from 0 to n do
          (if (not (las-faust-effect-already-plugged-? (nth 0 (gethash i *faust-effects-register*))))
              (setf final-list (append final-list (list (nth 2 (gethash i *faust-effects-register*)))))))
    final-list))




(defclass omgenmixer-window (om-window)
  ())

(defclass omgenmixer-view (om-view) ())

(defun make-general-mixer-win ()
  (let ((newwindow (om-make-window 'omgenmixer-window :window-title "OpenMusic General Mixer" 
                                   :size (om-make-point (+ 5 (* *channel-w* 10)) 370) 
                                   :scrollbars :h
                                   :position (om-make-point 100 50) :close t :resizable nil))
        panel)
    (setf panel (om-make-view 'omgenmixer-view
                              :owner newwindow
                              :position (om-make-point 0 0) 
                              :scrollbars :h
                              :retain-scrollbars t
                              :bg-color *om-steel-blue-color*
                              :field-size  (om-make-point (+ 5 (* *channel-w* las-channels)) 350)
                              :size (om-make-point (w newwindow) (h newwindow))))
    (om-add-subviews newwindow panel)
    (loop for i from 0 to (- las-channels 1) do
          (om-add-subviews panel (genmixer-make-single-channel-view panel i)))
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
                                 :field-size  (om-make-point (- *channel-w* 5) 345)
                                 :size (om-make-point (- *channel-w* 5) 345)
                                 :bg-color *om-light-gray-color*))
        (pos 8)
        (volval (cadr (gethash channel *general-mixer-values*)))
        (panval (car (gethash channel *general-mixer-values*)))
        (effectlist (build-faust-pool-list))
        channel-text bar1 pan-text pan-val pan-slider bar2 vol-text vol-val vol-slider
        bar3 faust-text effect1 effect2 effect3 effect4 effect5)

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
                                        :font *om-default-font2*))
    (setf pan-val (om-make-dialog-item 'om-static-text
                                       (om-make-point 45 pos) 
                                       (om-make-point 30 16)
                                       (number-to-string panval)
                                       :font *om-default-font2*))

    (incf pos 16)
    (setf pan-slider (om-make-view 'graphic-numbox :position (om-make-point 28 pos) 
                                   :size (om-make-point 20 20)
                                   :pict (om-load-and-store-picture "dial" 'di)
                                   :nbpict 65
                                   :pict-size (om-make-point 24 24)
                                   :di-action (om-dialog-item-act item
                                                (change-genmixer-channel-pan (+ channel 1) (value item))
                                                (om-set-dialog-item-text pan-val (number-to-string (value item))))
                                   :font *om-default-font2*
                                   :value panval
                                   :min-val -100
                                   :max-val 100))

    (incf pos 25)
    (setf bar2 (om-make-view 'bar-item 
                             :position (om-make-point 3 pos) 
                             :size (om-make-point 69 10)
                             :bg-color *om-light-gray-color*))

    (incf pos 10)
    (setf vol-text (om-make-dialog-item 'om-static-text 
                                        (om-make-point 11 pos) 
                                        (om-make-point 40 16)
                                        "Vol"
                                        :font *om-default-font2*))
    (setf vol-val (om-make-dialog-item 'om-static-text 
                                       (om-make-point 40 pos) 
                                       (om-make-point 30 16)
                                       (number-to-string volval)
                                       :font *om-default-font2*))
    
    (incf pos 20)
    (setf vol-slider (om-make-dialog-item 'om-slider  
                                          (om-make-point 20 pos) 
                                          (om-make-point 30 100) ""
                                          :di-action (om-dialog-item-act item
                                                       (change-genmixer-channel-vol (+ channel 1) (om-slider-value item))
                                                       (om-set-dialog-item-text vol-val (number-to-string (om-slider-value item))))
                                          :increment 1
                                          :range '(0 100)
                                          :value volval
                                          :direction :vertical
                                          :tick-side :none))

    (incf pos 110)
    (setf bar3 (om-make-view 'bar-item 
                             :position (om-make-point 3 pos) 
                             :size (om-make-point 69 10)
                             :bg-color *om-light-gray-color*))

    (incf pos 4)
    (setf faust-text (om-make-dialog-item 'om-static-text 
                                          (om-make-point 27 pos) 
                                          (om-make-point 75 16)
                                          "FX"
                                          :font *om-default-font1*))

    (incf pos 20)
    (setf effect1 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-plug panel item channel 0 effectlist)
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range (append effectlist (cdr (gethash 0 (gethash channel *faust-effects-by-track*))))
                                       :value (cadr (gethash 0 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (setf effect2 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-plug panel item channel 1 effectlist)
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range (append effectlist (cdr (gethash 1 (gethash channel *faust-effects-by-track*))))
                                       :value (cadr (gethash 1 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (setf effect3 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-plug panel item channel 2 effectlist)
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range (append effectlist (cdr (gethash 2 (gethash channel *faust-effects-by-track*))))
                                       :value (cadr (gethash 2 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (setf effect4 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-plug panel item channel 3 effectlist)
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range (append effectlist (cdr (gethash 3 (gethash channel *faust-effects-by-track*))))
                                       :value (cadr (gethash 3 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (setf effect5 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-plug panel item channel 4 effectlist)
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range (append effectlist (cdr (gethash 4 (gethash channel *faust-effects-by-track*))))
                                       :value (cadr (gethash 4 (gethash channel *faust-effects-by-track*)))))

    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5)

    (om-add-subviews main-view 
                     channel-text
                     bar1
                     pan-text
                     pan-val
                     pan-slider
                     bar2
                     vol-text
                     vol-val
                     vol-slider
                     bar3
                     faust-text
                     effect1
                     effect2
                     effect3
                     effect4
                     effect5)
    main-view))

(defun change-genmixer-channel-vol (channel value)
  (progn
    (las-change-channel-vol-visible channel (float (/ value 100)))
    (setf (cadr (gethash (- channel 1) *general-mixer-values*)) value)))

(defun change-genmixer-channel-pan (channel value)
  (progn
    (las-change-channel-pan-visible channel (- 1.0 (float (/ (+ value 100) 200))))
    (setf (car (gethash (- channel 1) *general-mixer-values*)) value)))

(defun update-available-effects-slots (effect1 effect2 effect3 effect4 effect5)
  (if (< (om-get-selected-item-index effect1) 2)
      (progn
        (om-enable-dialog-item effect2 nil)
        (om-enable-dialog-item effect3 nil)
        (om-enable-dialog-item effect4 nil)
        (om-enable-dialog-item effect5 nil))
    (progn
      (om-enable-dialog-item effect2 t)
      (if (< (om-get-selected-item-index effect2) 2)
          (progn
            (om-enable-dialog-item effect3 nil)
            (om-enable-dialog-item effect4 nil)
            (om-enable-dialog-item effect5 nil))
        (progn
          (om-enable-dialog-item effect3 t)
          (if (< (om-get-selected-item-index effect3) 2)
              (progn
                (om-enable-dialog-item effect4 nil)
                (om-enable-dialog-item effect5 nil))
            (progn
              (om-enable-dialog-item effect4 t)
              (if (< (om-get-selected-item-index effect4) 2)
                  (progn
                    (om-enable-dialog-item effect5 nil))
                (om-enable-dialog-item effect5 t)))))))))

(defun pop-up-las-plug (panel item channel effect-number effectlist)
  (let ((pointer (car (gethash (cadr (las-faust-search-name-in-register (om-get-selected-item item))) *faust-effects-register*)))
        (name (nth (om-get-selected-item-index item) effectlist))
        newlist)
    (if (gethash effect-number (gethash channel *faust-effects-by-track*))
        (las-faust-remove-effect-from-track (car (gethash effect-number (gethash channel *faust-effects-by-track*))) channel))
    (if pointer
        (las-faust-add-effect-to-track pointer name channel))
    (setf newlist (build-faust-pool-list))
    (loop for i from 0 to (- las-channels 1) do
          (loop for j from 11 to 15 do
                (om-set-item-list (nth j (om-subviews (nth i (om-subviews panel)))) (append newlist (cdr (gethash (- j 11) (gethash i *faust-effects-by-track*)))))
                (om-set-selected-item (nth j (om-subviews (nth i (om-subviews panel)))) (cadr (gethash (- j 11) (gethash i *faust-effects-by-track*))))))))