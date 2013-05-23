(in-package :om)

(defvar *general-mixer-window* nil)
(defvar *general-mixer-values* (make-hash-table))

(defun init-genmixer-values ()
  (loop for i from 0 to (- las-channels 1) do
        (setf (gethash i *general-mixer-values*) (list 0 100))))

(init-genmixer-values)



(defclass omgenmixer-window (om-window)
  ())

(defclass omgenmixer-view (om-view) ())




(defun make-general-mixer-win ()
  (let ((newwindow (om-make-window 'omgenmixer-window :window-title "OpenMusic General Mixer" 
                                   :size (om-make-point (+ 5 (* *channel-w* 10)) 420) 
                                   :scrollbars :h
                                   :position (om-make-point 100 50) :close t :resizable nil))
        panel)
    (setf panel (om-make-view 'omgenmixer-view
                              :owner newwindow
                              :position (om-make-point 0 0) 
                              :scrollbars :h
                              :retain-scrollbars t
                              :bg-color *om-steel-blue-color*
                              :field-size  (om-make-point (+ 5 (* *channel-w* las-channels)) 400)
                              :size (om-make-point (w newwindow) (h newwindow))))
    (loop for i from 0 to (- las-channels 1) do
          (genmixer-make-single-channel-view panel i))
    newwindow))

(defun omG-make-genmixer-dialog ()
  (if (and *general-mixer-window* (om-window-open-p *general-mixer-window*))
      (om-select-window *general-mixer-window*)
    (setf *general-mixer-window* (om-select-window (make-general-mixer-win)))))

(defun genmixer-make-single-channel-view (panel channel)
  (let* ((main-view (om-make-view 'om-view 
                                  :owner panel
                                  :position (om-make-point (+ 5 (* channel *channel-w*)) 5) 
                                  :scrollbars nil
                                  :retain-scrollbars nil
                                  :field-size  (om-make-point (- *channel-w* 5) 395)
                                  :size (om-make-point (- *channel-w* 5) 395)
                                  :bg-color *om-light-gray-color*))
         (pos 8)
         (volval (cadr (gethash channel *general-mixer-values*)))
         (panval (car (gethash channel *general-mixer-values*)))
         (effectlist (build-faust-effect-list channel))
         (synthlist (build-faust-synth-list channel))
         (effectlist1 effectlist) (effectlist2 effectlist) (effectlist3 effectlist) 
         (effectlist4 effectlist) (effectlist5 effectlist)
         channel-text bar1 pan-text pan-val pan-slider bar2 vol-text vol-val vol-slider
         bar3 synth-text synth bar4 faust-text effect1 effect2 effect3 effect4 effect5)


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
    (setf synth-text (om-make-dialog-item 'om-static-text 
                                          (om-make-point 17 pos) 
                                          (om-make-point 75 16)
                                          "SYNTH"
                                          :font *om-default-font1*))
    (incf pos 20)
    (setf synth (om-make-dialog-item 'om-pop-up-dialog-item 
                                     (om-make-point 0 pos) 
                                     (om-make-point 75 12)
                                     ""
                                     :di-action (om-dialog-item-act item
                                                  (pop-up-las-synth-plug panel item channel (om-get-item-list item)))
                                     :font *om-default-font1*
                                     :range synthlist
                                     :value (cadr (gethash 0 (gethash channel *faust-synths-by-track*)))))
    (incf pos 25)
    (setf bar4 (om-make-view 'bar-item 
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
    (loop for i from 0 to 4 do
          (if (/= i 0)
              (setf effectlist1 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist1 :test #'string=))))
    (setf effect1 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 0 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist1
                                       :value (cadr (gethash 0 (gethash channel *faust-effects-by-track*)))))
    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 1)
              (setf effectlist2 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist2 :test #'string=))))
    (setf effect2 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 1 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist2
                                       :value (cadr (gethash 1 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 2)
              (setf effectlist3 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist3 :test #'string=))))
    (setf effect3 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 2 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist3
                                       :value (cadr (gethash 2 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 3)
              (setf effectlist4 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist4 :test #'string=))))
    (setf effect4 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 3 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist4
                                       :value (cadr (gethash 3 (gethash channel *faust-effects-by-track*)))))

    (incf pos 20)
    (loop for i from 0 to 4 do
          (if (/= i 4)
              (setf effectlist5 (remove (cadr (gethash i (gethash channel *faust-effects-by-track*))) effectlist5 :test #'string=))))
    (setf effect5 (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 0 pos) 
                                       (om-make-point 75 12)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                    (pop-up-las-effect-plug panel item channel 4 (om-get-item-list item))
                                                    (update-available-effects-slots effect1 effect2 effect3 effect4 effect5))
                                       :font *om-default-font1*
                                       :range effectlist5
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
                     synth-text
                     synth
                     bar4
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
  (if (= (om-get-selected-item-index effect1) 0)
      (progn
        (om-enable-dialog-item effect2 nil)
        (om-enable-dialog-item effect3 nil)
        (om-enable-dialog-item effect4 nil)
        (om-enable-dialog-item effect5 nil))
    (progn
      (om-enable-dialog-item effect2 t)
      (if (= (om-get-selected-item-index effect2) 0)
          (progn
            (om-enable-dialog-item effect3 nil)
            (om-enable-dialog-item effect4 nil)
            (om-enable-dialog-item effect5 nil))
        (progn
          (om-enable-dialog-item effect3 t)
          (if (= (om-get-selected-item-index effect3) 0)
              (progn
                (om-enable-dialog-item effect4 nil)
                (om-enable-dialog-item effect5 nil))
            (progn
              (om-enable-dialog-item effect4 t)
              (if (= (om-get-selected-item-index effect4) 0)
                  (progn
                    (om-enable-dialog-item effect5 nil))
                (om-enable-dialog-item effect5 t)))))))))

(defun build-faust-effect-list (channel)
  (let ((n (- (las-get-number-faust-effects-register) 1))
        (final-list (list "-------"))) 
    (loop for i from 0 to n do
          (if (or (= 0 (nth 1 (gethash i *faust-effects-register*))) (= (+ channel 1) (nth 1 (gethash i *faust-effects-register*))))
              (setf final-list (append final-list (list (nth 2 (gethash i *faust-effects-register*)))))))
    final-list))
(defun build-faust-synth-list (channel)
  (let ((n (- (las-get-number-faust-synths-register) 1))
        (final-list (list "-------"))) 
    (loop for i from 0 to n do
          (if (or (= 0 (nth 1 (gethash i *faust-synths-register*))) (= (+ channel 1) (nth 1 (gethash i *faust-synths-register*))))
              (setf final-list (append final-list (list (nth 2 (gethash i *faust-synths-register*)))))))
    final-list))


(defun pop-up-las-effect-plug (panel item channel effect-number effectlist)
  (let ((pointer (car (gethash (cadr (las-faust-search-effect-name-in-register (om-get-selected-item item))) *faust-effects-register*)))
        (name (nth (om-get-selected-item-index item) effectlist))
        newlist
        (newlistn (make-hash-table)))
    ;unplug old one
    (if (gethash effect-number (gethash channel *faust-effects-by-track*))
        (las-faust-remove-effect-from-track (car (gethash effect-number (gethash channel *faust-effects-by-track*))) channel))
    ;plug new one
    (if pointer
        (las-faust-add-effect-to-track pointer name channel))
    ;update effects lists
    (loop for i from 0 to (- las-channels 1) do
          (setf newlist (build-faust-effect-list i))
          (loop for k from 0 to 4 do
                (setf (gethash k newlistn) newlist))
          (loop for j from 0 to 4 do
                (loop for k from 0 to 4 do
                      (if (/= j k)
                          (setf (gethash j newlistn) (remove (cadr (gethash k (gethash i *faust-effects-by-track*))) (gethash j newlistn) :test #'string=))))
                (om-set-item-list (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))) (gethash j newlistn))
                (om-set-selected-item (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))) (cadr (gethash j (gethash i *faust-effects-by-track*))))))))
(defun pop-up-las-synth-plug (panel item channel synthlist)
  (let ((pointer (car (gethash (cadr (las-faust-search-synth-name-in-register (om-get-selected-item item))) *faust-synths-register*)))
        (name (nth (om-get-selected-item-index item) synthlist))
        newlist)
    (print pointer)
    ;unplug old one
    (if (gethash 0 (gethash channel *faust-synths-by-track*))
        (las-faust-remove-synth-from-track (car (gethash 0 (gethash channel *faust-synths-by-track*))) channel))
    ;plug new one
    (if pointer
        (las-faust-add-synth-to-track pointer name channel))
    ;update effects lists
    (loop for i from 0 to (- las-channels 1) do
          (setf newlist (build-faust-synth-list i))
          (om-set-item-list (nth 11 (om-subviews (nth i (om-subviews panel)))) newlist)
          (om-set-selected-item (nth 11 (om-subviews (nth i (om-subviews panel)))) (cadr (gethash 0 (gethash i *faust-synths-by-track*)))))))