(in-package :om)

;===========================================================
;MIXER AUTOMATION
;===========================================================

(defclass! mixer-automation (BPF-controller) 
           ((track :initform nil :accessor track :initarg :track :documentation "a track number")
            (parameter :initform nil :accessor parameter :initarg :parameter :documentation "a parameter name (\"vol\" or \"pan\" or \"presets\")")
            ;(mixerfun :initform nil :accessor mixerfun)
            )
           (:icon 234))

(defmethod make-one-instance ((self mixer-automation) &rest slots-vals) 
  (let ((bpf (call-next-method))
        char1 res x y)
    (setf (track bpf) (nth 3 slots-vals))
    (setf (parameter bpf) (nth 4 slots-vals))
    (setf (decimals bpf) 1)
    (if (or (track bpf) (and (parameter bpf) (stringp (parameter bpf)))); (string= (string-downcase (parameter bpf)) "presets")))
        (progn
          (if (parameter bpf)
              (if (not (or (string= (string-downcase (parameter bpf)) "pan") 
                           (string= (string-downcase (parameter bpf)) "vol")
                           (string= (string-downcase (parameter bpf)) "presets")))
                  (setf res (make-param-select-window (list "Panoramic" "Volume" "Presets")))
                (cond ((string= (string-downcase (parameter bpf)) "pan") (setf res 0))
                      ((string= (string-downcase (parameter bpf)) "vol") (setf res 1))
                      ((string= (string-downcase (parameter bpf)) "presets") (setf res 2))
                      (t nil)))
            (setf res (make-param-select-window (list "Panoramic" "Volume"))))
    
          (cond ((and res (= res 0)) (setf (parameter bpf) "pan"))
                ((and res (= res 1)) (setf (parameter bpf) "vol"))
                ((and res (= res 2)) (setf (parameter bpf) "presets"
                                           (decimals bpf) 0)))))

    (if (or (and (parameter bpf) (track bpf) (numberp (track bpf)) (<= (track bpf) las-channels) (> (track bpf) 0))
            (and (parameter bpf) (string= (parameter bpf) "presets")))
        (progn
          (setf (player-fun bpf) (get-function-from-track bpf))
          (if (string= (parameter bpf) "presets")
              (setf x (loop for i from 0 to (1- (length *general-mixer-presets*)) collect
                            (* 2000 i))
                    y (loop for i from 0 to (1- (length *general-mixer-presets*)) collect
                            i))
            (setf x (interpolate (list 0 10000) (list 0 10000) 10)
                  y (interpolate (list 0 10000) (if (string= (parameter bpf) "pan") (list -100 100) (list 0 100)) 10)))
          (setf (y-points bpf) y)
          (setf (x-points bpf) x))
      (print "I cannot build a mixer-automation with these parameters"))
    bpf))



(defmethod prepare-to-play ((self (eql :bpfplayer)) (player omplayer) (object mixer-automation) at interval)
  (when (or (track object) (and (parameter object) (string= (string-downcase (parameter object)) "presets")))
    (call-next-method)))


(defun get-function-from-track (bpf)
  (let* ((track (track bpf))
         (parameter (parameter bpf))
         (minval (if (string= parameter "vol") 0 -100)) 
         (maxval 100)
         (npresets (length *general-mixer-presets*)))
    (cond  ((string= parameter "pan")
            #'(lambda (val)
                (if (< val minval) (setf val minval))
                (if (> val maxval) (setf val maxval))
                (change-genmixer-channel-pan track (float val))
                (if *general-mixer-window*
                    (progn
                      (om-set-dialog-item-text (nth 3 (om-subviews (nth (1- track) (om-subviews (panel-view *general-mixer-window*))))) (number-to-string (round val)))
                      (set-value (nth 4 (om-subviews (nth (1- track) (om-subviews (panel-view *general-mixer-window*))))) val)))))
           ((string= parameter "vol")
            #'(lambda (val)
                (if (< val minval) (setf val minval))
                (if (> val maxval) (setf val maxval))
                (change-genmixer-channel-vol track (float val))
                (if *general-mixer-window*
                    (progn
                      (om-set-dialog-item-text (nth 7 (om-subviews (nth (1- track) (om-subviews (panel-view *general-mixer-window*))))) (number-to-string (round val)))
                      (om-set-slider-value (nth 8 (om-subviews (nth (1- track) (om-subviews (panel-view *general-mixer-window*))))) val)))))
           ((string= parameter "presets")
            #'(lambda (val)
                (if (< val 0) (setf val 0))
                (if (>= val npresets) (setf val (- npresets 1)))
                (load-genmixer-preset val)
                (if *general-mixer-window*
                    (update-genmixer-display)))))))

(defmethod draw-control-info ((self t) (object mixer-automation)) 
  (let ((namelist (loop for i from 0 to (1- (length *general-mixer-presets*)) collect
                        (car (nth i *general-mixer-presets*)))))
    (om-with-focused-view self
      (om-with-fg-color self (om-make-color 1 0 0)
        (om-draw-string 80 (om-point-y (point2pixel self (om-make-point 0 0) (get-system-etat self)))
                              (format nil "0 - Default Preset"))
        (loop for i from 1 to (1- (length namelist)) do
              (om-draw-string 80 (om-point-y (point2pixel self (om-make-point 0 (* i (expt 10 (decimals object)))) (get-system-etat self)))
                              (format nil "~A - ~A" i (nth i namelist))))))))


(defun make-param-select-window (listing)
  (let ((win (om-make-window 'om-dialog
                             :window-title "Parameter selection" 
                             :size (om-make-point 400 210) 
                             :scrollbars nil
                             :position (om-make-point 100 50)))
        (maxchar 0))
   (loop for i from 0 to (1- (length listing)) do
         (if (< maxchar (count-if #'standard-char-p (nth i listing)))
             (setf maxchar (count-if #'standard-char-p (nth i listing)))))
    (setf panel (om-make-view 'om-view
                              :owner win
                              :position (om-make-point 0 0) 
                              :scrollbars nil
                              :retain-scrollbars nil
                              :bg-color *om-dark-gray-color*
                              :field-size  (om-make-point 400 200)
                              :size (om-make-point (w win) (h win))))
    (setf text1 (om-make-dialog-item 'om-static-text 
                                     (om-make-point 85 5) 
                                     (om-make-point 295 20)
                                     (format nil "Empty or invalid parameter name.")
                                     :font *om-default-font2b* 
                                     :fg-color *om-white-color*))
    (setf text2 (om-make-dialog-item 'om-static-text 
                                     (om-make-point 25 25) 
                                     (om-make-point 390 20)
                                     (format nil "Please select the parameter you want to automate in this list :")
                                     :font *om-default-font1* 
                                     :fg-color *om-white-color*))
    (setf paramlist (om-make-dialog-item  'om-single-item-list 
                                          (om-make-point 50 55) 
                                          (om-make-point 300 100) 
                                          "Available parameters"  
                                          :scrollbars (cond ((and (> (length listing) 4) (not (> maxchar 43))) :v)
                                                            ((and (> maxchar 43) (not (> (length listing) 4))) :h)
                                                            ((and (> (length listing) 4) (> maxchar 10)) t)
                                                            (t nil))
                                          :bg-color *om-dark-gray-color*
                                          :fg-color *om-white-color*
                                          :after-action (om-dialog-item-act item 
                                                          (om-return-from-modal-dialog win (om-get-selected-item-index item)))
                                          :range listing))
    (setf ok (om-make-dialog-item 'om-button 
                                  (om-make-point 105 163) 
                                  (om-make-point 70 24)  "OK"
                                  :di-action (om-dialog-item-act item 
                                               (if (om-get-selected-item-index paramlist)
                                                   (om-return-from-modal-dialog win (om-get-selected-item-index paramlist))))))
    (setf cancel (om-make-dialog-item 'om-button 
                                      (om-make-point 225 163) 
                                      (om-make-point 70 24)  "Cancel"
                                      :di-action (om-dialog-item-act item (om-return-from-modal-dialog win nil))))
    (om-add-subviews panel text1 text2 paramlist ok cancel)
    (om-modal-dialog win)))



