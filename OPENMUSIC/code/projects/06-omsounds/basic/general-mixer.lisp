;;;==================================================================================================================================================================
;;;===================================================================GENERAL MIXER (LAS)============================================================================
;;;==================================================================================================================================================================
;;;This general mixer is a single windows which controls the *audio-player-visible* vol and pan, but also the effect and synth plug system.
;;;You can plug up to 5 effects by channel and only one synth.

(in-package :om)

(defvar *general-mixer-window* nil)
(defvar *general-mixer-values* nil)
(defvar *general-mixer-current-preset* 0)
;(setf *general-mixer-values* '((0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100)))
;(setf *general-mixer-presets* (init-genmixer-values))

(defun init-genmixer-values ()
  (list (list "------------"
              (loop for i from 0 to (- las-channels 1) collect
                    (list 0 100)))))

(defun get-default-values ()
  (loop for i from 0 to (- las-channels 1) collect
        (list 0 100)))

(defun get-presets-list ()
  (loop for i from 0 to (- (length *general-mixer-presets*) 1) collect
              (car (nth i *general-mixer-presets*))))

(defclass omgenmixer-window (om-window)
  ((panel-view :initform nil :initarg :panel-view :accessor panel-view)
   (presets-view :initform nil :initarg :presets-view :accessor presets-view)))

(defclass omgenmixer-view (om-view) 
  ())


;/MAKE GENERAL MIXER WINDOW FUNCTION
;This function builds a general mixer windows, with 32 channels
(defun make-general-mixer-win ()

  ;;;HACK BECAUSE GET DEF VALS OF PREFERENCE CANT GET THIS SLOT.
  (if (not *general-mixer-presets*) (setf *general-mixer-presets* (init-genmixer-values)))

  (let ((newwindow (om-make-window 'omgenmixer-window 
                                   :window-title "OpenMusic General Mixer" 
                                   :size (om-make-point (+ 5 (* *channel-w* 10)) (+ 420 50)) 
                                   :scrollbars :h
                                   :position (om-make-point 100 50) :close t :resizable nil))
        (vals (copy-tree (cadr (nth *general-mixer-current-preset* *general-mixer-presets*)))))

    (setf (panel-view newwindow) (om-make-view 'omgenmixer-view
                                               :owner newwindow
                                               :position (om-make-point 0 0) 
                                               :scrollbars :h
                                               :retain-scrollbars t
                                               :bg-color *om-steel-blue-color*
                                               :field-size  (om-make-point (+ 5 (* *channel-w* las-channels)) 400)
                                               :size (om-make-point (w newwindow) (h newwindow))))
    (loop for i from 0 to (- las-channels 1) do
          (genmixer-make-single-channel-view (panel-view newwindow) i vals))
    (setf (presets-view newwindow) (om-make-view 'om-view
                                                 :owner (panel-view newwindow)
                                                 :position (om-make-point 5 405)
                                                 :scrollbars nil
                                                 :retain-scrollbars nil
                                                 :bg-color *om-dark-gray-color*
                                                 :field-size (om-make-point (- (* *channel-w* las-channels) 5) 45) 
                                                 :size (om-make-point (- (* *channel-w* las-channels) 5) 45)))
    (genmixer-make-preset-view (presets-view newwindow))
    newwindow))

;/MAKE GENMIXER DIALOG
;This function checks if the general mixer window is already opened. If yes, it focus on it, else it builds a new one.
(defun omG-make-genmixer-dialog ()
  (if (and *general-mixer-window* (om-window-open-p *general-mixer-window*))
      (om-select-window *general-mixer-window*)
    (setf *general-mixer-window* (om-select-window (make-general-mixer-win)))))


;/GENMIXER MAKE PRESET VIEW
;This function builds the mixer preset management view
(defun genmixer-make-preset-view (presets)
  (let ((preset-view (om-make-view 'om-view 
                                   :owner presets
                                   :position (om-make-point 0 0) 
                                   :scrollbars nil
                                   :retain-scrollbars nil
                                   :field-size  (om-make-point (- (* *channel-w* las-channels) 5) 45)
                                   :size (om-make-point (- (* *channel-w* las-channels) 5) 45)
                                   :bg-color *om-dark-gray-color*))
        (thelist (get-presets-list))
        text
        preset-list
        text1
        save-preset
        text2
        delete-preset)
    (setf text (om-make-dialog-item 'om-static-text
                                    (om-make-point 10 13)
                                    (om-make-point 130 20) "PRESETS :"
                                    :font *om-default-font1*
                                    :fg-color *om-white-color*))
    
    (setf preset-list (om-make-dialog-item 'om-pop-up-dialog-item 
                                           (om-make-point 75 12) 
                                           (om-make-point 120 12)
                                           ""
                                           :di-action (om-dialog-item-act item
                                                        (setf *general-mixer-current-preset* (1+ (om-get-selected-item-index item)))
                                                        (load-genmixer-preset (1+ (om-get-selected-item-index item)))
                                                        (update-genmixer-display))
                                           :font *om-default-font1*
                                           :range (if (> (length thelist) 1) (remove (car thelist) thelist) thelist)
                                           :value (nth *general-mixer-current-preset* thelist)))

    (setf save-preset (om-make-dialog-item 'om-button
                                           (om-make-point 260 10)
                                           (om-make-point 75 12)
                                           "SAVE"
                                           :di-action (om-dialog-item-act item 
                                                        (if (> *general-mixer-current-preset* 0)
                                                            (setf (cadr (nth *general-mixer-current-preset* *general-mixer-presets*)) (copy-tree *general-mixer-values*))
                                                          (om-message-dialog "ERROR : You have to select a preset to be able to save it. If there is no existing preset, build a new one"))
                                                        (set-pref (find-pref-module :audio) :audio-presets *general-mixer-presets*))
                                           :font *om-default-font1*))

    (setf new-preset (om-make-dialog-item 'om-button
                                          (om-make-point 335 10)
                                          (om-make-point 75 12)
                                          "NEW"
                                          :di-action (om-dialog-item-act item 
                                                       (save-current-settings)
                                                       (update-general-mixer-presets-lists)
                                                       (setf *general-mixer-current-preset* (1- (length (get-presets-list))))
                                                       (om-set-selected-item (nth 1 (om-subviews (car (om-subviews (presets-view *general-mixer-window*))))) (last-elem (get-presets-list)))
                                                       (set-pref (find-pref-module :audio) :audio-presets *general-mixer-presets*))
                                          :font *om-default-font1*))

    (setf delete-preset (om-make-dialog-item 'om-button
                                             (om-make-point 410 10)
                                             (om-make-point 75 12)
                                             "DELETE"
                                             :di-action (om-dialog-item-act item
                                                          (if (> *general-mixer-current-preset* 0)
                                                              (progn
                                                                (setf *general-mixer-presets* (remove (nth (1+ (om-get-selected-item-index preset-list)) *general-mixer-presets*) *general-mixer-presets*))
                                                                (update-general-mixer-presets-lists))
                                                            (om-message-dialog "ERROR : You have to select a preset to be able to delete it. If there is no existing preset, build a new one"))
                                                          (set-pref (find-pref-module :audio) :audio-presets *general-mixer-presets*))
                                             :font *om-default-font1*))
    
    (setf default-preset (om-make-dialog-item 'om-button
                                             (om-make-point 575 10)
                                             (om-make-point 100 12)
                                             "RESET ALL"
                                             :di-action (om-dialog-item-act item
                                                          (setf *general-mixer-values* (get-default-values))
                                                          (update-genmixer-display)
                                                          (apply-mixer-values))
                                             :font *om-default-font1*))

    (if (= (length thelist) 1) (om-enable-dialog-item preset-list nil))
    (om-add-subviews preset-view text preset-list save-preset new-preset delete-preset default-preset)
    preset-view))


;/GENMIXER MAKE SINGLE CHANNEL
;This function builds one channel view
(defun genmixer-make-single-channel-view (panel channel vals)
  (let* ((main-view (om-make-view 'om-view 
                                  :owner panel
                                  :position (om-make-point (+ 5 (* channel *channel-w*)) 5) 
                                  :scrollbars nil
                                  :retain-scrollbars nil
                                  :field-size  (om-make-point (- *channel-w* 5) 395)
                                  :size (om-make-point (- *channel-w* 5) 395)
                                  :bg-color *om-light-gray-color*))
         (pos 8)
         (volval (cadr (nth channel vals)))
         (panval (car (nth channel vals)))
         (effectlist (build-faust-effect-list channel))
         (synthlist (build-faust-synth-list channel))
         (effectlist1 effectlist) (effectlist2 effectlist) (effectlist3 effectlist) 
         (effectlist4 effectlist) (effectlist5 effectlist)
         channel-text bar1 pan-text pan-val pan-slider bar2 vol-text vol-val vol-slider
         bar3 synth-text synth bar4 faust-text effect1 effect2 effect3 effect4 effect5)


    (setf channel-text (om-make-dialog-item 'om-static-text
                                            (if (< channel 9) (om-make-point 4 pos) (om-make-point 1 pos))
                                            (om-make-point 80 20) (format nil "CHANNEL ~A" (+ 1 channel))
                                            :font *om-default-font1*))

    (incf pos 20)
    (setf bar1 (om-make-view 'bar-item 
                             :position (om-make-point 3 pos) 
                             :size (om-make-point 69 10)
                             :bg-color *om-light-gray-color*))

    (incf pos 10)
    (setf pan-text (om-make-dialog-item 'om-static-text
                                        (om-make-point 14 (- pos 5)) 
                                        (om-make-point 40 16)
                                        "Pan"
                                        :font *om-default-font2*))
    (setf pan-val (om-make-dialog-item 'om-static-text
                                       (om-make-point 45 (- pos 5)) 
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


;/CHANGE GENMIXER CHANNEL VOL FUNCTION
;This function changes a channel volume
(defun change-genmixer-channel-vol (channel value)
  (las-change-channel-vol-visible channel (float (/ value 100)))
  (setf (cadr (nth (- channel 1) *general-mixer-values*)) value))

;/CHANGE GENMIXER CHANNEL PAN
;This function changes a channel pan
(defun change-genmixer-channel-pan (channel value)
  (las-change-channel-pan-visible channel (- 1.0 (float (/ (+ value 100) 200))))
  (setf (car (nth (- channel 1) *general-mixer-values*)) value))

;/APPLY MIXER VALUES FUNCTION
;Get the pan and vol values in *general-mixer-values* and apply these to the LAS player.
(defun apply-mixer-values ()
  (let (pan vol)
    (loop for i from 0 to (- las-channels 1) do
          (setf pan (car (nth i *general-mixer-values*))
                vol (cadr (nth i *general-mixer-values*)))
          (las-change-channel-pan-visible (+ i 1) (- 1.0 (float (/ (+ pan 100) 200))))
          (las-change-channel-vol-visible (+ i 1) (float (/ vol 100))))))


;/UPDATE AVAILABLE EFFECTS SLOTS
;This function makes effects pop-up dialogs available or not according to the number of plugged effect on each channel
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


;/BUILD FAUST EFFECT LIST FUNCTION
;This function returns a list of availables effects for each channel.
(defun build-faust-effect-list (channel)
  (let ((n (- (las-get-number-faust-effects-register) 1))
        (final-list (list "-------"))) 
    (loop for i from 0 to n do
          (if (or (= 0 (nth 1 (gethash i *faust-effects-register*))) (= (+ channel 1) (nth 1 (gethash i *faust-effects-register*))))
              (setf final-list (append final-list (list (nth 2 (gethash i *faust-effects-register*)))))))
    final-list))

;/BUILD FAUST SYNTH LIST FUNCTION
;This function returns a list of availables effects for each channel.
(defun build-faust-synth-list (channel)
  (let ((n (- (las-get-number-faust-synths-register) 1))
        (final-list (list "-------"))) 
    (loop for i from 0 to n do
          (if (or (= 0 (nth 1 (gethash i *faust-synths-register*))) (= (+ channel 1) (nth 1 (gethash i *faust-synths-register*))))
              (setf final-list (append final-list (list (nth 2 (gethash i *faust-synths-register*)))))))
    final-list))

;/POP UP LAS EFFECT PLUG FUNCTION
;This function plug and/or unplug effect according to the user action on a pop-up effect dialog.
;It also update effect lists on each pop-up
(defun pop-up-las-effect-plug (panel item channel effect-number effectlist)
  (let ((pointer (car (gethash (cadr (las-faust-search-effect-name-in-register (om-get-selected-item item))) *faust-effects-register*)))
        (name (nth (om-get-selected-item-index item) effectlist))
        newlist
        )
    ;unplug old one
    (if (gethash effect-number (gethash channel *faust-effects-by-track*))
        (las-faust-remove-effect-from-track (car (gethash effect-number (gethash channel *faust-effects-by-track*))) channel))
    ;plug new one
    (if pointer
        (las-faust-add-effect-to-track pointer name channel))
    ;update effects lists
    (update-general-mixer-effects-lists panel)))

;/POP UP LAS SYNTH FUNCTION
;This function plug and/or unplug a synth according to the user action on a pop-up synth dialog.
;It also update synth lists on each channel.
(defun pop-up-las-synth-plug (panel item channel synthlist)
  (let ((pointer (car (gethash (cadr (las-faust-search-synth-name-in-register (om-get-selected-item item))) *faust-synths-register*)))
        (name (nth (om-get-selected-item-index item) synthlist))
        newlist)
    ;unplug old one
    (if (gethash 0 (gethash channel *faust-synths-by-track*))
        (las-faust-remove-synth-from-track (car (gethash 0 (gethash channel *faust-synths-by-track*))) channel))
    ;plug new one
    (if pointer
        (las-faust-add-synth-to-track pointer name channel))
    ;update effects lists
    (update-general-mixer-synths-lists panel)))

;/UPDATE GENMIXER EFFECTS LISTS FUNCTION
;This function updates the effect lists on each pop-up effect dialog.
(defun update-general-mixer-effects-lists (panel)
  (let ((newlistn (make-hash-table))
        newlist
        (effectn (make-hash-table)))
    (loop for i from 0 to (- las-channels 1) do
          (setf newlist (build-faust-effect-list i))
          (loop for k from 0 to 4 do
                (setf (gethash k newlistn) newlist))
          (loop for j from 0 to 4 do
                (loop for k from 0 to 4 do
                      (if (/= j k)
                          (setf (gethash j newlistn) (remove (cadr (gethash k (gethash i *faust-effects-by-track*))) (gethash j newlistn) :test #'string=))))
                (setf (gethash j effectn) (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))))
                (om-set-item-list (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))) (gethash j newlistn))
                (om-set-selected-item (nth (+ j 14) (om-subviews (nth i (om-subviews panel)))) (cadr (gethash j (gethash i *faust-effects-by-track*)))))
          (update-available-effects-slots (gethash 0 effectn) 
                                          (gethash 1 effectn) 
                                          (gethash 2 effectn) 
                                          (gethash 3 effectn) 
                                          (gethash 4 effectn)))))

;/UPDATE GENMIXER SYNTHS LISTS FUNCTION
;This function updates the synth lists on each pop-up synth dialog.
(defun update-general-mixer-synths-lists (panel)
  (let (newlist)
    (loop for i from 0 to (- las-channels 1) do
          (setf newlist (build-faust-synth-list i))
          (om-set-item-list (nth 11 (om-subviews (nth i (om-subviews panel)))) newlist)
          (om-set-selected-item (nth 11 (om-subviews (nth i (om-subviews panel)))) (cadr (gethash 0 (gethash i *faust-synths-by-track*)))))))


;/SAVE CURRENT SETTINGS FUNCTION
;Save the current settings to a new preset.
(defun save-current-settings ()
  (let ((name (om-get-user-string "Enter a name for this preset" 
                                  :initial-string (format nil "Preset ~A" (length *general-mixer-presets*)))))
    (if name
        (setf *general-mixer-presets* 
              (append *general-mixer-presets*
                      (list 
                       (list 
                        name
                        (copy-tree *general-mixer-values*))))))))

;/UPDATE GENMIXER PRESETS LISTS FUNCTION
;Refresh the available lists in preset choice list and delete list.
(defun update-general-mixer-presets-lists ()
  (let ((newlist (get-presets-list)))
    (om-set-item-list (nth 1 (om-subviews (car (om-subviews (presets-view *general-mixer-window*))))) newlist)
    (if (> (length newlist) 1)
        (progn
          (om-enable-dialog-item (cadr (om-subviews (car (om-subviews (presets-view *general-mixer-window*))))) t)
          (om-set-item-list (cadr (om-subviews (car (om-subviews (presets-view *general-mixer-window*))))) (remove (car newlist) newlist)))
      (progn
        (om-enable-dialog-item (cadr (om-subviews (car (om-subviews (presets-view *general-mixer-window*))))) nil)
        (om-set-item-list (cadr (om-subviews (car (om-subviews (presets-view *general-mixer-window*))))) newlist)))))

;/LOAD GENMIXER PRESET FUNCTION
;Loads a preset to the *general-mixer-values* variable, and apply these values.
(defun load-genmixer-preset (index)
  (let ((vals (cadr (nth index *general-mixer-presets*))))
    (setf *general-mixer-values* (copy-tree vals))
    (apply-mixer-values)))

;/UPDATE GENMIXER DISPLAY FUNCTION
;Update the genmixer vol and pan display.
(defun update-genmixer-display ()
  (let (volval panval)
    (if (and *general-mixer-window* (om-window-open-p *general-mixer-window*))
        (loop for i from 0 to (- las-channels 1) do
              (setf volval (cadr (nth i *general-mixer-values*))
                    panval (car (nth i *general-mixer-values*)))
              (om-set-dialog-item-text (nth 3 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) (number-to-string panval))
              (set-value (nth 4 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) panval)
              (om-set-dialog-item-text (nth 7 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) (number-to-string volval))
              (om-set-slider-value (nth 8 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) volval)))))