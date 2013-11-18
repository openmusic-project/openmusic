;;;==================================================================================================================================================================
;;;===================================================================GENERAL AUDIO MIXER (LAS)============================================================================
;;;==================================================================================================================================================================
;;;This general mixer is a single windows which controls the *audio-player-visible* vol and pan, but also the effect and synth plug system.
;;;You can plug up to 5 effects by channel and only one synth.

(in-package :om)

(defvar *general-mixer-window* nil)
;(defvar *general-mixer-values* nil)
;(defvar *general-mixer-current-preset* 0)
;(setf *general-mixer-values* '((0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100) (0 100)))
;(setf *general-mixer-presets* (init-genmixer-values))

(defvar *mixer-display-update-done* t)

(defclass mixer () 
  ((mixer-player :initform nil :initarg :mixer-player :accessor mixer-player)
   (mixer-presets :initform nil :initarg :mixer-presets :accessor mixer-presets)
   (mixer-current-preset :initform 0 :initarg :mixer-current-preset :accessor mixer-current-preset)
   (mixer-current-preset-float :initform 0 :initarg :mixer-current-preset-float :accessor mixer-current-preset-float)
   (mixer-values :initform nil :initarg :mixer-values :accessor mixer-values)))

;;; which players can the audio-mixer control ?
;;; the first in the list will be the default
(add-player-for-object 'mixer #+(or macosx win32) '(:libaudiostream) #+linux '(:jackaudio))

(defparameter *audio-mixer* nil)
(defparameter *audio-n-channels* nil)

;;; TODO:  find something more "modular" to determine the number of chanels
;; for now: takes the n-channels as defined in LAS...
(setf *audio-n-channels* las-channels)

; (init-mixer)

(defun init-mixer (&optional vals)
        (setf *audio-mixer* (make-instance 'mixer 
                                           :mixer-presets (or vals (init-genmixer-values))
                                           :mixer-current-preset 0
                                           :mixer-current-preset-float 0))
        (setf  
         (mixer-player *audio-mixer*) (car (enabled-players-for-object *audio-mixer*))
         (mixer-values *audio-mixer*)  (copy-tree (cadr (nth (mixer-current-preset *audio-mixer*) 
                                                             (mixer-presets *audio-mixer*)))))
        
        *audio-mixer*)

(defun get-default-mix-values ()
  (make-list *audio-n-channels* :initial-element (list 0 100)))

(defun init-genmixer-values ()
  (list (list "------------" (get-default-mix-values))))

(defun get-presets-list ()
  (mapcar 'car (mixer-presets *audio-mixer*)))

(defun save-presets-in-preferences (mixer prefmodule pref-key)
       (set-pref (find-pref-module prefmodule) pref-key (mixer-presets mixer)))

(defun put-audio-mixer-values (prefvals)
  (if (and prefvals (> (length prefvals) 1))
      (init-mixer prefvals)
    (init-mixer))
  (apply-mixer-values))

;;;==================================
;;; WINDOW
;;;==================================

(defclass omgenmixer-window (om-window)
  ((panel-view :initform nil :initarg :panel-view :accessor panel-view)
   (presets-view :initform nil :initarg :presets-view :accessor presets-view)))

(defclass omgenmixer-view (om-view) 
  ())

;(defmethod om-draw-contents ((self omgenmixer-view)) (print "draw"))

(defun mixer-track-size () 250)

;(make-general-mixer-win)

;/MAKE GENERAL MIXER WINDOW FUNCTION
;This function builds a general mixer windows, with 32 channels
(defun make-general-mixer-win ()
  ;;;HACK BECAUSE GET DEF VALS OF PREFERENCE CANT GET THIS SLOT.
  (unless *audio-mixer* (init-mixer))
  
  (let ((newwindow (om-make-window 'omgenmixer-window 
                                   :window-title "OpenMusic Audio Mixer" 
                                   :size (om-make-point (+ 5 (* *channel-w* 10)) (+ (mixer-track-size) 45 15 16)) 
                                   :scrollbars :h
                                   :position (om-make-point 100 50) :close t :resizable nil))
        ;(vals (copy-tree (or (cadr (nth *general-mixer-current-preset* *general-mixer-presets*)) (cadr (car *general-mixer-presets*)))))
        (vals (copy-tree (mixer-values *audio-mixer*)))
        )

    (setf (panel-view newwindow) (om-make-view 'omgenmixer-view
                                               :owner newwindow
                                               :position (om-make-point 0 0) 
                                               :scrollbars :h
                                               :retain-scrollbars t
                                               :bg-color *om-steel-blue-color*
                                               :field-size  (om-make-point (+ 5 (* *channel-w* *audio-n-channels*)) 400)
                                               :size (om-make-point (w newwindow) (h newwindow))))
    (loop for i from 0 to (- *audio-n-channels* 1) do
          (genmixer-make-single-channel-view (panel-view newwindow) i vals))
    (setf (presets-view newwindow) (om-make-view 'om-view
                                                 :owner (panel-view newwindow)
                                                 :position (om-make-point 5 (+ (mixer-track-size) 10))
                                                 :scrollbars nil
                                                 :retain-scrollbars nil
                                                 :bg-color *om-dark-gray-color*
                                                 :field-size (om-make-point (- (* *channel-w* *audio-n-channels*) 5) 45) 
                                                 :size (om-make-point (- (* *channel-w* *audio-n-channels*) 5) 45)))
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
                                   :field-size  (om-make-point (- (* *channel-w* *audio-n-channels*) 5) 45)
                                   :size (om-make-point (- (* *channel-w* *audio-n-channels*) 5) 45)
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
                                                        (setf (mixer-current-preset *audio-mixer*) (1+ (om-get-selected-item-index item)))
                                                        (setf (mixer-current-preset-float *audio-mixer*) (1+ (om-get-selected-item-index item)))
                                                        (load-genmixer-preset (1+ (om-get-selected-item-index item)))
                                                        (update-genmixer-display))
                                           :font *om-default-font1*
                                           :range (if (> (length thelist) 1) (remove (car thelist) thelist) thelist)
                                           :value (nth (mixer-current-preset *audio-mixer*) thelist)))

    (setf save-preset (om-make-dialog-item 'om-button
                                           (om-make-point 260 10)
                                           (om-make-point 75 12)
                                           "SAVE"
                                           :di-action (om-dialog-item-act item 
                                                        (if (> (mixer-current-preset *audio-mixer*) 0)
                                                            (setf (cadr (nth (mixer-current-preset *audio-mixer*) (mixer-presets *audio-mixer*))) (copy-tree (mixer-values *audio-mixer*)))
                                                          (om-message-dialog "ERROR : You have to select a preset to be able to save it. If there is no existing preset, build a new one"))
                                                        (save-presets-in-preferences *audio-mixer* :audio :audio-presets))
                                           :font *om-default-font1*))

    (setf new-preset (om-make-dialog-item 'om-button
                                          (om-make-point 335 10)
                                          (om-make-point 75 12)
                                          "NEW"
                                          :di-action (om-dialog-item-act item 
                                                       (save-current-settings)
                                                       (update-general-mixer-presets-lists)
                                                       (setf (mixer-current-preset *audio-mixer*) (1- (length (get-presets-list))))
                                                       (setf (mixer-current-preset-float *audio-mixer*) (1- (length (get-presets-list))))
                                                       (om-set-selected-item (nth 1 (om-subviews (car (om-subviews (presets-view *general-mixer-window*))))) (last-elem (get-presets-list)))
                                                       (save-presets-in-preferences *audio-mixer* :audio :audio-presets))
                                          :font *om-default-font1*))

    (setf delete-preset (om-make-dialog-item 'om-button
                                             (om-make-point 410 10)
                                             (om-make-point 75 12)
                                             "DELETE"
                                             :di-action (om-dialog-item-act item
                                                          (if (> (mixer-current-preset *audio-mixer*) 0)
                                                              (progn
                                                                (setf (mixer-presets *audio-mixer*) (remove 
                                                                                                     (nth (1+ (om-get-selected-item-index preset-list)) (mixer-presets *audio-mixer*)) 
                                                                                                     (mixer-presets *audio-mixer*)))
                                                                (update-general-mixer-presets-lists))
                                                            (om-message-dialog "ERROR : You have to select a preset to be able to delete it. If there is no existing preset, build a new one"))
                                                          (save-presets-in-preferences *audio-mixer* :audio :audio-presets))
                                             :font *om-default-font1*))
    
    (setf default-preset (om-make-dialog-item 'om-button
                                             (om-make-point 575 10)
                                             (om-make-point 100 12)
                                             "RESET ALL"
                                             :di-action (om-dialog-item-act item
                                                          (setf (mixer-values *audio-mixer*) (get-default-mix-values))
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
                                  :field-size  (om-make-point (- *channel-w* 5) (mixer-track-size))
                                  :size (om-make-point (- *channel-w* 5) (mixer-track-size))
                                  :bg-color *om-light-gray-color*))
         (pos 8)
         (volval (cadr (nth channel vals)))
         (panval (car (nth channel vals)))
         channel-text bar1 pan-text pan-val pan-slider bar2 vol-text vol-val vol-slider
         other-items
         )


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
    
    (setf other-items (make-more-mixer-items panel channel pos))

    
    (apply 'om-add-subviews 
           (append (list main-view 
                         channel-text
                         bar1
                         pan-text
                         pan-val
                         pan-slider
                         bar2
                         vol-text
                         vol-val
                         vol-slider)
                   other-items))
    main-view))


;; REDEFINED IN FAUST LIBRARY
(defun make-more-mixer-items (panel channel pos) nil)

(defmethod player-change-channel-vol (player channel value) nil)
(defmethod player-change-channel-pan (player channel value) nil)

;/CHANGE GENMIXER CHANNEL VOL FUNCTION
;This function changes a channel volume
(defun change-genmixer-channel-vol (channel value)
  (player-change-channel-vol (mixer-player *audio-mixer*) channel (float (/ value 100)))
  (setf (cadr (nth (- channel 1) (mixer-values *audio-mixer*))) value))

;/CHANGE GENMIXER CHANNEL PAN
;This function changes a channel pan
(defun change-genmixer-channel-pan (channel value)
  (player-change-channel-pan (mixer-player *audio-mixer*) channel (- 1.0 (float (/ (+ value 100) 200))))
  (setf (car (nth (- channel 1) (mixer-values *audio-mixer*))) value))

;/APPLY MIXER VALUES FUNCTION
;Get the pan and vol values in *general-mixer-values* and apply these to the LAS player.
(defun apply-mixer-values ()
  (let (pan vol)
    (loop for i from 0 to (- *audio-n-channels* 1) do
          (setf pan (car (nth i (mixer-values *audio-mixer*)))
                vol (cadr (nth i (mixer-values *audio-mixer*))))
          (player-change-channel-pan (mixer-player *audio-mixer*)  (+ i 1) (- 1.0 (float (/ (+ pan 100) 200))))
          (player-change-channel-vol (mixer-player *audio-mixer*)  (+ i 1) (float (/ vol 100))))))



;/SAVE CURRENT SETTINGS FUNCTION
;Save the current settings to a new preset.
(defun save-current-settings ()
  (let ((name (om-get-user-string "Enter a name for this preset" 
                                  :initial-string (format nil "Preset ~A" (length (mixer-presets *audio-mixer*))))))
    (if name
        (setf (mixer-presets *audio-mixer*)
              (append (mixer-presets *audio-mixer*)
                      (list (list name (copy-tree (mixer-values *audio-mixer*))))))
      )))

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
  (let ((vals (cadr (nth index (mixer-presets *audio-mixer*)))))
    (setf (mixer-values *audio-mixer*) (copy-tree vals))
    (setf (mixer-current-preset *audio-mixer*) index)
    (setf (mixer-current-preset-float *audio-mixer*) (1- (length (get-presets-list))))
    (apply-mixer-values)))


;/UPDATE GENMIXER DISPLAY FUNCTION
;Update the genmixer vol and pan display.
(defun update-genmixer-display ()
  (when *mixer-display-update-done*
    (setf *mixer-display-update-done* nil)
    (let (volval panval)
      (if (and *general-mixer-window* (om-window-open-p *general-mixer-window*))
          (loop for i from 0 to (- *audio-n-channels* 1) do
                (setf volval (cadr (nth i (mixer-values *audio-mixer*)))
                      panval (car (nth i (mixer-values *audio-mixer*))))
                (om-set-dialog-item-text (nth 3 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) (number-to-string (round panval)))
                (set-value (nth 4 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) panval)
                (om-set-dialog-item-text (nth 7 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) (number-to-string (round volval)))
                (om-set-slider-value (nth 8 (om-subviews (nth i (om-subviews (panel-view *general-mixer-window*))))) volval)))
      (setf *mixer-display-update-done* t))))