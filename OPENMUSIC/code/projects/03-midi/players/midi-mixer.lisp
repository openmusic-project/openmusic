
;;; SEVERAL MIDI SYSTEMS CAN BE AVAILABLE / LOAD IN OM
;;; ONE IS SELECTED AS THE DEFAULT SYSTEM (TO BE SELECTED IN PREFERENCES)
;;; INDEPENDENTLY ON THE PLAYER FOR SCORE OBJECTS ETC. THE MIDI SYSTEM IS IN CHARGE OF
;;; - LOADING / SAVING MIDI FILES
;;; - SENDING 'GENERAL' MESSAGES, E.G. the MIDI-SEND BOXES OR THE 
;;;====================================
;;; GENERAL MIDI MIXER 
;;;====================================

(in-package :om)

(defvar *midi-mixer-window* nil)
(defparameter *midi-mixer* nil)

(defclass midi-mixer (midi-mix-console OMBasicObject) 
  ((presets :accessor presets :initarg :presets :initform nil)
   (current-preset :accessor current-preset :initarg :current-preset :initform 0))
  (:default-initargs :nbtracks 16))

(defun init-midi-mixer (&optional vals)
  (or *midi-mixer* 
      (setf *midi-mixer* (make-instance 'midi-mixer :presets (or vals (def-midi-presets))))))

(defun def-midi-presets () '(("-----" nil)))

(defun put-midi-mixer-values ()
  (print "MIDI: Restoring presets from preferences...")
  (let ((vals (get-pref (find-pref-module :midi) :midi-presets)))
    (init-midi-mixer vals)
    (when *midi-mixer* 
      (set-preset *midi-mixer* (cadr (car vals))))))


(defun save-midi-presets-in-preferences (mixer)
  (set-pref (find-pref-module :midi) :midi-presets (presets mixer))
  (save-preferences))

;(om-add-init-func 'init-midi-mixer)

;;;==================================
;;; EDITOR
;;;==================================

; (show-midi-mixer-win)
(defun show-midi-mixer-win ()
  (if (and *midi-mixer-window* (om-window-open-p *midi-mixer-window*)) 
      (om-select-window *midi-mixer-window*)
    (setf *midi-mixer-window* (make-editor-window 'MIDIMixerEditor (init-midi-mixer)
                                                  "MIDI Mixer"
                                                  nil
                                                  :winsize (om-make-point (* *channel-w* 16) 600)
                                                  :resize nil))
    ))

(defclass MIDIMixerEditor (ConsoleEditor) ()
  (:default-initargs :delta-tracks 2 :send-rt t))

(defmethod get-panel-class ((Self MIDIMixerEditor)) 'MixerPanel)
(defclass MixerPanel (controllerPanel) ())
(defmethod get-channelpanel-class ((self MixerPanel)) 'mixerchannelview)
(defclass mixerchannelview (channelpanelview) ())

(defmethod make-channel-title-items ((self mixerchannelview))
  (list 
   (setf (channelText self) (om-make-dialog-item 'om-static-text
                                                 (om-make-point 5 5) 
                                                 (om-make-point 76 20) 
                                                 (format nil "CHANNEL" (midichannel (channelctr self)))
                                                 :font *om-default-font2*))
   (setf (channelBox self) (om-make-dialog-item 'om-static-text
                                                 (om-make-point 27 25) 
                                                 (om-make-point 76 20) 
                                                 (number-to-string (midichannel (channelctr self)))
                                                 :font *om-default-font3b*))
   ))


(defmethod set-preset ((self MIDI-Mixer) preset)
  "sets the vals in nth preset in the editor (and send them)"
  (when preset
    (loop for chan in preset 
        for ch-ctrl in (channels-ctrl self)
        do
        
        (setf (program ch-ctrl) (nth 0 chan)
              (vol-ctrl ch-ctrl) (nth 1 chan)
              (pan-ctrl ch-ctrl) (nth 2 chan)
              (pitch-ctrl ch-ctrl) (nth 3 chan)
              (control1-num ch-ctrl) (nth 4 chan)
              (control1-val ch-ctrl) (nth 5 chan)
              (control2-num ch-ctrl) (nth 6 chan)
              (control2-val ch-ctrl) (nth 7 chan))
        )
    (send-midi-settings self)
  ))

(defmethod set-mixer-editor ((self MIDIMixerEditor) (ref MIDI-Mixer))
  (loop for ch-ctrl in (channels-ctrl ref) 
        for ch-panel in (ch-panels self) 
        do
        (set-channel-values ch-panel
                            :program (program ch-ctrl)
                            :vol (vol-ctrl ch-ctrl)
                            :pan (pan-ctrl ch-ctrl)
                            :pitch (pitch-ctrl ch-ctrl)
                            :ctr1 (control1-num ch-ctrl)
                            :ctr1val (control1-val ch-ctrl)
                            :ctr2 (control2-num ch-ctrl)
                            :ctr2val (control2-val ch-ctrl))
        )
  )


(defmethod get-current-values ((self MIDIMixerEditor))
  "get the current values as a list (preset)"
  (loop for chan-ctrl in (channels-ctrl (object self)) collect 
        (list (program chan-ctrl)
              (vol-ctrl chan-ctrl)
              (pan-ctrl chan-ctrl)
              (pitch-ctrl chan-ctrl)
              (control1-num chan-ctrl)
              (control1-val chan-ctrl)
              (control2-num chan-ctrl)
              (control2-val chan-ctrl)
              )))

(defmethod make-preset-view ((self MIDIMixerEditor))
  (let* ((preset-view (om-make-view 'om-view 
                                   :position (om-make-point (delta-tracks self) (- (h self) (delta-tracks self) 45)) 
                                   :scrollbars nil
                                   :retain-scrollbars nil
                                   :field-size  (om-make-point (- (* *channel-w* *audio-n-channels*) 5) 45)
                                   :size (om-make-point (- (* *channel-w* 16) (* 2 (delta-tracks self))) 45)
                                   :bg-color *om-dark-gray-color*))
         
         (title (om-make-dialog-item 'om-static-text
                                     (om-make-point 10 13)
                                     (om-make-point 130 20) "PRESETS :"
                                     :font *om-default-font1*
                                     :fg-color *om-white-color*))
             
         (preset-list (om-make-dialog-item 'om-pop-up-dialog-item 
                                    (om-make-point 75 12) 
                                    (om-make-point 120 12)
                                    ""
                                    :di-action #'(lambda (item)
                                                 (let ((presetnum (om-get-selected-item-index item))) ;;; 0 is reserved for the current values
                                                   (setf (current-preset (object self)) presetnum)
                                                   (when (> (current-preset (object self)) 0)
                                                     (set-preset (object self) (cadr (nth presetnum (presets (object self)))))
                                                     (set-mixer-editor self (object self))
                                                     )))
                                    :font *om-default-font1*
                                    :range (mapcar 'car (presets (object self)))
                                    :value (car (nth (current-preset (object self)) (presets (object self))))))

         (save-preset (om-make-dialog-item 'om-button
                                           (om-make-point 260 10)
                                           (om-make-point 75 12)
                                           "SAVE"
                                           :di-action (om-dialog-item-act item 
                                                        (if (> (current-preset (object self)) 0)
                                                            (progn
                                                              (setf (cadr (nth (current-preset (object self)) (presets (object self)))) (get-current-values self))
                                                              (print (string+ "Preset " (car (nth (current-preset (object self)) (presets (object self)))) " saved !"))
                                                              (save-midi-presets-in-preferences (object self)))
                                                          (om-message-dialog "Can't save: please select or create a preset !"))
                                                        )
                                           :font *om-default-font1*))
         
         (new-preset (om-make-dialog-item 'om-button
                                          (om-make-point 335 10)
                                          (om-make-point 75 12)
                                          "NEW"
                                          :di-action (om-dialog-item-act item 
                                                       (let ((name (om-get-user-string 
                                                                    "Enter a name for this preset" 
                                                                    :initial-string (format nil "Preset ~A" (length (presets (object self)))))))
                                                         (when name
                                                           (setf (presets (object self))
                                                                 (append (presets (object self))
                                                                         (list (list name (get-current-values self)))))
                                                           (om-enable-dialog-item preset-list t)
                                                           (om-set-item-list preset-list (mapcar 'car (presets (object self))))
                                                           (om-set-selected-item preset-list name)
                                                           (setf (current-preset (object self)) (1- (length (presets (object self)))))
                                                           (save-midi-presets-in-preferences (object self)))))
                                          :font *om-default-font1*))

         (delete-preset (om-make-dialog-item 'om-button
                                             (om-make-point 410 10)
                                             (om-make-point 75 12)
                                             "DELETE"
                                             :di-action (om-dialog-item-act item
                                                          (if (> (current-preset (object self)) 0)
                                                              (progn
                                                                (setf (presets (object self)) (append (subseq (presets (object self)) 0 (om-get-selected-item-index preset-list))
                                                                                                      (subseq (presets (object self)) (1+ (om-get-selected-item-index preset-list)))))
                                                                (om-set-item-list preset-list (mapcar 'car (presets (object self))))
                                                                (om-set-selected-item-index preset-list 0)
                                                                (if (<= (length (presets (object self))) 1) (om-enable-dialog-item preset-list nil))
                                                                (setf (current-preset (object self)) 0)
                                                                (save-midi-presets-in-preferences (object self)))
                                                            (om-beep))
                                                          )
                                             :font *om-default-font1*))

         )

    (if (<= (length (presets (object self))) 1) (om-enable-dialog-item preset-list nil))
    (om-add-subviews preset-view title preset-list save-preset new-preset delete-preset)    

    preset-view))
    

    

    
    
    


