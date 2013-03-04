
(in-package :om)


;; called by player-menu-item

(defun select-player (obj &optional player)
  (let ((curplay (get-edit-param obj 'player))
        (out (get-edit-param obj 'outport)))
    (if player
        (let ((newparams (set-player-param-dialog player obj out)))
           (when newparams 
             (set-edit-param obj 'outport (nth 0 newparams))))
      (let ((newparams (select-player-dialog obj curplay out)))
        (when newparams 
          (set-edit-param obj 'player (nth 0 newparams))
          (set-edit-param obj 'outport (nth 1 newparams))
          (when (editorframe obj) 
            (update-controls-view (editorframe obj)))
          )
        ))
    ))

(defmethod update-controls-view ((self t)) nil)

(defmethod set-player-param-dialog ((player t) obj val) nil)

(defmethod set-player-param-dialog ((player (eql :midishare)) obj val)
  (let ((dialog (om-make-window 'om-dialog
                                :window-title (string+ "Midishare Port Settings for " (name obj))
                                :position :centered
                                :size (om-make-point 270 160)
                                :maximize nil :resizable nil
                                :font *om-default-font4*
                                :bg-color (om-make-color 0.623 0.623 0.623)))
        (pane (om-make-view 'om-view :bg-color *om-white-color*
                            :position (om-make-point 10 40) :size (om-make-point 320 65)))
        (i 0) portmenu porttext)
    (om-add-subviews pane
                     
                     (setf portmenu (om-make-dialog-item 'om-pop-up-dialog-item 
                                                         (om-make-point 20 (incf i 20)) (om-make-point 150 20) 
                                                         ""
                                                         :range '("Notes MIDI port" "Set global port" "Default MIDI port")
                                                         :value (cond ((null val) "Notes MIDI port")
                                                                      ((numberp val) "Set global port")
                                                                      (t "Default MIDI port"))
                                                         :enable (equal :midishare player)
                                                         :di-action (om-dialog-item-act item
                                                                      (case (om-get-selected-item-index item)
                                                                        (0 (enable-numbox porttext nil)
                                                                           (om-set-dialog-item-text porttext ""))
                                                                        (1 (enable-numbox porttext t)
                                                                           (om-set-dialog-item-text porttext 
                                                                                                    (format nil " ~D" (value porttext))))
                                                                        (2 (enable-numbox porttext nil)
                                                                           (set-value porttext *outmidiport*))))
                                                           :font *controls-font*))
                       
                       (setf porttext (om-make-dialog-item 'numbox (om-make-point 190 (+ i 2)) (om-make-point 40 22) 
                                                           (format nil " ~D" (if (equal :default val) *outmidiport* val)) 
                                                           :bg-color *om-white-color*
                                                           :value (if (numberp val) val *outmidiport*)
                                                           :enable (and (equal :midishare player) (numberp val))
                                                           :font *controls-font*))
                       )
                       
      (om-add-subviews dialog
                       (om-make-dialog-item 'om-static-text (om-make-point 10 10) (om-make-point 300 20) 
                                            (string+ "Select an OUTPUT port mode" ":")
                                            :font *controls-font*)
                       pane
                       
                       (om-make-dialog-item 'om-button (om-make-point 90 110) (om-make-point 80 24) "Cancel" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog nil)))
                       (om-make-dialog-item 'om-button (om-make-point 180 110) (om-make-point 80 24) "OK" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog 
                                                                                      (list 
                                                                                       (case (om-get-selected-item-index portmenu)
                                                                                         (0 nil)
                                                                                         (1 (value porttext))
                                                                                         (2 :default))
                                                                                       )))
                                            :default-button t))
      (om-modal-dialog dialog)))


(defmethod select-player-dialog ((self omboxeditcall) &optional (player nil) (midiout nil))
  (select-player-for-value (value self) self player midiout))

(defmethod select-player-dialog ((self editorview) &optional (player nil) (midiout nil))
  (select-player-for-value (object self) self player midiout))

(defmethod select-player-dialog ((self temporalbox) &optional (player nil) (midiout nil))
  (select-player-for-value (car (value self)) self player midiout))

(defmethod select-player-dialog ((self t) &optional (player nil) (midiout nil))
  (om-beep))


(defmethod select-player-for-value ((value t) box player midiout)
  (om-beep))

(defmethod select-player-for-value ((value score-element) box player midiout)
  (select-score-player box player midiout))

(defmethod select-player-for-value ((value note) box player midiout)
  (select-score-player box player midiout))

(defmethod select-player-for-value ((value maquette-obj) box player midiout)
  (select-score-player box player midiout))

(defun select-score-player (self &optional (player nil) (midiout nil))
  (let ((rep (select-score-player-dialog self player midiout)))
    (when (equal :microplayer (car rep))
      (launch-microplayer-app))
    rep))

(defun select-score-player-dialog (self &optional (player nil) (midiout nil))
  (when (member :midi-project *features*)
    (let ((dialog (om-make-window 'om-dialog
                                :window-title (string+ "Player Settings for " (name self))
                                :position :centered
                                :size (om-make-point 350 240)
                                :maximize nil :resizable nil
                                :font *om-default-font4*
                                :bg-color (om-make-color 0.623 0.623 0.623)))
        (pane (om-make-view 'om-view :bg-color *om-white-color*
                            :position (om-make-point 10 40) :size (om-make-point 320 140)))
        (i 0) msplay microplay portlabel portmenu porttext)
    (om-add-subviews pane
                     (setf msplay (om-make-dialog-item 'om-radio-button (om-make-point 10 (incf i 10))
                                                       (om-make-point 300 20) "MidiShare (default OM player)"
                                                       :checked-p (equal :midishare player)
                                                       :di-action (om-dialog-item-act item
                                                                    (om-set-fg-color portlabel *om-black-color*)
                                                                      (if (= (om-get-selected-item-index portmenu) 1)
                                                                          (om-enable-dialog-item porttext t))
                                                                      (om-enable-dialog-item portmenu t))))
                       (setf portlabel
                             (om-make-dialog-item 'om-static-text (om-make-point 40 (incf i 30)) (om-make-point 80 20) 
                                                  "Port"
                                                  :fg-color (if (equal :midishare player) *om-black-color* *om-gray-color*)
                                                  :font *controls-font*))
                       
                       (setf portmenu (om-make-dialog-item 'om-pop-up-dialog-item 
                                                           (om-make-point 100 i) (om-make-point 150 20) 
                                                           ""
                                                           :range '("Notes MIDI port" "Set global port" "Default MIDI port")
                                                           :value (cond ((null midiout) "Notes MIDI port")
                                                                        ((numberp midiout) "Set global port")
                                                                        (t "Default MIDI port"))
                                                           :enable (equal :midishare player)
                                                           :di-action (om-dialog-item-act item
                                                                        (case (om-get-selected-item-index item)
                                                                          (0 (enable-numbox porttext nil)
                                                                             (om-set-dialog-item-text porttext ""))
                                                                          (1 (enable-numbox porttext t)
                                                                             (om-set-dialog-item-text porttext 
                                                                                                      (format nil " ~D" (value porttext))))
                                                                          (2 (enable-numbox porttext nil)
                                                                             (set-value porttext *outmidiport*))))
                                                           :font *controls-font*))
                       
                       (setf porttext (om-make-dialog-item 'numbox (om-make-point 270 (+ i 2)) (om-make-point 40 22) 
                                                           (format nil " ~D" (if (equal :default midiout) *outmidiport* midiout)) 
                                                           :bg-color *om-white-color*
                                                           :value (if (numberp midiout) midiout *outmidiport*)
                                                           :enable (and (equal :midishare player) (numberp midiout))
                                                           :font *controls-font*))
                       
                       (setf microplay (om-make-dialog-item 'om-radio-button (om-make-point 10 (incf i 40))
                                                            (om-make-point 300 20) "MicroPlayer (externals Max/OSC player)"
                                                            :checked-p (equal :microplayer player)
                                                            :di-action (om-dialog-item-act item
                                                                         (om-set-fg-color portlabel *om-gray-color*)
                                                                         (enable-numbox porttext nil)
                                                                         (om-enable-dialog-item portmenu nil))))
                       )
      (om-add-subviews dialog
                       (om-make-dialog-item 'om-static-text (om-make-point 10 10) (om-make-point 300 20) 
                                            (string+ "Select a player mode for " (name self) " :")
                                            :font *controls-font*)
                       pane
                       
                       (om-make-dialog-item 'om-button (om-make-point 170 195) (om-make-point 80 24) "Cancel" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog nil)))
                       (om-make-dialog-item 'om-button (om-make-point 260 195) (om-make-point 80 24) "OK" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog 
                                                                                      (list 
                                                                                       (if (om-checked-p msplay) 
                                                                                           :midishare :microplayer)
                                                                                       (case (om-get-selected-item-index portmenu)
                                                                                         (0 nil)
                                                                                         (1 (value porttext))
                                                                                         (2 :default))
                                                                                       )))
                                            :default-button t))
      (om-modal-dialog dialog))))


