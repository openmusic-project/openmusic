
(in-package :om)


(defmethod get-default-score-params ((self sound))
  (pairlis '(approx fontsize staff cmnpref deltapict outport inport player
             zoom notechancolor? grillestep mode winsize winpos score-mode obj-mode show-stems scale) 
           (list *global-midi-approx* *music-fontsize* *default-satff* (make-instance 'edition-values) (om-make-point 0 0) 
                 nil *InMidiPort* :libaudio
                 1 nil 1000 0 (om-make-point 370 280) (om-make-point 400 20) 0 1 t nil)))

(defmethod select-player-for-value ((value sound) box player out)
  (let ((rep (select-audio-player-dialog box player out)))
    (player-special-action (car rep))
    rep))

(defmethod audio-player-name ((self t)) "XXX")
(defmethod audio-player-desc ((self t)) "xxx")
(defmethod player-special-action ((self t)) nil)

(defmethod audio-record-start ((self t)) (om-beep-msg "This player has no audio recording feature"))
(defmethod audio-record-stop ((self t)) (om-beep))

(defmethod get-score-player ((self soundpanel)) 
  (interne (get-edit-param (editor self) 'player)))
  
(defparameter *audio-players* '(:libaudio :multiplayer))

(defmethod audio-player-name ((self (eql :libaudio))) "LibAudioStream")
(defmethod audio-player-desc ((self (eql :libaudio))) "internal OM Player")

(defmethod audio-player-name ((self (eql :multiplayer))) "MultiPlayer")
(defmethod audio-player-desc ((self (eql :multiplayer))) "external Max/OSC player")
(defmethod player-special-action ((self (eql :multiplayer))) (launch-multiplayer-app))


(defun select-audio-player-dialog (self &optional (player nil) (out nil))
  (let* ((pane-h (+ 20 (* (length *audio-players*) 25)))
         (dialog (om-make-window 'om-dialog
                                :window-title (string+ "Player Settings for " (name self))
                                :position :centered
                                :size (om-make-point 350 (+ 120 pane-h))
                                :maximize nil :resizable nil
                                :font *om-default-font4*
                                :bg-color (om-make-color 0.623 0.623 0.623)))
        (pane (om-make-view 'om-view :bg-color *om-white-color*
                            :position (om-make-point 10 40) 
                            :size (om-make-point 320 pane-h)))
        (i 10) player-items)
    (setf player-items
         (loop for pl in *audio-players* collect
               (let ((item (om-make-dialog-item 'om-radio-button (om-make-point 10 i)
                                                (om-make-point 260 20) 
                                                (string+ (audio-player-name pl) " (" (audio-player-desc pl) ")")
                                                :checked-p (equal pl player)
                                                :di-action (om-dialog-item-act item t))))
                 (om-add-subviews pane item)
                 (incf i 25)
                 item)))

      (om-add-subviews dialog
                       (om-make-dialog-item 'om-static-text (om-make-point 10 10) (om-make-point 300 20) 
                                            (string+ "Select a player for " (name self) " :")
                                            :font *controls-font*)
                       pane
                       
                       (om-make-dialog-item 'om-button (om-make-point 165 (+ 60 pane-h))
                                            (om-make-point 80 24) "Cancel" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog nil)))
                       (om-make-dialog-item 'om-button (om-make-point 250 (+ 60 pane-h))
                                            (om-make-point 80 24) "OK" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog 
                                                                                      (let ((p (position-if 'om-checked-p player-items)))
                                                                                        (and p (list (nth p *audio-players*)))))
                                                         )
                                            :default-button t))
      (om-modal-dialog dialog)))