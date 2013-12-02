(in-package :om)


(defmethod player-selection-settings-pane ((type (eql :midi)) paneports reference selected-player)
  (let ((midiport (get-edit-param reference 'outport))
        midilabel midiportmenu midiporttext)
    (apply 'om-remove-subviews (cons paneports (om-subviews paneports)))
    (om-add-subviews paneports
                     (setf midilabel (om-make-dialog-item 'om-static-text (om-make-point 10 y2) (om-make-point 300 20) 
                                                          "MIDI port mode:"
                                                          :enable (equal :midi (player-type selected-player)) 
                                                          :font *om-default-font1b*))
                     (setf midiportmenu (om-make-dialog-item 'om-pop-up-dialog-item 
                                                             (om-make-point 20 (incf y2 30)) (om-make-point 150 20) 
                                                             ""
                                                             :range '("Use notes MIDI port" "Set global port" "Use default MIDI port")
                                                             :value (cond ((null midiport) "Use notes MIDI port")
                                                                          ((numberp midiport) "Set global port")
                                                                          (t "Use default MIDI port"))
                                                                   :di-action (om-dialog-item-act item
                                                                                (case (om-get-selected-item-index item)
                                                                                  (0 (enable-numbox midiporttext nil)
                                                                                     (om-set-dialog-item-text midiporttext ""))
                                                                                  (1 (enable-numbox midiporttext t)
                                                                                     (om-set-dialog-item-text midiporttext 
                                                                                                              (format nil " ~D" (value midiporttext))))
                                                                                  (2 (enable-numbox midiporttext nil)
                                                                                     (set-value midiporttext *outmidiport*))))
                                                                   :enable (equal :midi (player-type selected-player))
                                                                   :font *om-default-font1*))
                     
                     (setf midiporttext (om-make-dialog-item 'numbox (om-make-point 190 (+ y2 2)) (om-make-point 40 22) 
                                                             (format nil " ~D" (if (equal :default midiport) *outmidiport* midiport)) 
                                                             :bg-color *om-white-color*
                                                             :value (if (numberp midiport) midiport *outmidiport*)
                                                             :enable (and (equal :midi (player-type selected-player))
                                                                          (numberp midiport))
                                                             :font *om-default-font1*))
                     )))


(defmethod set-param-from-settings-pane ((type (eql :midi)) paneports reference) 
  (let ((midiportmenu (nth 1 (om-subviews paneports)))
        (midiporttext (nth 2 (om-subviews paneports))))
    (set-edit-param reference 'outport (case (om-get-selected-item-index midiportmenu)
                                         (0 nil)
                                         (1 (value midiporttext))
                                         (2 :default)))))
  




#|

;;; UDP = NOT USED

(defmethod player-selection-settings-pane ((type (eql :udp)) paneports reference selected-player) 
  (let (udplabel udpportmenu udpporttext udphosttext)
    (apply 'om-remove-subviews (cons paneports (om-subviews paneports)))
    (let ((udpport (get-edit-param reference 'udp-outport)))
      (incf y2 40)
      (om-add-subviews paneports
                       (setf udplabel (om-make-dialog-item 'om-static-text (om-make-point 10 y2) (om-make-point 300 20) 
                                                           "UDP port mode:"
                                                           :enable (equal :udp (player-type selected-player)) 
                                                           :font *om-default-font1b*))
                       (setf udpportmenu (om-make-dialog-item 'om-pop-up-dialog-item 
                                                              (om-make-point 20 (incf y2 30)) (om-make-point 150 20) 
                                                              ""
                                                              :range '("Set global port" "Use default UDP out port")
                                                              :value (cond ((numberp udpport) "Set global port")
                                                                           (t "Use default UDP out port"))
                                                              :di-action (om-dialog-item-act item
                                                                           (case (om-get-selected-item-index item)
                                                                             (0 (enable-numbox udpporttext t)
                                                                                (om-enable-dialog-item udphosttext t)
                                                                                (om-set-dialog-item-text udpporttext 
                                                                                                         (format nil " ~D" (value udpporttext))))
                                                                             (1 (enable-numbox udpporttext nil)
                                                                                (om-enable-dialog-item udphosttext nil)
                                                                                (set-value udpporttext *microplayer-out-port*)
                                                                                (om-set-dialog-item-text udphosttext *microplayer-host*)))
                                                                           )
                                                              :enable (equal :udp (player-type selected-player))
                                                              :font *om-default-font1*))
                       
                       (setf udpporttext (om-make-dialog-item 'numbox (om-make-point 190 (+ y2 2)) (om-make-point 40 22) 
                                                              (format nil " ~D" (if (equal :default udpport) *microplayer-out-port* udpport)) 
                                                              :bg-color *om-white-color*
                                                              :value (if (numberp udpport) udpport *microplayer-out-port*)
                                                              :enable (and (equal :udp (player-type selected-player)) 
                                                                           (numberp udpport))
                                                              :font *om-default-font1*))
                       (setf udphosttext (om-make-dialog-item 'om-editable-text (om-make-point 190 (incf y2 25)) (om-make-point 80 22) 
                                                              *microplayer-host* 
                                                              :bg-color *om-white-color*
                                                              :enable (and (equal :udp (player-type selected-player)) 
                                                                           (numberp udpport))
                                                              :font *om-default-font1*))
                       ))))


(defmethod set-param-from-settings-pane ((type (eql :udp)) paneports reference) 
  (let ((udpportmenu (nth 1 (om-subviews paneports)))
        (udpporttext (nth 2 (om-subviews paneports))))
    (set-edit-param reference 'udp-outport (case (om-get-selected-item-index udpportmenu)
                                             (0 (value udpporttext))
                                             (1 :default)))))
|#

                                                  

