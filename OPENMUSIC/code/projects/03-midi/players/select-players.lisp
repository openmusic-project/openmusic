(in-package :om)

;;; FOR THE REFERENCE IF IT IS NOT AN EDITOR
(defmethod update-controls-view ((self t)) nil)

;; called by 'reference' (e.g. Box or Editor) to change the player
;; reference maty have stored options for the other players as well
;; this functions manages all the edition-params settings in reference but not the possible extra actions to perform after these changes

(defmethod reference-object ((self t)) (object self))

(defun select-player (reference)
  (let* ((players-in-dialog (enabled-players-for-object (reference-object reference)))
         
         (dialog (om-make-window 'om-dialog
                                 :window-title (string+ "Player Settings for " (name reference))
                                 :position :centered
                                 :size (om-make-point 690 (+ 120 (* (length players-in-dialog) 60)))
                                 :maximize nil :resizable nil
                                 :font *om-default-font4*
                                 :bg-color (om-make-color 0.623 0.623 0.623)))
          
          (midi? (find :midi players-in-dialog :key 'player-type))
          (udp? NIL) ;;; (find :udp players-in-dialog :key 'player-type))
          
          (paneplayer (om-make-view 'om-view :bg-color *om-white-color*
                              :position (om-make-point 10 40) :size (om-make-point 320 (+ 20 (* (length players-in-dialog) 60)))))
          (paneports (om-make-view 'om-view :bg-color *om-white-color*
                              :position (om-make-point 350 40) :size (om-make-point 320 170)))
          (y 10) (y2 10)
          (selected-player (get-edit-param reference 'player))
          midilabel midiportmenu midiporttext udplabel udpportmenu udpporttext udphosttext)

      (om-add-subviews dialog
                       (om-make-dialog-item 'om-static-text (om-make-point 10 y) (om-make-point 300 20) 
                                            (if players-in-dialog (string+ "Select a player mode for " (name reference) " :")
                                              (string+ "No player available for " (name reference) "..."))
                                            :font *om-default-font1b*))
      
      (when midi?
        (let ((midiport (get-edit-param reference 'outport)))
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
      
      (when udp?
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
                           )))

      (loop for pl in players-in-dialog do
            (om-add-subviews paneplayer
                             (om-make-dialog-item 'om-radio-button (om-make-point 10 y)
                                                  (om-make-point 300 20) (player-name pl)
                                                  :checked-p (equal pl selected-player)
                                                  :di-action (let ((p pl))
                                                               (om-dialog-item-act item
                                                                 (declare (ignore item))
                                                                 (setf selected-player p)
                                                                 (let ((midiplay (equal :midi (player-type p))))
                                                                   (when midi?
                                                                     (om-enable-dialog-item midilabel midiplay)
                                                                     (om-enable-dialog-item midiportmenu midiplay)
                                                                     (enable-numbox midiporttext midiplay))
                                                                   (when udp?
                                                                     (om-enable-dialog-item udplabel (not midiplay))
                                                                     (om-enable-dialog-item udpportmenu (not midiplay))
                                                                     (enable-numbox udpporttext (not midiplay))
                                                                     (om-enable-dialog-item udphosttext (not midiplay))
                                                                     ))))
                                                  :font *om-default-font2*)
                             (om-make-dialog-item 'om-static-text (om-make-point 40 (+ y 20)) (om-make-point 160 20) 
                                                  (string+ "type: " (if (player-type pl) (symbol-name (player-type pl)) "undefined"))
                                                  :font *om-default-font1*)
                             (om-make-dialog-item 'om-static-text (om-make-point 40 (+ y 35)) (om-make-point 300 20) 
                                                  (player-desc pl)
                                                  :font *om-default-font1*)
                             )
            (incf y 60))
                             
      (incf y 60)

      (om-add-subviews dialog
                       paneplayer paneports
                       (om-make-dialog-item 'om-button (om-make-point 170 y) (om-make-point 80 24) "Cancel" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (om-return-from-modal-dialog dialog nil)))
                       (om-make-dialog-item 'om-button (om-make-point 260 y) (om-make-point 80 24) "OK" 
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (set-edit-param reference 'player selected-player)
                                                         (when midi?
                                                           (set-edit-param reference 'outport (case (om-get-selected-item-index midiportmenu)
                                                                                                (0 nil)
                                                                                                (1 (value midiporttext))
                                                                                                (2 :default)))
                                                           )
                                                         (when udp?
                                                           (set-edit-param reference 'udp-outport (case (om-get-selected-item-index udpportmenu)
                                                                                                    (0 (value udpporttext))
                                                                                                    (1 :default)))
                                                           )
                                                           
                                                         (om-return-from-modal-dialog dialog selected-player)
                                                         )
                                            :default-button t))
      (om-modal-dialog dialog)))



#|
(defmethod select-player-dialog ((self omboxeditcall) &optional (player nil) (midiout nil))
  (select-player-for-value (value self) self player midiout))

(defmethod select-player-dialog ((self editorview) &optional (player nil) (midiout nil))
  (select-player-for-value (object self) self player midiout))

(defmethod select-player-dialog ((self temporalbox) &optional (player nil) (midiout nil))
  (select-player-for-value (car (value self)) self player midiout))

(defmethod select-player-dialog ((self t) &optional (player nil) (midiout nil))
  (om-beep))


(defmethod select-player-for-value ((value t) box player params)
  (let ((rep (select-player-dialog box player params)))
    (player-special-action (car rep))
    rep))

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
|#


