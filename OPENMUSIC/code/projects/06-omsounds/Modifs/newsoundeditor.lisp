(in-package :om)

(defmethod remove-extra ((self OMPatch) (box OMBoxEditCall))
  (let ()
    (if (typep (value box) 'faust-effect-console)
        (let* ((console (value box))
               (ptr (effect-ptr console))
               (track (tracknum console))) (print ptr)
          (if ptr 
              (las::RemoveAudioEffect (gethash track oa::*effects-lists*) ptr)
          )))
    (if (typep (value box) 'sound)
        (let* ((snd (value box))
               (player (oa::assoc-player snd))
               (status-list (if (eq player oa::*audio-player-hidden*)
                                oa::*audio-player-hidden-tracks-info*
                              oa::*audio-player-visible-tracks-info*))
               (chan (if (eq player oa::*audio-player-hidden*)
                         (oa::tracknum-sys snd)
                       (tracknum snd)))
               (loadedsnd (car (gethash chan status-list)))
               (status (cadr (gethash chan status-list))))
          (if (eq snd loadedsnd)
              (let () 
                (oa::om-smart-stop snd)
                (setf (car (gethash chan status-list)) nil)))
          ))
    (call-next-method))
  )
;/////////////////////////////////////////////////////////////////
;////////////////       SOUNDEDITOR.LISP       ///////////////////
;/////////////////////////////////////////////////////////////////

;Disable palette (now palette is in the SoundEditor)
(defmethod editor-has-palette-p ((self soundEditor)) nil)

;Here we create the bottom bar of the SoundEditor.
;It contains a little transport palette, and you can also assign a track or not to the sound of the SoundEditor.
(defmethod add-sound-params ((self Aiff-control))
  (let* ((x0 220)
        (snd (object (om-view-container self)))
        (sndpanel (panel (om-view-container self)))) 
    (setf (dyn-ctrl-list self) (make-snd-ctrl-list self))
    
    (om-add-subviews self
                     (om-make-view 'om-icon-button :position (om-make-point (- x0 210) 5) :size (om-make-point 25 25)
                                         :icon1 "play" :icon2 "play-pushed"
                                         :action #'(lambda (item) (oa::om-smart-play sndpanel)))

                     (om-make-view 'om-icon-button :position (om-make-point (- x0 185) 5) :size (om-make-point 25 25)
                                         :icon1 "pause" :icon2 "pause"
                                         :action #'(lambda (item) (oa::om-smart-pause sndpanel)))

                     (om-make-view 'om-icon-button :position (om-make-point (- x0 160) 5) :size (om-make-point 25 25)
                                         :icon1 "stop" :icon2 "stop-pushed"
                                         :action #'(lambda (item) (oa::om-smart-stop snd sndpanel)))

                     (om-make-dialog-item 'om-check-box (om-make-point (- x0 90) 4)
                               (om-make-point 130 20) "Send to track :"
                               :checked-p (if (oa::assoc-player snd) (if (eq (oa::assoc-player snd) oa::*audio-player-hidden*) nil t) nil)
                               :di-action (om-dialog-item-act item (let ()
                                                                     (oa::om-smart-stop snd sndpanel) 
                                                                     (oa::om-send-to-track sndpanel))))

                     (om-make-dialog-item 'om-static-text (om-make-point (incf x0 80) 8) (om-make-point 40 20)
                                          "Vol" :font *om-default-font1* :bg-color *controls-color*)

                     (om-make-dialog-item 'om-static-text (om-make-point (incf x0 85) 8) (om-make-point 40 20)
                                          "Pan" :font *om-default-font1* :bg-color *controls-color*)

                     (om-make-dialog-item 'om-static-text (om-make-point (incf x0 85) 8) (om-make-point 50 20)
                                          "Player" :font *om-default-font1*)
                     
                     (first (dyn-ctrl-list self))
                     (second (dyn-ctrl-list self))
                     (third (dyn-ctrl-list self))
                     (fourth (dyn-ctrl-list self))
                     )
    t))


;SoundEditor init. The add-sound-params is now the last to be called, to give us access to the sound panel when we call it.
(defmethod initialize-instance :after ((self soundEditor) &rest l)
   (declare (ignore l))
   (let* ((ed-view (om-make-view (get-panel-class self) 
                     :owner self
                     :scrollbars :h
                     :position (om-make-point 0 *titlebars-h*) 
                     :size (om-make-point (w self) (- (h self) (+ 25 *titlebars-h* (get-control-h self))))))
          (rulerx (om-make-view 'sound-ruler
                    :owner self
                    :axe 'x
                    :assoc-view ed-view
                    :zoom 1000
                    :minzoom 1
                    :position (om-make-point 0 (- (h self) (get-control-h self) 25)) 
                    :size (om-make-point (w self) 25)))
          (control (om-make-view (get-score-class-ctrls self) 
                     :owner self
                     :position (om-make-point 0 (- (h self) (get-control-h self))) 
                     :size (om-make-point (w self) (get-control-h self))))
          (prev (om-make-view 'full-preview 
                     :owner self
                     :position (om-make-point 0 (get-control-h self)) 
                     :size (om-make-point (w self) (get-control-h self)))))
     (setf (mode self) 8)
     (setf (panel self) ed-view)
     (setf (control self) control)
     (setf (preview self) prev)
     (om-set-bg-color (control self) *controls-color*)
     (setf (sndpict self) (sound-get-pict (object self)))
     ;(setf (sndptr self) (get-sound-data (object self)))
     ;(setf (sndpict self) (sound-get-pict (sndptr self)))
     (setf (cursor-p (panel self)) t)
     (setf (rulerx ed-view) rulerx)
     (setf (rangex ed-view) (list 0 (get-obj-dur (object self))))
     (setf (bounds-x ed-view) (list 0 (get-obj-dur (object self))))
     (set-units-ruler ed-view rulerx)
     (add-sound-params control)
     (om-invalidate-view ed-view)))



;Correction of the Space bar key event
(defmethod handle-key-event ((self soundPanel) char)
   (case char
     (#\g (grille-on-off self))
     (#\A (align-markers self))
     (#\h (show-help-window "Commands for SOUND Editor" (get-help-list (editor self))))
     (:om-key-delete (delete-sound-marker self))
     (:om-key-esc (reset-cursor self))
     (#\SPACE (let* ((snd (object (om-view-container self)))
                     (player (oa::assoc-player snd))
                     (chan (if (eq player oa::*audio-player-hidden*)
                               (oa::tracknum-sys snd)
                             (tracknum snd)))
                     (status-list (if (eq player oa::*audio-player-hidden*)
                                      oa::*audio-player-hidden-tracks-info*
                                    oa::*audio-player-visible-tracks-info*)))
                (if (eq snd (car (gethash chan status-list)))
                    (if (string-equal "Playing" (cadr (gethash chan status-list)))
                        (oa::om-smart-stop snd self)
                      (oa::om-smart-play self))
                  (oa::om-smart-play self))))
     (otherwise (call-next-method))))


;Correction of help list ("Play/Stop" becomes "Play/Pause")
(defmethod get-help-list ((self soundeditor))
  (list '((alt+clic "Add Marker")
          (del "Delete Selected Markers")
          (("g") "Sow/Hide Grid")
          (("A") "Align Selected Markers to Grid")
          (esc "Reset cursor")
          (space "Play/Stop"))))


;Adaptation of display because of transport in window
;Adaptation of the track display : when it shows 1, it sends 0 to the system (las player channels start from 0)
(defmethod make-snd-ctrl-list ((self Aiff-control))
  (let ((x0 260)
        (snd (object (om-view-container self)))
        (sndpanel (panel (om-view-container self))))
    (list 
     (om-make-dialog-item 'numBox
                          (om-make-point x0 8)
                          (om-make-point 28 18) (format () " ~D" (tracknum (object (om-view-container self))))
                          :min-val 1
                          :max-val 32
                          :font *om-default-font1*
                          :bg-color *om-white-color*
                          :value (tracknum (object (om-view-container self)))
                          :afterfun #'(lambda (item)
                                        (let ()
                                          (if (eq (oa::assoc-player snd) *audio-player-visible*)
                                            (oa::om-smart-stop snd sndpanel))
                                          (setf (tracknum (object (om-view-container self))) (- (value item) 1))
                                          (report-modifications (om-view-container self))))
                          )
     (om-make-dialog-item 'numBox
                          (om-make-point (incf x0 70) 8)
                          (om-make-point 40 18) (format () " ~D" (vol (object (om-view-container self))))
                          :min-val 0
                          :max-val 100
                          :bg-color *om-white-color*
                          :font *om-default-font1*
                          :value (vol (object (om-view-container self)))
                          :afterfun #'(lambda (item)
                                        (setf (vol (object (om-view-container self))) (value item))
                                        (report-modifications (om-view-container self)))
                          )
     (om-make-dialog-item 'numBox
                          (om-make-point (incf x0 90) 8)
                          (om-make-point 40 18) (format () " ~D" (pan (object (om-view-container self))))
                          :min-val -100
                          :max-val 100
                          :bg-color *om-white-color*
                          :font *om-default-font1*
                          :value (pan (object (om-view-container self)))
                          :afterfun #'(lambda (item)
                                        (setf (pan (object (om-view-container self))) (value item))
                                        (report-modifications (om-view-container self)))
                          )
     (om-make-dialog-item 'om-pop-up-dialog-item 
                                            (om-make-point (incf x0 90) 5) 
                                            (om-make-point 100 20) ""
                                            :font *om-default-font1*
                                            :range (mapcar 'audio-player-name *audio-players*)
                                            :value (audio-player-name (get-edit-param (om-view-container self) 'player))
                                            :di-action  (om-dialog-item-act item 
                                                          (change-player (panel (om-view-container self)) 
                                                                         (nth (om-get-selected-item-index item) *audio-players*)))
                                            )
     )))