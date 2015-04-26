;==============================
; THE MIDI CONSOLE IS A SET OF MIDI SETTINGS TO BE SENT SIMULTANEOUSLY TO THE PLAYER 
; (A MIDI SETUP)
;==============================

(in-package :om)

;================================================
;=== CHANNEL CONTROLLER                     
;=== a single track controller 
;================================================
(defclass* Channel-Ctrl () 
  ((midiport :initform nil :initarg :midiport :accessor midiport :type integer)
   (midichannel :initform 1 :initarg :midichannel :accessor midichannel :type integer)
   (program :initform 0 :initarg :program :accessor program :type integer)
   (pan-ctrl :initform 64 :initarg :pan-ctrl  :accessor pan-ctrl :type integer)
   (control1-num :initform 1 :initarg :control1-num :accessor control1-num :type integer)
   (control2-num :initform 2 :initarg :control2-num :accessor control2-num :type integer)
   (control1-val :initform 0 :initarg :control1-val :accessor control1-val :type integer)
   (control2-val :initform 0 :initarg :control2-val :accessor control2-val :type integer)
   (vol-ctrl :initform 100 :initarg :vol-ctrl :accessor vol-ctrl :type integer)
   (pitch-ctrl :initform 8192 :initarg :pitch-ctrl :accessor pitch-ctrl :type integer)))

(defmethod channel-ctrl-p ((self channel-ctrl))  t)
(defmethod channel-ctrl-p ((self t)) nil)

;====================================
;=== SETTINGS CONTROLLER          ===
;=== a set of channel controllers ===
;====================================
(defclass* settings-ctrl (midi-score-element)
   ((midiport :initform nil :initarg :midiport :accessor midiport :type integer :documentation "output port number")
    (miditrack :initform 0 :accessor miditrack)
    (nbtracks :initform 1 :initarg :nbtracks :accessor nbtracks :type integer :documentation "number of tracks")
    (channels-ctrl :initform nil :accessor channels-ctrl :type t))
  (:icon 918))

;;; miditrack useful for QT player
(defmethod! set-track ((self settings-ctrl) tracks)
  :icon 917
  (setf (miditrack self) tracks)
  self)

;;; used in the maquette, to check if we need to instanciate a specific track for this object
(defmethod obj-in-sep-track ((self settings-ctrl)) nil)

(defmethod get-obj-dur ((self settings-ctrl)) 0)

(defmethod allowed-in-maq-p ((self settings-ctrl))  t)

;;; SOME SUBCLASSES MAY USE DIFFERENT CHANNEL CONTROLLERS
(defmethod get-channel-ctrl-class ((self t)) 'channel-ctrl)

(defmethod initialize-instance :after ((self settings-ctrl) &rest l)
   (declare (ignore l))
   (if (< (nbtracks self) 1) (setf (nbtracks self) 1))
   (if (> (nbtracks self) 16) (setf (nbtracks self) 16))

   (setf (channels-ctrl self) 
         (loop for i from 1 to (nbtracks self) collect (make-instance (get-channel-ctrl-class self) 
                                                         :midiport (midiport self)
                                                         :midichannel i)))
   )

;===========================
; THE 'REAL' OBJECT USED IN OM
; (TODO: REMOVE SETTINGS-CTRL SUPERCLASS?)
;===========================

(defclass* midi-mix-console (settings-ctrl) ()
  (:icon 918)
(:documentation "
   MIDI-MIX-CONSOLE represents a set of control events (volume, pan, program change, etc.) on <nbtracks> channels.

Open the mixing console editor after evaluating the box with the right number of tracks.
Modifications are sent immediately when performed in the editor.

The MIDI-MIX-CONSOLE object can also be 'played' as a musical object, from a patch window or in a maquette.
In this case, all internal events are sent simultaneously.
"
 ))

(add-player-for-object 'midi-mix-console '(:midi-player :midishare))

(defmethod default-edition-params ((self midi-mix-console))
  (pairlis '(player)
           '(:midi-player)))

(defmethod get-impulsion-pict ((self midi-mix-console)) 
  (om-load-and-store-picture "audiotrack-bg" 'internal))

(defmethod draw-mini-view  ((self t) (value midi-mix-console)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value midi-mix-console)) 
   (om-invalidate-view self t))


(defmethod draw-obj-in-rect ((self midi-mix-console) x x1 y y1 edparams view)
  (let ((pw (round (w view) (nbtracks self)))
        (pic (om-load-and-store-picture "audiotrack-bg" 'internal)))
    (loop for i from 0 to (nbtracks self) do
          (om-draw-picture view pic :pos (om-make-point (* i pw) 0)
                           :size (om-make-point pw (h view))
                           ))))


(defmethod omNG-copy ((self midi-mix-console))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self)
                :midiport ,(midiport self)
                :nbtracks ,(nbtracks self))))
     (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                             (omNG-copy ctrl))))
     (setf (miditrack rep) ',(miditrack self))
     rep
     ))

(defmethod copy-container  ((self midi-mix-console) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance (type-of self)
                :midiport (midiport self)
                :nbtracks (nbtracks self))))
    (setf (channels-ctrl rep) (loop for ctrl in (channels-ctrl self) collect
                                    (eval (omNG-copy ctrl))))
    (setf (miditrack rep) (miditrack self))
    rep
    ))

(defmethod omNG-save ((self midi-mix-console) &optional (values? nil))
  "Cons a Lisp expression that retunr a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self) 
                :midiport ,(midiport self) 
                :nbtracks ,(nbtracks self))))
     (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                             (omNG-save ctrl))))
     (setf (miditrack rep) ',(miditrack self))
     rep
     ))

;======================
;=== GET MIDIEVENTS ===
;======================

(defmethod! get-midievents ((self settings-ctrl) &optional test)
  (let ((evt-list (loop for chan-ctrl in (channels-ctrl self) append 
                        (get-midievents chan-ctrl test))))
    (when (miditrack self)
      (setf evt-list
            (loop for tr in (list! (miditrack self)) append
                  (loop for evt in evt-list collect
                        (let ((new-ev (clone evt)))
                          (setf (ev-ref new-ev) tr)
                          new-ev))))
       )
    
    (get-midievents evt-list test)))


(defmethod! get-midievents ((self channel-ctrl) &optional test)
   (list
    (make-instance 'MidiEvent
                   :ev-date 0
                   :ev-ref 0
                   :ev-type :ProgChange
                   :ev-chan (midichannel self)
                   :ev-port (midiport self)
                   :ev-fields (program self))
        
    (make-instance 'MidiEvent
                   :ev-date 0
                   :ev-ref 0
                   :ev-type :CtrlChange
                   :ev-chan (midichannel self)
                   :ev-port (midiport self)
                   :ev-fields (list 7 (vol-ctrl self)))
        
    (make-instance 'MidiEvent
                   :ev-date 0
                   :ev-ref 0
                   :ev-type :CtrlChange
                   :ev-chan (midichannel self)
                   :ev-port (midiport self)
                   :ev-fields (list 10 (pan-ctrl self)))
        
    (make-instance 'MidiEvent
                   :ev-date 0
                   :ev-ref 0
                   :ev-type :PitchBend
                   :ev-chan (midichannel self)
                   :ev-port (midiport self)
                   :ev-fields (list (pitch-ctrl self)))
        
    (make-instance 'MidiEvent
                   :ev-date 0
                   :ev-ref 0
                   :ev-type :CtrlChange
                   :ev-chan (midichannel self)
                   :ev-port (midiport self)
                   :ev-fields (list (control1-num self) (control1-val self)))
        
    (make-instance 'MidiEvent
                   :ev-date 0
                   :ev-ref 0
                   :ev-type :CtrlChange
                   :ev-chan (midichannel self)
                   :ev-port (midiport self)
                   :ev-fields (list (control2-num self) (control2-val self)))
    ))


;;;===============================
;;; FOR PLAY OR SAVE AS MIDI
;;;===============================

;;; !!! For QT Player, prpogram changes must be sent on the same channel !
(defmethod PrepareToPlay ((player (eql :midi)) (self settings-ctrl) at &key approx port interval voice)
  (PrepareToPlay player (get-midievents self) at 
                 :approx approx 
                 :port port
                 :interval interval
                 :voice voice))

;=================================
;=== SENDING CONTROLLER SETTINGS :
;=================================

(defmethod channel-send-prog ((self channel-ctrl))
  (let ((event (om-midi::make-midi-evt :type :ProgChange 
                              :chan (midichannel self)
                              :port (or (midiport self) *def-midi-out*)
                              :fields (list (program self)))))
    
    (midi-send-evt event)
    t))

    
(defmethod channel-send-vol ((self channel-ctrl))
  (let ((event (om-midi::make-midi-evt :type :CtrlChange
                                 :chan (midichannel self) 
                                 :port (or (midiport self) *def-midi-out*)
                                 :fields (list 7  (vol-ctrl self)))))
    (midi-send-evt event)
    t))

(defmethod channel-send-pan ((self channel-ctrl))
  (let ((event  (om-midi::make-midi-evt :type :CtrlChange
                                 :chan (midichannel self) :port (or (midiport self) *def-midi-out*)
                                 :fields (list 10 (pan-ctrl self)))))
    (midi-send-evt event)
    t))
    
(defmethod channel-send-ct1 ((self channel-ctrl))
  (let ((event  (om-midi::make-midi-evt :type :CtrlChange
                                 :chan (midichannel self) :port (or (midiport self) *def-midi-out*)
                                 :fields (list (control1-num self) (control1-val self)))))
    (midi-send-evt event)
    t))
    
(defmethod channel-send-ct2 ((self channel-ctrl))
  (let ((event  (om-midi::make-midi-evt :type :CtrlChange
                                 :chan (midichannel self) :port (or (midiport self) *def-midi-out*)
                                 :fields (list (control2-num self) (control2-val self)))))    
    (midi-send-evt event)
    t))

(defmethod channel-send-pitch ((self channel-ctrl))
  (let ((event (om-midi::make-midi-evt :type :PitchBend
                              :chan (midichannel self) :port (or (midiport self) *def-midi-out*)
                              :fields (pitch-ctrl self))))
    (midi-send-evt event)
    t))

(defmethod send-midi-settings ((self channel-ctrl))
  (channel-send-prog self) 
  (channel-send-vol self) 
  (channel-send-pan self) 
  (channel-send-ct1 self) 
  (channel-send-ct2 self) 
  (channel-send-pitch self))

(defmethod send-midi-settings ((self settings-ctrl))
  (loop for chan-ctrl in (channels-ctrl self) do
        (send-midi-settings chan-ctrl)))


;;================================================================
; EDITOR
;;================================================================

(defclass ConsoleEditor (EditorView) 
  ((ch-panels :initform nil :accessor ch-panels :type list)
   (presets-view :initform nil :initarg :presets-view :accessor presets-view)
   (delta-tracks :initform 0 :accessor delta-tracks :initarg :delta-tracks)
   (send-rt :initform nil :accessor send-rt :initarg :send-rt)))


(defmethod class-has-editor-p  ((self settings-ctrl)) t )
(defmethod get-editor-class ((self settings-ctrl)) 'ConsoleEditor)

;=== The Editor will be a scrollable editor with fixed size 
;(defmethod get-win-ed-size ((self settings-ctrl)) (om-make-point (+ 6 (min (* *channel-w* 6) (* *channel-w* (nbtracks self)))) 560))
(defmethod get-win-ed-size ((self settings-ctrl)) (om-make-point (min (* *channel-w* 16) (* *channel-w* (nbtracks self))) 560))

(defmethod default-edition-params ((self settings-ctrl))
  (pairlis '(winsize winpos) 
           (list (get-win-ed-size self) (om-make-point 300 20))))

(defmethod make-editor-window ((class (eql 'ConsoleEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
    win))




(defmethod get-panel-class ((Self ConsoleEditor)) 'controllerPanel)

(defmethod update-subviews ((Self ConsoleEditor))
   (om-set-view-size (panel self) (om-make-point (w self) (h self))))


;=== MAIN PANEL ===
(defclass controllerPanel (om-scroller) ()
  ;;;(:default-initargs :scrollbars :h :retain-scrollbars t)
   )

(defmethod editor ((self controllerPanel)) 
  (om-view-container self))

(defmethod get-object ((Self controllerPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self controllerPanel))
  (report-modifications (om-view-container self)))
                                   

;======== CHaNNeL ConTrOllEr PAneL =====
;;; superclass for channelpanelview and simplechanelpanel
(defclass channelPanel () 
  ((channelctr :initform nil :initarg :channelctr :accessor channelctr)
   (channelText :initform nil :accessor channelText :type t)
   (channelBox :initform nil :accessor channelBox :type t)
   (programMenu :initform nil :accessor programMenu :type t)
   (volumeText :initform nil :accessor volumeText :type t)
   (volumeVal :initform nil :accessor volumeVal :type t)
   (volumpeSlider :initform nil :accessor volumeSlider :type t)
   (pitchText :initform nil :accessor pitchText :type t)
   (pitchVal :initform nil :accessor pitchVal :type t)
   (pitchSlider :initform nil :accessor pitchSlider :type t)
   (panText :initform nil :accessor panText :type t)
   (panVal :initform nil :accessor panVal :type t)
   (panSlider :initform nil :accessor panSlider :type t)
   (ctrl1menu :initform nil :accessor ctrl1menu :type t)
   (ctrl1Val :initform nil :accessor ctrl1Val :type t)
   (ctrl1Slider :initform nil :accessor ctrl1Slider :type t)
   (ctrl2menu :initform nil :accessor ctrl2menu :type t)
   (ctrl2Val :initform nil :accessor ctrl2Val :type t)
   (ctrl2Slider :initform nil :accessor ctrl2Slider :type t)
   (resetbutton :initform nil :accessor resetbutton :type t)
))

(defclass channelpanelview (channelpanel om-view) ())

(defmethod editor ((self channelpanel)) 
  (editor (om-view-container self))) 

(defmethod update-subviews ((Self channelpanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self channelPanel))
   (call-next-method))


(defmethod get-object ((Self channelPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self channelPanel))
  (report-modifications (om-view-container self)))

(defmethod get-channelpanel-class ((self controllerpanel)) 'channelpanelview)

(defvar *channel-w* 140)
(setf *channel-w* 80)

;=======================
;=== INITIALIZATIONS ===
;=======================

(defmethod metaobj-scrollbars-params ((self ConsoleEditor))  '(:h t))

(defmethod make-preset-view ((self ConsoleEditor)) nil)

(defmethod initialize-instance :after ((self ConsoleEditor) &rest l)
   (declare (ignore l))
   
   (setf (panel self) (om-make-view (get-panel-class self) 
                                                     :owner self
                                                     :position (om-make-point 0 0) 
                                                     :bg-color (om-make-color 0.7 0.5 0.5)
                                                     :scrollbars (first (metaobj-scrollbars-params self))
                                                     :retain-scrollbars (second (metaobj-scrollbars-params self))
                                                     :field-size  (om-make-point (* *channel-w* (nbtracks (object self))) 540)
                                                     ;;;:size (om-make-point (w self) (- (h self) 15)))
                                                     :size (om-make-point (w self) (h self)))
      )
   
   (setf (presets-view self) (make-preset-view self))
   
   
   (setf (ch-panels self) 
      (loop for chctrl in (channels-ctrl (object self))
             for i = 0 then (+ i 1) collect
                (om-make-view (get-channelpanel-class (panel self))
                 :channelctr chctrl
                 :bg-color *om-light-gray-color*
                 :position (om-make-point (+ (delta-tracks self) (* i *channel-w*)) (delta-tracks self)) 
                 :size (om-make-point (- *channel-w* (* 2 (delta-tracks self))) 
                                      (- (h self) 
                                         (if (presets-view self) (+ (h (presets-view self)) (delta-tracks self)) 0)
                                         (* 2 (delta-tracks self)))))))
   
   (apply 'om-add-subviews (cons (panel self) 
                                 (ch-panels self)))
   (when (presets-view self) (om-add-subviews self (presets-view self)))


)





(defmethod update-editor-after-eval ((self ConsoleEditor) val)
  (setf (object self) val)
  (om-set-view-size (window self) (get-win-ed-size (object self)))
  (om-set-view-size self (get-win-ed-size (object self)))
  (om-set-field-size (panel self) (om-make-point (* *channel-w* (nbtracks (object self))) 540))
  (loop for chpan in (ch-panels self) do 
        (om-remove-subviews (panel self) chpan))
  (setf (ch-panels self) 
      (loop for chctrl in (channels-ctrl (object self))
             for i = 0 then (+ i 1) collect
                (om-make-view (get-channelpanel-class (panel self))
                                             :channelctr chctrl
                                             :owner (panel self)
                                             :bg-color *om-light-gray-color*
                                             :position (om-make-point (* i *channel-w*) 0) 
                                             :size (om-make-point *channel-w* (- (h self) 15)))))
  
)


(defmethod simple-controls-only ((self channelpanel)) nil)


(defmethod initialize-instance :after ((self channelPanel) &rest l)
  (declare (ignore l))
  (do-initialize-channel self))

(defmethod make-channel-title-items ((self channelpanel))
  (setf (channelText self) (om-make-dialog-item 'om-static-text
                                                        (om-make-point 8 5) 
                                                        (om-make-point 76 20) "CHANNEL" 
                                                        :font *om-default-font2b*))

  (setf (channelBox self) (om-make-dialog-item 'numBox (om-make-point 26 25) (om-make-point 28 18) 
                                                       (format nil " ~D" (midichannel (channelctr self)))
                                                       :min-val 1
                                                       :max-val 16
                                                       :value (midichannel (channelctr self))
                                                       :afterfun #'(lambda (item)
                                                                     (change-channel self (value item)))
                                                       :font *om-default-font2b*
                                                       ))
    
  (om-set-bg-color (channelBox self) *om-white-color*)
  (list  (channelText self) (channelBox self))
  )


(defmethod do-initialize-channel ((self channelPanel))  
  (let* ((progList *midi-programs*)
         (ctrlList *midi-controllers*) bar1 bar2 bar3 bar4
         (pos 0)
         (bgcolor *om-light-gray-color*)
         ctrlchgText
         (title-items (make-channel-title-items self)))
    
    (setf pos (+ pos 55))
    (setf (programMenu self) 
          (om-make-dialog-item 'om-pop-up-dialog-item 
                               (om-make-point 2 pos) 
                               (om-make-point 76 12)
                               ""
                               :di-action (om-dialog-item-act item
                                            (change-program self (second (nth (om-get-selected-item-index item) progList))))
                               :font *om-default-font1*
                               :range (loop for item in progList collect (first item))
                               :value (first (nth (program (channelctr self)) progList))
                               )
          )
    
    (setf pos (+ pos 30))
    (setf (panText self) (om-make-dialog-item 'om-static-text
                                              (om-make-point 16 pos) 
                                              (om-make-point 40 16)
                                              "Pan"
                                              :font *om-default-font2*
                                              ))
    
    (setf (panVal self) (om-make-dialog-item 'om-static-text
                                             (om-make-point 45 pos) 
                                             (om-make-point 30 16)
                                             (pan2str (pan-ctrl (channelctr self)))
                                             :font *om-default-font2*
                                             ))
    
    (setf pos (+ pos 20))
    (setf (panSlider self) 
          (om-make-view 'graphic-numbox :position (om-make-point 30 pos) 
                        :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                        :pict (om-load-and-store-picture "dial" 'di)
                        :nbpict 65
                        :pict-size (om-make-point 24 24)
                        :di-action (om-dialog-item-act item
                                     (change-pan self (value item)))
                        :font *om-default-font2*
                        :value (pan-ctrl (channelctr self))
                        :min-val 0
                        :max-val 127))
    
    (setf pos (+ pos 26))
    (setf bar1 (om-make-view 'bar-item 
                             :position (om-make-point 4 pos) 
                             :size (om-make-point 72 10)
                             :bg-color bgcolor)) 
    
    (setf pos (+ pos 5))
    (setf (volumeText self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 16 pos) 
                                                 (om-make-point 40 16)
                                                 "Vol"
                                                 :font *om-default-font2*
                                                 ))
    
    (setf (volumeVal self) (om-make-dialog-item 'om-static-text 
                                                (om-make-point 45 pos) 
                                                (om-make-point 30 16)
                                                (format nil "~D" (vol-ctrl (channelctr self)) )
                                                :font *om-default-font2*
                                                ))
    
    (setf pos (+ pos 20))
    (setf (volumeSlider self) (om-make-dialog-item 'om-slider  
                                                   (om-make-point 30 pos) 
                                                   (om-make-point 20 100) ""
                                                   :di-action (om-dialog-item-act item
                                                                
                                                                (change-volume self (om-slider-value item)))
                                                   :increment 1
                                                   :range '(0 127)
                                                   :value (vol-ctrl (channelctr self))
                                                   :direction :vertical
                                                   :tick-side :none
                                                   ))
    
    
    
    (unless (simple-controls-only self)

      (setf pos (+ pos 110))
      (setf bar2 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 72 10)
                               :bg-color bgcolor
                               ))
      
      (setf pos (+ pos 4))
      
      (setf (pitchText self) (om-make-dialog-item 'om-static-text
                                                  (om-make-point 5 pos) 
                                                  (om-make-point 40 16)
                                                  "Pitch"
                                                  :font *om-default-font2*
                                                  ))
      
      (setf (pitchVal self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 40 pos) 
                                                 (om-make-point 40 16)
                                                 (format nil "~D" (pitchwheel-to-mc (pitch-ctrl (channelctr self))))
                                                 :font *om-default-font2*
                                                 ))
      
      (setf pos (+ pos 26))
      (setf (pitchSlider self) (om-make-dialog-item 'om-slider  
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 20)
                                                    ""
                                                    :di-action (om-dialog-item-act item
                                                                 (change-pitchbend self (om-slider-value item)))
                                                    :increment 1
                                                    :range '(0 16383)
                                                    :value (pitch-ctrl (channelctr self))
                                                    :direction :horizontal
                                                    :tick-side :none
                                                    ))
      
      (setf pos (+ pos 40))
      (setf bar3 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 72 10)
                               :bg-color bgcolor
                               ))
      
      (setf pos (+ pos 8))
      
      (setf ctrlchgText (om-make-dialog-item 'om-static-text
                                             (om-make-point 6 pos) 
                                             (om-make-point 70 20)
                                             "CtrlChange"
                                             :font *om-default-font2*
                                             ))
      
      (setf pos (+ pos 22))
      
      (setf (ctrl1Menu self) (om-make-dialog-item 'om-pop-up-dialog-item 
                                                  (om-make-point 5 pos) 
                                                  (om-make-point 70 12) 
                                                  ""
                                                  :di-action (om-dialog-item-act item
                                                               (change-ctrl1-num self (second (nth (om-get-selected-item-index item) ctrlList))))
                                                  :font *om-default-font1*
                                                  :range (loop for item in ctrlList collect (first item))
                                                  :value (first (nth (control1-num (channelctr self)) ctrlList))
                                                  ))
      
      (setf pos (+ pos 22))
      (setf (ctrl1Slider self) (om-make-dialog-item 'om-slider  
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 20) 
                                                    ""
                                                    :di-action (om-dialog-item-act item
                                                                 (change-ctrl1-val self (om-slider-value item)))
                                                    :increment 1
                                                    :range '(0 127)
                                                    :value (control1-val (channelctr self))
                                                    :direction :horizontal
                                                    :tick-side :none
                                                    ))
      
      (setf pos (+ pos 18))
      (setf (ctrl1Val self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 10 pos) 
                                                 (om-make-point 30 16)
                                                 (format nil "~D" (control1-val (channelctr self)))
                                                 :font *om-default-font2*
                                                 ))
      
      (setf pos (+ pos 26))
      (setf (ctrl2Menu self) (om-make-dialog-item 'om-pop-up-dialog-item 
                                                  (om-make-point 5 pos) 
                                                  (om-make-point 70 12)
                                                  ""
                                                  :di-action (om-dialog-item-act item
                                                               (change-ctrl2-num self (second (nth (om-get-selected-item-index item) ctrlList))))
                                                  :font *om-default-font1*
                                                  :range (loop for item in ctrlList collect (first item))
                                                  :value (first (nth (control2-num (channelctr self)) ctrlList))
                                                  ))
      
      (setf pos (+ pos 22))
      (setf (ctrl2Slider self) (om-make-dialog-item 'om-slider  
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 20)
                                                    ""
                                                    :di-action (om-dialog-item-act item
                                                                 (change-ctrl2-val self (om-slider-value item)))
                                                    :increment 1
                                                    :range '(0 127)
                                                    :value (control2-val (channelctr self))
                                                    :direction :horizontal
                                                    :tick-side :none
                                                    ))
      
      (setf pos (+ pos 18))
      (setf (ctrl2Val self) (om-make-dialog-item 'om-static-text 
                                                 (om-make-point 10 pos) 
                                                 (om-make-point 30 16)
                                                 (format nil "~D" (control2-val (channelctr self)))
                                                 :font *om-default-font2*
                                                 ))
      
      (setf pos (+ pos 22))
      (setf bar4 (om-make-view 'bar-item 
                               :position (om-make-point 4 pos) 
                               :size (om-make-point 72 10)
                               :bg-color bgcolor
                               ))

      (setf pos (+ pos 6))
    
    
      (setf (resetbutton self) (om-make-dialog-item 'om-button
                                                    (om-make-point 5 pos) 
                                                    (om-make-point 70 16)
                                                    "Reset"
                                                    :di-action (om-dialog-item-act item
                                                                 (reset-all-values self))
                                                    :font *controls-font*)  
            
            )   ;;; fin unless simple-controls-only
      
      
      )
    
    (apply 'om-add-subviews self title-items)
    
    (om-add-subviews self  
                     (programMenu self) 
                     bar1 
                     (volumeSlider self) 
                     (volumeText self) 
                     (volumeVal self)
                     (panSlider self) 
                     (panText self) 
                     (panVal self)
                     )
    
    (unless (simple-controls-only self)
      (om-add-subviews self bar2 bar3 bar4
                       (pitchSlider self) 
                       (pitchText self) (pitchVal self)
                       (ctrl1Slider self) 
                       (ctrl1Menu self) 
                       (ctrl1Val self)
                       (ctrl2Slider self) 
                       (ctrl2Menu self) 
                       (ctrl2Val self)
                       ctrlchgText
                       (resetbutton self)
                       ))
    
    ))


(defmethod set-channel-values ((self channelPanel)
                               &key program vol pan pitch ctr1 ctr1val ctr2 ctr2val)
  (when program
    (om-set-selected-item-index (programMenu self) program))
  (when vol
     (om-set-slider-value (volumeSlider self) vol)
     (om-set-dialog-item-text (volumeVal self) (number-to-string vol)))
  (when pan
     (set-value (panSlider self) pan)
     (om-set-dialog-item-text (panVal self) (pan2str pan)))
  (when pitch
    (om-set-slider-value (pitchSlider self) pitch)
    (om-set-dialog-item-text (pitchVal self) (number-to-string (pitchwheel-to-mc pitch))))
  (when ctr1
    (om-set-selected-item-index (ctrl1Menu self) ctr1))
  (when ctr1val
    (om-set-slider-value (ctrl1Slider self) ctr1val)
    (om-set-dialog-item-text (ctrl1Val self) (number-to-string ctr1val))
    )
  (when ctr2
    (om-set-selected-item-index (ctrl2Menu self) ctr2))
  (when ctr2val
    (om-set-slider-value (ctrl2Slider self) ctr2val)
    (om-set-dialog-item-text (ctrl2Val self) (number-to-string ctr2val))
    )
  )




;==============================
;=== ACTIONS ON CHANNEL PANELS:
;==============================

(defmethod change-channel ((self channelPanel) value)
  (setf (midichannel (channelctr self)) value)
  (report-modifications self))

(defmethod change-program ((self channelPanel) value)
  (setf (program (channelctr self)) value)
  (when (send-rt (editor self))
    (channel-send-prog (channelctr self)))
  (report-modifications self))

(defmethod change-volume ((self channelPanel) value)
  (setf (vol-ctrl (channelctr self)) value)
  (when (send-rt (editor self))
    (channel-send-vol (channelctr self)))
  (let ((new-str (integer-to-string value))
        (target (volumeVal self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)
      ))
  (report-modifications self))

(defun pan2str (panvalue)
  (let* ((value (- panvalue 64))
         (new-str (cond ((= value 0) (integer-to-string value))
                        ((< value 0) (format nil "L~D" (- value)))
                        ((> value 0) (format nil "R~D" value)))))
    new-str))

(defmethod change-pan ((self channelPanel) value)
  (setf (pan-ctrl (channelctr self)) value)
  (when (send-rt (editor self))
    (channel-send-pan (channelctr self)))
  (let* ((target (panVal self))
         (new-str (pan2str value)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))

(defmethod change-ctrl1-val ((self channelPanel) value)
  (setf (control1-val (channelctr self)) value)
  (when (send-rt (editor self)) 
    (channel-send-ct1 (channelctr self)))
  (let ((new-str (integer-to-string value))
        (target (ctrl1Val self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))

(defmethod change-ctrl1-num ((self channelPanel) value)
  (setf (control1-num (channelctr self)) value)
  (report-modifications self))

(defmethod change-ctrl2-val ((self channelPanel) value)
  (setf (control2-val (channelctr self)) value)
  (when (send-rt (editor self)) 
    (channel-send-ct2 (channelctr self)))
  (let ((new-str (integer-to-string value))
        (target (ctrl2Val self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))

(defmethod change-ctrl2-num ((self channelPanel) value)
  (setf (control2-num (channelctr self)) value)
  (report-modifications self))

(defmethod change-pitchbend ((self channelPanel) value)
  (setf (pitch-ctrl (channelctr self)) value)
  (when (send-rt (editor self)) 
    (channel-send-pitch (channelctr self)))
  (let ((new-str (integer-to-string (pitchwheel-to-mc value)))
        (target (pitchVal self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))


(defmethod reset-all-values ((self channelPanel))
  (change-program self 0)
  (om-set-selected-item-index (programMenu self) 0)
  (change-pan self 64)
  (set-value (panSlider self) 64)
  (change-volume self 100)
  (om-set-slider-value (volumeSlider self) 100)
  (change-pitchbend self 8192)
  (om-set-slider-value (pitchSlider self) 8192)
  (change-ctrl1-val self 0)
  (om-set-slider-value (ctrl1Slider self) 0)
  (change-ctrl2-val self 0)
  (om-set-slider-value (ctrl2Slider self) 0)
)
  
 
;;;;;====================
;;; SIMPLE MIDI MIXER
;;;;;====================

(defclass* simple-midi-mix-console (midi-mix-console) ()
  (:icon 918))

(defmethod get-channel-ctrl-class ((self simple-midi-mix-console)) 'simple-channel-ctrl)

(defclass* simple-Channel-Ctrl (channel-ctrl) ())

(defmethod send-midi-settings ((self simple-channel-ctrl))
  (channel-send-prog self) 
  (channel-send-vol self) 
  (channel-send-pan self))

(defmethod! get-midievents ((self simple-channel-ctrl) &optional test)
  (declare (ignore test))
  (let ((evtList nil) 
        (progevt (make-instance 'MidiEvent
                       :ev-date 0
                       :ev-type :ProgChange
                       :ev-chan (midichannel self)
                       :ev-port (midiport self)
                       :ev-fields (program self)))
        
        (volevt (make-instance 'MidiEvent
                       :ev-date 0
                       :ev-type :CtrlChange
                       :ev-chan (midichannel self)
                       :ev-port (midiport self)
                       :ev-fields (list 7 (vol-ctrl self))))
        
        (panevt (make-instance 'MidiEvent
                       :ev-date 0
                       :ev-type :CtrlChange
                       :ev-chan (midichannel self)
                       :ev-port (midiport self)
                       :ev-fields (list 10 (pan-ctrl self))))
        
        )
        
    (setf evtlist (list progevt volevt panevt))

    evtList))

;;;=== EDITOR ===

(defmethod get-win-ed-size ((self simple-midi-mix-console)) (om-make-point (+ 6 (min (* *channel-w* 6) (* 140 (nbtracks self)))) 340))

(defmethod make-editor-window ((class (eql 'simpleConsoleEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
    win))

(defclass simpleConsoleEditor (ConsoleEditor) ())
(defclass simplecontrollerPanel (controllerPanel) ())
(defclass simplechannelpanel (channelpanel om-item-view) ())

(defmethod get-editor-class ((self simple-midi-mix-console)) 'simpleConsoleEditor)
(defmethod get-panel-class ((Self simpleConsoleEditor)) 'simplecontrollerPanel)
(defmethod get-channelpanel-class ((self simplecontrollerPanel)) 'simplechannelpanel)


(defmethod simple-controls-only ((self simplechannelpanel)) t)

(defmethod reset-all-values ((self simplechannelPanel))
  (change-pan self 64)
  (set-value (panSlider self) 64)
  (change-volume self 100)
  (om-set-slider-value (volumeSlider self) 100))


