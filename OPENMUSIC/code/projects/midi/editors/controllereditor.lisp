(in-package :om)

;;;======================================
;;; MIDICONTROL EDITOR
;;;======================================
(defclass midi-bpfEditor (bpfcontroleditor) ())


(defclass control-midibpf (control-bpf) 
   ((midi-params :accessor midi-params :initarg :midi-params :initform nil)))

(defmethod get-editor-class ((self midicontrol)) 'midi-bpfEditor)
(defmethod get-control-class ((Self midi-Bpfeditor)) 'control-midibpf)

(defvar *ctrl-type-list*
       '(("-- Tempo (bpm)" "Tempo")
         ("-- KeyPress" "KeyPress")
         ("-- ChanPress" "ChanPress")
         ("-- PitchBend (-64 63)" "PitchBend")
         ("-- PitchBend Fine (-8192 8191)" "PitchWheel")
         ("00 Bank Select" "BankSelect")
         ("01 Modulation Wheel" "ModulationWheel")
         ("02 Breath Controller" "BreathController")
         ("04 Foot Controller" "FootController")
         ("05 Portamento Time" "PortamentoTime")
         ("06 Data Entry MSB" "DataEntryMSB")
         ("07 Channel Volume" "ChannelVolume")
         ("08 Balance" "Balance")
         ("10 Pan" "Pan")
         ("11 Expression Controller" "ExpressionController")
         ("12 Effect Control 1" "EffectControl1")
         ("13 Effect Control 2" "EffectControl2")
         ("16 General Purpose Controller 1" "GeneralPurposeController1")
         ("17 General Purpose Controller 2" "GeneralPurposeController2")
         ("18 General Purpose Controller 3" "GeneralPurposeController3")
         ("19 General Purpose Controller 4" "GeneralPurposeController4")
         ("00/32 Bank Select Fine" "BankSelectFine")
         ("01/33 Modulation Wheel Fine" "ModulationWheelFine")
         ("02/34 Breath Controller Fine" "BreathControllerFine")
         ("04/36 Foot Controller Fine" "FootControllerFine")
         ("05/37 Portamento Time Fine" "PortamentoTimeFine")
         ("06/38 Data Entry MSB-LSB Fine" "DataEntryMSBLSB")
         ("07/39 Channel Volume Fine" "ChannelVolumeFine")
         ("08/40 Balance Fine" "BalanceFine")
         ("10/42 Pan Fine" "PanFine")
         ("11/43 Expression Controller Fine" "ExpressionControllerFine")
         ("12/44 Effect Control 1 Fine" "EffectControl1Fine")
         ("13/45 Effect Control 2 Fine" "EffectControl2Fine")
         ("16/48 General Purpose Controller 1 Fine" "GeneralPurposeController1Fine")
         ("17/49 General Purpose Controller 2 Fine" "GeneralPurposeController2Fine")
         ("18/50 General Purpose Controller 3 Fine" "GeneralPurposeController3Fine")
         ("19/51 General Purpose Controller 4 Fine" "GeneralPurposeController4Fine")
         ("64 Damper Pedal on/off (Sustain)" "DamperPedal")
         ("65 Portamento On/Off" "Portamento")
         ("66 Sustenuto On/Off" "Sustenuto")
         ("67 Soft Pedal On/Off" "SoftPedal")
         ("68 Legato Footswitch" "LegatoFootswitch")
         ("69 Hold 2" "Hold2")
         ("70 Sound Controller 1" "SoundController1")
         ("71 Sound Controller 2" "SoundController2")
         ("72 Sound Controller 3" "SoundController3")
         ("73 Sound Controller 4" "SoundController4")
         ("74 Sound Controller 5" "SoundController5")
         ("75 Sound Controller 6" "SoundController6")
         ("76 Sound Controller 7" "SoundController7")
         ("77 Sound Controller 8" "SoundController8")
         ("78 Sound Controller 9" "SoundController9")
         ("79 Sound Controller 10" "SoundController10")
         ("84 Portamento Control" "PortamentoControl")
         ("91 Effects 1 Depth" "Effects1Depth")
         ("92 Effects 2 Depth" "Effects2Depth")
         ("93 Effects 3 Depth" "Effects3Depth")
         ("94 Effects 4 Depth" "Effects4Depth")
         ("95 Effects 5 Depth" "Effects5Depth")
         ("96 Data Increment" "DataIncrement")
         ("-- Private" "Private")
         )) 
   
(defun change-control-type (container val)
   (let ((obj (object (editor container))))
     (setf (ctrltype obj) val)
     (setf (ev-type obj) (name2evtype (ctrltype obj)))
     (setf (ctr-num obj) (name2ctrNum (ctrltype obj)))
     (report-modifications (editor container)))
   t)

(defmethod make-midi-param-panel ((container control-midibpf) (ctrlobject midicontrol))
   (let* ((midipanel (om-make-view 'om-transparent-view
                                   :position (om-make-point (- (w container) 300) 0)
                                   :size (om-make-point 300 (h container))
                                   :owner container
                                   ))
            
            (ctrlbutton (om-make-dialog-item 'om-pop-up-dialog-item
                                             (om-make-point 0 10)
                                             (om-make-point 140 10) ""
                                      :font *om-default-font1*     
                                      :di-action (om-dialog-item-act item
                                                     (change-control-type (panel (om-view-container container)) 
                                                                          (cadr (nth (om-get-selected-item-index item) *ctrl-type-list*))))
                                      :range (mapcar 'car *ctrl-type-list*)
                                      :value (car (find (if (numberp (ctrltype ctrlobject)) 
                                                            (ctrlNum2str (ctrltype ctrlobject))
                                                          (ctrltype ctrlobject))
                                                        *ctrl-type-list* :key 'cadr :test 'string-equal))
                                      )))

            (om-add-subviews midipanel     
                             
                             ctrlbutton
                             
                             (om-make-dialog-item 'om-static-text  
                                                  (om-make-point 150 0)  
                                                  (om-make-point 40 18)
                                                  "Chan"
                                                  :font *om-default-font1*
                                                  )
                             
                             (om-make-dialog-item 'numBox
                                                  (om-make-point 190 2)
                                                  (om-make-point 24 18) (format nil "~D" (chan ctrlobject))
                                                  :min-val 1
                                                  :max-val 16
                                                  :font *om-default-font1*
                                                  :bg-color *om-white-color*
                                                  :value (or (chan ctrlobject) 1)
                                                  :afterfun #'(lambda (item)
                                                                (setf (chan ctrlobject) (value item))
                                                                (report-modifications (om-view-container container)))
                                                  )
                                         
                                         (om-make-dialog-item 'om-static-text 
                                                              (om-make-point 150 20) 
                                                              (om-make-point 40 18)
                                                              "Track"
                                                              :font *om-default-font1*)
                                       (om-make-dialog-item 'numBox
                                                            (om-make-point 190 22) 
                                                            (om-make-point 24 18)
                                                            (format nil "~D" (ref ctrlobject))
                                                            :min-val 0
                                                            :max-val 99
                                                            :font *om-default-font1*
                                                            :bg-color *om-white-color*
                                                            :value (or (ref ctrlobject) 0)
                                                            :afterfun #'(lambda (item)
                                                                          (setf (ref ctrlobject) (value item))
                                                                          (report-modifications (om-view-container container)))
                                                            )
                                       
                                       (om-make-dialog-item 'om-static-text 
                                                                                 (om-make-point 220 0) 
                                                                                 (om-make-point 40 18)
                                                                                 "Port"
                                                                                 :font *om-default-font1*)
                                       (om-make-dialog-item 'numBox
                                                                                 (om-make-point 260 2) 
                                                                                 (om-make-point 24 18)
                                                                                 (format nil "~D" (port ctrlobject))
                                                                                 :min-val 0
                                                                                 :max-val 99
                                                                                 :font *om-default-font1*
                                                                                 :bg-color *om-white-color*
                                                                                 :value (or (port ctrlobject) 0)
                                                                                 :afterfun #'(lambda (item)
                                                                                                       (setf (port ctrlobject) (value item))
                                                                                                       (report-modifications (om-view-container container)))
                                                                                 )
                                       
                                       )
     midipanel))



(defmethod initialize-instance :after ((Self midi-bpfEditor) &rest initargs) 
   (let* ((midipanel (make-midi-param-panel (control self) (object self))))
	(setf (midi-params (control self)) midipanel)
     ))


(defmethod om-set-view-size ((self control-midibpf) size)
   (call-next-method)
   (om-set-view-position (midi-params self) (om-make-point (- (om-point-h size) 300) 0)))

(defmethod update-editor-after-eval ((self midi-bpfEditor) val)
   (call-next-method)
   (om-remove-subviews (control self) (midi-params (control self)))
   (let ((newparams (make-midi-param-panel (control self) val)))
     (setf (midi-params (control self)) newparams)
     (om-add-subviews (control self) newparams)))