(in-package :om)

;===========================
;====== MIDICONTROL ========
;===========================

;=== This class represents a midi events sequence for continuous controllers
;=== A continuous controller sets a particular parameter: single type, single channel, single track, single port
;=== Only dates and values are variables (bpf editor)
;===
;=== ctrltype is a string name for the controller
;=== with this name, we can determine :
;=== - ev-num the type of midi evnts : control change (principally), pitchbend,...
;=== - ctr-num the control number in case we are dealing with control changes 
;===   (also used to determine in pitchbend is used with low (7 bits) or high (14bits) definition)
(defclass! MidiControl (sequence* bpf-controller)
  ((ctrltype :initform "ChannelVolume" :accessor ctrltype :initarg :ctrltype :type t :documentation "type of event (string)")
   (ev-num :initform "CtrlChange" :accessor ev-num :type t) 
   (ctr-num :initform 7 :accessor ctr-num :type t)
   (ref :initform 0 :accessor ref :initarg :ref :type t :documentation "track number")
   (port :initform 0 :accessor port :initarg :port :type t :documentation "output port number")
   (chan :initform 1 :accessor chan :initarg :chan :type t :documentation "MIDI channel (1-16)"))
  (:icon 903)
  (:documentation "
MIDICONTROL is a special BPF controlling the variation of a MIDI continuous controller. 

The x and y coordinates correspond to the slots <ldates> and <lvalues>.

MIDIControl can be 'played' as a musical object (for instance in a maquette) on a given MIDI channel/track and output port.
"
   ))

(defmethod Ldates ((self MidiControl)) (x-points self))
(defmethod Lvalues ((self MidiControl)) (y-points self))

(defmethod midicontrol-p ((self MidiControl))  t)
(defmethod midicontrol-p ((self t)) nil)

(defmethod initialize-instance :after ((self midicontrol) &rest args)
  (setf (player-fun self) #'(lambda (val) (midicontrol-play self val)))
  self)
 

(defmethod make-one-instance ((self midicontrol) &rest slots-vals)
  (let ((rep (apply 'simple-bpf-from-list (list (second slots-vals) (sixth slots-vals) (type-of self) 0))))
    (setf (ctrltype rep) (first slots-vals)
          (ref rep) (third slots-vals)
          (port rep) (fourth slots-vals)
          (chan rep) (fifth slots-vals)
          (ev-num rep) (name2evNum (ctrltype rep))
          (ctr-num rep) (name2ctrNum (ctrltype rep)))
    rep))

(defmethod execption-save-p ((self midicontrol)) 'midicontrol)

(defmethod save-exepcion ((self midicontrol))
  `(when (find-class ',(type-of self) nil)
     (let ((newctrl (simple-bpf-from-list ',(x-points self) ',(y-points self) ',(type-of self) ,(decimals self))))
       (setf (bpfcolor newctrl) ,(om-save-color (bpfcolor self)))
       (setf (ctrltype newctrl) ',(ctrltype self)
         (ref newctrl) ,(ref self)
         (port newctrl) ,(port self)
         (chan newctrl) ',(chan self)
         (ev-num newctrl) ,(name2evNum (ctrltype self))
         (ctr-num newctrl) ,(name2ctrNum (ctrltype self)))
       newctrl)))


(defmethod get-slot-in-out-names ((self MidiControl))
  (values '("self" "ctrltype" "Ldates" "ref" "port" "chan" "Lvalues")
          '(nil "ChannelVolume" nil 0 0 1 nil)
          '("object" "type of event (string)" "event dates (ms)" "track number" "output port number" "MIDI channel (1-16)" "value list")
          '(nil (( 1 (("-- Tempo (bpm)" "Tempo")
                      ("-- KeyPress" "KeyPress")
                      ("-- ChanPress" "ChanPress")
                      ("-- PitchBend (-64 to 63)" "PitchBend")
                      ("-- PitchBend Fine (-8192 to 8191)" "PitchWheel")
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
                      ("69 Hold 2 (on/off)" "Hold2")
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
                      )
                   )) nil nil nil nil nil)))


(defmethod allowed-in-maq-p ((self midicontrol))  t)

(defmethod get-obj-dur ((self midicontrol))
   (loop for item in (Ldates self)
         maximize item))

;=== Converts control name to midishare event number
(defun name2evNum (name)
  (let (num)
    (if (numberp name) (setf num 4)        ; consider it's a control change controller
        (cond
         ((string-equal name "Tempo") (setf num (om-midi-get-num-from-type "Tempo")))
         ((string-equal name "KeyPress") (setf num (om-midi-get-num-from-type "KeyPress")))
         ((string-equal name "ChanPress") (setf num (om-midi-get-num-from-type "ChanPress")))
         ((string-equal name "Private") (setf num (om-midi-get-num-from-type "Private")))
         ((string-equal name "PitchBend") (setf num (om-midi-get-num-from-type "PitchBend")))
         ((string-equal name "PitchWheel") (setf num (om-midi-get-num-from-type "PitchWheel")))     
         ;Ctrl Changes :
         (t (setf num (om-midi-get-num-from-type "CtrlChange")))))
    num))


;=== determine control change number with the control name
(defun name2ctrNum (name)
  (let (num)
    (if (numberp name) (setf num name)
        (cond
         ((string-equal name "Tempo") (setf num nil))
         ((string-equal name "KeyPress") (setf num nil))
         ((string-equal name "ChanPress") (setf num nil))
         ((string-equal name "Private") (setf num nil))
         ((string-equal name "PitchBend") (setf num 7))
         ((string-equal name "PitchWheel") (setf num 14))     
         ;Ctrl Changes :
         (t (setf num (str2ctrlNum name)))))
    num))




; next method
;(defmethod real-duration ((self midicontrol) time)
;  (values (get-obj-dur self) (+ time (get-obj-dur self))))

(defmethod strech ((self MidiControl) (num integer) (denom integer) &optional parent )
  (call-next-method))

(defmethod update-miniview ((self t) (value MidiControl))
   (om-invalidate-view self t))

(defmethod draw-mini-view  ((self t) (value MidiControl)) 
  (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))


;=== Creates a BPF beginning from 0 
;=== used to draw continuous controllers if first event occurs later than 0
(defmethod* create-bpf-with-initpt ((self MidiControl))
  (let ((dates (append (list 0 (first (Ldates self))) (Ldates self))) (vals (append (list 0 0) (Lvalues self))))
    (simple-bpf-from-list dates vals)))

(defmethod draw-obj-in-rect ((self MidiControl) x x1 y y1 edparams view)
  (let ((tmpBPF (create-bpf-with-initpt self)))
    (om-with-focused-view view
      (om-with-font *om-default-font1*
      (if (stringp (ctrltype self)) (om-draw-string 10 10 (string (ctrltype self))))
      (om-draw-rect 0 0 (- (w view) 1) (- (h view) 1))
    (draw-obj-in-rect tmpBPF x x1 y y1 (give-bpf-range tmpBPF) view))
    )))


;=== Conversion to BPF (!!: bpf is midicontrol superclass)
(defmethod* objFromObjs ((self MidiControl) (type bpf))
  (let ((dates (Ldates self)) (vals (Lvalues self)))
    (simple-bpf-from-list dates vals)))

(defmethod* objFromObjs ((self MidiControl) (type MidiControl))
  (clone self))
 
;=== Creates a new MidiControl with a constant fixed sample rate
(defmethod! om-sample ((self MidiControl) sample-rate &optional xmin xmax dec)
  :icon 910
  :numouts 3
  (let* ((rep (multiple-value-list (call-next-method)))
         (new-ctrl (car rep)))
    (setf (ctrltype new-ctrl) (ctrltype self))
    (setf (ev-num new-ctrl)(ev-num self))
    (setf (ctr-num new-ctrl)(ctr-num self))
    (setf (ref new-ctrl) (ref self))
    (setf (port new-ctrl) (port self))
    (setf (chan new-ctrl) (chan self))  
    (values new-ctrl (cadr rep) (caddr rep))))


(defmethod get-fields ((self MidiControl) value)
  (case (ev-num self)
    (4 (if (lsb-controller (ctr-num self))
           (values (list (- (ctr-num self) 32) (msb value))
                   (list (ctr-num self) (lsb value)))
         (values (list (ctr-num self) value) nil)))
    (7 (if (= (ctr-num self) 14) 
           (list (lsb (+ value 8192)) (msb (+ value 8192))))
       (list 0 (+ value 64)))
    (3 (list 0 value))
    (otherwise value)))


;=== Creates a list of MidiEvents 
(defmethod! get-midievents ((self MidiControl) &optional test)
  :icon 902
  (let ((evtList nil) evtMSB evtLSB)
    (loop for date in (Ldates self)
        for value in (Lvalues self) do
        (multiple-value-bind (fields fieldslsb) 
            (get-fields self value)
          
        ;send events for each channel, port, ref...
        (loop for po in (if (port self) (list! (port self)) (list *outmidiport*)) do
              (loop for ch in (if (chan self) (list! (chan self)) (list 1)) do
                    (loop for re in (if (ref self) (list! (ref self)) (list (if (= (ev-num self) (om-midi-get-num-from-type "Tempo")) 0 1))) do
              
                          (setf evtMSB (make-instance 'MidiEvent
                                         :ev-date date
                                         :ev-type (ev-num self)
                                         :ev-chan ch
                                         :ev-ref re
                                         :ev-port po
                                         :ev-fields fields
                                         ))
                          
                          (if (and (= (ev-num self) (om-midi-get-num-from-type "CtrlChange")) fieldslsb (not (= 0 (second fieldslsb)))) 
                            (setf evtLSB (make-instance 'MidiEvent
                                           :ev-date date
                                           :ev-type (ev-num self)
                                           :ev-chan ch
                                           :ev-ref re
                                           :ev-port po
                                           :ev-fields fieldslsb
                                           ))
                            (setf evtLSB nil))
                          
                          (if (or (not test) (funcall test evtMSB))
                            (push evtMSB evtList))
                          (if (and evtLSB (or (not test) (funcall test evtLSB)))
                            (push evtLSB evtList))
                          )))
        ))
    (reverse evtList)))



(defun midicontrol-play (self val)
  (multiple-value-bind (fields fieldslsb) 
            (get-fields self val)
    
    (loop for po in (if (port self) (list! (port self)) (list *outmidiport*)) do
          (loop for ch in (if (chan self) (list! (chan self)) (list 1)) do
                (loop for re in (if (ref self) (list! (ref self)) (list (if (= (ev-num self) (om-midi-get-num-from-type "Tempo")) 0 1))) do
                      (let ((event 
                             (midievent-to-msevent
                              (make-instance 'MidiEvent
                                                  :ev-date 0
                                                  :ev-type (ev-num self)
                                                  :ev-chan ch
                                                  :ev-ref re
                                                  :ev-port po
                                                  :ev-fields fields
                                                  )))
                            (event2 
                             (if (and (= (ev-num self) (om-midi-get-num-from-type "CtrlChange")) 
                                      fieldslsb 
                                      (not (= 0 (second fieldslsb))))
                                 (midievent-to-msevent 
                                  (make-instance 'MidiEvent
                                                :ev-date 0
                                                :ev-type (ev-num self)
                                                :ev-chan ch
                                                :ev-ref re
                                                :ev-port po
                                                :ev-fields fieldslsb)))))

                        (when event   
                          (om-midi-send-evt event *midiplayer*))
                        (when event2
                          (om-midi-send-evt event2 *midiplayer*))
                        ))))
    ))



;=======================================
;==== Extract continuous controllers ===
;==== from MidiFile or Midi-seq ========
;=======================================
(defmethod! get-continuous-ctrl ((self t) ctrlname ref port channel)
  :initvals '(nil nil nil 0 1)
  :indoc '("a MIDI file or sequence" "event type" "track number" "output port number" "MIDI channel (1-16)")
  :menuins '( (1 (("-- Tempo (bpm)" "Tempo")
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
                 )))
  :doc "
Extracts control events of type <ctrlname> (string) from a MIDI file or sequence at channel <channel>, track <ref> and port <port> anf returns a MIDICONTROL object.
"
  :icon 906
  (let (evtList 
        ;(evtList (get-midievents self))
        (new-ctrl (make-instance 'midicontrol))
        dates values eventtype control)
    
    (cond 
     ((string-equal ctrlname "Tempo") (setf eventtype (om-midi-get-num-from-type "Tempo")))
     ((string-equal ctrlname "KeyPress") (setf eventtype (om-midi-get-num-from-type "KeyPress")))
     ((string-equal ctrlname "ChanPress") (setf eventtype (om-midi-get-num-from-type "ChanPress")))
     ((string-equal ctrlname "PitchBend") (setf eventtype (om-midi-get-num-from-type "PitchBend"))
      (setf control 7))
     ((string-equal ctrlname "PitchWheel") (setf eventtype (om-midi-get-num-from-type "PitchWheel"))
      (setf control 14))
     ((string-equal ctrlname "Private") (setf eventtype (om-midi-get-num-from-type "Private")))
     (t (setf eventtype (om-midi-get-num-from-type "CtrlChange")) (setf control (str2CtrlNum ctrlname))))
    
    (setf evtList (get-midievents self #'(lambda (x) (and 
                                                      (test-port x port)
                                                      (test-ref x ref)
                                                      (test-channel x channel) 
                                                      (and (= (ev-type x) eventtype)
                                                           (if (= (ev-type x) 4)
                                                             (or (= control (first (ev-fields x)))
                                                                 (if (lsb-controller control)
                                                                   (= (- control 32) (first (ev-fields x)))
                                                                   nil))
                                                             t)
                                                           )
                                                      ))))
    
    (let ((last-date (- 1)) (curr-val 0))
      (loop for event in evtList do
            (cond 
             ((= eventtype (om-midi-get-num-from-type "Tempo"))
              (push (first (ev-fields event)) values))

           
             ((= eventtype (om-midi-get-num-from-type "PitchBend")) 
              (case control 
                (14 (setf curr-val (- (msb-lsb2value (second (ev-fields event)) (first (ev-fields event))) 8192)))
                (7 (setf curr-val (- (second (ev-fields event)) 64)))
                ))
             ((= eventtype (om-midi-get-num-from-type "KeyPress")) 
              (setf curr-val (second (ev-fields event))))
             ((= eventtype (om-midi-get-num-from-type "ChanPress"))
              (setf curr-val  (first (ev-fields event))))
             ((= eventtype (om-midi-get-num-from-type "Private"))
              (setf curr-val  (first (ev-fields event))))
             ((= eventtype (om-midi-get-num-from-type "CtrlChange"))
              (if (lsb-controller control)
                (setf curr-val (if (= (first (ev-fields event)) control)
                                 (msb-lsb2value (msb curr-val) (second (ev-fields event)))
                                 (msb-lsb2value (second (ev-fields event)) (lsb curr-val))))
                (setf curr-val (second (ev-fields event)))) 
             ))

            (if (= last-date (ev-date event))
              (setf (first values) curr-val)
              (progn
                (push (ev-date event) dates)
                (push curr-val values)))
            (setf last-date (ev-date event))))
      
    ;(setf new-ctrl (make-instance 'midicontrol))
    (setf new-ctrl (apply 'simple-bpf-from-list (list (reverse dates) (reverse values) 'midicontrol 0)))
    
    (setf (ctrltype new-ctrl) ctrlname)
    (setf (ev-num new-ctrl) eventtype)
    (setf (ctr-num new-ctrl) control)
    ;(setf (ref new-ctrl) (car (list! ref)))
    ;(setf (port new-ctrl) (car (list! port)))
    ;(setf (chan new-ctrl) (car (list! channel)))      
    (setf (ref new-ctrl) ref)
    (setf (port new-ctrl) port)
    (setf (chan new-ctrl) channel)      
    new-ctrl))



;;;======================================
;;; Editor
;;;======================================
(omg-defclass midi-bpfEditor (bpfcontroleditor) ())
(omg-defclass control-midibpf (control-bpf) 
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
     (setf (ev-num obj) (name2evNum (ctrltype obj)))
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
                                                  :font *om-default-font2*
                                                  )
                             
                             (om-make-dialog-item 'numBox
                                                  (om-make-point 190 2)
                                                  (om-make-point 24 18) (format nil "~D" (chan ctrlobject))
                                                  :min-val 1
                                                  :max-val 16
                                                  :font *om-default-font2*
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
                                                              :font *om-default-font2*)
                                       (om-make-dialog-item 'numBox
                                                            (om-make-point 190 22) 
                                                            (om-make-point 24 18)
                                                            (format nil "~D" (ref ctrlobject))
                                                            :min-val 0
                                                            :max-val 99
                                                            :font *om-default-font2*
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
                                                                                 :font *om-default-font2*)
                                       (om-make-dialog-item 'numBox
                                                                                 (om-make-point 260 2) 
                                                                                 (om-make-point 24 18)
                                                                                 (format nil "~D" (port ctrlobject))
                                                                                 :min-val 0
                                                                                 :max-val 99
                                                                                 :font *om-default-font2*
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


(add-player-for-object 'midicontrol '(:midishare :bpfplayer))

(defmethod editor-play/stop ((self midi-bpfEditor))
  (set-edit-param self 'player :bpfplayer)
  (call-next-method))


