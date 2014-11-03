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
;=== - ev-type the type of midi evnts : control change (principally), pitchbend,...
;=== - ctr-num the control number in case we are dealing with control changes 
(defclass! MidiControl (sequence* bpf) ;-controller)
  ((ctrltype :initform "ChannelVolume" :accessor ctrltype :initarg :ctrltype :type t :documentation "type of event")
   (ev-type :initform :CtrlChange :accessor ev-type :type t) 
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

(add-player-for-object 'midicontrol '(:midi-player :midishare))

(defmethod default-point-list ((self MidiControl)) nil)
(defmethod get-initval ((self MidiControl))
  (make-instance (class-of self) :point-list nil))

(defmethod Ldates ((self MidiControl)) (x-points self))
(defmethod Lvalues ((self MidiControl)) (y-points self))

(defmethod midicontrol-p ((self MidiControl))  t)
(defmethod midicontrol-p ((self t)) nil)

(defun (setf ev-num) (x y))
 

(defmethod make-one-instance ((self midicontrol) &rest slots-vals)
  (let ((rep (apply 'simple-bpf-from-list (list (second slots-vals) (sixth slots-vals) (type-of self) 0))))
    (setf (ctrltype rep) (first slots-vals)
          (ref rep) (third slots-vals)
          (port rep) (fourth slots-vals)
          (chan rep) (fifth slots-vals)
          (ev-type rep) (name2evtype (ctrltype rep))
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
             (ev-type newctrl) ',(name2evtype (ctrltype self))
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
                      ("-- PitchWheel (-8192 to 8191)" "PitchWheel")
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

;=== Creates a BPF beginning from 0 
;=== used to draw continuous controllers if first event occurs later than 0
(defmethod* create-bpf-with-initpt ((self MidiControl))
  (let ((dates (if (or (null (Ldates self)) (= 0 (car (Ldates self))))
                  (Ldates self)
                 (append (list 0 (first (Ldates self))) (Ldates self))))
        (vals (if (or (null (Ldates self)) (= 0 (car (Ldates self))))
                  (Lvalues self)
                (append (list 0 0) (Lvalues self)))))
    (simple-bpf-from-list dates vals)))


;=== Conversion to BPF (!!: bpf is a midicontrol superclass)
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
    (setf (ev-type new-ctrl) (ev-type self))
    (setf (ctr-num new-ctrl) (ctr-num self))
    (setf (ref new-ctrl) (ref self))
    (setf (port new-ctrl) (port self))
    (setf (chan new-ctrl) (chan self))  
    (values new-ctrl (cadr rep) (caddr rep))))

;=== Converts control name to midishare event TYPE
(defun name2evType (name)    
    (cond
     ((numberp name) :CtrlChange) ; consider it's a control change controller
     ((string-equal name "Tempo") :Tempo)
     ((string-equal name "KeyPress") :KeyPress)
     ((string-equal name "ChanPress") :ChanPress)
     ((string-equal name "Private") :Private)
     ((string-equal name "PitchBend") :PitchBend)
     ((string-equal name "PitchWheel") :PitchBend)
     (t :CtrlChange)))


;=== determine control change number with the control name
(defun name2ctrNum (name)
  (cond
   ((find name '("Tempo" "KeyPress" "ChanPress" "Private") :test 'string-equal) nil)
   ((string-equal name "PitchBend") 7)
   ((string-equal name "PitchWheel") 14)  
   (t (str2ctrlNum name))
   ))


;======================
; BOX 
;======================

(defmethod update-miniview ((self t) (value MidiControl))
   (om-invalidate-view self t))

(defmethod draw-mini-view  ((self t) (value MidiControl)) 
  (draw-obj-in-rect value 0 (w self) 0  (h self) (view-get-ed-params self) self))

(defmethod draw-obj-in-rect ((self MidiControl) x x1 y y1 edparams view)
  (let ((tmpBPF (create-bpf-with-initpt self)))
    (om-with-focused-view view
      (om-with-font *om-default-font1*
      (if (stringp (ctrltype self)) (om-draw-string 10 10 (string (ctrltype self))))
      (om-draw-rect 0 0 (- (w view) 1) (- (h view) 1))
    (draw-obj-in-rect tmpBPF x x1 y y1 (give-bpf-range tmpBPF) view))
    )))



;======================
; TOOLS 
;======================

(defun out-of-7bits (fields)
  (or (< (car fields) 0) (> (car fields) 127)
      (and (cadr fields)
           (< (cadr fields) 0) (> (cadr fields) 127))))

(defmethod get-fields ((self MidiControl) value)
  (case (ev-type self)
    (:CtrlChange (if (lsb-controller (ctr-num self))
           (values (list (- (ctr-num self) 32) (msb value))
                   (list (ctr-num self) (lsb value)))
         (values (list (ctr-num self) value) nil)))
    (:PitchBend (if (= (ctr-num self) 14)
                    (val2lsbmsb (+ value 8192))
                  (list 0 (+ value 64))
                  ))
    (:KeyPress (list 0 value))
    (otherwise (list value))))


;=== Creates a list of MidiEvents 
(defmethod! get-midievents ((self MidiControl) &optional test)
  :icon 902
  (let ((evtList nil) evtMSB evtLSB)
    (loop for date in (Ldates self)
        for value in (Lvalues self) do
        (multiple-value-bind (fields fieldslsb) 
             (get-fields self value)
          (when (out-of-7bits fields)
            (om-beep-msg (format nil "WARNING: Wrong values in MIDI controller: ~A" fields)))
        ;send events for each channel, port, ref...
          (loop for po in (if (port self) (list! (port self)) (list *def-midi-out*)) do
                (loop for ch in (if (chan self) (list! (chan self)) (list 1)) do
                      (loop for re in (if (ref self) (list! (ref self)) (list (if (equal (ev-type self) :Tempo) 0 1))) do
              
                          (setf evtMSB (make-instance 'MidiEvent
                                         :ev-date date
                                         :ev-type (ev-type self)
                                         :ev-chan ch
                                         :ev-ref re
                                         :ev-port po
                                         :ev-fields fields
                                         ))
                          
                          (when (and (equal (ev-type self) :CtrlChange) fieldslsb (not (= 0 (second fieldslsb)))) 
                            (when (out-of-7bits fieldslsb)
                              (om-beep-msg (format nil "WARNING: Wrong values in MIDI controller: ~A" fieldslsb)))
                                      
                            (setf evtLSB (make-instance 'MidiEvent
                                           :ev-date date
                                           :ev-type (ev-type self)
                                           :ev-chan ch
                                           :ev-ref re
                                           :ev-port po
                                           :ev-fields fieldslsb
                                           ))
                            )
                          
                          (if (or (not test) (funcall test evtMSB))
                            (push evtMSB evtList))
                          (if (and evtLSB (or (not test) (funcall test evtLSB)))
                            (push evtLSB evtList))
                          )))
        ))
    (reverse evtList)))




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
                 ("-- PitchBend" "PitchBend")
                 ("00/32 Bank Select" "BankSelectFine")
                 ("01/33 Modulation Wheel" "ModulationWheelFine")
                 ("02/34 Breath Controller" "BreathControllerFine")
                 ("04/36 Foot Controller" "FootControllerFine")
                 ("05/37 Portamento Time" "PortamentoTimeFine")
                 ("06/38 Data Entry MSB-LSB" "DataEntryMSBLSB")
                 ("07/39 Channel Volume" "ChannelVolumeFine")
                 ("08/40 Balance" "BalanceFine")
                 ("10/42 Pan" "PanFine")
                 ("11/43 Expression Controller" "ExpressionControllerFine")
                 ("12/44 Effect Control 1" "EffectControl1Fine")
                 ("13/45 Effect Control 2" "EffectControl2Fine")
                 ("16/48 General Purpose Controller 1" "GeneralPurposeController1Fine")
                 ("17/49 General Purpose Controller 2" "GeneralPurposeController2Fine")
                 ("18/50 General Purpose Controller 3" "GeneralPurposeController3Fine")
                 ("19/51 General Purpose Controller 4" "GeneralPurposeController4Fine")
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
     ((string-equal ctrlname "Tempo") (setf eventtype :Tempo))
     ((string-equal ctrlname "KeyPress") (setf eventtype :KeyPress))
     ((string-equal ctrlname "ChanPress") (setf eventtype :ChanPress))
     ((string-equal ctrlname "PitchBend") (setf eventtype :PitchBend))
     ; (setf control 7))
     ;((string-equal ctrlname "PitchWheel") (setf eventtype :PitchBend)
     ; (setf control 14))
     ((string-equal ctrlname "Private") (setf eventtype :Private))
     (t (setf eventtype :CtrlChange) (setf control (str2ctrlNum ctrlname))))
    
    ;(print (list eventtype control))
    
    (setf evtList (get-midievents self #'(lambda (x) (and 
                                                      (test-port x port)
                                                      (test-ref x ref)
                                                      (test-channel x channel) 
                                                      (and (equal (ev-type x) eventtype)
                                                           (if (equal (ev-type x) :CtrlChange)
                                                             (or (= control (first (ev-fields x)))
                                                                 (if (lsb-controller control)
                                                                   (= (- control 32) (first (ev-fields x)))
                                                                   nil))
                                                             t)
                                                           )
                                                      ))))
    
    (when  evtList
      (let ((last-date -1) 
            (curr-val 0))
        (loop for event in evtList do
              (cond 
               ((equal eventtype :Tempo)
                (setf curr-val (first (ev-fields event))))
               
               ((equal eventtype :PitchBend) 
                (setf curr-val (- (msb-lsb2value (second (ev-fields event)) (first (ev-fields event))) 8192))
                ;(case control 
                ;  (14 (setf curr-val (- (msb-lsb2value (second (ev-fields event)) (first (ev-fields event))) 8192)))
                ;  (7 (setf curr-val (- (second (ev-fields event)) 64))))
                )
               ((equal eventtype :KeyPress) 
                (setf curr-val (second (ev-fields event))))
               ((equal eventtype :ChanPress)
                (setf curr-val  (first (ev-fields event))))
               ((equal eventtype :Private)
                (setf curr-val  (first (ev-fields event))))
               ((equal eventtype :CtrlChange)
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
              (setf last-date (ev-date event)))))
      
    (setf new-ctrl (apply 'simple-bpf-from-list (list (reverse dates) (reverse values) 'midicontrol 0)))
    (setf (ctrltype new-ctrl) ctrlname)
    (setf (ev-type new-ctrl) eventtype)
    (setf (ctr-num new-ctrl) control)
    (setf (ref new-ctrl) ref)
    (setf (port new-ctrl) port)
    (setf (chan new-ctrl) channel)      
    new-ctrl))


;;; INTERNAL: GET ALL CONTROLLERS FROM A SEQUENCE
(defmethod get-continuous-controllers ((self t))
  (let ((evtList (get-midievents self #'(lambda (x) (test-type x '(:CtrlChange :Tempo :KeyPress :ChanPress :PitchBend)))))
        (controllers nil))
    (loop for ev in evtlist do
          (let* ((ev-value (cond 
                           ((equal (ev-type ev) :Tempo)
                            (car (ev-fields ev)))
                           ((equal (ev-type ev) :PitchBend) 
                            (- (second (ev-fields ev)) 8192) ; 64)
                            )
                           ((equal (ev-type ev) :keypress) 
                            (second (ev-fields ev)))
                           ((equal (ev-type ev) :ChanPress) 
                            (second (ev-fields ev)))
                           ((equal (ev-type ev) :CtrlChange) 
                            (second (ev-fields ev)))))
                 (ev-title (if (equal (ev-type ev) :CtrlChange)
                               (list (ev-type ev) (car (ev-fields ev)))
                             (ev-type ev)))
                 (control-exists (find ev controllers :test #'(lambda (evt listitem)
                                                                (and (equal ev-title (car listitem))
                                                                     (= (ev-chan evt) (cadr listitem)))))))
            (if control-exists 
                (setf (caddr control-exists) ;;; add a time-value pair
                      (append (caddr control-exists)   
                              (list (list (ev-date ev) ev-value))))
              (push (list ev-title (ev-chan ev) (list (ev-date ev) ev-value))
                    controllers))
            ))
    
    controllers))


                                        
        