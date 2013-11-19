(in-package :om)



;======================
;=== MIDI Utilities ===
;======================

;=== tests if a controller num corresponds 
;=== to LSB value of another one
(defun lsb-controller (ctrlNum)
  (and (>= ctrlNum 32) (<= ctrlNum 63)))

;=== Converts msb lsb to a value
(defun Msb-Lsb2value (msb lsb)
  (+ lsb (* 128 msb))
)

;=== gets MSB from a 14bits value
(defun msb (value)
  (floor (/ value 128))
)

;=== gets LSB from a 14bits value
(defun lsb (value)
  (- value (* (msb value) 128))
)


;==============================
;==== CONVERSION FUNCTIONS ====
;==============================

;=== Converts a MS event number to string
(defun eventtype2str (evt)
  (case evt 
    (0 "Note")    
    (1 "KeyOn ")
    (2 "KeyOff")
    (3 "KeyPress")
    (4 "CtrlChange")
    (5 "ProgChange")
    (6 "ChanPress")
    (7 "PitchBend")
    (8 "SongPos")
    (9 "SongSel")
    (10 "Clock")
    (11 "Start")
    (12 "Continue")
    (13 "Stop")
    (14 "Tune")
    (15 "ActiveSens")
    (16 "Reset")
    (17 "SysEx")
    (18 "Stream")
    (19 "Private")
    (128 "Process")
    (129 "DProcess")
    (130 "QFrame")
    (131 "Ctrl14b")
    (132 "NonRegParam")
    (133 "RegParam")
    (134 "SeqNum")
    (135 "Textual")
    (136 "Copyright")
    (137 "SeqName")
    (138 "InstrName")
    (139 "Lyric")
    (140 "Marker")
    (141 "CuePoint")
    (142 "ChanPrefix")
    (143 "EndTrack")
    (144 "Tempo")
    (145 "SMPTEOffset")
    (146 "TimeSign")
    (147 "KeySign")
    (148 "Specific")
    (149 "PortPrefix")
    (150 "RcvAlarm")
    (151 "ApplAlarm")
    (152 "Reserved")
    (255 "dead")
    (otherwise "Unknown event")
))


;=== Tests if a ms event type is a textual type
(defun isTextual (typeNum)
  (member typeNum '(135 136 137 138 139 140 141)))

;=== Converts an integer (ascii codes) list into a string
(defun list2string (list)
  (let ((rep ""))
    (loop for item in list do
          (if (not (= 10 item))
          (setf rep (string+ rep (string (code-char item))))))
    rep))

;=== Converts an integer (ascii codes) list into a string
(defun string2list (str)
  (map 'list #'char-code str))

;=== Converts a list of strings into a single string
(defun strlist2string (list)
  (let ((rep ""))
    (loop for item in list do
          (setf rep (string+ rep (remove (code-char 10) item)))
          )
    rep))



;=== Creates a list of controlChange midi events values from a list of values
(defmethod! values2ctrlChange (values ctrlNum)
  :initvals '(nil 7)
  :indoc '("values list" "control number")
  :doc "Creates a list of ControlChange event parameters"
  (mapcar #'(lambda (x) 
              (list ctrlNum x)) values) 
)

;=== Converts a control number to string value
(defun str2ctrlNum (name)
  (cond 
    ((string-equal name "BankSelect") 0)
    ((string-equal name "ModulationWheel") 1)
    ((string-equal name "BreathController") 2)
    ((string-equal name "FootController") 4)
    ((string-equal name "PortamentoTime") 5)
    ((string-equal name "DataEntryMSB") 6)
    ((string-equal name "ChannelVolume") 7)
    ((string-equal name "Balance") 8)
    ((string-equal name "Pan") 10)
    ((string-equal name "ExpressionController") 11)
    ((string-equal name "EffectControl1") 12)
    ((string-equal name "EffectControl2") 13)
    ((string-equal name "GeneralPurposeController1") 16)
    ((string-equal name "GeneralPurposeController2") 17)
    ((string-equal name "GeneralPurposeController3") 18)
    ((string-equal name "GeneralPurposeController4") 19)
    ((string-equal name "BankSelectFine") 32)
    ((string-equal name "ModulationWheelFine") 33)
    ((string-equal name "BreathControllerFine") 34)
    ((string-equal name "FootControllerFine") 36)
    ((string-equal name "PortamentoTimeFine") 37)
    ((string-equal name "DataEntryMSBLSB") 38)
    ((string-equal name "ChannelVolumeFine") 39)
    ((string-equal name "BalanceFine") 40)
    ((string-equal name "PanFine") 42)
    ((string-equal name "ExpressionControllerFine") 43)
    ((string-equal name "EffectControl1Fine") 44)
    ((string-equal name "EffectControl2Fine") 45)
    ((string-equal name "GeneralPurposeController1Fine") 48)
    ((string-equal name "GeneralPurposeController2Fine") 49)
    ((string-equal name "GeneralPurposeController3Fine") 50)
    ((string-equal name "GeneralPurposeController4Fine") 51)
    ((string-equal name "DamperPedal") 64)
    ((string-equal name "Portamento") 65)
    ((string-equal name "Sustenuto") 66)
    ((string-equal name "SoftPedal") 67)
    ((string-equal name "LegatoFootswitch") 68)
    ((string-equal name "Hold2") 69)
    ((string-equal name "SoundController1") 70)
    ((string-equal name "SoundController2") 71)
    ((string-equal name "SoundController3") 72)
    ((string-equal name "SoundController4") 73)
    ((string-equal name "SoundController5") 74)
    ((string-equal name "SoundController6") 75)
    ((string-equal name "SoundController7") 76)
    ((string-equal name "SoundController8") 77)
    ((string-equal name "SoundController9") 78)
    ((string-equal name "SoundController10") 79)
    ((string-equal name "PortamentoControl") 84)
    ((string-equal name "Effects1Depth") 91)
    ((string-equal name "Effects2Depth") 92)
    ((string-equal name "Effects3Depth") 93)
    ((string-equal name "Effects4Depth") 94)
    ((string-equal name "Effects5Depth") 95)
    ((string-equal name "DataIncrement") 96)
    (t nil)
    )) 


;=== Converts a string to control number 
(defun ctrlNum2str (num)
  (case num
    (0 "BankSelect")
    (1"ModulationWheel") 
    (2 "BreathController") 
    (4 "FootController") 
    (5 "PortamentoTime") 
    (6 "DataEntryMSB")
    (7 "ChannelVolume")
    (8 "Balance") 
    (10 "Pan") 
    (11 "ExpressionController") 
    (12 "EffectControl1") 
    (13 "EffectControl2") 
    (16 "GeneralPurposeController1") 
    (17 "GeneralPurposeController2")
    (18 "GeneralPurposeController3") 
    (19 "GeneralPurposeController4") 
    (32 "BankSelectFine")
    (33 "ModulationWheelFine") 
    (34 "BreathControllerFine") 
    (36 "FootControllerFine") 
    (37 "PortamentoTimeFine") 
    (38 "DataEntryMSBLSB")
    (39 "ChannelVolumeFine") 
    (40 "BalanceFine") 
    (42 "PanFine") 
    (43 "ExpressionControllerFine")
    (44 "EffectControl1Fine") 
    (45 "EffectControl2Fine") 
    (48 "GeneralPurposeController1Fine") 
    (49 "GeneralPurposeController2Fine") 
    (50 "GeneralPurposeController3Fine") 
    (51 "GeneralPurposeController4Fine") 
    (64 "DamperPedal") 
    (65 "Portamento") 
    (66 "Sustenuto") 
    (67 "SoftPedal") 
    (68 "LegatoFootswitch") 
    (69 "Hold2") 
    (70 "SoundController1")
    (71 "SoundController2")
    (72 "SoundController3") 
    (73 "SoundController4") 
    (74 "SoundController5") 
    (75 "SoundController6") 
    (76 "SoundController7") 
    (77 "SoundController8") 
    (78 "SoundController9") 
    (79 "SoundController10") 
    (84 "PortamentoControl") 
    (91 "Effects1Depth") 
    (92 "Effects2Depth") 
    (93 "Effects3Depth") 
    (94 "Effects4Depth")
    (95 "Effects5Depth") 
    (96 "DataIncrement") 
    (t "")
    )) 




;================================
; MIDI - Send    functions
;================================


;==================SEND METHODS

(defmethod* midi-o ((bytes list) &optional port)
   :icon 912
   :indoc '("data bytes" "output port number")
   :initvals '((144 60 64) nil)
   :doc "Sends <bytes> out of the port number <port>. 
"
   (when bytes
     (setf port (verify-port port))
     
     (if (list-subtypep bytes 'list)
       (if (integerp port)
         (loop for item in bytes do
               (midi-o item port))
         (loop for item in bytes 
               for item1 in port do
               (midi-o item item1)))
       
       (loop for aport in (list! port) do
             (let ((event (om-midi-new-evt (om-midi-get-num-from-type "Stream") :port aport :bytes bytes)))
               (when event (om-midi-send-evt event *midiplayer*)) 
               t)))))

;===================PITCHBEND & WHEEL

(defmethod* pitchwheel ((vals number) (chans number) &optional port)
   :icon 912
   :indoc '("pitch wheel value(s)" "MIDI channel(s) (1-16)" "output port number")
   :initvals '(0 1 nil)
   :doc "Sends one or more MIDI pitch wheel message(s) of <vals> in the MIDI channel(s) <chans>.  

<values> and <chans> can be single numbers or lists. 

The range of pitch wheel is between -8192 and 8190.
"
   (setf port (verify-port port))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi-new-evt (om-midi-get-num-from-type "PitchWheel")
                                                                  :chan (- chans 1) :port aport 
                                                                  :bend vals)))
           (when event (om-midi-send-evt event *midiplayer*))
	   t)))

(defmethod* pitchwheel ((vals number) (chans list) &optional port)
   (loop for item in chans do
         (pitchwheel vals item port)))

(defmethod* pitchwheel ((vals list) (chans list) &optional port)
   (loop for item in chans 
         for item1 in vals do
         (pitchwheel item1 item port)))

;------------------------

;==== MODIFIED FUNCTION
(defmethod* pitchbend ((vals number) (chans number) &optional port)
   :icon 912
   :indoc '("pitch bend value(s)" "MIDI channel(s) (1-16)" "output port number")
   :initvals '(0 1 nil)
   :doc "Sends one or more MIDI pitch bend message(s) of <vals> in the MIDI channel(s) <chans>.  

<values> and <chans> can be single numbers or lists. 

The range of pitch wheel is between 0 and 127.
"
   (setf port (verify-port port))
   (pitchwheel (round (* (/ vals 127) 16382 )) chans port))

(defmethod* pitchbend ((vals number) (chans list) &optional port)
   (loop for item in chans do
         (pitchbend vals item port)))

(defmethod* pitchbend ((vals list) (chans list) &optional port)
   (loop for item in chans 
         for item1 in vals do
         (pitchbend item1 item port)))

;===================PGCHANGE

;===== MODIFIED FUNCTION =====
(defmethod* pgmout ((progm integer) (chans integer) &optional port) 
   :icon 912
   :indoc '("program number" "MIDI channel(s)" "output port number")
   :initvals '(2 1 nil)
   :doc "Sends a program change event with program number <progm> to channel(s) <chans>.

<progm> and <chans> can be single numbers or lists."
   (setf port (verify-port port))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi-new-evt (om-midi-get-num-from-type "ProgChange")
                                                                  :chan (- chans 1) :port aport
                                                                  :pgm progm)))
           (when event (om-midi-send-evt event *midiplayer*)))))

(defmethod* pgmout ((progm number) (chans list) &optional port)
  (loop for item in chans do
        (pgmout progm item port)))

(defmethod* pgmout ((progm list) (chans list) &optional port)
   (if (or (null port) (integerp port))
     (loop for item in chans 
           for item1 in progm do
           (pgmout item1 item port))
     (loop for item in chans 
           for item1 in progm
           for item2 in port do
           (pgmout item1 item item2))))


;===================POLY KEY PRESSURE
(defmethod* polyKeypres ((values integer) (pitch integer) (chans integer) &optional port) 
   :icon 912
   :indoc '("pressure value" "target pitch" "MIDI channel (1-16)" "output port number")
   :initvals '(100 6000 1 nil)
   :doc "
Sends a key pressure event with pressure <values> and <pitch> on channel <cahns> and port <port>.

Arguments can be single numbers or lists.
"
   (setf port (verify-port port))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi-new-evt (om-midi-get-num-from-type "KeyPress")
                                                                  :chan (- chans 1) :port aport
                                                                  :kpress values :pitch (round pitch 100))))
           (when event (om-midi-send-evt event *midiplayer*)))))

(defmethod* polyKeypres ((values list) (pitch list) (chans list) &optional port)
   (loop for item in pitch
         for val in values
         for chan in chans do
         (polyKeypres val item chan port)))

(defmethod* polyKeypres ((values integer) (pitch list) (chans integer) &optional port)
   (loop for item in pitch  do
         (polyKeypres values item chans port)))

(defmethod* polyKeypres ((values integer) (pitch list) (chans list) &optional port)
   (loop for item in pitch
         for chan in chans do
         (polyKeypres values item chan port)))

(defmethod* polyKeypres ((values integer) (pitch list) (chans list) &optional port)
   (loop for val in values do
         (polyKeypres val pitch chans port)))

(defmethod* polyKeypres ((values list) (pitch integer) (chans list) &optional port)
   (loop for val in values
         for chan in chans do
         (polyKeypres val pitch chan port)))

(defmethod* polyKeypres ((values list) (pitch integer) (chans integer) &optional port)
   (loop  for val in values do
          (polyKeypres val pitch chans port)))

(defmethod* polyKeypres ((values list) (pitch list) (chans integer) &optional port)
   (loop for item in pitch
         for val in values do
         (polyKeypres val item chans port)))



;======================AFTER TOUCH
(defmethod* aftertouch ((val integer) (chans integer) &optional port) 
   :icon 912
   :indoc '("pressurev value"  "MIDI channel (1-16)" "output port number")
   :initvals '(100 1 nil)
   :doc "Sends an after touch event of <val> to channel <chans> and port <port>.

Arguments can be can be single numbers or lists.
"
   (setf port (verify-port port))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi-new-evt (om-midi-get-num-from-type "ChanPress")
                                                                  :chan (- chans 1) :port aport
                                                                  :param val)))
               (when event (om-midi-send-evt event *midiplayer*)))))

(defmethod* aftertouch ((values number) (chans list) &optional port)
  (loop for item in chans do
        (aftertouch values item port)))

(defmethod* aftertouch ((values list) (chans list) &optional port)
   (if (or (null port) (integerp port))
     (loop for item in values
           for val in chans do
           (aftertouch item val port))
     (loop for item in values
           for val in chans
           for item2 in port do
           (aftertouch item val item2))))


;======================CTRL CHANGE 
(defmethod* ctrlchg ((ctrlnum integer) (val integer) (chans integer) &optional port) 
   :icon 912
   :indoc '("control number"  "value" "MIDI channel (1-16)" "output port number")
   :initvals '(7 100 1 nil)
   :doc "Sends a control change event with control number <ctrlnum> and value <val> to channel <chans> (and port <port>)."
   (setf port (verify-port port))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi-new-evt (om-midi-get-num-from-type "CtrlChange")
                                                                  :chan (- chans 1) :port aport
                                                                  :ctrlchange (list ctrlnum val))))
           (when event (om-midi-send-evt event *midiplayer*)))))

(defmethod* ctrlchg ((ctrlnum integer) (val integer) (chans list) &optional port) 
  (loop for item in chans do
        (ctrlchg  ctrlnum val item port)))

(defmethod* ctrlchg ((ctrlnum list) (val list) (chans list) &optional port) 
  (loop for ctrl in ctrlnum
        for item in chans
        for aval in val do
        (ctrlchg  ctrl aval item port)))

(defmethod* ctrlchg ((ctrlnum list) (val list) (chans integer) &optional port) 
  (loop for ctrl in ctrlnum
        for aval in val do
        (ctrlchg  ctrl aval chans port)))

(defmethod* ctrlchg ((ctrlnum list) (val integer) (chans integer) &optional port) 
  (loop for ctrl in ctrlnum do
        (ctrlchg  ctrl val chans port)))

(defmethod* ctrlchg ((ctrlnum integer) (val integer) (chans list) &optional port) 
  (loop  for item in chans do
        (ctrlchg  ctrlnum val item port)))

(defmethod* ctrlchg ((ctrlnum list) (val integer) (chans list) &optional port) 
  (loop for ctrl in ctrlnum
        for item in chans do
        (ctrlchg  ctrl val item port)))

(defmethod* ctrlchg ((ctrlnum integer) (val list) (chans list) &optional port) 
  (loop for item in chans
        for aval in val do
        (ctrlchg  ctrlnum aval item port)))







;======================VOLUME 
(defmethod* volume ((vol integer) (chans integer) &optional port) 
   :icon 912
   :indoc '("value" "MIDI channel (1-16)" "output port number")
   :initvals '(100 1 nil)
   :doc "Sends MIDI volume message(s) to channel(s) <chans> and port <port>.

Arguments can be numbers or lists. 

The range of volume values is 0-127.
"
   (setf port (verify-port port))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi-new-evt (om-midi-get-num-from-type "CtrlChange")
                                                                  :chan (- chans 1) :port aport
                                                                  :ctrlchange (list 7 vol))))
             (when event (om-midi-send-evt event *midiplayer*)))))

(defmethod* volume ((volume number)  (chans list) &optional port)
  (loop for item in chans do
        (volume volume item port)))

(defmethod* volume ((volume list)  (chans list) &optional port)
   (if (or (null port) (integerp port))
     (loop for item in volume
           for val in chans do
           (volume item val port))
     (loop for item in volume
           for val in chans
           for item2 in port do
           (volume item val item2))))


;======================SYSTEME EXCLUSIVE
(defmethod* sysex ((databytes list) &optional port) 
   :icon 912
   :indoc '("data bytes" "output port number")
   :initvals '((1 1 1 ) nil)
   :doc "Sends a system exclusive MIDI message on <port> with any number of data bytes, leading $F0 and tailing $F7"
   (when databytes
     (setf port (verify-port port))
     (if (list-subtypep databytes 'list)
       (if (integerp port)
         (loop for item in databytes do
               (sysex item port))
         (loop for item in databytes 
               for item1 in port do
               (sysex item item1)))
       (loop for aport in (list! port) do
             (let ((event (om-midi-new-evt (om-midi-get-num-from-type "SysEx") :port aport :bytes databytes)))
               (when event (om-midi-send-evt event *midiplayer*)))))))


;======================RESET
(defmethod* midi-reset (port)
   :icon 912
   :indoc '("ouput MIDI port")
   :initvals '(0)
   :doc "Sends a MIDI Reset message on port <port>."
   (loop for chan from 0 to 15 do 
         (let ((event (om-midi-new-evt (om-midi-get-num-from-type "Reset") :port (verify-port port) :chan chan)))
           (when event (om-midi-send-evt event *midiplayer*))))
   nil)


;======================SEND ONE NOTE
(defmethod! send-midi-note (port chan pitch vel dur track)
   :icon 148
   :initvals '(0 1 60 100 1000 1)
   (when (< dur 65000)
     (let ((event (om-midi-new-evt (om-midi-get-num-from-type "Note") :port port :chan chan
                                                              :date 0 :ref track
                                                              :vals (list pitch vel dur)
                                                              )))
           (when event (om-midi-send-evt event *midiplayer*)))
     ))


; (send-midi-note 0 1 60 100 1000 1)

;==============
;= MIDI LISTS =
;==============

;=== General MIDI programs with program numbers
(defvar *midi-programs* '(("0 - Acoustic Grand Piano" 0)
                          ("1 - Bright Acoustic Piano" 1)
                          ("2 - Electric Grand Piano" 2)
                          ("3 - Honki Tonk Piano" 3)
                          ("4 - Electric Piano 1" 4)
                          ("5 - Electric Piano 2" 5)
                          ("6 - Harpsichord" 6)
                          ("7 - Clavinet" 7)
                          ("8 - Celesta" 8)
                          ("9 - Glockenspiel" 9)
                          ("10 - Music Box" 10)
                          ("11 - Vibraphone" 11)
                          ("12 - Marimba" 12)
                          ("13 - Xylophone" 13)
                          ("14 - Tubular bells" 14)
                          ("15 - Dulcimer" 15)
                          ("16 - Drawbar Organ" 16)
                          ("17 - Percussive Organ" 17)
                          ("18 - Rock Organ" 18)
                          ("19 - Church Organ" 19)
                          ("20 - Reed Organ" 20)
                          ("21 - Accordion" 21)
                          ("22 - Harmonica" 22)
                          ("23 - Tango Accordion" 23)
                          ("24 - Nylon Acoustic Guitar" 24)
                          ("25 - Steel Acoustic Guitar" 25)
                          ("26 - Jazz Electric Guitar" 26)
                          ("27 - Clean Electric Guitar" 27)
                          ("28 - Muted Electric Guitar" 28)
                          ("29 - Overdrive Guitar" 29)
                          ("30 - Distorted Guitar" 30)
                          ("31 - Guitar Harmonics" 31)
                          ("32 - Acoustic Bass" 32)
                          ("33 - Electric Fingered Bass" 33)
                          ("34 - Electric Picked Bass" 34)
                          ("35 - Fretless Bass" 35)
                          ("36 - Slap Bass 1" 36)
                          ("37 - Slap Bass 2" 37)
                          ("38 - Synth Bass 1" 38)
                          ("39 - Synth Bass 2" 39)
                          ("40 - Violin" 40)
                          ("41 - Viola" 41)
                          ("42 - Cello" 42)
                          ("43 - Contrabass" 43)
                          ("44 - Tremolo Strings" 44)
                          ("45 - Pizzicato Strings" 45)
                          ("46 - Orchestral Harp" 46)
                          ("47 - Timpani" 47)
                          ("48 - String Ensemble 1" 48)
                          ("49 - String Ensemble 2" 49)
                          ("50 - Synth Strings 1" 50)
                          ("51 - Synth Strings 2" 51)
                          ("52 - Choir Aahs" 52)
                          ("53 - Voice Oohs" 53)
                          ("54 - Synth Voice" 54)
                          ("55 - Orchestra Hit" 55)
                          ("56 - Trumpet" 56)
                          ("57 - Trombone" 57)
                          ("58 - Tuba" 58)
                          ("59 - Muted Trumpet" 59)
                          ("60 - French Horn" 60)
                          ("61 - Brass Section" 61)
                          ("62 - Synth Brass 1" 62)
                          ("63 - Synth Brass 2" 63)
                          ("64 - Soprano Sax" 64)
                          ("65 - Alto Sax" 65)
                          ("66 - Tenor Sax" 66)
                          ("67 - Baritone Sax" 67)
                          ("68 - Oboe" 68)
                          ("69 - English Horn" 69)
                          ("70 - Bassoon" 70)
                          ("71 - Clarinet" 71)
                          ("72 - Piccolo" 72)
                          ("73 - Flute" 73)
                          ("74 - Recorder" 74)
                          ("75 - Pan Flute" 75)
                          ("76 - Bottle Blow" 76)
                          ("77 - Shakuhachi" 77)
                          ("78 - Whistle" 78)
                          ("79 - Ocarina" 79)
                          ("80 - Syn Square Wave" 80)
                          ("81 - Syn Sawtooth Wave" 81)
                          ("82 - Syn Calliope" 82)
                          ("83 - Syn Chiff" 83)
                          ("84 - Syn Charang" 84)
                          ("85 - Syn Voice" 85)
                          ("86 - Syn Fifths Sawtooth w-Wave" 86)
                          ("87 - Syn Brass and Lead" 87)
                          ("88 - New Age Syn Pad" 88)
                          ("89 - Warm Syn Pad" 89)
                          ("90 - Polysynth Syn Pad" 90)
                          ("91 - Choir Syn Pad" 91)
                          ("92 - Bowed Syn Pad" 92)
                          ("93 - Metal Syn Pad" 93)
                          ("94 - Halo Syn Pad" 94)
                          ("95 - Sweep Syn Pad" 95)
                          ("96 - FX Rain" 96)
                          ("97 - FX Soundtrack" 97)
                          ("98 - FX Crystal" 98)
                          ("99 - FX Atmosphere" 99)
                          ("100 - FX Brightness" 100)
                          ("101 - FX Goblins" 101)
                          ("102 - FX Echoes" 102)
                          ("103 - FX Sci-fi" 103)
                          ("104 - Sitar" 104)
                          ("105 - Banjo" 105)
                          ("106 - Shamisen" 106)
                          ("107 - Koto" 107)
                          ("108 - Kalimba" 108)
                          ("109 - Bag Pipe" 109)
                          ("110 - Fiddle" 110)
                          ("111 - Shanai" 111)
                          ("112 - Tinkle Bell" 112)
                          ("113 - Agogo" 113)
                          ("114 - Steel Drums" 114)
                          ("115 - Woodblock" 115)
                          ("116 - Taiko Drum" 116)
                          ("117 - Melodic Tom" 117)
                          ("118 - Syn Drum" 118)
                          ("119 - Reverse Cymbal" 119)
                          ("120 - Guitar Fret Noise" 120)
                          ("121 - Breath Noise" 121)
                          ("122 - Seashore" 122)
                          ("123 - Bird Tweet" 123)
                          ("124 - Telephone Ring" 124)
                          ("125 - Helicopter" 125)
                          ("126 - Applause" 126)
                          ("127 - Gun Shot" 127)))


;=== Pitches for General MIDI drum notes (channel 10)
(defvar *midi-drum-notes* '(("Acoustic Bass Drum" 35)
                           ("Bass Drum 1" 36)
                           ("Side Stick" 37)
                           ("Acoustic Snare" 38)
                           ("Hand Clap" 39)
                           ("Electric Snare" 40)
                           ("Low Floor Tom" 41)
                           ("Closed Hi Hat" 42)
                           ("High Floor Tom" 43)
                           ("Pedal Hi Hat" 44)
                           ("Low Tom" 45)
                           ("Open Hi Hat" 46)
                           ("Low Mid Tom" 47)
                           ("High Mid Tom" 48)
                           ("Crash Cymbal 1" 49)
                           ("High Tom" 50)
                           ("Ride Cymbal 1" 51)
                           ("Chinese Cymbal" 52)
                           ("Ride Bell" 53)
                           ("Tambourine" 54)
                           ("Splash Cymbal" 55)
                           ("Cowbell" 56)
                           ("Crash Cymbal 2" 57)
                           ("Vibraslap" 58)
                           ("Ride Cymbal 2" 59)
                           ("High Bongo" 60)
                           ("Low Bongo" 61)
                           ("Mute High Conga" 62)
                           ("Open High Conga" 63)
                           ("Low Conga" 64)
                           ("High Timbale" 65)
                           ("Low Timbale" 66)
                           ("High Agogo" 67)
                           ("Low Agogo" 68)
                           ("Cabasa" 69)
                           ("Maracas" 70)
                           ("Short Whistle" 71)
                           ("Long Whistle" 72)
                           ("Short Guiro" 73)
                           ("Long Guiro" 74)
                           ("Claves" 75)
                           ("High Wood Block" 76)
                           ("Low Wood Block" 77)
                           ("Mute Cuica" 78)
                           ("Open Cuica" 79)
                           ("Mute Triangle" 80)
                           ("Open Triangle" 81)))


;=== MidiShare Events withs event numbers
(defvar *ms-events* '(("0 - Note" 0)    
                     ("1 - KeyOn" 1)
                     ("2 - KeyOff" 2)
                     ("3 - KeyPress" 3)
                     ("4 - CtrlChange" 4)
                     ("5 - ProgChange" 5)
                     ("6 - ChanPress" 6)
                     ("7 - PitchBend" 7)
                     ("8 - SongPos" 8)
                     ("9 - SongSel" 9)
                     ("10 - Clock" 10)
                     ("11 - Start" 11)
                     ("12 - Continue" 12)
                     ("13 - Stop" 13)
                     ("14 - Tune" 14)
                     ("15 - ActiveSens" 15)
                     ("16 - Reset" 16)
                     ("17 - SysEx" 17)
                     ("18 - Stream" 18)
                     ("19 - Private" 19)
                     ("128 - Process" 128)
                     ("129 - DProcess" 129)
                     ("130 - QFrame" 130)
                     ("131 - Ctrl14b" 131)
                     ("132 - NonRegParam" 132)
                     ("133 - RegParam" 133)
                     ("134 - SeqNum" 134)
                     ("135 - Textual" 135)
                     ("136 - Copyright" 136)
                     ("137 - SeqName" 137)
                     ("138 - InstrName" 138)
                     ("139 - Lyric" 139)
                     ("140 - Marker" 140)
                     ("141 - CuePoint" 141)
                     ("142 - ChanPrefix" 142)
                     ("143 - EndTrack" 143)
                     ("144 - Tempo" 144)
                     ("145 - SMPTEOffset" 145)
                     ("146 - TimeSign" 146)
                     ("147 - KeySign" 147)
                     ("148 - Specific" 148)
                     ("149 - PortPrefix" 149)
                     ("150 - RcvAlarm" 150)
                     ("151 - ApplAlarm" 151)
                     ("152 - Reserved" 152)
                     ("255 - dead" 255)))

;=== MidiShare Events withs event symbol
(defvar *ms-events-symb* '(("Note" 'Note)    
                      ("KeyOn " 'KeyOn)
                      ("KeyOff" 'KeyOff)
                      ("KeyPress" 'KeyPress)
                      ("CtrlChange" 'CtrlChange)
                      ("ProgChange" 'ProgChange)
                      ("ChanPress" 'ChanPress)
                      ("PitchWheel/PitchBend" 'PitchBend)
                      ("SongPos" 'SongPos)
                      ("SongSel" 'SongSel)
                      ("Clock" 'Clock)
                      ("Start" 'Start)
                      ("Continue" 'Continue)
                      ("Stop" 'Stop)
                      ("Tune" 'Tune)
                      ("ActiveSens" 'ActiveSens)
                      ("Reset" 'Reset)
                      ("SysEx" 'SysEx)
                      ("Stream" 'Stream)
                      ("Private" 'Private)
                      ("Process" 'Process)
                      ("DProcess" 'DProcess)
                      ("QFrame" 'QFrame)
                      ("Ctrl14b" 'Ctrl14b)
                      ("NonRegParam" 'NonRegParam)
                      ("RegParam" 'RegParam)
                      ("SeqNum" 'SeqNum)
                      ("Textual" 'Textual)
                      ("Copyright" 'Copyright)
                      ("SeqName" 'SeqName)
                      ("InstrName" 'InstrName)
                      ("Lyric" 'Lyric)
                      ("Marker" 'Marker)
                      ("CuePoint" 'CuePoint)
                      ("ChanPrefix" 'ChanPrefix)
                      ("EndTrack" 'EndTrack)
                      ("Tempo" 'Tempo)
                      ("SMPTEOffset" 'SMPTEOffset)
                      ("TimeSign" 'TimeSign)
                      ("KeySign" 'KeySign)
                      ("Specific" 'Specific)
                      ("PortPrefix" 'PortPrefix)
                      ("RcvAlarm" 'RcvAlarm)
                      ("ApplAlarm" 'ApplAlarm)
                      ("Reserved" 'Reserved)
                      ("dead" 'dead)))


;=== General MIDI controllers num
(setf *midi-ctrl-chge* '(("00 - Bank Select" 0)
                           ("01 - Modulation Wheel" 1)
                           ("02 - Breath Controller" 2)
                           ("03 - Undefined" 3)
                           ("04 - Foot Controller" 4)
                           ("05 - Portamento Time" 5)
                           ("06 - Data Entry MSB" 6)
                           ("07 - Channel Volume" 7)
                           ("08 - Balance" 8)
                           ("09 - Undefined" 9)
                           ("10 - Pan" 10)
                           ("11 - Expression Controller" 11)
                           ("12 - Effect Control 1" 12)
                           ("13 - Effect Control 2" 13)
                           ("14 - Undefined" 14)
                           ("15 - Undefined" 15)
                           ("16 - General Purpose Controller 1" 16)
                           ("17 - General Purpose Controller 2" 17)
                           ("18 - General Purpose Controller 3" 18)
                           ("19 - General Purpose Controller 4" 19)
                           ("20 - Undefined" 20)
                           ("21 - Undefined" 21)
                           ("22 - Undefined" 22)
                           ("23 - Undefined" 23)
                           ("24 - Undefined" 24)
                           ("25 - Undefined" 25)
                           ("26 - Undefined" 26)
                           ("27 - Undefined" 27)
                           ("28 - Undefined" 28)
                           ("29 - Undefined" 29)
                           ("30 - Undefined" 30)
                           ("31 - Undefined" 31)
                           ("32 - Bank Select Fine" 32)
                           ("33 - Modulation Wheel  Fine" 33)
                           ("34 - Breath Controller Fine" 34)
                           ("35 - Ctrl03 (Undefined) Fine" 35)
                           ("36 - Foot Controller Fine" 36)
                           ("37 - Portamento Time Fine" 37)
                           ("38 - Data Entry LSB" 38)
                           ("39 - Channel Volume Fine" 39)
                           ("40 - Balance Fine" 40)
                           ("41 - Ctrl09 (Undefined) Fine" 41)
                           ("42 - Pan Fine" 42)
                           ("43 - Expression Controller Fine" 43)
                           ("44 - Effect Control 1 Fine" 44)
                           ("45 - Effect Control 2 Fine" 45)
                           ("46 - Ctrl14 (Undefined) Fine" 46)
                           ("47 - Ctrl15 (Undefined) Fine" 47)
                           ("48 - General Purpose Controller 1 Fine" 48)
                           ("49 - General Purpose Controller 2 Fine" 49)
                           ("50 - General Purpose Controller 3 Fine" 50)
                           ("51 - General Purpose Controller 4 Fine" 51)
                           ("52 - Undefined" 52)
                           ("53 - Undefined" 53)
                           ("54 - Undefined" 54)
                           ("55 - Undefined" 55)
                           ("56 - Undefined" 56)
                           ("57 - Undefined" 57)
                           ("58 - Undefined" 58)
                           ("59 - Undefined" 59)
                           ("60 - Undefined" 60)
                           ("61 - Undefined" 61)
                           ("62 - Undefined" 62)
                           ("63 - Undefined" 63)
                           ("64 - Damper Pedal" 64)
                           ("65 - Portamento" 65)
                           ("66 - Sustenuto" 66)
                           ("67 - Soft Pedal" 67)
                           ("68 - Legato Footswitch" 68)
                           ("69 - Hold 2" 69)
                           ("70 - Sound Controller 1" 70)
                           ("71 - Sound Controller 2" 71)
                           ("72 - SoundController 3" 72)
                           ("73 - SoundController 4" 73)
                           ("74 - SoundController 5" 74)
                           ("75 - SoundController 6" 75)
                           ("76 - SoundController7" 76)
                           ("77 - SoundController8" 77)
                           ("78 - SoundController9" 78)
                           ("79 - SoundController10" 79)
                           ("80 - Undefined" 80)
                           ("81 - Undefined" 81)
                           ("82 - Undefined" 82)
                           ("83 - Undefined" 83)
                           ("84 - PortamentoControl" 84)
                           ("85 - Undefined" 85)
                           ("86 - Undefined" 86)
                           ("87 - Undefined" 87)
                           ("88 - Undefined" 88)
                           ("89 - Undefined" 89)
                           ("90 - Undefined" 90)
                           ("91 - Effects1Depth" 91)
                           ("92 - Effects2Depth" 92)
                           ("93 - Effects3Depth" 93)
                           ("94 - Effects4Depth" 94)
                           ("95 - Effects5Depth" 95)
                           ("96 - DataIncrement" 96)))


;==================================
;=== MIDI LISTS SELECTION BOXES ===
;==================================

(defmethod! GM-program (progName)
  :initvals '(nil)
  :indoc '("Instrument name")
  :menuins (list (list 0 *midi-programs*))
  :doc "Outputs General MIDI program number corresponding to <progName>."
  :icon 917
  progName)


(defmethod! GM-DrumNote (drumName)
  :initvals '(nil)
  :indoc '("Drum name")
  :menuins (list (list 0 *midi-drum-notes*))
  :doc "Outputs General MIDI note number corresponding to <drumname>."
  :icon 916
  drumName)


(defmethod! control-change (ctrl)
  :initvals '(nil)
  :indoc '("control name")
  :menuins (list (list 0 *midi-ctrl-chge*))
  :doc "Outputs control number corresponding to the <ctrl>."
  :icon 920
  ctrl
  )

(defmethod! MS-event (evt)
  :initvals '(nil)
  :indoc '("midishare event")
  :menuins (list (list 0 *ms-events*))
  :doc "Outputs event number corresponding to <evt>."
  :icon 148
  evt)


;=============================================================



 
