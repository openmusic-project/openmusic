;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name: rel-1_0_2 $
;;; $Revision: 1.4 $
;;; $Date: 2006/01/07 19:45:05 $

;;; A CFFI interface to Midishare. Should run in most Common Lisp
;;; implementations on Linux, OS X and Windows. For information about
;;; CFFI see http://common-lisp.net/project/cffi/

(in-package :cl-user)

(defvar *libmidishare*
  #+win32
  "/WINDOWS/system32/mshare32.dll"
  #+(or darwin macos macosx)
  "/System/Library/Frameworks/MidiShare.framework/MidiShare"
  #+(or linux (and clisp unix (not macos)))
  "/usr/lib/libMidiShare.so")

;(defvar *midishare* nil) ; t if loaded.

;(if (probe-file *libmidishare*)
;    (progn (cffi:load-foreign-library *libmidishare*)
;           (setq *midishare* t))
;    (error "Midishare library ~s not found. Fix *libmidishare*."
;           *libmidishare*))

(defpackage :midishare
  (:use :common-lisp) 
  (:nicknames :ms)
  (:shadow )
  (:export :typenote :typekeyon :typekeyoff :typekeypress 
           :typectrlchange :typeprogchange :typechanpress :typepitchwheel 
           :typepitchbend :typesongpos :typesongsel :typeclock :typestart 
           :typecontinue :typestop :typetune :typeactivesens :typereset
           :typesysex :typestream :typeprivate :typeprocess :typedprocess
           :typeqframe :typectrl14b :typenonregparam :typeregparam
           :typeseqnum :typetextual :typecopyright :typeseqname
           :typeinstrname :typelyric :typemarker :typecuepoint
           :typechanprefix :typeendtrack :typetempo :typesmpteoffset
           :typetimesign :typekeysign :typespecific :typeportprefix
           :typercvalarm :typeapplalarm :typereserved :typedead
           :midierrspace :midierrrefnum :midierrbadtype :midierrindex
           :modemport :printerport :midiexternalsync :midisyncanyport
           :smpte24fr :smpte25fr :smpte29fr :smpte30fr :midiopenappl
           :midicloseappl :midichgname :midichgconnect :midiopenmodem 
           :midiclosemodem :midiopenprinter :midicloseprinter
           :midisyncstart :midisyncstop :midichangesync :midiopendriver
           :midiclosedriver :midiaddslot :midiremoveslot 
           :midichgslotconnect :midichgslotname :link :date :evtype :ref
           :port :chan :field :fieldslist :pitch :vel :dur :linkse
           :linkst :kpress :ctrl :param :num :prefix :tempo :seconds
           :subframes :val :pgm :bend :clk :song :fields :text :fmsg
           :fcount :tsnum :tsdenom :tsclick :tsquarter :alteration
           :minor-scale :info :firstev :lastev :midishare :midigetversion
           :midicountappls :midigetindappl :midigetnamedappl :midiopen
           :midiclose :midigetname :midisetname :midigetinfo :midisetinfo
           :midinewfilter :midifreefilter :midiacceptchan
           :midiaccepttype :midiacceptport :midiisacceptedchan
           :midiisacceptedtype :midiisacceptedport :midigetfilter
           :midisetfilter :midigetrcvalarm :midisetrcvalarm 
           :midigetapplalarm :midisetapplalarm :midiconnect
           :midiisconnected :midigetportstate :midisetportstate
           :midifreespace :midinewev :midicopyev :midifreeev :midisetfield
           :midigetfield :midiaddfield :midicountfields :midinewseq
           :midiaddseq :midifreeseq :midiclearseq :midiapplyseq 
           :midigettime :midisendim :midisend :midisendat :midicountevs
           :midigetev :midiavailev :midiflushevs :midireadsync
           :midiwritesync :midicall :miditask :mididtask
           :midiforgettaskhdl :midiforgettask :midicountdtasks
           :midiflushdtasks :midiexec1dtask :midinewcell :midifreecell
           :miditotalspace :midigrowspace :midigetsyncinfo
           :midisetsyncmode :midigetexttime :midiint2exttime
           :midiext2inttime :miditime2smpte :midismpte2time
           :midicountdrivers :midigetinddriver :midigetdriverinfos
           :midigetindslot :midigetslotinfos :midiconnectslot
           :midiisslotconnected :midinewsmptelocation
           :midifreesmptelocation :midinewsyncinfo :midifreesyncinfo  
           :nullptrp :nullptr :load-framework-bundle :get-fun-addr
           :add-startup-action :add-quit-action 
           ;; added api
           :midishare-receive :midishare-stop-receive
           ))
  
(in-package :midishare)

(defvar *midishare* nil) ; t if loaded.

(defun midishare-framework ()
  (print (format nil "Loading MIDIShare library [~A]" (namestring cl-user::*libmidishare*)))
  (or *midishare*
      (setq *midishare*
            (if (probe-file cl-user::*libmidishare*)
                (cffi:load-foreign-library cl-user::*libmidishare*)
              (progn (print "Library MIDIShare not found!") NIL)
              ))))

(defun nullptrp (x)
  (cffi:null-pointer-p x))

(defun nullptr ()
  (cffi:null-pointer))

;; generated by Todd Ingalls from midishare headers using verrazano
;; for info about verrazano see http://common-lisp.net/project/fetter/

;;; CFFI 0.11: DEFCTYPE REPLACES DEFINE-FOREIGN-TYPE
;;;(cffi:define-foreign-type midi-sexptr () ':pointer)
;;;(cffi:define-foreign-type byte () ':unsigned-char)
(cffi:defctype midi-sexptr :pointer)
(cffi:defctype byte :unsigned-char)

(cffi:defcstruct tmidi-sex-1 (link midi-sexptr) (data byte :count 11))
(cffi:defcstruct tmidi-st-1 (val :long :count 3))
(cffi:defcstruct tmidi-ev-anonymous1648-anonymous1635 (number :short) (unused :short))
(cffi:defcstruct tmidi-ev-anonymous1648-anonymous1632 (num :short) (val :short))
(cffi:defcstruct tmidi-ev-anonymous1648-anonymous1629 (ton :char) (mode byte) (unused byte :count 1))
(cffi:defcstruct tmidi-ev-anonymous1648-anonymous1625 (numerator byte) (denominator byte) (n-clocks byte) (n32nd byte))
(cffi:defcstruct tmidi-ev-anonymous1648-anonymous1622 (pitch byte) (vel byte) (dur :short))

;;;(cffi:define-foreign-type midi-stptr () ':pointer)
(cffi:defctype midi-stptr :pointer)

(cffi:defcunion tmidi-ev-anonymous1648 
  (note tmidi-ev-anonymous1648-anonymous1622) 
  (time-sign tmidi-ev-anonymous1648-anonymous1625)
  (key-sign tmidi-ev-anonymous1648-anonymous1629) 
  (param tmidi-ev-anonymous1648-anonymous1632) 
  (seq-num tmidi-ev-anonymous1648-anonymous1635) 
  (short-fields :short :count 1) (long-field :long) (tempo :long) (data byte :count 3)
  (link-se midi-sexptr) (link-st midi-stptr))

;;; (cffi:define-foreign-type midi-ev-ptr () ':pointer)
(cffi:defctype midi-ev-ptr :pointer)

;; TMidiEv
(cffi:defcstruct tmidi-ev-1 (link midi-ev-ptr) (date :unsigned-long) (ev-type byte) (ref-num byte) (port byte) (chan byte) (info tmidi-ev-anonymous1648))

;;;(cffi:define-foreign-type ptr () ':pointer)
(cffi:defctype ptr :pointer)

(cffi:defcstruct tmidi-seq-1 (first midi-ev-ptr) (last midi-ev-ptr) (undef1 ptr) (undef2 ptr))
(cffi:defcstruct tfilter-1 (port :char :count 31) (ev-type :char :count 31) (channel :char :count 1) (unused :char :count 1))
(cffi:defcstruct slot-ref-num (drv-ref :short) (slot-ref :short))

;;;(cffi:define-foreign-type slot-name () ':char)
;;;(cffi:define-foreign-type driver-name () ':char)
(cffi:defctype slot-name :char)
(cffi:defctype driver-name :char)

;(cffi:defcenum slot-direction (:midi-input-slot 1) (:midi-output-slot 2) (:midi-input-output-slot 3))
(cffi:defcstruct tslot-infos-1 (name :char :count 31) (direction :short) (cnx :char :count 31) (reserved :long :count 1)) ; slot-direction

;;;(cffi:define-foreign-type wakeup-ptr () ':pointer)
;;;(cffi:define-foreign-type sleep-ptr () ':pointer)
(cffi:defctype wakeup-ptr :pointer)
(cffi:defctype sleep-ptr :pointer)

(cffi:defcstruct tdriver-operation-1 (wakeup wakeup-ptr) (sleep sleep-ptr) (reserved :long :count 2))
(cffi:defcstruct tdriver-infos-1 (name driver-name :count 31) (version :short) (slots :short) (reserved :long :count 1))
(cffi:defcstruct tsync-info-1 (time :long) (reenter :long) (sync-mode :short) (sync-locked byte) (sync-port byte) (sync-start :long) (sync-stop :long) (sync-offset :long) (sync-speed :long) (sync-breaks :long) (sync-format :short))
(cffi:defcstruct tsmpte-location-1 (format :short) (hours :short) (minutes :short) (seconds :short) (frames :short) (fracs :short))

#|
(cffi:define-foreign-type boolean () ':char)
;(cffi:define-foreign-type boolean () ':unsigned-char)
(cffi:define-foreign-type midi-filter-ptr () ':pointer)
(cffi:define-foreign-type task-ptr () ':pointer)                    
(cffi:define-foreign-type midi-seq-ptr () ':pointer)
(cffi:define-foreign-type apply-proc-ptr () ':pointer)
(cffi:define-foreign-type tslot-infos () 'tslot-infos-1)
(cffi:define-foreign-type midi-name () ':pointer)
(cffi:define-foreign-type tdriver-infos () 'tdriver-infos-1)
(cffi:define-foreign-type tdriver-operation () 'tdriver-operation-1)
(cffi:define-foreign-type appl-alarm-ptr () ':pointer)
(cffi:define-foreign-type rcv-alarm-ptr () ':pointer)
(cffi:define-foreign-type smpte-loc-ptr () ':pointer)
(cffi:define-foreign-type sync-info-ptr () ':pointer)
;; "user" structs
(cffi:define-foreign-type tsmpte-location () 'tsmpte-location-1)
(cffi:define-foreign-type tsync-info () 'tsync-info-1)
(cffi:define-foreign-type tfilter () 'tfilter-1)
(cffi:define-foreign-type tmidi-seq () 'tmidi-seq-1)
(cffi:define-foreign-type tmidi-ev () 'tmidi-ev-1)
(cffi:define-foreign-type tmidi-st () 'tmidi-st-1)
(cffi:define-foreign-type tmidi-sex () 'tmidi-sex-1)
|#

(cffi:defctype boolean :char)
;(cffi:defctype boolean :unsigned-char)
(cffi:defctype midi-filter-ptr :pointer)
(cffi:defctype task-ptr :pointer)                    
(cffi:defctype midi-seq-ptr :pointer)
(cffi:defctype apply-proc-ptr :pointer)
(cffi:defctype tslot-infos tslot-infos-1)
(cffi:defctype midi-name :pointer)
(cffi:defctype tdriver-infos tdriver-infos-1)
(cffi:defctype tdriver-operation tdriver-operation-1)
(cffi:defctype appl-alarm-ptr :pointer)
(cffi:defctype rcv-alarm-ptr :pointer)
(cffi:defctype smpte-loc-ptr :pointer)
(cffi:defctype sync-info-ptr :pointer)
;; "user" structs
(cffi:defctype tsmpte-location tsmpte-location-1)
(cffi:defctype tsync-info tsync-info-1)
(cffi:defctype tfilter tfilter-1)
(cffi:defctype tmidi-seq tmidi-seq-1)
(cffi:defctype tmidi-ev tmidi-ev-1)
(cffi:defctype tmidi-st tmidi-st-1)
(cffi:defctype tmidi-sex tmidi-sex-1)



(eval-when (:compile-toplevel)
  ;; unfortunately cffi:foreign-slot-value doesn't handle slot paths yet
  ;; so I hack it using metainfo about the structs defined above.
  (defvar %structinfo
    '((tmidi-ev-anonymous1648-anonymous1635 (number :short) 
       (unused :short))
      (tmidi-ev-anonymous1648-anonymous1632 (num :short) (val :short))
      (tmidi-ev-anonymous1648-anonymous1629 (ton :char) (mode byte)
       (unused byte 1))
      (tmidi-ev-anonymous1648-anonymous1625 (numerator byte) 
       (denominator byte) (n-clocks byte) (n32nd byte))
      (tmidi-ev-anonymous1648-anonymous1622 (pitch byte) (vel byte)
       (dur :short))
      (tmidi-ev-anonymous1648 (note tmidi-ev-anonymous1648-anonymous1622)
       (time-sign tmidi-ev-anonymous1648-anonymous1625)
       (key-sign tmidi-ev-anonymous1648-anonymous1629)
       (param tmidi-ev-anonymous1648-anonymous1632)
       (seq-num tmidi-ev-anonymous1648-anonymous1635)
       (short-fields :short 1)
       (long-field :long)
       (tempo :long)
       (data byte 3)
       (link-se midi-sexptr)
       (link-st midi-stptr))
      (tmidi-ev-1 (link midi-ev-ptr) (date :unsigned-long) (ev-type byte)
       (ref-num byte) (port byte) (chan byte) 
       (info tmidi-ev-anonymous1648))))
  (defun foreign-slot-access (ptr struct path)
    ;; return (possibly) nested foreign-slot-value forms for
    ;; a specified slot path
    (if (null path) ptr
        (flet ((findstruct (slot type)
                 (let ((data (assoc type %structinfo)))
                   (if data
                       (let ((next (cadr (assoc slot (cdr data)))))
                         (if (assoc next %structinfo)
                             next nil))
                       (error "Unknown struct: ~A" type))))
               (unq (x) (if (and (consp x) (eql (car x) 'quote))
                            (cadr x) x)))
          (let* ((type (unq struct))
                 (slot (unq (car path)))
                 (path (cdr path))
                 (next (findstruct slot type)))
            (foreign-slot-access `(cffi:foreign-slot-value ,ptr ',type ',slot)
                                 next path))))))

;; temporary substitute for cffi:foreign-slot-value
(defmacro %foreign-slot-value (x y &rest z)
  (foreign-slot-access x y z))

;;; entrypoints
(cl:progn
 (cffi:defcfun ("MidiIsAcceptedType" midi-is-accepted-type) boolean (f midi-filter-ptr) (type :short))
 (cffi:defcfun ("MidiIsAcceptedChan" midi-is-accepted-chan) boolean (f midi-filter-ptr) (chan :short))
 (cffi:defcfun ("MidiIsAcceptedPort" midi-is-accepted-port) boolean (f midi-filter-ptr) (port :short))
 (cffi:defcfun ("MidiAcceptType" midi-accept-type) :void (f midi-filter-ptr) (type :short) (state boolean))
 (cffi:defcfun ("MidiAcceptChan" midi-accept-chan) :void (f midi-filter-ptr) (chan :short) (state boolean))
 (cffi:defcfun ("MidiAcceptPort" midi-accept-port) :void (f midi-filter-ptr) (port :short) (state boolean))
 (cffi:defcfun ("MidiFreeFilter" midi-free-filter) :void (filter midi-filter-ptr))
 (cffi:defcfun ("MidiNewFilter" midi-new-filter) midi-filter-ptr)
 (cffi:defcfun ("MidiExec1DTask" midi-exec1dtask) :void (refnum :short))
 (cffi:defcfun ("MidiFlushDTasks" midi-flush-dtasks) :void (refnum :short))
 (cffi:defcfun ("MidiCountDTasks" midi-count-dtasks) :long (refnum :short))
 (cffi:defcfun ("MidiForgetTask" midi-forget-task) :void (e :pointer))
 (cffi:defcfun ("MidiDTask" midi-dtask) midi-ev-ptr (routine task-ptr) (date :long) (ref-num :short) (a1 :long) (a2 :long) (a3 :long))
 (cffi:defcfun ("MidiTask" midi-task) midi-ev-ptr (routine task-ptr) (date :long) (ref-num :short) (a1 :long) (a2 :long) (a3 :long))
 (cffi:defcfun ("MidiCall" midi-call) :void (routine task-ptr) (date :long) (ref-num :short) (a1 :long) (a2 :long) (a3 :long))
 (cffi:defcfun ("MidiWriteSync" midi-write-sync) :pointer (adr-mem :pointer) (val :pointer))
 (cffi:defcfun ("MidiReadSync" midi-read-sync) :pointer (adr-mem :pointer))
 (cffi:defcfun ("MidiFlushEvs" midi-flush-evs) :void (ref-num :short))
 (cffi:defcfun ("MidiAvailEv" midi-avail-ev) midi-ev-ptr (ref-num :short))
 (cffi:defcfun ("MidiGetEv" midi-get-ev) midi-ev-ptr (ref-num :short))
 (cffi:defcfun ("MidiCountEvs" midi-count-evs) :long (ref-num :short))
 (cffi:defcfun ("MidiSendAt" midi-send-at) :void (ref-num :short) (e midi-ev-ptr) (d :long))
 (cffi:defcfun ("MidiSend" midi-send) :void (ref-num :short) (e midi-ev-ptr))
 (cffi:defcfun ("MidiSendIm" midi-send-im) :void (ref-num :short) (e midi-ev-ptr))
 (cffi:defcfun ("MidiGetTime" midi-get-time) :long)
 (cffi:defcfun ("MidiApplySeq" midi-apply-seq) :void (s midi-seq-ptr) (proc apply-proc-ptr))
 (cffi:defcfun ("MidiClearSeq" midi-clear-seq) :void (s midi-seq-ptr))
 (cffi:defcfun ("MidiFreeSeq" midi-free-seq) :void (s midi-seq-ptr))
 (cffi:defcfun ("MidiAddSeq" midi-add-seq) :void (s midi-seq-ptr) (e midi-ev-ptr))
 (cffi:defcfun ("MidiNewSeq" midi-new-seq) midi-seq-ptr)
 (cffi:defcfun ("MidiCountFields" midi-count-fields) :long (e midi-ev-ptr))
 (cffi:defcfun ("MidiAddField" midi-add-field) :void (e midi-ev-ptr) (v :long))
 (cffi:defcfun ("MidiGetField" midi-get-field) :long (e midi-ev-ptr) (f :long))
 (cffi:defcfun ("MidiSetField" midi-set-field) :void (e midi-ev-ptr) (f :long) (v :long))
 (cffi:defcfun ("MidiFreeEv" midi-free-ev) :void (e midi-ev-ptr))
 (cffi:defcfun ("MidiCopyEv" midi-copy-ev) midi-ev-ptr (e midi-ev-ptr))
 (cffi:defcfun ("MidiNewEv" midi-new-ev) midi-ev-ptr (type-num :short))
 (cffi:defcfun ("MidiGrowSpace" midi-grow-space) :long (n :long))
 (cffi:defcfun ("MidiTotalSpace" midi-total-space) :long)
 (cffi:defcfun ("MidiFreeCell" midi-free-cell) :void (e midi-ev-ptr))
 (cffi:defcfun ("MidiNewCell" midi-new-cell) midi-ev-ptr)
 (cffi:defcfun ("MidiFreeSpace" midi-free-space) :long)
 (cffi:defcfun ("MidiSetPortState" midi-set-port-state) :void (port :short) (state boolean))
 (cffi:defcfun ("MidiGetPortState" midi-get-port-state) boolean (port :short))
 (cffi:defcfun ("MidiIsConnected" midi-is-connected) boolean (src :short) (dest :short))
 (cffi:defcfun ("MidiConnect" midi-connect) :void (src :short) (dest :short) (state boolean))
 (cffi:defcfun ("MidiIsSlotConnected" midi-is-slot-connected) boolean (port :short) (slot :pointer))
 (cffi:defcfun ("MidiConnectSlot" midi-connect-slot) :void (port :short) (slot :pointer) (state boolean))
 (cffi:defcfun ("MidiGetSlotInfos" midi-get-slot-infos) boolean (slot :pointer) (infos :pointer))
 (cffi:defcfun ("MidiSetSlotName" midi-set-slot-name) :void (slot :pointer) (name midi-name))
 (cffi:defcfun ("MidiRemoveSlot" midi-remove-slot) :void (slot :pointer))
 (cffi:defcfun ("MidiGetIndSlot" midi-get-ind-slot) slot-ref-num (refnum :short) (index :short))
 (cffi:defcfun ("MidiAddSlot" midi-add-slot) slot-ref-num (refnum :short) (name midi-name) (direction :short )) ;slot-direction
 (cffi:defcfun ("MidiGetDriverInfos" midi-get-driver-infos) boolean (refnum :short) (infos :pointer))
 (cffi:defcfun ("MidiGetIndDriver" midi-get-ind-driver) :short (index :short))
 (cffi:defcfun ("MidiCountDrivers" midi-count-drivers) :short)
 (cffi:defcfun ("MidiUnregisterDriver" midi-unregister-driver) :void (refnum :short))
 (cffi:defcfun ("MidiRegisterDriver" midi-register-driver) :short (infos :pointer) (op :pointer))
 (cffi:defcfun ("MidiSetApplAlarm" midi-set-appl-alarm) :void (ref-num :short) (alarm appl-alarm-ptr))
 (cffi:defcfun ("MidiGetApplAlarm" midi-get-appl-alarm) appl-alarm-ptr (ref-num :short))
 (cffi:defcfun ("MidiSetRcvAlarm" midi-set-rcv-alarm) :void (ref-num :short) (alarm rcv-alarm-ptr))
 (cffi:defcfun ("MidiGetRcvAlarm" midi-get-rcv-alarm) rcv-alarm-ptr (ref-num :short))
 (cffi:defcfun ("MidiSetFilter" midi-set-filter) :void (ref-num :short) (f midi-filter-ptr))
 (cffi:defcfun ("MidiGetFilter" midi-get-filter) midi-filter-ptr (ref-num :short))
 (cffi:defcfun ("MidiSetInfo" midi-set-info) :void (ref-num :short) (info-zone :pointer))
 (cffi:defcfun ("MidiGetInfo" midi-get-info) :pointer (ref-num :short))
 (cffi:defcfun ("MidiSetName" midi-set-name) :void (ref-num :short) (appl-name midi-name))
 (cffi:defcfun ("MidiGetName" midi-get-name) midi-name (ref-num :short))
 (cffi:defcfun ("MidiClose" midi-close) :void (ref-num :short))
 (cffi:defcfun ("MidiOpen" midi-open) :short (appl-name midi-name))
 (cffi:defcfun ("MidiSmpte2Time" midi-smpte2time) :long (loc smpte-loc-ptr))
 (cffi:defcfun ("MidiTime2Smpte" midi-time2smpte) :void (time :long) (format :short) (loc smpte-loc-ptr))
 (cffi:defcfun ("MidiExt2IntTime" midi-ext2int-time) :long (anonymous1613 :long))
 (cffi:defcfun ("MidiInt2ExtTime" midi-int2ext-time) :long (anonymous1614 :long))
 (cffi:defcfun ("MidiGetExtTime" midi-get-ext-time) :long)
 (cffi:defcfun ("MidiSetSyncMode" midi-set-sync-mode) :void (mode :short))
 (cffi:defcfun ("MidiGetSyncInfo" midi-get-sync-info) :void (p sync-info-ptr))
 (cffi:defcfun ("MidiGetNamedAppl" midi-get-named-appl) :short (name midi-name))
 (cffi:defcfun ("MidiGetIndAppl" midi-get-ind-appl) :short (index :short))
 (cffi:defcfun ("MidiCountAppls" midi-count-appls) :short)
 (cffi:defcfun ("MidiGetVersion" midi-get-version) :short)
 (cffi:defcfun ("MidiShare" midi-share) :int)
)

;;; Constant definitions for every type of MidiShare event

(defconstant typeNote          0 "note with pitch, velocity and duration")
(defconstant typeKeyOn         1 "key on with pitch and velocity")
(defconstant typeKeyOff        2 "key off with pitch and velocity")
(defconstant typeKeyPress      3 "key pressure with pitch and pressure value")
(defconstant typeCtrlChange    4 "control change with control number and control value")
(defconstant typeProgChange    5 "program change with program number")
(defconstant typeChanPress     6 "channel pressure with pressure value")
(defconstant typePitchWheel    7 "pitch bend with lsb and msb of the 14-bit value")
(defconstant typePitchBend     7 "pitch bender with lsb and msb of the 14-bit value")
(defconstant typeSongPos       8 "song position with lsb and msb of the 14-bit position")
(defconstant typeSongSel       9 "song selection with a song number")
(defconstant typeClock        10 "clock request (no argument)")
(defconstant typeStart        11 "start request (no argument)")
(defconstant typeContinue     12 "continue request (no argument)")
(defconstant typeStop         13 "stop request (no argument)")
(defconstant typeTune         14 "tune request (no argument)")
(defconstant typeActiveSens   15 "active sensing code (no argument)")
(defconstant typeReset        16 "reset request (no argument)")
(defconstant typeSysEx        17 "system exclusive with any number of data bytes. Leading $F0 and tailing $F7 are automatically supplied by MidiShare and MUST NOT be included by the user")
(defconstant typeStream       18 "special event with any number of unprocessed data/status bytes")
(defconstant typePrivate      19 "private event for internal use with 4 32-bits arguments")
(defconstant typeProcess     128 "interrupt level task with a function adress and 3 32-bits args")
(defconstant typeDProcess    129 "foreground task with a function address and 3 32-bits arguments")
(defconstant typeQFrame      130 "quarter frame message with a type from 0 to 7 and a value")
(defconstant typeCtrl14b     131)
(defconstant typeNonRegParam 132)
(defconstant typeRegParam    133)
(defconstant typeSeqNum	     134)
(defconstant typeTextual     135)
(defconstant typeCopyright   136)
(defconstant typeSeqName     137)
(defconstant typeInstrName   138)
(defconstant typeLyric	     139)
(defconstant typeMarker	     140)
(defconstant typeCuePoint    141)
(defconstant typeChanPrefix  142)
(defconstant typeEndTrack    143)
(defconstant typeTempo	     144)
(defconstant typeSMPTEOffset 145)
(defconstant typeTimeSign    146)
(defconstant typeKeySign     147)
(defconstant typeSpecific    148)
(defconstant typePortPrefix  149)
(defconstant typeRcvAlarm    150)
(defconstant typeApplAlarm   151)
(defconstant typeReserved    152)
(defconstant typedead        255)
;;; Constant definition for every MidiShare error code
(defconstant MIDIerrSpace   -1 "too many applications")
(defconstant MIDIerrRefNu   -2 "bad reference number")
(defconstant MIDIerrBadType -3 "bad event type")
(defconstant MIDIerrIndex   -4 "bad index")
;;; Constant definition for the Macintosh serial ports
(defconstant ModemPort   0 "Macintosh modem port")
(defconstant PrinterPort 1 "Macintosh printer port")
;;; Constant definition for the synchronisation modes
(defconstant MidiExternalSync #x8000 
  "Bit-15 set for external synchronisation")
(defconstant MidiSyncAnyPort  #x4000
  "Bit-14 set for synchronisation on any port")
;;; Constant definition for SMPTE frame format
(defconstant smpte24fr 0 "24 frame/sec")
(defconstant smpte25fr 1 "25 frame/sec")
(defconstant smpte29fr 2 "29 frame/sec (30 drop frame)")
(defconstant smpte30fr 3 "30 frame/sec")
;;; Constant definition for MidiShare world changes
(defconstant MIDIOpenAppl     1 "application was opened")
(defconstant MIDICloseAppl    2 "application was closed")
(defconstant MIDIChgName      3 "application name was changed")
(defconstant MIDIChgConnect   4 "connection was changed")
(defconstant MIDIOpenModem    5 "Modem port was opened") ; obsolete
(defconstant MIDICloseModem   6 "Modem port was closed") ; obsolete
(defconstant MIDIOpenPrinter  7 "Printer port was opened")
(defconstant MIDIClosePrinter 8 "Printer port was closed")
(defconstant MIDISyncStart    9 "SMPTE synchronisation just start")
(defconstant MIDISyncStop    10 "SMPTE synchronisation just stop")
(defconstant MIDIChangeSync     10)
(defconstant MIDIOpenDriver     11)
(defconstant MIDICloseDriver    12)
(defconstant MIDIAddSlot        13)
(defconstant MIDIRemoveSlot     14)
(defconstant MIDIChgSlotConnect 15)
(defconstant MIDIChgSlotName    16)

;;; Functions common to every type of event

(defun link (e &optional (d nil d?))
  "read or set the link of an event"
  (if d?
    (setf (cffi:foreign-slot-value e 'tmidi-ev-1 'link) d)
    (cffi:foreign-slot-value e 'tmidi-ev-1 'link)))

(defun date (e &optional d)
  "read or set the date of an event"
  (if d
    (setf (cffi:foreign-slot-value e 'tmidi-ev-1 'date) d)
    (cffi:foreign-slot-value e 'tmidi-ev-1 'date)))

(defun evtype (e &optional v)
  "read.set the type of an event. Be careful"
  (if v
    (setf (cffi:foreign-slot-value e 'tmidi-ev-1 'ev-type) v)
    (cffi:foreign-slot-value e 'tmidi-ev-1 'ev-type)))

(defun ref (e &optional v)
  "read or set the reference number of an event"
  (if v
    (setf (cffi:foreign-slot-value e 'tmidi-ev-1 'ref-num) v)
    (cffi:foreign-slot-value e 'tmidi-ev-1 'ref-num)))

(defun port (e &optional v)
  "read or set the port number of an event"
  (if v
    (setf (cffi:foreign-slot-value e 'tmidi-ev-1 'port) v)
    (cffi:foreign-slot-value e 'tmidi-ev-1 'port)))

(defun chan (e &optional v)
  "read or set the chan number of an event"
  (if v
    (setf (cffi:foreign-slot-value e 'tmidi-ev-1 'chan) v)
    (cffi:foreign-slot-value e 'tmidi-ev-1 'chan)))

(defun field (e &optional f v)
  "give the number of fields or read/set a field of an event"
  (if f
    (if v
      (midi-set-field e f v)
      (midi-get-field e f))
    (midi-count-fields e)))

(defun fieldsList (e &optional (n 4))
  "collect all the fields of an event into a list"
  (let (l)
    (dotimes (i (min n (midi-count-fields e)))
      (push (midi-get-field e i) l))
    (nreverse l)))

;;; Specific to typeNote events

;; (defun pitch (e &optional (v nil vp))
;;   "read or set the pitch of an event"
;;   (if vp
;;       (setf (cffi:foreign-slot-value 
;;              (cffi:foreign-slot-value
;;               (cffi:foreign-slot-value e 'tmidi-ev-1 'info)
;;               'tmidi-ev-anonymous1648 'note)
;;              'tmidi-ev-anonymous1648-anonymous1622 'pitch)
;;             v)
;;       (cffi:foreign-slot-value 
;;        (cffi:foreign-slot-value
;;         (cffi:foreign-slot-value e 'tmidi-ev-1 'info)
;;         'tmidi-ev-anonymous1648 'note)
;;        'tmidi-ev-anonymous1648-anonymous1622 'pitch)))

(defun pitch (e &optional v)
  "read or set the pitch of an event"
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch)))

(defun vel (e &optional v)
  "read or set the velocity of an event"
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel)))

(defun dur (e &optional v)
  "read or set the duration of an event"
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'dur) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'dur)))

;;; Specific to other types of events

(defun linkSE (e &optional (d nil d?))
  "read or set the link of an SEXevent "
  (if d?
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'link-se) d)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'link-se)))

(defun linkST (e &optional (d nil d?))
  "read or set the link of an STevent "
  (if d?
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'link-st) d)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'link-st)))

(defun kpress (e &optional v)
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel)))

(defun ctrl (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun param (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun num (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun prefix (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun tempo (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun seconds (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun subframes (e &optional v)
  (if v
    (midi-set-field e 1 v)
    (midi-get-field e 1)))

(defun val (e &optional v)
  (if v
    (midi-set-field e 1 v)
    (midi-get-field e 1)))

(defun pgm (e &optional v)
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch)))

;(defun bend (e &optional v)
;  "read or set the bend value of an event"
;  (if v
;    (multiple-value-bind (ms7b ls7b) (floor (+ v 8192) 128)
;      (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) ls7b)
;      (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel) ms7b))
;    (- (+ (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) 
;          (* 128 (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel)))
;       8192)))
(defun bend (e &optional v)
  "read or set the bend value of an event"
  (if v
    (multiple-value-bind (ms7b ls7b) (floor v 128)
      (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) ls7b)
      (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel) ms7b))
    (+ (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) 
       (* 128 (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel)))
    ))

(defun clk (e &optional v)
  (if v
    (multiple-value-bind (ms7b ls7b) (floor (round (/ v 6)) 128)
      (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) ls7b)
      (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel) ms7b))
    (* 6 (+ (pitch e) (* 128 (vel e)))) ))

(defun song (e &optional v)
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch)))

(defun fields (e &optional v)
  (if v
    (let ((e e)) (mapc #'(lambda (f) (midi-add-field e f)) v))
    (let (l (e e))
      (dotimes (i (midi-count-fields e))
        (push (midi-get-field e i) l)) (nreverse l)) ))

(defun text (e &optional s)
  (if s
    (fields e (map 'list #'char-code s))
    ;;; replaced character by code-char, jb 29/05/2010
    (map 'string #'code-char (fields e))))



(defun fmsg (e &optional v)
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'pitch)))

(defun fcount (e &optional v)
  (if v
    (setf (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel) v)
    (%foreign-slot-value e 'tmidi-ev-1 'info 'note 'vel)))

(defun tsnum (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun tsdenom (e &optional v)
  (if v
    (midi-set-field e 1 v)
    (midi-get-field e 1)))

(defun tsclick (e &optional v)
  (if v
    (midi-set-field e 2 v)
    (midi-get-field e 2)))

(defun tsquarter (e &optional v)
  (if v
    (midi-set-field e 3 v)
    (midi-get-field e 3)))

(defun alteration (e &optional v)
  (if v
    (midi-set-field e 0 v)
    (midi-get-field e 0)))

(defun minor-scale (e &optional v)
  (if v
    (midi-set-field e 1 (if v 1 0))
    (= 1 (midi-get-field e 1))))

(defun info (e &optional d)
  "read or set the info of an event"
  (if d ;(setf (cffi:foreign-slot-ptr e 'tmidi-ev-1 'info) d)
    (error "info: Attempt to setf an aggregate slot (midiEv.info)")
    (cffi:foreign-slot-value e 'tmidi-ev-1 'info)))

;;;  Macros for accessing MidiShare Sequences data structures

(defun firstEv (s &optional (e nil e?))
  "read or set the first event of a sequence"
  (if e?
    (setf (cffi:foreign-slot-value s 'tmidi-seq-1 'first) e)
    (cffi:foreign-slot-value s 'tmidi-seq-1 'first)))

(defun lastEv (s &optional (e nil e?))
  "read or set the last event of a sequence"
  (if e?
    (setf (cffi:foreign-slot-value s 'tmidi-seq-1 'last) e)
    (cffi:foreign-slot-value s 'tmidi-seq-1 'last)))


;;; To Know about MidiShare and Active Sessions

(defun MidiShare ()
  "returns true if MidiShare is installed"
  (if (null *midishare*) 0 1))

(defun MidiGetVersion () 
  "Give MidiShare version as a fixnum. For example 131 as result, means : version 1.31"
  (midi-get-version))

(defun MidiCountAppls ()
  "Give the number of MidiShare applications currently opened"
  (midi-count-appls))

(defun MidiGetIndAppl (index)
  "Give the reference number of a MidiShare application from its index, a fixnum between 1 and (MidiCountAppls)"
  (midi-get-ind-appl index))

(defun MidiGetNamedAppl (name)
  "Give the reference number of a MidiShare application from its name"
  (cffi:with-foreign-string (s name)
    (midi-get-named-appl s)))

;;; To Open and Close a MidiShare session

(defun MidiOpen (name)
  "Open a new MidiShare application, with name name. Give a unique reference number."
  (cffi:with-foreign-string (s name)
    (midi-open s)))

(defun MidiClose (refNum)
  "Close an opened MidiShare application from its reference number"
  (midi-close refNum))

;;; To Configure a MidiShare session

(defun MidiGetName (refNum)
  "Give the name of a MidiShare application from its reference number"
  (let ((res (midi-get-name refNum)))
    (if (cffi:null-pointer-p res)
        nil
        (cffi:foreign-string-to-lisp res))))

(defun MidiSetName (refNum name)
  "Change the name of a MidiShare application"
  (cffi:with-foreign-string (s name)
    (midi-set-name refNum s)))

(defun MidiGetInfo (refNum)
  "Give the 32-bits user defined content of the info field of a MidiShare application. Analogous to window's refcon."
  (midi-get-info refNum))

(defun MidiSetInfo (refNum p)
  "Set the 32-bits user defined content of the info field of a MidiShare application. Analogous to window's refcon."
  (midi-set-info refNum p))

(defun MidiNewFilter ()
  "Returns a new filter"
  (midi-new-filter))

(defun MidiFreeFilter (f)
  "Delete a filter"
  (midi-free-filter f))

(defun MidiAcceptChan (f c s)
  "Change the chan state of a filter"
  (midi-accept-chan f c s))

(defun MidiAcceptType (f c s)
  "Change the type state of a filter"
  (midi-accept-type f c s))

(defun MidiAcceptPort (f c s)
  "Change the port state of a filter"
  (midi-accept-port f c s))

(defun MidiIsAcceptedChan (f c)
  "Returns the chan state of a filter"
  (midi-is-accepted-chan f c))

(defun MidiIsAcceptedType (f c)
  "Returns the type state of a filter"
  (midi-is-accepted-type f c))

(defun MidiIsAcceptedPort (f c)
  "Returns the port state of a filter"
  (midi-is-accepted-port f c))

(defun MidiGetFilter (refNum)
  "Give a pointer to the input filter record of a MidiShare application. Give NIL if no filter is installed"
  (midi-get-filter refNum))

(defun MidiSetFilter (refNum p)
  "Install an input filter. The argument p is a pointer to a filter record."
  (midi-set-filter refNum p))

(defun MidiGetRcvAlarm (refNum)
  "Get the adress of the receive alarm"
  (midi-get-rcv-alarm refNum))

(defun MidiSetRcvAlarm (refNum alarm)
  "Install a receive alarm"
  (midi-set-rcv-alarm refNum alarm))

(defun MidiGetApplAlarm (refNum)
  "Get the adress of the context alarm"
  (midi-get-appl-alarm refNum))

(defun MidiSetApplAlarm (refNum alarm)
  "Install a context alarm"
  (midi-set-appl-alarm refNum alarm))

;;; To Manage MidiShare IAC and Midi Ports

(defun MidiConnect (src dst s)
  "Connect or disconnect two MidiShare applications"
  (midi-connect src dst s))

(defun MidiIsConnected (src dst)
  "Test if two MidiShare applications are connected"
  (midi-is-connected src dst))

(defun MidiGetPortState (port)
  "Give the state : open or closed, of a MidiPort"
  (midi-get-port-state port))

(defun MidiSetPortState (port state)
  "Open or close a MidiPort"
  (midi-set-port-state port state))

;;; To Manage MidiShare events

(defun MidiFreeSpace ()
  "Amount of free MidiShare cells"
  (midi-free-space))

(defun MidiNewEv (typeNum)
  "Allocate a new MidiEvent"
  (midi-new-ev typeNum))

(defun MidiCopyEv (ev)
  "Duplicate a MidiEvent"
  (midi-copy-ev ev))

(defun MidiFreeEv (ev)
  "Free a MidiEvent"
  (midi-free-ev ev))

(defun MidiSetField (ev f v)
  "Set a field of a MidiEvent"
  (midi-set-field ev f v))

(defun MidiGetField (ev f)
  "Get a field of a MidiEvent"
  (midi-get-field ev f))

(defun MidiAddField (ev val)
  "Append a field to a MidiEvent (only for sysex and stream)"
  (midi-add-field ev val))

(defun MidiCountFields (ev)
  "The number of fields of a MidiEvent"
  (midi-count-fields ev))

;;; To Manage MidiShare Sequences

(defun MidiNewSeq ()
  "Allocate an empty sequence"
  (midi-new-seq))

(defun MidiAddSeq (seq ev)
  "Add an event to a sequence"
  (midi-add-seq seq ev))

(defun MidiFreeSeq (seq)
  "Free a sequence and its content"
  (midi-free-seq seq))

(defun MidiClearSeq (seq)
  "Free only the content of a sequence. The sequence become empty"
  (midi-clear-seq seq))

(defun MidiApplySeq (seq proc)
  "Call a function for every events of a sequence"
  (midi-apply-seq seq proc))

;;; MidiShare Time

(defun MidiGetTime ()
  "give the current time"
  (midi-get-time))

;;; To Send MidiShare events

(defun MidiSendIm (refNum ev)
  "send an event now"
  (midi-send-im refNum ev))

(defun MidiSend (refNum ev)
  "send an event using its own date"
  (midi-send refNum ev))

(defun MidiSendAt (refNum ev date)
  "send an event at date <date>"
  (midi-send-at refNum ev date))

;;;  To Receive MidiShare Events

(defun MidiCountEvs (refNum)
  "Give the number of events waiting in the reception fifo"
  (midi-count-evs refNum))

(defun MidiGetEv (refNum)
  "Read an event from the reception fifo"
  (midi-get-ev refNum))

(defun MidiAvailEv (refNum)
  "Get a pointer to the first event in the fifo without removing it"
  (midi-avail-ev refNum))

(defun MidiFlushEvs (refNum)
  "Delete all the events waiting in the fifo"
  (midi-flush-evs refNum))

;;; To access shared data

(defun MidiReadSync (adrMem)
  "Read and clear a memory address (not-interruptible)"
  (midi-read-sync adrMem))

(defun MidiWriteSync (adrMem val)
  "write if nil into a memory address (not-interruptible)"
  (midi-write-sync adrMem val))

;;; Realtime Tasks

(defun MidiCall (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>"
  (midi-call proc date refNum arg1 arg2 arg3))

(defun MidiTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeProcess event"
  (midi-task proc date refNum arg1 arg2 arg3))

(defun MidiDTask (proc date refNum arg1 arg2 arg3)
  "Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. Return a pointer to the corresponding typeDProcess event"
  (midi-dtask proc date refNum arg1 arg2 arg3))

(defun MidiForgetTaskHdl (thdl)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  (midi-forget-task thdl))

(defun MidiForgetTask (ev)
  "Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  ;; FIX
  (progn ;ccl:without-interrupts 
   (cffi:with-foreign-object (taskptr :pointer)
     (setf (cffi:mem-ref taskptr :pointer) ev) 
     (midi-forget-task taskptr))))

(defun MidiCountDTasks (refNum)
  "Give the number of typeDProcess events waiting"
  (midi-count-dtasks refNum))

(defun MidiFlushDTasks (refNum)
  "Remove all the typeDProcess events waiting"
  (midi-flush-dtasks refNum))

(defun MidiExec1DTask (refNum)
  "Call the next typeDProcess waiting"
  (midi-exec1dtask refNum))

;;; Low Level MidiShare Memory Management

(defun MidiNewCell ()
  "Allocate a basic Cell"
  (midi-new-cell))

(defun MidiFreeCell (cell)
  "Delete a basic Cell"
  (midi-free-cell cell))

(defun MidiTotalSpace ()
  "Total amount of Cells"
  (midi-total-space))

(defun MidiGrowSpace (n)
  "Total amount of Cells"
  (midi-grow-space n))

;;; SMPTE Synchronisation functions

(defun MidiGetSyncInfo (syncInfo)
  "Fill syncInfo with current synchronisation informations"
  (midi-get-sync-info syncInfo))

(defun MidiSetSyncMode (mode)
  "set the MidiShare synchroniation mode"
  (midi-set-sync-mode mode))

(defun MidiGetExtTime ()
  "give the current external time"
  (midi-get-ext-time))

(defun MidiInt2ExtTime (time)
  "convert internal time to external time"
  (midi-int2ext-time time))

(defun MidiExt2IntTime (time)
  "convert internal time to external time"
  (midi-ext2int-time time))

(defun MidiTime2Smpte (time format smpteLocation)
  "convert time to Smpte location"
  (midi-time2smpte time format smpteLocation))

(defun MidiSmpte2Time (smpteLocation)
  "convert time to Smpte location"
  (midi-smpte2time smpteLocation))

;;; Drivers functions

(defun MidiCountDrivers ()
  "number of opened drivers"
  (midi-count-drivers))

(defun MidiGetIndDriver (index)
  "Give the reference number of a MidiShare driver from its index, a fixnum"
  (midi-get-ind-driver index))

(defun MidiGetDriverInfos (refNum info)
  "Give information about a driver"
  (midi-get-driver-infos refNum info))

(defun MidiGetIndSlot (refNum index)
  "Give the reference number of a driver slot from its order number."
  (midi-get-ind-slot refNum index))

(defun MidiGetSlotInfos (slotRefNum info)
  "Give information about a slot"
  (midi-get-slot-infos slotRefNum info))

(defun MidiConnectSlot (port slotRefNum state)
  "Make or remove a connection between a slot and a MidiShare logical port"
  (midi-connect-slot port slotRefNum state))

(defun MidiIsSlotConnected  (port slotRefNum)
  "Test a connection between a slot and a MidiShare logical port"
  (midi-is-slot-connected port slotRefNum))

(defun MidiNewSmpteLocation ()
  (cffi:foreign-alloc 'tsmpte-location))

(defun MidiFreeSmpteLocation (location)
  (cffi:foreign-free location))

(defun MidiNewSyncInfo ()
  (cffi:foreign-alloc 'tsync-info))

(defun MidiFreeSyncInfo (location)
  (cffi:foreign-free location))

;;; HKT: added these to api

(defun MidiNewMidiDriverInfos () 
  ;; (name driver-name 31) (version :short) (slots :short) (reserved :long 1)
  (cffi:foreign-alloc 'tdriver-infos-1))

(defun MidiFreeMidiDriverInfos (ptr) 
  (cffi:foreign-free ptr))

(defun md-name (ptr)
  (cffi:foreign-string-to-lisp 
   (cffi:foreign-slot-value ptr 'tdriver-infos-1 'name)))

(defun md-version (ptr)
  (cffi:foreign-slot-value ptr 'tdriver-infos-1 'version))

(defun md-slots (ptr)
  (cffi:foreign-slot-value ptr 'tdriver-infos-1 'slots))

(defun MidiNewMidiSlotInfos ()
  ;;slot-name direction (cnx :char 31) (reserved :long 1)
  (cffi:foreign-alloc 'tslot-infos-1))

(defun MidiFreeMidiSlotInfos (ptr)
  (cffi:foreign-free ptr))

(defun ms-name (ptr)
  (cffi:foreign-string-to-lisp 
   (cffi:foreign-slot-value ptr 'tslot-infos-1 'name)))

(defun ms-direction (ptr)
  (cffi:foreign-slot-value ptr 'tslot-infos-1 'direction))

(defun ms-cnx (ptr)
  (cffi:foreign-string-to-lisp 
   (cffi:foreign-slot-value ptr 'tslot-infos-1 'cnx)))

(eval-when (:load-toplevel :execute)
  (pushnew :midishare *features*))

;;;
;;; Midishare receiving via async alarm callback
;;;

(defvar *midishare-recv* nil)

(cffi:defcallback midishare-callback :void ((refnum :short))
  (restart-case
      (handler-bind ((error
                      #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'callback-error-exit))))
        (do ((ev (MidiGetEv refnum)
                 (MidiGetEv refnum)))
            ((or (not *midishare-recv*)
                 (cffi:null-pointer-p ev))
             (values))
          (funcall *midishare-recv* ev)))
    (callback-error-exit () 
      (format
       t "~&Caught error under Midishare callback, aborting receive!~%")
      (MidiFlushEvs refnum)
      (MidiSetRcvAlarm refnum (cffi:null-pointer))
      (values))))

(defun midishare-receive-stop (refnum)
  (unless *midishare-recv*
    (error "midishare-receive-stop: not receiving."))
  (MidiSetRcvAlarm refnum (cffi:null-pointer))
  (MidiFlushEvs refnum)
  (setq *midishare-recv* nil)
  (values))

(defun midishare-receive (refnum hook)
  (when *midishare-recv*
    (error "midishare-receive: already receiving."))
  (setq *midishare-recv* hook)
  (MidiSetRcvAlarm refnum (cffi:callback midishare-callback))
  (values))

;;; eof

