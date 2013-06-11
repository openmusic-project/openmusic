;;; **********************************************************************
;;; Copyright (C) 2005 Heinrich Taube, <taube (at) uiuc (dot) edu>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the Lisp Lesser Gnu Public License.
;;; See http://www.cliki.net/LLGPL for the text of this agreement.
;;; **********************************************************************

;;; $Name: rel-1_0_2 $
;;; $Revision: 1.1.1.1 $
;;; $Date: 2005/12/09 16:11:53 $

#-:midishare
(error "Attempt to load or compile Player.lisp without Midishare loaded.")

;;; A CFFI interface to Midishare. Should run in most Common Lisp
;;; implementations on Linux, OS X and Windows. For information about
;;; CFFI see http://common-lisp.net/project/cffi/

(in-package :cl-user)

(defvar *libplayer*
  #+win32
  "/WINDOWS/system32/player32.dll"
  #+(or darwin macos macosx) 
  "/System/Library/Frameworks/Player.framework/Player"
  #+(or linux (and clisp unix (not macos)))
  "/usr/lib/libPlayer.so")


(in-package :midishare)

  
 (defvar *player-framework* nil)
  
 (defun player-framework ()
    (or *player-framework*
        (setq *player-framework*
              (if (probe-file cl-user::*libplayer*)
                  (progn (cffi:load-foreign-library cl-user::*libplayer*)
           t)))))
  
  
 (defun close-player-framework ()
    (setq *player-framework* nil))
  
 

;; Player state   
(defconstant kIdle       0)
(defconstant kPause  	 1)
(defconstant kRecording  2)
(defconstant kPlaying    3)
(defconstant kWaiting    4)

;; Tracks state   
(defconstant kMaxTrack	256)
(defconstant kMuteOn    1)
(defconstant kMuteOff   0)
(defconstant kSoloOn    1)
(defconstant kSoloOff   0)
(defconstant kMute      0)
(defconstant kSolo      1)

;; Recording management  
(defconstant kNoTrack	 -1)
(defconstant kEraseMode  1)
(defconstant kMixMode 	 0)

;; Loop  management  
(defconstant kLoopOn  	0)
(defconstant kLoopOff 	1)

;; Step Playing  
(defconstant kStepPlay  1)
(defconstant kStepMute  0)

;; Synchronisation  
(defconstant kInternalSync  0)
(defconstant kClockSync     1)
(defconstant kSMPTESync     2)
(defconstant kExternalSync  3)
(defconstant kNoSyncOut	    0)
(defconstant kClockSyncOut  1)

;; MIDIfile  
(defconstant midifile0  0)
(defconstant midifile1  1)
(defconstant midifile2  2)
(defconstant TicksPerQuarterNote  0)
(defconstant Smpte24              24)
(defconstant Smpte25              25)
(defconstant Smpte29              29)
(defconstant Smpte30              30)
  
;; Errors  :  for MidiFile
(defconstant noErr               0)   ; no error
(defconstant ErrOpen             1)   ; file open error       
(defconstant ErrRead             2)   ; file read error       
(defconstant ErrWrite            3)   ; file write error      
(defconstant ErrVol              4)   ; Volume error          
(defconstant ErrGetInfo          5)   ; GetFInfo error        
(defconstant ErrSetInfo          6)   ; SetFInfo error        
(defconstant ErrMidiFileFormat   7)   ; bad MidiFile format   

;; Errors  : for the player
(defconstant PLAYERnoErr         -1)  ; No error            
(defconstant PLAYERerrAppl       -2)  ; Unable to open MidiShare app  
(defconstant PLAYERerrEvent      -3)  ; No more MidiShare Memory
(defconstant PLAYERerrMemory     -4)  ; No more Mac Memory
(defconstant PLAYERerrSequencer  -5)  ; Sequencer error

;; Record for position management
(cffi:defcstruct Pos
  (bar  :short)  
  (beat :short)     
  (unit :short))

#-cffi-new (cffi:define-foreign-type PosPtr () ':pointer)
#+cffi-new (cffi:defctype PosPtr :pointer)
 
(defun MidiNewPos ()
  (cffi:foreign-alloc 'Pos))

(defun MidiFreePos (pos)
  (cffi:foreign-free pos))

(defun p-bar (e &optional (d nil d?)) 
  (if d? (setf (cffi:foreign-slot-value e 'Pos 'bar) d)
      (cffi:foreign-slot-value e 'Pos 'bar)))
  
(defun p-beat (e &optional (d nil d?)) 
  (if d? (setf (cffi:foreign-slot-value e 'Pos 'beat) d)
      (cffi:foreign-slot-value e 'Pos 'beat)))
  
(defun p-unit (e &optional (d nil d?)) 
  (if d? (setf (cffi:foreign-slot-value e 'Pos 'unit) d)
      (cffi:foreign-slot-value e 'Pos 'unit)))
  
;; Record for state management
  23529
(cffi:defcstruct PlayerState
  (date      :long)
  (tempo     :long)
  (tsnum     :short)
  (tsdenom   :short)
  (tsclick   :short)
  (tsquarter :short)
  (bar       :short)   
  (beat      :short)     
  (unit      :short)
  (state     :short)
  (syncin    :short)
  (syncout   :short))

#-cffi-new(cffi:define-foreign-type PlayerStatePtr () ':pointer)
#+cffi-new(cffi:defctype PlayerStatePtr :pointer)
  
(defun MidiNewPlayerState ()
  (cffi:foreign-alloc 'PlayerState))

(defun MidiFreePlayerState (state)
  (cffi:foreign-free state))

(defun s-bar (e ) 
  (cffi:foreign-slot-value e 'PlayerState 'bar))
  
(defun s-beat (e) 
  (cffi:foreign-slot-value e 'PlayerState 'beat))
  
(defun s-unit (e) 
  (cffi:foreign-slot-value e 'PlayerState 'unit))
  
(defun s-date (e)  
  (cffi:foreign-slot-value e 'PlayerState 'date))
  
(defun s-tempo (e)  
  (cffi:foreign-slot-value e 'PlayerState 'tempo))
  
(defun s-num (e)  
  (cffi:foreign-slot-value e 'PlayerState 'tsnum))
  
(defun s-denom (e)  
  (cffi:foreign-slot-value e 'PlayerState 'tsdenom))
  
(defun s-click (e)  
  (cffi:foreign-slot-value e 'PlayerState 'tsclick))
  
(defun s-quarter (e)  
  (cffi:foreign-slot-value e 'PlayerState 'tsquarter))
  
(defun s-state (e) 
  (cffi:foreign-slot-value e 'PlayerState 'state))

(defun s-syncin (e)  
  (cffi:foreign-slot-value e 'PlayerState 'syncin))
  
(defun s-syncout (e)
  (cffi:foreign-slot-value e 'PlayerState 'syncout))
  
;; Record for MidiFile
(cffi:defcstruct MidiFileInfos
  (format  :long)     
  (timedef :long)   
  (clicks  :long)      
  (tracks  :long))

#-cffi-new(cffi:define-foreign-type MidiFileInfosPtr () ':pointer)
#+cffi-new(cffi:defctype MidiFileInfosPtr :pointer)
    
(defun MidiNewMidiFileInfos ()
  (cffi:foreign-alloc 'MidiFileInfos))

(defun MidiFreeMidiFileInfos (info)
  (cffi:foreign-free info))

(defun  mf-format (e &optional (d nil d?))  
  (if d? (setf (cffi:foreign-slot-value e 'MidiFileInfos 'format) d)
      (cffi:foreign-slot-value e 'MidiFileInfos 'format)))
    
(defun mf-timedef (e &optional (d nil d?))  
  (if d? (setf (cffi:foreign-slot-value e 'MidiFileInfos 'timedef) d)
      (cffi:foreign-slot-value e 'MidiFileInfos 'timedef)))
    
(defun mf-clicks (e &optional (d nil d?)) 
  (if d? (setf (cffi:foreign-slot-value e 'MidiFileInfos 'clicks) d)
      (cffi:foreign-slot-value e 'MidiFileInfos 'clicks)))
    
(defun mf-tracks (e  &optional (d nil d?))
  (if d? (setf (cffi:foreign-slot-value e 'MidiFileInfos 'tracks) d)
    (cffi:foreign-slot-value e 'MidiFileInfos 'tracks)))
  
;; Interface to C entry points
  
(cffi:defcfun "Version" :short)
(cffi:defcfun ("OpenPlayer" open_player) :short (name :pointer))
(cffi:defcfun "ClosePlayer" :void  (refnum :short))

;(version)
  
;; Transport control
(cffi:defcfun "StartPlayer" :void  (refnum :short))
(cffi:defcfun "ContPlayer" :void  (refnum :short))
(cffi:defcfun "StopPlayer" :void  (refnum :short))
(cffi:defcfun "PausePlayer" :void  (refnum :short))
  
;; Record management
(cffi:defcfun "SetRecordModePlayer" :void  (refnum :short)
                   (state :short))
(cffi:defcfun "RecordPlayer" :void (refnum :short) (tracknum :short))
(cffi:defcfun "SetRecordFilterPlayer" :void
  (refnum :short) (filter midi-filter-ptr))
  
;; Position management
(cffi:defcfun "SetPosBBUPlayer" :void (refnum :short) (pos PosPtr))
(cffi:defcfun "SetPosMsPlayer" :void (refnum :short)  (date_ms :long))
  
;; Loop management
(cffi:defcfun "SetLoopPlayer" :void (refnum :short) (state :short))
(cffi:defcfun "SetLoopStartBBUPlayer" :long (refnum :short) (pos PosPtr))
(cffi:defcfun "SetLoopEndBBUPlayer" :long  (refnum :short) (pos PosPtr))
(cffi:defcfun "SetLoopStartMsPlayer" :long 
  (refnum :short) (date_ms :long))
(cffi:defcfun "SetLoopEndMsPlayer" :long (refnum :short) (date_ms :long))
  
;; Synchronisation management
(cffi:defcfun "SetSynchroInPlayer" :void (refnum :short) (state :short))
(cffi:defcfun "SetSynchroOutPlayer" :void (refnum :short) (state :short))
(cffi:defcfun "SetSMPTEOffsetPlayer" :void 
  (refnum :short) (smptepos smpte-loc-ptr))
(cffi:defcfun "SetTempoPlayer" :void (refnum :short) (tempo :long))
  
;; State management
(cffi:defcfun "GetStatePlayer" :void 
  (refnum :short) (state PlayerStatePtr))
(cffi:defcfun "GetEndScorePlayer" :void  
  (refnum :short) (playerstate PlayerStatePtr))

;; Step playing 
(cffi:defcfun "ForwardStepPlayer" :void  (refnum :short) (flag :short))
(cffi:defcfun "BackwardStepPlayer" :void (refnum :short) (flag :short))
  
;; Tracks management
(cffi:defcfun "GetAllTrackPlayer" midi-seq-ptr (refnum :short))
(cffi:defcfun "GetTrackPlayer" midi-seq-ptr 
  (refnum :short) (tracknum :short))
(cffi:defcfun "SetTrackPlayer" :long  
  (refnum :short) (tracknum :short) (seq midi-seq-ptr))
(cffi:defcfun "SetAllTrackPlayer" :long   
  (refnum :short) (seq :pointer) (ticks_per_quarter :long))
(cffi:defcfun "SetParamPlayer" :void  
  (refnum :short) (tracknum :short) (param :short) (value :short))
(cffi:defcfun "GetParamPlayer" :short 
  (refnum :short) (tracknum :short) (param :short))
(cffi:defcfun "InsertAllTrackPlayer" :long  
  (refnum :short) (seq midi-seq-ptr))
(cffi:defcfun "InsertTrackPlayer" :long  
  (refnum :short) (tracknum :short) (seq midi-seq-ptr))
   
;; Midifile management

(cffi:defcfun ("MidiFileSave" midi_file_save) :long  
  (name :pointer) (seq midi-seq-ptr) (infos MidiFileInfosPtr))

(cffi:defcfun ("MidiFileLoad" midi_file_load) :long  
  (name :pointer) (seq midi-seq-ptr) (infos MidiFileInfosPtr))

(defun MidiFileSave (name seq infos)
  (cffi:with-foreign-string (s name)
    (midi_file_save s seq infos)))

(defun MidiFileLoad (name seq info)
  (cffi:with-foreign-string (s name)
    (midi_file_load s seq info)))

(defun OpenPlayer (name)
  (cffi:with-foreign-string (s name)
    (multiple-value-bind (a b)
      (floor (open_player s) #x10000)
      a
      b)))



(defun open-player (name)  
  (OpenPlayer name))

(defun midi-file-load (name seq info) 
  (MidiFileLoad name seq info))
    
(defun midi-file-save (name seq info)  
  (MidiFileSave name seq info))

(eval-when (:load-toplevel :execute)
  (export '(kIdle kPause kRecording kPlaying kWaiting kMaxTrack 
            kMuteOn kMuteOff kSoloOn kSoloOff kMute kSolo
            kNoTrack kEraseMode kMixMode kLoopOn kLoopOff kStepPlay
            kStepMute kInternalSync kClockSync kSMPTESync
            kExternalSync kNoSyncOut kClockSyncOut midifile0
            midifile1 midifile2 TicksPerQuarterNote Smpte24 Smpte25
            Smpte29 Smpte30 noErr ErrOpen ErrRead ErrWrite ErrVol
            ErrGetInfo ErrSetInfo ErrMidiFileFormat PLAYERnoErr
            PLAYERerrAppl PLAYERerrEvent PLAYERerrMemory
            PLAYERerrSequencer player-framework p-bar p-beat p-unit
            s-bar s-beat s-unit s-date s-tempo s-num s-denom
            s-click s-quarter s-state s-syncin s-syncout mf-format
            mf-timedef mf-clicks mf-tracks OpenPlayer ClosePlayer
            open-player StartPlayer ContPlayer StopPlayer
            PausePlayer SetRecordModePlayer RecordPlayer
            SetRecordFilterPlayer SetPosBBUPlayer SetPosMsPlayer
            SetLoopPlayer SetLoopStartBBUPlayer SetLoopEndBBUPlayer
            SetLoopStartMsPlayer SetLoopEndMsPlayer SetSynchroInPlayer
            SetSynchroOutPlayer SetSMPTEOffsetPlayer SetTempoPlayer
            GetStatePlayer GetEndScorePlayer ForwardStepPlayer
            BackwardStepPlayer GetAllTrackPlayer GetTrackPlayer
            SetTrackPlayer SetAllTrackPlayer SetParamPlayer
            InsertAllTrackPlayer InsertTrackPlayer MidiFileSave
            MidiFileLoad midi-file-save midi-file-load
            MidiNewMidiFileInfos MidiFreeMidiFileInfos
            MidiNewPlayerState MidiFreePlayerState MidiNewPos
            MidiFreePos Version)
          :midishare)
  (pushnew :midishare-player *features*)
  )

;;; eof
