;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;;; MIDI package

(in-package :om)

(defvar *midipackage* (omNG-protect-object (omNG-make-new-package "Midi")))

(defvar *midixtract-package* (omNG-protect-object (omNG-make-new-package "Inspect/Extract")))
(AddPackage2Pack *midixtract-package* *midipackage* :protect t)

(defvar *midiprocess-package* (omNG-protect-object (omNG-make-new-package "Processing")))
(AddPackage2Pack *midiprocess-package* *midipackage* :protect t)

(defvar *gmidi-package* (omNG-protect-object (omNG-make-new-package "General MIDI")))
(AddPackage2Pack *gmidi-package* *midipackage* :protect t)

(defvar *midisend-package* (omNG-protect-object (omNG-make-new-package "Send/Receive")))
(AddPackage2Pack *midisend-package* *midipackage* :protect t)

(defvar *midifilter-package* (omNG-protect-object (omNG-make-new-package "Filters")))
(AddPackage2Pack *midifilter-package* *midipackage* :protect t)


(AddClass2Pack '(MidiFile) *midipackage* :position (list (om-make-point 75 295) (om-make-point 75 115)))
(AddClass2Pack '(MidiEvent EventMidi-seq MidiControl Tempo-map Midi-Mix-Console) *midipackage* :position (list (om-make-point 75 295) (om-make-point 75 115)))


(AddGenFun2Pack  '(pgmout ctrlchg volume sysex midi-reset midi-allnotesoff midi-in pitchbend pitchwheel mc-to-pitchwheel set-port) *midisend-package*)
(AddGenFun2Pack  '(test-date test-channel test-type test-track test-port midievent-filter) *midifilter-package*)
(AddGenFun2Pack  '(mesure-time cseq+tempo->voice) *scorepackage*)
(AddGenFun2Pack '(mf-info save-as-midi) *midipackage*)
(AddGenFun2Pack  '(get-midievents get-tempomap get-mf-lyrics get-midi-notes get-continuous-ctrl) *midixtract-package*)
(AddGenFun2Pack  '(create-midiseq temporal-sort separate-channels me-textinfo save-as-midi) *midiprocess-package*)
(AddGenFun2Pack  '(gm-program gm-drumnote control-change) *gmidi-package*)


;-------------------
; OSC (add to Basic Tools Menu)
;-------------------

(defvar *oscpackage* (omNG-protect-object (omNG-make-new-package "OSC")))
(addPackage2Pack *oscpackage* *basic-package*)
(AddClass2Pack '(OSCEvent) *oscpackage*)
(AddGenFun2Pack  '(osc-send osc-receive) *oscpackage*)
(addPackage2Pack *oscpackage* *om-package-tree*)

; removed from ScoreOMInterface.lisp
;;;(omNG-add-element *package-music* *midipackage*)
(addPackage2Pack *midipackage* *om-package-tree*)

;;; from music package
(AddGenFun2Pack '(save-as-midi) *iopackage*)


;;;================== 
;;; REFERENCE
;;;==================

(add-ref-section (gen-ref-entries *scorepackage*))  ;; update
(add-ref-section (gen-ref-entries *midipackage*))
(add-ref-section (gen-ref-entries *oscpackage*))

;(add-ref-section 
; '("MIDI" (("Objects" (MidiFile MidiEvent EventMidi-seq MidiControl Tempo-map Midi-Mix-Console))
;           ("Inspect/Extract" (get-midievents get-tempomap get-mf-lyrics get-midi-notes get-continuous-ctrl))
;           ("Processing" (create-midiseq temporal-sort separate-channels save-as-midi))
;           ("General MIDI" (ms-event control-change gm-program gm-drumnote))
;           ("Filters" (test-date test-channel test-type test-track test-port midievent-filter))
;           ("Send" (midi-o pitchwheel pitchbend pgmout ctrlchg volume sysex midi-reset)))))
