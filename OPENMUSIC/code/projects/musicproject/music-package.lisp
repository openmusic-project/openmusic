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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================


(in-package :om)

;==============================================================================
;==============================================================================
;==============================================================================

(defvar *scorepackage*  (omNG-protect-object (omNG-make-new-package "Score")))


(addPackage2Pack *scorepackage* *om-package-tree*)


(push (omNG-make-new-boxalias (omNG-make-new-boxclass 'simple-score-element (om-make-point 38 2))
                              (om-make-point 38 2) "simple-score-element" t)
      (aliasclasses *scorepackage*))

(push (omNG-make-new-boxalias (omNG-make-new-boxclass 'sequence* (om-make-point 435 190))
                              (om-make-point 435 190) "sequence*" t)
      (aliasclasses *scorepackage*))
(push (omNG-make-new-boxalias (omNG-make-new-boxclass 'metric-sequence (om-make-point 199 182))
                              (om-make-point 199 182) "metric-sequence" t)
      (aliasclasses *scorepackage*))
(push (omNG-make-new-boxalias (omNG-make-new-boxclass 'superposition (om-make-point 395 5))
                              (om-make-point 319 5) "superposition" t)
      (aliasclasses *scorepackage*))

;;; !!! Midifile? aussi dans MidiPackage...
(AddClass2Pack '(note chord chord-seq multi-seq voice poly tracks) 
               *scorepackage* 
               :position (list (om-make-point  8 133) (om-make-point 55 133) (om-make-point 273 131) (om-make-point 227 285) (om-make-point 290 285) 
                               (om-make-point 165 285) (om-make-point 417 131) (om-make-point 108 133) (om-make-point 433 285) (om-make-point 329 131)))


(defvar *scorefun-package*  (omNG-protect-object (omNG-make-new-package "Score Functions")))

(AddGenFun2Pack '(object-dur 
                  true-durations
                  align-chords concat get-chords get-measures 
                  set-obj-pitch set-obj-vel set-obj-chan set-obj-port set-obj-tempo
                             ;mask 
                  merger select chord-filter chord-band-filter split-voices 
                  voice->voices concat-score-objs maquette2obj) 
                *scorefun-package*)

(AddPackage2Pack *scorefun-package* *scorepackage* :protect t)

;-------------------
;QUANTIFICATION & ANALYSIS
;-------------------

(defvar *quantification-package* (omNG-protect-object (omNG-make-new-package "Quantification")))
(AddPackage2Pack *quantification-package* *scorepackage* :protect t)

(AddGenFun2Pack '(OMquantify 
                  omg-quantify
                  ;gkant 
                  set-kant-analysis-segs
                  kant-voices
                  concatenate-kant-voices
                  remove-analysis ) *quantification-package*)

;-------------------
;TREES
;-------------------

(defvar *trees-package* (omNG-protect-object (omNG-make-new-package "Trees")))
(AddPackage2Pack *trees-package* *scorepackage* :protect t)

(AddGenFun2Pack  '(pulsemaker maketreegroups
                   tree2ratio mktree
                   reduce-rt tietree remove-rests invert-rhythm reversetree 
                   rotatetree rotateprops filtertree select-tree subst-rhythm remove-pulses
                   group-pulses n-pulses get-time-sig get-pulse-places get-rest-places
                   remove-tree-graces
                   ) *trees-package*)


;-------------------
;CONVERSIONS
;-------------------

(defvar *conversion-package* (omNG-protect-object (omNG-make-new-package "Conversions")))
(addPackage2Pack *conversion-package* *scorepackage*)

(addGenFun2Pack '(approx-m mc->f mc->n f->mc n->mc) *conversion-package*)


;-------------------
;EXTRAS
;-------------------

(defvar *extrapackage* (omNG-protect-object (omNG-make-new-package "Extras")))

(AddClass2Pack '(text-extra vel-extra head-extra line-extra) *extrapackage* )

(AddGenFun2Pack '(add-extra add-extra-list get-extras delete-extras remove-extras
                            put-extra-text  put-extra-dyn index-voice-pulses) *extrapackage*)

(addPackage2Pack *extrapackage* *scorepackage*)

;-------------------
;IMPORT/EXPORT
;-------------------

(defvar *iopackage*  (omNG-protect-object (omNG-make-new-package "Import/Export")))

(addPackage2Pack *iopackage* *scorepackage*)

(AddGenFun2Pack '(save-as-etf import-musicxml export-musicxml import-bach export-bach import-nap export-nap) *iopackage*)



;;;================== 
;;; REFERENCE
;;;==================

(add-ref-section (gen-ref-entries *basic-package*))  ;; update
(add-ref-section (gen-ref-entries *scorepackage*))

;(add-ref-section '("Basic Tools" (("OSC" (OSCevent osc-send osc-receive)))))

;(add-ref-section 
; '("SCORE" (("Objects" (note chord chord-seq multi-seq voice poly))
;            ("General Functions" (OMquantify true-durations align-chords concat get-chords get-measures 
;                                             mask merger select maquette2obj  mesure-time cseq+tempo->voice))
;            ("Conversions" (approx-m mc->f mc->n f->mc n->mc))
;            ("Trees" (pulsemaker maketreegroups tree2ratio mktree
;                                 reducetree tietree  remove-rests invert-rhythm reversetree filtertree rotatetree
;                                 n-pulses get-signatures get-pulse-places get-rest-places subst-rhythm group-pulses 
;                                 ))
;            ("Extras" ( text-extra vel-extra head-extra line-extra 
;                       add-extra add-extra-list get-extras delete-extras remove-extras))
;            ("Import/Export" (save-as-midi save-as-etf export-musicxml import-musicxml)))))


