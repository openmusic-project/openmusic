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
(AddClass2Pack '(note chord chord-seq multi-seq voice poly) 
               *scorepackage* 
               :position (list (om-make-point  8 133) (om-make-point 55 133) (om-make-point 273 131) (om-make-point 227 285) (om-make-point 290 285) 
                               (om-make-point 165 285) (om-make-point 417 131) (om-make-point 108 133) (om-make-point 433 285) (om-make-point 329 131)))


(defvar *scorefun-package*  (omNG-protect-object (omNG-make-new-package "Score Functions")))

(AddGenFun2Pack '(OMquantify object-dur 
                             true-durations
                             align-chords concat get-chords get-measures 
                             mask merger select
                             maquette2obj) *scorefun-package*)

(AddPackage2Pack *scorefun-package* *scorepackage* :protect t)



;-------------------
;TREES
;-------------------

(defvar *trees-package* (omNG-protect-object (omNG-make-new-package "Trees")))
(AddPackage2Pack *trees-package* *scorepackage* :protect t)

(AddGenFun2Pack  '(pulsemaker maketreegroups
                   tree2ratio mktree
                   reducetree tietree remove-rests invert-rhythm reversetree rotatetree filtertree select-tree
                   subst-rhythm 
                   group-pulses n-pulses get-signatures get-pulse-places get-rest-places
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

(AddGenFun2Pack  '(add-extra add-extra-list get-extras delete-extras remove-extras) *extrapackage*)

(addPackage2Pack *extrapackage* *scorepackage*)

;-------------------
;IMPORT/EXPORT
;-------------------

(defvar *iopackage*  (omNG-protect-object (omNG-make-new-package "Import/Export")))

(addPackage2Pack *iopackage* *scorepackage*)

(AddGenFun2Pack '(save-as-etf export-musicxml import-musicxml import-bach export-bach) *iopackage*)






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


