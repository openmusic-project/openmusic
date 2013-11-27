(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *LZ-lib-files* nil)
(setf *LZ-lib-files* (list (om::om-relative-path '("sources") "lz")
                           (om::om-relative-path '("sources") "Kant")))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(if (om-standalone-p)
    (mapc #'load *LZ-lib-files*)
  (mapc #'compile&load *LZ-lib-files*))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '( ("LZ" nil nil (LZify LZGenerate subLZ LZprint LZprintreconstr LZsize LZlength LZuntree) nil)
         ("PST" nil nil (PSTify PSTGenerate PSTprint PSTprintreconstr) nil)
         ("Cross-Alphabet Sequences" nil nil (Midi->Cross ListMidi->Cross Cross->ChordSeq ChordSeq->Midi) nil)
         ("Utilities" nil nil (Transposer TimeScaler Crop Midi->chordseqs) nil)
         ))
 
;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)



