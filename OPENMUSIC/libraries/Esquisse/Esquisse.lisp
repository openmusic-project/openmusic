(in-package :om)
 
;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *Esquisse-lib-files* nil)
(setf *Esquisse-lib-files* (list (om::om-relative-path '("sources") "esquisse")))
  
;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'compile&load *Esquisse-lib-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '( ("Intervals" nil nil (BEST-TRANSP) nil)
         ("Spectral Harmony"
          (("harm-series" nil nil (HARM-SERIES NTH-HARM) nil)
           ("modulations" nil nil (FREQ-MOD RING-MOD) nil)
           ("processing" nil nil ( FSHIFT FDISTOR ) nil)
           ("analysis" nil nil (  VIRT-FUND M-VIR-FUN HARM-DIST BEST-FREQ) nil))
          Nil Nil Nil)
         ("Utilities" nil nil nil nil)
         ("midi" nil nil nil nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)



