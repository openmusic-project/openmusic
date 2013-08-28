;--------------------------------------------------
;Package Definition (Optional, else use package :OM) 
;--------------------------------------------------

(defpackage "ALEA" 
  (:use "COMMON-LISP" "OpenMusic"))



(in-package "ALEA")

;;; mettre dans un des fichiers sources...
(defun om-random-value (val)
   (om::om-random-value val))

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *OMAlea-lib-files* nil)
(setf *OMAlea-lib-files* (list (om::om-relative-path '("sources") "distributions")
                               (om::om-relative-path '("sources") "random-walks")
                               (om::om-relative-path '("sources") "alea-sequences")
                               (om::om-relative-path '("sources") "outils")
                               ))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'om::compile&load *OMAlea-lib-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '(("distributions" nil nil (ran ran01 choix choixmultiple istexp  expobi distlin
                                      distcauchy distlog distCsHp distarsin poisson triang weilbull
                                      gauss gamma beta) nil)
        ("alea-seq" nil nil (not-centr alea-seq   linea-seq triang-seq) nil)
        ("random-walks" nil nil ( brownian1 brownian2  randwalk1  randwalk2  
   randwalkX achorripsis i1/f seq1/f markov1 markov2 Ana-Mark Ana-Mark1 Ana-Mark2 transition2) nil)
        ("tools" nil nil ( zoom1 zoom2  zoom3 zoom4 filtre1  filtre2  filtre3 filtre4) nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)



