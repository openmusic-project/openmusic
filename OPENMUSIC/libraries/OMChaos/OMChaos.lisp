;--------------------------------------------------
;Package Definition (Optional, else use package :OM) 
;--------------------------------------------------

(defvar ALEA)

(defpackage "ALEA" 
  (:use "COMMON-LISP" "OpenMusic"))


(in-package "ALEA")
 
;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *OMChaos-lib-files* nil)
(setf *OMChaos-lib-files* (list
                           (om::om-relative-path '("sources") "orbitals")
                           (om::om-relative-path '("sources") "ifs")
                           (om::om-relative-path '("sources") "fractus")
                           ))

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc #'om::compile&load *OMChaos-lib-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '(
        ("orbitals" nil nil (alea::Verhulst Verhulst2 alea::kaosn kaosn1 baker1 baker2 lorentz navier-stokes stein
                                            stein1 henon henon-heilles torus rossler ginger ginger2) nil)
        ("IFS" nil nil (alea::ifs-lib alea::IFSx alea::make-w alea::app-W-trans) nil)
        ("fractus" nil nil (midpoint1 midpoint2  alea::fract-gen1) nil)
        ("UTILS" nil nil (distance angle rad->deg deg->rad paires alea::choixaux) nil)
        ))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-list*)



