;;
;;
;;            Profile loader
;;
;;                       by Mikhail Malt   &   Jacopo Baboni Schilingi  © IRCAM 1994
           
  
;--------------------------------------------------
;Package Definition (Optional, else use package :OM) 
;--------------------------------------------------
(in-package :cl-user)

(defpackage "Profile"
  (:nicknames :prf) 
  (:use "COMMON-LISP" "CL-USER" "OpenMusic"))

(in-package "Profile")

(om::compile&load (om::om-relative-path '("sources") "profile"))

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(defvar *subpackages-profo* nil)
(setf *subpackages-profo*
      '(("Perturbation" nil nil (alea-pertb  compr/expan) nil)
        ("Change" nil nil (control-pertb prof-change) nil)
        ("Reflexions" nil nil (reflexion double-reflect multi-reflect) nil)
        ("Deriv/Integr" nil nil (mean-derivation pr-interlock derivation integration) nil)
        ("Interpolation" nil nil (Inter-dyn multi-interpol interpol-prof) nil)
        ("Utilities" nil nil (range-approx notes-change weight-average pr-group-list subst-list 
                                           interpol-tab bpf-interpolx ) nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------
(om::fill-library *subpackages-profo*)


