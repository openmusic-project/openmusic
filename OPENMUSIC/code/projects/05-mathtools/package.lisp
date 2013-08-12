(in-package :om)


(defvar *mamux-package* (omNG-protect-object (omNG-make-new-package "MathTools")))
(AddPackage2PAck *mamux-package* *om-package-tree*)

;-------Circle
(defvar *circle-package* (omNG-protect-object (omNG-make-new-package "Circle")))
(AddPackage2PAck *circle-package* *mamux-package*)

(AddClass2Pack '(n-cercle) *circle-package*)
(AddGenFun2Pack '(c2chord chord2c  c2chord-seq chord-seq2c c2rhythm  rhythm2c ) *circle-package*)


;------- Sieves 
(defvar *sieves-package* (omNG-protect-object (omNG-make-new-package "Sieves")))
(AddPackage2Pack *sieves-package* *mamux-package*)

(AddClass2Pack '(crible) *sieves-package*)
(AddGenFun2Pack '(c-union c-intersection c-complement revel-crible) *sieves-package*)



;------- Groups
(defvar *groups-package* (omNG-protect-object (omNG-make-new-package "Groups")))
(AddPackage2Pack *groups-package* *mamux-package*)

(AddGenFun2Pack '(mod+ mod- mod* n-scale n-structure get-min-period get-subsets) *groups-package*)

(defvar *zn-package* (omNG-protect-object (omNG-make-new-package "Zn")))
(AddPackage2Pack *zn-package* *groups-package*)
(AddGenFun2Pack '(card orbites famille transp transp-comb TL-ZN  get-tid) *zn-package* )


(defvar *dn-package* (omNG-protect-object (omNG-make-new-package "Dn")))
(AddPackage2Pack *dn-package* *groups-package*)
(addgenfun2pack '(inv dn-orbites pc-set dn-card n-ord p-form comp sub-power 
                  sub-p-form sub-rel sub-complex 
                  ) *dn-package*) 

(defvar *aff-package* (omNG-protect-object (omNG-make-new-package "Aff")))
(AddPackage2Pack *aff-package* *groups-package*)
(AddGenFun2Pack '(AllClasses ifunc inj inj-transp) *aff-package* )

;------- Sequences
(defvar *Sequences-package* (omNG-protect-object (omNG-make-new-package "Sequences")))
(AddPackage2Pack *Sequences-package* *mamux-package*)
(addgenfun2pack '(diff-in-list suite-reductible suite-reproductible suite-decomposition growing-by-add) *Sequences-package*)

;------- Polynomials
(defvar *Polynomials-package* (omNG-protect-object (omNG-make-new-package "Polynomials")))
(AddPackage2Pack *Polynomials-package* *mamux-package*)

;------- Canons

(defvar *Canons-package* (omNG-protect-object (omNG-make-new-package "Canons")))
(AddPackage2Pack *Canons-package* *mamux-package*)

(defvar *vuza-package* (omNG-protect-object (omNG-make-new-package "Vuza")))
(AddPackage2Pack *vuza-package* *Canons-package*)
(addgenfun2pack '(canon-n decompo infocanons Patterns Canons) *vuza-package*) 

(defvar *aug-package* (omNG-protect-object (omNG-make-new-package "Augmented")))
(AddPackage2Pack *aug-package* *Canons-package*)
(addgenfun2pack '(signatures ag-canoninfo  allcanons-aff  augmented-canon) *aug-package*) 

(defvar *cyclo-package* (omNG-protect-object (omNG-make-new-package "Cyclotomic")))
(AddPackage2Pack *cyclo-package* *Canons-package*)
(addgenfun2pack '(cyclo bonpolynome? poly2canon poly-mult mult-mult out-rythm get-canon-n  cm-conditions t2? t1? ) *cyclo-package*) 

