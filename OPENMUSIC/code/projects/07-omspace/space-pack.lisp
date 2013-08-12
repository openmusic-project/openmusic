(in-package :om)

(defvar *space-package* (omNG-protect-object (omNG-make-new-package "Space" :doc "Spatialization & 3D tools")))

(defvar *3D-package* (omNG-protect-object (omNG-make-new-package "3D")))
(AddClass2Pack '(3DC 3DC-lib) *3D-package*)
(AddPackage2Pack *3D-package* *basic-package*)

;;; GEOMETRIC UTILS
(defvar *geom-package* (omNG-protect-object (omNG-make-new-package "Geometry")))
(AddGenFun2Pack '(pol->car car->pol rad->deg deg->rad xy->ad ad->xy xyz->aed aed->xyz) *geom-package*)
(AddPackage2Pack *geom-package* *basic-package*)


;;; IN OM ROOT
;(AddPackage2Pack *space-package* *om-package-tree* )

(add-ref-section 
 '("BASIC TOOLS" (
                  ("3D" (3DC 3DC-lib 3D-trajectory))
                  ("Geometry" (deg->rad rad->deg car->pol pol->car xy->ad ad->xy xyz->aed aed->xyz))
                 )))