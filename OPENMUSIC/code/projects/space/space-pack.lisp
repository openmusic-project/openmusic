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
;===========================================================================


(in-package :om)

(defvar *space-package* (omNG-protect-object (omNG-make-new-package "Space" :doc "Spatialization & 3D tools")))

(defvar *3D-package* (omNG-protect-object (omNG-make-new-package "3D")))
(AddClass2Pack '(3DC 3DC-lib) *3D-package*)
(AddGenFun2Pack '(3D-interpol 3D-sample) *3D-package*)
(AddPackage2Pack *3D-package* *basic-package*)

;;; GEOMETRIC UTILS
(defvar *geom-package* (omNG-protect-object (omNG-make-new-package "Geometry")))
(AddGenFun2Pack '(pol->car car->pol rad->deg deg->rad xy->ad ad->xy xyz->aed aed->xyz) *geom-package*)
(AddPackage2Pack *geom-package* *basic-package*)


;;; IN OM ROOT
;(AddPackage2Pack *space-package* *om-package-tree* )

(add-ref-section  
 '("BASIC TOOLS" (
                  ("3D" (3DC 3DC-lib 3D-trajectory 3D-interpol 3D-sample))
                  ("Geometry" (deg->rad rad->deg car->pol pol->car xy->ad ad->xy xyz->aed aed->xyz))
                 )))