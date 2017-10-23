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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
; Setup the Kernel package
;DocFile

(in-package :om)

(defvar *kernel-package* (omNG-protect-object 
                          (omNG-make-new-package "Kernel" 
                                                 :doc "The OM kernel package contains the building blocks and basic tools for creating visual programs in OM.")))


(defvar *lisp-package* (omNG-make-new-package "Lisp" :doc "Basic functions defined in the Lisp programming language."))
(setf (classes *lisp-package*) (copy-list *Basic-Lisp-Types*))
(AddLispFun2Pack '(first second third nth rest nthcdr butlast reverse length
                   list  remove  cons  append  apply  funcall  mapcar mapcan) *lisp-package*)

(defvar *control-package* nil "Sub-package CONTROL of KERNEL package.")
(setf *control-package* (omNG-protect-object (omNG-make-new-package "Control")))

(defvar *logic-package* nil "Logical Operators")
(setf *logic-package* (omNG-protect-object (omNG-make-new-package "Logical Operators")))
(addPackage2Pack *logic-package* *control-package*)
(addGenFun2Pack '(omand omor) *logic-package*)
(addGenFun2Pack '(omloop omif conditional repeat-n sequence callnext-method ) *control-package*) 

(defvar *predicate-package* (omNG-protect-object (omNG-make-new-package "Predicates")) "Predicates for testing numbers")
(AddGenFun2Pack '(om< om> om<= om>= om= om/=) *predicate-package*)
(AddPackage2Pack *predicate-package* *control-package*)


(defvar *data-package* nil "Sub-package DATA of KERNEL package.")
(setf *data-package* (omNG-protect-object (omNG-make-new-package "Data")))
(AddClass2Pack '(store) *data-package*)
(addGenFun2Pack '(list-elements set-slot get-slot clone) *data-package*)


(defvar *maquette-package* nil "Sub-package MAQUETTE of KERNEL package.")
(setf *maquette-package* (omNG-protect-object (omNG-make-new-package "Maquette")))
(AddClass2Pack 'TemporalBox *maquette-package*)
(AddGenFun2Pack '(TemporalBoxes addBox2Maquette removeTemporalBox removeAllTemporalBoxes get-maquette set-eval-func) *maquette-package*)

(defvar *player-package* nil "Player tools and functions.")
(setf *player-package* (omNG-protect-object (omNG-make-new-package "Player")))
(AddGenFun2Pack '(play) *player-package*)

(defvar *fileutils-package* (omNG-protect-object (omNG-make-new-package "Files")))

(defvar *filebox-package* (omNG-protect-object (omNG-make-new-package "File Box")))
(AddGenFun2Pack '(file-box file-write-line file-write file-read-line file-eof-p) *filebox-package*)
(AddPackage2Pack *filebox-package* *fileutils-package*)

(AddGenFun2Pack '(file-chooser infile outfile tmpfile) *fileutils-package*)


(defvar *system-package* (omNG-protect-object (omNG-make-new-package "System")))
(AddGenFun2Pack '(om-shell) *system-package*)


;;; DI BOXES
(defvar *di-package*  (omNG-protect-object (omNG-make-new-package "Interface Boxes")))
(AddClass2Pack '(text-box text-view button check-box 
                          slider single-item-list multi-item-list pop-up-menu) *di-package*)





;;;  == OM Kernel ===
(AddPackage2Pack *lisp-package* *kernel-package*)
(AddPackage2Pack *control-package* *kernel-package*)
(AddPackage2Pack *data-package* *kernel-package*)
(AddPackage2Pack *maquette-package* *kernel-package*)
(AddPackage2Pack *player-package* *kernel-package*)
(addPackage2Pack *fileutils-package* *kernel-package*)
(addPackage2Pack *di-package* *kernel-package*)
(addPackage2Pack *system-package* *kernel-package*)

(AddPackage2Pack *kernel-package* *om-package-tree*)


(add-ref-section (gen-ref-entries *kernel-package*))

(add-ref-section '("Kernel"
                   (("OMLoop" (omloop loopdo initdo finaldo 
                                      forloop whileloop listloop onlistloop 
                                      accumulator counter sum minim maxi listing)))))

; (gen-reference *om-ref-entries* *om-reference-dir*)

;(add-ref-section 
; '("KERNEL" 
;   (("Lisp Functions" 
;     (first second third nth rest nthcdr butlast length reverse remove list cons append apply funcall mapcar mapcan))
;    ("Logical Operators"
;     (omand omor))
;    ("Predicates"
;     (om< om> om<= om>= om= om/=))
;    ("Control Modules"
;     (sequence repeat-n omloop omif conditional))
;    ("Data" 
;     (store clone list-elements))
;    ("Object-Oriented" 
;     (get-slot set-slot callnext-method))
;    ("Maquette" 
;     (temporalbox addbox2maquette get-maquette temporalboxes removetemporalbox removeAllTemporalBoxes set-eval-func))
;    ("Player" 
;     (play))
;    ("File Utils" 
;     (file-box file-write-line file-write file-read-line file-eof-p infile outfile tmpfile))
;    ("System" 
;     (om-shell))
;    )))


