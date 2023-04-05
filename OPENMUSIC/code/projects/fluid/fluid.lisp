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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; FLUID package

(in-package :om)



(defvar *fluid-files* nil)

(setf *fluid-files*
      '(
        ;;; JACK
       ; #+linux"jack;cl-jack-load"
        
        ;;; FLUIDSYNTH
        "fluidsynth;load-fluidsynth"
        
        ;;; PLAYER
        "player;fluid-player"
      
        ;;; OM MENUS ETC.  
       ; "midipackages"
        "player;fluid-preferences"
        "player;fluid-info"
        "player;fluid-sf2-setup"
        "player;fluid-midi-to-audio"

        "tools;fluid-send"
        "tools;fluid-console"
        "tools;fluid-mixer"
        "tools;fluid-channels"
        ))



(eval-when (eval compile load)
  (mapc #'(lambda (filename)
            (compile&load (namestring (make-local-path *load-pathname* filename))))
        *fluid-files*))

;(push :midi-project *features*)

;;;================= FLUID PACKAGES ================

(defvar *fluidpackage* (omNG-protect-object (omNG-make-new-package "Fluid")))
(addPackage2Pack *fluidpackage* *audiopackage*)

(AddGenFun2Pack '(fluid-gain fluid-pgmout fluid-pgm-change fluid-pitchwheel fluid-ctrlchg fluid-volume 
                             fluid-pan fluid-reverb fluid-chorus midi-to-audio) *fluidpackage*)
(AddClass2Pack '(fluid-synth-console fluid-mix-console) *audiopackage*)
