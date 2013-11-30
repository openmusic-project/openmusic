;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag, Augusto Agon, Jean Bresson

(in-package :om)



(defvar *midi-files* nil)

(setf *midi-files*
      '(
        ;;; TOOLS
        "tools;midi-tools"
        "tools;midi-read-write"
        
        ;;; OM OBJECTS
        "classes;midievents"
        "classes;midi-sequence"
        "classes;measures-tempo"
        "classes;continuous-controllers"
;        "classes;midicontroller"
        "classes;midifile"
        
        ;;; 
        "tools;midi-conversions"
        ;        "tools;midi-send"
     
        ;;; EDITORS
        "editors;midieditor"
        ;"editors;controllereditor" ON MY COMPUTER @IRCAM

        ;;; RENDERING
        
;        "player;midi-init"
;        "player;midiplayer"
;        "players;select-players"  
;        #-linux "players;ms-players"  
;	;; #+cl-fluidsynth "player;fluid-player"
;	#+(and linux cl-jack) "player;jack-midi-player"

        ;;; MISC (OSC)
       ; osc and microplayer
        "osc;send"
        "osc;receive"
        "osc;osc-events"
;        #-linux "players;microplayer"

        ;;; SYSTEM SETUP
;        "tools;midi-preferences"
;        "midipackages"

        ))


(eval-when (eval compile load)
  (mapc #'(lambda (filename)
            (compile&load (namestring (make-local-path *load-pathname* filename))))
        *midi-files*))

(push :midi-project *features*)
