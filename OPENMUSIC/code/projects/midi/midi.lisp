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
        ;;; BASIC/LOW LEVEL TOOLS
        "tools;midi-tools"
        "tools;midi-connection"
        
        ;;; OM OBJECTS
        "classes;midievents"
        "classes;midi-sequence"
        "classes;measures-tempo"
        "classes;continuous-controllers"
        "classes;midi-console"
        "classes;midifile"
        
        ;;; UTILS AND FUNCTIONS
        "tools;midi-conversions"
        "tools;midi-save"
        "tools;midi-preparetoplay"
        "tools;midi-send" ; => TODO !!
        "tools;midi-in" ; => TODO !!
     
        ;;; EDITORS
        "editors;midieditor"
        "editors;controllereditor"
        
        ;;; PREFS AND SETTINGS
        "players;select-players"
        "tools;midi-preferences"

        ;;; RENDERING
      
        "players;midi-mixer"

        "players;om-midi-player"  
        #+midishare "players;midishare-player"  
       	;;#+cl-fluidsynth "players;fluid-player"
	#+(and linux cl-jack-midi) "players;jack-midi-player"

        ;;; MISC (OSC)
        "osc;send-receive"
        "osc;osc-events"
        "osc;osc-route"
        "osc;osc-player"
        #+macosx "players;microplayer"

        ;;; OM MENUS ETC.  
        "midipackages"

        ))



(eval-when (eval compile load)
  (mapc #'(lambda (filename)
            (compile&load (namestring (make-local-path *load-pathname* filename))))
        *midi-files*))

(push :midi-project *features*)
