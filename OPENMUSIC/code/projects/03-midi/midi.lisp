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
       "players;select-players"  
        #-linux "players;ms-players"  
               
       ; osc and microplayer
        "osc;send"
        "osc;receive"
        "osc;osc-events"
        #-linux "players;microplayer"


        "player;midi-init"
        "tools;miditools"
        "classes;midifile"
        "editors;midieditor"
        "tools;midifunctions"
        "classes;midievents"
        "classes;continuousctrl"
        "tools;midiconversions"
        "classes;midicontroller"
        "player;midiplayer"
        "tools;midi-preferences"
        "midipackages"
	;; #+cl-fluidsynth "player;fluid-player"
	#+(and linux cl-jack) "player;jack-midi-player"
        ))


(eval-when (eval compile load)
  (mapc #'(lambda (filename)
            (compile&load (namestring (make-local-path *load-pathname* filename))))
        *midi-files*))

(push :midi-project *features*)
