
;;===========================================================================
;OM API 
;Multiplatform API for OpenMusic
;Macintosh version (Digitool Macintosh Common Lisp - MCL)
;
;Copyright (C) 2004 IRCAM-Centre Georges Pompidou, Paris, France.
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
;Authors: Jean Bresson and Augusto Agon
;;===========================================================================

;;===========================================================================
;DocFile
;loads the MIDI API - use Midishare (Grame)
;;===========================================================================

(in-package :om-api)

(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "MidiShare")) :name "midishare"))
(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "MidiShare")) :name "player"))
(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "MidiShare")) :name "midishare-api"))
(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "MidiShare")) :name "midishare-setup"))

(pushnew :midishare *features*)

(pushnew :midishare om-midi::*midi-systems*)
(pushnew :midishare om-midi::*midi-file-systems*)

(defmethod om-midi::load-midi-file-function ((midisystem (eql :midishare))) 'om-midi::midishare-load-file)
(defmethod om-midi::save-midi-file-function ((midisystem (eql :midishare))) 'om-midi::midishare-save-file)


(defmethod om-midi::send-midi-event-function ((midisystem (eql :midishare))) 'om-midi::midishare-send-evt)
(defmethod om-midi::midi-start-function ((midisystem (eql :midishare))) 'om-midi::midishare-start)
(defmethod om-midi::midi-stop-function ((midisystem (eql :midishare))) 'om-midi::midishare-stop)

(defmethod om-midi::midi-setup-function ((midisystem (eql :midishare))) 'om-midi::midishare-setup)
(defmethod om-midi::midi-connect-function ((midisystem (eql :midishare))) 'om-midi::midishare-connect-ports)
(defmethod om-midi::midi-restart-function ((midisystem (eql :midishare))) 'om-midi::midishare-restart)

(om-add-init-func 'om-midi::midishare-startup)
