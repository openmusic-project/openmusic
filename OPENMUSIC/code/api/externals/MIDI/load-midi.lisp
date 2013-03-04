
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


(defvar *midi-api-files* nil)
(setf *midi-api-files* '(
                         "MidiShare;midishare"
                         "MidiShare;player"
                         "midi-api"
                         ))

(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "MidiShare")) :name "midishare"))
(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI" "MidiShare")) :name "player"))
(compile&load (make-pathname :directory (append *externals-directory* (list "MIDI")) :name "midi-api"))
 
(push :om-midi-api *features*)
