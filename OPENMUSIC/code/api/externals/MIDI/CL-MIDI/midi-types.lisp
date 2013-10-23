;;===========================================================================
;;; midi-types.lisp
;;; Common Lisp Midi API - based on ms:: versions found in midi-api.lisp
;;; 
;;; This program is free software;;;  you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation;;;  either version 2
;;; of the License, or (at your option) any later version.
;;; 
;;; See file LICENSE for further informations on licensing terms.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY;;;  without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program;;;  if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;; 
;;; Author: Anders Vinjar


(defpackage :lispmidi (:nicknames :ml))
(in-package :ml)

;; typesetup as in old OM/midishare.lisp.  TODO: integrate MIDI
;; standard (ie. CM's midi1.scm):

(defparameter typeNote          0 "note with pitch, velocity and duration")
(defparameter typeKeyOn         1 "key on with pitch and velocity")
(defparameter typeKeyOff        2 "key off with pitch and velocity")
(defparameter typeKeyPress      3 "key pressure with pitch and pressure value")
(defparameter typeCtrlChange    4 "control change with control number and control value")
(defparameter typeProgChange    5 "program change with program number")
(defparameter typeChanPress     6 "channel pressure with pressure value")
(defparameter typePitchWheel    7 "pitch bend with lsb and msb of the 14-bit value")
(defparameter typePitchBend     7 "pitch bender with lsb and msb of the 14-bit value")
(defparameter typeSongPos       8 "song position with lsb and msb of the 14-bit position")
(defparameter typeSongSel       9 "song selection with a song number")
(defparameter typeClock        10 "clock request (no argument)")
(defparameter typeStart        11 "start request (no argument)")
(defparameter typeContinue     12 "continue request (no argument)")
(defparameter typeStop         13 "stop request (no argument)")
(defparameter typeTune         14 "tune request (no argument)")
(defparameter typeActiveSens   15 "active sensing code (no argument)")
(defparameter typeReset        16 "reset request (no argument)")
(defparameter typeSysEx        17 "system exclusive with any number of data bytes. Leading $F0 and tailing $F7 are automatically supplied by MidiShare and MUST NOT be included by the user")
(defparameter typeStream       18 "special event with any number of unprocessed data/status bytes")
(defparameter typePrivate      19 "private event for internal use with 4 32-bits arguments")
(defparameter typeProcess     128 "interrupt level task with a function adress and 3 32-bits args")
(defparameter typeDProcess    129 "foreground task with a function address and 3 32-bits arguments")
(defparameter typeQFrame      130 "quarter frame message with a type from 0 to 7 and a value")
(defparameter typeCtrl14b     131)
(defparameter typeNonRegParam 132)
(defparameter typeRegParam    133)
(defparameter typeSeqNum	     134)
(defparameter typeTextual     135)
(defparameter typeCopyright   136)
(defparameter typeSeqName     137)
(defparameter typeInstrName   138)
(defparameter typeLyric	     139)
(defparameter typeMarker	     140)
(defparameter typeCuePoint    141)
(defparameter typeChanPrefix  142)
(defparameter typeEndTrack    143)
(defparameter typeTempo	     144)
(defparameter typeSMPTEOffset 145)
(defparameter typeTimeSign    146)
(defparameter typeKeySign     147)
(defparameter typeSpecific    148)
(defparameter typePortPrefix  149)
(defparameter typeRcvAlarm    150)
(defparameter typeApplAlarm   151)
(defparameter typeReserved    152)
(defparameter typedead        255)
