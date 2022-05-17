;========================================================================
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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================

(in-package :om)

;============================
;Callable slot modifiers
;============================


;;;NOTE SLOTS
(pushr 'note-slots *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'note-slots)) position container)   
  (if (add-slots-enabled container 'note-slots)
      (let ((newslots (omNG-make-new-boxcall-slots (class-of (make-instance 'note)) 
                                                   position (mk-unique-name container "slots"))))
        newslots)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))

;;;CHORD SLOTS
(pushr 'chord-slots *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'chord-slots)) position container)   
  (if (add-slots-enabled container 'chord-slots)
      (let ((newslots (omNG-make-new-boxcall-slots (class-of (make-instance 'chord)) 
                                                   position (mk-unique-name container "slots"))))
        newslots)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))


;;;CHORD-SEQ SLOTS
(pushr 'cs-slots *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'cs-slots)) position container)   
  (if (add-slots-enabled container 'cs-slots)
      (let ((newslots (omNG-make-new-boxcall-slots (class-of (make-instance 'chord-seq)) 
                                                   position (mk-unique-name container "slots"))))
        newslots)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))


;;;GROUP SLOTS
(pushr 'group-slots *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'group-slots)) position container)   
  (if (add-slots-enabled container 'group-slots)
      (let ((newslots (omNG-make-new-boxcall-slots (class-of (make-instance 'group)) 
                                                   position (mk-unique-name container "slots"))))
        newslots)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))


;;;MEASURE SLOTS
(pushr 'measure-slots *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'measure-slots)) position container)   
  (if (add-slots-enabled container 'measure-slots)
      (let ((newslots (omNG-make-new-boxcall-slots (class-of (make-instance 'measure)) 
                                                   position (mk-unique-name container "slots"))))
        newslots)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))


;;;VOICE SLOTS
(pushr 'voice-slots *spec-new-boxes-types*)

(defmethod get-new-box-from-type ((type (eql 'voice-slots)) position container)   
  (if (add-slots-enabled container 'voice-slots)
      (let ((newslots (omNG-make-new-boxcall-slots (class-of (make-instance 'voice)) 
                                                   position (mk-unique-name container "slots"))))
        newslots)
    (om-beep-msg (format nil "!!! OUT boxes not allowed in ~A" (type-of (object container))))))

