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
;Authors: Gerard Assayag and Augusto Agon


;DocFile
;This File containts Midi initialization, termination and management.
;Last Modifications :
;17/11/97 A temporal scheduler is implemented, we will like to change
;         this scheduler for another one more performant.
;DocFile

(in-package :om)

(defvar *outmidiport* 0      "default output port number")
(defvar *inmidiport* 0      "default input port number")

(defvar *midiplayer* nil      "refnum of the player")
(defvar *midirecorder* nil    "refnum of the recorder")
(defvar *midifilter* nil      "allow filter midievents in the recorder")



(defvar *midi-share?* nil "Is MidiShare loaded?")

;;;Open MidiShare and connections
;;;
;;;=== MidiOpen : redefinition 
;;;== modif : connection du recorder
(defun midi-open ()
  "Check if MidiShare is present, if this is the case open the player and
the recorder, this function is called by a def-load-pointers"
  (setf *midiplayer* nil)
  (if (setf *midi-share?* (om-midi-startup))
      (om-without-interrupts  
        ;;; open player
        (open-ms-players)
        ;(add-assoc-player *general-player* 'midishare)
        ;;; set scheduler time to midi time
        ;(om-stop-scheduler)
        ;(defun clock-time () (om-midi-get-time))
        (enable-player :midishare)
        )
    (om-message-dialog (format nil (om-str :lib-error) "MIDI")))
  ;(init-scheduler)  ;; on demarre quand même le scheduler
  t)

; (midi-close)
; (midi-open)

;;; Close MidiShare and off the scheduler
(defun midi-close ()
   "If MidiShare is present, close the player and the recorder before quit the application"
   (when *midi-share?*
     ;(close-ms-players) ;;; remettre ?
     (om-midi-exit)
     (disable-player :midishare)
     (setf *midi-share?* nil))
   )

(defun open-ms-players ()
  (setq *midiplayer* (om-midi-open-player "OMPlayer"))
  (setf *midirecorder* (om-midi-open-player "OMRecorder"))
  )

(defun close-ms-players ()
  (when *midiplayer* (om-midi-close-player *midiplayer*))
  (when *midirecorder* (om-midi-close-player *midirecorder*))
  )

(defun midiplay-reset ()
  (ignore-errors (close-ms-players))
  (open-ms-players)
  (print "MIDI player reset."))

(defun make-port-menu (list posi) (declare (ignore list posi)))

;;;===============================
#-linux (om-add-init-func 'midi-open)  
(om-add-exit-cleanup-func 'midi-close t)
;;;===============================



;;;(ms::midishare)                                           	; <== EVALUATE THIS EXPRESSION.
;;;(defparameter *refnum* (ms::midiopen "Common Lisp"))     	; <== EVALUATE THIS EXPRESSION.
;;;
;;;
;;;(ms::MidiConnect *refnum* 0 -1)
;;;
(defun send-note (pitch)
  (let ((event (ms::MidiNewEv ms::typeNote)))	; ask for a new note event
    (ms::chan event 0)			; set the midi channel to 0 (means channel 1)
    (ms::port event 0)			; set the destination port to 0
    (ms::field event 0 pitch)		; set the pitch field
    (ms::field event 1 100)		; set the velocity field
    (ms::field event 2 1000)		; set the duration field to 1 second
    (ms::MidiSendIm *refnum* event))	; send the note immediatly
  )						; <== EVALUATE THIS DEFINITION
;;;
;;;
;;;
;;;(send-note 60)
