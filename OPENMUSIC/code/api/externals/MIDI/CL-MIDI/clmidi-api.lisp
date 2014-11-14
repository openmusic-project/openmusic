;;===========================================================================
;;; midi-api-cl.lisp
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

;;===========================================================================
; DocFile
; MIDI functions called by OpenMusic
; Using lisp-based SMF I/O + events
; Sources at Goldsmiths, Univ. of London: http://www.doc.gold.ac.uk/isms/lisp/midi/
;;===========================================================================

;;; this file - midi-api-cl.lisp - is meant as a replacement off all
;;; midishare-dependencies in midi-api.lisp, while staying compatible
;;; with all the various ms:: specialities.  Everything below named
;;; 'om-midi-*' seems to be part of the main OM api.
;;;
;;; TODO: get rid of ms:: thinking everywhere where live-streams arent
;;; wanted - ie smf read/write, message-crunching..  Substitute
;;; everything live/rt with ALSA-based api.

(in-package :om-midi)


;;;
;;; FILE OUTPUT: building useful midi-messages, writing SMF's:
;;;

(defparameter +note-off-opcode+		#x80)
(defparameter +note-on-opcode+		#x90)
(defparameter +key-pressure-opcode+	#xA0)
(defparameter +control-change-opcode+	#xB0)
(defparameter +program-change-opcode+	#xC0)
(defparameter +channel-pressure-opcode+ #xD0)
(defparameter +pitch-bend-opcode+	#xE0)
(defparameter +tempo-opcode+		#xFF)


(defun midi-command-type (event)
  (slot-value event 'midi::status-min))

(defun midi-status-byte (event)
  (slot-value event 'midi::status))

;; these two read data-bytes from various message-types

(defmethod midi-data-byte-1 ((event t)) 0)
(defmethod midi-data-byte-2 ((event t)) 0)

;; VOICE MESSAGES

(defmethod midi-channel ((event t)) nil)
(defmethod midi-channel ((event midi::channel-message))
  ;; used where channel isn't set explicit in instance
  (- (midi-status-byte event) (midi-command-type event)))


(defmethod midi-message-time ((msg midi::message)) (midi::message-time msg))

(defmethod midi-message-channel ((msg midi::channel-message)) (midi::message-channel msg))
(defmethod midi-message-channel ((msg t)) -1)

;;; Accessors to define for the different types of MIDI messages
(defmethod midi-message-type ((msg t)) (intern (concatenate 'string "Unknown:" (string (type-of msg)))))
(defmethod midi-message-fields ((msg t)) nil)


;; NOTE OFF

(defun make-note-off-message (time key vel chan)
  (make-instance 'midi:note-off-message :key key :time time :velocity vel :status (logior +note-off-opcode+ chan)))
(defmethod midi-data-byte-1 ((event midi::note-off-message))
  (midi::message-key event))
(defmethod midi-key ((event midi::note-off-message))
  (midi::message-key event))
(defmethod midi-data-byte-2 ((event midi::note-off-message))
  (midi::message-velocity event))
(defmethod midi-message-type ((msg midi::note-off-message)) :KeyOff)
(defmethod midi-message-fields ((msg midi::note-off-message))
  (list (midi::message-key msg) (midi::message-velocity msg)))

;; NOTE ON

(defun make-note-on-message (time key vel chan)
  (make-instance 'midi:note-on-message :key key :time time :velocity vel :status (logior +note-on-opcode+ chan)))
(defmethod midi-data-byte-1 ((event midi::note-on-message))
  (midi::message-key event))
(defmethod midi-key ((event midi::note-on-message))
  (midi::message-key event))
(defmethod midi-data-byte-2 ((event midi::note-on-message))
  (midi::message-velocity event))
(defmethod midi-message-type ((msg midi::note-on-message)) :KeyOn)
(defmethod midi-message-fields ((msg midi::note-on-message))
  (list (midi::message-key msg) (midi::message-velocity msg)))

;; PROGRAM CHANGE

(defun make-program-change-message (time prog chan)
  (make-instance 'midi:program-change-message :time time :program prog :status (logior +program-change-opcode+ chan)))
(defmethod midi-data-byte-1 ((event midi::program-change-message))
  0)
(defmethod midi-data-byte-2 ((event midi::program-change-message))
  (midi::message-program event))
(defmethod midi-message-type ((msg midi::program-change-message)) :ProgChange)
(defmethod midi-message-fields ((msg midi::program-change-message)) (list (midi::message-program msg)))

;; CONTROL CHANGE

(defun make-control-change-message (time controller value chan)
  (make-instance 'midi::control-change-message :time time :controller controller :value value :status (logior +control-change-opcode+ chan)))
(defmethod midi-data-byte-1 ((event midi::control-change-message))
  (midi::message-value event))
(defmethod midi-data-byte-2 ((event midi::control-change-message))
  (midi::message-controller event))
(defmethod midi-message-type ((msg midi::control-change-message)) :CtrlChange)
(defmethod midi-message-fields ((msg midi::control-change-message)) (list (slot-value msg 'midi::controller)
                                                                          (slot-value msg 'midi::value)))

;; PITCH BEND

;; range = 14 bits (-8192 -> 8190 in user-code)
;;; !! not anymore in OM 6.9 PW = 0-16384
(defun make-pitch-bend-message (time bend chan)
  (make-instance 'midi:pitch-bend-message :time time :value bend :status (logior +pitch-bend-opcode+ chan)))
(defun 7-msb (14bitval) (logand (ash 14bitval -7) #x7f))
(defun 7-lsb (14bitval) (logand 14bitval #x7f))
(defmethod midi-data-byte-1 ((event midi::pitch-bend-message))
  (7-lsb (midi::message-value event)))
(defmethod midi-data-byte-2 ((event midi::pitch-bend-message))
  (7-msb (midi::message-value event)))

(defmethod midi-message-type ((msg midi::general-text-message)) :Textual)
(defmethod midi-message-type ((msg midi::sequence/track-name-message)) :SeqName)
(defmethod midi-message-type ((msg midi::instrument-message)) :InstrName)
(defmethod midi-message-type ((msg midi::lyric-message)) :Lyric)
(defmethod midi-message-type ((msg midi::copyright-message)) :Copyright)
;;; Superclass for all text messages
(defmethod midi-message-fields ((msg midi::text-message)) (map 'list #'char-code (slot-value msg 'midi::text)))  ;; restore the list of ASCII.. ?

(defun make-tempo-message (time tempo)
  (make-instance 'midi:tempo-message :time time :tempo tempo :status +tempo-opcode+))
(defmethod midi-message-type ((msg midi::tempo-message)) :Tempo)
(defmethod midi-message-fields ((msg midi::tempo-message)) (list (midi::message-tempo msg)))

(defmethod midi-message-type ((msg midi::time-signature-message)) :TimeSign)
(defmethod midi-message-fields ((msg midi::time-signature-message)) 
  (list (midi::message-numerator msg)
        (midi::message-denominator msg)
        (slot-value msg 'midi::cc)
        (slot-value msg 'midi::bb)))


;; takes instances of the various midi:*message classes, returning a list of midi-evt
(defun make-event-from-message (msg ref)
  (make-midi-evt :type (midi-message-type msg)
                 :date (midi-message-time msg)
                 :chan (1+ (midi-message-channel msg))
                 :ref ref
                 :fields (midi-message-fields msg)))

(defun tracks2seq (tracks)
  (sort (loop for track in tracks 
              for ref = 0 then (+ ref 1) append
              (loop for msg in track collect (make-event-from-message msg ref)))
        #'midi-evt-<))

;;; THE FUNCTION CALLED BY OM
(defun cl-midi-load-file (pathname)
  (let ((f (midi:read-midi-file pathname))) 
    (values (tracks2seq (midi:midifile-tracks f))
            (length (midi:midifile-tracks f)) 
            (midi:midifile-division f) 
            (midi:midifile-format f))))

;; META MESSAGES


(defun event2note-on-off (ev)
  (let ((k (first (midi-evt-fields ev)))
	(v (second (midi-evt-fields ev)))
	(onset (midi-evt-date ev))
	(dur (third (midi-evt-fields ev)))
	(chan (1- (midi-evt-chan ev))))
    (let ((on (make-note-on-message onset k v chan))
	  (off (make-note-off-message (+ onset dur) k 0 chan)))
      (list on off))))

(defun event2note-off (ev)
  (make-note-off-message (midi-evt-date ev) (first (midi-evt-fields ev)) (second (midi-evt-fields ev)) (1- (midi-evt-chan ev))))

(defun event2note-on (ev)
  (make-note-on-message (midi-evt-date ev) (first (midi-evt-fields ev)) (second (midi-evt-fields ev)) (1- (midi-evt-chan ev))))

(defun event2program-change-message (ev)
  (make-program-change-message (midi-evt-date ev) (first (midi-evt-fields ev)) (midi-evt-chan ev)))

(defun event2tempo (ev)
  (make-tempo-message (midi-evt-date ev) (first (midi-evt-fields ev))))

(defun make-messages-from-event (ev)
  (let ((type (midi-evt-type ev)))
    (cond 
      ((equal type :Note) (event2note-on-off ev)) ;returns cons
      ((equal type :keyOn) (event2note-on ev))
      ((equal type :keyOff) (event2note-off ev))
      ((equal type :Tempo) (event2tempo ev))
      (t (print (format nil "(cl-midi) message-type ~A isn't supported yet" type)) NIL))))

(defun seq2tracks (seq)
  (remove nil
          (loop for ev in seq
                for msg = (make-messages-from-event ev)
                if (listp msg) append msg
                else collect msg)))

(defun cl-midi-save-file (seq filename fileformat clicks)
  (declare (ignore timedef tracks))
  (let ((mf (make-instance 'midi:midifile :format fileformat :division clicks)))
    (setf (slot-value mf 'midi::tracks) (list (seq2tracks seq)))
    #+lispworks(sys::ENSURE-DIRECTORIES-EXIST filename :verbose t)  ;;; !!! LW specific
    (midi:write-midi-file mf filename)
    filename))


(defun om-midi::cl-midi-send-evt (event &optional player)
  (print 'not-yet-set))

;;; I think there's no need to handle this here.
;;; In principle OM supports KeyOn/KeyOff as well as Note (incl. duration) messages.
;;; We can suppose the 'Note' concept is not considered here

#|
(defun event-is-on-off-or... (msg)
  (cond ((or (typep msg 'midi:note-off-message)
	     (and (typep msg 'midi:note-on-message) (zerop (midi:message-velocity msg))))
	 'off)
	((typep msg 'midi:note-on-message) 'on)
	(t t)))

(defun make-event-from-message (msg ref note-list)
  (case (event-is-on-off-or... msg)
    (off (let* ((key (midi:message-key msg))
		(channel (midi:message-channel msg))
		(startevt (gethash (list key channel) note-list)))
	   (when startevt
	     (remhash (list key channel) note-list)
	     (let ((duration (- (midi:message-time msg) (midi:message-time startevt))))
	       (change-class startevt 'midimsg2evt)
	       (setf (event-dur startevt) duration)
	       (setf (event-ref startevt) ref)
	       startevt))))
    (on (let ((key (midi:message-key msg))
	      (channel (midi:message-channel msg)))
	  ;;push data to table and return nil (for collectors...):
	  (setf (gethash (list key channel) note-list) msg)
	  nil))
    (t (make-midi-evt :type (midi-message-type msg)
                      :date (midi::message-time msg)
                      :channel (midi::message-channel msg)
                      :ref ref
                      :fields (midi-message-fields msg)))
    ))

(defun messages2events (trk ref)
  (let ((note-list (make-hash-table :test 'equal)))
    (loop for message in trk
          for event = (make-event-from-message message ref note-list)
          when event collect event)))

(defun tracks2seq (tracks)
  (sort (loop for ref from 0
              for track in tracks
              append (messages2events track ref))
        #'midi-evt-<))

|#

;;; THESE FUNCTIONS ARE NOT USED ANYMORE

#|

;;; run through whole seq 'linking' events:

(defun linkevents (events)		
  (loop for this in events
       for next in (cdr events)
       collect (progn (setf (event-link this) next) this) into bag
       finally (return (nconc bag (list next)))))
|#

;; (defun tracks2seq (tracks)
;;   (mapcar #'linkevents (apply #'append (mapcar #'messages2events tracks))))

#|
(defun tracks2seq (tracks)
  (if (> (length tracks) 1)
      (append
       ;;mf-format=1, ie. dont link events from tempo-track...
       (messages2events (car tracks) 0)
       (linkevents
	(sort (loop for ref from 1
		 for track in (cdr tracks)
		 append (messages2events track ref))
	      #'sort-events-<)))
      ;; else mf-format=0, handle interspersed tempo-messages
      (linkevents		
       (sort (loop for ref from 0
		for track in tracks
		append (messages2events track ref))
	     #'sort-events-<))))
|#

