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


(in-package :om-midi)


;;; TODO:  pack everything in to a macro:
;;;
;;; - status-byte
;;; - typename ie, :note-on


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
(defparameter +tempo-opcode+		#x51)
(defparameter +time-signature-opcode+	#x58)
(defparameter +key-signature-opcode+	#x59)
(defparameter +midi-port-opcode+	#x21)
(defparameter +end-of-track-opcode+	#x2F)

(defconstant +reset-all-controllers-message-opcode+ #xB0)
(defconstant +all-notes-off-message-opcode+ #xB0)

;; ;; midi text messages
;; (defconstant +general-text-opcode+	#x01)
;; (defconstant +copyright-opcode+		#x02)
;; (defconstant +sequence/track-name-opcode+ #x03)
;; (defconstant +instrument-opcode+	#x04)
;; (defconstant +lyric-opcode+		#x05)
;; (defconstant +marker-opcode+		#x06)
;; (defconstant +cue-point-opcode+		#x07)
;; (defconstant +program-name-opcode+	#x08)
;; (defconstant +device-name-opcode+	#x09)

#|

Meta events: from http://www.ccarh.org/courses/253/handout/smf/

0x00 	Sequence number
0x01 	Text event
0x02 	Copyright notice
0x03 	Sequence or track name
0x04 	Instrument name
0x05 	Lyric text
0x06 	Marker text
0x07 	Cue point
0x20    MIDI channel prefix assignment
0x2F    End of track
0x51    Tempo setting
0x54    SMPTE offset
0x58    Time signature
0x59    Key signature
0x7F    Sequencer specific event
|#


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

(defun event2note-off (ev)
  (make-note-off-message (midi-evt-date ev) (first (midi-evt-fields ev)) (second (midi-evt-fields ev)) (1- (midi-evt-chan ev))))

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

(defun event2note-on (ev)
  (make-note-on-message (midi-evt-date ev) (first (midi-evt-fields ev)) (second (midi-evt-fields ev)) (1- (midi-evt-chan ev))))

;; PROGRAM CHANGE

(defun make-program-change-message (time prog chan)
  (make-instance 'midi:program-change-message :time time :program prog :status (logior +program-change-opcode+ chan)))
(defmethod midi-data-byte-1 ((event midi:program-change-message))
  0)
(defmethod midi-data-byte-2 ((event midi:program-change-message))
  (midi::message-program event))
(defmethod midi-message-type ((msg midi:program-change-message)) :ProgChange)
(defmethod midi-message-fields ((msg midi:program-change-message)) (list (midi::message-program msg)))

(defun event2program-change-message (ev)
  (make-program-change-message (midi-evt-date ev) (first (midi-evt-fields ev)) (1- (midi-evt-chan ev))))

;; CONTROL CHANGE

(defun make-control-change-message (time controller value chan)
  (make-instance 'midi::control-change-message :time time :controller controller :value value :status (logior +control-change-opcode+ chan)))
(defmethod midi-data-byte-1 ((event midi::control-change-message))
  (midi::message-value event))
(defmethod midi-data-byte-2 ((event midi::control-change-message))
  (midi::message-controller event))
(defmethod midi-message-type ((msg midi::control-change-message)) :CtrlChange)
(defmethod midi-message-fields ((msg midi::control-change-message))
  (list (slot-value msg 'midi::controller) (slot-value msg 'midi::value)))

(defun event2control-change (ev)
  (make-control-change-message (midi-evt-date ev) (first (midi-evt-fields ev)) (second (midi-evt-fields ev)) (1- (midi-evt-chan ev))))

;; PITCH BEND

;; range = 14 bits (-8192 -> 8190 in user-code)
;;; !! not anymore in OM 6.9 PW = 0-16384
(defun make-pitch-bend-message (time bend chan)
  (make-instance 'midi::pitch-bend-message :time time :value bend :status (logior +pitch-bend-opcode+ chan)))
(defmethod midi-message-type ((msg midi::pitch-bend-message)) :PitchBend)
(defun 7-msb (14bitval) (logand (ash 14bitval -7) #x7f))
(defun 7-lsb (14bitval) (logand 14bitval #x7f))
(defmethod midi-data-byte-1 ((event midi::pitch-bend-message))
  (7-lsb (midi::message-value event)))
(defmethod midi-data-byte-2 ((event midi::pitch-bend-message))
  (7-msb (midi::message-value event)))
(defmethod midi-message-fields ((msg midi::pitch-bend-message))
  (list (midi::message-value msg)))

(defun event2pitch-bend (ev)
  (when (midi-evt-fields ev)
    (make-pitch-bend-message (midi-evt-date ev) (first (midi-evt-fields ev)) (1- (midi-evt-chan ev)))))

;; TEXT MESSAGES

(defmethod midi-message-type ((msg midi::general-text-message)) :Textual)
(defmethod midi-message-type ((msg midi:sequence/track-name-message)) :SeqName)
(defmethod midi-message-type ((msg midi::instrument-message)) :InstrName)
(defmethod midi-message-type ((msg midi::lyric-message)) :Lyric)
(defmethod midi-message-type ((msg midi::copyright-message)) :Copyright)

;;; Superclass for all text messages
(defmethod midi-message-fields ((msg midi::text-message))
  (map 'list #'char-code (slot-value msg 'midi::text)))  ;; restore the list of ASCII.. ?



(defconstant +copyright-opcode+ #x02)
(defun event2copyright (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::copyright-message :time time :status +copyright-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defconstant +sequence/track-name-opcode+ #x03)
(defun event2seqname (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::sequence/track-name-message :time time :status +sequence/track-name-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defconstant +instrument-opcode+ #x04)
(defun event2instrument (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::instrument-message :time time :status +instrument-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defconstant +lyric-opcode+ #x05)
(defun event2lyric (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::lyric-message :time time :status +lyric-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defconstant +marker-opcode+ #x06)
(defun event2marker (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::marker-message :time time :status +marker-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defconstant +cue-point-opcode+ #x07)
(defun event2cue-point (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::cue-point-message :time time :status +cue-point-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

;; (defconstant +lyric-opcode+		#x05)

;; (defvar all-text-messages '((general-text . (#x01 :Textual))
;; 			    (midi::sequence/track-name-message . (#x03 :SeqName))
;; 			    (lyric . (#x05 :Lyric))
;; 			    (instrument . (#x4 :InstrName))
;; 			    (copyright . (#x2 :Copyright))
;; 			    ))

;; (defmacro midi-support-for-message (message)
;;   (let ((event2msg-function-name (intern (format nil "event2~A" message)))
;; 	(keywordname (caddr (assoc message all-text-messages)))
;; 	(opcode (cadr (assoc message all-text-messages)))
;; 	(midiclass (find-class message)))
;;     `(progn (defmethod midi-message-type ((msg ,message)) ,keywordname)
;; 	    (defun ,event2msg-function-name (ev)
;; 	      (let* ((time (midi-evt-date ev))
;; 		     (value (first (midi-evt-fields ev)))
;; 		     (inst (make-instance ,message :time time :status (logior ,opcode))))
;; 		(setf (slot-value inst 'midi::text) text)
;; 		inst)))))


;; (midi-support-for-message midi::sequence/track-name-message)

;; (progn
;;   (defmethod midi-message-type
;;     ((msg midi:sequence/track-name-message))
;;     :seqname)
;;   (defun |event2sequence/track-name-message| (ev)
;;     (let* ((time (midi-evt-date ev))
;;            (value (first (midi-evt-fields ev)))
;;            (inst
;;             (make-instance midi:sequence/track-name-message
;;                            :time
;;                            time
;;                            :status
;;                            (logior 3))))
;;       (setf (slot-value inst 'midi::text) text)
;;       inst)))





;; TEMPO

(defun make-tempo-message (time tempo)
  (make-instance 'midi:tempo-message :time time :tempo tempo :status +tempo-opcode+))
(defmethod midi-message-type ((msg midi::tempo-message)) :Tempo)
(defmethod midi-message-fields ((msg midi::tempo-message)) (list (midi::message-tempo msg)))

(defun event2tempo (ev)
  (make-tempo-message (midi-evt-date ev) (first (midi-evt-fields ev))))

;; TIME SIGNATURE

(defun make-time-signature-message (time nn dd cc bb)
  (let ((inst (make-instance 'midi::time-signature-message :time time
			     :status +time-signature-opcode+)))
    (setf (slot-value inst 'midi::nn) nn
	  (slot-value inst 'midi::dd) dd
	  (slot-value inst 'midi::cc) cc
	  (slot-value inst 'midi::bb) bb
	  )
    inst))

(defmethod midi-message-type ((msg midi::time-signature-message)) :TimeSign)
(defmethod midi-message-fields ((msg midi::time-signature-message)) 
  (list (midi::message-numerator msg)
        (midi::message-denominator msg)
	(slot-value msg 'midi::cc)
        (slot-value msg 'midi::bb)))

(defun event2time-signature (ev)
  (apply #'make-time-signature-message (midi-evt-date ev) (midi-evt-fields ev)))


;; KEY SIGNATURE

(defun make-key-signature-message (time sf mi)
  (let ((inst (make-instance 'midi::key-signature-message :time time
			     :status +key-signature-opcode+)))
    (setf (slot-value inst 'midi::sf) sf
	  (slot-value inst 'midi::mi) mi)
    inst))

(defmethod midi-message-type ((msg midi::key-signature-message)) :KeySign)
(defmethod midi-message-fields ((msg midi::key-signature-message)) 
  (list (midi::message-sf msg)
        (midi::message-mi msg)))

(defun event2key-signature (ev)
  (apply #'make-key-signature-message (midi-evt-date ev) (midi-evt-fields ev)))

;;
;; MIDI PORT MESSAGE
;; 
(defun make-midi-port-message (time port)
  (let ((inst (make-instance 'midi::midi-port-message :time time
			     :status +midi-port-opcode+)))
    (setf (slot-value inst 'midi::port) port)
    inst))

(defmethod midi-message-type ((msg midi::midi-port-message)) :MidiPortMsg)
(defmethod midi-message-fields ((msg midi::midi-port-message)) 
  (list (slot-value msg 'midi::port)))
(defun event2midi-port-msg (ev)
  (apply #'make-midi-port-message (midi-evt-date ev) (midi-evt-fields ev)))


;;
;; END OF TRACK MESSAGE
;; 
(defun make-end-of-track-message (time status)
  (let ((inst (make-instance 'midi::end-of-track-message :time time
			     :status +end-of-track-opcode+)))
    (setf (slot-value inst 'midi::status) status)
    inst))

(defmethod midi-message-type ((msg midi::end-of-track-message)) :EndOfTrackMsg)
(defmethod midi-message-fields ((msg midi::end-of-track-message)) 
  (list (slot-value msg 'midi::status)))
(defun event2end-of-track-msg (ev)
  (apply #'make-end-of-track-message (midi-evt-date ev) (midi-evt-fields ev)))

;; :ResetAllControllers

(defmethod midi-message-type ((msg midi::reset-all-controllers-message)) :ResetAllControllers)
(defun make-reset-all-controllers-message (time chan)
  (make-instance 'midi::reset-all-controllers-message :time time :status (logior +reset-all-controllers-message-opcode+ chan)))
(defun event2reset-all-controllers-message (ev)
  (make-reset-all-controllers-message  (midi-evt-date ev) (midi-evt-chan ev)))

;; :AllNotesOff 

(defmethod midi-message-type ((msg midi::all-notes-off-message)) :AllNotesOff)
(defun make-all-notes-off-message (time chan)
  (make-instance 'midi::all-notes-off-message :time time :status (logior +all-notes-off-message-opcode+ chan)))

(defun event2all-notes-off-message (ev)
  (make-all-notes-off-message (midi-evt-date ev) (midi-evt-chan ev)))


;; ABSTRACTED MESSAGES

(defun event2note-on-off (ev)
  (let ((k (first (midi-evt-fields ev)))
	(v (second (midi-evt-fields ev)))
	(onset (midi-evt-date ev))
	(dur (third (midi-evt-fields ev)))
	(chan (1- (midi-evt-chan ev))))
    (let ((on (make-note-on-message onset k v chan))
	  (off (make-note-off-message (+ onset dur) k 0 chan)))
      (list on off))))

;;;
;;;
;;;  END OF MESSAGE TYPES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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

;; (defun make-messages-from-event (ev)
;;   (let ((type (midi-evt-type ev)))
;;     (cond 
;;       ((equal type :Note) (event2note-on-off ev)) ;returns cons
;;       ((equal type :keyOn) (event2note-on ev))
;;       ((equal type :keyOff) (event2note-off ev))
;;       ((equal type :Tempo) (event2tempo ev))
;;       (t (print (format nil "(cl-midi) message-type ~A isn't supported yet" type)) (break) NIL))))

(defun make-messages-from-event (ev)
  (let ((type (midi-evt-type ev)))
    (case type 
      (:Note (event2note-on-off ev)) ;returns cons
      (:keyOn (event2note-on ev))
      (:keyOff (event2note-off ev))
      (:Tempo (event2tempo ev))
      (:CtrlChange (event2control-change ev))
      (:ProgChange (event2program-change-message ev))
      (:PitchBend (event2pitch-bend ev))
      (:TimeSign (event2time-signature ev))
      (:KeySign (event2key-signature ev))
      (:MidiPortMsg (event2midi-port-msg ev))
      ((:Textual :SeqName :InstrName :Lyric :Copyright) (event2textual ev))
      (:ResetAllControllers (event2reset-all-controllers-message ev))
      (:AllNotesOff (event2all-notes-off-message ev))
      (:EndOfTrackMsg (event2end-of-track-msg ev))
      (t (print (format nil "(cl-midi) message-type ~A isn't supported yet" type))  NIL))))

(defun event2textual (ev)
  (case (midi-evt-type ev)
    (:Lyric (event2lyric ev))
    (:CopyRight (event2copyright ev))
    (:SeqName (event2seqname ev))
    (:Textual (event2textual ev))
    (:InstrName (event2instrument ev))
    (:Marker (event2marker ev))
    (:CuePoint (event2cue-point ev))))

(defun seq2tracks (seq)
  (let ((tracks nil))
    (loop for ev in seq
       for msg = (make-messages-from-event ev)
       do (let* ((tracknum (or (midi-evt-ref ev) 0))
		 (trackpos (position tracknum tracks :key 'car :test '=)))
	    (if trackpos
		(setf (nth trackpos tracks) 
		      (list tracknum
			    (append (cadr (nth trackpos tracks)) (if (listp msg) msg (list msg)))))
		(push (list tracknum (if (listp msg) msg (list msg))) tracks))))
    (mapcar 'cadr (sort tracks '< :key 'car))))



(defun cl-midi-save-file (seq filename fileformat clicks)
  (declare (ignore timedef tracks))
  (let ((mf (make-instance 'midi:midifile :format fileformat :division clicks)))
    (setf (slot-value mf 'midi::tracks) (seq2tracks seq))
    #+lispworks(sys::ENSURE-DIRECTORIES-EXIST filename :verbose t)  ;;; !!! LW specific
    (midi:write-midi-file mf filename)
    filename))


;; (defun om-midi::cl-midi-send-evt (event &optional player)
;;   (print 'not-yet-set))

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

