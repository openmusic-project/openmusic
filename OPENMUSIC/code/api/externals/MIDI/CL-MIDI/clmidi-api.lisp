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

;;; smf refs:
;;;
;;; http://acad.carleton.edu/courses/musc108-00-f14/pages/04/04StandardMIDIFiles.html
;;; http://cs.fit.edu/~ryan/cse4051/projects/midi/midi.html
;;;


;;; FILE OUTPUT: building useful midi-messages, writing SMF's:
;;;
(defconstant +note-off-opcode+		#x80)
(defconstant +note-on-opcode+		#x90)
(defconstant +key-pressure-opcode+	#xA0)
(defconstant +control-change-opcode+	#xB0)
(defconstant +program-change-opcode+	#xC0)
(defconstant +channel-pressure-opcode+	#xD0)
(defconstant +pitch-bend-opcode+	#xE0)
(defconstant +tempo-opcode+		#x51)
(defconstant +time-signature-opcode+	#x58)
(defconstant +key-signature-opcode+	#x59)
(defconstant +midi-port-opcode+		#x21)
(defconstant +end-of-track-opcode+	#x2F)
(defconstant +reset-all-controllers-message-opcode+ #xB0)
(defconstant +all-notes-off-message-opcode+ #xB0)
(defconstant +copyright-opcode+		#x02)
(defconstant +sequence/track-name-opcode+ #x03)
(defconstant +instrument-opcode+	#x04)
(defconstant +lyric-opcode+		#x05)
(defconstant +marker-opcode+		#x06)
(defconstant +cue-point-opcode+		#x07)

;; VOICE MESSAGES

(defmethod midi-channel ((event t)) nil)
(defmethod midi-channel ((event midi::channel-message))
  ;; used where channel isn't set explicit in instance
  (- (slot-value event 'midi::status) (slot-value event 'midi::status-min)))


(defmethod midi-message-time ((msg midi::message)) (midi::message-time msg))

(defmethod midi-message-channel ((msg midi::channel-message)) (midi::message-channel msg))
(defmethod midi-message-channel ((msg t)) -1)

;;; Accessors to define for the different types of MIDI messages
(defmethod midi-message-type ((msg t)) (intern (concatenate 'string "clmidi-api: unknown + " (string (type-of msg)))))
(defmethod midi-message-fields ((msg t)) nil)


;; NOTE OFF

(defmethod midi-message-type ((msg midi::note-off-message)) :KeyOff)

(defmethod midi-message-fields ((msg midi::note-off-message))
  (list (midi::message-key msg) (midi::message-velocity msg)))

(defun event2note-off (ev)
  (let* ((fields (midi-evt-fields ev))
	 (key (first fields))
	 (vel (second fields))
	 (chan (1- (midi-evt-chan ev))))		    ;TODO: find where this goes to 1-based offset
    (make-instance 'midi:note-off-message
		   :time (midi-evt-date ev)
		   :key key
		   :velocity vel
		   :status (logior +note-off-opcode+ chan))))

(defun make-note-off-message (time key vel chan)
  (make-instance 'midi:note-off-message :key key :time time :velocity vel :status (logior +note-off-opcode+ chan)))

;; NOTE ON

(defmethod midi-message-type ((msg midi::note-on-message)) :KeyOn)

(defmethod midi-message-fields ((msg midi::note-on-message))
  (list (midi::message-key msg) (midi::message-velocity msg)))

(defun event2note-on (ev)
  (let* ((fields (midi-evt-fields ev))
	 (key (first fields))
	 (vel (second fields))
	 (chan (1- (midi-evt-chan ev))))
    (make-instance 'midi:note-on-message
		   :time (midi-evt-date ev)
		   :key key
		   :velocity vel
		   :status (logior +note-on-opcode+ chan))))

(defun make-note-on-message (time key vel chan)
  (make-instance 'midi:note-on-message :key key :time time :velocity vel :status (logior +note-on-opcode+ chan)))

;; PROGRAM CHANGE

(defmethod midi-message-type ((msg midi:program-change-message)) :ProgChange)

(defmethod midi-message-fields ((msg midi:program-change-message)) (list (midi::message-program msg)))

(defun event2program-change-message (ev)
  (make-instance 'midi:program-change-message
		 :time (midi-evt-date ev)
		 :program (first (midi-evt-fields ev))
		 :status (logior +program-change-opcode+ (1- (midi-evt-chan ev)))))

;; CONTROL CHANGE

(defmethod midi-message-type ((msg midi::control-change-message)) :CtrlChange)

(defmethod midi-message-fields ((msg midi::control-change-message))
  (list (slot-value msg 'midi::controller) (slot-value msg 'midi::value)))

(defun event2control-change (ev)
  (let* ((fields (midi-evt-fields ev))
	 (controller (first fields))
	 (value (second (midi-evt-fields ev))))
    (make-instance 'midi::control-change-message
		   :time (midi-evt-date ev)
		   :controller controller
		   :value value
		   :status (logior +control-change-opcode+ (1- (midi-evt-chan ev))))))

;; PITCH BEND

(defmethod midi-message-type ((msg midi::pitch-bend-message)) :PitchBend)

(defmethod midi-message-fields ((msg midi::pitch-bend-message))
  (list (midi::message-value msg)))

(defun event2pitch-bend (ev)
  (when (midi-evt-fields ev)
    (make-instance 'midi::pitch-bend-message
		   :time (midi-evt-date ev)
		   :value (first (midi-evt-fields ev))
		   :status (logior +pitch-bend-opcode+ (1- (midi-evt-chan ev))))))

;; TEXT MESSAGES

(defmethod midi-message-type ((msg midi::general-text-message)) :Textual)
(defmethod midi-message-type ((msg midi:sequence/track-name-message)) :SeqName)
(defmethod midi-message-type ((msg midi::instrument-message)) :InstrName)
(defmethod midi-message-type ((msg midi::lyric-message)) :Lyric)
(defmethod midi-message-type ((msg midi::copyright-message)) :Copyright)

;;; Superclass for all text messages
(defmethod midi-message-fields ((msg midi::text-message))
  (map 'list #'char-code (slot-value msg 'midi::text)))  ;; restore the list of ASCII.. ?

(defun event2copyright (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::copyright-message :time time :status +copyright-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defun event2seqname (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::sequence/track-name-message :time time :status +sequence/track-name-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defun event2instrument (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::instrument-message :time time :status +instrument-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defun event2lyric (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::lyric-message :time time :status +lyric-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defun event2marker (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::marker-message :time time :status +marker-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

(defun event2cue-point (ev)
  (let ((time (midi-evt-date ev))
	(value (first (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::cue-point-message :time time :status +cue-point-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

;; TEMPO

(defmethod midi-message-type ((msg midi::tempo-message)) :Tempo)

(defmethod midi-message-fields ((msg midi::tempo-message)) (list (midi::message-tempo msg)))

(defun event2tempo (ev)
  (make-instance 'midi:tempo-message
		 :time (midi-evt-date ev)
		 :tempo (first (midi-evt-fields ev))
		 :status +tempo-opcode+))

;; TIME SIGNATURE

(defmethod midi-message-type ((msg midi::time-signature-message)) :TimeSign)

(defmethod midi-message-fields ((msg midi::time-signature-message))
  (list (midi::message-numerator msg)
        (midi::message-denominator msg)			    ;power of 2
	(slot-value msg 'midi::cc)			    ;midi clocks pr. metronome click
        (slot-value msg 'midi::bb)))			    ;n 32nd notes notated per quarter note

(defun event2time-signature (ev)
  (let ((inst (make-instance 'midi::time-signature-message
			     :time (midi-evt-date ev)
			     :status +time-signature-opcode+))
	(data (midi-evt-fields ev)))
    (setf (slot-value inst 'midi::nn) (first data)
	  (slot-value inst 'midi::dd) (second data)
	  (slot-value inst 'midi::cc) (third data)
	  (slot-value inst 'midi::bb) (fourth data))
    inst))


;; KEY SIGNATURE

(defmethod midi-message-type ((msg midi::key-signature-message)) :KeySign)

(defmethod midi-message-fields ((msg midi::key-signature-message))
  (list (midi::message-sf msg)
        (midi::message-mi msg)))

(defun event2key-signature (ev)
  (let ((inst (make-instance 'midi::key-signature-message
			     :time (midi-evt-date ev)
			     :status +key-signature-opcode+)))
    (setf (slot-value inst 'midi::sf) (first (midi-evt-fields ev))
	  (slot-value inst 'midi::mi) (second (midi-evt-fields ev)))
    inst))

;;
;; MIDI PORT MESSAGE
;;
(defmethod midi-message-type ((msg midi::midi-port-message)) :MidiPortMsg)

(defmethod midi-message-fields ((msg midi::midi-port-message))
  (list (slot-value msg 'midi::port)))

(defun event2midi-port-msg (ev)
  (let ((inst (make-instance 'midi::midi-port-message :time (midi-evt-date ev)
			     :status +midi-port-opcode+)))
    (setf (slot-value inst 'midi::port) (midi-evt-fields ev))
    inst))


;;; SUPERCLASS FOR ALL MODE MESSAGES

(defmethod midi-message-fields ((msg midi::mode-message))
  (list (slot-value msg 'midi::channel)))
;;
;; EndOfTrack
;;
(defmethod midi-message-type ((msg midi::end-of-track-message)) :EndOfTrackMsg)

(defun event2end-of-track-msg (ev)
  (let ((inst (make-instance 'midi::end-of-track-message
			     :time (midi-evt-date ev)
			     :status +end-of-track-opcode+)))
    (setf (slot-value inst 'midi::status) (midi-evt-fields ev))
    inst))


;; :ResetAllControllers
(defmethod midi-message-type ((msg midi::reset-all-controllers-message)) :ResetAllControllers)

(defun event2reset-all-controllers-message (ev)
  (make-instance 'midi::reset-all-controllers-message
		 :time (midi-evt-date ev)
		 :status (logior +reset-all-controllers-message-opcode+ (1- (midi-evt-chan ev)))))

;; :AllNotesOff

(defmethod midi-message-type ((msg midi::all-notes-off-message)) :AllNotesOff)

(defun event2all-notes-off-message (ev)
  (make-instance 'midi::all-notes-off-message
		 :time (midi-evt-date ev)
		 :status (logior +all-notes-off-message-opcode+ (1- (midi-evt-chan ev)))))


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
                 :date (midi::message-time msg)
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

(defun make-messages-from-event (ev)
  (let ((type (midi-evt-type ev)))
    (case type
      (:Note (event2note-on-off ev))			    ;returns cons
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
  (let ((mf (make-instance 'midi:midifile :format fileformat :division clicks)))
    (setf (slot-value mf 'midi::tracks) (seq2tracks seq))
    #+lispworks(sys::ENSURE-DIRECTORIES-EXIST filename :verbose t) ;;; !!! LW specific
    (midi:write-midi-file mf filename)
    filename))
