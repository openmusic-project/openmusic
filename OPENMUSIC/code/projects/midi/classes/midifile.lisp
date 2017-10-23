;=========================================================================
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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;;; MIDI package

(in-package :om)

;=================================================
;Midi File Object
;=================================================

(defclass InternalMidiFile () 
  ((MidiFileName :initform nil :initarg :MidiFileName :accessor MidiFileName)
   (fileseq :initform nil :accessor fileseq)
   (tracks :initform nil :initarg :tracks :accessor tracks)
   (controllers :initform nil :initarg :controllers :accessor controllers)))

(defclass* MidiFile (midi-score-element InternalMidiFile) () 
  (:icon 904)
  (:documentation "
A MIDI file on your computer disk.

MidiFile is a pointer to an existing file. 
I can be initialized with a pathname connected to <self> or simply by evaluating the box and choosing a file.

Midifile factories can also be obtained by dragging a MIDI file from the finder to a patch window. 

Lock the box ('b') in order to keep the current pointer and not reinitialize the MidiFile.
")
  )

(add-player-for-object 'midifile :midi-player)

(defmethod default-edition-params ((self midifile))
  (pairlis '(outport player)
           (list *def-midi-out* :midi-player)))

(defmethod make-one-instance ((self midifile) &rest slots-vals)
   (get-midifile))

(defmethod good-val-p? ((self midifile))
   (midifilename self))

(defmethod get-initval ((self MidiFile)) (make-instance 'midifile))

(defmethod real-dur ((self midifile)) (round (extent->ms self)))

(defmethod real-duration ((self midifile) time) 
  (values time (+ time (round (extent->ms self)))))

(defmethod copy-container ((self midifile) &optional (pere ()))
  (let ((copy (make-instance 'MidiFile))
        (slots  (class-instance-slots (find-class 'simple-container))))
    (when (MidiFileName self) 
      (setf (MidiFileName copy) (MidiFileName self))
      (setf (fileseq copy) (mapcar 'om-midi::copy-midi-evt (fileseq self)))
      (setf (tracks copy) (loop for track in (tracks self) 
                                collect  (make-instance 'MidiTrack
                                           :midinotes  (midinotes track))))
      (setf (slot-value copy 'parent) pere)
      (loop for slot in slots
            when (not (eq (slot-definition-name slot) 'parent))
            do (setf (slot-value  copy  (slot-definition-name slot))
                     (copy-container (slot-value self  (slot-definition-name slot)) copy))))
    copy))

(defmethod execption-save-p ((self midifile)) 'midifile)

(defmethod save-exepcion ((self midifile)) 
  (and (MidiFileName self) 
       (register-resource :midi (MidiFileName self))
       `(load-midi ,(om-save-pathname-relative (MidiFileName self)))))


;;; Maquette interface
(defmethod allowed-in-maq-p ((self MidiFile))  (good-val-p? self))
(defmethod get-obj-dur ((self MidiFile)) (extent self))
(defmethod allow-strech-p ((self MidiFile) (factor number)) nil)

(defmethod get-name ((self MidiFile))
  (or (get-filename (MidiFileName self)) ""))
  

;=================================================
;Midi track object
;=================================================

(defclass MidiTrack () 
  ((midinotes :initform nil :initarg :midinotes :accessor midinotes)))

;--------------------set notes to Track ------------------
(defmethod cons-midi-track ((self MidiTrack) notes)
  (setf (midinotes self) notes))


;-------------------get the notes fall in the interval (x1 x2) ------------------------------
(defmethod give-notes-in-x-range ((self MidiTrack) x1 x2)
  (let ((notes (midinotes self))
        res-notes)
    (loop while (and notes (> x1 (second (car notes)))) do
          (pop notes))
    (loop while (and notes (> x2 (second (car notes)))) do
          (push (pop notes) res-notes))
    (nreverse res-notes)))

;-------------------get the notes fall in the interval (y1 y2) ------------------------------
(defun give-notes-iny-range (notes y1 y2)
  (let (res-notes)
    (loop for note in notes do
          (when (and (>= y2 (first note)) (<= y1 (first note)))
            (push note res-notes)))
    (nreverse res-notes)))

;=================================================
; LOAD FROM FILE
;=================================================

;;; MAKE A MIDIFILE INSTANCE FROM FILE
(defun load-midifile (name) 
  (let ((themidiFile (make-instance 'MidiFile))
	track-list)
    (om-print (string+ "Loading MIDI file: " (namestring name) " ..."))
    (multiple-value-bind (seq nbtracks clicks format)
        (midi-load-file (namestring name))
      ;(print (list "format" format))
      ;(print (list "clicks" clicks))
      
      (when (equal seq :error) (om-beep-msg (string+ "Error loading a MIDI file " (namestring name))) (om-abort))
      (setf (MidiFileName themidiFile) name)
      (when seq
       	(setf (fileseq themidiFile) (convert-tempo-info seq clicks))
	(setf track-list (make-list nbtracks :initial-element nil))
	
 
        ;;; (pitch date dur vel chan ref port)
   	(loop for note in (midievents2midilist (fileseq themidiFile)) 
              when (plusp (third note))  ;;; dur > 0
              do (push (list (first note) (second note) (third note) (fourth note) (fifth note)) (nth (sixth note) track-list)))
	(setf (extent themidiFile) (loop for track in track-list 
                                         if track maximize (+ (third (car track)) (second (car track)))))
        (setf (Qvalue themidiFile)  1000)
        (setf (tracks themidiFile) (loop for track in track-list 
                                         if track collect 
                                         (make-instance 'MidiTrack :midinotes (reverse track))))
        (setf (controllers themidiFile) (get-continuous-controllers themidifile))
        themidifile)))) 


(defmethod* get-midifile () 
            :initvals nil :indoc nil :icon 148
            (let ((name (om-choose-file-dialog 
                         :directory (def-load-directory)
                         :prompt (om-str :choose-midi) :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi" 
                                                                            (om-str :all-files) "*.*"))))
              (if name
                (progn
                  (setf *last-loaded-dir* (pathname-dir name))
                  (load-midifile name))
                (om-abort))))

(defun load-midi (name)
  (om-load-if name 'load-midifile))

(defmethod* objfromObjs ((self t) (type MidiFile))
   (when self
       (objFromObjs (save-as-midi self) type)))

(defmethod* objfromObjs ((self string) (type MidiFile))
   (load-midifile self))

(defmethod* objfromObjs ((self pathname) (type MidiFile))
   (load-midifile self))


;=================
; FUNCTIONS
;=================
(defmethod* mf-info ((self midifile) &optional (tracknum nil))
  :initvals (list nil nil ) :indoc '("a Midifile object" "a track number or nil")
   :doc "Converts a Midifile object into a symbolic description.
The result of mf-info is a list of tracks. Each track is a list of notes. 
Each note is a list of parameters in the form :

(midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)

optional <tracknum> (a number in 0-15) allows to choose a single track."
  :icon 148
  (if tracknum
    (midinotes (nth tracknum (tracks self)))
    (loop for item in (tracks self)
          collect (midinotes item))))

(defmethod* mf-info-mc ((self midifile) &optional (tracknum nil))
            :initvals (list nil nil ) :indoc '("a Midifile object" "a track number or nil")
            :doc "Converts a Midifile object into a symbolic description.
The result of mf-info is a list of tracks. Each track is a list of notes. 
Each note is a list of parameters in the form :

(midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)

optional <tracknum> (a number in 0-15) allows to choose a single track."
            :icon 148
            (if tracknum
                (midinotes (nth tracknum (tracks self)))
              (loop for item in (tracks self)
                    collect (midinotes item))))


;=== Creates MidiEvent list with all Midi events 
;=== optionnaly filtered with a test function

(defmethod! get-midievents ((self midifile) &optional test)
  :initvals '(nil nil) 
  :indoc '("an OM object" "a test function")
  :doc "
Converts any OM object (<self>) to a list of MIDIEvents.

The optional argument <test> is a function or lambda patch testing MIDIEvents one by one.
If <test> returns T, then the MIDIEvent is collected.
"
  :icon 902
   (remove nil
           (loop for e in (fileseq self) collect 
                 (let ((event (make-instance 'MidiEvent
                                            :ev-date (om-midi::midi-evt-date e)
                                            :ev-type (om-midi::midi-evt-type e)
                                            :ev-chan (om-midi::midi-evt-chan e)
                                            :ev-ref (om-midi::midi-evt-ref e)
                                            :ev-port (om-midi::midi-evt-port e)
                                            :ev-fields (if (equal (om-midi::midi-evt-type e) :Tempo) 
                                                           (list (mstempo2bpm (om-midi::midi-evt-tempo e)))
                                                         (om-midi::midi-evt-fields e))
                                            )))
                   (when (or (not test) (funcall test event))
                     event))
                 
                 )))

   

;=== Creates a string with Lyric events from Midi file

(defmethod! get-mf-lyrics ((self midifile))
  :initvals '(nil) 
  :indoc '("a MIDI file or sequence") 
  :numouts 2
  :doc "Extracts lyrics (event type 'Lyric') from <self>.

The second output returns the corresponding dates"
  :icon '(908)
  (let ((rep
         (mat-trans (loop for evt in (fileseq self)  
                          when (equal (om-midi::midi-evt-type evt) :Lyric)
                          collect (list (if (stringp (car (om-midi::midi-evt-fields evt))) (om-midi::midi-evt-fields evt) (list2string (om-midi::midi-evt-fields evt)))
                                        (om-midi::midi-evt-date evt)))
                    )))
    (values (car rep) (cadr rep))))


(defmethod! get-midi-notes ((self midifile))
  :initvals '(nil) 
  :indoc '("a MIDI file or sequence") 
  :doc "Extracts and returns the notes from <self>.

The result is a list of lists where each first level list represents a MIDI track and contains a list of note values.
Note values are lists of (pitch date dur vel chan).

"
  :icon 909
  (mf-info self))


;========== conversions

(defmethod* objFromObjs ((self midifile) (type chord-seq))
  (let ((newcs (make-instance (type-of type)))
        (midilist (sort (loop for item in (mf-info-mc self)
                              append item) '< :key 'second))) 
    (setQValue newcs 1000 :recursive nil)
    (setf (inside newcs) nil)
    (setf (inside newcs)  (make-quanti-chords midilist *global-deltachords*))
    (adjust-extent newcs)
    (QNormalize newcs)
    newcs
    ))

(defmethod* objFromObjs ((self midifile) (type multi-seq))
  (let ((midilist (mf-info-mc self)) rep)
    (loop for item in midilist do
          (let ((newcs (make-instance 'chord-seq)))
            (setQValue newcs 1000 :recursive nil)
            (setf (inside newcs) nil)
            (setf (inside newcs) (make-quanti-chords item *global-deltachords*))
            (QNormalize newcs)
            (push newcs rep)))
    (make-instance 'multi-seq
      :chord-seqs (reverse rep))))

;=====================
; MIDIFILE BOX
;=====================

(defclass OMMidiFilebox (OMBoxEditCall) ())

(defmethod get-type-of-ed-box ((self MidiFile))  'OMMidiFilebox)
(defmethod default-obj-box-size ((self MidiFile)) (om-make-point 50 72))
(defmethod get-frame-class ((self OMMidiFilebox)) 'boxmidiframe)

(defclass boxmidiframe (boxEditorFrame) ())

;(defmethod om-get-menu-context ((object boxmidiframe))
;  (boxframe-default-list object))

;(defmethod remove-extra ((self OMPatch) (box OMMidiFilebox)) 
;   (when (and (value box) (fileseq (value box)))
;      (om-midi-free-seq (fileseq (value box)))))


;(defun copy-midiseq-without-tempoevents (seq)
;   (om-midi-copy-seq seq '(:type 144)))

;ojo deberia ser standard
;notalist (midi ? vel ? ttime ?)

(defmethod get-obj-from-file ((type (eql 'mid)) filename)
  (load-midifile filename))
(defmethod get-obj-from-file ((type (eql 'midi)) filename)
  (load-midifile filename))

;======================================================
; MINIVIEW
;======================================================



(defun drawmini-track-notes (view ranges y1 y2 notes)
    (declare (special *16-color-list*))
  (loop for note in notes do
        (let* ((topleft (point-to-pixel-with-sizes ranges (om-make-point (second note) (first note)) (w view) (- y2 y1)))
               (width (round  (* (third note) (/ (w view) (second ranges))))))
          (om-with-fg-color nil (nth (mod (1- (nth 4 note)) 16) *16-color-list*)
            (om-fill-rect (om-point-h topleft) (+ y1 (om-point-v topleft)) (max width 1) 2)))))

(defun draw-track-mini (view track mint maxt y1 y2)
  (let* ((minpitch 24) 
         (maxpitch 96)
         (x-notes (give-notes-in-x-range track mint maxt))
         (notes (sort (give-notes-iny-range x-notes minpitch maxpitch) '< :key 'second)))
        
    (om-draw-line 0 y2 (w view) y2)
    (drawmini-track-notes view (list mint maxt minpitch maxpitch) y1 y2 notes)))

(defun draw-mini-midi (view dur val) 
  (let ((trackh (if (tracks val) (round (h view) (length (tracks val))) (h view))))
    (loop for track in (tracks val)
          for i = 0 then (+ i 1) do
          (draw-track-mini view track 0 dur (* i trackh) (* (1+ i) trackh)))))

(defmethod draw-mini-view ((self miniview) (value MidiFile))
   (if (midifilename value)
     (om-with-focused-view self
       (draw-mini-midi self (real-dur value) value))
     (om-with-focused-view self
       (om-draw-string 5 15 "No file attached"))
     ))



;(defmethod draw-obj-in-rect ((self  midifile) x x1 y y1 edparams  view)
;  (let ((dur-to-draw (real-dur self)))
;    (loop for tr in (tracks self)
;          for i = 0 then (+ i 1) do
;          (om-with-fg-color nil (nth (mod i 15) *16-color-list*)
;            (let* ((x-notes tr) ;(give-notes-in-x-range item 0 dur-to-draw))
;                   (notes (sort (give-notes-iny-range x-notes 24 96) '< :key 'second)))
;              (loop for note in notes do
;                    (let* ((topleft (point-to-pixel-with-sizes '(0 dur-to-draw 24 96) (om-make-big-point (second note) (first note)) (- x1 x) (- y1 y)))
;                           (rigth (round  (* (third note) (/ (- x1 x) dur-to-draw)))))
;                      (when (> (+ rigth (om-point-h topleft)) (- x1 x))
;                        (setf rigth (- (- x1 x) (om-point-h topleft))))
;                      (om-fill-rect (+ x (om-point-h topleft)) (+ y (om-point-v topleft)) rigth 1))))))))

;(defmethod update-miniview ((self t) (type midifile)) 
;   (om-invalidate-view self t))

;======================================================
;USED IN MAQUETTES
;======================================================

(defmethod draw-editor-mode ((self midifile) view)
  (draw-carre view t)
  (om-with-focused-view view
    (draw-mini-midi view (real-dur self) self)))


(defmethod update-miniview ((self tempobjframe) (value midifile))
   (om-invalidate-view self t))






