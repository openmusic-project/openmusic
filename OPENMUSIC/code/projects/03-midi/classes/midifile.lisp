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

(in-package :om)

;=================================================
;Midi File Object
;=================================================

(defclass InternalMidiFile () 
  ((MidiFileName :initform nil :initarg :MidiFileName :accessor MidiFileName)
   (fileseq :initform nil :accessor fileseq)
   (tracks :initform nil :initarg :tracks :accessor tracks)))

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

(defmethod get-type-of-ed-box ((self MidiFile))  'OMMidiFilebox)

(defmethod real-dur ((self midifile)) (round (extent->ms self)))

(defmethod real-duration ((self midifile) time) 
  (values time (+ time (round (extent->ms self)))))

(defmethod copy-container ((self midifile) &optional (pere ()))
  (let ((copy (make-instance 'MidiFile))
        (slots  (class-instance-slots (find-class 'simple-container))))
    (when (MidiFileName self) 
      (setf (MidiFileName copy) (MidiFileName self))
      (setf (fileseq copy) (clone (fileseq self)))
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
;Midi track Object
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


;---------------------
(defmethod make-one-instance ((self midifile) &rest slots-vals)
   (get-midifile))

(defmethod good-val-p? ((self midifile))
   (midifilename self))

(defmethod Class-has-editor-p ((self MidiFile)) t)

(defmethod get-initval ((self MidiFile)) (make-instance 'midifile))


(defmethod* objfromObjs ((self t) (type MidiFile))
   (when self
       (objFromObjs (save-as-midi self) type)))

(defmethod* objfromObjs ((self string) (type MidiFile))
   (load-midi-file self))

(defmethod* objfromObjs ((self pathname) (type MidiFile))
   (load-midi-file self))



(defmethod default-obj-box-size ((self MidiFile)) (om-make-point 50 72))

(defmethod get-editor-class ((self MidiFile)) 'MidiEditor)

        
 ;=======EDITOR

(defclass OMMidiFilebox (OMBoxEditCall) ())

(defmethod get-frame-class ((self OMMidiFilebox)) 'boxmidiframe)

(omg-defclass boxmidiframe (boxEditorFrame) ())

(defmethod om-get-menu-context ((object boxmidiframe))
  (boxframe-default-list object))

(defmethod remove-extra ((self OMPatch) (box OMMidiFilebox))
   (when (and (value box) (fileseq (value box)))
      (om-midi-free-seq (fileseq (value box)))))


;(defun copy-midiseq-without-tempoevents (seq)
;   (om-midi-copy-seq seq '(:type 144)))

;ojo deberia ser standard
;notalist (midi ? vel ? ttime ?)

(defmethod get-obj-from-file ((type (eql 'mid)) filename)
  (load-midi-file filename))
(defmethod get-obj-from-file ((type (eql 'midi)) filename)
  (load-midi-file filename))

(defmethod* get-midifile () 
            :initvals nil :indoc nil :icon 148
            (let ((name (om-choose-file-dialog 
                         :directory (def-load-directory)
                         :prompt (om-str :choose-midi) :types (list (format nil (om-str :file-format) "MIDI") "*.mid;*.midi" 
                                                                            (om-str :all-files) "*.*"))))
              (when name
                (setf *last-loaded-dir* (pathname-dir name))
                (load-midi-file name))))

(defun load-midi (name)
  (om-load-if name 'load-midi-file))


;=====================================
;==== Midi file analysis functions ===
;=====================================

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
#|
(defmethod! get-midievents ((self midifile) &optional test)
  :initvals '(nil nil) 
  :indoc '("an OM object" "a test function")
  :doc "
Converts any OM object (<self>) to a list of MIDIEvents.

The optional argument <test> is a function or lambda patch testing MIDIEvents one by one.
If <test> returns T, then the MIDIEvent is collected.
"
  :icon 902
   (let ((seq (fileseq self))
         msevent event (rep nil))
     (when seq
       (setf msevent (om-midi-seq-first-evt seq))
       (loop while msevent do
             (setf event (make-instance 'MidiEvent   
                                        :ev-type (om-midi-evt-get msevent :type) 
                                        :ev-date (om-midi-evt-get msevent :date) 
                                        :ev-ref (om-midi-evt-get msevent :ref) 
                                        :ev-port (om-midi-evt-get msevent :port)
                                        :ev-chan (+ (om-midi-evt-get msevent :chan) 1)
                                        :ev-fields (om-midi-evt-get msevent :fields)))
             (if (= (om-midi-evt-get msevent :type) (om-midi-get-num-from-type "Tempo")) 
                 (setf (ev-fields event) (list (mstempo2bpm (car (om-midi-evt-get msevent :fields))))))
             (if (or (not test) (funcall test event))  
                 (setf rep (cons event rep)))
             (setf msevent (om-midi-next-evt msevent))
             )
       (reverse rep))))
|#

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
           (loop for evt in (fileseq self) collect 
                 (when (or (not test) (funcall test avt))  
                     (let ((evt2 (clone evt)))
                       (when (equal (ev-type evt2) 'Tempo)
                         (setf (ev-fields evt2) (list (mstempo2bpm (car (ev-fields evt))))))
                       evt2)))
           ))
                   

;=== Creates a string with Lyric events from Midi file
#|
(defmethod! get-mf-lyrics ((self midifile))
  :initvals '(nil) 
  :indoc '("a MIDI file or sequence") 
  :numouts 2
  :doc "Extracts lyrics (event type 'Lyric') from <self>.

The second output returns the corresponding dates"
  :icon '(908)
  (let ((seq (fileseq self))
         event str rep dates)
     (when seq
       (setf event (om-midi-seq-first-evt seq))
       (loop while event do
             (when (= (om-midi-evt-get event :type) (om-midi-get-num-from-type "Lyric"))
                 (setf rep (cons (om-midi-get-evt-text event) rep))
                 (setf dates (cons (om-midi-evt-get event :date) dates)))
             (setf event (om-midi-next-evt event))
             ))
     (values (reverse rep) (reverse dates))))
|#

(defmethod! get-mf-lyrics ((self midifile))
  :initvals '(nil) 
  :indoc '("a MIDI file or sequence") 
  :numouts 2
  :doc "Extracts lyrics (event type 'Lyric') from <self>.

The second output returns the corresponding dates"
  :icon '(908)
  (let ((rep
         (mat-trans (loop for evt in (fileseq self)  
               when (equal (ev-type evt) 'Lyric) 
               collect (list (if (stringp (car (ev-fields evt))) (ev-fields evt) (list2string (ev-fields evt)))
                             (ev-date evt)))
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



;--------------
; MINIVIEW
;--------------

(defun draw-track-mini (self track minx maxx miny maxy mode)
  (declare (ignore mode))
  (let* ((x-notes (give-notes-in-x-range track minx maxx))
         (notes (sort (give-notes-iny-range x-notes miny maxy) '< :key 'second))
         (ysize (round (h self) (- maxy miny))))
    (drawmini-track-note self (list minx maxx miny maxy)  notes ysize)))

(defun drawmini-track-note (self ranges notes ysize)
  (declare (ignore ysize))
  (loop for note in notes do
        (let* ((topleft (point-to-pixel-with-sizes ranges (om-make-big-point (second note) (first note)) (w self) (h self)))
               (rigth (round  (* (third note) (/ (w self) (second ranges))))))
          (om-fill-rect (om-point-h topleft) (om-point-v topleft) rigth 1))))

(defmethod draw-mini-view ((self miniview) (value MidiFile))
   (if (midifilename value)
     (om-with-focused-view self
       (draw-mini-midi self 5000 value))
     (om-with-focused-view self
       (om-draw-string 5 15 "No file attached"))
     ))

(defun draw-mini-midi (self x val) 
   (declare (special *16-color-list*))
   (loop for item in (tracks val)
         for i = 0 then (+ i 1) do
         (om-with-fg-color nil (nth (mod i 15) *16-color-list*)
           (draw-track-mini self item 0 x 24 96 t))))


(defmethod draw-obj-in-rect ((self  midifile) x x1 y y1 edparams  view)
  (loop for item in (tracks self)
         for i = 0 then (+ i 1) do
         (om-with-fg-color nil (nth (mod i 15) *16-color-list*)
           (let* ((x-notes (give-notes-in-x-range item 0 5000))
                  (notes (sort (give-notes-iny-range x-notes 24 96) '< :key 'second))
                  )
             (loop for note in notes do
                   (let* ((topleft (point-to-pixel-with-sizes '(0 5000 24 96) (om-make-big-point (second note) (first note)) (- x1 x) (- y1 y)))
                          (rigth (round  (* (third note) (/ (- x1 x) 5000)))))
                     (when (> (+ rigth (om-point-h topleft)) (- x1 x))
                       (setf rigth (- (- x1 x) (om-point-h topleft))))
                     (om-fill-rect (+ x (om-point-h topleft)) (+ y (om-point-v topleft)) rigth 1)))))))

(defmethod update-miniview ((self t) (type midifile)) 
   (om-invalidate-view self t))

;======================================================
;USED IN MAQUETTES
;======================================================

(defmethod draw-editor-mode ((self midifile) view)
  (draw-carre view t)
  (om-with-focused-view view
    (draw-mini-midi view (real-dur self) self)))


(defmethod update-miniview ((self tempobjframe) (value midifile))
   (om-invalidate-view self t))
