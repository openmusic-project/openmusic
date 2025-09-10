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
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================

(in-package :om)

(defclass named-score-object (named-object) ())
;==========================================================================
; OpenMusic : ScoreObjects.lisp
;==========================================================================

;==========================================================================
;    Class definitions
;==========================================================================

;;; SEE ANALYSIS FRAMEWORK
(defclass! analyse-object ()
  ((analysis :accessor analysis :initform nil :documentation "analysis system(s)")))

(defmethod analysis ((self t)) nil)

(defclass* simple-score-element (simple-container) () (:icon 140))
(defclass* score-element (container analyse-object object-in-box) () (:icon 138))
(defclass* superposition (score-element) () (:icon 231))
(defclass* sequence* (score-element) () (:icon 230))
(defclass* metric-sequence (sequence* tonal-object) () (:icon 230))

(add-player-for-object 'score-element '(:midi-player :osc-scoreplayer :microplayer))
(add-player-for-object 'simple-score-element '(:midi-player :osc-scoreplayer :microplayer))

(defmethod players-for-object ((self score-element)) 
  (if *force-score-player* 
      (list *default-score-player*)
    (call-next-method)))


(defclass* note (simple-score-element object-in-box tonal-object)
  ((midic :initform 6000 :accessor midic :initarg :midic :type number :documentation "pitch (midicents)")
   (vel :initform 80 :accessor vel :initarg :vel :type number :documentation "velocity (0-127)")
   (dur :initform 1000 :accessor dur :initarg :dur :type number :documentation "duration (ms)")
   (chan :initform 1 :accessor chan :initarg :chan :type integer :documentation "MIDI channel (1-16)")
   (port :initform nil :accessor port)
   (tie :initform nil :accessor tie)
   (approx :initform *global-midi-approx* :accessor approx :type integer)
   (symb-info :initform nil :accessor symb-info))
  (:icon 137)
  
  (:documentation "
A simple NOTE defined with :

- pitch (midicents: 100 = 1 half-tone - 6000 = C3)
- velocity (MIDI velocity from 0 to 127)
- duration in milliseconds
- MIDI channel 
")
  )

(defmethod get-obj-dur ((self note)) (dur self))

(defmethod real-dur ((self note)) 
  ;(round (* (dur self) (/ 60 (QTempo self) ))))
  (case (tie self)
    (begin (loop for obj = self then (next-tied-note obj)
                 while obj
                 sum (extent->ms obj)))
    (t (extent->ms self))))

(defmethod real-duration ((self note) time)
  (values time (+ time (dur self))))

(defmethod LChan ((self note))
  (list (chan self)))

;-----grace notes
(defclass gn-object ()
  ((gnotes :accessor gnotes :initarg :gnotes :initform nil)))

(om-with-redefinitions
 (defclass* rest (simple-score-element tonal-object gn-object) () 
  (:icon 142)))


 
(defclass* chord (superposition tonal-object gn-object)  
  ((Lmidic :initform (list 6000) :accessor LMidic :initarg :LMidic :type list :documentation "pitches (list of midicents)")
   (LVel :initform (list 100) :accessor LVel :initarg :LVel :type list :documentation "velocities (list of values 0-127)")
   (LOffset :initform (list 0) :accessor LOffset :initarg :LOffset :type list :documentation "offsets (list of values in ms)")
   (Ldur :initform (list 1000) :accessor Ldur :initarg :Ldur :type list :documentation "durations (list of values in ms)")
   (LChan :initform (list 1) :accessor LChan :initarg :LChan :type list :documentation "MIDI channels (list of values 0-16)")
   (approx :initform *global-midi-approx* :accessor approx  :type integer)
   ) 
  (:icon 139)
  
  (:documentation "
A CHORD object (set of simultaneous notes) defined with 

- list of pitches (midicents: 100 = 1 half-tone - 6000 = C3)
- velocities (MIDI velocity from 0 to 127)
- offsets (delay of notes after the actual chord onset)
- durations in milliseconds
- MIDI channels for each note

"))




(defclass* poly (superposition tonal-object) 
  ((voices :initform (list (make-instance 'voice )) :initarg :voices :accessor voices :type T :documentation "list of VOICE objects")
   (approx :initform *global-midi-approx* :accessor approx  :type integer)) 
  (:icon 224)
  (:documentation "
POLY is a polyphonic object made of a superimposition of VOICE objects.
")
  )

;;; probleme de l'initialisation de tree. Tree devrait etre au niveau de sequence* !!
(defclass* group (metric-sequence)  
  ((tree :initform '(1/4 (1 1 1))  :initarg :tree :type list)
   (approx :initform *global-midi-approx* :accessor approx  :type integer))
  (:icon 226)
 (:documentation "
An OM object representing a group in a rhythm.
"))
(defclass* measure (metric-sequence)  
  ((tree :initform '((4 4) (1 1 1 1))  :initarg :tree :type list)
   (approx :initform *global-midi-approx* :accessor approx  :type integer)) 
  (:icon 228)
(:documentation "
An OM object representing a measure in a rhythm.
"))

(defclass* voice (metric-sequence named-score-object) 
  ((tree :initform '(? (((4 4) (1 1 1 1))))
         :initarg :tree :documentation "a list representing a rhythm tree" :type list)
   (chords :initform (list  (make-instance 'chord))
           :accessor chords :initarg :chords :type t 
           :documentation "a chord object, a list of chords, a list of midics, a list of lists of midics...")
   (tempo :initform 60
          :accessor tempo :initarg :tempo :type t
          :documentation "frequency of the quarter-note (default 60/mn)")
   (legato :initform 100 :accessor legato :initarg :legato :type integer 
           :documentation "overlapping percentage between every successive chords, calculated from the second chord's duration")
   (ties :initform nil :accessor ties :initarg :ties :type list
         :documentation "sub lists (one sub list per chord) indicating notes to be tied to notes of the same value, in a next chord")
   (approx :initform *global-midi-approx* :accessor approx  :type integer))
  (:icon 223)
  
  (:documentation "
A sequence of chords with rhytmic structure.

Chords and rhythm data are specified and accessed separately.
The rhythmic structure (<tree>) is given in the form of a Rhythm Tree (RT - See OM User Manual for more details) 

")
  )


;==========================================================================
;   INTERFACE
;==========================================================================

#|

(defmethod poly-p ((self poly)) t)
(defmethod poly-p ((self t )) nil )

(defmethod voice-p ((self voice)) t)
(defmethod voice-p ((self t )) nil )

(defmethod measure-p ((self measure)) t)
(defmethod measure-p ((self t )) nil )


(defmethod group-p ((self group)) t)
(defmethod group-p ((self t )) nil )

(defmethod note-p ((self note)) t)
(defmethod note-p ((self t )) nil )

(defmethod rest-p ((self rest)) t)
(defmethod rest-p ((self t )) nil )

(defmethod chord-p ((self chord)) t)
(defmethod chord-p ((self t )) nil )

(defmethod infra-group-p ((self simple-container )) 
   (or (chord-p self) (simple-container-p self)))

(defmethod note-or-chord-p ((self T)) (or (chord-p self) (note-p self)))


;by aaa
;(defmethod Objfromobjs ((Self T) (Type (Eql T))) )
;Construct a new object of type <type> by analyzing <self>, which is another object,
;a list of objects, etc. Example : a chord from a list of notes.

(defmethod set-relative-offset ((self sequence*)))
Computes and set the offset's of elements in the sequence from their extent
so elements are contiguous (as a sequence should be).

(defmethod adjust-extent ((self sequence* ))
Sets the extent of sequence* to be the exact sum of the extents of its elements.

(defmethod insert ((self sequence*) (element sequence*) &key (where :atEnd))
Inserts a sequence* into the <inside> slot of another sequence* (e.g., a new measure into a voice)
Modifies accordingly the <QValue> and <Extent> slots in the parent sequence*.
The <where> keyword takes value among : :atend, :atbegin (not yet), or an integer index into the
existing sequence.

(defmethod distribute-chords  ((self score-element) (chords list)))
Distributes a list of chords at the terminal level of <self>. Existing leaves of <self>
are removed, chords are cloned, their offset, extent and qvalue set to the corresponding ones
of the leaves their replace.

(defmethod measure-list ((self T) &key (class 'voice))
Applies to a rhythm tree in the form of a list or a metric-sequence and extracts the corresponding list
of measure signatures in the form (.. (numerator denominator) ..)

(defmethod delete-useless-containers ((self score-element)))
Destructively modifies a musical container so that groups containing a single element are
replaced by that element.

(defmethod blank-fill ((self score-element)))
Destructively modifies a musical container so that "holes" in the structure are replaced
by rests.

(defmethod voice->poly ((self voice)) 
(defmethod measure->voice ((self measure)) 
Embedding methods. Clone their argument.

(defmethod poly->voice ((self poly) num )
(defmethod voice->measure ((self voice) num)
Extraction methods.


(defmethod* ->Cmn ((self simple-container)) 
   
   |#


;==========================================================================
;   Predicates
;==========================================================================
(defmethod poly-p ((self poly)) t)
(defmethod poly-p ((self t )) nil )

(defmethod voice-p ((self voice)) t)
(defmethod voice-p ((self t )) nil )

(defmethod measure-p ((self measure)) t)
(defmethod measure-p ((self t )) nil )


(defmethod group-p ((self group)) t)
(defmethod group-p ((self t )) nil )

(defmethod note-p ((self note)) t)
(defmethod note-p ((self t )) nil )

(defmethod rest-p ((self rest)) t)
(defmethod rest-p ((self t )) nil )

(defmethod chord-p ((self chord)) t)
(defmethod chord-p ((self t )) nil )

(defmethod infra-group-p ((self simple-container )) 
   (or (chord-p self) (simple-container-p self)))

(defmethod note-or-chord-p ((self T)) (or (chord-p self) (note-p self)))


;================================================================================
;    Special access to slots. Redefinitions of slots. Initialize-instance methods
;================================================================================

;;;;;;;;; in scoreobject.lisp

(defmethod dur ((self note))
  (extent->ms self))

#|
(defmethod midic ((self note))
  ;(setf (approx self) (get-approx self))
  (approx-m (slot-value self 'midic) (approx self)))
|#

(defmethod midic ((self note))
  (slot-value self 'midic))


;enlever la premiere definition ?
(defmethod (setf dur) ((dur number) (self note))
  (setQValue self 1000)
  (setf (extent self) dur)
  self)
   
(defmethod (setf dur) ((dur number) (self note))
  (SetQValue self 1000)
  (setf (slot-value  self 'extent) dur )
  (QNormalize self)
  self)

;non sinon c'est default 2
;(defmethod (setf approx) ((approx number) (self note))
;  (call-next-method)
;  (setf (slot-value self 'approx) approx)
;  self)

(defmethod initialize-instance ((self note) &rest initargs &key (empty nil))
  (declare (ignore initargs))
  (call-next-method)
  
  (when (find-if 'consp (cadr (mat-trans (group-list initargs 2 'circular))))
    (om-beep-msg "Error NOTE attributes must be atomic value (not lists) !")
    (if (consp (midic self)) (setf (midic self) (car (midic self))))
    (if (consp (vel self)) (setf (vel self) (car (vel self))))
    (if (consp (dur self)) (setf (dur self) (car (dur self))))
    (if (consp (chan self)) (setf (chan self) (car (chan self))))
    )
      
  (unless empty 
    (setqvalue self 1000)
    (setf (slot-value self 'extent) (slot-value self 'dur))
    (qnormalize self))
  self)

;;; CHORDS
#|
(defmethod LMidic ((self chord))
  (loop for i in (inside self)
        collect (setf (approx i) (approx self)))
  (loop for chord in (inside self)
        collect (approx-m (midic chord) (approx self))))
|#

(defmethod LMidic ((self chord))
  (loop for i in (inside self)
        collect (setf (approx i) (approx self)))
  (loop for note in (inside self)
        collect (midic note)))


(defmethod LChan ((self chord))
  (loop for note in (inside self)
        collect (chan note)))

(defmethod Lvel ((self chord))
  (loop for note in (inside self)
        collect (vel note)))

(defmethod LDur ((self chord))
  (loop for note in (inside self)
        collect (dur note)))

(defmethod LOffset ((self chord))
  (loop for note in (inside self)
        collect (Offset->ms note)))

(defmethod LPort ((self chord))
  (get-port self))

(defmethod (setf Lmidic) ((LMidic list) (self chord))
  (do-initialize self 
                 :LMidic LMidic
                 :LVel (LVel self)
                 :LOffset (LOffset self)
                 :LDur (LDur self)
                 :LChan (LChan self)
                 :LPort (LPort self)))

(defmethod (setf LChan) ((LChan list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self)
                 :LVel (LVel self)
                 :LOffset (LOffset self)
                 :LDur (LDur self)
                 :LChan LChan
                 :LPort (LPort self)))

(defmethod (setf LVel) ((LVel list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self)
                 :LVel LVel
                 :LOffset (LOffset self)
                 :LDur (LDur self)
                 :LChan (LChan self)
                 :LPort (LPort self)))

(defmethod (setf LOffset) ((LOffset list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self)
                 :LVel (LVel self)
                 :LOffset LOffset
                 :LDur (LDur self)
                 :LChan (LChan self)
                 :LPort (LPort self)))

(defmethod (setf LDur) ((Ldur list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self) 
                 :LVel  (LVel self) 
                 :LOffset (LOffset self)
                 :LDur LDur
                 :LChan (LChan self)
                 :LPort (LPort self)))

(defmethod (setf LPort) ((LPort list) (self chord))
  (loop for port in LPort
        for note in (inside self)
        do (set-port note port))
  self)


(defmethod (setf approx) ((approx number) (self chord))
  (call-next-method)
  #|
  (when (gnotes self)
    (loop for i in (glist (gnotes self))
            do (setf (approx i) approx)))
  |#
  (loop for i in (inside self)
        do (setf (approx i)  approx))
  self)

;;; CHORDS 

(defmethod initialize-instance ((self chord) &rest initargs  &key (Empty nil) (NoteType 'note) (LPort nil)) 
  (declare (ignore initargs)) 
  (call-next-method)
  (unless Empty
    (do-initialize self 
                   :LMidic (slot-value  self 'LMidic) 
                   :LVel (slot-value  self 'LVel)
                   :LOffset (slot-value  self 'LOffset)
                   :LDur (slot-value  self 'LDur)
                   :LChan (slot-value  self 'LChan)
                   :LPort LPort
                   ))
  (setf (slot-value self 'LMidic) nil (slot-value self 'LVel) nil 
        (slot-value self 'LOffset) nil  (slot-value self 'LDur) nil 
        (slot-value self 'LChan) nil)
  (loop for i in (inside self) do (setf (approx i) (approx self)))
  self)


(defmethod do-initialize ((self chord) &key LMidic LVel LOffset LDur LChan LPort)

  (setQValue self 1000 :recursive nil)

  (when (find-if 'atom  (list LMidic LVel LOffset LDur LChan LPort))
    (om-beep-msg "Error CHORD attributes must be LISTS !!")
    (unless (listp LMidic) (setf LMidic (list LMidic)))
    (unless (listp LVel) (setf LVel (list LVel)))
    (unless (listp LOffset) (setf LOffset (list LOffset)))
    (unless (listp LDur) (setf LDur (list LDur)))
    (unless (listp LChan) (setf LChan (list LChan)))
    (unless (listp LPort) (setf LPort (list LPort))))

  (setf (inside self)
        (loop while LMidic 
              for midic = (or (pop LMidic) midic)
              while midic
              for vel = (or (pop LVel) vel 80)
              for offset = (or (pop LOffset) offset 0)
              for dur = (or (pop LDur) dur 1000)
              for chan = (or (pop LChan) chan 1)
              for port = (or (pop LPort) port 0)   ;;; now port can be nil.. 
              for note = (mki 'note :midic (round midic) :vel (round vel) :dur (round dur) :chan chan )
              do (setf (offset note) (round offset))
              (setf (port note)  port)
              collect note ))
  (QNormalize self)
  self)


(defmethod* Objfromobjs ((Self note) (Type Chord)) 
  (ObjFromObjs (list self) Type))


(defmethod* Objfromobjs ((Self list) (Type Chord))
  (cond
   ((list-subtypep self 'chord)
    (let ((notes (flat (mapcar 'inside self))))
      (objfromobjs notes type)))
   ((chord-p (car self)) (Clone (car self)))
   ((list-subtypep self 'number)
    (mki (type-of type) :LMidic self))
   ((list-subtypep self 'note)
    (let ((chord (make-instance (type-of type) :empty t)))
      (setQValue chord 1000 :recursive nil)
      (setf (inside chord) (mapcar 'clone self))
      (QNormalize chord)
      (setf (slot-value chord 'LMidic) nil  (slot-value chord 'LVel) nil 
            (slot-value chord 'LOffset) nil  (slot-value chord 'LDur) nil 
            (slot-value chord 'LChan) nil) 
      chord))
   (t nil)))


(defmethod* Objfromobjs ((Self number) (Type Chord))
  (ObjFromObjs (mki 'note :midic (round self)) Type))

;; these methods are defined in container.lisp. Here we cheat because chord is actually a container
;; but for most treatments must be seen as a simple container.
(defmethod first-simple-container ((self chord )) self )
(defmethod last-simple-container ((self chord )) self  )



;;; SEQUENCES

;from modif ;aaa  3-28-99
(defmethod initialize-instance ((self metric-sequence) &rest initargs
                                &key (PropagateExtent 4) (InternalCall nil) (Empty nil)) 
  (declare (ignore initargs)) 
  (call-next-method)
  (cond
   ((or Empty (null (slot-value self 'tree))
        (null (remove nil (slot-value self 'tree)))) ;; :tree (nil nil)  
    (setf (slot-value self 'tree) nil))
   (t (do-initialize-metric-sequence self :tree (slot-value self 'tree) :PropagateExtent PropagateExtent :InternalCall InternalCall)))
  self
  )

(defmethod do-initialize-metric-sequence ((self metric-sequence) &key tree  (Empty nil) (PropagateExtent 4) (InternalCall nil) )
  (cond
   ((and tree (not empty))
    (setf (slot-value self 'QValue ) 1)
    (init-seq-from-tree  
     self 
     (if InternalCall  
       tree
       (setf (slot-value self 'tree)   (normalize-tree (resolve-? tree))))
     :PropagateExtent PropagateExtent)
    (unless InternalCall
      (set-relative-offset self)
      (integerize self)
      (QNormalize self) ))
   (t (setf (slot-value self 'tree) nil))))


(defmethod initialize-instance ((self voice) &key (PropagateExtent 4) (InternalCall nil) (Empty nil))
  (declare (ignore initargs))
  (call-next-method) 
  (unless Empty
    (do-initialize self :tree (slot-value self 'tree) 
                   :chords (slot-value self 'chords)
                   :tempo (tempo self)
                   :legato (slot-value self 'legato)
                   :ties (slot-value self 'ties) ))
  (setf (slot-value self 'chords) nil  (slot-value self 'ties) nil)
  self)

(defmethod do-initialize ((self voice) &key tree chords tempo legato ties (PropagateExtent 4) (InternalCall nil))
  (distribute-chords self chords)
  (when (> legato 0) (normalize-chord self legato))
  (set-ties self ties)
  (setf (tempo self) tempo)
  self)

;170624 KH fixes restfloat after float!
;such as  (? (((4 4) (2 (1 (1 -5)) -1)))) -> ((4 4) (2 (1 (1 -4 -1.0)) -1)) 
; should be -> ((4 4) (2 (1 (1 -4 -1)) -1))

(defmethod do-initialize-metric-sequence ((self voice) &key tree  (Empty nil) (PropagateExtent 4) (InternalCall nil) )
  (cond
   ((and tree (not empty))
    (setf (slot-value self 'QValue ) 1)
    (init-seq-from-tree  
     self 
     (if InternalCall  
       tree
       (progn
         (if (ratios-tree-p tree)
           (setf tree (mktree tree (second *quantify-def-params*)))
           (if (not (or (numberp (car tree))  (and (symbolp (car tree)) (string-equal (string (car tree)) "?"))))
             (setf tree (cons '? (list tree)))))
         (setf tree (resolve-? tree))
         (setf tree (singleton tree))
         (setf tree (list-first-layer tree))
         (setf (slot-value self 'tree) (normalize-tree (reducetree tree)))
         (setf tree (add-ties-to-tree tree))
         ;(setf tree (normalize-tree-voice  tree))
         (setf (slot-value self 'tree) tree)
         ;(setf (slot-value self 'tree) (normalize-tree (reducetree tree)))
         )) ;!!!!
     :PropagateExtent PropagateExtent)
    (unless InternalCall
      (set-relative-offset self)
      (integerize self)
      (QNormalize self)))
   (t (setf (slot-value self 'tree) nil))))



(defmethod set-ties ((self voice) ties) 
  (loop for tie-list in ties
        for chord in (real-chords self)
        do (loop for tied in tie-list
                 for to-tie  = (find tied (inside chord) :key 'midic)
                 when to-tie
                 do 
                 (when (prep-chord-p (parent to-tie))
                   (setf to-tie
                         (loop for next = (next-container to-tie '(chord)) then (next-container next '(chord))
                               until (con-chord-p next)
                               finally (return (find tied (inside next) :key 'midic)))))                 
                 
                 (cond  
                  ((eq (tie to-tie) nil)
                   (setf (tie to-tie) 'begin)
                   (let ((next  (next-tied-note to-tie)))
                     (if next 
                         (if (prep-chord-p (parent next))
                             (setf (tie next) 'continue)
                           (setf (tie next) 'end))
                       (setf (tie to-tie) nil))))
                  ((eq (tie to-tie) 'end) 
                   (setf (tie to-tie) 'continue)
                   (let ((next (next-tied-note to-tie)))
                     (if next
                         (if (prep-chord-p (parent next))
                             (setf (tie next) 'continue)
                           (setf (tie next) 'end))
                       (setf (tie to-tie) 'end))) ) ))))


(defmethod ties ((self voice))
  (loop for chord in (real-chords self)
        collect
        (if (prep-chord-p chord) 
          (loop for note in (inside (last-cont-chord chord))
                when (eq (tie note) 'continue) collect (midic note))
          (loop for note in (inside chord)
                when (member (tie note) '(begin continue)) collect (midic note)))))

(defmethod (setf ties) ((ties list) (self voice))
    (do-initialize self :chords (chords self) :tempo (tempo self) :tree (tree self) :legato (legato self) :ties ties))


    
(defmethod initialize-instance ((self measure)
                                &key (PropagateExtent 4) (InternalCall nil) (Empty nil))
  (declare (ignore initargs))
  (call-next-method) 
  (unless (or  Empty internalCall)
    (do-initialize self))
  self)
    

;(defmethod do-initialize ((self measure) &key )
;  (distribute-chords self (list (mki 'chord)))
;  self)

;to be tested!
(defmethod do-initialize ((self measure) &key chords)
  (distribute-chords self chords)
  (normalize-chord self 100)
  self)

(defmethod initialize-instance ((self group)
                                &key (PropagateExtent 4) (InternalCall nil) (Empty nil))
  (declare (ignore initargs))
  (call-next-method) 
  (unless (or  Empty internalCall)
    (do-initialize self))
  (setf (slot-value self 'tree) nil)
  self)
    
(defmethod do-initialize ((self group) &key )
  (distribute-chords self (list (mki 'chord)))
  self)


;;; probleme de l'initialisation de tree. Tree devrait etre au niveau de Rhythm !!
;from modif 24-03-99 aaa
#|
(defmethod tree ((self metric-sequence))
  (if (slot-value self 'tree)
    (slot-value self 'tree)
    (setf (slot-value self 'tree) (build-tree self))))
|#

;new correction from Krim 3.2.7 14-02-2000

(defmethod check-tree-for-contchord ((tree list) (self metric-sequence))
  (list (first tree)
        (loop for subtree in (second tree)
              for subself in (inside self)
              collect (check-tree-for-contchord subtree subself))))

(defmethod check-tree-for-contchord ((tree number) (self chord))
  tree)


(defmethod check-tree-for-contchord ((tree number) (self t))
  tree)

(defmethod tree ((self metric-sequence))
  (setf (approx self) (get-approx self));in order to retain approx
  (if (slot-value self 'tree)
    (slot-value self 'tree)
    (setf (slot-value self 'tree) (check-tree-for-contchord (build-tree self) self))))

;;outputs correctly grace trees after editing
(defmethod graces-in? ((self voice))
  (let ((chords (flat (collect-chords-graces self))))
    (find-if #'grace-chord-p chords)))


(defmethod tree ((self voice)) 
  (when (graces-in? self)
    (let* ((chrds (collect-chords-rest-graces self))
           (pos (remove nil
                        (loop for i in chrds
                              for n from 0 to (length chrds)
                              collect (if (listp i) n))))
           (lgt (remove nil
                        (loop for i in chrds
                              collect (if (listp i) (1- (length i)))))))
      (setf (slot-value self 'tree)
            (add-tree-graces 
             (remove-tree-graces (slot-value self 'tree))
             ;(slot-value self 'tree)
             pos lgt))))
  ;(slot-value self 'tree)
  (call-next-method)
  )
;;;

(defmethod (setf tree) ((tree list) (self metric-sequence))
  (initialize-instance self :tree tree ))

(defmethod (setf tree) ((tree list) (self voice))
  (let* ((chords (chords self)) (ties (ties self))
         (box (associated-box self))
         (editor (if box (editorframe box))))
    (do-initialize-metric-sequence self :tree tree )
    (do-initialize self :chords chords :tempo (tempo self) :tree tree :legato (legato self) :ties ties)
    (when editor
      (update-panel (panel editor)))
    self))


(defmethod (setf tree) ((tree list) (self measure))
  (do-initialize-metric-sequence self :tree tree )
  (do-initialize self )
  self)

(defmethod (setf tree) ((tree list) (self group))
  (do-initialize-metric-sequence self :tree tree )
  (do-initialize self )
  self)


(defmethod (setf chords) ((chords list) (self voice))
  (do-initialize self :chords chords :tempo (tempo self) :tree (tree self) :legato (legato self) :ties (ties self))
  (setf (tree self) (tree self))
  )

(defmethod (setf legato) ((legato integer) (self voice))
  (do-initialize self :chords (chords self) :tempo (tempo self) :tree (tree self) :legato legato  :ties (ties self)))


(defmethod chords ((self voice))
  (loop for i in (slot-value self 'chords)
        do (setf (approx i) (approx self)))
  ;(setf (approx self) (get-approx self));in order to retain approx
  (call-next-method))


;outputs also grace notes
;A revoir!
#|
(defmethod chords ((self voice))
  (setf (approx self) (get-approx self));in order to retain approx
  (if (graces-in? self)
    (setf (slot-value self 'chords) (get-real-chords-and-graces self))
    (call-next-method)))
|#

(defmethod chords ((self sequence*)) 
  (loop for sub in (inside self)
        when (not (cont-chord-p sub))
        if (note-or-chord-p sub) collect (ot-clone sub)
          else if (container-p sub) append (chords sub)))


;;redifinition for grace-notes....
;in order to have the gracetree output after edition
;but doesn't work!

(defmethod note-chord-or-rest-p ((self T)) 
  (or (chord-p self) (rest-p self) (note-p self)))
#|
(defmethod chords ((self sequence*)) 
  (let ((chords 
         (remove nil 
                 (flat (loop for sub in (inside self)
                             when (not (cont-chord-p sub))
                               if (note-chord-or-rest-p sub) 
                                 collect (cond 
                                          ((and (rest-p sub) (gnotes sub)) (glist (gnotes sub)))
                                          ((gnotes sub) (x-append (glist (gnotes sub)) (ot-clone sub)))
                                          ((not (rest-p sub)) (ot-clone sub))
                                          (t nil))
                               else if (container-p sub) append (chords sub))))))
    (loop for i in chords
          collect (if (grace-chord-p i)
                      (objfromobjs i (make-instance 'chord))
                    i))))
|#
;;;
(defmethod (setf approx) ((approx number) (self poly))
  (call-next-method)
  (loop for i in (inside self)
        do (setf (approx i)  approx))
  self)

(defmethod (setf approx) ((approx number) (self voice))
  (call-next-method)
  (loop for i in (inside self)
        do (setf (approx i)  approx))
  self)


(defmethod (setf approx) ((approx number) (self measure))
  (call-next-method)
  (loop for i in (inside self)
        do (setf (approx i)  approx))
  self)

(defmethod (setf approx) ((approx number) (self group))
  (call-next-method)
  (loop for i in (inside self)
        do (setf (approx i)  approx))
  self)
;;;;



;---tempo
(defmethod tempo-a-la-noire ((tempo number)) tempo)

(defmethod tempo-a-la-noire ((tempo list))
  (* (second tempo) (/ (first tempo) 1/4)))

(defun tempo-list-p (tempo)
  (and (listp tempo) (numberp (first tempo)) (numberp (second tempo))))

(defun convert-tempo-to-list (tempo)
   (cond 
     ((numberp tempo)  (list 1/4 tempo))
     ((tempo-list-p tempo) tempo)))



(defmethod get-voice-tempilist ((self voice)) 
  (second (tempo self)))

(defmethod set-voice-tempilist ((self voice) list) 
  (setf (nth 1 (tempo self)) list))

;;;;;;;;;;;;;;;;;;;;;;;;;;format-om-tempi

(defmethod* format-omtempo ((note-fig list) 
                             (tempi list))
   :initvals '(1/4 t) 
   :indoc '("list of note figure" "list of tempi")
   :icon 134
   :doc   "Gets a list of note-figures, a list of tempi and return a formated tempo list by measure for om VOICE object. If nil is found skips measure and repeats the former tempo"

  (let* ((first-tempo (list (car note-fig) (car tempi)))
         (restfig (cdr note-fig))
         (resttempi (cdr tempi))
         (restliste (remove nil       
            (loop 
             for i from 1 to (length resttempi)
             for temp in resttempi
             for fig in restfig
             collect (if temp
                         (list (list i 0) (list fig temp nil)))))))
    (list first-tempo restliste)))


(defmethod* format-omtempo ((note-fig number) 
                            (tempi list))
  "Gets a list of note-figures, a list of tempi and return a formated tempo list by measure for om VOICE object. If nil is found skips measure and repeats the former tempo"

  (let* ((first-tempo (list note-fig (car tempi)))
         (resttempi (cdr tempi))
         (restliste (remove nil       
            (loop 
             for i from 1 to (length resttempi)
             for temp in resttempi
             collect (if temp
                         (list (list i 0) (list note-fig temp nil)))))))
    (list first-tempo restliste)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf tempo) ((tempo t) (self voice))
  (let (thetempi tempolist)
    (cond 
     ((numberp tempo)
      (setf tempolist (list (convert-tempo-to-list tempo) nil)))
     ((tempo-list-p tempo) (setf tempolist (list tempo nil)))
     (t (setf tempolist (list (convert-tempo-to-list (car tempo)) (second tempo)))))
    (setf (slot-value self 'Qtempo) (tempo-a-la-noire (car tempolist)))
    (propagate-tempo self)
    (setf (slot-value self 'tempo) tempolist)
    (setf thetempi (get-voice-tempilist self))
    (when thetempi
      (make-voice-tempo-change self thetempi))
  tempolist))

#|
(defmethod init-seq-from-tree ((self metric-sequence) (tree list) &key (PropagateExtent 1)) 
  (let ((subtrees (second tree))
        (Extent (* (fullratio (first tree)) PropagateExtent))
        (nbsubunits (reduce  
                     #'(lambda (x y) (+  (abs x) (subtree-extent y))) 
                     (second tree)
                     :initial-value 0))
        (curr-obj nil)
        (current-graces nil) (current-note nil))
    (remove NIL
            (loop 
             for subtree in subtrees
             do (setf curr-obj 
                      (cond
                       ((numberp subtree)
                        (let ((object 
                               (cond 
                                ((zerop subtree) ;; GRACE NOTE
                                 (if current-note (setf (mus-const current-note) (append (list! (mus-const current-note)) '(1)))
                                   (setf current-graces (cons -1 current-graces)))
                                 NIL)
                                ((> subtree 0)
                                 (let ((note (make-instance 'note :empty t :extent (* (fullratio subtree) (/ Extent nbsubunits)))))
                                   (when current-graces ;; add grace notes before
                                     (setf (mus-const note) current-graces))
                                   (setf current-note note)
                                   note))
                                ((< subtree 0)
                                 (make-instance 'rest :extent (*  (abs (fullratio subtree)) (/ Extent nbsubunits)))))))
                          (when (and (plusp subtree) (floatp subtree))
                            (setf (tie object) 'continue))
                          object))
                       ((listp subtree)
                        (make-instance (next-metric-class self)
                                       :tree subtree 
                                       :PropagateExtent (/ Extent nbsubunits)
                                       :InternalCall t))))
             when curr-obj
             collect curr-obj
             into inside
             finally 
             (setf (slot-value self 'inside) inside
                   (slot-value self 'extent) Extent)
             ))))
|#

(defmethod init-seq-from-tree ((self metric-sequence) (tree list) &key (PropagateExtent 1)) 
  (let ((subtrees (second tree)) 
        (Extent (* (fullratio (first tree)) PropagateExtent))
        (nbsubunits (reduce  
                     #'(lambda (x y) (+  (abs x) (subtree-extent y))) 
                     (second tree)
                     :initial-value 0))
        (curr-obj nil)
        (current-graces nil) 
        (current-note nil))
    (remove NIL
            (loop 
               for subtree in subtrees
               do (setf curr-obj 
                        (cond
                         ;;;for group graces!
                         ((and (listp subtree) (equal 0 (car subtree)))
                          (loop for i in (second subtree)
                                do (setf current-graces (cons 1 current-graces))) ;;regler l'affaire 0.0 = aftergrace
                          )
                         ((numberp subtree)
                          (let ((object 
                                 (cond 
                                  ((equal subtree 0) ;; BEFOR GRACE
                                   (setf current-graces (cons 1 current-graces))
                                   NIL)
                                  
                                  ((equal subtree 0.0) ;; AFTER GRACE 
                                   (if current-note 
                                       (setf (mus-const current-note) (append (list! (mus-const current-note)) '(1)))
                                     )
                                   NIL)
                                   
                                  ((> subtree 0)
                                   (let ((note (make-instance 'note :empty t :extent (* (fullratio subtree) (/ Extent nbsubunits)))))
                                     (when current-graces ;; add grace notes before ;;; avoir !
                                       (setf (mus-const note) current-graces)
                                       (setf current-graces nil)
                                       )
                                     (setf current-note note)
                                     
                                     note))
                                  ((< subtree 0)
                                   (let ((rest (make-instance 'rest :empty t :extent (* (abs (fullratio subtree)) (/ Extent nbsubunits)))))
                                     (when current-graces ;; add grace notes before ;;; avoir !
                                       (setf (mus-const rest) current-graces)
                                       (setf current-graces nil))
                                     (setf current-note rest)
                                     rest))
                                  )))
                            (when (and (plusp subtree) (floatp subtree))
                              (setf (tie object) 'continue))
                            object))
                         ((listp subtree)
                          (make-instance (next-metric-class self)
                                         :tree subtree 
                                         :PropagateExtent (/ Extent nbsubunits)
                                         :InternalCall t))
                         ))
               when curr-obj
                 collect curr-obj
                   into inside
                 
               finally 
                 (setf (slot-value self 'inside) inside
                       (slot-value self 'extent) Extent)
               ))))


(defmethod set-relative-offset ((self sequence*))
  "Computes the offsets of subcontainers from their extents. Offsets are relative
to the direct supercontainer."
  (loop for sub in (inside self)
        with offset = 0 
        do 
        (setf (offset sub) offset
              offset (+ offset (* (/ (QValue self) (QValue sub)) (extent sub))))
        (set-relative-offset sub))
  self)
(defmethod set-relative-offset ((self simple-container)) self)

(defmethod adjust-extent ((self sequence*))
  "Adjusts the extent of a container to be exactly the sum of the extents 
of all its direct subcontainers (supposed adjacent)"
  (setf (extent self)
        (loop for sub in (inside self)
              maximize  (+ (offset sub) (InContext self (extent sub))))))

;; new jean 27/03/09
;(defmethod adjust-extent ((self t)) nil)
;(defmethod adjust-extent ((self chord))
;  "Adjusts the extent of a chord to be the max of the extents of all its direct subcontainers"
;  (setf (extent self)
;        (loop for sub in (inside self) maximize (InContext self (extent sub)))))




(defmethod insert ((self sequence*) (element sequence*) &key (where :atEnd))
  (cond ((null (inside self))
         (setf (inside self) (list (clone element))
               (QValue self) (QValue element)
               (extent self) (extent element)
               (offset element) 0))
        (t
         (let* ((newQValue (cLcm self element (first (last (inside self)))))
                (last (first (last (inside self)))))
           (SetQvalue self newQValue)
           (setQValue element newQvalue)
           (setf (offset element) (+ (offset last) (InContext self (extent last)))
                 (inside self) (append (inside self) (list (clone element)))
                 (extent self) (+ (extent self) (extent element))                 
                 ))))
  self)

(defmethod cast-to-chords ((self t)) (declare (ignore self)) t)

(defmethod cast-to-chords ((self sequence*))
  (loop for obj on (inside self)
        do (cast-to-chords (first obj))
           (when (subtypep (type-of (first obj)) 'note)
             (setf (first obj) 
                   (let ((chord (ObjFromObjs (first obj) (mki 'chord))))
                     (setf (parent chord) (parent (first obj))
                           (offset chord) (offset (first obj))
                           (extent chord) (extent (first obj))
                           (QValue chord) (QVAlue (first obj))
                           (offset (first (inside chord)))  0)
                     chord)))))


;;; MEASURE


(defmethod* Objfromobjs ((Self list) (Type measure))
  (cond ((list-subtypep self 'group)
         (let ((measure (make-instance (type-of type) :empty t)))
          (loop for group in self
                do (insert measure group :where :atEnd)
                finally (QNormalize measure))
          measure))
        (t nil)))
       
(defmethod* Objfromobjs ((Self group) (Type measure))
  (ObjFromObjs (list self) type))


;;; VOICE


(defmethod* Objfromobjs ((Self list) (Type voice))
            (cond ((and self (list-subtypep self 'measure))
                   (let ((voice (make-instance (type-of type) :empty t)))
                     (setf (tempo voice) 60)
                     (loop for measure in self
                           do (insert voice measure :where :atEnd)
                           finally (QNormalize voice))
                     voice))
                  (t nil)))
        
(defmethod* Objfromobjs ((Self measure) (Type voice))
  (ObjFromObjs (list self) type))



;;; POLY


(defmethod voices ((self poly))
  (setf (approx self) (get-approx self));approx persistance
  (inside self))
           

(defmethod initialize-instance ((self poly) &rest initargs &key (Empty nil))
  (declare (ignore initargs)) 
  (call-next-method)
  (unless Empty (do-initialize self :voices (slot-value self 'voices)))
  (setf (slot-value self 'voices) nil)
  self)


(defmethod do-initialize ((self poly) &key voices)
  (cond ((and (consp voices) (list-subtypep voices 'voice))
         (loop for voice in voices 
               do  (om-assemble-into-poly self voice)))
        ((subtypep (type-of voices) 'voice)
         (om-assemble-into-poly self  voices))
        (t (om-beep-msg (format nil "~A must be initialized with a voice list!" (type-of self))) 
              (om-assemble-into-poly self (make-instance 'voice))))
  self)


(defmethod (setf voices) ((voices list) (self poly))
  (do-initialize self :voices voices))


;;a voir si cela ne pose pas de probleme.
(defmethod tempo ((self poly))
  (let ((inside (inside self)))
    (mapcar #'tempo inside)))


;=====================================
;    NOTES
;=====================================

;(defmethod copy-container ((self note) &optional (pere ()))
;   "Copies recursively a container"
;   (let ((note (call-next-method)))
;(setf (tie note) nil)
;    note))


(defmethod next-tied-note ((self note))
  (and (parent self)
       (tie self)
       (not (eq (tie self) 'end))
       (let ((next-obj (next-container 
                        (if (subtypep (type-of (parent self)) 'chord) 
                          (parent self) 
                          self) 
                        '(chord note))))
         (and next-obj
              (if (subtypep (type-of next-obj) 'chord)
                (find (midic self) (inside next-obj) :key 'midic)
                next-obj)))))

(defmethod previous-tied-note ((self note))
  (and (parent self)
       (tie self)
       (not (eq (tie self) 'begin))
       (let ((previous (previous-container 
                        (if (subtypep (type-of (parent self)) 'chord) 
                          (parent self) 
                          self) 
                        '(chord note))))
         (and previous
              (if (subtypep (type-of previous) 'chord)
                (find (midic self) (inside previous) :key 'midic)
                previous)))))

(defmethod break-tie ((self note))
  (cond ((prep-chord-p (parent self))
         (let* ((last-cont (last-cont-chord (parent self)))
                (last-note (find (midic self) (inside last-cont) :key 'midic))
                (right-note (next-tied-note last-note)))
           (setf (tie last-note) 'end)
           (when right-note
             (setf (tie right-note) 
                   (case (tie right-note) (end nil) (continue 'begin))))
           (when (eq  (tie self) 'continue )
             (let ((previous (previous-tied-note self)))
               (when previous
                 (case (tie previous)
                   (continue (setf (tie previous) 'end))
                   (t (setf (tie previous) nil))))
               (setf (tie self) 'begin)))))
        (t
         (case (tie self)
           (begin (let ((next (next-tied-note self)))
                    (when next
                      (case (tie next)
                        (continue (setf (tie next) 'begin))
                        (t (setf (tie next) nil))))))
           (continue (let ((previous (previous-tied-note self))
                           (next (next-tied-note self)))
                       (when previous
                         (case (tie previous)
                           (continue (setf (tie previous) 'end))
                           (t (setf (tie previous) nil))))
                       (when next
                         (case (tie next)
                           (continue (setf (tie next) 'begin))
                           (t (setf (tie next) nil))))))
           (end (let ((previous (previous-tied-note self)))
                  (when previous
                    (case (tie previous)
                      (continue (setf (tie previous) 'end))
                      (t (setf (tie previous) nil)))))))
         (setf (tie self) nil))))

                   
(defmethod distribute-chords  ((self score-element) (chords score-element))
  (distribute-chords self (collect-chords chords)))

#|
(defmethod distribute-chords ((self score-element) (chords list))
  (let ((fringe nil) 
        (chord-model (mki 'chord))
        (def-chord (or (last-elem chords) (mki 'chord))))
    (labels ((distribute (self chords)
               (setf (inside self)
                       (loop for sub in (inside self)
                             with chord
                             ;with chord-model  = (mki 'chord)
                             ;if (null chords) collect sub
                             if (not (infra-group-p sub)) 
                             do (setf chords (distribute sub chords)) and collect sub
                             else if (rest-p sub) collect sub
                             else do
                             (if (and (note-p sub) (eq (tie sub) 'continue))
                               (progn (setf chord 
                                            (objfromobjs 
                                             (or (loop for c in fringe if (chord-p c) return c)
                                                 (mki 'chord))
                                             chord-model))
                                      (change-class chord 'continuation-chord))
                               (if (mus-const sub)  ;;; THERE ARE GRACE NOTES !!
                                   (let* ((grace-time 100)  ;;; 50ms ?
                                          (nnotes-before (count -1 (mus-const sub) :test '=))
                                          (nnotes-after (count 1 (mus-const sub) :test '=))
                                          (chords-before (loop for i from 1 to nnotes-before collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model)))
                                          (main-chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model))
                                          (chords-after (loop for i from 1 to nnotes-after collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model))))
                                     (loop for c in (reverse chords-before)
                                           for dt = grace-time then (+ dt grace-time)
                                           do (setf (loffset c) (om- (loffset c) dt)))
                                     (loop for c in chords-after
                                           for dt = grace-time then (+ dt grace-time)
                                           do (setf (loffset c) (om+ (loffset c) dt)))
                                     ;(print (mapcar 'loffset (append chords-before (list main-chord) chords-after)))
                                     (setf chord (objfromobjs (append chords-before (list main-chord) chords-after) chord-model))
                                     ;;; OBJFROMOBJS REDUCES LOFFSET TO 1 ...
                                     (setf (loffset chord) 
                                           (append (flat (mapcar 'loffset chords-before)) 
                                                   (loffset main-chord)
                                                   (flat (mapcar 'loffset chords-after))))
                                     ;(print (loffset chord))
                                     )
                                 
                                 (setf chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model))
                                 ))
                             (setf (offset chord) (offset sub))
                             (InContext sub (setf (extent chord) (extent sub)))
                             (when (and (note-p sub) (eq (tie sub) 'continue))  (push 'tie fringe))
                             (push chord fringe)
                             and collect chord))
                 chords))
      
      (distribute self chords)
      
      (setf fringe (nreverse fringe)) 
      (loop for item1 in fringe
            for item2 = (rest fringe) then (rest item2)
            with state = 0
            do
            (cond ((eq (first item2) 'tie)
                   (cond 
                    ((= state 0) (tie-chord item1 'begin) (incf state))
                    (t (tie-chord item1 'continue) (setf (state item1) 'continue))))
                  ((not (eq item1 'tie))
                   (cond ((> state 0) (tie-chord item1 'end) (setf state 0 (state item1) 'end))))))
      ;(print (mapcar 'loffset (chords self)))
      self)))
|#

#|
(defmethod distribute-chords ((self score-element) (chords list))
  (let ((fringe nil) 
        (chord-model (mki 'chord))
        (rest-model (mki 'rest))
        (def-chord (or (last-elem chords) (mki 'chord))))
    (labels ((distribute (self chords)
               (setf (inside self)
                     (loop for sub in (inside self)
                           with chord
                             ;with chord-model  = (mki 'chord)
                             ;if (null chords) collect sub
                           if (not (infra-group-p sub)) 
                             do 
                               (setf chords (distribute sub chords))
                             and collect sub
                           else do
                               (cond
                                ((and (note-p sub) (eq (tie sub) 'continue))
                                 (progn (setf chord 
                                              (objfromobjs 
                                               (or (loop for c in fringe if (chord-p c) return c)
                                                   (mki 'chord))
                                               chord-model))
                                   (change-class chord 'continuation-chord)))
                                ;;;;;;;;;;;;;;;;;;;;;;;;
                                ((mus-const sub) ;;; THERE ARE GRACE NOTES !!
                                 (let* ((nnotes-before (count 1 (mus-const sub) :test '=)); ici c'etait -1 et c'est bien cela qui fout la merde....
                                        (nnotes-after (count -1 (mus-const sub) :test '=))
                                        (chords-before (loop for i from 1 to nnotes-before collect (objfromobjs (or (pop chords) (clone def-chord)) chord-model)))
                                        (chords-after (loop for i from 1 to nnotes-after collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model)))
                                        (gchords (loop for i in chords-before collect (objfromobjs i (make-instance 'grace-chord)))))
                                   
                                   (if (rest-p sub)
                                       (let ((main-chord (objfromobjs (clone def-chord) chord-model)))
                                         (setf chord sub)
                                         (loop for i in gchords do (setf (thechord i) sub))
                                         (setf (gnotes sub) 
                                               (make-instance 'grace-notes
                                                              :glist gchords
                                                              :thechord sub
                                                              :before? t)))
                                   
                                     (let ((main-chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model)))
                                       (setf chord (objfromobjs (append (list main-chord)) chord-model))
                                       (loop for i in gchords do (setf (thechord i) chord))
                                       (setf (gnotes chord) 
                                             (make-instance 'grace-notes
                                                            :glist gchords
                                                            :thechord chord
                                                            :before? t))
                                         
                                       ))))
                                 ;;;;;;;;;;;;;;;;;;;;;;;;
                                 ((rest-p sub) 
                                  (setf chord sub))
                                 ;;;;;;;;;;;;;;;;;;;;;;;
                                 (t 
                                  (setf chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model))))
                               
                               (setf (offset chord) (offset sub))
                               (InContext sub (setf (extent chord) (extent sub)))
                               (when (and (note-p sub) (eq (tie sub) 'continue))  (push 'tie fringe))
                               (push chord fringe)
                             and collect chord))
               chords))
            
      (distribute self chords)
      
      (setf fringe (nreverse fringe)) 
      (loop for item1 in fringe
            for item2 = (rest fringe) then (rest item2)
            with state = 0
            do
              (cond ((eq (first item2) 'tie)
                     (cond 
                      ((= state 0) (tie-chord item1 'begin) (incf state))
                      (t (tie-chord item1 'continue) (setf (state item1) 'continue))))
                    ((not (eq item1 'tie))
                     (cond ((> state 0) (tie-chord item1 'end) (setf state 0 (state item1) 'end))))))
      
      self)))
|#

(defmethod distribute-chords ((self score-element) (chords list))
  (let ((fringe nil) 
        (chord-model (mki 'chord))
        (rest-model (mki 'rest))
        (def-chord (or (last-elem chords) (mki 'chord))))
    (labels ((distribute (self chords)
               (setf (inside self)
                     (loop for sub in (inside self)
                           with chord
                             ;with chord-model  = (mki 'chord)
                             ;if (null chords) collect sub
                           if (not (infra-group-p sub)) 
                             do 
                               (setf chords (distribute sub chords))
                             and collect sub
                           else do
                               (cond
                                ((and (note-p sub) (eq (tie sub) 'continue))
                                 (progn (setf chord 
                                              (objfromobjs 
                                               (or (loop for c in fringe if (chord-p c) return c)
                                                   (mki 'chord))
                                               chord-model))
                                   (change-class chord 'continuation-chord)))
                                ;;;;;;;;;;;;;;;;;;;;;;;;
                                ((mus-const sub) ;;; THERE ARE GRACE NOTES !! 
                                 (let* ((nnotes-before (count 1 (mus-const sub) :test '=)); ici c'etait -1 et c'est bien cela qui fout la merde....
                                        (nnotes-after (count -1 (mus-const sub) :test '=))
                                        ;(chords-before (loop for i from 1 to nnotes-before collect (objfromobjs (or (pop chords) (clone def-chord)) chord-model)))
                                        (chords-after (loop for i from 1 to nnotes-after collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model)))
                                        ;(gchords (loop for i in chords-before collect (objfromobjs i (make-instance 'grace-chord))))
                                        (gchords (if (or (numberp (car chords)) (null chords))
                                                     (loop for i in 
                                                           (loop for i from 1 to nnotes-before 
                                                                 collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model))
                                                         collect (objfromobjs i (make-instance 'grace-chord)))
                                                   (if (gnotes (car chords))
                                                   (glist (gnotes (car chords)))
                                                     ;compat
                                                     (loop for i in 
                                                           (loop for i from 1 to nnotes-before 
                                                                 collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model))
                                                         collect (objfromobjs i (make-instance 'grace-chord)))
                                                     ))))
                                   (if (rest-p sub)
                                       (let ((main-chord (objfromobjs (clone def-chord) chord-model)))
                                         (setf chord sub)
                                         (loop for i in gchords do (setf (thechord i) sub))
                                         (setf (gnotes sub) 
                                               (make-instance 'grace-notes
                                                              :glist gchords
                                                              :thechord sub
                                                              :before? t)))
                                   
                                     (let ((main-chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model)))
                                       (setf chord (objfromobjs (append (list main-chord)) chord-model))
                                       (loop for i in gchords do (setf (thechord i) chord))
                                       (setf (gnotes chord) 
                                             (make-instance 'grace-notes
                                                            :glist gchords
                                                            :thechord chord
                                                            :before? t))
                                         
                                       ))))
                                 ;;;;;;;;;;;;;;;;;;;;;;;;
                                 ((rest-p sub) 
                                  (setf chord sub))
                                 ;;;;;;;;;;;;;;;;;;;;;;;
                                 (t 
                                  (setf chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model))))
                               
                               (setf (offset chord) (offset sub))
                               (InContext sub (setf (extent chord) (extent sub)))
                               (when (and (note-p sub) (eq (tie sub) 'continue))  (push 'tie fringe))
                               (push chord fringe)
                             and collect chord))
               chords))
            
      (distribute self chords)
      
      (setf fringe (nreverse fringe)) 
      (loop for item1 in fringe
            for item2 = (rest fringe) then (rest item2)
            with state = 0
            do
              (cond ((eq (first item2) 'tie)
                     (cond 
                      ((= state 0) (tie-chord item1 'begin) (incf state))
                      (t (tie-chord item1 'continue) (setf (state item1) 'continue))))
                    ((not (eq item1 'tie))
                     (cond ((> state 0) (tie-chord item1 'end) (setf state 0 (state item1) 'end))))))
      
      self)))


(defmethod tie-chord ((self chord) mode)
  (loop for note in (inside self)
        do (setf (tie note) mode)))



(defmethod untie-chords ((self chord))
  (setf (inside self)
        ;(remove-if #'(lambda (x)  (eq x 'untie) ) (inside self) :key 'tie))
        (remove-if #'(lambda (x)  (and x (not (equal x 'begin)))) (inside self) :key 'tie))
  (loop for note in (inside self)
        when (eq (tie note) 'begin)
        do (loop for nnote = (next-tied-note note) then (next-tied-note nnote)
                 while nnote
                 for nnnote =  nnote
                 ;do (setf (tie nnote) 'untie) 
                 finally
                 (setf (extent note)
                       (+ (- (+ (offset nnnote) (offset (parent nnnote)))
                             (+ (offset self) (offset note)))
                          (extent nnnote))))
        and do (setf (tie note) nil)))

(defclass* continuation-chord (chord)
  ((state :accessor state :initform 'begin)))

(defmethod cont-chord-p ((self continuation-chord)) t)
(defmethod cont-chord-p ((self t)) nil)

;TO BE TESTED

(defmethod tie-chord ((self continuation-chord) mode)   
"When VOICE receives chords, keep duration if <legato> is 0. Continuation-chords will 
be taken into consideration"
(setf (gnotes self) nil);if gnotes before
  (loop for note in (inside self)
        do (progn 
             (setf (tie note) mode)
             (setf (dur note) 0))))

;to prevent (rest begin) issue
(defmethod tie-chord ((self t) mode))


(defmethod check-tree-for-contchord ((tree number) (self continuation-chord))
  (float tree))

(defmethod prep-chord-p ((self chord)) (cont-chord-p (next-container self '(chord))))
(defmethod prep-chord-p ((self t)) nil)

(defmethod con-chord-p ((self chord)) (and (cont-chord-p self) (not (cont-chord-p (next-container self '(chord))))))
(defmethod con-chord-p ((self t)) nil)

(defmethod last-cont-chord ((self chord))
  (loop for next = (next-container self '(chord)) then (next-container next '(chord))
        until (con-chord-p next)
        finally (return next)))

(defmethod real-chords ((self sequence*)) 
  (loop for sub in (inside self)
        when (not (cont-chord-p sub))
        if (note-or-chord-p sub) collect  sub
        else if (container-p sub) append (real-chords sub)))

(defmethod normalize-chord ((self chord) &optional (percent 100))
  (setQValue self 1000 :recursive t)
  (let ((proportion (round (* (extent self) (/ percent 100)))))
    (loop for note in (inside self) 
          do (incontext self (setf (extent note) proportion))
          ;(setf (offset note) 0); commented out for offsets of grace chords!
          ))
  (QNormalize self)
  self)

(defmethod normalize-chord ((self container) &optional (percent 100))
  (loop for object in (inside self)
        do (normalize-chord object percent))
  self)

(defmethod normalize-chord ((self simple-container) &optional (percent 100)) self)

(defmethod measure-list ((self list) &key (class 'voice))
  (setf self (resolve-? self))
  (case class
    (voice (setf self (second self)))
    (measure   (setf self (list self)))
    (group (setf self (list self))))
  (mapcar #'(lambda (x) (list (fnumerator (first x)) (fdenominator (first x)))) self))

(defmethod measure-list ((self metric-sequence) &key (class 'voice))
  (measure-list   (tree self)  :class  (type-of self)))
  


(defmethod delete-useless-containers ((self container))
   "modifie une structure en supprimant les groupes inutiles "
   (if (and (group-p self)                                 ;;; self est un groupe
            (eq (length (inside self)) 1)                   ;;; qui ne contient qu'un seul element
            (zerop (offset (car (inside self))))                ;;; qui commence exactement avec self
            (zerop (- (* (extent self) (qvalue (car (inside self))))    ;;; et se termine exactement
                      (* (qvalue self) (extent (car (inside self))))))   ;;; avec self
            )
     ;;; alors self ne sert vraiment a rien : il faut l'enlever
     (let ((pere (parent self))
           (fils (car (inside self))))
       (setf (offset fils) (offset self))
       (setf (parent fils) pere)
       (setf (inside pere) (sort (cons fils (set-difference (inside pere) (list self))) #'< :key #'offset))
       )
     ()
     )
   (loop for item in (inside self)
         when (container-p item)
         do (delete-useless-containers item)
         )
   
   )

(defmethod delete-useless-containers ((self simple-container)) self)


;;; cette methode consiste a rajouter des silences, la ou dans une structure musicale, il y a un trou...
;;; sinon, l'affichage ne se fait pas correctement (decalage des mesures)

(defmethod blank-fill ((self simple-container))
  (change-qvalue self (fraction-minimale self))
  (blank-fill-in self)
  (reduit-qvalue self)
  )


;;; supprimer la version precedente de blank-fill du fichier scoretainer.lisp



(defmethod blank-fill-in ((self simple-container )) () )
(defmethod blank-fill-in ((self chord )) () )
(defmethod blank-fill-in ((self voice )) (loop for item in (inside self) do (blank-fill-in item))  )
(defmethod blank-fill-in ((self poly ))  (loop for item in (inside self) do (blank-fill-in item)) )

(defmethod blank-fill-in ((self container ))
   "remplie les blancs dans une structure : on raisonne sur la structure reduite a la fraction minimale"
   (let ((rest-to-add  
          (if (inside self)
            (append 
             ;;; si il y a des trous entre les sous containers
             (loop for item in (inside self)
                   for item-suivant in (cdr (inside self))
                   when (> (offset item-suivant) (+ (offset item) (extent item)))
                   collect (mki 'rest 
                                :offset (+ (offset item) (extent item))
                                :qvalue (qvalue self)
                                :extent (- (offset item-suivant) (+ (offset item) (extent item)))
                                :parent self   ; a rajouter 
                                ))
             ;;; si le premier container ne commence pas au debut de self
             (if (> (offset (car (inside self))) 0) 
               (list (mki 'rest
                          :offset 0
                          :qvalue (qvalue self)
                          :extent (offset (car (inside self)))
                          :parent self   ; a rajouter 
                          ))
               ()
               )
             ;;; si le dernier container ne termine pas a la fin de self
             (let ((last-item (car (last (inside self)))))
               (if (< (+ (offset last-item) (extent last-item))  (extent self) ) 
                 (list (mki 'rest
                            :offset (+ (offset last-item) (extent last-item))
                            :qvalue (qvalue self)
                            :extent (- (+ (offset self) (extent self)) (+ (offset last-item) (extent last-item)))
                            :parent self   ; a rajouter 
                            ))
                 () )
               )
             )
            (list (mki 'rest 
                       :offset 0
                       :extent (extent self)
                       :qvalue (qvalue self)
                       :parent self   ; a rajouter 
                       ) )
            )
          ))
     (if rest-to-add
       (setf (inside self) (sort (append (inside self) rest-to-add) #'< :key #'offset ))
       ()
       )
     (loop for item in (inside self) do (blank-fill-in item))
     )
   )








;(measure-list ' (3/4 (1 1 1 ) ) :class 'measure)
;(measure-list (mki 'voice))

;(inspect (make-instance 'measure))


;;; ===============================================================================================
;;; ENROBAGE - EXTRACTION
;;; TOUTES CES FONCTIONS DUPLIQUENT LA STRUCTURE AVANT D'OPERER
;;; ===============================================================================================

;;; ===========
;;; 1- ENROBAGE 

(defmethod voice->poly ((self voice)) 
   (let ((temp
          (mki 'poly
               :empty t
               :offset (offset self)
               :extent (extent self)
               :qvalue (qvalue self)
               )))
     (setf (inside temp) (list (duplique-structure-musicale self temp)))
     temp
     )
   )

(defmethod measure->voice ((self measure)) 
   (let ((temp
          (mki 'voice
               :empty t
               :offset (offset self)
               :extent (extent self)
               :qvalue (qvalue self)
               )))
     (setf (inside temp) (list (duplique-structure-musicale self temp)))
     temp
     )
   )

;;; =============
;;; 2- EXTRACTION 


(defmethod poly->voice ((self poly) num )
   (if (<= num (length (inside self))) (duplique-structure-musicale (nth num (inside self) ) ) () )
   )


#|
(defmethod voice->measure ((self voice) num)
   (if (<= num (length (inside self))) 
     (let ((resultat  (duplique-structure-musicale (nth num (inside self) )  ) ))
       (let ((premier (first-simple-container resultat))
             (dernier (last-simple-container resultat)))
         (if (and (note-p premier) (tie premier))
           (setf (tie premier) 'begin)
           ()
           )
         (if (and (note-p dernier) (tie dernier))
           (setf (tie dernier) 'end )
           ()
           )
         )
       resultat
       )
     ()
     )
   )
|#

(defmethod voice->measure ((self voice) num)
  (let* ((measure (copy-container (or (nth num (inside self))
                                      (first (last (inside self)))))))
    (loop for note in (inside (first-container measure '(chord)))
          do (case (tie note)
               (end (setf (tie note) nil))
               (continue (setf (tie note) 'begin))))
    (loop for note in (inside (last-container measure '(chord)))
          do (case (tie note)
               (begin (setf (tie note) nil))
               (continue (setf (tie note) 'end))))
    measure))


;from modifs

(defmethod collect-chords ((self container))
  (loop for object in (inside self)
        if (infra-group-p object) collect  object
        else append (collect-chords object)))


(defmethod collect-chords ((self simple-container)) (list self))

(defmethod collect-chords ((self list)) self)

;correction from Krim 3.7.2

(defun abstree (tree)
  (if (numberp tree) 
    ; (round (abs tree))
    ;(abs tree)
    (if (floatp tree) (round (abs tree)) (abs tree))
    (if (consp tree) 
      (cons (abstree (first tree)) (abstree (rest tree)))
      tree)))


;import from modif by aaa
(defmethod ot-clone ((self chord))
  (let ((object (call-next-method)))
    (loop for note in (inside object) do (setf (slot-value note 'tie) nil ))
    object))
          
(defmethod ot-clone ((self note))
   (let ((note (call-next-method)))
     (setf (slot-value note 'tie) nil)
     note))



;;; MIDI CHANNEL
(defmethod! set-channel ((self list) chan)
   :icon 148
   (loop for item in self collect (set-channel item chan)))
(defmethod! set-channel ((self container) chan)
   (loop for item in (inside self) do (set-channel item chan)))
(defmethod! set-channel ((self note) chan)
   (setf (chan self) chan))
(defmethod! set-channel ((self t) chan) t)


;;; VELOCITY 
(defmethod! set-vel ((self container) vel)
   (loop for item in (inside self) do (set-vel item vel)))
(defmethod! set-vel ((self list) vel)
   (loop for item in self do (set-vel item vel)))
(defmethod! set-vel ((self note) vel)
   (setf (vel self) vel))
(defmethod! set-vel ((self t) vel) t)


(defmethod get-object-vel ((self note)) (vel self))
(defmethod get-object-vel ((self t)) nil)
(defmethod get-object-vel ((self container)) 
  (or (om-mean (remove nil (mapcar 'get-object-vel (inside self)))) 0))


;=================== DURATION

(defmethod set-dur ((self note) dur) 
   (set-extent-ms self dur))
(defmethod set-dur ((self container) dur) 
   (loop for item in (inside self) do (set-dur item dur)))

;=================== OFFSET

(defmethod set-offset-ms ((note note) (offset integer))
   (when (parent note)
     (SetQvalue (parent note) 1000)
     (setf (slot-value note 'offset) offset)
     (QNormalize (parent note))))
(defmethod set-offset-ms ((note t) (offset integer)) t)
(defmethod set-offset-ms ((self container) (offset integer))
   (loop for item in (inside self) do (set-offset-ms item offset)))

;=============
(defmethod offset->ms-tempo-fixe ((self simple-container) tempo grandparent)
  (let ((limit (if (null grandparent) (parent self) grandparent)))
    (round (loop for current = self then (parent current)
                 for father = (parent current)
                 until (or (null current) (eq current limit))
                 when (null father) do (setf father current)
                 sum
                 (* 1000 (/ 60.0 tempo) (/ (offset current) (QValue father)))))))

;;; !!! redefinition of the method from container.lisp ...
(defmethod offset->ms ((self simple-container) &optional grandparent)
  (let ((limit (if (null grandparent) (parent self) grandparent)))
    (round (loop for current = self then (parent current)
                 for father = (and current (parent current))
                 until (or (null current) (eq current limit))
                 when (null father) do (setf father current)
                 sum
                 (get-offset->ms father (qtempo father) current)))))

(defmethod get-offset->ms ((self simple-container) (qtempo number) current)
  (* 1000 (/ 60.0 qtempo) (/ (offset current) (QValue self))))

(defmethod get-offset->ms ((self simple-container) (qtempo list) current)
  (if (= (length qtempo) 1)
      (* 1000 (/ 60.0 (second (car qtempo))) (/ (offset current) (QValue self)))
    (let* ((lasttempo (second (car qtempo)))
           (element-list (inside self))
           (last-fig (car element-list))
           (pos (position current element-list :test 'equal))
           (cont 0) stop)
      (if pos
          (loop for item in qtempo 
                while (not stop) do
                (if (>= (caar item) pos) (setf stop t)
                  (let* ((element (get-fig-from-path self (car item)))
                         (newtempo (second item))
                         (intervale (- (offset->ms-tempo-fixe element lasttempo self)
                                       (offset->ms-tempo-fixe last-fig lasttempo self))))
                    (setf last-fig element)
                    (setf cont (+ cont intervale))
                    (setf lasttempo newtempo)
                    )))
        )
     (+ (- (* 1000 (/ 60.0 lasttempo) (/ (offset current) (QValue self))) (offset->ms-tempo-fixe last-fig lasttempo self))
        cont))))


;=================== pas terrible !!

;;; &optional copy determines wether a modify copy of the object should be returned
;;; or if the modification occurs on the input object
;;; for compatibility with previous versions, copy defaults to NIL 
;;; in any case only the top-level container needs to be copied

(defmethod! set-port ((self note) port &optional (copy nil))
  :icon 148
  (let ((obj (if copy (clone self) self)))
    (setf (port obj) port)
    obj))

(defmethod! set-port ((self container) port &optional (copy nil))
  (let ((obj (if copy (clone self) self)))
    (loop for item in (inside obj) do (set-port item port nil))
    obj))
  
(defmethod! set-port ((self container) (port list) &optional (copy nil))
  (let ((obj (if copy (clone self) self)))
    (loop for item in (inside obj) 
          for p in port do 
          (set-port item p nil))
    obj))

(defmethod! set-port ((self list) port &optional (copy nil))
  (loop for item in self collect (set-port item port copy)))

(defmethod! set-port ((self t) port &optional (copy nil)) self)




(defmethod! get-port ((self note))
   :icon 148
   (port self))

(defmethod! get-port ((self container))
   (loop for item in (inside self) collect (get-port item)))

;not good:?
;(defmethod! get-port ((self voice)) nil)
(defmethod! get-port ((self t) ) nil)

(defmethod load-port-info ((self note) port)
   :icon 148
   (setf (port self) port))

(defmethod load-port-info ((self container) port)
   (loop for item in (inside self)
         for port-elem in (list! port) do
         (load-port-info item port-elem)))

(defmethod load-port-info  ((self t) port) nil)

;===============

(defmethod! get-measures ((self voice))
  :initvals (list  t) 
  :indoc '("a voice")
  :icon 134
  :doc "
Returns the list of all measure in <self>.
"
  (inside self))

(defmethod! get-measures ((self poly))
  (loop for voice in self
        append (get-measures voice)))

(defmethod! get-measures ((self t)) nil)

;===============

(defun get-fig-from-path (father path)
  ;; no #'inside unless obj is subclass of 'container (rest, note...)
  (if (or (null path) (not (typep father 'container)))
      father
      (get-fig-from-path  (nth (car path) (inside father)) (cdr path))))

;=========================================================================