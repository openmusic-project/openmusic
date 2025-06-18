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

(defclass* chord-seq (sequence* named-score-object tonal-object)   
  ((Lmidic :initform (list 6000) :accessor LMidic :initarg :LMidic :type t :documentation "pitches (mc): list or list of lists")
   (LOnset :initform (list  0 1000) :accessor LOnset :initarg :LOnset :type t :documentation "onsets (ms): list")
   (Ldur :initform (list 1000) :accessor Ldur :initarg :Ldur :type t :documentation "durations (ms): list or list of lists")
   (LVel :initform (list 100) :accessor LVel :initarg :LVel :type t :documentation "velocities (0-127): list or list of lists")
   (LOffset :initform (list 0) :accessor LOffset :initarg :LOffset :type t :documentation "offsets (ms): list or list of lists")
   (Lchan :initform (list 1) :accessor Lchan :initarg :Lchan :type t :documentation "MIDI channels (1-16): list or list of lists")
   (legato :initform 0 :accessor legato :initarg :legato :type integer :documentation "relative chords duration (0-100)")
   (approx :initform *global-midi-approx* :accessor approx  :type integer))
  (:icon 138)
  (:documentation "
A sequence of chords.

Time is expressed in absolute dates and durations. (For rhytmic structures see the VOICE object.)

CHORD-SEQ is defined with:

- <lmidic>: a list of list of pitches (midicents: 100 = 1 half-tone - 6000 = C3); e.g. '((6000 6500) (6100 6700 7100)). Each pitch list is considered as a chord in teh sequence. Single-note sequences can be specified with simple list of pitches, e.g. (6000 61000 6800) ; in this case each pitch will be considered as a single-note chord.
- <lonsets>: a list of onsets in milliseconds (one for each chord). If the list is shorter than the list of pitch, the last interval is repeated.
- <ldur>: a list or list of lists values (in milliseconds) expressing chords durations or note durations inside each chord.
- <lvel>: a list or list of lists of values (MIDI velocity from 0 to 127) expressing chords velocities or note durations inside each chord.
- <loffset>: a list or list of lists of values (milliseconds) expressing note offsets for the notes inside each chord.
- <lchan>: a list or list of lists of values (1-16) expressing MIDI channels for each chord or notes inside each chord.
- <legato>: a number between 0 and 100, indicating the duration of chords as a percentage of inter-onsets time intervals. If different from 0 (the default), the ldur and loffset inputs are ignored, and notes in the chords are formatted with regard to the legato value.

All values (excepted onsets and legato) are returned (in the box outputs) as list of lists (one value for each note, missing values computed from previous notes or chords).

")
  )

(defmethod chord-seq-p ((self chord-seq)) t)
(defmethod chord-seq-p ((self t)) nil) 

(defmethod initialize-instance ((self chord-seq) &rest initargs  &key (Empty nil) (LPort nil))
   (declare (ignore initargs)) 
   (call-next-method)
   (unless Empty
     (do-initialize self 
                    :LMidic (slot-value self 'LMidic)
                    :LVel (slot-value self 'LVel)  
                    :Loffset (slot-value self 'LOffset) 
                    :Ldur (slot-value self 'LDur) 
                    :Lonset (slot-value self 'LOnset) 
                    :LChan (slot-value self 'LChan) 
                    :Legato (slot-value self 'legato)
                    :approx (slot-value self 'approx)
                    :LPort LPort))
   (setf (slot-value self 'LMidic) nil  (slot-value self 'LVel) nil 
         (slot-value self 'LOffset) nil  (slot-value self 'LDur) nil
         (slot-value self 'LOnset) nil (slot-value self 'LChan) nil)
   self)





(defmethod do-initialize ((self chord-seq)  &key LMidic LVel Loffset  Ldur  Lonset  LChan  Legato LPort approx)
  (let ((midics (list! LMidic))
        (vels (list! LVel))
        (durs (list! LDur))
        (offsets (list! LOffset))
        (chans (list! LChan))
        (ports (list! LPort))
        (approx approx)
        (defdelay (if (>= (length LOnset) 2)
                      (- (car (last LOnset))
                         (car (last LOnset 2)))
                    1000))
        (defstart (or (pop LOnset) 0)))
    
    (setQValue self 1000 :recursive nil)
    
    (cond 
     ((list-subtypep midics '(chord))
      (setf (inside self) 
            (mapcar #'(lambda (object) (ObjfromObjs object (mki (type-of object))))
                    midics)))
     ((list-subtypep midics '(note))
      (setf (inside self) 
            (mapcar #'(lambda (object) (ObjfromObjs object (mki 'chord))) 
                    midics)))
     (t 
      (setf (inside self)
            (loop while (or midics vels durs offsets ports)
                  for midic = (or (pop midics) midic)
                  for vel = (or (pop vels) vel)
                  for dur = (or (pop durs) dur)
                  for offset = (or (pop offsets) offset)
                  for chan = (or (pop chans) chan)
                  for port = (or (pop ports) port) ; (list 0))
                  collect (let ((chord (mki 'chord 
                                            :Lmidic (list! midic) 
                                            :Lvel   (list! vel)
                                            :Ldur    (list! dur )
                                            :Loffset (list! offset)
                                            :LChan (list! chan)
                                            )))
                            (setf (LPort chord) port)
                            (setf (approx chord) approx)
                            ;(setf (approx chord) (approx self))
                            chord) ))))

    (loop for chord in (inside self)
          for onset = defstart then (or (pop LOnset)  (+ onset defdelay))
          for outset = (or (first LOnset)  (+ onset defdelay))
          do  (setf (slot-value chord 'offset)  (round onset))
          (InContext self (setf (extent chord) (round (- outset onset))))
          )
    
    (adjust-extent self)
    (QNormalize self)
    
    (when (> legato 0)
      (propagate-tempo self)
      (normalize-chord self legato)))
  self)


;;; changes pour ACL7.0

(defmethod untie-chords ((self chord-seq))
  (QReduce self) 
  (adjust-extent self)
  (loop for chord in (inside self) do (untie-chords chord))
  (setf (inside self)
        (loop for chord in (inside self)
              ;; GA 11/04/09 for onset in (lonset self)
              for onset = (offset chord)
              if (not (null (inside chord))) 
              collect chord into chords and collect onset into onsets
              finally   ;;;do  
              ;GA 11/04/09  (loop for extent in  (x->dx (append onsets (last (lonset self))))
              (loop for extent in  (x->dx (append onsets (list (extent self))))
                    for chord in chords
                    do (setf (extent chord) extent))
              (return chords)))
  (QNormalize self)
  self
  )


(defmethod chords ((self chord-seq))
   (inside self))

;GET SLOTS
(defmethod LMidic ((self chord-seq))
  (loop for chord in (inside self)
        collect (approx-m (Lmidic chord) (approx self))))

(defmethod Lvel ((self chord-seq))
   (loop for chord in (inside self)
         collect (Lvel chord)))

(defmethod LDur ((self chord-seq))
   (loop for chord in (inside self)
         collect (Ldur chord)))

(defmethod LOffset ((self chord-seq))
   (loop for chord in (inside self)
         collect (LOffset chord)))

(defmethod LOnset ((self chord-seq))
  (nconc (loop for chord in (inside self)
               collect (Offset->ms chord))
          (list (get-obj-dur self))))


(defmethod LChan ((self chord-seq))
   (loop for chord in (inside self)
         collect (LChan chord)))

(defmethod LPort ((self chord-seq))
  (get-port self))

(defmethod approx ((self chord-seq))
  (slot-value self 'approx))


(defmethod notEndLOnset ((self chord-seq))
  (butlast (LOnset self)))


;SET SLOTS
(defmethod (setf Lmidic) ((LMidic list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic LMidic
                  :LVel (LVel self)
                  :LOnset (LOnset self)
                  :LOffset (LOffset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)
                  :approx (approx self)))

(defmethod (setf LVel) ((LVel list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel LVel
                  :LOnset (LOnset self)
                  :LOffset (LOffset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)
                  :approx (approx self)))

(defmethod (setf LOffset) ((LOffset list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset LOffset
                  :LOnset (LOnset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)
                  :approx (approx self)))

(defmethod (setf LDur) ((Ldur list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOnset (LOnset self)
                  :LOffset (LOffset self)
                  :LDur lDur
                  :LChan (LChan self)
                  :Legato (legato self)
                  :approx (approx self)))

(defmethod (setf LOnset) ((LOnset list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset (LOffset self)
                  :LOnset LOnset
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)
                  :approx (approx self)))

(defmethod (setf LChan) ((LChan list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset (LOffset self)
                  :LOnset (LOnset self)
                  :LDur (LDur self)
                  :LChan LChan
                  :Legato (legato self)
                  :approx (approx self)))

(defmethod (setf Legato) ((Legato number) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset (LOffset self)
                  :LOnset (LOnset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato legato
                  :approx (approx self)))

#|
(defmethod (setf apprpx) ((approx number) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset (LOffset self)
                  :LOnset (LOnset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)
                  :approx approx ))
|#

(defmethod (setf approx) ((approx number) (self chord-seq))
  (call-next-method)
  (loop for chord in (inside self)
        do (setf (approx chord) approx))
  self)


(defmethod (setf LPort) ((LPort list) (self chord-seq))
  (loop for ports in LPort
        for chord in (inside self)
        do (setf (Lport chord)  ports))
  self)



(defmethod* Objfromobjs ((Self chord) (Type Chord-seq)) 
(let ((chrdseq (ObjFromObjs (list self) Type)))
  (setapprox type (approx self));;Edo scale heritage
  (setf (approx chrdseq) (approx self))
  chrdseq))

(defmethod* Objfromobjs ((Self note) (Type Chord-seq)) 
(let ((chrdseq (ObjFromObjs (list self) Type)))
  (setapprox type (approx self));;Edo scale heritage
  (setf (approx chrdseq) (approx self))
  chrdseq))


(defmethod* Objfromobjs ((Self list) (Type Chord-seq))
  (cond
   ((list-subtypep self '(chord note))
    (let ((chord-seq (make-instance (type-of type) :empty t)))
      (setQValue chord-seq 1000 :recursive nil)
      (if (list-subtypep self '(note))
        (setf (inside chord-seq) 
              (mapcar #'(lambda (object) (ObjfromObjs object (mki 'chord)))
                      self))
        (setf (inside chord-seq) 
              (mapcar #'(lambda (object) (ObjfromObjs object (mki (type-of object))))
                      self)))
      (loop for chord in (inside chord-seq)
            for onset from 0 by 1000
            do  (setf (slot-value chord 'offset)  onset)
              (InContext chord-seq (setf (extent chord) 1000)))
      (QNormalize chord-seq)
      (adjust-extent chord-seq)
      chord-seq))    
   (t nil)))



(defmethod* Objfromobjs ((Self sequence*) (Type Chord-seq))
  (if (subtypep (type-of self) (type-of type))
    (call-next-method)
    (let (;(chordseq  (flatten-container self '(chord note) (type-of type)))
          (chordseq  (collect-and-transform self '(chord note rest))));for grace notes compat
      (SET-TEMPO chordseq 60) ; in case we come from a voice in a diff tempo
      (cast-to-chords chordseq)
      (untie-chords chordseq)
      (adjust-extent chordseq)
      ;not working for maquette! TODO
      ;(setapprox type (approx self));;Edo scale heritage
      (setf (approx chordseq) (approx self))
      chordseq)))



(defun mk-chord-at (t-time pitch-list dur-list offset-list chan-list vel-list port-list)
  (let ((chord (mki 'chord
                     :LMidic pitch-list 
                     :Ldur dur-list 
                     :LOffset offset-list
                     :Lchan chan-list
                     :Lvel vel-list
                     :Lport port-list
                     ;:approx approx 
                     )))
    (setf (offset chord) t-time)
    chord))



(defmethod execption-save-p ((self chord-seq)) 'chord-seq) 
(defmethod save-exepcion ((self chord-seq))
  (let* ((list (get-tonal-values self)))
    (if list
      `(when (find-class ',(type-of self) nil)
         (let ((rep
                (make-instance ',(type-of self)
                  :LMidic ',(Lmidic self)
                  :LOnset ',(LOnset self)
                  :Ldur ',(Ldur self)
                  :LVel ',(LVel self)
                  :LOffset ',(LOffset self)
                  :Lchan ',(Lchan self)
                  :legato ,(legato self)
                  :approx ,(approx self))))
           (restore-tonalite rep ',list)
           rep))
      `(when (find-class ',(type-of self) nil)
         (make-instance ',(type-of self)
           :LMidic ',(Lmidic self)
           :LOnset ',(LOnset self)
           :Ldur ',(Ldur self)
           :LVel ',(LVel self)
           :LOffset ',(LOffset self)
           :Lchan ',(Lchan self)
           :legato ,(legato self)
           :approx ,(approx self)))
      )))

(defmethod get-tonal-values ((self chord-seq))
  (let ((list ()))
    (loop for chord in (inside self) for i = 0 then (+ i 1) do
          (when (tonalite chord) 
            (push (list i (omng-save (tonalite chord))) list))
          (loop for note in (inside chord) for j = 0 then (+ j 1) do
                (when (tonalite note) 
                  (push (list (list i j) (omng-save (tonalite note))) list))))
    (remove nil (reverse list))))

(defmethod restore-tonalite ((self chord-seq) ton-list)
  (loop for ton in ton-list do
        (if (consp (car ton))
            (set-tonalite (nth (cadr (car ton)) (inside (nth (car (car ton)) (inside self)))) (eval (cadr ton)))
          (set-tonalite (nth (car ton) (inside self)) (eval (cadr ton)))
          )))


;;; !!!!!!!! RANGEMENT
(defmethod get-tonal-values ((self chord))
  (let ((list ()))
    (loop for note in (inside self) for i = 0 then (+ i 1) do
          (when (tonalite note) 
            (push (list i (omng-save (tonalite note))) list)))
    (remove nil (reverse list))))
  
(defmethod restore-tonalite ((self chord) ton-list)
  (loop for ton in ton-list do
        (set-tonalite (nth (car ton) (inside self)) (eval (cadr ton)))))

#|
ahora
(midic date dur vel chan track)
antes
(midic dur vel chan date track)
make-quanti
(midic date dur)
|#

(defun make-quanti-chords (note-list delta)
  (loop while note-list
        for note = (car note-list)
        with pitch-list and dur-list  and offset-list and chan-list and vel-list and port-list
        with base-time = (second (first note-list))
        if (<= (- (second (first note-list)) base-time) delta)
        do 
        (push (* 100 (first note)) pitch-list)
        ;;; (push (first note) pitch-list) 
        (push (third note) dur-list)
        (push (fifth note) chan-list)
        (push (fourth note) vel-list)
        (push (sixth note) port-list)
        (push (- (second note) base-time) offset-list)
        (pop note-list)
        else
        collect (mk-chord-at base-time  pitch-list dur-list offset-list chan-list vel-list port-list) into result
        and do (setf base-time (second note) pitch-list () dur-list ()   offset-list () chan-list () vel-list ())
        finally (return (append result (list  (mk-chord-at base-time  pitch-list dur-list offset-list chan-list vel-list port-list))))))


(defun make-quanti-chords-MC (note-list delta)
  (loop while note-list
        for note = (car note-list)
        with pitch-list and dur-list  and offset-list and chan-list and vel-list and port-list
        with base-time = (second (first note-list))
        if (<= (- (second (first note-list)) base-time) delta)
        do 
        (push (first note) pitch-list)
        ;;; (push (first note) pitch-list) 
        (push (third note) dur-list)
        (push (fifth note) chan-list)
        (push (fourth note) vel-list)
        (push (sixth note) port-list)
        (push (- (second note) base-time) offset-list)
        (pop note-list)
        else
        collect (mk-chord-at base-time  pitch-list dur-list offset-list chan-list vel-list port-list) into result
        and do (setf base-time (second note) pitch-list () dur-list ()   offset-list () chan-list () vel-list () port-list ())
        finally (return (append result (list  (mk-chord-at base-time  pitch-list dur-list offset-list chan-list vel-list port-list))))))


;=== Redefinition of make-quanti-chords and mk-chord-at functions
;=== with complete midi notes descrption : note = (pitch date dur vel chan track port)
;=== to include midiport in chords

(defun make-quanti-chords-with-midiport (note-list delta)
  (loop while note-list
        for note = (car note-list)
        with pitch-list and dur-list  and offset-list and chan-list and vel-list and port-list
        with base-time = (second (first note-list))
        if (<= (- (second (first note-list)) base-time) delta)
        do 
        (push (* 100 (first note)) pitch-list)
        ;;; (push (first note) pitch-list) pour gérer les MC
        (push (third note) dur-list)
        (push (fifth note) chan-list)
        (push (fourth note) vel-list)
        (push (seventh note) port-list)
        (push (- (second note) base-time) offset-list)
        (pop note-list)
        else
        collect (mk-chord-at-with-midiport base-time  pitch-list dur-list offset-list chan-list vel-list port-list) into result
        and do (setf base-time (second note) pitch-list () dur-list () offset-list () chan-list () vel-list () port-list ())
        finally (return (append result (list  (mk-chord-at-with-midiport 
                                               base-time  pitch-list dur-list offset-list chan-list vel-list port-list))))))


(defun mk-chord-at-with-midiport (t-time pitch-list dur-list offset-list chan-list vel-list port-list)
  (let ((chord (mki 'chord
                     :LMidic pitch-list 
                     :Ldur dur-list 
                     :LOffset offset-list
                     :Lchan chan-list
                     :Lvel vel-list
                     :Lport port-list)))
    (setf (offset chord) t-time)
    chord))



;=== Conversion Chord-seq -> voice : dans le cas ou le chord-seq ne commence pas 0
(defmethod* objFromObjs ((self chord-seq) (type voice))
            (if (chords self)
                (let* ((newchordseq (align-chords self *global-deltachords*)); PROB....
                       (quantypar *quantify-def-params*)
                       (durs (append (butlast (x->dx (lonset newchordseq)))
                                     (list (extent->ms (car (last (chords newchordseq)))))))
                       (durs (if (zerop (car (lonset newchordseq)))
                                 durs (cons (* (car (lonset newchordseq)) -1) durs)))
                       (newvoice (make-instance (type-of type)
                                                :tree (if *quant-grace* 
                                                          (omg-quantify  
                                                           durs
                                                           (first quantypar)
                                                           (second quantypar)
                                                           (third quantypar)
                                                           (fifth quantypar)
                                                           (fourth quantypar) ;bug utiliser onset
                                                           (sixth quantypar))
                                                        (omquantify  
                                                         durs
                                                         (first quantypar)
                                                         (second quantypar)
                                                         (third quantypar)
                                                         (fifth quantypar)
                                                         (fourth quantypar) ;bug utiliser onset
                                                         (sixth quantypar)))
                                                        
                                                :tempo (first quantypar)
                                                :legato 100 ;KH fix 240919
                                                :chords  (chords newchordseq)
                                                ;:approx (approx self); ne suffit pas car ne propage pas!
                                                )))
                  (setf (approx newvoice) (approx self)); il faut utiliser la methode!
                  newvoice)
              (make-instance (type-of type)
                             :tree '(0 nil)
                             :tempo (first *quantify-def-params*)
                             :legato 0
                             :chords  nil)))

;;;;;Fix Merger poly->chrd-seq
;;;Ici in a desormais les 1/4 de tons et 
;;; le chord-seq n'est plus en tie = 100
(defmethod chord-seq->mf-info ((self chord-seq))
    (loop for lpitch in (lmidic self)
          for onset in (lonset self)
          for ldur in (ldur self)
          for lvel in (lvel self)
          for loffset in (loffset self)
          for lchan in (lchan self)
          for lport in (lport self)
          append  (loop for pitch in lpitch
                        for dur in ldur
                        for vel in lvel
                        for offset in loffset
                        for chan in lchan
                        for port in lport
                        collect 
                        ;(list pitch (+ onset offset) dur vel chan)   
                        (list (round pitch 100) (+ onset offset) dur vel chan port)
                        ))
    )

(defmethod chord-seq->mf-info-MC ((self chord-seq))
    (loop for lpitch in (lmidic self)
          for onset in (lonset self)
          for ldur in (ldur self)
          for lvel in (lvel self)
          for loffset in (loffset self)
          for lchan in (lchan self)
          for lport in (lport self)
          append  (loop for pitch in lpitch
                        for dur in ldur
                        for vel in lvel
                        for offset in loffset
                        for chan in lchan
                        for port in lport
                        collect 
                        (list pitch (+ onset offset) dur vel chan port)
                        ))
    )

(defmethod mf-info->chord-seq ((self list) &optional deltachord)
  (let* ((chords (make-quanti-chords self (or deltachord *global-deltachords*)))
         (lonset (mapcar 'offset chords))
         (last-note (first (inside (first (last chords))))))
    (setf lonset (append lonset (list (+ (extent->ms last-note) (first (last lonset)))))) 
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset 
      ;:legato 100
      )))



(defmethod mf-info-MC->chord-seq ((self list) &optional deltachord)
  (let* ((chords (make-quanti-chords-MC self (or deltachord *global-deltachords*)))
         (lonset (mapcar 'offset chords))
         (last-note (first (inside (first (last chords))))))
    (setf lonset (append lonset (list (+ (extent->ms last-note) (first (last lonset)))))) 
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset 
      ;:legato 100
      )))


(defmethod align-chordseq-chords ((self chord-seq))
  (align-offsets (mf-info-mc->chord-seq (chord-seq->mf-info-mc self))))


(defmethod align-offsets ((self chord-seq))
  (loop for chord in (inside self)
        do (loop for note in (inside chord)
                 do (setf (slot-value note 'offset) 0)))
  self)


(defmethod* Objfromobjs ((Self poly) (Type Chord-seq))
  (let ((chordseq
         (reduce #'merger 
                 (mapcar #'(lambda (voice) (objFromObjs voice type)) (inside self)))))
    ;(setapprox type (approx self));;Edo scale heritage
    (setf (approx chordseq) (approx self))
    chordseq))

;;; POLY -> VOICE = just keeps the first voice of the poly
(defmethod* Objfromobjs ((Self poly) (Type voice))
  (let ((voice (ObjFromObjs (first (voices self)) type)))
      ;(setapprox type (approx self));;Edo scale heritage
      (setf (approx voice) (approx self))
      voice)) 

(defmethod* Objfromobjs ((Self voice) (Type poly))
  (let ((poly (make-instance (type-of type) :voices (list (clone self)))))
  ;(setapprox type (approx self));;Edo scale heritage
  (setf (approx poly) (approx self))
  poly))




;;;====================================
;;; CHORD-SEQ TOOL BOX
;;;====================================

(defmethod* align-chords ((self chord-seq) (delta number))
  :initvals (list nil 100)
  :indoc '("a chord-seq" "an integer")
  :icon 230
  :doc "
Transforms <self> so that notes falling in a small time interval are grouped into a chord.

<delta> gives the time interval in ms.
"
  (let ((note-seq (flatten-container self 'note 'chord-seq))
        (chseq (make-instance 'chord-seq :empty t)) 
        note-list chord-list) 
    (when (inside self)
      (setQValue note-seq 1000 :recursive t) 
      (setQValue chseq 1000 :recursive nil)
      (setf note-list (inside note-seq))
      (setf chord-list
            (loop while note-list
                  for note = (car note-list)
                  with pitch-list and dur-list  and offset-list and chan-list and vel-list and port-list
                  with base-time = (offset (first note-list))
                  if (<= (- (offset (first note-list)) base-time) delta)
                  do 
                  (push  (midic note) pitch-list)
                  (push (extent note) dur-list)
                  (push (chan note) chan-list)
                  (push (vel note) vel-list)
                  (push (port note) port-list)
                  (push 
                 ;(- (offset note) base-time) this if wanna keep note offsets 
                   0 offset-list)
                  (pop note-list)
                  else
                  collect (mk-chord-at base-time  pitch-list dur-list offset-list chan-list vel-list port-list) into result
                  and do (setf base-time (offset note) pitch-list () dur-list ()   offset-list () chan-list () vel-list ())
                  finally (return (append result (list  (mk-chord-at base-time  pitch-list dur-list offset-list chan-list vel-list port-list ))))))
      (setf (inside chseq) chord-list)
      (adjust-extent chseq))
    chseq))

(defmethod* align-chords ((self chord-seq) (delta null)) self)


(defmethod* merger ((chs1 chord-seq) (chs2 chord-seq))
  (if (and (inside chs1) (inside chs2))
      (let* ((mf (sort (nconc (chord-seq->mf-info-MC chs1)  (chord-seq->mf-info-MC chs2))
                       #'< :key #'second)))
        (mf-info-MC->chord-seq mf 0))
    ;; one is empty...
    (if (inside chs1) 
        (clone chs1) 
      (clone chs2))))


(defmethod* temporal-sort ((self chord-seq))
  :indoc '("an object containing temporal events to be sorted")
  :initvals '(nil)
  :doc "Sorts the events in a copy of <self> in temporal order, returns the new copy"
  :icon 915
  (let ((out-cs (mki 'chord-seq :empty t :qvalue 1000))
	(data (sort (mapcar #'(lambda (ons dat) (cons ons dat))
			    (lonset self)
			    (inside self))
		    #'< :key #'car)))
    (setf (inside out-cs) (mapcar #'cdr data))
    (setf (lonset out-cs) (mapcar #'car data))
    (adjust-extent out-cs)
    (QNormalize out-cs)
    out-cs))


;;; by Gilbert Nouno
(defmethod* split-voices ((self chord-seq) &optional (random nil))
  :indoc '("a 'polyphonic' chord-seq" "random distribution strategy?")
  :initvals '(nil nil)
  :doc "Separates a CHORD-SEQ with overlapping notes into a list of monoponic CHORD-SEQs

If <random> = T, voice distribution is chosen randomly. Otherwise the first available voice is selected."
  :icon 915
  
  (let ((chords-lists nil))
    
    (loop for chord in (get-chords self)
          for time in (lonset self)
          for i = 0 then (1+ i) do
          
          (let ((position nil) 
                (list-indices (arithm-ser 0 (1- (length chords-lists)) 1)))
            (if random (setf list-indices (permut-random list-indices)))
            (loop for n in list-indices
                  while (not position) do 
                  (let ((voice? (nth n chords-lists)))
                    (when (> time (+ (car (car voice?)) (list-max (ldur (cadr (car voice?))))))
                      (setf (nth n chords-lists) (cons (list time chord) (nth n chords-lists)))
                      ;(setf voice? (cons (list time chord) voice?))
                      (setf position t))))
            
            (unless position
              (setf chords-lists
                    (append chords-lists (list (list (list time chord))))))
              
            ))

    ;;; chords-list =  
    ;;; (((onset1 chord1) (onset2 chord2) ...)
    ;;;  ((onset1 chord1) (onset2 chord2) ...)
    ;;;  ...[n times]... )

    (loop for list in chords-lists collect
          (let ((chords (reverse (mapcar 'cadr list)))
                (onsets (reverse (mapcar 'car list))))
            (make-instance 'chord-seq
                           :lonset onsets
                           :lmidic (mapcar 'lmidic chords)
                           :ldur (mapcar 'ldur chords)
                           :lvel (mapcar 'lvel chords)
                           :lchan (mapcar 'lchan chords))))
    )
 
)



;======================================================
; MULTI-SEQ
;======================================================

(defclass* multi-seq (superposition tonal-object) 
  ((chord-seqs :initform (list (make-instance 'chord-seq)) 
               :initarg :chord-seqs :accessor chord-seqs :type t :documentation "list of CHORD-SEQ objects"))
  (:icon 261)
  (:documentation "
MULTI-SEQ is a polyphonic object made of a superimposition of CHORD-SEQ objects.
")
  )
 
(defmethod multi-seq-p ((self multi-seq)) t)
(defmethod multi-seq-p ((self t)) nil) 

(defmethod initialize-instance ((self multi-seq) &rest initargs &key (Empty nil))
  (declare (ignore initargs)) 
  (call-next-method)
  (unless Empty (do-initialize self :chord-seqs (slot-value self 'chord-seqs)))
  (setf (slot-value self 'chord-seqs) nil)
  self)



(defmethod do-initialize ((self multi-seq) &key chord-seqs)
  (cond ((and (consp chord-seqs) 
              (list-subtypep chord-seqs 'chord-seq))
         (loop for seq in chord-seqs 
               do  (om-assemble-into-multi self seq)))
        ((subtypep (type-of chord-seqs) 'chord-seq)
         (om-assemble-into-multi self  chord-seqs))
        (t (om-beep-msg (format nil "~A must be initialized with a chord-seq list!" (type-of self))) 
           (om-assemble-into-multi self (make-instance 'chord-seq))))
  self)


(defmethod chord-seqs ((self multi-seq))
  (setf (approx self) (approx self));persistance
  (loop for c in (inside self) collect 
        (let ((c2 (clone c)))
          (setqvalue c2 1)
         ; (setf (extent c2) (real-dur c2));;;gives errounous extent.
          (qnormalize c2)
          c2)))


(defmethod (setf chord-seqs) ((chseqs list) (self multi-seq))
  (do-initialize self :chord-seqs chseqs)) 

(defmethod (setf approx) ((approx number) (self multi-seq))
  (call-next-method)
  (loop for i in (inside self)
        do (setf (approx i)  approx))
  self)

(defmethod om-assemble-into-multi ((self multi-seq) (s2 chord-seq))
  (setf s2 (clone s2))
  (let ((frac-min (fraction-minimale-commune self s2)))
    (change-qvalue self frac-min)
    (change-qvalue s2 frac-min)
    (let ((new-extent (max (extent self) (extent s2))))
      (setf (slot-value s2 'offset) 0
            (slot-value self 'offset) 0
            (slot-value self 'qvalue) frac-min
            (slot-value self 'extent) new-extent
            (slot-value self 'inside) (append (inside self) (list s2)))
      (loop for chordseq in (inside self)
            when (< (extent chordseq) new-extent)
            do (setf (extent chordseq) new-extent)) ;;; attention !!! 
      self)))


(defmethod* Objfromobjs ((Self multi-seq) (Type Chord-seq))
  (let ((chordseq (reduce #'merger (inside self))))
  ;(setapprox type (approx self));;Edo scale heritage
  (setf (approx chordseq) (approx self))
  chordseq))

(defmethod* Objfromobjs ((Self chord-seq) (Type multi-seq))
  (let ((multi (make-instance 'multi-seq :chord-seqs (list self))))
  ;(setapprox type (approx self));;Edo scale heritage
  (setf (approx multi) (approx self))
  multi))

(defmethod* Objfromobjs ((Self multi-seq) (Type poly))
  (let* ((voice (make-instance 'voice))
         (poly (make-instance 'poly :voices (mapcar #'(lambda (chseq) (objFromObjs chseq voice)) (inside self)))))
    ;(setapprox type (approx self));;Edo scale heritage
    (setf (approx poly) (approx self))
    poly))

(defmethod* Objfromobjs ((Self poly) (Type multi-seq))
  (let ((multi (make-instance 'multi-seq 
                              :chord-seqs (loop for voice in (inside self)
                                                collect (ObjFromObjs voice (make-instance 'chord-seq))))))
    ;(setapprox type (approx self));;Edo scale heritage
    (setf (approx multi) (approx self))
    multi))


(defmethod! align-chords ((self multi-seq) (delta integer))
  (mki 'multi-seq :chord-seqs (car-mapcar 'align-chords (chord-seqs self) delta)))


(defmethod! align-chords ((self multi-seq) (delta null)) self)




;;;=============================================

;;; !!! ça marche pas du tout !!!
(defmethod* mk-obj-from-list ((self list) (type chord-seq))
  (cond
   ((list-subtypep self '(chord note rest))
    (let* ((filtlist (remove nil (loop for item in self collect 
                                       (cond ((typep item 'note) 
                                              (let ((new (objfromobjs item (make-instance 'chord))))
                                                (setf (parent new) (parent (parent item)))
                                                (setf (offset new) (offset (parent item)))
                                                new))
                                             ((typep item 'chord) item)
                                             (t nil)))))
           (clist (sort filtlist '< :key 'offset->ms))
           (minoffset (offset->ms (car clist)))
          offset-list onset-list midic-list vel-list dur-list chan-list port-list
          chord-seq)
      
      (loop for chord in clist do
          (pushr (- (offset->ms chord) minoffset) onset-list)
          (pushr (loffset chord) offset-list)
          (pushr (ldur chord) dur-list)
          (pushr (lmidic chord) midic-list)
          (pushr (lvel chord) vel-list)
          (pushr (lchan chord) chan-list)
          (pushr (lport chord) port-list)
          )
      
      (setf chord-seq (make-instance (type-of type) :lonset onset-list 
                        :lmidic midic-list :ldur dur-list :loffset offset-list :lvel vel-list :lchan chan-list
                        :lport port-list))
      ;;; new tonalite
      (loop for c in clist
            for i = 0 then (+ i 1) do
            (when (tonalite c) (set-tonalite (nth i (inside chord-seq)) (tonalite c)))) 
      (when (get-tonalite (parent (car clist))) (set-tonalite chord-seq (get-tonalite (parent (car clist)))))
      ;;;
      
      chord-seq))    
   (t nil)))


