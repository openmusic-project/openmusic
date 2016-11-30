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


;;; ====================================================================
;;;
;;;      OM2FINALE 3.0 v1.0
;;;
;;;      © 2000 IRCAM - Gerard Assayag ,Carlos Agon & Karim Haddad
;;;
;;; ====================================================================


;***********************************************************************


;;; ====================================================================
;;;
;;;      CLENI: Common Lisp to Enigma Interface 3.0
;;;
;;;      © 2000 IRCAM - Gerard Assayag & karim Haddad
;;;
;;; ====================================================================

(in-package :om)

(defpackage "CLENI" 
  (:use "COMMON-LISP"))

(in-package "CLENI")




(export '(new-score describe-score translate-score *half-tone* *quarter-tone*
           *G-key* *C-3-key* *C-4-key* *F-key* ))

(defclass Score ()
  (
   (temperament :initform 2)
   (voice-system :initform () :accessor voice-system)
   (current-voice :initform 0 :accessor current-voice)
   (staff-system :initform () :accessor staff-system)
   (measure-system :initform () :accessor measure-system)
   (current-measure :initform () :accessor current-measure)
   (event-system :initform () :accessor event-system)
   (current-event :initform () :accessor current-event)
   (tuplet-list :initform () :accessor tuplet-list)
   (current-tuplet :initform () :accessor current-tuplet)
   (tuplet-level :initform 0 :accessor tuplet-level)
   (tuplet-max-level :initform 0 :accessor tuplet-max-level)
   ))

(defclass VoiceSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (voice-list :initform () :accessor voice-list)
   ))

(defclass Voice ()
  (
   (parent-system :initform () :initarg :parent-system :accessor parent-system)
   (event-list :initform () :accessor event-list)
   (voice-number :initform 0 :accessor voice-number :initarg :voice-number)
   ))

(defclass EventSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (event-list :initform () :accessor event-list)
   (event-count :initform 0 :accessor event-count)
   ))

(defclass StaffSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (staff-list :initform () :accessor staff-list)
   ))

(defclass Staff ()
  (
   (parent-system :initform nil :initarg :parent-system :accessor parent-system)
   (start-key :initform 0 :accessor start-key :initarg :start-key)
   (voice :initform () :accessor voice :initarg :voice)
   ))

(defclass MeasureSystem ()
  (
   (parent-score :initform () :accessor parent-score :initarg :parent-score)
   (measure-list :initform () :accessor measure-list)
   (biggest-measure :initform ())
   ))

(defclass Measure ()
  (
   (parent-system :initform nil :initarg :parent-system :accessor parent-system)
   (num :initform 4 :accessor num :initarg :num)
   (denum :initform 4 :accessor denum :initarg :denum)
   (width :initform 600 :accessor width :initarg :width)
   (measure-number :initform 0 :accessor measure-number :initarg :measure-number)))

(defclass ScoreEvent ()
  (
   (parent-score  :initform nil :initarg :parent-score :accessor parent-score)
   ;;; 4ter note = 1024. 8th note = 256. etc.
   (event-duration :initform 1024 :accessor event-duration :initarg :event-duration)
   (rank :initform 0 :accessor rank)
   (event-voice :accessor event-voice)
   (event-measure :accessor event-measure)
   (event-tuplet :initform () :accessor event-tuplet)
   (follow :initform () :accessor follow :initarg :follow)
   (first-in-tuplet :initform () :accessor first-in-tuplet)
   ))


(defclass Note (ScoreEvent)
  (
   (pitch :initform #x0000 :accessor pitch :initarg :pitch)
   (begin-tie :initform () :accessor begin-tie :initarg :b-tie)
   (index-in-chord :initform 1 :accessor index-in-chord :initarg :index-in-chord)
   (n-notes :initform 1 :accessor n-notes :initarg :n-notes)
   (note-number :initform 0 :accessor note-number :initarg :note-number)
   (voice-num :initform 0 :accessor voice-num :initarg :voice-num)
   (measure-num :initform 0 :accessor measure-num :initarg :measure-num)
   ))


(defclass Crest (ScoreEvent)
  (   
   (pitch :initform #x0060 :accessor pitch :initarg :pitch)
   (index-in-chord :initform 1 :accessor index-in-chord :initarg :index-in-chord)
   (n-notes :initform 1 :accessor n-notes :initarg :n-notes)
   (note-number :initform 0 :accessor note-number :initarg :note-number)
   (voice-num :initform 0 :accessor voice-num :initarg :voice-num)
   (measure-num :initform 0 :accessor measure-num :initarg :measure-num)
   ))


(defclass Chord (ScoreEvent)
  (
   (note-list
    :initform (list (make-instance 'note) 
                    (make-instance 'note :pitch 6600) 
                    (make-instance 'note :index-in-chord 2)
                    (make-instance 'note :n-notes 2)
                    (make-instance 'note :note-number 0)
                    (make-instance 'note :voice-num 0)
                    (make-instance 'note :measure-num 0)) 
    :accessor note-list :initarg :note-list)
   (voice-num :initform 0 :accessor voice-num :initarg :voice-num)
   (note-number :initform 0 :accessor note-number :initarg :note-number)
   (measure-num :initform 0 :accessor measure-num :initarg :measure-num)
   )
  )



(defclass Tuplet ()
  (
   (nbunit1 :initform 3 :accessor nbunit1 :initarg :nbunit1)
   (unit1 :initform 512 :accessor unit1 :initarg :unit1)
   (nbunit2 :initform 3 :accessor nbunit2 :initarg :nbunit2)
   (unit2 :initform 512 :accessor unit2 :initarg :unit2)
   (level :initform 0 :accessor level :initarg :level)
   (first-event :initform nil :accessor first-event )
   (event-count :initform 0 :accessor event-count)
   
))


;;; ====================================================================
;;;     Globals
;;; ====================================================================

;;; Keys

(defvar *G-key* 0)
(defvar *C-3-key* 1)
(defvar *C-4-key* 2)
(defvar *F-key* 3)

;;; note reference for rest positioning

(defvar *rest-reference* #(b4 c4 a3 d3))

;;; Temperaments

(defvar *half-tone* 2)
(defvar *quarter-tone* 4)
(defvar *eight-tone* 8)


;;; ====================================================================
;;;     Macros and utilities
;;; ====================================================================

;;; extend a list towards right

(defmacro pushr (item place)
  `(if ,place
     (nconc  ,place (list ,item))
     (setf ,place (list ,item))))


;;; convert the ... 4, 8, 16 ... convention for duration  to Enigma
;;; covention ...1024, 512, 256...


(defmacro enigma-duration (denum)
  `(/ (* 1024 4) ,denum))



;;; pitch unit conversions

(defvar *temperament* 2)

(defvar *diatonic-table*  #(0 0 1 1 2 3 3 4 4 5 5 6) )
(defvar *accidental-table* #(0 1 0 1 0 0 1 0 1 0 1 0) )

(defun chromatic-to-diatonic (d)
  (values (elt *diatonic-table* (mod d 12))
          (elt *accidental-table* (mod d 12))) )



;changed for Finale 98-Enigma compatibility (+ #x10000 eni)


(defun midics-to-enigma (midics)
  (let* (m1 m2 octave eni (midics (om::approx-m midics *temperament* )))
    (setf m1 (truncate midics 100)
          m2 (mod m1 12)
          octave (- (truncate m1 12) 5)
          eni (+ (* (+ (* octave 7) (elt *diatonic-table* m2)) #x0010)
                 (truncate (- midics
                              (* (- m1 (elt *accidental-table* m2))
                                 100))
                           (truncate 200 *temperament*))))
    (if (< eni 0) (+ #x0 eni) eni)))



;changed for Finale 98-Enigma compatibility (+ #x10000 deg)

(defun symbol-to-enigma (symb)
  (let ((symb (string-upcase (symbol-name symb)) ) deg oct (acc 0) (index 0))
    (setf deg (elt symb index)
          oct (elt symb (incf index)))
    (setf deg (mod (- (char-code deg) (char-code #\C)) 7)
          oct (* 7 (- (- (char-code oct) (char-code #\0)) 4)))
    (when (> (length symb) 2)
      (case (elt symb (incf index))
        (#\S (setf acc (/ *temperament* 2) ))
        (#\B (setf acc (- (/ *temperament* 2))))
        (t (decf index))))
    (do ((i (incf index) (1+ i))) ( (>= i (length symb)) )
      (case (elt symb i)
        (#\+ (incf acc))
        (#\- (decf acc)) ))
    (when (< acc 0) (setf acc (+ 8 (abs acc))))
    (setf deg (+ (* 16 deg) (* 16 oct) acc))
    (if (< deg 0) (+ #x0 deg) deg) ))

;;; ====================================================================
;;;      methods for Score
;;; ====================================================================


(defmethod initialize-instance :after ((self Score) &key controls)
  (declare (ignore controls))
  (setf  (staff-system self)
         (make-instance 'StaffSystem :parent-score self))
  (setf  (measure-system self)
         (make-instance 'MeasureSystem :parent-score self))
  (setf  (voice-system self)
         (make-instance 'VoiceSystem :parent-score self))
  (setf  (event-system self)
         (make-instance 'EVentSystem :parent-score self))
  )


(defmethod set-temperament ((self Score) temperament)
  (setf (slot-value self 'temperament) temperament)
  )

(defmethod push-staff ((self Score) &rest more)
  (apply 'push-staff (staff-system self) more))

(defmethod push-measure ((self Score) &rest more)
  (apply 'push-measure (measure-system self) more))

(defmethod push-voice ((self Score) &rest more)
  (apply 'push-voice (voice-system self) more))

(defmethod push-event ((self Score) &rest more)
  (apply 'push-event (event-system self) more))

(defmethod number-objects ((self Score))
    (number-objects (event-system self)))
    
(defmethod rewind-measure ((self Score))
  (setf (current-measure self)
        (first (measure-list (measure-system self)))))

(defmethod go-next-measure ((self Score))
  (setf (current-measure self)
        (second (member (current-measure self)
                     (measure-list (measure-system self))))))

(defmethod rewind-voice ((self Score))
  (setf (current-voice self)
        (first (voice-list (voice-system self)))))

(defmethod go-next-voice ((self Score))
  (setf (current-voice self)
        (second (member (current-voice self)
                     (voice-list (voice-system self))))))

(defmethod select-staff ((self Score) index)
  (let ( staff
         (staff-list (staff-list (staff-system self))) )
    (setf staff (nth (1- index) staff-list))
    (unless staff
      (dotimes  (i (- index (length staff-list)))
        (setf staff (push-staff self))))
    (setf (current-voice self) (voice staff))
    staff))

   
(defmethod select-measure ((self Score) index)
  (let ( measure
         (measure-list (measure-list (measure-system self))) )
    (setf measure (nth (1- index) measure-list))
    (unless measure
      (dotimes  (i (- index (length measure-list)))
        (setf measure (push-measure self)) ))
    (setf (current-measure self) measure)
    measure))


(defmethod open-tuplet ((self Score) nbunit1 unit1 nbunit2 unit2)
  (incf (tuplet-level self))
  (setf (tuplet-max-level self) (max (tuplet-level self) (tuplet-max-level self)))
  (push  
   (make-instance 'tuplet :nbunit1 nbunit1 :unit1 unit1 
                  :nbunit2 nbunit2 :unit2 unit2 
                  :level (tuplet-level self))
   (current-tuplet self))
  )

(defmethod close-tuplet ((self Score))
  (decf (tuplet-level self))
  (when (zerop (tuplet-level self))
    (loop for tuplet in (current-tuplet self)
          do (setf (level tuplet) (- (tuplet-max-level self) (level tuplet))))
    (setf (tuplet-max-level self) 0)
    (setf (tuplet-list self) 
          (append (current-tuplet self) (tuplet-list self)))
    (setf (current-tuplet self) nil)))


(defmethod get-map ((self Score))
  (let ((event-list (event-list (event-system self))) (measure-nb 0) map event1 event2)
    (do* ( (voice-list (voice-list (voice-system self)))
           (voice (pop voice-list) (pop voice-list))
           (nv 1 (1+ nv)) )
         ((null voice) (reverse map))
      (do* ( (measure-list (measure-list (measure-system self)) )
             (measure (pop measure-list) (pop measure-list))
             (nm 1 (1+ nm)) )
           ((null measure)) 
        (when event-list
          (setf event1 (car event-list))
          (when (and (eq (event-voice event1) voice)
                     (eq (event-measure event1) measure))
            (do () ((or (null event-list)
                        (not (eq  (event-measure (car event-list)) measure))
                        (not (eq  (event-voice (car event-list)) voice))))
              (setf event2 (pop event-list)))
            (push  (list nv nm (incf measure-nb) (rank-left event1) (rank-left event2))
                   map) ))  ))))






(defmethod put-map-to-enigma ((self score))
  (let ((map (get-map self)) 
        (staff-list (staff-list (staff-system self)))
        (measure-list (measure-list (measure-system self))))
    (format t "~%~%^others~%~%")
    (dolist (map-elt map)
      (format t "^FR(~D) ~D ~D 0 0~%"
              (third map-elt)  (fourth map-elt) (fifth map-elt)
              )) ;(width (elt measure-list (1- (second map-elt)))) removed
    (format t "~%~%^details~%~%")
    (dolist (map-elt map)
      (format t "^GF(~D,~D) ~D 0 ~D 0 0 0~%^GF(~D,~D) 0 0 0 0 0 0~%"
              
              (first map-elt)  (second map-elt)
              (start-key (elt staff-list (1- (first map-elt)))) 
              (third map-elt)
              (first map-elt)  (second map-elt) ;--->This must be the measure number!!!
              ))

    (loop for tuplet in (tuplet-list self)
          for event = (first-event tuplet)
          
         ;;; le niveau du tuplet se trouve dans (level tuplet) 
         ;;; 0,1,2, ... croissant du bas vers le haut
         
          do
          (format t "^TN(0,~D) 24 0 0 0 1~%"
                  (rank event))
          (format t "^TP(0,~D) ~D ~D ~D ~D ~D ~%"
                  (rank event)
                  (nbunit1 tuplet)
                  (unit1 tuplet)
                  (nbunit2 tuplet)
                  (unit2 tuplet)
                  0)
          (format t "^TP(0,~D) 0 ~D 0 14 ~D ~%"
                  (rank event)
                  ( * (+ (level tuplet) 1) 35)
                  (if (om::powerof2? (nbunit2 tuplet)) 9873 9874));--->tuplet pref dialogue/(9873 is for number - 9874 for ratio)
          (format t "^TP(0,~D) -13 0 -13 0 0 ~%"
                  (rank event));----->tuplet pref dialogue
          )   
    t))




(defmethod translate-to-enigma ((self Score))
  (setf *temperament* (slot-value self 'temperament))
  (number-objects self)
  (put-map-to-enigma self)
  (format t "~%~%^others~%~%")
  (translate-to-enigma (staff-system self))
  (translate-to-enigma  (measure-system self))
  (format t "~%~%^entries~%~%")
  (translate-to-enigma (voice-system self))
  (translate-to-enigma (event-system self))
  t)



;;; ====================================================================
;;;      methods for VoiceSystem
;;; ====================================================================

(defmethod push-voice ((self VoiceSystem) &rest more)
  (declare (ignore more))
  (let ((voice (make-instance 'voice ':parent-system self)))
    (setf (current-voice (parent-score self)) voice)
    (pushr voice (voice-list self))
    voice))


(defmethod translate-to-enigma ((self VoiceSystem))
)


;;; ====================================================================
;;;      methods for Voice
;;; ====================================================================


(defmethod translate-to-enigma  ((self Voice))
)


(defmethod parent-score ((self Voice)) (parent-score (parent-system self)))



(defmethod number-voice ((self voice))
  (setf (voice-number self) (position self (voice-list (parent-system self)) :test 'equal)))



(defmethod index-voices ((self score))
  (mapcar #'(lambda (x) (number-voice x))
(voice-list (voice-system self))))



;;; ====================================================================
;;;      methods for EventSystem
;;; ====================================================================

(defmethod push-event ((self EventSystem) &key (duration 1024) 
                       (type 'note) pitch 
                       (follow nil) 
                       (b-tie nil)
                       (c-tie nil)
                       (e-tie nil))
  (let ( (event (make-instance type :event-duration duration :follow follow)) )
    (setf *temperament* (slot-value (parent-score self) 'temperament))
    ;(when (and  (typep event 'Crest) (not pitch))
    ;  (setf (pitch event) (elt *rest-reference* (start-key (parent-score self)))))
    (when (and (or (typep event 'note) (typep event 'Crest)) pitch)
      (setf (pitch event)
            (if (numberp pitch)
              (midics-to-enigma pitch)
              (symbol-to-enigma pitch))))
    (when (and (typep event 'chord) pitch)
      (setf (note-list event)
            (mapcar
             #'(lambda (xpitch index)
                 (make-instance 'note 
                   :pitch (if (numberp xpitch)
                            (midics-to-enigma xpitch)
                            (symbol-to-enigma xpitch))
                   :index-in-chord index
                   :n-notes (length pitch)
                   :event-duration duration
                   :follow follow
                   ))
             pitch 
             (do 
               ((n 1 (1+ n))
                (lst '() (push n lst)))
               ((= n (+ 1 (length pitch))) (reverse lst))))))
    (when b-tie
      (if (typep event 'note)
        (setf (begin-tie event) b-tie)
        (mapc #'(lambda (note tie-val) (setf (begin-tie note) tie-val))
              (note-list event)
              b-tie)))
    (when c-tie
      (if (typep event 'note)
        (setf (begin-tie event) c-tie)
        (mapc #'(lambda (note tie-val) (setf (begin-tie note) tie-val))
              (note-list event)
              c-tie)))
    (when e-tie
      (if (typep event 'note)
        (setf (begin-tie event) e-tie)
        (mapc #'(lambda (note tie-val) (setf (begin-tie note) tie-val))
              (note-list event)
              e-tie)))
    (setf (current-event (parent-score self)) event)
    (pushr event (event-list  self))
    (setf (event-voice event) (current-voice (parent-score self))
          (event-measure event) (current-measure (parent-score self)))
    (loop for tuplet in (current-tuplet (parent-score self))
          while (null (first-event tuplet))
          do 
          (setf (first-in-tuplet event) t
                (first-event tuplet) event ))
    
     (when (typep event 'chord)
       (mapc #'(lambda (note)
                 (setf (first-in-tuplet note) (first-in-tuplet event)))
             (note-list event)))
     t))

(defmethod number-objects ((self EventSystem))
  (let ((i 0) (count 0))
    (dolist (event (event-list self))
      (case (class-name (class-of event))
        ((note Crest)
         (setf (rank event) (incf i))
         (incf count))
        (chord 
         (setf (rank event) (1+ i))
         (dolist (note (note-list event))
           (setf (rank note) (incf i))
           (incf count) ))))
    (setf (event-count self) count)))

(defmethod translate-to-enigma  ((self EventSystem))
  (let ((prev 0) next (cprev 0) cnext)
    (mapl
     #'(lambda (levent)
         (setf next (if (cdr levent) (rank (cadr levent)) 0)  
               cprev 0 cnext 0)
         (if (not (typep (car levent) 'chord))
           (translate-event-to-enigma (car levent) prev next cprev cnext)
           (mapl 
            #'(lambda (lnote) 
                (setf cnext (if (cdr lnote) (rank (cadr lnote)) 0))  
                (translate-event-to-enigma (car lnote) prev next cprev cnext )
                (setf prev 0 next 0 cprev (rank (car lnote))) )
            (note-list (car levent)))) ;---> Ici le prob commence: pour les chords. C'est la classe pitch qui n'aime pas hŽriter des chords
         (setf prev (rank (car levent))))
     (event-list self)) ))



;;; ====================================================================
;;;      methods for Staff
;;; ====================================================================


(defmethod parent-score ((self Staff)) (parent-score (parent-system self)))

(defmethod translate-staff-to-enigma ((self Staff) &key (rank 1))
  (let ((biggest-measure
         (biggest-measure (measure-system (parent-score (parent-system self))))))
    (format t "^PL(~D) 0 0 0 0 0 0~%" rank)
    (format t "^PL(~D) 20565 0 0 0 0 0~%" rank)
    (format t "^IS(~D) 0 0 0 0 0 0~%" rank)           
    (format t "^IS(~D) ~D 0 5 0 0 0~%" rank (start-key self))
    (format t "^IS(~D) -772 -772 -4 0 0 0~%" rank)))


;;; ====================================================================
;;;      methods for Measure
;;; ====================================================================

(defmethod parent-score ((self Measure)) (parent-score (parent-system self)))

(defmethod number-measure ((self measure))
  (setf (measure-number self) (position self (measure-list (parent-system self)) :test 'equal)))

(defmethod index-measures ((self score))
  (mapcar #'(lambda (x) (number-measure x))
(measure-list (measure-system self))))



(defmethod translate-measure-to-enigma ((self Measure) &key (rank 1))
  (format t
          "^MS(~D) ~D ~D ~D ~D 6 16~%"
          rank
          (width self)
          (cond 
           ((= (slot-value (parent-score self) 'temperament) 2) 0)
           ((= (slot-value (parent-score self) 'temperament) 4) 768)
           ((= (slot-value (parent-score self) 'temperament) 8) 1024)
           (t 0))  
          (num self)
          (enigma-duration (denum self))) 
  (format t "^MS(~D) 0 0 240 0 0 0~%" rank))



;;; ====================================================================
;;;      methods for StaffSystem
;;; ====================================================================


(defmethod push-staff ((self StaffSystem) &key (start-key 0))
  (let ((staff (make-instance 'Staff
                              :parent-system self 
                              :start-key start-key
                              :voice (push-voice (parent-score self)) ) ))
    (pushr staff  (staff-list self))
    staff))




(defmethod translate-to-enigma ((self StaffSystem))
  (let ((i 0))
    (format t "^IP(65534) ~D 0 0 0 0 0~%" (length (staff-list self)))
    (do ((n 0 (1+ n))
     (i 1 (1+ i))
     (j -176 (+ j -360)))
     ((= n (length (staff-list self))) t)
     (format t "^Iu(0) ~D 0 0 0 ~D ~%" i j);i fo staff number, j for space between staves
     (format t "^Iu(0) 1 0 32767 2147483647 ~%"))

    (dolist (staff (staff-list self))
      (translate-staff-to-enigma staff :rank (incf i))) ))


;;; ====================================================================
;;;      methods for MeasureSystem
;;; ====================================================================

(defmethod push-measure ((self MeasureSystem) &key (num 4) (denum 4))
  (let ((measure (make-instance 'Measure
                                ':parent-system self 
                                ':num num
                                ':denum denum)))
    (setf (current-measure (parent-score self))
          measure)
    (pushr measure (measure-list self))
    (setf (slot-value self 'biggest-measure) nil)
    measure))

(defmethod biggest-measure ((self MeasureSystem))
  (or (slot-value self 'biggest-measure)
      (let (length the-measure (the-length 0))
        (dolist (a-measure (measure-list self))
          (setf length (/ (num a-measure) (denum a-measure)))
          (when (or (> length the-length)
                    (and (= length the-Length)
                         (> (denum a-measure) (denum the-measure))))
            (setf the-measure a-measure
                  the-length length)) )
        (setf (slot-value self 'biggest-measure) the-measure))))

(defmethod translate-to-enigma ((self MeasureSystem))
  (let ((i 0))
    (dolist (measure (measure-list self))
      (translate-measure-to-enigma measure :rank (incf i) ))) )



;;; ====================================================================
;;;      methods for ScoreEvent
;;; ====================================================================



(defmethod translate-event-to-enigma ((self ScoreEvent) prev next cprev cnext)
  (let ((pitch (pitch self))
        (index-in-chord (index-in-chord self))
        (n-notes (n-notes self))
        (acc 0))
    (if ( > index-in-chord 1) 
      
      (progn 
        
        (format t "   ~D "
                pitch)
        
        (format t "$~A~A0~A0000~%"
                (if (and (not (typep self 'Crest)) (and (typep self 'note) (begin-tie self))) 
                  (cond 
                   ((equal (slot-value self 'begin-tie) 'b )"C")
                   ((equal (slot-value self 'begin-tie) 'c ) "E")
                   (t "A"))
                  8)
                (if (zerop (mod pitch 16)) 0 1)
                index-in-chord)  ;---------->(nth note in chord)   
        )
      
      
      (progn
        
        
        (format t "^eE(~D) ~D ~D ~D ~D "
                (rank self)
                prev next 
                (event-duration self)
                0)
        
        (format t "$~A~A~A~A~A~A~A~A "
                (if (typep self 'note) "C" 8)
                (if (typep self 'Crest) 1 1)
                0 
                (if (first-in-tuplet self) 8 0)
                0 
                (if (follow self) 0 8)
                0 0)
        
        (format t " ~D ~D ~%   "
                (if (typep self 'tuplet) 8 0) ;---->doesn't work and maybe unecesserary
                n-notes);----->(number-of-notes-inchord)
        
        
        (format t "~D "
                pitch)
        
        
        
        
        (format t "$~A~A0~A0000~%"
                (if
                 (and (typep self 'note) (begin-tie self)) 
                  (cond 
                   ((equal (slot-value self 'begin-tie) 'b )"C")
                   ((equal (slot-value self 'begin-tie) 'c ) "E")
                   ((equal (slot-value self 'begin-tie) 'e ) "A")
                   (t (begin-tie self)))
                 8)
                (if (zerop (mod pitch 16)) 0 1)
                index-in-chord)  ;---------->(nth note in chord)   
        
        ))))




(defmethod rank-left ((self ScoreEvent)) (rank self))
(defmethod rank-right ((self ScoreEvent)) (rank self))



;;; ====================================================================
;;;      methods for Notes, Crest & Chord
;;; ====================================================================

;get all events as a list 
(defmethod get-event-num ((self score))
  (let* ((event-syst (event-system self))
        (events (event-list event-syst)))
    events))

;------------------------note-number-----------------------------
;write the note entry in the note-number slot
;all voices 

(defmethod number-notes ((self note))
  (setf (note-number self) 
        (position self 
                  (event-list 
                   (event-system 
                    (parent-score 
                     (parent-system 
                      (event-voice self))))) :test 'equal)))


(defmethod number-notes ((self crest))
  (setf (note-number self) 
        (position self 
                  (event-list
                   (event-system 
                    (parent-score 
                     (parent-system 
                      (event-voice self))))) :test 'equal)))



(defmethod number-notes ((self chord))
  (progn
    (setf (note-number self) 
          (position self (event-list (event-system (parent-score (parent-system (event-voice self))))) :test 'equal))
    (mapcar #'(lambda (x) (setf (note-number x) 
                                (position self 
                                          (event-list 
                                           (event-system 
                                            (parent-score 
                                             (parent-system
                                              (event-voice self))))) :test 'equal)))
            
            (note-list self))))


(defmethod index-notes ((self score))
  (mapcar #'(lambda (x) (number-notes x))
(get-event-num self)))




;--------------------voice-num-------------------------

(defmethod get-event-voice ((self note))
(setf (voice-num self) (voice-number (event-voice self))))

(defmethod get-event-voice ((self crest))
(setf (voice-num self) (voice-number (event-voice self))))

(defmethod get-event-voice ((self chord))
  (progn 
    (mapcar #'(lambda (x)  (setf (voice-num x) (voice-number (event-voice self))))
(note-list self));sets also voice-num of notes inside chords
  (setf (voice-num self) (voice-number (event-voice self)))))


(defmethod index-note-voices ((self score))
  (mapcar #'(lambda (x) (get-event-voice x))
(get-event-num self)))


;--------------------measure-num--------------------------
;this may be practical for spacing,But...
;The problem is when you get multiple voices,
;doing spacing there is problematic.
;---------------------------------------------------------



(defmethod measure-index ((self note))
  (setf (measure-num self)
       (measure-number (event-measure self))))

(defmethod measure-index ((self crest))
  (setf (measure-num self)
       (measure-number (event-measure self))))

(defmethod measure-index ((self chord))
  (progn
  (setf (measure-num self)
       (measure-number (event-measure self)))
  (mapcar #'(lambda (x) (setf (measure-num x) (measure-number (event-measure self))))
          (note-list self))))
          


(defmethod index-note-measures ((self score))
  (mapcar #'(lambda (x) (measure-index x))
(get-event-num self)))



(defmethod rank-right ((self Chord))
  (rank (first (last (note-list self)))))



;;; ====================================================================
;;;     User/ Programmer Interface
;;; ====================================================================


;;; redirect terminal io stream

(defmacro with-output-to-file ((filename &key if-exists) &body forms)
  (let ((fvar (gensym)))
    `(with-open-file (,fvar ,filename
                            :direction :output :if-exists ,(or if-exists :overwrite)
                            :if-does-not-exist :create)
       (let ((*standard-output* ,fvar) (*load-verbose* nil) )
         ,. forms ))))

(defun cleni-error (format &rest args)
  (format *error-output* "Common Lisp to Enigma Interface. Error.~%")
  (apply 'error format args))

(defun check-pop-token (tok-list type want-err)
  (let ((errcount nil))
    (cond
       ((null tok-list)
        (when want-err (cleni-error "End of description reached")))
       (t
        (cond
         ((consp type)
          (unless (some #'(lambda (single-type) (check-pop-token tok-list single-type nil))
                        type)
            (setf errcount t)))
         (t
          (if (keywordp type)
            (unless  (eq type (car tok-list))
              (setf errcount t))
            (unless  (and (subtypep (type-of (car tok-list)) type)
                          (not (and (eq type 'symbol) (keywordp (car tok-list)))))
              (setf errcount t)))))
        (or (not errcount)
            (and want-err
                 (cleni-error "Syntax error.A token of type ~S was expected.~%~S"
                              type (subseq tok-list 0 )))))) ))
                    
(defmacro pop-token (tok-list type err)
  `(when (check-pop-token ,tok-list ,type ,err) 
         (pop ,tok-list)))



(defun new-score (&key temperament)
  (let ((score (make-instance 'score)))
    (when temperament  (set-temperament score temperament))
    score))





(defun describe-score (score event-list) 
  (let (token index staff measure dur pitch token2 follow tie
              ;;; GA 170996 because of unconsistencies in MCL 3.9 type system
              (number-type (list (type-of 1) (type-of 1/4))))
    (loop
      (unless (setf token  (pop event-list)) (return))
      (unless (keywordp token)
        (cleni-error "Syntax error. Keyword was expected.~%~S"
                     (cons token (subseq event-list 0 10)) ))
      (case token
        (:temperament (set-temperament score (pop-token event-list number-type t)))
        
        (:staff 
         (setf index (pop-token event-list number-type t)
               staff (select-staff score index))
         (when (setf token (pop-token event-list :key nil))
           (setf (start-key staff)
                 (pop-token event-list number-type t))))
        (:measure 
         (setf index  (pop-token event-list number-type t)
               measure (select-measure score  index))
         (when (setf token (pop-token event-list :signature nil))
           (setf token (pop-token event-list 'list t)
                 (num measure) (car token)
                 (denum measure) (cadr token))))
        
        (:open-tuplet
         (open-tuplet score
                      (pop-token event-list number-type t)
                      (truncate (* (pop-token event-list number-type t) 4096))
                      (pop-token event-list number-type t)
                      (truncate (* (pop-token event-list number-type t) 4096))))
        
        
        (:close-tuplet
         (close-tuplet score))
        ((:note :crest :chord)
         (setf dur (pop-token event-list number-type t))
         (setf pitch nil follow nil tie nil)
         (case token
           (:Crest
            (setf pitch (pop-token event-list (cons 'symbol number-type) nil)))
           (:note
            (setf pitch (pop-token event-list (cons 'symbol number-type) t)))
           (:chord
            (setf pitch (pop-token event-list 'list t))))           
         (setf token2 nil)      
         (loop      
           (setf token2 (or (or (or (pop-token event-list :follow nil)
                                   (pop-token event-list :b-tie nil))
                               (pop-token event-list :c-tie nil))
                           (pop-token event-list :e-tie nil)))
           (unless token2 (return))
           (case token2
             (:b-tie
              (setf tie "C")
              (when (eq token :chord)
                (setf tie (pop-token event-list 'list t))))
             (:c-tie
              (setf tie "E")
              (when (eq token :chord)
                (setf tie (pop-token event-list 'list t))))
             (:e-tie
              (setf tie "A")
              (when (eq token :chord)
                (setf tie (pop-token event-list 'list t))))
             (:follow  (setf follow t))))
         (push-event score
                     :type (intern (symbol-name token) 'cleni)
                     :duration (truncate (* dur 4096))
                     :pitch pitch
                     :follow follow
                     :b-tie tie
                     :c-tie tie 
                     :e-tie tie ))
        (t (cleni-error "Syntax Error. Unknown token~%~S"
                        (cons token (subseq event-list 0 10)) ))) )
    (setf (tuplet-list score) (nreverse (tuplet-list score)))
    (index-voices score)
    (index-measures score)
    (index-notes score)
    (index-note-voices score)
    (index-note-measures score)
    t)
 )
                       


(in-package "CLENI")
;;; a faire bien...
(defun translate-score (score filename)
  (let ((template-file (om::omroot "resources/template.etf"))
        (target-file filename)   ;;;   (full-pathname (merge-pathnames filename ".etf")))
        )
    ;;;(om::my-copy-file template-file target-file)
    (oa::om-copy-file template-file target-file)
    (with-output-to-file (target-file :if-exists :append)
      (translate-to-enigma score)
      )
    ;;;(set-mac-file-type target-file :ETF3)
    ;;;(set-mac-file-creator target-file :FIN3)
    T))

;;(full-pathname "CLENI:template~D.etf")
;;; ====================================================================
;;;     Provide CLENI
;;; ====================================================================

(provide 'cleni)



;******************************************************************************



;************************************************
;
;                   OM2FINALE
;
;************************************************

(defvar *my-score* nil)

(defmethod cons-cleni-expr ((self om::poly) index &optional (approx 2))
  (let ((voices (om::inside self)))
    `(describe-score *my-score* 
                    ',(append (list   :temperament approx  )
                               (loop for staff in voices
                                     for i = 1 then (+ i 1)
                                     append  (cons-cleni-expr staff i approx))))))




(defmethod cons-cleni-expr ((self om::voice) index &optional (approx 2))
  (let ((mesures (om::inside self)))
    `(:staff ,index
             ,.(loop for mes in mesures
                     for i = 1 then (+ i 1)
                     append (cons-cleni-expr mes i approx)))))


;symb-beat-val= For a key signature equivalent to 3//3 will be the half note (blanche)
;real-beat-val= For the same key sign, this will be the halfnote of a triplet (blanche de triolet)
;These refer to the beats in a measure, and for special cases using non-standard key signature



(defmethod cons-cleni-expr ((self om::measure) index &optional (approx 2))
  (let* ((tree (om::tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first
                                                                  tree)))))
         (inside (om::inside self)))
    `(:measure ,index :signature , (list (om::fnumerator (first tree))
                                         (om::fdenominator (first tree)))
               ,.(loop for obj in inside
                       append (let* ((dur-obj-noire (/ (om::extent obj)
                                                       (om::qvalue obj)))
                                     (factor (/ (* 1/4 dur-obj-noire)
                                                real-beat-val)))
                                (cons-cleni-expr obj (* symb-beat-val
                                                        factor)  approx))))))







;This will make correct :follow 



(defmethod cons-cleni-expr ((self om::chord) durtot &optional (approx 2)) ;--->ici c'est entre autre pour les ties!
  (let ((inside (om::inside self)))
    (if (and (or (om::measure-p (om::parent self))(= (om::offset self) 0)) (not (om::group-p (om::parent (om::parent self)))))
    `(:chord ,durtot
            ,(loop for note in inside
                    collect (om::midic note)) 
            ,.(cond 
               
               
               ((and (not (om::cont-chord-p self))
                     (om::cont-chord-p (om::next-container self '(om::chord))))
                (list :b-tie (make-list (length inside) :initial-element 'b)))
               
               
               ((and (om::cont-chord-p self)
                     (om::cont-chord-p (om::next-container self '(om::chord))))
                (list :c-tie (make-list (length inside) :initial-element 'c)))
               
               
               ((and (om::cont-chord-p self)
                     (not (om::cont-chord-p (om::next-container self '(om::chord)))))
                (list :e-tie (make-list (length inside) :initial-element 'e)))
               
               ))
    `(:chord ,durtot
            ,(loop for note in inside
                    collect (om::midic note)) 
            ,.(cond 
               
               
               ((and (not (om::cont-chord-p self))
                     (om::cont-chord-p (om::next-container self '(om::chord))))
                (list :b-tie (make-list (length inside) :initial-element 'b)))
               
               
               ((and (om::cont-chord-p self)
                     (om::cont-chord-p (om::next-container self '(om::chord))))
                (list :c-tie (make-list (length inside) :initial-element 'c)))
               
               
               ((and (om::cont-chord-p self)
                     (not (om::cont-chord-p (om::next-container self '(om::chord)))))
                (list :e-tie (make-list (length inside) :initial-element 'e)))
               
               ) :follow))))

(defmethod cons-cleni-expr ((self om::rest) durtot &optional (approx 2))
  (if (and (om::group-p (om::parent self)) (not (= (om::offset self) 0))) 
     `(:crest ,durtot , :follow)
      `(:crest ,durtot )))



#|
(defmethod cons-cleni-expr ((self om::group) durtot &optional (approx 2))
  (let* ((num (if (om::etf-tuplet-p self) (om::my-tuplet-p self) (om::extent self))) ;A VERIFIER extent for groups that are not tuplets
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (second denom) denom))
         (unite (/ durtot denom))
         (inside (om::inside self))
         (sympli (/ num denom)))
    (cond
     ((not (om::etf-tuplet-p self)) 
      `(,.(loop for obj in inside
                append (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                                          (/ (om::extent self) (om::qvalue self)))))
                         (cons-cleni-expr obj (* dur-obj durtot)  approx)))))
     ((= sympli 1)
      `(,.(loop for obj in inside
                append (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                            (/ (om::extent self) (om::qvalue self))))
                              (dur-obj (numerator (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                     (/ (om::extent self) (om::qvalue self))))))
                         (setf dur-obj (* dur-obj (/ num (denominator operation))))
                         (cons-cleni-expr obj (* dur-obj unite)  approx)))))
     
     (t
      
      `(:open-tuplet ,num ,unite ,denom ,unite
                     ,.(loop for obj in inside
                             append (let* (
                                           (operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                         (/ (om::extent self) (om::qvalue self))))
                                           (dur-obj (numerator operation)))
                                      
                                      (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                      (cons-cleni-expr obj (* dur-obj unite)  approx))) :close-tuplet)))))
|#

;corrects annoying bugg in pseudo ternaire group!! 25/02/02
(defmethod cons-cleni-expr ((self om::group) durtot &optional (approx 2))
  (let* (
         ;(num (if (om::etf-tuplet-p self) (om::my-tuplet-p self) (om::extent self)));actual one
         (num (or (om::get-group-ratio self)  (om::extent self)))
         ;(num (if (om::get-group-ratio self) (om::my-tuplet-p self) (om::extent self))) ;A VERIFIER extent for groups that are not tuplets
         (denom (om::find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (second denom) denom))
         (unite (/ durtot denom))
         (inside (om::inside self))
         (sympli (/ num denom)))
    (cond
     ((not (om::get-group-ratio self)) 
      `(,.(loop for obj in inside
                append (let* ((dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) 
                                          (/ (om::extent self) (om::qvalue self)))))
                        (cons-cleni-expr obj (* dur-obj durtot)  approx)))))
     ((= sympli 1)
      `(,.(loop for obj in inside
                append (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                         (/ (om::extent self) (om::qvalue self))))
                              (dur-obj (numerator (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                     (/ (om::extent self) (om::qvalue self))))))
                         (setf dur-obj (* dur-obj (/ num (denominator operation))))
                         (cons-cleni-expr obj (* dur-obj unite)  approx)))))
     
     (t
      
      `(:open-tuplet ,num ,unite ,denom ,unite
                     ,.(loop for obj in inside
                             append (let* (
                                           (operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                         (/ (om::extent self) (om::qvalue self))))
                                           (dur-obj (numerator operation)))
                                      
                                      (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                      (cons-cleni-expr obj (* dur-obj unite)  approx))) :close-tuplet)))))



(in-package :om)


(defmethod etf-export ((self t) &key approx path name)
  (om-beep-msg "Only for voice or poly objects"))

(defmethod etf-export ((self voice) &key (approx 2) path name)
  (let ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                      :name name
                                                      :prompt "New ETF file"  :types '("ETF Files" "*.etf")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (setf cleni::*my-score* (cleni::new-score))
      (eval (cleni::cons-cleni-expr (make-instance 'poly :voices self) 0 approx))
      (cleni::translate-score cleni::*my-score* pathname)
      pathname)))

(defmethod etf-export ((self poly) &key (approx 2) path name)
  (let ((pathname (or path 
                      (om-choose-new-file-dialog :directory (def-save-directory) 
                                                 :name name 
                                                 :prompt "New ETF file"  :types '("ETF Files" "*.etf")))))
     (when pathname 
       (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
       (setf cleni::*my-score* (cleni::new-score))
       (eval (cleni::cons-cleni-expr self 0 approx))
       (cleni::translate-score cleni::*my-score* pathname))
     pathname))

(defmethod etf-export ((self t) &key approx path name)
  (om-beep-msg "Only for voice or poly objects"))



(defmethod! save-as-etf ((object t) &optional (approx 2) path)
   :initvals '(nil) 
   :indoc '("a VOICE or POLY object") 
   :icon 350
   :doc "
Saves <object> as an ETF (Enigma Transportable Format by MakeMusic) file.

The target pathnamewill be asked through a file choose dialog.
- <approx> specifies the tone division (supported: 2 or 4).
- <path> is a pathname for output
"
   (etf-export object :approx approx :path path))


