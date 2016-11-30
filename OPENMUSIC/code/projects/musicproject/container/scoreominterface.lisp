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
;==========================================================================
; OpenMusic : ScoreOMInterface.lisp
;==========================================================================


;--------------------
;  DURATION
;--------------------

(defmethod! object-dur ((self simple-container))
  :initvals '(nil)
  :indoc '("a musical object")
  :outdoc '("duration (ms)") 
  :icon 230
  :doc "Returns the total duration of a musical object (ms)"
  (get-obj-dur self))


;--------------------
;  CONCAT
;--------------------

(defmethod* concat ((s1 sequence*) (s2 sequence*))
  :initvals '(nil nil) 
  :indoc '("a musical sequence" "a musical sequence")
  :icon 230
  :doc "Concatenates two music sequences into a new one.

Type of the return value :
 voice x voice   => Voice
 measure x voice => Voice
 voice x measure => Voice
 multi-seq x multi-seq => multi-seq
 chord-seq x chord-seq => chord-seq
 poly x poly => poly
" 
  (let ((frac-min (fraction-minimale-commune s1 s2))
        (ss1 (duplique-structure-musicale s1))
        (ss2 (duplique-structure-musicale s2)))
    
    (change-qvalue ss1 frac-min)
    (change-qvalue ss2 frac-min)

    (loop for item in (inside ss2) do (setf (offset item) (+ (offset item) (extent ss1))))
    (mki (type-of s1) 
             :empty t
             :offset 0
             :extent (+ (extent ss1) (extent ss2))
             :qvalue frac-min
             :inside (append (inside ss1) (inside ss2)))))

(defmethod* concat ((s1 simple-container) (s2 null)) s1)
(defmethod* concat ((s1 null) (s2 simple-container)) s2)

(defun equal-tempi (t1 t2)
  (and (= (car t1) (car t2))
       (= (second t1) (second t2))
       (equal (third t1) (third t2))))

(defun concat-tempi (voice1 voice2)
  (let* ((tempo1 (tempo voice1))
        (tempo2 (tempo voice2))
        (newtempo (copy-list tempo1))
        lasttempo)
    (if (second tempo1)
        (setf lasttempo (second (car (last (second tempo1)))))
      (setf lasttempo (car tempo1)))
    (unless (equal-tempi (car tempo2) lasttempo)
      (setf (nth 1 newtempo) (append (nth 1 newtempo) (list (list (list (length (inside voice1)) 0) (car tempo2))))))
    (loop for item in (second tempo2) do
          (setf (nth 1 newtempo) (append (nth 1 newtempo) 
                                         (list (list (list (+ (length (inside voice1))
                                                              (caar item)) (second (car item))) (second item))))))
    newtempo))
      
(defmethod* concat ((s1 voice) (s2 voice))
 (let ((frac-min (fraction-minimale-commune s1 s2))
        (ss1 (duplique-structure-musicale s1))
        (ss2 (duplique-structure-musicale s2))
        rep)
    (change-qvalue ss1 frac-min)
    (change-qvalue ss2 frac-min)
    (loop for item in (inside ss2) do (setf (offset item) (+ (offset item) (extent ss1)))) 
    (setf rep
          (mki 'voice 
             :empty t
             :offset 0
             :extent (+ (extent ss1) (extent ss2))
             :qvalue frac-min
             :inside (append (inside ss1) (inside ss2))))
    (setf (tempo rep) (concat-tempi s1 s2))
    rep))

(defmethod* concat ((s1 measure) (s2 voice))
  (concat (measure->voice s1) s2))

(defmethod* concat ((s1 voice ) (s2 measure))
  (concat  s1 (measure->voice s2)))


(defmethod* concat ((s1 poly ) (s2 poly))
  (mki 'poly
       :voices (mapcar 'concat (inside s1) (inside s2)) ))

(defmethod* concat ((s1 multi-seq ) (s2 multi-seq))
  (mki 'multi-seq 
       :chord-seqs
       (loop 
         with chs1 = (inside s1)
         with chs2 = (inside s2)
         while (or chs1 chs2)
         collect (if chs1
                   (if chs2 (concat (pop chs1) (pop chs2))
                       (pop chs1))
                   (pop chs2)))))



;--------------------
;  GET-CHORDS
;--------------------

(defmethod* get-chords ((self sequence*))
   :initvals '(nil) 
   :indoc '("a music sequence (voice, chord-seq, poly, multi-seq)")
   :icon 262
   :doc "
Extract an ordered list (or list of list) of chords from a music sequence or superposition.
Time (Onset) information is removed.
"
   (loop for sub in (inside self)
         when (not (cont-chord-p sub))
         if (note-or-chord-p sub) collect (ot-clone sub)
         else if (container-p sub) append (chords sub)))


(defmethod* get-chords ((self superposition))
  (mapcar 'get-chords (inside self)))

(defmethod* get-chords ((self chord))
  (list (clone self)))



;--------------------
;  MASK
;--------------------


(defmethod* mask ((obj1 sequence*) (obj2 sequence*) &optional (mode 1))
   :initvals '(nil nil 1) :indoc '("a voice" "a voice" "1 or -1")
   :doc "
Masks <obj1> by <obj2>, that is, plays the <obj1> when <obj2> is not playing. 

Optional argument <mode> = -1 inverts the mask.
" 
   :icon 263
   (normalize-chord obj1)
   (normalize-chord obj2)
   (om-masque obj1 obj2 :mode (if (= mode 1) 'masque 'demasque)))


;--------------------
;  MERGER
;--------------------



;===================temporary merge correction===================================
;by KA 06-11-2001
(defun correct-merge (tree)
  
  (if (atom tree)
    tree
    (list (if (ratiop (first tree)) 
            (list (numerator (first tree)) 
                  (denominator (first tree))) 
            (first tree))
          (mapcar 'correct-merge (second tree)))))

(defmethod old-merger ((obj1 sequence*) (obj2 sequence*))
  (normalize-chord obj1)
  (normalize-chord obj2)
  (om-merge obj1 obj2))

(defmethod! merger ((self1 sequence*) (self2 sequence*)) 
   :initvals '(nil nil) 
   :indoc '("a music sequence" "a music sequence")
   :icon 253
   :doc "
Merges two voices, measures or chord-seqs into a new voice, measure or chord-seq.

MERGER can also merge two chords into a new one.
"
   (let* ((result (old-merger self1 self2))
          (newself (clone result))
          (tree (tree newself))
          (newtree (list '? (cadr (correct-merge tree)))))
     (setf (tree newself) newtree)
     ))

(defmethod! merger ((self1 chord) (self2 chord)) 
    (objfromobjs (list self1 self2) (make-instance 'chord)))


;--------------------
;  SELECT
;--------------------
(defun first-tempo-a-la-noir (measure)
  (let ((chords (cons-chord&rest-list measure)))
    (list 1/4 (/ (round (* 10 (qtempo (car chords)))) 10.0) )))

(defun last-tempo-a-la-noir (measure i)
  (let ((chords (cons-chord&rest-list measure)))
    (list (list i (- (length chords) 1)) (list 1/4 (/ (round (* 10 (qtempo (car (last chords))))) 10.0)))))

(defun select-tempi (voice start end)
  (let* ((tempo (copy-list (tempo voice))))
    (if (null (second tempo)) tempo
     (let ((changes (second tempo))
           (lasttempo (car tempo))
           tempilist)
       (loop for item in changes do
              (when (< (caar item) start)
               (setf lasttempo (second item)))
             (when (and (>= (caar item) start) (<= (caar item) end))
               (push (list (list (- (caar item) start) (second (car item))) (second item)) tempilist)))
       (setf tempilist (reverse tempilist))
       (if  (not tempilist)
           (if (third lasttempo)
               (let* ((measure (nth start (inside voice)))
                      (ftempo (first-tempo-a-la-noir measure))
                      (ltempo (last-tempo-a-la-noir (nth end (inside voice)) (- end start) )))
                 (list ftempo (list (list '(0 0)  (append ftempo (list t))) ltempo))) 
             (list lasttempo nil))
         (let (rep lastchange)
           (cond
            ((equal (caar tempilist) '(0 0))
             (if (third (second (car tempilist)))
                 (setf rep (list (butlast (second (car tempilist))) tempilist)) 
               (setf rep (list (second (car tempilist)) (cdr tempilist)))))
            (t (if (third lasttempo)
                   (let* ((measure (nth start (inside voice)))
                          (ftempo (first-tempo-a-la-noir measure)))
                     (setf rep (list ftempo (cons (list '(0 0) (append ftempo (list t))) tempilist)))) 
                 (setf rep (list lasttempo tempilist)))))
           (setf lastchange (second (car (last (second rep)))))
           (when (third lastchange)
             (let* ((ltempo (last-tempo-a-la-noir (nth end (inside voice)) (- end start) )))
               (setf (nth 1 rep) (append (nth 1 rep) (list ltempo)))))
           rep))))))


(defmethod* select ((self voice) (start integer) (end integer))
   :initvals '(nil 0 1) 
   :indoc '("a music sequence" "an integer" "an integer")
   :icon 330
   :doc "
Extracts a subseqence :

when :
<self> is a voice, <start> and <end> are measure numbers, result is a voice.
<self> is a poly, <start> and <end> are measure numbers, result is a poly
<self> is a chord-seq, <start> and <end> are absolute positions in ms, result is a chord-seq.
<self> is a multi-seq, <start> and <end> are absolute positions in ms, result is a multi-seq.
"
   
   (when (> start end) (Om-Message-abort "Extract-measures : Bad start/end parameters"))
   (let* ((measures (clone (subseq (inside self) 
                                   (max 0 (min start (1- (length (inside self)))))
                                   (min (1+ end) (length (inside self))))))
          (temp-voice (mki 'voice :empty t :inside measures))
          (first-chord (first-container temp-voice '(chord)))
          (last-chord (last-container temp-voice '(chord))))
     (when first-chord
       (when (cont-chord-p first-chord) (change-class first-chord 'chord))
       (loop for note in (inside first-chord)
             do (case (tie note)
                  (end (setf (tie note) nil))
                  (continue (setf (tie note) 'begin)))))
     (when last-chord
       (loop for note in (inside last-chord)
             do (case (tie note)
                  (begin (setf (tie note) nil))
                  (continue (setf (tie note) 'end)))))
     (loop for measure in measures
           with offset = (offset (first measures))
           do (setf (offset measure) (- (offset measure) offset)))
     (let ((voice (mki 'voice 
                       :empty t
                       :offset 0
                       :extent 0
                       :qvalue (qvalue self)
                       :inside measures
                       )))
       (setf (tempo voice)  (select-tempi self start end))
       (adjust-extent voice)
       voice)))


(defmethod* select ((self poly) (start integer) (end integer))
   (mki 'poly
        :voices (loop for voice in (inside self) 
                      collect (select voice start end))))

(defmethod* select ((self chord-seq) (start number) (end number))
   (unless (and (>= start 0)
                (< start end)
                (<= end (get-obj-dur self)))   ;;; replaced extent->ms
     (Om-Message-abort "select : Bad start/end parameters"))
   (let ((chords 
          (loop for onset in (Lonset self)
                for chord in (inside self)
                when (and (>= onset start) (< onset end))
                collect (list (- onset start) chord))))
     (if chords
       (mki (type-of self) 
            :LMidic (mapcar 'second chords)
            :Lonset  (append (mapcar 'first chords) (list (- end start)))
            :legato 0)
       (mki (type-of self)
            :Lonset (list 0 (- end start))
            :Lmidic NIL)
       )))

(defmethod* select ((self multi-seq) (start number) (end number))
   (mki 'multi-seq
        :chord-seqs
        (loop for chord-seq in (inside self)
              collect (select chord-seq start end))))



(defun load-obj-list-from-save (list)
  (loop for item in list
        collect (eval item)))

(defmethod get-real-chords (self)
  (loop for item in (collect-chords self)
        when (and (chord-p item) (not (cont-chord-p item))) collect item))

  


(defmethod execption-save-p ((self voice)) 'voice)
(defmethod save-exepcion ((self voice))
  `(when (find-class ',(type-of self) nil)
     (make-instance ',(type-of self)
       :tree ',(tree self)
       :chords (load-obj-list-from-save '(,.(mapcar #'(lambda (x) (omNG-save x)) (get-real-chords self))))
       :tempo ',(tempo self)
       :legato ,(legato self)
       :ties ',(ties self))))


