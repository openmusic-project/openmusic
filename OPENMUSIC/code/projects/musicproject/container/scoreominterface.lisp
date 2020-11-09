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



(defmethod* concat ((s1 sequence*) (s2 sequence*) &optional s2-offset)
  :initvals '(nil nil) 
  :indoc '("a musical sequence" "a musical sequence" "(absolute) offset, expressed in ms.")
  :icon 230
  :doc "Concatenates two music sequences into a new one.

Optional input 's2-offset' may be used to pass an (absolute) offset value for s2.  s2-offset is expressed ms.

Type of the return value :
 voice x voice   => Voice
 measure x voice => Voice
 voice x measure => Voice
 multi-seq x multi-seq => multi-seq
 chord-seq x chord-seq => chord-seq
 poly x poly => poly
" 
  (let ((ss1 (duplique-structure-musicale s1))
	(ss2 (duplique-structure-musicale s2)))

    (setqvalue ss1 1000)     ;work in ms. here
    (setqvalue ss2 1000)

    (let ((s2-offset (or s2-offset (extent ss1))))
      (loop for item in (inside ss2) do (incf (offset item) s2-offset))
      (let ((outseq (mki (type-of s1)
			 :empty t
			 :offset 0
			 :extent (+ (extent ss1) (extent ss2) s2-offset)
			 :qvalue 1000
			 :inside (append (inside ss1) (inside ss2)))))
	;; optimize qvalue for output seq:
	(QNormalize outseq)))))

(defmethod* concat ((s1 simple-container) (s2 null) &optional s2-offset)
  s1)
(defmethod* concat ((s1 null) (s2 simple-container) &optional s2-offset)
  (concat (mki (type-of s2) :empty t) s2 s2-offset))


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
      
(defmethod* concat ((s1 voice) (s2 voice) &optional s2-offset)
 (declare (ignore s2-offset))
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

(defmethod* concat ((s1 measure) (s2 voice) &optional s2-offset)
  (declare (ignore s2-offset))
  (concat (measure->voice s1) s2))

(defmethod* concat ((s1 voice ) (s2 measure) &optional s2-offset)
  (declare (ignore s2-offset))
  (concat  s1 (measure->voice s2)))

(defmethod* concat ((s1 poly ) (s2 poly) &optional s2-offset)
  (declare (ignore s2-offset))
  (mki 'poly
       :voices (mapcar 'concat (inside s1) (inside s2)) ))

(defmethod* concat ((s1 multi-seq ) (s2 multi-seq) &optional s2-offset)
  (setqvalue s1 1000)
  (setqvalue s2 1000)
  (mki 'multi-seq 
       :chord-seqs
       (let ((cur-offset (or s2-offset 0)))
	 (loop 
	    with chs1 = (inside s1)
	    with chs2 = (inside s2)
	    do (setf cur-offset (or s2-offset (and (car chs1) (extent (car chs1))) cur-offset))
	    while (or chs1 chs2)
	    collect (concat (pop chs1) (pop chs2) cur-offset)))))




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
;  GET-ALL-CHORDS
;--------------------
;kh

(defmethod* get-all-chords ((self poly))
   :initvals '(nil) 
   :indoc '("a voice or poly")
   :icon 262
   :doc "Returns all chords, rests and continuous-chord objects"
(loop for i in (inside self)
      collect (get-all-chords i)))

(defmethod* get-all-chords ((self voice))
(flat (loop for i in (inside self)
      collect (get-all-chords i))))

(defmethod* get-all-chords ((self measure))
(loop for i in (inside self)
      collect (get-all-chords i)))

(defmethod* get-all-chords ((self group))
(loop for i in (inside self)
      collect (get-all-chords i)))

(defmethod* get-all-chords ((self continuation-chord))
self)

(defmethod* get-all-chords ((self chord))
self)

(defmethod* get-all-chords ((self rest))
self)

(defmethod* get-all-chords ((self t))
t)

(defmethod get-chords&cont-chords ((self measure))
  (let ((chords (get-all-chords self)))
  (remove nil (loop for i in chords
                    collect (if (not (rest-p i)) i)))))

(defmethod get-chords&cont-chords ((self voice))
  (let ((chords (get-all-chords self)))
  (remove nil (loop for i in chords
                    collect (if (not (rest-p i)) i)))))

    
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
     (loop
	with offset = (offset (first measures))
	for measure in measures
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
  
  (when (> start end)
    (om-beep-msg (format nil "select : Bad start/end parameters: start: ~A, end: ~A" start end)))

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

;; if no end: go until the end 
(defmethod* select ((self chord-seq) (start number) (end t))
  (select self start (if end end (list-max (lonset self)))))

(defmethod* select ((self multi-seq) (start number) (end t))
  (select self start (if end end (list-max (lonset self)))))



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


