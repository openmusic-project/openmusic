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

#|
;legacy!
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
  (let* ((frac-min (fraction-minimale-commune s1 s2))
	 (ss1 (duplique-structure-musicale s1))
	 (ss2 (duplique-structure-musicale s2))
	 (s2-offset-in-qv (when s2-offset (* frac-min  (/ s2-offset 1000))))) ;expressed in qvalue
    
    (change-qvalue ss1 frac-min)
    (change-qvalue ss2 frac-min)

    (loop for item in (inside ss2) do (setf (offset item) (+ (offset item) (or s2-offset-in-qv (extent ss1)))))
    (mki (type-of s1) 
             :empty t
             :offset 0
             :extent (+ (extent ss1) (extent ss2) (or s2-offset-in-qv 0))
             :qvalue frac-min
             :inside (append (inside ss1) (inside ss2)))))
|#


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
			 :extent (+ (extent ss1) (extent ss2))
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
;kh debugging utilities

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



(defmethod get-chords&rests ((self measure))
  (let ((chords (get-all-chords self)))
  (remove nil (loop for i in chords
                    collect (if (not (cont-chord-p i)) i)))))

(defmethod get-chords&rests ((self voice))
  (let ((chords (get-all-chords self)))
  (remove nil (loop for i in chords
                    collect (if (not (cont-chord-p i)) i)))))


;--------------------
;  GET-GROUP-OBJS
;--------------------
;kh 

(defmethod get-group-objs ((self voice))
   :icon 134
   :indoc '("self")
   :initvals (list t)
   :doc "Returns list of groups as objects contained in <self>."
(mapcar #'(lambda (x) (get-group-objs x))
          (inside self)))

(defmethod get-group-objs ((self measure))
(mapcar #'(lambda (x) (get-group-objs x))
          (inside self)))

(defmethod get-group-objs ((self group))
(remove nil (list self (remove nil (mapcar #'(lambda (x) (get-group-objs x))
          (inside self))) (second (absolute-props (tree self))))))


(defmethod get-group-objs ((self group))
(remove nil (list self (remove nil (mapcar #'(lambda (x) (get-group-objs x))
          (inside self))))))

(defmethod get-group-objs ((self continuation-chord))
())

(defmethod get-group-objs ((self chord))
())

(defmethod get-group-objs ((self rest))
())


;;;;
(defmethod get-chord-groups ((self chord))
  "Returns all groups that self belongs to"
  (let* ((parent (if (group-p (parent self)) (parent self) nil))
         res
         (switch t))
    (loop while switch
          do 
          (if (group-p parent)
              (progn 
                (setf switch t)
                (push parent res)
                (setf parent (parent parent))
                )
            (setf switch nil)))
    (reverse res)))

(defmethod get-chord-groups ((self t)) nil)    

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;       TOOLS     ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod update-obj ((self t)) t)

(defmethod update-obj ((self chord))
  "when <self>'s panel is openned, updates graphic contents"
(let ((box (associated-box self))) 
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))
  
(defmethod update-obj ((self chord-seq))
(let ((box (associated-box self)))
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))


(defmethod update-obj ((self multi-seq))
(let ((box (associated-box self)))
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))

(defmethod update-obj ((self voice))
  (let ((box (associated-box self))) 
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))

(defmethod update-obj ((self poly))
  (let ((box (associated-box self))) 
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  SET-OBJ-TEMPO      ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qtempo->tempo (qtempo)
  (cond ((atom qtempo) 
         (list (list 1/4 qtempo) nil))
        ((listp (caar qtempo))
         (let* ((frst (second (car qtempo)))
                (newfrst (list 1/4 frst))
                (rst (cdr qtempo))
                (indx (mapcar 'caar rst))
                (tempi (mapcar 'second rst))
                (newrst (loop for i in tempi
                              for n in indx
                              collect (list (list n 0) (list 1/4 i nil)))))
           (cons newfrst (list newrst))))
        (t qtempo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-tempo ((self voice) tempo &key (mode 'clone))
  :initvals '(t '((1/4 60) nil) 'clone)
  :indoc '("voice or poly" "tempo" "mode")
  :menuins '((2 (("clone" clone) 
                 ("destructive" destructive))))
  :icon 355
  :doc "Changes tempo of <self>. 
If <mode> is clone, this will output a new voice (default).
If <mode> is destructive, tempo of <self> will be destructively changed." 
  (let ((tempo (qtempo->tempo tempo))) 
    (if (equal mode 'destructive)
        (progn 
          (setf (tempo self) tempo)
          (update-obj self))
      (let ((new (clone self)))
        (setf (tempo new) tempo)
        new))))


(defmethod! set-obj-tempo ((self poly) tempo  &key (mode 'clone))
  (if (equal mode 'clone)
      (make-instance 'poly
                     :voices  (loop for i in (inside self)
                                    for tp in tempo
                                    collect (set-obj-tempo i tp :mode 'clone)))
    (progn
      (loop for i in (inside self)
            for tp in tempo
            do (set-obj-tempo i tp :mode 'destructive))
      (update-obj self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-VEL     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-vel ((self note) 
                          (vel number)
                          &key (mode 'clone) (n nil))
  :initvals '(t 64 nil 'clone) 
  :indoc '("self" "chan" "n")
  :menuins '((2 (("clone" 'clone) 
                 ("destructive" 'destructive))))
  :icon 355
  :doc  "Changes midi channels in a voice or poly object. If it is a poly object
you can specify which voice will be changed by openning the optional input <n>.
Further more, in voice method, if list inputed, it will change all notes according to the list."
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (setf (vel clone) vel)
        clone)
    (setf (vel self) vel)))


(defmethod! set-obj-vel ((self chord) 
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              do (set-obj-vel i vel :mode 'destructive))
        clone)
    (progn
      (loop for i in (inside self)
            do (set-obj-vel i vel :mode 'destructive))
      (update-obj self))))

(defmethod! set-obj-vel ((self chord) 
                          (vel list)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              for vl in vel
              do (set-obj-vel i vl :mode 'destructive))
        clone)
    (progn
      (loop for i in (inside self)
            for vl in vel
            do (set-obj-vel i vl :mode 'destructive))
      (update-obj self))))
    
(defmethod! set-obj-vel ((self chord-seq) 
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for ob in objs
              do (set-obj-vel ob vel :mode 'destructive))
        clone)
    (progn
      (loop for ob in (inside self)
            do (set-obj-vel ob vel :mode 'destructive))
      (update-obj self))))

(defmethod! set-obj-vel ((self chord-seq) 
                          (vel list)
                          &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for i in vel
              for ob in objs
              do (set-obj-vel ob i :mode 'destructive))
        clone)
    (progn
      (loop for i in vel
            for ob in (inside self)
            do (set-obj-vel ob i :mode 'destructive))
      (update-obj self))))


(defmethod! set-obj-vel ((self multi-seq) (vel number) &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for vx in n 
                    do (set-obj-vel (nth vx voices) vel :mode 'destructive))
              clone)
          (progn
            (loop for vx in (inside clone)
                  do (set-obj-vel vx vel :mode 'destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for vx in n 
                  do (set-obj-vel (nth vx voices) vel :mode 'destructive)))
        (loop for vx in (inside self)
              do (set-obj-vel vx vel :mode 'destructive)))
      (update-obj self))))

(defmethod! set-obj-vel ((self multi-seq) (vel list) &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for i in vel
                    for vx in n 
                    do (set-obj-vel (nth vx voices) i :mode 'destructive))
              clone)
          (progn
            (loop for i in vel
                  for vx in (inside clone)
                  do (set-obj-vel vx i :mode 'destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for i in vel
                  for vx in n 
                  do (set-obj-vel (nth vx voices) i :mode 'destructive)))
        (loop for i in vel
              for vx in (inside self)
              do (set-obj-vel vx i :mode 'destructive)))
      (update-obj self))))
  


(defmethod! set-obj-vel ((self voice)
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-vel chrdseq vel :mode 'destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-vel chrdseq vel :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-vel ((self voice)
                          (vel list)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-vel chrdseq vel :mode 'destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-vel chrdseq vel :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-vel ((self poly)
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-vel multiseq vel :mode 'destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-vel multiseq vel :mode 'destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

(defmethod! set-obj-vel ((self poly)
                         (vel list)
                         &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-vel multiseq vel :mode 'destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-vel multiseq vel :mode 'destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-CHAN     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-chan ((self note) 
                          (chan number)
                          &key (mode 'clone) (n nil))
  :initvals '(t 1 nil 'clone) 
  :indoc '("self" "chan" "n")
  :menuins '((2 (("clone" 'clone) 
                 ("destructive" 'destructive))))
  :icon 355
  :doc  "Changes midi channels in a voice or poly object. If it is a poly object
you can specify which voice will be changed by openning the optional input <n>.
Further more, in voice method, if list inputed, it will change all notes according to the list."
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (setf (chan clone) chan)
        clone)
    (setf (chan self) chan)))


(defmethod! set-obj-chan ((self chord) 
                          (chan number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              do (set-obj-chan i chan :mode 'destructive))
        clone)
    (progn
      (loop for i in (inside self)
            do (set-obj-chan i chan :mode 'destructive))
      (update-obj self))))

(defmethod! set-obj-chan ((self chord) 
                          (chan list)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              for ch in chan
              do (set-obj-chan i ch :mode 'destructive))
        clone)
    (progn
      (loop for i in (inside self)
            for ch in chan
            do (set-obj-chan i ch :mode 'destructive))
      (update-obj self))))
    
(defmethod! set-obj-chan ((self chord-seq) 
                          (chan number)
                          &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for ob in objs
              do (set-obj-chan ob chan :mode 'destructive))
        clone)
    (progn
      (loop for ob in (inside self)
            do (set-obj-chan ob chan :mode 'destructive))
      (update-obj self))))

(defmethod! set-obj-chan ((self chord-seq) 
                          (chan list)
                          &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for i in chan
              for ob in objs
              do (set-obj-chan ob i :mode 'destructive))
        clone)
    (progn
      (loop for i in chan
            for ob in (inside self)
            do (set-obj-chan ob i :mode 'destructive))
      (update-obj self))))


(defmethod! set-obj-chan ((self multi-seq) (chan number) &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for vx in n 
                    do (set-obj-chan (nth vx voices) chan :mode 'destructive))
              clone)
          (progn
            (loop for vx in (inside clone)
                  do (set-obj-chan vx chan :mode 'destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for vx in n 
                  do (set-obj-chan (nth vx voices) chan :mode 'destructive)))
        (loop for vx in (inside self)
              do (set-obj-chan vx chan :mode 'destructive)))
      (update-obj self))))

(defmethod! set-obj-chan ((self multi-seq) (chan list) &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for i in chan
                    for vx in n 
                    do (set-obj-chan (nth vx voices) i :mode 'destructive))
              clone)
          (progn
            (loop for i in chan
                  for vx in (inside clone)
                  do (set-obj-chan vx i :mode 'destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for i in chan
                  for vx in n 
                  do (set-obj-chan (nth vx voices) i :mode 'destructive)))
        (loop for i in chan
              for vx in (inside self)
              do (set-obj-chan vx i :mode 'destructive)))
      (update-obj self))))
  


(defmethod! set-obj-chan ((self voice)
                          (chan number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-chan chrdseq chan :mode 'destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-chan chrdseq chan :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-chan ((self voice)
                          (chan list)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-chan chrdseq chan :mode 'destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-chan chrdseq chan :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-chan ((self poly)
                          (chan number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-chan multiseq chan :mode 'destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-chan multiseq chan :mode 'destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

(defmethod! set-obj-chan ((self poly)
                          (chan list)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-chan multiseq chan :mode 'destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-chan multiseq chan :mode 'destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-PORT     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod! set-obj-port ((self note) 
                        (port number)
                        &key (mode :clone) (n nil))
  :initvals '(t 0 nil 'clone) 
  :indoc '("self" "port" "n")
  :menuins '((2 (("clone" :clone) 
                 ("destructive" :destructive))))
  :icon 355
  :doc  "Changes midi port in a voice or poly object. If it is a poly object
you can specify which voice will be changed by openning the optional input <n>.
Further more, in voice method, if list inputed, it will change all notes according to the list."
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (setf (port clone) port)
        clone)
    (setf (port self) port)))

(defmethod! set-obj-port ((self chord) 
                          (port number)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              do (set-obj-port i port :mode :destructive))
        clone)
    (progn
      (loop for i in (inside self)
            do (set-obj-port i port :mode :destructive))
      (update-obj self))))

(defmethod! set-obj-port ((self chord) 
                          (port list)
                          &key (mode :clone) (n nil))
    (if (equal mode :clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              for p in port
              do (set-obj-port i p :mode :destructive))
        clone)
      (progn
        (loop for i in (inside self)
              for p in port
              do (set-obj-port i p :mode :destructive))
        (update-obj self))))
    
(defmethod! set-obj-port ((self chord-seq) 
                        (port number)
                        &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
       (let* ((clone (clone self))
              (objs (inside clone)))
         (loop for ob in objs
               do (set-obj-port ob port :mode :destructive))
         clone)
    (progn
      (loop for ob in (inside self)
            do (set-obj-port ob port :mode :destructive))
      (update-obj self))))

(defmethod! set-obj-port ((self chord-seq) 
                        (port list)
                        &key (mode :clone) (n nil))
  (if (equal  mode :clone)
       (let* ((clone (clone self))
              (objs (inside clone)))
         (loop for i in port
               for ob in objs
               do (set-obj-port ob i :mode :destructive))
         clone)
    (progn
    (loop for i in port
          for ob in (inside self)
          do (set-obj-port ob i :mode :destructive))
    (update-obj self))))

(defmethod! set-obj-port ((self multi-seq) (port number) &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for vx in n 
                    do (set-obj-port (nth vx voices) port :mode :destructive))
              clone)
          (progn
            (loop for vx in (inside clone)
                  do (set-obj-port vx port :mode :destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for vx in n 
                  do (set-obj-port (nth vx voices) port :mode :destructive)))
        (loop for vx in (inside self)
              do (set-obj-port vx port :mode :destructive)))
      (update-obj self))))


(defmethod! set-obj-port ((self multi-seq) (port list) &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for i in port
                    for vx in n 
                    do (set-obj-port (nth vx voices) i :mode :destructive))
              clone)
          (progn
            (loop for i in port
                  for vx in (inside clone)
                  do (set-obj-port vx i :mode :destructive))
            clone)))
    (progn
    (if n
        (let ((voices (inside self)))
          (loop for i in port
                for vx in n 
                do (set-obj-port (nth vx voices) i :mode :destructive)))
        (loop for i in port
              for vx in (inside self)
              do (set-obj-port vx i :mode :destructive))
        )
    (update-obj self))))
  


(defmethod! set-obj-port ((self voice)
                        (port number)
                        &key (mode :clone) (n nil))
    (if (equal mode :clone)
        (let* ((clone (clone self))
               (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
          (set-obj-port chrdseq port :mode 'destructive)
          (setf (chords clone) (inside chrdseq))
          clone)
      
      (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
        (set-obj-port chrdseq port :mode 'destructive)
        (setf (chords self) (inside chrdseq))
        (update-obj self))))

(defmethod! set-obj-port ((self voice)
                          (port list)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-port chrdseq port :mode 'destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-port chrdseq port :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-port ((self poly)
                          (port number)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-port multiseq port :mode :destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-port multiseq port :mode :destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

(defmethod! set-obj-port ((self poly)
                          (port list)
                          &key (mode :clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-port multiseq port :mode :destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-port multiseq port :mode :destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-PITCH    ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-pitch ((self voice) 
                           (newpitch list) 
                           &key 
                           (mode 'clone)
                           (nth nil)) 
  :initvals '(t '(6000 6100) 'clone nil) 
  :indoc '("self" "newpitch" "mode" "nth")
  :menuins '((2 (("clone" clone) 
                 ("destructive" destructive))))
  :icon 355
  :doc  "Changes pitches in a voice or poly object. 
<mode> keyword can be clone, <self> isunchanged. If :mode = destructive, <self> will be mnodified.
If <self> is a poly object
if key <nth> is specified, nth voices will be changed. 
If <nth> is nil (default), the pitches will be distributed vertically in the poly."

  (if (equal mode 'clone)
      (make-instance 'voice 
                     :tree (tree self) 
                     :chords newpitch
                     :tempo (tempo self))
    (setf (chords self) newpitch)))
    

(defmethod! set-obj-pitch ((self voice) 
                           (newpitch number) 
                           &key 
                           (mode 'clone) 
                           (nth nil))
   (let* ((npuls (n-pulses (tree self)))
          (pitch (repeat-n newpitch npuls)))
     (if (equal mode 'clone)
     (make-instance 'voice 
                    :tree (tree self) 
                    :chords pitch
                    :tempo (tempo self))

     (setf (chords self) pitch))))


(defmethod! set-obj-pitch ((self voice) 
                           (newpitch chord-seq) 
                           &key 
                           (mode 'clone) 
                           (nth nil))
  (let ((chords (inside newpitch)))
  (if (equal mode 'clone)
      (make-instance 'voice 
                     :tree (tree self) 
                     :chords chords
                     :tempo (tempo self))

    (setf (chords self) chords))))


(defmethod! set-obj-pitch ((self poly) 
                           (newpitch list)
                           &key 
                           (mode 'clone)
                           (nth nil))
  (if (equal mode 'clone)
      (let* ((newself (clone self)))
        (if nth
            (progn
              (mapcar #'(lambda (nth-voices pitches) 
                          (setf (nth nth-voices (inside newself))
                                (set-obj-pitch (nth nth-voices (inside newself)) pitches)))
                      nth newpitch)
              newself)
        (verticalize-pitch newself newpitch)))
        (if nth
            (mapcar #'(lambda (nth-voices pitches) 
                (setf (nth nth-voices (inside self))
                      (set-obj-pitch (nth nth-voices (inside self)) pitches)))
            nth newpitch)
          (verticalize-pitch self newpitch))
        ))

(defmethod! verticalize-pitch ((self poly) (pitch list))
   :initvals (list t '(6000 6100)) 
   :indoc '("poly" "pitches")
   :icon 355
   :doc "In first input a poly object.
In the second object a simple list of pitches. The method will return
a poly object with the same rythm but with new pitches. These 
will occur as a melodic line, i.e the same order given in <pitch> but 
distributed in each voice according to their offsets. Meaning that the
melodic line will be distributed in all voices. It is important to know 
the total pitches needed throughout the poly i.e in all voices. "
  (let* ((voices (inside self))
         (tempi (mapcar #'tempo (inside self)))
         (vox->chrdsq (mapcar #'(lambda (x) (Objfromobjs x (make-instance 'chord-seq)))
                              voices))
         (voxoffsets (mapcar #'(lambda (x) (butlast (lonset x)))
                             vox->chrdsq))
         (voxnum  (loop for i from 0 to (- (length voices) 1)
                        for vx in voices 
                        collect (repeat-n i (length (chords vx)))))
          
         (listdata  (mapcar #'(lambda (onsets pitch) (mat-trans (list onsets pitch)))
                            voxoffsets
                            voxnum))
         
         (sortedata (sort. (flat-once listdata) '< 'first))
         (index (mapcar 'second sortedata))
         (resultat (repeat-n '() (length (inside self))))
         (parsednotes (loop 
                       for i in pitch 
                       for indx in index
                       do (push i (nth indx resultat))))
         (ordered (loop for i in resultat
                        collect (reverse i)))
         (trees (loop for i in (inside self)
                      collect (tree i))))

   
    (make-instance 'poly 
                   :voices (loop for i in trees
                                 for chrd in ordered
                                 for tmp in tempi
                                 collect (make-instance 'voice
                                                        :tree i
                                                        :chords chrd
                                                        :tempo tmp )))



    ))

(defmethod! verticalize-pitch ((self poly) (chord-seq chord-seq))
  (let ((pitch (inside chord-seq)))
    (verticalize-pitch self pitch)))
