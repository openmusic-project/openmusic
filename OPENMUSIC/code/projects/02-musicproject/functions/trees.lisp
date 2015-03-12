(in-package :om)


;********************************************
;This code was written By Karim Haddad, IRCAM 03/2004
;********************************************

;mktree was written By Orjan Sandred, IRCAM 03/1999

;Use the function simple->tree. All other functions are called from this one.
;This function builds a hierarchical rhythmtree from a sequence of rhythm values on the format
;(1/4 1/4 1/4 1/4).

;In "better-predefined-subdiv?" you can define other way of notation for special cases (choise of subtrees).


(in-package :om) 

(defun make-proportional-cell (dur-list)
  (mapcar #'(lambda (dur) (* dur (apply 'lcm (mapcar 'denominator dur-list))))
          dur-list))

;offset from 1 always!!! Because otherwise the first pause ( -0!) will dissapear.
(defun x-dx-pause-ok (x-list)
  (mapcar '*
          (mapcar #'(lambda (absdur) (if (> absdur 0) 1 -1)) x-list)
          (om::x->dx (mapcar 'abs x-list))))

(defun dx-x-pause-ok (starttime x-list)
  (mapcar '*
          (append (mapcar #'(lambda (absdur) (if (> absdur 0) 1 -1)) x-list) '(1))
          (om::dx->x starttime (mapcar 'abs x-list))))

#|
(defun simplify-proportions (proportion-list)
  (mapcar #'(lambda (value) (/ value (apply 'gcd proportion-list))) proportion-list))
|#

;=== redefinition: cas des valeurs non entieres
(defun simplify-proportions (proportion-list)
  (let* ((list (mapcar  'round proportion-list))
         (gcdl (apply 'gcd list)))
    (mapcar #'(lambda (value) (/ value gcdl)) list)))

;all negative numbers following eachother should be fused to single negative numbers
(defun fuse-pauses (proportional-list)
  (let ((pointer-to-list 0))
    (simplify-proportions
     (loop until (>= pointer-to-list (length proportional-list))
           collect (if (< (nth pointer-to-list proportional-list) 0)
                     (let ((this-pause (nth pointer-to-list proportional-list)))
                       (incf pointer-to-list)
                       (loop until (or (>= pointer-to-list (length proportional-list))
                                       (> (nth pointer-to-list proportional-list) 0))
                             do (progn
                                  (incf this-pause (nth pointer-to-list proportional-list))
                                  (incf pointer-to-list)))
                       this-pause)
                     (progn (incf pointer-to-list)
                            (nth (1- pointer-to-list) proportional-list)))))))



;if a pause first, start list with -1
;if a tied note first, start list on next event (i.e. /= 1)
(defun make-sub-tree (local-onset)
  (list 1
        ;(fuse-pauses
         (cond ((and (or (= (car local-onset) 1) (= (car local-onset) -1)))
                (make-proportional-cell (x-dx-pause-ok local-onset)))
               (t
                (let ((proportional-list
                       (make-proportional-cell (x-dx-pause-ok (cons 1 local-onset)))))
                  (cons (float (first proportional-list))
                        (cdr proportional-list))))
               )))


(defun filter-events-between (start stop onsettimes)
  (let ((no-low-values (member start onsettimes :test #'(lambda (item value) (<= item (abs value))))))
    (reverse (member stop (reverse no-low-values) :test #'(lambda (item value) (>= item (abs value)))))))


(defun build-local-times (global-onsets global-start)
       (mapcar #'(lambda (onset) (if (> onset 0)
                                   (- onset (1- global-start))
                                   (+ onset (1- global-start))))
               global-onsets))


(defun create-beat (global-onset global-start beat-length)
  (let ((local-onset (build-local-times global-onset global-start))
        tree)
    (if (not local-onset) (setf local-onset (list (1+ beat-length))))
    (if (= (car (last local-onset)) (- -1 beat-length))
      (setf local-onset (append (butlast local-onset) (list (1+ beat-length)))))
    (if (/= (car (last local-onset)) (1+ beat-length))
      (setf local-onset (append local-onset (list (1+ beat-length)))))
    (setf tree (better-predefined-subdiv? (make-sub-tree local-onset)))
    (if (= (length (cadr tree)) 1)
      (caadr tree)
      tree)))


    
(defun fuse-pauses-and-tied-notes-between-beats (measure-tree no-of-beats)
  (let ((beat-nr 0))
    (loop until (>=  beat-nr no-of-beats)
          collect (cond ((and (typep (nth beat-nr measure-tree) 'number)
                              (< (nth beat-nr measure-tree) 0))
                         (let ((value (nth beat-nr measure-tree)))
                           (incf beat-nr)
                           (loop until (or (typep (nth beat-nr measure-tree) 'list)
                                           (>=  beat-nr no-of-beats)
                                           (and (typep (nth beat-nr measure-tree) 'integer)
                                                (> (nth beat-nr measure-tree) 0)))
                                 do (progn (decf value (truncate (abs (nth beat-nr measure-tree))))
                                           (incf beat-nr)))
                           value))
                        ((and (typep (nth beat-nr measure-tree) 'number))
                         (let ((value (nth beat-nr measure-tree)))
                           (incf beat-nr)
                           (loop until (not (typep (nth beat-nr measure-tree) 'float))
                                 do (progn (incf value (truncate (nth beat-nr measure-tree)))
                                           (incf beat-nr)))
                           value))
                        (t (incf beat-nr)
                           (nth (1- beat-nr) measure-tree))))
    ))



(defun build-one-measure (local-onset no-of-beats beat-length)
  (let ((beatlist (om::dx->x 1 (make-list no-of-beats :initial-element beat-length)))
        tree)
    (setf tree
          (fuse-pauses-and-tied-notes-between-beats
           (loop for beat-nr from 0 to (- (length beatlist) 2)
                 collect (let ((these-events (filter-events-between (nth beat-nr beatlist)
                                                                    (nth (1+ beat-nr) beatlist)
                                                                    local-onset)))
                           ;check if tied pause within subtree - if yes: give startpoint as pause
                           (if (and these-events
                                    (/= (abs (first these-events)) (nth beat-nr beatlist))
                                    (get-onsettime-before (nth beat-nr beatlist) local-onset)
                                    (> 0 (get-onsettime-before (nth beat-nr beatlist) local-onset)))
                             (setf these-events (append (list (- 0 (nth beat-nr beatlist)))
                                                        these-events))) 
                           ;check if tied pause within subtree - if yes: give startpoint as pause
                           (if (and (not these-events)
                                    (get-onsettime-before (nth beat-nr beatlist) local-onset)
                                    (> 0 (get-onsettime-before (nth beat-nr beatlist) local-onset)))
                             (setf these-events (list (- 0 (nth beat-nr beatlist)))))
                           
                           (create-beat these-events (nth beat-nr beatlist) beat-length)))
           no-of-beats))
    (list (list no-of-beats (/ 1 beat-length)) tree)))



(defun get-onsettime-before (timepoint abs-rhythm)
  (car (member timepoint (reverse abs-rhythm) :test #'(lambda (item value) (> item (abs value))))))


(defun buildmeasure-seq (abs-rhythms timesigns)
  (let ((measure-start-points (om::dx->x 1 (mapcar #'(lambda (timesign) (apply '/ timesign)) timesigns))))
    (loop for measure from 0 to (1- (length timesigns))
          collect (let ((this-seq (filter-events-between (nth measure measure-start-points)
                                                         (nth (1+ measure) measure-start-points)
                                                         abs-rhythms))
                        (this-timesign (nth measure timesigns))
                        local-onset)
                    ;check if measure starts with tied pause
                    (if (and this-seq
                             (/= (abs (first this-seq)) (nth measure measure-start-points))
                             (> 0 (get-onsettime-before (nth measure measure-start-points) abs-rhythms)))
                      (setf this-seq (append (list (- 0 (nth measure measure-start-points)))
                                             this-seq)))
                    (if (and (not this-seq) 
                             (> 0 (get-onsettime-before (nth measure measure-start-points) abs-rhythms)))
                      (setf this-seq (list (- 0 (nth measure measure-start-points)))))
                    (setf local-onset (build-local-times this-seq (nth measure measure-start-points)))
                    
                    (build-one-measure local-onset 
                                       (car this-timesign)
                                       (/ 1 (cadr this-timesign)))))))


(defun simple->tree (rhythmseq timesignseq)
  (let ((abs-rhythms (dx-x-pause-ok 1 (append rhythmseq '(-100)))))
    (list '? (buildmeasure-seq abs-rhythms timesignseq))))



(defun better-predefined-subdiv? (sub-tree)
  (let* ((proportional-list (cadr sub-tree))
        (pauses (mapcar #'(lambda (value) (if (< value 0) -1 1)) proportional-list))
        (abs-proportional-list (mapcar 'abs proportional-list))
        abs-answer)
    (setf abs-answer
          (cond ((equal abs-proportional-list '(2 2 2 3 3))
                 (list (list 2 (list (first pauses)(second pauses)(third pauses)))(fourth pauses)(fifth pauses)))
                ((equal abs-proportional-list '(3 3 2 2 2))
                 (list (first pauses)(second pauses)(list 2 (list (third pauses)(fourth pauses)(fifth pauses)))))
                ((equal abs-proportional-list '(3 2 2 2 3))
                 (list (first pauses)(list 2 (list (second pauses)(third pauses)(fourth pauses)))(fifth pauses)))
                ((equal abs-proportional-list '(3.0 2 2 2 3))
                 (list (coerce (first pauses) 'float)(list 2 (list (second pauses)(third pauses)(fourth pauses)))(fifth pauses)))
                ((equal abs-proportional-list '(3 3 4 2))
                 (list (first pauses)(second pauses)(list 2 (list (* 2 (third pauses))(fourth pauses)))))
                ((equal abs-proportional-list '(4 2 3 3))
                 (list (list 2 (list (* 2 (first pauses))(second pauses)))(third pauses)(fourth pauses)))
                ((equal abs-proportional-list '(2 4 3 3))
                 (list (list 2 (list (first pauses)(* 2 (second pauses))))(third pauses)(fourth pauses)))
                ((equal abs-proportional-list '(3 3 2 4))
                 (list (first pauses)(second pauses)(list 2 (list (third pauses)(* 2 (fourth pauses))))))
                ((equal abs-proportional-list '(3 1 1 1))
                 (list (first pauses)(list 1 (list (second pauses)(third pauses)(fourth pauses)))))
                ((equal abs-proportional-list '(3.0 1 1 1))
                 (list (coerce (first pauses) 'float)(list 1 (list (second pauses)(third pauses)(fourth pauses)))))
                ((equal abs-proportional-list '(1 1 1 3))
                 (list (list 1 (list (first pauses)(second pauses)(third pauses)))(fourth pauses)))
                (t proportional-list)))
    (list 1 abs-answer)))


;************************
;om function
;
; litttle correction

(defun get-ratio-duration (l)
  (loop for item in l sum (abs item)))

(defmethod! mktree ((rhythm list) (timesigns list)) 
  :initvals '((1/4 1/4 1/4 1/4) (4 4))
  :indoc '("list of integer ratios" "list of time signatures")
  :doc "
Builds a hierarchical rhythm tree from a simple list of note values (<rhythm>).
1/4 is the quarter note.

<timesigns> is a list of time signatures, e.g. ( (4 4) (3 4) (5 8) ... )
If a single time signature is given (e.g. (4 4)), it is extended as much as required
by the 'rhythm' length.

The output rhythm tree is intended for the <tree> input of a 'voice' factory box.
"
  :icon 254
  
  (if (typep (car timesigns) 'list)
      (simple->tree  rhythm timesigns)
      (let* ((nbmesreal (* (/ (get-ratio-duration rhythm) 
                         (car timesigns))
                      (cadr timesigns)))
             (nbmes (if (integerp nbmesreal) nbmesreal (1+ (truncate nbmesreal)))))
        (simple->tree rhythm (make-list nbmes :initial-element timesigns)))
  ))

;;;;;;;;;;;;;;;;;;;;;;;;;REDUCETREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun grouper1 (liste)
"groups succesive floats"
  (if (null liste)
    liste
    (let* ((first (car liste))
           (rest (rest liste))
           )
      (if (numberp first)
        (if (plusp first)
          (cons (+ first (loop while (and (numberp (first rest)) (floatp (first rest)))
                               sum (round (pop rest))))
                (grouper1 rest))
          (cons first (grouper1 rest)))
        (cons (grouper1 first) (grouper1 rest))))))
                
                  

(defun grouper2  (liste)
"groups succesive rests (-1) into one"
  (if (null liste)
    liste
    (let* ((first (car liste))
           (rest (rest liste)))
      (if (numberp first)
        (if (plusp first) 
          (cons first (grouper2 rest))
          (cons (+ first (loop while (and (integerp (first rest)) (minusp (first rest)))
                               sum  (pop rest)))
                (grouper2 rest)))
        (cons (grouper2 first) (grouper2 rest))))))
 

(defun grouper3 (liste)
"reduces concatenated rests in the form of (1(-3)) into -1"
  (if (atom  liste)
    liste
    (if (and (numberp (first (second liste)))
             (minusp (first (second liste)))
             (null (rest (second liste)))
             (not (listp (first liste))))
      (- (first liste))
      (list (first liste)
            (mapcar 'grouper3 (second liste))))))

(om::defmethod! reduced-tree ((tree list))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("a rhythm tree")
   :icon 254
   :doc "
Reduces and simplifies a tree by concatenating consecutive rests and floats.
"
   (let ((liste (resolve-? tree)))
     (grouper3 (grouper2 (grouper1 liste)))))



#|
(om::defmethod! reducetree ((self voice))
   :initvals '(t)
   :indoc '("voice")
   :icon 254
   :doc "reduces and simplifies a tree by concatenating consecutive rests and floats
into a single correct note"
   (let* ((newvoice (clone self))
          (newtree (reducetree (tree newvoice))))
     (setf (tree newvoice) newtree)
     newvoice))

|#


(om::defmethod! reducetree ((tree list))
   :initvals '((? ((4//4 (1 (1 (1 2.0 1.0 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 -1)))))
   :indoc '("a rhythm tree")
   :icon 225
   :doc "
Reduces and simplifies a tree by concatenating consecutive rests and floats.
"
   (let ((liste (reduced-tree tree)))
     (loop
       while (not (equal liste tree))
       do 
       (setf tree liste)
       (setf liste (reduced-tree liste)))
     (remove nil liste)))


;;;;;;;;;;;;;;;;;;;;;;;;;PULSEMAKER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(om::defmethod! pulsemaker ((measures-num list) (beat-unit list) (npulses list))
; :initvals (list '(4 4) '(4 4) '(4 4))
; :indoc '("measures-num" "beat-unit" "npulses")
; :icon 225
; :doc "Constructs a tree starting with a list of measures numerators <measures-num>
;and measures denominators <beat-unit> filling these measures with n-beats <npulses>"
; (let ((pulses (loop 
;                 for i in measures-num
;                 for u in npulses
;                 collect (list i (repeat-n 1 u)))))
;
;   (list '? (loop 
;     for num in measures-num
;     for bt in beat-unit
;     for pls in pulses
;     collect (list (list num bt) (list pls))))))

;;; REPLACED BY PULSEMAKER FROM REPMUS
(om::defmethod! pulsemaker ((measures list) (beat-unit list) (n-pulses list))
 :initvals (list '(4 4) '(8 8) '(4 (1 1 1 1)))
 :indoc '("measure numerator(s)" "measure denominator(s)" "contents (pulses/subdivisions)")
 :icon 225
 :doc "
Constructs a tree starting from a (list of) measure(s) numerator(s) <measures-num>
and a (list of) denominator(s) <beat-unit> filling these measures with <npulses>.
"
 (let*((pulses (mapcar 'cons measures
          (mapcar #'(lambda (x) 
                      (if (listp x) (list x) (list (om::repeat-n 1 x)))) n-pulses)))

       (mes (mapcar #'(lambda (x y) (list x y)) measures beat-unit))
       (tree (mapcar #'(lambda (x y) (list x (list y))) mes pulses)))
   (list '? tree)))

(om::defmethod! pulsemaker ((measures list) (beat-unit number) (n-pulses list))
  (let* ((lght (length measures))
        (bt-unt-lst (repeat-n beat-unit lght)))
    (pulsemaker measures bt-unt-lst n-pulses)))

(om::defmethod! pulsemaker ((measures number) (beat-unit list) (n-pulses list))
  (let* ((lght (length beat-unit))
        (measure-lst (repeat-n measures lght)))
    (pulsemaker measure-lst beat-unit n-pulses)))

(om::defmethod! pulsemaker ((measures number) (beat-unit number) (n-pulses list))
  (let* ((lght (length n-pulses))
        (bt-unt-lst (repeat-n beat-unit lght))
        (measure-lst (repeat-n measures lght)))
    (pulsemaker measure-lst bt-unt-lst n-pulses)))




;;;;;;;;;;;;;;;;;;;;;;;;;TIETREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! tietree ((tree t))
   :initvals '((? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
   :indoc '("a rhythm tree")
   :icon 225
   :doc "
Converts all rests in <tree> (a rhytm tree, VOICE or POLY object) into ties (i.e. float values in the RT).
"
   (cond
    ((and (atom tree) (> tree 0)) tree)
    ((atom tree) (* (* tree -1) 1.0))
    (t (list (first tree)
             (mapcar 'tietree (second tree))))))

(defmethod! tietree ((self voice))  
  (let* ((newself (clone self))
         (tempo (qtempo self))
         (tempo1 
          (if (atom tempo) tempo (cadar tempo)));pour le moment...
         (tree (tietree (tree self)))
         (verynewself (setf newself (make-instance 'voice :tree tree :chords (chords self)))))
    (progn 
      (change-tempo verynewself tempo1)
      verynewself)))

(defmethod! tietree ((self poly))
  (let* ((voices (mapcar #'(lambda (x) (tietree x))
          (inside self)))
       (newpoly (make-instance 'poly :voices voices)))
       newpoly ))


;;;;;;;;;;;;;;;;;;;;;;;;;REMOVE-RESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform-rests (list)
"traces a simple list coming from trans-tree and flattened according to:
if note encoutered, then floats are checked and transformed into rests, else
rests encoutered and either other rests or errounous floats are transformed 
into notes. From Gerard."
    (loop while list
        for courant =  (pop list)
        ;do (print (mapcar 'tvalue list))
        ;do (print (tvalue courant))
        do (if (and (integerp (tvalue courant)) (minusp (tvalue courant)))
             (progn
             (setf  (tvalue courant) (- (tvalue courant)))
             (loop while (and list (not (and (integerp (tvalue (first list))) (plusp (tvalue (first list))))))
                   do (setf (tvalue (car list)) (float (abs (tvalue (car list)))))
                   (pop list))))))



(om::defmethod! remove-rests ((tree t))
   :initvals '((2 (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))))
   :indoc '("a rhythm tree")
   :icon 225
   :doc "
Converts all rests to notes.
"
(let* ((liste (if (typep tree 'voice) (tree tree) tree))
       (tree2obj (trans-tree liste))
       (tree2objclean (remove-if 'numberp (flat tree2obj)))
       (treeobjinverted (transform-rests tree2objclean)))
  (trans-obj tree2obj)))




;;;;;;;;;;;;;;;;;;;;;;;;;FILTERTREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform-notes-flt (list places)
""
  (loop while list
        for courant =  (pop list)
        do (if (and (and (integerp (tvalue courant)) (plusp (tvalue courant))) (member (tindex courant) places))
             (progn
               (setf  (tvalue courant) (- (tvalue courant)))
               (loop while (and list (floatp (tvalue (car list)))) 
                     do (setf (tvalue (car list)) (round (- (tvalue (car list)))))
                     (pop list))))))





(defun trans-note-index (treeobjlist)
  "puts index only on expressed notes and not on floats or rests (for filtering purposes)."
(if (atom treeobjlist)
  (if  (and (typep treeobjlist 'treeobj) (integerp (tvalue treeobjlist)) (plusp (tvalue treeobjlist)))
    (setf (tindex treeobjlist) (incf n)) treeobjlist)
    (list (first treeobjlist) (mapcar 'trans-note-index (second treeobjlist)))))



(om::defmethod! filtertree ((tree t) (places list))
   :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))) (0 1))
   :indoc '("a rhytm tree" "a list of indices")
   :icon 225
   :doc "
Replaces expressed notes in given positions from <places> with rests.
"
   (setf n -1)
   (let* ((liste (if (typep tree 'voice) (tree tree) tree))
          (tree2obj (trans-tree liste))
          (tree2o (trans-note-index tree2obj))
          (tree2objclean (remove-if 'numberp (flat tree2obj)))
          (treeobjinverted (transform-notes-flt tree2objclean places )))
     (trans-obj tree2obj)))



(om::defmethod! select-tree ((tree list) (places list))
   :initvals '('(? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))) '(0 1))
   :indoc '("tree" "places")
   :icon 225
   :doc "selects expressed notes only in given places <places> "
   (let* ((pulse-places (arithm-ser 0 (n-pulses tree) 1))
          (positions (x-diff pulse-places places)))
     (filtertree tree positions)))


;;;;;;;;;;;;;;;;;;;;;;;;;N-PULSES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;peut-etre probleme

(defun real-copy-list (list)
  (loop for item in list 
       collect 
       (cond
        ((listp item) (real-copy-list item))
        (t item))))



(defun give-pulse (liste)
  (let* ((D (first liste))
         (n (second liste)) rep)
    (loop for item in n append
          (if (atom item) (list item)
              (give-pulse item)))))
              


(defun pulse (mesure) 
  (om::flat (mapcar #'(lambda (x)
                               (give-pulse x))
                           mesure)))

(defun pulses (mesures)
            "retourne le nombre de pulses (pas les pauses) d'une RTM"
  
    (om::flat (mapcar #'(lambda (x) (pulse (list x))) mesures)))

(defun om-pulses (tlist)
  (mapcar #'(lambda (x)
              (pulse (list (cons '() (list x)))))
          tlist))


(defun find-po (list)
  (remove '()
          (mapcar #'(lambda (x y)
                      (if (floatp x) nil y ))
                  list (om::arithm-ser 0 (length list) 1))))
         

(defmethod! group-pulses ((tree list))
  
  :initvals '((? ((4//4 (1 (1 (1 2.0 1.0 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("a rhythm tree")
  :icon 254
  :doc "
Collects every pulses (expressed durations, including tied notes) from <tree>. 
"
  (let* ((tree2 
          (second 
           (om::mat-trans 
            (om::flat-once 
             (om::mat-trans (rest (real-copy-list tree)))))))
         (the-pulses (om::flat (om-pulses tree2)))
         (the-pos (om::remove-dup
                   (om::flat 
                    (list 0 (find-po the-pulses) (length the-pulses)))
                   'eq 1)))
    
    (if (null (find-po the-pulses)) nil 
    
    (om::group-list the-pulses
                    (om::x->dx the-pos)
                    'linear))))


(defmethod! n-pulses ((tree t))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("a rhythm tree")
  :icon 225
  :doc "
Returns the numbre of pulses in <tree>.
"
  (let (( liste (if (typep tree 'voice) (tree tree) tree)))
    (length
     (remove nil
             (mapcar #'(lambda(x) (if (> (first x) 0) x nil)) (om::group-pulses liste))))))



;;;;;;;;;;;;;;;;;;;;;;;;;REVERSETREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The class is defined in order to access easily the try consecutives kind:
;plusp integers, minusp integers and floats (pulse, rests and tied notes)


(defclass! treeobj ()
((tvalue :initform 1 :accessor tvalue :initarg :tvalue :type t)
 (tindex :initform 0 :accessor tindex :initarg :tindex :type t)))

(defun trans-tree (tree)
"transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes."
  (if (atom tree)
    (make-instance 'treeobj :tvalue tree)
    (list (first tree) (mapcar 'trans-tree (second tree)))))


(defun trans-obj (tree)
  (if (atom tree)
    (if (typep tree 'treeobj) (tvalue tree) tree)
    (list (first tree) (mapcar 'trans-obj (second tree)))))


(defun group-ties (liste)
"liste is a liste of treeobjs"
(let* (res)
  (loop for i in liste
        do (cond 
            ((and (plusp (tvalue i)) (not (floatp (tvalue i))))
               (push (list i) res))
            ((floatp (tvalue i))
               (push i (car res)))
            (t))
            )
  (loop for i in res
        do (if (> (length i) 1)
             (progn 
               (setf (tvalue (first i)) (round (tvalue (first i))))
               (setf (tvalue (last-elem i)) (* 1.0 (tvalue (last-elem i)))))))))


(defun reversedties (tree)
   
(let* ((liste (if (typep tree 'voice) (tree tree) (resolve-? tree)))
       (tree2obj (trans-tree liste))
       (tree2objclean (group-ties (remove-if 'numberp (flat tree2obj)))))
  (trans-obj tree2obj)))


(defun reversedtree (tree)
  (if (atom tree)
    tree
    (list (first tree)
          (reverse (mapcar 'reversedtree (second tree))))))


(om::defmethod! reversetree ((tree t))
  :initvals '((? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("a rhythm tree")
  :icon 225
  :doc "
Recursively reverses <tree>.
"
(let ((tree (if (typep tree 'voice) (tree tree) tree)))
       (reversedtree (reversedties tree))))




;;;;;;;;;;;;;;;;;;;;;;;;;SUBST-RHYTHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trans-tree-index (tree)
  "transforms a rhythm tree into a tree with objects in the place of musical events:
notes ,rests and tied notes and 
marks the index of events."
  (if (atom tree)
    (make-instance 'treeobj :tvalue tree
                   :tindex (incf n))
    (list (first tree) (mapcar 'trans-tree-index (second tree)))))




(defun substreeall (list pos elem)
  (loop 
    for i from 0 to (length pos)
    for a in pos
    collect (if (listp (nth i elem)) 
              (setf (tvalue (nth a list)) (list (abs (floor (tvalue (nth a list)))) (nth i elem)))
              (setf (tvalue (nth a list)) (nth i elem)))))
           





;pour eviter les tree avec '?'
;ATTENTION!! numtree transforme les 5  en (4 1.0) etc...
;ceci pourrait fausser les transformations
; toutefois il est utlisé pour regler l'histoire de ?
(defun numtree (tree)
  (let* ((newvoice (make-instance 'voice :tree tree)))
    (tree newvoice)))



; This is for both trees from measures & VOICES

(defun optimize-tree (tree )
  "this function optimizes trees modified by rotate-tree and subst-rhythm methods
by grouping each measure as a group, in order to correctly read the new rhythm 
outputed."
  
  (if (or (equal '? (first tree)) (atom (first tree))) 
    (let* ((header (first tree))
           (info (second tree))
           (splitree (mat-trans info))
           (signatures (first splitree))
           (measures (second splitree))
           (opt-meas (mapcar #'(lambda (x) (list (list 1 x))) measures))
           (withmes (mapcar #'(lambda (x y) (list x y)) signatures opt-meas)))
      (list header withmes))
    (let* ((signatures (first tree))
           (measures (second tree))
           (opt-meas (list (list 1 measures))) 
           )
      (list signatures opt-meas))))



;In Om all 5 and 7 etc.. are transformed by voice and numtree function
; in (4 1.0) as tied notes. If we need to permut trees as expressed 
; i.e 5 and not (4 1.0) use reduce mode. (here we use reducetree function!)
; and finally remove this option and put it by default in reduce mode
; for accurate computation!


(om::defmethod! subst-rhythm ((tree t) 
                              (pos list)
                              (elem list)
                              &optional 
                              (option 'reduce)
                              (output 'optimized))
   :initvals '((? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) nil (1 2) reduce optimized)
   :indoc '("a rhythm tree" "list of positions" "list of new items" "option" "output")
   :menuins '((3 (("reduce" 'reduce) 
                 ("tied" 'tied)))
              (4 (("optimized" 'optimized) 
                  ("not optimized" 'not-optimized))))
   :icon 225
   :doc "
Substitutes elements in <tree>.
<elem> input is a list that can accept either atoms or lists  or both.
Atoms are pulses. Lists will be proportions
creating groups . For example a list (1 1 1) substituting 2 will yield (2 (1 1 1)).
<pos> if left nil will substitute from 0 position until the end of list <elem>. 
If the positions are specified (a list) each nth elem of tree being pulses will be replaced
sequentially by elements from <elem>."
   (setf n -1)
   (let* ((liste (if (typep tree 'voice) 
                   (if (equal option 'reduce) (reducetree (tree tree)) (tree tree))
                   (if (equal option 'reduce) (reducetree (numtree tree)) (numtree tree))))
          (position (if (null pos)
                      (loop for i from 0 to (1- (length elem))
                            collect i) (first-n pos (length elem) )))
          (tree2obj (trans-tree-index liste))
          (tree2objclean (remove-if 'numberp (flat tree2obj)))
          (treeobjinverted 
           (substreeall tree2objclean position elem)))
     (case output
       (optimized (optimize-tree (trans-obj tree2obj)))
       (not-optimized (trans-obj tree2obj)))))




;;;;;;;;;;;;;;;;;;;;;;;;;INVERT-RHYTHM;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun transform (list)
"traces a simple list coming from trans-tree and flattened according to:
if note encoutered, then floats are checked and transformed into rests, else
rests encoutered and either other rests or errounous floats are transformed 
into notes. From Gerard."
    (loop while list
        for courant =  (pop list)
        ;do (print (mapcar 'tvalue list))
        ;do (print (tvalue courant))
        do (cond
            ((and   (integerp (tvalue courant)) (plusp (tvalue courant)))
             (setf (tvalue courant) (- (tvalue courant)))
             (loop while (and list (floatp (tvalue (car list))))
                   do (setf (tvalue (car list)) (round (- (tvalue (car list)))))
                    (pop list)))
            ((and (integerp (tvalue courant)) (minusp (tvalue courant)))
             (setf  (tvalue courant) (- (tvalue courant)))
             (loop while (and list (not (and (integerp (tvalue (first list))) (plusp (tvalue (first list))))))
                   do (setf (tvalue (car list)) (float (abs (tvalue (car list)))))
                   (pop list))))))



(om::defmethod! invert-rhythm ((tree t))
   :initvals '((? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
   :indoc '("a rhythm tree")
   :icon 225
   :doc "
Inverts <tree> : every note becomes a rest and every rest becomes a note.
"
(let* ((liste (if (typep tree 'voice) (tree tree) (resolve-? tree)))
       (tree2obj (trans-tree liste))
       (tree2objclean (remove-if 'numberp (flat tree2obj)))
       (treeobjinverted (transform tree2objclean)))
  (trans-obj tree2obj)))




;;;;;;;;;;;;;;;;;;;;;;;;;tree2ratio;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;from jean bresson

(defun tree-to-ratios (list)
  (loop for mesure in (cadr list) collect
        (let* ((signature (car mesure))
               (vals (cadr mesure))
               (ratios (mesure-ratios vals)))
          (om/
           (om* ratios (car signature))
           (cadr signature)))))

(defun mesure-ratios (list)
 (let ((div (round (loop for elt in list sum (abs (if (listp elt) (car elt) elt))))))
    (flat (loop for elt in list 
                collect (if (listp elt)
                          (om* (/ (round (car elt)) div) (mesure-ratios (cadr elt)))
                          (/ (round elt) div)))
)))



;;this is to get the floats working correctly
;;this is me
(defun get-s-from-meas (tree)
  (if (atom tree)
    tree
   (mapcar 'get-s-from-meas (second tree))))


(defun get-s-by-mes (tree)
  (loop 
    for elt in (cadr tree)
    collect (flat (get-s-from-meas elt))))


(defun correct-measurefloats (tree)
  (let* (res
         (pulses (flat (get-s-by-mes tree)))
         (ratios (flat (tree-to-ratios tree))))
    (loop for p in pulses do 
          (if (floatp p)
              ;;; tie with previous ratio
              (if res 
                  ;; not the first pulse
                  (setf (car res) (+ (or (car res) 0) (pop ratios)))
                ;; first pulse
                (setf res (list (- (pop ratios)))))
            ;;; new ratio
            (push (pop ratios) res)
            ))
    (reverse res)))




(om::defmethod! tree2ratio ((tree t))
  :initvals '((? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))))
  :indoc '("a rythm tree")
  :icon 254
  :doc "
Converts <tree> into a list of ratio where 
1/4 is a quarter note, 1/8 is an eight note etc.
"
  (correct-measurefloats tree))


;;;;;;;;;;;;;;;;;;;;;;;;;ROTATE-TREE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;rotate-by measure la ne veut rien dire:
;;example qud on a une seconde mesure commencant par une liaison 
;;donc qui appartien t a la premiere mesure on a un probleme!!!

(om::defmethod! rotatetreepulse ((tree t) (nth integer))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) 1)
   :indoc '("tree" "nth")
   :icon 225
     :doc "Returns a circular permutation of a copy of <tree> durations starting from its <nth> 
element"

   (let* ((ratios (tree2ratio tree))
          (signatures (mapcar 'car (cadr tree)))
          (rotation (rotate ratios nth)))
     (mktree rotation signatures)))



(defun get-all-treeobj (tree)
  (remove nil      
          (mapcar #'(lambda (x) 
                      (if (typep x 'treeobj) x))
                  (flat tree))))



(defun permtree (list nth)
(let* ((listobj (rotate list nth))
       (values (mapcar 'tvalue listobj))
       )

  (loop 
    for i from 0 to (1- (length listobj))
    for a in values
    collect (setf (tvalue (nth i list)) a))))




;In Om all 5 and 7 etc.. are transformed by voice and numtree function
; in (4 1.0) as tied notes. If we need to permut trees as expressed 
; i.e 5 and not (4 1.0) use reduce mode. (here we use reducetree function!)
; and finally remove this option and put it by default in reduce mode
; for accurate computation!



(om::defmethod! rotate-tree ((tree t) (nth integer))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) 1)
   :indoc '("tree" "nth")
   :icon 254
   :doc "rotates tree following nth."
   (setf n -1)
   (let* ((tree2obj (trans-tree-index tree))
          (tree2objclean (get-all-treeobj tree2obj))
          (treepermute (permtree tree2objclean nth)))
     (trans-obj tree2obj)))



(defmethod! rotatetree ((tree t) (n t) 
                        &optional (mode 'pulse))
   :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1)))) 1 pulse)
   :indoc '("a rhythm tree" "a number" "rotation mode")
   :menuins '((2 (("pulse" 'pulse) 
                 ("prop" 'prop))))
             
   :icon 225
   :doc " 
Applies a rotation of <n> positions to the pulses in <tree>.

<mode> = 'pulse' : applies to pulses (expressed durations).
<mode> = 'prop' : applies to the tree values.
"
     (if (equal mode 'pulse)
       (rotatetreepulse tree n)
       (rotate-tree tree n)))
       

;;;;;;;;;;;;;;;;;;;;;;;;;TREE-CANON;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|

;;pas pour le moment !!!!

(om::defmethod! tree-canon ((tree t) (unite number) (nbr number))
   :initvals '('(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1)))) 1/4 1)
   :indoc '("tree" "ratio" "nth-times")
   :icon 225
   :doc "decale le rythm de <self> de n fois donne en <nbr>
d'unitees rythmiques donne en <unite>. En sachant que :
Une ronde -> 1
Une blanche -> 1/2
Une noire -> 1/4
Une croche -> 1/8
etc.....
"
   (let* ((measures (car (mapcar 'car (cadr tree))))
          (ratios (tree2ratio tree))
          (offset (om* -1 (apply '+ (repeat-n unite nbr))))
          (decal (x-append offset ratios)))
     (mktree decal measures)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;MAKE-TREE-GROUPS;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun grplst (lst grp)
  (let* ((grouped (loop 
                    for a in grp
                    collect (prog1
                              (first-n lst  a)
                              (setf lst (nthcdr a lst))))))
    (remove nil (if lst (x-append grouped (list lst)) grouped))))


(defmethod! maketreegroups ((figures list)
                            (sequence list)
                            (measures list))
   :initvals (list '((1 1 1) (1 2 1) (3 4) (1 1 1 1))
                   '(0 3 0 2 0 1 0 0 3) 
                   '((4 4)))
 :indoc '("rhythm figures" "control sequence" "list of time signatures")
 :icon 225
 :doc "
Builds a Rhythm Tree starting from a a list of rhythmic figures (<figures>).

<sequence> is a list of index used to pick items in <figures>. 
An inconsistent index (> than the length of <figures>) will produce a rest. 

<measures> is a list of time signatures used to build the RT.
"
 (let* ((mesures
         (let* ((dernieremesure (last-elem measures))
                (lgtseq (length sequence))
                (lgtbeats (apply '+ (mapcar 'car measures))))
           (if (> lgtseq lgtbeats)
             (x-append measures (repeat-n dernieremesure
                                          (ceiling (/ (- lgtseq lgtbeats) 
                                                      (car dernieremesure)))))
             measures)))
        (num (mapcar 'car mesures))
        (denom (mapcar 'cadr mesures))
        (pos (posn-match figures sequence))
        (donnes (loop for i in pos
                      collect 
                      (if (null i) -1 (list 1 i))))
        (groupment (grplst donnes num)))
     (list '? (loop
                for i in num
                for j in denom
                for a in groupment
                collect 
                (if (< (length a) i)
                  (list (list i j) (x-append  a (* -1 (- i (length a)))))
                  (list (list i j) a))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;Utils;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;GET-SIGNATURES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defmethod! get-signatures ((tree list))
   :icon 225
   :indoc '("a rhythm tree, voice or poly")
   :initvals (list  '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))) 
   :doc "
Returns the list of time signatures in the rhythm tree (<tree>).
"
   (mapcar #'first (cadr tree)))

(defmethod! get-signatures  ((self voice))
   (let* ((tree (tree self)))
     (get-signatures  tree)))

(defmethod! get-signatures  ((self poly))
   (let* ((voices (inside self)))
     (loop for i in voices
     collect (get-signatures i))))



;;;;;;;;;;;;;;;;;;;;;;;;;GET-PULSE-PLACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(om::defmethod! get-pulse-places ((tree list))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("a rhythm tree")
  :icon 225
  :doc "
Returns the positions of the pulses in <tree>.
"
  (let* (res 
         (n 0) 
         (puls (group-pulses tree))
         (places (loop 
                   for i in puls
                   do (if (plusp (car i))
                        (progn 
                          (push n res)
                          (incf n))
                        (incf n)))))
    (reverse res)))




(om::defmethod! get-pulse-places ((self voice))
  (let ((tree (tree self)))
    (get-pulse-places tree)))



;;;;;;;;;;;;;;;;;;;;;;;;;GET-REST-PLACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(om::defmethod! get-rest-places ((tree list))
  :initvals '((? (((4 4) (1 (1 (1 2 1 1)) 1 1)) ((4 4) (1 (1 (1 2 1 1)) -1 1))))) 
  :indoc '("a rhythm tree")
  :icon 225
  :doc "
Returns the positions of the rests in <tree>.
"
  (let* (res 
         (n 0) 
         (puls (group-pulses tree))
         (places (loop 
                   for i in puls
                   do (if (minusp (car i))
                        (progn 
                          (push n res)
                          (incf n))
                        (incf n)))))
    (reverse res)))



(om::defmethod! get-rest-places ((self voice))
  (let ((tree (tree self)))
    (get-rest-places tree)))







