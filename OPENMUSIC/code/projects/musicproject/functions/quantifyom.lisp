  
;;;
;;; Quantizer. Taken from PW
;;; CR [Jan/30/98]
;;;

(in-package :om)


(defvar *distance-weight* nil)
(defvar *valid-rtm-expands* '(#\/))
(defvar *maximum-pulses* ())
(setf *maximum-pulses* 32)
(defvar *max-division* 8)
(defvar *min-pulses* 5)
(defvar *forbidden-rythmic-divisions* ())
(defvar *distance-function* ())


;(setf *read-default-float-format* 'single-float)
;(setf *read-default-float-format* 'double-float)
;(setf a (coerce (/ 9 3.0) 'double-float))
;(/ (/ 100 a) 1.0)
  

(defvar *min-percent* 0.6)
(defvar *tempo-scalers* '(1 2 3 4 5 6 7 8))
(defvar *min-tempo* 40)
(defvar *max-tempo* 200)
(defvar *accum-error* 0)

(defvar *minimum-quant-dur* ())
(setf *minimum-quant-dur* (/ 100 16))

(defvar *unquantized-notes* 0)
(defvar *global-grace-notes* ())

;;quantizing parameters
(defvar *unit-division-hierarchy*
  '(1 2 4 3 6 5 8 7 10 12 16 9 14 11 13 15 17 18 19 20 21 22 23 24 25 26 27
    28 29 30 31 32
    33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50))
(defvar *iota* 0.0001)
(defvar *dist-iota* 0.03)
(defvar *proportions-iota* 0.001)

(defmethod* om::OMquantify ((durs list) (tempi t) (measures list)
                            (max/ t)
                            &optional
                            forbid
                            offset
                            precis)
  :initvals '((100) 60 (4 4) 8 nil 0 0.5)
  :icon 252
  :indoc '("durations (list)" "tempo" "list of time signature(s)" "maximum subdivision"  "list forbidden subdivisions" "grace-notes?" "precision (0.0-1.0)")
  :doc "Quantizes a list of durs (1000 = 1 sec.) into a metric structure.
The result is a rhythm tree to be connected to the <tree> input of a voice.

<durs>   is a list of durations in 1/1000th of a second.
<tempi>  is a tempo
<measures> is a list of measure signature (e.g. ((3 4) (7 8) (1 4) ... ) )
<max/>   is the maximum  subdivision of the beat (8 with beat=quarter means 32nd note)
<forbid> is a list of forbidden subdivision of the beat
<offset> is an offset expressed as a ratio of the beat
<precis> is a float in (0.0 .. 1.0) smaller values mean 'simplicity' while bigger values mean 'precision'

<forbid>

With this variable, you can control the subdivisions of the beats, either by
avoiding them or by imposing them, at a global level or at the beat level.

A simple list, such as ( 11 9 7 6), does not permit at a global level
divisions by 11, 9, 7, or 6. The introduction of sub-lists at the first level indicates
a control over the measures. For example,  ((5 6) (7 4) () () (4 7)) indicates that
in the first
measure, subdivisions by 5 and by 6 are forbidden, and in the second and fifth
measure, subdivisions by 7 and by 4 are forbidden. As the third and fourth sub-
lists are empty lists, there are no restrictions for these measures. A second
level of sub-lists will permit to control subdivisions of beats.
The list ( ((5 4) () (3 6) ())  (() () ( 8 7) ())  ( 3 2)  ()  )  
indicates :

first measure
        first beat - subdivisions by 5 and by 4 forbidden
        second beat : no restriction
        third beat : subdivisions by 3 and by 6 forbidden
        fourth beat : no restriction

second measure
        first beat :  no restrictions
        second beat : no restrictions
        third beat : subdivisions by 8 and by 7 forbidden
        fourth beat : no restrictions

third measure
        all beats : subdivisions by 3 and by 2 forbidden

fourth measure
        all beats : no restrictions

To impose subdivisions, you add a !  at the beginning of the lists.

At a global level

(! 5)           imposes a subdivision by five on the entire sequence
(! 5 7 6)       imposes a subdivision by 5, by 7, or by 6 on the entire sequence.

The syntax is the same for all other levels:

For measures

((!  3 4) (! 5) () () ())

and for beats

(   ((! 5 4) () (!  3 6) ())  (() () ( ! 8 7) ())  (!  3 2)  ()  ) .

Of course, it is possible to mix syntaxes at the measure level as well as
at the beat level. Here is an example:
(   (( 5 4) () (!  3 6) ())  ((! 6) () (  8 7) ())  (!  3 2)  (6 8)  )"

  (unless precis (setq precis 0.5))
  (setf *distance-weight*  (if (numberp precis) (list (list precis)) precis))
  ; (quant-edit durs tempi measures  (if (numberp max/) (list (list max/))
  ;                                     max/) forbid (or offset 0) 1)
  ;aaa 12-03-98 quantify in ms
  (let ((rep (quant-edit (om/ durs 10) tempi measures  (if (numberp max/) (list (list max/)) max/) (list! forbid) (or offset 0) 1)))
    (korrected-kant (reducetree rep))
  ))



;;;korrected-kant: kant correction by K. Haddad
;;;included in omquantify 21/10/2008 in OM 6.0.4
;;;the before last duration is mostly a rest followed by a continuation-chord
;;;this means that the continuation chord must be rest
(defun correctdashit (list)
  (loop for i in list collect (if (minusp (first i))
                                  (loop for truc in i
                                        collect (if (minusp truc)
                                                    (round truc)
                                                  (round (* -1 truc))))
                                i)))

(defun korrected-kant (tree)
  
  (let* ((liste (if (typep tree 'voice) (tree tree) (resolve-? tree)))
         (grp-pulse (group-pulses liste))
         (corrected (correctdashit grp-pulse))
         (tree2obj (trans-tree liste))
         (tree2objclean (remove-if 'numberp (flat tree2obj)))
         (fixed (loop for objs in tree2objclean
                      for i in (flat corrected)
                      do (setf (tvalue objs ) i))))
    
    (trans-obj tree2obj)))


(defun get-beat-duration (tempo unit)
  (coerce (/ 24000 tempo unit) 'double-float))

#|
(defun make-a-measure (voice-ms tempo sign-denom)
  (declare (ignore tempo))
  (list (read-from-string (format nil "~A//~A" (length voice-ms)
                                  sign-denom)) voice-ms))
|#

;new by gas 30/04/2001
(defun make-a-measure (voice-ms tempo sign-denom)
  (declare (ignore tempo))
  (list (read-from-string
         (format nil "~A//~A"
                 (reduce '+ voice-ms
                         :key #'(lambda (x) (if (numberp x) (round (abs x)) 1)))
                 sign-denom)) voice-ms))

(defun set-error (to from) (setf *accum-error* (- to from  *iota*)))
(defun reset-error () (setf *accum-error* 0))
(defun accum-error? () (plusp *accum-error*))
(defun get-accum-error () *accum-error*)

(defun select-tempo (durs)
  (declare (ignore durs))
  (warn "Automatic tempo option is no longuer available") '(60))

;TO set max to max - 1
(defun reduit-max (form)
  (if (numberp form) (+ form 1)
      (loop for item in form collect (reduit-max item))))

(defun quant-edit (durs tempi measures max/
                        &optional
                        (forbid nil)
                        (offset 0)
                        (autom 1))
  " Quantizes a list of <durs> (100 = 1 sec.) into the given measure(s),
with the given <tempi>.
<max/> is the maximum unit division that is taken to be a
significant duration.
A list of forbidden <forbid> unit divisions can optionally be
specified. The output is a list of
'measure-objects' that can be entered directly into an rtm-
box."
 (setf max/ (reduit-max max/))  ;by AAA
  (let* ((tempos (if (= autom 2) (select-tempo durs) (expand-lst (list! tempi))))   ;(expand-lists (list! tempi))))
         (measures-x (expand-lst measures))
         (measures (if (consp (car measures-x)) measures-x (list measures-x)))
         (def-measure (cons (first (car (last measures))) (second (car (last measures)))))
         (durs (if (zerop offset) durs (cons (- (* offset (get-beat-duration (car tempos) (second (car measures)))))
                                             durs)))
         (positive-durs (om-abs durs))
         (deftempo (car (last tempos)))
         (atimes (dx->x 0.0 positive-durs)) (c-time 0)
         result slur-fl current-tempo current-unit
         (max-list (and max/ (if (or (null (first max/)) (consp (first max/))) max/ (list max/))))
         (max-preci (and *distance-weight* (if (or (null (first *distance-weight*)) (consp (first *distance-weight*)))
                                             *distance-weight* (list *distance-weight*))))
         (forbids (and forbid (if (or (null (first forbid)) (consp (first forbid))) forbid (list forbid))))
         (*unquantized-notes* 0) (measure-number -1)
         (def-forbid (first (last forbids)))
         (def-max (first (last max/)))
         (def-preci (first (last *distance-weight*)))
         *global-grace-notes* old-silence (*max-division* max/))
    (reset-error)
    (do ()  ((null (cdr atimes)))			    ;dx->x above adds extra item
      (setf *max-division* (or (nth (+ 1 measure-number) max-list) def-max))
      (setf *distance-weight* (or (nth (+ 1 measure-number) max-preci) def-preci))
      (multiple-value-bind (beats times slur? current-time )
                           (get-rhythms atimes :tempo (setq current-tempo (or (pop tempos) deftempo))
                                        :sign (if (car measures)
                                                (cons (first (car measures))
                                                      (setq current-unit (second (pop measures))))
                                                (progn (setq current-unit (cdr def-measure)) def-measure))
                                        :start-time c-time :forbid (or (nth (incf measure-number) forbids) def-forbid)
                                        :old-slur? slur-fl)
        (setq atimes times c-time current-time)
        (setq slur-fl slur?)
        (if beats
          (multiple-value-bind (beats-with-silences modifs new-silence)
                               (put-in-silences beats durs old-silence)
            (setq old-silence new-silence)
            (when beats-with-silences
              (push (make-a-measure  beats-with-silences current-tempo current-unit) result)       
              (setq durs (nthcdr modifs durs))))
          (setq atimes nil))))
    (unless (zerop *unquantized-notes*)
      (if result
        (when *om-verbose* 
          (format *om-stream*
                  "Warning: with the given constraints, ~D notes are lost while quantizing ~%"
                  *unquantized-notes*))
        (om-beep-msg "cannot quantize with the given constraints"))
      )
    (setq result (nreverse result))
    ;;(unless (zerop *unquantized-notes*)
    ;; (and result (setf (pw::extra-measure-stuff (car result))
    ;;                   (set-grace-notes-pos *global-grace-notes* positive-durs))))
    ;;(make-instance 'pw::C-measure-line :measures result)
    (list '?  result)))

;;;;;;;;;;;;;;;;;===============================================
(defun set-grace-notes-pos (grace-notes durs)
  (let ((atimes (om-round (dx->x 0 durs) 1)) (count -1))
    (mapcar #'(lambda (pair) (cons (- (position (om-round (first pair) 1)
                                                atimes :test #'=) (incf count))
                                   (cdr pair)))
            (sort grace-notes '< :key #'first))))
;;;;;;;;;;;;;;;================================================


;(put-in-silences '((1 (1)) (1 (1.0 1))) '(-10 20 20))

(defun put-in-silences (beats durs &optional prev-silence)
  (if (every #'plusp durs)
    (values beats 0 nil)
    (let ((count 0) new-silence)
      (labels ((loop-beats (beat-tree)
                 (cond ((null beat-tree) beat-tree)
                       ((consp (car beat-tree))
                        (cons  (list (caar beat-tree) (loop-beats (cadar beat-tree)))
                               (loop-beats (cdr beat-tree))))
                       ((not (integerp (car beat-tree)))
                        (if (or (and (not (zerop count)) (silence? durs (1- count)))
                                (and (zerop count) prev-silence))
                          (progn (setq prev-silence t new-silence t)
                                 (cons (- (truncate (car beat-tree)))
                                       (loop-beats (cdr beat-tree))))
                          (progn (setq prev-silence nil new-silence nil)
                                 (cons (car beat-tree) (loop-beats (cdr beat-tree))))))
                       ((silence? durs count)
                        (incf count) (setq new-silence t)
                        (cons (- (car beat-tree)) (loop-beats (cdr beat-tree))))
                       (t (incf count) (setq new-silence nil)
                          (cons (car beat-tree) (loop-beats (cdr  beat-tree)))))))
        (values (rearrange-silences (loop-beats  beats)) count new-silence)))))

(defun silence? (durs position) (or (not (nth position durs)) (minusp (nth position durs))))

(defun rearrange-silences (beats)
  (let (res ok new-beats)
    (labels ((loop-beats (beat-tree)
               (cond ((null beat-tree) beat-tree)
                     ((consp (car beat-tree))
                      (setq new-beats (loop-beats (cadar beat-tree)))
                      (if (and (every 'numberp new-beats) (every 'minusp new-beats))
                        (cons  (- (caar beat-tree)) (loop-beats (cdr  beat-tree)))
                        (cons  (list (caar beat-tree) new-beats)
                               (loop-beats (cdr beat-tree)))))
                     ((every 'numberp beat-tree)
                      (setq ok nil res nil)
                      (if (every 'minusp beat-tree)
                        beat-tree
                        (dolist (item beat-tree (nreverse (if res (push
                                                                   (apply '+ res) ok) ok)))
                          (if (minusp item)
                            (push item res)
                            (if res (progn (push (apply '+ res) ok) (push
                                                                     item ok) (setq res nil))
                                (push item ok))))))
                     (t (cons (car beat-tree) (loop-beats (cdr beat-tree)))))))
      (loop-beats beats))))

;(setf *read-default-float-format* 'single-float)
;(setf *read-default-float-format* 'double-float)
;(/ 1 (coerce (/ 1 240) 'double-float))

(defun get-rhythms (notes &key (tempo 60) (sign '(4 . 4)) (nb-pulse-max 16)
                          start-time  old-slur? forbid)
  (declare (ignore nb-pulse-max))
  (let* ((measure-dur (* 24000.0 (car sign) (coerce (/ 1 (* (cdr sign) tempo)) 'double-float)))
         (atimes notes)
         (beat-dur (coerce (/ measure-dur (car sign)) 'double-float))
         (tmin start-time)
         (from-dur tmin) (to-dur  (+ beat-dur tmin))
         (measure-end (+ measure-dur start-time))
         (forbids (if (or (null (first forbid)) (consp (first forbid)))
                    forbid (list forbid)))
         (max-list (if (or (null (first *max-division*)) (consp (first
                                                                 *max-division*)))
                     *max-division* (list *max-division*)))
         (preci-list (if (or (null (first *distance-weight*)) (consp (first
                                                                      *distance-weight*)))
                       *distance-weight* (list *distance-weight*)))
         (default-max (first (last max-list)))
         (default-preci (first (last preci-list)))
         (default-forbid (first (last forbids)))
         partition beat-rythm-forms
         (i 0))
    ;
    (setf *distance-weight* (or (nth i (car preci-list)) (car (last
                                                               default-preci))))
    (setf *max-division* (coerce (or (nth i (car max-list)) (car (last default-max))) 'double-float))
    (setf *minimum-quant-dur* (/ 60 tempo *max-division*))
    ;
    (do () ((not (and (>= (- to-dur from-dur) *minimum-quant-dur*) atimes)))
      (setq partition
            (get-list-section atimes from-dur to-dur)
            atimes (nthcdr (length partition) atimes))
      (setq *forbidden-rythmic-divisions* (or (pop forbids) default-forbid))
      (multiple-value-bind (a-section slur? prev-slur)
                           (get-optimal-time-section partition to-dur
                                                     beat-dur from-dur old-slur?) ;;tmin?
        (if a-section
          (let ((chosen-rythm (simplify (search-rythm a-section from-dur
                                                      prev-slur))))  ;;tmin?
            (if chosen-rythm
              (progn (push chosen-rythm beat-rythm-forms)
                     (setq tmin (car (last a-section))))
              (progn (setq beat-rythm-forms nil atimes nil)
                     (om-print "cannot quantize with the given constraints")
                     (om-beep))
              ))
          )
        (setq old-slur? slur?)
        (psetq to-dur (min (+ to-dur beat-dur) measure-end) from-dur to-dur))
      ;
      (incf i)
      (setf *distance-weight* (or (nth i (car preci-list)) (car (last
                                                                 default-preci))))
      (setf *max-division* (coerce (or (nth i (car max-list)) (car (last default-max))) 'double-float))
      (setf *minimum-quant-dur* (/ 60 tempo *max-division*))
      ;
      )
    (when (not atimes)
      (when old-slur? (last-division-is-silence beat-rythm-forms) (setq
                                                                   old-slur? nil))
      (do () ((not (>= (- measure-end to-dur) *minimum-quant-dur*))) (push
                                                                      '(-1) beat-rythm-forms) (incf to-dur beat-dur)))
    (values (compound-beats (nreverse beat-rythm-forms)) atimes old-slur?
            to-dur beat-dur)))

(defun compound-beats (beat-list)
  (mapcar #'(lambda (beat) (if (null (cdr beat)) (car beat) (list 1 beat)))
          beat-list))

(defun last-division-is-silence (form)
  (let ((last-beat (last (first form))))
    (setf (first last-beat) (- (floor (first last-beat))))))

(defun quanti-of (quant-structure) (cdr quant-structure))
(defun pulses-of (quant-structure) (caar quant-structure))
(defun deleted-of (quant-structure) (second (first quant-structure)))

(defun search-rythm (a-section tmin prev-slur)
  "Finds the beat list structure corresponding to the quantized durations"
  (beat-structure a-section prev-slur tmin (first (last (quanti-of
                                                         a-section)))))

(defun beat-structure (quants slur? from to)
  (let* ((atimes (form-atimes-list (copy-list (quanti-of quants)) from to))
         (durs (remove 0.0 (x->dx atimes) :test #'=))
         (min-dur (/ (- (car (last atimes)) (first atimes)) (coerce (pulses-of quants) 'double-float)))
         (beats (and durs (remove 0
                                  (if slur?
                                    (cons (float (round (pop durs) min-dur))
                                          (mapcar #'(lambda (dur) (round
                                                                   dur min-dur)) durs))
                                    (mapcar #'(lambda (dur) (round dur
                                                                   min-dur)) durs)) :test #'=))))
    beats))

(defun form-atimes-list (times from to)
  (and times
       (if (= (first times) from)
         (if (= (first (last times)) to) times (nconc times (list to)))
         (cons from (if (= (first (last times)) to) times (nconc times
                                                                 (list to)))))))

(defun get-list-section (list from to)
  (let (result (epsilon -1e-3))
    (dolist (elem list)
      (cond ((>= (- elem to) epsilon) ;; GA 10/10/94 (> elem to)
             (return ()))
            ((>= (- elem from) epsilon) (push elem result))
            (t )))
    (nreverse result)))

(defun simplify (beats) (and beats (if (= (length beats) 1) (list (/ (first beats) (first beats))) beats)))

(defun get-optimal-time-section (list to beat-dur tmin prev-slur)
  "Given a beat duration span and an initial time (tmin), quantizes the
attack times in list. Beat duration
is beat-dur. Beat's onset time is tmin. Beat's end time is 'to'. If
prev-slur is on, the first
onset time of this beat should be slurred with the last one of the previous
beat. Prev-slur may change in
this function.If  Slur? is on, last onset of this beat should be linked
with first of next beat (i.e. slur?
becomes prev-slur in the next call)."
  ;;(reset-error)
  (let* ((atimes (and list  (test-quantize-constraints list tmin beat-dur
                                                       prev-slur)))
         (last-list (first (last list)))
         (q-list (and atimes (quanti-of atimes)))
         slur? lagging-count head end-surplus partition)
    (setq lagging-count (and (plusp tmin) q-list (not (= tmin (car q-list)))))
    (when (and  (accum-error?) (not lagging-count))   ;;note deleted at the border between beats
      (when list
        (keep-unquantized-statistics (or (first list) tmin) (- tmin
                                                               (get-accum-error))))
      (reset-error))
    (setq head (and q-list (if lagging-count (cons tmin q-list) (progn
                                                                  (setq prev-slur nil) q-list))))
    (setq end-surplus (and head (- to (first (last head)))))
    (setq partition
          (and end-surplus
               (if (> end-surplus 1e-4) ; GA 21/10/94 (plusp end-surplus)
                 (progn (reset-error) (setq slur? t) (nconc head (list to)))
                 (if (> to last-list) (progn (set-error to last-list) head)
                     head))))
    (cond
     ((null partition)
      (if (and list (not atimes))
        (progn
          (mapcar #'(lambda (time) (keep-unquantized-statistics to
                                                                time)) (butlast list 2))
          (setq prev-slur nil)
          (unless (or (= last-list to) (= last-list tmin) (not (rest
                                                                list)))
            (set-error to last-list))
          (values (test-quantize-constraints (list tmin to) tmin
                                             beat-dur prev-slur) t nil))
        (values (test-quantize-constraints (list tmin to) tmin beat-dur
                                           prev-slur) t prev-slur)))
     (t (setf (rest atimes) partition) (values atimes slur? prev-slur)))))

(defun keep-unquantized-statistics (atime previous)
  (incf *unquantized-notes*)
  (when atime (push (cons previous (- atime previous)) *global-grace-notes*)))

#|
(defun test-quantize-constraints (list tmin beat-dur prev-slur)
  (let ((q-structures (score-approx list tmin beat-dur)) result)
    (if
      (dolist (current-atimes q-structures nil)
        (unless (forbidden-structure (beat-structure current-atimes
prev-slur tmin (+ tmin beat-dur)))
          (return (progn (setq result (if (plusp (deleted-of current-atimes))
                                        (less-bad-quanta (list
current-atimes) list tmin beat-dur)
                                        current-atimes)) t))))
      result
      (less-bad-quanta q-structures list tmin beat-dur))))
|#

(defun test-quantize-constraints (list tmin beat-dur prev-slur)
  (let ((q-structures (score-approx list tmin beat-dur)))
    (dolist (current-atimes q-structures (less-bad-quanta q-structures list
                                                          tmin beat-dur))
      (unless (forbidden-structure (beat-structure current-atimes prev-slur
                                                   tmin (+ tmin beat-dur)))
        (return (if (plusp (deleted-of current-atimes))
                  (less-bad-quanta (list current-atimes) list tmin beat-dur)
                  current-atimes))
        ))))

(defun forbidden-structure (struct)
  (let ((division (apply '+ struct)))
    (or (> division *max-division*)
        (if (and (first *forbidden-rythmic-divisions*)
                 (symbolp (first *forbidden-rythmic-divisions*)) (string=
                                                                  (first *forbidden-rythmic-divisions*) '!))
          (not (member division (rest *forbidden-rythmic-divisions*) :test
                       #'=))
          (member division *forbidden-rythmic-divisions* :test #'=)))))

(defun get-nb-error (quant) (second (first quant)))
(defun get-distance (quant) (third (first quant)))
(defun get-proportion-distance (quant) (fourth (first quant)))
(defun get-nb-pulse (quant) (first (first quant)))

(defun pulse-number (qtime nb-pulse tmax tmin)
  (round (* nb-pulse (- qtime tmin)) (- tmax tmin)))
(defun get-all-pulse-nums (note tmax tmin)
  (mapcar #'(lambda (qtime) (pulse-number qtime (caar note) tmax tmin))
          (cdr note)))

(defun less-bad-quanta (q-structures times from dur)
  (let ((try (first (member 1 q-structures :test #'<= :key #'get-nb-error))))
    (when try (try-eliminating-one try times from dur))))

(defun try-eliminating-one (q-structure times from dur)
  (dolist (current-atimes
           (score-approx  (get-rid-of-duplicates (quanti-of q-structure)
                                                 times) from dur) nil)
    (unless (forbidden-structure (beat-structure current-atimes nil from (+
                                                                          from dur)))
      (return current-atimes))))

(defun get-rid-of-duplicates (x times)
  (cond ((not (rest x)) times)
        ((= (first x) (second x))
         (keep-unquantized-statistics (second times) (first times))
         (get-rid-of-duplicates (rest x) (rest times)))
        (t  (cons (first times) (get-rid-of-duplicates (rest x) (rest
                                                                 times))))))

(defun score-approx (times tmin  &optional (sec/black 1.0)
                           nb-pulse-max)
  (unless nb-pulse-max (setq nb-pulse-max (min *maximum-pulses*
                                               *max-division*)))
  (let ((option1 (sort (compute-quanta tmin (+ tmin sec/black)
                                       nb-pulse-max times)
                       #'quant-test1))
        (option2 (sort (compute-quanta tmin (+ tmin sec/black)
                                       nb-pulse-max times)
                       #'quant-test2)))
    (mapcar #'(lambda (item1 item2) (if (< (* (get-distance item1) (* 1.2
                                                                      (- 1 *distance-weight*)))
                                           (* (get-distance item2)
                                              *distance-weight*) )
                                      item1 item2)) option1 option2)))


(defun is-better (n-pulse1 n-pulse2)
  (< (position n-pulse1 *unit-division-hierarchy*) (position n-pulse2
                                                             *unit-division-hierarchy*)))

(defun quant-test1 (quant1 quant2)
  (cond ((< (get-nb-error quant1) (get-nb-error quant2)) t)     ;;1
        ((> (get-nb-error quant1) (get-nb-error quant2)) nil)
        
        ((and (is-better (get-nb-pulse quant1) (get-nb-pulse quant2))   ;;2
              (< (get-proportion-distance quant1) *dist-iota*)) t)
        ((< (get-proportion-distance quant1) (- (get-proportion-distance
                                                 quant2) *proportions-iota*)) t)
        ((> (get-proportion-distance quant1) (+ (get-proportion-distance
                                                 quant2) *proportions-iota*)) nil)))

(defun quant-test2 (quant1 quant2)
  (cond ((< (get-nb-error quant1) (get-nb-error quant2)) t)     ;;1
        ((> (get-nb-error quant1) (get-nb-error quant2)) nil)
        ((is-better (get-nb-pulse quant1) (get-nb-pulse quant2)) t)))

(defun compute-quanta (tmin tmax max-pulses attack-times)
  (let (quants quantized-times)
    (dolist (needed-pulses (needed-pulses max-pulses attack-times tmin
                                          tmax) (nreverse quants))
      (setq quantized-times (mapcar #'(lambda (time) (adjust-time-to-grid
                                                      time needed-pulses tmin tmax)) attack-times))
      (push
       (make-quanta needed-pulses (deletions quantized-times) (distance
                                                               attack-times quantized-times)
                    (proportion-distance attack-times quantized-times tmin
                                         tmax) quantized-times)
       quants))))

(defun make-quanta (pulses deletions euclidean-distance proportion-distance
                           quantized-times)
  (cons (list pulses deletions euclidean-distance proportion-distance)
        quantized-times))

(defun adjust-time-to-grid (time pulses tmin tmax)
  (+ tmin (* (- tmax tmin) (/ pulses) (round (* (- time tmin) pulses) (- tmax tmin)))))

(defun needed-pulses (nb-pulse-max attack-times tmin tmax)
  (if (not attack-times) '(1)
      (let* ((minimum-pulses (minimum-pulses attack-times tmin tmax))
             (pulses (arithm-ser minimum-pulses (1- nb-pulse-max) 1)))
        (unless pulses (setf pulses (list (1- nb-pulse-max))))
        (if nil ;; GA 10/10/94 (< (length pulses) *min-pulses*)
	    (arithm-ser minimum-pulses (1- (max nb-pulse-max (* 2 minimum-pulses))) 1)
	    pulses))))

(defun minimum-pulses (attack-times tmin tmax)
  (let ((deltas (* 3 (apply 'min (- tmax tmin) (x->dx (remove-duplicates attack-times)))))) ;truncate freaks if divisor=0
    (min *maximum-pulses*
	 (max 1 ;;;;;(1- (length attack-times))
	      (truncate  (- tmax tmin) deltas)))))


;;euclidean distance
(defun sqr (n) (* n n))
(defun distance (list qlist)
  (let ((accum 0.0) (length 0))
    (mapc #'(lambda (x y) (incf accum (sqr (- x y))) (incf length)) list qlist)
    (/ (sqrt accum) (max 1.0 (float length 1.0d0)))))

(defun deletions (quantized-times) (count 0 (x->dx quantized-times) :test #'=))

(defun proportion-distance (l ll tmin tmax)
  (win3 (remove 0 (x->dx (cons tmin (append l (list tmax)))) :test #'=)
        (remove 0 (x->dx (cons tmin (append ll (list tmax)))) :test #'=)))



;;;This is computer generated code from Joshua Finneberg's error measure patch [931026]
;;;function sort-list merged with om sort
(defun win3 (var1 var2)
  (apply '+
         (om^
          (let (A)
            (mapcar #'(lambda (B) (setf A B) (om/ (apply '+ A) (length A)))
                    (let (C D)
                      (mapcar #'(lambda (E F)
                                  (setf C E D F)
                                  (let (G)
                                    (mapcar #'(lambda (H) (setf G H) (om^ (apply '/
                                                                                 (sort-list G :test '>)) '3))
                                            (mat-trans (list C D)))))
                              (let (J)
                                (mapcar #'(lambda (K) (setf J K) (cond ((= (length J)
                                                                           1) J) (t (g-scaling/sum J '100))))
                                        (mapcar 'list
                                                (x-append  var1
                                                           (create-list  (om-abs (om-
                                                                                  (max (length var1) (length var2))
                                                                                  
                                                                                  (length var1))) 1)))))
                              (let (M)
                                (mapcar #'(lambda (N)
                                            (setf M N)
                                            (cond ((= (length M) 1) M)
                                                  (t (g-scaling/sum M '100))))
                                        (mapcar 'list
                                                (x-append var2
                                                          (create-list (om-abs (om-
                                                                                (max (length var1) (length var2))
                                                                                
                                                                                (length var2))) 1)))))))))
          3)))



;;;; jean
;;;  a faire sur un chord-seq avant que le quantifier 
;;;  met ds un meme accord les accords simultanés ouy séparés de - de delta
(defmethod merge-chords ((self chord-seq) &optional (delta 0))
  (let ((len (length (inside self)))
        tmpchord tmplist)
    (loop for i = 0 then (+ i 1) while (< i len) do
          (setf tmpchord (nth i (inside self)))
          (setf tmplist (remove nil (loop for c in (nthcdr (+ i 1) (inside self)) 
                                        collect (when (<= (abs (- (offset->ms c) (offset->ms tmpchord))) delta) c))))
          (union-a-chs self (push tmpchord tmplist))
          (setf len (length (inside self))))
    self))
                    


