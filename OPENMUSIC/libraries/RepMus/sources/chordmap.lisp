(in-package :om)

;;
;;
;;            Librairie RepMus
;;
;;            Gerard Assayag, Claudy Malherbe  © IRCAM 1996
           

 

(defun rm-ambitus (chord)
  (abs (- (first (first (last chord))) (first (first chord)))))

(defun register (chord)
  (/ (apply #'+ (mapcar #'first chord)) (length chord)))

(defun distance-register (chord1 chord2)
  (/ (abs (- (register chord1) (register chord2))) 8400.0))

(defun distance-ambitus (chord1 chord2)
  (/ (abs (- (rm-ambitus chord1) (rm-ambitus chord2))) 8400))

(defun distance-nnotes (chord1 chord2)
  (abs (- (length chord1) (length chord2))))

(defun distance-freq-amp (chord1 chord2)
  (loop for (note1 amp1) in chord1
        for (note2 distance) = (closest-freq note1 amp1 chord2)
        ;collect (list note1 note2 distance) into notes
        sum distance into gdistance
        finally (return  (/ gdistance (length chord1)))))

(defun closest-freq (note amp chord)
  (loop 
    with distance = 10000
    with rnote = 0
    with ramp = 1
    for (note1 amp1) in chord
    ;for fdist = (mod (abs (- (approx-m note1 8)  (approx-m note 8))) 1200)
    ; normalize distance to 7 octaves = 1.
    for fdist =  (/ (abs (-  note1 note)) 8400)
    do (setf amp1 (max 1 amp1))
    if (< fdist distance) do (setf distance fdist rnote note1 ramp amp1)
    if (> fdist distance)  do (loop-finish)
    finally (return (list rnote (/ (* distance ramp) 127.0))))) 



(defun chord-distance (chord1 chord2 cf ca cr cn)
  ;(let ((chord1   (c-get-note-slots::get-note-slots chord1 '(midic vel)))
  ;      (chord2 (c-get-note-slots::get-note-slots chord2 '(midic vel))))
  (sqrt (+ (expt (* cf (distance-freq-amp chord1 chord2)) 2)
           (expt (* ca (distance-ambitus  chord1 chord2)) 2)
           (expt (* cr (distance-register  chord1 chord2)) 2)
           (expt (* cn (distance-nnotes  chord1 chord2)) 2))))


(defmethod! map-chords ((chs1 list)
                        (chs2  list)
                        (cf integer)
                        (ca integer) 
                        (cr integer) 
                        (cn integer) 
                        (approx integer) 
                        (penal integer))
  :icon 250
  :initvals '(() () 10 0 0 0 2 5)
  :indoc '("chords" "chords" "an integer"  "an integer"  "an integer"  "an integer"  "an integer"  "an integer"  )
  :doc "map-chords takes a list of chords as a model, and another list of chords as a
reservoir. Then it picks chords in the reservoir and it builds up a new sequence, 
trying to make that sequence look as much as possible like the model.
map-chords uses a euclidian distance measure between chords in the reservoir and
chords in the model. Dimensions used are : the number of common notes, the ambitus
(dist from the bottom to the to of the chord), the register (the gravity center of the chord),
the difference in the number of notes. The user has the ability to give a weighting
coefficient for any of these criteria thus influing on the resolution. If O the criterium is
totally ignored. Typical values are between 0 and 10.
There is also a penalty parameter for chord repetition : if this value is high, a chord cannot
be repeated in the sequence except if its first occurence is very far behind. Values typically
 between 0 (no penalty) and 10.
 
parameters

chs1 : a list of chords (chord-objects or midics) or a chord-seq This is the model.
chs2 : a list of chords (chord-objects or midics) or a chord-seq This is the reservoir.
cf : integer, coefficient for common notes criteria
ca : integer, coefficient for ambitus criteria
cr : integer, coefficient for register criteria
cn : integer, coefficient for number of notes criteria
approx: an integer between 1 and 16. Microtone approximation used in comparisons. 2 = 1/2tone.
penal : an integer >=0, penalty value for chord repetition

output : 

a chord-seq
"
(when (list-subtypep chs1 '(list)) (setf chs1 (mapcar #'(lambda (midics) (make-instance 'chord :lmidic midics)) chs1)))
(when (list-subtypep chs2 '(list)) (setf chs2 (mapcar #'(lambda (midics) (make-instance 'chord :lmidic midics)) chs2)))
(let ((chseq1 (mapcar #'(lambda (chord) (mat-trans (list (lmidic chord) (lvel chord)))) chs1))
      (chseq2  (mapcar #'(lambda (chord) (mat-trans (list (lmidic chord) (lvel chord)))) chs2)))
  (setf chseq1 (mapcar #'(lambda (chord)
                           (mapcar #'(lambda (note) (list (approx-m (first note) approx) (second note)))
                                   chord))
                       chseq1)
        chseq2 (mapcar #'(lambda (chord)
                           (mapcar #'(lambda (note) (list (approx-m (first note) approx) (second note)))
                                   chord))
                       chseq2))
    (loop
      for chord1 in chseq1
      for index1 from 0
      with index
      with used
      do (princ index1) (princ " ")
      do (loop for chord2 in chseq2
               for index2 from 0
               with gdistance = 1e+10
               for distance = (chord-distance chord1 chord2 cf ca cr cn)
               if (> penal 0.0) do (incf distance  (already-used-weight index2 used (* penal distance))) 
               if (< distance gdistance) 
               do (setf gdistance distance index index2) )
      do (push index used) 
      collect  (clone (nth index chs2) ))))


(defmethod! map-chords ((chs1 t)
                        (chs2 t)
                        (cf integer)
                        (ca integer) 
                        (cr integer) 
                        (cn integer) 
                        (approx integer) 
                        (penal integer))
  (unless (listp chs1) (setf chs1 (chords chs1)))
  (unless (listp chs2) (setf chs2 (chords chs2)))
  (map-chords  chs1  chs2 cf ca cr cn approx penal))





(defun already-used-weight (index list factor)
  (/ factor (1+ (or (position index list) (- 1e+5 1)))))





;(closest-freq 1000 100 '((250 100) (500 100) (900 100) (1050 100) (2000 100)))
;        
;(distance-freq-amp '((250 100) (500 100) (900 100) (1050 100) (2000 100))
;                   '((300.0 100.) (500 50) (900 25) ))

