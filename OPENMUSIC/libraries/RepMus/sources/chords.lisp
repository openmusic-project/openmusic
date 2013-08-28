(in-package :om)
 

;;; MUTATION CHSEQ->POLY autotransp

(defmethod chords->midics ((self chord-seq))
  (LMidic self))

(defmethod chords->midics ((self chord))
  (Lmidic self))

(defmethod chords->midics ((chords integer))
  chords)

(defmethod chords->midics ((chords list))
  (mapcar #'(lambda (chord) (chords->midics chord)) chords))

(defun do-mutation (lfreq1 lfreq2  l-ord g-ord inout)
  (let ((source (copy-list lfreq1)) (transitions (do-pairing lfreq1 lfreq2 l-ord g-ord inout)) mutations)
    (while transitions
           (let ((transition (pop transitions)))
             (cond
              ((zerop (second transition)) 
               (push (copy-list (setf lfreq1 (remove (first transition) lfreq1))) mutations))
              ((zerop (first transition))
               (push (copy-list (setf lfreq1 (sort (cons (second transition) lfreq1) '<))) mutations))
              ((/= (first transition) (second transition))
               (push (copy-list
                      (setf lfreq1
                            (sort (substitute (second transition) (first transition) lfreq1) '<)))
                     mutations))
              (t ))))
    (cons source (nreverse mutations))))

(defmethod! mutation ((chords chord-seq) &optional (inout 1))
  :icon 250
  :initvals '(nil 1)
  :indoc '("a sequence of chords" "mode")
  :menuins '((1 (("In" 1) ("Out" 2))))
  :doc " Computes a transition sequence between two or more chords.
'mutation' works differently from an interpolator : it generates a series
of small moves - take off a note here, add a note there, move a note here etc. -
that changes the first chord into the second. It does not introduce any note
other than the ones that are present in the chords. If given more than two chords
it generates a sequence with the transitional chords stuffed between the original
chords.

parameters : 

chords : a list of list of midics, or a list of chord-objects or a chord-line object.
inout : controls the order in which  notes are added and removed.

output : a series of chord in the form of a list of lists of midics.
"
  (let ((chords (chords->midics chords)))
    (unless (listp (first chords)) (setf chords (list chords)))
    (loop for chord in chords
          for next in (rest chords)
          for i from 0
          for mutation = (do-mutation chord next nil nil (eq inout 2))
          append (if (zerop i) mutation (rest mutation)))))



(defun do-pairing (lfreq1 lfreq2  l-ord g-ord inout)
  (labels ((f-local-order  (x y)
             (or (zerop x)
                 (and (not (zerop y))
                      (funcall (if  l-ord '> '<) x y))))
           (f-global-order (x y) (funcall (if g-ord '> '<) x y)))
    (let (l-transitions l-transition2 in-transition out-transition comp2
                        (intersection (intersection lfreq1 lfreq2)))
      (mapc #'(lambda (comp1)
                (let ((distance 0) transition (target lfreq2))
                  (while  target
                    (setf comp2 (pop target))
                    (unless (or (member comp1 intersection) (member comp2 intersection))
                      (setf distance  (abs (- comp2 comp1)))
                      (push  (list comp1 comp2  distance) transition)))
                  (when transition
                    (push (sort  transition  #'f-local-order :key 'third)
                          l-transitions))))
            lfreq1)
      (setf l-transitions
            (sort (nreverse l-transitions) #'f-global-order :key 'caddar))
      (let (forbidden a-transition)
        (dolist (transition l-transitions)
          (setf a-transition 
                (find  nil transition 
                       :test #'(lambda (x y) (declare (ignore x))
                                (not (member (second y) forbidden))) ))
          (cond
           (a-transition
            (push a-transition l-transition2)
            (push (second a-transition) forbidden))
           (t (unless (member (first (first transition)) lfreq2)
                (push (list (first (first transition)) 0 0) out-transition))))))
      (mapc #'(lambda (comp2)
                (unless (or (member comp2 l-transition2 :key 'second)
                            (member comp2 intersection))
                  (push (list 0 comp2 0) in-transition)))
            lfreq2)
      (append (if inout  out-transition in-transition)
              ;(sort l-transition2 #'f-order  :key 'third)
              (nreverse l-transition2)
              (if inout in-transition out-transition)))))



;=============
(defmethod! chseq->poly ((chseq chord-seq) (approx integer))
  :icon 250
  :initvals '(nil 2)
  :doc "Converts a sequence of chords in a pseudo polyphony where common notes
between two chords are changed into a single sustained note (harmonic link).

chseq is chord-seq object
approx (1, 2, 4, 8) tells the approximation used for finding common notes.

Outputs a chord-seq object"
  
  (let ((time 0)  prevch  poly vel-list del-list)
    (setf vel-list (Lvel chseq)
          del-list (x->dx (Lonset chseq))
          chseq (Lmidic chseq))
    (setf chseq (approx-m chseq approx)
          prevch (make-list (length (car chseq)) :initial-element 0))
    (mapc #'(lambda (curch curvels curdel)
              (mapc
               #'(lambda (n1 vel)
                   (if (not (memq n1 prevch))
                     (push (list n1 time curdel vel) poly)
                     (let ((event (member n1 poly :test #'(lambda (n e) (= n (car e))))))
                       (incf  (caddar event) curdel))))
               curch
               curvels)
              (incf time curdel)
              (setf prevch curch))
          chseq
          vel-list
          del-list)
    (setf poly (mat-trans (reverse poly)))
    (make-instance 'chord-seq
      :Lmidic (first poly)
      :Lonset (second poly)
      :Ldur (third poly)
      :Lvel (fourth poly))))

  

;;; AUTOTRANSPOSITIONS


(defun nth-auto-transp (chord n p)
  (let* ((note1 (nth n chord)) (note2 (nth p chord)) (offset (- note2 note1)))
    (mapcar #'(lambda (note) (- note offset)) chord)))

(defun all-auto-transp (chord &optional norep band)
  (let ((l-transp) (max (length chord)))
    (dotimes (n max)
      (dotimes (p max)
        ;(when (or (not norep) (or (and (zerop n) (zerop p)) (/= n p)))
        (let ((auto (nth-auto-transp chord n p)))
          (when band
            (setf auto (band-filter auto band 'pass)))
          (when (>= (length auto) (- (length chord) 1))
            ;(= (length auto) (length chord) )
            (push  auto l-transp)))))
    (when norep
      (setf l-transp (remove-duplicates l-transp :test 'equal)))
    (nreverse l-transp)))



(defun nth-modal-auto-transp (l-partials n p)
  (let* ((f1 (nth n l-partials)) (f2 (nth p l-partials))
         (ratio  (/ f1 f2)))
    (mapcar #'(lambda (partial)  (round (* partial ratio)))
            l-partials)))
  

(defun all-modal-auto-transp (l-partials &optional norep)
  (let ((l-transp) (max (length l-partials)))
    (dotimes (n max)
      (dotimes (p max)
        (when (or (not norep) (or (and (zerop n) (zerop p)) (/= n p)))
          (push (nth-modal-auto-transp l-partials n p)
                l-transp))))
    (if norep (remove-duplicates (nreverse l-transp) :test 'equal)
        (nreverse l-transp))))



(defun ch-auto-transp (chord &optional band)
  (all-auto-transp chord t band))

(defun p-auto-transp (l-partials fund approx &optional modal?)
  (cond (modal?
         (mapcar 
          #'(lambda (chord) (remove-duplicates chord :test '=))
          (f->mc  
           (om* (all-modal-auto-transp l-partials t) 
                    (mc->f fund))
           approx)))
        (t
         (all-auto-transp
          (f->mc (om* l-partials (mc->f fund)) approx)
          t))))

(defun auto-seq (l-chords)
  (let* ((flat (flat (copy-list l-chords)))
         (ambitus (list (apply 'min flat) (apply 'max flat)))
         (l-autos (mapcar #'(lambda (chord)
                              (ch-auto-transp chord  ambitus))
                          l-chords)))
    (append (flat (mapcar 'permut-random l-autos) 1) (last l-chords))))


(defun auto-transp (l-chords modal? fund band?)
  (cond
   ((not modal?)
    (let* (;(flat (flat (copy-list l-chords)))
           ;(ambitus (list (apply 'min flat) (apply 'max flat)))
           (l-autos (mapcar #'(lambda (chord)
                                (ch-auto-transp chord  ()))
                            l-chords))
           (res l-autos))
      (if band? (band-filter (flat res 1) band? 'pass) (flat res 1))))
   (modal?
    (let* ((fund (mc->f fund))
           ;(flat (flat (copy-list l-chords)))
           ;(ambitus (list (apply 'min flat) (apply 'max flat)))
           (l-npart  (mapcar #'(lambda (chord)
                                 (mapcar #'(lambda (midic) (round (/ (mc->f midic) fund)))
                                         chord))
                             l-chords))
           (l-autos  (mapcar #'(lambda (npart) (all-modal-auto-transp npart t)) l-npart))
           (res  (mapcar #'(lambda (subseq)
                             (mapcar #'(lambda (npart)
                                         (mapcar #'(lambda (part) (f->mc (* part fund))) npart))
                                     subseq))
                         l-autos)))
      (if band? (band-filter (flat res 1) band? 'pass) (flat res 1))))))



(defmethod! autotransp ((chords t)
                        &optional 
                        (band '(0 12700))
                        (mode 1)
                        (fund 2400)  ) 

  :indoc '("a chord or a chord sequence" "a midics band-list" "mode" "a midic")
  :initvals '((6000) (0 12700) 1 2400)
  :icon 250 
  :menuins '( (2 ( ("Chromatic" 1) ("Spectral" 2) )))
  :doc
"
Takes a chord or a series of chords and builds the auto-transposition of
these chords. The auto-transposition of a chord is a set of chords resulting
from transpositions of that chord, such that any note of the resulting chord is
made equal to any note of the original chord. There is also a 'spectral' mode
where all the notes in the transpositions are approximed to a harmonic partial
of a fundamental that is specified.
If you specify a series of chords, autotransp will build the transposition
set for every chords and put all the results in sequence.

parameters

chords : a list of midics, or a chord-object, or a list of these, or a chord-line
band : (optional) a list of 2 midics, to limit the pitches down and upwards
mode : (optional, menu) if 'chrom' normal transposition, if 'spec' spectral transposition
fund: (optional, midic) gives a fundamental if in 'spec' mode.

output : 

a list of list of midics
"
  (let ((chords (chords->midics chords))
        (band (flat (chords->midics band))))
    (when (numberp (first chords))
      (setf chords (list chords)))
    (auto-transp chords (= mode 2) (if (listp fund) (car fund) fund) (list (list (apply #'min band) (apply #'max band))))) )




