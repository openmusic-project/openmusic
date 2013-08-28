;;
;;            Librairie RepMus
;;
;;            Gerard Assayag, Claudy Malherbe, Joshua Fineberg, Peter Hanappe  © IRCAM 1996
           


(in-package :om)

(defclass partial ()
  ((ponset :accessor ponset :initarg :ponset)
   (outset :accessor outset :initarg :outset)
   (frequency :accessor frequency :initarg :frequency)
   (amplitude :accessor amplitude :initarg :amplitude)
))

(defclass partial-set ()
  ((partials :accessor partials :initarg :partials)
   (inter-onsets :accessor inter-onsets :initarg :inter-onsets)
   (chord-delta :accessor chord-delta :initarg :chord-delta)
)) 

(defmethod pduration ((partial partial))
  (- (outset partial) (ponset partial)))

(defmethod round-time ((partial partial))
  (setf (ponset partial)  (round (* 100  (ponset partial))))
  (setf (outset partial) (max (ponset partial) (round (* 100 (outset partial)))))
  partial)

(defmethod move-onset ((partial partial) onset)
  (setf (outset partial) (+ (outset partial) (- (ponset partial) onset))
        (ponset partial) onset)
  partial)

(defmethod set-inter-onset ((partial-set partial-set))
  (setf (inter-onsets partial-set)
        (loop for partial1 in (partials partial-set)
              for partial2 in (rest (partials partial-set))
              collect (- (ponset partial2) (ponset partial1))))
  partial-set)

(defmethod round-time ((partial-set partial-set))
  (loop
    for partial in (partials partial-set)
    with window-left = 0 and window-right = (chord-delta partial-set)
    if (< (ponset partial) window-right) do
    (move-onset partial window-left)
    else do (setf window-left (ponset partial)
                  window-right (+ window-left (chord-delta partial-set)))
    )
  partial-set)


(defun mk-partial-set (analyse delta vmin vmax fmin fmax)
  (when (consp (first analyse)) (setf analyse (first analyse)))
  (unless (string= (symbol-name (pop analyse)) "PARTIALS")
    (error "This is not a spectral analysis"))
  (let ((nbpartials (pop analyse)))
    (setf analyse
          (mapcar #'(lambda (partial)
                      (pop partial)
                      (loop with nbpoints = (pop partial)
                            with first-date = (first partial)
                            with last-date 
                            for date in partial by #'cdddr
                            for freq in (cdr partial) by #'cdddr
                            for amp in (cddr partial) by #'cdddr
                            do (setf last-date date) 
                            sum freq into freq-sum
                            sum amp into amp-sum
                            finally (return (list first-date last-date 
                                                  (/ freq-sum nbpoints) 
                                                  (/ amp-sum nbpoints)))))
                  analyse))
    (setf analyse (mat-trans analyse)) 
    (setf (fourth analyse)
          (om-round 
           (om-scale (mapcar #'(lambda (x) (* x (exp (* x 2))))
                             (om-scale (fourth analyse) 0.0 1.0))
                     vmin vmax)))
    (let
      ((partial-set
        (make-instance 'partial-set
          :chord-delta delta
          :partials
          (apply  #'mapcar
                  #'(lambda (onset outset freq amp)
                      (round-time (make-instance 'partial :ponset onset :outset outset :frequency freq
                                                 :amplitude amp)))
                  analyse))))
      (setf (partials partial-set)
            (loop for partial in (partials partial-set)
                  when (<= fmin (frequency partial) fmax)
                  collect partial))
      (round-time partial-set)
      (set-inter-onset partial-set)
      partial-set)))

(defun partials->chords (partial-set approx npoly)
  (and (partials partial-set)
       (let ((partial-list (list (list (first (partials partial-set)))))
             (chord-list) )          
         (loop for partial in (rest (partials partial-set))
               for inter-onset in (inter-onsets partial-set)
               if (zerop inter-onset) do
               (push partial (first partial-list))
               else do (push (list partial) partial-list))
         (setf partial-list (reverse partial-list))
         (setf chord-list
           (mapcar #'(lambda (partials)
                             (setf partials (reduce-partials partials approx npoly))
                             (make-instance 'chord
                                 :LMidic (f->mc (mapcar #'frequency partials))
                                 :LDur (om* 10 (mapcar #'pduration partials))
                                 :LVel (mapcar #'amplitude partials)))
               partial-list))
         (make-instance 'chord-seq
           :lmidic chord-list
           :lonset  (om* 10 (mapcar #'(lambda ( partials) (ponset (first partials)))  partial-list))) )))
         
  

(defun reduce-partials (partials approx npoly)
  (let ((pbuf ()) (partials (sort partials #'(lambda (p1 p2) (< (frequency p1) (frequency p2))))))
    (loop for partial in partials
          if (and pbuf 
                  (= (approx-m (f->mc (frequency partial)) approx)
                     (approx-m (f->mc (frequency (first pbuf))) approx))) do
          (setf (amplitude (first pbuf)) (max (amplitude (first pbuf)) (amplitude partial))
                (outset (first pbuf)) (max (outset (first pbuf)) (outset partial)))
          else do (push  partial pbuf))
    (first-n   (sort pbuf '> :key #'amplitude) npoly)))
    
        
                    
(defmethod! AS->OM ((analyse list)
                    (vmin integer)
                    (vmax integer)
                    (delta integer)
                    (mmin integer )
                    (mmax integer )
                    (approx integer)
                    (npoly integer ))
  :initvals '( () 40 100 5 4000 8600 8 10)
  :indoc '("Analyse" "vel min" "vel max" "delta" "midic min" "midic max" "approx" "poly. density")
  :icon 250
  :doc  "
Converts partials-analysis data, obtained from AudioSculpt by the 'Export Partials' command,
in a suitable format for displaying and manipulating in OM

parameters : 

analyse : connect here the output of a text module containing the partial analysis.
vmin,vmax : integers, amplitudes will be scaled as Midi Velocities between  vmin and vmax
delta: integer, events whose onset-time fall within a window of <delta> 1/1000sec will be gathered into chords
mmin,mmax: midic values that define the allowed pitch range for the output.
approx: 1,2,4, or 8. Micro-tonal approximation.
npoly: tries and reduce the polyphony to <npoly> notes at the same time by taking the louder partials first.

output : 

a list of chords to be connected to a chordseq module.
"
  (partials->chords (mk-partial-set analyse (round delta 10) vmin vmax (mc->f mmin) (mc->f mmax)) approx npoly))

