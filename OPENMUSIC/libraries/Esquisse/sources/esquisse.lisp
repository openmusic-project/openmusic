(in-package :om)

 
; HARM-SERIES NTH-HARM FREQ-MOD RING-MOD M-VIR-FUN FSHIFT FDISTOR HARM-DIST VIRT-FUND BEST-FREQ BEST-TRANSP

     

(defmethod! harm-series ((fund t) (numer integer) 
                         (denom integer) (begin integer)
                         (end integer)
                         (unit integer) 
                         (type integer))
  :initvals '(3600 1 1 1 7 1 2)
  :indoc '("Fundamental" "Numerator" "Denominator" "Begin" "End" "Unit" "Type")
  :menuins '((5 (("Midics" 1) ("Freqs" 2)))(6 (("Chord" 1) ("ChordSeq" 2))))
  :icon 242 
  :doc  "Builds the harmonic and/or sub-harmonic series starting with the 
fundamental <fund>. The arguments <numer> and <denom> determine what sample (<numer>/<denom>)
of the partials is taken. (e.g. 1/1 = all; 1/2 = every other; 2/5 = the 
first two of each group of five)

The arguments <begin> and <end> determine the lowest and highest partials
generated. The fundamental is represented by '1' or '-1' sub-harmonics are 
represented by negative numbers, overtones by positive. (e.g. partial
number 7 is 7 times the fundamental frequency,partial -7 is the 
fundamental frequency divided by 7; thus to go from the seventh undertone 
to the seventh overtone <begin> would equal '-7' and <end> would equal 
'7')

The menu argument <unit> determines whether the <fund> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
value will be converted to frequency inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculation and output are all in hertz.

When <fund> is a list, the menu argument <type> is used to determine
the format of the output. The value 'seq' returns a list of chords 
representing the partials requested for each successive fundamental. The
value 'chord' returns a single chord containing all the partials of all 
the fundamentals." 
  (let* ((fund (if (= unit 2) fund (mc->f  fund)))
         (res (car-mapcar 'sercalc  fund  numer denom begin end)))
    (setq res (if (= unit 2) res  (f->mc res) ))
    (if (= type 1) (flat res) res)))


(defmethod! nth-harm ((fund t) (nth t) (unit integer) (type integer))
  :initvals '(3600 '(1 2 3 4 5) 1 1)
  :indoc '("Fundamental" "Partial-numbers" "Unit" "Type")
  :menuins '((2 (("Midics" 1) ("Freqs" 2)))(3 (("Chord" 2) ("ChordSeq" 1))))
  :icon 242 
  :doc  
  "Receives a fundamental <fund>, or list of fundamentals, and returns 
a list of partials whose rank is defined by <nth>.
<nth> is a number or a list of number Negative numbers mean subharmonics.
Floating point numbers mean non-harmonic partials.

The menu argument <unit> determines whether the <fund> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
value will be converted to frequency inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculation and output are all in hertz.

When <fund> is a list, the menu argument <type> is used to determine
the format of the output. The value 'ChordSeq' returns a list of chords 
representing the partials requested for each successive fundamental. The
value 'chord' returns a single chord containing  the merged partials of all 
the fundamentals."

  (let* ((nth (list! nth)) (fund (list! fund))
         (listfund  (not (one-elem fund)))
         (listnth (not (atom (car nth))))
         (listchord (not (atom (car fund))))
         (fund (if (= unit 2)  fund   (mc->f fund)))
         res)
    (cond  (listchord (setq res (seqfund-nth fund nth type listnth)))
           ((and listfund listnth) (setq res (doublenth fund nth type)))
           ((not listfund)  (setq res (flat-once (simplenth fund nth type))))
           (t (setq res (simplenth fund nth type))))
    (if (one-elem res) (setq res (first res)))
    (if (= unit 2) res (f->mc res))))


(defmethod! freq-mod ((carrier number) (modul number) (index number) (unit symbol) (type symbol))
   :initvals '(3600 4000 1 'midic 'chord)
   :indoc '("Carrier" "Moduler" "Index" "Unit" "Type")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq)))(4 (("Chord" 'chord) ("ChordSeq" 'chordseq))))
   :icon 242 
   :doc  
   "Computes a FM spectrum from <carrier>, <modul> and <index>.
<carrier> and <modul> may be expressed in midics (the default) or in freqs. In that case
the menu <unit> must be set to 'Freq'.
<index>, the modulation index is a positive integer between 1 and 25.
Outputs a chord object.
<carrier>, <modul> and <index> may also be lists. In that case,  the 1st item in <carrier>
is modulated by the 1st item in <modul>,  with the 1st index in <index>, and so on.
The results are then merged into a chord object.
When using lists, you may also have the result as a series of chords in a 
chord-seq object by selecting 'chordseq' in the menu <type>"
   
   (when (eq unit 'midic)
     (setf carrier (mc->f carrier) modul (mc->f modul)))
   (let* ((spec (fmspec carrier modul index))
          (vel (mapcar #'(lambda (x) (round (* (/ 127 3.0) (if (<= (cdr x) 0.0) 0 (log (cdr x) 10))))) spec))
          (spec (band-filter (mapcar #'car spec) '((15.0 20000.0)) 'pass)) )
     (make-instance 'chord
       :lmidic (f->mc spec)
       :lvel vel) ))


(defmethod! freq-mod ((carrier list) (modul list) (index list) (unit symbol) (type symbol))
  (let ((fmlist (loop while (or carrier modul index)
                      for car = (pop carrier) then (if carrier (pop carrier) car)
                      for mod = (pop modul) then (if modul (pop modul) mod)
                      for ind = (pop index) then (if index (pop index) ind)
                      collect (freq-mod car mod ind unit type))))
    (if (eq type 'chordseq)
      (make-instance 'chord-seq :lmidic fmlist)
      (make-instance 'chord :lmidic (flat (mapcar 'lmidic fmlist)) :lvel (flat (mapcar 'lvel fmlist))))))

(defmethod! freq-mod ((carrier t) (modul t) (index t) (unit symbol) (type symbol))
  (freq-mod (list! carrier) (list! modul) (list! index) unit type))

(defmethod! freq-mod ((carrier chord) (modul t) (index t) (unit symbol) (type symbol))
  (freq-mod (lmidic carrier) modul index 'midic type))


(defmethod! m-vir-fun ((chord list) (approx integer)  (thresh number)  (unit symbol))
   :initvals '('(6000 6400 6700) 50 1200 'midic)
   :indoc '("Chord" "Approx" "MinFund" "Unit")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq))))
   :icon 242 
   :doc  
   "Computes a series of possible virtual fundamentals from <chord> (a list of pitches) and <approx> (in midicents).
The result is a list of chord-seqs that can be input in the 'chord-seqs'input of a 'multi-seq'.
Each chord-seq is a possible solution, sorted from the least significant to the more significant.
Each chord-seq is a series of chords where the lowest note is a vir. fun. and the remaining notes are a subset
of the original chord.
<threshold> is a minimum value for the vir. fundamentals. 
If the menu <unit> is 'freq' then <chord> and <thresh> must be given in Hz, otherwise Midics."
   
   (setf chord (sort (copy-list chord) '<))
   (when (eq unit 'midic) (setf chord (mc->f chord) thresh (mc->f thresh)))
   (let ((classement (make-classement chord  (cents->coef approx) thresh)) )
     (loop while  (iteration classement ) )
     (loop for regroupement in (rest (regroupements classement))
           collect
           (make-instance 'chord-seq :lmidic 
                          (loop for spectre in (spectres regroupement)
                                collect   
                                (f->mc (join-fund-to-spec (first (fondamentales spectre))
                                                          (mapcar 'frequence (partiels spectre)))))))) )
     


(defmethod! m-vir-fun ((chord chord) (approx integer)  (thresh number)  (unit symbol))
  (m-vir-fun (lmidic chord) approx thresh 'midic))


(defmethod! ring-mod ((ch1 number) (ch2 number) (unit symbol) (type symbol))
   :initvals '(6000 6200  'midic 'chord)
   :indoc '("Chord" "Chord" "Unit" "Mode")
   :menuins '((2 (("Midics" 'midic) ("Freqs" 'freq))) (3 (("Chord" 'chord) ("ChordSeq" 'chordseq))) )
   :icon 242 
   :doc  
   "Simulates the ring modulation of each note of <ch1> by all the notes of 
<ch2>. The frequency of each note of <ch2> is added to and subtracted 
from the frequency of each note of <ch1>; thus, all the possible 
additive and subtractive combinations are produced.

<ch1> and <ch2> may be midics, list of midics, list of lists of midics or chord objects.

The optional argument <unit> determines whether <ch1> and <ch2> are 
entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is 
selected the values will be converted to frequencies inside the function 
and then the output is reconverted to midicents. If 'freq' is selected the 
entries, calculations and output are all in hertz. (note: Ring-modulation 
can produce negative frequencies; conversion to midicents will 
automatically reflect these notes back into the positive domain.)

When <ch1> contains multiple notes, the optional argument <type> is used 
to determine the format of the output. The value 'seq' returns a list of 
chords representing the modulation of each successive note of <ch1> by 
all the notes of <ch2>. The value 'chord' returns a single chord 
containing all the notes of all the modulations.

The output is always list of midics or list of list of midics.
<ch1> and <ch2> may be chord objects in which case the unit is set to 'midic internally.
"
   
   (let ((res (if (eq unit 'freq) 
                (ring/freq  ch1  ch2)
                (f->mc (ring/freq (mc->f ch1) (mc->f ch2))))))
     ;(if (= output 2) (setq res (push (x-append ch1 ch2) res)) res)
     (setq res (push (x-append ch1 ch2) res))
     (setq res (band-filter res '((100 12000)) 'pass))
     (if (or (eq type 'chord) (one-elem ch1)) (flat res) res)))


(defmethod! ring-mod ((ch1 t) (ch2 t) (unit symbol) (type symbol))
  (let ((ring (remove-dup
               (loop for note in (list! ch1)
                     collect (loop for mod in (list! ch2) append (ring-mod note mod unit type)))
               'eq
               2)))
    (if (eq type 'chord)
      (remove-dup (flat ring) 'eq 1)
      ring)))


(defmethod! ring-mod ((ch1 CHORD) (ch2 CHORD) (unit symbol) (type symbol))
   (RING-MOD (LMIDIC ch1) (lmidic ch2) 'midic type))



(defmethod! fshift ((pitch number) (dfreq number) (unit symbol) (type symbol)) 
  :initvals '(6000 100  'midic 'chord)
  :indoc '("pitch" "dfreq" "Unit" "Mode")
  :menuins '((2 (("Midics" 'midic) ("Freqs" 'freq))) (3 (("Chord" 'chord) ("ChordSeq" 'chordseq))) )
  :icon 242 
  :doc  
  "Shifts the frequency of each note of <chord> by a frequency <dfreq> 
(positive or negative, but always in hertz).

<chord> may  be a list of midics, a list of list of midics, a chord object or a chord-seq objest.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz. 

If <chord> is a list of chords the optional argument <type> is used to 
determine whether the output will be a list of chords ('seq'), each one
shifted by <dfreq> or a single chord combining the notes of all the 
shifted chords ('chord'). If <dfreq> is a list the same argument is used
to choose between a list of chords shifted by each successive <dfreq> or a 
single chord combining the different distortions. If both <chord> and
<dfreq> are lists the position 'seq' will return a list of chords 
containing  each chord shifted by each frequency; the position 'chord' 
will return a list of chords containing each chord shifted by all the 
listed frequencies.

The optional argument <output> determines whether the original <chord> is 
included ('inclu') or excluded ('exclu') from the output list."
  
  (let ((fchord (if (eq unit 'freq)  pitch   (mc->f pitch)))    res)
    (setq res (simpleshift fchord dfreq 1))
    (if (eq unit 'freq) res (f->mc res))))



(defmethod! fshift ((pitch list) (dfreq number) (unit symbol) (type symbol))
   (let ((res (loop for item in  pitch
                     collect (fshift item dfreq unit type))))
      (if (eq type 'chord)
        (apply 'x-append res)
        res)))


(defmethod! fshift ((pitch t) (dfreq list) (unit symbol) (type symbol))
   (let ((pitch (list! pitch)) res)
     (cond ((list-subtypep pitch '(number))
            (setf res (loop for freq in dfreq
                            collect (loop for pit in pitch
                                          collect (fshift pit freq unit type)))))
           ((list-subtypep pitch '(list))
            (setf res (loop for pit in pitch
                            collect (apply 'append  (fshift pit dfreq unit 'chordseq)))))
           (t pitch))
     (if (eq type 'chord)
       (apply 'x-append res)
       res)))
            

(defmethod! fshift ((pitch chord) (dfreq t) (unit symbol) (type symbol))
  (fshift (lmidic pitch) dfreq unit type))

(defmethod! fshift ((pitch chord-seq) (dfreq t) (unit symbol) (type symbol))
   (fshift (lmidic pitch) dfreq unit type))



(defmethod! fdistor ((chord list) (minout number) (maxout number) (unit symbol)
                     &optional minin  maxin)
   :initvals '(6000 5700 7000  'midic nil nil)
   :indoc '("pitches" "minout" "maxout" "Unit" "minin" "maxin")
   :menuins '((3 (("Midics" 'midic) ("Freqs" 'freq)))  )
   :icon 242 
   :doc  
   "Distorts the frequencies of <chord> so that the lowest note is changed to
<minout> and the highest note to <maxout>. Interior notes are rescaled so
as to preserve the relative positions of their frequencies.

The optional inputs <minin> and <maxin> allow the scaling to be done 
relative to two selected reference notes rather than the highest and 
lowest notes of the chord. The note entered as <minin> will be moved to 
<minout>, and <maxin> to <maxout> the rest of the chord is then 
rescaled accordingly.

<chord> may  be a list of midics, a list of list of midics, a chord object or a chord-seq objest.

If <chord> is a list of chords, output will be a corresponding list of 
distorted chords.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz.

"
   (when  (null minin) (setf minin (list-min chord)))
   (when  (null maxin) (setf maxin (list-max chord)))
   (when (not (equal unit 'freq))
     (setq chord (mc->f chord) 
           minout (mc->f minout) 
           maxout (mc->f maxout)
           minin  (mc->f minin)
           maxin  (mc->f maxin)))
   (let ((res (om-scale chord  minout  maxout minin maxin)))
     
     (if (eq unit 'freq) res (f->mc res))))

(defmethod! fdistor ((chord chord) (minout number) (maxout number) (unit symbol)
                     &optional minin  maxin)
   (fdistor (lmidic chord) minout maxout 'lmidic minin maxin))

(defmethod! fdistor ((chord chord-seq) (minout number) (maxout number) (unit symbol)
                     &optional minin  maxin)
   (fdistor (lmidic chord) minout maxout 'lmidic minin maxin))



(defmethod! harm-dist ((chord list) (fund number) (unit symbol))
   :initvals '(6000 2400 'midic)
   :indoc '("pitches""fundamental" "Unit" )
   :menuins '((2 (("Midics" 'midic) ("Freqs" 'freq)))  )
   :numouts 2
   :icon 242 
   :doc     
   "Calculates the ratios between each note of <chord> and the closest 
partial of the harmonic series built on <fund>.

<chord> may  be a list of midics, a list of list of midics, a chord object or a chord-seq objest.

If <chord> is a sequence of chords the result will be a sequence of the analyses of each 
successive chord.


The optional argument <unit> determines whether <chord> and <fund> are
entered in midicents, ('midic'), or in hertz ('freq'). If 'midic' is 
selected the notes will be converted to frequencies inside the function
before analysis.

There are 2 outputs : the ratios and the corresponding partial numbers" 


   (if (not (equal unit 'freq)) (setq fund (mc->f fund) chord (mc->f chord)))
   (let ((res (deep-mapcar/1 #'(lambda (x)  (multiple-value-list (harm-dist-f fund x))) chord)))
     (cond ((list-subtypep chord '(number))
            (values-list (mat-trans res)))
           ((list-subtypep chord '(list))
            (values-list (mat-trans (mapcar 'mat-trans res))))
           (t (values nil nil)))))


(defmethod! harm-dist ((chord number) (fund number) (unit symbol))
   (let ((res (multiple-value-list (harm-dist (list chord) fund unit))))
     (values (first (first res)) (first (second res)))))
     

(defmethod! harm-dist ((chord chord) (fund number) (unit symbol))
  (harm-dist  (lmidic chord) fund unit))

(defmethod! harm-dist ((chord chord-seq) (fund number) (unit symbol))
  (harm-dist  (lmidic chord) fund unit))



(defmethod! virt-fund ((chord list) (cents integer) (unit symbol)) 

   :initvals '('(6000) 50  'midic)
   :indoc '("pitches""approx" "unit" )
   :menuins '((2 (("Midics" 'midic) ("Freqs" 'freq)))  )
   :icon 242 
   :doc     

  "Returns the highest fundamental for which the notes of <chord> could be 
thought of as harmonic partials. In general, the lower the virtual 
fundamental, the more dissonant the chord. 

<chord> may  be a list of midics, a list of list of midics, a chord object or a chord-seq objest.

The argument <cents> determines the precision of the analysis. (a value of 
'0' would return the real fundamental; the larger the value the more 
approximate the result) 

If <chord> is a sequence of chords the box returns a list of virtual 
fundamentals.

The optional argument <unit> determines whether <chord> is entered and the 
result returned in midicents, ('midic'), or in hertz ('freq'). The argument
<cents> remains, however, unchanged."

  (if (eq unit 'freq)
    (less-deep-mapcar 'virt-fund1 chord cents)
    (f->mc (less-deep-mapcar 'virt-fund1 (mc->f chord) cents))))

(defmethod! virt-fund ((chord chord) (cents integer) (unit symbol)) 
   (virt-fund (lmidic chord) cents 'midic))

(defmethod! virt-fund ((chord chord-seq) (cents integer) (unit symbol)) 
   (virt-fund (lmidic chord) cents 'midic))


(defmethod! best-freq ((chord list) (unit symbol))
  :initvals '('(6000)  'midic)
  :indoc '("pitches" "unit" )
  :menuins '((1 (("Midics" 'midic) ("Freqs" 'freq)))  )
  :icon 242 
  :doc     
  
  "Returns the note which is the minimum possible distance from the 
frequencies of all the notes of <chord>. (minimum sum of the squares of 
the distances) This note can be thought of as a sort of center of gravity
for <chord> (it is not usually a member of the chord).

If <chord> is a list of chords the box returns a list of best frequencies.

The optional argument <unit> determines whether <chord> is entered in 
midicents, ('midic'), or in hertz ('freq'). If 'midic' is selected the 
values will be converted to frequencies inside the function and then the 
output is reconverted to midicents. If 'freq' is selected the entry, 
calculations and output are all in hertz."
  (if (eq unit 'freq)
    (less-deep-mapcar 'best-freq1 chord)
    (f->mc (less-deep-mapcar 'best-freq1 (mc->f chord)))))


(defmethod! best-freq ((chord chord) (unit symbol))
   (best-freq (lmidic chord) 'lmidic))

(defmethod! best-freq ((chord chord-seq) (unit symbol))
   (best-freq (lmidic chord) 'lmidic))


(defmethod! best-transp ((ch1 list) (ch2 list) (fct symbol))
  :initvals '('(6000)  '(6000) 'sum)
  :indoc '("chord" "chord" "fct")
  :menuins '((2 (("Sum" 'sum) ("Max" 'max)))  )
  :icon 242 
  :doc     
 
"Transposes the chord <ch2> (a single chord in midicents) so that its 
intervallic distance to <ch1> (also a single chord in midicents) is as 
small as possible. Thus the distance between each note of <ch2> and each 
note of <ch1> becomes as small as possible.This is essentially the same 
as the box 'best-inv' except the ordering of <ch2> is preserved.

The optional argument <fct> allows the choice between two different 
algorithms for calculating this function,'sum' and 'max'. The default
is sum because 'max' can produce quarter tones from demi-tone input. For
best results one should experiment with both and chose according to 
context."

(om+ ch2 (if (eq fct 'max) (ma-best-transp ch1 ch2) (sa-best-transp ch1 ch2))))


(defmethod! best-transp ((ch1 t) (ch2 list) (fct symbol))
   (best-transp (lmidic ch1) ch2 fct))

(defmethod! best-transp ((ch1 list) (ch2 chord) (fct symbol))
   (best-transp  ch1 (lmidic ch2) fct))

(defmethod! best-transp ((ch1 chord) (ch2 chord) (fct symbol))
   (best-transp (lmidic ch1) (lmidic ch2) fct))





;=============================================================
;===================== Utilities ==========================
;=============================================================



(defun cents->coef (nb-cents)
  "<cents->coef> takes an interval expressed in midi-cents and returns the ratio 
between two frequencies separated by that interval; i.e., the value: (freq + <nb-
cents>) / freq."
  (expt 2.0 (/ nb-cents 1200.0)))

(defun car-mapcar (fun list?  &rest args)
  "Mapcars <fun> if list? is a list or applies <fun> if it is an atom or
a one-element list"
  (cond  ((atom list?) (apply fun list? args))
         ((= (length list?) 1) (apply fun (car list?) args))
         (t (mapcar #'(lambda (x) (apply fun x  args ))  list? ))))

(defun less-deep-mapcar (fun  list? &rest args)
  "Applies <fun> to <list?> <args> if <list?> is a one-level list .
   Mapcars <fun> to <list?> <args> if <list?> is a multi-level list. "
  (cond
    ((null list?) ())
    ((atom (car list?)) (apply fun list? args))
    ((atom (car (car list?))) 
     (cons (apply fun (car list?)  args ) (apply #'less-deep-mapcar fun (cdr list?) args)))
    (t (cons (apply #'less-deep-mapcar fun  (car list?) args)
             (apply #'less-deep-mapcar fun  (cdr list?) args)))))

(defun deep-mapcar/1 (fun list? &rest args)
  (labels ((map-structure (str accum)
             (cond ((null str) (reverse accum))
                   ((not (consp str))
                    (if accum (reverse (cons (apply fun str args) accum)) (apply fun str args)))
                   (t (map-structure (cdr str) (cons (map-structure (car str) ()) accum))))))
    (map-structure list? nil)))

(defun one-elem (item)
  (or (atom item) (= (length item) 1)))

(defun simplenth (fund nth type)
  (let ((res (deep-mapcar/1  'hrmcalc  fund  nth)))
    (if (= type 2) (flat-once res) res)))


(defun doublenth (fund nth type)
  (if (= type 1) (flat-once (simplenth fund nth type))
      (mapcar #'(lambda (x) (flat (simplenth fund x type))) nth)))

(defun seqfund-nth (fund nth type listnth)
  (cond ((not listnth) (mapcar #'(lambda (x) (simplenth x nth 2)) fund)) 
        ((= type 1) (flat-once (mapcar #'(lambda (x) (doublenth x nth  2)) fund)))
        ((= type 2)  (mapcar #'(lambda (x) (flat (simplenth x nth type))) fund) )))


(defun virt-fund1 (chord cents)
  (car-mapcar #'(lambda (c) (fond-virt-f  chord (1- (cents->coef c)) ;;(round (/ 100 c))
                                          )) cents))

(defun fond-virt-f (freqs approx)
  (tolerant-gcd freqs approx))

;;From Gerard Assayag [93 07 16]

(defun tolerant-gcd (values grid-ratio)
  "floating gcd with tolerance grid-ratio around the values."
  (labels ((grid-above (val) (* val (1+ grid-ratio)))
           (grid-below (val) (/ val (1+ grid-ratio)))
           (gcd-try (values gcd-min gcd-max)
             (when (<= gcd-min gcd-max)
               (ifnot values
                 (/ (+ gcd-min gcd-max) 2.0)
                 (let* ((val-below (grid-below (first values)))
                        (val-above (grid-above (first values)))
                        (quo-min (ceiling (/ val-below gcd-max)))
                        (quo-max (floor (/ val-above gcd-min))))
                   (do* ((quotient quo-min (1+ quotient)) (gcd-interval))
                        ((> quotient quo-max) nil)
                     (setf gcd-interval
                           (gcd-try (rest values)
                                         (max gcd-min (/ val-below quotient))
                                         (min gcd-max (/ val-above quotient))))
                     (when gcd-interval
                       (return-from gcd-try gcd-interval))))))))
    (gcd-try values .1 (grid-above (apply 'min values)))))



;=============================================================
;===================== freq harmony ==========================
;=============================================================


; ==================== serie harm  ===========================

(defun rankcalc (numer denom begin end )
  (let ((cdeb (mod begin denom)) (pas (if (< end begin) -1 1)) res)
    (for (n begin pas end) 
      (if (not (>= (mod (- n cdeb) denom) numer)) (push n res)) )
    (nreverse (remove 0 (if (and (not (null (find 1 res))) (not (null (find -1 res)))) 
                        (remove -1 res) res)))))

(defun hrmcalc1 ( listharm fond)
  (let (res)
    (dolist ( n listharm)
          (cond ( ( > n 0 ) (push (* fond  n ) res))
                ( ( < n 0 ) (push (/ fond (abs n))  res))
                ( t () ) ))
    (nreverse  res)))

(defun hrmcalc (fond listharm )
  (less-deep-mapcar #'hrmcalc1 listharm fond))

(defun sercalc (fund  numer denom begin end )
  (hrmcalc fund (rankcalc numer denom begin end) ))

(defun harm-dist-f (f0 freq) 
  "Returns the ratio between the closest harmonic of <f0> and <freq>."
  (if (<= freq f0) (values (/ freq f0) 1)
      (let* ((ratio (/ freq f0))
             (n-partial (floor ratio))
             (d1 (/ ratio n-partial))
             (d2 (/ (1+ n-partial) ratio)))
        (if (< d1 d2) (values d1 n-partial) (values d2 (1+ n-partial))))))

(defun best-freq1 (freqs)
  (let ((sum 0) (nb-freq (length freqs)))
    (while freqs (incf sum (log (nextl freqs))))
    (exp (/ sum nb-freq))))


; ==================== ring mod  ===========================

(defun ring/freq (freqs1 freqs2 )
"Rend une liste de listes de frquences contenant la modulation en anneau 
de chaque frequence de la liste <freqs1> par la liste <freqs2>"
  (let* (ll (freqs1 (list! freqs1)) (freqs2 (list! freqs2))
         (x (one-elem freqs1)))
    (while freqs1
      (let ((a (pop freqs1)))
        (push (append (om+ a freqs2) (om- a freqs2)) ll) ))
    (if  x (flat (nreverse ll)) (nreverse ll))))

; ==================== freq shifting  ===========================

(defun simpleshift (fchord dfreq output)
  (let ((res (om+ dfreq  fchord )))
    (if (= output 2) (x-append fchord res) res)))


; ==================== freq modulation  ===========================

(defvar *maxorder* 25)
(defvar *bessel*
      (make-array '(26 25)
                  :initial-contents
      '(
        ( 1000 765 223 -260 -397 -177 150 300 171 -90 -245 -171 47 206 171 -14 -174 -169 -13 
           146 167 36 -120 -162 -56)
        ( 0 440 576 339 -66 -327 -276 -4 234 245 43 -176 -223 -70 133 205 90 -97 -187 -105 66 
          171 117 -39 -154)
        ( 0 114 352 436 364 46 -242 -301 -112 144 254 139 -84 -217 -152 41 186 158 -7 -157 -160 
           -20 131 158 43)
        ( 0 19 128 309 430 364 114 -167 -291 -180 58 227 195 3 -176 -194 -43 134 186 72 -98 -174 
           -93 67 161)
        ( 0 2 33 132 281 391 357 157 -105 -265 -219 -15 182 219 76 -119 -202 -110 69 180  130 -29 
           -156 -141 -3)
        ( 0 0 7 43 132 261 362 347 185 -55 -234 -238 -73 131 220 130 -57 -187 -155 3 151 163 36 
           -116 -162)
        ( 0 0 1 11 49 131 245 339 337 204 -14 -201 -243 -118 81 206 166 0 -155 -178 -55 107 173 90 
           -64)
        (0 0 0 2 15 53 129 233 320 327 216 18 -170 -240 -150 34 182 187 51 -116 -184 -102 58 163 130)
        (0 0 0 0 4 18 56 127 223 305 317 224 45 -141 -231 -173 -7 153 195 92 -73 -175 -136 8 140)
        (0 0 0 0 0 5 21 58 126 214 291 308 230 66 -114 -220 -189 -42 122 194 125 -31 -157 -157 -36)
        (0 0 0 0 0 1 6 23 60 124 207 280 300 233 85 -90 -206 -199 -73 91 186 148 7 -132 -167)
        (0 0 0 0 0 0 2 8 25 62 123 201 270 292 235 99 -68 -191 -204 -98 61 173 164 42 -103)
        (0 0 0 0 0 0 0 2 9 27 63 121 195 261 285 236 112 -48 -176 -205 -118 32 156 173 72)
        (0 0 0 0 0 0 0 0 3 10 28 64 120 190 253 278 236 122 -30 -161 -204 -135 6 137 176)
        (0 0 0 0 0 0 0 0 1 3 11 30 65 118 185 246 272 236 131 -15 -146 -200 -148 -17 118)
        (0 0 0 0 0 0 0 0 0 1 4 13 31 65 117 181 239 266 235 138 0 -132 -195 -158 -38)
        (0 0 0 0 0 0 0 0 0 0 1 5 13 32 66 116 177 234 261 234 145 12 -118 -189 -166)
        (0 0 0 0 0 0 0 0 0 0 0 1 5 14 33 66 114 173 228 255 233 150 23 -105 -183)
        (0 0 0 0 0 0 0 0 0 0 0 0 2 6 15 34 66 113 170 223 251 231 154 34 -93)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 2 6 16 35 67 112 167 218 246 229 158 43)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 7 17 36 67 111 164 214 242 228 161)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 7 18 36 67 110 162 210 238 226)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 8 18 37 67 109 159 206 234)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 8 19 38 67 108 157 203)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 3 9 19 38 67 107 155)
        (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 4 9 20 39 67 106)
      )
      )
      )

(defun fix (x) (truncate x))
(defun ceil (x) (ceiling x))
(defun fixp (x) (integerp x))
(defun add1 (x) (1+ x))

(defun fmSpec (c m imod &optional order)
    (let ((spec) s p  MI)
         (setq MI imod)
         (if (floatp imod) (setq imod (ceil imod)) (setq imod (fix imod)))
         (if (null order)
             ;(setq order (car order))
             (setq order (add1 imod)) )
         (setq order (min order *maxorder*))
         (setq spec `((, c . 0 )))
         (for (i 1 1 order)
              (newl spec (cons (- c (* i m)) (- i)))
              (setq  spec (nconc spec (list (cons (+ c (* i m)) i))))
              (when (and (null p) (< (caar spec) 0))
                    (setq p spec)))
         (setq s spec)
         (while s
                ;(when (and (null p) (>= (caar s) 0))
                ;      (setq p q))
                (cond 
                      ( (< (cdar s) 0)
                        (if (oddp (cdar s))
                            (rplacd (car s) (- (bessel MI (abs (cdar s)))))
                            (rplacd (car s) (bessel MI (abs (cdar s)))))
                        (when (< (caar s) 0)
                              (rplaca (car s) (- (caar s)))
                              (rplacd (car s) (- (cdar s)))))
                      ( t
                        (rplacd (car s) (bessel MI (cdar s)))))
                ;(setq q s)
                (nextl s))
         (setq spec
               (if (not p)
                    spec
                    (fmMerge (cdr p) 
                             (progn (rplacd p ()) (nreverse spec)))))
         (mapc #'(lambda (comp)
                       (rplacd comp (abs (cdr comp))))
               spec)
         (when (<= (caar spec) 0) (nextl spec))
          (fmNormalize spec)
         spec
))


(defun fmNormalize (spec)
    (let ((etot 0) ratio)
         (mapc #'(lambda (x) (setf  etot (+ etot (cdr x))))
               spec)
         (setq ratio (/ 1000.0 etot))
         (mapc #'(lambda (x)
                  (rplacd x (fix (* (cdr x) ratio))))
               spec)
        spec))


 
(defun bessel (imod i)
    (if (fixp imod)
        (aref *bessel* i imod)
        (let ((i1 (aref *bessel* i (fix imod))) (i2 (aref *bessel* i (ceil imod))))
             (fix (+ i1 (* (- imod (fix imod)) (- i2 i1)))))))

(defun fmMerge   (f1 f2)
    (let ((r (list ())))
         (fmMerge2 r f1 f2)
         (cdr r)))


(defun fmMerge2 (r f1 f2)
    (cond 
          ((null f1) (rplacd r f2))
          ((null f2) (rplacd r f1))
          ((< (caar f1) (caar f2))
           (rplacd r f1)
           (fmMerge2 f1 (cdr f1) f2))
          ((= (caar f1) (caar f2))
           (rplaca f1 (cons (caar f1) (+ (cdar f1) (cdar f2))))
           (rplacd r f1)
           (fmMerge2 f1 (cdr f1) (cdr f2)))
          (t (rplacd r f2)
             (fmMerge2 f2 f1 (cdr f2)))))


(defun fm (c m i)
  (let ((spec (fmspec c m i)))
    (cons (mapcar #'car spec)
          (mapcar #'(lambda (x) (round (* (/ 127 3.0) (log (cdr x) 10)))) spec))))






;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------
;;; Fondamentales virtuelles multiples. Algorithme par Olivier Delerue.
;;; -------------------------------------------------------------------------
;;; -------------------------------------------------------------------------






(defvar *tolerance* 1.0293 ) ;;; quart de ton
(defvar *frequence-min* 30 )

;;; (setf *frequence-min* 20 ) 
;;; (setf *tolerance* 1.01 )    un pour cent d'erreur

;;; -------------------------------------------------------------------------
;;; Dfinition des classes 
;;; -------------------------------------------------------------------------

(defclass spectre ( )
  ((fondamentales :initarg :fondamentales :initform () :accessor fondamentales )
   (partiels :initarg :partiels :initform ()  :type list  :accessor partiels :documentation "une liste d'objets partiels" )
   ))

(defclass partiel () ((frequence :initarg :frequence :initform 0 :accessor frequence )))
    
(defclass regroupement () ( (spectres :initarg :spectres :initform ()  :accessor spectres :type list )))

(defclass classement () 
  ((regroupements :initarg :regroupements :initform () :accessor regroupements :type list ) 
   (itere0-p :initform nil :accessor itere0-p ))
  )

;;; -------------------------------------------------------------------------
;;; Fonctions utiles  
;;; -------------------------------------------------------------------------


(defmethod fondamentales-communes-spectres ((sp1 spectre) (sp2 spectre))
  (fondamentales-communes (fondamentales sp1) (fondamentales sp2 ))
  )

(defmethod fondamentales-communes-spectres ((sp1 spectre) (sp2 list))
  (fondamentales-communes (fondamentales sp1) sp2)
  )

(defmethod fondamentales-communes ((liste1 list) (liste2 list) )
  (if (and liste1 liste2)
    (let ((temp (intersection-p (car liste1) (car liste2) )))
      (if temp
        (cons temp (fondamentales-communes (cdr liste1) (cdr liste2)) )
        (if (<= (caar liste1) (caar liste2) )
          (fondamentales-communes liste1 (cdr liste2) )
          (fondamentales-communes (cdr liste1) liste2 )
          )
        )
      )
    ()
    )
  )

(defmethod fondamentales-communes-liste ( liste-spectres )
  (if (cdr liste-spectres)
    (fondamentales-communes (fondamentales (car liste-spectres)) 
                            (fondamentales-communes-liste (cdr liste-spectres)))
    (fondamentales (car liste-spectres))
    )
  )

(defmethod premiere-fondamentale-commune ((sp1 spectre) (sp2 spectre))
  (premiere-fondamentale-commune (fondamentales sp1) (fondamentales sp2))
  )

(defmethod premiere-fondamentale-commune ((liste1 list) (liste2 list))
  (if (and liste1 liste2)
    (let ((temp (intersection-p (car liste1) (car liste2) )))
      (if temp
        temp
        (if (<= (caar liste1) (caar liste2) )
          (premiere-fondamentale-commune liste1 (cdr liste2) )
          (premiere-fondamentale-commune (cdr liste1) liste2 )
          )
        )
      )
    '(1 1)
    )
  )

(defun cree-partiels (liste-frequences) 
  (loop for item in liste-frequences collect (make-instance 'partiel :frequence item ) )
  )

(defun cree-liste-spectres (liste-frequences tolerance freq-min)
  (loop for item in liste-frequences collect 
        (make-instance 'spectre 
          :partiels (list (make-instance 'partiel :frequence item ) )
          :fondamentales (liste-fondamentales-possibles (list (/ item tolerance ) (* item tolerance )) freq-min))
        )
  )

(defun liste-fondamentales-possibles ( partiels freq-min)
  (loop for sub from 1
        while (>= (/ (car partiels) sub)  freq-min )
        collect (list (float (/ (car partiels) sub))  (float (/ (cadr partiels) sub ))    )
        )
  )
        
(defun intersection-p ( liste1 liste2 ) 
  (if (or (and (<= (car liste2) (cadr liste1) ) (>= (car liste2) (car liste1) ))  
          (and (<= (cadr liste2) (cadr liste1)) (>= (cadr liste2) (car liste1) )) 
          (and (<= (car liste2) (car liste1)) (>= (cadr liste2) (cadr liste1) ))
          )
    (list (max (car liste1) (car liste2) ) (min (cadr liste1) (cadr liste2) ) )
    ()
    )
  )

(defun make-classement (liste-partiels tolerance freq-min)
  (make-instance 'classement     
    :regroupements (list (make-instance 'regroupement 
                           :spectres (cree-liste-spectres liste-partiels  tolerance freq-min))) ))


(defmethod regroupe-deux-spectres ((spectre1 spectre) (spectre2 spectre))
  (make-instance 'spectre 
    :partiels (append (partiels spectre1 ) (partiels spectre2 )) 
    :fondamentales (fondamentales-communes-spectres spectre1 spectre2)
    )
  )


;;; -------------------------------------------------------------------------
;;; Fonction de distance. 
;;; -------------------------------------------------------------------------

(defmethod distance-nulle ((sp1 spectre) (sp2 spectre))
  (if (> (caar (fondamentales sp1)) (caar (fondamentales sp2)))
    (distance-nulle sp2 sp1)
    (let ((temp (premiere-fondamentale-commune (list (car (fondamentales sp1))) (fondamentales sp2)) ))     
      (if (eq (car temp) 1)
        ()
        temp
        )
      )
    )
  )

;;; -------------------------------------------------------------------------
;;; Methode globale d'iteration sur un classement 
;;; -------------------------------------------------------------------------

(defmethod iteration ((self classement))
  (if (itere0-p self )
    (if (> (length ( spectres (car (last ( regroupements self ))))) 1 )
      (let ((temp (regroupe-distance-non-nulle (car (last ( regroupements self )))) ))
        (if temp 
          (true (setf (regroupements self) 
                      (append ( regroupements self ) 
                              (list temp)) ))
          nil
          )
        )
      nil
      )
    (progn 
        (setf (regroupements self) 
              (append (regroupements self)
                      (list (regroupe-spectres-distance-nulle (car (last (regroupements self )))))))
        (setf (itere0-p self) T )
      )
    )
  )

;;; -------------------------------------------------------------------------
;;; Regroupement de spectres  distance nulle
;;; -------------------------------------------------------------------------

(defmethod regroupe-spectres-distance-nulle ((self regroupement))
  (make-instance 'regroupement 
    :spectres (bidon (spectres self))
    )
  )
  
(defun bidon (liste-spectres)
  (if liste-spectres
    (let ((new-spectre (make-instance 'spectre 
                         :partiels (partiels (car liste-spectres))
                         :fondamentales (fondamentales (car liste-spectres ))
                         )))
      (let ((temp 
             (loop for item in (cdr liste-spectres )
                   when (distance-nulle new-spectre item)
                   do (progn
                        (setf (partiels new-spectre) (append (partiels new-spectre) (partiels item) ) )
                        (setf (fondamentales new-spectre) (fondamentales-communes-spectres new-spectre item))
                        )
                  and collect item
                   )))
        (cons new-spectre (bidon (set-difference (cdr liste-spectres) temp )))
        )
      )
     
    ()
    )
  )

;;; -------------------------------------------------------------------------
;;; Regroupement de spectres a distance NON nulle
;;; -------------------------------------------------------------------------

(defmethod regroupe-distance-non-nulle ((self regroupement))
  (let ((temp (evalue-distances self )))
    (if (eq (caaar temp) 1)
      ()
     (make-instance 'regroupement 
        :spectres (append (list (regroupe-deux-spectres (cadar temp) (caddar temp) ) ) 
                          (set-difference ( spectres self) (cdar temp))
                          )
        )
      )
    )
  )

(defmethod evalue-distances (( self regroupement ) )
  (let (temp)
    (loop for x on ( spectres self) do
          (loop for y in (cdr x )
                do (push (list (premiere-fondamentale-commune (car x) y ) (car x) y ) temp )
                )
          )
    (sort temp #'> :key #'caar )
    )
  )





;===========================================================================
;;; INTERFACE
;===========================================================================


(defun join-fund-to-spec (fund spec) 
  (if (<= (first fund) (first spec) (second fund))
    spec
    (cons (second fund) spec)))




(defun gather-duplicates-1 (list accum)
  (cond ((null list) (reverse accum ))
        ((or (null (first accum)) (= (first list) (first (first accum))))
         (gather-duplicates-1 (rest list) (cons (cons (first list) (first
                                                                    accum)) (rest accum))))
        (t (gather-duplicates-1 (rest list) (cons (list (first list))
                                                  accum)))))


(defun gather-duplicates (list)
  (and list (gather-duplicates-1 list (list (list)))))








;=============================================================
;===================== intervalles ==========================
;=============================================================


;; ---- max-abs-idt ----

(defun max-abs-idt (ch1 ch2)
  "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition as second value."
  (let* ((ints (mapcar #'- ch1 ch2))
         (int-min (apply #'min ints))
         (int-max (apply #'max ints)))
    (values (/ (- int-max int-min) 2) (/ (+ int-max int-min) 2))))

;;so that user extended box works with max-abs-idt as default value...!!!
(defun CL-USER::max-abs-idt (ch1 ch2) (max-abs-idt ch1 ch2))

(defun ma-best-transp (ch1 ch2) 
  "Uses as intervalic distance the maximum of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Computes the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition."
  (multiple-value-bind (dist ch) (max-abs-idt ch1 ch2)
    (declare (ignore dist))
    ch))

;; ---- sum-abs-idt ----

;; - the best transposition "Tbest" is the middle point of the list:
;;   (cond ((oddp (length ints)) (nth (/ n 2) ints))
;;         ((evenp (length ints)) (/ (+ (nth (floor n 2) ints)
;;                                     (nth (floor (1+ n) 2) ints)) 2)))
;; - the corresponding distance is:
;;   D = (Sum (i 0 n/2) |Tbest-INTi|) + (Sum (i n/2 n+1/2) |INTi-Tbest|)
;;   D = (Sum (i n/2 n+1/2) INTi) - (Sum (i 0 n/2) INTi)

(defun sum-abs-idt (ch1 ch2)
  "Uses as intervalic distance the arevage of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Returns the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition as second value."
  (let* ((ints (sort (mapcar #'- ch1 ch2) #'<))
         (1-length (1- (length ints)))
         (summin 0) (summax 0)
         transpos)
    (repeat (floor 1-length 2)
      (incf summin (nextl ints)))
    (if (evenp 1-length)
      (nextl ints transpos)
      (progn
        (incf summin (nextl ints transpos))
        (setq transpos (/ (+ transpos (car ints)) 2))))
    (while ints (incf summax (nextl ints)))
    (values (- summax summin) transpos)))

(defun sa-best-transp (ch1 ch2) 
  "Uses as intervalic distance the arevage of the absolute intervals (in cents)
between the corresponding notes of the two chords <ch1> and <ch2>.
Computes the minimum intervalic distance between <ch1> and the best transposition
of <ch2> and returns this transposition."
  (multiple-value-bind (dist ch) (sum-abs-idt ch1 ch2)
    (declare (ignore dist))
    ch))

