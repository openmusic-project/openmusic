(in-package :cl-user)


(defun pts->motive (solution impulses all-measures motive-lengths tempo &optional chords changes)
  (when (numberp (first all-measures)) (setq all-measures (list all-measures)))
  (let ((from 0) result 
        (NumPoints/voice (mapcar #'length motive-lengths))
        (default-measure (first (last all-measures)))
        (measure all-measures))
    (when (consp (first solution)) (setq solution (first solution)))
    (when changes (setq solution (move-points solution impulses NumPoints/voice changes)))
    (build-sit-rtm all-measures
                   (dolist (n-points NumPoints/voice (nreverse result)) 
                     (push 
                      (voice-point-motive (subseq solution from (+ from n-points)) (pop impulses)
                                          (or (pop measure) default-measure) (pop motive-lengths))
                      result)
                     (incf from n-points))
                   tempo chords)))
 
(defvar *rtm-elements* '(-1 1.0))
(defvar *current-rtm-element* -1)
;;(setf *current-rtm-element* 1.0)

#+:PW
(pw::defunp rtm-sol ((sol list) (sign list (:value '(4 4))) (tempo fix>0 (:value 60)) (i/obj list) 
                     &optional (h-obj list (:value '())) (s/obj list (:value '())) (modif list (:value '()))
                     (slur? list (:value '()))) list
            " "
  (let ((pts (pw::g/ (ch-sol sol) 100)) (note 48) result (dif 7) (incr 0) (maxi 37)
        (*current-rtm-element* (if slur? 1.0 -1)))
    (unless s/obj 
      (setq s/obj (mapcar #'(lambda (x) (make-list (length x) :initial-element 1)) pts))
      (unless h-obj
        (dolist (x pts (setq h-obj (pw::g* (nreverse result) 100)))
          (push (make-list (length x) :initial-element (+ note incr)) result)
          (setq incr (rem (+ incr dif) maxi)) 
          ) ))
    (pts->motive (pw::flat-once pts)
                 i/obj sign (pw::g* s/obj i/obj) tempo h-obj modif)))

#+:OM
(om::defmethod! om::rtm-sol ((sol list) (sign list) (tempo number) (u-obj list)  
                   &optional h-obj i-pts c-pts slur? )
  :initvals '(nil nil nil nil 60 nil nil nil)
  :indoc '("solution"  "measure" "tempo" "impulsion/object" "harmonic objects" "harmonic object's sizes"
           "modifications" "slur?")
  :icon 195
  :doc " "
  (let ((pts (om::om/ (om::ch-sol sol) 100)) (note 48) result (dif 7) (incr 0) (maxi 37)
        (*current-rtm-element* (if slur? 1.0 -1)))
    (unless i-pts 
      (setq i-pts (mapcar #'(lambda (x) (make-list (length x) :initial-element 1)) pts))
      (unless h-obj
        (dolist (x pts (setq h-obj (om::om* (nreverse result) 100)))
          (push (make-list (length x) :initial-element (+ note incr)) result)
          (setq incr (rem (+ incr dif) maxi)) 
          ) ))
  (pts->motive (flat-once pts)
               u-obj sign (om::om* i-pts u-obj) tempo h-obj c-pts)))

(defun measure-now (point measure) (floor point measure))

(defun build-default-measures (measures last-point)
  (let ((default-measure (first (last measures)))
        (last-beat 0) result integrated-beats)
    (do ((last-measure (pop measures) (or (pop measures) default-measure)))
        ((< last-point last-beat))
      (push (make-list  (first last-measure) :initial-element *current-rtm-element*) result)
      (push
       #+:PW (pw::arithm-ser last-beat (/ 1 (second last-measure))
                             (+ last-beat (/ (1- (first last-measure)) (second last-measure))))
       #+:OM (om::arithm-ser last-beat 
                             (+ last-beat (/ (1- (first last-measure)) (second last-measure)))
                             (/ 1 (second last-measure)))
       integrated-beats)
      (incf last-beat (/ (first last-measure) (second last-measure))))
    (values (flat-once (nreverse result)) (flat-once (nreverse integrated-beats)))))

;;(build-default-measures '( (4 4) ) 7/8)

(defun voice-point-motive (points impulse measure-signs length)
  (when (numberp (first measure-signs)) (setq measure-signs (list measure-signs)))
  (let (len after before)
    (multiple-value-bind (beat-measures integrated-beats)
                         (build-default-measures measure-signs (first (last points)))
      (dolist (p points beat-measures)
        (setq len (pop length))
        (cond ((and (numberp len) (minusp len))
               (setq before (- (abs len) impulse) after 0))
              ((and (numberp len) (plusp len))
               (setq after (- len impulse) before 0))
              (t (setq before (abs (first len)) after (second len))))
        (do ((imp p (+ imp impulse))) ((> (- imp p) after))
          (set-beat-motive-notes imp beat-measures measure-signs impulse integrated-beats))
        (do ((imp (- p impulse) (- imp impulse))) ((< (+ imp before) p))
          (set-beat-motive-notes imp beat-measures measure-signs impulse integrated-beats))))))

(defun find-relevant-measure (point measures)
  (let ((from 0))
    (dolist (m measures (first (last measures)))
      (when (< point (+ from (/ (first m) (second m)))) (return m))
      (incf from (/ (first m) (second m))))))

;;(find-relevant-measure  8/16  '( (2 4)(3 8) ))

(defun find-relevant-offset (point measures)
  (let ((from 0))
    (dolist (m measures (- point from))
      (when (< point (+ from (/ (first m) (second m)))) (return (- point from)))
      (incf from (/ (first m) (second m))))))

(defun set-beat-motive-notes (pt beat-measures all-measures impulse integrated-beats)
  (let (prev-beat beat-number n offset
        (measure (find-relevant-measure pt all-measures)))
    ;;(setq offset (rem pt (/ 1 (second measure))))
    (setq offset (rem (find-relevant-offset pt all-measures) (/ 1 (second measure))))
    (setq n (position pt integrated-beats :test '<)
          beat-number (if n (1- n) (1- (length integrated-beats))))
    (setq prev-beat (nth beat-number beat-measures))
    (setf (nth beat-number beat-measures)
          (build-rhythm-motive prev-beat offset impulse measure))))

(defun build-rhythm-motive (prev-beat offset impulse measure)
  (let ((beat (if (eq prev-beat *current-rtm-element*)
                (make-list (floor (/ (second measure)) (or (third measure) impulse))
                         :initial-element *current-rtm-element*)
                (second prev-beat))))
    (setf (nth (floor offset (or (third measure) impulse)) beat) 1)
    (list 1 beat)))

(defun get-motive-notes (solution p/voice impulses chord-set) " "
       (declare (ignore chord-set))
  (let ((voices 
         #+:PW (pw::group-list solution p/voice 0)
         #+:OM (group-list solution p/voice 0)
         )
        imp lengths result)
    (dolist (v voices (nreverse result))
      (setq imp (pop impulses) lengths nil)
      (push (first v) lengths)
      (dolist (dist #+:PW (pw::x->dx v) #+:OM (om::x->dx v))
        ;;(push (floor dist imp) lengths)
        (push dist lengths))
      (push (nreverse lengths) result))))

 ;;


(defun move-points (solution impulses p/voice changes)
" "
  (let ((voices #+:PW (pw::group-list (copy-list solution) p/voice 0)
                #+:OM (group-list (copy-list solution) p/voice 0)
                ) v point imp)
    (dolist (form changes #+:PW (pw::flat-once voices)  #+:OM (flat-once voices))
      (setq v (nth (first form) voices) imp (nth (first form) impulses))
      (dolist (pairs (rest form))
        (cond ((numberp (first pairs))
               (setq point (nth (first pairs) v))
               (setf (nth (first pairs) v) (+ point (* imp (second pairs)))))
              (t (setq v (sort (cons (* imp (second pairs)) v) '<))
                 (setf (nth (first form) voices) v)))))))


(defun build-sit-rtm (meas-signature beat-list tempo &optional chords)
  (let (result current-measures default-measure ch-obs voice-ms)
    (dolist (voice beat-list (nreverse result))
      (setq ch-obs (mapcar #'(lambda (ch) #+:PW (pw::mk-chord  ch 100 0 100)
                                          #+:OM (make-instance 'om::chord :lmidic  ch)
                              ) (pop chords)))
      (setq current-measures (pop meas-signature))
      (push
       (cond ((numberp (first current-measures))
              (setq default-measure current-measures)
              (setq voice-ms
                    (if (= (length voice) (first current-measures))
                      (list voice)
                      #+:PW (pw::list-explode voice (/ (length voice) (first current-measures)))
                      #+:OM (list-explode voice (/ (length voice) (first current-measures)))
                      ))
              (construct-pw&om-measures voice-ms tempo ch-obs 
                                     (make-list (length voice-ms) :initial-element  current-measures)))
             ((null current-measures)
              (setq voice-ms
                    (if (= (length voice) (first default-measure))
                      (list voice)
                      #+:PW (pw::list-explode voice (/ (length voice) (first default-measure)))
                      #+:OM (list-explode voice (/ (length voice) (first default-measure)))
                      ))
              (construct-pw&om-measures voice-ms tempo ch-obs
                                     (make-list (length voice-ms) :initial-element  default-measure)))
             (t
              (construct-pw&om-measures #+:PW (pw::group-list voice (mapcar #'first current-measures) 0)
                                     #+:OM (group-list voice (mapcar #'first current-measures))
                                     tempo ch-obs
                                     current-measures)))
       result))))

(defvar *before-first-beat* t)
      
#+:PW
(defun construct-pw&om-measures (voice-ms tempo ch-obs all-signs)
  (setf *before-first-beat* nil)
  (let ((pulses (mapcar #'second all-signs))) ;;(*before-first-beat* t))
    (make-instance 
      'pw::C-measure-line
      :measures
      (mapcar 
       #'(lambda (beats pulse)
           (pw::make-measure pulse
                             (mapcar 
                              #'(lambda (beat)
                                  (unless (listp beat) (setq beat (list (abs (floor beat)) (list (/ beat (abs beat))))))
                                  (multiple-value-bind (b chs)
                                                       (pw::beat-constructor (first beat) (second beat) ch-obs)
                                    (setq ch-obs chs)
                                    b))
                              (join-consecutive-beats beats))
                             tempo))
       voice-ms pulses))))

#+:OM
(defun construct-pw&om-measures (voice-ms tempo ch-obs signs)
  (make-instance 
    'om::voice
    :tree
    (list 'om::? 
          (mapcar 
           #'(lambda (beats pulse)
               (list (read-from-string (format nil "~A//~A" (first pulse) (second pulse))) (join-consecutive-beats beats)))
           voice-ms signs))
    :chords ch-obs
    :tempo tempo))




(defun fix-accum (accum)
  (if *before-first-beat* (- (floor (abs accum))) accum))

(defun join-consecutive-beats (beats)
  (let (result (accum 0))
    (dolist (beat beats (nreverse (if (zerop accum) result (push accum result))))
      (if (numberp beat)
        (cond ((zerop accum) (incf accum beat))
              ((and (integerp beat) (= beat 1))
               (push (fix-accum accum) result) (setq accum beat)
               (setf *before-first-beat* nil))
              ((and (integerp accum) (plusp beat)) (incf accum (floor beat)))
              ((and (plusp accum) (minusp beat))
               (push (fix-accum accum) result) (setq accum beat))  
              (t (incf accum beat)))
        (progn
          (unless (zerop accum) (push (fix-accum accum) result) (setq accum 0))
          (push (list (first beat) (join-consecutive-beats (second beat))) result))))))

#|
(defun join-consecutive-beats (beats)
  (let (result (accum 0))
    (dolist (beat beats (nreverse (if (zerop accum) result (push accum result))))
      (if (numberp beat)
        (cond ((zerop accum) (incf accum beat))
              ((and (integerp beat) (= beat 1)) (push accum result) (setq accum beat))
              ((and (integerp accum) (plusp beat)) (incf accum (floor beat)))
              ((and (plusp accum) (minusp beat)) (push accum result) (setq accum beat))  
              (t (incf accum beat)))
        (progn
          (unless (zerop accum) (push accum result) (setq accum 0))
          (push (list (first beat) (join-consecutive-beats (second beat))) result))))))
|#

;;; add optional arg mode GAS 5/10/98
(defun group-list (list1 segm &optional mode)
  (remove nil
          (let (aux aux1 elem (k 0))
            (dolist (n segm (reverse aux1))
              (dotimes (m n)
                (when (setq elem (nth k list1)) (push  elem aux))
                (incf k))
              (push (reverse aux) aux1)
              (setf aux nil)))
          ))

;;(group-list '(1 2 3 4 5 6 7) '(3 4))

#+:OM
(defun list-explode (list nlists)
  "list-explode divides a list into <nlist> sublists of consecutives elements.  
For example, if list is (1 2 3 4 5 6 7 8 9), and ncol is 2, the result is ((1 2 3 4 5) 
(6 7 8 9)),
if list is (1 2 3 4 5 6 7 8 9), and ncol is 5, the result is: ((1 2) (3 4) (5 6) (7 8) (9)). 
If the number of divisions exceeds the number of elements in the list, the 
remaining divisions are returned as nil."
  (if (> nlists (length list)) 
    (setq list (append list (make-list (- nlists (length list)) :initial-element (first (last list))))))
  (if (<= nlists 1) list
      (let* ((length (length list))
             (low (floor length nlists))
             (high (ceiling length nlists))
             (step (if (< (abs (- length (* (1- nlists) high))) (abs (- length (* nlists low))))
                     high  low))
             (rest (mod length nlists))
             (end (- length 1 rest)) 
             (ser (om::arithm-ser 0 (1- step) 1))
             res)
        (do ((i 0 (+ i step))) ((> i end))
          (push (remove () (mapcar #'(lambda (n) (nth n list)) (om::om+ i ser))) res))
        (setq low (length (flat-once res)))
        (if (< low length) (setq res (cons (append (first res) (nthcdr low list)) (rest res))))
        (cond ((> (length res) nlists) 
               (nreverse (cons (nconc (second res) (first res)) (nthcdr 2 res))))
              ((< (length res) nlists)
               (when (= (length (first res)) 1)
                 (setq res (cons (nconc (second res) (first res)) (nthcdr 2 res))))
               (nreverse (nconc (nreverse (list-explode (first res) (1+ (- nlists (length res)))))
                                (rest res))))
              (t (nreverse res))))))


          
