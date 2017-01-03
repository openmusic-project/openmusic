


(in-package :om)


;;;=======================================
;;;TRUE DURATIONS
;;;=======================================


;;;Fixed quand il y a un silence et une note
;;; updated 11/05/2009 by kh

;;;Very new one. Should work even with polyphonic chord-seqs!
;;;;fixed on 050411

(defun normalize-chord-seq (chrdseq)
  (let* ((xdx (x->dx (lonset chrdseq)))
         (filt-durs1 (mapcar 'list-min (ldur chrdseq)))
         (lst-durs (mapcar 'list xdx filt-durs1))
         (filt-durs2 (mapcar 'list-min lst-durs))
         (newdurs (loop 
                   for pt in (lmidic chrdseq)
                   for drs in filt-durs2
                   collect (repeat-n drs (length pt)))))
    (make-instance 'chord-seq 
                   :lmidic (lmidic chrdseq)
                   :lonset (lonset chrdseq)
                   :ldur newdurs)))

(defmethod! true-durations ((self t)) 
  :icon 134
  :indoc '("a score object")
  :doc "Gives the durations in milliseconds of om object including rests
(rest are negative numbers).
IMPORTANT: chord-seqs with overlapping notes will be cropped according to
next note, legato=100."
  (let* ((newchrdseq (if (typep self 'note) 
                         (Objfromobjs (Objfromobjs self (make-instance 'chord)) (make-instance 'chord-seq))
                       (Objfromobjs self (make-instance 'chord-seq))))

         (newcs (normalize-chord-seq newchrdseq))
         (onsets (Lonset newcs))
         (dur (Ldur newcs))
         (newonsets (if (= 2 (length onsets)) (x->dx  onsets) (butlast (x->dx onsets))))
         (newdurs (mapcar 'first dur))
         (resultat1 
          (x-append 
           (flat
            (list (mapcar #'(lambda (x y) (if (= 0 (- x y)) x 
                                            (list x (- x y))))
                          newdurs newonsets)
                  (last newdurs)))
           (last-elem newdurs)))
         (resultat2 (butlast
                     (if (= 0 (first onsets)) resultat1 (cons (* -1 (first onsets)) resultat1)))))
    
    (let ((result (remove nil (mapcar #'(lambda (x) (if (not (or (= x 1) (= x -1))) x ))
                                      resultat2))))
      (if (= 2 (length onsets)) (list (car result) (second result)) result))
    )
  )