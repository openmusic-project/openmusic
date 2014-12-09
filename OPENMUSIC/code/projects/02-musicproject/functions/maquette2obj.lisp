
;; ==================================================================================== 
;;                               MAQUETTE2OBJ
;; ==================================================================================== 

;;; =================================================================================== 
;;;
;;;                       © 2001  Karim Haddad - IRCAM
;;;
;;; =================================================================================== 





(in-package :om)


;=====================================utilities================================

;-----------------------------reducetree--------------------------------------

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

(om::defmethod! reducetree1 ((tree t))
   :initvals '(? ((4//4 (1 (1 (1 2 1 1)) 1 1)) (4//4 (1 (1 (1 2 1 1)) -1 1))))
   :indoc '("tree")
   :icon 134
   :doc "reduces and simplifies a tree by concatenating consecutive rests and floats
into a single correct note"
   (let* ((tree (if (typep tree 'voice) (tree tree) tree))
         (liste (resolve-? tree)))
(grouper3 (grouper2 (grouper1 liste)))))


;-----------------------------true-durations--------------------------------------


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

(om::defmethod! true-durations ((self t)) 
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







;-----------------------------------trac-for-mid---------------------------------------
;--------------------------quantify-maquette--------------------------------------

(defun transform-seq (maquette)
"everythy voice or poly is transformed into a chord-seq or multiseq"
  (let ((obj (inside maquette)))
    (remove nil (loop
      for i in obj
      collect (cond
               ((or (typep i 'note) (typep i 'chord) (typep i 'midifile))
                (objfromobjs i (make-instance 'chord-seq)))
               ((typep i 'voice)
                (objfromobjs i (make-instance 'chord-seq )))
               ((typep i 'poly)
                (objfromobjs i (make-instance 'multi-seq)))
               ((typep i 'chord-seq) i)
               ((typep i 'multi-seq) i)
               )))))


(defun find-maq-inside (maquette)
  (loop 
    for i in (inside maquette)
    collect (remove nil (if (typep i 'maquette) i))))

;;; pb qd on cree l'image
(defun transform-seq-mergeopt (maquette)
"everythy voice or poly is transformed into a chord-seq or multiseq"
  (let ((obj (inside maquette)))
    (remove nil (loop for i in obj collect 
                                    (cond 
                                               ((or (typep i 'note) (typep i 'chord) (typep i 'midifile))
                                                (objfromobjs i (make-instance 'chord-seq)))
                                               ((typep i 'voice)
                                                (objfromobjs i (make-instance 'chord-seq )))
                                               ((typep i 'poly)
                                                (objfromobjs i (make-instance 'multi-seq)))
                                               ((typep i 'chord-seq) i)
                                               ((typep i 'multi-seq) i)
                                               ((typep i 'maquette-obj) 
                                                (merge-maquette i))
                                               (t nil)
                                     )
                                    ))
    ))



(defun get-offsets (maquette)
  (let ((objs (inside maquette)))
    (flat (remove nil 
                            (loop for i in objs collect 
                                      (cond ((or (typep i 'note )
                                                         (typep i 'chord)
                                                         (typep i 'chord-seq)
                                                         (typep i 'midifile)
                                                         (typep i 'voice)
                                                         (typep i 'maquette-obj)) (offset i))
                                                  ((or (typep i 'multi-seq)
                                                         (typep i 'poly)) (repeat-n (offset i) (length (inside i))))
                                                  ;((typep i 'om::sound) nil)
                                                  (t nil)
                                                  )
                               
                                      )))
    ))




(defun track-mid-parser (list)

(let* ((lstarg list)
       (lstinit '())
       (lstgc '()))
  (progn 
    (push (first lstarg) lstinit)
    (pop lstarg))
  
(loop
  for i in lstarg
  do (if (> (second i) (third (first lstinit)))
  (progn 
    (push i lstinit)
    (pop lstarg))

  (progn 
    (push i lstgc)
    (pop lstarg))))
(list (reverse lstinit) (reverse lstgc))))

(defun parse-mid-tracks (maq-info)
  (let ((lst (sort. maq-info #'< 'second))
        (result '()))
    (do (
         (lust lst 
               (progn 
                 (push (first (track-mid-parser lust)) result)
                 (setf lust (second (track-mid-parser lust))))))
        ((null lust) (reverse result)))))


#|

;list format (<name> <offset> <ending>) 
( parse-mid-tracks '(("a"  1 3 (45 54))
               ("b"  2 4 (45 54))
               ("c"  5 8 (45 54))
               ("d"  6 9 (45 54))
               ("e"  7 11 (45 54))
               ("f"  10 12 (45 54))))

|#




(defun append-last-durs (lst)
 (let* ((total-durs (loop 
                     for i in lst
                     collect 
                     (reduce '+ (om-abs i) :initial-value 0)
                     ;(apply '+ (om-abs i))
                     ))
        (max-val (list-max total-durs))
        (end-vals(om- total-durs max-val)))
   (loop 
    for i in lst
    for a in end-vals
    collect (if (= 0 a) i
              (let ((lst (reverse i)))
                (reverse (push a lst))))))) 


(defmethod! maquette2obj ((maquette maquette-obj) 
                              (mode symbol)
                              &optional
                              (tempi 60)
                              (measures '(4 4))
                              (max/ 8)
                              (forbid nil)
                              (offset 0)
                              (precis 0.5))
                
                :icon 333
                :initvals '(t 'poly  60 (4 4) 8  nil 0 0.5)
                :indoc '("maquette" "mode" "tempo" "measures" "maximum division"  "forbidden divisions" "grace-notes?"  "precision")
                :menuins '((1 (("poly" 'poly)
                               ("multi-seq" 'multi-seq)
                               ("sound" 'sound)
                               )))

                :doc "
Creates a poly, multi-seq, or sound object (depending on <mode>) starting from the result of <maquette>.

If a sub-maquette is embedded, all its contents is merged into a single voice. 

If <mode> = 'poly', he optional inputs allow to set the quantification parameters (see OMQUANTIFY)
If <mode> = 'sound', a sound file is created by mixing the present sound files in the maquette. Other objects are ignored."

(if (equal mode 'sound) (maquette2sound maquette)
   (let* ((objs (transform-seq-mergeopt maquette))
          (offsets ;(om* -1 (get-offsets maquette));offsets of tempobjs in main maquette
           (get-offsets maquette))
          (objects (flat (loop 
                           for i in objs
                           collect (if (typep i 'multi-seq) (inside i) i))));builds chordseqs from tempobj (if multi->n-chrdseq)
          (durations (loop
                       for i in objects
                       collect (true-durations i)));total durs of chordseqs (tempobjs)
          
          (endings (loop
                     for dur in durations
                     for offs in offsets
                     collect (+ offs 
                                (reduce '+ (om-abs dur) :initial-value 0)
                               ;(apply '+ (om-abs dur))
                                )));endings off chordseqs
          (max-end (list-max endings));max ending
          
          (parsed-tracks 
           (let
             ((pmt (parse-mid-tracks (mat-trans (list objects offsets endings durations)))))
             (loop 
               for tracks in pmt
               collect (mat-trans tracks))));group chordseqs by tracks
          
          (treedurs
           (loop 
             for tracks in parsed-tracks
             collect (let* ((chords (first tracks))
                            (newoffs (cons (* -1 (car (second tracks))) (om- (third tracks)  (cdr (second tracks)))))
                            (durations (mapcar 'cons newoffs (fourth tracks))))
                       (remove 0 (flat durations)))));durations for each track for quantification

          (new-durs+offs (append-last-durs treedurs));same as above with the correct offset

          (treechords
           (loop 
             for tracks in parsed-tracks
             collect (flat (let* ((chords (flat (first tracks))))
                             (loop 
                               for i in chords
                               collect (inside i))))));sequence of chord obj for each track in order to preserve midi info

          ;;;pour multiseq
          (chrdseq-offs (let* ((chrdseqs (mapcar 'first parsed-tracks))
                               (local-offs (loop 
                                             for track in chrdseqs
                                             collect (mapcar 'lonset track)))
                               (real-offs (mapcar 'second parsed-tracks)))
    
                          (loop 
                            for seqs in local-offs
                            for offs in real-offs
                            collect (flat (loop 
                                            for i in seqs
                                            for y in offs
                                            collect (butlast (flat (om+ y i))))))
                          
                          ));gives the chord-seqs offsets for each track
          

          (chordseqs (loop 
                       for offs in chrdseq-offs
                       for chords in treechords
                       collect (make-instance 'chord-seq
                                 :lmidic chords
                                 :lonset offs))) ;make chord-seqs
          
          (multisq (make-instance 'multi-seq :chord-seqs chordseqs));build the multiseq

          )

     (case mode 

  (multi-seq  multisq)

;quantification

  (poly 
   
   (let* ((trees (loop 
                   for i in (inside multisq)
                   collect (omquantify (true-durations i) tempi measures max/  forbid offset precis)))
       (chords (loop 
                for i in (inside multisq)
                collect (lmidic i)))
       (voices (loop 
          for i in trees
          for a in chords
          collect (make-instance 'voice
                    :tree i
                    :chords a
                    :tempo tempi)))) 

(make-instance 'poly :voices voices))) 

))))



(defmethod! maquette2obj ((self ommaquette) (mode symbol)
                              &optional (tempi 60) (measures '(4 4)) (max/ 8) (forbid nil) (offset 0) (precis 0.5))
            (when (and (value self) (typep (value self) 'maquette-obj))
              (maquette2obj (value self) mode tempi measures max/ forbid offset precis)))
            
;must put the specific lets while choosing the mode not before
;because when asking for multiseq, there will be quantification
; and this is not necessary in this condition



(defun merge-maquette (maquette)
  "merges evrything in a maquette into a chord-seq"
  (let* ((objs (transform-seq maquette))
         (offsets ;(om* -1 (get-offsets maquette));offsets of tempobjs in main maquette
          (get-offsets maquette))
         (objects (flat (loop 
                          for i in objs
                          collect (if (typep i 'multi-seq) (inside i) i))));builds chordseqs from tempobj (if multi->n-chrdseq)
         (durations (loop
                      for i in objects
                      collect (true-durations i)));total durs of chordseqs (tempobjs)
         
         (endings (loop
                    for dur in durations
                    for offs in offsets
                    collect (+ offs 
                               (reduce '+ (om-abs dur) :initial-value 0)
                               ;(apply '+ (om-abs dur))
                               )));endings off chordseqs
         (max-end (list-max endings));max ending
         (parsed-tracks 
           (let
             ((pmt (parse-mid-tracks (mat-trans (list objects offsets endings durations)))))
             (loop 
               for tracks in pmt
               collect (mat-trans tracks))))
                   (treechords
           (loop 
             for tracks in parsed-tracks
             collect (flat (let* ((chords (flat (first tracks))))
                             (loop 
                               for i in chords
                               collect (inside i))))));sequence of chord obj for each track in order to preserve midi info

         
         (chrdseq-offs (let* ((chrdseqs (mapcar 'first parsed-tracks))
                              (local-offs (loop 
                                            for track in chrdseqs
                                            collect (mapcar 'lonset track)))
                              (real-offs (mapcar 'second parsed-tracks)))
                         
                         
                         
                         (loop 
                           for seqs in local-offs
                           for offs in real-offs
                           collect (flat (loop 
                                           for i in seqs
                                           for y in offs
                                           collect (butlast (flat (om+ y i))))))
                         
                         
                         
                         
                         ));gives the chord-seqs offsets for each track
         
         
         
         
         (chordseqs (loop 
                      for offs in chrdseq-offs
                      for chords in treechords
                      collect (make-instance 'chord-seq
                                :lmidic chords
                                :lonset offs))) ;make chord-seqs
         
         (newmulti (make-instance 'multi-seq
                     :chord-seqs chordseqs)) ;build the multiseq
         )
    (objfromobjs newmulti (make-instance 'chord-seq))))



(defmethod* maquette2sound ((maquette t) 
                                &optional (outsound nil))

   :icon 333
   :indoc '("maquette" "pathname")
  
   :doc "Outputs a sound file mixing the present sound files in the maquette." 
   (let ((sndlist (remove nil 
                          (loop for b in (inside maquette) collect
                                (if (subtypep (type-of b) 'sound) b nil))))
         sndmix)
     (setf sndmix (reduce 'sound-mix (mapcar #'(lambda (b) (sound-seq (sound-silence (offset b)) b)) sndlist)))
     (save-sound sndmix outsound)))