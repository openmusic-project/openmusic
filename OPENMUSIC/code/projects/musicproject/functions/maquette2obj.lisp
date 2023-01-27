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
;=========================================================================


;; ==================================================================================== 
;;                               MAQUETTE2OBJ
;; ==================================================================================== 

;;; =================================================================================== 
;;;
;;;                       © 2001  Karim Haddad - IRCAM
;;;
;;; =================================================================================== 


(in-package :om)

;-----------------------------------trac-for-mid---------------------------------------
;--------------------------quantify-maquette--------------------------------------

(defun transform-seq (maquette)
  "every voice or poly is transformed into a chord-seq or multiseq"
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
(parse-mid-tracks '(("a"  1 3 (45 54))
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



(defmethod* maquette2sound ((maquette maquette-obj) &optional (outsound nil))
   :icon 333
   :indoc '("maquette" "pathname")
   :doc "Outputs a sound file mixing the present sound files in the maquette." 
   (let* ((sndlist (remove nil 
                           (loop for b in (inside maquette) collect
                                 (if (subtypep (type-of b) 'sound) b nil))))
          (nch (apply 'max (mapcar 'om-sound-n-channels sndlist)))
          (sr (apply 'max (mapcar 'om-sound-sample-rate sndlist)))
          sndmix)
     (setf sndmix (reduce 'sound-mix (mapcar #'(lambda (b) (sound-seq (sound-silence (offset b) nch sr) b)) sndlist)))
     (save-sound sndmix outsound)))


(defmethod* maquette2sound ((maquette ommaquette) &optional (outsound nil))
   (if (and (value self) (typep (value self) 'maquette-obj))
       (maquette2sound (value self) outsound)
     (om-beep-msg "The maquette output must be a maquette-obj (check your synth-patch?)")))
   