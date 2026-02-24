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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad    
;              
;=========================================================================
;Author: Karim Haddad
;DocFile
;This file defines an experimental quantifier [In progress]
;Last Modifications :
;03/03/2022 first date.
;DocFile


(in-package :om)

;=============================================
;TOOLS
;=============================================


(defvar *quantminval* nil)

(defun getngrace (durs tempi measures max/
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
    (do ()  ((null atimes))
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

    (setq result (nreverse result))
    ;(print (set-grace-notes-pos *global-grace-notes* positive-durs))
    (let* ((pos (mapcar 'car (set-grace-notes-pos *global-grace-notes* positive-durs)))
           (decalage (arithm-ser 0 (length pos) 1)))
     ; (print (list pos decalage *global-grace-notes* positive-durs));;ya qq chose ici !
      (om+ pos decalage));;;les positions de set-grace doivent etre decales!
    ))


(defmethod* get-n-grace ((durs list) (tempi t) (measures list)
                            (max/ t)
                            &optional
                            forbid
                            offset
                            precis)
  :initvals '((100) 60 (4 4) 8  nil 0 0.5)
  :icon 252
  :indoc '("durations (list)" "tempo" "list of time signature(s)" "maximum subdivision"  "list forbidden subdivisions" "grace-notes?" "precision (0.0-1.0)")
  :doc "Returns grace places in given duration list."

  (unless precis (setq precis 0.5))
  (setf *distance-weight*  (if (numberp precis) (list (list precis)) precis))
  (let ((rep (getngrace (om/ durs 10) tempi measures  (if (numberp max/) (list (list max/))
                                                max/) forbid (or offset 0) 1)))
    rep
  ))

;;;pour trouver min val de la derniere gracenote
;;;60000 / (tempo * (denom / 4))
;;; unite en ms = (/ 60000 ( * tempo ( / denom 4)))
;;;THis is a gross workaround!!!!!!!!!

;;;pour trouver min val de la derniere gracenote
;;;60000 / (tempo * (denom / 4))
;;; unite en ms = (/ 60000 ( * tempo ( / denom 4)))
;(defun quant-minval (tempo sig max-sub)
;"this one returns the minimum value of a note in ms before being considered by omquantify as a gracenote.
;NOTE: it is really the correct value of omquantify, but it seems that omquantify is not that 
;clever... So carefull it is for just one isolated value. Sometimes omquatify tricks itself bzarely.
;so keep this just for our buisness, i.e to fix last note if it is a gracenote---this note is for myself. K.H"
;(let* ((unit (/ 60000 ( * tempo ( / (second sig) 4))))
;       (minval (+ 1 (floor (/ (/ unit max-sub) 2))))
;       )
;  
;  minval))


(defun quant-minval (tempo sig max-sub)
"this one returns the minimum value of a note in ms before being considered by omquantify as a gracenote.
NOTE: it is really the correct value of omquantify, but it seems that omquantify is not that 
clever... So carefull it is for just one isolated value. Sometimes omquatify tricks itself bzarely.
so keep this just for our buisness, i.e to fix last note if it is a gracenote---this note is for myself. K.H"
(let* ((unit (/ 60000 ( * tempo ( / (second sig) 4))))
       (minval (floor (/ unit max-sub)))
       )
  
  minval))



(defmethod* chord+ ((self1 chord) (self2 chord))
 
  (let* ((offsets (append (loffset self1) (loffset self2)))
         (chord  (merger self1 self2)));;le prob de merger, revient a la note qui bousille les offsets
    chord))

(defmethod* chord+ ((self1 chord) (self2 t))
  (let* ((offsets (loffset self1))
         (chord  self1))
    chord))


(defun graceoffcounter (pos)
  (setf res nil)
  (let ((buf (car pos)))
    (loop for i in pos
          do
          (if (= i (+ 1 buf)) 
              (progn 
                (push (+ 10 (caar res)) (car res))
                (setf buf i))
            (progn
              (push (list 10) res)
              (setf buf i))))
    (loop for i in (reverse res) 
          collect (om* -1 i))
    ))
;(graceoffcounter '(1 2 3 31 33 35 37 56 57 58 60 61))
                   
(defmethod* set-grace-offset ((chords list) (pos list))
  (let ((clone (clone chords))
         (offsets (flat (graceoffcounter pos))))
    ;(print offsets)
    (loop for i in pos
          for p in offsets
          do (progn 
               (setf (loffset (nth i clone)) (list p))
               (loop for n in (inside (nth i clone))
                     do 
                     (setf (mus-color n) *om-red-color*)
                     );bug in offset notes (divided by 10 ?)
               ))
          clone))


(defun concat-grace-chord (chords pos)
  (let ((res (set-grace-offset chords pos))
        (lgt (length chords)))
    (loop for i in pos
          do (progn
               (if (not (= (1- i) lgt)) ;pour la derniere aftergrace
                   (setf (nth (+ 1 i) res) (chord+ (nth i res) (nth (+ 1 i) res)))
                 )
               ))    
    (loop for i in pos
          do (if (not (= (1+ i) lgt)) 
                 (setf (nth i res) nil)))
    (remove 'nil res)))


;=====================================
;KANT-WITH-GRACE
;=====================================

;kant-with-grace -> kant-grace -> kant-self-grace

;kant-with-grace (first level) doesn't give the correct rests
;starting from kant-grace, it's good


;;;in /quant.wrk/quantification/bugs/

(defun make-zero-chord ()(make-instance 'chord :lmidic '(0)))

(defun rest-subst-zero-mc (durations chords)
  (let* ((res '())
         (clone (clone chords)))
    (loop for i in durations
          collect (if (minusp i) 
                      (push (make-zero-chord) res)
                    (progn
                      (push (car clone) res)
                      (pop clone))))
    (reverse res)))


(defun pre-quant-grace (durations lmidic)
  (let* ((notes (rest-subst-zero-mc durations lmidic)))
    (list (om-abs durations) notes)))
    

(defun filter-zeromc (mc)
  (loop for i in mc
        collect (remove 0 i)))



(defun filt-zerochord (chord)
  (let* ((midic (lmidic chord)))
    (remove 'nil
            (loop 
             for n in (inside chord)
             for m in midic
             collect (if (not (zerop m))
                         n)))))


(defun filter-zerochords (voice)
  (let ((chords (chords voice)))
    (remove 'nil
            (loop for i in chords
                  collect (let ((notes (filt-zerochord i)))
                            (if notes 
                              (objfromobjs notes (make-instance 'chord));problem here
                              ))))))

(defun filtertree-zeromc (voice)
  (let* ((chords (chords voice))
         (midics (mapcar 'lmidic chords))
         (flt (filter-zeromc midics))
         (pos (member-pos 'nil flt)))
    (filtertree (tree voice) pos 'rests)))


(defun post-quant-grace (voice)
  (let* ((tree (filtertree-zeromc voice))
         (chords (filter-zerochords voice)))
    (make-instance 'voice 
                   :tree tree
                   :chords chords
                   :tempo (tempo voice)
                   )))
         
  
(defmethod! gkant  ((durs list) (chords list) (tempi t) (measures list)
                             (max/ t)
                             &optional
                             forbid
                             offset
                             precis)
   :initvals '((100) nil 60 (4 4) 8  nil 0 0.5)
   :icon 252
   :indoc '("durations" "pitch" "Tempo" "measures" "maximum division"  "forbidden
divisions" "grace-notes?"
            "precission")
   :doc "Applies omquantify on list, chord-seq and voices taking into consideration grace notes that wil lbe concatenated into chords (in red) avoiding skipping notes.
For more info, see omquantify help."

   (let* ((chords 
           (let ((n-durs (length (remove 'nil 
                                         (loop for i in durs
                                               collect (if (plusp i) i))))))
             (if (null chords) (repeat-n (make-instance 'chord)  n-durs) chords)))
          (data (pre-quant-grace durs chords)) ;ici ca mets lmidic= 0
          (durations (car data))
          (notes (second data))
          (minval 
           (if (all-atoms? measures)
               (quant-minval tempi measures max/)
             (quant-minval tempi (car measures) max/)))
          (durs (if ( >= (last-elem durs) minval) durations (x-append (butlast durations) minval)))        
          (pos (get-n-grace durs tempi measures max/ forbid offset precis))
          (new-chords (concat-grace-chord notes pos))
          (tree (omquantify durs tempi measures max/ forbid offset precis))
          (quant (make-instance 'voice 
                    :tree tree
                    :chords new-chords
                    :tempo tempi
                    ;:legato 100
                    ))
          )
     (post-quant-grace quant); ca refiltre les midic = 0
     ))


  
(defmethod! gkant  ((self voice)
                    (chords list)
                     (tempi t) 
                     (measures list)
                     (max/ t)
                     &optional
                     forbid
                     offset
                     precis)
   
   (let ((durs (true-durations self))
         (chords (if chords chords (chords self))))
     (gkant durs chords tempi measures max/ forbid offset precis)))

(defmethod! gkant  ((self poly) 
                    (chords list)
                    (tempi t) 
                    (measures list)
                    (max/ t)
                    &optional
                    forbid
                    offset
                    precis)

            (let* ((voices (inside self))
                  (kants (loop for i in voices
                               collect (gkant i chords tempi measures max/ forbid offset precis))))
              (make-instance 'poly
                             :voices kants)))

(defmethod! gkant  ((self chord-seq)
                    (chords list)
                     (tempi t) 
                     (measures list)
                     (max/ t)
                     &optional
                     forbid
                     offset
                     precis)
   
   (let ((durs (true-durations self))
         (chords (if chords chords (chords self))))
     (gkant durs chords tempi measures max/ forbid offset precis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;OMG-QUANTIFY;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defmethod! omg-quantify  ((self list) (tempi t) (measures list)
                             (max/ t)
                             &optional
                             forbid
                             offset
                             precis)
   :initvals '((100) 60 (4 4) 8  nil 0 0.5)
   :icon 252
   :indoc '("durations" "Tempo" "measures" "maximum division"  "forbidden
divisions" "grace-notes?"
            "precission")
   :doc "Same as omquantify but displays gracenotes.
For more info, see omquantify help."
   (let* ((tree (omquantify self tempi measures max/ forbid offset precis))
          (graces (get-n-grace self tempi measures max/ forbid offset precis))
          (lgt (length graces))
          (pos (om- graces (arithm-ser 0 lgt 1)))
          (ins (insert-graces tree pos (repeat-n 1 lgt))))
     (format-grace-notes ins)))
|#


(defun getgnpos (lst)
  (remove nil 
          (loop for n from 0 to (length lst)
                for i in lst
                collect (if (and (chord-p i)
                                 (> (length (lmidic i)) 1))
                            (list n (1- (length (lmidic i))))))))

;Uses gkant which avoids the rest quantification problem.

(defmethod! omg-quantify  ((self list) (tempi t) (measures list)
                             (max/ t)
                             &optional
                             forbid
                             offset
                             precis)
   :initvals '((100) 60 (4 4) 8  nil 0 0.5)
   :icon 252
   :indoc '("durations" "Tempo" "measures" "maximum division"  "forbidden
divisions" "grace-notes?"
            "precission")
   :doc "Same as omquantify but displays gracenotes.
For more info, see omquantify help."
   (let* ((voice (gkant self nil tempi measures max/ forbid offset precis))
          (tree (tree voice))
          (chords (collect-chords-and-rests voice))
          (gnpos (mat-trans (getgnpos chords))))
     (add-tree-graces tree (car gnpos) (second gnpos))))


;;; VOICE => VOICE


(defmethod! omg-quantify  ((self voice) (tempi t) (measures list)
                             (max/ t)
                             &optional
                             forbid
                             offset
                             precis)
  
  (let* ((durs (true-durations self))
          (chords (chords self))
          (tree (omg-quantify durs tempi measures max/ forbid offset precis)))
     (make-instance 'voice
                    :tree tree
                    :chords chords
                    :tempo tempi)))

(defmethod! omg-quantify  ((self poly) (tempi t) (measures list)
                             (max/ t)
                             &optional
                             forbid
                             offset
                             precis)
  (let* ((voices (inside self))
         (quants (loop for i in voices
                         collect (omg-quantify i tempi measures max/ forbid offset precis))))

     (make-instance 'poly
                    :voices quants)))
        

;;; CHORD-SEQ => VOICE

(defmethod* omg-quantify ((self chord-seq) 
                        (tempi t) (measures list)
                        (max/ t)
                        &optional
                        forbid
                        offset
                        precis)
  (let ((tree
         (omg-quantify (true-durations self)
                     tempi measures max/ forbid offset precis))
        (chords (get-chords self))
        (tempo (if (atom tempi) (list tempi) tempi)))
    (make-instance 'voice
                   :tree tree
                   :chords chords
                   :tempo (format-omtempo 1/4 tempo))))



