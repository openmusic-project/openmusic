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
;;; authors G. Assayag, C. Agon, K. Haddad
;=========================================================================
;
;
;============================================================================
; File author: Karim Haddad
;============================================================================



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



(om::defmethod! true-durations ((self t)) 
  :icon 217
  :doc "Gives the durations in milliseconds of an om object including rests (rests are negative numbers).
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


;in case voice is empty:
(om::defmethod! true-durations ((self voice)) 
  (if (= (n-pulses self) 0)
      (list (* (get-obj-dur self) -1))
    (true-durations (Objfromobjs self (make-instance 'chord-seq)))))
