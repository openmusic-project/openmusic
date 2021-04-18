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
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================

;;; Object functions package
;;; For voice, chord-seq, etc...



(in-package :om)




;=========================================================================
;k.h 28/09/20
;----------Voice2voices


(defun dbl-tempo-p (tempo)
  "Indicates first tempo of measure exists as in:
   ((1/4 60) (((0 0) (1/8 120 t))))"
(let* ((cartempo (car tempo))
       (cdrtempo (cdr tempo))
       (frsttempopos (caaar cdrtempo))
       (frsttemposig (butlast (cadaar cdrtempo))))
  (if (equal frsttempopos '(0 0))
      t nil)))

(defmethod cadr-tempo ((self list))
  "pushes init tempo in cadr tempo list if there are cadr tempo list doesnot start as ((0 0) ....)"
  (let* ((cr (car self))
         (dr (cadr self))
         )
    (if (not (dbl-tempo-p self)) 
        (let ((frst (list '(0 0) (flat (list cr nil )))))
          (append (list frst) dr)) dr)))

(defmethod cadr-tempo ((self voice))
  (cadr-tempo (tempo self)))

;(cadr-tempo  '((1/4 60) (((1 0) (1/4 45 nil)) ((2 0) (1/8 50 nil)))))

(defmethod meas-tempo ((self voice))
  (let* ((lgt (length (inside self)))
         (tempo (cadr-tempo self))
         (init (second (car tempo)))
         (indx 0)
         res)

    (loop while tempo
          do (if  (= indx (caaar tempo))
                 (progn 
                    (push (car tempo) res)
                    (setf init (cadar tempo))
                    (pop tempo))
              
               (progn 
                 (incf indx)
                 (if (and (= indx (caaar tempo))
                          (= 0 (cadaar tempo)))
                     (progn
                       (push (car tempo) res)
                       (setf init (cadar tempo))
                       (pop tempo))
                   (push (list (list indx 0)  init) res)))
               ))
    (if (< (caaar res) (1- lgt))
      (progn
        (loop for i from (1+ (caaar res)) to (1- lgt)
              do (push (list (list i 0) (cadar res)) res))
        (reverse res))
    (reverse  res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

(defun group-tempi-by-meas (tempi)
  (let ((clone (clone tempi))
        (indx 0)
        (res (list '())))

    (loop while clone
          do (If (= indx (caaar clone))
                 (progn
                 (push (car clone) (car res))
                 (pop clone))
               (progn
               (incf indx)
               (push '() res))))
    (reverse
     (loop for i in res
          collect (reverse i)))))
          


(defun reset-offset (tree)
  (loop for x in tree
        collect (if (atom x)
                  (progn 
                    (setf (offset x) 0)
                    x)
                  (reset-offset x))))

        
(defmethod! translate-tempo ((self voice))
  (let ((tempi (group-tempi-by-meas
                 (meas-tempo self))))

         (loop for i in tempi
               collect (if (= 1 (length i))
                           (list (butlast (second (car i))) nil)
                         (let ((cr (car i))
                               (cd (cdr i)))
                           (if (last-elem (second cr))
                               (x-append (list (butlast (second cr)))
                                         (list i))
                           (x-append (list (butlast (second cr)))
                                     (list cd))
                           ))))
                         
         ))




;for split
(defun init-meas-tempo-pos (liste)
  (let ((clone (clone liste)))
    (loop for lst in clone
         do  (if (second lst)
              (loop for i in (second lst)
                    do (setf (caar i) 0)
                    )))
    clone))


(defmethod! voice->voices ((self voice))
   :initvals (list t) 
   :indoc '("voice")
   :icon 217
   :doc "Splits down a <voice> into measures converted to voices."

  (let* ((measures (reset-offset (inside (clone self))))
         (chords (loop for i in measures
                       collect (get-chords i)))
         (trees (loop for i in measures
                      collect (list '?
                                    (list (tree i)))))
         (tempi (init-meas-tempo-pos (translate-tempo self))))
         
    (loop for i in trees
          for k in chords
          for tp in tempi
          collect (make-instance 'voice 
                                 :tree i
                                 :chords k
                                 :tempo tp))))


(defun set-meas-pos (meas pos)
  (let ((clone (clone meas)))
    (loop for i in clone
          do (setf (caar i) (+ pos (caar i))))
   (reverse clone)))


(defun concatenate-tempi (liste)
  "takes a list of voices- par contre attention, les voices ne sont pas necessaiements a une mesure !!!"
  (let* ((tempi (mapcar 'tempo liste))
         (tempos (mapcar 'cadr-tempo tempi))
         (frst (car tempos))
         (rst (cdr tempos))
         (indx (caar (last-elem frst)))
         (res (list frst))
         )
    (loop for i in rst
          do (progn
               (push (reverse (set-meas-pos i (1+ indx))) res)
               (setf indx (+ 1 indx (caaar (last-elem res))))
               ))
     (setf res  (flat-once (reverse res)))
     (let ((res1 (list (car res))))
     (loop for i in (cdr res)
           do (if (not (equal (second i) (second (car res1))))
                  (push i res1)))
    (setf res (reverse res1)))
     (list (butlast (second (car res)))
           (cdr res))
    ))
   

(defmethod! concat-voices ((liste list))
   :initvals (list t) 
   :indoc '("list of voices")
   :icon 217
   :doc "concatenates a list of voices into one voice."
   (let* ((trees (mapcar #'tree liste))
          (conc-tree (list '? (flat-once (flat-once (mapcar 'cdr trees)))))
          (chords (remove 'nil (flat (mapcar 'chords liste))))
          (tempo (concatenate-tempi liste))
          )
     (make-instance 'voice
                    :tree conc-tree
                    :chords chords
                    :tempo tempo)
     ))


;;;;;;;;;


(defmethod cont-chord->chord ((self t)) t)
(defmethod cont-chord->chord ((self continuation-chord))
  (make-instance 'chord
                 :lmidic (lmidic self)
                 :lvel (lvel self)
                 :loffset (loffset self)
                 :ldur (ldur self)
                 :lchan (lchan self)
                 ))


;=========================================================================
;k.h 16/04/21
;----------chord-filter
;;a revoir

(defmethod! chord-filter ((self note) 
                          &key
                          (type 'band)
                          (mode 'pass)
                          (min 6000) 
                          (max 6100)
                          (chords '(6000)))
                         
  :icon 658
  :indoc '("self" "type" "mode" "min" "max" "chords")
  :initvals '( t 'band 'pass 6000 6100 (list 6000))
  :menuins '((1 (("band" band) 
                 ("selected" selected)))
             (2 (("pass" pass) 
                 ("reject" reject))))
  :doc "Filters <self> according to pitch range  <min> and <max> (inclusive).
<mode> by default is pass. <self> is either a chord or a chord-seq." 

  (let ((midic (midic self)))
    (if (equal type 'band)
    (case mode
        (pass (if (and (>= midic min) (<= midic max)) self))
        (reject (if (not (and (>= midic min) (<= midic max))) self)))
      (case mode
        (pass (if (member midic chords :test '=) self))
        (reject (if (not (member midic chords :test '=)) self))))))



(defmethod! chord-filter ((self chord) 
                          &key
                          (type 'band)
                          (mode 'pass)
                          (min 6000) 
                          (max 6100)
                          (chords '(6000)))
 
  (let* ((notes (inside self))
         (flt (remove nil
                      (loop for i in notes
                            collect (chord-filter i :type type :mode mode :min min :max max :chords chords)))))
    (if flt (objfromobjs flt (make-instance 'chord)))))


(defmethod! chord-filter ((self chord-seq) 
                          &key   
                          (type 'band)
                          (mode 'pass)
                          (min 6000) 
                          (max 6100)
                          (chords '(6000)))

  (let* ((chrds (inside self))
         (midics (if (listp chords) chords
                   (flat (lmidic chords))))
         (flt (loop for i in chrds
                    collect (chord-filter i :type type :mode mode :min min :max max :chords midics)))
         ons)
    (loop for i in flt
          for on in (lonset self)
          do (if i (push on ons)))
   (if flt
    (let ((chrdseq (objfromobjs (remove nil flt) 
                                (make-instance 'chord-seq))))
      (setf (lonset chrdseq) (reverse ons))
      chrdseq))))
       
;=========================================================================
;k.h 17/04/21
;----------chord-band-filter


(defmethod! chord-band-filter ((self note) 
                               (min number) 
                               (max number)
                               &key
                               (mode 'pass))
                         
  :icon 658
  :indoc '("self" "min" "max" "mode")
  :initvals '( t 6000 6100 'pass)
  :menuins '((3 (("pass" pass) 
                 ("reject" reject))))
  :doc "Filters <self> according to a pitch range  <min> and <max> (inclusive).
<mode> by default is pass. <self> is either a chord or a chord-seq." 

  (let ((midic (midic self)))
    (case mode
      (pass (if (and (>= midic min) (<= midic max)) self))
      (reject (if (not (and (>= midic min) (<= midic max))) self)))))



(defmethod! chord-band-filter ((self chord) 
                               (min number) 
                               (max number)
                               &key
                               (mode 'pass))
 
  (let* ((notes (inside self))
         (flt (remove nil
                      (loop for i in notes
                            collect (chord-band-filter i  min max :mode mode)))))
    (if flt (objfromobjs flt (make-instance 'chord)))))


(defmethod! chord-band-filter ((self chord-seq) 
                               (min number) 
                               (max number)
                               &key   
                               (mode 'pass))

  (let* ((chords (inside self))
         (flt (loop for i in chords
                    collect (chord-band-filter i min max :mode mode)))
         ons)
    (loop for i in flt
          for on in (lonset self)
          do (if i (push on ons)))
   (if flt
    (let ((chrdseq (objfromobjs (remove nil flt) 
                                (make-instance 'chord-seq))))
      (setf (lonset chrdseq) (reverse ons))
      chrdseq))))
       
