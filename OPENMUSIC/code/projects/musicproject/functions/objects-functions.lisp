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


;==========================================================================
;   Tools
;==========================================================================


(defmethod get-obj-parent ((self chord))
  "Returns the root father of object, or object itself if it is not a child"
  (if (parent self)
    (get-obj-parent (parent self))
    self))

(defmethod get-obj-parent ((self group))
  (if (parent self)
    (get-obj-parent (parent self))
    self))

(defmethod get-obj-parent ((self measure))
  (if (parent self)
    (get-obj-parent (parent self))
    self))

(defmethod get-obj-parent ((self voice))
  (if (parent self)
    (get-obj-parent (parent self))
    self))

(defmethod get-obj-parent ((self poly))
  (if (parent self)
    (get-obj-parent (parent self))
    self))

(defmethod get-obj-parent ((self chord-seq))
  (if (parent self)
    (get-obj-parent (parent self))
    self))

(defmethod get-obj-parent ((self t)) nil)


;=========================================================================


(defmethod get-obj-offset ((self t)) nil)
(defmethod get-obj-offset ((self poly)) 0)
(defmethod get-obj-offset ((self voice)) 0)

 
(defmethod get-obj-offset ((self measure))
  "Returns the offset of <self> starting from Score objects."
 (let ((parent (parent self)))
    (if parent
        (+ (get-obj-offset parent) (offset->ms self))
      (offset->ms self))))
 
(defmethod get-obj-offset ((self group)) 
 (let ((parent (parent self)))
    (if parent
        (+ (get-obj-offset parent) (offset->ms self))
      (offset->ms self))))
 
(defmethod get-obj-offset ((self chord)) (parent self)
  (let ((parent (parent self)))
    (if parent
        (+ (get-obj-offset parent) (offset->ms self))
      (offset->ms self))))

(defmethod get-obj-offset ((self rest)) (parent self)
  (let ((parent (parent self)))
    (if parent
        (+ (get-obj-offset parent) (offset->ms self))
      (offset->ms self))))


;=========================================================================
;k.h 28/09/20
;----------Voice2voices

#|
(defun dbl-tempo-p (tempo)
  "Indicates first tempo of measure exists as in:
   ((1/4 60) (((0 0) (1/8 120 t))))"
(let* ((cartempo (car tempo))
       (cdrtempo (cdr tempo))
       (frsttempopos (caaar cdrtempo))
       (frsttemposig (butlast (cadaar cdrtempo))))
  (if (equal frsttempopos '(0 0))
      t nil)))
|#

(defun dbl-tempo-p (tempo)
  "Indicates first tempo of measure exists as in:
   ((1/4 60) (((0 0) (1/8 120 t))))"
(let* ((cartempo (car tempo))
       (cdrtempo (cdr tempo))
       (allfrst (mapcar #'car (car cdrtempo)))
       (frsttempopos (caaar cdrtempo))
       (frsttemposig (butlast (cadaar cdrtempo))))
  (if (member '(0 0) allfrst :test 'equal)
      t nil)))

;;;;;ordered cadr tempo

(defun list< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((= (first a) (first b)) (list< (rest a) (rest b)))
        (t (< (first a) (first b))) ))


;(sort (copy-seq '(((119 0) (1/4 96 nil)) ((0 0) (1/4 160 nil)) ((0 1) (1/4 160 nil)))) 
;     #'(lambda (x y) (list< (car x) (car y))))

(defmethod ordered-cadr-tempo ((self list))
  (sort self #'(lambda (x y) (list< (car x) (car y)))))

;(ordered-cadr-tempo '(((119 0) (1/4 96 nil)) ((10 1 2) (1/4 160 nil)) ((0 0) (1/4 160 nil)) ((0 1) (1/4 160 nil))))







(defmethod fix-frst-tempo ((self voice))
  "fixes first measure, when it has tempo changes.Should be fixed at the source of the factory!"
  (let* ((tempo (tempo self))
         (head (car tempo))
         (other (cadr tempo))
         (fix (if other
                  (loop for i in other
                        collect 
                        (let ((tete (car i))
                              (queue (second i)))
                           (if (= 2 (length queue))        
                        (list (car i) (flat (list (second i) nil)))
                             i)))
                ))
         )
    (list head fix)))

(defmethod cadr-tempo ((self list))
  "pushes init tempo in cadr tempo list if there are cadr tempo list doesnot start as ((0 0) ....)"
  (let* ((cr (car self))
         (dr (cadr self))
         (ordered-dr (ordered-cadr-tempo dr)))
    (if (not (dbl-tempo-p self)) 
        (let ((frst (list '(0 0) (flat (list cr nil )))))
          (append (list frst) dr)) dr)))

(defmethod cadr-tempo ((self voice))
  (cadr-tempo (fix-frst-tempo self)))

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
          


(defun reset-offsets (tree)
  (loop for x in tree
        collect (if (atom x)
                  (progn 
                    (setf (offset x) 0)
                    x)
                  (reset-offsets x))))

        
(defmethod! translate-tempo ((self voice))
  (let ((tempi (group-tempi-by-meas
                 (meas-tempo self))))

         (loop for i in tempi
               collect (if (= 1 (length i))
                           (list (butlast (second (car i))) nil)
                         (let* ((cr (car i))
                                (head (butlast (second cr)))
                                (cd (cdr i))
                                (tail (loop for i in cd
                                            collect (cons 
                                                     (list 0 (second (car i)))
                                                     (cdr i)))))
                           
                           (list head tail)
                           ))))
                         
         )




;for split (not used ?)
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
   :doc "Splits down a <self>, a VOICE or a POLY  into measures converted to a list of VOICES or POLYS."

  (let* ((measures (reset-offsets (inside (clone self))))
         (chords (loop for i in measures
                       collect (chords i)))
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
                                 :tempo tp
                                 :approx (approx self)))))


;(defmethod! voice->voices ((self poly))
;            (let ((voices (inside self)))
;              (mapcar #'voice->voices voices)))



(defmethod! voice->voices ((self poly))
(let* ((voices (inside self))
      (split (loop for i in voices
                   collect (voice->voices i))))
  (loop for i in (mat-trans split)
        collect (make-instance 'poly
                               :voices (remove 'nil i)))))


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
   

(defmethod concatenate-voices ((liste list))
   (let* ((voices (flat (mapcar #'voice->voices liste)));;important if a voice contains more than one measure.
          (trees (mapcar #'tree voices))
          (conc-tree (list '? (flat-once (flat-once (mapcar 'cdr trees)))))
          (chords (remove 'nil (flat (mapcar 'chords voices))))
          (tempo (concatenate-tempi voices))
          )
     (make-instance 'voice
                    :tree conc-tree
                    :chords chords
                    :tempo tempo
                    :approx (approx (car voices)))))

(defmethod concatenate-polys ((liste list))
  (let* ((voices (mat-trans (loop for i in liste
                                  collect (inside i))))
         (concate (loop for i in voices  
                        collect (concatenate-voices (remove 'nil i)))))
    (make-instance 'poly 
                   :voices concate)))

;kept for compatibility.
(defmethod! concat-voices ((liste list))
   :initvals (list t) 
   :indoc '("list of voices")
   :icon 217
   :doc "concatenates a list of voices or a list of poly  into one voice or poly."
   (if (voice-p (car liste)) 
       (concatenate-voices liste)
     (concatenate-polys liste)))

(defmethod! concat-chord-seq ((liste list))
  (let ((res (car liste)))
    (loop for i in (cdr liste)
            do (setf res (concat res i)))
    res))

(defmethod! concat-multi-seq ((liste list))
  (let ((res (car liste)))
    (loop for i in (cdr liste)
            do (setf res (concat res i)))
    res))

(defmethod! concat-score-objs ((liste list))
   :initvals (list t) 
   :indoc '("list of voices")
   :icon 217
   :doc "concatenates a list of voices or a list of poly  into one voice or poly."
   (cond ((voice-p (car liste)) 
          (concatenate-voices liste))
         ((poly-p (car liste))
          (concatenate-polys liste))
         ((chord-seq-p (car liste))
          (concat-chord-seq liste))
         ((multi-seq-p (car liste))
          (concat-multi-seq liste))
         (t (om-beep-msg "No applicable method for elements in this list"))))

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

(defmethod! chord-filter ((self note) 
                          (chords list)
                          &key
                          (mode 'pass))
                                                 
  :icon 658
  :indoc '("self" "chords" "mode")
  :initvals '( t '(6000 6100) 'pass)
  :menuins '((2 (("pass" pass) 
                 ("reject" reject))))
  :doc "Filters <self> according to pitch list  <chords>,
<mode> by default is pass. <self> is either a chord or a chord-seq." 

  (let ((midic (midic self)))
    (case mode
      (pass (if (member midic chords :test '=) self))
      (reject (if (not (member midic chords :test '=)) self)))))


(defmethod! chord-filter ((self chord) 
                          (chords list)
                          &key
                          (mode 'pass))
  
  (let* ((notes (inside self))
         (flt (remove nil
                      (loop for i in notes
                            collect (chord-filter i chords :mode mode)))))
    (if flt (objfromobjs flt (make-instance 'chord)))))


(defmethod! chord-filter ((self chord-seq)
                          (chords list)
                          &key   
                          (mode 'pass))

  (let* ((chrds (inside self))
         (midics (if (listp chords) chords
                   (flat (lmidic chords))))
         (flt (loop for i in chrds
                    collect (chord-filter i chords :mode mode)))
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
       

(defmethod! chord-band-filter ((self voice) 
                               (min number) 
                               (max number)
                               &key   
                               (mode 'pass))
  (let* ((chords (chords self))
         (flt (loop for i in chords
                      collect (chord-band-filter i min max :mode mode)))
         (pos (remove nil (loop for i in flt
                    for n from 0 to (length flt)
                      collect (if i n)))))
    (make-instance 'voice
                   :tree (select-tree (tree self) pos)
                   :chords (remove nil flt)
                   :tempo (tempo self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;       TOOLS     ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod get-cont-chords (self)
"returns only chords of <self> omitting grace notes. grace notes will still be in the 
gnotes slot of chord"
  (loop for item in (collect-chords self)
        when (cont-chord-p item) collect item))

(defmethod fix-contchord ((self voice))
  (let ((chords (get-cont-chords self)))
    (loop for i in chords
          do (let* ((prev (previous-container i '(chord)))
                    (chan (lchan prev))
                    (port (lport prev)))
               (setf (lchan i) chan 
                     (lport i) port)
               (loop for n in (inside i) do (setf (tie n) (state i)))))))

(defmethod fix-contchord ((self poly))
  (mapcar #'fix-contchord (inside self)))


(defmethod update-obj ((self t)) t)

(defmethod update-obj ((self chord))
  "when <self>'s panel is openned, updates graphic contents"
(let ((box (associated-box self))) 
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))
  
(defmethod update-obj ((self chord-seq))
(let ((box (associated-box self)))
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))


(defmethod update-obj ((self multi-seq))
(let ((box (associated-box self)))
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))

(defmethod update-obj ((self measure))
  (let ((box (associated-box self))) 
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))

(defmethod update-obj ((self voice))
  (let ((box (associated-box self))) 
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))

(defmethod update-obj ((self poly))
  (let ((box (associated-box self))) 
    (when box
      (let ((editor (editorframe box))) 
        (when editor
            (update-panel (panel editor)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;  SET-OBJ-TEMPO      ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun qtempo->tempo (qtempo)
  (cond ((atom qtempo) 
         (list (list 1/4 qtempo) nil))
        ((listp (caar qtempo))
         (let* ((frst (second (car qtempo)))
                (newfrst (list 1/4 frst))
                (rst (cdr qtempo))
                (indx (mapcar 'caar rst))
                (tempi (mapcar 'second rst))
                (newrst (loop for i in tempi
                              for n in indx
                              collect (list (list n 0) (list 1/4 i nil)))))
           (cons newfrst (list newrst))))
        (t qtempo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-tempo ((self voice) tempo &key (mode :clone))
  :initvals '(t '((1/4 60) nil) :clone)
  :indoc '("voice or poly" "tempo" "mode")
  :menuins '((2 (("clone" :clone) 
                 ("destructive" :destructive))))
  :icon 355
  :doc "Changes tempo of <self>. 
If <mode> is clone, this will output a new voice (default).
If <mode> is destructive, tempo of <self> will be destructively changed." 
  (let ((tempo (qtempo->tempo tempo))) 
    (if (not (equal mode :destructive))
        (let ((new (clone self)))
        (setf (tempo new) tempo)
        new)
        (progn 
          (setf (tempo self) tempo)
          (update-obj self)))))


(defmethod! set-obj-tempo ((self poly) tempo  &key (mode :clone))
  (if (listp (caar tempo))
      (if (not (equal mode :destructive))
          (let ((clone (clone self)))
            (loop for i in (inside clone)
                  for tp in tempo
                  do (set-obj-tempo i tp :mode :destructive))
            clone)
        (progn
          (loop for i in (inside self)
                for tp in tempo
                do (set-obj-tempo i tp :mode :destructive))
          (update-obj self)))
    (if (not (equal mode :destructive))
        (let ((clone (clone self)))
          (loop for i in (inside clone)
                do (set-obj-tempo i tempo :mode :destructive))
          clone)
      (progn
        (loop for i in (inside self)
              do (set-obj-tempo i tempo :mode :destructive))
        (update-obj self)))
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-VEL     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-vel ((self note) 
                          (vel number)
                          &key (mode 'clone) (n nil))
  :initvals '(t 64 nil 'clone) 
  :indoc '("self" "chan" "n")
  :menuins '((2 (("clone" 'clone) 
                 ("destructive" 'destructive))))
  :icon 355
  :doc  "Changes midi channels in a voice or poly object. If it is a poly object
you can specify which voice will be changed by openning the optional input <n>.
Further more, in voice method, if list inputed, it will change all notes according to the list."
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (setf (vel clone) vel)
        clone)
    (setf (vel self) vel)))


(defmethod! set-obj-vel ((self chord) 
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              do (set-obj-vel i vel :mode 'destructive))
        clone)
    (progn
      (loop for i in (inside self)
            do (set-obj-vel i vel :mode 'destructive))
      (update-obj self))))

(defmethod! set-obj-vel ((self chord) 
                          (vel list)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              for vl in vel
              do (set-obj-vel i vl :mode 'destructive))
        clone)
    (progn
      (loop for i in (inside self)
            for vl in vel
            do (set-obj-vel i vl :mode 'destructive))
      (update-obj self))))
    
(defmethod! set-obj-vel ((self chord-seq) 
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for ob in objs
              do (set-obj-vel ob vel :mode 'destructive))
        clone)
    (progn
      (loop for ob in (inside self)
            do (set-obj-vel ob vel :mode 'destructive))
      (update-obj self))))

(defmethod! set-obj-vel ((self chord-seq) 
                          (vel list)
                          &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for i in vel
              for ob in objs
              do (set-obj-vel ob i :mode 'destructive))
        clone)
    (progn
      (loop for i in vel
            for ob in (inside self)
            do (set-obj-vel ob i :mode 'destructive))
      (update-obj self))))


(defmethod! set-obj-vel ((self multi-seq) (vel number) &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for vx in n 
                    do (set-obj-vel (nth vx voices) vel :mode 'destructive))
              clone)
          (progn
            (loop for vx in (inside clone)
                  do (set-obj-vel vx vel :mode 'destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for vx in n 
                  do (set-obj-vel (nth vx voices) vel :mode 'destructive)))
        (loop for vx in (inside self)
              do (set-obj-vel vx vel :mode 'destructive)))
      (update-obj self))))

(defmethod! set-obj-vel ((self multi-seq) (vel list) &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for i in vel
                    for vx in n 
                    do (set-obj-vel (nth vx voices) i :mode 'destructive))
              clone)
          (progn
            (loop for i in vel
                  for vx in (inside clone)
                  do (set-obj-vel vx i :mode 'destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for i in vel
                  for vx in n 
                  do (set-obj-vel (nth vx voices) i :mode 'destructive)))
        (loop for i in vel
              for vx in (inside self)
              do (set-obj-vel vx i :mode 'destructive)))
      (update-obj self))))
  


(defmethod! set-obj-vel ((self voice)
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-vel chrdseq vel :mode 'destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-vel chrdseq vel :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-vel ((self voice)
                          (vel list)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-vel chrdseq vel :mode 'destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-vel chrdseq vel :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-vel ((self poly)
                          (vel number)
                          &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-vel multiseq vel :mode 'destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-vel multiseq vel :mode 'destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

(defmethod! set-obj-vel ((self poly)
                         (vel list)
                         &key (mode 'clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-vel multiseq vel :mode 'destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-vel multiseq vel :mode 'destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-CHAN     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-chan ((self note) 
                          (chan number)
                          &key (mode :clone) (n nil))
  :initvals '(t 1 nil 'clone) 
  :indoc '("self" "chan" "n")
  :menuins '((2 (("clone" :clone) 
                 ("destructive" :destructive))))
  :icon 355
  :doc  "Changes midi channels in a voice or poly object. If it is a poly object
you can specify which voice will be changed by openning the optional input <n>.
Further more, in voice method, if list inputed, it will change all notes according to the list."
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (setf (chan clone) chan)
        clone)
    (setf (chan self) chan)))


(defmethod! set-obj-chan ((self chord) 
                          (chan number)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              do (set-obj-chan i chan :mode :destructive))
        clone)
    (progn
      (loop for i in (inside self)
            do (set-obj-chan i chan :mode :destructive))
      (update-obj self))))

(defmethod! set-obj-chan ((self chord) 
                          (chan list)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              for ch in chan
              do (set-obj-chan i ch :mode :destructive))
        clone)
    (progn
      (loop for i in (inside self)
            for ch in chan
            do (set-obj-chan i ch :mode :destructive))
      (update-obj self))))
    
(defmethod! set-obj-chan ((self chord-seq) 
                          (chan number)
                          &key (mode :clone) (n nil))
  (if (equal  mode :clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for ob in objs
              do (set-obj-chan ob chan :mode :destructive))
        clone)
    (progn
      (loop for ob in (inside self)
            do (set-obj-chan ob chan :mode :destructive))
      (update-obj self))))

(defmethod! set-obj-chan ((self chord-seq) 
                          (chan list)
                          &key (mode :clone) (n nil))
  (if (equal  mode :clone)
      (let* ((clone (clone self))
             (objs (inside clone)))
        (loop for i in chan
              for ob in objs
              do (set-obj-chan ob i :mode :destructive))
        clone)
    (progn
      (loop for i in chan
            for ob in (inside self)
            do (set-obj-chan ob i :mode :destructive))
      (update-obj self))))


(defmethod! set-obj-chan ((self multi-seq) (chan number) &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for vx in n 
                    do (set-obj-chan (nth vx voices) chan :mode :destructive))
              clone)
          (progn
            (loop for vx in (inside clone)
                  do (set-obj-chan vx chan :mode :destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for vx in n 
                  do (set-obj-chan (nth vx voices) chan :mode :destructive)))
        (loop for vx in (inside self)
              do (set-obj-chan vx chan :mode :destructive)))
      (update-obj self))))

(defmethod! set-obj-chan ((self multi-seq) (chan list) &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for i in chan
                    for vx in n 
                    do (set-obj-chan (nth vx voices) i :mode :destructive))
              clone)
          (progn
            (loop for i in chan
                  for vx in (inside clone)
                  do (set-obj-chan vx i :mode :destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for i in chan
                  for vx in n 
                  do (set-obj-chan (nth vx voices) i :mode :destructive)))
        (loop for i in chan
              for vx in (inside self)
              do (set-obj-chan vx i :mode :destructive)))
      (update-obj self))))
  


(defmethod! set-obj-chan ((self voice)
                          (chan number)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-chan chrdseq chan :mode :destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-chan chrdseq chan :mode :destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-chan ((self voice)
                          (chan list)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-chan chrdseq chan :mode :destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-chan chrdseq chan :mode 'destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-chan ((self poly)
                          (chan number)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-chan multiseq chan :mode :destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-chan multiseq chan :mode :destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

(defmethod! set-obj-chan ((self poly)
                          (chan list)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-chan multiseq chan :mode :destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-chan multiseq chan :mode :destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-PORT     ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod! set-obj-port ((self note) 
                        (port number)
                        &key (mode :clone) (n nil))
  :initvals '(t 0 nil 'clone) 
  :indoc '("self" "port" "n")
  :menuins '((2 (("clone" :clone) 
                 ("destructive" :destructive))))
  :icon 355
  :doc  "Changes midi port in a voice or poly object. If it is a poly object
you can specify which voice will be changed by openning the optional input <n>.
Further more, in voice method, if list inputed, it will change all notes according to the list."
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (setf (port clone) port)
        clone)
    (setf (port self) port)))

(defmethod! set-obj-port ((self chord) 
                          (port number)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              do (set-obj-port i port :mode :destructive))
        clone)
    (progn
      (loop for i in (inside self)
            do (set-obj-port i port :mode :destructive))
      (update-obj self))))

(defmethod! set-obj-port ((self chord) 
                          (port list)
                          &key (mode :clone) (n nil))
    (if (equal mode :clone)
      (let ((clone (clone self)))
        (loop for i in (inside clone)
              for p in port
              do (set-obj-port i p :mode :destructive))
        clone)
      (progn
        (loop for i in (inside self)
              for p in port
              do (set-obj-port i p :mode :destructive))
        (update-obj self))))
    
(defmethod! set-obj-port ((self chord-seq) 
                        (port number)
                        &key (mode 'clone) (n nil))
  (if (equal  mode 'clone)
       (let* ((clone (clone self))
              (objs (inside clone)))
         (loop for ob in objs
               do (set-obj-port ob port :mode :destructive))
         clone)
    (progn
      (loop for ob in (inside self)
            do (set-obj-port ob port :mode :destructive))
      (update-obj self))))

(defmethod! set-obj-port ((self chord-seq) 
                        (port list)
                        &key (mode :clone) (n nil))
  (if (equal  mode :clone)
       (let* ((clone (clone self))
              (objs (inside clone)))
         (loop for i in port
               for ob in objs
               do (set-obj-port ob i :mode :destructive))
         clone)
    (progn
    (loop for i in port
          for ob in (inside self)
          do (set-obj-port ob i :mode :destructive))
    (update-obj self))))

(defmethod! set-obj-port ((self multi-seq) (port number) &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for vx in n 
                    do (set-obj-port (nth vx voices) port :mode :destructive))
              clone)
          (progn
            (loop for vx in (inside clone)
                  do (set-obj-port vx port :mode :destructive))
            clone)))
    (progn
      (if n
          (let ((voices (inside self)))
            (loop for vx in n 
                  do (set-obj-port (nth vx voices) port :mode :destructive)))
        (loop for vx in (inside self)
              do (set-obj-port vx port :mode :destructive)))
      (update-obj self))))


(defmethod! set-obj-port ((self multi-seq) (port list) &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let ((clone (clone self)))
        (if n
            (let ((voices (inside clone)))
              (loop for i in port
                    for vx in n 
                    do (set-obj-port (nth vx voices) i :mode :destructive))
              clone)
          (progn
            (loop for i in port
                  for vx in (inside clone)
                  do (set-obj-port vx i :mode :destructive))
            clone)))
    (progn
    (if n
        (let ((voices (inside self)))
          (loop for i in port
                for vx in n 
                do (set-obj-port (nth vx voices) i :mode :destructive)))
        (loop for i in port
              for vx in (inside self)
              do (set-obj-port vx i :mode :destructive))
        )
    (update-obj self))))
  


(defmethod! set-obj-port ((self voice)
                        (port number)
                        &key (mode :clone) (n nil))
    (if (equal mode :clone)
        (let* ((clone (clone self))
               (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
          (set-obj-port chrdseq port :mode :destructive)
          (setf (chords clone) (inside chrdseq))
          clone)
      
      (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
        (set-obj-port chrdseq port :mode :destructive)
        (setf (chords self) (inside chrdseq))
        (update-obj self))))

(defmethod! set-obj-port ((self voice)
                          (port list)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (chrdseq (objfromobjs clone (make-instance 'chord-seq))))
        (set-obj-port chrdseq port :mode :destructive)
        (setf (chords clone) (inside chrdseq))
        clone)
      
    (let ((chrdseq (objfromobjs self (make-instance 'chord-seq))))
      (set-obj-port chrdseq port :mode :destructive)
      (setf (chords self) (inside chrdseq))
      (update-obj self))))

(defmethod! set-obj-port ((self poly)
                          (port number)
                          &key (mode :clone) (n nil))
  (if (equal mode :clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-port multiseq port :mode :destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-port multiseq port :mode :destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

(defmethod! set-obj-port ((self poly)
                          (port list)
                          &key (mode :clone) (n nil))
  (if (equal mode 'clone)
      (let* ((clone (clone self))
             (multiseq (objfromobjs clone (make-instance 'multi-seq))))
        (set-obj-port multiseq port :mode :destructive)
        (loop for i in (inside clone)
              for chrds in (inside multiseq)
              do (setf (chords i) (inside chrds)))
        clone)
    (let ((multiseq (objfromobjs self (make-instance 'multi-seq))))
      (set-obj-port multiseq port :mode :destructive)
      (loop for i in (inside self)
            for chrds in (inside multiseq)
            do (setf (chords i) (inside chrds)))
      (update-obj self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;      SET-OBJ-PITCH    ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod! set-obj-pitch ((self voice) 
                           (newpitch list) 
                           &key 
                           (mode 'clone)
                           (nth nil)) 
  :initvals '(t '(6000 6100) 'clone nil) 
  :indoc '("self" "newpitch" "mode" "nth")
  :menuins '((2 (("clone" clone) 
                 ("destructive" destructive))))
  :icon 355
  :doc  "Changes pitches in a voice or poly object. 
<mode> keyword can be clone, <self> isunchanged. If :mode = destructive, <self> will be mnodified.
If <self> is a poly object
if key <nth> is specified, nth voices will be changed. 
If <nth> is nil (default), the pitches will be distributed vertically in the poly."

  (if (equal mode 'clone)
      (make-instance 'voice 
                     :tree (tree self) 
                     :chords newpitch
                     :tempo (tempo self))
    (setf (chords self) newpitch)))
    

(defmethod! set-obj-pitch ((self voice) 
                           (newpitch number) 
                           &key 
                           (mode 'clone) 
                           (nth nil))
   (let* ((npuls (n-pulses (tree self)))
          (pitch (repeat-n newpitch npuls)))
     (if (equal mode 'clone)
     (make-instance 'voice 
                    :tree (tree self) 
                    :chords pitch
                    :tempo (tempo self))

     (setf (chords self) pitch))))


(defmethod! set-obj-pitch ((self voice) 
                           (newpitch chord-seq) 
                           &key 
                           (mode 'clone) 
                           (nth nil))
  (let ((chords (inside newpitch)))
  (if (equal mode 'clone)
      (make-instance 'voice 
                     :tree (tree self) 
                     :chords chords
                     :tempo (tempo self))

    (setf (chords self) chords))))


(defmethod! set-obj-pitch ((self poly) 
                           (newpitch list)
                           &key 
                           (mode 'clone)
                           (nth nil))
  (if (equal mode 'clone)
      (let* ((newself (clone self)))
        (if nth
            (progn
              (mapcar #'(lambda (nth-voices pitches) 
                          (setf (nth nth-voices (inside newself))
                                (set-obj-pitch (nth nth-voices (inside newself)) pitches)))
                      nth newpitch)
              newself)
        (verticalize-pitch newself newpitch)))
        (if nth
            (mapcar #'(lambda (nth-voices pitches) 
                (setf (nth nth-voices (inside self))
                      (set-obj-pitch (nth nth-voices (inside self)) pitches)))
            nth newpitch)
          (verticalize-pitch self newpitch))
        ))

(defmethod! verticalize-pitch ((self poly) (pitch list))
   :initvals (list t '(6000 6100)) 
   :indoc '("poly" "pitches")
   :icon 355
   :doc "In first input a poly object.
In the second object a simple list of pitches. The method will return
a poly object with the same rythm but with new pitches. These 
will occur as a melodic line, i.e the same order given in <pitch> but 
distributed in each voice according to their offsets. Meaning that the
melodic line will be distributed in all voices. It is important to know 
the total pitches needed throughout the poly i.e in all voices. "
  (let* ((voices (inside self))
         (tempi (mapcar #'tempo (inside self)))
         (vox->chrdsq (mapcar #'(lambda (x) (Objfromobjs x (make-instance 'chord-seq)))
                              voices))
         (voxoffsets (mapcar #'(lambda (x) (butlast (lonset x)))
                             vox->chrdsq))
         (voxnum  (loop for i from 0 to (- (length voices) 1)
                        for vx in voices 
                        collect (repeat-n i (length (chords vx)))))
          
         (listdata  (mapcar #'(lambda (onsets pitch) (mat-trans (list onsets pitch)))
                            voxoffsets
                            voxnum))
         
         (sortedata (sort. (flat-once listdata) '< 'first))
         (index (mapcar 'second sortedata))
         (resultat (repeat-n '() (length (inside self))))
         (parsednotes (loop 
                       for i in pitch 
                       for indx in index
                       do (push i (nth indx resultat))))
         (ordered (loop for i in resultat
                        collect (reverse i)))
         (trees (loop for i in (inside self)
                      collect (tree i))))

   
    (make-instance 'poly 
                   :voices (loop for i in trees
                                 for chrd in ordered
                                 for tmp in tempi
                                 collect (make-instance 'voice
                                                        :tree i
                                                        :chords chrd
                                                        :tempo tmp )))



    ))

(defmethod! verticalize-pitch ((self poly) (chord-seq chord-seq))
  (let ((pitch (inside chord-seq)))
    (verticalize-pitch self pitch)))
