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

(in-package :om)

;============ GRACES NOTES

;-------play
(defvar *gdur* 20)
(setf *gdur* 100)



;ici ajouter les vel et les chan correspondant aux grace-chords!                    
(defmethod PrepareToPlay ((player t) (self chord) at &key approx port interval voice)
  (append 
   (when (gnotes self)
     (let ((chseq (make-instance 'chord-seq 
                                 :lmidic (mapcar 'lmidic (glist (gnotes self)))
                                 :lonset (list 0 *gdur*)
                                 :ldur (list (- *gdur* 1))
                                 :lvel (list (car (lvel self)))
                                 :lchan (list (car (lchan self))))))
       (PrepareToPlay player chseq (- at (* *gdur* (length (glist (gnotes self)))))
                      :approx approx 
                      :port port
                      :interval interval
                      :voice voice)))
   (call-next-method)))


;a voir
(defmethod PrepareToPlay ((player t) (self rest) at &key approx port interval voice)
  (append
   (when  (gnotes self)
     (let ((chseq (make-instance 'chord-seq 
                                 :lmidic (mapcar 'lmidic (glist (gnotes self)))
                                 :lonset (list 0 *gdur*)
                                 :ldur (list (- *gdur* 1)))))
       (PrepareToPlay player chseq (- at (* *gdur* (length (glist (gnotes self)))))
                      :approx approx 
                      :port port
                      :interval interval
                      :voice voice)))
   (call-next-method)))
                
;=======CLASS

(defclass* grace-notes () 
  ((glist :initform (list (make-instance 'grace-chord)) :accessor glist :initarg :glist)
   ;(gchords :initform (list (make-instance 'grace-chord)) :accessor gchords :initarg :gchords)
   (mus-color :initform *om-black-color* :accessor mus-color)
   (thechord :initform nil :accessor thechord  :initarg :thechord)
   (before? :initform t :accessor before? :initarg :before?)))

(defclass* grace-chord (chord) 
           ((pos :initform 0 :accessor pos  :initarg :pos); avoir si c'est necessaire
            (thechord :initform nil :accessor thechord  :initarg :thechord)))

(defmethod grace-chord-p ((self grace-chord)) t)
(defmethod grace-chord-p ((self t)) nil)


(defmethod* Objfromobjs ((Self chord) (Type grace-chord)) 
  (make-instance 'grace-chord
                 :lmidic (lmidic self)
                 :lvel (lvel self)
                 :ldur (ldur self)
                 :lchan (lchan self)
                 :approx (approx self)))
;(objfromobjs (make-instance 'chord) (make-instance 'grace-chord))

(defmethod offset->ms ((self grace-notes) &optional grandparent)
  (let ((thechord (thechord self)))
     (- (offset->ms thechord) 1)  ))

(defmethod! set-grace-notes ((self simple-container) chords before?)
  (setf (gnotes self) (make-instance 'grace-notes
                        :glist chords
                       ; :gchords (loop for i in chords collect (make-instance 'grace-chord :lmidic (lmidic i)))
                        :thechord self
                        :before? before?)))

(defmethod! add-grace-notes ((self voice) chord-pos pitches)
  (let* ((mesure (nth  (car chord-pos)  (inside self)))
         (chords (cons-chord&rest-list mesure))
         (thechord (nth (second chord-pos) chords)))
    (when thechord
      (set-grace-notes thechord pitches t))
    self))

(defmethod! add-grace-notes ((self t) chord pitches)
  (om-beep-msg (format nil "~D is not a VOICE or a POLY" self)))

;;ajout
(defmethod! insert-grace-note ((self voice) (pos list) (pitches list))
  (let ((chords (real-chords self)))
    (loop for i in pitches
          for n in pos
          do (setf self (set-grace-notes (nth n chords) i t)))))


;=======GRAPHIC CLASS

(defclas grap-grace-notes ();grap-container ;added heritage
  ((grc :initform nil)) )  ;a enlever de scoretools

(defmethod graces? ((self grap-grace-notes)) t)
(defmethod graces? ((self t)) nil)

(defclass s-grap-grace-notes (grap-grace-notes grap-ryth-chord) ())

(defclas g-grap-grace-notes (grap-grace-notes grap-group) ())


;**************************************************************
; GRAPHIC
;**************************************************************

;(defmethod add-grace-notes-dialog ((self simple-container))
;  (set-grace-notes self '((6000 6500 7200) (7400 7900 8100) (5400 5700)) t))

;just for testing purposes
(defmethod add-grace-notes-dialog ((self simple-container))
  (set-grace-notes self 
                   (loop for i in '((6000 6500 7200) (7400 7900 8100) (5400 5700))
                         collect (let ((chord (make-instance 'grace-chord :lmidic i :thechord self)))
                                   (setf (thechord chord) self)
                                   chord))
                   t))

(defmethod delete-grace-notes ((self simple-container))
  (setf (gnotes self) nil))

;=================================

; ----------------a borrar
;;pas utilise!
(defmethod grap-offset->ms ((self t) father)
   (offset->ms (reference self) father))

(defmethod grap-offset->ms ((self s-grap-grace-notes) father)
   (let ((thechord (reference (grc self))))
     (- (offset->ms thechord father) 2)))

(defmethod grap-offset->ms ((self g-grap-grace-notes) father) 
   (let ((thechord (reference (grc self))))
     (- (offset->ms thechord father) (*  (length (inside self)) 2))))

(defmethod grap-offset->ms ((self grap-ryth-chord) father)
  (let ((pere (parent self)))
   (if (graces? pere)
        (let ((thechord (reference (grc pere))))
         (- (offset->ms thechord father) (* (+ 1 (position self (reverse (inside pere)) :test 'equal)) 2)))
      (offset->ms (reference self) father))))


;==========================================
;----------BUILD
;==========================================
(defvar *grace-factor* 4/3)
;(setf *grace-factor* 10/6)
(setf *grace-factor* 5/4)

(defmethod make-graces-from-list ((self grace-notes) top staffsys linespace scale sel pere grc)
  ;(print (list "get grc" grc (reference grc) (mapcar 'lmidic (glist self)) pere))
  (let ((list (mapcar 'lmidic (glist self))))
    (when list
      (if (= (length list) 1)
          (make-simple-grace self top staffsys linespace scale sel pere grc)
        (make-group-grace self top staffsys linespace scale sel pere grc)))))

;--------simple
(defmethod make-simple-grace ((self grace-notes) top staffsys linespace scale sel pere grc)
   (let* ((list (mapcar 'lmidic (glist self)))
         (chord (car (glist self)))
         (thenotes (sort (copy-list (inside chord)) '< :key 'midic))
         (onlyhead (head-1/4))
         (beams-num 1)
         (zigzag-list (make-alt-zig-zag chord scale))
         (note-head-list (make-chord-zig-zag chord scale))
         (new-grace (make-instance 's-grap-grace-notes
                      :reference chord
                      :parent self
                      :grc grc))
         (maxw 0))
    (setf (inside new-grace)
          (loop for item in thenotes
                for pos in note-head-list
                for i = 0 then (+ i 1)
                collect
                (let ((notegrap (make-graph-ryth-obj item  top staffsys linespace  scale sel new-grace nil nil))
                      (alt-char (get-alt-char item scale (armure staffsys)))
                      notew)
                  (setf (points notegrap) 0)
                  (setf (delta-head notegrap) (round pos *grace-factor*))
                  (setf (headchar notegrap) onlyhead)
                  (when alt-char
                    (setf (alt-char notegrap) alt-char))
                  (when (natural-alt-char notegrap)
                    (setf (alteration notegrap) (correct-alteration notegrap (pop zigzag-list))))
                  (setf notew (round (* 2 (max (* linespace pos)
                                               (* linespace (if (alteration notegrap) (* -1 (- (alteration notegrap) 1)) 0)))) *grace-factor*))
                  (setf (rectangle notegrap) (list 0 0 notew (round linespace)))
                  (setf (nth 0 (main-point notegrap)) (round (* linespace pos) *grace-factor*))
                  (setf maxw (max maxw notew))
                  notegrap)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ; avoir pour les group
        
    (setf (qvalue (reference new-grace)) (qvalue (thechord self)))
    (setf (extent (reference new-grace)) (extent (thechord self)))
    (setf (offset (reference new-grace)) (offset (thechord self)))
    (setf (parent (reference new-grace)) (parent (thechord self)))   
    (setf (qtempo (reference new-grace)) (qtempo (thechord self)))
    ;;;;;;;;;;;;;;;;;;;;;;;;;


    (setf (beams-num new-grace) beams-num)
    (setf (stemhigh new-grace) (round (* 2.5 linespace) *grace-factor*))
    (setf (rectangle new-grace)  (list 0 0 maxw 0))
    new-grace))

(defun not-stem-dir (dir)
  (if (string-equal dir "up") "dw" "up"))

;-------group

(defmethod make-group-grace ((self grace-notes) top staffsys linespace scale sel pere grc) 
  (let* ((list (mapcar 'lmidic (glist self)))
         (group (make-instance 'group :tree (list (/ (length list) 2) (make-list (length list) :initial-element 1))))
         new-group direstart)
  ;;;;add   
 ;Leprobleme c'est que sel c'est la selection et non le chord d'attachement!! utiliser -> (thechord self)
    (setf (qvalue group) (qvalue (thechord self)))
    (setf (extent group) (extent (thechord self)))
    (setf (offset group) (offset (thechord self)))
    (setf (parent group) (parent (thechord self)))   
    (setf (qtempo group) (qtempo (thechord self)))
    
    (setf (inside group) (glist self));IMPORTANT pour pouvoir changer les notes des graces!!!!
  
    (loop for item in (inside group)
          do (setf (qtempo item) (qtempo group)))    
    
    (setf new-group (make-instance 'g-grap-grace-notes
                                   :grc grc
                                   :reference group
                                   :parent self))
       
    (setf (numdenom new-group)  nil)
    (loop for item in (inside group)
          for chord in list
          for n from 0 to (length list)
          do
            (progn
            (setf (offset item) n); allows us to display a group!
            (setf (lmidic item) chord)))

    (setf (inside new-group) (loop for item in (inside group) 
                                   for i = 0 then (+ i 1)
                                   collect
                                     (make-graph-ryth-obj  item  top staffsys linespace  scale sel new-group 1/8)))
    new-group))


;==========================================
;----------DRAW
;==========================================


;This one is from scoretools.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod draw-object-ryth ((self grap-measure) view x y zoom minx maxx miny maxy slot size linear?  staff chnote)
  (let ((space (get-chiffrage-space self size))
        (deltachiff (round size 5))
        (poly-p (poly-p (GET-ROOT-PARENT (reference self)))))
    (let ((previous (car (inside self))));nil)) ;previous should be a grap-ryth-chord
     
      (loop for item in (inside self) do
              (if (graces? item)
                  (progn ;(print (list "FUCK" (reference item) (car (main-point previous)))) ;item= grap-group-notes
                    (let ((off (* zoom (+ (car (main-point previous)) 8)))); previous a regler pour les miniview de merde!
                (draw-object-ryth item view (+ x space  off) y (/ zoom 10) minx maxx miny maxy slot size linear? staff chnote)
                ;(print (list "item" item slot (main-point previous) chnote))
                ))
                (draw-object-ryth item view (+ x space) y zoom minx maxx miny maxy slot size linear? staff chnote))
            ;;;=================
            (when (and *om-tonalite* (not *draw-mini-pict*))
              (setf previous (draw-modulation (parent self) item previous x y zoom size view))
              ;(print (list "previous" (main-point previous)))
              )
            ;;;=================
            ))
    (collect-rectangles self)
    (setf (nth 0 (rectangle self)) (- (nth 0 (rectangle self)) space))
    (om-with-fg-color nil *system-color*
      (loop for thestaff in (staff-list staff) do
            (let ((ys (+ y (line2pixel (posy thestaff) nil (/ size 4)))))
              (setf (nth 1 (rectangle self)) (min (nth 1 (rectangle self)) ys))
              (setf (nth 3 (rectangle self)) (max (nth 3 (rectangle self)) (+ ys (system-size-in-pix staff size))))
              (om-with-font (get-font-to-draw 2)
                            (cond 
                             ((first-of-group? self)
                              (om-draw-string  (+ (car (rectangle self)) deltachiff) (+ (round size 4) ys) (format () "~D" (caar (tree (reference self)))))
                              (om-draw-string  (+ (car (rectangle self)) deltachiff) (+ ys (round size 4) (round size 2)) (format () "~D" (cadar (tree (reference self)))))
                              )
                             ((show-chifrage self) 
                              (om-draw-string  (+ (car (rectangle self)) deltachiff) (+ (round size 4) ys) (format () "~D" (caar (tree (reference self)))))
                              (om-draw-string  (+ (car (rectangle self)) deltachiff) (+ ys (round size 4) (round size 2)) (format () "~D" (cadar (tree (reference self)))))
                              )
                             (t (let ((previous-mes (nth (- (position self (inside (parent self)) :test 'equal) 1) (inside (parent self)))))
                                  (unless (equal (metric self) (metric previous-mes))
                                    (om-draw-string  (+ (car (rectangle self)) deltachiff) (+ (round size 4) ys) (format () "~D" (caar (tree (reference self)))))
                                    (om-draw-string  (+ (car (rectangle self)) deltachiff) (+ ys (round size 4) (round size 2)) (format () "~D" (cadar (tree (reference self)))))
                                    )))))
              
              
              (unless (or (first-of-group? self) (subtypep (type-of view) 'polypanel)) ;; poly-p
                (om-draw-line  (car (rectangle self))  ys 
                               (car (rectangle self)) (+ ys size))))))
    
    
    (draw-extras self view size staff)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod draw-object-ryth ((self grap-grace-notes) view x y zoom minx maxx miny maxy slot size linear?  staff chnote)
  ;(print (list "grap" self (reference self) x))
  (om-with-fg-color nil *om-red-color* ;(mus-color (reference self)) ;ca c'est les hampes
    (draw-grace-notes self x y zoom minx maxx miny maxy slot size linear?  staff chnote)))

;----simple
(defmethod draw-grace-notes ((self s-grap-grace-notes) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
  (om-with-fg-color nil (mus-color (reference self))
    (let* ((dir (not-stem-dir (stemdir  (grc self))))
           (thenotes (inside self)));(copy-list (inside self))))
      (loop for item in thenotes do
            (draw-head-grace item x y zoom minx maxx miny maxy slot size linear? staff chnote))
      (collect-rectangles self)
      (om-with-font (om-make-music-font *heads-font* (round size *grace-factor*))
                    (draw-chord-grace-stem self x y zoom (beams-num self) dir (round size *grace-factor*))))))

(defmethod draw-head-grace ((self t) x y zoom minx maxx miny maxy slot size linear?  staff chnote); (print (list "head" (reference self)))
  (declare (ignore minx maxx miny maxy linear? grille-p))
  (let* ((new-size (round size *grace-factor*))
         (realrealpos (+ 1 x (* (/ new-size 4) (delta-head self)) (* zoom (- (x self) (* (/ new-size 4) (delta-head self))))))
         (realpos (round realrealpos))
         (altpos (if (alteration self) 
                   (round (- (+ realrealpos (* (- (alteration self) 1) (/ new-size 4))) (* (/ new-size 4) (delta-head self))))
                   realpos))
         (str (headchar self))
         (headsizex (get-name-size str (om-make-font *music-font* new-size)))
         (note (reference self))
         (note-color *om-red-color*);(get-mus-color note))
         (altstr (string (alt-char self)))  
         tie)
    (om-with-fg-color nil (if chnote (nth (chan (reference self)) *16-color-list*) note-color)
      (om-with-font (om-make-music-font *heads-font* new-size) 
                    (om-draw-string  realpos (+ y (y self))  str)) 
      (when (alteration self)
        (om-with-font (om-make-music-font *micron-font* new-size) 
                      (om-draw-string altpos (+ y (y self)) altstr)))
      
      (setf (rectangle self) (list altpos (+ y (- (y self) (round new-size 8)))
                                   (+ realpos (round new-size 3)) (+ y (round new-size 8) (y self)))))
    (draw-auxiliar-lines self x y  size realpos headsizex)))

(defmethod draw-chord-grace-stem ((self grap-ryth-chord) x0 y0 zoom numbeams  dir size)
  (let* ((domaine (om+ y0 (get-min-max self)))
         (taille (round (max (+ (/ size 4) (* (- numbeams 1) (/ size 3))) (* size 7/8)))) 
         (yfin  (if (string-equal dir "up") 
                  (- (car domaine)  taille)
                  (+ (second domaine)  taille)))
         (ystart (if (string-equal dir "up") (second domaine) (car domaine)))
         (xpos (if (string-equal dir "up") 
                 (round (+  1 x0 (/ size 3.5) (* zoom (x self))))
                 (round (+  x0  (* zoom (x self)))))))
    (draw-stem-no-group  xpos (selected self)  ystart  yfin)
    (if  (string-equal dir "up")
      (progn
        (draw-beam-string  xpos (round (+ (+ yfin (* 1/4 size)) )) (beam-up) (selected self))
        (om-draw-char  xpos (round (+ (+ yfin (* 1/2 size)) )) (code-char 111) ))
      (progn
        (draw-beam-string  xpos (round  yfin ) (beam-dwn) (selected self))
        (om-draw-char  xpos (round (+ (+ yfin 0) (* 1/8 size))) (code-char 112) )))))

;-------group

(defmethod figure-?  ((self g-grap-grace-notes)) (call-next-method))

(defmethod draw-grace-notes ((self g-grap-grace-notes) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
  ;(print (list "a;" (loop for ch in (inside self) collect (print (list (reference ch) ch)))))
  (loop for chord in (inside self) do
        (loop for item in (inside chord) do
              (draw-head-grace item (+ x 0) y zoom minx maxx miny maxy slot size linear? staff chnote))
        (collect-rectangles chord))
  (collect-rectangles self)
  (let ((dire (dirgroup self)))
    (om-with-font (om-make-music-font *heads-font* (round size *grace-factor*))
                  (group-draw-stems-gn self dire  x y (rectangle self)  zoom (round size *grace-factor*))
                  (draw-beams-note-in-group self dire x -1 (rectangle self)  zoom (round size *grace-factor*))
                  (if (string-equal dire "up")
                      (om-draw-char  (+ (car (rectangle self)) (round size 5)) (+ (second (rectangle self)) (round size 4)) (code-char 111) )
                    (om-draw-char  (- (third (rectangle self)) (round size 2.9)) (+ (fourth (rectangle self)) (round size 5)) (code-char 111) )))))


;==============STEMS

(defmethod get-atoms-in-group ((self grap-grace-notes)) nil)

(defmethod group-draw-stems-gn ((self g-grap-grace-notes) dir x y rect zoom size)
   (loop for item in (inside self) do
         (group-draw-stems item dir x y rect zoom size)))

(defmethod group-draw-stems ((self grap-grace-notes) dir x y rect zoom size) nil)

(defmethod draw-beams-note-in-group ((self g-grap-grace-notes) dir x y rect zoom size)
  (let ((atoms (inside self)))
    (loop for i from 0 to (- (length atoms) 1) do
          (let* ((cur-atom (nth i atoms))
                 (next-atom (nth (+ i 1) atoms))
                 (prev-atom (unless (zerop i) (nth (- i 1) atoms))) )
            (cond
             ((or (= i 0) (first-of-group? cur-atom))
              (setf shared-beams (if next-atom (set-shared-from-prev cur-atom next-atom) 0))
              (setf propres-beams (- (beams-num cur-atom) shared-beams))
              (when next-atom
                (draw-n-long-beams  cur-atom shared-beams dir x (ceiling (* zoom  (- (x next-atom) (x cur-atom))))  y rect zoom size))
              (draw-court-beams cur-atom propres-beams dir x y shared-beams rect zoom size))
             ((= i (- (length atoms) 1))
              (setf shared-beams (if prev-atom (set-shared-from-prev prev-atom cur-atom) 0))
              (setf propres-beams (- (beams-num cur-atom) shared-beams))
              (draw-court-beams cur-atom propres-beams dir (- x (* size 1/4)) y shared-beams rect zoom size))
             (t
              (cond 
               ((or (last-of-group? cur-atom) (first-of-group? next-atom)) 
                (setf shared-beams (if prev-atom (set-shared-from-prev prev-atom cur-atom) 0))
                (setf propres-beams (- (beams-num cur-atom) shared-beams))
                (draw-court-beams cur-atom propres-beams dir (- x (* size 1/4)) y shared-beams rect zoom size)
                (setf shared-beams (min 1 (beams-num next-atom) (beams-num cur-atom)))
                (draw-n-long-beams  cur-atom shared-beams  
                                    dir x  (ceiling (* zoom  (-  (x next-atom) (x cur-atom))))  y rect zoom size))                
               (t
                (setf shared-beams (if next-atom (set-shared-from-prev cur-atom next-atom) 0))
                (setf propres-beams (- (beams-num cur-atom) shared-beams))
                (draw-n-long-beams  cur-atom shared-beams  
                                    dir x  (ceiling (* zoom  (-  (x next-atom) (x cur-atom) )))  y rect zoom size)
                (if (and (<= (beams-num next-atom) (beams-num prev-atom)) (not (last-of-group? prev-atom))) 
                  (draw-court-beams cur-atom propres-beams dir (- x (* size 1/4)) y shared-beams rect zoom size)
                  (draw-court-beams cur-atom propres-beams dir x y shared-beams rect zoom size))))))))))



;;afficher bien les groupes
;;afficher bien la talla de un grupo que contiene grace notes
;export import de grace notes
;supprimer les grace notes
;editer les grace notes
;;mirar bien los methods que convierten a chord-seq
;;select en mode chord o group no debe tomar las graces
;;primero de una mesura es muy separado
;primero de la voice es negativo 
;tie chords repeat the grace notes

(defmethod get-chord&rest-not-graces ((self t)) nil)
(defmethod get-chord&rest-not-graces ((self grap-chord)) (list self))
(defmethod get-chord&rest-not-graces ((self grap-rest)) (list self))
(defmethod get-chord&rest-not-graces ((self grap-grace-notes)) nil)
(defmethod get-chord&rest-not-graces ((self grap-container)) 
   (loop for item in (inside self) append (get-chord&rest-not-graces item)))

(defmethod get-graph-type-obj ((self grap-grace-notes) type) nil)


(defmethod set-graces-dir-after ((self g-grap-grace-notes) figure staffsys linespace)
  (set-dir-and-high-g self (not-stem-dir (stemdir  figure)) linespace))

(defmethod set-graces-dir-after ((self s-grap-grace-notes) figure staffsys linespace) t)

(defmethod set-dir-and-high ((self grap-grace-notes) dir linespace) (call-next-method))


;only for group-graces
(defmethod set-dir-and-high-g ((self g-grap-grace-notes) dir linespace)
   (setf (dirgroup self) dir)
   (loop for item in (inside self) do
         (set-dir-and-high-g item dir linespace)))  ;poner para los grace)groupos


(defmethod set-dir-and-high-g ((self grap-ryth-chord) dir linespace)
   (setf (stemdir self)  dir)
   (setf (stemhigh self) (round (max (* 3 linespace) (+ (* 2 linespace) (* (/ (beams-num self) 2)  linespace))))))

;;;;;;;;;;;;ADDED
;Find or Create these functions:
; set-main-point
; grace-notes-space

(defmethod grace-notes-space ((self g-grap-grace-notes) size) 
  (loop for item in (inside self) maximize  (third (rectangle item))))


;a definir!
(defmethod set-main-point ((self g-grap-grace-notes) count) ;count est le car de main-point du chord des graces
  ;(print (list "insd" self count (inside self)))
    (loop for i in (inside self)
          ;for n from 0 to (length (inside self))
          for n = 0 then (+ n 1)
          do ;(setf (main-point i) (list (+ n (- count 12)) nil))
           ; (print (list "info" i))
            ;(setf (main-point i) (list (+ n count) nil)) ; ca decale le playing point...
            ;(setf (main-point i) (list 0 nil))
            (setf (main-point i) (list 0 nil))
          ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;CARLOS GRACES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; with lots of redefined methods from other files, and lost missing methods
;;; Indispensable.

;;graces should be notated as 0
;;grace grous as (0 (1 1 1 ))


(defclass* group-gn (metric-sequence)  
           ((tree :initform '(0 (1 1 1))  :initarg :tree :type list)
            (approx :initform *global-midi-approx* :accessor approx  :type integer))
           (:icon 226)
           (:documentation "
An OM object representing a group in a rhythm.
"))

(defmethod group-gn-p ((self group-gn)) t)
(defmethod group-gn-p ((self t )) nil )


(defmethod initialize-instance ((self group-gn)
                                &key (PropagateExtent 4) (InternalCall nil) (Empty nil))
  (declare (ignore initargs))
  (call-next-method))

(defmethod (setf tree) ((tree list) (self group-gn))
  (do-initialize-metric-sequence self :tree tree )
  (do-initialize self )
  self)

(defmethod (setf approx) ((approx number) (self group-gn))
  (call-next-method)
  (loop for i in (inside self)
        do (setf (approx i)  approx))
  self)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod init-seq-from-tree ((self metric-sequence) (tree list) &key (PropagateExtent 1)) 
  (let ((subtrees (second tree)) 
        (Extent (* (fullratio (first tree)) PropagateExtent))
        (nbsubunits (reduce  
                     #'(lambda (x y) (+  (abs x) (subtree-extent y))) 
                     (second tree)
                     :initial-value 0))
        (curr-obj nil)
        (current-graces nil) 
        (current-note nil))
    (remove NIL
            (loop 
               for subtree in subtrees
               do (setf curr-obj 
                        (cond
                         ;;;for group graces!
                         ((and (listp subtree) (equal 0 (car subtree)))
                          ;(print (list "info" (car subtree) subtree))
                          (loop for i in (second subtree)
                                do (setf current-graces (cons  1 current-graces))) ;;regler l'affaire 0.0 = aftergrace
                          )
                         ((numberp subtree)
                          (let ((object 
                                 (cond 
                                  ((equal subtree 0) ;; BEFOR GRACE
                                   (setf current-graces (cons  1 current-graces))
                                   ;(print (list "currentnot" current-note  current-graces subtree))
                                   NIL)
                                  
                                  ((equal subtree 0.0) ;; AFTER GRACE 
                                   (if current-note 
                                       (progn 
                                         ;(print (list "currentnot" current-note  subtree))
                                       (setf (mus-const current-note) (append (list! (mus-const current-note)) '(1)))
                                       )
                                     ;(setf current-graces (cons  -1 current-graces))
                                     )
                                   NIL)
                                   
                                  ((> subtree 0)
                                   (let ((note (make-instance 'note :empty t :extent (* (fullratio subtree) (/ Extent nbsubunits)))))
                                     (when current-graces ;; add grace notes before ;;; avoir !
                                       ;(print "before")
                                       (setf (mus-const note) current-graces)
                                       (setf current-graces nil)
                                       )
                                     (setf current-note note)
                                     
                                     note))
                                  ((< subtree 0)
                                   ;;;ici une fois les gn sont representees
                                   ;;;mettre before grace rest
                                   ;;;comme ci-dessus
                                   (make-instance 'rest :extent (*  (abs (fullratio subtree)) (/ Extent nbsubunits)))))))
                            (when (and (plusp subtree) (floatp subtree))
                              (setf (tie object) 'continue))
                            ;(print (list "object" object))
                            object))

                    ((listp subtree)
                          (make-instance (next-metric-class self)
                                         :tree subtree 
                                         :PropagateExtent (/ Extent nbsubunits)
                                         :InternalCall t))
                         ))
               when curr-obj
                 collect curr-obj
                   into inside
                 
               finally 
                 (setf (slot-value self 'inside) inside
                       (slot-value self 'extent) Extent)
               ))))

;;maintenant regler l'affaire des loffsets

(defmethod distribute-chords ((self score-element) (chords list))
  (let ((fringe nil) 
        (chord-model (mki 'chord))
        (def-chord (or (last-elem chords) (mki 'chord))))
    (labels ((distribute (self chords)
               (setf (inside self)
                       (loop for sub in (inside self)
                             with chord
                             ;with chord-model  = (mki 'chord)
                             ;if (null chords) collect sub
                             if (not (infra-group-p sub)) 
                             do 
                                 (progn 
                                   ;(print (list "ens sub" sub))
                                 (setf chords (distribute sub chords)) 
                                 )
                               and collect sub
                             else if (rest-p sub) collect sub
                             else do
                             (if (and (note-p sub) (eq (tie sub) 'continue))
                               (progn (setf chord 
                                            (objfromobjs 
                                             (or (loop for c in fringe if (chord-p c) return c)
                                                 (mki 'chord))
                                             chord-model))
                                      (change-class chord 'continuation-chord))
                               (if (mus-const sub)  ;;; THERE ARE GRACE NOTES !!
                                   (progn
                                     ;(print (list "voir elt sub" sub))
                                   (let* ((grace-time *gdur*)  ;;; 50ms ?
                                          (nnotes-before (count 1 (mus-const sub) :test '=)); ici c'etait -1 et c'est bien cela qui fout la merde....
                                          (nnotes-after (count -1 (mus-const sub) :test '=))
                                          (chords-before (loop for i from 1 to nnotes-before collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model)))
                                          (main-chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model))
                                          (chords-after (loop for i from 1 to nnotes-after collect (objfromobjs  (or (pop chords) (clone def-chord)) chord-model))))
                                     
                                     #|
                                     (loop for c in (reverse chords-before)
                                           for dt = grace-time then (+ dt grace-time)
                                           do (setf (loffset c) (om- (loffset c) dt)))
                                     
                                     (loop for c in (reverse chords-before)
                                           do (setf (ldur c) (list *gdur*) ))
                                     
                                     
                                     (loop for c in chords-after
                                           for dt = grace-time then (+ dt grace-time)
                                           do (setf (loffset c) (om+ (loffset c) dt)))
                                     
                                     (loop for c in (reverse chords-after)
                                           do (setf (ldur c) (list *gdur*) ))
                                      |#   
                                     ;(print (list "mainchord" (append (list main-chord)))) 
                                     (setf chord (objfromobjs (append (list main-chord)) chord-model))
                                     ;bien mais ca il faut faire qq chose!!!
                                     ; TODO
                                                                       
                                     (let ((gchords (loop for i in chords-before
                                                          collect (objfromobjs i (make-instance 'grace-chord)))))
                                       (loop for i in gchords
                                             do (setf (thechord i) chord))
                                       
                                       (setf (gnotes chord) 
                                             (make-instance 'grace-notes
                                                            :glist gchords
                                                            :thechord chord
                                                            :before? t)))
                                     )
                                   )
                                 (setf chord (objfromobjs (or (pop chords) (clone def-chord)) chord-model))
                                
                                 ))
                             (setf (offset chord) (offset sub))
                             (InContext sub (setf (extent chord) (extent sub)))
                             (when (and (note-p sub) (eq (tie sub) 'continue))  (push 'tie fringe))
                             (push chord fringe)
                             and collect chord))
                 chords))
      
      (distribute self chords)
      
      (setf fringe (nreverse fringe)) 
      (loop for item1 in fringe
            for item2 = (rest fringe) then (rest item2)
            with state = 0
            do
            (cond ((eq (first item2) 'tie)
                   (cond 
                    ((= state 0) (tie-chord item1 'begin) (incf state))
                    (t (tie-chord item1 'continue) (setf (state item1) 'continue))))
                  ((not (eq item1 'tie))
                   (cond ((> state 0) (tie-chord item1 'end) (setf state 0 (state item1) 'end))))))
      self)))



;this must be added for our new class
;;apparement non utilisee pour le moment!
(defmethod make-graph-ryth-obj ((self group-gn) top staffsys linespace  scale sel pere durtot &optional ryth) 
   (let* (new-group direstart)
     (setf new-group (make-instance 'grap-group
                       :reference self
                       :parent pere))
     (setf (inside new-group) (flat ;;; FLAT or not flat ?
                               (loop for item in (inside self) 
                                    for i = 0 then (+ i 1)
                                    collect
                                    (let ((newchord (make-graph-ryth-obj 
                                                     item top staffsys linespace  scale sel new-group 
                                                     
                                                     (let* ;((dur-obj (/ (/ (extent item) (qvalue item)) 
                                                           ;                 (/ (extent self) (qvalue self)))))
                                                       ((dur-obj 100))
                                                           ;(* dur-obj durtot)
                                                       (print (list "durtot GN" durtot (/ (car (second ryth)) (first ryth))))
                                                           (* 100 1/4)
                                                           )
                                                     
                                                     (list (/ (car (second ryth)) (first ryth))
                                                           (nth i 
                                                                ;;; dirty fix when 0 (grace notes) are misplaced in the tree..
                                                                (remove 0
                                                                        (cadr (second ryth)))))

                                                     )))
                                      newchord))
                               ))
     
     (when (figure-? new-group)
       (setf direstart (calcule-dir-et-start new-group (midicenter staffsys)))
       (set-dir-and-high new-group (car direstart) linespace))
     (make-graphic-extras new-group)
     new-group))



(defmethod make-graph-ryth-obj ((self group)  top staffsys linespace  scale sel pere durtot &optional ryth)
   (let* ((group-ratio (get-group-ratio self))
          (num (or group-ratio (extent self)))
          (denom (find-denom num durtot))
          (num (if (listp denom) (car denom) num))
          (denom (if (listp denom) (second denom) denom))
          (unite (/ durtot denom))
          (sympli (/ num denom))
          new-group direstart)
     (setf new-group (make-instance 'grap-group
                       :reference self
                       :parent pere))
     
     (setf (numdenom new-group) (cond
                                 ((not group-ratio) nil)
                                 ((= sympli 1) nil)
                                 (t  (reduce-num-den (list num denom)))))   ;;; kh 10/2016 add reduce-num-den
     
     (setf (inside new-group) (flat ;;; FLAT or not flat ?
                               (loop for item in (inside self) 
                                    for i = 0 then (+ i 1)
                                    collect
                                    (let ((newchord (make-graph-ryth-obj 
                                                     item top staffsys linespace  scale sel new-group 
                                                     (if (not group-ratio) 
                                                                           
                                                         (let* ((dur-obj (/ (/ (extent item) (qvalue item)) 
                                                                            (/ (extent self) (qvalue self)))))
                                                           (* dur-obj durtot))
                                                                           
                                                       (let* ((operation (/ (/ (extent item) (qvalue item)) 
                                                                            (/ (extent self) (qvalue self))))
                                                              (dur-obj (numerator operation)))
                                                         (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                                         (* dur-obj unite))
                                                       )
                                                      (unless (atom (second ryth))                   
                                                     (list (/ (car (second ryth)) (first ryth))
                                                           (nth i 
                                                                ;;; dirty fix when 0 (grace notes) are misplaced in the tree..
                                                                (remove 0
                                                                        (cadr (second ryth)))))
                                                     )
                                                     )))
                                      ;(print (list "whadda" item))
                                      newchord))
                               )
           )
     
     (when (numdenom new-group)
       (setf (chiflevel new-group) (calcule-chiff-level new-group)))
     (when (figure-? new-group)
       (setf direstart (calcule-dir-et-start new-group (midicenter staffsys)))
       (set-dir-and-high new-group (car direstart) linespace)
       ;;; grace notes
       ;(print (list "group[" new-group (om-inspect (second (get-chord&rest-not-graces new-group)))))
       (loop for item in (get-chord&rest-not-graces new-group) do
             (when  (grap-grace-notes item)
               (set-graces-dir-after (grap-grace-notes item) item staffsys linespace)))
       
       )
     (make-graphic-extras new-group)
     new-group))




;;;;MISSING FUNCTIONS
;;;a voir!



(defmethod space-size&offset ((self grap-rest) size) 
 (let ((gspace 0))
   (when (grap-grace-notes self) 
     (setf gspace 0 )
    ; (grace-notes-space (grap-grace-notes self) size)
   )
   (list 0 0 gspace (third (rectangle self)))))



;Maybe ???
(defmethod space-size&offset ((self grap-ryth-chord) size) ;size = size of font
  (let ((gspace 0)
        minalteration maxhead ) 
    (setf minalteration 
          (loop for item in (inside self) maximize 
                (if (alteration item) (abs (* (- (alteration item) 1) (/ size 3))) 0)))
    ;(print (list "deltaghead"   (loop for item in (inside self) collect (delta-head item))))
    (setf maxhead 
          (loop for item in (inside self) maximize 
                (if (delta-head item) (* (/ size 4) (delta-head item)) 0)))
    (when (points (car (inside self)))
      (setf maxhead (+ maxhead (*  (+ 1 (points (car (inside self))))  (/ size 2)))))
    (when (grap-grace-notes self) ; (print (list "grap" self (grap-grace-notes self)))
      (setf gspace 0)
      ;(grace-notes-space (grap-grace-notes self) size)
      )
    (list 0 0 (+ gspace minalteration) maxhead)))




;;;;

(defmethod do-space-object ((self grap-ryth-chord)  count  maxpixelclef maxpixelmeasure maxpixelgraces maxpixelsize size)
  
 (setf (main-point self) (list (+ count  maxpixelclef maxpixelmeasure maxpixelgraces) (second (main-point self))))
  (loop for item in (inside self) do
        ;(setf (main-point item) (list (+ count (first (main-point item)) maxpixelclef maxpixelmeasure maxpixelgraces) (second (main-point item))))
        (setf (main-point item) (list (+ count  maxpixelclef maxpixelmeasure maxpixelgraces (car (main-point item))) (second (main-point item)))))
  (when (grap-grace-notes self) 
            ;(setf (main-point i) (list (+ n 72)  (second (main-point self)))))
      ;(setf (main-point (grap-grace-notes self)) (list (- (car (main-point self)) 12) nil))
      ;A FAIRE
;(set-main-point (grap-grace-notes self) count)
    )
  )


(defmethod do-space-object ((self grap-rest)  count maxpixelclef maxpixelmeasure maxpixelgraces maxpixelsize size)
  (setf (main-point self) (list (+ count  maxpixelclef maxpixelmeasure maxpixelgraces) (second (main-point self))))
  ;(when (grap-grace-notes self) 
  ;  (set-main-point (grap-grace-notes self) count))
  )
