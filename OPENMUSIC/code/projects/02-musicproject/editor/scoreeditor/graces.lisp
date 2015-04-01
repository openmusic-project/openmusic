(in-package :om)

;============ GRACES NOTES

;-------play
(defvar *gdur* 20)
(setf *gdur* 60)
                    
(defmethod PrepareToPlay ((player t) (self chord) at &key approx port interval voice)
  (append 
   (when (gnotes self)
     (let ((chseq (make-instance 'chord-seq 
                                 :lmidic (glist (gnotes self))
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
                                 :lmidic (glist (gnotes self))
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
  ((glist :initform '((6000)) :accessor glist :initarg :glist)
   (mus-color :initform *om-black-color* :accessor mus-color)
   (thechord :initform nil :accessor thechord  :initarg :thechord)
   (before? :initform t :accessor before? :initarg :before?)))


(defmethod offset->ms ((self grace-notes) &optional grandparent)
  (let ((thechord (thechord self)))
     (- (offset->ms thechord) 1)  ))

(defmethod! set-grace-notes ((self simple-container) chords before?)
  (setf (gnotes self) (make-instance 'grace-notes
                        :glist chords
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



;=======GRAPHIC CLASS

(defclas grap-grace-notes ()
  ((grc :initform nil)) )  ;a enlever de scoretools

(defmethod graces? ((self grap-grace-notes)) t)
(defmethod graces? ((self t)) nil)

(defclass s-grap-grace-notes (grap-grace-notes grap-ryth-chord) ())

(defclas g-grap-grace-notes (grap-grace-notes grap-group) ())


;**************************************************************
; GRAPHIC
;**************************************************************

(defmethod add-grace-notes-dialog ((self simple-container))
  (set-grace-notes self '((7200) (7400) (7200)) t))


(defmethod delete-grace-notes ((self simple-container))
  (setf (gnotes self) nil))

;=================================

; ----------------a borrar
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
(setf *grace-factor* 10/6)

(defmethod make-graces-from-list ((self grace-notes) top staffsys linespace scale sel pere grc)
  (let ((list (glist self)))
    (when list
      (if (= (length list) 1)
        (make-simple-grace self top staffsys linespace scale sel pere grc)
        (make-group-grace self top staffsys linespace scale sel pere grc)))))

;--------simple
(defmethod make-simple-grace ((self grace-notes) top staffsys linespace scale sel pere grc)
  (let* ((list (glist self))
         (chord (make-instance 'chord :lmidic (car list)))
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
    (setf (beams-num new-grace) beams-num)
    (setf (stemhigh new-grace) (round (* 2.5 linespace) *grace-factor*))
    (setf (rectangle new-grace)  (list 0 0 maxw 0))
    new-grace))

(defun not-stem-dir (dir)
  (if (string-equal dir "up") "dw" "up"))

;-------group


(defmethod make-group-grace ((self grace-notes) top staffsys linespace scale sel pere grc)
  (let* ((list (glist self))
         (group (make-instance 'group :tree (list (/ (length list) 2) (make-list (length list) :initial-element 1))))
         new-group direstart)
    (setf new-group (make-instance 'g-grap-grace-notes
                      :grc grc
                      :reference group
                      :parent self))
    
    (setf (numdenom new-group)  nil)
    (loop for item in (inside group)
          for chord in list do
          (setf (lmidic item) chord))
    (setf (inside new-group) (loop for item in (inside group) 
                                   for i = 0 then (+ i 1)
                                   collect
                                   (make-graph-ryth-obj  item  top staffsys linespace  scale sel new-group 1/8 )))  
   new-group))




;==========================================
;----------DRAW
;==========================================

(defmethod draw-object-ryth ((self grap-grace-notes) view x y zoom minx maxx miny maxy slot size linear?  staff chnote)
  (om-with-fg-color nil (mus-color (reference self))
    (draw-grace-notes self x y zoom minx maxx miny maxy slot size linear?  staff chnote)))

;----simple
(defmethod draw-grace-notes ((self s-grap-grace-notes) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
  (om-with-fg-color nil (mus-color (reference self))
    (let* ((dir (not-stem-dir (stemdir  (grc self))))
           (thenotes (copy-list (inside self))))
      (loop for item in thenotes do
            (draw-head-grace item x  y zoom minx maxx miny maxy slot size linear? staff chnote))
      (collect-rectangles self)
      (om-with-font (om-make-music-font *heads-font* (round size *grace-factor*))
                    (draw-chord-grace-stem self x y zoom (beams-num self) dir (round size *grace-factor*))))))

(defmethod draw-head-grace ((self t) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
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
         (note-color (get-mus-color note))
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
  (loop for chord in (inside self) do
        (loop for item in (inside chord) do
              (draw-head-grace item x y zoom minx maxx miny maxy slot size linear? staff chnote))
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
