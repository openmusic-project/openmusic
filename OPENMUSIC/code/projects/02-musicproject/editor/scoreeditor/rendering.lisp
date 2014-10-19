;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;setf
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)

;============================================
; (1) GLOBALS
;============================================
(defvar *note-color* *om-black-color*)
(defvar *system-color* *om-black-color*)

;============================================
; (2) KEYS
;============================================

(defclass OMkey ()
   ((key :initform (key-g ) :initarg :key :accessor key)
    (octave :initform nil :initarg :octave :accessor octave)
    (keyline :initform 2 :initarg :keyline :accessor keyline)))

(defmethod draw-key ((self Omkey) x y ls)
   (om-draw-char x (round y) (key self))
   (when (octave self)
     (om-draw-string x (round (+ y (* ls (second (octave self)))))
                  (chif2sstr (car (octave self))))))

(defvar *g-key* (make-instance 'OMkey))
(defvar *f-key* (make-instance 'OMkey :key #\? :keyline 4))
(defvar *g2-key* (make-instance 'OMkey :octave '(15 -4.5)))
(defvar *f2-key* (make-instance 'OMkey :octave '(15 4) :key #\? :keyline 4))


;============================================
; (3) STAFF
; Staff est une classe uniquement pour la musique
; d'autre types de TRACKS doit vent etre implementes
; comme OMSYSTEMS (par example voix faites de sons
;============================================
(defclass OMStaff ()
   ((key-obj :initform nil :initarg :key-obj :accessor key-obj)
    (posy :initform nil :initarg :posy :accessor posy)
    (range :initform nil :initarg :range :accessor range)
    (numlines :initform 5 :initarg :numlines :accessor numlines)))

;; NUMLINES ?? est ce que ca peux servir ?? ou il faudrait faire une autre classe 
;; le problem est de savoir si le DENT horizontale fait encore un ton !!!!

;-----------------------------------------------------------------
;draw a staff and return de miny et maxy position in pixels.
;-----------------------------------------------------------------
(defmethod draw-staff ((self OMStaff) x top xsize fontsize deltay &optional stempo name)
  (let* ((linespace (round fontsize (- (numlines self) 1)))
         (mytop (line2pixel (posy self) top linespace)))
    (draw-tempo 4 stempo (+ x 15)  (- (+  deltay mytop) (round fontsize 2))  fontsize name)
    (loop for i from 0 to (- (numlines self) 1) do
          (om-draw-line x (+  deltay (* i linespace) mytop)
                        (+ x xsize) (+  deltay (* i linespace) mytop)))
    (draw-key (key-obj self) (+ x 15) 
              (+ deltay (* (- (numlines self) (keyline (key-obj self))) linespace) mytop) linespace)
    (list (+  deltay  mytop)
          (+  deltay (* (- (numlines self) 1) linespace) mytop))))

(defmethod get-staff-starts ((self OMStaff) top fontsize deltay)
   (let* ((linespace (round fontsize (- (numlines self) 1)))
          (mytop (line2pixel (posy self) top linespace)))
     (+  deltay  mytop)))

(defun draw-tempo (fig num x y size name)
  (let ((textsize (round size 2)))
    (when num
      (om-draw-string x y (get-stringfrom-num fig))
      (om-with-font (om-make-font *om-def-font-face*  textsize) 
                    (om-draw-string (+ x (round size 2)) y (format nil "=~D" num))))
    (when name
      (om-with-font (om-make-font *om-def-font-face*  textsize) 
                    (om-draw-string (+ x (round size 2)) (if num (- y (round size 1.8)) y) name)))
    ))


;TRES MAL le changement de staff car il se deplace verticalement IL faut changer ca


;============================================
; (4) SYSTEM
;============================================

(defclass OMSystem ()
  ((staff-list :initform nil :initarg :staff-list :accessor staff-list)
   (deltax :initform 2 :initarg :deltax :accessor deltax)
   (sysname :initform nil :initarg :sysname :accessor sysname)
   (top-in-midi :initform 120  :initarg :top :accessor top-in-midi)
   (armure :initform nil :initarg :armure :accessor armure)))


;-----------------------------------------------------------------
;Fait une copy du SYstem
;-----------------------------------------------------------------
(defmethod omng-copy ((self OMSystem))
   `(get-staff-system ',(sysname self)))

(defmethod get-system-starts ((self OMSystem) fontsize deltay)
   (get-staff-starts (car (staff-list self)) (top-in-midi self) fontsize deltay))

(defmethod get-deltax ((self OMSystem))
   (+ (deltax self) (if (armure self) 2 0)))

;-----------------------------------------------------------------
;draw a system and return de miny et maxy position in pixels.
;-----------------------------------------------------------------
;ATTENTION ca doit etre un parametre

(defmethod draw-system ((self OMSystem) x  xsize fontsize deltay selected &optional showtempo name)
  (om-with-fg-color nil *system-color*
    (let (y0 y1)
      (loop for staff in (staff-list self) do
            (let ((y (draw-staff staff x  (top-in-midi self) xsize fontsize deltay (unless y0 showtempo) (unless y0 name))))
              (when *om-tonalite* 
                (draw-armure staff (armure self) x (top-in-midi self) xsize fontsize deltay))
              (unless y0 (setf y0 (first y)))
              (setf y1 (second y))))
      (list y0 y1))))

;-----------------------------------------------------------------
;La taille du system in DENTS
;-----------------------------------------------------------------
(defmethod system-size-in-lines ((self OMSystem))
  (let ((lis (staff-list self)))
    (+ (- (posy (car (last lis))) (posy (car lis))) 4)))

(defmethod system-size-in-pix ((self OMSystem) size)
  (* (round  size 4) (system-size-in-lines self)))

(defmethod system-offset-in-pix ((self OMSystem) size)
  (* (round  size 4) (posy (car (staff-list self)))))

(defmethod top-in-midi ((self list))
  (top-in-midi (car self) ))

(defmethod system-offset-in-pix ((self list) size)
  (system-offset-in-pix (car self) size))

(defmethod system-size-in-pix ((self list) size)
  (let ((rep 0))
    (loop for item in self do
          (setf rep (+ rep (system-size-in-pix item size))))
    rep))


;change de y position for a system
(defmethod move-system-in-y ((self OMSystem) &optional (up nil))
   (setf (top-in-midi self) (+ (top-in-midi self) (if up -12 12)))
   (loop for item in (staff-list self) do
         (setf (posy item) (+ (posy item) (if up -3.5 3.5)))))

(defmethod midicenter ((self OMSystem))
  (let ((max (car (range (first (staff-list self)))))
        (min (second (range (car (last (staff-list self)))))))
    (+ min (round (- max min) 2))))
   
(defmethod get-staff-system ((symbol symbol))
   (make-instance 'OMSystem
     :sysname symbol
     :staff-list
     (cond
      ((equal symbol 'F) (list (make-instance 'OMstaff :key-obj  *f-key* :posy 17.5 :range '(43 57))))
      ((equal symbol 'G) (list (make-instance 'OMstaff :key-obj  *g-key* :posy 11.5 :range'(64 77))))
      ((equal symbol 'G2) (list (make-instance 'OMstaff :key-obj  *g2-key*  :posy 4.5 :range'(88 101))))
      ((equal symbol 'F2) (list (make-instance 'OMstaff  :key-obj  *f2-key* :posy 24.5 :range '(19 33))))
      ((equal symbol 'GF) (list (make-instance 'OMstaff :key-obj  *g-key* :posy 11.5 :range'(64 77))
                                (make-instance 'OMstaff  :key-obj  *f-key* :posy 17.5 :range '(43 57))))
      ((equal symbol 'GG) (list (make-instance 'OMstaff :key-obj  *g-key* :posy 4.5 :range'(88 101))
                                (make-instance 'OMstaff :key-obj  *g-key* :posy 11.5 :range'(64 77))))
      ((equal symbol 'FF) (list (make-instance 'OMstaff :key-obj  *f-key* :posy 17.5 :range '(43 57))
                                (make-instance 'OMstaff :key-obj  *f-key* :posy 24.5 :range '(19 33))))
      ((equal symbol 'GFF) (list (make-instance 'OMstaff :key-obj  *g-key* :posy 11.5 :range'(64 77))
                                 (make-instance 'OMstaff :key-obj  *f-key* :posy 17.5 :range '(43 57))
                                 (make-instance 'OMstaff :key-obj  *f-key* :posy 24.5 :range '(19 33))))
      ((equal symbol 'GGF) (list (make-instance 'OMstaff :key-obj  *g-key* :posy 4.5 :range'(88 101))
                                 (make-instance 'OMstaff :key-obj  *g-key* :posy 11.5 :range'(64 77))
                                 (make-instance 'OMstaff :key-obj  *f-key* :posy 17.5 :range '(43 57))))
      ((equal symbol 'GGFF) (list (make-instance 'OMstaff :key-obj  *g-key*  :posy 4.5 :range'(88 101))
                                  (make-instance 'OMstaff  :key-obj  *g-key*  :posy 11.5 :range'(64 77))
                                  (make-instance 'OMstaff :key-obj  *f-key*  :posy 17.5 :range '(43 57))
                                  (make-instance 'OMstaff  :key-obj  *f-key* :posy 24.5 :range '(19 33))))
      ((equal symbol 'empty) (list (make-instance 'OMstaff  :key-obj  *g-key*  :posy 11.5 :range'(64 77) :numlines 0)
                                   )))))

(defmethod get-staff-system ((self list))
  (loop for staff in self collect (get-staff-system staff)))

;OJO Il faut faire un system GENERAL un rectangle (i.e. maquettes, sounds, bpfs, etc)

;======================CONVERTIONS=============================
(defvar *onesec* 3 "one sec = 3 times de font size")

;ELIMINER TOP si il ne sert a rien
(defun line2pixel (line top linespace) 
   (round (* line linespace)))

(defun ms2pixel (ms linespace zoom)
   (let ((onesec (* linespace 4 *onesec*)))
     (round (* ms onesec zoom) 1000)))

(defun pixel2ms (pix linespace zoom)
   (let ((onesec (* linespace 4 *onesec*)))
     (round (* (/ pix onesec)  1000) zoom)))

(defun midi2pixel (midi top linespace scale)
  (let* ((app (approx-factor scale))
         (topmc (* top 100))
         (tone (approx2tone app))
         (midiapp (approx-m midi tone))
         (numtones (* 6 tone)))
    (round (* (+ (* 3.5 (floor (- topmc midiapp) 1200)) 
                 (lines-from-do (/ (mod (- (* top 100) midiapp) 1200) app) scale numtones) -0.5 ) linespace))))

;Une petite doc s'impose
(defun lines-from-do (x scale tone)
   (setf tone (round tone))
   (if 
     (and (zerop x) 
          (= 0 (car (lines-list scale))))   ;;; !!!! pour les echelles qui commencent pas par 0 ex : *7#-scale*
     0  
     (- 3.5 (/ (nth (mod (round (- tone x)) tone) (lines-list scale)) 2))
     ))

;MMMM    a documenter aussi
(defun delta-to-name (ssize y1 startmidic) 
   (setf startmidic (approx-m startmidic 2))
   (let* ((start-octave (floor (/ startmidic 1200 ) ) )
          (start-list-pos (position (round (mod startmidic 1200) 100)  '( 0 1 2 3 4 4 5 6 7 8 9 10 11 11 )))
          (demi-tons-graphiques (round (/ (* y1 16) ssize)))
          (demi-tons-graphiques-dans-octave (mod demi-tons-graphiques 14 )) 
          (delta-octaves (floor (/ demi-tons-graphiques 14 ))) 
          (delta-pos (mod (+ start-list-pos demi-tons-graphiques-dans-octave) 14 ))
          (extra-octave (floor (/ (+ start-list-pos demi-tons-graphiques-dans-octave) 14))))
     (+  (* (nth delta-pos  '( 0 1 2 3 4 4 5 6 7 8 9 10 11 11)) 100 ) 
         (* (+ delta-octaves start-octave extra-octave) 1200 ))))

;======================Placing notes in a chord=============================
;ca commence mal

(defvar *alt-tolerance* 6)

(defmethod note-diatone ((self note) list)
  (+ (* 7 (+ (truncate (/ (midic self) 1200)) (third list))) (first list)))

(defmethod note-alteration ((self note) list) (second list))

(defmethod scale-list ((self note) scale)
   (give-alteration scale (approx-m (midic self) (approx2tone (approx-factor scale)))))

(defmethod make-alt-groups ((self chord) scale)
  (let ((notes (inside self))
        (group-length 1)
        alt-now big-alt-temp)
    (while notes 
           (while (and notes (not (note-alteration (car notes) (scale-list (car notes) scale)))) (pop notes))
           (if notes (setq alt-now (note-diatone (car notes) (scale-list (pop notes) scale))))
           (while (and notes (not (note-alteration (car notes) (scale-list (car notes) scale)))) (pop notes))
           (while (and notes 
                       (< (-  (note-diatone (car notes) (scale-list (car notes) scale)) alt-now) *alt-tolerance*))
                  (incf group-length)
                  (pop notes)
                  (while (and notes (not (note-alteration (car notes) (scale-list (car notes) scale)))) (pop notes)))
           (push group-length big-alt-temp)
           (setq group-length 1))
    (nreverse big-alt-temp)))

(defmethod make-alt-zig-zag ((self chord) scale)
  (let ((alt-groups (make-alt-groups self scale))
        (x-shift 1)
        x-values left right alt-group-temp 
        x-now  left? )
    (while (setq alt-group-temp (pop alt-groups))
           (setq left (* x-shift (truncate (/ (1+  alt-group-temp) 2)))) 
           (setq right (- left x-shift))
           (setq x-now 0)
           (setq left? ())
           (push  x-now x-values)
           (repeat (1- alt-group-temp)
             (if left?
               (push (incf x-now right) x-values)
               (push (decf x-now left) x-values))
             (setq left? (not left?))))
    x-values))

(defmethod make-diatone-groups ((self chord) scale)
  (let ((notes (sort (copy-list (inside self)) '< :key 'midic))
        (group-length 1) (dia-tolerance 2)
        dia-now big-dia-temp)
    (while notes
           (setq dia-now (note-diatone (car notes) (scale-list (pop notes) scale)))
           (while (and notes 
                       (< (abs (- dia-now (note-diatone (car notes) (scale-list (car notes) scale)))) dia-tolerance))
                  (incf group-length)
                  (pop notes))
           (push group-length big-dia-temp)
           (setq group-length 1))
    (nreverse big-dia-temp)))

(defmethod make-chord-zig-zag ((self chord) scale)
  (let ((dia-groups (make-diatone-groups self scale))
        (x-shift 1.15)
        x-values left right dia-group-temp
         x-now left?)
    (while (setq dia-group-temp (pop dia-groups))
      (setq left (* x-shift (truncate (/ (1+  dia-group-temp) 2)))) 
      (setq right (- left x-shift))
      (setq x-now 0)
      (setq left? ())
      (push  x-now x-values)
      (repeat (1-  dia-group-temp)
        (if left?
          (push (decf x-now right) x-values)
          (push (incf x-now left) x-values))
        (setq left? (not left?))))
     (nreverse x-values)))



;====================================================================================
;============================GRAPHIC-CONTAINERS=====================
;====================================================================================


;*********SIMPLE*********
(defclas simple-graph-container ()
  ((reference :initform nil)
   ;Le vrai objct (note, chord, etc)
   (parent :initform nil)
   (rectangle :initform (list 1000000000 100000000 1000000000 100000000))
   ; (x0 y0 x1 y1)
   (selected :initform nil)
   (main-point :initform nil)
   ; JE NE SAIT PAS
   (extras :initform nil)))

(defmethod draw-rectangle ((self simple-graph-container) &optional fill )
   (draw-h-rectangle (rectangle self) fill))

;debe ser mas complicado si esta eb modo linear o no mais cette fun n'est pas utilisee
; donc a effacer
(defmethod get-rendered-rectangle ((self simple-graph-container) x y zoom)
   (let ((rec (rectangle self)))
     (list (+ x (* zoom (car rec))) (+ y (second rec))
           (+ x (* zoom (third rec))) (+ y (fourth rec)))))

;c'est quoi le main-point ?
(defmethod x   ((self simple-graph-container)) (first (main-point self)))
(defmethod y   ((self simple-graph-container)) (second (main-point self)))

(defmethod make-graphic-extras ((self simple-graph-container))
   (setf (extras self)
         (remove nil (loop for item in (extra-obj-list (reference self)) collect
                           (progn
                             (setf (object item) (reference self))
                             (make-graph-extra-obj item self))))))

(defmethod draw-extras ((self simple-graph-container) size  staff)
   (loop for item in (extras self) collect
         (draw-graph-extra-obj item size staff)))


;*********CONTAINER*********
(defclas grap-container (simple-graph-container)
   ((inside :initform nil)))

(defmethod draw-object ((self grap-container) x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
   (loop for item in (inside self) do
         (draw-object item x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote))
   (draw-extras self size staff))


(defmethod set-parent ((self grap-container) list)
   (loop for item in list do
         (setf (parent item) self)))


(defmethod draw-score-selection ((self grap-container) selection)
   (loop for item in (extras self) do
           (draw-score-selection item selection))
   (if (member (reference self) selection :test 'equal)
     (draw-rectangle self t)
     (loop for item in (inside self) do
           (draw-score-selection item selection))))

(defmethod draw-score-selection ((self simple-graph-container) selection)
   (if (member (reference self) selection :test 'equal)
     (draw-rectangle self t)
     (loop for item in (extras self) do
           (draw-score-selection item selection))))

(defmethod draw-score-selection ((self t) selection) t)


;======================================================
;SUBCLASSES DE GRAP-CONTAINERS
;======================================================

;*********NOTE**********
(defclas grap-note (simple-graph-container) 
  ((alteration :initform nil)  
   ; Le nombre de symbols qui altere la note (0 pas d'alteration, 1 un diese par example, etc.)
   (alt-char :initform nil)
   (auxlines :initform nil)
   (headchar :initform (head-1/4))
   (delta-head :initform 0)))

;----Main

(defmethod points ((self grap-note) ) 0)
(defmethod ties ((self grap-note) ) nil)


(defmethod get-next-tied-noted ((self grap-note) )
   (and (parent self)
        (let ((next-chord (next-figure (parent self))))
          (and next-chord (grap-ryth-chord-p next-chord) 
               (find (midic (reference self)) (inside next-chord) :key #'(lambda (x) (midic (reference x))))))))

(defmethod get-last-tied-noted ((self grap-note) )
   (and (parent self)
        (let ((previous (previous-figure (parent self))))
          (and previous (grap-ryth-chord-p previous)
               (find (midic (reference self)) (inside previous) :key #'(lambda (x) (midic (reference x))))))))

(defmethod draw-rectangle ((self grap-note) &optional fill )
  (when (rectangle self)
    (let* ((rec (rectangle self))
           (rec (list (- (car rec) 2) (- (second rec) 2) (+ (third rec) 2 ) (+ (fourth rec) 2))))
      (if fill
        (om-draw-hilite-rect (car rec) (second rec) (- (third rec) (car rec) ) (- (fourth rec) (second rec)))
        (om-with-fg-color nil *om-select-color*
          (om-draw-rect (car rec) (second rec) (- (third rec) (car rec) ) (- (fourth rec) (second rec))))))))
 

;;;============================================================================================
;;; methodes redefinies dans HarmonicProject pour quand on donne manuellement des enharmonies
;;;============================================================================================

(defmethod get-special-alterat ((self note) armure) nil)

(defmethod get-alt-char ((self note) scale armure) 
  (or (and *om-tonalite*
           (get-special-alterat self armure))
      (note-alteration self (scale-list self scale))))

(defun get-alteration-n (alt-char)
  (cond ((characterp alt-char) -1)
        ((stringp alt-char) (- (length alt-char)))
        (t nil)))

;;; rajoute un ecart quand on a plusieurs alterations
(defmethod correct-alteration ((self grap-note) alteration)
  alteration)

;;; dit si c'est une alteration d'echelle
(defmethod natural-alt-char ((self grap-note))
  (alt-char self))

(defmethod get-graphic-pos ((self note) top linespace scale)
  (midi2pixel (midic self) top linespace scale))

;;;============================================================================================

(defun midi2pixel (midi top linespace scale)
  (let* ((app (approx-factor scale))
         (topmc (* top 100))
         (tone (approx2tone app))
         (midiapp (approx-m midi tone))
         (numtones (* 6 tone)))
    (round (* (+ (* 3.5 (floor (- topmc midiapp) 1200)) 
                 (lines-from-do (/ (mod (- (* top 100) midiapp) 1200) app) scale numtones) -0.5 ) linespace))))



;***************
;Make a GRAP-NOTE
;***************
(defmethod make-graph-form-obj ((self note) x top linespace mode scale sel system stem)
  (declare (ignore mode))
  (let* ((ypos (get-graphic-pos self top linespace scale))
         (alt-char (get-alt-char self scale (armure system)))
         alteration rep)
    (when alt-char (setf alteration (get-alteration-n alt-char)))
    (setf rep (make-instance 'grap-note
                :reference self
                :alt-char alt-char
                :alteration alteration
                :rectangle (list x (- ypos (round linespace 2)) (round (+ x linespace)) (+ ypos (round linespace 2)))
                :main-point (list x (round (- ypos (/ linespace 2))))  ;ATTENTION  c'est ICI qu'on donne la position en Y
                :selected (member self sel :test 'equal)
                :auxlines (get-aux-lines self system top scale linespace ypos)))
    (make-graphic-extras rep)
    rep))

;***************
;Draw a GRAP-NOTE
;***************

;peut etre qu'il me faudra la view....
(defmethod draw-object ((self grap-note) x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
  (declare (ignore minx maxx miny maxy linear? grille-p))
  (let* ((note (reference self))
         (note-color (get-mus-color note))
         (realrealpos (+ 1 x (* (/ size 4) (delta-head self)) (* zoom (- (x self) (* (/ size 4) (delta-head self))))))
         (realpos (round realrealpos))
         (altpos (if (alteration self) 
                   (round (- (+ realrealpos (* (- (alteration self) 1) (/ size 4))) (* (/ size 4) (delta-head self))))
                   realpos))
         (extra-head (get-extra-head note))
         (str (or extra-head (headchar self)))
         (headsizex (get-name-size str (om-make-font *music-font* size)))
         (altstr (string (alt-char self)))  
         tie)
    (om-with-fg-color nil (if chnote (nth (chan note) *16-color-list*) note-color)
      (om-draw-string  realpos (+ y (y self)) str)
      (write-note-points self  (+ headsizex realpos) (+ y (y self)) size) 
      (when (alteration self)
        (om-draw-string altpos (+ y (y self)) altstr))
      (setf (rectangle self) (list altpos (+ y (- (y self) (round size 8)))
                                   (+ realpos (round size 3)) (+ y (round size 8) (y self))))
      (draw-note-liason note self size realpos zoom y))
    (draw-note-aux-lines self size realpos headsizex y)
    (write-note-slot self realpos (+ y (y self)) slot size zoom)
    (draw-extras self size staff)))

;-------drawing extra lines

(defmethod get-aux-lines ((self note) system top scale ls ypos)
   (let ((midi (round (midic self) 100))
         (staffs (staff-list system))
         (dir 'up) rep)
     (when staffs
       (cond
        ((> midi (second (range (first staffs)))) 
         (setf dir 'up rep (midi2pixel (* 100 (second (range (first staffs)))) top ls scale)))
        ((< midi (first (range (car (last staffs))))) 
         (setf dir 'dw rep (midi2pixel (* 100 (first (range (car (last staffs))))) top ls scale)))
        (t (let ((x0 (first (range (car staffs)))))
             (loop for item in (cdr staffs)
                   for i = 1 then (+ i 1) do
                   (if (and (< midi x0) (> midi (second (range item))))
                     (setf rep  (midi2pixel (* 100 (second (range item))) top ls scale)))
                   (setf x0 (first (range item)))))))
       (when rep (setf rep (list dir rep ypos))))
     rep))

(defmethod get-aux-lines ((self note) system top scale ls ypos)
   (get-aux-lines-midic (round (midic self) 100) system top scale ls ypos))


(defun get-aux-lines-midic (midi system top scale ls ypos)
   (let ((staffs (staff-list system))
         (dir 'up) rep)
     (when staffs
       (setf scale *2-tone-chromatic-scale*)
       (cond
        ((> midi (second (range (first staffs)))) 
         (setf dir 'up rep (midi2pixel (* 100 (second (range (first staffs)))) top ls scale)))
        ((< midi (first (range (car (last staffs))))) 
         (setf dir 'dw rep (midi2pixel (* 100 (first (range (car (last staffs))))) top ls scale)))
        (t (let ((x0 (first (range (car staffs)))))
             (loop for item in (cdr staffs)
                   for i = 1 then (+ i 1) do
                   (if (and (< midi x0) (> midi (second (range item))))
                     (setf rep  (midi2pixel (* 100 (second (range item))) top ls scale)))
                   (setf x0 (first (range item)))))))
       (when rep (setf rep (list dir rep ypos))))
     rep))

(defun draw-note-aux-lines (self size realpos headsizex y) 
  (when (auxlines self)
    (om-with-fg-color nil *system-color* 
      (let ((dir (car (auxlines self)))
            (topy (+ (- y (round size 8)) (second (auxlines self))))        
            (limy (+ (- y (round size 8)) (third (auxlines self)))))
        (if (equal dir 'dw)
          (progn
            (setf topy (+ topy (round size 4))) 
            (loop while (<= topy limy) do
                  (om-draw-line (- realpos (round size 8)) topy 
                                (+  headsizex  realpos) topy)
                  (setf topy (+ topy (round size 4)))))
          (progn
            (setf topy (- topy (round size 4))) 
            (loop while (>= topy limy) do
                  (om-draw-line (- realpos (round size 8)) topy 
                                (+  headsizex  realpos) topy)
                  (setf topy (- topy (round size 4))))))))))

;-------drawing ties
(defun draw-note-liason (note self size realpos zoom y)
  (let ((tie (tie note)))
    (when tie
      (let* ((direction (get-tie-direction self))
             (toptie (if (string-equal direction "down") (+ y (y self)) (- (+ y (y self)) (round size 3))))
             (bottie (if (string-equal direction "down") (+ y (y self) (round size 3)) (+ y (y self))))
             othernote)
        (when (or (equal tie 'begin) (equal tie 'continue))
          (setf othernote (get-next-tied-noted self))
          (when othernote
            (draw-liason-begin (+ realpos (round size 3)) toptie 
                               (round (+ realpos (* zoom (abs (- (x othernote) (x self))))))
                               bottie direction)))
        (when (or (equal tie 'end) (equal tie 'continue))
          (setf othernote (get-last-tied-noted self))
          (when othernote
            (draw-liason-end (round (- (+ realpos (round size 3))  (* zoom (abs (- (x self) (x othernote))))))
                             toptie realpos bottie direction)))))))

(defun draw-liason-begin (left top right bot direction)
  (let (major-angle)
    (if (string-equal direction "down")
        (setf major-angle 90) 
      (setf major-angle 180))
    (om-draw-ellipse-arc (om-make-point (+ left (round (- right left) 2)) (+ top (round (- bot top) 2)))
                                           (round (- right left) 2) (round (- bot top) 2)
                                           180 major-angle 90)))



(defun draw-liason-end (left top right bot direction)
  (let (major-angle l-angle)
      (if (string-equal direction "down")
          (setf major-angle 0) 
        (setf major-angle 270))   
    (om-draw-ellipse-arc (om-make-point (+ left (round (- right left) 2)) (+ top (round (- bot top) 2)))
                          (round (- right left) 2) (round (- bot top) 2) 180 major-angle 90)))




(defmethod get-tie-direction ((self grap-note))
   (let* ((note (midic (reference self)))
          (list (sort (lmidic (parent (reference self))) '<)))
     (if (>= (position note list :test 'equal) (ceiling (/ (length list) 2)))
       "up" "down")))

;--------drawing points for rhythmic notes
(defun write-note-points (self  x y  size )
  (om-with-font (om-make-font *om-def-font-face*  size)
                (loop for i from 1 to (points self) do
                      (om-draw-char  (+ x (* (- i 1) (round size 4))) y  #\.))))


;--------drawing Open Music SLOTS
(defun write-note-slot (self realpos y slot size zoom) 
  (unless (equal slot 'midic)
    (let ((new-font (om-make-font *om-def-font-face*  (round (* size 0.8)))))
      (om-with-fg-color nil (nth (chan (reference self)) *16-color-list*) 
        (cond
         ((equal slot 'midic) nil)
         ((equal slot 'chan)
          (om-draw-string  (+ realpos (round size 4) 3) y (format () "~D" (chan (reference self)))))
         ((equal slot 'port)
          (om-draw-string  (+ realpos (round size 4) 3) y (format () "~D" (port (reference self)))))
         ((equal slot 'dur)
          (om-fill-rect realpos y
                        (ms2pixel (dur (reference self)) (round size 4) zoom)
                        (round size 12)))
         ((equal slot 'dyn)
          (om-draw-string  (+ realpos (round size 4) 3) y (get-dyn-from-vel (vel (reference self))))))))))


;**********CHORD**********
(defclas grap-chord (grap-container) 
  ((stem :initform nil)))

;***************
;Make a GRAP-CHORD
;***************

(defmethod make-graph-form-obj ((self chord)  x top linespace  mode scale sel system stem)
  (let* ((thenotes (copy-list (inside self)))
         (thenotes (cond 
                    ((or (=  mode 3) (=  mode 4)) thenotes)
                    ((= mode 2) (sort thenotes '> :key 'midic))
                    (t (sort thenotes '< :key 'midic))))
         (offsets (Loffset self))
         (zigzag-list (make-alt-zig-zag self scale))
         (note-head-list (make-chord-zig-zag self scale))
         (grap-notes (loop for item in thenotes
                           for pos in note-head-list
                           for i = 0 then (+ i 1)
                           collect
                           (let (notegrap)
                             (cond
                              ((= mode 0)
                               (setf notegrap (make-graph-form-obj item (round (+ x (* linespace pos))) top linespace 0 scale sel system stem))
                               (setf (delta-head notegrap) pos)
                               (when (natural-alt-char notegrap)
                                 (setf (alteration notegrap) (correct-alteration notegrap (pop zigzag-list)))          
                                 )
                               )
                              ((= mode 4)
                               (let* ((off (round (* (pop offsets) *onesec* 4 linespace) 1000)))
                                 (setf notegrap (make-graph-form-obj item (+ x off) top linespace 0 scale sel system stem))
                                 (when (natural-alt-char notegrap)
                                   (setf (alteration notegrap) (correct-alteration notegrap (pop zigzag-list))))
                                 ))
                              (t (setf notegrap (make-graph-form-obj item (round (+ x (* 8 linespace i))) top linespace 0 scale sel system stem))
                                 (when (alt-char notegrap)
                                   (setf (alteration notegrap) -1))))
                             notegrap)))
         (newchord (make-instance 'grap-chord
                     :reference self
                     :main-point (list x 0)
                     :rectangle (list 0 0 0 0)
                     :stem (if (and stem (or (= mode 0) (= mode 4))) (round (* 3 linespace)))
                     :selected (member self sel :test 'equal)
                     :inside grap-notes)))
    (set-parent newchord grap-notes)
    (make-graphic-extras newchord)
    newchord))



;***************
;DRAW a GRAP-CHORD
;***************

(defmethod draw-object ((self grap-chord) x y zoom minx maxx miny maxy slot size linear? staff grille-p chnote)
  (loop for item in (inside self) do
        (draw-object item x y zoom minx maxx miny maxy slot size linear? staff nil chnote))
  (when *om-tonalite*
    (draw-chiffrage self x y zoom size))
  (when (and (not grille-p) (stem self))
    (draw-stem  self  (round (+  x (/ size 3.5) (* zoom (x self)))) y (selected self) (stem self)))
  (collect-rectangles self)
  (draw-extras self size staff))

(defmethod draw-stem ((self grap-chord) x y sel stemsize)
  (when (inside self)
    (let* ((thenotes (copy-list (inside self)))
           (thenotes (sort thenotes '< :key 'y))
           (y-min (y (car thenotes)))
           (y-max (y   (car (last thenotes)))))
      (om-with-fg-color nil (mus-color (reference self))
        (om-draw-line x (+ y (- y-min stemsize)) x (+ y y-max))))))


;**********CHORD-SEQ**********
(defclas grap-chord-seq (grap-container) ())


;===MAKE
(defmethod make-graph-form-obj ((self chord-seq)  x top linespace mode scale sel system stem)
   (let* ((chordlist (loop for item in (chords self)
                           for off in (notEndLOnset self) collect
                           (make-graph-form-obj item (+ x (ms2pixel off linespace 1)) top linespace mode scale sel system stem)))
          (newc-s (make-instance 'grap-chord-seq
                    :reference self
                    :inside chordlist)))
     (set-parent newc-s chordlist)
     (make-graphic-extras newc-s)
     newc-s))


;===DRAW
(defmethod draw-object ((self grap-chord-seq) x y zoom minx maxx miny maxy slot size modepage? staff grille-p chnote)
  (if modepage?
    (let* ((pageh (car modepage?))
           (pagination (second modepage?))
           (params (third modepage?)))
      (draw-in-page-mode self pagination pageh 0 1 x y zoom minx maxx 
                         miny maxy slot size params staff grille-p chnote))
    (let* ( previous)
      (loop for item in (inside self) 
            while (<= (+ x (round (* zoom (x item)))) maxx) do
            (when (>= (round (* zoom (x item))) minx)
              (draw-object item x y zoom minx maxx miny maxy slot size nil staff grille-p chnote)
              (when *om-tonalite*
                (draw-modulation self item previous x y zoom size)
                (setf previous item))))
      (draw-chord-seq-grille maxx x size zoom grille-p miny   maxy)
      (collect-rectangles self)
      (draw-extras self size staff))))

(defun draw-chord-seq-grille (maxx x size zoom grille-p miny   maxy)
  (when grille-p
    (let ((endxms (* (ceiling (pixel2ms (- maxx x)  (/ size 4) zoom) 1000) 1000)))
      (om-with-pen (nil :line :gray-line)
        (loop for i from 0 to endxms  by grille-p do
              (let ((posx (+ x (ms2pixel i (/ size 4) zoom))))
                (om-draw-line posx  miny  posx  maxy)))))))


;**********GRAP-MULTISEQ**********
(defclas grap-multiseq (grap-container) ())

(defmethod make-graph-form-obj ((self multi-seq)  x top linespace mode scale sel system stem)
   (declare (ignore top))
   (let* ((cslist (loop for item in (inside self)
                        for i = 0 then (+ i 1)
                        collect
                        (make-graph-form-obj item x (top-in-midi (nth i system)) linespace  
                                             mode scale sel (nth i system) stem)))
          (newmulti (make-instance 'grap-multiseq
                      :reference self
                      :inside cslist)))
     (set-parent newmulti cslist)
     (make-graphic-extras newmulti)
     newmulti))
     
; linear? c'est pas le bon nom pour ce paramettre
(defmethod draw-object ((self grap-multiseq) x y zoom minx maxx miny maxy slot size modepage? staff grille-p chnote)
  (let* ((pageh (car modepage?))
         (pagination (second modepage?))
         (linear? (first modepage?)))
    (if linear?
      (let* ((def-prefer (third (copy-list (second modepage?))))
             (pos&size (get-staff-pos&size staff (fourth def-prefer)))
             (positions (cdr pos&size)))
        (loop for item in (inside self)
              for i = 0 then (+ i 1)
              for posy in positions do
              (draw-object item x (+ (round (* size posy)) y) zoom minx maxx miny maxy slot size nil (nth i staff) grille-p chnote))
        (collect-rectangles self)
        (draw-extras self size staff)
        )
      (let* ((def-prefer (third (copy-list (second modepage?))))
             (interlinea (third def-prefer))
             (pos&size (get-staff-pos&size staff (fourth def-prefer)))
             (positions (cdr pos&size))
             (total (car pos&size)))
        (loop for item in (inside self)
              for i = 0 then (+ i 1)
              for posy in positions do
              (if linear?
                (setf def-prefer nil)
                (setf (nth 2 def-prefer) (- (+ interlinea total) (/ (system-size-in-lines (nth i staff)) 4))))
              (draw-object item x (+ (round (* size posy)) y) zoom minx maxx miny maxy slot size def-prefer (nth i staff) grille-p chnote))
        (collect-rectangles self)
        (draw-extras self size staff)
        ))))



;===================================================================================================
;=================================RYTHM CLASSES=====================================================
;===================================================================================================

;ca merite un autre fichier

;-------------predicates------------------

; je ne sais rien

(defmethod figure-?  ((self t)) 
   (not (group-p (parent (reference self)))))

(defmethod first-of-figure?  ((self t))
   (and (figure-? (parent self))
        (first-of-group? self)))

(defmethod first-of-group?  ((self t)) 
   (equal self (car (inside (parent self)))))

(defmethod last-of-group?  ((self t)) 
   (equal self (car (last (inside (parent self))))))



;**********GRAP-POLY**********

(defclas grap-poly (grap-container) ())


;****MAKE
(defmethod make-graph-form-obj ((self poly)  x top linespace mode scale sel system stem)
  (let ((graph-obj (make-graph-ryth-obj self top system linespace  scale sel nil nil nil)))
    (space-objects (get-temporal-objects graph-obj) (* 4 linespace))
    (set-graph-rectangles graph-obj)
    graph-obj))

;****RHYTHMIC MAKE
(defmethod make-graph-ryth-obj ((self poly) top staffsys linespace  scale sel pere durtot &optional ryth)
   (declare (ignore ryth durtot))
   (let* ((thepoly (make-instance 'grap-poly :reference self :parent pere))
          (voicelist (loop for item in (inside self)
                           for i = 0 then (+ i 1) collect
                           (make-graph-ryth-obj item (top-in-midi (nth i staffsys))  (nth i staffsys) linespace  scale sel thepoly nil))))
     (setf (inside thepoly) voicelist)
     (make-graphic-extras thepoly)
     thepoly))

;****DRAW
;ca marche mais on ne comprnd rien
(defmethod draw-object ((self grap-poly) x y zoom minx maxx miny maxy slot size modepage? staff grille-p chnote)
  (let* ((pageh (car modepage?))
         (pagination (second modepage?))
         (linear? (first modepage?)))
    (if linear?
      (let* ((def-prefer (third (copy-list (second modepage?))))
             (pos&size (get-staff-pos&size staff (fourth def-prefer)))
             (positions (cdr pos&size))
             (meas-list (cdr (get-aligne-measures (reference self))))
             )
        (loop for item in (inside self)
              for i = 0 then (+ i 1)
              for posy in positions do
              (draw-object item x (+ (round (* size posy)) y) zoom minx maxx miny maxy slot size nil (nth i staff) grille-p chnote))
        (collect-rectangles self)
        (draw-aligned-measures self meas-list staff size y positions)
        (draw-extras self size staff))
      (let* ((def-prefer (copy-list (second linear?)))
             (interlinea (third def-prefer))
             (pos&size (get-staff-pos&size staff (fourth def-prefer)))
             (positions (cdr pos&size))
             (total (car pos&size))
             (meas-list (cdr (get-aligne-measures (reference self))))
             )
        (loop for item in (inside self)
              for i = 0 then (+ i 1)
              for posy in positions do
              (if (first linear?)
                (setf def-prefer nil)
                (setf (nth 2 def-prefer) (- (+ interlinea total) (/ (system-size-in-lines (nth i staff)) 4))))
              (draw-object item x (+ (round (* size posy)) y) zoom minx maxx miny maxy slot size def-prefer (nth i staff) grille-p chnote))
        (collect-rectangles self)
        (draw-aligned-measures self meas-list staff size y positions)
        (draw-extras self size staff)))))


; =======
;POUR TRACE UNE LONG LINE ENTRE MEASURES CONSECUTIVES

(defmethod get-aligne-measures ((self poly))
   (let* ((ofsets (loop for item in (inside self)
                        collect (loop for mes in (inside item) collect (offset->ms mes))))
          (all (sort (remove-duplicates (flat ofsets) :test 'equal) '<)) rep)
     (loop for item in all do
           (let (resp )
             (loop for voice in ofsets
                   for i = 0 then (+ i 1) do
                   (let ((pos (position item voice)))
                     (when pos (push (list pos i) resp))))
              (push (reverse resp) rep)))
     (reverse rep)))

(defun make-aligned-groups (list)
  (let (rep temp)
    (loop for i from 0 to (- (length list) 1) do
          (let ((cur (nth i list)))
            (if (null temp) (push cur temp)
                (if (= (second cur) (+ (second (car temp)) 1))
                  (push cur temp)
                  (progn
                    (push (reverse temp) rep)
                    (setf temp (list cur)) )))))
    (when temp (push (reverse temp) rep))
    (reverse rep)))
    

(defmethod draw-aligned-measures ((self grap-poly) list staff size y positions)
   (loop for group in list do
         (let* ((measures (loop for item in group collect 
                                (nth (first item) (inside (nth (second item) (inside self))))))
                (min (loop for item in measures minimize (first (rectangle item)))) subgroups)
           (loop for item in measures do
                 (setf (nth 0 (rectangle item)) min))
           (setf subgroups (make-aligned-groups group))
           (loop for bar in subgroups do
                 (let* ((firstm (car bar))
                        (lastm (car (last bar)))
                        (realmes (nth (first firstm) (inside (nth (second firstm) (inside self)))))
                        (ys (+ y (round (* size (nth (second firstm) positions)))
                               (line2pixel (posy (car (staff-list (nth (second firstm) staff)))) nil (/ size 4))))
                        (ysf (+ y (round (* size (nth (second  lastm) positions))) 
                                (line2pixel (posy (car (last (staff-list (nth (second  lastm) staff))))) nil (/ size 4)))))
                   (om-draw-line  (car (rectangle realmes))  ys 
                                 (car (rectangle realmes)) (+ ysf size)))))))
; =======

;**********GRAP-VOICE**********

(defclas grap-voice (grap-container)  
   ((show-chifrage :initform nil :accessor show-chifrage )))

;==MAKE
(defmethod make-graph-form-obj ((self voice)  x top linespace mode scale sel system stem)
   (let ((graph-obj (make-graph-ryth-obj  self top system linespace  scale sel nil nil nil)))
     (space-objects (get-temporal-objects graph-obj) (* 4 linespace))
     (set-graph-rectangles graph-obj)
     graph-obj))

;==RHYTHMIC MAKE
(defmethod make-graph-ryth-obj ((self voice) top staffsys linespace  scale sel pere durtot &optional ryth)
   (declare (ignore ryth durtot))
   (let* ((thevoice (make-instance 'grap-voice :reference self :parent pere))
          (meslist (loop for item in (inside self) collect
                           (make-graph-ryth-obj item  top staffsys linespace  scale sel thevoice nil))))
     (setf (inside thevoice) meslist)
     (make-graphic-extras thevoice)
     thevoice))



;==DRAW
(defmethod draw-object ((self grap-voice) x y zoom minx maxx miny maxy slot size modepage? staff grille-p chnote)
  (let* ((pageh (car modepage?))
         (pagination (second modepage?))
         (linear? (third modepage?)))
    
    (loop for i from 0 to (- (length (inside self)) 1) 
          do (let ((cur-mes (nth i (inside self)))
                   (next-mes (nth (+ i 1) (inside self)))) 
               (if (or (null next-mes) (> (round (* zoom (x next-mes) )) minx))
                 (draw-object-ryth cur-mes x y zoom minx maxx miny maxy slot size linear? staff chnote))))
    (collect-rectangles self)
    (remake-measures (inside self))
    (draw-extras self size staff)))


(defmethod draw-object-ryth ((self t) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
   (draw-object self x y zoom minx maxx miny maxy slot size linear? staff nil chnote)
  (draw-extras self size staff)
  )

(defmethod draw-object-ryth ((self grap-container) x y zoom minx maxx miny maxy slot size linear? staff chnote)
   (loop for item in (inside self) do
         (draw-object-ryth item x y zoom minx maxx miny maxy slot size linear? staff chnote)))


(defmethod draw-object-ryth ((self voice) x y zoom minx maxx miny maxy slot size linear? staff chnote)
  (call-next-method)
  (remake-measures (inside self))
  (draw-extras self size staff))


;**********GRAP-MEASURE**********

(defclas grap-measure (grap-container) 
   ((metric :initform nil)))

(defmethod grap-measure-p ((self grap-measure)) t)
(defmethod grap-measure-p ((self t)) nil)


(defmethod make-graph-ryth-obj ((self measure) top staffsys linespace  scale sel pere durtot &optional ryth)
   (declare (ignore ryth))
   (let* ((tree (tree self))
          (real-beat-val (/ 1 (fdenominator (first tree))))
          (symb-beat-val (/ 1 (find-beat-symbol (fdenominator (first tree)))))
          (themes (make-instance 'grap-measure
                    :reference self
                    :metric (list (fnumerator (first tree))
                                  (fdenominator (first tree)))
                    :parent pere))
          (inside (loop for item in (inside self) 
                        for i = 0 then (+ i 1)
                        collect 
                        (let* ((dur-obj-noire (/ (extent item) (qvalue item)))
                               (factor (/ (* 1/4 dur-obj-noire) real-beat-val)))
                          (make-graph-ryth-obj item  top staffsys linespace  scale sel themes 
                                               (* symb-beat-val factor) (list real-beat-val (nth i (cadr (tree self)))))))))
     (setf (inside themes) inside)
     (make-graphic-extras themes)
     themes))


(defmethod measure-number ((self measure))
   (+ 1 (position self (inside (parent self)) :test 'equal)))


(defmethod show-chifrage ((self grap-measure))
   (and (parent self) (show-chifrage (parent self))))

(defmethod get-chifrage-space ((self grap-measure) size)
   (if (or (show-chifrage self) (first-of-group? self)) size
       (let ((previous-mes (nth (- (position self (inside (parent self)) :test 'equal) 1) (inside (parent self)))))
         (if (equal (metric self) (metric previous-mes))
           (round size 1) size))))


(defmethod draw-object-ryth ((self grap-measure) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
   (let ((space (get-chifrage-space self size))
         (deltachiff (round size 5))
         (poly-p (poly-p (GET-ROOT-PARENT (reference self)))))
     (let ((previous nil))
       (loop for item in (inside self) do
             (draw-object-ryth item (+ x space) y zoom minx maxx miny maxy slot size linear? staff chnote)
       ;;;=================
       (when *om-tonalite*
         (setf previous (draw-modulation (parent self) item previous x y zoom size)))
       ;;;=================
       ))
     (collect-rectangles self)
     (setf (nth 0 (rectangle self)) (- (nth 0 (rectangle self)) space))
     (om-with-fg-color nil *system-color*
       (loop for thestaff in (staff-list staff) do
             (let ((ys (+ y (line2pixel (posy thestaff) nil (/ size 4)))))
               (setf (nth 1 (rectangle self)) (min (nth 1 (rectangle self)) ys))
               (setf (nth 3 (rectangle self)) (max (nth 3 (rectangle self)) (+ ys (system-size-in-pix staff size))))
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
                       ))))
               (unless (or (first-of-group? self) poly-p)
                 (om-draw-line  (car (rectangle self))  ys 
                             (car (rectangle self)) (+ ys size))))))
     (draw-extras self size staff)
     ))


(defun remake-measures (list)
  (loop for i from 0 to (- (length list) 2) do
        (setf (nth 2 (rectangle (nth i list))) (nth 0 (rectangle (nth (+ i 1) list))))))


;**********GRAP-RYTH-NOTE**********
(defclas grap-ryth-note (grap-note) 
   ((points :initform nil)
    (tie :initform nil)
    (durtot :initform 1/4)))

(defmethod make-graph-ryth-obj ((self note) top staffsys linespace scale sel pere durtot &optional ryth)
   (declare (ignore ryth))
   (let* (
          ;(ypos (midi2pixel (midic self) top linespace scale)) 
          (ypos (get-graphic-pos self top linespace scale)) 
          thenote)
     (setf thenote
           (make-instance 'grap-ryth-note
             :reference self
             :parent pere
             :durtot durtot
             :main-point (list 0 (- ypos (round linespace 2)))
             :selected (member self sel :test 'equal)
             :auxlines (get-aux-lines self staffsys top scale linespace ypos)))
     (make-graphic-extras thenote)
     thenote))


;**********GRAP-REST**********
(defclas grap-rest (simple-graph-container) 
   ((durtot :initform 1/4)
    (stemdir :initform "up")
    (headchar :initform (rest-1/4))
    (bigrest :initform nil)
    (beams-num :initform 0)
    (propre-group :initform nil)
    (points :initform nil)))

(defmethod is-rest-? ((self grap-rest)) t)
(defmethod is-rest-? ((self t)) nil)

(defun default-rest-position (system)
   (if (and system (staff-list system))
     (let ((max (car (range (first (staff-list system)))))
           (min (second (range (first (staff-list system))))))
       (* 100 (+ min (round (- max min) 2))))
     7200))

(defmethod make-graph-ryth-obj ((self rest) top staffsys linespace scale sel pere durtot &optional ryth)
   (declare (ignore head ryth))
   (let* ((ypos (midi2pixel (default-rest-position staffsys)  top linespace scale))
          (symb-info (note-head-and-points (abs durtot) t))
          (str (first symb-info))
          (str (if (listp str) (string+ (rest-1) (num2sstr (car str)) (rest-1)) str)) 
          delta-y sizex  beams beams-num obj onlyhead)
     (setf onlyhead (first symb-info))
     (loop for i from 1 to (second symb-info) do
           (setf str (string+ str ".")))
     (setf sizex (get-name-size str (om-make-font *music-font* (round (* 4 linespace)))))
     (setf beams (get-number-of-beams durtot))
     (setf beams-num (if (listp beams) (car beams) beams))
     (setf delta-y (if (< beams-num 1) (list (round  linespace -2) 0) (list 0 0))) 
     (setf obj (make-instance 'grap-rest
                 :reference self
                 :durtot durtot
                 :headchar (first symb-info)
                 :points (second symb-info)
                 :beams-num  beams-num
                 :propre-group (if (listp beams) (second beams))
                 :main-point (list 0 (round (+ (car delta-y) ypos)))
                 :parent pere
                 :rectangle (list 0 0 sizex (apply '+ delta-y))
                 :selected (member self sel :test 'equal)))
     (when (listp  onlyhead)
       (setf (bigrest obj) (car onlyhead))
       (setf (headchar obj) (rest-1)))
     (make-graphic-extras obj)
     obj))


(defun delta-beams-rest (i)
  (cond
   ((<= i 0) 0)
   ((or (= i 1) (= i 2)) 3)
   (t (+ 3 i))))


(defun draw-long-silence (size x y num points)
  (let ((str (num2sstr num)))
    (om-draw-line x (- y (round size 8)) x (+ y (round size 4)))
    (om-fill-rect    x y (round size 1.5) (round size 8))
    (setf x (+ x (round size 1.5) (round size 16)))
    (om-draw-string  x (+ y (round size 4)) str)
    (setf x (+ x (get-name-size str (om-make-font *music-font* size)) (round size 16)))
    (om-fill-rect    x y (round size 1.5) (round size 8))
    (setf x (+ x (round size 1.5)))
    (om-draw-line x (- y (round size 8)) x (+ y (round size 4)))
    (setf x (+ x (round size 8)))
    (om-with-font (om-make-font *music-font* (round (* size 1.7)))
                  (loop for i from 1 to points do
                        (om-draw-char  (+ x (* (- i 1) (round size 4))) (+ y (round size 8))  #\.)))
  ))


(defun delta-beams-rest (i)
  (if (= i 1) 2 i))

(defun delta-beams-up (i)
  (cond ((<= i 0) 0)
        ((> i 5) (- i 4))
        (t 1)))

(defun delta-beams-dwn (i)
  (cond ((<= i 0) 0)
        ((> i 5) (- i 3))
        (t 2)))

(defun delta-rect-dwn (i)
  (cond ((<= i 1) 0)
        ((> i 5) (- i 3))
        (t 2)))



;
;toute a refaire dans cette fonction
;
;


(defmethod draw-object-ryth ((self grap-rest) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
   (declare (ignore linear? maxy slot staff miny minx maxx))
   (let* ((realpos (round (+ x  (* zoom (x self)))))
          (str (headchar self))
          (line (case (elt str 0) (#\·  (round size 4)) (#\î 0)))
          (strsizex (get-name-size str (om-make-font *music-font* size)))
          delta-y delta-h )
     (if (group-p (reference (parent self)))
       (progn 
         (if (string-equal (stemdir self) "up")
           (progn
             (case (elt str 0)
               ((code-char 183)  (setf delta-y (round size 4)
                           delta-h 0))
               ((code-char 238) (setf delta-y (round size 8)
                          delta-h (round size 8)))
               ((code-char 206) (setf delta-y (round size 2) 
                          delta-h (round size 2)))
               (otherwise (setf delta-y (* (round size 4) (delta-beams-rest (beams-num self)))
                                delta-h (if (= (beams-num self) 1) (round size 4) (round size 2)))))
             (setf (rectangle self) (list realpos (+ y (y self) (* (delta-beams-up (beams-num self))  (round size 4)))
                                          (+ realpos strsizex) 
                                          (+ y (y self) delta-y delta-h))))
           (progn
             (case (elt str 0)
               ((code-char 183)  (setf delta-y (round size -4)
                           delta-h 0))
               ((code-char 238) (setf delta-y (round size -8)
                          delta-h (round size 8)))
               ((code-char 206) (setf delta-y (round size -2) 
                          delta-h (round size )))
               (otherwise (setf delta-y (* (round size -4)  (delta-beams-dwn (beams-num self))  )
                                delta-h (if (= (beams-num self) 1) (round size 4) (round size 2)))))
             (setf (rectangle self) (list realpos (+ y (y self) delta-y (* (round size -4) (- (min 5 (delta-beams-rest (beams-num self))) 1)))
                                          (+ realpos strsizex) 
                                          (+ y (y self))))))
         (om-with-fg-color nil  (mus-color (reference self)) 
           (if (bigrest self)
             (draw-long-silence size realpos (+ y (y self) delta-y) (bigrest self) (points  self))
             
             (progn
                (om-draw-string  realpos (+ y (y self) delta-y) str)
               (write-note-points self  (+ (round size 2) realpos) (+ y (y self) delta-y) size)
               (when line
                 (om-draw-line (- realpos (round size 8)) (- (+ y (y self) delta-y) line) (+ realpos (round size 12/5)) (- (+ y (y self) delta-y) line))))
             )))
       (progn
         (case (elt str 0)
           ((code-char 183)  (setf delta-y (round size 4)
                       delta-h 0))
           ((code-char 238) (setf delta-y (round size 8)
                      delta-h (round size 8)))
           ((code-char 206) (setf delta-y (round size 2) 
                      delta-h (round size 2)))
           (otherwise (setf delta-y (* (round size 4) (min 4 (- (delta-beams-rest (beams-num self)) 1)))
                            delta-h (if (= (beams-num self) 1) (round size 4) (round size 2)))))
         (draw-one-rest self str strsizex size realpos (+ y (y self)) line delta-y delta-h)
         ))
     (draw-extras self size staff)))
;OJOJ faltan los grandes
     

(defmethod draw-one-rest ((self grap-rest) str strsizex size xpos ypos line delta-y delta-h) ;ypos = y + (y self) xpos = realpos
   (om-with-fg-color nil (mus-color (reference self))
     (if (bigrest self)
       (draw-long-silence size xpos ypos (bigrest self) (points  self))
       (progn
          (om-draw-string  xpos ypos str)
          (write-note-points self  (+ (round size 2) xpos) ypos size)
         (when line
           (om-draw-line (- xpos (round size 8)) (- ypos line) (+ xpos (round size 12/5)) (- ypos line)))))
     (setf (rectangle self) (list xpos (- ypos delta-y) (+ xpos strsizex) (+ ypos delta-h)))))



;**********GRAP-RYTH-CHORD**********
(defclas grap-ryth-chord (grap-chord) 
   ((durtot :initform 1/4)
    (points :initform 4 )
    (beams-num :initform 4 )
    (stemdir :initform "up")
    (propre-group :initform nil)
    (bigchord :initform nil)
    (stemhigh :initform 1)))


;-----------------------------------------

(defmethod grap-ryth-chord-p  ((self grap-ryth-chord)) t)
(defmethod grap-ryth-chord-p  ((self t)) nil)

(defmethod make-graph-ryth-obj ((self chord)  top staffsys linespace  scale sel pere durtot &optional ryth)
   (let* ((thenotes (sort (copy-list (inside self)) '< :key 'midic))
          (onlyhead (note-head-and-points durtot))  ;(onlyhead (/ (second ryth) (first ryth))))
          (points (second onlyhead))
          (onlyhead (first onlyhead))
          (beams (get-number-of-beams durtot)) 
          (beams-num (if (listp beams) (car beams) beams))
          (zigzag-list (make-alt-zig-zag self scale))
          (note-head-list (make-chord-zig-zag self scale))
          (new-chr (make-instance 'grap-ryth-chord
                     :reference self
                     :parent pere
                     :durtot durtot
                     :stem (not (or (listp  onlyhead) (equal onlyhead (head-4)) 
                                    (equal onlyhead (head-2)) (equal onlyhead (head-1)))) 
                     :selected (member self sel :test 'equal)))
          (maxw 0))
     (when (listp  onlyhead)
       (setf (bigchord new-chr) (car onlyhead))
       (setf onlyhead (head-2)))
     (setf (inside new-chr)
           (loop for item in thenotes
                 for pos in note-head-list
                 for i = 0 then (+ i 1)
                 collect
                 (let ((notegrap (make-graph-ryth-obj item  top staffsys linespace  scale sel new-chr durtot ryth))
                       ;(alt-char (note-alteration item (scale-list item scale)))
                       (alt-char (get-alt-char item scale (armure staffsys)))
                       notew)
                   (setf (delta-head notegrap) pos)
                   (setf (points notegrap) points)
                   (setf (headchar notegrap) onlyhead)
                   (when alt-char
                     (setf (alt-char notegrap) alt-char))
                   (when (natural-alt-char notegrap)
                     (setf (alteration notegrap) (correct-alteration notegrap (pop zigzag-list))))
                   (setf notew (round (* 2 (max (+ (* linespace points) (* linespace pos))
                                                (* linespace (if (alteration notegrap) (* -1 (- (alteration notegrap) 1)) 0))))))
                   (setf (rectangle notegrap) (list 0 0 notew (round linespace)))
                   (setf (nth 0 (main-point notegrap)) (round (* linespace pos)))
                   (setf maxw (max maxw notew))
                   notegrap)))
     (setf (beams-num new-chr) beams-num)
     (setf (propre-group new-chr) (if (listp beams) (second beams)))
     (when (and (stem new-chr) (not (group-p (parent self))))
       (setf (stemdir new-chr) (chord-direction new-chr (midicenter staffsys)))
       (setf (stemhigh new-chr) (round (* 3 linespace))))
     (setf (rectangle new-chr)  (list 0 0 maxw 0))
     (make-graphic-extras new-chr)
     new-chr))

(defmethod chord-direction ((self grap-ryth-chord) staff-center)
   (let* ((thenotes (Lmidic (reference self)))
          (thenotes (sort thenotes '<))
          (moyen (round (+ (car (last thenotes)) (car thenotes) ) 2)))
     (if (< moyen (* staff-center 100)) "up" "dw")))
  

(defmethod draw-object-ryth ((self grap-ryth-chord) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
  (om-with-fg-color nil (mus-color (reference self))
                    (let* ((dir (stemdir self))
                           (thenotes (copy-list (inside self))))
                      (loop for item in thenotes do
                            (draw-object-ryth item x y zoom minx maxx miny maxy slot size linear? staff chnote))
                      (collect-rectangles self)
                      (when (bigchord self) 
                        (om-with-font (om-make-font *music-font* (round size 1.6))
                                      (om-draw-string  (+ (car (rectangle self)) (round size 5)) 
                                                      (+ (round size 7) (fourth (rectangle self))) 
                                                      (format nil "~D" (bigchord self)))
                                      ))
                      (when (figure-? self) 
                        (when (stem self)
                          (draw-chord-beams self x y zoom (beams-num self) dir size)))
                      (draw-extras self size staff))
                    
                    (when *om-tonalite*
                      ;(draw-degre self (- (round (+  x (/ size 3.5) (* zoom (x self)))) 4) y size))
                      (draw-chiffrage self x y zoom size))
                    ))


(defmethod draw-chord-beams ((self grap-ryth-chord) x0 y0 zoom numbeams  dir size)
   (let* ((domaine (om+ y0 (get-min-max self)))
          (taille (round (max (+ (/ size 4) (* (- numbeams 1) (/ size 3))) (* size 7/8)))) 
          (yfin  (if (string-equal dir "up") 
                   (- (car domaine)  taille)
                   (+ (second domaine)  taille)))
          (ystart (if (string-equal dir "up") (second domaine) (car domaine)))
          (xpos (if (string-equal dir "up") 
                  (round (+  x0 (/ size 3.5) (* zoom (x self))))
                  (round (+  x0  (* zoom (x self)))))))
     (when (propre-group self)
       (draw-propre-group  xpos ystart (chif2sstr (propre-group self)) (selected self) dir size))
     (draw-stem-no-group  xpos (selected self)  ystart  yfin)
     (loop for i from 0 to (- numbeams 1) do
           (if  (string-equal dir "up")
             (draw-beam-string  xpos (round (+ (+ yfin (* 1/4 size)) (* 3/12 i size))) (beam-up) (selected self))
             (draw-beam-string  xpos (round (- yfin (* 3/14 i size))) (beam-dwn) (selected self))))))


(defun draw-propre-group (x y str sel dir size)
   " x (left) y (top) str (string) sel (selected?)"
   (if  (string-equal dir "up")
       (progn
         (om-draw-line (- x (round size 3.5)) (+ y (round size 4)) (- x (round size 3.5)) (+ y (round size 3)))
         (om-draw-line (- x (round size 3.5)) (+ y (round size 3)) (- x (round size 10)) (+ y (round size 3)))
         (om-draw-string (- x (round size 10)) (+ y (round size 2)) str))
       (progn
         (progn
         (om-draw-line x (- y (round size 4)) x (- y (round size 3)))
         (om-draw-line x (- y (round size 3)) (+ x (round size 4.5)) (- y (round size 3)))
         (om-draw-string (+ x (round size 4)) (- y (round size 5)) str)))))

(defun draw-beam-string (x y str sel)
   " x (left) y (top) str (string) sel (selected?)"
   (om-draw-string x y str))  

(defun draw-beam (x y w h sel)
   " x (left) y (top) w (widh) h (hight) sel (selected?)"
   (om-fill-rect  x y w h)
     )

(defun draw-stem-no-group (x sel ystart yfin)
   " x (x pos in pixel)  sel (if chord is selected) ystart (debut de la line) yfin (fin de la line)"
   (om-draw-line x ystart x yfin))


(defmethod get-min-max ((self grap-ryth-chord))
   "Get the min and max y note position in pixels"
   (let* ((thenotes (copy-list (inside self)))
          (thenotes (sort thenotes '< :key 'y))
          (y-min (y (car thenotes)))
          (y-max (y   (car (last thenotes)))))
     (list y-min y-max)))



;**********GRAP-GROUP**********

(defclas grap-group (grap-container)  
   ((durtot :initform 1/4)
    (numdenom :initform nil)
    (chiflevel :initform nil)
    (dirgroup :initform nil)))

;----Main
(defmethod make-graph-ryth-obj ((self group)   top staffsys linespace  scale sel pere durtot &optional ryth)
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
     
     (setf (numdenom new-group)  (cond
                                  ((not group-ratio) nil)
                                  ((= sympli 1) nil)
                                  (t  (list num denom))))
     
     (setf (inside new-group) (loop for item in (inside self) 
                                    for i = 0 then (+ i 1)
                                    collect
                                    (let ((newchord (make-graph-ryth-obj item  top staffsys linespace  scale sel new-group 
                                                                         (if (not group-ratio) 
                                                                           (let* ((dur-obj (/ (/ (extent item) (qvalue item)) 
                                                                                              (/ (extent self) (qvalue self)))))
                                                                             (* dur-obj durtot))
                                                                           (let* ((operation (/ (/ (extent item) (qvalue item)) 
                                                                                                (/ (extent self) (qvalue self))))
                                                                                  (dur-obj (numerator operation)))
                                                                             (setf dur-obj (* dur-obj (/ num (denominator operation))))
                                                                             (* dur-obj unite)))
                                                                         
                                                                         (list (/ (car (second ryth)) (first ryth))
                                                                               (nth i (cadr (second ryth)))))))
                                      newchord)))
     
     
     (when (numdenom new-group)
       (setf (chiflevel new-group) (calcule-chiff-level new-group)))
     (when (figure-? new-group)
       (setf direstart (calcule-dir-et-start new-group (midicenter staffsys)))
       (set-dir-and-high new-group (car direstart) linespace))
     (make-graphic-extras new-group)
     new-group))

(defmethod set-dir-and-high ((self grap-group) dir linespace)
   (setf (dirgroup self) dir)
   (loop for item in (inside self) do
         (set-dir-and-high item dir linespace)))


(defmethod set-dir-and-high ((self grap-ryth-chord) dir linespace)
   (setf (stemdir self)  dir)
   (setf (stemhigh self) (round (max (* 3 linespace) (+ (* 2 linespace) (* (/ (beams-num self) 2)  linespace))))))

(defmethod set-dir-and-high ((self grap-rest) dir linespace)
   (setf (stemdir self)  dir)
   (setf (rectangle self) (list (first (rectangle self)) (second (rectangle self)) 
                                (third (rectangle self)) (round (* 2 linespace)))))

(defmethod draw-object-ryth ((self grap-group) x y zoom minx maxx miny maxy slot size linear?  staff chnote)
   (om-without-interrupts  
    (call-next-method)
    (collect-rectangles self))
   (om-with-fg-color nil (mus-color (reference self))
     (let ((dire (dirgroup self)))
       (when (figure-? self)
         (group-draw-stems self dire x y (rectangle self) zoom size)
         (draw-beams-note-in-group self dire x -1 (rectangle self) zoom size)
         (draw-num-denom-s self x size (rectangle self)))))
   (draw-extras self size staff))


(defmethod draw-num-denom-s ((self t) x size recttop) t)

(defmethod draw-num-denom-s ((self grap-group) x size recttop)
   (let ((dire (dirgroup self)) deltachif top)
     (when (numdenom self)
       (if (string= dire "up")
         (progn
           (setf deltachif (* -1 (- (chiflevel self) 1) (round size 2)))
           (setf top (- (second recttop) (round size 16))))
         (progn
           (setf deltachif (* (- (chiflevel self) 1) (round size 2)))
           (setf top (+ (fourth recttop) (round size 16)))))
       (draw-num-denom  (round (+ deltachif top)) (numdenom self)
                        (car (rectangle self)) 
                        (round (third (rectangle self))) size dire)))
   (loop for item in (inside self) do (draw-num-denom-s item x size recttop)))



(defmethod get-atoms-in-group ((self grap-chord)) (list self))

(defmethod get-atoms-in-group ((self grap-group))
  (loop for item in (inside self)
        append (get-atoms-in-group item)))

(defmethod get-atoms-in-group ((self grap-rest)) (list self))

(defun set-shared-from-prev (prev-atom cur-atom)
  (if (or (last-of-group? prev-atom) (first-of-group? cur-atom))  
    (min 1 (beams-num prev-atom) (beams-num cur-atom))
    (min (beams-num prev-atom) (beams-num cur-atom))))

(defun draw-court-beams (cur-atom propres-beams dir x y shared-beams rect zoom size) 
   (draw-n-court-beams  cur-atom propres-beams
                        dir x (if  (string-equal dir "up")
                                (+ y (* shared-beams (inter-beam-space size)))
                                (- y (* shared-beams (inter-beam-space size))))
                        rect zoom size))


(defmethod draw-beams-note-in-group ((self grap-group) dir x y rect zoom size)
  (let ((atoms (get-atoms-in-group self)))
    (loop for i from 0 to (- (length atoms) 1) do
          (let* ((cur-atom (nth i atoms))
                 (next-atom (nth (+ i 1) atoms))
                 (prev-atom (unless (zerop i) (nth (- i 1) atoms))) )
            (cond
             ((or (= i 0) (first-of-group? cur-atom))
              (setf shared-beams (if next-atom (set-shared-from-prev cur-atom next-atom) 0))
              (setf propres-beams (- (beams-num cur-atom) shared-beams))
              (when  next-atom
                (draw-n-long-beams  cur-atom shared-beams dir x  (ceiling (* zoom  (- (x next-atom) (x cur-atom))))  y rect zoom size))
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
                  (draw-court-beams cur-atom propres-beams dir x y shared-beams rect zoom size)
                  )))))))))



;==================================
(defun inter-beam-space (size)
   (+ 1 (round (* 3 (/ size 16 )))))

(defun drawNlong-beams (self n dir x sizex y rect zoom size)
   (let* ((ygroup (+ y (if (string-equal dir "up") (second rect) (fourth rect) )))
          (xpos (if (string-equal dir "up")
                  (round (+  x (/ size 3.5) (* zoom (x self))))
                  (round (+  x  (* zoom (x self))))))
          (spacesize (inter-beam-space size)))
     (loop for i from 0 to (- n 1) do
           (if  (string-equal dir "up")
             (draw-beam    xpos (+ ygroup (* spacesize i))
                           sizex (round size 8) (selected self))
             (draw-beam  xpos (- ygroup (* spacesize i))
                         sizex (round size 8) (selected self))))))

(defmethod draw-n-long-beams ((self grap-ryth-chord) n dir x sizex y rect zoom size)
   (drawNlong-beams self n dir x sizex y rect zoom size))

(defmethod draw-n-long-beams ((self grap-rest) n dir x sizex y rect zoom size)
   (drawNlong-beams self n dir x sizex y rect zoom size))

;==================================
(defun drawNcourt-beams (self n dir x y rect zoom size)
   (let* ((ygroup (+ y (if (string-equal dir "up") (second rect) (fourth rect) )))
          (xpos (if (string-equal dir "up")
                  (round (+  x (/ size 3.5) (* zoom (x self))))
                  (round (+  x  (* zoom (x self))))))
          (spacesize (inter-beam-space size)))
     
     (loop for i from 0 to (- n 1) do
           (if  (string-equal dir "up")
             (draw-beam  xpos (+ ygroup (* spacesize i))
                         (round (* size 1/4)) (round size 8) (selected self))
             (draw-beam  xpos (- ygroup (* spacesize i))
                         (round (* size 1/4)) (round size 8) (selected self))))))


(defmethod draw-n-court-beams ((self grap-rest) n dir x y rect zoom size)
   (drawNcourt-beams self n dir x y rect zoom size))

(defmethod draw-n-court-beams ((self grap-ryth-chord) n dir x y rect zoom size)
   (drawNcourt-beams self n dir x y rect zoom size))


(defmethod group-draw-beams ((self grap-ryth-chord) dir x  y rect zoom size)
   (let* ((ygroup (+ y (if (string-equal dir "up") (second rect) (fourth rect) )))
          (xpos (if (string-equal dir "up")
                  (round (+  x (/ (* size 8) 3.5) (* zoom (x self))))
                  (round (+  x  (* zoom (x self)))))
                ))
     
     (loop for i from 0 to (- (beams-num self) 1) do
           (if  (string-equal dir "up")
             (draw-beam  xpos (+ ygroup (round (* 3/2 i size)))
                         (round (* size 10/5)) size (selected self))
             (draw-beam  xpos (- ygroup (round (* 3/2 i size)))
                         (round (* size 10/5)) size (selected self))))))


(defmethod group-draw-stems ((self t) dir x y rect zoom size) t)
(defmethod group-draw-stems ((self grap-group) dir x y rect zoom size)
   (loop for item in (inside self) do
         (group-draw-stems item dir x y rect zoom size)))

(defmethod group-draw-stems ((self grap-ryth-chord) dir x y rect zoom size)
   ""
   (when (stem self)
     (let* ((note-min-max (om+ y (get-min-max self)))
            (ystart (if (string-equal dir "up") (second note-min-max) (first note-min-max)))
            (ygroup (if (string-equal dir "up") (second rect) (fourth rect) )))
       (setf y 0)
       (draw-stem-no-group  (if (string-equal dir "up") 
                              (round (+  x (/ size 3.5) (* zoom (x self))))
                              (round (+  x  (* zoom (x self))))) 
                            (selected self) 
                            (+ y ystart)
                            (+ y ygroup)))))

(defmethod group-draw-stems ((self grap-rest) dir x y rect zoom size)
   ""
   (let* ((ystart (if (string-equal dir "up") (if (>= (beams-num self) 1) 
                                                (+ (second (rectangle self)) (round (* (- (fourth (rectangle self)) 
                                                                                          (second (rectangle self))) 0.4)))
                                                (second (rectangle self))) 
                      (if (>= (beams-num self) 1) 
                        (- (fourth (rectangle self)) (round (* (- (fourth (rectangle self)) 
                                                                  (second (rectangle self))) 0.4)))
                        (fourth (rectangle self)))))
          (ygroup (if (string-equal dir "up") (second rect) (fourth rect) )))
     (draw-stem-no-group  (if (string-equal dir "up") 
                            (round (+  x (/ size 3.5) (* zoom (x self))))
                            (round (+  x  (* zoom (x self))))) 
                          (selected self) 
                          ystart
                          ygroup)))

(defmethod group-draw-stems ((self grap-rest) dir x y rect zoom size)
   ""
   (let* ((ystart (if (string-equal dir "up") (second (rectangle self)) (fourth (rectangle self))))
          (ygroup (if (string-equal dir "up") (second rect) (fourth rect) )))
     (draw-stem-no-group  (if (string-equal dir "up") 
                            (round (+  x (/ size 3.5) (* zoom (x self))))
                            (round (+  x  (* zoom (x self))))) 
                          (selected self) 
                          ystart
                          ygroup)))


;-----Others
;Calcule la profondeur du num and dem d'un group
(defmethod calcule-chiff-level ((self t)) 0)
(defmethod calcule-chiff-level ((self grap-group))
  (+ (if (numdenom self) 1 0) 
     (loop for item in (inside self)
           maximize (calcule-chiff-level item))))


;;Direction ("up" or "dw") and start (in midic) of the group
(defmethod calcule-dir-et-start ((self grap-group) midicenter)
   (let ((notes (get-graph-type-obj self 'grap-note)) )
     (if notes
       (let ((aux 0) (max 0) (min 100000))
         (loop for item in notes do
               (let ((midic (round (midic (reference item)) 100)))
                 (setf aux (+ aux midic))
                 (setf min (min min midic))
                 (setf max (max max midic))))
         (setf aux (round aux (length notes)))
         (if (< aux midicenter)
           (list "up" max )
           (list "dw"  min )))
       (list "up" 60 ))))

#|
;get the start y position (in pixels) of the root group
(defmethod get-most-group-start ((self t)) nil)

(defmethod get-most-group-start ((self grap-group)) 
   (if (figure-? self)
     self;(third (rectangle self))
     (get-most-group-start (parent self))))
|#



;-------------------DRAW----------------------

(defun draw-num-denom (y numdenom x x1 size dir)
   (let* ((fontsize (round size 2.2))
          (ls (round size 4))
          (str (if (listp numdenom)
                 (if (is-binaire? (/ 1 (second numdenom)))
                   (format nil "~D" (car numdenom) )
                   (format nil "~D:~D" (car numdenom) (second numdenom)))
                 (format nil "~D" numdenom)))   ;(chif2sstr numdenom))
          (strspace (get-name-size str (om-make-font *om-def-font-face*  fontsize)))
           xm)
     (loop while (< (- x1 x) strspace) do
           (setf fontsize (- fontsize 1))
           (setf strspace (get-name-size str (list "Arial" fontsize))))
     (setf xm (round (- (+ x x1) strspace) 2))
     (om-with-font (om-make-font *om-def-font-face*  fontsize)
     (setf x (round x))
     (if (string= dir "up")
       (progn
         (om-draw-line  x (- y (round ls 4)) x (- y ls))
         (om-draw-line  x (- y ls) xm (- y ls))
         (om-draw-line  (+ xm strspace) (- y ls) x1 (- y ls))
         (om-draw-string xm  (- y (round ls 2)) str)  
         (om-draw-line  x1 (- y ls) x1 (- y (round ls 4))))
       (progn
         (om-draw-line  x (+ y (round ls 4)) x (+ y ls))
         (om-draw-line  x (+ y ls) xm (+ y ls))
         (om-draw-line  (+ xm strspace) (+ y ls) x1 (+ y ls))
         (om-draw-string xm  (+ y ls ls) str) 
         (om-draw-line  x1 (+ y ls) x1 (+ y (round ls 4))))))))
  
;-----------------------
;for spacing ca merite un fichier
;-----------------------

(defmethod measures-first ((a grap-measure) b)  t)
(defmethod measures-first ((a t) b) nil)
  
(defun put-maxdur (list dur)
  (loop for item in list
        collect (append item (list dur))))

(defmethod get-temporal-objects ((self grap-container))
   (let ((temp-list (sort (collect-temporal-objects self 0) '< :key 'car ))
         (maxdur 0)
         serie? rep)
     (loop for item in temp-list do
           (if serie?
             (if (= (first item) (first (car serie?)))
               (progn
                 (push item serie?)
                 (setf maxdur (max maxdur (third (rectangle (second item))))))
               (progn
                 (setf rep (append rep (put-maxdur (sort serie? 'measures-first :key 'second) maxdur)))
                 (setf serie? (list item))
                 (setf maxdur (third (rectangle (second item))))))
             (progn 
               (setf serie? (list item))
               (setf maxdur (max maxdur (third (rectangle (second item))))))))
     (setf rep (append rep (put-maxdur (sort serie? 'measures-first :key 'second) maxdur)))
     rep))


(defmethod get-temporal-objects ((self grap-container))
   (let ((temp-list (sort (collect-temporal-objects self 0) '< :key 'car ))
         (maxdur 0)
         serie? rep)
     (loop for item in temp-list do
           (if serie?
             (if (= (first item) (first (car serie?)))
               (progn
                 (push item serie?)
                 (setf maxdur (max maxdur (space-size (second item)))))
               (progn
                 (setf rep (append rep (put-maxdur (sort serie? 'measures-first :key 'second) maxdur)))
                 (setf serie? (list item))
                 (setf maxdur (space-size (second item) ))))
             (progn 
               (setf serie? (list item))
               (setf maxdur (max maxdur (space-size (second item)))))))
     (setf rep (append rep (put-maxdur (sort serie? 'measures-first :key 'second) maxdur)))
     rep))
                 


(defmethod collect-temporal-objects ((self grap-container) at)
   (loop for item in (inside self)
         append (collect-temporal-objects item (+ at (offset->ms (reference item))))))

(defmethod collect-temporal-objects ((self grap-rest) at ) 
   (list (list  at  self )))

(defmethod collect-temporal-objects ((self grap-measure) at ) 
   (cons (list at self)
         (loop for item in (inside self)
               append (collect-temporal-objects item (+ at (offset->ms (reference item)))))))

(defmethod collect-temporal-objects ((self grap-group) at)
   (loop for item in (inside self)
         append (collect-temporal-objects item (+ at (offset->ms (reference item))))))

(defmethod collect-temporal-objects ((self grap-ryth-chord) at )
   (list (list  at  self )))

(defun space-objects (list size)
   (let ((count 0)
         (maxsize 0))
     (loop for i from 0 to (- (length list) 1) do
           (let ((obji (nth i list))
                 (obji+1 (nth (+ i 1) list)))
             (setf count (space-girl (second obji+1) (first obji+1)  
                                     (second obji) (first obji) count size (round (third obji) 2)))))))

;enviar por paquettes de pronto

(defvar *factor-spacing* 3/2)

(defun ryhtm2pixels (ms)
  (expt  *factor-spacing* (log (/  ms  1000) 2)))



;===============measure is i

;------before a measure
(defmethod space-girl ((obji+1 grap-measure) (onseti+1 integer) (obji grap-measure) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list count (second (main-point obji))))
   count)

;------before a chord or a rest
(defmethod space-girl ((obji+1 t) (onseti+1 integer) (obji grap-measure) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list count (second (main-point obji))))
   (+ count (round size 4) (get-chifrage-space obji size )))

;===============chord is i
;------last element
(defmethod space-girl ((obji+1 null) onseti+1 (obji grap-ryth-chord)  onseti  count size maxdur)
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (loop for item in (inside obji) do
         (setf (main-point item) (list (+ count (first (main-point item))  maxdur) (second (main-point item)))))
     (+ count  size ))

;------before a measure
(defmethod space-girl ((obji+1 grap-measure) (onseti+1 integer) (obji grap-ryth-chord) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (loop for item in (inside obji) do
         (setf (main-point item) (list (+ count (first (main-point item)) maxdur ) (second (main-point item))))
         )
   (if (zerop (- onseti+1 onseti))
     (+ count  (round size 2)) 
     (+ count (* maxdur 2)  (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4))))

;------before a chord or a rest
(defmethod space-girl ((obji+1 t) (onseti+1 integer) (obji grap-ryth-chord) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (loop for item in (inside obji) do
         (setf (main-point item) (list (+ count (first (main-point item)) maxdur ) (second (main-point item)))))
   (if (zerop (- onseti+1 onseti))
     count
     (+ count (* maxdur 2) (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4))))


;===============rest is i
;------last element
(defmethod space-girl ((obji+1 null) onseti+1 (obji grap-rest)  onseti  count size maxdur)
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (+ count  size ))

;------before a measure
(defmethod space-girl ((obji+1 grap-measure) (onseti+1 integer) (obji grap-rest) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (if (zerop (- onseti+1 onseti))
     (+ count  (round size 2)) ;maxnotedur pero sin decalar
     (+ count (* maxdur 2)  (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4))))

;------before a chord or a rest
(defmethod space-girl ((obji+1 t) (onseti+1 integer) (obji grap-rest) (onseti integer) count size maxdur) 
   (setf (main-point obji) (list (+ count  maxdur) (second (main-point obji))))
   (if (zerop (- onseti+1 onseti))
     count
     (+ count (* maxdur 2) (round (* size (ryhtm2pixels (- onseti+1 onseti)))) (round size 4))))



;==================================================================================

(defmethod space-size ((self grap-measure)) 0) 

(defmethod space-size ((self grap-ryth-chord)) 
   (loop for item in (inside self) maximize  (third (rectangle item))))

(defmethod space-size ((self grap-rest))   (third (rectangle self)) )

(defmethod space-size ((self t) ) 0)     


;-----------------------
;setting bounds

(defmethod collect-rectangles ((self grap-container) )
   (let ((rect (loop for item in (inside self) 
                     maximize (fourth (rectangle item)) into y1
                     maximize (third (rectangle item)) into x1
                     minimize (first (rectangle item)) into x
                     minimize (second (rectangle item)) into y
                     finally (return (list x y x1 y1)))))
     (setf (rectangle self) rect )))


(defmethod collect-rectangles ((self grap-chord) )
   (let ((rect (loop for item in (inside self) 
                     maximize (fourth (rectangle item)) into y1
                     maximize (third (rectangle item)) into x1
                     minimize (first (rectangle item)) into x
                     minimize (second (rectangle item)) into y
                     finally (return (list x y x1 y1)))))
     (if (stem self)
       (setf (rectangle self) (list (car rect) (- (second rect) (stem self)) (third rect) (fourth rect) ))
       (setf (rectangle self) (list (car rect) (second rect)  (third rect) (fourth rect))))))


(defmethod collect-rectangles ((self grap-ryth-chord) )
   (let ((rect (loop for item in (inside self) 
                     maximize (fourth (rectangle item)) into y1
                     maximize (third (rectangle item)) into x1
                     minimize (first (rectangle item)) into x
                     minimize (second (rectangle item)) into y
                     finally (return (list x y x1 y1)))))
     (if (string-equal (stemdir self) "up")
       (setf (rectangle self) (list (car rect) (- (second rect) (stemhigh self)) (third rect) (fourth rect) ))
       (setf (rectangle self) (list (car rect) (second rect) (third rect) (+ (fourth rect) (stemhigh self)) )))))

(defmethod collect-rectangles ((self grap-rest) ) (rectangle self) )

(defmethod collect-rectangles ((self grap-ryth-note) ) (rectangle self) )

(defmethod collect-rectangles ((self grap-note) )  (rectangle self) )




;--------------------------------------

(defun only-rest-p (list ) 
   (let ((rep t))
     (loop for item in list
           while rep do
           (unless (is-rest-? item) (setf rep nil)))
     rep))


(defmethod move-silence-in-group ((self grap-group))
   (let ((atoms (get-atoms-in-group self))
         (dir (dirgroup self))
         moyen)
     (unless (only-rest-p atoms)
       (setf moyen (loop for item in atoms
                         when (not (is-rest-? item)) 
                         maximize (if (string-equal dir "up") (fourth (rectangle item)) (second (rectangle item))) into ymin
                         when (not (is-rest-? item)) 
                         minimize (if (string-equal dir "up") (fourth (rectangle item)) (second (rectangle item))) into ymax
                         finally (return (list ymin ymax))))
       (when moyen 
         (setf moyen (+ (min (car moyen) (second moyen)) (round (abs (- (car moyen) (second moyen))) 2))))
       (loop for item in atoms do
             (when (is-rest-? item)
               (when moyen
                 (if (string-equal dir "up") 
                   (progn
                     (setf (nth 1 (main-point item)) moyen)
                     (setf (nth 3 (rectangle item)) (nth 1 (main-point item)))
                     (setf (nth 1 (rectangle item)) (nth 1 (main-point item))))
                   (progn
                     (setf (nth 1 (main-point item)) moyen)
                     (setf (nth 3 (rectangle item)) moyen)
                     (setf (nth 1 (rectangle item)) (nth 1 (main-point item)))))))))))


(defmethod set-graph-rectangles ((self grap-container))
   (loop for item in (inside self) do (set-graph-rectangles item))
   (collect-rectangles self))

(defmethod set-graph-rectangles ((self grap-group) )
   (loop for item in (inside self) do (set-graph-rectangles item))
   (move-silence-in-group self)
   (collect-rectangles self))
   
   
(defmethod set-graph-rectangles ((self grap-rest) ) 
   (setf (rectangle self) (list (car (main-point self)) (second (main-point self)) 
                                (+ (car (main-point self)) (third (rectangle self))) 
                                (+ (second (main-point self)) (fourth (rectangle self))))))

(defmethod set-graph-rectangles ((self grap-ryth-note) )
   (setf (rectangle self) (list (car (main-point self)) (second (main-point self)) 
                                (+ (car (main-point self)) (third (rectangle self))) 
                                (+ (second (main-point self)) (fourth (rectangle self))))))

(defmethod set-graph-rectangles ((self grap-note) )
   (rectangle self) )






;=========DRAW ?????????
(defmethod real-x0-note ((self grap-note))
   (car (rectangle self)))

(defmethod real-y0-note ((self grap-note) size)
   (+ (second (rectangle self)) (round size 8) ))

;==================  EVENTS


(defmethod click-in-obj ((self grap-container) type where view)
   (if (subtypep (type-of self) type)
     (let* ((rect (rectangle self)))
       (when (point-in-rectangle-p  where (second rect) (first rect) (fourth rect) (third rect))
         self))
     (let (rep)
       (loop for item in (inside self)
             while (not rep) do
             (setf rep (click-in-obj item type where view)))
       rep)))

(defmethod click-in-obj ((self grap-group) type where view)
   (if (subtypep  (type-of self) type)
     (let* ((rect (rectangle self)) rep)
       (loop for item in (inside self)
             while (not rep) do
             (setf rep (click-in-obj item type where view)))
       (or rep (when (point-in-rectangle-p where (second rect) (first rect) (fourth rect) (third rect))
                 self)))
     (call-next-method)))

(defmethod click-in-obj ((self grap-note) type where view)
  (if (subtypep (type-of self) type)
       (let* ((rect (rectangle self)))
       (when (point-in-rectangle-p where (second rect) (first rect) (fourth rect) (third rect))
         self))))

(defmethod click-in-obj ((self grap-rest) type where view)
   (if (or (equal type 'grap-chord)
           (equal type 'grap-note))
     (let* ((rect (rectangle self)))
       (when (point-in-rectangle-p where (second rect) (first rect) (fourth rect) (third rect))
         self))))

(defmethod click-in-obj ((self grap-rest) type where view)
   (when (equal type 'grap-chord)
     (let* ((rect (rectangle self)))
       (when (point-in-rectangle-p where (second rect) (first rect) (fourth rect) (third rect))
         self))))

;-----------------------
(defmethod click-in-extra-p ((self grap-container) where)
   (let (rep)
     (loop for item in (extras self)
           while (not rep) do
           (let ((rect (rectangle item)))
             (when (point-in-rectangle-p where (second rect) (first rect) (fourth rect) (third rect))
               (setf rep item))))
     (if rep rep
         (progn
           (loop for item in (inside self)
                 while (not rep) do
               (setf rep (click-in-extra-p item where)))
           rep))))

(defmethod click-in-extra-p ((self simple-graph-container) where)
   (let (rep)
     (loop for item in (extras self)
           while (not rep) do
           (let ((rect (rectangle item)))
             (when (point-in-rectangle-p where (second rect) (first rect) (fourth rect) (third rect))
               (setf rep item))))
     rep))

;--------------------------------------------------------


(defmethod get-graph-type-obj ((self grap-container) type)
   "Get the list of selected objects in self"
   (if (subtypep (type-of self) type) (list self)
       (loop for item in (inside self)
                 append (get-graph-type-obj item type))))

(defmethod get-graph-type-obj ((self grap-group) (type (eql 'grap-group)))
   (cons self (loop for item in (inside self)
                    append (get-graph-type-obj item type))))

(defmethod get-graph-type-obj ((self t) type )
   (if (subtypep (type-of self) type) (list self)))

(defmethod get-graph-type-obj ((self grap-rest) (type (eql 'grap-chord)))
   (list self))

;==================
;get the list of selected graphic objects
(defmethod select-grap-objs ((self grap-container) type objlist)
   (if (subtypep (type-of self) type) (list self)
   (if (subtypep (type-of (car (inside self))) type)
     (loop for item in (inside self)
           when (member (reference item) objlist :test 'equal) collect item)
     (loop for item in (inside self)
           append (select-grap-objs item type objlist)))))

(defmethod select-grap-objs ((self grap-group) (type (eql 'grap-group)) objlist)
   (if (member (reference self) objlist :test 'equal)
     (list self)
     (loop for item in (inside self)
           when (equal (type-of item) type) append (select-grap-objs item type objlist))))

(defmethod select-grap-objs ((self grap-note) type objlist)
   (when (member (reference self) objlist :test 'equal) (list self)))

(defmethod select-grap-objs ((self t) type objlist) 
   (when (member (reference self) objlist :test 'equal) (list self)))


;==================
;get correspond grap-object
(defmethod get-correspond-grap ((self grap-container) obj)
   (if (equal (reference self) obj) self
       (let (rep)
         (loop for item in (inside self)
               while (not rep) do
               (setf rep (get-correspond-grap item obj)))
         rep)))

(defmethod get-correspond-grap ((self t) obj) 
   (when (equal (reference self) obj) self))

  
;===========FOR PRINT

#|
;X
(defmethod how-many-pgh ((self t) sizex)
   (declare (ignore sizex)) 1)

(defmethod how-many-pgh ((self grap-chord) sizex)
   (let* ((last (loop for note in (inside self)
                      maximize (third (rectangle note)))))
     (ceiling last sizex)))

(defmethod how-many-pgh ((self grap-chord-seq) sizex )
   (ceiling (third (rectangle (car (last (inside self))))) sizex))

(defmethod how-many-pgh ((self grap-multiseq) sizex )
   (loop for ch-seq in (inside self) maximize (how-many-pgh ch-seq sizex )))

(defmethod how-many-pgh ((self grap-voice) sizex )
   (ceiling (third (rectangle (car (last (inside self))))) sizex))

(defmethod how-many-pgh ((self grap-poly) sizex )
   (loop for voice in (inside self) do (how-many-pgh voice sizex )))


;Y
(defmethod how-many-pgv ((self t) sizey zoom size up widt sizeh)
   (declare (ignore sizey zoom size up widt sizeh)) 1)

(defmethod how-many-pgv ((self grap-chord-seq) sizey zoom size up widt sizeh)
   (let* ((lastchord (car (last (inside self))))
          (staffnumber (floor (* zoom (x lastchord)) widt)))
     (ceiling (+ (* staffnumber sizeh) sizeh up) sizey)))


(defmethod how-many-pgv ((self grap-multiseq) sizey zoom size up widt sizeh)
   (let* ((last (car (last (inside self)))))
     (how-many-pgv last sizey zoom size up widt sizeh)))
|#



;===========TOOLS  DONDE ??????
(defun add-zoom2control (control zoom &optional position)
  (setf zoom (round (* zoom 100)))
  (om-add-subviews control 
                   (om-make-dialog-item 'numbox (or position (om-make-point 100 5)) (om-make-point 32 12) (format nil "~D" zoom)
                                        :di-action (om-dialog-item-act item
                                                              (change-editor-zoom (panel (om-view-container (om-view-container item))) (value item)))
                                        :font (om-make-font *om-def-font-face*  9)
                                        :value zoom
                                        :min-val 1
                                        :max-val 1000)))

;======================  ????? ENCORE

(defmethod first-figure ((self t)) nil)
(defmethod first-figure ((self simple-graph-container)) nil)
(defmethod first-figure ((self grap-rest)) self)
(defmethod first-figure ((self grap-chord)) self)
(defmethod first-figure ((self grap-container)) 
  (first-figure (car (inside self))))

(defmethod last-figure ((self t)) nil)
(defmethod last-figure ((self simple-graph-container)) nil)
(defmethod last-figure ((self grap-rest)) self)
(defmethod last-figure ((self grap-chord)) self)

#|
(defmethod last-figure ((self grap-container))
  (let ((max 0) rep)
    (loop for item in (inside self) do
          (let ((last (last-figure item)))
            (if (and last (> (offset->ms last self) max))
              (setf max (offset->ms last self)
                    rep last))))
    rep))
|#

(defmethod last-figure ((self grap-container)) 
  (last-figure (car (last (inside self)))))

(defmethod next-figure ((self t) &optional list)  nil)
(defmethod next-figure ((self simple-graph-container) &optional list)
  (if list (car list)
      (let ((brothers (and (parent self) (nthcdr (1+ (position self (inside (parent self)))) (inside (parent self))))))
        (cond
         (brothers (first-figure (car brothers)))
         (t (next-figure (parent self)))))))

(defmethod previous-figure ((self t))  nil)
(defmethod previous-figure ((self simple-graph-container)) 
  (let ((brothers (and (parent self) (reverse (subseq (inside (parent self))  0  (position self (inside (parent self))))))))
    (cond
     (brothers (last-figure (car brothers)))
     (t (previous-figure (parent self))))))


(defmethod cons-gchord&rest-list ((self t)) nil)
(defmethod cons-gchord&rest-list ((self grap-chord)) (list self))
(defmethod cons-gchord&rest-list ((self grap-rest)) (list self))
(defmethod cons-gchord&rest-list ((self grap-container)) 
   (loop for item in (inside self) append (cons-gchord&rest-list item)))



(defmethod click-in-grap-measure? ((self grap-voice) where)
   (let ((x (om-point-h where))
         (y (om-point-v where))rep)
     (loop for item in (inside self)
           while (not rep) do
           (let ((x0 (car (rectangle item)))
                 (x1 (third (rectangle item))))
             (when (and (> x x0) (< x x1) (> y (second (rectangle item))) (< y (fourth (rectangle item))))
               (setf rep item))))
     rep))


(defmethod click-in-grap-measure? ((self grap-poly) where)
   (let (rep)
     (loop for item in (inside self)
           while (not rep) do
           (setf rep (click-in-grap-measure? item where)))
     rep))


