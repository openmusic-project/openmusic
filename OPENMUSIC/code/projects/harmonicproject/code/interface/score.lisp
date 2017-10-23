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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================
;;; Harmonic project by J. Bresson, C. Truchet


(in-package :om)

;;;==== ACTIONS ====

;;; quand on bouge une un truc dans l'editeur
(defmethod transpose-a :after ((self tonal-object) trans)
  (actualise-tonalite self))

;;; pour les notes on reinitialise leur tonalite
(defmethod transpose-a :before ((self note) trans)
  (set-tonalite self nil)
  (actualise-tonalite self)
  )

(defmethod transpose-a :after ((self note) trans)
  (when (and (parent self) (get-tonalite (parent self)))
    (set-degre-accord (parent self))
    ))

(defmethod general-delete :after ((view t) (self note))
  (when (and (parent self) (get-tonalite (parent self)) (inside (parent self)))
    (set-degre-accord (parent self)))
  )
  

;;; adapter l'armure de l'editeur a la tonalite de l'objet
(defmethod set-editor-tonality ((self scorePanel))
  (let ((tonobj (object (editor self)))
        (staff (staff-sys self)))
    (when (and tonobj staff)
      (setf (armure staff) (get-armure-from-tonalite tonobj))
      )))

(defmethod set-editor-tonality ((self notePanel)) nil)

(defmethod set-system-tonality ((self OMsystem) object)
  (setf (armure self) (get-armure-from-tonalite object)))

(defmethod set-editor-tonality ((self multiseqPanel))
  (let ((tonobj (object (om-view-container self)))
        (staffsys (staff-sys self)))
    (when (and tonobj staffsys)
      (loop for staff in staffsys do
            (setf (armure staff) (get-armure-from-tonalite tonobj)))
      )))



;;;==== TOOLS ====

(defun arabe->romain (n)
  (cond 
   ((listp n) (arabe->romain (car n)))
   ((zerop n) "0")
   ((= n 1) "I")
   ((= n 2) "II")
   ((= n 3) "III")
   ((= n 4) "IV")
   ((= n 5) "V")
   ((= n 6) "VI")
   ((= n 7) "VII")
   ((= n 8) "VIII")
   ((= n 9) "IV")
   ((= n 10) "X")
   ((= n 11) "XI")
   ))

(defmethod tonalite-to-string ((self tonalite))
  (string+ (get-note-name (tonnote self)) "" 
           (cond 
            ((equal (tonalt self) 'bemol) "b")
            ((equal (tonalt self) 'diese) "#")
            (t ""))
           (if (equal *majeur* (mode self)) "M" "m")
           ))




;;;=======================================================================
;;; ALTERATIONS ET ENHARMONIES DES NOTES


(defun alt-2-str (alt)
  (let ((str ""))
    (if (consp alt)
      (if (equal alt '(diese diese)) (setf str (db-diese))
          (loop for item in alt do (setf str (string+ str (alt-2-str item)))))
      (setf str (cond 
                 ((equal alt 'bemol) (string (bemol)))
                 ((equal alt 'diese) (string (diese)))
                 ((equal alt 'becarre) (string (beca)))
                 (t ""))))
    str))

;;; renvoie l'alteration correspondant a une note etant donnee l'armure
;;; ses valeurs de tonalite.. 
(defmethod get-special-alterat ((self note) armure)
  (when (tonalite self)
    (alt-2-str (tonalt (tonalite self)))))      
    
;;; decalage graphique de l'alteration 
;;; au cas ou il y a plusieurs symboles
(defmethod correct-alteration ((self grap-note) alteration)
  (let ((ton (tonalite (reference self))))
    (- alteration (if (and ton (consp (tonalt ton))) (- (length (tonalt ton)) 1) 0))
    ))

(defmethod natural-alt-char ((self grap-note))
  (if (and (tonalite (reference self)) (tonalt (tonalite (reference self))))
    nil
    (alt-char self)))

;;; calcul la hauteur "graphique", 
;;; correspondant a la ligne sur laquelle on doit dessiner la note sur la portee
(defmethod get-graphic-pos ((self note) top linespace scale)
  (if (tonalite self) 
    (let ((midix (midic self)))
      (setf midix (+ midix 
                     (loop for alt in (list! (tonalt (tonalite self)))
                           sum (cond ((equal 'diese alt) -100) ((equal 'bemol alt) 100) (t 0)))))
      (midi2pixel midix top linespace *2-tone-chromatic-scale*))
  (midi2pixel (midic self) top linespace scale)))

;;;=======================================================================



;;;==== DRAW VARIABLES ====

(defvar *tonalite-show-degres-acc* t)
(defvar *tonalite-show-chiffrages-acc* t)
(defvar *tonalite-show-accident-notes* nil)
(defvar *tonalite-show-modulations* t)
(defvar *tonalite-show-tonalite* t)
(defvar *tonalite-show-cadences* nil)

(defvar *tonalite-show-ornement-notes* nil)
;;(setf *tonalite-show-ornement-notes* t)

(defun tonalite-show-variables-dialog ()
  (let ((win (om-make-window 'om-dialog :position :centered :size (om-make-point 200 290) :window-title ""
                             :resizable nil :maximize nil :minimize nil
                             :bg-color *om-white-color*))
        (d (om-make-dialog-item 'om-check-box (om-make-point 30 50) (om-make-point 150 24) "degrees" :checked-p *tonalite-show-degres-acc*))
        (c (om-make-dialog-item 'om-check-box (om-make-point 30 80) (om-make-point 150 24) "figuring" :checked-p *tonalite-show-chiffrages-acc*))
        (cad (om-make-dialog-item 'om-check-box (om-make-point 30 110) (om-make-point 150 24) "cadences" :checked-p *tonalite-show-cadences*))
        (m (om-make-dialog-item 'om-check-box (om-make-point 30 140) (om-make-point 150 24) "modulations" :checked-p *tonalite-show-modulations*))
        (a (om-make-dialog-item 'om-check-box (om-make-point 30 170) (om-make-point 150 24) "accidentals" :checked-p *tonalite-show-accident-notes*))
        (to (om-make-dialog-item 'om-check-box (om-make-point 30 200) (om-make-point 150 24) "general tonality" :checked-p *tonalite-show-tonalite*)))
    (om-add-subviews win
                     (om-make-dialog-item 'om-static-text (om-make-point 30 20) (om-make-point 150 24) "Display" :font *om-default-font2b*)
                     d c cad m a to
                     (om-make-dialog-item 'om-button (om-make-point  20 240) (om-make-point 70 24) "OK"
                                          :di-action (om-dialog-item-act item 
                                                       (setf *tonalite-show-degres-acc* (om-checked-p d))
                                                       (setf *tonalite-show-chiffrages-acc* (om-checked-p c))
                                                       (setf *tonalite-show-cadences* (om-checked-p cad))
                                                       (setf *tonalite-show-modulations* (om-checked-p m))
                                                       (setf *tonalite-show-accident-notes* (om-checked-p a))
                                                       (setf *tonalite-show-tonalite* (om-checked-p to))
                                                       (om-return-from-modal-dialog win t)))
                     
                     (om-make-dialog-item 'om-button (om-make-point  110 240) (om-make-point 70 24) "Cancel"
                                           :di-action (om-dialog-item-act item 
                                                       (om-return-from-modal-dialog win nil)))
                     )
    (om-modal-dialog win)
    ))
        




;;;=======================
;;;==== DRAW TONALITE ====
;;;=======================


;;;==================================
;;; TONALITE GENERALE

(defmethod draw-armure ((self OMStaff) armure x top xsize fontsize deltay)
  (when (and armure (key-obj self))
    (let* ((linespace (/ fontsize (- (numlines self) 1)))
           (mytop (line2pixel (posy self) top linespace))
           (char (first armure))
           (thelist (second armure))
           (deltak (if (equal #\? (key (key-obj self))) 10 8)))
      (om-with-font (get-font-to-draw 1)
                    ;;;(setf char (if (equal char 'b) (bemol) (diese)))
                    (loop for k in thelist
                          for i = 0 then (+ i 1) do
                          (om-draw-char (round (+ x (* i linespace) 15 fontsize)) (round (+  deltay (* (* -1 (- k deltak)) (/ linespace 2)) mytop))
                                        char))
                    ))))


(defmethod draw-general-tonality ((self scorepanel)) 
  (when *tonalite-show-tonalite*
    (let* ((obj (object (editor self)))
           (size (staff-size self))
           (deltay (* size (score-top-margin self)))
           (system (staff-sys self))
           (start deltay)
           )
      (when (tonalite obj)
        (om-with-focused-view self
          (om-with-font (om-make-font  *om-score-font-face* (round (staff-size self) 2) :style '(:bold))
                        (om-draw-string (+ (om-point-h (om-scroll-position self)) 5) 
                                        (max size (round (- start (* 2 size))))
                                        (tonalite-to-string (tonalite obj)))))
        ))))


(defmethod draw-general-tonality ((self notepanel)) nil)

;;;==================================
;;; NOTES : ACCIDENTS


;;; Redefinition de methode de scoretools

(defmethod get-mus-color ((self note))
  (cond ((and *tonalite-show-accident-notes* (tonal-values self) (accident (tonal-values self)))
         *accident-color*)
        ((and *tonalite-show-ornement-notes* (tonal-values self) (ornement (tonal-values self)))
         *ornement-color*)
        (t (mus-color self))))


;;;==================================
;;; ACCORDS : DEGRE ET CHIFFRAGE

;;; pas appelee : on ecrit le degre des accords
(defmethod draw-degre ((self grap-note) realpos y size)
  (when *tonalite-show-degres-acc*
    (let* ((note (reference self))
           (tonalval (tonal-values note))
           ;(ypos (+ (+ y (y self)) (round size)))
           (ypos (max (+ y (* 6 (round size))) (+ (+ y (y self)) (round size))))
           degre 
         fonte)
      (when tonalval
        (setf degre (get-degre note))
      (setf fonte (om-make-font "Arial" (max 4 (round size 2))))
      (when (and degre (not (ornement tonalval)) (not  (accident tonalval)))
        (om-with-font fonte
                      (om-draw-string realpos ypos (arabe->romain degre))))))))

(defmethod draw-degre ((self grap-chord) realpos y size)
  (when *tonalite-show-degres-acc*
    (let* ((chord (reference self))
           (tonalval (tonal-values (car (inside chord))))
           (thenotes (copy-list (inside self)))
           (thenotes (sort thenotes '> :key 'y))
           (y-min (y (car thenotes)))
           (ypos (max (+ y (* 8 (round size))) (+ y-min (* 2 (round size)))))
         ;(ypos (max (+ y (* 6 (round size))) (+ (+ y (y self)) (round size))))
           degre 
           fonte)
      (when tonalval
        (setf degre (degre tonalval))
        (setf fonte (om-make-font *om-score-font-face* (max 4 (round size 2))))
        (when (and degre (not (ornement tonalval)) (not  (accident tonalval)))
        (om-with-font fonte
                      (om-draw-string realpos ypos (arabe->romain degre))))))))


(defmethod draw-chiffrage ((self grap-chord) x y zoom size)
  (when (tonal-values (reference self))
    (let* ((accord (reference self))
           (chiff (cadr (get-degre accord)))
           (deg (car (get-degre accord)))
           (cad (cadence (tonal-values accord)))
           (xpos (round (+ x (* zoom (x self)))))
           ;(ypos (+ (y self) y (round size)))
           (thenotes (copy-list (inside self)))
           (thenotes (sort thenotes '> :key 'y))
           (y-min (y (car thenotes)))
           (ypos (max (+ y (* 6 (round size))) (+ y-min (* 2 (round size)))))
           )
      (when (and deg *tonalite-show-chiffrages-acc*) 
        (let ((n (length chiff))
              (x 0) (y 0) (inc (round size 2)))
          (loop for k from 0 to (- n 1) do
                (let ((char (elt chiff k)))
                  (cond ((equal #\Space char) (incf y inc) (setf x 0))
                        ((equal #\/ char) (om-draw-char xpos (+ ypos y (round inc 1.4)) (code-char 111)))
                        ((equal #\+ char) (om-with-font  (om-make-font *om-score-font-face* (round size 1.6)) (om-draw-char (- xpos (round inc 1.5)) (+ ypos y (round inc 2)) char)))
                        (t (om-with-font (om-make-music-font *signs-font* (round size 1.8)) (om-draw-char (+ xpos x) (+ ypos y) (elt chiff k)) (incf x inc)))
                        )))))
      
      (om-with-font (om-make-font *om-score-font-face* (round size 2.5))
                    
                    (when (and deg *tonalite-show-degres-acc*) 
                      (om-draw-string xpos (+ ypos (* size 2)) (arabe->romain deg)))
                    
                    (when (and cad *tonalite-show-cadences*)
                      (om-draw-string xpos (+ ypos (* size 3)) (num2string cad)))
                    )
      )
    ))



;;;==================================
;;; ACCORDS : TONALITE ET MODULATIONS

(defmethod get-y-self ((self grap-chord) &optional (size 0) (zoom 1)) 
  (max size (round (* size 2))))

(defmethod get-x-self ((self grap-chord) &optional (size 0) (zoom 1)) (* zoom (x self)))

(defmethod get-y-self ((self grap-ryth-chord) &optional (size 0) (zoom 1))
  (max size (round (* size 2))))

(defmethod get-x-self ((self grap-ryth-chord) &optional (size 0) (zoom 1)) (+ size (* zoom (x self))))


(defmethod draw-tonalite ((self grap-chord) x y zoom size begin panel)
  (let* ((xpos (round (+ x (get-x-self self size zoom))))
         (deltay (* size (score-top-margin panel)))
         (ypos (- deltay -2 (get-y-self self size zoom)))
         (obj (reference self))
         (tonalite (get-tonalite obj)))
    (om-draw-line (- xpos (round size 2)) ypos (+ (round size 1.6) xpos) ypos)
    (if begin
        
      (om-with-font (om-make-font *om-score-font-face* (round size 2.5))
                    (om-draw-line (- xpos (round size 2)) ypos (- xpos (round size 2)) (+ ypos (round size 2.2)))
                    (om-draw-line (- xpos (round size 2) 1) ypos (- xpos (round size 2) 1) (+ ypos (round size 2.2)))
                    (om-draw-string (- xpos (round size 2) -4) (+ ypos (round size 2)) 
                                    (tonalite-to-string tonalite)
                                    ))
      (progn
        (om-draw-line (+ xpos (round size 1.6)) ypos (+ xpos (round size 1.6)) (+ ypos (round size 2.2)))
        (om-draw-line (+ xpos (round size 1.6) 1) ypos (+ xpos (round size 1.6) 1) (+ ypos (round size 2.2))))
      )))


(defmethod draw-modulation ((self grap-container) (newchord grap-chord) oldchord x y zoom size panel)
  (when *tonalite-show-modulations*
    (let ((container (reference self))
          (chord-apres (reference newchord))
          (chord-avant (if oldchord (reference oldchord) nil)))
      
      ;(when (and (get-tonalite container) (get-tonalite chord-apres))
      (if (get-tonalite chord-apres)
        
        (if (not chord-avant)
          ;;; c'est le premier accord on le dessine si il est dans une autre tonalite
            (when 
                ;(or (not (get-tonalite container))
                    (not (meme-tonalite? (get-tonalite chord-apres) (get-tonalite container)))
                    ;)
              (draw-tonalite newchord x y zoom size t panel))
            
            ;;; sinon on compare avec le precedent pour voir si on le suit ou si on change
            (when (not (meme-tonalite? (get-tonalite chord-apres) (get-tonalite chord-avant)))
              ;;; -> changement de tonalite
            (unless (or (not (get-tonalite chord-avant)) (meme-tonalite? (get-tonalite chord-avant) (get-tonalite container)))
              (draw-tonalite oldchord x y zoom size nil panel))
            (unless (meme-tonalite? (get-tonalite chord-apres) (get-tonalite container))
              (draw-tonalite newchord x y zoom size t panel))
            )
          )
        (when (and chord-avant (get-tonalite chord-avant))
              (draw-tonalite oldchord x y zoom size nil panel))
       )
      ))
  newchord)


(defmethod draw-modulation ((self grap-container) (newchord grap-group) oldchord x y zoom size panel)
  (when *tonalite-show-modulations*
    (let ((previous oldchord)
          (new nil))
      (loop for gr-chord in (inside newchord) do
            (setf new gr-chord)
            (draw-modulation self new previous x y zoom size panel)
            (setf previous new))
      new)))



;=======================================================
;Tonalite pane for preferences 225
;======================================================

(defclass tonaloptions-view (om-view) 
  ((object :initform nil :initarg :object :accessor object)))

(defmethod om-component-border ((self tonaloptions-view)) :line)

(defmethod initialize-instance :after ((self tonaloptions-view) &rest initargs)
  (let ((tonal-list (get-pref (object self) :tonal-options))
	(textdims (om-make-point 150 24)))
    (om-add-subviews self
                     (om-make-dialog-item 'om-check-box (om-make-point 10 5) textdims "General Tonality" 
                                          :checked-p (nth 0 tonal-list)
                                          :di-action (om-dialog-item-act item 
                                                       (let ((list (get-pref (object self) :tonal-options)))
                                                         (setf (nth 0 list) (om-checked-p item))
                                                         (set-pref (object self) :tonal-options list))))
                     (om-make-dialog-item 'om-check-box (om-make-point 10 25) textdims "Modulations" 
                                          :checked-p (nth 1 tonal-list)
                                          :di-action (om-dialog-item-act item 
                                                       (let ((list (get-pref (object self) :tonal-options)))
                                                         (setf (nth 1 list) (om-checked-p item))
                                                         (set-pref (object self) :tonal-options list))))
                     (om-make-dialog-item 'om-check-box (om-make-point 10 45) textdims "Accidentals" 
                                          :checked-p (nth 2 tonal-list)
                                          :di-action (om-dialog-item-act item 
                                                       (let ((list (get-pref (object self) :tonal-options)))
                                                         (setf (nth 2 list) (om-checked-p item))
                                                         (set-pref (object self) :tonal-options list))))
                     (om-make-dialog-item 'om-check-box (om-make-point 10 65) textdims "Degrees" 
                                          :checked-p (nth 3 tonal-list)
                                          :di-action (om-dialog-item-act item 
                                                       (let ((list (get-pref (object self) :tonal-options)))
                                                         (setf (nth 3 list) (om-checked-p item))
                                                         (set-pref (object self) :tonal-options list))))
                     (om-make-dialog-item 'om-check-box (om-make-point 10 85) textdims "Figuring" 
                                          :checked-p (nth 4 tonal-list)
                                          :di-action (om-dialog-item-act item 
                                                       (let ((list (get-pref (object self) :tonal-options)))
                                                         (setf (nth 4 list) (om-checked-p item))
                                                         (set-pref (object self) :tonal-options list))))
                     (om-make-dialog-item 'om-check-box (om-make-point 10 105) textdims "Cadences" 
                                          :checked-p (nth 5 tonal-list)
                                          :di-action (om-dialog-item-act item 
                                                       (let ((list (get-pref (object self) :tonal-options)))
                                                         (setf (nth 5 list) (om-checked-p item))
                                                         (set-pref (object self) :tonal-options list))))
                     )))

;; (general modulations accidents degrees numbering cadencies)
(defun tonal-defaults () 
  (list t t nil nil nil nil))

(defun put-tonal-prefs (prefs)
  (setf *tonalite-show-tonalite* (nth 0 prefs)
        *tonalite-show-modulations* (nth 1 prefs)
        *tonalite-show-accident-notes* (nth 2 prefs)
        *tonalite-show-degres-acc* (nth 3 prefs)
        *tonalite-show-chiffrages-acc* (nth 4 prefs)
        *tonalite-show-cadences* (nth 5 prefs)))

;;;;=================================
