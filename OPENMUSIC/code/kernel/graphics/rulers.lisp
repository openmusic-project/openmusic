;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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

;DocFile
;This File implements the class ruler and an abstract class for view containing rulers.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defclass ruler (om-view) 
   ((axe :initform nil :initarg :axe :accessor axe)
    (zoom :initform 1 :initarg :zoom :accessor zoom)
    (minzoom :initform 0.1 :initarg :minzoom :accessor minzoom)
    (hide-ruler? :initform nil :initarg :hide-ruler? :accessor hide-ruler?)
    (ruler-selection? :initform nil :initarg :ruler-selection? :accessor ruler-selection?)
    (distrib :initform 10 :initarg :distrib :accessor distrib)
    (assoc-view :initform nil :initarg :assoc-view :accessor assoc-view))
   (:documentation "This class implements all rulers bars in Open Music.#enddoc#
#seealso# (static-ruler maq-ruler ruler-metric) #seealso#
#axe# X if the ruler is horizontal or Z if the ruler is vertical.#axe#
#zoom# This slot is used to determinate the units of the ruler, if zoom = 0 the unit
is 10^0, so 1, if zoom = 1 the unit is 10^1 10 , etc.#zoom#
#minzoom# This slot is the minimum zoom allowed in the ruler,
for exemple 0.01 implic that the centieme is the smallest sub division of the unit.#minzoom#
#hide-ruler?# T if the ruler is not showed, the default value is NIL.#hide-ruler?#
#ruler-selection?# If non NIL it slot contains a list (min max) representing a selcted interval.#ruler-selection?#
#distrib# Generally it slot is 10 so decimal mode but you can use any subdivision#distrib#
#assoc-view# This slot contains de view associated to this ruler.#assoc-view#")
   (:default-initargs #+win32 :draw-with-buffer #+win32 t))


(defmethod om-view-cursor ((self ruler))
   (if (equal (axe self) 'x)
       *om-horiz-size-cursor*
     *om-verti-size-cursor*
     ))
    

(defmethod om-draw-contents  ((self ruler))
  (unless (hide-ruler? self)
    (om-with-pen (self :size 1)
      (call-next-method)
      (om-with-focused-view self
        (om-with-fg-color self *om-dark-gray-color*
          
             (om-draw-view-outline self 1)
             (let* ((axe (axe self))
                    (range (if (equal axe 'x) (rangex (assoc-view-singleton self))
                             (rangey (assoc-view-singleton self)))))
               (draw-one-ruler self axe range (ruleroffsetx-from-editor (om-view-container self)) 
                               (ruleroffsety-from-editor (om-view-container self)))
               ))))))

(defmethod ruleroffsetx-from-editor ((self t)) 0)
(defmethod ruleroffsety-from-editor ((self t)) 0)

;(defparameter *rulerx-font* (om-make-font "verdana" #+cocoa 10 #-cocoa 8))
(defparameter *rulerx-font* (om-make-font "verdana"  10))

;(defparameter *rulery-font* (om-make-font "verdana" #+cocoa 8 #-cocoa 6))
(defparameter *rulery-font* (om-make-font "verdana" 8))

(defmethod assoc-view-list ((self ruler))
   (list! (assoc-view self)))

(defmethod assoc-view-singleton ((self ruler))
   (car (list! (assoc-view self))))

;=====================================

(defmethod draw-one-ruler  ((self ruler) (axe (eql 'x)) range offx offy)
   "The second argument selects the method used to draw the ruler
this method draw a vertical ruler, the argument RANGE is a list (minval maxval)"
   (unless (= (car range) (cadr range))
     (let* ((i (first range))
            (j (second range))
            (unit (zoom self))
            (ppu (norme2pixel (assoc-view-singleton self) axe unit))
            (system-etat (get-system-etat (assoc-view-singleton self)))
            (stry 17))
       (setf i (+ i (- unit (mod i unit))))
       (om-with-font *rulerx-font*
       (loop while (< i j) do
             (let* ((pixel (om-subtract-points 
                            (point2pixel (assoc-view-singleton self) (om-make-big-point i 0) system-etat)
                            (om-scroll-position (assoc-view-singleton self)))))
               (cond
                ((and  (> (* 10 ppu) 10) (zerop (mod i (* 10 unit))))
                 (om-draw-line (+ offx  (om-point-h pixel)) offy (+ offx  (om-point-h pixel)) (+ offy 8))
                 (om-draw-string (+ offx (-  (om-point-h pixel) 7)) (+ offy stry) (get-string-nom (assoc-view-singleton self) i axe)))
                ((>= ppu  20)
                 (om-draw-string (+ offx (-  (om-point-h pixel) 7)) (+ offy stry) (get-string-nom (assoc-view-singleton self) i axe))
                 (om-draw-line (+ offx  (om-point-h pixel)) offy (+ offx  (om-point-h pixel)) (+ offy 4)))
                ((>= ppu 5)
                 (om-draw-line (+ offx  (om-point-h pixel)) offy (+ offx  (om-point-h pixel)) (+ offy 4))))
               (incf i unit)))
       (when (ruler-selection? self)
         (draw-selection self))))))


(defmethod draw-one-ruler  ((self ruler) (axe (eql 'y)) range offx offy)
   "The second argument selects the method used to draw the ruler
this method draw a horizontal ruler, the argument RANGE is a list (minval maxval)"
   (let* ((offx2 (- (w self) 25))
           (i (first range))
           (j (second range))
           (unit (zoom self))
           (ppu (norme2pixel (assoc-view-singleton self) axe unit))
           (system-etat (get-system-etat (assoc-view-singleton self)))
           (strydelta 3))
     (setf i (+ i (- unit (mod i unit))))
     (om-with-font *rulery-font*
     (loop while (< i j) do
           (let* ((pixel (point2pixel (assoc-view-singleton self) (om-make-big-point 0 i) system-etat)))
             (cond
              ((and (> (* 10 ppu) 10) (zerop (mod i (* 10 unit))))
               (om-draw-line (+ offx2 15) (+ offy (om-point-v pixel)) (w self) (+ offy (om-point-v pixel)))
               (om-draw-string (+ offx 2) (+ offy (- (om-point-v pixel) strydelta)) (get-string-nom (assoc-view-singleton self) i axe)))
              ((>= ppu 20)
               (om-draw-string (+ offx 2) (+ offy (- (om-point-v pixel) strydelta)) (get-string-nom (assoc-view-singleton self) i axe))
               (om-draw-line (+ offx2 20) (+ offy (om-point-v pixel)) (w self) (+ offy (om-point-v pixel))))
              ((>= ppu 5)
               (om-draw-line (+ offx2 20) (+ offy (om-point-v pixel)) (w self) (+ offy (om-point-v pixel)))))
             (incf i unit))))))

; by Jean Bresson
(defmethod ruler-print-draw-grille ((self ruler) (axe (eql 'y)) offx offy system-etat)
  (let* ((view (assoc-view self))
          (i (first (rangey view)))
          (unit (zoom self))
          (ppu (norme2pixel view 'y unit))
          (j (second (rangey view))))
     
    ;(setf i (if (minusp i) (+ i (- unit (mod i unit))) (+ i (mod i unit))))
     (setf i (+ i (- unit (mod i unit))))
     (loop while (< i j) do
           (let* ((pixel (point2pixel view (om-make-big-point 0 i) system-etat)))
             (cond
              ((and (> (* 10 ppu) 10) (zerop (mod i (* 10 unit))))
               (draw-grille-liney view (+ offy (om-point-v pixel)) offx))
              ((>= ppu 20)
               (draw-grille-liney view (+ offy (om-point-v pixel)) offx)))
             (incf i unit)))))
;idem
(defmethod ruler-print-draw-grille ((self ruler) (axe (eql 'x)) offx offy system-etat)
  (let* ((view (assoc-view self))
          (i (first (rangex view)))
          (unit (zoom self))
          (ppu (norme2pixel view 'x unit))
          (j (second (rangex view))))
     ;(setf i (if (minusp i) (+ i (- unit (mod i unit))) (+ i (mod i unit))))
     (setf i (+ i (- unit (mod i unit))))
     (loop while (< i j) do
           (let* ((pixel (point2pixel view (om-make-big-point i 0) system-etat)))
             (cond
              ((and (> (* 10 ppu) 10) (zerop (mod i (* 10 unit))))
               (draw-grille-linex view (+ offy (om-point-h pixel)) offx))
              ((>= ppu 20)
               (draw-grille-linex view (+ offy (om-point-h pixel)) offx)))
             (incf i unit)))))


(defvar *ruler-last-click* nil)

(defmethod om-view-click-handler  ((self ruler) where)
    (reset-selection self)
    (setf *ruler-last-click* where)
    (om-init-motion-functions self 'strech-ruler-motion 'strech-ruler-release))

(defmethod strech-ruler-motion ((self ruler) pos) 
  (let* ((axe (axe self))
         (editor (assoc-view-singleton self))
         (oldrange (if (eql axe 'x) (second (rangex editor))
                     (second (rangey editor))))
         (delta  (if (eql axe 'x) 
                     (- (om-point-h *ruler-last-click*) (om-point-h pos))
                   (-  (om-point-v pos) (om-point-v *ruler-last-click*))))
         (ppu (norme2pixel editor axe (zoom self)))
         (deltapoint (pixel2norme editor axe delta)))
    (unless (and (minusp delta) nil
                 (= (zoom self) (minzoom self)) 
                 (> ppu 150))
      (loop for item in (assoc-view-list self) do
            (if (eql axe 'x)
                (setf (second (rangex item)) (+ oldrange deltapoint))
              (setf (second (rangey item)) (+ oldrange deltapoint)))
            (set-units-ruler item self))
      (om-redraw-view self)
      (setf *ruler-last-click* pos))))

(defmethod strech-ruler-release ((view ruler) pos)
  (om-invalidate-view view)
  (loop for item in (assoc-view-list view) do
        (update-view-of-ruler item)))

(defmethod reset-selection ((self ruler))
   (when (ruler-selection? self)
     (setf (ruler-selection? self) nil)
     (om-invalidate-view self t)))

;(defmethod corrige-selection ((self ruler) editor)
;   (when (ruler-selection? self)
;    (setf (ruler-selection? self)
;           (list (max 0 (first (ruler-selection? self)))
;                 (min (get-obj-dur (car (get-obj-to-play editor)))
;                      (second (ruler-selection? self)))))))

(defmethod draw-selection ((self ruler))  ;on peut l'effacer je pense!
  (let* ((editor (assoc-view-singleton self))
         (sys-etat (get-system-etat editor)))
    (om-with-hilite-foreground
     (let* ((x  (om-point-h (point2pixel editor 
                                         (om-make-big-point (first (ruler-selection? self)) 0) sys-etat)))
            (x1  (om-point-h (point2pixel editor 
                                          (om-make-big-point (second (ruler-selection? self)) 0) sys-etat))))
       (om-draw-rect  x 0 (- x1 x) (h self)))
     )
    ))

;-------------------------------------------
;static rulers
;-------------------------------------------


(omg-defclass static-ruler (ruler) () )

(defmethod strech-ruler-motion ((self static-ruler) pos) t)
(defmethod strech-ruler-release ((view static-ruler) pos) t)
(defmethod om-view-cursor ((self static-ruler)) nil)

;-------------------------------------------
;metric rulers
;-------------------------------------------

(omg-defclass ruler-metric (ruler) 
   ((tempo :initform '(60 4) :initarg :tempo :accessor tempo)
    (maxsub :initform 16 :initarg :maxsub :accessor maxsub)
    (loop-mes-p :initform t  :initarg :loop-mes-p :accessor loop-mes-p)
    (meslist :initform '((4 4) (3 4)) :initarg :meslist :accessor meslist)))

(defun size-pulsation (tempo measure)
   (let ((ref (second tempo)))
     (/ (* (first tempo) (/ 60 (second measure))) ref)))

(defun size-measure (tempo measure)
   (* (car measure) (size-pulsation tempo measure)))

(defun get-ieme-measure (i meslist loop-p)
   (let ((len (length meslist)))
     (cond
      ((null loop-p) (if (< i (- len 1))
                       (nth i meslist) (car (last meslist))))
      (t  (nth (mod i len)  meslist)))))
  
(defmethod get-list-mes-dur ((self ruler-metric))
   (loop for mes in (meslist self)
         sum (* (first mes) (size-pulsation (tempo self) mes))))
  
(defmethod get-beat-from-val ((self ruler-metric) val)
   (let* ((tempo (tempo self))
          (listmes (meslist self))
          (currentval 0) (loop-p (loop-mes-p self))
          (i 0) rep)
     (when (or (= val 0) (< val (size-pulsation tempo (first listmes))))
       (setf rep '(0 0)))
     (loop while (not rep) do
           (let* ((themeasure (get-ieme-measure i listmes loop-p))
                  (mespulsation (size-pulsation tempo themeasure))
                  (lastval (+ currentval (* 1000 (first themeasure) mespulsation))))
             (unless (< lastval val)
               (loop for j from 1 to (first themeasure)
                     while (not rep) do
                     (unless (< (+ currentval (* 1000 j mespulsation)) val)
                       (setf rep (list i j)))))
             (setf currentval  (+ currentval (* 1000 (first themeasure) mespulsation)))
             (incf i)))
     rep))


(defmethod ms2listmetric (val tempo listmes quantum loop-p)
   (let* ((currentval 0) (i 0) rep)
     (when (or (= val 0) (< val (* 1000 (size-pulsation tempo (first listmes)))))
       (setf rep '(1 1 0)))
     (list (size-pulsation tempo (first listmes)) rep)
     (loop while (not rep) do
           (let* ((themeasure (get-ieme-measure i listmes loop-p))
                  (mespulsation (size-pulsation tempo themeasure))
                  (lastval (+ currentval (* 1000 (first themeasure) mespulsation)))
                  (simbmeasure (+ i 1)))
             (cond 
              ((= lastval val)
               (setf rep (list (+ simbmeasure 1) 1 0)))
              ((> lastval val)
               (loop for j from 0 to (- (first themeasure) 1)
                     while (not rep) do
                     (unless (< (+ currentval (* 1000 (+ j 1) mespulsation)) val)
                       (setf rep (list simbmeasure (+ j 1) (ms2u (abs (- val (+ currentval (* 1000 j mespulsation)) )) tempo quantum))))))
              (t (setf currentval  (+ currentval (* 1000 (first themeasure) mespulsation)))
                 (incf i)))))
     rep))

(defmethod listmetric2ms (list tempo listmes quantum loop-p)
   (let* ((j 0) (acum 0) 
          (lastmeasure (get-ieme-measure 0 listmes loop-p)))
     (loop for i from 0 to (- (car list) 2) do
           (incf j)
           (setf lastmeasure (get-ieme-measure i listmes loop-p))
           (setf acum (+ acum (size-measure tempo lastmeasure))))
     (setf lastmeasure (get-ieme-measure j listmes loop-p))
     (setf acum (* 1000 (+ acum (* (- (second list) 1) (size-pulsation tempo lastmeasure)))))
     (setf acum (+ acum (u2ms (third list) tempo quantum)))
     (round acum)))

;------------------
(defmethod durms2listmetric (val0 val tempo listmes quantum loop-p)
   (let* ((lm0 (ms2listmetric val0 tempo listmes quantum loop-p))
          (lm (ms2listmetric (+ val0 val) tempo listmes quantum loop-p))
          hm_measures)
     (setf hm_measures (- (car lm) (car lm0)))
     (if (zerop hm_measures) 
       (cons hm_measures (om- (cdr lm) (cdr lm0)))
       (cons hm_measures (cdr lm)))))
  
;(durms2listmetric 6000 2500 '(4 60) '((4 4) (1 4)) 16 t)
;(ms2listmetric 8500 '(4 60) '((4 4) (4 4)) 8 t)

(defmethod listmetric2durms (list tempo listmes quantum loop-p)
   (let* ((acum 0) 
          (lastmeasure (get-ieme-measure 0 listmes loop-p)))
     (loop for i from 0 to (- (car list) 2) do
           (setf lastmeasure (get-ieme-measure i listmes loop-p))
           (setf acum (+ acum (size-measure tempo lastmeasure))))
     (setf acum (* 1000 (+ acum (* (- (second list) 1) (size-pulsation tempo lastmeasure)))))
     (setf acum (+ acum (u2ms (third list) tempo quantum)))
     acum))

;------------------
;(ms2listmetric 4500 '(4 60) '((4 4) (4 4)) 16 t)
;(listmetric2ms '(2 3 0) '(4 60) '((4 4) (5 8)) 16 t)

(defun simple-list-dur (meslist i tempo)
   (loop for j from 1 to i
         for mes in meslist
         sum (* (first mes) (size-pulsation tempo mes))))

(defmethod get-val-from-meas ((self ruler-metric) i listduration)
   (let* ((meslist (meslist self))
          (tempo (tempo self))
          (loop-p (loop-mes-p self))
          (len (length meslist)))
     (cond
      ((null loop-p) (if (< i (- len 1))
                       (simple-list-dur meslist i tempo)
                       (+ listduration (* (- i len) (* (first (car (last meslist))) 
                                                       (size-pulsation tempo (car (last meslist))))))))
      (t (if (< i (- len 1))
           (simple-list-dur meslist i tempo)
           (+ (simple-list-dur meslist (mod i len) tempo) (* (floor i len) listduration)))))))


(defmethod draw-one-ruler  ((self ruler-metric) (axe (eql 'x)) range offx offy)
  "Draw a metric ruler in a maquette"
  (declare (ignore offx offy))
  (flet ((div (n p) (/ (- n (mod n p)) p)))
    (let* ((i (first range))
           (j (second range))
           (listmes (meslist self))
           (tempo (tempo self))
           (loop-p (loop-mes-p self))
           (first-beat (get-beat-from-val self i))
           (last-beat (get-beat-from-val self j))
           (listduration (get-list-mes-dur self))
           (system-etat (get-system-etat (assoc-view-singleton self)))
           (mesdisplay 1)
           (ppmes (if (> (car last-beat) (car first-beat))
                      (div (w self) (- (car last-beat) (car first-beat)))
                    (w self)))
           (ppmes2 nil)
           (stry 16))
      (when (<= ppmes 10)
        (setf ppmes2 ppmes)
        (loop while (<= ppmes2 15) do 
              (incf mesdisplay)
              (setf ppmes2 (div (* mesdisplay (w self)) (- (car last-beat) (car first-beat))))
              ))
      (om-with-font *rulerx-font*
                    (loop for i from (first first-beat) to (first last-beat) 
                          when (equal 0 (mod i mesdisplay))
                          do
                          (let* ((themeasure (get-ieme-measure i listmes loop-p))
                                 (sizepul (* 1000 (size-pulsation tempo themeasure))) 
                                 (sec (* 1000 (get-val-from-meas self i listduration))))
                           
                            (loop for delta from 0 to (- (first themeasure) 1) do
                                  (let ((pixel (point2pixel (assoc-view-singleton self) (om-make-big-point (round (+ (* delta sizepul) sec)) 0) system-etat)))
                                    (if (= delta 0)
                                        (progn (om-draw-line (om-point-h pixel) 0  (om-point-h pixel) (h self))
                                         ;(om-draw-line  (1+ (om-point-h pixel)) 0  (1+ (om-point-h pixel)) (h self))
                                         ;(om-draw-string (+  (om-point-h pixel) 2) stry (format () "~D" i))
                                          )
                                      (when (> ppmes 20)
                                        (om-draw-line  (om-point-h pixel) 0  (om-point-h pixel) (round (h self) 3)))
                                      )))
                           
                            )))
      )))



(defmethod draw-grille-metric  ((self t)  range)
   (om-with-focused-view self
     (om-with-line '(2 2)
       (let* ((ruler (rulermetric (om-view-container self)))
              (i (first range))
              (j (second range))
              (listmes (meslist ruler))
              (tempo (tempo ruler))
              (loop-p (loop-mes-p ruler))
              (first-beat (get-beat-from-val ruler i))
              (last-beat (get-beat-from-val ruler j))
              (listduration (get-list-mes-dur ruler))
              (system-etat (get-system-etat self)))
         (loop for i from (first first-beat) to (first last-beat) do
               (let* ((themeasure (get-ieme-measure i listmes loop-p))
                      (sizepul (* 1000 (size-pulsation tempo themeasure))) 
                      (sec (* 1000 (get-val-from-meas ruler i listduration))))
                 (loop for delta from 0 to (- (first themeasure) 1) do
                       (let ((pixel (point2pixel self (om-make-big-point (round (+ (* delta sizepul) sec)) 0) system-etat)))
                         (if (= delta 0)
                             (om-draw-line  (om-point-h pixel) 0  (om-point-h pixel) (h self)))))))))))
                    
                  
      


;=================================
;view with ruler class

(defclass view-with-ruler-mixin () 
   ((grille-p :initform nil :accessor grille-p)))

(defmethod view-with-ruler-mixin-p ((self t)) nil)
(defmethod view-with-ruler-mixin-p ((self view-with-ruler-mixin)) t)

(defmethod assoc-w ((self view-with-ruler-mixin)) 
   (w self))
(defmethod assoc-h ((self view-with-ruler-mixin)) 
   (h self))

(defmethod norme2pixel  ((editor view-with-ruler-mixin) (axe (eql 'x)) val)
   "Convert val (in x ruler units) to pixels"
   (let ((durx  (abs (- (second (rangex editor)) (first (rangex editor))))))
     (if (zerop durx) 10
       (round (* (assoc-w editor) val) durx))))

(defmethod norme2pixel  ((editor view-with-ruler-mixin) (axe (eql 'y)) val)
   "Convert val (in y ruler units) to pixels"
   (let ((dury (abs (- (second (rangey editor)) (first (rangey editor))))))
     (if (zerop dury) 10
       (round (* (h editor) val) dury))))

(defmethod pixel2norme  ((editor view-with-ruler-mixin) (axe (eql 'x)) val)
   "Convert val (in pixel units) to the units defined by the ruler x associed to the view 'editor'"
  (let* ((durx  (- (second (rangex editor)) (first (rangex editor))))
        (norme (round (* (abs durx) val) (assoc-w editor))))
     norme))

(defmethod pixel2norme  ((editor view-with-ruler-mixin) (axe (eql 'y)) val)
   "Convert val (in pixel units) to the units defined by the ruler y associed to the view 'editor'"
   (let ((dury (abs (- (second (rangey editor)) (first (rangey editor)))))
           (editor-h (h editor)))
    (if (> editor-h 0) (round (* dury val) (h editor)) 0)))

(defmethod draw-grille-linex  ((self view-with-ruler-mixin) point offy)
   (om-draw-line point (+ offy 0) point (+ offy (h self))))

(defmethod draw-grille-liney  ((self view-with-ruler-mixin) point offx)
   (om-draw-line (+ offx 0) point (+ offx (assoc-w self)) point))

   
;"The default assume that num is an integer."
(defmethod get-string-nom  ((self view-with-ruler-mixin)  num axe)
   (declare (ignore axe))
   (format () "~D" num))

(defmethod update-view-of-ruler  ((self view-with-ruler-mixin))
   "Sometimes update drawing is hard, you can redefine this method."
  (om-redraw-view self))

(defmethod grille-on-off  ((self view-with-ruler-mixin))
   (setf (grille-p self) (not (grille-p self)))
   (if (grille-p self)
       (draw-grille self)
     (om-invalidate-view self)))


;------------

(defclass view-with-ruler-x (view-with-ruler-mixin) 
   ((rangex :initform nil :accessor rangex :initarg :rangex)
    (rulerx :initform nil :accessor rulerx)))

(defmethod get-x-range ((self view-with-ruler-x)) (rangex self))

(defmethod get-system-etat ((self view-with-ruler-x)) 
   "Return some factors used for unit conversions"
   (let ((durrangex (abs (- (second (rangex self)) (first (rangex self))))))
     (if (zerop durrangex) (list 0 0)
       (let* ((sizex (assoc-w self))
              (factx (/ sizex durrangex))
              (offsetx (round (* (first (rangex self)) factx))))
         (list factx offsetx)))))

(defmethod point2pixel ((self view-with-ruler-x) point sys-etat)
   "Convert 'point' to pixels in the view 'self', 'sys-etat' is calculated by the 'get-system-etat' function."
   (let* ((x  (om-point-h point))
          (x1 (round (* x (first sys-etat)))))
     (om-make-point (- x1 (second sys-etat)) 0)))

(defmethod xpoint2pixel ((self view-with-ruler-x) x sys-etat)
  (- (round (* x (first sys-etat))) (second sys-etat)))

(defmethod pixel2point ((self view-with-ruler-x) pixel)
   (let* ((x (om-point-h pixel))
          (sizex (assoc-w self))
          (durpointx (- (second (rangex self)) (first (rangex self))))
          (x1 (* (/ x sizex) durpointx)))
     (om-make-big-point (round (+ x1 (first (rangex self)))) 0)
     ))

(defmethod set-units-ruler  ((self view-with-ruler-x) (ruler ruler))
  (let* ((durruler (abs (- (second (rangex self)) (first (rangex self))))))
    (unless (zerop durruler)
      (setf (zoom ruler) (expt 10 (- (floor (log durruler 10)) 1))))))


(defmethod draw-grille  ((self view-with-ruler-x))
  (om-with-focused-view self
    (om-with-fg-color self *om-gray-color*
      (om-with-line '(2 2)
       (let* ((rulerx (rulerx self))
              (system-etat (get-system-etat self))
              i j ppu unit)
         (setf unit (zoom rulerx))
         (setf i (first (rangex self)))
         (setf i (if (minusp i) (+ i (- unit (mod i unit))) (+ i (mod i unit))))
         (setf j (second (rangex self)))
         (setf ppu (norme2pixel self 'x unit)) 
         (loop while (< i j) do
               (let* ((pixel (point2pixel self (om-make-big-point i 0) system-etat)))
                 (cond
                  ((and  (> (* 10 ppu) 10) (zerop (mod i (* 10 unit))))
                   (draw-grille-linex self  (om-point-h pixel) 0))
                  ((>= ppu  20)
                   (draw-grille-linex self (om-point-h pixel) 0)))
                 (incf i unit))))))))


(defmethod draw&print-axis ((self view-with-ruler-mixin) &key offx offy)
   (let* ((system-etat (get-system-etat self)))
     (om-with-line-size 1
       (when (and offx (< (first (rangex self)) 0) (< 0 (second (rangex self))))
         (let ((x0 (+ offx  (om-point-h (point2pixel self (om-make-point 0 0) system-etat)))))
           (om-draw-line x0 (+ offy 0) x0 (+ offy (h self)))))
       (when (and offy (< (first (rangey self)) 0) (< 0 (second (rangey self))))
         (let ((y0 (+ offy (om-point-v (point2pixel self (om-make-point 0 0) system-etat)))))
           (om-draw-line (+ offx 0) y0  (+ offx (assoc-w self)) y0))))))


(defmethod draw-axis ((self view-with-ruler-x)) 
   "Draw the X axes."
   (om-with-focused-view self
     (draw&print-axis self :offx 0)))



;------------

(defclass view-with-ruler-y (view-with-ruler-mixin) 
   ((rangey :initform nil :accessor rangey)
    (rulery :initform nil :accessor rulery)))

(defmethod get-system-etat ((self view-with-ruler-y)) 
   "Return some factors used for unit conversions"
   (let* ((durrangey  (abs (- (second (rangey self)) (first (rangey self)))))
          (sizey (assoc-h self))
          (facty (/ sizey durrangey))
          (offsety (round (* (first (rangey self)) facty))))
     (list facty offsety)))

(defmethod point2pixel ((self view-with-ruler-y) point sys-etat)
   "Convert 'point' to pixels in the view 'self', 'sys-etat' is calculated by the 'get-system-etat' function."
   (let* ((y  (om-point-v point))
          (y1 (round (* y (first sys-etat)))))
     (om-make-point 0 (- (- (h self) y1) (second sys-etat)))))

(defmethod pixel2point ((self view-with-ruler-y) pixel)
   (let* ((y (om-point-v pixel))
          (durpointy (- (second (rangey self)) (first (rangey self))))
          (y1 (* (/ y (assoc-h self)) durpointy)))
     (om-make-big-point 0 (round (+ (- durpointy y1) (first (rangey self)))))
     ))

(defmethod set-units-ruler  ((self view-with-ruler-y) (ruler ruler))
  (let* ((sizeruler (abs (- (second (rangey self)) (first (rangey self)))))
         (base10 (floor (log sizeruler 10))))
    (setf (zoom ruler) (expt 10 (- base10 1)))))

;------------

(defclass view-with-ruler-xy (view-with-ruler-mixin) 
   ((rangex :initform nil :accessor rangex)
    (rulerx :initform nil :accessor rulerx)
    (scrollerx :initform nil :accessor scrollerx)
    (rangey :initform nil :accessor rangey)
    (rulery :initform nil :accessor rulery)
    (scrollery :initform nil :accessor scrollery)
    ))

(defmethod get-system-etat ((self view-with-ruler-xy)) 
   "Return some factors used for unit conversions"
   (let* ((durrangex (abs (- (second (rangex self)) (first (rangex self)))))
          (durrangey (abs (- (second (rangey self)) (first (rangey self)))))
          (sizex (assoc-w self))
          (sizey (h self))
          (factx (if (zerop durrangex) 1 (/ sizex durrangex)))
          (facty (if (zerop durrangey) 1 (/ sizey durrangey)))
          (offsetx (round (* (first (rangex self)) factx)))
          (offsety (round (* (first (rangey self)) facty))))
     (list factx facty offsetx offsety)))

(defmethod point2pixel ((self view-with-ruler-xy) point sys-etat)
   "Convert 'point' to pixels in the view 'self', 'sys-etat' is calculated by the 'get-system-etat' function."
   (let* ((x  (om-point-h point))
          (y (om-point-v point))
          (x1 (round (* x (first sys-etat))))
          (y1 (round (* y (second sys-etat)))))
     (om-make-point (- x1 (third sys-etat))  
                    (+ (- (h self) y1) (fourth sys-etat)))))

(defmethod pixel2point ((self view-with-ruler-xy) pixel)
   "Convert pixels to a point in units defined by the ruler x and y associed to the view 'self'."
   (let* ((x  (om-point-h pixel))
          (y (om-point-v pixel))
          (sizex (assoc-w self))
          (sizey (h self))
          (durpointx  (abs (- (second (rangex self)) (first (rangex self)))))
          (durpointy  (abs (- (second (rangey self)) (first (rangey self)))))
          (x1 (round (* x durpointx) sizex))
          (y1 (round (* y durpointy) sizey)))
     (om-make-big-point (+  x1 (first (rangex self)))  
                     (+  (- durpointy y1) (first (rangey self))))))

(defmethod set-units-ruler  ((self view-with-ruler-xy) (ruler ruler))
   (let* ((durruler (max 1 
                         (if (equal (axe ruler) 'x)
                             (abs (- (second (rangex self)) (first (rangex self))))
                           (abs (- (second (rangey self)) (first (rangey self)))))))
          (base10 (floor (log durruler 10))))
     (setf (zoom ruler) (max 1 (expt 10 (- base10 1))))))


(defmethod zoom-system ((self view-with-ruler-xy) where)
  "Used to zoom in the wiew 'self' if it containts x and y rulers"
  (om-init-motion-functions self 'zoom-system-motion 'zoom-system-release)
  (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-selection-rectangle))


(defmethod zoom-system-motion ((self view-with-ruler-xy) pos)
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) (second rect) (max 4 (third rect)) (max 4 (fourth rect))))))

(defmethod zoom-system-release ((self view-with-ruler-xy) pos)
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos)))
        user-rect )
    (when rect
      (om-erase-movable-object self)
      (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
      (let* ((x (om-rect-topleft user-rect))
             (x1 (om-rect-bottomright user-rect))
             (dif (om-subtract-points x1 x)))
        (when (and (> (abs  (om-point-h dif)) 10) (> (abs (om-point-v dif)) 10))
          (let ((new-point (pixel2point self x))
                (new-point1 (pixel2point self x1)))
            (when (and (>  (om-point-h new-point1) (+  (om-point-h new-point) 1))
                       (> (om-point-v new-point) (+ (om-point-v new-point1) 1)))
              (set-ranges self (list  (om-point-h new-point)  (om-point-h new-point1))
                          (list (om-point-v new-point1) (om-point-v new-point) ))
              (update-view-of-ruler self))))
        (om-invalidate-view (rulerx self) t)
     (om-invalidate-view (rulery self) t))))) ;effacer zoom-system



(defmethod init-coor-system ((self view-with-ruler-xy))
   (let ((ranges (give-editor-list-range (om-view-container self))))
     (set-ranges self (list (first ranges) (second ranges)) 
                  (list (third ranges) (fourth ranges)))
     (update-view-of-ruler self)
     ))

(defmethod set-ranges ((self view-with-ruler-xy) rangex rangey)
   (setf (rangex self) rangex)
   (setf (rangey self) rangey)
   (set-units-ruler self (rulery self))
   (set-units-ruler self (rulerx self))
   )

(defmethod draw-grille  ((self view-with-ruler-xy))
  (om-with-focused-view self
    (draw&print-grille self :offx 0  :offy 0)))


(defmethod draw&print-grille  ((self view-with-ruler-xy) &key offx  offy)
   (let* ((rulerx (rulerx self))
          (rulery (rulery self))
          (system-etat (get-system-etat self)))
     (om-with-fg-color self *om-gray-color*
       (om-with-line '(2 2)
         (ruler-print-draw-grille rulery 'y offx offy system-etat)
         (ruler-print-draw-grille rulerx 'x offx offy system-etat)))))
   
(defmethod draw-axis ((self view-with-ruler-xy)) 
   "Draw the X and Y axes."
   (om-with-focused-view self
     (om-with-fg-color self *om-light-gray-color*
     (draw&print-axis self :offx 0 :offy 0))))


;(defmethod play-from-palette ((self view-with-ruler-xy))  
;  (when (and (scroll-to-0 self) (not (= 0 (first (rangex self)))))
;    (setf (rangex self) (list 0 (- (second (rangex self)) (first (rangex self)))))
;    (update-view-of-ruler self)
;    (om-invalidate-view (rulerx self)))
;  (call-next-method)
;  )
    

