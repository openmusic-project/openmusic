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
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon


(in-package :om)
 
;=======

(defmethod edition-values   ((self miniview))
  (get-mini-param self 'cmnpref))

(defmethod edition-values  ((self tempobjframe))
  (get-mini-param self 'cmnpref))

;=======
(defmethod get-mini-zoom    ((self t)) 0.8)
(defmethod get-mini-zoom    ((self miniview)) 0.8)
(defmethod get-mini-zoom   ((self tempobjframe))
   (let* ((musobj (get-mus-ob (object self)))
          (obj-dur (get-obj-dur musobj))
          (pixsize (w self))
          (zomm1pixsize (max 1 (ms2pixel obj-dur 4 1))))
     (/ pixsize zomm1pixsize)))


(defmethod default-obj-box-size      ((self simple-score-element))    (om-make-point 65 70))
(defmethod default-obj-box-size      ((self score-element))    (om-make-point 80 70))

(defparameter *miniview-font-size* 20)
(defmethod mv-font-size  ((self t)) *miniview-font-size*)

;;; plus la peine avec LW ?
(defmethod mv-view-size  ((value t) self)
  ;(om-make-point (w self) (max 500 (h self)))
  (om-view-size self))

(defmethod cons-mini-pict ((self simple-container) frame fontsize size)
  (let ((newfont (om-make-music-font *heads-font* fontsize)))
    (om-record-pict newfont size
      (draw-mini-obj self frame fontsize size))
    ))

(defmethod get-mini-staff ((self simple-container) view)
  (get-staff-system (or (get-mini-param view 'staff) 'g)))

(defmethod get-win-ed-size  ((self simple-container)) (om-make-point 500 280))

(defmethod mv-deltay ((self simple-container) view staffsys sizefont) 
   (round (+  (inity view) (* -1 (system-offset-in-pix staffsys sizefont))  (* 1.1 sizefont))))

(defvar *draw-mini-pict* nil)

; redefined in tonality project
(defmethod set-system-tonality (staffsys object) nil)

; (get-approx-scale self)

(defmethod get-miniview-scale (view object)
  (or (and *om-tonalite* (get-scale-from-tonality object))
      (get-current-scale (get-mini-param view 'approx))))

(defmethod get-miniview-top ((self simple-container) staffsys) 
  (top-in-midi staffsys))

(defmethod draw-mini-obj ((self simple-container) view sizefont size)
  (let* ((*internal-score-fonts* (init-fonts-to-draw sizefont))
         (*draw-mini-pict* t)
         (staffsys (get-mini-staff self view))
         (scale (get-miniview-scale view self))
         (grapobj (make-graph-form-obj self 0 (get-miniview-top self staffsys) 
                                       (/ sizefont 4) 0 scale nil staffsys t))
         (deltay (+ (inity view) (round (* 1.4 sizefont)))))
    (when grapobj 
      (space-objects grapobj sizefont)
      (set-graph-rectangles grapobj))
      (set-system-tonality staffsys self)
      (om-with-line-size 1 
        (om-with-font (get-font-to-draw 0)
                      (draw-object-mini-obj self view grapobj staffsys sizefont size deltay)
                      ))))

(defmethod draw-object-mini-obj ((self simple-container) view grapobj staffsys sizefont size deltay) 
  (draw-one-system staffsys -6 deltay (w view) sizefont (minitempo self) nil nil)
  (when grapobj
    (draw-object grapobj view (* sizefont (if (tonalite self) 4 2)) (- deltay (round (* (posy (car (staff-list staffsys))) (/ sizefont 4)))) 
                 (get-mini-zoom view) 0 (om-point-h size) 0 (om-point-v size) 
                 'midic sizefont t staffsys nil nil)))


(defmethod draw-mini-piano-roll ((self simple-container) (view tempobjframe) size)
  (let* ((deltay 0)
         (dur (get-obj-dur self))
         (minh 4000) (maxh 9000)
         (range (round (- maxh minh) 100))
         (noteh (max 2 (round (om-point-v size) range))))
    (om-with-fg-color view (colorframe (object view))
      (draw-mini-piano-notes self 0 deltay size dur minh maxh noteh))))
     
(defmethod draw-mini-piano-notes ((self simple-container) x y size dur minh maxh noteh)
  (draw-mini-piano-notes (objfromobjs self (make-instance 'chord-seq)) x y size dur minh maxh noteh))
 
(defmethod draw-mini-piano-notes ((self chord-seq) x y size dur minh maxh noteh)
    (loop for chord in (inside self) do
          (draw-mini-piano-notes chord x y size dur minh maxh noteh)))

(defmethod draw-mini-piano-notes ((self chord) x y size dur minh maxh noteh)
  (let ((xc (om-scale (offset->ms self) 0 (om-point-h size) 0 dur)))
    (loop for note in (inside self) do
          (draw-mini-piano-notes note (+ x xc) y size dur minh maxh noteh))))

(defmethod draw-mini-piano-notes ((self note) x y size dur minh maxh noteh)
  (let* ((x1 (round (+ x (om-scale (offset self) 0 (om-point-h size) 0 dur))))
         (w (round (om-scale (dur self) 0 (om-point-h size) 0 dur)))
         (x2 (round (+ x1 w)))
         (y (round (- (om-point-v size) (om-scale (midic self) 0 (om-point-v size) minh maxh)))))
    (om-fill-rect x1 y w noteh)
    ))


(defmethod minitempo ((self t)) nil)
(defmethod minitempo ((self voice)) (car (tempo self)))
(defmethod minitempo ((self poly)) (loop for item in (inside self) collect (car (tempo item))))
;;;;===== synth maq
(defmethod collect-page-all-line-elements ((self multiseqPanel) grap-obj fdoc pagenum line)
  (loop for chord-seq in (inside grap-obj)
          for voice = 0 then (+ voice 1) append
          (get-page-line-elements  chord-seq fdoc pagenum line voice)))

(defmethod collect-page-all-line-elements ((self polyPanel) grap-obj fdoc pagenum line)
 (loop for voice in (inside grap-obj)
          for i = 0 then (+ i 1) append
          (get-page-line-elements voice fdoc pagenum line 0)))

(defmethod cons-maq-mini-obj ((self chord-seq) (view t) sizefont size)
  (let ((deltay 0)
        (dur (get-obj-dur self))
        (minh 3000) (maxh 9800) range noteh)
    (setf range (round (- maxh minh) 100))
    (setf noteh (round (om-point-v size) range))
    (om-with-fg-color nil (colorframe (object view))
    (loop for chord in (inside self) do
        (let ((x (om-scale  (offset->ms chord) 0 (om-point-h size) 0 dur)))
          (loop for note in (inside chord) do
                (let* ((x1 (round (+ x (om-scale (offset note) 0 (om-point-h size) 0 dur))))
                       (w (round (om-scale (dur note) 0 (om-point-h size) 0 dur)))
                       (x2 (round (+ x1 w)))
                       (y (round (- (om-point-v size) (om-scale (midic note) 0 (om-point-v size) minh maxh)))))
                  (om-fill-rect x1 y w noteh)
                  )
          ))))))

(defmethod cons-maq-mini-obj ((self voice) (view t) sizefont size)
  (cons-maq-mini-obj (objfromobjs self (make-instance 'chord-seq)) view sizefont size))

(defmethod cons-maq-mini-obj ((self chord) (view t) sizefont size)
  (cons-maq-mini-obj (objfromobjs self (make-instance 'chord-seq)) view sizefont size))

(defmethod cons-maq-mini-obj ((self note) (view t) sizefont size)
  (cons-maq-mini-obj (objfromobjs self (make-instance 'chord-seq)) view sizefont size))

;;-------For note voices, chords ,etc

(defmethod draw-mini-view ((self t) (value simple-score-element))  
  (score-draw-mini-view self value))

(defmethod draw-mini-view ((self t) (value score-element))
  (score-draw-mini-view self value))

;;; no minipict for score objects 
(defmethod score-draw-mini-view ((self t) value)
   (if (minipict self) ;; => always NIL
     (let ((x0 (initx self)) (y0 (inity self)) (pictsize (om-get-picture-size (minipict self))))
       (om-draw-picture self (minipict self) :pos (om-make-point x0 y0) :size pictsize))
   (om-with-focused-view self
     (draw-mini-obj value self (mv-font-size value) (mv-view-size value self)))))

(defmethod update-miniview ((self t) (value simple-score-element)) 
  (score-update-miniview self value))

(defmethod update-miniview ((self t) (value score-element)) 
  (score-update-miniview self value))

(defmethod score-update-miniview ((self t) value)
  (when (minipict self) (om-kill-picture (minipict self)) (setf (minipict self) nil))
  ;(setf (minipict self)  (cons-mini-pict value self (mv-font-size value) (mv-view-size value self)))
  (om-invalidate-view self t))

;;; when drawing must fit in a given rectangle
(defmethod draw-obj-in-rect ((self  simple-score-element) x x1 y y1 edparams  view)
  (score-draw-obj-in-rect self x x1 y y1 edparams view))

(defmethod draw-obj-in-rect ((self  score-element) x x1 y y1 edparams  view)
  (score-draw-obj-in-rect self x x1 y y1 edparams view))

(defmethod score-draw-obj-in-rect (self x x1 y y1 edparams  view)
  (let* ((size (om-make-point (- x1 x) (- y1 y)))
         (thepict (cons-mini-pict self view (mv-font-size view) size)))
     (om-draw-picture view thepict :pos (om-make-point x y) :size size)
     (om-kill-picture thepict) t))

;exceptions por ahora

(defmethod draw-mini-view ((self t) (value rest)) t)
(defmethod update-miniview ((self t) (value rest)) (om-invalidate-view self t))
(defmethod draw-obj-in-rect ((self  rest) x x1 y y1 edparams  view) t)

(defmethod draw-mini-view ((self t) (value group)) t)
(defmethod update-miniview ((self t) (value group)) (om-invalidate-view self t))
(defmethod draw-obj-in-rect ((self  group) x x1 y y1 edparams  view) t)

(defmethod draw-mini-view ((self t) (value measure)) t)
(defmethod update-miniview ((self t) (value measure)) (om-invalidate-view self t))
(defmethod draw-obj-in-rect ((self  measure) x x1 y y1 edparams  view) t)


;------------------------------
;note
;------------------------------
(defmethod Class-has-editor-p  ((self note)) t)
(defmethod get-editor-class ((self note)) 'noteEditor)

(defmethod get-mini-staff ((self note) view)
   (get-staff-system (if (midic self)
                         (cond
                          ((> (midic self) 8700) 'G2)
                          ((> (midic self) 5800) 'G)
                          ((> (midic self) 4200) 'F)
                          (t 'F2))
                       'G)))


;-----------------------
;Chord
;-----------------------
(defmethod Class-has-editor-p  ((self chord)) t)
(defmethod get-editor-class ((self chord)) 'chordEditor)

;------------------------------
;voice
;------------------------------
(defmethod Class-has-editor-p  ((self voice)) t)
(defmethod get-editor-class ((self voice)) 'voiceEditor)

;(defmethod update-miniview ((self t) (value voice)) 
;   (when (minipict self) (om-kill-picture (minipict self))
;         (setf (minipict self) nil))
;   ;;;(setf (minipict self) (cons-mini-pict value self (mv-font-size value) (mv-view-size value self)))
;   (om-invalidate-view self t))

;------------------------------
;poly
;------------------------------

(defmethod Class-has-editor-p  ((self poly)) t )
(defmethod get-editor-class ((self poly)) 'polyEditor)

(defun draw-multi-mini-obj (self view grapobj staffsys sizefont size deltay) 
  (draw-score-line staffsys view -6 deltay
                   (w view) sizefont (minitempo self) nil nil)
  (draw-object grapobj view (* 2 sizefont) deltay
               (get-mini-zoom view)
               0 (om-point-h size) 0 (om-point-v size) 'midic sizefont t staffsys nil nil))

(defmethod draw-object-mini-obj ((self poly) view grapobj staffsys sizefont size deltay) 
  (draw-multi-mini-obj self view grapobj staffsys sizefont size deltay))

(defmethod get-mini-staff ((self poly) view)
  (correct-staff-val self (get-staff-system (get-mini-param view 'staff)) view))

(defmethod get-miniview-top ((self poly) staffsys) nil)


;------------------------------
;chordseq
;------------------------------
(defmethod Class-has-editor-p  ((self chord-seq)) t )
(defmethod get-editor-class ((self chord-seq)) 'chordseqEditor)

;------------------------------
;multiseq
;------------------------------
(defmethod Class-has-editor-p  ((self multi-seq)) t )
(defmethod get-editor-class ((self multi-seq)) 'multiseqEditor)


(defmethod draw-object-mini-obj ((self multi-seq) view grapobj staffsys sizefont size deltay) 
  (draw-multi-mini-obj self view grapobj staffsys sizefont size deltay))

(defmethod update-miniview ((self t) (value multi-seq))
   (when (minipict self) (om-kill-picture (minipict self))
         (setf (minipict self) nil))
   (set-mini-param self 'staff (correct-staff-val value (get-mini-param self 'staff) self)) 
   ;(setf (minipict self)  (cons-mini-pict value self (mv-font-size value) (mv-view-size value self)))
   (om-invalidate-view self t))


(defmethod get-mini-staff ((self multi-seq) view)
  (correct-staff-val self (get-staff-system (get-mini-param view 'staff)) view))

(defmethod get-miniview-top ((self multi-seq) staffsys) nil)

(defun correct-staff-val (multi staff view)
  (setf staff (list! staff))
  (let ((numcs (length (inside multi)))
        (numsat (length staff))
        (system-space (score-system-space view)))
    (unless (= numcs (length system-space))
      (setf system-space (make-list numcs :initial-element 1))
      (score-system-space view system-space))
    (cond 
     ((> numcs numsat)
      (setf system-space (reverse system-space))
      (setf staff (reverse staff))
      (loop for i from 1 to (- numcs numsat) do
            (push (clone (car staff)) staff)
            (push 1 system-space))
      (setf staff (reverse staff))
      (score-system-space view (reverse system-space)))
     ((< numcs numsat)
      (setf staff (subseq staff 0 numcs))
      (score-system-space view (subseq system-space 0 numcs))))
    staff))

;===============================================
;Methods for MUSIC EDITORS box
;===============================================

;------Music notation

;----note
(defmethod draw-editor-mode ((self note) view) 
   (draw-mini-view view self)
   (draw-carre view))
;-----chord
(defmethod draw-editor-mode ((self chord) view) 
   (draw-mini-view view self)
   (draw-carre view))

;-----voice
(defmethod draw-editor-mode ((self voice) view) 
   (draw-mini-view view self)
   (draw-carre view))

;-----poly
(defmethod draw-editor-mode ((self poly) view) 
   (draw-mini-view view self)
   (draw-carre view))

;-----voice
(defmethod draw-editor-mode ((self chord-seq) view) 
   (draw-mini-view view self)
   (draw-carre view))

;-----poly
(defmethod draw-editor-mode ((self multi-seq) view) 
   (draw-mini-view view self)
   (draw-carre view))



#|
(defun get-x-size (score)
(let ((ssize (cmn::size score)))
     (loop for item in (cmn::systems score)
           maximize 
           (loop for staff in (cmn::staves item)
                 maximize
                 (let* ((thedata (cmn::staff-data staff))
                        (inside (cddr thedata)))
                   (loop for item1 in inside
                         maximize (* ssize (+ (cmn::dx item1) (cmn::x1 item1)))))))))
|#
#|
(defun get-y-size (score)
   (let ((ssize (cmn::size score)))
     (loop for item in (cmn::systems score)
           minimize 
           (loop for staff in (cmn::staves item)
                 minimize 
                 (* ssize (+ (cmn::dy staff)  (cmn::y0 staff) 1))))))

|#



;===========
(defmethod draw-editor-mode ((self maquette-obj) view)
   (let ((durx (get-obj-dur self)))
     (om-with-focused-view view
       (rectangles&containers self 0 0 (w view) (h view) durx 0))
     (draw-carre view t)))


(defmethod rectangles&containers ((self container) x0 y0 x1 y1 durx i)
   (call-next-method)
   (when (inside self)
     (let ((deltay (round y1 (length (inside self))))) 
       (loop for item in (inside self)
             for posy = 0 then (+ posy deltay) do
             (let ((ofpix (round (* (offset->ms item) x1) durx))
                   (durpix (round (* (get-obj-dur item) x1) durx)))
               (if (zerop durpix) (setf durpix 2))
               (rectangles&containers item  (+ x0 ofpix) (+ y0 posy) (- durpix 1) (- deltay 1)  (extent->ms item)  (+ i 1)))))))

(defmethod rectangles&containers ((self sequence*) x0 y0 x1 y1 durx i)
   (om-with-fg-color nil (nth (mod i 6) *obje-color-list*)
     (om-fill-rect x0 y0 x1 y1))
   (when (inside self)
     (let ((dury (round (* y1 0.8)))) 
       (loop for item in (inside self) do
             (let ((ofpix (round (* (offset->ms item) x1) durx))
                   (durpix (round (* (get-obj-dur item) x1) durx)))
               (if (zerop durpix) (setf durpix 2))
               (rectangles&containers item  (+ x0 ofpix) (+ y0 (round (* y1 0.1))) (- durpix 1) (- dury 1)  (extent->ms item)  (+ i 1)))))))

(defmethod rectangles&containers ((self simple-container)  x0 y0 x1 y1 durx i)
   (om-with-fg-color nil (nth (mod i 6) *obje-color-list*)
     ;(draw-rect x0 y0 x1 y1)
     (om-fill-rect x0 y0 x1 y1)))
                 

(defmethod draw-obj-in-rect ((self  ommaquette) x x1 y y1 edparams  view)
  (om-with-fg-color nil (maq-color (params self)) 
      (let ((durmaq (get-obj-dur self))
            (rangey (max-y-item self)))
        (om-fill-rect x y (- x1 x) (- y1 y))
        (loop for item in (boxes self) do
              (when (boxtempobj-p item)
                (draw-rect-in-rect item  (- x1 x) (- (- y1 y) 5)  x y  durmaq (first rangey) (second rangey) 1 view))))))



(defmethod default-obj-box-size      ((self chord))    (om-make-point 100 70))
(defmethod default-obj-box-size      ((self voice))    (om-make-point 140 70))
(defmethod default-obj-box-size      ((self poly))    (om-make-point 130 65))

(defmethod default-obj-box-size      ((self chord-seq))    (om-make-point 130 70))
(defmethod default-obj-box-size      ((self multi-seq))    (om-make-point 130 65))

(defmethod default-obj-box-size      ((self note))    (om-make-point 55 65))

