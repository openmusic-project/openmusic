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
;===========================================================================

; SHEET package by C. Agon & J. Bresson

(in-package :om)


(defmethod collect-objects-to-break ((self grap-sheet) n) 
  (loop for item in (inside self)
        for i = 0 then (+ i 1) 
        append (collect-objects-to-break item i)))

(defmethod collect-onsets-to-break ((self grap-sheet) n) 
  (loop for item in (inside self)
        for i = 0 then (+ i 1) 
        append (collect-onsets-to-break item i)))

(defmethod collect-objects-to-break ((self grap-track-ev) n) 
  (list (list n self (rectangle self))))

(defmethod collect-onsets-to-break ((self grap-track-ev) n)  
  (list (list n (car (rectangle self)) (main-point self) (if (start? self) (start-t (reference self)) (end-t (reference self))))))


(defmethod cons-mixed-lines ((score sheetPanel) list linesize)
  (make-page-break (x->dx list) linesize))

;=======================
;POUR SHEET A FAIRE
;=======================

;old pour le sheet
;(defmethod cons-score-pages-list ((score sheetPanel) (self sheet) graph-obj line size)
;  (let* ((onsets+voice (sort (collect-onsets-to-break graph-obj 0) '< :key 'second))
;         (onsets (loop for item in onsets+voice collect (second item)))
;         (wpage (score-widht score size))
;         (hpage (score-height score size))
;         (mixed-lines (make-page-break (x->dx onsets) wpage)) ; ojo los ultimos ceros deben pasar a la linea siguiente
;         (pages (make-pages-from-lines mixed-lines hpage line size score))
;         (fdoc (make-instance 'fdoc))
;         (numvoices (length (inside self)))
;         (linecount -1)
;         )
;    (loop for item in pages do
;          (let ((newpage (make-instance 'fpage)))
;            (loop for line in item do
;                  (let ((newline (make-instance 'fline)))
;                    (setf curline (nth (incf linecount) mixed-lines))
;                    (setf (line-info newline) (compute-line-info (first-n onsets+voice (length curline)) numvoices))
;                    (setf onsets+voice (nthcdr  (length curline) onsets+voice))
;                    (push newline (line-list newpage))))
;            (setf (line-list newpage) (reverse (line-list newpage)))
;            (push newpage (page-list fdoc))))
;    (setf (page-list fdoc) (reverse (page-list fdoc)))
 ;   (score-fdoc score fdoc)))

;(defmethod make-score-pages-picts ((score sheetPanel) self  grapobj x y zoom  slot size  staff chnote)
;  (let ((newfont (om-make-font *heads-font* size))
;        (page-size (score-paper-size score)))
;    (score-picts-list score
;                      (loop for i from 0 to (- (length (page-list (score-fdoc score))) 1) collect
;                            (om-open-pict-window page-size newfont)))
;;    (loop for item in (score-picts-list score)
 ;         for i = 0 then (+ i 1) do
 ;         (om-with-focused-view item
 ;           (om-with-fg-color nil *om-white-color* 
 ;             (om-fill-rect 0 0 (om-point-h page-size) (om-point-v page-size))
 ;             (draw-score-page  staff  (howmany-lines (score-fdoc score) i) score x y (score-widht score size) size nil nil))))
 ;   (page-draw-object  grapobj score (car (score-picts-list score)) self x y zoom  slot size  staff  chnote)
 ;   (score-picts-list score
 ;                     (loop for item in  (score-picts-list score) collect
 ;                           (om-close-pict-window item)))))


;(defmethod page-draw-object ((self grap-track) score view sheet x y zoom  slot size  staff  chnote )
;  (draw-object self view x y zoom 0 0 0 0 slot size t staff nil chnote))


         
;(defmethod page-draw-object ((self grap-sheet) score view sheet x y zoom  slot size  staff  chnote )
;  (let ((posy y)
;        (timebpf (make-bpf-time (collect-bpftime-objects self 0 size))))
;    (setf (timebpf self) timebpf)
;    (om-with-focused-view view
;      (loop for item in (inside self)
;            for i = 0 then (+ i 1) 
;            for system in staff do
;            (page-draw-object item score view (make-instance 'page-pointer 
;                                          :score score
;                                          :view view
;                                          :voice i
;                                          :linesizex (score-widht  score size)
;                                          :linesizey (get-delta-line staff size score )) 
;                              x (- posy (round (* size (/ 23 8)))) zoom  slot size (nth i staff)  chnote )
;            (setf posy (+ posy (get-delta-system system size score i))))
;      (collect-rectangles self)
;      ;(draw-aligned-measures self meas-list staff size y positions)
;      (draw-extras self view size staff))))

;borrar en drawing-pages

;(defmethod make-pages-form-obj ((score sheetPanel) (self sheet)  x top linespace mode scale sel system stem)
;  (let ((graph-obj (make-graph-ryth-obj self top system linespace  scale sel nil nil nil))
;        (size  (* linespace 4)))
;    (space-objects (get-temporal-objects graph-obj) (* 4 linespace))
;    (set-graph-rectangles graph-obj)
;    (cons-score-pages-list score self graph-obj system (* linespace 4))
;    (make-score-pages-picts  score self graph-obj (+ x (* size (score-left-margin score))) (* (* linespace 4) (score-top-margin score)) 1  0 
;                             (* linespace 4)  system nil)
;    graph-obj))


;(defmethod get-aligne-measures ((self sheet))
;   (let* ((ofsets (loop for item in (inside self)
;                        when (voice-p item) collect (loop for mes in (inside item) collect (offset->ms mes))))
;          (all (sort (remove-duplicates (flat ofsets) :test 'equal) '<)) rep)
;     (loop for item in all do
;           (let (resp )
;             (loop for voice in ofsets
;                   for i = 0 then (+ i 1) do
;                   (let ((pos (position item voice)))
;                     (when pos (push (list pos i) resp))))
;              (push (reverse resp) rep)))
;     (reverse rep)))get-page-line-elements


;(defmethod draw-aligned-measures ((self grap-sheet) list staff size positions)
;   t)

