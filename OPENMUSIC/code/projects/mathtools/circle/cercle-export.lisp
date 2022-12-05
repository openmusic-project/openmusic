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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; MATHTOOLS EXPORT TO SVG by K. Haddad

(in-package :om)

(defun get-one-list (n modulo-points centrex centrey radio j patt polygon step blue view)
  "outputs the polygon coordinates"
  (let (res)
  (when modulo-points
    (let (last-point first-point) 
      (loop for i from 0 to (- n 1) do
              (when (num-in-list-mod modulo-points n i)
                (let ((point (polar2car (+ (/ pi -2) (* step i)) radio)))
                  (when polygon
                    (setf first-point point)  
                    (setf last-point point))
                  (push (list  (+ centrex (car point)) (+ centrey (second point))) res))
                  ))))
  res))

(defmethod get-cercle-points ((self n-cercle) centrex centrey nsize ballsize radio point)
  "outputs all points in n-cercle according to iys modulo (n self)"
  (let* ((step (/ (* pi 2) (n self))) rep res)
    (loop for i from 0 to (- (n self) 1)
            while (not rep) do
             (let ((ppoint (polar2car (+ (/ pi -2) (* step i)) radio)))
                (let ((pr (om-make-rect (- (+ centrex (car ppoint)) ballsize)
                                        (- (+ centrey (second ppoint)) ballsize)
                                        (+ (+ centrex (car ppoint)) ballsize)
                                        (+ (+ centrey (second ppoint)) ballsize) 
                                        )))
                  (push (list (+ centrex (car ppoint)) (+ centrey (second ppoint))) res)
                  (when (om-point-in-rect-p point pr)
                    (setf rep i)))))
    res))



(defmethod cercle->svg ((self cerclePanel))
  (let* ((editor (om-view-container self))
         (obj (object editor))
         (centrex (round (w self) 2))
         (centrey (round (h self) 2))   
         (radio (round (min (w self) (h self)) 2.5))
         (cercle-pts (get-cercle-points obj 
                                        centrex centrey 
                                        12 4 radio 
                                        (om-make-point 0 0)))
         (step (/ (* pi 2) (n obj)))
         (point-list (if (consp (car (puntos obj))) (puntos obj) (list (puntos obj))))
         (modulo-points-l (loop for points in point-list
                                collect (loop for item in (remove nil points) collect (mod item (n obj)))))
         (poly-pts 
          (loop for i in modulo-points-l
                  
                collect (reverse (get-one-list (n obj) i centrex centrey radio
                                               0 *om-black-pattern* (show-polygon self)
                                               step 4 self))))
         (ppts (mapcar #'format-polypts  poly-pts)))
    (export-cercle self poly-pts cercle-pts centrex centrey radio nil)
    ))

(defun inter-pts (point lst)
  (find point lst  :test 'equal))

;(inter-pts '(259 40) '((259 40) (340 342) (178 342)))

(defun format-polypts (lst)
  (let* ((liste (append lst (list (car lst))))
        (rep (format nil "~D" (car (flat liste)))))
  (loop for i in (cdr (flat liste))
          do  (setf rep (string+ rep (format nil  ",~D" i))))
  rep))

;(format-polypts '((188 37) (264 319) (113 319)))

;(omcolor->hex (om-random-color))
;(omcolor->hex (car *16-color-list*))

(defun frst-non-nil (lst)
  (loop for i in lst 
        collect (position-if-not #'null i)))

;ajouter options:
;info
;affichage ou non du modulo

(defmethod* export-cercle ((self cerclepanel) 
                           (polyg list) 
                           (points list)
                           (centrex number)
                           (centrey number)   
                           (radio number)
                           file-path 
                           &key (line-size 1))

  "Exports <self> to SVG format."
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory)
                                                            :prompt "New SVG file"
                                                            :types '("SVG Files" "*.svg")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let* ((ppts (mapcar #'format-polypts  polyg))
             (points-color (loop for i in points
                                 collect (loop for n in polyg
                                               collect (inter-pts i n))))
             (pt-colors (frst-non-nil points-color))
             )
        (with-open-file (s pathname :direction :output :if-exists :supersede)
          (format s "<svg width=\"~D\" height=\"~D\" version=\"1.1\">~%" (* 2 centrex) (* 2 centrey))
          (format s   "<path d=\"\" fill=\"none\" stroke=\"black\" stroke-width=\"1\"/>~%")
          (format s "<g>~%")
          (format s "<circle cx=\"~D\" cy=\"~D\" r=\"~D\" stroke=\"black\"~%" centrex centrey radio) 
          (format s "stroke-width=\"1\" fill=\"none\"></circle>~%")
          ;le texte
          (format s "<text text-anchor=\"middle\" x=\"~D\" y=\"~D\" fill=\"#000000\"~%" centrex centrey)
          (format s "id=\"text1\">~D</text>~%" (n (object (om-view-container self))))
          ;les polygones
          (loop for i in ppts
                for n from 1 to (length ppts)
                do (progn 
                     (format s "<polyline points=~S~%" i)
                     (format s "fill=\"none\" stroke=~S stroke-width=\"1\">~%" 
                             (omcolor->hex (car (rotate *16-color-list* n))))
                     (format s "</polyline>~%")
                     (format s "</g>~%")
                     (format s "<svg>"))
                   )
           ;les points
          (loop for i in points
                for n in pt-colors
                do (progn
                     (format s "<circle cx=\"~D\" cy=\"~D\" r=\"3\" stroke=~S~%" (car i) (second i)
                             (if n (omcolor->hex (car (rotate *16-color-list* (1+ n)))) "none"))
                     (format s "stroke-width=\"1\" fill=~S></circle>~%" 
                             (if n (omcolor->hex (car (rotate *16-color-list* (1+ n)))) "none"))
                             
                     ))
         
          ))
      pathname
      )))
