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



;;;===============================================
;;; SVG export
;;;===============================================

(in-package :om)

;;faire un  pre-process du bpf-lib:
;; valeurs >  0 .. Multiplier les x (bpf-scale)
;; pour les Ys idem.... pas de valeurs negatives.



;a revoir
(defmethod* pre-process-bpf ((self bpf))
  (let* ((xs (x-points self))
         (ys (y-points self))
         (lim-x (list (reduce #'min xs) (reduce #'max xs)))
         (lim-y (list (reduce #'min ys) (reduce #'max ys)))
         )
    (print (list lim-x lim-y))))


;;must do this for bpf-libs.  Since we cannot individualy scale bpfs
;because of coordinates.

(defmethod* pre-process-bpf ((self bpf-lib))
  "If necessary, this will shift coordinates in order to have only positive ones."
  (let* ((bpfs (bpf-list self))
         (xs (mapcar #'x-points bpfs))
         (ys (mapcar #'y-points bpfs))
         (min-x (reduce #'min (flat xs)))
         (min-y (reduce #'min (flat ys)))
         (off-x (if (minusp min-x) (abs min-x) 0))
         (off-y (if (minusp min-y) (abs min-y) 0))
         (shift-bpfs
          (loop for i in bpfs collect
                (let ((minx (reduce #'min (x-points i)))
                      (maxx (reduce #'max (x-points i)))
                      (miny (reduce #'min (y-points i)))
                      (maxy (reduce #'max (y-points i))))
                (bpf-scale i
                           :x1 (+ off-x minx) :x2  (+ off-x maxx)
                           :y1 (+ off-y miny) :y2  (+ off-y  maxy))))))
    (make-instance 'bpf-lib 
                   :bpf-list shift-bpfs);;in order to accept bpc-libs also..
    ))


 (defmethod* pre-process-bpf ((self bpc-lib))
  "If necessary, this will shift coordinates in order to have only positive ones."
  (let* ((bpfs (bpf-list self))
         (xs (mapcar #'x-points bpfs))
         (ys (mapcar #'y-points bpfs))
         (min-x (reduce #'min (flat xs)))
         (min-y (reduce #'min (flat ys)))
         (off-x (if (minusp min-x) (abs min-x) 0))
         (off-y (if (minusp min-y) (abs min-y) 0))
         (shift-bpfs
          (loop for i in bpfs collect
                (let ((minx (reduce #'min (x-points i)))
                      (maxx (reduce #'max (x-points i)))
                      (miny (reduce #'min (y-points i)))
                      (maxy (reduce #'max (y-points i))))
                (bpf-scale i
                           :x1 (+ off-x minx) :x2  (+ off-x maxx)
                           :y1 (+ off-y miny) :y2  (+ off-y  maxy))))))
    (make-instance 'bpc-lib 
                   :bpf-list shift-bpfs);;in order to accept bpc-libs also..
    ))   

;used to auto-calculate H and w of svg-scene.
(defmethod bpfs-limits ((self bpf-lib))
  (let* ((bpfs (bpf-list self))
         (xs (mapcar #'x-points bpfs))
         (ys (mapcar #'y-points bpfs))
         (max-x (reduce #'max (flat xs)))
         (max-y (reduce #'max (flat ys))))
    (list max-x max-y)))

;peut-etre pas utile??
#|
(defmethod bpfs-limits ((self bpc-lib))
  (let* ((bpfs (bpf-list self))
         (xs (mapcar #'x-points bpfs))
         (ys (mapcar #'y-points bpfs))
         (max-x (reduce #'max (flat xs)))
         (max-y (reduce #'max (flat ys))))
    (list max-x max-y)))
|#




(defmacro with-svg-scene-to-file ((file-path scene) &body body)
  (let ((pathname (gensym)))
    `(let* ((,pathname (or ,file-path (om-choose-new-file-dialog :directory (def-save-directory)
								 :prompt "New SVG file"
								 :types '("SVG Files" "*.svg")))))
       (when ,pathname
	 (setf *last-saved-dir* (make-pathname :directory (pathname-directory ,pathname)))
	 ,@body
	 (with-open-file (s ,pathname :direction :output :if-exists :supersede)
	   (cl-svg::stream-out s ,scene))
	 pathname))))



;;;maybe update it :::

(defmethod* export-svg ((self bpf) file-path &key with-points (w 300) (h 300) (margins 20) (line-size 1))
  :icon 659
  :indoc '("a BPF object" "a pathname" "draw-points" "image width" "image height" "margins size" "line-size")
  :initvals '(nil nil nil 300 300 20 1)
  :doc "
Exports <self> to SVG format.
"
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory)
                                                            :prompt "New SVG file"
                                                            :types '("SVG Files" "*.svg")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let* ((bpf-points (point-pairs (bpf-scale self :x1 margins :x2 (- w margins) :y2 margins :y1 (- h margins)))) ; y2 and y1 switched to have the correct orientation
             (scene (cl-svg::make-svg-toplevel 'cl-svg::svg-1.1-toplevel :height h :width w))
             (prev_p nil)
             (path (cl-svg::make-path))
             (bpfcolorstr (format nil "rgb(~D, ~D, ~D)"
				    (round (* 255 (om-color-r (bpfcolor self))))
				    (round (* 255 (om-color-g (bpfcolor self))))
				    (round (* 255 (om-color-b (bpfcolor self)))))))
        ;draw line
        (loop for pt in bpf-points do
              (cl-svg::with-path path
                (if prev_p
                    (cl-svg::line-to (car pt) (cadr pt))
                  (cl-svg::move-to (car pt) (cadr pt))))
              (setf prev_p pt))
        (cl-svg::draw scene (:path :d path)
                   :fill "none" :stroke bpfcolorstr :stroke-width line-size)

        ;if points, draw points
        (when with-points
          (loop for pt in bpf-points do
                (cl-svg::draw scene (:circle :cx (car pt) :cy (cadr pt) :r (if (numberp with-points) with-points 2))
                           :stroke "rgb(0, 0, 0)"
                           :fill bpfcolorstr)))

        (with-open-file (s pathname :direction :output :if-exists :supersede)
          (cl-svg::stream-out s scene)))
      pathname
      )))




(defmethod* export-svg ((self bpf-lib) file-path &key with-points (w nil) (h nil) (margins 20) (line-size 1)) 
  
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory)
							    :prompt "New SVG file"
							    :types '("SVG Files" "*.svg"))))
         (preself (pre-process-bpf self))
         (lim (bpfs-limits preself))
         (width (if w w (car lim)))
         (height (if h h (second lim)))
	 (scene (cl-svg::make-svg-toplevel 'cl-svg::svg-1.1-toplevel :height height :width width))
         )
    
    (when pathname
      (with-svg-scene-to-file (pathname scene)
	(loop for bpf in (bpf-list preself)
              do (let* ((ys (om* -1 (y-points bpf)))
                        (bpf-points (point-pairs (bpf-scale bpf 
                                                            :y1 (+ (second lim) (reduce #'max ys)); (second lim) -> max y 
                                                            :y2 (+ (second lim) (reduce #'min ys))
                                                            )))
                        (prev_p nil)
                        (path (cl-svg::make-path))
                        (bpfcolorstr (format nil "rgb(~D, ~D, ~D)"
                                             (round (* 255 (om-color-r (bpfcolor bpf))))
                                             (round (* 255 (om-color-g (bpfcolor bpf))))
                                             (round (* 255 (om-color-b (bpfcolor bpf)))))))

                   ;(print (list (second lim) (list-min ys) (list-max ys)))

							    ;draw line
                   (loop for pt in bpf-points do
                         (cl-svg::with-path path
                           (if prev_p
                               (cl-svg::line-to (car pt) (cadr pt))
                             (cl-svg::move-to (car pt) (cadr pt))))
                         (setf prev_p pt))
                   (cl-svg::draw scene (:path :d path)
                                 :fill "none" :stroke bpfcolorstr
                                 :stroke-width line-size)

							    ;if points, draw points
                   (when with-points
                     (loop for pt in bpf-points do
                           (cl-svg::draw scene (:circle :cx (car pt) :cy (cadr pt) :r (if (numberp with-points) with-points 2))
                                         :stroke "rgb(0, 0, 0)"
                                         :fill bpfcolorstr)))))))))


;;;===============================================
;;; SVG import
;;;===============================================


;;;tools

(defun string-to-list (string &optional (separator " "))
  (when string
    (multiple-value-bind (token rest)
        (string-until-char string separator)
      (cons token (string-to-list rest separator)))))

(defun string-until-char (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

(defun string-until-char1 (string)
  (let ((index (search "," string)))
    (if index (list (read-from-string (subseq string 0 index)) (read-from-string (subseq string (+ index 1))))
        (list (read-from-string string) nil))))


(defun string-until-char2 (string)
  (let ((index (search "," string)))
    (if index 
        (list  (read-from-string (subseq string 0 index))
               (read-from-string (subseq string (+ index 1))))
      (if (numberp  (read-from-string string)) (list (read-from-string string))
      string)
        )))


;;;svg stuff

;;Needs revision and cleanup


;;; coordinates go by pair starting with M
;;;M = moteto

;;;when h = horizontal one value is given
;;;take precedent point and apply new point x2 as :
;;;;(list (+ x1 x2) y1)

;;; same should be for v = vertical 
;;;; (list x1 (+ y1 y2))

;;; z  is for close path 
;;;so get the first point !


;;;;


;;use string= because equal doesn't differentiate between upper/lower case
  
;;; A REVOIR !
(defun svg-format-path (data)
  (let* ((clone (clone data))
         (token (car data))
         res)
    ;(print data)
    (loop while clone
          do (if (stringp (car clone))
                 (progn (setf token (car clone))
                   (pop clone))
               (cond 
                ((equal token "M")
                 (progn 
                  ; (print (car clone))
                   (push (car clone) res)
                   (pop clone)
                  ; (setf token "L");;peut-etre non!
                   ))
                ((equal token "m")
                 (push (car clone) res)
                 (push (om+ (car clone) (second clone)) res)
                 (pop clone) 
                 (pop clone)
                        ;(print res)
                 )              
                ((equal token "H")
                 (progn 
                   ;(print (list (caar clone) (second (car res))))
                   (push (list (caar clone) (second (car res))) res)
                   (pop clone)))
                
                ((equal token "V")
                 (progn 
                  ; (print (caar clone))
                   (push (list (caar res) (caar clone)) res)
                   (pop clone)))
               
                ((equal token "h")
                 (progn 
                  ; (print (list (list res (caar clone)) (cdar res)))
                   (push (list (+ (caar res) (caar clone)) (cadar res)) res)
                   (pop clone)))
                
                ((equal token "v")
                 (progn
                   ;(print (list (caar res) (list (cadar res) (caar clone))))
                   (push (list (caar res) (+ (cadar res) (caar clone))) res)
                   (pop clone)))
                ((equal token "L") ;a voir
                 (progn 
                   (push (car clone) res)
                   (pop clone)))
              
                ((equal token "l") ;a voir
                 (progn 
                   (push (list  
                          (+ (caar clone) (caar res))
                          (+ (second (car clone)) (second (car res))) 
                          ) res)
                   (pop clone)))
               ; ((string= token "Z")
               ;  (push (last-elem res) res))
                (t (pop clone)))))

    (if (or (equal (last-elem data) "Z")    
            (equal (last-elem data) "z"))
        (push (last-elem res) res))
    (reverse res)
    ))
                

(defmethod! make-bpc ((x-points list) 
                      (y-points list)
                      &optional (decimals 0))
            (let ((mybpc (make-instance 'bpc))
                  (points (loop for x in x-points
                                for y in y-points
                                collect (om-make-point x y))))
              (setf (point-list mybpc) points)
              mybpc))


(defun svg-render-bpc (data height)
  (let* ((trans (mat-trans data))
         (x (car trans))
         (y (om- height (second trans))))
   ; (print (list x y))
    (make-bpc x y)))
    


(defun string-to-list1 (string &optional (separator " "))
  (when string
    (multiple-value-bind (token rest)
        (string-until-chartruc string separator)
      (cons token (string-to-list1 rest separator)))))

(defun string-until-chartruc (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

(defun insert-alpha-space (strg)
(let (res) 
  (loop for char across strg
        do (if (alpha-char-p char) 
            (progn
              (push char res)
              (push #\Space res))
             (push char res)))
  (coerce (reverse res) 'string)))
  
(defun format-om-svg (strg)
"insert space and removes newlines from the om's export format. "
  (let (res res1) 
    (loop for char across strg
          do (if (alpha-char-p char) 
                 (progn
                   (push char res)
                   (push #\Space res))
               (push char res)
               ))
    (loop for char in res
          do (if (equal char #\Newline) (push #\Space res1) (push char res1))) 
    (coerce res1 'string)))


(defun remove-empty-str (data)
  (remove nil
          (loop for i in data
                collect (if (equal "" i) nil i))))


;to be cleaned up !!!!!
(defmethod* import-svg (&optional (path nil))
  :icon 659
  :indoc '("path" )
  :initvals '(nil nil)
  :doc "Imports lilypond format to OM
Note: Curves and arcs not supported yet!"

  (let ((file (catch-cancel (or path (om-choose-file-dialog)))))
    (when file
      (let* ((listfile (flat-once (om-list-from-xml-file file)))
             (paths (get-tagged-elements listfile 'path))
             (listgeo (car (om-list-from-xml-file file)))
             (viewbox (member ':|viewBox| listgeo))
             (h (if viewbox (second (last-n (str->list (second viewbox)) 2)) 0))
             (data
              (loop for i in (get-tagged-elements listfile 'path) 
                    collect (string-to-list
                             (format-om-svg (second (member ':\d  (flat i)))))))
             (data1
              (loop for i in data
                    collect (remove-empty-str i)))
             (pt (loop for dat in data1 
                       collect (loop for i in dat
                                     collect (remove nil (string-until-char2 i))
                                     )))
              
             (points
              (loop for i in pt
                    collect (svg-format-path i))
              )
          
             (bpcs
              (if viewbox 
                  (loop for i in points
                        collect (svg-render-bpc i h))
                (loop for i in points
                      collect  (svg-render-bpc (butlast (n-group-list (flat i) 2)) h)))))
         ; (print points)
        (make-instance 'bpc-lib
                       :bpf-list bpcs)
        ))))
             
