;OpenMusic
;
;Copyright (C) 1997-2010 by IRCAM-Centre Georges Pompidou, Paris, France.
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
;Authors: Jean Bresson


(in-package :om)

;;;=================================================================================================
;;; WRITE FILE TOOLS
;;;=================================================================================================


(defmethod save-params ((self bpf) (file pathname))
  (with-open-file (out file :direction :output 
                         :if-does-not-exist :create :if-exists :supersede)
        (loop for x in (x-points self) 
              for y in (y-points self) do
              (format out "~D ~D~A" x y #\Linefeed))
        )
  file)

;;; ((x1 y1 ... yn)(x2 y2 ... yn) ...)
(defmethod save-params ((self list) (file pathname))
  (with-open-file (out file :direction :output 
                       :if-does-not-exist :create :if-exists :supersede)
    (loop for elt in self do
          (loop for p in elt do (if (integerp p) (format out "~d " p) (format out "~f " p)))
          (format out "~A" #\Linefeed))
    )
  file)


(defmethod save-params ((self textfile) (file pathname))
  (cond ((string-equal (eval-mode self) "text")
         (let ((parlist (list-of-lines (buffer-text self))))
           (with-open-file (out file :direction :output 
                                :if-does-not-exist :create :if-exists :supersede)
             (loop for line in parlist do
                   (format out "~A~%" line)))))
        ((string-equal (eval-mode self) "data")
         (let ((parlist (list-of-data (buffer-text self))))
           (with-open-file (out file :direction :output 
                                :if-does-not-exist :create :if-exists :supersede)
             (loop for line in parlist do
                   (loop for p in line do (format out "~D " p))
                   (format out "~A" #\Newline)))))
        ((string-equal (eval-mode self) "list")
         (let ((parlist (data-from-line (om-buffer-text (buffer-text self)))))
           (with-open-file (out file :direction :output 
                                :if-does-not-exist :create :if-exists :supersede)
             (loop for line in parlist do
                   (loop for p in line do (format out "~D " p))
                   (format out "~A" #\Newline)))))
        )
  file)

(defmethod save-params ((self t) (file string))
  (save-params self (pathname file)))

(defmethod! save-data ((self t) &optional (path nil))
  :icon 908
  :initvals '(nil "data.txt")
  :indoc '("data (list, BPF, or TextFile)" "a file location")
  :doc "Saves the data from <self> as a text file in <path>." 
  (let ((out (cond ((pathnamep path) path)
                   (path (outfile path))
                   (t (om-choose-new-file-dialog :directory (def-save-directory) 
                                                 :prompt "New Text file"
                                                 :types '("Text Files" "*.txt;*.*"))))))
    (when (and (pathnamep out)
               (or (bpf-p self)
                   (typep self 'textfile)
                   (listp self)))
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory out)))
      (save-params self out)
      out)))


;;;===============================================
;;; SVG export
;;;===============================================

(defmethod* export-svg ((self bpf) file-path &key with-points (w 300) (h 300) (margins 20) (line-size 1))
  :icon 908
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
      (let ((bpf-points (point-pairs (bpf-scale self :x1 margins :x2 (- w margins) :y2 margins :y1 (- h margins)))) ; y2 and y1 switched to have the correct orientation
            (scene (svg::make-svg-toplevel 'svg::svg-1.1-toplevel :height h :width w))
            (prev_p nil)
            (path (svg::make-path))
            (bpfcolorstr (format nil "rgb(~D, ~D, ~D)" 
                                 (round (* 255 (om-color-r (bpfcolor self))))
                                 (round (* 255 (om-color-g (bpfcolor self))))
                                 (round (* 255 (om-color-b (bpfcolor self)))))))
        ;draw line
        (loop for pt in bpf-points do
              (svg::with-path path
                (if prev_p
                    (svg::line-to (car pt) (cadr pt))
                  (svg::move-to (car pt) (cadr pt))))
              (setf prev_p pt))
        (svg::draw scene (:path :d path)
                   :fill "none" :stroke bpfcolorstr :stroke-width line-size)

        ;if points, draw points
         (when with-points
           (loop for pt in bpf-points do
                 (svg::draw scene (:circle :cx (car pt) :cy (cadr pt) :r (if (numberp with-points) with-points 2))
                            :stroke "rgb(0, 0, 0)"
                            :fill bpfcolorstr)))

        (with-open-file (s pathname :direction :output :if-exists :supersede)
          (svg::stream-out s scene)))
      pathname
      )))




