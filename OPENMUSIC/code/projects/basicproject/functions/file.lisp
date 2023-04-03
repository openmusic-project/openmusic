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


(in-package :om)


;;;=================================================================================================
;;; TEXT UTILITIES
;;;=================================================================================================



(defmethod! string->ascii ((text t))

   :initvals '("toto")
   :indoc '("A Textfile" )
   :icon '(141) 
   :doc  "Translates strings into equivalent ascii integers"

(let ((char-text (coerce text 'list)))
  (mapcar #'(lambda (x) (char-code (character x)))
          char-text)))




(defmethod! ascii->string ((asciilist list))
   :initvals '( '(112))
   :indoc '("list of ascii numbers" )
   :icon '(141) 
   :doc  "Translates ascii integers into equivalent strings"
  (let ((aString (make-string  (length asciilist))))
    (loop for ascii in asciilist
          for index from 0 do
          (setf (elt aString index) (code-char ascii)))
    astring))




;;;=================================================================================================
;;; WRITE FILE TOOLS
;;;=================================================================================================


(defmethod save-params ((self bpf) (file pathname))
  (with-open-file (out file :direction :output 
                         :if-does-not-exist :create :if-exists :supersede)
        (loop for x in (x-points self) 
              for y in (y-points self) do
              (if (integerp x) (format out "~D " x) (format out "~f " x))
              (if (integerp y) (format out "~D" y) (format out "~f" y))
              (format out "~A" #\Linefeed))
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


