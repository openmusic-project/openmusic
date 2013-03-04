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

(defmethod! save-data ((self t) &optional (path "data.txt"))
  :icon 908
  :initvals '(nil "data.txt")
  :indoc '("data (list, BPF, or TextFile)" "a file location")
  :doc "Saves the data from <self> as a text file in <path>." 
  (let ((out (if (pathnamep path) path (outfile path))))
    (when (and (pathnamep out)
               (or (bpf-p self)
                   (typep self 'textfile)
                   (listp self)))
      (save-params self out)
      out)))



