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
; Function reference generation
;DocFile


(in-package :om)

(export '(gen-lib-reference) :om)
         

;;; auto gen reference
(defparameter *om-ref-entries* nil)

(defvar *om-reference-dir* nil)

(defvar *om-ref-css* "")

(defvar *credit-line* nil)

; (gen-reference *om-ref-entries* *om-reference-dir*)


(defun special-path-check (name)
  (loop while (setf pos (position #\/ name)) do
        (replace name "I" :start1 pos))
  (loop while (setf pos (position #\\ name)) do
        (replace name "l" :start1 pos))
  (loop while (setf pos (position #\: name)) do
        (replace name "%" :start1 pos))
  (loop while (setf pos (position #\* name)) do
        (replace name "X" :start1 pos))
  (loop while (setf pos (position #\? name)) do
        (replace name "$" :start1 pos))
  (loop while (setf pos (position #\" name)) do
        (replace name "'" :start1 pos))
  (loop while (setf pos (position #\> name)) do
        (replace name ")" :start1 pos))
  (loop while (setf pos (position #\< name)) do
        (replace name "(" :start1 pos))
  (loop while (setf pos (position #\| name)) do
        (replace name "I" :start1 pos))
  name)

(defun special-html-check (name)
  (loop while (setf pos (position #\> name)) do
        (setf name (concatenate 'string (subseq name 0 pos)
                                "&gt;" (subseq name (+ pos 1)))))
  (loop while (setf pos (position #\< name)) do
        (setf name (concatenate 'string (subseq name 0 pos)
                                "&lt;" (subseq name (+ pos 1)))))
  name)


(defparameter *om-ref-text*
  "This is the reference documentation for the main functions and classes in OpenMusic. They correspond to the following OM packages:")

(defun gen-om-reference ()
  (gen-reference *om-ref-entries* *om-reference-dir*
                 :maintext *om-ref-text*))

; (gen-om-reference)

(defun gen-reference (entries dir &key title maintext)
  (when (probe-file dir) (om-delete-directory dir))
  (om-create-directory dir)
  (setf *credit-line* (concatenate 'string "<center><font size=-2>" "Auto doc generation by OpenMusic " *version-string* " © 2011 IRCAM"  "</font></center>"))
  (let ((title (or title (concatenate 'string "OM " *version-string*)))
        (indexpath (make-pathname :directory (pathname-directory dir)
                                  :name "index" :type "html"))
        (alphaindexpath (make-pathname :directory (pathname-directory dir)
                                  :name "ind-alpha" :type "html"))
        (allsymbols (remove nil (loop for pack in entries append
                                      (loop for group in (cadr pack) append (cadr group)))))
        )
    (with-open-file (index indexpath :direction :output)
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      (write-line "<body bgcolor=BBBBBB>" index)
      (write-line "<table align=center bgcolor=FFFFFF width=87%><tr>" index)
      (write-line "<td>" index)
      (write-line (concatenate 'string "<H1>" title "<br>Reference pages</H1>") index)
      (write-line "</td>" index)
      (write-line (concatenate 'string "<td width=120><img src=./omlogo.gif width=100 align=right>") index)
      (write-line "<br><br><p style=\"text-align: right;\"><a href=ind-alpha.html>Alphabetical Index</a></p>" index)
      (write-line  "</td></tr>" index)

      (write-line "<tr><td>" index)
      (when maintext
        (loop for par in (list! maintext) do
              (write-line "<p>" index)
              (write-line par index)
              (write-line "</p>" index)))
      (write-line "</td><td></td></tr>" index)
      (write-line "<tr><td colspan=2>" index)
      (write-line "<blockquote>" index)

      (loop for pack in entries do
            (let ((name (if (consp (car pack)) (first (car pack)) (car pack))))
              (when (and name (not (string-equal name "")))
                (write-line (concatenate 'string "- <a href=#" (string name) ">" (string-upcase (string name)) "</a><br>") index))))
      (write-line "</blockquote>" index)
      (loop for pack in entries do
            (let ((name (if (consp (car pack)) (first (car pack)) (car pack)))
                  (doc (if (consp (car pack)) (second (car pack)) nil)))
            (when name
              (write-line (concatenate 'string "<hr><a name=" (string name) ">" "<h2>" (string-upcase (string name)) "</h2></a>") index))
            (when doc
              (write-line (concatenate 'string "<p>" doc "</p>") index))
            (write-line "<blockquote>" index)
            (loop for group in (cadr pack) do
                  (let ((n (if (consp (car group)) (first (car group)) (car group)))
                        (d (if (consp (car group)) (second (car group)) nil)))
                  (when n
                    (write-line (concatenate 'string "<h3>" (string n) "</h3>") index))
                  (when d
                    (write-line (concatenate 'string "<p>" (string d) "</p>") index))
                  (write-line "<blockquote>" index)
                  (loop for item in (cadr group) do
                        (write-line (concatenate 'string "<a href=" 
                                                 (special-path-check
                                                  (string-downcase (string item))) ".html>" 
                                                 (special-html-check (string item)) "</a><br>") index)
                        )
                  (write-line "</blockquote>" index)
                  ))
            (write-line "</blockquote>" index)
            (write-line "<br><br>" index)
            ))
      (write-line "<br><br></td></tr><td colspan=2 class=center>" index)
      (write-line *credit-line* index)
      (write-line "</td></tr></table>" index)
      (write-line "</body></html>" index))
    (with-open-file (index alphaindexpath :direction :output)
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" title " Reference Index</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      (write-line "<body bgcolor=BBBBBB>" index)
      (write-line "<table align=center bgcolor=FFFFFF width=87%><tr>" index)
      (write-line "<td>" index)
      (write-line "<H1>Alphabetical Index</H1>" index)
      (write-line (concatenate 'string "<font size=-2>Generated from the <a href=index.html>" title " Reference</a></font><br><br>") index)
      (write-line "<blockquote>" index)
      (mapcar #'(lambda (item) (write-line (concatenate 'string "<a href=" (special-path-check
                                                                               (string-downcase (string item))) ".html>" 
                                                        (string item) "</a><br>") index))
              (sort allsymbols 'string<))
      (write-line "</blockquote>" index)
      (write-line "<br><br></td></tr><td class=center>" index)
      (write-line *credit-line* index)
      (write-line "</td></tr></table>" index)
      (write-line "</body></html>" index))
    (mapcar #'(lambda (symb) (make-ref-page symb dir title)) allsymbols)
    (let ((logopict (make-pathname :directory (pathname-directory *om-pict-folder*) :name "omlogo" :type "gif")))
      (when (probe-file logopict)
        (om-copy-file logopict
                      (make-pathname :directory (pathname-directory dir) :name "omlogo" :type "gif"))))
    indexpath))


(defun make-ref-page (symbol dir &optional title)
  (let ((title (or title ""))
        (pagepath (make-pathname :directory (pathname-directory dir)
                                 :name (special-path-check
                                        (string-downcase (string symbol)))
                                 :type "html"))
        (doc (om-get-documentation symbol)))
    (with-open-file (index pagepath :direction :output :if-exists :supersede)
      (write-line "<html>" index)
      (write-line (concatenate 'string "<head><title>" (special-html-check (string symbol)) " - " title " Reference Documentation</title>") index)
      (write-line *om-ref-css* index)
      (write-line "</head>" index)
      (write-line "<body bgcolor=BBBBBB>" index)
      (write-line "<br>" index)
      (write-line "<table align=center bgcolor=FFFFFF width=87%><tr>" index)
      (write-line "<td>" index)
      (write-line (concatenate 'string "<h2>" (special-html-check (string symbol)) "</h2>") index)
      
      (unless (null doc)
        (write-line (concatenate 'string "<font size=-1>[" (string (nth 1 doc)) "]" "</font><br><br>") index))
      (write-line "</td><td width=130>" index)
      (let ((icn nil))
        (cond ((and (find-class symbol nil) (omclass-p (find-class symbol nil)))
               (setf icn (icon (find-class symbol nil))))
              ((and (fboundp symbol) (omgenfun-p (fdefinition symbol)))
               (setf icn (icon (fdefinition symbol))))
              (t nil))
        (write-line (concatenate 'string "<img src=./omlogo.gif width=100><br>") index)
        (write-line (concatenate 'string "<font size=-2 align=right><a href=index.html>" title "<br> Function Reference</a></font>") index)
        )
      ; do not work with TIFF images !
        ;(when icn 
        ;  (write-line (concatenate 'string "<img src=" 
        ;                           (namestring (om-get-resource-file (integer-to-string 127) *om-icon-folder* *om-icon-type*))
        ;                           " width=30 align=right>")
        ;              index))
      
      (write-line "</td></tr>" index)
      (write-line "<tr><td colspan=2>" index)
      
      (if (null doc)
          (write-line "<p>No documentation</p></td>" index)
        (progn
          (cond ((or (equal (nth 1 doc) 'FUNCTION)
                     (equal (nth 1 doc) 'GENERIC-FUNCTION))
                 (write-line "<font color=882222><b>ARGUMENTS:</b></font><br>" index)
                 (if (null (nth 2 doc))
                     (write-line "None<br><br>" index)
                   (progn
                     (write-line "<table width=100% border=0>" index)
                     (loop for arg in (nth 2 doc) 
                         for i = 0 then (+ i 1) do
                         (write-line "<tr>" index)
                         (if (and (not (consp arg)) (equal #\& (elt (string arg) 0)))
                             (progn 
                               (write-line "<td>" index)
                               (write-line (concatenate 'string "<i><b>" (string-downcase (string arg))"</b></i><br>") index)
                               (write-line "</td>" index)
                               (write-line "<td colspan=3>&nbsp;</td></tr>" index)
                               (setf i (- i 1)))
                           (let ((argname (if (consp arg) (car arg) arg)))
                             (write-line "<td width=40>&nbsp;</td>" index) 
                             (write-line "<td>" index)
                             (write-line (concatenate 'string "- <font color=333366><b>" (format nil "~s" (intern (string-upcase argname))) "</b></font>") index)
                             (write-line "</td>" index)
                             ;; doc and defaults
                             (if (subtypep (class-of (fdefinition symbol)) 'OMGenericFunction)
                                 ; OM FUN
                                 (let ((indoc (nth i (inputs-doc (fdefinition symbol))))
                                       (defval (nth i (inputs-default (fdefinition symbol)))))
                                   (if indoc
                                       (write-line (concatenate 'string "<td>" indoc "</td>") index)
                                     (write-line "<td>&nbsp;</td>" index))

                                   ;(format nil "~s" (quote k1)) (gen-lib-reference (find-library "OM2CSound"))
                                   (if defval
                                       (write-line (concatenate 'string "<td>[default = " (format nil "~s" defval) "]</td>") index)
                                     (write-line "<td>&nbsp;</td>" index))
                                   )
                               (progn
                                 (write-line "<td width=20%>&nbsp;</td>" index)
                                 (if (consp arg)
                                     (write-line (concatenate 'string 
                                                              "<td>[default = " 
                                                              (format nil "~s" (cadr arg)) "]</td>")
                                                 index)
                                   (write-line "<td>&nbsp;</td>" index))
                                 )
                               )
                             (write-line "</tr>" index)
                             ))
                         (write-line "<tr>" index))
                     (write-line "</table>" index)
                     )))
                ((equal (nth 1 doc) 'CLASS)
                 (write-line "<font color=882222><b>SLOTS:</b></font><br><br>" index)
                 (if (null (nth 2 doc))
                     (write-line "None<br><br>" index)
                   (progn
                     (write-line "<table width=100% border=0>" index)
                     (loop for slot in (nth 2 doc) do
                           (write-line "<tr>" index)
                           (if (or (null (cadr slot)) (string-equal (cadr slot) "No documentation"))
                             ;(and (subtypep (class-of (find-class symbol nil)) 'OMClass)
                             ;     (nth i (slot-docs (find-class symbol nil)))
                             ;     (not (string-equal (cadr (nth i (slot-docs (find-class symbol nil)))) "No documentation")))
                             (write-line (concatenate 'string "<td colspan=2> - " (string-upcase (string (car slot))) "</td>>") index)
                           (write-line (concatenate 'string "<td width=160> - <font color=333366><b>" (string-upcase (string (car slot))) "</font></b>"
                                                    " :</td><td>" (cadr slot) "</td>") index))
                           (write-line "</tr>" index)
                           )
                     (write-line "</table>" index))
                 ))
          (write-line "</td>" index))))
      (write-line "</tr>" index)
      (write-line "<tr><td colspan=2>" index)
      (when doc
        (write-line "<br><font color=882222><b>Description:</b></font><p>" index)
        (loop for str in (om-text-to-lines (nth 3 doc)) do
              (write-line (concatenate 'string "" (special-html-check str) "<br>") index))
        )
      
      (write-line "</p><br><br></td></tr>" index)
      (write-line "<td class=center colspan=2>" index)
      (write-line *credit-line* index)
      (write-line "</td></tr></table>" index)
      
      ;(write-line "<p class=center><a href=index.html>Back</a></p>" index)  
      (write-line "</body></html>" index))
    pagepath))
      
; (gen-reference *om-ref-entries* *om-reference-dir*)

(setf *om-ref-css* 
      "<STYLE TYPE=\"text/css\"><!--  

BODY {background-color: #EEEEEE;
	  font-family: Verdana;
font-size: 12;
color : #0000000;
}

A:link {text-decoration: underline; color : 333366;}
A:visited {text-decoration: underline; color : 333366;}
A:active {Verdana; text-decoration: underline; color:333366;}
A:hover {Verdana; text-decoration: underline; color: 333366;}

H1 {
 color : #333333;
 font-size: 18;
 font-weight: bold
 text-align: left;
}
	
H2 {
 color : #333333;
 font-size: 16;
 font-weight: bold;
}

H3 {
 color : #444444;
 font-size: 12;
 font-weight: bold;
}

P,UL,LI, TD {	
 font-size: 11;
 text-indent: 0cm;
 text-align: justify;
}
.right {text-align: right ;}
.center {text-align: center ;}
.top {text-align: top ;}
   --></style>")


(defun refsection-equal (s1 s2)
  (let ((n1 (if (consp s1) (car s1) s1))
        (n2 (if (consp s2) (car s2) s2)))
    (string-equal n1 n2)))
        

(defun add-ref-section (section)
  (let ((pos (position (car section) *om-ref-entries* :test 'refsection-equal :key 'car)))
    (if (null pos)
        (setf *om-ref-entries* (append *om-ref-entries* (list section)))
      (let ((existing-section (cadr (nth pos *om-ref-entries*))))
        ;;; subsections
        (loop for item in (cadr section) do
              (let ((pos2 (position (car item) existing-section :test 'refsection-equal :key 'car)))
                  (if (null pos2)
                    (setf existing-section (append existing-section (list item)))
                  (setf (nth pos2 existing-section)
                        (list (car (nth pos2 existing-section)) (remove-duplicates (append (cadr (nth pos2 existing-section)) (cadr item)))))
                )
              ))
        ;;;
        (setf (nth pos *om-ref-entries*)
              (list (car (nth pos *om-ref-entries*)) existing-section)))
      )))
   
       
(defun set-ref-dir ()
  (setf *om-reference-dir* 
        (make-pathname :directory (append (pathname-directory *om-resources-folder*) '("reference")))))

(om-add-init-func 'set-ref-dir)


;;; AuTO GEN ENTRIES

(defun gen-ref-entries (pack)
  (list (list (name pack) (doc pack))     
              (cons
               (list "" (remove nil (remove-duplicates (append (mapcar 'class-name (classes pack)) (mapcar 'function-name (functions pack))) :test 'string-equal)))
               (remove nil (loop for pack in (subpackages pack) append (gen-subpack-entries pack)))
               )))

(defun gen-subpack-entries (pack &key exclude-packages)
  (unless (find (name pack) exclude-packages :test 'string-equal)
    (let ((packentries (remove nil (remove-duplicates (append (mapcar 'class-name (classes pack)) (mapcar 'function-name (functions pack))) :test 'string-equal))))
      (remove nil (cons
                   (if packentries (list (list (name pack) (doc pack)) packentries))
                   (loop for pk in (subpackages pack) append (gen-subpack-entries pk :exclude-packages exclude-packages))
                   )))))

; (gen-om-reference)


;;; USER LIBRARIES

(defun lib-ref-path (lib)
  (make-pathname :directory (append (pathname-directory (lib-pathname lib)) '("resources" "reference"))
                 :host (pathname-host (lib-pathname lib)) :device (pathname-device (lib-pathname lib))))
  
(defmethod lib-ref-location ((self omclass))
  (let* ((libname (lib-class-p self))
        (lib (and libname (exist-lib-p libname))))
    (when lib (lib-ref-path lib))))

(defmethod lib-ref-location ((self omgenericfunction))
  (let* ((libname (lib-fun-p self))
         (lib (and libname (exist-lib-p libname))))
    (when lib (lib-ref-path lib))))

(defun gen-lib-entries (lib &key exclude-packages)
  (list (list "" 
              (cons
               (list "" (remove nil (append  (mapcar 'class-name (classes lib)) (mapcar 'function-name (functions lib)))))
               (remove nil (loop for pack in (subpackages lib) 
                                 append (gen-subpack-entries pack :exclude-packages exclude-packages)))
               ))))


(defun gen-lib-reference (&optional lib)
  (let ((library (if (stringp lib) (find-library lib)
                   (or lib *current-lib*))))
    (when library
      (let ((ref-function (interne (string+ (name library) "-reference"))))
        (if (fboundp ref-function)
            (funcall ref-function)
          (gen-reference (gen-lib-entries library)
                         (lib-ref-path library) 
                         :title (name library)
                         :maintext (doc library)))))))



;(gen-lib-entries (exist-lib-p "esquisse"))


