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
;This File contains generals functions and macros.
;In particular this file implements a set of Le Lisp Macros.
;Last Modifications :
;17/11/97 first date.
;DocFile

(in-package :om)

(export '(list!
          string+
          list+
          firstn
          replace-in-list
          insert-in-list
          erase-n
          string-to-number
          number-to-string

          string-until-CR
          string-until-space
          string-from-space
          delete-spaces
          string-until-char

          om-relative-path

          interne
          internp

          *om-version*
          om-message-abort
          om-beep-msg
          om-abort
          om-quit
          om-print) :om)

;----------------------
; VARS
;----------------------

(defvar *om-version* 6 "The current version of OM")
(defvar *version-string* "6")
(defvar *app-name* "OpenMusic")

(setf *om-version* *version*)
(setf *version-string* *version-str*)


;; updated in build file

(defvar *let-list* nil "This var is a list for the boxes in mode ev-once, it is used while code generation")

(defvar *om-current-file-version*) ;non used, only for correction with first version (before 2.03)

(defvar *music-font* "omicron")

(defvar *micron-font* "omicron")
(defvar *extras-font* "omextras")
(defvar *heads-font* "omheads")
(defvar *signs-font* "omsign")


;----------------------
;UTILITIES
;-----------------------

(defmethod name ((self string)) self)
(defun first?  (thing) (if (listp thing) (first thing) thing))
(defun list!   (thing) (if (listp thing) thing (list thing)))

(defun string+ (&rest strings) (eval `(concatenate 'string ,.strings)))
(defun list+ (&rest lists) (apply 'concatenate (append (list 'list) lists)))

(defun firstn (list  n )
   (cond
    ((< (length list)  n) list )
    (t  (butlast  list (- (length list) n)))))

(defun replace-in-list (list elem pos)
   (let* ((first-list (subseq list 0 pos))
          (second-list (subseq list (+ pos 1))))
     (append first-list (list elem) second-list)))

(defun insert-in-list (list elem pos)
  (if (> pos (- (length list) 1))
    (append list (list elem))
    (let* ((first-list (subseq list 0 pos))
           (second-list (subseq list pos)))
      (append first-list (list elem) second-list))))

(defun erase-n (list pos)
   (loop for item in list
         for i = 0 then (+ i 1)
         when (not (= i pos)) collect item))

;; pos = a list of indices
(defun erase-nn (list pos)
   (loop for item in list
         for i = 0 then (+ i 1)
         when (not (member i pos)) collect item))
  
(defun omlistp (list)
  (and list (listp list) (or (not (symbolp (car list))) (not (fboundp (car list))))))

(defun valued-val (val)
   (if (or (symbolp val) (omlistp val)) val (eval val)))


(defun string-to-number (string)
  (and (stringp string) (read-from-string string)))

(defun number-to-string (int)
   (format nil "~D" int))

(defun integer-to-string (int)
   (format nil "~D" int))


;-----------------------
; POSITION
;-----------------------

(defun interval-intersec (int1 int2)
   (when (and int2 int1)
     (let ((x1 (max (car int1) (car int2)))
           (x2 (min (cadr int1) (cadr int2))))
       (if (<= x1 x2)
         (list x1 x2)))))

(defun point-in-interval (point interval)
   (and (<= point (second interval))
            (>= point (first interval))))


(defun point-in-rectangle-p (point top left bottom right)
   (let ((rect (om-make-rect left top right bottom ))
           rep)
       (setf rep (om-point-in-rect-p point rect))
     rep))


(defun borne-position (pos)
   (om-make-point (max 0 (om-point-h pos)) (max 0 (om-point-v pos))))

(defun point2list (pt)
  (list (om-point-h pt) (om-point-v pt)))

(defun print-point (pt)
  (if pt
      (print (point2list pt))
    (print "pt = NIL"))
  pt)


;----------------------------------------------------
; PARAMETERS
;----------------------------------------------------

(defun get-fixed-par-num (lis funname)
   (let ((j 0))
     (loop while lis do
           (let ((var (pop lis)))
             (if (not (member var lambda-list-keywords :test 'equal))
               (incf j)))) 
     (- j (length (get-keywords-fun funname)))))

(defun min-inp-number-from-arglist (lis)
   (let ((i 0) (continue t))
     (when lis
       (loop while continue do
             (let ((var (nth i lis)))
               (if (member var lambda-list-keywords :test 'equal)
                 (setf continue nil)
                 (incf i))
               (if (= i (length lis))
                 (setf continue nil)))))
     i))

(defun ordered-arg-list (fun)
  (remove nil 
          (loop for item in (arglist fun)
                collect (when (not (member item lambda-list-keywords :test 'equal)) item))))



(defun min-inp-number (fun)
   (min-inp-number-from-arglist (arglist fun)))


(defun get-keywords-fun (fun)
  (let ((args (arglist fun))
        rep)
    (loop while (and args (not rep)) do
          (when (equal (pop args) '&key)
            (setf rep t)))
    (setf rep nil)
    (loop for item in args 
          while (not (member item lambda-list-keywords :test 'equal)) do
          (push (if (listp item) (car item) item) rep))
    (reverse rep)))

(defun get-optional-fun (fun)
 (let ((args (arglist fun))
       rep)
   (loop while (and args (not rep)) do
         (when (equal (pop args) '&optional)
           (setf rep t)))
   (setf rep nil)
   (loop for item in args
         while (not (member item lambda-list-keywords :test 'equal)) do
         (push (if (listp item) (car item) item) rep))
   (reverse rep)))

;-------------------------------------
; OM files 
;-------------------------------------
;this function return a string that is not contained in the list list
;list is a list of object which have a slot name
; mode num = toujours commencer avec un numero
(defun unique-name-from-list (name list &key (mode nil))
   (let* ((nompot name)
          (i 0)
          (seguir (if (equal mode :num) t (match-name1 list nompot))))
     (loop while seguir do
           (incf i)
           (setf nompot (string+ name (format nil "~D" i)))
           (setf seguir (match-name1 list nompot)))
     nompot))

(defun match-name1 (list name)
   (let* ((next t))
     (loop while (and list next) do
           (if (string-equal name (name (pop list)))
             (setf next nil)))
     (if next nil t)))

(defun correct-name (string)
  (reverse (delete-spaces (reverse (delete-spaces string)))))

(defun unique-name-from-list-new (name list &key (mode nil) (space t))
  (let* ((correct-name (correct-name name))
         (last-item (and (search " " correct-name)
                         (read-from-string (reverse (string-until-char (reverse correct-name) " ")))))
         (number (if (integerp last-item) last-item nil))
         (main-name (if number (subseq correct-name 0 (- (length correct-name) (+ 1 (length (integer-to-string number))))) correct-name))
         (nompot (if (equal mode :num) (string+ main-name (if space " 1" "1")) main-name))
         (i 1))
    (loop while (find nompot list :test 'string-equal :key #'(lambda (obj) (correct-name (name obj)))) do
             (incf i)
             (setf nompot (string+ main-name (if space (format nil " ~D" i) (format nil "~D" i)))))
       nompot))



(defun names-from-list (list)
  (loop for item in list collect (name item)))

(defun uniqueNameinpatch (name patch)
   (unique-name-from-list name  (get-elements patch)))



;(delete-package "Pareto")


; load a non text file : maquette, patch, ...
(defun eval-non-text-file (path)
  (declare (special *current-workSpace*))
  (if (probe-file path) 
      (let* ((condition nil)
             (err (catch 'read-error 
                   (handler-bind ((reader-error #'(lambda (c) 
                                                    (setf condition c)
                                                    (print (om-report-condition c))
                                                    (throw 'read-error (type-of c)))))
                     
                     (om-load-file path)
                     ))))
        (or (and (pathnamep err) err)
            (cond ((string-equal (symbol-name err) "package-not-found-reader")
               (let ((packname (reverse (subseq (string-until-space (reverse (om-report-condition condition))) 1))))
                 (if (om-y-or-n-dialog (format nil "Error in ~s:~%~% Package ~A not found. ~%~%Create the package ?" 
                                               (namestring path)
                                               packname))
                     (progn 
                       (eval `(defpackage ,packname))
                       (om-load-file path))
                   (om-abort)
                   )))
              (t 
               (if (om-y-or-n-dialog (format nil  "File reader error in ~s:~%~A~%~%    Try to restore the file?" 
                                             (namestring path)
                                             (om-report-condition condition)))
                   (if (repair-old-file path)
                       (om-load-file path)
                     (progn 
                       (om-message-dialog (string+ "Sorry, the file " (namestring path) " could not be restored"))
                       (om-abort)
                       ))
                 (om-abort)))
              
              )
            ))
     (if (si-o-no (format nil "File ~s not found.~%Do you want to look for it?" (namestring path)) nil)
         (let ((name (om-choose-file-dialog :prompt (string+ "Look for file " (string (real-pathname-name path)))
                                            :directory (make-pathname :directory
                                                                      (append (pathname-directory (mypathname *current-workSpace*))
                                                                              (list "elements"))))))
           (when name
             (eval-non-text-file name))))))

(defun repair-old-file (pathname)
  (when (probe-file pathname)
    (let ((linelist nil))
      (with-open-file (file pathname :direction :input)
        (let ((line (read-line file nil :eof)))
          (loop while (not (equal line :eof)) do 
                (setf linelist (append linelist (list line)))
                (setf line (read-line file nil :eof)))))
      (with-open-file (file pathname :direction :output :if-exists :supersede)
        (setf line (pop linelist))
        (loop while (and line (not (equal line :eof))) do
              (write-line (str-check line) file)
              (setf line (pop linelist))
              ))
      )
    t))

;--------------------------------------------------------------
; SPECIAL MESSAGES
;--------------------------------------------------------------
(defun dialog-message (message) 
   (declare (special *receiving-in-drag*)) 
   (if  *receiving-in-drag*
     (not (om-beep-msg message))
     (om-message-dialog message)))

;special yes-or-not dialog, you can not show a dialog while a drag&drop operation
(defun si-o-no (message &optional (rep t) &key (cancel nil))
   (declare (special *receiving-in-drag*))
   (if  *receiving-in-drag*
     (progn
       (om-beep-msg message) rep)
     (if cancel 
         (om-y-n-cancel-dialog message :default-button :yes)
       (om-y-or-n-dialog message :default-button :yes))))

(defun om-y-or-n-option-dialog (message option-message &optional (default :yes) (option nil) (title "") (size 150))
  (let ((win (om-make-window 'om-dialog :position :centered :size (om-make-point 310 (+ size 50))
                             :resizable nil :maximize nil :minimize nil
                             :bg-color *om-window-def-color* :window-title title))
        (text (om-make-dialog-item 'om-static-text (om-make-point 20 20) (om-make-point 270 (- size 70))
                                   message
                                   :font *controls-font*
                                   ))
        (box (om-make-dialog-item 'om-check-box (om-make-point 20 (- size 40)) (om-make-point 270 20)
                                  option-message
                                  :font *controls-font*
                                  :checked-p option)))
    (om-add-subviews win text box
                     (om-make-dialog-item 'om-button (om-make-point 134 size) (om-make-point 80 20) (om-str :no)
                                          :default-button (equal default :no)
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog win (list nil (om-checked-p box)))))
                     (om-make-dialog-item 'om-button (om-make-point 216 size) (om-make-point 80 20) (om-str :yes)
                                          :default-button (equal default :yes)
                                          :di-action (om-dialog-item-act item
                                                       (om-return-from-modal-dialog win (list t (om-checked-p box)))))
                     )
    (om-modal-dialog win (om-front-window))))



(defun om-message-abort (message)
  (om-message-dialog message)
  (om-abort))

(defun om-beep-msg (string)
   (om-beep)
   (print string) nil)


;----------------------------------------------------
; Boxes info
;----------------------------------------------------
;Find all elements in the list <lis> which their class is <class>
(defun find-class-boxes (lis class)
   (let* (rep)
     (mapc #'(lambda (box)
               (if (equal (class-name (class-of box))  class)
                 (push box rep))) lis)
     (reverse rep)))

(defun find-condition-boxes (lis fun)
   (let* (rep)
     (mapc #'(lambda (box)
               (when (funcall fun box)
                 (push box rep))) lis)
     (reverse rep)))


(defun default-from-list (list)
   (cond
    ((null list) 0)
    ((= (length list) 1) (car list))
    (t (- (car (last list 1)) (car (last list 2))))))


;-----------------------------------------------------------
;SubStrings
;-----------------------------------------------------------
;Left prefix of a string upto a CR or the end if no
(defun string-until-CR (string)
  (let ((index (search (format nil "~%") string)))
    (if index (subseq string 0 index) string)))

(defun string-until-space (string)
   (if (or (string-equal string "OMsituation2.01") (string-equal string "OMsituation3.0"))
     "OMSituation"
     (let ((index (search " " string)))
       (if index (subseq string 0 index) string))))

(defun string-from-space (string)
   (let ((index (search " " string)))
     (if index (subseq string (1+ index)) string)))

(defun delete-spaces (string)
   (let ((pos (position-if #'(lambda (x) (and 
                                          (not (equal x #\Linefeed))   ;;; test jean
                                          (not (equal x #\Space))
                                          (not (equal x #\Tab)))) string)))
     (if pos
       (subseq string pos)
       "")))

(defun good-text-box-size (text font)
  (om-add-points (get-good-size text font)
                 (om-make-point 4 4)))

(defun get-good-size (text font)
  (let ((strlist (om-text-to-lines text)))
    (om-make-point 
     (om-round (+ 16 (reduce 'max (mapcar #'(lambda (line) (om-string-size line font)) strlist))))
     (* (om-string-h font) (+ 1 (length strlist))))))

;-----------------------------------------------------------
; folders and files 
;-----------------------------------------------------------

(defun string-until-char (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

(defun flat-string (string)
  (substitute #\Space #\Newline string)) 

(defun str2list-path (str)
  (let (list)
    (loop while str do
          (let ((rep (multiple-value-list (string-until-char str ";"))))
            (setf str (second rep))
            (when (first rep) (push (first rep) list))))
    (reverse list)))

;1 ---------add om root to str pathname
(defun OMroot (str)
  (let* ((list (str2list-path str))
         (name (car (last list))))
    (if (equal name "")
      (om-make-pathname :device *om-root* :directory (append (pathname-directory *om-root*) (butlast list)))
      (om-make-pathname :device *om-root* :directory (append (pathname-directory *om-root*) (butlast list)) :name name))))


;3 ---------change pathname file in pathne folder (if index is given rename the path with index)
(defun OMfile2folder (path &optional index)
  (if index
    (make-pathname :directory (append (pathname-directory path) (list (string+ (pathname-name path) (format nil "~D" index)))))
    (make-pathname :directory (append (pathname-directory path) (list (pathname-name path))))))

;4 ---------name + type and other special chars

;; --> om-namestring ajoute
;(defun real-pathname-name (path)
;  (let ((type (pathname-type path)))
;    (if (or (equal type :UNSPECIFIC) (not type))
;      (om-namestring (pathname-name path))
;      (string+ (om-namestring (pathname-name path)) "." type)
;      )))

(defun real-pathname-name (path)
  (om-namestring (pathname-name path)))


(defun copy-folder (srcpath targetpath)
  (om-create-directory targetpath :if-exists :supersede)
  (loop for item in (om-directory srcpath :files t :directories t) do
        (if (directoryp item)
          (copy-folder item (make-pathname :device (pathname-device targetpath) 
                                           :directory (append (pathname-directory targetpath) (last (pathname-directory item)))))
          (om-copy-file item (make-pathname :device (pathname-device targetpath) 
                                            :directory (pathname-directory targetpath)
                                            :name (pathname-name item) :type (pathname-type item))))))


(defvar *om-doc-types* '("omp" "omm" "omc" "omi" "ome" "she"))

(defun om-persistant-p (path)
  (or (directoryp path) 
      (member (pathname-type path) *om-doc-types* :test 'string-equal)))

;TOOLS
;---------name of a directory
(defun name-of-directory (path)
  (car (last (pathname-directory path))))

(defun get-filename (p)
  (let ((path (and p (pathname p))))
  (when (pathnamep path)
    (string+ (pathname-name path) 
             (if (and (pathname-type path) (stringp (pathname-type path)))
                 (string+ "." (pathname-type path)) 
               "")))))

;;;===============================================================================

(defun copy-file-sp (source-path target-path)
   (om-copy-file source-path
                 target-path
                 :if-exists :supersede))

(defun copy-file-in-dir (file dir)
  (om-copy-file file 
                (make-pathname :device (pathname-device dir)
                               :host (pathname-host dir)
                               :directory (pathname-directory dir)
                               :name (pathname-name file)
                               :type (pathname-type file))))

(defvar *delete-file* t)

(defun delete-file-protection (path)
   (declare (special *delete-file*))
   (when (and *delete-file* (probe-file path))
     (om-delete-file path)))
 
(defun corrige-path-space (name) name)

(defun om-relative-path (dirs file)
  (make-pathname
   :host (pathname-host *load-pathname*) :device (pathname-device *load-pathname*) 
   :directory (append (pathname-directory *load-pathname*) dirs)
   :name file))

(defvar *last-file-loaded-dir* nil)

;(let ((*relative-path-reference* "/Users/bresson/WORKSPACES/mk-examples.omp"))
;  (restore-path "../../../../../../test/ist/ooo.omp")
;  )
; (pathname-directory "../../../test/ist/ooo.omp")


(defmethod restore-path ((self pathname))
  (let ((dir (pathname-directory self))
        (refpath (relative-path-reference)))
    ;;(print (list dir refpath))
     (if refpath
        (cond ((and (equal :relative (car dir)) (cdr dir))
               (let ((updirs (or (position-if-not #'(lambda (item) (or (equal item :up)(equal item :back))) (cdr dir)) 0)))
                 (make-pathname 
                  :device (pathname-device refpath) :host (pathname-host refpath)
                  :directory (append (list (car (pathname-directory refpath)))
                                     (butlast (cdr (pathname-directory refpath)) updirs) 
                                     (nthcdr updirs (cdr dir)))
                  :name (pathname-name self) :type (pathname-type self))))
              ((equal :absolute (car dir)) self)
              ((or (null dir) (null (cdr dir)))
               ;; could not restore pathname
               (make-pathname 
                :device (pathname-device refpath) :host (pathname-host refpath)
                :directory (pathname-directory refpath)
                :name (pathname-name self) :type (pathname-type self))))
      self)
    ))

(defmethod restore-path ((self string))
  (restore-path (pathname self)))

(defmethod restore-path ((self t)) nil)


;(setf p1 #P"/Users/bresson/WORKSPACES/aaaa/elements/mk-examples.omp")
;(setf p2 #P"/Users/bresson/WORKSPACES/aaaa/elements/NewFolder/bouches/piece1.omp")
;(setf p3 #P"/Users/bresson/WORKSPACES/infiles/test.aif")
;(relative-pathname p3 p1)

(defun relative-pathname (path refpath)
  (let ((dirlist '(:relative))
        (refrest (cdr (pathname-directory refpath)))
        (dirrest (cdr (pathname-directory path))))
    (loop for path-dir in (cdr (pathname-directory path))
          for ref-dir in (cdr (pathname-directory refpath)) 
          while (string-equal path-dir ref-dir) do
          (setf refrest (cdr refrest)
                dirrest (cdr dirrest)))
    (loop for item in refrest do
          (setf dirlist (append dirlist (list ".."))))
    (loop for item in dirrest do
          (setf dirlist (append dirlist (list item))))
    (make-pathname :device (pathname-device refpath) :host (pathname-host refpath)
     :directory dirlist :name (pathname-name path) :type (pathname-type path))
    ))


;; reinitialized in load-patch
(defparameter *file-search-all* nil)



; (load-sound "G4 HD2:archives sonores:instr mono:poly2a")

(defun search-file-for-load (pathname)
  (let ((restored-pathname (restore-path pathname)))
    (when restored-pathname
      (or (probe-file restored-pathname)
          (and *last-file-loaded-dir*
               (probe-file (make-pathname :directory (pathname-directory *last-file-loaded-dir*)
                                          :device (pathname-device *last-file-loaded-dir*)
                                          :host (pathname-host *last-file-loaded-dir*)
                                          :name (pathname-name restored-pathname) :type (pathname-type restored-pathname))))
          (and *om-infiles-folder*
               (probe-file (make-pathname :directory (pathname-directory *om-infiles-folder*)
                                          :device (pathname-device *om-infiles-folder*)
                                          :host (pathname-host *om-infiles-folder*)
                                          :name (pathname-name restored-pathname) :type (pathname-type restored-pathname))))
          (and *relative-path-reference*
               (probe-file (make-pathname :directory (pathname-directory *relative-path-reference*)
                                          :device (pathname-device *relative-path-reference*)
                                          :host (pathname-host *relative-path-reference*)
                                          :name (pathname-name restored-pathname) :type (pathname-type restored-pathname))))
          )
      )))
  
(defun om-load-if (pathname fun)
  (ignore-errors 
  (let ((load-path (search-file-for-load pathname)))
    (if load-path              
        (funcall fun load-path)
      (unless (and *file-search-all* (= *file-search-all* 0))
        (when (or *file-search-all* 
                  (let ((rep (om-y-or-n-option-dialog 
                              (format nil "~a.~%~a" (format nil (om-str :file-not-found) (namestring name)) (om-str :look?))
                              (om-str :apply-all-lost) :no t)))
                    (when (cadr rep) (setf *file-search-all* (if (car rep) 1 0)))
                    (car rep)))
          (let ((name2 (om-choose-file-dialog :prompt (string+ (om-str :lookingfor) " \"" 
                                                               (pathname-name name) "." (pathname-type name) "\"")
                                              :button-string "OK")))
            (when name2
              (progn
                (setf *last-file-loaded-dir* (make-pathname :directory (pathname-directory name2)
                                                            :device (pathname-device name2) :host (pathname-host name2)))
                (funcall fun name2))))))))))


(defun check-folder (path)
  (unless (probe-file path) (om-create-directory path :if-exists nil))
  path)



(defun load-files (folder mytype &optional subfolder?)
"
Load all the files of type mytype contained in folder adding the type mytype.
If subfolder is active, look for the files in all the subfolders as well.
"
  (loop for els in (om-directory folder :files t :directories t) do
         (if (directoryp els)
           (when subfolder?
             (load-files els mytype subfolder?))
           (when (equal (pathname-type els) mytype)
             (print (string+ ".. " (pathname-name els) " .."))
             (load els)))))

;;;=========================
;;; SOURCES LOADING UTILS

;;;(defun get-current-pathname () *load-pathname*)

(defun make-local-path (folder-path relative-path)
  (if folder-path
    (make-pathname 
     :host (pathname-host (truename folder-path)) :device (pathname-device (truename folder-path))
     :directory (append (pathname-directory (truename folder-path)) (butlast (str2list-path relative-path)))
     :name (car (last (str2list-path relative-path))))
    (pathname relative-path)))


;-----------------------------------------------------------
;symbol management
;-----------------------------------------------------------
(defun interne        (string)     (intern (string-upcase string) :om))
(defun internk        (string)     (intern (string-upcase string) :keyword))
(defun internp        (string pck) (intern (string-upcase string) pck)) 
(defun string2initarg (str)        (read-from-string (if (equal #\: (elt str 0)) str (string+ ":" str))))
(defun num2string     (num)        (format () "~D" num))
(defun float2string     (num)        (format () "~D" num))

;;; return int or nil si error
(defun string2int (str)
   (if (equal str "") nil
     (let ((rep (read-from-string str)))
       (if (integerp rep) rep nil))))

(defun string2num (str)
   (if (equal str "") nil
     (let ((rep (read-from-string str)))
       (if (numberp rep) rep nil))))


(defun list-subtypep (list typeList)
   "Checks if every elt in 'list' belongs to one of the types in 'typelist'"
  (every #'(lambda (elt) (some #'(lambda (type) (subtypep (type-of elt) type)) (list! typelist))) list))

(defun xcons (x y) "Inverse cons" (cons y x))

(defun put-quote (obj) (if (or (symbolp obj) (listp obj)) `',obj obj))

;push item in place but in the last position
(defmacro pushr (item place)
  `(if ,place
     (setf (cdr (last ,place)) (cons ,item nil))
     (setf ,place (list ,item))))


(defmacro ignore-error-msg (msg &body body)
   `(ignore-errors
    (handler-bind ((error #'(lambda (c) (declare (ignore c))
                              (unless *loading-ws* (om-beep-msg ,msg))
                             )))
       (progn ,.body))))


;; =============================================================================-======
;; LELISP MACROS 
;; =============================================================================-======

(defmacro ifnot (testform elseform &body body)
  `(if (not ,testform) ,elseform (progn ,.body)))

(defmacro repeat (count &body body)
  `(dotimes (,(gensym) ,count)
     ,.body))

(defmacro for ((var begin step end) &body body)
  (let ((s2 (gensym)) (s3 (gensym)))
    `(let ((,var ,begin) (,s2 ,step) (,s3 ,end))
       (if (> ,s2 0)
         (loop
           (when (> ,var ,s3) (return))
           (progn ,.body)
           (incf ,var ,s2))
         (loop
           (when (< ,var ,s3) (return))
           (progn ,.body)
           (incf ,var ,s2))))))

;; =============================================================================-======

(defmacro newl (lst elem) `(push ,elem ,lst))

(defmacro nextl (lst &optional symb)
  (if symb
    `(setq ,symb (pop ,lst))
    `(pop ,lst) ))

(defmacro vref (vect index) `(svref ,vect ,index))
(defmacro vset (vect index val) `(setf (svref ,vect ,index) ,val))

;; =============================================================================-======

(defmacro tell (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-")))
    (if args
      `(let ((,args-var (list ,@args)) (,fun-var ,fun))
         (mapc #'(lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
      `(mapc ,fun ,outlet))))

(defmacro ask (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-"))
        (out-var (gensym "OUT-")) (result-var (gensym "RESULT-")))
    `(let ((,args-var (list ,@args)) (,fun-var ,fun) (,result-var nil))
       (dolist (,out-var ,outlet)
         (when (setq ,result-var (apply ,fun-var ,out-var ,args-var))
           (return)))
       ,result-var)))

(defmacro ask-all (outlet fun &rest args)
  (let ((args-var (gensym "ARGS-")) (fun-var (gensym "FUN-")))
    (if args
      `(let ((,args-var (list ,@args)) (,fun-var ,fun))
         (mapcar #'(lambda (x) (apply ,fun-var x ,args-var)) ,outlet))
      `(mapcar ,fun ,outlet))))

;; =============================================================================-======
;; The syntaxe is different from the Le_Lisp "with"
(defmacro with (l-place-value &body body)
  "Changes locally the value of \"setf-able\" places (like a \"let\" where places
would not be restricted to variables)."
  (let ((places (mapcar #'first l-place-value))
        (values (mapcar #'second l-place-value))
        (vars (mapcar #'(lambda (pv) (declare (ignore pv)) (gensym "WITH-"))
                      l-place-value)))
    `(let ,(mapcar #'list vars places)
       (unwind-protect
         (progn
           ,.(mapcar #'(lambda (place value) `(setf ,place ,value)) places values)
           ,.body)
         ,.(mapcar #'(lambda (place var) `(setf ,place ,var)) places vars)))))

;(let ((l '(a . b))) (with (((car l) 1) ((cdr l) 2)) (print l)))



;==========================================================================
;     From Utilities.lisp - old in containers folder
;==========================================================================

(defmacro defclas (name inheritance &rest rest)
  "simpler defclass"
  (let ((slots (pop rest)))
    `(defclass
       ,name
       ,inheritance
       ,(mapcar #'(lambda (slot) 
                    `( ,(first slot) :accessor ,(first slot) :initarg 
                       ,(intern (symbol-name (first slot)) 'keyword)
                       ,. (rest slot)))
                slots)
       ,. rest)))

(defmacro mki (class &rest rest)
  "simpler make-instance"
  `(make-instance ,class ,. rest))

(defmethod copy-instance-to ((source standard-object) (target standard-object))
   "Shallow copies the slots of 'source' to the slots of 'target'. 
Source must be subclass of target"
   (loop for slot in (mapcar 'car (class-instance-slots (class-of source)))
          do (setf (slot-value target slot) (slot-value source slot)))
   target)


;;; -------------------------------

;; fullratios are either ratios or lists (num denum)
;; use fullratio function to cast a fullratio to a number
;; use fdenominator and fdenominator to access to a fullratio num and denum
;; obviously here to avoid MACL automatic simplification of ratios.

(defmethod fullratio ((self list)) (/  (first self)  (second self)))
(defmethod fullratio ((self number)) self)
(defmethod fullratio ((self float)) (round self))

(defmethod fdenominator ((self t)) (denominator (fullratio self)))
(defmethod fdenominator ((self list)) (second self))
(defmethod fnumerator ((self t)) (numerator (fullratio self)))
(defmethod fnumerator ((self list)) (first self))




;-----------------------------------------
;Mmmm 4.2 to 4.3
;-----------------------------------------

#|
(defun get-finder-comment (path)
   (ccl::get-finder-comment path))

(defun set-finder-comment (path str)
   (ccl::set-finder-comment path str))
|#


;=====================================
;   Files Types
;=====================================


;Used to save files
;(deftype :PATC (x) (equal x :TEST))
;(deftype :INST (x) (equal x :TEST))
;(deftype :MAQT (x) (equal x :TEST))
;(deftype :CLAS (x) (equal x :TEST))
;(deftype :METH (x) (equal x :TEST))



;=====================================
;   METACLASSES VARIABLES
;=====================================

(defvar *def-metaclass-class*  'omstandardclass "The meta-class for om classes")
(defvar *def-metaclass-genfun* 'omgenericfunction "The meta-class for om generic functions")
(defvar *def-metaclass-slot*   'omslot "The meta-class for om slots")
(defvar *def-metaclass-method* 'ommethod "The meta-class for om methods")
(defvar *def-metaclass-patch*  'ompatch "The meta-class for om patches")
(defvar *def-metaclass-maq*    'ommaquette "The meta-class for om maquettes")
(defvar *def-metaclass-box*    'OMBox "The meta-class for om boxes")
(defvar *def-metaclass-box-fun*    'omboxcall "The meta-class for om method boxes")
(defvar *def-metaclass-box-type*    'OMBoxTypeCall "The meta-class for om type boxes")
(defvar *def-metaclass-box-edit*    'OMBoxEditCall "The meta-class for om class editor boxes")
(defvar *def-metaclass-box-patch*    'OMBoxPatch "The meta-class for om patch boxes")
(defvar *def-metaclass-box-maq*    'OMBoxMaquette "The meta-class for om maquette boxes")
(defvar *def-metaclass-box-slot*    'OMSlotsBox "The meta-class for om slot boxes")

(defvar *def-metaclass-box-inst*    'OMBoxInstance "The meta-class for om instance boxes")
(defvar *def-metaclass-box-tempo*    'TemporalBox "The meta-class for om temporal boxes")


;=======================
; ABORT + CLEANUP
;=======================

(defun om-abort () 
  (when *cur-eval-panel* (clear-ev-once *cur-eval-panel*))
  ;(om-cancel-drag)
  (om-listener-echo "Aborted")
  (abort))


;=======================
; EXIT OM
;=======================
(defun om-quit ()  
  (let ((om-app-name (pathname-name *app-name*)))
    (when (om-y-or-n-dialog  (string+ (om-str :quit) " " om-app-name "?"))
      (om-confirmed-quit))))

;=======================
; Print IN listener
;=======================

(defvar *om-verbose* t)
(defun om-print (str)  
  (when *om-verbose*
    (print str)))

;=======================
; bouton "add something"
;=======================
(defun om-add-key-p ()
  (om-command-key-p))

;=======================
; Mmmm
;=======================

(defun str-check (string)
  (let ((pos nil))
    (loop for char-switch in (get-switch-chars *om-os*) do
          (loop while (setf pos (position (code-char (car char-switch)) string)) do
                (replace string (string (code-char (cadr char-switch))) :start1 pos)))
    ;; unknown chars... a chercher
    (loop for ch in *unknown-chars* do
          (loop while (setf pos (position ch string)) do   
                (replace string "?" :start1 pos)))
    string))

;; multiple tabs
;(setf string (remove-duplicates string :test #'(lambda (x y) (equal x #\Tab))))

(defun get-switch-chars (os)
  (cond 
   ((equal os :win)    
    '((381 233) ;; é
      (136 224) ;; à
      (144 234) ;; ê
      (143 232) ;; è
      (157 249) ;; ù
      (153 244) ;; ô
      (148 238) ;; î
      (0 231)   ;; ç
      (8217 39) ;; '
      ))
   (t ;(equal os :mac)
    '((142 233) ;; é
      (136 224) ;; à
      (144 243) ;; ê
      (143 232) (768 232) ;; è     ;; #\U+0300
      (157 249) ;; ù
      (153 244) ;; ô
      (148 238) ;; î
      (139 227) ;; ã     ;; #\U+008B
      (141 231) ;; ç
      (135 225) ;; á     ;; #\U+0087
      (146 237) ;; í     ;; #\U+0092
      (8217 39) ;; '
      ))))


(defvar *unknown-chars* '(#\U+0080 #\U+0081 #\U+0082 #\U+0083 #\U+0084 #\U+0085 #\U+0086 #\U+0088 #\U+0089 #\U+008A 
                                   #\U+008C #\U+008D #\U+008E #\U+008F #\U+0090 #\U+0091 #\U+0093 #\U+0094 #\U+0095 
                                   #\U+0096 #\U+0097 #\U+0098 #\U+0099 #\U+009A #\U+009B #\U+009C #\U+009D #\U+009E #\U+009F))

; (string #\U+0097)
; (code-char 135)
; (print (remove-duplicates (mapcar 'code-char (om::arithm-ser 0 10000 1)) :test 'equal))
; (char-code (elt "bamos" 1))
; (char-code #\U+008B)
; (char-code #\U+0087)

; (char-code #\) (char-code #\')
; (str-check "Laghj « dqkjdqmlj »")


