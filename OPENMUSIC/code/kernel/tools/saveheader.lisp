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
;=========================================================================

;DocFile
; OM documents header R/W utils
;DocFile

(in-package :om)
                   
;;; HEADER = 
;OM File Header - Saved 2008/09/04 19:45:12
; (om-version om-type position size editorsize doc-string icon presentation datecreated datemodified)
;End File Header

; return a list with the header params
(defun get-pathname-wsparams (pathname)
  "Return a keyword indicating the type of pathname"
  (let (line)
    (with-safe-open-file (file pathname :direction :input :if-does-not-exist nil)
        (unless (equal 'oa::eof (om-read-line file))
          ;; 2nd line
          (setf line (read-from-string (subseq (om-read-line file) 1))))
        line)))

; write objet header in file
(defun write-header (self file)
  (write-line (string+ "; OM File Header - Saved " (om-get-date)) file)
  (write-line (header-comment-from-obj self) file)
  (write-line "; End File Header" file))

; return default header for obj
(defmethod header-comment-from-obj ((self t)) 
  (let* ((icon (icon self))
        (ftype (obj-file-type self))
        (version (omversion self))
        (wspar (om-save-point-list (ensure-ws-params self)))
        (params (list version ftype (first wspar) (second wspar) (third wspar) (str-without-nl (doc self)) (save-icon icon) 0
                      (car (create-info self)) (cadr (create-info self)))))
    (string+ ";" (format nil " ~S" params))))

; create-file for obj
(defun create-om-file (self pathname &key (file-type :TEXT))
  "Creates an empty file named pathname and its documentation file, and returns the truename of the created file"
  (when (directoryp pathname)
    (setq pathname (om-make-pathname :directory pathname :name ".finderinfo")))
  (with-open-file (file pathname :direction :output  :if-does-not-exist :create :if-exists :supersede
			:external-format :utf-8
			) ;;; :external-format file-type )
    (write-header self file))
  (truename pathname))

; get header params for old or recent files
(defun get-finder-comment (pathname)
  "Returns the finder comment of a file or nil if it doesn't have one"
  
  (let (wsparams resources)
    (when (and (not (equal (format nil "~A" (file-namestring pathname)) ".finderinfo"))
               (not (equal (format nil "~A" (file-namestring pathname)) "/.finderinfo"))
              (not (equal (format nil "~A" (file-namestring pathname)) ".DS_Store")))
      (if (directoryp pathname)
        (let ((folder-info-path (om-make-pathname :directory pathname :name  ".finderinfo")))
          (unless (probe-file folder-info-path)
            (with-open-file (file folder-info-path :direction :output :if-does-not-exist :create
				  :external-format :utf-8) ;;; :external-format file-type )
              (write-line (string+ "; OM File Header - Created " (om-get-date)) file)
              (write-line (header-comment-from-obj (make-instance 'omfolder :icon 186)) file)
              (write-line "; End File Header" file)))
          (setf wsparams (get-pathname-wsparams folder-info-path)))
        (cond
         ((not (file-has-comments pathname))
          (setf wsparams (list 2 (file-type pathname) (om-make-point 24 24) (om-make-point 50 50) (om-make-point 500 400) "" nil nil nil)))
         ((file-has-old-comments pathname)
          ;(setf resources (comp-get-resourcespathname pathname))
          (setf wsparams (list 4 (read-old-type pathname) (om-make-point 24 24) (om-make-point 50 50) (om-make-point 500 400) "" nil nil nil))
          ;(remove-old-comments pathname wsparams)
          ;(comp-set-resourcespathname pathname resources)
          )
         (t
          (setf wsparams (get-pathname-wsparams pathname))))))
    
    wsparams))

;search 
(defun get-init-wsparams (path)
  (let ((wsparams (get-finder-comment path)))
    (unless (and wsparams (>= (length wsparams) 8))
      (setf wsparams (list *om-version* :fold (om-make-point 24 24) (om-make-point 50 50) (om-make-point 500 400) "" nil 0)))
    wsparams))



;==============get-finder-comment

(defun is-a-comment (line)
  (or (not (stringp line))
      (equal (elt line 0) #\;)))

(defun set-finder-comment (pathname self)
  "Sets the finder comment of a file"
  (when (om-persistant-p pathname)
    (let (line (linelist nil) folderp rsrc)
      (when (setf folderp (directoryp pathname))
        (setq pathname (om-make-pathname :directory pathname :name ".finderinfo")))
      (when (probe-file pathname)
        (setf rsrc (get-resources pathname))
        (with-safe-open-file (file pathname :direction :input)
          ; skip comments
          (loop while (and (not (equal line :eof))
                           (is-a-comment line))
                do (setf line (read-line file nil :eof)))
          ;(om-read-line file)
          ;(om-read-line file)
          ;(om-read-line file)
          (loop while (not (equal line :eof)) do
                (unless (equal line "") (setf linelist (append linelist (list line))))
                (setf line (read-line file nil :eof))))
        (delete-file-protection pathname))
        (with-open-file (file pathname :direction :output :if-exists :supersede
			      :external-format :utf-8
			      )
          (write-header self file)
          (when rsrc 
            (write-line "; External resources " file)
            (write-line (format nil "; ~S" (omng-save rsrc)) file))
          (setf line (pop linelist))
          (loop while (and line (not (equal line :eof))) do
                (write-line line file)
                (setf line (pop linelist))
                ))
        )))


;======================file-type

(defun file-has-comments (path)
  (let (rep)
    (with-safe-open-file (in path :direction :input)
      (let ((char (read-char in nil :eof)))
        (when (and (not (eq char :eof)) (equal char #\;))
          (setf rep t))))
    rep))


(defun file-has-old-comments (path)
  (let (rep)
    (with-safe-open-file (in path :direction :input  )
      (let ((char (read-char in nil :eof)))
        (setf char (read-char in nil :eof))
        (setf char (read-char in nil :eof))
        (when (and (not (eq char :eof)) (equal char #\*))
          (setf rep t))))
    rep))

;;; not used anymore.. ?
(defun remove-old-comments (pathname params)
  (let ((data nil)
        oldheader line)
    (with-safe-open-file (file pathname :direction :input)
      (loop for i from 1 to 7 do (read-line file nil :eof))
      (setq line (read-line file nil :eof))
      (loop while (not (eq line :eof)) do
            (push line data)
            (setq line (read-line file nil :eof))))
    (setf data (reverse data))
    (delete-file-protection pathname)
    (with-open-file (file pathname :direction :output :if-exists :supersede :external-format :utf-8)
      (write-line ";fileheader" file)
      (write-line (format nil "; (~D :~D ~D ~D ~D ~S ~D)" (first params) (second params)
                          (om-save-point (third params)) (om-save-point (fourth params)) (om-save-point (fifth params))
                          (sixth params) (seventh params)) file)
      (write-line ";endfileheader" file)
      (loop for item in data do
            (write-line item file)))
    ))


(defun read-old-type (pathname)
  (let (line)
    (with-safe-open-file (file pathname :direction :input)
      (read-line file)
      (read-line file)
      (setq line (read-from-string (subseq (read-line file) 2))))
    line))

(defun om-put-path-extension (path type)
  (make-pathname :device (pathname-device path) :host (pathname-host path)
                 :directory (pathname-directory path) 
                 :name (pathname-name path)
                 :type type))
