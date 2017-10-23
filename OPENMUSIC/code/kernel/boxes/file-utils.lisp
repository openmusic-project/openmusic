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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
; OM in/out files utils
;DocFile

(in-package :om)


;;;=========
; USER FILES 
;;;=========

(defvar *om-outfiles-folder* nil)
(defvar *om-tmpfiles-folder* nil)
(defvar *om-infiles-folder* nil)
(defvar *om-tmp-draw-filename* nil)


;;;=== IN FILE ==

(defmethod! set-infiles-folder ((path string))
  :icon 250
  (setf *om-infiles-folder* (pathname path)))

(defmethod! set-infiles-folder ((path pathname))
  :icon 250
  (setf *om-infiles-folder* path))

(defmethod! infile ((name string) &key (subdirs nil) (unix nil) (type nil))
  :icon 250
  :indoc '("file name" "directories" "unix format" "type extension")
  :initvals '("" nil nil nil)
  :doc "Returns a file pathname corresponding to <name> in the default OM IN FILES directory.

The IN FILES directory can be set in the OM Preferences. It is used as a default location to read files in OM.

<subdirs> is a list of strings corresponding to IN FILES subdirectories.
<type> is a type extension to append to the filename. If not specified, the type of <name> is used.
If <unix> is T then the output files is formatted for Unix and system commands.

Ex. (infile \"myfile.midi\") ==> #P\"/Users/bresson/om-infiles/myfile.midi\"
Ex. (infile \"myfile.midi\" :subdirs '(\"folder1\" \"folder2\") ==> #P\"/Users/bresson/om-infiles/folder1/folder2/myfile.midi\"
"
  (let ((pa (make-pathname :directory (append (if *om-infiles-folder* (pathname-directory *om-infiles-folder*) '(:RELATIVE))
                                              (list! subdirs))
                           :host (and *om-infiles-folder* (pathname-host *om-infiles-folder*))
                           :name (pathname-name name) :type (or type (pathname-type name)))))
    (if unix (namestring pa) pa)))
  
(defmethod! infile ((name null) &key (subdirs nil) (unix nil) (type nil))
  :icon 250
  (let ((pa (make-pathname :directory (append (pathname-directory *om-infiles-folder*) (list! subdirs))
                           :host (pathname-host *om-infiles-folder*))))
    (if unix (om-path2cmdpath pa) pa)))


;;;=== OUT FILE ==

(defmethod! set-outfiles-folder ((path string))
  :icon 250
  (setf *om-outfiles-folder* (pathname path)))

(defmethod! set-outfiles-folder ((path pathname))
  (setf *om-outfiles-folder* path))

(defmethod! outfile ((name string) &key (subdirs nil) (unix nil) (type nil))
  :icon 250
  :indoc '("file name" "directories" "unix format" "type extension")
  :initvals '("" nil nil nil)
  :doc "Returns a file pathname corresponding to <name> in the default OM OUT FILES directory.

The OUT FILES directory can be set in the OM Preferences. It is used as a default location to write files in OM.

<subdirs> is a list of strings corresponding to INFILES subdirectories.
<type> is a type extension to append to the filename. If not specified, the type of <name> is used.
If <unix> is T then the output files is formatted for Unix and system commands.

Ex. (outfile \"myfile.midi\") ==> #P\"/Users/bresson/om-outfiles/myfile.midi\"
Ex. (outfile \"myfile.midi\" :subdirs '(\"folder1\" \"folder2\") ==> #P\"/Users/bresson/om-outfiles/folder1/folder2/myfile.midi\"
"
  (let ((pa (om-make-pathname :directory (append (pathname-directory *om-outfiles-folder*) (list! subdirs))
                           :host (pathname-host *om-outfiles-folder*)
                           :name (pathname-name name) :type (or type (pathname-type name)))))
    (if unix (om-path2cmdpath pa) pa)))

(defmethod! outfile ((name null) &key (subdirs nil) (unix nil) (type nil))
  :icon 250
  (let ((pa (om-make-pathname :directory (append (pathname-directory *om-outfiles-folder*) (list! subdirs))
                           :host (pathname-host *om-outfiles-folder*))))
    (if unix (om-path2cmdpath pa) pa)))


;;;=== TMP FILE ==

(defmethod! set-tmpfiles-folder ((path string))
  :icon 250
  (setf *om-tmpfiles-folder* (pathname path)))

(defmethod! set-tmpfiles-folder ((path pathname))
  :icon 250
  (setf *om-tmpfiles-folder* path))

(defmethod! tmpfile ((name string) &key (subdirs nil) (unix nil) (type nil))
  :icon 250
  :indoc '("file name" "directories" "unix format" "type extension")
  :initvals '("" nil nil nil)
  :doc "Returns a file pathname corresponding to <name> in the default OM TMP FILES directory.

The TMP FILES directory can be set in the OM Preferences. It is used as a default location to write temporary files in OM.

<subdirs> is a list of strings corresponding to TMP FILES subdirectories.
<type> is a type extension to append to the filename. If not specified, the type of <name> is used.
If <unix> is T then the output files is formatted for Unix and system commands.

Ex. (tmpfile \"myfile.midi\") ==> #P\"/Users/bresson/om-tmpfiles/myfile.midi\"
Ex. (tmpfile \"myfile.midi\" :subdirs '(\"folder1\" \"folder2\") ==> #P\"/Users/bresson/om-tmpfiles/folder1/folder2/myfile.midi\"
"
  (let ((pa (make-pathname :directory (append (pathname-directory *om-tmpfiles-folder*) (list! subdirs)) 
                           :host (pathname-host *om-tmpfiles-folder*)
                           :name (pathname-name name) :type (or type (pathname-type name)))))
    (if unix (om-path2cmdpath pa) pa)))

(defmethod! tmpfile ((path null) &key (subdirs nil) (unix nil) (type nil))
  :icon 250
  (let ((pa (make-pathname :directory (append (pathname-directory *om-tmpfiles-folder*) (list! subdirs))
                           :host (pathname-host *om-tmpfiles-folder*))))
    (if unix (om-path2cmdpath pa) pa)))

(defun init-user-folders ()
  (setf *om-outfiles-folder* 
    (check-folder (make-pathname :device (pathname-device (mypathname *current-workspace*))
                                 :host (pathname-host (mypathname *current-workspace*))
                                 :directory (append (pathname-directory (mypathname *current-workspace*)) (list "out-files")))))
  (setf *om-tmpfiles-folder* 
    (check-folder (make-pathname 
                       :device (pathname-device (mypathname *current-workspace*))
                       :host (pathname-host (mypathname *current-workspace*))
                       :directory (append (pathname-directory (mypathname *current-workspace*)) (list "out-files")))))
  (setf *om-infiles-folder*
        (check-folder (make-pathname 
                       :device (pathname-device (mypathname *current-workspace*))
                       :host (pathname-host (mypathname *current-workspace*))
                       :directory (append (pathname-directory (mypathname *current-workspace*)) (list "in-files")))))
  (setf *om-tmp-draw-filename* (om-make-pathname :directory (pathname-directory *om-outfiles-folder*) :name "tmpfile-to-draw" :type "aiff"))
  )

(add-init-user-func 'init-user-folders)

(defun pathname-dir (pathname)
  (make-pathname :directory (pathname-directory pathname)
                 :host (pathname-host pathname) :device (pathname-device pathname)))

(defvar *last-saved-dir* nil)

(defun def-save-directory ()
  (or (and *last-saved-dir* (probe-file *last-saved-dir*))
      (and *om-outfiles-folder* (probe-file *om-outfiles-folder*))
      (om-user-home)))

(defvar *last-loaded-dir* nil)

(defun def-load-directory ()
  (or (and *last-loaded-dir* (probe-file *last-loaded-dir*))
      (and *om-infiles-folder* (probe-file *om-infiles-folder*))
      (om-user-home)))


;;; FILE CHOOSE TOOLS

(defmethod! file-chooser (&optional (type 'file) (mode 'existing) (initial-folder nil) (message nil))
  :icon 186
  :initvals '(file existing desktop nil)
  :indoc '("file or directory" "new or existing" "pathname" "prompt for the dialog")
  :menuins '((0 (("file" 'file) ("directory" 'directory))) 
             (1 (("new" 'new) ("existing" 'existing))) 
             (2 (("home" 'home) ("desktop" 'desktop) ("other" nil))))
  :doc "Pops up a file or directory chooser dialog.

<type> allows to choose between a file or directory.
<mode> determines whether this should be an existing file or directory or a new one to be created.
<initial-folder> allows to determine a strating directory for browsing the file system.
<message> allows to set a specific message on the dialog.

Returns the selected pathname or NIL if cancelled."

  (let ((initfolder 
         (cond ((equal initial-folder 'home) (om-user-home))
               ((equal initial-folder 'desktop) (om-make-pathname :directory (append (pathname-directory (om-user-home)) '("Desktop"))))
               (t *last-imported*)))
        (rep nil))
    (setf rep
          (cond ((and (equal type 'file) (equal mode 'existing))
                 (om-choose-file-dialog :prompt message :directory initfolder))
                ((and (equal type 'directory) (equal mode 'existing))
                 (om-choose-directory-dialog :prompt message :directory initfolder))
                ((and (equal type 'file) (equal mode 'new))
                 (om-choose-new-file-dialog :prompt message :directory initfolder))
                ((and (equal type 'directory) (equal mode 'new))
                 (om-choose-new-directory-dialog :prompt message :directory initfolder)))
          )
    (when rep (setf *last-imported* (om-make-pathname :directory rep)))
    rep
    ))










