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
; Import tutorials and examples
;DocFile

(in-package :om)


(defun collect-resources (path)
  (loop for item in (om-directory path) append
        (cond ((directoryp item) (collect-resources item))
              ((om-persistant-p item) 
               (let ((fileres (get-resources item)))
                 (loop for restype in fileres append
                       (loop for path in (cdr restype) collect
                             (let ((*relative-path-reference* item)
                                   (corrected-path (if (and (stringp path) (pathnamep (read-from-string path)))
                                                       (read-from-string path)
                                                     path)))
                               (list (car restype) (restore-path corrected-path))
                               )))))
              (t nil)
              )))


(defun search-file-recursive (name dir type)
  (let ((rep nil))
    (loop for file in (om-directory dir :files t :directories t) 
          while (not rep) do
          (if (directoryp file)
              (setf rep (search-file-recursive name file type))
            (if (and (string-equal (pathname-name file) name)
                     (or (not type) (string-equal type (pathname-type file))))
                (setf rep file)
              )
            ))
    ;(print (list name dir type "=>" rep)) 
    rep))


(defun find-resource-file (file type tutorial-type tutorial-path)
  (cond ((equal type :picture)
         (let* ((nnn (pathname-name file))
                (ddd (make-pathname :directory (pathname-directory file)))
                (pictfile (find nnn (om-directory ddd :files t :directories nil :type *om-pict-type*) 
                                :key 'pathname-name :test 'string-equal)))
           (unless pictfile
             (setf pictfile (search-file-recursive nnn (om-make-pathname :directory (pathname-directory tutorial-path)) nil)))
           pictfile))
        
         (t (if (probe-file file)   
                file
                (search-file-recursive (pathname-name file) (make-pathname :directory 
                                                                           (if (equal tutorial-type 'ws)
                                                                               (append (pathname-directory tutorial-path) '("in-files"))
                                                                             (pathname-directory tutorial-path)))
                                       (pathname-type file))
              ))
         ))

(defun find-examples-patches (lib)
  (let ((lib-subfolders (om-directory (om-make-pathname :directory (lib-pathname lib)) :files nil :hidden-files nil)))  
    (find-if #'(lambda (item) (find item (list "examples" "patches" "tutorials")
                                    :test 'string-equal))  
                 lib-subfolders :key #'(lambda (path) (car (last (pathname-directory path)))))))


;;; dir = the folder to import
;;; name = the name of the tutorial
;;; editor = the caller editor
;;; tutorial-type = 'kernel or 'lib
(defun import-tutorial (dir name editor tutorial-type tutorial-path) 
  (let ((resources (collect-resources dir)))
    (setf resources (remove-duplicates resources :test #'(lambda (a b) (and (equal (car a) (car b))
                                                                            (string-equal (namestring (cadr a)) (namestring (cadr b)))))))
    ;(print resources)
    
    ;;; import external resources
    (loop for item in resources do
          (let ((file (find-resource-file (cadr item) (car item) tutorial-type tutorial-path)))
            ;(print (list (cadr item) (car item) tutorial-type tutorial-path))
            (if file
              (cond ((equal (car item) :picture)
                     (print (string+ "importing picture: " (pathname-name (cadr item))))
                     (copy-file-in-dir file
                                       (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*))
                                                                         (list "resources" "pict")))))
                    ((member (car item) '(:sound :sdif :midi :text))
                     (print (string+ "importing resource: " (pathname-name (cadr item)) " (" (string (car item)) ")"))
                     (copy-file-in-dir file 
                                       (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*))
                                                                         (list "in-files")))))
                    (t nil))
              (print (string+ "resource: " (string+ (pathname-name (cadr item)) 
                                                    (if (pathname-type (cadr item))
                                                        (string+ "." (pathname-type (cadr item)))
                                                      "")) " not found...")))
            ))
    ;;; import patches
    ;(print (list dir name))
    (make-new-folder (panel editor) dir (or (find-a-position (panel editor)) (om-make-point 20 30)) name)
    ))

*om-resources-folder*

(defun import-tutorial-menu (editor)
  (let ((tut-folder (merge-pathnames "tutorials/" *om-resources-folder*)))
    (list (list 
           (om-make-menu 
            "Import Tutorials Patches..." 
            (append 
             (list (mapcar #'(lambda (f)
                               (om-new-leafmenu (car (last (pathname-directory f))) 
                                                #'(lambda () 
                                                    (import-tutorial  f (car (last (pathname-directory f))) editor 'kernel tut-folder))))
                           (om-directory tut-folder :directories t :files nil)))
             (list 
              (om-make-menu 
               "Libraries"
               (list #'(lambda (x)
                         (remove nil 
                                 (mapcar #'(lambda (lib) 
                                             (let ((exdir (find-examples-patches lib)))
                                               (when (and exdir (probe-file exdir) (om-directory exdir :directories t :files t :type '("omp" "omm" "she")))
                                                 (om-new-leafmenu (name lib) 
                                                                  #'(lambda () 
                                                                      (import-tutorial exdir (string+ (name lib) "-tutorial-patches") editor 'lib (lib-pathname lib)))))))
                                         (subpackages *library-package*))
                                 )))))
                                
             ))))
    ))
