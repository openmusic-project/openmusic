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

;=========================================================================
;;; OpenMusic packaging            
;;; load this file to produce a standalone release from delivered OM app
;;; nb: no need to load OM here, these are just folder copies / cleanup
;=========================================================================

(in-package "CL-USER")

(load-all-patches)

;;; :intel :win 
;;; :src no more supported => just get it from the source repository !
;;; :ub no more supported

;;;===========================
;;; CLEANING UTILS
;;;===========================

(defun remove-directory (path)
  (when (probe-file path) 
    #+cocoa(system::call-system (concatenate 'string "rm -Rf \"" (namestring path) "\""))
    #+win32(system::call-system (concatenate 'string "rd /S /Q \"" (namestring path) "\""))
    ))


(defun copy-directory (srcpath targetpath)
  (print (namestring srcpath))
  #+win32(progn
           (ensure-directories-exist targetpath)
           (loop for item in (directory srcpath) do
                 (if (system::directory-pathname-p item)
                     (unless (string-equal (car (last (pathname-directory item))) ".svn") 
                       (copy-directory (namestring item) (make-pathname :device (pathname-device targetpath) 
                                                                          :directory (append (pathname-directory targetpath) 
                                                                                             (last (pathname-directory item))))))
                   (system::copy-file item (make-pathname :device (pathname-device targetpath) 
                                                          :directory (pathname-directory targetpath)
                                                          :name (pathname-name item) :type (pathname-type item))))
                 ))
  #+cocoa(system::call-system (concatenate 'string "cp -R \"" (namestring srcpath) "\" \""
                                           (namestring targetpath) "\"")))


(defun clean-sources (&optional dir &key remove-files remove-extensions (verbose t))
  (let ((src-root (or dir (make-pathname :directory (append (butlast (pathname-directory (current-pathname)) 2) '("code"))))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (clean-sources file :remove-files remove-files :remove-extensions remove-extensions :verbose verbose)
                (when (or 
                       (and remove-files (or 
                                          (member (pathname-name file) remove-files :test 'string-equal)
                                          (and (stringp (pathname-type file))
                                               (member (concatenate 'string (pathname-name file) "." (pathname-type file))
                                                       remove-files :test 'string-equal))))
                       (and remove-extensions (stringp (pathname-type file))
                            (member (pathname-type file) remove-extensions :test 'string-equal)))
                  (when verbose (print (concatenate 'string "  Deleting " (namestring file) " ...")))
                  (delete-file file)
                  )))
          (directory (namestring src-root) :directories t))
    ))

 

(print "=====================================")
(print "MAKING PACKAGE")
(print "=====================================")

;;;===========================
;;; PATH DEFINITIONS
;;;===========================

(defvar *om-root-dir* (make-pathname :directory (butlast (pathname-directory (current-pathname)))))
                                            
(defvar *image-pathname* (find "OM" ;(directory (make-pathname :directory (append (pathname-directory *om-root-dir*) (list "image" "macos-i"))))
                               (directory *om-root-dir*)
                               :test 'string-equal 
                               :key #'(lambda (file) 
                                        #+cocoa(let ((name (car (last (pathname-directory file)))))
                                          (if (and 
                                               (system::directory-pathname-p file)
                                               (stringp name) (> (length name) 2))
                                              (subseq name 0 2)
                                            ""))
                                        #+win32(let ((name (pathname-name file)))
                                                 (if (and (string-equal (pathname-type file) "exe")
							  (stringp name) (> (length name) 2))
                                                     (subseq name 0 2)
                                                   ""))
                                        )))

(defparameter *om-vers* 
  #+cocoa(subseq (car (last (pathname-directory *image-pathname*))) 
                          3 (- (length (car (last (pathname-directory *image-pathname*)))) 4))
  #+win32(subseq (pathname-name *image-pathname*) 3)
  )


(defvar *image-name* (concatenate 'string "OM " *om-vers*))

(defparameter *target-dir* (make-pathname :directory (append (butlast (pathname-directory *om-root-dir*))
                                                             (list (concatenate 'string "OM "  *om-vers*)))))

 
;;;===========================
;;; PACKAGING 
;;;===========================

(when (probe-file *target-dir*)
  (print (concatenate 'string "== REMOVING PREVIOUS BUILD: " (namestring *target-dir*) " ..."))
  (remove-directory *target-dir*))

(print (concatenate 'string "== CREATING RELEASE: "(namestring *target-dir*) " ..."))
(copy-directory *om-root-dir* *target-dir*)


(print "--- REMOVING FASL FILES ...")
(clean-sources (make-pathname :directory (append (pathname-directory *target-dir*) '("code")))
                  :remove-extensions '("64xfasl" "xfasl" "nfasl" "ofasl" "ufasl" "lisp~" "DS_STORE")
                  :verbose nil)



;; copy the icon file in resources (for the installer)
#+win32
(progn (print "--- COPYING OM ICON...")
  (copy-file (make-pathname :directory (append (pathname-directory *target-dir*) '("build" "win"))
                            :name "om" :type "ico")
             (make-pathname :directory (append (pathname-directory *target-dir*) '("resources")) 
                            :name "om" :type "ico"))
  )

(print "--- REMOVING BUILD FOLDER ...")
                    
(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("build"))))


(print "--- CLEANING RESOURCES FOLDER...")

#-macosx
(clean-sources (make-pathname :directory (append (pathname-directory *target-dir*) '("resources")))
               :remove-extensions '("db" "DS_STORE") :verbose nil)

;;; EXTERNAL LIBRARIES

#+win32
(progn (print "--- MOVING DLLs TO ROOT FOLDER...")
  (mapc #'(lambda (file) 
            (unless (system::directory-pathname-p file)
              (system::copy-file file
                                 (make-pathname :directory (pathname-directory *target-dir*)
                                                :name (pathname-name file)
                                                :type (pathname-type file)))))
        #+x86(directory (make-pathname :directory (append (pathname-directory *image-pathname*)
                                                          '("resources" "lib" "win"))))
        #+x64(directory (make-pathname :directory (append (pathname-directory *image-pathname*)
                                                          '("resources" "lib" "win64"))))
        )
  )

(print "--- MOVING BINARIES FROM RESOURCES FOLDER...")
(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "lib"))))

;;; on mac this was moved inside the .app by the deliver script
#+macosx
(progn (print "--- REMOVING EXTERNAL FOLDERS (resources/code/init)...")
  (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" ))))
  (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("code" ))))
  (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("init" ))))
)

(print "--- REMOVING PLATFORM-SPECIFIC FONTS...")
#-macosx(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "mac"))))
#-win32(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "win"))))
#-linux(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "linux"))))

    
(print "--- CLEANING...")
(clean-sources *target-dir* :remove-extensions '("lisp~" "DS_STORE") :verbose nil)

(quit)

;/Applications/LispWorks\ 5.1/LispWorks.app/Contents/MacOS/lispworks-5-1-0-macos-universal -init make-package.lisp






