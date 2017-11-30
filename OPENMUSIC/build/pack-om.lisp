;;;===================================
;;; PACKAGING SCRIPTS
;;; NO NEED TO LOAD THE ACTUAL OM CODE
;;;===================================

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


(defun clean-sources (&optional dir &key remove-files remove-extensions)
  (let ((src-root (or dir (make-pathname :directory (append (butlast (pathname-directory (current-pathname)) 2) '("code"))))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (clean-sources file :remove-files remove-files :remove-extensions remove-extensions)
                (when (or 
                       (and remove-files (or 
                                          (member (pathname-name file) remove-files :test 'string-equal)
                                          (and (stringp (pathname-type file))
                                               (member (concatenate 'string (pathname-name file) "." (pathname-type file))
                                                       remove-files :test 'string-equal))))
                       (and remove-extensions (stringp (pathname-type file))
                            (member (pathname-type file) remove-extensions :test 'string-equal)))
                  (print (concatenate 'string "  Deleting " (namestring file) " ..."))
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


(print "== CLEANING SOURCES ...")
(clean-sources (make-pathname :directory (append (pathname-directory *target-dir*) '("code")))
               :remove-extensions '("64xfasl" "xfasl" "nfasl" "ofasl" "ufasl" "lisp~" "DS_STORE"))



;; copy the icon file in resources (for the installer)
#+win32
(progn (print "== COPYING OM ICON...")
  (copy-file (make-pathname :directory (append (pathname-directory *target-dir*) '("build" "win"))
                            :name "OpenMusic" :type "ico")
             (make-pathname :directory (append (pathname-directory *target-dir*) '("resources")) 
                            :name "OpenMusic" :type "ico"))
  )

(print "== REMOVING BUILD FOLDER ...")
                    
(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("build"))))


(print "== CLEANING RESOURCES FOLDER...")

#-macosx
(clean-sources (make-pathname :directory (append (pathname-directory *target-dir*) '("resources")))
               :remove-extensions '("db" "DS_STORE"))

;;; EXTERNAL LIBRARIES

#+win32
(progn (print "== MOVING DLLs TO ROOT FOLDER...")
  (mapc #'(lambda (file) 
            (unless (system::directory-pathname-p file)
              (system::copy-file file
                                 (make-pathname :directory (pathname-directory *target-dir*)
                                                :name (pathname-name file)
                                                :type (pathname-type file)))))
        (directory (make-pathname :directory (append (pathname-directory *image-pathname*)
                                                     '("resources" "lib" "win")))))
  )

(print "== MOVING BINARIES FROM RESOURCES FOLDER...")
(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "lib"))))

;;; on mac this was moved inside the .app by the deliver script
#+macosx
(progn (print "== REMOVING EXTERNAL FOLDERS (resources/code/init)...")
  (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" ))))
  (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("code" ))))
  (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("init" ))))
)

(print "== REMOVING PLATFORM-SPECIFIC FONTS...")
#-macosx(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "mac"))))
#-win32(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "win"))))
#-linux(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "linux"))))


#+macosx
;(let ((currentimage (make-pathname :directory (append (pathname-directory *image-pathname*) '("Contents" "MacOS"))
;                                   :name *image-name*))
;      (target-image (make-pathname 
;                     :directory (append (pathname-directory *target-dir*) 
;                                        (list (concatenate 'string *image-name* ".app") "Contents" "MacOS"))
;                     :name *image-name*)))
;  
;  (print "copying new image...")
;  (system::call-system (concatenate 'string "rm -Rf \"" (namestring target-image) "\""))
;  (system::call-system (concatenate 'string "cp -R \"" (namestring currentimage) "\" \""
;                                    (namestring target-image) "\""))
;  )
;  )
    
(print "== CLEANING...")
(clean-sources *target-dir* :remove-extensions '("lisp~" "DS_STORE"))

(print (format nil "== DONE! ~%~%"))

(quit)

;/Applications/LispWorks\ 5.1/LispWorks.app/Contents/MacOS/lispworks-5-1-0-macos-universal -init make-package.lisp






