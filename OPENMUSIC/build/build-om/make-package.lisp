
(in-package "CL-USER")

(load-all-patches)

;;; :ub :intel :win :src
(defvar *release* :src)


(print "=====================================")
(print "MAKING PACKAGE")
(print "=====================================")

;;;===========================
;;; PATH DEFINITIONS
;;;===========================

(defvar *om-root-dir* (make-pathname :directory (butlast (pathname-directory (current-pathname)) 2)))

(defvar *libs-in-om-release* nil)
(unless (equal *release* :src)
  (setf *libs-in-om-release* (mapcar #'(lambda (dir) (car (last (pathname-directory dir)))) 
                                     (directory (make-pathname :directory (append (pathname-directory *om-root-dir*)
                                                                                  '("libraries")))
                                                :directories t
                                                :test #'(lambda (item) (and (null (pathname-name item)) (null (pathname-type item))
                                                                            (not (equal #\. (elt (car (last (pathname-directory item))) 0)))))
                                                ))))
                                                
(defvar *projects-in-om-release* nil)
(setf *projects-in-om-release* '("basicproject" "musicproject" "midi" 	
                                 "mathtools" "sdif" "omsounds" "space"
                                 "harmonicproject" "sheet"))
                                              

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
                                                             (if (equal *release* :src)
                                                                 (list (concatenate 'string "OM-"  *om-vers* "-SRC"))
                                                               (list (concatenate 'string "OM "  *om-vers*))))))

;;;===========================
;;; CLEANING STUFF
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

  
(defun clean-svn (&optional dir)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (if (string-equal ".svn" (car (last (pathname-directory file))))
                      (remove-directory file)
                    (clean-svn file))))
          (directory (namestring src-root) :directories t))))
              
;;;===========================
;;; PACKAGE PROCESS 
;;;===========================

(when (probe-file *target-dir*)
  (print (concatenate 'string "Removing previous directory: "(namestring *target-dir*) " ..."))
  (remove-directory *target-dir*))

(print (concatenate 'string "Creating release in "(namestring *target-dir*) " ..."))
(copy-directory *om-root-dir* *target-dir*)


(print "CLEANING SOURCES ...")
(clean-sources (make-pathname :directory (append (pathname-directory *target-dir*) '("code")))
               :remove-extensions '("64xfasl" "xfasl" "nfasl" "ofasl" "ufasl" "lisp~" "DS_STORE"))

(loop for proj in (directory (make-pathname :directory (append (pathname-directory *target-dir*) '("code" "projects")))) do
      (unless (member (car (last (pathname-directory proj))) *projects-in-om-release* :test 'string-equal)
        (remove-directory proj)))




(print "REMOVING BUILD FOLDERS ...")

;; copy the icon file in resources (for the installer)
#+win32(copy-file (make-pathname :directory (append (pathname-directory *target-dir*) '("build" "build-om" "win"))
                                   :name "OpenMusic" :type "ico")
                    (make-pathname :directory (append (pathname-directory *target-dir*) '("resources")) 
                                   :name "OpenMusic" :type "ico"))
                    

(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("build"))))

(print "CLEANING RESOURCES ...")
#+macosx(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" ))))
#-macosx(clean-sources (make-pathname :directory (append (pathname-directory *target-dir*) '("resources")))
                       :remove-extensions '("db" "DS_STORE"))

;;; EXTERNAL LIBRARIES
#+win32(unless (equal *release* :src)
            (mapc #'(lambda (file) 
                      (unless (system::directory-pathname-p file)
                        (system::copy-file file
                                           (make-pathname :directory (pathname-directory *target-dir*)
                                                          :name (pathname-name file)
                                                          :type (pathname-type file)))))
                  (directory (make-pathname :directory (append (pathname-directory *image-pathname*)
                                                               '("resources" "lib" "win")))))
            )

(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "lib"))))

#-macos(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "mac"))))
#-win32(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "win"))))
#-linux(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) '("resources" "fonts" "linux"))))

(print "CLEANING LIBRARIES ...")
(loop for lib in (directory (make-pathname :directory (append (pathname-directory *target-dir*) '("libraries")))) do
      (unless (member (car (last (pathname-directory lib))) *libs-in-om-release* :test 'string-equal)
        (remove-directory lib))
      (clean-sources lib :remove-extensions '("lisp~" "DS_STORE")))


(if (equal *release* :src)
    (progn
      (print "Removing Image")
      #+cocoa(remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) 
                                                                 (list (concatenate 'string *image-name* ".app")))))
      #+win32(delete-file (make-pathname :directory (pathname-directory *target-dir*) 
                                         :name *image-name* :type "exe"))
      
      (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) 
                                                          '("resources" "sample-ws"))))

      (remove-directory (make-pathname :directory (append (pathname-directory *target-dir*) 
                                                          '("resources" "online"))))
  
      (clean-sources (make-pathname :directory (append (pathname-directory *target-dir*) '("resources")))
                     :remove-extensions '("xfasl" "nfasl" "ofasl" "lisp~" "DS_STORE"))
      )
  
  #+cocoa
  (let ((currentimage (make-pathname :directory (append (pathname-directory *image-pathname*) '("Contents" "MacOS"))
                                     :name *image-name*))
        (target-image (make-pathname 
                       :directory (append (pathname-directory *target-dir*) 
                                          (list (concatenate 'string *image-name* ".app") "Contents" "MacOS"))
                       :name *image-name*)))
    (WHEN (equal *release* :ub)
      (print "Setting Universal Binary image")
      ;;; UB exe is saved as image-name in the current directory
      (setf currentimage *image-name*))
    (print "copying new image...")
    (system::call-system (concatenate 'string "rm -Rf \"" (namestring target-image) "\""))
    (system::call-system (concatenate 'string "cp -R \"" (namestring currentimage) "\" \""
                                      (namestring target-image) "\""))
    )
  )
    

(print "CLEAN SVN.......")
(clean-svn *target-dir*)
(clean-sources *target-dir* :remove-extensions '("lisp~" "DS_STORE"))

(print (format nil "==> Done. ~%~%"))

(quit)

;/Applications/LispWorks\ 5.1/LispWorks.app/Contents/MacOS/lispworks-5-1-0-macos-universal -init make-package.lisp






