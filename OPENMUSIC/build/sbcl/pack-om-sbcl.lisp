;=========================================================================
;;; OpenMusic packaging on SBCL
;;; load this file to produce a standalone release from the delivered OM executable
;=========================================================================

(in-package "CL-USER")

(require :asdf)

(defparameter *om-root-dir* (uiop:pathname-parent-directory-pathname
                             (uiop:pathname-parent-directory-pathname
                              (uiop:pathname-directory-pathname *load-truename*))))

(defparameter *target-dir*
  (find-if (lambda (dir)
             (let ((name (car (last (pathname-directory dir)))))
               (and (uiop:string-prefix-p "OM " name)
                    (uiop:string-suffix-p name " SBCL")
                    #+darwin (find-if (lambda (sub)
                                        (uiop:string-suffix-p (car (last (pathname-directory sub))) ".app"))
                                      (uiop:subdirectories dir))
                    #-darwin (uiop:directory-files dir "*.exe"))))
           (uiop:subdirectories (uiop:pathname-parent-directory-pathname *om-root-dir*))))

(unless *target-dir*
  (error "No release folder \"OM <version> SBCL\" with an executable next to ~A; run deliver-sbcl.lisp first."
         *om-root-dir*))

#+darwin
(defparameter *app-bundle*
  (find-if (lambda (sub) (uiop:string-suffix-p (car (last (pathname-directory sub))) ".app"))
           (uiop:subdirectories *target-dir*)))

#+darwin
(defparameter *app-resources-dir* (uiop:subpathname *app-bundle* "Contents/Resources/"))

(defparameter *skipped-directories* '(".git" "dev" "logs"))

(defparameter *skipped-types* '("fasl" "lisp~" "DS_STORE" "db"))

(defun copy-tree-into (source target &key (skip-directories *skipped-directories*))
  (ensure-directories-exist target)
  (dolist (file (uiop:directory-files source))
    (unless (member (pathname-type file) *skipped-types* :test #'string-equal)
      (uiop:copy-file file (make-pathname :name (pathname-name file) :type (pathname-type file)
                                          :defaults target))))
  (dolist (dir (uiop:subdirectories source))
    (let ((name (car (last (pathname-directory dir)))))
      (unless (member name skip-directories :test #'string-equal)
        (copy-tree-into dir (uiop:subpathname target (concatenate 'string name "/"))
                        :skip-directories skip-directories)))))

(print "=====================================")
(print (concatenate 'string "MAKING PACKAGE IN " (namestring *target-dir*)))
(print "=====================================")

#-darwin
(progn
  (print "--- COPYING code/ init/ resources/ ...")
  (dolist (folder '("code/" "init/"))
    (copy-tree-into (uiop:subpathname *om-root-dir* folder) (uiop:subpathname *target-dir* folder)))
  (copy-tree-into (uiop:subpathname *om-root-dir* "resources/") (uiop:subpathname *target-dir* "resources/")
                  :skip-directories (cons "lib" *skipped-directories*))

  (print "--- REMOVING PLATFORM-SPECIFIC FONTS...")
  (dolist (os (list #-win32 "win" #-linux "linux" "mac"))
    (uiop:delete-directory-tree (uiop:subpathname *target-dir* (format nil "resources/fonts/~A/" os))
                                :validate t :if-does-not-exist :ignore)))

#+darwin
(progn
  (print "--- macOS: code/, init/ and resources/ are already inside the bundle")

  (print "--- SETTING FONTS INSIDE THE BUNDLE...")
  (let ((mac-fonts (uiop:subpathname *app-resources-dir* "fonts/mac/"))
        (fonts (uiop:subpathname *app-resources-dir* "fonts/")))
    (unless (probe-file mac-fonts)
      (error "Missing the macOS fonts of the release: ~A" mac-fonts))
    (dolist (file (uiop:directory-files mac-fonts))
      (uiop:copy-file file (make-pathname :name (pathname-name file) :type (pathname-type file)
                                          :defaults fonts))))
  (dolist (os '("mac" "win" "linux"))
    (uiop:delete-directory-tree (uiop:subpathname *app-resources-dir* (format nil "fonts/~A/" os))
                                :validate t :if-does-not-exist :ignore))

  (print "--- REMOVING EXTERNAL FOLDERS (resources/code/init)...")
  (dolist (folder '("resources/" "code/" "init/"))
    (uiop:delete-directory-tree (uiop:subpathname *target-dir* folder)
                                :validate t :if-does-not-exist :ignore)))

#+win32
(progn
  (print "--- COPYING DLLs BESIDE THE EXECUTABLE...")
  (dolist (dir '("resources/lib/win64/" "resources/lib/win64/sbcl/"))
    (dolist (file (uiop:directory-files (uiop:subpathname *om-root-dir* dir)))
      (uiop:copy-file file (make-pathname :name (pathname-name file) :type (pathname-type file)
                                          :defaults *target-dir*))))
  (print "--- COPYING OM ICON...")
  (uiop:copy-file (uiop:subpathname *om-root-dir* "build/win64/om.ico")
                  (uiop:subpathname *target-dir* "resources/om.ico")))

(print "--- DONE")
(uiop:quit 0)
