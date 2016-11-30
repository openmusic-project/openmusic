
(in-package "CL-USER")

(load-all-patches)

(load (current-pathname "build-om"))

(oa::om-root-init)
(oa::init-def-rsrc)
(oa::init-sub-rsrc)

(clos::set-clos-initarg-checking nil)

;;; OM IS LOADED
(print "OM IS LOADED")

;;;===========================
;;; LIB DEFINITION
;;;===========================

(defparameter *lib-name-path* (print (make-pathname :directory (butlast (pathname-directory (current-pathname)) 4) :name "lib-name" :type "lisp.tmp")))
(defparameter *lib-name* nil)

(if (probe-file *lib-name-path*)
  (load *lib-name-path*)
  (progn (print "Quitting") (quit))
)

(defparameter *release-dir* (make-pathname :directory (append (butlast (pathname-directory (current-pathname)) 4) 
                                                              '("OM-LIBRARIES-RELEASE"))))
(om::om-create-directory *release-dir*)

(defvar *current-lib-version* nil)

(om::init-omlib-directory)

(defun register-lib (pathname)
  (let* ((omlib (om::omng-make-new-lib pathname)))
    (when omlib
      (om::AddPackage2Pack omlib om::*library-package*))))

(defun prepare-and-pack-lib (libpath target)
  (let* ((libfullname (car (last (pathname-directory libpath))))
         (libname (om::string-until-space libfullname))
         (omlib (om::find-library libname))
         (om::*current-lib* omlib))
    (when omlib
      (load (om::lib-pathname omlib))
      (om::gen-lib-reference omlib)
      (let* ((version (or (om::lib-release omlib) 
                         (when (search " " libfullname) (read-from-string (om::string-from-space libfullname)))))
            (packedlibpath (make-pathname :directory (append (pathname-directory target) 
                                                             (list 
                                                              (if version
                                                                  (concatenate 'string libname " " (format nil "~D" version))
                                                                libname))))))
        (om::om-copy-directory libpath packedlibpath)
        (clean-svn packedlibpath)
        (clean-sources packedlibpath)
        ))))

(defmethod om::require-library (lib &optional (abort-if-not-found nil))
  (let* ((old-lib om::*current-lib*)
         (thelib (om::find-library lib)))
    (unless thelib 
      (let ((path (om::om-make-pathname :directory (append (butlast (pathname-directory (om::lib-pathname old-lib)))
                                                           (list lib)))))
        (register-lib path)
        (setf thelib (om::find-library lib))))
    (if thelib
        (progn 
          (setf om::*current-lib* thelib)
          (unless (om::loaded? thelib)
            (load (om::lib-pathname thelib)))
          (setf om::*current-lib* old-lib)
          t)
      (when abort-if-not-found
        (print (om::string+ "Library " lib " not found!"))
        (oa::om-abort)))))



(when *lib-name*
  (print (concatenate 'string "READY TO PACK LIB " (namestring *lib-name*)))
  (register-lib *lib-name*)
  (prepare-and-pack-lib *lib-name* *release-dir*))

(quit)

                  
