
(in-package "CL-USER")

(load-all-patches)

(load (current-pathname "build-om"))

(oa::om-root-init)
(oa::init-def-rsrc)
(oa::init-sub-rsrc)

(clos::set-clos-initarg-checking nil)

;;; OM IS LOADED

;;;===========================
;;; PATH DEFINITIONS
;;;===========================

(defparameter *om-root-dir* (make-pathname :directory (butlast (pathname-directory (current-pathname)) 3)))
(defparameter *forum-libs-src* (make-pathname :directory (append (pathname-directory *om-root-dir*) '("LIBRARIES" "OM-FORUM-LIBRARIES"))))
(defparameter *extra-libs-src* (make-pathname :directory (append (pathname-directory *om-root-dir*) '("LIBRARIES" "EXTRA-LIBS"))))

(defparameter *forum-libs* '("OM_Diph" "OM-Chant" "OM-Orchidee" "OM-pm2" "OM-Spat" "OM-SuperVP" "OM2Csound" "OMChroma" "OMPrisma" "Pixels"))
(defparameter *extra-libs* '("Morphologie" "OM_ASX" "OMCombine" "OMRC" "OMCS" "OMTimePack" "OMPitchField"))

(defparameter *pack-forum* (make-pathname :directory (append (pathname-directory *om-root-dir*) '("OM-FORUM-LIBRARIES"))))
(defparameter *pack-extra* (make-pathname :directory (append (pathname-directory *om-root-dir*) '("OM-EXTRA-LIBRARIES"))))

(defvar *current-lib-version* nil)

(om::init-omlib-directory)

(defun register-lib (pathname)
  (let* ((omlib (om::omng-make-new-lib pathname)))
    (when omlib
      (om::AddPackage2Pack omlib om::*library-package*))))


;(defun prepare-lib (libpath)
;  (let* ((omlib (om::find-library (om::string-until-space (car (last (pathname-directory libpath))))))
;         (om::*current-lib* omlib))
;    (when omlib
;      (load (om::lib-pathname omlib))
;      (om::gen-lib-reference omlib)
;      )))

;(defun pack-lib (libpath target)
;  (let ((packedlibpath (make-pathname :directory (append (pathname-directory target) 
;                                                      (last (pathname-directory libpath))))))
;  (om::om-copy-directory libpath packedlibpath)
;  (clean-svn packedlibpath)
;  (clean-sources packedlibpath)
;  ))

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
        (thelib (om::find-library lib))
        (om::*current-lib* thelib))
    (if thelib
        (progn 
          (unless (om::loaded? thelib)
            (load (om::lib-pathname thelib)))
          (setf om::*current-lib* old-lib)
          t)
      (when abort-if-not-found
        (print (string+ "Library " lib " not found!"))
        (oa::om-abort)))))

(defun do-libs ()
  (flet ((get-libs-in-folder (folder namelist) 
           (remove nil (mapcar #'(lambda (lib) (or (find lib (om::om-directory folder :files nil :directories t) :test 'string-equal 
                                                         :key #'(lambda (path) (om::string-until-space (car (last (pathname-directory path))))))
                                                   (progn
                                                     (print (format nil "!!!! Folder for library ~s not found in ~s" lib (namestring folder)))
                                                     nil)
                                                   ))
                               namelist))))
  (let ((forumlibs (get-libs-in-folder *forum-libs-src* *forum-libs*))
        (extralibs (get-libs-in-folder *extra-libs-src* *extra-libs*)))
    
    (dolist (libpath (append forumlibs extralibs))
      (register-lib libpath))
    
    (dolist (targetpath (list *pack-forum* *pack-extra*))    
      (om::om-delete-directory targetpath)
      (om::om-create-directory targetpath))

    (dolist (libpath forumlibs)
      (prepare-and-pack-lib libpath *pack-forum*))
    (dolist (libpath extralibs)
      (prepare-and-pack-lib libpath *pack-extra*))
    )
    
  (let ((forum-readme (make-pathname :directory (pathname-directory *forum-libs-src*)
                                     :name "README" :type "TXT"))
        (extra-readme (make-pathname :directory (pathname-directory *extra-libs-src*)
                                     :name "README" :type "TXT")))
    (when (probe-file forum-readme)
      (system::copy-file forum-readme
                         (make-pathname :directory (pathname-directory *pack-forum*)
                                        :name "README" :type "TXT")))
    (when (probe-file extra-readme)
      (system::copy-file forum-readme
                         (make-pathname :directory (pathname-directory *pack-extra*)
                                        :name "README" :type "TXT")))
    )))

(do-libs)

(quit)

                  
