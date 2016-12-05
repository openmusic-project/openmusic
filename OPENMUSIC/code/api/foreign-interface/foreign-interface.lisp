;;;=========================================================
;;; STANDARD INTERFACE FOR EXTERNAL PROGRAMS AND LIBRARIES
;;; USES CFFI
;;;=========================================================
(in-package :cl-user)

(defpackage :om-fi
  (:use :common-lisp)
  (:export 
   :om-foreign-libraries-directory
   :om-foreign-library-pathname
   :om-load-foreign-library
   :om-load-foreign-libs))

(load (merge-pathnames "ffi/load-cffi" *load-pathname*))
(compile&load (merge-pathnames "dereference" *load-pathname*))

(in-package :om-fi)

;;; REDEFINITION OF MEM ACCESSORS
;(cl-user::compile-file-if-needed (merge-pathnames "dereference.lisp" *load-pathname*))
;(load (merge-pathnames "dereference" *load-pathname*))

;;;========================
;;; FOREIGN LIBRARIES
;;;========================

(defun string-until-char (string char)
  (let ((index (search char string)))
    (if index (values (subseq string 0 index) (subseq string (+ index 1)))
        (values string nil))))

;;; changes the pathname to match with OM's default foreign lib pathname
(defun om-foreign-library-pathname (lib)
  (let ((libraries-directory (om-foreign-libraries-directory)))
    (if (and libraries-directory (probe-file libraries-directory))
        #+(or win32 linux)
      (namestring (make-pathname :directory (pathname-directory libraries-directory)
                                 :host (pathname-host libraries-directory) 
                                 :device (pathname-device libraries-directory)
                                 :name (pathname-name lib)
                                 :type (pathname-type lib)))
      #+cocoa
      (let ((frameworkpos (position "framework" (cdr (pathname-directory lib))
                                    :test 'string-equal :key #'(lambda (item) (cadr (multiple-value-list (string-until-char item ".")))))))

        (make-pathname :directory (append (pathname-directory libraries-directory) 
                                          (if frameworkpos (subseq (pathname-directory lib) (1+ frameworkpos))))
                       :host (pathname-host libraries-directory) 
                       :device (pathname-device libraries-directory)
                       :name (pathname-name lib)
                       :type (pathname-type lib)))
      lib)))


;;; a redefinir pour changer de repertoire par defaut...
;(defun om-foreign-libraries-directory () nil)

; cl-user::*om-src-directory*

;(defvar *load-folder* cl-user::*om-src-directory*)
(defvar *load-folder* (lw-tools::lisp-image-name))

;;; dans le dossier de l'appli
(defun om-foreign-libraries-directory () *load-folder*)

;  #+win32(make-pathname :directory (pathname-directory (LISP-IMAGE-NAME))
;                 :host (pathname-host (LISP-IMAGE-NAME)) :device (pathname-device (lw::LISP-IMAGE-NAME)))
;  #+macosx(make-pathname :directory (append (pathname-directory *root-folder*) '("resources" "lib" "mac"))
;                 :host (pathname-host *root-folder*) :device (pathname-device *root-folder*))
;  #+linux(make-pathname :directory (append (pathname-directory *root-folder*) '("resources" "lib" "linux"))
;                 :host (pathname-host *root-folder*) :device (pathname-device *root-folder*))


;;; We follow the CFFI conventions:
;;;
;;; (cffi::define-foreign-library opengl
;;;   (:darwin  (:framework "OpenGL"))
;;;   (:unix    (:or "libGL.so" "libGL.so.1"
;;;                  #p"/myhome/mylibGL.so"))
;;;   (:windows "opengl32.dll")
;;;   ;; an hypothetical example of a particular platform
;;;   ((:and :some-system :some-cpu) "libGL-support.lib")
;;;   ;; if no other clauses apply, this one will and a type will be
;;;   ;; automatically appended to the name passed to :default
;;;   (t (:default "libGL")))
;;;
;;; This information is stored in the *FOREIGN-LIBRARIES* hashtable
;;; and when the library is loaded through LOAD-FOREIGN-LIBRARY (or
;;; USE-FOREIGN-LIBRARY) the first clause matched by FEATUREP is
;;; processed.

(defun om-load-foreign-library (name spec)
  (let ((lib (intern (string-upcase name) :keyword)))
    (eval `(cffi::define-foreign-library ,lib ,.spec))
    (print (format nil "Loading foreign library: ~A" name))
    (cffi::load-foreign-library lib)))

(defvar *loaders* nil)

(defun om-load-foreign-libs (&optional load-folder)
  (when load-folder (setf *load-folder* load-folder))
  (mapcar 'funcall (reverse *loaders*)))

(defun add-foreign-loader (fun)
  (pushnew fun *loaders*))
  
  



