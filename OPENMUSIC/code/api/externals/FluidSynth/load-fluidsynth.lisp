(require :cffi "/home/andersvi/site/OM/OM_SVN/branches/linux_initial/OPENMUSIC/code/api/externals/FFI/load-new-cffi.lisp")

;; #-cffi-new (require :cffi (make-pathname :directory (append om-api::*externals-directory* '("FFI")) :name "load-new-cffi"))

(defpackage :cl-fluidsynth (:use :common-lisp :cffi))
(in-package :cl-fluidsynth)

;;; LINK IN FLUIDSYNTH LIB:

(define-foreign-library fluidsynth
  (t (:default "libfluidsynth")))

(use-foreign-library fluidsynth)

;;; swig-generated FFI-wrappers for {include}/fluidsynth.h,
;;; {include}/fluidsynth/*.h:

(defun compile?-and-load (file)
  (compile-file file)
  (load file))

(setf cl-fluidsynth-files '("fluidsynth_ffi"))

(dolist (file cl-fluidsynth-files)
  (compile?-and-load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

(provide :cl-fluidsynth)
