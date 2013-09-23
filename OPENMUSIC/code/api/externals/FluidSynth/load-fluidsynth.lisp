;; common lisp - fluidsynth connection kit, supports 'internal' fluidsynth,

(require :cffi "/home/andersvi/site/OM/OM_SVN/branches/linux_initial/OPENMUSIC/code/api/externals/FFI/load-new-cffi.lisp")

;; #-cffi-new (require :cffi (make-pathname :directory (append om-api::*externals-directory* '("FFI")) :name "load-new-cffi"))

(defpackage :cl-fluidsynth (:use :common-lisp :cffi))
(in-package :cl-fluidsynth)

;;; swig-generated FFI-wrappers for {include}/fluidsynth.h,
;;; {include}/fluidsynth/*.h:

(defun compile?-and-load (file)
  #+lispworks (hcl::compile-file-if-needed file)
  (load file))

(setf cl-fluidsynth-files '("fluidsynth_ffi" "cl-fluidsynth" "fluidsynth_external"))

;; (dolist (file cl-fluidsynth-files)
;;   (om::compile&load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

(dolist (file cl-fluidsynth-files)
  (compile?-and-load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

(oa::om-add-init-func 'cl-fluid-init-fluidsynth)

(provide :cl-fluidsynth)
