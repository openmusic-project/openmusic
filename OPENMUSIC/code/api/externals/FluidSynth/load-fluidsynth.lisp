;; common lisp - fluidsynth connection kit, supports 'internal' fluidsynth,

(defpackage :cl-fluidsynth (:use :common-lisp :cffi))
(in-package :cl-fluidsynth)

;;; swig-generated FFI-wrappers for {include}/fluidsynth.h,
;;; {include}/fluidsynth/*.h - spec in ./fluidsynth.i

(defun compile?-and-load (file)
  #+lispworks (hcl::compile-file-if-needed file)
  (load file))

(setf cl-fluidsynth-files '("fluidsynth_ffi" "cl-fluidsynth" "fluidsynth_external"))

;; (dolist (file cl-fluidsynth-files)
;;   (om::compile&load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

(dolist (file cl-fluidsynth-files)
  (compile?-and-load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

(defun cl-fluid-init-fluidsynth ()
  (cl-fluid-init-fluidsynth-ffi)
  (cl-fluid-setup-fluidsynth)
  ;;(fluid_version_str)
  )


(oa::om-add-init-func 'cl-fluid-init-fluidsynth)

(pushnew :cl-fluidsynth *features*)
(provide :cl-fluidsynth)
