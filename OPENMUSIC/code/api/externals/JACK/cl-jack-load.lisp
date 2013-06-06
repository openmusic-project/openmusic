(unless (find :cffi-new *features*)
  (require :cffi (make-pathname :directory (append om-api::*externals-directory* '("FFI")) :name "load-new-cffi")))

(defun compile?-and-load (file)
  (compile-file-if-needed file)
  (load file))

(setf cl-jack-files '("cl-jack" "cl-fluidsynth.lisp" "cl-jack-midi" "cl-jack-audio"))

(dolist (file cl-jack-files)
  (compile?-and-load (make-pathname :directory (pathname-directory *load-pathname*) :name file)))

(provide :cl-jack)

