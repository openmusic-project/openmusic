
;;; LOAD CFFI FILES


(defvar *cffi-files* '(
                    "utils"
                    "features"
                    "cffi-lispworks"
                    "package"
                    "libraries"
                    "early-types"
                    "types"
                    "enum"
                    "strings"
                    "functions"
                    "foreign-vars"
                    ))

(defvar *current-directory* nil)
(setf *current-directory* (pathname-directory *load-pathname*))

(mapc (lambda (filename)
        (let ((file (make-pathname :directory *current-directory* :name filename)))
          (compile-file file)
          (load file)))
      *cffi-files*)



