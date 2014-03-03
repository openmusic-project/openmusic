
(in-package "CL-USER")


;;; :ub :intel :src

(defvar *release* nil)
(setf *release* :intel)


(load (make-pathname :directory (pathname-directory *load-pathname*)
 :name "make-package" :type "lisp"))





