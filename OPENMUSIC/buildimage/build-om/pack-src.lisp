
(in-package "CL-USER")

;;; :ub :intel :src
(defvar *release* nil)	
(setf *release* :src)

(load (make-pathname :directory (pathname-directory *load-pathname*)
		     :name "make-package" :type "lisp"))





