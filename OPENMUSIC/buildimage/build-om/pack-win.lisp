
(in-package "CL-USER")


;;; :ub :intel :win :src

(defvar *release* nil)	

(setf *release* :win)


(load (make-pathname :directory (pathname-directory *load-pathname*)
 :name "make-package" :type "lisp"))





