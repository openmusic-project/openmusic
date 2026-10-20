(in-package :cl-user)

(pushnew (merge-pathnames "alloy-desktop/"
                          (make-pathname :name nil :type nil :defaults *load-truename*))
         asdf:*central-registry*
         :test #'equal)
(ql:quickload :alloy-desktop)
