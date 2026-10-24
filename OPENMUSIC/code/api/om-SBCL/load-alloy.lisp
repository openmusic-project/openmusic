(in-package :cl-user)

(require :asdf)

(let ((root (merge-pathnames "alloy-desktop/"
                             (make-pathname :name nil :type nil :defaults *load-truename*))))
  (asdf:initialize-source-registry
   `(:source-registry
     (:directory ,root)
     (:tree ,(merge-pathnames "DEPENDENCIES/" root))
     :ignore-inherited-configuration)))

(asdf:load-system "alloy-desktop")
