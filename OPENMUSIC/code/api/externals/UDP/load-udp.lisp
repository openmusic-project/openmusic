
(in-package :cl-user)

(load (make-pathname :directory (append (pathname-directory *load-pathname*) '("lispworks-udp")) :name "lispworks-udp.asd"))

(asdf:operate 'asdf:load-op 'lispworks-udp)
