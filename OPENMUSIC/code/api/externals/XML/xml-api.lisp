
(in-package :oa)

(export '(om-list-from-xml-file) :oa)

(defun om-list-from-xml-file (file)
  (when (probe-file file)
    (with-open-file (f file :direction :input :element-type 'character)
      (om-list-from-xml f))))

(defun om-list-from-xml (stream)
  (s-xml::parse-xml-dom stream :lxml))

