(in-package :oa)

(let ((dir (pathname-directory (truename *load-pathname*))))
  (dolist (f '("om-text-editor" "treeeditor" "tempoeditor" "infoeditor" "treetempoeditor"
               "commenteditor" "om-table-editor" "export"))
    (cl-user::compile&load (make-pathname :directory dir :name f))))
