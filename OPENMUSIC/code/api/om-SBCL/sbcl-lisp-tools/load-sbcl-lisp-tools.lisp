(in-package :oa)

(let ((dir (pathname-directory (truename *load-pathname*))))
  (dolist (f '("textbuffer" "find-definition" "eval-process" "eval-support" "output"
               "texteditor" "lisp-mode" "completion" "lisp-edit-view" "listener"
               "syntax-coloring" "editor-find-dialogs" "export"))
    (cl-user::compile&load (make-pathname :directory dir :name f))))
