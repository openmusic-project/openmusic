(in-package :cl-user)

(defvar *alloy-editor-directory* nil)
(setf *alloy-editor-directory* (pathname-directory (truename *load-pathname*)))

(defvar *alloy-editor-files*
  '("package" "buffer" "document" "layout" "policy" "view" "commands" "lisp-mode"
    "output" "input")
  "The sources in dependency order, the same order and the same list the system
definition carries.  The toolkit, BABEL, BORDEAUX-THREADS and TRIVIAL-GRAY-STREAMS
have to be loaded before this runs: the files past BUFFER read symbols of theirs.")

(defvar *alloy-editor-compile* nil
  "True loads the folder through COMPILE-FILE and a fasl, as the API build does; NIL loads the sources and writes nothing.")

(defun load-alloy-editor (&key (compile *alloy-editor-compile*) (verbose t))
  "Load the alloy-editor sources in dependency order."
  (dolist (name *alloy-editor-files*)
    (let ((source (make-pathname :directory *alloy-editor-directory*
                                 :name name :type "lisp")))
      (if compile
          (let* ((fasl (compile-file-pathname source))
                 (out-of-date (and (probe-file fasl)
                                   (> (file-write-date source) (file-write-date fasl)))))
            (when (or (not (probe-file fasl)) out-of-date)
              (compile-file source :verbose verbose))
            (load fasl :verbose verbose))
          (load source :verbose verbose)))))

(load-alloy-editor)
