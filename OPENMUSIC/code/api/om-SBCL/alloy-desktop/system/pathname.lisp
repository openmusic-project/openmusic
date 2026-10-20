(in-package :alloy-desktop)

(defun current-pathname (&optional relative-pathname (type nil type-supplied-p))
  "Resolve RELATIVE-PATHNAME against the file currently being loaded or compiled.

This is the reusable desktop/runtime equivalent of LispWorks CURRENT-PATHNAME.
When TYPE is supplied it overrides the result pathname type, including NIL."
  (let ((ref (or *load-truename* *load-pathname*
                 *compile-file-truename* *compile-file-pathname*
                 *default-pathname-defaults*)))
    (if (null relative-pathname)
        ref
        (let* ((str (if (stringp relative-pathname)
                        relative-pathname
                        (namestring relative-pathname)))
               (parts (loop for start = 0 then (1+ pos)
                            for pos = (position #\/ str :start start)
                            collect (subseq str start pos)
                            while pos)))
          (make-pathname
           :host (pathname-host ref)
           :device (pathname-device ref)
           :directory (append (pathname-directory ref) (butlast parts))
           :name (car (last parts))
           :type (if type-supplied-p type (pathname-type ref)))))))

(defun directory-pathname-p (pathname)
  "Return true when PATHNAME denotes a directory pathname."
  (uiop:directory-pathname-p pathname))

(defun directory-entries (pathname)
  "Return the immediate files and subdirectories of PATHNAME as one list.

PATHNAME may be a pathname or namestring. The operation is not recursive."
  (let ((dir (if (pathnamep pathname) pathname (parse-namestring pathname))))
    (append (uiop:directory-files dir)
            (uiop:subdirectories dir))))
