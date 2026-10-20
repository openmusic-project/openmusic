(in-package :alloy-desktop)

(defun find-definitions (symbol &key (package (or (symbol-package symbol)
                                                  (find-package :common-lisp-user)))
                                     (readtable *readtable*))
  "Return a (label pathname) list of the recorded source locations of SYMBOL's definitions; the pathnames are as recorded by the compiler and may not exist."
  (let ((raw (let ((swank::*buffer-package* package)
                   (swank::*buffer-readtable* readtable))
               (swank:find-definitions-for-emacs (symbol-name symbol)))))
    (loop for (label location) in raw
          for file = (and (consp location) (eq :location (first location))
                          (second (find :file (rest location)
                                        :key (lambda (x) (and (consp x) (first x))))))
          when file
          collect (list label (pathname file)))))
