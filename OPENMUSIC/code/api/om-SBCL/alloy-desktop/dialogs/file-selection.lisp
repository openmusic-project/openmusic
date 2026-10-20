(in-package :alloy-desktop)

;;; Path policy around the native file choosers: the filter a caller writes as
;;; glob patterns becomes the one-extension-per-entry filter every file-select
;;; backend expects, and the CAPI prompt-for-file contract (filters plist, full
;;; pathname, post-selection checks) is applied to the chosen file.

(defun normalize-file-pattern (pattern)
  "Strip a leading \"*.\" from a single glob PATTERN, yielding the raw
   extension the file-select backends re-glob (win32 \"*.~a\", macOS
   allowedFileTypes, gtk \"*.~a\", zenity \"*.~a\"); \"*.*\" -> \"*\" (all
   files). Passing a glob would double-star it."
  (if (and (>= (length pattern) 2) (string= (subseq pattern 0 2) "*."))
      (subseq pattern 2)
      pattern))

(defun split-file-pattern (pattern)
  "Split a glob PATTERN on #\\; into its raw extensions. A caller may pack
   several globs into one pattern string, e.g. \"*.aiff;*.aif\", while every
   file-select backend expects ONE bare extension per filter entry."
  (loop with start = 0
        for semi = (position #\; pattern :start start)
        collect (normalize-file-pattern
                 (subseq pattern start (or semi (length pattern))))
        while semi
        do (setf start (1+ semi))))

(defun types->filter (types)
  "Convert TYPES = (label1 pattern1 label2 pattern2 ...) to
   file-select FILTER = ((label1 ext1a) (label1 ext1b) (label2 ext2) ...): one
   entry per raw extension. A single glob per label yields byte-identical
   output. The (AND LABEL PATTERN) guard drops an incomplete trailing pair
   rather than emitting a NIL pattern into the backend."
  (loop for (label pattern) on types by #'cddr
        when (and label pattern)
          nconc (loop for ext in (split-file-pattern pattern)
                      collect (list label ext))))

(defun as-directory-pathname (path)
  "Normalize PATH to a directory-shaped pathname, folding any name/type into the
   directory list. Idempotent for already-directory pathnames. It serves the reply of
   the Save dialog that NAMES a new directory, which is file-shaped by design; a
   :directory selection already comes back directory-shaped from every backend
   (file-select/protocol.lisp: the normalize-selection reached by the existing-with /
   new-with :around methods)."
  (if (or (pathname-name path) (pathname-type path))
      (make-pathname :host (pathname-host path)
                     :device (pathname-device path)
                     :directory (append (or (pathname-directory path) '(:relative))
                                        (list (if (pathname-type path)
                                                  (format nil "~a.~a"
                                                          (pathname-name path) (pathname-type path))
                                                  (pathname-name path))))
                     :name nil :type nil)
      path))

(defun dialog-title (prompt default)
  "TITLE for file-select, which needs a STRING in every backend
(file-select/win32.lisp: the file-dialog-set-title of open*): an
explicit :prompt NIL from the caller falls back to DEFAULT."
  (if (stringp prompt) prompt default))

;;; CAPI prompt-for-file: FILTERS is a plist
;;; (name expression ...); FILTER selects one expression and is prepended under
;;; "Files" when no entry carries it.

(defun file-filter-name-for (filter filters)
  (when filters
    (if filter
        (or (loop for tail on filters by #'cddr
                  when (and (stringp (second tail))
                            (string-equal (second tail) filter))
                    return (first tail))
            "Files")
        (first filters))))

(defun normalize-file-filters (filter filters)
  (when (and filters (oddp (length filters)))
    (error "CAPI:PROMPT-FOR-FILE :FILTERS is not a property list: ~S"
           filters))
  (let ((result (copy-list filters)))
    (when (and filter
               (not (loop for (name expression) on filters by #'cddr
                          thereis (and name (stringp expression)
                                       (string-equal expression filter)))))
      (setf result (list* "Files" filter result)))
    result))

(defun full-file-pathname (result pathname)
  (when result
    (merge-pathnames (pathname result)
                     (or pathname *default-pathname-defaults*))))

(defun check-file-selection
    (filename operation if-exists if-does-not-exist ok-check)
  (when filename
    (let ((exists (probe-file filename)))
      (when (and (not exists)
                 (eq if-does-not-exist :error))
        (error 'file-error :pathname filename))
      ;; The native save panel implements :PROMPT itself.  Post-selection
      ;; validation still distinguishes :OK and enforces every non-GUI rule.
      (when (and (eq operation :save) exists
                 (not (member if-exists '(:ok :prompt))))
        (error "Invalid CAPI :IF-EXISTS policy ~S." if-exists))
      (when (and ok-check (not (funcall ok-check filename)))
        (return-from check-file-selection nil))
      filename)))
