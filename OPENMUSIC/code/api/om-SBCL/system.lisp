(in-package :om-api)

(export '(om-add-init-func
          om-init-funcall
          om-add-exit-cleanup-func
          om-exit-funcall
          *om-os*
          throw-cancel
          catch-cancel
          om-confirmed-quit
          om-eval-enqueue
          om-set-eval-process
          om-delayed-funcall
          om-gc
          om-with-redefinitions
          om-redefinition-warnings
          om-set-redefinition-warnings
          om-with-redefinition-warnings
          om-compiler-warnings
          om-set-compiler-warnings
          om-with-compiler-warnings
          om-load-verbose
          om-set-load-verbose
          om-with-load-verbose
          *om-separator*
          *om-compiled-type*
          directoryp
          directory-pathname-p
          systemdirectoryp
          om-make-pathname
          om-create-directory
          om-create-file
          om-copy-file
          om-copy-directory
          om-delete-directory
          om-delete-file
          stream-eofp
          om-load-file
          om-directory
          om-read-line
          om-read-file
          om-correct-line
          om-namestring
          *om-root*
          om-get-user-name
          om-user-home
          om-user-pref-folder
          om-get-date
          om-command-line
          om-term-cmd
          om-run-application
          om-run-program
          *om-open-cmd*
          om-find-process
          om-select-program
          om-run-process
          om-kill-process
          om-with-priority
          om-with-new-gc
          om-without-interrupts
          om-process-state
          om-error-handle-funcall
          om-with-error-handle
          om-standalone-p
          om-set-clipboard
          om-get-clipboard
          create-file
          create-directory
          create-folder
          om-path2cmdpath)
        :om-api)

;;; OS detection

(defvar *om-os* nil)
(setf *om-os*
      (cond ((uiop:os-macosx-p) :mac)
            ((uiop:os-windows-p) :win)
            ((uiop:os-unix-p) :linux)
            (t :unknown)))

;;; THERE IS NO LISPWORKS VERSION IN SBCL!!!!
(defparameter *lw-version*
  (read-from-string (subseq (lisp-implementation-version) 0 1)))

;;; Catch/throw cancel

(defmacro catch-cancel (&body body)
  `(catch :cancel ,@body))

(defmacro throw-cancel (body)
  `(throw :cancel ,body))

;;; Clipboard
;;; The reusable OS clipboard implementation lives in ALLOY-DESKTOP. OM keeps
;;; only its public compatibility wrappers.

(defun om-set-clipboard (value)
  (when (stringp value)
    (alld:clipboard-write-text (alld:default-clipboard-service) value)))

(defun om-get-clipboard ()
  (alld:clipboard-read-text (alld:default-clipboard-service)))

;;; Error handlers

(defun om-error-handle-funcall (func)
  (handler-bind ((error (lambda (err)
                          (om-message-dialog
                           (format nil "An error of type ~a occurred: ~a"
                                   (type-of err) (format nil "~A" err)))
                          (abort err))))
    (funcall func)))

(defmacro om-with-error-handle (&body body)
  `(if (om-standalone-p)
       (handler-bind ((error (lambda (err)
                               (om-message-dialog
                                (format nil "An error of type ~a occurred: ~a"
                                        (type-of err) (format nil "~A" err)))
                               (abort err))))
         ,@body)
       (progn ,@body)))

(defparameter *log-location* nil)

;;; Bind this function to *debugger-hook* in order to catch all unexpected errors...
(defun om-debugger-hook (condition old-debugger-hook)
  (declare (ignore old-debugger-hook))
  (let ((log (om-y-or-n-dialog
              (format nil "An  error occured : ~a~%~%Display Log file ?" condition)
              :default-button nil)))
    (when log
      ;; LW dbg::logs-directory parent maps to the folder above om-user-pref-folder
      (let* ((pref (om-user-pref-folder))
             (logpath (make-pathname
                       :host (pathname-host pref) :device (pathname-device pref)
                       :directory (append (butlast (pathname-directory pref))
                                          (list "OpenMusic"))
                       :name (concatenate
                              'string "OM-"
                              ;; cl-user::*version-str* is defined by the kernel loader
                              (substitute #\- #\. (symbol-value 'cl-user::*version-str*)) "-Log_"
                              (substitute #\- #\: (substitute #\- #\/ (substitute #\_ #\Space (om-get-date))))))))
        (ensure-directories-exist logpath)
        ;; LW dbg:log-bug-form equivalent: condition + backtrace into the log file
        (with-open-file (s logpath :direction :output :if-exists :supersede)
          (format s "===   Log at ~a  ===~2%" (om-get-date))
          (format s "An error occured : ~a~2%" condition)
          (sb-debug:print-backtrace :stream s :count 128))
        (om-open-new-text-file logpath)
        (unless *log-location*
          (setf *log-location* t)
          (om-message-dialog
           (format nil "The log file has been written in ~a" logpath)))))
    (abort)))

;;; GC + standalone

(defun om-gc ()
  (sb-ext:gc :full t))

(defun om-standalone-p ()
  (if (member :om-deliver *features*) t nil))

;; LW (setf sys::*stack-overflow-behaviour* nil) grew the stack silently; SBCL's control stack is fixed at startup (no runtime equivalent)

;;; Init / exit hooks

(defvar *init-func-list* nil)

(defun om-add-init-func (func-name)
  (unless (member func-name *init-func-list* :test 'equal)
    (push func-name *init-func-list*)))

(defun om-init-funcall ()
  (mapc 'funcall (reverse *init-func-list*)))

(defvar *om-cleanup-forms* nil)

(defun om-add-exit-cleanup-func (func-name &optional last?)
  (unless (member func-name *om-cleanup-forms* :test 'equal)
    (if last?
        (push func-name *om-cleanup-forms*)
        (setf *om-cleanup-forms* (append *om-cleanup-forms* (list func-name))))))

(defvar *cleanup-done* nil)

(defun om-exit-funcall ()
  ;; idempotent after a fully clean run (LW quit-machinery and quit-om-callback
  ;; may both reach here on quit); a failed cleanup leaves it retryable
  (unless *cleanup-done*
    (let ((clean t))
      (dolist (fn (reverse *om-cleanup-forms*))
        ;; cleanups are independent teardowns: log the failing one, run the rest
        (handler-case (funcall fn)
          (serious-condition (c)
            (setf clean nil)
            (format *error-output* "[om-exit-funcall] ~A: ~A~%"
                    fn (princ-to-string c)))))
      (setf *cleanup-done* clean)))
  t)

(defvar *confirmed-quit* nil)

(defvar *om-quit-actions* nil
  "LW \"Confirm when quitting image\" action list (build/build-om.lisp: its define-action): each
function is called on quit; any NIL return aborts the quit.")

(defun om-confirmed-quit ()
  (unless *confirmed-quit*
    (setf *confirmed-quit* t)
    (unwind-protect
        (when (loop for fn in (reverse *om-quit-actions*)
                    ;; LW quits with :ignore-errors-p t (om-LW/system.lisp:
                    ;; om-confirmed-quit; lw-lispworks-89.htm:65)
                    always (handler-case (funcall fn)
                             (error (c)
                               (format *error-output* "[om-quit] ~A: ~A~%"
                                       fn (princ-to-string c))
                               t)))
          ;; LW's (quit) ran cleanup via its action machinery; on SBCL we run it
          ;; here so players/audio/midi close regardless of the save decision
          (om-exit-funcall)
          (sb-ext:exit :code 0 :abort nil))
      (setf *confirmed-quit* nil))))

;;; Thread initial bindings

(defun %make-om-thread (function &key name)
  "Start a thread on FUNCTION with the OM initial bindings and only :divide-by-zero armed, the LispWorks process-wide trap set the kernel relies on; returns the thread."
  (alld:make-desktop-thread function :name name
                                     :initial-bindings *process-initial-bindings*
                                     :armed-traps '(:divide-by-zero)))

;;; Eval enqueue

(defparameter *use-eval-process* t)

(defun om-set-eval-process (mode)
  (setq *use-eval-process* mode))

;;; UI executor — ALLOY-DESKTOP is the single owner of GLFW/window/GL work.
;;; OM keeps only its task-execution policy (%run-ui-task).

(defun %run-ui-task (thunk)
  "Run one generic UI task under OpenMusic's historical task error policy."
  ;; Tasks are independent: one failure or abort must not drop the rest. Queue
  ;; ownership, snapshot draining and recursive-drain protection live in ALLD.
  (alld:call-with-abort-restart
   (lambda ()
     (handler-case (funcall thunk)
       (serious-condition (c)
         (format *error-output* "[om-api task] ~A~%" (princ-to-string c)))))))

(defun om-eval-enqueue (form &optional view)
  (if *use-eval-process*
      (%enqueue-on-eval-process
       (lambda ()
         (om-listener-echo "Running...")
         ;; capi::apply-in-pane-process equivalent: post to the UI render-tick queue.
         ;; That thread writes the header and has no *package* binding of its own,
         ;; so only this branch pins; the worker inherits its generation's binding.
         (if view
             (alld:post-to-ui-thread (lambda () (let ((*package* (find-package :om))) (eval form))))
             (eval form))
         (om-listener-echo "Ready")))
      (eval form)))

(om-add-exit-cleanup-func 'om-abort-eval-process)

;;; Scheduler / delayed funcall

(defvar *scheduler-timer* nil)

(defun scheduler-fun () t)

(defun om-delayed-funcall (time func &rest args)
  ;; ALLOY-DESKTOP owns timer scheduling. OM keeps only its thread-entry policy:
  ;; inherited package/dynamic bindings and the LispWorks-compatible float mask.
  (alld:start-timer (alld:default-timer-service)
                    (/ time 1000.0)
                    (alld:desktop-thread-entry
                     (lambda () (apply func args))
                     :initial-bindings *process-initial-bindings*
                     :armed-traps '(:divide-by-zero))))

;;; Redefinition / compiler / load verbose

(defmacro om-with-redefinitions (&body body)
  ;; without-package-locks is runtime-only (SBCL src/code/package.lisp: the
  ;; without-package-locks macro); compile-time
  ;; proclaims of locked names need the lexical disable-package-locks declaration.
  (let ((names (loop for form in body
                     when (and (consp form) (symbolp (car form))
                               (>= (length (symbol-name (car form))) 3)
                               (string= (symbol-name (car form)) "DEF" :end1 3)
                               (symbolp (second form)))
                       collect (second form)))
        (wrapped `(handler-bind ((sb-kernel:redefinition-warning
                                  #'muffle-warning))
                    (sb-ext:without-package-locks ,@body))))
    (if names
        `(locally (declare (sb-ext:disable-package-locks ,@names))
           ,wrapped)
        wrapped)))

(defvar *redefinition-warnings* t)

(defun om-redefinition-warnings () *redefinition-warnings*)

;; muffle redefinition warnings iff the policy flag is currently off (T lets them show)
(defmacro %honoring-redefinition-warnings (&body body)
  `(handler-bind ((sb-kernel:redefinition-warning
                   (lambda (c) (unless *redefinition-warnings* (muffle-warning c)))))
     ,@body))

(defun om-set-redefinition-warnings (t-or-nil)
  (setf *redefinition-warnings* (and t-or-nil t))
  ;; compile-time layer; runtime (load/eval) is gated by %honoring-redefinition-warnings
  (if t-or-nil
      (proclaim '(sb-ext:unmuffle-conditions sb-kernel:redefinition-warning))
      (proclaim '(sb-ext:muffle-conditions sb-kernel:redefinition-warning)))
  *redefinition-warnings*)

(defmacro om-with-redefinition-warnings (t-or-nil &body body)
  ;; NIL suppresses redefinition warnings for the dynamic extent; non-NIL lets them show
  `(let ((*redefinition-warnings* ,t-or-nil))
     (%honoring-redefinition-warnings ,@body)))

(defun %om-redefinition-location-unchanged-p (warning)
  "T when the old and the new definition share a source location outside any file.
CAPI reports a redefinition only when the recorded location changes and code
evaluated outside a file keeps one keyword location, so OM's regenerated patch
code stays quiet; SBCL instead treats an unknown location as always different."
  (flet ((fun-file (f)
           (and (functionp f) (sb-kernel::function-file-namestring f)))
         (loc-file (l)
           (and l (sb-c::definition-source-location-namestring l)))
         (bound (c slot)
           (and (slot-boundp c slot) (slot-value c slot))))
    (typecase warning
      (sb-kernel:redefinition-with-defmethod
       (let ((old (bound warning 'sb-kernel::old-method)))
         (and old
              (null (loc-file (sb-pcl::definition-source old)))
              (null (loc-file (bound warning 'sb-kernel::new-location))))))
      (sb-kernel::function-redefinition-warning
       (let* ((name (bound warning 'sb-kernel::name))
              ;; NAME is read off the warning object, so it may name nothing here
              (old (and name
                        (if (and (symbolp name) (macro-function name))
                            (macro-function name)
                            (and (fboundp name) (fdefinition name)))))
              (new (bound warning 'sb-kernel::new-function)))
         (and old new (null (fun-file old)) (null (fun-file new))))))))

(setf sb-ext:*muffled-warnings*
      '(or sb-kernel:uninteresting-redefinition
           (satisfies %om-redefinition-location-unchanged-p)))

(defun om-compiler-warnings () t)
(defun om-set-compiler-warnings (t-or-nil) (declare (ignore t-or-nil)) t)
(defmacro om-with-compiler-warnings (t-or-nil &body body)
  (declare (ignore t-or-nil))
  `(progn ,@body))

(defun om-load-verbose () *load-verbose*)
(defun om-set-load-verbose (t-or-nil) (setf *load-verbose* t-or-nil))
(defmacro om-with-load-verbose (t-or-nil &body body)
  `(let ((*load-verbose* ,t-or-nil)) ,@body))

;;; Files

(defvar *om-separator* "/")

(defparameter *om-compiled-type* "fasl")
  ;(pathname-type (compile-file-pathname "x.lisp"))) ???

(defvar *om-root* nil)

(defun om-root-init ()
  (if (om-standalone-p)
      (progn
        (setf *om-root*
              (make-pathname
               :device (pathname-device sb-ext:*runtime-pathname*)
               :host (pathname-host sb-ext:*runtime-pathname*)
               :directory
               (case *om-os*
                 ;; vanilla #+cocoa: climb out of <app>.app/Contents/MacOS
                 (:mac (butlast (pathname-directory (truename sb-ext:*runtime-pathname*)) 3))
                 ;; /usr|/opt image -> <parent>/share/openmusic-sbcl
                 (:linux (let ((dir (pathname-directory (truename sb-ext:*runtime-pathname*))))
                           (cond ((equal (last dir 2) '("share" "openmusic-sbcl")) dir)
                                 ((member (second dir) '("usr" "opt") :test #'string=)
                                  (append (butlast dir) '("share" "openmusic-sbcl")))
                                 (t dir))))
                 (t (pathname-directory (truename sb-ext:*runtime-pathname*))))))
        (setf (symbol-value 'cl-user::*om-src-directory*) *om-root*))
      (setf *om-root*
            (or (symbol-value 'cl-user::*om-src-directory*) *om-root*)))
  #+linux
  (pushnew (merge-pathnames "resources/lib/linux/" *om-root*)
           cffi:*foreign-library-directories* :test #'equal))

(om-api-add-init-func 'om-root-init)

;; captured at load: the api folder of THIS bridge (vanilla hardcodes "OM-LW")
(defparameter *om-api-dir-name*
  (or (and *load-pathname* (car (last (pathname-directory *load-pathname*))))
      "om-SBCL"))

(defun set-om-logical-path ()
  (let* ((image (or sb-ext:*runtime-pathname* (user-homedir-pathname)))
         (image-dir (pathname-directory image))
         (root-path (make-pathname
                     :device (pathname-device image)
                     :host (pathname-host image)
                     :directory (case *om-os*
                                  (:mac (butlast image-dir 3))
                                  (t image-dir))))
         (path-code (make-pathname
                     :device (pathname-device root-path)
                     :host (pathname-host root-path)
                     :directory (append (pathname-directory root-path) (list "CODE"))))
         (path-api (make-pathname
                    :device (pathname-device root-path)
                    :host (pathname-host root-path)
                    :directory (append (pathname-directory root-path)
                                       (list "CODE" "API" *om-api-dir-name*)))))
    (when (om-standalone-p)
      (setf (logical-pathname-translations "CL")
            (list (list "**;*.*.*" (format nil "~A**/*.*" (namestring root-path)))))
      (setf (logical-pathname-translations "code")
            (list (list "**;*.*.*" (format nil "~A**/*.*" (namestring path-code)))))
      (setf (logical-pathname-translations "api")
            (list (list "**;*.*.*" (format nil "~A**/*.*" (namestring path-api))))))))

(om-api-add-init-func 'set-om-logical-path)

(defvar *volume-sys* nil)

(defun init-volume-var ()
  t)

(defun real-pathname-name1 (path)
  (let ((type (pathname-type path)))
    (if (or (equal type :UNSPECIFIC) (not type))
        (om-namestring (pathname-name path))
        (concatenate 'string (om-namestring (pathname-name path)) "." type))))

(defun corrige-path-space (name) name)

(defun directoryp (p)
  (alld:directory-pathname-p p))

(defun directory-pathname-p (p)
  (alld:directory-pathname-p p))

(defun systemdirectoryp (p)
  (and (alld:directory-pathname-p p)
       (let ((last (car (last (pathname-directory p)))))
         (and (stringp last)
              (> (length last) 0)
              (char= (char last 0) #\.)))))

(defun om-make-pathname (&key directory name type host device)
  ;; LW keeps :unspecific in the host/device slots of a relative pathname and its
  ;; make-pathname accepts it back; SBCL requires (or string host null) for :host.
  ;; Patches saved by older OM store ":device :unspecific :host :unspecific"
  ;; verbatim, so the form re-read at load time signalled TYPE-ERROR.
  (when (eq host :unspecific) (setf host nil))
  (when (eq device :unspecific) (setf device nil))
  ;; On SBCL, a string host like "C" is treated as a logical host name
  ;; (SBCL src/code/target-pathname.lisp: the with-host macro, which dispatches to
  ;; find-logical-host, which signals).  LW passes "C" to mean a Windows drive
  ;; letter, which must become
  ;; :device.  The drive may arrive in :host alone (om-save-pathname in
  ;; kernel-patch-after.lisp) or in :host AND :device at once (kernel
  ;; graphics/icons/picture.lisp: omNG-save for internepicture passes
  ;; pathname-device to both), so the letter is dropped from :host either way and
  ;; only fills an empty :device.
  (when (and (stringp host) (= 1 (length host)) (alpha-char-p (char host 0)))
    (unless device (setf device host))
    (setf host nil))
  ;; LispWorks names a :type without :name dir/.TYPE (om-LW/system.lisp:
  ;; om-make-pathname); SBCL has no native namestring for it, so the dotfile is spelled out here.
  (when (and (null name) (stringp type) (plusp (length type)))
    (setf name (concatenate 'string "." type)
          type nil))
  (make-pathname
   :host (or host
             (and device (pathnamep device) (pathname-host device))
             (and directory (pathnamep directory) (pathname-host directory)))
   :device (cond ((and device (pathnamep device)) (pathname-device device))
                 (device device)
                 ((and directory (pathnamep directory)) (pathname-device directory)))
   :directory (if (pathnamep directory) (pathname-directory directory) directory)
   :name name
   :type type))

(defun om-create-file (pathname)
  ;; vanilla empty body: creates the file, returns NIL (never a closed stream)
  (with-open-file (s pathname :direction :output :if-does-not-exist :create)
    (declare (ignorable s))))

(defun om-create-directory (pathname &key (if-exists nil))
  (declare (ignore if-exists))
  (ensure-directories-exist pathname))

(defun create-file (path) (om-create-file path))
(defun create-directory (path) (om-create-directory path))
(defun create-folder (path) (om-create-directory path))

(defun om-copy-file (sourcepath targetpath &key (if-exists :supersede))
  (declare (ignore if-exists))
  (handler-bind ((error (lambda (err)
                          (om-message-dialog
                           (format nil "An error of type ~a occurred: ~a"
                                   (type-of err) (format nil "~A" err)))
                          (abort err))))
    ;; uiop:copy-file already supersedes (:if-exists :rename-and-delete)
    (uiop:copy-file sourcepath targetpath)
    targetpath))

(defun om-copy-directory (sourcedir targetpath)
  (case *om-os*
    ((:mac :linux)
     (uiop:run-program (list "cp" "-R" (namestring sourcedir) (namestring targetpath))))
    (:win
     (apply #'uiop:run-program (list "robocopy" (namestring sourcedir) (namestring targetpath)
                                     "/E" "/NFL" "/NDL" "/NJH" "/NJS")
            :ignore-error-status t (alld:hidden-console-options)))))

(defun om-delete-directory (path)
  (if (uiop:directory-pathname-p path)
      (uiop:delete-directory-tree path :validate t :if-does-not-exist :ignore)
      ;; file-error -> nil mirrors LW (delete-file path :no-error)
      (handler-case (delete-file path) (file-error () nil))))

(defun om-delete-file (name)
  (when (and name (probe-file name))
    ;; file-error -> nil mirrors LW (delete-file name :no-error)
    (handler-case (delete-file name) (file-error () nil))))

;;; --------------------------------------------------------------------------
;;; File encodings — LispWorks parity
;;;
;;; Neither the kernel nor om-LW ever passes :external-format when reading or
;;; writing an OM document (om-LW/system.lisp: om-load-file = (load path); kernel
;;; ompatch.lisp: omNG-save for OMPatch writes the .omp; kernel saveheader.lisp:
;;; get-pathname-wsparams, set-finder-comment, file-has-comments and
;;; file-has-old-comments read and
;;; rewrite it).  Under LispWorks the omitted argument means :external-format
;;; :default, which guess-external-format resolves through
;;; *file-encoding-detection-algorithm* (LW 8.1 manual lw/lw-common-lisp-36.htm:50,74-76;
;;; lw/lw-sys-35.htm:39-46; CLHS f_load.htm:32 for load).  The chain is:
;;;   find-filename-pattern-encoding-match — table has only "TAGS" (lw/lw-sys-38.htm:40)
;;;   find-encoding-option                 — EMACS -*- coding: -*- line (lw/lw-sys-39.htm:62)
;;;   detect-utf32-bom/detect-unicode-bom/detect-utf8-bom — BOM only (lw/lw-sys-28.htm:78)
;;;   specific-valid-file-encoding         — nil by default (lw/lw-sys-146.htm:70)
;;;   locale-file-encoding                 — (lw/lw-sys-83.htm:62)
;;; and locale-file-encoding yields (:latin-1 :encoding-error-action 63) whenever the
;;; Windows ANSI code page is 1252 or 28591 (lw/lw-win32-misc-7.htm:40-44); Latin-1 is
;;; also the documented default on the other platforms (editor/editor-commands-5.htm:175).
;;; Latin-1 "can read anything without any loss" (lw/lw-i18n-ug-7.htm:110) — that is why
;;; vanilla never signals on the accented bytes OM itself writes.
;;;
;;; SBCL's global default is :utf-8, so the very same kernel code signalled
;;; STREAM-DECODING-ERROR on those bytes.  The port therefore installs LW's
;;; default: (:latin-1 :replacement #\?) is the SBCL spelling of the LW spec — it decodes
;;; every octet and substitutes #\? for characters the encoding cannot write, exactly what
;;; :encoding-error-action 63 does.  Setting the SBCL global reaches every kernel
;;; with-open-file/load site at once, which is what LW's global default does too.

(defparameter *om-default-external-format* '(:latin-1 :replacement #\?)
  "SBCL spelling of LispWorks' locale-file-encoding result on a Latin-1 code page.")

(defparameter *om-specific-valid-file-encodings* '(:utf-8)
  "LW *specific-valid-file-encodings* (lw/lw-sys-146.htm:70, lw/lw-i18n-ug-7.htm:216):
encodings preferred over the locale default when the file is entirely valid in them.
The fork sets the knob because .omp files written under SBCL's global :utf-8 default
already exist on disk, so documents in either encoding keep their characters.")

(defun om-set-default-external-format ()
  (setf sb-ext:*default-external-format* *om-default-external-format*))

(om-api-add-init-func 'om-set-default-external-format)

(defun %om-bom-external-format (path)
  "LW detect-utf32-bom / detect-unicode-bom / detect-utf8-bom (lw/lw-sys-28.htm:78),
in that order: FF FE 00 00 matches both the UTF-32LE and the UTF-16LE mark, and LW
resolves it as UTF-32 because detect-utf32-bom runs first (lw/lw-sys-35.htm:56)."
  (with-open-file (s path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when s
      (let* ((n (min 4 (file-length s)))
             (b (make-array 4 :element-type '(unsigned-byte 8) :initial-element 0)))
        (read-sequence b s :end n)
        (flet ((at (i v) (and (> n i) (= (aref b i) v))))
          (cond ((and (at 0 #x00) (at 1 #x00) (at 2 #xFE) (at 3 #xFF)) :utf-32be)
                ((and (at 0 #xFF) (at 1 #xFE) (at 2 #x00) (at 3 #x00)) :utf-32le)
                ((and (at 0 #xFE) (at 1 #xFF))                         :utf-16be)
                ((and (at 0 #xFF) (at 1 #xFE))                         :utf-16le)
                ((and (at 0 #xEF) (at 1 #xBB) (at 2 #xBF))             :utf-8)))))))

(defun om-set-bom-reader-syntax ()
  "LW makes the Byte Order Mark whitespace in the default readtable, so the Lisp
reader reads a file whose encoding was detected from its BOM whether the mark is
present or not (lw/lw-i18n-ug-7.htm:247).  SBCL's external formats do not strip the
mark, so without this the BOM branch above would resolve the encoding correctly and
then hand U+FEFF to the reader as the first character of the document."
  (set-syntax-from-char (code-char #xFEFF) #\Space))

(om-set-bom-reader-syntax)

(defun %om-encoding-option-external-format (path)
  "LW find-encoding-option (lw/lw-sys-39.htm:62): an EMACS -*- attribute line naming
CODING / ENCODING / EXTERNAL-FORMAT on the first line of the file."
  (let ((line (with-open-file (s path :direction :input :if-does-not-exist nil
                                      :external-format :latin-1)
                (and s (read-line s nil nil)))))
    (when line
      (let* ((open (search "-*-" line))
             (close (and open (search "-*-" line :start2 (+ open 3)))))
        (when close
          (let ((body (subseq line (+ open 3) close)))
            (dolist (key '("external-format:" "encoding:" "coding:"))
              (let ((p (search key body :test #'char-equal)))
                (when p
                  (let* ((rest (string-trim " 	;" (subseq body (+ p (length key)))))
                         (end (position-if (lambda (c) (member c '(#\Space #\Tab #\;))) rest))
                         (name (string-upcase (subseq rest 0 end)))
                         (ef (and (plusp (length name)) (intern name :keyword))))
                    (when (and ef (sb-impl::get-external-format ef))
                      (return ef))))))))))))

(defun %om-file-decodes-p (path external-format)
  (handler-case
      (with-open-file (s path :direction :input :if-does-not-exist nil
                              :external-format external-format)
        (when s
          (let ((buf (make-string 8192)))
            (loop while (plusp (read-sequence buf s)))
            t)))
    (sb-int:stream-decoding-error () nil)))

(defun om-file-external-format (path)
  "LW guess-external-format (lw/lw-i18n-ug-7.htm:174) for an OM document, walked in the
order of *file-encoding-detection-algorithm* (lw/lw-sys-35.htm:41-47):

  find-filename-pattern-encoding-match  table holds only \"TAGS\" (lw/lw-sys-38.htm:40),
                                        so no OM document ever matches -- omitted
  find-encoding-option                  the EMACS -*- attribute line
  detect-utf32-bom detect-unicode-bom detect-utf8-bom
  specific-valid-file-encoding          *om-specific-valid-file-encodings*
  locale-file-encoding                  *om-default-external-format*

The attribute line comes BEFORE the BOM detectors: an explicit -*- coding: -*- wins.
The content-driven steps get a zero-length buffer when the file does not exist
(lw/lw-sys-35.htm:52), and specific-valid-file-encoding then returns the first element
of the list unconditionally (lw/lw-sys-146.htm:62)."
  (if (probe-file path)
      (or (%om-encoding-option-external-format path)
          (%om-bom-external-format path)
          (loop for ef in *om-specific-valid-file-encodings*
                when (%om-file-decodes-p path ef) return ef)
          *om-default-external-format*)
      (or (first *om-specific-valid-file-encodings*)
          *om-default-external-format*)))

;;; Writing OM documents -- the knob is SYMMETRIC, the way LispWorks' knob is
;;;
;;; No kernel save site passes :external-format (kernel ompatch.lisp: omNG-save for
;;; OMPatch; kernel ommaquette.lisp: omNG-save for OMMaquette; kernel
;;; saveheader.lisp: create-om-file, get-finder-comment, set-finder-comment and
;;; remove-old-comments; kernel save.lisp: omNG-save for OMInstance and
;;; save-instance; kernel omclass.lisp: omng-save for Omclass; kernel ommethod.lisp:
;;; omNG-save for OMMethod; kernel omworkspace.lisp: workspace-from-name,
;;; save-ws-ws-params, Make-one-new-WS-folder and save-omprefs; kernel
;;; preferences.lisp: save-preferences) -- every one of them is a bare
;;; with-open-file, so under
;;; LW guess-external-format decides, and it decides BY DIRECTION:
;;;
;;;   input   buffer holds the start of the file, so specific-valid-file-encoding
;;;           answers :utf-8 only when the content really is UTF-8; otherwise the
;;;           chain falls through to locale-file-encoding -> :latin-1
;;;   output  buffer has length 0 (lw/lw-sys-35.htm:52), so specific-valid-file-encoding
;;;           "will always" return the FIRST element of *specific-valid-file-encodings*
;;;           (lw/lw-sys-146.htm:62); that is already a complete spec, so the chain
;;;           STOPS there and locale-file-encoding never runs (lw/lw-sys-53.htm:64)
;;;
;;; With *specific-valid-file-encodings* = (:utf-8) -- the value this port needs,
;;; because .omp files written in UTF-8 exist -- LispWorks therefore WRITES UTF-8.
;;; Wiring that knob to the read side alone is not LW's knob: it made the fork read
;;; UTF-8 and write Latin-1, and a character above U+00FF that reached the image as a
;;; character came back as #\?.
;;;
;;; SBCL has one global for both directions and no way to define an external format
;;; in a running image (sb-impl::define-external-format,
;;; define-external-format/variable-width and add-external-format are macros of
;;; SBCL's own build: find-symbol answers NIL for all three in 2.6.6).  The direction
;;; has to come from the callsite instead, and every bare with-open-file goes through
;;; exactly one: CL:OPEN, which is what SBCL's with-open-file expands to.  The bridge
;;; below encapsulates it -- sb-int:encapsulate, the mechanism TRACE uses -- and
;;; supplies the format LW would have resolved, for OM documents only, and only when
;;; the caller named none.  Non-document files keep the global default untouched.

(defparameter *om-document-file-types*
  '("omp" "omm" "omc" "omi" "ome" "she" "oml" "oms" "om")
  "Pathname types of an OM document.  Mirrors the kernel's *om-doc-types*
(kernel tools/lisptools.lisp: *om-doc-types*) plus \"oms\" (projects
sheet-persistant.lisp: obj-file-extension for sheet-document) and \"om\"
(kernel modele.lisp: obj-file-extension for OMPersistantObject).  The predicate
unions this with the kernel list at run time, so
the two cannot drift apart.")

(defvar *om-resolving-external-format* nil
  "T while the chain is opening the file to resolve its encoding, so the bridge
does not try to resolve the encoding of the file it is already resolving.")

(defun %om-document-file-p (spec)
  (let ((path (handler-case (pathname spec)
                ;; not a pathname designator (type-error), a malformed namestring
                ;; (parse-error) or a stream with no file (file-error): not a document
                ((or type-error parse-error file-error) () nil))))
    (when path
      (let* ((type (pathname-type path))
             (kernel-types
               ;; late lookup: this file compiles before the kernel's OM package exists
               (handler-case (symbol-value (find-symbol "*OM-DOC-TYPES*" "OM"))
                 ((or package-error unbound-variable) () nil))))
        (or (and (stringp type)
                 (or (member type kernel-types :test #'string-equal)
                     (member type *om-document-file-types* :test #'string-equal)))
            (equal (handler-case (file-namestring path)
                     ;; no-namestring-error: this pathname has no printable file part
                     (file-error () nil))
                   ".finderinfo"))))))

(defparameter *om-lossless-write-encodings*
  '(:utf-8 :utf8 :utf-16 :utf-16be :utf-16le :utf-32 :utf-32be :utf-32le)
  "Encodings that can write every character OM can hold, so no substitution is
possible on output.")

(defvar *om-lossy-write-reported* nil
  "Encodings already reported by %om-report-lossy-write, so the warning is one per
encoding per session instead of one per save.")

(defun %om-report-lossy-write (path ef)
  "Choosing an encoding that cannot write every character is a decision that
must not be silent.  SBCL applies :replacement without signalling anything, so
the character itself is unreachable here; what is reachable, and what this
reports, is the file and the encoding that will replace it."
  (let ((name (if (consp ef) (first ef) ef)))
    (unless (or (member name *om-lossless-write-encodings*)
                (member name *om-lossy-write-reported*))
      (push name *om-lossy-write-reported*)
      (format *error-output*
              "~&; OM: writing ~S as ~S, which cannot encode every character; characters outside it are replaced (see *om-specific-valid-file-encodings*).~%"
              path ef)
      (finish-output *error-output*))))

(defun om-document-external-format (path direction)
  "The external format LispWorks' guess-external-format resolves for an OM document
opened in DIRECTION: the content-driven chain on input, and the first element of
*om-specific-valid-file-encodings* on output (lw/lw-sys-146.htm:62)."
  (if (member direction '(:output :io))
      (let ((ef (or (first *om-specific-valid-file-encodings*)
                    *om-default-external-format*)))
        (%om-report-lossy-write path ef)
        ef)
      (om-file-external-format path)))

(defun %om-character-stream-type-p (element-type)
  (or (eq element-type :default)
      (and (subtypep element-type 'character) t)))

(defun %om-open-document-encoding (open-fn &rest args)
  (let* ((spec (first args))
         (opts (rest args)))
    (if (or *om-resolving-external-format*
            (getf opts :external-format)
            (not (%om-character-stream-type-p (getf opts :element-type 'character)))
            (not (%om-document-file-p spec)))
        (apply open-fn args)
        (let ((ef (let ((*om-resolving-external-format* t))
                    (handler-case
                        (om-document-external-format spec (getf opts :direction :input))
                      (serious-condition (c)
                        (format *error-output*
                                "~&; OM: cannot resolve the external format of ~S: ~A~%"
                                spec (princ-to-string c))
                        (finish-output *error-output*)
                        nil)))))
          (if ef
              (apply open-fn spec :external-format ef opts)
              (apply open-fn args))))))

(defvar *om-document-encoding-bridge* nil
  "T while CL:OPEN carries the OM document encoding bridge.")

(defun om-install-document-encoding-bridge ()
  (unless *om-document-encoding-bridge*
    (sb-int:encapsulate 'open 'om-document-encoding #'%om-open-document-encoding)
    (setf *om-document-encoding-bridge* t)))

(defun om-remove-document-encoding-bridge ()
  (when *om-document-encoding-bridge*
    (sb-int:unencapsulate 'open 'om-document-encoding)
    (setf *om-document-encoding-bridge* nil)))

(om-api-add-init-func 'om-install-document-encoding-bridge)
(om-add-exit-cleanup-func 'om-remove-document-encoding-bridge)

(defun om-load-file (path)
  "LW parity: return the loaded file's pathname (kernel tests pathnamep for success).
The encoding is resolved the way LW resolves :external-format :default."
  (let ((sb-ext:*evaluator-mode* :interpret))
    (load path :external-format (om-file-external-format path)))
  (truename path))

(defun om-namestring (path) (namestring path))

(defun om-read-line (file) (read-line file nil 'eof))

(defun om-correct-line (line &optional stream)
  (declare (ignore stream))
  (if (stringp line)
      (if (> (length line) 0)
          (let ((last (elt line (1- (length line)))))
            (if (or (char= last #\Space) (char= last #\Linefeed))
                (om-correct-line (subseq line 0 (1- (length line))))
                line))
          line)
      line))

(defun om-read-file (file)
  ;; file-length counts OCTETS, so on a multibyte encoding the string vanilla
  ;; allocates (om-LW/system.lisp: om-read-file) is longer than the text and keeps a
  ;; tail of #\Nul; unreachable under LW's Latin-1, reachable here.
  (with-open-file (s file :direction :input :if-does-not-exist nil
                          :external-format (om-file-external-format file))
    (when s
      (let* ((str (make-string (file-length s)))
             (n (read-sequence str s)))
        (subseq str 0 n)))))

(defun stream-eofp (s)
  (let ((c (read-char s nil 'eof)))
    (unless (eq c 'eof) (unread-char c s))
    (eq c 'eof)))

(defun om-directory (path &key (type nil) (directories t) (files t)
                              (resolve-aliases nil) (hidden-files nil) (sorted? t))
  ;; One native scan, as vanilla's single DIRECTORY call (om-LW/system.lisp: OM-directory);
  ;; probe-file canonicalizes the prefix case once instead of a truename per entry.
  (let* ((dir (probe-file (uiop:ensure-directory-pathname path)))
         (rep (when dir
                (let ((acc nil))
                  (sb-ext:map-directory (lambda (entry) (push entry acc))
                                        dir
                                        :files files :directories directories
                                        :classify-symlinks nil :errorp nil)
                  ;; LW (directory ... :link-transparency t) equivalent
                  (when resolve-aliases
                    (setf acc (mapcar #'truename acc)))
                  (nreverse acc)))))
    (unless hidden-files
      (setf rep (remove-if
                 (lambda (item)
                   (or (and (directoryp item)
                            (let ((d (car (last (pathname-directory item)))))
                              (and (stringp d) (plusp (length d))
                                   (char= (char d 0) #\.))))
                       (and (stringp (pathname-name item))
                            (or (zerop (length (pathname-name item)))
                                (char= (char (pathname-name item) 0) #\.)))))
                 rep)))
    (when type
      (cond ((stringp type)
             (setf rep (remove-if-not
                        (lambda (i) (or (directoryp i)
                                        (string-equal (pathname-type i) type)))
                        rep)))
            ((consp type)
             (setf rep (remove-if-not
                        (lambda (i) (or (directoryp i)
                                        (member (pathname-type i) type
                                                :test 'string-equal)))
                        rep)))))
    (if sorted?
        (sort rep #'string< :key #'namestring)
        rep)))

;;; OS user / env / date

(defun om-get-user-name ()
  (alld:get-user-name))

(defun om-user-home ()
  (alld:user-home))

(defun om-user-pref-folder ()
  (let ((home (om-user-home)))
    (make-pathname
     :host (pathname-host home) :device (pathname-device home)
     :directory (append (pathname-directory home)
                        (case *om-os*
                          (:mac '("Library" "Preferences"))
                          (:win '("Application Data"))
                          (:linux '(".local" "share")))))))

(defun om-get-date ()
  (alld:date-string))

;;; External programs

(defvar *om-open-cmd* alld:*system-open-command*)

;;; uiop:run-program forces uiop:*utf-8-external-format* over the image default; only a
;;; Windows console emits non-UTF-8 (CP1252/CP850) bytes, so the tolerant format is a
;;; Windows-only correction — elsewhere this is exactly uiop's own default.
(defparameter *om-process-external-format*
  #+win32 *om-default-external-format*
  #-win32 uiop:*utf-8-external-format*
  "External format for reading process output (om-command-line / om-term-cmd).")

;;; Vanilla shows the process CONSOLE (sys:call-system-showing-output), which merges
;;; stderr; Csound writes all its messages there, so dropping it hides every failure.
(defun %om-show-process-output (str shell current-path prefix)
  (nth-value 1 (alld:call-system-showing-output
                str :shell-type shell :current-directory current-path
                    :prefix prefix :label str :show-cmd nil
                    ;; late lookup: *om-stream* is defined in sbcl-lisp-tools, loaded after this file
                    :output-stream (symbol-value '*om-stream*)
                    :external-format *om-process-external-format*)))

(defun om-command-line (str &optional (redirect-output nil) (wait t) (current-path nil))
  (let ((command (alld:shell-command-form str))
        (options (alld:shell-command-options str)))
    (cond
      ((pathnamep redirect-output)
       (apply #'uiop:run-program command :output redirect-output
              :error-output redirect-output
              :directory current-path :wait wait
              :external-format *om-process-external-format*
              :if-output-exists :append
              options))
      (redirect-output
       (%om-show-process-output str "sh" current-path ":: "))
      (t
       (apply #'uiop:run-program command :directory current-path :wait wait
              :ignore-error-status t options)))))

(defun om-term-cmd (str &optional (shell "/bin/bash") (redirect-output t)
                                  (wait t) (current-path nil))
  (if (eq *om-os* :win)
      (om-command-line str redirect-output wait current-path)
      (let ((command (list shell "-c" str)))
        (cond
          ((pathnamep redirect-output)
           (uiop:run-program command
                             :output redirect-output :error-output redirect-output
                             :directory current-path :wait wait
                             :external-format *om-process-external-format*
                             :if-output-exists :append))
          (redirect-output
           (%om-show-process-output str shell current-path " "))
          (t
           (uiop:run-program command
                             :directory current-path :wait wait
                             :ignore-error-status t))))))

(defun om-run-program (path &optional afterfun)
  (alld:run-application path :after afterfun
                             :initial-bindings *process-initial-bindings*
                             :armed-traps '(:divide-by-zero)))

(defun om-run-application (path)
  (alld:open-with-system path))

(defun om-select-program (id)
  (alld:open-with-system id :wait t))

(defun om-path2cmdpath (path) (namestring path))

;;; Threads / processes

(defvar *current-priority* 1)

(defmacro om-with-priority (priority &body body)
  `(let ((*current-priority* ,priority)) ,@body))

;; vanilla passed '(:priority (or *current-priority* 10)); SBCL threads have no priority
(defmacro om-run-process (name func &rest args)
  `(%make-om-thread
    (lambda () (funcall ,(if (listp func) func `(if (functionp ,func) ,func ',func))
                        ,@args))
    :name (string ',name)))

;; self-kill mirrors LW mp:process-kill via current-process-kill (lw-mp-92.htm)
(defun om-kill-process (process) (alld:kill-thread process))

(defun om-find-process (id) (alld:find-thread id))

(defun om-process-state (process) (alld:thread-state process))

;;; Interrupts / autorelease

(defmacro om-without-interrupts (&body body)
  `(sb-sys:without-interrupts ,@body))

;; vanilla #+cocoa objc:with-autorelease-pool
(defmacro om-with-new-gc (&body body)
  `(alld:with-autorelease-pool ,@body))
