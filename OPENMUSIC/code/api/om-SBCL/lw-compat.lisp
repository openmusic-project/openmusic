;;; lw-compat.lisp
;;; SBCL implementations / stubs for LispWorks-specific symbols that the
;;; OpenMusic kernel and externals depend on.
;;;
;;; Loaded by load-api.lisp after build-om.lisp has loaded alloy-desktop, so these bindings are
;;; available to everything that follows: the API bridge, sbcl-lisp-tools,
;;; editors, foreign-interface, externals, and the kernel.
;;;
;;; INTERNAL POLICY — keep this file:
;;;   1. No (ql:quickload) / (require) here: declare deps in
;;;      alloy-desktop/alloy-desktop.asd, which load-alloy.lisp loads first.  Available: alexandria, uiop, trivial-open-browser.
;;;   2. One section per LW symbol, with a comment identifying the
;;;      LW caller(s).
;;; This folder is only ever loaded on SBCL: no reader conditionals.

(in-package :cl-user)

;;; ======================================================================
;;; SECTION 1 — Low-level LW variables / streams
;;; ======================================================================

  ;; LispWorks:  *stdout*  — synonym stream bound to the process stdout.
  ;; Used by:    externals/OSC/om-osc-sbcl.lisp, externals/OSC/om-osc.lisp
  (defvar *stdout* sb-sys:*stdout*
    "LispWorks-compatible standard output stream (SBCL synonym).")


;;; ======================================================================
;;; SECTION 2 — compile-file-if-needed
;;; ======================================================================
;;;
;;; LispWorks provides HCL:COMPILE-FILE-IF-NEEDED. The reusable implementation
;;; now lives in ALLOY-DESKTOP. Import it into CL-USER for legacy loaders, and
;;; into HCL below when that compatibility package is created.

(shadowing-import 'alloy-desktop:compile-file-if-needed :cl-user)
(export 'alloy-desktop:compile-file-if-needed :cl-user)

;;; ======================================================================
;;; SECTION 3 — LispWorks MOP-compat packages + shims
;;; ======================================================================
;;;
;;; LispWorks: the SYSTEM and HCL packages exist natively.
;;; Used by: kernel setfs system::class-documentation (documentation.lisp);
;;;          projects call hcl::class-direct-subclasses (scoreeditor-analysis.lisp).
;;; SBCL has neither by default -- create them (loaded before mop.lisp, so its
;;; reader finds them) and home the shim defuns here.

;;; CCL is a nickname of SYSTEM in LispWorks (in LispWorks: SYS, CCL,
;;; SYSTEM-INTERNAL) -- MCL-heritage OM libraries read and intern ccl:: names.
(defpackage #:system
  (:nicknames #:sys #:ccl #:system-internal)
  (:use)
  (:import-from #:alloy-desktop #:run-shell-command #:pipe-exit-status #:call-system #:call-system-showing-output #:open-url #:get-user-name #:date-string #:directory-pathname-p)
  (:export #:run-shell-command
           #:pipe-exit-status
           #:open-url
           #:call-system
           #:call-system-showing-output
           #:get-user-name
           #:date-string
           #:directory-pathname-p))


;;; SYSTEM:CALL-SYSTEM is provided by ALLOY-DESKTOP and imported above.

(defpackage #:hcl
  (:use)
  (:import-from #:alloy-desktop #:compile-file-if-needed #:date-string)
  (:export #:date-string))

(defpackage #:mp
  (:use #:common-lisp)
  (:export #:process-run-function
           #:unsupported-process-option
           #:unsupported-process-option-name
           #:unsupported-process-option-value
           #:unsupported-process-priority
           #:unsupported-process-priority-value))

(defun system::class-documentation (class)
  (documentation (if (symbolp class) (find-class class nil) class) t))

(defun (setf system::class-documentation) (value class)
  (setf (documentation (if (symbolp class) (find-class class nil) class) t)
        value))

(defun hcl::class-direct-subclasses (class)
  (sb-mop:class-direct-subclasses (if (symbolp class) (find-class class) class)))

;;; ======================================================================
;;; SECTION 3a — LispWorks MP process compatibility
;;; ======================================================================
;;;
;;; LispWorks: mp:process-run-function name keywords function &rest arguments
;;;            => process
;;;
;;; The live SBCL callers use an empty keyword list.  LispWorks also accepts
;;; :priority, but Bordeaux Threads and SBCL expose no portable/settable thread
;;; priority.  Report that loss explicitly instead of pretending to honor it.
;;; Mailboxes and LW termination protocols are not equivalent to raw threads;
;;; non-NIL requests are rejected before a worker is created.

(define-condition mp:unsupported-process-priority (warning)
  ((value :initarg :value :reader mp:unsupported-process-priority-value))
  (:report
   (lambda (condition stream)
     (format stream
             "MP process priority ~S cannot be mapped to Bordeaux Threads; using the host scheduler default."
             (mp:unsupported-process-priority-value condition)))))

(define-condition mp:unsupported-process-option (error)
  ((name :initarg :name :reader mp:unsupported-process-option-name)
   (value :initarg :value :reader mp:unsupported-process-option-value))
  (:report
   (lambda (condition stream)
     (format stream
             "MP process option ~S with value ~S has no safe SBCL/Bordeaux Threads equivalent."
             (mp:unsupported-process-option-name condition)
             (mp:unsupported-process-option-value condition)))))

(defun mp::%validate-process-keywords (keywords)
  (unless (listp keywords)
    (error 'type-error :datum keywords :expected-type 'list))
  (let ((length (list-length keywords)))
    (unless (and length (evenp length))
      (error "MP process keywords must be a finite property list, got ~S."
             keywords)))
  (loop for (name value) on keywords by #'cddr
        do (case name
             (:priority
              (unless (typep value 'fixnum)
                (error 'type-error :datum value :expected-type 'fixnum))
              (warn 'mp:unsupported-process-priority :value value))
             ((:mailbox :internal-server :remote-terminator
               :local-terminator :terminate-by-send)
              ;; NIL selects the LispWorks default/no-feature behavior and
              ;; therefore requires no emulation.  Any true value changes
              ;; lifecycle or communication and must not be silently dropped.
              (when value
                (error 'mp:unsupported-process-option
                       :name name :value value)))
             (otherwise
              (error 'mp:unsupported-process-option
                     :name name :value value))))
  keywords)

(defun mp:process-run-function (name keywords function &rest arguments)
  "Create and immediately return a real worker process.

NAME is a string designator. KEYWORDS follows the LispWorks property-list
calling convention. FUNCTION is resolved before the thread is created so an
invalid designator is reported to the caller. Conditions raised by FUNCTION
remain worker-thread conditions and are deliberately not swallowed here."
  (mp::%validate-process-keywords keywords)
  (let ((thread-name (string name))
        (callable (coerce function 'function)))
    (oa::%make-om-thread (lambda () (apply callable arguments))
                         :name thread-name)))

;;; ======================================================================
;;; SECTION 4 — current-pathname
;;; ======================================================================
;;;
;;; LispWorks provides CURRENT-PATHNAME in the ambient Lisp environment.
;;; The reusable implementation lives in ALLOY-DESKTOP; the bridge imports
;;; and exports the same symbol through CL-USER for legacy OpenMusic readers.

(shadowing-import 'alloy-desktop:current-pathname :cl-user)
(export 'alloy-desktop:current-pathname :cl-user)

;;; ======================================================================
;;; SECTION 5 — print environment parity
;;; ======================================================================
;;;
;;; LispWorks defaults *print-pretty* to NIL; SBCL defaults it to T. OM's file
;;; serialization uses (format nil "~S" form) and reads it back one line at a
;;; time (kernel saveheader.lisp: header-comment-from-obj for t writes,
;;; get-pathname-wsparams reads), so under SBCL's pretty
;;; default the pretty-printer wraps long forms and the read hits end-of-file.
;;; Match LispWorks: keep pretty off globally (the listener rebinds it locally).

(setf *print-pretty* nil)
;; a launcher REPL may hold thread-local bindings: force the true global too
(setf (sb-ext:symbol-global-value '*print-pretty*) nil)

;;; ======================================================================
;;; SECTION 6 — tolerate #<...> unreadable objects in OM save files
;;; ======================================================================
;;;
;;; OM persistence (kernel save.lisp: om-save-pathname) serializes a
;;; pathname as (om-make-pathname ... :host ,(pathname-host p) ...). On SBCL
;;; Windows (pathname-host p) is always #<sb-impl::win32-host ...>, which is
;;; unreadable, so LOADing any saved OM file (prefs, patches, workspaces) hits
;;; "illegal sharp macro character: #\<". The host is meaningless here --
;;; om-make-pathname rebuilds the path from :device + :directory -- so read
;;; #<...> as NIL. (LispWorks returns a readable host, so this never bit it.)

;(defun %read-unreadable-as-nil (stream sub-char infix)
;  (declare (ignore sub-char infix))
;  (loop for c = (read-char stream nil :eof)
;        until (or (eq c :eof) (char= c #\>)))
;  nil)

;(set-dispatch-macro-character #\# #\< #'%read-unreadable-as-nil)

;;; ======================================================================
;;; SECTION 4b — Unlock COMMON-LISP package for kernel symbol assignments
;;; ======================================================================
;;;
;;; The kernel assigns function bindings to symbols like SEQUENCE that
;;; normally name CL types/classes.  On LispWorks the CL package is
;;; unlocked so this works; on SBCL the package is locked.  We unlock
;;; the entire CL package before kernel load.

;(eval-when (:compile-toplevel :load-toplevel :execute)
;  (ignore-errors (sb-ext:unlock-package :common-lisp)))

;;; ======================================================================
;;; SECTION 5 — CAPI stub package
;;; ======================================================================
;;;
;;; The OpenMusic kernel was written for LispWorks CAPI.  On SBCL,
;;; CAPI-style calls are handled by the OM-API bridge (views, windows,
;;; dialog-items).  But many kernel files still contain direct CAPI
;;; references (especially display-message, apply-in-pane-process, and
;;; redisplay-element).  We create a stub package so that the reader
;;; can parse them, and define no-op / minimal shims to let the kernel
;;; load without reader errors.
;;;
;;; As the port matures, these stubs should be replaced by proper
;;; OM-API / Alloy equivalents.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; reload idempotence: incremental compile&load re-runs this toplevel and
  ;; MAKE-PACKAGE signals on an existing package
  (unless (find-package "CAPI")
    (make-package "CAPI" :use nil)))

(progn
  ;; Stub generic functions — kernel defines :around/:before/:after methods
  ;; on several CAPI symbols, so they must be generic functions not ordinary
  ;; defuns.  Each gets a default method.

  (defclass capi::capi-object ()
    ((capi::name :initarg :name :initform nil :accessor capi::capi-object-name)
     (capi::plist :initarg :plist :initform nil :accessor capi::capi-object-plist)))

  (defclass capi::interface (capi::capi-object)
    ((capi::title :initarg :title :initform nil :accessor capi::interface-title)
     (capi::layout :initarg :layout :initform nil :accessor capi::interface-layout)
     (capi::display-state :initarg :display-state :initform :normal
                          :accessor capi::interface-display-state)
     ;; Native CAPI stores these associations on the interface.  In this port
     ;; they are metadata for the single GLFW/UI process.
     (capi::screen :initarg :screen :initform nil
                   :accessor capi::%interface-screen)
     (capi::process :initarg :process :initform nil
                    :accessor capi::%interface-process)
     (capi::window-styles :initarg :window-styles :initform nil
                          :accessor capi::%interface-window-styles)
     (capi::menu-bar-items :initarg :menu-bar-items)))

  ;; The interface owner is the desktop window registry's owner (window.lisp:
  ;; capi:interface-owner and its setf for om-look-window).
  (defgeneric capi::interface-owner (interface))
  (defgeneric (setf capi::interface-owner) (owner interface))

  (defclass capi::menu-object (alld:menu-object capi::capi-object)
    ((capi::callback :initarg :callback :initform nil :accessor capi::menu-callback)
     (capi::callback-type :initarg :callback-type :initform nil
                          :accessor capi::menu-callback-type)
     (capi::callback-data-function :initarg :callback-data-function :initform nil
                                   :accessor capi::menu-callback-data-function)
     (capi::enabled-function :initarg :enabled-function :initform nil
                             :accessor capi::menu-enabled-function)
     (capi::enabled-slot :initarg :enabled-slot :initform nil)
     (capi::setup-callback-argument :initarg :setup-callback-argument :initform nil
                                    :accessor capi::menu-setup-callback-argument)
     (capi::print-function :initarg :print-function :initform nil)
     (capi::help-key :initarg :help-key :initform t)
     (capi::mnemonic :initarg :mnemonic :initform nil)))

  (defclass capi::menu (alld:menu capi::menu-object)
    ((capi::items-function :initarg :items-function :initform nil
                           :accessor capi::menu-items-function)
     (capi::image-function :initarg :image-function :initform nil)
     (capi::popup-callback :initarg :popup-callback :initform nil))
    (:default-initargs :title nil))

  (defclass capi::menu-item (alld:menu-item capi::menu-object)
    ((capi::data :initarg :data :initform nil :accessor capi::item-data)
     (capi::text :initarg :text :initform nil :accessor capi::item-text)
     (capi::alternative :initarg :alternative :initform nil)
     (capi::selected-function :initarg :selected-function :initform nil
                              :accessor capi::menu-selected-function)
     (capi::enabled-function-for-dialog :initarg :enabled-function-for-dialog
                                        :initform nil))
    (:default-initargs :title nil :accelerator :default))

  (defclass capi::menu-component (alld:menu-component capi::menu-object)
    ((capi::items-function :initarg :items-function :initform nil
                           :accessor capi::menu-items-function)
     (capi::title :initarg :title :initform nil
                  :accessor capi::%menu-component-title)))

  ;; Compatibility accessors delegate neutral menu state to alloy-desktop.
  (defgeneric capi::menu-title (object))
  (defgeneric (setf capi::menu-title) (value object))
  (defmethod capi::menu-title ((object capi::menu))
    (alld:menu-entry-title object))
  (defmethod capi::menu-title ((object capi::menu-item))
    (alld:menu-entry-title object))
  (defmethod capi::menu-title ((object capi::menu-component))
    (capi::%menu-component-title object))
  (defmethod (setf capi::menu-title) (value (object capi::menu))
    (setf (alld:menu-entry-title object) value))
  (defmethod (setf capi::menu-title) (value (object capi::menu-item))
    (setf (alld:menu-entry-title object) value))
  (defmethod (setf capi::menu-title) (value (object capi::menu-component))
    (setf (capi::%menu-component-title object) value))

  (defgeneric capi::menu-items (object))
  (defgeneric (setf capi::menu-items) (value object))
  (defmethod capi::menu-items ((object capi::menu))
    (alld:menu-entry-items object))
  (defmethod capi::menu-items ((object capi::menu-component))
    (alld:menu-entry-items object))
  (defmethod (setf capi::menu-items) (value (object capi::menu))
    (setf (alld:menu-entry-items object) value))
  (defmethod (setf capi::menu-items) (value (object capi::menu-component))
    (setf (alld:menu-entry-items object) value))

  (defgeneric capi::menu-item-accelerator (object))
  (defgeneric (setf capi::menu-item-accelerator) (value object))
  (defmethod capi::menu-item-accelerator ((object capi::menu-item))
    (alld:menu-entry-accelerator object))
  (defmethod (setf capi::menu-item-accelerator) (value (object capi::menu-item))
    (setf (alld:menu-entry-accelerator object) value))

  ;; The drop model (slots, effect negotiation, formats, payload) is
  ;; alloy-desktop's alld:drop-object; the CAPI accessors stay generic functions
  ;; of capi::drop-object that delegate to it.
  (defclass capi::drop-object (alld:drop-object) ())

  (defmethod capi::drop-object-pane ((drop-object capi::drop-object))
    (alld:drop-object-pane drop-object))

  (defmethod capi::drop-object-pane-x ((drop-object capi::drop-object))
    (alld:drop-object-pane-x drop-object))

  (defmethod (setf capi::drop-object-pane-x) (x (drop-object capi::drop-object))
    (setf (alld:drop-object-pane-x drop-object) x))

  (defmethod capi::drop-object-pane-y ((drop-object capi::drop-object))
    (alld:drop-object-pane-y drop-object))

  (defmethod (setf capi::drop-object-pane-y) (y (drop-object capi::drop-object))
    (setf (alld:drop-object-pane-y drop-object) y))

  (defmethod capi::drop-object-drop-effect ((drop-object capi::drop-object))
    (alld:drop-object-drop-effect drop-object))

  (defmethod (setf capi::drop-object-drop-effect) (effect (drop-object capi::drop-object))
    (setf (alld:drop-object-drop-effect drop-object) effect))

  (defclass capi::metafile-port (alld:metafile-port) ())

  (defun capi::display-message (format-string &rest format-args)
    (oa::om-message-dialog (apply #'format nil format-string format-args)))

  (defun capi::display (interface &key screen owner window-styles process)
    (oa::%capi-display-interface interface :screen screen :owner owner
                                 :window-styles window-styles :process process))

  ;; capi-capi-ref-41.htm: COLLECT-INTERFACES is generic. The registry and
  ;; ordering authority live in window.lisp, which is loaded after this file.
  (defgeneric capi::collect-interfaces
      (proto &key screen current-process-first sort-by))

  (defmethod capi::collect-interfaces
      ((proto t) &key screen (current-process-first t) (sort-by :create))
    (oa::%capi-collect-interfaces proto :screen screen
                                  :current-process-first current-process-first
                                  :sort-by sort-by))

  ;; Full CAPI reuse protocol (LW 8.1 capi-capi-ref-160/214/218/245/390).
  ;; The generics live here so libraries may specialize them before window.lisp
  ;; is loaded; the default registry work remains in the window backend.
  (defvar *capi-reuse-interfaces-p* t)

  (defun capi::reuse-interfaces-p () *capi-reuse-interfaces-p*)

  (defun (setf capi::reuse-interfaces-p) (value)
    (setf *capi-reuse-interfaces-p* (not (null value))))

  (defgeneric capi::interface-match-p
      (interface &rest initargs &key &allow-other-keys))

  (defmethod capi::interface-match-p
      ((interface t) &rest initargs &key &allow-other-keys)
    (declare (ignore interface initargs))
    nil)

  (defmethod capi::interface-match-p
      ((interface capi::interface) &rest initargs &key &allow-other-keys)
    ;; Preserve the fork's existing OA specializations (Preferences and
    ;; om-table) while exposing the documented CAPI generic to new libraries.
    (apply #'oa::interface-match-p interface initargs))

  (defgeneric capi::interface-reuse-p
      (interface &rest initargs &key &allow-other-keys))

  (defmethod capi::interface-reuse-p
      ((interface t) &rest initargs &key &allow-other-keys)
    (declare (ignore interface initargs))
    nil)

  (defmethod capi::interface-reuse-p
      ((interface capi::interface) &rest initargs &key &allow-other-keys)
    (declare (ignore interface initargs))
    t)

  (defgeneric capi::reinitialize-interface (interface &rest initargs))

  (defmethod capi::reinitialize-interface
      ((interface capi::interface) &rest initargs)
    (declare (ignore initargs))
    interface)

  (defgeneric capi::locate-interface
      (class-spec &rest initargs &key screen no-busy-interface
                               &allow-other-keys))

  (defmethod capi::locate-interface
      ((class-spec t) &rest initargs &key screen no-busy-interface
                                    &allow-other-keys)
    (alld:call-on-ui-thread
     (lambda ()
       (oa::%capi-locate-interface-now class-spec initargs screen no-busy-interface))))

  (defgeneric capi::find-interface
      (class-name &rest initargs &key screen &allow-other-keys))

  (defmethod capi::find-interface
      ((class-name t) &rest initargs &key screen &allow-other-keys)
    (alld:call-on-ui-thread
     (lambda () (oa::%capi-find-interface-now class-name initargs screen))))

  (defgeneric capi::interface-display (interface))

  (defmethod capi::interface-display ((interface capi::interface))
    (oa::om-show-window interface)
    interface)

  (defgeneric capi::destroy (interface))

  (defmethod capi::destroy ((interface capi::interface))
    (oa::%capi-destroy-interface interface))

  (defun capi::quit-interface (pane &key force)
    (oa::%capi-quit-interface pane force))

  ;; capi-capi-ref-81.htm. Keeping the public entry point here lets external
  ;; libraries compile while window.lisp remains the sole owner of modal GLFW
  ;; state, focus, positioning, timeout, and the two-value result protocol.
  (defun capi::display-dialog
      (interface &key screen focus (modal t) timeout owner x y
                      (position-relative-to :owner) continuation
                      callback-error-handler)
    (oa::%capi-display-dialog interface :screen screen :focus focus :modal modal
                              :timeout timeout :owner owner :x x :y y
                              :position-relative-to position-relative-to
                              :continuation continuation
                              :callback-error-handler callback-error-handler))

  (defun capi::exit-dialog (value)
    "Terminate the current modal dialog with an accepted VALUE."
    (alloy-desktop:exit-modal value))

  (defun capi::abort-dialog ()
    "Terminate the current modal dialog without accepting a value."
    (alloy-desktop:abort-modal))

  (defun capi::raise-interface (pane)
    (let ((window (if (typep pane 'oa::om-graphic-object)
                      (oa::om-view-window pane)
                      pane)))
      (when window (oa::om-select-window window))
      window))

  (defun capi::element-interface (self)
    (when self (oa::om-view-window self)))

  ;; CAPI runs FUNCTION in the pane's process (the UI thread); marshal via the
  ;; om-api UI executor.
  (defun capi::apply-in-pane-process (pane function &rest args)
    (declare (ignore pane))
    (alld:call-on-ui-thread (lambda () (apply function args)))
    nil)

  (defun capi::current-pointer-position (&key relative-to (pane-relative-p t))
    (if pane-relative-p
        (let ((point (oa::%mouse-position-primitive relative-to)))
          (values (oa::om-point-h point) (oa::om-point-v point)))
        ;; GLFW already reports screen coordinates in the CAPI y-down convention.
        (let ((window (or (and relative-to (oa::om-view-window relative-to))
                          (alloy-desktop:focused-window))))
          (multiple-value-bind (x y)
              (if window
                  (org.shirakumo.fraf.glfw:cursor-location window)
                  (let ((point (oa::%mouse-position-primitive nil)))
                    (values (oa::om-point-h point) (oa::om-point-v point))))
            ;; cursor-location is measured from the CONTENT area, so the origin to
            ;; add is the content origin -- not om-view-position, which answers in
            ;; the external box the kernel positions windows by.
            (let ((origin (and window
                               (org.shirakumo.fraf.glfw:pointer window)
                               (org.shirakumo.fraf.glfw:location window))))
              (values (+ x (if origin (first origin) 0))
                      (+ y (if origin (second origin) 0))))))))

  ;; A pinboard object is a drawn frame; a dialog item is a PANE and never answers
  ;; here, so a widget under the point must not shadow the frame that owns it.
  (defgeneric capi::pinboard-object-at-position (pinboard x y))

  (defmethod capi::pinboard-object-at-position ((pinboard t) x y)
    (let ((found (oa::om-find-view-containing-point
                  pinboard (oa::om-make-point x y) nil)))
      (when (and found
                 (not (eq found pinboard))
                 (oa::om-item-view-p found))
        found)))

  (defun capi::static-layout-child-position (self)
    (let ((point (oa::om-view-position self)))
      (values (oa::om-point-h point) (oa::om-point-v point))))

  (define-setf-expander capi::static-layout-child-position (self)
    (let ((object (gensym "OBJECT")) (x (gensym "X")) (y (gensym "Y")))
      (values (list object) (list self) (list x y)
              `(progn
                 (oa::om-set-view-position ,object (oa::om-make-point ,x ,y))
                 (values ,x ,y))
              `(capi::static-layout-child-position ,object))))

  (defun capi::static-layout-child-size (self)
    (let ((point (oa::om-view-size self)))
      (values (oa::om-point-h point) (oa::om-point-v point))))

  (define-setf-expander capi::static-layout-child-size (self)
    (let ((object (gensym "OBJECT")) (width (gensym "WIDTH"))
          (height (gensym "HEIGHT")))
      (values (list object) (list self) (list width height)
              `(progn
                 (oa::om-set-view-size ,object (oa::om-make-point ,width ,height))
                 (values ,width ,height))
              `(capi::static-layout-child-size ,object))))

  (defvar *capi-hint-tables* (make-hash-table :test 'eq :weakness :key))

  (defparameter *capi-geometric-hint-keys*
    '(:x :y :best-x :best-y :best-width :best-height
      :external-min-width :external-max-width
      :external-min-height :external-max-height
      :visible-min-width :visible-max-width
      :visible-min-height :visible-max-height
      :internal-min-width :internal-max-width
      :internal-min-height :internal-max-height
      :min-width :max-width :min-height :max-height
      :scroll-width :scroll-height))

  (defun %capi-geometric-hint-key-p (key)
    (member key *capi-geometric-hint-keys* :test #'eq))

  (defun %capi-extract-geometric-hints (plist)
    (loop for (key value) on plist by #'cddr
          when (%capi-geometric-hint-key-p key)
            append (list key value)))

  (defun %normalize-capi-hint-update (plist)
    "Return PLIST with only the first occurrence of each key, as CAPI does."
    (when (oddp (length plist))
      (error "CAPI hint table is not a property list: ~S" plist))
    (let ((seen nil) (result nil))
      (loop for (key value) on plist by #'cddr
            unless (member key seen :test #'eq)
              do (push key seen)
                 (setf result (nconc result (list key value))))
      result))

  (defun %merge-capi-hints (old new)
    ;; A later call replaces an old key, but within that call the first
    ;; occurrence wins.  Append untouched old pairs so custom hints survive.
    (let* ((normalized (%normalize-capi-hint-update new))
           (updated-keys (loop for (key nil) on normalized by #'cddr
                               collect key)))
      (append normalized
              (loop for (key value) on old by #'cddr
                    unless (member key updated-keys :test #'eq)
                      append (list key value)))))

  (defun %capi-hint-entry (plist keys)
    (dolist (key keys (values nil nil nil))
      (loop for (candidate value) on plist by #'cddr
            when (eq candidate key)
              do (return-from %capi-hint-entry
                   (values value t key)))))

  (defun %capi-hint-text (element)
    ;; :text-width measures the element's CAPI item text: capi::item-text for menu
    ;; items, the pane text (om-dialog-item-text) for dialog items
    ;; (dialog-items.lisp: the item-text slot of om-standard-dialog-item)
    (or (typecase element
          (capi::menu-item (capi::item-text element))
          (oa::om-standard-dialog-item
           (oa::om-dialog-item-text element)))
        ""))

  (defun %capi-hint-font (element)
    ;; elements without an om-font slot (bare alloy components) measure in the default font;
    ;; symbol-value: lw-compat compiles before graphics.lisp defines the var
    ;; (load-api.lisp: the compile&load of lw-compat, before the *api-files* list
    ;; where graphics enters)
    (if (typep element 'oa::om-graphic-object)
        (or (oa::om-get-font element)
            (symbol-value 'oa::*om-default-font1*))
        (symbol-value 'oa::*om-default-font1*)))

  (defun %capi-text-width (element string)
    (oa::om-string-size string (%capi-hint-font element)))

  (defun %capi-text-height (element &optional (lines 1))
    (* lines (oa::om-string-h (%capi-hint-font element))))

  (defun %capi-resolve-hint-value (element value axis)
    "Resolve the documented CAPI numeric hint language for AXIS."
    (labels ((resolve (form)
               (cond
                 ((realp form) form)
                 ((eq form t) t)
                 ((eq form :text-width)
                  (%capi-text-width element (%capi-hint-text element)))
                 ((eq form :text-height) (%capi-text-height element))
                 ((member form '(:screen-width :screen-height) :test #'eq)
                  (let ((size (oa::om-screen-size)))
                    (if (eq form :screen-width)
                        (oa::om-point-h size)
                        (oa::om-point-v size))))
                 ((consp form)
                  (let ((operator (first form)) (arguments (rest form)))
                    (cond
                      ((member (symbol-name operator)
                               '("CHARACTER" "STRING") :test #'string=)
                       (let ((datum (first arguments)))
                         (if (eq axis :width)
                             (%capi-text-width
                              element
                              (if (stringp datum)
                                  datum
                                  (make-string datum :initial-element #\M)))
                             (%capi-text-height
                              element (if (integerp datum) datum 1)))))
                      ((member operator '(max min + - * /) :test #'eq)
                       (apply (symbol-function operator)
                              (mapcar #'resolve arguments)))
                      ((eq operator 'symbol-value)
                       (resolve (symbol-value (first arguments))))
                      ((eq operator 'funcall)
                       (resolve (apply (first arguments)
                                       (mapcar #'resolve (rest arguments)))))
                      ((eq operator 'apply)
                       (resolve (apply (first arguments)
                                       (mapcar #'resolve (rest arguments)))))
                      (t (error "Unsupported CAPI geometry hint ~S." form)))))
                 (t (error "Unsupported CAPI geometry hint ~S." form)))))
      (let ((result (resolve value)))
        (unless (or (eq result t)
                    (and (realp result) (not (minusp result))))
          (error "CAPI geometry hint ~S resolved to invalid size ~S."
                 value result))
        result)))

  (defun %capi-scrolling-element-p (element)
    (and (typep element 'alld:scroll-mixin)
         (not (alld:scroll-passive-p element))))

  (defun %capi-axis-constraint (element hints axis kind)
    (let* ((suffix (ecase axis (:width "WIDTH") (:height "HEIGHT")))
           (external (intern (format nil "EXTERNAL-~A-~A" kind suffix) :keyword))
           (visible (intern (format nil "VISIBLE-~A-~A" kind suffix) :keyword))
           (deprecated (intern (format nil "~A-~A" kind suffix) :keyword))
           (internal (intern (format nil "INTERNAL-~A-~A" kind suffix) :keyword)))
      (multiple-value-bind (value presentp)
          (%capi-hint-entry hints (list external))
        (when (and presentp (null value)) (setf presentp nil))
        (unless presentp
          (multiple-value-setq (value presentp)
            (%capi-hint-entry hints (list visible deprecated)))
          (when (and presentp (null value)) (setf presentp nil)))
        (unless (or presentp (%capi-scrolling-element-p element))
          (multiple-value-setq (value presentp)
            (%capi-hint-entry hints (list internal)))
          (when (and presentp (null value)) (setf presentp nil)))
        (values (and presentp
                     (%capi-resolve-hint-value element value axis))
                presentp))))

  (defun %capi-axis-limits (element axis)
    (let ((hints (gethash element *capi-hint-tables*)))
      (multiple-value-bind (minimum minimum-p)
          (%capi-axis-constraint element hints axis "MIN")
        (multiple-value-bind (maximum maximum-p)
            (%capi-axis-constraint element hints axis "MAX")
          (when (and maximum-p (eq maximum t))
            (unless minimum-p
              (error "CAPI maximum T requires a minimum on the same axis."))
            (setf maximum minimum))
          ;; In an inconsistent pair CAPI's minimum remains authoritative.
          (when (and minimum-p maximum-p (> minimum maximum))
            (setf maximum minimum))
          (values minimum maximum minimum-p maximum-p)))))

  (defun %capi-constrain-axis (value minimum maximum minimum-p maximum-p)
    (let ((result value))
      (when minimum-p (setf result (max result minimum)))
      (when maximum-p (setf result (min result maximum)))
      result))

  (defun %capi-constrain-element-size (element width height)
    (multiple-value-bind (min-width max-width min-width-p max-width-p)
        (%capi-axis-limits element :width)
      (multiple-value-bind (min-height max-height min-height-p max-height-p)
          (%capi-axis-limits element :height)
        (values (%capi-constrain-axis
                 width min-width max-width min-width-p max-width-p)
                (%capi-constrain-axis
                 height min-height max-height min-height-p max-height-p)))))

  (defun %capi-preferred-axis (element hints axis current)
    (let ((key (ecase axis (:width :best-width) (:height :best-height))))
      (multiple-value-bind (value presentp) (%capi-hint-entry hints (list key))
        (if (and presentp value)
            (%capi-resolve-hint-value element value axis)
            current))))

  (defun %capi-apply-internal-hints (element hints)
    (when (%capi-scrolling-element-p element)
      (let* ((field (oa::om-field-size element))
             (old-width (oa::om-point-h field))
             (old-height (oa::om-point-v field)))
        (labels ((internal-value (axis current)
                   (let ((keys (if (eq axis :width)
                                   '(:internal-min-width :scroll-width)
                                   '(:internal-min-height :scroll-height))))
                     (multiple-value-bind (value presentp)
                         (%capi-hint-entry hints keys)
                       (if (and presentp value)
                           (%capi-resolve-hint-value element value axis)
                           current)))))
          (oa::om-set-field-size
           element
           (oa::om-make-point (internal-value :width old-width)
                              (internal-value :height old-height)))))))

  (defun %capi-apply-hint-table (element hints update)
    (let* ((position (oa::om-view-position element))
           (size (oa::om-view-size element))
           ;; VX/VY are the OM Y-down placement authority before attachment;
           ;; live Alloy bounds are authoritative for non-OM CAPI elements.
           (old-x (if (typep element 'oa::om-graphic-object)
                      (oa::vx element)
                      (oa::om-point-h position)))
           (old-y (if (typep element 'oa::om-graphic-object)
                      (oa::vy element)
                      (oa::om-point-v position)))
           (old-width (oa::om-point-h size))
           (old-height (oa::om-point-v size)))
      (multiple-value-bind (x x-p) (%capi-hint-entry hints '(:x :best-x))
        (multiple-value-bind (y y-p) (%capi-hint-entry hints '(:y :best-y))
          (when (or (and x-p x) (and y-p y))
            (oa::om-set-view-position
             element
             (oa::om-make-point
              (if (and x-p x)
                  (%capi-resolve-hint-value element x :width)
                  old-x)
              (if (and y-p y)
                  (%capi-resolve-hint-value element y :height)
                  old-y))))))
      (%capi-apply-internal-hints element hints)
      (when (some #'%capi-geometric-hint-key-p
                  (loop for (key nil) on update by #'cddr collect key))
        (when (typep element 'oa::om-look-window)
          (oa::%apply-capi-window-hints element))
        (multiple-value-bind (width height)
            (%capi-constrain-element-size
             element
             (%capi-preferred-axis element hints :width old-width)
             (%capi-preferred-axis element hints :height old-height))
          (oa::om-set-view-size element (oa::om-make-point width height))))))

  (defun %capi-initialize-hints (element attributes)
    "Apply permanent CAPI initargs and transient :INITIAL-CONSTRAINTS."
    (let ((permanent (%capi-extract-geometric-hints attributes))
          (initial (getf attributes :initial-constraints)))
      (when permanent
        (capi::set-hint-table element permanent))
      (when initial
        (let ((saved (copy-list (gethash element *capi-hint-tables*))))
          (capi::set-hint-table element initial)
          ;; Initial constraints choose creation geometry only.  Restore the
          ;; permanent table and native limits without undoing that geometry.
          (if saved
              (setf (gethash element *capi-hint-tables*) saved)
              (remhash element *capi-hint-tables*))
          (when (typep element 'oa::om-look-window)
            (oa::%apply-capi-window-hints element))))
      element))

  (defun capi::set-hint-table (element plist)
    (let ((merged (%merge-capi-hints (gethash element *capi-hint-tables*) plist)))
      (setf (gethash element *capi-hint-tables*) merged)
      (%capi-apply-hint-table element merged plist)
      (when (typep element 'org.shirakumo.alloy:renderable)
        (capi::redisplay-element element))
      merged))

  (defun %capi-normalize-redisplay-region (element x y width height)
    (let* ((size (oa::om-view-size element))
           (normalized-x (or x 0))
           (normalized-y (or y 0))
           (normalized-width
             (or width (- (oa::om-point-h size) normalized-x)))
           (normalized-height
             (or height (- (oa::om-point-v size) normalized-y))))
      (check-type normalized-x (real 0 *))
      (check-type normalized-y (real 0 *))
      (check-type normalized-width (real 0 *))
      (check-type normalized-height (real 0 *))
      (values normalized-x normalized-y normalized-width normalized-height)))

  (defun capi::redisplay-element (element &optional x y width height)
    (when element
      (multiple-value-bind (region-x region-y region-width region-height)
          (%capi-normalize-redisplay-region element x y width height)
        (alld:post-to-ui-thread
         (lambda ()
           (oa::om-invalidate-rectangle
            element region-x region-y region-width region-height)))))
    nil)

  (defun capi::update-drawing-with-cached-display
      (pane &optional x y width height)
    (declare (ignore pane x y width height))
    nil)

  (defun capi::update-drawing-with-cached-display-from-points
      (pane x1 y1 x2 y2 &key (extend 0) extend-x extend-y)
    (let* ((ex (or extend-x extend))
           (ey (or extend-y extend))
           (x (- (min x1 x2) ex))
           (y (- (min y1 y2) ey))
           (width (+ (- (max x1 x2) x) ex))
           (height (+ (- (max y1 y2) y) ey)))
      (capi::update-drawing-with-cached-display pane x y width height)))

  (defun %capi-position->alloy-index (pinboard position)
    ;; CAPI index zero is topmost; Alloy draws the last child on top.
    (let ((last (max 0 (1- (length (oa::om-subviews pinboard))))))
      (cond ((eq position :bottom) 0)
            ((eq position :top) last)
            ((integerp position) (max 0 (- last position)))
            (t last))))

  (defun %capi-restack-host (pinboard)
    (cond ((typep pinboard 'oa::om-item-view)
           (oa::item-container pinboard))
          ;; Subviews added directly to a window are actually entered into its
          ;; fixed root layout.  Restacking the window itself only reorders its
          ;; chrome and leaves the pane behind overlapping siblings.
          ((typep pinboard 'oa::om-look-window)
           (oa::om-window-root pinboard))
          (t pinboard)))

  (defun %capi-restack (pinboard object position)
    (let* ((index (%capi-position->alloy-index pinboard position))
           (host (%capi-restack-host pinboard)))
      (when (and (typep host 'org.shirakumo.alloy:vector-container)
                 (alld:layout-attached-p object))
        (org.shirakumo.alloy:update object host
                      :index (min index
                                  (max 0 (1- (length (org.shirakumo.alloy:elements host)))))))
      (let* ((rest (remove object (oa::vsubviews pinboard)))
             (at (min index (length rest))))
        (setf (oa::vsubviews pinboard)
              (append (subseq rest 0 at) (list object) (subseq rest at))))
      (oa::om-invalidate-view pinboard)
      index))

  (defgeneric capi::manipulate-pinboard
      (pinboard object action &key position))

  (defmethod capi::manipulate-pinboard
      ((pinboard t) object action &key position)
    (labels ((transient-overlay-p (item)
             ;; In CAPI the playback cursor is a DRAWN-PINBOARD-OBJECT and the
             ;; sound editor periodically restacks it.  Alloy represents that
             ;; cursor as a %TRANSIENT-ANIM replayed after normal contents, so
             ;; it is already topmost and is deliberately not a subview.
             (oa::%transient-anim-p item))
           (add (item where)
             (if (transient-overlay-p item)
                 (oa::om-invalidate-view pinboard)
                 (progn
                   (unless (member item (oa::om-subviews pinboard) :test #'eq)
                     (oa::om-add-subviews pinboard item))
                   (%capi-restack pinboard item where)))))
      (ecase action
        (:add (add object (or position :top)))
        (:add-top (add object :top))
        (:add-bottom (add object :bottom))
        (:add-many (dolist (item object) (add item (or position :top))))
        (:delete (unless (transient-overlay-p object)
                   (oa::om-remove-subviews pinboard object)))
        (:delete-if
         (dolist (item (remove-if-not object (copy-list (oa::om-subviews pinboard))))
           (oa::om-remove-subviews pinboard item)))))
    nil)

  (defgeneric capi::set-pane-focus (pane)
    (:method (pane) (declare (ignore pane)) nil))

  (defun capi::beep-pane (&optional pane)
    (declare (ignore pane))
    (oa::om-beep))

  ;; capi-capi-ref-488.htm:124-126 lists completion-function among the ACCESSORS of
  ;; text-input-pane, and :58-120 shows the slot exists on no other pane: neither
  ;; the reader nor the writer has a default method.
  (defgeneric capi::text-input-pane-completion-function (self))

  (defgeneric (setf capi::text-input-pane-completion-function) (function self))

  (defgeneric capi::text-input-pane-in-place-complete (self)
    (:method (self) (declare (ignore self)) nil))

  (defun capi::choice-selection (self)
    (oa::om-get-selected-item-index self))

  (defun (setf capi::choice-selection) (index self)
    (oa::om-set-selected-item-index self index)
    index)

  (defun capi::collection-items (self)
    (oa::om-get-item-list self))

  (defun (setf capi::collection-items) (items self)
    (oa::om-set-item-list self (coerce items 'list))
    items)

  (defun capi::interface-menu-bar-items-bound-p (self)
    (slot-boundp self 'capi::menu-bar-items))

  (defun capi::interface-menu-bar-items (self)
    (when (capi::interface-menu-bar-items-bound-p self)
      (slot-value self 'capi::menu-bar-items)))

  (defun (setf capi::interface-menu-bar-items) (items self)
    (setf (slot-value self 'capi::menu-bar-items) items)
    (when (oa::om-window-open-p self)
      (oa::om-add-menu-to-win self))
    items)

  (defun capi::drop-object-allows-drop-effect-p (drop-object effect)
    (alld:drop-object-allows-drop-effect-p drop-object effect))

  (defun capi::set-drop-object-supported-formats (drop-object formats)
    (alld:set-drop-object-supported-formats drop-object formats))

  (defun capi::drop-object-provides-format (drop-object format)
    (alld:drop-object-provides-format drop-object format))

  (defun capi::drop-object-get-object (drop-object pane format &rest args)
    (declare (ignore pane args))
    (alld:drop-object-get-object drop-object format))

  (defun capi::prompt-for-file
      (message &key pathname ok-check filter filters
                    (if-exists nil if-exists-p)
                    (if-does-not-exist nil if-does-not-exist-p)
                    file-package-is-directory (operation :open) owner pane-args
                    popup-args continuation)
    (unless (member operation '(:open :save))
      (error "CAPI:PROMPT-FOR-FILE :OPERATION must be :OPEN or :SAVE, not ~S."
             operation))
    (let* ((if-exists (if if-exists-p if-exists
                          (if (eq operation :open) :ok :prompt)))
           (if-does-not-exist
             (if if-does-not-exist-p if-does-not-exist
                 (if (eq operation :open) :prompt :ok)))
           (normalized-pathname (and pathname (pathname pathname)))
           (normalized-filters (alld:normalize-file-filters filter filters)))
      (unless (member if-exists '(:ok :prompt))
        (error "Invalid CAPI :IF-EXISTS policy ~S." if-exists))
      (unless (member if-does-not-exist '(:ok :prompt :error))
        (error "Invalid CAPI :IF-DOES-NOT-EXIST policy ~S."
               if-does-not-exist))
      (flet ((invoke ()
               (let* ((result
                        (if (eq operation :save)
                            (oa::om-choose-new-file-dialog
                             :prompt (or message "Choose a File")
                             :pathname normalized-pathname
                             :directory normalized-pathname
                             :name (or (and normalized-pathname
                                            (file-namestring normalized-pathname))
                                       "")
                             :types normalized-filters :filter filter
                             :owner owner :if-exists if-exists
                             :if-does-not-exist if-does-not-exist
                             :file-package-is-directory file-package-is-directory
                             :pane-args pane-args :popup-args popup-args)
                            (oa::om-choose-file-dialog
                             :prompt (or message "Choose a File")
                             :pathname normalized-pathname
                             :directory normalized-pathname
                             :types normalized-filters :filter filter
                             :owner owner :if-exists if-exists
                             :if-does-not-exist if-does-not-exist
                             :file-package-is-directory file-package-is-directory
                             :pane-args pane-args :popup-args popup-args)))
                      (full (alld:full-file-pathname
                             result normalized-pathname))
                      (filename
                        (alld:check-file-selection
                         full operation if-exists if-does-not-exist ok-check))
                      (successp (not (null filename)))
                      (filter-name
                        (and successp
                             (alld:file-filter-name-for filter normalized-filters))))
                 (if continuation
                     (funcall continuation filename successp filter-name)
                     (values filename successp filter-name)))))
        (if (and (alld:ui-thread) (not (alld:on-ui-thread-p)))
            (alld:call-on-ui-thread #'invoke)
            (invoke)))))

  (defun %fit-capi-metafile-size (source-width source-height width height
                                  max-width max-height)
    (let* ((aspect (if (and (plusp source-width) (plusp source-height))
                       (/ source-height source-width)
                       1))
           (w (cond (width width) (height (round height aspect)) (t source-width)))
           (h (cond (height height) (width (round (* width aspect))) (t source-height))))
      (when (and max-width (> w max-width))
        (setf h (max 1 (round (* h (/ max-width w)))) w max-width))
      (when (and max-height (> h max-height))
        (setf w (max 1 (round (* w (/ max-height h)))) h max-height))
      (values (max 1 w) (max 1 h))))

  (defun capi::draw-metafile-to-image
      (pane metafile &key width height max-width max-height
                            (background nil background-p) alpha)
    (unless (alld:picture-p metafile)
      (error "~S is not a CAPI metafile." metafile))
    (dolist (dimension (list width height max-width max-height))
      (when (and dimension
                 (or (not (integerp dimension)) (minusp dimension)))
        (error "CAPI metafile dimensions must be non-negative integers, not ~S."
               dimension)))
    (multiple-value-bind (w h)
        (%fit-capi-metafile-size
         (alld:picture-width metafile) (alld:picture-height metafile)
         width height max-width max-height)
      (oa::%om-rasterize-metafile
       pane metafile w h background background-p alpha)))

  ;; capi-capi-ref-545.htm:40 — (var &key pane bounds format owner drawing-mode) => metafile.
  ;; The offscreen recorder itself lives in icons-picts.lisp, which loads much later.
  (defmacro capi::with-internal-metafile ((var &key pane bounds format owner drawing-mode)
                                          &body body)
    (declare (ignore format owner drawing-mode))
    `(oa::%om-call-with-internal-metafile
      ,pane ,bounds
      (lambda (,var) (declare (ignorable ,var)) ,@body)))

  ;; capi-capi-ref-101.htm:56 — draw METAFILE to PANE at X,Y with size WIDTH,HEIGHT;
  ;; :60 — the graphics-state TRANSFORM and MASK affect it, and the mask is PANE's
  ;; own box, as vanilla wraps the call (om-LW/icons-picts.lisp: the
  ;; with-graphics-state of om-draw-picture for (view internal-picture)).  The body is
  ;; in icons-picts.lisp, which owns the picture drawing and the clip primitive.
  (defun capi::draw-metafile (pane metafile x y width height)
    (oa::%om-draw-metafile pane metafile x y width height))

  (defun capi::free-metafile (metafile)
    "capi-capi-ref-167.htm:50 — release the storage used by METAFILE."
    (oa::om-kill-picture metafile))

  (defvar *capi-object-plists* (make-hash-table :test 'eq :weakness :key))

  (export '(capi::apply-in-pane-process capi::beep-pane
            capi::capi-object capi::capi-object-name capi::capi-object-plist
            capi::choice-selection capi::collection-items
             capi::collect-interfaces capi::current-pointer-position
             capi::destroy capi::display capi::display-dialog capi::display-message
             capi::exit-dialog capi::abort-dialog
            capi::draw-metafile capi::draw-metafile-to-image capi::drop-object
            capi::drop-object-allows-drop-effect-p capi::drop-object-drop-effect
            capi::drop-object-get-object capi::drop-object-pane
            capi::drop-object-pane-x capi::drop-object-pane-y
            capi::drop-object-provides-format capi::element-interface
             capi::find-interface capi::free-metafile
             capi::interface capi::interface-menu-bar-items
             capi::interface-display capi::interface-match-p
             capi::interface-menu-bar-items-bound-p capi::interface-owner
             capi::interface-reuse-p capi::item-data capi::item-text
             capi::locate-interface
            capi::manipulate-pinboard capi::menu capi::menu-component
            capi::menu-item capi::menu-items capi::menu-object capi::menu-title
            capi::metafile-port capi::pinboard-object-at-position
             capi::prompt-for-file capi::quit-interface capi::raise-interface
             capi::redisplay-element capi::reinitialize-interface
             capi::reuse-interfaces-p
            capi::set-drop-object-supported-formats capi::set-hint-table
            capi::set-pane-focus capi::static-layout-child-position
            capi::static-layout-child-size
            capi::text-input-pane-completion-function
            capi::text-input-pane-in-place-complete
            capi::update-drawing-with-cached-display
            capi::update-drawing-with-cached-display-from-points
            capi::with-internal-metafile)
          :capi))

;;; ======================================================================
;;; SECTION 6 — SYSTEM package, nickname SYS
;;;             (LispWorks sys:open-url, sys:call-system-showing-output)
;;; ======================================================================
;;;
;;; LispWorks: open-url url  (lw-sys-103.htm:40) -- displays URL in a browser.
;;; Used by: kernel doc/documentation.lisp: om-show-reference-doc for symbol and
;;; help-items for editorview (Help menu callbacks).
;;;
;;; LispWorks: call-system-showing-output command &key current-directory prefix
;;;            show-cmd output-stream wait shell-type kill-process-on-abort
;;;            external-format => status, output-string-or-signal-number
;;;            (lw-sys-14.htm:40)
;;; Used by: projects fluid/player/fluid-info.lisp: print-sf2-pgms (its #+linux and
;;; #+macosx branches).
;;; Without it the reader cannot resolve the single-colon sys: symbol on those
;;; platforms and the whole fluid project is skipped.

;;; SYS:OPEN-URL is provided by ALLOY-DESKTOP and imported by the SYSTEM
;;; compatibility package above.


;;; SYS:CALL-SYSTEM-SHOWING-OUTPUT is provided by ALLOY-DESKTOP and imported
;;; by the SYSTEM compatibility package above.

;;; ======================================================================
;;; SECTION 7 — GP package (LispWorks Graphics Package)
;;; ======================================================================
;;;
;;; The package and the symbol must exist before the kernel reader meets
;;; gp::draw-polygon; the drawing body is defined in graphics.lisp, which owns
;;; the pen state it needs and is loaded later.

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; reload idempotence: incremental compile&load re-runs this toplevel and
  ;; MAKE-PACKAGE signals on an existing package
  (unless (find-package "GP")
    (make-package "GP" :use nil))
  (export (list (intern "DRAW-POLYGON" "GP")) "GP"))

;;; ======================================================================
;;; SECTION 8 — :macosx, :cocoa and :mswindows features
;;;             (LW defines them on Mac/Windows; SBCL :darwin and :win32)
;;; ======================================================================

;; kernel/projects read #+macosx, #+cocoa and #+mswindows; must be set before they are read
#+darwin
(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :macosx *features*)
  (pushnew :cocoa *features*))

#+win32
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (pushnew :mswindows *features*))


;;; ======================================================================
;;; SECTION 9 — MISC
;;; ======================================================================

;;; push-end, push-end-new (from Serapeum/LispWorks)

(defun nconc1 (list item)
  "Destructively append ITEM at the end of LIST and return the list; (list ITEM) when LIST is NIL."
  (nconc list (list item)))

(defmacro push-end (item place &environment env)
  "Destructively push ITEM to the end of PLACE.
Like `push', but affects the last item rather than the first.

You may want to use `enq' on a `queue' instead.

From LispWorks."
  (multiple-value-bind (dummies vals temps setter getter)
      (get-setf-expansion place env)
    (when (rest temps) (error "Invalid place for push-end"))
    (let ((temp (car temps)))
      `(let* (,@(mapcar #'list dummies vals)
              (,temp ,getter))
         (setf ,temp (nconc1 ,temp ,item))
         ,setter))))

(defmacro push-end-new (item place
                        &rest kwargs
                        &key key test test-not
                        &environment env)
  "Pushes ITEM to the end of place (like `push-end') but only if it not already a member of PLACE (like `pushnew').

For the use of KEY, TEST, and TEST-NOT, see `pushnew'."
  (declare (ignore key test test-not))
  (alexandria:once-only (item)
    (multiple-value-bind (dummies vals temps setter getter)
        (get-setf-expansion place env)
      (when (rest temps) (error "Invalid place for push-end"))
      (let ((temp (car temps)))
        `(let* (,@(mapcar #'list dummies vals)
                (,temp ,getter))
           (unless (member ,item ,temp ,@kwargs)
             (setf ,temp (nconc1 ,temp ,item)))
           ,setter)))))

(defun ratiop (x)
 (sb-impl::ratiop x))


;;; ======================================================================
;;; LispWorks sequence compatibility
;;; ======================================================================
;;;
;;; LispWorks 8.1, lw-lispworks-105.htm:
;;; split-sequence separator-bag sequence &key start end test key
;;;                coalesce-separators count => sequences

(defun split-sequence
    (separator-bag sequence
     &key (start 0) end (test #'eql) key coalesce-separators
          (count most-positive-fixnum))
  "Return subsequences of SEQUENCE delimited by elements of SEPARATOR-BAG.

The returned parts retain the sequence type through SUBSEQ.  KEY is applied
only to input elements, before TEST compares each value with a separator.
COUNT limits the number of returned parts; the last part then contains the
unprocessed remainder, including any separators in that remainder."
  (check-type count (integer 1 *))
  (check-type start (integer 0 *))
  (when end (check-type end (integer 0 *)))
  (let* ((sequence-length (length sequence))
         (limit (or end sequence-length))
         (separator-count (length separator-bag))
         (test-function (coerce test 'function))
         (key-function (and key (coerce key 'function))))
    (unless (<= start limit sequence-length)
      (error "Invalid bounding indices START=~D END=~D for a sequence of length ~D."
             start limit sequence-length))
    (let ((parts nil)
          (part-start start)
          (parts-emitted 0))
      (labels ((separator-p (element)
                 (let ((value (if key-function
                                  (funcall key-function element)
                                  element)))
                   (loop for separator-index below separator-count
                         thereis
                         (funcall test-function value
                                  (elt separator-bag separator-index)))))
               (emit-part (part-end)
                 (unless (and coalesce-separators
                              (= part-start part-end))
                   (push (subseq sequence part-start part-end) parts)
                   (incf parts-emitted)
                   t)))
        (unless (= count 1)
          (loop for index from start below limit
                when (separator-p (elt sequence index))
                  do (let ((emitted-p (emit-part index)))
                       (setf part-start (1+ index))
                       (when (and emitted-p
                                  (= parts-emitted (1- count)))
                         (loop-finish)))))
        (emit-part limit)
        (nreverse parts)))))

(export 'ratiop :cl-user)
(export 'push-end :cl-user)
(export 'push-end-new :cl-user)
(export 'split-sequence :cl-user)


