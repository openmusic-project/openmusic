(in-package :alloy-desktop)

;;; macOS native menu bar (NSMenu in the global system menu bar).
;;; Translates the neutral menu model (menu, menu-item, menu-component read through
;;; the menu-entry-* generics of menu/menu.lisp) into an NSMenu per window; the ObjC
;;; runtime binding it shares is in native/native.lisp. Elsewhere the in-window
;;; alloy:menubar of menu/menu.lisp is used.

#+darwin
(progn

;;; --- objc runtime + msgSend (arm64-safe: plain objc_msgSend only; the menu path
;;;     never returns float/struct, so objc_msgSend_fpret/_stret — OBJC_ARM64_UNAVAILABLE
;;;     on Apple Silicon — are never needed). ---

(cffi:defcfun ("objc_allocateClassPair" %objc-allocate-class-pair) :pointer
  (superclass :pointer) (name :string) (extra-bytes :unsigned-long))

(cffi:defcfun ("class_addMethod" %objc-class-add-method) :char  ; BOOL = signed char on macOS
  (cls :pointer) (name :pointer) (imp :pointer) (types :string))

(cffi:defcfun ("objc_registerClassPair" %objc-register-class-pair) :void
  (cls :pointer))

(declaim (inline %sel))
(defun %sel (name)
  "Register/return the SEL for NAME using the shared native binding."
  (%sel-register-name name))

(defmacro %msg (receiver selector return-type &rest typed-args)
  "Send SELECTOR to RECEIVER via plain objc_msgSend; TYPED-ARGS are CFFI type/value pairs."
  `(cffi:foreign-funcall "objc_msgSend"
                         :pointer ,receiver
                         :pointer (%sel ,selector)
                         ,@typed-args ,return-type))

(declaim (inline %class))
(defun %class (name) (%objc-get-class name))

(defun %nsstring (s)
  (%msg (%class "NSString") "stringWithUTF8String:" :pointer :string (or s "")))

(defun %release (obj)
  "Balance the +1 an alloc gives us, once the container has retained OBJ."
  (%msg obj "release" :void))

(defconstant +ns-command-mask+ #x100000 "NSEventModifierFlagCommand = 1<<20.")
(defconstant +ns-shift-mask+   #x20000  "NSEventModifierFlagShift = 1<<17.")

;;; --- click dispatch: NSMenuItem tag -> Lisp closure ---

(defun %new-menu-actions ()
  (make-array 8 :adjustable t :fill-pointer 0 :initial-element nil))

(defparameter *macos-menu-actions* (%new-menu-actions)
  "Index = NSMenuItem tag; element = the menu action closure to funcall on click.")

(defvar *menu-target* nil
  "Singleton instance of the runtime-built OMMenuTarget ObjC class (the items' target).")

(defvar *macos-menu-item-ptrs* nil
  "Alist (title . NSMenuItem pointer), rebuilt with each menubar build; drives live enable/disable.")

(defun %reset-menu-actions ()
  ;; a fresh vector, not (setf fill-pointer 0): lowering the fill-pointer keeps the
  ;; closures beyond it reachable from this global, and they close over the editor.
  (setf *macos-menu-actions* (%new-menu-actions))
  (setf *macos-menu-item-ptrs* nil))

(defun macos-update-item-enabled (title enabled-p)
  "setEnabled: on every cached NSMenuItem whose title matches (live host toggles)."
  (loop for (label . item) in *macos-menu-item-ptrs*
        when (string-equal label title)
        do (%msg item "setEnabled:" :void :char (if enabled-p 1 0))))

(defun %register-action (closure)
  "Store CLOSURE and return its integer tag."
  (vector-push-extend closure *macos-menu-actions*)
  (1- (fill-pointer *macos-menu-actions*)))

(cffi:defcallback %menu-action :void ((self :pointer) (cmd :pointer) (sender :pointer))
  ;; IMP for -[OMMenuTarget omMenuAction:] (encoding "v@:@"). A Lisp condition must
  ;; NEVER unwind across the ObjC ABI frame (SIGSEGV) — log and swallow here.
  (declare (ignore self cmd)
           (sb-ext:muffle-conditions sb-ext:compiler-note))
  (handler-case
      (let ((tag (%msg sender "tag" :long)))
        (when (and (integerp tag) (<= 0 tag) (< tag (fill-pointer *macos-menu-actions*)))
          (let ((action (aref *macos-menu-actions* tag)))
            (when action
              (float-features:with-float-traps-masked t
                (funcall action))))))
    (error (c)
      ;; last resort inside an ObjC frame: if even *error-output* is gone
      ;; (stream-error) there is nowhere left to report, and unwinding would SIGSEGV
      (handler-case
          (format *error-output* "[macos-menu] action error: ~A~%" (princ-to-string c))
        (stream-error () nil)))))

(defun %ensure-menu-target ()
  "Create the OMMenuTarget ObjC class once and return its singleton instance."
  (or *menu-target*
      (let ((cls (%objc-allocate-class-pair (%class "NSObject") "OMMenuTarget" 0)))
        (when (cffi:null-pointer-p cls)
          (error "objc_allocateClassPair OMMenuTarget failed (name in use?)"))
        (when (zerop (%objc-class-add-method cls (%sel "omMenuAction:")
                                             (cffi:callback %menu-action) "v@:@"))
          (error "class_addMethod omMenuAction: failed"))
        (%objc-register-class-pair cls)
        (setf *menu-target*
              (%msg (%msg cls "alloc" :pointer) "init" :pointer)))))

;;; --- accelerator (rac: a char, a 1-char string, or (:shift char/string)) ---

(defun %rac->key-equivalent (rac)
  "=> (values key-equivalent-string modifier-mask) for an NSMenuItem, or (values nil 0)."
  (labels ((char-of (x) (cond ((characterp x) x)
                              ((and (stringp x) (plusp (length x))) (char x 0))
                              (t nil)))
           (build (x shiftp)
             (let ((ch (char-of x)))
               (if ch
                   (values (string (char-downcase ch))
                           (logior +ns-command-mask+ (if shiftp +ns-shift-mask+ 0)))
                   (values nil 0)))))
    (cond ((null rac) (values nil 0))
          ((and (consp rac) (eq (first rac) :shift)) (build (second rac) t))
          (t (let ((ch (char-of rac)))
               (build rac (and ch (alpha-char-p ch) (upper-case-p ch))))))))

;;; --- NSMenu tree construction from the menu model ---

(defun %make-nsmenu (title)
  (let ((menu (%msg (%msg (%class "NSMenu") "alloc" :pointer)
                    "initWithTitle:" :pointer :pointer (%nsstring title))))
    (%msg menu "setAutoenablesItems:" :void :char 0)   ; honor our explicit enabled flags
    menu))

(defun %make-leaf-item (title action enabled-p rac)
  "Build an NSMenuItem wired to the OMMenuTarget singleton via a tag, or a separator."
  (multiple-value-bind (key-eq mask) (%rac->key-equivalent rac)
    (let ((item (%msg (%msg (%class "NSMenuItem") "alloc" :pointer)
                      "initWithTitle:action:keyEquivalent:" :pointer
                      :pointer (%nsstring (or title ""))
                      :pointer (%sel "omMenuAction:")
                      :pointer (%nsstring (or key-eq "")))))
      (when key-eq
        (%msg item "setKeyEquivalentModifierMask:" :void :unsigned-long mask))
      (%msg item "setTarget:" :void :pointer (%ensure-menu-target))
      (%msg item "setEnabled:" :void :char (if enabled-p 1 0))
      (%msg item "setTag:" :void :long (%register-action (or action (constantly nil))))
      (push (cons (or title "") item) *macos-menu-item-ptrs*)
      item)))

(defun %separator-item ()
  (%msg (%class "NSMenuItem") "separatorItem" :pointer))

(defun add-entry-to-nsmenu (nsmenu item window)
  "Translate one menu model element into NSMenuItem(s) appended to NSMENU."
  (cond
    ((typep item 'menu)                                 ; nested submenu
     (let* ((sub  (%make-nsmenu (menu-entry-title item)))
            (host (%msg (%msg (%class "NSMenuItem") "alloc" :pointer)
                        "initWithTitle:action:keyEquivalent:" :pointer
                        :pointer (%nsstring (menu-entry-title item))
                        :pointer (cffi:null-pointer)
                        :pointer (%nsstring ""))))
       (add-entries-to-nsmenu sub (menu-entry-items item) window)
       (%msg host "setSubmenu:" :void :pointer sub)
       (%release sub)
       (%msg nsmenu "addItem:" :void :pointer host)
       (%release host)))
    ((typep item 'menu-item)
     (let ((leaf (%make-leaf-item (menu-entry-title item)
                                  (menu-entry-action item)
                                  (menu-entry-enabled item)
                                  (menu-entry-accelerator item))))
       (%msg nsmenu "addItem:" :void :pointer leaf)
       (%release leaf)))))

(defun add-entries-to-nsmenu (nsmenu items window)
  "Append ITEMS to NSMENU with the CAPI grouping rule (same as menu/menu.lisp: popup-normalize-entries):
a menu-component is delimited by separators on both sides and an EMPTY group
adds nothing. A function element is an :items-function generator: it is
called with WINDOW and its result spliced in."
  (let ((first-p      t)
        (prev-group-p nil))
    (flet ((expand (subs)
             (loop for sub in subs
                   append (cond ((functionp sub) (remove nil (funcall sub window)))
                                (sub (list sub))))))
      (dolist (item items)
        (let* ((group-p (typep item 'menu-component))
               (subs    (expand (if group-p (menu-entry-items item) (list item)))))
          (when subs
            (when (and (not first-p) (or group-p prev-group-p))
              (%msg nsmenu "addItem:" :void :pointer (%separator-item)))
            (dolist (sub subs) (add-entry-to-nsmenu nsmenu sub window))
            (setf first-p      nil
                  prev-group-p group-p)))))))

(defun make-app-nsmenu (app-menu-items)
  "Build the application menu (first bar item) from APP-MENU-ITEMS, or NIL.
APP-MENU-ITEMS is (title specs); a spec is (label action &optional rac), or a list
of specs (a group, set off by a separator)."
  (when app-menu-items
    (let ((specs (second app-menu-items))
          (menu  (%make-nsmenu (or (first app-menu-items) ""))))
      (flet ((add-leaf (spec)
               (let ((leaf (%make-leaf-item (first spec) (second spec) t (third spec))))
                 (%msg menu "addItem:" :void :pointer leaf)
                 (%release leaf))))
        (loop for spec in specs
              with first-p = t
              when (consp spec)
              do (cond ((consp (first spec))
                        ;; nested group spec (list-headed item): separator + sub-leaves
                        (unless first-p
                          (%msg menu "addItem:" :void :pointer (%separator-item)))
                        (dolist (sub spec)
                          (when (consp sub) (add-leaf sub)))
                        (setf first-p nil))
                       (t (add-leaf spec)
                          (setf first-p nil)))))
      menu)))

(defun build-window-nsmenu (window menus-function app-menu-items)
  "Build the full NSMenu bar for WINDOW from APP-MENU-ITEMS + the menus that
MENUS-FUNCTION (no arguments) returns, called after the app menu is built.
Resets and repopulates *macos-menu-actions* (tags are per-build). Returns the NSMenu."
  (%reset-menu-actions)
  (let ((bar (%make-nsmenu "")))
    (%msg bar "setAutoenablesItems:" :void :char 0)
    ;; app menu (first, positional)
    (let ((app (make-app-nsmenu app-menu-items)))
      (when app
        (let ((host (%msg (%msg (%class "NSMenuItem") "alloc" :pointer)
                          "initWithTitle:action:keyEquivalent:" :pointer
                          :pointer (%nsstring "") :pointer (cffi:null-pointer)
                          :pointer (%nsstring ""))))
          (%msg host "setSubmenu:" :void :pointer app)
          (%release app)
          (%msg bar "addItem:" :void :pointer host)
          (%release host))))
    ;; one top-level item per menu
    (dolist (menu (funcall menus-function))
      (when (typep menu 'menu)
        (let* ((sub  (%make-nsmenu (menu-entry-title menu)))
               (host (%msg (%msg (%class "NSMenuItem") "alloc" :pointer)
                           "initWithTitle:action:keyEquivalent:" :pointer
                           :pointer (%nsstring (menu-entry-title menu))
                           :pointer (cffi:null-pointer)
                           :pointer (%nsstring ""))))
          (add-entries-to-nsmenu sub (menu-entry-items menu) window)
          (%msg host "setSubmenu:" :void :pointer sub)
          (%release sub)
          (%msg bar "addItem:" :void :pointer host)
          (%release host))))
    bar))

;;; --- install / focus swap (run on the GLFW main thread) ---

(defvar *window-nsmenus* (make-hash-table :test 'eq)
  "window -> (nsmenu actions item-ptrs): the bar it owns plus the tag tables built with it.")

(defun %nsapp ()
  (%msg (%class "NSApplication") "sharedApplication" :pointer))

(defvar *retired-nsmenus* nil
  "NSMenu bars awaiting release, drained one frame later by DRAIN-RETIRED-NSMENUS.")

(defun drain-retired-nsmenus ()
  "Release the bars retired on an earlier frame, once AppKit has unwound its stack:
a menu action may retire the very bar whose item AppKit is still working on."
  (let ((pending *retired-nsmenus*))
    (when pending
      (setf *retired-nsmenus* nil)
      (float-features:with-float-traps-masked t
        (with-autorelease-pool
          (dolist (menu pending) (%release menu)))))))

(defun retire-window-nsmenu (window)
  "Window funeral: drop WINDOW's registry and queue the NSMenu bar it owns."
  (let ((registry (gethash window *window-nsmenus*)))
    (when registry
      (float-features:with-float-traps-masked t
        (with-autorelease-pool
          (destructuring-bind (menu actions item-ptrs) registry
            (let ((app (%nsapp)))
              (when (cffi:pointer-eq menu (%msg app "mainMenu" :pointer))
                (%msg app "setMainMenu:" :void :pointer (cffi:null-pointer))))
            (remhash window *window-nsmenus*)
            (when (eq *macos-menu-actions* actions)
              (setf *macos-menu-actions* (%new-menu-actions)))
            (when (eq *macos-menu-item-ptrs* item-ptrs)
              (setf *macos-menu-item-ptrs* nil))
            (push menu *retired-nsmenus*)))
        window))))

(register-window-retirement :nsmenu
                            #'retire-window-nsmenu
                            :order 30)

(defun macos-install-window-menu (window menus-function app-menu-items)
  "Build WINDOW's NSMenu (see BUILD-WINDOW-NSMENU), cache it, and if WINDOW is key
make it [NSApp mainMenu]. Must run on the main thread (window setup already runs
there under the renderer)."
  (float-features:with-float-traps-masked t
    (with-autorelease-pool
      (retire-window-nsmenu window)                     ; a rebuild owns a new bar; free the old one
      (let ((app  (%nsapp))
            (menu (build-window-nsmenu window menus-function app-menu-items)))
        (setf (gethash window *window-nsmenus*)
              (list menu *macos-menu-actions* *macos-menu-item-ptrs*))
        (%msg app "setActivationPolicy:" :char :long 0) ; Regular: needed for a non-bundled bar
        (%msg app "setMainMenu:" :void :pointer menu)
        menu))))

(defun macos-focus-window-menu (window)
  "Swap [NSApp mainMenu] to WINDOW's cached NSMenu on focus-gain, and make its tag
tables the active ones (the tags are per-build, so they belong to that bar). Main
thread (focus callback runs there). No-op if the window has no menu yet."
  (let ((registry (gethash window *window-nsmenus*)))
    (when registry
      (float-features:with-float-traps-masked t
        (with-autorelease-pool
          (destructuring-bind (menu actions item-ptrs) registry
             (setf *macos-menu-actions*   actions
                   *macos-menu-item-ptrs* item-ptrs)
             (%msg (%nsapp) "setMainMenu:" :void :pointer menu)))))))


)  ; #+darwin progn
