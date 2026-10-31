(in-package :oa)

(defvar *class-definitions* nil
  "Alist of (class-name pathstring) recorded by add-class-definition; consumed by edit-definition.")

(defvar *method-definitions* nil
  "Alist of ((gf-name (specializer-classes)) pathstring) recorded by add-method-definition.")

(defparameter *rec-def-path-root* nil
  "Pathname root in effect when the kernel was compiled, captured by init-root-definition-pathname.")

(defparameter *new-def-path-root* nil
  "Pathname root in effect now, used to relocate recorded paths after a workspace move.")

(defun %recorded-source-path (path)
  "Namestring of the .lisp source for PATH. The kernel records *load-pathname*
(kernel omclass.lisp: the add-class-definition of the defclass* macro; kernel
ommethod.lisp: the add-method-definition of the defmethod* macro), which under SBCL
names the .fasl being
loaded; the LW image stores the .lisp."
  (when path
    (namestring (or (lisp-file? path) path))))

(defun add-class-definition (class path)
  (let ((pathstr (%recorded-source-path path))
        (name (class-name class)))
    (unless (find (list name pathstr) *class-definitions*
                  :test #'(lambda (a b) (and (equal (car a) (car b))
                                             (string-equal (cadr a) (cadr b)))))
      (push (list name pathstr) *class-definitions*))))

(defun %method-specializer-names (method)
  "Class specializers as class-names; eql specializers as (eql obj) conses."
  (mapcar (lambda (s) (if (consp s) s (class-name s)))
          (method-specializers method)))

(defun add-method-definition (method path)
  (let ((name (sb-mop:generic-function-name (sb-mop:method-generic-function method)))
        (pathstr (%recorded-source-path path)))
    (unless (find (list (list name (%method-specializer-names method)) pathstr)
                  *method-definitions*
                  :test #'(lambda (a b) (and (equal (caar a) (caar b))
                                             (equal (cadar a) (cadar b))
                                             (if (and (cadr a) (cadr b))
                                                 (string-equal (cadr a) (cadr b))
                                                 (equal (cadr a) (cadr b))))))
      (push (list
             (list name (%method-specializer-names method))
             pathstr) *method-definitions*))))

(defun restore-root (path oldroot newroot)
  (let ((rec-root-dir (pathname-directory oldroot))
        (path-dir (pathname-directory (translate-logical-pathname path))))
    (if (and (>= (length path-dir) (length rec-root-dir))
             (equal rec-root-dir (butlast path-dir (- (length path-dir) (length rec-root-dir)))))
        (merge-pathnames (make-pathname :name (pathname-name path)
                                        :type (or (pathname-type path) "lisp")
                                        :directory (append (pathname-directory newroot)
                                                           (nthcdr (length rec-root-dir) path-dir)))
                         newroot)
        path)))

(defun init-root-definition-pathname (oldroot newroot)
  (setf *rec-def-path-root* oldroot
        *new-def-path-root* newroot)
  (loop for def in *class-definitions* do
        (setf (nth 1 def) (namestring (restore-root (nth 1 def) oldroot newroot))))
  (loop for def in *method-definitions* do
        (setf (nth 1 def) (namestring (restore-root (nth 1 def) oldroot newroot)))))

(defun om-find-definitions (symbol &optional type)
  (loop for item in (cond ((equal type :method) *method-definitions*)
                          ((equal type :class)  *class-definitions*)
                          (t (append *method-definitions* *class-definitions*)))
        when (if (listp (car item)) (equal symbol (caar item)) (equal symbol (car item)))
        collect (list (car item)
                      (and (cadr item) (merge-pathnames (cadr item) (make-pathname :type "lisp"))))))

(defun %normalize-swank-label (label)
  "Parse a swank dspec label string -- \"(DEFGENERIC NAME (ARGS))\" or
\"(DEFMETHOD NAME SPEC...)\" -- into (NAME SIGNATURE-LIST), matching the vanilla
lw-find-definitions (name (specializers)) shape. Falls back to LABEL unchanged."
  (let ((form (and (stringp label)
                   ;; reader-error on a malformed dspec string -> fall back to raw label
                   (ignore-errors
                     (let ((*read-eval* nil) (*package* (find-package :oa)))
                       (read-from-string label nil nil))))))
    (if (and (consp form) (cdr form) (symbolp (second form)))
        (let ((name (second form))
              (rest (cddr form)))
          (cond ((and (= 1 (length rest)) (listp (first rest)))
                 (list name (first rest)))
                (rest (list name rest))
                (t (list name nil))))
        label)))

(defun sbcl-find-definitions (symbol &optional type)
  "List ALL definitions of SYMBOL via swank:find-definitions-for-emacs (the SBCL
   equivalent of the vanilla lw-find-definitions/dspec). Returns list of (label path)."
  (declare (ignore type))
  (when (and symbol (symbolp symbol))
    ;; swank introspects arbitrary definition kinds, so there is no specific class to
    ;; name: report WHICH symbol failed and re-signal, this is a user command
    (handler-bind ((error (lambda (c)
                            (format *error-output* "[sbcl-find-definitions ~A] ~A~%"
                                    symbol (princ-to-string c))
                            (finish-output *error-output*))))
      ;; CL builtins carry the SYS: sources of the machine the SBCL binary was
      ;; built on, absent here; the vanilla finder returns nothing for them
      ;; (lw-find-definitions 'car => NIL), so entries with no file on disk
      ;; are dropped.
      (loop for (label path) in (alld:find-definitions symbol)
            for restored = (and *rec-def-path-root* *new-def-path-root*
                                (restore-root path *rec-def-path-root* *new-def-path-root*))
            for existing = (cond ((and restored (pathnamep restored) (probe-file restored))
                                  (truename restored))
                                 ((probe-file path) (truename path)))
            for source = (and existing (or (lisp-file? existing) existing))
            when source
            collect (list (%normalize-swank-label label) source)))))

(defun %def-anchor (def)
  "Anchor for om-open-new-text-editor-at: a name string, or (name specializer*) when
DEF is a method descriptor ((gf-name (specializer-classes)))."
  (if (listp def)
      (let ((specs (and (cadr def)
                        (loop for tp in (cadr def) unless (eq tp t) collect (string tp)))))
        (if specs (cons (string (car def)) specs) (string (car def))))
      (string def)))

(defclass definitions-window (om-window)
  ((list1     :initform nil :accessor list1)
   (scroll-el :initform nil :accessor scroll-el)
   (clipper   :initform nil :accessor clipper)
   (xbar      :initform nil :accessor xbar)
   (ybar      :initform nil :accessor ybar)))

(defun %autohide-scrollbars (win)
  "Show each scroll-view scrollbar only when its axis overflows; reclaim the track otherwise."
  (let ((border-el (scroll-el win))
        (clip      (clipper win))
        (xbar      (xbar win))
        (ybar      (ybar win)))
    (when (and border-el clip xbar ybar)
      (alloy:with-unit-parent border-el
        (let* ((content (alloy:suggest-size (alloy:px-size 1 1) (alloy:inner clip)))
               (cw      (alloy:pxw content))
               (ch      (alloy:pxh content))
               (w       (alloy:pxw (alloy:bounds border-el)))
               (h       (alloy:pxh (alloy:bounds border-el)))
               (sb      20.0)
               (need-x  nil) (need-y nil))
          (dotimes (i 2)
            (setf need-x (> cw (- w (if need-y sb 0.0))))
            (setf need-y (> ch (- h (if need-x sb 0.0)))))
          (flet ((show (bar place)
                   (unless (eq bar (alloy:index-element place border-el))
                     (alloy:enter bar border-el :place place :size (alloy:un 20))))
                 (hide (bar place)
                   (when (eq bar (alloy:index-element place border-el))
                     (alloy:leave bar border-el))))
            (if need-x (show xbar :south) (hide xbar :south))
            (if need-y (show ybar :east)  (hide ybar :east)))
          (alloy:refit border-el))))))

(defmethod alloy:handle :around ((event alloy:key-down) (self definitions-window))
  "Drive the list directly from arrows; skipping call-next-method bypasses the default
map-event focus navigation, whose ensure-visible would reset the horizontal scroll."
  (case (alloy:key event)
    ((:up :down)
     (let ((list1 (list1 self)))
       (when list1
         (let* ((n   (alloy:element-count list1))
                (cur (alld:committed-index list1))
                (new (cond ((null cur) 0)
                           ((eq (alloy:key event) :down) (min (1- n) (1+ cur)))
                           (t (max 0 (1- cur))))))
           (when (and (plusp n) (not (eql new cur)))
             (setf (alld:committed-index list1) new)
             (alld:mark-list-children-for-render list1)
             (alld:notify-list-change list1 new)
             (let ((row (alloy:index-element new list1))
                   (cv  (alld:find-ancestor-clip-view list1)))
               (when (and row cv) (alloy:ensure-visible row cv)))))))
     t)
    ((:left :right)
     ;; Drive the x-scrollbar via its value API (the drag/pinch path) so the offset
     ;; setter notifies and the handle shape repaints; a slot bypass leaves it static.
     (let ((xbar (xbar self)))
       (when xbar
         (if (eq (alloy:key event) :right)
             (incf (alloy:value xbar) (alloy:step xbar))
             (decf (alloy:value xbar) (alloy:step xbar)))))
     t)
    ((:enter :return :kp-enter)
     (let ((list1 (list1 self)))
       (when (and list1 (alld:committed-index list1) (dialog-item-after-fun list1))
         (funcall (dialog-item-after-fun list1) list1)))
     t)
    (t (call-next-method))))

(defmethod om-window-class-menubar ((self definitions-window))
  (list (om-make-menu "File"
                      (list (om-new-leafmenu "Close" (lambda () (om-close-window self)) #\w)))))

(defmethod om-resize-callback ((self definitions-window) x y w h)
  (declare (ignore x y))
  (call-next-method)
  (let* ((menu (alld:window-menubar self))
         (mbh  (if menu (alloy:pxh (alloy:bounds (alloy:layout-element menu))) 0))
         (lw   (max 0 (- w 20)))
         (lh   (max 0 (round (- h mbh 20)))))
    (when (scroll-el self)
      (om-set-view-size (scroll-el self) (om-make-point lw lh))
      (om-set-view-position (scroll-el self) (om-make-point 10 10))
      (%autohide-scrollbars self))))

(defun %report-definition-failure (report message)
  "Beep and hand MESSAGE to REPORT, the way vanilla pairs BEEP-PANE with its
PRINT and, crucially, leaves the window OPEN (om-LW lw-lisp-tools/find-definition.lisp:
the Unknown location branch of show-definitions-dialog, and edit-definition).
Always returns NIL."
  (om-beep)
  (funcall report message)
  nil)

(defun show-definitions-dialog (symb deflist &optional (report (lambda (msg) (warn "~A" msg))))
  (let* ((items (loop for item in deflist collect
                      (format nil "~A -- ~A"
                              (if (listp (car item))
                                  (if (cadar item)
                                      (format nil "~A ~A" (caar item) (cadar item))
                                      (format nil "~A" (caar item)))
                                  (format nil "~A" (car item)))
                              (cadr item))))
         (win   (om-make-window 'definitions-window
                                :window-title (format nil "Definitions of ~A" (string-upcase (string symb)))
                                :size (om-make-point 460 240)
                                :min-size (om-make-point 300 150)
                                :position :centered
                                :resizable t)))
    (setf (list1 win)
          (om-make-dialog-item 'om-single-item-list
                               (om-make-point 0 0) (om-make-point 100 100) ""
                               :focus nil
                               :range items
                               :after-action
                               (lambda (item)
                                 (let ((idx (om-get-selected-item-index item)))
                                   (when idx
                                     (let* ((entry (nth idx deflist))
                                            (def   (car entry))
                                            (file  (cadr entry)))
                                       (cond ((not (and file (or (stringp file) (pathnamep file))))
                                              (%report-definition-failure
                                               report (format nil "Unknown location for ~A definition" symb)))
                                             ;; the OPEN is the success test: probe-file on the
                                             ;; recorded path passes for a .fasl with no .lisp beside it
                                             ((om-open-new-text-editor-at file (%def-anchor def))
                                              (om-close-window win))
                                             (t
                                              (%report-definition-failure
                                               report (format nil "File ~A not found" file))))))))))
    (let ((sv (make-instance 'alloy:scroll-view
                             :layout (list1 win) :focus (list1 win)
                             :layout-parent (om-window-root win)
                             :focus-parent  (om-window-focus win)
                             :scroll t)))
      (setf (scroll-el win) (alloy:layout-element sv))
      (alloy:do-elements (e (scroll-el win))
        (cond ((typep e 'alloy:clip-view)   (setf (clipper win) e))
              ((typep e 'alloy:x-scrollbar) (setf (xbar win) e))
              ((typep e 'alloy:y-scrollbar) (setf (ybar win) e)))))
    (%om-focus-pane win (list1 win))
    (setf (capi::interface-menu-bar-items win)
          (om-window-class-menubar win))
    (om-add-menu-to-win win)
    (om-resize-callback win 0 0 460 240)
    (om-select-window win)
    (%glfw:focus win)
    win))

(defun edit-definition (symbol &optional type (report (lambda (msg) (warn "~A" msg))))
  ;;; REPORT receives a one-line status string; caller decides where it is shown.
  (let ((definitions (or (om-find-definitions symbol type)
                         (sbcl-find-definitions symbol type))))
    (cond ((null definitions)
           (%report-definition-failure
            report (format nil "~A: no definition found" (string-upcase (string symbol)))))
          ((= 1 (length definitions))
           (let* ((entry (car definitions))
                  (def   (car entry))
                  (file  (cadr entry)))
             (cond ((not (and file (or (pathnamep file) (stringp file))))
                    (%report-definition-failure
                     report (format nil "Unknown location for ~A definition"
                                    (string-upcase (string symbol)))))
                   ((om-open-new-text-editor-at file (%def-anchor def)))
                   (t
                    (%report-definition-failure
                     report (format nil "~A => ~A (source not available)"
                                    (string-upcase (string symbol)) file))))))
          (t
           (show-definitions-dialog symbol definitions report)))))
