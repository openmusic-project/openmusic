(in-package :om)

;; LispWorks' delete-file accepts a 2nd ignored arg; cl:delete-file does not.
;; The shadow has to be in place before the reader meets the symbol, in this file
;; and in every kernel file read afterwards -- hence :compile-toplevel too, so the
;; file behaves the same whether it is loaded as source or compiled.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'delete-file :om))

(defun delete-file (path &optional (errorp nil))
  ;; LW semantics (measured live): nil tolerates only non-existence;
  ;; :no-error swallows any failure; :error (or t) always signals.
  (case errorp
    ((:no-error) (handler-case (cl:delete-file path)
                   (file-error () nil)))
    ((:error t) (cl:delete-file path))
    (otherwise (if (probe-file path)
                   (cl:delete-file path)
                   nil))))

;; LispWorks' make-pathname accepts :unspecific in the host/device slots and reads a
;; one-letter string host as a Windows drive; SBCL requires (or string host null) for
;; :host and treats a string host as a LOGICAL host name.  Patches saved by older OM
;; versions carry the literal form
;;   (make-pathname :directory '(:relative "in-files") :device :unspecific
;;                  :host :unspecific :name "INV4" :type "MID")
;; (e.g. resources/tutorials/OM-Tutorials/tutorial-28.omp), and that form is READ in
;; this package while the .omp is loaded, so the shadow is what reaches it.
;; Same mechanism as the delete-file shadow above; the body is a strict widening of
;; cl:make-pathname (every other argument is passed through untouched).
(eval-when (:compile-toplevel :load-toplevel :execute)
  (shadow 'make-pathname :om))

(defun make-pathname (&rest args &key (host nil host-p) (device nil device-p)
                      &allow-other-keys)
  (declare (ignore host-p device-p))
  (let ((a (copy-list args)))
    (when (eq host :unspecific) (setf (getf a :host) nil))
    (when (eq device :unspecific) (setf (getf a :device) nil))
    ;; A one-letter alphabetic string in :host is a Windows drive under LW, never a
    ;; logical host.  kernel graphics/icons/picture.lisp (omNG-save for
    ;; internepicture) passes the same
    ;; pathname-device to :host AND :device, so the letter must be dropped from
    ;; :host even when :device is already filled -- requiring an empty :device here
    ;; left that callsite reaching cl:make-pathname with :host "c" and signalling.
    (let ((h (getf a :host)))
      (when (and (stringp h) (= 1 (length h)) (alpha-char-p (char h 0)))
        (unless (getf a :device) (setf (getf a :device) h))
        (setf (getf a :host) nil)))
    (apply #'cl:make-pathname a)))

;;; special-form-p
(defun special-form-p (x)
 (cl-user::special-operator-p x))

;; Metaclass stubs: SBCL PCL requires the metaclass in find-class at COMPILE time.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*om-version*)
    (defvar *om-version* nil "Stub — real value defined by kernel init"))

  (unless (find-class 'omobject nil)
    (defclass omobject ()
      ((name :initform nil :initarg :name :accessor name))))

  (unless (find-class 'ombasicobject nil)
    (defclass ombasicobject (omobject)
      ((icon :initform nil :initarg :icon :accessor icon)
       (frames :initform nil :accessor frames)
       (editorframe :initform nil :accessor editorframe)
       (attached-objs :initform nil :accessor attached-objs)
       (protected-p :initform nil :initarg :protected-p :accessor protected-p)
       (infowin :initform nil :accessor infowin))))

  (unless (find-class 'ompersistantobject nil)
    (defclass ompersistantobject (ombasicobject)
      ((mypathname :initform nil :initarg :mypathname :accessor mypathname)
       (omversion :initform *om-version* :accessor omversion)
       (doc :initform "" :accessor doc)
       (loaded? :initform t :accessor loaded?)
       (saved? :initform t  :accessor saved?)
       (wsparams :initform nil :accessor wsparams)
       (create-info :initform '(nil nil) :accessor create-info)
       (changed-wsparams? :initform nil :accessor changed-wsparams?))))

  (unless (find-class 'omclass nil)
    (defclass omclass (standard-class ompersistantobject)
      ((lib-class-p :initform nil :accessor lib-class-p)
       (internal-met :initform nil :accessor internal-met)
       (slot-docs :initform nil :accessor slot-docs)))
    (defmethod sb-mop:validate-superclass ((class omclass) (super standard-class)) t))

  (unless (find-class 'omstandardclass nil)
    (defclass omstandardclass (omclass) ()
      (:metaclass omclass)))

  (defmethod sb-mop:validate-superclass ((class omstandardclass) (super standard-class)) t)
  (defmethod sb-mop:validate-superclass ((class standard-class) (super omstandardclass)) t)
  (defmethod sb-mop:validate-superclass ((class standard-class) (super omclass)) t))
