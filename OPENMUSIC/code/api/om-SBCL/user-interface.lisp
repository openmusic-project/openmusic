(in-package :om-api)

(export '(
                om-get-user-string
                om-choose-directory-dialog
                om-choose-new-directory-dialog
                om-choose-file-dialog
                om-choose-new-file-dialog
                om-y-or-n-dialog
                om-y-n-cancel-dialog
                om-choose-color-dialog
                om-pick-color-view
                om-choose-font-dialog
                om-message-dialog
                om-beep
                ) :om-api)

;;; ---- Dialog support ----

(defvar *om-file-dialog-backend* nil
  "Optional test/platform adapter called as (operation &rest normalized-options).
When NIL, the org.shirakumo.file-select native backend is used.")

;;; ---- File / directory choosers (delegate to file-select) ----

(defun om-choose-file-dialog
    (&key (prompt "Choose a File") pathname directory button-string types filter
          owner if-exists if-does-not-exist file-package-is-directory
          pane-args popup-args)
  "Pick an existing file through alld:run-file-dialog (the native dialog, or the
adapter in *om-file-dialog-backend* when set) and remember its directory in
*last-directory*; return the pathname, or NIL when the user cancels."
  (declare (ignore button-string))
  (let* ((options (list :title (alld:dialog-title prompt "Choose a File")
                        :default (or pathname directory *last-directory*)
                        :filter (alld:types->filter types)
                        :owner owner :selected-filter filter
                        :if-exists if-exists
                        :if-does-not-exist if-does-not-exist
                        :file-package-is-directory file-package-is-directory
                        :pane-args pane-args :popup-args popup-args))
         (rep (alld:run-file-dialog :open options :backend *om-file-dialog-backend*)))
    (when rep
      (setf *last-directory* (alld:directory-to-remember rep :file)))
    rep))

(defun om-choose-new-file-dialog
    (&key (prompt "Choose a New File") pathname directory (name "")
          button-string types filter owner if-exists if-does-not-exist
          file-package-is-directory pane-args popup-args)
  (declare (ignore button-string))
  ;; Prefill the whole NAME as the file component (no dot-split): object names may
  ;; carry dots that are not extensions; the :types filter drives the real extension.
  (let* ((dir (or directory pathname *last-directory*))
         (default (or pathname
                      (cond (dir (make-pathname :directory (pathname-directory dir)
                                            :name (when (plusp (length name)) name)
                                            :type nil))
                            ((plusp (length name)) (make-pathname :name name)))))
         (options (list :title (alld:dialog-title prompt "Choose a New File")
                        :default default :filter (alld:types->filter types)
                        :owner owner :selected-filter filter
                        :if-exists if-exists
                        :if-does-not-exist if-does-not-exist
                        :file-package-is-directory file-package-is-directory
                        :pane-args pane-args :popup-args popup-args))
         (rep (alld:run-file-dialog :save options :backend *om-file-dialog-backend*)))
    (when rep
      (setf *last-directory* (alld:directory-to-remember rep :file)))
    rep))

(defun om-choose-directory-dialog (&key (prompt "Choose a Directory") directory)
  ;; A :directory reply is already directory-shaped on every backend
  ;; (file-select/protocol.lisp: normalize-selection).
  (let ((rep (alld:choose-directory
              (make-instance 'alld:file-dialog-service)
              :title (alld:dialog-title prompt "Choose a Directory")
              :default (or directory *last-directory*))))
    (when rep
      (setf *last-directory* (alld:directory-to-remember rep :directory))
      rep)))

(defun om-choose-new-directory-dialog (&key (prompt "Choose location and name for the new directory")
                                            directory defname)
  ;; Vanilla names a new directory via a Save dialog (prompt-for-file :operation :save);
  ;; file-select's :new WITHOUT :filter :directory is that Save dialog (the :directory
  ;; filter would set FOS_PICKFOLDERS, which win32/macOS reject). The typed name is folded
  ;; into a directory pathname, then created on disk.
  (let* ((dir     (or directory *last-directory*))
         (default (cond (dir     (make-pathname :directory (pathname-directory dir)
                                                :name defname :type nil))
                        (defname (make-pathname :name defname))))
         (rep     (alld:choose-new-directory
                   (make-instance 'alld:file-dialog-service)
                   :title (alld:dialog-title prompt "Choose location and name for the new directory")
                   :default default)))
    (when rep
      (let ((full (alld:as-directory-pathname rep)))
        (setf *last-directory* (alld:directory-to-remember full :new-directory))
        (ensure-directories-exist full)
        full))))

;;; ---- Color / font choosers (delegate to color-select / font-select) ----

(defun om-choose-color-dialog (&key color (full-open t) prevent-full-open)
  (let ((default (when (and color (om-color-p color))
                   (list (om-color-r color) (om-color-g color) (om-color-b color)))))
    (multiple-value-bind (rgb ok)
        (alld:choose-color (make-instance 'alld:color-dialog-service)
                           :title "Color"
                           :default default
                           :full-open full-open
                           :prevent-full-open prevent-full-open)
      (when (and ok rgb)
        (destructuring-bind (r g b) rgb
          (om-make-color r g b))))))

(defun %font->select-plist (font)
  "FONT as the property list font-select speaks: (:family STRING :size INTEGER
:weight (:regular|:bold) :slant (:roman|:italic)) (font-select/protocol.lisp: the
plist contract described in select).
Every backend reads it with GETF (font-select/win32.lisp: the select-with for win32;
font-select/macos.lisp: the select-with for macos; font-select/gtk.lisp: the
select-with for gtk; font-select/yad.lisp: the select-with for yad), so the
OM-FONT-DESCRIPTION the kernel carries has to
be converted here. :SIZE is the REALIZED POINT, the number the native dialog shows
and returns; the descriptor's unit has no counterpart in the plist and is not sent.
The plist contract types :SIZE as an integer and the win32 backend pushes it into
an :int32 foreign slot, so the point is rounded HERE, at that boundary only -- the
fraction is preserved on the way back by om-choose-font-dialog.
NIL means \"no preselection\"; a plist is passed through."
  (cond ((null font) nil)
        ((om-font-p font)
         (let ((family (om-font-face font))
               (size   (%omfd-point font)))
           (list :family (if (stringp family) family *om-def-font-face*)
                 :size   (if (realp size) (round size) (nth 0 *om-def-font-sizes*))
                 :weight (if (member :bold   (om-font-style font)) :bold   :regular)
                 :slant  (if (member :italic (om-font-style font)) :italic :roman))))
        ((consp font) font)
        (t nil)))

(defun om-choose-font-dialog (&key (font *om-default-font2*) (variant :full))
  "The chosen font is a NATIVE POINT descriptor: vanilla returns the description of
the realized font and never re-enters om-make-font (om-LW/user-interface.lisp:
om-choose-font-dialog), so the size the dialog returns is a point size and is
stored verbatim."
  (let ((default (%font->select-plist font)))
    (multiple-value-bind (plist ok)
        (alld:choose-font (make-instance 'alld:font-dialog-service)
                          :default default :variant variant)
      (when (and ok plist)
        ;; macOS font-select returns (:family :system ...) the caller must re-resolve.
        (let ((family (getf plist :family)))
          (when (eq family :system)
            (setf family (or (getf default :family) *om-def-font-face*)))
          (%om-make-font-pt family
                            (getf plist :size)
                            :style (append (when (eq (getf plist :weight) :bold) '(:bold))
                                           (when (eq (getf plist :slant) :italic) '(:italic)))))))))

;;; ---- Modal message / confirmation dialogs (kernel-spec: om-dialog + subviews) ----

;;; Vanilla hands these four to native LispWorks prompts, which size themselves to the
;;; message and ignore :size (om-LW/user-interface.lisp: om-get-user-string,
;;; om-y-or-n-dialog, om-y-n-cancel-dialog and om-message-dialog). The fork
;;; builds the window itself, so it measures the text first; :size stays the MINIMUM.
;;; alloy-desktop composes the prompt; this host supplies the OM objects. The font and
;;; the colours are read each time a method runs: the kernel reassigns *om-white-color*
;;; to the user's background preference.

(defclass om-prompt-host (alld:prompt-host) ())

(defvar *om-prompt-host* (make-instance 'om-prompt-host))

(defmethod alld:prompt-measure-width ((host om-prompt-host) line)
  (om-string-size line *om-default-font2*))

(defmethod alld:prompt-line-height ((host om-prompt-host))
  (om-string-h *om-default-font2*))

(defmethod alld:prompt-wrapped-rows ((host om-prompt-host) line width)
  (%wrapped-row-count line *om-default-font2* width))

(defmethod alld:prompt-make-window ((host om-prompt-host) title width height)
  (om-make-window 'om-dialog :position :centered :size (om-make-point width height)
                             :resizable nil :maximize nil :minimize nil
                             :window-title title :bg-color *om-window-def-color*))

(defmethod alld:prompt-make-static-text ((host om-prompt-host) x y width height text wrap-p)
  (om-make-dialog-item 'om-static-text (om-make-point x y) (om-make-point width height) text
                       :font *om-default-font2* :wrap wrap-p :bg-color *om-white-color*))

(defmethod alld:prompt-make-editable-text ((host om-prompt-host) x y width height text)
  (let ((input (om-make-dialog-item 'om-editable-text (om-make-point x y)
                                    (om-make-point width height) text
                                    :font *om-default-font2* :bg-color *om-white-color*)))
    (om-set-dialog-item-text input text)
    input))

(defmethod alld:prompt-make-button ((host om-prompt-host) x y width height label default-p action)
  (om-make-dialog-item 'om-button (om-make-point x y) (om-make-point width height) label
                       :default-button default-p
                       :di-action (om-dialog-item-act item
                                    (declare (ignore item))
                                    (funcall action))))

(defmethod alld:prompt-add-items ((host om-prompt-host) window items)
  (apply #'om-add-subviews window items))

(defmethod alld:prompt-run-modal ((host om-prompt-host) window &optional focus)
  (%om-modal-dialog window nil :focus focus))

(defmethod alld:prompt-item-text ((host om-prompt-host) item)
  (om-dialog-item-text item))

(defmethod alld:prompt-exit-modal ((host om-prompt-host) window value)
  (om-return-from-modal-dialog window value))

(defun om-message-dialog (message &key (window-title "Warning") owner
                                       (size (om-make-point 335 100))
                                       (position (om-make-point 200 140)))
  "Display the message dialog entirely on the Alloy/GLFW UI thread."
  (declare (ignore owner position))
  (alld:message-dialog *om-prompt-host* message :title window-title
                                                :minimum-width (om-point-h size)
                                                :minimum-height (om-point-v size)))

(defun om-y-or-n-dialog (message &key (size (om-make-point 300 150)) default-button)
  "Display a modal Yes/No prompt entirely on the Alloy/GLFW UI thread."
  (alld:yes-or-no-dialog *om-prompt-host* message :minimum-width (om-point-h size)
                                                  :minimum-height (om-point-v size)
                                                  :default-button default-button))

(defun om-y-n-cancel-dialog (message &key (size (om-make-point 300 150)) default-button)
  "Display a modal Yes/No/Cancel prompt entirely on the Alloy/GLFW UI thread."
  (alld:yes-no-cancel-dialog *om-prompt-host* message :minimum-width (om-point-h size)
                                                      :minimum-height (om-point-v size)
                                                      :default-button default-button))

(defun om-get-user-string (prompt &key (initial-string "") (window-title "Get string") owner
                                       (size (om-make-point 365 100))
                                       (position (om-make-point 200 140)))
  "Prompt for a string entirely on the Alloy/GLFW UI thread."
  (declare (ignore owner position))
  (alld:prompt-for-string *om-prompt-host* prompt :initial-string initial-string
                                                  :title window-title
                                                  :minimum-width (om-point-h size)
                                                  :minimum-height (om-point-v size)))

(defun om-beep ()
  "Emit the native system alert beep (kernel error/attention beep)."
  (alld:system-beep))

(defclass om-pick-color-view (om-view)
  ((color     :accessor color     :initarg :color     :initform (om-make-color 0 0 0))
   (after-fun :accessor after-fun :initarg :after-fun :initform nil)))

(defmethod om-view-click-handler ((self om-pick-color-view) pos)
  (declare (ignore pos))
  (let ((c (om-choose-color-dialog :color (color self) :full-open nil)))
    (when c
      (om-set-bg-color self c)          ; before after-fun: caller reads back via om-get-bg-color
      (setf (color self) c)
      (when (after-fun self) (funcall (after-fun self) self)))))

