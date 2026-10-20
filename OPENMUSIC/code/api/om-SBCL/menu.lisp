(in-package :om-api)

#+darwin
(defun %macos-install-window-menu (window)
  "Build WINDOW's NSMenu bar from its OM menus and the application menu, and install it
(alloy-desktop/native/macos-menu.lisp: macos-install-window-menu)."
  (alld:macos-install-window-menu window
                                  (lambda () (remove nil (%window-menubar window)))
                                  *om-app-menu-items*))

(export '(
                om-new-menu
                om-new-leafmenu
                om-make-menu
                om-create-menu
                om-copy-menu
                om-add-menu-items
                om-remove-menu-items
                om-menu-items
                om-set-menu-default-item
                om-set-menuitem-key
                om-menuitem-enable
                om-menuitem-disable
                om-find-menu-item
                om-get-menu-item
                om-menu-item-title

                om-window-class-menubar
                *om-default-menubar*
                *om-app-menu-items*
                om-add-about-menu
                om-add-menu-to-win

                om-pop-up-menu
                om-get-menu-context
                om-popup-menu-context
                om-open-pop-up-menu
                ) :om-api)

(defvar *om-default-menubar* t)
(defvar *om-app-menu-items*  nil)

(defclass om-menu-item (alld:menu-item) ())

(defmethod initialize-instance :after ((entry om-menu-item)
                                       &key (rac nil rac-p) &allow-other-keys)
  ;; :RAC is the historical OM-API initarg. The model state itself lives only
  ;; in ALLOY-DESKTOP:MENU-ITEM.
  (when rac-p
    (setf (alld:menu-entry-accelerator entry) rac)))

(defclass om-menu (alld:menu) ())

(defclass om-menu-component (alld:menu-component)
  ((interaction :initarg :interaction :initform :no-selection
                :accessor om-menu-component-interaction)))

;;; Compatibility accessors keep the OM-API surface while delegating to the
;;; single model state owned by alloy-desktop.
(defgeneric om-menu-item-title (entry))
(defgeneric (setf om-menu-item-title) (value entry))
(defmethod om-menu-item-title ((entry om-menu-item)) (alld:menu-entry-title entry))
(defmethod om-menu-item-title ((entry om-menu)) (alld:menu-entry-title entry))
(defmethod (setf om-menu-item-title) (value (entry om-menu-item))
  (setf (alld:menu-entry-title entry) value))
(defmethod (setf om-menu-item-title) (value (entry om-menu))
  (setf (alld:menu-entry-title entry) value))

(defgeneric om-menu-items (entry))
(defgeneric (setf om-menu-items) (value entry))
(defmethod om-menu-items ((entry om-menu)) (alld:menu-entry-items entry))
(defmethod om-menu-items ((entry om-menu-component)) (alld:menu-entry-items entry))
(defmethod (setf om-menu-items) (value (entry om-menu))
  (setf (alld:menu-entry-items entry) value))
(defmethod (setf om-menu-items) (value (entry om-menu-component))
  (setf (alld:menu-entry-items entry) value))

(defgeneric om-menu-item-action (entry))
(defgeneric (setf om-menu-item-action) (value entry))
(defmethod om-menu-item-action ((entry om-menu-item)) (alld:menu-entry-action entry))
(defmethod (setf om-menu-item-action) (value (entry om-menu-item))
  (setf (alld:menu-entry-action entry) value))

(defgeneric om-menu-item-accelerator (entry))
(defgeneric (setf om-menu-item-accelerator) (value entry))
(defmethod om-menu-item-accelerator ((entry om-menu-item))
  (alld:menu-entry-accelerator entry))
(defmethod (setf om-menu-item-accelerator) (value (entry om-menu-item))
  (setf (alld:menu-entry-accelerator entry) value))

(defgeneric om-menu-item-enabled-p (entry))
(defgeneric (setf om-menu-item-enabled-p) (value entry))
(defmethod om-menu-item-enabled-p ((entry om-menu-item)) (alld:menu-entry-enabled entry))
(defmethod om-menu-item-enabled-p ((entry om-menu)) (alld:menu-entry-enabled entry))
(defmethod (setf om-menu-item-enabled-p) (value (entry om-menu-item))
  (setf (alld:menu-entry-enabled entry) value))
(defmethod (setf om-menu-item-enabled-p) (value (entry om-menu))
  (setf (alld:menu-entry-enabled entry) value))

(defgeneric om-menu-item-selected-p (entry))
(defgeneric (setf om-menu-item-selected-p) (value entry))
(defmethod om-menu-item-selected-p ((entry om-menu-item)) (alld:menu-entry-selected entry))
(defmethod (setf om-menu-item-selected-p) (value (entry om-menu-item))
  (setf (alld:menu-entry-selected entry) value))

;; vanilla om-LW/menu.lisp (the defclass om-pop-up-menu) -- a pop-up menu IS a dialog item, so it carries the
;; om geometry slots; om-standard-dialog-item is forward-referenced (dialog-items.lisp).
(defclass om-pop-up-menu (om-standard-dialog-item om-menu) ())

(defgeneric menu? (object))
(defmethod  menu? ((self om-menu))      t)
(defmethod  menu? ((self om-pop-up-menu)) t)
(defmethod  menu? ((self t))            nil)

(defun om-new-menu (title &rest menus)
  (make-instance 'om-menu :title title :items menus))

(defun om-new-leafmenu (title action &optional (rac nil) (enable t) (select nil))
  (make-instance 'om-menu-item
                 :title    title
                 :action   action
                 :rac      rac
                 :enabled  enable
                 :selected select))

(defun om-make-menu (title itemlist)
  (let ((items (loop for elt in (remove nil itemlist)
                     collect (cond ((listp elt)
                                    (make-instance 'om-menu-component
                                                   :items (remove nil
                                                                  (if (symbolp (car elt)) (cdr elt) elt))
                                                   :interaction (if (eq :selection (car elt))
                                                                    :multiple-selection
                                                                    :no-selection)))
                                   ((functionp elt)
                                    (make-instance 'om-menu-component :items (list elt)))
                                   (t elt)))))
    (apply #'om-new-menu title items)))

(defun list-to-menu (menu)
  "Vanilla normalizer: wrap sublists as multiple-selection component groups."
  (if (listp menu)
      (loop for elt in menu
            collect (if (listp elt)
                        (make-instance 'om-menu-component
                                       :items (list-to-menu elt)
                                       :interaction :multiple-selection)
                        elt))
      menu))

(defun om-create-menu (class itemlist)
  (make-instance class :items (list-to-menu itemlist)))

(defun om-copy-menu (menu) menu)

(defun om-add-menu-items (menu end &rest menu-items)
  (apply #'alld:add-menu-items menu end menu-items))

(defun om-remove-menu-items (menu &rest menu-items)
  (apply #'alld:remove-menu-items menu menu-items))

(defun om-find-menu-item (menu title)
  (alld:find-menu-item menu title))

(defun om-get-menu-item (menu num)
  (alld:menu-item-at menu num))

(defun om-set-menu-default-item (menu defnum)
  (let ((target (nth defnum (om-menu-items menu))))
    (when (typep target 'om-menu-item)
      (setf (om-menu-item-selected-p target) t)))
  menu)

(defun om-set-menuitem-key (menu-item rac)
  (setf (om-menu-item-accelerator menu-item) rac)
  rac)

(defun om-menuitem-enable (menu-item)
  ;; kernel's enable-all-menu-items loops over ALL items incl. menus/components;
  ;; a whole om-menu grays its dropdown title too (vanilla :enabled-function)
  (when (typep menu-item '(or om-menu om-menu-item))
    (alld:set-menu-entry-enabled menu-item t))
  menu-item)

(defun om-menuitem-disable (menu-item)
  (when (typep menu-item '(or om-menu om-menu-item))
    (alld:set-menu-entry-enabled menu-item nil))
  menu-item)

(defgeneric om-window-class-menubar (window))
(defmethod  om-window-class-menubar ((window t)) nil)

(defgeneric internal-window-class-menubar (window))
(defmethod  internal-window-class-menubar ((window t)) nil)

(defvar *app-menubar-cache* nil
  "(*om-app-menu-items* . (om-menu)) — rebuilt only when the spec list changes.")

(defun default-app-menubar (window)
  "App menu (\"OM x.x\": About/Preferences/Quit) as a real om-menu, prepended to
every menubar so both render and accelerator dispatch see it (om-LW/menu.lisp:
default-app-menubar and om-add-menu-to-win).
Specs are (label action &optional rac), or a LIST of specs (nested group).
NIL on darwin, where the app menu is the native NSMenu one (vanilla #+cocoa nil,
om-LW/menu.lisp: the #+cocoa branch of default-app-menubar) — otherwise alloy-desktop/native/macos-menu.lisp: build-window-nsmenu would add it twice."
  (declare (ignore window))
  #+darwin nil
  #-darwin
  (when (and *om-app-menu-items* (second *om-app-menu-items*))
    (unless (eq (car *app-menubar-cache*) *om-app-menu-items*)
      (setf *app-menubar-cache*
            (cons *om-app-menu-items*
                  (list (om-make-menu
                         (first *om-app-menu-items*)
                         (mapcar (lambda (item)
                                   (if (listp (car item))
                                       (mapcar (lambda (sub)
                                                 (om-new-leafmenu (first sub) (second sub) (third sub)))
                                               item)
                                       (om-new-leafmenu (first item) (second item) (third item))))
                                 (second *om-app-menu-items*)))))))
    (cdr *app-menubar-cache*)))

(defun %capi-menu-callback-argument (item window)
  (case (capi::menu-callback-type item)
    (:interface window)
    (:data (capi::item-data item))
    (:item item)
    (otherwise nil)))

(defun %capi-menu-action (item window)
  (let ((callback (capi::menu-callback item))
        (type (capi::menu-callback-type item)))
    (when callback
      (lambda ()
        (if (member type '(:interface :data :item))
            (funcall callback (%capi-menu-callback-argument item window))
            (funcall callback))))))

(defun %capi-menu-predicate (function item window &optional default)
  (if function
      (lambda ()
        (funcall function
                 (or (capi::menu-setup-callback-argument item)
                     (%capi-menu-callback-argument item window)
                     item)))
      default))

(defun %capi-menu-entries (menu window)
  (let ((items-function (capi::menu-items-function menu)))
    (if items-function
        (list (lambda (owner)
                (declare (ignore owner))
                (remove nil
                        (mapcar (lambda (item) (%normalize-window-menu item window))
                                (funcall items-function
                                         (or (capi::menu-setup-callback-argument menu)
                                             window))))))
        (remove nil
                (mapcar (lambda (item) (%normalize-window-menu item window))
                        (capi::menu-items menu))))))

(defun %normalize-window-menu (menu window)
  (typecase menu
    (om-menu menu)
    (om-menu-item menu)
    (om-menu-component menu)
    (capi::menu
     (make-instance 'om-menu
                    :title (or (capi::menu-title menu)
                               (capi::capi-object-name menu) "")
                    :items (%capi-menu-entries menu window)
                    :enabled (%capi-menu-predicate
                              (capi::menu-enabled-function menu) menu window t)))
    (capi::menu-component
     (make-instance 'om-menu-component
                    :items (%capi-menu-entries menu window)))
    (capi::menu-item
     (make-instance 'om-menu-item
                    :title (or (capi::item-text menu)
                               (capi::menu-title menu)
                               (capi::capi-object-name menu) "")
                    :action (%capi-menu-action menu window)
                    :rac (let ((accelerator (capi::menu-item-accelerator menu)))
                           (unless (eq accelerator :default) accelerator))
                    :enabled (%capi-menu-predicate
                              (capi::menu-enabled-function menu) menu window t)
                    :selected (%capi-menu-predicate
                               (capi::menu-selected-function menu) menu window nil)))
    (t nil)))

(defun %window-menubar (window)
  "Return the normalized, complete menubar for WINDOW."
  (let ((source
          (if (capi::interface-menu-bar-items-bound-p window)
              (capi::interface-menu-bar-items window)
              (append (default-app-menubar window)
                      (internal-window-class-menubar window)
                      (om-window-class-menubar window)
                      (and (typep window 'om-look-window)
                           (window-instance-menu-items window))))))
    (remove nil (mapcar (lambda (menu) (%normalize-window-menu menu window))
                        source))))

(defgeneric om-add-about-menu (window))
(defmethod  om-add-about-menu ((window t)) nil)

(defun %resize-menu-window (window width height)
  ;; Preserve the OM sizing contract (constraints and host resize callbacks).
  (om-set-view-size window (om-make-point width height)))

(defun om-add-menu-to-win (window)
  (alld:set-window-menu-items window (%window-menubar window)
                              :app-menu *om-app-menu-items*
                              :provider #'%window-menubar
                              :resize-function #'%resize-menu-window)
  window)

(defgeneric om-get-menu-context (view))
(defmethod om-get-menu-context ((view t)) nil)

(defun om-add-dummy-menu-to-win (window)
  "Vanilla: strip the window's menubar (linux dialogs)."
  #+darwin (declare (ignore window))
  #-darwin
  (alld:set-window-menu-items window nil
                              :resize-function #'%resize-menu-window)
  nil)

(defclass om-popup-window (alld:popup-window) ()
  (:documentation "Compatibility name for a desktop popup surface."))

;; These adapters preserve OM's selection/caret policy, not the popup engine.
(defmethod alld:popup-repair-focus ((owner om-look-window))
  (%om-repair-window-focus owner))

(defmethod alld:popup-focus-pane ((owner om-look-window) pane)
  (%om-focus-pane nil pane))

(defun %focus-return-target (view)
  "Nearest alloy:focus-element up VIEW's container chain, or NIL — only
focus-elements accept (setf alloy:focus)."
  (loop for v = view then (and (typep v 'om-graphic-object) (vcontainer v))
        while v
        when (typep v 'alloy:focus-element) return v))

(defun om-open-pop-up-menu (themenu owner)
  (let* ((window (om-view-window owner))
         (mouse  (alld:cursor-location (alld:pointer-state)))
         (px     (alloy:pxx mouse))
         (py     (alloy:pxy mouse))
         (raw    (cond ((typep themenu 'om-menu)        (om-menu-items themenu))
                       ((typep themenu 'om-pop-up-menu) (om-menu-items themenu))
                       ((listp themenu)                 themenu)
                       (t (list themenu)))))
    (when window
      (alld:open-popup-menu window raw px py
                            :return-focus (%focus-return-target owner)))))

(defmethod om-context-menu-callback ((self om-graphic-object) x y)
  ;; vanilla: activate the window, then take the DEEPEST subview's menu context
  (let ((win (om-view-window self)))
    (when win (om-window-activate win t)))
  (let* ((clicked (or (om-find-view-containing-point self (om-make-point x y)) self))
         (items   (om-get-menu-context clicked)))
    (when items
      (om-open-pop-up-menu items (if (typep clicked 'om-graphic-object) clicked self))
      t)))

(defgeneric om-popup-menu-context (view &optional container-view))

(defmethod om-popup-menu-context ((self om-graphic-object) &optional container-view)
  (declare (ignore container-view))
  (let ((items (om-get-menu-context self)))
    (when items
      (om-open-pop-up-menu items self))))

(defmethod om-popup-menu-context ((self t) &optional container-view)
  (let ((items (om-get-menu-context self))
        (target (or container-view self)))
    (when (and items (typep target 'om-graphic-object))
      (om-open-pop-up-menu items target))))
