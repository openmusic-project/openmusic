(in-package :alloy-desktop)

;;; Tab layout: alloy:tab-view with a flush tab bar, the page on show, per-tab styles.

(defclass tab-layout (alloy:tab-view) ())

(defmethod initialize-instance :after ((self tab-layout) &key)
  (let ((tablist (alloy:index-element :north (alloy:layout-element self))))
    (setf (alloy:cell-margins tablist) (alloy:margins 0))))

(defun current-page (self)
  "The layout element of SELF's current tab, or NIL."
  (let ((tab (alloy:current self)))
    (when (typep tab 'alloy:tab) (alloy:layout-element tab))))

(defun make-tab (name page &key focus)
  "A tab titled NAME showing PAGE in a TAB-LAYOUT. FOCUS, when given, is the focus
element (e.g. a MAKE-FOCUS-LIST of PAGE's controls) that joins the layout's focus
tree while the tab is current."
  (make-instance 'alloy:tab :name name :layout-element page :focus-element focus))

(defun make-focus-list (elements)
  "A focus-list holding the focusable ELEMENTS not yet in a focus parent, for an alloy:tab focus-element."
  (let ((fl (make-instance 'alloy:focus-list)))
    (dolist (child elements)
      (when (and (typep child 'alloy:focus-element) (not (focus-attached-p child)))
        (alloy:enter child fl)))
    fl))

(defvar *tab-styles* (make-hash-table :test 'eq)
  "alloy:tab -> style plist read by the tab-button look.")

(defun store-tab-style (tab plist)
  "Record PLIST as TAB's style."
  (setf (gethash tab *tab-styles*) plist))

(defun tab-style (button key default)
  "KEY of the style stored for BUTTON's tab; DEFAULT if absent."
  (let ((s (gethash (alloy:active-value button) *tab-styles*)))
    (if s (getf s key default) default)))

(defun forget-tab-styles (layout)
  "Drop LAYOUT's tabs from *TAB-STYLES*, which otherwise keep the layout and its window reachable."
  (alloy:do-elements (tab layout)
    (remhash tab *tab-styles*))
  layout)

;;; Tab-button look: the default look's realization is on default-look-and-feel, so the
;;; window needs its own, plus a sizing strategy that fits the label for the flush tab bar.

(defparameter *tab-face* (chroma:rgb (/ 243 255.0) (/ 243 255.0) (/ 243 255.0))
  "Background of an unselected tab.")
(defparameter *tab-active-face* (chroma:rgb (/ 249 255.0) (/ 249 255.0) (/ 249 255.0))
  "Background of the selected tab.")
(defparameter *tab-border* (chroma:rgb (/ 229 255.0) (/ 229 255.0) (/ 229 255.0))
  "Separator between tabs, and the rule under an unselected tab.")
(defparameter *tab-focus-face* (chroma:rgb 0.75 0.75 0.75)
  "Background of the focused unselected tab.")

(presentations:define-realization (window alloy:tab-button)
  ((:border-bg simple:rectangle)
   (alloy:margins)
   :z-index 0)
  ((:background simple:rectangle)
   (alloy:margins 0 1 1 0)
   :z-index 1)
  ((:label simple:text)
   (alloy:margins 2 2)
   (alloy:name (alloy:active-value alloy:renderable))
   :font (font-entry-font (role-font-entry alloy:renderer :dialog-default))
   :size (font-entry-size-unit (role-font-entry alloy:renderer :dialog-default))
   :halign :middle :valign :middle
   :z-index 10))

(presentations:define-update (window alloy:tab-button)
  (:border-bg
   :pattern (tab-style alloy:renderable :border-color *tab-border*))
  (:background
   ;; The selected tab fuses with the panel; an unselected one is inset at the
   ;; bottom too, so the border rectangle below shows through as its base rule.
   :bounds (if (alloy:active-p alloy:renderable)
               (alloy:margins 0 1 1 0)
               (alloy:margins 0 1 1 1))
   :pattern (cond ((alloy:active-p alloy:renderable)
                   (tab-style alloy:renderable :active-bg *tab-active-face*))
                  ((alloy:focus alloy:renderable)
                   *tab-focus-face*)
                  (t
                   (tab-style alloy:renderable :bg *tab-face*))))
  (:label
   :pattern colors:black))

(defmethod initialize-instance :after ((button alloy:tab-button) &key)
  ;; alloy:radio bakes (fixed-size 15); re-install auto-fit measuring the live :label via suggest-size.
  (setf (alloy:sizing-strategy button)
        (alloy:dynamic-size
         (lambda (rb sz)
           (let* ((label (presentations:find-shape :label rb nil))
                  (w (if label
                         (alloy:pxw (alloy:suggest-size sz label))
                         (let* ((tree  (alloy:layout-tree rb))
                                (entry (and tree (role-font-entry (alloy:ui tree) :dialog-default))))
                           ;; a renderer with no atlas at all resolves no font to measure with
                           (if (and entry (font-entry-font entry))
                               (alloy:with-unit-parent rb
                                 (string-advance (font-entry-font entry)
                                                 (alloy:to-px (font-entry-size-unit entry))
                                                 (princ-to-string (alloy:name (alloy:active-value rb)))))
                               0)))))
             (alloy:px-size (+ w 4)
                            (+ (role-points-px :dialog-default) 18)))))))
