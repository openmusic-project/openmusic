;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;This File implements the class icon-view (a simple view which plots
;an icon in itself. 
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;;;=== ICONS ======

(defvar *om-icon-folder* nil)
(defvar *om-di-folder* nil)

(defvar *om-icon-type* '("tif" "tiff" "gif" "jpg" "jpeg" "png" "bmp"))

(defun init-icon-folder () 
   (setf *om-icon-folder* (make-pathname :device (pathname-device *om-root*) 
                                         :directory (append (pathname-directory *om-root*) (list "resources" "icon"))))
   (setf *om-di-folder* (make-pathname :device (pathname-device *om-root*) 
                                         :directory (append (pathname-directory *om-root*) (list "resources" "di")))))

(om-add-init-func 'init-icon-folder)

; (init-icon-folder)

;;; ==== SPECIAL ICON : id = number

(defun icon-string-name (icon dir)
  (let ((fileicon (and dir (find icon (om-directory dir :directories nil :files t)
                        :test '= :key #'(lambda (file) (if (and (pathname-name file) (> (length (pathname-name file)) 0)
                                                                (integerp (read-from-string (pathname-name file))))
                                                           (read-from-string (pathname-name file))
                                                         -1))))))
    (if fileicon (pathname-name fileicon)
      (format nil "~D" icon))))

;;; tests if a kernel resource exists for icon number
(defun om-probe-icon-resource (icon &optional folder)
  (let ((dir (if folder folder *om-icon-folder*)))
    (om-get-resource-file (icon-string-name icon dir) dir *om-icon-type*)))
  
; loads creates a system handle for the icon number ID if exists
; and returns the icon (icon can be freed with kill-picture)
(defun om-load-icon (id &optional folder)
  (let ((dir (if folder folder *om-icon-folder*)))
    (om-load-pixmap (icon-string-name id dir) *om-icon-type* dir t)))

 
; returns the list of all icon numbers in the kernel resources
(defun om-get-all-icons-id (&optional path)
  (let ((iconspath (if path path *om-icon-folder*))
        (icn-list nil))
    (loop for iconfile in (om-directory iconspath) do
          (unless (equal "" (pathname-name iconfile))
            (let ((icn (read-from-string (pathname-name iconfile))))
              (if (integerp icn) (push icn icn-list)))))
    (reverse icn-list)))


(defvar *default-icon* 179 "Icon by default")

;icon pos if the icon is already loaded
(defun loaded-icon-p (icon)
   (declare (special *om-package-tree*))
   (position icon (icon-resources *om-package-tree*) :test #'(lambda (x y) (= x (car y)))))

;get the icon handler from the loaded list
(defun get-icon-loaded (icon)
   (declare (special *om-package-tree*))
   (let ((n (loaded-icon-p icon)))
     (when n
       (second (nth (loaded-icon-p icon) (icon-resources *om-package-tree*))))))


; load the icon list (id handle) for the om default-icon
(defun load-default-icon-list ()
   (let (list resource) 
     (om-without-interrupts
      (setq resource (om-load-icon *default-icon*))
      (when resource
        (push (list *default-icon* resource) list)))
     list))


;;; a diolag for the user to choose an icon in kernel or user resources
;;; returns a icon number if it's a kernel icon
;;; returns a list (number package) if it's a user resource
;;; returns nil if no icon
(defun choise-icon (&optional (kernel t) (user t))
  (let ((res (choose-resource-dialog :icon :kernel kernel :user user))
        (icn nil))
    (when (consp res)
        (cond 
         ((equal (third res) 'kernel)
          (setf icn (car (get&corrige-icon 
                                     (read-from-string (string (first res)))))))
          ((equal (third res) 'user) 
           (setf icn (car (get&corrige-icon 
                      (list (read-from-string (string (first res))) *package-user*)))))
          (t nil))
      )
    icn))
;  (choise-icon t t)

;;;========================================
;;; ICONS ACCESSORS
;;;========================================

; returns list (iconID iconHandle) for an icon number
; correct ID/Handle if problem



(defmethod get&corrige-icon ((icon integer))
   (declare (special *om-package-tree*))
   (let (iconhdl)
     (unless (loaded-icon-p icon)
       (setq iconhdl (om-load-icon icon))
       (if iconhdl
         (push (list icon iconhdl) (icon-resources *om-package-tree*))
         (setf icon *default-icon*)))
     (list icon (get-icon-loaded icon))))

; if null returns default icon
(defmethod get&corrige-icon ((icon null))
   (get&corrige-icon *default-icon*))

(defun lib-or-om-icon (icon)
  (if (and *current-lib* (listp icon))
    (list icon *current-lib*)
    icon))

; if list (id package) return (iconID iconHdler) from the package resources
(defmethod get&corrige-icon ((icon list))
  (let ((rep (if (equal *package-user* (second icon))
                 (get-user-icon (car icon))
               (if (cadr icon) (get-lib-icon (car icon) (cadr icon)) icon))))
    (if (cadr rep) rep 
      (get&corrige-icon *default-icon*))))


(defun get-lib-icon (id lib)
  (let* ((icon-list (icon-resources lib))
         (pos (position id icon-list :test '= :key 'car)))
    (if pos 
        (list (list id lib) (second (nth pos icon-list)))
      (list (list id lib) nil))))

(defun get-user-icon (id)
  (let ((pos (position id (icon-resources *package-user*) :test '= :key 'car)))
    (if pos 
        (list (list id *package-user*) (second (nth pos (icon-resources *package-user*))))
      (let ((icn (om-load-icon id (make-pathname :directory (append (pathname-directory (mypathname *current-workspace*)) 
                                                                    (list "resources" "icon"))))))
        (when icn (push (list id icn) (icon-resources *package-user*)))
        (list (list id *package-user*) icn)))))


;;;=== ICON VIEW =======




(omg-defclass icon-view (om-item-view select-object) 
;(defclass icon-view (om-view) 
   ((iconID :initform 1  :initarg :iconID :accessor iconID))
   (:documentation "This class implement a simple view that draw one icon in it.
this class inherite from view.#enddoc#
#iconID# This slot stocks an ID for Cicn resources in the aplication. #iconID#
#selected-p# Used to draw the icon in selected or unselected mode.#selected-p#"))

(defmethod icon-view-p ((self t)) nil)
(defmethod icon-view-p ((self icon-view)) t)



(defmethod initialize-instance :after ((self icon-view) &key controls)
   (declare (ignore controls))
   (get&corrige-icon (iconID self)))


(defmethod plot-icon-view ((self icon-view) &optional (selected nil))
  "Plot the icon referenced by (iconID self) in the view self,
selected parameter is used to draw the icon in normal or selected mode"
  (when(iconID self)
    (let* ((iconparams (get&corrige-icon (iconID self)))
           (iconhdlr (second iconparams)))
      (setf (iconID self) (first iconparams))
      (om-with-focused-view self
        (om-draw-picture self iconhdlr :size (om-view-size self) :selected selected)
        ))))


(defmethod om-draw-contents ((self icon-view))
  (call-next-method)
  (plot-icon-view self (selected-p self)))


(defvar *icon-size-factor* 1)
(setf *icon-size-factor* 1)

;get the point ( (min 64 sizex) , (min 64 sizey)) where size is the size of the icon
; resizes according to the user pref ratio
(defun icon-sizes (Icon &optional (defsize '(nil nil)))
   (if icon
       (om-without-interrupts
         (let (resource)
           (setf resource (second (get&corrige-icon icon)))
           (list (* *icon-size-factor* (or (car defsize) (min 40 (om-pict-width resource))))
                 (* *icon-size-factor* (or (cadr defsize) (min 40 (om-pict-height resource)))))))
     (list 0 0)))
   
(defmethod om-view-cursor :around ((self icon-view))
   (if (and (om-control-key-p) (or (boxframe-p (om-view-container self)) (icon-finder-p (om-view-container self)))) 
       *om-contex-cursor*
       *om-arrow-cursor*
     ))


;;;============================
;;; deprecated classes for icon button : see om-icon-button
;;; except if icon is an OM icon and not a button !
;;;============================

;==========================================================
;;; ICON VIEW + ACTION

(omg-defclass button-icon (icon-view) 
   ((action :initform nil :initarg :action :accessor action))
   (:documentation "The button-icon is a special icon-view which perform an action
when you click in it.#enddoc#
#seealso# (icon-view lock-button unaire-fun-icon) #seealso#
#action# This slot contains a function with one parameter (the icon-button), 
this function is executed each time that the user click into the button-icon.#action#"))

(defmethod om-view-click-handler ((self button-icon) where)
   "this function call the slot action of SELF with the parameter SELF"
   (declare (ignore where))
   (setf (selected-p self) t)
   (om-redraw-view self)
   (om-init-motion-functions self nil 'release-button-action)
   ;(when (action self)
   ; (apply (action self) (list self)))
   )

(defmethod release-button-action ((Self button-icon) Where)
  (declare (ignore where))
  (when (action self)
    (apply (action self) (list self)))
  (setf (selected-p self) nil)
  (om-invalidate-view self t)
  )

;==========================================================
; stays hilited ("pushed") after click 
; drawn special if clicked

(omg-defclass hilite-button-icon (button-icon) ())

(defmethod om-view-click-handler ((self hilite-button-icon) where)
   (declare (ignore where))
   (when (action self)
     (apply (action self) (list self))))

;(defmethod om-draw-contents ((self hilite-button-icon))
;   (when (selected-p self)
;     (om-with-focused-view self
;       (om-with-hilite-foreground
;         (om-fill-rect 0 0 (w self) (h self)))))
;   (call-next-method)) 






