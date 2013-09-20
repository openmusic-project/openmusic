;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

;;===========================================================================
;DocFile
; MENUS AND POPUP MENUS
;DocFile
;;===========================================================================


(in-package :om-api)

;;;=========================
;;; export :
;;;=========================
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
;;;=========================


(defclass om-menu (capi::menu)
  ((enabled? :initform t :accessor enabled? :initarg :enabled?)))

(defmethod menu? ((self om-menu)) t)
(defmethod menu? ((self t)) nil)

;;;Creates a new menu with the given <title> and the list of <menus>.
(defun om-new-menu (title &rest menus)
   (let ((newvar (gensym)))
     (eval `(defvar ,newvar t))
     (make-instance 'om-menu 
                    :title title 
                    :callback-type :none
                    :items menus
                    ;; (list (make-instance 'capi:menu-component :items menus :callback-type :none :interaction :no-selection))
                    :enabled? newvar
                    :enabled-function #'(lambda (menu) (eval `,newvar)))))


(defclass om-menu-item-class (capi::menu-item)
  ((enabled? :initform t :accessor enabled? :initarg :enabled?)
   (selected? :initform nil :accessor selected? :initarg :selected?)))


;Creates a new leaf menu with the given <title> and <action>.
(defun om-new-leafmenu (title action &optional (rac nil) (enable t enablef-p) (select nil selectf-p))
  (let ((newvar (gensym))
        ;(selecvar (gensym))
        (enablef (cond ((functionp enable) #'(lambda (win) (funcall enable)))
                       ; (capi::execute-with-interface win enable)
                       ((and (symbolp enable) (fboundp enable)) #'(lambda (win) (funcall enable)))
                      (enable #'(lambda (win) t))
                      (enablef-p #'(lambda (win) nil))
                      (t nil)))
        (selectf  (cond ((functionp select) #'(lambda (win) (funcall select)))
                      ((and (symbolp select) (fboundp select)) #'(lambda (win) (funcall select)))
                      (select #'(lambda (win) t))
                      (selectf-p #'(lambda (win) nil))
                      (t nil)))
        item)
    (eval `(defvar ,newvar ,enable))
    (make-instance 'om-menu-item-class 
                   :title title 
                   :accelerator (if rac (concatenate 'string "accelerator-" rac) nil)
                   :enabled? newvar
                   ;:setup-callback-argument :none
                   :enabled-function enablef ;#'(lambda (menu) (eval `,newvar))
                   :callback-type :none
                   :selected-function selectf
                   :callback action
                   ;; :callback #'(lambda () (print title))
		   )
    ))



(defun om-make-menu (title itemlist)
  (let ((items (loop for elt in (remove nil itemlist) collect
                     (cond ((listp elt) 
                            (make-instance 
                             'capi:menu-component
                             :items (remove nil (if (symbolp (car elt)) (cdr elt) elt))
                             :callback-type :item
                             :interaction (if (equal :selection (car elt)) :multiple-selection :none)
                             ))
                           ((functionp elt)
                            (make-instance 
                             'capi:menu-component
                             :items-function elt
                             :callback-type :item
                             :interaction :none))
                           (t elt)))))
    (apply 'om-new-menu (cons title items))))

                       
;action est une function a deux args data et interface


(defun om-copy-menu (menu) menu) ;pour l'instant on n'a pas besoin


(defun om-add-menu-items (menu end &rest menu-items)
  (if end
    (setf (menu-items menu) (append (menu-items menu) menu-items))
    (setf (menu-items menu) (append menu-items (menu-items menu)))))

(defun om-remove-menu-items (menu &rest menu-items)
   (loop for item in menu-items do
         (setf (menu-items menu) (remove item (menu-items menu) :test 'equal))))

;get the list of menu items for the menu <menu>
(defun om-menu-items (menu)
  (menu-items menu))

(defun om-find-menu-item (menu title)
   "Returns menu item with given title or nil if none."
   (let ((pos (position title (menu-items menu) :test 'string-equal :key 'om-menu-item-title)))
     (when pos
       (nth pos (menu-items menu)))))

(defun om-get-menu-item (menu num)
   "Returns menu item with given title or nil if none."
   (nth num (menu-items menu)))

(defun om-menu-item-title (menuitem)
  (if (menu? menuitem)
      (capi::menu-title menuitem)
    (capi::item-title menuitem)))

(defun om-set-menu-default-item (menu defnum)
  (setf (capi::item-selected (nth defnum (menu-items menu))) t))


(defun om-menuitem-enable (menu-item)
 (eval `(setf ,(enabled? menu-item)  t)))

(defun om-menuitem-disable (menu-item)
(eval `(setf ,(enabled? menu-item)  nil)))

;-----------
(defun om-set-menuitem-key (menu-item rac)
  (setf (capi::menu-item-accelerator menu-item) rac))

;===========================


(defmethod om-window-class-menubar ((self om-abstract-window)) nil)

(defvar *om-default-menubar* t)
(defvar *om-app-menu-items* nil)

(defun default-app-menubar (window)
  #+cocoa nil   ;;; in cocoa the app menu is defined in the deliver program
  #-cocoa (when *om-app-menu-items* 
             (list (make-instance 'om-menu 
                                  :title (car *om-app-menu-items*)
                                  :callback-type :none
                                  :items ; (append 
                                          (mapcar (lambda (item) 
                                                    (if (listp (car item))
                                                        (make-instance 'capi:menu-component
                                                                       :items (mapcar #'(lambda (sub) (om-new-leafmenu (car sub) (cadr sub)
                                                                                                                       (caddr sub))) item)
                                                                       :callback-type :item :interaction :no-selection)
                                                      (om-new-leafmenu (car item) (cadr item) (caddr item)))
                                                    )
                                                 (cadr *om-app-menu-items*))
                                          ;(list (make-instance 
                                          ;       'capi:menu-component
                                          ;       :items (list (om-new-leafmenu "Quit" #'(lambda () (quit :confirm nil))))
                                          ;       :callback-type :item
                                          ;       :interaction :no-selection))
                                          ))))


(defun om-add-menu-to-win (window) 
 (let ((menubar (remove nil (append (default-app-menubar window) (om-window-class-menubar window)))))
   (capi::execute-with-interface window
                                 #'(lambda () (setf (capi::interface-menu-bar-items window) menubar)
                                     ))))

; om::make-editor-window

   

;;;;===================
;;;; POP UP / CONTEXT MENU
;;;;===================

(defun om-open-pop-up-menu (themenu self)
  (capi::display-popup-menu themenu :owner self))

;;; DEFAULT BEHAVIOR
;;; right click -> open menu context
(defvar *menu-context-open* nil)

(defmethod om-context-menu-callback ((self om-graphic-object) x y)
  ;;; cancel d&d init
  (let ((win (capi::top-level-interface self)))
    (capi::find-interface (type-of win) :name (capi::capi-object-name win)))
  (om-activate-callback self t)
  (let ((clicked (om-find-view-containing-point self (om-make-point x y))))
    (om-view-click-handler clicked (om-convert-coordinates (om-make-point x y) self clicked))
    (when (om-get-menu-context clicked)
      (setf *menu-context-open* t)
      (om-open-pop-up-menu (capi::make-menu-for-pane self (list-to-menu (om-get-menu-context clicked))) self)
      (setf *menu-context-open* nil))
    ;(setf (drag-on *om-drag-session*) nil) ;; test 12/04/09
    ))

(defun list-to-menu (menu)
  (if (listp menu)
      (loop for elt in menu collect 
            (if (listp elt)
                (make-instance 
                 'capi:menu-component
                 :items (list-to-menu elt)
                 :interaction :multiple-selection)
              (list-to-menu elt)
              ))
    menu))

(defmethod om-get-menu-context ((self om-graphic-object)) nil)


;;; POP-UP AS AN OBJECT
;;; Explicit call for open

(defclass om-pop-up-menu (om-standard-dialog-item capi::menu) ())

(defun om-create-menu (class itemlist)
  (make-instance class :items (list-to-menu itemlist)))

;;; pour un objet graphique
(defmethod om-popup-menu-context ((self om-graphic-object) &optional container-view)
  (let ((themenu (capi::make-menu-for-pane self (om-get-menu-context self))))
    (om-open-pop-up-menu themenu self)))

;;; pour un objet non graphique
(defmethod om-popup-menu-context ((self t) &optional container-view)
  (let ((themenu (capi::make-menu-for-pane (om-get-menu-context self) self))
        (container (if container-view container-view self)))
    (om-open-pop-up-menu themenu container)))







