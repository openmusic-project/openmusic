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
;Class hierarchical tree editor.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defun open-new-RelationTree (object name elements)
   (let* (newwindow)
     (setf newwindow 
           (make-editor-window 'classTreeEditor object name nil 
                               :winsize (om-make-point 500 300) 
                               :winpos (get-win-position object)))
     (om-with-delayed-update (panel newwindow)
       (mapc #'(lambda (elem)
                 (let ((newframe (make-frame-from-callobj elem)))
                   (om-add-subviews (panel newwindow) newframe)
                   (add-subview-extra newframe))) elements)
       (mapc #'(lambda (elem)
                 (update-graphic-connections elem elements)) (get-subframes (panel newwindow)))
       )
     (add-window-buttons (panel newwindow))
     (set-field-size (panel newwindow))
     newwindow))


;---------------------------------------------

(defclass classTreeEditor (relationEditor)  ()
   (:documentation "xx"))

(defmethod get-editor-panel-class ((self classTreeEditor))  'classTreePanel)

(defmethod om-view-close-event ((self classTreeEditor)) nil)
(defmethod get-clipboard ((self classTreeEditor)) nil)



(defmethod editor-make-new-icon-window ((self classTreeEditor) &optional type)
   (declare (ignore type))
   (if (protected-p (object (panel self)))
     (dialog-message "This package is protected, you can not add classes to it!")
     (let* ((scrollframe (panel self))
            new-object new-frame dial string iconID doc)
       (setf dial (get-classname-dialog))
       (when dial
         (setf string (first dial))
         (setf iconID (third dial))
         (setf doc (second dial))
         (when (string-equal doc "")
           (setf doc "no documentation"))
         (when (and string (not (string-equal "" string)))
           (if (exist-class-p string) 
             (progn
               (dialog-message (string+ "The class " string " is already defined!"))
               ;(om-abort)
               (editor-make-new-icon-window self type))
             (progn
               (setf new-object (eval `(defclass* ,(interne string ) () () (:icon ,(car (list! iconID))) (:documentation ,doc))))
               (setf (create-info new-object) (list (om-get-date) (om-get-date)))
               (when (listp iconID)
                 (icon-for-user-package new-object (second iconID)))
               (setf new-frame (omNG-add-element (object scrollframe) new-object))
               (om-add-subviews scrollframe (make-frame-from-callobj new-frame)))))))))


;--------------
;PANEL
;--------------

(defclass classTreePanel (relationPanel) ()
   (:documentation "This is the class for hierarchical class tree editors.
Boxes refer to classes and connections to inheritance.#enddoc#
#seealso# (OMBoxClass classboxframe) #seealso#"))

;This methods works only in patches
(defmethod clear-ev-once ((self classTreePanel)))
(defmethod make-undefined-box ((self classTreePanel) x) (declare (ignore x)))
(defmethod make-delete-before ((self classTreePanel) dragged cible) (declare (ignore dragged cible)))

(defmethod set-panel-color ((self classTreePanel))
  (om-set-bg-color self *package-color-3*))

;-------------------------

(defmethod delete-general ((self classTreePanel))
  (let ((actives (get-actives self))
          (connections (get-actives-connections self)))
      (loop for item in actives do
            (if (equal (class-name (class-of item)) 'aliasBoxframe)  
                (unless (protected-p (object item))
                  (omg-remove-element self item))
              (unless (protected-p (find-class (reference (object item)) nil))
                (omg-remove-element self item))))
      (mapc #'(lambda (connec)
                (let ((thebox (thebox connec)))
                  (if (protected-p (find-class (get-reference (object thebox))))
                      (om-beep-msg (string+ "The class " (string (get-reference (object thebox))) "  is protected"))
                    (progn
                      (remove-connection thebox (index connec))
                      (unconnect-class (object thebox) (index connec))
                      (redraw-frame thebox)
                      ;(om-invalidate-view (om-view-container thebox))
                      ))))
            connections)
      ))




(defmethod omg-remove-element ((self classTreePanel) frame)
   "We can remove only alias from 'self' if it is the case we redefine classes inheriting from the alias class."
   (let ((subclasses (get-subclasses-list (object frame) (mapcar #'(lambda (fra)
                                                                     (object fra)) (get-subframes self)))))
     (loop for item in subclasses do
           (let ((classbox (car (frames (first item)))))
             (remove-connection classbox  (second item))
             (unconnect-class (first item) (second item))
             (redraw-frame classbox)))
     (omng-remove-element (object self) (object frame))
     (om-remove-subviews self frame)
     ))


(defmethod handle-key-event ((self classTreeEditor) char) 
   (let ((container (panel self)))
     (case char
       (:om-key-delete (delete-general container))
       (:om-key-up (mapc #'(lambda (item) (move-frame-delta item 0)) (get-actives container))
        (make-move-after container (get-actives container)))
       (:om-key-down (mapc #'(lambda (item) (move-frame-delta item 1)) (get-actives container))
        (make-move-after container (get-actives container)))
       (:om-key-left  (mapc #'(lambda (item) (move-frame-delta item 3)) (get-actives container))
        (make-move-after container (get-actives container)))
       (:om-key-right  (mapc #'(lambda (item) (move-frame-delta item 2)) (get-actives container))
        (make-move-after container (get-actives container)))
       (#\D  (om-invalidate-view self))
       (#\d  (mapcar 'show-big-doc (get-actives container)))
       (otherwise (om-beep)))))



(defmethod make-move-after ((self classTreePanel) dragged)
   "This method is called after moving one or more boxes, it redraw the connections involving in the moving operation."
   (call-next-method)
   (mapc #'(lambda (oneobject)
             (set-icon-pos (object oneobject) (om-view-position oneobject)))
         dragged)
   t)


(defmethod om-view-cursor ((self classTreePanel))
   *om-arrow-cursor*)


(defmethod om-get-menu-context ((self classTreePanel))
  (let ((pos (om-mouse-position self)))
    (if (protected-p (object (editor self)))
        (list (om-new-leafmenu "Protected Package" nil nil nil))
      (list
       (om-package-classes2menu *om-package-tree* "Select Superclass" #'(lambda (c) 
                                                                          (let ((object (omNG-make-new-boxalias c pos (string+ (name c) "-alias"))))
                                                                            (when object
                                                                              (let ((new-frame (make-frame-from-callobj object)))
                                                                                (omG-add-element self new-frame)
                                                                                )))))
       (om-new-leafmenu "Import Class"
                        #'(lambda ()
                            (let ((file (om-choose-file-dialog :prompt "Choose a Class file" :types '("OM Classes" "*.omc" "All files" "*.*"))))
                              (when file
                            ; (import-user-class (object self) file)
                                (om-beep-msg "not available yet...")
                                ))))
       ))))