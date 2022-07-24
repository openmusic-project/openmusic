;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Jean Bresson, Carlos Agon, Karim Haddad
;=========================================================================

;;===========================================================================
;DocFile
; SPECIAL OPENMUSIC FUNCTIONS
;DocFile
;;===========================================================================


(in-package :om-api)
(export '(om-inspect) :om-api)

;=======================
; INSPECTOR
;=======================

(defclass inspect-dialog (om-window)
  ((list1 :initarg :list1 :initform nil :accessor list1)
   (list2 :initarg :list2 :initform nil :accessor list2)
   (selected :initarg :selected :initform nil :accessor selected)))


(defun decompose-object (obj)
  (cond ((consp obj) 
         (if (listp (cdr obj))
             (loop for item in obj
                   for i = 0 then (+ i 1)
                   collect (list i item))
           (list (list "car" (car obj)) (list "cdr" (cdr obj)))))
        ((arrayp obj)
         (loop for i = 0 then (+ i 1) while (< i (length obj))
               collect (list i (aref obj i))))
        ((structurep obj)
         (loop for sl in (structure::structure-class-slot-names (find-class (type-of obj) nil))
               collect (list sl (slot-value obj sl))))
        ((and (find-class (type-of obj) nil) (hcl::class-slots (find-class (type-of obj))))
         (loop for slot in (hcl::class-slots (find-class (type-of obj))) 
               collect (list (hcl::slot-definition-name slot) 
                             (if (hcl::slot-boundp obj (hcl::slot-definition-name slot))
                                 (slot-value obj (hcl::slot-definition-name slot))
                               :unbound))))
        ((atom obj) (list obj))
        (t nil)))

(defun set-inspector-panel (item-list object)
  (setf (capi::collection-items item-list)
        (loop for elt in (decompose-object object) 
              collect (if (consp elt) 
                          (concatenate 'string 
                                       (string-upcase (format nil "~A" (car elt)))
                                       (format nil ":   ~A"  (cadr elt)))
                        (format nil "~A"  elt)))))

(defmethod om-resize-callback ((self inspect-dialog) x y w h)
  (call-next-method)
  (let ((diff (abs (- w 500))))
    (when (list1 self)
      (om-set-view-size (list1 self) (om-make-point (+ (round (/ diff 2)) 230) (- h 40))))
    (when (list2 self)
      (om-set-view-size (list2 self) (om-make-point (+ (round (/ diff 2)) 230) (- h 40)))
      (om-set-view-position (list2 self) (om-make-point (abs (- (- w 250) (round (/ diff 2))))  10))
      )))


(defun om-inspect (object &optional position)
  (let* ((pos (or position (om-make-point 200 200)))
         (win (om-make-window 'inspect-dialog 
                              :size (om-make-point 450 300)
                              :position pos
                              :window-title (format nil "Inspecting ~A" object)
                              :resizable t
                              :menu-items (list (om-make-menu
                                                 "File"
                                                 (list (om-new-leafmenu
                                                        "Close all inspector windows"
                                                        #'(lambda () (mapc 'om-close-window (om-get-all-windows 'inspect-dialog)))
                                                        "w"
                                                        ))))
                              )))
    (setf (list1 win) (om-make-dialog-item 'om-single-item-list
                                           (om-make-point 10 10)
                                           (om-make-point 200 260)
                                           ""
                                           :focus nil
                                           :resizable t
                                           :scrollbars :v
                                           :callback-type '(:collection)
                                           :test-function 'string-equal
                                           :range nil
                                           :action-callback #'(lambda (list)
                                                                (let ((sel (cadr (nth (capi::choice-selection list) 
                                                                                      (decompose-object object)))))
                                                                  (when (consp (car (decompose-object sel)))
                                                                    (om-inspect sel (om-add-points (om-view-position win) (om-make-point 20 20))))
                                                                  ))
                                           :selection-callback #'(lambda (list)
                                                                   (setf (selected win)
                                                                         (cadr (nth (capi::choice-selection list) 
                                                                                    (decompose-object object))))
                                                                   (set-inspector-panel (list2 win) (selected win)))
                                           ))

    (setf (list2 win) (om-make-dialog-item 'om-single-item-list
                                           (om-make-point 230 10)
                                           (om-make-point 200 260)
                                           ""
                                           :focus nil
                                           :resizable t
                                           :scrollbars :v
                                           :callback-type '(:collection)
                                           :test-function 'string-equal
                                           :range nil
                                           :action-callback #'(lambda (list)
                                                                (when (consp (nth (capi::choice-selection list) 
                                                                                  (decompose-object (selected win))))
                                                                  (let ((new (cadr (nth (capi::choice-selection list) 
                                                                                        (decompose-object (selected win))))))
                                                                    (when (consp (car (decompose-object new)))
                                                                      (om-inspect new (om-add-points (om-view-position win) (om-make-point 20 20)))))
                                                                  ))
                         
                                           ))
    
    (set-inspector-panel (list1 win) object)
    (om-set-selected-item-index (list1 win) 0)
    (setf (selected win) (cadr (car (decompose-object object))))
    (set-inspector-panel (list2 win) (selected win))
    (om-add-subviews win (list1 win) (list2 win))
    (setf (list1 win) (list1 win) (list2 win) (list2 win))
    (om-select-window win)))

; (om-inspect (make-instance 'om::chord-seq))
