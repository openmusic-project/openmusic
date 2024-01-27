;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; FLUID package
; Author: Karim Haddad
;==============================
; BASIC CLASS FOR ALL FLUID INTERFACE BOXES
; 
;==============================

(in-package :om)

;============================
;FLUID Dialog item boxes
;============================

;la class pour le dialog-item boxes
(defclass FLDIntbox (OMBoxEditCall) ())

(defmethod omNG-box-value ((self FLDIntbox) &optional (numout 0))
   "Eval a factory."
   (handler-bind ((error #'(lambda (c)
                             (when *msg-error-label-on*
                               (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " " 
                                                        (om-report-condition c))
                                               :size (om-make-point 300 200))
                               (om-abort)))))

     (om-without-interrupts  
       (om-dialog-item-action (value self))
     (cond
        ((or (equal (allow-lock self) "l")  (equal (allow-lock self) "o") 
             (and (equal (allow-lock self) "x") (value self))
             (and (equal (allow-lock self) "&") (ev-once-p self)))
             (rep-editor (value self) numout))
        (t (let* ((args  (mapcar #'(lambda (input) (omNG-box-value input)) (inputs self)))
                  rep)
             (setf rep (set-dialog-item-params (value self) self args))
             (if (null rep)
                  (progn
                    (om-beep-msg (string+ "I can not construct a " (string (type-of (value self))) " with these parameters"))
                    (om-abort))
                (progn
                  (setf (value self) rep)
                  (rep-editor (value self) numout)
                  )))))
     ;(om-dialog-item-action (value self)) ;si activee outputs retournent plus que des nil 
     )
     ))

(defmethod gen-code-call ((self FLDIntbox) &optional args)
   `(apply 'set-dialog-item-params (list ,(value self) ,self (list ,.(decode self)))))


(defmethod make-outputs-from-names ((self FLDIntbox) value module) 
   "The outputs of these boxes depent from the initarg slots of the class reference."
   (let ((numouts (numouts self))
         (nameouts (get-outs-name value)))
     (loop for i from 0 to (- numouts 1) do
           (let ((thenewout (om-make-view (get-out-class self)
                              :position (om-make-point (- (* (+ i 1) (round (w module) (+ numouts 1))) 4) 
                                                         (- (h module) 9))
                              :size (om-make-point 8 8)
                              :help-spec (nth i nameouts)
                              :index i)))
             (push thenewout (outframes module))
             (om-add-subviews module thenewout)))))

(defmethod make-frame-from-callobj ((self FLDIntbox))
   "Make a simple frame for the editor factory 'self'."
   (let ((name (string-downcase (name self)))
         (defsize (get-boxsize self))
         (numouts (numouts self))
         (numins (length (inputs self)))
         (index 0) 
         (module (om-make-view (get-frame-class self)
                               :name (name (value self))
                               :position (frame-position self)
                               :object self))
         title)
    
     (unless (frame-size self) 
       (setf (frame-size self) (om-make-point 
                                (apply #'max (list (om-point-h defsize) (* 8 numouts) (* 8 numins))) 
                                (om-point-v defsize)))
       )

     (setf (inputframes module) (mapcar #'(lambda (input)
                                            
                                            (setf index (+ index 1))
                                            (om-make-view (get-input-class-frame self)
                                                            :object input
                                                            :help-spec (string+ "<" (string-downcase (name input))
                                                                                "> " (doc-string input))
                                                            :size (om-make-point 8 8)
                                                            :position (om-make-point 
                                                                       (- (* index (round (om-point-h (frame-size self)) (+ numins 1))) 4) 
                                                                       1)
                                                            ))
                                        (inputs self)))
      (setf title (om-make-dialog-item 'om-static-text (om-make-point 5 8) (om-make-point 90 24) (name module)
                           :font *controls-font*)) 
     (loop for input-f in (inputframes module) do (om-add-subviews module input-f))

     (make-outputs-from-names self (value self) module)
     
     (setf (iconview module) (value self))                
     (om-add-subviews module (iconview module) title)

     (setf (frames self) (list module))
     (setf (name module) name)
     (add-box-resize module)
     
     (om-set-view-size module (frame-size self))
     (update-di-size (value self) module)
     
     (when (allow-lock self)
       (add-lock-button module (allow-lock self)))
     
     module))

(defmethod get-frame-class ((self FLDIntbox)) 'FIEditorframe)


;=======================
;the frame
;=======================

(defclass FIEditorframe (omboxframe OMSimpleFrame om-transparent-view om-view-drag ) ())


(defmethod show-fun-code ((self FIEditorframe))
  (edit-definition (class-name (reference (object self)))))

(defmethod show-big-doc ((self FIEditorframe))
  (om-show-reference-doc (class-name (reference (object self)))))

(defmethod allow-new-size ((self FIEditorframe) new-pos) 
   (om-make-point (max 20 (om-point-h new-pos )) (max 20 (om-point-v new-pos))))

(defmethod add-lock-button ((self FIEditorframe) &optional (mode "x"))
   "Add a lock button, if the box referenced by 'self' allow it."
   (when (allow-lock-button (object self))
     (setf (lock-button self) (make-lock-button self mode))
     (om-set-view-position (lock-button self) (om-make-point 0 8))
     (om-add-subviews self (lock-button self))
     (om-invalidate-view self)
     (setf (allow-lock (object self)) mode)))

(defmethod remove-lock-button ((self FIEditorframe))
   "Do not set value to nil."
   (om-remove-subviews self (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil))

(defmethod centre-icon ((self FIEditorframe))
   (om-set-view-size 
    (iconview self) 
    (om-subtract-points (om-view-size self) (om-make-point 0 17))))

(defmethod make-drag-region ((self FIEditorframe) region x0 y0 view)
  (declare (ignore view))
  (let* ((x (- (x self) x0))
         (y (- (y self) y0)))
    (om-set-rect-region region x y  (+ x (w self)) (- (+ y (h self)) 16)))
   region)

(defmethod omG-select ((self FIEditorframe))
   (when (not (active-mode self))
     (setf (active-mode self) t)
     (om-invalidate-view self)
     ))

(defmethod omG-unselect ((self FIEditorframe))
   (when (active-mode self)
     (setf (active-mode self) nil)
     (om-invalidate-view self)
     ))


(defmethod om-draw-contents ((self FIEditorframe))
  (call-next-method)
  (when (active-mode self)
    (om-with-focused-view self
      (om-with-fg-color self *om-gray-color*
      (om-draw-rect 1 8 (- (w self) 3) (- (h self) 17) :pensize 2)))))

(defmethod change-boxframe-size ((view FIEditorframe) new-size)
   (when (setf new-size (allow-new-size view new-size))
     (om-set-view-size view new-size)
     (make-move-after (om-view-container view) (list view))
     (update-di-size (value (object view)) view)
     (om-invalidate-view view)
     (om-invalidate-view (om-view-container view))
     ))

(defmethod reinit-size ((self FIEditorframe)) 
   (setf (frame-size (object self)) (get-boxsize (object self)))
   (change-boxframe-size self (frame-size (object self)))
   (update-di-size (value (object self)) self)
   (om-invalidate-view self))

(defmethod allow-new-size ((self FIEditorframe) new-pos) 
   (om-make-point (max 30 (om-point-h new-pos )) (max 40 (om-point-v new-pos ))))

(defmethod add-subview-extra ((self FIEditorframe))
  (update-di-size (value (object self)) self))

(defmethod om-view-doubleclick-handler ((self FIEditorframe) pos) nil)

;==================
; The object
;==================

(defclass! fluid-i-box (select-object) 
     ((di-data :accessor di-data :initform nil)))

(defmethod get-type-of-ed-box ((self fluid-i-box))  'FLDIntbox)

(defmethod default-obj-box-size ((self fluid-i-box)) (om-make-point 130 74))

(defmethod get-slot-in-out-names ((self fluid-i-box))
   (values '("text") 
           '("untitled")
           '("dialog-item text (string)")
           '(nil)))

(defmethod set-dialog-item-params ((self fluid-i-box)  box args)
  (om-set-dialog-item-text self (format nil "~D" (car args)))
  self)

(defmethod update-di-size ((self fluid-i-box) container)
  (om-set-view-position self (om-make-point 10 18))
  (om-set-view-size self (om-make-point (- (om-width container) 20) (max 20 (- (om-height container) 36)))))

(defmethod omng-copy ((self fluid-i-box))
  (let ((newitem (eval (omng-save self))))
    (om-set-dialog-item-action-function newitem (om-dialog-item-action-function  self))
    newitem))

(defmethod spec-obj-icon-size ((self fluid-i-box)) '(nil nil))

