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
; Lisp Patch editor window
;DocFile

(in-package :om)


(omg-defclass patch-lambda-exp-window (om-lisp-edit-window) 
  ((patchref :initform nil :initarg :patchref :accessor patchref))
   (:documentation "Special fred editor, used when the patch is defined by a lambda expression.#enddoc#
#seealso# (OMPatch) #seealso#
#patchref# The patch being edited. #patchref#")
   (:default-initargs :save-callback 'save-lisp-patch :echoarea t))

(defmethod omG-rename ((self patch-lambda-exp-window) new-name)
   (om-set-window-title self (string+ "Lisp Function - " new-name)))



;;; ATTENTION : MARCHE PAS
(defmethod om-texteditor-modified ((self patch-lambda-exp-window))
  (setf (compiled? (patchref self)) nil)
  (setf (saved? (patchref self)) nil)
  (omG-rename self (string+ "^" (name (patchref self))))
  (call-next-method))

(defmethod save-lisp-patch ((self patch-lambda-exp-window))
  (if (mypathname (patchref self))
    (progn 
      (compile-before-close self)
      (if *patch-abort-definition*
          (om-message-dialog "Error in lambda expression.~%Lambda expressions are of the form (lambda <param-list> <body>).~%Lisp patch can not be saved.")
        (progn
          (omng-save (patchref self) nil) 
          (setf (saved? (patchref self)) t)
          (omG-rename self (name (patchref self)))
          )))
    (om-beep-msg "This Lisp Patch is not a persistant object. Drag the box to the workspace to save it as a patch file.")))


(defvar *patch-abort-definition* nil)

;;; mettre ici le test !!!
(defmethod om-window-check-before-close ((self patch-lambda-exp-window))
  (compile-before-close self)
  (if *patch-abort-definition*
      (let ((rep (om-y-or-n-dialog (format nil *patch-abort-definition*))))
        (setf *patch-abort-definition* nil)
        rep)
    t))

(defmethod om-window-close-event ((self patch-lambda-exp-window))
  (handler-bind ((error #'(lambda (c) 
                            (om-message-dialog (format nil "Patch Definition Error: ~S. ~%No modification will be make to the patch." (om-report-condition c)))
                            (om-kill-window-buffer self)
                            (when (patchref self)
                              (setf (editorframe (patchref self)) nil))
                            (setf *patch-abort-definition* nil)
                            (om-abort))))
    (loop for item in (attached-objs (patchref self)) do
          (update-from-reference item))
    (setf (compiled? (patchref self)) t)
    (setf (editorframe (patchref self)) nil)
    (setf *patch-abort-definition* nil)
    (om-kill-window-buffer self)
    (call-next-method)
    ))

(defmethod compile-before-close ((self patch-lambda-exp-window))
  (handler-bind ((error #'(lambda (c) 
                            (declare (ignore c))
                            (setf *patch-definition-aborted* 
                                  (string+ "Error in function definition: " 
                                           (om-report-condition c) 
                                           ".~%~%Close editor anyway?~%(No modification will be made to the patch)."))
                            )))
  (let* ((expression (om-get-lisp-expression self)))
    (unless (lambda-expression-p expression)
      (setf *patch-abort-definition* "Error in lambda expression.~%Lambda expressions are of the form (lambda <param-list> <body>).~%~%Close editor anyway?~%(No modification will be made to the patch)."))
    (unless *patch-abort-definition*
      ;(setf (lisp-exp-p (patchref self)) expression)
      (setf (lisp-exp (patchref self)) (om-get-text self))
      (eval `(defun ,(intern (string (code (patchref self))) :om)
                    ,.(cdr (get-lisp-exp (lisp-exp (patchref self))))))
      ))))


(defmethod compile-without-close ((self patch-lambda-exp-window))
  (let* ((expression (om-get-lisp-expression self)))
    (unless (lambda-expression-p expression)
      (om-message-dialog (string+ "Error! The expression in the Lisp patch" (name (patchref self)) " is not a valid lambda expression. 
Lambda expression are of the form (lambda <param-list> <body>)"))
      (om-abort))
    (unless (equal (get-lisp-exp (lisp-exp (patchref self))) expression)
      ;;;(setf (lisp-exp-p (patchref self)) expression)
      (setf (lisp-exp (patchref self)) (om-get-text self))
      (compile-lisp-patch-fun (patchref self))
      (loop for item in (attached-objs (patchref self)) do
            (update-from-reference item)))
    (setf (compiled? (patchref self)) t)))



(defmethod edit-new-lambda-expression ((self ompatch))
   (setf (editorframe self)
      (om-make-window 'patch-lambda-exp-window
                      :size (om-make-point 300 200)
                      :patchref self
                      :window-title (string+ "Lisp Patch - " (name self))
                      :window-show nil))
   ;(om-set-text (editorframe self) (format nil "~S" (lambda () (om-beep))))
   (om-set-text (editorframe self) 
                (format nil 
                        ";;; Edit a valid LAMBDA EXPRESSION for ~s~%;;; e.g. (lambda (arg1 arg2 ...) ( ... ))~%~%(lambda () (om-beep))" 
                        (name self)))
   (om-add-menu-to-win (editorframe self))
   (setf (saved? self) nil)
   (editorframe self)
   )

(defmethod edit-existing-lambda-expression ((self ompatch))
   (let ((editor (om-make-window 'patch-lambda-exp-window
                                 :patchref self
                                 :size (om-make-point 300 200)
                                 :window-title (string+ "Lisp Patch - " (name self))
                                 :window-show nil)))
     ;(om-set-text editor (format nil "~S" (lisp-exp-p self)))
     (om-set-text editor (lisp-exp self))
     (om-add-menu-to-win editor)
     editor))



