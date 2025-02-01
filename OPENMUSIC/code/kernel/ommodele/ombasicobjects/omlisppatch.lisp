;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2010 IRCAM-Centre Georges Pompidou, Paris, France.
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
;Lis Patch.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)

(defclass OMLispPatch (OMPatch) 
  ((list-exp :initform nil :initarg :lisp-exp :accessor lisp-exp))
  (:icon 124)
  (:documentation "The a patch written in Lisp")
  (:metaclass omstandardclass))

(defclass OMLispPatchAbs (OMLispPatch ompatchabs) ()
  (:documentation "The a patch written in Lisp")
  (:metaclass omstandardclass))

(defmethod obj-file-type ((self OMLispPatch)) :LISP)
(defmethod obj-file-extension ((self OMLispPatch)) "oml")

(defmethod get-object-insp-name ((self OMLispPatch)) "Lisp Function")


(defmethod lisp-exp-p ((self OMLispPatch)) t)
(defmethod lisp-exp-p ((self t)) nil);ca manque


;;; convert to string (for old patches)
;;; restores line breaks
(defmethod get-lisp-str ((exp string)) (str-with-nl exp))
(defmethod get-lisp-str ((exp t)) (format nil "~A" exp))

;;; converts patch lisp-str to lambda expression
(defmethod get-lisp-exp ((exp string)) (unless (string-equal "" exp) (read-from-string exp)))
(defmethod get-lisp-exp ((exp t)) exp)


(defun omNG-make-new-lisp-patch (name &optional (posi (om-make-point 0 0)))
   "Make an instance of patch."
   (let ((newpatch (make-instance 'OMLispPatch :name name :icon 124)))
     (set-icon-pos newpatch posi)
     newpatch))

(defun compile-lisp-patch-fun (patch)
  (if (get-lisp-exp (lisp-exp patch))
      (eval `(defun ,(intern (string (code patch)) :om)
                    ,.(cdr (get-lisp-exp (lisp-exp patch)))))
    (eval `(defun ,(intern (string (code patch)) :om) () nil))))



(defmethod compile-patch ((self OMLispPatch)) 
  "Generation of lisp code from the graphic boxes."
  (unless (compiled? self)
    (handler-bind 
        ((error #'(lambda (err)
                    (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                    (abort err))))
      (compile-lisp-patch-fun self)
      (setf (compiled? self) t))
    ))


(defmethod load-abstraction-attributes ((self omlisppatch) currentpersistent)
  (call-next-method) ;;; ompatch
  (setf (lisp-exp self) (lisp-exp currentpersistent))
  (when (lisp-exp self) (compile-lisp-patch-fun self)))



(defun om-load-lisp-patch (name version expression)
   (let ((newpatch (omNG-make-new-lisp-patch name)))
     (setf (omversion newpatch) version)
     (setf (lisp-exp newpatch) (get-lisp-str expression))
     newpatch))

(defun om-load-lisp-abspatch (name version expression)
   (let ((newpatch (make-instance 'OMLispPatchAbs :name name :icon 123)))
     (setf (omversion newpatch) version)
     (setf (lisp-exp newpatch) (get-lisp-str expression))
     (compile-lisp-patch-fun newpatch)
     newpatch))

(defmethod get-patch-inputs ((self OMLispPatch))
  (unless (compiled? self)
    (compile-lisp-patch-fun self))
  (let* ((args (arglist (intern (string (code self)) :om)))
         (numins (min-inp-number-from-arglist args)) (i -1))
    (mapcar #'(lambda (name) 
                (make-instance 'omin
                               :indice (incf i)
                               :name (string name))) 
            (subseq args 0 numins))))
     
(defmethod get-patch-outputs ((self OMLispPatch))
  (list (make-instance 'omout
                       :name "lisp function output"
                       :indice 0)))

(defmethod om-save ((self OMLispPatch) &optional (values? nil))
  `(setf *om-current-persistent* 
         (om-load-lisp-patch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp self)))))


(defmethod omNG-copy ((self OMLispPatch))
  `(let ((copy ,(call-next-method)))
     (setf (lisp-exp copy) (lisp-exp ,self))
     (compile-lisp-patch-fun copy)
     copy))


(defmethod om-save ((self OMLispPatchAbs) &optional (values? nil))
   "Generation of code to save 'self'."
   `(om-load-lisp-abspatch ,(name self) ,*om-version* ,(str-without-nl (lisp-exp self))))

(defmethod abs2patch ((self OMLispPatchAbs) name pos)
   "Cons a new instance of 'OMPatch from the abstraction patch 'self'."
  (let ((newabs (omNG-make-new-lisp-patch name pos)))
    (setf (lisp-exp newabs) (lisp-exp self))
    (set-icon-pos newabs (get-icon-pos self)) 
    (setf (doc newabs) (doc self))
    newabs))

(defmethod patch2abs ((self OMLispPatch))
   "Cons a new instance of 'OMPatchAbs from the patch 'self'."
   (let ((newabs (make-instance 'OMLispPatchAbs :name (name self)  :icon 123)))
     (setf (lisp-exp newabs) (lisp-exp self))
     (set-icon-pos newabs (get-icon-pos self))
     (setf (doc newabs) (doc self))
     newabs))

(defmethod OpenEditorframe ((self OMLispPatch))
   "Open the patch editor, this method open too all persistantes objects referenced into the patch."
   (declare (special *om-current-persistent*))
   (load-patch self)
   (or (editorframe self)
       (if (get-lisp-exp (lisp-exp self))
           (edit-existing-lambda-expression self)
         (edit-new-lambda-expression self))))




