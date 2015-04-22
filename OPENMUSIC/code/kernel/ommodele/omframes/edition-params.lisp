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
;EDITION PARAMS: parameters attached to the boxes and editors
;DocFile

(in-package :om)


;;; compat only
(defclass ed-par-array () ())

;;; return new list of params corrected according to the type of object
;;; (for version changes and compatibility)
(defmethod corrige-edition-params ((self t) params) 
  (if (consp params) params
    (default-edition-params self)))

;; parametre par defaut pour differentes classes d'objets
(defmethod default-edition-params ((self t)) 
  (pairlis '(winsize winpos)
           (list (or (get-win-ed-size self) (om-make-point 370 280))
                 (or (get-win-ed-pos self) (om-make-point 400 20)))))

(defmethod edition-params ((self t))  nil)

(defmethod get-edit-param ((self t) param) nil)

(defmethod editor-default-edit-params ((self EditorView)) 
  (default-edition-params (object self)))

(defmethod editor-edit-params ((self EditorView))
  (cond ((or (ominstance-p (ref self)) (is-boxpatch-p (ref self)))
         (print "a")
          ;;; cas principal: edition-params de l'objet de reference
          (or (edition-params (ref self)) (setf (edition-params (ref self)) (editor-default-edit-params self))))
         ((and (EditorView-p (ref self)) (editor-compatible-params-p self (ref self)))
          ;;; editeur interne : edition params de l'editeur principal
          (print "b") (editor-edit-params (ref self)))
         (t (editor-default-edit-params self))))

(defmethod editor-compatible-params-p ((ed1 t) (ed2 t)) nil)

;; accessors
(defmethod get-edit-param ((self EditorView) param)
   ;;; renvoie sur la liste d'assoc
  (get-edit-param (editor-edit-params self) param))
   
(defmethod get-edit-param ((paramlist list) param)
   (cdr (assoc param paramlist)))

(defmethod set-edit-param ((self EditorView) param val)
  ;(print (list self param val))
  (let ((paramlist (print (editor-edit-params self))))
    (if (and paramlist (assoc param paramlist))
        (rplacd (assoc param paramlist) val)
      (if paramlist (push (cons param val) paramlist)))
    ))
  ;    (if (edition-params (ref self))
  ;        (push (cons param val) (edition-params (ref self)))
  ;      ))))

(defmethod get-edit-param ((self OMBoxcall) param) 
  (cdr (find param (edition-params self) :test 'eql :key 'car)))

(defmethod set-edit-param ((self OMBoxcall) param newval) 
  (if (find param (edition-params self) :test 'eql :key 'car)
    (setf (cdr (find param (edition-params self) :test 'eql :key 'car)) newval)
    (push (cons param newval) (edition-params self)))
  )


(defmethod set-edit-param ((self OMBoxEditCall) param newval) 
  (call-next-method)
  (when (editorframe self)
    (update-editor-controls (editorframe self))
    ))


(defmethod set-win-position ((self omboxeditcall) newpos) 
  (set-edit-param self 'winpos newpos))

(defmethod set-win-size ((self omboxeditcall) newsize)
  (set-edit-param self 'winsize newsize))

(defmethod copy-value-params ((self t) box)
  (when self
   `(copy-alist ',(edition-params box))))

(defmethod save-value-params ((self t) params) 
  (save-alist (corrige-edition-params self params)))



;;; saved form (eventually corrects) of the edition params of the box
(defmethod save-edition-params ((self OMBoxEditCall))
   (save-value-params (value self) (edition-params self)))

(defmethod set-edition-params ((self t) box) 
  (declare (ignore box))  
  (setf (edition-params box) (default-edition-params self)))

(defmethod save-edition-params ((self temporalbox))
   (save-value-params (car (value self)) (edition-params self)))

(defmethod copy-edition-params ((self temporalBox))
   (copy-value-params (car (value self)) self))

(defmethod edition-params ((self OMBoxInstance))
   (edition-params (reference self)))
