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
  (if (consp params) params (default-edition-params self)))


;;;===========================
;; EDITION-PARAMS 
;;;===========================

;; The default are determined according to the box VALUES !
(defmethod default-edition-params ((self t)) 
  (list (cons 'winsize (or (get-win-ed-size self) (om-make-point 370 280)))
        (cons 'winpos (or (get-win-ed-pos self) (om-make-point 400 20)))))

;; security :(
(defmethod edition-params ((self t))  nil)
(defmethod get-edit-param ((self t) param) 
  (print (format nil "error: objects of type ~A have no edition params!" (type-of self)))
  nil)

(defun get-param (editparams param)
  (cdr (find param editparams :test 'eql :key 'car)))

;(defmethod get-edit-param ((paramlist list) param)
;   (cdr (assoc param paramlist)))
(defmethod get-edit-param ((self object-with-persistant-params) param) 
  (let ((par-pair (find param (edition-params self) :test 'eql :key 'car)))
    (if par-pair ;; ok
        (cdr par-pair)
      ;; not ok : add the defaut to the edition params
      (let ((def-pair (find param (default-edition-params (value self)) :test 'eql :key 'car)))
        (if def-pair 
            (progn (push (copy-list def-pair) (edition-params self))
              (cdr def-pair))
          ;(om-beep-msg (format nil "error: def param ~A not found for object ~A" param (value self)))
          nil
          ))
      )))

(defmethod set-edit-param ((self object-with-persistant-params) param newval) 
  (if (find param (edition-params self) :test 'eql :key 'car)
    (setf (cdr (find param (edition-params self) :test 'eql :key 'car)) newval)
    (push (cons param newval) (edition-params self))))

(defmethod set-edit-param ((self OMBoxEditCall) param newval) 
  (call-next-method)
  (when (editorframe self) (update-editor-controls (editorframe self))))

;;; for box instancfe, it's special
;;; the instance is likely to be exported without the box, 
;;; and embeds the edition params.
(defmethod edition-params ((self OMBoxInstance))
   (edition-params (reference self)))


;;;===========================
;; Utilities...
;;;===========================

(defmethod set-win-position ((self omboxeditcall) newpos) 
  (set-edit-param self 'winpos newpos))

(defmethod set-win-size ((self omboxeditcall) newsize)
  (set-edit-param self 'winsize newsize))

(defmethod copy-value-params ((self t) box)
  (when self `(copy-alist ',(edition-params box))))

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


;;;===========================
;; access from editors...
;;;===========================

(defmethod get-edit-param ((self EditorView) param)
  ;(print (list self param))
  (let ((val (and (ref self) (get-edit-param (ref self) param))))
    (or  val 
        (let ((def-pair (find param (default-edition-params (object self)) :test 'eql :key 'car)))
          (if def-pair 
              (progn (when (ref self) (set-edit-param (ref self) param (cdr def-pair)))
                (cdr def-pair))
            ;(om-beep-msg (format nil "error: def param ~A not found for object ~A" param (object self)))
            nil
            ))
        )))
        

(defmethod set-edit-param ((self EditorView) param val)
  (when (ref self) (set-edit-param (ref self) param val)))

;; not used anywhere else
(defmethod editor-compatible-params-p ((ed1 t) (ed2 t)) nil)






