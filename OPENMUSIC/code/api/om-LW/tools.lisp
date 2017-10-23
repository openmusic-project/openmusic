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
; Authors: Jean Bresson, Carlos Agon
;=========================================================================

;;===========================================================================
;DocFile
; Misc. TOOLS (buffers, pointers, print, etc...)
;DocFile
;;===========================================================================

(in-package :om-api)
;;;========
;;; export :
;;;========
(export '(
                quoted-form-p
                lambda-expression-p
                om-report-condition
                om-read-list-from-string
                om-text-to-lines
          
                memq
                while
                
                om-copy-command
                om-paste-command
                om-cut-command

                om-random-value
                
                om-show-documentation
                
                om-page-setup
                om-print-window
                
                om-closure-function
                ) :om-api)

;----------------------
; LISP TOOLS FEATURES
;----------------------

(defun memq (item list)
       (member item list :test #'eq))

(defmacro %car (x)
  `(car (the cons ,x)))

(defmacro %cdr (x)
  `(cdr (the cons ,x)))

(defmacro %cadr (x)
  `(cadr (the cons ,x)))

(defun quoted-form-p (form)
  (and (consp form)
        (eq (%car form) 'quote)
        (consp (%cdr form))
        (null (%cdr (%cdr form)))))

(defun lambda-expression-p (form)
   (and (consp form)
       (eq (%car form) 'lambda)
       (consp (%cdr form))
       (listp (%cadr form))))


(defmacro while (test &body body)
  (let ((testlab (gensym))
        (toplab (gensym)))
    `(tagbody
       (go ,testlab)
      ,toplab
      (progn ,@body)
      ,testlab
      (when ,test (go ,toplab)))))



(defmethod om-report-condition ((c condition))
  (format nil "~A" c))

;(defmethod special-form-p (s)
;  (lispworks::special-form-p s))

(defun om-read-list-from-string (string &optional (pos 0))
  (multiple-value-bind (val pos) (ignore-errors 
				  (read-from-string string nil :eof :start pos))
    (if (eql val :eof)
	nil
	(cons val (om-read-list-from-string string pos)))))

(defun om-text-to-lines (text)
  (let ((p2 (position #\Newline text)) 
        (rep nil))
        (loop while p2 do
              (push (subseq text 0 p2) rep)
              (setf text (subseq text (+ p2 1)))
              (setf p2 (position #\Newline text)))
        (push text rep)
        (reverse rep)))

;=================
; copy/paste 

;;; appel au clipboard du systeme
(defmethod om-copy-command ((self t))
  (print "Nothing to copy to clipboard"))

(defmethod om-paste-command ((self t))
  (print "Nothing to paste from clipboard"))

(defmethod om-cut-command ((self t))
   (print "Nothing to cut and copy to clipboard"))



;=================
;safe random
(defun om-random-value (num)
  (if (= num 0) 0
  (if (< num 0)
    (- (random (- num)))
    (random num))))


;======for situation

(defun om-closure-function (fun) t)


;==================

(in-package :cl-user)

(defun set-nthcdr (index list new-value)
  "If INDEX is 0, just return NEW-VALUE."
  (if (not (zerop index))
    (rplacd (nthcdr (1- index) list)
            new-value))
  new-value)

(let ((lispworks::*HANDLE-WARN-ON-REDEFINITION* nil))
  (defsetf nthcdr set-nthcdr)
  )

