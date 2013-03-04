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
;Dialog to define get and set slots Methods.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

; (open-dialog-slots (find-class 'myclass))
;;; non marcha piu (LWM)

;;; utilise dans method-def
(defun getsetfunname (str)
   (let ((index (search (format nil "-") str)))
     (if index (read-from-string (subseq str 4 index)) 
         (error "bad set-get slot name"))))

(defun open-get-slot (class slot-name)
   (let ((fun-name (internp (string+ "&get" (string slot-name) "-" (string (class-name class))) 
                            (symbol-package (class-name class))))
         thegetmethod)
     (unless (fboundp fun-name)
       (let (newmethod input1 out1 set1)
         (setf newmethod (eval `(defmethod* ,fun-name ((self ,(class-name class))) nil)))
         (setf input1 (make-new-typed-input "self" (class-name class) 0 (om-make-point 15 15)))
         (setf out1 (make-new-output "out" 0 (om-make-point 15 250)))
         (setf set1 (omNG-make-new-boxcall (fdefinition 'get-slot) (om-make-point 30 150) "get"))
         (setf (class-method-p newmethod) 'get)
         (push newmethod (internal-met class))
         (setf (graph-fun newmethod) nil)
         (omng-add-element newmethod input1)
         (omng-add-element newmethod out1)
         (omng-add-element newmethod set1)
         (omNG-connect input1 0  set1 0 nil)
         (omNG-connect set1 0  out1 0 nil)
         (setf (value (nth 1 (inputs set1))) slot-name)))
     (setf thegetmethod (car (generic-function-methods  (fdefinition fun-name) )))
     (when thegetmethod
       (openObjecteditor thegetmethod))
     ))

(defun open-set-slot (class slot-name type)
   (let ((fun-name (internp (string+ "@set" (string slot-name) "-" (string (class-name class))) 
                            (symbol-package (class-name class))))
         thesetmethod)
     (unless (fboundp fun-name)
       (let (newmethod input1 input2 out1 set1)
         (setf newmethod (eval `(defmethod* ,fun-name ((self ,(class-name class)) (val ,type)) nil)))
         (setf input1 (make-new-typed-input "self" (class-name class) 0 (om-make-point 15 15)))
         (setf input2 (make-new-typed-input "val" type 1 (om-make-point 150 15)))
         (setf out1 (make-new-output "out" 0 (om-make-point 15 250)))
         (setf set1 (omNG-make-new-boxcall (fdefinition 'set-slot) (om-make-point 30 150) "set"))
         (setf (graph-fun newmethod) nil)
         (omng-add-element newmethod input1)
         (omng-add-element newmethod input2)
         (setf (value (nth 1 (inputs set1))) slot-name)
         (omng-add-element newmethod out1)
         (omng-add-element newmethod set1)
         (omNG-connect input1 0  set1 0 nil)
         (omNG-connect input2 0  set1 2 nil)
         (omNG-connect set1 0  out1 0 nil)
         (setf (class-method-p newmethod) 'set)
         (push newmethod (internal-met class))))
     (setf thesetmethod (car (generic-function-methods  (fdefinition fun-name))))
     (when thesetmethod
       (openObjecteditor thesetmethod))
     ))







                    
                   
            
  