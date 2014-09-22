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
;Some boxes like omloop have a patch as editor.
;This file defines the abstract class for these boxes.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;========BOXES WITH EDITOR===================
(defclass patchForBox (OMPatch)
   ((box :initform nil :accessor box))
   (:documentation "Some boxes as omloop have a patch that define the meaning of the box.
This class is the abstract class for these patches.  #enddoc#
#seealso# (OMPatch OMLoop) #seealso#
#box# This slot keeps box defined by the patch. #box#"))

(defmethod OpenEditorframe ((self patchForBox))  nil)


;EDITOR
(omg-defclass boxpatchEditor (patchEditor) ())

(defmethod get-editor-panel-class ((self boxpatchEditor))  'boxpatchPanel)

(defmethod extra-menu-info ((self boxpatchEditor))
  (disable-this-menu-items *window-menu-file*  '("save")))


;PANEL
(defclass boxpatchPanel (patchPanel) ()
   (:documentation "This is the class for editors of patchforbox instances.  #enddoc#
#seealso# (patchForBox box-with-patch looppanel) #seealso#"))

(defmethod compile-win ((self boxpatchPanel))
   (compile-patch (object self))
   (setf (compiled? (object self)) t)
   (om-close-window (window self)))

;-------------------------------------------
(defclass box-with-patch (OMBoxcall) 
   ((patch :initform nil :accessor patch))
   (:documentation "Some boxes as omloop have a patch that define the meaning of the box.
This class is the abstract class for these boxes.  #enddoc#
#seealso# (OMBoxcall omLoop-Box box-with-patch-frame) #seealso#
#patch# This slot keeps the patch associated to the box. #patch#"))

(defmethod get-frame-class ((self box-with-patch)) 'box-with-patch-frame)

(defmethod allow-rename ((self box-with-patch)) t)

(defmethod get-patch-editor-class ((self box-with-patch))
   "Return the class of the Patch associated to 'self'."
   'patchForBox)

(defmethod do-add-one-input-extra ((self box-with-patch)) 
   "When you add one input to the box you must add one input to the patch too."
   (let* ((container (editorframe (patch self)))
          (i (- (length (inputs self)) 1))
          (input (make-new-patch-input (string+ "input" (format () "~D" i))
                                       i (om-make-point (+ 5 (* i 30)) 40))))
     (if container
       (omG-add-element container (make-frame-from-callobj input))
       (omNG-add-element (patch self) input))
     t))

(defmethod do-delete-one-input-extra ((self box-with-patch))
   "When you remove one input from the box you must remove one input from the patch too."
   (let* ((container (editorframe (patch self)))
          (boxes (boxes (patch self)))
          (in-boxes (find-class-boxes boxes 'OMin)))
     (setf in-boxes (sort in-boxes '< :key 'indice))
     (omng-remove-element (patch self) (car (last in-boxes)))
     (when container
       (om-remove-subviews container (car (frames (car (last in-boxes)))))
       (close-frame (car (frames (car (last in-boxes))))))
     t))

(defmethod omNG-copy ((self box-with-patch))
  `(let* ((copy ,(omNG-make-new-boxcall (fdefinition (reference self))
                                        (frame-position self)
                                        (name self))))
     (setf copy (update-boxes ,self copy))
     (setf (patch copy) (eval ,(omNG-copy (patch self))))
     (setf (box (patch copy)) copy)
     (setf (code (patch copy)) (list (gensym)))
     (setf (pictu-list (patch copy)) ',(loop for item in (pictu-list (patch self))
                                        collect (let ((newpict (make-instance 'patch-picture)))
                                                  ;;;(setf (thepict newpict) (om-copy-pict-handler (thepict item)))
                                                       (setf (thepict newpict) (thepict item))
                                                       (setf (name newpict) (name item))
                                                       (setf (pict-pos newpict) (pict-pos item))
                                                       newpict)))
     (compile-patch (patch copy))
     copy))

(defmethod initialize-instance ((self box-with-patch) &key controls)
   (declare (ignore controls))
   (call-next-method)
   (let ((new-patch (eval `(make-instance ',(get-patch-editor-class self) :name ,(name self)
                                          :icon 183))))
     (setf (name new-patch) (name self))
     (setf (box new-patch) self)
     (setf (code new-patch) (list (gensym)))
     (setf (patch self) new-patch)))

(defmethod special-value ((self  box-with-patch) &optional (num-out 0))
   (declare (ignore num-out))
   (let ((in-list (mapcar #'(lambda (thein) (omNG-box-value thein)) (inputs self))))
     (unless (compiled? (patch self)) 
       (compile-patch (patch self)))
     (setf (compiled? (patch self)) t)
     (eval `(,(reference self) ,.in-list ,(code (patch self))))))


(defmethod omNG-save ((self box-with-patch) &optional (values? nil))
   (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self)))
          (value (when values? (omNG-save (value self) values?)))
          (boxes (boxes (patch self))) pictlist)
     (setf pictlist (omng-save (pictu-list (patch self))))
     `(om-load-boxwithed1 'box-with-win ,(name self) ',(reference self) ',inputs ,(om-save-point (frame-position self)) 
                         ,(om-save-point (frame-size self)) ,value ,(allow-lock self) 
                         ,(omNG-save boxes) ',(mk-connection-list boxes) ,(numouts self) ,(frame-name self) ,pictlist)))



(defmethod om-load-boxwithed1 ((class t) name reference inputs position size value lock boxes conec numouts 
                               &optional fname pictlist)
  (let ((newbox (omNG-make-new-boxcall (mk-object-refer class reference) (om-correct-point position) name)))
    (setf (name (patch newbox)) (string-downcase (or fname name)))   ;;; new
    (setf (frame-size newbox) size)
    (setf (inputs newbox) (mapcar #'(lambda (input) (eval input)) inputs))
    (set-box-to-inputs (inputs newbox) newbox)
    (setf (value newbox) (if (listp value) value (eval value)))
    (setf (allow-lock newbox) lock)
    (setf (frame-name newbox) (string-downcase (or fname name)))
    (setf (boxes (patch newbox)) nil)
    (mapc #'(lambda (box) (omNG-add-element (patch newbox) (eval box)))  boxes)
    (setf (boxes (patch newbox)) (reverse (boxes (patch newbox))))
    (setf (connec (patch newbox)) conec)
    (remk-connections (boxes (patch newbox)) (loop for i in conec collect (load-connection i)))
    (setf (numouts newbox) numouts)
    (setf (pictu-list (patch newbox)) pictlist)
    (push (patch newbox) *loaading-stack*)
    newbox))


(omg-defclass box-with-patch-frame (boxframe) ()
   (:documentation "Abstract class for simple frames of the box-with-patch instances.#enddoc#
#seealso# (box-with-patch) #seealso#"))

(defmethod close-frame ((self box-with-patch-frame))
   "When you remove the frame you must close the patch's editor if it is open."
   (when (EditorFrame (patch (object self)))
     (om-close-window (window (EditorFrame (patch (object self))))))
   (setf (frames (object self)) nil))



