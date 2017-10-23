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
; OM TEXT EDIT WINDOW
; & exported functions
;DocFile
;;===========================================================================
;;; uses text editor from lw-lisp-tools


(in-package :oa)

(export '(
          om-text-edit-window
          om-set-text
          om-set-text-buffer
          om-get-text
          om-kill-window-buffer 
          om-lisp-edit-window
          om-get-lisp-expression
          om-texteditor-modified
          ) :om-api)


       
;;; !!! created with om-make-window :
(defclass om-text-edit-window (om-text-editor) 
  ((save-callback :accessor save-callback :initarg :save-callback :initform nil)
   (echoarea :accessor echoarea :initarg :echoarea :initform nil)))


(defmethod make-window-layout ((self om-text-edit-window) &optional color) 
  (make-instance 'simple-layout :description
                 (list (setf (om-lisp::ep self) 
                             (make-instance 'capi::editor-pane 
                                            :font om-lisp::*def-text-edit-font*
                                            :echo-area (echoarea self)
                                            :change-callback 'texteditor-change-callback)))))

(defmethod om-window-class-menubar ((self om-text-edit-window))
  (append (om-lisp::internal-window-class-menubar self)
          (call-next-method)))

(defun texteditor-change-callback (pane point old-length new-length)
  (om-texteditor-modified (capi::top-level-interface pane)))


;;; celui-la soit il a un fichier soit non mais c'est fixe
(defmethod om-lisp::file-operations-enabled ((self om-text-edit-window)) nil)

(defmethod om-lisp::save-operation-enabled ((self om-text-edit-window))
  (or (save-callback self) 
      (and (om-lisp::editor-file self)
           (om-lisp::buffer-modified-p self))))

(defmethod om-lisp::save-text-file ((self om-text-edit-window))
  (if (save-callback self) (funcall (save-callback self) self)
    (call-next-method)))

;;; DOES NOT KILL THE BUFFER 
(defmethod om-destroy-callback ((self om-text-edit-window))
  (setf (om-lisp::window (om-lisp::editor-buffer self)) nil)
  (setf om-lisp::*editor-files-open* (remove self om-lisp::*editor-files-open*))
  (om-window-close-event self))

(defmethod om-lisp::check-close-buffer ((self om-text-edit-window))
  (cond ((and (om-lisp::buffer-modified-p self) (om-lisp::editor-file self))
         (if (call-next-method)
             (om-window-check-before-close self)
         nil))
        (t (om-window-check-before-close self))))

(defmethod om-select-window ((self om-text-edit-window)) 
  (capi::find-interface (type-of self) :name (capi::capi-object-name self)))

(defmethod om-view-window ((self om-text-edit-window)) self)

(defmethod om-set-window-title ((self om-text-edit-window) (title string))
  (setf (capi::interface-title self) title))

;;; met un nouveau buffer
(defmethod om-set-text-buffer ((self om-text-edit-window) newbuffer &optional kill) 
  (let ((rec (om-lisp::buffer (om-lisp::editor-buffer self))))
    (setf (om-lisp::editor-buffer self) newbuffer)
    (om-lisp::init-text-editor self)
     ;;; detruit le buffer precedent
    (when kill (editor::kill-buffer-no-confirm rec))))

;;; garde le même buffer mais change le contenu
(defmethod om-set-text ((self om-text-edit-window) (text string)) 
  (om-buffer-delete (om-lisp::editor-buffer self))
  (om-buffer-insert (om-lisp::editor-buffer self) text))

(defmethod om-get-text ((self om-text-edit-window)) 
 (om-buffer-text (om-lisp::editor-buffer self)))

(defmethod om-kill-window-buffer ((self om-text-edit-window)) 
 (om-kill-buffer (om-lisp::editor-buffer self)))

(defmethod om-texteditor-modified ((self om-text-edit-window)) nil)


(defclass om-lisp-edit-window (om-text-edit-window) ()
  (:default-initargs :lisp-editor? t))

(defmethod om-lisp::lisp-operations-enabled ((self om-lisp-edit-window)) t)

(defmethod om-get-lisp-expression ((self om-lisp-edit-window))
  (let ((textbuffer (om-lisp::buffer (om-lisp::editor-buffer self))))
    (editor::use-buffer textbuffer
      ;; (editor::SKIP-LISP-READER-WHITESPACE (editor:buffers-start textbuffer) textbuffer)
      (editor::current-form-as-read (editor:buffers-start textbuffer))
      )))

;(defmethod test-lisp-form ((self om-text-edit-window))
;  (let ((textbuffer (buffer (editor-buffer self))))
;    (editor::use-buffer textbuffer
 ;     ;(editor::SKIP-LISP-READER-WHITESPACE (editor:buffers-start textbuffer) textbuffer)
 ;;     (editor::current-form-as-read (editor:buffers-start textbuffer))
;      )))


;  (om-buffer-text (editor-buffer self)))
; SKIP-LISP-READER-WHITESPACE
; CURRENT-FORM-AS-READ
; READ-FORM
; READ-FUNCTION
