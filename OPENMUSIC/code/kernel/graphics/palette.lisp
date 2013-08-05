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


(in-package :om)

(defmethod editor-palettes ((self t)) nil)

(defmethod open-win-palettes (pal ed) nil)
(defmethod close-win-palettes (pal ed) nil)

(defmethod editor-open-palettes ((self editorview))
  (mapc #'(lambda (pal) (open-win-palettes pal self)) 
        (editor-palettes self)))

(defmethod editor-close-palettes ((self editorview))
  (mapc #'(lambda (pal) (close-win-palettes pal self)) 
        (editor-palettes self)))


;;; PALETTES MUST APPEAR / DISAPPEAR WITH THEIR ASSOCIATED EDITOR

;;; Editor window appears
(defmethod om-select-window ((self EditorWindow))
  (let ((rep (call-next-method)))
    (editor-open-palettes (editor self))
    rep))

;;; Editor window is closed
(defmethod om-window-close-event ((self EditorWindow)) 
  (editor-close-palettes (editor self))
  (call-next-method))

;;; Editor window is activated/deactivated
(defmethod om-window-activate ((self EditorWindow) &optional (activatep t))
  (call-next-method)
  (if activatep
    (editor-open-palettes (editor self))
    (editor-close-palettes (editor self))
    ))

;;; Editor is programmatically selected / unselected
(defmethod (setf selected-p) :after (selected-p (self EditorView))
  (if selected-p
      (editor-open-palettes self) 
    (editor-close-palettes self)))
