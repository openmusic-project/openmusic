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
;=========================================================================
;;; Music package 
;=========================================================================

(in-package :om)

;;;============================================
;;;;loading pictures


(defvar *insertmodes* nil)
 
(defun init-music-pict ()
   (setf *insertmodes* (om-load-and-store-picture "insertmodes" 'internal)))

(om-add-init-func 'init-music-pict)


(defvar *add-cursor* nil)
(defvar *hand-up* nil)
(defvar *handleft* nil)
(defvar *staff-cursor* nil)
(defvar *move-chord* nil)
(defvar *c-nota* nil)
(defvar *c-chord* nil)
(defvar *c-measure* nil)
(defvar *c-voice* nil)
(defvar *c-group* nil)

(defun create-score-cursors ()
   (setf *add-cursor* (om-make-cursor "add-cursor" (om-make-point 7 0)))
   (setf *hand-up* (om-make-cursor "hand-up" (om-make-point 8 9)))
   (setf *handleft* (om-make-cursor "handleft" (om-make-point 8 9)))
   (setf *staff-cursor* (om-make-cursor "staff-cursor" (om-make-point 8 9)))
   (setf *move-chord* (om-make-cursor "move-chord" (om-make-point 7 0)))
   (setf *c-nota* (om-make-cursor "c-nota" (om-make-point 0 0)))
   (setf *c-chord* (om-make-cursor "c-chord" (om-make-point 0 0)))
   (setf *c-measure* (om-make-cursor "c-measure" (om-make-point 0 0)))
   (setf *c-voice* (om-make-cursor "c-voice" (om-make-point 0 0)))
   (setf *c-group* (om-make-cursor "c-group" (om-make-point 0 0)))
   (setf *mark* (om-make-cursor "marker-cursor" (om-make-point 0 0)))
   )


(om-add-init-func 'create-score-cursors) 
