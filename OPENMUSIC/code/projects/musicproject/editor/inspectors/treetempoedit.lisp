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
;;; authors  G. Assayag, C. Agon, K. Haddad
;=========================================================================
;
;============================================================================
; File author: Karim Haddad
;============================================================================
;;Tree/Tempo editor


      
(in-package :om)



(defun tree-tempo-edit-pane (self tree tempo)
  (let* ((treebuff (setf om-edit::*editor-text* tree))
         (tempobuff (setf om-edit::*tempo-editor-text* tempo))
         (ept (om-edit::open-tree-tempo-editor self treebuff tempobuff)))
    ;(print (list (om-edit::ep ept)))
    (setf (om-edit::score (car (om-edit::ep ept))) self) 
    (setf (om-edit::score (second (om-edit::ep ept))) self) 
    (setf (om-edit::intfunc (car (om-edit::ep ept))) #'om::set-tree)
    (setf (om-edit::intfunc (second (om-edit::ep ept))) #'om::set-tree-tempo)
    ))

(defmethod get-tree-tempo-list ((self scorepanel))
  (let* ((selection (selection? self)))
        (if selection
        (let ((treeobj (car selection))
              (tempoobj selection))
          (cond ((voice-p treeobj) 
                 (let ((ntree (format-tree-by-meas (list '? (cadr (tree treeobj)))))
                       (ntempo (format-voice-tempo-by-meas (tempo (car tempoobj)))))
                   (tree-tempo-edit-pane self ntree ntempo)))
                
                ((measure-p treeobj)
                 (let* ((pere (parent treeobj))
                        (pos (sort. (loop for i in tempoobj
                                             collect (position i (inside pere) :test 'equal)) '>))
                        (tempo (reverse (loop for i in pos 
                                              append (reverse (nth-tempo-mes (tempo-meas-changes pere) i))))))
                   (tree-tempo-edit-pane self (format nil "~S" (tree treeobj)) (format-measures-tempo-by-meas tempo))))
                
                ;stdby
                ;((group-p treeobj)
               ;(tree-edit-pane self (format nil "~S" (redratiogrp treeobj)))) 
              (t))
          ))
        ))
