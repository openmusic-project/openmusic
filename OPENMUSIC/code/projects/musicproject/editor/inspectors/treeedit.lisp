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
;;Tree editor


(in-package :om)

;-------------------General utilities-------------------

(defun str->list (str) 
  (read (make-string-input-stream 
         (concatenate 'string "(" str ")"))))

(defun concat-string (list)
  "A non-recursive function that concatenates a list of strings. From Vijay Nathew"
  (if (listp list)
      (with-output-to-string (s)
         (dolist (item list)
           (if (stringp item)
             (format s "~a" item))))))

(defun format-tree-by-meas (tree)
  "This formats a voice's tree with measures on each line"
  (let ((rep (list (format nil "(? ( ~%")))
        (meas (cadr tree)))
    (loop for i in meas
          do (setf rep (append rep (list (format nil "~S ~%" i)))))
    (setf rep (append (list rep (format nil ")) ~%"))))
    (concat-string (flat rep))
    ))

;-------------------Score utilities-------------------

(defmethod redratiogrp ((self group))
  "gives the REAL tree of a group without the ratios as a D"
  (let* ((pere (parent self))
         (pos (position self (inside pere) :test 'equal))
         (treep (cadr (tree pere)))
         )
    (if pos
        (nth pos treep) 
      )))

(defmethod redratiogrp ((self measure))
  "gives the REAL tree of a group without the ratios as a D"
  (let ((cartree (car (tree self))))
    (if (ratiop cartree)
        (progn
        (setf (car (tree self)) (list (numerator cartree) (denominator cartree)))
        self)
      self
      )))

;-----------------------------------------------------------



(defun tree-edit-pane (self tree)
  (let* ((buff (setf om-edit::*editor-text* tree))
        (ept (om-edit::open-tree-editor self buff)))
    (setf (om-edit::score ept) self) 
    (setf (om-edit::intfunc ept) #'om::set-tree)
    ))


(defmethod get-score-tree ((self scorepanel))
  (let* ((selection (selection? self)))
        (if selection
        (let ((obj (car selection)))
          (cond ((voice-p obj) 
                 (let ((ntree (format-tree-by-meas (list '? (cadr (tree obj))))))
                   (tree-edit-pane self ntree)))
                ((measure-p obj)
                 (tree-edit-pane self (format nil "~S" (tree obj))))
                ;stdby
                ;((group-p obj)
               ;(tree-edit-pane self (format nil "~S" (redratiogrp obj)))) 
              (t))
          ))
    ))

;--------------------------
;set-tree
    
(defmethod replace-meas-tree ((self voice) (item list) (n number))
  (let* ((tree (cadr (tree self)))
         (repl (replace-in-list tree item n)))
    (list '? repl)))
         



(defmethod set-tree ((self voicepanel) tree)
  (if (selection? self)
      (let* ((selection (car (selection? self)))
             (voice (object (om-view-container self))))
        (if (voice-p selection) 
            (setf (tree voice) (car (str->list tree)))
          
          (let* ((pos (position selection (inside voice) :test 'equal))
                 (reptree (replace-meas-tree voice (car (str->list tree)) pos)))
            (setf (tree voice) (fix-tree-floats-rests reptree))
            (setf (chords voice) (get-chords&cont-chords voice))
            (setf (selection? self) (list selection))
        ;in order to keep measure selection:
            (setf (selection? self) (list (nth pos (inside voice))))
            ))
             ; (do-initialize-metric-sequence voice)
        (update-panel self t)
        )
    (om-beep-msg "Please select a voice or a measure") 
    ))

(defmethod set-tree ((self polypanel) tree)
  (if (selection? self)
      (let* ((selection (car (selection? self)))
             (poly (object (om-view-container self))))
        (cond 
         ((voice-p selection) 
          (setf (tree selection) (car (str->list tree))))
         ((measure-p selection)
          (let* ((pere (parent selection))
                 (pos (position selection (inside pere) :test 'equal))
                 (reptree (replace-meas-tree pere (car (str->list tree)) pos)))
            (setf (tree pere) (fix-tree-floats-rests reptree))
            (setf (chords pere) (get-chords&cont-chords pere))
            (setf (selection? self) (list selection))
            (setf (selection? self) (list (nth pos (inside pere))))
            ))
         (t))
        (update-panel self t)
        )
    (om-beep-msg "Please select a voice or a measure") 
    ))
