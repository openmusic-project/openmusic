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

(defmethod change-chords ((self measure) 
                          (chords list))
  (let* ((pere (parent self))
         (pos (position self (inside pere) :test 'equal))
         (voices (voice->voices pere))
         (measure (nth pos voices)))
    (setf (chords measure) chords)
     (setf (tree measure) (tree measure))
     (car (inside measure))))    

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

(defmethod rectree ((self group) &key (position nil) (obj nil))
  (let* ((pere (parent self))
         (pos (position self (inside pere) :test 'equal))
         (qval (qvalue self))
         (ext (extent self))
         (chords (chords self))
         )
    (if pos
        (if position 
            (progn
              ;(setf (inside self) chords)
              (setf (nth pos (inside pere)) self)
              ;(setf (inside self) chords)
              (setf (qvalue self) qval)
              (setf (extent self) ext)
              ;(setf (inside self) chords)
              
            (loop for i in (inside self)
                  for n from 0 to (length (inside self))
                  do (rectree i :position pos))
              (rectree pere :position pos :obj self)
              )
          (progn
           ; (setf (inside self) chords)
            (setf (tree self) obj)
            (setf (qvalue self) qval)
            (setf (extent self) ext)
            (loop for i in (inside self) 
                  for n from 0 to (length (inside self))
                  do (rectree i :position n)) 
            (rectree pere :position pos :obj self)
            )
          )
      (om-beep-msg "Please select a GROUP"))
    ))

(defmethod rectree ((self measure) &key (position nil) (obj nil))
  (let* ((pere (parent self))
         (pos (position self (inside pere) :test 'equal))
         (chords (get-chords self))
         )
    (if pos
        (if position 
            (rectree pere :position pos :obj self)
          (progn
            (setf (tree self) obj) 
            (rectree pere :position pos :obj (change-chords self chords))
            )
          )
      (om-beep-msg "Please select a MEASURE."))
    ))


(defmethod rectree ((self voice) &key (position nil) (obj nil))
  (setf (nth position (inside self)) obj)
  (do-initialize-metric-sequence self)
  (setf (tree self) (tree self)))


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


(defmethod set-tree ((self voicepanel) tree)
  (let* ((selection (car (selection? self)))
         (chords (loop for item in (selection? self)
                       append (cons-chord&rest-list item)))
         (voice (object (om-view-container self))))
    (if (voice-p selection) 
        (setf (tree voice) (car (str->list tree)))
      (rectree selection :obj (car (str->list tree))))
    (do-initialize-metric-sequence voice)
    (update-panel self t)
    ))

(defmethod set-tree ((self polypanel) tree)
  (let* ((selection (car (selection? self)))
         (poly (object (om-view-container self))))
    (if (voice-p selection) 
        (setf (tree selection) (car (str->list tree)))
      (rectree selection :obj (car (str->list tree))))
    (update-panel self t)
    ))
