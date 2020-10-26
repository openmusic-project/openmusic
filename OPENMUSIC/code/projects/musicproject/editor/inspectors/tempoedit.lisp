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
;;Tempo editor

(in-package :om)


(defmethod tempo-meas-changes ((self voice))
  "returns all tempi of each measures even if no changes. Editing utility."
  (let* ((tempo (tempo self))
         (format-tempo (cons (list '(0 0) (car tempo))
                             (cadr tempo)))
         (mes (loop for i from 0 to (- (length (inside self)) 1)
                                       collect (list (list i 0) nil)))
         res mem (indx 0)
         )
    (loop while mes
          do (if (equal (caar mes) (caar format-tempo))
                 (progn 
                   (push (car format-tempo) res)
                   (pop format-tempo)
                   (pop mes))
               (if format-tempo
                   (if (< (caaar format-tempo) (caaar mes))
                       (progn
                         (push (car format-tempo) res)
                         (setf mem (cadar format-tempo))
                         (pop format-tempo))
                     (progn 
                       (push (list (caar mes) mem) res)
                       (pop mes)))
                 (progn 
                       (push (list (caar mes) mem) res)
                       (pop mes)))
               ))
    (reverse res)))
                     

(defun nth-tempo-mes (liste n)
(remove nil  (loop for i in liste
                   collect (if (= (caar i) n) i))))



;;;;Access

(defun format-measures-tempo-by-meas (tempo)
  "This formats given measures' tempo on each line"
  (let (rep)
    (loop for i in tempo
          do (setf rep (append rep (list (format nil "~S ~%" i))))) 
    (concat-string (flat rep))
    ))

(defun format-voice-tempo-by-meas (tempo)
  "This formats a voice's tempo with measures on each line"
  (let* ((init (car tempo))
         (rep (list (format nil "( ~S ~% (" init)))
         (meas (cadr tempo)))
    (loop for i in meas
          do (setf rep (append rep (list (format nil "~S ~%" i))))) 
          
    (setf rep (append (list rep (format nil ")) ~%"))))
    (concat-string (flat rep))
    ))

(defun tempo-edit-pane (self tempo)
  (let* ((buff (setf om-edit::*tempo-editor-text* tempo))
        (ept (om-edit::open-tempo-editor self buff)))
    (setf (om-edit::score ept) self) 
    (setf (om-edit::intfunc ept) #'om::set-tree-tempo)
    ))

(defmethod get-tempo-tree ((self scorepanel))
  (let* ((selection (selection? self)))
        (if selection
        (let ((obj selection))
          (cond ((voice-p (car obj)) 
                 (let ((ntree (format-voice-tempo-by-meas (tempo (car obj)))))
                   (tempo-edit-pane self ntree)))
                ((measure-p (car obj))
                 (let* ((pere (parent (car obj)))
                        (pos (sort. (loop for i in obj
                                             collect (position i (inside pere) :test 'equal)) '>))
                        (tempo (reverse (loop for i in pos 
                                              append (reverse (nth-tempo-mes (tempo-meas-changes pere) i))))))
                   
                   (tempo-edit-pane self (format-measures-tempo-by-meas tempo))
                   ))
              (t))
          ))
        ))

;;;set tempo

(defun rep-if-car (item seq)
    (loop for i in seq
          collect (if (equal (car item) (car i))
                 item i)))
(defun replace-if-car (liste seq)
  (let ((res seq))
    (loop for i in liste
          do (setf res (rep-if-car i res)))
    res))

(defun change-nth-meas-tempo (voice liste)
  (let* ((tempo (tempo voice))
         (meas (cadr tempo)))
    (replace-if-car liste meas)))
        

(defmethod set-tree-tempo ((self voicepanel) tree)
  (let* ((selection (car (selection? self)))
         (voice (object (om-view-container self))))
    (if (voice-p selection) 
        (setf (tempo voice) (car (str->list tree)))
      (let* ((meas (selection? self))
             (tempo-header (car (tempo voice)))
             (change (change-nth-meas-tempo voice (str->list tree)))
             )
       (setf (tempo voice) (list tempo-header change))
       ))
    (do-initialize-metric-sequence voice)
    (update-panel self t)
    ))

#|
(defmethod set-tree-tempo ((self polypanel) tree)
  (let* ((selection (car (selection? self)))
         (poly (object (om-view-container self))))
    (if (voice-p selection) 
        (setf (tree selection) (car (str->list tree)))
      (rectree selection :obj (car (str->list tree))))
    (update-panel self t)
    ))
|#
