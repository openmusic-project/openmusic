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
         (mes (loop for i from 0 to (length (inside self))
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
         (rep (list (format nil "( ~S ~% (~%" init)))
         (meas (cadr tempo)))
    (loop for i in meas
          do (setf rep (append rep (list (format nil "~S ~%" i))))) 
          
    (setf rep (append (list rep (format nil "))"))))
    (concat-string (flat rep))
    ))

(defun format-poly-tempo (tempo)
  (let* ((vx-temp (mapcar #'format-voice-tempo-by-meas tempo))
         rep)
    (loop for i in vx-temp
          do (setf rep (append rep (list (format nil "~A ~% ~%" i)))))
    (concat-string (flat rep)) ))


(defun tempo-edit-pane (self tempo)
  (let* ((buff (setf om-edit::*tempo-editor-text* tempo))
        (ept (om-edit::open-tempo-editor self buff)))
    (push ept (attached-editors (om-view-container self)))
    (setf (om-edit::score ept) self) 
    (setf (om-edit::intfunc ept) #'om::set-tree-tempo)
    ))

(defmethod get-tempo-tree ((self scorepanel))
  (let* ((selection (selection? self)))
    (if selection
        (let ((obj selection))
          (cond 
           ((poly-p (car obj)) 
            (let ((tempi (format-poly-tempo 
                          (mapcar #'tempo (inside (car obj))))))
              (tempo-edit-pane self tempi)))
           ((voice-p (car obj)) 
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
           ((or (rest-p (car obj))
                (chord-p (car obj)))
            (progn
              (add-tempo-change-extra (car obj))
              (update-panel self)
              ))
           (t))
          ))
    ))
;;;remove tempo [legacy pane] wrapper

(defmethod remove-tempo ((self t)) nil)

(defmethod remove-tempo ((self scorepanel))
  (let ((sel-obj (car (selection? self))))
    (rmv-tempo-change-extra sel-obj)
    (update-panel self)))

;;;set tempo


(defun remove-if-car (item seq)
(remove nil  (loop for i in seq
        collect (if (= (caar i) item) nil i))))


(defun sort-tempo-list (tempo)
  (sort-list 
   (sort-list 
    (remove-duplicates tempo :test 'equal :key 'car) 
    :test '< :key 'cadar) 
   :test '< :key 'caar))
    

(defun replace-new-mes-tree (voice meas tree)
  (let* ((pos (position meas (inside voice) :test 'equal))
         (vxtemp (tempo voice))
         (tempheader (car vxtemp))
         (templst (remove-if-car pos (second vxtemp)))
         (sort (sort-tempo-list (append tree templst)))
         )
    (list tempheader sort)))
  
(defmethod set-tree-tempo ((self voicepanel) tree)
  (if (selection? self)
      (let* ((selection (car (selection? self)))
             (voice (object (om-view-container self)))
             (tempovoice (tempo voice)))
        (if (voice-p selection) 
            (setf (tempo voice) (car (str->list tree)))
          (let* ((meas (selection? self))
                 (newtree (replace-new-mes-tree (parent selection) (car meas) (str->list tree))))
            (setf (tempo voice) newtree)
            ))
        (do-initialize-metric-sequence voice)
        (update-panel self t)
        )
    (om-beep-msg "Please select a voice or a measure") 
    ))


(defmethod set-tree-tempo ((self polypanel) tree)
  (if (selection? self)
      (let* ((selection (car (selection? self)))
             (poly (object (om-view-container self))))
        (cond 
         ((poly-p selection) 
          (let ((inside (inside selection))
                (trees (str->list tree)))
            (loop for i in inside
                  for tr in trees
                  do (setf (tempo i) tr))))
         ((voice-p selection) 
          (setf (tempo selection) (car (str->list tree)))
          (do-initialize-metric-sequence selection))
         ((measure-p selection)
          (let* ((meas (selection? self))
                 (pere (parent selection))
                 (newtree (replace-new-mes-tree pere (car meas) (str->list tree))))
            (setf (tempo pere) newtree)
            ))
         (t))
    ;(do-initialize-metric-sequence (car poly))
        (update-panel self t)
        )
    (om-beep-msg "Please select a voice or a measure") 
    ))

