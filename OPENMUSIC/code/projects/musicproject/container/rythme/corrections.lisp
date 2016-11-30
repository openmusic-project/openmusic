;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om )
;;; CORRECTION DE AUTOMATIC CHORD INSERT EN METTANT DES ACCORDS PARTOUT (MEME POUR LES NOTES SEULES)
;;; elle sert a faire des accords partout, mme pour des notes isolees.

(defmethod automatic-chord-insert ((self container))
  (let ((note-list (loop for item in (inside self) when (note-p item) collect item)))
    (if note-list
      (setf (inside self) (append (set-difference (inside self) note-list) ;; la structure
                                  (loop for item in (chord-formated-list note-list)
                                        collect (list->chord item)
                                        )
                                  ))
      ()
      )
    (setf (inside self) (sort (inside self) #'< :key #'offset ))
    (loop for item in (inside self)
          when (and (container-p item)  (not (chord-p item))) ;;; a changer avec le nouveau predicat
          do (automatic-chord-insert item) )
    )
  )

(defun chord-formated-list ( note-list)
  (if (cdr note-list)
    (if (eq (offset (car note-list)) (offset (cadr note-list)))
      (let ((temp (chord-formated-list (cdr note-list))))
        (cons (cons (car note-list) (car temp)) (cdr temp))
        )
      (cons (list (car note-list)) (chord-formated-list (cdr note-list)))
      )
    (list note-list)
    )
  )





;;; CORRECTION DE GET-STRUCTURE
(defmethod get-structure ((self sequence*)  &optional (pere ()) )
   (declare (ignore pere))
   (let ((sequence (call-next-method )))
     (setf (slot-value  sequence 'tree) nil)
     sequence
     ))



;; On rajoute (setf (offset item) 0) pour que chaque note commence exactement au debut du nouvel accord
;; --> permet d'enlever une correction de Gerard.

(defmethod list->chord (list-of-notes)
  (let ((new-chord (mki 'chord :empty t)))
    (setf (inside new-chord) list-of-notes )
    (setf (qvalue new-chord) (qvalue (car list-of-notes)))
    (setf (extent new-chord) (extent (car list-of-notes)))
    (setf (offset new-chord) (offset (car list-of-notes)))
    (loop for item in list-of-notes
          do (setf (parent item) new-chord)
          do (setf (offset item) 0)
          )
    new-chord
    )
  )



;; propage subdivision avec "by 1" ce qui impose de decomposer le container en un nombre de sous container
;; egal a la subdivision irreguliere.

(defmethod propage-subdivision ((self container) (sub integer) (c container)  &optional (running-offset 0) )
   ; sub est le chiffrage de la subdivision irreguliere a propager, c est le container qui la represente.
   (if (inside self)
     ; si le container n'est pas terminal dans la structure, on propage dans son contenu.
     (loop for item in (inside self) 
           do (propage-subdivision item sub c  (+ running-offset (offset self))))
     (let ((start-self (+ (offset self) running-offset))
           (end-self (+ running-offset (offset self) (extent self)))
           (start-c (offset c))
           (end-c (+ (offset c) (extent c)))
           )
       (if (and
            (or 
             (and (>= start-c start-self) (< start-c end-self))
             (and (> end-c start-self) (<= end-c end-self))
             (and (< start-c start-self) (> end-c end-self))
             )
            (zerop (mod (extent self) sub))
            )
         (setf (inside self)
               (loop for compteur from 1 to sub by 1 ;;; ( / (extent self) sub ) 

                     collect (mki 'group 
                                  :empty t
                                  :parent self
                                  :extent ( / (extent self) sub )  ;; sub
                                  :offset (*  (- compteur 1)   ( / (extent self) sub ) )
                                  :qvalue (qvalue self )
                                  )
                     )
               )
         ()
         )
       )
     )
   )