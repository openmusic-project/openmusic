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
;;; authors G. Assayag, C. Agon, J. Bresson
;=========================================================================


(in-package :om)

;;; ------------------------------------------------------------
;;; GET-FRINGE DONNE LA LISTE A PLAT DES OBJETS SIMPLE CONTAINER
;;; ------------------------------------------------------------

(defmethod get-fringe ((self poly)
                         &key (frac-min (fraction-minimale self )) (previous-qvalue 1)  (current-offset 0) 
                         (w-note t) (w-rest ())  )
  (loop for item in (inside self)  
        collect (get-fringe item :frac-min frac-min 
                                 :previous-qvalue previous-qvalue 
                                 :current-offset current-offset 
                                 :w-note w-note  
                                 :w-rest w-rest )  )
  )


(defmethod get-fringe ((self container) 
                         &key  (frac-min (fraction-minimale self ))  (previous-qvalue 1)  (current-offset 0) 
                         (w-note t) (w-rest ()) )
    (let ((new-current-offset (+ current-offset (/ (* (offset self) frac-min) previous-qvalue) ) ))
      (loop for item in (inside self)
          append (get-fringe item :frac-min frac-min 
                                  :previous-qvalue (qvalue self) 
                                  :current-offset new-current-offset 
                                  :w-note w-note 
                                  :w-rest w-rest )
          )
      )
    )

(defmethod get-fringe ((self note) 
                         &key (frac-min (fraction-minimale self )) 
                         (previous-qvalue 1) (current-offset 0)
                         (w-note t) (w-rest ()) )
   (declare (ignore w-rest))
   (if w-note
     (let ((new-simple-container (duplique-structure-musicale self)))
       (setf (offset new-simple-container) (+ (/ (* (offset self) frac-min) previous-qvalue) current-offset))
       (setf (qvalue new-simple-container) frac-min)
       (setf (extent new-simple-container) (/ (* (extent self) frac-min) (qvalue self)) )
       (list new-simple-container)
       )
     ()
     )
   )

(defmethod get-fringe ((self rest) 
                         &key (frac-min (fraction-minimale self )) (previous-qvalue 1) (current-offset 0) 
                         (w-note t) (w-rest ()) )
      (declare (ignore w-note))
      (if w-rest 
     (let ((new-simple-container (duplique-structure-musicale self)))
       (setf (offset new-simple-container) (+ (/ (* (offset self) frac-min) previous-qvalue) current-offset))
       (setf (qvalue new-simple-container) frac-min)
       (setf (extent new-simple-container) (/ (* (extent self) frac-min) (qvalue self)) )
       (list new-simple-container)
       )
     ()
     )
   )

;;; ===============================================================================================
;;; UNION EXCLUSIVE DE DEUX FRANGES - ***** EN TRAVAUX ***** 
;;; ===============================================================================================


(defmethod fringe-xor ((fringe1 list) (fringe2 list))
  (if (and fringe1 fringe2)
    (let ((e1 (car fringe1)) (e2 (car fringe2)))
      (if (< (offset e1) (offset e2) )
        (progn 
          (if (> (+ (offset e1) (extent e1)) (offset e2))
            (setf (extent e1) (- (offset e2) (offset e1)))
            ()
            )
          (cons e1 (fringe-xor (cdr fringe1) fringe2))
          )
        (if (> (offset e1) (offset e2) )
          (fringe-xor fringe2 fringe1)
          (if (< (extent e1) (extent e2))
            (cons e1 (fringe-xor (cdr fringe1) (cdr fringe2)))
            (cons e2 (fringe-xor (cdr fringe1) (cdr fringe2)))
            )
          )
        )
      )
    (if fringe1
      fringe1
      fringe2
      )
    )
  )


;;; ===============================================================================================
;;; Union de deux franges : 
;;; ===============================================================================================

(defmethod fringe-or ((fringe1 list) (fringe2 list))
   (if (and fringe1 fringe2)
     (let ((e1 (car fringe1)) (e2 (car fringe2)))
       (if (< (offset e1) (offset e2) )
         (if (>= (offset e2) (+ (offset e1) (extent e1)))
           ;; pas de chevauchement
           (if (or (eq (tie e1) 'continue) (eq (tie e1) 'begin ))
             (let ((eseconde ;; on cherche dans fringe2 une note qui commence quand e1 termine et de meme midic que e1
                    (loop for item in fringe2    
                          when (and (eq (offset item) (+ (extent e1) (offset e1)) ) (eq (midic item) (midic e1) ) ) do (return item )
                          when (> (offset item) (+ (extent e1) (offset e1)) ) do (return nil )
                          )
                    )
                   )
               (if eseconde 
                 (let ((suivant-e1 
                        (loop for item in fringe1
                              when (and (eq (offset item) (+ (extent e1) (offset e1)) ) (eq (midic item) (midic e1) ) )
                              do (return item)
                              )
                        )
                       )
                   (if (or (eq (tie eseconde) 'end) (eq (tie eseconde) 'continue))
                     (cons e1 (fringe-or (cdr fringe1) fringe2))
                     (progn 
                       (setf (tie suivant-e1) (cond
                                 ((eq (tie suivant-e1) 'end) 'nil)
                                 ((eq (tie suivant-e1) 'continue) 'begin)))
                       (setf (tie e1) (cond
                                 ((eq (tie e1) 'begin) 'nil)
                                 ((eq (tie e1) 'end) 'end)
                                 ((eq (tie e1) 'continue) 'end)
                                 (t 'nil )))
                       (cons e1 (fringe-or (cdr fringe1) fringe2))
                       )
                     
                     )
                   )
                 
                 
                 
                 
                 (cons e1 (fringe-or (cdr fringe1) fringe2))
                 )
               )
             (cons e1 (fringe-or (cdr fringe1) fringe2))
             )


           ;; sinon, chevauchement, il faut couper e1
           (let ((new-note (duplique-structure-musicale e1))
                 (eprime 
                    (loop for item in fringe2    
                          when (and (eq (offset item) (offset e2) ) (eq (midic item) (midic e1) ) ) do (return item )
                          when (> (offset item) (offset e2 ) ) do (return nil )
                          )
                    )
                 )
             (setf (extent new-note) (- (+ (extent e1) (offset e1)) (offset e2) ))
             (setf (offset new-note) (offset e2) )
             (setf (extent e1) (- (offset e2) (offset e1) ) )
             ; ici, il faut verifier dans fringe2 si un midic correspondant a celui de e1 correspond. 
             ; dans ce cas, on ne fait pas de liaison. 
             (if eprime

               (if (or (eq (tie eprime) 'continue ) (eq (tie eprime) 'end))
                 (progn 
                   (setf (tie new-note) (cond
                                         ((eq (tie e1) 'begin) 'continue)
                                         ((eq (tie e1) 'end) 'end)
                                         ((eq (tie e1) 'continue) 'continue)
                                         (t 'end )))
                   (setf (tie e1) (cond
                                   ((eq (tie e1) 'begin) 'begin)
                                   ((eq (tie e1) 'end) 'continue)
                                   ((eq (tie e1) 'continue) 'continue)
                                   (t 'begin )))
                   )
                 
                 (progn ;;; il y a une note de meme hauteur : on ne fait pas de liaison 
                   (setf (tie new-note) (cond
                                         ((eq (tie e1) 'begin) 'begin)
                                         ((eq (tie e1) 'end) '() )
                                         ((eq (tie e1) 'continue) 'begin)
                                         (t '() )))
                   (setf (tie e1) (cond
                                   ((eq (tie e1) 'begin) 'nil)
                                   ((eq (tie e1) 'end) 'end)
                                   ((eq (tie e1) 'continue) 'end)
                                   (t 'nil )))
                   )
                 )
               (progn ;;; il n'y a pas de note de meme hauteur... on fait la liaison normalement 
                 (setf (tie new-note) (cond
                                       ((eq (tie e1) 'begin) 'continue)
                                       ((eq (tie e1) 'end) 'end)
                                       ((eq (tie e1) 'continue) 'continue)
                                       (t 'end )))
                 (setf (tie e1) (cond
                                 ((eq (tie e1) 'begin) 'begin)
                                 ((eq (tie e1) 'end) 'continue)
                                 ((eq (tie e1) 'continue) 'continue)
                                 (t 'begin )))
                 )
               )
               (cons e1 (fringe-or (sort (cons new-note (cdr fringe1)) #'< :key #'offset ) fringe2))
             )
           )
         (if (> (offset e1) (offset e2))
           (fringe-or fringe2 fringe1)
           ;; sinon les deux notes commencent en meme temps (offset e2 ) = (offset e1 )
           (if (< (extent e1) (extent e2))
             (let ((new-note (duplique-structure-musicale e2))
                   (eprime ;; on cherche dans fringe1 une note qui commence avec e1 et de meme midic que e2
                    (loop for item in fringe1    
                          when (and (eq (offset item) (offset e1) ) (eq (midic item) (midic e2) ) ) do (return item )
                          when (> (offset item) (offset e2 ) ) do (return nil )
                          )
                    )
                   (eseconde ;; on cherche dans fringe1 une note qui commence quand e1 termine et de meme midic que e2
                    (loop for item in fringe1    
                          when (and (eq (offset item) (+ (extent e1) (offset e1)) ) (eq (midic item) (midic e2) ) ) do (return item )
                          when (> (offset item) (+ (extent e1) (offset e1)) ) do (return nil )
                          )
                    )
                   )
               (setf (extent new-note) (- (extent e2) (extent e1)))
               (setf (offset new-note) (+ (extent e1) (offset e1)) )
               (setf (extent e2) (extent e1) )
               
               (if eprime ;; si cette note existe
                 (if (and  eseconde (not (or (eq (tie eprime) 'begin) (eq (tie eprime) 'continue))))
                   ;; et si il y en a une autre juste derriere de meme hauteur... 
                   (progn
                     ;; on abandonne e2, on regle le tie de eprime, on ne prend pas encore e1 : c'est peut etre eprime
                     
                     (setf (tie new-note) (cond
                                           ((eq (tie e2) 'begin) 'begin)
                                           ((eq (tie e2) 'end) '())
                                           ((eq (tie e2) 'continue) 'begin)
                                           (t '() )))
                     (setf (tie eprime) (cond
                                         ((eq (tie eprime) 'end) 'end)
                                         ((eq (tie e2) 'continue) 'end)
                                         ((eq (tie e2) 'end) 'end)
                                         (t '() )))

                     (fringe-or fringe1 (sort (cons new-note (cdr fringe2) ) #'< :key #'offset ) )
                     )
                   
                   (progn ;; et si il n'y en a PAS une autre juste derriere de meme hauteur...
                     ;; on abandonne e2, eprime fera l'affaire une fois qu'on aura regle le tie
                     (setf (tie new-note) (cond
                                           ((eq (tie e2) 'begin) 'continue)
                                           ((eq (tie e2) 'end) 'end)
                                           ((eq (tie e2) 'continue) 'continue)
                                           (t 'end )))
                     (setf (tie eprime) (cond
                                         ((eq (tie eprime) 'end) 'continue)
                                         ((eq (tie e2) 'begin) 'begin)
                                         ((eq (tie e2) 'continue) 'continue)
                                         ((eq (tie e2) 'end) 'continue)
                                         (t 'begin )))
                     (fringe-or fringe1   (sort (cons new-note (cdr fringe2) ) #'< :key #'offset ) )
                     )
                   )
                 ;; si eprime n'existe pas 
                 (if eseconde ;;; alors il ne faut pas lier e2 et newnote,
                   (progn
                     (setf (tie new-note) (cond
                                           ((eq (tie e2) 'begin) 'begin)
                                           ((eq (tie e2) 'end) nil)
                                           ((eq (tie e2) 'continue) 'begin)
                                           (t nil )))
                     (setf (tie e2) (cond
                                     ((eq (tie e2) 'begin) nil)
                                     ((eq (tie e2) 'end) 'end)
                                     ((eq (tie e2) 'continue) 'end)
                                     (t nil )))
                     (cons e1 (cons e2 (fringe-or (cdr fringe1)  
                                                  (sort (cons new-note (cdr fringe2) ) #'< :key #'offset ) )) )
                     )
                   (progn  ;;; sinon il faut les lier, on peut quand meme garder e1
                     (setf (tie new-note) (cond
                                           ((eq (tie e2) 'begin) 'continue)
                                           ((eq (tie e2) 'end) 'end)
                                           ((eq (tie e2) 'continue) 'continue)
                                           (t 'end )))
                     (setf (tie e2) (cond
                                     ((eq (tie e2) 'begin) 'begin)
                                     ((eq (tie e2) 'end) 'continue)
                                     ((eq (tie e2) 'continue) 'continue)
                                     (t 'begin )))
                     (cons e1 (cons e2 (fringe-or (cdr fringe1)  
                                                  (sort (cons new-note (cdr fringe2) ) #'< :key #'offset ) )))
                     )
                   )
                 
                 )
               )
             (if (> (extent e1) (extent e2))
               (fringe-or fringe2 fringe1)  ;; on inverse les deux franges 
               ;;; les deux notes son parfaitement ensembles 
               (let (
                     (eprime ;; on cherche dans fringe1 une note qui commence avec e1 et de meme midic que e2
                      (loop for item in fringe1    
                            when (and (eq (offset item) (offset e1) ) (eq (midic item) (midic e2) ) ) do (return item )
                            when (> (offset item) (offset e2 ) ) do (return nil )
                            )
                      )
                     )
                 (if eprime 
                   (progn  ;; on se debarasse de e2, en modifiant le tie de eprime
                     (setf (tie eprime) (tie-choose (tie eprime) (tie e2) ))
                     (fringe-or fringe1 (cdr fringe2 ) )
                     )
                   
                   ;; si eprime n'existe pas, on conserve simplement e2  tel quel 
                   (cons e2 (fringe-or fringe1 (cdr fringe2)))
                   )
                 )
               )
             )
           )
         )
       )
     (if fringe1 fringe1 (if fringe2 fringe2 ())) ;; pour terminer la liste
     )
   )


(defmethod tie-choose (tie1 tie2)
  (cond 
   ((eq tie1 'continue) 'continue )
   ((eq tie2 'continue) 'continue )

   ((and (eq tie1 'begin) (eq tie2 'end)) 'continue)
   ((and (eq tie2 'begin) (eq tie1 'end)) 'continue)

   ((and (eq tie1 'begin) (eq tie2 '())) 'begin)
   ((and (eq tie2 'begin) (eq tie1 '())) 'begin)

   ((and (eq tie1 'end) (eq tie2 '())) 'end)
   ((and (eq tie2 'end) (eq tie1 '())) 'end)

   ((and (eq tie1 'begin) (eq tie2 'begin)) 'begin)
   ((and (eq tie1 'end) (eq tie2 'end)) 'end)
   ((and (eq tie1 '()) (eq tie2 '())) '())
   )
  )





;;; ===============================================================================================
;;; INTERSECTION DE DEUX FRANGES   **** EN TRAVAUX *****
;;; ===============================================================================================


(defmethod fringe-and ((fringe1 list) (fringe2 list) &optional (err-rate 0 ))
  (if (and fringe1 fringe2 )
    (let ((e1 (car fringe1)) (e2 (car fringe2)))
      (if (<= (offset e1) (offset e2))
        (if (<= (abs (- (offset e1) (offset e2)) ) err-rate)
          (cons (if (< (extent e1) (extent e2)) e1 e2) (fringe-and (cdr fringe1) (cdr fringe2) err-rate))
          (fringe-and (cdr fringe1) fringe2)
          )
        (fringe-and fringe2 fringe1 err-rate)
        )
      )
    ()
    )
  )