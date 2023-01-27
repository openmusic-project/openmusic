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


(defmethod do-grouping ((self container))
   (qreduce self)
   (setf (inside self) (loop for item in  (grouping-formated-list (inside self)) 
                             when (eq (length item ) 1)
                             collect (car item)
                             when (> (length item) 1 )
                             collect (list->group item )
                             )
         )
   (loop for item in (inside self) 
         when (and (container-p item) (not (chord-p item)))
         do (do-grouping item)
         )
   )



(defmethod list->group (item-list)
   "effectue le regroupement d'un ensemble de simple-containers"
   ;;(format t "list->group : regroupement de ~A items , ~A ~%" (length item-list) item-list)
  (if (chord-p (car item-list))
    (let ((new-extent  (* (loop for sc in item-list sum (/ (extent sc )   (qvalue sc))     )  (qvalue (car item-list))   )))
      (setf (extent (car item-list )) new-extent )
      (loop for note in (inside (car item-list))
            do (progn 
                 (setf (tie note) (tie-remplacement note (corresponding-note (car (last item-list)) note )))
                 (setf (extent note) new-extent )
                 )
            )
      (car item-list)
      )
    (let ((new-extent  (* (loop for sc in item-list sum (/ (extent sc )   (qvalue sc))     )  (qvalue (car item-list))   )))
      (setf (extent (car item-list )) new-extent )
      (if (note-p (car item-list)) 
        (setf (tie (car item-list)) (tie-remplacement (car item-list) (car (last item-list)))       ) 
        ())
      (car item-list)
      )
    )
  )





(defmethod tie-remplacement ((n1 note) (n2 note))
   "Donne la nouvelle valeur de tie pour deux notes consecutives qui vont etre regroupees"
  (cond 
   ((and (eq (tie n1) 'begin ) (eq (tie n2) 'end )) ())
   ((and (eq (tie n1) 'begin ) (eq (tie n2) 'continue )) 'begin)
   ((and (eq (tie n1) 'continue ) (eq (tie n2) 'continue )) 'continue)
   ((and (eq (tie n1) 'continue ) (eq (tie n2) 'end )) 'end)
   )
  )

(defmethod corresponding-note ( (c1 chord) (c2 note) )
  (loop for item in (inside c1)  thereis (and (eq (midic item) (midic c2)) item ) )
  )



(defmethod grouping-formated-list ( item-list )
   "formate en sous listes des elements consecutifs qui peuvent etre regroupes "
  (if (cdr item-list)
    (if (regroupables (car item-list) (cadr item-list ) )
      (let ((temp (grouping-formated-list (cdr item-list))))
        (cons (cons (car item-list) (car temp)) (cdr temp))
        )
      (cons (list (car item-list)) (grouping-formated-list (cdr item-list)))
      )
    (list item-list)
    )
  )


(defmethod regroupables ((c1 note) (c2 note))
  (and (consecutifs c1 c2)
       (or (eq (tie c1) 'begin ) (eq (tie c1) 'continue ) )
       (durees-compatibles c1 c2)
       )
  )

(defmethod regroupables ((c1 rest) (c2 rest))
  (and (consecutifs c1 c2)
       (durees-compatibles c1 c2)
       )
  )

(defmethod regroupables ((c1 chord) (c2 chord ))
   ;;(format t "regroupables, accord offset c1 ~A, extent c1 : ~A, offset c2 :~A, extent c2 :~A ~%" 
   ;;        (offset c1) (extent c1) (offset c2) (extent c2))

  (and (consecutifs c1 c2)
       (durees-compatibles c1 c2 )
       (loop for item in (inside c1)
             with logvar = t
             do (setf logvar (and logvar (or (eq (tie item) 'begin ) (eq (tie item) 'continue ) ) ))
             finally (return logvar )
             )
       (eq (length (inside c1)) (length (inside c2)))
       )
  )



(defmethod regroupables ((c1 t) (c2 t ) ) () )

(defmethod consecutifs ((c1 simple-container) (c2 simple-container))
  "Rend vrai lorsque c2 suit immediatement c1"
  (eq (offset c2) (+ (offset c1) (extent c1)))
  )

(defmethod durees-compatibles ((c1 simple-container) (c2 simple-container))
   "Rend vrai si c1 et c2 ont des durÂes que l'ont peut regrouper"
  ;;(or (power-of-two-p (gcd (extent c1) (extent c2))) (eq (extent c1) (extent c2)))
   t
  )


;; get-smalest-duration ou gsd : recherche la durÂe de l'objet le plus court dans une 
;; structure. Sa taille donne un tactus maximal pour effectuer une quantification sans
;; perte. 

(defmethod get-smalest-duration (( self container)) (gsd self) )
(defmethod gsd ((self container) )
  (apply #'min (cons  (/ (extent self)  (qvalue self) ) 
                      (loop for item in (inside self) collect (gsd item) )))         
  )
(defmethod gsd ((self chord))  (/ (extent self) (qvalue self)) )
(defmethod gsd ((self simple-container)) (/ (extent self) (qvalue self)) )

