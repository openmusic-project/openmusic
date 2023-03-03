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

;;; ===============================================================================================
;;;
;;;                                OPERATIONS RYTHMIQUES ELEMENTAIRES
;;;
;;;                                        OLIVIER DELERUE
;;;
;;; ===============================================================================================


;;;; PROVISOIRE !!!
(defun fraction-minimale (&rest x)
  (apply 'cLcm x))
(defun fraction-minimale-commune (&rest x)
  (apply 'cLcm x))
(defun DUPLIQUE-STRUCTURE-MUSICALE (&rest x)
  (apply 'copy-container x))
(defun reduit-qvalue (&rest x)
  (apply 'QNormalize x))
;;; ===============================================================================================
;;; ASSEMBLAGE DE STRUCTURES MUSICALES : on les met en parallle dans un objet vertical (poly).
;;; ===============================================================================================

(defmethod om-assemble ((s1 poly) (s2 poly))
   (let ((frac-min (fraction-minimale-commune s1 s2))
         (ss1 (duplique-structure-musicale s1))
         (ss2 (duplique-structure-musicale s2)))
     (change-qvalue ss1 frac-min)
     (change-qvalue ss2 frac-min)
     (mki'poly                
         :empty t
         :offset 0
         :qvalue frac-min
         :extent (max (extent ss1) (extent ss2))
         :inside (append (inside ss1) (inside ss2))
         )
     )
   )

(defmethod om-assemble ((s1 voice) (s2 poly))
   (let ((frac-min (fraction-minimale-commune s1 s2))
         (ss1 (duplique-structure-musicale s1))
         (ss2 (duplique-structure-musicale s2)))
     (setf (offset ss1) 0 )
     (change-qvalue ss1 frac-min)
     (change-qvalue ss2 frac-min)
     (setf (inside ss2) (append (inside ss2) (list ss1)))
     ss2
     )
   )


(defmethod om-assemble ((s1 poly) (s2 voice))
  (om-assemble s2 s1)
  )

(defmethod om-assemble ((s1 voice) (s2 voice))
   (let ((frac-min (fraction-minimale-commune s1 s2) )
         (ss1 (duplique-structure-musicale s1))
         (ss2 (duplique-structure-musicale s2)))
     (change-qvalue ss1 frac-min)
     (change-qvalue ss2 frac-min)
     (setf (offset ss1) 0)
     (setf (offset ss2) 0)
     (mki'poly
         :empty t
         :offset 0 
         :qvalue frac-min
         :extent (max (extent ss1) (extent ss2))
         :inside (list ss1 ss2)
         )
     )
   )



;;; ces methodes assemblent a l'interieur (et modifient) un poly existant.
(defmethod om-assemble-into-poly ((self poly) (s2 voice))
  (let ((frac-min (fraction-minimale-commune self s2))
        (ss2 (duplique-structure-musicale s2)))
    (change-qvalue self frac-min)
    (change-qvalue ss2 frac-min)
    (setf (slot-value self 'offset) 0
          (slot-value self 'qvalue) frac-min
          (slot-value self 'extent) (max (extent self) (extent ss2))
          (slot-value self 'inside) (append (inside self) (list ss2)))
    self))

(defmethod om-assemble-into-poly ((self poly) (s2 poly))
  (let ((frac-min (fraction-minimale-commune self s2))
        (ss2 (duplique-structure-musicale s2)))
    (change-qvalue self frac-min)
    (change-qvalue ss2 frac-min)
    (setf (slot-value self 'offset) 0
          (slot-value self 'qvalue) frac-min
          (slot-value self 'extent) (max (extent self) (extent ss2))
          (slot-value self 'inside) (append (inside self) (inside ss2)))
    self))



;;; ===============================================================================================
;;; MERGE DE STRUCTURES MUSICALES
;;; ===============================================================================================


;;; c'est la plus longue des deux structures qui est gardée 


(defmethod om-merge ((v1 voice) (v2 voice))
   (let ((frac-min (fraction-minimale-commune v1 v2)))
     (let ((new-fringe (fringe-or (get-fringe v1 :frac-min frac-min :w-rest ()) 
                                  (get-fringe v2 :frac-min frac-min :w-rest ()) ))
           (new-structure (smart-structure v1 v2))
           )
       (setf new-fringe (insert-rests new-fringe))
       (applique-liste-structure new-structure new-fringe)
       (automatic-chord-insert new-structure)

       (delete-useless-containers new-structure )
       (do-grouping new-structure )

       new-structure
       )
     )
   )


(defmethod om-merge ((v1 measure) (v2 measure))
   (voice->measure (om-merge (measure->voice v1) (measure->voice v2)) 0 )
   )



;;; ===============================================================================================
;;; MASQUAGE DE STRUCTURES MUSICALES
;;; ===============================================================================================



;;; v1 est la voix originale et v2 est le masque. 
;;; le resultat est la voix v1 masquée par v2.
;;; masque et demasque

(defmethod om-masque ((v1 voice) (v2 voice)  &key (mode 'masque))
  (let ((frac-min (fraction-minimale-commune v1 v2)))
     (let ((new-fringe (fringe-masque (get-fringe v1 :frac-min frac-min :w-rest ()) 
                                      (get-fringe v2 :frac-min frac-min :w-rest ()) 
                                      :mode mode )
                       )
           (new-structure (smart-structure v1 v2))
           )
       (applique-liste-structure new-structure new-fringe)
       (automatic-chord-insert new-structure)
       (blank-fill new-structure )
       (delete-useless-containers new-structure )
       (do-grouping new-structure )
       new-structure
       )
     )
  )


(defmethod get-fringe-activity ((fringe list))
   (if (cdr fringe )
     (let ((resultat (get-fringe-activity (cdr fringe))))
       
       (if (>= (+ (offset (car fringe)) (extent (car fringe))) (caar resultat))
         (cons (list (offset (car fringe)) (cadar resultat)) (cdr resultat))
         (if (= (offset (car fringe)) (caar resultat))
           resultat
           (cons (list  (offset (car fringe)) 
                        (+ (offset (car fringe)) (extent (car fringe))))
                 resultat )
           )
         )
       )
     (list (list (offset (car fringe))  (+ (offset (car fringe)) (extent (car fringe)))))
     )
   )


(defmethod get-fringe-inactivity ((fringe list))
   (append 
    (list (list 0 (offset (car fringe))))
    
    (loop for item1 in fringe
          for item2 in (cdr fringe ) 
          when (> (offset item2) (+ (offset item1) (extent item1)))
          collect (list (+ (offset item1) (extent item1)) (offset item2) )
          )
    (let ((last-item (car (last fringe))))
      (list (list (+ (offset last-item) (extent last-item)) 100000000)))
    )
   )
 
(defmethod fringe-masque ((fringe1 list) (fringe2 list) &key (mode 'masque))
   (let ((mask (if (eq mode 'masque ) 
                 (get-fringe-activity fringe2)
                 (if (eq mode 'demasque )
                   (get-fringe-inactivity fringe2)
                   ()
                   )
                 )))
     ;;(print mask )
     (loop for item in fringe1 append (masquage item mask ))
     )
   )

(defmethod masquage ((self note) (activity list) )
   (loop for item in activity 
         when (and (< (offset self) (cadr item))
                   (> (+ (offset self) (extent self)) (car item)) )
         
         collect (let ((new-note (copy-container self)))
                   (setf (offset new-note) (max (car item) (offset self)) )
                   (setf (extent new-note) (- (min (cadr item) (+ (extent self) (offset self)) ) 
                                              (max (car item) (offset self)) ) )
                   
                   new-note
                   )
         
         )
   )



;;; ===============================================================================================
;;; LEGATO
;;; ===============================================================================================


;; transformation legato : elle remplace les silences par une note liee a la precedente
;; legato modifie la structure et s'applique au premier simple-container de la structure
;; do-legato s'appplique a une structure complete, et rend une nouvelle structure.


(defmethod do-legato ((self simple-container))
  (let ((new-struct (Qreduce (duplique-structure-musicale self))))
    (legato (first-simple-container new-struct ))
    (delete-useless-containers new-struct)
    (do-grouping new-struct)
    (setf (tree new-struct) nil) ;; here to force recomputation of the tree
    new-struct
    )
  )


(defmethod legato ((self simple-container))
  (let ((next-one (next-simple-container self)))
    (if next-one
      (if (rest-p next-one)
        (let ((new-next-one (replace-simple-container self next-one ) ))

          (setf (tie new-next-one ) 
                (cond 
                 ((eq (tie self) 'nil ) 'end )
                 ((eq (tie self) 'begin ) 'continue )
                 ((eq (tie self) 'end ) 'end )
                 ((eq (tie self) 'continue ) 'continue )
                 )
                )
          (setf (tie self )
                (cond 
                 ((eq (tie self) 'nil ) 'begin )
                 ((eq (tie self) 'begin ) 'begin )
                 ((eq (tie self) 'end ) 'continue )
                 ((eq (tie self) 'continue ) 'continue )
                 )
                )
          (legato new-next-one)
          )
        (legato next-one)
        )
      ()
      )
    )
  )
  


(defmethod legato ((self chord))
  (let ((next-one (next-simple-container self)))
    (if next-one
      (if (rest-p next-one)
        (let ((new-next-one (replace-simple-container self next-one ) ))


          (loop for note in (inside self)
                do (progn
                     (setf (tie (corresponding-note new-next-one note ) ) 
                           (cond 
                            ((eq (tie note) 'nil ) 'end )
                            ((eq (tie note) 'begin ) 'continue )
                            ((eq (tie note) 'end ) 'end )
                            ((eq (tie note) 'continue ) 'continue )
                            )
                           )
                     (setf (tie note )
                           (cond 
                            ((eq (tie note) 'nil ) 'begin )
                            ((eq (tie note) 'begin ) 'begin )
                            ((eq (tie note) 'end ) 'continue )
                            ((eq (tie note) 'continue ) 'continue )
                            )
                           )
                     )
                )          
          (legato new-next-one)
          )
        (legato next-one)
        )
      ()
      )
    )
  )




;;; ===============================================================================================
;;; SPLIT TIME
;;; ===============================================================================================


(defmethod split-time ((self container)   tree )
   
  (let ((new-struct (duplique-structure-musicale self))
        (temp-group (make-instance 'group :tree tree)))
    (do-split-time (first-simple-container new-struct ) temp-group)
    (setf (tree new-struct) nil)
    new-struct
    )
  )


(defmethod splitit ((self container) (temp-group group))
  (let ((new-struct (duplique-structure-musicale self))
        )
    (do-split-time (first-simple-container new-struct ) temp-group)
    (setf (tree new-struct) nil)
    new-struct
    )
  )

(defmethod do-split-time  ((self note ) temp-group)
   (let ((new-simple-container (replace-simple-container 
                                (transpose temp-group (- (midic self) 6000 ) )
                                self) ))
     (if (next-simple-container new-simple-container)
       (do-split-time (next-simple-container new-simple-container) temp-group )
       ()
       )
     )
   )

(defmethod do-split-time  ((self rest) temp-group)
     (if (next-simple-container self)
       (do-split-time (next-simple-container self) temp-group )
       ()
       )
   )


;; il reste un probleme pour la transposition dans le cas de l'accord... 
;; j'ai mis un zero rapidement...
(defmethod do-split-time  ((self chord ) temp-group)
   (let ((new-simple-container (replace-simple-container 
                                ;; c'est ici qu'il faut faire quelque chose mais quoi ? 
                                (transpose temp-group (- (midic (car (inside self))) 6000 ) )
                                ;; (splitit temp-group self)
                                self) ))
     (if (next-simple-container new-simple-container)
       (do-split-time (next-simple-container new-simple-container) temp-group )
       ()
       )
     )
   )

;;; ===============================================================================================
;;; POUR REDUIRE UN POLY EN UNE SEULE VOICE
;;; ===============================================================================================

(defmethod om-merge-down ((self poly)) (merge-down-voices (inside self)) )

(defmethod merge-down-voices (list-of-voices) 
  (if (cdr list-of-voices)
    (om-merge (car list-of-voices) (merge-down-voices (cdr list-of-voices)))
    (car list-of-voices)
    )
  )


;;; on cherche une structure commune sans casser les groupes 
;;; pour eviter d'avoir des tuplets a cheval sur deux mesures 

(defmethod smart-structure ((v1 voice) (v2 voice))
   (let ((frac-min (fraction-minimale-commune v1 v2))
         (st1 (get-structure v1) )
         (st2 (get-structure v2) ))
     (change-qvalue st1 frac-min)
     (change-qvalue st2 frac-min)
     
     (let ((structure-longue (if (< (extent st1) (extent st2) ) st2 st1) )
           (structure-courte (if (< (extent st1) (extent st2) ) st1 st2) ))
       
       (loop for item in (tuplet-collection structure-courte)
             do (propage-subdivision structure-longue (car item) (cadr item)))
      
       ;(applique-liste-structure structure-longue (tuplet-collection-old structure-courte )) ;ancienne methode
       structure-longue
       )
     )
   )


(defmethod propage-subdivision ((self container) (sub integer) (c container)  &optional (running-offset 0) )
   (if (inside self)
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
               (loop for compteur from 1 to sub by ( / (extent self) sub ) 

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


(defmethod tuplet-collection ((self container) &optional (running-offset 0) )
   (let ((tp (tuplet-p self)))
     (if  tp 
       (let ((new-tup (copy-container self)))                                                      
         (setf (offset new-tup) (+ (offset self) running-offset))
         (setf (inside new-tup) ())
         (cons (list tp new-tup) (loop for item in (inside self) append (tuplet-collection item )))
         )
       (loop for item in (inside self) append (tuplet-collection item (+ running-offset (offset self))))
       )
     )
   )

(defmethod tuplet-collection ((self simple-container) &optional (running-offset 0) ) () )



(defmethod do-grouping ((self container))
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
  (if (chord-p (car item-list))
    (let ((new-extent (* (loop for sc in item-list sum (/ (extent sc ) (qvalue sc))) (qvalue (car item-list)))))
      (setf (extent (car item-list )) new-extent )
      (loop for note in (inside (car item-list))
            do (progn 
                 (setf (tie note) (tie-remplacement note (corresponding-note (car (last item-list)) note )))
                 (setf (extent note) new-extent )
                 )
            )
      (car item-list)
      )
    (let ((new-extent (* (loop for sc in item-list sum (/ (extent sc ) (qvalue sc)))  (qvalue (car item-list)))))
      (setf (extent (car item-list )) new-extent )
      (if (note-p (car item-list)) 
        (setf (tie (car item-list)) (tie-remplacement (car item-list) (car (last item-list)))) 
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

;(trace grouping-formated-list )

;; dans le predicat regroupables, on suppose que c2 suit c1. 

(defmethod regroupables ((c1 note) (c2 note))
  (and (= (offset c2) (+ (offset c1) (extent c1)))
       (or (eq (tie c1) 'begin ) (eq (tie c1) 'continue ) )
       (or (power-of-two-p (gcd (extent c1) (extent c2)))
           (eq (extent c1) (extent c2)))
       )
  )

(defmethod regroupables ((c1 rest) (c2 rest))
  (and (= (offset c2) (+ (offset c1) (extent c1)))
       (or (power-of-two-p (gcd (extent c1) (extent c2)))`
           (eq (extent c1) (extent c2)))
       )
  )

(defmethod regroupables ((c1 chord) (c2 chord ))
  (and (= (offset c2) (+ (offset c1) (extent c1)))
       (or (power-of-two-p (gcd (extent c1) (extent c2))) (eq (extent c1) (extent c2)))
       (loop for item in (inside c1)
             with logvar = t
             do (setf logvar (and logvar (or (eq (tie item) 'begin ) (eq (tie item) 'continue ) ) ))
             finally (return logvar )
             )
       (eq (length (inside c1)) (length (inside c2)))
       )
  )

(defmethod regroupables ((c1 t) (c2 t ) ) () )


  

(defmethod merge-first-two-voices ((self poly ))
   (voice->poly (om-merge (poly->voice self 0) (poly->voice self 1)))
   )



;;; ===============================================================================================
;;; CONCATENATION DE STRUCTURES MUSICALES
;;; 
;;; concat s'applique a des structures horizontales
;;;
;;; ===============================================================================================


(defmethod om-concat ((s1 measure) (s2 measure))
   (let ((frac-min (fraction-minimale-commune s1 s2))
         (ss1 (duplique-structure-musicale s1))
         (ss2 (duplique-structure-musicale s2)))
     (change-qvalue ss1 frac-min)
     (change-qvalue ss2 frac-min)
     (loop for item in (inside ss2) do (setf (offset item) (+ (offset item) (extent ss1))))
     (mki 'measure 
          :empty t
          :offset 0
          :extent (+ (extent ss1) (extent ss2))
          :qvalue frac-min
          :inside (append (inside ss1) (inside ss2))
          )
     )
   )

(defmethod om-concat ((s1 voice) (s2 voice))
   (let ((frac-min (fraction-minimale-commune s1 s2))
         (ss1 (duplique-structure-musicale s1))
         (ss2 (duplique-structure-musicale s2)))
     (change-qvalue ss1 frac-min)
     (change-qvalue ss2 frac-min)
     (loop for item in (inside ss2) do (setf (offset item) (+ (offset item) (extent ss1))))
     (mki 'voice 
          :empty t
          :offset 0
          :extent (+ (extent ss1) (extent ss2))
          :qvalue frac-min
          :inside (append (inside ss1) (inside ss2))
          )
     )
   )





;;; ===============================================================================================
;;; GET-STRUCTURE DUPLIQUE TOUTE LA STRUCTURE SAUF LES OBJETS TERMINAUX
;;; ===============================================================================================

(defmethod get-structure ((self container)  &optional (pere ()) )
   (let ((new-container (copy-container self))
         ;;; (tp (tuplet-p self))
         )
     (setf (parent new-container) pere)
     (setf (inside new-container) (loop for cont in (inside self) collect (get-structure cont new-container )  ) )
     new-container
     )
   )

(defmethod get-structure ((self metric-sequence)  &optional (pere ()) )
   (let ((sequence (call-next-method )))
     (setf (slot-value  sequence 'tree) nil)
     sequence
     ))


(defmethod get-structure ((self simple-container)  &optional (pere ()) )
   (mki 'group 
        :empty t
        :parent pere
        :offset (offset self)
        :extent (extent self)
        :qvalue (qvalue self)
        :inside ()
        )
   )

(defmethod get-structure ((self chord)  &optional (pere ()) )
   (mki 'group 
        :empty t
        :parent pere
        :offset (offset self)
        :extent (extent self)
        :qvalue (qvalue self)
        :inside ()
        )
   )





;;; n-list-intersection ne sert plus a rien, je crois...
(defun n-list-intersection (list-of-lists )
  (if (cdr list-of-lists )
    (intersection (car list-of-lists) (n-list-intersection (cdr list-of-lists)))
    (car list-of-lists)
    ) )

(defmethod applique-liste-structure ((self poly) liste-objets &optional (running-offset 0) )
  (loop for sous-structure in (inside self)
        for sous-liste in liste-objets
        do (applique-liste-structure sous-structure sous-liste running-offset)
        )
  self
  )


(defmethod applique-liste-structure ((self container) liste-objets &optional (running-offset 0))
   ;;; on commence par distribuer des elements aux eventuelles sous structures de self
  (let ((reste-liste
         (if (inside self)
               (loop for item in (inside self )
                     with sub-liste = liste-objets
                     when (container-p item)
                     do (setf sub-liste (applique-liste-structure item sub-liste (+ running-offset (offset self))) )
                     finally (return sub-liste)
                     )
           liste-objets
           )))
 ;;; il reste a prendre maintenant ce qui revient a self (ce qui n'a pas ete pris par une sous structure)
 ;;; b- veut dire begin et e- end... 
    (loop for item in reste-liste
          for b-item = (offset item)
          for e-item = (+ (offset item) (extent item))
          for b-self = (+ running-offset (offset self) )
          for e-self = (+ running-offset (offset self) (extent self))
          
          
          ; quand l'objet rentre completement on le garde 
          when (and (>= b-item b-self ) (<= e-item e-self ))
          do (progn
               (setf (offset item) (- (offset item) b-self ))
               (setf (inside self) (append  (inside self) (list item)))
               (setf (parent item) self)
               )
          
          ; quand l'objet ne rentre pas du tout on le rejette 
          when (or (>= b-item e-self ) (<= e-item b-self ))
          collect item into rejected-elements
          
          ; quand l'objet rentre a moitie (l'entree de <item> est dans le container <self> )
          when (and (>= b-item b-self ) (< b-item e-self ) (> e-item e-self))
          ; on rejette la moitie qui ne rentre pas
          collect (let ((apres (duplique-structure-musicale item)))
                    (setf (offset apres) e-self )
                    (setf (extent apres) (- e-item e-self))
                    (if (note-p item) (setf (tie apres) (get-new-tie (tie item) 'apres)) () )
                    apres ) into rejected-elements
          
          and do (progn ; et on prend la moitie qui rentre 
                   (setf (offset item) (- (offset item) (+ running-offset (offset self))) )
                   (setf (extent item) (- (extent self) (offset item) ))
                   (setf (inside self) (cons item (inside self) )) ;; la on prend l'element
                   (setf (parent item) self)
                   (if (note-p item) (setf (tie item) (get-new-tie (tie item) 'avant)) )
                   )
          
          ; quand l'objet rentre a moitie (la sortie de <item> est dans le container <self> )
          when (and (< b-item b-self ) (> e-item b-self ) (<= e-item e-self))
          ; on rejette la premiere moitie qui ne rentre pas
          collect (let ((avant (duplique-structure-musicale item)))
                    (setf (extent avant) (-  b-self b-item ))
                    (if (note-p item) (setf (tie avant) (get-new-tie (tie item) 'avant)) ())
                    avant ) into rejected-elements
          
          and do (progn ; et on prend la moitie qui rentre 
                   (setf (extent item) (- e-item b-self))
                   (setf (offset item) 0)
                   (setf (inside self) (cons item (inside self) )) ;; ici on prend l'element
                   (setf (parent item) self)
                   (if (note-p item) (setf (tie item) (get-new-tie (tie item) 'apres)) ())
                   )
          
          ; quand l'objet <item> est  a cheval sur <self> 
          when (and (< b-item b-self )  (> e-item e-self ) )
          collect (let ((avant (duplique-structure-musicale item)))
                    (setf (extent avant) (- b-self b-item))
                    (if (note-p item) (setf (tie avant) (get-new-tie (tie item) 'avant)) ())
                    avant ) into rejected-elements
          
          and collect (let ((apres (duplique-structure-musicale item)))
                        (setf (offset apres) e-self )
                        (setf (extent apres) (- e-item e-self ) )
                        (if (note-p item) (setf (tie apres) (get-new-tie (tie item) 'apres)) ())
                        apres ) into rejected-elements
          
          and do (progn ; et on prend la moitie qui rentre 
                   (setf (offset item) 0)
                   (setf (extent item) (extent self))
                   (setf (inside self) (cons item (inside self) )) ;; ici on prend l'element
                   (setf (parent item) self)
                   (if (note-p item) (setf (tie item) 'continue) ())
                   )
          
          finally (progn
                    (setf (inside self) (sort (inside self) #'< :key #'offset ))
                    (return rejected-elements)
                    )
          )
    )
  )


(defmethod get-new-tie (old-tie type )
     (if (eq type 'avant)
       (cond
        ((eq old-tie 'begin) 'begin)
        ((eq old-tie 'end) 'continue)
        ((eq old-tie 'continue) 'continue)
        (t 'begin ))
       (if (eq type 'apres)
         (cond
          ((eq old-tie 'begin) 'continue)
          ((eq old-tie 'end) 'end)
          ((eq old-tie 'continue) 'continue)
          (t 'end))
         ()
         )
       )
     )




;;; ===============================================================================================
;;; PARCOURT LA STRUCTURE ET INSERE UN ACCORD QUAND DEUX NOTES TOMBENT SIMULTANEMENT
;;; ===============================================================================================

(defmethod automatic-chord-insert ((self container))
  (let ((note-list (loop for item in (inside self) when (note-p item) collect item)))
    (if note-list
      (setf (inside self) (append (set-difference (inside self) note-list) ;; la structure
                                  (loop for item in (chord-formated-list note-list)
                                        when (eq (length item) 1)
                                        collect (car item)
                                        when (> (length item) 1)
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

;;; UNE BIDOUILLE NECESSAIRE ET IMPOSSIBLE A COMMENTER

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

;;; ===============================================================================================
;;; TRANSFORME UN ENSEMBLE DE NOTES SIMULTANEES EN UN ACCORD
;;; ===============================================================================================

(defmethod list->chord (list-of-notes)
  (let ((new-chord (mki 'chord :empty t)))
    (setf (inside new-chord) list-of-notes )
    (setf (qvalue new-chord) (qvalue (car list-of-notes)))
    (setf (extent new-chord) (extent (car list-of-notes)))
    (setf (offset new-chord) (offset (car list-of-notes)))
    (loop for item in list-of-notes
          do (setf (parent item) new-chord))
    new-chord
    )
  )
  




;;; ===============================================================================================
;;; INSERTION AUTOMATIQUE DES SILENCES
;;; *** NE SERT PLUS A RIEN : J'UTILISE LA FONCTION QUI SUIT
;;; SERA UTILE LORSQUE L'ON VOUDRA VERIFIER L'INTEGRITE D'UNE STRUCTURE
;;; ===============================================================================================

;;; n'est pas utilisee pour l'instant. Atention, elle n'ajoute pas de silence en debut et fin de container

(defmethod automatic-rest-insert ((self container))
   (let ((fringe (get-fringe self :frac-min (fraction-minimale self) :w-rest t))
         (frac-min (fraction-minimale self)))
     (change-qvalue self frac-min)
     (let ((new-rests (loop for i1 in fringe 
                            for i2 in (cdr fringe)
                            when (> (offset i2) (+ (offset i1) (extent i1)))
                            collect (mki 'rest 
                                         :offset (+ (offset i1) (extent i1))
                                         :extent (- (offset i2) (+ (offset i1) (extent i1)))
                                         :qvalue frac-min
                                         )
                            )))
       ;(print new-rests)
       ;(print self)
       (if new-rests
         (applique-liste-structure self new-rests )
         ()
         )
       )
     )
   )


;;; ===============================================================================================
;;; INSERTION AUTOMATIQUE DES SILENCES
;;; INSERE DES SILENCES DANS UNE FRANGE DE MANIERE A CE QU'ELLE SOIT PLEINE...
;;; PEUT ETRE DES PROBLEMES A LA FIN DE LA FRANGE...-> IMPLIQUE D'UTILISER EGALEMENT LA FONCTION PRECEDENTE
;;; ===============================================================================================

(defmethod insert-rests ((fringe list))
  (sort (append fringe
         (loop for n1 in fringe
               for n2 in (cdr fringe )
               when (> (offset n2) (+ (offset n1) (extent n1) ))
               collect (mki 'rest 
                            :offset (+ (offset n1) (extent n1))
                            :extent (- (offset n2) (+ (offset n1) (extent n1)))
                            :qvalue (qvalue n1)
                            )
               )
         ) #'< :key #'offset )
  )



;;; ===============================================================================================
;;; TRANSFORMATIONS RYTHMIQUES
;;; ===============================================================================================


(defmethod strech ((self container) (num integer) (denom integer) &optional parent)
   (let ((temp (copy-container self)))
           (setf (extent temp)  (* (extent self) num ) )
           (setf (Qvalue temp) (* (Qvalue self) denom ) )
           (setf (offset temp) (* (offset self) num ) )
           (setf (parent temp) (if parent parent ()) )   
           (setf (inside temp) (loop for item in (inside self) collect (strech item num denom temp)))
           temp
           )
   )

(defmethod strech ((self simple-container) (num integer) (denom integer) &optional parent )
  (let ((temp (copy-container self)))
           (setf (extent temp)  (* (extent self) num ) )
           (setf (Qvalue temp) (* (Qvalue self) denom ) )
           (setf (offset temp) (* (offset self) num ) )
           (setf (parent temp) (if parent parent ()) )    
           temp
           ))



(defmethod change-qvalue ((self simple-container) (new-qvalue integer) &optional (previous-qvalue 1))
  (let ((new-q (if (eq (mod new-qvalue (fraction-minimale self)) 0) new-qvalue (fraction-minimale self))))
    (if (container-p self)
      (loop for item in (inside self )
            do (change-qvalue item new-q (qvalue self) )
            )
      ()
      )
    (setf (offset self) (/ (* (offset self) new-q ) previous-qvalue ))
    (setf (extent self) (/ (* (extent self) new-q ) (qvalue self) ) )
    (setf (qvalue self) new-q)
    )
  )


;;; ===============================================================================================
;;; TRANSFORMATIONS SUR LES HAUTEURS
;;; ===============================================================================================

(defmethod transpose ((self simple-container) trans &optional (pere ()))
   (let ((temp-cont (copy-container self) ))
     (setf (parent temp-cont) pere )
     (if (container-p self)
       (setf (inside temp-cont)  (loop for item in (inside self) collect (transpose item trans self) ) )
       (if (eq (type-of temp-cont) 'note)
         (setf (midic temp-cont) (+ (midic temp-cont) trans ))
         ()
         ) 
       )
     temp-cont
     )
   )

;;; POUR L'INSTANT RANDOM-PITCH MODIFIE LA STRUCTURE AU LIEU DE LA DUPLIQUER.
(defmethod random-pitch ((self simple-container)) 
  (if (container-p self)
    (loop for item in (inside self) do (random-pitch item))
    (if (note-p self) (setf (midic self) (+ (* (om-random-value 15) 100) 6000)) () )
    )
  )


;;; ===============================================================================================
;;; QUANTIFICATION BRUTALE
;;; ===============================================================================================

(defmethod quantify ((self simple-container) (new-qvalue integer))
  (change-qvalue self (fraction-minimale self))
  (let ((resultat (quantify2 self new-qvalue) ))
    (reduit-qvalue resultat )
    resultat
    )
  )

(defmethod quantify2 ((self simple-container) (new-qvalue integer))
  (let ((temp-cont (copy-container self)))
    (setf (offset temp-cont) (round (/ (* (offset self) new-qvalue) (qvalue self))))
    (setf (extent temp-cont) (round (/ (* (extent self) new-qvalue) (qvalue self))))
    (setf (qvalue temp-cont) new-qvalue)
    (if (zerop (extent temp-cont))
      ()
      (if (container-p temp-cont)
        (progn 
          (setf (inside temp-cont) (loop for item in (inside self) collect (quantify2 item new-qvalue) ) )
          (if (inside temp-cont) temp-cont () )
          )
        temp-cont
        )
      )
    )
  )
 
