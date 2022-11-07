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
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================

(in-package :om)

;;; ===============================================================================================
;;; ORGANISATION DES TUPLETS : ON MET A JOUR LE CHAMP TUPLET-INFO JUSQU'AUX FEUILLES
;;; ===============================================================================================

(defmethod tuplet-check ((self simple-container) &key (begin ()) (continue ()) (end ()) (status ()))
   (if (and (container-p self) (not (chord-p self)))
     (let ((tp (tuplet-p self))
           (td (tuplet-depth self)))
       (let (
             (my-begin     (append begin    (if tp (list (list 'begin td tp )) () )) )      
             (my-continue  (append continue (if tp (list (list 'continue td tp )) () )))
             (my-end       (append end      (if tp (list (list 'end td tp )) () )))
             (nb-items     (length (inside self)))
             )
         ;;; trois appels recursifs differents a tuplet-check suivant la position des containers interieurs
         ;;; le premier 
         
         (cond 
          ((= nb-items 1) (tuplet-check (car (inside self))
                                        :begin     my-begin
                                        :continue  my-continue
                                        :end       my-end
                                        :status status
                                        ))
          (t 
           (tuplet-check (car (inside self))
                         :begin     my-begin
                         :continue  my-continue
                         :end       my-continue
                         :status 'begin
                         )
           ;;; au milieu
           (loop for item in (cdr (inside self))
                 for ite2 in (cddr (inside self))
                 do (tuplet-check item
                                  :begin     my-continue
                                  :continue  my-continue
                                  :end       my-continue
                                  :status 'continue
                                  ))
           ;;; le dernier 
           (tuplet-check (car (last (inside self)))
                         :begin      my-continue
                         :continue   my-continue
                         :end        my-end
                         :status 'end
                         )
           )
          )
         )
       )
     (setf (tuplet-info self) 
           (cond 
            ((eq status 'begin ) begin )
            ((eq status 'continue) continue)
            ((eq status 'end ) end)
            )
           )
     )
   )


(defmethod tuplet-depth ((self container)) 
  (+ 
   (if (tuplet-p self) 1 0 )
   (apply #'max (loop for item in (inside self) collect (tuplet-depth item)))
   )
  )
    
(defmethod tuplet-depth ((self simple-container)) 0 )
(defmethod tuplet-depth ((self chord)) 0) 
(defmethod tuplet-depth ((self rest)) 0) 

;;; ================================================================================
;;; Reconnaitre si un container est une subdivision irreguliere 
;;; ================================================================================

;USED AND MADE BY KARIM
;------------------------------Om-etf----------------------------
(defmethod etf-tuplet-p ((self container) &optional (sub-cumulate 1) )
   (let ((mgcd (apply #'gcd 
                      (cons (extent self) (loop for item in (inside self) collect (offset item)) )
                      ) 
               ))
     (if (or  (power-of-two-p (/ (extent self) mgcd ))
              (power-of-two-p (/ (/ (qvalue self) (gcd mgcd (qvalue self)) ) sub-cumulate ) )
              (not (inside self))
              )
       ()
       (reduce-pow-two (/ (extent self) mgcd ) )
       ;;(/ (extent self) mgcd )
       )
     )
   )




(defmethod etf-tuplet-p ((self group) &optional (sub-cumulate 1)) (call-next-method  self ) )
(defmethod etf-tuplet-p ((self measure) &optional (sub-cumulate 1)) (call-next-method  self ) )
(defmethod etf-tuplet-p ((self voice) &optional (sub-cumulate 1) ) () )
(defmethod etf-tuplet-p ((self poly) &optional (sub-cumulate 1)) () )

(defmethod my-tuplet-p ((self container) &optional (sub-cumulate 1) )
   (let ((mgcd (apply #'gcd 
                      (cons (extent self) (loop for item in (inside self) collect (offset item)) )
                      ) 
               ))
     (if (or  (power-of-two-p (/ (extent self) mgcd ))
              (power-of-two-p (/ (/ (qvalue self) (gcd mgcd (qvalue self)) ) sub-cumulate ) )
              (not (inside self))
              )
       ()
       (/ (extent self) mgcd )
       )
     )
   )

(defmethod tuplet-p ((self container) &optional (sub-cumulate 1) )
   (let ((mgcd (apply #'gcd 
                      (cons (extent self) (loop for item in (inside self) collect (offset item)) )
                      ) 
               ))
     (if (or  (power-of-two-p (/ (extent self) mgcd ))
              (power-of-two-p (/ (/ (qvalue self) (gcd mgcd (qvalue self)) ) sub-cumulate ) )
              (not (inside self))
              )
       ()
       (reduce-pow-two (/ (extent self) mgcd ) )
       ;;(/ (extent self) mgcd )
       )
     )
   )



(defmethod tuplet-p ((self group) &optional (sub-cumulate 1)) (call-next-method  self ) )
(defmethod tuplet-p ((self measure) &optional (sub-cumulate 1)) (call-next-method  self ) )
(defmethod tuplet-p ((self voice) &optional (sub-cumulate 1) ) () )
(defmethod tuplet-p ((self poly) &optional (sub-cumulate 1)) () )


(defmethod contains-tuplet-p ((self simple-container)) () )

(defmethod contains-tuplet-p ((self chord)) () )

(defmethod contains-tuplet-p ((self container))
   (or  (tuplet-p self) 
        (loop for item in (inside self) thereis (contains-tuplet-p item) )
        )
   )
        
(defmethod power-of-two-p (( self integer))
  (if (or (eq self 1) (eq self 2))
    t
    (if (zerop (mod self 2)) (power-of-two-p (/ self 2)) () )
    )
  )


(defmethod reduce-pow-two ( ( self integer ) ) 
  (if (eq (mod self 2 ) 0) 
    (reduce-pow-two (/ self 2 ))
    self
    )
  )

;------------------------------Om-mxml----------------------------

(defmethod real-d-val ((obj measure)) 1)

(defmethod real-d-val ((obj group))
  (* 1/4 (/ (extent obj) (qvalue obj))))

(defmethod symb-d-val ((self measure))
  1)
  

(defmethod symb-d-val ((self group))
  (let* ((parent (parent self))
        (d-valp (symb-d-val parent))
        (d-valself (real-d-val self)))
    (if (= d-valp d-valself) d-valself
      (* d-valself (/ 1 d-valp)))))


(defmethod num-denom-val ((self group))
" Returns num/denum (ratio) of <self>"
  (let* ((symb-val (symb-d-val self))
         (num (get-group-ratio self)))
    (if num
        (let* ((denom (find-denom num symb-val))
              (den (if (listp denom) (list2ratio denom) denom)))
        (/ num den)) 1)))


(defmethod real-tuplet-p ((self t)) nil)

(defmethod real-tuplet-p ((self group))
  (let ((ratio (num-denom-val self)))
    (if (= 1 ratio) nil ratio)))

(defmethod real-tuplet-p ((self chord))
  (let ((parent (parent self)))
    (when parent 
      (real-tuplet-p parent))))

(defmethod real-tuplet-p ((self rest))
  (let ((parent (parent self)))
    (when parent 
      (real-tuplet-p parent))))



(defmethod first-of-group-p ((self t))
  nil)

(defmethod first-of-group-p ((self group))
  (let ((parent (parent self)))
    (if (first-of-group? self) parent)))

(defmethod first-of-group-p ((self chord))
  (let ((parent (parent self)))
    (if (first-of-group? self) parent)))

(defmethod first-of-group-p ((self rest))
  (let ((parent (parent self)))
    (if (first-of-group? self) parent)))



(defmethod first-last-of-group-p ((self t))
  nil)

(defmethod first-last-of-group-p ((self group))
  (let ((parent (parent self)))
    (if (or (last-of-group? self) (first-of-group? self)) parent)))

(defmethod first-last-of-group-p ((self chord))
  (let ((parent (parent self)))
    (if (or (last-of-group? self) (first-of-group? self)) parent)))

(defmethod first-last-of-group-p ((self rest))
  (let ((parent (parent self)))
    (if (or (last-of-group? self) (first-of-group? self)) parent)))



(defmethod get-all-groups ((self chord))
  "returns all groups of a chord which is first-of his-group"
  (let ((res (list (first-last-of-group-p self)))) 
    (loop while (car res)
           do (push (first-last-of-group-p (car (inside (parent (car res))))) res))
    (remove nil (reverse res))))

(defmethod get-all-groups ((self rest))
  "returns all groups of a chord which is first-of his-group"
  (let ((res (list (first-last-of-group-p self))))
    (loop while (car res)
          do (push (first-last-of-group-p (car (inside (parent (car res))))) res))
    (remove nil (reverse res))))

(defmethod real-group-p ((self t)) nil)
(defmethod real-group-p ((self group))
"returns true if the group is an irrational"
  (let ((ratio (get-group-ratio self)))
    (when ratio
    (if (= 1 ratio) nil ratio))))

(defmethod get-real-groups ((self t)) nil)

(defmethod get-real-groups ((self chord))
  (let ((groups (get-all-groups self)))
    (remove nil (loop for i in groups
            collect (if (and (or (equal self (car (flat (get-all-chords i))))
                                 (equal self (last-elem (flat (get-all-chords i)))))
                             (real-group-p i))
                             i)))))

(defmethod get-real-groups ((self rest))
  (let ((groups (get-all-groups self)))
    (remove nil (loop for i in groups
            collect (if (and (or (equal self (car (flat (get-all-chords i))))
                                 (equal self (last-elem (flat (get-all-chords i)))))
                             (real-group-p i))
                             i)))))

