;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;Simple-container and container classes are defined in this file.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)
;==========================================================================
; OpenMusic : Container.Lisp
;==========================================================================




;==========================================================================
;    Class definitions
;==========================================================================

(defvar *container-context* ())
 
(defclas simple-container ()
  ((parent :initform nil)
   (Qvalue :initform 1)
   (QTempo :initform 60)
   (offset :initform 0)   
   (extent :initform 1)
   (mus-const :initform nil)  ;may be it does nothing
   (mus-patch :initform nil)
   (mus-color :initform *om-black-color*)
   (tuplet-info :initform ())
   (extra-obj-list :initform nil))
  
  (:documentation "Defines atomic musical objects, and is a superclass for container, which defines compound musical objects. 
#enddoc#
#seealso# (container) #seealso#
#parent# a (super) container to which the simple-container belongs (e.g. a note to a chord) #parent#
#qvalue# An integer. 1/qvalue defines a rational fraction of the quarter note. This is the abstract time unit in which the extent is expressed #Qvalue#
#qtempo# An integer. a tempo expressed as a frequency of the quarter note per minute. Gives an interpretation of Qvalue in the physical time #qtempo#
#offset# An integer. time offset of the simple-container w/regard to the super-container it belongs eventually to. Expressed in the qvalue of the super-container #offset#
#extent# An integer. duration of the simple-container. Expressed in the qvalue of the simple-container #offset#
"))

(defclas container (simple-container)
  ((inside :initform nil))
  
  (:documentation "Defines compound musical objects. 
A container contains a list of either either instances of simple-container, or instances of container.
So a container is really a tree where nodes are containers and leaves are simple-containers.
As a container is also a simple-container, it has its own parent, qvalue, qtempo, offset and extent.
The qvalue of a container defines an abstract time unit in which the offsets of all its subcontainers are expressed,
and in which its own extent is expressed. So, each subcontainer in a container sets up its own time scale for itself and its childs.

#enddoc#
#seealso# (simple-container) #seealso#
#inside# a list of containers or simple-containers #inside#
") )

(defmethod set-mus-color ((self container) color)
  (setf (mus-color self) color)
  (loop for item in (inside self) do (set-mus-color item color)))

(defmethod set-mus-color ((self simple-container) color)
  (setf (mus-color self) color))

(defmethod get-mus-color ((self simple-container))
  (mus-color self))



;==========================================================================
;   INTERFACE
;==========================================================================


#|
(defmacro InContext (container &body body)
  "A call to (extent obj) inside <body> yields a value converted to the
QValue of <container>. A call to (setf (container obj) value) considers
value is expressed in container's QValue and changes value and (QValue obj)
acordingly")
 
(defmethod creduce ((self container) &key (key 'extent) (accum 1) (op #'lcm))
  "Compute an accumulated value over a certain slot of all sub-containers of <self>. 
Preorder recursive traversal.")

(defmethod Integerize ((self container))
  "Sets all offset and extent slots to be integer. Changes Qvalue slot accordingly.
Attention ne marche pas encore dans le cas general, mais uniquement pour tree->container." )

(defmethod SetQValue ((self container) (QValue integer) &key (recursive t))
  "Changes the QValue slot (must be a multiple of previous). 
Changes extent and offset slot accordingly. If <recursive> is T, changes through all the tree.
Attention : le cas recursif n'est pas evident, et devrait verifier la compatibilite de l'ancien
et du nouveau QValue" )

(defun cLcm (&rest containers)
 "Computes lcm over the qvalues of a series of containers")

(defmethod QNormalize ((self container))
  "Sets the QValue at each level to optimal, that is minimal w/ regard to the inside" )

(defmethod QReduce ((self container) &key  (with 1))
  "Sets the QValues at each level to the lcm of all the structure. Uses cLcm.
If (:with n) reduces the QValues to the lcm of n and the lcm of all the structure." )

(defmethod Extent->ms ((self simple-container))
  "Yields the extent of <self> converted into milliseconds")

(defmethod offset->ms ((self simple-container))
  "Yields the offset of <self> converted into milliseconds")

(defmethod copy-container ((self simple-container) )
   "Copies recursively a container")


(defmethod first-container ((self container) (type list) &key (reverse nil))
  "Searches recursively a container for the first container whose type is in <type>. If
<reverse>, searches backwards."    

(defmethod next-container ((self simple-container) (type list)) 
  "Starting from a container in a super container, searches for the next right occurence of
a container whose type is in <type>"

(defmethod previous-container ((self simple-container) (type list)) 
  "Starting from a container in a super container, searches for the next left occurence of
a container whose type is in <type>"

(defmethod brothers ((self simple-container) &key (direction :right)) 
"Gets the list of brothers (:right or :left), not including <self>."


; (defmethod previous-simple-container  ((self simple-container))
; recherche dans une structure le simple-container précédent (de type note, silence ou accord)

; (defmethod next-simple-container ((self simple-container))
; recherche dans une structure le simple-container suivant (de type note, silence ou accord)

; (defmethod first-simple-container ((self simple-container)) self )
; (defmethod first-simple-container ((self chord )) self )
; (defmethod first-simple-container ((self container))
;;; premiere feuille d'une structure hiérarchique

; (defmethod last-simple-container ((self container ))
; (defmethod last-simple-container ((self simple-container)) self )
; (defmethod last-simple-container ((self chord )) self  )
;;; derniere feuille d'une structure hierarchique

; (defmethod replace-simple-container ((object simple-container) (destination simple-container))
; remplace une structure <destiantion> par une autre <object>
; <object> est redimensionnée a la taille intiale de <destination> par un strech )



|#


;==========================================================================
;    Special access to slots. Redefinitions of slots
;==========================================================================

(defmacro InContext (container &body body)
  "A call to (extent obj) inside <body> yields a value converted to the
QValue of <container>. A call to (setf (container obj) value) considers
value is expressed in container's QValue and changes value and (QValue obj)
acordingly"
  `(let ((*container-context* ,container))
     ,. body ))


;; the <extent> field of a container may be accessed in the context of a 
;; super-container. See macro Incontext.
(defmethod extent ((self simple-container))
  (if (or (null *container-context*) (eq *container-context* self))
      (slot-value self 'extent)
    ;;;;(round ;; ??
     (* (/ (QValue *container-context*) (Qvalue self)) 
        (slot-value self 'extent))))

;; The extent field of a container can be set to a value that is expressed
;; in the QValue of a supercontainer. See macro InContext
(defmethod (setf extent) ((value number) (self simple-container))
  (if (or (null *container-context*) (eq *container-context* self))
    (setf (slot-value self 'extent) value)
    (progn (SetQValue self  (QValue *container-context*) :recursive nil)
           (setf (slot-value self 'extent) 
                 (* (QValue self) (/ value (QValue *container-context*)))))))


(defmethod (setf inside) :after ((inside list) (self container)) 
  (loop for object in inside
        do (setf (parent object) self))
  inside)

;==========================================================================
;    container and simple-container methods
;==========================================================================

(defmethod initialize-instance :after ((self container) &rest initargs)
  (declare (ignore initargs))
  (loop for object in (inside self)
        do (setf (parent object) self))
  self)
        



;;; Utilitaires

(defmethod container-p ((self container)) t)
(defmethod container-p ((self t )) nil )

(defmethod simple-container-p ((self simple-container )) (not (container-p self)))
(defmethod simple-container-p ((self t)) nil)

(defmethod NbElements ((self container))
  (length (inside self)))

#|
(defmethod print-object ((self simple-container) x) 
  (format x "QValue: ~D offset: ~D extent: ~D~%" 
          (QValue self)
          (offset self)
          (extent self)))

(defmethod print-object ((self container) x) 
  (format x "-----")
  (call-next-method)
  (loop for sub in (inside self)
        do (print-object sub t)))

|#



(defmethod creduce ((self container) &key (key 'extent) (accum 1) (op #'lcm))
  "Compute an accumulated value over a certain slot of all sub-containers of <self>. 
Keywords are :key for the slot name, :accum for the initialization value, a,d :op for the binary function to be applied.
Preorder recursive traversal."
  (let ((acc (funcall op accum (funcall key self))))
    (loop
      for sub in (inside self)
      do (setf acc (creduce sub :key key :accum acc :op op))
;;;      finally return acc)))
      finally (return acc))))



(defmethod creduce ((self simple-container) &key (key 'extent) (accum 1) (op #'lcm))
""
  (funcall op accum (funcall key self)))


(defmethod Integerize ((self container))
  "Sets all offset and extent slots to be integer. Changes Qvalue slot accordingly.
Does not work yet in the general case, works when called from tree->container"
  (setf (QValue self) 
        (reduce  #'(lambda (x y) (lcm  x (denominator y)))
                 (inside self) :key 'extent :initial-value 1))
  (setf (extent self) (* (extent self) (Qvalue self)))
  (loop for sub in (inside self)
        do (setf (offset sub) (* (offset sub) (Qvalue self)))
        (Integerize sub))
  self)
(defmethod Integerize ((self simple-container)) 
""
  (setf (extent self) (/ (extent self) (QValue self))
        (QValue self) (denominator (extent self))
        (extent self) (numerator (extent self)))
  self)

(defmethod SetQValue ((self container) (QValue integer) &key (recursive t))
  "Changes the QValue slot to the lcm of previous QValue and new QValue.
Changes extent and offset slot accordingly. If <recursive> is T, changes the qvalue slot through all the container tree."
  (let ((QRatio (/ (lcm QValue (QValue self)) (QValue self))))
    (setf (QValue self) (lcm QValue (QValue self))
          (slot-value self 'extent) (* (slot-value self 'extent) QRatio))
    (loop for sub in (inside self)
          do (setf (offset sub) (* (offset sub) QRatio))
          when recursive do (SetQValue sub QValue :recursive t))
    self))

(defmethod SetQValue ((self simple-container) (QValue integer)  &key (recursive t))
""
  (declare (ignore recursive))
  (let ((QRatio (/ (lcm QValue (QValue self)) (QValue self))))
    (setf (slot-value self 'extent) (* (slot-value self 'extent) QRatio))
    (setf (QValue self) (lcm QValue (QValue self)))
    self))

;;; this replaces fraction minimale and fraction-minimale-commune 
(defun cLcm (&rest containers)
 "Computes lcm over the qvalues of a list of containers"
  (reduce  #'(lambda (c1 c2) (lcm c1 (creduce c2 :key 'QValue :accum 1 :op #'lcm)))
           containers :initial-value 1))

;;; replaced  "finally do" by  "finally"
(defmethod QNormalize ((self container))
  "Sets the QValue slot in self and in each of its subcontainers to optimal, 
i.e. the smallest possible integer number, considering the offset and extent values
that have to be expressed  as integer quantities of 1/QValue."
  ;(print (list self (QValue self) (Extent self)))
  (loop with gcd = (gcd  (QValue self) (Extent self)
                        (reduce  #'gcd (inside self) :key 'offset :initial-value 0))
      for sub in (inside self) do 
        (setf (offset sub) (/ (offset sub) gcd))
        (QNormalize sub)
      finally (setf (QValue self) (/ (QValue self) gcd)
                (extent self) (/ (extent self) gcd)))
  self)

(defmethod QNormalize ((self simple-container))
""
  (let ((gcd (gcd (QValue self) (extent self))))
    (setf (QValue self) (/ (QValue self) gcd)
          (extent self) (/ (extent self) gcd))
    self))


(defmethod QReduce ((self container) &key  (with 1))
  "Changes the QValue at each node of the container tree to the lcm of all the structure.
If (:with n) reduces the QValues to the lcm of n and the lcm of all the structure.
Is used to make 2 containers comparable w/regard to time scale."
  (setQValue self (lcm with (cLcm self)) :recursive t)
  self)


(defmethod Extent->ms ((self simple-container))
  "Converts the extent of <self> to milliseconds"
  (round (* 1000 (/ 60 (Qtempo self)) (/ (extent self) (QValue self)))))



(defmethod offset->ms ((self simple-container) &optional grandparent)
  "Converts the offset of <self> to milliseconds. The offset is defined w/regard to the parent container.
If optional <grandparent> is given, the offset will be considered w/regard to the given grandparent."
  (let ((limit (if (null grandparent) (parent self) grandparent)))
    (round (loop for current = self then (parent current)
                 for father = (parent current)
                 until (or (null current) (eq current limit))
                 when (null father) do (setf father current)
                 sum
                 (* 1000.0 (/ 60.0 (Qtempo father)) (/ (offset current) (QValue father)))))))


(defmethod extent->rh ((self simple-container) &key (parent nil))
  (when (and parent (parent self))
    (setf self (parent self)))
  (/ (extent self) (QValue self)))


(defmethod set-extent-ms ((self simple-container) (durms integer))
  (setqvalue self 1000)
  (setf (slot-value self 'extent) (round (* (/ (Qtempo self) 60) durms))))


(defmethod copy-container ((self simple-container) &optional (pere ()))
   "Builds a deep copy of a container"
   (let ((copy (make-instance (type-of self)))
         (slots (class-instance-slots (class-of self))))
     (setf (slot-value copy 'parent) pere)
     (loop for slot in slots
           when (not (eq (slot-definition-name slot) 'parent))
           do        
           (setf (slot-value copy (slot-definition-name slot))
                  (copy-container (slot-value self (slot-definition-name slot)) copy))
           )
     ;;;
     (when (and *om-tonalite* (tonal-object-p copy))
       (actualise-tonalite copy))
     ;;;
     copy))



(defmethod copy-container ((self list) &optional (pere ()))
  (mapcar #'(lambda (X) (copy-container x pere)) self))

(defmethod copy-container ((self T) &optional (pere ())) self)


;(defmethod copy-container ((self cons) &optional (pere ()))
;  (loop for item in self collect (copy-container item pere)))

(defmethod copy-container ((self cons) &optional (pere ()))
  (if (listp (cdr self))
      (loop for item in self collect (copy-container item pere))
    (cons (copy-container (first self) pere) (copy-container (rest self) pere))))


(defmethod first-container ((self simple-container) (type list) &key (reverse nil))
  "Browse through the sub containers tree in <self> and yields the first subcontainer of type <type>.
if (:reverse T), the browsing order is reversed. See also next-container."
  (some #'(lambda (type) (and (subtypep (type-of self) type) self)) type))

(defmethod first-container ((self container) (type list) &key (reverse nil))
  (or (call-next-method)
      (some #'(lambda (child) (first-container child type))
            (if reverse (reverse (inside self)) (inside self)))))
      
(defmethod next-container ((self simple-container) (type list)) 
  "Yields the sub-container of type <type> which is next to <self> in the super-container. Order is top-down, left-right. See also first-container."
  (or (some #'(lambda (brother) (first-container brother type)) (brothers self :direction :right))
      (and (parent self)
           (next-container (parent self) type))))

(defmethod brothers ((self simple-container) &key (direction :right)) 
  "Yields the list of right brothers of <self> in the direct super-container. If (:direction :left), yields the left brothers."
  (let ((p (and (parent self) (position self (inside (parent self))))))
        (and ;(parent self)
             p
             (case direction
               (:right (nthcdr (1+ p) (inside (parent self))))
               (:left (reverse (subseq (inside (parent self))  0  p)))))
        ))


(defmethod previous-container ((self simple-container) (type list)) 
  "Yields the sub-container previous to <self> and of type <type> in the super-container. Order is top-down, left-right. See also first-container."
  (or (some #'(lambda (brother) (first-container brother type :reverse t)) (brothers self :direction :left))
      (and (parent self)
           (previous-container (parent self) type))))



;;; feuille precedente d'une structure hierarchique

(defmethod previous-simple-container ((self simple-container))
   "Similar to previous-container, but used to browse through the leaves of the container tree"
  (if (parent self )
    (let ((self-position (position self (inside (parent self)) ) ))
      (if (eq self-position  0 )
        (previous-simple-container (parent self))
        (last-simple-container (nth (- self-position 1 ) (inside (parent self))  ))
        )
      )
    ()
    )
  )

;;; feuille suivante d'une structure hierarchique

(defmethod next-simple-container ((self simple-container))
   "Similar to next-container, but used to browse through the leaves of the container tree"
  (if (parent self )
    (let ((self-position (position self (inside (parent self)) ) ))
      (if (eq (length (inside (parent self))) (+ self-position 1 ))
        (next-simple-container (parent self))
        (first-simple-container (nth (+ self-position 1 ) (inside (parent self)) ))
        )
      )
    ()
    )
  )
        


;;; premiere feuille d'une structure hierarchique
(defmethod first-simple-container ((self simple-container)) 
   "Similar to first-container, but used to browse through the leaves of the container tree"
   self )

(defmethod first-simple-container ((self container))
  (if (inside self) (first-simple-container (first (inside self))) ()) 
  )

(defmethod last-simple-container ((self simple-container))    
   "Last leave of the container tree"
             self )

;;; derniere feuille d'une structure hierarchique
(defmethod last-simple-container ((self container ))
  (if (inside self)  (last-simple-container (car (last (inside self))))  ()  )
  )




;; remplace une structure par une autre (l'autre est redimensionnée a la taille de la premiere par un strech )
(defmethod replace-simple-container ((object simple-container) (destination simple-container))
   (if (parent destination )
     (let ((strech-divisor (gcd (* (qvalue object) (extent destination)) (* (qvalue destination) (extent object)) )))
       
       (let ((remplacement (strech object  
                                   ( / (* (qvalue object) (extent destination)) strech-divisor)
                                   ( / (* (qvalue destination) (extent object)) strech-divisor )
                                   )))
         (setf (parent remplacement ) (parent destination))
         (setf (offset remplacement ) (offset destination))
         (setf (inside (parent destination) )
               (sort (cons remplacement
                           (set-difference (inside (parent destination)) (list destination))) 
                     #'< :key #'offset )
               
               )
         remplacement
         )
       )
     ()
     )
   )

(defmethod flatten-container ((self container) (below t) (target-type symbol))
"Constructs a container of type <target-type>, with QValue 1000, where all subcontainers below (and including) the
class <below> are cloned and placed in a flat way. All offsets in millisecs."
  (let* ((target (mki target-type :empty t))
         (objects (collect-subcontainers self below))
         (offsets (loop for object in objects  collect  (offset->ms object self))))
    (setf (inside target) (mapcar 'copy-container objects))
    (loop for object in (inside target) 
          for offset in offsets
          do (setf (offset object) offset))
    (setf (QVAlue target) 1000)
    (qnormalize target) 
    ;; GA 11/04/09
    (adjust-extent target)
    ;;
    target ))


(defmethod collect-subcontainers ((self container) (below t))
    (loop for object in (inside self)
                if (some #'(lambda (type) (subtypep (type-of object) type)) (list! below))
                collect   object
                else append (collect-subcontainers object below)))
          
(defmethod collect-subcontainers ((self simple-container) (below t)) nil)

;=============================
;IMPORT from mmodif aaa
;=============================
(defmethod omNG-copy ((self simple-container)) 
  `(copy-container ,self))

;;; replaced copy-instance with copy-container
(defmethod ot-clone ((self simple-container))
  (let ((object (copy-container self)))
    (setf (slot-value object 'parent) nil
          (slot-value object 'offset) 0
          (slot-value object 'QTempo) 60
          (slot-value object 'tuplet-info) nil)
    object))


(defmethod propagate-tempo ((Self simple-container)) self)
          
(defmethod ot-clone ((self container))
  (let ((object (call-next-method)))
    (setf (inside object) (mapcar 'copy-container (inside self)))
    (propagate-tempo object)
    object))

(defmethod propagate-tempo ((Self container))
  (loop for object in (inside self)
        do (setf (Qtempo object) (Qtempo self))
        (propagate-tempo object)))


;===================Maquette Interface


(defmethod real-duration ((self t) time)
  (values time (+ time (get-obj-dur self))))

(defmethod real-duration ((self container) time)
  (loop for sub in (inside self)
        with tmin = 0 and tmax = 0 ; (extent->ms self)
        do 
        (multiple-value-bind (begin end)  (real-duration sub (+ time (offset->ms sub)))
          (setf tmin (min tmin begin) tmax (max tmax end)))
        finally (return (values tmin tmax))))

(defmethod real-duration ((self simple-container) time)
  (values time (+ time (extent->ms self))))

(defmethod stretch-in-maq ((self simple-container) (factor number))
  (setf (Qtempo self) (round (Qtempo self) factor)))

(defmethod stretch-in-maq ((self container) (factor number))
  (setf (Qtempo self) (round (Qtempo self) factor))
  (loop for obj in (inside self) do (stretch-in-maq obj factor)))

(defmethod allow-strech-p ((self simple-container) (factor number))  factor)

(defmethod get-delta-offset ((self t))
  (abs (before-zero self 0 0)))

(defmethod before-zero ((self container) time before)
  (loop for sub in (inside self)
        do 
        (setf before (min before
                          (before-zero sub (+ time (offset->ms sub)) before))))
  before)

(defmethod before-zero ((self simple-container) time before)
  (declare (ignore before))
  time)

(defmethod real-dur ((self simple-container)) 
"Duration in ms of <self>, including events that could exist before the beginning or last after the end of <self>.
This is different from extent."
  (multiple-value-bind (begin end)  (real-duration self 0)
    (- end begin)))

(defmethod get-obj-dur ((self simple-container)) (real-dur self))


(defmethod allowed-in-maq-p ((self simple-container)) t)

(defmethod last-container ((self simple-container) (type list))
  (let ((previous  (first-container self type)))
    (and previous
         (loop 
           for next = (next-container previous type) then (next-container next type)
           while next
           do (setf previous next)
         finally (return previous))
         )))



;New from  gerard 01/05/2001

(defmethod set-tempo ((self container) (tempo integer))
  (let ((tempo-ratio (/ tempo (round (qtempo self)) )))
    (call-next-method)
    (loop for sub in (inside self)
          do (setf (offset sub) (* (offset sub) (numerator tempo-ratio)))
          (set-tempo sub tempo))
    self))

(defmethod set-tempo ((self simple-container) (tempo integer))
  (let ((tempo-ratio (/ tempo (round (qtempo self)) )))
    (setf (qtempo self) tempo
          (qvalue self) (* (qvalue self) (denominator tempo-ratio))
          (extent self) (* (extent self) (numerator tempo-ratio)))
    self))  

;==================extras new 09/04/04 

(defmethod get-all-extras ((self t)) nil)

(defmethod get-all-extras ((self simple-container))
  (extra-obj-list self))

(defmethod get-all-extras ((self container))
  (append (extra-obj-list self)
          (loop for item in (inside self)
              append (get-all-extras item))))