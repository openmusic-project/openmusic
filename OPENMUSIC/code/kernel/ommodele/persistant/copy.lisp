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
;Methods for omng-copy generic function specialized for different objects.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


(defmethod om-copy-point (point)
  (when point
      `(om-make-point ,(om-point-h point) ,(om-point-v point))))

(defmethod copy-point (point)
  (when point
      (om-make-point (om-point-h point) (om-point-v point))))

(defun om-copy-point-list (list)
  (loop for item in list collect
        (om-copy-point item)))

(defun om-copy-color (color)
  (when color
      `(om-make-color ,(om-color-r color) ,(om-color-g color) ,(om-color-b color))))

(defun om-copy-font (font)
    `(om-make-font ,(om-font-face font) ,(om-font-size font) :family ,(om-font-family font) :style ',(om-font-style font) :mode ',(om-font-mode font)))



;------------------BASIC OBJECTS

(defmethod omNG-copy ((self OMFolder))
   (let ((obj (call-next-method)))
     `(let ((copy ,obj))
        (setf (elements copy) (list ,@(mapcar #'omNG-copy (elements self))))
        (set-icon-pos copy ,(om-copy-point (get-icon-pos self)))
        copy)))


(defmethod omNG-copy ((self OMInstance))
   (let* ((instance (instance self))
          (obj (call-next-method))
          (ed-params (copy-value-params instance self)))
     `(let ((copy ,obj))
        (setf (instance copy) (eval ,(omng-copy instance)))
        (set-icon-pos copy ,(om-copy-point (get-icon-pos self)))
        (setf (doc copy) ,(doc self))
        (setf (edition-params copy) ,ed-params)
        copy)))


(defmethod omNG-copy ((self OMlistInstance))
   `(let ((copy (make-instance ',(class-name (class-of self))
                            :name ,(name self)
                            :icon ,(icon self))))
        (setf (instance copy) ,(omng-copy (instance self)))
        (set-icon-pos copy ,(om-copy-point (get-icon-pos self)))
        copy))


(defmethod omNG-copy ((self OMPatch))
  (let ((obj (call-next-method)))
    `(let ((copy ,obj))
       (if ,(loaded? self)
         (progn
           (loop for item in (list ,.(reverse (mapcar #'omNG-copy (boxes self)))) do
                 (omng-add-element copy item))
           (copy-connections ',(boxes self) (boxes copy)))
         (setf (mypathname copy) ,(mypathname self)))
       (set-icon-pos copy ,(om-copy-point (get-icon-pos self)))
       (set-win-size copy ,(om-copy-point (get-win-size self)))
       (setf (omversion copy) ,(omversion self))
       (setf (pictu-list copy) ',(mapcar 'copy-picture (pictu-list self)))
       (setf (lisp-exp-p copy) ,(lisp-exp-p self))
       copy)))



(defmethod omNG-copy ((self OMMaquette))
  (let ((obj (call-next-method)))
    `(let ((copy ,obj))
       (copy-markers ',(boxes self) (boxes copy))
       (setf (eval-func copy) ,(omng-copy (eval-func self)))
       (setf (params copy) ,(omng-copy (params self)))
       (setf (pictu copy) ,(omng-copy (pictu self)))
       ;(when (thepict (pictu ,self))
       ;  (setf (thepict (pictu copy)) (thepict (pictu ,self)))
       ;  (setf (name (pictu copy)) (name (pictu ,self))))
       copy)))


(defmethod omNG-copy ((self Maquette-params))
  `(make-instance ',(class-name (class-of self))
    :maq-color ,(om-copy-color (maq-color self))
    :range ',(range self)
    :metricparam ',(metricparam self)
    :show-conect ,(show-conect self)
    :show-ruler-metric ,(show-ruler-metric self)))   


;----boxes

(defmethod update-boxes (oldbox newbox)
  (setf (value newbox) (eval (omNG-copy (value oldbox)))) 
  (setf (frame-position newbox) (borne-position (frame-position oldbox)))
  (setf (frame-size newbox) (frame-size oldbox))
  (setf (frame-name newbox) (frame-name oldbox))
  (setf (allow-lock newbox) (allow-lock oldbox))
  (setf (inputs newbox) (eval (omNG-copy (inputs oldbox))))
  (set-box-to-inputs (inputs newbox) newbox)
  newbox)

;PATCH INPUT
(defmethod omNG-copy ((self OMIn))
  `(let ((copy (make-instance ',(class-name (class-of self))
                 :name ,(name self)
                 :icon ,(copy-icon (icon self))
                 :reference nil
                 :indice ,(indice self))))
     (setf (frame-position copy) ,(om-copy-point (frame-position self)))
     (setf (frame-size copy) ,(om-copy-point (frame-size self)))
     (setf (frame-name copy) ,(frame-name self))
     (setf (docu copy) ,(docu self))
     (setf (defval copy) (put-quote ,(clone (defval self))))
     copy))


;PATCH OUTPUT

(defmethod omNG-copy ((self OMOut))
  `(let ((copy (make-new-output ,(name self) ,(indice self) ,(om-copy-point (frame-position self)) 
                                ,(copy-icon (icon self))',(class-name (class-of self)))))
     (setf (frame-name copy) ,(frame-name self))
     (setf (frame-size copy) ,(om-copy-point (frame-size self)))
     copy))


;NORMAL-INPUT without connections
(defmethod omNG-copy ((self input-funbox))
   `(make-instance ',(class-name (class-of self))
             :doc-string  ,(doc-string self)
             :name ,(name self)
             :value ,(omNG-copy (value self))))

(defmethod omNG-copy ((self input-funmenu))
  `(let ((newin (make-instance ',(class-name (class-of self))
                  :doc-string  ,(doc-string self)
                  :name ,(name self)
                  :value ,(omNG-copy (value self)))))
     (setf (thepopup newin) ,(omNG-copy (thepopup self)))
     newin))

(defmethod omNG-copy ((self input-keyword))
  `(let ((newin ,(call-next-method)))
     (setf (def-value newin) ,(omNG-copy (def-value self)))
     (setf (val-menu newin) ,(omNG-copy (val-menu self)))
     newin))




;;;;TEMPORAL PATCH OUTPUT

(defmethod omNG-copy ((self OMtempOut))
  `(let* ((theout (make-instance 'OMtempOut
                    :name ,(name self)
                    :icon ,(copy-icon (icon self))
                    :reference nil )))
     (setf (frame-position theout) ,(om-copy-point (frame-position self)))
     (setf (inputs theout) (list (make-instance 'input-funbox
                                   :name "out"
                                   :value nil
                                   :doc-string "out")))
     theout))


(defmethod omNG-copy ((self selftempin))
  `(let* ((thein (make-instance ',(class-name (class-of self))
                   :name ,(name self)
                   :icon ,(copy-icon (icon self))
                   :reference 'TemporalBox
                   :indice 0)))
          (setf (frame-position thein) ,(om-copy-point (frame-position self)))
          (setf (frame-size thein) ,(om-copy-point (frame-size self)))
          (setf (defval thein) (make-instance 'temporalbox))
     thein))


;GENFUN
(defmethod omNG-copy ((self OMBoxcall))
  `(let* ((copy ,(omNG-make-new-boxcall (fdefinition (reference self))
                                        (frame-position self)
                                        (name self))))
     (setf copy (update-boxes ,self copy))
     copy))

;BASICTYPE
(defmethod omNG-copy ((self OMBoxTypeCall))
  `(let ((copy ,(omNG-make-new-boxcall (get-basic-type (reference self))
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     (setf (thestring copy) ,(thestring self))
     copy))

;LISP
(defmethod omNG-copy ((self OMBoxlispCall))
  `(let ((copy ,(omNG-make-new-lispboxcall (reference self)
                                           (frame-position self) (name self))))
     (setf copy (update-boxes ,self copy))
     copy))


(defmethod omNG-copy ((self OMBoxEditCall))
 `(let ((copy (omNG-make-new-boxcall ,(reference self)
                                     ,(frame-position self)
                                     ,(name self))))
     (setf copy (update-boxes ,self copy))
     (setf (showpict copy) ,(showpict self))
     (setf (show-name copy) ,(show-name self))
     (setf (minieditor? copy) ,(minieditor? self))
     (setf (edition-params copy) ,(copy-value-params (value self) self)) ; (copy-alist ',(edition-params self)))
     copy))

;SLOTS
(defmethod omNG-copy ((self OMSlotsBox))
  `(let ((copy ,(omNG-make-new-boxcall-slots (reference self)
                                           (frame-position self)
                                           (name self))))
     (setf copy (update-boxes ,self copy))
      copy))


;INSTANCE
(defmethod omNG-copy ((self OMBoxInstance))
   (let ((global? (mypathname (reference self))))
     
     `(let ((copy ,(omNG-make-new-boxcall (if global? (reference self)
                                                                               (eval (omNG-copy (reference self))))
                                                                             (frame-position self)
                                                                             (name self))))
        (setf (frame-position copy) ,(om-copy-point (borne-position (frame-position self))))
        (setf (allow-lock copy) ,(allow-lock self))
        (setf (inputs copy) ,(omNG-copy (inputs self)))
        copy)))


;DEAD
(defmethod omNG-copy ((self general-box-dead))
  `(let ((copy ,(omNG-make-new-boxcall 'dead
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     (setf (numouts copy) ,(numouts self))
     (setf (mesage copy) ,(mesage self))
     (setf (save-code copy) ',(save-code self))
     copy))

;PATCH
(defmethod omNG-copy ((self OMBoxPatch))
  `(let ((copy ,(omNG-make-new-boxcall (reference self)
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     copy))

;MAQUETTE
(defmethod omNG-copy ((self OMBoxMaquette))
  `(let ((copy ,(omNG-make-new-boxcall (reference self)
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     ,(if (= (mode self) 1) `(change-mode copy 1 t))
     copy))


;PROGN
(defmethod omNG-copy ((self box-seq-call))
  `(let* ((copy ,(call-next-method)))
     (setf (numouts copy) ,(numouts self))
     copy))


;COMMENTS
(defmethod omNG-copy ((self OMBoxcomment))
  `(let ((copy ,(omNG-make-new-boxcall 'comment
                                       (frame-position self)
                                       (name self))))
     (setf copy (update-boxes ,self copy))
     (setf (reference copy) ,(reference self))
     (setf (textcolor copy) ,(om-copy-color (textcolor self)))
     (setf (textstyle copy) ,(om-copy-font (textstyle self)))
     copy))


;UNDEFINED
(defmethod omNG-copy ((self OMBoxundefined))
  `(let ((copy ,(omNG-make-new-boxcall 'undefined
                                       (frame-position self)
                                       (name self))))
     copy))


;TEMPORAL OBJECTS
(defmethod omNG-copy ((self TemporalBox)) 
   (let (newreference oldboxes)
     (if ;(or (Maquette-p (reference self)) 
       (have-persistant-patch? self)
       
       (setf newreference (reference self))
       (setf newreference (eval (omNG-copy (reference self)))))
     
     (when (patch-p newreference)
       (setf oldboxes (boxes (reference self))))
     `(let ((copy ,(omNG-make-tempobj newreference (om-make-big-point (slot-value self 'offset) 
                                                                   (posy self)) (name self))))
        (correct-boxes-maq ,newreference ',oldboxes)
        (setf (slot-value copy 'posy) ,(posy self))
        (setf (slot-value copy 'sizey) ,(sizey self))
        (setf (allow-lock copy) ,(allow-lock self))
        (setf (extend copy) ,(extend self))
        (setf (offset copy) ,(offset self))
        (setf (doc copy) ,(doc self))
        (setf (show-name copy) ,(show-name self))
        (setf (edition-params copy) ,(copy-edition-params self))
        (setf (free-store copy) (eval (omng-copy (free-store ,self))))
        (when (and (pictu ,self) (thepict (pictu ,self)))
          (setf (pictu copy) (copy-picture ,(pictu self))))
        (setf (slot-value copy 'strech-fact) ,(strech-fact self))
        (setf (colorframe copy) ,(om-copy-color (colorframe self)))
        (setf (inputs copy) ',(eval (omNG-copy (inputs self))))
        
        ;;; new pour sauver la valeur de la temporal box
        ;;; mais ca fait un pb entre value et reference pour les instances ...
        ;(when (value ,self)
        ;  (setf (value copy) (clone (value ,self))))
        
        (setf (mute copy) ,(mute self)
              (lock copy) ,(lock self))
        
        (when ,(abspatch-p newreference)
          (update-box-reference ,newreference copy)
          )
        copy)))

;MARKERS IN MAQ
(defmethod omNG-copy ((self temp-marker))
  `(let ((copy (make-instance 'temp-marker 
                  :name ,(name self)
                  :reference nil 
                  :icon 143
                  :offset ,(offset self)
                  :inputs nil)))
     (setf (frame-position copy) ,(om-copy-point (borne-position (frame-position self))))
     (setf (doc copy) (str-with-nl ,(str-without-nl (doc self))))
     copy))

 

(defmethod correct-boxes-maq ((self t) oldboxes) 
  (declare (ignore oldboxes)) nil)



;=====================================================
;DEFENSE DE COPIER
;=====================================================

(defmethod omNG-copy ((self omboxClass))
   (om-abort))

(defmethod omNG-copy ((self omboxAlias))
   (om-abort))

(defmethod omNG-copy ((self OMPackage))
   (om-abort))
     
;=====================================================
;copy forbidden
;=====================================================

(defmethod no-allow-copy-p ((self t))  
  "If non NIL self does not allow a copy"
  nil)
(defmethod no-allow-copy-p ((self OMClass)) "classes")
(defmethod no-allow-copy-p ((self OMGenericFunction)) "functions")
(defmethod no-allow-copy-p ((self OMLib)) "libraries")
(defmethod no-allow-copy-p ((self OMLispFun)) "lisp functions")
(defmethod no-allow-copy-p ((self OMMethod)) "methods")
(defmethod no-allow-copy-p ((self OMPackage)) "packages")
(defmethod no-allow-copy-p ((self OMSlot)) "slots")
(defmethod no-allow-copy-p ((self OMWorkSpace)) "workspaces")

(defmethod no-allow-copy-p ((self OMglobalsFolder)) "the globlas folder")

(defmethod no-allow-copy-p ((self OMIn)) "inputs")
(defmethod no-allow-copy-p ((self OMOut)) "outputs")
(defmethod no-allow-copy-p ((self OMTypedIn)) "method's inputs")

;=====================================================
;clone special for classes
;=====================================================
(defun copy-icon (icon)
   (cond
    ((numberp icon) icon)
    ((null icon) *default-icon*)
    ((listp icon)
     `(list ,(car icon) ,(second icon)))
    (t *default-icon*)))


(defmethod* objFromObjs ((self t) (type t))
  :icon 141
  :indoc '("Source object" "Object that specified the type.")
  :initvals '(nil nil)
  "Make un object of th same type as <type> from the object <self>."
  (cond
   ((subtypep (type-of self) (type-of type)) (clone self))
   (t nil)))
