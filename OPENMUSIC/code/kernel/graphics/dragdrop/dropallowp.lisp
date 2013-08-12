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
;This File contains a set of predicats (methods) that allow or forbiden to drag
;a source object to a target one.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;------------Slot target
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target OMSlot))  
   (if (protected-p (find-class (classname target)))
     (om-beep-msg "This slot is protected, you can not change its type")
     t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxClass) (target OMSlot))  
   (if (protected-p (find-class (classname target)))
     (om-beep-msg "This slot is protected, you can not change its type")
     t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBasicType) (target OMSlot))
   (if (protected-p (find-class (classname target)))
     (om-beep-msg "This slot is protected, you can not change its type")
     t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMInstance) (target OMSlot))
   (if (protected-p (find-class (classname target)))
     (om-beep-msg "This slot is protected, you can not change its type")
     t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxinstance) (target OMSlot))
   (if (protected-p (find-class (classname target)))
     (om-beep-msg "This slot is protected, you can not change its type")
     t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMSlot) (target OMSlot))
   (if (protected-p (find-class (classname target)))
     (om-beep-msg "This slot is protected, you can not change its type")
     t))



;------------Patch target
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged TemporalBox) (target OMPatch)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target OMPatch)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMGenericFunction) (target OMPatch)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMLispFun) (target OMPatch))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMethod) (target OMPatch))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBasictype) (target OMPatch))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target OMPatch))  t)


(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxCall) (target OMPatch))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMSlot) (target OMPatch))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMInstance) (target OMPatch))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxClass) (target OMPatch))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxAlias) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMIn) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMOut) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMTypedIn) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged loopIterators) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMfinalDo) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMLoopDo) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged acumboxes) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged temp-marker) (target OMPatch))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged temp-marker) (target OMGenericFunction))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged temp-marker) (target OMMethod))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged temp-marker) (target OMMaquette))  nil)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged loopIterators) (target looppanel))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged acumboxes) (target looppanel))  t)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged temporalbox) (target OMWorkSpace)) 
  (or
   (equal (type-of (reference dragged)) 'OMPatchAbs)
   (equal (type-of (reference dragged)) 'OMMaqAbs)))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged temporalbox) (target OMFolder)) 
  (or
   (equal (type-of (reference dragged)) 'OMPatchAbs)
   (equal (type-of (reference dragged)) 'OMMaqAbs)))



;------------Maquette target
;;; est ce que self est un parent de container?
(defmethod maq-ancestor-p ((self OMPatch) (container OMPatch) checkedlist)
  (if (member self checkedlist :test 'equal)
    nil
    (let ((rep nil))
      (push self checkedlist)
      (cond
       ((eq self container) (setf rep t))
       (t (loop for item in (get-elements self) 
                while (not rep) do
                (when (maq-ancestor-p item container checkedlist)
                  (setf rep t))))
       )
      rep)))

(defmethod maq-ancestor-p ((self t) (container t) checkedlist) nil)

(defmethod maq-ancestor-p ((self TemporalBox) (container OMMaquette) checkedlist)
  (maq-ancestor-p (reference self) container checkedlist))

(defmethod maq-ancestor-p ((self OMBoxCall) (container OMPatch) checkedlist)
  (maq-ancestor-p (reference self) container checkedlist))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxCall) (target OMMaquette)) nil)

;;; autoriser pour les boxpatch boxmaquette boxabspatch boxabsmaquette
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxPatch) (target OMMaquette)) 
  (if (maq-ancestor-p dragged target nil)
      (om-beep-msg "You can not place a maquette into itself or its elements") t)
  )

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxMaquette) (target OMMaquette)) 
  (if (maq-ancestor-p dragged target nil)
     (om-beep-msg "You can not place a maquette into itself or its elements") t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMaquette) (target OMMaquette)) 
  (if (maq-ancestor-p dragged target nil)
     (om-beep-msg "You can not place a maquette into itself or its elements") t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target OMMaquette)) 
  (if (maq-ancestor-p dragged target nil)
     (om-beep-msg "You can not place a maquette into itself or its elements") t))


(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMaquette) (target OMPatch)) 
  (if (maq-ancestor-p dragged target nil)
     (om-beep-msg "You can not place a maquette into itself or its elements") t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxMaquette) (target OMPatch)) 
  (if (maq-ancestor-p dragged target nil)
     (om-beep-msg "You can not place a maquette into itself or its elements") t))



(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxEditCall) (target OMMaquette)) t)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxInstance) (target OMMaquette)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMGenericFunction) (target OMMaquette))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMLispFun) (target OMMaquette))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMethod) (target OMMaquette))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBasictype) (target OMMaquette))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged TemporalBox) (target OMMaquette))  t)


;-----------Typed Input target

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target OMTypedIn))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBasictype) (target OMTypedIn))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxEditCall) (target OMTypedIn))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxInstance) (target OMTypedIn))  t)



;------------Method target
;(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target OMMethod)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMGenericFunction) (target OMMethod))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBasictype) (target OMMethod))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target OMMethod))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMaquette) (target OMMethod))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMethod) (target OMMethod))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMboxcall) (target OMMethod))  t)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target OMMethod)) nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMboxpatch) (target OMMethod))  nil)

;-----------Folder target
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMFolder) (target OMFolder)) 
   (cond ((equal target dragged) nil)
         ((ancestor-p dragged target)
          (om-beep-msg "You can not place a folder into itself"))
         (t t)))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target OMFolder)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMaquette) (target OMFolder)) t)


;-----------globals folder target
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxInstance) (target OMglobalsFolder))
  (if (mypathname (reference dragged))
    (om-beep-msg "the object is already in the globals folder")
    t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMInstance) (target OMglobalsFolder)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target OMglobalsFolder)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMFolder) (target OMglobalsFolder))  nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target OMglobalsFolder)) nil)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMaquette) (target OMglobalsFolder)) nil)




;----------WS target

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMFolder) (target OMWorkSpace)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPatch) (target OMWorkSpace)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMMaquette) (target OMWorkSpace)) t)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxAbsPatch) (target OMWorkSpace)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxAbsmaq) (target OMWorkSpace)) t)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxAbsPatch) (target OMFolder)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxAbsmaq) (target OMFolder)) t)


;----------Class target

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target OMClass))
  (if (protected-p target)
    (om-beep-msg "This class is protected, you can not add slots to it")
    t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBasicType) (target OMClass))
  (if (protected-p target)
    (om-beep-msg "This class is protected, you can not add slots to it")
    t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMboxclass) (target OMClass))
  (if (protected-p target)
    (om-beep-msg "This class is protected, you can not add slots to it")
    t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMSlot) (target OMClass))
  (if (protected-p target)
    (om-beep-msg "This class is protected, you can not add slots to it")
    t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMInstance) (target OMClass))
  (if (protected-p target)
    (om-beep-msg "This class is protected, you can not add slots to it")
    t))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMboxInstance) (target OMClass))
  (if (protected-p target)
    (om-beep-msg "This class is protected, you can not add slots to it")
    t))

;---------------Package target-----------

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMPackage) (target OMPackage))
  (cond
   ((equal target *library-package*)
    (allow-make-lib-p dragged))
   ((or (protected-p target) (protected-p dragged))
    (om-beep-msg "Protected packages can not be moved or modified !"))
   (t (not (ancestor-p dragged target)))))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target OMPackage))
  (cond
   ((protected-p dragged) (om-beep-msg "This class is protected, you can not move it!"))
   ((protected-p target) (om-beep-msg "This package is protected, you can not add classes to it!"))
   (t t)))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxAlias) (target OMPackage))
  (if (OMBoxClass? (reference dragged))
    (if (protected-p target) 
      (om-beep-msg "This package is protected, you can not add classes to it")
      t)))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxClass) (target OMPackage))
  (om-beep-msg "Drag not allowed. You can drag a class from a suitcase to a suitcase."))
   
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMGenericFunction) (target OMPackage)) 
  (cond
   ((protected-p target) (om-beep-msg "This package is protected, you can not add functions to it!"))
   ((protected-p dragged) (om-beep-msg "This function is protected!"))
   (t t)))

;---------------LIB target-----------
(defun allow-make-lib-p (dragged) 
  (cond
   ((omlib-p dragged)  (om-beep-msg "Cannot make a lib from a lib !"))
   ((protected-p dragged) (om-beep-msg "Cannot make a lib out of a OM protected package!")) 
   (t t)))


;-----------------csp-------------

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxInstance) (target slot-icon-frame))
  (drop-allow-p D&DHandler (reference dragged) target))

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMInstance) (target slot-icon-frame))
  (if (protected-p (object target))
    (om-beep-msg "Protected class!")
    (if (check-type-p (thetype (object target)) (instance dragged)) t
        (om-beep-msg "Type mismatch!"))))


;-----------------Inputs default

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxInstance) (target OMin)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMInstance) (target OMin)) t)

(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxInstance) (target OMtypedin)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMInstance) (target OMtypedin)) t)



(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMClass) (target input-maker))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxClass) (target input-maker))  t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBasictype) (target input-maker)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxEditCall) (target input-maker)) t)
(defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged OMBoxInstance) (target input-maker)) t)




;=========================================================
; COPY PREDICATS
;=========================================================
(defmethod can-copy ((objtarget t) (objdragged t) )
  (list objtarget  objdragged) t)

(defmethod can-copy ((objtarget OMPatch) (objdragged OMIn) ) nil)
(defmethod can-copy ((objtarget OMPatch) (objdragged OMOut) ) nil)
(defmethod can-copy ((objtarget OMPatch) (objdragged OMBox)) t)
(defmethod can-copy ((objtarget OMPatch) (objdragged t)) nil)

(defmethod can-copy ((objtarget omworkspace) (objdragged omboxabspatch)) nil)


(defmethod can-copy ((objtarget OMglobalsFolder) (objdragged OMInstance)) t)

(defmethod can-copy ((objtarget OMglobalsFolder) (objdragged t)) nil)


(defmethod can-copy ((objtarget OMMaquette) (objdragged omboxeditcall)) nil)