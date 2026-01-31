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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;DocFile
;Define graphic drag-and-drop actions.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

;===========================================================================
;Drag&Drop Graphic actions
;===========================================================================
;;; removed all receive-file methods


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged t) 
                           (target t) position) (declare (ignore position))  nil)

;-----------------Folder target------------

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged folder-icon-frame) 
                           (target FolderPanel) position)
   (omG-change-container dragged target position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged folder-icon-frame) 
                           (target folder-icon-frame) position)
   (omG-change-to-little-icon dragged target position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame) 
                           (target FolderPanel) position)
   (omG-change-container dragged target position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame) 
                           (target folder-icon-frame) position)
  (omG-change-to-little-icon dragged target position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquette-icon-frame) 
                           (target FolderPanel) position)
   (omG-change-container dragged target position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquette-icon-frame) 
                           (target folder-icon-frame) position)
   (omG-change-to-little-icon dragged target position) t)


;-----------------globals folder target------------

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                           (target globals-folder-icon) position)
   (declare (ignore position))
   (if (get-inst-from-globals (name dragged))
     (om-beep-msg "This name already exists !!!")
     (if (EditorFrame (object target))
       (perform-drop D&DHandler dragged (EditorFrame (object target)) (om-make-point 20 20))
       (progn
         (omg-add-element-in-icon target (reference (object dragged))) t))))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                           (target FolderPanel) position)
   (if (get-inst-from-globals (name dragged))
     (om-beep-msg "This name already exists !!!")
     (let* ((obj (reference (object dragged)))
            new-frame)
       (setf new-frame (make-icon-from-object  obj (om-point-h position) (om-point-v position) 1 1))
       (omg-add-element target new-frame)
       (omng-save obj)
       (push (object dragged) (attached-objs obj))  t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                           (target globalsFolderPanel) position)
   (if (get-inst-from-globals (name dragged))
     (om-beep-msg "This name already exists !!!")
     (let* ((obj (reference (object dragged)))
            new-frame)
       (setf new-frame (make-icon-from-object  obj (om-point-h position) (om-point-v position) 1 1))
       (omg-add-element target new-frame)
       (omng-save obj)
       (push (object dragged) (attached-objs obj))  t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                           (target FolderPanel) position)
   (let ((name (name (object dragged)))
         (obj (get-super-default-value (class-name (object dragged))))
         new-frame)
     (loop while (get-inst-from-globals name)
           for i = 1 then (incf i) do
           (setf name (format nil "~A~D" (name (object dragged)) i)))
     (setf obj (omNG-make-new-instance obj name))
     (setf new-frame (make-icon-from-object  obj (om-point-h position) (om-point-v position) 1 1))
     (omg-add-element target new-frame)
     (omng-save obj)
     t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                           (target globals-folder-icon) position)
   (declare (ignore position))
   (if (EditorFrame (object target))
     (perform-drop D&DHandler dragged (EditorFrame (object target)) (om-make-point 20 20))
     (let* ((name (name (object dragged)))
            (obj (get-super-default-value (class-name (object dragged)))))
       (loop while (get-inst-from-globals name)
             for i = 1 then (incf i) do
             (setf name (format nil "~A~D" (name (object dragged)) i)))
       (setf obj (omNG-make-new-instance obj name))
       (omg-add-element-in-icon target obj) t)))


;----------------Globals target------------


;-----------------WorkSpace target------------

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patchboxframe) 
                         (target WorkSpacePanel) position)
  (let* ((patch (reference (object dragged)))
         (newpatch (abs2patch patch (mk-unique-name target (name patch)) position))
         (new-frame (make-icon-from-object  newpatch (om-point-h position) (om-point-v position) 1 1)))
    (omg-add-element target new-frame)
    (externalize dragged newpatch)
    t))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquetteframe) 
                         (target WorkSpacePanel) position)
  (let* ((patch (reference (object dragged)))
         (newpatch (abs2maquette patch (mk-unique-name target (name patch)) position))
         (new-frame (make-icon-from-object  newpatch (om-point-h position) (om-point-v position) 1 1)))
     (omg-add-element target new-frame)
     (externalize dragged newpatch) t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patchboxframe) 
                         (target FolderPanel) position)
  (let* ((patch (reference (object dragged)))
         (newpatch (abs2patch patch (mk-unique-name target (name patch)) position))
         (new-frame (make-icon-from-object  newpatch (om-point-h position) (om-point-v position) 1 1)))
    (omg-add-element target new-frame)
    (externalize dragged newpatch) 
    t))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquetteframe) 
                         (target FolderPanel) position)
  (let* ((patch (reference (object dragged)))
         (newpatch (abs2maquette patch (mk-unique-name target (name patch)) position))
         (new-frame (make-icon-from-object  newpatch (om-point-h position) (om-point-v position) 1 1)))
     (omg-add-element target new-frame)
     (externalize dragged newpatch) t))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged folder-icon-frame) 
                         (target WorkSpacePanel) position)
  (omG-change-container dragged target position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame) 
                         (target WorkSpacePanel) position)
  (omG-change-container dragged target position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquette-icon-frame) 
                         (target WorkSpacePanel) position)
  (omG-change-container dragged target position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                         (target WorkSpacePanel) position)
  (let* ((obj (reference (object dragged)))
         (new-frame (make-icon-from-object  obj (om-point-h position) (om-point-v position) 1 1)))
    (omg-add-element target new-frame)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instance-icon-frame) 
                           (target WorkspacePanel) position)
   (omG-change-container dragged target position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                         (target WorkspacePanel) position)
  (let* ((instance (get-super-default-value (class-name (object dragged))))    ;aaa*
         (instanceobj (omNG-make-new-instance instance (mk-unique-name target (name dragged))
                                              position))
         (new-frame (make-icon-from-object  instanceobj (om-point-h position) (om-point-v position) 1 1)))
    (omg-add-element target new-frame)) t)


;-----------------Patch target------------

(defmethod make-func-from-obj ((self patchPanel) obj pos &optional name)
  (let ((theobj (object obj)))
    (omG-add-element self (make-frame-from-callobj 
                           (omNG-make-new-boxcall theobj  pos 
                                                  (or name (mk-unique-name self (name theobj))))))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame) 
                         (target patchPanel) position)
  (make-func-from-obj target dragged position) t)

;(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame) 
;                           (target patch-icon-frame) position)
;   (declare (ignore position))
;   (if (editorFrame (object target))
;     (make-func-from-obj (editorFrame (object target)) dragged (om-make-point 22 50))
;     (omnG-add-element (object target) (omNG-make-new-boxcall (object dragged)  
;                                                              (om-make-point 22 50) 
;                                                              (uniqueNameinpatch (name (object dragged))
;                                                                                 (object target)))))
;   t)
  
(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquette-icon-frame) 
                           (target patchPanel) position)
   (make-func-from-obj target dragged position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged genfun-icon-frame) 
                         (target patchPanel) position)
  (make-func-from-obj target dragged (om-mouse-position target)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged lispfun-icon-frame) 
                         (target patchPanel) position)
  (let ((theobj (object dragged)))
    (omG-add-element target (make-frame-from-callobj 
                           (omNG-make-new-lispboxcall (funname theobj)  position 
                                                  (mk-unique-name target (name theobj)))))) t)


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged type-icon-frame) 
                         (target patchPanel) position)
  (make-func-from-obj target dragged position) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instance-icon-frame) 
                         (target patchPanel) position)
  (make-func-from-obj target dragged position (name (object dragged))) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                           (target patchPanel) position)
   (let* ((theobj (object dragged))
          (new-name (name theobj)) newobj)
     (if (om-shift-key-p)
         (setf newobj (omNG-make-new-boxcall-slots theobj  (om-mouse-position target) (mk-unique-name target new-name)))
       (setf newobj (omNG-make-new-boxcall theobj  (om-mouse-position target) (mk-unique-name target new-name))))
     (omG-add-element target (make-frame-from-callobj newobj))
     t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classboxframe) 
                           (target patchPanel) position)
   (let* ((theobj (find-class (reference (object dragged))))
          (new-name (name theobj)) newobj)
     (if (om-shift-key-p)
       (setf newobj (omNG-make-new-boxcall-slots theobj  position (mk-unique-name target new-name)))
       (setf newobj (omNG-make-new-boxcall theobj  position (mk-unique-name target new-name))))
     (omG-add-element target (make-frame-from-callobj newobj))
     t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged slot-icon-frame) 
                           (target patchPanel) position)
   (let* ((typeslot (string (thetype (object dragged))))
          (basic? (find-if #'(lambda (x) 
                               (string-equal (class-name x) typeslot)) *Basic-Lisp-Types*)))
     (unless basic?
       (setf basic? (find-class (thetype (object dragged)))))   ;aaa*
     (omG-add-element target (make-frame-from-callobj 
                              (omNG-make-new-boxcall basic? position 
                                                     (mk-unique-name target (name basic?)))))
     t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged omboxframe) 
                           (target patchPanel) position)
   (omg-remove-element (om-view-container dragged) dragged)
   (let ((new (make-frame-from-callobj (object dragged))))
     (omG-add-element target new)
     (setf (frames (object new)) (list new))
     (om-set-view-position new position)
     (setf (frame-position (object new)) (borne-position position))
     t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged texteditorframe)
                           (target patchPanel) position)
  (omg-remove-element (om-view-container dragged) dragged))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged icon-method) 
                         (target patchPanel) position)
  (omG-add-element target (make-frame-from-callobj 
                           (omNG-make-new-boxcall (fdefinition (method-name (object dragged)))  position  ;aaa* 
                                                  (mk-unique-name target (name dragged))))) t)


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged tempobjframe) 
                         (target patchPanel) position)

  (let ((new-position position)) 
    ;;; would be nice to do something here to compensate the size difference between possibly-long temporal boxes and fixed-with patch boxes...

    ;;; from maquette to patch, it is better to just create the box where the mouse is...
    (cond
     ((or (om-maquette-abs-p (reference (object dragged))) (abspatch-p (reference (object dragged))))
      (omG-add-element target (make-frame-from-callobj 
                               (omNG-make-new-boxcall (clone (reference (object dragged)))
                                                    new-position (mk-unique-name target (name (reference (object dragged)))))))
      t)
     ((patch-p (reference (object dragged)))
      (omG-add-element target (make-frame-from-callobj 
                               (omNG-make-new-boxcall (reference (object dragged)) 
                                                      new-position (mk-unique-name target (name (reference (object dragged))))))) 
    t)
     ((ominstance-p (reference (object dragged)))
    
      (let ((newbox (omNG-make-new-boxcall (omNG-make-new-instance (clone (car (value (object dragged)))) 
                                                                   (mk-unique-name target (name dragged)))
                                           new-position (mk-unique-name target (name dragged)))))
        (setf (edition-params (reference newbox)) (eval (copy-edition-params (object dragged))))
        (omG-add-element target (make-frame-from-callobj newbox))
        t))
   (t nil))))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged tempobjframe) 
                                                  (target MaquettePanel) position)
   nil)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged tempobjframe) 
                         (target WorkSpacePanel) position)
  (cond 
   ((equal (type-of (reference (object dragged))) 'OMPatchAbs)
    (let* ((patch (reference (object dragged)))
           (newpatch (abs2patch patch (mk-unique-name target (name patch)) position))
           (new-frame (make-icon-from-object newpatch (om-point-h position) (om-point-v position) 1 1)))
      (omg-add-element target new-frame)
      (externalize dragged newpatch) t))
   ((equal (type-of (reference (object dragged))) 'OMMaqAbs)
    (let* ((patch (reference (object dragged)))
           (newpatch (abs2maquette patch (mk-unique-name target (name patch)) position))
           (new-frame (make-icon-from-object newpatch (om-point-h position) (om-point-v position) 1 1)))
      (omg-add-element target new-frame)
      (externalize dragged newpatch) t))
   (t nil)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged tempobjframe) 
                         (target FolderPanel) position)
   (cond 
   ((equal (type-of (reference (object dragged))) 'OMPatchAbs)
    (let* ((patch (reference (object dragged)))
           (newpatch (abs2patch patch (mk-unique-name target (name patch)) position))
           (new-frame (make-icon-from-object  newpatch (om-point-h position) (om-point-v position) 1 1)))
      (omg-add-element target new-frame)
      (externalize dragged newpatch) t))
   ((equal (type-of (reference (object dragged))) 'OMMaqAbs)
    (let* ((patch (reference (object dragged)))
           (newpatch (abs2maquette patch (mk-unique-name target (name patch)) position))
           (new-frame (make-icon-from-object  newpatch (om-point-h position) (om-point-v position) 1 1)))
      (omg-add-element target new-frame)
      (externalize dragged newpatch) t))
   (t nil)))




  
;--------------------------------------------------------
;------------------------Slot target---------------------
;--------------------------------------------------------

(defun changing-slot-type (slotframe newtype icon)
  ; NO SLOT OF THE SAME CLASS TYPE...
  (unless (subtypep newtype (class-name (object (om-view-container slotframe))))
   (let ((new-slot (omG-change-type-slot (object slotframe) newtype)))
     (setf (object slotframe) new-slot)
     (setf (iconID (iconview slotframe)) icon)
     (re-sort-slots (om-view-container slotframe))
     t)))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged type-icon-frame) 
                           (target slot-icon-frame) position)
   (declare (ignore position))
   (changing-slot-type target (class-name (object dragged)) (iconID (iconview dragged))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                           (target slot-icon-frame) position)
   (declare (ignore position))
   (changing-slot-type target (class-name (object dragged)) (iconID (iconview dragged))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classboxframe) 
                           (target slot-icon-frame) position)
   (declare (ignore position))
   (changing-slot-type target (reference (object dragged)) (iconID (iconview dragged))))

;;;  !!! drag les icon slots dans un autre (class editor) 
(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged slot-icon-frame) 
                           (target slot-icon-frame) position)
   (declare (ignore position))
   ;; change classname pour thetype
   (changing-slot-type target (thetype (object dragged)) 
                       (iconID (iconview dragged)))
   )

;;; + DEF VALUE
;;; ===> does NOT work
(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instance-icon-frame) (target slot-icon-frame) position)
   (declare (ignore position))
   (when (changing-slot-type target (type-of (instance (object dragged))) (iconID (iconview dragged)))
     (omg-change-initform (object target) (clone (instance (object dragged))))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) (target slot-icon-frame) position)
   (declare (ignore position))
   (when (changing-slot-type target (type-of (instance (reference (object dragged)))) (iconID (iconview dragged)))
     (omg-change-initform (object target) (clone (instance (reference (object dragged)))))))


;-------------------Class-tree target-----------------------
(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classboxFrame) 
                         (target classTreePanel) position)
  (if (protected-p (find-class (reference (object dragged))))
    (om-beep-msg "This class is protected, try with an alias")
    (progn
      (omg-remove-element  (om-view-container  dragged) dragged)
      (omG-add-element  target dragged)
      (om-set-view-position dragged position)
      (setf (frame-position (object dragged)) (borne-position position))
      t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged aliasBoxframe) 
                         (target classTreePanel) position)
  (if (and (OMBoxClass? (reference (object dragged)))
           (not (protected-p (object dragged))))
    (progn
      (omg-remove-element  (om-view-container  dragged) dragged)
      (omG-add-element  target dragged)
      (om-set-view-position dragged position)
      (setf (frame-position (object dragged)) (borne-position position))
      t)))




;-------------------Class target-----------------------
(defun make-add-slot (target thetype position)
  ; NO SLOT OF THE SAME CLASS TYPE...
  (unless (subtypep thetype (class-name (object target)))
    (let* (new-object new-frame)
      (setf new-object (omNG-make-new-slot (class-name (object target))
                                           (mk-unique-name  target "slot") thetype ':instance (get-super-default-value thetype) t))
      (setf new-frame (make-icon-from-object  new-object (om-point-h position) (om-point-v position) 1 1))
      (omg-add-element target new-frame)
      t)))
  
(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                         (target ClassPanel) position)
  (make-add-slot target (class-name (object dragged)) position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classboxframe) 
                         (target ClassPanel) position)
  (make-add-slot target (reference (object dragged))   position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged type-icon-frame) 
                         (target ClassPanel) position)
  (make-add-slot target (class-name (object dragged)) position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instance-icon-frame) 
                         (target ClassPanel) position)
  (make-add-slot target (class-name (class-of (instance (object dragged)))) position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                         (target ClassPanel) position)
  (make-add-slot target (class-name (class-of (instance (reference (object dragged))))) position))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged slot-icon-frame) 
                         (target ClassPanel) position)
  (if (protected-p (object dragged))
    (om-beep-msg "The slot source is protected, you can not change its container, try with option-key")
    (omG-change-container dragged target position)))



;----------------Maquette target---

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patch-icon-frame) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (object dragged) maqpos 
                                      (name (object dragged))
                                      ;(mk-unique-name target "tempobj")
                                      ))
         new-frame)
    (setf new-frame (make-frame-from-callobj new-call))
    (omG-add-element target new-frame)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquette-icon-frame) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
        (new-call (omNG-make-tempobj (object dragged) maqpos 
                                     ;;;(mk-unique-name target "maquette")
                                     (name (object dragged))
                                     ))
        new-frame)
   (setf new-frame (make-frame-from-callobj new-call))
    (omG-add-element target new-frame)) t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged omboxframe) 
                         (target MaquettePanel) position)
  (if (allowed-in-maq-p (value (object dragged)))
    (let* ((omins (omNG-make-new-instance (clone (value (object dragged))) (name (object dragged))))
           (maqpos (get-offset/posy-from-pixel target position))
           (new-call (omNG-make-tempobj omins maqpos (name (object dragged)))))
      (setf (edition-params new-call) (eval (copy-value-params (value (object dragged)) (object dragged))))
      (omG-add-element target (make-frame-from-callobj new-call))
      t)
    (om-beep-msg "I can not put this into the maquette")))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                         (target MaquettePanel) position)
  ;for score instances
  (when (allowed-in-maq-p (car (value (object dragged))))
    (let* ((omins (omNG-make-new-instance (clone (car (value (object dragged)))) (name (object dragged))))
           (maqpos (get-offset/posy-from-pixel target position))
           (new-call (omNG-make-tempobj omins maqpos (name (object dragged)))))
      (setf (edition-params new-call) (eval (copy-value-params (car (value (object dragged))) (object dragged))))
      (omG-add-element target (make-frame-from-callobj new-call))
      t))
  ;;;
  (if (allowed-in-maq-p (value (object dragged)))
      (let ((maqpos (get-offset/posy-from-pixel target position))
            (newcall nil))
      (if (global-p (reference (object dragged)))
          (setf new-call (omNG-make-tempobj (reference (object dragged)) maqpos (name (object dragged))))
        (setf new-call (omNG-make-tempobj (omNG-make-new-instance (clone (value (object dragged)))
                                                                  (name (object dragged))) 
                                          maqpos (name (object dragged)))))
      
      (setf (edition-params new-call) (eval (copy-value-params (value (object dragged)) (object dragged))))
      (omG-add-element target (make-frame-from-callobj new-call))
      t)
    (unless (allowed-in-maq-p (car (value (object dragged))))
    (om-beep-msg "I can not put this into the maquette"))))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (object dragged) maqpos (mk-unique-name target "tempobj")))
         new-frame)
    (when new-call
      (setf new-frame (make-frame-from-callobj new-call))
      (omG-add-element target new-frame) t)))

;(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged slot-icon-frame) 
;                         (target MaquettePanel) position)
;  (let* ((maqpos (get-offset/posy-from-pixel target position))
;         (new-call (omNG-make-tempobj (object dragged) maqpos (mk-unique-name target "tempobj")))
;         new-frame)
;    (when new-call
;      (omG-add-element target new-frame) t)))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instance-icon-frame) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (object dragged) maqpos (name (object dragged))))
         new-frame)
    (when new-call
      (setf new-frame (make-frame-from-callobj new-call))
      (omG-add-element target new-frame) t)))

;;; ****
(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patchboxframe) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (reference (object dragged)) maqpos 
                                      (name (reference (object dragged)))
                                      ;(mk-unique-name target "tempobj")
                                      )))
    (omG-add-element target (make-frame-from-callobj new-call))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquetteframe) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (reference (object dragged)) maqpos 
                                      (name (reference (object dragged)))
                                      ;(mk-unique-name target "maquette")
                                      )))
    (omG-add-element target (make-frame-from-callobj new-call))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patchboxabsframe) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (eval (omng-copy (reference (object dragged)))) maqpos 
                                      (name (reference (object dragged))) ; (mk-unique-name target "tempobj")
                                      )))
    (omG-add-element target (make-frame-from-callobj new-call))
    t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquetteabsframe) 
                         (target MaquettePanel) position)
  (let* ((maqpos (get-offset/posy-from-pixel target position))
         (new-call (omNG-make-tempobj (eval (omng-copy (reference (object dragged)))) maqpos 
                                      (name (reference (object dragged))) ; (mk-unique-name target "abs-maquette")
                                      )))
    (omG-add-element target (make-frame-from-callobj new-call))
    t))



;;; ****

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged patchboxframe) 
                         (target patchPanel) position)
  (call-next-method)   
  (push (object dragged) (attached-objs (reference (object dragged))))
  t)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged maquetteframe) 
                           (target patchPanel) position)
  (call-next-method)   
  (push (object dragged) (attached-objs (reference (object dragged))))
  t)




;-------------Package target-------------

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                         (target PackagePanel) position)
  (omG-change-container dragged target position))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged package-icon-frame) 
                         (target PackagePanel) position)
  (cond
   ((equal (object target) *library-package*) 
    (package2userLib (object dragged))
    (om-close-window (window target)))
   (t (omG-change-container dragged target position))))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged package-icon-frame) 
                         (target package-icon-frame) position)
  (cond
   ((equal (object target) *library-package*) 
    (package2userLib (object dragged))
    (om-close-window (window (om-view-container  target))) t)
   (t (omG-change-to-little-icon dragged target position) t)))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                         (target package-icon-frame) position)
  (omG-change-to-little-icon dragged target position))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged genfun-icon-frame) 
                         (target package-icon-frame) position)
   (omG-change-to-little-icon dragged target position))


;------------------- TypedIn 

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged type-icon-frame) 
                         (target TypedInFrame) position)
  (declare (ignore position))
  (when (or (equal (win-mod (editor target)) :abs)
            (equal (win-mod (editor target)) :new))
    (omG-change-type target (read-from-string (name (object dragged)))) t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) 
                         (target TypedInFrame) position)
  (declare (ignore position))
  (when (or (equal (win-mod (editor target)) :abs)
            (equal (win-mod (editor target)) :new))
    (omG-change-type target (class-name (object dragged))) t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instBoxframe) 
                         (target TypedInFrame) position)
  (declare (ignore position))
  (when (or (equal (win-mod (editor target)) :abs)
            (equal (win-mod (editor target)) :new))
    (setf (defval (object target)) (omng-copy (value (object dragged))))
    (omG-change-type target (class-name (class-of (value (object dragged)))))
     t))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged boxEditorFrame) 
                         (target TypedInFrame) position)
  (declare (ignore position))
  (when (or (equal (win-mod (editor target)) :abs)
            (equal (win-mod (editor target)) :new))
    (setf (defval (object target)) (omng-copy (value (object dragged))))
    (omG-change-type target (class-name (class-of (value (object dragged))))) t))



;----------------------------------------------------------

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                         (target initform-ttybox) position)
   (declare (ignore position))
  (let* ((slot (object (object target)))
         (inform (instance (reference (object dragged)))))
    (omG-change-initform slot inform)
    (re-sort-slots (om-view-container  target))))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
                         (target initform-button) position)
  (declare (ignore position))
  (let* ((slot (object (object target)))
         (inform (instance (reference (object dragged)))))
    (omG-change-initform slot inform)
    (re-sort-slots (om-view-container  target))))


;;; suppr
;(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instboxframe) 
;                         (target defval-editbox) position)
;   (declare (ignore position))
;  (let* ((input (object target))
;         (inform (instance (reference (object dragged))))
;         (defval (omng-copy inform)))
;    (setf (defval input) defval)
;    (setf (instance-p target) t) ;;; new jb
;    (om-set-dialog-item-text target (format () "~S" (eval `',inform)))
;    t))    


(defmethod drop-area-action ((target drop-area) (dragged t)) nil)

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged t) 
                         (target drop-area) position)
   (declare (ignore position))
   (drop-area-action target dragged))


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged class-icon-frame) (target typed-input-button) position)
  (declare (ignore position))
  (funcall (action target) dragged))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged type-icon-frame) (target typed-input-button) position)
  (declare (ignore position))
  (funcall (action target) dragged))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged instBoxframe)  (target typed-input-button) position)
  (declare (ignore position))
  (make-typed-input-from-obj (class-of (value (object dragged))) (om-view-container target) (value (object dragged))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged boxEditorFrame) (target typed-input-button) position)
  (declare (ignore position))
  (make-typed-input-from-obj (class-of (value (object dragged))) (om-view-container target) (value (object dragged))))

(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged classBoxframe)  (target typed-input-button) position)
  (declare (ignore position))
  (make-typed-input-from-obj (find-class (reference (object dragged)) nil) (om-view-container target)))

;=====================================================
;Perform copies with OPT-KEY
;=====================================================


(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMPatch) (target-frame t) subframes copies pos0)
  (unless (subtypep (type-of (container-view D&DHandler)) 'maquettepanel)
       (let (boxes rep)
     (copy-connections subframes copies)
     (loop for item in (get-actives target-frame) do
               (omG-unselect item))
     
     (om-with-delayed-update target-frame
       (mapcar #'(lambda (object)
                   (setf (frame-position object) 
                         (borne-position (om-add-points
                                          (frame-position object)
                                          (om-subtract-points (drop-mouse-pos D&DHandler) (initial-mouse-pos D&DHandler))
                                          )
                                         ))
                   (let ((new-frame (make-frame-from-callobj object)))
                     (push new-frame rep)
                   (omG-add-element target-frame new-frame)
                   (omG-select new-frame))) copies)
       (setf boxes (get-elements targetobj))
       (mapc #'(lambda (box)
                 (update-graphic-connections box boxes)) rep)
       )
     
     (om-invalidate-view target-frame)
     t)
   ))


(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMMethod) (target-frame t) subframes copies pos0)
   (let (boxes rep)
     (copy-connections subframes copies)
     (loop for item in (get-actives target-frame) do
           (omG-unselect item))
     (mapcar #'(lambda (object)
                 (setf (frame-position object) 
                       (borne-position  (om-add-points
                                          (frame-position object)
                                          (om-subtract-points (drop-mouse-pos D&DHandler) (initial-mouse-pos D&DHandler))
                                          )))
                 (let ((new-frame (make-frame-from-callobj object)))
                   (push new-frame rep)
                   (omG-add-element target-frame new-frame)
                   (omG-select new-frame))) copies)
     (setf boxes (get-elements targetobj))
     (mapc #'(lambda (box)
               (update-graphic-connections box boxes)) rep)) t)


(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMMaquette) (target-frame t) subframes copies pos0)
    
  (let ((rep ()))

    (copy-connections subframes copies)
    
    (loop for item in (get-actives target-frame) do (omG-unselect item))
    
    (let ((symbolic-drop-pos (get-offset/posy-from-pixel target-frame (drop-mouse-pos D&DHandler)))
          (symbolic-clic-start (pixel2point 
                                (container-view D&DHandler) 
                                (initial-mouse-pos D&DHandler)
                                )))
      
      ;(print (list "origin clic pos" symbolic-clic-start))
      ;(print (list "target drop pos" symbolic-drop-pos))
      
      (om-with-delayed-update target-frame
        
        (mapcar #'(lambda (object)
                    
                    (let* ((symbolic-initpos (om-make-point (slot-value object 'offset) (posy object)))
                           (symbolic-delta (om-subtract-points symbolic-initpos symbolic-clic-start))
                           (symbolic-new-pos (om-add-points symbolic-drop-pos symbolic-delta)))

                      ;(print (list "origin box pos" symbolic-initpos))
                      ;(print (list "origin box delta" symbolic-delta))
                      ;(print (list "new box pos" symbolic-new-pos))
                    
                      (setf (slot-value object 'posy)  (om-point-v symbolic-new-pos))
                      (setf (offset object) (om-point-h symbolic-new-pos))
                
                      (let ((new-frame (make-frame-from-callobj object)))
                        (push new-frame rep)
                        (omG-add-element target-frame new-frame)
                        (omG-select new-frame)))
                    )
                copies)
        
        (let ((boxes (get-elements targetobj)))   
          (mapc #'(lambda (box)
                    (update-graphic-connections box boxes))
                rep))
        )
      t)))


(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMPatch) (target-frame icon-finder) subframes copies pos0)
   (declare (ignore subframes copies pos0))
   (om-beep-msg "Open the patch if you want to make the copies"))

(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj OMMaquette) (target-frame icon-finder) subframes copies pos0)
   (declare (ignore subframes copies pos0))
   (om-beep-msg "Open the maquette if you want to make the copies"))

(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj t) (target-frame t) subframes copies pos0 )
   (declare (ignore subframes))
   (loop for item in (get-actives target-frame) do
           (omG-unselect item))
   (mapcar #'(lambda (object)
               (setf (name object) (mk-unique-name target-frame (name object)))
               (let* ((position (om-add-points
                                          (get-icon-pos object)
                                          (om-subtract-points (drop-mouse-pos D&DHandler) (initial-mouse-pos D&DHandler))
                                          )
                                )
                      (new-frame (make-icon-from-object object (om-point-h position) (om-point-v position) 1 1)))
                 (unless (big-icon-p (editor target-frame))
                   (let* ((frames (get-subframes target-frame))
                          ;(frames (get-subframes targetobj))
                          (newcol (findcol target-frame frames new-frame)))
                     (setf (col new-frame) newcol)))
                 (add-icon-finder new-frame target-frame)
                 (om-paste (object target-frame) object)
                 (omG-select new-frame))) copies)
   t)

(defmethod perform-duplicate-list ((D&DHandler omdrag-drop) (targetobj t) (target-frame icon-finder) subframes copies pos0)
  (declare (ignore pos0 subframes))
  (when (editorFrame (object target-frame))
    (om-close-window (window (editorFrame (object target-frame)))))
  (mapcar #'(lambda (object)
              (set-icon-pos object (om-make-point 22 22))
              (om-paste (object target-frame) object)) copies)
  t)




;=========================================================
;This function get the container of an OMBASICOBJECT
;=========================================================

(defun get-relative-path (obj)
   (let ((wslist (pathname-directory (mypathname *current-workSpace*)))
         (objlist (pathname-directory (mypathname obj)))
         (continue t))
     (when (folder-p obj)
       (setf objlist (butlast objlist)))
     (loop while (and wslist objlist continue) do
           (setf continue (equal (pop wslist) (pop objlist))))
     (when (and continue (null wslist))
       objlist)))
          
(defmethod get-real-container ((self OMBasicObject))
   (when (mypathname self)
     (let ((path (get-relative-path self)) object)
       (when path
         (setf object *current-workSpace*)
         (setf path (cdr path))
         (loop for item in path do
               (let ((elements (get-elements object)))
                 (setf object (find-if #'(lambda (x) (string-equal (name x) item)) elements))
                 (unless object
                   (setf path nil))))
           object))))

(defmethod get-real-container ((self OMPackage)) (father self))

(defmethod get-real-container ((self OMClass))
   (get-class-real-container self *package-user*))

(defun get-class-real-container (class package)
  (let (rep)
    (loop for item in (classes package)
          while (not rep) do
          (when (equal item class)
            (setf rep package)))
    (cond
     (rep rep)
     ((null (subpackages package))
      nil)
     (t (loop for item in (subpackages package)
              while (not rep) do
              (setf rep (get-class-real-container class item)))
        rep))))

(defmethod get-real-container ((self OMGenericFunction))
  (get-genfun-real-container self *package-user*))

(defun get-genfun-real-container (class package)
  (let (rep)
    (loop for item in (functions package)
          while (not rep) do
          (when (equal item class)
            (setf rep package)))
    (cond
      (rep rep)
      ((null (subpackages package))
       nil)
      (t (loop for item in (subpackages package)
               while (not rep) do
               (setf rep (get-genfun-real-container class item)))
         rep))))


(defmethod get-real-container ((self OMBoxAlias))
  (get-alias-class-real-container self *package-user*))

(defun get-alias-class-real-container (class package)
  (let (rep)
    (loop for item in (aliasclasses package)
          while (not rep) do
          (when (equal item class)
            (setf rep package)))
    (cond
      (rep rep)
      ((null (subpackages package))
       nil)
      (t (loop for item in (subpackages package)
               while (not rep) do
               (setf rep (get-alias-class-real-container class item)))
         rep))))

