;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) IRCAM-Centre Georges Pompidou, Paris, France.
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
;Define a class to handle drag-and-drop operations.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)

(defvar *OM-drag&drop-handler* "An object that handles the drag and drop of views at the lisp level.")

(defclass omdrag-drop ()
   ((dragged-view :initform nil :accessor dragged-view)
    (dragged-list-objs :initform nil :accessor dragged-list-objs)
    (container-view :initform nil :accessor container-view)
    (target-view :initform nil :accessor target-view)
    (true-target-view :initform nil :accessor true-target-view)
    (true-dragged-view :initform nil :accessor true-dragged-view)
    (opt-key-p :accessor opt-key-p :initform nil)
    (shift-key-p :accessor shift-key-p :initform nil)
    (initial-mouse-pos :initform nil :accessor initial-mouse-pos)
    (drop-mouse-pos :initform nil :accessor drop-mouse-pos)
    (drag-flavor :initform nil :accessor drag-flavor)))

(defun init-D&D-handler ()
  (setf *OM-drag&drop-handler* (make-instance 'omdrag-drop)))


(om-add-init-func 'init-D&D-handler) 


(defvar *receiving-in-drag* nil)
;(defvar *cur-drag* nil)

(defmethod om-draw-contents-for-drag ((self om-view-drag))
  ;;; actually draws what's in (dragged-list-objs *OM-drag&drop-handler*) 
  (om-with-fg-color nil (om-make-color-alpha 0.7 0.7 0.6 #+linux 0.3 #-linux 0.3) 
    (mapcar #'(lambda (v)
		(let ((drag-obj (get-drag-object v)))
		  ;#-linux
                  (om-fill-rect (x drag-obj) (y drag-obj) (w drag-obj) (h drag-obj))
		  ;#+linux(om-draw-rect-outline (x drag-obj) (y drag-obj) (w drag-obj) (h drag-obj) 3)
		  ))
	    (dragged-list-objs *OM-drag&drop-handler*))))

(defmethod om-drag-reference-view ((self om-view-drag))
  (get-drag-object self))


(defmethod om-drag-start ((view om-view-drag))
  (let* ((theview (get-drag-object view))
	 (container (and theview (om-view-container theview))))
    (when (and theview container)
      (let* ((regionall (om-new-region))
	     (viewpos (om-view-position theview))
	     (x0 (om-point-h viewpos))
	     (y0 (om-point-v viewpos))
	     (i 2)
	     actives dragged-lisp-objects)
        (if (om-shift-key-p) (setf (shift-key-p *OM-drag&drop-handler*) t)
          (setf (shift-key-p *OM-drag&drop-handler*) nil))
	(setf (dragged-view *OM-drag&drop-handler*)  theview
	      (initial-mouse-pos *OM-drag&drop-handler*) (om-mouse-position theview)
	      (dragged-list-objs *OM-drag&drop-handler*) (get-actives container)
	      (container-view *OM-drag&drop-handler*) container
	      (true-dragged-view *OM-drag&drop-handler*) view
	      (drag-flavor *OM-drag&drop-handler*) :omvw))
      t)))



(defmethod view-frame ((self t)) self)

(defmethod om-drag-receive ((view om-view-drop) (dragged-view t) position &optional (effect nil))
  (unless (dragged-view *OM-drag&drop-handler*)
    (setf (dragged-view *OM-drag&drop-handler*) dragged-view))
  (if (equal (drag-flavor *OM-drag&drop-handler*) :omsc)
    (score-drag-receive view dragged-view)
    (when dragged-view
      (let* ((drop-pos position)
             (*receiving-in-drag* t) rep)
        
        (setf (opt-key-p *OM-drag&drop-handler*) (equal effect :copy)) ;  (om-option-key-p)
        
        (setf (target-view *OM-drag&drop-handler*) 
              (if (eq (get-drag-object view) (dragged-view *OM-drag&drop-handler*))
                  (om-view-container (get-drag-object view))
                view))
        
        (setf
              ;;; wrong in the case of drag from package windows' sub-panels...
              (initial-mouse-pos *OM-drag&drop-handler*) (om-convert-coordinates (initial-mouse-pos *OM-drag&drop-handler*)
                                                                                 (dragged-view *OM-drag&drop-handler*)
                                                                                 (target-view  *OM-drag&drop-handler*))
             
              (true-target-view *OM-drag&drop-handler*) (view-frame view)
              (drop-mouse-pos *OM-drag&drop-handler*) (om-mouse-position (target-view  *OM-drag&drop-handler*))  ; (get-pos-in-object view drop-pos))
              )
        
        (setf rep (finalize-drag&drop *OM-drag&drop-handler*))
        ;;; test
        (om-highlight-view (get-drag-object view) nil)
        (om-highlight-view view nil)
        ;;;test : reinitialize values
        (setf (dragged-view *OM-drag&drop-handler*) nil
              (dragged-list-objs *OM-drag&drop-handler*) nil
              (container-view *OM-drag&drop-handler*) nil
              (true-dragged-view *OM-drag&drop-handler*) nil
              (drag-flavor *OM-drag&drop-handler*) nil)
        rep
        ))))
 
(defun in-non-visible-icon (D&DHandler)
   (or (and (icon-finder-p (true-target-view  D&DHandler)) (icon-finder-p (dragged-view  D&DHandler)))
       (and (boxframe-p  (true-target-view  D&DHandler)) (boxframe-p (dragged-view  D&DHandler)))))

;---------------------------------------------------------------------------------
(defmethod finalize-drag&drop ((D&DHandler omdrag-drop))
  
  ;(print (list (true-target-view D&DHandler) (icon-finder-p (true-target-view D&DHandler))))
  (cond 
   ((opt-key-p *OM-drag&drop-handler*)
    (perform-duplicate-view D&DHandler))  
   
   ((shift-key-p *OM-drag&drop-handler*)
    (perform-make-slots-view D&DHandler))
   
   (;;; D&D IN FOLDERS ETC.
    (and (icon-finder-p (true-target-view D&DHandler)) (icon-finder-p (dragged-view  D&DHandler))
         (drop-allow-p D&DHandler (object (dragged-view  D&DHandler)) (object (true-target-view D&DHandler)))
         (not (equal (true-target-view  D&DHandler) (dragged-view  D&DHandler))))
    (perform-change-view D&DHandler))
   
   (;;; AUTO MOVE
    (in-non-visible-icon D&DHandler)
    (if (equal (om-view-container (true-target-view  D&DHandler)) (om-view-container (dragged-view  D&DHandler)))
      (when (allow-move-element (om-view-container (target-view  D&DHandler)))
        (make-move-before (om-view-container (dragged-view  D&DHandler)) (dragged-list-objs D&DHandler))
        (perform-move-view D&DHandler)
        (make-move-after (om-view-container (true-target-view  D&DHandler)) (dragged-list-objs D&DHandler))
        )
      (progn
        (make-move-before (om-view-container (dragged-view D&DHandler)) (dragged-list-objs D&DHandler))
        (setf (target-view  D&DHandler) (om-view-container (true-target-view  D&DHandler)))
        (perform-change-view D&DHandler)
        )))
    ((eq (target-view D&DHandler) (om-view-container (dragged-view D&DHandler)))
     (when (allow-move-element (target-view  D&DHandler))
       (make-move-before (om-view-container (dragged-view  D&DHandler)) (dragged-list-objs D&DHandler))
       (perform-move-view D&DHandler)
       (make-move-after (target-view  D&DHandler) (dragged-list-objs D&DHandler))
       ))
    ((eq (target-view  D&DHandler) (dragged-view  D&DHandler))
     (when (allow-move-element (om-view-container (target-view  D&DHandler)))
       (make-move-before (om-view-container (dragged-view  D&DHandler)) (dragged-list-objs D&DHandler))
       (perform-special-move-view D&DHandler)
       (make-move-after (om-view-container (target-view  D&DHandler)) (dragged-list-objs D&DHandler))
       ))
    ;;; test for jeremie (allow drag on comments, boxes etc.)
    ((eq (om-view-container (om-view-container (target-view D&DHandler))) (om-view-container (dragged-view D&DHandler)))
     (make-move-before (om-view-container (dragged-view  D&DHandler)) (dragged-list-objs D&DHandler))
     (perform-special-move-view D&DHandler)
     (make-move-after (om-view-container (target-view  D&DHandler)) (dragged-list-objs D&DHandler)))
    (t (perform-change-view D&DHandler)))
  )



;------DUPLICATE WITH OPT_KEY
(defmethod perform-duplicate-view ((D&DHandler omdrag-drop))
   (let ((target-frame (get-drag-object (target-view  D&DHandler)))
         (pos0 (get-relative-position (get-drag-object (dragged-view D&DHandler))))
         (some-item-used t)
         nameerr)
    ; (when  (allow-move-element (target-view  D&DHandler))
       (mapc #'(lambda (dragged-frame)
                 (when  (or (no-allow-copy-p (object dragged-frame))
                            (not (drop-allow-p D&DHandler (object dragged-frame) (object target-frame)))
                            (null (can-copy (object target-frame) (object dragged-frame))))
                   (setf nameerr (name (object dragged-frame)))
                   (setf some-item-used nil))) (dragged-list-objs D&DHandler))
       (if some-item-used
         (let* ((subframes (mapcar #'(lambda (frame)
                                       (object frame)) (dragged-list-objs D&DHandler)))
                (copies (mapcar #'(lambda (frame)
                                   (eval (omNG-copy frame))) subframes)))
           (setf some-item-used (perform-duplicate-list D&DHandler (object target-frame) target-frame subframes copies pos0))
           ;(print (list D&DHandler (object target-frame) target-frame subframes copies pos0))
           )
         (om-beep-msg (string+ "The object " nameerr " can not be copied to this window.")))
     ;  )
     some-item-used))


;------MAKE SLOTBOX WITH SHIFT_KEY

(defmethod perform-make-slots-view ((D&DHandler omdrag-drop))
  (let ((pos0 (get-position (dragged-view D&DHandler))))
    (mapc #'(lambda (oneobject)
              (OMGMoveObject oneobject
                             (om-add-points
                             ; (om-subtract-points (get-position oneobject) pos0) 
                              (om-view-position oneobject)
                              (om-subtract-points (drop-mouse-pos D&DHandler) (initial-mouse-pos D&DHandler))
                             )
                             )
              )
      (dragged-list-objs D&DHandler)) 
    t)
  )



                 
;------MOVE IN THE SAME WINDOW
(defmethod perform-move-view ((D&DHandler omdrag-drop))
  (let ((pos0 (get-position (dragged-view D&DHandler))))
    (mapc #'(lambda (oneobject)
              (OMGMoveObject oneobject
                             (om-add-points
                             ; (om-subtract-points (get-position oneobject) pos0) 
                              (om-view-position oneobject)
                              (om-subtract-points (drop-mouse-pos D&DHandler) (initial-mouse-pos D&DHandler))
                             )
                             )
              )
      (dragged-list-objs D&DHandler)) t)
  )


;Move in the same object
(defmethod perform-special-move-view ((D&DHandler omdrag-drop))
   (mapc #'(lambda (oneobject)
             (OMGMoveObject oneobject
                            (om-add-points (get-position oneobject)
                                           (om-subtract-points (drop-mouse-pos D&DHandler) (initial-mouse-pos D&DHandler)))))
     (dragged-list-objs D&DHandler)) 
  t)


;----------WHEN THE TARGET AND THE SOURCES WINDOWS ARE DIFFERENTS

;;; si on drop plusieurs objets sur une meme cible, 
;; est ce qu'on renvoie la liste (t)
;; ou bien on fait des drops sur chaque objet (nil) 
(defmethod allow-drag-list ((self t)) nil)

(defmethod allow-drag-list ((self omgenericfunction)) 
  (function-allow-list self))

(defmethod perform-drop-list ((dd t) (dragged t) (target t) pos) nil)

(defmethod perform-change-view ((D&DHandler omdrag-drop))
  (let* ((target-frame (get-drag-object (target-view  D&DHandler)))
         (pos0 (get-position (get-drag-object (dragged-view D&DHandler))))
         (list-objs (mapcar #'(lambda (frame) (object frame)) (dragged-list-objs D&DHandler)))
         ;; c'etait en comment
         (connectlist (save-connections target-frame (container-view D&DHandler) list-objs))
         (some-item-used nil)
         (correctmove t))
   (loop for item in (dragged-list-objs D&DHandler)
          while correctmove do
          (setf correctmove (drop-allow-p D&DHandler (object item) (object target-frame))))
   (if correctmove
      (progn 
        ;; removes the connections to other boxes. problem if we want to restore them (e.g. externalize)
        (make-delete-before (container-view D&DHandler) (dragged-list-objs D&DHandler) target-frame)
        (if (allow-drag-list (object target-frame))
          (setf some-item-used (perform-drop-list D&DHandler (dragged-list-objs D&DHandler) target-frame
                                                  (om-add-points
                                                   (om-subtract-points (get-position (car (dragged-list-objs D&DHandler))) pos0) 
                                                   (om-subtract-points (drop-mouse-pos D&DHandler) (initial-mouse-pos D&DHandler)))
                                                  
                                                  ))
          (mapc #'(lambda (dragged-frame)
                    (setf some-item-used (perform-drop D&DHandler dragged-frame target-frame
                                                      (om-add-points  (drop-mouse-pos D&DHandler)
                                                                       (om-subtract-points 
                                                                        (get-position dragged-frame)
                                                                        (initial-mouse-pos D&DHandler)))
                                                      )
                          ))
                (dragged-list-objs D&DHandler))
          )
        ;; c'etait en comment
        (remake-draggeds-connections target-frame (container-view D&DHandler)  list-objs connectlist)
        ;;; new attention a tester...
        (when (patchpanel-p target-frame) (modify-patch target-frame))
        )
      (om-beep))
    (om-invalidate-view target-frame) 
    some-item-used))

  

; ---------------------------------------------------------------------
;To subclass
; ---------------------------------------------------------------------

(defmethod make-move-after ((self t) list) 
   "This method is called after moving one or more elements of self."
   t)

(defmethod make-move-after ((self om-view) dragged) 
   "This method is called after moving one or more elements of self."
   ;(mapc #'(lambda (oneobject) (om-invalidate-view oneobject)) dragged)
   ;(om-invalidate-view self)
   t)

(defmethod make-move-before ((self om-view) dragged) 
   ;(mapc #'(lambda (oneobject) (om-invalidate-view oneobject)) dragged)
   t)


(defmethod receive-file              ((self t) (type t) path pos) (declare (ignore path pos))  nil)
(defmethod save-connections          ((source t) (target t) list) 
   "Save the connections beetwen boses of 'self' as a list."
   (declare (ignore list))
   nil)
(defmethod make-delete-before        ((source t) (target t) (cible t))
   "This method is called after deleting one or elements of source."
   nil)
(defmethod external-object-p         ((self t)) nil)


(defmethod get-drag-object           ((self t)) self)
(defmethod get-drag-object           ((self om-graphic-object)) self)
(defmethod get-position              ((self om-graphic-object)) (om-view-position self))
(defmethod get-relative-position     ((self om-graphic-object)) (om-view-position self))



(defmethod get-pos-in-object         ((self t) where) where)
(defmethod allow-move-element        ((self t))
   "T if you can change the position of a self subviews, the default is T"
   t)
(defmethod drop-allow-p              ((D&DHandler omdrag-drop) (dragged t) (target t)) nil)

(defmethod remake-draggeds-connections ((target t) (source t) listobj list) 
   "Used to remake connections when you drag and drop a subset of boxes from 'self' to 'source'."
  (declare (ignore list listobj)) nil)

(defmethod make-drag-region ((self om-view) region x0 y0 view) 
   (om-set-rect-region region (- (x self) x0) (- (y self) y0) (- (x+w self) x0) (- (y+h self) y0))
   region)


(defmethod perform-drop ((D&DHandler omdrag-drop) (dragged t) (target t) position)
   (declare (ignore position)) nil)



