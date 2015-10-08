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
;Editors for OMinstances OMlistinstances and OMconstant.
;Last Modifications :
;18/10/97 first date.
;DocFile

(in-package :om)


;---------------------

(omg-defclass InstanceEditor (nonrelationEditor) ())

(defmethod get-editor-panel-class ((self InstanceEditor))  'InstancePanel)

(defmethod get-clipboard ((self InstanceEditor)) nil)


(defmethod update-editor-after-eval ((self InstanceEditor) val)
   (let (slot-boxes)
     (setf (object self) val)
     (apply 'om-remove-subviews (cons (panel self) (om-subviews (panel self))))
     (setf slot-boxes (slots-inst-boxes (ref  self) val))
     (mapc #'(lambda (frame)
               (omG-add-element (panel self) frame)) slot-boxes)
     (om-invalidate-view self t)))

;---------------------
;PANEL
;---------------------
(omg-defclass InstancePanel (nonrelationPanel) ()
   (:documentation "This is the class for OMInstance's editors. 
Elements of these editors are instance-icon-frame instances.#enddoc#
#seealso# (OMInstance instance-icon-frame listPanel) #seealso#"))

(defmethod instsroller-p ((self InstancePanel)) t)
(defmethod instsroller-p ((self t)) nil)  

(defmethod allow-move-element ((self InstancePanel))
   "You can not slot-value boxes in an instance editor." nil)

(defmethod get-actives ((self InstancePanel) &optional class)
   "Return a list with the selected icons (simpleframes) subviews of 'self'."
   (let* (rep)
     (mapc #'(lambda (icon)
               (if (and (subtypep (class-of icon) 'OMSimpleframe)
                        (active-mode icon)) 
                 (push icon rep))) (om-subviews self))
     (reverse rep)))

(defmethod om-view-click-handler ((self instancepanel) position)
  (call-next-method)
  (mapcar #'(lambda (frame) (when (subtypep (class-of frame) 'OMSimpleframe)
                              (omG-unselect frame))) (om-subviews self)))



;***********************************************
;Special list Editor
;***********************************************

;-------------------
;Window
;-------------------



(omg-defclass list-enter-view (edit-text-enter-view) 
   ((direction :initform 0 :initarg :direction :accessor direction)))

;; new : text-view sur self directement
(defmethod exit-from-dialog ((self list-enter-view) newtext)
   (let* ((newpos (read-from-string newtext))
         (container (om-view-container self))
         (instance (instance (object container))))
     (if (and (integerp newpos) (>= newpos 0))
       (change-list-ranges container (direction self) newpos)
       (om-beep))
     (setf (text-view container) nil)
     (om-remove-subviews container self)))

(omg-defclass control-list (om-view)  ())


(defmethod initialize-instance :after  ((self control-list) &key)
   (om-add-subviews self
     (om-make-view 'button-icon
       :iconID 188
       :position (om-make-point 14 5)
       :size (om-make-point 11 11)
       :action  #'(lambda (item) 
                    (advance-range (om-view-container (om-view-container item)) 0)))   
     (om-make-view 'button-icon
       :iconID 164
       :position (om-make-point 83 5)
       :size (om-make-point 11 11)
       :action  #'(lambda (item) 
                    (advance-range (om-view-container (om-view-container item)) 1)))
     
     (om-make-view 'button-icon
       :iconID 202
       :position (om-make-point 144 5)
       :size (om-make-point 11 11)
       :action  #'(lambda (item) 
                    (advance-range (om-view-container (om-view-container item)) 2)))   
     (om-make-view 'button-icon
       :iconID 165
       :position (om-make-point 213 5)
       :size (om-make-point 11 11)
       :action  #'(lambda (item) 
                    (advance-range (om-view-container (om-view-container item)) 3)))))
                
(defmethod om-draw-contents :before ((self control-list))
  (om-with-focused-view self
       (om-draw-line 0 19 (w self) 19)
       (om-draw-string 30 14  (format nil "~D" (car (ranges (om-view-container self)))))
    (om-draw-string 160 14 (format nil "~D" (second (ranges (om-view-container self)))))))

;;; textview on the panel
(defmethod om-view-click-handler ((self control-list) where)
   (let ((editor (om-view-container self)))
     (call-next-method)
     (when (text-view editor)
       (exit-from-dialog (text-view editor) (om-dialog-item-text (text-view editor))))
     (cond
      ((and (>= (om-point-h where) 29) (<= (om-point-h where) 80))
       (om-add-subviews editor
         (setf (text-view editor)
            (om-make-dialog-item 'list-enter-view (om-make-point 29 4)
                                                      (om-make-point 50 10)
                                                      (format () "~D" (car (ranges editor)))
                                                      :allow-returns t
                                                      :object self
                                                      :font *om-default-font2*))))
      ((and (>= (om-point-h where) 159) (<= (om-point-h where) 210))
       (om-add-subviews editor
         (setf (text-view editor)
            (om-make-dialog-item 'list-enter-view (om-make-point 159 4)
                                                      (om-make-point 50 10)
                                                      (format () "~D" (second (ranges editor)))
                                                      :direction 1
                                                      :allow-returns t
                                                      :object self
                                                      :font *om-default-font2*)))))))


;---------------------------
; Editor
;---------------------------

(omg-defclass listEditor (instanceEditor) 
   ((control :initform nil :accessor control)
    (ranges :initform '(0 0) :accessor ranges)))

(defmethod get-editor-panel-class ((self listEditor))  'listPanel)

(defmethod panel-position ((self listEditor)) (om-make-point 0 20))

;;; enlevé les 15 du scroller
(defmethod panel-size ((self listEditor)) 
   ;(om-make-point (- (w self) 15) (- (h self) 35))
   (om-make-point (w self) (- (h self) 20))
   )

(defmethod initialize-instance :after ((self listEditor) &rest l)
   (declare (ignore l))
   (let* ((control (om-make-view 'control-list 
                     :owner self
                     :position (om-make-point 0 0) 
                     :size (om-make-point (w self) 20))))
     (setf (control self) control)
     ))

(defmethod update-subviews :after ((self listEditor))
   (om-set-view-size (control self) (om-make-point (w self) 20)))

(defmethod check-new-range ((self listEditor) range)
   (setf range (list (max  0 (car range)) (max 0 (second range))))
   (if (< (first range) (length (instance (object self))))
     range
     (om-beep)))

(defmethod  remake-boxes ((self listEditor) new-range)
   (let (newboxes)
     (setf (ranges self) new-range)
     (apply 'om-remove-subviews (cons (panel self) (om-subviews (panel self))))
     (setf newboxes (make-elemnts-list-boxes (object self) (car new-range) (second new-range)))
     (mapc #'(lambda (frame)
                       (omG-add-element (panel self) frame)) newboxes)
     (om-invalidate-view self t)
     t))


(defmethod advance-range ((self listEditor) dir)
   (let* ((x (car (ranges self)))
          (y (second (ranges self))) new-range)
     (setf new-range (check-new-range self (case dir 
                                             (0 (list (- x *size-list-inx*) y))
                                             (1 (list (+ x *size-list-inx*) y))
                                             (2 (list x (- y *size-list-iny*)))
                                             (3 (list x (+ y *size-list-iny*))))))
     (when new-range
       (remake-boxes self new-range))))
     
         

(defmethod change-list-ranges ((self listEditor) dir newval)
   (let* ((x (car (ranges self)))
          (y (second (ranges self))) new-range)
     (setf new-range (check-new-range self (case dir 
                                             (0 (list newval y))
                                             (1 (list x newval)))))
     (when new-range
       (remake-boxes self new-range)))
   )



;-------------------
;Panel
;-------------------

(omg-defclass listPanel (InstancePanel) ()
   (:documentation "This is the class for OMListInstance's editors. 
Elements of these editors are instance-icon-frame instances.#enddoc#
#seealso# (OMListInstance instance-icon-frame InstancePanel) #seealso#"))

(defmethod om-view-cursor ((self InstancePanel))  *om-arrow-cursor*)







;;;===========================
;;; key shortcuts 'p' and 's' for list editor, to play selected score-objects and sounds
;;; without opening a window

;;; arrow keys navigate the selection


(defmethod handle-key-event ((self listPanel) char)
  (let ((actives (get-actives self)))
    (labels ((selected-pos () (position (car actives) (om-subviews self)))

             (pos-select (p)
               (omg-select (nth (max 0 (min (1- (length (om-subviews self))) 
                                            p))
                                (om-subviews self))))

             (shift-select (dir)
               (if (null actives)
                   (pos-select 0)

                 (when (<= 1 (length actives))
                   (let ((pos (selected-pos)))
                     (omg-unselect (car actives))
                     
                     (cond ((and (= pos 0) (= dir -1))
                            (if (> (car (ranges (om-view-container self))) 0)
                                (pos-select (1- (length (om-subviews self))))
                              (pos-select pos)))
                           
                           ((and (= pos (1- (length (om-subviews self)))) (= dir 1))
                            (if (advance-range (om-view-container self) 1)
                                (pos-select 0)
                              (pos-select pos)))

                           (t (pos-select (+ pos dir)))))))))

      (case char

        (#\p (let ((f (first (get-actives self))))
               (when f (play (instance (object f))))))
                 

        (#\s (stop *general-player*))

        (:om-key-left 
         (shift-select -1))

        (:om-key-right
         (shift-select 1))

        (:om-key-up 
         (let ((pos (selected-pos)))
           (advance-range (om-view-container self) 0)
           (pos-select pos)))

        (:om-key-down 
         (let ((pos (selected-pos)))
           (advance-range (om-view-container self) 1)
           (pos-select pos)))
        ))))