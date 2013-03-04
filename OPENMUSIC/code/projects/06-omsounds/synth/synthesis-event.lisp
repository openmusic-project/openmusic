(in-package :om)

                  
;============================================================
; Non Graphic CLASSES
;============================================================
; SYNTHESIS EVENTS ARE SPECIAL CLASS-ARRAY INCLUDING
; an action time and a "parsing-fun"
; only action-time is visible by default
(defclass internSynthEvt ()
   ((parsing-fun :initform nil :accessor parsing-fun)))

;============================================================
;============================================================
(defclass! SynthesisEvt (class-Array simple-container internSynthEvt) 
           ((action-time :initform 0 :accessor action-time))
           (:icon 603))

(defmethod omng-copy ((self SynthesisEvt))
  `(let ((rep ,(call-next-method)))
     (setf (action-time rep) ,(action-time self))
     rep))

;; from simple-container
(defmethod Extent->ms ((self SynthesisEvt))
  (get-obj-dur self))

;; called in synthesize methods
(defmethod get-parsing-fun ((self SynthesisEvt)) 
  (slot-value self 'parsing-fun))

(defmethod draw-editor-mode ((self SynthesisEvt) view) 
   (draw-mini-view view self)
   (draw-carre view))

(defmethod draw-with-mini-pict ((self SynthesisEvt)) nil)

(defmethod special-keyword-index ((value (eql :parsing-fun)) (obj SynthesisEvt)) :parsing-fun)
                  
(defmethod rep-editor ((self SynthesisEvt) num)
       (cond ((equal num :parsing-fun) (parsing-fun self))
             (t (call-next-method))))
 
(defmethod array-special-keyword ((self SynthesisEvt) (key (eql :parsing-fun))) t)
(defmethod array-special-keyword-action ((self SynthesisEvt) (key (eql :parsing-fun)) val)
   (setf (parsing-fun self) val))

(defmethod array-special-keyword ((self SynthesisEvt) (key (eql :precision))) t)
(defmethod array-special-keyword-action ((self SynthesisEvt) (key (eql :precision)) val)
   (put-precision self val))


;============================================================
;============================================================

(defun get-all-slots-instance-of-class (Class)
    (let* ((Class-List (omcpl (find-class class)))
               Theslotti)
       (mapcar #'(lambda (Superclass)
                              (setf theslotti (remove-duplicates (concatenate 'list theslotti (class-direct-instance-slots superclass))
                                                                 :test #'(lambda (x y) (equal (slot-definition-name x) (slot-definition-name y)))))) 
                        (reverse class-list))
       (make-slots class nil theslotti)))

(defun get-init-instance-slots-of-class (Class)
  (remove nil
          (mapcar #'(lambda (Slot)
                      (when (io-p slot)
                        (list (internp (name slot) (symbol-package class)) (thetype slot)))) 
                  (get-all-slots-instance-of-class class))))


(defmethod omNG-save ((self SynthesisEvt) &optional (values? nil)) 
  (let* ((theclass (class-name (class-of self)))
         (nc (numcols self))
         (slots (mapcan #'(lambda (slot)
                            (list `(when (member ',(first slot) (get-init-slots-of-class ',theclass) :key 'car) 
                                     (setf (slot-value rep ',(first slot)) 
                                           ,(save-slot-value self (slot-value self (first slot)) nc)
                                           )))) 
                        (get-init-instance-slots-of-class theclass))))
    `(if (find-class ',theclass nil)
         (let ((rep (make-instance ',theclass :numcols ,(numcols self))))
           ,.slots
           (setf (Lcontrols rep)
                 (list ,.(loop for con in (Lcontrols self)
                               collect (save-array-control self con nc))))
           (setf (action-time rep) ,(action-time self))
           (setf (data rep) ,(omng-save (data self)))
           rep))))


