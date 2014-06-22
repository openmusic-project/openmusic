
;;; UTILITY SUPERCLASSES FOR OBJECTS IN OM PROGRAMS

(in-package :om)

(defclass named-object ()
  ((name :initform nil :initarg :name :accessor name)))

(defmethod set-name ((self named-object) newname)
  (setf (name self) newname))

(defmethod get-name ((self named-object))
  (name self))

(defmethod set-name ((self t) newname) nil)
(defmethod get-name ((self t)) nil)



(defclass object-in-box ()
  ((associated-box :initform nil :accessor associated-box)))
  
(defmethod (setf value) :after ((value object-in-box) (self OMBoxEditCall)) 
  (setf (associated-box value) self))

(defmethod notify-change ((self object-in-box))
  (when (associated-box self)
    (update-if-editor (associated-box self))))
