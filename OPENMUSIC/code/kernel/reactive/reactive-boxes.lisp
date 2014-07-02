(in-package :om)

;;; REACTIVE BEHAVIOR (ACTIVE BOXES):
;;; THE BOX AUTOMATICALLY EVALUATES when the INPUT of a box changes
;;;    - DIRECT input change (menu or user input)
;;;    - IF the value of the connected box changed (REQUIRES NOTIFICATION)

;;; WHEN A BOX IS SET REACTIVE OR WHEN IT IS REACTIVE AND GETS CONNECTED TO A BOX, IT REGISTERS AMONG THE CLIENTS

;;; NOTIFICATION BEHAVOR (ALL BOXES):
;;; When the value change (evaluation or user edition, e.g. inthe editors)
;;; ALL CLIENTS ARE NOTIFIED

;;; - DURING NOTIFICATION THE BOX CAN NOT BE REEVALUATED: IT IS TEMPORARILY LOCKED
;;; (works if NOTIFICATIONS ARE SINGLE THREADED AND SYNCHRONOUS)

;;; THE VALUE OF FUNCTION BOXES IS STORED ONLY DURING THE NOTIFICATION(S)

;;;=============================
;;; ReactiveBox:
;;; Any box participating to the reactive process (either "actively" or not)
;;;=============================

(pushnew :om-reactive *features*)



;;; use this superclass instead of OMBox if needed
(defclass OMReactiveBox (OMBox) 
  ((active :initarg :active :accessor active :initform nil)
   (listeners :initarg :listeners :accessor listeners :initform nil)
   ;;; STATE-LOCK = this box is the event source and his evaluation is locked
   (state-lock :initarg :state-lock :accessor state-lock :initform nil) 
   ;;; GEN-FLAG = this box has already been valuated during this generation
   (gen-flag :initarg :gen-flag :accessor gen-flag :initform nil)
   ;;; PUSH-TAG = this box is tagged as being is in the notification path for the current event
   (push-tag :initarg :push-tag :accessor push-tag :initform nil)
   (color :initarg :color :accessor color :initform nil)
   ))


(defmethod active ((self t)) nil)
(defmethod listeners ((self t)) nil)
(defmethod (setf listeners) (a b) nil)

;;; ACTIVE STATE APPEARS IN BOX COPY/SAVE
;;; => SEE UPDATE-BOXES

(defmethod set-active ((self t) react) (om-beep))

(defmethod set-active ((self OMReactiveBox) react) 
  (when *reactive-patches*
    (setf (active self) react)))

;;; called in box copy
(defmethod update-boxes ((oldbox OMReactiveBox) (newbox OMReactiveBox))
  (let ((newbox (call-next-method)))
    (set-active newbox (active oldbox))
    newbox))
  

;;; PROBLEM HERE ?
(defmethod omNG-save :around ((self OMReactiveBox) &optional (values? t)) 
  `(let ((box ,(call-next-method)))
     (when (fboundp 'set-active) (set-active box ,(active self)))
     box))

;;; AUTRES 
;(defclass box-repeat-n-call (OMReactiveBox OMBoxcall) ())
;;; A FAIRE: OMLOOP, SEQUENCE, OMIF....


#|
(defmethod handle-key-event ((self patchPanel) (char (eql '#\k)))
  (mapcar #'(lambda (boxframe) 
              (list (name (object boxframe))
                           (listeners (object boxframe)))
              )
          (get-actives self)))

(defmethod remove-lock-button ((self omboxframe))
   "Remove the button subview from self."
   (om-remove-subviews (iconview self) (lock-button self))
   (om-invalidate-view self)
   (setf (lock-button self) nil)
   (setf (allow-lock (object self)) nil)
   ;(setf (value (object self)) nil)
   )
|#





;;; AN EXTERNAL EVENT HAPPENS
(defmethod signal-event ((self OMReactiveBox))
  (setf (state-lock self) t)
  (OMR-Notify self)
  (setf (state-lock self) nil))

;;; SELF-NOTIFICATION (REEVALUATES/SIGNALS ON A SEPARATE THREAD)
(defmethod self-notify (box &optional (separate-thread t))
  (when (active box)
    (let ((panel (and (car (frames box))
                      (panel (om-view-window (car (frames box)))))))
      (funcall 
       (if separate-thread 'om-eval-enqueue 'eval)
       (if panel    
           `(progn
              (setf *cur-eval-panel* ,panel)
              (omng-box-value ,box)
              (signal-event ,box)
              (clear-ev-once ,panel))
         `(omr-notify ,box)
         )))))

(defmethod OMR-Notify ((self t)) nil)

;(defun propagate-push-tag (box)
;  (mapcar #'propagate-push-tag-to-listeners (remove-if-not 'active (remove nil (listeners box)))))

;(defun propagate-push-tag-to-listeners (box)
;  (unless (state-lock box)
;    (setf (push-tag box) t)
;    (box-color box (color box) 0)
;    (mapcar #'propagate-push-tag-to-listeners (remove-if-not 'active (remove nil (listeners box))))))


;;; REACTIVE BEHAVIOUR
;;; DO NOT NOTIFY WHO JUST CALLED ME
(defmethod OMR-Notify ((self OMReactiveBox))
  ;(print (list "NOTIFIED BOX" (name self)))
  ;(box-color self *notify-color*)
  (unless (push-tag self)
    (setf (push-tag self) t)
    (let ((listeners (remove-if-not 'active (listeners self))))
      (if (and (active self) listeners)
          (mapcar 'omr-notify listeners)
        (omNG-box-value self))))
  (box-color self nil))


;;;=====================================
;;; SOURCES
;;;=====================================

;;; not implemented yet
(defun om-push (event) (print EVENT))




