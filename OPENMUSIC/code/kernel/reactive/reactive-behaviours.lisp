
;;; EXTENSION OF STANDARD OM BEHAVIOURS DUE TO REACTIVE BOXES
(in-package :om)

;;; Use this to set the delay in box evaluation/notification
(defparameter *defcolortime* nil)


;;; CONNECTION/ REGISTRATION OF CLIENTS
;;; called when a connection is made by subclasses of OMReactiveBox 
;;; if they call-next-method
(defmethod connect-ctrl :after ((self OMReactiveBox) input numout)
  (let ((box (box-ref input)))
    (if box
        (unless (find box (listeners self) :test 'equal)
          (push box (listeners self)))
      (print (format nil "Warning -- Connecting box ~A: target input has no box reference." (name self))))))

(defmethod remove-as-listener ((self t) &optional index) nil)

(defmethod remove-as-listener ((self OMReactiveBox) &optional index)
  (if index 
      (let ((connected (car (connected? (nth index (inputs self))))))
        (setf (listeners connected)
              (remove self (listeners connected) :test 'equal)))
    (mapcar #'(lambda (in)
                (when (connected? in)
                  (setf (listeners (car (connected? in)))
                      (remove self (listeners (car (connected? in))) :test 'equal))
                ))
            (inputs self))
    ))

;;; CONNECTION IS REMOVED  
(defmethod remove-connection :after ((self omboxframe) index)
  (remove-as-listener (object self) index))

(defmethod delete-connections-with-other-boxes :after (deletedboxframe otherframes patchpanel)
  (remove-as-listener (object deletedboxframe)))


;(defmethod xxx ((self note)) (print "normal"))
;(defmethod xxx :before ((self note)) (print "before"))
;(defmethod xxx :after ((self note)) (print "after"))
;(defmethod xxx :around ((self note)) (print "around") (call-next-method))
;(defmethod xxx ((self nnn)) (print "NNN"))
;(defclass nnn (note) ()) 
;(xxx (make-instance 'nnn))

;;; NOTIFICATION TO CLIENTS
;;; When the box is evaluated


(defparameter *inactive-color* (om-make-color 0.8 0.5 0.5))

(defparameter *eval-color* (om-make-color 0.9 0.6 0.6))
(defparameter *notify-color* (om-make-color 0.5 0.6 0.7))


(defmethod box-color (box color &optional wait)
  (when *defcolortime*
    (setf (color box) color)
    (when (car (frames box))
      (om-redraw-view (car (frames box)))
      (om-invalidate-view (car (frames box)))
      (when wait
        (sleep (or wait *defcolortime*))))))
  



(defmethod box-color ((box OMBoxTypeCall) color &optional wait)
    (when *defcolortime*
      (when (car (frames box))
        (let ((prev-color (om-get-bg-color (iconview (car (frames box))))))
          (om-set-bg-color (iconview (car (frames box))) color)
          (when wait
            (sleep (or wait *defcolortime*)))
          (om-set-bg-color (iconview (car (frames box))) prev-color)
    ;(om-redraw-view (iconview (car (frames box))))
          (om-invalidate-view (iconview (car (frames box))))
          ))))


(defmethod draw-before-box :after ((self omboxframe)) 
  (draw-active-state self))  ;;; reactive state)

(defparameter *reactive-color* (om-make-color-alpha 0.6 0.4 0.4 0.2))

(defmethod draw-active-state ((self OMBoxFrame))
  (om-with-focused-view self
    (when (or (active (object self)) (color (object self)))
      (om-with-fg-color nil (or (color (object self)) *reactive-color*)
        ;(if (active (object self))
            (om-fill-rect 0 0 (1- (w self)) (- (h self) 4))
          ;(om-draw-rect 0 4 (1- (w self)) (- (h self) 8))
        ;  )
        ))
    ;(when (push-tag (object self))
    ;  (om-with-fg-color nil *om-black-color*        
    ;    (om-fill-rect 0 0 6 6)))
    ;(when (gen-flag (object self))
    ;  (om-with-fg-color nil *om-red-color*        
    ;    (om-fill-rect 8 0 6 6)))
    ;(when (state-lock (object self))
    ;  (om-with-fg-color nil *om-purple-color*        
    ;    (om-fill-rect 16 0 6 6)))
    ))


(defmethod clear-ev-once :around ((self OMReactiveBox))
  (call-next-method)
  (setf (state-lock self) nil)
  (setf (gen-flag self) nil)
  (setf (push-tag self) nil)
  (setf (color self) nil)
  (when (car (frames self)) (om-invalidate-view (car (frames self)))))



(defmethod omNG-box-value :around ((self OMReactiveBox) &optional (numout 0)) 
  (if (state-lock self)
      (current-box-value self numout)
   (let (val)
     ;(print (list "EVAL BOX" (name self) numout))
     (box-color self *eval-color* *defcolortime*)   
     (setf val (call-next-method))
     ;(print val)
     (setf (gen-flag self) t)
     (box-color self nil) ; *inactive-color*
     val)
   )
 )



;;; TRIGGER NOTIFICATIONS 

(defmethod eval-box :around ((self omboxframe)) 
  (call-next-method)
  (signal-event (object self)))

;;; when an input is edited
(defmethod exit-from-dialog ((self input-text-enter-view) newtext)
  (call-next-method)
  (let ((box (object (om-view-container (object self)))))
    (self-notify box)))

;;; EDITOR NOTIFICATION (when the editor is modified by user actions)
(defmethod report-modifications :after ((self EditorView))
  (when (is-boxpatch-p (ref self))
    (self-notify (ref self))))

;;; BoxType edited
(defmethod exit-from-dialog ((self BoxType-enter-view) newtext)
  (call-next-method)
  (self-notify (object (om-view-container (object self))))
  )


(defmethod set-delivered-value :after ((box ReceiveBox) msg)
  (self-notify box nil))

(defmethod set-active ((self ReceiveBox) react) 
  (call-next-method)
  (if react ;; => SET REACTIVITY ON
      (when (and (not (etat self))  ;;; not running
                 (start-receive-fun (reference self)))
        (funcall (start-receive-fun (reference self)) self))
    (when (and (etat self) ;; running
               (stop-receive-fun (reference self)))
      (funcall (stop-receive-fun (reference self)) self))))

