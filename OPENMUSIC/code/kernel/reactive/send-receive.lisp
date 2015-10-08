
(in-package :om)

;;;=====================================
;;; SEND/RECEIVE
;;;=====================================

(defmethod! om-send ((self t) &optional (target :om))
   (let ((boxes (find-receive-boxes target)))
     (mapcar #'(lambda (b)
                 (setf (value b) (list self))
                 (self-notify b nil))
             boxes)))
                 
(defmethod! om-receive (targetname) :initvals '(:om) t)

(defclass OMReceiveBox (OMBoxCall) ())
(defmethod get-boxcallclass-fun ((self (eql 'om-receive))) 'OMReceiveBox)
(defmethod omNG-box-value ((self OMReceiveBox) &optional (numout 0)) 
  (let ((inval (omng-box-value (car (inputs self)))))
    (unless (equal inval (value (car (inputs self))))
      (print (format nil "RECEIVE ID SET TO: ~A" inval))
      (setf (value (car (inputs self))) inval)))
  (if numout (nth numout (value self)) (value self)))
  
;(defmethod omNG-box-value ((self OMReceiveBox) &optional (numout 0))
;  (if numout (nth numout (value self)) (value self)))

(defun find-boxes (type)
  (loop for win in (remove-if-not 
                    #'(lambda (w) (equal 'patcheditor (type-of (editor w))))
                    (om-get-all-windows 'editorwindow)) append
        (loop for b in (boxes (object (editor win))) 
              when (equal type (reference b))
              collect b)))

(defun find-receive-boxes (target)
  (let ((boxes (find-boxes 'om-receive)))
    (remove-if-not #'(lambda (b) (equal (value (nth 0 (inputs b))) target))
                   boxes)
    ))


