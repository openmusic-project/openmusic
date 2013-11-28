
(in-package :om)

(om::defmethod! osc-receive (port msg-processing)
  :icon 611
  :indoc '("port number" "incoming message processing patch")
  :initvals '(3000 nil)
  :doc "A local OSC server.
 
At evaluation, the OSC-RECEIVE box should turn on/off (highlighted or not).
When the server is on, OSC-RECEIVE waits for OSC messages to come and calls <patch> with the decoded message as parameter.

<msg-processing> must be a patch in mode 'lambda' with 1 input corresponding to an OSC message. 
This patch should handle and process the incoming messages.
"


  t)

(defclass ReceiveBox (OMBoxCall) 
  ((etat :initform nil :initarg :etat :accessor etat)
   (process :initform nil :initarg :process :accessor process)))

(defmethod get-boxcallclass-fun ((self (eql 'osc-receive))) 'ReceiveBox)

(defmethod allow-lock-button ((self ReceiveBox))  nil)

(defmethod set-delivered-value ((box ReceiveBox) msg)
  (setf (value box) (list (reverse msg))))


;(defmethod call-gen-code ((self ReceiveBox) numout)
;   (declare (ignore numout))
;   `(if ,(gen-code (first (inputs self)) 0) ,(gen-code (second (inputs self)) 0) ,(gen-code (third (inputs self)) 0)))

;(defmethod gen-code-call ((self ReceiveBox))
;   (call-gen-code self 0))
 
(defmethod omNG-box-value ((self ReceiveBox) &optional (numout 0))
  (value self))


(defclass ReceiveBoxFrame (boxframe) ())

(defmethod get-frame-class ((self ReceiveBox)) 'ReceiveBoxFrame)

(defmethod om-get-menu-context ((self ReceiveBoxFrame))
  (append (list (list 
                 (if (etat (object self)) 
                     (om-new-leafmenu  "STOP RECEIVE" #'(lambda () (osc-stop-receive (object self))))
                   (om-new-leafmenu  "START RECEIVE" #'(lambda () (osc-start-receive (object self)))))))
          (call-next-method)))

(defmethod draw-after-box ((self ReceiveBoxFrame)) 
   (when (etat (object self))
     (om-with-focused-view self
       (om-draw-hilite-rect 0 0 (w self) (h self)))))

(defmethod OpenEditorframe ((self ReceiveBox)) (om-beep))
 

;;;========================================
;;; oSC features
;;;========================================

(defmethod osc-start-receive ((box ReceiveBox))
  (when (etat box) (osc-stop-receive box))
  (let ((port (omng-box-value (car (inputs box)))))
    (if (and port (numberp port))
        (let ((fun (omng-box-value (cadr (inputs box)))))
          (setf (process box) 
                (om-start-osc-server port "localhost"
                                     #'(lambda (msg) 
                                         (let* ((message (om-decode-msg-or-bundle msg)))
                                           ;(print (format nil "OSC RECEIVE= ~A" message))
                                           (let ((delivered (deliver-bundle message fun)))
                                             ;(loop for mess in delivered do
                                             (set-delivered-value box delivered)
                                             ;
                                             )
                                           )
                                         nil)
                                     ))
          (print (format nil "OSC-RECEIVE START on port ~D" port))
          (setf (etat box) t))
      (om-beep-msg (format nil "Error - bad port number for OSC-RECEIVE: ~A" port)))
    (when  (car (frames box))
      (om-invalidate-view (car (frames box)) t))))


(defmethod osc-stop-receive ((box ReceiveBox))
  (when (process box)
    (om-stop-osc-server (process box))
    (print (format nil "OSC-RECEIVE STOP on port ~D" (omng-box-value (car (inputs box))))))
  (setf (etat box) nil)
  (when (car (frames box))
    (om-invalidate-view (car (frames box)) t)))


(defun deliver-bundle (bundle patch)
  (let ((tt nil))
    (if (or (and (arrayp (car bundle)) (not (stringp (car bundle)))) 
            (consp (car bundle)))
        ;;; LOOKS LIKE OSC...
        (loop for item in bundle 
              unless (and (arrayp item) (not (stringp item)) (setf tt item)) ;;; THIS IS THE TIME TAG
              collect
              (if (and (consp item) (consp (car item))) ;;; THE FIRST ELEMENT OF THE MESSAGE IS ANOTHER MESSAGE
                  (deliver-bundle (if tt (cons tt (car item)) (car item)) patch)
                (deliver-message item patch))
              )
      ;;; NOT OSC
      (list (deliver-message bundle patch)))))

;;----------------------------------------------------------------------------
;; We provide a real bad-boy feature here : if a message is prefixed with the
;; selector "/lisp", the rest of the message is executed as a normal lisp
;; function call.
;;-----------------------------------------------------------------------------
;;; ((string-equal (car item) "/lisp") (execute-message item))
;;; ((string-equal (car item) "/lisp-string") (execute-message-from-string item))



(defmethod deliver-message (message (fun OMPatch))
  (apply (intern (string (code fun)) :om) (list message)))

(defmethod deliver-message (message (fun null)) message)

(defmethod deliver-message (message (fun function)) 
  (apply fun (list message)))

(defmethod deliver-message (message (fun symbol))
  (when (fboundp fun) (apply fun (list message))))

(defun execute-message (message)
  (apply (if (symbolp (cadr message)) (cadr message) (read-from-string (cadr message))) (cddr message)))

(defun execute-message-from-string (message)
 (eval (read-from-string (cadr message)))
)


