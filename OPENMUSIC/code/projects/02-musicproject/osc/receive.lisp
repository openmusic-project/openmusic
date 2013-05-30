
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



;;-----------------------------------------------------------------------------
;; We need to define a class that will contain the properties of an instance of
;; Moscou:receive box, since each individual box is associated with a background
;; process that sits and waits for OSC messages to arrive.
;;-----------------------------------------------------------------------------
(defclass ReceiveBox (OMBoxCall) 
   ((etat     :initform nil  :initarg :etat      :accessor etat)
    (process  :initform nil  :initarg :process   :accessor process)))



;;-----------------------------------------------------------------------------
;; We then have to redefine a few functions that implement the behaviour of the
;; box. Here, we replace the standard frame class with our freshly defined
;; box class.
;;-----------------------------------------------------------------------------
(defmethod get-frame-class ((self ReceiveBox)) 'ReceiveBoxFrame)



;;-----------------------------------------------------------------------------
;; Call the Moscou:receive method when the box is evaluated.
;;-----------------------------------------------------------------------------
(defmethod get-boxcallclass-fun ((self (eql 'osc-receive))) 'ReceiveBox)



;;-----------------------------------------------------------------------------
;; There is no editor for our box.
;;-----------------------------------------------------------------------------
(defmethod OpenEditorframe ((self ReceiveBox)) 
   (not (dialog-message "Compiled Function RECEIVE")))



;;-----------------------------------------------------------------------------
;; Our box cannot be locked, since it has no result. What is important
;; is what happens during it's execution, not the result it returns.
;;-----------------------------------------------------------------------------
(defmethod allow-lock-button ((self ReceiveBox))  nil)



;;-----------------------------------------------------------------------------
;; (Not really sure what this does. Check with Carlos for that.)
;;-----------------------------------------------------------------------------
(defmethod call-gen-code ((self ReceiveBox) numout)
   (declare (ignore numout))
   `(if ,(gen-code (first (inputs self)) 0) ,(gen-code (second (inputs self)) 0) ,(gen-code (third (inputs self)) 0)))



;;-----------------------------------------------------------------------------
;; (Not really sure what this does. Check with Carlos for that.)
;;-----------------------------------------------------------------------------
(defmethod gen-code-call ((self ReceiveBox))
   (call-gen-code self 0))
 


;;-----------------------------------------------------------------------------
;; This is the function actually executed when the user evaluates the box.
;; It basically starts a process waiting for OSC messages.
;;-----------------------------------------------------------------------------
(defmethod omNG-box-value ((self ReceiveBox) &optional (numout 0))
  (declare (ignore numout))
   (setf (etat self) (not (etat self)))
   (if (etat self)
     (let* ((args (eval-box-inputs self))
            patch)
       (when (second args)
         (progn (setf patch (first (connected? (nth 1 (inputs self)))))
                (setf patch (reference patch))
                (unless (patch-p patch)
                  (error "Second input must be a function symbol or a patch"))))
       (setf (process self) 
         (om-start-osc-server (first args) "localhost"
                              #'(lambda (msg) 
                                (let ((message (om-decode-msg-or-bundle msg)))
                                  (deliver-bundle message patch)
                                  nil))))
       (print (format nil "OSC-RECEIVE START on port ~D"(first args)))
       )
     (progn
       (om-stop-osc-server (process self))
       (print (format nil "OSC-RECEIVE STOP on port ~D" (omNG-box-value (car (inputs self)))))
       ))
  (when  (car (frames self))
    (om-invalidate-view (car (frames self)) t))
   t)



;;-----------------------------------------------------------------------------
;; We need to draw a special frame around our receive box, so that the user
;; sees which state it is in (listening or idle).
;;-----------------------------------------------------------------------------
(omg-defclass ReceiveBoxFrame (boxframe) ())

;;-----------------------------------------------------------------------------
;; Redefinition of the draw-after-box method.
;;-----------------------------------------------------------------------------
(defmethod draw-after-box ((self ReceiveBoxFrame)) 
   (when (etat (object self))
     (om-with-focused-view self
       (om-draw-hilite-rect 0 0 (w self) (h self)))))





;;-----------------------------------------------------------------------------
;; Act when receiving a bundle.
;;
;; We provide a real bad-boy feature here : if a message is prefixed with the
;; selector "/lisp", the rest of the message is executed as a normal lisp
;; function call.
;;-----------------------------------------------------------------------------
(defun deliver-bundle (bundle patch)
  (let ((tt nil))
    (loop for item in bundle collect
          (cond ((and (arrayp item) (not (stringp item))) (setf tt item))
                ((consp item)
                 (cond
                  ((stringp (car item)) (cond ((string-equal (car item) "/lisp") (execute-message item))
                                              ((string-equal (car item) "/lisp-string") (execute-message-from-string item))
                                              ;(t (deliver-message (if tt (list tt item) item) patch))
                                              (t (deliver-message item patch))
                                              ))
                  ((consp (car item)) (deliver-bundle (if tt (cons tt (car item)) (car item)) patch))
                  (t nil)))
                ;(t (deliver-message (if tt (list tt item) item) patch))
                (t (deliver-message item patch))
                )
          )))

(defmethod deliver-message (message (fun OMPatch))
  (apply (intern (string (code fun)) :om) (list message)))

(defmethod deliver-message (message (fun null)) message)

(defmethod deliver-message (message (fun function)) (apply fun (list message)))

(defmethod deliver-message (message (fun symbol))
  (when (fboundp fun) (apply fun (list message))))

(defun execute-message (message)
  (apply (if (symbolp (cadr message)) (cadr message) (read-from-string (cadr message))) (cddr message)))

(defun execute-message-from-string (message)
 (eval (read-from-string (cadr message)))
)


