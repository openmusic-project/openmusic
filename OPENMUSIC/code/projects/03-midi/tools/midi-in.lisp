
(in-package :om)

(om::defmethod! midi-in (port msg-processing-fun)
  :icon 917
  :indoc '("port number" "incoming message processing patch")
  :initvals '(0 nil)
  :doc "A MIDI IN server.
 
Right-click and select the appropriate option to turn on/off.
When the server is on, MIDI-IN waits for MIDI messages on port <port> and calls <msg-processing-fun> with the decoded message as parameter.

<msg-processing> must be a patch in mode 'lambda' with 1 input corresponding to an OSC message. 
This patch should handle and process the incoming messages.
"
  t)


(defmethod get-boxcallclass-fun ((self (eql 'midi-in))) 'ReceiveBox)

(defmethod start-receive-fun ((self (eql 'midi-in))) 'start-midi-in)
(defmethod stop-receive-fun ((self (eql 'midi-in))) 'stop-midi-in)

;;;========================================
;;; oSC features
;;;========================================

(defmethod start-midi-in ((box ReceiveBox))
  (when (etat box) (osc-stop-receive box))
  (let ((port (omng-box-value (car (inputs box)))))
    (if (and port (numberp port))
        (let ((fun (or (omng-box-value (cadr (inputs box)))
                       (fdefinition 'identity))))
          (setf (process box) 
                (midi-in-start port
                               #'(lambda (message time)
                                   (let ((msg (deliver-message (msg-to-midievent message port) fun)))
                                     (when msg (set-delivered-value box msg))
                                     ))
                               1)
                )
          (print (format nil "MIDI IN START on port ~D" port))
          (setf (etat box) t))
      (om-beep-msg (format nil "Error - bad port number for MIDI IN: ~A" port)))
    (when  (car (frames box))
      (om-invalidate-view (car (frames box)) t))))


(defmethod stop-midi-in ((box ReceiveBox))
  (when (process box)
    (midi-in-stop (process box))
    (print (format nil "MIDI IN STOP on port ~D" (omng-box-value (car (inputs box))))))
  (setf (etat box) nil)
  (when (car (frames box))
    (om-invalidate-view (car (frames box)) t)))



(defun msg-to-midievent (event &optional p)
  (make-instance 'midievent 
                 :ev-type (om-midi::midi-evt-type event)
                 :ev-fields (om-midi::midi-evt-fields event)
                 :ev-chan (om-midi::midi-evt-chan event)
                 :ev-port p))




