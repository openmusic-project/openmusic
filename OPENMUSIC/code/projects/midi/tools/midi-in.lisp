
(in-package :om)

(om::defmethod! midi-in (port msg-processing-fun thru-to)
  :icon 917
  :indoc '("port number" "incoming message processing patch" "redirect input to output num")
  :initvals '(0 nil nil)
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


(defparameter *running-midi-boxes* nil)

(defmethod start-midi-in ((box ReceiveBox))
  
  (when (and 
         (etat box)
         (check-def-midi-system 'om-midi::midi-in-stop-function nil)
         )
    (stop-midi-in box))
  
  (let ((port (omng-box-value (car (inputs box))))
        (redirect (and (caddr (inputs box)) (omng-box-value (caddr (inputs box))))))
    (cond 
     ((or (null port) (not (numberp port)))
          (om-beep-msg (format nil "Error - bad port number for MIDI IN: ~A" port)))
     
     ((check-def-midi-system 'om-midi::midi-in-start-function nil)
      (let ((fun (or (omng-box-value (cadr (inputs box)))
                         (fdefinition 'identity))))
        (setf (process box) 
              (midi-in-start port
                             #'(lambda (message time)
                                 (let ((msg (deliver-message (msg-to-midievent message port) fun)))
                                   (when msg (set-delivered-value box msg))
                                   ))
                             1
                             redirect
                             )
              )
        (push box *running-midi-boxes*)
        (when (process box)
          (print (format nil "MIDI IN START on port ~D" port))
          (setf (etat box) t)))
      ))
    (when  (car (frames box))
      (om-invalidate-view (car (frames box)) t))))


(defmethod stop-midi-in ((box ReceiveBox))
  (when (process box)
    (midi-in-stop (process box))
    (print (format nil "MIDI IN STOP on port ~D" (omng-box-value (car (inputs box))))))
  (setf (etat box) nil)
  (when (car (frames box))
    (om-invalidate-view (car (frames box)) t))
  (setf *running-midi-boxes* (remove box *running-midi-boxes*))
  )



(defun msg-to-midievent (event &optional p)
  (make-instance 'midievent 
                 :ev-type (om-midi::midi-evt-type event)
                 :ev-fields (om-midi::midi-evt-fields event)
                 :ev-chan (om-midi::midi-evt-chan event)
                 :ev-port p))




