;;;==============================
;;; PORTMIDI PORTS SETUP TOOL
;;;==============================

;;; PORTMIDI SETTINGS =
;;; ((PORTNUM "OUT-DEVICE-NAME" "IN-DEVICE-NAME")
;;;  (...)
;;;  ...)

(in-package :om-midi)

;;; tables ((port name stream) ...)
(defvar *portmidi-in-ports-table* nil)
(defvar *portmidi-out-ports-table* nil)
(defparameter *portmidi-def-buffer-size* 1024)

(defun portmidi-close-all-midi-ports ()
  (mapcar #'pm::pm-close (mapcar #'third (remove-duplicates *portmidi-in-ports-table* :key 'cadr :test 'string-equal)))
  (mapcar #'pm::pm-close (mapcar #'third (remove-duplicates *portmidi-out-ports-table* :key 'cadr :test 'string-equal)))
  (setf *portmidi-in-ports-table* nil *portmidi-out-ports-table* nil))

(defun portmidi-connect-ports (settings)
  (portmidi-close-all-midi-ports)
  (portmidi-restart)
  (unless (pm-time-started) (pm-time-start))
  (let ((pm-devices (list-devices)))
    (loop for item in settings 
          do
          (loop for indevice in (second item) do 
                (let ((stream (nth 2 (find indevice *portmidi-in-ports-table* :key 'cadr :test 'string-equal))))
                  (unless stream 
                    (let ((device-id (car (find-if #'(lambda (device) (and (string-equal (nth 4 device) indevice) (nth 6 device))) pm-devices))))
                      (when device-id (setf stream (pm::pm-open-input device-id *portmidi-def-buffer-size*)))))
                  (push (list (car item) indevice stream) *portmidi-in-ports-table*)
                  ))
          do
          (loop for outdevice in (third item) do 
                (let ((stream (nth 2 (find outdevice *portmidi-out-ports-table* :key 'cadr :test 'string-equal))))
                  (unless stream 
                    (let ((device-id (car (find-if #'(lambda (device) (and (string-equal (nth 4 device) outdevice) (nth 8 device))) pm-devices))))
                      (when device-id (setf stream (pm::pm-open-output device-id *portmidi-def-buffer-size* 0)))))
                  (push (list (car item) outdevice stream) *portmidi-out-ports-table*)
                  ))
          )
    t))


(defun get-input-stream-from-port (port)
   (when port
     (nth 2 (find port *portmidi-in-ports-table* :key 'car :test '=))))

(defun get-output-stream-from-port (port)
  (when port
    (nth 2 (find port *portmidi-out-ports-table* :key 'car :test '=))))


(defmethod portmidi-setup (settings)
  (show-portmidi-dialog settings))

(defclass portmidi-ports-dialog (oa::om-dialog) 
  ((portviews :accessor portviews :initform nil :initarg :portviews)
   (settings :accessor settings :initform nil :initarg :settings)))

(defmethod set-portmidi-connection-view ((self portmidi-ports-dialog))
  (let ((indevices (remove nil (loop for ref in (list-devices) when (nth 6 ref) collect (nth 4 ref))))
        (outdevices (remove nil (loop for ref in (list-devices) when (nth 8 ref) collect (nth 4 ref)))))
    (oa::om-with-delayed-update self
      (apply 'oa::om-remove-subviews (cons self (portviews self)))
      (when (settings self)
        (apply 'oa::om-add-subviews (cons self
                                          (setf (portviews self)
                                                (loop for portsetting in (sort (settings self) '< :key 'car)
                                                      for i = 0 then (+ i 1) collect
                                                      (let* ((y (+ 50 (* i 25)))
                                                             (vv (oa::om-make-view 'oa::om-view 
                                                                                  :size (oa::om-make-point 400 25)
                                                                                  :position (oa::om-make-point 20 y))))
                                                        ;(when (> y 200) (oa::om-set-view-size self (oa::om-make-point (oa::om-width self) (+ y 200))))
                                                        (oa::om-add-subviews vv
                                                                             ;(oa::om-make-dialog-item 'oa::om-pop-up-dialog-item (oa::om-make-point 0 0) (oa::om-make-point 170 20) ""
                                                                             ;                         :range (cons "[disconnected]" indevices)
                                                                             ;                         :value (car (nth 1 portsetting)) ;; inputs for this port  
                                                                             ;                         :di-action (let ((p (position (car portsetting) (settings self) :test '= :key 'car)))
                                                                             ;                                      (oa::om-dialog-item-act list
                                                                             ;                                        (print (list (car portsetting) "IN=" (oa::om-get-selected-item list)))
                                                                             ;                                        (setf (nth 1 (nth p (settings self))) 
                                                                             ;                                              (if (= 0 (oa::om-get-selected-item-index list)) nil
                                                                             ;                                                (list (oa::om-get-selected-item list))))
                                                                             ;                                        )))

                                                                             (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 60 0) (oa::om-make-point 30 20)
                                                                                                      (format nil "~D" (car portsetting))
                                                                                                      :font oa::*om-default-font2b*)
                                                                              
                                                                             (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 0 -3) (oa::om-make-point 40 20) "-"
                                                                                                      :di-action (let ((n (car portsetting)))
                                                                                                                   (oa::om-dialog-item-act button
                                                                                                                     (setf (settings self) (remove n (settings self) :key 'car :test '=))
                                                                                                                     (set-portmidi-connection-view self)
                                                                                                                     )))
                                                                              
                                                                             (oa::om-make-dialog-item 'oa::om-pop-up-dialog-item (oa::om-make-point 160 0) (oa::om-make-point 220 20) ""
                                                                                                      :range (cons "[disconnected]" outdevices)
                                                                                                      :value (car (nth 2 portsetting)) ;; inputs for this port  
                                                                                                      :di-action (let ((p (position (car portsetting) (settings self) :test '= :key 'car)))
                                                                                                                   (oa::om-dialog-item-act list
                                                                                                                     (print (list (car portsetting) "OUT=" (oa::om-get-selected-item list)))
                                                                                                                     (setf (nth 2 (nth p (settings self))) 
                                                                                                                           (if (= 0 (oa::om-get-selected-item-index list)) nil
                                                                                                                             (list (oa::om-get-selected-item list))))
                                                                                                                     )))
                                                                             )
                                                        vv)
                                                      )))
               )))))


(defun show-portmidi-dialog (settings)
  (let ((dd (oa::om-make-window 'portmidi-ports-dialog 
                                :window-title "PortMIDI Setup"
                                :bg-color oa::*om-light-gray-color*
                                :size (oa::om-make-point 420 310)
                                :resizable nil
                                :settings settings
                                )))
        
    (set-portmidi-connection-view dd)
  
    (oa::om-add-subviews dd 
                     
                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 60 10) (oa::om-make-point 80 20) "Ports"
                                                  :font oa::*om-default-font2b*)
                         
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 20 5) (oa::om-make-point 40 20) "+"
                                                  :di-action #'(lambda (item) 
                                                                 (let ((newport 0))
                                                                   (loop while (find newport (settings dd) :test '= :key 'car)
                                                                         do (setf newport (1+ newport)))
                                                                   (setf (settings dd) (append (settings dd) (list (list newport nil nil))))
                                                                   (set-portmidi-connection-view dd)
                                                                   )))

                         ;(oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 20 10) (oa::om-make-point 100 20) "Input Devices"
                         ;                         :font oa::*om-default-font2b*)

                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 180 10) (oa::om-make-point 120 20) "Output Devices"
                                                  :font oa::*om-default-font2b*)
                         

                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 20 265) (oa::om-make-point 130 20) "Refresh Devices"
                                                  :di-action #'(lambda (item) 
                                                                 (portmidi-restart)
                                                                 (set-portmidi-connection-view dd)
                                                                 ))
                                                  
                         ;(oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 20 265) (oa::om-make-point 130 20) "Apply"
                         ;                         :di-action #'(lambda (item) (portmidi-connect-ports (settings dd))))
                         
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 320 240) (oa::om-make-point 80 20) "Cancel"
                                                  :di-action #'(lambda (item) (oa::om-return-from-modal-dialog dd nil)))
                         
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 320 265) (oa::om-make-point 80 20) "OK"
                                                  :di-action #'(lambda (item) (oa::om-return-from-modal-dialog dd (settings dd))))
                         )
    
    (oa::om-modal-dialog dd)
    ))






