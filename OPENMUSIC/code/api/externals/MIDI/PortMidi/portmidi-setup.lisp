;;;==============================
;;; PORTMIDI PORTS SETUP TOOL
;;;==============================

;;; SETTINGS =
;;; (((IN1 ("IN-DEVICE-NAME" ...))
;;;  ...)
;;; ((OUT1 ("OUT-DEVICE-NAME" ...))
;;;  ...))

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
  (portmidi-restart)
  (unless (pm-time-started) (pm-time-start))
  (let ((pm-devices (list-devices)))
    ;;; IN
    (loop for item in (car settings) do
          (loop for indevice in (cadr item) do 
                (let ((stream (nth 2 (find indevice *portmidi-in-ports-table* :key 'cadr :test 'string-equal))))
                  (unless stream  ;;; no stream is open for this device : create stream and open it
                    (let ((device-id (car (find-if #'(lambda (device) (and (string-equal (getf (cdr device) :name) indevice) (getf (cdr device) :input))) pm-devices))))
                      (when device-id 
                        (setf stream (pm::pm-open-input device-id *portmidi-def-buffer-size*))
                        )))
                  (print (format nil "PortMIDI :: INPUT port ~D => ~A" (car item) indevice))
                  (push (list (car item) indevice stream) *portmidi-in-ports-table*) ;;; add this port/stream pair in the table
                  )))

    ;;; OUT
    (loop for item in (cadr settings) do
          (loop for outdevice in (cadr item) do 
                (let ((stream (nth 2 (find outdevice *portmidi-out-ports-table* :key 'cadr :test 'string-equal))))
                  (unless stream ;;; no stream is open for this device : create stream and open it
                    (let ((device-id (car (find-if #'(lambda (device) (and (string-equal (getf (cdr device) :name) outdevice) (getf (cdr device) :output))) pm-devices))))
                      (when device-id 
                        (setf stream (pm::pm-open-output device-id *portmidi-def-buffer-size* 0)))))
                  (print (format nil "PortMIDI :: OUTPUT  port ~D => ~A" (car item) outdevice))
                  (push (list (car item) outdevice stream) *portmidi-out-ports-table*)   ;;; add this port/stream pair in the table
                  ))
          )
    t))


(defun get-input-stream-from-port (port)
   (when port
      (let ((device (find port *portmidi-in-ports-table* :key 'car :test '=)))
        (values (nth 2 device) (nth 1 device)))))

(defun get-output-stream-from-port (port)
  (when port
    (let ((device (find port *portmidi-out-ports-table* :key 'car :test '=)))
      (values (nth 2 device) (nth 1 device)))))


(defmethod portmidi-setup (settings &optional action)
  (show-portmidi-dialog settings action))

(defclass portmidi-ports-dialog (oa::om-dialog) 
  ((portviews :accessor portviews :initform nil :initarg :portviews)
   (settings :accessor settings :initform nil :initarg :settings)))

(defclass portmidi-ports-view (oa::om-view) 
  ((portlines :accessor portlines :initform nil :initarg :portlines)
   (direction :accessor direction :initform nil :initarg :direction)))



(defmethod set-portmidi-connection-view ((self portmidi-ports-view) dialog)
  (let ((devices (remove nil (loop for ref in (list-devices) 
                                   when (nth (if (equal (direction self) :in) 6 8) ref) 
                                   collect (nth 4 ref))))
        (pos-in-settings (if (equal (direction self) :in) 0 1)))
    (oa::om-with-delayed-update self
      (apply 'oa::om-remove-subviews (cons self (portlines self)))

      (when (nth pos-in-settings (settings dialog))
        (apply 'oa::om-add-subviews 
               (cons self
                     (setf (portlines self)
                           (loop for portsetting in (sort (nth pos-in-settings (settings dialog)) '< :key 'car)
                                 for i = 0 then (+ i 1) collect
                                 (let* ((y (+ 50 (* i 25)))
                                        (vv (oa::om-make-view 'oa::om-view 
                                                              :size (oa::om-make-point 400 25)
                                                              :position (oa::om-make-point 20 y))))
                                                        ;(when (> y 200) (oa::om-set-view-size self (oa::om-make-point (oa::om-width self) (+ y 200))))
                                   (oa::om-add-subviews vv
                                                        (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 60 3) (oa::om-make-point 30 20)
                                                                                 (format nil "~D" (car portsetting))
                                                                                 :font oa::*om-default-font2b*)
                                                                              
                                                        (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 0 0) (oa::om-make-point 40 20) "-"
                                                                                 :di-action (let ((n (car portsetting)))
                                                                                              (oa::om-dialog-item-act button
                                                                                                (setf (nth pos-in-settings (settings dialog)) 
                                                                                                      (remove n (nth pos-in-settings (settings dialog)) :key 'car :test '=))
                                                                                                (set-portmidi-connection-view self dialog)
                                                                                                )))
                                                                              
                                                        (oa::om-make-dialog-item 'oa::om-pop-up-dialog-item (oa::om-make-point 100 0) (oa::om-make-point 220 20) ""
                                                                                 :range (cons "[disconnected]" devices)
                                                                                 :value (car (cadr portsetting)) ;; device for this port  
                                                                                 :di-action (let ((p (position (car portsetting) (nth pos-in-settings (settings dialog)) 
                                                                                                               :test '= :key 'car))
                                                                                                  (port (car portsetting)))
                                                                                              (oa::om-dialog-item-act list
                                                                                                ;(print (format nil "PORTMIDI ~A :: PORT ~D = ~A" 
                                                                                                ;               (direction self) port (oa::om-get-selected-item list)))
                                                                                                (setf (nth p (nth pos-in-settings (settings dialog)))
                                                                                                      (list port
                                                                                                            (if (= 0 (oa::om-get-selected-item-index list)) nil
                                                                                                              (list (oa::om-get-selected-item list)))))
                                                                                                )))
                                                        )
                                   vv)
                                 )))
               )))))

(defun show-portmidi-dialog (settings &optional action)
  (let ((dd (oa::om-make-window 'portmidi-ports-dialog 
                                :window-title "PortMIDI Setup"
                                :bg-color oa::*om-light-gray-color*
                                :size (oa::om-make-point 800 310)
                                :resizable nil
                                :settings settings
                                ))
        (inv (oa::om-make-view 'portmidi-ports-view :position (oa::om-make-point 10 10)
                               :size (oa::om-make-point 380 210)
                               :direction :in))
        
        (outv (oa::om-make-view 'portmidi-ports-view :position (oa::om-make-point 400 10)
                                :size (oa::om-make-point 380 210)
                                :direction :out)))
    
    (oa::om-add-subviews inv 
                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 60 10) (oa::om-make-point 80 20) "In"
                                                  :font oa::*om-default-font2b*)
                         
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 20 5) (oa::om-make-point 40 20) "+"
                                                  :di-action #'(lambda (item) 
                                                                 (let ((newport 0))
                                                                   (loop while (find newport (car (settings dd)) :test '= :key 'car)
                                                                         do (setf newport (1+ newport)))
                                                                   (setf (settings dd) (list 
                                                                                        (append (car (settings dd)) (list (list newport nil)))
                                                                                        (cadr (settings dd))))
                                                                   (set-portmidi-connection-view inv dd)
                                                                   )))
                         
                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 120 10) (oa::om-make-point 120 20) "Input Devices"
                                                  :font oa::*om-default-font2b*)
                         )
    (oa::om-add-subviews outv 
                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 60 10) (oa::om-make-point 80 20) "Out"
                                                  :font oa::*om-default-font2b*)
                         
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 20 5) (oa::om-make-point 40 20) "+"
                                                  :di-action #'(lambda (item) 
                                                                 (let ((newport 0))
                                                                   (loop while (find newport (cadr (settings dd)) :test '= :key 'car)
                                                                         do (setf newport (1+ newport)))
                                                                   (setf (settings dd) (list 
                                                                                        (car (settings dd))
                                                                                        (append (cadr (settings dd)) (list (list newport nil)))
                                                                                        ))
                                                                   (set-portmidi-connection-view outv dd)
                                                                   )))
                         
                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 120 10) (oa::om-make-point 120 20) "Output Devices"
                                                  :font oa::*om-default-font2b*)
                         )

    (setf (portviews dd) (list inv outv))
    (oa::om-add-subviews dd inv outv) 
    (set-portmidi-connection-view inv dd)
    (set-portmidi-connection-view outv dd)
  
    (oa::om-add-subviews dd 
                         
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 20 265) (oa::om-make-point 130 20) "Refresh Devices"
                                                  :di-action #'(lambda (item) 
                                                                 ;;;(portmidi-restart)
                                                                 (when action (funcall action))
                                                                 (portmidi-connect-ports (settings dd))
                                                                 (set-portmidi-connection-view inv dd)
                                                                 (set-portmidi-connection-view outv dd)
                                                                 ))
                                                  
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 700 240) (oa::om-make-point 80 20) "Cancel"
                                                  :di-action #'(lambda (item) (oa::om-return-from-modal-dialog dd nil)))
                         
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 700 265) (oa::om-make-point 80 20) "OK"
                                                  :di-action #'(lambda (item) (oa::om-return-from-modal-dialog dd (settings dd))))
                         )
    
    (oa::om-modal-dialog dd)
    ))






