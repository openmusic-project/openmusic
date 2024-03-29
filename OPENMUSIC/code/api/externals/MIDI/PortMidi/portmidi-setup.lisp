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
  (format om-lisp::*om-stream* "MIDI Setup:~%")
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
                  (format om-lisp::*om-stream* "  MIDI-IN port ~D => ~A~%" (car item) indevice)
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
                  (format om-lisp::*om-stream* "  MIDI-OUT port ~D => ~A~%" (car item) outdevice)
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

(defvar *portmidi-setup-window* nil)

(defclass portmidi-ports-dialog (oa::om-window) 
  ((portviews :accessor portviews :initform nil :initarg :portviews)
   (settings :accessor settings :initform nil :initarg :settings)))

(defmethod oa::om-window-close-event ((self portmidi-ports-dialog))
  (setf *portmidi-setup-window* NIL))

(defclass portmidi-ports-view (oa::om-scroller) 
  ((portlines :accessor portlines :initform nil :initarg :portlines)
   (direction :accessor direction :initform nil :initarg :direction)))

(defmethod oa::om-resize-callback ((self portmidi-ports-dialog) x y w h)
  (call-next-method)
  (let ((buttons (reverse (oa:om-subviews self)))
        (panel1 (car (portviews self)))
        (panel2 (second (portviews self))))
    (when (car buttons) 
      (oa::om-set-view-position (car buttons) (oa::om-make-point 680 (abs (- (- h  20) 20))))
      )
    (when (second buttons) 
      (oa::om-set-view-position (second buttons) (oa::om-make-point 575 (abs (- (- h  20) 20))))
      )
    
    (when panel1
      (oa::om-set-view-size panel1 (oa::om-make-point 380 (- h 90))))
    (when panel2
      (oa::om-set-view-size panel2 (oa::om-make-point 380 (- h 90))))))

(defmethod set-portmidi-connection-view ((self portmidi-ports-view) dialog)
  (let* ((devices (remove nil (loop for ref in (list-devices) 
                                   when (nth (if (equal (direction self) :in) 6 8) ref) 
                                   collect (nth 4 ref))))
         (pos-in-settings (if (equal (direction self) :in) 0 1))
         (settings-list (nth pos-in-settings (settings dialog)))
	 (dy #-linux 25 #+linux 35))
    (oa::om-with-delayed-update self
      (apply 'oa::om-remove-subviews (cons self (portlines self)))
      (oa::om-set-field-size self (oa::om-make-point (oa::om-point-x (oa::om-view-size self)) 
                                                     (+ 20 (* (length settings-list) 25))))
      (when settings-list
        (apply 'oa::om-add-subviews 
               (cons self
                     (setf (portlines self)
                           (loop for portsetting in (sort settings-list '< :key 'car)
                                 for i = 0 then (+ i 1) collect
                                 (let* ((y (+ 10 (* i dy)))
                                        (vv (oa::om-make-view 'oa::om-view 
                                                              :size (oa::om-make-point 400 dy)
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
                                                                                                      (remove n settings-list :key 'car :test '=))
                                                                                                (set-portmidi-connection-view self dialog)
                                                                                                )))
                                                                              
                                                        (oa::om-make-dialog-item 'oa::om-pop-up-dialog-item (oa::om-make-point 100 0) (oa::om-make-point 220 20) ""
                                                                                 :range (cons "[disconnected]" devices)
                                                                                 :value (car (cadr portsetting)) ;; device for this port  
                                                                                 :di-action (let ((p (position (car portsetting) settings-list :test '= :key 'car))
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
  (if *portmidi-setup-window*
      (oa::om-select-window *portmidi-setup-window*)
    (let* ((dd (oa::om-make-window 'portmidi-ports-dialog 
                                   :window-title "PortMIDI Setup"
                                   :bg-color oa::*om-light-gray-color*
                                   :size (oa::om-make-point 800 270)
                                   :resizable t
                                   :external-min-width 800
                                   :external-max-width 800
                                   :external-min-height 270 
                                   :settings settings
                                   ))
        
           (inv (oa::om-make-view 'portmidi-ports-view :position (oa::om-make-point 10 40)
                                  :size (oa::om-make-point 380 180) :scrollbars :v :retain-scrollbars nil
                                  :direction :in))

           (outv (oa::om-make-view 'portmidi-ports-view :position (oa::om-make-point 400 40)
                                   :size (oa::om-make-point 380 180) :scrollbars :v :retain-scrollbars nil
                                   :direction :out)))
    
      (oa::om-add-subviews dd 
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
      (oa::om-add-subviews dd 
                           (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 460 10) (oa::om-make-point 80 20) "Out"
                                                    :font oa::*om-default-font2b*)
                         
                           (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 420 5) (oa::om-make-point 40 20) "+"
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
                         
                           (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 520 10) (oa::om-make-point 120 20) "Output Devices"
                                                    :font oa::*om-default-font2b*)
                           )

      (setf (portviews dd) (list inv outv))
      (oa::om-add-subviews dd inv outv) 
      (set-portmidi-connection-view inv dd)
      (set-portmidi-connection-view outv dd)
  
      (oa::om-add-subviews dd 
                           ;;; DOES NOT WORK...
                           ;(oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 10 230) (oa::om-make-point 130 20) 
                           ;                         "Refresh Devices"
                           ;                         :di-action #'(lambda (item) 
                           ;                                        ;;;(portmidi-restart)
                           ;                                        (when action (funcall action (settings dd)))
                           ;                                        (portmidi-connect-ports (settings dd))
                           ;                                        (set-portmidi-connection-view inv dd)
                           ;                                        (set-portmidi-connection-view outv dd)
                           ;                                        ))
                                                  
                           (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 575 230) (oa::om-make-point 100 20) "Apply"
                                                    :di-action #'(lambda (item) (when action (funcall action (settings dd))))
                                                    )
                           (oa::om-make-dialog-item 'oa::om-button  (oa::om-make-point 680 230) (oa::om-make-point 80 22) "OK" 
                                           :di-action #'(lambda (item) (progn 
                                                                         (when action (funcall action (settings dd)))
                                                                         (oa::om-close-window (oa::om-view-window item))))

                                           )
                           )
      
      (setf *portmidi-setup-window* dd)
      (oa:om-show-window dd)
      )))






