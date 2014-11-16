

(in-package :om-midi)

;;;==============================
;;; MIDISHARE SETUP TOOLS
;;;==============================


(defmethod midishare-setup (settings &optional action)
  (declare (ignore settings))
  #+(or powerpc win32) (launch-midishare-setup-app)
  #-(or powerpc win32) (make-midishare-setup-dialog))


(defmethod midishare-connect-ports (settings)
   #-(or powerpc win32)
   (progn
     (sleep 0.5)
     (oa::om-without-interrupts (restore-midishare-connections settings))))



#|
;;;==================================================
;;; COLLECTS DETECTED CONNECTIONS
(defun get-midishare-connections ()
  (let ((connections nil)
        (portinfo nil))
    (loop for p from 0 to 255 do
          (setf portinfo (midishare-get-connections p))
          (when (or (car portinfo) (cadr portinfo))
              (push (list p (mapcar 'cadr (car portinfo)) (mapcar 'cadr (cadr portinfo))) connections)))
    connections))

;;;==================================================
;;; RESTORE THE CONNECTIONS
(defun restore-midishare-connections (connection-data) 
  (let ((inslots (remove nil (loop for ref in (midishare-get-drivers) append (nth 1 (midishare-driver-info ref)))))
        (outslots (remove nil (loop for ref in (midishare-get-drivers) append (nth 2 (midishare-driver-info ref))))))
    
    ;(print "RESTORING MIDI CONNECTIONS")
    ;(print connection-data)
    
    ; déconnecte tout
    (loop for p from 0 to 255 do
          (loop for sss in (append inslots outslots) do
                (midishare-unconnect-slot (car sss) p)))

    (loop for port-connect in connection-data do
          
          (loop for inslot in (nth 1 port-connect) do
                (let ((slt (find inslot inslots :key 'cadr :test 'string-equal)))
                  (when slt (midishare-connect-slot (car slt) (car port-connect)))))
          (loop for outslot in (nth 2 port-connect) do
                (let ((slt (find outslot outslots :key 'cadr :test 'string-equal)))
                  (when slt (midishare-connect-slot (car slt) (car port-connect)))))

          )))
|#


;;; NOW IN AND OUT POORTS ARE DIFFERENT

;;;==================================================
;;; COLLECTS DETECTED CONNECTIONS
(defun get-midishare-connections ()
  (let ((inconnections nil)
        (outconnections nil)
        (portinfo nil))
    (loop for p from 0 to 255 do
          (setf portinfo (midishare-get-connections p))
          (when (car portinfo)
              (push (list p (mapcar 'cadr (car portinfo))) inconnections))
          (when (cadr portinfo)
            (push (list p (mapcar 'cadr (cadr portinfo))) outconnections)))
    (list inconnections outconnections)))

; (get-midishare-connections)

;;;==================================================
;;; RESTORE THE CONNECTIONS
(defun restore-midishare-connections (connection-data) 
  (let ((inslots (remove nil (loop for ref in (midishare-get-drivers) append (nth 1 (midishare-driver-info ref)))))
        (outslots (remove nil (loop for ref in (midishare-get-drivers) append (nth 2 (midishare-driver-info ref))))))
    
    ;(print "RESTORING MIDI CONNECTIONS")
    ;(print connection-data)
    
    ; déconnecte tout
    (loop for p from 0 to 255 do
          (loop for sss in (append inslots outslots) do
                (midishare-unconnect-slot (car sss) p)))

    (loop for in-port-connect in (car connection-data) do
          (loop for inslot in (nth 1 in-port-connect) do
                (let ((slt (find inslot inslots :key 'cadr :test 'string-equal)))
                  (when slt (midishare-connect-slot (car slt) (car in-port-connect))))))

    (loop for out-port-connect in (cadr connection-data) do
          (loop for outslot in (nth 1 out-port-connect) do
                (let ((slt (find outslot outslots :key 'cadr :test 'string-equal)))
                  (when slt (midishare-connect-slot (car slt) (car out-port-connect)))))

          )
    ))



;;;==========================================================================================
;;; WINDOWS AND OSX PPC
;;; ON OSX INTEL (10.5) close-player crashes

(defvar *midishare-setup-app* NIL "the id of the midishare-setup application")
(defvar *midishare-setup-app-path* NIL "the current path of the midishare setup program")
(defvar *midishare-setup-app-default-path* NIL "the default path of the midishare setup program")

(defun init-midishare-setup ()
  (setf *midishare-setup-app-default-path*
        (oa::om-default-application-path '("MidiShare") "msDrivers"))
  (setf *midishare-setup-app-path* 
        (or (probe-file *midishare-setup-app-default-path*)
            (probe-file 
             #+cocoa(oa::om-external-app '("MidiShare") "msDrivers")
             #+win32(oa::om-make-pathname :directory (pathname (LISP-IMAGE-NAME)) :name "msDrivers" :type "exe")
	     #+linux(oa::om-make-pathname :directory (pathname (LISP-IMAGE-NAME)) :name "msDrivers") ;; placeholder for now. AV
             )))
  )

(defun launch-midishare-setup-app ()
  (if (and *midishare-setup-app* (oa::om-find-process *midishare-setup-app*))
    (oa::om-select-program *midishare-setup-app*)
    (progn
      (unless *midishare-setup-app-path* (init-midishare-setup))
      (if (and *midishare-setup-app-path* (probe-file *midishare-setup-app-path*))
        (progn 
          ;(close-ms-players)
          (setf *midishare-setup-app* (oa::om-run-program *midishare-setup-app-path*))   ; 'om::open-ms-players))   ;; do somthing after ?
      ;(setf *ms-setup-app* (om-run-program *om-midi-settings-app-path*
      ;                                    #'(lambda () (om-message-dialog "Warning: The new MIDI drivers setup will be used for your next OM session only. OM must exit and restart to use them."))))
          (sleep 0.9)
          (oa::om-select-program *midishare-setup-app*)
          NIL)
      (progn (oa::om-message-dialog "MidiShare Setup Application msDrivers not found!") nil)
    ))))


;;;==========================================================================================
;;; OSX INTEL
;;; REDEF THE WHOLE PROCESS AND STORE IN PREFERENCES
(defclass mini-portview (oa::om-item-view) 
  ((i :accessor i :initform nil :initarg :i)))

(defmethod oa::om-draw-contents ((self mini-portview))
  (let ((ci (midishare-get-connections (i self))))
    (oa::om-with-focused-view self
      (when (and (selectedport (oa::om-view-container self))
                 (= (i self) (selectedport (oa::om-view-container self))))
        (oa::om-with-fg-color self oa::*om-select-color*
          (oa::om-fill-rect 0 0 (oa::om-width self) (oa::om-height self))))
      (when (or (car ci) (cadr ci))
        (oa::om-with-fg-color self oa::*om-gray-color*
          (oa::om-draw-rect 0 0 (- (oa::om-width self) 1) (- (oa::om-height self) 1)))))))
    
(defmethod oa::om-view-click-handler ((self mini-portview) pos)
  (setf (selectedport (oa::om-view-container self)) (i self))
  (setf (currport (oa::om-view-container self)) (i self))  
  (oa::om-invalidate-view (oa::om-view-container self))
  t)

(defclass ms-dialog (oa::om-dialog) 
  ((currport :accessor currport :initform nil :initarg :currport)
   (selectedport :accessor selectedport :initform nil :initarg :selectedport)
   (inslotslistitem :accessor inslotslistitem :initform nil :initarg :inslotslistitem)
   (outslotslistitem :accessor outslotslistitem :initform nil :initarg :outslotslistitem)
   (prefmodule :accessor prefmodule :initform nil :initarg :prefmodule)))

(defmethod oa::om-window-mouse-moved-handler :after ((self ms-dialog) pos)
  (let ((vv (oa::om-find-view-containing-point self pos)))
    (when (and (equal 'mini-portview (type-of vv)) (or (null (currport self))
                                                       (not (= (currport self) (i vv)))))
      (setf (currport self)  (i vv))
      (oa::om-invalidate-rectangle self 190 230 170 70))))

(defmethod oa::om-view-click-handler ((self ms-dialog) pos)
  (when (equal self (call-next-method))
    (setf (selectedport self) nil)
    (oa::om-invalidate-view self)))


(defmethod update-slots-connection ((self ms-dialog))
  (if (selectedport self)
      (let ((connections (midishare-get-connections (selectedport self))))
        (oa::om-set-selected-item (inslotslistitem self) (mapcar 'cadr (car connections)))
        (oa::om-set-selected-item (outslotslistitem self) (mapcar 'cadr (cadr connections)))
        )
    (progn 
      (oa::om-set-selected-item (inslotslistitem self) nil)
      (oa::om-set-selected-item (outslotslistitem self) nil)
    )))

(defmethod oa::om-draw-contents ((self ms-dialog))
  (when (currport self)
    (let ((connect-info (midishare-get-connections (currport self))))
      (oa::om-with-focused-view self
        (oa::om-with-font oa::*om-default-font2*
                      (oa::om-draw-string 200 250 (format nil "Port ~D:" (currport self)))
                      (oa::om-draw-string 200 270 (format nil "      ~D input connections"  (length (car connect-info))))
                      (oa::om-draw-string 200 290 (format nil "      ~D output connections" (length (cadr connect-info))))
                      )))))
  
(defmethod (setf selectedport) :after (port (self ms-dialog))
  (update-slots-connection self))


;(make-midishare-setup-dialog)

(defun make-midishare-setup-dialog ()
  (let ((dd (oa::om-make-window 'ms-dialog 
                                :window-title "MidiShare Setup"
                                :bg-color oa::*om-light-gray-color*
                                :size (oa::om-make-point 580 310)
                                :resizable nil
                                ;;;:prefmodule prefmodule
                                ))
        (b-posy 310)
        (deltagrid 12)
        ports inslots outslots)
      
    (setf inslots  (remove nil (loop for ref in (midishare-get-drivers) append (nth 1 (midishare-driver-info ref)))))
    (setf outslots (remove nil (loop for ref in (midishare-get-drivers) append (nth 2 (midishare-driver-info ref)))))
    
    (oa::om-with-delayed-update dd
      (apply 'oa::om-add-subviews (cons dd 
                                        (loop for i = 0 then (+ i 1) while (< i 256) collect
                                              (oa::om-make-view 'mini-portview 
                                                                :size (oa::om-make-point 10 10)
                                                                :bg-color oa::*om-white-color*
                                                                :position (oa::om-make-point (+ 195 (* deltagrid (mod i 16))) (+ 40 (* deltagrid (floor i 16))))
                                                                :i i))))
      )
    
    (oa::om-add-subviews dd 
                     
                         (setf (inslotslistitem dd) (oa::om-make-dialog-item 'oa::om-multi-item-list (oa::om-make-point 20 40) (oa::om-make-point 160 190) ""
                                                                             :range (mapcar 'cadr inslots)
                                                                             :di-action (oa::om-dialog-item-act item
                                                                                          (when (selectedport dd)
                                                                                            (mapcar 
                                                                                             #'(lambda (slot) (if (member (cadr slot) (oa::om-get-selected-item item))
                                                                                                                  (midishare-connect-slot (car slot) (selectedport dd))
                                                                                                                (midishare-unconnect-slot (car slot) (selectedport dd))))
                                                                                             inslots)
                                                                                            (update-slots-connection dd)))))
                     
                         (setf (outslotslistitem dd) (oa::om-make-dialog-item 'oa::om-multi-item-list (oa::om-make-point 400 40) (oa::om-make-point 160 190) ""
                                                                              :range (mapcar 'cadr outslots)
                                                                              :di-action (oa::om-dialog-item-act item
                                                                                           (when (selectedport dd)
                                                                                             (mapcar 
                                                                                              #'(lambda (slot) (if (member (cadr slot) (oa::om-get-selected-item item))
                                                                                                                   (midishare-connect-slot (car slot) (selectedport dd))
                                                                                                                 (midishare-unconnect-slot (car slot) (selectedport dd))))
                                                                                              outslots)
                                                                                             (update-slots-connection dd)))))

                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 240 10) (oa::om-make-point 100 20) "MIDI Ports"
                                                  :font oa::*om-default-font2b*)

                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 20 10) (oa::om-make-point 100 20) "Input Devices"
                                                  :font oa::*om-default-font2b*)

                         (oa::om-make-dialog-item 'oa::om-static-text (oa::om-make-point 400 10) (oa::om-make-point 120 20) "Output Devices"
                                                  :font oa::*om-default-font2b*)

                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 480 240) (oa::om-make-point 80 20) "Cancel"
                                                  :di-action #'(lambda (item) (oa::om-return-from-modal-dialog dd nil)))
                     
                         (oa::om-make-dialog-item 'oa::om-button (oa::om-make-point 480 265) (oa::om-make-point 80 20) "OK"
                                                  :di-action #'(lambda (item) (oa::om-return-from-modal-dialog dd (get-midishare-connections))))
                         )
    
    (oa::om-modal-dialog dd)
    ))







