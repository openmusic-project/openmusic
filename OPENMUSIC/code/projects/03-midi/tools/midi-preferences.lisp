(in-package :om)




;=================================================
;MIDI PREFERENCES MODULE  icon 212
;=================================================

;;; PREFS 212 =
;;; 0: OUT PORT
;;; 1: IN PORT
;;; 2: MS SETTING PATH
;;; 3: microplay out port
;;; 4: microplay in port
;;; 5: microplay host
;;; 6: Microplayer app

;;;; old version saved ports as strings
(defun strig2numport (port &optional (out t))
  (if (integerp port) port
    (if out *Outmidiport* *Inmidiport*)))


;;;==============================
;;; MIDI SETUP TOOL
;;;==============================

(defvar *ms-setup-app* "the id of the midishare-setup application")
(setf *ms-setup-app* nil)
(defvar *om-midi-settings-app-path* "the current path of the midishare setup program")
(defvar *om-midi-settings-app-default-path* "the default path of the midishare setup program")

(defun init-midisetup ()
  (setf *om-midi-settings-app-default-path*
        (om-default-application-path '("MidiShare") "msDrivers"))
  (setf *om-midi-settings-app-path* 
        (or (probe-file *om-midi-settings-app-default-path*)
            (probe-file 
             #+cocoa(om-external-app '("MidiShare") "msDrivers")
             #+win32(om-make-pathname :directory (pathname (LISP-IMAGE-NAME)) :name "msDrivers" :type "exe")
             )))
  ;(print (string+ "MIDI Setup App: " (if *om-midi-settings-app-path* (namestring *om-midi-settings-app-path*) "NOT FOUND")))
  )


(om-add-init-func 'init-midisetup)

; (om::open-ms-players)

;;; ONLY CALL ON OSX PPC !!!
;;; else close player crashes with OSX 10.5
(defun launch-midishare-setup ()
  (if (and *ms-setup-app* (om-find-process *ms-setup-app*))
    (om-select-program *ms-setup-app*)
    (if (and *om-midi-settings-app-path* (probe-file *om-midi-settings-app-path*))
        (progn 
          (close-ms-players)
          (setf *ms-setup-app* (om-run-program *om-midi-settings-app-path* 'om::open-ms-players))
      ;(setf *ms-setup-app* (om-run-program *om-midi-settings-app-path*
      ;                                    #'(lambda () (om-message-dialog "Warning: The new MIDI drivers setup will be used for your next OM session only. OM must exit and restart to use them."))))
          (sleep 0.9)
          (om-select-program *ms-setup-app*))
      (om-message-dialog "MidiShare Setup Application msDrivers not found!")
    )))


(defun restore-midishare-connections (connection-data) 
  (let ((inslots (flat (remove nil (loop for ref in (oa::midi-get-drivers) collect (nth 1 (oa::midi-driver-info ref)))) 1))
        (outslots (flat (remove nil (loop for ref in (oa::midi-get-drivers) collect (nth 2 (oa::midi-driver-info ref)))) 1)))
    
    ;(print "RESTORING MIDI CONNECTIONS")
    ;(print connection-data)
    
    ; déconnecte tout
    (loop for p from 0 to 255 do
          (loop for sss in (append inslots outslots) do
                (oa::unconnect-slot (car sss) p)))

    (loop for port-connect in connection-data do
          
          (loop for inslot in (nth 1 port-connect) do
                (let ((slt (find inslot inslots :key 'cadr :test 'string-equal)))
                  (when slt (oa::connect-slot (car slt) (car port-connect)))))
          (loop for outslot in (nth 2 port-connect) do
                (let ((slt (find outslot outslots :key 'cadr :test 'string-equal)))
                  (when slt (oa::connect-slot (car slt) (car port-connect)))))

          )))

; (put-preferences 212)

(omg-defclass mini-portview (om-item-view) 
              ((i :accessor i :initform nil :initarg :i)))

(defmethod om-draw-contents ((self mini-portview))
  (let ((ci (oa::midi-get-connections (i self))))
    (om-with-focused-view self
      (when (and (selectedport (om-view-container self))
                 (= (i self) (selectedport (om-view-container self))))
        (om-with-fg-color self *om-select-color*
          (om-fill-rect 0 0 (w self) (h self))))
      (when (or (car ci) (cadr ci))
        (om-with-fg-color self *om-gray-color*
          (om-draw-rect 0 0 (- (w self) 1) (- (h self) 1)))))))
    
(defmethod om-view-click-handler ((self mini-portview) pos)
  (setf (selectedport (om-view-container self)) (i self))
  (setf (currport (om-view-container self)) (i self))  
  (om-invalidate-view (om-view-container self))
  t)

(omg-defclass ms-dialog (om-window) 
              ((currport :accessor currport :initform nil :initarg :currport)
               (selectedport :accessor selectedport :initform nil :initarg :selectedport)
               (inslotslistitem :accessor inslotslistitem :initform nil :initarg :inslotslistitem)
               (outslotslistitem :accessor outslotslistitem :initform nil :initarg :outslotslistitem)
               (prefmodule :accessor prefmodule :initform nil :initarg :prefmodule)))

(defmethod om-window-mouse-moved-handler :after ((self ms-dialog) pos)
  (let ((vv (om-find-view-containing-point self pos)))
    (when (and (equal 'mini-portview (type-of vv)) (or (null (currport self))
                                                       (not (= (currport self) (i vv)))))
      (setf (currport self)  (i vv))
      (om-invalidate-rectangle self 190 230 170 70))))

(defmethod om-view-click-handler ((self ms-dialog) pos)
  (when (equal self (call-next-method))
    (setf (selectedport self) nil)
    (om-invalidate-view self)))

(defmethod om-window-close-event ((self ms-dialog))
  (when (and (prefmodule self) 
             (om-y-or-n-dialog  
              (format nil "Save MIDI setup in OM preferences ?") 
              :default-button :yes))
    ; SAVE MIDI IN PREFS
    (let ((connections nil)
          (portinfo nil))
      (loop for p from 0 to 255 do
            (setf portinfo (oa::midi-get-connections p))
            (when (or (car portinfo) (cadr portinfo))
              (push (list p (mapcar 'cadr (car portinfo)) (mapcar 'cadr (cadr portinfo))) connections)))
      (set-pref (prefmodule self) :ms-drivers (reverse connections))
      ))
  (setf *ms-setup-win* nil)
  (call-next-method))


(defmethod update-slots-connection ((self ms-dialog))
  (if (selectedport self)
      (let ((connections (oa::midi-get-connections (selectedport self))))
        (om-set-selected-item (inslotslistitem self) (mapcar 'cadr (car connections)))
        (om-set-selected-item (outslotslistitem self) (mapcar 'cadr (cadr connections)))
        )
    (progn 
      (om-set-selected-item (inslotslistitem self) nil)
      (om-set-selected-item (outslotslistitem self) nil)
    )))

(defvar *ms-setup-win* nil)



(defun om-midishare-setup (&optional prefmodule)
  (if *ms-setup-win* (om-select-window *ms-setup-win*)
    (let ((dd (om-make-window 'ms-dialog 
                              :window-title "MidiShare Setup"
                              :bg-color *om-light-gray-color*
                              :size (om-make-point 580 310)
                              :resizable nil
                              :prefmodule prefmodule))
          (b-posy 310)
          (deltagrid 12)
          ports inslots outslots)
      
    (setf inslots (flat (remove nil (loop for ref in (oa::midi-get-drivers) collect (nth 1 (oa::midi-driver-info ref)))) 1))
    (setf outslots (flat (remove nil (loop for ref in (oa::midi-get-drivers) collect (nth 2 (oa::midi-driver-info ref)))) 1))
    
    (om-with-delayed-update dd
        (eval `(om-add-subviews ,dd ,.(loop for i = 0 then (+ i 1) while (< i 256) collect
                                            (om-make-view 'mini-portview 
                                                          :size (om-make-point 10 10)
                                                          :bg-color *om-white-color*
                                                          :position (om-make-point (+ 195 (* deltagrid (mod i 16))) (+ 40 (* deltagrid (floor i 16))))
                                                          :i i)))))
    
    (om-add-subviews dd 
                     
                     (setf (inslotslistitem dd) (om-make-dialog-item 'om-multi-item-list (om-make-point 20 40) (om-make-point 160 190) ""
                                                                     :range (mapcar 'cadr inslots)
                                                                     :di-action (om-dialog-item-act item
                                                                                  (when (selectedport dd)
                                                                                    (mapcar 
                                                                                     #'(lambda (slot) (if (member (cadr slot) (om-get-selected-item item))
                                                                                                          (oa::connect-slot (car slot) (selectedport dd))
                                                                                                        (oa::unconnect-slot (car slot) (selectedport dd))))
                                                                                     inslots)
                                                                                    (update-slots-connection dd)))))
                     
                     (setf (outslotslistitem dd) (om-make-dialog-item 'om-multi-item-list (om-make-point 400 40) (om-make-point 160 190) ""
                                                                     :range (mapcar 'cadr outslots)
                                                                     :di-action (om-dialog-item-act item
                                                                                  (when (selectedport dd)
                                                                                  (mapcar 
                                                                                   #'(lambda (slot) (if (member (cadr slot) (om-get-selected-item item))
                                                                                                       (oa::connect-slot (car slot) (selectedport dd))
                                                                                                     (oa::unconnect-slot (car slot) (selectedport dd))))
                                                                                   outslots)
                                                                                  (update-slots-connection dd)))))

                     (om-make-dialog-item 'om-static-text (om-make-point 240 10) (om-make-point 100 20) "MIDI Ports"
                                          :font *om-default-font2b*)

                     (om-make-dialog-item 'om-static-text (om-make-point 20 10) (om-make-point 100 20) "Input Devices"
                                          :font *om-default-font2b*)

                     (om-make-dialog-item 'om-static-text (om-make-point 400 10) (om-make-point 120 20) "Output Devices"
                                          :font *om-default-font2b*)
                      )
    (om-select-window dd)
    (setf *ms-setup-win* dd)

    )))



(defmethod om-draw-contents ((self ms-dialog))
  (when (currport self)
    (let ((connect-info (oa::midi-get-connections (currport self))))
      (om-with-focused-view self
        (om-with-font *om-default-font2*
                      (om-draw-string 200 250 (format nil "Port ~D:" (currport self)))
                      (om-draw-string 200 270 (format nil "      ~D input connections"  (length (car connect-info))))
                      (om-draw-string 200 290 (format nil "      ~D output connections" (length (cadr connect-info))))
                      )))))
  
(defmethod (setf selectedport) :after (port (self ms-dialog))
  (update-slots-connection self))

; help en bas : coluer des carrés
; modes : port / slot 

; (om-midishare-setup)

           
;;;==============================

; (get-pref (get-pref-by-icon 212) :ms-drivers)
; (put-preferences 212)


(defmethod put-preferences ((iconID (eql :midi)))
  (let ((modulepref (find-pref-module iconID)))
       (when (and (find :om-midi-api *features*) *midi-share?*)
         (setf *Outmidiport* (strig2numport (get-pref modulepref :midi-out)))
         (setf *Inmidiport* (strig2numport (get-pref modulepref :midi-in)))
         ;(setf *om-midi-settings-app-path* (get-pref modulepref :midisetup-path))
         (setf *def-midi-format* (get-pref modulepref :midi-format))
         (when (get-pref modulepref :ms-drivers) 
           (sleep 0.5)
           (om-without-interrupts (restore-midishare-connections (get-pref modulepref :ms-drivers)))))
       ;(when (find 'microplayer (assoc-players *general-player*))
       ;  (setf *microplayer-out-port* (get-pref modulepref :micro-out))
       ;  ;(setf *microplayer-host* (get-pref modulepref :micro-host))
       ;  (setf *micro-player-path* (get-pref modulepref :micro-path))
       ;  (unless (= *microplayer-in-port* (get-pref modulepref :micro-in))
       ;    (show-message-win (format nil "Updating OSC connection.~%Please wait..."))    
       ;    (close-microplayer)
       ;    (setf *microplayer-in-port* (get-pref modulepref :micro-in))
       ;    (open-microplayer)
       ;    (hide-message-win))
       ;  
       ;  )
       )) 

(defmethod get-def-vals ((iconID (eql :midi))) (list :midi-out 0 :midi-in 0 
                                                   :ms-drivers nil
                                                   :midi-format 1
                                                   :midisetup-path (when *om-midi-settings-app-default-path* 
                                                                     (probe-file *om-midi-settings-app-default-path*))
                                                   :micro-out 3000 :micro-in 3010 :micro-host "127.0.0.1" 
                                                   :micro-path (when *micro-player-path* (probe-file *micro-player-path*))
                                                   
                                                   ))


(defmethod make-new-pref-scroll ((num (eql :midi)) modulepref)
   (let ((thescroll (om-make-view 'preference-pane
                                  :name "MIDI / OSC"
                                  :pref-id num
                                  :size (get-pref-scroll-size)
                                  :position (om-make-point 66 0)
                                  :font *controls-font* 
                                  :scrollbars :v 
                                  :retain-scrollbars t
                                  :bg-color *om-light-gray-color*
                                  ))
         (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2))
         msapp microapp
         (i 0))
    (om-add-subviews thescroll
                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 200 30) "MIDIShare"
                                          :font *om-default-font3b*)
                      
       (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 35)) (om-make-point 150 24) "Default Ports:" :font *controls-font*)

       (om-make-dialog-item 'om-static-text (om-make-point 130 i) (om-make-point 60 20) "Out" :font *controls-font*)
       
       (om-make-dialog-item 'om-editable-text (om-make-point 165 i) (om-make-point 37 13)
                            (format nil "~D" (get-pref modulepref :midi-out))
                                                 :after-action 
                                                 (om-dialog-item-act item
                                                   (let ((text (om-dialog-item-text item))
                                                         number)
                                                     (unless (string= "" text)
                                                       (setf number (read-from-string text))
                                                       (if (and (integerp number) (>= number 0) (<= number 255))
                                                           (set-pref modulepref :midi-out number)
                                                         (progn 
                                                           (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                           (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :midi-out))))
                                                         ))))
                                                 :di-action 
                                                 (om-dialog-item-act item
                                                   (let ((text (om-dialog-item-text item))
                                                         number)
                                                     (unless (string= "" text)
                                                       (setf number (read-from-string text))
                                                       (if (and (integerp number) (>= number 0) (<= number 255))
                                                           (set-pref modulepref :midi-out number)
                                                         (progn 
                                                           (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                           (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :midi-out))))
                                                         ))))
                                                 :font *om-default-font2*)
       
       (om-make-dialog-item 'om-static-text (om-make-point 240 i) (om-make-point 150 20) "In" :font *controls-font*)
       
       (om-make-dialog-item 'om-editable-text (om-make-point 270 i) (om-make-point 37 13)
                            (format nil "~D" (get-pref modulepref :midi-in)) 
                            :after-action (om-dialog-item-act item
                                         (let ((text (om-dialog-item-text item))
                                               number)
                                           (unless (string= "" text)
                                             (setf number (read-from-string text))
                                             (if (and (integerp number) (>= number 0) (<= number 255))
                                               (set-pref modulepref :midi-in number)
                                               (progn 
                                                 (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                 (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :midi-in))))))))
                            :di-action (om-dialog-item-act item
                                         (let ((text (om-dialog-item-text item))
                                               number)
                                           (unless (string= "" text)
                                             (setf number (read-from-string text))
                                             (if (and (integerp number) (>= number 0) (<= number 255))
                                               (set-pref modulepref :midi-in number)
                                               (progn 
                                                 (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                 (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :midi-in))))))))
                            :font *om-default-font2*)
       
       ;(om-make-dialog-item 'om-static-text (om-make-point 20 80) (om-make-point 180 24) "MidiShare SetUp App." :font *controls-font*)
       
       ;(setf msapp (om-make-dialog-item 'om-static-text (om-make-point 20 100) (om-make-point 280 50) 
       ;                                 (if (get-pref modulepref :midisetup-path) 
       ;                                     (namestring (get-pref modulepref :midisetup-path))
       ;                                   "NOT FOUND")
       ;                                     :font *om-default-font2*
       ;                                     :fg-color (if (get-pref modulepref :midisetup-path) *om-black-color* *om-red-color*)))

      ;(om-make-dialog-item 'om-button (om-make-point 320 100) (om-make-point 50 24) "..."
      ;                                    :di-action (om-dialog-item-act item
      ;                                                 (declare (ignore button))
      ;                                                 (let* ((path (om-choose-file-dialog :directory (om-default-application-path nil nil))))
      ;                                                  (if (and path (probe-file path))
      ;                                                      (progn 
      ;                                                        (set-pref modulepref :midisetup-path path)
      ;                                                       (om-set-dialog-item-text msapp (namestring path))
      ;                                                        (om-invalidate-view thescroll))
      ;                                                      (om-beep-msg "Bad path for MisiShare setup app.")))))
      
      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 45)) (om-make-point 100 24) "Set MIDI Ports:" :font *controls-font*)

      (om-make-view 'button-icon
                    :position (om-make-point 160 (- i 4)) 
                    :size (om-make-point 32 32)
                    :action #'(lambda (item) (declare (ignore item))
                                (when *midi-share?*
                                  #+(or powerpc win32) (launch-midishare-setup)
                                  #-(or powerpc win32) (om-midishare-setup (find-pref-module :midi (local-prefs (om-view-window item)))))
                                
                                )
                    :iconid 135)

      
      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 55)) (om-make-point 120 40) "In case of emergency:" :font *controls-fonti*)

      (om-make-dialog-item 'om-button
                           (om-make-point 160  i) 
                           (om-make-point 180 20)
                     "Reset MIDIShare"
                     :di-action #'(lambda (item) (declare (ignore item))
                                    (when *midi-share?* (midiplay-reset))))
      
      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 60)) (om-make-point 100 44) "Default MIDI export format:" :font *controls-font*)
      
      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 160 (+ i 5)) (om-make-point 180 24) ""
                           :range '("0 (Merge all Tracks)" "1 (Split Tracks)")
                           :value (nth (get-pref modulepref :midi-format) '("0 (Merge all Tracks)" "1 (Split Tracks)"))
                           :di-action (om-dialog-item-act item
                                        (set-pref modulepref :midi-format (om-get-selected-item-index item)))
                           )
      ;(om-make-dialog-item 'om-button (om-make-point 250 148) (om-make-point 120 20) "MIDI Restart"
      ;                                    :di-action (om-dialog-item-act item
      ;                                                 (declare (ignore button))
      ;                                                 (show-message-win "Setting MIDI drivers...")
      ;                                                 (close-ms-players)
      ;                                                 (sleep 1)
      ;                                                 (open-ms-players)
      ;                                                 (hide-message-win)
      ;                                                 ))
      
        ) 

   
    (om-add-subviews thescroll
                     (om-make-dialog-item 'om-static-text (om-make-point l2 (setf i 25)) (om-make-point 200 30) "MicroPlayer"
                                          :font *om-default-font3b*)
                     
                     (om-make-dialog-item 'om-static-text (om-make-point l2 (incf i 35)) (om-make-point 150 24) "UDP Ports:" :font *controls-font*);

                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 110) i) (om-make-point 150 20) "Out" :font *controls-font*)
                     
                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l2 145) i) (om-make-point 42 13)
                                          (format nil "~D" (get-pref modulepref :micro-out)) 
                                          :after-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :micro-in))))
                                                                  (set-pref modulepref :micro-out number)
                                                                (progn 
                                                                  (om-beep-msg "OSC Player port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :micro-out))))
                                                                ))))
                                          :di-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :micro-in))))
                                                                  (set-pref modulepref :micro-out number)
                                                                (progn 
                                                                  (om-beep-msg "OSC Player port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :micro-out))))
                                                                ))))
                                          :font *om-default-font2*)
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 220) i) (om-make-point 150 24) "In" :font *controls-font*)
                     
                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l2 250) i) (om-make-point 42 13)
                                          (format nil "~D" (get-pref modulepref :micro-in)) 
                                          :after-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :micro-out))))
                                                                  (set-pref modulepref :micro-in number)
                                                                (progn 
                                                                  (om-beep-msg "OSC Player port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :micro-in))))))))
                                          :di-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :micro-out))))
                                                                  (set-pref modulepref :micro-in number)
                                                                (progn 
                                                                  (om-beep-msg "OSC Player port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :micro-in))))))))
                                          :font *om-default-font2*)
                     
                     ;(om-make-dialog-item 'om-static-text (om-make-point 20 246) (om-make-point 150 24) 
                     ;                     "Default UDP Host:" :font *controls-font*)
                     
                     ;(om-make-dialog-item 'om-editable-text (om-make-point 165 241) 
                     ;                     (om-make-point 100 13)
                     ;                     (get-pref modulepref :micro-host) 
                     ;                     :after-action (om-dialog-item-act item
                     ;                                     (let ((text (om-dialog-item-text item)))
                     ;                                       (unless (string= "" text)
                     ;                                         (set-pref modulepref :micro-host text)
                     ;                                         )))
                     ;                     :font *om24-default-font2*)
                     
                     (om-make-dialog-item 'om-static-text (om-make-point l2 (incf i 40)) (om-make-point 180 24) "MicroPlayer App." :font *controls-font*)
                     
                     
                     (setf microapp (om-make-dialog-item 'om-static-text (om-make-point l2 (incf i 20)) (om-make-point 280 50) 
                                                         (if (get-pref modulepref :micro-path) 
                                                             (namestring (get-pref modulepref :micro-path))
                                                           "NOT FOUND")
                                                         :font *om-default-font2*
                                                         :fg-color (if (get-pref modulepref :micro-path) *om-black-color* *om-red-color*)))
                     
                     (om-make-view 'om-icon-button 
                                                      :icon1 "folder" :icon2 "folder-pushed"
                                                      :position (om-make-point (+ l2 300) i) :size (om-make-point 26 25) 
                                                      :action (om-dialog-item-act item
                                                                (declare (ignore button))
                                                                (let* ((path (om-choose-file-dialog :directory (om-default-application-path nil nil))))
                                                                  (if path
                                                                      (if (probe-file path)
                                                                          (progn 
                                                                            (set-pref modulepref :micro-path path)
                                                                            (om-set-dialog-item-text microapp (namestring path))
                                                                            (om-invalidate-view thescroll))
                                                                        (om-beep-msg "Bad path for MicroPlayer app."))))))

                     
                     )

    thescroll))


(defun add-midi-preferences ()
  (push-pref-module (list :midi (get-def-vals :midi))))

(add-init-user-func 'add-midi-preferences)
