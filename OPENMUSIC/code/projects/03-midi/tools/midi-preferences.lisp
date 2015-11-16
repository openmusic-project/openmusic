(in-package :om)


;=================================================
;MIDI PREFERENCES MODULE
;=================================================

(defvar *def-midi-out* 0 "default output port number")
(defvar *def-midi-in* 0 "default input port number")

(defvar *def-midi-format* 1)
(defparameter *default-midi-system* nil)
(defparameter *default-midi-file-system* nil)

(defparameter *midi-microplay* nil)
(defparameter *default-score-player* :midi-player)   ; :midi-player :midishare :osc-scoreplayer :microplayer
(defparameter *score-players* '(:midi-player :microplayer :osc-scoreplayer))
(defparameter *force-score-player* nil)

;; #+linux (setf *default-midi-file-system* :cl-midi)
;(setf *default-midi-system* :cl-midi)
;(setf *default-midi-system* :midishare)

;;;==============================================

(defmethod put-preferences ((iconID (eql :midi)))
  (let ((modulepref (find-pref-module iconID)))
    (setf *def-midi-in* (get-pref modulepref :midi-in))
    (setf *def-midi-out* (get-pref modulepref :midi-out))
    (setf *default-midi-system* :portmidi)  ; (get-pref modulepref :midi-system)
    (setf *default-score-player* (get-pref modulepref :score-player))
    (setf *force-score-player* (get-pref modulepref :force-player))
    (setf *midi-microplay* (get-pref modulepref :auto-microtone-bend))    
    (when (and (om-midi::midi-connect-function *default-midi-system*) (get-pref modulepref :midi-setup))
      (when *running-midi-boxes*
	(om-message-dialog 
	 (format nil "Warning: Restarting MIDI will stop all currently running MIDI receive loops.~%[currently: ~D running]" 
		 (length *running-midi-boxes*)))
	(mapcar 'stop-midi-in *running-midi-boxes*))
      (funcall (om-midi::midi-connect-function *default-midi-system*) (get-pref modulepref :midi-setup)))
    (put-midi-mixer-values)
       
    (setf *default-midi-file-system* :cl-midi) ; (get-pref modulepref :midi-file-system)
    (setf *def-midi-format* (get-pref modulepref :midi-format))))


;; #-linux (midishare::*midishare* :midishare)
;; #-linux (midishare::*midishare* :midishare)
(defmethod get-def-vals ((ID (eql :midi)))
    (list :midi-out 0 :midi-in 0 
	  :midi-system :portmidi
          :score-player :midi-player
          :force-player nil
	  :midi-file-system :cl-midi
	  :midi-format 1
	  :auto-microtone-bend nil
	  :midi-setup '(((0 nil)) ((0 nil))) ;;; (in out)
	  :midi-presets (def-midi-presets)))

(defmethod make-new-pref-scroll ((num (eql :midi)) modulepref)
   
   (let ((thescroll (om-make-view 'preference-pane
                                  :name "MIDI" :pref-id num
                                  :size (get-pref-scroll-size)
                                  :position (om-make-point 66 0)
                                  :font *controls-font* 
                                  :scrollbars :v :retain-scrollbars t
                                  :bg-color *om-light-gray-color*))
         (init-action #'(lambda () 
                          (when *running-midi-boxes*
                            (om-message-dialog 
                             (format nil "Warning: Restarting MIDI will stop all currently running MIDI receie loops.~%[currently: ~D running]" 
                                     (length *running-midi-boxes*)))
                            (mapcar 'stop-midi-in *running-midi-boxes*))))
         (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2)) (i 0))
     (om-add-subviews thescroll
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 200 30) 
                                           "MIDI File system:" :font *om-default-font2b*)
                      
                      ;(om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 160 i) (om-make-point 140 24) ""
                      ;                     :range (if *default-midi-system* ; (get-pref modulepref :midi-system)
                      ;                                om-midi::*midi-file-systems*
                      ;                              (cons "..." om-midi::*midi-file-systems*))
                      ;                     :value (or (and *default-midi-system* (get-pref modulepref :midi-file-system)) "---")
                      ;                     :di-action (om-dialog-item-act item
                      ;                                  (set-pref modulepref :midi-file-system (om-get-selected-item item)))
                      ;                     )
                      ;(om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 30)) (om-make-point 300 30) "Library used by OM to read and write MIDI files"
                      ;                     :font *om-default-font1*)

                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 40)) (om-make-point 160 44) 
                                           "Default export format:" :font *controls-font*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 160 i) (om-make-point 140 24) ""
                                           :range '("0 (Merge all Tracks)" "1 (Split Tracks)")
                                           :value (nth (get-pref modulepref :midi-format) '("0 (Merge all Tracks)" "1 (Split Tracks)"))
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :midi-format (om-get-selected-item-index item)))
                                           )

                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 70)) (om-make-point 200 30) 
                                           "Default score Player:" :font *om-default-font2b*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 180 i) (om-make-point 140 24) ""
                                           :range (if *default-score-player* ; (get-pref modulepref :midi-system)
                                                      *score-players*
                                                    (cons "..." *score-players*))
                                           :value (or (and *default-score-player* (get-pref modulepref :score-player)) "---")
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :score-player (om-get-selected-item item)))
                                           )
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 400 30) 
                                           "... will apply to new score boxes ..." :font *om-default-font1*)

                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 130 40) "Force score player:" :font *controls-font*)
                      (om-make-dialog-item 'om-check-box (om-make-point 180  i) (om-make-point 180 20) ""
                                           :checked-p (get-pref modulepref :force-player)
                                           :di-action #'(lambda (item) 
                                                          (set-pref modulepref :force-player (om-checked-p item))))
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 400 30) 
                                           "... will change the score player of existing boxes as well ..."
                                           :font *om-default-font1*)
                      )

     ;;; MIDI PLAYER / RT
     (om-add-subviews thescroll
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (setf i 25)) (om-make-point 200 30) 
                                           "MIDI I/O" :font *om-default-font2b*)
                      (om-make-dialog-item 'om-static-text (om-make-point 480 i) (om-make-point 300 30) 
                                           "[PortMidi]" :font *om-default-font2*)
                      
                      ;(om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 540 i) (om-make-point 140 24) ""
                      ;                     :range (if *default-midi-system* ; (get-pref modulepref :midi-system)
                      ;                                om-midi::*midi-systems*
                      ;                             (cons "..." om-midi::*midi-systems*))
                      ;                     :value (or (and *default-midi-system* (get-pref modulepref :midi-system)) "---")
                      ;                     :di-action (om-dialog-item-act item
                      ;                                  (set-pref modulepref :midi-system (om-get-selected-item item)))
                      ;                     )
                      ;(om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 30)) (om-make-point 300 30) 
                      ;                     "Library used by OM to send/receive MIDI messages" :font *om-default-font1*)
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 30)) (om-make-point 150 24) "Default ports:" :font *controls-font*)

                      (om-make-dialog-item 'om-static-text (om-make-point 540 i) (om-make-point 60 20) "Out" :font *controls-font*)
                      (om-make-dialog-item 'om-editable-text (om-make-point 570 i) (om-make-point 30 13)
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
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 630 i) (om-make-point 60 20) "In" :font *controls-font*)
                      (om-make-dialog-item 'om-editable-text (om-make-point 650 i) (om-make-point 30 13)
                                           (format nil "~D" (get-pref modulepref :midi-in))
                                           :after-action 
                                           (om-dialog-item-act item
                                             (let ((text (om-dialog-item-text item))
                                                   number)
                                               (unless (string= "" text)
                                                 (setf number (read-from-string text))
                                                 (if (and (integerp number) (>= number 0) (<= number 255))
                                                     (set-pref modulepref :midi-in number)
                                                   (progn 
                                                     (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                     (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :midi-in))))
                                                   ))))
                                           :di-action 
                                           (om-dialog-item-act item
                                             (let ((text (om-dialog-item-text item))
                                                   number)
                                               (unless (string= "" text)
                                                 (setf number (read-from-string text))
                                                 (if (and (integerp number) (>= number 0) (<= number 255))
                                                     (set-pref modulepref :midi-in number)
                                                   (progn 
                                                     (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                     (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :midi-in))))
                                                   ))))
                                           :font *om-default-font2*)
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 40)) (om-make-point 130 40) "Auto microtone bend:" :font *controls-font*)
                      (om-make-dialog-item 'om-check-box (om-make-point 640  i) (om-make-point 180 20) ""
                                           :checked-p (get-pref modulepref :auto-microtone-bend)
                                           :di-action #'(lambda (item) 
                                                          (set-pref modulepref :auto-microtone-bend (om-checked-p item))))
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 45)) (om-make-point 160 24) "Devices/ports setup:" :font *controls-font*)
                      (if (and *default-midi-system* (om-midi::midi-setup-function *default-midi-system*))
                          (om-make-view 'button-icon
                                    :position (om-make-point 570 (- i 4)) 
                                    :size (om-make-point 32 32)
                                    :action #'(lambda (item) (declare (ignore item))
                                                (let ((setup-values (funcall (om-midi::midi-setup-function *default-midi-system*) (get-pref modulepref :midi-setup) init-action)))
                                                  (when setup-values 
                                                    (set-pref modulepref :midi-setup setup-values)
                                                    (when (om-midi::midi-connect-function *default-midi-system*)
                                                      (funcall init-action)
                                                      (funcall (om-midi::midi-connect-function *default-midi-system*) setup-values)
                                                      ))
                                                  ))
                                    :iconid 135)
                        (om-make-dialog-item 'om-static-text (om-make-point 560 i) (om-make-point 100 40) 
                                           "SETUP UNAVAILABLE"
                                           :font *om-default-font1* :fg-color *om-gray-color*))
                      
                      
                      ;(om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 50)) (om-make-point 200 30) 
                      ;                     "System-specific:" :font *om-default-font2b*)
                      ;(om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 250 30) 
                      ;                     (string+ "These actions will be applied on the current MIDI system: " (string-upcase *default-midi-system*))
                      ;                     :font *om-default-font1*)
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 40)) (om-make-point 120 40) "In case of emergency:" :font *controls-fonti*)
                      (om-make-dialog-item 'om-button (om-make-point 560 (+ i 4)) (om-make-point 100 20) "Restart" 
                                           :enable (and *default-midi-system* (om-midi::midi-restart-function *default-midi-system*))
                                           :di-action #'(lambda (item) (declare (ignore item))
                                                          (when (om-midi::midi-restart-function *default-midi-system*)
                                                            (funcall init-action)
                                                            (funcall (om-midi::midi-restart-function *default-midi-system*)))
                                                          
                                                          ;;; TEST
                                                          (when (om-midi::midi-connect-function *default-midi-system*)
                                                            (funcall (om-midi::midi-connect-function *default-midi-system*) (get-pref modulepref :midi-setup))
                                                            )))
                      )
    thescroll))

(defun add-midi-preferences ()
  (push-pref-module (list :midi (get-def-vals :midi))))

(add-init-user-func 'add-midi-preferences)
