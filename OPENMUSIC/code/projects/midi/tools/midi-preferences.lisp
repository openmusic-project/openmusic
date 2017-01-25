(in-package :om)


;=================================================
;MIDI PREFERENCES MODULE
;=================================================

(defvar *def-midi-out* 0 "default output port number")
(defvar *def-midi-in* 0 "default input port number")
(defparameter  *midi-port-modulo-channel* t "alloc consecutive ports for channels above 16")

(defvar *def-midi-format* 1)
(defvar *default-midi-system* nil)
(defvar *default-midi-file-system* nil)
(defvar *midi-setup* nil)

(defparameter *midi-microplay* nil)
(defparameter *default-score-player* :midi-player)   ; :midi-player :midishare :osc-scoreplayer :microplayer
(defparameter *score-players* '(:midi-player :microplayer :osc-scoreplayer))
(defparameter *force-score-player* nil)




;;;==============================================

(defmethod put-preferences ((iconID (eql :midi)))
  (let ((modulepref (find-pref-module iconID)))
    (setf *def-midi-in* (get-pref modulepref :midi-in))
    (setf *def-midi-out* (get-pref modulepref :midi-out))
    (setf *midi-port-modulo-channel* (get-pref modulepref :port-modulo-channel))
    (setf *default-midi-system* :portmidi)  ; (get-pref modulepref :midi-system)
    (setf *default-score-player* (get-pref modulepref :score-player))
    (setf *force-score-player* (get-pref modulepref :force-player))
    (setf *midi-microplay* (get-pref modulepref :auto-microtone-bend))
    (setf *micro-channel-mode-on* (get-pref modulepref :channel-shift))
    (setf *micro-channel-approx* (get-pref modulepref :channel-shift-approx))
    (when (and (om-midi::midi-connect-function *default-midi-system*) 
               (not (equal *midi-setup* (get-pref modulepref :midi-setup))))
      (setf *midi-setup* (get-pref modulepref :midi-setup))
      (when *running-midi-boxes*
	(om-message-dialog 
	 (format nil "Warning: Restarting MIDI will stop all currently running MIDI receive loops.~%[currently: ~D running]" 
		 (length *running-midi-boxes*)))
	(mapcar 'stop-midi-in *running-midi-boxes*))
      (funcall (om-midi::midi-connect-function *default-midi-system*) (get-pref modulepref :midi-setup)))
    (put-midi-mixer-values)
       
    (setf *default-midi-file-system* :cl-midi) ; (get-pref modulepref :midi-file-system)
    (setf *def-midi-format* (get-pref modulepref :midi-format))))

(defmethod get-def-vals ((ID (eql :midi)))
    (list :midi-out 0 :midi-in 0 
	  :port-modulo-channel t
	  :midi-system :portmidi
          :score-player :midi-player
          :force-player nil
	  :midi-file-system :cl-midi
	  :midi-format 1
          :channel-shift '(4 8)
          :channel-shift-approx 8
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
         (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2)) (i 0))
     (om-add-subviews thescroll
                      ;(om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 200 30) 
                      ;                     "MIDI File system:" :font *om-default-font2b*)
                      
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

                      
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 30)) (om-make-point 200 30) 
                                           "MIDI I/O" :font *om-default-font2b*)
                      (om-make-dialog-item 'om-static-text (om-make-point 180 i) (om-make-point 300 30) 
                                           "[PortMidi setup]" :font *om-default-font2*)
                      
                      ;(om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 45)) (om-make-point 160 24) 
                      ;                     "Devices/ports setup:" :font *controls-font*)
                      (if (and *default-midi-system* (om-midi::midi-setup-function *default-midi-system*))
                          (om-make-view 'button-icon
                                    :position (om-make-point 290 (- i 4)) 
                                    :size (om-make-point 28 28)
                                    :action #'(lambda (item) (declare (ignore item)) (midi-setup modulepref))
                                    :iconid 135)
                        (om-make-dialog-item 'om-static-text (om-make-point 240 i) (om-make-point 100 40) 
                                             "SETUP UNAVAILABLE"
                                             :font *om-default-font1* :fg-color *om-gray-color*))
                      
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
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 40)) (om-make-point 150 24)
                                           "Default ports:" :font *controls-font*)

                      (om-make-dialog-item 'om-static-text (om-make-point 180 i) (om-make-point 60 20) "Out" :font *controls-font*)
                      (om-make-dialog-item 'om-editable-text (om-make-point 210 i) (om-make-point 30 13)
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
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 260 i) (om-make-point 60 20) "In" :font *controls-font*)
                      (om-make-dialog-item 'om-editable-text (om-make-point 285 i) (om-make-point 30 13)
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

                      
		      
		      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 40)) (om-make-point 280 40) 
                                           "MIDI channels above 16 go to successive out ports:" :font *controls-fonti*)

		      (om-make-dialog-item 'om-check-box (om-make-point 320  i) (om-make-point 20 20) ""
                                           :checked-p (get-pref modulepref :port-modulo-channel)
                                           :di-action #'(lambda (item) 
                                                          (set-pref modulepref :port-modulo-channel (om-checked-p item))))
		      
		      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 30)) (om-make-point 120 40) 
                                           "In case of emergency:" :font *controls-fonti*)
                      (om-make-dialog-item 'om-button (om-make-point 180 (+ i 4)) (om-make-point 100 20) "Restart" 
                                           :enable (and *default-midi-system* (om-midi::midi-restart-function *default-midi-system*))
                                           :di-action #'(lambda (item) (declare (ignore item))
                                                          (when (om-midi::midi-restart-function *default-midi-system*)
                                                            (before-restart-action)
                                                            (funcall (om-midi::midi-restart-function *default-midi-system*)))
                                                          ;;; TEST
                                                          (when (om-midi::midi-connect-function *default-midi-system*)
                                                            (funcall (om-midi::midi-connect-function *default-midi-system*) 
                                                                     (get-pref modulepref :midi-setup))
                                                            )))

                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 60)) (om-make-point 160 44) 
                                           "MIDI export format:" :font *om-default-font2b*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 180 i) (om-make-point 140 24) ""
                                           :range '("0 (Merge all Tracks)" "1 (Split Tracks)")
                                           :value (nth (get-pref modulepref :midi-format) '("0 (Merge all Tracks)" "1 (Split Tracks)"))
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :midi-format (om-get-selected-item-index item)))
                                           )

                      )

     ;;; MIDI PLAYER / RT
     (om-add-subviews thescroll
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (setf i 30)) (om-make-point 200 30) 
                                           "Default score Player:" :font *om-default-font2b*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 580 i) (om-make-point 140 24) ""
                                           :range (if *default-score-player* ; (get-pref modulepref :midi-system)
                                                      *score-players*
                                                    (cons "..." *score-players*))
                                           :value (or (and *default-score-player* (get-pref modulepref :score-player)) "---")
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :score-player (om-get-selected-item item)))
                                           )
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 400 30) 
                                           "... will apply to new score boxes ..." :font *om-default-font1*)

                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 130 40) "Force score player:" :font *controls-font*)
                      (om-make-dialog-item 'om-check-box (om-make-point 580  i) (om-make-point 180 20) ""
                                           :checked-p (get-pref modulepref :force-player)
                                           :di-action #'(lambda (item) 
                                                          (set-pref modulepref :force-player (om-checked-p item))))
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 400 30) 
                                           "... will change the score player of existing boxes as well ..."
                                           :font *om-default-font1*)

                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 30)) (om-make-point 200 30) 
                                           "Micro-intervals:" :font *om-default-font2b*)
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 30)) (om-make-point 150 24)
                                           "Shift MIDI chanels:" :font *controls-font*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 550 i) (om-make-point 170 24) ""
                                           :range '("always" "never" "when approx is 4 or 8")
                                           :value (let ((mode (get-pref modulepref :channel-shift)))
                                                    (cond ((equal mode t) "always")
                                                          ((equal mode nil) "never")
                                                          (t "when approx is 4 or 8")))
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :channel-shift 
                                                                  (case (om-get-selected-item-index item)
                                                                    (0 t) (1 nil) (2 '(4 8)))))
                                           )

                     (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 30)) (om-make-point 260 24)
                                           "Shift sensibility (tone division):" :font *controls-font*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 600 i) (om-make-point 120 24) ""
                                           :range '("8" "same as approx") ;(mapcar 'number-to-string '(3 4 5 6 7 8 9 10 11 12 13 14 15 16))
                                           :value (if (numberp (get-pref modulepref :channel-shift-approx)) "8" "same as approx")
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :channel-shift-approx 
                                                                  (if (= 0 (om-get-selected-item-index item))
                                                                      8 nil)))
                                           )
 

                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 30)) (om-make-point 130 40) "Auto microtone bend:" :font *controls-font*)
                      (om-make-dialog-item 'om-check-box (om-make-point 600 i) (om-make-point 180 20) ""
                                           :checked-p (get-pref modulepref :auto-microtone-bend)
                                           :di-action #'(lambda (item) 
                                                          (set-pref modulepref :auto-microtone-bend (om-checked-p item))))

                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 360 30) 
                                           "Applies 1/8th tone pitchbend to channels 1-4 during playback"
                                           :font *om-default-font1*)
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 16)) (om-make-point 360 30) 
                                           "[works only with objects using a global MIDI port]"
                                           :font *om-default-font1*)
               
                      ;(om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 50)) (om-make-point 200 30) 
                      ;                     "System-specific:" :font *om-default-font2b*)
                      ;(om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 250 30) 
                      ;                     (string+ "These actions will be applied on the current MIDI system: " (string-upcase *default-midi-system*))
                      ;                     :font *om-default-font1*)
                      
                      
                      )
    thescroll))

(defun before-restart-action () 
  (when *running-midi-boxes*
    (om-message-dialog 
     (format nil "Warning: Restarting MIDI will stop all currently running MIDI receive loops.~%[currently: ~D running]" 
             (length *running-midi-boxes*)))
    (mapcar 'stop-midi-in *running-midi-boxes*)))

(defun apply-midi-setup (vals)
  (before-restart-action)
  (when (om-midi::midi-connect-function *default-midi-system*)
    (funcall (om-midi::midi-connect-function *default-midi-system*) vals)))

(defun midi-setup (modulepref)
  (funcall (om-midi::midi-setup-function *default-midi-system*)
           (get-pref modulepref :midi-setup) 
           #'(lambda (vals) 
              (set-pref modulepref :midi-setup vals) 
              (apply-midi-setup vals))))
    
(defun add-midi-preferences ()
  (push-pref-module (list :midi (get-def-vals :midi))))

(add-init-user-func 'add-midi-preferences)
