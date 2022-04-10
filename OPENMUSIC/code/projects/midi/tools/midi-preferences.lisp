;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;;; MIDI package

(in-package :om)


;=================================================
;MIDI PREFERENCES MODULE
;=================================================

(defvar *def-midi-out* 0 "default output port number")
(defvar *def-midi-in* 0 "default input port number")
(defparameter  *midi-port-modulo-channel* nil "alloc consecutive ports for channels above 16")

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
	  :port-modulo-channel nil ;t
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
                                  :bg-color *om-light-gray-color*
                                  ))
         (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2)) (i 0)
	 (dy #-linux 30 #+linux 33))
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
                                           "Default score Player:" :font *om-default-font2b*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 200 i) (om-make-point 140 24) ""
                                           :range (if *default-score-player* ; (get-pref modulepref :midi-system)
                                                      *score-players*
                                                    (cons "..." *score-players*))
                                           :value (or (and *default-score-player* (get-pref modulepref :score-player)) "---")
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :score-player (om-get-selected-item item)))
                                           :font *controls-font*)
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 400 30) 
                                           "Applies to all new score boxes" :font *om-default-font1*)
		      
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 22)) (om-make-point 130 40) 
                                           "Force score player:" :font *controls-font*)
                      (om-make-dialog-item 'om-check-box (om-make-point 200  i) (om-make-point 180 20) ""
                                           :checked-p (get-pref modulepref :force-player)
                                           :di-action #'(lambda (item) 
                                                          (set-pref modulepref :force-player (om-checked-p item))))
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 25)) (om-make-point 400 30) 
                                           "Changes the score player of existing boxes"
                                           :font *om-default-font1*)
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 34)) (om-make-point 200 30) 
                                           "MIDI I/O" :font *om-default-font2b*)
                      
                      (if (and *default-midi-system* (om-midi::midi-setup-function *default-midi-system*))
                          (om-make-dialog-item 'om-button (om-make-point 200 (- i 2)) (om-make-point 120 30) 
                                           "Ports setup" :font *om-default-font1*
                                           :di-action #'(lambda (item) (declare (ignore item)) (midi-setup modulepref)))
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

		      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 40)) (om-make-point 300 40) 
                                          "MIDI channels above 16 go to successive out ports:" :font *controls-fonti*)

		      (om-make-dialog-item 'om-check-box (om-make-point 330 i) (om-make-point 20 20) ""
                                           :checked-p (get-pref modulepref :port-modulo-channel)
                                           :di-action #'(lambda (item) 
                                                          (set-pref modulepref :port-modulo-channel (om-checked-p item))))
		      
		      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 30)) (om-make-point 200 40) 
                                           "In case of emergency:" :font *controls-fonti*)
                      (om-make-dialog-item 'om-button (om-make-point 200 i) (om-make-point 100 20) "Restart" 
                                           :enable (and *default-midi-system* (or (om-midi::midi-connect-function *default-midi-system*)
                                                                                  (om-midi::midi-restart-function *default-midi-system*)))
                                           :di-action #'(lambda (item) (declare (ignore item))
                                                          (before-restart-action)  
                                                          (if (om-midi::midi-connect-function *default-midi-system*)
                                                              ;;; will restart MIDI
                                                              (funcall (om-midi::midi-connect-function *default-midi-system*) 
                                                                       (get-pref modulepref :midi-setup))
                                                            (when (om-midi::midi-restart-function *default-midi-system*)
                                                              (funcall (om-midi::midi-restart-function *default-midi-system*)))
                                                            )
                                                          )
					   :font *controls-font*)

                      ;(om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 60)) (om-make-point 160 44) 
                      ;                     "MIDI export format:" :font *om-default-font2b*)
                      ;(om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 180 i) (om-make-point 140 24) ""
                      ;                     :range '("0 (Merge all Tracks)" "1 (Split Tracks)")
                      ;                     :value (nth (get-pref modulepref :midi-format) '("0 (Merge all Tracks)" "1 (Split Tracks)"))
                      ;                     :di-action (om-dialog-item-act item
                      ;                                  (set-pref modulepref :midi-format (om-get-selected-item-index item)))
                      ;                     )

                      )

     ;;; MIDI PLAYER / RT
     (om-add-subviews thescroll
                      
                     

                     

                      ;A REMETTRE UNE FOIS LES ELEMENTS FAIT
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (setf i 30)) (om-make-point 300 30) 
                                           "Micro-intervals (MIDI player)" :font *om-default-font2b*)
                     
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i dy)) (om-make-point 150 24)
                                           "Shift MIDI channels:" :font *controls-font*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 580 (- i (/ dy 4))) (om-make-point 170 24) ""
                                           :range '("always" "never" "when approx is 4 or 8")
                                           :value (let ((mode (get-pref modulepref :channel-shift)))
                                                    (cond ((equal mode t) "always")
                                                          ((equal mode nil) "never")
                                                          (t "when approx is 4 or 8")))
                                           :di-action (om-dialog-item-act item
                                                        (set-pref modulepref :channel-shift 
                                                                  (case (om-get-selected-item-index item)
                                                                    (0 t) (1 nil) (2 '(4 8)))))
					   :font *controls-font*
                                           )
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 340 40) 
                                           "Ditpatches notes on different MIDI channels when the pitch is not a semi-tone" :font *om-default-font1*)

                     (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 40)) (om-make-point 260 24)
                                           "Number of channels:" :font *controls-font*)
                      (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 580 (- i (/ dy 4))) (om-make-point 170 24) ""
                                           :range '("4" "depending on approx") ;(mapcar 'number-to-string '(3 4 5 6 7 8 9 10 11 12 13 14 15 16))
                                           :value (if (numberp (get-pref modulepref :channel-shift-approx)) "4" "depending on approx")
                                           :di-action (om-dialog-item-act item
                                                      (nth 2 *om-def-font-sizes*)  (set-pref modulepref :channel-shift-approx 
                                                                  (if (= 0 (om-get-selected-item-index item))
                                                                      8 nil)))
					   :font *controls-font*
                                           )
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 25)) (om-make-point 360 80) 
                                           "The number of channels used for microtones can be fixed for a 1/8th-tone optimal configuration (4 channels / default), or it can be determined according to the score approximation." :font *om-default-font1*)
                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 44)) (om-make-point 360 80) 
                                           "With 4 channels  semi-tones remain on ch.1, 1/8th tones shift to ch.2, 1/4th tones to ch.3 and 3/8th tones to ch.4" :font *om-default-font1*)

                      (om-make-dialog-item 'om-static-text (om-make-point 400 (incf i 40)) (om-make-point 130 40) "Auto microtone bend:" :font *controls-font*)
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

