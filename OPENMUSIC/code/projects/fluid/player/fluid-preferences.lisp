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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson, Karim Haddad
;=========================================================================

;;; Fluidsynth package

(in-package :om)


;=================================================
;FLUID PREFERENCES MODULE
;=================================================

(defparameter *n-fsynth* 1)
(defparameter *fluid-sf2* (merge-pathnames (make-pathname :directory '(:relative "resources/online/in-files") :name "merlin.sf2") cl-user::*om-src-directory*))
;(pathname (concatenate 'string (namestring cl-user::*om-src-directory*) "resources/online/in-files/merlin.sf2")))
(defparameter *fluid-loaded* "Unloaded...")

;;;==============================================

(defmethod put-preferences ((iconID (eql :fluid)))
  (let ((modulepref (find-pref-module iconID)))
    (if (probe-file (get-pref modulepref :fluid-sf2))
        (setf *fluid-sf2* (get-pref modulepref :fluid-sf2))
      (push :fluid-sf2 *restore-defaults*))
    (setf *n-fsynth* (get-pref modulepref :n-fsynth))
    ;(put-midi-mixer-values)
    ))

(defmethod get-def-vals ((ID (eql :fluid)))
    (list 
          :fluid-sf2 (merge-pathnames (make-pathname :directory '(:relative "resources/online/in-files") :name "merlin.sf2") cl-user::*om-src-directory*)
          :n-fsynth 1
          ;:midi-presets (def-midi-presets)
          ))


(defmethod save-pref-module ((iconID (eql :fluid)) item)
   (list iconID `(list 
                  :fluid-sf2 ,(om-save-pathname *fluid-sf2*)
                  :n-fsynth ,*n-fsynth* 
                       ) *om-version*))

(defmethod make-new-pref-scroll ((num (eql :fluid)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "FluidSynth" 
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 66 0)
                                 :font *controls-font* 
                                ; :scrollbars :v :retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                  ))
                   
                    
        (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2)) (i 0)
        (dy #-linux 30 #+linux 33) sf2txt)
    
     (om-add-subviews thescroll                  
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 20)) (om-make-point 200 30) 
                                           (concatenate 'string "FluidSynth"  " " " " "v." (cl-fluid::fluid_version_str)) 
                                           :font *om-default-font2b*)

                      (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf i 30)) (om-make-point 80 22) "Sf2 File:"
                                           :font *controls-font*)
                      
                      (setf sf2txt (om-make-dialog-item 'om-static-text  (om-make-point 80 i) (om-make-point 320 45)
                                                        (om-namestring (get-pref modulepref :fluid-sf2))
                                                        :font *om-default-font1*))
                      
                      (om-make-view 'om-icon-button 
                                    :icon1 "folder" :icon2 "folder-pushed"
                                    :position (om-make-point 400 (- i 5)) :size (om-make-point 26 25) 
                                    :action (om-dialog-item-act item
                                              (declare (ignore item))
                                              (let ((newsf2 (om-choose-file-dialog :directory
                                                                                   (get-pref modulepref :fluid-sf2))))
                                                (when newsf2
                                                  (om-set-dialog-item-text sf2txt (om-namestring newsf2))
                                                  (setf *fluid-sf2* (om-namestring newsf2))
                                                  (set-pref modulepref :fluid-sf2 newsf2))
                                                )))
                      
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 40)) (om-make-point 150 24)
                                           "Number of Synths:" :font *controls-font*)
                      (om-make-dialog-item 'om-editable-text (om-make-point 210 i) (om-make-point 30 13)
                                           (format nil "~D" (get-pref modulepref :n-fsynth))
                                           :after-action 
                                           (om-dialog-item-act item
                                             (let ((text (om-dialog-item-text item))
                                                   number)
                                               (unless (string= "" text)
                                                 (setf number (read-from-string text))
                                                 (if (and (integerp number) (>= number 0) (<= number 255))
                                                     (set-pref modulepref :n-fsynth number)
                                                   (progn 
                                                     (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                     (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :n-fsynth))))
                                                   ))))
                                           :di-action 
                                           (om-dialog-item-act item
                                             (let ((text (om-dialog-item-text item))
                                                   number)
                                               (unless (string= "" text)
                                                 (setf number (read-from-string text))
                                                 (if (and (integerp number) (>= number 0) (<= number 255))
                                                     (set-pref modulepref :n-fsynth number)
                                                   (progn 
                                                     (om-beep-msg "Midi port must be an integer between 0 and 255.")
                                                     (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :n-fsynth))))
                                                   ))))
                                           :font *om-default-font2*)
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 40)) (om-make-point 150 24)
                                           "Load Synths:" :font *controls-font*)
                      (om-make-view 'om-icon-button 
                                    :icon1 "stop" :icon2 "stop-pushed"
                                    :position (om-make-point 210 (- i 5)) :size (om-make-point 26 25)
                                    :action (om-dialog-item-act item
                                              (declare (ignore item))
                                              (progn 
                                                (load-all-fsynths *n-fsynth*)
                                                (load-sf-to-all)
                                                (setf *fluid-loaded* "Loaded!")
                                                (om-set-dialog-item-text fsynthtxt *fluid-loaded*)
                                                (om-set-fg-color fsynthtxt *om-black-color*)
                                                (if (= 1 *n-fsynth*)
                                                    (print "Loaded one instance!")
                                                  (print (format nil "Loaded ~D fluid instances!" *n-fsynth*)))
                                                )))
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 40)) (om-make-point 150 24)
                                           "Delete Synths:" 
                                           :font *controls-font*)
                       (setf fsynthtxt (om-make-dialog-item 'om-static-text (om-make-point 250 (- i 43)) (om-make-point 76 18) 
                                           *fluid-loaded*
                                           :bg-color *om-white-color*
                                           :fg-color (if cl-fluid::*fl-synths* *om-black-color* *om-red-color*)
                                          ; :font *om-default-font2b*
                                           ))
                      (om-make-view 'om-icon-button 
                                    :icon1 "stop" :icon2 "stop-pushed"
                                    :position (om-make-point 210 (- i 5)) :size (om-make-point 26 25)
                                    :action (om-dialog-item-act item
                                              (declare (ignore item))
                                              (progn 
                                                (cl-fluid::delete-all-audio-drivers)
                                                (print (format nil "Deleted ~D fluid instances!" *n-fsynth*))
                                                (setf *fluid-loaded* "Unloaded...")
                                                (om-set-dialog-item-text fsynthtxt *fluid-loaded*)
                                                (om-set-fg-color fsynthtxt *om-red-color*)
                                                )))
                      
                      (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 54)) (om-make-point 200 30) 
                                           "SF2 Libs" :font *om-default-font2b*)
                      (om-make-dialog-item 'om-button (om-make-point 200 (- i 2)) (om-make-point 120 30) 
                                           "SF2 setup" :font *om-default-font1*
                                           :di-action #'(lambda (item) (declare (ignore item)) (sf2-setup modulepref)))
  
                      )
     

     ;;; MIDI PLAYER / RT
     #|
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
                      

                      
                      )|#
    thescroll))


(defun add-fluid-preferences ()
  (push-pref-module (list :fluid (get-def-vals :fluid))))

(progn
(add-fluid-preferences)
(add-init-user-func 'add-fluid-preferences))


