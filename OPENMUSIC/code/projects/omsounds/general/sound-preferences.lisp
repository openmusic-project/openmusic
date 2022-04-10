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
;===========================================================================
; Authors: G. Assayag, C. Agon, J. Bresson
;===========================================================================


(in-package :om)


;;;;=================================================
;;;;SND PREFERENCES MODULE  icon 287
;;;;=================================================

(defvar *sys-console* nil)
(defvar *audio-res* 16)
(defvar *automatic-rename* nil)
(defvar *delete-inter-file* t)
(defvar *def-snd-format* #+cocoa :aiff #-cocoa :wav)
(defvar *normalize* t)
(defvar *normalize-level* 0.0)

(defvar *normalizer* :om)
(defvar *loaded-normalizers* '(:om))

;;; redefined in general-mixer.lisp
(defvar *general-mixer-presets* nil)
(defun default-genmixer-values () nil)

(defparameter *audio-sr* 44100)
(defparameter *audio-out-n-channels* 2)
(defparameter *audio-out-device* nil)

;;; these values depend on the selected device
(defvar *audio-sr-options* '(44100))
(defvar *audio-out-chan-options* '(2))

;; redefined by player
(defmethod player-apply-setup (player) nil)

(defmethod put-preferences ((id (eql :audio)))
  (let ((modulepref (find-pref-module id))
        (defpref (get-def-vals id)))
    
    (setf *sys-console* (get-pref modulepref :sys-console))
    (setf *def-snd-format* (get-pref modulepref :audio-format)) 
    (setf *delete-inter-file* (get-pref modulepref :delete-tmp))
    (setf *automatic-rename* (get-pref modulepref :auto-rename))
    (setf *normalize* (get-pref modulepref :normalize))
    (setf *normalize-level* (get-pref modulepref :normalize-level))
    
    (when (get-pref modulepref :normalizer)
      (if (find (get-pref modulepref :normalizer) *loaded-normalizers* :test 'equal)
          (setf *normalizer* (get-pref modulepref :normalizer))
        (print (string+ "Normalize module " (string (get-pref modulepref :normalizer)) " not loaded. Default module (" (string *normalizer*) ") will be used."))
        ))

    (let ((new-sr (get-pref modulepref :audio-sr)))
      (when new-sr
        (if (and (integerp new-sr) (> new-sr 0) (<= new-sr 1000000))
            (setf *audio-sr* new-sr)
          (progn 
            (om-beep-msg "Bad value for AUDIO SAMPLE RATE. The default value will be restored.")
            (set-pref modulepref :audio-sr 44100)
            ))))
    
    (let ((new-res (get-pref modulepref :audio-res)))
      (when new-res
        (if (and (integerp new-res) (>= new-res 4) (<= new-res 4096))
            (setf *audio-res* new-res)
          (progn 
            (om-beep-msg "Bad value for AUDIO RESOLUTION. The default value will be restored.")
            (set-pref modulepref :audio-res 16)
            ))))

    (setf *audio-out-device* (get-pref modulepref :audio-device))
    (setf *audio-out-n-channels* (get-pref modulepref :audio-n-channels))
    
    (player-apply-setup :om-audio)
    
    ;;; after setup these parameters might have been force-changed
    (set-pref modulepref :audio-n-channels  *audio-out-n-channels*)
    (set-pref modulepref :audio-sr *audio-sr*)
                               
    
    (setf *multiplayer-out-port* (get-pref modulepref :multi-out))
    (setf *multiplayer-in-port* (get-pref modulepref :multi-in))
    (setf *multiplayer-path* (get-pref modulepref :multip-path))
    
    t))


(defmethod save-pref-module ((iconID (eql :audio)) values)
  (list iconID `(list
		 :audio-format ',*def-snd-format*
		 :sys-console ,*sys-console*
		 :audio-sr ,*audio-sr*
		 :audio-res ,*audio-res*
		 :auto-rename ,*automatic-rename* 
		 :delete-tmp ,*delete-inter-file* 
		 :normalize ,*normalize*
		 :normalize-level ,*normalize-level*
		 :normalizer ,*normalizer*
		 #+multiplayer ,@`(:multi-out ,*multiplayer-out-port* :multi-in ,*multiplayer-in-port*
					    :multip-path ,(when *multiplayer-path* (om-save-pathname *multiplayer-path*)))
		 :audio-device ,*audio-out-device*
		 :audio-n-channels ,(if (> *audio-out-n-channels* 0) *audio-out-n-channels* 2)
		 ;;:audio-presets ',(get-audio-mixer-presets)
		 ) *om-version*))

(defmethod get-def-vals ((iconID (eql :audio)))
  (list :audio-format #+cocoa :aiff #-cocoa :wav :sys-console t :audio-sr 44100 :audio-res 16
        :auto-rename nil :delete-tmp t :normalize t :normalize-level 0.0 :normalizer :om
        :multi-out 7071 :multi-in 7072 :multi-host "127.0.0.1" 
        :multip-path (when (and (boundp '*multiplayer-path*) *multiplayer-path*) (probe-file *multiplayer-path*))
        :audio-device *audio-out-device* ;; use default
	:audio-n-channels 2
        ))


(defmethod get-def-normalize-value ((self t)) 0.0)
(defmethod get-module-name ((self t)) "...")

;;;(defvals (get-pref-by-icon 287 *pref-item-list*))
;;; *om-outfiles-folder*

(defmethod make-new-pref-scroll ((num (eql :audio)) modulepref)
  (let ((thescroll (om-make-view 'preference-pane
                                 :pref-id num
                                 :name "Audio"
                                 :size (get-pref-scroll-size)
                                 :position (om-make-point 66 0)
                                 :scrollbars :v 
                                 :retain-scrollbars t
                                 :bg-color *om-light-gray-color*
                                 ))
        
        (audio-devices (player-get-devices :om-audio))        
        (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2))
        normtext normval useval usetext
        noutlist srlist
        (pos 0)
	(dy #-linux 30 #+linux 33))
     
    (om-add-subviews thescroll
		     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 20)) (om-make-point 300 30)
					  "Audio Player"
					  :font *om-default-font3b*)
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ 20 0) (incf pos dy)) (om-make-point 170 dy) 
                                          "Device"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 190 pos) 
                                          (om-make-point 160 20)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (set-pref modulepref :audio-device
                                                                 (om-get-selected-item item))
                                                       )
                                          :font *controls-font* 
                                          :range audio-devices 
                                          :value (get-pref modulepref :audio-device)
                                          )
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ 20 0) (incf pos dy)) (om-make-point 170 dy) 
                                          "Outputs"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 190 pos) 
                                          (om-make-point 140 20)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (set-pref modulepref :audio-n-channels
                                                                 (read-from-string (om-get-selected-item item))))
                                          :font *controls-font* 
                                          :range (mapcar 'number-to-string *audio-out-chan-options*) 
                                          :value (number-to-string (get-pref modulepref :audio-n-channels))
                                          )

		     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos dy)) (om-make-point 170 dy)
					  "Player Sample Rate (Hz)"
					  :font *controls-font*)
                         
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 190 pos) 
                                          (om-make-point 140 20)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (set-pref modulepref :audio-sr
                                                                 (read-from-string (om-get-selected-item item))))
                                          :font *controls-font* 
                                          :range (mapcar 'number-to-string *audio-sr-options*) 
                                          :value (number-to-string (get-pref modulepref :audio-sr))
                                          )
                     
                     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf pos dy)) (om-make-point 350 dy) 
					  "(Also used as default SR for sound synthesis)"
					  :font *om-default-font1*)
                       		     
		     )
    
                      
    (om-add-subviews thescroll
                     (om-make-dialog-item 'om-static-text (om-make-point l2 (setf pos 20)) (om-make-point 280 dy) 
                                          "Synthesis / Processing outputs"
                                          :font *om-default-font3b*)

                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos dy)) (om-make-point 170 dy) "Default Audio Format"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 170) pos) 
                                          (om-make-point 100 20)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (set-pref modulepref :audio-format
                                                                 (nth (om-get-selected-item-index item) '(:aiff :wav))))
                                          :font *controls-font* 
                                          :range '("AIFF" "WAV") 
                                          :value (case (get-pref modulepref :audio-format)
						   (:wav "WAV")
						   (t  "AIFF"))
                                          )

                         
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos dy)) (om-make-point 170 dy) "Default Resolution (b)"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 170) pos) 
                                          (om-make-point 100 20)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (set-pref modulepref :audio-res
                                                                 (nth (om-get-selected-item-index item) '(8 16 24 32))))
                                          :font *controls-font*
                                          :range '("8" "16" "24" "32") 
                                          :value (case (get-pref modulepref :audio-res)
                                                   (8 "8") (16 "16") (24 "24") (32 "32")
                                                   (t "16"))
                                          )
                         
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos dy)) (om-make-point 190 dy) "If Output File Exists..."
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 170) pos) (om-make-point 100 20) ""
                                          :font *controls-font*
                                          :range '("Replace" "Auto-rename") 
                                          :di-action (om-dialog-item-act item 
                                                       (if (= 0 (om-get-selected-item-index item))
                                                           (set-pref modulepref :auto-rename nil)
                                                         (set-pref modulepref :auto-rename t)
                                                         ))
                                          :value (if (get-pref modulepref :auto-rename) "Auto-rename" "Replace"))
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos dy)) (om-make-point 190 dy) "Delete Temporary Files"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-check-box (om-make-point (+ l2 170) pos) (om-make-point 20 20) ""
                                          :font *controls-font*
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :delete-tmp (om-checked-p item)))
                                          :checked-p (get-pref modulepref :delete-tmp))

                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos dy)) (om-make-point 190 dy) "Normalize Output (default)"
                                          :font *controls-font*)

                     (om-make-dialog-item 'om-check-box (om-make-point (+ l2 170) pos) (om-make-point 20 20) ""
                                          :font *controls-font*
                                          :di-action (om-dialog-item-act item 
                                                          
                                                       (if (om-checked-p item)
                                                           (let ((text (om-dialog-item-text normval))
                                                                 (number nil))
                                                             (set-pref modulepref :normalize t)
                                                             (set-pref modulepref :normalize-level
                                                                       (unless (string= "" text)
                                                                         (setf number (read-from-string text))
                                                                         (if (numberp number)
                                                                             number
                                                                           (progn 
                                                                             (om-beep)
                                                                             (let ((defval (get-def-normalize-value (get-pref modulepref :normalizer))))
                                                                               (om-set-dialog-item-text normval (format nil "~D" defval))
                                                                               defval))))))
                                                         (set-pref modulepref :normalize nil))
                                                                    
                                                       (om-enable-dialog-item normval (om-checked-p item))
                                                       (om-enable-dialog-item normtext (om-checked-p item))
                                                       (om-enable-dialog-item useval (om-checked-p item))
                                                       (om-enable-dialog-item usetext (om-checked-p item))
                                                       )
                                          :checked-p (get-pref modulepref :normalize))

                     (setf usetext (om-make-dialog-item 'om-static-text (om-make-point (+ l2 200) pos) (om-make-point 40 dy) "Use:"
                                                        :font *controls-font*
                                                        :enable (get-pref modulepref :normalize)))
                         
                     (setf useval (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 230) (- pos (/ dy 4))) (om-make-point 120 dy) ""
                                                       :font *controls-font*
                                                       :range (mapcar 'get-module-name *loaded-normalizers*)  
                                                       :di-action (om-dialog-item-act item 
                                                                    (set-pref modulepref :normalizer (nth (om-get-selected-item-index item) *loaded-normalizers*))
                                                                    )
                                                       :value (get-module-name (get-pref modulepref :normalizer))
                                                       :enable (get-pref modulepref :normalize)))

                     (setf normtext (om-make-dialog-item 'om-static-text (om-make-point (+ l2 200) (incf pos dy)) (om-make-point 60 dy) 
                                                         "Level (dB)"
                                                             ;(if (equal (get-pref modulepref :normalizer) :supervp) "Level (dB)" "Gain")
                                                         :font *controls-font*
                                                         :enable (get-pref modulepref :normalize)))
                         
                     (setf normval (om-make-dialog-item 'om-editable-text 
                                                        (om-make-point (+ l2 280) pos) 
                                                        (om-make-point 50 30)
                                                        (let ((val (get-pref modulepref :normalize-level)))
                                                          (format nil "~D" (if (numberp val) val (get-def-normalize-value (get-pref modulepref :normalizer)))))
                                                        :enable (get-pref modulepref :normalize)
                                                        :modify-action (om-dialog-item-act item
                                                                         (let ((text (om-dialog-item-text item))
                                                                               number)
                                                                           (unless (string= "" text)
                                                                             (setf number (ignore-errors (read-from-string text)))
                                                                             (if (numberp number)
                                                                                 (set-pref modulepref :normalize-level number)
                                                                               ))))
                                                        :font *om-default-font2*))



                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos dy)) (om-make-point 190 dy) 
                                          "Print System Outputs"
                                          :font *controls-font*)
                         
                     (om-make-dialog-item 'om-check-box (om-make-point (+ l2 170) pos) (om-make-point 20 20) ""
                                          :font *controls-font*
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :sys-console (om-checked-p item)))
                                          :checked-p (get-pref modulepref :sys-console))
                     (om-make-dialog-item 'om-static-text  (om-make-point (+ l2 0) (incf pos 20)) (om-make-point 350 22) 
                                          "(Redirects outputs to the OM Listener)"
                                          :font *om-default-font1*)
                     )
        ;(setf pos 180)

    thescroll))


;;; appele juste apres la creation du workspace
(defun add-snd-prefs ()
  (push-pref-module (list :audio (get-def-vals :audio))))

(add-init-user-func 'add-snd-prefs)








