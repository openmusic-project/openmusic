

(in-package :om)


;;;;=================================================
;;;;SND PREFERENCES MODULE  icon 287
;;;;=================================================

(defvar *sys-console* nil)
(defvar *audio-sr* 44100)
(defvar *audio-res* 16)
(defvar *automatic-rename* nil)
(defvar *delete-inter-file* t)
(defvar *def-snd-format* 'aiff)
(defvar *normalize* t)
(defvar *normalize-level* 0.0)

(defvar *normalizer* :csound)
(defvar *loaded-normalizers* '(:csound))

;;; redefined in general-mixer.lisp
(defvar *general-mixer-presets* nil)
(defun default-genmixer-values () nil)

(defmethod put-preferences ((iconID (eql :audio)))
  (let* ((modulepref (find-pref-module iconID))
         (defpref (get-def-vals iconID))
         (new-sr (get-pref modulepref :audio-sr))
         (new-res (get-pref modulepref :audio-res)))
    (setf *sys-console* (get-pref modulepref :sys-console))
    (when(get-pref modulepref :audio-format)
      (setf *def-snd-format* (get-pref modulepref :audio-format)))
    (when new-sr
      (if (and (integerp new-sr) (>= new-sr 0) (<= new-sr 1000000))
          (when (/= new-sr *audio-sr*)
            (setf *audio-sr* new-sr)
            (las-set-sample-rate *audio-sr*))
        (progn 
          (om-beep-msg "Bad value for AUDIO SAMPLE RATE. The default value will be restored.")
          (set-pref modulepref :audio-sr 44100)
          )))
    (setf *delete-inter-file* (get-pref modulepref :delete-tmp))
    (setf *automatic-rename* (get-pref modulepref :auto-rename))
    (setf *normalize* (get-pref modulepref :normalize))
    (setf *normalize-level* (get-pref modulepref :normalize-level))
    (when (get-pref modulepref :normalizer)
      (if (find (get-pref modulepref :normalizer) *loaded-normalizers* :test 'equal)
          (setf *normalizer* (get-pref modulepref :normalizer))
        (om-beep-msg (string+ "Normalize module " (string (get-pref modulepref :normalizer)) " not loaded. Default module (" (string *normalizer*) ") will be used."))
        ))
    (when new-res
      (if (and (integerp new-res) (>= new-res 4) (<= new-res 4096))
          (setf *audio-res* new-res)
        (progn 
          (om-beep-msg "Bad value for AUDIO RESOLUTION. The default value will be restored.")
          (set-pref modulepref :audio-res 16)
          )))
    
    (setf *multiplayer-out-port* (get-pref modulepref :multi-out))
    (setf *multiplayer-in-port* (get-pref modulepref :multi-in))
    (setf *multiplayer-path* (get-pref modulepref :multip-path))
    
    (when (get-pref modulepref :audio-presets)
      (put-audio-mixer-values (get-pref modulepref :audio-presets)))
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
              :multi-out ,*multiplayer-out-port* :multi-in ,*multiplayer-in-port*
              :multip-path ,(when *multiplayer-path* (om-save-pathname *multiplayer-path*))
              :audio-presets ',*general-mixer-presets*
              ) *om-version*))

(defmethod get-def-vals ((iconID (eql :audio)))
  (list :audio-format 'aiff :sys-console t :audio-sr 44100 :audio-res 16
        :auto-rename nil :delete-tmp nil :normalize t :normalize-level 0.0 :normalizer :csound
        :multi-out 7071 :multi-in 7072 :multi-host "127.0.0.1" 
        :multip-path (when *multiplayer-path* (probe-file *multiplayer-path*))
        :audio-presets (init-genmixer-values)))


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
                                 :bg-color *om-light-gray-color*))
        (l1 20) (l2 (round (om-point-h (get-pref-scroll-size)) 2))
        normtext normval useval usetext
        (pos 0))
     
    (om-add-subviews thescroll
		     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 20)) (om-make-point 300 30)
					  #-linux "LibAudioStream" #+linux "JACK"
					  :font *om-default-font3b*)
                         
		     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 30)) (om-make-point 170 30)
					  (format nil "~A Sample Rate (Hz)" #-linux "Player" #+linux "Server")
					  :font *controls-font*)
                         
		     (om-make-dialog-item 'om-editable-text 
					  (om-make-point 295 (+ pos 6))
					  (om-make-point 60 13)
					  (format nil "~D" (get-pref modulepref :audio-sr)) 
					  :modify-action (om-dialog-item-act item
							   (let ((text (om-dialog-item-text item))
								 number)
							     (unless (string= "" text)
							       (setf number (ignore-errors (read-from-string text)))
							       (when (numberp number)
								 (set-pref modulepref :audio-sr number))
							       )))
					  :font *om-default-font2*)
                         
		     (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf pos 20)) (om-make-point 350 22) 
					  "(Also used as default SR for sound synthesis)"
					  :font *om-default-font1*)
		     #+linux (om-make-dialog-item 'om-static-text  (om-make-point 20 (incf pos 30))
						  (om-make-point 350 22) 
						  "OM will attempt to start a JACK-server if its not running already"
						  :font *om-default-font1*)
		     )
    
                      
    (om-add-subviews thescroll
                     (om-make-dialog-item 'om-static-text (om-make-point l2 (setf pos 20)) (om-make-point 280 30) 
                                          "Synthesis / Processing outputs"
                                          :font *om-default-font3b*)

                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos 30)) (om-make-point 170 30) "Default Audio Format"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 170) pos) 
                                          (om-make-point 100 20)
                                          ""
                                          :di-action (om-dialog-item-act item
                                                       (set-pref modulepref :audio-format
                                                                 (nth (om-get-selected-item-index item) '(aiff wav))))
                                          :font *controls-font* 
                                          :range '("WAV" "AIFF") 
                                          :value (cond ((equal (get-pref modulepref :audio-format) 'wav) "WAV")
                                                       (t #-linux "AIFF" #+linux "WAV"))
                                          )

                         
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos 30)) (om-make-point 170 30) "Default Resolution (b)"
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
                         
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos 30)) (om-make-point 190 30) "If Output File Exists..."
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
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos 30)) (om-make-point 190 30) "Delete Temporary Files"
                                          :font *controls-font*)
                     (om-make-dialog-item 'om-check-box (om-make-point (+ l2 170) pos) (om-make-point 20 20) ""
                                          :font *controls-font*
                                          :di-action (om-dialog-item-act item 
                                                       (set-pref modulepref :delete-tmp (om-checked-p item)))
                                          :checked-p (get-pref modulepref :delete-tmp))

                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos 30)) (om-make-point 190 30) "Normalize Output (default)"
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

                     (setf usetext (om-make-dialog-item 'om-static-text (om-make-point (+ l2 200) pos) (om-make-point 40 30) "Use:"
                                                        :font *controls-font*
                                                        :enable (get-pref modulepref :normalize)))
                         
                     (setf useval (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point (+ l2 230) pos) (om-make-point 100 20) ""
                                                       :font *controls-font*
                                                       :range (mapcar 'get-module-name *loaded-normalizers*)  
                                                       :di-action (om-dialog-item-act item 
                                                                    (set-pref modulepref :normalizer (nth (om-get-selected-item-index item) *loaded-normalizers*))
                                                                    )
                                                       :value (get-module-name (get-pref modulepref :normalizer))
                                                       :enable (get-pref modulepref :normalize)))

                     (setf normtext (om-make-dialog-item 'om-static-text (om-make-point (+ l2 200) (incf pos 30)) (om-make-point 60 30) 
                                                         "Level (dB)"
                                                             ;(if (equal (get-pref modulepref :normalizer) :supervp) "Level (dB)" "Gain")
                                                         :font *controls-font*
                                                         :enable (get-pref modulepref :normalize)))
                         
                     (setf normval (om-make-dialog-item 'om-editable-text 
                                                        (om-make-point (+ l2 280) pos) 
                                                        (om-make-point 50 13)
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



                     (om-make-dialog-item 'om-static-text (om-make-point (+ l2 0) (incf pos 30)) (om-make-point 190 30) 
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








