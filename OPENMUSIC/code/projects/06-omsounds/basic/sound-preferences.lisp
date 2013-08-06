

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
            (when *audio-player*
              (audio-close)
              (setf *om-player-sample-rate* *audio-sr*)
              (audio-open)))
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
    
    (setf *general-mixer-presets* (get-pref modulepref :audio-presets))
    (if (not *general-mixer-presets*)
        (setf *general-mixer-presets* (init-genmixer-values)))
    t))


;(get-pref (get-pref-by-icon 287) :audio-sr)

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
                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 20)) (om-make-point 300 30) "LibAudioStream"
                                          :font *om-default-font3b*)
                         
                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf pos 30)) (om-make-point 170 30) "Player Sample Rate (Hz)"
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
                                          :font *om-default-font1*))


    (om-add-subviews thescroll
                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf pos 50)) (om-make-point 200 30) "MultiPlayer"
                                          :font *om-default-font3b*)
                     
                     ;(om-make-dialog-item 'om-check-box (om-make-point l2 (incf i 30)) (om-make-point 180 15) " Enable" 
                     ;;                     :di-action (om-dialog-item-act item 
                     ;                                  (set-pref modulepref :multi-enable (om-checked-p item)))
                     ;                     :font *controls-font*
                     ;                     :checked-p (get-pref modulepref :multi-enable))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf pos 30)) (om-make-point 150 24) "UDP Ports:" :font *controls-font*);

                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 110) pos) (om-make-point 150 20) "Out" :font *controls-font*)
                     
                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 145) pos) (om-make-point 42 20)
                                          (format nil "~D" (get-pref modulepref :multi-out)) 
                                          :after-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :multi-in))))
                                                                  (set-pref modulepref :multi-out number)
                                                                (progn 
                                                                  (om-beep-msg "OSC Player port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :multi-out))))
                                                                ))))
                                          :di-action (om-dialog-item-act item
                                                       (let ((text (om-dialog-item-text item))
                                                             number)
                                                         (unless (string= "" text)
                                                           (setf number (read-from-string text))
                                                           (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :multi-in))))
                                                               (set-pref modulepref :multi-out number)
                                                             (progn 
                                                               (om-beep-msg "OSC Player port must be an integer")
                                                               (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :multi-out))))
                                                             ))))
                                          :font *om-default-font2*)
                     
                     (om-make-dialog-item 'om-static-text (om-make-point (+ l1 220) pos) (om-make-point 150 24) "In" :font *controls-font*)
                     
                     (om-make-dialog-item 'om-editable-text (om-make-point (+ l1 250) pos) (om-make-point 42 13)
                                          (format nil "~D" (get-pref modulepref :multi-in)) 
                                          :after-action (om-dialog-item-act item
                                                          (let ((text (om-dialog-item-text item))
                                                                number)
                                                            (unless (string= "" text)
                                                              (setf number (read-from-string text))
                                                              (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :multi-out))))
                                                                  (set-pref modulepref :multi-in number)
                                                                (progn 
                                                                  (om-beep-msg "OSC Player port must be an integer")
                                                                  (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :multi-in))))))))
                                          :di-action (om-dialog-item-act item
                                                       (let ((text (om-dialog-item-text item))
                                                             number)
                                                         (unless (string= "" text)
                                                           (setf number (read-from-string text))
                                                           (if (and (integerp number) (>= number 0) (not (= number (get-pref modulepref :multi-out))))
                                                               (set-pref modulepref :multi-in number)
                                                             (progn 
                                                               (om-beep-msg "OSC Player port must be an integer")
                                                               (om-set-dialog-item-text item (format nil "~D" (get-pref modulepref :multi-in))))))))
                                          :font *om-default-font2*)

                     (om-make-dialog-item 'om-static-text (om-make-point l1 (incf pos 40)) (om-make-point 180 24) "MultiPlayer App." :font *controls-font*)

                     (setf multiapp (om-make-dialog-item 'om-static-text (om-make-point l1 (incf pos 20)) (om-make-point 280 50) 
                                                         (if (get-pref modulepref :multip-path) 
                                                             (namestring (get-pref modulepref :multip-path))
                                                           "...")
                                                         :font *om-default-font2*
                                                         :fg-color (if (and (get-pref modulepref :multip-path)
                                                                            (probe-file (get-pref modulepref :multip-path)))
                                                                       *om-black-color* *om-red-color*)))
                     
                     
                     (om-make-view 'om-icon-button 
                                   :icon1 "folder" :icon2 "folder-pushed"
                                   :position (om-make-point (+ l1 280) pos) :size (om-make-point 26 25) 
                                   :action (om-dialog-item-act item
                                             (let* ((path (om-choose-file-dialog :directory (om-default-application-path nil nil))))
                                               (if path
                                                   (if (probe-file path)
                                                       (progn 
                                                         (set-pref modulepref :multip-path path)
                                                         (om-set-dialog-item-text multiapp (namestring path))
                                                         (om-set-fg-color multiapp *om-black-color*)
                                                         (om-invalidate-view thescroll))
                                                     (om-beep-msg "Bad path for MultiPlayer app.")))))))
                        
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
                                          :range '("AIFF" "WAV") 
                                          :value (cond ((equal (get-pref modulepref :audio-format) 'wav) "WAV")
                                                       (t "AIFF"))
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








