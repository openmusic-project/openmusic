(in-package :om)


(defmethod player-name ((self (eql :multiplayer))) "MultiPlayer")
(defmethod player-desc ((self (eql :multiplayer))) "external Max multi-channel player")
(defmethod player-special-action ((self (eql :multiplayer))) (launch-multiplayer-app))
(defmethod player-type ((player (eql :multiplayer))) :UDP)

(defvar *multiplayer-out-port* nil)
(setf *multiplayer-out-port* 7071)
(defvar *multiplayer-in-port* nil)
(setf *multiplayer-in-port* 7072)
(defvar *multiplayer-host* nil)
(setf *multiplayer-host* "127.0.0.1")
 
;==================
; APP
;==================

(defvar *multiplayer-app* nil)
(defvar *multiplayer-path* nil)

(defun init-multiplayer-app ()
  (setf *multiplayer-path* 
        (probe-file (om-default-application-path '() "MultiPlayer"))))


(om-add-init-func 'init-multiplayer-app)

(defun launch-multiplayer-app ()
  (unless (and *multiplayer-app* (om-find-process *multiplayer-app*))
    (let ((path *multiplayer-path*))
      (if (or (null path) (not (probe-file path)))
        (om-message-dialog "MultiPlayer application not found. Please set the application path in OM preferences.") 
      (setf *multiplayer-app* (om-run-application path))
      ))))

;================
; PROTOCOL
;================

(defvar *multiplayer-file-to-play* nil)

;(defmethod InitPlayingSeq ((player (eql 'multiplayer)) dur &key (port nil)) t)
;(defmethod FinalizePlayingSeq ((player (eql 'multiplayer)) dur &key (port nil)) t)

(defmethod prepare-to-play ((engine (eql :multiplayer)) (player omplayer) object at interval)
   
   (when *multiplayer-file-to-play* (player-stop :multiplayer (list *multiplayer-file-to-play*)))
   
   ;(print (list "multiplayer prepare" (om-sound-file-name object)))

   (setf *multiplayer-file-to-play* (namestring (om-sound-file-name object)))

   ;(let* ((message nil)
   ;       (reply nil)
   ;       (timeout 500)
   ;       (listen-answer-process (om-start-osc-server *multiplayer-in-port* "localhost"  
   ;                                                   #'(lambda (msg host) 
   ;                                                       (let* ((rmsg (om-decode-msg-or-bundle msg))
   ;                                                              (msgcontents (if (listp (cadr rmsg)) (cadr rmsg) rmsg)))
   ;                                                        ; (print (format nil "RECEIVED FROM MultiPlayer ~A" msgcontents))
   ;                                                        ; (when (and (not reply)
   ;                                                        ;            (stringp (car msgcontents))
   ;                                                        ;            (string-equal "/Multiplayer/loaded" (car msgcontents)))
   ;                                                       ;    (setf reply t)
    ;                                                       ;   (setf message msgcontents)
   ;                                                       ;    )
   ;                                                         )
   ;                                                       ))))
   (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/open" (namestring (om-sound-file-name object))))
   (sleep 0.1)
   ;(loop for count = 0 then (+ count 1)
   ;      while (not reply) do
   ;      (sleep 0.01)
   ;      (setf count (+ count 1))
   ;      (when (> count timeout)
   ;        (print "file loader did not answer...")
   ;        (setf reply t)))
   ;(om-stop-osc-server listen-answer-process)
   ;)
   (when interval
     (let ((newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at)))
     (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/start" (/ (car newinterval) 1000.0)))))
   
   ;;; schedule the playback
   (call-next-method)
   )



(defmethod player-start ((engine (eql :multiplayer)) &optional play-list) (call-next-method))

(defmethod player-play-object ((engine (eql :multiplayer)) (object sound) &key interval)
  ;(print "multiplayer play")
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/play" 1))))

(defmethod player-stop ((engine (eql :multiplayer)) &optional play-list)
  ;(print "multiplayer stop")
  (when *multiplayer-file-to-play*
     (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/play" 0))
     (setf *multiplayer-file-to-play* nil)))

(defmethod player-pause ((engine (eql :multiplayer)) &optional play-list)
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/pause" 1))))

(defmethod player-continue ((engine (eql :multiplayer)) &optional play-list)
  (when *multiplayer-file-to-play*
    (om-send-osc-message *multiplayer-out-port* *multiplayer-host*  (list "/fileplayer/pause" 0))))



;;;=========================================
;;; PREFS

;;;==================================
;;;; ADD EXTERNAL PREF MODULE
;;;==================================

(add-external-pref-module 'multiplayer)

(defmethod get-external-name ((module (eql 'multiplayer))) "MultiPlayer")
(defmethod get-external-icon ((module (eql 'multiplayer))) 950)

(defmethod get-external-module-vals ((module (eql 'multiplayer)) modulepref) (get-pref modulepref :multiplayer-options))
(defmethod get-external-module-path ((module (eql 'multiplayer)) modulepref) (get-pref modulepref :multiplayer-path))
(defmethod set-external-module-vals ((module (eql 'multiplayer)) modulepref vals) (set-pref modulepref :multiplayer-options vals))
(defmethod set-external-module-path ((module (eql 'multiplayer)) modulepref path) 
  (set-pref modulepref :multiplayer-path path))

(defun def-multiplayer-options () '(7071 7072 "127.0.0.1"))

(defmethod get-external-def-vals ((module (eql 'multiplayer))) 
    (list :multiplayer-path (when *multiplayer-path* (probe-file *multiplayer-path*))
          :multiplayer-options (def-multiplayer-options)))

(defmethod save-external-prefs ((module (eql 'multiplayer))) 
  `(:multiplayer-path ,(om-save-pathname *multiplayer-path*) 
    :multiplayer-options (list ,*multiplayer-out-port* ,*multiplayer-in-port* ,*multiplayer-host*)))


(defmethod put-external-preferences ((module (eql 'microplayer)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :multiplayer-options)))
    (when list-prefs 
      (setf *multiplayer-out-port* (nth 0 list-prefs))
      (setf *multiplayer-in-port* (nth 1 list-prefs))
      (setf *multiplayer-host* (nth 2 list-prefs))
      )
    (when (get-pref moduleprefs :multiplayer-path)
      (setf *multiplayer-path* (find-true-external (get-pref moduleprefs :multiplayer-path))))
    ))

(put-external-preferences 'multiplayer (find-pref-module :externals))



(defmethod show-external-prefs-dialog ((module (eql 'multiplayer)) prefvals)

  (let* ((rep-list (copy-list prefvals))
         (dialog (om-make-window 'om-dialog
                                 :window-title "MultiPlayer Options"
                                 :size (om-make-point 360 220)
                                 :position :centered
                                 :resizable nil :maximize nil :close nil))
         (i 10) initem outitem hostitem)
    
    (om-add-subviews dialog
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 10 i) (om-make-point 250 24) "MultiPlayer settings" :font *om-default-font2b*)

                     (om-make-dialog-item 'om-static-text (om-make-point 10 (incf i 35)) (om-make-point 150 24) "UDP Ports" :font *om-default-font2*);

                     (om-make-dialog-item 'om-static-text (om-make-point 120 i) (om-make-point 150 20) "OM Out" :font *om-default-font2*)
                     
                     (setf outitem (om-make-dialog-item 'om-editable-text (om-make-point 190 i) (om-make-point 42 13)
                                          (format nil "~D" (nth 0 prefvals))
                                          :font *om-default-font1*))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 120 (incf i 25)) (om-make-point 150 24) "OM In" :font *om-default-font2*)
                     
                     (setf initem (om-make-dialog-item 'om-editable-text (om-make-point 190 i) (om-make-point 42 13)
                                          (format nil "~D" (nth 1 prefvals))
                                          :font *om-default-font1*))
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 10 (incf i 35)) (om-make-point 150 24) 
                                          "MultiPlayer Host " :font *om-default-font2*)
                     
                     (setf hostitem (om-make-dialog-item 'om-editable-text (om-make-point 165 i) 
                                                         (om-make-point 100 13)
                                                         (nth 2 prefvals) 
                                                         :font *om-default-font1*))




      
      ;;; boutons
      (om-make-dialog-item 'om-button (om-make-point 15 (incf i 55)) (om-make-point 90 20) "Restore"
                           :di-action (om-dialog-item-act item
                                        (om-set-dialog-item-text outitem (number-to-string (nth 0 (def-multiplayer-options))))
                                        (om-set-dialog-item-text initem (number-to-string (nth 1 (def-multiplayer-options))))
                                        (om-set-dialog-item-text hostitem (number-to-string (nth 2 (def-multiplayer-options))))
                                        ))
      
      (om-make-dialog-item 'om-button (om-make-point 160 i) (om-make-point 90 20) "Cancel"
                           :di-action (om-dialog-item-act item
                                        (om-return-from-modal-dialog dialog nil)))
      
      (om-make-dialog-item 'om-button (om-make-point 250 i) (om-make-point 90 20) "OK"
                           :di-action (om-dialog-item-act item
                                        (let* ((argerror nil)
                                               (intxt (om-dialog-item-text initem)) 
                                               (in (and (not (string= "" intxt)) (read-from-string intxt)))
                                               (outtxt (om-dialog-item-text outitem)) 
                                               (out (and (not (string= "" outtxt)) (read-from-string outtxt))))
                                         
                                          (if (and (integerp in)
                                                   (>= in 0)
                                                   (integerp out)
                                                   (>= out 0)
                                                   (not (= in out)))
                                              (setf (nth 0 rep-list) out
                                                    (nth 1 rep-list) in)
                                            (setf argerror t))
                                          
                                          (if (not (string= "" (om-dialog-item-text hostitem)))
                                              (setf (nth 2 rep-list) (om-dialog-item-text hostitem))
                                            (setf argerror t))
                                          
                                          (if argerror
                                              (om-message-dialog (format nil "Error in a MultiPlayer option!~%Preference values could not be set."))
                                            (om-return-from-modal-dialog dialog rep-list))
                                          ))
                           :default-button t :focus t)
      )
    (om-modal-dialog dialog)))






