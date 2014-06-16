(in-package :om)



;;; METHODS TO REDEFINE FOR EVERY PLAYER                   
(defmethod player-name ((player (eql :microplayer))) "MicroPlayer")   ;;; A short name
(defmethod player-desc ((player (eql :microplayer))) "external Max player")   ;;; a description
(defmethod player-special-action ((player (eql :microplayer))) (launch-microplayer-app))  ;;; an action to perform when the player is selected for an object (e.g. activate...)
(defmethod player-params ((player (eql :microplayer))) nil)   ;;; the default values for the player params
(defmethod player-type ((player (eql :microplayer))) :UDP)   ;;; communication protocol (:midi / :udp)
(defmethod player-init ((player (eql :microplayer))) (restart-microplayer))  ;;; called when this player is selected / options changed etc.

(enable-player :microplayer)

;===================================================
;MICROPLAYER PROCESS

(defvar *microosc-packets* nil)
(defvar *index-packets* 0)

(defvar *microplayer-in-port* nil)
(setf *microplayer-in-port* 3010)

(defvar *microplayer-out-port* nil)
(setf *microplayer-out-port* 3000)

(defvar *microplayer-host* nil)
(setf *microplayer-host* "127.0.0.1")

;; (defvar *max-buf-size* nil)
;; definie dans l'api
(setf *om-udp-max-buf-size* 500)


(defvar *micro-listen-process* nil)

;;; MARCHE PAS !!!
;;; look like MicroPLayer does not send the message...
(defun open-microplayer ()
  (unless *micro-listen-process*
    (setf *micro-listen-process* (om-start-osc-server *microplayer-in-port* *microplayer-host* #'send-more-notes))))

(defun restart-microplayer ()
  (close-microplayer)
  (open-microplayer) )
    
(defun close-microplayer ()
  (when *micro-listen-process*
    (om-stop-osc-server *micro-listen-process*)
    (setf *micro-listen-process* nil)))

; (om-add-init-func 'open-microplayer)
; (om-add-exit-cleanup-func 'close-microplayer)


;=====================================================
;MICROPLAYER APP

(defvar *micro-player-app* nil)
(defvar *micro-player-path* nil)

;(defun init-microplayer-app ()
;  (setf *micro-player-path* 
;        (or (probe-file (om-default-application-path '("MicroPlayer") "bm-microton"))
;            (probe-file (om-external-app nil "MicroPlayer"))))
;  (enable-player :microplayer))

;(om-add-init-func 'init-microplayer-app)


(defun launch-microplayer-app ()
  (unless (and *micro-player-app* (om-find-process *micro-player-app*))
    (let ((path *micro-player-path*))
      (if (or (null path) (not (probe-file path)))
        (om-message-dialog "MicroPlayer application not found. Please set the application path in OM preferences.") 
      (setf *micro-player-app* (om-run-application path))
      )))
  ;(open-microplayer)
  )

; (launch-microplayer-app)


;=====================================================
;MICROPLAYER PROTOCOL

(defun micro-reset ()
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/reset")))
  (setf *microosc-packets* nil))

;================
(defun micro-start ()
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/start"))))

;================
;(defun sort-micro-events ()
;  (setf *microosc-packets* (sort *microosc-packets* '< :key 'second)))
(defun sort-micro-events (list)
  (sort list '< :key 'microplay-note-date))

;================

(defstruct microplay-note (date) (pitch) (vel) (dur) (chan))

(defun send-200 ()
  (loop for i from 1 to 200
        while (< *index-packets* (length *microosc-packets*)) do
        (let ((event (copy-microplay-note (nth *index-packets* *microosc-packets*))))
          (unless (zerop *index-packets*)
            (setf (microplay-note-date event) (- (microplay-note-date event) (microplay-note-date (nth (- *index-packets* 1) *microosc-packets*)))))
          (setf *index-packets* (+ *index-packets* 1))
          (om-send-osc-bundle *microplayer-out-port* *microplayer-host* 
                              (list (list "/play.µt/fifos" 
                                          (microplay-note-date event)
                                          (microplay-note-pitch event)
                                          (microplay-note-vel event)
                                          (microplay-note-dur event)
                                          (microplay-note-chan event)
                                          )))))
  )

;================


(defun send-more-notes (msg)
  (let ((message (om-decode-msg-or-bundle msg)))
  (when (string-equal (string (car message)) "/play.mt/more")
    (send-200)
    (micro-start))
  nil))
 
;================


(defmethod PrepareToPlay ((player (eql :microplayer)) (self t) at &key  approx port interval voice)
   (declare (ignore approx port interval voice))
   (call-next-method))


(defmethod PrepareToPlay ((player (eql :microplayer)) (self arp-chord) at &key approx port interval voice)
   (loop for note in (notes self)
          for offset from 0 by 400
          collect (PrepareToPlay player note (+ offset at) 
                                 :approx approx
                                 :port port :interval interval :voice voice)))

(defmethod PrepareToPlay ((player (eql :microplayer)) (self measure) at &key approx port interval voice)
   (loop for sub in (inside self) collect
         (let* ((objstart (+ at (offset->ms sub)))
                (in-interval (interval-intersec interval (list objstart (+ objstart (get-obj-dur sub))))))
             (PrepareToPlay player sub objstart 
                            :approx approx 
                            :interval (if in-interval interval nil)))))
   
(defmethod PrepareToPlay ((player (eql :microplayer)) (self note) at &key approx port interval voice)
   (when (not (memq (tie self) '(continue end)))
     (let ((chan (chan self))
           (pitch (/ (if approx 
                         (approx-scale (get-current-scale approx) (midic self))
                       (midic self))  ;;; IF NO APPROX: PLAY FULL PRECISION
                     100.0))
           (vel (vel self))
           (dur (- (real-dur self) 2))
           (date at))  ;;; (+  *MidiShare-start-time* at)))
       (if interval
         (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
           (when newinterval
             (make-osc-note chan pitch vel (- (second newinterval) (first newinterval) 1) 
                       (- (first newinterval) ;;; (+  *MidiShare-start-time* (first newinterval)) 
                          (first interval)))))
         (make-osc-note chan pitch vel dur date)))))

(defun make-osc-note (chan pitch vel dur date)
   (list (make-microplay-note :date date :pitch pitch :vel vel :dur dur :chan chan)))


;;; = tests to fix the microplayer + maquette
(defmethod prepare-to-play ((engine (eql :microplayer)) (player omplayer) object at interval params)
  ;;;(player-stop :microplayer)
  ;;;(setf *microosc-packets* nil)
  ;;;(setf *MidiShare-start-time* 1)
  (let ((approx (if (caller player) (get-edit-param (caller player) 'approx))))
    (setf *microosc-packets* (sort-micro-events 
                              (append *microosc-packets*  ;;;
                                      (remove nil 
                                              (flat 
                                               (PrepareToPlay :microplayer object (+ at (real-duration object 0)) 
                                                              :interval interval :approx approx)))
                                      ) ;;;
                                      ))))

(defmethod player-start ((self (eql :microplayer)) &optional play-list)
  ;;;
  ;(player-stop :microplayer)
  (setf *MidiShare-start-time* 1)
  ;;;
  (open-microplayer)
  (setf *index-packets* 0) 
  (send-200) 
  (micro-start))

(defmethod player-stop  ((self (eql :microplayer)) &optional play-list)
   (declare (ignore view))
   ;;
   (setf *microosc-packets* nil)
   ;;
   (close-microplayer)
   (micro-reset))

(defmethod player-continue ((self (eql :microplayer)) &optional play-list)
    (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/continue"))))

(defmethod player-pause ((self (eql :microplayer)) &optional play-list)
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/pause"))))




(defmethod player-loop ((self (eql :microplayer)) player &optional play-list)
  (declare (ignore player))
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/reset")))
  (setf *index-packets* 0)
  (send-200)
  (om-send-osc-bundle *microplayer-out-port* *microplayer-host*  '(("/play.µt/start")))
  )





;;;==================================
;;;; ADD EXTERNAL PREF MODULE
;;;==================================

(add-external-pref-module 'microplayer)

(defmethod get-external-name ((module (eql 'microplayer))) "MicroPlayer")
(defmethod get-external-icon ((module (eql 'microplayer))) 950)

(defmethod get-external-module-vals ((module (eql 'microplayer)) modulepref) (get-pref modulepref :microplay-options))
(defmethod get-external-module-path ((module (eql 'microplayer)) modulepref) (get-pref modulepref :microplay-path))
(defmethod set-external-module-vals ((module (eql 'microplayer)) modulepref vals) (set-pref modulepref :microplay-options vals))
(defmethod set-external-module-path ((module (eql 'microplayer)) modulepref path) 
  (set-pref modulepref :microplay-path path))

(defun def-microplay-options () '(3000 3010 "127.0.0.1"))

(defmethod get-external-def-vals ((module (eql 'microplayer))) 
    (list :microplay-path (probe-file (om-default-application-path '("MicroPlayer") "bm-microton")) 
          ;;;;(when *micro-player-path* (probe-file *micro-player-path*))
          :microplay-options (def-microplay-options)))

(defmethod save-external-prefs ((module (eql 'microplayer))) 
  `(:microplay-path ,(om-save-pathname *micro-player-path*) 
    :microplay-options (list ,*microplayer-out-port* ,*microplayer-in-port* ,*microplayer-host*)))


(defmethod put-external-preferences ((module (eql 'microplayer)) moduleprefs)
  (let ((list-prefs (get-pref moduleprefs :microplay-options)))
    (when list-prefs 
      (setf *microplayer-out-port* (nth 0 list-prefs))
      (setf *microplayer-in-port* (nth 1 list-prefs))
      (setf *microplayer-host* (nth 2 list-prefs))
      )
    (when (get-pref moduleprefs :microplay-path)
      ;(setf *micro-player-path* (find-true-external (get-pref moduleprefs :microplay-path)))
      (setf *micro-player-path* (get-pref moduleprefs :microplay-path))
      )
    ))

(put-external-preferences 'microplayer (find-pref-module :externals))



(defmethod show-external-prefs-dialog ((module (eql 'microplayer)) prefvals)

  (let* ((rep-list (copy-list prefvals))
         (dialog (om-make-window 'om-dialog
                                 :window-title "MicroPlayer Options"
                                 :size (om-make-point 360 220)
                                 :position :centered
                                 :resizable nil :maximize nil :close nil))
         (i 10) initem outitem hostitem)
    
    (om-add-subviews dialog
                     
                     (om-make-dialog-item 'om-static-text (om-make-point 10 i) (om-make-point 250 24) "MicroPlayer settings" :font *om-default-font2b*)

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
                                          "MicroPlayer Host " :font *om-default-font2*)
                     
                     (setf hostitem (om-make-dialog-item 'om-editable-text (om-make-point 165 i) 
                                                         (om-make-point 100 13)
                                                         (nth 2 prefvals) 
                                                         :font *om-default-font1*))




      
      ;;; boutons
      (om-make-dialog-item 'om-button (om-make-point 15 (incf i 55)) (om-make-point 90 20) "Restore"
                           :di-action (om-dialog-item-act item
                                        (om-set-dialog-item-text outitem (number-to-string (nth 0 (def-microplay-options))))
                                        (om-set-dialog-item-text initem (number-to-string (nth 1 (def-microplay-options))))
                                        (om-set-dialog-item-text hostitem (number-to-string (nth 2 (def-microplay-options))))
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
                                              (om-message-dialog (format nil "Error in a MicroPlayer option!~%Preference values could not be set."))
                                            (om-return-from-modal-dialog dialog rep-list))
                                          ))
                           :default-button t :focus t)
      )
    (om-modal-dialog dialog)))







