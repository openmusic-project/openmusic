;; ;;; uses om::MidiEvent class to hold event-data (notes w. durations etc.):

;; (defmethod update-instance-for-different-class :before ((msg midi::message) (ev om::midievent) &key)
;;   (setf (om::ev-date ev) (midi::message-time msg)))

;; (defmethod update-instance-for-different-class :before ((msg midi::channel-message) (ev om::midievent) &key)
;;   (setf (om::ev-chan ev) (midi::message-channel msg)))

;; (defmethod update-instance-for-different-class :before ((msg midi::tempo-message) (ev om::midievent) &key)
;;   (setf (om::ev-type ev) (om-midi-get-num-from-type "Tempo"))
;;   (setf (om::ev-fields ev) (midi::message-tempo msg)))

;; (defmethod update-instance-for-different-class :before ((msg midi::note-on-message) (ev om::midievent) &key)
;;   (setf (om::ev-type ev) (om-midi-get-num-from-type "Note"))
;;   (setf (om::ev-fields ev) (list (midi::message-key msg) (midi::message-velocity msg) nil)))

;; (defmethod update-instance-for-different-class :before ((msg midi::note-off-message) (ev om::midievent) &key)
;;   (setf (om::ev-type ev) (om-midi-get-num-from-type "keyOff"))
;;   (setf (om::ev-fields ev) (list (midi::message-key msg) (midi::message-velocity msg) nil)))



;; (defun om-midi-evt-get (event slot)
;;   (case slot
;;     (:type (om::ev-type event))
;;     (:date (om::ev-date event))
;;     (:ref (om::ev-ref event))
;;     (:port (om::ev-port event))
;;     (:chan (om::ev-chan event))
;;     (:fields (om::ev-fields event))
;;     (:dur (third (om::ev-fields event)))
;;     (:pitch (first (om::ev-fields event)))
;;     (:vel (second (om::ev-fields event)))
;;     (:kpress (om::ev-kpress event))
;;     (:tempo (om::ev-fields event))
;;     (:text (om::ev-fields event))))

;; (defun om-midi-get-evt-text (event)
;;   (om::ev-text event))

;; (defun om-midi-evt-set (evt &key dur date port ref chan pgm param kpress bend tempo ctrlchange vals bytes field text)
;;   (when dur (setf (om::ev-dur evt) dur))
;;   (when date (setf (om::ev-date evt) date))
;;   (when port (setf (om::ev-port evt) port))
;;   (when chan (setf (om::ev-chan evt) chan))
;;   (when ref (setf (om::ev-ref evt) ref))
;;   (when pgm (setf (om::ev-pgm evt) pgm))
;;   (when param (setf (om::ev-param evt) param))
;;   (when kpress (setf (om::ev-kpress evt) kpress))
;;   (when bend (setf (om::ev-bend evt) bend))
;;   (when tempo (setf (om::ev-tempo evt) tempo))
;;   (when text (setf (om::ev-text evt) text))
;;   (when ctrlchange 
;;     (setf (om::ev-ctrl evt) (car ctrlchange))
;;     (setf (om::ev-val evt) (cadr ctrlchange)))
;;   (when bytes (dolist (byte (if (consp bytes) bytes (list bytes)))
;;                 (setf (om::ev-midiaddfield evt) byte)))
;;   (when vals
;;     (if (listp vals)
;;         (loop for v in vals
;; 	   for i = 0 then (+ i 1)
;; 	   do (setf (om::ev-field evt) (list i v)))
;; 	(setf (om::ev-field evt) (list 0 vals))))
;;   (when field (setf (om::ev-field evt) (list (car field) (cadr field)))))


;; ;; using arbitrary slot to hold pointer:
;; (defmethod ev-link ((evt om::midievent))
;;   (slot-value evt 'om::extra-obj-list))

;; (defun ev-set-link (evt next)
;;   (setf (slot-value evt 'om::extra-obj-list) next))

;; (defsetf ev-link ev-set-link)

;; (defun ev-set-dur (evt dur)
;;   (setf (third (om::ev-fields evt)) dur))

;; (defsetf om::ev-dur ev-set-dur)

