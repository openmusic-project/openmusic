
(in-package :om)

(defclass! bpf-control (simple-container BPF) 
   ((c-action :initform nil :accessor c-action :initarg :c-action)))

(defmethod make-one-instance ((self bpf-control) &rest slots-vals)
  (let ((bpf (call-next-method)))
    (setf (c-action bpf) (nth 3 slots-vals))
    bpf))

(defmethod omng-copy ((self bpf-control))
  (let ((bpf (eval (call-next-method))))
    (setf (c-action bpf) (c-action self))
    bpf))

(defmethod print-object ((self bpf-control) stream)
  (call-next-method))
;  (format stream "BPF-CONTROL: ~A ~D" (c-action self) (slot-value self 'offset)))

(defmethod play-obj? ((self bpf-control)) t)
(defmethod allowed-in-maq-p ((self bpf-control)) t)
(defmethod get-obj-dur ((self bpf-control)) (last-elem (x-points self)))
;(defclass bpf-player (omplayer) ())
;(defmethod class-from-player-type ((type (eql :bpfplayer))) 'bpf-player)

(defmethod prepare-to-play ((self (eql :bpfplayer)) (player omplayer) (object bpf-control) at interval)
  ;(player-unschedule-all self)
  (when (c-action object)
    (if interval
        (mapcar #'(lambda (point) 
                    (if (and (>= (car point) (car interval)) (<= (car point) (cadr interval)))
                        (schedule-task player #'(lambda () (funcall (c-action object) (cadr point))) (+ at (car point)))))
                (point-pairs object))
      (mapcar #'(lambda (point) 
                  (schedule-task player #'(lambda () (funcall (c-action object) (cadr point))) (+ at (car point))))
              (point-pairs object)))
    ))

;(defmethod player-stop ((player bpf-player) &optional object)
;  (call-next-method)
;  (player-unschedule-all player))

(defmethod default-edition-params ((self bpf-control)) 
  (pairlis '(player) '(:bpfplayer) (call-next-method)))


;;;=======================================================

(defmethod get-editor-class ((self bpf-control)) 'bpfcontroleditor)

(defclass bpfcontroleditor (bpfeditor play-editor-mixin) ())
;;(defmethod get-score-player ((self bpfcontroleditor)) :bpfplayer)
;(defmethod get-edit-param ((self bpfcontroleditor) (param (eql 'player))) :bpfplayer)
(defmethod cursor-panes ((self bpfcontroleditor)) (list (panel self)))

(defclass bpfcontrolpanel (bpfpanel cursor-play-view-mixin) ())
(defmethod view-turn-pages-p ((self bpfcontrolpanel)) nil)
(defmethod get-panel-class ((Self bpfcontroleditor)) 'bpfcontrolpanel)

(defmethod time2pixel ((self bpfcontrolpanel) time)
  (call-next-method self (* time (expt 10 (decimals (object (editor self)))))))

(defmethod handle-key-event ((Self bpfcontrolpanel) Char)
  (cond ((equal char #\SPACE) (editor-play/stop (editor self)))
        (t (call-next-method))))
