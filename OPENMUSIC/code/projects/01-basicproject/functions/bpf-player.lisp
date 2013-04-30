
(in-package :om)

(defclass! bpf-control (BPF) 
   ((c-action :initform nil :accessor c-action :initarg :c-action)))

(defmethod make-one-instance ((self bpf-control) &rest slots-vals)
  (let ((bpf (call-next-method)))
    (setf (c-action bpf) (nth 3 slots-vals))
    bpf))






(defclass bpf-player (omplayer) ())
(defmethod class-from-player-type ((type (eql :bpfplayer))) 'bpf-player)


(defmethod player-play ((self bpf-player) (object bpf-control) &key interval)
  (player-unschedule-all self)
  (if interval
      (mapcar #'(lambda (point) 
                  (if (and (>= (car point) (car interval)) (<= (car point) (cadr interval)))
                      (player-schedule self #'(lambda () (funcall (c-action object) (cadr point))) (car point))))
              (point-pairs object))
    (mapcar #'(lambda (point) 
                (player-schedule self #'(lambda () (funcall (c-action object) (cadr point))) (car point)))
            (point-pairs object)))
  (call-next-method))

(defmethod player-stop ((player bpf-player) &optional object)
  (call-next-method)
  (player-unschedule-all player))


;;;=======================================================

(defmethod get-editor-class ((self bpf-control)) 'bpfcontroleditor)

(defclass bpfcontroleditor (bpfeditor play-editor-mixin) ())
(defmethod get-score-player ((self bpfcontroleditor)) :bpfplayer)
(defmethod cursor-panes ((self bpfcontroleditor)) (list (panel self)))

(defclass bpfcontrolpanel (bpfpanel cursor-play-view-mixin) ())
(defmethod view-turn-pages-p ((self bpfcontrolpanel)) nil)
(defmethod get-panel-class ((Self bpfcontroleditor)) 'bpfcontrolpanel)

(defmethod handle-key-event ((Self bpfcontrolpanel) Char)
  (cond ((equal char #\SPACE) (editor-play/stop (editor self)))
        (t (call-next-method))))
