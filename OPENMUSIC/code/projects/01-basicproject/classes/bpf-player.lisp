(in-package :om)

;================================
; A BPF with basic playback features
;================================
(defmethod player-name ((self (eql :bpfplayer))) "BPF-Player")
(defmethod player-desc ((self (eql :bpfplayer))) "Special player for BPFs and automations")
(defmethod player-type ((player (eql :bpfplayer))) :internal)

(enable-player :bpfplayer)


(defclass! BPF-controller (simple-container BPF) 
  ((player-fun :initform nil :accessor player-fun)))

(defmethod play-obj? ((self BPF-controller)) t)
(defmethod allowed-in-maq-p ((self BPF-controller)) t)
(defmethod get-obj-dur ((self BPF-controller)) (last-elem (x-points self)))


(add-player-for-object 'BPF-controller '(:bpfplayer))

(defmethod default-edition-params ((self BPF-controller)) 
  (pairlis '(player) '(:bpfplayer)))

(defmethod get-editor-class ((self BPF-controller)) 'bpfcontroleditor)

(defmethod get-player-action ((self t)) nil)
(defmethod get-player-action ((self BPF-controller)) (player-fun self))

(defmethod prepare-to-play ((self (eql :bpfplayer)) (player omplayer) (object bpf) at interval params)
  (let ((fun (get-player-action object))) 
    (when fun
      (if interval
          (progn
            (mapcar #'(lambda (point)
                        (if (and (>= (car point) (car interval)) (<= (car point) (cadr interval)))
                            (schedule-task player
                                           #'(lambda () (funcall fun (cadr point))) 
                                           (+ at (car point)))))
                    (point-pairs object)))
        (progn
          (mapcar #'(lambda (point)
                      (schedule-task player
                                     #'(lambda () (funcall fun (cadr point))) 
                                     (+ at (car point))))
                  (point-pairs object)))))))
  
(defmethod player-loop ((self (eql :bpfplayer)) player &optional play-list)
  (declare (ignore player))
  (loop for obj in play-list do
        (prepare-to-play self player obj 0 (play-interval player))))
  
;================================
; EDITOR
;================================

(defclass bpfcontroleditor (bpfeditor play-editor-mixin) ())

(defmethod cursor-panes ((self bpfcontroleditor)) (list (panel self)))
(defmethod get-panel-class ((Self bpfcontroleditor)) 'bpfcontrolpanel)

(defclass bpfcontrolpanel (bpfpanel cursor-play-view-mixin) ())

(defmethod view-turn-pages-p ((self bpfcontrolpanel)) t)

(defmethod om-draw-contents ((Self bpfcontrolpanel))
  (call-next-method)
  (draw-control-info self (currentbpf self)))

(defmethod draw-control-info ((self t) (object t)) nil)

(defmethod time-to-pixels ((self bpfcontrolpanel) time)
  (call-next-method self (* time (expt 10 (decimals (object (editor self)))))))

(defmethod om-set-scroll-position ((self bpfcontrolpanel) pos) nil)

(defmethod get-x-range ((self bpfcontrolpanel))
  (let* ((bpf (object (editor self)))
         (range (give-bpf-range bpf)))
    (list (nth 0 range) (nth 1 range))))

(defmethod handle-key-event ((Self bpfcontrolpanel) Char)
  (cond ((equal char #\SPACE) (editor-play/stop (editor self)))
        (t (call-next-method))))


