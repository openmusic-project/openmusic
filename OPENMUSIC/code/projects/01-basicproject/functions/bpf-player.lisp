(in-package :om)

(defclass! bpf-control (simple-container BPF) 
   ((c-action :initform nil :accessor c-action :initarg :c-action)
    (faust-control :initform nil :accessor faust-control :initarg :faust-control :documentation "A Faust Effect/Synth, or a list of a Faust Effect/Synth and a name of a parameter (e.g : (<faust-synth-console> \"freq\"))")
    (paraminfos :initform nil :accessor paraminfos)
    (paramnum :initform nil :accessor paramnum)
    (faustfun :initform nil :accessor faustfun)))

(defmethod make-one-instance ((self bpf-control) &rest slots-vals)
  (let ((bpf (call-next-method))
        infos fullinf xl1 xl2 yl1 yl2 xlist ylist)
    (setf (c-action bpf) (nth 3 slots-vals))
    (setf (faust-control bpf) (nth 4 slots-vals))
    (if (and (faust-control bpf) 
             (or
              (and
               (listp (faust-control bpf)) 
               (or (typep (car (faust-control bpf)) 'faust-effect-console) (typep (car (faust-control bpf)) 'faust-synth-console)) 
               (typep (cadr (faust-control bpf)) 'string))
              (and
               (not (listp (faust-control bpf)))
               (or (typep (faust-control bpf) 'faust-effect-console) (typep (faust-control bpf) 'faust-synth-console)))))
        (progn
          (setf fullinf (get-infos-from-faust-control (faust-control bpf)))
          (setf (paraminfos bpf) (car fullinf))
          (if (paraminfos bpf)
              (progn
                (setf (paramnum bpf) (cadr fullinf))
                (setf (faustfun bpf) (get-function-from-faust-control bpf))
                (if (<= (- (nth 2 (paraminfos bpf)) (nth 1 (paraminfos bpf))) 10)
                    (if (<= (- (nth 2 (paraminfos bpf)) (nth 1 (paraminfos bpf))) 3)
                        (if (= (nth 2 (paraminfos bpf)) (nth 1 (paraminfos bpf)))
                            (setf (decimals bpf) 0)
                          (setf (decimals bpf) 3))
                      (setf (decimals bpf) 1)))
                  (if (and (equal '(0 100) (nth 1 slots-vals)) (equal '(0 100) (nth 0 slots-vals)))
                      (progn
                        (setf xl1 (interpolate (list 0 500) (list 0 500) 10))
                        (setf xl2 (interpolate (list 9500 10000) (list 9500 10000) 10))
                        (setf xlist (append xl1 xl2))
                        (setf yl1 (interpolate (list 0 500) (list (nth 1 (paraminfos bpf)) (nth 3 (paraminfos bpf))) 10))
                        (setf yl2 (interpolate (list 9500 10000) (list (nth 3 (paraminfos bpf)) (nth 2 (paraminfos bpf))) 10))
                        (setf ylist (append yl1 yl2))
                        (setf (y-points bpf) ylist)
                        (setf (x-points bpf) xlist))))
                (print "I cannot build a bpf-control with these parameters"))
            bpf))))

(defmethod omng-copy ((self bpf-control))
  (let ((bpf (eval (call-next-method))))
    (setf (c-action bpf) (c-action self)
          (faust-control bpf) (faust-control self)
          (decimals bpf) (decimals self)
          (x-points bpf) (x-points self)
          (y-points bpf) (y-points self))
    bpf))

(defmethod copy-container ((self bpf-control) &optional (pere ()))
   "Builds a copy of a bpf control"
   (let ((bpf (eval (call-next-method))))
    (setf (c-action bpf) (c-action self)
          (faust-control bpf) (faust-control self)
          (decimals bpf) (decimals self)
          (x-points bpf) (x-points self)
          (y-points bpf) (y-points self))
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
  (let ((faustfun (faustfun object)))
    (when (or (c-action object) (faust-control object))
      (if interval
          (mapcar #'(lambda (point)
                      (if (and (>= (car point) (car interval)) (<= (car point) (cadr interval)))
                          (progn
                            (if (c-action object) 
                                (schedule-task player 
                                               #'(lambda () (funcall (c-action object) (cadr point))) 
                                               (+ at (car point))))
                            (if (faust-control object) 
                                (schedule-task player
                                               #'(lambda () (funcall faustfun (cadr point))) 
                                               (+ at (car point)))))))
                  (point-pairs object))
        (mapcar #'(lambda (point)
                    (if (c-action object)
                        (schedule-task player 
                                       #'(lambda () (funcall (c-action object) (cadr point))) 
                                       (+ at (car point))))
                    (if (faust-control object) 
                        (schedule-task player
                                       #'(lambda () (funcall faustfun (cadr point))) 
                                       (+ at (car point)))))
                (point-pairs object))))))

;(defmethod player-stop ((player bpf-player) &optional object)
;  (call-next-method)
;  (player-unschedule-all player))

(defmethod default-edition-params ((self bpf-control)) 
  (pairlis '(player) '(:bpfplayer) (call-next-method)))

(defmethod view-turn-pages-p ((self bpfcontrolpanel)) nil)
;;;=======================================================

(defmethod get-editor-class ((self bpf-control)) 'bpfcontroleditor)

(defclass bpfcontroleditor (bpfeditor play-editor-mixin) ())
;;(defmethod get-score-player ((self bpfcontroleditor)) :bpfplayer)
;(defmethod get-edit-param ((self bpfcontroleditor) (param (eql 'player))) :bpfplayer)
(defmethod cursor-panes ((self bpfcontroleditor)) (list (panel self)))

(defclass bpfcontrolpanel (bpfpanel cursor-play-view-mixin) ())
(defmethod view-turn-pages-p ((self bpfcontrolpanel)) nil)
(defmethod get-panel-class ((Self bpfcontroleditor)) 'bpfcontrolpanel)

(defmethod get-x-range ((self bpfcontrolpanel))
  (let* ((bpf (object (editor self)))
         (range (give-bpf-range bpf))
         (xrange (list (nth 0 range) (nth 1 range))))
    xrange))

(defmethod time-to-pixels ((self bpfcontrolpanel) time)
  (call-next-method self (* time (expt 10 (decimals (object (editor self)))))))

(defmethod handle-key-event ((Self bpfcontrolpanel) Char)
  (cond ((equal char #\SPACE) (editor-play/stop (editor self)))
        (t (call-next-method))))

(defun get-function-from-faust-control (bpf)
  (let* ((faust-control (faust-control bpf))
         (console (if (listp faust-control) (car faust-control) faust-control))
         (ptr (if (typep console 'faust-effect-console) (effect-ptr console) (synth-ptr console)))
         (maxnum (las-faust-get-control-count ptr))
         (infos (paraminfos bpf))
         (found (paramnum bpf))
         minval maxval range text-to-up display graph-to-up paramtype)
    (setf minval (nth 1 infos))
    (setf maxval (nth 2 infos))
    (if (= minval maxval) (setf minval 0
                                maxval 1))
    (setf range (- maxval minval))
    (setf paramtype (param-type (nth found (params-ctrl console))))
    (if (display (nth found (params-ctrl console)))
        #'(lambda (val)
            (if (< val minval) (setf val minval))
            (if (> val maxval) (setf val maxval))
            (las-faust-set-control-value ptr found (float val))
            (cond ((string= paramtype "checkbox")
                   (om-set-check-box (paramgraph (display (nth found (params-ctrl console)))) (if (>= val 1) t)))
                  ((string= paramtype "numentry")
                   (progn
                     (om-set-dialog-item-text (paramval (display (nth found (params-ctrl console)))) (number-to-string (float val)))
                     (set-value (paramgraph (display (nth found (params-ctrl console)))) (* 100 (/ (- val minval) range)))))
                  (t 
                   (progn
                     (om-set-dialog-item-text (paramval (display (nth found (params-ctrl console)))) (number-to-string (float val)))
                     (om-set-slider-value (paramgraph (display (nth found (params-ctrl console)))) (* 100 (/ (- val minval) range)))))))
      #'(lambda (val) 
          (las-faust-set-control-value ptr found (float val))))))

(defun get-infos-from-faust-control (faust-control)
  (let* ((name (if (listp faust-control) (cadr faust-control)))
         (console (if (listp faust-control) (car faust-control) faust-control))
         (ptr (if (typep console 'faust-effect-console) (effect-ptr console) (synth-ptr console)))
         maxnum
         found
         listing)
    (if (and ptr (not (las-faust-null-ptr-p ptr)))
        (progn
          (setf maxnum (las-faust-get-control-count ptr))
          (if name
              (loop for i from 0 to (- maxnum 1) do
                    (if (string= name (car (las-faust-get-control-params ptr i)))
                        (setf found i))))
          (if found
              (list (las-faust-get-control-params ptr found) found)
            (let ((param-n (make-param-select-window 
                            (loop for i from 0 to (- maxnum 1) collect
                                  (car (las-faust-get-control-params ptr i))))))
              (if param-n 
                  (list (las-faust-get-control-params ptr param-n) param-n)
                nil)))))))



(defun make-param-select-window (listing)
  (let ((win (om-make-window 'om-dialog
                             :window-title "Parameter selection" 
                             :size (om-make-point 400 210) 
                             :scrollbars nil
                             :position (om-make-point 100 50)))
        (maxchar 0))
   (loop for i from 0 to (1- (length listing)) do
         (if (< maxchar (count-if #'standard-char-p (nth i listing)))
             (setf maxchar (count-if #'standard-char-p (nth i listing)))))
    (setf panel (om-make-view 'om-view
                              :owner win
                              :position (om-make-point 0 0) 
                              :scrollbars nil
                              :retain-scrollbars nil
                              :bg-color *om-dark-gray-color*
                              :field-size  (om-make-point 400 200)
                              :size (om-make-point (w win) (h win))))
    (setf text1 (om-make-dialog-item 'om-static-text 
                                     (om-make-point 85 5) 
                                     (om-make-point 295 20)
                                     (format nil "Empty or invalid parameter name.")
                                     :font *om-default-font2b* 
                                     :fg-color *om-white-color*))
    (setf text2 (om-make-dialog-item 'om-static-text 
                                     (om-make-point 25 25) 
                                     (om-make-point 390 20)
                                     (format nil "Please select the parameter you want to automate in this list :")
                                     :font *om-default-font1* 
                                     :fg-color *om-white-color*))
    (setf paramlist (om-make-dialog-item  'om-single-item-list 
                                          (om-make-point 50 55) 
                                          (om-make-point 300 100) 
                                          "Available parameters"  
                                          :scrollbars (cond ((and (> (length listing) 4) (not (> maxchar 43))) :v)
                                                            ((and (> maxchar 43) (not (> (length listing) 4))) :h)
                                                            ((and (> (length listing) 4) (> maxchar 10)) t)
                                                            (t nil))
                                          :bg-color *om-dark-gray-color*
                                          :fg-color *om-white-color*
                                          :after-action (om-dialog-item-act item 
                                                          (om-return-from-modal-dialog win (om-get-selected-item-index item)))
                                          :range listing))
    (setf ok (om-make-dialog-item 'om-button 
                                  (om-make-point 105 163) 
                                  (om-make-point 70 24)  "OK"
                                  :di-action (om-dialog-item-act item 
                                               (if (om-get-selected-item-index paramlist)
                                                   (om-return-from-modal-dialog win (om-get-selected-item-index paramlist))))))
    (setf cancel (om-make-dialog-item 'om-button 
                                      (om-make-point 225 163) 
                                      (om-make-point 70 24)  "Cancel"
                                      :di-action (om-dialog-item-act item (om-return-from-modal-dialog win nil))))
    (om-add-subviews panel text1 text2 paramlist ok cancel)
    (om-modal-dialog win)))
