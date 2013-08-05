;;; SCORE EDITOR ANALYSIS FUNCTIONS
;;; CALLED FROM MAIN SCORE EDITOR 

(in-package :om)


(defmethod analysis-mode? ((self scorepanel)) (= (score-mode self) 3))

(defmethod segmentation-update ((self scorepanel))
  (loop for an in (list! (analysis (object (editor self))))
        do (analysis-update an (object (editor self)))))



;;;============================
;;; DRAW
;;;============================

(defmethod draw-analysis ((self scorepanel)) 
  (when (analysis (reference (graphic-obj self)))
  (let ((t1 (pixels-to-time self (om-h-scroll-position self)))
        (t2 (pixels-to-time self (+ (om-h-scroll-position self) (w self)))))
    (om-with-focused-view self
      ;(loop for an in (list! (analysis (reference (graphic-obj self)))) do
      ;      (draw-analysis-segments an self t1 t2))
      (draw-analysis-segments (car (list! (analysis (reference (graphic-obj self))))) self t1 t2)
      ))))

(defmethod draw-analysis-segments ((self abstract-analysis) (view scorepanel) &optional tmin tmax) 
  (om-with-focused-view view
    (loop for seg in (analysis-segments self) do
          ;(print (list tmin tmax (segment-data seg) (segment-begin seg) (segment-end seg)))
          (when (or (and (segment-begin seg) (>= (segment-begin seg) tmin) (<= (segment-begin seg) tmax))
                    (and (segment-end seg) (>= (segment-end seg) tmin) (<= (segment-end seg) tmax)))
            (draw-one-analysis-segment seg self view)))))

(defmethod draw-one-analysis-segment ((self segment) analysis view)
  (draw-segment self view)
  (draw-segment-data analysis self view))

(defmethod analysis-infostring ((self scoreeditor))
  (let ((curr-seg (car (list! (analysis (object self))))))
    (if (car (list! (analysis (object self))))
        (format nil "Current Segmentation: ~A [~A] (~A)" 
                (if (get-name curr-seg) (string (get-name curr-seg)) "")
                (string (type-of curr-seg))
                (string-downcase (if (> (length (analysis-segments curr-seg)) 0)
                                     (format nil "size=~D" (length (analysis-segments curr-seg)))
                                   "empty")))
      "Currently no segmentation/analysis")))

;;;============================
;;; CLICS AND ACTIONS
;;;============================

;;;====================
;;; CLICK

;;; RETURNS THE SELECTED SEGMENT IN THE EDITOR
(defmethod get-selected-segments ((self scorepanel))
  (loop for an in (list! (analysis (object (editor self)))) append 
        (selected-segments an)))

(defmethod off-analysis-selection ((self scorepanel))
  (let ((sel (get-selected-segments self)))
    (loop for item in sel do
          (setf (selected item) nil))
    (loop for an in (list! (analysis (object (editor self)))) do 
          (setf (selected-segments an) nil))))


;;; RETURNS THE LIST OF CLICKED SEGMENTS
;;; as pairs (ANALYSIS - SEGMENT)
(defmethod click-in-segment ((self scorepanel) where)
  (let ((current-analysis (car (list! (analysis (reference (graphic-obj self)))))))
    (when current-analysis
      (loop for seg in (analysis-segments current-analysis) 
            when (segment-clicked-p seg self where) 
            collect (list current-analysis seg)))))


(defmethod analysis-list-p ((self t)) nil)
(defmethod analysis-list-p ((self list)) 
  (let ((split (mat-trans self)))
    (and (list-subtypep (car split) 'abstract-analysis)
         (list-subtypep (cadr split) 'segment))))

;;; HANDLES CLICK ACTIONS ON THE PAIRS ANALYSIS/SEGMENT SELECTED
(defmethod handle-click-analysis ((self list) panel pos) 
  (setf (selection? panel) nil)
  (loop for an in (list! (analysis (object (editor panel)))) do
        (setf (selected-segments an) nil))
  (when (analysis-list-p self)  
    (mapcar #'(lambda (selected-item) 
                (pushr (cadr selected-item) (selected-segments (car selected-item)))
                (setf (selected (cadr selected-item)) t)
                (handle-segment-click (car selected-item) (cadr selected-item) panel pos)
                ) self)
    (update-panel panel)))

(defmethod handle-doubleclick-analysis ((self list) panel pos) 
  (when (analysis-list-p self)  
    (mapcar #'(lambda (selected-item) 
                (handle-segment-doubleclick (car selected-item) (cadr selected-item) panel pos)
                ) self)
    (update-panel panel)))


(defmethod handle-add-click-analysis ((self scorepanel) where)
  (unless (analysis (object (editor self)))
    (setf (analysis (object (editor self)))
          (list (make-instance 'simple-segmentation))))
  (analysis-add-click (car (analysis (object (editor self)))) self where)
  (om-invalidate-view (title-bar (editor self)))
  (update-panel self))

;;;====================
;;; KEYBOARD

(defmethod analysis-handle-key-event ((self chordseqPanel) char)
  (case char
    (:om-key-tab (change-current-analysis self))
    (:om-key-esc (off-analysis-selection self) (update-panel self))
    (#\n (change-analysis-name self))
    (#\Space (play-in-analysis self))   
    (otherwise 
     (when (car (list! (analysis (object (editor self)))))
       (analysis-key-event (car (list! (analysis (object (editor self))))) self char)
       (om-invalidate-view (title-bar (editor self)))
       (update-panel self)))))


;;;====================
;;; MENUS
(defmethod analysis-menu-items ((editor scoreeditor))
  (let ((analysis (car (list! (analysis (object editor))))))
    (append 
     (when analysis 
      (list (om-new-leafmenu "Change Analysis Name"
                             #'(lambda () (change-analysis-name (panel editor))))
            (om-new-leafmenu "Run Segmentation"
                             #'(lambda () 
                                 (compute-segments analysis (object editor))
                                 (update-panel (panel editor))
                                 (om-invalidate-view (title-bar editor)))
                             nil (compute-segments-p analysis))
            (om-new-leafmenu "Analyse Segments"
                             #'(lambda () 
                                 (analyse-segments analysis (object editor))
                                 (update-panel (panel editor))
                                 (om-invalidate-view (title-bar editor)))
                             nil (analyse-segments-p analysis))
            (om-new-leafmenu "Analyse Selected Segment"
                                  #'(lambda () 
                                      (loop for seg in (selected-segments analysis) do
                                            (analyse-one-segment analysis seg (object editor)))
                                      (update-panel (panel editor))
                                      (om-invalidate-view (title-bar editor)))
                                  nil (if (selected-segments analysis) (analyse-segments-p analysis) nil))
            (om-new-leafmenu "Segmentation and Analyse"
                             #'(lambda () 
                                 (compute-and-analyse-segments analysis (object editor))
                                 (update-panel (panel editor))
                                 (om-invalidate-view (title-bar editor)))
                             nil (compute+analyse-segments-p analysis))
            (om-new-leafmenu "Reset Analysis"
                             #'(lambda () 
                                 (reset-object-analysis analysis)
                                 (update-panel (panel editor))
                                 (om-invalidate-view (title-bar editor))))
            
      (when (get-analysis-menu-items analysis)
              (list (om-make-menu (or (get-name analysis) (string (type-of analysis)))
                            (get-analysis-menu-items analysis editor))))
      ))
     
     (list (list (om-new-leafmenu "Delete Current Analysis"
                             #'(lambda () 
                                 (remove-object-analysis (object editor) analysis)
                                 (update-panel (panel editor))
                                 (om-invalidate-view (title-bar editor)))
                             nil analysis)
                 (om-new-leafmenu "Add New Analysis"
                             #'(lambda () 
                                 (let ((new-analysis (select-new-analysis (object editor))))
                                   (when new-analysis
                                     (add-object-analysis (object editor) (make-instance new-analysis))
                                     (update-panel (panel editor))
                                     (om-invalidate-view (title-bar editor))))))
           ))
     )))

(defmethod get-analysis-menu-items ((self abstract-analysis) &optional editor) nil)


;;;====================
;;; GLOBAL ACTIONS


(defmethod play-in-analysis ((self chordseqPanel))
  (if (Idle-p *general-player*)
  (let ((current (car (list! (analysis (object (editor self)))))))
    (if (and current (selected-segments current))
        (let* ((segments (selected-segments current))
               (interval (list (list-min (mapcar 'segment-begin segments)) 
                               (list-max (mapcar 'segment-end segments)))))
          (setf (cursor-interval self) interval)
          (play-selection-from-palette self)
          (setf (cursor-interval self) '(0 0)))
      (play-selection-from-palette self)))
    (stop-from-palette self)))
    

(defmethod change-current-analysis ((self chordseqPanel))
  (when (analysis (object (editor self)))
    (setf (analysis (object (editor self)))
          (append (cdr (list! (analysis (object (editor self)))))
                  (list (car (list! (analysis (object (editor self))))))))
    (update-panel self)
    (om-invalidate-view (title-bar (editor self)))))

(defmethod change-analysis-name ((self chordseqPanel))
  (let ((current (car (list! (analysis (object (editor self)))))))
  (if (analysis (object (editor self)))
      (let ((name (om-get-user-string "New analysis name:" :initial-string (or (get-name current) ""))))
        (when name
          (set-name current name)
          (om-invalidate-view (title-bar (editor self)))))
    (om-beep))))

(defclass select-class-item-list (oa::om-single-item-list) ())

(defmethod oa::double-click-on-list ((self select-class-item-list)) 
  (om-return-from-modal-dialog (om-view-window self) (om-get-selected-item self)))

(defun class-all-subclasses (class)
  (loop for sc in (hcl::class-direct-subclasses class) append 
        (cons sc (class-all-subclasses sc))))

(defun filter-valid-analysis (classnames object)
  (if object 
      (remove nil (loop for item in classnames when (compatible-analysis-p (make-instance item) object) collect item))
    classnames))

(defun select-new-analysis (&optional object)
  (let ((analysis-classes (filter-valid-analysis (mapcar 'class-name (class-all-subclasses (find-class 'abstract-analysis)))
                                                 object)))
    (if (null analysis-classes)
        (progn 
          (om-message-dialog (string+ "No segmentation or analysis available for " (string (type-of object))))
          nil)
    (let ((win (om-make-window 'om-dialog :size (om-make-point 250 240)))
          list)
      (om-add-subviews win
                       (om-make-dialog-item 'om-static-text (om-make-point 20 20)
                                            (om-make-point 200 40)
                                            "Select the type of analysis:"
                                            :font *om-default-font2b*)
                       (setf list (om-make-dialog-item 'select-class-item-list (om-make-point 20 60)
                                                        (om-make-point 200 100)
                                                        "" :range analysis-classes :scrollbars t))
                       (om-make-dialog-item 'om-button (om-make-point 60 180)
                                            (om-make-point 80 20)
                                            "Cancel"
                                            :di-action (om-dialog-item-act item 
                                                         (om-return-from-modal-dialog win nil)))
                       (om-make-dialog-item  'om-button (om-make-point 140 180)
                                             (om-make-point 80 20)
                                             "OK"
                                             :default-button t
                                             :di-action (om-dialog-item-act item 
                                                          (om-return-from-modal-dialog win 
                                                                                       (om-get-selected-item list)))))
      (om-modal-dialog win)))))

; TO DO: select only the segments to be drawn, corresponding to the time span of the clipview
   
                       

;(select-new-analysis)                   
;(defclass! test3 (random-tonal-analysis) ())
;(hcl::class-direct-subclasses (find-class 'abstract-analysis))
;(lw-tools::class-all-subclasses (find-class 'abstract-analysis))



