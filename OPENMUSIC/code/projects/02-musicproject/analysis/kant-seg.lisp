;;;===========================
;;; ANALYSIS CLASS
;;;===========================

(in-package :om)

;;;============================
;;; KANT
;;; Segments = note marker, untel next one
;;; Segment-data = a VOICE
(defclass! KANT-seg (ABSTRACT-ANALYSIS) ())

(defmethod compatible-analysis-p ((analyse KANT-seg) (object chord-seq)) t)
(defmethod compatible-analysis-p ((analyse KANT-seg) (object t)) nil)

(defclass! kant-data ()
  ((tempo :accessor tempo :initarg :tempo :initform 60)
   (signature :accessor signature :initarg :signature :initform '(4 4))
   (maxdiv :accessor maxdiv :initarg :maxdiv :initform 8)
   (forbid :accessor forbid :initarg :forbid :initform nil)
   (offset :accessor offset :initarg :offset :initform 0)
   (precision :accessor precision :initarg :precision :initform 0.5)
   (voice :accessor voice :initarg :voice :initform nil)
   (updateflag :accessor updateflag :initform nil)))

(defmethod default-segment-class ((self KANT-seg)) 'chord-marker)

(defmethod compute-segments-p ((self KANT-seg)) nil)
(defmethod analyse-segments-p ((self KANT-seg)) t)
(defmethod compute+analyse-segments-p ((self KANT-seg)) nil)

(defmethod analysis-init ((self KANT-seg) object)
  (unless (analysis-segments self)
    (setf (analysis-segments self)
          (list (make-instance 'chord-marker :chord-id 0))))
  (call-next-method))

(defmethod analysis-init-segment ((analyse KANT-seg) segment)
  (unless (segment-data segment) 
    (setf (segment-data segment) (make-instance 'kant-data)))
  (when (previous-segment segment)
    (setf (updateflag (segment-data (previous-segment segment))) nil)))
  
(defmethod delete-from-analysis ((self KANT-seg) segment)
  (when (previous-segment segment)
    (setf (updateflag (segment-data (previous-segment segment))) nil))
  (call-next-method))


(defmethod analyse-one-segment ((self KANT-seg) (seg segment) (object t))
  (let* ((tmpcseq (select object (segment-begin seg) (min (segment-end seg) (get-obj-dur object))))
         (durs (x->dx (lonset tmpcseq)))
         (kant-data (or (segment-data seg) 
                        (setf (segment-data seg) (make-instance 'kant-data)))))
    (setf (voice kant-data) (make-instance 'voice 
                                            :tree (omquantify durs (tempo kant-data) (signature kant-data)  
                                                              (maxdiv kant-data) (forbid kant-data)
                                                              (offset kant-data) (precision kant-data))
                                            :chords (get-chords tmpcseq)
                                            :tempo (tempo kant-data)))
    (setf (updateflag kant-data) t)))

(defmethod handle-segment-doubleclick ((self KANT-seg) segment panel pos) 
  (kant-data-window  (or (segment-data segment) 
                         (setf (segment-data segment) (make-instance 'kant-data))))
  (update-panel panel))


(defmethod draw-segment-data ((self KANT-seg) segment view) 
  (let ((x1 (time-to-pixels view (segment-begin segment))))
    (when (segment-data segment)
      (om-with-font *om-default-font1*
                    (om-draw-string x1 (- (h view) 150) (format nil "KANT PARAMS:"))
                    (om-draw-string x1 (- (h view) 140) (format nil "Tempo: ~A" (tempo (segment-data segment))))
                    (om-draw-string x1 (- (h view) 130) (format nil "Measure: ~A" (signature (segment-data segment))))
                    (om-draw-string x1 (- (h view) 120) (format nil "Max. div.: ~A" (maxdiv (segment-data segment))))
                    (om-draw-string x1 (- (h view) 110) (format nil "Forbid. div.: ~A" (forbid (segment-data segment))))
                    (om-draw-string x1 (- (h view) 100) (format nil "Offset: ~A" (offset (segment-data segment))))
                    (om-draw-string x1 (- (h view) 90) (format nil "Precision: ~A" (precision (segment-data segment))))))
    
    (om-with-fg-color view (if (updateflag (segment-data segment)) *om-black-color* *om-gray-color*)
      (om-with-font *om-default-font1b*
		    (om-draw-string x1 (- (h view) (if (oddp (position segment (analysis-segments self))) 60 40))
				    (segment-data-tostring self segment))))))

(defmethod segment-data-tostring ((self KANT-seg) segment) 
  (if (and (segment-data segment) (voice (segment-data segment)))
      (format nil "~A" (tree (voice (segment-data segment))))
      ""))

(defmethod get-kant-voices ((self KANT-seg))
  (loop for seg in (analysis-segments self) collect
        (voice (segment-data seg))))

(defmethod! concatenate-kant-voices ((self chord-seq))
   :icon 252
   (let* ((kant-analyses (remove nil (loop for an in (analysis self) 
                                          when (equal (type-of an) 'kant-seg)
                                          collect an)))
          (kant-voices (mapcar 
                        #'(lambda (kant) (reduce 'concat (get-kant-voices kant)))
                        kant-analyses)))
     (if (<= (length kant-voices) 1) (car kant-voices)
         kant-voices)))
   
                          
(defmethod kant-data-window ((kdata kant-data))
  (let ((win (om-make-window 'om-dialog :position :centered 
                             :size (om-make-point 430 200)))
        (pane (om-make-view 'om-view
                            :size (om-make-point 400 180)
                            :position (om-make-point 10 10)
                            :bg-color *om-white-color*))
        (i 0)
        tempotxt forbidtxt mesuretxt precistxt maxdivtxt offsettxt)
    
    (om-add-subviews pane
                     (om-make-dialog-item 'om-static-text (om-make-point 20 (incf i 16))
                                          (om-make-point 380 40)
                                          "Set quantification parameters for selected segment:"
                                          :font *om-default-font2b*)
     
                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 30)) (om-make-point 120 20) "Tempi"
                                          :font *om-default-font1*)
                     (setf tempotxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i)  (om-make-point 37 13)
                                                         (format nil "~D" (tempo kdata)) 
                                                         :font *om-default-font1*))

                     (om-make-dialog-item 'om-static-text  (om-make-point 230 i) (om-make-point 120 20) "Forbidden Div."
                                          :font *om-default-font1*)
                     (setf forbidtxt (om-make-dialog-item 'om-editable-text (om-make-point 330 i) (om-make-point 37 13)
                                                          (format nil "~D" (forbid kdata)) 
                                                          :font *om-default-font1*))

                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "Measure"
                                          :font *om-default-font1*)
                     (setf mesuretxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i)  (om-make-point 37 13)
                                                          (format nil "~D" (signature kdata))
                                                          :font *om-default-font1*))
                         
                     (om-make-dialog-item 'om-static-text  (om-make-point 210 i) (om-make-point 120 20) "Precision (0.0-1.0)"
                                          :font *om-default-font1*)
                     (setf precistxt (om-make-dialog-item 'om-editable-text (om-make-point 330 i) (om-make-point 37 13)
                                                          (format nil "~D" (precision kdata)) 
                                                          :font *om-default-font1*))

                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "Max. Division"
                                          :font *om-default-font1*)
                     (setf maxdivtxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i) (om-make-point 37 13)
                                                          (format nil "~D" (maxdiv kdata)) 
                                                          :font *om-default-font1*))

                     (om-make-dialog-item 'om-static-text  (om-make-point 230 i) (om-make-point 120 20) "Offset"
                                          :font *om-default-font1*)
                     (setf offsettxt (om-make-dialog-item 'om-editable-text (om-make-point 330 i) (om-make-point 37 13)
                                                          (format nil "~D" (offset kdata)) 
                                                          :font *om-default-font1*))
                         
                     (om-make-dialog-item 'om-button (om-make-point 200 (incf i 35))
                                          (om-make-point 80 20)
                                          "Cancel"
                                          :di-action (om-dialog-item-act item 
                                                       (om-return-from-modal-dialog win nil)))
                     
                     (om-make-dialog-item  'om-button (om-make-point 300 i)
                                           (om-make-point 80 20)
                                           "OK"
                                           :di-action (om-dialog-item-act item 
                                                        (let ((tempo (ignore-errors (read-from-string (om-dialog-item-text tempotxt))))
                                                              (mesure (ignore-errors (read-from-string (om-dialog-item-text mesuretxt))))
                                                              (maxdiv (ignore-errors (read-from-string (om-dialog-item-text maxdivtxt))))
                                                              (forbid (ignore-errors (read-from-string (om-dialog-item-text forbidtxt))))
                                                              (precis (ignore-errors (read-from-string (om-dialog-item-text precistxt))))
                                                              (offset (ignore-errors (read-from-string (om-dialog-item-text offsettxt)))))
                                                          (setf (tempo kdata) tempo
                                                                (signature kdata) mesure
                                                                (maxdiv kdata) maxdiv
                                                                (precision kdata) precis
                                                                (offset kdata) offset
                                                                (forbid kdata) forbid)
                                                          (setf (updateflag kdata) nil)
                                                          (om-return-from-modal-dialog win t))))
                     )
  
    (om-add-subviews win pane)
    (om-modal-dialog win)
    ))



;;;===========
;;; version quantification "externe"

(defun auto-mark (chord-seq beat-times)
  (let ((an (make-instance 'kant-seg))
        (cs (clone chord-seq)))
    (setf (analysis cs) (list an))
    (analysis-init an cs)
    (setf (analysis-segments an) nil)
    (loop for c in (inside cs)
          for o in (lonset cs)
          for i = 0 then (1+ i)
          when (find o beat-times :test '=)
          do (add-in-analysis an 
                              (make-instance 'chord-marker 
                                             :chord c :chord-id i)))
    cs))
 

(defun quantify-segments (cs tempo maxdiv forbidden precision)
  (let* ((kant-analysis (car (remove nil (loop for an in (analysis cs) 
                                               when (equal (type-of an) 'kant-seg)
                                               collect an)))))
    (if kant-analysis
        (let ((kant-voices 
               (loop for seg in (analysis-segments kant-analysis) 
                     for i = 1 then (+ i 1) collect
                     (progn 
                       (print (format nil "SEGMENT ~D: ~Dms - ~Dms" i (segment-begin seg) (segment-end seg)))
                       (or (voice (segment-data seg))
                           (quantify-segment cs (segment-begin seg) (segment-end seg) 
                                             tempo maxdiv forbidden precision))))))
          (unless (= 0 (segment-begin (car (analysis-segments kant-analysis))))
            (print (format nil "SEGMENT 0: 0ms - ~Dms" (segment-begin (car (analysis-segments kant-analysis)))))
            (setf kant-voices 
                  (cons (quantify-segment cs 0 (segment-begin (car (analysis-segments kant-analysis)))
                                          tempo maxdiv forbidden precision)
                        kant-voices)))
          ;(reduce 'concat kant-voices)
          kant-voices
          )
      (om-beep-msg "NO KANT-SEG IN CHORD-SEQ"))))

(defun quantify-segment (cs t1 t2 tempo maxdiv forbidden precision)
  (let* ((tmpcseq (select cs t1 (min t2 (get-obj-dur cs))))
         (durs (x->dx (lonset tmpcseq)))
         (chords-and-beats (loop for d in durs for c in (get-chords tmpcseq)
                                 collect (if (get-extras c 'text)
                                             (list c d) (list nil (- d))))))
    (when (and (zerop t1) (get-chords tmpcseq))
      (setf chords-and-beats 
            (cons (list nil (- (car (lonset tmpcseq)))) chords-and-beats)))
    (make-instance 'voice 
                   :tree (omquantify (mapcar 'cadr chords-and-beats)
                                            tempo '(4 4)  
                                            (or maxdiv 8) forbidden
                                            0 (or precision 0.0))
                   :chords (remove nil (mapcar 'car chords-and-beats))
                   :tempo tempo)))
;;;===========


        


                   

