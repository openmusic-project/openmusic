;;;===========================
;;; ANALYSIS CLASS
;;;===========================

(in-package :om)

;;;============================
;;; KBEST
;;; Segments = time marker, until next one
;;; Segment-data = a VOICE
(defclass! KBEST-seg (ABSTRACT-ANALYSIS) ())

(defmethod compatible-analysis-p ((analyse KBEST-seg) (object chord-seq)) t)
(defmethod compatible-analysis-p ((analyse KBEST-seg) (object t)) nil)



(defclass! kbest-data ()
  ((tempo :accessor tempo :initarg :tempo :initform 60)
   (signature :accessor signature :initarg :signature :initform '(4 4))
   (schema :accessor schema :initarg :maxdiv :initform '(( ((2 3) (2 3) 2)   (5 (2 3) 2)   ((7 11 13)) )) )
   (offset :accessor offset :initarg :offset :initform 0)
   (precision :accessor precision :initarg :precision :initform 0.5)
   (voice :accessor voice :initarg :voice :initform nil)
   (updateflag :accessor updateflag :initform nil)))

(defmethod default-segment-class ((self KBEST-seg)) 'marker-segment)

(defmethod compute-segments-p ((self KBEST-seg)) t)
(defmethod analyse-segments-p ((self KBEST-seg)) t)
(defmethod compute+analyse-segments-p ((self KBEST-seg)) t)

(defmethod analysis-init ((self KBEST-seg) object)
  (unless (analysis-segments self)
    (setf (analysis-segments self)
          (list (make-instance 'marker-segment :mrk-time 0))))
  (call-next-method))

(defmethod analysis-init-segment ((analyse KBEST-seg) segment)
  (unless (segment-data segment)

    (let* ((chords (analysis-object analyse))
           (onsets (lonset chords)))

    (labels ((to-tempo (gcd)
                   (let ((tempo (/ 60000 gcd)))
                     (loop while (> tempo 300)
                           do (setq tempo (/ tempo 2)))
                     tempo)))
      (let* ((end (segment-end segment))
             (on-seg (loop for x in onsets when (and (>= x  (segment-begin segment))  (<= x (segment-end segment))) collect x))
             (dur-seg  (x->dx on-seg))
             (gcd (if (null dur-seg) 1000 (agcd dur-seg 0.1))))

        (setf (segment-data segment) (make-instance 'kbest-data :tempo (to-tempo gcd)))))))

  (when (previous-segment segment)
    (setf (updateflag (segment-data (previous-segment segment))) nil)))
  
(defmethod delete-from-analysis ((self KBEST-seg) segment)
  (when (previous-segment segment)
    (setf (updateflag (segment-data (previous-segment segment))) nil))
  (call-next-method))



(defmethod agcd (vals tolerance)

  (labels 
      ((grid-above (val) (* val (1+ tolerance)))
       (grid-below (val) (- val (* val tolerance)))
       (gcd-try (vals gcd-min gcd-max)
         (if (null vals)
             (/ (+ gcd-min gcd-max) 2.0)
           (let* ((val-below (grid-below (first vals)))
                  (val-above (grid-above (first vals)))
                  (quo-min (ceiling val-below gcd-max))
                  (quo-max (floor val-above gcd-min)))
             (loop for quotient from quo-min upto quo-max
                   for gcd-interval =
                   (gcd-try (rest vals) 
                            (max gcd-min (/ val-below quotient))
                            (min gcd-max (/ val-above quotient)))
                   when gcd-interval do (return-from gcd-try gcd-interval))))))
    (gcd-try vals .1 (grid-above (first vals)))))



;(agcd '(510 275 252 1100) 0.1)
  

;; Pour créer une segmentation à partir de rien 

(defmethod compute-analysis-segments ((self KBEST-seg) (object chord-seq)) 
  (if (or (not (analysis-segments self))
          (om-y-or-n-dialog "This operation will delete the current segmentation. Continue?"))
      (let* ((chords (get-real-chords object))
             (lonsets (lonset object))
             (ldurs (x->dx lonsets))
             (nb-chords (length lonsets))
             (marks nil))
        ;onsets contient les onsets à l'intérieur du segment
        (labels ((estim-tempo ( dur-seg durs on-seg onsets &optional (error 0) (lpgcd nil))
                   (if (null durs)
                       (let* ((gcd (if (null lpgcd) (agcd dur-seg 0.15) (average lpgcd nil))))
                         (setq marks (append marks (list (make-instance 'marker-segment
                                                                        :mrk-time (first on-seg) 
                                                                        :color (om-random-color)
                                                                        :segment-data (make-instance 'kbest-data :tempo (to-tempo gcd)))
                                                         (make-instance 'marker-segment
                                                                        :mrk-time (first onsets) 
                                                                        :color (om-random-color)
                                                                        :segment-data (make-instance 'kbest-data :tempo (to-tempo gcd)))))))
                   (let* ((new-pgcd (agcd dur-seg 0.15)))
                     (if (null lpgcd)
                         ;si début du segment
                         (estim-tempo (append dur-seg (list (first durs))) (rest durs) (append on-seg (list (first onsets))) (rest onsets) (compute-error on-seg new-pgcd) (list new-pgcd))
                       (let ((old-pgcd (average lpgcd nil)))
                         (if (> (/ (abs (- new-pgcd old-pgcd)) old-pgcd) 0.1)
                           ;on découpe
                             (progn 
                               (setq marks (append marks (list (make-instance 'marker-segment
                                                                  :mrk-time (first on-seg)
                                                                  :color (om-random-color)
                                                                  :segment-data (make-instance 'kbest-data :tempo (to-tempo old-pgcd))))))
                               (estim-tempo (list (car (last dur-seg)) (first durs)) (cdr durs) (list (car (last on-seg)) (first onsets)) (cdr onsets)))
                        
 
                         ;on continue
                           (estim-tempo (append dur-seg (list (first durs))) (rest durs) (append on-seg (list (first onsets))) (rest onsets) (compute-error on-seg new-pgcd) (append lpgcd (list new-pgcd)))))))))
                


                 (compute-error (dur-seg gcd &optional (error 0))
                   (if (null dur-seg)
                       error
                     (compute-error (rest dur-seg) gcd (+ error (abs (nth-value 1 (round (first dur-seg) gcd)))))))
                 (to-tempo (gcd)
                   (let ((tempo (/ 60000 gcd)))
                     (loop while (> tempo 300)
                           do (setq tempo (/ tempo 2)))
                     tempo)))

          

          
          (estim-tempo (subseq ldurs 0 2) (cddr ldurs) (subseq lonsets 0 2) (cddr lonsets))
                     
                             
                             ))))
          

(defmethod compute-analysis-segments-test (entry) 

      (let* (
             (lonsets entry)
             (ldurs (x->dx lonsets))
             (nb-chords (length lonsets))
             (marks nil))
        ;onsets contient les onsets à l'intérieur du segment
        (labels ((estim-tempo ( dur-seg durs on-seg onsets &optional (error 0) (lpgcd nil))
                   (if (null durs)
                       (let* ((gcd (if (null lpgcd) (agcd dur-seg 0.15) (average lpgcd nil))))
                         (setq marks (append marks (list (make-instance 'marker-segment
                                                                        :mrk-time (first on-seg) 
                                                                        :color (om-random-color)
                                                                        :segment-data (make-instance 'kbest-data :tempo (to-tempo gcd)))
                                                         (make-instance 'marker-segment
                                                                        :mrk-time (first onsets) 
                                                                        :color (om-random-color)
                                                                        :segment-data (make-instance 'kbest-data :tempo (to-tempo gcd)))))))
                   (let* ((new-pgcd (agcd dur-seg 0.15)))
                     (if (null lpgcd)
                         ;si début du segment
                         (estim-tempo (append dur-seg (list (first durs))) (rest durs) (append on-seg (list (first onsets))) (rest onsets) (compute-error on-seg new-pgcd) (list new-pgcd))
                       (let ((old-pgcd (average lpgcd nil)))
                         (if (> (/ (abs (- new-pgcd old-pgcd)) old-pgcd) 0.1)
                           ;on découpe
                             (progn 
                               (setq marks (append marks (list (make-instance 'marker-segment
                                                                  :mrk-time (first on-seg)
                                                                  :color (om-random-color)
                                                                  :segment-data (make-instance 'kbest-data :tempo (to-tempo old-pgcd))))))
                               (estim-tempo (list (car (last dur-seg)) (first durs)) (cdr durs) (list (car (last on-seg)) (first onsets)) (cdr onsets)))
                        
 
                         ;on continue
                           (estim-tempo (append dur-seg (list (first durs))) (rest durs) (append on-seg (list (first onsets))) (rest onsets) (compute-error on-seg new-pgcd) (append lpgcd (list new-pgcd)))))))))
                


                 (compute-error (dur-seg gcd &optional (error 0))
                   (if (null dur-seg)
                       error
                     (compute-error (rest dur-seg) gcd (+ error (abs (nth-value 1 (round (first dur-seg) gcd)))))))
                 (to-tempo (gcd)
                   (let ((tempo (/ 60000 gcd)))
                     (loop while (> tempo 300)
                           do (setq tempo (/ tempo 2)))
                     tempo)))

          

          
          (estim-tempo (subseq ldurs 0 2) (cddr ldurs) (subseq lonsets 0 2) (cddr lonsets))
                     
                             
                             )))


(compute-analysis-segments-test '(0 110 230 500 600 700 820))
(compute-analysis-segments-test '(1619 2207 2430 2670 2875 3134 3628 4129 4614 5043 5200 5466 5693 5926 6182 6695 7195 7616 7753 8222 8447 8714 8942 9213 9700 9950 10195 10413 10672 11162 11393 11655 11876 12095 12212 12652 12888 13121 13364 13648 14148 14372 14599 14830 15097 15571 16072 16577 16976 17216 17446 17700 17942 18216 18697 18949 19203 19426 19605 19747 20202 20433 20686 20920 21180 21645 21885 22129 22368 22646 23108 23359 23609 23858 24169 25244 25976))



;        (loop for i from 0 to nb-chords by 4 collect
;              (make-instance 'marker-segment
;                        :mrk-time (nth  i lonsets)
;                        :color (om-random-color)
;                        :segment-data (make-instance 'kbest-data :tempo 200)
;                        ))


                        
;         ))))






                   
                


(defmethod analyse-one-segment ((self KBEST-seg) (seg segment) (object t))
  (let* ((tmpcseq (select object (segment-begin seg) (min (segment-end seg) (get-obj-dur object))))
         (durs (x->dx (lonset tmpcseq)))
         (kbest-data (or (segment-data seg) 
                        (setf (segment-data seg) (make-instance 'kbest-data)))))
    (setf (voice kbest-data) (make-instance 'voice 
                                            :tree (omquantify durs (tempo kbest-data) (signature kbest-data)  
                                                              (maxdiv kbest-data) (forbid kbest-data)
                                                              (offset kbest-data) (precision kbest-data))
                                            :chords (get-chords tmpcseq)
                                            :tempo (tempo kbest-data)))
    (setf (updateflag kbest-data) t)))

(defmethod handle-segment-doubleclick ((self KBEST-seg) segment panel pos) 
  (kbest-data-window  (or (segment-data segment) 
                         (setf (segment-data segment) (make-instance 'kbest-data))))
  (update-panel panel))


(defmethod draw-segment-data ((self KBEST-seg) segment view) 
  (let ((x1 (time-to-pixels view (segment-begin segment)))
        (x2 (time-to-pixels view (segment-end segment))))
    (when (segment-data segment)
      (om-with-fg-color view
          (color segment) 
      
        (om-with-font *om-default-font1*
                    
                      (om-draw-string x1 (- (h view) 170) (format nil "t=~D" (segment-begin segment)))
                    
                      (om-draw-string x1 (- (h view) 150) (format nil "KANT PARAMS:"))
                      (om-draw-string x1 (- (h view) 140) (format nil "Tempo: ~A" (tempo (segment-data segment))))
                      (om-draw-string x1 (- (h view) 130) (format nil "Measure: ~A" (signature (segment-data segment))))
                      (om-draw-string x1 (- (h view) 120) (format nil "Schema : ~A" (schema (segment-data segment))))
                      (om-draw-string x1 (- (h view) 110) (format nil "Offset: ~A" (offset (segment-data segment))))
                      (om-draw-string x1 (- (h view) 100) (format nil "Precision: ~A" (precision (segment-data segment))))
    
                      ;;; grille
                      (let* ((seg-dur (- (segment-end segment) (segment-begin segment)))
                             (temps (* (/ 60 (tempo (segment-data segment))) 1000))
                             (nb-temps (ceiling seg-dur temps)))
                        
                        (om-draw-line x1 (- (h view) 190) x2 (- (h view) 190))
                        (loop for i from 1 to nb-temps
                              for time = (segment-begin segment) then (+ time temps) do
                              (om-draw-string (time-to-pixels view time) (- (h view) 190) (format nil "~A" i))
                              (om-with-dashline '(2 2) 
                                (om-draw-line (time-to-pixels view time) (- (h view) 190) 
                                              (time-to-pixels view time) 0))))
                      
                      )
    (om-with-fg-color view (if (updateflag (segment-data segment)) *om-black-color* *om-gray-color*)
      (om-with-font *om-default-font1b*
		    (om-draw-string x1 (- (h view) (if (oddp (position segment (analysis-segments self))) 60 40))
				    (segment-data-tostring self segment))))))))

(defmethod segment-data-tostring ((self KBEST-seg) segment) 
  (if (and (segment-data segment) (voice (segment-data segment)))
      (format nil "~A" (tree (voice (segment-data segment))))
      ""))


(defmethod analysis-key-event ((self KBEST-seg) panel char)
  (case char
    (:om-key-up (selected-segment-change-tempo self 1)) 
    (:om-key-down (selected-segment-change-tempo self -1))
    (:om-key-left (selected-segment-change-mrk-time self -5))
    (:om-key-right (selected-segment-change-mrk-time self 5))
    (otherwise (call-next-method))))


;;;==========================

(defmethod selected-segment-change-tempo ((self KBEST-seg) delta)
  (when (selected-segments self)
    (loop for seg in (selected-segments self) do 
          (setf (tempo (segment-data seg)) (+ (tempo (segment-data seg)) delta)))))

(defmethod selected-segment-change-mrk-time ((self KBEST-seg) delta)
  (when (selected-segments self)
    (loop for seg in (selected-segments self) do 
          (setf (mrk-time  seg) (+ (mrk-time  seg) delta)))))


(defmethod get-kant-voices ((self KBEST-seg))
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
   
                          
(defmethod kbest-data-window ((kdata kbest-data))
  (let ((win (om-make-window 'om-dialog :position :centered 
                             :size (om-make-point 430 200)))
        (pane (om-make-view 'om-view
                            :size (om-make-point 400 180)
                            :position (om-make-point 10 10)
                            :bg-color *om-white-color*))
        (i 0)
        tempotxt schematxt mesuretxt precistxt maxdivtxt offsettxt)
    
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

                     (om-make-dialog-item 'om-static-text  (om-make-point 210 i) (om-make-point 120 20) "Schema"
                                          :font *om-default-font1*)
                     (setf schematxt (om-make-dialog-item 'om-editable-text (om-make-point 270 i) (om-make-point 97 13)
                                                          (format nil "~D" (schema kdata)) 
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

;                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "Max. Division"
;                                          :font *om-default-font1*)
;                     (setf maxdivtxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i) (om-make-point 37 13)
;                                                          (format nil "~D" (maxdiv kdata)) 
;                                                          :font *om-default-font1*))

                     (om-make-dialog-item 'om-static-text  (om-make-point 50 (incf i 26)) (om-make-point 120 20) "Offset"
                                          :font *om-default-font1*)
                     (setf offsettxt (om-make-dialog-item 'om-editable-text (om-make-point 140 i) (om-make-point 37 13)
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
                                                             ; (maxdiv (ignore-errors (read-from-string (om-dialog-item-text maxdivtxt))))
                                                              (schema (ignore-errors (read-from-string (om-dialog-item-text schematxt))))
                                                              (precis (ignore-errors (read-from-string (om-dialog-item-text precistxt))))
                                                              (offset (ignore-errors (read-from-string (om-dialog-item-text offsettxt)))))
                                                          (setf (tempo kdata) tempo
                                                                (signature kdata) mesure
                                                           ;     (maxdiv kdata) maxdiv
                                                                (precision kdata) precis
                                                                (offset kdata) offset
                                                                (schema kdata) schema)
                                                          (setf (updateflag kdata) nil)
                                                          (om-return-from-modal-dialog win t))))
                     )
  
    (om-add-subviews win pane)
    (om-modal-dialog win)
    ))












                   

