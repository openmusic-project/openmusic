(in-package :om)

;;;============================
;;; PITCH CLASS SET ANALYSIS
;;;============================

(defclass! PCSET-ANALYSIS (ABSTRACT-ANALYSIS) ())

(defmethod default-segment-class ((self pcset-analysis)) 'chord-segment)

(defmethod analysis-add-click ((self pcset-analysis) panel pos) 
  (om-beep-msg "Use SELECTION + 'S' to create new segments in this analysis"))

(defmethod compute-segments-p ((self pcset-analysis)) t)
(defmethod analyse-segments-p ((self pcset-analysis)) t)
(defmethod compute+analyse-segments-p ((self pcset-analysis)) t)

(defmethod compute-analysis-segments ((self pcset-analysis) (object t)) 
  (if (or (not (analysis-segments self))
          (om-y-or-n-dialog "This operation will delete the current segmentation. Continue?"))
      (loop for c in (get-real-chords object)
            for i = 0 then (+ i 1) collect
            (make-instance 'chord-segment
                       :chord-ids (list i)
                       :color (om-random-color)
                       ))
    ))
      
(defmethod analyse-one-segment ((self pcset-analysis) (seg segment) (object t))
  (setf (segment-data seg) 
        (chord2c (make-instance 'chord 
                                :lmidic (apply 'append (mapcar 'lmidic (chords seg))))
                 2)))

(defmethod segment-data-tostring ((self pcset-analysis) segment)
  (if (segment-data segment)
      (format nil "~A" (car (puntos (segment-data segment))))
    ""))

(defmethod draw-segment-data ((self pcset-analysis) segment view)
  (let* ((x1 (time-to-pixels view (segment-begin segment)))
         (x2 (time-to-pixels view (segment-end segment)))
         (mid (round (+ x1 x2) 2))
         (cr 40)) 
  (om-with-font *om-default-font1*
       (when (segment-data segment)
               (draw-cercle (segment-data segment) view 
                            mid (- (h view) 120) 
                            cr
                            2 3 t 0))
       (om-draw-string (- mid (round (om-string-size (segment-data-tostring self segment) *om-default-font1*) 2))
                       (- (h view) 60)
                       (segment-data-tostring self segment))
       ;(om-draw-string mid (- (h view) 60) (number-to-string (length (chords segment))))
       )))


