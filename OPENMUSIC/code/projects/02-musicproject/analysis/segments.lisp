;;;============================
;;; SEGMENT CLASSES FOR SCORE OBJECTS
;;;============================

(in-package :om)


;;;============================
;;; INTERVAL (T1=>T2)
;;;============================
(defclass! time-segment (segment)
  ((t1 :accessor t1 :initarg :t1 :initform 0 :documentation "start time (ms)")
   (t2 :accessor t2 :initarg :t2 :initform 0 :documentation "end time (ms)")))

(defmethod segment-begin ((self time-segment)) (t1 self))
(defmethod segment-end ((self time-segment)) (t2 self))

(defmethod draw-segment ((self time-segment) view) 
; TO DO: segments are not drawn in the correct place when the zoom is changed
  (let* ((p1 (time-to-pixels view (t1 self)))        
         (p2 (time-to-pixels view (t2 self)))
         (p2corr (max (1+ p1) (- p2 2))))
    (om-with-line-size (if (selected self) 2 1.5)
      (om-with-line :dash
      (om-with-fg-color view *om-steel-blue-color*
        (om-draw-line p1 0 p1 (h view))
        (om-draw-line p1 0 (+ p1 4) 0))
      ;(om-with-fg-color view *om-steel-blue-color*
      ;  (om-draw-line (+ p1 2) 0 (+ p1 2) (h view)))
      (om-with-fg-color view *om-red2-color*
        (om-draw-line p2corr 0 p2corr (h view))
        (om-draw-line p2corr 0 (- p2corr 4) 0)
        )))
    (when (selected self) (om-draw-hilite-rect p1 0 (- p2 p1) (h view)))
    ))

(defmethod segment-handle-add-click ((self time-segment) analysis panel pos)
  (let ((segment-release-selection #'(lambda (view pos)
                                       (release-interval-select view pos)
                                       (unless (= (car (cursor-interval view))
                                                  (second (cursor-interval view)))
                                         (setf (t2 self) (second (cursor-interval view)))))))
    (setf (t1 self) (pixels-to-time panel (om-point-h pos))
          (t2 self) (t1 self))
    (om-init-motion-functions panel 'interval-select-action 
                              segment-release-selection)
    (om-new-movable-object panel (om-point-h pos) 0 4 (h panel) 'om-selection-rectangle)
    self))



;;;============================
;;; MARKER (T)
;;;============================
(defclass! marker-segment (segment)
  ((mrk-time :accessor mrk-time :initarg :mrk-time :initform 0 :documentation "marker time (ms)"))
  (:default-initargs :color *om-red2-color*))

(defmethod segment-begin ((self marker-segment)) (mrk-time self))
(defmethod segment-end ((self marker-segment)) (mrk-time self))

(defmethod draw-segment ((self marker-segment) view) 
  (let* ((p1 (time-to-pixels view (mrk-time self))))
    (om-with-line-size (if (selected self) 3 1.5)
      (om-with-line :solid
      (om-with-fg-color view (or (color self) *om-red2-color*)
        (om-draw-line p1 0 p1 (h view)))
        ))))

(defmethod segment-clicked-p ((self marker-segment) panel pos) 
  (let* ((x (om-point-h pos))
         (p1 (time-to-pixels panel (segment-begin self))))
    (and (>= x (1- p1)) (<= x (1+ p1)))))

(defmethod segment-handle-add-click ((self marker-segment) analysis panel pos)
  (setf (mrk-time self) (pixels-to-time panel (om-point-h pos)))
  self)

(defmethod handle-segment-doubleclick ((self abstract-analysis) (segment marker-segment) panel pos)
  (let ((str (om-get-user-string "Enter new label for the segment"
                                 :initial-string (if (stringp (segment-data segment))
                                                     (segment-data segment)
                                                     (format nil "~A" (segment-data segment))))))
    (when str 
      (setf (segment-data segment) str)
      (update-panel panel))))

;;;============================
;;; GROUP OF NOTES SEGMENT (TO DO)
;;;============================        

(defclass! note-segment (segment)
  ((note-list :accessor note-list :initarg :note-list :initform nil :documentation "list of the note identifiers in this segment")))

;;;============================
;;; SET OF CHORDS SEGMENT
;;;============================        

(defclass! chord-segment (segment)
  ((chords :accessor chords :initform nil :documentation "list of the chords in this segment")
   (chord-ids :accessor chord-ids :initarg :chord-ids :initform nil :documentation "list of the chord numbers in this segment")))

(defmethod segment-init ((self chord-segment)) 
  (unless (chords self)
    (setf (chords self) 
          (loop for i in (chord-ids self) 
                collect (nth i (get-real-chords (analysis-object (container-analysis self)))))))
  (call-next-method))

;;; called when the object is modified
;;; may not work in voice tree modifications since chords-are re-built
(defmethod segment-update ((self chord-segment) object) 
  (setf (chords self) (remove-if-not 
                       #'(lambda (chord) (find chord (get-real-chords object) :test 'equal)) 
                       (chords self)))
  (setf (chord-ids self) (loop for c in (chords self) collect
                               (position c (get-real-chords object) :test 'equal)))
  (and (chords self) self)
  (call-next-method))
              

(defmethod segment-begin ((self chord-segment)) 
  (if (and (car (chords self)) (container-analysis self) (analysis-object (container-analysis self)))
      (offset->ms (car (chords self)) (analysis-object (container-analysis self)))
    (tb self)))

(defmethod segment-end ((self chord-segment)) 
  (if (and (last-elem (chords self)) (container-analysis self) (analysis-object (container-analysis self)))
      (+ (offset->ms (last-elem (chords self)) (analysis-object (container-analysis self)))
         (get-obj-dur (last-elem (chords self))))
    (te self)))

(defmethod segment-clicked-p ((self chord-segment) panel pos) 
  (when (and (segment-begin self) (segment-end self))
  (let* ((x (om-point-h pos)) (y (om-point-v pos))
         (p1 (time-to-pixels panel (segment-begin self) ))
         (p2 (time-to-pixels panel (segment-end self))))
    (and (>= x p1) (<= x p2) (>= y 10) (<= y (- (h panel) 180))))))


(defmethod draw-segment ((self chord-segment) view)
  (when (chord-ids self)
    (let ((t1 (time-to-pixels view (segment-begin self)))
          (t2 (time-to-pixels view (segment-end self))))
      (om-with-line-size 1.5
        (om-with-fg-color view (or (color self) *om-gray-color*)
          (om-draw-rect (- t1 2) 10 (- t2 t1 2) (- (h view) 180))
          (if (selected self) 
              (om-with-fg-color view (om-color-alpha (or (color self) *om-light-gray-color*) 0.2)
                (om-fill-rect (- t1 2) 10 (- t2 t1) (- (h view) 180))))
          )))))

(defmethod segment-handle-add-key ((self chord-segment) analysis panel) 
  (when (selection? panel)
    (let ((ordered-selection (sort (get-real-chords (selection? panel)) '< 
                                   :key #'(lambda (c) (offset->ms c (analysis-object analysis))))))
      (if ordered-selection
          (if (find ordered-selection (analysis-segments analysis)
                    :key 'chords :test 'equal)
              (om-beep-msg "This segment already exists !")
            (progn
              (setf (chords self) (copy-list ordered-selection)
                    (chord-ids self) (loop for c in ordered-selection 
                                           collect (position c (get-real-chords (analysis-object analysis))))
                (color self) (om-random-color))
          self))
        (om-beep)))))

;;;============================
;;; CHORD-MARKER
;;;============================        

(defclass! chord-marker (segment)
  ((chord :accessor chord :initform nil :documentation "the chord starting this segment")
   (chord-id :accessor chord-id :initarg :chord-id :initform nil :documentation "the chord index in its container"))
  (:default-initargs :color *om-steel-blue-color*))

(defmethod segment-init ((self chord-marker))
  (unless (chord self)
    (setf (chord self) 
          (nth (chord-id self) (get-real-chords (analysis-object (container-analysis self))))))
  (call-next-method))

;;; called when the object is modified
;;; may not work in voice tree modifications since chords-are re-built
(defmethod segment-update ((self chord-marker) object) 
  (setf (chord self) (find (chord self) (get-real-chords object) :test 'equal))
  (when (chord self) 
    (setf (chord-id self) 
          (position (chord self) (get-real-chords object) :test 'equal)))
  (and (chord self) self)
  (call-next-method))
              
(defmethod segment-begin ((self chord-marker)) 
  (if (and (container-analysis self) (analysis-object (container-analysis self)) (chord self))
      (offset->ms (chord self) (analysis-object (container-analysis self)))
    (tb self)))

(defmethod segment-end ((self chord-marker)) 
  (if (and (container-analysis self) (analysis-object (container-analysis self)))
      (if (next-segment self)
          (segment-begin (next-segment self))
        (get-obj-dur (analysis-object (container-analysis self))))
    (te self)))
  
(defmethod segment-clicked-p ((self chord-marker) panel pos) 
  (let* ((x (om-point-h pos))
         (p1 (time-to-pixels panel (segment-begin self)))
         (p2 (time-to-pixels panel (segment-end self))))
    ;(and (>= x (- p1 2)) (<= x (+ p1 2)))
    (and (>= x p1) (<= x p2))
    ))

(defmethod draw-segment ((self chord-marker) view)
  (when (chord-id self)
    (let ((t1 (time-to-pixels view (segment-begin self)))
          (t2 (time-to-pixels view (segment-end self))))
      (om-with-line-size (if (selected self) 3 1.5)
        (om-with-fg-color view (or (color self) *om-light-gray-color*)
          (om-draw-line (- t1 2) 10 (- t1 2) (- (h view) 40))
          ))
      (when (selected self) (om-draw-hilite-rect t1 0 (- t2 t1) (- (h view) 200))))))

(defmethod segment-handle-add-key ((self chord-marker) analysis panel) 
  (when (selection? panel)
    (let ((first-in-selection (car (sort (get-real-chords (selection? panel)) '< 
                                   :key #'(lambda (c) (offset->ms c (analysis-object analysis)))))))
      (if first-in-selection
      (if (find first-in-selection (analysis-segments analysis)
                :key 'chord :test 'equal)
          (om-beep-msg "This segment marker already exists !")
        (progn
          (setf (chord self) first-in-selection
                (chord-id self) (position first-in-selection (get-real-chords (analysis-object analysis))))
          self))
        (om-beep-msg "Select a CHORD in the score to create a chord-marlker!")))))

;;;============================
;;; MESURE
;;;============================        

(defclass! measure-segment (segment) 
   ((mesure :accessor mesure :initform nil)
    (mesure-id :accessor mesure-id :initarg :mesure-id :initform nil)))
            
(defmethod segment-begin ((self measure-segment)) 
  (if (and (mesure self) (container-analysis self) (analysis-object (container-analysis self)))
      (offset->ms (mesure self) (analysis-object (container-analysis self)))
    (tb self)))

(defmethod segment-end ((self measure-segment)) 
  (if (and (mesure self) (container-analysis self) (analysis-object (container-analysis self)))
      (+ (offset->ms (mesure self) (analysis-object (container-analysis self))) 
         (get-obj-dur (mesure self)))
    (te self)))

(defmethod segment-init ((self measure-segment))
  (unless (mesure self)
    (when (mesure-id self)
      (setf (mesure self) 
            (nth (mesure-id self) (inside (analysis-object (container-analysis self)))))))
  (call-next-method))

(defmethod segment-update ((self measure-segment) object) 
  (setf (mesure-id self) (position (mesure self) (inside object) :test 'equal))
  (and (mesure-id self) self)
  (call-next-method))

