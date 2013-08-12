;;; ANALYSIS / SEGMENTATIONA AS STANDALONE OBJECTS
;;; INTERFACE IN SHEET EDITOR

(in-package :om)

;;;=======================
;;; OMSHEET
;;;=======================

(defmethod allowed-in-sheet ((self abstract-analysis)) t)

(defmethod draw-track-event ((self abstract-analysis) sheet-object view sheetpanel)
    (let* ((boxw (w view))
           (boxh (h view))
           (begtime (start-t sheet-object))
           (begpos (get-x-pos sheetpanel begtime 1)))
      (loop for seg in (analysis-segments self) do
            (let* ((x1 (- (get-x-pos sheetpanel (+ (segment-begin seg) begtime) 1) begpos))
                   (x2 (- (get-x-pos sheetpanel (+ (segment-end seg) begtime) 1) begpos))
                   (index (position seg (analysis-segments self))))
              (draw-segment-data self seg view)
              (om-with-line-size 1.5
                (om-with-line :dash
                  (om-with-fg-color view *om-steel-blue-color*
                    (om-draw-line x1 0 x1 boxh))
                  ))))))

(defmethod time-to-pixels ((view sheet-objectframe) time-ms)
  (let* ((sheet-panel (panel (score-view (sheet-editor view))))
         (begtime (start-t (reference view)))
         (begpos (get-x-pos sheet-panel begtime 1)))
    (- (get-x-pos sheet-panel (+ time-ms begtime) 1) begpos)
    ))
