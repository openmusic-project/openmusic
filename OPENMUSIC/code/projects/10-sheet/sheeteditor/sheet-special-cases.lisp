(in-package :om)

;Protocole and special objects in a sheet


;============================================================================
(defmethod draw-track-event ((self t) sheet-object view sheetpanel)
  (draw-mini-view view self))

;====================
; BPF
;====================

(defmethod get-obj-dur ((self bpf)) (last-elem (x-points self)))
(defmethod offset->ms ((self bpf) &optional grandparent) 0)
(defmethod extent ((self bpf)) (get-obj-dur self))

(defmethod draw-track-event ((bpf bpf) sheet-object view sheetpanel)
  (om-with-fg-color view (bpfcolor bpf)
    (let* ((ranges (give-bpf-range bpf))
           (rangey (abs (- (fourth ranges) (third ranges))))
           (y0 (min (fourth ranges) (third ranges)))
           (boxw (w view))
           (boxh (h view))
           (npoints (length (point-list bpf)))
           (begtime (start-t sheet-object))
           (begpos (get-x-pos sheetpanel begtime 1)))
      (loop for point-list on (point-pairs bpf) 
            while (cdr point-list) do
            (let* ((p1 (car point-list))
                   (p2 (cadr point-list))
                   (x1 (- (get-x-pos sheetpanel (+ (car p1) begtime) 1) begpos))
                   (y1 (- boxh (round (* (- (cadr p1) y0) (- boxh 6)) rangey) 3))
                   (x2 (- (get-x-pos sheetpanel (+ (car p2) begtime) 1) begpos))
                   (y2 (- boxh (round (* (- (cadr p2) y0) (- boxh 6)) rangey) 3)))
              (om-draw-line x1 y1 x2 y2))))))


;====================
; SOUND
;====================

(defun soundms2pix (time-ms pict-size sound-dur)
  (round (* time-ms pict-size) sound-dur))

(defmethod draw-track-event ((self sound) sheet-object view sheetpanel)
    (let* ((bpftime (timebpf sheetpanel))
           (dur (sound-dur-ms self))
           (boxw (w view))
           (boxh (h view))
           (picture (sound-get-pict self))
           (pictw (om-pict-width picture))
           (picth (om-pict-height picture))
           (begtime (start-t sheet-object))
           (begpos (get-x-pos sheetpanel begtime 1))
           (screen-t1 (get-ms-pos sheetpanel (om-h-scroll-position sheetpanel) 1))
           (screen-t2 (get-ms-pos sheetpanel (+ (om-h-scroll-position sheetpanel) (w sheetpanel)) 1)))
      (om-with-focused-view view
        (let* ((markerlist (remove nil 
                                   (mapcar #'(lambda (mrk) 
                                               (let ((mrkms (round (sec->ms mrk))))
                                                 (when (and (>= mrkms screen-t1) (<= mrkms screen-t2))
                                                   (list mrkms
                                                         (- (get-x-pos sheetpanel (+ mrkms begtime) 1) begpos)))))
                                           (markers self))))
              (bpfpointslist (remove nil
                                     (loop for pt in (point-pairs bpftime)
                                           when (and (>= (car pt) begtime)
                                                     (<= (car pt) (end-t sheet-object)))
                                           collect (list (- (car pt) begtime)
                                                         (- (cadr pt) begpos)
                                                         ))))
              (time-points (remove-duplicates 
                            ;;(sort (copy-list (append bpfpointslist markerlist)) '< :key 'car)
                            (list (list 0 0)
                                  (list dur (get-x-pos sheetpanel dur 1)))
                            :test '= :key 'car)))
          
          ;;; temp modif because internal-metafile can not be drawn by segments
          ;(setf picture (om-internal-picture-to-pict picture view))

          (loop for segment on time-points ; by 'cddddr
                while (cdr segment) do
                (let ((x1 (cadr (car segment)))
                      (x2 (cadr (cadr segment)))
                      (pict-x1 (soundms2pix (car (car segment)) pictw dur))
                      (pict-x2 (soundms2pix (car (cadr segment)) pictw dur)))
                  (when t ;(and (>= (+ begtime (car (car segment))) screen-t1)
                          ;     (<= (+ begtime (car (cadr segment))) screen-t2))
                  (om-draw-picture view picture 
                                   ;:pos (om-make-point x1 0) :size (om-make-point (- x2 x1) boxh)
                                   :srctopleft (om-make-point pict-x1 0) :srcsize (om-make-point (- pict-x2 pict-x1) picth)
                                   )
                  ))
                )
        
          (loop for item in markerlist do
                  (om-with-fg-color view *om-steel-blue-color*
                    (om-with-line '(2 3 )
                      (om-draw-line (cadr item) 0 (cadr item) boxh)
                    )))
          ))))




;====================
; MAQUETTE
;====================

(defmethod offset->ms ((self ommaquette) &optional grandparent) 0)

(defmethod draw-track-event ((self ommaquette) sheet-object view sheetpanel)
  (let* ((durmaq (get-obj-dur self))
         (rangey (max-y-item self))
         (miny (second rangey))
         (maxy (first rangey))
         (rangey (- maxy miny))
         (y0 4)
         (x0 0)
         (boxw (w view))
         (boxh (h view))
         (deltah (+ 8 (if (eval-func self) (round boxh 5) 0)))
         (drawh (- boxh deltah))
         (begtime (start-t sheet-object))
         (begpos (get-x-pos sheetpanel begtime 1)))
    ;(om-with-fg-color view *om-gray-color* ;;; (maq-color (params self))
    ;  (om-with-line-size 1 
    ;    (om-draw-rect 0 0 (- boxw 2) (- boxh 1))))
    (when (boxes self)
      (loop for item in (remove-if-not 'boxtempobj-p (boxes self)) do
            (let ((x1 (- (get-x-pos sheetpanel (+ (slot-value item 'offset) begtime) 1) begpos))
                  (x2 (- (get-x-pos sheetpanel (+ (slot-value item 'offset) (round (* (slot-value item 'extend) (slot-value item 'strech-fact))) begtime) 1) begpos))
                  (ry (+ y0 (- drawh (round (* drawh (- (posy item) miny)) rangey))))
                  (rh (round (* drawh (sizey item)) rangey)))
                (om-with-fg-color view (colorframe item)
                  ;(om-fill-rect x1 ry (- x2 x1) rh)
                  (draw-obj-in-rect (car (value item)) x1 x2 ry (+ ry rh) (view-get-ed-params view) view)
                  )))
        (setf deltah (- deltah 8))
        (when (eval-func self)
          (om-with-fg-color nil *om-light-gray-color*
            (om-fill-rect 1 (- boxh deltah) (- boxw 2) (1- deltah)))
          (om-draw-string 50 (- boxh 4) (format nil "~A" (value self)))
          ))))





;====================
; MIDIFILE
;====================

(defmethod draw-track-event ((self midifile) sheet-object view sheetpanel)
  (let ((bpftime (timebpf sheetpanel)))
    (loop for track in (tracks self)
          for i = 0 then (+ i 1) do
          (om-with-fg-color nil (nth (mod i 15) *16-color-list*)
            (let* ((minx 0)
                   (maxx (real-dur self))
                   (miny 24)
                   (maxy 96)
                   (x-notes (give-notes-in-x-range track minx maxx))
                   (notes (sort (give-notes-iny-range x-notes miny maxy) '< :key 'second))
                   (ysize (round (- y1 y0) (- maxy miny)))
                   (ranges (list minx maxx miny maxy)))
              (loop for note in notes do
                    (let* ((topleft (point-to-pixel-with-sizes ranges (om-make-big-point (second note) (first note)) (- x1 x0) (- y1 y0)))
                           (realleft (- (get-x-pos sheetpanel (+ (start-t (reference (reference gobj))) (second note))) (sheet-offset view) x0))
                           (realright (- (get-x-pos sheetpanel (+ (start-t (reference (reference gobj))) (+ (second note) (third note)))) (sheet-offset view) x0)))
                      (om-fill-rect realleft (+ y0 (om-point-v topleft)) (- realright realleft) 1))))))))


;====================================
;TEXT
;====================================
(defclass! temporal-text ()
  ((textos :initform '((0 ("hola"))) :initarg :textos :accessor textos)
   (text-dur :initform 2000 :initarg :text-dur :accessor text-dur)))

(defmethod allowed-in-sheet ((self temporal-text)) t)

(defmethod get-obj-dur ((self temporal-text)) (+ (text-dur self) (caar (last (textos self)))))

(defmethod draw-track-event ((self temporal-text) sheet-object view sheetpanel)
  (loop for item in (textos self) do
        (let ((xpos (- (get-x-pos sheetpanel (+ (start-t (reference (reference gobj))) (car item))) (sheet-offset view) x0))
              (ypos 5))
          (loop for str in (second item) do
                (om-draw-string xpos (incf ypos 20) str)))))
