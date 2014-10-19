(in-package :om)

;****************************************************************************************************
;****************************************************************************************************
;DRAWING IN LINEAR MODE
;****************************************************************************************************
;****************************************************************************************************

;;;======================
;;; ALIGNEMENT
;;;======================



;============sheet============

(defclas grap-sheet (grap-poly) 
  ((page-list :initform nil)
   (timebpf :initform nil)))

;-----------BUILD GRAPHIC OBJ
(defmethod make-graph-form-obj ((self sheet)   x top linespace mode scale sel system stem)
  (let ((graph-obj (make-graph-ryth-obj  self top system linespace  scale sel nil nil nil))
        (size  (* linespace 4)))
    (space-objects (get-temporal-objects graph-obj) (* 4 linespace))
    (set-graph-rectangles graph-obj)
    graph-obj))


(defmethod make-graph-ryth-obj ((self sheet) top staffsys linespace  scale sel pere durtot &optional ryth)
  (declare (ignore ryth durtot))
  (let* ((thesheet (make-instance 'grap-sheet :reference self :parent pere))
         (voicelist (loop for item in (inside self)
                          for i = 0 then (+ i 1) collect
                          (make-graph-ryth-obj  item (top-in-midi (nth i staffsys))  (nth i staffsys) linespace  scale sel thesheet nil))))
    (setf (inside thesheet) voicelist)
    (make-graphic-extras thesheet)
    thesheet))


;-----------DRAW GRAPHIC OBJ

(defun make-bpf-time (list)
  (let (lx ly)
    (setf list (sort (remove-duplicates list :key 'first) '< :key 'first))
    (loop for item in list do
          (push (first item) lx)
          (push (second item) ly))
    (simple-bpf-from-list (reverse lx) (reverse ly))))
  
(defmethod draw-object ((self grap-sheet) view x y zoom minx maxx miny maxy slot size modepage? staff grille-p chnote)
  (let* ((sheet (reference self))
         (timebpf (make-bpf-time (collect-bpftime-objects self 0 size)))
         (meas-list (cdr (get-aligne-measures sheet)))
         (posy y) 
         (positions nil))
    (setf (timebpf self) timebpf)
    (loop for item in (inside self)
          for i = 0 then (+ i 1) 
          for system in staff do
          (draw-object item view (if (grap-voice-p item) x (+ x size)) (- posy (round (* (posy (car (staff-list system))) (/ size 4)))) zoom minx maxx miny maxy slot size t (nth i staff) grille-p chnote)
          (setf posy (+ posy (get-delta-system system size view i)))
          (push posy positions))
    (setf positions (reverse positions))
    (collect-rectangles self)
    (draw-aligned-measures self meas-list staff size  positions)
    (draw-extras self view size staff)))

;=============track============

(defmethod make-graph-ryth-obj ((self track) top staffsys linespace  scale sel pere durtot &optional ryth)
  (declare (ignore ryth))
  (let* ((newtrack (make-instance 'grap-track
                   :reference self
                    :rectangle (list 0 0 0 0)
                   :parent pere))
         (inside (loop for item in (get-track-ev self) 
                       for i = 0 then (+ i 1)
                       append 
                       (make-graph-ryth-track-obj item  i top staffsys linespace  scale sel newtrack))))
    (setf (inside newtrack) inside)
    (make-graphic-extras newtrack) 
    newtrack))

(defmethod make-graph-ryth-track-obj ((self track-ev) i top staffsys linespace scale sel pere)
  (let* ((new-ev-s (make-instance 'grap-track-ev
                     :pict (get-obj-pict (obj self))
                     :reference self
                     :parent pere
                     :index i
                     :main-point (list 0 0)
                     :start? t
                     :rectangle (list 0 0 1 1)
                     :selected (member self sel :test 'equal)))
         (new-ev-e (make-instance 'grap-track-ev
                     :reference self
                     :index i
                     :start? nil
                     :main-point (list 0 0)
                     :parent pere
                     :rectangle (list 0 0 1 1)
                     :selected (member self sel :test 'equal))))
    (list new-ev-s new-ev-e)))


(defmethod draw-rectangle ((self grap-track) system size &optional fill )
   (when (rectangle self)
     (let* ((rec (rectangle (car (inside self))))
            (rec (list (- (car rec) 4) (- (second rec) 4) (+ (third rec) 4) (+ (fourth rec) 4))))
       (if fill
           (om-draw-hilite-rect (car rec) (second rec) (- (third rec) (car rec) ) (- (fourth rec) (second rec)))
         (om-with-fg-color nil *om-select-color*
                                          (om-draw-rect (car rec) (second rec) (- (third rec) (car rec) ) (- (fourth rec) (second rec))))
         ))))

(defmethod draw-object ((self grap-track) view x y zoom minx maxx miny maxy slot size modepage? staff grille-p chnote)
  (setf y (+ y (* 3 size)))
  (let ((sys-size (get-system-size staff size)))
    (loop for item in (get-track-ev (reference self))
          for grap-item in (inside self)
          for i = 0 then (+ i 1) do
          
          (let ((start (find-if #'(lambda (x) (and (= (index x) i) (start? x)))  (inside self)))
                (end (find-if #'(lambda (x) (and (= (index x) i) (not (start? x))))  (inside self))))
            (draw-track-event  (obj item) grap-item x 
                              (round (+ x (round size  3.5) (* zoom (car (main-point start))))) 
                              (round (+ x (round size  3.5) (* zoom (car (main-point end)))))
                              y  (- (+ y sys-size) 3) view zoom size)
            (setf (rectangle grap-item) 
                  (list (round (+ x (round size  3.5) (* zoom (car (main-point start))))) 
                        y 
                        (round (+ x (round size  3.5) (* zoom (car (main-point end))))) 
                        (- (+ y sys-size) 3)))))))


(defmethod draw-track-event ((self t) gobj deltax x0 x1 y0 y1 view zoom size sheetpanel)
  (draw-obj-in-rect self x0 x1 y0 y1 nil view))

;;(defun convert-bpf-ime (bpf time deltax zoom size)
;;  (round (+ deltax (round size 3.5) (* zoom (bpf-get-val bpf time )))))

  
;----------------

;----------------

(defmethod click-in-obj ((self grap-track-ev) type where view)
   (let* ((rect (rectangle self)))
       (when (point-in-rectangle-p where (second rect) (first rect) (fourth rect) (third rect))
         self)))
;------




(defmethod collect-temporal-objects ((self sheetpanel) at)
   (loop for item in (trackpanels  self)
         append (collect-temporal-objects item at)))

(defmethod collect-temporal-objects ((self trackpanel) at)
   (loop for item in (track-subviews self)
         append (collect-temporal-objects item at)))

;(defmethod collect-temporal-objects ((self sheet-objectframe) at)
;(list (list (+ at (start-t (reference self))) self))
;  (collect-temporal-objects (obj (reference self)) at)
;  )

(defmethod collect-temporal-objects ((self track-ev) at)
  (list (list (+ at (start-t self)) self)))

(defmethod collect-temporal-objects ((self grap-chord) at )
   (list (list at self)))

;(defmethod collect-temporal-objects ((self grap-track) at)
;   (loop for item in (inside self)
;         append (collect-temporal-objects item (+ at (if (start? item) (start-t (reference item)) (end-t (reference item)))))))

;(defmethod collect-temporal-objects ((self grap-track-ev) at )
;   (list (list  at  self)))

(defmethod collect-temporal-objects ((self grap-track) at)
   (loop for item in (inside self)
         append (collect-temporal-objects item (+ at (if (start? item) (start-t (reference (reference item))) (end-t (reference (reference item))))))))

(defmethod collect-temporal-objects ((self grap-track-ev) at )
   (list (list at self)))


(defmethod collect-bpftime-objects ((self grap-track) at size)
   (loop for item in (inside self)
         append (collect-bpftime-objects item (+ at (if (start? item) (start-t (reference (reference item))) (end-t (reference (reference item))))) size)))










