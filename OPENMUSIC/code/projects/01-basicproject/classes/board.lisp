(in-package :om)

;;;;; fred voisin  IRCAM avril 2002


(defclass! board ()
   ((data :initform nil :accessor data :initarg :data :type list)
    (bcolor :initform *om-black-color* :accessor bcolor )
    (ecolor :initform *om-white-color* :accessor ecolor ))
   (:icon 518)
   (:documentation "Data is a list of triplets x-pos y-pos and time"))

(defmethod! get-x-y-z ((self board))
   :numouts 3
   :icon 500
   (let (x-list y-list z-list)
     (loop for item in (data self) do
           (push (first item) x-list)
           (push (second item) y-list)
           (push (third item) z-list))
     (values (reverse x-list) (reverse y-list) (reverse z-list))))

;==============================
(defmethod Class-has-editor-p ((self board)) t)
(defmethod get-editor-class ((self board)) 'boardEditor)

(defmethod rep-editor ((self board) num)
  (cond 
   ( (= num 0) self)
   ((= num 1) (rep-board-data (cdr (reverse (data self)))))))

(defmethod draw-obj-in-rect ((self  board) x x1 y y1 edparams  view)
  (let ((data (data self)) datax daty last-pt x0 y0)
    (when data
      (setf data (mat-trans data))
      (setf datax (om-scale (first data) 0 (- x1 x) (apply 'min (first data)) (apply 'max (first data))))
      (setf datay (om-scale (second data) 0 (- y1 y) (apply 'min (second data)) (apply 'max (second data))))
        (setf x0 (round (+ x (car datax))) 
              y0 (round (+ y (car datay))))
        (om-with-focused-view view
          (om-with-fg-color view (bcolor self) 
            (loop for xp in (cdr datax)
                  for yp in (cdr datay) do
                  (om-draw-line x0 y0  (round (+ x xp)) (round (+ y yp)))
                  (setf x0 (round (+ x xp)) 
                        y0 (round (+ y yp)))))))))

(defmethod* Objfromobjs ((Self board) (Type bpf))
  (let* ((data (rep-board-data (cdr (reverse (data self)))))
         (data (sort  (remove-duplicates (copy-list data) :key 'car) '< :key 'car))
         (data (mat-trans data))
         (new-bpf (simple-bpf-from-list (car data) (second data) (type-of type) 6 )))
    (setf (bpfcolor new-bpf) (bcolor self))
    new-bpf))

(defmethod* Objfromobjs ((Self board) (Type bpc))
  (let* ((data (rep-board-data (cdr (reverse (data self)))))
         (data (mat-trans data))
         (new-bpf (simple-bpf-from-list (car data) (second data) (type-of type) 6 )))
    (setf (bpfcolor new-bpf) (bcolor self))
    new-bpf))

;=====================

(defun /0 (x y)
  (if (zerop y) 0 (/ x y)))

(defmethod scale-board-data ((data list))
  (let* ((mapcadr (mapcar #'cadr data))
         (miny (apply #'min mapcadr))
         (maxy (apply #'max mapcadr))
         (minx (apply #'min (mapcar #'car data)))
         (maxx (apply #'max (mapcar #'car data)))
         (minz (apply #'min (mapcar #'third data)))
         (maxz (apply #'max (mapcar #'third data)))
         datan)
    (setf datan
          (loop for x in data
                for n from 0 to (1- (length data))
                collect
                (list
                 (* -1.0 (/ (- minx (car x)) (- maxx minx)))
                 (1+ (* 1.0 (/ (- miny (cadr x)) (- maxy miny))))
                 (float (/ (abs (* (- minz (third x)) 1)) (- maxz minz)))  ;;;approx
                 n)))
    (setf datan (sort datan '< :key #'fourth))))

(defmethod rep-board-data ((data list))
 (loop for item in (scale-board-data data) collect 
       (list (first item) (second item) (third item))))

;;; !!! erreur avec interpol-segment?
(defun interpol-list  (list nbsamples)
  (let* ((x-max (car (butlast list)))
         (x-min (car list))
         (step (/ (- x-max x-min) (1- (float nbsamples)))))
    (loop with x = (pop list) and xx = (pop list)
          with x-index = x-min
          with s-index = 0
          while (and xx  (< s-index nbsamples))
          if (and (>= x-index x) (<= x-index xx))
            collect (interpol-segment x-index x xx)
            and do (setf x-index (+ x-min (*  (incf s-index) step)))
          else do (setf x xx xx (pop list-x)))))

(defun echant (list n)
  "Does a resampling of list by n samples."
  (let ((l (length list))
        (r '()))
    (if (>= n l)
      (dotimes (i n (reverse r))
        (push (nth (floor (* i (/ l n))) list) r))
      (let ((newn (- n 2)))
        (dotimes (i newn (append (list (car list)) (reverse r) (last
                                                                list)))
          (push (nth (round (* (1+ i) (/ (- l 2) (if (evenp newn)
                                                   (1+ newn)
                                                   (if (= 1 newn)
                                                     2 newn))))) list)
                r))))))

(defun resamp-draw (data n)
  (progn
    (setf data (sort data '< :key #'first))
    (sort (om::mat-trans (list (echant (mapcar #'first data) n)
                               (echant (mapcar #'second data)  n)
                               (echant (mapcar #'third data) n)
                               (echant (mapcar #'fourth data) n)))
          '< :key #'fourth)))

;;; from morphologie ===
(defun min-dom (list)
  (first list))
(defun max-dom (list)
  (second list))
(defun pos-int-dom (list-dom val)
  (flet ((match (x) (and (<= val (max-dom x)) (>= val (min-dom x)))))
    (position-if #'match list-dom)))

;;;====================

(defmethod! samp-board ((self board) (n integer) &optional reg)
  :initvals '(nil 20 t)
  :icon 518
  :numouts 4
  (let* ((data (scale-board-data (cdr (reverse (data self)))))
         (d (resamp-draw data n)))
    (if reg
      (let ((onsets (mapcar #'first  d))
            (domain (loop for k from 0 to 1. by (/ 1 n)
                          collect k)))
        
        (values d (loop for o in onsets
                        collect
                        (pos-int-dom (mat-trans (list (mapcar #'car domain)
                                                      (mapcar #'cadr domain)))
                                     o))
                (mapcar #'second d)
                (mapcar #'third d)))
      (values d (mapcar #'first  d) (mapcar #'second d) (mapcar #'third d)))))

;=====================

(defclass boardEditor (editorview) 
  ((last-mouse-pos :accessor last-mouse-pos :initform nil)
   (mode :initform 0 :accessor mode)))


(defmethod handle-key-event ((self boardeditor) char)
  (case char 
    (#\c (let ((new-color (om-choose-color-dialog :color (bcolor (object self)))))
            (when new-color
              (setf (bcolor (object self)) new-color)
              (om-invalidate-view self))))
    (#\C (let ((new-color (om-choose-color-dialog :color (ecolor (object self)))))
            (when new-color
              (setf (ecolor (object self)) new-color)
              (om-set-bg-color self new-color))))
    (:om-key-delete 
     (setf (data (object self)) nil)
     (om-invalidate-view self))
    (otherwise (call-next-method))))
    

(defmethod initialize-instance :after ((self boardEditor) &key)
   (setf (last-mouse-pos self) (om-make-point 0 0))
   (om-set-bg-color self (ecolor (object self))))

(defmethod om-draw-contents :after ((self boardEditor))
  (let ((data (data (object (editor self)))))
    (when data
      (om-with-focused-view self
                            (om-with-fg-color self (bcolor (object self))
                              (let ((last-pt (car data)) x0 y0)
                                (setf x0 (car last-pt) y0 (cadr last-pt))
                                (dolist (pt data)
                                   (om-draw-line x0 y0 (car pt) (cadr pt))
                                   (setf x0 (car pt) y0 (cadr pt)))))))))

(defmethod editor-null-event-handler :after ((view boardEditor))
  (unless (om-points-equal-p (last-mouse-pos view) (setf (last-mouse-pos view) (om-mouse-position view)))
    (when (om-command-key-p)
      (let* ((pt (last-mouse-pos view))
             (size (om-view-size view))
             (position (om-view-position view))
             (vpos-x (om-point-h position))
             (vpos-y (om-point-v position))
             (view-width (om-point-h size))
             (view-height (om-point-v size)))
        (when (and (< (+ (om-point-h pt) 0) (+ view-width vpos-x))
                   (< (+ (om-point-v pt) 0) (+ view-height vpos-y))
                   (> (+ (om-point-h pt) 0) (+ vpos-x 2))
                   (> (+ (om-point-v pt) 0) (+ vpos-y 2)))
          (push (list (- (om-point-h pt) vpos-x) (- (om-point-v pt) vpos-y)(get-internal-run-time)) 
                (data (object view))))
        ;;;(om-invalidate-view view)     
        (when (and (car (data (object view))) (cadr (data (object view)))) 
            (om-with-focused-view view
                   (om-with-fg-color view (bcolor (object view))
                          (om-draw-line (car (car (data (object view)))) (cadr (car (data (object view)))) (car (cadr (data (object view))))(cadr (cadr (data (object view))))))))
     ))))

;=======================


