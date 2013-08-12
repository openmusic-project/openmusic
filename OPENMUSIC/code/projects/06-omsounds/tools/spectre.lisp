(in-package :om)


#|
;;;========================================================
;;; CHANGER lE NOM !!!


(defclass! spectre ()
  ((sdifdatafile :initform nil :accessor sdifdatafile)
   (in-sound :initform nil :accessor in-sound :initarg :in-sound :type string)
   (pixels :initform nil :accessor pixels )
   (begin-time :initform nil :accessor begin-time :initarg :begin-time :type t)
   (end-time :initform nil :accessor end-time :initarg :end-time :type t)
   (fft-param :initform '(4096 4000 250 "hanning" 1) :accessor fft-param :initarg :fft-param :type list)
   )
  (:icon 263))

(defmethod allowed-in-maq-p ((self spectre)) nil)




(defmethod objfromobjs ((self spectre) (type sdiffile))
  (objfromobjs (sdifdatafile self) type))  


(defmethod initialize-instance :after  ((self spectre) &rest initargs) 
  (declare (ignore initargs)) 
  (when (in-sound self)
    (setf (sdifdatafile self)
          (fft (in-sound self) (begin-time self) (end-time self) 
                      (first (fft-param self))(second (fft-param self))
                      (third (fft-param self))(fourth (fft-param self)) (fifth (fft-param self))))
    (print (format nil "spectre data stored in   ~D" (sdifdatafile self)))
    (when (sdifdatafile self)
      (setf (pixels self) (make-spect-pict self)))
    self))


(defmethod make-spect-pict  ((self spectre))
  (let* ((sdifF (make-instance 'sdiffile))
         (alldata (om-scale (getsdifdata (objfromobjs (sdifdatafile self) sdifF) 0 "1GB1" "1GB1" 0 nil nil nil nil)
                            0 1))
         (tempdata nil)
         (t-size (length alldata))
         (f-size (length (first alldata)))
         (xsize (min 800 t-size))
         (ysize (min 600 f-size))
         (t-fact (round (/ t-size xsize)))
         (f-fact (round (/ f-size ysize)))
         (view (make-instance 'window 
                 :window-show nil
                 :window-title "jaja"
                 :view-size (make-point xsize ysize)))
         thepict)
    (print "making pict...")
    (with-focused-view view
      (start-picture view)
      (loop for ypt from 0 to (- (h view) 1) do
            (loop for xpt from 0 to (- (w view) 1) do
                  (setf tempdata nil)
                  (loop for i from (* t-fact xpt) to (- (* t-fact (+ xpt 1)) 1) do
                        (loop for j from (* f-fact ypt) to (- (* f-fact (+ ypt 1)) 1) do
                        (setf val (nth j (nth i alldata)))
                        (if val (push val tempdata))))
                  ;(print tempdata)
                  (setf val (apply 'max tempdata))
                  ;(setf val (nth (* ypt f-fact) (nth (* xpt t-fact) alldata)))
                  (if (not val) (progn (print "NO VALUE!!") (setf val 1)) (setf val (- 1 (sqrt val))))
                  (with-fore-color(list2color val val val)
                    (draw-point xpt (- (h view) ypt)))))
      (setf thepict (get-picture view))
      (window-close view))
    thepict)) 




(defmethod! get-the-pict ((self spectre))
  (pixels self))


(defmethod draw-obj-in-rect ((self  spectre) x x1 y y1 edparams  view)
   (if (pixels self)
     (time (draw-picture view (pixels self) x y x1 y1 ))))

|#

   
