

(in-package :om)


(defclass! time-array (class-array)
           ((times :initform '(0) :accessor times))
           (:icon 264)
           (:documentation ""))

(defmethod get-slot-in-out-names ((self time-array))
   (values '("self" "times") '(nil (0))
           '("time-array object" "sorted list of onsets [seconds]")
           '(nil nil)))

(defmethod fixed-slots-list ((self time-array)) '(times))

(defun error-test-times (list)
  (cond
   ((null list) "Times can not be NIL")
   ((not (listp list)) "Times must be a list")
   ;((not (= (car list) 0)) "First element of times must be zero")
   ((not (apply '< list)) "Times are out of order")
   (t nil)))

(defmethod omNG-copy ((self time-array))
   `(let ((copy ,(call-next-method)))
        (setf (times copy) ',(times self))
        copy))

(defmethod cons-array ((self time-array) args argkeys)
  (let ((terr (error-test-times (second args))))
    (if terr (error terr) 
      (let ((rep (call-next-method)))
        (setf (times rep) (second args))
        rep))))
    
(defmethod array-size-from-input ((self time-array) input)
  (max 0 (1- (length input))))

(defmethod get-row-bpf ((array time-array) (row list) &optional (precision 4))
  (if (and (list-subtypep row 'number) (> (length row) 1))
       (simple-bpf-from-list (if (= 1 (length row)) '(0) (times array)) row 'bpf precision)
    nil))


(defmethod get-array-ranges ((self time-array))
   (list (- (car (times self)) 1) (+ (car (last (times self))) 1)))


;======================
; BOX
;======================

(defclass timearraybox (arrayBox) ())

(defmethod get-type-of-ed-box ((self time-array))  'timearraybox)

(defmethod do-add-one-keyword ((self timearraybox) &optional (input-key nil))
   (let* ((normalinputs (find-class-boxes  (inputs self) 'input-funbox))
          (paraminputs (find-class-boxes  (inputs self) 'control-keyword))
          (newval (format nil "Dim~D" (+ 1  (length paraminputs)))))
     
     (setf (inputs self) (append normalinputs
                                 (append paraminputs
                                         (list (make-instance 'control-keyword
                                                              :name newval         
                                                              :box-ref self
                                                              :value (string2initarg newval) 
                                                              :doc-string "Extra dimension")))))
     (pushr (list (string2initarg newval) nil) (Lcontrols (value self)))
     (update-params-for-slots self (value self))
     (when (editorFrame self)
       (update-editor-after-eval (editorFrame self) (value self)))
     t))




(defmethod matrix-gen-code ((self timearraybox) (val time-array))
   (let* ((params (decode self))
          (fixinputs (length (fixed-slots-list val)))
          (theval (if (connected? (first (inputs self)))
                    `(objFromObjs ,(gen-code (first (inputs self)) nil) ,val)
                    `(let* ((timeline ,(gen-code (second (inputs self)) nil))
                            (err (error-test-times timeline)))
                       (funcall 'cons-array ,val 
                                (list nil timeline)
                                (list ,.(cddr params)))))))
     `(let ((array ,theval))
        (set-data array)
        array)
     ))

;==========================
; EDITOR
;==========================

(defclass timearrayeditor (arrayeditor) ())
(defclass timearraypanel (arraypanel) ())

(defclass timearray-parameter-panel () ())

(defclass time-list-parameter-panel (timearray-parameter-panel list-parameter-panel) ())
(defclass time-bpf-parameter-panel (timearray-parameter-panel bpf-parameter-panel) ())

(defmethod bpf-panel-class ((self timearraypanel)) 'time-bpf-parameter-panel)
(defmethod list-panel-class ((self timearraypanel)) 'time-list-parameter-panel)

(defmethod get-editor-class ((self time-array)) 'timearrayeditor)
(defmethod editor-array-panel-class ((self timearrayeditor)) 'timearraypanel)

(defmethod get-string-nom  ((self timearraypanel)  num (axe (eql 'x)))
    (format () "~,2F" num))

(defmethod get-comp-str ((self time-array) comp-num)
  (if comp-num
      (format nil "t= [~D-~D]" (nth comp-num (times self)) (or (nth (1+ comp-num) (times self)) "..."))
    ""))

(defmethod do-editor-null-event ((self timearrayeditor))
   (when (and (om-view-contains-point-p (panel self) (om-mouse-position self)) (>= (om-point-h (om-mouse-position self)) *array-ruler-width*))
     (let* ((pixel (om-mouse-position self))
            (x (- (om-point-h pixel) 25))
            (sizex (assoc-w (panel self)))
            (durpointx (- (second (rangex (panel self))) (first (rangex (panel self)))))
            (x1 (+ (first (rangex (panel self))) (* (/ x sizex) durpointx))))
       (setf (select-comp self) (position x1 (times (object self)) :test '>= :from-end t))
       (show-composant (title-bar self))
       (show-composant (panel self))
       )))

(defmethod pixel2point ((self timearraypanel) pixel)
  (let* ((x (- (om-point-h pixel) 25))
         (sizex (assoc-w self))
         (durpointx (- (second (rangex self)) (first (rangex self))))
         (x1 (float (* (/ x sizex) durpointx))))
    (om-make-point (+ x1 (first (rangex self))) 0)
    ))

(defmethod get-ith-pixvalue ((self time-list-parameter-panel) i)
  (norme2pixel (om-view-container self) 'x (1+ (nth i (times (object (get-panel self)))))))

(defmethod get-ith-deltax ((self time-list-parameter-panel) i)
  (let ((timelist (times (object (get-panel self)))))
    (norme2pixel (om-view-container self) 'x (-  (or 
                              (nth (1+ i) timelist)
                              (1+ (nth i timelist)))
                             (nth i timelist)))))

(defmethod component-n-at ((self timearraypanel) where)
   (position (om-point-h (pixel2point self where)) (times (object (om-view-container self))) :test '> :from-end t))


(defmethod om-draw-contents ((self timearray-parameter-panel)) 
  (call-next-method)
  (om-with-focused-view self
    (om-with-line-size 2
    (om-with-fg-color self *om-red2-color*
      (loop for time in (times (object (get-panel self))) do
            (let ((x (norme2pixel (om-view-container self) 'x (1+ time))))
             (om-draw-line x 0 x (h self))))))))
          
 