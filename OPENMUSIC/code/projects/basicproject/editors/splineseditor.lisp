;; jb 28.03.05

(in-package :om)



;;;===================================
;;; INTERFACE SPLINES dans BPF EDITOR
;;;===================================

(defmethod spline-p ((self bpfeditor)) t)
(defmethod spline-p ((self t)) nil)


(defclass editor-spline-manager ()
  ((editor :accessor editor :initform nil :initarg :editor)
   (active :accessor active :initform nil :initarg :active)
   (degre :accessor degre :initform 3 :initarg :degre)
   (s-resolution :accessor s-resolution :initform 100 :initarg :s-resolution)
   (opt-items :accessor opt-items :initform nil :initarg :opt-items)
   (outspline :accessor outspline :initform nil :initarg :outspline)))


(defmethod init-spline-manager ((self bpfeditor))
  (let ((splm (make-instance 'editor-spline-manager :editor self)))
    (setf (spline self) splm)
    (om-add-subviews (control self)
                     ;(om-make-dialog-item 'om-static-text (om-make-point 220 6) (om-make-point 80 12) "Spline"
                     ;                     :font *controls-font*
                     ;                     :bg-color *controls-color*
                     ;                     )
                     (om-make-dialog-item 'om-check-box (om-make-point 400 7) (om-make-point 110 8) 
                                          (format nil "Spline preview")
                                          :di-action  (om-dialog-item-act item
                                                        (set-spline-preview (spline (om-view-container (om-view-container item)))
                                                                            (om-checked-p item))
                                                        (om-invalidate-view (panel (om-view-container (om-view-container  item))) t))
                                          :checked-p (active splm)
                                          :font *om-default-font1*
                                          :bg-color *controls-color*
                                          ))
    (when (active splm)
      (add-opt-items splm 550))
    ))

(defmethod add-opt-items ((self editor-spline-manager) pos)
  (let ((x0 pos))
    (setf (opt-items self)
          (list       (om-make-dialog-item 'om-static-text
                                           (om-make-point x0 2) 
                                           (om-make-point 70 20) "Degree" 
                                           :font *om-default-font1*
                                           :bg-color *controls-color*
                                           )
                      (om-make-dialog-item 'numBox (om-make-point (+ x0 70) 3) (om-make-point 20 18) 
                                           (format nil " ~D" (degre self))
                                           :min-val 2
                                           :max-val 5
                                           :value (degre self)
                                           :bg-color *om-white-color*
                                           :afterfun #'(lambda (item)
                                                         (setf (degre self) (value item))
                                                         (compute-spline self)
                                                         (om-invalidate-view (panel (om-view-container (om-view-container item))) t))
                                           :font *om-default-font1*
                                           )
                      (om-make-dialog-item 'om-static-text
                                           (om-make-point x0 22) 
                                           (om-make-point 70 20) "Resolution" 
                                           :font *om-default-font1*
                                           :bg-color *controls-color*
                                           )
                      (om-make-dialog-item 'numBox (om-make-point (+ x0 70) 23) (om-make-point 36 18) 
                                           (format nil " ~D" (s-resolution self))
                                           :min-val 10
                                           :max-val 999
                                           :bg-color *om-white-color*
                                           :value (s-resolution self)
                                           :afterfun #'(lambda (item)
                                                         (setf (s-resolution self) (value item))
                                                         (compute-spline self)
                                                         (om-invalidate-view (panel (om-view-container (om-view-container item))) t))
                                           :font *om-default-font1*
                                           )
                      ))
    (loop for item in (opt-items self) do 
          (om-add-subviews (control (editor self)) item))
    ))

(defmethod remove-opt-items ((self editor-spline-manager))
  (loop while (opt-items self) do 
        (om-remove-subviews (control (editor self)) (pop (opt-items self))))
  )


(defmethod set-spline-preview ((self editor-spline-manager) t-or-nil)
  (setf (active self) t-or-nil)
  (if t-or-nil
    (progn
      (add-opt-items self 550)
      (compute-spline self))
    (remove-opt-items self))
  (om-invalidate-view (panel (editor self)))
  )

(defparameter *max-spline-points* 300)

(defmethod compute-spline ((self editor-spline-manager))
  (let* ((bpf (currentbpf (panel (editor self))))
         (points (point-list bpf))
         (N (- (length points) 1))
         knots inpts)
    (when (> N 0)
    (if (< N *max-spline-points*)
      (progn
        (setf knots (SplineKnots N (degre self)))
        (setf inpts (mat-trans (list (x-points bpf) (y-points bpf))))
        (setf (outspline self) (SplineCurve2D inpts N knots (degre self) (s-resolution self)))
      )
      (om-print "spline can not be calculated because of too many points")
      ))))




(defmethod draw-bpf :after ((Self Bpfpanel) (Bpf Bpf) Minx Maxx Miny Maxy &optional (Deltax 0) (Deltay 0) (dr-points nil))
  (when (spline-p (editor self))
    (let* ((spline (spline (editor self)))
           (points (outspline spline))
           (system-etat (get-system-etat self))
           (bpf-selected? (and (equal (selection? self) t) (equal bpf (currentbpf self))))
           (fact (expt 10.0 (decimals bpf))))
      (when (and (active spline) points); bpf-selected?)
        (om-with-fg-color self *om-red2-color*
          (if (show-lines-p self)
              (loop for thepoint in points do
                    (let* ((pix-point (point2pixel self (om-make-big-point (round (* fact (car thepoint))) 
                                                                           (round (* fact (cadr thepoint)))) 
                                                   system-etat)))
                      (om-fill-rect (om-point-h pix-point) (om-point-v pix-point) 2 2)
                  ))
            (let ((p1 (point2pixel self (om-make-big-point (round (* fact (car (car points)))) 
                                                            (round (* fact (cadr (car points))))) 
                                   system-etat)))
              (loop for thepoint in (cdr points) do
                    (let* ((p2 (point2pixel self (om-make-big-point (round (* fact (car thepoint))) 
                                                                           (round (* fact (cadr thepoint)))) 
                                            system-etat)))
                      (om-draw-line (om-point-h p1) (om-point-v p1) (om-point-h p2) (om-point-v p2))
                      (setf p1 p2)))))
          
                ))
      )))


(defmethod spline ((self t)) nil)

(defmethod release-scroll-point :after ((Self Bpfpanel) initpos pos)
  (when (and (spline (editor self)) (active (spline (editor self))))
    (compute-spline (spline (editor self)))))

(defmethod release-scroll-bpf :after ((Self Bpfpanel)  initpos pos)
  (when (and (spline (editor self)) (active (spline (editor self))))
    (compute-spline (spline (editor self)))))

(defmethod handle-key-event :after ((Self Bpfpanel) char)
  (when (and (spline (editor self)) (active (spline (editor self))))
    (compute-spline (spline (editor self)))))


