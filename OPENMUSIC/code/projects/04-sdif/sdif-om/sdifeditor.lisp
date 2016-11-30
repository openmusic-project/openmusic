
(in-package :om)


(defclass sdifeditor (editorview) 
  ((filedescpanel :accessor filedescpanel :initform nil)
   (datapanel :accessor datapanel :initform nil)
   (selection :accessor selection :initform nil)))


(defmethod class-has-editor-p ((self sdiffile)) t)
(defmethod get-editor-class ((self sdiffile)) 'sdifeditor)

;(defmethod OpenObjectEditor ((self sdifFilebox)) (call-next-method))

(defmethod get-win-ed-size ((self sdiffile)) 
  (om-make-point 400 500))

(defclass sdif-desc-panel (om-view) ())

(defclass sdif-data-panel (om-view) 
  ((menuitem :accessor menuitem :initarg :menuitem :initform nil)
   (drawpane :accessor drawpane :initarg :drawpane :initform nil)
   (data :accessor data :initarg :data :initform nil)
   (vmin :accessor vmin :initarg :vmin :initform nil)
   (vmax :accessor vmax :initarg :vmax :initform nil)
   (tmax :accessor tmax :initarg :tmax :initform 0)))

(defmethod editor ((self sdif-desc-panel)) 
  (om-view-container self))
(defmethod editor ((self sdif-data-panel)) 
  (om-view-container self))

(defclass sdif-fstreamdesc-panel (om-view) 
  ((fstrdesc :accessor fstrdesc :initarg :fstrdesc :initform nil))
  (:default-initargs :draw-with-buffer t))
   
(defclass sdif-mstreamdesc-panel (om-view) 
  ((mstrdesc :accessor mstrdesc :initarg :mstrdesc :initform nil)
   (selected :accessor selected :initarg :selected :initform nil))
  (:default-initargs :draw-with-buffer t))

(defmethod editor ((self sdif-fstreamdesc-panel)) 
  (editor (om-view-container self)))
(defmethod editor ((self sdif-mstreamdesc-panel)) 
  (editor (om-view-container self)))

(defmethod initialize-instance :after ((self sdifeditor) &rest l)
  (declare (ignore l))
  (init-sdif-panels self)
  self)

(defmethod update-editor-after-eval ((self sdifeditor) val)
  (setf (object self) val)
  (apply 'om-remove-subviews (cons self (om-subviews self)))
  (init-sdif-panels self)
  (init-selection self)
  (update-subviews self))

(defmethod init-sdif-panels ((self sdifeditor))
  (let ((mainpanelcolor *om-gray-color*)
        (streampanelcolor (om-make-color 0.19 0.25 0.22))
        (matpanelcolor *om-light-gray-color*)) ; (om-make-color 0.19 0.3 0.32)))
    
    (om-set-window-title (window self) (namestring (filepathname (object self))))
    
    (om-add-subviews self (setf (filedescpanel self) 
                                (om-make-view 'sdif-desc-panel 
                                              :position (om-make-point 0 0)
                                              :size (om-make-point 100 30)
                                              :bg-color mainpanelcolor)))
  
    (loop for fstream in (streamsdesc (object self)) do
          (let ((strpanel (om-make-view 'sdif-fstreamdesc-panel
                                        :bg-color streampanelcolor
                                        :fstrdesc fstream)))
            (om-add-subviews (filedescpanel self) strpanel)
            (if (fstreamdesc-matrices fstream)
                (loop for mstream in (fstreamdesc-matrices fstream) do 
                      (om-add-subviews strpanel (om-make-view 'sdif-mstreamdesc-panel
                                                              :bg-color matpanelcolor
                                                              :mstrdesc mstream))
                      )
              ;(om-add-subviews strpanel (om-make-view 'sdif-mstreamdesc-panel
              ;                                        :bg-color matpanelcolor
              ;                                        :mstrdesc NIL))
              )))
    
    (om-add-subviews self (setf (datapanel self) 
                                (om-make-view 'sdif-data-panel 
                                              :position (om-make-point 0 0)
                                              :size (om-make-point 100 30)
                                              :bg-color matpanelcolor)))
    ))
  

(defmethod update-subviews ((self sdifeditor))
  (call-next-method)
  (let ((space 0))
    (om-set-view-size (filedescpanel self)
                      (om-make-point (round (- (w self) (* space 3)) 2) ; (- (w self) (* space 2))
                                     (- (h self) (* space 2)))) 
    (update-subviews (filedescpanel self))

    (om-set-view-size (datapanel self)
                      (om-make-point (round (- (w self) (* space 3)) 2)
                                     (- (h self) (* space 2))))
    (om-set-view-position (datapanel self)
                      (om-make-point (round (- (w self) (* space 3)) 2)
                                     space))

    (update-subviews (datapanel self))
    ))

(defmethod update-subviews ((self sdif-desc-panel))
  (when (om-subviews self)
    (let* ((space 4)
           (n1 (max 1 (apply '+ (mapcar #'(lambda (sv) (length (om-subviews sv))) (om-subviews self)))))
           (n (length (om-subviews self)))
           (vh (round (- (h self) (* (1+ n) space) (* n 16)) n1))
           (vw (- (w self) (* space 2)))
           (vx space)
           (pos space))
      (loop for sv in (om-subviews self) 
            for i = 0 then (+ i 1) do
            (let ((nsv (max 1 (length (om-subviews sv)))))
              (om-set-view-size sv (om-make-point vw (+ 16 (* vh nsv))))
              (om-set-view-position sv (om-make-point vx pos))
              (setf pos (+ pos 16 (* vh nsv) space))
              (update-subviews sv)))
      )))
  
(defmethod update-subviews ((self sdif-fstreamdesc-panel))
  (when (om-subviews self)
    (let* ((n (length (om-subviews self)))
           (vh (round (- (h self) (* (1+ n) 4) 16) n))
           (vw (- (w self) 8))
           (vx 4))
      (loop for sv in (om-subviews self) 
            for i = 0 then (+ i 1) do
            (om-set-view-size sv (om-make-point vw vh))
            (om-set-view-position sv (om-make-point vx (+ 4 16 (round (* i (+ 4 vh)))))))
      )))


(defmethod init-selection ((self sdifeditor))
  (loop for sp in (om-subviews (filedescpanel self)) do
        (loop for mp in (om-subviews sp) do
              (setf (selected mp) nil)
              (om-set-bg-color mp *om-light-gray-color*)
              (om-set-fg-color mp *om-black-color*)))
  (setf (selection self) nil)
  (apply 'om-remove-subviews (cons (datapanel self) (om-subviews (datapanel self))))
  (setf (menuitem (datapanel self)) nil
        (drawpane (datapanel self)) nil
        (data (datapanel self)) nil)
  )


(defmethod set-selection ((self sdifeditor) (mpanel sdif-mstreamdesc-panel))
  (init-selection self)
  (setf (selected mpanel) t)
  (setf (selection self) mpanel)  
  (om-set-bg-color mpanel (om-make-color 0.25 0.29 0.29))
  (om-set-fg-color mpanel *om-white-color*)
  (set-data-panel (datapanel self))
  (om-invalidate-view self t))

(defmethod om-draw-contents ((self sdif-fstreamdesc-panel)) 
  (om-with-focused-view self
    (om-with-fg-color self *om-light-gray-color*
      (om-with-font *om-default-font1*
                    (om-draw-string 4 12 (format nil "Stream ~D: ~A [~D frames from ~f to ~fs]" 
                                                 (fstreamdesc-id (fstrdesc self)) (fstreamdesc-fsig (fstrdesc self))
                                                 (fstreamdesc-nf (fstrdesc self)) (fstreamdesc-tmin (fstrdesc self)) (fstreamdesc-tmax (fstrdesc self))))
                    (unless (fstreamdesc-matrices (fstrdesc self)) 
                      (om-draw-string 4 28 "[no matrices inside]"))
                    ))))

(defmethod om-draw-contents ((self sdif-mstreamdesc-panel)) 
  (om-with-focused-view self
    (if (mstrdesc self)
      (om-with-font *om-default-font1*
                    (om-draw-string 4 12 (format nil "Matrix: ~A" (mstreamdesc-msig (mstrdesc self))))
                    (om-draw-string 4 24 (format nil "  Matrix Fields: ~A" (mstreamdesc-fields (mstrdesc self))))
                    (om-draw-string 4 36 (format nil "  Max. Elts.: ~D" (mstreamdesc-rmax (mstrdesc self))))
                    (om-draw-string 4 48 (format nil "  Nb. Occurrences: ~D" (mstreamdesc-nf (mstrdesc self)))))
      (om-with-font *om-default-font1*
                    (om-draw-string 4 12 "[empty]"))
      )))

(defmethod om-view-click-handler ((self sdif-mstreamdesc-panel) pos)
  (unless (selected self)
    (set-selection (editor self) self)))



;;;===============
;;; DRAW PART
;;;===============

(defclass sdif-draw-pane (om-view) ()
  (:default-initargs :draw-with-buffer t))

(defmethod editor ((self sdif-draw-pane)) 
  (editor (om-view-container self)))


(defmethod get-selected-sdif-data ((self sdif-data-panel) selectedpane &optional (field 0)) 
    (multiple-value-bind (sdifdata sdiftimes) 
        (getsdifdata (object (editor self))
                     (fstreamdesc-id (fstrdesc (om-view-container selectedpane)))
                     (fstreamdesc-fsig (fstrdesc (om-view-container selectedpane)))
                     (mstreamdesc-msig (mstrdesc selectedpane))
                     field nil nil nil nil)
      (setf (vmin self) (list-min (flat sdifdata)))
      (setf (vmax self) (list-max (flat sdifdata)))
      (setf (tmax self) (car (last sdiftimes)))
      
      (setf (data self) 
            ;;; TAKES ONLY the FIRST 1000 ROWS
            (loop for r from 0 to (min 100 (1- (mstreamdesc-rmax (mstrdesc selectedpane)))) collect
                  (loop for timetag in sdiftimes
                        for data in sdifdata 
                        when (nth r data) collect 
                        (list timetag (nth r data)))
                  ))
      ))
           

(defmethod set-data-panel ((self sdif-data-panel))
  (unless (menuitem self)
    (om-add-subviews self (setf (menuitem self)
                                (om-make-dialog-item 'om-pop-up-dialog-item
                                                     (om-make-point (- (w self) 180) 4)
                                                     (om-make-point 160 18) ""
                                                     :di-action (om-dialog-item-act item
                                                                  (get-selected-sdif-data self (selection (editor self))
                                                                                          (om-get-selected-item-index item))
                                                                  (om-invalidate-view self t)
                                                                  (when (drawpane self) (om-invalidate-view (drawpane self) t)))
                                                     ))))
  (when (selection (editor self))
    (om-set-item-list (menuitem self)
                      (mstreamdesc-fields (mstrdesc (selection (editor self))))))

  (unless (drawpane self)
    (om-add-subviews self (setf (drawpane self)
                                (om-make-view 'sdif-draw-pane
                                              :position (om-make-point 20 40)
                                              :size (om-make-point (- (w self) 45) (- (h self) 60))
                                              ))))
  (get-selected-sdif-data self (selection (editor self)))
  )

(defmethod update-subviews ((self sdif-data-panel))
  (when (menuitem self)
    (om-set-view-position (menuitem self)
                          (om-make-point (- (w self) 180) 4)))
  (when (drawpane self)
    (om-set-view-size (drawpane self) 
                      (om-make-point (- (w self) 45) (- (h self) 60)))))
            
(defmethod om-draw-contents ((self sdif-data-panel)) 
  (when (selection (editor self))
    (om-with-focused-view self
      (om-with-font *om-default-font1b*
                    (om-draw-string 20 20 (format nil "Matrix: ~A" (mstreamdesc-msig (mstrdesc (selection (editor self)))))))
      )))



;;; ONLY FIRST ROW & DOES NOT USE TIME !!!
(defmethod om-draw-contents ((self sdif-draw-pane)) 
  (om-with-focused-view self
    (let* ((vals (data (om-view-container self)))
           (mi (vmin (om-view-container self)))
           (ma (vmax (om-view-container self)))
           (lh (- (h self) 20))
           (xfact (if (plusp (tmax (om-view-container self)))
                      (/ (- (w self) 20) (tmax (om-view-container self)))
                    1))
           (delta 0))
      (if (= mi ma) (setf mi (- mi 10) ma (+ ma 10)))
      (loop for row in vals
            for i = 0 then (+ i 1) do
            ;(om-with-fg-color self (om-make-color (* i 0.05) (* i 0.05) (* i 0.05))
            (if (= 1 (length row)) (om-draw-string 20 (+ 20 (* i 20)) (format nil "~f" (cadr (car row))))
            (loop for v on row
                  when (cdr v) do
                  (om-draw-line  (+ 10 (* (first (car v)) xfact))
                                 (+ 10 (om-scale (second (car v)) lh 0 mi ma))
                                 (+ 10 (* (first (cadr v)) xfact))
                                 (+ 10 (om-scale (second (cadr v)) lh 0 mi ma))
                  ))
            ;)
            ))
      )))










