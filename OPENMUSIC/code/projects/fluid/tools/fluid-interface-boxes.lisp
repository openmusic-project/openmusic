(in-package :om)


;=========================
; FLUID MICRO TUNE
;========================

(defclass! fluid-microtune (om-pop-up-dialog-item d-i-box) () 
   (:icon 297)
   (:documentation " Sets tuning of the fluidsynth. Second input is the Port input."
))

;; compat
(defclass! fluid-microtune-box (fluid-microtune) ()) 

(defmethod get-slot-in-out-names ((self fluid-microtune))
  (values '("items" "port") 
          '(("1" "1#" "1/2" "1/3" 
             "1/3#" "1/4" "1/5" "1/5#"
             "1/6" "1/7" "1/7#" "1/8" 
             "1/10" "1/12" "1/14" "1/16") nil)
          '("Tunings" "Port")
          '(nil nil)))

(defmethod omng-save ((self fluid-microtune) &optional (values? nil))
  `(let ((rep (om-make-dialog-item 'fluid-microtune (om-make-point 1 1 ) (om-make-point ,(om-width self) ,(om-height self) ) "untitled"
                                   :range ',(om-get-item-list self))))
     (oa::om-set-selected-item-index rep ',(om-get-selected-item-index self))
     rep))


(defmethod get-super-default-value ((type (eql 'fluid-microtune)))
  (om-make-dialog-item 'fluid-microtune (om-make-point 1 4) (om-make-point 50 20) "untitled" 
                       :range '("1" "1#" "1/2" "1/3" 
                                "1/3#" "1/4" "1/5" "1/5#"
                                "1/6" "1/7" "1/7#" "1/8" 
                                "1/10" "1/12" "1/14" "1/16")))

(defmethod update-di-size ((self fluid-microtune) container)
  (om-set-view-position self (om-make-point 10 (- (round (h container) 2) 11)))
  (om-set-view-size self (om-make-point (- (w container) 20) 24)))


(defmethod om-dialog-item-action ((self fluid-microtune)) 
  (when (oa::di-action self)
    (funcall (oa::di-action self) self)))

;(defmethod update-di-size ((self fluid-microtune) container) 
;  (om-set-view-position self #+win32(om-make-point 12 12) #-win32(om-make-point 12 10))
;  (om-set-view-size self (om-subtract-points (om-view-size container) #+win32(om-make-point 28 24) #-win32(om-make-point 28 20))))

(defmethod set-dialog-item-params ((self fluid-microtune) box args) 
  (let* ((boxframe (om-view-container self))
         (newpop (om-make-dialog-item 'fluid-microtune (om-make-point 1 4) (om-make-point (if boxframe (- (w boxframe) 20) 80) 20) 
                                      "untitled" 
                                      :range (if (and (pathnamep (car args)) (directoryp (car args)))
                                                 (om-directory (car args))
                                               (car args))
                                     ; :di-action (om-dialog-item-act item
                                     ;              (lambda ()(print (list "illinois" self box args)))
                                     ;              )          
                                      )))
 
    (when (om-view-container self)
      (om-remove-subviews boxframe self)
      (om-add-subviews boxframe newpop)
      (om-set-dialog-item-action-function newpop #'(lambda (x) 
                                                (let ((tuning (om-get-selected-item newpop))
                                                      (port (omNG-box-value (second (inputs self)))))
                                                  (if port
                                                      (change-tuning port tuning)
                                                    (change-tuning 0 tuning)
                                                    ))))
      (update-di-size newpop boxframe))
    newpop))

(defmethod rep-editor ((self fluid-microtune) num) 
  (cond
   ((= num 0) (om-get-selected-item-index self))
   ((= num 1) (om-get-selected-item self))
   (t nil)))

(defmethod (setf value) :after ((value fluid-microtune) (self OMBoxEditCall)) ;(self omdiebox))
  (om-set-dialog-item-action-function value #'(lambda (x) 
                                                (let ((tuning (om-get-selected-item value))
                                                      (port (omNG-box-value (second (inputs self)))))
                                                  (if port
                                                      (change-tuning port tuning)
                                                    (change-tuning 0 tuning)
                                                    )))))


