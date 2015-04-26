


(in-package :om)



(defmethod editor-compatible-params-p ((ed1 scoreeditor) (ed2 scoreeditor)) t)


(defmethod default-edition-params ((self simple-container))
  (get-default-score-params self))

(defmethod get-default-score-params ((self t))
  (pairlis '(approx fontsize staff cmnpref deltapict outport inport player
             zoom notechancolor? grillestep mode winsize winpos score-mode obj-mode cursor-mode show-stems scale) 
           (list *global-midi-approx* *music-fontsize* *default-satff* (make-instance 'edition-values) (om-make-point 0 0) 
                 nil nil *default-score-player*
                 1 nil 1000 0 (om-make-point 370 280) (om-make-point 400 20) 0 1 :normal t nil)))

(defmethod get-default-score-params ((self chord))
  (pairlis '(approx fontsize staff cmnpref deltapict outport inport player
             zoom notechancolor? grillestep mode winsize winpos score-mode obj-mode cursor-mode show-stems scale) 
           (list *global-midi-approx* *music-fontsize* *default-satff* (make-instance 'edition-values) (om-make-point 0 0) 
                 nil nil *default-score-player*
                 1 nil 1000 0 (om-make-point 370 280) (om-make-point 400 20) 0 0 :normal t nil)))

(defmethod get-default-score-params ((self note))
  (pairlis '(approx fontsize staff cmnpref deltapict outport inport player
             zoom notechancolor? grillestep mode winsize winpos score-mode obj-mode cursor-mode show-stems scale) 
           (list *global-midi-approx* *music-fontsize* *default-satff* (make-instance 'edition-values) (om-make-point 0 0) nil nil *default-score-player*
                 1 nil 1000 0 (om-make-point 370 280) (om-make-point 300 20) 0 0 :normal t nil)))

;(defmethod set-edition-params ((self simple-container) box)
;   (setf (edition-params box) (get-default-score-params self)))

(defmethod corrige-edition-params ((self simple-container) params) 
  (let ((rep params))
    (unless (assoc 'deltapict params)
      (setf rep (pairlis (list 'deltapict) (list (om-make-point 0 0)) rep)))
    ;(unless (assoc 'player params)
    ;  (setf rep (pairlis (list 'player) (list :midishare) rep)))
    (unless (assoc 'zoom params)
      (setf rep (pairlis (list 'zoom) (list 1) rep)))
    (unless (assoc 'obj-mode params)
      (setf rep (pairlis (list 'obj-mode) (list 0) rep)))
    (unless (assoc 'cmnpref params)
      (setf rep (pairlis (list 'cmnpref) (list (make-instance 'edition-values)) rep)))
    (unless (new-ed-params? (cdr (assoc 'cmnpref rep)))
      (rplacd (assoc 'cmnpref rep) (make-instance 'edition-values)))
    (unless (assoc 'notechancolor? params)
      (setf rep (pairlis (list 'notechancolor?) (list nil) rep)))
    (unless (assoc 'grillestep params)
      (setf rep (pairlis (list 'grillestep) (list 1000) rep)))
    (unless (assoc 'mode params)
      (setf rep (pairlis (list 'mode) (list 0) rep)))
    (unless (assoc 'winsize params)
      (setf rep (pairlis (list 'winsize) (list (om-make-point 370 280)) rep)))
    (unless (assoc 'winpos params)
      (setf rep (pairlis (list 'winpos) (list (om-make-point 10 40)) rep)))
    (unless (assoc 'show-stems params)
      (setf rep (pairlis (list 'show-stems) (list t) rep)))
    (unless (assoc 'scale params)
      (setf rep (pairlis (list 'scale) (list nil) rep)))
    (rplacd (assoc 'deltapict rep) (om-correct-point (cdr (assoc 'deltapict  rep))))
    rep))

(defmethod corrige-edition-params ((self score-element) params)
  (when (and *force-score-player* (not (equal (cdr (assoc 'player params)) *default-score-player*)))
    (print (format nil "Warning: replacing player of ~A with default player: ~A (see 'force player' options in the MIDI preferences)." self *default-score-player*))
    (rplacd (assoc 'player params) *default-score-player*))
  (call-next-method self params))

;=====================================================================
;PARAMETRES D'EDITION
;=====================================================================


(defclas fdoc () 
   ((page-list :initform nil)))

(defmethod howmany-pages ((self fdoc))
  (length (page-list self)))

(defmethod howmany-lines ((self fdoc) i)
  (length (line-list (nth i (page-list self)))))

(defclas fpage () 
   ((line-list :initform nil)))

(defclas fline () 
   ((line-info :initform nil)))

; line-info = ( (voice #) ... (voice #) )

(defclass edition-values () 
  ((paper-size :initform (om-make-point 600 800)  :accessor paper-size :initarg :paper-size)
   (top-margin :initform 2  :accessor top-margin :initarg :top-margin)
   (left-margin :initform 1  :accessor left-margin :initarg :left-margin)
   (right-margin :initform 1  :accessor right-margin :initarg :right-margin)
   (bottom-margin :initform 1  :accessor bottom-margin :initarg :bottom-margin)
   (orientation :initform nil  :accessor orientation :initarg :orientation)
   (scale :initform nil  :accessor scale :initarg :scale)
   (system-space :initform '(1)  :accessor system-space :initarg :system-space)
   (system-color :initform nil :accessor system-color :initarg :system-color)
   (line-space :initform 1  :accessor line-space :initarg :line-space)
   (fdoc :initform (make-instance 'fdoc)  :accessor fdoc :initarg :fdoc)
   (title :initform nil  :accessor title :initarg :title)
   (picts-list :initform nil  :accessor picts-list :initarg :picts-list)
   (show-title? :initform nil  :accessor show-title? :initarg :show-title?)
   (show-page? :initform nil  :accessor show-page? :initarg :show-page?)
   (sheet-id :initform nil  :accessor sheet-id :initarg :sheet-id)
   (page-mode :initform nil  :accessor page-mode :initarg :page-mode)))

(defmethod new-ed-params? ((self edition-values)) t)
(defmethod new-ed-params? ((self t)) nil)

(defmethod omNG-save ((self edition-values) &optional (values? nil))
  `(let ((newobj (make-instance ',(type-of self))))
     (setf (paper-size newobj) ,(om-save-point (paper-size self)))
      (setf (top-margin newobj) ,(top-margin self))
      (setf (left-margin newobj) ,(left-margin self))
      (setf (right-margin newobj) ,(right-margin self))
      (setf (bottom-margin newobj) ,(bottom-margin self))
      (setf (orientation newobj) ,(orientation self))
      (setf (scale newobj) ,(scale self))
      (setf (system-space newobj) ',(system-space self))
      (setf (system-color newobj) ',(omng-save (system-color self)))
      (setf (line-space newobj) ,(line-space self))
      (setf (title newobj) ,(title self))
      (setf (show-title? newobj) ,(show-title? self))
      (setf (show-page? newobj) ,(show-page? self))
      (setf (sheet-id newobj) ,(sheet-id self))
      (setf (page-mode newobj) ,(page-mode self))
     newobj))


;==============
(defmethod score-paper-size ((self t) &optional val)
  (om-score-paper-size))

(defmethod score-top-margin ((self t) &optional val)
  (if val
    (setf (top-margin (edition-values self)) val)
    (top-margin (edition-values self))))

(defmethod score-left-margin ((self t) &optional val)
  (if val
    (setf (left-margin (edition-values self)) val)
   (if (not *old-print-mode*) 0 (left-margin (edition-values self)))))

(defmethod score-right-margin ((self t) &optional val)
  (if val
    (setf (right-margin (edition-values self)) val)
     (if (not *old-print-mode*) 0 (right-margin (edition-values self)))))

(defmethod score-bottom-margin ((self t) &optional val)
  (if val
    (setf (bottom-margin (edition-values self)) val)
    (bottom-margin (edition-values self))))

(defmethod score-orientation ((self t) &optional val)
  (if val
    (setf (orientation (edition-values self)) val)
    (orientation (edition-values self))))

(defmethod score-scale ((self t) &optional val)
  (if val
    (setf (scale (edition-values self)) val)
    (scale (edition-values self))))

(defmethod score-system-space ((self t) &optional val)
  (if val
    (setf (system-space (edition-values self)) val)
    (system-space (edition-values self))))

(defmethod score-line-space ((self t) &optional val)
  (if val
    (setf (line-space (edition-values self)) val)
    (line-space (edition-values self))))

(defmethod score-fdoc ((self t) &optional val)
  (if val
    (setf (fdoc (edition-values self)) val)
    (fdoc (edition-values self))))



(defmethod score-title ((self t) &optional val)
  (if val
    (setf (title (edition-values self)) val)
    (title (edition-values self))))

(defmethod score-show-title? ((self t) &optional val)
  (if val
    (setf (show-title? (edition-values self)) val)
    (show-title? (edition-values self))))

(defmethod score-show-page? ((self t) &optional val)
  (if val
    (setf (show-page? (edition-values self)) val)
    (show-page? (edition-values self))))

(defmethod score-sheet-id ((self t) &optional val)
  (if val
    (setf (sheet-id (edition-values self)) val)
    (sheet-id (edition-values self))))

(defmethod score-page-mode ((self t))
  (page-mode (edition-values self)))

(defmethod set-score-page-mode ((self t) val)
  (setf (page-mode (edition-values self)) val))

(defmethod score-picts-list ((self t) &optional val)
  (if val
    (setf (picts-list (edition-values self)) val)
    (picts-list (edition-values self))))


(defmethod score-widht ((self t) size)
  (- (om-point-h (score-paper-size self)) (* size (score-left-margin self)) (* size (score-right-margin self))))
 
(defmethod score-height ((self t) size)
  (- (om-point-v (score-paper-size self)) (* size (score-bottom-margin self)) (* size (score-top-margin self))))


;==============



(defmethod correct-page-par ((self scoreEditor) par)
  (if (new-ed-params? par) par
      (let ((rep (make-instance 'edition-values)))
        (set-edit-param self 'cmnpref rep)
        rep)))


(defclass page-pointer ()
  ((curpage :initform 0 :accessor curpage)
   (curline :initform 0 :accessor curline)
   (curelem :initform 0 :accessor curelem)
   (score :initform nil :accessor score :initarg :score)
   (page-voice :initform nil :accessor page-voice :initarg :page-voice)
   (view :initform nil :accessor view :initarg :view)
   (linesizex :initform nil :accessor linesizex :initarg :linesizex)
   (linesizey :initform nil :accessor linesizey :initarg :linesizey)))


(defmethod move-page-pointer ((self page-pointer))
  (let* ((fdoc (score-fdoc (score self)))
         (curpage (nth (curpage self) (page-list fdoc)))
         (curline (nth (curline self) (line-list  curpage)))
         (hmlines (length (line-list curpage)))
         (hmelem (nth (page-voice self) (line-info curline))))
    (setf (curelem self) (+ (curelem self) 1))
    (if (= (curelem self) hmelem)
      (progn
        (setf (curelem self) 0)
        (setf (curline self) (+ (curline self) 1))
        (if (and (= (curline self) hmlines) )
          (progn
            (setf (curline self) 0)
            (setf (curpage self) (+ (curpage self) 1))
            (setf (view self) (nth (curpage self) (score-picts-list (score self))))))))))


(defmethod compute-delat-x-y ((self page-pointer))
  (let* ((score (score self))
         (fdoc (score-fdoc score))
         (linesizex  (linesizex self))
         (linesizey  (linesizey self))
         (repx 0) (repy 0))
    (loop for i from 0 to (- (curpage self) 1) do
          (setf repx (+ repx (* linesizex (length (line-list  (nth i (page-list fdoc))))))))
    (setf repx (+ repx (* linesizex (curline self))))
    (setf repy (* linesizey (curline self)))
    (list repx repy)))

;ca sert a rien ???
(defmethod compute-delat-x-y-zoom ((self page-pointer))
  (let* ((score (score self))
         (fdoc (score-fdoc score))
         (linesizex  (linesizex self))
         (linesizey  (linesizey self))
         (repx 0) (repy 0))
    (loop for i from 0 to (- (curpage self) 1) do
          (setf repx (+ repx (* linesizex (length (line-list  (nth i (page-list fdoc))))))))
    (setf repx (+ repx (* linesizex (curline self))))
    (setf repy (* linesizey (curline self)))
    (list repx repy)))



(defmethod editor-page-setup ((panel scorepanel))
  (let* ((rep (show-page-score-dialog panel)))
    (when (and rep (eval `(and ,.rep)))
      (score-top-margin panel (first rep))
      (score-left-margin panel (second rep))
      (score-bottom-margin panel (third rep))
      (score-right-margin panel (fourth rep))
      (score-line-space panel (fifth rep))
      (update-panel panel))))
 

(defmethod show-page-score-dialog ((self scorepanel))
  (let* ((params (list (score-top-margin self) (score-left-margin self) (score-bottom-margin self) (score-right-margin self) (score-line-space self) ))
         (font *om-default-font1*)
         (win (om-make-window 'om-dialog
                              :window-title "Page Setup"
                              :position :centered 
                              :close t
                              :resizable nil
                              :maximize nil
                              :size (om-make-point 290 175)))
         (y 0)
         (defvals '("2" "1" "1" "1" "1"))
         paramtxt valtxt intertxt  top left bottom right inter okb annulerb defb
         )
    (setf paramtxt (om-make-dialog-item 'om-static-text (om-make-point 20 y) (om-make-point 120 20) 
                                        "Top:                 Left:" :font font))
    (incf y 26)
    (setf top (om-make-dialog-item 'om-editable-text (om-make-point 20 y)
                                      (om-make-point 50 30) (num2string (first params))))
    (setf left (om-make-dialog-item 'om-editable-text (om-make-point 100 y)
                                      (om-make-point 50 30) (num2string (second params))))
    (incf y 40)
    (setf valtxt (om-make-dialog-item 'om-static-text (om-make-point 20 y) (om-make-point 140 20) 
                                      "Bottom:            Right:"  :font font))
    (incf y 26)
    (setf bottom (om-make-dialog-item 'om-editable-text (om-make-point 20 y)
                                        (om-make-point 50 30) (num2string (third defvals))))
    (setf right (om-make-dialog-item 'om-editable-text (om-make-point 100 y)
                                        (om-make-point 50 30) (num2string (fourth params))))
    (setf y 66)
    (setf intertxt (om-make-dialog-item 'om-static-text (om-make-point 200 y) (om-make-point 130 40) 
                                        (str-check "Inter-line:") 
                                         :font font))
    (incf y 26)
    (setf inter (om-make-dialog-item 'om-editable-text (om-make-point 200 y)
                                     (om-make-point 50 30) (num2string (fifth params))))
    (incf y 40)
    (setf defb (om-make-dialog-item 'om-button 
                                    (om-make-point 20 y)  
                                    (om-make-point 70 25)
                                    "Defaults"
                                    :font font
                                    :di-action (om-dialog-item-act item
                                                 (declare (ignore item))
                                                 (om-set-dialog-item-text top (first defvals))
                                                 (om-set-dialog-item-text left (second defvals))
                                                 (om-set-dialog-item-text bottom (third defvals))
                                                 (om-set-dialog-item-text right (fourth defvals))
                                                 (om-set-dialog-item-text inter (fifth defvals)))))
   (setf okb (om-make-dialog-item 'om-button 
                                   (om-make-point 200 y)  
                                   (om-make-point 70 25)
                                   "OK"
                                   :font font
                                   :di-action (om-dialog-item-act item
                                                (declare (ignore item))
                                                (om-return-from-modal-dialog win 
                                                                             (list (string2num (om-dialog-item-text top))
                                                                                    (string2num (om-dialog-item-text left))
                                                                                    (string2num (om-dialog-item-text bottom))
                                                                                    (string2num (om-dialog-item-text right))
                                                                                    (string2num (om-dialog-item-text inter)))))))
    (setf annulerb (om-make-dialog-item 'om-button 
                                        (om-make-point 110 y)
                                        (om-make-point 70 25)
                                        "Annuler"
                                        :font font
                                        :di-action (om-dialog-item-act item
                                                     (declare (ignore item))
                                                     (om-return-from-modal-dialog win nil))))
    
    (om-add-subviews win  paramtxt valtxt intertxt  inter top left bottom right okb annulerb defb)
    (om-modal-dialog win)))
