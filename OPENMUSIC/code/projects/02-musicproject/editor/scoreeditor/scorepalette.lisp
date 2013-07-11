(in-package :om)


;======================================================
;PALETTE   abstract class
;======================================================

;(defun but-function (item)
;  (let* ((pwin (om-view-window item))
;         (buttons (buttons *palette*))
;         (ed-view (panel (editor-assoc *palette*)))
;         (newval (position item (car buttons) :test 'equal)))
;    (loop for b in (car (buttons *palette*)) do
;          (when (selected-p b)
;            (setf (selected-p b) nil)
;            (om-invalidate-view b t)))
;    (setf (selected-p item) t)
;    (om-invalidate-view item t)
;    (setf (obj-mode ed-view) (nth newval (object-order ed-view)))
;    (off-selection ed-view)
;    (setf (selection? ed-view)  nil)
;    (om-invalidate-view ed-view t)
;   (om-invalidate-view (title-bar (editor-assoc *palette*)))))

;Buttons


;=====================
(defvar *bnote* nil)
(defvar *bchord* nil)
(defvar *bgroup* nil)
(defvar *bmes* nil)
(defvar *bvoice* nil)
(defvar *bsys* nil)
(defvar *bfleches* nil)


(defun int-scorepal-buttons ()
   (setf  *bnote* 
          (om-make-view 'om-icon-button
                                :lock-push t
                                :position (om-make-point  0 25)
                                :size (om-make-point 26 25)
                                :action #'(lambda (item)
                                            (but-function item))
                                :icon1 "note"))
   
   (setf *bchord*
      (om-make-view 'om-icon-button
                                 :lock-push t
                                   :position (om-make-point  25 25)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                                      (but-function item))
                                   :icon1 "chord"))
   
   (setf *bgroup*
      (om-make-view 'om-icon-button
                                 :lock-push t
                                   :position (om-make-point  50 25)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                                      (but-function item))
                                   :icon1 "group"))
   
   (setf *bmes*
      (om-make-view 'om-icon-button
                                 :lock-push t
                                   :position (om-make-point  75 25)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                                      (but-function item))
                                   :icon1 "meas"))
   
   (setf *bvoice*
      (om-make-view 'om-icon-button
                                 :lock-push t
                                   :position (om-make-point  100 25)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                                      (but-function item))
                                   :icon1 "voice"))
   
   (setf *bsys*
      (om-make-view 'om-icon-button
                                 :lock-push t
                                   :position (om-make-point  125 25)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                                      (but-function item))
                                   :icon1 "poly"))
   
   ;Buttons
   (setf *bfleches*
      (list (om-make-view 'om-icon-button
                                           :position (om-make-point  175 25)
                                           :size (om-make-point 26 25)
                                           :action #'(lambda (item)
                                                              (update-inspector (om-view-container item) 0))
                                           :icon1 "first") 
             (om-make-view 'om-icon-button
                                          :position (om-make-point  200 25)
                                          :size (om-make-point 26 25)
                                          :action #'(lambda (item)
                                                             (but-function item))
                                          :icon1 "prev")
             (om-make-view 'om-icon-button
                                          :position (om-make-point  225 25)
                                          :size (om-make-point 26 25)
                                          :action #'(lambda (item)
                                                             (but-function item))
                                          :icon1 "next")
             (om-make-view 'om-icon-button
                                          :position (om-make-point  250 25)
                                          :size (om-make-point 26 25)
                                          :action #'(lambda (item)
                                                             (let* ((selection (selection? (panel (editor-assoc (om-view-container item))))))
                                                               (update-inspector (om-view-container item) (- (length selection) 1))))
                                          :icon1 "last")))
   )  


(om-add-init-func 'int-scorepal-buttons)
   

;==================================================


(defclass music-score-palette (Playing-Palette) 
   ((button-mode :initform nil  :accessor button-mode)
    (extras-p :initform nil  :accessor extras-p)
    (extras :initform nil  :accessor extras)
    (indexselec :initform 0 :initarg :indexselec :accessor indexselec))
   ;(:default-initargs :source-name "musicpalette")
   )

(defclass chord-palette (music-score-palette) ()
  ;(:default-initargs :source-name nil :delta-inpix (om-make-point 150 0))
  )

;=for voice
(defclass ryth-palette (music-score-palette) ())
;(defmethod palette-act ((self ryth-palette) x) (call-next-method))
;(defmethod info-button-position ((self ryth-palette)) (call-next-method))


(defmethod palette-init ((self scoreeditor))
  (call-next-method)
  (let ((panel (panel self)))
    ;(palette-clean *palette*)
    (setf (buttons *palette*) (buttons-list panel))
    (init-play-palette self)  
     (unless (mode self)
       (setf (mode self) 7))     
     (add-palette-buttons *palette*)
     (when (extra-palette panel)
       (make-extra-palette panel *buttons-score-list*))
     ))

(defmethod palette-clean ((self music-score-palette))
  (om-with-delayed-update (view *palette-win*)
    (mapcar #'(lambda (but) (om-remove-subviews (view *palette-win*) but))
            (om-subviews (view *palette-win*))
            ;(car (buttons self))
          )))
  
(defmethod add-palette-buttons ((self music-score-palette))
   (let* ((ed-view (panel (editor-assoc self)))
          (selected (position (obj-mode ed-view) (object-order ed-view) :test 'string-equal))
          (i 0) (j 25))
     (om-with-delayed-update (view *palette-win*)
       (palette-clean self)
       (loop for item in (buttons self) do
             (setf i 0)
             (loop for b in item
                   for k = 0 then (+ k 1) do
                   ;(let ((b (om-make-view 'om-view :size (om-make-point 10 10))))
                     (setf (selected-p b)  (= k selected)) 
                     (om-set-view-position b (om-make-point i j))
                     (setf i (- (+ i (om-point-h (om-view-size b))) 1))
                     (om-add-subviews (view *palette-win*) b)
                     ;)
             )
             (setf j (+ j 25))))))


   

(defmethod palette-act ((self music-score-palette) x) 
  (unless (call-next-method)
    (when (and (or (= x 7) (= x 8)) (not (= x (mode (editor-assoc self)))))
      (progn 
        (setf (cursor-p (panel (editor-assoc self))) (= x 8))
        (update-panel (panel (editor-assoc self)))
        (setf (mode (editor-assoc self)) x)
        (om-invalidate-view (view *palette-win*) t)
        ))))



