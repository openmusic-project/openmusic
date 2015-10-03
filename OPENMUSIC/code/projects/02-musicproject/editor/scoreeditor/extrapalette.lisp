(in-package :om)

;;;============================
;;; EXTRA PALETTE :
(defvar *extramanager* nil)

(defclass extra-palette-win (om-windoid) 
  ((extramanager :initform nil :initarg :extramanager :accessor extramanager)
   (buttons :initform nil :initarg :buttons :accessor buttons)
   (extraitems :initform nil :initarg :extraitems :accessor extraitems)
   (preview :initform nil :initarg :preview  :accessor preview)))

;;; edit-mode = :text :pict :dyn :graphics :lines :fig
(defclass extramanager ()
  ((win :initform nil  :accessor win)
   (winpos :initform nil  :accessor winpos)
   (show :initform nil  :accessor show)
   (edit-mode :initform nil  :accessor edit-mode)
   (params :initform nil  :accessor params)
   (current-editor :initform nil :accessor current-editor :initarg :current-editor)))

(defmethod om-window-close-event :after ((self extra-palette-win))
  (setf (winpos *extramanager*) (om-view-position self))
  (setf (win *extramanager*) nil)
  (setf (show *extramanager*) nil)
  (setf (edit-mode *extramanager*) nil)
  (setf (current-editor *extramanager*) nil)
  ;(when (om-front-window) (om-add-menu-to-win (om-front-window)))
  )

;;(push 'extrapal *palettes*)

;;; automatic call on window activate
(defmethod open-win-palettes ((pal (eql 'extrapal)) editor)
  (unless *extramanager* (setf *extramanager* (make-instance 'extramanager)))  
  ;(when (and *extramanager* (win *extramanager*))
  ;  (om-hide-window (win *extramanager*)))
  (show-extrapalette-win editor))

;; menu call
(defun show-extra-palette (editor) 
  (unless *extramanager* (setf *extramanager* (make-instance 'extramanager)))
  (setf (show *extramanager*) t)
  (show-extrapalette-win editor))

(defmethod close-win-palettes ((pal (eql 'extrapal)) editor)
  (when (and *extramanager* (win *extramanager*))
    (let ((show? (show *extramanager*)))
      (om-close-window (win *extramanager*))
      (setf (show *extramanager*) show?))))

;;; enables or not the menu
(defun show-extra-palette-enabled ()
  (or (not *extramanager*)
      (not (win *extramanager*))))


(defmethod show-extrapalette-win ((self t)) nil)

(defmethod show-extrapalette-win ((self scoreeditor)) 
  (when (and *extramanager* (show *extramanager*) (= (score-mode (panel self)) 0))
    (if (win *extramanager*) (om-select-window (win *extramanager*))
      (progn 
        (setf (win *extramanager*)
              (om-make-window 'extra-palette-win :window-title "Extra Edition Palette"
                              :extramanager *extramanager*
                              :resizable nil :close nil
                              :position (or (winpos *extramanager*)
                                            (om-make-point (x (window self))
                                                                (+ (y (window self))
                                                                   (h (window self))
                                                                   25)
                                                                )
                                            (om-make-point 100 400))
                              :size (om-make-point 182 50)))
        (add-buttons (win *extramanager*))
        (setf (edit-mode *extramanager*) nil)
        (setf (current-editor *extramanager*) self)
        ))
    ))




(defmethod add-buttons ((self extra-palette-win))
  (setf (buttons self)
                  (list 
                   ;;; nil
                   (om-make-view 'om-icon-button
                                 :lock-push t
                                 :position (om-make-point 0 0)
                                 :size (om-make-point 26 25)
                                 :action #'(lambda (item)
                                             (extra-action item nil))
                                 :icon1 "mousecursor"
                                 :owner self)
                   ;vel
                   (om-make-view 'om-icon-button
                                 :lock-push t
                                 :position (om-make-point 26 0)
                                 :size (om-make-point 26 25)
                                 :action #'(lambda (item)
                                             (extra-action item :dyn))
                                 :icon1 "vel"
                                 :owner self)
                   
                   ;;; figure
                   (om-make-view 'om-icon-button
                                 :lock-push t
                                 :position (om-make-point 52 0)
                                 :size (om-make-point 26 25)
                                 :action #'(lambda (item)
                                             (extra-action item :figure))
                                 :icon1 "fig"
                                 :owner self)
                   ;;; lines
                   (om-make-view 'om-icon-button
                                 :lock-push t
                                 :position (om-make-point 78 0)
                                 :size (om-make-point 26 25)
                                 :action #'(lambda (item)
                                             (extra-action item :lines))
                                 :icon1 "slur"
                                 :owner self)
                   ;;; pict
                   (om-make-view 'om-icon-button
                                 :lock-push t
                                 :position (om-make-point  104 0)
                                 :size (om-make-point 26 25)
                                 :action #'(lambda (item)
                                             (extra-action item :pict))
                                 :icon1 "picture"
                                 :owner self)
                   ;;; text
                   (om-make-view 'om-icon-button
                                 :lock-push t
                                 :position (om-make-point 130 0)
                                 :size (om-make-point 26 25)
                                 :action #'(lambda (item)
                                             (extra-action item :text))
                                 :icon1 "text"
                                 :owner self)
                   ;;; graphics
                   (om-make-view 'om-icon-button
                                 :lock-push t
                                 :position (om-make-point 156 0)
                                 :size (om-make-point 26 25)
                                 :action #'(lambda (item)
                                             (extra-action item :graphics))
                                 :icon1 "shapes"
                                 :owner self)
                   )))


(omg-defclass extra-preview (om-view) ())

(defmethod om-draw-contents ((self extra-preview))
  (let* ((value (edit-mode *extramanager*))
         (params (get-extra-param *extramanager* value)))
    (case value
     (:figure
       ;;; char size color
       (om-with-focused-view self
         (om-with-font (om-make-music-font *extras-font* (nth 1 params))
                       (om-with-fg-color self (nth 2 params) 
                         (om-draw-string (round (w self) 2) 30 (string (nth 0 params))))))
      )
      (:lines 
       ;;; fig dash linesize color
       (om-with-focused-view self
       (om-with-line-size (nth 2 params)
         (om-with-fg-color self (nth 3 params) 
             (if (equal (nth 1 params) 'dash)
                 (om-with-dashline 
                   (case (nth 0 params) 
                     ('slur (om-draw-ellipse-arc  30 20 (- (w self) 60) 28 0 pi))
                     ('decresc (om-draw-line 20 20 (- (w self) 20) 30)
                               (om-draw-line 20 40 (- (w self) 20) 30))
                     ('cresc (om-draw-line 20 30 (- (w self) 20) 20)
                             (om-draw-line 20 30 (- (w self) 20) 40))
                     ('brack (om-draw-line 20 20 (- (w self) 20) 20)
                             (om-draw-line 20 20 20 30)
                             (om-draw-line (- (w self) 20) 20 (- (w self) 20) 30))
                     ))
               (case (nth 0 params) 
                     ('slur (om-draw-ellipse-arc  30 20 (- (w self) 60) 28 0 pi))
                     ('decresc (om-draw-line 20 20 (- (w self) 20) 30)
                               (om-draw-line 20 40 (- (w self) 20) 30))
                     ('cresc (om-draw-line 20 30 (- (w self) 20) 20)
                             (om-draw-line 20 30 (- (w self) 20) 40))
                     ('brack (om-draw-line 20 20 (- (w self) 20) 20)
                             (om-draw-line 20 20 20 30)
                             (om-draw-line (- (w self) 20) 20 (- (w self) 20) 30))
                     )
               )))))
      (:pict
       ;;; pict
       nil)
      (:text
       ;;; font color
       (om-with-focused-view self
         (om-with-font (nth 0 params) 
           (om-with-fg-color self (nth 1 params) 
             (om-draw-string 30 30 "Text")))))
     (:graphics
       ;;; fig dash linesize color fill
       (om-with-focused-view self
       (om-with-line-size (nth 2 params)
         (om-with-fg-color self (nth 3 params) 
             (if (equal (nth 1 params) 'dash)
                 (om-with-dashline 
                   (case (nth 0 params) 
                     ('circ (if (nth 4 params) 
                                  (om-fill-ellipse (round (w self) 2) (round (h self) 2)
                                                   (round (w self) 4) (round (h self) 4))
                                (om-draw-ellipse (round (w self) 2) (round (h self) 2)
                                                 (round (w self) 4) (round (h self) 4))))
                     ('rect (if (nth 4 params) 
                                (om-fill-rect 30 15 (- (w self) 60) (- (h self) 30))
                                (om-draw-rect 30 15 (- (w self) 60) (- (h self) 30))))
                     ('line (om-draw-line 20 20 (- (w self) 20) (- (h self) 20)))
                     ('polyg (if (nth 4 params) 
                                (oa::om-fill-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60)))
                                (oa::om-draw-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60))))
                             )
                     ))
               (case (nth 0 params) 
                 ('circ (if (nth 4 params) 
                              (om-fill-ellipse (round (w self) 2) (round (h self) 2)
                                               (round (w self) 4) (round (h self) 4))
                            (om-draw-ellipse (round (w self) 2) (round (h self) 2)
                                             (round (w self) 4) (round (h self) 4))
                            ))
                 ('rect (if (nth 4 params) 
                            (om-fill-rect 30 15 (- (w self) 60) (- (h self) 30))
                          (om-draw-rect 30 15 (- (w self) 60) (- (h self) 30))))
                 ('line (om-draw-line 20 20 (- (w self) 20) (- (h self) 20)))
                 ('polyg (if (nth 4 params) 
                                (oa::om-fill-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60)))
                                (oa::om-draw-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60))))
                             )
                 )
               )))))
     (otherwise nil))))


(defun preview-p (value)
  (or (equal value :graphics)
      (equal value :lines)
      (equal value :figure)
      (equal value :text)))

(defun add-extra-preview (win)
  (setf (preview win) (om-make-view 'extra-preview :position (om-make-point 20 (h win))
                                    :size (om-make-point (- (w win) 40) 60)))
  (om-set-view-size win (om-make-point (om-point-h (om-interior-size win)) (+ (h win) 70)))
  (om-add-subviews win (preview win)))

(defun extra-action (button value)

  (let ((win (om-view-container button)))
      (unless (and (equal (edit-mode *extramanager*) value)
                   (extraitems win))
        (loop for b in (buttons win) do
              (when (selected-p b)
                (setf (selected-p b) nil)
                (om-invalidate-view b t)))
        (when (extraitems win) (mapc #'(lambda (sv) (om-remove-subviews win sv)) (extraitems win)))
        (when (preview win) (om-remove-subviews win (preview win)))
        (setf (selected-p button) t)
        (om-invalidate-view button t)
        (setf (edit-mode *extramanager*) value)
        (when (and value (not (get-extra-param *extramanager* value)))
          (set-extra-param *extramanager* value (extraedit-def-params value)))
        (let ((extraedit (get-extra-items value)))
          (om-set-view-size win (om-make-point (om-point-h (om-interior-size win)) (car extraedit)))
          (setf (extraitems win) (cdr extraedit))
          (when (extraitems win) (mapc #'(lambda (sv) (om-add-subviews win sv)) (extraitems win)))
          (when (preview-p value) (add-extra-preview win))
          ))))

;(set-extra-param (params *extramanager*) :graphics)
;(set-extra-param *extramanager* :graphics (extraedit-def-params :graphics))

(defun get-extra-param (em id)
  (when (find id (params em) :key 'car)
    (cadr (find id (params em) :key 'car))))

(defun set-extra-param (em id val)
  (let ((pos (position id (params em) :key 'car)))
    (if pos
        (setf (cadr (nth pos (params em))) val)
      (push (list id val) (params em)))))

; (setf (params *extramanager*) nil)
(defun extraedit-def-params (value)
  (case value
    (:dyn
     ;;; size char color
     (list #\- ; (elt (dyn-f) 0) 
           12 *om-black-color*))
    (:figure
     ;;; size char color
     (list (code-char 113) 12 *om-black-color*))
    (:lines 
     ;;; fig dash linesize color
     (list 'slur 'plain 1 *om-black-color*))
    (:pict
     ;;; pict
     (list nil))
    (:text
     ;;; font color
     (list *om-default-font1* *om-black-color*))
    (:graphics
      ;;; fig dash linesize color fill
      (list 'rect 'plain 1 *om-black-color* nil))))



(defmethod get-extra-items ((value t)) 
  (list 50))

(defmethod get-extra-items ((value (eql :pict))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 100 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Picture Extra"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 40 55)
                               (om-make-point 100 20) "(not available)"
                               :fg-color *om-gray-color*
                               :font *om-default-font1*))
    ))

(defmethod get-extra-items ((value (eql :graphics))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 210 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Graphic Extra"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 60)
                               (om-make-point 100 20) "Shape"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 55)
                               (om-make-point 100 20) ""
                               :range '("Rectangle" "Circle" "Line" "Polygon")
                               :value (nth (position (nth 0 params) '(rect circ line polyg)) '("Rectangle" "Circle" "Line" "Polygon"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 0 params) (nth (om-get-selected-item-index item) '(rect circ line polyg)))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 90)
                               (om-make-point 100 20) "Line"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 85)
                               (om-make-point 100 20) ""
                               :range '("Normal" "Dashed")
                               :value (nth (position (nth 1 params) '(plain dash)) '("Normal" "Dashed"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 1 params) (nth (om-get-selected-item-index item) '(plain dash)))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 120)
                               (om-make-point 100 20) "Pen Size"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 115)
                               (om-make-point 100 20) ""
                               :range '("1" "2" "3" "4" "5" "6" "7" "8")
                               :value (nth (position (nth 2 params) '(1 2 3 4 5 6 7 8)) '("1" "2" "3" "4" "5" "6" "7" "8"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 2 params) (nth (om-get-selected-item-index item) '(1 2 3 4 5 6 7 8)))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 150)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          (om-make-view 'om-color-view :position (om-make-point 75 150)
                               :size (om-make-point 80 16) 
                               :color (nth 3 params)
                               :after-fun #'(lambda (item)
                                            (let ((c (color item)))
                                              (when c 
                                                (setf (nth 3 params) c)
                                                (set-extra-param *extramanager* value params)
                                                (om-invalidate-view (preview (win *extramanager*)))))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 180)
                               (om-make-point 100 20) "Fill"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-check-box (om-make-point 70 175)
                               (om-make-point 100 20) ""
                               :checked-p (nth 4 params)
                               :di-action (om-dialog-item-act item
                                            (setf (nth 4 params) (om-checked-p item))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))))
          )))

(defmethod get-extra-items ((value (eql :lines))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 190 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Connections"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 60)
                               (om-make-point 100 20) "Shape"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 55)
                               (om-make-point 100 20) ""
                               :range '("Slur" "Crescendo" "Decrescendo" "Bracket")
                               :value (nth (position (nth 0 params) '(slur cresc decresc brack)) 
                                           '("Slur" "Crescendo" "Decrescendo" "Bracket"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 0 params) (nth (om-get-selected-item-index item) '(slur cresc decresc brack)))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 90)
                               (om-make-point 100 20) "Line"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 85)
                               (om-make-point 100 20) ""
                               :range '("Normal" "Dashed")
                               :value (nth (position (nth 1 params) '(plain dash)) '("Normal" "Dashed"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 1 params) (nth (om-get-selected-item-index item) '(plain dash)))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 120)
                               (om-make-point 100 20) "Pen Size"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 115)
                               (om-make-point 100 20) ""
                               :range '("1" "2" "3" "4" "5" "6" "7" "8")
                               :value (nth (position (nth 2 params) '(1 2 3 4 5 6 7 8)) '("1" "2" "3" "4" "5" "6" "7" "8"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 2 params) (nth (om-get-selected-item-index item) '(1 2 3 4 5 6 7 8)))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 150)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          (om-make-view 'om-color-view :position (om-make-point 75 152)
                               :size (om-make-point 80 16) 
                               :color (nth 3 params)
                               :after-fun #'(lambda (item)
                                            (let ((c (color item)))
                                              (when c 
                                                (setf (nth 3 params) c)
                                                (set-extra-param *extramanager* value params)
                                                (om-invalidate-view (preview (win *extramanager*)))))))
          
         )))


(defmethod get-extra-items ((value (eql :text))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 130 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Text Extra"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 60)
                               (om-make-point 100 20) "Font"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-button (om-make-point 70 58)
                               (om-make-point 90 20) "Choose"
                               :di-action (om-dialog-item-act item
                                            (let ((f (om-choose-font-dialog :font (nth 0 params))))
                                              (when f 
                                                (setf (nth 0 params) f)
                                                (set-extra-param *extramanager* value params)
                                                (om-invalidate-view (preview (win *extramanager*)))))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 90)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          
          (om-make-view 'om-color-view :position (om-make-point 75 92)
                               :size (om-make-point 80 16) 
                               :color (nth 1 params)
                               :after-fun #'(lambda (item)
                                            (let ((c (color item)))
                                              (when c 
                                                (setf (nth 1 params) c)
                                                (set-extra-param *extramanager* value params)
                                                (om-invalidate-view (preview (win *extramanager*)))))))
    )))

(defmethod get-extra-items ((value (eql :figure))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 100 
          (om-make-dialog-item 'om-static-text (om-make-point 20 30)
                               (om-make-point 160 20) "Articulation Symbols"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 50 60)
                               (om-make-point 70 20) ""
                               :range (loop for i from 113 to 125 collect (string (code-char i)))
                               :value (string (nth 0 params))
                               :font (om-make-music-font *extras-font* 24)
                               :di-action (om-dialog-item-act item
                                            (setf (nth 0 params) (elt (om-get-selected-item item) 0))
                                            (set-extra-param *extramanager* value params)
                                            (om-invalidate-view (preview (win *extramanager*)))
                                            ))
          )))

(defmethod get-extra-items ((value (eql :dyn))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 100 

          (om-make-dialog-item 'om-static-text (om-make-point 45 30)
                               (om-make-point 160 20) "Extra Dynamics"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 50 55)
                               (om-make-point 70 20) ""
                               :range (append '("unspecific" "-") (mapcar #'(lambda (d) (string (cadr d))) *dynamics-list*))
                               :value (string (nth 0 params))
                               :font (om-make-music-font *extras-font* 24)
                               :di-action (om-dialog-item-act item
                                            (setf (nth 0 params) (elt (om-get-selected-item item) 0))
                                            (set-extra-param *extramanager* value params))))))


(defmethod get-extra-items ((value (eql nil)))
  (let ((params (cadr (find value (params *extramanager*) :key 'car)))
        (heads-list (append (list (string (code-char 110)) (string (code-char 81)) (string (code-char 80)))
                                              (loop for i from 94 to 105 collect (string (code-char i))))))
    (list 180
          (om-make-dialog-item 'om-static-text (om-make-point 20 30)
                               (om-make-point 160 20) "Score Selection"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 60)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          (om-make-view 'om-color-view :position (om-make-point 75 62)
                               :size (om-make-point 80 16) 
                               :color *om-black-color*
                               :after-fun #'(lambda (item)
                                              (let ((sel (selection? (panel (current-editor *extramanager*)))))
                                                (if sel
                                                    (let ((c (color item)))
                                                      (when c 
                                                        (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                              (set-mus-color obj c))
                                                        (update-panel (panel (current-editor *extramanager*)))))
                                                  (om-beep)))))
          
          (om-make-dialog-item 'om-static-text (om-make-point 10 90)
                               (om-make-point 100 20) "Heads"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 80)
                               (om-make-point 70 20) ""
                               :range heads-list
                               :value (string (code-char 110))
                               :font (om-make-music-font *heads-font* 20)
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                              (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                    (if (= (om-get-selected-item-index item) 0)
                                                        (delete-extras (get-extras obj "head"))
                                                        (add-head-extra obj (nth (om-get-selected-item-index item) heads-list))))
                                              (update-panel (panel (current-editor *extramanager*))))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 130)
                               (om-make-point 100 20) "Dynamics"
                               :font *om-default-font1*)
          
          (om-make-dialog-item 'om-button (om-make-point 70 118)
                               (om-make-point 65 20) "Show"
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                              (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                    (add-vel-extra obj))
                                              (update-panel (panel (current-editor *extramanager*)))
                                            )))
          (om-make-dialog-item 'om-button (om-make-point 70 140)
                               (om-make-point 65 20) "Hide"
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                            (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                    (delete-extras (get-extras obj "vel")))
                                            (update-panel (panel (current-editor *extramanager*)))
                                            )))
    )))
  
