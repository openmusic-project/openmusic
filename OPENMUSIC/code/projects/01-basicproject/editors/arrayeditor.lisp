;openmusic
;
;copyright (c) 1997, 1998, 1999, 2000 by ircam-centre georges pompidou, paris, france.
; 
;this program is free software; you can redistribute it and/or
;modify it under the terms of the gnu general public license
;as published by the Free Software Foundation; either version 2
;of the license, or (at your option) any later version.
;
;see file license for further informations on licensing terms.
;
;this program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;merchantability or fitness for a particular purpose.  see the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;foundation, inc., 59 temple place - suite 330, boston, ma  02111-1307, usa.
;
;authors: gerard assayag and augusto agon


(in-package :om)


;===========================================================
;editor
;===========================================================
;==================================================
(omg-defclass comp-text-enter-view (edit-text-enter-view) ())

(defmethod exit-from-dialog ((self comp-text-enter-view) newtext)
   (let* ((container (editor (om-view-container self)))
           (comp-num (if (equal newtext "") 
                                      (select-comp container) 
                                    (read-from-string newtext))))
     (when (and (integerp comp-num) (>= comp-num 0) (< comp-num (numcols (object container))))
         (setf (select-comp container) comp-num)
         (show-composant (panel container))
         (om-invalidate-view (control-view container) t))
     (setf (text-view container) nil)
     (om-remove-subviews container self)))


(omg-defclass array-controls (3Dborder-view)  
   ((groupname :initform nil :accessor groupname)))


(defmethod initialize-instance :after  ((self array-controls) &key show-cont (grupo "All open"))
  (om-add-subviews self
    (om-make-dialog-item 'om-check-box (om-make-point 25 2) (om-make-point 120 22) "Lines"
                                              :font *controls-font*
                                              :checked-p t
                                              :di-action
                                              (om-dialog-item-act item
                                                (change-lines (om-view-container self) (om-checked-p item))))
    (om-make-dialog-item 'om-check-box (om-make-point 25 20) (om-make-point 120 22) "Grid"
                                              :font *controls-font*
                                              :checked-p nil
                                              :di-action
                                              (om-dialog-item-act item
                                                (loop for v in (bpf-views (panel (om-view-container self))) do
                                                      (setf (grille-p v) (om-checked-p item))
                                                      (om-invalidate-view v)))
                                              ))
  (add-some-controls (om-view-container self) self show-cont grupo)
  (om-set-bg-color self *controls-color*))

;;; NE FAIT RIEN...
;(defmethod om-view-click-handler ((self array-controls) where)
;   (if (and (= (mode (panel (om-view-container self))) 6)
;              (>= (om-point-h where) 50) (<= (om-point-h where) 180))
;       (setf (text-view (editor (om-view-container self)))
;          (om-make-dialog-item 'comp-text-enter-view (om-make-point 150 12) (om-make-point 30 10)
;                                                    (format () "~D" (select-comp (om-view-container self)))
;                                                    :allow-returns t
;                                                    :object (om-view-container self)
;                                                    :container (om-view-container self)
;                                                    :di-selected-p t
;                                                    :font *om-default-font2*))
;       (call-next-method)))

;=======================EDITOR====================
;=================================================

(defvar *size-h-min* 40)
(defvar *h-names-size* 15)

(defparameter *array-ruler-width* 20)

(defclass arrayeditor (editorview object-editor) 
   ((select-comp :initform nil :accessor select-comp)
    (deltay :initform *size-h-min* :accessor deltay)
    (groups :initform nil :accessor groups)
    (cur-group-ind :initform 0 :accessor cur-group-ind)
    (control-view :initform nil :accessor control-view)
    (show-controls-p :initform nil :accessor show-controls-p)))

(defmethod editor-minimum-size ((self arrayeditor))
  (om-make-point 200 100))

(defmethod get-control-h ((self arrayeditor)) 45)


(defclass array-titlebar (editor-titlebar) 
    ((mode-buttons :accessor mode-buttons :initarg :mode-buttons :initform nil)))

(defmethod om-draw-contents ((Self array-titlebar))
  (call-next-method)
  (let* ((comp-str (get-comp-str (object (om-view-container self)) (select-comp (om-view-container self))))
         (str-size (+ (om-string-size comp-str) 10)))
    (om-with-focused-view self 
      (om-draw-string (- (w self) str-size) 16 comp-str))))

(defmethod get-comp-str ((self class-array) comp-num)
  (format nil "Component ~D" comp-num))

(defmethod show-composant ((self array-titlebar))
  (om-invalidate-view self))

(defmethod get-titlebar-class ((self arrayeditor)) 'array-titlebar)

(defmethod editor-null-event-handler :after ((self arrayeditor))
  (do-editor-null-event self))

(defmethod metaobj-scrollbars-params ((self arrayeditor))  '(:v nil))


(defmethod do-editor-null-event ((self arrayeditor))
   (when (and (om-view-contains-point-p (panel self) (om-mouse-position self)) (>= (om-point-h (om-mouse-position self)) *array-ruler-width*))
     (setf (select-comp self) (om-point-h (pixel2point (panel self) (om-mouse-position self))))
     (setf (select-comp self) (min (max 0 (select-comp self)) (- (numcols (object self)) 1)))
     ;(unless (zerop (select-comp self))
       (show-composant (title-bar self))
       ;(when (= (mode (panel self)) 6)
         (show-composant (panel self))
       ; ))
     ))



(defmethod init-titlebar ((self arrayeditor))
  (call-next-method)
  (setf (mode-buttons (title-bar self))
        (append 
         (loop for mode in '(:normal :zoom)
               for icon in '("mousecursor" "zoomcursor")
               for xx = 180 then (+ xx 21) 
               collect 
               (let ((m mode))
                 (om-make-view 'om-icon-button :position (om-make-point xx 2) :size (om-make-point 22 22)
                               :id mode
                               :icon1 icon :icon2 (string+ icon "-pushed")
                               :lock-push t
                               :selected-p (and (panel self) ;;; before initialization...
                                                (equal m (mode (panel self))))
                               :action #'(lambda (item) (set-cursor-mode self m)))
                 ))
         (list (om-make-view 'om-icon-button :position (om-make-point 240 2) :size (om-make-point 22 22)
                               :id :resize
                               :icon1 "resize" :icon2 "resize-pushed"
                               :lock-push nil
                               :action #'(lambda (item) 
                                           (setf (rangex (panel self))
                                                 (get-array-ranges (object self)) ))))
         ))
  (apply 'om-add-subviews (cons (title-bar self) 
                                (mode-buttons (title-bar self)))         
         ))

(defmethod set-cursor-mode ((self arrayeditor) &optional mode)
  (setf (mode (panel self)) mode)
  (update-cursor-mode-buttons (title-bar self)))

(defmethod update-cursor-mode-buttons ((self array-titlebar))
  (loop for button in (mode-buttons self) do
    (setf (selected-p button) (equal (mode (om-view-container self)) (id button))))
  (om-invalidate-view self))



(defmethod om-view-click-handler ((self arrayeditor) where)
  (cond 
   ((text-view self)
    (exit-from-dialog (text-view self) (om-dialog-item-text (text-view self))))
   (t (call-next-method))))

(defmethod create-a-group ((self arrayeditor) name cdr)(groups self)
   (cons name cdr))
   
(defmethod change-group ((self arrayeditor) val)
   (cond
    ((equal val -1)
     (let ((groupname (unique-name-from-list "group " (loop for item in (groups self) collect (car item)) :mode :num)))
       (setf (groups self) (cons  (create-a-group self groupname (cdr (panel-list (panel self))))
                                  (groups self)))
       (update-edition-params self 'panel-list (groups self))
       (setf (cur-group-ind self) 0)
       (update-edition-params self 'cur-group-ind 0)
       (setf (panel-list (panel self)) (copy-list (nth 0 (groups self))))
       (update-ins-panel (panel self) :removeall t)))
    ((equal val -2)
     (let ((groupname (car (nth (cur-group-ind self) (groups self)))))
       (if (or (string-equal "all open" groupname) (string-equal "all close" groupname))
         (om-beep-msg (format nil "Default group ~D can not be deleted." groupname))
         (progn
           (setf (groups self) (remove (nth (cur-group-ind self) (groups self)) (groups self) :test 'equal))
           (update-edition-params self 'panel-list (groups self))
           (setf (cur-group-ind self) 0)
           (update-edition-params self 'cur-group-ind 0)
           (setf (panel-list (panel self)) (copy-list (nth 0 (groups self))))
           (update-ins-panel (panel self) :removeall t)))))
    (t (let ((index (position val (groups self) :test 'equal)))
         (when index
           (setf (cur-group-ind self) index)
           (setf (panel-list (panel self)) (copy-list (nth (cur-group-ind self) (groups self))))
           (update-edition-params self 'cur-group-ind index)
           (update-ins-panel (panel self) :removeall t))))))

(defmethod mode ((self arrayeditor))
   (mode (panel self)))

(defmethod change-show-controls ((self arrayeditor))
   (setf (show-controls-p self) (not (show-controls-p self)))
   (update-ins-panel  (panel self) :removeall t)
   (update-edition-params self 'show-opt-fields (show-controls-p self))
   )

(defmethod change-lines ((self arrayeditor) val)
   (loop for item in (bpf-views (panel self)) do
         (setf (lines-p item) val)
         (om-invalidate-view item t)))


(defmethod load-edition-params ((self arrayeditor))
  (when (ref self)
    (setf (groups self) (get-edit-param self 'panel-list)
          (cur-group-ind self) (get-edit-param self 'cur-group-ind)
          (show-controls-p self) (get-edit-param self 'show-opt-fields))
    ))

(defmethod update-edition-params ((self arrayeditor) param val)
  (when (ref self)
    (set-edit-param (ref self) param val)))

(defmethod get-control-list ((self arrayeditor)) nil)

(defmethod add-some-controls ((self arrayeditor) view show-cont grupo)
  (let* ((showcontrols (om-make-dialog-item 'om-check-box (om-make-point 120 0) (om-make-point 120 44) (format nil "Show Optional~%Fields")
                                          :di-action (om-dialog-item-act item (declare (ignore item)) 
                                                       (change-show-controls self))
                                          :font *controls-font*
                                          :checked-p (show-controls-p self)
                                          ))
         (grouptext (om-make-dialog-item 'om-static-text (om-make-point 280 0) (om-make-point 110 22) "Display Config:"
                                         :font *controls-font*))

         (groupitem (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 390 10) (om-make-point 100 22) ""
                                                                           :font *controls-font*
                                                                           :value grupo
                                                                           :range (let ((items (loop for item in (groups self)
                                                                                                     collect (car item))))
                                                                                    (append (butlast items 2) '("-") (last items 2)))
                                                                           :di-action (om-dialog-item-act item
                                                                                        (let ((i (if (>= (om-get-selected-item-index item)
                                                                                                         (position "-" (om-get-item-list item) 
                                                                                                                   :test 'string-equal))
                                                                                                     (- (om-get-selected-item-index item) 1)
                                                                                                   (om-get-selected-item-index item))))
                                                                                          (change-group self 
                                                                                                        (nth i (groups self)))))))

         (groupbut+ (om-make-dialog-item 'om-button (om-make-point 290 18) (om-make-point 40 20) "+"
                                         :di-action (om-dialog-item-act item
                                                      (change-group self -1)
                                                      (om-set-item-list groupitem (let ((items (loop for item in (groups self)
                                                                                                     collect (car item))))
                                                                                    (append (butlast items 2) '("-") (last items 2))))
                                                      (om-set-selected-item groupitem (car (nth (cur-group-ind self) (groups self)))))
                                         ))
         (groupbut- (om-make-dialog-item 'om-button (om-make-point 325 18) (om-make-point 40 20) "-"
                                         :di-action (om-dialog-item-act item
                                                      (change-group self -2)
                                                      (om-set-item-list groupitem (let ((items (loop for item in (groups self)
                                                                                                     collect (car item))))
                                                                                    (append (butlast items 2) '("-") (last items 2))))
                                                      (om-set-selected-item groupitem (car (nth (cur-group-ind self) (groups self)))))
                                         )))
    
    (setf (groupname view) groupitem)
    
    ;; removed showcontrols
    (om-add-subviews view groupbut+ groupbut- groupitem grouptext)))



(defmethod editor-array-panel-class ((self arrayeditor)) 'arraypanel)

(defmethod edition-params ((self arrayeditor)) 
  (or (edition-params (ref self))
      (setf (edition-params (ref self)) (default-edition-params (ref self)))))

;;; DO NOTHING ?
(defmethod (setf edition-params) (params (self arrayeditor)) 
  (setf (edition-params (ref self)) params))

(defmethod editor-compatible-params-p ((ed1 t) (ed2 arrayeditor)) t)

;;; dans class-editor...
(defmethod edition-params ((self initform-button)) 
  (default-edition-params (val self)))

(defmethod initialize-instance :after ((self arrayeditor) &rest l)
  (declare (ignore l))
  (load-edition-params self)
  (let* ((ed-view (om-make-view (editor-array-panel-class self)
                    :owner self
                    :position (om-make-point 0 *array-ruler-width*) 
                    :scrollbars (first (metaobj-scrollbars-params self))
                    :size (om-make-point (w self) (- (h self) *array-ruler-width*))
                    :panel-list (copy-list (nth (cur-group-ind self) (groups self)))
                    :c-panel-list (get-control-list self)))
         (rulerx (om-make-view 'static-ruler 
                   :owner self
                   :axe 'x
                   :zoom 1
                   :minzoom 1
                   :position (om-make-point 25 (- (h self) *array-ruler-width*)) 
                   :size (om-make-point (- (w self) *array-ruler-width*) *array-ruler-width*)))
         (control (om-make-view 'array-controls 
                    :owner self
                    :position (om-make-point 0 0) 
                    :size (om-make-point 20 25)
                    :grupo (car (nth (cur-group-ind self) (groups self))))))
    (setf (panel self) ed-view)
    (setf (rulerx (panel self)) rulerx)
    (setf (control-view self) control)
    (setf (rangex (panel self)) (get-array-ranges (object self)))
    (update-ins-panel ed-view :removeall t)
    ))

(defmethod update-subviews ((self arrayeditor))
   (when (title-bar self)
     (om-set-view-size  (title-bar self) (om-make-point (w self) *titlebars-h*)))
   (om-set-view-size  (panel self) (om-make-point (w self) (- (h self) (get-control-h self) *titlebars-h* *array-ruler-width*)))
   (om-set-view-position  (panel self) (om-make-point 0 *titlebars-h*))
   (om-set-view-size  (control-view self) (om-make-point (w self) (get-control-h self)))
   (om-set-view-position (control-view self) (om-make-point 0 (- (h self) (get-control-h self))))
   (change-delta-y self)
   (update-subviews (panel self))
   (om-invalidate-view self t))


(defmethod change-delta-y ((self arrayeditor))
   (let* ((panel (panel self))
          (sizebynames (* (length (name-views panel)) *h-names-size*))
          (resto (- (h panel) sizebynames))
          (bpfnum (length (bpf-views (panel self)))))
     (when (> bpfnum 0)
       (setf (deltay self) (max (round resto bpfnum) *size-h-min*)))))



(defun modif-colors (arrayeditor)
  (let* ((panel (panel arrayeditor))
         (views (bpf-views panel))
         (clist (get-edit-param (ref arrayeditor) 'color-list)))
    (if (selected-index panel)
        (let ((new-color (om-choose-color-dialog :color (nth (car (selected-index panel)) clist))))
             (when new-color
               (loop for i in (selected-index panel) do 
                     (fill-bpf-with-color (nth i views) new-color)
                     (setf (nth i clist) new-color)
                     (om-invalidate-view (nth i views) t))
               (set-edit-param (ref arrayeditor) 'color-list clist)))
      (om-beep-msg "Select the views to colorize."))))



;------------------------------------


(omg-defclass arraypanel (om-scroller view-with-ruler-x) 
  ((mode :initform :normal :accessor mode)
   (panel-list :initform nil :initarg :panel-list :accessor panel-list)
   (c-panel-list :initform nil :initarg :c-panel-list :accessor c-panel-list)   ;;; not used anymore...
   (selected-index :initform nil :accessor selected-index)
   (selected-component :initform nil :accessor selected-component)
   (name-views :initform nil :accessor name-views)
   (bpf-views :initform nil :accessor bpf-views))
  (:default-initargs :scrollbars :v :retain-scrollbars t))


(defmethod multibpf? ((self arraypanel)) nil)
(defmethod show-position ((self t)) nil)
(defmethod assoc-w ((self arraypanel)) (- (w self) 25))

(defmethod bpf-panel-class ((self arraypanel)) 'bpf-parameter-panel)
(defmethod list-panel-class ((self arraypanel)) 'list-parameter-panel)


;(defmethod mode ((self arraypanel))
;   (if (= 8 (slot-value self 'mode)) 9 (slot-value self 'mode)))

(defmethod fill-bpf-with-color ((self arraypanel) color)
   (loop for index in (selected-index self) do
         (let ((view (get-subview-by-index self 'bpf index)))
           (when view
           (setf (bpfcolor (currentbpf view)) color)
           (om-invalidate-view view t)))))


(defmethod pixel2point ((self arraypanel) pixel)
  (let* ((x (- (om-point-h pixel) 25))
         (sizex (assoc-w self))
         (durpointx (- (second (rangex self)) (first (rangex self))))
         (x1 (ceiling (* (/ x sizex) durpointx))))
    ;(print (format nil "~D ~D ~D ~f" x sizex durpointx x1))
    (om-make-big-point (+ x1 (first (rangex self))) 0)
    ))


(defmethod show-composant ((self arraypanel))
   (loop for item in (name-views self) do
         (show-composant item)))

(defmethod array-bpf-precision (array type vals) 4)

(defmethod update-ins-panel ((self arraypanel) &key removeall updateref)
  (when removeall
    (change-delta-y (om-view-container self))
    (loop for item in (om-subviews self) do
          (om-remove-subviews self item))
    (let* ((group (cdr (panel-list self)))
           (deltay (deltay (om-view-container self)))
           (cur-y 0)
           (rulerx (rulerx self))
           (thearray (object (om-view-container self))))
      (setf (name-views self) nil)
      (setf (bpf-views self) nil)
      (setf (c-panel-list self) nil)
      (loop for pair-i in group collect
            (let ((i (car pair-i))
                  (open? (second pair-i))) 
              (push (om-make-view 'bar-name 
                      :owner self
                      :index i
                      :position (om-make-point 0 cur-y)
                      :size (om-make-point (w self) *h-names-size*)
                      :control-p (>= i (num-array-slots thearray))  
                      :open? (and open? (show-controls-p (om-view-container self))))
                    (name-views self))
              (incf cur-y *h-names-size*)
              (when open?
                (let* ((row (get-array-row thearray i))
                       (newpanel (new-editor-from-val 
                                  (get-row-bpf thearray row (array-bpf-precision thearray (row-type thearray i) row))
                                 self i nil)))
                  (when newpanel
                    (om-set-view-position  newpanel (om-make-point *array-ruler-width* cur-y))
                    (om-set-view-size newpanel (om-make-point (- (w self) *array-ruler-width*) deltay))
                    (incf cur-y deltay))))))
      
      (setf (name-views self) (reverse (name-views self)))
      (setf (bpf-views self) (sort (bpf-views self) '< :key 'index))
      (setf (assoc-view rulerx) self)))
  (update-subviews (om-view-container self)))

(defmethod update-subviews ((self arraypanel))
   (let* ((group (append (cdr (panel-list self)) (c-panel-list self)))
          (deltay (deltay (om-view-container self)))  (cur-y 0))
     (om-set-view-size (rulerx self) (om-make-point (- (w (om-view-container (rulerx self))) 40) *array-ruler-width*))
     (om-set-view-position (rulerx self) (om-make-point 25 (- (h (om-view-container (rulerx self))) 
                                                              (get-control-h (om-view-container (rulerx self))) *array-ruler-width*)))
     (loop for pair-i in group do
           (let ((open? (second pair-i)))
             (om-set-view-position (get-subview-by-index self 'names (car pair-i)) (om-make-point 0 cur-y))
             (om-set-view-size (get-subview-by-index self 'names (car pair-i))  (om-make-point (w self) *h-names-size*))
             (incf cur-y *h-names-size*)
             (when open?
               (let* ((subview (get-subview-by-index self 'bpf (car pair-i))))
                 (update-panel-array subview self cur-y deltay)
                 (incf cur-y deltay)))))))

(defmethod get-subview-by-index ((self arraypanel) type index)
   (let ((list (if (equal type 'names) (name-views self) (bpf-views self))))
     (find-if  #'(lambda (x) (equal (index x) index)) list)))


(defmethod (setf rangex) ((ranges list) (self arraypanel))
   (setf (slot-value self 'rangex) ranges)
   (loop for item in (bpf-views self) do
         (init-coor-system item))
   (om-invalidate-view (rulerx self) t))

(defmethod handle-key-event ((self arraypanel) char)
  (cond ((equal char #\c)
         (modif-colors (om-view-container self)))
        (t (loop for item in (bpf-views self) do
        (when (selected-p item)
          (handle-key-event item char))))
        ))

;(defmethod open-internal-edit ((self arraypanel) where index)
;  (let* ((select-comp (om-point-h (pixel2point self (om-mouse-position self))))
;         (array (object (om-view-container self)))(ref self)
;         (valor (valor-from-index array index)) rep)
;    (unless (exact-list-p valor)
;      (set-exact-list array index 
;                      (get-points (valor-from-index array index) (row-type array index) array)))
;    
;    (setf rep (nth select-comp (thelist (valor-from-index array index)))) 
;    (if (Class-has-editor-p rep)
;      (push (make-editor-window (get-editor-class rep) rep (format nil "temp editor composant ~D" select-comp)
;                                                           (om-view-container self)) 
;                  (attached-editors (om-view-container self)))
;      (om-beep))))

(defmethod component-n-at ((self arraypanel) where)
  (when (pixel2point self where)
    (max 0 (om-point-h (pixel2point self where)))))

(defmethod open-internal-edit ((self arraypanel) where row)
  (let* ((select-comp (component-n-at self where))
         (array (object (om-view-container self)))
         (val (get-array-val array row select-comp)))   
    (if (Class-has-editor-p val)
      (push (make-editor-window (get-editor-class val) val (format nil "Array Component #~D" select-comp)
                                                           (om-view-container self)) 
                  (attached-editors (om-view-container self)))
      (om-beep))))



(defmethod special-set-ranges ((self arraypanel) new-pi-panel decimals rangebpf)
  (set-ranges new-pi-panel (om*  (rangex self) 
                                 (expt 10 decimals)) 
              (list (third rangebpf) (fourth rangebpf))))


(defmethod update-editor-after-eval ((Self arrayeditor) Val)
  (let ((grille (and (bpf-views (panel self)) (grille-p (car (bpf-views (panel self)))))))
    (setf (object self) val)
    (setf (rangex (panel self)) (get-array-ranges (object self)))
    (load-edition-params self)
    (setf (panel-list (panel self)) (copy-list (nth (cur-group-ind self) (groups self))))
    (update-ins-panel (panel self) :removeall t)
    (when grille (loop for item in (bpf-views (panel self)) do
                       (setf (grille-p item) t)))
    ))


;------------------------------------

(defvar *change-name-view* nil)


;---------------------------------------------------------------------
(omg-defclass bar-name (om-view) 
   ((index :initform 0 :initarg :index :accessor index)
    (control-p :initform nil :initarg :control-p :accessor control-p)
    (at-editor :initform nil  :accessor at-editor)))

(defmethod get-panel ((self bar-name))
   (om-view-container (om-view-container self)))

(defmethod initialize-instance :after  ((self bar-name) &key open?)
   (om-add-subviews self  
     (om-make-view 'button-icon
       :iconid (if open? 165 164)
       :action  #'(lambda (item) 
                    (setf (iconid item) (if (= (iconid item) 164) 165 164))
                    (show/hide-editor (om-view-container item)))
       :position (om-make-point 3 3)
       :size (om-make-point 11 11))))

(defmethod selected-p ((self bar-name)) (member (index self) (selected-index (om-view-container self))))

(defmethod get-fill-color ((self bar-name))
   (cond
    ((selected-p self) *azulote*)
    ((control-p self) *om-light-gray-color*)
    (t  *azulito*)))


(defmethod om-view-click-handler ((self bar-name) where) 
   (declare (ignore where))
   (unless nil ; (control-p self)
     (if (om-shift-key-p)
         (if (member (index self) (selected-index (om-view-container self)))
             (setf (selected-index (om-view-container self)) (remove  (index self) (selected-index (om-view-container self))))
           (push (index self) (selected-index (om-view-container self))))
       (setf (selected-index (om-view-container self))  (list (index self))))
     (update-bar-names (om-view-container self))))



(defmethod update-bar-names ((self arraypanel))
   (loop for item in (name-views  self) do
         (om-invalidate-view item t)))

(defmethod get-name-view ((self bar-name))
  (index2label (object (get-panel self)) (index self)))
             
(defmethod om-draw-contents ((self bar-name))
  (let* ((panel (om-view-container self))
         (y0  (om-point-v (om-scroll-position panel))))
    (when (and (> (y+h  self) y0) (< (y self) (+ y0 (h panel)))) 
        (om-with-focused-view self
          (om-with-font *om-default-font1*
                        (om-with-fg-color self (get-fill-color self)
                          (om-fill-rect 0 0 (w self) (h self)))
                        (om-draw-string 20 12 (get-name-view self))
                        (om-draw-line 0 (h self) (w self) (h self))
                        )
          (call-next-method)))))


(defmethod show-composant ((self bar-name))
  (let* ((array (object (get-panel self)))
         (comp (select-comp (get-panel self)))
         rep)
    (when comp
      (setf rep (get-array-val array (index self) comp))
      (om-with-focused-view self
        (om-with-fg-color self (get-fill-color self)
         (om-fill-rect (- (w self) 150) 2 100 11))
        (om-draw-string (- (w self) 150) 12 (cond
                                ((integerp rep) (format nil "~D" rep))
                                ((numberp rep) (format nil "~,6F" rep))
                                (t "*")))))))

(defmethod show/hide-editor ((self bar-name))
  (let* ((panel (om-view-container self))
         (editor (om-view-container panel))
         (thearray (object (get-panel self)))
         (oldlist (cons (car (panel-list panel)) 
                        (loop for item in (cdr (panel-list panel)) collect (copy-list item))))
         (old-c-list (loop for item in (c-panel-list panel) collect (copy-list item)))
         view-in-question)
    (if (< (index self) (length (cdr oldlist)))
      (setf open? (setf (nth 1 (nth (index self) (cdr oldlist)))
                        (not (nth 1 (nth (index self) (cdr oldlist))))))
      (setf open? (setf (nth 1 (nth (- (index self) (length (cdr oldlist))) old-c-list))
                        (not (nth 1 (nth (- (index self) (length (cdr oldlist))) old-c-list))))))
    (setf (panel-list panel) oldlist)
    (setf (c-panel-list panel) old-c-list)
    (if open?
      (om-without-interrupts  
       (new-editor-from-val (get-row-bpf thearray (get-array-row thearray (index self))) ;; (row-type thearray (index self)) 
                            panel (index self) nil)
       (setf (bpf-views panel) (sort (bpf-views panel) '< :key 'index)))
      (progn
        (setf view-in-question (find-if #'(lambda (x) (equal (index x) (index self))) (bpf-views panel)))
        (remove-extra-editor view-in-question)
        (setf (bpf-views panel) (remove view-in-question (bpf-views panel) :test 'equal)) 
        (om-remove-subviews panel view-in-question)))
    (om-set-view-size editor (om-view-size editor))
    ;(update-subviews panel)
    ))

(defmethod new-editor-from-val ((val bpf) panel index control-p)
  (let* ((newbpf val)
         (rangebpf (give-bpf-range newbpf))
         (new-pi-panel (om-make-view (bpf-panel-class panel)
                         :owner panel
                         :index index
                         :control-p control-p
                         :position (om-make-point 0 0)
                         :size (om-make-point 0 0))))
    (push new-pi-panel (bpf-views panel))
    (setf (rulerx new-pi-panel) 
          (om-make-view 'ruler 
            :assoc-view  new-pi-panel
            :hide-ruler? t
            :axe 'x
            :position (om-make-point 0 0)
            :size (om-make-point 0 0)))
    (setf (bpfcolor newbpf) 
          (nth index (get-edit-param (ref (om-view-container panel)) 'color-list)))
    (setf (currentbpf new-pi-panel) newbpf)
    (setf (rulery new-pi-panel) 
          (om-make-view 'ruler 
            :owner panel
            :assoc-view  new-pi-panel
            :axe 'y
            :position (om-make-point 0 0)
            :size (om-make-point 0 0)))
    (special-set-ranges panel new-pi-panel (decimals newbpf) rangebpf)
    (setf (bpf-views panel) (sort (bpf-views panel) '< :key 'index))
    new-pi-panel))



(defmethod new-editor-from-val ((val null) panel index control-p)
   (let* ((new-panel (om-make-view (list-panel-class panel) 
                       :owner panel
                       :index index
                       :control-p control-p
                       :position (om-make-point 0 0)
                       :size (om-make-point 0 0))))
     (push new-panel (bpf-views panel))
     (setf (rulerx new-panel) (rulerx panel))
     (setf (rangex new-panel) (rangex panel))
     ;(pushr new-panel (assoc-view (rulerx panel)))
     new-panel))

;------------------------------------

(defclass array-parameter-panel () ())

(omg-defclass list-parameter-panel (om-view view-with-ruler-x array-parameter-panel) 
   ((index :initform 0 :initarg :index :accessor index)
    (lines-p :initform nil  :accessor lines-p)
    (control-p :initform nil :initarg :control-p :accessor control-p)
    (minipictslist :initform nil :accessor minipictslist)))

(defvar *minx-for-show* 30)
(defvar *miny-for-show* 30)
(setf *minx-for-show* 10)
(setf *miny-for-show* 10)

(defmethod get-panel ((self list-parameter-panel))    
   (om-view-container (om-view-container self)))

(defmethod om-view-click-handler :before ((self list-parameter-panel) where)
  (setf (selected-index (om-view-container self)) (list (index self)))
  (setf (selected-component (om-view-container self)) (component-n-at (om-view-container self) where))
  (update-bar-names (om-view-container self)))

(defmethod selected-p ((self list-parameter-panel)) 
  (member (index self) (selected-index (om-view-container self))))

(defmethod get-ith-pixvalue ((self list-parameter-panel) i)
  (norme2pixel (om-view-container self) 'x i))

(defmethod get-ith-deltax ((self list-parameter-panel) i)
  (norme2pixel (om-view-container self) 'x 1))

(defmethod om-draw-contents ((self list-parameter-panel))
  (let* ((panel (om-view-container self))
         (array (object (get-panel self)))
         (y0  (om-point-v (om-scroll-position panel)))
         (rowlist (get-array-row array (index self))))
    (when (and (> (y+h  self) y0) (< (y self) (+ y0 (h panel)))) 
      (call-next-method)
      (om-draw-view-outline self)
      (when (grille-p self)
        (draw-grille self))
      (let* ((x (round (w self) (- (second (rangex panel)) (first (rangex panel))))))
        (if (and (>= x *minx-for-show*) (>= (h self) *miny-for-show*))
            (let* ((cur-x 0))
              (om-with-focused-view self
                (loop while (< cur-x (w self))
                      for i = 0 then (+ i 1)
                      for j from 0 to (- (numcols array) 1) 
                      do
                      (let ((obj (nth (+ i (max 0 (car (rangex self)))) rowlist))
                            (x0 (get-ith-pixvalue self i))
                            (delta-x (get-ith-deltax self i)))
                        (om-draw-line x0 0 x0 (h self))
                        (setf cur-x (incf cur-x delta-x))
                        (when (and (selected-component panel) (= i (selected-component panel)))
                          (draw-h-rectangle (list x0 0 (+ x0 delta-x) (h self)) t))
                        (if (class-has-editor-p obj)
                            (draw-obj-in-rect obj x0 (+ x0 delta-x) 0 (h self) (default-edition-params obj) self)
                          (om-with-clip-rect self (om-make-rect  x0 0 (+ x0 delta-x) (h self))
                            (om-draw-string (+ cur-x 3) (round (h self) 2) (format nil "~D" obj))
                            ))
                        )
                    )))
          (om-with-focused-view self
             (om-with-fg-color self *om-light-gray-color*
               (om-fill-rect 0 0 (w self) (h self)))
             (om-with-fg-color self *om-gray-color*
               (om-with-font *om-default-font1*
                             (om-draw-string (- (round (w self) 2) 100) (round (h self) 2) "Too many elements to display...")
                             ))
             )
          )))))

(defmethod om-view-cursor ((Self list-parameter-panel))
   (declare (ignore where))
   (unless (om-command-key-p)
     (if (equal (mode self) :zoom)
       *om-loupe-cursor*
       *om-arrow-cursor*)))

(defmethod om-view-click-handler ((self list-parameter-panel) where)
  (om-invalidate-view (om-view-container self))
  self)


;;; XXX
(defmethod om-view-doubleclick-handler ((self list-parameter-panel) where)
   (open-internal-edit (om-view-container self) where (index self))
   self)

(defmethod remove-extra-editor ((self list-parameter-panel)) nil)

(defmethod init-coor-system ((self list-parameter-panel))
   (setf (rangex self) (rangex (om-view-container self)))
   (om-invalidate-view self t))


(defmethod update-panel-array ((subview list-parameter-panel) self cur-y deltay)
   (om-set-view-position subview  (om-make-point 25 cur-y))
   (om-set-view-size subview  (om-make-point (- (w self) *array-ruler-width*) deltay)))

(defmethod fill-bpf-with-color ((self list-parameter-panel) color) t)

(defmethod get-mini-param ((self list-parameter-panel) param)
   (cdr (assoc param (default-edition-params (make-instance 'note)))))

(defmethod set-mini-param ((self list-parameter-panel) param val) nil)

(defmethod initx ((self list-parameter-panel)) 0) 

(defmethod inity ((self list-parameter-panel)) 0)
     
(defmethod get-mini-zoom  ((self list-parameter-panel)) 0.8)

(defmethod mode ((self list-parameter-panel))
   (mode (om-view-container self)))


;------------------------------------
(omg-defclass bpf-parameter-panel (bpfpanel array-parameter-panel) 
  ((index :initform 0 :initarg :index :accessor index)
   (control-p :initform nil :initarg :control-p :accessor control-p)))

(defmethod bpf-decimals ((self bpf-parameter-panel))
  (decimals (currentbpf self)))

(defmethod get-panel ((self bpf-parameter-panel))
  (om-view-container (om-view-container self)))

(defmethod editor ((self bpf-parameter-panel))
  (om-view-container (om-view-container self)))


(defmethod update-panel ((self bpf-parameter-panel) &optional updateref)
  (change-array-row self)
  (call-next-method))

(defmethod om-get-menu-context ((self bpf-parameter-panel)) nil)

;-----------draw
(defmethod draw-axis ((self bpf-parameter-panel))  nil)


;-----------Edit

(defmethod om-view-click-handler :before ((self bpf-parameter-panel) where)
  (setf (selected-index (om-view-container self)) (list (index self)))
  (update-bar-names (om-view-container self)))

(defmethod handle-key-event ((self bpf-parameter-panel) char)
  (when (or (equal char :om-key-up)
            (equal char :om-key-down)) 
    (call-next-method))) 

(defmethod scroll-point ((Self bpf-parameter-panel) where)
  (setf *bpf-last-click* (om-mouse-position self))
  (setf *bpf-first-click* *bpf-last-click*)
  (setf *bpf-offset-click* 0)
  (om-init-motion-functions self 'make-scroll-point 'release-scroll-point))

(defmethod release-scroll-point ((Self bpf-parameter-panel) Where) 
  ;(change2exact-bpf self)
  (change-array-row self)
  (om-invalidate-view self t)
  (update-panel (editor self) t))

(defmethod make-scroll-point ((Self bpf-parameter-panel) Where)
  (let* ((old-Mouse *bpf-last-click*)
         (first-Mouse *bpf-first-click*)
         (Inity *bpf-offset-click*)
         (new-mouse where)
         (Offy (pixel2norme self 'y (- (om-point-v first-mouse) (om-point-v new-mouse))))
         (moveds (move-points-in-bpf (currentbpf self) (selection? self)  0 (- offy inity ))))
    (if moveds (setf (selection? self) moveds))
    (om-invalidate-view self t)
    (setf *bpf-offset-click*  offy)
    (setq *bpf-last-click* where)))


;(defmethod change2exact-bpf ((self bpf-parameter-panel))  
;  (let* ((thearray (object (get-panel self)))
;         (bpf (currentbpf self))
;         (valor (valor-from-index  thearray (index self))))
;    (if (exact-bpf-p valor)
;      (if (not (equal (thebpf valor) bpf))
;        (set-exact-bpf thearray (index self) bpf))
;      (set-exact-bpf thearray (index self) bpf)
;      )))

(defmethod change-array-row ((self bpf-parameter-panel))  
  (let* ((thearray (object (get-panel self)))
         (bpf (currentbpf self)))
    (setf (nth (index self) (data thearray))
          (y-points bpf))))


(defmethod zoom-system ((self Bpf-Parameter-Panel) where)
  (om-init-motion-functions self 'zoom-system-motion 'zoom-system-release)
  (om-new-movable-object self (om-point-h where) 0 4 (h self) 'om-selection-rectangle))

(defmethod zoom-system-motion ((self Bpf-Parameter-Panel) pos)
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) 0 (max 4 (third rect)) (h self)))))


(defmethod zoom-system-release ((self Bpf-Parameter-Panel) pos)
  (let ((editor (om-view-container self))
        (rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos)))
        user-rect )
    (when rect
      (om-erase-movable-object self)
      (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
      (let* ((x (om-rect-topleft user-rect))
             (x1 (om-rect-bottomright user-rect))
             (dif (om-subtract-points x1 x)))
        (when (and (> (abs  (om-point-h dif)) 10) (> (abs (om-point-v dif)) 10))
          (let ((new-point (pixel2point editor x))
                (new-point1 (pixel2point editor x1)))
            (when  (>  (om-point-h new-point1) (+  (om-point-h new-point) 1)) 
              (setf (rangex editor) (list (om-point-h new-point) (om-point-h new-point1))))))))))



(defmethod x-class-move-point ((self bpf-parameter-panel)) 'om-static-text)

(defmethod update-panel-array ((subview bpf-parameter-panel) self cur-y deltay)
   (om-set-view-position subview  (om-make-point 25 cur-y))
   (om-set-view-size subview  (om-make-point (- (w self) 25) deltay))
   (om-set-view-position (rulery subview)  (om-make-point 0 cur-y))
   (om-set-view-size (rulery subview)  (om-make-point 25 deltay)))

(defmethod init-coor-system ((self bpf-parameter-panel))
  (let* ((bpf (get-bpf-obj self))
         (rangebpf (give-bpf-range bpf))
         (panel (om-view-container self)))
    (special-set-ranges panel self (decimals bpf) rangebpf)
    (om-invalidate-view self)))


(defmethod set-ranges ((self bpf-parameter-panel) rangex rangey)
   (let* ((inty (- (second rangey) (first rangey)))
          (delta (round inty 5)))
     (setf (rangex self) rangex)
     (setf (rangey self) (list (- (first rangey) delta) (+ (second rangey) delta)))
     (set-units-ruler self (rulery self))
     (set-units-ruler self (rulerx self))))



(defmethod selected-p ((self bpf-parameter-panel))
   (member (index self) (selected-index (om-view-container self))))

(defmethod get-bpf-obj ((self bpf-parameter-panel))  (currentbpf self) )

(defmethod get-string-nom  ((self bpf-parameter-panel)  num (axe (eql 'x)))  (format () "~D" num))

(defmethod remove-extra-editor ((Self Bpf-Parameter-Panel))
   (om-remove-subviews (om-view-container (rulery self)) (rulery self))
   (setf (rulery self) nil))

(defmethod om-view-click-handler ((Self Bpf-Parameter-Panel) Where)
   (cond 
    ((equal (mode self) :normal) (select-system self where))
    ((equal (mode self) :zoom) (zoom-system self where))
   ))

(defmethod mode ((Self Bpf-Parameter-Panel))
   (mode (om-view-container self)))

(defmethod om-draw-contents ((self Bpf-Parameter-Panel))
  (let* ((panel (om-view-container self))
         (y0  (om-point-v (om-scroll-position panel))))
    (when (and (> (y+h  self) y0) (< (y self) (+ y0 (h panel)))) 
      (call-next-method))))
 
; not used
(defmethod draw-grille  ((self list-parameter-panel)) 
  (om-with-focused-view self
    (om-with-dashline '(1 3)
     (let* ((assoc-view (om-view-container self))
            (rulerx (rulerx assoc-view))
           (system-etat (get-system-etat assoc-view))
            i j ppu unit )
       (setf unit (zoom rulerx))
       (setf i (first (rangex assoc-view)))
       (setf i (if (minusp i) (+ i (- unit (mod i unit))) (+ i (mod i unit))))
       (setf j (second (rangex assoc-view)))
       (setf ppu (norme2pixel assoc-view 'x unit)) 
       (loop while (< i j) do
             (let* ((pixel (point2pixel assoc-view (om-make-big-point i 0) system-etat)))
               (cond
                ((and  (> (* 10 ppu) 10) (zerop (mod i (* 10 unit))))
                 (draw-grille-linex assoc-view (om-point-h pixel) 0))
                ((>= ppu  20)
                 (draw-grille-linex assoc-view (om-point-h pixel) 0)))
               (incf i unit)))))))





