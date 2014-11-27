;OpenMusic
;
;Copyright (C) 1997, 1998, 1999, 2000 by IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Authors: Gerard Assayag and Augusto Agon

(in-package :om)


;===========================================================
;EDITOR
;===========================================================
   
(defclass bpfeditor (editorview object-editor) 
   ((multibpf? :initform nil :accessor multibpf?)
    (control :initform nil :accessor control)
    (pict :initform nil :accessor pict)
    (current-point :initform nil :accessor current-point)
    (spline :initform nil :accessor spline)))



;=============Controls===========
(defclass control-bpf (3Dborder-view) 
              ((precisionitem :initform nil :accessor precisionitem)
               (lineitem :initform nil :accessor lineitem)))


(defmethod initialize-instance :after  ((Self Control-Bpf) &key)
  (om-add-subviews self
                  (setf (lineitem  self)
                        (om-make-dialog-item 'om-check-box (om-make-point 25 2) (om-make-point 60 16) "Lines"
                                             :di-action  (om-dialog-item-act item
                                                           (setf (lines-p (panel (om-view-container  (om-view-container item))))
                                                                 (om-checked-p item))
                                                           (om-invalidate-view (panel (om-view-container (om-view-container item))) t))
                                             :font *om-default-font1*
                                             :checked-p nil 
                                             ))
                   
                 
                   (om-make-dialog-item 'om-check-box (om-make-point 25 20) (om-make-point 60 16) "Grid"
                                        :di-action (om-dialog-item-act item
                                                     (let ((pane (panel (om-view-container (om-view-container item)))))
                                                       (setf (grille-p pane) (om-checked-p item))
                                                       (if (grille-p pane)
                                                           (draw-grille pane)
                                                         (om-invalidate-view pane))
                                                       ))
                                        :font *om-default-font1*
                                        :checked-p nil
                                        )

                   (setf (precisionitem self) (om-make-dialog-item 'numbox (om-make-point 340 4) (om-make-point 30 18) (format nil " ~D" 0)
                                        :di-action nil
                                        :font *om-default-font1*
                                        :bg-color *om-white-color*
                                        :value 0
                                        :afterfun #'(lambda (item)
                                                      (editor-change-precision (om-view-container self) (value item)))
                                        :min-val 0
                                        :max-val 10))
                   (om-make-dialog-item 'om-static-text (om-make-point 220 4) (om-make-point 120 18) "Precision (decimals)"
                                         :font *om-default-font1*
                                         :bg-color *controls-color*)
                   )
  (om-set-bg-color self *controls-color*))

                                                   
(defmethod add-multibpf-controls ((self Control-Bpf))
  (om-add-subviews self
              (om-make-dialog-item 'om-check-box (om-make-point 92 2) (om-make-point 100 20) "Background"
                                     :di-action (om-dialog-item-act item
                                      (setf (show-back-p (panel (om-view-container  (om-view-container  item))))
                                        (om-checked-p item))
                                      (om-invalidate-view (panel (om-view-container  (om-view-container  item))) t))
                                     :checked-p t
                                     :font *om-default-font1*
                                     :bg-color *controls-color*
                                     )) )

(defmethod editor ((self control-bpf)) 
   (om-view-container self))

(defmethod om-view-click-handler ((Self Control-Bpf) Where)
   (if (text-view (editor self))
     (exit-text-view (editor self))
   (if (and (>= (om-point-h where) 100) (<= (om-point-h where) 120))
     (when (multibpf? (om-view-container  self))
       (om-add-subviews (om-view-container  self)
         (setf (text-view (om-view-container  self))
               (om-make-dialog-item 'bpf-text-enter-view (om-make-point 100 12) (om-make-point 20 10)
                 (format () "~D" (position 
                                                    (currentbpf (panel (om-view-container  self)))
                                  (bpf-list (object (om-view-container  self)))))
                                    :object (om-view-container  self)
                                                         :font *om-default-font1*))))
     (call-next-method))))

;==================================================
(defclass bpf-text-enter-view (edit-text-enter-view) ())


(defmethod exit-from-dialog ((Self Bpf-Text-Enter-View) Newtext)
  (let ((Newpos (read-from-string newtext))
        (Container (om-view-container  self)) Temp)
    (when (and (integerp newpos) (>= newpos 0) (< newpos (length (bpf-list (object container)))))
      (setf temp (nth newpos (bpf-list (object container))))
      (setf (nth (position (currentbpf (panel container)) (bpf-list (object container))) (bpf-list (object container)))
            temp)
      (setf (nth newpos (bpf-list (object container))) (currentbpf (panel container)))
      (om-invalidate-view (control container) t))
    (setf (text-view container) nil)
    (om-remove-subviews container self)))


;;;=== TITLE BAR ===

(defclass bpf-titlebar (editor-titlebar) 
  ((mode-buttons :accessor mode-buttons :initform nil)))

 
(defmethod init-titlebar ((self bpfeditor))
  (call-next-method)
  (setf (mode-buttons (title-bar self))
        (append 
         (loop for mode in '(:normal :pen :zoom :scroll :move)
               for icon in '("mousecursor" "pencursor" "zoomcursor" "handcursor" "handbpfcursor")
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
         (list (om-make-view 'om-icon-button :position (om-make-point 300 2) :size (om-make-point 22 22)
                               :id :resize
                               :icon1 "resize" :icon2 "resize-pushed"
                               :lock-push nil
                               :action #'(lambda (item) (init-coor-system (panel self)))))
         ))
  (apply 'om-add-subviews (cons (title-bar self) 
                                (mode-buttons (title-bar self)))         
         ))

(defmethod set-cursor-mode ((self bpfeditor) &optional mode)
  (setf (mode (panel self)) mode)
  (update-cursor-mode-buttons (title-bar self)))

(defmethod update-cursor-mode-buttons ((self bpf-titlebar))
  (loop for button in (mode-buttons self) do
    (setf (selected-p button) (equal (mode (om-view-container self)) (id button))))
  (om-invalidate-view self))


(defmethod om-draw-contents :before ((Self bpf-titlebar)) 
  (let ((editor (om-view-container self)))
  (when (multibpf? editor)
    (om-with-focused-view self
      (om-with-font *om-default-font1*
                    (om-draw-string (- (w self) 200) 11 (format nil "~A ~D" 
                                                                (string-upcase (class-name (class-of (currentbpf (panel editor)))))
                                                                (position (currentbpf (panel editor))
                                                                          (bpf-list (object editor)))))
                    
                    (om-draw-string (- (w self) 200) 22 (or (name (currentbpf (panel editor))) ""))
                    
                    )
      ))
  (when (current-point editor) (draw-position self (current-point editor)))
  ))


(defmethod draw-position ((Self bpf-titlebar) point)
  (let* ((editor (om-view-container self))
         (decimals (bpf-decimals (panel editor)))
         (xwid 90))
    (when point
      (om-with-focused-view self
        (om-with-font *rulery-font*
      (if (zerop decimals)
          (progn (om-draw-string (- (w self) xwid) 10 (format () "x  ~D" (om-point-h point)))
            (om-draw-string (- (w self) xwid) 20 (format () "y  ~D" (om-point-v point))))
        (progn (om-draw-string (- (w self) xwid) 10 (format () (format nil "x  ~a,~DF" "~" decimals) 
                                                            (/ (om-point-h point) (expt 10.0 decimals))))
          (om-draw-string (- (w self) xwid) 20 (format () (format nil "y  ~a,~DF" "~" decimals) 
                                                       (/ (om-point-v point) (expt 10.0 decimals))))))
      )))))


;------------------------------------
;BPF EDITOR Class definition and initialization
;------------------------------------

(defmethod get-bg-pict ((self bpfeditor)) (pict self))
(defmethod get-bg-pict ((self t)) nil)


(defmethod get-titlebar-class ((self bpfeditor)) 'bpf-titlebar)

(defmethod editor-null-event-handler :after ((Self Bpfeditor)) 
  (do-editor-null-event self))

(defmethod editor-select-all ((Self Bpfeditor))
   (setf (selection? (panel self)) t)
   (om-invalidate-view (panel self) t))


(defmethod mode ((Self Bpfeditor))
   (mode (panel self)))

(defmethod record-new-bpf ((Self Bpfeditor))
   (setf (value (ref self)) (object self)))

(defmethod get-panel-class ((Self Bpfeditor)) 'bpfPanel)
(defmethod get-control-class ((Self Bpfeditor)) 'control-bpf)

(defmethod get-control-h ((self Bpfeditor)) 45)

(defmethod initialize-instance :after ((Self Bpfeditor) &rest L) 
   (declare (ignore l))
   (unless (bpf-p (object self))
     (setf (multibpf? self) t))
   (let* ((ch (get-control-h self))
          (Panel (om-make-view (get-panel-class self) 
                     :position (om-make-point 25 (get-control-h self)) 
                     :size (om-make-point (- (w self) 25) (- (h self) (+ 25 (get-control-h self))))))
          (Control (om-make-view (get-control-class self) 
                     :position (om-make-point 0 0) 
                     :size (om-make-point (w self) (get-control-h self))))
          (Rulery (om-make-view 'ruler 
                    :axe 'y
                    :assoc-view panel
                    :position (om-make-point 0 (get-control-h self)) 
                    :size (om-make-point 25 (- (h self) (+ 25 (get-control-h self))))))
          (Rulerx (om-make-view 'ruler 
                    :axe 'x
                    :assoc-view panel
                    :position (om-make-point 25 (- (h self) 25)) 
                    :size (om-make-point (- (w self) 25) 25)))
          Ranges)
     (om-add-subviews self panel control rulery rulerx)

     (update-editor-pict self (object self))
     (update-object-pict self (object self))

     (if (multibpf? self)
       (loop for item in (bpf-list (object self)) do
             (setf (selected-p item) nil))
       (setf (selected-p (object self)) nil))
     (setf (rulerx panel) rulerx)
       
    (setf (rulery panel) rulery)
    (setf (panel self) panel)
    (setf (currentbpf panel) (if (multibpf? self) (car (bpf-list (object self))) (object self)))
    
    (init-coor-system panel)
    ;(setf ranges (space-bpf-ranges (give-editor-list-range self)))
    ;(set-ranges panel  (list (first ranges) (second ranges)) (list (third ranges) (fourth ranges)))
    (set-control self control)
    (when (multibpf? self) (add-multibpf-controls (control self)))
    )
   
   (init-spline-manager self))


(defmethod set-control ((self bpfeditor) control) 
  (setf (control self) control)
  (set-value (precisionitem control) (decimals (object self)))
  (om-set-check-box (lineitem control) (lines-p (panel self))))

(defmethod update-precision ((self bpfeditor) val) 
  (set-value (precisionitem (control self)) val))

(defmethod update-editor-after-eval ((Self Bpfeditor) Val)
  (let (Ranges)
    (setf (object self) val)
    (setf (currentbpf (panel self)) (if (multibpf? self) (car (bpf-list val)) val))
    (setf ranges (space-bpf-ranges (give-editor-list-range self)))
    (set-ranges (panel self) (list (first ranges) (second ranges))
                 (list (third ranges) (fourth ranges)))
    (update-editor-pict self (object self))
    (update-precision self (decimals (object self)))
    (om-invalidate-view self t)
    (om-invalidate-view (panel self) t)))

(defun update-editor-pict (editor object)
  (when (and (subtypep (type-of object) 'object-with-pict) (pict object))
    (set-edit-param editor 'picture (pict object)))
  (setf (pict editor) (get-edit-param editor 'picture)))

(defun update-object-pict (editor object)
  (when (and (ref editor) (get-edit-param editor 'picture))
    (setf (name (ref editor)) (name (get-edit-param editor 'picture))))
  (when (subtypep (type-of object) 'object-with-pict)
    (setf (pict object) (get-edit-param editor 'picture))))


;------------------------------------
;Events
;------------------------------------
(defmethod om-view-click-handler ((Self Bpfeditor) Where)
   (declare (ignore where))
   (if (text-view self)
     (exit-text-view self)
     (call-next-method)))

(defmethod exit-text-view ((self bpfeditor))
   (exit-from-dialog (text-view self) (om-dialog-item-text (text-view self))))

(defmethod do-editor-null-event ((Self Bpfeditor))
   (when  (om-view-contains-point-p (panel self) (om-mouse-position self))
     (show-position self)))

(defmethod show-position ((Self Bpfeditor))
  (let* ((pixel (om-mouse-position (panel self))))
    (when pixel
      (setf (current-point self) (pixel2point (panel self) pixel))
      (om-invalidate-view (title-bar self)))))




;(format nil (format nil "x  ~a,~DF" "~" 4) 3.8)

(defmethod update-subviews ((self bpfeditor))
   (when (title-bar self)
     (om-set-view-size  (title-bar self) (om-make-point (w self) *titlebars-h*)))
   (om-set-view-size  (panel self ) (om-make-point (- (w self) 25) (- (h self) (get-control-h self) *titlebars-h* 25)))
   (om-set-view-position  (panel self ) (om-make-point 25 *titlebars-h*))
   (om-set-view-size  (control self) (om-make-point (w self) (get-control-h self)))
   (om-set-view-position (control self) (om-make-point 0 (- (h self) (get-control-h self))))
   (om-set-view-position (rulerx (panel self)) (om-make-point 25 (- (h self) 25 (get-control-h self))))
   (om-set-view-size (rulerx (panel self)) (om-make-point (- (w self) 25) 25))
   (om-set-view-position (rulery (panel self)) (om-make-point 0 *titlebars-h*))
   (om-set-view-size (rulery (panel self)) (om-make-point 25 (- (h self) 25 (get-control-h self) *titlebars-h*)))
   (om-invalidate-view self))

;------------------------------------
;Tools
;------------------------------------
;return a list (minx maxx miny maxy) for the bpf in the bpflist
(defmethod give-editor-list-range ((Self Bpfeditor))
   (if (multibpf? self)
     (loop for item in (bpf-list (object self))
           for range = (give-bpf-range item)
           maximize (fourth range) into y1
           maximize (second range) into x1
           minimize (first range) into x
           minimize (third range) into y
           finally (return (list x x1 y y1)))
     (give-bpf-range (object self))))

;------------------------------------
;BPF EDITOR VIEW Definition and initialization
;------------------------------------

; om-view-drop
(omg-defclass bpfpanel (om-view view-with-ruler-xy) 
   ((mode :initform :normal :accessor mode)
    (selection? :initform nil :accessor selection?)
    (show-back-p :initform t :accessor show-back-p)
    (show-point-indices :initform nil :accessor show-point-indices)   
    (lines-p :initform t :accessor lines-p)
    (currentbpf :initform nil  :accessor currentbpf))
   #+win32(:default-initargs :draw-pinboard-objects :buffer))

(defmethod update-panel ((self bpfpanel) &optional (updateref nil))
  (om-invalidate-view self)
  (update-panel (editor self) updateref))


(defmethod get-bpf-obj ((Self Bpfpanel))
   (object (om-view-container self)))


(defmethod bpf-decimals ((Self Bpfpanel))
   (if (multibpf? (editor self))
     (decimals (currentbpf self))
     (decimals (get-bpf-obj self))))

(defmethod editor ((Self Bpfpanel)) (om-view-container  self))

;------------------------------------
;Draw
;------------------------------------

(defmethod x-label ((self bpfpanel)) "X")
(defmethod y-label ((self bpfpanel)) "Y")

(defmethod redraw-rulers ((self bpfpanel))
  (unless (hide-ruler? (rulerx self)) (om-redraw-view (rulerx self))) 
  (unless (hide-ruler? (rulery self)) (om-redraw-view (rulery self))))

(defmethod om-draw-contents ((Self Bpfpanel))
  (let* ((Conta (om-view-container self))
         (Listbpf (if (multibpf? conta) (bpf-list (object conta)) (list (get-bpf-obj self)))))
    (call-next-method)   
    (when (get-bg-pict (editor self))
      (draw-om-picture (get-bg-pict (editor self)) self))
    (when (grille-p self)
      (draw-grille self))
    (draw-axis self)  
    (om-draw-view-outline self)
    ; (redraw-rulers self)
    (om-with-focused-view self                  
      (when (show-back-p self)
        (loop for item in listbpf do
              (unless (equal item (currentbpf self))
                (om-with-dashline
                  (om-with-fg-color self (bpfcolor item)
                    (draw-bpf self item 
                              (first (rangex self)) (second (rangex self)) 
                              (first (rangey self)) (second (rangey self))))))))
       (om-with-fg-color nil (bpfcolor (currentbpf self))
          (draw-bpf self (currentbpf self)  
                    (first (rangex self)) (second (rangex self)) 
                    (first (rangey self)) (second (rangey self)))
          ) )))
   
   

;DRAW BPF

(defmethod get-string-nom  ((Self Bpfpanel) Num Axe)
   (declare (ignore axe))
   (let ((Dec (bpf-decimals self)))
     (if (zerop dec)
       (format nil "~D" num)
       (let ((fstr (string+ "~," (integer-to-string dec) "F")))
         (format nil fstr (/ num (expt 10.0 dec)))))))
     
(defmethod draw-bpf ((Self Bpfpanel) (Bpf Bpf) Minx Maxx Miny Maxy &optional (Deltax 0) (Deltay 0) (dr-points nil)) 
   (let* ((X-Points (give-points-in-x-range bpf minx maxx))
          (Y-Points (give-points-in-y-range bpf miny maxy))
          (Points (sort (intersection x-points y-points :test 'equal)  '< :key 'om-point-h)))
     (draw-bpf-points self bpf points deltax deltay dr-points)
     
     ;;; optimizacion!
     ;;;(let* ((ptlist (loop for p in (point-list bpf) collect (point2pixel self p (get-system-etat self))))
     ;;;          (n (length ptlist))
     ;;;          (nn (mod (- n 1) 3))
     ;;;          (ptlist (first-n ptlist (- n nn)))) 
     ;;;(cg::draw-curve (cg::window self) ptlist)
     
     ))
    

(defmethod show-lines-p ((self bpfpanel))
  (lines-p self))

(defmethod draw-bpf-points ((Self Bpfpanel) (Bpf Bpf) Points &optional (Deltax 0) (Deltay 0) (dr-points nil))
  (let* ((System-Etat (get-system-etat self))
         (Bpf-Selected? (and (equal (selection? self) t) (equal bpf (currentbpf self)))))
    (loop for point-list on points 
          for i = 0 then (+ i 1) do
          (let* ((Thepoint (car point-list))
                 (Pix-Point (point2pixel self thepoint system-etat))
                 (Points-Prev-Next (give-prev+next-x bpf thepoint))
                 (Prev-Point (first points-prev-next))
                 (Next-Point (second points-prev-next)))
            (when (show-lines-p self)
              (when prev-point
                (let ((Prev-Pixel (point2pixel self prev-point system-etat)))
                  (om-draw-line (+ deltax (om-point-h prev-pixel)) (+ deltay (om-point-v prev-pixel))
                                (+ deltax  (om-point-h pix-point)) (+ deltay (om-point-v pix-point)))))
              (when (or (and next-point (null (second point-list)))
                        (and (second point-list) 
                             (not (equal next-point (second point-list)))))
                (let ((Next-Pixel (point2pixel self next-point system-etat)))
                  (om-draw-line (+ deltax (om-point-h pix-point)) (+ deltay (om-point-v pix-point))
                                (+ deltax (om-point-h next-pixel)) (+ deltay (om-point-v next-pixel))))))
            (when (or (null (show-lines-p self)) (selected-p bpf) (and (not next-point) (not prev-point)) dr-points)
              (om-draw-rect (+ deltax (- (om-point-h pix-point) 2)) (+ deltay (- (om-point-v pix-point) 2)) 4 4))
            (when (or bpf-selected? (and (listp (selection? self))  (member thepoint (selection? self))))
              (om-with-fg-color nil *om-black-color*
                (om-fill-rect (+ deltax (- (om-point-h pix-point) 3)) (+ deltay (- (om-point-v pix-point) 3)) 6 6)))
            (when (show-point-indices self) 
              (om-draw-string (+ deltax (om-point-h pix-point) -4) (+ deltay (om-point-v pix-point) -6) (format nil "~d" i)))
            )
          )))
              
  
;------------------------------------
;EVENTS
;------------------------------------

;Set cursor
(defmethod om-view-cursor ((Self Bpfpanel))
   (declare (ignore where))
   (cond ((equal (mode self) :pen) *om-pen-cursor*)
         ((om-command-key-p)
          (if (and (om-shift-key-p) (multibpf? (editor self)))
              *om-addbpf-cursor*
            *om-point-cursor*))
         ((equal (mode self) :zoom) *om-loupe-cursor*)
         ((equal (mode self) :scroll) *om-hand-cursor*)
         ((equal (mode self) :move) *om-hand-bpf-cursor*)
         (t *om-arrow-cursor*)))

(defmethod om-view-click-handler ((Self Bpfpanel) Where)
   (if (text-view (editor self))
       (exit-text-view (editor self))
     (cond 
      ((equal (mode self) :normal)
       (if (om-add-key-p) 
           (if (om-shift-key-p)
               (if (multibpf? (editor self)) 
                   (add-new-bpf self)
                 (om-beep))
             (add-point-to-bpf self where))
         (select-system self where)))
      ((equal (mode self) :pen)
       (if (om-add-key-p)
           (select-system self where)
         (draw-points-in-bpf self where)))
               
      ((equal (mode self) :zoom) (zoom-system self where))
      ((equal (mode self) :scroll) (scroll-system self where))
      ((equal (mode self) :move) (scroll-bpf self where))
      (t nil))))

(defmethod om-view-doubleclick-handler ((Self bpfpanel) Where)
   (if (equal (mode self) :normal)
       (let ((Position-Obj (point-in-bpf self (currentbpf self) where)))
         (when (consp Position-Obj)
           (apply 'special-move-point (cons self 
                                            (if (consp (car position-obj))
                                                ; case bpc : '(point pos)
                                                (list (caar position-obj) (cadar position-obj))
                                                ; case bpf : point
                                              (list (car position-obj) nil)
                                              )))
           (om-invalidate-view self t)
         ))
     (call-next-method)))

(defmethod change-current-bpf ((self bpfpanel) obj &optional (inc 1))
  (let ((ind (mod (+ (position (currentbpf self) (bpf-list obj)) inc)
                  (length (bpf-list obj)))))
    (setf (selected-bpf obj) ind)
    (setf (currentbpf self) (nth (mod (+ (position (currentbpf self) (bpf-list obj)) inc)
                                      (length (bpf-list obj))) (bpf-list obj)))))
  

(defmethod handle-key-event ((Self bpfpanel) Char)
    (let ((myobj (get-bpf-obj self)))
     (case char
       
       (:om-key-tab (when (multibpf? (editor self))
                      (change-current-bpf self myobj)
                      (om-invalidate-view (editor self) t)))
       ;(#\g (grille-on-off self))
       (#\p (setf (show-point-indices self) (not (show-point-indices self)))
            (om-invalidate-view self))
       (#\c 
         (let ((new-color (om-choose-color-dialog :color (bpfcolor (currentbpf self)))))
           (when new-color
             (fill-bpf-with-color self new-color))))
       (#\n 
        (when (multibpf? (editor self))
          (let ((new-name (om-get-user-string "Object name:" :initial-string (get-name (currentbpf self)))))
           (when new-name
             (set-name (currentbpf self) new-name)
             (om-invalidate-view (editor self) t)))))
       (#\h  (show-help-window (format nil "Commands for ~A Editor" 
                                 (string-upcase (class-name (class-of (object (editor self)))))) 
                          (get-help-list (editor self))))
       (:om-key-delete 
         (cond
          ((null (selection? self))
           (om-beep))
          ((listp (selection? self))
           (loop for point in (selection? self) do
                     (remove-point (currentbpf self) point))
           (update-panel self t))
          ((selection? self)
           (delete-current-bpf self)
           (update-panel self t))
          (t (om-beep)))
        (setf (selection? self) nil))
       (:om-key-up (cond
                                ((null (selection? self))
                                 (om-beep))
                                ((listp (selection? self))
                                 (let ((Moveds (move-points-in-bpf (currentbpf self) (selection? self) 
                                                                                                0 (zoom (rulery self)))))
                                   (if moveds
                                       (progn
                                          (setf (selection? self) moveds)
                                          (update-panel self t)
                                          )
                                     (om-beep-msg "I can not move these points"))))
                                ((selection? self)
                                 (move-bpf-in-x-y (currentbpf self) 0 (zoom (rulery self)))
                                 (update-panel self t))
                                (t (om-beep))))
       (:om-key-down (cond
                     ((null (selection? self))
                      (om-beep))
                     ((listp (selection? self))
                      (let ((Moveds (move-points-in-bpf (currentbpf self) (selection? self) 
                                                        0 (* -1 (zoom (rulery self))))))
                        (if moveds
                          (progn
                            (setf (selection? self) moveds)
                            (update-panel self t))
                          (om-beep-msg "I can not move these points"))))
                     ((selection? self)
                      (move-bpf-in-x-y (currentbpf self) 0 (* -1 (zoom (rulery self))))
                      (update-panel self t))
                     (t (om-beep))))
       (:om-key-right (cond
                        ((null (selection? self))
                         (om-beep))
                        ((listp (selection? self))
                         (let ((Moveds (move-points-in-bpf (currentbpf self) (selection? self) 
                                                           (zoom (rulerx self)) 0)))
                           (if moveds
                             (progn
                               (setf (selection? self) moveds)
                               (update-panel self t))
                             (om-beep-msg "I can not move these points"))))
                        ((selection? self)
                         (move-bpf-in-x-y (currentbpf self) (zoom (rulerx self)) 0)
                         (update-panel self t))
                        (t (om-beep))))
       (:om-key-left (cond
                     ((null (selection? self))
                      (om-beep))
                     ((listp (selection? self))
                      (let ((Moveds (move-points-in-bpf (currentbpf self) (selection? self) 
                                                        (* -1 (zoom (rulerx self))) 0)))
                        (if moveds
                          (progn
                            (setf (selection? self) moveds)
                            (update-panel self t))
                          (om-beep-msg "I can not move these points"))))
                     ((selection? self)
                      (move-bpf-in-x-y (currentbpf self) (* -1 (zoom (rulerx self)))0 )
                      (update-panel self t))
                     (t (om-beep)))))))
    


(defmethod get-help-list ((self  bpfeditor))
  (remove nil 
          (list '((#+cocoa "cmd+clic" #-cocoa "ctrl+clic" "Add point / Draw")
                  ("lrud" "Move selected points")
                  ("del" "Delete selected points")
                  (("c") "Change BPF/BPC color")
                  (("p") "Show point indices")
                  )
                (when (multibpf? self)
                  '(("alt+shift+clic" "Add BPF/BPC")
                    ("tab" "Change current BPF/BPC")
                    (("n") "Change name")
                    )))))

;------------------------------------
;ACTIONS
;------------------------------------

(defmethod add-new-bpf ((Self Bpfpanel))
   (let* ((dec (value (precisionitem (control (editor self)))))
          (Newbpf (make-instance 'bpf :point-list (list (om-make-point 0 0)  (om-make-point (expt 10 (+ dec 2)) (expt 10 (+ dec 2))))
                                :decimals dec)))
     (when (currentbpf self)
       (setf (selected-p (currentbpf self)) nil))
     (if (multibpf? (om-view-container  self))
       (setf (bpf-list (get-bpf-obj self))
             (concatenate 'list (bpf-list (get-bpf-obj self)) (list newbpf)))
       (setf (object (om-view-container self)) newbpf))
     (record-new-bpf (om-view-container self))
     (setf (currentbpf self) newbpf)
     (update-panel self t)))

(defmethod draw-points-in-bpf ((Self Bpfpanel) Where)
  (om-init-motion-functions self 'draw-in-bpf 'end-draw-bpf))

(defun end-draw-bpf (panel pos)
  (update-panel panel t))

(defun draw-in-bpf (panel pos)
  (let* ((new-point (pixel2point panel pos))
         (position-obj (point-in-bpf panel (currentbpf panel) pos)))
    (if (and position-obj (listp position-obj))
        (progn
          ;(setf (selection? panel) position-obj)
          ;(scroll-point panel (first position-obj))
          )
      (progn
        (insert-point (currentbpf panel) new-point)
        (om-invalidate-view panel)
        ))))

(defmethod add-point-to-bpf ((Self Bpfpanel) Where)
   (let* ((New-Point (pixel2point self where))
          (Position-Obj (point-in-bpf self (currentbpf self) where)))
     (if (and position-obj (listp position-obj))
       (progn
         (setf (selection? self) position-obj)
         (scroll-point self (first position-obj)))
       (progn
         (insert-point (currentbpf self) new-point)
         (setf (selection? self) (list new-point))
         (update-panel self t)

          ;;; essayer d'optimizer...
         (scroll-point self new-point)
         ;(update-panel self t)
         ))))

(defmethod delete-current-bpf ((Self Bpfpanel))
   (if (multibpf? (om-view-container  self))
     (progn
       (setf (bpf-list (get-bpf-obj self))
             (remove  (currentbpf self) (bpf-list (get-bpf-obj self)) :test 'equal))
       (setf (currentbpf self) (car (bpf-list (get-bpf-obj self))))
       (setf (selection? self) nil))
     (setf (currentbpf self) nil))
   (unless (currentbpf self)
     (add-new-bpf self))
   (update-panel self t))

(defmethod fill-bpf-with-color ((Self Bpfpanel) Color)
   (setf (bpfcolor (currentbpf self)) color)
   (update-panel self t))


(defvar *bpf-last-click* nil)
(defvar *bpf-offset-click* nil)
(defvar *bpf-first-click* nil)

(defmethod scroll-bpf ((Self Bpfpanel) Where)
  (setf *bpf-last-click* where)
  (setf *bpf-first-click* where)
  (setf *bpf-offset-click* (om-make-point 0 0))
  (om-init-motion-functions self 'make-scroll-bpf 'release-scroll-bpf))

(defmethod release-scroll-bpf ((Self Bpfpanel) Where) 
  (update-panel self t))


(defmethod make-scroll-bpf ((Self Bpfpanel) Where)
   (let* ((old-Mouse *bpf-last-click*)
          (first-Mouse *bpf-first-click*)
          (Initx (om-point-h *bpf-offset-click*))
          (Inity (om-point-v *bpf-offset-click*))
          (Offx (pixel2norme self 'x (- (om-point-h where) (om-point-h first-mouse))))
          (Offy (pixel2norme self 'y (- (om-point-v first-mouse) (om-point-v where)))))
     (move-bpf-in-x-y (currentbpf self) (- offx initx) (- offy inity ))
     (report-modifications (om-view-container self))
     (om-invalidate-view self t)
     (show-position (om-view-container self))
     (setf *bpf-offset-click* (om-make-point offx  offy))
     (setq *bpf-last-click* where)))



(defmethod scroll-point ((Self Bpfpanel) where)
  (setf *bpf-last-click* (om-mouse-position self))
  (setf *bpf-first-click* *bpf-last-click*)
  (setf *bpf-offset-click* (om-make-point 0 0))
  (om-init-motion-functions self 'make-scroll-point 'release-scroll-point))

(defmethod release-scroll-point ((Self Bpfpanel) Where) 
  (unless (om-points-equal-p where *bpf-first-click*)
    (update-panel self t)))

(defmethod make-scroll-point ((Self Bpfpanel) Where)
  (when (om-view-contains-point-p self (om-convert-coordinates where self (om-view-container self)))
  (let* ((old-Mouse *bpf-last-click*)
         (first-Mouse *bpf-first-click*)
         (Initx (om-point-h *bpf-offset-click*))
         (Inity (om-point-v *bpf-offset-click*))
         (Offx (pixel2norme self 'x (- (om-point-h where) (om-point-h first-mouse))))
         (Offy (pixel2norme self 'y (- (om-point-v first-mouse) (om-point-v where))))
         (Moveds (move-points-in-bpf (currentbpf self) (get-selected-points-list self) (- offx initx) (- offy inity ))))
    (if moveds (setf (selection? self) moveds))
    (om-invalidate-view self)
    (show-position (om-view-container self))
    (setf *bpf-offset-click* (om-make-point offx offy))
    (setq *bpf-last-click* where))))

(defmethod scroll-system ((Self Bpfpanel) where)
  (setf *bpf-last-click* where)
  (setf *bpf-first-click* where)
  (setf *bpf-offset-click* (om-make-point 0 0))
  (om-init-motion-functions self 'make-scroll-system 'release-scroll-system))

(defmethod release-scroll-system ((Self Bpfpanel) Where) 
  (update-panel self t))

(defmethod make-scroll-system ((Self Bpfpanel) Where)
  (let* ((old-Mouse *bpf-last-click*)
         (Initmouse old-mouse)
         (Initx (om-point-h *bpf-offset-click*))
         (Inity (om-point-v *bpf-offset-click*))
         (Initrangex (rangex self))
         (Initrangey (rangey self))
         Deltax Deltay)
    (setf deltax (pixel2norme self 'x (- (om-point-h initmouse) (om-point-h Where))))
    (setf deltay (pixel2norme self 'y (- (om-point-v Where) (om-point-v initmouse))))
    (setf (rangex self) (list (+ (first initrangex) deltax)
                              (+ (second initrangex) deltax)))
    (setf (rangey self) (list (+ (first initrangey) deltay)
                              (+ (second initrangey) deltay)))
    (redraw-rulers self)
    ;(om-redraw-view self)
    ;(show-position (om-view-container self))
    (om-invalidate-view self)
    (setq *bpf-last-click* where)))


(defmethod do-after-move ((Self Bpfpanel)) 
  (when (and (spline-p (editor self)) (active (spline (editor self))))
    (compute-spline (spline (editor self))))
  ;(update-panel self t)
  (om-invalidate-view self))

(defmethod x-class-move-point ((Self Bpfpanel)) 'om-editable-text)

(defmethod special-move-point ((Self Bpfpanel) Point i)
  (declare (ignore i))
  (let* ((Dec (bpf-decimals self))
         (Xsize (max 60 (* 10 dec)))
         (Mydialog (om-make-window 'om-dialog
                    :size (om-make-point (+ xsize 140) 80)
                    :window-title ""
                    :maximize nil :minimize nil :resizable nil
                    ;:bg-color *om-window-def-color*
                    :position (om-add-points (om-view-position (window self)) (om-mouse-position self))))
         (Xed (om-make-dialog-item (x-class-move-point self) 
                                   (if (equal (x-class-move-point self) 'om-static-text)
                                       (om-make-point 25 11) (om-make-point 24 0))
                                    (om-make-point xsize 15)
                                   (if (zerop dec) (format () "~D" (om-point-h point))
                                     (format () (format nil "~S,~DF" '~ dec) 
                                             (/ (om-point-h point) (expt 10.0 dec))))
                                   :font *om-default-font2*
                                  ))
         (Yed (om-make-dialog-item 'om-editable-text 
                                    (om-make-point 24 26)
                                    (om-make-point xsize 15)
                                   (if (zerop dec) (format () "~D" (om-point-v point))
                                     (format () (format nil "~S,~DF" '~ dec) 
                                             (/ (om-point-v point) (expt 10.0 dec))))
                                   :font *om-default-font2*)))
    
    (om-add-subviews mydialog 
                     (om-make-dialog-item 'om-static-text (om-make-point 2 5 ) (om-make-point 20 20) (x-label self)
                                          :font *om-default-font2*)
                     (om-make-dialog-item 'om-static-text (om-make-point  2 30) (om-make-point 20 20) (y-label self)
                                          :font *om-default-font2*)
                     
                     xed yed
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 8) (om-make-point 70 15) (om-str :cancel)
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog mydialog ()))
                                          :focus nil
                                          :default-button nil)
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 34) (om-make-point 70 15) (om-str :ok)
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (let ((xval (read-from-string (om-dialog-item-text xed)))
                                                             (yval (read-from-string (om-dialog-item-text yed))))
                                                         (if (and (numberp xval) (numberp yval))
                                                             (if 
                                                                 (move-points-in-bpf 
                                                                  (currentbpf self) 
                                                                  (list point) 
                                                                  (- (round (* (read-from-string (om-dialog-item-text xed)) (expt 10.0 dec))) 
                                                                     (om-point-h point)) 
                                                                  (- (round (* (read-from-string (om-dialog-item-text yed)) (expt 10.0 dec))) 
                                                                     (om-point-v point)))
                                                                 (do-after-move self)
                                                               ;; redisplay
                                                               (om-beep-msg "Illegal move"))
                                                           (om-beep-msg (format nil "Error in enter values: (~D, ~D) are not numbers" xval yval)))
                                                           (om-return-from-modal-dialog mydialog ()))
                                                       ;(om-close-window mydialog)
                                                       )
                                          :focus t
                                          :default-button t))
    (om-modal-dialog mydialog)
    ;(om-select-window mydialog)
    ))


(defmethod editor-change-precision ((self bpfeditor) value) 
  (let ((oldvalue (decimals (object self))))
    (change-precision (object self) value)
    (adapt-coor-system (panel self) (- value oldvalue))
    (redraw-rulers (panel self))
    ))

(defmethod init-coor-system ((Self Bpfpanel))
  (let ((Ranges (space-bpf-ranges 
                 (or (and (pict (editor self))
                          (thepict (pict (editor self)))
                          (ignore-errors (bpf-picture-ranges (pict (editor self)) self)))
                     (give-editor-list-range (om-view-container self))))))
    (set-ranges self (list (first ranges) (second ranges)) 
                (list (third ranges) (fourth ranges)))
    (redraw-rulers self)
    (om-invalidate-view self)
    ))

(defmethod adapt-coor-system ((Self Bpfpanel) diff)
  (set-ranges self (om* (rangex self) (expt 10 diff))
              (om* (rangey self) (expt 10 diff)))
  (redraw-rulers self)
  (om-invalidate-view self)
  )

(defun space-bpf-ranges (Range)
  (list (round (- (first range) (abs (* (- (second range) (first range)) 0.05))))
        (round (+ (second range) (abs (* (- (second range) (first range)) 0.05))))
        (round (- (third range) (abs (* (- (fourth range) (third range)) 0.05))))
        (round (+ (fourth range) (abs (* (- (fourth range) (third range)) 0.05))))))

(defun bpf-picture-ranges (pict panel)
  (when (draw-params pict)
       (om* 
        (if (equal (car (draw-params pict)) 'c)
            (let ((params (cdr (draw-params pict))))
              (list (nth 0 params) (nth 2 params)
                    (nth 1 params) (nth 3 params)))
          (if (equal (car (draw-params pict)) 'p)
              (list 0 (om-pict-width (thepict pict)) 0 (om-pict-height (thepict pict)))))
        (expt 10 (decimals object (editor panel)))
        )))


(defmethod get-real-selection-list ((self bpfpanel))
  (if (consp (slot-value self 'selection?)) ;;; selection? = ((point pos) (point pos) ...) or T or NIL
      (slot-value self 'selection?)
    (when (slot-value self 'selection?)
      (loop for pt in (point-list (currentbpf self)) for i = 0 then (+ i 1) collect
            (list pt i)))))

(defmethod get-selected-points-list ((self bpfpanel))
  (if (and (selection? self) (not (consp (selection? self))))
      (point-list (currentbpf self))
    (selection? self)))


(defmethod select-system ((self Bpfpanel) where)
    (let* ((Position-Obj (point-in-bpf self (currentbpf self) where))
           (Pos-In-Win (view-position-win self)))
      (cond
       ((null position-obj)
        (setf (selected-p (currentbpf self)) nil)
        (setf (selection? self) nil)
        (let ((Newbpf (click-in-bpf-p self where)))
          (if (and newbpf (not (equal newbpf (currentbpf self))))
              (progn 
                (setf (currentbpf self) newbpf)
                (om-invalidate-view (control (om-view-container  self)) t)
                (om-invalidate-view self t))
            (progn 
              (om-init-motion-functions self 'make-select-system 'release-select-system)
              (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-selection-rectangle)
              ))
          ))
       ((listp position-obj)
        (cond 
         ((om-shift-key-p)
          (if (position (first position-obj) (selection? self))
            (setf (selection? self) (remove (first position-obj) (selection? self)))
            (push (first position-obj) (selection? self)))
          (om-invalidate-view self t))
         (t (when (and (null (om-shift-key-p)) 
                       (and (listp (selection? self)) (null (member (first position-obj) (selection? self)))))
              (setf (selection? self) position-obj))))
        (if (equal (selection? self) t)
            (scroll-bpf self where)
          (if (member (first position-obj) (selection? self))
            (scroll-point self (first position-obj)))))
       (t 
        (setf (selected-p (currentbpf self)) t)
        (om-invalidate-view self t)))))


(defmethod make-select-system ((Self Bpfpanel) pos)
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) (second rect) (max 4 (third rect)) (max 4 (fourth rect))))))

(defmethod release-select-system ((self Bpfpanel) pos)  
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos)))
        user-rect scratch-rect-i scratch-rect-n i-rect n-rect)
    (when rect
      (om-erase-movable-object self)
      (do-select-items-in-rect self rect))))

(defmethod do-select-items-in-rect ((self Bpfpanel) rect) 
  (let (user-rect)
    (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
    (let* ((Top-Left (pixel2point self (om-rect-topleft user-rect) ))
           (Bottom-Rig (pixel2point self (om-rect-bottomright user-rect) ))
           (X-Points (give-points-in-x-range (currentbpf self) (om-point-h top-left) (om-point-h bottom-rig)))
           (Y-Points (give-points-in-y-range (currentbpf self) (om-point-v bottom-rig) (om-point-v top-left) ))
           (Points (sort (intersection x-points y-points :test 'equal)  '< :key 'om-point-h)))
      (setf (selection? self) points)
      (om-invalidate-view self t) )))




;--------------------------------------
;TOOLS
;--------------------------------------

(defmethod click-in-bpf-p ((Self Bpfpanel) Where)
   (let ((Myobj (if (multibpf? (om-view-container  self))
                  (bpf-list (get-bpf-obj self))
                  (list (get-bpf-obj self))))
         Rep)
     (loop for item in myobj
           while (not rep) do
           (when (point-in-bpf self item where)
             (setf rep item)))
     rep))


(defmethod corrige-point ((Self Bpfpanel) Point)
   (let* ((Factor (expt 10 (bpf-decimals self))))
     (om-make-point (* factor (round (om-point-h point) factor)) (om-point-v point))))


;-------------------true if point in bpf------------------------------
 
(defmethod point-in-bpf ((Editor Bpfpanel) (Self Bpf) Where)
   (let* ((System-Etat (get-system-etat editor))
          (Point (pixel2point editor where))
          (Point-Intervall (get-3-points self point))
          (Approx 4)
          Region  Rep  Firstpointpix Secpointpix Thirdpointpix)
     (if (null point-intervall) nil
         (progn
           (setf firstpointpix (point2pixel editor (first point-intervall) system-etat))
           (setf secpointpix   (point2pixel editor (second point-intervall) system-etat))
           (setf thirdpointpix (point2pixel editor (third point-intervall) system-etat))
           (om-with-focused-view editor
             (om-open-region editor)
             (dr-reg-2-points firstpointpix secpointpix approx)
             (dr-reg-2-points secpointpix thirdpointpix approx) 
             (setf region (om-close-region editor))
             (setf rep (om-point-in-region-p region where))
             (setf approx (om-make-point approx approx))
             (when (pixel-in-point where firstpointpix region approx)
               (setf rep (list (first point-intervall))))
             (when (pixel-in-point where secpointpix region approx)
               (setf rep (list (second point-intervall))))
             (when (pixel-in-point where thirdpointpix region approx)
               (setf rep (list (third point-intervall))))
             (om-dispose-region region)
             rep))))
  )

(defun dr-reg-2-points (Primo Seco Approx)
;;;   (let ((Mline (abs (m-line primo seco))) Dx Dy)
;;;     (if (< mline 1)
;;;       (setf dx 0 dy approx)
;;;       (setf dy 0 dx approx))
;;;     (#_MoveTo :long (om-add-points primo (om-make-point (* -1 dx) (* -1 dy))))
;;;     (#_LineTo :long (om-add-points primo (om-make-point  dx  dy)))
;;;     (#_LineTo :long (om-add-points seco (om-make-point  dx  dy)))
;;;     (#_LineTo :long (om-add-points seco (om-make-point (* -1 dx) (* -1 dy))))
;;;     (#_lineTo :long (om-add-points primo (om-make-point (* -1 dx) (* -1 dy)))))
   (om-open-region-add-line Primo Seco Approx)
   )


(defun pixel-in-point (Where Point Reg Approx)
  (om-set-rect-region reg (om-point-h (om-subtract-points point approx)) (om-point-v (om-subtract-points point approx))
                                       (om-point-h (om-add-points point approx)) (om-point-v (om-add-points point approx)))
  (om-point-in-region-p reg where))



(defmethod get-menubar ((self bpfEditor)) 
  (list (om-make-menu "File" 
                      (list (om-new-leafmenu  "Close" #'(lambda () (om-close-window (window self))) "w")))
        (om-make-menu "Edit" 
                      (list (om-new-leafmenu  "Select All" #'(lambda () (editor-select-all self)) "a")))
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self :disable '())))

;;; gives draw params of pict depending on editor's BPF object precision
(defmethod get-draw-params (pict (view bpfpanel))
  (cons (car (draw-params pict))
        (om* (cdr (draw-params pict)) (expt 10 (decimals (object (editor view)))))))

(defmethod om-get-menu-context ((self bpfpanel))
  (list (list (om-new-leafmenu "Background Picture..."
                               #'(lambda () 
                                   (let ((rep (picture+params-dialog (get-bg-pict (editor self)))))
                                     (when rep
                                       (setf (pict (editor self)) rep)
                                       (set-edit-param (editor self) 'picture rep)
                                       (update-object-pict (editor self) (object (editor self)))
                                       (om-invalidate-view self))))))))


;===================================
;Do Undo
;===================================

(defmethod extra-menu-info ((self bpfeditor))
  (enable-this-menu-items *window-menu-edit* '("undo")))

(defmethod handle-key-event :before ((self bpfPanel) char)
  (setf (undo (editor self)) (clone (object (editor self)))))
   
;(defmethod select-system :before ((Self Bpfpanel) where)
;  (setf (undo (editor self)) (clone (object (editor self)))))

(defmethod scroll-point :before ((Self Bpfpanel) where)
  ;(setf (undo (editor self)) (clone (object (editor self))))
  t)

(defmethod add-point-to-bpf :before ((Self Bpfpanel) Where)
   ;(setf (undo (editor self)) (clone (object (editor self))))
   t)

(defmethod scroll-bpf :before ((Self Bpfpanel) Where)
  (setf (undo (editor self)) (clone (object (editor self)))))

(defmethod do-undo ((self bpfeditor)) 
  (when (undo self)
    (let ((obj (clone (object self))))
      (cons-bpf (object self) (point-list (undo self)))
      (update-panel (panel self) t)
      (setf (undo self) obj))
  ))

