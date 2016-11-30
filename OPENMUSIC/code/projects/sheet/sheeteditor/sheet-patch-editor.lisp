(in-package :om)

;;;===============================
;;; PATCH PART
;;;===============================

;;; sheet-patcheditor: OBJECT = patch
(defclass sheet-patcheditor (patcheditor 3dborder-view) 
  ((currentpatch :accessor currentpatch :initform 0 :initarg :currentpatch)
   (buttonpane :accessor buttonpane :initform nil :initarg :buttonpane)
   )
  (:default-initargs :draw-with-buffer t))

(defmethod sheet-editor ((self sheet-patcheditor)) 
  (om-view-container self))

(defmethod my-current-patch ((self sheet-patcheditor))
  (nth (currentpatch self) (patch-list (object (sheet-editor self)))))

(defmethod om-draw-contents ((self sheet-patcheditor))
  (call-next-method)
  (when (my-current-patch self)
  (let ((str1 (name (my-current-patch self)))
        (str2 (string+ "#" (number-to-string (1+ (currentpatch self)))
                       "/" (number-to-string (length (patch-list (object (sheet-editor self))))))))
    
   
      (let* ((font *om-default-font1b*)
            (size (om-string-size str1 font)))
        (om-with-focused-view self
          (om-with-font font 
                        (om-draw-string (- (round (w self) 2) size) 16 (or str1 "sheet patch"))
                        (om-draw-string (+ (round (w self) 2) 40) 16 str2)))
        ))))

;(defmethod om-view-doubleclick-handler ((self sheet-patcheditor) where)
;  (when (< (om-point-v where) 40)
;    (let* ((oldname (name (my-current-patch self)))
;          (newname (om-get-user-string "Type a new name for this patch."
;                                       :initial-string (if oldname oldname ""))))
;      (when newname 
;        (setf (name (my-current-patch self)) newname)
;        (om-invalidate-view self)))))

(defmethod initialize-instance :after ((self sheet-patcheditor) &rest args)
  (om-add-subviews self
                   (setf (buttonpane self)
                         (om-make-view 'sheet-buttonpane :bg-color *om-window-def-color*))
                   
                   (om-make-view 'om-icon-button 
                                 :position (om-make-point 20 6)
                                 :size (om-make-point 16 16)
                                 :icon1 "prev" :icon2 "prev-pushed"
                                 :help-spec "see previous patch"
                                 :action (om-dialog-item-act item
                                             (setf (currentpatch self) (mod (1- (currentpatch self)) (length (patch-list (object (sheet-editor self))))))
                                             (update-patch-editor self)))
            
                   (om-make-view 'om-icon-button 
                                 :position (om-make-point 40 6)
                                 :size (om-make-point 16 16)
                                 :icon1 "next" :icon2 "next-pushed"
                                 :help-spec "see next patch"
                                 :action (om-dialog-item-act item
                                             (setf (currentpatch self) (mod (1+ (currentpatch self)) (length (patch-list (object (sheet-editor self))))))
                                             (update-patch-editor self)))
                   
                   )
  (update-patch-editor self))

(defmethod update-subviews ((self sheet-patcheditor))
  (call-next-method)
  (om-set-view-position (buttonpane self) (om-make-point 20 (- (h self) 45)))
  (om-set-view-size (buttonpane self) (om-make-point (- (w self) 40) 40)))

(defmethod clear-ev-once ((self sheet-patcheditor))
  (clear-ev-once (panel self)))

(defclass sheet-buttonpane (om-view) ()
  (:default-initargs :draw-with-buffer t))


(defmethod initialize-instance :after ((self sheet-buttonpane) &rest args)
  (om-add-subviews self
                   (om-make-view 'om-icon-button 
                                          :position (om-make-point 0 0)
                                          :size (om-make-point 16 16)
                                          :icon1 "+" :icon2 "+-pushed"
                                          :help-spec "add new patch"
                                          :action (om-dialog-item-act item
                                                    (let ((ed (editor (om-view-container (om-view-container item)))))
                                                      (add-patch-in-sheet (object (sheet-editor ed)) (1+ (currentpatch ed)))
                                                      (setf (currentpatch ed) (1+ (currentpatch ed)))
                                                      (update-patch-editor ed))))
                   (om-make-view 'om-icon-button 
                                          :position (om-make-point 20 0)
                                          :size (om-make-point 16 16)
                                          :icon1 "-" :icon2 "--pushed"
                                          :help-spec "remove current patch"
                                          :action (om-dialog-item-act item
                                                    (let ((ed (editor (om-view-container (om-view-container item)))))
                                                      (when (om-y-or-n-dialog "Are you sur you want to remove this patch from the sheet patch list ? (removed patches can not be restored)")
                                                      (remove-patch-from-sheet (object (sheet-editor ed)) (currentpatch ed))
                                                      (setf (currentpatch ed) (min (currentpatch ed) (1- (length (patch-list (object (sheet-editor ed)))))))
                                                      (update-patch-editor ed)))))
                   
                   (om-make-view 'om-icon-button 
                                          :position (om-make-point 100 0)
                                          :size (om-make-point 16 16)
                                          :icon1 "first" :icon2 "first-pushed"
                                          :help-spec "move current before (patch order)"
                                          :action (om-dialog-item-act item
                                                    (let ((ed (editor (om-view-container (om-view-container item)))))
                                                      (if (switch-sheet-patch (object (sheet-editor ed)) (currentpatch ed) -1)
                                                          (setf (currentpatch ed) (1- (currentpatch ed)))
                                                        (om-beep))
                                                      (om-invalidate-view ed))))

                   (om-make-view 'om-icon-button 
                                          :position (om-make-point 120 0)
                                          :size (om-make-point 16 16)
                                          :icon1 "last" :icon2 "last-pushed"
                                          :help-spec "move current after (patch order)"
                                          :action (om-dialog-item-act item
                                                    (let ((ed (editor (om-view-container (om-view-container item)))))
                                                      (if (switch-sheet-patch (object (sheet-editor ed)) (currentpatch ed) 1)
                                                          (setf (currentpatch ed) (1+ (currentpatch ed)))
                                                        (om-beep))
                                                      (om-invalidate-view ed))))


                   ))


(defclass sheet-patchpanel (patchpanel) ())
(defmethod get-editor-panel-class ((self sheet-patcheditor)) 'sheet-patchpanel)

(defmethod focus-on-panel ((self sheet-patchpanel))
  (setf (active-view (sheet-editor self)) (editor self)))

(defmethod focus-on-panel ((self t)) nil)

(defmethod om-view-click-handler :after ((self sheet-patchpanel) where)
  (focus-on-panel self))

(defmethod toggle-icon-active-mode :after ((self OMSimpleFrame))
  (focus-on-panel (om-view-container self)))



(defmethod panel-position ((self sheet-patcheditor)) (om-make-point 20 25))
(defmethod panel-size ((self sheet-patcheditor)) (om-make-point (- (w self) 40) (- (h self) 75)))

(defmethod sheet-editor ((self sheet-patchpanel)) 
  (sheet-editor (om-view-container self)))

(defmethod my-current-patch ((self sheet-patchpanel))
  (my-current-patch (editor self)))

(defmethod modify-patch ((self sheet-patchPanel))
  (setf (compiled? (my-current-patch self)) nil)
  (setf (saved? (my-current-patch self)) nil))


(defmethod update-patch-editor ((self sheet-patcheditor))
  (setf (editorframe (object self)) nil)
  (setf (object (panel self)) (my-current-patch self))
  (setf (editorframe (my-current-patch self)) self)
  (om-with-delayed-update (panel self)
    (loop for item in (om-subviews (panel self)) do (om-remove-subviews (panel self) item))
    (mapc #'(lambda (elem)
              (let ((newframe (make-frame-from-callobj elem)))
                (om-add-subviews (panel self) newframe)
                (add-subview-extra newframe))) 
          (get-elements (my-current-patch self)))
    (mapc #'(lambda (elem)
              (update-graphic-connections elem (get-elements (my-current-patch self)))) 
          (get-subframes (panel self)))
    (om-invalidate-view self)))







