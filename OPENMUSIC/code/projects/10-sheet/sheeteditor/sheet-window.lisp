(in-package :om)

(defclass sheetEditor (editorview play-editor-mixin) 
  ((score-view :accessor score-view :initform nil :initarg :score-view)
   (patch-view :accessor patch-view :initform nil :initarg :patch-view)
   (patch-display-mode :accessor patch-display-mode :initform nil :initarg :patch-display-mode)
   (active-view :accessor active-view :initform nil)
   ))

(defmethod cursor-panes ((self sheetEditor))
  (list (panel (score-view self))))


(defmethod initialize-instance :after ((self sheetEditor) &rest l)
  (declare (ignore l))
  (om-set-bg-color self *om-dark-gray-color*)
  (om-add-subviews self 
                   (setf (score-view self) (om-make-view 'sheet-scoreeditor 
                                                         :owner self
                                                         :object (object self)
                                                         :bg-color *om-white-color*))
                   (setf (patch-view self) (om-make-view 'sheet-patcheditor 
                                                         :owner self
                                                         :object (object self)
                                                         :bg-color *controls-color* ;(om-make-color 0.4 0.44 0.42)
                                                         )))
  (setf (patch-display-mode self) (get-edit-param self 'patch-open))
  )

(defmethod close-editorFrame ((self sheeteditor))
  (if (PersistantObject-p (object self))
      (setf (Editorframe (object self)) nil)
    (call-next-method)))


(defmethod get-active-view ((self sheeteditor))
  (if (patch-display-mode self) 
      (active-view self)
    (score-view self)))

(defmethod get-patchpanel ((self sheeteditor)) (panel (patch-view self)))


(defmethod update-editor-after-eval ((self sheetEditor) val) 
  (setf (attached-objs val) (attached-objs (object self)))
  (setf (attached-objs (object self)) nil)
  (setf (object self) val)
  (setf (object (score-view self)) val)
  (mapcar #'(lambda (access) (setf (reference access) val)) (attached-objs val))
  (setf (selection? (panel (score-view self))) nil)
  (init-tracks (panel (score-view self)))
  (update-panel (panel (score-view self))))

(defmethod update-subviews ((self sheetEditor)) 
  (if (patch-display-mode self)
      (unless (equal (patch-display-mode self) :temp)
        (let ((mid (round (w self) 2)))
          (om-set-view-size (score-view self) (om-make-point mid (h self)))
          (om-set-view-size (patch-view self) (om-make-point (- (w self) mid) (h self)))
          (om-set-view-position (patch-view self) (om-make-point mid 0))))
    (let ((mid (w self)))
      (om-set-view-size (score-view self) (om-view-size self))
      (om-set-view-position (patch-view self) (om-make-point mid 0))
      )))

(defmethod open-patches ((self sheetEditor))
  (setf (patch-display-mode self) :temp)
  ;(om-set-interior-size (window self) (om-make-point (* 2 (om-point-h (om-interior-size (window self))))
  ;                                               (om-point-v (om-interior-size (window self)))))  
  (setf (patch-display-mode self) t)
  (update-subviews self))

(defmethod close-patches ((self sheetEditor))
  (setf (patch-display-mode self) :temp)
  
  ;(om-set-interior-size (window self) (om-make-point (round (om-point-h (om-interior-size (window self))) 2)
  ;                                              (om-point-v (om-interior-size (window self)))))
  (setf (patch-display-mode self) nil)
  (update-subviews self)
  )


(defmethod get-menubar ((self sheeteditor)) 
  (list (om-make-menu "File"
                      (list 
                       (list 
                        (om-new-leafmenu "New Track" #'(lambda () (editor-add-track (score-view self)) t) "n" 
                                         #'(lambda () (equal (get-active-view self) (score-view self))))
                        (om-new-leafmenu "New Object" #'(lambda () (editor-add-object (score-view self)) t) "N" 
                                         #'(lambda () (equal (get-active-view self) (score-view self)))))
                       (list
                        (om-new-leafmenu "Save" #'(lambda () (window-save (om-view-window self))) "s" #'(lambda () (PersistantObject-p (object self))))
                        (om-new-leafmenu "Last Saved..." #'(lambda () (editor-last-saved self)) nil #'(lambda () (PersistantObject-p (object self))))
                        )
                       (list 
                        (om-new-leafmenu "Close" #'(lambda () (om-close-window (om-view-window self))) "w"))
                       ))

        (om-make-menu "Edit" (list 
                              (list 
                               (om-new-leafmenu "Cut" #'(lambda () (editor-cut (get-active-view self))) "x")
                               (om-new-leafmenu "Copy" #'(lambda () (editor-copy (get-active-view self))) "c")
                               (om-new-leafmenu "Paste" #'(lambda () (editor-paste (get-active-view self))) "v")
                               )
                               (om-new-leafmenu "Select All" #'(lambda () (editor-select-all (get-active-view self))) "a")
                               (om-new-leafmenu "Select All Tracks" #'(lambda () (editor-select-all-tracks (get-active-view self))) "A"
                                                #'(lambda () (equal (get-active-view self) (score-view self))))))  
        
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self)))


(defmethod editor-save ((self sheeteditor))
   "Relation windows save the object associated to the scroller, not the selected icons."
   (let ((thesheet (object self)))
     (set-win-size thesheet (om-view-size (om-view-window self)))
     (set-win-position thesheet (om-view-position (om-view-window self)))
     (om-set-window-title (window self) (name (object self)))
     (omNG-save thesheet nil)))

(defmethod report-modifications ((self sheeteditor))
  (modify-sheet self)
  (call-next-method))

(defmethod modify-sheet ((self sheeteditor))
  (when (persistantobject-p (object self))
    (setf (saved? (object self)) nil)
    (om-set-window-title (window self) (string+ "^" (name (object self))))))

(defmethod editor-last-saved ((self sheetEditor))
  (update-last-saved (object self)))


;;;===============================
;;; PATCH PART
;;;===============================

;;; see sheetpatcheditor.lisp

;;;===============================
;;; SCORE PART
;;;===============================

(defclass sheet-scoreeditor (scoreeditor 3dborder-view) ())

(defmethod update-subviews ((self sheet-scoreeditor))
  (call-next-method)
  (update-subviews (panel self)))


(defclass sheet-scorepanel (scorepanel) 
  ((timebpf :accessor timebpf :initarg :timebpf :initform nil)
   (trackpanels :accessor trackpanels :initarg :trackpanels :initform nil))
  (:default-initargs :draw-with-buffer t))

(defclass sheet-titlebar (editor-titlebar) 
  ((currtime :accessor currtime :initarg :currtime :initform 0)
   (play-buttons :accessor play-buttons :initarg :play-buttons :initform nil)
   (mode-buttons :accessor mode-buttons :initarg :mode-buttons :initform nil)))

(defmethod init-titlebar ((self sheet-scoreeditor))

  (setf (play-buttons (title-bar self))
        (list (om-make-view 'om-icon-button :position (om-make-point 50 2) :size (om-make-point 22 22)
                            :icon1 "play" :icon2 "play-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-play (om-view-container self))))
              
               (om-make-view 'om-icon-button :position (om-make-point 71 2) :size (om-make-point 22 22)
                             :icon1 "pause" :icon2 "pause-pushed"
                             :lock-push t
                             :action #'(lambda (item) (editor-pause (om-view-container self))))
              
               (om-make-view 'om-icon-button :position (om-make-point 92 2) :size (om-make-point 22 22)
                             :icon1 "stop" :icon2 "stop-pushed"
                             :action #'(lambda (item) (editor-stop (om-view-container self))))
               
               (om-make-view 'om-icon-button :position (om-make-point -10 -10) :size (om-make-point 1 1)
                             :icon1 "rec" :icon2 "rec-pushed") ;; dummy rec
              
               (om-make-view 'om-icon-button :position (om-make-point 123 2) :size (om-make-point 22 22)
                             :icon1 "loopbutton" :icon2 "loopbutton-pushed"
                             :lock-push t
                             :selected-p (loop-play (om-view-container self))
                             :action #'(lambda (item) 
                                         (setf (loop-play (om-view-container self))
                                               (not (loop-play (om-view-container self))))
                                         (setf (selected-p item) (loop-play (om-view-container self)))
                                         ))
              
               ))
   
   (setf (mode-buttons (title-bar self))
         (list (om-make-view 'om-icon-button :position (om-make-point 220 2) :size (om-make-point 22 22)
                             :icon1 "mousecursor" :icon2 "mousecursor-pushed"
                             :lock-push t
                             :selected-p (and (panel self) (equal :normal (cursor-mode (panel self))))
                             :action #'(lambda (item) 
                                         (setf (cursor-mode (panel self)) :normal)
                                         (setf (selected-p item) t
                                               (selected-p (cadr (mode-buttons (title-bar self)))) nil)
                                         (om-invalidate-view self)))
               (om-make-view 'om-icon-button :position  (om-make-point 241 2) :size (om-make-point 22 22)
                             :icon1 "beamcursor" :icon2 "beamcursor-pushed"
                             :lock-push t
                             :selected-p (and (panel self) (equal :interval (cursor-mode (panel self))))
                             :action #'(lambda (item) 
                                         (setf (cursor-mode (panel self)) :interval)
                                         (setf (selected-p item) t
                                               (selected-p (car (mode-buttons (title-bar self)))) nil)
                                         (om-invalidate-view (title-bar self))))
               
               
               ))


   (apply 'om-add-subviews (cons (title-bar self)
                                 (append (play-buttons (title-bar self))
                                         (mode-buttons (title-bar self))
                                         )))
   )



(defmethod get-editor-assoc ((self sheeteditor)) (score-view self))


(defmethod om-draw-contents ((self sheet-titlebar)) 
  (call-next-method)
  (om-with-focused-view self
    (om-draw-string (- (w self) 180) 18 
                    (string+ "time (ms): " (number-to-string (currtime self))))))

(defmethod om-view-cursor ((self sheet-scorepanel))
   (if (cursor-p self)
       *om-i-beam-cursor*
     *om-arrow-cursor*))

(defmethod get-score-class-panel ((self sheet-scoreeditor)) 'sheet-scorepanel)
(defmethod get-titlebar-class ((self sheet-scoreeditor)) 'sheet-titlebar)

(defmethod sheet-editor ((self t)) nil)
  
(defmethod sheet-editor ((self sheet-scorepanel)) 
  (om-view-container (om-view-container self)))

(defmethod sheet-editor ((self sheet-scoreeditor)) 
  (om-view-container self))

(defmethod om-get-menu-context ((self sheet-scorepanel)) nil)

(defmethod initialize-instance :after ((self sheet-scorepanel) &rest initargs)
  (init-tracks self)
  (update-panel self t))

(defmethod init-music-patch ((self sheet-scorepanel)) nil)

(defmethod get-sheet-objframes ((self sheet-scorepanel))
  (remove nil (flat (mapcar 'om-subviews (trackpanels self)))))

(defmethod update-subviews ((self sheet-scorepanel))
  (mapcar #'(lambda (p) (om-set-view-size p (om-make-point (om-point-h (om-field-size self)) (h p)))) 
          (trackpanels self)))

(defmethod off-selection ((self sheet-scorepanel))
  (setf (selection? self) nil)
  (mapcar #'off-selection (trackpanels self)))

(defmethod pixel-toms ((self sheet-scorepanel) pixel)
  (values (get-ms-pos self (om-point-h pixel) (staff-zoom self)) 0))


(defmethod om-view-click-handler ((self sheet-scorepanel) where)
  (setf (active-view (sheet-editor self)) (editor self))
  (off-selection self)
  (if (cursor-p self)
      (new-interval-cursor self where)
    (update-panel self t)))

(defmethod om-draw-contents ((self sheet-scorepanel))
  (when (get-edit-param (sheet-editor self) 'grille)
    (let* ((step (get-edit-param (sheet-editor self) 'grille-step))
           (y0 (om-point-v (om-scroll-position self)))
           (h (h self))
           (x0 (om-point-h (om-scroll-position self)))
           (w (w self))
           (x 0))
      (om-with-focused-view self
        (om-with-font (om-make-font *om-def-font-face* 8)
        (om-with-fg-color self *om-gray-color*
        (om-with-line '(1 5)
          (loop for time = 0 then (+  time step)
                while (< x (+ x0 w)) do      
                (setf x (get-x-pos self time (staff-zoom self)))
                (when (> x x0)
                  (om-draw-line x y0 x (+ y0 h))
                  (om-draw-string x (+ y0 h -20) (number-to-string time)))
                )))))))
  ;(when (equal (mode (editor self)) :interval)
    (draw-interval-cursor self)
  ;  )
  )
          

(defmethod time-to-pixels ((view sheet-scorepanel) time-ms)
  (get-x-pos view time-ms (staff-zoom view)))




(defmethod update-slot-edit ((self sheet-scorepanel)) t)

(defmethod om-drag-selection-p ((self sheet-scorepanel) where) nil)

(defmethod om-score-click-handler ((self sheet-scorepanel) where double-click-p) nil)

(defmethod editor-null-event-handler ((self sheetEditor))
  (when (om-view-contains-point-p (panel (score-view self)) (om-mouse-position self))
    (editor-null-event-handler (panel (score-view self)))))

(defmethod editor-null-event-handler ((self sheet-scorepanel)) 
  (setf (currtime (title-bar (editor self))) 
        (get-ms-pos self (om-point-h (om-mouse-position self)) (staff-zoom self)))
  (om-invalidate-view (title-bar (editor self)) t))
    
(defmethod handle-key-event ((self sheeteditor) key)
  (cond ((equal key #\SPACE)
         (editor-play/stop self))
        (t (when (get-active-view self)
             (handle-key-event (get-active-view self) key)))))


(defmethod handle-key-event ((self sheet-scorepanel) key)
  (cond ((equal key :om-key-up) 
         (mapcar 'sheet-key-up (selection? self))
         (when (find-if 'track-p (selection? self)) (init-tracks self))
         (update-panel self))
        ((equal key :om-key-down) 
         (mapcar 'sheet-key-down (selection? self))
         (when (find-if 'track-p (selection? self)) (init-tracks self))
         (update-panel self))
        ((equal key :om-key-left) 
         (mapcar 'sheet-key-left (selection? self))
         (when (find-if 'track-p (selection? self)) (init-tracks self))
         (update-panel self)
         (report-modifications (sheet-editor self)))
        ((equal key :om-key-right) 
         (mapcar 'sheet-key-right (selection? self))
         (when (find-if 'track-p (selection? self)) (init-tracks self))
         (update-panel self)
         (report-modifications (sheet-editor self)))
        ((equal key :om-key-delete) (sheet-key-delete self))
        ((equal key :om-key-esc) (off-selection self) (reset-cursor self) (update-panel self))
        ((equal key #\i) (sheet-key-init self))
        ((equal key #\a) (sheet-key-align self (get-edit-param (sheet-editor self) 'grille-step)))
        ;((equal key #\+) 
        ; (setf (staff-zoom self) (print (* (staff-zoom self) 2)))
        ; (init-tracks self)
        ; (update-panel self))
        ;((equal key #\-) 
        ; (setf (staff-zoom self) (print (/ (staff-zoom self) 2)))
        ; (init-tracks self)
        ; (update-panel self))
        )
  )

(defmethod sheet-key-up ((self t)) nil)
(defmethod sheet-key-down ((self t)) nil)
(defmethod sheet-key-left ((self t)) nil)
(defmethod sheet-key-right ((self t)) nil)

(defmethod sheet-key-up ((self sheet-track)) 
  (cond ((om-shift-key-p)
         (setf (track-pos self) (max 0 (1- (track-pos self)))))
        ((om-option-key-p)
         (switch-track (parent self) self -1))
        (t (setf (track-size self) (max 10 (- (track-size self) 10))))
        ))

(defmethod sheet-key-down ((self sheet-track)) 
  (cond ((om-shift-key-p)
         (setf (track-pos self) (1+ (track-pos self))))
        ((om-option-key-p)
         (switch-track (parent self) self 1))
        (t (setf (track-size self) (+ (track-size self) 10)))))

(defmethod sheet-key-up ((self sheet-track-obj)) 
  (setf (obj-margin self) (- (obj-margin self) 0.2)))

(defmethod sheet-key-down ((self sheet-track-obj)) 
  (setf (obj-margin self) (+ (obj-margin self) 0.2)))

(defmethod sheet-key-left ((self sheet-track-obj)) 
  (move-object self (if (om-shift-key-p) -1000 -100)))

(defmethod sheet-key-right ((self sheet-track-obj)) 
  (move-object self (if (om-shift-key-p) 1000 100)))

(defmethod move-object ((self sheet-track-obj) delta)
  (let* ((newstart (max 0 (+ (start-t self) delta)))
         (diff (- newstart (start-t self))))
    (setf (start-t self) (+ (start-t self) diff))
    (setf (end-t self) (+ (end-t self) diff))))


(defmethod editor-add-track ((self sheet-scoreeditor) &optional track)
  (add-one-track (object (sheet-editor self)) (length (inside (object (sheet-editor self)))) track)
  (init-tracks (panel self))
  (update-panel (panel self) t)
  (report-modifications (sheet-editor self)))

(defmethod editor-add-object ((self sheet-scoreeditor))
  (let ((sel (car (selection? (panel self)))))
    (if (track-p sel)
      (progn
        (add-one-object (object (sheet-editor self)) sel nil 
                        (or (list-max (mapcar 'end-t (objs sel))) 0))
        (init-tracks (panel self))
        (update-panel (panel self))
        (report-modifications (sheet-editor self)))
      (om-beep-msg "SELECT A SHEET TRACK BEFORE TO CREATE A NEW OBJECT"))))

(defmethod add-obj-at-pos ((self sheet-track) panel x)
  (let ((time-pos (get-ms-pos panel (om-point-h x) (staff-zoom panel))))
    (add-one-object (object (sheet-editor panel))
                    self nil time-pos)
    (init-tracks panel)
    (update-panel panel)
    (report-modifications (sheet-editor panel))))


(defmethod sheet-delete ((sheet omsheet) (self sheet-track))
  (let ((pos (position self (inside sheet) :test 'equal)))
    (remove-one-track sheet pos)))

(defmethod sheet-delete ((sheet omsheet) (self sheet-track-obj))
  (if (om-shift-key-p)
      (setf (inside self) nil)
    (remove-one-object sheet self)))

(defmethod sheet-key-delete ((self sheet-scorepanel))
  (loop for obj in (selection? self) do
        (sheet-delete (object (sheet-editor self)) obj))
  (setf (selection? self) nil)
  (init-tracks self)
  (update-panel self)
  (report-modifications (sheet-editor self)))


(defmethod sheet-key-init ((self sheet-scorepanel))
  (loop for obj in (selection? self) do
        (sheet-init obj))
  (init-tracks self)
  (update-panel self)
  (report-modifications (sheet-editor self)))


(defmethod sheet-align ((self sheet-track) unit) t)


(defmethod sheet-align ((self sheet-track-obj) unit) 
  (let ((dur (- (end-t self) (start-t self))))
    (setf (start-t self) (* (round (start-t self) unit) unit))
    (setf (end-t self) (+ (start-t self) dur))))


(defmethod sheet-key-align ((self sheet-scorepanel) unit)
  (loop for obj in (selection? self) do
        (sheet-align obj unit))
  (init-tracks self)
  (update-panel self)
  (report-modifications (sheet-editor self)))


(defmethod editor-select-all ((self sheet-scoreeditor))
  (off-selection (panel self))
  (loop for trp in (om-subviews (panel self)) do
        (loop for box in (om-subviews trp) do
              (setf (selected box) t)
              (pushr (reference box) (selection? (panel self)))))
  (om-invalidate-view self))

(defmethod editor-select-all-tracks ((self sheet-scoreeditor))
  (off-selection (panel self))
  (loop for trp in (om-subviews (panel self)) do
        (setf (selected trp) t)
        (pushr (reference trp) (selection? (panel self))))
  (om-invalidate-view self))

(defmethod editor-cut ((self sheet-scoreeditor))
  (if (> (length (selection? (panel self))) 1)
      (om-beep-msg "Only one object at a time for cut/copy in the sheet editor!")
    (progn
      (editor-copy self)
      (loop for obj in (selection? (panel self)) do
            (setf (inside obj) nil))
      (setf (selection? (panel self)) nil)
      (init-tracks (panel self))
      (update-panel (panel self))
      (report-modifications (sheet-editor self)))))

(defmethod editor-copy ((self sheet-scoreeditor))
  (if (> (length (selection? (panel self))) 1)
      (om-beep-msg "Only one object at a time for cut/copy in the sheet editor!")
    (when (selection? (panel self))
      (setf *score-clipboard* (mapcar #'(lambda (obj) 
                                          (if (track-p obj) 
                                              (clone obj)    
                                            (clone (obj obj))))
                                      (selection? (panel self))))
    )))
      

(defmethod editor-paste ((self sheet-scoreeditor))
  (mapcar #'(lambda (obj)
              (paste-in-sheet self obj))
          *score-clipboard*)
  (off-selection (panel self)))

(defmethod paste-in-sheet ((self sheet-scoreeditor) (obj sheet-track))
  (let ((newtrack (clone obj)))
    (mapcar #'(lambda (obj) (setf (id obj) nil)) (objs newtrack))
    (editor-add-track self newtrack)
    (fill-objects-ids (object (sheet-editor self)))))

(defmethod paste-in-sheet ((self sheet-scoreeditor) (obj t))
  (cond ((and (selection? (panel self))
              (allowed-in-sheet obj))
         (mapcar #'(lambda (box)
                     (setf (inside box) (list (clone obj)))
                     (setf (end-t box) (+ (start-t box) (get-obj-dur obj))))
                 (selection? (panel self)))
         (init-tracks (panel self))
         (update-panel (panel self))
         (report-modifications (sheet-editor self)))
        ((null (selection? (panel self)))
         (om-beep-msg "Select a box to copy in.."))
        ((not (allowed-in-sheet obj))
         (om-beep-msg (string+ (string (type-of obj)) " objects are not allowed in om sheets")))
        (t nil)))




;;;===================
;;; CONTROLS
 
(defclass sheet-controlview (3dBorder-view) 
  ((patchbutton :initform nil :accessor patchbutton :initarg :patchbutton))
  (:default-initargs 
    :c++ *controls-color++* :c+ *controls-color+* 
    :c-- *controls-color--* :c- *controls-color-*
    :draw-with-buffer t))

(defmethod sheet-editor ((self sheet-controlview)) 
  (om-view-container (om-view-container self)))

(defmethod om-view-click-handler ((self sheet-controlview) where)
  (setf (active-view (sheet-editor self)) (om-view-container self)))

(defmethod get-score-class-ctrls ((self sheet-scoreeditor)) 'sheet-controlview)

(defmethod initialize-instance :after ((self sheet-controlview) &rest args)                                     
  (declare (ignore l))
  (let* ((bgcol *controls-color*)
         (l1 230)
         (l2 380)
         (tracksbox (om-make-dialog-item 'om-check-box (om-make-point 20 24) (om-make-point 60 20) "Tracks"
                                         :checked-p (get-edit-param (sheet-editor self) 'show-tracks)
                                         :font *om-default-font1*
                                         :bg-color *controls-color*
                                         :di-action (om-dialog-item-act item 
                                                      (set-edit-param (sheet-editor self) 'show-tracks (om-checked-p item))
                                                      (om-invalidate-view (panel (om-view-container self))))))
         (timebox (om-make-dialog-item 'om-check-box (om-make-point 160 24) (om-make-point 80 20) "Onsets"
                                       :checked-p (get-edit-param (sheet-editor self) 'show-time)
                                       :font *om-default-font1*
                                       :bg-color *controls-color*
                                       :di-action (om-dialog-item-act item 
                                                    (set-edit-param (sheet-editor self) 'show-time (om-checked-p item))
                                                    (om-invalidate-view (panel (om-view-container self))))))
         (idbox (om-make-dialog-item 'om-check-box (om-make-point 160 2) (om-make-point 40 20) "IDs"
                                       :checked-p (get-edit-param (sheet-editor self) 'show-ids)
                                       :font *om-default-font1*
                                       :bg-color *controls-color*
                                       :di-action (om-dialog-item-act item 
                                                    (set-edit-param (sheet-editor self) 'show-ids (om-checked-p item))
                                                    (om-invalidate-view (panel (om-view-container self))))))

         (stepitem (om-make-dialog-item 'om-editable-text (om-make-point 70 4) (om-make-point 40 18) 
                                        (format nil "~D" (get-edit-param (sheet-editor self) 'grille-step))
                                        :font *om-default-font1*
                                        :enable (get-edit-param (sheet-editor self) 'grille)
                                        :modify-action (om-dialog-item-act item
                                                         (let ((val (ignore-errors (read-from-string (om-dialog-item-text item)))))
                                                           (if (and (integerp val) (plusp val))
                                                               (progn
                                                                 (set-edit-param (sheet-editor self) 'grille-step val)
                                                                 (update-panel (panel (om-view-container self))))
                                                             (progn
                                                               (om-set-dialog-item-text item 
                                                                                        (format nil "~D" (get-edit-param (sheet-editor self) 'grille-step)))
                                                               (om-beep)
                                                               ))))
                                        :bg-color *om-white-color*))
         
         (grilletxt (om-make-dialog-item 'om-static-text 
                              (om-make-point 120 4) 
                              (om-make-point 30 20)
                               "ms."
                               :font *om-default-font1*
                               :fg-color (if (get-edit-param (sheet-editor self) 'grille) *om-black-color* *om-gray-color*)
                               :bg-color *controls-color*))
         
         (grillebut (om-make-dialog-item 'om-check-box 
                               (om-make-point 20 2) 
                               (om-make-point 50 20)
                               "Grid"
                               :checked-p (get-edit-param (sheet-editor self) 'grille)
                               :di-action (om-dialog-item-act item
                                            (set-edit-param (sheet-editor self) 'grille (om-checked-p item))
                                            (om-enable-dialog-item stepitem (om-checked-p item))
                                            (om-set-fg-color stepitem (if (om-checked-p item) *om-black-color* *om-gray-color*))
                                            (om-set-fg-color grilletxt (if (om-checked-p item) *om-black-color* *om-gray-color*))
                                            (update-panel (panel (om-view-container self)))
                                            t)
                               :font *om-default-font1*
                               :bg-color *controls-color*))
         )
         
    (om-set-bg-color self *controls-color*)
    (om-add-subviews self tracksbox timebox idbox grilletxt grillebut stepitem)
    
    (om-add-subviews self 
                     (setf (patchbutton self) 
                           (om-make-dialog-item'om-check-box 
                               (om-make-point 20 5) 
                               (om-make-point 160 20)
                               "Show Sheet Patches"
                               :checked-p (get-edit-param (sheet-editor self) 'patch-open)
                               :di-action (om-dialog-item-act item 
                                            (setf (patch-display-mode (sheet-editor self)) (om-checked-p item))
                                            (set-edit-param (sheet-editor self) 'patch-open (om-checked-p item))
                                            (if (patch-display-mode (sheet-editor self))
                                                (open-patches (sheet-editor self))
                                              (close-patches (sheet-editor self)))
                                            )
                               :font *om-default-font1*
                               :bg-color *controls-color*)
                           ))
    (om-set-bg-color self bgcol) 
    ))


(defmethod om-set-view-size ((self sheet-controlview) size)
  (call-next-method)
  (om-set-view-position (patchbutton self) (om-make-point (- (w self) 180) 2)))
  

;;;===================
;;; TRACKS
;;;===================
(defclass trackpanel (om-transparent-view)  ; om-view-drop
  ((selected :accessor selected :initform nil)
   (reference :accessor reference :initarg :reference :initform nil)
   ))

(defmethod sheet-editor ((self trackpanel)) 
  (sheet-editor (om-view-container self)))

(defmethod off-selection ((self trackpanel))
  (setf (selected self) nil)
  (mapcar #'off-selection (om-subviews self)))


(defmethod om-view-click-handler ((self trackpanel) where) 
  (setf (active-view (sheet-editor self)) (editor (om-view-container self)))
  (let ((panel (om-view-container self)))
    (unless (track-p (car (selection? panel)))
      (off-selection panel))
    (cond ((om-add-key-p) 
           (add-obj-at-pos (reference self) panel where))
          ((om-shift-key-p) 
           (select-track-with-shift panel self))
          ((not (member (reference self) (selection? panel) :test 'equal))
           (off-selection panel)
           (setf (selected self) t)
           (setf (selection? panel) (list (reference self))))
          (t nil))
    (om-invalidate-view panel)))

(defmethod select-track-with-shift ((self sheet-scorepanel) graph-obj)
  (unless (track-p (car (selection? self)))
    (setf (selection? self) nil))
  (if (member (reference graph-obj) (selection? self) :test 'equal)
      (progn
        (setf (selection? self) (remove (reference graph-obj) (selection? self) :test 'equal))
        (setf (selected graph-obj) nil))
    (progn
      (push (reference graph-obj) (selection? self))
      (setf (selected graph-obj) t))))

(defmethod om-draw-contents ((self trackpanel))
  (when (or (get-edit-param (sheet-editor self) 'show-tracks) (selected self))
    (om-with-focused-view self 
      (om-with-fg-color self (if (selected self) (om-make-color-alpha 0.78 0.8 0.79 0.4) (om-make-color-alpha 0.9 0.9 0.9 0.4))
          (om-fill-rect 0 0 (w self) (h self))))))



;;;===================
;;; OBJECT FRAMES
;;;===================
(omg-defclass sheet-objectframe (om-scroller om-view-drag om-view-drop) 
  ((selected :accessor selected :initform nil)
   (reference :initarg :reference :accessor reference :initform nil)))


(defmethod sheet-editor ((self sheet-objectframe)) 
  (sheet-editor (om-view-container self)))

(defmethod off-selection ((self sheet-objectframe))
  (setf (selected self) nil))

;;; scoreobjects
(omg-defclass sheet-scoreobjectframe (sheet-objectframe scorepanel) ())

;;; maquette, sound, bpf, midifile
(omg-defclass sheet-linobjectframe (sheet-objectframe) ((graphic-obj :initform nil :accessor graphic-obj)))

(defmethod get-sheetframe-class ((self t)) 'sheet-linobjectframe)
(defmethod get-sheetframe-class ((self score-element)) 'sheet-scoreobjectframe)
(defmethod get-sheetframe-class ((self midicontrol)) 'sheet-linobjectframe)

(defmethod panel ((self sheet-objectframe))
  (om-view-container (om-view-container self)))

(defmethod init-music-patch ((self sheet-scoreobjectframe)) nil)

(defmethod do-initialize-instance ((self sheet-objectframe))
  (let* ((track (om-view-container self))
         (panel (when track (om-view-container track)))
         (obj (reference self)))
    (when panel
      (om-set-view-size self (om-make-point (- (get-x-pos panel (end-t obj)) (get-x-pos panel (start-t obj))) (h self)))
    nil)))

(defmethod om-view-cursor ((self sheet-scoreobjectframe)) nil)

(defmethod om-view-click-handler ((self sheet-objectframe) where) 
  (setf (active-view (sheet-editor self)) (editor (om-view-container (om-view-container self))))
  (let ((panel (panel self)))
    (unless (track-obj-p (car (selection? panel)))
      (off-selection panel))
    (if (om-shift-key-p) 
        (select-obj-with-shift panel self)
      (when (not (member (reference self) (selection? panel) :test 'equal))
        (off-selection panel)
        (setf (selected self) t)
        (setf (selection? panel) (list (reference self)))))
   (om-invalidate-view panel)))

(defmethod select-obj-with-shift ((self sheet-scorepanel) graph-obj)
  (unless (track-obj-p (car (selection? self)))
    (setf (selection? self) nil))
  (if (member (reference graph-obj) (selection? self) :test 'equal)
      (progn
        (setf (selection? self) (remove (reference graph-obj) (selection? self) :test 'equal))
        (setf (selected graph-obj) nil))
    (progn
      (push (reference graph-obj) (selection? self))
      (setf (selected graph-obj) t))))



(defmethod view-get-ed-params ((self sheet-objectframe))
  (let ((obj (reference self)))
    (corrige-edition-params (obj obj) (edition-params obj))
    ))

(defmethod score-top-margin ((self sheet-scoreobjectframe) &optional val)
  (if val
    (setf (obj-margin (reference self)) val)
    (obj-margin (reference self))))

(defmethod set-size ((self sheet-objectframe) size) t)
(defmethod set-staff-sys ((self sheet-objectframe)) t)

(defmethod set-staff-sys ((self sheet-scoreobjectframe))
  (setf (staff-sys self) (get-staff-system (obj-staff (reference self)))))

(defmethod set-size ((self sheet-scoreobjectframe) size) 
  (setf (staff-size self) size))

(defmethod om-get-menu-context ((self sheet-scoreobjectframe))

  (list (om-make-menu "Object Size" 
                      (list (mapcar #'(lambda (size) 
                                        (om-new-leafmenu (number-to-string size)
                                                         #'(lambda () 
                                                             (if (> (length (selection? (panel self))) 1)
                                                                 (let ((panel (panel self)))
                                                                   (loop for obj in (selection? (panel self)) do
                                                                         (setf (obj-size obj) size))
                                                                   (init-tracks panel)
                                                                   (update-panel panel))
                                                               (progn 
                                                                 (setf (staff-size self) size)
                                                                 (setf (obj-size (reference self)) size)
                                                                 (update-panel (panel (score-view (sheet-editor self)))))
                                                               ))
                                                         nil t t))
                        '(8 12 16 20 24 28 36))))
        (om-make-menu "Staff" 
                      (list (mapcar #'(lambda (staff) 
                                        (om-new-leafmenu (car staff)
                                                         #'(lambda () 
                                                             (if (> (length (selection? (panel self))) 1)
                                                                   (let ((panel (panel self)))
                                                                     (loop for obj in (selection? (panel self)) do
                                                                           (setf (obj-staff obj) (cadr staff)))
                                                                     (init-tracks panel)
                                                                     (update-panel panel))
                                                             (progn
                                                               (setf (staff-sys self) (get-staff-system (cadr staff)))
                                                               (setf (obj-staff (reference self)) (cadr staff))
                                                               (update-panel (panel (score-view (sheet-editor self))))
                                                               )))
                                                         nil t t))
                        *chord-satff-om*)))))



;;; needed for standard d&d
(defmethod object ((self sheet-objectframe)) (reference self))

(defmethod om-drag-receive ((view sheet-objectframe)
                            (dragged-view t) position &optional (effect nil))
  (unless (equal view dragged-view)
    (let* ((dragged (dragged-list-objs *OM-drag&drop-handler*))
           (editor (sheet-editor view))
           (newobj (cond ((equal (drag-flavor *OM-drag&drop-handler*) :omsc)
                          (collect-from-score dragged))
                         ((> (length dragged) 1)
                          (om-beep-msg "Only 1 object at a time in a sheet box!"))
                         (t (dragged-box-to-sheet (car dragged))))))
      (if newobj 
          (progn
            (change-object (reference view) (clone newobj))
            (sheet-init (reference view))
            (init-tracks (panel (score-view editor)))
            (update-panel (panel (score-view editor)))
            (report-modifications editor)
            t)
        (progn 
          (om-highlight-view (get-drag-object view) nil)
          (om-highlight-view view nil)
          (om-beep))
        ))))


(defmethod dragged-box-to-sheet ((self t)) nil)

(defmethod dragged-box-to-sheet ((self boxeditorframe)) 
  (and (allowed-in-track (value (object self)))
       (value (object self))))
        
(defmethod dragged-box-to-sheet ((self tempobjframe)) 
  (and (allowed-in-sheet (car (value (object self))))
       (car (value (object self)))))

(defmethod dragged-box-to-sheet ((self sheet-objectframe)) 
  (and (obj (reference self))))



(defmethod om-drag-selection-p ((view sheet-objectframe) local-mouse-position)
  (declare (ignore view local-mouse-position)) t)
(defmethod om-drag-selection-p ((view sheet-scoreobjectframe) local-mouse-position)
  (declare (ignore view local-mouse-position)) t)

(defmethod om-drag-start ((view sheet-scoreobjectframe))
  (sheet-startdrag-object view) t)

(defmethod om-drag-start ((view sheet-objectframe))
  (sheet-startdrag-object view)
  t)

(defmethod om-drag-reference-view ((self sheet-objectframe)) self)
(defmethod om-drag-container-view ((self sheet-objectframe)) 
  (om-view-container (om-view-container (om-drag-reference-view self))))

(defmethod om-draw-contents-for-drag ((self sheet-objectframe))
  (om-with-fg-color nil (om-make-color-alpha 0.5 0.5 0.5 0.5)
    (om-fill-rect (x self) (+ (y self) (y (om-view-container self)))
                  (w self) (h self))
    ))


(defmethod sheet-startdrag-object ((self sheet-objectframe))
  (let* ((container (om-view-container (om-view-container self))))
    (setf (dragged-view *OM-drag&drop-handler*)  self
          (dragged-list-objs *OM-drag&drop-handler*)  (list self)
          (container-view *OM-drag&drop-handler*)  container
          (true-dragged-view *OM-drag&drop-handler*) self
          (drag-flavor *OM-drag&drop-handler*) :omvw)
    t))

;;; (defmethod drop-allow-p ((D&DHandler omdrag-drop) (dragged omboxeditcall) (target track)) t)

(defmethod om-drag-receive ((self sheet-scorepanel) (dragged-view sheet-objectframe) position &optional (effect nil))
  nil)

(defmethod om-drag-receive ((self patchpanel) (dragged-view sheet-objectframe) position &optional (effect nil))
  (when (obj (reference dragged-view))
    (let* ((pos (borne-position (om-mouse-position self)))
           (newbox (omNG-make-new-boxcall (find-class 'sheet-track-obj) 
                                          pos
                                          (mk-unique-name self (string "sheet-object")))))
      (setf (value newbox) (clone (reference dragged-view)))
      (let ((boxframe (make-frame-from-callobj newbox)))
        (omG-add-element self boxframe)
        (setf (frames newbox) (list boxframe))
        (om-set-view-position boxframe pos))
      t)))


;;;===================
;;; SHEET-OBJ SPACING/DISPLAY
;;;===================

(defmethod om-draw-contents ((self sheet-objectframe))
  (let ((panel (panel (score-view (sheet-editor self)))))
    (om-with-focused-view self 
      (om-with-fg-color self (if (selected self) (om-make-color 0.8 0.79 0.78) *om-white-color*)
        (om-fill-rect 0 0 (w self) (h self)))
      (draw-track-event (obj (reference self)) (reference self) self panel)
      (when (get-edit-param (sheet-editor self) 'show-time)
        (om-draw-string 2 (- (h self) 2) (number-to-string (start-t (reference self)))))
      (when (get-edit-param (sheet-editor self) 'show-ids)
       (om-with-font *om-default-font1b*
         (om-draw-string 2 10 (number-to-string (id (reference self)))))))))


(defmethod om-draw-contents ((self sheet-scoreobjectframe))
  (let* ((panel (panel (score-view (sheet-editor self))))
         (size (staff-size self))
         (deltay (round (* size (score-top-margin self))))
         (deltax (- (get-x-pos panel 0 (staff-zoom panel)) 
                    (get-x-pos panel (start-t (reference self)) (staff-zoom panel)))))
    (om-with-focused-view self 
      (om-with-fg-color self (if (selected self) (om-make-color 0.8 0.79 0.78) *om-white-color*)
        (om-fill-rect 0 0 (w self) (h self)))
      (let ((*internal-score-fonts* (init-fonts-to-draw (staff-size self))))
        (draw-system-only self)
        (when (graphic-obj self)  
          (om-with-font (get-font-to-draw 0)
          (draw-object  (graphic-obj self) self 
                        deltax
                        (- deltay (round (* (posy (car (staff-list (staff-sys self)))) (/ size 4))))
                        (staff-zoom panel) 
                        (om-h-scroll-position panel) ; 0 
                        (+ (om-h-scroll-position panel) (w panel)) ; (- (w self) deltax)
                        0 ;y0 
                        (h self) ; (+ y0 (h self)) 
                        nil ;'dur ; (slots-mode self) 
                        size (linear? self) (staff-sys self) nil (noteaschan? self))
          )))
      (when (get-edit-param (sheet-editor self) 'show-time)
        (om-draw-string 2 (- (h self) 2) (number-to-string (start-t (reference self)))))
      (when (get-edit-param (sheet-editor self) 'show-ids)
        (om-with-font *om-default-font1b*
          (om-draw-string 2 10 (number-to-string (id (reference self))))))
      )))

(defmethod sheet-obj-position ((frame sheet-objectframe) panel)
  (om-make-point (get-sheet-event-pos panel frame (start-t (reference frame))) 3))

(defmethod sheet-obj-size ((frame sheet-objectframe) panel)
  (om-make-point (- (get-x-pos panel (end-t (reference frame)) (staff-zoom panel)) 
                    (get-sheet-event-pos panel frame (start-t (reference frame))))
                 (- (h (om-view-container frame)) 6)))



(defmethod sheet-obj-size ((frame sheet-scoreobjectframe) panel)
  (om-make-point (+ (- (get-x-pos panel (end-t (reference frame)) (staff-zoom panel)) 
                    (get-sheet-event-pos panel frame (start-t (reference frame))))
                    0)
                 (- (h (om-view-container frame)) 6)))

(defmethod get-sheet-event-pos ((self sheet-scorePanel) (frame sheet-scoreobjectframe) time)
   (+ (- (get-x-pos self time (staff-zoom self)) 
         (get-x-pos self 0 (staff-zoom self)))
      (- (get-x-pos self time (staff-zoom self)) 
         (second (car (collect-bpftime-objects (graphic-obj frame) (reference (graphic-obj frame)) (staff-size frame))))
         (mesure-space (graphic-obj frame) (staff-size frame) t)
         )))
      
      
(defmethod get-sheet-event-pos ((self sheet-scorePanel) (frame sheet-objectframe) time)
   (get-x-pos self time (staff-zoom self)))


;;;===================
;;; INTERNAL EDITORS
;;;===================

(defmethod om-view-doubleclick-handler ((self sheet-objectframe) pos)
  (OpenObjectEditor self))
;  (open-sheet-internal-editor self))

(defmethod OpenObjectEditor ((self sheet-objectframe)) 
   "If there is a EditorFrame open  for SELF select the window of EditorFrame, 
else create a new Editor frame, and select its window."
   (setf (EditorFrame (reference self)) (OpenEditorframe (reference self)))
   (unless (find (EditorFrame (reference self)) (attached-editors (sheet-editor self)))
     (push (EditorFrame (reference self)) (attached-editors (sheet-editor self))))
   (when (EditorFrame (reference self))
     (om-select-window (window (Editorframe (reference self))))))

(defmethod OpenEditorframe ((self sheet-track-obj))
  (let* ((object (obj self))
         (int-info (obj-for-internal-editor object)))
    (if int-info
        (or (editorframe self)
            (cond ((maquette-p object) 
                   (load-maquette object)
                   (panel (open-new-RelationFrame object (if (saved? object) (name object) (string+ "^" (name object))) 
                                                  (get-elements object) self)))
                  (t (let ((ed (panel (make-editor-window (first int-info) object 
                                          (string+ (second int-info) " - ID=" (integer-to-string (id self))) 
                                          self))))
                       (change-system ed (obj-staff self))
                       ed)))
            )
      (om-beep))))

(defmethod change-system ((self t) staff) nil)

(defmethod special-close-editor-frame ((ref sheet-track-obj) ed)
  (setf (editorframe ref) nil))

(defmethod special-close-editor-frame ((ref sheet-track-obj) (ed maquetteeditor))
  (setf (editorframe ref) nil)
  (sheet-init ref)
  (update-panel (panel (score-view (sheet-editor (frame ref)))))
  (report-modifications (sheet-editor (frame ref))))
  

(defmethod change-in-int-editor ((self sheet-track-obj) internal newobject lastobj)
  (sheet-init self)
  (update-panel (panel (score-view (sheet-editor (frame self)))))
  (report-modifications (sheet-editor (frame self))))

(defmethod change-in-int-editor ((self sheet-track-obj) (internal scorepanel) newobject lastobj)
  (setf (obj-staff self) (sysname (staff-sys internal)))
  (when (frame self) (setf (staff-sys (frame self)) (get-staff-system (obj-staff self))))
  (call-next-method))
                                 
(defmethod obj-for-internal-editor ((self ommaquette))
  (list 'maquetteeditor "internal maquette"))

(defmethod obj-for-internal-editor ((self sound))
  (list 'soundeditor "sound"))

(defmethod obj-for-internal-editor ((self bpf))
  (list 'bpfeditor "internal BPF"))


;;;===================
;;; TRACKS SETTINGS
;;;===================

(defmethod update-panel ((self sheet-scorepanel) &optional (updateref nil))
  (setf (graphic-obj self) (make-instance 'grap-sheet :reference (object (sheet-editor self))))
  (loop for objframe in (get-sheet-objframes self) do
        (let ((*internal-score-fonts* (init-fonts-to-draw (staff-size self))))
           (make-grap-obj objframe)
          ))
  (handler-bind 
      ;;; THERE ARE SOME ERRORS ON WINDOWS WHERE GRAP-OBJS ARE (sometimes) NIL
      ((error #'(lambda (e) 
                  (print (format nil "ERROR: ~s" e))
                  (abort))))
    (space-sheet-tracks self) 
    (loop for trp in (trackpanels self) do
          (loop for ov in (om-subviews trp) do
                (om-set-view-position ov (sheet-obj-position ov self))
                (om-set-view-size ov (sheet-obj-size ov self))))
    )
  (om-invalidate-view self t)
  )

;;; creation des tracks et des objets dans les tracks
;;; appelee quand le panel est cree ou quand la structure du sheet a change
(defmethod init-tracks ((self sheet-scorepanel))
  (om-with-delayed-redraw self
  (loop for trackp in (trackpanels self) collect 
        (om-remove-subviews self trackp))
  (let ((yunit 20)
        (y 0))
  (setf (trackpanels self)
        (loop for track in (inside (object (sheet-editor self)))
              for i = 0 then (+ i 1) 
              collect
              (let* ((trackp (om-make-view 'trackpanel 
                                           :position (om-make-point 0 (+ y (* (track-pos track) yunit))) 
                                           :size (om-make-point (om-point-h (om-field-size self)) 
                                                                (track-size track))
                                          :reference track)))
                (when (find track (selection? self))
                  (setf (selected trackp) t))
                (setf y (+ y (* (track-pos track) yunit) (track-size track)))
            (loop for object in (objs track) do
                  (let ((objview (om-make-view (get-sheetframe-class (obj object))
                                         :reference object
                                         :position (om-make-point (get-x-pos self (start-t object) (staff-zoom self)) 3)
                                         :scrollbars nil
                                         :size (om-make-point (- (get-x-pos self (end-t object) (staff-zoom self))
                                                                 (get-x-pos self (start-t object) (staff-zoom self))) 
                                                              (- (h trackp) 6))
                                         :field-size (om-make-point (- (get-x-pos self (end-t object) (staff-zoom self)) 
                                                                       (get-x-pos self (start-t object) (staff-zoom self))) 
                                                                    (- (h trackp) 6))
                                         :edition-values (make-instance 'edition-values)
                                         )))
                    (setf (frame object) objview)
                    (set-staff-sys objview)
                    (set-size objview (obj-size object))
                    (do-initialize-instance objview)
                    (om-add-subviews trackp objview)
                    ))
                (om-add-subviews self trackp)
                trackp))
        )
  )))


;;;=====================
;;; PLAY
;;;=====================

(defclas grap-sheet (grap-poly) 
  ((page-list :initform nil)))

(defclass grap-marker () ())


(defun selection-interval (selection)
  (cond ((track-p (car selection))
         (list 0 
               (loop for track in selection maximize (get-obj-dur track))))
        (t (list (loop for obj in selection minimize (start-t obj))
                 (loop for obj in selection maximize (end-t obj))))))


(defmethod get-obj-to-play ((self sheeteditor))
  (let* ((scorepanel (panel (score-view self)))
         (selection (selection? scorepanel)))
    (if selection
        (cond ((track-p (car selection))
               (make-instance 'omsheet :voices selection))
              (t (make-instance 'omsheet 
                                :voices (list (make-instance 'sheet-track 
                                                             :objs selection)))))
      (call-next-method))))


(defmethod play-selection-first ((self sheeteditor)) t)

(defmethod get-interval-to-play ((self sheeteditor))
  (let ((scorepanel (panel (score-view self))))
    (if (and (equal :normal (cursor-mode scorepanel)) (selection? scorepanel))
        (selection-interval (selection? scorepanel))
      (call-next-method))))


(defmethod collect-temporal-objects ((self grap-sheet) father)
   (loop for tr in (inside father)
         append (collect-temporal-objects tr father)))

(defmethod collect-temporal-objects ((self sheet-track) father)
   (loop for obj in (objs self)
         append (collect-temporal-objects obj father)))

(defmethod collect-temporal-objects ((self sheet-track-obj) father)
  (cons (list (start-t self) self)
        (collect-temporal-objects (frame self) self)))
 
(defmethod collect-temporal-objects ((self sheet-objectframe) father)
  (sort (append (collect-temporal-objects (graphic-obj self) father) 
                (get-special-cursor-positions (reference self) (obj (reference self))))
        '< :key 'car))
                 
(defmethod get-special-cursor-positions ((self t) obj) nil)

(defmethod get-special-cursor-positions ((self sheet-track-obj) (obj bpf)) 
  (mapcar #'(lambda (p) (list (+ (start-t self) (car p)) (make-instance 'grap-marker))) (point-pairs obj)))

(defmethod get-special-cursor-positions ((self sheet-track-obj) (obj sound)) 
  (mapcar #'(lambda (m) (list (+ (start-t self) (round (* m 1000))) (make-instance 'grap-marker))) (markers obj)))

(defmethod get-special-cursor-positions ((self sheet-track-obj) (obj ommaquette)) 
  (loop for b in (remove-if-not 'boxtempobj-p (boxes obj)) append
        (list 
         (list (+ (start-t self) (offset b)) (make-instance 'grap-marker)) 
         (list (+ (start-t self) (offset b) (round (* (extend b) (strech-fact b)))) (make-instance 'grap-marker)))))


(defmethod editor-play ((self sheeteditor))
  (update-panel (panel (score-view self)))
  (let ((interval (get-interval-to-play self))
        (cursorevents (remove-duplicates (get-temporal-objects (graphic-obj (panel (score-view self)))) :test 'equal :key 'car)))
    (setf *events-play-cursor* 
          (if interval
              (remove-if #'(lambda (x) (or (> (car x) (second interval)) (< (car x) (car interval)))) cursorevents)
            cursorevents)))
  (call-next-method))


;(defmethod schedule-editor-contents ((self sheeteditor))
;  (player-schedule (player self) 
;                   (get-obj-to-play self)
;                   (get-player-engine self) 
;                   :at 0 
;                   :interval (get-interval-to-play self)))



(defmethod update-cursor ((self sheet-scorepanel) time &optional y1 y2)
  (let ((currevent (find time *events-play-cursor* :key 'car :test '>= :from-end t)))
    (when currevent
      (let* ((cur-evt (second currevent))
             (cur-time (first currevent))
             (cur-pixel (get-x-pos self cur-time (staff-zoom self)))
             (y0 (om-v-scroll-position self)))
        (when (> cur-pixel (+ (om-h-scroll-position self) (w self)))
          (om-set-scroll-position self (om-make-point cur-pixel (om-v-scroll-position self))))
        (om-update-movable-cursor self cur-pixel (om-v-scroll-position self) 4 (h self))
        ))))

