(in-package :om)

;================================================
;=== CHANNEL CONTROLLER                       ===
;=== a single track of the general controller ===
;================================================
(defclass* audio-Channel-Ctrl () 
  ((track :initform 1 :initarg :track :accessor track :type integer)
   (pan-ctrl :initform 0 :initarg :pan-ctrl  :accessor pan-ctrl :type integer)
   (vol-ctrl :initform 100 :initarg :vol-ctrl :accessor vol-ctrl :type integer)
   ))


;====================================
;=== SETTINGS CONTROLLER          ===
;=== a set of channel controllers ===
;====================================
(defclass* audio-mix-console (simple-score-element) ;; (midi-score-element)
   ((nbtracks :initform 1 :initarg :nbtracks :accessor nbtracks :type t :documentation "number of tracks in the console")
    (channels-ctrl :initform nil :accessor channels-ctrl :type t))
   (:icon 918)
   (:documentation "
   AUDIO-MIX-CONSOLE represents a set of control events (volume, pan) on <nbtracks> channels.

Open the mixing console editor after evaluating the box with the right number of tracks.
Modifications are sent immediately to the audio player when performed in the editor.

The AUDIO-MIX-CONSOLE object can also be 'played' as a musical object, from a patch window or in a maquette.
In this case, all internal events are sent simultaneously.
"))

(defmethod initialize-instance :after ((self audio-mix-console) &rest l)
   (declare (ignore l))
   (cond 
    ((listp (nbtracks self))
     (setf (nbtracks self) (remove-duplicates (loop for tr in (nbtracks self) when (and (integerp tr) (> tr 0) (< tr 15)) collect tr)))
     (setf (channels-ctrl self) 
         (loop for tr in (nbtracks self) collect (make-instance 'audio-Channel-Ctrl 
                                                   :track tr))))
    ((integerp (nbtracks self))
     (if (< (nbtracks self) 1) (setf (nbtracks self) 1))
     (if (> (nbtracks self) 32) (setf (nbtracks self) 32))
     (setf (channels-ctrl self) 
           (loop for i from 1 to (nbtracks self) collect (make-instance 'audio-Channel-Ctrl 
                                                           :track i)))
     )
    (t nil)))

(defmethod allowed-in-maq-p ((self audio-mix-console))  t)

(defmethod get-impulsion-pict ((self audio-mix-console)) *ctrl-impulsion-pict*)

(defmethod Class-has-editor-p  ((self audio-mix-console)) t )
(defmethod get-editor-class ((self audio-mix-console)) 'audiocontrollerEditor)

(defmethod draw-mini-view  ((self t) (value audio-mix-console)) 
   (draw-obj-in-rect value 0 (w self) 0 (h self) (view-get-ed-params self) self))

(defmethod update-miniview ((self t) (value audio-mix-console)) 
   (om-invalidate-view self t))

(defmethod draw-obj-in-rect ((self audio-mix-console) x x1 y y1 edparams view)
   (let ((pw (round (w view) (nbtracks self)))
        (pic (om-load-and-store-picture "audiotrack-bg" 'internal)))
    (loop for i from 0 to (nbtracks self) do
          (om-draw-picture view pic :pos (om-make-point (* i pw) 0)
                           :size (om-make-point pw (h view))
                           ))))

(defmethod omNG-copy ((self audio-mix-console))
   "Cons a Lisp expression that return a copy of self when it is valuated."
   `(let ((rep (make-instance ',(type-of self)
                 :nbtracks ',(nbtracks self))))
      (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                              (omNG-copy ctrl))))
      rep
      ))

(defmethod copy-container  ((self audio-mix-console) &optional (pere nil))
  "Cons a Lisp expression that return a copy of self when it is valuated."
  (let ((rep (make-instance (type-of self)
                :nbtracks (nbtracks self))))
    (setf (channels-ctrl rep) (loop for ctrl in (channels-ctrl self) collect
                                    (eval (omNG-copy ctrl))))
    rep
    ))

(defmethod omNG-save ((self audio-mix-console) &optional (values? nil))
  "Cons a Lisp expression that retunr a copy of self when it is valuated."
  `(let ((rep (make-instance ',(type-of self) 
                :nbtracks ',(nbtracks self))))
     (setf (channels-ctrl rep) (list ,.(loop for ctrl in (channels-ctrl self) collect
                                  (omNG-save ctrl))))
     rep
     ))

(defmethod get-obj-dur ((self audio-mix-console)) 0)


;=========== CONTROLLER EDITOR ===================
;=== The Editor will be a scrollable editor with fixed size 





(defmethod make-editor-window ((class (eql 'audiocontrollerEditor)) object name ref &key 
                                 winsize winpos (close-p t) (winshow t) 
                                 (resize nil) (maximize nil))
   (let ((win (call-next-method class object name ref :winsize (get-win-ed-size object) :winpos winpos :resize nil 
                                                      :close-p t :winshow t
                                                      )))
    win))


(defclass audiocontrollerEditor (EditorView) 
  ((ch-panels :initform nil :accessor ch-panels :type list)))

(defmethod get-win-ed-size ((self audio-mix-console)) 
  (let ((n (if (listp (nbtracks self)) (length (nbtracks self)) (nbtracks self))))
    (om-make-point (* 80 n) 250)))

(defmethod audio-chan-w ((self audiocontrollerEditor)) 80)


(defmethod get-panel-class ((Self audiocontrollerEditor)) 'audiocontrollerPanel)

(defmethod update-subviews ((Self audiocontrollerEditor))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   ;;;(om-update-scroll-bar-limits (panel self )) ;;; mettre dans om-set-field-size pour mac
   ;(om-invalidate-view self t)
   )


;=== MAIN PANEL ===
(defclass audiocontrollerPanel (om-scroller) ()
  ;;;(:default-initargs :scrollbars :h :retain-scrollbars t)
   )

(defmethod get-object ((Self audiocontrollerPanel))
   (object (om-view-container self)))

(defmethod report-modifications ((self audiocontrollerPanel))
  (report-modifications (om-view-container self)))
                                   

;======== CHaNNeL ConTrOllEr PAneL =====

(defclass audiochannelPanel () 
  ((channelctr :initform nil :initarg :channelctr :accessor channelctr)
   (channelText :initform nil :accessor channelText :type t)
   ;(channelBox :initform nil :accessor channelBox :type t)
   (volumeText :initform nil :accessor volumeText :type t)
   (volumeVal :initform nil :accessor volumeVal :type t)
   (volumpeSlider :initform nil :accessor volumeSlider :type t)
   (panText :initform nil :accessor panText :type t)
   (panVal :initform nil :accessor panVal :type t)
   (panSlider :initform nil :accessor panSlider :type t)
   (resetbutton :initform nil :accessor resetbutton :type t)
))


(defclass audiochannelPanelview (audiochannelPanel om-view) ())

(defmethod update-subviews ((Self audiochannelPanel))
   (om-set-view-size (panel self ) (om-make-point (w self) (h self)))
   (om-invalidate-view self t))

(defmethod om-draw-contents ((self audiochannelPanel))
   (call-next-method))



(defmethod get-object ((Self audiochannelPanel))
   (get-object (om-view-container self)))

(defmethod report-modifications ((self audiochannelPanel))
  (report-modifications (om-view-container self)))


(defmethod get-channelpanel-class ((self audiocontrollerPanel)) 'audiochannelPanelview)

;=======================
;=== INITIALIZATIONS ===
;=======================

(defmethod metaobj-scrollbars-params ((self audiocontrollerEditor))  '(:h nil))

(defmethod initialize-instance :after ((self audiocontrollerEditor) &rest l)
   (declare (ignore l))
   (let ((n (if (listp (nbtracks (object self))) (length (nbtracks (object self))) (nbtracks (object self)))))
   (setf (panel self) (om-make-view (get-panel-class self) 
                                                     :owner self
                                                     :position (om-make-point 0 0) 
                                                     :scrollbars (first (metaobj-scrollbars-params self))
                                                     :retain-scrollbars (second (metaobj-scrollbars-params self))
                                                     :field-size  (om-make-point (* (audio-chan-w self) n) 540)
                                                     ;;;:size (om-make-point (w self) (- (h self) 15)))
                                                     :size (om-make-point (w self) (h self)))
      )
   
   (setf (ch-panels self) 
      (loop for chctrl in (channels-ctrl (object self))
             for i = 0 then (+ i 1) collect
                (om-make-view (get-channelpanel-class (panel self))
                 :channelctr chctrl
                 :owner (panel self)
                  :bg-color *om-light-gray-color*
                  :position (om-make-point (* i (audio-chan-w self)) 0) 
                 :size (om-make-point (audio-chan-w self) (h self)))))
   
))



(defmethod update-editor-after-eval ((self audiocontrollereditor) val)
  (setf (object self) val)
  (let ((n (if (listp (nbtracks (object self))) (length (nbtracks (object self))) (nbtracks (object self)))))
    (om-set-view-size (window self) (om-make-point (om-point-h (get-win-ed-size (object self))) (h (window self))))
    (om-set-field-size (panel self) (om-make-point (* (audio-chan-w self) n) 250))
    (loop for chpan in (ch-panels self) do 
          (om-remove-subviews (panel self) chpan))
    (setf (ch-panels self) 
          (loop for chctrl in (channels-ctrl (object self))
                for i = 0 then (+ i 1) collect
                (om-make-view (get-channelpanel-class (panel self))
                              :channelctr chctrl
                              :owner (panel self)
                              :position (om-make-point (* i (audio-chan-w self)) 0) 
                              :size (om-make-point (audio-chan-w self) (- (h self) 15))
                              )))
    ))



(defmethod initialize-instance :after ((self audiochannelPanel) &rest l)
   (declare (ignore l))
   (do-initialize-channel self))


(defmethod make-track-title ((self audiochannelPanel) &optional (ypos 0))
  (om-make-dialog-item 'om-static-text
                       (om-make-point 15 ypos) 
                       (om-make-point 80 16) (format nil "Track ~D" (track (channelctr self))) 
                       :font *om-default-font2b*))
  
(defmethod do-initialize-channel ((self audiochannelPanel))  
   (let* (bar1 bar2 (pos 0) (color (om-make-color 0.9 0.9 0.9)))
     (om-set-bg-color self color)
     (setf pos (+ pos 10))
     (setf (channelText self) (make-track-title self pos))
           
     ;(setf (channelBox self)
     ;      (om-make-dialog-item 'om-static-text
     ;                           (om-make-point 55 pos) 
     ;                           (om-make-point 20 16) (format nil "~D" (track (channelctr self))) 
     ;                           :font *om-default-font2b*
     ;                           :bg-color color)
           ;(om-make-dialog-item 'numBox (om-make-point 45 pos) (om-make-point 20 16) 
           ;                     (format nil "~D" (track (channelctr self)))
           ;                     :min-val 1
           ;                     :max-val 16
           ;                     :value (track (channelctr self))
           ;                     :afterfun #'(lambda (item)
           ;                                   (change-track self (value item)))
           ;                     :font *om-default-font2*
           ;                     )
       ;    )
      
      ;(om-set-part-color (channelBox self) :body *om-white-color*)

     (setf pos (+ pos 35))
     
     (setf (panText self) (om-make-dialog-item 'om-static-text
                                 (om-make-point 18 pos) 
                                 (om-make-point 40 16)
                                 "Pan"
                                 :font *om-default-font1*
                                 :bg-color color
                                 ))
      
     (setf (panVal self) (om-make-dialog-item 'om-static-text
                               (om-make-point 45 pos) 
                               (om-make-point 30 16)
                               (audio-pan2str (pan-ctrl (channelctr self)))
                               :font *om-default-font1*
                               :bg-color color
                               ))
          
     (setf pos (+ pos 20))
     (setf (panSlider self) 
       (om-make-view 'graphic-numbox :position (om-make-point 30 pos) 
                     :size (om-make-point 20 20) ;;; (format nil "~D" zoom)
                     :pict (om-load-and-store-picture "dial" 'di)
                     :nbpict 65
                     :pict-size (om-make-point 24 24)
                     :di-action (om-dialog-item-act item
                                                    (change-pan self (value item)))
                     :value (pan-ctrl (channelctr self))
                     :min-val -100
                     :max-val 100)
       )
     
     (setf pos (+ pos 26))
     ;(setf bar1 (om-make-view 'bar-item 
     ;             :position (om-make-point 4 pos) 
     ;             :size (om-make-point 72 10)
     ;             :bg-color (om-get-bg-color self)))

     (setf pos (+ pos 5))
     (setf (volumeText self) (om-make-dialog-item 'om-static-text 
                               (om-make-point 16 pos) 
                               (om-make-point 40 16)
                               "Vol"
                               :font *om-default-font1*
                               :bg-color color
                               ))
      
     (setf (volumeVal self) (om-make-dialog-item 'om-static-text 
                              (om-make-point 45 pos) 
                              (om-make-point 30 16)
                              (format nil "~D" (vol-ctrl (channelctr self)) )
                              :font *om-default-font1*
                              :bg-color color
                              ))
     
     (setf pos (+ pos 20))
     (setf (volumeSlider self) 
           (om-make-view 'graphic-numbox :position (om-make-point 26 116) 
                                            :size (om-make-point 31 94)
                                            :pict (om-load-and-store-picture "fader" 'di)
                                            :nbpict 77
                                            :pict-size (om-make-point 31 94)
                                            :di-action (om-dialog-item-act item
                                                         (change-volume self (value item)))
                                            :font *om-default-font2*
                                            :value (vol-ctrl (channelctr self))
                                            :min-val 0
                                            :max-val 100))
     
     (setf pos (+ pos 110))
     ;(setf bar2 (om-make-view 'bar-item 
     ;                         :position (om-make-point 4 pos) 
     ;                         :size (om-make-point 132 10)
     ;                         :bg-color color
     ;                         ))
     
     (setf pos (+ pos 10))
    
     (setf (resetbutton self) (om-make-dialog-item 'om-button
                                                   (om-make-point 15 pos) 
                                                   (om-make-point 50 16)
                                                   "Reset"
                                                   :di-action (om-dialog-item-act item
                                                                (reset-all-values self))
                                                   :font *om-default-font1*
                                                   )  
           )
     
     (om-add-subviews self (channelText self) ;(channelBox self) 
                                         ;bar1 
                                         ;bar2  
                                         (volumeSlider self) 
                                         (volumeText self) (volumeVal self)
                                         (panSlider self) 
                                         (panText self) 
                                         (panVal self)
                                         ;(resetbutton self)
                                         )
     ))


;==============================
;=== ACTIONS ON CHANNEL PANELS:
;==============================

(defmethod change-track ((self audiochannelPanel) value)
  (setf (track (channelctr self)) value)
  (report-modifications self))

(defmethod change-volume ((self audiochannelPanel) value)
  (setf (vol-ctrl (channelctr self)) value)
  (las-change-channel-vol-visible (track (channelctr self)) (float (/ value 100)))
  (let ((new-str (integer-to-string value))
        (target (volumeVal self)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)
      ))
  (report-modifications self))

(defun audio-pan2str (panvalue)
  (cond 
   ((= panvalue 0) (integer-to-string panvalue))
   ((< panvalue 0) (format nil "L~D" (- panvalue)))
   ((> panvalue 0) (format nil "R~D" panvalue))
   ))

(defmethod change-pan ((self audiochannelPanel) value)
  (setf (pan-ctrl (channelctr self)) value)
  (las-change-channel-pan-visible (track (channelctr self)) (- 1.0 (float (/ (+ value 100) 200))))
  (let* ((target (panVal self))
         (new-str (audio-pan2str value)))
    (unless (string= new-str (om-dialog-item-text target))
      (om-set-dialog-item-text target new-str)
      (om-redraw-view target)))
  (report-modifications self))


(defmethod reset-all-values ((self audiochannelPanel))
  (change-pan self 0)
  (set-value (panSlider self) 0)
  (change-volume self 100)
  (om-set-slider-value (volumeSlider self) 100))
  
 
