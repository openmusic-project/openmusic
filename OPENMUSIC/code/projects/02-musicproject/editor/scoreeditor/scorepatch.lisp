(in-package :om)


(defmethod get-panel-connections-lines ((self omboxframe) (panel scorepanel) in connection boxsource possource sizesource)
  (let ((x-self (x self))
        (y-self (y self))
        (in-xs (x in))
        x1 y1 xi yi)
    (setq x1 (round (+ (om-point-h possource)  
                       (- (* (+ (second connection) 1) (/ (om-point-h sizesource) (+ (numouts boxsource) 1))) 2))))
    (setq y1 (- (+ (om-point-v possource) (h (car (frames boxsource)))) 2))
    (setq xi (+ x-self in-xs 4))
    (setq yi y-self)
    (get-line-connection x1 y1 xi yi)
    ))



;--------------------------------------------------
;BOXes in the score
;-------------------------------------------------- 

(defclas score-box (omboxcall) 
   ((connected? :initform nil)))

(defmethod numouts ((self score-box)) 1)
(defmethod get-documentation ((self score-box)) "musical box")

(defun make-score-box (musobj class posi name size)
   (let* (rep)
     (setf rep (make-instance class
                 :name name
                 :reference musobj))
     (setf (frame-position rep) posi)
     (setf (frame-size rep) size)
     (setf (name rep) name) 
     rep))

(defmethod make-frame-from-callobj ((self score-box))
   (let* ((module (om-make-view (get-frame-class self) 
                    :position (frame-position self)
                    :help-spec (get-documentation self)
                    :size  (frame-size self)
                    :object self))
          (input (car (inputs self)))
          thenewout inputf)
     (setf thenewout (om-make-view (get-out-class self)
                       :position (om-make-point (- (round (om-point-h (frame-size self)) 2) 4) 
                                                  (- (h module) 8))
                       :size (om-make-point 8 8)
                       :help-spec "option-click to evalue or drag for connections"
                       :index 0))
     (push thenewout (outframes module))
     (om-add-subviews module thenewout )
     (setf (name module) (frame-name self))
     (setf (frames self) (list module))
     module))

(defmethod OpenEditorframe ((self score-box)) nil)

(defmethod* omNG-box-value ((self score-box) &optional (numout 0))
   (reference self))
                            
(defmethod change-music-reference ((self t) (new t)) 
   (om-beep-msg "Type mistmach!") (abort) )

(defmethod get-frame-class ((self score-box)) 'scoreboxframe)


;================================================
(omg-defclass scoreboxframe (boxframe) ())

(defmethod frame-in-score-p ((self scoreboxframe)) t)

(defmethod centre-icon  ((self scoreboxframe)) t) 

(defmethod draw-before-box ((self scoreboxframe))
  (box-revise-references self (om-view-container self))
  (om-with-focused-view self
     (om-with-fg-color nil *scorepatch-color*
       (if (active-mode self) 
         (om-with-pen (self 
                       ;:pattern *om-gray-pattern* 
                       :mode :xor)
           (om-fill-rect 0 0 (w self) (h self)))
       (om-draw-rect 0 0 (w self) (h self))))))


(defmethod omG-select ((self scoreboxframe))
   "Set the frame SELF and the object pointed for frame in selected mode"
   (when (not (active-mode self)) 
     (setf (active-mode self) t)
     (om-invalidate-view self t)))

(defmethod omG-unselect ((self scoreboxframe))
   "Set the frame SELF and the object pointed for frame in unselected mode"
   (when (active-mode self)
     (setf (active-mode self) nil)
     (om-invalidate-view self t)))


(defmethod add-lock-button ((self scoreboxframe) &optional (icon "x")) t)
(defmethod remove-lock-button ((self scoreboxframe)) t)





;========================

;CONTAINERS


(defmethod (setf mus-patch) ((val t) (self t)) t)
(defmethod mus-patch ((self t)) nil)

(defmethod remake-references ((self simple-container))
   (when (mus-patch self)
     (loop for item in (boxes (mus-patch self)) do
           (remake-reference item self))))

(defmethod remake-references ((self t)) t)

(defmethod remake-reference ((self t) obj) t)

(defmethod remake-reference ((self score-box) obj)
   (setf (reference self) (get-obj-from-container-path obj (reference self))))

;========================

;BOX IN SCORE


(defmethod save-reference ((self score-box))
   (cons-container-path (reference self)))


(defmethod omNG-save ((self score-box) &optional (values? nil))
   "Save a box"
   (let* ((inputs (mapcar #'(lambda (input) (omNG-save input values?)) (inputs self))))
     `(om-load-score-box ',(type-of self) ,(name self) ',(save-reference self) ',inputs ,(om-save-point (frame-position self)) 
                      ,(om-save-point (frame-size self))  ,(frame-name self))))

(defun om-load-score-box (class name reference inputs position size fname)
   (let* ((rep (make-instance class
                 :name name
                 :reference (reverse reference))))
     (setf (frame-position rep) (om-correct-point position))
     (setf (frame-size rep) (om-correct-point size))
     (setf (frame-name rep) fname)
     (setf (name rep) name)
     (setf (inputs rep) (mapcar #'(lambda (input) (eval input)) inputs))
     (set-box-to-inputs (inputs newbox) newbox)
     rep))



;========================

;CONSTR PATCH
(defmethod om-save ((self null) &optional (values? nil)) nil)





;======================================
;MUSBOXES IN THE SCORE
;======================================

(defmethod initialize-instance :after ((self scorePanel) &rest l ) 
   (declare (ignore l))
   (init-music-patch self))

(defmethod init-music-patch ((self scorePanel))
   (let ((object (object (editor self))))
     (unless (mus-patch object)
       (setf (mus-patch object) (make-instance 'OMPatchAbs :name "patch" :icon 210)))
     (setf (object self) (mus-patch object))))

;=================

(defmethod change-size-and-pos ((self scorePanel))
   (mapc #'(lambda (box)
             (change-size-and-pos box)) (get-subframes self)))

(defmethod change-size-and-pos ((self t)) t)
(defmethod change-size-and-pos ((self scoreboxframe))
   (let* ((ref (reference (object self)))
          (grap-container (graphic-obj (om-view-container self)))
          (grap-ref (get-correspond-grap grap-container ref))
          (size (round (staff-size (om-view-container self)) 8))
          rec)
     (when grap-ref
       (setf rec (rectangle grap-ref))
       (om-set-view-position self (om-make-point (- (first rec) size) (- (second rec) size 8)))
       (om-set-view-size  self (om-make-point (+ (* 2 size) (- (third rec) (first rec))) 
                                       (+  16 (* 2 size) (- (fourth rec) (second rec))))))))

;--------------

(defmethod revise-references ((self scorePanel))
   (mapc #'(lambda (box) (box-revise-references box self)) (get-subframes self))
   (make-move-after self (get-subframes self)))

(defmethod box-revise-references ((self t) panel) t)
(defmethod box-revise-references ((self scoreboxframe) panel)
   (let* ((object (object (om-view-container panel)))
          (pere (get-root-parent object))
          (newref (get-obj-from-container-path pere (reverse (cons-container-path  (reference (object self)))))))
     (if newref
       (progn
         (setf (reference (object self)) newref)
         (change-size-and-pos self))
       (omg-remove-element panel self))))

;=================

(defmethod set-panel-boxes ((self scorePanel))
   (let ((elements (boxes (object self)))) 
     (mapc #'(lambda (elem)
               (let ((newframe (make-frame-from-callobj elem)))
                     
                 (om-add-subviews self newframe)
                 
                 (add-subview-extra newframe))) elements)
     (mapc #'(lambda (elem)
               (update-graphic-connections elem elements)) (get-subframes self))))


(defmethod remove-panel-boxes ((self scorePanel))
   (mapc #'(lambda (elem)
             (om-remove-subviews self elem)) (get-subframes self))
   (om-invalidate-view self t))

(defmethod scorepanel-p ((self scorePanel)) t)
(defmethod scorepanel-p ((self t)) nil)

(defmethod modify-patch ((self scorePanel)) t)


(defmethod clear-ev-once :after ((self scorePanel))
   "After one evaluation this methods set the ev-once flag of all boxes in ev-once mode to nil."
   (update-panel self t))

;
;=====================================
;CONSTRAINTS

(defmethod get-posi-score-box (item size)
   (om-make-point (- (first (rectangle item)) size) 
               (+ (- (second (rectangle item)) size) 8)))

(defmethod get-size-score-box (item size)
   (om-make-point (+ (* 2 size) (- (third (rectangle item)) (first (rectangle item))))
               (+ 8 (* 2 size) (- (fourth (rectangle item)) (second (rectangle item))))))

(defmethod mk-musobj-box ((self scorePanel))
   (let* ((mode-obj (grap-class-from-type  (obj-mode self)))
          (size (round (staff-size self) 8)))
     (list mode-obj (selection? self))
     (loop for item in (get-graph-selection? self mode-obj) do
           (let* ((newconst (make-score-box  (reference item) 'score-box 
                                             (get-posi-score-box item size)
                                             "musical box"
                                             (get-size-score-box item size)))
                  (frame (make-frame-from-callobj newconst)))
             (setf (selection? self) (remove  (reference item) (selection? self) :test 'equal))
             (setf (selected item) nil)
             (omG-add-element self frame)))))


(defun non-connected-box (self list)
   (let ((rep t))
     (loop for item in list 
           while list do
           (when (is-connected? self item)
             (setf rep nil)
             (setf list nil))) rep))

(defun get-score-to-value (list)
   (let (rep)
     (mapc #'(lambda (box)
               (when (non-connected-box box list)
                 (push box rep))) list) rep))

(defmethod patch-clear-ev-once ((self ompatch))
   (mapc #'(lambda (box)
             (clear-ev-once box)) (boxes self)))

(defmethod get-all-score-patches ((self simple-container))
  (when (mus-patch self)
    (list (mus-patch self))))

(defmethod get-all-score-patches ((self container))
  (append (loop for item in (inside self)
                append (get-all-score-patches item))
          (call-next-method)))

(defmethod eval-score ((self scorePanel))
   "Eval all boxes in 'self'."
   (let ((allpatches (get-all-score-patches (object (om-view-container self)))))
     (loop for patch in allpatches do
           (loop for item in (get-score-to-value (boxes patch)) do
                 (omNG-box-value item 0))
           (patch-clear-ev-once patch))
     (update-panel self t)))



;;=========================================
;; IN / OUTS


(defclass scorein (maq-omin) ())
(defclass scoreout (maq-omout) ())

(defmethod get-new-box-from-type ((type (eql 'in)) position (container scorepanel))
  (let* ((boxes (get-subframes container)) 
         (i (length (find-class-boxes boxes 'maqinFrame)))
         (newin (make-new-patch-input (mk-unique-name container "input") i position 165 'scorein)))
    (setf (offset newin) (pixel-toms container position))
    newin
    ))

(defmethod get-new-box-from-type ((type (eql 'out)) position (container scorepanel))
  (let* ((boxes (get-subframes container)) 
         (i (length (find-class-boxes boxes 'maqoutFrame)))
         (newout (make-new-output (mk-unique-name container "output") i position 165 'scoreout)))  
    newout
    ))


(defmethod pixel2point ((self scorepanel) pixel)
   (om-make-point (pixel-toms self pixel) (om-point-v pixel)))

(defmethod pixel2point ((self voicepanel) pixel)
   (om-make-point (pixels-to-time self (om-point-x pixel)) (om-point-v pixel)))

(defmethod pixel2point ((self polypanel) pixel)
   (om-make-point (pixels-to-time self (om-point-x pixel)) (om-point-v pixel)))



(defmethod get-system-etat ((self scorepanel)) nil)

(defmethod point2pixel ((self scorepanel) point sys-etat)
  (om-make-point (ms2pixel (om-point-h point)  (/ (staff-size self) 4) (staff-zoom self)) (om-point-v point))) 

(defmethod get-offset/posy-from-pixel ((container scorepanel) pointpixel)
  (let* ((posx (pixel-toms container pointpixel))
         (posy (om-point-v pointpixel)))
    (om-make-point posx posy)))


(defmethod get-offset/posy-in-pixel (tempobj (container scorepanel))
  (om-view-position tempobj))


;====================
;DRAW BOXES
;===================

(defmethod executable-patch? ((self OMBox))
  (member "exec" (score-action self) :key 'car :test 'string-equal))

(defmethod show-always? ((self OMBox)) 
  (member "allw" (score-action self) :key 'car :test 'string-equal))

(defmethod app-box? ((self OMBox)) 
  (member "app" (score-action self) :key 'car :test 'string-equal))

(defmethod disapp-box? ((self OMBox)) 
  (member "disp" (score-action self) :key 'car :test 'string-equal))

(defmethod score-action-pos-box? ((self OMBox)) 
  (member "pos" (score-action self) :key 'car :test 'string-equal))


; Dialog for box action in the score
(defun box-action-score-dialog (box)
  (let* ((showv (car (show-always? box)))
         (appv (car (app-box? box)))
         (apptv (second appv))
         (disappv (car (disapp-box? box)))
         (disapptv (second disappv))
         (playv (car (executable-patch? box)))
         (playtv (second playv))
         (font *om-default-font1*)
         (win (om-make-window 'om-dialog
                              :window-title "Box action"
                              :position :centered 
                              :close t
                              :resizable nil
                              :maximize nil
                              :size (om-make-point 290 210)))
         (y 10)
         show app appt disapp  disappt play okb annulerb playt posi posiv deltax deltaxv deltay deltayv
         )
    (setf show (om-make-dialog-item 'om-check-box (om-make-point 20 y) (om-make-point 120 20) 
                                        "show" :font font :checked-p showv ))
    (incf y 26)
    (setf app (om-make-dialog-item 'om-check-box (om-make-point 20 y)
                                      (om-make-point 100 30) "Appear at"  :font font :checked-p appv ))
    (setf appt (om-make-dialog-item 'om-editable-text (om-make-point 120 y)
                                        (om-make-point 80 30) (format nil "~D"  apptv )  :font font))
    (incf y 26)
    (setf disapp (om-make-dialog-item 'om-check-box (om-make-point 20 y)
                                      (om-make-point 100 30) "Disappear at"  :font font :checked-p disappv))
    (setf disappt (om-make-dialog-item 'om-editable-text (om-make-point 120 y)
                                        (om-make-point 80 30) (format nil "~D" disapptv )  :font font))
    (incf y 26)
    (setf play (om-make-dialog-item 'om-check-box (om-make-point 20 y) (om-make-point 140 20) 
                                      "Play"  :font font :checked-p playv))
    (setf playt (om-make-dialog-item 'om-editable-text (om-make-point 120 y)
                                        (om-make-point 80 30) (format nil "~D" playtv)  :font font))
   ; (incf y 26)
    ;(setf posi (om-make-dialog-item 'om-static-text (om-make-point 20 (+ y 8)) (om-make-point 140 20) 
    ;                                  "Box position"  :font font))
    ;(setf posiv (om-make-dialog-item 'om-editable-text (om-make-point 120 y)
    ;                                    (om-make-point 80 30) (format nil "~D" '(0 2 1))  :font font))
    ;  (incf y 26)
    ;(setf deltax (om-make-dialog-item 'om-static-text (om-make-point 20 (+ y 8)) (om-make-point 140 20) 
    ;                                  "Delta-x (dent)"  :font font))
    ;(setf deltaxv (om-make-dialog-item 'om-editable-text (om-make-point 90 y )
    ;                                    (om-make-point 40 30) (format nil "~D" 0)  :font font))
    ;(setf deltay (om-make-dialog-item 'om-static-text (om-make-point 140 (+ y 8)) (om-make-point 140 20) 
    ;                                  "Delta-y (dent)"  :font font))
    ;(setf deltayv (om-make-dialog-item 'om-editable-text (om-make-point 210 y)
    ;                                    (om-make-point 40 30) (format nil "~D" 0)  :font font))
    (incf y 40)
    (setf okb (om-make-dialog-item 'om-button 
                                   (om-make-point 150 y)  
                                   (om-make-point 70 25)
                                   "OK"
                                   :font font
                                   :di-action (om-dialog-item-act item
                                                (declare (ignore item))
                                                (om-return-from-modal-dialog win 
                                                                             (let (rep)
                                                                                    (when (om-checked-p show)
                                                                                      (push (list "allw") rep))
                                                                                    (when (om-checked-p app)
                                                                                      (push (list "app" (read-from-string (om-dialog-item-text appt))) rep))
                                                                                    (when (om-checked-p disapp)
                                                                                      (push (list "disp" (read-from-string (om-dialog-item-text disappt))) rep))
                                                                                    (when (om-checked-p play)
                                                                                      (push (list "exec" (read-from-string (om-dialog-item-text playt))) rep))
                                                                                    ;(when rep
                                                                                    ;  (push (list "pos" (read-from-string (om-dialog-item-text posiv))
                                                                                    ;              (list (read-from-string (om-dialog-item-text deltaxv))
                                                                                    ;                    (read-from-string (om-dialog-item-text deltayv)))) rep)
                                                                                      
                                                                                    rep)))))
    (setf annulerb (om-make-dialog-item 'om-button 
                                        (om-make-point 60 y)
                                        (om-make-point 70 25)
                                        "Annuler"
                                        :font font
                                        :di-action (om-dialog-item-act item
                                                     (declare (ignore item))
                                                     (om-return-from-modal-dialog win :cancel))))
    
    (om-add-subviews win show app appt disapp  disappt play okb annulerb playt)
    (om-modal-dialog win)))


;get the list of boxes having an action
(defmethod get-score-action-boxes ((self scorePanel))
  (let* ((obj (object (om-view-container self)))
         (boxes (boxes (mus-patch obj))))
        (loop for item in boxes when (score-action item) collect item)))


;draw visible boxes in the score
(defmethod draw-editors-in-editor ((self scorePanel))
  (unless (in-patch-mode? self)
    (om-with-focused-view self
      (let* ((boxes  (score-action-boxes self)))
        (loop for item in boxes do
              (when (show-always? (car item))
                (draw-action-score-box (car item) (get-ref-chord-from-pos self (third item)) (fourth item) self self)))))))


;Execute Show and Hide Boxes in the score
(defmethod play-boxes-in-score ((self scorePanel))
  (let* ((boxes (score-action-boxes self))
         (port (if (score-page-mode self) (or (om-get-current-port) self) self)))
    ;(start 
     (loop for item in boxes do
           (loop for act in (score-action (car item)) do
                 (cond ((string-equal (car act) "exec")
                        ;(dfuncall (second act) 'play-score-box (car item))
                        (schedule-task (player (editor self))
                                       #'(lambda () (play-score-box (car item)))
                                       (second act))
                        )
                       ((string-equal (car act) "app")
                        ;(dfuncall  (second act) 'draw-action-score-box (car item) 
                        ;          (get-ref-chord-from-pos self (third item)) (fourth item) self port)
                        (schedule-task (player (editor self))
                                       #'(lambda () (draw-action-score-box (car item)
                                                                           (get-ref-chord-from-pos self (third item)) 
                                                                           (fourth item) self port))
                                       (second act))
                        )
                       ((string-equal (car act) "disp")
                        ;(dfuncall (second act) 'play-disp-score-box (car item) (get-ref-chord-from-pos self (third item)) (fourth item) self port)
                        (schedule-task (player (editor self))
                                       #'(lambda () (play-disp-score-box (car item) 
                                                                         (get-ref-chord-from-pos self (third item)) 
                                                                         (fourth item) self port))
                                       (second act))
                        )
                       )))
           ))
                           
;cons a list of box and chord  and delta


(defmethod cons-action-boxes-and-chords ((self scorePanel))
  (when (mus-patch (object (om-view-container self)))
    (let* ((obj (object (om-view-container self)))
           (boxes (boxes (mus-patch obj))) rep)
        (loop for item in boxes do 
              (when (score-action item)
                (let ((pos-s (car (score-action-pos-box? item))))
                  (when pos-s
                    (let* ((chord (second pos-s))
                           (delta (third pos-s))
                           (thechord (get-ref-chord-from-pos self chord)))
                      (add-box-action-subview item self thechord delta)
                      (push (list item (reference thechord) chord delta) rep))))))
        rep)))


(defmethod add-box-action-subview ((self OMDIebox)  view chord delta)
  (let* ((value (value self))
         points pos)
    (when chord
      (setf points (convert-delta-to-points chord (list (om-make-point (car delta) (second delta)))  (staff-size view)))
      (setf pos (car points))
      (om-set-view-position value pos)
      (om-add-subviews view value))))

(defmethod add-box-action-subview ((self t)  view chord delta) t)

;draw an editor

(defmethod get-ref-chord-from-pos ((self chordseqpanel) pos)
  (let* ((chords (inside (graphic-obj self)))
         (rep (nth (second pos) chords)))
    rep)) 

(defmethod get-ref-chord-from-pos ((self multiseqpanel) pos)
  (let* ((chords (inside (nth (first pos) (inside (graphic-obj self)))))
         (rep (nth (second pos) chords)))
    rep)) 

(defmethod get-ref-chord-from-pos ((self voicepanel) pos)
  (let* ((meslist (inside (graphic-obj self)))
         (mes (nth (second pos) meslist))
         (chords (cons-gfig-tempo-list mes))
         (rep (nth (third pos) chords)))
    rep))

(defmethod get-ref-chord-from-pos ((self polypanel) pos)
  (let* ((voicelist (inside (graphic-obj self)))
         (voice (nth (first pos) voicelist))
         (meslist (inside voice))
         (mes (nth (second pos) meslist))
         (chords (cons-gfig-tempo-list mes))
         (rep (nth (third pos) chords)))
    rep))

(defmethod draw-action-score-box ((self OMBoxEditCall) chord delta view port)
  (let ((value (value self))
        (size (frame-size self))
        points pos)
   (when chord
     (setf points (convert-delta-to-points chord (list (om-make-point (car delta) (second delta)))  (staff-size view)))
     (setf pos (car points))
     (om-with-focused-view port
       (draw-obj-in-rect value (om-point-h pos) (+ (om-point-h size) (om-point-h pos))
                         (+ (om-point-v pos) 8) (- (+ (om-point-v pos) (om-point-v size)) 8) 
                         (edition-params self)  port)))))
    

;eval a patch
(defmethod play-score-box ((self OMBoxPatch)) 
   (format *om-stream* "OM->~S~%" (omng-box-value self)))


;hide an editor
(defmethod play-disp-score-box ((self OMBoxEditCall) chord delta view port) 
  (let ((value (value self))
        (dent (round (staff-size view) 4))
        (size (frame-size self))
         pos)
   (when chord
          (setf pos (om-make-point (+ (car (rectangle chord)) (* dent (car delta)))
                                   (+ (second (rectangle chord)) (* dent (second delta)))))
    (om-invalidate-rectangle port 
                             (+ (car (rectangle chord)) (* dent (car delta)))
                             (+ (second (rectangle chord)) (* dent (second delta)))
                             (om-point-h size) (om-point-v size)))))


(defmethod object-specific-menu ((self t)) nil)

(defmethod om-get-menu-context ((self boxeditorframe))
  (remove nil 
          (list+  (list (list 
                         (import-menu (object self))
                         (export-menu (object self))
                         ))
                  (boxframe-default-list self)
                  (player-menu-item (object self))
                  (object-specific-menu (value (object self))) 
                  (when (scorepanel-p (om-view-container self))
                    (list (list (om-new-leafmenu "Score action"
                                                 #'(lambda () (edit-score-action self)))))))))


(defmethod import-menu ((self omboxeditcall))
  (import-menu-from-obj self (value self)))

(defmethod export-menu ((self omboxeditcall))
  (export-menu-from-obj self (value self)))

(defmethod import-menu-from-obj ((self omboxeditcall) (obj t))
  (om-new-leafmenu "Import" #'(lambda () (import-box-editor self))))

(defmethod export-menu-from-obj ((self omboxeditcall) (obj t))
  (om-new-leafmenu "Export" #'(lambda () (export-box-editor self))))



(defmethod om-get-menu-context ((object PictboxFrame))
  (if (scorepanel-p (om-view-container object))
             (list (om-new-leafmenu "Score action"
                                    #'(lambda () (edit-score-action object))))
    (list (om-new-leafmenu  "Set as Background Picture" #'(lambda () (pict2bkg object)))
          (pict-save-menu (object object)))))

;;; redefinition !!
(defmethod om-get-menu-context ((object commentboxframe))
 (list+ (list (list (om-new-leafmenu "Text Color" #'(lambda () (color-comments (get-patchpanel (editor (om-view-window object))))
                                                      ))
                    (om-new-leafmenu "Text Font" #'(lambda () (font-comments (get-patchpanel (editor (om-view-window object))))))))
  (when (scorepanel-p (om-view-container object))
            (list (list (om-new-leafmenu "Score action"
                                       #'(lambda () (edit-score-action object))))))))



(defmethod om-get-menu-context ((object patchboxFrame))
  (list+  (list (list (om-new-leafmenu "Update Doc"
                                       #'(lambda () (apply-win (om-view-window object) 'update-doc))))) 
          (boxframe-default-list object)
          (when (scorepanel-p (om-view-container object))
            (list (list (om-new-leafmenu "Score action"
                                       #'(lambda () (edit-score-action object))))))))


;score-action = (action (list "pos" (0 0 0) (deltax deltay)))
(defmethod edit-score-action ((self omboxFrame))
  (let* ((box (object self))
        (panel (om-view-container self))
        (size (staff-size panel))
        pos nearest points rep)
    (setf rep (catch-cancel (box-action-score-dialog box)))
    (unless (equal rep :cancel)
      (setf nearest (get-near-obj-pos-from-pixel (init-grap-for-pos (graphic-obj panel)) t (om-point-h (om-view-position self))))
      (setf pos (car nearest))
      (setf nearest (second nearest))
      (setf points (convert-points-to-delta (car (rectangle nearest)) (second (rectangle nearest)) (list (om-view-position self)) size))
      (setf (score-action box)  (append rep (list (list "pos" (cons 0 pos) (list (om-point-h (car points)) (om-point-v (car points))))))))))


;redrawing boxes
(defmethod draw-after-box ((self patchboxFrame)) 
   (when (and (scorepanel-p (om-view-container self)) (executable-patch? (object self)))
     (om-with-focused-view self
       (om-with-fg-color self *om-blue-color*
          (om-fill-rect 0 0 20 20)))))


;====

;(defmethod* redraw-score  () 
;   (update-panel (panel (om-front-window)) t))







