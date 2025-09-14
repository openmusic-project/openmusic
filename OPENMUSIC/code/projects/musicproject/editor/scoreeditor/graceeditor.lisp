(in-package :om)


;;;;ADD GRACE NOTES

;;We use a chord-seq panel.
(defclass* grace-note-seq (chord-seq)()) 

(defclass graceEditor (scoreeditor) ())

(defmethod graceeditor-p ((self graceEditor)) t) 
(defmethod graceeditor-p ((self t)) nil) 

(defmethod Class-has-editor-p  ((self grace-note-seq)) t)
(defmethod get-editor-class ((self grace-note-seq)) 'graceEditor)


;CONTROLS
(defclass grace-controls-view (gracecontrols-view) ())


(defmethod initialize-instance :after ((self grace-controls-view) &rest l &key (mode 0))
  (declare (ignore l))
  (add-grace-control self mode))


(defclass gracecontrols-view (3dBorder-view) 
  ((slotedit :initform nil :accessor slotedit))
  (:default-initargs 
   :draw-with-buffer t
    :c++ *controls-color++* :c+ *controls-color+* 
    :c-- *controls-color--* :c- *controls-color-*))

(defmethod GET-staff-LIST ((self gracecontrols-view)) *chord-satff-om*)
(defmethod GET-slot-LIST ((self gracecontrols-view)) 
  '(("midic" midic) ("channel" chan) ("dur" dur) ("dyn" dyn) ("port" port) ("offset" offset) ("onset" onset)))

(defmethod GET-tone-LIST ((self gracecontrols-view)) (editor-tone-list))


;-------------INITS
(defmethod initialize-instance :after ((self gracecontrols-view) &rest l 
                                       &key (tone "1/2") (staff "ffgg") (font-size "24") (zoom 1) (mode 0) (onset 0) (measure 1))
  
  (declare (ignore l))
  (setf onsetval 0)
  (setf measnumval 1)
  (let* ((editor (om-view-container self))
         (obj (object editor))
         (panel (panel editor))
         (bgcol *controls-color*)
         (di-font *controls-font*)
         (approx (approx (object (ref (om-view-container self)))))
         ;(approx (if (listp approx) (car approx) approx)) ;for tracks obj
         ;(edoname "Tunings")
         (l1 230)
         (l2 380)
         (l3 500)
         (l4 620)
         (c1 2)
         (c2 *second-row-y*)
         
         ;;; Slot
         (minied (om-make-dialog-item 'edit-numbox (om-make-point 96 (+ c1 2)) (om-make-point 50 18) " "
                                      :value nil
                                      :font *om-default-font1*
                                      :bg-color *om-white-color*
                                      :help-spec ""
                                      ))
         

         (slotbut (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 5 c1) 
                                       (om-make-point 80 22)
                                       ""
                                       :range (loop for item in (GET-slot-LIST self) collect (car item)) 
                                       :value "midic"
				       :font di-font
                                       :di-action 
                                       (om-dialog-item-act item
                                         (let ((newslot (cadr (nth (om-get-selected-item-index item) (GET-slot-LIST self)))))
                                           (change-slot-edit (panel (om-view-container self)) newslot)))
                                       
                                       ))
         ;;; Font Size
         (sizeitem (om-make-dialog-item 'om-static-text 
                                        (om-make-point (- l1 (om-string-size "Font size" *om-default-font1*) 8) (+ c2 2)) 
                                        (om-make-point 90 16) "Font size"
					:font *om-default-font1*
                                        :bg-color *controls-color*))
         (sizebut (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point l1 c2) 
                                       (om-make-point 56 22)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                               (let ((newsize (cadr (nth (om-get-selected-item-index item) *mus-font-size*))))
                                                                 (change-editor-size (panel (om-view-container self)) newsize)))
                                       
                                       :font di-font 
                                       :range (loop for item in *mus-font-size* collect (car item)) 
                                       :value font-size
                                       ))
         
         ;;; staff
         (staffitem (om-make-dialog-item 'om-static-text (om-make-point (- l2 36) (+ c1 2)) (om-make-point 60 20) "Staff"
                                         :font *om-default-font1*
                                         :bg-color *controls-color*))
         
         (staffbut (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point l2 c1) 
                                       #+(or linux win32)(om-make-point 90 22)
                                       #+macosx(om-make-point 102 22)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                               (let ((newstaff (cadr (nth (om-get-selected-item-index item) (GET-staff-LIST self)))))
                                                                 (change-system (panel (om-view-container self)) newstaff)))
                                       
                                       :font di-font
                                       :range (loop for item in (GET-staff-LIST self) collect (car item)) 
                                       :value (car (find (get-edit-param (editor (om-view-container self)) 'staff) (GET-staff-LIST self) :key 'cadr :test 'equal))
                                       ))   
     
                  ;;;  SET
         (okbutton (om-make-dialog-item 'om-button
                                      #+linux(om-make-point l2 c2)
                                      #+win32(om-make-point l2 (- c2 3))
                                      #+macosx(om-make-point (- l2 5) (- c2 5))
                                      #+(or linux win32) (om-make-point 90 25)
                                      #+macosx(om-make-point 110 30)
                                      (format nil "SET") 
                                      :font *om-default-font1*
                                      :di-action 
                                      (om-dialog-item-act item
                                                       (declare (ignore item)) 
                                                        (om-close-window editor))
                                      ))
         
                  
         ;;;selection
         #|
         (duration (om-make-dialog-item 'om-static-text (om-make-point (+ l4 5) (+ c1 2)) (om-make-point 260 50) 
                                        ""
                                         :font *om-default-font1*
                                         :bg-color *controls-color*))
         |#
         )
         
    (setf (slotedit self) minied)
    (om-add-subviews self ;duration
                     staffitem staffbut sizeitem slotbut 
                     minied ;ca c'est les slots
                     sizebut 
                     okbutton)
    ;;(additional-port-menu (title-bar (om-view-container self)) :pos (om-make-point 300 4) :color *editor-bar-color*)
    (add-zoom2control self zoom (om-make-point l1 c1))
    
    (progn
      (setf (approx (object (om-view-container self))) approx)
      (setf (staff-tone (panel (om-view-container self))) approx))
    (om-set-bg-color self *controls-color*)
    )
)


;same as add-chordseq-control (so maybe not neede to replicate!)
(defun add-grace-control (self mode) nil)

;;; when slotedit has no slot "edit" :)
(defmethod edit ((self t)) nil)

(defmethod om-view-click-handler ((self gracecontrols-view) pos)
  (when (edit (slotedit self))
    (exit-from-dialog (edit (slotedit self)) (om-dialog-item-text (edit (slotedit self)))))
  (call-next-method))

;===========================================
;===========================================
;===========================================


;VIEW

(defmethod editor-null-event-handler :after ((self graceEditor))
  (do-editor-null-event self))


(defmethod init-draw ((self graceEditor)) 
   (setf (grille-step (panel self)) (get-edit-param self 'grillestep))
   (update-panel (panel self)))

(defmethod get-score-class-ctrls ((self graceEditor)) 'grace-controls-view)
(defmethod get-score-class-panel ((self graceEditor)) 'gracepanel)

(defmethod do-editor-null-event ((self graceEditor))
  #+linux (om-invalidate-view (panel self)) 
  #+(or linux win32)(when (equal (state (player self)) :play)
                      (capi:manipulate-pinboard (panel self) 
                                                (slot-value (panel self) 'oa::animation)
                                                :add-top))
  (when (om-view-contains-point-p (panel self) (om-mouse-position self))
    (show-position-ms self (pixel-toms (panel self) (om-mouse-position (panel self))))))


;PANEL

(defclass gracepanel (chordseqPanel)  
   ((grille-p :initform nil :accessor grille-p)
    (grille-step :initform 1000 :accessor grille-step)
    (clic-pos :initform nil :accessor clic-pos)
    ))


(defmethod grille-step-p ((self gracepanel)) nil)
;   (if (grille-p self) (grille-step self)))

(defmethod show-tempo ((self scorePanel)) nil)


(defmethod draw-view-contents ((self gracepanel))
  (let* ((x0  (om-h-scroll-position self))
         (y0  (om-v-scroll-position self))
         (size (staff-size self))
         (deltax (round (get-key-space self)))
         (deltay (round (* size (score-top-margin self)))))
    (when (and (linear? self) (cursor-p self))
      (draw-interval-cursor self))
    (om-with-focused-view self 
      (om-with-font (get-font-to-draw 0) 
                    ;(om-make-font *heads-font* size)
                    (draw-system-only self)
                    (when (graphic-obj self)
                      (om-with-clip-rect self 
                          (om-make-rect (+ x0 (- deltax (round size 2))) 
                                        y0  
                                        (+ x0 (w self)) 
                                        (+ y0 (h self)))
                        ;#+linux (draw-system-only self)
                        (scorepanel-draw-object self x0 y0 deltax deltay size)
                        (if (analysis-mode? self) (draw-analysis self))
                        (draw-score-selection (graphic-obj self) (selection? self) (staff-sys self) size)
                        (draw-edit-cursor self deltay)
                        )
                      )
                    ))
    ))

(defmethod scorepanel-draw-object ((self gracepanel) x0 y0 deltax deltay size)
  (draw-object (graphic-obj self) self deltax 
                                     (- deltay (round (* (posy (car (staff-list (staff-sys self)))) (/ size 4)))) 
                                     (staff-zoom self) x0 (+ x0 (w self)) y0 (+ y0 (h self))
                                     (slots-mode self) size (linear? self) (staff-sys self) (grille-step-p self) (noteaschan? self)))


(defmethod edit-preferences ((self gracepanel))
   (setf (linear? self) (not (linear? self)))
   (update-panel self))


(defmethod control-actives ((self gracepanel) where)
  (if (and (linear? self) (cursor-p self))
    (new-interval-cursor self where)
    (call-next-method)))


(defmethod scroll-play-window ((self gracepanel))
   (let ((delta (get-key-space self)))
     (om-set-scroll-position self (om-make-point 
                                   (- (+ (om-h-scroll-position self) (w self)) delta) 
                                   (om-v-scroll-position self)
                                   ))))



(defmethod get-graph-select-chords ((self gracepanel))
   (remove-duplicates (loop for item in (get-graph-selection? self (grap-class-from-type (obj-mode self))) 
                            collect  item) :test 'equal))

(defmethod translate-chords-p ((self gracepanel)) t)

(defmethod display-time-selection ((self gracepanel)) nil)

(defmethod move-chords-in-x ((self gracepanel) first-mouse new-mouse chords zero?)
   (let ((chordseq (object (om-view-container self)))
         (delta (pixel2ms  (- (om-point-h first-mouse) (om-point-h new-mouse)) 
                           (/ (staff-size self) 4) (staff-zoom self))))
     (unless (or zero? (>= (- (offset (car chords)) delta) 0))
       (setf delta (offset (car chords))))
     (all-chords-2-ms chordseq)
     (change-chords-in-x self chords (- delta)) 
     (normalize-chords-x chordseq)
     (update-panel self t)
     (om-invalidate-view self)
     t))


(defmethod set-chords-offset ((self gracepanel) objects newoffset)
  (when (translate-chords-p self)
    (all-chords-2-ms (object (om-view-container self)))
    (loop for chord in (list! objects) do
          (setf (offset chord) newoffset))
    (normalize-chords-x (object (om-view-container self)))
    (update-panel self t)))



(defmethod handle-key-event ((self gracepanel) char)
  (if (analysis-mode? self)
      (analysis-handle-key-event self char)
      (case char
	;(#\g (set-unset-grille self))
	;(#\G (edit-step-grille self))
	(#\a (if (equal (slots-mode self) 'dur)
		 (adjoust-grille-durs self)
		 (adjoust-grille-chords self)))
	(#\z (set-cursor-mode (editor self)))
	(otherwise (call-next-method)))))


(defun interchange-chords (oldchord newobject)
  (setf (Lmidic oldchord) (Lmidic newobject)
        (LVel oldchord) (LVel newobject)
        (LOffset oldchord) (LOffset newobject)
        (LChan oldchord) (LChan newobject))
  (when (cont-chord-p oldchord)
    (tie-chord oldchord (state oldchord))))


(defmethod change-in-int-editor ((self gracepanel) (internal scorePanel) (newobject chord) oldchord)
   (unless (equal oldchord newobject)
     (interchange-chords oldchord newobject) 
     (setf (object (om-view-container internal)) oldchord))
   (change-ties-too  self oldchord))


(defmethod change-ties-too ((self gracepanel) notes)
   (update-panel self t))

  
(defmethod set-name-to-mus-obj ((self gracepanel))
  (let ((name (om-get-user-string "Voice name:" :initial-string (or (get-name (object (editor self))) ""))))
    (when name
      (set-name (object (editor self)) name)
      (om-invalidate-view self t))))


(defmethod get-help-list ((self gracepanel)) 
  (list '(
          #+macosx("alt+clic" "Add Chord/Note")
          #+(or linux win32)("ctrl+clic" "Add Chord/Note")
          ("del" "Delete Selection")
          ("tab" "Change Obj. Mode")
          (("z") "Obj/Time Selection")
          ("ud" "Transpose Selection")
          ("shift+ud" "Transpose Octave")
          ("alt+ud" "Transpose Fifth")
          ("alt+grab" "Transpose/Move Chord/Note")
          ("shift+grab" "Duplicate Chord/Note")
          ("lr" "Change Offsets/Dur.")
          (("*") "Group Chords")
          (("+") "Union Chords (Group + Offset)")
          (("c") "Show Channel Color")
          (("i") "Show Selection info"))
         
        '((("g") "Show/Hide Grid")
          (("G") "Edit Grid Step")
          (("a") "Adjust Chords/Durs to Grid")
          (("C") "Change Color")
          (("S") "Set Editor Scale")
          (("t" "T") "Set/Remove Tonality")
          (("x") "Extra Edition Palette")
          (("n") "Set Voice Name")
          (("o") "Open Internal Chord Editor")
          ("space" "Play/Stop")
          (("q") "Start Midi Record")
          (("w") "Stop Midi Record")
          ("esc" "Stop  + Reset")
	  )))


(defmethod update-slot-edit ((self gracePanel))
  (let* ((ed (om-view-container self))
         (obj (object ed))
         (control (slotedit (ctr-view (om-view-container self))))
         (slotmode (slots-mode self))
         (firstnote nil))
      (loop for obj in (reverse (selection? self)) while (not firstnote) do
            (setq firstnote (get-first-note obj)))
      ;(enable-numbox control nil)
      (when firstnote ; (and (string-equal (obj-mode self) "note") firstnote)
        (enable-numbox control t)
        (set-value control nil)
        (cond
         ((equal slotmode 'dur)
          (setf (min-val control) 0)
          (setf (max-val control) 100000)
          (set-value control (extent->ms firstnote))
          (setf (afterfun control) 
                #'(lambda (x)  
                    (loop for item in (selection? self) do
                          (set-dur item (value x)))
                    #-macosx(update-panel self)
                    #+macosx(unless (in-page-mode? self) (update-alt-panel self))
                    )))
        ((equal slotmode 'port)
         (setf (min-val control) 0)
         (setf (max-val control) 255)
         (set-value control (port firstnote))
         (setf (afterfun control) 
               #'(lambda (x)  
                   (loop for item in (selection? self) do
                         (set-port item (value x)))
                   #-macosx(update-panel self)
                   #+macosx(unless (in-page-mode? self) (update-alt-panel self))
                   )))
        
        ((equal slotmode 'offset)
         (setf (min-val control) -100000)
         (setf (max-val control) 100000)
         (set-value control (offset->ms firstnote))
         (setf (afterfun control) 
               #'(lambda (x)  
                   (loop for item in (selection? self) do
                         (set-offset-ms item (value x)))
                   #-macosx(update-panel self)
                   #+macosx(unless (in-page-mode? self) (update-alt-panel self))
                   )))
        ((equal slotmode 'onset)
	 (enable-numbox control (if (string-equal (obj-mode self) "chord") t nil))
         (setf (min-val control) 0)
         (setf (max-val control) (+ (offset->ms firstnote self) 100000))
	 (set-value control (offset->ms firstnote self))
	 (setf (afterfun control)
	       (progn
		 #'(lambda (x)
		     (loop
			for item in (selection? self)
			do (set-chords-offset self item (value x)))
                     #-macosx(update-panel self)
                     #+macosx(unless (in-page-mode? self) (update-alt-panel self))
                     ))))
	((equal slotmode 'dyn)
         (setf (min-val control) 0)
         (setf (max-val control) 127)
         (set-value control (vel firstnote))
         (setf (afterfun control)
               #'(lambda (x) 
                   (loop for item in (selection? self) do
                         (set-vel item (value x)))
                   #-macosx(update-panel self)
                   #+macosx(unless (in-page-mode? self) (update-alt-panel self))
                   )))
        ((equal slotmode 'chan)
         (setf (min-val control) 1)
         (setf (max-val control) *chan-count*);16
         (set-value control (chan firstnote))
         (setf (afterfun control)
               #'(lambda (x) 
                   (loop for item in (selection? self) do
                         (set-channel item (value x)))
                   #-macosx(update-panel self)
                   #+macosx(unless (in-page-mode? self) (update-alt-panel self))
                   )))
        ((equal slotmode 'midic)
         (setf (min-val control) 0)
         (setf (max-val control) 12700)
         (set-value control (midic firstnote))
         (setf (afterfun control) 
               #'(lambda (x) 
                   (loop for item in (selection? self) do
                         ;(when (note-p item)
                        (change-midic item (value x)))
					;);scroll edit fix
                  #-macosx(update-panel self)
                  #+macosx(unless (in-page-mode? self) (update-alt-panel self))
                  )))
        ((equal slotmode 'chord-offset)
         (setf (min-val control) 0)
         (setf (max-val control) 1000000)
         (set-value control (offset->ms (object (om-view-container self))))
         (setf (afterfun control) 
               #'(lambda (x) 
                   (all-chords-2-ms (parent (object (om-view-container self))))
                   (setf (offset (object (om-view-container self))) (max 0 (value x))) 
                   (normalize-chords-x (parent (object (om-view-container self))))
                   (update-panel self t)
                   )))
        ((equal slotmode 'non)
         (setf (min-val control) 0)
         (setf (max-val control) 0)
         (setf (afterfun control) 
               #'(lambda (x) (declare (ignore x)) (om-beep))))))   
      (om-invalidate-view self t)))


;;;this is for grace panel:
#|
(defmethod add-grace-notes-dialog ((self simple-container)) 
  (let ((root (get-root-parent self)))
    (cond ((voice-p root) (open-add-grace-panel (get-voice self) self))
          ((poly-p root) (open-add-grace-panel (get-poly self) self))
          (t (print "Only for VOICE and POLY editors!")))))
|#

(defmethod add-grace-notes-dialog ((self t) (panel t))
  (om-message-dialog "Please choose CHORD selection mode only!"))
  
(defmethod add-grace-notes-dialog ((self chord) (panel scorepanel))
  (let ((root (get-root-parent self)))
    (cond ((voice-p root) (open-add-grace-panel (get-voice self) self panel))
          ((poly-p root) (open-add-grace-panel (get-poly self) self panel))
          (t (print "Only for VOICE and POLY editors!")))))


(defmethod open-add-grace-panel ((self voice) thing panel)
  (let* ((gnotes (gnotes thing))
         (editor (om-view-container panel))
         (chrdseq (make-instance 'grace-note-seq 
                                 :approx (approx self); C'est la!!
                                 :lmidic (if gnotes (mapcar 'lmidic (glist gnotes)) '(6000))
                                 ))
         (internal (obj-for-internal-editor chrdseq))
         (win (make-editor-window 'graceeditor chrdseq "Grace note editor" editor)))
    (setf (approx chrdseq) (approx self));ADD
    (push win (attached-editors editor))))


(defmethod open-add-grace-panel ((self poly) thing panel)
  (let* ((gnotes (gnotes thing))
         (editor (om-view-container panel))
         (chrdseq (make-instance 'grace-note-seq 
                                 :approx (approx self); C'est la!!
                                 :lmidic (if gnotes (mapcar 'lmidic (glist gnotes)) '(6000))
                                 ))
         (internal (obj-for-internal-editor chrdseq))
         (win (make-editor-window 'graceeditor chrdseq "Grace note editor" editor)))
    (setf (approx chrdseq) (approx self));ADD
    (push win (attached-editors editor))))
