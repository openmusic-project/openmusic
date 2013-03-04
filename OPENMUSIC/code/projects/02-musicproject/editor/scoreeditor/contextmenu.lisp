(in-package :om)

(defmethod om-get-menu-context ((object t)) nil)

;=============================

(defun propor-menu-context (object)
  (remove nil (append 
               (list  
                (list 
                 (om-make-menu "Mode"
                               (list (list (om-new-leafmenu "Normal" #'(lambda () (change-score-mode (panel object) 0)))
                                           (om-new-leafmenu "Segmentation" #'(lambda () (change-score-mode (panel object) 3)))
                                           (om-new-leafmenu "Page" #'(lambda () (change-score-mode (panel object) 2)))
                                           (om-new-leafmenu "Patch" #'(lambda () (change-score-mode (panel object) 1)))
                                           ))))
                (if (analysis-mode? (panel object))
                    (analysis-menu-items object)
                (list 
                 (list 
                  (om-new-leafmenu (if (show-stems (panel object)) "Hide stems" "Show stems")
                                   #'(lambda () (show-hide-stems (panel object))))
                  (om-new-leafmenu "Set Score Margins"
                                   #'(lambda () (editor-page-setup (panel object)))))
                  
                 (om-new-leafmenu "Eval" #'(lambda () (eval-score (panel object))))   
                 ))))))

(defun simple-menu-context (object)
  (remove nil (append 
               (list  
                (list 
               (om-new-menu "Mode"
                            (om-new-leafmenu "Normal" #'(lambda () (change-score-mode (panel object) 0)))
                            (om-new-leafmenu "Segmentation" #'(lambda () (change-score-mode (panel object) 3)))
                            (om-new-leafmenu "Page" #'(lambda () (change-score-mode (panel object) 2)))
                            (om-new-leafmenu "Patch" #'(lambda () (change-score-mode (panel object) 1)))
                            )
                (if (analysis-mode? (panel object))
                    (analysis-menu-items object)
                  (list 
                   (list 
                    (om-new-leafmenu "Set Score Margins"
                                     #'(lambda () (editor-page-setup (panel object)))))
                   (om-new-leafmenu "Eval"
                                #'(lambda () (eval-score (panel object)))))))))))


(defmethod om-get-menu-context ((self scorepanel))
  (let ((selection (get-click-in-obj self (graphic-obj self) 
                                 'contex ;(grap-class-from-type (obj-mode self))
                                 (om-mouse-position self))))
    (if selection
        (om-get-menu-context selection)
      (om-get-menu-context (editor self)))))

(defmethod om-get-menu-context ((object scoreeditor))
  (simple-menu-context object))

(defmethod om-get-menu-context ((object voiceeditor))
  (simple-menu-context object))

(defmethod om-get-menu-context ((object polyeditor))
  (simple-menu-context object))

(defmethod om-get-menu-context ((object chordeditor))
  (propor-menu-context object))

(defmethod om-get-menu-context ((object chordseqeditor))
  (propor-menu-context object))

(defmethod om-get-menu-context ((object noteeditor)) nil)

;----------------------------
;on peut mettre quelques actions ici
;----------------------------


(defmethod om-view-window ((self simple-graph-container))
  (om-front-window))

(defmethod om-view-window ((self grap-extra-objet))
  (om-front-window))


;(defmethod menu-item-context ((self simple-graph-container) container)
  ;;; bidouy : on a passe le panel au lieu de where
;  (om-popup-menu-context self container)
;  (update-panel container)
;)

;(defmethod menu-item-context ((self grap-extra-objet) where)
;   (om-popup-menu-context self))


(defmethod om-get-menu-context ((object simple-graph-container))
  (list (om-new-leafmenu "Do Nothing..."
                         #'(lambda () 
                             (om-beep-msg "This menu do nothing")))))

(defmethod om-get-menu-context ((object grap-extra-objet))
  (list (om-new-leafmenu "Do Nothing..."
                         #'(lambda () 
                             (om-beep-msg "This menu do nothing")))))



;------Context menus for chord and rest we can attach tempo dates only to these objects
(defmethod om-get-menu-context ((object grap-rest))
  (let ((panel (panel (editor (om-front-window))))
        rep graces)
     (setf rep (list (om-new-leafmenu "Tempo Change"
                                                 #'(lambda () 
                                                     (add-tempo-change-extra (reference object) )
                                                     (update-panel panel)))))
    (when (voice-has-tempi? (reference object))
      (setf rep (append rep (list (om-new-leafmenu "Suppr. Tempo Change"
                                                   #'(lambda () 
                                                       (rmv-tempo-change-extra (reference object) )
                                                       (update-panel panel)))))))
     (setf graces (list (om-new-leafmenu "Edit grace notes"
                                                 #'(lambda () 
                                                     (add-grace-notes-dialog (reference object) )
                                                     (update-panel panel)))))
     (when (gnotes (reference object))
       (setf graces (append graces (list (om-new-leafmenu "Suppr. grace notes"
                                                   #'(lambda () 
                                                       (delete-grace-notes (reference object) )
                                                       (update-panel panel)))))))
     (append rep (list graces))))


(defmethod om-get-menu-context ((object grap-ryth-chord))
  (let* ((panel (panel (editor (om-front-window))))
         (rep (call-next-method))
         tempi graces)
    
    (setf tempi (list (om-new-leafmenu "Tempo Change"
                                       #'(lambda () 
                                           (add-tempo-change-extra (reference object) )
                                           (update-panel panel)))))
    (when (voice-has-tempi? (reference object))
      (setf tempi (append tempi (list (om-new-leafmenu "Suppr. Tempo Change"
                                                       #'(lambda () 
                                                           (rmv-tempo-change-extra (reference object) )
                                                           (update-panel panel)))))))
    (setf graces (list (om-new-leafmenu "Edit grace notes"
                                        #'(lambda () 
                                            (add-grace-notes-dialog (reference object) )
                                            (update-panel panel)))))
    (when (gnotes (reference object))
      (setf graces (append graces (list (om-new-leafmenu "Suppr. grace notes"
                                                         #'(lambda () 
                                                             (delete-grace-notes (reference object) )
                                                             (update-panel panel)))))))
    (append rep (list tempi) (list graces))))