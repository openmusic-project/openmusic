;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (c) 1997-... IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
;;; Music package 
;;; authors G. Assayag, C. Agon, J. Bresson, K. Haddad
;=========================================================================

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

                 (cond ((analysis-mode? (panel object))
                        (analysis-menu-items object))
                       ((in-patch-mode? (panel object))
                        (scorepatch-menu-items object))
                       (t (list 
                           (list 
                            (om-new-leafmenu "Set Score Margins"
                                             #'(lambda () (editor-page-setup (panel object)))))
                           (om-new-leafmenu "Eval"
                                            #'(lambda () (eval-score (panel object))))))))))))

(defmethod scorepatch-menu-items ((self scoreeditor))
  (let ((posi (om-mouse-position self))
        (panel (panel self)))
    (list 
     (om-new-leafmenu "Comment" #'(lambda () 
                                    (let ((newbox (omNG-make-new-boxcall 'comment posi "comment")))
                                      (when newbox
                                        (omG-add-element (panel self) (make-frame-from-callobj newbox))
                                        ))))
     (list 
      (om-package-fun2menu *om-package-tree* nil #'(lambda (f) (add-box-from-menu f posi)))
      (om-package-classes2menu *om-package-tree* nil #'(lambda (c) (add-box-from-menu c posi)))
      )
     (list 
      (om-new-menu "Internal..." 
                   (om-new-leafmenu "Patch" #'(lambda () (omG-add-element panel (make-frame-from-callobj 
                                                                                 (omNG-make-new-boxcall 
                                                                                  (make-instance 'OMPatchAbs :name "mypatch" :icon 210)
                                                                                  posi (mk-unique-name panel "mypatch"))))))
                   (om-new-leafmenu "Maquette" #'(lambda () (omG-add-element panel (make-frame-from-callobj 
                                                                                    (omNG-make-new-boxcall 
                                                                                     (make-instance 'OMMaqAbs :name "mymaquette" :icon 265)
                                                                                     posi (mk-unique-name panel "mymaquette"))))))
                   (om-new-leafmenu "Loop" #'(lambda () (add-box-from-menu (fdefinition 'omloop) posi)))
                   (om-new-leafmenu "Lisp Function" #'(lambda () (omG-add-element panel 
                                                                                  (make-frame-from-callobj 
                                                                                   (omNG-make-new-boxcall 
                                                                                    (make-instance 'OMLispPatchAbs :name "lispfunction" :icon 123)
         
                                                                                    posi (mk-unique-name panel "lispfunction")))))))))))


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
                                                     (add-grace-notes-dialog (reference object) panel)
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
                                            (add-grace-notes-dialog (reference object) panel)
                                            (update-panel panel)))))
    (when (gnotes (reference object))
      (setf graces (append graces (list (om-new-leafmenu "Suppr. grace notes"
                                                         #'(lambda () 
                                                             (delete-grace-notes (reference object) )
                                                             (update-panel panel)))))))
    (append rep (list tempi) (list graces))))