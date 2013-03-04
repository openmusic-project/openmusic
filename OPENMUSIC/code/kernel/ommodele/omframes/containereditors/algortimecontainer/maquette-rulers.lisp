;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2010 IRCAM-Centre Georges Pompidou, Paris, France.
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
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;Maquette Editor Rulers.
;Last Modifications :
; 03/02/10
;DocFile

(in-package :om)

;-------------------------------------------------
; X RULER
;-------------------------------------------------
(omg-defclass maq-ruler (ruler) 
  ((x-step :accessor x-step :initarg :x-step :initform 1)))


(defmethod change-x-params  ((self maq-ruler) list)
   (let* ((container (panel (om-view-container self))))
     (setf (x-step self) (car list))
     (setf (xparam (params (object container))) (list (x-step self)))
     (setf (car (snap (params (object container)))) (cadr list))
     ))

(defmethod om-view-click-handler ((self maq-ruler) where) 
   (if (om-add-key-p)
     (add-new-marquer self where)
     (let* ((container (panel (om-view-container self)))
            (frames (get-subframes container)))
       (call-next-method)
       (make-move-after container frames))))

(defmethod add-new-marquer  ((self maq-ruler) where)
  (let* ((x (om-point-h (pixel2point (assoc-view self) where))))
    (omg-add-element (assoc-view self) 
                     (make-frame-from-callobj (omNG-make-new-marker (om-make-point (om-point-h where) (- (h (assoc-view self)) 26))
                                                                    (mk-unique-name (assoc-view self) "marker") x)))))

(defmethod om-view-cursor ((self maq-ruler))
 (if (om-add-key-p) *mark* (call-next-method)))

(defmethod strech-ruler-release ((view maq-ruler) pos)
  (call-next-method)
  (when (rulermetric (editor (assoc-view view)))
    (om-invalidate-view (rulermetric (editor (assoc-view view))) t)))

;-------------------------------------------------
;;; VERTICAL RULER
;-------------------------------------------------
(omg-defclass maq-y-ruler (ruler) 
   ((y-step :accessor y-step :initarg :y-step :initform 1)
    (force :accessor force :initarg :force :initform t)))

(defun edit-y-ruler (ruler)
  (let ((changes (edit-y-params ruler)))
       (when (consp changes)
           (change-y-params ruler changes)
           (om-invalidate-view ruler t))))
  
(defmethod om-view-doubleclick-handler ((self maq-y-ruler) where) nil)
;  (edit-y-ruler self))

(defmethod om-get-menu-context ((self maq-y-ruler)) nil)
;  (list (om-new-leafmenu "Y Settings..." #'(lambda () (edit-y-ruler self)))))

;;; y params = step , force-t-ou-nil
(defmethod change-y-params  ((self maq-y-ruler) list)
   (let* ((container (panel (om-view-container self))))
     (setf (y-step self) (car list))
     (setf (yparam (params (object container))) (list (y-step self)))
     (setf (cadr (snap (params (object container)))) (cadr list))
     ))

(defmethod edit-y-params  ((self maq-y-ruler))
  (let ((dialog (om-make-window 'om-dialog
                                :window-title "Y Settings"
                                :position :centered
                                :size (om-make-point 290 150)
                                :maximize nil :resizable nil
                                :font *om-default-font4*
                                :bg-color (om-make-color 0.623 0.623 0.623)))
        (steplabel (om-make-dialog-item 'om-static-text (om-make-point 20 20) (om-make-point 80 20) "Y Step"
                                        :font *controls-font*))
        (stepdi (om-make-dialog-item 'om-editable-text (om-make-point 100 15) (om-make-point 40 16)
                                     (num2string (y-step self))))
        
        (flabel (om-make-dialog-item 'om-static-text (om-make-point 20 60) (om-make-point 80 20) "Magnetic"
                                     :font *controls-font*))
        (fdi (om-make-dialog-item 'om-check-box (om-make-point 100 60) (om-make-point 20 20) ""
                                  :checked-p (force self)))
        )
    
    (om-add-subviews dialog steplabel stepdi flabel fdi 
                     (om-make-dialog-item 'om-button (om-make-point 110 100) (om-make-point 80 24) "Cancel" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog nil)))
                     (om-make-dialog-item 'om-button (om-make-point 190 100) (om-make-point 80 24) "OK" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (let ((step (read-from-string (om-dialog-item-text stepdi))))
                                                         (if (integerp step)
                                                           (om-return-from-modal-dialog dialog 
                                                                                        (list step (om-checked-p fdi)))
                                                           (progn (om-message-dialog "Bad Step Value !!")
                                                                  (om-return-from-modal-dialog dialog nil)))))
                                          :default-button t))
    (om-modal-dialog dialog)))

;-------------------------------------------------
;This is the optional metric ruler
;-------------------------------------------------
(omg-defclass metric-ruler (om-view-drop ruler-metric) ())

(defmethod draw-metric  ((self metric-ruler) mesure)
  (om-with-fg-color self *om-dark-gray-color*
    (om-with-font ;(om-make-font "verdana" #+cocoa 8 #-cocoa 7)
                  (om-make-font "verdana" 8)
                  (om-draw-string 6 8 (number-to-string (car mesure)))
                  (om-draw-string 6 18 (number-to-string (cadr mesure)))
                  )
    (om-draw-line 4 9 13 9)
    ))

(defmethod om-draw-contents ((self metric-ruler))
  (let* ((params (params (object (editor (assoc-view self)))))
         (mesure (car (second (metricparam params)))))   
    (om-with-focused-view self
      (draw-metric self mesure)))
    (call-next-method)
    )

(defmethod om-view-drag-hilite-p ((self metric-ruler)) nil)

(defmethod om-view-click-handler ((self metric-ruler) where)
  (cond
   ((om-add-key-p) (add-new-marquer self where))
   (t  (let* ((container (panel (om-view-container self)))
              (frames (get-subframes container)))
         (call-next-method)
         (om-invalidate-view (rulerx container) t)
         (make-move-after container frames)   ;;; ?? pourquoi ??
         ))))

(defun edit-metrics (ruler)
  (let ((changes (edit-metric-ruler ruler)))
    (when (consp changes)
      (change-params ruler changes)
      (om-invalidate-view (om-view-container ruler) t)
      (om-invalidate-view ruler t))))

(defmethod om-view-doubleclick-handler  ((self metric-ruler) where)
  (if (om-add-key-p) 
      (add-new-marquer self where)
    (edit-metrics self)))   


(defmethod om-get-menu-context ((self metric-ruler))
  (list (om-new-leafmenu "Metric Settings..." #'(lambda () (edit-metrics self)))))


(defmethod add-new-marquer  ((self metric-ruler) where)
  (let* ((x (om-point-h (pixel2point (assoc-view self) where))))
    (omg-add-element (assoc-view self) 
                     (make-frame-from-callobj (omNG-make-new-marker (om-make-point (om-point-h where) (- (h (assoc-view self)) 26))
                                                                    (mk-unique-name (assoc-view self) "marker")   x)))))


(defmethod strech-ruler-release ((view metric-ruler) pos)
  (call-next-method)
  (om-invalidate-view (rulerx (assoc-view view)) t))

;;===========================================

(omg-defclass change-tempo-item (om-view)
  ((temponum :initform 4 :initarg :temponum :accessor temponum)))

(defmethod om-draw-contents ((self change-tempo-item))
  (om-with-focused-view self
                        (om-with-font (om-get-font self)
                                      (om-draw-string 5 (- (h self) 6) (get-stringfrom-num (temponum self))))))

(defmethod om-view-click-handler ((self change-tempo-item) pos)
  (declare (ignore pos))
  (update-tempo-item self))


(defmethod update-tempo-item ((self change-tempo-item))
   (let ((numtempo (temponum self))
          (strin nil))
     (loop while (not strin) do
           (setf numtempo (if (zerop (mod (+ numtempo 1) 17)) 1 (mod (+ numtempo 1) 17)))
           (setf strin (get-stringfrom-num numtempo))
           )
     (setf (temponum self)  numtempo)
     (om-invalidate-view self t)))

(defmethod edit-metric-ruler  ((self metric-ruler))
  (let ((dialog (om-make-window 'om-dialog
                                :window-title "Metric Settings"
                                :position :centered
                                :size (om-make-point 290 210)
                                :maximize nil :resizable nil
                                :font *om-default-font4*
                                :bg-color (om-make-color 0.623 0.623 0.623)))
        (metric (om-make-dialog-item 'om-editable-text (om-make-point 60 52) (om-make-point 200 20) 
                                     (num2string (meslist self))))
        (tempo1 (om-make-view 'change-tempo-item :position (om-make-point 60 6) :size (om-make-point 24 36)
                              :font (om-make-music-font *heads-font* 24)
                              :temponum (first (tempo self))
                              ))
        (tempo2 (om-make-dialog-item 'om-editable-text (om-make-point 105 10) (om-make-point 40 16)
                                     (num2string (second (tempo self))) ))
        (loop-p (om-make-dialog-item 'om-radio-button (om-make-point 100 85) (om-make-point 120 20) "Loop measures"
                                     :checked-p (loop-mes-p self)
                                     :font *controls-font*
                                     :radio-button-cluster 'loop))
        (last-p (om-make-dialog-item 'om-radio-button  (om-make-point 100 110) (om-make-point 200 20) "Repeat last measure"
                                     :checked-p (not (loop-mes-p self))
                                     :font *controls-font*
                                     :radio-button-cluster 'loop)))
    (om-add-subviews dialog metric  loop-p last-p tempo1 tempo2  ; maxsub
                     (om-make-dialog-item 'om-static-text (om-make-point 3 12) (om-make-point 100 20) "Tempo"
                                          :font *om-default-font2*)
                     (om-make-dialog-item 'om-static-text (om-make-point 3 55) (om-make-point 40 20) "Metric" 
                                          :font *om-default-font2*)
                    
                     (om-make-dialog-item 'om-button (om-make-point 80 155) (om-make-point 80 24) "Cancel" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog nil)))
                     (om-make-dialog-item 'om-button (om-make-point 160 155) (om-make-point 80 24) "OK" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog (list (list (temponum tempo1) 
                                                                                                       (read-from-string (om-dialog-item-text tempo2)))
                                                                                                 (read-from-string (om-dialog-item-text metric))
                                                                                                 (om-checked-p loop-p))))
                                          :default-button t))
    (om-modal-dialog dialog)))
  
(defmethod change-params  ((self metric-ruler) list)
   (let* ((container (panel (om-view-container self)))
          (params (metricparam (params (object container)))))
     (setf (tempo self) (nth 0 list))
     (setf (nth 0 params) (nth 0 list))
     (setf (meslist self) (nth 1 list))
     (setf (nth 1 params) (nth 1 list))
     (setf (loop-mes-p self) (nth 2 list))
     (setf (nth 3 params) (nth 2 list))  
     ))

;D&D

(defmethod om-drag-selection-p ((self metric-ruler) mouse-position)
  (declare (ignore mouse-position)) nil)

(defmethod om-drag-receive ((view metric-ruler) (dragged-view t) position &optional (effect nil)) nil)


;;;=================
;;; DIALOGS

(defmethod edit-rulers-settings  ((self ommaquette))
  (let ((rep (snap-dialog self)))
    (when rep
      (let ((x-snap (car (car rep)))
            (step-x (cadr (car rep)))
            (max-div (caddr (car rep)))
            (y-snap (car (cadr rep)))
            (step-y (cadr (cadr rep))))
        (setf (snap (params self)) (list x-snap y-snap))
        (setf (xparam (params self)) (list step-x))
        (setf (yparam (params self)) (list step-y))
        (setf (nth 2 (metricparam (params self))) max-div)
        ))))

(defun snap-dialog (self)
  (let* ((dialog (om-make-window 'om-dialog
                                :window-title "rulers Settings"
                                :position :centered
                                :size (om-make-point 340 260)
                                :maximize nil :resizable nil
                                :font *om-default-font4*
                                :bg-color (om-make-color 0.623 0.623 0.623)))
        (htext (om-make-dialog-item 'om-static-text (om-make-point 20 5) (om-make-point 200 20) "Horizontal snap (Time)"
                                        :font *om-default-font2b*))
        
        (abslabel (om-make-dialog-item 'om-static-text (om-make-point 140 32) (om-make-point 70 20)
                                       "Unit (ms)" :font *om-default-font2* 
                                       :enable (equal (car (snap (params self))) 'abs)))
        (absdi (om-make-dialog-item 'om-editable-text (om-make-point 220 30) (om-make-point 50 20)
                                     (num2string (car (xparam (params self))))
                                     :enable (equal (car (snap (params self))) 'abs)))
        
        (metriclabel (om-make-dialog-item 'om-static-text (om-make-point 140 57) (om-make-point 120 20)
                                          "Measure subdivision" :font *om-default-font2* 
                                          :enable (and 
                                                   (equal :on (maq-show-ruler self))
                                                   (equal (car (snap (params self))) 'metric))))
        (metricdi (om-make-dialog-item 'om-editable-text (om-make-point 265 55) (om-make-point 50 20)
                                     (num2string (third (metricparam (params self))))
                                     :enable (and 
                                              (equal :on (maq-show-ruler self))
                                              (equal (car (snap (params self))) 'metric))))
        
        (habschoice (om-make-dialog-item 'om-radio-button (om-make-point 40 30) (om-make-point 80 20) "Absolute"
                                        :font *om-default-font2*
                                        :checked-p (equal (car (snap (params self))) 'abs)
                                        :di-action (om-dialog-item-act item 
                                                     (om-enable-dialog-item abslabel t)
                                                     (om-enable-dialog-item absdi t)
                                                     (om-enable-dialog-item metriclabel nil)
                                                     (om-enable-dialog-item metricdi nil))
                                                     ))
        
        (hmetricchoice (om-make-dialog-item 'om-radio-button (om-make-point 40 55) (om-make-point 80 20) "Metric"
                                            :font *om-default-font2*
                                            :enable (equal :on (maq-show-ruler self))
                                            :checked-p (equal (car (snap (params self))) 'metric)
                                            :di-action (om-dialog-item-act item 
                                                         (om-enable-dialog-item abslabel nil)
                                                         (om-enable-dialog-item absdi nil)
                                                         (om-enable-dialog-item metriclabel t)
                                                         (om-enable-dialog-item metricdi t))))


        
        (hnochoice (om-make-dialog-item 'om-radio-button (om-make-point 40 80) (om-make-point 80 20) "None"
                                            :font *om-default-font2*
                                            :checked-p (null (car (snap (params self))))
                                            :di-action (om-dialog-item-act item 
                                                         (om-enable-dialog-item abslabel nil)
                                                         (om-enable-dialog-item absdi nil)
                                                         (om-enable-dialog-item metriclabel nil)
                                                         (om-enable-dialog-item metricdi nil))))
        
        (vtext (om-make-dialog-item 'om-static-text (om-make-point 20 130) (om-make-point 200 20) "Vertical snap (Y)"
                                        :font *om-default-font2b*))


        (steplabel (om-make-dialog-item 'om-static-text (om-make-point 140 157) (om-make-point 80 20) "Step"
                                        :font *om-default-font2*
                                        :enable (cadr (snap (params self)))))

        (stepdi (om-make-dialog-item 'om-editable-text (om-make-point 200 155) (om-make-point 40 20)
                                     (num2string (car (yparam (params self))))
                                     :enable (cadr (snap (params self)))))
        
        (ycheck (om-make-dialog-item 'om-check-box (om-make-point 40 155) (om-make-point 80 20) " Activate"
                                     :checked-p (cadr (snap (params self)))
                                     :font *om-default-font2*
                                     :di-action (om-dialog-item-act item 
                                                         (om-enable-dialog-item steplabel (om-checked-p item))
                                                         (om-enable-dialog-item stepdi (om-checked-p item))
                                                         )))
        )
    
    (om-add-subviews dialog htext habschoice abslabel absdi hmetricchoice metriclabel metricdi hnochoice vtext ycheck steplabel stepdi 
                     (om-make-dialog-item 'om-button (om-make-point 80 210) (om-make-point 85 24) "Cancel" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog dialog nil)))
                     (om-make-dialog-item 'om-button (om-make-point 170 210) (om-make-point 85 24) "OK" 
                                          :di-action (om-dialog-item-act item
                                                       (declare (ignore item))
                                                       (let ((stepx (read-from-string (om-dialog-item-text absdi)))
                                                             (maxdiv (read-from-string (om-dialog-item-text metricdi)))
                                                             (xsnap (if (om-checked-p hmetricchoice) 'metric
                                                                      (if (om-checked-p habschoice) 'abs nil)))
                                                             (stepy (read-from-string (om-dialog-item-text stepdi)))
                                                             (ysnap (om-checked-p ycheck)))
                                                         (if (and (integerp stepx) (integerp maxdiv) (integerp stepy))
                                                             (om-return-from-modal-dialog dialog 
                                                                                        (list (list xsnap stepx maxdiv) 
                                                                                              (list ysnap stepy)))
                                                           (progn (om-beep-msg "Bad step values !!")
                                                                  (om-return-from-modal-dialog dialog nil)))))
                                          :default-button t))
    (om-modal-dialog dialog)))