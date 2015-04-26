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



(defclass control-bpc (control-bpf) ())



(defmethod initialize-instance :after  ((self control-bpc) &key)
  (om-add-subviews 
   self
   (om-make-dialog-item 'om-check-box (om-make-point 92 20) (om-make-point 100 16) "Closed BPC"
                        :di-action (om-dialog-item-act item
                                     (setf (close-line (panel (om-view-container (om-view-container item))))
                                           (om-checked-p item))
                                     (om-invalidate-view (panel (om-view-container (om-view-container item))) t))
                        :font *om-default-font1*
                        :checked-p nil
                        )))

;------------------------------------
;bpc EDITOR Class definition and initialization
;------------------------------------

(defclass bpcEditor (bpfEditor) ())

(defmethod get-panel-class ((self bpcEditor)) 'bpcPanel)
(defmethod get-control-class ((self bpcEditor)) 'control-bpc)


;------------------------------------
;bpc EDITOR VIEW Definition and initialization
;------------------------------------
(defclass bpcPanel (bpfPanel)  
   ((close-line :initform nil :accessor close-line)))

(defmethod draw-bpf ((self bpcPanel) (bpc bpc) minx maxx miny maxy &optional (deltax 0) (deltay 0) (dr-points nil))
   (declare (ignore minx maxx miny maxy))
   (draw-bpc-points self bpc (point-list bpc) deltax deltay nil dr-points))

;------------------------------------
;ACTIONS
;------------------------------------


(defmethod make-scroll-point ((Self bpcPanel) Where)
  (let* ((old-Mouse *bpf-last-click*)
         (first-Mouse *bpf-first-click*)
         (Initx (om-point-h *bpf-offset-click*))
         (Inity (om-point-v *bpf-offset-click*))
         (Offx (pixel2norme self 'x (- (om-point-h where) (om-point-h first-mouse))))
         (Offy (pixel2norme self 'y (- (om-point-v first-mouse) (om-point-v where))))
         (moveds (move-selection-bpf self  (- offx initx) (- offy inity))))
    (if moveds (setf (selection? self) moveds))
    (om-invalidate-view self t)
    (show-position (om-view-container self))
    (setf *bpf-offset-click* (om-make-point offx  offy))
    (setq *bpf-last-click* where)))


(defmethod add-new-bpf ((self bpcPanel))
  (let ((newbpc (make-instance 'bpc :point-list nil 
                               :decimals (value (precisionitem (control (om-view-container self)))))))
     (when (currentbpf self)
       (setf (selected-p (currentbpf self)) nil))
     (if (multibpf? (om-view-container self))
       (setf (bpf-list (get-bpf-obj self))
             (concatenate 'list (bpf-list (get-bpf-obj self)) (list newbpc)))
       (setf (object (om-view-container self)) newbpc))
     (record-new-bpf (om-view-container self))
     (setf (currentbpf self) newbpc)
     (om-invalidate-view self t)))

(defmethod add-point-to-bpf ((self bpcPanel) where)
   (let ((length (length (point-list (currentbpf self))))
         (new-point (pixel2point self where))
         (position-seg (segment-in-bpf self (currentbpf self) where)))
     (if position-seg
         (progn
           (cons-bpf (currentbpf self) (insert-in-list (point-list (currentbpf self)) new-point position-seg ))
           (setf (selection? self) (list (list new-point position-seg))))
       (progn
         (insert-point (currentbpf self) new-point)
         (setf (selection? self) (list (list new-point length)))
         (update-panel self t)))
     (scroll-point self new-point)))


(defmethod select-system ((self bpcPanel) where)
  (if (om-command-key-p)
      ;;; ???
      (call-next-method)
    (let* ((position-obj (point-in-bpf self (currentbpf self) where))  ;;; position-obj = ((point pos) (point pos) ...)
           (pos-in-win (view-position-win self)))
      (cond
       ((null position-obj)
        (setf (selected-p (currentbpf self)) nil)
        (setf (selection? self) nil)
        (om-init-motion-functions self 'make-select-system 'release-select-system)
        (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-selection-rectangle))
       ((listp position-obj)
        (let ((selection-list (get-real-selection-list self)))
          (cond 
           ((om-shift-key-p)
            (if (find (car (first position-obj)) selection-list :test 'om-points-equal-p :key 'first)
                (setf (slot-value self 'selection?) (remove (cadr (first position-obj)) selection-list :test '= :key 'second))
              (setf (slot-value self 'selection?) (cons  (first position-obj) selection-list))))
            
           (t (unless (find (car (first position-obj)) selection-list :test 'om-points-equal-p :key 'first)
                (setf (slot-value self 'selection?) position-obj))))
          (setf selection-list (get-real-selection-list self))
          (when (find (car (first position-obj)) selection-list :test 'om-points-equal-p :key 'first)
            (scroll-point self (first position-obj)))
          (om-invalidate-view self)
          ))
       (t
        (setf (selected-p (currentbpf self)) t)
        (om-invalidate-view self t))))))

(defmethod do-select-items-in-rect ((self Bpcpanel) rect) 
  (let (user-rect)
    (setf user-rect (om-make-rect (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
    (let* ((Top-Left (pixel2point self (om-rect-topleft user-rect)))
           (Bottom-Rig (pixel2point self (om-rect-bottomright user-rect) ))
           (points (give-points-in-rect (currentbpf self) (om-make-point (om-point-h top-left) (om-point-v bottom-rig))
                                               (om-make-point (om-point-h bottom-rig) (om-point-v top-left)))))
      (setf (selection? self) points)
      (om-invalidate-view self t) )))



(defmethod draw-bpc-points ((self bpcPanel) (bpf bpc) points deltax deltay &optional index dr-points)
   (let* ((system-etat (get-system-etat self))
          (bpf-selected? (and (equal (selection? self) t) (equal bpf (currentbpf self))))
          (real-points (if index (point-list bpf) points))
          (first-point (car points))
          (last-point (car (last points)))
          (close-mode (close-line self)))
     (if index
       (when first-point
         (let ((prev-pixel (unless (zerop (second first-point)) 
                             (point2pixel self (nth (- (second first-point) 1) real-points)  system-etat)))
               (first (point2pixel self (car first-point) system-etat)))
           (when prev-pixel
             (om-draw-line (+ deltax (om-point-h prev-pixel)) (+ deltay (om-point-v prev-pixel))
                        (+ deltax  (om-point-h first)) (+ deltay (om-point-v first))))))
       (let (first last)
         (when first-point
           (setf first (point2pixel self first-point system-etat))
           (setf last (point2pixel self last-point system-etat))
           (om-with-fg-color self  *om-blue-color*
                             (om-draw-char (+ deltax (- (om-point-h first) 2)) (+ deltay (om-point-v first) 4) #\o))
           (if (and close-mode (lines-p self))
             (om-draw-line (+ deltax (om-point-h first)) (+ deltay (om-point-v first)) 
                        (+ deltax (om-point-h last)) (+ deltay (om-point-v last)))
             (om-with-fg-color self *om-red-color*
                                             (om-draw-char (+ deltax (- (om-point-h last) 2)) (+ deltay (om-point-v last) 4) #\o))
             ))))
     (loop for thepoint in points
           for i = 0 then (+ i 1) do
           (let* ((pix-point (if index (point2pixel self (car thepoint) system-etat) (point2pixel self thepoint system-etat)))
                  (curindex (if index (second thepoint) i))
                  (next-point (nth (+ curindex 1) real-points)))
             (when (lines-p self)
               (when next-point
                 (let ((next-pixel (point2pixel self next-point system-etat)))
                   (om-draw-line (+ deltax (om-point-h pix-point)) (+ deltay (om-point-v pix-point))
                              (+ deltax (om-point-h next-pixel)) (+ deltay (om-point-v next-pixel))))))
             (when (or (null (lines-p self)) (selected-p bpf))
               (om-draw-rect (+ deltax (- (om-point-h pix-point) 2)) (+ deltay (- (om-point-v pix-point) 2)) 4 4))
             (when (or bpf-selected? (and (listp (selection? self)) (member thepoint (selection? self) :test 'om-points-equal-p)))
               (om-with-fg-color self *om-black-color*
                 (om-fill-rect (+ deltax (- (om-point-h pix-point) 3)) (+ deltay (- (om-point-v pix-point) 3)) 6 6)))
             (more-bpc-draw self thepoint (om-add-points pix-point (om-make-point deltax deltay)) i)
             ))))


(defmethod more-bpc-draw ((self bpcpanel) point pos index)
  (when (show-point-indices self) 
    (om-draw-string (+ (om-point-h pos) -4) (+ (om-point-v pos) -6) (format nil "~d" index))))

;-------------------true if point in bpc------------------------------
 
(defmethod point-in-bpf ((editor bpcPanel) (self bpc) where)
   (let* ((points (denormalize-points editor (point-list self)))
          primo  region  rep)
     (when points
       (om-with-focused-view editor
         (loop for item in points
               for i = 0 then (+ i 1)
               while (not rep) do
               (om-open-region editor)
               (om-open-region-add-rect (- (om-point-h item) 2) (- (om-point-v item) 2) (+ (om-point-h item) 2) (+ (om-point-v item) 2))
               (setf region (om-close-region editor))
               (when (om-point-in-region-p region where)
                 (setf rep (list (list (nth i (point-list self)) i)))))
         (unless rep
           (setf primo (pop points))
           (loop while points
                  while (not rep) do
                     (om-open-region editor)
                     (let ((seco (car points)))
                       (om-open-region-add-line primo seco 2)
                       (setf primo (pop points)))
                     (setf region (om-close-region editor))
                     (setf rep (om-point-in-region-p region where))))
         (om-dispose-region region)
         rep))))

(defmethod segment-in-bpf ((editor bpcPanel) (self bpc) where)
   (let* ((points (denormalize-points editor (point-list self)))
          primo  region  rep)
     (when points
       (om-with-focused-view editor
         (setf primo (pop points))
         (loop while points
               while (not rep)
               for i = 1 then (+ i 1) do
               (om-open-region editor)
               (let ((seco (car points)))
                 (om-open-region-add-line primo seco 2)
                 (setf primo (pop points)))
               (setf region (om-close-region editor))
               (when (om-point-in-region-p region where)
                 (setf rep i)))
         (when region 
           (om-dispose-region region))
         rep))))




;--------to change

(defmethod selection? ((self bpcPanel))
  (if (listp (slot-value self 'selection?))
    (mapcar #'car (slot-value self 'selection?))
    (slot-value self 'selection?)))

;(defmethod move-selection-bpf ((self bpcPanel) x y)
;  (move-points-in-bpf (currentbpf self) (selection? self)  x y))

(defmethod move-selection-bpf ((self bpcPanel) x y)
  (move-points-in-bpf (currentbpf self) (get-real-selection-list self) x y)) ;  (slot-value self 'selection?) x y))


(defmethod handle-key-event ((self bpcPanel) char)
   (let ((myobj (get-bpf-obj self)))
     (case char
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
       (:om-key-tab (when (multibpf? (om-view-container self))
                (setf (currentbpf self) (nth (mod (+ (position (currentbpf self) (bpf-list myobj)) 1)
                                                  (length (bpf-list myobj))) (bpf-list myobj)))
                (om-invalidate-view (om-view-container self) t)))
       (:om-key-delete (cond
                  ((null (selection? self))
                   (om-beep))
                  ((listp (selection? self))
                   (loop for point in (selection? self) do
                         (remove-point (currentbpf self) point))
                   (update-panel self t)
                   )
                  ((selection? self)
                   (delete-current-bpf self)
                   (update-panel self t))
                  (t (om-beep)))
        (setf (selection? self) nil))
       (:om-key-up (cond
                   ((null (selection? self))
                    (om-beep))
                   ((listp (selection? self))
                    (let ((moveds (move-selection-bpf self 0 (zoom (rulery self)))))
                      (if moveds
                        (progn
                          (setf (selection? self) moveds)
                          (update-panel self t))
                        (om-beep-msg "I can not move these points"))))
                   ((selection? self)
                    (move-bpf-in-x-y (currentbpf self) 0 (zoom (rulery self)))
                    (update-panel self t))
                   (t (om-beep))))
       (:om-key-down (cond
                     ((null (selection? self))
                      (om-beep))
                     ((listp (selection? self))
                      (let ((moveds (move-selection-bpf self 0 (* -1 (zoom (rulery self))))))
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
                         (let ((moveds (move-selection-bpf self (zoom (rulerx self)) 0)))
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
                      (let ((moveds (move-selection-bpf self (* -1 (zoom (rulerx self))) 0)))
                        (if moveds
                          (progn
                            (setf (selection? self) moveds)
                            (update-panel self t))
                          (om-beep-msg "I can not move these points"))))
                     ((selection? self)
                      (move-bpf-in-x-y (currentbpf self) (* -1 (zoom (rulerx self)))0 )
                      (update-panel self t))
                     (t (om-beep)))))))
  


(defmethod special-move-point ((self bpcPanel) point i)
  (let* ((dec (decimals (get-bpf-obj self)))
         (xsize (max 50 (* 10 dec)))
         (mydialog (om-make-window 'om-dialog
                                   :size (om-make-point (max 180 (+ 100 (* 12 dec))) 90)
                                   :window-title ""
                                   :position (om-add-points (om-view-position (window self)) (om-mouse-position self))))
         (xed (om-make-dialog-item 'om-editable-text (om-make-point 30 5) (om-make-point xsize 10)
                                   (if (zerop dec) (format () "~D" (om-point-h point))
                                       (format () (format nil "~S,~DF" '~ dec) 
                                               (/ (om-point-h point) (expt 10.0 dec))))
                                   ))
         (yed (om-make-dialog-item 'om-editable-text (om-make-point 30 35) (om-make-point xsize 10)
                                   (if (zerop dec) (format () "~D" (om-point-v point))
                                       (format () (format nil "~S,~DF" '~ dec) 
                                               (/ (om-point-v point) (expt 10.0 dec))))
                                   )))
    (om-add-subviews mydialog 
                     (om-make-dialog-item 'om-static-text (om-make-point 5 9) (om-make-point 20 20) (x-label self) 
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     (om-make-dialog-item 'om-static-text (om-make-point 5 39)  (om-make-point 20 20) (y-label self) 
                                          :font *om-default-font3b*
                                          :bg-color *om-window-def-color*)
                     xed yed
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 5) (om-make-point 70 20) "Cancel"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog mydialog ()))
                                          :default-button nil)
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 36) (om-make-point 70 20) "OK"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (if 
                                                         (move-points-in-bpf 
                                                          (currentbpf self) 
                                                          (list (list point i)) 
                                                          (- (round (* (read-from-string (om-dialog-item-text xed)) (expt 10.0 dec))) (om-point-h point))
                                                          (- (round (* (read-from-string (om-dialog-item-text yed)) (expt 10.0 dec))) (om-point-v point)))
                                                         (do-after-move self)
                                                         (om-beep-msg "Ilegal move"))
                                                       (om-return-from-modal-dialog mydialog ()))
                                          :default-button t))
    (om-modal-dialog mydialog)))