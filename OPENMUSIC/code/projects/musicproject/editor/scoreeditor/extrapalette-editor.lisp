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
;;; authors G. Assayag, C. Agon, J. Bresson, K.Haddad
;=========================================================================

(in-package :om)

;;;============================
;;; EXTRA PALETTE :
(defvar *extramanager* nil)

;;c'est mieux om-window pour avoir la main apres

(defclass extra-pal-win #+macosx(om-window) #-macosx(om-dialog) 
  ((extramanager :initform nil :initarg :extramanager :accessor extramanager)
   (buttons :initform nil :initarg :buttons :accessor buttons)
   (extraitems :initform nil :initarg :extraitems :accessor extraitems)
   (preview :initform nil :initarg :preview  :accessor preview)))
  
;;; edit-mode = :text :pict :dyn :graphics :lines :fig
(defclass extramanager ()
  ((win :initform nil  :accessor win)
   (winpos :initform nil  :accessor winpos)
   (show :initform nil  :accessor show)
   (edit-mode :initform nil  :accessor edit-mode)
   (params :initform nil  :accessor params)
   (current-editor :initform nil :accessor current-editor :initarg :current-editor)))


(defmethod show-extra-palette-tools ((self t)) nil)
(defmethod show-extra-palette-tools ((self scorepanel))
  (let ((win (om-make-window 'extra-pal-win :window-title "Extra Edition Palette"
                             :size (om-make-point 182 26) 
                             :position (om-view-position (om-view-container self)) ;(om-make-point 0 0)
                             :resizable nil :maximize nil :minimize nil
                             :window-show nil
                             :destroy-callback #'(lambda (interface) (om-window-close-event interface))
                             )))
    (setf *extramanager* (make-instance 'extramanager))
    (om-add-subviews win 
                     (om-make-view 'om-icon-button
                                   ;:lock-push t
                                   :position (om-make-point 0 0)
                                  ; :selected-p (and (get-edit-param self 'obj-mode) (= (get-edit-param self 'obj-mode) n))
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                               (extra-pal-action item nil))
                                   :icon1 "chord" :icon2 "chord-pushed"
                                   :owner win)
                     ;;;vel
                     (om-make-view 'om-icon-button
                                   ;:lock-push t
                                   :position (om-make-point 26 0)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item) (extra-pal-action item :dyn))
                                   :icon1 "vel" :icon2 "vel-pushed";uncommented
                                   :owner win)
                   
                     ;;; figure
                     (om-make-view 'om-icon-button
                                   ;:lock-push t
                                   :position (om-make-point 52 0)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                               (extra-pal-action item :figure))
                                   :icon1 "fig" :icon2 "fig-pushed" ;todo
                                   :owner win)
                     ;;; lines
                     (om-make-view 'om-icon-button
                                   ;:lock-push t
                                   :position (om-make-point 78 0)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                               (extra-pal-action item :lines))
                                   :icon1 "slur" :icon2 "slur-pushed" ;todo
                                   :owner win)
                     ;;; pict
                     (om-make-view 'om-icon-button
                                   ;:lock-push t
                                   :position (om-make-point  104 0)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                               (extra-pal-action item :pict))
                                   :icon1 "picture" :icon2 "picture-pushed"
                                   :owner win)
                     ;;; text
                     (om-make-view 'om-icon-button
                                   ;:lock-push t
                                   :position (om-make-point 130 0)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                               (extra-pal-action item :text))
                                   :icon1 "text" :icon2 "text-pushed" 
                                   :owner win)
                     ;;; graphics
                     (om-make-view 'om-icon-button
                                   ;:lock-push t
                                   :position (om-make-point 156 0)
                                   :size (om-make-point 26 25)
                                   :action #'(lambda (item)
                                               (extra-pal-action item :graphics))
                                   :icon1 "shapes" :icon2 "shapes-pushed" ;todo
                                   :owner win)
                     )
    (setf (win *extramanager*) win)
    (setf (edit-mode *extramanager*) nil)
    (setf (current-editor *extramanager*) self)
    ;;necessary to close palette when editor is closed
    (push win (attached-editors (om-view-container (current-editor *extramanager*))))
    #+macosx(om-show-window win)
    #-macosx(om-select-window win)
    ))
    


(defmethod om-window-close-event :after ((self extra-pal-win)) 
  (let ((panel (current-editor *extramanager*)))
  #+macosx(setf (winpos *extramanager*) (om-view-position self))
  (setf (current-editor *extramanager*) nil)
  (setf (win *extramanager*) nil)
  (setf *extramanager* nil)
  (om-invalidate-view panel t)
  ))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extra-pal-action (button value) 
  (let* ((win (om-view-container button))
         (pos (om-view-position (om-get-interface (om-view-container (current-editor *extramanager*)))));pos of editor interface (on screen!)
         (x (om-point-x pos))
         (y (om-point-y pos))
         )
    (unless (and (equal (edit-mode *extramanager*) value)
                 (extraitems win))
      (loop for b in (buttons win) do
              (when (selected-p b)
                (setf (selected-p b) nil)
                (om-invalidate-view b t)))
      (when (extraitems win) (mapc #'(lambda (sv) (om-remove-subviews win sv)) (extraitems win)))
      (when (preview win) (om-remove-subviews win (preview win)))
      (setf (selected-p button) t)
      (om-invalidate-view button t)
      (setf (edit-mode *extramanager*) value)
      (when (and value (not (get-extra-param *extramanager* value)))
        (set-extra-param *extramanager* value (extraedit-def-params value)))
      (let ((extraedit (get-extra-items value)))
        (om-set-view-size win (om-make-point (om-point-h (om-interior-size win)) (car extraedit)))
        (setf (extraitems win) (cdr extraedit))
        (when (extraitems win) (mapc #'(lambda (sv) (om-add-subviews win sv)) (extraitems win)))
        (when (preview-p value) (add-extra-preview win))
        )
      (om-set-view-position win (om-make-point (- x 182) y))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass extra-preview (om-view) ())

;des probleme de taille de la fenetre elle-meme
;Faire le preview aussi des dynamiques OU
;; ne garder le preview que pour le text

(defmethod om-draw-contents ((self extra-preview))
  (let* ((value (edit-mode *extramanager*))
         (params (get-extra-param *extramanager* value)))
    ;(print (list "params" params))
    (case value
     (:figure
       ;;; char size color
       (om-with-focused-view self
         (om-with-font (om-make-music-font *extras-font* 24);(nth 1 params))
                       (om-with-fg-color self (nth 2 params) 
                         (om-draw-string (round (w self) 2) 30 (string (nth 0 params))))))
      )
     #|
      (:lines 
       ;;; fig dash linesize color
       (om-with-focused-view self
       (om-with-line-size (nth 2 params)
         (om-with-fg-color self (nth 3 params) 
             (if (equal (nth 1 params) 'dash)
                 (om-with-dashline 
                   (case (nth 0 params) 
                     ('slur (om-draw-ellipse-arc  30 20 (- (w self) 60) 28 0 pi))
                     ('decresc (om-draw-line 20 20 (- (w self) 20) 30)
                               (om-draw-line 20 40 (- (w self) 20) 30))
                     ('cresc (om-draw-line 20 30 (- (w self) 20) 20)
                             (om-draw-line 20 30 (- (w self) 20) 40))
                     ('brack (om-draw-line 20 20 (- (w self) 20) 20)
                             (om-draw-line 20 20 20 30)
                             (om-draw-line (- (w self) 20) 20 (- (w self) 20) 30))
                     ))
               (case (nth 0 params) 
                     ('slur (om-draw-ellipse-arc  30 20 (- (w self) 60) 28 0 pi))
                     ('decresc (om-draw-line 20 20 (- (w self) 20) 30)
                               (om-draw-line 20 40 (- (w self) 20) 30))
                     ('cresc (om-draw-line 20 30 (- (w self) 20) 20)
                             (om-draw-line 20 30 (- (w self) 20) 40))
                     ('brack (om-draw-line 20 20 (- (w self) 20) 20)
                             (om-draw-line 20 20 20 30)
                             (om-draw-line (- (w self) 20) 20 (- (w self) 20) 30))
                     )
               )))))
      |#
      (:pict
       ;;; pict
       nil)
      (:text
       ;;; font color
       (om-with-focused-view self
         (om-with-font (nth 0 params) 
           (om-with-fg-color self (nth 1 params) 
             (om-draw-string 30 30 "Text")
             ))))
     (:graphics
       ;;; fig dash linesize color fill
       (om-with-focused-view self
       (om-with-line-size (nth 2 params)
         (om-with-fg-color self (nth 3 params) 
             (if (equal (nth 1 params) 'dash)
                 (om-with-dashline 
                   (case (nth 0 params) 
                     ('circ (if (nth 4 params) 
                                  (om-fill-ellipse (round (w self) 2) (round (h self) 2)
                                                   (round (w self) 4) (round (h self) 4))
                                (om-draw-ellipse (round (w self) 2) (round (h self) 2)
                                                 (round (w self) 4) (round (h self) 4))))
                     ('rect (if (nth 4 params) 
                                (om-fill-rect 30 15 (- (w self) 60) (- (h self) 30))
                                (om-draw-rect 30 15 (- (w self) 60) (- (h self) 30))))
                     ('line (om-draw-line 20 20 (- (w self) 20) (- (h self) 20)))
                     ('polyg (if (nth 4 params) 
                                (oa::om-fill-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60)))
                                (oa::om-draw-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60))))
                             )
                     ))
               (case (nth 0 params) 
                 ('circ (if (nth 4 params) 
                              (om-fill-ellipse (round (w self) 2) (round (h self) 2)
                                               (round (w self) 4) (round (h self) 4))
                            (om-draw-ellipse (round (w self) 2) (round (h self) 2)
                                             (round (w self) 4) (round (h self) 4))
                            ))
                 ('rect (if (nth 4 params) 
                            (om-fill-rect 30 15 (- (w self) 60) (- (h self) 30))
                          (om-draw-rect 30 15 (- (w self) 60) (- (h self) 30))))
                 ;('line (om-draw-line 20 20 (- (w self) 20) (- (h self) 20)))
                 ('polyg (if (nth 4 params) 
                                (oa::om-fill-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60)))
                                (oa::om-draw-polygon (list (om-make-point 60 0) (om-make-point 90 60) (om-make-point 30 60))))
                             )
                 )
               )))))
     (otherwise nil))))

;commented out some falied previews!
(defun preview-p (value)
  (or ;(equal value :graphics)
      ;(equal value :lines)
      ;(equal value :figure)
      (equal value :text)))


(defun add-extra-preview (win) 
  (setf (preview win) (om-make-view 'extra-preview :position (om-make-point 20 (h win))
                                    :size (om-make-point (- (w win) 40) 60)))
  (om-set-view-size win (om-make-point (om-point-h (om-interior-size win)) (+ (h win) 70)))
  (om-add-subviews win (preview win)))

(defun extra-action (button value)
  (let ((win (om-view-container button)))
      (unless (and (equal (edit-mode *extramanager*) value)
                   (extraitems win))
        (loop for b in (buttons win) do
              (when (selected-p b)
                (setf (selected-p b) nil)
                (om-invalidate-view b t)))
        (when (extraitems win) (mapc #'(lambda (sv) (om-remove-subviews win sv)) (extraitems win)))
        (when (preview win) (om-remove-subviews win (preview win)))
        (setf (selected-p button) t)
        (om-invalidate-view button t)
        (setf (edit-mode *extramanager*) value)
        (when (and value (not (get-extra-param *extramanager* value)))
          (set-extra-param *extramanager* value (extraedit-def-params value)))
        (let ((extraedit (get-extra-items value)))
          (om-set-view-size win (om-make-point (om-point-h (om-interior-size win)) (car extraedit)))
          (setf (extraitems win) (cdr extraedit))
          (when (extraitems win) (mapc #'(lambda (sv) (om-add-subviews win sv)) (extraitems win)))
          (when (preview-p value) (add-extra-preview win))
          ))))



(defun get-extra-param (em id)
  (when (find id (params em) :key 'car)
    (cadr (find id (params em) :key 'car))))

(defun set-extra-param (em id val)
  (let ((pos (position id (params em) :key 'car)))
    (if pos
        (setf (cadr (nth pos (params em))) val)
      (push (list id val) (params em)))))


(defun extraedit-def-params (value)
  (case value
    (:dyn
     ;;; size char color
     (list nil 12 *om-black-color*))
    (:figure
     ;;; size char color
     (list (code-char 113) 12 *om-black-color*))
    (:lines 
     ;;; fig dash linesize color
     (list 'slur 'plain 1 *om-black-color*))
    (:pict
     ;;; pict
     (list nil))
    (:text
     ;;; font color
     (list *om-default-font1* *om-black-color*))
    (:graphics
      ;;; fig dash linesize color fill
      (list 'rect 'plain 1 *om-black-color* nil))))



(defmethod get-extra-items ((value t)) 
  (list 50))


;note heads
(defmethod get-extra-items ((value (eql nil)))
  (let ((params (cadr (find value (params *extramanager*) :key 'car)))
        (heads-list (append (list (string (code-char 173)) 
                                  (string (code-char 110))
                                  (string (code-char 82)) 
                                  (string (code-char 83)) 
                                  (string (code-char 81)) 
                                  (string (code-char 80)))
                                              (loop for i from 94 to 105 collect (string (code-char i))))))
    (list 180
          (om-make-dialog-item 'om-static-text (om-make-point 45 30) ;(om-make-point 20 30)
                               (om-make-point 160 20) "Score Selection"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
                    
          (om-make-dialog-item 'om-static-text (om-make-point 10 60) ;(om-make-point 10 90)
                               (om-make-point 100 20) "Heads"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 75 62) ;(om-make-point 70 80)
                               (om-make-point 70 5) ""
                               :range heads-list
                               :value (string (code-char 173))
                               :font (om-make-music-font *heads-font* 24)
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                              (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                    (when (or (container-p obj) (simple-container-p obj))
                                                      (if (= (om-get-selected-item-index item) 0)
                                                        (delete-extras (get-extras obj "head"))
                                                        (add-head-extra obj (nth (om-get-selected-item-index item) heads-list)))))
                                              (update-panel (panel (current-editor *extramanager*))))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 120) ;(om-make-point 10 60)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          (om-make-view 'om-color-view :position (om-make-point 75 120)
                               :size (om-make-point 65 16) 
                               :color *om-black-color*
                               :after-fun #'(lambda (item)
                                              (let ((sel (selection? (panel (current-editor *extramanager*)))))
                                                (if sel
                                                    (let ((c (color item)))
                                                      (when c 
                                                        (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                             (when (or (container-p obj) (simple-container-p obj))
                                                               (set-mus-color obj c)))
                                                        (update-panel (panel (current-editor *extramanager*)))))
                                                  (om-beep)))))
          )))


(defmethod get-extra-items ((value (eql :pict))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 100 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Picture Extra"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 40 55)
                               (om-make-point 100 20) "(not available)"
                               :fg-color *om-gray-color*
                               :font *om-default-font1*))
    ))

(defmethod get-extra-items ((value (eql :graphics))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 210 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Graphic Extra"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 60)
                               (om-make-point 100 20) "Shape"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 60 55)
                               (om-make-point 85 20) ""
                               :range '("Rectangle" "Circle" "Line" "Polygon")
                               :value (nth (position (nth 0 params) '(rect circ line polyg)) '("Rectangle" "Circle" "Line" "Polygon"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 0 params) (nth (om-get-selected-item-index item) '(rect circ line polyg)))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          (om-make-dialog-item 'om-static-text (om-make-point 10 90)
                               (om-make-point 100 20) "Line"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 60 85)
                               (om-make-point 85 20) ""
                               :range '("Normal" "Dashed")
                               :value (nth (position (nth 1 params) '(plain dash)) '("Normal" "Dashed"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 1 params) (nth (om-get-selected-item-index item) '(plain dash)))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          (om-make-dialog-item 'om-static-text (om-make-point 10 120)
                               (om-make-point 100 20) "Pen Size"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 115)
                               (om-make-point 75 20) ""
                               :range '("1" "2" "3" "4" "5" "6" "7" "8")
                               :value (nth (position (nth 2 params) '(1 2 3 4 5 6 7 8)) '("1" "2" "3" "4" "5" "6" "7" "8"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 2 params) (nth (om-get-selected-item-index item) '(1 2 3 4 5 6 7 8)))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          (om-make-dialog-item 'om-static-text (om-make-point 10 150)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          (om-make-view 'om-color-view :position (om-make-point 75 150)
                               :size (om-make-point 60 16) 
                               :color (nth 3 params)
                               :after-fun #'(lambda (item)
                                            (let ((c (color item)))
                                              (when c 
                                                (setf (nth 3 params) c)
                                                (set-extra-param *extramanager* value params)
                                                ;(om-invalidate-view (preview (win *extramanager*)))
                                                ))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 180)
                               (om-make-point 100 20) "Fill"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-check-box (om-make-point 70 175)
                               (om-make-point 100 20) ""
                               :checked-p (nth 4 params)
                               :di-action (om-dialog-item-act item
                                            (setf (nth 4 params) (om-checked-p item))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          )))

(defmethod get-extra-items ((value (eql :lines))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 190 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Connections"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 60)
                               (om-make-point 100 20) "Shape"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 60 55)
                               (om-make-point 80 20) ""
                               :range '("Slur" "Crescendo" "Decrescendo" "Trill" "Sost. Ped.") ; "Bracket"
                               :value (nth (position (nth 0 params) '(slur cresc decresc trill sost-ped)) 
                                           '("Slur" "Crescendo" "Decrescendo" "Trill" "Sost. Ped."))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 0 params) (nth (om-get-selected-item-index item) '(slur cresc decresc trill sost-ped)))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          (om-make-dialog-item 'om-static-text (om-make-point 10 90)
                               (om-make-point 100 20) "Line"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 60 85)
                               (om-make-point 80 20) ""
                               :range '("Normal" "Dashed")
                               :value (nth (position (nth 1 params) '(plain dash)) '("Normal" "Dashed"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 1 params) (nth (om-get-selected-item-index item) '(plain dash)))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          (om-make-dialog-item 'om-static-text (om-make-point 10 120)
                               (om-make-point 100 20) "Pen Size"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 70 115)
                               (om-make-point 70 20) ""
                               :range '("1" "2" "3" "4" "5" "6" "7" "8")
                               :value (nth (position (nth 2 params) '(1 2 3 4 5 6 7 8)) '("1" "2" "3" "4" "5" "6" "7" "8"))
                               :di-action (om-dialog-item-act item
                                            (setf (nth 2 params) (nth (om-get-selected-item-index item) '(1 2 3 4 5 6 7 8)))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          (om-make-dialog-item 'om-static-text (om-make-point 10 150)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          (om-make-view 'om-color-view :position (om-make-point 75 152)
                               :size (om-make-point 60 16) 
                               :color (nth 3 params)
                               :after-fun #'(lambda (item)
                                            (let ((c (color item)))
                                              (when c 
                                                (setf (nth 3 params) c)
                                                (set-extra-param *extramanager* value params)
                                               ; (om-invalidate-view (preview (win *extramanager*)))
                                                ))))
          
         )))



(defun make-text-extra (self &optional (deltay 3))
  "Get text from panel, and creates a text-extra instance"
  (let ((text (om-dialog-item-text self)))
    (make-instance 'text-extra
                   :deltay deltay
                   :thetext text)))

(defmethod get-extra-items ((value (eql :text))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 230 
          (om-make-dialog-item 'om-static-text (om-make-point 40 30)
                               (om-make-point 100 20) "Text Extra"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-static-text (om-make-point 10 60)
                               (om-make-point 100 20) "Font"
                               :font *om-default-font1*)
          (om-make-dialog-item 'om-button (om-make-point 60 58)
                               (om-make-point 80 20) "Choose"
                               :di-action (om-dialog-item-act item
                                            (let ((f (om-choose-font-dialog :font (nth 0 params))))
                                              (when f 
                                                (setf (nth 0 params) f)
                                                (set-extra-param *extramanager* value params)
                                                (om-invalidate-view (preview (win *extramanager*)))))))
          (om-make-dialog-item 'om-static-text (om-make-point 10 90)
                               (om-make-point 100 20) "Color"
                               :font *om-default-font1*)
          
          (om-make-view 'om-color-view :position (om-make-point 65 92)
                        :size (om-make-point 70 16) 
                        :color (nth 1 params)
                        :after-fun #'(lambda (item)
                                       (let ((c (color item)))
                                         (when c 
                                           (setf (nth 1 params) c)
                                           (set-extra-param *extramanager* value params)
                                           (om-invalidate-view (preview (win *extramanager*)))))))
          
          (setf textinput (om-make-dialog-item 'om-text-edit-view
                                               (om-make-point 15 125)
                                               (om-make-point 120 60)
                                               "Text"
                                               :font *om-default-font1*
                                               )
                )
          (om-make-dialog-item 'om-button (om-make-point 40 200)
                               (om-make-point 80 20) "Set"
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                              (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                    (when (or (container-p obj) (simple-container-p obj))
                                                      (add-extra obj (make-text-extra textinput) nil nil)))
                                              (update-panel (panel (current-editor *extramanager*)))
                                            
                                            ))))
    ))


(defmethod get-extra-items ((value (eql :figure))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car))))
    (list 120 
          (om-make-dialog-item 'om-static-text (om-make-point 20 30)
                               (om-make-point 160 20) "Articulation Symbols"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 50 55) ;(om-make-point 20 60)
                               (om-make-point 70 20) ""
                               :range (append (list (string (code-char 173))) (loop for i from 113 to 125 collect (string (code-char i))))
                               :value (string (code-char 173)) ;(string (nth 0 params))
                               :font (om-make-music-font *extras-font* 24)
                               :di-action (om-dialog-item-act item
                                            (setf (nth 0 params) (elt (om-get-selected-item item) 0))
                                            (set-extra-param *extramanager* value params)
                                            ;(om-invalidate-view (preview (win *extramanager*)))
                                            ))
          )))



(defmethod get-extra-items ((value (eql :dyn))) 
  (let ((params (cadr (find value (params *extramanager*) :key 'car)))
        (vels (mapcar #'(lambda (d) (string (cadr d))) *dynamics-symbols-list*))
        )
    (list 250 

          (om-make-dialog-item 'om-static-text (om-make-point 45 30)
                               (om-make-point 160 20) "Extra Dynamics"
                               :fg-color *om-dark-gray-color*
                               :font *om-default-font1b*)
          
          (om-make-dialog-item 'om-pop-up-dialog-item (om-make-point 50 55)
                               (om-make-point 70 20) ""
                               :range vels
                               :value (string (code-char 173)) ;(string (nth 0 params))
                               :font (om-make-music-font *extras-font* 24)
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                              (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                      (when (or (container-p obj) (simple-container-p obj))
                                                        (if (container-p obj) 
                                                           (loop for i in (inside obj) 
                                                                   do (setf (vel i) (third (nth (om-get-selected-item-index item) *dynamics-symbols-list*))))
                                                        (setf (vel obj) (third (nth (om-get-selected-item-index item) *dynamics-symbols-list*))))
                                                        (add-vel-extra obj)))
                                              (update-panel (panel (current-editor *extramanager*))))))
                                         
          (om-make-dialog-item 'om-button (om-make-point 50 118)
                               (om-make-point 65 20) "Show"
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                              (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                   (when (or (container-p obj) (simple-container-p obj))
                                                     (add-vel-extra obj)))
                                              (update-panel (panel (current-editor *extramanager*)))
                                            )))
          
          (om-make-dialog-item 'om-button (om-make-point 50 150)
                               (om-make-point 65 20) "Hide"
                               :di-action (om-dialog-item-act item
                                            (when (selection? (panel (current-editor *extramanager*)))
                                            (loop for obj in (selection? (panel (current-editor *extramanager*))) do
                                                    (when (or (container-p obj) (simple-container-p obj))
                                                      (delete-extras (get-extras obj "vel"))))
                                            (update-panel (panel (current-editor *extramanager*)))
                                            )))
          )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;EDIT DIALOGS;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;crescendo

(defmethod edit-extra-vals ((self scorePanel) (extra extra-objet))
  (cond 
   ((trill-p extra)
    (open-edit-trill-dialog self extra))
   ((d-dynamic-extra-p extra)
    (open-edit-d-dynamic-dialog self extra))
   (t (print "No yet!"))))


;cresc-decrec
;ajouter reset x-pos according to begin and end of chords 
(defmethod open-edit-d-dynamic-dialog ((self scorePanel) (extra extra-objet))
  (let* ((Mydilog (om-make-window 'om-dialog
                                 :size (om-make-point 260 220)
                                 :window-title "Crescendo/Decrescendo values"
                                 :resizable nil :maximize nil :minimize nil
                                 :font *om-default-font1*
                                 :window-show nil
                                 :position :centered))
         (diff (if (cursor-interval self)
                   (- (second (cursor-interval self))
                      (car (cursor-interval self))) 0))
          
        (start (om-make-dialog-item 'om-editable-text (om-make-point 20 36) (om-make-point 90 12)
                                    (format nil "~D" (start-val extra))
                                    :font *om-default-font1*
                                    :focus nil))
        (end (om-make-dialog-item 'om-editable-text (om-make-point 20 100) (om-make-point 90 12)
                                  (format nil "~D" (end-val extra))
                                  :value (format nil "~D" (end-val extra))
                                  :enabled t
                                  :font *om-default-font1*
                                  :focus nil)))
                 
      (om-add-subviews mydilog 
                       (om-make-dialog-item 'om-static-text (om-make-point 25 5) (om-make-point 80 18) "Start [1-127]:"           
                                            :font *om-default-font2*
                                            :bg-color *om-window-def-color*)
                       (om-make-dialog-item 'om-static-text (om-make-point 25 70) (om-make-point 80 18) "End [1-27]:"           
                                            :font *om-default-font2*
                                            :bg-color *om-window-def-color*)
                       (om-make-dialog-item 'om-button (om-make-point 140 36) (om-make-point 80 20) "Cancel"
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item)) (om-return-from-modal-dialog mydilog ())))
                       (om-make-dialog-item 'om-button (om-make-point 140 76) (om-make-point 80 20) "Apply"
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (when (not (equal "" (om-dialog-item-text start))) 
                                                           (let ((repstart (read-from-string (om-dialog-item-text start)))
                                                                 (repend (read-from-string (om-dialog-item-text end))))
                                                             (apply-dynamic-vel extra self repstart repend)
                                                             (om-invalidate-view self t)
                                                         (om-return-from-modal-dialog mydilog ())
                                                         )))
                                            ;:focus t
                                            :default-button t
                                            )
                       start end
                       )
      ;to display value of numbox
      (om-modal-dialog mydilog)))

;;TRILL
(defmethod open-edit-trill-dialog ((self scorePanel) (extra extra-objet))
  (let* ((Mydilog (om-make-window 'om-dialog
                                 :size (om-make-point 260 220)
                                 :window-title "Trill values"
                                 :resizable nil :maximize nil :minimize nil
                                 :font *om-default-font1*
                                 :window-show nil
                                 :position :centered))
         (diff (if (cursor-interval self)
                   (- (second (cursor-interval self))
                      (car (cursor-interval self))) 0))
          
        (start (om-make-dialog-item 'om-editable-text (om-make-point 20 36) (om-make-point 90 12)
                                    (format nil "~D" (start-val extra))
                                    :font *om-default-font1*
                                    :focus nil))
        (end (om-make-dialog-item 'om-editable-text (om-make-point 20 100) (om-make-point 90 12)
                                  (format nil "~D" (end-val extra))
                                  :value (format nil "~D" (end-val extra))
                                  :enabled t
                                  :font *om-default-font1*
                                  :focus nil)))
                 
      (om-add-subviews mydilog 
                       (om-make-dialog-item 'om-static-text (om-make-point 25 5) (om-make-point 80 18) "Start [1-127]:"           
                                            :font *om-default-font2*
                                            :bg-color *om-window-def-color*)
                       (om-make-dialog-item 'om-static-text (om-make-point 25 70) (om-make-point 80 18) "End [1-27]:"           
                                            :font *om-default-font2*
                                            :bg-color *om-window-def-color*)
                       (om-make-dialog-item 'om-button (om-make-point 140 36) (om-make-point 80 20) "Cancel"
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item)) (om-return-from-modal-dialog mydilog ())))
                       (om-make-dialog-item 'om-button (om-make-point 140 76) (om-make-point 80 20) "Apply"
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (when (not (equal "" (om-dialog-item-text start))) 
                                                           (let ((repstart (read-from-string (om-dialog-item-text start)))
                                                                 (repend (read-from-string (om-dialog-item-text end))))
                                                             (if (and (integerp repstart) (> repstart 0))
                                                                 (progn
                                                                   (setf (cursor-pos self) repstart)
                                                                 ;(set-edit-param (om-view-container self) 'grillestep  rep)
                                                                   (if  (and repend (> repend repstart))
                                                                       (setf (cursor-interval self) (list repstart repend))
                                                                     (om-beep-msg (string+ "Bad end value ! (" (om-dialog-item-text end) ") should be > than (om-dialog-item-text start)" )))
                                                                   (om-invalidate-view self t))
                                                               (om-beep-msg (string+ "Bad start value ! (" (om-dialog-item-text start) ")" )))))
                                                         (om-return-from-modal-dialog mydilog ()))
                                            ;:focus t
                                            :default-button t
                                            )
                       start end
                       )
      ;to display value of numbox
      (om-modal-dialog mydilog)))


