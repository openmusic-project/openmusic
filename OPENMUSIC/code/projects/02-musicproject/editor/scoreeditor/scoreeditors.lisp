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


;======================================================
;VIEW   abstract class
;======================================================
(defclass scoreEditor (EditorView object-editor play-editor-mixin) 
  ((ctr-view :initform nil :accessor ctr-view)
   (mode :initform nil :accessor mode)
   ))

;;; SPECIAL TITLEBAR
(defclass score-titlebar (editor-titlebar) 
  ((play-buttons :accessor play-buttons :initform nil)
  (edit-buttons :accessor edit-buttons :initform nil)
  (mode-buttons :accessor mode-buttons :initform nil)
  ))

(defmethod editor ((self editor-titlebar)) (om-view-container self))

(defmethod get-titlebar-class ((self scoreeditor)) 'score-titlebar)

(defmethod title-bar-infostring ((self scoreeditor))
  (cond ((analysis-mode? (panel self))
         (analysis-infostring self))
        (t (format nil "Selection: ~A" (string-upcase (string (obj-mode (panel self))))))))

(defmethod om-draw-contents :after ((self score-titlebar)) nil)
;  (let ((str (title-bar-infostring (om-view-container self))))
;  (om-with-focused-view self
;    (om-with-font *om-default-font1*
;                  (om-draw-string (- (w self) (om-string-size str *om-default-font1*) 8) 17 str)))))


(defmethod init-titlebar ((self scoreeditor))
  (call-next-method)
  
  (setf (edit-buttons (title-bar self))
        (list (om-make-view 'om-icon-button :position (om-make-point 180 2) :size (om-make-point 22 22)
                            :icon1 "mousecursor" :icon2 "mousecursor-pushed"
                            :lock-push t
                            :selected-p (equal :normal (get-edit-param self 'cursor-mode))
                            :action #'(lambda (item) (set-cursor-mode self :normal)))
              
              (om-make-view 'om-icon-button :position (om-make-point 201 2) :size (om-make-point 22 22)
                            :icon1 "beamcursor" :icon2 "beamcursor-pushed"
                            :lock-push t
                            :selected-p (equal :interval (get-edit-param self 'cursor-mode))
                            :action #'(lambda (item) (set-cursor-mode self :interval)))))

  (setf (mode-buttons (title-bar self))
        (loop for mode in (object-order self) 
              for x = 240 then (+ x 21) for 
              n = 0 then (+ n 1) 
              collect
              (om-make-view 'om-icon-button
                            :lock-push t
                            :position (om-make-point x 2)
                            :selected-p (= (get-edit-param self 'obj-mode) n)
                            :size (om-make-point 22 22)
                            :action (let ((m n))
                                      #'(lambda (item)
                                          (set-obj-mode self m)
                                          (update-mode-buttons (title-bar self))))
                            :icon1 mode :icon2 (string+ mode "-pushed"))))

  (setf (play-buttons (title-bar self))
        (list (om-make-view 'om-icon-button :position (om-make-point 400 2) :size (om-make-point 22 22)
                            :icon1 "play" :icon2 "play-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-play self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 421 2) :size (om-make-point 22 22)
                            :icon1 "pause" :icon2 "pause-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-pause self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 442 2) :size (om-make-point 22 22)
                            :icon1 "stop" :icon2 "stop-pushed"
                            :action #'(lambda (item) (editor-stop self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 463 2) :size (om-make-point 22 22)
                            :icon1 "rec" :icon2 "rec-pushed"
                            :lock-push t
                            :action #'(lambda (item) (editor-record self)))
              
              (om-make-view 'om-icon-button :position (om-make-point 484 2) :size (om-make-point 22 22)
                            :icon1 "loopbutton" :icon2 "loopbutton-pushed"
                            :lock-push t
                            :selected-p (loop-play self)
                            :action #'(lambda (item) 
                                        (setf (loop-play self)
                                              (not (loop-play self)))
                                        (setf (selected-p item) (loop-play self))
                                        )
                            )
              
              (om-make-view 'om-icon-button :position (om-make-point 524 2) :size (om-make-point 22 22)
                            :icon1 "player" :icon2 "player-pushed"
                            :action #'(lambda (item) 
                                        (let* ((editor self)
                                               (previousplayer (get-edit-param editor 'player))
                                               (newplayer (select-player editor)))
                                          (when (and newplayer (not (equal previousplayer newplayer)))
                                                     ;(om-set-dialog-item-text playertext (player-name newplayer))
                                            (player-special-action newplayer)
                                            (player-init newplayer)
                                            (update-player-controls editor newplayer)))))))

  (apply 'om-add-subviews (cons (title-bar self) 
                                (append (play-buttons (title-bar self))
                                        (edit-buttons (title-bar self))
                                        (mode-buttons (title-bar self))))         
         ))

(defmethod update-mode-buttons ((self score-titlebar))
  (let ((n (get-edit-param (om-view-container self) 'obj-mode)))
    (loop for b in (mode-buttons self)
          for i = 0 then (+ i 1) do
          (setf (selected-p b) (= i n)))
    (om-invalidate-view self)))


(defmethod update-cursor-mode-buttons ((self score-titlebar))
  (let ((mode (get-edit-param (om-view-container self) 'cursor-mode)))
    (setf (selected-p (car (edit-buttons self))) (equal mode :normal)
          (selected-p (cadr (edit-buttons self))) (equal mode :interval))
    (om-invalidate-view self)))

(defmethod update-play-buttons ((self om-view))
  (let ((state (state (player (editor self)))))
    (setf (selected-p (first (play-buttons self))) (or (equal state :play) (equal state :pause))
          (selected-p (second (play-buttons self))) (equal state :pause)
          (selected-p (third (play-buttons self))) (equal state :stop))
    (when (fourth (play-buttons self))
      (setf (selected-p (fourth (play-buttons self))) (equal state :record)))
    (om-invalidate-view self)
    (setf (selected-p (third (play-buttons self))) nil)))


(defmethod editor-play ((self scoreeditor))
  (let ((interval (get-interval-to-play self))
        (cursorevents (mixed-collect-cursor-objects (panel self))))
    (setf *events-play-cursor* 
          (if interval
              (remove-if #'(lambda (x) (or (> (car x) (second interval)) (< (car x) (car interval)))) cursorevents)
            cursorevents)))
  (play-boxes-in-score (panel self))
  (call-next-method)
  (update-play-buttons (title-bar self)))

(defmethod editor-pause ((self scoreeditor))
  (call-next-method)
  (update-play-buttons (title-bar self)))

(defmethod editor-stop ((self scoreeditor))
  (call-next-method)
  (update-play-buttons (title-bar self)))



;===========
;OBJECT ORDER

;;; to be redefined for each class
(defmethod object-order ((self scoreeditor)) '("note" "chord" "chord-seq"))


(defmethod set-obj-mode ((self scoreeditor) n)
  (setf (obj-mode (panel self)) (nth n (object-order self)))
  (set-edit-param self 'obj-mode n))


(defun grap-class-from-type (str)
   (cond
    ((string-equal str "note") 'grap-note)
    ((string-equal str "chord") 'grap-chord)
    ((string-equal str "chord-seq") 'grap-chord-seq)
    ((string-equal str "res") 'grap-note)
    ((string-equal str "group") 'grap-group)
    ((string-equal str "measure") 'grap-measure)
    ((string-equal str "voice") 'grap-voice)
    ((string-equal str "poly") 'grap-poly)
    ((string-equal str "multi-seq") 'grap-multiseq)))


;--------- MENU actions

(defmethod get-menubar ((self scoreeditor)) 
  (list (om-make-menu "File"
                      (list 
                       (list 
                        (om-new-leafmenu "Close" #'(lambda () (om-close-window (om-view-window self))) "w"))
                       (list 
                        (import-menu self)
                        (export-menu self)
                        )
                       (list 
                        (om-new-leafmenu "Page Setup" #'(lambda () (om-page-setup)))
                        (om-new-leafmenu "Print" #'(lambda () (om-print-window (om-view-window self))) "p"))
                       ))
        (om-make-menu "Edit" (remove nil 
                                      (list
                                       (om-new-leafmenu "Undo" #'(lambda() (do-undo self)) "z")
                                       (list 
                                        (om-new-leafmenu "Cut" #'(lambda () (editor-cut self)) "x")
                                        (om-new-leafmenu "Copy" #'(lambda () (editor-copy self)) "c")
                                        (om-new-leafmenu "Paste" #'(lambda () (editor-paste self)) "v"))
                                       (list (om-new-leafmenu "Copy Special (score data)" #'(lambda () (score-copy self)) "C" (score-copy-p self)))
                                       (om-new-leafmenu "Select All" #'(lambda () (editor-select-all self)) "a"))))
        (om-make-menu "Presentation" (list (list :selection
                                      (om-new-leafmenu "Normal" #'(lambda () (change-score-mode (panel self) 0)) 
                                                        nil t #'(lambda () (= (score-mode (panel self)) 0)))
                                      (om-new-leafmenu "Segmentation/Analysis" 
                                                       #'(lambda () (change-score-mode (panel self) 3)) 
                                                       nil t #'(lambda () (= (score-mode (panel self)) 3)))
                                      (om-new-leafmenu "Page" #'(lambda () (change-score-mode (panel self) 2)) 
                                                       nil t #'(lambda () (= (score-mode (panel self)) 2)))
                                      (om-new-leafmenu "Patch" #'(lambda () (change-score-mode (panel self) 1)) 
                                                       nil t #'(lambda () (= (score-mode (panel self)) 1)))
                                      )))
        (make-om-menu 'windows :disable '("Packages" "Global Variables" "Resources") :editor self)
        (make-om-menu 'help :editor self)))



(defmethod editor-select-all ((self scoreEditor)) (select-all (panel self)))

;;; special copuy for copy/paste export (Finale/NAP...)
(defmethod score-copy-p ((self t)) nil)



;----

(defmethod do-print-editor ((self scoreeditor))  
  (om-print-document self))


(defmethod get-defaults-edit-param ((self scoreEditor))
  (default-edition-params (object self)))

(defmethod change-val-of-reference ((self scoreEditor) newobject lastobject)
  (cond  
   ((EditorView-p (ref self))  (change-in-int-editor (ref self) self newobject lastobject))
   ((ominstance-p (ref self)) 
    (setf (instance (ref self)) newobject))
   ((boxtempobj-p (ref self)) 
    (when (listp (value (ref self)))
      (setf (nth 0 (value (ref self))) newobject))
    (om-invalidate-view (car (frames (ref self)))))
   ((is-boxpatch-p  (ref self))
    (setf (value (ref self)) newobject)
    (om-invalidate-view (car (frames (ref self)))))
   (t  nil)))

;------------INITS

(defmethod get-score-class-ctrls ((self scoreEditor)) 'omcontrols-view)

(defmethod editor-palettes ((self scoreEditor)) '(inspector extrapal))


(defmethod get-control-h ((self scoreEditor)) 50)
(defmethod get-editor-field-size ((self scoreEditor)) (om-make-point 300000 20000))

;;(default-edition-params (object self))

(defmethod initialize-instance :after ((self scoreEditor) &rest l)
  (declare (ignore l))
  (let* ((size (get-edit-param self 'fontsize))
         (mode (get-edit-param self 'mode))
         (obj-mode (get-edit-param self 'obj-mode))
         (zoom (get-edit-param self 'zoom))
         (score-mode (or (get-edit-param self 'score-mode) 0))
         (noteaschan (get-edit-param self 'notechancolor?))
         (stemp (get-edit-param self 'show-stems))
         (approx (get-edit-param self 'approx))
         (ed-view (om-make-view (get-score-class-panel self) 
                                 :position (om-make-point 0 0) 
                                 :font (om-make-music-font *heads-font* size)
                                 :field-size (get-editor-field-size self)
                                 :scrollbars (first (metaobj-scrollbars-params self))
                                 :owner self
                                 ;;;:size (om-make-point (- (w self) 15) (- (h self) 40))
                                 :size (om-make-point (w self) (- (h self) (get-control-h self)))
                                 :staff-tone approx
                                 :noteaschan? noteaschan
                                 :staff-zoom zoom
                                 :staff-mode mode
                                 :edition-values (correct-page-par self (get-edit-param self 'cmnpref))
                                 :staff-size size))
         controls staff)
    ;(om-set-field-size ed-view (get-editor-field-size self))
    (setf (show-stems ed-view) stemp)
    (setf (panel self) ed-view)
    (setf staff (correct-staff (panel self) (get-edit-param self 'staff)))
    (setf (staff-sys (panel self)) (get-staff-system staff))
    (setf controls (om-make-view  (get-score-class-ctrls self) 
                                  :position (om-make-point 0 (- (h self) (get-control-h self))) 
                                  :size (om-make-point (w self) (get-control-h self))
                                  :staff (string  (car (list! staff)))
                                  :owner self
                                  :zoom zoom
                                  :mode mode
                                  :font-size (format nil "~D" size)
                                  :tone (car (find approx (editor-tone-list) :key 'cadr :test 'equal))))
    (setf (ctr-view self) controls)

    ;;;;;;;
    (when *om-tonalite*
      (set-editor-tonality (panel self)))
    ;;;;;;;
    (when (equal score-mode 1) (setf score-mode 0))
    (change-score-mode ed-view score-mode)
    (setf (score-mode ed-view) score-mode)
    (setf (obj-mode ed-view) (nth obj-mode (object-order self)))
    (change-slot-edit ed-view (slots-mode ed-view))
    (change-cursor-mode (panel self) (or (get-edit-param self 'cursor-mode) :normal))
    (init-draw self)
    (init-boxes-in-score ed-view)))





(defmethod set-cursor-mode ((self scoreeditor) &optional mode)
  (change-cursor-mode (panel self) mode)
  (set-edit-param self 'cursor-mode (cursor-mode (panel self)))
  (update-cursor-mode-buttons (title-bar self)))

(defmethod update-controls-view ((self scoreeditor))
  (let ((size (get-edit-param self 'fontsize))
        (mode (get-edit-param self 'mode))
        (staff (correct-staff (panel self) (get-edit-param self 'staff)))
        (zoom (get-edit-param self 'zoom))
        (approx (get-edit-param self 'approx)))
    (when (ctr-view self)
      (om-remove-subviews self (ctr-view self)))
    (setf controls (om-make-view  (get-score-class-ctrls self) 
                                  :position (om-make-point 0 (- (h self) (get-control-h self))) 
                                  :size (om-make-point (w self) (get-control-h self))
                                  :staff (string  (car (list! staff)))
                                  :owner self
                                  :zoom zoom
                                  :mode mode
                                  :font-size (format nil "~D" size)
                                  :tone (car (find approx (editor-tone-list) :key 'cadr :test 'equal))))
    (setf (ctr-view self) controls)))

(defmethod init-draw ((self scoreEditor))
   (update-panel (panel self)))


;-------------EVENTS

(defmethod update-subviews ((self scoreeditor))
   (when (title-bar self)
     (om-set-view-size  (title-bar self) (om-make-point (w self) *titlebars-h*)))
   (om-set-view-size  (panel self ) (om-make-point (w self) (- (h self) (get-control-h self) *titlebars-h*)))
   (om-set-view-position  (panel self ) (om-make-point 0 *titlebars-h*))
   (om-set-view-size  (ctr-view self) (om-make-point (w self) (get-control-h self)))
   (om-set-view-position (ctr-view self) (om-make-point 0 (- (h self) (get-control-h self))))
   (om-invalidate-view self))


;-------------UPDATES
;used in applications after a patch evaluation
(defmethod update-item-value ((self scoreEditor) val)
   (setf (object self) val)
   (update-panel (panel self)))

;called after the patch valuation if the editor is open
(defmethod update-editor-after-eval ((self scoreEditor) val) 
  (om-with-compiler-warnings nil
    (setf (object self) val)
    (setf (selection? (panel self)) nil)
    (init-music-patch (panel self))
    (remove-panel-boxes (panel self))
    (update-panel (panel self))
    (om-invalidate-view (title-bar self))
    ))

(defmethod report-modifications ((self scoreeditor))
  (update-inspector self 0) 
  (update-slot-edit (panel self))
  (segmentation-update (panel self))
  (call-next-method))



(defmethod change-in-int-editor ((self scoreEditor) (internal scoreEditor) newobject lastobj)
   (change-in-int-editor (panel self) (panel internal) newobject lastobj))

;;; sert plus a rien..
;;;(defmethod editor-event-after ((self scoreEditor)) 
;;;   (let ((mouse (om-mouse-position self)))
;;;     (when (and (help-on?) (not (equal mouse *mouse-help-position*)))
;;;       (set-help nil))
;;;     (setf *mouse-help-position* mouse)))



;===========================================================
;Controls par default
;===========================================================

(defclass omcontrols-view (3dBorder-view) 
  ((slotedit :initform nil :accessor slotedit))
  (:default-initargs 
   :draw-with-buffer t
    :c++ *controls-color++* :c+ *controls-color+* 
    :c-- *controls-color--* :c- *controls-color-*))

(defmethod GET-staff-LIST ((self omcontrols-view)) *chord-satff-om*)
(defmethod GET-slot-LIST ((self omcontrols-view)) 
  '(("midic" midic) ("channel" chan) ("dur" dur) ("dyn" dyn) ("port" port) ("offset" offset)))

(defmethod GET-tone-LIST ((self omcontrols-view)) (editor-tone-list))

(defun editor-tone-list ()
  (loop for item in *scales-list* collect (list (third item) (car item))))


;-------------INITS
(defmethod initialize-instance :after ((self omcontrols-view) &rest l 
                                       &key (tone "1/2") (staff "ffgg") (font-size "24") (zoom 1) (mode 0))
  (declare (ignore l))
  
  (let* ((bgcol *controls-color*)
         (l1 230)
         (l2 380)
         (c1 2)
         (c2 25)

         ;;; Slot
         (minied (om-make-dialog-item 'edit-numbox (om-make-point 96 (+ c1 2)) (om-make-point 50 18) " "
                                      :value nil
                                      :font *om-default-font1*
                                      :bg-color *om-white-color*
                                      :help-spec ""
                                      ))
         
         
         
         ;(slotitem (om-make-dialog-item 'om-static-text 
         ;                               (om-make-point 2 24) 
         ;                               (om-make-point 40 20) "View"
         ;                               :font *controls-font*
         ;                              :bg-color *controls-color*))
         (slotbut (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point 5 c1) 
                                       (om-make-point 80 22)
                                       ""
                                       :range (loop for item in (GET-slot-LIST self) collect (car item)) 
                                       :value "midic"
                                       :font *om-default-font1* ;*om-default-font1*
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
                                       
                                       :font *om-default-font1* 
                                       :range (loop for item in *mus-font-size* collect (car item)) 
                                       :value font-size
                                       ))
         
         ;;; staff
         (staffitem (om-make-dialog-item 'om-static-text (om-make-point (- l2 36) (+ c1 2)) (om-make-point 60 20) "Staff"
                                         :font *om-default-font1*
                                         :bg-color *controls-color*))
         
         (staffbut (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point l2 c1) 
                                       (om-make-point 80 22)
                                       ""
                                       :di-action (om-dialog-item-act item
                                                               (let ((newstaff (cadr (nth (om-get-selected-item-index item) (GET-staff-LIST self)))))
                                                                 (change-system (panel (om-view-container self)) newstaff)))
                                       
                                       :font *om-default-font1*
                                       :range (loop for item in (GET-staff-LIST self) collect (car item)) 
                                       :value (car (find (get-edit-param (editor (om-view-container self)) 'staff) (GET-staff-LIST self) :key 'cadr :test 'equal))
                                       ))   
         ;;; approx
         (toneitem (om-make-dialog-item 'om-static-text (om-make-point (- l2 50) (+ c2 2)) (om-make-point 52 20) "Approx"
                                        :font *om-default-font1*
                                        :bg-color *controls-color*))
         
         (tonebut (om-make-dialog-item 'om-pop-up-dialog-item 
                                       (om-make-point l2 c2)
                                       (om-make-point 80 22) ""
                                       :di-action (om-dialog-item-act item
                                                               (let ((newtone (cadr (nth (om-get-selected-item-index item) (GET-tone-LIST self)))))
                                                                 (change-editor-tone (panel (om-view-container self)) newtone)))
                                       :font *om-default-font1*
                                       :range (loop for item in (GET-tone-LIST self) collect (car item)) 
                                       :value tone
                                       ))
         )
         
    
    (setf (slotedit self) minied)
    (om-add-subviews self  staffitem staffbut sizeitem slotbut toneitem tonebut minied  sizebut)
                            
    ;;(additional-port-menu (title-bar (om-view-container self)) :pos (om-make-point 300 4) :color *editor-bar-color*)
    (add-zoom2control self zoom (om-make-point l1 c1))
    
    (om-set-bg-color self *controls-color*)
    
    )
)





(defun add-zoom2control (control zoom &optional position)
  (setf zoom (round (* zoom 100)))
  (let ((pos (or (om-add-points position (om-make-point 3 2))
                 (om-make-point 100 2))))
  (om-add-subviews control 
                   (om-make-dialog-item 'numbox pos (om-make-point 46 18) (format nil " ~D" zoom)
                                        :di-action (om-dialog-item-act item
                                                              (change-editor-zoom (panel (om-view-container (om-view-container item))) (value item)))
                                        :font *om-default-font1*
                                        :bg-color *om-white-color*
                                        :value zoom
                                        :afterfun #'(lambda (item)
                                                      (change-editor-zoom-after (panel (om-view-container (om-view-container item))) (value item)))
                                        :min-val 1
                                        :max-val 1000)
                   (om-make-dialog-item 'om-static-text (om-make-point (- (om-point-h pos) 40) 4) (om-make-point 40 20) "Zoom"
                                         :font *om-default-font1*
                                         :bg-color *controls-color*))))

(defmethod additional-port-menu ((self t) &key (pos (om-make-point 410 1)) (color *om-white-color*) (in t) (out t))
  (when in
    (om-add-subviews self
                     (om-make-dialog-item 'om-static-text pos (om-make-point 43 12) "InPort"
                                          :font *om-default-font1*
                                          :bg-color color)
                     (om-make-dialog-item 'numbox (om-add-points pos (om-make-point 50 -1)) (om-make-point 40 18)
                                          (format nil " ~D" (get-edit-param (om-view-container self) 'inport))
                                          :di-action (om-dialog-item-act item
                                                                (change-editor-inport (editor (om-view-container (om-view-container item))) (value item)))
                                          :font *om-default-font1*
                                          :value (get-edit-param (om-view-container self) 'inport)
                                          :min-val 0
                                          :max-val 255))))

(defun add-chordseq-control (self mode)
  (om-add-subviews self 
                   (om-make-dialog-item 'om-pop-up-dialog-item 
                                        (om-make-point 5 25)
                                        (om-make-point 80 22) ""
                                        :di-action (om-dialog-item-act item
                                                     (let ((newtone (case (om-get-selected-item-index item) (0 0) (1 4))))
                                                       (change-editor-mode (panel (om-view-container self)) newtone)))
                                        :font *om-default-font1*
                                        :range '("chord" "offset") 
                                        :value (case mode (0 "chord") (4 "offset"))
                                        )))

(defun add-chord-control (self mode)
  (om-add-subviews self 
                   (om-make-dialog-item 'om-pop-up-dialog-item 
                                        (om-make-point 5 25)
                                        (om-make-point 80 22) ""
                                        :di-action (om-dialog-item-act item
                                                     (let ((newtone (cadr (nth (om-get-selected-item-index item) '(("chord"  0) ("arpUp" 1) ("arpDown" 2) ("order" 3) ("offset" 4))))))
                                                       (change-editor-mode (panel (om-view-container self)) newtone)))
                                        :font *om-default-font1*
                                        :range (loop for item in '(("chord"  0) ("arpUp" 1) ("arpDown" 2) ("order" 3) ("offset" 4)) collect (car item)) 
                                        :value (case mode (0 "chord") (1 "arpUp") (2 "arpDown") (3 "order") (4 "offset"))
                                        )
                   ))

;;; when slotedit has no slot "edit" :)
(defmethod edit ((self t)) nil)

(defmethod om-view-click-handler ((self omcontrols-view) pos)
  (when (edit (slotedit self))
    (exit-from-dialog (edit (slotedit self)) (om-dialog-item-text (edit (slotedit self)))))
  (call-next-method))

(defmethod set-mini-ed-page ((self scoreeditor) page?)
  (let ((control-view (ctr-view self)))  
  (when (slotedit control-view)
    (om-remove-subviews control-view (slotedit control-view))
    (setf (slotedit control-view) nil))
  (let* ((bgcol *controls-color*)
         (l1 230)
         (l2 380)
         minied)
    (if page?
      (setf minied (om-make-dialog-item 'om-pop-up-dialog-item
                                        (om-make-point 96 5) (om-make-point 70 18) " "
                                        :di-action (om-dialog-item-act item
                                                     (let ((newsize (cadr (nth (om-get-selected-item-index item) *mus-page-factors*))))
                                                       (change-editor-scale (panel self) newsize)))

                                        :font *om-default-font1*
                                        :range (loop for item in *mus-page-factors* collect (car item))
                                        :value "100%"
                                        ))
      (setf minied (om-make-dialog-item 'edit-numbox (om-make-point 96 3) (om-make-point 50 18) " "
                                      :value nil
                                      :font *om-default-font1*
                                      :bg-color *om-white-color*
                                      :help-spec ""
                                      )) )
    (setf (slotedit control-view) minied)
    (om-add-subviews control-view minied))))
;===========================================================
;default music panel
;===========================================================


(omg-defclass scorePanel (patchpanel cursor-play-view-mixin om-view-drag om-view-drop) 
   ((staff-size :initform 24 :initarg :staff-size :accessor staff-size)
    (staff-sys :initform 'ggff :initarg :staff-sys :accessor staff-sys)
    (staff-mode :initform 0  :initarg :staff-mode :accessor staff-mode)
    (staff-tone :initform 2 :initarg :staff-tone :accessor staff-tone)
    (staff-zoom :initform 1 :initarg :staff-zoom :accessor staff-zoom)
    (noteaschan? :initform nil :initarg :noteaschan? :accessor noteaschan?)
    (slots-mode :initform 'midic  :accessor slots-mode)
    (selection? :initform nil :accessor selection?)
    (graphic-obj :initform nil :accessor graphic-obj)
    (recording? :initform nil :accessor recording?)
    (edition-values :initform nil :initarg :edition-values :accessor edition-values)
    ;;(linear? :initform t :accessor linear?)
    (obj-mode :initform "note" :accessor obj-mode)
    (score-mode :initform 0  :accessor score-mode)
    (show-stems :initform t  :accessor show-stems)
    (def-subdiv :initform 3  :accessor def-subdiv)
    (meaure-edition-mode :initform 2  :accessor meaure-edition-mode)
    (edit-cursor :initform nil  :accessor edit-cursor)
    (object :initform nil :accessor object)
    (extra-palette :initform nil :accessor extra-palette)
    (extra-mode :initform 'nothing :accessor extra-mode)
    (pagination :initform nil :accessor pagination)
    ;(player :initform nil :accessor player)
    (score-action-boxes :initform nil :accessor score-action-boxes)
    (grille-p :initform nil :accessor grille-p)
    (grille-step :initform 1000 :accessor grille-step)
    (timebpf :accessor timebpf :initarg :timebpf :initform nil))
   (:default-initargs :field-size (om-make-point 20000 10000)
    :scrollbars t))

(defmethod get-score-class-panel ((self scoreEditor)) 'scorePanel)

(defmethod start-position ((self scorepanel)) 
  (if (equal (get-edit-param (om-view-container self) 'cursor-mode) :normal)
      0
    (call-next-method)))

(defmethod linear? ((self scorepanel)) (not (= (score-mode self) 2)))
(defmethod in-patch-mode? ((self scorepanel)) (= (score-mode self) 1))
(defmethod in-page-mode? ((self scorepanel)) (= (score-mode self) 2))


(defmethod cursor-by-obj-mode ((self scorePanel))
   (cond
    ((string-equal (obj-mode self) "note") *c-nota*)
    ((string-equal (obj-mode self) "chord") *c-chord*)
    ((string-equal (obj-mode self) "group") *c-group*)  
    ((string-equal (obj-mode self) "measure") *c-measure*)
    ((or (string-equal (obj-mode self) "voice")
         (string-equal (obj-mode self) "chord-seq"))
         *c-voice*)
    (t *om-arrow-cursor*)))

(defmethod start-position ((self scorepanel)) 
  (if (equal (get-edit-param (om-view-container self) 'cursor-mode) :normal)
      0
    (call-next-method)))


;;; pour pas perdre les scrollbars quand on commence a mettre des objets dans le score-patch...
(defmethod set-field-size ((self scoreeditor)) 
   t)

(defmethod set-field-size ((self scorepanel)) 
   t)

(defmethod correct-staff ((self scorepanel) staff)
  (let ((staff (if (member staff *chord-satff-om* :test 'equal :key 'cadr) staff
                   (second (cadr *chord-satff-om*)))))
    (set-edit-param (editor self) 'staff staff)
    staff))


(defmethod om-invalidate-after-scroll ((self scorepanel))
   (om-invalidate-view self t))


(defmethod init-boxes-in-score ((self scorePanel))
  (setf (score-action-boxes self) (cons-action-boxes-and-chords self))
  (when (in-patch-mode? self)
     ;(om-set-bg-color self *scorepatch-color*)
    (change-text (title-bar (editor self)) "PATCH MODE") 
    (setf *redraw-diamonds* t)
    (set-panel-boxes self)))


(defmethod cursor-panes ((self scoreeditor))
  (list (panel self)))


(defvar *scorehelp* nil)
(setf *scorehelp* '((alt+clic "New Object")
                    (del "Delete Selection")
                    (("c") "Show Channel Color")
                    (("C") "Change Selection Color")
                    (("o") "Open Selection Internal Editor")
                    (ud "Transpose Selection")
                    (lr "Change Selection Offset/Duration")
                    (space "Play/Stop")))

(defmethod get-help-list ((self scorepanel)) (list *scorehelp*))

;-----------------EVENTS

(defmethod change-in-int-editor ((self scorePanel) (internal scorePanel) newobject lastobj)
   (update-panel self t))


(defmethod temporal-get-notes ((self container))
   (loop for item in (inside self)
         append (temporal-get-notes item)))

(defmethod temporal-get-notes ((self continuation-chord)) nil)

(defmethod temporal-get-notes ((self simple-score-element)) (list self))


(defmethod handle-key-event ((self scorePanel) char)
  (cond ((in-patch-mode? self)
         (case char
           (#\t (mk-musobj-box self))
           (t (call-next-method))))
        ((edit-cursor self)
         (cond 
          ((and (rythmic? (edit-cursor self)) (char-is-figure char))
           (add-or-replace-in-measure self char))
          ((or (equal char :om-key-return) (equal char :om-key-enter))
           (if (assoc-chord (edit-cursor self))
               (enter-a-note self)
             (om-beep-msg "Enter a number between 0 and 7")))
          ((and (rythmic? (edit-cursor self)) (equal char #\TAB))
           (create-new-edit-cursor self))
          ((equal char :om-key-up)
           (move-editor-pitch self 0)
           (update-panel self t))
          ((equal char :om-key-down)
           (move-editor-pitch self 1)
           (update-panel self t))
          ((and (rythmic? (edit-cursor self)) (equal char :om-key-left))
           (advance-edit-cursor self -1))
          ((and (rythmic? (edit-cursor self)) (equal char :om-key-right))
           (advance-edit-cursor self 1))
          ;;; MARCHE PAS
          ((equal char #\.) (point-edit-cursor self))
          ))
        (t 
         (case char
           (#\SPACE (editor-play/stop (editor self)))
           (#\c (note-chan-color self))
           (#\n (set-name-to-mus-obj self))
           (#\h (show-help-window (format nil "Commands for ~A Editor" 
                                          (string-upcase (class-name (class-of (object (editor self)))))) 
                                  (get-help-list self)))
           (#\t (when *om-tonalite*
                  (score-set-tonalite self)
                  )) 
           (#\T (when *om-tonalite*
                  (score-remove-tonalite self)
                  )) 
           (#\s (create-editor-scale (editor self)))
           (:om-key-tab (change-obj-mode self 1))
           (:om-key-up  (move-selection self 0))
           (:om-key-down  (move-selection self 1))
           (otherwise (if (selection? self)
                          (if (char-is-digit char) (do-subdivise self char)
                            (case char
                              (:om-key-left (cond ((extra-p (car (selection? self)))
                                                   (advance-extras self -1))
                                                  ((equal (slots-mode self) 'dur)
                                                   (change-dur self -1))
                                                  (t (change-x self -1))))
                              (:om-key-right (cond ((extra-p (car (selection? self)))
                                                    (advance-extras self 1))
                                                   ((equal (slots-mode self) 'dur)
                                                    (change-dur self 1))
                                                   (t (change-x self 1))))
                              (#\C  (set-color-to-mus-obj self))
                           
                              (:om-key-delete (delete-selection self))
                              (:om-key-esc (toggle-selection self))
                              (#\+ (do-union self))
                              (#\* (do-group self))
                              (#\- (un-group self))
                           ;(#\= (if (om-option-key-p) (untie-selection self) (tie-selection self)))
                              (#\= (tie-selection self))
                              (#\/ (untie-selection self))
                              (#\o (open-internal-editor self))
                              ;; (#\/ (subdivise-edit-cursor self)) ;a faire
                           
                              (otherwise (om-beep))))
                        (case char
                          (:om-key-esc (reset-cursor self)))
                        ))
           )
         (when (editor self) (update-inspector (editor self) 0))
         )))
  



(defmethod set-color-to-mus-obj ((self scorePanel))
  (when (selection? self)
    (let ((color (om-choose-color-dialog)))
      (when color
        (loop for item in (selection? self) do
              (set-mus-color item color))
        (om-invalidate-view self t)))))

(defmethod set-name-to-mus-obj ((self t)) nil)

  
(defmethod omselect-with-shift ((self scorePanel) graph-obj)
   (if (member (reference graph-obj) (selection? self) :test 'equal)
     (progn
       (setf (selection? self) (remove  (reference graph-obj) (selection? self) :test 'equal))
       (setf (selected graph-obj) nil))
     (push-select-note self graph-obj)))

;; optimisation variable pour eviter les operations inutiles
(defvar *score-lock* nil)

(defmethod select-note ((self scorePanel) note)
   (setf (selection? self)  (list (reference note)))
   (setf (selected note) t)
   (when (and (not *score-lock*) *scoreinspector* (show *scoreinspector*))
     (get-inspector (editor self))))

(defmethod push-select-note ((self scorePanel) note)
   (push (reference note) (selection? self))
   (setf (selected note) t)
   (when (and (not *score-lock*) *scoreinspector* (show *scoreinspector*))
     (get-inspector (editor self)))
   )

(defmethod extra-palette-action? ((self scorePanel))
  (and (extra-palette self) (not (equal (extra-mode self) 'nothing))))



(defmethod om-view-click-handler ((self scorePanel) where)
  (when (equal (om-score-click-handler self where nil) :call-next-method)
    (call-next-method)))

(defmethod om-view-doubleclick-handler  ((self scorePanel) where)
   (when (equal (om-score-click-handler self where t) :call-next-method)
     (call-next-method)))


(defun click-in-key? (self system x y width size score where)
  (let ((posy y) rep)
    (loop for sys in  (list! system)
          for i = 0 then (+ i 1)
          while (not rep) do
          (when (point-in-rectangle-p where posy x  (+ posy (get-delta-system sys size score i)) (+ x (* size 2)))
            (setf rep sys))
          (setf posy (+ posy (get-delta-system sys size score i)))
          )
    rep))
 

(defmethod omselect-staff-with-shift ((self scorePanel) graph-obj)
  (unless (system? (car (selection? self)))
    (setf (selection? self) nil))
  (if (member (reference graph-obj) (selection? self) :test 'equal)
    (setf (selection? self) (remove (reference graph-obj) (selection? self) :test 'equal))
    (push (reference graph-obj) (selection? self)))
  )

(defmethod get-click-in-obj ((self scorePanel) grap-obj mode where)
  (if (score-page-mode self)
      (page-click-in-obj self grap-obj mode where)
      (click-in-obj grap-obj mode where)))


(defmethod collect-page-all-line-elements ((self scorePanel) grap-obj fdoc pagenum line)
  (get-page-line-elements  grap-obj fdoc pagenum line 0))


(defmethod get-page-all-line-elements ((self scorePanel) fdoc pagenum grap-obj)
  (loop for line from 0 to (- (howmany-lines fdoc pagenum) 1) append
                              (collect-page-all-line-elements self  grap-obj fdoc pagenum line)))

(defmethod page-click-in-obj ((self scorePanel) grap-obj mode where)
  (unless (equal mode 'contex) ;;; mmm
  (let ((rectangles (get-rectangle-only-pages self))
        (factor (or (score-scale self) 1))
        pagenum currect)
    (loop for item in rectangles
          for i = 0 then (+ i 1)
          while (not pagenum) do
          (when (om-point-in-rect-p where item)
            (setf pagenum i)
            (setf currect item)))
    (when pagenum
      (setf where (point-pixel-to-page currect factor where))
      (let* ((fdoc (score-fdoc self))
             (elements (get-page-all-line-elements self fdoc pagenum grap-obj))
             rep)
        (loop for item in elements
          while (not rep) do
          (setf rep (click-in-obj item mode where))) rep)))))


(defmethod om-score-click-handler ((self scorePanel) where double-click-p)
  (or (and (score-in-extra-mode-p) (not (in-patch-mode? self))
           (om-score-click-extra self where double-click-p))
      (let ((staff-selection? (click-in-key? (graphic-obj self) (staff-sys self) (om-h-scroll-position self)  
                                             (* (staff-size self) (score-top-margin self)) 
                                             (w self) (staff-size self) self where)))
        (if (and staff-selection? (not (cursor-p self)))
            (progn 
              (if (om-shift-key-p) 
                  (omselect-staff-with-shift self staff-selection?)
                (when (not (member (reference staff-selection?) (selection? self) :test 'equal))
                  (off-selection self)
                  (setf (selection? self)  (list (reference staff-selection?)))
                  ))
              (setf (edit-cursor self) nil)
              (om-invalidate-view self t))
          (let* ((mode-obj (grap-class-from-type  (obj-mode self)))
                 (graph-obj (get-click-in-obj self (graphic-obj self) mode-obj where))
                 (segment nil))
            (cond ((in-patch-mode? self)
                   ;;; PATCH MODE
                   (cond 
                    ((om-control-key-p) 
                     (if graph-obj (menu-item-context graph-obj self)
                       (menu-item-context (om-view-container self) where)))
                    (graph-obj
                     (if (om-shift-key-p) 
                         (omselect-with-shift self graph-obj )
                       (when (not (member (reference graph-obj) (selection? self) :test 'equal))
                         (off-selection self)
                         (select-note self graph-obj)))
                     (setf (edit-cursor self) nil)
                     (if (equal mode-obj 'grap-note)
                         (update-slot-edit self)
                       (om-invalidate-view self t))
                     ;;; PATCH :
                     :call-next-method)
                    (t (when (or (selection? self) (edit-cursor self))
                         (setf (edit-cursor self) nil)
                         (make-unselect self)
                         (om-invalidate-view self t))
                       ;;; PATCH :
                       :call-next-method
                       )
                    ))
                   ((and (cursor-p self) (not (om-add-key-p)))
                    (om-with-focused-view self (control-actives self where)))
                   ;;; ANALYSIS / SEGMENTATION MODE
                   ;((analysis-mode? self)
                   ; (off-selection self)
                   ; (if (om-add-key-p)
                   ;     (handle-add-click-analysis self where) ;;; HERE : ADD A NEW SEGMENT SOMEHOW...
                   ;   (progn 
                   ;     (setf graph-obj (click-in-segment self where)) ;;; HERE RETURN A SEGMENT IF CLICKED INSIDE 
                   ;     (if double-click-p
                   ;         (handle-doubleclick-analysis graph-obj self where)
                   ;       (handle-click-analysis graph-obj self where))
                   ;     ))  
                   ; )

                   ;;; NORMAL MODE OR ANALYSIS
                   (t
                    (if (analysis-mode? self) 
                      (progn
                        (off-analysis-selection self)
                        (setf segment (click-in-segment self where))
                        ;;; segment = (analysis segment)
                        )
                      (cond 
                       ((not graph-obj)   ;(and (not graph-obj) (extra-palette self))
                        (setf graph-obj (click-in-extra-p (graphic-obj self) where)))
                       ((and graph-obj (not (om-add-key-p)) (extra-palette-action? self))
                        (make-new-extra-mode self graph-obj)
                        (setf graph-obj nil)))
                      )
                    (cond 
                     ((om-add-key-p) 
                      (if (analysis-mode? self)
                          (handle-add-click-analysis self where)
                        (add-new-object self mode-obj where graph-obj))
                      (when (editor self) (update-inspector (editor self) 0)))
                     ((and (grap-extra-p graph-obj) double-click-p) 
                      (open-extra-editor self graph-obj))
                     
                 ;((om-control-key-p)
                 ; (if graph-obj (menu-item-context graph-obj self)   ;;; !!! pas bo normalement il faut passer where
                 ;   (menu-item-context (om-view-container self) where)))
                     ((or graph-obj segment)
                      ;;; new 
                      (if double-click-p
                          (if (and (analysis-mode? self) segment)
                              (handle-doubleclick-analysis segment self where)
                            (progn 
                              (off-selection self)
                              (select-note self graph-obj)
                              (open-internal-editor self)))
                        ;;;
                        (if (and (analysis-mode? self) (not graph-obj) segment)
                            (handle-click-analysis segment self where)
                          (if (om-shift-key-p) 
                              (omselect-with-shift self graph-obj)
                            (when (not (member (reference graph-obj) (selection? self) :test 'equal))
                              (off-selection self)
                              (select-note self graph-obj)
                              (get-inspector (editor self))))
                          )
                        )
                      (setf (edit-cursor self) nil))
                     (t (om-with-focused-view self (control-actives self where))
                        (when (edit-cursor self)
                          (setf (edit-cursor self) nil)
                          (om-invalidate-view self))
                        (get-inspector (editor self)))
                     )
                    (update-slot-edit self)
                    )
                   )
                  )))))



(defmethod off-selection ((self scorePanel))
  (when (analysis-mode? self)
    (off-analysis-selection self))
  (let ((selected-things (get-graph-selection? self (grap-class-from-type (obj-mode self)))))
    (loop for item in selected-things do
          (setf (selected item) nil))))

(defmethod make-unselect ((self scorePanel))
   (off-selection self)
   (setf (selection? self) nil)
   (when (and *scoreinspector* (show *scoreinspector*))
     (get-inspector (editor self))))
       
(defmethod get-graph-selection? ((self scorePanel) mode-obj)
   (select-grap-objs (graphic-obj self) mode-obj (selection? self)))

(defmethod control-actives ((self scorePanel) where)
  (unless (in-patch-mode? self)
    (let* ((window (window self))
           (mode-obj (grap-class-from-type (obj-mode self))))
      (when (and (editor self) (text-view (editor self)))
        (exit-from-dialog (text-view (editor self)) (om-dialog-item-text (text-view (editor self)))))
      (unless (om-shift-key-p)
        (when (selection? self) (make-unselect self)
          (om-invalidate-view self))
        )
      ))
    (call-next-method)
    )


(defun rect-intersect (rect1 rect2)
  (and (<= (max (nth 0 rect1) (nth 0 rect2)) (min (nth 2 rect1) (nth 2 rect2)))
       (<= (max (nth 1 rect1) (nth 1 rect2)) (min (nth 3 rect1) (nth 3 rect2)))))


(defmethod do-select-items-in-rect ((self scorePanel) rect) 
  (let ((mode-obj (grap-class-from-type  (obj-mode self))))
    (setf *score-lock* t)
    (loop for item in (get-graph-type-obj (graphic-obj self) mode-obj) do
          (when (rect-intersect (rectangle item) 
                                (list (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
            (push-select-note self item)
            ))
    (setf *score-lock* nil)
    (when (selection? self) 
      (om-invalidate-view self t)
      (update-slot-edit self))
    ))


                                      
(defmethod om-view-cursor ((self scorePanel))
   (cond 
    ((om-add-key-p) *add-cursor*)
    (t (cursor-by-obj-mode self))))



;--------------------EXTRAS

;(get-extra-param *extramanager* (edit-mode *extramanager*))
  
;((and (grap-extra-p graph-obj) double-click-p) 
;                  (open-extra-editor self graph-obj))

;((om-control-key-p)
;                  (if graph-obj (menu-item-context graph-obj self)   ;;; !!! pas bo normalement il faut passer where
;                    (menu-item-context (om-view-container self) where)))

(defmethod om-score-click-extra ((self scorePanel) where double-click-p)
  (let* ((extra-mode (score-get-extra-mode))
         (mode-obj (grap-class-from-type  (obj-mode self)))
         (extra-obj (click-in-extra-p (graphic-obj self) where))
         grap-obj)
    (cond
     ((and extra-obj (not (equal (reference extra-obj) (car (selection? self)))))
      (select-note self extra-obj)
      (om-invalidate-view self))
     ((and (selection? self) (compose-extra-p (car (selection? self))))
      (let ((action (do-click-compose-extra self (car (selection? self)) where)))
        (unless action (setf (selection? self) nil))
        (om-invalidate-view self)))
     ((om-add-key-p)
       (setf graph-obj (get-click-in-obj self (graphic-obj self) mode-obj where))
       (make-new-extra-mode self where graph-obj double-click-p)
       t)
      (t nil))))


(defun score-in-extra-mode-p ()
  (and *extramanager* (edit-mode *extramanager*)))

(defun score-get-extra-mode ()
  (when *extramanager*
    (cond ((equal (edit-mode *extramanager*) :figure)
           'accent)
          ((equal (edit-mode *extramanager*) :dyn)
           'dynamic)
          ((equal (edit-mode *extramanager*) :text)
           'text)
          (t (car (get-extra-param *extramanager* (edit-mode *extramanager*)))))))

(defun score-get-extra-params ()
    (get-extra-param *extramanager* (edit-mode *extramanager*)))


;(defmethod advance-extras ((self scorePanel) dir)
;   (loop for item in (selection? self) do
;         (when (extra-p item)
;           (move-in-x item dir)))
;   (update-panel self t))

(defmethod advance-extras ((self scorePanel) dir)
   (loop for item in (selection? self) do
         (when (extra-p item)
           (move-in-x item (/ dir (round (staff-size self) 2)))))
   (update-panel self t))


(defvar *start-extra-obj-click* nil)
(defvar *start-extra-gobj-click* nil)
(defvar *extra-initial-pos* nil)
(defmethod make-new-extra-mode ((self scorePanel) where gobj dc)
   (let ((mode (score-get-extra-mode))
         obj)
     (when gobj
       (setf obj (reference gobj))
       (setf *start-extra-obj-click* obj)
       (setf *start-extra-gobj-click* gobj))
     (setf *extra-initial-pos* where)
     (add-new-extra-drag self where obj mode dc)))

(defmethod make-connection-motion ((self scorepanel) pos)
  (let* ((panel self)
         (initpoint pos)
         (rx (om-point-h initpoint))
         (ry (om-point-v initpoint))
         (rect  (om-init-point-movable-object panel)))
    (when rect
      (let* ((newrect (om-pts-to-rect (om-make-point (first rect) (second rect)) (om-make-point rx ry)))
             (nx  (om-rect-left newrect))
             (ny (om-rect-top newrect))
             (nw (om-rect-w newrect))  
             (nh (om-rect-h newrect)))
        (om-update-movable-object panel nx ny (max nw 2) (max nh 2))))))

(defmethod release-connection-motion ((self scorepanel) pos)
  (om-erase-movable-object self)
  (do-release-extra-action self (score-get-extra-mode) pos ))

(defmethod do-release-extra-action ((self scorepanel) mode pos) t)
       
;;;==========================
;;; DRAW
;;;==========================

;;; (heads alteration clef channels-ports dynamiques groups ? points)  
(defvar *internal-score-fonts* nil)

(defun get-font-to-draw (index)
  (nth index *internal-score-fonts*))

; (om-make-font *heads-font* size)

(defun init-fonts-to-draw (fontsize)
 (list 
  (om-make-font-object (om-make-music-font *heads-font* fontsize))
  (om-make-font-object (om-make-music-font *micron-font* fontsize)) 
  (om-make-font-object (om-make-music-font *signs-font* fontsize))  
  (om-make-font-object (om-make-music-font *signs-font* (round (* fontsize 0.8))))
  (om-make-font-object (om-make-music-font *extras-font* fontsize))
  (om-make-font-object (om-make-music-font *om-def-font-face* (round fontsize 2.3) ))   
  (om-make-font-object (om-make-music-font *om-def-font-face* (round fontsize 2.8))) 
  (om-make-font-object (om-make-music-font *om-def-font-face*  fontsize))
  (om-make-font-object (om-make-music-font *heads-font*  (round fontsize 1.6)))))


(defmethod get-key-space ((self scorePanel))
   (* (get-deltax (staff-sys self)) (staff-size self) 1.5))


(defmethod om-draw-contents ((self scorePanel))
  (call-next-method) 
  (let ((*internal-score-fonts* (init-fonts-to-draw (staff-size self))))
    (if (score-page-mode self)
        (draw-panel-pages self)
      (om-with-focused-view self
        (when (in-patch-mode? self)
          (om-with-fg-color self *scorepatch-color*
            (om-with-line-size 6
              (om-draw-rect (om-h-scroll-position self) (om-v-scroll-position self) 
                            (- (om-point-h (om-view-size self)) 16) (- (om-point-v (om-view-size self)) 16)))))
        (when *om-tonalite* (draw-general-tonality self))
        (om-with-line-size 0.6
          (draw-view-contents self))
        (when *redraw-diamonds* nil
          (revise-references self)
          (setf *redraw-diamonds* nil))
        (draw-editors-in-editor self)
        (draw-time-selection self)
        ))))


(defmethod draw-panel-pages ((self scorePanel))
  (let* ((x0 (om-h-scroll-position self))
         (y0 (om-v-scroll-position self))
         (object (object (editor self)))
         (numpages (length (page-list (score-fdoc self))))
         (wpict (om-point-h (score-paper-size self)))
         (hpict  (om-point-v (score-paper-size self)))
         (factor (or (score-scale self) 1))
         (fwpict (round (* wpict factor)))
         (fhpict (round (* hpict factor)))
         (sepw 15)
         (seph 15)
         (h-pages (max 1 (floor (w self) (+ sepw sepw fwpict)))) 
         (v-pages (ceiling  numpages h-pages))
         (pict-count -1)
         (picts (score-picts-list self)))
  (let ((*internal-score-fonts* (init-fonts-to-draw (staff-size self))))
    (om-with-focused-view self
      (om-with-fg-color self *om-gray-color* 
        (om-fill-rect x0 y0 (w self) (h self) ))
      (om-draw-picture  self (nth 0 picts) 
                        :pos (om-make-point (+ sepw 0)  (+ seph 0)) 
                        :size (om-make-point fwpict fhpict))
      (loop for i from 0 to (- v-pages 1)  do
            (loop for k from 0 to (- h-pages 1) do
                  (when (< (+ k (* h-pages i)) numpages)
                    (om-draw-picture  self (nth (incf pict-count) picts) 
                                      :pos (om-make-point (+ sepw (* k (+ sepw fwpict))) 
                                                     (+ seph (* i (+ seph fhpict)))) 
                                      :size (om-make-point fwpict fhpict )))))
      (page-draw-selected-objects self (graphic-obj self))))))

(defmethod page-draw-selected-objects ((self scorePanel) grap-obj)
  (let* ((rectangles (get-rectangle-pages self))
         (factor (or (score-scale self) 1))
         (fdoc (score-fdoc self))
         pagenum rep)
    (loop for page in rectangles
          for pagenum = 0 then (+ pagenum 1)  do
          (let ((rect (car page))
                (rectlines (second page)))
          (loop for line from 0 to (- (howmany-lines fdoc pagenum) 1)
                for rectline in rectlines do
                (let* ((elements (collect-page-all-line-elements self grap-obj fdoc pagenum line)))
                  (loop for item in elements do
                        (page-draw-score-selection item (selection? self) rect factor)
                       )))))))

(defmethod draw-time-selection ((self scorepanel)) nil)


(defmethod get-mini-param ((self scorePanel) param)
  (cdr (assoc param (default-edition-params (make-instance 'note)))))

(defvar *extra-with-pairs-list* nil)
(defmethod draw-view-contents ((self scorePanel))
  (let* ((x0  (om-h-scroll-position self))
         (y0  (om-v-scroll-position self))
         (deltax (get-key-space self))
         (size (staff-size self))
         (deltay (round (* size (score-top-margin self)))))
    (setf *extra-with-pairs-list* nil)
    (om-with-focused-view self
      (om-with-font (get-font-to-draw 0)
                    (draw-system-only self)
                    (when (graphic-obj self)
                      (draw-object (graphic-obj self) self deltax 
                                   (- deltay (round (* (posy (car (staff-list (staff-sys self)))) (/ size 4))))
                                   (staff-zoom self) x0 (+ x0 (w self)) y0 (+ y0 (h self)) 
                                   (slots-mode self) size (linear? self) 
                                   (staff-sys self) nil (noteaschan? self))
                      (draw-score-selection (graphic-obj self) (selection? self) (staff-sys self) size)
                      (draw-edit-cursor self deltay))))))

(defmethod draw-system-only ((self scorePanel) &optional printinfo)
  (let* ((x0 (om-h-scroll-position self))
         (y0 (om-v-scroll-position self))
         (tempi (show-tempo self))
         (size (staff-size self))
         selected?)
    (setf selected? (member  (staff-sys self) (selection? self) :test 'equal))
    (unless (in-page-mode? self))
      (let ((posy (round (* size (score-top-margin self)))))
        (draw-one-system (staff-sys self) x0 (round (* size (score-top-margin self)))
                         (om-point-h (om-field-size self)) size tempi nil selected?)
        )))


;----------------ACTIONS

(defmethod get-approx-scale ((self scorePanel))  
  (or (and *om-tonalite* (get-scale-from-tonality (object (editor self))))
      (get-edit-param (om-view-container self) 'scale)
      (get-current-scale (staff-tone self))))

;;;;============================
;;; SCORE EDITOR / SCALE INTERFACE

(defmethod create-editor-scale ((self scoreeditor))
  ;(print (editor-params-edition self)))
  (let ((win (om-make-window 'om-dialog :window-title "Editor Scale"
                             :size (om-make-point 250 160) :resize nil))
        txt def)
    (om-add-subviews win (setf txt (om-make-dialog-item 'om-static-text (om-make-point 20 10) (om-make-point 200 100)
                                              (format nil "Current editor scale is:~%~%~A"
                                                      (cond ((and *om-tonalite* (get-scale-from-tonality (object self)))
                                                             (string+ "  " (tonalite-to-string (tonalite (object self))) " scale"))
                                                            ((get-edit-param self 'scale)
                                                             (let ((sca (get-edit-param self 'scale))
                                                                   (str ""))
                                                               (loop for line in (lines-list sca)
                                                                     for alt in (alteration-list sca) do
                                                                     (setf str (string+ str "(" (integer-to-string line) " " (string alt) ") ")))
                                                               str))
                                                            (t
                                                             (string+ "  Default 1/" (integer-to-string (staff-tone (panel self))) " tone scale"))))
                                              :font *controls-font*))
                     
                     (setf def (om-make-dialog-item  'om-button (om-make-point 60 110) (om-make-point 80 20)
                                           "Default"
                                           :di-action (om-dialog-item-act item
                                                        (set-edit-param self 'scale nil)
                                                        (om-set-dialog-item-text txt 
                                                                                 (format nil "Current editor scale is:~%~%~A"
                                                      (cond ((and *om-tonalite* (get-scale-from-tonality (object self)))
                                                             (string+ "  " (tonalite-to-string (tonalite (object self))) " scale"))
                                                            ((get-edit-param self 'scale)
                                                             (let ((sca (get-edit-param self 'scale))
                                                                   (str ""))
                                                               (loop for line in (lines-list sca)
                                                                     for alt in (alteration-list sca) do
                                                                     (setf str (string+ str "(" (integer-to-string line) " " (string alt) ") ")))
                                                               str))
                                                            (t
                                                             (string+ "  Default 1/" (integer-to-string (staff-tone (panel self))) " tone scale")))))
                                                        (update-panel (panel self))
                                                        (om-enable-dialog-item item nil)
                                                        )
                                           :enable (get-edit-param self 'scale)))
                     (om-make-dialog-item  'om-button (om-make-point 150 110) (om-make-point 80 20)
                                           "Edit..."
                                           :di-action (om-dialog-item-act item
                                                        (let ((scale (or (and *om-tonalite* (get-scale-from-tonality (object (editor self)))
                                                                              (omng-copy (get-scale-from-tonality (object (editor self)))))
                                                                         (get-edit-param self 'scale)
                                                                         (let ((sc (get-new-scale (staff-tone (panel self)))))
                                                                           (set-edit-param self 'scale sc)
                                                                           sc))))
                                                          (make-editor-window 'scaleeditor scale "SCALE EDITOR" self))
                                                        (om-enable-dialog-item def t)
                                                        (om-close-window win)
                                                        (update-panel (panel self)))
                                           :enable (not (and *om-tonalite* (tonalite (object self))))
                                           )
                     ;(om-make-dialog-item  'om-button (om-make-point 200 150) (om-make-point 80 20)
                     ;                      "OK"
                     ;                      :di-action (om-dialog-item-act item
                     ;                                   (om-return-from-modal-dialog win t)))
                     )
    (om-select-window win)))


(defmethod objectfromeditor ((self scorePanel)) 
   (object (editor self)))

(defvar *redraw-diamonds* nil)


(defmethod update-panel ((self scorePanel) &optional (updateref nil))
 (set-editor-tonality self)
 (let ((*internal-score-fonts* (init-fonts-to-draw (staff-size self)))
       (linespace (/ (staff-size self) 4)))
   (setf (graphic-obj self) 
         (if (score-page-mode self)
           (make-pages-form-obj self (objectfromeditor self) 0
                                120 linespace 
                                (staff-mode self)
                                (get-approx-scale self)
                                (selection? self) (staff-sys self) (show-stems self) )
           (make-graph-form-obj (objectfromeditor self) 0
                                120 linespace 
                                (staff-mode self)
                                (get-approx-scale self)
                                (selection? self) (staff-sys self) (show-stems self) )))
   (when (and (graphic-obj self) (not (score-page-mode self)))
     (space-objects (graphic-obj self) (* 4 linespace))
     (set-graph-rectangles (graphic-obj self))
     (cons-the-bpf-time self (graphic-obj self)))
   (when (and (editor self) updateref)
     (report-modifications (editor self)))
   (setf *redraw-diamonds* t)
   (om-invalidate-view self)))


;===========PORT CHANGE

(defmethod change-editor-outport ((self t) newport)
    (set-edit-param self 'outport  newport) t)

(defmethod change-editor-inport ((self t) newport)
    (set-edit-param self 'inport  newport) t)


(defmethod additional-port-menu ((self t) &key (pos (om-make-point 410 1)) (in t) (out t))
  (when in
    (om-add-subviews self
                     (om-make-dialog-item 'om-static-text pos (om-make-point 43 12) "InPort"
                                          :font *om-default-font1*)
                     (om-make-dialog-item 'numbox (om-add-points pos (om-make-point 50 -1)) (om-make-point 32 12)
                                          (format nil "~D" (get-edit-param (om-view-container self) 'inport))
                                          :di-action (om-dialog-item-act item
                                                                (change-editor-inport (editor (om-view-container (om-view-container item))) (value item)))
                                          :font *om-default-font1*
                                          :value (get-edit-param (om-view-container self) 'inport)
                                          :min-val 0
                                          :max-val 255))))


;------------ Redefinition of player

;(defmethod selection-to-play-? ((self scorePanel)) nil)

;(defmethod get-obj-to-play ((self scorePanel))
;   (list (object (editor self)) 
;         :port (get-edit-param (om-view-container self) 'outport)
;         :approx (get-edit-param (om-view-container self) 'approx)))

(defmethod additional-player-params ((self scoreeditor))
  (list :port (get-edit-param self 'outport) 
        :approx (get-edit-param self 'approx)))

(defmethod record2obj ((self scorePanel) list)
   (object (om-view-container self)))

(defmethod allow-record ((self scorePanel)) t)


(defmethod editor-stop-record ((self scoreeditor))
  (let ((data (call-next-method))
        (lastobject (object self))
        (newobject nil))
    (when data
      (setf newobject (record2obj (panel self) recordlist))
      (when newobject
        (setf (object self) newobject)
        (change-val-of-reference self newobject lastobject)
        (update-panel (panel self) t)))
    ))


;===================================================================

(defmethod note-chan-color ((self scorePanel))
   (setf (noteaschan? self) (not (noteaschan? self)))
   (set-edit-param (om-view-container self) 'notechancolor?  (noteaschan? self))
   (om-invalidate-view self t))

(defmethod change-system ((self scorePanel) newsys)
   (unless (equal newsys (staff-sys self))
     (setf (staff-sys self) (get-staff-system newsys))
     (set-edit-param (om-view-container self) 'staff newsys)
     (update-panel self t) ; t pour le sheet
     t))
    
(defmethod change-editor-size ((self scorePanel) newsize)
  (unless (equal newsize (staff-size self))
    (setf (staff-size self) newsize)
    (om-set-font self (om-make-music-font *heads-font* newsize))
    (set-edit-param (editor self) 'fontsize  newsize)
    (update-panel self) t))


(defvar *mus-page-factors* '(("10%" 0.1) ("25%" 0.25) ("50%" 0.5) ("75%" 0.75) ("100%" 1) ("125%" 1.25)
                            ("150%" 1.50) ("175%" 1.75) ("200%" 2) ("500%" 5)))

(defmethod change-editor-scale ((self scorePanel) newscale)
  (unless (equal newscale (score-scale self))
    (score-scale self newscale)
    (om-invalidate-view self) ))

(defmethod score-remove-subviews ((self scorePanel))
  (apply 'om-remove-subviews (cons self (om-subviews self))))

(defmethod change-score-mode ((self scorePanel) newmode)
  (unless (= newmode (score-mode self))
    (if (= 1 (score-mode self))
      (setf (score-action-boxes self) (cons-action-boxes-and-chords self))
      (score-remove-subviews self)) 
    (setf (score-mode self) newmode)
    (when (get-edit-param (om-view-container self) 'score-mode)
      (set-edit-param (om-view-container self) 'score-mode newmode))
    (case (score-mode self)
      (0 
       (set-score-page-mode self nil)
       (setf *redraw-diamonds* nil)
       (set-mini-ed-page (editor self) nil)
       (change-text (title-bar (editor self)) (string (class-name (class-of (object (editor self))))))
       (remove-panel-boxes self))
      (1 
       (set-score-page-mode  self nil)
       (change-text (title-bar (editor self)) "Patch Mode")
       (set-mini-ed-page (editor self) nil)
       (setf *redraw-diamonds* t)
       (set-panel-boxes self))
      (2 
       (change-text (title-bar (editor self)) "Page Mode")
       (set-score-page-mode self t)
       (setf *redraw-diamonds* nil)
       (set-mini-ed-page (editor self) t)
       (remove-panel-boxes self))
      (3 
       (change-text (title-bar (editor self)) "Segmentation/Analysis")
       (set-score-page-mode self nil)
       (setf *redraw-diamonds* nil)
       (set-mini-ed-page (editor self) nil)
       (remove-panel-boxes self))
      )
    (update-panel self)
    ))

(defmethod change-editor-mode ((self scorePanel) newmode)
  (unless (equal (staff-mode self) newmode)
    (setf (staff-mode self) newmode)
    (set-edit-param (om-view-container self) 'mode  newmode)
    (update-panel self) t))

(defmethod change-editor-tone ((self scorePanel) newtone)
  (unless (equal (staff-tone self) newtone)
    (setf (staff-tone self) newtone)
    (set-edit-param (om-view-container self) 'approx newtone)
    (update-panel self t) ;;; t pour le sheet
    t))

(defmethod show-hide-stems ((self scorePanel))
   (setf (show-stems self) (not (show-stems self)))
   (set-edit-param (om-view-container self) 'show-stems  (show-stems self))
   (update-panel self))

(defmethod change-editor-zoom ((self scorePanel) newzoom)
   (unless (= (staff-zoom self) newzoom)
     (setf (staff-zoom self) (/ newzoom 100))
     (set-edit-param (om-view-container self) 'zoom  (/ newzoom 100))
     (unless (score-page-mode self)
       (om-redraw-view self)
     )))

(defmethod change-editor-zoom-after ((self scorePanel) newzoom)
   (unless (= (staff-zoom self) newzoom)
     (setf (staff-zoom self) (/ newzoom 100))
     (set-edit-param (om-view-container self) 'zoom  (/ newzoom 100))
      (when (score-page-mode self)
        (update-panel self )
     )))

(defmethod change-obj-mode ((self scorePanel) val)
  (let* ((list (object-order (editor self)))
         (oldval (position (obj-mode self) list :test 'string-equal))
         (newval (mod (+ val (position (obj-mode self) list :test 'string-equal)) (length list))))
    
    (setf (obj-mode self) (nth newval list))
    (set-edit-param (om-view-container self) 'obj-mode newval)
    (make-unselect self)
    (update-mode-buttons (title-bar (om-view-container self)))
    (update-panel self)
    ))

   
(defmethod change-slot-edit ((self scorePanel) slotedit)
   (setf (slots-mode self) slotedit)
   (update-slot-edit self) t)

(defmethod first-note-in-selction ((self scorePanel))
   (let (rep)
     (loop for item in (selection? self)
           while (not rep) do
           (when (note-p item)
             (setf rep item)))
     rep))

(defmethod get-first-note ((self t)) nil)
(defmethod get-first-note ((self note)) self)
(defmethod get-first-note ((self container)) 
  (let ((fnote nil))
    (loop for elt in (inside self) 
          while (not fnote) do
          (setf fnote (get-first-note elt)))
    fnote))

;-------------

(defmethod set-dur-to-note ((self note) dur) 
   (set-extent-ms self dur))
(defmethod set-dur-to-note ((self container) dur) 
   (loop for item in (inside self) do (set-dur-to-note item dur)))

;-------------
(defmethod set-dyn-to-note ((self note) dyn) 
   (setf (vel self) dyn))
(defmethod set-dyn-to-note ((self container) dyn) 
   (loop for item in (inside self) do (set-dyn-to-note item dyn)))
(defmethod set-dyn-to-note ((self t) dyn) t)

;-------------
(defmethod set-chan-to-note ((self note) chan) 
   (setf (chan self) chan))
(defmethod set-chan-to-note ((self container) chan) 
   (loop for item in (inside self) do (set-chan-to-note item chan)))
(defmethod set-chan-to-note ((self t) chan) t)

;-------------
(defmethod set-offset-ms ((note note) (offset integer))
   (when (parent note)
     (SetQvalue (parent note) 1000)
     (setf (slot-value note 'offset) offset)
     (QNormalize (parent note))))
(defmethod set-offset-ms ((note t) (offset integer)) t)
(defmethod set-offset-ms ((self container) (offset integer))
   (loop for item in (inside self) do (set-offset-ms item offset)))
;-------------

(defmethod update-slot-edit ((self scorePanel))
  (unless (score-page-mode self)
    (let ((control (slotedit (ctr-view (om-view-container self))))
          (slotmode (slots-mode self))
          (firstnote nil))
      (loop for obj in (reverse (selection? self)) while (not firstnote) do
            (setq firstnote (get-first-note obj)))
      (enable-numbox control nil)
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
                          (set-dur-to-note item (value x)))
                    (update-panel self))))
        ((equal slotmode 'port)
         (setf (min-val control) 0)
         (setf (max-val control) 255)
         (set-value control (port firstnote))
         (setf (afterfun control) 
               #'(lambda (x)  
                   (loop for item in (selection? self) do
                         (set-port item (value x)))
                   (update-panel self))))
        
        ((equal slotmode 'offset)
         (setf (min-val control) -100000)
         (setf (max-val control) 100000)
         (set-value control (offset->ms firstnote))
         (setf (afterfun control) 
               #'(lambda (x)  
                   (loop for item in (selection? self) do
                         (set-offset-ms item (value x)))
                   (update-panel self))))
        ((equal slotmode 'dyn)
         (setf (min-val control) 0)
         (setf (max-val control) 127)
         (set-value control (vel firstnote))
         (setf (afterfun control)
               #'(lambda (x) 
                   (loop for item in (selection? self) do
                         (set-dyn-to-note item (value x)))
                   (update-panel self))))
        ((equal slotmode 'chan)
         (setf (min-val control) 1)
         (setf (max-val control) 16)
         (set-value control (chan firstnote))
         (setf (afterfun control)
               #'(lambda (x) 
                   (loop for item in (selection? self) do
                         (set-chan-to-note item (value x)))
                   (update-panel self))))
        ((equal slotmode 'midic)
         (setf (min-val control) 0)
         (setf (max-val control) 12700)
         (set-value control (midic firstnote))
         (setf (afterfun control) 
               #'(lambda (x) 
                   (loop for item in (selection? self) do
                         (when (note-p item)
                           (change-midic item (value x))))
                   (update-panel self))))
        ((equal slotmode 'chord-offset)
         (setf (min-val control) 0)
         (setf (max-val control) 1000000)
         (set-value control (offset->ms (object (om-view-container self))))
         (setf (afterfun control) 
               #'(lambda (x) 
                   (all-chords-2-ms (parent (object (om-view-container self))))
                   (setf (offset (object (om-view-container self))) (max 0 (value x))) 
                   (normalize-chords-x (parent (object (om-view-container self))))
                   (update-panel self t))))
        ((equal slotmode 'non)
         (setf (min-val control) 0)
         (setf (max-val control) 0)
         (setf (afterfun control) 
               #'(lambda (x) (declare (ignore x)) (om-beep)))))
             )
     (om-invalidate-view self t))))

;===================================================================
    
(defmethod save-editor ((self scorePanel))
   (save-as-midi (object (om-view-container self)) (om-choose-new-file-dialog :prompt "New MIDI file") :approx (staff-tone self)))

        
(defmethod edit-preferences ((self scorePanel))
  (om-beep-msg "No mode page for this editor"))

;=================KEY EVENTS

(defmethod set-unset-grille ((self scorePanel))
   (om-beep))

(defmethod edit-step-grille ((self scorePanel)) t)


;;;================ PLAY
(defvar *metric-strat-time* 0)
(defvar *events-play-cursor* nil)
(defvar *stop-time-play* nil)

(defmethod panel-show-cursor-p ((self scorepanel)) t)

(defmethod mixed-collect-cursor-objects ((self t))
  (if (score-page-mode self)
      (remove-duplicates (sort (page-collect-temporal-objects self (graphic-obj self)) '< :key 'car ) :test 'equal :key 'car)
    (remove-duplicates (get-temporal-objects (graphic-obj self)) :test 'equal :key 'car)))



(defmethod page-collect-temporal-objects ((self scorePanel) grap-obj)
  (let* ((rectangles (get-rectangle-pages self))
         (factor (or (score-scale self) 1))
         (fdoc (score-fdoc self))
         pagenum rep)
    (loop for page in rectangles
          for pagenum = 0 then (+ pagenum 1)  do
          (let ((rect (car page))
                (rectlines (second page)))
          (loop for line from 0 to (- (howmany-lines fdoc pagenum) 1)
                for rectline in rectlines do
                (let* ((elements (collect-page-all-line-elements self grap-obj fdoc pagenum line)))
                  (loop for item in elements do
                       (push (loop for item in (apply 'collect-temporal-objects (list item (reference grap-obj)))
                                   collect (append item (list pagenum rect line rectline))) rep))))))
     (flat (reverse rep) 1)))


(defmethod get-x-range ((self scorepanel)) 
  (if (score-page-mode self)
      (list 0 (get-obj-dur (object (editor self))))
    (call-next-method)))


(defmethod update-cursor ((self scorepanel) time &optional y1 y2)
  (if (score-page-mode self)
      (let ((currevent (find time *events-play-cursor* :key 'car :test '>= :from-end t)))
        (when currevent
          (let* ((cur-evt (second currevent))
                 (rect-line (sixth currevent))
                 (rect-page (fourth currevent))
                 (rect-event (rectlist-page-to-pixel rect-page (or (score-scale self) 1) (rectangle cur-evt))))
            (om-update-movable-cursor self (car rect-event) (- (om-rect-top rect-line) (round (staff-size self) 2))  
                                      4 (- (om-rect-bottom rect-line) (om-rect-top rect-line)))
            )))
    (call-next-method)))

;==========================================================================
; SPECIFIC EDITORS
;==========================================================================

;==================================
;NOTE
;==================================

;VIEW
(defclass noteEditor (scoreEditor) ())
(defmethod get-score-class-panel ((self noteEditor)) 'notePanel)
(defmethod get-score-class-ctrls ((self noteEditor)) 'omnote-controls-view)


;CONTROLS
(defclass omnote-controls-view (omcontrols-view) ())

(defmethod GET-slot-LIST ((self omnote-controls-view)) 
   '(("midic" midic) ("channel" chan) ("dur" dur) ("dyn" dyn) ("port" port)))


;PANEL
(defclass notePanel (scorePanel) ())

(defmethod get-key-space ((self notePanel))
   (* (get-deltax (staff-sys self)) (staff-size self) 2))

(defmethod allow-record ((self notePanel)) nil)


(defmethod get-help-list ((self notepanel)) 
  (list '((ud "Transpose")
          (shift+ud "Transpose Octave")
          (lr "Change Duration")
          (("C") "Change Color")
          (("c") "Show Channel Color")
          (("s") "Set Editor Scale")
          (space "Play/Stop"))))

(defmethod panel-show-cursor-p ((self notepanel)) nil)

(defmethod get-menubar ((self noteeditor)) 
  (list (om-make-menu "File"
                      (list 
                       (list 
                        (om-new-leafmenu "Close" #'(lambda () (om-close-window (om-view-window self))) "w"))
                       (list 
                        (import-menu self)
                        (export-menu self)
                        )
                       (list 
                        (om-new-leafmenu "Page Setup" #'(lambda () (om-page-setup)))
                        (om-new-leafmenu "Print" #'(lambda () (om-print-window (om-view-window self))) "p"))
                       ))
        (make-om-menu 'edit :disable '("Duplicate" "Clear" "Cut") :editor self)      
        (make-om-menu 'windows :disable nil :editor self)
        (make-om-menu 'help :editor self)))

;==================================
;CHORD
;==================================


(defclass chordEditor (scoreEditor) ())
(defmethod get-score-class-panel ((self chordEditor)) 'chordPanel)
(defmethod get-score-class-ctrls ((self chordEditor)) 'omchord-controls-view)

(defclass omchord-controls-view (omcontrols-view) ())

(defmethod initialize-instance :after ((self omchord-controls-view) &rest l &key (mode 0))
  (declare (ignore l))
  (add-chord-control self mode))

(defmethod get-editor-callback ((self chordeditor)) nil)


(defmethod GET-slot-LIST ((self omchord-controls-view))
   '(("midic" midic) ("channel" chan) ("dur" dur) ("dyn" dyn) ("port" port) ("offsets" offset)))


;=============================================
;=============================================

(defclass chordPanel (scorePanel) ())


;--------------ACTIONS

(defmethod get-key-space ((self chordPanel))
   (* (get-deltax (staff-sys self)) (staff-size self) 2))


(defmethod find-indice-new-note ((self chordPanel) x)
   (let ((realpix (- x (get-key-space self))))
     (if (<= realpix 0) 0
         (ceiling (- x (get-key-space self)) (* (staff-zoom self)  2 (staff-size self))))))                        



    
;----------------------PLAY

(defclass arp-chord ()
  ((notes :initform nil :initarg :notes :accessor notes)))

(add-player-for-object 'arp-chord '(:midi-player :midishare :osc-scoreplayer :microplayer))

(defmethod extent ((self arp-chord))
   (* (length (notes self)) 500))

(defmethod get-obj-dur ((self arp-chord)) (extent self))

(defmethod chord-obj-to-play ((self chord) mode)
  (if (find mode '(1 2 3) :test '=)     
     (let ((notes (copy-list (inside self))))
       (case mode
         (1 (make-instance 'arp-chord :notes (sort notes '< :key 'midic)))
         (2 (make-instance 'arp-chord :notes (sort notes '> :key 'midic)))
         (3 (make-instance 'arp-chord :notes notes)))
     ;(list objtoplay 
     ;      :port (get-edit-param self 'outport)
     ;      :approx (get-edit-param self 'approx))
       )
    self))

(defmethod play-obj-from-value ((value chord) (box omboxeditcall)) 
  (chord-obj-to-play value (get-edit-param box 'mode)))

(defmethod get-obj-to-play ((self chordeditor))
  (chord-obj-to-play (object self) (staff-mode (panel self))))

(defmethod record2obj ((self chordPanel) list)
  (let ((chord (object (editor self))))
    (setf (Lmidic chord) (loop for item in list
                               collect (* 100 (first item))))
    (setf (Lvel chord) (loop for item in list
                             collect (fourth item)))
    (setf (Ldur chord) (loop for item in list
                             collect (third item)))
    (setf (Lchan chord) (loop for item in list
                              collect (fifth item)))
    ))
     
(defun record2chord (list)
   (make-instance 'chord
     :LMidic  (loop for item in list
                    collect (* 100 (first item)))
     :LVel (loop for item in list
                 collect (fourth item))
     :Ldur (loop for item in list
                 collect (third item))
     :Lchan (loop for item in list
                  collect (fifth item))))


(defmethod panel-show-cursor-p ((self chordpanel)) t)


(defmethod get-help-list ((self chordpanel)) 
  (list '((alt+clic "Add Note")
          (del "Delete Selection")
          (tab "Change Obj. Mode")
          (ud "Transpose Selection")
          (shift+ud "Transpose Octave")
          (lr "Change Durations")
          (("C") "Change Color")
          (("c") "Show Channel Color")
          (("s") "Set Editor Scale")
          (("t" "T") "Set/Remove Tonality")
          (space "Play/Stop"))))

;==================================
;chordseq
;==================================


;CONTROLS
(defclass chordseq-controls-view (omcontrols-view) ())

(defmethod initialize-instance :after ((self chordseq-controls-view) &rest l &key (mode 0))
  (declare (ignore l))
  (add-chordseq-control self mode))



;VIEW
(defclass chordseqEditor (scoreEditor) ())


(defmethod editor-null-event-handler :after ((self chordseqEditor))
     (do-editor-null-event self))

(defmethod init-draw ((self chordseqEditor))
   (setf (grille-step (panel self)) (get-edit-param self 'grillestep))
   (update-panel (panel self)))

(defmethod get-score-class-ctrls ((self chordseqEditor)) 'chordseq-controls-view)
(defmethod get-score-class-panel ((self chordseqEditor)) 'chordseqPanel)

(defmethod do-editor-null-event ((self chordseqEditor)) 
  (when (om-view-contains-point-p (panel self) (om-mouse-position self))
    (show-position-ms self (pixel-toms (panel self) (om-mouse-position (panel self))))))

(defmethod show-position-ms ((self chordseqEditor) point)
   (when (and point (not (minusp point)))
     (om-with-focused-view (panel self) 
                           (let ((x (om-h-scroll-position (panel self)))
                                 (y (om-v-scroll-position (panel self))))
                             (om-with-font (om-make-font *om-score-font-face* (nth 1 *om-def-font-sizes*)) ; :style '(:bold))
                                           (om-with-fg-color (panel self) (om-get-bg-color (panel self))
                                             (om-fill-rect (+ x 6) (+ y (- (h (panel self)) 42)) 70 15))
                              
                                           (om-draw-string (+ x 8) (+ y (- (h (panel self)) 30)) (format () "t: ~D ms" point))
)))))


;PANEL

(defclass chordseqPanel (scorePanel)  
   ((grille-p :initform nil :accessor grille-p)
    (grille-step :initform 1000 :accessor grille-step)
    (clic-pos :initform nil :accessor clic-pos)
    ))


(defmethod draw-time-selection ((self chordseqpanel))
  (unless (or (system? (car (selection? self))) (extra-p (car (selection? self))))
    (om-with-focused-view self 
      (let ((x (+ (om-h-scroll-position self) 
                  (w self) -150))
            (y (+ (om-v-scroll-position self) 
                  (h self) -32))
            (b 0) 
            (e (get-obj-dur (object (editor self)))))
        (if (and (linear? self) (cursor-p self) (cursor-interval self) (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
            (setf b (car (cursor-interval self))
                  e (cadr (cursor-interval self)))
        ;nil
          (when (selection? self) 
            (setf b e) (setf e 0)
            (loop for item in (selection? self) do
                  (unless (or (system? item) (extra-p item))
                    (when (< (offset->ms item (object (editor self))) b) (setf b (offset->ms item (object (editor self)))))
                    (when (> (+ (offset->ms item (object (editor self))) (get-obj-dur item)) e) (setf e (+ (offset->ms item (object (editor self))) (get-obj-dur item))))
                    ))) ) 
        (om-with-font (om-make-font *om-score-font-face* (nth 1 *om-def-font-sizes*))
                      (om-draw-string x (+ y 2) 
                                      (if (selection? self)
                                          (str-check (string+ (om-str :selection) ": " (format () "~D - ~D ms" b e)))
                                        (str-check (string+ (om-str :duration) ": " (format () "~D ms" e)))))
                      )))))

(defvar *interpage-pixels* 10)

(defmethod grille-step-p ((self chordseqPanel))
   (if (grille-p self) (grille-step self)))

(defmethod show-tempo ((self scorePanel)) nil)


(defmethod draw-view-contents ((self chordseqPanel))
  (let* ((x0  (om-h-scroll-position self))
         (y0  (om-v-scroll-position self))
         (size (staff-size self))
         (deltax (round (get-key-space self)))
         (deltay (round (* size (score-top-margin self)))))
    (when (and (linear? self) (cursor-p self))
      (draw-interval-cursor self))
    (om-with-focused-view self 
      (om-with-font (get-font-to-draw 0) ;(om-make-font *heads-font* size)
                    (draw-system-only self)
                    (when (graphic-obj self)
                      (om-with-clip-rect self (om-make-rect (+ x0 (- deltax (round size 2))) y0  (+ x0 (w self)) (+ y0 (h self)))
                        (scorepanel-draw-object self x0 y0 deltax deltay size)
                        (if (analysis-mode? self) (draw-analysis self))
                        (draw-score-selection (graphic-obj self) (selection? self) (staff-sys self) size)
                        (draw-edit-cursor self deltay)
                        )
                      )))
      ))

(defmethod scorepanel-draw-object ((self chordseqPanel) x0 y0 deltax deltay size)
  (draw-object (graphic-obj self) self deltax 
                                     (- deltay (round (* (posy (car (staff-list (staff-sys self)))) (/ size 4)))) 
                                     (staff-zoom self) x0 (+ x0 (w self)) y0 (+ y0 (h self))
                                     (slots-mode self) size (linear? self) (staff-sys self) (grille-step-p self) (noteaschan? self)))


(defmethod edit-preferences ((self chordseqPanel))
   (setf (linear? self) (not (linear? self)))
   (update-panel self))


(defmethod control-actives ((self chordseqPanel) where)
  (if (and (linear? self) (cursor-p self))
    (new-interval-cursor self where)
    (call-next-method)))


(defmethod pixel-toms ((self scorepanel) pixel)
  (if (linear? self)
    (values (pixel2ms (- (om-point-h pixel) (get-key-space self) ) 
                      (/ (staff-size self) 4) (staff-zoom self)) 
            0)))



(defmethod scroll-play-window ((self chordseqPanel))
   (let ((delta (get-key-space self)))
     (om-set-scroll-position self (om-make-point 
                                   (- (+ (om-h-scroll-position self) (w self)) delta) 
                                   (om-v-scroll-position self)
                                   ))))

(defmethod record2obj ((self chordseqPanel) list)
   (let* ((editor (om-view-container self))
          (obj  (object editor)))
     (when list
       (close-attached-editors editor)
       (let* ((chords (sort (make-quanti-chords list *global-deltachords*) '< :key 'offset))
              (first (offset (car chords))))
         (loop for item in chords do
               (setf (offset item) (- (offset item) first))
               (setf (parent item) obj))
         (setQValue obj 1000 :recursive nil)
         (setf (inside obj) nil)
         (setf (inside obj)  chords)
         (adjust-extent obj)
         (QNormalize obj)))
     obj))



(defmethod all-chords-2-ms ((self chord-seq))
   (loop for item in (chords self) do
         (setf (offset item) (offset->ms item))))

(defmethod all-chords-2-ms ((self multi-seq))
   (loop for item in (inside self) do
         (all-chords-2-ms item)))


(defmethod get-graph-select-chords ((self chordseqPanel))
   (remove-duplicates (loop for item in (get-graph-selection? self (grap-class-from-type (obj-mode self))) 
                            collect  item) :test 'equal))

;a faire
(defmethod adjoust-grille-chords ((self chordseqPanel))
   (when (grille-p self)
     (let ((obj (object (om-view-container self)))
           (chords (get-graph-select-chords self)))
       (loop for item in chords do
             (let ((realchord (reference item)))
               (when (chord-p realchord)
                 (multiple-value-bind (ntimes nrest) (floor (offset->ms realchord) (grille-step self))
                   ;(print (list (offset->ms realchord) (grille-step self) ntimes nrest (round (grille-step self) 2)))
                   (set-chords-offset self realchord (*  (grille-step self)
                                                         (if (> nrest (round (grille-step self) 2)) (+ 1 ntimes) ntimes)))
                   ;(if (> nrest (round (grille-step self) 2))
                   ;    (setf (offset realchord) (* (+ 1 ntimes) (grille-step self) ))
                   ;  (setf (offset realchord) (* ntimes (grille-step self) )))
                   ))))
       (update-panel self t))))



(defmethod adjoust-grille-durs ((self chordseqPanel))
   (when (grille-p self)
     (let ((obj (object (om-view-container self)))
           (chords (get-graph-select-chords self)))

       (loop for item in chords do
             (let ((realchord (reference item)))
               (cond 
                ((chord-p realchord) 
                 (let ((offs (offset->ms realchord)))
                   (loop for n in (inside realchord) do
                         (multiple-value-bind (ntimes nrest) (floor (+ (dur n) offs) (grille-step self))
                           (if (> nrest (round (grille-step self) 2))
                             (setf (dur n) (- (* (+ 1 ntimes) (grille-step self)) offs))
                             (setf (dur n) (- (* ntimes (grille-step self)) offs))))
                         )
                   ))
                ((note-p realchord)
                 (multiple-value-bind (ntimes nrest)  (floor (+ (dur realchord) (offset->ms (parent realchord))) (grille-step self))
                   (if (> nrest (round (grille-step self) 2))
                     (setf (dur realchord) (-  (* (+ 1 ntimes) (grille-step self)) (offset->ms (parent realchord))))
                     (setf (dur realchord) (-  (* ntimes (grille-step self)) (offset->ms (parent realchord)))))
                   ))
                 
                (t nil))
               ))
       (update-panel self t)
       )))

(defmethod normalize-chords-x ((self chord-seq) )
   (setf (inside self) (sort (inside self) '< :key 'offset))
   (setf (QValue self) 1000)
   (adjust-extent self)
   (QNormalize self))

(defmethod normalize-chords-x ((self multi-seq) )
   (loop for item in (inside self) do
         (normalize-chords-x item)))

(defmethod translate-chords-p ((self scorePanel)) nil)
(defmethod translate-chords-p ((self chordseqPanel)) t)

(defmethod change-chords-in-x (chordseqPanel chords delta)
   (loop for item in chords do
         (setf (offset item) (max 0 (+ (offset item) delta))) ))

(defmethod move-chords-in-x ((self chordseqPanel) first-mouse new-mouse chords zero?)
   (let ((chordseq (object (om-view-container self)))
         (delta (pixel2ms  (- (om-point-h first-mouse) (om-point-h new-mouse)) 
                           (/ (staff-size self) 4) (staff-zoom self))))
     (unless (or zero? (>= (- (offset (car chords)) delta) 0))
       (setf delta (offset (car chords))))
     (all-chords-2-ms chordseq)
     (change-chords-in-x self chords (- delta)) 
     (normalize-chords-x chordseq)
     (update-panel self t) t))


(defmethod set-chords-offset ((self chordseqpanel) objects newoffset)
  (when (translate-chords-p self)
    (all-chords-2-ms (object (om-view-container self)))
    (loop for chord in (list! objects) do
          (setf (offset chord) newoffset))
    (normalize-chords-x (object (om-view-container self)))
    (update-panel self t)))


(defmethod set-unset-grille ((self chordseqPanel))
   (setf (grille-p self) (not (grille-p self)))
   (om-invalidate-view self t))

(defmethod edit-step-grille ((self chordseqPanel)) 
  (when (grille-p self) 
    (let ((Mydilog (om-make-window 'om-dialog
                                   :size (om-make-point 190 80)
                                   :window-title "Grille step"
                                   :resizable nil :maximize nil :minimize nil
                                   :font *om-default-font1*
                                   :window-show nil
                                   :position :centered))
          (step (om-make-dialog-item 'om-editable-text (om-make-point 20 26) (om-make-point 50 12)
                                     (format () (format nil "~D" (grille-step self)))
                                     :font *om-default-font1*
                                     :focus nil)))
      (om-add-subviews mydilog 
                       (om-make-dialog-item 'om-static-text (om-make-point 15 5) (om-make-point 80 18) "Step [ms]"           
                                            :font *om-default-font2*
                                            :bg-color *om-window-def-color*)
                       
                       (om-make-dialog-item 'om-button (om-make-point 100 8) (om-make-point 80 20) "Cancel"
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item)) (om-return-from-modal-dialog mydilog ())))
                       (om-make-dialog-item 'om-button (om-make-point 100 36) (om-make-point 80 20) "OK"
                                            :di-action (om-dialog-item-act item
                                                         (declare (ignore item))
                                                         (when (not (equal "" (om-dialog-item-text step))) 
                                                           (let ((rep (read-from-string (om-dialog-item-text step))))
                                                             (if (and (integerp rep) (> rep 0))
                                                               (progn
                                                                 (setf (grille-step self) rep)
                                                                 (set-edit-param (om-view-container self) 'grillestep  rep)
                                                                 (om-invalidate-view self t))
                                                               (om-beep-msg (string+ "Bad step value ! (" (om-dialog-item-text step) ")" )))))
                                                         (om-return-from-modal-dialog mydilog ()))
                                            ;:focus t
                                            :default-button t
                                            )
                       step)
      (om-modal-dialog mydilog))))


(defmethod handle-key-event ((self chordseqPanel) char)
  (if (analysis-mode? self)
      (analysis-handle-key-event self char)
  (case char
    (#\g (set-unset-grille self))
    (#\G (edit-step-grille self))
    (#\a (if (equal (slots-mode self) 'dur)
           (adjoust-grille-durs self)
           (adjoust-grille-chords self)))
    (#\z (set-cursor-mode (editor self)))
    (otherwise (call-next-method)))))

;(defmethod selection-to-play-? ((self chordseqPanel)) 
;   (and (linear? self) (cursor-p self)))

;(defmethod convert-interval ((self chordseqPanel))
;   (let* ((obj (car (get-obj-to-play self)))
;         (dur (get-obj-dur obj))
;         (int (cursor-interval self))  x x1 rep)
;     (if (listp int)
;       (setf x (max 0 
;                    ;;;(om-point-h (pixel-toms self (om-make-point (car int) 0)))
;                    (pixel-toms self (om-make-point (car int) 0))
;                    )
;             x1 (min dur 
;                     ;;;(om-point-h (pixel-toms self (om-make-point (second int) 0)))
;                     (pixel-toms self (om-make-point (second int) 0))
;                     ))
;       (setf x (max 0 
;                    ;;;(om-point-h (pixel-toms self (om-make-point int 0)))
;                    (pixel-toms self (om-make-point int 0))
;                    ) 
;             x1 (extent->ms obj)))
;     (setf (cursor-pos self) x)
;     (setf rep (if (<= x x1)
;                 (list x x1)
;                 (list x1 x)))
;     (if (< (car rep) dur)
;       rep
;       '(0 0))))



;(defmethod get-selection-to-play ((self chordseqPanel))
;  (let ((obj (car (get-obj-to-play self)))
;        (interval (get-play-interval self)))
;    (values  (list obj
;                   :interval interval
;                   :approx (get-edit-param (om-view-container self) 'approx)
;                   :port (get-edit-param (om-view-container self) 'outport))
;             (first interval) (second interval))))


(defun interchange-chords (oldchord newobject)
  (setf (Lmidic oldchord) (Lmidic newobject)
        (LVel oldchord) (LVel newobject)
        (LOffset oldchord) (LOffset newobject)
        (LChan oldchord) (LChan newobject))
  (when (cont-chord-p oldchord)
    (tie-chord oldchord (state oldchord))))


(defmethod change-in-int-editor ((self chordseqPanel) (internal scorePanel) (newobject chord) oldchord)
   (unless (equal oldchord newobject)
     (interchange-chords oldchord newobject) 
     (setf (object (om-view-container internal)) oldchord))
   (change-ties-too  self oldchord))


(defmethod change-ties-too ((self chordseqPanel) notes)
   (update-panel self t))

  
(defmethod set-name-to-mus-obj ((self chordseqpanel))
  (let ((name (om-get-user-string "Voice name:" :initial-string (or (get-name (object (editor self))) ""))))
    (when name
      (set-name (object (editor self)) name)
      (om-invalidate-view self t))))


(defmethod get-help-list ((self chordseqpanel)) 
  (list '((alt+clic "Add Chord/Note")
          (del "Delete Selection")
          (tab "Change Obj. Mode")
          (("z") "Obj/Time Selection")
          (ud "Transpose Selection")
          (shift+ud "Transpose Octave")
          (lr "Change Offsets/Dur.")
          (("*") "Group Chords")
          (("+") "Union Chords (Group + Offset)")
          (("c") "Show Channel Color")
          (space "Play/Stop"))
        '((("g") "Show/Hide Grid")
          (("G") "Edit Grid Step")
          (("a") "Adjust Chords/Durs to Grid")
          (("C") "Change Color")
          (("s") "Set Editor Scale")
          (("t" "T") "Set/Remove Tonality")
          (("n") "Set Voice Name")
          (("o") "Open Internal Chord Editor")
          )))

;==========================================================
; multi-seq
;==========================================================


(omg-defclass multiseq-controls-view (chordseq-controls-view) ())

;---------------------
(omg-defclass multiseqEditor (chordseqEditor) ())
(defmethod get-score-class-panel ((self multiseqEditor)) 'multiseqPanel)
(defmethod get-score-class-ctrls ((self multiseqEditor)) 'multiseq-controls-view)


(defmethod update-editor-after-eval ((self multiseqEditor) val)
   (let ((newstaff (correct-staff-val  val (staff-sys (panel self)) (panel self))))
     (setf (object self) val)
     (setf (staff-sys (panel self)) newstaff)
     (set-edit-param self 'staff  (loop for item in newstaff collect (sysname item)))
     (init-music-patch  (panel self))
     (remove-panel-boxes (panel self))
     (update-panel (panel self))))

;---------------------
   
(omg-defclass multiseqPanel (chordseqPanel) ())

(defmethod correct-staff ((self multiseqpanel) staff)
  (setf staff (correct-staff-val (object (editor self)) staff self))
  (set-edit-param (editor self) 'staff  staff)
  staff)

(defmethod initialize-instance :after ((self multiseqPanel) &rest initargs)
  (declare (ignore initargs))
  ;(loop for object in (inside (object (om-view-container self)))
  ;      for i = 1 then (+ i 1)
  ;      do (unless (name object) (setf (name object) (format nil "Staff~D" i))))
  )


(defmethod get-key-space ((self multiseqPanel))
  (* (get-deltax (car (list! (staff-sys self)))) (staff-size self)))


(defmethod omselect-with-shift ((self scorePanel) graph-obj)
  (when (system? (car (selection? self)))
    (setf (selection? self) nil))
   (if (member (reference graph-obj) (selection? self) :test 'equal)
     (progn
       (setf (selection? self) (remove  (reference graph-obj) (selection? self) :test 'equal))
       (setf (selected graph-obj) nil))
     (progn
       (push (reference graph-obj) (selection? self))
       (setf (selected graph-obj) t))))


(defmethod click-in-which-voice? ((self multiseqPanel) where)
   (when (linear? self)
     (let* ((size (staff-size self))
            (up (round (* size (score-top-margin self))))
            (x0  (om-h-scroll-position self))
            (numvoices (length (inside (graphic-obj self)))) 
            (posy 0)
            (point-y (om-point-v where))
            (interlinea (round (* size (score-line-space self)))) rep)
       (cond
        ((< point-y (+ up (line2pixel (posy (car (staff-list (nth 0 (staff-sys self))))) (top-in-midi (nth 0 (staff-sys self)))
                                      (/ size 4))))
         (setf rep 0))
        (t (om-with-focused-view self
                                 (om-with-font (om-get-font self)
                                               (setf where (om-mouse-position self))
                                               (loop for i from 0 to (- numvoices 1) do
                                                     (let* ((curstaff (nth i (staff-sys self)))
                                                            (y0 (+ up  posy (line2pixel  (posy (car (staff-list curstaff))) (top-in-midi curstaff)
                                                                                        (/ size 4))))
                                                            (ysize (system-size-in-pix  curstaff size)) 
                                                            (r (om-make-rect x0 y0
                                                                              (+ x0 (w self)) (+ y0 ysize ))))
                                                             (when (om-point-in-rect-p where r)
                                                               (setf rep i))
                                                       (setf posy (+ posy ysize interlinea ))))))
                                 (unless rep (setf rep (- numvoices 1)))))
        rep)))

(defmethod click-in-which-voice? ((self multiseqPanel) where)
  (when (linear? self)
    (let* ((system (staff-sys self))
           (y (* (staff-size self) (score-top-margin self)))
           (width (w self))
           (size (staff-size self))
           (x  (om-h-scroll-position self))
           (posy y) rep)
      (loop for sys in  (list! system)
            for i = 0 then (+ i 1)
            while (not rep) do
            (when (point-in-rectangle-p where posy x  (+ posy (get-delta-system sys size self i)) (+ x width))
              (setf rep i))
            (setf posy (+ posy (get-delta-system sys size self i)))
            )
      (unless rep (setf rep (- (length (list! system)) 1)))
      rep)))


(defmethod change-system ((self multiseqPanel) newsys)
   (if (selection? self)
     (loop for item in (selection? self) do
           (change-select-system self item newsys))
     (change-select-system self (object (om-view-container self)) newsys))
   (update-panel self))

;;; obj = omsystem
(defmethod change-select-system ((self multiseqPanel) (obj t) newsys)
  (let ((oldpar (get-edit-param (om-view-container self) 'staff))
        (pos (position obj (staff-sys self) :test 'equal)))
    (when pos
      (setf (nth pos (staff-sys self)) (get-staff-system newsys))
      (setf (nth pos oldpar) newsys)
      (set-edit-param (om-view-container self) 'staff oldpar))))
   
(defmethod change-select-system ((self multiseqPanel) (obj simple-container) newsys)
  (let ((oldpar (get-edit-param (om-view-container self) 'staff)))
    (loop for pos from 0 to (- (length (staff-sys self)) 1) do
          (setf (nth pos (staff-sys self)) (get-staff-system newsys))
          (setf (nth pos oldpar) newsys))
    (set-edit-param (om-view-container self) 'staff oldpar)))
   


(defmethod get-staff-selection ((self multiseqPanel))
  (loop for item in (selection? self) collect
        (position item (staff-sys self) :test 'equal)))

(defmethod draw-system-only ((self multiseqPanel) &optional printinfo)
  (let* ((x0 (om-h-scroll-position self))
         (y0 (om-v-scroll-position self))
         (size (staff-size self))
         (tempi (show-tempo self))
         poslist)
    (setf poslist (get-staff-selection self)) 
    (if (linear? self)
        (draw-score-line (staff-sys self) self x0  (round (* size (score-top-margin self))) (w self) size tempi nil poslist)
      )))


(defmethod om-view-cursor ((self chordseqPanel))
  (let ((where (om-mouse-position self)))
    (cond
     ((om-add-key-p) *add-cursor*)
     (t (if (and (linear? self) (cursor-p self))
          *om-i-beam-cursor* 
          (cursor-by-obj-mode self))))))

(defmethod scorepanel-draw-object ((self multiseqpanel) x0 y0 deltax deltay size)
  (draw-object  (graphic-obj self) self deltax deltay
                (staff-zoom self) x0 (+ x0 (w self)) y0 (+ y0 (h self)) 
                (slots-mode self) size (linear? self) (staff-sys self) 
                (grille-step-p self) (noteaschan? self)))


(defmethod record2obj ((self multiseqPanel) list)
   (let* ((editor (om-view-container self))
          (obj  (object editor)))
     (when list
       (close-attached-editors editor)
       (let ((track-list (make-list 16 :initial-element nil))
             (min 10000000) rep)
         (loop for note in list do
               (push note (nth (fifth note) track-list)))
         (setf track-list (remove nil track-list))
         (loop for item in track-list do
               ;(setf item (sort item '< :key 'second))
               (let* ((newlist (reverse item))
                      (newcs (make-instance 'chord-seq))
                      (chords (sort (make-quanti-chords newlist *global-deltachords*) '< :key 'offset))
                      first )
                 (setQValue newcs 1000 :recursive nil)
                 (setf (inside newcs) nil)
                 (setf (inside newcs)  chords)
                 (adjust-extent newcs)
                 (QNormalize newcs)
                 (push newcs rep)
                 (setf first (offset->ms (car chords)))
                 (setf min (min min first))))
         (loop for item in rep do
               (loop for ch in (chords item) do
                     (setf (offset ch) (- (offset ch) min))
                     (setf (parent ch) item)))
         (change-multi-inside self (reverse rep))))
     obj))

(defmethod change-multi-inside ((self multiseqPanel) newobjes)
   (let* ((editor (om-view-container self))
          (obj (object editor)) staff)
     (setf (inside obj) nil)
     (initialize-instance obj :chord-seqs newobjes)
     (setf staff (correct-staff-val obj (get-edit-param editor 'staff) self))
     (set-edit-param editor 'staff  staff)
     (setf (staff-sys self) (get-staff-system staff))
     obj))


(defmethod set-name-to-mus-obj ((self multiseqpanel))
   (if (and (selection? self)
            (= 1 (length (selection? self)))
            (subtypep (type-of (car (selection? self))) (find-class 'sequence*))
            (member (obj-mode self) '("voice" "chord-seq") :test 'string-equal))
     (let ((name (om-get-user-string "Voice name:" :initial-string (or (get-name (car (selection? self))) ""))))
       (when name
         (loop for item in (selection? self) do
               (set-name item name)))
       (om-invalidate-view self t))
     (om-beep)))


(defmethod get-help-list ((self multiseqpanel)) 
  (list '((alt+clic "Add Voice")
          (del "Delete Selection")
          (tab "Change Obj. Mode")
          (("z") "Obj/Time Selection")
          (ud "Transpose Selection")
          (shift+ud "Transpose Octave")
          (lr "Change Offsets/Dur.")
          (("+") "Union Voices")
          (("c") "Show Channel Color")
          (space "Play/Stop"))
        '((("g") "Show/Hide Grid")
          (("G") "Edit Grid Step")
          (("a") "Adjust Chords/Durs to Grid")
          (("C") "Change Color")
          (("s") "Set Editor Scale")
          (("t" "T") "Set/Remove Tonality")
          (("n") "Set Selected Voice Name")
          (("o") "Open Internal Editor")
          )))


;==========================================================
; VOICE
;==========================================================



;CONTROLS
(omg-defclass voice-controls-view (chordseq-controls-view) ())

(defmethod GET-slot-LIST ((self voice-controls-view)) 
   '(("midic" midic) ("channel" chan) ("dyn" dyn) ("port" port)))


;VIEW
(omg-defclass voiceEditor (chordseqEditor) ())

(defmethod get-score-class-ctrls ((self voiceEditor)) 'voice-controls-view)
(defmethod get-score-class-panel ((self voiceEditor)) 'voicepanel)

(defmethod show-position-ms ((self voiceEditor) point) t)

                     


;PANEL
(defclass rythmicpanel () 
   ((emptymode :initform nil :initarg :emptymode :accessor emptymode)
    (rhyt-object :initform nil :initarg :rhyt-object :accessor rhyt-object)))


(defmethod objectfromeditor ((self rythmicpanel))
   (if (emptymode self) 
     (or (rhyt-object self)
         (new-ryth-object self (object (om-view-container self)))) (call-next-method)))

(defmethod new-ryth-object ((self rythmicpanel) (voice voice))
   (let ((new-voice (clone voice)))
     (setf (chords new-voice) (list (make-instance 'chord)))
     (setf (rhyt-object self) new-voice)))

(defmethod new-ryth-object ((self rythmicpanel) (poly poly))
   (let ((newpoly (make-instance (type-of poly)
                    :inside (loop for item in (inside poly)
                                  collect (let ((new-voice (clone item)))
                                            (setf (chords new-voice) (list (make-instance 'chord)))
                                            new-voice)))))
     (setf (rhyt-object self) newpoly)))

(defmethod change-system ((self rythmicpanel) newsys)
   (call-next-method)
   (if (string-equal "EMPTY" (string newsys))
     (progn (setf (emptymode self) t) (update-panel self t))
     (if (emptymode self) (progn (setf (emptymode self) nil) (update-panel self t)))))


;(defclass voicepanel (rythmicpanel chordseqPanel) ())

(omg-defclass voicepanel  (chordseqPanel) ())


 
(defmethod draw-view-contents ((self voicepanel))
  (let* ((x0  (om-h-scroll-position self))
         (y0  (om-v-scroll-position self))
         (size (staff-size self))
         (deltax (get-key-space self))
         (deltay (round (* size (score-top-margin self))))
         )
    (when (and (linear? self) (cursor-p self))
      (draw-interval-cursor self))
    (om-with-focused-view self
      (om-with-font (om-make-font *heads-font* size)  
                    (draw-system-only self)
                    (when (graphic-obj self)
                      (om-with-clip-rect self (om-make-rect (+ x0 (- deltax (round size 2))) y0  (+ x0 (w self)) (+ y0 (h self)))
                        (draw-object (graphic-obj self) self deltax 
                                     (- deltay (round (* (posy (car (staff-list (staff-sys self)))) (/ size 4))))
                                     (staff-zoom self) x0 (+ x0 (w self)) y0 (+ y0 (h self)) 
                                     (slots-mode self) size (linear? self) (staff-sys self) (grille-step-p self) (noteaschan? self))
                                           
                        (if (analysis-mode? self) (draw-analysis self))
                        (draw-score-selection (graphic-obj self) (selection? self) (staff-sys self) size)
                        (draw-edit-cursor self deltay))
                      )))))

        
;ACTIONS

(defmethod set-unset-grille ((self voicepanel)) (om-beep))
(defmethod edit-step-grille ((self voicepanel)) t)
(defmethod translate-chords-p ((self voicepanel)) nil)


(defmethod change-ties-too ((self voicePanel) chord)
   (let ((pointer (next-container chord '(chord))))
     (if (cont-chord-p pointer) 
       (tie-chord chord 'begin)
       (setf pointer nil))
     (loop while pointer do
           (interchange-chords  pointer chord)
           (tie-chord pointer (state pointer))
           (setf pointer (next-container pointer '(chord)))
           (unless (cont-chord-p pointer) (setf pointer nil)))
     (update-panel self t)))

(defmethod omselect-with-shift ((self voicePanel) note)
   (unless (and (note-p (reference note)) (cont-chord-p (parent (reference note))))
     (call-next-method)))


(defmethod select-all ((self scorePanel))
  (when (edit-cursor self)
    (setf (edit-cursor self) nil))
  (let ((grap-class (grap-class-from-type  (obj-mode self))))
    (setf (selection? self) nil)
    (loop for item in (get-graph-type-obj (graphic-obj self) grap-class) do
          (push-select-note self item))
    (update-panel  self t)))

(defmethod select-note ((self voicePanel) note)
   (unless (and (note-p (reference note)) (cont-chord-p (parent (reference note))))
     (call-next-method)))

(defmethod push-select-note ((self voicePanel) note)
   (unless (or (and (note-p (reference note)) (cont-chord-p (parent (reference note))))
               (and (group-p (reference note)) (group-p (parent (reference note)))))
     (call-next-method)))


(defmethod show-tempo ((self voicepanel)) 
  (car (tempo (object (om-view-container self)))))

(defmethod record2obj ((self voicepanel) list)
   (let* ((editor (om-view-container self))
          (obj  (object editor)))
     (when list
       (close-attached-editors editor)
       (let* ((news (make-quanti-chords list *global-deltachords*)))
         (setf (chords obj) (loop for item in (chords obj)
                                  for newc in news
                                  collect newc))))
     obj))


;(defun rhythm-convert-interval (self)
;   (let* ((obj (car (get-obj-to-play self)))
;          (dur (get-obj-dur obj))
;          (int (cursor-interval self))
;          (list (sort (collect-temporal-objects (graphic-obj self) (reference (graphic-obj self))) '< :key 'car ))
;          (list (remove-duplicates list :test 'equal :key 'car))
;          (fpoint (if (listp int) (car int) int))
;          (fig (pop list))
;          fig2 x x1)
;     (loop while (and fig (< (car (rectangle (second fig))) fpoint)) do
;           (setf fig (pop list)))
;     (if fig
;       (progn 
;         (setf fig2 fig)
;         (setf x (car fig))
;         (if (not (listp int))
;           (progn (setf x1 dur) (setf fig2 (car (last list))))
;           (progn
;             (loop while (and fig2 (< (car (rectangle (second fig2))) (second int))) do
;                   (setf fig2 (pop list)))
;             (if fig2 (setf x1 (car fig2))
;                 (progn (setf x1 dur) (setf fig2 (car (last list))))))))
;       (setf x 0 x1 0))
;     (list x  x1 (second fig) (second fig2))))


(defmethod adjoust-grille-chords ((self voicepanel)) t)

;(defmethod convert-interval ((self voicepanel))
;   (rhythm-convert-interval self))  

;(defun rythm-draw-interval-cursor (self)
;  (let* ((interval (print (cursor-interval self)))
;         (cursor-pos-pix (if (third interval)
;                             (car (rectangle (third interval)))
;                           (+ (get-key-space self) (ms2pixel (cursor-pos self) (/ (staff-size self) 4) (staff-zoom self))))))
;    
;    (om-with-focused-view self
;      (when interval (and (third interval) (fourth interval))
;        (draw-h-rectangle (list (+ (get-key-space self) (ms2pixel (car interval) (/ (staff-size self) 4) (staff-zoom self))) ;(car (rectangle (third interval))) 
;                                (om-v-scroll-position self) 
;                                (+ (get-key-space self) (ms2pixel (cadr interval) (/ (staff-size self) 4) (staff-zoom self)))
;                                ;(third (rectangle (fourth interval))) 
;                                (+ (om-v-scroll-position self) (h self)))
;                          t)) 
;      (om-with-fg-color self *om-red2-color*
;        (om-with-dashline 
;            (om-with-line-size 2 
;              (om-draw-line cursor-pos-pix (om-v-scroll-position self) 
;                             cursor-pos-pix (+ (om-v-scroll-position self) (h self)))))))
;    ))



(defmethod edit-preferences ((self voicepanel))
   (om-beep-msg "No mode page for this editor"))


(defmethod panel-save-as-etf ((self voicePanel))
   (save-as-etf (object (om-view-container self)) (staff-tone self)))

(defmethod panel-save-as-xml ((self voicePanel))
   (export-musicxml (object (om-view-container self)) '((G 2)) (staff-tone self)))


(defmethod get-help-list ((self voicepanel)) 
  (list '((cmd+clic "Add Note/Chord/Measure")
          (del "Delete Selection")
          (tab "Change Obj. Mode")
          (("z") "Obj/Time Selection")
          (ud "Transpose Selection")
          (shift+ud "Transpose Octave")
          (("+") "Union Pulses")
          (("-") "Break Group (Group Mode)")
          (esc "Switch Note/Silence")
          (1-9 "Subdivise Pulse")
          (("=") "Tie Selection")
          (("/") "Untie Selection")
          ;(.)
          )
        '((("C") "Change Color")
          (("s") "Set Editor Scale")
          (("t" "T") "Set/Remove Tonality")
          (("n") "Set Voice Name")
          (("o") "Open Internal Editor")
          (("c") "Show Channel Color")
          (space "Play/Stop")
          )))

;==============================================
;POLY
;==============================================


;CONTROLS
(defclass poly-controls-view (multiseq-controls-view) ())

(defmethod GET-slot-LIST ((self poly-controls-view)) 
   '(("midic" midic) ("channel" chan) ("dyn" dyn) ("port" port)))

;VIEW
(defclass polyEditor (multiseqEditor) ())

(defmethod get-score-class-ctrls ((self polyEditor)) 'poly-controls-view)
(defmethod get-score-class-panel ((self polyEditor)) 'polypanel)
(defmethod show-position-ms ((self polyEditor) point) t)



;PANEL

(defclass polypanel (multiseqPanel)  ())
        
(defmethod adjoust-grille-chords ((self polypanel)) t)


(defmethod show-tempo ((self polypanel)) 
   (loop for item in (voices (object (om-view-container self)))
         collect (car (tempo item))))

(defmethod translate-chords-p ((self polypanel)) nil)

(defmethod change-multi-inside ((self polypanel) newobjes)
   (let* ((editor (om-view-container self))
          (obj  (object editor)) staff)
     (setf (inside obj) newobjes)
     (setf staff (correct-staff-val obj (get-edit-param editor 'staff) self))
     (set-edit-param editor 'staff  staff)
     (setf (staff-sys self) (get-staff-system staff))
     obj))

(defmethod set-unset-grille ((self polypanel)) (om-beep))
(defmethod edit-step-grille ((self polypanel)) t)

(defmethod draw-line-cursor ((self polypanel) &key newpixel (draw? t)) 0)

(defmethod record2obj ((self polyPanel) list)
   (let* ((obj (object (om-view-container self))))
     (when list
       (let ((track-list (make-list 16 :initial-element nil)) rep)
         (loop for note in list do
               (push note (nth (fifth note) track-list)))
         (setf track-list (remove nil track-list))
         (loop for item in track-list do
               (setf item (sort item '< :key 'second))
               (let ((chords (sort (make-quanti-chords item *global-deltachords*) '< :key 'offset)))
                 (push chords rep)))
         (loop for item in (inside obj)
               for chrds in (reverse rep) do
               (setf (chords item) chrds))))
     obj))


(defmethod change-ties-too ((self polypanel) chord)
   (let ((pointer (next-container chord '(continuation-chord))))
     (if (cont-chord-p pointer) 
       (tie-chord chord 'begin)
       (setf pointer nil))
     (loop while pointer do
           (interchange-chords  pointer chord)
           (tie-chord pointer (state pointer))
           (setf pointer (next-container pointer '(chord)))
           (unless (cont-chord-p pointer) (setf pointer nil)))
     (update-panel self t)))


 
(defmethod draw-view-contents ((self polypanel))
   (let* ((x0  (om-h-scroll-position self))
          (y0  (om-v-scroll-position self))
          (size (staff-size self))
          (deltax (get-key-space self))
          (deltay (round (* size (score-top-margin self)))))
     (when (and (linear? self) (cursor-p self))
       (draw-interval-cursor self))
     (om-with-focused-view self
       (om-with-font (om-make-music-font *heads-font* size)
                     (draw-system-only self)
                     
                     (when (graphic-obj self)
                       (om-with-clip-rect self (om-make-rect (+ x0 (- deltax (round size 2))) y0  (+ x0 (w self)) (+ y0 (h self)))
           
                         (draw-object (graphic-obj self) self deltax deltay 
                                      (staff-zoom self) x0 (+ x0 (w self)) y0 (+ y0 (h self) ) 
                                      (slots-mode self) (staff-size self) (linear? self) (staff-sys self) 
                                      nil (noteaschan? self))
                         (draw-score-selection (graphic-obj self) (selection? self) (staff-sys self) size)
                         (draw-edit-cursor self deltay)
                         )
                       )))))



(defmethod change-ties-too ((self polypanel) chord)
   (let ((pointer (next-container chord '(chord))))
     (if (cont-chord-p pointer) 
       (tie-chord chord 'begin)
       (setf pointer nil))
     (loop while pointer do
           (interchange-chords  pointer chord)
           (tie-chord pointer (state pointer))
           (setf pointer (next-container pointer '(chord)))
           (unless (cont-chord-p pointer) (setf pointer nil)))
     (update-panel self t)))


(defmethod select-note ((self polypanel) note)
   (unless (and (note-p (reference note)) (cont-chord-p (parent (reference note))))
     (call-next-method)))

(defmethod omselect-with-shift ((self polypanel) note)
   (unless (and (note-p (reference note)) (cont-chord-p (parent (reference note))))
     (call-next-method)))

(defmethod push-select-note ((self polypanel) note)
   (unless (or (and (note-p (reference note)) (cont-chord-p (parent (reference note))))
               (and (group-p (reference note)) (group-p (parent (reference note)))))
     (call-next-method)))


;(defmethod convert-interval ((self polypanel)) (rhythm-convert-interval self))

(defmethod edit-preferences ((self polypanel)) (om-beep-msg "No mode page for this editor"))

(defmethod panel-save-as-etf ((self polypanel)) 
  (save-as-etf (object (om-view-container self)) (staff-tone self) ))

(defmethod panel-save-as-xml ((self polypanel)) 
  (export-musicxml (object (om-view-container self)) '((G 2)) (staff-tone self)))


(defmethod get-help-list ((self polypanel)) 
  (list '((alt+clic "Add Note/Chord/Measure/Voice")
          (del "Delete Selection")
          (tab "Change Obj. Mode")
          (("z") "Obj/Time Selection")
          (ud "Transpose Selection")
          (shift+ud "Transpose Octave")
          (("+") "Union Pulses")
          (("-") "Break Group (Group Mode)")
          (esc "Switch Note/Silence")
          (1-9 "Subdivise Pulse")
          (("=") "Tie Selection")
          (("/") "Untie Selection")
          ;(.)
          )
        '((("C") "Change Color")
          (("s") "Set Editor Scale")
          (("t" "T") "Set/Remove Tonality")
          (("n") "Set Selected Voice Name")
          (("o") "Open Internal Editor")
          (("c") "Show Channel Color")
          (space "Play/Stop")
          )))



;==============================================
;Maquette repons
; a terminer...
; non a effacer
;==============================================
;WINDOW

;CONTROLS
(omg-defclass maqobjEditor (scoreEditor) ())

(defmethod update-subviews ((self maqobjEditor))
   (change-zoom (panel self)))

(defmethod get-edit-param   ((self maqobjEditor) (param (eql 'staff))) 'g)

;-------------INITS
(defmethod get-score-class-panel ((self maqobjEditor)) 'maqobjPanel)
(defmethod do-editor-null-event       ((self maqobjEditor)) nil) 


;PANEL
(omg-defclass maqobjPanel (chordseqPanel) 
   ())
                 
(defmethod update-panel ((self maqobjPanel) &optional (updateref nil))
   (update-panel (om-view-container self) updateref))

(defmethod select-all ((self maqobjPanel)) nil)

(defmethod draw-view-contents ((self maqobjPanel))
   t)


(defmethod change-zoom ((self maqobjPanel))
   t)



(defmethod control-actives ((self maqobjPanel) where) t)
(defmethod get-key-space ((self maqobjPanel)) 0)

(defmethod get-key-space ((self t)) 0)


;=========================================================
;EDITION
;=========================================================
;UNION

(defmethod do-union ((self scorePanel)) (om-beep))

(defmethod do-union ((self chordseqPanel)) 
   (union-a-chs (object (om-view-container self)) (selection? self))
   (setf (selection? self) nil)
   (update-panel self t))

(defmethod do-union ((self multiseqPanel)) 
   (let ((obj (grap-class-from-type  (obj-mode self)))
         (multi-seq (object (om-view-container self))))
     (cond ((equal obj 'grap-chord-seq)
            (let ((selection (selection? self)))
              (when (> (length selection) 1)
                (loop for item in (cdr selection) do
                      (chseq-union (car selection) item))
                (change-multi-inside self (inside multi-seq))
                (setf (selection? self) nil)
                (update-panel self t))))
           ((equal obj 'grap-multiseq)
            (let ((selection (when (selection? self) (inside (car (selection? self))))))
              (when (> (length selection) 1)
                (loop for item in (cdr selection) do
                      (chseq-union (car selection) item))
                (change-multi-inside self (inside multi-seq))
                (setf (selection? self) nil)
                (update-panel self t))))
           (t (let* ((themulti (object (om-view-container self)))
                     (list (cons-selection-in-chseqs themulti (selection? self))))
                (loop for item in list
                      for chs in (inside themulti) do
                      (when item
                        (union-a-chs chs item)))
                (setf (selection? self) nil)
                (update-panel self t))))))

(defmethod do-union ((self voicePanel))
   (let ((obj (grap-class-from-type  (obj-mode self)))
         (voice (object (om-view-container self))))
     (cond
      ((equal obj 'grap-note) (om-beep))
      ((equal obj 'grap-chord)
       (union-a-voice (object (om-view-container self)) (selection? self))
       (setf (selection? self) nil)
       (update-panel self t))
      ((equal obj 'grap-group)
       (loop for item in (selection? self) do
             (group-union item))
       (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
       (setf (selection? self) nil)
       (update-panel self t))
      ((equal obj 'grap-measure)
       (loop for list in (contigous-objects (selection? self) '(measure)) do
             (union-chord&rest-list  list))
       (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
       (setf (selection? self) nil)
       (update-panel self t))
      (t (union-a-voice (object (om-view-container self)) (selection? self))
         (setf (selection? self) nil)
         (update-panel self t)))))

(defmethod do-union :after ((self voicePanel))
  (clean-groups self)
  t)


(defmethod clean-groups ((self voicepanel))
  (let ((voice (object (om-view-container self))))
    ;(setf (tree voice) (cleantree (tree voice)))
    (loop for mesure in (inside voice) do
          (loop for obj in (inside mesure) do 
                (clean-groups obj)))
    (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
    (update-panel self t)))

(defmethod clean-groups ((self t)) t)

(defmethod clean-groups ((self group)) 
  (if (equal 1 (length (inside self)))
    (group-union self)
    (loop for item in (inside self) do (clean-groups item))))


(defun do-union-voice (voice obj selection)
   (cond
    ((equal obj 'grap-note) (om-beep))
    ((equal obj 'grap-chord)
     (union-a-voice voice selection))
    ((equal obj 'grap-group)
     (loop for item in selection do
           (group-union item))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice)))
    ((equal obj 'grap-measure)
     (loop for list in (contigous-objects selection '(measure)) do
           (union-chord&rest-list  list))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice)))
    (t (union-a-voice voice selection))))

(defmethod do-union ((self polyPanel))
   (let ((obj (grap-class-from-type  (obj-mode self)))
         (thepoly (object (om-view-container self))))
     (cond ((equal obj 'grap-voice)
            (let ((selection (selection? self)))
              (when (> (length selection) 1)
                (loop for item in (cdr selection) do
                      (rythm-union (car selection) item))
                (change-multi-inside self (inside thepoly))
                (setf (selection? self) nil)
                (update-panel self t))))
           ((equal obj 'grap-poly)
            (let ((selection (when (selection? self) (inside (car (selection? self))))))
              (when (> (length selection) 1)
                (loop for item in (cdr selection) do
                      (rythm-union (car selection) item))
                (change-multi-inside self (inside thepoly))
                (setf (selection? self) nil)
                (update-panel self t))))
           (t (let* ((thepoly (object (om-view-container self)))
                     (list (cons-selection-in-voices thepoly (selection? self))))
                (loop for item in list
                      for voice in (inside thepoly) do
                      (when item
                        (do-union-voice voice obj item)))
                (setf (selection? self) nil)
                (update-panel self t))))))

;-------------------

(defun change-multi-selection (selection)
  (if (multi-seq-p (car selection))
    (inside (car selection)) selection))

(defun get-the-ch-seq (obj)
   (cond
    ((null obj) nil)
    ((chord-seq-p obj) obj)
    (t (get-the-ch-seq (parent obj)))))

(defun cons-selection-in-chseqs (self selection)
  (let* ((ch-seqs (inside self))
         (rep (create-list (length ch-seqs) nil)))
    (setf selection (change-multi-selection selection))
    (loop for item in selection do
          (let* ((thech-seq (get-the-ch-seq item))
                 (pos (position thech-seq ch-seqs :test 'equal)))
            (when pos
              (push item (nth pos rep)))))
    (loop for item in rep collect (reverse item))))

;;;====== GROUP = rassemble sans changer les offset

(defmethod chord-group ((self chord) (chord1 chord))
   (let ((offset (LOffset self)))
     (setf (LMidic self) (append (LMidic self) (LMidic chord1))
           (LVel self) (append (LVel self) (LVel chord1))
           (Ldur self) (append (LDur self) (LDur chord1))
           (LChan self) (append (LChan self) (LChan chord1)))
     (setf (Loffset self)  (append offset (om+ (- (offset->ms chord1) (offset->ms self)) (LOffset chord1))))
     self))

(defmethod chord-group ((self null) (chord1 chord)) chord1)

(defmethod group-a-chs ((thechordseq chord-seq) selection) 
  (let (newchord new-chord-list offset)
    (setf selection (loop for item in selection
                        append (cons-chord-list item)))
     (when (car selection)
       (setf offset (offset->ms (car selection))))
     (loop for item in selection do
           (when (chord-p item)
             (setf newchord (chord-group newchord item))))
     (when newchord
       (loop for item in selection do
             (setf (inside thechordseq) (remove item (inside thechordseq) :test 'equal)))
       (setf new-chord-list (cons newchord (chords thechordseq)))
       (loop for item in (chords thechordseq) do
             (setf (offset item) (offset->ms item)))
       (setf (offset newchord) offset)
       (setf new-chord-list (cons newchord (chords thechordseq)))
       (setf (inside thechordseq) (sort new-chord-list '< :key 'offset)) 
       (setf (Qvalue thechordseq) 1000)
       (adjust-extent thechordseq)
       (QNormalize thechordseq))))



;;;======== UNION = rassemble + offset 0

(defmethod union-a-chs ((thechordseq chord-seq) selection) 
  (let (newchord new-chord-list offset)
    (setf selection (loop for item in selection
                        append (cons-chord-list item)))
     (when (car selection)
       (setf offset (offset->ms (car selection))))
     (loop for item in selection do
           (when (chord-p item)
             (setf newchord (chord-union newchord item))))
     (when newchord
       (loop for item in selection do
             (setf (inside thechordseq) (remove item (inside thechordseq) :test 'equal)))
       (setf new-chord-list (cons newchord (chords thechordseq)))
       (loop for item in (chords thechordseq) do
             (setf (offset item) (offset->ms item)))
       (setf (offset newchord) offset)
       (setf new-chord-list (cons newchord (chords thechordseq)))
       (setf (inside thechordseq) (sort new-chord-list '< :key 'offset)) 
       (setf (Qvalue thechordseq) 1000)
       (adjust-extent thechordseq)
       (QNormalize thechordseq))))
       


(defmethod chord-union ((self null) (chord1 chord)) chord1)

(defmethod chord-union ((self chord) (chord1 chord))
  (setf (LMidic self) (append (LMidic self) (LMidic chord1))
        (LVel self) (append (LVel self) (LVel chord1))
        (Ldur self) (append (LDur self) (LDur chord1))
        (LChan self) (append (LChan self) (LChan chord1))
        (Loffset self) (list 0))
  self)

(defmethod cons-chord&rest-list ((self t)) nil)
(defmethod cons-chord&rest-list ((self chord)) (list self))
(defmethod cons-chord&rest-list ((self rest)) (list self))
(defmethod cons-chord&rest-list ((self container)) 
   (loop for item in (inside self) append (cons-chord&rest-list item)))

(defmethod mk-union-chord&rest-list ((self list))
   (let (aux rep) 
     (loop for item in self do
           (let ((next (next-container item '(chord rest))))
             (push item aux)
             (when (or (not (member next self :test 'equal))
                       (not (subtypep (type-of next) (type-of item)))
                       (not (equal (parent next) (parent item))))
               (when (> (length aux) 1) (push (reverse aux) rep))
               (setf aux nil))))
     (reverse rep)))

(defun contigous-objects (self type)
  (let (aux rep)
    (setf self (sort self '< :key 'offset))
     (loop for item in self do
           (let ((next (next-container item type)))
             (push item aux)
             (when (or (not (member next self :test 'equal))
                       (not (subtypep (type-of next) (type-of item)))
                       (not (equal (parent next) (parent item))))
               (when (> (length aux) 1) (push (reverse aux) rep))
               (setf aux nil))))
     (reverse rep)))

(defun union-a-voice (voice selection)
   (let* ((chords (loop for item in selection
                        append (cons-chord&rest-list item))))
     (setf chords (sort chords '< :key #'(lambda (x) (offset->ms x voice))))
     (setf chords (mk-union-chord&rest-list chords))
     (loop for list in chords do
           (union-chord&rest-list list))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
     ))


(defmethod union-chord&rest-list ((self list))
   (loop for item in (cdr self) do
         (rythm-union (car self) item)))

(defmethod rythm-union ((self chord) (chord1 chord)) 
  
   (setf (extent self) (+ (extent self) (extent chord1)))
   (setf (inside self) (append (inside self) (inside chord1)))
   (setf (inside self) (remove-duplicates (inside self) :test 'equal :key 'midic))
   (setf (inside (parent self)) (remove chord1 (inside (parent self)) :test 'equal)))

(defmethod rythm-union ((self measure) (chord1 measure)) 
   (let* ((sign1 (nth 0 (tree self)))
          (sign2 (nth 0 (tree chord1)))
          (sign3 (+ (/ (car sign1) (second sign1)) (/ (car sign2) (second sign2)))))
     (setf (extent self) (+ (extent self) (extent chord1)))
     (setf (inside self) (append (inside self) (inside chord1)))  ;OJO una normalization qvalue debe ser hecha
     (setf (nth 0 (tree self)) (list (numerator sign3) (denominator sign3))) 
     (setf (inside (parent self)) (remove chord1 (inside (parent self)) :test 'equal))))

(defmethod rythm-union ((self rest) (chord1 rest)) 
   (setf (extent self) (+ (extent self) (extent chord1)))
   (setf (inside (parent self)) (remove chord1 (inside (parent self)) :test 'equal)))

(defmethod rythm-union ((self voice) (chord1 voice)) 
   (let ((newchseq (merger self chord1)))
     (setf (offset self) (offset newchseq) 
           (extent self) (extent newchseq)
           (Qtempo self) (Qtempo newchseq)
           (Qvalue self) (Qvalue newchseq)
           (inside self) (inside newchseq))
     (setf (inside (parent self)) (remove chord1 (inside (parent self)) :test 'equal))))

(defmethod rythm-union ((self group) (chord1 group)) chord1)

(defmethod group-union ((self group))
   (let* ((pere (parent self))
          (pos (position self (inside pere) :test 'equal))
          (newch (make-instance 'chord)))
     (setf (offset newch) (offset self) 
           (extent newch) (extent self)
           (Qtempo newch) (Qtempo self)
           (Qvalue newch) (Qvalue self))
     (setf (nth pos (inside pere)) newch))) 

(defmethod chseq-union ((self chord-seq) (chord1 chord-seq))
   (let ((newchseq (merger self chord1)))
     (setf (offset self) (offset newchseq) 
           (extent self) (extent newchseq)
           (Qtempo self) (Qtempo newchseq)
           (Qvalue self) (Qvalue newchseq)
           (inside self) (inside newchseq))
     (setf (inside (parent self)) (remove chord1 (inside (parent self)) :test 'equal))
     ))
           




;DELETE
;=======================
(defmethod set-defaul-value ((self t)) t)

(defmethod set-defaul-value ((self chord))
   (setf (inside self) (list (make-instance 'note))))

(defmethod set-defaul-value ((self chord-seq))
   (let ((chord (make-instance 'chord)))
     (setf (inside self) (list chord))
     (setf (offset chord) 0)
     (setf (Qvalue self) 1000)
     (adjust-extent self)
     (QNormalize self)))

(defmethod set-defaul-value ((self multi-seq))
   (setf (inside self) (list (make-instance 'chord-seq))))

(defmethod set-defaul-value ((self voice))
   (setf (inside self) (list (make-instance 'measure))))

(defmethod set-defaul-value ((self poly))
   (setf (inside self) (list (make-instance 'voice))))



;=======================
(defmethod delete-selection ((self notePanel)) 
  (unless (system? (car (selection? self)))
    (loop for obj in (selection? self) do
          (when (extra-p obj)
            (general-delete self obj)))
    (update-panel self t)))

(defmethod delete-selection ((self scorePanel))
  (unless (system? (car (selection? self)))
    (loop for obj in (selection? self) do
          (general-delete self obj))
    (setf (selection? self) nil)
    (update-panel self t)))

(defmethod delete-selection ((self voicePanel))
  (unless (system? (car (selection? self)))
   (let ((voice (object (om-view-container self))))
     (loop for obj in (selection? self) do
           (general-delete self obj))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
     (setf (selection? self) nil)
     (update-panel self t))))

(defmethod delete-selection ((self polyPanel))
  (unless (system? (car (selection? self)))
    (let (voices)
      (loop for obj in (selection? self) do
            (push (get-the-voice obj) voices)
            (general-delete self obj))
      (loop for voice in (remove-duplicates (remove nil voices :test 'equal) :test 'equal) do
            (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice)))
      (setf (selection? self) nil)
      (update-panel self t))))

(defmethod deep-delete-obj ((self t) container) t)

(defmethod deep-delete-obj ((self simple-container) container)
   (let ((pere (parent self)))
     (if pere
       (if (not (equal self container))
         (progn
           (setf (inside pere) (remove self (inside pere) :test 'equal))
           (unless (inside pere)
             (deep-delete-obj (parent self) container))))
       (setf (inside container) nil))))

(defun delete-with-test (self container)
   (deep-delete-obj self container)
   (unless (inside container)
     (set-defaul-value container)))

(defmethod general-delete ((view t) (self t))
   (let ((thechord (object (om-view-container view))))
     (delete-with-test self thechord)))

(defmethod general-delete ((view chordPanel) (self chord))
   (let ((thechord (object (om-view-container view))))
     (delete-with-test self thechord)))

(defmethod general-delete ((view scorePanel) (self note))
   (delete-with-test self (object (om-view-container view))))


(defmethod general-delete ((view chordseqPanel) (self note))
   (let ((chord-seq (object (om-view-container view))))
     (delete-with-test self chord-seq)
     (adjust-extend-after-delete view))
   )

(defmethod general-delete ((view chordseqPanel) (self chord-seq))
   (let ((chord-seq (object (om-view-container view))))
     (delete-with-test self chord-seq)
     (adjust-extend-after-delete view)))

(defmethod general-delete ((view chordseqPanel) (self chord))
   (let ((chord-seq (object (om-view-container view))))
     (delete-with-test self chord-seq )
     (adjust-extend-after-delete view)))


(defmethod adjust-extend-after-delete  ((view chordseqpanel) ) 
  (let ((chordseq (object (om-view-container view))))
    (adjust-extent chordseq)
    (QNormalize chordseq)))

;---------MULTISEQ
(defmethod general-delete ((view multiseqPanel) (self note))
    (delete-with-test self (parent (parent self)))
    (adjust-extend-after-delete view))

(defmethod general-delete ((view multiseqPanel) (self chord-seq))
   (let ((multi-seq (object (om-view-container view))))
     (delete-with-test self multi-seq)
     (change-multi-inside view (inside multi-seq))
     (adjust-extend-after-delete view)))

(defmethod general-delete ((view multiseqPanel) (self multi-seq))
   (let ((multi-seq (object (om-view-container view))))
     (delete-with-test self multi-seq)
     (change-multi-inside view (inside multi-seq))
     (adjust-extend-after-delete view)))

;a faire
(defmethod adjust-extend-after-delete  ((view multiseqPanel) ) 
  t)




;---------VOICE
(defun remove-element-from-voice (voice deleted)
   (let ((next (next-container deleted '(chord))))
     (loop while (and next (cont-chord-p next)) do
           (deep-delete-obj next voice)
           (setf next (next-container deleted '(chord))))
     (setf (inside (parent deleted)) (remove deleted (inside (parent deleted)) :test 'equal))))
     

(defmethod general-delete ((view voicePanel) (self note))
   (if (> (length (inside (parent self))) 1)
     (progn
       (when (prep-chord-p (parent self))
         (loop for note in (loop for next = (next-tied-note self) then (next-tied-note next)
                                 while (and next (cont-chord-p (parent next)))
                                 collect next)
               do  (setf (inside (parent note)) (remove note (inside (parent note)) :test 'equal))))
       (setf (inside (parent self)) (remove self (inside (parent self)) :test 'equal)))
     (general-delete view (parent self))))

(defmethod general-delete ((view voicePanel) (self t))
   (if (> (length (inside (parent self))) 1)
     (let* ((voice (object (om-view-container view))))
       (remove-element-from-voice voice self))
     (general-delete view (parent self))))



(defmethod general-delete ((view voicePanel) (self voice))
   (let* ((voice (object (om-view-container view))))
     (setf (tree voice) '(? ((4//4 (-4)))))))

;-----------------------------

(defun get-the-voice (obj)
   (cond
    ((null obj) nil)
    ((voice-p obj) obj)
    ((poly-p obj) nil)
    ((extra-p obj) nil)
    (t (get-the-voice (parent obj)))))

(defun get-the-measure (obj)
   (cond
    ((null obj) nil)
    ((voice-p obj) nil)
    ((poly-p obj) nil)
    ((measure-p obj) obj)
    ((extra-p obj) nil)
    (t (get-the-measure (parent obj)))))

(defmethod general-delete ((view polyPanel) (self note))
   (if (> (length (inside (parent self))) 1)
     (progn
       (when (prep-chord-p (parent self))
         (loop for note in (loop for next = (next-tied-note self) then (next-tied-note next)
                                 while (and next (cont-chord-p (parent next)))
                                 collect next)
               do  (setf (inside (parent note)) (remove note (inside (parent note)) :test 'equal))))
       (setf (inside (parent self)) (remove self (inside (parent self)) :test 'equal)))
     (general-delete view (parent self))))

(defmethod general-delete ((view polyPanel) (self t))
   (if (> (length (inside (parent self))) 1)
     (let* ((voice (get-the-voice self)))
       (remove-element-from-voice voice self))
     (general-delete view (parent self))))

(defmethod general-delete ((view polyPanel) (self voice))
   (if (> (length (inside (parent self))) 1)
     (change-multi-inside view (remove self (inside (object (om-view-container view))) :test 'equal))
     (general-delete view (parent self))))

(defmethod general-delete ((view polyPanel) (self poly))
   (change-multi-inside view (list (make-instance 'voice :tree '(? ((4//4 (-4))))))))

;TOGGLE
;=======================

(defmethod toggle-selection ((self scorePanel)) (om-beep))

(defmethod toggle-selection ((self voicePanel))
   (let* ((voice (object (om-view-container self))))
     (loop for obj in (selection? self) do
           (general-toggle self obj))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
     (setf (selection? self) nil)
     (update-panel self t)))

(defmethod toggle-selection ((self polyPanel))
   (let (voices)
     (loop for obj in (selection? self) do
           (if (poly-p obj)
               (setf voices (append voices (inside obj)))
             (push (get-the-voice obj) voices))
           (general-toggle self obj))
     (loop for voice in (remove-duplicates voices :test 'equal) do
           (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice)))
     (setf (selection? self) nil)
     (update-panel self t)))



;---------VOICE
     
(defmethod toggle ((self rest))
   (change-class self 'chord)
   (setf (inside self) (list (make-instance 'note)))
   self)

(defmethod toggle ((self chord))
   (untie-chord-2 self)
   (change-class self 'rest)
   self
   )

(defmethod toggle ((self group))
   (setf (inside self)
         (loop for item in (inside self)
               collect (toggle item)))
   self)

(defmethod toggle ((self measure))
   (setf (inside self)
         (loop for item in (inside self)
               collect (toggle item)))
   self)

(defmethod toggle ((self voice))
   (setf (inside self)
         (loop for item in (inside self)
               collect (toggle item)))
   self)

(defmethod toggle ((self poly))
   (setf (inside self)
         (loop for item in (inside self)
               collect (toggle item)))
   self)

(defmethod general-toggle ((view voicePanel) (self note)) t)

(defmethod general-toggle ((view voicePanel) (self t))
   (toggle self))


;-----------------------------

(defmethod general-toggle ((view polyPanel) (self note)) t)


(defmethod general-toggle ((view polyPanel) (self t))
   (toggle self))

(defmethod general-toggle ((view polyPanel) (self poly))
   (loop for item in (inside self) do (general-toggle view item)))


;UN_TIE
;=======================


(defmethod untie-selection ((self scorePanel)) (om-beep))

(defmethod untie-selection ((self voicePanel))
  (let ((voice (object (om-view-container self))))
   (loop for obj in (selection? self) do
           (untie-chord  obj))
     (setf (selection? self) nil)
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
     (update-panel self t)))

(defmethod untie-selection ((self polyPanel))
   (loop for obj in (selection? self) do
         (untie-chord obj))
   (setf (selection? self) nil)
   (loop for voice in (voices (object (editor self))) do
         (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice)))
   (update-panel self t))

(defmethod get-all-continuation-chords ((self t))
   (let ((next (next-container self '(chord)))
         rep) 
     (loop while (and next (cont-chord-p next)) do
           (push next rep)
           (setf next (next-container next '(chord))))
     (reverse rep)))


(defmethod untie-chord-cont ((self continuation-chord))
   (loop for item in (inside self) do
         (cond
          ((equal (tie item) 'end)
           (setf (tie item) nil))
          ((equal (tie item) 'continue)
           (setf (tie item) 'begin))
          (t (setf (tie item) nil))))
   (change-class self 'chord))

(defmethod untie-chord ((self t))
   (loop for item in (cons-chord-list self) do
         (untie-chord item)))

(defmethod untie-chord ((self chord))
   (let ((continuations (get-all-continuation-chords self))
         first-chord)
     (when continuations
       (setf first-chord self)
       (untie-chord-cont (car continuations))
       (loop for item in (inside first-chord) do
             (cond
              ((equal (tie item) 'begin)
               (setf (tie item) nil))
              ((equal (tie item) 'continue)
               (setf (tie item) 'end))
              (t (setf (tie item) nil)))))))

(defmethod untie-chord-2 ((self chord))
   (untie-chord self)
   (let (first-chord)
     (setf first-chord (previous-container self '(chord rest)))
     (when (chord-p first-chord)
       (loop for item in (inside first-chord) do
             (cond
              ((equal (tie item) 'begin)
               (setf (tie item) nil))
              ((equal (tie item) 'continue)
               (setf (tie item) 'end))
              (t (setf (tie item) nil)))))))

(defmethod untie-chord ((self note)) nil)
(defmethod untie-chord ((self rest)) nil)

;TIE
;=======================

(defmethod cons-chord-list ((self t)) nil)
(defmethod cons-chord-list ((self chord)) (list self))
(defmethod cons-chord-list ((self container)) 
   (loop for item in (inside self) append (cons-chord-list item)))

(defmethod mk-tie-chord-list ((self list))
   (let (aux rep) 
     (loop for item in self do
           (let ((next (next-container item '(chord rest))))
             (push item aux)
             (when (not (member next self :test 'equal))
               (when (> (length aux) 1) (push (reverse aux) rep))
               (setf aux nil))))
     (reverse rep)))


(defmethod tie-selection ((self scorePanel)) (om-beep))

(defmethod tie-selection ((self voicePanel))
   (tie-a-voice (object (om-view-container self)) (selection? self))
   (setf (selection? self) nil)
   (update-panel self t))

(defmethod tie-selection ((self polyPanel))
   (let* ((thepoly (object (om-view-container self)))
          (list (cons-selection-in-voices thepoly (selection? self))))
     (loop for item in list
           for voice in (inside thepoly) do
           (when item
             (tie-a-voice voice item)))
     (setf (selection? self) nil)
     (update-panel self t)))

(defun tie-a-voice (voice selection)
   (let* ((chords (loop for item in selection
                        append (cons-chord-list item))))
     (setf chords (sort chords '< :key #'(lambda (x) (offset->ms x voice))))
     (setf chords (mk-tie-chord-list chords))
     (loop for list in chords do
           (tie-chord-list  list))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))))

(defun change-poly-selection (selection)
  (if (poly-p (car selection))
    (inside (car selection)) selection))

(defmethod cons-selection-in-voices ((self poly) selection)
   (let* ((voices (inside self))
          (rep (create-list (length voices) nil)))
     (setf selection (change-poly-selection selection))
     (loop for item in selection do
           (let* ((thevoice (get-the-voice item))
                  (pos (position thevoice voices :test 'equal)))
             (when pos
               (push item (nth pos rep)))))
     (loop for item in rep collect (reverse item))))

(defmethod tie-chord-list ((self list))
   (loop for item in (cdr self) do
         (tie-a-chord (car self) item)))

(defmethod tie-a-chord ((self chord) (totie chord))
   (change-class totie 'continuation-chord)
   (setf (Lmidic totie) (Lmidic self)))






;=========================
;OPEN INTERNAL EDITOR
;=========================
(defmethod obj-for-internal-editor ((self note)) (list 'chordEditor "internal chord"))
(defmethod obj-for-internal-editor ((self chord)) (list 'chordEditor "internal chord"))
(defmethod obj-for-internal-editor ((self chord-seq)) (list 'chordseqEditor  "internal chord-seq"))
(defmethod obj-for-internal-editor ((self voice)) (list 'voiceEditor  "internal voice"))

(defmethod obj-for-internal-editor ((self continuation-chord)) nil)
(defmethod obj-for-internal-editor ((self rest)) nil)
(defmethod obj-for-internal-editor ((self group)) nil)
(defmethod obj-for-internal-editor ((self measure)) nil)
(defmethod obj-for-internal-editor ((self t)) nil)

(defmethod real-internal-editor ((self note)) (parent self))
(defmethod real-internal-editor ((self t)) self)

(defmethod real-internal-editor-list ((self list))
   (remove-duplicates (loop for item in self collect (real-internal-editor item)) :test 'equal))


(defmethod open-internal-editor ((self scorePanel)) (om-beep))

(defmethod open-internal-editor ((self chordseqPanel))
   (loop for item in (real-internal-editor-list (selection? self)) do
         (let ((int-info (obj-for-internal-editor item)))
           (when (and int-info (not (equal (car int-info) 'chordseqEditor)))
             (let ((win (make-editor-window (first int-info) item (second int-info) (om-view-container self))))
               (push win (attached-editors (om-view-container self)))
               )))))

(defmethod open-internal-editor ((self multiseqPanel))
   (loop for item in (real-internal-editor-list (selection? self)) do
         (let ((int-info (obj-for-internal-editor item)))
           (when int-info
             (let ((win (make-editor-window (first int-info) item (second int-info) (om-view-container self))))
               (push win  (attached-editors (om-view-container self)))
               )))))

(defmethod open-internal-editor ((self voicePanel))
   (loop for item in (real-internal-editor-list (selection? self)) do
         (let ((int-info (obj-for-internal-editor item)))
           (when (and int-info (not (equal (car int-info) 'voiceEditor)))
             (let ((win (make-editor-window (first int-info) item (second int-info) (om-view-container self))))
               (push win  (attached-editors (om-view-container self)))
               )))))

(defmethod open-internal-editor ((self polyPanel))
   (loop for item in (real-internal-editor-list (selection? self)) do
         (let ((int-info (obj-for-internal-editor item)))
           (when int-info
             (let ((win (make-editor-window (first int-info) item (second int-info) (om-view-container self))))
               (push win (attached-editors (om-view-container self)))
               )))))


;=========================
;ADD
;=========================
(defmethod add-new-object ((self notePanel) obj where graph-obj)
  (declare (ignore obj))
  (let* ((up (round (* (score-top-margin self) (staff-size self)) ))
         (midic (delta-to-name  (staff-size self)  (- (* -1 (-  (om-point-v where) up )) 
                                                      (round (* (posy (car (staff-list (staff-sys self)))) (/ (staff-size self) 4))) )
                                (* 100 (- (top-in-midi (staff-sys self)) 3))))
         (thenote (object (om-view-container self))))
    (setf (midic thenote) midic)
    (update-panel self t)))

(defmethod add-new-object ((self chordPanel) obj where graph-obj)
  (if (or (= (staff-mode self) 1) (= (staff-mode self) 2))
    (om-beep-msg "Insert notes not allowed in arpeggio mode")
    (let* ((up (round (* (score-top-margin self) (staff-size self)) ))
           (midic (delta-to-name (staff-size self)  (- (* -1 (-  (om-point-v where) up))
                                                       (round (* (posy (car (staff-list (staff-sys self)))) (/ (staff-size self) 4))))
                                 (* 100 (- (top-in-midi (staff-sys self)) 3))))
           (thechord (objectfromeditor self))
           (extras (loop for note in (inside thechord) collect (clone (extra-obj-list note)))))
      (if (= (staff-mode self) 0)
        (progn
          (setf (LMidic thechord) (concatenate 'list (LMidic thechord) (list midic)))
          (setf extras (concatenate 'list extras (list nil))))
        (let ((posnote (find-indice-new-note self (om-point-h where))))
          (progn
            (setf (LMidic thechord) (insert-in-list (LMidic thechord) midic posnote))
            (setf extras (insert-in-list  extras nil posnote)))))
      (loop for note in (inside thechord)
            for extra in extras do
            (loop for ex in extra do
                  (set-extra-in-list ex note)))
      
      (when *om-tonalite*
        (actualise-tonalite thechord))
      
      (update-panel self t))))


(defmethod add-new-object ((self chordseqPanel) obj where graph-obj)
   (if (and graph-obj (chord-p (reference graph-obj)))
     (setf (edit-cursor self) 
           (create-edit-cursor self (reference graph-obj) (om-point-h where) (car (Lmidic (reference graph-obj))) nil 0 nil))
     (multiple-value-bind (whattime numstaff) (pixel-toms self where)
       (when whattime
         (let* ((up (* (score-top-margin self) (staff-size self)) )
                (midic (delta-to-name (staff-size self)  (- (* -1 (-  (om-point-v where) numstaff up)) 
                                                            (round (* (posy (car (staff-list (staff-sys self)))) (/ (staff-size self) 4))))
  (* 100 (- (top-in-midi (staff-sys self)) 3))))
                (chordseq (object (om-view-container self)))
                (new-chord (mki 'chord  :Lmidic (list midic)))
                new-chord-list)
           (setf (offset new-chord) (pixel-toms self where))
           (loop for item in (chords chordseq) do
                 (setf (offset item) (offset->ms item)))
           (setf new-chord-list (cons new-chord (chords chordseq)))
           (setf (inside chordseq) (sort new-chord-list '< :key 'offset)) 
           (setf (Qvalue chordseq) 1000)
           (adjust-extent chordseq)
           (QNormalize chordseq)
           (setf (edit-cursor self) 
                 (create-edit-cursor self new-chord (om-point-h where) (car (Lmidic new-chord)) nil 0 nil))
           
           (when *om-tonalite*
             (actualise-tonalite new-chord))

           (update-panel self t))))))

(defmethod system-from-chord ((self multiseqPanel) chord)
  (let* ((chseq (parent chord))
         (pos (position chseq (inside (parent chseq)) :test 'equal)))
    (nth pos (staff-sys self))))

(defmethod add-new-object ((self multiseqPanel) obj where graph-obj)
   (if (and graph-obj (chord-p (reference graph-obj)))
     (let ((system (system-from-chord self (reference graph-obj)))) 
       (setf (edit-cursor self) 
             (create-edit-cursor self (reference graph-obj) (om-point-h where) (car (Lmidic (reference graph-obj))) nil 
                                 (position system (staff-sys self) :test 'equal) nil)))
     (if (or (equal obj 'grap-chord-seq) (equal obj 'grap-multiseq))
       (let ((multi (object (om-view-container self)))
             (ind (click-in-which-voice? self where)))
         (setf (inside multi) (insert-in-list (inside multi) (make-instance 'chord-seq) ind)) 
         (change-multi-inside self (inside multi))
         (update-panel self t))
       (om-beep-msg "Open a chord-seq internal editor in order to add chords"))))



(defmethod chord-to-edit ((self note)) (parent self))
(defmethod chord-to-edit ((self rest)) self)
(defmethod chord-to-edit ((self chord)) self)
(defmethod chord-to-edit ((self continuation-chord)) nil)
(defmethod chord-to-edit ((self group)) nil)

(defun add-measure-in-voice (voice pos)
  (let* ((sign (if (= pos 0) (car (tree (car (inside voice))))
                  (car (tree (nth (- pos 1) (inside voice))))))
        (newmes (make-instance 'measure :tree (list sign (list (* -1 (car sign)))))))
    (insert-measure-in-voice voice newmes pos)))

(defun insert-measure-in-voice (voice newmes pos)
  (let ((sign (car (tree newmes))))
    (unless (= pos 0)
      (let ((nextmeasure (nth (- pos 1) (inside voice))))
        (untie-chord (car (last (cons-chord-list nextmeasure))))))
    (setf (extent newmes) (* 4 (qvalue voice) (/ (car sign) (second sign))))
  (setf (qvalue newmes) (qvalue voice))
  (setf (inside voice) (insert-in-list (inside voice) newmes pos))
  (setf (offset newmes)
        (if (= pos 0) 0 (offset (nth (+ pos 1) (inside voice)))))
  (loop for item from (+ pos 1) to (- (length (inside voice))) do
        (setf (offset item) (+ (offset item) (extent newmes))))
  (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))))

(defmethod system-from-measure ((self polypanel) measure)
  (let* ((voice (parent measure))
         (pos (position voice (inside (parent voice)) :test 'equal)))
    (nth pos (staff-sys self))))



(defmethod add-new-object ((self VoicePanel) obj where graph-obj)
   (cond
    ((or (equal obj 'grap-note) (equal obj 'grap-chord) (equal obj 'grap-group))
     (let ((whichmeasure (click-in-grap-measure? (graphic-obj self) where)))
       (when whichmeasure
         (setf (edit-cursor self) 
               (create-edit-cursor self (if graph-obj (chord-to-edit (reference graph-obj))) 
                                   (om-point-h where) (* 100 (midicenter (staff-sys self))) t 0 whichmeasure))
         (make-unselect self))))
    ((equal obj 'grap-measure)
     (let* ((voice (object (om-view-container self)) )
            (pos (if graph-obj (position graph-obj (inside (parent graph-obj)) :test 'equal) 
                     (length (inside voice)))))
       (add-measure-in-voice voice pos)
       (setf (selection? self) nil)
       (update-panel self t)))))

(defmethod add-new-object ((self polypanel) obj where graph-obj)
   (cond
    ((or (equal obj 'grap-note) (equal obj 'grap-chord) (equal obj 'grap-group))
     (let ((whichmeasure (click-in-grap-measure? (graphic-obj self) where)) system)
       (when whichmeasure
         (setf system (system-from-measure self whichmeasure))
         (setf (edit-cursor self) 
               (create-edit-cursor self (if graph-obj (chord-to-edit (reference graph-obj))) 
                                   (om-point-h where) (* 100 (midicenter system)) t 
                                   (position system (staff-sys self) :test 'equal) whichmeasure))
         (make-unselect self))))
    ((equal obj 'grap-measure)
     (let* ((poly (object (om-view-container self)) )
            (pos (if graph-obj (position graph-obj (inside (parent graph-obj)) :test 'equal) 
                     (length (inside (car (inside poly)))))))
       (loop for voice in (inside poly) do
             (add-measure-in-voice voice pos))
       (setf (selection? self) nil)
       (update-panel self)))
    ((or (equal obj 'grap-voice) (equal obj 'grap-poly))
     (let* ((poly (object (om-view-container self)))
            (ind (click-in-which-voice? self where)) newtree)
       (when ind
         (setf newtree (tree (nth ind (inside poly))))
         (setf newtree (tree-from-sign (sign-from-tree newtree)))
         (setf (inside poly) (insert-in-list (inside poly) 
                                             (make-instance 'voice :tree newtree) ind)) 
         (change-multi-inside self (inside poly))
         (update-panel self t))))))

(defun sign-from-tree (tree)
  (loop for item in (second tree)
        collect (car item)))

(defun tree-from-sign (list)
  (list '?
        (loop for item in list
              collect (list item (list (* -1 (car item)))))))

;=========================
;TRANSPOSE
;=========================

(defmethod transpose-drag ((self scorePanel) list first-mouse)
  (let* ((new-mouse (om-mouse-position self))
         (ssize (staff-size self)))
    (unless (equal first-mouse new-mouse)
      (let ((midic-transp (delta-to-name ssize (- (om-point-v first-mouse) (om-point-v new-mouse)) 6000)))
        (setf midic-transp (- midic-transp 6000))
        (loop for item in list do
              (transpose-a item midic-transp))
        (update-panel self t) t))))

(defmethod transpose-drag ((self chordPanel) list first-mouse)
   (if (or (= (staff-mode self) 1) (= (staff-mode self) 2))
     (om-beep-msg "Transpose notes not allowed in arpeggio mode")
     (call-next-method)))


(defmethod trans-drag ((self t) pixel ssize) t)

(defmethod trans-drag ((self container) pixel ssize)
   (loop for item in (inside self) do
         (trans-drag item pixel ssize)))

(defmethod trans-drag ((self note) pixel ssize)
   (change-midic self (delta-to-name ssize pixel (midic self))))


(defmethod change-midic ((self t) midic) t)

(defmethod change-midic ((self container) midic)
   (loop for item in (inside self) do
         (change-midic item midic)))

(defmethod change-midic ((self note) midic)
   (transpose-a self (- midic (midic self))))

(defmethod transpose-a ((self t) trans) t)



(defmethod transpose-a ((self container) trans)
   (loop for item in (inside self) do
         (transpose-a item trans)))

(defmethod transposable-p ((self note))
   (not (and (tie self) (not (equal (tie self) 'begin)))))

(defmethod transpose-a ((self note) trans)
   (when (transposable-p self) 
     (when (and (prep-chord-p (parent self)) (equal (tie self) 'begin)) 
       (loop for note in (loop for next = (next-tied-note self) then (next-tied-note next)
                               while (and next (cont-chord-p (parent next)))
                               collect next)
             do  (setf (midic note) (+ (midic note) trans))))
     (setf (midic self) (+ (midic self) trans))
     ))

(defmethod score-move-a  ((self simple-container) panel trans)
  (transpose-a self trans))


;KEY ACTIONS

(defmethod move-selection ((self scorePanel) dir)
  (loop for item in (selection? self) do
        (score-move-a item self (cond
                                 ((om-shift-key-p) (if (= dir 0) 1200 -1200))
                                 ((om-command-key-p) (if (= dir 0) 700 -700))
                                 (t (let ((factor (round (approx-factor (get-current-scale (staff-tone self))))))
                                      (if (= dir 0) factor
                                        (* -1 factor)))))))
        (update-panel self t))


;;; new : changer la duree avec les touche R/L
(defmethod change-dur ((self chordseqpanel) dir)
  (loop for item in (selection? self) do
        (change-dur-to-note item 
                            (cond
                             ((om-shift-key-p) (if (= dir 1) (grille-step self) (- (grille-step self)))) 
                             (t (if (= dir 1) (round (grille-step self) 10) (- (round (grille-step self) 10)))))
                            )
        ;;;(adjust-extent item)
        )
  ;;; idee: utiliser adjust-extent pour mettre a jour l'extent des accords (et du chord-seq si c'est a la fin)
  ;;; pb: ça marche pas (plantages avec pgc dans QNormalize)
  ;;;(adjust-extent (object (editor self)))
  (update-panel self t)
  )


(defmethod change-dur ((self scorepanel) dir)
    (loop for item in (selection? self) do
          (change-dur-to-note item 
                              (cond
                               ((om-shift-key-p) (if (= dir 1) 1000 (- 1000))) 
                               (t (if (= dir 1) 100 (- 100))))))
    (update-panel self t)
    )


(defmethod change-dur-to-note ((self simple-container) val)
  (loop for elt in (inside self) do 
        (change-dur-to-note elt val)))

(defmethod change-dur-to-note ((self note) val)
  (set-dur-to-note self (max 10 (+ (extent->ms self) val))))

(defmethod change-dur-to-note ((self chord) val)
  (loop for note in (inside self) do 
        (change-dur-to-note note val)))

(defmethod change-dur-to-note ((self t) val) t)

;;; new : changer la duree avec les touche R/L  

(defmethod change-x ((self scorepanel) dir)
  (when (translate-chords-p self)
    (all-chords-2-ms (object (om-view-container self)))
    (loop for item in (selection? self) do
          (change-offset-to-note item 
                                 (cond
                                  ((om-shift-key-p) (if (= dir 1) (grille-step self) (- (grille-step self)))) 
                                  (t (if (= dir 1) (round (grille-step self) 10) (- (round (grille-step self) 10)))))
                                 ))
    (normalize-chords-x (object (om-view-container self)))
    (update-panel self t)
    ))

(defmethod change-offset-to-note ((self container) val)
  (loop for item in (inside self) do
        (change-offset-to-note item val)))

(defmethod change-offset-to-note ((self chord) val)
  (change-chords-in-x nil (list self) val))

(defmethod change-offset-to-note ((self note) val)
  (om-beep)) 

(defmethod change-offset-to-note ((self t) val) 
  (om-beep))



;===============================================
;MINI EDITOR
;===============================================

(defclas cursor-editor () 
   ((x-pos :initform nil)
    (assoc-chord :initform nil)
    (assoc-staff :initform nil)
    (grap-measure-assoc :initform nil)
    (rythmic? :initform t)
    (pos-pitch :initform nil)))

(defmethod create-edit-cursor ((self scorePanel) assoc-obj x-pos pos-pitch rythmic? staffnum gma)
  (setf (selection? self) nil)
  (make-instance 'cursor-editor
    :x-pos x-pos
    :assoc-chord assoc-obj
    :assoc-staff staffnum
    :grap-measure-assoc gma
    :rythmic? rythmic?
    :pos-pitch pos-pitch))

(defun filtre-by-pre (list fun)
   (loop for item in list
         when (funcall fun item) collect item))




(defun char-is-figure (char)
   (or (equal char #\1) (equal char #\2) (equal char #\3) (equal char #\4) (equal char #\5)
       (equal char #\6) (equal char #\7) (equal char #\0)))

(defun char-is-digit (char)
   (and (not (or (equal char #\1) (equal char #\0)))
        (or (equal char #\8) (equal char #\9)  
            (char-is-figure char))))

(defmethod system-start-in-pix ((self scorePanel) pos) 
   (let* ((size (staff-size self))
          (pos&size (get-staff-pos&size (list! (staff-sys self)) (score-line-space self))))
     (round (* size (nth pos (cdr pos&size))))))

(defun get-the-measure (obj)
   (cond
    ((null obj) nil)
    ((measure-p obj) obj)
    ((poly-p obj) nil)
    ((voice-p obj) nil)
    (t (get-the-measure (parent obj)))))

;(object rythmic? staffnum)
(defmethod remake-edit-cursor ((self scorePanel)) 
   (let ((cur-ed (edit-cursor self)))
     (when (consp cur-ed)
       (let ((gmeasure (if (second cur-ed)
                         (get-correspond-grap (graphic-obj self)  (get-the-measure (first cur-ed)))))
             (gchord (get-correspond-grap (graphic-obj self) (first cur-ed)))
             (system (nth (third cur-ed) (list! (staff-sys self)))))
         (setf (edit-cursor self) 
               (create-edit-cursor self (first cur-ed) 
                                   (car (rectangle gchord)) (* 100 (midicenter system))
                                   (second cur-ed) (third cur-ed)  gmeasure))))))


(defmethod draw-edit-cursor ((self scorePanel) deltay) 
  (when (edit-cursor self)
     (remake-edit-cursor self)
     (let* ((system (nth (assoc-staff (edit-cursor self)) (list! (staff-sys self))))
            (size (staff-size self))
            (max (* 100 (second (range (first (staff-list system))))))
            (min (* 100 (car (range (car (last (staff-list system)))))))
            (h (+ size (get-system-size system size)))
            (x (x-pos (edit-cursor self)))
            (midic (pos-pitch (edit-cursor self)))
            (top (top-in-midi (staff-sys self)))
            (scale (get-approx-scale self))
            (off-y (round (* (posy (car (staff-list system))) (/ size 4))))
            (sysstart (system-start-in-pix self (assoc-staff (edit-cursor self))))
            (y0 (+ sysstart deltay (round size -2) ))
            pitch-pix auxlines alteration)
       (setf pitch-pix (- (+ deltay sysstart (midi2pixel midic top (round size 4) scale)) off-y))
       (setf alteration (second (give-alteration scale (approx-scale scale midic))))   
       (cond
        ((> midic max)
         (setf auxlines (get-aux-lines-midic (round midic 100) system top scale (round size 4) (+ (- pitch-pix deltay sysstart) off-y)))
         (setf h (+ h (round size 4) (round size 2) (abs (- pitch-pix y0))))
         (setf y0 (- pitch-pix (round size 4) (round size 2) sysstart)))
        ((< midic min)
         (setf auxlines (get-aux-lines-midic (round midic 100) system top scale (round size 4) (+ (- pitch-pix deltay sysstart) off-y)))
         (setf h (- (+ pitch-pix (round size 4) (round size 2)) y0 ))))

       (om-with-fg-color self (if (assoc-chord (edit-cursor self)) *om-green2-color* *om-red2-color*)
         (om-draw-rect (- x (round size 8)) y0 (round size 2) h))
       
       (om-with-fg-color self *om-gray-color*
         (om-draw-string x (- pitch-pix (round size 8)) (head-1/4)))
     
       (when alteration
        (om-with-fg-color self *om-gray-color*
          (om-with-font (om-make-music-font *micron-font* size)
                        (om-draw-char (- x (round size 3)) (- pitch-pix (round size 8)) alteration) )))
       (when auxlines
         (om-with-fg-color self  *system-color* 
           (let ((dir (car auxlines))
                 (topy (+ (- (+ deltay sysstart) (round size 8) off-y)  (second auxlines)))        
                 (limy (+ (- (+ deltay sysstart) (round size 8) off-y)  (third auxlines))))
             (if (equal dir 'dw)
               (progn
                 (setf topy (+ topy (round size 4))) 
                 (loop while (<= topy limy) do
                       (om-draw-line (- x (round size 8)) topy 
                                  (+ x (* (round size 8)) 3) topy)
                       (setf topy (+ topy (round size 4)))))
               (progn
                 (setf topy (- topy (round size 4))) 
                 (loop while (>= topy limy) do
                       (om-draw-line (- x (round size 8)) topy 
                                  (+ x (* (round size 8)) 3) topy)
                       (setf topy (- topy (round size 4))))))))))))



(defmethod move-editor-pitch ((self scorePanel) dir)
  (when (edit-cursor self)
    (setf (pos-pitch (edit-cursor self))
          (+ (pos-pitch (edit-cursor self)) 
             (cond
              ((om-shift-key-p) (if (= dir 0) 1200 -1200))
              ((om-command-key-p) (if (= dir 0) 700 -700))
              (t (let ((factor (approx-factor (get-current-scale (staff-tone self)))))
                   (if (= dir 0) factor (* -1 factor)))))))))




;ENTER
(defmethod enter-a-note ((self scorePanel)) 
   (when (and (edit-cursor self) (assoc-chord (edit-cursor self)) (chord-p (assoc-chord (edit-cursor self))))
     (let ((midic (pos-pitch (edit-cursor self)))
           (thechord (assoc-chord (edit-cursor self))))
       (setf (LMidic thechord) (concatenate 'list (LMidic thechord) (list midic)))
       (change-ties-too  self (assoc-chord (edit-cursor self))))))

;==========================================

(defun char-as-noir (char)
  (case char (#\7 4) (#\6 2) (#\5 1) (#\4 1/2) (#\3 1/4) (#\2 1/8)  (#\1 1/16) (#\0 1/32)))

(defun char-as-digit (char)
  (case char (#\9 9) (#\8 8) (#\7 7) (#\6 6) (#\5 5) (#\4 4) (#\3 3) (#\2 2) (#\1 1) (#\0 0)))

(defun find-next-bef-gobj (g-obj-list x)
   (let (rep bef)
     (loop for item in g-obj-list
           while (not rep) do
           (let ((x0 (first (rectangle item))))
             (if (<= x x0)
               (setf rep item)
               (setf bef item))))
     (list  bef rep)))

(defun less-deep-cont (c1 c2)
   (if (< (deep-cont c1) (deep-cont c2)) c1 c2))

(defun deep-cont (c )
   (if (null (parent c)) 0 (+ 1 (deep-cont  (parent c))))) 

(defun filtre-gcontchords (list)
  (loop for item in list
        when (not (cont-chord-p (reference item))) collect item))

(defmethod val-in-noires ((self list))
   (let ((aux (val-in-noires (reference (car self))))
         rep)
     (loop for item in (cdr self) do
           (let ((obj (reference item)))
             (cond
              ((cont-chord-p obj)
               (setf aux (+ aux (val-in-noires obj))))
              ((chord-p obj) 
               (push aux rep)
               (setf aux (val-in-noires obj)))
              ((rest-p obj)
               (push aux rep)
               (setf aux (val-in-noires obj))))))
     (push aux rep)
     (loop for item in  (reverse rep) collect (/ item 4))))


(defmethod val-in-noires ((self simple-container))
   (/ (extent self)  (qvalue self)))

(defmethod val-in-noires ((self rest))
   (* -1 (/ (extent self) (qvalue self))))

;==============
(defun replace-edit-cursor (self voice newobj)
  (setf newobj (position newobj (cons-chord&rest-list voice) :test 'equal))
  (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
  (when newobj
    (setf newobj (nth newobj (cons-chord&rest-list voice)))
    (when newobj
      (setf (edit-cursor self) (list newobj (rythmic? (edit-cursor self)) (assoc-staff (edit-cursor self))))))
  (unless newobj (setf (edit-cursor self) nil)))

(defmethod add-or-replace-in-measure ((self scorePanel) char) t)

(defmethod point-edit-cursor ((self scorePanel))
   (when (assoc-chord (edit-cursor self))
     (let* ((mtmode (meaure-edition-mode self))
            (measure (reference (grap-measure-assoc (edit-cursor self))))
            (assoc-chord (assoc-chord (edit-cursor self)))
            (newfig (* (/ (val-in-noires assoc-chord) 4) 3/2))
            (voice (parent measure))
            (container (parent assoc-chord))
            (chords (cons-gchord&rest-list (grap-measure-assoc (edit-cursor self))))
            (pos (position assoc-chord (loop for item in chords collect (reference item)) :test 'equal))
            newobj)
       (setf newobj (replace-ryth-chord measure mtmode container pos newfig ))
       (replace-edit-cursor self voice newobj)
       (update-panel self))))


(defmethod add-or-replace-in-measure ((self scorePanel) char)
   (let* ((mtmode (meaure-edition-mode self))
          (newfig (/ (char-as-noir char) 4))
          (measure (reference (grap-measure-assoc (edit-cursor self))))
          (assoc-chord (assoc-chord (edit-cursor self)))
          (voice (parent measure))
          (chords (cons-gchord&rest-list (grap-measure-assoc (edit-cursor self))))
          newobj container pos  bounds)
     (if (not assoc-chord)
       (progn
         (setf bounds (find-next-bef-gobj chords (x-pos (edit-cursor self))))
         (cond
          ((null (car bounds)) (setf container measure pos 0))
          ((null (second bounds)) 
           (setf container measure)
           (setf pos (length (inside container))))
          ((equal (parent (car bounds)) (parent (second bounds)))
           (setf container (reference (parent (car bounds))))
           (setf pos (position (reference (second bounds)) (inside container) :test 'equal)))
          (t (setf container (reference (parent (less-deep-cont (car bounds) (second bounds))))
                   pos (position (second bounds) chords :test 'equal)))))
       (progn (setf container (parent assoc-chord))
              (when (rest-p assoc-chord) (setf newfig (* -1 newfig)))
              (setf pos (position assoc-chord (loop for item in chords collect (reference item)) :test 'equal))))
     
     (setf newfig (if (equal container measure) newfig (/ newfig (qvalue container))))
     
     (setf newobj
           (if assoc-chord
             (replace-ryth-chord measure mtmode container pos newfig )
             (add-ryth-chord measure mtmode container  pos newfig (list (pos-pitch (edit-cursor self))))))
     (replace-edit-cursor self voice newobj)
     (update-panel self)))
     

(defun calc-new-position (container bounds)
  (cond
   ((null (car bounds)) 0)
   ((null (second bounds)) (length (inside container)))
   (t (let ((lpc (less-deep-cont (car bounds) (second bounds))))
        (if (equal lpc (second bounds)) 
          (position (second bounds) (inside (parent (second bounds))) :test 'equal)
          (+ (position (car bounds) (inside (parent (car bounds))) :test 'equal) 1))))))

(defun val-in-q (qvalue valinnoir)
  (* valinnoir qvalue ))

;========MODE 0

(defmethod add-ryth-chord ((self measure) (mtmode (eql 0)) container pos newval lmidic)
   (let* ((newchord (make-instance 'chord :Lmidic lmidic))
          (oldsign (car (tree self)))
          (newsign (+ (/ (first oldsign) (second oldsign)) newval))
          (newsign (list (numerator newsign) (denominator newsign))))
     (add-chord-to-container self container  pos newchord newsign newval)))

(defmethod replace-ryth-chord ((self measure) (mtmode (eql 0)) container pos newval )
   (let* ((newchord (nth pos (inside container)))
          (oldval (abs (val-in-noires newchord)))
          (newval (abs (* 4 newval)))
          (oldsign (car (tree self)))
          (delta (* (qvalue container) (- (abs newval) oldval)))
          (newext  newval)
          newsign)  
     (setf newsign (+ (/ (first oldsign) (second oldsign)) (/ (- (abs newval) oldval) 4)))
     (setf newsign (list (numerator newsign) (denominator newsign)))
     (setf (extent newchord) (numerator newext))
     (setf (qvalue newchord) (denominator newext) )
     (setf (extent container) (+ (extent container) delta))
     (setf (nth 0 (tree self)) newsign)
     (loop for i from (+ pos 1) to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     newchord))

(defmethod delete-ryth-chord ((self measure) (mtmode (eql 0)) container pos)
   (let* ((newchord (nth pos (inside container)))
          (oldval (abs (val-in-noires newchord)))
          (oldsign (car (tree self)))
          (delta (* (qvalue container) oldval))
          newsign)  
     (setf newsign (- (/ (first oldsign) (second oldsign)) (/ oldval 4)))
     (setf newsign (list (numerator newsign) (denominator newsign)))
     (setf (extent container) (+ (extent container) delta))
     (setf (inside container) (append (subseq (inside container) 0 pos) (subseq (inside container) (+ pos 1)))) 
     (setf (nth 0 (tree self)) newsign)
     (loop for i from pos to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     newchord))



(defun calcule-modif-vals (oldlist  dur newval) 
   (om* (/ dur (+ dur newval)) oldlist))

(defun filtre-rest-and-cont (list) 
   (loop for item in list 
         when (chord-p (reference item))
         collect (reference item)))

(defun filtre-cont (list) 
   (loop for item in list 
         when (not (cont-chord-p (reference item))) collect (reference item)))

;========MODE 1

(defmethod add-ryth-chord ((self measure) (mtmode (eql 1)) container pos newval lmidic)
   (let* ((newchord (make-instance 'chord :Lmidic lmidic))
          (newsign (car (tree self))))
     (add-chord-to-container self container  pos newchord newsign newval)))

(defmethod replace-ryth-chord ((self measure) (mtmode (eql 1)) container pos newval )
   (let* ((newchord (nth pos (inside container)))
          (oldval (abs (val-in-noires newchord)))
          (newval (abs (* 4 newval)))
          (newsign (car (tree self)))
          (delta (* (qvalue container) (- (abs newval) oldval)))
          (newext  newval))  
     (setf (extent newchord) (numerator newext))
     (setf (qvalue newchord) (denominator newext) )
     (setf (extent container) (+ (extent container) delta))
     (setf (nth 0 (tree self)) newsign)
     (loop for i from (+ pos 1) to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     newchord))


(defmethod delete-ryth-chord ((self measure) (mtmode (eql 1)) container pos)
   (let* ((newchord (nth pos (inside container)))
          (oldval (abs (val-in-noires newchord)))
          (delta (* (qvalue container) oldval)))  
     (setf (inside container) (append (subseq (inside container) 0 pos) (subseq (inside container) (+ pos 1)))) 
     (loop for i from pos to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     newchord))


;========MODE 2
(defmethod add-ryth-chord ((self measure) (mtmode (eql 2)) container pos newval lmidic)
   (let* ((newchord (make-instance 'chord :Lmidic lmidic))
          (newsign (car (tree self)))
          (delta (* 4 (qvalue container) newval))
          (newext  newval))
     (setf (offset newchord) (offset (nth pos (inside container))))
     (set-val-from-noire newchord newext)
     (setf (parent newchord) container)
     (setf (inside container) (insert-in-list (inside container) newchord pos))
     (loop for i from (+ pos 1) to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     (cut-a-gauche self (/ (first newsign) (second newsign)))
     newchord))

(defmethod replace-ryth-chord ((self measure) (mtmode (eql 2)) container pos newval )
   (let* ((newchord (nth pos (inside container)))
          (oldval (abs (val-in-noires newchord)))
          (newval (abs (* 4 newval)))
          (newsign (car (tree self)))
          (delta (*  (qvalue container) (- (abs newval) oldval)))
          (newext  (/ newval 4)))
     (set-val-from-noire newchord newext)
     (loop for i from (+ pos 1) to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     (cut-a-gauche self (/ (first newsign) (second newsign)))
     newchord))

(defmethod delete-ryth-chord ((self measure) (mtmode (eql 2)) container pos)
   (let* ((newchord (nth pos (inside container)))
          (oldval (abs (val-in-noires newchord)))
          (delta (* (qvalue container) oldval)))  
     (setf (inside container) (append (subseq (inside container) 0 pos) (subseq (inside container) (+ pos 1)))) 
     (loop for i from pos to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     
     (setf newchord (make-instance 'rest))
     (setf (offset newchord) (- (extent self) oldval))
     (setf (extent newchord) (numerator oldval))
     (setf (qvalue newchord) (denominator oldval))
     (setf (parent newchord) self)
     (setf (Qtempo newchord) (Qtempo self))
     (setf (inside self) (append (inside self) (list newchord)))))

(defmethod add-chord-to-container ((self measure) container pos newchord newsign newval)
   (let ((delta (* 4 (qvalue container) newval)))
     (setf (offset newchord) (offset (nth pos (inside container))))
     (set-val-from-noire newchord newval)
     (setf (parent newchord) container)
     (setf (extent container) (+ (extent container) delta))
     (setf (nth 0 (tree self)) newsign)
     (setf (inside container) (insert-in-list (inside container) newchord pos))
     (loop for i from (+ pos 1) to (- (length (inside container)) 1) do
           (setf (offset (nth i (inside container))) (+ delta (offset (nth i (inside container))))))
     newchord))


(defmethod cut-a-gauche ((self container) maxdur)
   (let* ((count 0) (continue t) newlist)
     (setf newlist
           (loop for item in (inside self)
                 while continue collect
                 (let ((duritem (abs (/ (val-in-noires item) 4))))
                   (if (>= (+ duritem count) maxdur)
                     (progn
                       (setf continue nil)
                       (cut-a-gauche item (- maxdur count)))
                     (progn
                       (setf count (+ count duritem))
                       item)))))
     (setf (inside self) newlist)
     self))

(defmethod cut-a-gauche ((self chord) maxdur)
   (set-val-from-noire self maxdur) self)

(defmethod cut-a-gauche ((self rest) maxdur)
   (set-val-from-noire self maxdur) self)

(defun set-val-from-noire (self maxdur)
   (setf maxdur (* 4 maxdur))
   (setf (extent self) (numerator maxdur))
   (setf (qvalue self) (denominator maxdur)))



;=================================================

(defmethod do-subdivise ((self scorePanel) char) t)

(defmethod do-subdivise ((self voicePanel) char)
   (let* ((subdiv (char-as-digit char))
          (chords (loop for item in (selection? self)
                        append (cons-chord&rest-list item)))
          (voice (object (om-view-container self))))
     (loop for item in chords do
           (subdivise-figure  item subdiv))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
     (setf (selection? self) nil)
     (update-panel self t)))

(defmethod do-subdivise ((self polyPanel) char)
   (let* ((subdiv (char-as-digit char))
          (chords (loop for item in (selection? self)
                        append (cons-chord&rest-list item)))
          (poly (object (om-view-container self))) voices)
     (loop for item in chords do
           (push (get-the-voice item) voices)
           (subdivise-figure  item subdiv))
     (loop for voice in (remove-duplicates voices :test 'equal) do
           (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice)))
     (setf (selection? self) nil)
     (update-panel self t)))


(defmethod subdivise-figure ((self t) subdiv) t)

(defmethod subdivise-figure ((self chord) subdiv)
   (let* ((pere (parent self))
          (pos (position self (inside pere) :test 'equal))
          (newgroup (make-instance 'group 
                      :tree (list (extent self) 
                                  (create-list subdiv (if (cont-chord-p self) 1.0 1))))))
     (setf (offset newgroup) (offset self)
           (qvalue newgroup) (qvalue self)
           (extent newgroup) (extent self)
           (parent newgroup) pere)
     (loop for item in (inside newgroup) do
           (setf (Lmidic item) (Lmidic self)
                 (LVel item) (LVel self)
                 (LOffset item) (LOffset self)
                 (LChan item) (LChan self)))
     (setf (nth pos (inside pere)) newgroup)
     (reverse (cons-container-path (car (inside newgroup))))))

(defmethod subdivise-figure ((self rest) subdiv)
   (let* ((pere (parent self))
          (pos (position self (inside pere) :test 'equal))
          (newgroup (make-instance 'group :tree (list (extent self) (create-list subdiv -1)))))
     (setf (offset newgroup) (offset self)
           (qvalue newgroup) (qvalue self)
           (extent newgroup) (extent self)
           (parent newgroup) pere)
     (setf (nth pos (inside pere)) newgroup)
     (reverse (cons-container-path (car (inside newgroup))))))

;===========================
(defun vocie-next-container (obj chords)
  (let ((pos (position obj chords :test 'equal)))
    (when pos
      (nth (+ pos 1) chords))))

(defun vocie-prev-container (obj chords)
  (let ((pos (position obj chords :test 'equal)))
    (when (and pos (not (= pos 0)))
      (nth (- pos 1) chords))))

(defun next-real-chord (obj)
  (let* ((voice (get-the-voice obj))
         (chords (cons-chord&rest-list voice))
         (next (vocie-next-container obj chords)))
    (loop while (and next (cont-chord-p next)) do
          (setf next (vocie-next-container next chords)))
    next))

(defun previous-real-chord (obj)
  (let* ((voice (get-the-voice obj))
         (chords (cons-chord&rest-list voice))
         (next (vocie-prev-container obj chords)))
    (loop while (and next (cont-chord-p next)) do
          (setf next (vocie-prev-container next chords)))
    next))

(defmethod advance-edit-cursor ((self scorePanel) dir)
   (let* ((cur (assoc-chord (edit-cursor self)))
          next)
     (if cur (setf next (if (= dir 1)
                          (next-real-chord cur )
                          (previous-real-chord cur)))
         (let* ((chords (cons-gchord&rest-list (grap-measure-assoc (edit-cursor self))))
                (bounds (find-next-bef-gobj chords (x-pos (edit-cursor self)))))
           (setf next (if (= dir 1)
                        (next-real-chord (reference (first bounds)))
                        (previous-real-chord (reference (second bounds)))))))
         (if next
           (progn
             (setf (edit-cursor self) (list next (rythmic? (edit-cursor self)) (assoc-staff (edit-cursor self))))
             (om-invalidate-view self t))
           (om-beep))))

(defmethod create-new-edit-cursor ((self scorePanel))
   (let* ((cur (assoc-chord (edit-cursor self)))
          (size (staff-size self))
          next new-x whichmeasure gchord)
     (when cur
       (setf next  (next-real-chord cur))
       (when next
       (setf new-x (if next
                     (progn
                       (setf gchord (get-correspond-grap (graphic-obj self)  next))
                       (- (car (rectangle gchord)) (round size 4)))
                     (+ (x-pos (edit-cursor self)) (round size 4))))
       
       (setf whichmeasure (if next
                            (get-the-gmeasure gchord)
                            (grap-measure-assoc (edit-cursor self))))
       
       (setf (edit-cursor self) 
             (create-edit-cursor self nil new-x 
                                 (pos-pitch (edit-cursor self))
                                 (rythmic? (edit-cursor self))
                                 (assoc-staff (edit-cursor self)) whichmeasure))
       (om-invalidate-view self t)))))

(defun get-the-gmeasure (obj)
   (cond
    ((null obj) nil)
    ((grap-measure-p obj) obj)
    (t (get-the-gmeasure (parent obj)))))


;=================== DO-GROUP


(defmethod do-group ((self scorePanel)) (om-beep))

(defmethod do-group ((self voicePanel))
   (om-beep-msg "Group chords not implemented"))

(defmethod do-group ((self polyPanel))
   (om-beep-msg "Group chords not implemented"))


(defmethod do-group ((self chordseqPanel))    
  (group-a-chs (object (om-view-container self)) (selection? self))
  (setf (selection? self) nil)
  (update-panel self t))

(defmethod do-group ((self multiseqPanel)) nil)


;=================== UN-GROUP

(defmethod un-group ((self scorePanel)) (om-beep))


(defmethod un-group ((self voicePanel))
   (let ((obj (grap-class-from-type  (obj-mode self)))
         (gobj (graphic-obj self))
         (voice (object (om-view-container self))))
     (cond
      ((equal obj 'grap-group)
       (loop for item in (selection? self) do
             (free-group item gobj))
       (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
       (setf (selection? self) nil)
       (update-panel self t))
      (t (om-beep)))))


(defmethod un-group ((self polyPanel))
   (let ((obj (grap-class-from-type  (obj-mode self)))
         (gobj (graphic-obj self)) voices)
     (cond ((equal obj 'grap-group)
            (loop for item in (selection? self) do
                  (push (get-the-voice item) voices) 
                  (free-group item gobj))
            (loop for voice in (remove-duplicates voices :test 'equal) do
                  (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice)))
            (setf (selection? self) nil)
            (update-panel self t))
           (t (om-beep)))))


(defmethod free-group ((self group) gobj)
   (let* ((ggroup (get-correspond-grap gobj  self))
          (pere (parent self))
          (pos (position self (inside pere) :test 'equal)))
     (unless (numdenom ggroup)
       (change-qvalue pere (qvalue self) (qvalue pere)) 
       (setf (inside pere) (remove self (inside pere) :test 'equal))
       (loop for item in (inside self) do
             (setf (offset item) (+ (offset item) (offset self))
                   (parent item) pere)
             (setf (inside pere) (insert-in-list (inside pere) item pos))
             (setf pos (+ pos 1))))))
;========
(defmethod change-signature ((panel scorepanel) (self measure) sign)
   (let ((voice (parent self)) pos) 
     (setf (nth 0 (tree self)) sign)
     (setf pos (position self (inside voice) :test 'equal))
     (setf (tree voice) (check-tree-for-contchord (build-tree voice) voice))
     (when pos
       (setf (selection? panel) (list (nth pos (inside voice)))))
     (setf (edit-cursor panel) nil)
     (update-panel panel t)
     (update-inspector (editor panel) pos)
     ))



;===============================
;TIME conversion space vs time
;===============================

(defmethod cons-the-bpf-time ((self notePanel) graph-obj) t)

(defmethod cons-the-bpf-time ((self scorePanel) graph-obj) 
  (setf (timebpf self) (make-score-bpf-time self (collect-bpftime-objects graph-obj (reference graph-obj) (staff-size self))))
  ;(setf *a* (timebpf self))
  )

(defmethod make-score-bpf-time ((self scorePanel) list)
  (when list
  (let (lx ly)
    (setf list (sort (remove-duplicates list :key 'first) '< :key 'first))
    (loop for item in list do
          (push (first item) lx)
          (push (second item) ly))
    (simple-bpf-from-list (reverse (cons (+ 1000 (car lx)) lx))  (reverse (cons (+ 20 (car ly)) ly))))))


(defmethod get-x-pos ((self t) (time number) zoom)
  (get-x-pos self (round time) zoom))


(defmethod get-x-pos ((self scorepanel) (time-ms integer) zoom)
  (let ((rep (if (timebpf self)
                 (let ((max (car (last (x-points (timebpf self)))))
                       (min (car (x-points (timebpf self)))))
                   (cond
                    ((> time-ms max) (round (+ (bpf-get-val (timebpf self) max) (round (- time-ms max) (staff-size self)))))
                    ((< time-ms min) (round (- (bpf-get-val (timebpf self) min) (round (- min time-ms) (staff-size self)))))
                    (t (round (bpf-get-val (timebpf self) time-ms)))))
               (round time-ms (staff-size self)))))
    (+ (get-key-space self) (staff-size self) (* zoom rep))))



(defmethod get-ms-pos ((self scorePanel) (x-pos integer) zoom)
  ;(print (list self x-pos))
  ;(setf *bbb* (timebpf self))
  (let ((rep (if (timebpf self)
                 (let ((max (car (last (y-points (timebpf self)))))
                       (min (car (y-points (timebpf self)))))
                   (cond
                    ((> x-pos max) (round (+ (car (y-transfer (timebpf self) max 0)) (* (- x-pos max) (staff-size self)))))
                    ((< x-pos min) (max 0 (round (- (car (y-transfer (timebpf self) min 0)) (* (- min x-pos) (staff-size self))))))
                    (t (round (car (y-transfer (timebpf self) x-pos 0))))))
               (round (* x-pos (staff-size self))) ;;; ???
               )
             ))
    rep
    ))

(defmethod get-ms-pos ((self voicepanel) (x-pos integer) zoom)
  (rythmpanel-mspos self x-pos zoom))

(defmethod get-ms-pos ((self polypanel) (x-pos integer) zoom)
  (rythmpanel-mspos self x-pos zoom))

(defun rythmpanel-mspos (self x-pos zoom)
  (when (timebpf self)
    (let* ((xmax (car (last (y-points (timebpf self)))))
           (xmin (car (y-points (timebpf self))))
           (xx (- x-pos (- (get-x-pos self 0 1) xmin)))
           (y-pos-in-timebpf 
                    (cond 
                     ((> xx xmax) (round (+ (last-elem (x-points (timebpf self))) (* (- xx xmax) (staff-size self)))))
                     ((< xx xmin) (max 0 (round (- (car (x-points (timebpf self))) (* (- xmin xx) (staff-size self))))))
                     (t (round (car (y-transfer (timebpf self) xx 0)))))))
      
      (round (/ y-pos-in-timebpf zoom))
      )))


;===================================
;Do Undo
;===================================

(defmethod handle-key-event :before ((self scorePanel) char)
  (when (or (char-is-digit char) (member char '(:om-key-delete 
                                                :om-key-up 
                                                :om-key-down 
                                                :om-key-left 
                                                :om-key-right 
                                                :om-key-esc 
                                                #\t #\T #\t #\+ #\* #\- #\= #\/)))
    (record-undo (editor self))
    ))

(defmethod record-undo ((self t)) nil)

(defmethod record-undo ((self scoreeditor)) 
  (setf (undo self) (clone (object self))))
   


(defun check-undo () *undo*)

(defmethod score-drag&drop :before ((D&DHandler omdrag-drop))
  (let ((editor (target-view  D&DHandler)))
    (record-undo editor)))

(defmethod add-new-object :before ((self scorePanel) obj where graph-obj)
  (when (editor self)
    (record-undo (editor self))))

(defmethod do-undo ((self scoreeditor)) 
  (when (undo self)
    (let ((obj (clone (object self))))
      ;(setf (inside (object self)) (inside (undo self)))
      (setf (object self) (undo self))
      (set-value (ref self) (object self))
      (off-selection (panel self))
      (setf (selection? (panel self)) nil)
      (update-panel (panel self) t)
      (setf (undo self) obj))
  ))

(defmethod set-value ((self OMBox) val)
  (setf (value self) val))

(defmethod set-value ((self OMInstance) val)
  (setf (instance self) val))


;(defmethod do-undo ((self noteeditor))
;  (when (undo self)
;    (let ((obj (clone (object self))))
;      (setf (midic (object self)) (midic (undo self))
;            (vel (object self)) (vel (undo self))
;            (dur (object self)) (dur (undo self))
;            (chan (object self)) (chan (undo self))
;            (port (object self)) (port (undo self)))
;      (update-panel (panel self) t)
;      (setf (undo self) obj))
;  ))


;;;=================================
;;; COPY/PASTE
;;;=================================

(defvar *score-clipboard* nil)

(defmethod editor-copy ((self scoreeditor))
  (setf *score-clipboard* (clone (selection? (panel self)))))

(defmethod editor-cut ((self scoreeditor))
  (record-undo self) 
  (setf *score-clipboard* (clone (selection? (panel self))))
  (delete-selection (panel self)))

(defmethod editor-paste ((self scoreeditor))
  (let ((newobj (make-obj-for-target *score-clipboard* (object self))))
    (when newobj
      (record-undo self)
      (add-obj-in-editor newobj self)
      (update-panel (panel self) t))))

(defmethod make-obj-for-target (obj target) (om-beep))
(defmethod add-obj-in-editor ((obj t) (editor t)) (om-beep))


;; paste in NOTE editor
(defmethod make-obj-for-target ((obj note) (target note))
  (clone obj))

(defmethod make-obj-for-target ((obj list) (target note))
  (when (and (= (length obj) 1)
             (list-subtypep obj 'note))
    (make-obj-for-target (car obj) target)))


(defmethod add-obj-in-editor ((obj note) (editor noteeditor))
  (let ((editor-obj (object editor)))
    (setf (midic editor-obj) (midic obj)
          (vel editor-obj) (vel obj)
          (dur editor-obj) (dur obj)
          (chan editor-obj) (chan obj)
          (port editor-obj) (port obj))
    editor-obj))
          
;; paste in CHORD editor
(defmethod make-obj-for-target ((obj note) (target chord))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj chord) (target chord))
  (clone (inside obj)))

(defmethod make-obj-for-target ((obj list) (target chord))
  (inside (objfromobjs obj (make-instance 'chord))))

(defmethod add-obj-in-editor ((obj list) (editor chordeditor))
  (let ((editor-obj (object editor)))
    (setf (inside editor-obj) (append (inside editor-obj) obj))
    editor-obj))
          
;; paste in CHORD-SEQ editor
(defmethod om-score-click-handler :before ((self chordseqPanel) where double-click-p)
  (setf (clic-pos self) where))

(defmethod make-obj-for-target ((obj note) (target chord-seq))
  (list (objfromobjs obj (make-instance 'chord))))

(defmethod make-obj-for-target ((obj chord) (target chord-seq))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj chord-seq) (target chord-seq))
  (clone (inside obj)))

(defmethod make-obj-for-target ((obj list) (target chord-seq))
  (or (sort (remove nil (loop for item in obj append (make-obj-for-target item target))) '< :key 'offset)
      (om-beep)))


(defmethod add-obj-in-editor ((obj list) (editor chordseqeditor))
  (let ((editor-obj (object editor))
        (onset (offset (car obj))))
    (if (clic-pos (panel editor)) (mapcar #'(lambda (chord) 
                                              (setf (offset chord)
                                                    (+ (max 0 (pixel-toms (panel editor) (clic-pos (panel editor))))
                                                       (- (offset chord) onset))))
                                          obj))
    (setf (inside editor-obj) (append (inside editor-obj) obj))
    editor-obj))

;; paste in MULTI-SEQ editor
(defmethod make-obj-for-target ((obj chord-seq) (target multi-seq))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj multi-seq) (target multi-seq))
  (clone (inside obj)))

(defmethod make-obj-for-target ((obj voice) (target multi-seq)) nil)
(defmethod make-obj-for-target ((obj poly) (target multi-seq)) nil)

(defmethod make-obj-for-target ((obj list) (target multi-seq)) 
  (remove nil (loop for item in obj append (make-obj-for-target item target))))

(defmethod add-obj-in-editor ((obj list) (editor multiseqeditor))
  (let ((editor-obj (object editor))
        (ind (if (clic-pos (panel editor))
                 (1+ (click-in-which-voice? (panel editor) (clic-pos (panel editor))))
               (length (inside (object editor))))))
    (change-multi-inside (panel editor) (append (clone (subseq (inside editor-obj) 0 ind)) obj (clone (subseq (inside editor-obj) ind))))
    editor-obj))


;; paste in POLY editor
(defmethod make-obj-for-target ((obj voice) (target poly))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj poly) (target poly))
  (clone (inside obj)))

(defmethod make-obj-for-target ((obj list) (target poly)) 
  (remove nil (loop for item in obj append (make-obj-for-target item target))))


;; paste in VOICE editor

(defmethod make-obj-for-target ((obj note) (target voice))
  (list (objfromobjs obj (make-instance 'chord))))

(defmethod make-obj-for-target ((obj chord) (target voice))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj group) (target voice))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj rest) (target voice))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj measure) (target voice))
  (list (clone obj)))

(defmethod make-obj-for-target ((obj voice) (target voice))
  (clone (inside obj)))

(defmethod make-obj-for-target ((obj list) (target voice))
  (or (sort (remove nil (loop for item in obj append (make-obj-for-target item target))) '< :key 'offset)
      (om-beep)))

;;; 2 cas: liste de mesures ou liste de chords/groups

(defmethod add-obj-in-editor ((obj list) (editor voiceeditor))
  (let* ((editor-obj (object editor))
        (clicked-mes (or (and (clic-pos (panel editor)) 
                              (position (click-in-grap-measure? (graphic-obj (panel editor)) (clic-pos (panel editor)))
                                        (inside (graphic-obj (panel editor))) :test 'equal))
                         (1- (length (inside editor-obj))))))
    (cond ((list-subtypep obj 'measure)
           (when (and (selection? (panel editor)) (list-subtypep (selection? (panel editor)) 'measure))
             (setf clicked-mes (1- (position (car (selection? (panel editor))) (inside editor-obj) :test 'equal)))
             (delete-selection (panel editor)))
           (loop for item in obj do
                 (insert-measure-in-voice editor-obj item (incf clicked-mes))))
          ((selection? (panel editor))
           (if (or (> 1 (length (selection? (panel editor)))))
               (om-beep)
             (cond ((member (type-of (car (selection? (panel editor)))) '(chord rest group) :test 'subtypep)
                    (let ((newmes (make-instance 'measure)))
                      (setf (inside newmes) obj)
                      (insert-measure-in-voice editor-obj newmes (incf clicked-mes))
                      (setf (tree editor-obj) (build-tree editor-obj))))
                   
                   ((typep (car (selection? (panel editor))) 'measure)
                    (setf (inside (car (selection? (panel editor)))) obj)
                    (setf (tree editor-obj) (build-tree editor-obj)))
                   
                   (t (om-beep))))))
    editor-obj))


(defmethod object-order ((self chordeditor)) '("note" "chord"))
(defmethod object-order ((self noteeditor)) '("note"))
(defmethod object-order ((self chordseqeditor)) '("note" "chord" "chord-seq"))
(defmethod object-order ((self multiseqeditor)) '("note" "chord" "chord-seq" "multi-seq"))
(defmethod object-order ((self voiceeditor)) '("note" "chord" "group" "measure" "voice"))
(defmethod object-order ((self polyeditor)) '("note" "chord" "group" "measure" "voice" "poly"))



;===================================
; TOOLS : NEEDS CLEANUP
;===================================

(defmethod time-to-pixels ((view scorepanel) time-ms)
  (+ (get-key-space view) (ms2pixel time-ms (/ (staff-size view) 4) (staff-zoom view))))

(defmethod time-to-pixels ((view voicepanel) time-ms)
  (get-x-pos view time-ms (staff-zoom view)))

(defmethod time-to-pixels ((view polypanel) time-ms)
  (get-x-pos view time-ms (staff-zoom view)))

(defmethod pixels-to-time ((view scorepanel) pix)
  (pixel2ms (- pix (get-key-space view)) 
            (/ (staff-size view) 4) (staff-zoom view)))

(defmethod pixels-to-time ((view voicepanel) pix)
  (get-ms-pos view pix (staff-zoom view)))

(defmethod pixels-to-time ((view polypanel) pix)
  (get-ms-pos view pix (staff-zoom view)))


;===================================
; PRINT
;===================================


(defvar *print-score-header* t)
(defmethod print-score-header ((self scorePanel) y pg-no howmany)
 (when *print-score-header*
   (let ((obj (object (editor self))))
     (om-draw-string 0 y (format nil "OpenMusic----- page ~D/~D -----~D"  pg-no howmany (system::date-string))))))


(defmethod om-print-view ((self scorePanel) pg-size pg-no hm )
  (let* ((picts (score-picts-list self)))
    (om-draw-picture self (nth (- pg-no 1) picts) :size pg-size)))

(defmethod om-print-view ((self scoreeditor) pg-size pg-no hm ) 
  (om-print-view (panel self) pg-size pg-no hm ))

(defvar *score-printing* nil)
(defvar *old-print-mode* t)


(defmethod om-compute-page-number ((view scoreeditor) page-size ) 
  (let* ((self (panel view))
         (*internal-score-fonts* (init-fonts-to-draw (staff-size self)))
         (*old-print-mode* (score-page-mode self))
         (linespace (/ (staff-size self) 4))
         (*score-printing* t) numpages)
    (unless (score-page-mode self)
      (set-score-page-mode self t)
      (make-pages-form-obj self (objectfromeditor self) 0
                           120 linespace 
                           (staff-mode self)
                           (get-approx-scale self)
                           (selection? self) (staff-sys self) (show-stems self) )
      (set-score-page-mode self *old-print-mode*))
    (length (score-picts-list self))))



;;;=====================

(defmethod collect-page-all-line-elements ((self multiseqPanel) grap-obj fdoc pagenum line)
  (loop for chord-seq in (inside grap-obj)
          for voice = 0 then (+ voice 1) append
          (get-page-line-elements  chord-seq fdoc pagenum line voice)))

(defmethod collect-page-all-line-elements ((self polyPanel) grap-obj fdoc pagenum line)
 (loop for voice in (inside grap-obj)
          for i = 0 then (+ i 1) append
          (get-page-line-elements voice fdoc pagenum line 0)))

