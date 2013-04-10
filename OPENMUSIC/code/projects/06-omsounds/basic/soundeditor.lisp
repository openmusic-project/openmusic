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

(defmethod make-editor-window ((class (eql 'soundEditor)) object name ref &key 
                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
                               (wintype nil))
  (call-next-method class object (if (om-sound-file-name object)
                                     (namestring (om-sound-file-name object))
                                   "empty sound")
                                   ref :winsize winsize :winpos winpos :resize resize 
                    :close-p close-p :winshow winshow :resize resize
                    :retain-scroll retain-scroll :wintype wintype
                    ))

     
;============= CONTROLS ===========
(omg-defclass Aiff-control (3dBorder-view) 
  ((dyn-ctrls-list :initform nil :accessor dyn-ctrl-list)))


(defmethod make-snd-ctrl-list ((self Aiff-control))
  (let ((x0 260)
        (snd (object (om-view-container self)))
        (sndpanel (panel (om-view-container self))))
    (list 
     (om-make-dialog-item 'numBox
                          (om-make-point x0 8)
                          (om-make-point 28 18) (format () " ~D" (tracknum (object (om-view-container self))))
                          :min-val 1
                          :max-val 32
                          :font *om-default-font1*
                          :bg-color *om-white-color*
                          :value (tracknum (object (om-view-container self)))
                          :afterfun #'(lambda (item)
                                        (let ()
                                          (if (eq (oa::assoc-player snd) *audio-player-visible*)
                                            (oa::om-smart-stop snd sndpanel))
                                          (setf (tracknum (object (om-view-container self))) (- (value item) 1)) 
                                          (report-modifications (om-view-container self))))
                          )
     ;(om-make-dialog-item 'numBox
     ;                     (om-make-point (incf x0 70) 8)
     ;                     (om-make-point 40 18) (format () " ~D" (vol (object (om-view-container self))))
     ;                     :min-val 0
     ;                     :max-val 100
     ;                     :bg-color *om-white-color*
     ;                     :font *om-default-font1*
     ;                     :value (vol (object (om-view-container self)))
     ;                     :afterfun #'(lambda (item)
     ;                                   (setf (vol (object (om-view-container self))) (value item))
     ;                                   (report-modifications (om-view-container self)))
     ;                     )
     ;(om-make-dialog-item 'numBox
     ;                     (om-make-point (incf x0 90) 8)
     ;                     (om-make-point 40 18) (format () " ~D" (pan (object (om-view-container self))))
     ;                     :min-val -100
     ;                     :max-val 100
     ;                     :bg-color *om-white-color*
     ;                     :font *om-default-font1*
     ;                     :value (pan (object (om-view-container self)))
     ;                     :afterfun #'(lambda (item)
     ;                                   (setf (pan (object (om-view-container self))) (value item))
     ;                                   (report-modifications (om-view-container self)))
     ;                     )
     (om-make-dialog-item 'om-pop-up-dialog-item 
                                            (om-make-point (incf x0 390) 5) 
                                            (om-make-point 100 20) ""
                                            :font *om-default-font1*
                                            :range (mapcar 'audio-player-name *audio-players*)
                                            :value (audio-player-name (get-edit-param (om-view-container self) 'player))
                                            :di-action  (om-dialog-item-act item 
                                                          (change-player (panel (om-view-container self)) 
                                                                         (nth (om-get-selected-item-index item) *audio-players*)))
                                            )
     )))



(defmethod add-sound-params ((self Aiff-control))
  (let* ((x0 220)
        ;(snd (object (om-view-container self)))
         (editor (om-view-container self))
         (sndpanel (panel editor))) 
    (setf (dyn-ctrl-list self) (make-snd-ctrl-list self))
    
    (om-add-subviews self
                     (om-make-view 'om-icon-button :position (om-make-point (- x0 210) 5) :size (om-make-point 20 20)
                                         :icon1 "simple_play" :icon2 "simple_play_pushed"
                                         :action #'(lambda (item) (editor-play editor))

                     (om-make-view 'om-icon-button :position (om-make-point (- x0 185) 5) :size (om-make-point 20 20)
                                         :icon1 "simple_pause" :icon2 "simple_pause_pushed"
                                         :action #'(lambda (item) (oa::om-smart-pause snd sndpanel)))

                     (om-make-view 'om-icon-button :position (om-make-point (- x0 160) 5) :size (om-make-point 20 20)
                                         :icon1 "simple_stop" :icon2 "simple_stop_pushed"
                                         :action #'(lambda (item) (oa::om-smart-stop snd sndpanel)))

                     (om-make-dialog-item 'om-check-box (om-make-point (- x0 90) 4)
                               (om-make-point 130 20) "Send to track :"
                               :checked-p (if (oa::assoc-player snd) (if (eq (oa::assoc-player snd) oa::*audio-player-hidden*) nil t) nil)
                               :di-action (om-dialog-item-act item (let ()
                                                                     (oa::om-smart-stop snd sndpanel) 
                                                                     (oa::om-send-to-track sndpanel))))

                     (om-make-dialog-item 'om-check-box (om-make-point (+ x0 130) 4)
                               (om-make-point 170 20) "Use Original Sound"
                               :checked-p (if (or (= -1 (oa::current-is-original snd)) 
                                                  (= 0 (oa::current-is-original snd))) nil t)
                               :di-action (om-dialog-item-act item (let ()
                                                                     (oa::om-use-original-sound sndpanel))))

                     ;(om-make-dialog-item 'om-static-text (om-make-point (incf x0 80) 8) (om-make-point 40 20)
                     ;                     "Vol" :font *om-default-font1* :bg-color *controls-color*)

                     ;(om-make-dialog-item 'om-static-text (om-make-point (incf x0 85) 8) (om-make-point 40 20)
                     ;                     "Pan" :font *om-default-font1* :bg-color *controls-color*)

                     (om-make-dialog-item 'om-static-text (om-make-point (incf x0 380) 8) (om-make-point 50 20)
                                          "Player" :font *om-default-font1*)
                     
                     (first (dyn-ctrl-list self))
                     (second (dyn-ctrl-list self))
                     ;(third (dyn-ctrl-list self))
                     ;(fourth (dyn-ctrl-list self))
                     )
    t))





(defmethod update-controls ((self Aiff-control))
  (loop for item in (dyn-ctrl-list self) do (om-remove-subviews self item))
  (setf (dyn-ctrl-list self) (make-snd-ctrl-list self))
  (loop for item in (dyn-ctrl-list self) do (om-add-subviews self item))  
  t)


;== RULERS ==========

(omg-defclass static-ruler (ruler) () )

(defmethod strech-ruler-motion ((self static-ruler) pos) t)

(defmethod strech-ruler-release ((view static-ruler) pos) t)

(defmethod om-view-cursor ((self static-ruler)) nil)


(omg-defclass sound-ruler (ruler) ())

(defmethod strech-ruler-motion ((self sound-ruler) pos) 
  (call-next-method))

(defmethod strech-ruler-release ((self sound-ruler) pos) 
  (call-next-method)
  (let ((size (- (cadr (rangex (assoc-view self))) (car (rangex (assoc-view self))))))
    (when (> (cadr (rangex (assoc-view self))) (cadr (bounds-x (assoc-view self))))
      ;(om-beep)
      (setf (rangex (assoc-view self))
            (list 
             (max 0 (- (cadr (bounds-x (assoc-view self))) size))
             (cadr (bounds-x (assoc-view self)))
             ))
      ))
  (update-subviews (om-view-container self))
  )


;=========PREVIEW===========

(defclass full-preview (3Dborder-view om-view-cursor-play) ()
     (:default-initargs
      #+win32 :draw-with-buffer #+win32 t)
     )

(defvar *preview-size* "size of the preview pane")
(setf *preview-size* 40)

(defmethod draw-line-cursor ((self full-preview) &key newpixel (draw? t))
  (unless newpixel
    (setf newpixel (round (* (/ (cursor-pos *general-player*) (cadr (bounds-x (panel (om-view-container self))))) (w self)))))
  (when draw? 
    (om-update-movable-cursor self newpixel 0 4 (h self)))
   newpixel)

(defmethod om-draw-contents ((self full-preview))  
  (call-next-method)
  (if (and (om-view-container self) (sndpict (om-view-container self)))
    (let* ((panel (panel (om-view-container self)))
           (dur (cadr (bounds-x panel))))
      (unless (zerop dur)
        (om-with-focused-view self
          (om-draw-picture self (sndpict (om-view-container self)) (om-make-point 0 0) (om-view-size self))
          (om-with-fg-color self *om-steel-blue-color*
            (om-draw-rect (round (* (w self) (/ (car (rangex panel)) 
                                                dur)))
                          2
                          (round (* (w self) (/ (- (cadr (rangex panel))  (car (rangex panel)))
                                                dur)))
                          (- (h self) 5)))
          (om-with-fg-color self *om-gray-color*
            (om-with-dashline
              (loop for item in (markers (object (om-view-container self)))
                    for k = 0 then (+ k 1) do
                    (om-draw-line (round (* (w self) item 1000) dur) 0 (round (* (w self) item 1000) dur) (h self))
                    )))
          (om-with-fg-color self *om-red2-color*
            (om-draw-line (round (* (w self) (cursor-pos panel)) dur) 0 
                          (round (* (w self) (cursor-pos panel)) dur) (h self)))
          (when (cursor-interval panel)
            (draw-h-rectangle (list (round (* (w self) (car (cursor-interval panel))) dur) 0 
                                    (round (* (w self) (cadr (cursor-interval panel))) dur) (h self)) t t))
          ))
      )
    (om-with-focused-view self 
      (om-with-fg-color self *om-gray-color*
          (om-draw-string 10 24 "sound preview not available"))) 
    ))

;=======================EDITOR====================
   
(omg-defclass soundEditor (EditorView object-editor) 
   ((rulerx :initform nil :accessor rulerx)
    (mode :initform nil :accessor mode)
    (control :initform nil :accessor control)
    (preview :initform nil :accessor preview)
    (sndpict :initform nil :accessor sndpict)
    ;(sndptr :initform nil :accessor sndptr)
    (timeunit :initform 0 :accessor timeunit :initarg :timeunit)))

(defmethod get-score-class-ctrls ((self soundeditor)) 'Aiff-control)
(defmethod get-panel-class ((self soundEditor)) 'soundpanel)
(defmethod get-control-h ((self soundEditor)) 36)
(defmethod get-titlebar-class ((self soundeditor)) 'sound-titlebar)

(defmethod editor-has-palette-p ((self soundEditor)) nil)

;;; AJOUTER DANS LES CONTROLS
; (setf (cursor-mode view) :interval)
; (setf (cursor-mode view) :normal)
; (init-coor-system view)



(defmethod editor-null-event-handler :after ((self soundEditor))  ;chercher *mouse-window-event* par tout et enlever 
   (do-editor-null-event self))

(defmethod editor-has-palette-p ((self soundEditor)) 'sound-palette)



(defmethod initialize-instance :after ((self soundEditor) &rest l)
   (declare (ignore l))
   (let* ((ed-view (om-make-view (get-panel-class self) 
                     :owner self
                     :scrollbars :h
                     :position (om-make-point 0 *titlebars-h*) 
                     :size (om-make-point (w self) (- (h self) (+ 25 *titlebars-h* (get-control-h self))))))
          (rulerx (om-make-view 'sound-ruler
                    :owner self
                    :axe 'x
                    :assoc-view ed-view
                    :zoom 1000
                    :minzoom 1
                    :position (om-make-point 0 (- (h self) (get-control-h self) 25)) 
                    :size (om-make-point (w self) 25)))
          (control (om-make-view (get-score-class-ctrls self) 
                     :owner self
                     :position (om-make-point 0 (- (h self) (get-control-h self))) 
                     :size (om-make-point (w self) (get-control-h self))))
          (prev (om-make-view 'full-preview 
                     :owner self
                     :position (om-make-point 0 (get-control-h self)) 
                     :size (om-make-point (w self) (get-control-h self)))))
     (setf (mode self) 8)
     (setf (panel self) ed-view)
     (setf (control self) control)
     (setf (preview self) prev)
     (om-set-bg-color (control self) *controls-color*)
     (setf (sndpict self) (sound-get-pict (object self)))
     (setf (cursor-p (panel self)) t)
     (setf (rulerx ed-view) rulerx)
     (setf (rangex ed-view) (list 0 (get-obj-dur (object self))))
     (setf (bounds-x ed-view) (list 0 (get-obj-dur (object self))))
     (set-units-ruler ed-view rulerx)
     (add-sound-params control)
     (om-invalidate-view ed-view)))


(defmethod update-editor-after-eval ((self soundEditor) val)
  (call-next-method)
  (update-controls (control self))
  (setf (rangex (panel self)) (list 0 (get-obj-dur (object self))))
  (setf (bounds-x (panel self)) (list 0 (get-obj-dur (object self))))
  (set-units-ruler (panel self) (rulerx (panel self)))
  (let ((path (om-sound-file-name (object self))))
    (om-set-window-title (om-view-window self) (if path (namestring path) "empty sound")))
  (setf (sndpict self) (sound-get-pict (object self)))
  (update-titlebar self)
  (update-subviews self))

(defmethod update-editor-controls ((self soundEditor)) 
  (update-controls (control self)))


(defmethod sound-import ((self soundeditor))
  (let ((newsound (get-sound)))
    (when newsound
      (setf (value (ref self)) newsound)
      (om-invalidate-view (car (frames (ref self))))
      (update-editor-after-eval self (value (ref self))))))

(defmethod do-editor-null-event ((self soundEditor)) 
   (when (om-view-contains-point-p (panel self) (om-mouse-position self))
     (om-with-focused-view (panel self) ;;(control self) 
       (let* ((pixel (om-mouse-position (panel self)))
              (point (pixel2point (panel self) pixel))
              (timestr (if (= 1000 (timeunit self)) 
                           (format () "t: ~4D s" (/ (om-point-h point) 1000.0))
                         (format () "t: ~D ms" (om-point-h point)))))
         (om-with-fg-color (panel self) (om-get-bg-color (panel self))
           (om-fill-rect (om-h-scroll-position (panel self)) 0 80 20))
         (om-with-font (om-make-font *om-score-font-face* (nth 1 *om-def-font-sizes*))
                       (om-draw-string (+ 6 (om-h-scroll-position (panel self))) 12 timestr))))))

      
(defun calc-panel-size (bounds width range)
  (if (zerop (- (cadr range) (car range))) width
    (round (* (/ width (- (cadr range) (car range))) (- (cadr bounds) (car bounds))))))


(defmethod update-subviews ((self soundeditor))
   (when (title-bar self)
     (om-set-view-size  (title-bar self) (om-make-point (w self) *titlebars-h*)))
   (om-set-view-size  (panel self) (om-make-point (w self)
                                                   (- (h self) (get-control-h self) *preview-size* *titlebars-h* 25)))
   (om-set-field-size  (panel self) (om-make-point (calc-panel-size (bounds-x (panel self)) 
                                                                    (w self)
                                                                    (rangex (panel self)))
                                                   (- (h self) (get-control-h self) *preview-size* *titlebars-h* 25)))
   (om-set-view-position (panel self) (om-make-point 0 (+ *preview-size* *titlebars-h*)))
   
   (unless (zerop (cadr (bounds-x (panel self))))
     (om-set-scroll-position (panel self)
                             (om-make-point (round (* (om-point-h (om-field-size (panel self)))
                                                      (/ (car (rangex (panel self)))
                                                         (cadr (bounds-x (panel self))))))
                                            0)))
    
   (om-set-view-position (preview self) (om-make-point 0 *titlebars-h*))
   (om-set-view-size (preview self) (om-make-point (w self) *preview-size*))
   (om-set-view-size  (control self) (om-make-point (w self) (get-control-h self)))
   (om-set-view-position (control self) (om-make-point 0 (- (h self) (get-control-h self))))
   (om-set-view-position (rulerx (panel self)) (om-make-point 0 (- (h self) 25 (get-control-h self))))
   (om-set-view-size (rulerx (panel self)) (om-make-point (w self) 25))
   (om-invalidate-view self))

(defmethod om-get-menu-context ((self soundEditor))
  (let* ((thesound (object self)))
    (when (pict-spectre thesound)
      (if (pict-spectre? thesound)
        (list (om-new-leafmenu "show waveform" #'(lambda () (change-sound-pict (panel self))))
              )
        (list (om-new-leafmenu "show spectre" #'(lambda () (change-sound-pict (panel self))))
              )))))

(defmethod get-menubar ((self soundEditor)) 
  (list (om-make-menu "File" 
                      (list
                       (om-new-leafmenu  "Close" #'(lambda () (om-close-window (window self))) "w")
                       (list 
                        (om-new-leafmenu  "Import..." #'(lambda () (sound-import self)) nil))))
        (om-make-menu "Edit" 
                      (list
                       (om-new-leafmenu  "Select All" #'(lambda () (editor-select-all self)) "a")))
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self)))

(defmethod get-help-list ((self soundeditor))
  (list '((alt+clic "Add Marker")
          (del "Delete Selected Markers")
          (("g") "Sow/Hide Grid")
          (("A") "Align Selected Markers to Grid")
          (esc "Reset cursor")
          (space "Play/Stop"))))

;;;======= PANEL =======
(omg-defclass soundPanel (om-scroller view-with-ruler-x cursor-play-view-mixin om-view-drag) 
  ((mode :initform 0 :accessor mode)
   (selection? :initform nil :accessor selection?)
   (bounds-x :initform '(0 1) :accessor bounds-x :initarg :bounds-x))
  (:default-initargs
      #+win32 :draw-with-buffer #+win32 t)
     )

;;; temp compatibilité
(defmethod (setf cursor-p) (val (self soundpanel))
  (setf (cursor-mode self) (if val :interval :normal)))

(defmethod cursor-p ((self soundpanel))
  (equal (cursor-mode self) :interval))


(defmethod editor ((self soundpanel)) (om-view-container self)) 

(defmethod om-view-scrolled ((self soundpanel) x y)
  (let ((oldrange (rangex self))
        (newx (round (* (/ x (om-point-h (om-field-size self))) (cadr (bounds-x self))))))
    (setf (rangex self)
          (list newx (+ newx (- (cadr oldrange) (car oldrange)))))
    (om-invalidate-view (rulerx self))
    (om-invalidate-view (preview (editor self)))))
 
(defmethod get-string-nom  ((Self soundpanel) Num Axe)
   (declare (ignore axe))
   (format nil "~,2F" (ms->sec num)))

(defmethod point2pixel ((self soundpanel) point sys-etat)
   "Convert 'point' to pixels in the view 'self', 'sys-etat' is calculated by the 'get-system-etat' function."
   (om-add-points (call-next-method) (om-scroll-position self)))
           

(defmethod assoc-w ((self soundpanel)) 
  (w self))
;   (om-point-h (om-field-size self)))

(defmethod pixel2point ((self soundpanel) pixel)
  (call-next-method self (om-subtract-points pixel (om-scroll-position self))))


(defmethod init-coor-system ((self soundpanel))
  (setf (rangex self) (copy-list (bounds-x self)))
  (set-units-ruler self (rulerx self))
  (update-subviews (om-view-container self)))

;(defmethod update-view-of-ruler  ((self view-with-ruler-mixin))
;   "Sometimes update drawing is hard, you can redefine this method."
;  (om-redraw-view self));


(defmethod selection-to-play-? ((self soundpanel))
  (or (and (cursor-interval self) 
           (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
      (and (cursor-pos self) (not (zerop (cursor-pos self))))))

(defmethod get-selection-to-play ((self soundpanel))
  (if (and (cursor-interval self)
           (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
      (call-next-method)
    (let ((interval (list (cursor-pos self) (cadr (bounds-x self)))))
      (values  (list (object (om-view-container self))
                     :interval interval)
               (first interval)
               (second interval)))))

(defmethod attached-cursor-views ((self soundpanel)) (list (preview (editor self))))

;; no turn page
(defmethod scroll-play-window ((self soundPanel)) t)


(defmethod change-player ((panel soundpanel) val)
  (call-next-method)
  (if (equal val :multiplayer) (launch-multiplayer-app)))

;------------------------------------
;Events
;------------------------------------

(defmethod om-view-cursor ((self soundPanel))
   (if (equal (cursor-mode self) :interval) ; (cursor-p self)
     ;(if (om-option-key-p)
     ;  *om-hand-cursor*
       *om-i-beam-cursor*;)
     *om-arrow-cursor*))


(defmethod handle-key-event ((self soundPanel) char)
   (case char
     (#\g (grille-on-off self))
     (#\A (align-markers self))
     (#\h (show-help-window "Commands for SOUND Editor" (get-help-list (editor self))))
     (:om-key-delete (delete-sound-marker self))
     (:om-key-esc (reset-cursor self))
     (#\SPACE (let ((snd (object (om-view-container self))))
                (oa::om-smart-play-stop snd self)))
     (otherwise (call-next-method))))



(defmethod reset-cursor ((self soundpanel))
  (setf (cursor-pos self) 0)
  (om-invalidate-view self))

(defmethod align-markers ((self soundpanel))
  (when (selection? self)
    (loop for i in (selection? self) do 
          (setf (nth i (markers (object (editor self))))
                (/ (* (zoom (rulerx self)) 
                      (round (* (nth i (markers (object (editor self)))) 1000) (zoom (rulerx self))))
                   1000.0)))
    (om-invalidate-view self)
    (report-modifications (editor self))))
          
(defmethod mrk-before ((self sound) t-s)
  (let ((b 0))
    (loop for m in (markers self) do
          (when (and (> m b) (<= m t-s)) (setf b m)))
    b))

(defmethod mrk-after ((self sound) t-s)
  (let ((a (/ (get-obj-dur self) 1000.0)))
    (loop for m in (markers self) do
          (when (and (< m a) (>= m t-s)) (setf a m)))
    a))
 
(defmethod special-move-marker ((Self soundPanel) Point)
  (let* ((thesound (object (om-view-container self)))
         (dur (/ (om-sound-n-samples  thesound) (om-sound-sample-rate  thesound)))
         (dec 6)
         (Xsize 80)
         (Mydialog (om-make-window 'om-dialog
                                   :size (om-make-point (+ xsize 110) 80)
                                   :window-(when (not *activate-handler*)
        (setf *activate-handler* t)
        (palette-open self)
        (setf *activate-handler* nil)
        (palette-init (editor self)))title "Marker Onset"
                                   :maximize nil :minimize nil :resizable nil
                                   :bg-color *om-window-def-color*
                                   :position (om-add-points (om-view-position (window self)) (om-mouse-position self))))
         (Xed (om-make-dialog-item 'om-editable-text (om-make-point 10 30)  (om-make-point (- xsize 25) 16)
                                   (format nil "~F" (nth point (markers thesound)))
                                   :font *controls-font*)))
    (om-add-subviews mydialog 
                     xed
                     (om-make-dialog-item 'om-static-text (om-make-point 14 8) (om-make-point 50 16) "t (s)"
                                          :font *om-default-font2b*)
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 8) (om-make-point 70 15) "Cancel"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (om-return-from-modal-dialog mydialog ()))
                                          :focus nil
                                          :default-button nil)
                     (om-make-dialog-item 'om-button (om-make-point (- (w mydialog) 80) 34) (om-make-point 70 15) "OK"
                                          :di-action (om-dialog-item-act item 
                                                       (declare (ignore item))
                                                       (let ((val (read-from-string (om-dialog-item-text xed))))
                                                         (when (and (numberp val)
                                                                    (>= val 0)
                                                                    (<= val dur))
                                                           (setf (nth point (markers thesound)) val)
                                                           (setf (markers thesound)
                                                                 (remove-duplicates (sort (markers thesound) '<)))
                                                           (setf (selection? self) (list (position val (markers thesound))))
                                                           (report-modifications (editor self))))
                                                       
                                                       (om-return-from-modal-dialog mydialog ()))
                                          :default-button t
                                          ))
    (om-modal-dialog mydialog)))


(defmethod change-sound-pict ((self soundPanel))
  (let* ((thesound (object (om-view-container self))))
  (setf (pict-spectre? thesound) (not (pict-spectre? thesound)))
  (om-invalidate-view self t)))

(defmethod om-view-click-handler ((self soundPanel) where)
  (if (om-add-key-p) (add-sound-marker self where)
    (if (equal (cursor-mode self) :interval) ; (cursor-p self)
        (progn 
          (setf (selection? self) nil)
          (new-interval-cursor self where)
          )
      (let* ((graph-obj (click-in-sound-marker-p self where)))
        (setf (cursor-pos self) 0)
        (if graph-obj
            (if (om-shift-key-p) (omselect-with-shift self graph-obj)
              (when (not (member graph-obj (selection? self) :test 'equal))
                (setf (selection? self) (list graph-obj))
                (om-invalidate-view self t)))
          (unless (or (om-shift-key-p) (om-drag-selection-p self where))
            (setf (cursor-interval self) '(0 0))
            (control-actives self where)
            ))))))

(defmethod om-view-doubleclick-handler ((self soundPanel) Where)
  (if (equal (cursor-mode self) :interval) ; (cursor-p self)
      (progn
        (setf (cursor-pos self) (om-point-h (pixel2point self where)))
        (setf (cursor-interval self) (list (cursor-pos self) (cadr (bounds-x self))))
        (om-invalidate-view self t))
    (let ((Position-Obj (click-in-sound-marker-p self where)))
      (if Position-Obj
          (special-move-marker self position-obj)
        (when (markers (object (editor self)))
          (let* ((time (/ (om-point-h (pixel2point self where)) 1000.0))
                 (t1 (mrk-before (object (editor self)) time))
                 (t2 (mrk-after (object (editor self)) time)))
            (setf (cursor-interval self) (list (round (* t1 1000)) (round (* t2 1000))))
            (setf (cursor-pos self) (car (cursor-interval self)))
            )))
      (om-invalidate-view self t)
      )))

(defmethod control-actives ((self soundPanel) where)
  (unless (om-shift-key-p)
    (setf (selection? self) nil))
  (om-init-motion-functions self 'make-selection-rectangle 'release-selection-rectangle)
  (om-new-movable-object self (om-point-h where) (om-point-v where) 4 4 'om-selection-rectangle))

(defmethod make-selection-rectangle ((self soundPanel) pos)
  (let ((rect  (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos))))
    (when rect
      (om-update-movable-object self (first rect) (second rect) (max 4 (third rect)) (max 4 (fourth rect))))))

(defmethod release-selection-rectangle ((self soundPanel) pos) 
  (let* ((rect (om-get-rect-movable-object self (om-point-h pos) (om-point-v pos)))
         (dur (cadr (bounds-x self)))
         minx maxx)
    (when rect
      (om-erase-movable-object self)
      (let (user-rect)
        (setf user-rect (om-make-rect  (first rect) (second rect) (+ (first rect) (third rect)) (+ (second rect) (fourth rect))))
        (setf minx (min (om-rect-left user-rect) (om-rect-right user-rect)))
        (setf maxx (max (om-rect-left user-rect) (om-rect-right user-rect)))
        (loop for item in (markers (object (editor self)))
              for k = 0 then (+ k 1) do
            (let ((marker-pix (round (* (om-point-h (om-field-size self)) item 1000) dur)))
              (when (and (>= marker-pix minx) (<= marker-pix maxx))
                (push k (selection? self))))))
      (om-invalidate-view self))
    ))



(defmethod omselect-with-shift ((self soundPanel) graph-obj)
  (if (member graph-obj (selection? self) :test 'equal)
    (setf (selection? self) (remove  graph-obj (selection? self) :test 'equal))
    (push graph-obj (selection? self)))
  (om-invalidate-view self))


(defmethod click-in-sound-marker-p ((self soundPanel) where &optional (approx 3))
  (let* ((thesound (object (om-view-container self)))
         (x (om-point-h where))
         dur rep)
    (when (and (valid-sound-p thesound) (numberp (om-sound-sample-rate thesound)))
        (setf dur (/ (om-sound-n-samples thesound) (om-sound-sample-rate  thesound)))
    (loop for item in (markers thesound)
          for i = 0 then (+ i 1)
          while (not rep) do
          (let ((pix (round (* (om-point-h (om-field-size self)) item) dur)))
            (when (and (> x (- pix approx) ) (< x (+ pix approx)) )
              (setf rep i))))
    rep)))

(defmethod add-sound-marker ((Self soundPanel) Where)
  (let* ((thesound (object (om-view-container self)))
         (point-x (om-point-h (pixel2point self where)))
         (newpoint (/ point-x 1000.0)))
    (setf (markers thesound) (remove-duplicates (sort (cons newpoint (markers thesound)) '<)))
    (setf (selection? self) (list (position newpoint (markers thesound))))
    (om-invalidate-view self t)
    (report-modifications (editor self))))

(defmethod delete-sound-marker ((Self soundPanel))
  (let* ((thesound (object (om-view-container self)))
         (copy (copy-list (markers thesound))))
    (loop for item in (selection? self) do
          (let ((list (remove (nth item copy) (markers thesound))))
            (setf (markers thesound) list)))
    (setf (selection? self) nil)
    (om-invalidate-view self t)
    (report-modifications (editor self))))

(defmethod editor-select-all ((self soundeditor)) 
  (let ((thesound (object self)))
    (setf (selection? (panel self) )
          (loop for item in  (markers thesound)
                for k = 0 then (+ k 1) collect k))
    (om-invalidate-view self t)))

;------------------------------------
;DRAW
;------------------------------------
; (capi::draw-metafile-to-image self (oa::themetafile (pic-to-draw thesound)) :width 1000 :height 1000)

(defmethod om-draw-contents ((self soundPanel))
  (call-next-method)  
  (if ;(recording? self)
      (equal (state (player self)) :recording)
      (om-with-focused-view self  
         (om-with-fg-color self *om-red2-color*
           (om-with-font *om-default-font4b*
              (let ((halftext (round (om-string-size "Recording" *om-default-font4b*) 2)))
                (om-draw-string (- (round (w self) 2) halftext) (round (h self) 2) "Recording")))))
  (let* ((thesound (object (om-view-container self)))
         (dur (or (and (and (om-sound-sample-rate thesound) (om-sound-n-samples thesound))
                       (/ (om-sound-n-samples thesound) (om-sound-sample-rate thesound)))
                  0))
         (total-width (om-point-h (om-field-size self)))
         (thepicture (and dur (pic-to-draw thesound))))
    (om-with-focused-view self
      (when (and thesound thepicture)
        (om-with-fg-color self *om-dark-gray-color*
          ;(print (list (w self) (om-point-h (om-field-size self))))
          (om-draw-picture self thepicture (om-make-point 0 0) (om-subtract-points (om-field-size self)
                                                                                   (om-make-point 0 15))
                           ;:srctopleft (om-make-point (round (* (/ (car (rangex self)) (- (cadr (bounds-x self)) (car (bounds-x self))))
                           ;                                     (om-point-h (om-get-picture-size thepicture))))
                           ;                           0)
                           ;:srcsize (om-make-point (print (round (* (/ (- (cadr (rangex self)) (car (rangex self))) 
                           ;                                            (- (cadr (bounds-x self)) (car (bounds-x self))))
                           ;                                  (om-point-h (om-get-picture-size thepicture)))))
                           ;                        (om-point-v (om-get-picture-size thepicture))))                 
                           ))
        (om-with-fg-color self *om-blue-color*
          (loop for item in (markers thesound)
                for k = 0 then (+ k 1) do
                (om-with-line-size (if (member k (selection? self)) 2 1)
                  (om-draw-line (round (* total-width item) dur) 0 (round (* total-width item) dur) (h self))
                  (om-fill-rect (- (round (* total-width item) dur) 2) 0 5 5)))))
      (when (grille-p self)
        (draw-grille self))
      (draw-interval-cursor self)
      (unless thepicture
        (if (and (om-sound-file-name thesound) (zerop dur))
            (om-with-focused-view self
              (om-draw-string 30 30 (format nil "Error: file ~s is empty" (om-sound-file-name (object (editor self))))))
          (om-with-focused-view self 
            (om-draw-string (round (w self) 2) (round (h self) 2) "..."))
          )
        )))))


(defmethod draw-interval-cursor ((self soundPanel))
  (unless (zerop (or (om-sound-n-samples (object (om-view-container self))) 0))
    (call-next-method)
    (let ((cursor-pos-pix (om-point-h (point2pixel self (om-make-big-point (cursor-pos self) 0) (get-system-etat self)))))
      (om-with-focused-view self
        (om-with-fg-color self *om-red2-color*
          (om-with-dashline 
              (om-with-line-size 2 
                (om-draw-line cursor-pos-pix 0 cursor-pos-pix (h self)))))
        ))))





(defmethod draw-grille  ((self soundpanel)) 
   (om-with-focused-view self
     (om-with-fg-color self *om-gray-color*
         (om-with-line '(2 2)
           ;(ruler-print-draw-grille (rulerx self) 'x 0 (om-h-scroll-position self) (get-system-etat self))
           (ruler-print-draw-grille (rulerx self) 'x 0 0 (get-system-etat self))
           ))))
   
(defmethod om-invalidate-view ((self soundpanel) &optional erase)
  (call-next-method)
  (om-invalidate-view (preview (editor self))))


;;;=========== BAR ===========

(omg-defclass sound-titlebar (editor-titlebar) ())

(defmethod init-titlebar ((self soundeditor))
  (let* ((pathname (om-sound-file-name (object self)))
         (name (if pathname
                   (if (stringp (pathname-type pathname))
                       (string+ (pathname-name pathname) "." (pathname-type pathname))
                     (pathname-name pathname))
                 "No file attached"))) 
  (om-add-subviews (title-bar self)
                   (om-make-dialog-item 'om-static-text (om-make-point 10 4) 
                                        (om-make-point 
                                         (+ 10 (om-string-size (string+ (om-str :file) ": " name)
                                                               *om-default-font1b*))
                                         18)
                                        (string+ (om-str :file) ": " name)
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1b*
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 200 4) (om-make-point 120 18)
                                        (format nil "Format: ~D" (or (om-format-name (om-sound-format (object self))) "--"))
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1*
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 300 4) (om-make-point 80 18)
                                        (format nil "SR: ~D" (if (om-sound-sample-rate (object self))
                                                                 (round (om-sound-sample-rate (object self)))
                                                               "--"))
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1*
                                        )
                   (om-make-dialog-item 'om-static-text (om-make-point 400 4) (om-make-point 80 18)
                                        (format nil "SS: ~D" (or (om-sound-sample-size (object self)) "--"))
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1*
                                        )
                   )))





;;;------------------------------
;;; DRAG SOUND

(defmethod om-drag-selection-p ((self soundpanel) position) 
  (and (equal (cursor-mode self) :normal) ; (not (cursor-p self)) 
       (not (om-shift-key-p))
       (or (and (selection? self) (click-in-sound-marker-p self position))
           (and
            (<= (om-point-h (pixel2point self position)) (cadr (cursor-interval self)))
            (>= (om-point-h (pixel2point self position)) (car (cursor-interval self)))))))

(defmethod om-view-drag-hilite-p ((self soundpanel)) nil)

(defmethod om-drag-container-view ((self soundpanel)) self)

(defmethod om-draw-contents-for-drag ((self soundpanel))
  (let ((m (dragged-list-objs *OM-drag&drop-handler*))
        (marks (markers (object (editor self)))))
    (cond ((and m (numberp m))
           (let ((pos (om-point-h (point2pixel self (om-make-big-point (round (* (nth m marks) 1000)) 0) (get-system-etat self)))))
             (om-with-fg-color nil (om-make-color-alpha 0.5 0.5 0.5 0.5)
               (om-fill-rect (- pos 2) 0 4 (h self))
               )))
          ((cursor-interval self)
           (let ((pos1 (om-point-h (point2pixel self (om-make-big-point (car (cursor-interval self)) 0) (get-system-etat self))))
                 (pos2 (om-point-h (point2pixel self (om-make-big-point (cadr (cursor-interval self)) 0) (get-system-etat self)))))
             (om-with-fg-color nil (om-make-color-alpha 0.5 0.5 0.5 0.5)
               (om-fill-rect pos1 0 (- pos2 pos1) (h self))
               )))
          (t nil))))



(defmethod om-drag-start ((self soundpanel))
  (when (om-drag-selection-p self (om-mouse-position self))
  (setf (dragged-view *OM-drag&drop-handler*) self
        (dragged-list-objs *OM-drag&drop-handler*) nil
        (container-view *OM-drag&drop-handler*) self
        (true-dragged-view *OM-drag&drop-handler*) self
        (drag-flavor *OM-drag&drop-handler*) :omvw)
  (let ((r (om-new-region))
        (marks (markers (object (editor self))))
        (m (click-in-sound-marker-p self (om-mouse-position self) 10)))
    (if m
        (progn 
          (setf (selection? self) (list m))
          (setf (dragged-list-objs *OM-drag&drop-handler*) m))
      (when (cursor-interval self)
        (setf r (om-set-rect-region r
                                    (+ (om-h-scroll-position self)
                                     )
                                    0 
                                    (om-point-h (point2pixel self (om-make-big-point (cadr (cursor-interval self)) 0) (get-system-etat self)))
                                       (h self))))
      )
    )
  ))
  

(defmethod extract-sound-selection ((self soundpanel) &optional filename)
  (let ((file (or filename (om-choose-new-file-dialog :prompt "Choose a New File" 
                                                      :directory *om-outfiles-folder*
                                                      ))
              ))
    (when file 
      (setf *last-saved-dir* (om-make-pathname :directory file))
      (save-sound (sound-cut (object (editor self)) (car (cursor-interval self)) (cadr (cursor-interval self)))
                  file))
    ))



(defmethod om-drag-receive ((target soundpanel) (dragged-ref t) position &optional (effect nil))
  (let ((dragged (dragged-view *OM-drag&drop-handler*)))
    (if (equal dragged target)
      (let* ((newpos (om-mouse-position target))
             (draggedobj (dragged-list-objs *OM-drag&drop-handler*)))
        (when (integerp draggedobj)
          (setf (nth draggedobj (markers (object (editor target))))
                (/ (om-point-h (pixel2point target newpos)) 1000.0))
          (om-invalidate-view target)
          (report-modifications (editor target))
          t
          ))
      )))


;;;============================
;;; SPECIAL AUDIO EDITOR TO PATCHPANEL


(defmethod om-drag-receive ((target patchpanel) (dragged-ref t) position &optional (effect nil))
  (let ((dragged (dragged-view *OM-drag&drop-handler*))
        (posi (om-mouse-position target)))
    (if (subtypep (class-name (class-of dragged)) 'soundpanel)
        (unless (integerp (car (selection? dragged)))
          (let* ((newsoundfile (extract-sound-selection dragged))
                 newsound newbox)
            (when newsoundfile (setf newsound (load-sound-file newsoundfile)))
            (when newsound
              (setf newbox (make-instance 'OMBoxEditCall
                                          :name "sound extract"
                                          :reference (class-of newsound) 
                                          :icon (icon (class-of newsound))
                                          :inputs (get-inputs-from-inst newsound)))
              (setf (value newbox) newsound)
              (set-edition-params (value newbox) newbox)
              (push newbox (attached-objs (class-of newsound)))
              (setf (numouts newbox) (length (get-outs-name (value newbox))))
              (setf (frame-position newbox) 
                    (borne-position posi))
              (loop for item in (get-actives target) do
                    (omG-unselect item))
              (let ((new-frame (make-frame-from-callobj newbox)))
                (omG-add-element target new-frame)
                (omG-select new-frame))
              )
            (om-invalidate-view target)
            t
            ))
      (call-next-method))))

;;;====================================
;;; AUDIO RECORD

(defmethod allow-record ((self soundpanel)) t)

(defmethod panel-record ((self soundpanel))
  (when (audio-record-start (get-score-player self))
    (om-invalidate-view self)
    t))

(defmethod DoStopRecord ((self soundpanel))
  (let* ((soundfile (audio-record-stop (get-score-player self)))
         (editor (om-view-container self))
         (box (ref editor)))
    (when (and soundfile (probe-file soundfile))
      (let ((newsound (load-sound-file soundfile)))
        (setf (object (om-view-container self)) newsound)
        (when (is-boxpatch-p box)
          (setf (value box) newsound)
          (om-invalidate-view (car (frames box))))
        (update-editor-after-eval editor newsound)   
        ))
    ;(setf (recording? self) nil)
    (setf (state (player self)) :stop)
    ))

