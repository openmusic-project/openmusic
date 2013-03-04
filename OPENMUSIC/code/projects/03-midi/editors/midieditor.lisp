
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
(defmethod make-editor-window ((class (eql 'midiEditor)) object name ref &key 
                               winsize winpos (close-p t) (winshow t) (resize t) (retain-scroll nil)
                               (wintype nil))
  (call-next-method class object (namestring (midifilename object)) ref :winsize winsize :winpos winpos :resize resize 
                    :close-p close-p :winshow winshow :resize resize
                    :retain-scroll retain-scroll :wintype wintype
                    ))

;Palette
(omg-defclass midi-Palette (playing-palette)  () 
   ;(:default-initargs :source-name "midi-palette")
   )

(defmethod palette-act ((self midi-palette) x)
   (unless (call-next-method)
     (let ((ed-view (panel (editor-assoc self))))
       (case x
         ;(9 (grille-on-off ed-view))
         ;(10 (om-beep-msg "Not implemented yet"))
         
         (7 (setf (cursor-p ed-view) nil)
            (setf (mode (editor-assoc self)) x))
         (8 (setf (cursor-p ed-view) t) 
            (setf (mode (editor-assoc self)) x))
         (9 (setf (cursor-p ed-view) nil) 
            (setf (mode (editor-assoc self)) x))
         (10 (init-coor-system ed-view))
         (otherwise
          (unless (= x 6)
            (setf (mode (editor-assoc self)) x)))
         )
       (om-invalidate-view (view *palette-win*) t)
       (om-invalidate-view ed-view)
       (om-invalidate-view (preview (editor-assoc self))))))  

;=============The control===========--
(omg-defclass Midi-control (3dBorder-view)  ())


(defmethod om-draw-contents :before ((self Midi-control))
   ;(om-with-focused-view self
   ;  (om-draw-line 0 24 (w self) 24)
   ;  (om-draw-string 25 8 "Pitch :")
   ;  (om-draw-string 25 20 "X-time:")
   ;  (om-draw-string 120 10 "file:")
   ;  (om-draw-string 160 10 (namestring (MidiFileName (object (om-view-container self))))))
   )

(defmethod initialize-instance :after ((self Midi-control) &rest l)
   (declare (ignore l))
   (additional-port-menu self :pos (om-make-point 120 0) :in nil)
   (om-add-subviews self 
                    (om-make-dialog-item 'om-check-box (om-make-point 40 2) (om-make-point 160 20)
                                         "  Separate MIDI tracks  "
                                         :di-action (om-dialog-item-act item 
                                                      (set-split-tracks (om-view-container self) (om-checked-p item)))))
   )
          


;=============The ruler y===========
(omg-defclass piano-roll-view (om-view) 
   ((thepal :initform (om-load-and-store-picture "piano-pict" 'kernel) :accessor thepal)
    (nb-tracks :initform 1 :accessor nb-tracks :initarg :nb-tracks)))

(defmethod om-draw-contents :before ((self piano-roll-view))
  (let ((h (round (h self) (max 1 (nb-tracks self)))))
    (loop for i = 0 then (+ i 1) while (< i (nb-tracks self)) do
          (om-draw-picture self (thepal self) (om-make-point 0 (* i h)) (om-make-point (w self) h)))))

(defmethod om-view-click-handler ((self piano-roll-view) where)
   (om-with-focused-view (panel (om-view-container self))
     (om-with-line 'dash 
       (om-draw-line 0 (om-point-v where) (w (panel (om-view-container self))) (om-point-v where)))
    (om-init-motion-functions self nil 'piano-release)))

(defmethod piano-release ((self piano-roll-view) pos)
  (om-invalidate-view (panel (om-view-container self))))

(defmethod ruler-print-draw-grille ((self piano-roll-view) axe offx offy system-etat)
   (declare (ignore axe offx offy system-etat)) t)
        






;=========PREVIEW===========

(defclass midi-preview (3Dborder-view om-view-cursor-play) ()
  #+win32(:default-initargs :draw-with-buffer t)
  )

(defmethod draw-line-cursor ((self midi-preview) &key newpixel (draw? t))
  (unless newpixel
    (setf newpixel (round (* (/ (cursor-pos *general-player*) (max 1 (cadr (bounds-x (panel (om-view-container self)))))) (w self)))))
  (when draw? 
    (om-update-movable-cursor self newpixel 0 4 (h self)))
   newpixel)

(defmethod om-draw-contents ((self midi-preview))  
  (call-next-method)
  (when (om-view-container self)
    (let* ((panel (panel (om-view-container self)))
           (dur (cadr (bounds-x panel))))
      (unless (zerop dur)

          (om-with-focused-view self
            (loop for tr in (tracks (object (om-view-container self))) do
                  (loop for note in (midinotes tr) do
                        (let ((y (- (h self) (round (* (- (first note) 24) (- (h self) 4)) 72) 2))
                              (dur (get-obj-dur (object (om-view-container self)))))
                        (om-with-fg-color self (nth (mod (- (nth 4 note) 1) 16) *16-color-list*)
                          (om-draw-line (round (* (second note) (w self)) dur) y (round (* (+ (second note) (third note)) (w self)) dur) y)))))
            
            (unless (and (= (car (rangex panel)) (car (bounds-x panel)))
                         (= (cadr (rangex panel)) (cadr (bounds-x panel))))
            (om-with-fg-color self (om-make-color-alpha (om-color-r *om-steel-blue-color*)
                                                        (om-color-g *om-steel-blue-color*)
                                                        (om-color-b *om-steel-blue-color*)
                                                        0.2)
            (om-fill-rect (round (* (w self) (/ (car (rangex panel)) 
                                                dur)))
                          2
                          (round (* (w self) (/ (- (cadr (rangex panel))  (car (rangex panel)))
                                                dur)))
                          (- (h self) 5)))

          (om-with-fg-color self *om-steel-blue-color*
            (om-draw-rect (round (* (w self) (/ (car (rangex panel)) 
                                                dur)))
                          2
                          (round (* (w self) (/ (- (cadr (rangex panel))  (car (rangex panel)))
                                                dur)))
                          (- (h self) 5)))
          )
          
          (when (cursor-p panel)
            (om-with-fg-color self *om-red2-color*
              (om-draw-line (round (* (w self) (cursor-pos panel)) dur) 0 
                            (round (* (w self) (cursor-pos panel)) dur) (h self)))
            (when (cursor-interval panel)
              (draw-h-rectangle (list (round (* (w self) (car (cursor-interval panel))) dur) 0 
                                      (round (* (w self) (cadr (cursor-interval panel))) dur) (h self)) t t))
            )))
      )))


(defvar *midi-preview-last-click* nil)
(defmethod om-view-click-handler ((self midi-preview) where)
     (om-init-motion-functions self 'scroll-panel-motion nil)
     (setf *midi-preview-last-click* where))

(defmethod scroll-panel-motion ((self midi-preview) pos)
  (let* ((panel (panel (om-view-container self)))
         (initmouse *midi-preview-last-click*)
         (initrangex (rangex panel))
         (x (om-point-h pos))
         (y (om-point-v pos))
         deltax)
      (setf deltax (* 50 (- x (om-point-h initmouse))))
      (unless (> (+ (second initrangex) deltax) (cadr (bounds-x panel)))
        (if (minusp (+ (first initrangex) deltax))
            (setf (rangex panel) (list 0 (- (second initrangex) (first initrangex))))
          (setf (rangex panel) (list (+ (first initrangex) deltax)
                                     (+ (second initrangex) deltax)))
          ))
      (om-invalidate-view (om-view-container self) t)
      (setf *midi-preview-last-click* (om-make-point x y))))



;------------------------------------
(omg-defclass MidiEditor (EditorView object-editor) 
   ((control :initform nil :accessor control)
    (mode :initform 7 :accessor mode)
    (preview :initform nil :accessor preview)
    (split-tracks :initform nil :initarg :split-tracks :accessor split-tracks)))

(defmethod get-menubar ((self midiEditor)) 
  (list (om-make-menu "File" 
                      (list (om-new-leafmenu  "Close" #'(lambda () (om-close-window (window self))) "w")))
        (make-om-menu 'windows :editor self)
        (make-om-menu 'help :editor self)))


;;; BAR 
(omg-defclass midi-titlebar (editor-titlebar) ())

(defmethod get-titlebar-class ((self MidiEditor)) 'midi-titlebar)

(defmethod init-titlebar ((self midieditor))
  (let* ((name (pathname-name (midifilename (object self)))))
    (om-add-subviews (title-bar self)
                     (om-make-dialog-item 'om-static-text (om-make-point 10 4) 
                                        (om-make-point 
                                         (+ 10 (om-string-size (format nil "File: ~A" name)
                                                               *om-default-font1b*))
                                         18)
                                        (format nil "File: ~A" name)
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1b*)
                   (om-make-dialog-item 'om-static-text (om-make-point 200 4) (om-make-point 80 18)
                                        (format nil "Nb Tracks: ~D" (length (tracks (object self))))
                                        :bg-color *editor-bar-color*
                                        :font *om-default-font1*
                                        )
                  )))


(defmethod editor-has-palette-p ((self MidiEditor)) 'midi-palette)

(defmethod get-palette-pict ((self midieditor)) 
  (om-load-and-store-picture "midi-palette" 'internal))

(defmethod palette-init ((self MidiEditor))
  (call-next-method)
  (init-play-palette self))

(defmethod editor-null-event-handler :after ((self MidiEditor))
  (do-editor-null-event self))

(defmethod set-split-tracks ((self midieditor) t-or-nil)
  (setf (split-tracks self) t-or-nil)
  (setf (nb-tracks (rulery (panel self))) (if t-or-nil (length (tracks (object self))) 1))
  (om-invalidate-view (rulery (panel self)))
  (om-invalidate-view (panel self)))

(defvar *mouse-last-point* nil)

(defmethod do-editor-null-event ((self MidiEditor))
  (when (and (om-view-contains-point-p (panel self) (om-mouse-position self))
             (if *mouse-last-point* (not (om-points-equal-p (om-mouse-position self) *mouse-last-point*)) t))
    (setf *mouse-last-point* (om-mouse-position self))
    (om-with-focused-view (title-bar self) 
      (let* ((pixel (om-mouse-position (panel self)))
             (point (pixel2point (panel self) pixel)))
        (om-erase-rect-content 400 0 700 23)
        (om-invalidate-view (panel self))
        (om-draw-string 400 15 (format () "t=~Dms" (om-point-h point)))
        (om-draw-string 500 15 (format () "pitch=~D" (om-point-v point)))
        (when (split-tracks self) (om-draw-string 600 15 (format () "Track=~D" 
                                                                 (ceiling (om-point-v pixel) (floor (h (panel self)) (nb-tracks (rulery (panel self)))))))
          )
        point))))


(defmethod update-subviews ((self MidiEditor))
  (om-set-view-position (panel self) (om-make-point 25 (+ (h (preview self)) (h (title-bar self)))))
  (om-set-view-size (panel self) (om-make-point (- (w self) 25) (- (h self) (h (title-bar self)) (h (control self)) (h (preview self)) 25)))
  
  ;; calc-panel-size from soundeditor
  (om-set-field-size  (panel self) (om-make-point (calc-panel-size (bounds-x (panel self)) 
                                                                   (w (panel self))
                                                                   (rangex (panel self)))
                                                  (- (h self) (get-control-h self) (h (preview self)) (h (control self)) 25)))
  
  (om-set-view-position (preview self) (om-make-point 25 (h (title-bar self))))
  (om-set-view-size (preview self) (om-make-point (- (w self) 25) (h (preview self))))
  
  (om-set-view-position (control self) (om-make-point 0 (- (h self) 30)))
  (om-set-view-size (control self) (om-make-point (w self) 30))

  (when (title-bar self)
    (om-set-view-size (title-bar self) (om-make-point (w self) *titlebars-h*)))
  
  (om-set-view-size (rulery (panel self )) (om-make-point 25 (- (h self) (h (title-bar self)) (h (preview self)) (h (control self)) 25)))
  (om-set-view-position (rulery (panel self )) (om-make-point 0 (+ (h (title-bar self)) (h (preview self)))))

  (om-set-view-size (rulerx (panel self )) (om-make-point (w self) 25))
  (om-set-view-position (rulerx (panel self )) (om-make-point 25 (- (h self) 25 (h (control self)))))
  
  (unless (zerop (cadr (bounds-x (panel self))))
    (om-set-scroll-position (panel self)
                            (om-make-point (round (* (om-point-h (om-field-size (panel self)))
                                                     (/ (car (rangex (panel self)))
                                                        (cadr (bounds-x (panel self))))))
                                           0)))
  
  (om-invalidate-view self t))

(defmethod initialize-instance :after ((self MidiEditor) &rest l)
   (declare (ignore l))
   (let* ((tracklist (tracks (object self)))
          (ed-view (om-make-view 'midiPanel 
                     :owner self
                     :scrollbars nil
                     :position (om-make-point 25 *titlebars-h*) 
                     :size (om-make-point (- (w self) 25) (- (h self) 50))))
          (control (om-make-view 'Midi-control 
                     :owner self
                     :position (om-make-point 0 (- (h self) 25))
                     :size (om-make-point (w self) 25)))
          (rulery (om-make-view 'piano-roll-view 
                    :owner self
                    :position (om-make-point 0 25) 
                    :size (om-make-point 25 (- (h self) 50))))
          (rulerx (om-make-view 'sound-ruler 
                    :owner self
                    :axe 'x
                    :assoc-view ed-view
                    :zoom 1000
                    :minzoom 0.001
                    :position (om-make-point 25 (- (h self) 25)) 
                    :size (om-make-point (w self) 25)))
          (prev (om-make-view 'midi-preview 
                     :owner self
                     :position (om-make-point 0 (get-control-h self)) 
                     :size (om-make-point (w self) 40))))
     (setf (rulerx ed-view) rulerx)
     (setf (control self) control)
     (setf (preview self) prev)
     (om-set-bg-color (control self) *controls-color*)
     (setf (rulery ed-view) rulery)
     (setf (panel self) ed-view)
     (setf (rangex ed-view) (list 0 (get-obj-dur (object self))))
     (setf (bounds-x ed-view) (list 0 (get-obj-dur (object self))))
     (update-subviews self)
     (set-units-ruler ed-view rulerx)))

(defmethod update-editor-after-eval ((self midiEditor) val)
  (call-next-method)
  (setf (rangex (panel self)) (list 0 (get-obj-dur (object self))))
  (setf (bounds-x (panel self)) (list 0 (get-obj-dur (object self))))
  (set-units-ruler (panel self) (rulerx (panel self)))
  (let ((s-t (split-tracks self)))
    (set-split-tracks self nil)
    (when s-t (set-split-tracks self t)))
  (om-set-window-title (om-view-window self) (namestring (midifilename (object self))))
  (update-titlebar self)
  (update-subviews self))


;------------------------------------
(defclass midiPanel (om-scroller view-with-ruler-xy cursor-play-view-mixin) 
              ((bounds-x :initform '(0 1) :accessor bounds-x :initarg :bounds-x))
              #+win32(:default-initargs :draw-with-buffer t)
              )

(defmethod set-units-ruler  ((self midiPanel) (ruler ruler)) nil)

(defmethod om-view-cursor ((self midiPanel))
   (declare (ignore where))
   (cond
    ((= 9 (mode (om-view-container self))) *om-hand-cursor*)
    ((cursor-p self) *om-i-beam-cursor*) 
    (t *om-arrow-cursor*)
    ))

;(defmethod om-view-scrolled ((self soundpanel) x y)
;  (let ((oldrange (rangex self))
;        (newx (round (* (/ x (om-point-h (om-field-size self))) (cadr (bounds-x self))))))
;    (setf (rangex self)
;          (list newx (+ newx (- (cadr oldrange) (car oldrange)))))
;    (om-invalidate-view (rulerx self))
;    (om-invalidate-view (preview (editor self)))))

(defvar *midi-panel-last-click* nil)
(defmethod om-view-click-handler ((self midiPanel) where)
  (if (cursor-p self)
      (progn 
        (if (equal (cursor-interval self) '(0 0))
            (new-interval-cursor self where)
          (setf (cursor-interval self) '(0 0)))
        (om-invalidate-view self)
        (om-invalidate-view (preview (editor self))))
    (when (=  (mode (om-view-container self)) 9) 
      (om-init-motion-functions self 'scroll-system-motion nil)
      (setf *midi-panel-last-click* where)
      ;(setf (grille-p self) nil)
      )))

(defmethod release-interval-select ((self midiPanel) pos)  
  (call-next-method)
  (om-invalidate-view (preview (editor self))))
  
(defmethod scroll-system-motion ((self midiPanel) pos)
  (let* ((initmouse *midi-panel-last-click*)
         (initrangex (rangex self))
         (x (om-point-h pos))
         (y (om-point-v pos))
         deltax)
      (setf deltax (* 100 (- (om-point-h initmouse) x)))
      (if (minusp (+ (first initrangex) deltax))
          (setf (rangex self) (list 0 (- (second initrangex) (first initrangex))))
        (setf (rangex self) (list (+ (first initrangex) deltax)
                                  (+ (second initrangex) deltax))))
      (om-invalidate-view (om-view-container self) t)
      (setf *midi-panel-last-click* (om-make-point x y))))

(defmethod select-all ((self midiPanel)) nil)

(defmethod assoc-w ((self midipanel)) 
  (w self))

(defmethod init-coor-system ((self midipanel))
  (setf (rangex self) (copy-list (bounds-x self)))
  (set-units-ruler self (rulerx self))
  (update-subviews (om-view-container self)))

;=====================DRAW


;=== optimization of midi main panel drawing method
(defmethod om-draw-contents ((self midiPanel))
   (call-next-method)
   (om-draw-view-outline self)
   (when (grille-p self)
     (draw-grille self))
   (when  (cursor-p self)
       (draw-interval-cursor self))
   (let ((trh (round (h self) (max 1 (length (tracks (object (editor self))))))))
     (om-with-focused-view self
       (loop for item in (tracks (object (om-view-container self)))
             for i = 0 then (+ i 1) do
             (draw-track self item 
                         (first (rangex self)) (second (rangex self)) 
                         24 96 
                         (if (split-tracks (editor self)) (* i trh) 0)))))
   )
 

    

(defmethod draw-track ((self midiPanel) (track MidiTrack) minx maxx miny maxy y0)
  (let* ((x-notes (give-notes-in-x-range track minx maxx))
         (notes (sort (give-notes-iny-range x-notes miny maxy) '< :key 'second))
         (ysize (max 1 (round (h self) (* (- maxy miny) (if (split-tracks (editor self)) 
                                                     (nb-tracks (rulery self)) 1))))))
    (om-with-fg-color self *om-gray-color*
      (om-with-line-size (min 2 ysize)
      (om-draw-line 0 y0 (w self) y0)))
    (draw-track-notes self track notes ysize y0)))


(defmethod draw-track-notes (self track notes ysize y0)
  (let ((sys-etat (get-system-etat self)))
   (loop for note in notes do
         (let* ((topleft (point2pixel self (om-make-big-point (second note) (first note)) sys-etat))
                (rigth (max 1 (round  (* (third note) (first sys-etat))))))
           (om-with-fg-color self (nth (mod (- (nth 4 note) 1) 16) *16-color-list*)
             (om-fill-rect (om-point-h topleft) 
                           (+ y0 (if (split-tracks (editor self)) 
                                     (round (om-point-v topleft) (nb-tracks (rulery self)))
                                   (om-point-v topleft)))
                           rigth ysize))))
    ))
         

(defmethod draw-interval-cursor ((self midiPanel))
  (call-next-method)
  (let* ((sys-etat (get-system-etat self))
         (cursor-pos-pix (om-point-h (point2pixel self (om-make-big-point (cursor-pos self) 0) sys-etat))))
      (om-with-focused-view self
        (om-with-fg-color self *om-red2-color*
          (om-with-dashline 
            (om-with-line-size 2 
              (om-draw-line cursor-pos-pix 0 cursor-pos-pix (h self)))))
        )))
            

(defmethod get-obj-to-play ((self midiPanel))
   (list (object (om-view-container self)) 
         :port (get-edit-param (editor self) 'outport)))

(defmethod editor ((self midiPanel))
   (om-view-container self))

(defmethod window ((self midiPanel))
   (om-view-window self))


(defmethod get-help-list ((self midieditor))
  (list '((("g") "Sow/Hide Grid")
          (space "Play/Stop"))))

(defmethod handle-key-event ((self midiPanel) char)
   (case char
     (#\g (grille-on-off self))
     (#\h (show-help-window "Keyboard Commands for MIDI Editor" (get-help-list (editor self))))
     (#\SPACE (when *palette-win*
                (if (Idle-p *general-player*)
                    (palette-act *palette* (if (selection-to-play-? self) 4 0))
                  (palette-act  *palette* 1))))
     (otherwise (call-next-method))))   

;; factoriser dans view-player-mixin (avec soundeditor, etc.)
(defmethod get-selection-to-play ((self midiPanel))
  (if (and (cursor-interval self)
           (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
      (call-next-method)
    (let ((interval (list (cursor-pos self) (cadr (bounds-x self)))))
      (values  (list (object (om-view-container self))
                     :interval interval)
               (first interval)
               (second interval)))))


(defmethod selection-to-play-? ((self midiPanel))
  (or (and (cursor-interval self) 
           (not (= (car (cursor-interval self)) (cadr (cursor-interval self)))))
      (and (cursor-pos self) (not (zerop (cursor-pos self))))))

(defmethod attached-cursor-views ((self midipanel)) (list (preview (editor self))))
         
(defmethod scroll-play-window ((self midipanel)) 
  (call-next-method)
  (om-invalidate-view (preview (editor self))))



;------------------------------------
;POINT CONVERSION AND RULERS
;------------------------------------
(defmethod get-system-etat ((self midiPanel)) 
  (let* ((durrangex (abs (- (second (rangex self)) (first (rangex self)))))
         (sizex (w self))
         (sizey (h self))
         (factx (/ sizex (max 1 durrangex)))
         (facty (/ sizey 72))   ;(if (split-tracks (editor self)) (* 72 (nb-tracks (rulery self))) 72)))
         (offsetx (round (* (first (rangex self)) factx)))
         (offsety (round (* 24 facty))))
    (list factx facty offsetx offsety)))

(defmethod point2pixel ((self midiPanel) point sys-etat)
   (let* ((x (om-point-h point))
          (y (om-point-v point))
          (x1 (round (* x (first sys-etat))))
          (y1 (round (* y (second sys-etat)))))
     (om-add-points 
      (om-make-point (- x1 (third sys-etat))  (+ (- (h self) y1) (fourth sys-etat)))
      (om-scroll-position self))))
                                                
(defmethod pixel2point ((self midiPanel) pix)
   (let* ((pixel (om-subtract-points pix (om-scroll-position self)))
          (x (om-point-h pixel))
          (y (if (split-tracks (editor self))
                 (mod (om-point-v pixel) (round (h self) (nb-tracks (rulery self)))) 
                 (om-point-v pixel)))
          (sizeview (om-view-size self))
          (sizex (om-point-h sizeview))
          (sizey (if (split-tracks (editor self))
                     (round (h self) (nb-tracks (rulery self)))
                     (om-point-v sizeview)))
          (durpointx  (abs (- (second (rangex self)) (first (rangex self)))))
          (durpointy  73)  ;; de 24 --> 96
          (x1 (round (* x durpointx) sizex))
          (y1 (floor (* y durpointy) sizey)))
     (om-make-big-point (+  x1 (first (rangex self)))  
                 (- 96 y1))))

;=====================================================================
